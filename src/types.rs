use std::rc::Rc;

use binder::compiler::Stop;
use binder::compiler::session::Session;
use binder::compiler::syntax::*;
use itertools::Itertools;

#[derive(Clone)]
pub struct CustomTypeModule(&'static str, Rc<Box<Fn(&str) -> &str>>);

impl CustomTypeModule {
    pub fn new<F>(mod_name: &'static str, convert: F) -> Self
        where F: Fn(&str) -> &str + 'static
    {
        CustomTypeModule(mod_name, Rc::new(Box::new(convert)))
    }
}


struct Include {
    type_name: String,
    module_name: String,
}


/// Unwraps Result<Option<..>> if it is Ok(Some(..)) else returns.
macro_rules! try_some {
    ($expr:expr) => {{ match $expr {
        Ok(Some(val)) => val,
        expr => return expr,
    }}};
}

pub struct TypeFactory {
    includes: Vec<ast::ViewPath_>,
    custom_type_modules: Vec<CustomTypeModule>,
    custom_type_module_names: Vec<String>,
}


impl TypeFactory {
    /// Turn a Rust type with an associated name or type into a C type.
    pub fn rust_to_c(&self,
                     ty: &ast::Ty,
                     assoc: &str,
                     session: Option<&Session>)
                     -> Result<Option<String>, Stop> {

        match ty.node {
            // Function pointers make life an absolute pain here.
            ast::TyKind::BareFn(ref bare_fn) => self.fn_ptr_to_c(bare_fn, ty.span, assoc, session),
            // All other types just have a name associated with them.
            _ => Ok(Some(format!("{} {}", try_some!(self.anon_rust_to_c(ty, session)), assoc))),
        }
    }

    pub fn new(includes: Vec<ast::ViewPath_>, mut custom_mods: Vec<CustomTypeModule>) -> Self {
        custom_mods.push(CustomTypeModule::new("std::os::raw", osraw_ty_to_c));
        custom_mods.push(CustomTypeModule::new("libc", libc_ty_to_c));
        let names = custom_mods.iter().map(|m| m.0.to_owned()).collect();
        TypeFactory {
            includes: includes,
            custom_type_module_names: names,
            custom_type_modules: custom_mods,
        }
    }

    /// Turn a Rust type into a C type.
    fn anon_rust_to_c(&self,
                      ty: &ast::Ty,
                      session: Option<&Session>)
                      -> Result<Option<String>, Stop> {
        match ty.node {
            // Function pointers should not be in this function.
            ast::TyKind::BareFn(..) => {
                Err(session.map_or(Stop::Fail, |s| {
                    s.span_err(ty.span,
                               "C function pointers must have a name or function declaration \
                                associated with them")
                }))
            }
            // Standard pointers.
            ast::TyKind::Ptr(ref ptr) => self.ptr_to_c(ptr, session),
            // Plain old types.
            ast::TyKind::Path(None, ref path) => self.path_to_c(path, session, ty.span),
            // Possibly void, likely not.
            _ => {
                let new_type = print::pprust::ty_to_string(ty);
                if new_type == "()" {
                    Ok(Some("void".into()))
                } else {
                    Err(session.map_or(Stop::Fail, |s| {
                        s.span_err(ty.span,
                                   &format!("cheddar can not handle the type `{}`", new_type))
                    }))
                }
            }
        }
    }

    /// Turn a Rust pointer (*mut or *const) into the correct C form.
    fn ptr_to_c(&self, ty: &ast::MutTy, session: Option<&Session>) -> Result<Option<String>, Stop> {
        let new_type = try_some!(self.anon_rust_to_c(&ty.ty, session));
        let const_spec = match ty.mutbl {
            // *const T
            ast::Mutability::Immutable => " const",
            // *mut T
            ast::Mutability::Mutable => "",
        };

        Ok(Some(format!("{}{}*", new_type, const_spec)))
    }

    /// Turn a Rust function pointer into a C function pointer.
    ///
    /// Rust function pointers are of the form
    ///
    /// ```ignore
    /// fn(arg1: Ty1, ...) -> RetTy
    /// ```
    ///
    /// C function pointers are of the form
    ///
    /// ```C
    /// RetTy (*inner)(Ty1 arg1, ...)
    /// ```
    ///
    /// where `inner` could either be a name or the rest of a function declaration.
    fn fn_ptr_to_c(&self,
                   fn_ty: &ast::BareFnTy,
                   fn_span: codemap::Span,
                   inner: &str,
                   session: Option<&Session>)
                   -> Result<Option<String>, Stop> {
        use binder::compiler::syntax::abi::Abi;
        match fn_ty.abi {
            // If it doesn't have a C ABI it can't be called from C.
            Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {}
            _ => return Ok(None),
        }

        if !fn_ty.lifetimes.is_empty() {
            return Err(session.map_or(Stop::Fail,
                                      |s| s.span_err(fn_span, "cheddar can not handle lifetimes")));
        }

        let fn_decl: &ast::FnDecl = &*fn_ty.decl;

        let mut buf_without_return = format!("(*{})(", inner);

        let has_args = !fn_decl.inputs.is_empty();

        for arg in &fn_decl.inputs {
            let arg_name = print::pprust::pat_to_string(&*arg.pat);
            let arg_type = try_some!(self.rust_to_c(&*arg.ty, &arg_name, session));
            buf_without_return.push_str(&arg_type);
            buf_without_return.push_str(", ");
        }

        if has_args {
            // Remove the trailing comma and space.
            buf_without_return.pop();
            buf_without_return.pop();
        } else {
            buf_without_return.push_str("void");
        }

        buf_without_return.push(')');

        let output_type = &fn_decl.output;
        let full_declaration = match *output_type {
            ast::FunctionRetTy::Default(..) => format!("void {}", buf_without_return),
            ast::FunctionRetTy::Ty(ref ty) => {
                try_some!(self.rust_to_c(&*ty, &buf_without_return, session))
            }
        };


        Ok(Some(full_declaration))
    }

    /// Convert a Rust path type (my_mod::MyType) to a C type.
    ///
    /// Types hidden behind modules are almost certainly custom types (which wouldn't work) except
    /// types in `libc` which we special case.
    fn path_to_c(&self,
                 path: &ast::Path,
                 session: Option<&Session>,
                 span: codemap::Span)
                 -> Result<Option<String>, Stop> {
        for s in &path.segments {
            if !s.parameters.is_empty() {
                return Err(session.map_or(Stop::Fail, |s| {
                    s.span_err(span,
                               &format!("{} is a generic type",
                                        Self::path_to_string(&path.segments)))
                }));
            }
        }
        // I don't think this is possible.
        if path.segments.is_empty() {
            Err(session.map_or(Stop::Fail,
                               |s| s.fatal("what the fuck have you done to this type?!")))
            // Types in modules, `my_mod::MyType`.
        } else {
            if path.segments.len() > 1 {
                let (ty, module) = path.segments
                    .split_last()
                    .expect("There are 2 elements");
                let p = Self::path_to_string(module);
                if let Some(ref m) = self.custom_type_modules
                    .iter()
                    .filter_map(|m| if m.0 == p { Some(&m.1) } else { None })
                    .next() {
                    return Ok(Some(m(&*ty.identifier.name.as_str()).into()));
                }
            }
            let (ty, module) = path.segments.split_last().expect("Segments are not empty");
            if let Some(Include { module_name, type_name }) =
                   self.match_type_to_include(ty, module) {
                if let Some(ref m) = self.custom_type_modules
                    .iter()
                    .filter_map(|m| if m.0 == module_name {
                        Some(&m.1)
                    } else {
                        None
                    })
                    .next() {
                    let r = m(&type_name);
                    if r != type_name {
                        return Ok(Some(r.into()));
                    } else if module.is_empty() {
                        let r2 = rust_ty_to_c(&*ty.identifier.name.as_str()).to_owned();
                        if r2 != type_name {
                            return Ok(Some(r2));
                        } else {
                            return Ok(Some(r.into()));
                        }
                    } else {
                        return Ok(Some(r.into()));
                    }
                } else {
                    return Err(session.map_or(Stop::Fail, |s| {
                        s.span_err(span,
                                   "Cheddar does not support types from other modules, expect \
                                    from libc and std::os::raw")
                    }));
                }

            } else if module.is_empty() {
                return Ok(Some(rust_ty_to_c(&*ty.identifier.name.as_str()).into()));
            } else {
                return Err(session.map_or(Stop::Fail, |s| {
                    s.span_err(span,
                               "Cheddar does not support types from other modules, expect from \
                                libc and std::os::raw")
                }));
            }
        }
    }



    fn match_type_to_include(&self,
                             ty: &ast::PathSegment,
                             module: &[ast::PathSegment])
                             -> Option<Include> {
        if module.is_empty() {
            self.includes
                .iter()
                .filter_map(|i| Self::get_include(i, ty.identifier, &self.custom_type_module_names))
                .next()
        } else {
            if let Some(i) = self.includes
                .iter()
                .filter_map(|i| {
                    Self::get_include(i, module[0].identifier, &self.custom_type_module_names)
                })
                .next() {
                if module.len() > 1 {
                    let m = Self::path_to_string(&module[1..]);
                    Some(Include {
                        type_name: (&*ty.identifier.name.as_str()).to_owned(),
                        module_name: format!("{}::{}::{}", i.module_name, i.type_name, m),
                    })
                } else if !i.module_name.is_empty() {
                    Some(Include {
                        type_name: (&*ty.identifier.name.as_str()).to_owned(),
                        module_name: format!("{}::{}", i.module_name, i.type_name),
                    })
                } else {
                    Some(Include {
                        type_name: (&*ty.identifier.name.as_str()).to_owned(),
                        module_name: i.type_name,
                    })
                }
            } else {
                None
            }
        }
    }

    fn get_include(include: &ast::ViewPath_,
                   i: ast::Ident,
                   custom_mod_names: &[String])
                   -> Option<Include> {
        match include {
            &ast::ViewPath_::ViewPathGlob(ref p) => {
                let import_name = Self::path_to_string(&p.segments);
                println!("import_name: {}", import_name);
                if custom_mod_names.iter().any(|c| c.starts_with(&import_name)) {
                    Some(Include {
                        type_name: (*i.name.as_str()).to_owned(),
                        module_name: import_name,
                    })
                } else {
                    None
                }
            }
            &ast::ViewPath_::ViewPathList(ref p, ref items) => {
                items.iter()
                    .filter_map(|item| {
                        match item.node {
                            ast::PathListItemKind::Ident { ref name, ref rename, .. } => {
                                if let &Some(ref rename) = rename {
                                    if rename.name == i.name {
                                        Some(Include {
                                            type_name: (*name.name.as_str()).to_owned(),
                                            module_name: Self::path_to_string(&p.segments),
                                        })
                                    } else {
                                        None
                                    }
                                } else if name.name == i.name {
                                    Some(Include {
                                        type_name: (*name.name.as_str()).to_owned(),
                                        module_name: Self::path_to_string(&p.segments),
                                    })
                                } else {
                                    None
                                }
                            }
                            ast::PathListItemKind::Mod { ref rename, .. } => {
                                let (ty, module) = p.segments.split_last().unwrap();
                                if let &Some(ref rename) = rename {
                                    if rename.name == i.name {
                                        Some(Include {
                                            type_name: (*ty.identifier.name.as_str()).to_owned(),
                                            module_name: Self::path_to_string(module),
                                        })
                                    } else {
                                        None
                                    }
                                } else {
                                    if ty.identifier.name == i.name {
                                        Some(Include {
                                            type_name: (*ty.identifier.name.as_str()).to_owned(),
                                            module_name: Self::path_to_string(module),
                                        })
                                    } else {
                                        None
                                    }
                                }
                            }
                        }
                    })
                    .next()
            }
            &ast::ViewPath_::ViewPathSimple(ref ident, ref path) => {
                if ident.name == i.name {
                    let (ty, module) = path.segments
                        .split_last()
                        .unwrap();
                    Some(Include {
                        type_name: (*ty.identifier.name.as_str()).to_owned(),
                        module_name: Self::path_to_string(module),
                    })
                } else {
                    None
                }
            }

        }
    }

    fn path_to_string(p: &[ast::PathSegment]) -> String {
        p.iter().map(|p| (&*p.identifier.name.as_str()).to_owned()).join("::")
    }
}

/// Convert a Rust type from `libc` into a C type.
///
/// Most map straight over but some have to be converted.
fn libc_ty_to_c(ty: &str) -> &str {
    match ty {
        "c_void" => "void",
        "c_float" => "float",
        "c_double" => "double",
        "c_char" => "char",
        "c_schar" => "signed char",
        "c_uchar" => "unsigned char",
        "c_short" => "short",
        "c_ushort" => "unsigned short",
        "c_int" => "int",
        "c_uint" => "unsigned int",
        "c_long" => "long",
        "c_ulong" => "unsigned long",
        "c_longlong" => "long long",
        "c_ulonglong" => "unsigned long long",
        // All other types should map over to C.
        ty => ty,
    }
}

/// Convert a Rust type from `std::os::raw` into a C type.
///
/// These mostly mirror the libc crate.
fn osraw_ty_to_c(ty: &str) -> &str {
    match ty {
        "c_void" => "void",
        "c_char" => "char",
        "c_double" => "double",
        "c_float" => "float",
        "c_int" => "int",
        "c_long" => "long",
        "c_longlong" => "long long",
        "c_schar" => "signed char",
        "c_short" => "short",
        "c_uchar" => "unsigned char",
        "c_uint" => "unsigned int",
        "c_ulong" => "unsigned long",
        "c_ulonglong" => "unsigned long long",
        "c_ushort" => "unsigned short",
        // All other types should map over to C.
        ty => ty,
    }
}

/// Convert any Rust type into C.
///
/// This includes user-defined types. We currently trust the user not to use types which we don't
/// know the structure of (like String).
fn rust_ty_to_c(ty: &str) -> &str {
    match ty {
        "()" => "void",
        "f32" => "float",
        "f64" => "double",
        "i8" => "int8_t",
        "i16" => "int16_t",
        "i32" => "int32_t",
        "i64" => "int64_t",
        "isize" => "intptr_t",
        "u8" => "uint8_t",
        "u16" => "uint16_t",
        "u32" => "uint32_t",
        "u64" => "uint64_t",
        "usize" => "uintptr_t",
        // This is why we write out structs and enums as `typedef ...`.
        // We `#include <stdbool.h>` so bool is handled.
        ty => ty,
    }
}


#[cfg(test)]
mod test {
    use binder::compiler::syntax::{ast, parse};

    fn ty(source: &str) -> ast::Ty {
        ty_helper(source, &parse::ParseSess::new())
    }
    fn ty_helper(source: &str, sess: &parse::ParseSess) -> ast::Ty {

        let mut parser = parse::new_parser_from_source_str(sess, vec![], "".into(), source.into());

        match parser.parse_ty() {
            Ok(p) => (*p).clone(),
            _ => {
                panic!("internal testing error: could not parse type from {:?}",
                       source)
            }
        }

    }

    fn use_(uses: Vec<&str>) -> Vec<ast::ViewPath_> {
        let sess = parse::ParseSess::new();
        uses.into_iter()
            .map(|u| {
                let mut parser = parse::new_parser_from_source_str(&sess,
                                                                   vec![],
                                                                   "".into(),
                                                                   format!("use {};", u));
                match parser.parse_crate_mod() {
                    Ok(c) => {
                        match c.module.items[0].node {
                            ast::ItemKind::Use(ref p) => (*p).node.clone(),
                            _ => {
                                panic!("internal testing error: could not parse import from {:?}",
                                       u)
                            }
                        }
                    }
                    _ => {
                        panic!("internal testing error: could not parse import from {:?}",
                               u)
                    }

                }
            })
            .collect()
    }

    fn make_factory(uses: Vec<&str>) -> super::TypeFactory {
        super::TypeFactory::new(use_(uses), Vec::new())
    }

    #[test]
    fn generics() {
        let name = "azazael";

        let source = "Result<f64, i32>";
        let factory = make_factory(Vec::new());
        let typ = factory.anon_rust_to_c(&ty(source), None);
        assert!(typ.is_err(),
                "successfully parsed invalid type {:?} with no name",
                source);

        let source = "Option<i16>";
        let typ = factory.rust_to_c(&ty(source), name, None);
        assert!(typ.is_err(),
                "successfully parsed invalid type {:?} with name {:?}",
                source,
                name);
    }

    #[test]
    fn pure_rust_types() {
        let type_map = [("()", "void"),
                        ("f32", "float"),
                        ("f64", "double"),
                        ("i8", "int8_t"),
                        ("i16", "int16_t"),
                        ("i32", "int32_t"),
                        ("i64", "int64_t"),
                        ("isize", "intptr_t"),
                        ("u8", "uint8_t"),
                        ("u16", "uint16_t"),
                        ("u32", "uint32_t"),
                        ("u64", "uint64_t"),
                        ("usize", "uintptr_t")];

        let name = "gabriel";
        let factory = make_factory(Vec::new());

        for &(rust_type, correct_c_type) in &type_map {
            let parsed_c_type = factory.anon_rust_to_c(&ty(rust_type), None)
                .expect(&format!("error while parsing {:?} with no name", rust_type))
                .expect(&format!("did not parse {:?} with no name", rust_type));
            assert_eq!(parsed_c_type, correct_c_type);

            let parsed_c_type = factory.rust_to_c(&ty(rust_type), name, None)
                .expect(&format!("error while parsing {:?} with name {:?}", rust_type, name))
                .expect(&format!("did not parse {:?} with name {:?}", rust_type, name));
            assert_eq!(parsed_c_type, format!("{} {}", correct_c_type, name));
        }
    }

    #[test]
    fn libc_types() {
        let type_map = [("libc::c_void", "void"),
                        ("libc::c_float", "float"),
                        ("libc::c_double", "double"),
                        ("libc::c_char", "char"),
                        ("libc::c_schar", "signed char"),
                        ("libc::c_uchar", "unsigned char"),
                        ("libc::c_short", "short"),
                        ("libc::c_ushort", "unsigned short"),
                        ("libc::c_int", "int"),
                        ("libc::c_uint", "unsigned int"),
                        ("libc::c_long", "long"),
                        ("libc::c_ulong", "unsigned long"),
                        ("libc::c_longlong", "long long"),
                        ("libc::c_ulonglong", "unsigned long long"),
                        // Some other common ones.
                        ("libc::size_t", "size_t"),
                        ("libc::dirent", "dirent"),
                        ("libc::FILE", "FILE")];

        let name = "lucifer";
        let factory = make_factory(vec!["libc"]);

        for &(rust_type, correct_c_type) in &type_map {
            let parsed_c_type = factory.anon_rust_to_c(&ty(rust_type), None)
                .expect(&format!("error while parsing {:?} with no name", rust_type))
                .expect(&format!("did not parse {:?} with no name", rust_type));
            assert_eq!(parsed_c_type, correct_c_type);

            let parsed_c_type = factory.rust_to_c(&ty(rust_type), name, None)
                .expect(&format!("error while parsing {:?} with name {:?}", rust_type, name))
                .expect(&format!("did not parse {:?} with name {:?}", rust_type, name));
            assert_eq!(parsed_c_type, format!("{} {}", correct_c_type, name));
        }
    }

    #[test]
    fn const_pointers() {
        let name = "maalik";
        let factory = make_factory(vec!["libc"]);

        let source = "*const u8";
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "uint8_t const*");

        let source = "*const ()";
        let parsed_type = factory.rust_to_c(&ty(source), name, None)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("void const* {}", name));

        let source = "*const *const f64";
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "double const* const*");

        let source = "*const *const i64";
        let parsed_type = factory.rust_to_c(&ty(source), name, None)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("int64_t const* const* {}", name));
    }

    #[test]
    fn mut_pointers() {
        let name = "raphael";
        let factory = make_factory(vec!["libc"]);

        let source = "*mut u16";
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "uint16_t*");

        let source = "*mut f32";
        let parsed_type = factory.rust_to_c(&ty(source), name, None)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("float* {}", name));

        let source = "*mut *mut *mut i32";
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "int32_t***");

        let source = "*mut *mut i8";
        let parsed_type = factory.rust_to_c(&ty(source), name, None)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("int8_t** {}", name));
    }

    #[test]
    fn mixed_pointers() {
        let name = "samael";
        let factory = make_factory(vec!["libc"]);

        let source = "*const *mut *const bool";
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "bool const** const*");

        let source = "*mut *mut *const ::libc::c_ulonglong";
        let parsed_type = factory.rust_to_c(&ty(source), name, None)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("unsigned long long const*** {}", name));

        let source = "*const *mut *mut i8";
        let parsed_type = factory.rust_to_c(&ty(source), name, None)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("int8_t** const* {}", name));
    }

    #[test]
    fn function_pointers() {
        let name = "sariel";
        let factory = make_factory(vec!["libc"]);

        let source = "fn(a: bool)";
        let parsed_type = factory.anon_rust_to_c(&ty(source), None);
        assert!(parsed_type.is_err(),
                "C function pointers should have an inner or name associated");

        let source = "fn(a: i8) -> f64";
        let parsed_type = factory.rust_to_c(&ty(source), name, None)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name));
        assert!(parsed_type.is_none(), "parsed a non-C function pointer");

        let source = "extern fn(hi: libc::c_int) -> libc::c_double";
        let parsed_type = factory.rust_to_c(&ty(source), name, None)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("double (*{})(int hi)", name));
    }

    #[test]
    fn paths() {
        let name = "zachariel";
        let factory = make_factory(Vec::new());

        let source = "MyType";
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "MyType");

        let source = "SomeType";
        let parsed_type = factory.rust_to_c(&ty(source), name, None)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("SomeType {}", name));

        let source = "my_mod::MyType";
        let parsed_type = factory.anon_rust_to_c(&ty(source), None);
        assert!(parsed_type.is_err(),
                "can't use a multi-segment path which isn't `libc`");

        let source = "some_mod::SomeType";
        let parsed_type = factory.rust_to_c(&ty(source), name, None);
        assert!(parsed_type.is_err(),
                "can't use a multi-segment path which isn't `libc`");
    }

    #[test]
    fn use_modules() {

        for m in vec!["libc", "std::os::raw"] {
            let factory = make_factory(vec![&format!("{}::c_char", m)]);
            let source = "c_char";
            let parsed_type = factory.anon_rust_to_c(&ty(source), None)
                .expect(&format!("error while parsing {:?} with no name", source))
                .expect(&format!("did not parse {:?} with no name", source));
            assert_eq!(parsed_type, "char");

            let factory = make_factory(vec![&format!("{}::*", m)]);
            let parsed_type = factory.anon_rust_to_c(&ty(source), None)
                .expect(&format!("error while parsing {:?} with no name", source))
                .expect(&format!("did not parse {:?} with no name", source));
            assert_eq!(parsed_type, "char");

            let factory = make_factory(vec![&format!("{}::{{c_char, c_void}}", m)]);
            let parsed_type = factory.anon_rust_to_c(&ty(source), None)
                .expect(&format!("error while parsing {:?} with no name", source))
                .expect(&format!("did not parse {:?} with no name", source));
            assert_eq!(parsed_type, "char");

            let factory = make_factory(vec![&format!("{}::*", m)]);
            let source = "i32";
            let parsed_type = factory.anon_rust_to_c(&ty(source), None)
                .expect(&format!("error while parsing {:?} with no name", source))
                .expect(&format!("did not parse {:?} with no name", source));
            assert_eq!(parsed_type, "int32_t");
        }
    }

    #[test]
    fn use_modules_rename() {

        for m in vec!["libc", "std::os::raw"] {
            let source = "t_char";
            let factory = make_factory(vec![&format!("{}::c_char as t_char", m)]);
            let parsed_type = factory.anon_rust_to_c(&ty(source), None)
                .expect(&format!("error while parsing {:?} with no name", source))
                .expect(&format!("did not parse {:?} with no name", source));
            assert_eq!(parsed_type, "char");


            let factory = make_factory(vec![&format!("{}::{{c_char as t_char, c_void}}", m)]);
            let parsed_type = factory.anon_rust_to_c(&ty(source), None)
                .expect(&format!("error while parsing {:?} with no name", source))
                .expect(&format!("did not parse {:?} with no name", source));
            assert_eq!(parsed_type, "char");

        }
    }

    #[test]
    fn use_modules_partial() {

        let source = "raw::c_char";
        let factory = make_factory(vec!["std::os::raw"]);
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "char");


        let factory = make_factory(vec!["std::os::{raw, linux}"]);
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "char");

        let factory = make_factory(vec!["std::os::*"]);
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "char");

        let source = "raw_t::c_char";
        let factory = make_factory(vec!["std::os::raw as raw_t"]);
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "char");



        let factory = make_factory(vec!["std::os::{raw as raw_t, linux}"]);
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "char");


        let source = "os::raw::c_char";
        let factory = make_factory(vec!["std::os::{self, linux}"]);
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "char");

        let source = "os_t::raw::c_char";
        let factory = make_factory(vec!["std::os::{self as os_t, linux}"]);
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "char");

        let source = "libc::c_char";
        let factory = make_factory(vec!["libc"]);
        let parsed_type = factory.anon_rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "char");

    }
}
