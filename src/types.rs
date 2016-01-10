//! Functions for converting Rust types to C types.

use syntax::ast;
use syntax::codemap;
use syntax::print;

use Error;
use Level;

// TODO: don't pass a Ty and a &str, pass
// enum Type<'t, 'n> {
//     Named(&'t ast::Ty, &'n name),
//     Unnamed(&'t ast::Ty),
// }
// impl Type {
//     fn fmt(ty: &str) -> String {
//         match ty
//             Named => format!("{} {}", ty, name),
//             Unnamed => format!("{}", ty),
//         }
//     }
// }
/// Turn a Rust (type, name) pair into a C (type, name) pair.
///
/// If name is `None` then there is no name associated with that type.
pub fn rust_to_c(ty: &ast::Ty, name: Option<&str>) -> Result<Option<String>, Error> {
    match ty.node {
        // Standard pointers.
        ast::Ty_::TyPtr(ref mutty) => ptr_to_c(mutty, name),
        // Function pointers.
        ast::Ty_::TyBareFn(ref bare_fn) => if let Some(name) = name {
            fn_ptr_to_c(bare_fn, ty.span, name)
        } else {
            Err(Error {
                level: Level::Error,
                span: Some(ty.span),
                message: "C function pointers must have a name associated with them".into(),
            })
        },
        // Plain old types.
        ast::Ty_::TyPath(None, ref path) => ty_to_c(path, name),
        // Possibly void, likely not.
        _ => {
            let new_type = print::pprust::ty_to_string(ty);
            if new_type == "()" {
                Ok(Some(if let Some(name) = name {
                    format!("void {}", name)
                } else {
                    "void".to_owned()
                }))
            } else {
                Err(Error {
                    level: Level::Error,
                    span: Some(ty.span),
                    message: format!("cheddar can not handle the type `{}`", new_type),
                })
            }
        },
    }
}

/// Turn a Rust pointer (*mut or *const) into the correct C form.
fn ptr_to_c(ty: &ast::MutTy, name: Option<&str>) -> Result<Option<String>, Error> {
    let new_type = try_some!(rust_to_c(&ty.ty, None));
    let const_spec = match ty.mutbl {
        // *const T
        ast::Mutability::MutImmutable => {
            // Avoid multiple `const` specifiers (you can't have `const const int**` in C).
            if new_type.starts_with("const ") {
                ""
            } else {
                "const "
            }
        },
        // *mut T
        ast::Mutability::MutMutable => "",
    };

    Ok(Some(if let Some(name) = name {
        format!("{}{}* {}", const_spec, new_type, name)
    } else {
        format!("{}{}*", const_spec, new_type)
    }))
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
/// RetTy (*name)(Ty1 arg1, ...)
/// ```
///
/// C function pointers _must_ have a name associated with them.
fn fn_ptr_to_c(fn_ty: &ast::BareFnTy, fn_span: codemap::Span, name: &str) -> Result<Option<String>, Error> {
    use syntax::abi::Abi;
    match fn_ty.abi {
        // If it doesn't have a C ABI it can't be called from C.
        Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {},
        _ => return Ok(None),
    }

    if !fn_ty.lifetimes.is_empty() {
        return Err(Error {
            level: Level::Error,
            span: Some(fn_span),
            message: "cheddar can not handle lifetimes".into(),
        });
    }

    let fn_decl: &ast::FnDecl = &*fn_ty.decl;

    let output_type = &fn_decl.output;
    let output_type = match *output_type {
        ast::FunctionRetTy::NoReturn(span) => {
            return Err(Error {
                level: Level::Error,
                span: Some(span),
                message: "panics across a C boundary are naughty!".into(),
            });
        },
        ast::FunctionRetTy::DefaultReturn(..) => "void".to_owned(),
        ast::FunctionRetTy::Return(ref ty) => try_some!(rust_to_c(&*ty, None)),
    };

    let mut buffer = format!("{} (*{})(", output_type, name);

    let has_args = !fn_decl.inputs.is_empty();

    for arg in &fn_decl.inputs {
        let arg_name = print::pprust::pat_to_string(&*arg.pat);
        let arg_type = try_some!(rust_to_c(&*arg.ty, Some(&arg_name)));
        buffer.push_str(&format!("{}, ", arg_type));
    }

    if has_args {
        // Remove the trailing comma and space.
        buffer.pop();
        buffer.pop();
    }

    buffer.push_str(")");

    Ok(Some(buffer))
}

/// Convert a Rust path type (my_mod::MyType) to a C type.
///
/// Types hidden behind modules are almost certainly custom types (which wouldn't work) except
/// types in `libc` which we special case.
fn ty_to_c(path: &ast::Path, name: Option<&str>) -> Result<Option<String>, Error> {
    let new_type;

    // I don't think this is possible.
    if path.segments.is_empty() {
        return Err(Error {
            level: Level::Bug,
            span: Some(path.span),
            message: "what the fuck have you done to this type?!".into(),
        });
    // Types in modules, `my_mod::MyType`.
    } else if path.segments.len() > 1 {
        let module: &str = &path.segments[0].identifier.name.as_str();
        let ty: &str = &path.segments.last()
            .expect("already checked that there were at least two elements")
            .identifier.name.as_str();

        if module != "libc" {
            return Err(Error {
                level: Level::Error,
                span: Some(path.span),
                message: format!(
                    "cheddar can not handle types in modules (except `libc`), try `use {}::{}` if you really know what you're doing",
                    module,
                    ty,
                ),
            });
        } else {
            new_type = libc_ty_to_c(ty).to_owned();
        }
    } else {
        new_type = rust_ty_to_c(&path.segments[0].identifier.name.as_str()).to_owned();
    }

    Ok(Some(if let Some(name) = name {
        format!("{} {}", new_type, name)
    } else {
        new_type
    }))
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
    fn ty(source: &str) -> ::syntax::ast::Ty {
        let sess = ::syntax::parse::ParseSess::new();
        let mut parser = ::syntax::parse::new_parser_from_source_str(
            &sess,
            vec![],
            "".into(),
            source.into(),
        );

        match parser.parse_ty() {
            Ok(p) => (*p).clone(),
            _ => panic!("internal testing error: could not parse type from {:?}", source),
        }
    }

    // TODO: do a check for genericness at the top of rust_to_c
    #[test]
    #[ignore]
    fn test_generics() {
        let name = "azazael";

        let source = "Result<f64, i32>";
        let typ = super::rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source));
        assert!(typ.is_none(), "successfully parsed invalid type {:?} with no name", source);

        let source = "Option<i16>";
        let typ = super::rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name));
        assert!(typ.is_none(), "successfully parsed invalid type {:?} with name {:?}", source, name);
    }

    #[test]
    fn test_pure_rust_types() {
        let type_map = [
            ("()", "void"),
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
            ("usize", "uintptr_t"),
        ];

        let name = "gabriel";

        for &(rust_type, correct_c_type) in &type_map {
            let parsed_c_type = super::rust_to_c(&ty(rust_type), None)
                .expect(&format!("error while parsing {:?} with no name", rust_type))
                .expect(&format!("did not parse {:?} with no name", rust_type));
            assert_eq!(parsed_c_type, correct_c_type);

            let parsed_c_type = super::rust_to_c(&ty(rust_type), Some(name))
                .expect(&format!("error while parsing {:?} with name {:?}", rust_type, name))
                .expect(&format!("did not parse {:?} with name {:?}", rust_type, name));
            assert_eq!(parsed_c_type, format!("{} {}", correct_c_type, name));
        }
    }

    #[test]
    fn test_libc_types() {
        let type_map = [
            ("libc::c_void", "void"),
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
            ("libc::FILE", "FILE"),
        ];

        let name = "lucifer";

        for &(rust_type, correct_c_type) in &type_map {
            let parsed_c_type = super::rust_to_c(&ty(rust_type), None)
                .expect(&format!("error while parsing {:?} with no name", rust_type))
                .expect(&format!("did not parse {:?} with no name", rust_type));
            assert_eq!(parsed_c_type, correct_c_type);

            let parsed_c_type = super::rust_to_c(&ty(rust_type), Some(name))
                .expect(&format!("error while parsing {:?} with name {:?}", rust_type, name))
                .expect(&format!("did not parse {:?} with name {:?}", rust_type, name));
            assert_eq!(parsed_c_type, format!("{} {}", correct_c_type, name));
        }
    }

    #[test]
    fn test_const_pointers() {
        let name = "maalik";

        let source = "*const u8";
        let parsed_type = super::rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "const uint8_t*");

        let source = "*const ()";
        let parsed_type = super::rust_to_c(&ty(source), Some(name))
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("const void* {}", name));

        let source = "*const *const f64";
        let parsed_type = super::rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "const double**");

        let source = "*const *const i64";
        let parsed_type = super::rust_to_c(&ty(source), Some(name))
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("const int64_t** {}", name));
    }

    #[test]
    fn test_mut_pointers() {
        let name = "raphael";

        let source = "*mut u16";
        let parsed_type = super::rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "uint16_t*");

        let source = "*mut f32";
        let parsed_type = super::rust_to_c(&ty(source), Some(name))
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("float* {}", name));

        let source = "*mut *mut *mut i32";
        let parsed_type = super::rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "int32_t***");

        let source = "*mut *mut i8";
        let parsed_type = super::rust_to_c(&ty(source), Some(name))
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("int8_t** {}", name));
    }

    #[test]
    fn test_mixed_pointers() {
        let name = "samael";

        let source = "*const *mut *const bool";
        let parsed_type = super::rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "const bool***");

        let source = "*mut *mut *const libc::c_ulonglong";
        let parsed_type = super::rust_to_c(&ty(source), Some(name))
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("const unsigned long long*** {}", name));
    }

    #[test]
    fn test_function_pointers() {
        let name = "sariel";

        let source = "fn(a: bool)";
        let parsed_type = super::rust_to_c(&ty(source), None);
        assert!(parsed_type.is_err(), "C function pointers should have a name associated");

        let source = "fn(a: i8) -> f64";
        let parsed_type = super::rust_to_c(&ty(source), Some(name))
            .expect(&format!("error while parsing {:?} with name {:?}", source, name));
        assert!(parsed_type.is_none(), "parsed a non-C function pointer");

        let source = "extern fn(hi: libc::c_int) -> libc::c_double";
        let parsed_type = super::rust_to_c(&ty(source), Some(name))
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("double (*{})(int hi)", name));
    }

    #[test]
    fn test_paths() {
        let name = "zachariel";

        let source = "MyType";
        let parsed_type = super::rust_to_c(&ty(source), None)
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "MyType");

        let source = "SomeType";
        let parsed_type = super::rust_to_c(&ty(source), Some(name))
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("SomeType {}", name));

        let source = "my_mod::MyType";
        let parsed_type = super::rust_to_c(&ty(source), None);
        assert!(parsed_type.is_err(), "can't use a multi-segment path which isn't `libc`");

        let source = "some_mod::SomeType";
        let parsed_type = super::rust_to_c(&ty(source), Some(name));
        assert!(parsed_type.is_err(), "can't use a multi-segment path which isn't `libc`");
    }

    // TODO: C function pointers _don't_ need a name (return pointers for example)
    #[test]
    #[ignore]
    fn test_signal() {
        // See `man 3 signal` for info.
        let source = "extern fn(sig: libc::c_int, func: extern fn(libc::c_int)) -> extern fn(libc::c_int)";
        let name = "signal";
        let parsed_type = super::rust_to_c(&ty(source), Some(name))
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, "void (*signal(int sig, void (*func)(int)))(int)");
    }
}
