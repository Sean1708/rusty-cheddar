#![feature(rustc_private)]
#![feature(plugin_registrar)]
#![feature(box_syntax)]
#![feature(stmt_expr_attributes)]

#![feature(plugin)]
#![plugin(clippy)]

#[macro_use] extern crate rustc;
extern crate rustc_plugin;
extern crate syntax;

// External
use rustc::lint;
use rustc::lint::EarlyContext;
use rustc::lint::LintArray;
use syntax::abi::Abi;
use syntax::ast;
use syntax::ast::Attribute;
use syntax::ast::Item;
use syntax::ast::Item_;
use syntax::codemap;
use syntax::print::pprust;

// Internal
use std::fs;
use std::path::PathBuf;

// Traits
use std::io::Write;


/// Unwraps Result<Option<..>> if it is Ok(Some(..)) else returns.
macro_rules! try_some {
    ($expr:expr) => {{ match $expr {
        Ok(Some(val)) => val,
        expr => return expr,
    }}};
}


// TODO: error handling by having an enum with levels and returning Vec?
// enum Error {
//     Fatal(Option<Span>, String),
//     Error(Option<Span>, String),
//     Warn(Option<Span>, String),
//     Note(Option<Span>, String),
// }
// then have
// type Result = std::result::Result<Option(String), Vec<Error>>;


pub struct CheddarPass {
    file: PathBuf,
}

type Result = std::result::Result<Option<String>, (codemap::Span, String)>;

declare_lint!(CHEDDAR, Allow, "What does this actually do? Do I need it?");

impl lint::LintPass for CheddarPass {
    fn get_lints(&self) -> LintArray {
        lint_array!(CHEDDAR)
    }
}

impl lint::EarlyLintPass for CheddarPass {
    fn check_crate(&mut self, context: &EarlyContext, krate: &ast::Crate) {
        let mut buffer = format!(
            "#ifndef cheddar_gen_{0}_h\n#define cheddar_gen_{0}_h\n\n",
            self.file.file_stem().and_then(|p| p.to_str()).unwrap_or("default"),
        );
        buffer.push_str("#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n");
        buffer.push_str("#include <stdint.h>\n#include <stdbool.h>\n\n");

        for item in &krate.module.items {
            // If it's not visible it can't be called from C.
            if let ast::Visibility::Inherited = item.vis { continue; }

            // Dispatch to correct method.
            let res = match item.node {
                // TODO: Check for ItemStatic and ItemConst as well.
                //     - How would this work?
                //     - Is it even possible?
                Item_::ItemTy(..) => self.parse_ty(context, item),
                Item_::ItemEnum(..) => self.parse_enum(context, item),
                Item_::ItemStruct(..) => self.parse_struct(context, item),
                Item_::ItemFn(..) => self.parse_fn(context, item),
                _ => Ok(None),
            };

            // Display any non-fatal errors, fatal errors are handled at cause.
            match res {
                Err((span, msg)) => context.sess.span_err(span, &msg),
                Ok(Some(buf)) => buffer.push_str(&buf),
                // TODO: put span notes in these or would that just get annoying?
                Ok(None) => {},  // Item should not be written to header.
            };
        }

        buffer.push_str("#ifdef __cplusplus\n}\n#endif\n\n");
        buffer.push_str("#endif\n");

        let bytes_buf = buffer.into_bytes();

        if let Err(error) = fs::File::create(&self.file).and_then(|mut f| f.write_all(&bytes_buf)) {
            context.sess.err(&format!("could not write to '{}': {}", self.file.display(), error))
        };
    }
}

// TODO: Maybe it would be wise to use syntax::attr here.
fn parse_attr<C, R>(attrs: &[Attribute], check: C, retrieve: R) -> (bool, String)
    where C: Fn(&Attribute) -> bool,
          R: Fn(&Attribute) -> Option<String>,
{
    let mut check_passed = false;
    let mut retrieved_str = String::new();
    for attr in attrs {
        // Don't want to accidently set it to false after it's been set to true.
        if !check_passed { check_passed = check(attr); }
        // If this attribute has any strings to retrieve, retrieve them.
        if let Some(string) = retrieve(attr) { retrieved_str.push_str(&string); }
    }

    (check_passed, retrieved_str)
}

fn check_repr_c(attr: &Attribute) -> bool {
    match attr.node.value.node {
        ast::MetaItem_::MetaList(ref name, ref word) if *name == "repr" => match word.first() {
            Some(word) => match word.node {
                // Return true only if attribute is #[repr(C)].
                ast::MetaItem_::MetaWord(ref name) if *name == "C" => true,
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}

fn check_no_mangle(attr: &Attribute) -> bool {
    match attr.node.value.node {
        ast::MetaItem_::MetaWord(ref name) if *name == "no_mangle" => true,
        _ => false,
    }
}

fn retrieve_docstring(attr: &Attribute, prepend: &str) -> Option<String> {
    match attr.node.value.node {
        ast::MetaItem_::MetaNameValue(ref name, ref val) if *name == "doc" => match val.node {
            // Docstring attributes omit the trailing newline.
            ast::Lit_::LitStr(ref docs, _) => Some(format!("{}{}\n", prepend, docs)),
            _ => unreachable!("docs must be literal strings"),
        },
        _ => None,
    }
}

// TODO: refactor:
//     - path_to_c(&ast::Path)
//     - probably pull the FnDecl parsing logic out of parse_fn
// TODO: C function pointers _must_ have a name associated with them but this Option business feels
//       like a shit way to handle that
//     - maybe have a named_rust_to_c which allows fn_pointers and rust_to_c doesn't?
fn rust_to_c(ty: &ast::Ty, name: Option<&str>) -> Result {
    match ty.node {
        // Standard pointers.
        ast::Ty_::TyPtr(ref mutty) => ptr_to_c(mutty, name),
        // Function pointers.
        ast::Ty_::TyBareFn(ref bare_fn) => if let Some(name) = name {
            fn_ptr_to_c(bare_fn, ty.span, name)
        } else {
            Err((ty.span, "C function pointers must have a name associated with them".to_owned()))
        },
        // Plain old types.
        ast::Ty_::TyPath(None, ref path) => ty_to_c(path, name),
        // Possibly void, likely not.
        _ => {
            let new_type = pprust::ty_to_string(ty);
            if new_type == "()" {
                Ok(Some(if let Some(name) = name {
                    format!("void {}", name)
                } else {
                    "void".to_owned()
                }))
            } else {
                Err((ty.span, format!("cheddar can not handle the type `{}`", new_type)))
            }
        },
    }
}

fn ptr_to_c(ty: &ast::MutTy, name: Option<&str>) -> Result {
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

fn fn_ptr_to_c(fn_ty: &ast::BareFnTy, fn_span: codemap::Span, name: &str) -> Result {
    match fn_ty.abi {
        // If it doesn't have a C ABI it can't be called from C.
        Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {},
        _ => return Ok(None),
    }

    if !fn_ty.lifetimes.is_empty() {
        return Err((fn_span, "rusty-cheddar can not handle lifetimes".to_owned()));
    }

    // C function pointers have the form
    //     R (*name)(T1 ident1, T2 ident2, ...)

    let fn_decl: &ast::FnDecl = &*fn_ty.decl;

    let output_type = &fn_decl.output;
    let output_type = match *output_type {
        ast::FunctionRetTy::NoReturn(span) => {
            return Err((span, "panics across a C boundary are naughty!".to_owned()));
        },
        ast::FunctionRetTy::DefaultReturn(..) => "void".to_owned(),
        ast::FunctionRetTy::Return(ref ty) => try_some!(rust_to_c(&*ty, None)),
    };

    let mut buffer = format!("{} (*{})(", output_type, name);

    let has_args = !fn_decl.inputs.is_empty();

    for arg in &fn_decl.inputs {
        let arg_name = pprust::pat_to_string(&*arg.pat);
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

fn ty_to_c(path: &ast::Path, name: Option<&str>) -> Result {
    let new_type;

    // I don't think this is possible.
    if path.segments.is_empty() {
        return Err((
            path.span,
            "what the fuck have you done to this type?! this may be a bug".to_owned()
        ));
    // Types in modules, `my_mod::MyType`.
    } else if path.segments.len() > 1 {
        let module: &str = &path.segments[0].identifier.name.as_str();
        let ty: &str = &path.segments.last()
            .expect("already checked that there were at least two elements")
            .identifier.name.as_str();

        if module != "libc" {
            return Err((path.span, format!(
                "cheddar can not handle types in modules (except `libc`), try `use {}::{}`",
                module, ty,
            )));
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

impl CheddarPass {
    fn parse_ty(&mut self, context: &EarlyContext, item: &Item) -> Result {
        let (_, docs) = parse_attr(&item.attrs, |_| true, |attr| retrieve_docstring(attr, ""));

        let mut buffer = String::new();
        buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        let new_type = match item.node {
            Item_::ItemTy(ref ty, ref generics) => {
                // Can not yet convert generics.
                if generics.is_parameterized() { return Ok(None); }

                try_some!(rust_to_c(&*ty, Some(&name)))
            },
            _ => {
                context.sess.span_fatal(item.span, "`parse_ty` called on incorrect `Item_`");
            },
        };

        buffer.push_str(&format!("typedef {};\n\n", new_type));

        Ok(Some(buffer))
    }

    fn parse_enum(&mut self, context: &EarlyContext, item: &Item) -> Result {
        let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, |attr| retrieve_docstring(attr, ""));
        // If it's not #[repr(C)] then it can't be called from C.
        if !repr_c { return Ok(None); }

        let mut buffer = String::new();
        buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        buffer.push_str(&format!("typedef enum {} {{\n", name));
        if let Item_::ItemEnum(ref definition, ref generics) = item.node {
            if generics.is_parameterized() {
                return Err((item.span, "cheddar can not handle parameterized `#[repr(C)]` enums".to_owned()));
            }

            for var in &definition.variants {
                if !var.node.data.is_unit() {
                    return Err((var.span, "cheddar can not handle `#[repr(C)]` enums with non-unit variants".to_owned()));
                }

                let (_, docs) = parse_attr(&var.node.attrs, |_| true, |attr| retrieve_docstring(attr, "\t"));
                buffer.push_str(&docs);

                buffer.push_str(&format!("\t{},\n", pprust::variant_to_string(var)));
            }
        } else {
            context.sess.span_fatal(item.span, "`parse_enum` called in wrong `Item_`");
        }

        buffer.push_str(&format!("}} {};\n\n", name));

        Ok(Some(buffer))
    }

    fn parse_struct(&mut self, context: &EarlyContext, item: &Item) -> Result {
        let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, |attr| retrieve_docstring(attr, ""));
        // If it's not #[repr(C)] then it can't be called from C.
        if !repr_c { return Ok(None); }

        let mut buffer = String::new();
        buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        buffer.push_str(&format!("typedef struct {} {{\n", name));

        if let Item_::ItemStruct(ref variants, ref generics) = item.node {
            if generics.is_parameterized() {
                return Err((item.span, "cheddar can not handle parameterized `#[repr(C)]` structs".to_owned()));
            }

            if variants.is_struct() {
                for field in variants.fields() {
                    let (_, docs) = parse_attr(&field.node.attrs, |_| true, |attr| retrieve_docstring(attr, "\t"));
                    buffer.push_str(&docs);

                    let name = match field.node.ident() {
                        Some(name) => name.name.as_str(),
                        None => context.sess.span_fatal(field.span, "a tuple struct snuck through"),
                    };
                    let ty = try_some!(rust_to_c(&*field.node.ty, Some(&name)));
                    buffer.push_str(&format!("\t{};\n", ty));
                }
            } else {
                return Err((item.span, "cheddar can not handle unit or tuple `#[repr(C)]` structs".to_owned()));
            }
        } else {
            context.sess.span_fatal(item.span, "`parse_struct` called on wrong `Item_`");
        }

        buffer.push_str(&format!("}} {};\n\n", name));

        Ok(Some(buffer))
    }

    fn parse_fn(&mut self, context: &EarlyContext, item: &Item) -> Result {
        let (no_mangle, docs) = parse_attr(&item.attrs, check_no_mangle, |attr| retrieve_docstring(attr, ""));
        // If it's not #[no_mangle] then it can't be called from C.
        if !no_mangle { return Ok(None); }

        let mut buffer = String::new();
        let name = item.ident.name.as_str();

        if let Item_::ItemFn(ref fn_decl, _, _, abi, ref generics, _) = item.node {
            match abi {
                // If it doesn't have a C ABI it can't be called from C.
                Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {},
                _ => return Ok(None),
            }
            if generics.is_parameterized() {
                return Err((item.span, "cheddar can not handle parameterized extern functions".to_owned()));
            }

            let fn_decl: &ast::FnDecl = &*fn_decl;
            let output_type = &fn_decl.output;
            let output_type = match *output_type {
                ast::FunctionRetTy::NoReturn(span) => {
                    return Err((span, "panics across a C boundary are naughty!".to_owned()));
                },
                ast::FunctionRetTy::DefaultReturn(..) => "void".to_owned(),
                ast::FunctionRetTy::Return(ref ty) => try_some!(rust_to_c(&*ty, None)),
            };

            buffer.push_str(&docs);
            buffer.push_str(&format!("{} {}(", output_type, name));

            let has_args = !fn_decl.inputs.is_empty();

            for arg in &fn_decl.inputs {
                let arg_name = pprust::pat_to_string(&*arg.pat);
                let arg_type = try_some!(rust_to_c(&*arg.ty, Some(&arg_name)));
                buffer.push_str(&format!("{}, ", arg_type));
            }

            if has_args {
                // Remove the trailing comma and space.
                buffer.pop();
                buffer.pop();
            }

            buffer.push_str(");\n\n");
        } else {
            context.sess.span_fatal(item.span, "`parse_fn` called on wrong `Item_`");
        }

        Ok(Some(buffer))
    }
}


fn file_name_from_plugin_args(reg: &mut rustc_plugin::Registry) -> std::result::Result<Option<PathBuf>, ()> {
    let args = reg.args();
    if args.is_empty() {
        Ok(None)
    } else {
        // All plugin arguments should be `MetaWord`s.
        // Last argument is the file name without the ".h" extension.
        // E.g.
        //     #![plugin(cheddar(target, debug, include, my_header))]
        let mut temp_pathbuf = PathBuf::new();
        let len = args.len();

        // Push the given directories.
        // Don't iterate over the last element since that needs to be converted into a file.
        #[allow(needless_range_loop)]
        for i in 0..len-1 {
            temp_pathbuf.push(match args[i].node {
                ast::MetaItem_::MetaWord(ref string) => {
                    let string_slice: &str = &string;
                    String::from(string_slice)
                },
                _ => {
                    reg.sess.span_err(args[i].span, "cheddar plugin args must be `MetaWord`s");
                    return Err(());
                },
            })
        }

        // Create all the directories before we push the file name.
        if let Err(error) = fs::create_dir_all(&temp_pathbuf) {
            reg.sess.err(&format!("could not create directories in '{}': {}", temp_pathbuf.display(), error));
            return Err(());
        }

        // Push the header file name.
        temp_pathbuf.push(match args[len-1].node {
            ast::MetaItem_::MetaWord(ref string) => {
                    let string_slice: &str = &string;
                    String::from(string_slice)
            },
            _ => {
                reg.sess.span_err(args[len-1].span, "cheddar plugin args must be `MetaWord`s");
                return Err(());
            },
        });

        temp_pathbuf.set_extension("h");
        Ok(Some(temp_pathbuf))
    }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut rustc_plugin::registry::Registry) {
    let file = match file_name_from_plugin_args(reg) {
        // Error messages are done in `file_name_from_plugin_args`.
        Err(_) => return,
        Ok(file) => file,
    }
        // If no file was specified in the arguments try using the crate name.
        .or(reg.sess.opts.crate_name.clone()
            // Crate name is a String so convert it.
            .map(|name| PathBuf::from(name)
                 .with_extension("h")))
        // If there is no crate name try using the source file name.
        .or(reg.sess.local_crate_source_file.clone()
            // Don't want the full path.
            .and_then(|file| file.file_name()
                      // `.file_name()` returns an Option<OsStr>.
                      .map(PathBuf::from))
            .map(|file| file.with_extension("h")))
        // If all else fails...
        .unwrap_or(PathBuf::from("cheddar.h"));

    reg.register_early_lint_pass(box CheddarPass { file: file });
}
