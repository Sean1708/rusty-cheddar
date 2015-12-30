//! Functions for converting Rust types to C types.

use syntax::ast;
use syntax::codemap;
use syntax::print;

use Error;
use Level;

// TODO: C function pointers _must_ have a name associated with them but this Option business feels
//       like a shit way to handle that
//     - maybe have a named_rust_to_c which allows fn_pointers and rust_to_c doesn't?
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
