//! Helper functions for turning Rust types into string representations of C types.

use compiler::Stop;
use syntax::ast;
use syntax::codemap;
use syntax::print;

// TODO: GET RID OF SUPER!!! AND ::!!!
//     ACTUALLY THIS MIGHT BE A GOOD USE OF SUPER!!!

/// Turn a Rust type with an associated name or type into a C type.
///
/// `assoc` is required because of function pointers. Function pointers have to "wrap" something,
/// e.g.
///
/// ```c
/// float* (*my_func)(int a, char b);
/// ```
///
/// where the function pointer "wraps" the name `my_func`, or
///
/// ```c
/// void (*foo(int bar, void* baz))(char* bux);
/// ```
///
/// where the function pointer "wraps" the partial declaration
///
/// ```c
/// foo(int bar, void* baz)
/// ```
///
/// . `assoc` is therefore the thing which the function pointer "wraps".
pub fn rust_to_c(session: &mut ::parse::Session, ty: &ast::Ty, assoc: &str, spec: super::Spec) -> Result<String, Stop> {
    match ty.node {
        // Function pointers make life an absolute pain here.
        ast::Ty_::TyBareFn(ref bare_fn) => fn_ptr_to_c(session, bare_fn, ty.span, assoc, spec),
        // All other types just have a name associated with them.
        _ => Ok(format!("{} {}", try!(anon_rust_to_c(session, ty, spec)), assoc)),
    }
}

/// Turn a Rust type into a C type.
fn anon_rust_to_c(session: &mut ::parse::Session, ty: &ast::Ty, spec: super::Spec) -> Result<String, Stop> {
    match ty.node {
        // Function pointers should not be in this function.
        ast::Ty_::TyBareFn(..) => Err(session.span_err(
            ty.span,
            "C function pointers must have a name or function declaration associated with them",
        )),
        // Standard pointers.
        ast::Ty_::TyPtr(ref ptr) => ptr_to_c(session, ptr, spec),
        // Plain old types.
        ast::Ty_::TyPath(None, ref path) => path_to_c(session, path),
        // Possibly void, likely not.
        _ => {
            let new_type = print::pprust::ty_to_string(ty);
            if new_type == "()" {
                Ok("void".into())
            } else {
                Err(session.span_err(
                    ty.span,
                    &format!("cheddar can not handle the type `{}`", new_type),
                ))
            }
        },
    }
}

/// Turn a Rust pointer (*mut or *const) into the correct C form.
fn ptr_to_c(session: &mut ::parse::Session, ty: &ast::MutTy, spec: super::Spec) -> Result<String, Stop> {
    let new_type = try!(anon_rust_to_c(session, &ty.ty, spec));
    let const_spec = match ty.mutbl {
        // *const T
        ast::Mutability::MutImmutable => {
            // Avoid multiple `const` specifiers (you can't have `const const int**` in C).
            // TODO: but you can have `int const* const*`
            if new_type.starts_with("const ") {
                ""
            } else {
                "const "
            }
        },
        // *mut T
        ast::Mutability::MutMutable => "",
    };

    Ok(format!("{}{}*", const_spec, new_type))
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
fn fn_ptr_to_c(session: &mut ::parse::Session, fn_ty: &ast::BareFnTy, fn_span: codemap::Span, inner: &str, spec: super::Spec) -> Result<String, Stop> {
    use syntax::abi::Abi;
    match fn_ty.abi {
        // If it doesn't have a C ABI it can't be called from C.
        Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {},
        _ => return Err(Stop::Abort),
    }

    if !fn_ty.lifetimes.is_empty() {
        return Err(session.span_err(fn_span, "cheddar can not handle lifetimes"));
    }

    let fn_decl: &ast::FnDecl = &*fn_ty.decl;

    let mut buf_without_return = format!("(*{})(", inner);

    let has_args = !fn_decl.inputs.is_empty();

    for arg in &fn_decl.inputs {
        // TODO: ast_util::pat_is_ident
        let arg_name = print::pprust::pat_to_string(&*arg.pat);
        let arg_type = try!(rust_to_c(session, &*arg.ty, &arg_name, spec));
        buf_without_return.push_str(&arg_type);
        buf_without_return.push_str(", ");
    }

    if has_args {
        // Remove the trailing comma and space.
        buf_without_return.pop();
        buf_without_return.pop();
    }

    buf_without_return.push_str(")");

    let output_type = &fn_decl.output;
    let full_declaration = match *output_type {
        ast::FunctionRetTy::NoReturn(..) => {
            if spec >= super::Spec::C11 {
                format!("noreturn void {}", buf_without_return)
            } else {
                format!("void {}", buf_without_return)
            }
        },
        ast::FunctionRetTy::DefaultReturn(..) => format!("void {}", buf_without_return),
        ast::FunctionRetTy::Return(ref ty) => try!(rust_to_c(session, &*ty, &buf_without_return, spec)),
    };


    Ok(full_declaration)
}

/// Convert a Rust path type (my_mod::MyType) to a C type.
///
/// Types hidden behind modules are almost certainly custom types (which wouldn't work) except
/// types in `libc` which we special case.
fn path_to_c(session: &::parse::Session, path: &ast::Path) -> Result<String, Stop> {
    // I don't think this is possible.
    if path.segments.is_empty() {
        session.span_bug(path.span, "what the fuck have you done to this type?!")
    // Types in modules, `my_mod::MyType`.
    } else if path.segments.len() > 1 {
        let module: &str = &path.segments[0].identifier.name.as_str();
        let ty: &str = &path.segments.last()
            .expect("already checked that there were at least two elements")
            .identifier.name.as_str();

        if module != "libc" {
            Err(session.span_err(
                path.span,
                "cheddar can not handle types in other modules (except `libc`)",
            ))
        } else {
            Ok(libc_ty_to_c(ty).into())
        }
    } else {
        Ok(rust_ty_to_c(&path.segments[0].identifier.name.as_str()).into())
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
    // rust_to_c_abort_generic
    // rust_to_c_fail_generic
    #[test]
    #[ignore]
    fn test_generics() {
        let name = "azazael";

        // TODO: this should be an error
        let source = "Result<f64, i32>";
        let typ = super::anon_rust_to_c(&ty(source))
            .expect(&format!("error while parsing {:?} with no name", source));
        assert!(typ.is_none(), "successfully parsed invalid type {:?} with no name", source);

        let source = "Option<i16>";
        let typ = super::rust_to_c(&ty(source), name)
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
            let parsed_c_type = super::anon_rust_to_c(&ty(rust_type))
                .expect(&format!("error while parsing {:?} with no name", rust_type))
                .expect(&format!("did not parse {:?} with no name", rust_type));
            assert_eq!(parsed_c_type, correct_c_type);

            let parsed_c_type = super::rust_to_c(&ty(rust_type), name)
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
            let parsed_c_type = super::anon_rust_to_c(&ty(rust_type))
                .expect(&format!("error while parsing {:?} with no name", rust_type))
                .expect(&format!("did not parse {:?} with no name", rust_type));
            assert_eq!(parsed_c_type, correct_c_type);

            let parsed_c_type = super::rust_to_c(&ty(rust_type), name)
                .expect(&format!("error while parsing {:?} with name {:?}", rust_type, name))
                .expect(&format!("did not parse {:?} with name {:?}", rust_type, name));
            assert_eq!(parsed_c_type, format!("{} {}", correct_c_type, name));
        }
    }

    #[test]
    fn test_const_pointers() {
        let name = "maalik";

        let source = "*const u8";
        let parsed_type = super::anon_rust_to_c(&ty(source))
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "const uint8_t*");

        let source = "*const ()";
        let parsed_type = super::rust_to_c(&ty(source), name)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("const void* {}", name));

        let source = "*const *const f64";
        let parsed_type = super::anon_rust_to_c(&ty(source))
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "const double**");

        let source = "*const *const i64";
        let parsed_type = super::rust_to_c(&ty(source), name)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("const int64_t** {}", name));
    }

    #[test]
    fn test_mut_pointers() {
        let name = "raphael";

        let source = "*mut u16";
        let parsed_type = super::anon_rust_to_c(&ty(source))
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "uint16_t*");

        let source = "*mut f32";
        let parsed_type = super::rust_to_c(&ty(source), name)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("float* {}", name));

        let source = "*mut *mut *mut i32";
        let parsed_type = super::anon_rust_to_c(&ty(source))
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "int32_t***");

        let source = "*mut *mut i8";
        let parsed_type = super::rust_to_c(&ty(source), name)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("int8_t** {}", name));
    }

    #[test]
    fn test_mixed_pointers() {
        let name = "samael";

        let source = "*const *mut *const bool";
        let parsed_type = super::anon_rust_to_c(&ty(source))
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "const bool***");

        let source = "*mut *mut *const libc::c_ulonglong";
        let parsed_type = super::rust_to_c(&ty(source), name)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("const unsigned long long*** {}", name));
    }

    #[test]
    fn test_function_pointers() {
        let name = "sariel";

        let source = "fn(a: bool)";
        let parsed_type = super::anon_rust_to_c(&ty(source));
        assert!(parsed_type.is_err(), "C function pointers should have an inner or name associated");

        let source = "fn(a: i8) -> f64";
        let parsed_type = super::rust_to_c(&ty(source), name)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name));
        assert!(parsed_type.is_none(), "parsed a non-C function pointer");

        let source = "extern fn(hi: libc::c_int) -> libc::c_double";
        let parsed_type = super::rust_to_c(&ty(source), name)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("double (*{})(int hi)", name));
    }

    #[test]
    fn test_paths() {
        let name = "zachariel";

        let source = "MyType";
        let parsed_type = super::anon_rust_to_c(&ty(source))
            .expect(&format!("error while parsing {:?} with no name", source))
            .expect(&format!("did not parse {:?} with no name", source));
        assert_eq!(parsed_type, "MyType");

        let source = "SomeType";
        let parsed_type = super::rust_to_c(&ty(source), name)
            .expect(&format!("error while parsing {:?} with name {:?}", source, name))
            .expect(&format!("did not parse {:?} with name {:?}", source, name));
        assert_eq!(parsed_type, format!("SomeType {}", name));

        let source = "my_mod::MyType";
        let parsed_type = super::anon_rust_to_c(&ty(source));
        assert!(parsed_type.is_err(), "can't use a multi-segment path which isn't `libc`");

        let source = "some_mod::SomeType";
        let parsed_type = super::rust_to_c(&ty(source), name);
        assert!(parsed_type.is_err(), "can't use a multi-segment path which isn't `libc`");
    }
}
