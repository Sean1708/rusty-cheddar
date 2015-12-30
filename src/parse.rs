//! Functions for actually parsing the source file.

use std::path;
use std::io::Write;

use syntax::ast;
use syntax::parse;
use syntax::print;

use ::types;

use ::Result;


/// The main entry point when looking for a specific module.
///
/// Determines which module to parse, ensures it is `pub use`ed then hands off to
/// `Cheddar::parse_mod`.
pub fn parse_crate(sess: &parse::ParseSess, krate: &ast::Crate, module: &str, file: &path::Path) {
    let mut mod_item = None;
    let mut pub_used = false;

    // Find the module.
    for item in &krate.module.items {
        match item.node {
            ast::Item_::ItemMod(ref inner_mod) => {
                let name: &str = &item.ident.name.as_str();
                if name == module {
                    mod_item = Some(inner_mod);
                }
            },
            ast::Item_::ItemUse(ref path) => {
                if let ast::Visibility::Public = item.vis {
                    if let ast::ViewPath_::ViewPathGlob(ref path) = path.node {
                        if let Some(path) = path.segments.first() {
                            let path: &str = &path.identifier.name.as_str();
                            if path == module {
                                pub_used = true;
                            }
                        }
                    }
                }
            },
            _ => {},
        }
    }

    if let Some(mod_item) = mod_item {
        if pub_used {
            parse_mod(sess, &mod_item, file);
        } else {
            sess.span_diagnostic.err(&format!(
                "C api must exist in top level module, try `pub use {}::*`",
                module,
            ));
        }
    } else {
        sess.span_diagnostic.err(&format!("could not find module '{}'", module));
    }
}

/// The manager of rusty-cheddar and entry point when the crate is the module.
///
/// Iterates through all items in the module and dispatches to correct methods, then pulls all
/// the results together into a header.
pub fn parse_mod(sess: &parse::ParseSess, module: &ast::Mod, file: &path::Path) {
    let mut buffer = format!(
        "#ifndef cheddar_gen_{0}_h\n#define cheddar_gen_{0}_h\n\n",
        file.file_stem().and_then(|p| p.to_str()).unwrap_or("default"),
    );
    buffer.push_str("#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n");
    buffer.push_str("#include <stdint.h>\n#include <stdbool.h>\n\n");

    for item in &module.items {
        // If it's not visible it can't be called from C.
        if let ast::Visibility::Inherited = item.vis { continue; }

        // Dispatch to correct method.
        let res = match item.node {
            // TODO: Check for ItemStatic and ItemConst as well.
            //     - How would this work?
            //     - Is it even possible?
            ast::Item_::ItemTy(..) => parse_ty(sess, item),
            ast::Item_::ItemEnum(..) => parse_enum(sess, item),
            ast::Item_::ItemStruct(..) => parse_struct(sess, item),
            ast::Item_::ItemFn(..) => parse_fn(sess, item),
            _ => Ok(None),
        };

        // Display any non-fatal errors, fatal errors are handled at cause.
        match res {
            Err((span, msg)) => sess.span_diagnostic.span_err(span, &msg),
            Ok(Some(buf)) => buffer.push_str(&buf),
            // TODO: put span notes in these or would that just get annoying?
            Ok(None) => {},  // Item should not be written to header.
        };
    }

    buffer.push_str("#ifdef __cplusplus\n}\n#endif\n\n");
    buffer.push_str("#endif\n");

    let bytes_buf = buffer.into_bytes();

    if let Err(error) = ::std::fs::File::create(&file).and_then(|mut f| f.write_all(&bytes_buf)) {
        sess.span_diagnostic.err(&format!("could not write to '{}': {}", file.display(), error))
    };
}

/// Convert `pub type A = B;` into `typedef B A;`.
///
/// Aborts if A is generic.
fn parse_ty(sess: &parse::ParseSess, item: &ast::Item) -> Result {
    let (_, docs) = parse_attr(&item.attrs, |_| true, |attr| retrieve_docstring(attr, ""));

    let mut buffer = String::new();
    buffer.push_str(&docs);

    let name = item.ident.name.as_str();
    let new_type = match item.node {
        ast::Item_::ItemTy(ref ty, ref generics) => {
            // Can not yet convert generics.
            if generics.is_parameterized() { return Ok(None); }

            try_some!(types::rust_to_c(&*ty, Some(&name)))
        },
        _ => {
            fatal!(sess, item.span, "`parse_ty` called on incorrect `Item_`");
        },
    };

    buffer.push_str(&format!("typedef {};\n\n", new_type));

    Ok(Some(buffer))
}

/// Convert a Rust enum into a C enum.
///
/// The Rust enum must be marked with `#[repr(C)]` and must be public otherwise the function
/// will abort.
///
/// Cheddar will error if the enum if generic or if it contains non-unit variants.
fn parse_enum(sess: &parse::ParseSess, item: &ast::Item) -> Result {
    let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, |attr| retrieve_docstring(attr, ""));
    // If it's not #[repr(C)] then it can't be called from C.
    if !repr_c { return Ok(None); }

    let mut buffer = String::new();
    buffer.push_str(&docs);

    let name = item.ident.name.as_str();
    buffer.push_str(&format!("typedef enum {} {{\n", name));
    if let ast::Item_::ItemEnum(ref definition, ref generics) = item.node {
        if generics.is_parameterized() {
            return Err((item.span, "cheddar can not handle parameterized `#[repr(C)]` enums".to_owned()));
        }

        for var in &definition.variants {
            if !var.node.data.is_unit() {
                return Err((var.span, "cheddar can not handle `#[repr(C)]` enums with non-unit variants".to_owned()));
            }

            let (_, docs) = parse_attr(&var.node.attrs, |_| true, |attr| retrieve_docstring(attr, "\t"));
            buffer.push_str(&docs);

            buffer.push_str(&format!("\t{},\n", print::pprust::variant_to_string(var)));
        }
    } else {
        fatal!(sess, item.span, "`parse_enum` called in wrong `Item_`");
    }

    buffer.push_str(&format!("}} {};\n\n", name));

    Ok(Some(buffer))
}

/// Convert a Rust struct into a C struct.
///
/// The rust struct must be marked `#[repr(C)]` and must be public otherwise the function will
/// abort.
///
/// Cheddar will error if the struct is generic or if the struct is a unit or tuple struct.
fn parse_struct(sess: &parse::ParseSess, item: &ast::Item) -> Result {
    let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, |attr| retrieve_docstring(attr, ""));
    // If it's not #[repr(C)] then it can't be called from C.
    if !repr_c { return Ok(None); }

    let mut buffer = String::new();
    buffer.push_str(&docs);

    let name = item.ident.name.as_str();
    buffer.push_str(&format!("typedef struct {}", name));

    if let ast::Item_::ItemStruct(ref variants, ref generics) = item.node {
        if generics.is_parameterized() {
            return Err((item.span, "cheddar can not handle parameterized `#[repr(C)]` structs".to_owned()));
        }

        // TODO: refactor this into mutliple methods.
        if variants.is_struct() {
            buffer.push_str(" {\n");

            for field in variants.fields() {
                let (_, docs) = parse_attr(&field.node.attrs, |_| true, |attr| retrieve_docstring(attr, "\t"));
                buffer.push_str(&docs);

                let name = match field.node.ident() {
                    Some(name) => name.name.as_str(),
                    None => unreachable!("a tuple struct snuck through"),
                };
                let ty = try_some!(types::rust_to_c(&*field.node.ty, Some(&name)));
                buffer.push_str(&format!("\t{};\n", ty));
            }

            buffer.push_str("}");
        } else if variants.is_tuple() && variants.fields().len() == 1 {
            // #[repr(C)] pub struct Foo(Bar);  =>  typedef struct Foo Foo;
        } else {
            return Err((
                item.span,
                "cheddar can not handle unit or tuple `#[repr(C)]` structs with >1 members".to_owned()
            ));
        }
    } else {
        fatal!(sess, item.span, "`parse_struct` called on wrong `Item_`");
    }

    buffer.push_str(&format!(" {};\n\n", name));

    Ok(Some(buffer))
}

/// Convert a Rust function declaration into a C function declaration.
///
/// The function declaration must be marked `#[no_mangle]` and have a C ABI otherwise the
/// function will abort.
///
/// If the declaration is generic or diverges then cheddar will error.
fn parse_fn(sess: &parse::ParseSess, item: &ast::Item) -> Result {
    let (no_mangle, docs) = parse_attr(&item.attrs, check_no_mangle, |attr| retrieve_docstring(attr, ""));
    // If it's not #[no_mangle] then it can't be called from C.
    if !no_mangle { return Ok(None); }

    let mut buffer = String::new();
    let name = item.ident.name.as_str();

    if let ast::Item_::ItemFn(ref fn_decl, _, _, abi, ref generics, _) = item.node {
        use syntax::abi::Abi;
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
            ast::FunctionRetTy::Return(ref ty) => try_some!(types::rust_to_c(&*ty, None)),
        };

        buffer.push_str(&docs);
        buffer.push_str(&format!("{} {}(", output_type, name));

        let has_args = !fn_decl.inputs.is_empty();

        for arg in &fn_decl.inputs {
            let arg_name = print::pprust::pat_to_string(&*arg.pat);
            let arg_type = try_some!(types::rust_to_c(&*arg.ty, Some(&arg_name)));
            buffer.push_str(&format!("{}, ", arg_type));
        }

        if has_args {
            // Remove the trailing comma and space.
            buffer.pop();
            buffer.pop();
        }

        buffer.push_str(");\n\n");
    } else {
        fatal!(sess, item.span, "`parse_fn` called on wrong `Item_`");
    }

    Ok(Some(buffer))
}


// TODO: Maybe it would be wise to use syntax::attr here.
/// Loop through a list of attributes.
///
/// Check that at least one attribute matches some criteria (usually #[repr(C)] or #[no_mangle])
/// and optionally retrieve a String from it (usually a docstring).
fn parse_attr<C, R>(attrs: &[ast::Attribute], check: C, retrieve: R) -> (bool, String)
    where C: Fn(&ast::Attribute) -> bool,
          R: Fn(&ast::Attribute) -> Option<String>,
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

/// Check the attribute is #[repr(C)].
fn check_repr_c(attr: &ast::Attribute) -> bool {
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

/// Check the attribute is #[no_mangle].
fn check_no_mangle(attr: &ast::Attribute) -> bool {
    match attr.node.value.node {
        ast::MetaItem_::MetaWord(ref name) if *name == "no_mangle" => true,
        _ => false,
    }
}

/// If the attribute is  a docstring, indent it the required amount and return it.
fn retrieve_docstring(attr: &ast::Attribute, prepend: &str) -> Option<String> {
    match attr.node.value.node {
        ast::MetaItem_::MetaNameValue(ref name, ref val) if *name == "doc" => match val.node {
            // Docstring attributes omit the trailing newline.
            ast::Lit_::LitStr(ref docs, _) => Some(format!("{}{}\n", prepend, docs)),
            _ => unreachable!("docs must be literal strings"),
        },
        _ => None,
    }
}
