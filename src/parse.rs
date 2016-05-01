//! Functions for actually parsing the source file.

use syntax::ast;
use syntax::print;

use types;
use Error;
use Level;

// TODO: we should use our own parse state which tracks what types are callable from C
//     - then we can give decent errors for ones that aren't
//     - will require multiple passes of the module
//         - each time a struct, enum, or typedef changes do another pass
//         - only store errors on the final pass
//             - maybe there will be some errors which will need to be stored before then
//     - search inside struct as well for whitelisted types
//     - possibly also search other crates when encountering a path

/// Check that an expected path has been `pub use`d.
fn check_pub_use(item: &ast::Item, expected: &ast::Path) -> bool {
    if let ast::Item_::ItemUse(ref path) = item.node {
        // API has to be public to be used.
        if let ast::Visibility::Public = item.vis {
            // Easiest way to ensure all of API has been brought into scope.
            if let ast::ViewPath_::ViewPathGlob(ref path) = path.node {
                return path.segments == expected.segments;
            }
        }
    }

    false
}


/// The main entry point when looking for a specific module.
///
/// Determines which module to parse, ensures it is `pub use`ed then hands off to
/// `cheddar::parse::parse_mod`.
pub fn parse_crate(krate: &ast::Crate, path: &ast::Path) -> Result<String, Vec<Error>> {
    // First look to see if the module has been `pub use`d.
    if !krate.module.items.iter().any(|item| check_pub_use(&item, &path)) {
        return Err(vec![
            Error {
                level: Level::Error,
                span: None,
                message: format!("module `{}` has not been brought into global scope", path),
            },
            Error {
                level: Level::Help,
                span: None,
                message: format!("try putting `pub use {}::*` in your root source file", path),
            },
        ]);
    }

    // For each module in the path, look for the corresponding module in the source.
    let mut current_module = &krate.module;
    for module in &path.segments {
        let mut found = false;
        for item in &current_module.items {
            if let ast::Item_::ItemMod(ref new_module) = item.node {
                if module.identifier == item.ident {
                    current_module = new_module;
                    found = true;
                    break;
                }
            }
        }

        if !found {
            return Err(vec![Error {
                level: Level::Fatal,
                span: None,
                message: format!("module `{}` could not be found", module.identifier),
            }]);
        }
    }

    parse_mod(&current_module)
}

/// The manager of rusty-cheddar and entry point when the crate is the module.
///
/// Iterates through all items in the module and dispatches to correct methods, then pulls all
/// the results together into a header.
pub fn parse_mod(module: &ast::Mod) -> Result<String, Vec<Error>> {
    let mut buffer = String::new();
    let mut errors = vec![];
    for item in &module.items {
        // If it's not visible it can't be called from C.
        if let ast::Visibility::Inherited = item.vis { continue; }

        // Dispatch to correct method.
        let res = match item.node {
            // TODO: Check for ItemStatic and ItemConst as well.
            //     - How would this work?
            //     - Is it even possible?
            ast::Item_::ItemTy(..) => parse_ty(item),
            ast::Item_::ItemEnum(..) => parse_enum(item),
            ast::Item_::ItemStruct(..) => parse_struct(item),
            ast::Item_::ItemFn(..) => parse_fn(item),
            _ => Ok(None),
        };

        match res {
            // Display any non-fatal errors, fatal errors are handled at cause.
            Err(error) => errors.push(error),
            Ok(Some(buf)) => buffer.push_str(&buf),
            // TODO: put span notes in these or would that just get annoying?
            Ok(None) => {},  // Item should not be written to header.
        };
    }

    if errors.is_empty() {
        Ok(buffer)
    } else {
        Err(errors)
    }
}

/// Convert `pub type A = B;` into `typedef B A;`.
///
/// Aborts if A is generic.
fn parse_ty(item: &ast::Item) -> Result<Option<String>, Error> {
    let (_, docs) = parse_attr(&item.attrs, |_| true, |attr| retrieve_docstring(attr, ""));

    let mut buffer = String::new();
    buffer.push_str(&docs);

    let name = item.ident.name.as_str();
    let new_type = match item.node {
        ast::Item_::ItemTy(ref ty, ref generics) => {
            // Can not yet convert generics.
            if generics.is_parameterized() { return Ok(None); }

            try_some!(types::rust_to_c(&*ty, &name))
        },
        _ => {
            return Err(Error {
                level: Level::Bug,
                span: Some(item.span),
                message: "`parse_ty` called on wrong `Item_`".into(),
            });
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
fn parse_enum(item: &ast::Item) -> Result<Option<String>, Error> {
    let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, |attr| retrieve_docstring(attr, ""));
    // If it's not #[repr(C)] then it can't be called from C.
    if !repr_c { return Ok(None); }

    let mut buffer = String::new();
    buffer.push_str(&docs);

    let name = item.ident.name.as_str();
    buffer.push_str(&format!("typedef enum {} {{\n", name));
    if let ast::Item_::ItemEnum(ref definition, ref generics) = item.node {
        if generics.is_parameterized() {
            return Err(Error {
                level: Level::Error,
                span: Some(item.span),
                message: "cheddar can not handle parameterized `#[repr(C)]` enums".into(),
            });
        }

        for var in &definition.variants {
            if !var.node.data.is_unit() {
                return Err(Error {
                    level: Level::Error,
                    span: Some(var.span),
                    message: "cheddar can not handle `#[repr(C)]` enums with non-unit variants".into(),
                });
            }

            let (_, docs) = parse_attr(&var.node.attrs, |_| true, |attr| retrieve_docstring(attr, "\t"));
            buffer.push_str(&docs);

            buffer.push_str(&format!("\t{},\n", print::pprust::variant_to_string(var)));
        }
    } else {
        return Err(Error {
            level: Level::Bug,
            span: Some(item.span),
            message: "`parse_enum` called on wrong `Item_`".into(),
        });
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
fn parse_struct(item: &ast::Item) -> Result<Option<String>, Error> {
    let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, |attr| retrieve_docstring(attr, ""));
    // If it's not #[repr(C)] then it can't be called from C.
    if !repr_c { return Ok(None); }

    let mut buffer = String::new();
    buffer.push_str(&docs);

    let name = item.ident.name.as_str();
    buffer.push_str(&format!("typedef struct {}", name));

    if let ast::Item_::ItemStruct(ref variants, ref generics) = item.node {
        if generics.is_parameterized() {
            return Err(Error {
                level: Level::Error,
                span: Some(item.span),
                message: "cheddar can not handle parameterized `#[repr(C)]` structs".into(),
            });
        }

        if variants.is_struct() {
            buffer.push_str(" {\n");

            for field in variants.fields() {
                let (_, docs) = parse_attr(&field.node.attrs, |_| true, |attr| retrieve_docstring(attr, "\t"));
                buffer.push_str(&docs);

                let name = match field.node.ident() {
                    Some(name) => name.name.as_str(),
                    None => unreachable!("a tuple struct snuck through"),
                };
                let ty = try_some!(types::rust_to_c(&*field.node.ty, &name));
                buffer.push_str(&format!("\t{};\n", ty));
            }

            buffer.push_str("}");
        } else if variants.is_tuple() && variants.fields().len() == 1 {
            // #[repr(C)] pub struct Foo(Bar);  =>  typedef struct Foo Foo;
        } else {
            return Err(Error {
                level: Level::Error,
                span: Some(item.span),
                message: "cheddar can not handle unit or tuple `#[repr(C)]` structs with >1 members".into(),
            });
        }
    } else {
        return Err(Error {
            level: Level::Bug,
            span: Some(item.span),
            message: "`parse_struct` called on wrong `Item_`".into(),
        });
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
fn parse_fn(item: &ast::Item) -> Result<Option<String>, Error> {
    let (no_mangle, docs) = parse_attr(&item.attrs, check_no_mangle, |attr| retrieve_docstring(attr, ""));
    // If it's not #[no_mangle] then it can't be called from C.
    if !no_mangle { return Ok(None); }

    let mut buffer = String::new();
    let name = item.ident.name.as_str();
    buffer.push_str(&docs);

    if let ast::Item_::ItemFn(ref fn_decl, _, _, abi, ref generics, _) = item.node {
        use syntax::abi::Abi;
        match abi {
            // If it doesn't have a C ABI it can't be called from C.
            Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {},
            _ => return Ok(None),
        }

        if generics.is_parameterized() {
            return Err(Error {
                level: Level::Error,
                span: Some(item.span),
                message: "cheddar can not handle parameterized extern functions".into(),
            });
        }

        let fn_decl: &ast::FnDecl = &*fn_decl;
        // Handle the case when the return type is a function pointer (which requires that the
        // entire declaration is wrapped by the function pointer type) by first creating the name
        // and parameters, then passing that whole thing to `rust_to_c`.
        let mut buf_without_return = format!("{}(", name);

        let has_args = !fn_decl.inputs.is_empty();

        for arg in &fn_decl.inputs {
            use syntax::ast::{PatIdent, BindingMode};
            let arg_name = match arg.pat.node {
                PatIdent(BindingMode::ByValue(_), ref ident, None) => {
                    ident.node.name.to_string()
                }
                _ => panic!("hi")
            };
            let arg_type = try_some!(types::rust_to_c(&*arg.ty, &arg_name));
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
            ast::FunctionRetTy::NoReturn(span) => {
                return Err(Error {
                    level: Level::Error,
                    span: Some(span),
                    message: "panics across a C boundary are naughty!".into(),
                });
            },
            ast::FunctionRetTy::DefaultReturn(..) => format!("void {}", buf_without_return),
            ast::FunctionRetTy::Return(ref ty) => try_some!(types::rust_to_c(&*ty, &buf_without_return)),
        };

        buffer.push_str(&full_declaration);
        buffer.push_str(";\n\n");
    } else {
        return Err(Error {
            level: Level::Bug,
            span: Some(item.span),
            message: "`parse_fn` called on wrong `Item_`".into(),
        });
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
