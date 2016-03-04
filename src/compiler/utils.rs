//! Useful functions for obtaining information from the AST.

use compiler::Stop;
use parse;
use syntax;
use syntax::ast;

// TODO: export useful `syntax` functions such as `variant_to_string`

/// Obtain the docstring from an item's attributes.
///
/// The comment characters (`///`, `/**`, or `*/`) and any remaining spaces are stripped from the
/// left- and right- sides of the lines before prepending `prepend` and appending `append` to each
/// line.
pub fn docs_from_attrs(attrs: &[ast::Attribute], prepend: &str, append: &str) -> String {
    let mut buffer = String::new();

    for attr in attrs {
        match attr.node.value.node {
            ast::MetaItem_::MetaNameValue(ref name, ref val) if *name == "doc" => match val.node {
                // Docstring attributes omit the trailing newline.
                ast::Lit_::LitStr(ref docs, _) => buffer.push_str(&format!(
                    "{}{}{}\n",
                    prepend,
                    // TODO: I wanted to do something like this
                    //     docs.trim().trim_matches(["///", "/**", "*/", "*"]).trim(),
                    //     which would trim all occurrences of those strings from left and right
                    docs.trim(),
                    append,
                )),
                _ => unreachable!("docs must be literal strings"),
            },
            _ => {},
        }
    }

    buffer
}

// TODO: there must be some way to get this to work
// pub fn name_from_ident(ident: &ast::Ident) -> &str {
//     &ident.name.as_str()
// }

/// Extracts a non-generic `Ty` from an `Item`.
pub fn ty_from_item<'i>(session: &mut parse::Session, item: &'i ast::Item) -> Result<&'i ast::Ty, Stop> {
    match item.node {
        ast::Item_::ItemTy(ref ty, ref generics) => {
            if generics.is_parameterized() {
                Err(Stop::Abort)
            } else {
                Ok(&*ty)
            }
        },
        _ => session.span_bug_with_note(
            item.span,
            "`ty_from_item` was called on an incorrect item",
            "expected `syntax::ast::Item_::ItemTy`",
        ),
    }
}

/// Extracts a non-generic `EnumDef` from an `Item`.
pub fn enum_from_item<'i>(session: &mut parse::Session, item: &'i ast::Item) -> Result<&'i ast::EnumDef, Stop> {
    match item.node {
        ast::Item_::ItemEnum(ref enum_def, ref generics) => {
            if generics.is_parameterized() {
                Err(Stop::Abort)
            } else {
                Ok(&enum_def)
            }
        },
        _ => session.span_bug_with_note(
            item.span,
            "`enum_from_item` was called on an incorrect item",
            "expected `syntax::ast::Item_::ItemEnum`",
        ),
    }
}

/// Extract the fields from a non-generic struct.
// TODO: does the commented out optimisation work?
pub fn struct_from_item<'f>(session: &mut parse::Session, item: &'f ast::Item) -> Result<&'f [ast::StructField], Stop> {
// pub fn struct_from_item(session: &mut parse::Session, item: &ast::Item) -> Result<Vec<ast::StructField>, Stop> {
    match item.node {
        ast::Item_::ItemStruct(ast::VariantData::Struct(ref fields, _), ref generics) => {
            if generics.is_parameterized() {
                Err(Stop::Abort)
            } else {
                Ok(fields)
            }
        },
        ast::Item_::ItemStruct(..) => Err(session.span_err(
            item.span,
            "unit and tuple structs have no C equivalent",
        )),
        _ => session.span_bug_with_note(
            item.span,
            "`struct_from_item` was called on an incorrect item",
            "expected `syntax::ast::Item_::ItemStruct`",
        ),
    }
}

/// Extract a non-generic `FnDecl` of C ABI.
pub fn fn_from_item<'i>(session: &mut parse::Session, item: &'i ast::Item) -> Result<&'i ast::FnDecl, Stop> {
    match item.node {
        ast::Item_::ItemFn(ref fn_decl, _, _, abi, ref generics, _) => {
            if generics.is_parameterized() || !is_c_abi(abi) {
                Err(Stop::Abort)
            } else {
                Ok(fn_decl)
            }
        },
        _ => session.span_bug_with_note(
            item.span,
            "`fn_decl_from_item` was called on an incorrect item",
            "expected `syntax::ast::Item_::ItemFn`",
        ),
    }
}

pub fn for_each_variant<F>(session: &mut parse::Session, definition: &ast::EnumDef, mut f: F) -> Result<(), Stop>
where F: FnMut(&ast::Variant_) {
    for var in &definition.variants {
        if !var.node.data.is_unit() {
            return Err(session.span_err(
                var.span,
                "non-unit enum variants have no C equivalent",
            ))
        }

        f(&var.node);
    }

    Ok(())
}


/// Check whether the given path type contains any parameters.
pub fn ty_is_generic(ty: &ast::Ty) -> bool {
    match ty.node {
        ast::Ty_::TyPath(_, ref path) => path.segments.iter().any(
            |segment| !segment.parameters.is_empty()
        ),
        _ => false,
    }
}

// TODO: implement this when fixing #30.
// /// Check whether the given path type contains any parameters.
// ///
// /// The exception is a nullable function pointer which is represented as
// ///
// /// ```ignore
// /// Option<extern fn(...) -> ...>
// /// ```
// fn ty_is_generic_not_null_fn_ptr(ty: &ast::Ty) -> bool {
//     false
// }

// TODO: implement this
// /// Check if there is an equivalent C type.
// pub fn ty_maps_to_c(ty: &ast::Ty) -> bool {
//     true
// }

/// Check whether any fields of a struct are public.
///
/// If all struct fields are private then the struct will be represented as an opaque struct.
pub fn is_opaque(fields: &[ast::StructField]) -> bool {
    fields.iter().all(
        |&syntax::codemap::Spanned{ node: ref field, .. }| match field.kind {
            ast::StructFieldKind::NamedField(_, ast::Visibility::Inherited) => true,
            ast::StructFieldKind::UnnamedField(ast::Visibility::Inherited) => true,
            _ => false,
        }
    )
}

/// Check whether the ABI is a C ABI.
pub fn is_c_abi(abi: syntax::abi::Abi) -> bool {
    use syntax::abi::Abi;
    match abi {
        Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => true,
        _ => false,
    }
}
