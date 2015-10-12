#![feature(rustc_private)]
#![feature(plugin_registrar)]
#![feature(box_syntax)]

extern crate rustc;
extern crate syntax;

use rustc::plugin;
use syntax::ast;
use syntax::ast::TokenTree;
use syntax::codemap;
use syntax::ext::base;
use syntax::parse::token;
use syntax::parse::token::InternedString;
use syntax::ptr;

use std::fs;
use std::iter;
use std::path;
use std::slice;

use std::io::Write;
use syntax::ext::build::AstBuilder;


// macro_rules! plugin_try {}
//     plugin_try!(cx, sp, expr);
//     match expr {
//         Ok(v) => v,
//         Err(e) => {
//             cx.span_err(sp, e.description());
//             return base::DummyResult::any(sp);
//         },
//     }

// TODO: add docstrings to the header file somehow.
fn parse_header(
    context: &mut base::ExtCtxt, span: codemap::Span, toktree: &[ast::TokenTree]
) -> Box<base::MacResult + 'static> {
    let mut toktree = toktree.iter().peekable();
    // TODO: turn option into error then use plugin_try!
    let possible_path = match toktree.peek() {
        Some(t) => t.clone(),
        None => {
            context.span_err(span, "macro contains no tokens");
            return base::DummyResult::any(span);
        },
    };
    // Check whether the first token specifies a path.
    let output_file_path = if let ast::TokenTree::TtToken(
        _, token::Token::Literal(token::Lit::Str_(s), _)
    ) = *possible_path {
        // Skip it if it does.
        toktree.next();
        path::PathBuf::from(s.as_str().to_string())
    } else {
        // otherwise build a default path
        // TODO: There must be a better way to do this!
        let mut temp = path::PathBuf::new();
        // TODO: what if they're not using cargo?
        temp.push("target");
        // TODO: temp.push("debug") for Debug builds, "release" for release builds
        temp.push("include");
        // TODO: a better default name
        temp.push("header.h");
        temp
    };

    // TODO: have a way to specify tabs vs x number of spaces
    //     - #[plugin(cheddar(spaces=4))] or #[plugin(cheddar(tabs))]
    //     - compute the string in plugin_registrar and save it as an environment variable
    //     - pull that environment variable in parse_header and pass it to the functions
    //     - is it really worth it?
    // TODO: properly handle errors with span_err, i.e. don't panic!
    //     - also use the correct spans, I'm ignoring most of them at them moment
    // TODO: spans don't need to be references.
    // TODO: can we automatically set the crate-type dylib attribute?
    // TODO: layout the header file as like for like with the cheddar! block (a *block* of cheddar. lol)
    //     - leave all comments, docstrings and items (enums, structs and fns) in same place
    // TODO: retain arbitrary attributes
    // TODO: I've just found syntax::parser, I should probably use that.

    // Create the parent directories for the header file.
    // TODO: Do we need the .with_file_name("")?
    fs::create_dir_all(output_file_path.with_file_name("")).ok().expect("Can not create directories for output file.");
    let mut output_file = fs::File::create(&output_file_path).ok().expect("Can not open the header file.");
    output_file.write_all(format!(
        // TODO: stdbool.h
        "#ifndef cheddar_gen_{0}_h\n#define cheddar_gen_{0}_h\n\n#include <stdint.h>\n#include <stdbool.h>\n\n\n",
        // TODO: can we use .to_str_lossy()? It uses unicode though so probably not.
        output_file_path.file_stem().expect("Why no file stem?").to_str().expect("File stem not stringable."),
    ).as_bytes()).ok().expect("Can not write guard to header file.");

    let mut enum_buf = String::new();
    let mut struct_buf = String::new();
    let mut func_buf = String::new();
    let mut items = vec![];

    let mut opt_cur = toktree.next();
    while let Some(cur) = opt_cur {
        match *cur {
            ast::TokenTree::TtToken(_, token::Token::Ident(n, _)) => match &*n.name.as_str() {
                "enum" => items.push(parse_enum(
                    &mut toktree, &mut enum_buf, context, &span
                ).ok().expect("Enumeration failure!")),
                "struct" => items.push(parse_struct(
                    &mut toktree, &mut struct_buf, context, &span
                ).ok().expect("Structural failure!")),
                "fn" => items.push(parse_func(
                    &mut toktree, &mut func_buf, context, span
                ).ok().expect("Functional failure!")),
                // Ignore all other tokens.
                _ => {},
            },
            // Ignore delimited blocks.
            _ => {},
        }
        opt_cur = toktree.next();
    }

    output_file.write_all(format!("// Enums\n\n{}\n", enum_buf).as_bytes())
        .ok().expect("Can not write enums to header file.");
    output_file.write_all(format!("// Structs\n\n{}\n", struct_buf).as_bytes())
        .ok().expect("Can not write structs to header file.");
    output_file.write_all(format!("// Functions\n\n{}\n", func_buf).as_bytes())
        .ok().expect("Can not write functions to header file.");
    output_file.write_all(b"#endif\n").ok().expect("Can not write endif to header file.");

    // println!("{:#?}", items);
    base::MacEager::items(syntax::util::small_vector::SmallVector::many(items))
}


fn parse_enum<'a>(
    toktree: &mut iter::Peekable<slice::Iter<'a, ast::TokenTree>>,
    buffer: &mut String,
    context: &mut base::ExtCtxt,
    span: &codemap::Span,
// TODO: return an error type
//     - what about just calling span_err inside this function?
//         - that way we have the Span to hand so we don't need to worry about passing it around
//     - alternatively return an error type which holds a span and &'static str
//         - this might be more conducive to a general macro approach
) -> Result<ptr::P<ast::Item>, String> {
    let ident = match toktree.next() {
        // Find the name of the enum.
        Some(&ast::TokenTree::TtToken(_, token::Token::Ident(i, _))) => i,
        _ => return Err("no enum identifier".to_owned()),
    };
    buffer.push_str(&format!("typedef enum {} {{\n", ident.name.as_str()));

    let mut variants = vec![];
    // Find the vector of enum variants.
    if let Some(&ast::TokenTree::TtDelimited(_, ref delims)) = toktree.next() {
        for tok in &delims.tts {
            // Find the names of the enum variants, ignoring other tokens.
            if let &ast::TokenTree::TtToken(_, token::Token::Ident(id, _)) = tok {
                variants.push(ptr::P(context.variant(*span, id, vec![])));
                buffer.push_str(&format!("\t{},\n", &*id.name.as_str()));
            }
        }
    } else {
        return Err("empty enums not supported".to_owned());
    }

    buffer.push_str(&format!("}} {};\n\n", ident.name.as_str()));

    // Create the #[repr(C)] attribute.
    let c = context.meta_word(*span, InternedString::new("C"));
    let repr = context.meta_list(*span, InternedString::new("repr"), vec![c]);
    // Create the enum.
    Ok(ptr::P(ast::Item {
        ident: ident,
        attrs: vec![context.attribute(*span, repr)],
        id: ast::DUMMY_NODE_ID,
        node: ast::Item_::ItemEnum(
            ast::EnumDef { variants: variants, },
            ast::Generics {
                lifetimes: vec![],
                ty_params: syntax::owned_slice::OwnedSlice::empty(),
                where_clause: ast::WhereClause {
                    id: ast::DUMMY_NODE_ID,
                    predicates: vec![],
                },
            },
        ),
        vis: ast::Visibility::Public,
        span: *span,
    }))
}

fn parse_struct<'a>(
    toktree: &mut iter::Peekable<slice::Iter<'a, ast::TokenTree>>,
    buffer: &mut String,
    context: &mut base::ExtCtxt,
    span: &codemap::Span,
) -> Result<ptr::P<ast::Item>, String> {
    let ident = match toktree.next() {
        // Find the name of the struct.
        Some(&ast::TokenTree::TtToken(_, token::Token::Ident(i, _))) => i,
        _ => return Err("no struct identifier".to_owned()),
    };
    buffer.push_str(&format!("typedef struct {} {{\n", ident.name.as_str()));

    let mut fields = vec![];
    // TODO: This is fucking hairy.
    // Find the struct fields and types, ignoring other tokens.
    if let Some(&ast::TokenTree::TtDelimited(_, ref delims)) = toktree.next() {
        let mut elem_iter = delims.tts.iter();
        let mut opt_elem = elem_iter.next();
        while let Some(elem) = opt_elem {
            // Find a field name.
            if let &ast::TokenTree::TtToken(_, token::Token::Ident(id, _)) = elem {
                // After the name should be a colon then the type.
                if let Some(&ast::TokenTree::TtToken(_, token::Token::Colon)) = elem_iter.next() {
                    if let Some(&ast::TokenTree::TtToken(_, token::Token::Ident(typ, _))) = elem_iter.next() {
                        buffer.push_str(&format!("\t{} {};\n", rust_to_c(&typ.name.as_str()), id.name.as_str()));
                        // TODO: does this need to use ident_of()?
                        let ty = context.ident_of(&typ.name.as_str());
                        fields.push(codemap::spanned(span.lo, span.hi, ast::StructField_ {
                            kind: ast::StructFieldKind::NamedField(id, ast::Visibility::Public),
                            id: ast::DUMMY_NODE_ID,
                            ty: context.ty_ident(*span, ty),
                            attrs: vec![],
                        }));
                    } else {
                        return Err("type must follow a colon".to_owned())
                    }
                } else {
                    return Err("identifier must be followed by a colon".to_owned())
                }
            }
            opt_elem = elem_iter.next();
        }
    } else {
        return Err("structs must contain fields".to_owned())
    }

    buffer.push_str(&format!("}} {};\n\n", ident.name.as_str()));
    // Create the #[repr(C)] attribute.
    let c = context.meta_word(*span, InternedString::new("C"));
    let repr = context.meta_list(*span, InternedString::new("repr"), vec![c]);
    // Create the struct.
    Ok(ptr::P(ast::Item {
        ident: ident,
        attrs: vec![context.attribute(*span, repr)],
        id: ast::DUMMY_NODE_ID,
        node: ast::Item_::ItemStruct(
            ptr::P(ast::StructDef { fields: fields, ctor_id: None, }),
            ast::Generics {
                lifetimes: vec![],
                ty_params: syntax::owned_slice::OwnedSlice::empty(),
                where_clause: ast::WhereClause {
                    id: ast::DUMMY_NODE_ID,
                    predicates: vec![],
                },
            },
        ),
        vis: ast::Visibility::Public,
        span: *span,
    }))
}

fn parse_func<'a>(
    toktree: &mut iter::Peekable<slice::Iter<'a, ast::TokenTree>>,
    buffer: &mut String,
    context: &mut base::ExtCtxt,
    span: codemap::Span,
) -> Result<ptr::P<ast::Item>, String> {
    let func_ident = match toktree.next() {
        // Find the name of the function.
        Some(&ast::TokenTree::TtToken(_, token::Token::Ident(i, _))) => i,
        _ => return Err("no function identifier".to_owned()),
    };

    let mut in_args = vec![];
    // TODO: can we just stringify fields of the Arg struct instead?
    let mut c_args = String::new();
    if let Some(&ast::TokenTree::TtDelimited(_, ref intoks)) = toktree.next() {
        // TODO: I was very tired when I wrote this bit.
        let intoks = (*intoks).clone();
        let intoks = &intoks.tts;
        let mut intoks = intoks.iter();
        let mut opt_tok = intoks.next();
        while let Some(tok) = opt_tok {
            // Find argument identifier.
            if let &ast::TokenTree::TtToken(_, token::Token::Ident(arg_ident, _)) = tok {
                match intoks.next() {
                    // Skip over the colon.
                    Some(&ast::TokenTree::TtToken(_, token::Token::Colon)) => {
                        // Find type identifier and write shit to the buffers.
                        if let Some(&ast::TokenTree::TtToken(_, token::Token::Ident(typ, _))) = intoks.next() {
                            c_args.push_str(&format!("{} {}, ", rust_to_c(&typ.name.as_str()), &arg_ident.name.as_str()));
                            let ty = context.ty_ident(span, typ);
                            in_args.push(context.arg(span, arg_ident, ty));
                        } else {
                            return Err("colon must be followed by a type identifier".to_owned());
                        }
                    },
                    _ => return Err("argument identifier must be followed by a colon".to_owned()),
                };
            };
            opt_tok = intoks.next();
        }
        // Delete the trailing comma and space from the input arguments.
        c_args.pop();
        c_args.pop();
    }

    let c_out: String;
    let out_type = match toktree.peek() {
        Some(&&ast::TokenTree::TtToken(_, token::Token::RArrow)) => {
            // Skip the RArrow so we can nab the type.
            toktree.next();
            if let Some(&ast::TokenTree::TtToken(_, token::Token::Ident(typ, _))) = toktree.next() {
                c_out = typ.name.as_str().to_string();
                context.ty_ident(span, typ)
            } else {
                return Err("right arrow must be followed by a type identifier".to_owned());
            }
        },
        // Assume () if no RArrow.
        _ => {
            c_out = "void".to_owned();
            context.ty(span, ast::Ty_::TyTup(vec![]))
        },
    };

    buffer.push_str(&format!("{} {}({});", c_out, &func_ident.name.as_str(), c_args));

    let block;
    if let Some(d) = toktree.next() {
        // TODO: still fucking shattered.
        let d = d.clone();
        match d {
            delim @ ast::TokenTree::TtDelimited(_, _) => {
                let mut parser = context.new_parser_from_tts(&[delim]);
                block = match parser.parse_block() {
                    Ok(b) => b,
                    _ => return Err("function must have a body".to_owned()),
                };
            },
            _ => return Err("function must have a body".to_owned()),
        }
    } else {
        return Err("function must have a body".to_owned());
    }

    let fn_decl = context.fn_decl(in_args, out_type);
    let no_mangle = context.meta_word(span, InternedString::new("no_mangle"));
    Ok(ptr::P(ast::Item {
        ident: func_ident,
        attrs: vec![context.attribute(span, no_mangle)],
        id: ast::DUMMY_NODE_ID,
        node: ast::Item_::ItemFn(
            fn_decl,
            // TODO: This makes me **very** uneasy!
            ast::Unsafety::Normal,
            // TODO: I don't really know what this means.
            //     - I assume it's for RFC 911.
            ast::Constness::NotConst,
            // TODO: What is cdecl? Should we use system?
            syntax::abi::Abi::C,
            ast::Generics {
                lifetimes: vec![],
                ty_params: syntax::owned_slice::OwnedSlice::empty(),
                where_clause: ast::WhereClause {
                    id: ast::DUMMY_NODE_ID,
                    predicates: vec![],
                },
            },
            block,
        ),
        vis: ast::Visibility::Public,
        span: span,
    }))
}

fn rust_to_c(typ: &str) -> &str {
    match typ {
        "f32" => "float",
        "f64" => "double",
        "i8" => "int8_t",
        "i16" => "int16_t",
        "i32" => "int32_t",
        "i64" => "int64_t",
        // TODO: is this correct?
        "isize" => "intptr_t",
        "u8" => "uint8_t",
        "u16" => "uint16_t",
        "u32" => "uint32_t",
        "u64" => "uint64_t",
        "usize" => "uintptr_t",
        // This is why we write out structs and enums as `typedef ...`.
        // We #include<stdbool.h> so bool is handled.
        t => t,
    }
}


#[doc(hidden)]
#[plugin_registrar]
pub fn plugin_registrar(reg: &mut plugin::Registry) {
    reg.register_macro("cheddar", parse_header);
}
