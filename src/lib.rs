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
use syntax::ext::base::DummyResult;
use syntax::parse::parser;
use syntax::parse::token;
use syntax::parse::token::Token;
use syntax::parse::token::InternedString;
use syntax::ptr;

use std::fs;
use std::path;

use std::io::Write;
use syntax::ext::build::AstBuilder;


// macro_rules! cheddar_try {
// }

// TODO: some sort of bump! macro.
//     - will prob need one for parse_header, the other for parse_*
//     - or possibly use context.span_fatal


// TODO: add docstrings to the header file somehow.
fn parse_header(
    context: &mut base::ExtCtxt, span: codemap::Span, toktree: &[ast::TokenTree]
) -> Box<base::MacResult + 'static> {
    let mut parser = context.new_parser_from_tts(toktree);
    let header_path = match parser.token {
        // If the first token is a string, use it for the header path.
        Token::Literal(token::Lit::Str_(s), _) => {
            if let Err(_) = parser.bump() {
                context.span_err(parser.span, "could not read token");
                return DummyResult::any(span);
            }
            path::PathBuf::from(s.as_str().to_string())
        },
        // If the first token is Eof then we can't do anything.
        Token::Eof => {
            context.span_err(span, "the block of cheddar contains no tokens");
            return DummyResult::any(span);
        },
        // Otherwise build a default path
        _ => {
            // TODO: There must be a better way to do this!
            let mut temp = path::PathBuf::new();
            // TODO: what if they're not using cargo?
            temp.push("target");
            // TODO: temp.push("debug") for Debug builds, "release" for release builds
            temp.push("include");
            // TODO: a better default name
            temp.push("header.h");
            temp
        }
    };

    // TODO: have a way to specify tabs vs x number of spaces
    //     - #[plugin(cheddar(spaces=4))] or #[plugin(cheddar(tabs))]
    //     - compute the string in plugin_registrar and save it as an environment variable
    //     - pull that environment variable in parse_header and pass it to the functions
    //     - is it really worth it?
    // TODO: can we automatically set the crate-type dylib attribute?
    // TODO: layout the header file as like for like with the cheddar! block (a *block* of cheddar. lol)
    //     - leave all comments, docstrings and items (enums, structs and fns) in same place
    // TODO: retain arbitrary attributes

    // Create the parent directories and header file.
    fs::create_dir_all(header_path.with_file_name(""))
        .ok().expect("Can not create directories for output file.");
    let mut header_file = fs::File::create(&header_path)
        .ok().expect("Can not open the header file.");
    header_file.write_all(format!(
        "#ifndef cheddar_gen_{0}_h\n#define cheddar_gen_{0}_h\n\n#include <stdint.h>\n#include <stdbool.h>\n\n\n",
        header_path.file_stem().expect("Why no file stem?").to_str().expect("File stem not stringable."),
    ).as_bytes()).ok().expect("Can not write guard to header file.");

    let mut item_buf = String::new();
    let mut items = vec![];

    loop {
        // TODO: I feel like I shouldn't need a clone() here.
        let tok = parser.token.clone();
        match tok {
            Token::Eof => break,
            // If the token is not Eof then see if it's a cheddar-able item.
            tok => match tok {
                Token::Ident(n, _) => match &*n.name.as_str() {
                    "enum" => match parse_enum(&mut parser, &mut item_buf, context) {
                        Ok(i) => items.push(i),
                        Err(_) => return DummyResult::any(span),
                    },
                    "struct" => match parse_struct(&mut parser, &mut item_buf, context) {
                        Ok(i) => items.push(i),
                        Err(_) => return DummyResult::any(span),
                    },
                    "fn" => match parse_func(&mut parser, &mut item_buf, context) {
                        Ok(i) => items.push(i),
                        Err(_) => return DummyResult::any(span),
                    },
                    // Ignore any other identifiers.
                    _ => if let Err(_) = parser.bump() {
                        context.span_err(parser.span, "could not read token");
                        return DummyResult::any(span);
                    },
                },
                // Ignore non-identifier tokens.
                _ => if let Err(_) = parser.bump() {
                    context.span_err(parser.span, "could not read token");
                    return DummyResult::any(span);
                },
            },
        };
    };

    header_file.write_all(item_buf.as_bytes()).ok().expect("Can not write to header file.");
    header_file.write_all(b"#endif\n").ok().expect("Can not write endif to header file.");

    base::MacEager::items(syntax::util::small_vector::SmallVector::many(items))
}


fn parse_enum<'a>(
    parser: &mut parser::Parser<'a>,
    buffer: &mut String,
    context: &mut base::ExtCtxt,
// TODO: think more about how to do errors.
//     - See if we can get a pattern going so we can write a macro.
//     - Alternatively I think most of these things use PResult so we could use try!();
) -> Result<ptr::P<ast::Item>, ()> {
    let kwd_span = parser.span;
    // Current token is still Ident("enum").
    if let Err(_) = parser.eat_keyword(token::keywords::Keyword::Enum) {
        return Err(());
    }

    let ident = match parser.parse_ident() {
        Ok(tok) => tok,
        Err(_) => return Err(()),
    };

    buffer.push_str(&format!("typedef enum {} {{\n", ident.name.as_str()));

    // A Brace is a curly bracket.
    if let Err(_) = parser.expect(&Token::OpenDelim(token::DelimToken::Brace)) {
        return Err(());
    }

    let close_span;
    let mut variants = vec![];
    // Find the vector of enum variants.
    // TODO: there must be an better way to do this!
    loop {
        if let Ok(tok) =  parser.bump_and_get() {
            match tok {
                Token::CloseDelim(token::DelimToken::Brace) => {
                    close_span = parser.last_span;
                    break
                },
                Token::Ident(id, _) => {
                    variants.push(ptr::P(context.variant(parser.span, id, vec![])));
                    buffer.push_str(&format!("\t{},\n", id.name.as_str()));
                    // TODO: This forces a trailing comma.
                    if let Err(_) = parser.expect(&Token::Comma) {
                        return Err(());
                    };
                },
                _ => {
                    context.span_err(parser.span, "expected enum variants");
                    return Err(());
                },
            };
        } else {
            context.span_err(parser.span, "could not read token");
            return Err(());
        };
    };

    buffer.push_str(&format!("}} {};\n\n", ident.name.as_str()));

    // Create the #[repr(C)] attribute, the enum keyword span simplicity.
    let c = context.meta_word(kwd_span, InternedString::new("C"));
    let repr = context.meta_list(kwd_span, InternedString::new("repr"), vec![c]);
    // Create the enum, can't use context.item_enum because we need Public visibility.
    Ok(ptr::P(ast::Item {
        ident: ident,
        attrs: vec![context.attribute(kwd_span, repr)],
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
        // Use kwd_span.expn_id since all IDs are DUMMY_NODE_ID at this stage.
        // TODO: Is this how Span works in an item? Or does it go from open curly bracket?
        span: codemap::Span { lo: kwd_span.lo, hi: close_span.hi, expn_id: kwd_span.expn_id, },
    }))
}

fn parse_struct<'a>(
    parser: &mut parser::Parser<'a>,
    buffer: &mut String,
    context: &mut base::ExtCtxt,
) -> Result<ptr::P<ast::Item>, ()> {
    let kwd_span = parser.span;
    // Current token is still Ident("struct").
    if let Err(_) = parser.eat_keyword(token::keywords::Keyword::Struct) {
        return Err(());
    }

    let ident = match parser.parse_ident() {
        Ok(tok) => tok,
        Err(_) => return Err(()),
    };

    buffer.push_str(&format!("typedef struct {} {{\n", ident.name.as_str()));

    // A Brace is a curly bracket.
    if let Err(_) = parser.expect(&Token::OpenDelim(token::DelimToken::Brace)) {
        return Err(());
    }

    // Find the vector of struct fields.
    let close_span;
    let mut fields = vec![];
    loop {
        let tok =  parser.token.clone();
        match tok {
            Token::CloseDelim(token::DelimToken::Brace) => {
                close_span = parser.span;
                if let Err(_) = parser.expect(&Token::CloseDelim(token::DelimToken::Brace)) {
                    return Err(());
                };
                break
            },
            Token::Ident(id, _) => {
                // TODO: should this be after .bump()?
                let id_span = parser.span;
                // TODO: See how the rust parser handles .bump().
                if let Err(_) = parser.bump() {
                    context.span_err(parser.span, "something horrible has happened");
                    return Err(());
                };

                if let Err(_) = parser.expect(&Token::Colon) {
                    return Err(());
                };

                // TODO: Should this be after .parse_ident()?
                let typ_span = parser.span;
                let typ = match parser.parse_ident() {
                    Ok(t) => t,
                    Err(_) => return Err(()),
                };

                buffer.push_str(&format!("\t{} {};\n", rust_to_c(&typ.name.as_str()), id.name.as_str()));
                fields.push(codemap::spanned(id_span.lo, typ_span.hi, ast::StructField_ {
                    kind: ast::StructFieldKind::NamedField(id, ast::Visibility::Public),
                    id: ast::DUMMY_NODE_ID,
                    ty: context.ty_ident(typ_span, typ),
                    attrs: vec![],
                }));

                // TODO: This forces a trailing comma.
                if let Err(_) = parser.expect(&Token::Comma) {
                    return Err(());
                };
            },
            _ => {
                context.span_err(parser.span, "expected struct fields");
                return Err(());
            },
        };
    };

    buffer.push_str(&format!("}} {};\n\n", ident.name.as_str()));

    // Create the #[repr(C)] attribute.
    let c = context.meta_word(kwd_span, InternedString::new("C"));
    let repr = context.meta_list(kwd_span, InternedString::new("repr"), vec![c]);
    // Create the struct.
    Ok(ptr::P(ast::Item {
        ident: ident,
        attrs: vec![context.attribute(kwd_span, repr)],
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
        span: codemap::Span { lo: kwd_span.lo, hi: close_span.hi, expn_id: kwd_span.expn_id, },
    }))
}

fn parse_func<'a>(
    parser: &mut parser::Parser<'a>,
    buffer: &mut String,
    context: &mut base::ExtCtxt,
) -> Result<ptr::P<ast::Item>, ()> {
    let kwd_span = parser.span;
    // Current token is still Ident("struct").
    if let Err(_) = parser.eat_keyword(token::keywords::Keyword::Fn) {
        return Err(());
    }

    let ident = match parser.parse_ident() {
        Ok(tok) => tok,
        Err(_) => return Err(()),
    };

    if let Err(_) = parser.expect(&Token::OpenDelim(token::DelimToken::Paren)) {
        return Err(());
    }

    let close_span;
    let mut in_args = vec![];
    let mut c_args = String::new();
    loop {
        if let Ok(tok) = parser.bump_and_get() {
            match tok {
                Token::CloseDelim(token::DelimToken::Paren) => {
                    close_span = parser.last_span;
                    break
                },
                Token::Ident(id, _) => {
                    // TODO: should this just be span?
                    let id_span = parser.last_span;

                    if let Err(_) = parser.expect(&Token::Colon) {
                        return Err(());
                    };

                    // TODO: Should this be after .parse_ident()?
                    let typ_span = parser.span;
                    let typ = match parser.parse_ident() {
                        Ok(t) => t,
                        Err(_) => return Err(()),
                    };

                    c_args.push_str(&format!(
                        "{} {}, ", rust_to_c(&typ.name.as_str()), id.name.as_str()
                    ));
                    in_args.push(context.arg(
                        codemap::Span { lo: id_span.lo,
                            hi: typ_span.hi,
                            expn_id: id_span.expn_id,
                        },
                        id,
                        context.ty_ident(typ_span, typ),
                    ));

                    // TODO: use this above!
                    if let Err(_) = parser.expect_one_of(
                        &[Token::Comma], &[Token::CloseDelim(token::DelimToken::Paren)]
                    ) {
                        return Err(());
                    };
                },
                _ => {
                    context.span_err(parser.span, "expected function arguments");
                    return Err(());
                },
            };
        } else {
            context.span_err(parser.span, "could not read token");
            return Err(());
        }
    }
    // Delete the trailing comma and space from the input arguments.
    c_args.pop();
    c_args.pop();

    let c_out: String;
    let out_type = match parser.token {
        Token::RArrow => {
            if let Err(_) = parser.expect(&Token::RArrow) {
                return Err(());
            }

            // TODO: This would be a perfect use of try!
            match parser.parse_ident() {
                Ok(t) => {
                    c_out = rust_to_c(&t.name.as_str()).to_owned();
                    context.ty_ident(parser.last_span, t)
                },
                Err(_) => return Err(()),
            }
        },
        // Assume () if no RArrow.
        _ => {
            c_out = "void".to_owned();
            context.ty(parser.span, ast::Ty_::TyTup(vec![]))
        },
    };

    buffer.push_str(&format!("{} {}({});\n\n", c_out, &ident.name.as_str(), c_args));

    // TODO: another perfect use of try!
    let block = match parser.parse_block() {
        Ok(b) => b,
        Err(_) => return Err(()),
    };

    let fn_decl = context.fn_decl(in_args, out_type);
    let no_mangle = context.meta_word(kwd_span, InternedString::new("no_mangle"));
    Ok(ptr::P(ast::Item {
        ident: ident,
        attrs: vec![context.attribute(kwd_span, no_mangle)],
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
        span: codemap::Span { lo: kwd_span.lo, hi: close_span.hi, expn_id: kwd_span.expn_id, },
    }))
}

// TODO: Can we use AsRef or Deref here?
fn rust_to_c(typ: &str) -> &str {
    // TODO: warn on String or &str.
    // TODO: How do we handle pointers?
    match typ {
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
        // We #include<stdbool.h> so bool is handled.
        t => t,
    }
}


#[doc(hidden)]
#[plugin_registrar]
pub fn plugin_registrar(reg: &mut plugin::Registry) {
    reg.register_macro("cheddar", parse_header);
}
