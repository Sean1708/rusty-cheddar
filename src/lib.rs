#![feature(rustc_private)]
#![feature(plugin_registrar)]
#![feature(box_syntax)]

extern crate rustc;
extern crate syntax;

use rustc::plugin;
use syntax::ast;
use syntax::ast::TokenTree;
use syntax::codemap;
use syntax::diagnostic::FatalError;
use syntax::ext::base;
use syntax::ext::base::DummyResult;
use syntax::parse;
use syntax::parse::parser;
use syntax::parse::token;
use syntax::parse::token::Token;
use syntax::parse::token::InternedString;
use syntax::ptr;

use std::fs;
use std::path;

use std::io::Write;
use syntax::ext::build::AstBuilder;


fn parse_header(
    context: &mut base::ExtCtxt, span: codemap::Span, toktree: &[ast::TokenTree]
) -> Box<base::MacResult + 'static> {
    macro_rules! cheddar_try {
        ($action:expr, $span:expr, $msg:expr,) => {cheddar_try!($action, $span, $msg)};
        ($action:expr, $span:expr, $msg:expr) => {{
            match $action {
                Ok(v) => v,
                Err(_) => {
                    context.span_err($span, $msg);
                    return DummyResult::any(span);
                },
            }
        }};
    }

    let filename_span;
    let mut parser = context.new_parser_from_tts(toktree);
    let header_path = match parser.token {
        // If the first token is a string, use it for the header path.
        Token::Literal(token::Lit::Str_(s), _) => {
            filename_span = parser.span;
            cheddar_try!(parser.bump(), parser.span, "could not read token");
            path::PathBuf::from(s.as_str().to_string())
        },
        // If the first token is Eof then we can't do anything.
        Token::Eof => {
            context.span_err(span, "the block of cheddar contains no tokens");
            return DummyResult::any(span);
        },
        // Otherwise build a default path
        _ => {
            filename_span = parser.last_span;
            let mut temp = path::PathBuf::new();
            // TODO: what if they're not using cargo?
            temp.push("target");
            // TODO: temp.push("debug") for Debug builds, "release" for release builds
            temp.push("include");
            temp.push(&format!("{}.h", context.ecfg.crate_name));
            temp
        }
    };

    // TODO: add docstrings to the header file somehow.
    // TODO: have a way to specify tabs vs x number of spaces
    //     - #[plugin(cheddar(spaces=4))] or #[plugin(cheddar(tabs))]
    //     - compute the string in plugin_registrar and save it as an environment variable
    //     - pull that environment variable in parse_header and pass it to the functions
    //     - is it really worth it?
    // TODO: can we automatically set the crate-type dylib attribute?
    // TODO: layout the header file as like for like with the cheddar! block (a *block* of cheddar. lol)
    //     - leave all comments, docstrings and items (enums, structs and fns) in same place
    // TODO: retain arbitrary attributes
    // TODO: check all spans are what we think they are!
    //     - do this by iserting a context.span_note everywhere there is a span
    // TODO: test failures, especially file opening ones!

    // Create the parent directories and header file.
    cheddar_try!(
        // Without .with_file_name("") the header file is created as a directory.
        fs::create_dir_all(header_path.with_file_name("")),
        filename_span,
        "can not create directories for header path",
    );
    let mut header_file = cheddar_try!(
        fs::File::create(&header_path),
        filename_span,
        "can not create header file",
    );
    cheddar_try!(
        header_file.write_all(format!(
            "#ifndef cheddar_gen_{0}_h\n#define cheddar_gen_{0}_h\n\n",
            // TODO: there must be a better way to do this.
            header_path.file_stem().map(|p| p.to_str().unwrap_or("default")).unwrap_or("default"),
        ).as_bytes()),
        filename_span,
        "can not write guard to header file",
    );
    cheddar_try!(
        header_file.write_all(b"#include <stdint.h>\n#include <stdbool.h>\n\n"),
        filename_span,
        "can not write includes to header file",
    );

    let mut item_buf = String::new();
    let mut items = vec![];
    loop {
        // TODO: I feel like I shouldn't need a clone() here.
        //     - what about .bump_and_get()?
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
                    _ => cheddar_try!(parser.bump(), parser.span, "could not read token"),
                },
                // Ignore non-identifier tokens.
                _ => cheddar_try!(parser.bump(), parser.span, "could not read token"),
            },
        };
    };

    cheddar_try!(
        header_file.write_all(item_buf.as_bytes()),
        filename_span,
        "can not write items to header file",
    );
    cheddar_try!(
        header_file.write_all(b"#endif\n"),
        filename_span,
        "can not write endif to header file",
    );

    base::MacEager::items(syntax::util::small_vector::SmallVector::many(items))
}


fn parse_enum<'a>(
    parser: &mut parser::Parser<'a>,
    buffer: &mut String,
    context: &mut base::ExtCtxt,
) -> parse::PResult<ptr::P<ast::Item>> {
    let kwd_span = parser.span;
    // Current token is still Ident("enum").
    try!(parser.eat_keyword(token::keywords::Keyword::Enum));

    let ident = try!(parser.parse_ident());
    buffer.push_str(&format!("typedef enum {} {{\n", ident.name.as_str()));

    // A Brace is a curly bracket.
    try!(parser.expect(&Token::OpenDelim(token::DelimToken::Brace)));

    let close_span;
    let mut variants = vec![];
    // Find the vector of enum variants.
    loop {
        let tok =  try!(parser.bump_and_get());
        match tok {
            Token::CloseDelim(token::DelimToken::Brace) => {
                close_span = parser.last_span;
                break
            },
            Token::Ident(id, _) => {
                variants.push(ptr::P(context.variant(parser.span, id, vec![])));
                buffer.push_str(&format!("\t{},\n", id.name.as_str()));
                try!(parser.expect_one_of(
                    &[Token::Comma],
                    &[Token::CloseDelim(token::DelimToken::Brace)],
                ));
            },
            _ => {
                // TODO: I want to avoid calling span_err at all really.
                context.span_err(parser.span, "expected enum variants");
                return Err(FatalError);
            },
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
) -> parse::PResult<ptr::P<ast::Item>> {
    let kwd_span = parser.span;
    // Current token is still Ident("struct").
    try!(parser.eat_keyword(token::keywords::Keyword::Struct));

    let ident = try!(parser.parse_ident());
    buffer.push_str(&format!("typedef struct {} {{\n", ident.name.as_str()));

    // A Brace is a curly bracket.
    try!(parser.expect(&Token::OpenDelim(token::DelimToken::Brace)));

    // Find the vector of struct fields.
    let close_span;
    let mut fields = vec![];
    loop {
        let tok =  try!(parser.bump_and_get());
        match tok {
            Token::CloseDelim(token::DelimToken::Brace) => {
                close_span = parser.span;
                break
            },
            Token::Ident(id, _) => {
                // TODO: should this be last_span?
                let id_span = parser.span;
                try!(parser.expect(&Token::Colon));

                // TODO: Should this be after .parse_ident()?
                let typ_span = parser.span;
                let typ = try!(parser.parse_ident());

                buffer.push_str(&format!("\t{} {};\n", rust_to_c(&typ.name.as_str()), id.name.as_str()));
                fields.push(codemap::spanned(id_span.lo, typ_span.hi, ast::StructField_ {
                    kind: ast::StructFieldKind::NamedField(id, ast::Visibility::Public),
                    id: ast::DUMMY_NODE_ID,
                    ty: context.ty_ident(typ_span, typ),
                    attrs: vec![],
                }));

                try!(parser.expect_one_of(
                    &[Token::Comma],
                    &[Token::CloseDelim(token::DelimToken::Brace)],
                ));
            },
            _ => {
                context.span_err(parser.span, "expected struct fields");
                return Err(FatalError);
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
) -> parse::PResult<ptr::P<ast::Item>> {
    let kwd_span = parser.span;
    // Current token is still Ident("struct").
    try!(parser.eat_keyword(token::keywords::Keyword::Fn));
    let ident = try!(parser.parse_ident());
    try!(parser.expect(&Token::OpenDelim(token::DelimToken::Paren)));

    let close_span;
    let mut in_args = vec![];
    let mut c_args = String::new();
    loop {
        let tok = try!(parser.bump_and_get());
        match tok {
            Token::CloseDelim(token::DelimToken::Paren) => {
                close_span = parser.last_span;
                break
            },
            Token::Ident(id, _) => {
                // TODO: should this just be span?
                let id_span = parser.last_span;
                try!(parser.expect(&Token::Colon));

                // TODO: Should this be after .parse_ident()?
                let typ_span = parser.span;
                let typ = try!(parser.parse_ident());

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

                try!(parser.expect_one_of(
                    &[Token::Comma],
                    &[Token::CloseDelim(token::DelimToken::Paren)],
                ));
            },
            _ => {
                context.span_err(parser.span, "expected function arguments");
                return Err(FatalError);
            },
        };
    }
    // Delete the trailing comma and space from the input arguments.
    c_args.pop();
    c_args.pop();

    // Must be mutable for closure.
    let mut c_out = String::new();
    let out_type = match parser.token {
        Token::RArrow => {
            try!(parser.expect(&Token::RArrow));
            // A type should always follow a ->.
            try!(parser.parse_ident().map(|typ| {
                c_out.push_str(rust_to_c(&typ.name.as_str()));
                context.ty_ident(parser.last_span, typ)
            }))
        },
        // Assume () if no RArrow.
        _ => {
            c_out = "void".to_owned();
            context.ty(parser.span, ast::Ty_::TyTup(vec![]))
        },
    };
    // Make it immutable again.
    let c_out = c_out;

    buffer.push_str(&format!("{} {}({});\n\n", c_out, &ident.name.as_str(), c_args));

    // TODO: another perfect use of try!
    let block = try!(parser.parse_block());
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
    // TODO: are there any types that we should just die on?
    //     - or should we assume that the programmer has handled it?
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
