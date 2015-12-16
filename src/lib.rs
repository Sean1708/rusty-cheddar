//! # Usage
//!
//! Compiler plugins have not yet been stabilised so you must use a nightly compiler to build
//! rusty-cheddar, however there are ways to use rusty-cheddar with a crate designed for stable Rust
//! which are described below. If you wish to build against stable Rust as well then you must use
//! [multirust] or [multirust-rs](https://github.com/Diggsey/multirust-rs).
//!
//! rusty-cheddar targets C99 or later (for sane single line comments and use of `stdint.h` and
//! `stdbool.h`), if you really really really really really have to use an older standard then please
//! open an issue at the [repo] and I will begrudgingly figure out how to implement support for it
//! (after arguing with you lots and lots).
//!
//! ## Invocation From the Command Line
//!
//! You can invoke rusty-cheddar from the command line. First you must grab the [repo] and build it
//! (remember to use nightly Rust to build rusty-cheddar):
//!
//! ```sh
//! $ cargo build --release
//! ```
//! Then compile your file with:
//!
//! ```sh
//! $ rustc -L $CHEDDAR/target/release -Z extra-plugins=cheddar $SOURCE
//! ```
//!
//! where `$CHEDDAR` is the path to rusty-cheddar's `Cargo.toml` (it should be enough for the dylib to
//! be in your `$PATH` but I've not checked this yet) and `$SOURCE` is the source file you wish to
//! compile, you may also need to add the `--crate-type=...` flag.
//!
//! Another common workflow is to use rusty-cheddar to compile the header file without compiling the
//! rest of the crate. For projects using `cargo` you can do:
//!
//! ```sh
//! $ cargo rustc -- -L $CHEDDAR/target/release -Z extra-plugins=cheddar -Z no-trans
//! ```
//!
//! Otherwise:
//!
//! ```sh
//! $ rustc -L $CHEDDAR/target/release -Z extra-plugins=cheddar -Z no-trans $SOURCE
//! ```
//!
//! ### Using rusty-cheddar With Crates Built for Stable Rust
//!
//! Using the above technique and [multirust] you can build your crate on stable while still being able
//! to invoke rusty-cheddar. First you must grab the source from the [repo] and build it with _nightly_
//! Rust:
//!
//! ```sh
//! $ cd $CHEDDAR
//! $ multirust run nightly cargo build --release
//! ```
//!
//! Then build your project on stable Rust and use nightly Rust to invoke rusty-cheddar:
//!
//! ```sh
//! $ cd $YOUR_PROJECT
//! $ multirust override stable  # if you have a different default
//! $ cargo build --release
//! $ multirust run nightly cargo rustc -- -L $CHEDDAR/target/release -Z extra-plugins=cheddar -Z no-trans
//! ```
//!
//! ## Invocation In Source File
//!
//! You can also get rusty-cheddar to run automatically each time you compile, but this means that your
//! crate must be built with nightly Rust. First add the following to your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! rusty-cheddar = "0.1"
//! ```
//!
//! Then at the top of your `lib.rs`:
//!
//! ```no_run
//! #![feature(plugin)]
//! #![plugin(cheddar)]
//! ```
//!
//! rusty-cheddar will then create a `cheddar.h` file in your working directory containing the generated
//! header file. Note that rusty-cheddar emits very few warnings, it is up to the programmer to write a
//! library which can be correctly called from C.
//!
//! You can optionally specify a path for the header file using plugin arguments. Use `dir =
//! "/path/to/out/dir"` to specify an output directory and `file = "name.h"`. So
//!
//! ```no_run
//! #![plugin(dir = "target/include", file = "my_header.h")]
//! ```
//!
//! will first create the directories in `target/include` if they don't exist and will then create
//! `my_header.h` in `target/include`.
//!
//! ## API In a Module
//!
//! You can also place your API in a to help keep your source code neat. **Note that this module
//! must currently be only one level deep, i.e. `api::*` is fine but `api::c_api::*` is not.**
//!
//! To do this you must specify the name of the module in the plugin args, then you must `pub use`
//! the module with a glob to bring all the items into the top level module.
//!
//! ```no_run
//! #![feature(plugin)]
//! #![plugin(cheddar(module = "c_api"))]
//!
//! pub use c_api::*;
//!
//! mod c_api {
//!     // api goes here ...
//! }
//! ```
//!
//! In the examples below, boilerplate has been omitted from the header.
//!
//! ## Typedefs
//!
//! rusty-cheddar converts `pub type A = B` into `typedef B A;`. Types containing generics are ignored.
//!
//! Rust:
//!
//! ```no_run
//! type UInt32 = u32;
//! pub type UInt64 = u64;
//! pub type MyOption<T> = Option<T>
//! ```
//!
//! Header:
//!
//! ```C
//! // Some boilerplate omitted.
//! typedef uint64_t UInt64;
//! // Some more boilerplate omitted.
//! ```
//!
//! ## Enums
//!
//! rusty-cheddar will convert public enums which are marked `#[repr(C)]`. If the enum is generic or
//! contains tuple or struct variants then `cheddar` will fail. rusty-cheddar should correctly handle
//! explicit discriminants.
//!
//! Rust:
//!
//! ```no_run
//! #[repr(C)]
//! pub enum Colours {
//!     Red = -6,
//!     Blue,
//!     Green = 7,
//!     Yellow,
//! }
//!
//! // This would fail is it was #[repr(C)].
//! pub enum Tastes<T> {
//!     Savoury,
//!     Sweet,
//! }
//!
//! // This would fail if it was public.
//! #[repr(C)]
//! enum Units {
//!     Kg(f64),
//!     M(f64),
//!     S(f64),
//!     A(f64),
//!     K(f64),
//!     Mol(f64),
//!     Cd(f64),
//! }
//! ```
//!
//! Header:
//!
//! ```C
//! // Some boilerplate omitted.
//! typedef enum Colours {
//!         Red = -6,
//!         Blue,
//!         Green = 7,
//!         Yellow,
//! } Colours;
//! // Some more boilerplate omitted.
//! ```
//!
//! ## Structs
//!
//! Structs are handled very similarly to enums, they must be public, marked `#[repr(C)]`, and they must not
//! contain generics (this currently only checked at the struct-level, generic fields are not checked).
//!
//! Rust:
//!
//! ```no_run
//! #[repr(C)]
//! pub struct Person {
//!     age: i32,
//!     height: f64,
//!     weight: f64,
//! }
//! ```
//!
//! Header:
//!
//! ```C
//! // Some boilerplate omitted.
//! typedef struct Person {
//!         int32_t age;
//!         double height;
//!         double weight;
//! } Person;
//! // Some more boilerplate omitted.
//! ```
//!
//! ## Functions
//!
//! For rusty-cheddar to pick up on a function declaration it must be public, marked `#[no_mangle]` and
//! have one of the following ABIs:
//!
//! - C
//! - Cdecl
//! - Stdcall
//! - Fastcall
//! - System
//!
//! I'm not totally up to speed on calling conventions so if you believe one of these has been including
//! in error, or if one has been omitted, then please open an issue at the [repo].
//!
//! rusty-cheddar will fail on functions which are marked as diverging (`-> !`).
//!
//! Rust:
//!
//! ```no_run
//! use std::ops::Add;
//!
//! #[no_mangle]
//! pub extern fn hello() {
//!     println!("Hello!");
//! }
//!
//! fn add<O, R, L: Add<R, Output=O>>(l: L, r: R) -> O {
//!     l + r
//! }
//!
//! #[no_mangle]
//! #[allow(non_snake_case)]
//! pub extern fn MyAdd_add_u8(l: u8, r: u8) -> u8 {
//!     add(l, r)
//! }
//!
//! #[no_mangle]
//! #[allow(non_snake_case)]
//! pub extern fn MyAdd_add_u16(l: u16, r: u16) -> u16 {
//!     add(l, r)
//! }
//! ```
//!
//! Header:
//!
//! ```C
//! // Some boilerplate omitted.
//! void hello();
//!
//! uint8_t MyAdd_add_u8(uint8_t l, uint8_t r);
//!
//! uint16_t MyAdd_add_u16(uint16_t l, uint16_t r);
//! // Some more boilerplate omitted.
//! ```
//!
//! ## Type Conversions
//!
//! rusty-cheddar currently does not handle type paths (e.g. `mymod::MyType`), instead they must be `use`ed
//! first:
//!
//! ```no_run
//! // pub type MyCType = mymod::MyType;  // This will put `typedef mymod::MyType MyCType;` into the header.
//! use mymod::MyType;
//! pub type MyCType = MyType;
//! ```
//!
//! The very important exception to this rule is `libc`, types used from `libc` _must_ be qualified
//! (e.g. `libc::c_void`) so that they can be converted properly.
//!
//! # Contributing
//!
//! Contributions to rusty-cheddar are more than welcome.
//!
//! ## Bugs
//!
//! If you find a bug or have a feature request please open an issue. I can't guarantee that I'll fix it
//! but I'll give it a damn good go.
//!
//! If you find the source code unclear in any way then I consider that a bug. I try to make my source
//! code as clear as possible but I'm not very good at it, so any help in that regard is appreciated.
//!
//! ## PRs
//!
//! I love pull requests they tend to make my job much easier, so if you want to fix a bug or implement a
//! feature yourself then that would be great. If you're confused by anything or need some pointers on
//! how to proceed then feel free to open an issue so that I can help, otherwise
//! [these docs](http://manishearth.github.io/rust-internals-docs/syntax/ast/index.html) are a good
//! place to start.
//!
//! ### Tests
//!
//! The tests require you to have a recent version (> `v2.7.2`) of [CppHeaderParser] installed for the
//! version of Python which is installed as `python` (usually Python 2). Furthermore due to the fact
//! that the tests are a massive pile of wanky hacks, you must be in the same directory as
//! rusty-cheddar's `Cargo.toml` to successfully run them.
//!
//!
//! [multirust]: https://github.com/brson/multirust
//! [repo]: https://github.com/Sean1708/rusty-cheddar
//! [CppHeaderParser]: https://bitbucket.org/senex/cppheaderparser
#![feature(rustc_private)]
#![feature(plugin_registrar)]
#![feature(box_syntax)]
#![feature(stmt_expr_attributes)]

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
    module: Option<String>,
}

type Result = std::result::Result<Option<String>, (codemap::Span, String)>;

declare_lint!(CHEDDAR, Allow, "What does this actually do? Do I need it?");

impl lint::LintPass for CheddarPass {
    fn get_lints(&self) -> LintArray {
        lint_array!(CHEDDAR)
    }
}

impl lint::EarlyLintPass for CheddarPass {
    /// The main entry point.
    ///
    /// Determines which module to parse, ensures it is `pub use`ed then hands off to
    /// `CheddarPass::parse_mod`.
    fn check_crate(&mut self, context: &EarlyContext, krate: &ast::Crate) {
        if let Some(ref module) = self.module.clone() {
            let mut mod_item = None;
            let mut pub_used = false;

            // Find the module.
            for item in &krate.module.items {
                match item.node {
                    Item_::ItemMod(ref inner_mod) => {
                        let name: &str = &item.ident.name.as_str();
                        if name == module {
                            mod_item = Some(inner_mod);
                        }
                    },
                    Item_::ItemUse(ref path) => {
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
                    self.parse_mod(context, &mod_item);
                } else {
                    context.sess.err(&format!(
                        "C api must exist in top level module, try `pub use {}::*`",
                        module,
                    ));
                }
            } else {
                context.sess.err(&format!("could not find module '{}'", module));
            }
        } else {
            self.parse_mod(context, &krate.module);
        }
    }
}

// TODO: Maybe it would be wise to use syntax::attr here.
/// Loop through a list of attributes.
///
/// Check that at least one attribute matches some criteria (usually #[repr(C)] or #[no_mangle])
/// and optionally retrieve a String from it (usually a docstring).
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

/// Check the attribute is #[repr(C)].
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

/// Check the attribute is #[no_mangle].
fn check_no_mangle(attr: &Attribute) -> bool {
    match attr.node.value.node {
        ast::MetaItem_::MetaWord(ref name) if *name == "no_mangle" => true,
        _ => false,
    }
}

/// If the attribute is  a docstring, indent it the required amount and return it.
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
//     - probably pull the FnDecl parsing logic out of parse_fn
// TODO: C function pointers _must_ have a name associated with them but this Option business feels
//       like a shit way to handle that
//     - maybe have a named_rust_to_c which allows fn_pointers and rust_to_c doesn't?
/// Return a Rust (type, name) pair into a C (type, name) pair.
///
/// If name is `None` then there is no name associated with that type.
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

/// Takes a Rust pointer (*mut or *const) and converts it into the correct C form.
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

/// Takes a Rust function pointer and makes it C-like.
///
/// Rust function pointers are of the form
///
/// ```no_run
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
fn fn_ptr_to_c(fn_ty: &ast::BareFnTy, fn_span: codemap::Span, name: &str) -> Result {
    match fn_ty.abi {
        // If it doesn't have a C ABI it can't be called from C.
        Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {},
        _ => return Ok(None),
    }

    if !fn_ty.lifetimes.is_empty() {
        return Err((fn_span, "cheddar can not handle lifetimes".to_owned()));
    }

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

/// Attempts to convert a Rust path type (my_mod::MyType) to a C type.
///
/// Types hidden behind modules are almost certainly custom types (which wouldn't work) except
/// types in `libc` which we special case.
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
                "cheddar can not handle types in modules (except `libc`), try `use {}::{}` if you really know what you're doing",
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

impl CheddarPass {
    /// The manager of rusty-cheddar.
    ///
    /// Iterates through all items in the module and dispatches to correct methods, then pulls all
    /// the results together into a header.
    fn parse_mod(&mut self, context: &EarlyContext, module: &ast::Mod) {
        let mut buffer = format!(
            "#ifndef cheddar_gen_{0}_h\n#define cheddar_gen_{0}_h\n\n",
            self.file.file_stem().and_then(|p| p.to_str()).unwrap_or("default"),
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

    /// Convert `pub type A = B;` into `typedef B A;`.
    ///
    /// Aborts if A is generic.
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

    /// Convert a Rust enum into a C enum.
    ///
    /// The Rust enum must be marked with `#[repr(C)]` and must be public otherwise the function
    /// will abort.
    ///
    /// Cheddar will error if the enum if generic or if it contains non-unit variants.
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

    /// Convert a Rust struct into a C struct.
    ///
    /// The rust struct must be marked `#[repr(C)]` and must be public otherwise the function will
    /// abort.
    ///
    /// Cheddar will error if the struct is generic or if the struct is a unit or tuple struct.
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

    /// Convert a Rust function declaration into a C function declaration.
    ///
    /// The function declaration must be marked `#[no_mangle]` and have a C ABI otherwise the
    /// function will abort.
    ///
    /// If the declaration is generic or diverges then cheddar will error.
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


/// Parse plugin arguments into a file path and create any directories required.
fn parse_plugin_args(reg: &mut rustc_plugin::Registry) -> std::result::Result<(PathBuf, Option<String>), ()> {
    let mut dir = PathBuf::new();
    let mut file: Option<&str> = None;
    let mut module: Option<String> = None;

    for arg in reg.args() {
        if let ast::MetaItem_::MetaNameValue(ref name, ref value) = arg.node {
            let name: &str = name;

            if name == "dir" {
                if let ast::Lit_::LitStr(ref dir_str, _) = value.node {
                    let dir_str: &str = dir_str;
                    dir.push(&dir_str);
                } else {
                    reg.sess.span_err(value.span, "`dir` argument value must be a string literal");
                    return Err(());
                }
            } else if name == "file" {
                if let ast::Lit_::LitStr(ref file_str, _) = value.node {
                    let file_str: &str = file_str;
                    file = Some(file_str);
                } else {
                    reg.sess.span_err(value.span, "`file` argument value must be a string literal");
                    return Err(());
                }
            } else if name == "module" {
                if let ast::Lit_::LitStr(ref file_str, _) = value.node {
                    let file_str: &str = file_str;
                    module = Some(file_str.to_owned());
                } else {
                    reg.sess.span_err(value.span, "`module` argument value must be a string literal");
                    return Err(());
                }
            } else {
                reg.sess.span_err(arg.span, &format!("unrecognised cheddar argument `{}`", &name));
                return Err(());
            }
        } else {
            reg.sess.span_err(arg.span, "cheddar plugin arguments must be of the form `name = value`");
            return Err(());
        }
    }

    // Create all the directories before we push the file name.
    if let Err(error) = fs::create_dir_all(&dir) {
        reg.sess.err(&format!("could not create directories in '{}': {}", dir.display(), error));
        return Err(());
    }

    let file = file
        .map(PathBuf::from)
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

    dir.push(&file);

    Ok((dir, module))
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut rustc_plugin::registry::Registry) {
    let (file, module) = if let Ok(val) = parse_plugin_args(reg) {
        val
    } else {
        return;
    };

    reg.register_early_lint_pass(box CheddarPass { file: file, module: module });
}
