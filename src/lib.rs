//! rusty-cheddar is a library for converting Rust source files into C header files.
//!
//! **A note on versioning:** While rusty-cheddar is still in a significant flux (i.e.
//! pre-`v1.0.0`) it will likely go through numerous breaking changes. However, until `v1.0.0`, any
//! time a breaking change is made the minor version will be bumped and any time a new feature is
//! added the path version will be bumped.
//!
//! rusty-cheddar targets C99 or later (for sane single line comments and use of `stdint.h` and
//! `stdbool.h`), if you really really really really really have to use an older standard then please
//! open an issue at the [repo] and I will begrudgingly figure out how to implement support for it
//! (after arguing with you lots and lots).
//!
//! The most useful way to use rusty-cheddar is in a build script. To do this add the following
//! `build-dependencies` section to your `Cargo.toml` (to use it as a normal library simply replace
//! `build-dependencies` with `dependencies`):
//!
//! ```toml
//! # Cargo.toml
//!
//! [build-dependencies]
//! rusty-binder = { git = "https://gitlab.com/rusty-binder/rusty-binder" }
//! rusty-cheddar = "0.3.0"
//! ```
//!
//! You can also place your API in a module to help keep your source code neat. To do this you must
//! supply the name of the module to Cheddar, then ensure that the items are available in the
//! top-level scope:
//!
//! ```no_run
//! // build.rs
//! extern crate binder;
//! extern crate cheddar;
//!
//! fn main() {
//!     binder::Binder::new().expect("could not read manifest")
//!         .register(cheddar::Cheddar::default())
//!         .module("c_api").expect("malformed module path")
//!         .run_build();
//! }
//! ```
//!
//! ```ignore
//! // src/lib.rs
//!
//! pub use c_api::*;
//!
//! mod c_api {
//!     // api goes here ...
//! }
//! ```
//! The generated header could be found in targed/binder/C/c_api.h
//!
//! There is also the `.compile()` method for finer control.
//! See the documentation of [rusty-binder](https://gitlab.com/rusty-binder/rusty-binder) for details
//!
//! # Conversions
//!
//! In the examples below, boilerplate has been omitted from the header.
//!
//! ## Typedefs
//!
//! rusty-cheddar converts `pub type A = B` into `typedef B A;`. Types containing generics are ignored.
//!
//! Rust:
//!
//! ```ignore
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
//! ```ignore
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
//!     Savoury(T),
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
//! ```ignore
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
//! ### Opaque Structs
//!
//! One common C idiom is to hide the implementation of a struct using an opaque struct, which can
//! only be used behind a pointer. This is especially useful in Rust-C interfaces as it allows you
//! to use _any arbitrary Rust struct_ in C.
//!
//! To define an opaque struct you must define a public newtype which is marked as `#[repr(C)]`.
//!
//! Rust:
//!
//! ```ignore
//! struct Foo<T> {
//!     bar: i32,
//!     baz: Option<T>,
//! }
//!
//! #[repr(C)]
//! pub struct MyCrate_Foo(Foo<PathBuf>);
//! ```
//!
//! Header:
//!
//! ```C
//! // Some boilerplate omitted.
//! typedef struct MyCrate_Foo MyCrate_Foo;
//! // Some boilerplate omitted.
//! ```
//!
//! Note that the newtype _must not_ be generic but the type that it wraps can be arbitrary.
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
//! ```ignore
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
//! ## Paths
//!
//! You must not put types defined in other modules in an exported type signature without hiding it
//! behind an opaque struct. This is because the C compiler must know the layout of the type and
//! rusty-cheddar can not yet search other modules.
//!
//! The very important exception to this rule are the C ABI types defined in
//! the `libc` crate and `std::os::raw`. 
//!
//! If absolute needed it is also possible to add more custom modules.
//!
//! ```no_run
//! // build.rs
//! extern crate binder;
//! extern crate cheddar;
//!
//!
//! fn custom_convert(t: &str) -> &str {
//!     // your convert code
//!     t
//! }
//!
//! fn main() {
//!     let custom = cheddar::CustomTypeModule::new("my::module", custom_convert);
//!
//!     binder::Binder::new().expect("could not read manifest")
//!         .register(cheddar::Cheddar::with_custom_type_module(vec![custom]))
//!         .module("c_api").expect("malformed module path")
//!         .run_build();
//! }
//! ```
//!
//!
//! [the cargo docs]: http://doc.crates.io/build-script.html
//! [repo]: https://github.com/Sean1708/rusty-cheddar
extern crate binder;
extern crate itertools;

use std::collections::HashMap;

use binder::compiler::*;
use binder::compiler::syntax::*;
use itertools::Itertools;

mod types;
pub use self::types::CustomTypeModule;

pub struct Cheddar {
    buf: HashMap<String, String>,
    type_factory: HashMap<String, self::types::TypeFactory>,
    default_factory: self::types::TypeFactory,
    custom: Vec<self::types::CustomTypeModule>,
}

impl Default for Cheddar {
    fn default() -> Self {
        Cheddar {
            buf: HashMap::default(),
            type_factory: HashMap::default(),
            default_factory: self::types::TypeFactory::new(Vec::new(), Vec::new()),
            custom: Vec::new(),
        }
    }
}

impl Cheddar {
    pub fn with_custom_type_module(custom: Vec<self::types::CustomTypeModule>) -> Self {
        Cheddar {
            buf: HashMap::default(),
            type_factory: HashMap::default(),
            default_factory: self::types::TypeFactory::new(Vec::new(), custom.clone()),
            custom: custom,
        }
    }

    fn ident_to_str(i: ast::Ident) -> parse::token::InternedString {
        i.name.as_str()
    }

    fn retrieve_docstring(attrs: &[ast::Attribute], indent: usize) -> String {
        let doc = utils::docs_from_attrs(attrs, "// ", "");
        if doc.is_empty() {
            String::new()
        } else {
            let i = ::std::iter::repeat(" ").take(indent).collect::<String>();
            format!("{}{}", i, doc)
        }
    }

    fn get_mod_name(mod_path: &Option<ast::Path>) -> String {
        mod_path.clone().map_or(String::from("capi"), {
            |p| {
                p.segments
                    .iter()
                    .map(|s| (&*Self::ident_to_str(s.identifier)).to_owned())
                    .join("_")
            }
        })
    }

    fn add_to_buf(&mut self, mod_path: &Option<ast::Path>, content: &str) {
        let mod_name = Self::get_mod_name(mod_path);
        let mut mod_content = self.buf.entry(mod_name.to_owned()).or_insert(String::new());
        println!("{}\n", content);
        *mod_content = format!("{}\n\n{}", mod_content, content);
    }

    fn type_factory(&self, mod_name: &String) -> &self::types::TypeFactory {
        self.type_factory
            .get(mod_name)
            .unwrap_or(&self.default_factory)
    }

    fn struct_is_opaque(fields: &[ast::StructField]) -> bool {
        fields.iter().any(|field| field.ident.is_none())
    }
}


impl Compiler for Cheddar {
    fn compile_mod(&mut self, session: &mut session::Session, module: &ast::Mod) -> Result {
        let uses = module.items
            .iter()
            .filter_map(|i| {
                match i.node {
                    ast::ItemKind::Use(ref p) => Some(p.node.clone()),
                    _ => None,
                }
            });
        let mod_name = Self::get_mod_name(&session.module);
        self.type_factory
            .entry(mod_name)
            .or_insert(self::types::TypeFactory::new(uses.collect(), self.custom.clone()));
        Ok(())
    }

    fn compile_ty_item(&mut self, session: &mut session::Session, type_item: &ast::Item) -> Result {
        let doc = Self::retrieve_docstring(&type_item.attrs, 0);
        let mod_name = Self::get_mod_name(&session.module);
        let new_type = Self::ident_to_str(type_item.ident);
        let tpe = try!(utils::ty_from_item(session, type_item));
        let to_type = try!(self.type_factory(&mod_name).rust_to_c(&*tpe, &new_type, Some(session)));
        let t = format!("{doc}typedef {tpe};", doc = doc, tpe = to_type.unwrap());
        self.add_to_buf(&session.module, &t);
        Ok(())
    }

    fn compile_enum_item(&mut self,
                         session: &mut session::Session,
                         enum_item: &ast::Item)
                         -> Result {
        let doc = Self::retrieve_docstring(&enum_item.attrs, 0);
        let enum_def = try!(utils::enum_from_item(session, enum_item));
        let variants = enum_def.variants
            .iter()
            .map(|i| {
                let name = Self::ident_to_str(i.node.name);
                let doc = Self::retrieve_docstring(&i.node.attrs, 4);
                if let Some(ref e) = i.node.disr_expr {
                    format!("{doc}    {item} = {expr},",
                            doc = doc,
                            item = name,
                            expr = print::pprust::expr_to_string(&*e))
                } else {
                    format!("{}    {},", doc, name)
                }
            })
            .fold(String::new(), |acc, r| format!("{}\n{}", acc, r));
        let name = Self::ident_to_str(enum_item.ident);
        let e = format!("{doc}typedef enum {enum_name} {{{variants}\n}} {enum_name};",
                        doc = doc,
                        enum_name = name,
                        variants = variants);
        self.add_to_buf(&session.module, &e);
        Ok(())
    }

    fn compile_struct_item(&mut self,
                           session: &mut session::Session,
                           struct_item: &ast::Item)
                           -> Result {
        let mod_name = Self::get_mod_name(&session.module);
        let doc = Self::retrieve_docstring(&struct_item.attrs, 0);
        let fields = try!(utils::struct_from_item(session, struct_item));
        let name = Self::ident_to_str(struct_item.ident);
        let s = if Self::struct_is_opaque(fields) {
            format!("{doc}typedef struct {struct_name} {struct_name};",
                    doc = doc,
                    struct_name = name)
        } else {
            let fields = try!(fields.iter()
                .map(|f| {
                    let name = Self::ident_to_str(f.ident.unwrap());
                    let tpe = try!(self.type_factory(&mod_name)
                        .rust_to_c(&*f.ty, &*name, Some(session)));
                    if let Some(t) = tpe {
                        let doc = Self::retrieve_docstring(&f.attrs, 4);
                        Ok(Some(format!("{}    {};", doc, t)))
                    } else {
                        Ok(None)
                    }
                })
                .fold_results(String::new(), |acc, r| {
                    if let Some(r) = r {
                        format!("{}\n{}", acc, r)
                    } else {
                        acc
                    }
                }));
            format!("{doc}typedef struct {struct_name} {{{struct_fields}\n}} {struct_name};",
                    doc = doc,
                    struct_name = name,
                    struct_fields = fields)
        };

        self.add_to_buf(&session.module, &s);
        Ok(())
    }

    fn compile_fn_item(&mut self, session: &mut session::Session, fn_item: &ast::Item) -> Result {

        let mod_name = Self::get_mod_name(&session.module);
        let doc = Self::retrieve_docstring(&fn_item.attrs, 0);
        let name = Self::ident_to_str(fn_item.ident);
        let decl = try!(utils::fn_from_item(session, fn_item));
        let args = try!(decl.inputs
                .iter()
                .filter_map(|arg| {
                    let arg_name = match utils::arg_name_from_pattern(session, &*arg.pat) {
                        Ok(a) => a,
                        Err(e) => return Some(Err(e)),
                    };
                    match self.type_factory(&mod_name)
                        .rust_to_c(&*arg.ty, &arg_name, Some(session)) {
                        Err(e) => Some(Err(e)),
                        Ok(None) => None,
                        Ok(Some(s)) => Some(Ok(s.trim().to_owned())),
                    }
                })
                .fold_results(Vec::new(), |mut acc, r| {
                    acc.push(r);
                    acc
                }))
            .iter()
            .join(", ");
        let name = format!("{}({})", name, args);
        let ret: String = match decl.output {
            ast::FunctionRetTy::Default(_) => format!("void {}", name),
            ast::FunctionRetTy::Ty(ref t) => {
                try!(self.type_factory(&mod_name).rust_to_c(&*t, &name, Some(session))).unwrap()
            }
        };
        let f = format!("{doc}extern {fn_decl};", doc = doc, fn_decl = ret);
        self.add_to_buf(&session.module, &f);
        Ok(())
    }

    fn compile_bindings(&self) -> Vec<File> {
        self.buf
            .iter()
            .map(|(mod_name, content)| {
                let filecontent = wrap_guard(content, mod_name);
                File {
                    path: ::std::path::PathBuf::from(format!("{}.h", mod_name)),
                    contents: filecontent,
                }
            })
            .collect()
    }

    fn language(&self) -> String {
        String::from("C")
    }
}

/// Wrap a block of code with an extern declaration.
fn wrap_extern(code: &str) -> String {
    format!(r#"
#ifdef __cplusplus
extern "C" {{
#endif{}


#ifdef __cplusplus
}}
#endif
"#,
            code)
}

/// Wrap a block of code with an include-guard.
fn wrap_guard(code: &str, id: &str) -> String {
    format!(r"#ifndef cheddar_generated_{0}_h
#define cheddar_generated_{0}_h

#include <stdint.h>
#include <stdbool.h>
{1}
#endif
",
            sanitise_id(id),
            wrap_extern(code))
}

/// Remove illegal characters from the identifier.
///
/// This is because macros names must be valid C identifiers. Note that the identifier will always
/// be concatenated onto `cheddar_generated_` so can start with a digit.
fn sanitise_id(id: &str) -> String {
    // `char.is_digit(36)` ensures `char` is in `[A-Za-z0-9]`
    id.chars().filter(|ch| ch.is_digit(36) || *ch == '_').collect()
}
