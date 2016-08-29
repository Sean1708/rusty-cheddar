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
//! rusty-cheddar = "0.3.0"
//! ```
//!
//! Then create the following `build.rs`:
//!
//! ```no_run
//! // build.rs
//!
//! extern crate cheddar;
//!
//! fn main() {
//!     cheddar::Cheddar::new().expect("could not read manifest")
//!         .run_build("include/my_header.h");
//! }
//! ```
//!
//! This should work as is providing you've set up your project correctly. **Don't forget to add a
//! `build = ...` to your `[package]` section, see [the cargo docs] for more info.**
//!
//! rusty-cheddar will then create a `my_header.h` file in `include/`. Note that rusty-cheddar
//! emits very few warnings, it is up to the programmer to write a library which can be correctly
//! called from C.
//!
//! ### API In a Module
//!
//! You can also place your API in a module to help keep your source code neat. To do this you must
//! supply the name of the module to Cheddar, then ensure that the items are available in the
//! top-level scope:
//!
//! ```no_run
//! // build.rs
//!
//! extern crate cheddar;
//!
//! fn main() {
//!     cheddar::Cheddar::new().expect("could not read manifest")
//!         .module("c_api").expect("malformed module path")
//!         .run_build("target/include/rusty.h");
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
//!
//! There is also the `.compile()` and `.compile_code()` methods for finer control.
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
//! the `libc` crate and `std::os::raw`. Types from these two modules _must_
//! be fully qualified (e.g. `libc::c_void` or `std::os::raw::c_longlong)
//! so that they can be converted properly. Importing them with a `use`
//! statement will not work.
//!
//! [the cargo docs]: http://doc.crates.io/build-script.html
//! [repo]: https://github.com/Sean1708/rusty-cheddar

extern crate syntex_errors as errors;
extern crate syntex_syntax as syntax;
extern crate toml;

use std::convert;
use std::io::Read;
use std::io::Write;
use std::path;


/// Unwraps Result<Option<..>> if it is Ok(Some(..)) else returns.
macro_rules! try_some {
    ($expr:expr) => {{ match $expr {
        Ok(Some(val)) => val,
        expr => return expr,
    }}};
}


mod types;
mod parse;


pub use errors::Level;

/// Describes an error encountered by the compiler.
///
/// These can be printed nicely using the `Cheddar::print_err` method.
#[derive(Debug)]
pub struct Error {
    pub level: Level,
    span: Option<syntax::codemap::Span>,
    pub message: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}: {}", self.level, self.message)
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        match self.level {
            Level::Bug => "internal error",
            Level::Fatal | Level::Error => "error",
            Level::Warning => "warning",
            Level::Note => "note",
            Level::Help => "help",
            _ => unreachable!(),
        }
    }
}

impl Error {
    /// Use a ParseSess to print the error in the correct format.
    #[allow(unused_must_use)]
    fn print(&self, sess: &syntax::parse::ParseSess) {
        // TODO: there must be some way to reduce the amount of code here.
        // Throw away the results (with { ...; }) since they are handled elsewhere.
        if let Some(span) = self.span {
            match self.level {
                Level::Bug => { sess.span_diagnostic.span_bug(span, &self.message); },
                Level::Fatal => { sess.span_diagnostic.span_fatal(span, &self.message); },
                Level::Error => { sess.span_diagnostic.span_err(span, &self.message); },
                Level::Warning => { sess.span_diagnostic.span_warn(span, &self.message); },
                Level::Note => { sess.span_diagnostic.span_note_without_error(span, &self.message); },
                Level::Help => { sess.span_diagnostic.struct_dummy().span_help(span, &self.message); },
                _ => unreachable!(),
            };
        } else {
            match self.level {
                Level::Bug => { sess.span_diagnostic.bug(&self.message); },
                Level::Fatal => { sess.span_diagnostic.fatal(&self.message); },
                Level::Error => { sess.span_diagnostic.err(&self.message); },
                Level::Warning => { sess.span_diagnostic.warn(&self.message); },
                Level::Note => { sess.span_diagnostic.note_without_error(&self.message); },
                Level::Help => { sess.span_diagnostic.struct_dummy().help(&self.message); },
                _ => unreachable!(),
            };
        }
    }
}


/// Store the source code.
enum Source {
    String(String),
    File(path::PathBuf),
}

/// Stores configuration for the Cheddar compiler.
///
/// # Examples
///
/// Since construction can only fail if there is an error _while_ reading the cargo manifest it is
/// usually safe to call `.unwrap()` on the result (though `.expect()` is considered better
/// practice).
///
/// ```no_run
/// cheddar::Cheddar::new().expect("unable to read cargo manifest");
/// ```
///
/// If your project is a valid cargo project or follows the same structure, you can simply place
/// the following in your build script.
///
/// ```no_run
/// cheddar::Cheddar::new().expect("unable to read cargo manifest")
///     .run_build("path/to/output/file");
/// ```
///
/// If you use a different structure you should use `.source_file("...")` to set the path to the
/// root crate file.
///
/// ```no_run
/// cheddar::Cheddar::new().expect("unable to read cargo manifest")
///     .source_file("src/root.rs")
///     .run_build("include/my_header.h");
/// ```
///
/// You can also supply the Rust source as a string.
///
/// ```no_run
/// let rust = "pub type Float32 = f32;";
/// cheddar::Cheddar::new().expect("unable to read cargo manifest")
///     .source_string(rust)
///     .run_build("target/include/header.h");
/// ```
///
/// If you wish to hide your C API behind a module you must specify the module with `.module()`
/// (don't forget to `pub use` the items in the module!).
///
/// ```no_run
/// cheddar::Cheddar::new().expect("unable to read cargo manifest")
///     .module("c_api").expect("malformed header path")
///     .run_build("header.h");
/// ```
pub struct Cheddar {
    /// The root source file of the crate.
    input: Source,
    // TODO: this should be part of a ParseOpts struct
    /// The module which contains the C API.
    module: Option<syntax::ast::Path>,
    /// Custom C code which is placed after the `#include`s.
    custom_code: String,
    /// The current parser session.
    ///
    /// Used for printing errors.
    session: syntax::parse::ParseSess,
}

impl Cheddar {
    /// Create a new Cheddar compiler.
    ///
    /// This can only fail if there are issues reading the cargo manifest. If there is no cargo
    /// manifest available then the source file defaults to `src/lib.rs`.
    pub fn new() -> std::result::Result<Cheddar, Error> {
        let source_path = try!(source_file_from_cargo());
        let input = Source::File(path::PathBuf::from(source_path));

        Ok(Cheddar {
            input: input,
            module: None,
            custom_code: String::new(),
            session: syntax::parse::ParseSess::new(),
        })
    }

    /// Set the path to the root source file of the crate.
    ///
    /// This should only be used when not using a `cargo` build system.
    pub fn source_file<T>(&mut self, path: T) -> &mut Cheddar
        where path::PathBuf: convert::From<T>,
    {
        self.input = Source::File(path::PathBuf::from(path));
        self
    }

    /// Set a string to be used as source code.
    ///
    /// Currently this should only be used with small strings as it requires at least one `.clone()`.
    pub fn source_string(&mut self, source: &str) -> &mut Cheddar {
        self.input = Source::String(source.to_owned());
        self
    }

    /// Set the module which contains the header file.
    ///
    /// The module should be described using Rust's path syntax, i.e. in the same way that you
    /// would `use` the module (`"path::to::api"`).
    ///
    /// # Fails
    ///
    /// If the path is malformed (e.g. `path::to:module`).
    pub fn module(&mut self, module: &str) -> Result<&mut Cheddar, Vec<Error>> {
        // TODO: `parse_item_from_source_str` doesn't work. Why?
        let sess = syntax::parse::ParseSess::new();
        let result = {
            let mut parser = ::syntax::parse::new_parser_from_source_str(
                &sess,
                vec![],
                "".into(),
                module.into(),
            );
            parser.parse_path(syntax::parse::parser::PathStyle::Mod)
        };

        if let Ok(path) = result {
            self.module = Some(path);
            Ok(self)
        } else {
            Err(vec![Error {
                level: Level::Fatal,
                span: None,
                message: format!("malformed module path `{}`", module),
            }])
        }
    }

    /// Insert custom code before the declarations which are parsed from the Rust source.
    ///
    /// If you compile a full header file, this is inserted after the `#include`s.
    ///
    /// This can be called multiple times, each time appending more code.
    pub fn insert_code(&mut self, code: &str) -> &mut Cheddar {
        self.custom_code.push_str(code);
        self
    }

    /// Compile just the code into header declarations.
    ///
    /// This does not add any include-guards, includes, or extern declarations. It is mainly
    /// intended for internal use, but may be of interest to people who wish to embed
    /// rusty-cheddar's generated code in another file.
    pub fn compile_code(&self) -> Result<String, Vec<Error>> {
        let sess = &self.session;
        let krate = match self.input {
            Source::File(ref path) => syntax::parse::parse_crate_from_file(path, vec![], sess),
            Source::String(ref source) => syntax::parse::parse_crate_from_source_str(
                "cheddar_source".to_owned(),
                // TODO: this clone could be quite costly, maybe rethink this design?
                //     or just use a slice.
                source.clone(),
                vec![],
                sess,
            ),
        }.unwrap();

        if let Some(ref module) = self.module {
            parse::parse_crate(&krate, module)
        } else {
            parse::parse_mod(&krate.module)
        }.map(|source| format!("{}\n\n{}", self.custom_code, source))
    }

    /// Compile the header declarations then add the needed `#include`s.
    ///
    /// Currently includes:
    ///
    /// - `stdint.h`
    /// - `stdbool.h`
    fn compile_with_includes(&self) -> Result<String, Vec<Error>> {
        let code = try!(self.compile_code());

        Ok(format!("#include <stdint.h>\n#include <stdbool.h>\n\n{}", code))
    }

    /// Compile a header while conforming to C89 (or ANSI C).
    ///
    /// This does not include `stdint.h` or `stdbool.h` and also wraps single line comments with
    /// `/*` and `*/`.
    ///
    /// `id` is used to help generate the include guard and may be empty.
    ///
    /// # TODO
    ///
    /// This is intended to be a public API, but currently comments are not handled correctly so it
    /// is being kept private.
    ///
    /// The parser should warn on uses of `bool` or fixed-width integers (`i16`, `u32`, etc.).
    #[allow(dead_code)]
    fn compile_c89(&self, id: &str) -> Result<String, Vec<Error>> {
        let code = try!(self.compile_code());

        Ok(wrap_guard(&wrap_extern(&code), id))
    }

    /// Compile a header.
    ///
    /// `id` is used to help generate the include guard and may be empty.
    pub fn compile(&self, id: &str) -> Result<String, Vec<Error>> {
        let code = try!(self.compile_with_includes());

        Ok(wrap_guard(&wrap_extern(&code), id))
    }

    /// Write the header to a file.
    pub fn write<P: AsRef<path::Path>>(&self, file: P) -> Result<(), Vec<Error>> {
        let file = file.as_ref();

        if let Some(dir) = file.parent() {
            if let Err(error) = std::fs::create_dir_all(dir) {
                return Err(vec![Error {
                    level: Level::Fatal,
                    span: None,
                    message: format!("could not create directories in '{}': {}", dir.display(), error),
                }]);
            }
        }

        let file_name = file.file_stem().map_or("default".into(), |os| os.to_string_lossy());
        let header = try!(self.compile(&file_name));

        let bytes_buf = header.into_bytes();
        if let Err(error) = std::fs::File::create(&file).and_then(|mut f| f.write_all(&bytes_buf)) {
            Err(vec![Error {
                level: Level::Fatal,
                span: None,
                message: format!("could not write to '{}': {}", file.display(), error),
            }])
        } else {
            Ok(())
        }
    }

    /// Write the header to a file, panicking on error.
    ///
    /// This is a convenience method for use in build scripts. If errors occur during compilation
    /// they will be printed then the function will panic.
    ///
    /// # Panics
    ///
    /// Panics on any compilation error so that the build script exits and prints output.
    pub fn run_build<P: AsRef<path::Path>>(&self, file: P) {
        if let Err(errors) = self.write(file) {
            for error in &errors {
                self.print_error(error);
            }

            panic!("errors compiling header file");
        }
    }

    /// Print an error using the ParseSess stored in Cheddar.
    pub fn print_error(&self, error: &Error) {
        error.print(&self.session);
    }
}

/// Extract the path to the root source file from a `Cargo.toml`.
fn source_file_from_cargo() -> std::result::Result<String, Error> {
    let cargo_toml = path::Path::new(
        &std::env::var_os("CARGO_MANIFEST_DIR")
            .unwrap_or(std::ffi::OsString::from(""))
    ).join("Cargo.toml");

    // If no `Cargo.toml` assume `src/lib.rs` until told otherwise.
    let default = "src/lib.rs";
    let mut cargo_toml = match std::fs::File::open(&cargo_toml) {
        Ok(value) => value,
        Err(..) => return Ok(default.to_owned()),
    };

    let mut buf = String::new();
    match cargo_toml.read_to_string(&mut buf) {
        Ok(..) => {},
        Err(..) => return Err(Error {
            level: Level::Fatal,
            span: None,
            message: "could not read cargo manifest".into(),
        }),
    };

    let table = match toml::Parser::new(&buf).parse() {
        Some(value) => value,
        None => return Err(Error {
            level: Level::Fatal,
            span: None,
            message: "could not parse cargo manifest".into(),
        }),
    };

    // If not explicitly stated then defaults to `src/lib.rs`.
    Ok(table.get("lib")
        .and_then(|t| t.lookup("path"))
        .and_then(|s| s.as_str())
        .unwrap_or(default)
        .into())
}

/// Wrap a block of code with an extern declaration.
fn wrap_extern(code: &str) -> String {
    format!(r#"
#ifdef __cplusplus
extern "C" {{
#endif

{}

#ifdef __cplusplus
}}
#endif
"#, code)
}

/// Wrap a block of code with an include-guard.
fn wrap_guard(code: &str, id: &str) -> String {
    format!(r"
#ifndef cheddar_generated_{0}_h
#define cheddar_generated_{0}_h

{1}

#endif
", sanitise_id(id), code)
}

/// Remove illegal characters from the identifier.
///
/// This is because macros names must be valid C identifiers. Note that the identifier will always
/// be concatenated onto `cheddar_generated_` so can start with a digit.
fn sanitise_id(id: &str) -> String {
    // `char.is_digit(36)` ensures `char` is in `[A-Za-z0-9]`
    id.chars().filter(|ch| ch.is_digit(36) || *ch == '_').collect()
}


#[cfg(test)]
mod test {
    #[test]
    fn sanitise_id() {
        assert!(super::sanitise_id("") == "");
        assert!(super::sanitise_id("!@£$%^&*()_+") == "_");
        // https://github.com/Sean1708/rusty-cheddar/issues/29
        assert!(super::sanitise_id("filename.h") == "filenameh");
    }
}
