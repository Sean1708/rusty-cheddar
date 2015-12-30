//! # Usage
//!
//! rusty-cheddar targets C99 or later (for sane single line comments and use of `stdint.h` and
//! `stdbool.h`), if you really really really really really have to use an older standard then please
//! open an issue at the [repo] and I will begrudgingly figure out how to implement support for it
//! (after arguing with you lots and lots).
//!
//! ## As a Library
//!
//! The most useful way to use rusty-cheddar is in a build script. To do this add the following
//! `build-dependencies` section to your `Cargo.toml` (to use it as a normal library simply replace
//! `build-dependencies` with `dependencies`):
//!
//! ```toml
//! [build-dependencies]
//! rusty-cheddar = "0.3"
//! ```
//!
//! Then create the following `build.rs`:
//!
//! ```no_run
//! extern crate cheddar;
//!
//! fn main() {
//!     cheddar::Cheddar::new().expect("could not read manifest")
//!         .file("my_header.h")
//!         .compile();
//! }
//! ```
//!
//! This should work as is providing you've set up your project correctly. **Don't forget to add a
//! `build = ...` to your `[package]` section, see [the cargo docs] for more info.**
//!
//! rusty-cheddar will then create a `my_header.h` file in in `$OUT_DIR` where `$OUT_DIR` is set by
//! `cargo` (usually `target/debug/build/{your crate}_{some hash}/out`). Note that rusty-cheddar
//! emits very few warnings, it is up to the programmer to write a library which can be correctly
//! called from C.
//!
//! ### API In a Module
//!
//! You can also place your API in a module to help keep your source code neat. **Note that this
//! module must currently be only one level deep, e.g. `api::*` is fine but `api::c_api::*` is
//! not.**
//!
//! To do this you must supply the name of the module to Cheddar, then ensure that the items are
//! available in the top-level scope:
//!
//! ```no_run
//! // build.rs
//!
//! extern crate cheddar;
//!
//! fn main() {
//!     cheddar::Cheddar::new().expect("could not read manifest")
//!         .file("my_header.h")
//!         .module("c_api")
//!         .compile();
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
//! ## Type Conversions
//!
//! rusty-cheddar currently does not handle type paths (e.g. `mymod::MyType`), instead they must be `use`ed
//! first:
//!
//! ```ignore
//! // pub type MyCType = mymod::MyType;  // This will put `typedef mymod::MyType MyCType;` into the header.
//! use mymod::MyType;
//! pub type MyCType = MyType;
//! ```
//!
//! The very important exception to this rule is `libc`, types used from `libc` _must_ be qualified
//! (e.g. `libc::c_void`) so that they can be converted properly.
//!
//!
//! [the cargo docs]: http://doc.crates.io/build-script.html
//! [repo]: https://github.com/Sean1708/rusty-cheddar
//! [CppHeaderParser]: https://bitbucket.org/senex/cppheaderparser

extern crate syntex_syntax as syntax;
extern crate toml;

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


// TODO: error handling by having an enum with levels and returning Vec?
// enum Error {
//     Error(Option<Span>, String),
//     Warn(Option<Span>, String),
//     Note(Option<Span>, String),
// }
// then have
// type Result = std::result::Result<Option(String), Vec<Error>>;
// then parse_mod and parse_crate can return a vector of errors.
// compile_to_string should also return a vec of errors and should not print any.
// then we should privide a helper function for users to print out errors.

pub use syntax::errors::Level;

/// Describes an error encountered by the compiler.
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
                Level::Note => { sess.span_diagnostic.span_note(span, &self.message); },
                Level::Help => { sess.span_diagnostic.span_help(span, &self.message); },
            };
        } else {
            match self.level {
                Level::Bug => { sess.span_diagnostic.bug(&self.message); },
                Level::Fatal => { sess.span_diagnostic.fatal(&self.message); },
                Level::Error => { sess.span_diagnostic.err(&self.message); },
                Level::Warning => { sess.span_diagnostic.warn(&self.message); },
                Level::Note => { sess.span_diagnostic.note(&self.message); },
                Level::Help => { sess.span_diagnostic.help(&self.message); },
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
pub struct Cheddar {
    /// The root source file of the crate.
    input: Source,
    /// The directory in which to place the header file.
    ///
    /// Default is the environment variable `OUT_DIR` when available, otherwise it is the current
    /// directory.
    outdir: path::PathBuf,
    /// The file name of the header file.
    ///
    /// Default is `cheddar.h`.
    outfile: path::PathBuf,
    // TODO: store this as a syntax::ast::Path when allowing arbitrary modules.
    /// The module which contains the C API.
    module: Option<String>,
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

        let outdir = std::env::var_os("OUT_DIR")
            .map(path::PathBuf::from)
            .unwrap_or(path::PathBuf::new());

        Ok(Cheddar {
            input: input,
            outdir: outdir,
            outfile: path::PathBuf::from("cheddar.h"),
            module: None,
            session: syntax::parse::ParseSess::new(),
        })
    }

    /// Set the path to the root source file of the crate.
    ///
    /// This should only be used when not using a `cargo` build system.
    pub fn source_file(&mut self, path: &str) -> &mut Cheddar {
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

    /// Set the output directory.
    ///
    /// Default is [`OUT_DIR`] when available, otherwise it is the current directory.
    ///
    /// [`OUT_DIR`]: http://doc.crates.io/environment-variables.html#environment-variables-cargo-sets-for-build-scripts
    pub fn directory(&mut self, path: &str) -> &mut Cheddar {
        self.outdir = path::PathBuf::from(path);
        self
    }

    /// Set the name for the created header file.
    ///
    /// Default is `cheddar.h`.
    pub fn file(&mut self, path: &str) -> &mut Cheddar {
        self.outfile = path::PathBuf::from(path);
        self
    }

    /// Set the module which contains the header file.
    ///
    /// The module should be described using Rust's path syntax, i.e. in the same way that you
    /// would `use` the module (`"path::to::api"`). Cheddar can not yet handle multiple path
    /// segments.
    pub fn module(&mut self, module: &str) -> &mut Cheddar {
        self.module = Some(module.to_owned());
        self
    }

    /// Compile the header into a string.
    ///
    /// Returns a vector of errors which can be printed using `Cheddar::print_error`.
    pub fn compile_to_string(&self) -> Result<String, Vec<Error>> {
        let sess = &self.session;
        let file_name = self.outfile.file_stem().and_then(|p| p.to_str()).unwrap_or("default");

        let krate = match self.input {
            Source::File(ref path) => syntax::parse::parse_crate_from_file(path, vec![], sess),
            Source::String(ref source) => syntax::parse::parse_crate_from_source_str(
                "cheddar_source".to_owned(),
                // TODO: this clone could be quite costly, maybe rethink this design?
                source.clone(),
                vec![],
                sess,
            ),
        };

        if let Some(ref module) = self.module {
            parse::parse_crate(&krate, module, &file_name)
        } else {
            parse::parse_mod(&krate.module, &file_name)
        }
    }

    /// Write the header to a file.
    ///
    /// This is a convenience method for use in build scripts. If errors occur during compilation
    /// they will be printed then the function will panic.
    ///
    /// # Panics
    ///
    /// Panics on any compilation error so that the build script exits.
    pub fn compile(&self) {
        let sess = &self.session;

        if let Err(error) = std::fs::create_dir_all(&self.outdir) {
            sess.span_diagnostic.err(&format!(
                "could not create directories in '{}': {}",
                self.outdir.display(),
                error,
            ));

            panic!("errors compiling header file");
        }

        let header = match self.compile_to_string() {
            Ok(header) => header,
            Err(errors) => {
                for error in errors {
                    error.print(sess);
                }

                panic!("errors compiling header file");
            },
        };


        let file = self.outdir.join(&self.outfile);
        let bytes_buf = header.into_bytes();
        if let Err(error) = std::fs::File::create(&file).and_then(|mut f| f.write_all(&bytes_buf)) {
            sess.span_diagnostic.err(&format!("could not write to '{}': {}", file.display(), error));
            panic!("errors compiling header file");
        };
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
