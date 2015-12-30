//! # Usage
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
//! must currently be only one level deep, e.g. `api::*` is fine but `api::c_api::*` is not.**
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
//! ```no_run
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
//!
//! [multirust]: https://github.com/brson/multirust
//! [repo]: https://github.com/Sean1708/rusty-cheddar
//! [CppHeaderParser]: https://bitbucket.org/senex/cppheaderparser

extern crate syntex_syntax as syntax;

use std::path;
use std::io::Write;


/// Unwraps Result<Option<..>> if it is Ok(Some(..)) else returns.
macro_rules! try_some {
    ($expr:expr) => {{ match $expr {
        Ok(Some(val)) => val,
        expr => return expr,
    }}};
}

/// Print a fatal error message then panic.
macro_rules! fatal {
    () => { panic!("abort due to fatal error"); };

    ($sess:expr, $msg:expr) => {
        let _ = $sess.span_diagnostic.fatal($msg);
        fatal!();
    };

    ($sess:expr, $span:expr, $msg:expr) => {
        let _ = $sess.span_diagnostic.span_fatal($span, $msg);
        fatal!();
    }
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


type Result = std::result::Result<Option<String>, (syntax::codemap::Span, String)>;


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
    // TODO: store this as a syntax::ast::Path
    /// The module which contains the C API.
    module: Option<String>,
}

impl Cheddar {
    /// Create a new Cheddar compiler.
    pub fn new() -> Cheddar {
        // TODO: explicitly check the Cargo.toml and fall back to "src/lib.rs"
        let input = Source::File(path::PathBuf::from("src/lib.rs"));
        let outdir = std::env::var_os("OUT_DIR")
            .map(path::PathBuf::from)
            .unwrap_or(path::PathBuf::new());

        Cheddar {
            input: input,
            outdir: outdir,
            outfile: path::PathBuf::from("cheddar.h"),
            module: None,
        }
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
    pub fn compile_to_string(&self) -> String {
        let sess = syntax::parse::ParseSess::new();
        let file_name = self.outfile.file_stem().and_then(|p| p.to_str()).unwrap_or("default");

        let krate = match self.input {
            Source::File(ref path) => syntax::parse::parse_crate_from_file(path, vec![], &sess),
            Source::String(ref source) => syntax::parse::parse_crate_from_source_str(
                "cheddar_source".to_owned(),
                // TODO: this clone could be quite costly, maybe rethink this design?
                source.clone(),
                vec![],
                &sess,
            ),
        };

        if let Some(ref module) = self.module {
            match parse::parse_crate(&sess, &krate, module, &file_name) {
                Err(err) => {
                    sess.span_diagnostic.err(&err);
                    String::new()
                },
                Ok(header) => header,
            }
        } else {
            parse::parse_mod(&sess, &krate.module, &file_name)
        }
    }

    /// Write the header to a file.
    pub fn compile(&self) {
        let file = self.outdir.join(&self.outfile);

        let header = self.compile_to_string();

        let bytes_buf = header.into_bytes();
        if let Err(error) = std::fs::File::create(&file).and_then(|mut f| f.write_all(&bytes_buf)) {
            let sess = syntax::parse::ParseSess::new();
            sess.span_diagnostic.err(&format!("could not write to '{}': {}", file.display(), error))
        };
    }
}
