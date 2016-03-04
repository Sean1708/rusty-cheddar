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
//! The very important exception to this rule is `libc`, types used from `libc` _must_ be qualified
//! (e.g. `libc::c_void`) so that they can be converted properly.
//!
//!
//! [the cargo docs]: http://doc.crates.io/build-script.html
//! [repo]: https://github.com/Sean1708/rusty-cheddar

extern crate syntex_syntax as syntax;
extern crate toml;

use std::convert;
use std::io::Read;
use std::io::Write;
use std::path;


// TODO: THE WHOLE PRIVACY STORY OF THIS CRATE NEEDS REDOING!!!

pub mod compiler;
pub mod languages;
pub mod parse;

/// Store the source code.
// TODO: this really shouldn't be public
pub enum Source {
    String(String),
    File(path::PathBuf),
}

impl Source {
    // TODO: take this by value?
    pub fn parse_crate(&self, sess: &syntax::parse::ParseSess) -> syntax::ast::Crate {
        match *self {
            Source::File(ref path) => syntax::parse::parse_crate_from_file(path, vec![], sess),
            Source::String(ref source) => syntax::parse::parse_crate_from_source_str(
                "cheddar_source".to_owned(),
                // TODO: this clone could be quite costly, maybe rethink this design?
                //     or just use a slice.
                // TODO: can we avoid the clone if we take by value?
                source.clone(),
                vec![],
                sess,
            ),
        }
    }
}

// TODO: an error in this module which has io, parse, and compile

/// The type responsible for managing the compilers of language bindings.
pub struct Binder {
    /// The Rust source code.
    input: Source,
    /// The directory in which to place all code.
    output: std::path::PathBuf,
    session: parse::Session,
    compilers: Vec<LanguageCompiler>,
}

pub type Language = String;

/// A compiled binding which has not yet been written to a file.
pub struct Binding {
    pub language: Language,
    pub files: Vec<compiler::File>,
    pub dependencies: Vec<Binding>,
}

/// A compiler for a specific language.
pub struct LanguageCompiler {
    compiler: Box<compiler::Compiler>,
    dependencies: Vec<LanguageCompiler>,
}

impl LanguageCompiler {
    /// Recursively create a compiler for a given language and any dependent compilers.
    fn new(compiler: Box<compiler::Compiler>, dependencies: Vec<Box<compiler::Compiler>>) -> LanguageCompiler {
        let mut lc_dependencies = vec![];

        for dep in dependencies {
            let recursive_dependencies = dep.dependencies();
            lc_dependencies.push(LanguageCompiler::new(dep, recursive_dependencies));
        }

        LanguageCompiler {
            compiler: compiler,
            dependencies: lc_dependencies,
        }
    }
}


impl Binder {
    /// Create a new Cheddar compiler.
    ///
    /// This can only fail if there are issues reading the cargo manifest. If there is no cargo
    /// manifest available then the source file defaults to `src/lib.rs`.
    pub fn new() -> std::result::Result<Binder, std::io::Error> {
        let source_path = try!(source_file_from_cargo());
        let input = Source::File(path::PathBuf::from(source_path));

        Ok(Binder {
            input: input,
            output: path::Path::new("target").join("binder"),
            session: parse::Session::new(),
            compilers: vec![],
        })
    }

    /// Set the path to the root source file of the crate.
    ///
    /// This should only be used when not using a `cargo` build system.
    pub fn source_file<T>(&mut self, path: T) -> &mut Binder
        where path::PathBuf: convert::From<T>,
    {
        self.input = Source::File(path::PathBuf::from(path));
        self
    }

    /// Set a string to be used as source code.
    ///
    /// Currently this should only be used with small strings as it requires at least one `.clone()`.
    pub fn source_string(&mut self, source: &str) -> &mut Binder {
        self.input = Source::String(source.to_owned());
        self
    }

    /// Set the directory in which to place all output bindings.
    ///
    /// This defaults to `target/binder`, and the output from each language will be put in it's own
    /// folder.
    pub fn output_directory<T>(&mut self, path: T) -> &mut Binder
        where path::PathBuf: convert::From<T>,
    {
        self.output = path::PathBuf::from(path);
        self
    }

    /// Set the module which contains the API which is to be bound.
    ///
    /// The module should be described using Rust's path syntax, i.e. in the same way that you
    /// would `use` the module (`"path::to::api"`).
    ///
    /// # Fails
    ///
    /// If the path is malformed (e.g. `path::to:module`).
    pub fn module(&mut self, module: &str) -> Result<&mut Binder, ()> {
        // TODO: `parse_item_from_source_str` doesn't work. Why?
        let sess = syntax::parse::ParseSess::new();
        let mut parser = ::syntax::parse::new_parser_from_source_str(
            &sess,
            vec![],
            "".into(),
            module.into(),
        );

        if let Ok(path) = parser.parse_path(syntax::parse::parser::PathParsingMode::NoTypesAllowed) {
            self.session.module = Some(path);
            Ok(self)
        } else {
            sess.span_diagnostic.err(&format!("malformed module path {:?}", module));
            Err(())
        }
    }

    /// Register a compiler to be used to generate bindings.
    pub fn register<C: compiler::Compiler + 'static>(&mut self, compiler: C) -> &mut Binder {
        let dependencies = compiler.dependencies();

        self.compilers.push(LanguageCompiler::new(Box::new(compiler), dependencies));

        self
    }

    /// Compile the bindings to strings.
    ///
    /// # Fails
    ///
    /// If there was an error during compilation.
    pub fn compile(&mut self) -> Result<Vec<Binding>, std::io::Error> {
        let krate = self.session.parse_crate(&self.input);
        // TODO: expose this as a utility function of some sort
        parse::parse_crate(&krate, &mut self.session, &mut self.compilers)
    }

    /// Compile the bindings and write them to the appropriate files.
    ///
    /// # Fails
    ///
    /// If there was an error during compilation or writing to file.
    // TODO: expose this as a utility function of some sort
    pub fn write(&mut self) -> Result<(), std::io::Error> {
        let bindings = try!(self.compile());
        write_binding(&self.output, &bindings)
    }
}

/// Recursively write bindings and their dependencies to the file system.
fn write_binding(root: &path::Path, bindings: &[Binding]) -> Result<(), std::io::Error> {
    for binding in bindings {
        let out_dir = root.join(&binding.language);
        try!(std::fs::create_dir_all(&out_dir));

        for file in &binding.files {
            let mut f = try!(std::fs::File::create(&out_dir.join(&file.path)));
            try!(f.write_all(file.contents.as_bytes()));
        }

        let dependencies_root = out_dir.join("dependencies");
        try!(write_binding(&dependencies_root, &binding.dependencies));
    }

    Ok(())
}

/// Extract the path to the root source file from a `Cargo.toml`.
fn source_file_from_cargo() -> std::result::Result<String, std::io::Error> {
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
    try!(cargo_toml.read_to_string(&mut buf));

    let table = match toml::Parser::new(&buf).parse() {
        Some(t) => t,
        None => return Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "cargo manifest could not be parsed",
        )),
    };

    // If not explicitly stated then defaults to `src/lib.rs`.
    Ok(table.get("lib")
        .and_then(|t| t.lookup("path"))
        .and_then(|s| s.as_str())
        .unwrap_or(default)
        .into())
}
