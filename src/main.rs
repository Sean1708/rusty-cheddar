//! Currently rusty-cheddar can only be run on a single rust file and does not yet support writing
//! output to a file. There are plans to make rusty-cheddar `cargo` aware and possibly even a full
//! `rustc` replacement, but there are ongoing issues with these. rusty-cheddar also targets C99 or
//! later (for sane single line comments and use of `stdint.h` and `stdbool.h`), if you really really
//! really really really have to use an older standard then please open an issue at the [repo] and I
//! will begrudgingly figure out how to implement support for it (after arguing with you lots and lots).
//!
//! The usage is fairly simple, first you should write a rust file with a C callable API (rusty-cheddar
//! does not yet support any error checking or warnings but this is planned):
//!
//! ```rust
//! // File: ./capi.rs
//! #![allow(non_snake_case)]
//!
//! /// Typedef docstrings!
//! pub type Kg = f32;
//!
//! /// This is documentation!!!!
//! ///
//! /// With two whole lines!!
//! #[repr(C)]
//! pub enum Eye {
//!     Blue = 1,
//!     /// Unfortunately variant docstrings are not indented properly yet.
//!     Green,
//!     Red,
//! }
//!
//! /// Doccy doc for a struct-a-roony!!
//! #[repr(C)]
//! pub struct Person {
//!     age: i8,
//!     eyes: Eye,
//!     weight: Kg,
//!     height: f32,
//! }
//!
//! /// Private functions are ignored.
//! #[no_mangle]
//! extern fn private_c(age: i8) {
//!     println!("Creating a {} year old!", age);
//! }
//!
//! /// As are non-C functions
//! pub fn public_rust(weight_lbs: f32) {
//!     println!("Who weighs {} lbs.", weight_lbs);
//! }
//!
//! #[no_mangle]
//! /// Function docs.
//! pub extern fn Person_create(age: i8, eyes: Eye, weight_lbs: f32, height_ins: f32) -> Person {
//!     private_c(age);
//!     public_rust(weight_lbs);
//!     Person {
//!         age: age,
//!         eyes: eyes,
//!         weight: weight_lbs * 0.45,
//!         height: height_ins * 0.0254,
//!     }
//! }
//!
//! #[no_mangle]
//! pub extern fn Person_describe(person: Person) {
//!     let eyes = match person.eyes {
//!         Eye::Blue => "blue",
//!         Eye::Green => "green",
//!         Eye::Red => "red",
//!     };
//!     println!(
//!         "The {}m {} year old weighed {}kg and had {} eyes.",
//!         person.height, person.age, person.weight, eyes,
//!     );
//! }
//! ```
//!
//! Then just call `cheddar capi.rs > capi.h` to create the header file:
//!
//! ```c
//! #ifndef cheddar_gen_cheddar_h
//! #define cheddar_gen_cheddar_h
//!
//! #ifdef __cplusplus
//! extern "C" {
//! #endif
//!
//! #include <stdint.h>
//! #include <stdbool.h>
//!
//! /// Typedef docstrings!
//! typedef float Kg;
//!
//! /// This is documentation!!!!
//! ///
//! /// With two whole lines!!
//! typedef enum Eye {
//! 	Blue = 1,
//! /// Unfortunately variant docstrings are not indented properly yet.
//! 	Green,
//! 	Red,
//! } Eye;
//!
//! /// Doccy doc for a struct-a-roony!!
//! typedef struct Person {
//! 	int8_t age;
//! 	Eye eyes;
//! 	Kg weight;
//! 	float height;
//! } Person;
//!
//! /// Function docs.
//! Person Person_create(int8_t age, Eye eyes, float weight_lbs, float height_ins);
//!
//! void Person_describe(Person person);
//!
//! #ifdef __cplusplus
//! }
//! #endif
//!
//! #endif
//! ```
//!
//! ### Typedefs
//!
//! rusty-cheddar converts `pub type A = B` into `typedef B A;`. Types containing generics are ignored.
//!
//! ### Enums
//!
//! rusty-cheddar will convert public enums which are marked `#[repr(C)]`. If the enum is generic or
//! contains tuple or struct variants then `cheddar` will fail. rusty-cheddar should correctly handle
//! explicit discriminants.
//!
//! ### Structs
//!
//! Structs are handled very similarly to enums, they must be marked `#[repr(C)]` and they must not
//! contain generics (this currently only checked at the struct-level, generic fields are not checked).
//!
//! ### Functions
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
//!
//! [repo]: https://github.com/Sean1708/rusty-cheddar
// TODO: MAKE THINGS CLEARER!!!
//     - Give your variables descriptive names!!!
// RULES:
//     - Anything called once and only once shall be specified by full path.
//     - Anything called between one and five times exclusive shall be specified by one level of
//         indirection.
//     - If a module is used five times or more, it shall be imported.
//     - Anything called five times or more shall be specfied only by the item name.
//     - If you import a final item then import it's parent, unless it is a trait.
#![feature(rustc_private)]
#![feature(box_syntax)]

extern crate getopts;
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_resolve;
extern crate rustc_trans;
extern crate syntax;

// External
use rustc::session;
use syntax::ast;
use syntax::print::pprust;
use syntax::visit;

// Std
use std::path::PathBuf;
use std::process::Command;

// Trait
use rustc_driver::CompilerCalls;
use std::error::Error;
use std::fmt::Display;
use syntax::codemap::Pos;
use syntax::visit::Visitor;


struct CheddarCalls;

impl<'a> CompilerCalls<'a> for CheddarCalls {
    fn build_controller(
        &mut self,
        _sess: &session::Session
    ) -> rustc_driver::driver::CompileController<'a> {
        let mut control = rustc_driver::driver::CompileController::basic();
        control.after_expand.stop = rustc_driver::Compilation::Stop;
        control.after_expand.callback = box |state| {
            // As far as I'm aware this should always be Some in this callback.
            let krate = state.expanded_crate.expect(concat!(file!(), ":", line!(), ": no crate found"));

            let header_file = PathBuf::from("cheddar.h");
            let mut visitor = CheddarVisitor::new();

            visitor.buffer.push_str(&format!(
                "#ifndef cheddar_gen_{0}_h\n#define cheddar_gen_{0}_h\n\n",
                // TODO: this be horrible.
                header_file.file_stem().map(|p| p.to_str().unwrap_or("default")).unwrap_or("default"),
            ));
            visitor.buffer.push_str("#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n");
            visitor.buffer.push_str("#include <stdint.h>\n#include <stdbool.h>\n\n");

            visit::walk_crate(&mut visitor, krate);

            visitor.buffer.push_str("#ifdef __cplusplus\n}\n#endif\n\n");
            visitor.buffer.push_str("#endif\n");

            match visitor.error {
                Err(Cherror::Span(span, msg)) => state.session.span_err(span, msg),
                Err(Cherror::Internal(file, line, msg)) => {
                    state.session.fatal(&format!("{}:{}: {}", file, line, msg.unwrap_or("unknown")));
                },
                _ => println!("{}", visitor.buffer),
            };
        };
        control
    }
}


// TODO: I think this should be a method of CheddarVisitor so that we can set errors as needed.
// TODO: Maybe it would be wise to use syntax::attr here.
fn parse_attr<C, R>(attrs: &[ast::Attribute], check: C, retrieve: R) -> (bool, String)
    where C: Fn(&ast::Attribute) -> bool,
          R: Fn(&ast::Attribute) -> String,
{
    let mut check_passed = false;
    let mut retrieved_str = String::new();
    for attr in attrs {
        // Don't want to accidently set it to false after it's been set to true.
        if !check_passed { check_passed = check(attr); }
        retrieved_str.push_str(&retrieve(attr));
    }

    (check_passed, retrieved_str)
}

fn check_repr_c(attr: &ast::Attribute) -> bool {
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

fn check_no_mangle(attr: &ast::Attribute) -> bool {
    match attr.node.value.node {
        ast::MetaItem_::MetaWord(ref name) if *name == "no_mangle" => true,
        _ => false,
    }
}

// TODO: How do we do this without allocating so many Strings?
//     - With Some() of course!
fn retrieve_docstring(attr: &ast::Attribute) -> String {
    match attr.node.value.node {
        ast::MetaItem_::MetaNameValue(ref name, ref val) if *name == "doc" => match val.node {
            // Docstring attributes omit the trailing newline.
            ast::Lit_::LitStr(ref docs, _) => docs.to_string() + "\n",
            // TODO: Is this an error?
            _ => String::new(),
        },
        _ => String::new(),
    }
}


type FileName = &'static str;
type LineNumber = u32;
#[derive(Debug)]
enum Cherror {
    /// Errors in the source file.
    Span(syntax::codemap::Span, &'static str),
    /// Errors in rusty-cheddar.
    Internal(FileName, LineNumber, Option<&'static str>),
}
type Chesult = Result<(), Cherror>;

impl Display for Cherror {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Cherror::Span(span, msg) => write!(f, "error in bytes {} to {}: {}", span.lo.to_usize(), span.hi.to_usize(), msg),
            Cherror::Internal(file, line, msg) => write!(f, "internal error:{}:{}: {}", file, line, msg.unwrap_or("unknown")),
        }
    }
}

impl Error for Cherror {
    fn description(&self) -> &str {
        "error in rusty-cheddar"
    }
}

struct CheddarVisitor {
    /// The ouput C header file.
    buffer: String,
    /// Carry error information around.
    ///
    /// We don't want to panic the compiler so we store error information and handle any error
    /// messages once we've walked the crate.
    error: Chesult,
}

macro_rules! fail {
    ($container:expr, $span:expr, $msg:expr) => {{
        $container = Err(Cherror::Span($span, $msg));
        return;
    }};

    ($container:expr, $msg:expr) => {{
        $container = Err(Cherror::Internal(file!(), line!(), Some($msg)));
        return;
    }};

    ($container:expr) => {{
        $container = Err(Cherror::Internal(file!(), line!(), None));
        return;
    }};
}

impl CheddarVisitor {
    fn new() -> CheddarVisitor {
        CheddarVisitor { buffer: String::new(), error: Ok(()) }
    }

    fn parse_ty(&mut self, item: &ast::Item) {
        // TODO: Maybe use check_repr_c in the future!
        let (_, docs) = parse_attr(&item.attrs, |_| true, retrieve_docstring);

        let new_type = item.ident.name.as_str();
        let old_type = match item.node {
            ast::Item_::ItemTy(ref ty, ref generics) => {
                // rusty-cheddar ignores generics.
                if generics.is_parameterized() { return; }

                pprust::ty_to_string(&*ty)
            },
            _ => {
                // TODO: Show which incorrect Item_.
                fail!(self.error, "parse_ty called on wrong Item_");
            },
        };

        self.buffer.push_str(&docs);
        self.buffer.push_str(&format!("typedef {} {};\n\n", rust_to_c(&old_type), new_type));
    }

    fn parse_enum(&mut self, item: &ast::Item) {
        let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, retrieve_docstring);
        // If it's not #[repr(C)] then it can't be called from C.
        if !repr_c { return; }
        self.buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        self.buffer.push_str(&format!("typedef enum {} {{\n", name));
        if let ast::Item_::ItemEnum(ref definition, ref generics) = item.node {
            if generics.is_parameterized() {
                fail!(self.error, item.span, "cheddar can not handle parameterized #[repr(C)] enums");
            }

            for var in &definition.variants {
                if !var.node.data.is_unit() {
                    fail!(self.error, var.span, "cheddar can not handle #[repr(C)] enums with non-unit variants");
                }

                let (_, docs) = parse_attr(&var.node.attrs, |_| true, retrieve_docstring);
                // TODO: Some way to indent the docs.
                //     - maybe have a prepend argument to retrieve_docstring then wrap it in a closure
                self.buffer.push_str(&docs);

                self.buffer.push_str(&format!("\t{},\n", pprust::variant_to_string(var)));
            }
        } else {
            // TODO: Show which incorrect Item_.
            fail!(self.error, "parse_enum called in wrong Item_");
        }

        self.buffer.push_str(&format!("}} {};\n\n", name));
    }

    fn parse_struct(&mut self, item: &ast::Item) {
        let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, retrieve_docstring);
        // If it's not #[repr(C)] then it can't be called from C.
        if !repr_c { return; }
        self.buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        self.buffer.push_str(&format!("typedef struct {} {{\n", name));

        if let ast::Item_::ItemStruct(ref variants, ref generics) = item.node {
            if generics.is_parameterized() {
                fail!(self.error, item.span, "cheddar can not handle parameterized #[repr(C)] structs");
            }

            // TODO: maybe .fields() and .is_struct() can help here?
            if let ast::VariantData::Struct(ref variant_vec, _) = *variants {
                for var in variant_vec {
                    let (_, docs) = parse_attr(&var.node.attrs, |_| true, retrieve_docstring);
                    self.buffer.push_str(&docs);

                    let name = match var.node.ident() {
                        Some(name) => name,
                        None => fail!(self.error, "a tuple struct snuck through"),
                    };
                    let ty = pprust::ty_to_string(&*var.node.ty);
                    let ty = rust_to_c(&ty);
                    self.buffer.push_str(&format!("\t{} {};\n", ty, name));
                }
            } else {
                fail!(self.error, item.span, "cheddar can not handle unit or tuple #[repr(C)] structs");
            }
        } else {
            fail!(self.error, "parse_struct called on wrong Item_");
        }

        self.buffer.push_str(&format!("}} {};\n\n", name));
    }

    fn parse_fn(&mut self, item: &ast::Item) {
        let (no_mangle, docs) = parse_attr(&item.attrs, check_no_mangle, retrieve_docstring);
        // If it's not #[no_mangle] then it can't be called from C.
        if !no_mangle { return; }
        self.buffer.push_str(&docs);

        let name = item.ident.name.as_str();

        if let ast::Item_::ItemFn(ref fn_decl, _, _, abi, ref generics, _) = item.node {
            use syntax::abi::Abi;
            match abi {
                // If it doesn't have a C ABI it can't be called from C.
                Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {},
                _ => return,
            }
            if generics.is_parameterized() {
                fail!(self.error, item.span, "cheddar can not handle parameterized extern functions");
            }

            let fn_decl: &ast::FnDecl = &*fn_decl;
            let output_type = &fn_decl.output;
            let output_type = match output_type {
                &ast::FunctionRetTy::NoReturn(span) => {
                    // TODO: are there cases when this is ok?
                    fail!(self.error, span, "panics across a C boundary are naughty!");
                },
                &ast::FunctionRetTy::DefaultReturn(_) => "void".to_owned(),
                &ast::FunctionRetTy::Return(ref ty) => {
                    let ty = pprust::ty_to_string(&*ty);
                    rust_to_c(&ty).to_owned()
                },
            };

            self.buffer.push_str(&format!("{} {}(", output_type, name));

            // TODO: Is there a nicer way of doing this?
            let has_args = fn_decl.inputs.len() > 0;

            for arg in &fn_decl.inputs {
                let arg_name = pprust::pat_to_string(&*arg.pat);
                let arg_type = pprust::ty_to_string(&*arg.ty);
                self.buffer.push_str(&format!("{} {}, ", rust_to_c(&arg_type), arg_name));
            }

            if has_args {
                // Remove the trailing comma and space.
                self.buffer.pop();
                self.buffer.pop();
            }

            self.buffer.push_str(");\n\n");
        } else {
            // TODO: show which Item_.
            fail!(self.error, "parse_fn called on wrong Item_");
        }
    }
}

fn rust_to_c(typ: &str) -> String {
    // TODO: Pointers (esp. function pointers).
    // TODO: Is .to_owned() on a String a no-op?
    match typ {
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
        t => t,
    }.to_owned()
}


impl<'v> Visitor<'v> for CheddarVisitor {
    // We use visit_item() because we need access to the attributes.
    fn visit_item(&mut self, item: &'v ast::Item) {
        // No point doing anything if we've had an error
        // TODO: Should we return or just let compilation continue?
        if let Err(_) = self.error { return; }
        // If it's not visible it can't be called from C.
        if let ast::Visibility::Inherited = item.vis { return; }

        // Dispatch to correct method.
        match item.node {
            // TODO: Maybe these methods should return a Result?
            //     - then we could leverage try! in the methods and only assign to self.error here.
            //     - does this next!
            // TODO: Check for ItemStatic and ItemConst as well.
            //     - How would this work?
            //     - Is it even possible?
            ast::Item_::ItemTy(..) => self.parse_ty(item),
            ast::Item_::ItemEnum(..) => self.parse_enum(item),
            ast::Item_::ItemStruct(..) => self.parse_struct(item),
            ast::Item_::ItemFn(..) => self.parse_fn(item),
            _ => {},
        };
        // Just keep on walkin'.
        visit::walk_item(self, item);
    }
}


fn main() {
    let mut args: Vec<_> = std::env::args().collect();

    // TODO: We have to explicitly pass --sysroot, is there anyway around this?
    // TODO: We should probably do something better than panic here.
    let sysroot_output = Command::new("rustc").arg("--print=sysroot").output();
    let sysroot = match sysroot_output {
        Ok(ref output) => {
            if output.status.success() {
                String::from_utf8_lossy(&output.stdout)
            } else {
                panic!("`rust --print=sysroot` failed: {}", String::from_utf8_lossy(&output.stderr));
            }
        },
        Err(error) => panic!("could not run `rust --print=sysroot`: {}", error),
    };
    args.push(format!("--sysroot={}", sysroot.trim()));

    rustc_driver::run_compiler(&args, &mut CheddarCalls);
}
