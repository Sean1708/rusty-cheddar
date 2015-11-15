// TODO: MAKE THINGS CLEARER!!!
//     - Give your variables descriptive names!!!
// RULES:
//     - Anything called once and only once shall be specified by full path.
//     - Anything called between one and five times exclusive shall be specified by one level of
//         indirection.
//     - If a module is used five times or more, it shall be imported.
//     - Anything called five times or more shall be specfied only by the item name.
//     - If you import a final item then always import it's parent.
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
use syntax::visit;

// Std
use std::io;
use std::path::PathBuf;
use std::process::Command;

// Trait
use rustc_driver::CompilerCalls;
use std::error::Error;
use syntax::print::pprust;
use syntax::visit::Visitor;


// TODO: we need to get rid of this eventually.
macro_rules! expect {
    ($e:expr,) => (expect!($e));
    ($e:expr) => ({
        match $e {
            Some(val) => val,
            None => panic!("called `expect!()` on a `None` value"),
        }
    });

    ($e:expr, $msg:expr,) => (expect!($e, $msg));
    ($e:expr, $msg:expr) => ({
        match $e {
            Some(val) => val,
            None => panic!("{}", $msg),
        }
    });
}


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
            let krate = expect!(state.expanded_crate);

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

            if let Err(e) = visitor.error {
                state.session.err(&format!("error creating header file: {}", e.description()));
            } else {
                println!("{}", visitor.buffer);
            }
        };
        control
    }
}


// TODO: I think this should be a method of CheddarVisitor so that we can set errors as needed.
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

fn check_repr_c(a: &ast::Attribute) -> bool {
    match a.node.value.node {
        // TODO: use word.first() so we don't panic.
        ast::MetaItem_::MetaList(ref name, ref word) if *name == "repr" => match word[0].node {
            // Return true only if attribute is #[repr(C)].
            ast::MetaItem_::MetaWord(ref name) if *name == "C" => true,
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
fn retrieve_docstring(a: &ast::Attribute) -> String {
    match a.node.value.node {
        ast::MetaItem_::MetaNameValue(ref name, ref val) if *name == "doc" => match val.node {
            // Docstring attributes omit the trailing newline.
            ast::Lit_::LitStr(ref docs, _) => docs.to_string() + "\n",
            _ => String::new(),
        },
        _ => String::new(),
    }
}

// TODO: custom error type
//     - span errors
//         - need span and msg
//     - internal errors
//         - need file! line! and maybe message
//         - maybe differentiate unreachable errors?
//     - io errors
//     - general errors
//         - just message
type Chesult = std::io::Result<()>;

struct CheddarVisitor {
    buffer: String,
    /// Carry error information around.
    ///
    /// We don't want to panic the compiler so we store error information and handle any error
    /// messages once we've walked the crate.
    error: Chesult,
}

impl CheddarVisitor {
    fn new() -> CheddarVisitor {
        CheddarVisitor { buffer: String::new(), error: Ok(()) }
    }

    fn parse_ty(&mut self, i: &ast::Item) {
        // TODO: Maybe use check_repr_c in the future!
        let (_, docs) = parse_attr(&i.attrs, |_| true, retrieve_docstring);
        self.buffer.push_str(&docs);

        let new_type = i.ident.name.as_str();
        let old_type = match i.node {
            ast::Item_::ItemTy(ref ty, ref generics) => {
                if generics.is_parameterized() {
                    // TODO: How to distinguish between types intended for C and public Rust types?
                    //     - Do we even need to?
                    //         - If not we should just leave this check out.
                    self.error = Err(io::Error::new(io::ErrorKind::Other, "C types can not be parameterized"));
                    return;
                }

                pprust::ty_to_string(&*ty)
            },
            _ => {
                self.error = Err(io::Error::new(io::ErrorKind::Other, concat!("internal error: ", file!(), ":", line!())));
                return;
            },
        };

        self.buffer.push_str(&format!("typedef {} {};\n\n", rust_to_c(&old_type), new_type));
    }

    fn parse_enum(&mut self, i: &ast::Item) {
        let (repr_c, docs) = parse_attr(&i.attrs, check_repr_c, retrieve_docstring);
        // If it's not #[repr(C)] then it can't be called from C.
        if !repr_c { return; }
        self.buffer.push_str(&docs);

        let name = i.ident.name.as_str();
        self.buffer.push_str(&format!("typedef enum {} {{\n", name));
        if let ast::Item_::ItemEnum(ref definition, ref generics) = i.node {
            if generics.is_parameterized() {
                // TODO: this isn't an io error. In fact this needs span info.
                self.error = Err(io::Error::new(io::ErrorKind::Other, "#[repr(C)] enums can not be parameterized"));
                return;
            }

            for var in &definition.variants {
                if !var.node.data.is_unit() {
                    // TODO: also not io error
                    self.error = Err(io::Error::new(io::ErrorKind::Other, "#[repr(C)] enums must have unit variants"));
                    return;
                }

                let (_, docs) = parse_attr(&var.node.attrs, |_| true, retrieve_docstring);
                // TODO: Some way to indent the docs.
                //     - maybe have a prepend argument to retrieve_docstring then wrap it in a closure
                self.buffer.push_str(&docs);

                self.buffer.push_str(&format!("\t{},\n", pprust::variant_to_string(var)));
            }
        } else {
            // TODO: definitely need a better error type!
            // TODO: is it even possible to reach this branch?
            self.error = Err(io::Error::new(io::ErrorKind::Other, concat!("internal error: ", file!(), ":", line!())));
        }

        self.buffer.push_str(&format!("}} {};\n\n", name));
    }

    fn parse_struct(&mut self, i: &ast::Item) {
        let (repr_c, docs) = parse_attr(&i.attrs, check_repr_c, retrieve_docstring);
        // If it's not #[repr(C)] then it can't be called from C.
        if !repr_c { return; }
        self.buffer.push_str(&docs);

        let name = i.ident.name.as_str();
        self.buffer.push_str(&format!("typedef struct {} {{\n", name));
        if let ast::Item_::ItemStruct(ref variants, ref generics) = i.node {
            if generics.is_parameterized() {
                // TODO: this isn't an io error. In fact this needs span info.
                self.error = Err(io::Error::new(io::ErrorKind::Other, "#[repr(C)] structs can not be parameterized"));
                return;
            }

            if let ast::VariantData::Struct(ref variant_vec, _) = *variants {
                for var in variant_vec {
                    let (_, docs) = parse_attr(&var.node.attrs, |_| true, retrieve_docstring);
                    self.buffer.push_str(&docs);

                    let name = expect!(var.node.ident());
                    let ty = pprust::ty_to_string(&*var.node.ty);
                    let ty = rust_to_c(&ty);
                    self.buffer.push_str(&format!("\t{} {};\n", ty, name));
                }
            } else {
                // TODO: again needs span info.
                self.error = Err(io::Error::new(io::ErrorKind::Other, "currently can not handle unit or tuple structs"));
                return;
            }
        } else {
            // TODO: definitely need a better error type!
            // TODO: is it even possible to reach this branch?
            // TODO: just use unreachable?
            self.error = Err(io::Error::new(io::ErrorKind::Other, concat!("internal error: ", file!(), ":", line!())));
        }

        self.buffer.push_str(&format!("}} {};\n\n", name));
    }

    fn parse_fn(&mut self, i: &ast::Item) {
        let (no_mangle, docs) = parse_attr(&i.attrs, check_no_mangle, retrieve_docstring);
        // If it's not #[no_mangle] then it can't be called from C.
        if !no_mangle { return; }
        self.buffer.push_str(&docs);

        let name = i.ident.name.as_str();

        if let ast::Item_::ItemFn(ref fn_decl, _, _, abi, ref generics, _) = i.node {
            use syntax::abi::Abi;
            match abi {
                // If it doesn't have a C ABI it can't be called from C.
                Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {},
                _ => return,
            }
            if generics.is_parameterized() {
                // TODO: this isn't an io error. In fact this needs span info.
                self.error = Err(io::Error::new(io::ErrorKind::Other, "`extern \"C\"` funcs can not be parameterized"));
                return;
            }

            let fn_decl: &ast::FnDecl = &*fn_decl;
            let output_type = &fn_decl.output;
            let output_type = match output_type {
                // TODO: Use the span, Sean.
                &ast::FunctionRetTy::NoReturn(_) => {
                    // TODO: are there cases when this is ok?
                    self.error = Err(io::Error::new(io::ErrorKind::Other, "panics across a C boundary are bad!"));
                    return;
                },
                &ast::FunctionRetTy::DefaultReturn(_) => "void".to_owned(),
                &ast::FunctionRetTy::Return(ref ty) => {
                    let ty = pprust::ty_to_string(&*ty);
                    rust_to_c(&ty).to_owned()
                },
            };

            self.buffer.push_str(&format!("{} {}(", output_type, name));

            for arg in &fn_decl.inputs {
                let arg_name = pprust::pat_to_string(&*arg.pat);
                let arg_type = pprust::ty_to_string(&*arg.ty);
                self.buffer.push_str(&format!("{} {}, ", arg_type, arg_name));
            }

            // Remove the trailing comma and space.
            self.buffer.pop();
            self.buffer.pop();

            self.buffer.push_str(");\n\n");
        }
        // TODO: Shoule there be an else here?
    }
}

fn rust_to_c(typ: &str) -> &str {
    // TODO: pointers (esp. function pointers).
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
    }
}


impl<'v> Visitor<'v> for CheddarVisitor {
    // We use visit_item() because we need access to the attributes.
    fn visit_item(&mut self, i: &'v ast::Item) {
        // No point doing anything if we've had an error
        // TODO: Should we return or just let compilation continue?
        if let Err(_) = self.error { return; }
        // If it's not visible it can't be called from C.
        if let ast::Visibility::Inherited = i.vis { return; }

        // Dispatch to correct method.
        match i.node {
            // TODO: Maybe these methods should return a Result?
            //     - then we could leverage try! in the methods and only assign to self.error here.
            // TODO: Check for ItemStatic and ItemConst as well.
            //     - How would this work?
            //     - Is it even possible?
            ast::Item_::ItemTy(..) => self.parse_ty(i),
            ast::Item_::ItemEnum(..) => self.parse_enum(i),
            ast::Item_::ItemStruct(..) => self.parse_struct(i),
            ast::Item_::ItemFn(..) => self.parse_fn(i),
            _ => {},
        };
        // Just keep on walkin'.
        visit::walk_item(self, i);
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

    // TODO: check this isn't already there.
    //     - or maybe check whether this is here in early_callback().
    args.push("--crate-type=dylib".to_owned());
    rustc_driver::run_compiler(&args, &mut CheddarCalls);
}
