// RULES:
//     - Anything called once and only once shall be specified by full path.
//     - Anything called between one and five times exclusive shall be specified by one level of
//         indirection.
//     - If a module is used fice times or more, it shall be imported.
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
use rustc::session::config;
use syntax::ast;
use syntax::diagnostics::registry;
use syntax::visit;

// Std
use std::io;
use std::path::PathBuf;

// Trait
use rustc_driver::CompilerCalls;
use std::error::Error;
use syntax::visit::Visitor;


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


struct CheddarCalls {
    dir: Option<PathBuf>,
    file: Option<PathBuf>,
    default_calls: rustc_driver::RustcDefaultCalls,
}

impl CheddarCalls {
    fn new(dir: Option<PathBuf>, file: Option<PathBuf>) -> CheddarCalls {
        CheddarCalls {
            dir: dir,
            file: file,
            default_calls: rustc_driver::RustcDefaultCalls,
        }
    }
}

impl<'a> CompilerCalls<'a> for CheddarCalls {
    fn early_callback(
        &mut self,
        m: &getopts::Matches,
        r: &registry::Registry,
        c: syntax::diagnostic::ColorConfig,
    ) -> rustc_driver::Compilation {
        self.default_calls.early_callback(m, r, c)
    }

    fn late_callback(
        &mut self,
        m: &getopts::Matches,
        s: &session::Session,
        i: &config::Input,
        odir: &Option<PathBuf>,
        ofile: &Option<PathBuf>,
    ) -> rustc_driver::Compilation {
        let dir = self.dir.clone();
        let dir = dir.or(odir.clone());
        self.dir = dir;

        let file = self.file.clone();
        let file = file.or(ofile.clone());
        self.file = file;

        self.default_calls.late_callback(m, s, i, odir, ofile)
    }

    fn some_input(
        &mut self,
        input: config::Input,
        input_path: Option<PathBuf>
    ) -> (config::Input, Option<PathBuf>) {
        self.default_calls.some_input(input, input_path)
    }

    fn no_input(
        &mut self,
        m: &getopts::Matches,
        o: &config::Options,
        odir: &Option<PathBuf>,
        ofile: &Option<PathBuf>,
        r: &registry::Registry
    ) -> Option<(config::Input, Option<PathBuf>)> {
        self.default_calls.no_input(m, o, odir, ofile, r)
    }

    fn build_controller(
        &mut self,
        _sess: &session::Session
    ) -> rustc_driver::driver::CompileController<'a> {
        let mut control = rustc_driver::driver::CompileController::basic();
        // TODO: let file = RefCell::new(self.file.clone());
        // TODO: let dir = RefCell::new(self.dir.clone());
        control.after_expand.callback = box |state| {
            // As far as I'm aware this should always be Some in this callback.
            let krate = state.expanded_crate.expect(concat!(file!(), ":", line!()));

            // TODO: let dir = dir.unwrap_or(state.out_dir.map(|p| p.to_path_buf()).unwrap_or(state.session.working_dir.clone()));
            // TODO: let file = file.unwrap_or(state.crate_name.map(|p| PathBuf::from(p)).expect(concat!(file!(), ":", line!())));
            let header_file = PathBuf::from("cheddar.h");
            let mut visitor = CheddarVisitor::new();

            visitor.buffer.push_str(&format!(
                "#ifndef cheddar_gen_{0}_h\n#define cheddar_gen_{0}_h\n\n",
                // TODO: this be horrible.
                header_file.file_stem().map(|p| p.to_str().unwrap_or("default")).unwrap_or("default"),
            ));
            visitor.buffer.push_str("#include <stdint.h>\n#include <stdbool.h>\n\n");

            visit::walk_crate(&mut visitor, krate);

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

// TODO: custom error type
//     - span errors
//         - need span and msg
//     - internal errors
//         - nedd file! line! and maybe message
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

    fn parse_enum(&mut self, i: &ast::Item) {
        // If it's not visible it can't be called from C.
        match i.vis {
            ast::Visibility::Inherited => return,
            _ => {},
        };
        // If it's not #[repr(C)] then it can't be called from C.
        if !i.attrs.iter().any(check_repr_c) { return; }

        let name = i.ident.name.as_str();
        self.buffer.push_str(&format!("typedef enum {} {{\n", name));
        if let ast::Item_::ItemEnum(ref definition, ref generics) = i.node {
            if generics.is_parameterized() {
                // TODO: this isn't an io error. In fact this needs span info.
                self.error = Err(io::Error::new(io::ErrorKind::Other, "#[repr(C)] enums can not be parameterized"));
                return;
            }

            // TODO: refactor this into it's own function?
            for var in &definition.variants {
                let var = &var.node;
                if !var.data.is_unit() {
                    // TODO: also not io error
                    self.error = Err(io::Error::new(io::ErrorKind::Other, "#[repr(C)] enums must have unit variants"));
                    return
                }
                match var.disr_expr {
                    // Handle a variant with default values.
                    Some(ref expr) => match expr.node {
                        // TODO: This only handles positive integers.
                        ast::Expr_::ExprLit(ref lit) => match lit.node {
                            ast::Lit_::LitInt(ref i, _) => self.buffer.push_str(&format!("\t{} = {},\n", var.name.name.as_str(), i)),
                            _ => panic!("<more error handling>"),
                        },
                        _ => panic!("<insert proper error handling here>"),
                    },
                    None => self.buffer.push_str(&format!("\t{},\n", var.name.name.as_str())),
                };
            }

            self.buffer.push_str(&format!("}} {};\n\n", name));
        } else {
            // TODO: definitely need a better error type!
            self.error = Err(io::Error::new(io::ErrorKind::Other, concat!("internal error: ", file!(), ":", line!())));
        }
    }
}

impl<'v> Visitor<'v> for CheddarVisitor {
    // We use visit_item() because we need access to the attributes.
    fn visit_item(&mut self, i: &'v ast::Item) {
        // No point doing anything if we've had an error
        if let Err(_) = self.error { return; }
        // Dispatch to correct method.
        match i.node {
            // TODO: Maybe these methods should return a Result?
            ast::Item_::ItemEnum(_, _) => self.parse_enum(i),
            _ => {},
        };
        // Just keep on walkin'.
        visit::walk_item(self, i);
    }
}


// TODO: take ref to String
fn index_of(haystack: &Vec<String>, needle: String) -> Option<usize> {
    for (index, elem) in haystack.iter().enumerate() {
        if *elem == needle {
            return Some(index);
        }
    }
    None
}

fn main() {
    let mut args: Vec<_> = std::env::args().collect();
    // TODO: can we do this in early_callback?
    // TODO: better error checking.
    let index = index_of(&args, "--cheddar-dir".to_owned());
    let dir = index.map(|i| {
        // Remove --cheddar-dir.
        args.remove(i);
        // Get directory path.
        let arg = args.remove(i);
        PathBuf::from(arg)
    });
    let index = index_of(&args, "--cheddar-file".to_owned());
    let file = index.map(|i| {
        // Remove --cheddar-file.
        args.remove(i);
        // Get file path.
        let arg = args.remove(i);
        PathBuf::from(arg)
    });
    // TODO: check this isn't already there.
    //     - or maybe check whether this is here in early_callback().
    args.push("--crate-type=dylib".to_owned());
    rustc_driver::run_compiler(&args, &mut CheddarCalls::new(dir, file));
}
