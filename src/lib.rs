#![feature(rustc_private)]
#![feature(box_syntax)]
#![feature(plugin_registrar)]

#[macro_use] extern crate rustc;
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
use syntax::print::pprust;

// Internal
use std::fs;
use std::path::PathBuf;

// Traits
use std::io::Write;


pub struct CheddarPass {
    buffer: String,
    file: Option<PathBuf>,
}

declare_lint!(CHEDDAR, Allow, "What does this actually do? Do I need it?");

impl lint::LintPass for CheddarPass {
    fn get_lints(&self) -> LintArray {
        lint_array!(CHEDDAR)
    }
}

impl lint::EarlyLintPass for CheddarPass {
    fn check_crate(&mut self, context: &EarlyContext, krate: &ast::Crate) {
        let file = self.file.clone().unwrap_or(PathBuf::from("cheddar.h"));
        self.buffer.push_str(&format!(
            "#ifndef cheddar_gen_{0}_h\n#define cheddar_gen_{0}_h\n\n",
            // TODO: this be horrible.
            file.file_stem().map(|p| p.to_str().unwrap_or("default")).unwrap_or("default"),
        ));
        self.buffer.push_str("#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n");
        self.buffer.push_str("#include <stdint.h>\n#include <stdbool.h>\n\n");

        for item in &krate.module.items {
            // If it's not visible it can't be called from C.
            if let ast::Visibility::Inherited = item.vis { continue; }

            // Dispatch to correct method.
            match item.node {
                // TODO: Check for ItemStatic and ItemConst as well.
                //     - How would this work?
                //     - Is it even possible?
                Item_::ItemTy(..) => self.parse_ty(context, item),
                Item_::ItemEnum(..) => self.parse_enum(context, item),
                Item_::ItemStruct(..) => self.parse_struct(context, item),
                Item_::ItemFn(..) => self.parse_fn(context, item),
                _ => {},
            };
        }

        self.buffer.push_str("#ifdef __cplusplus\n}\n#endif\n\n");
        self.buffer.push_str("#endif\n");

        let bytes_buf = self.buffer.clone().into_bytes();

        if let Err(error) =  fs::File::create(&file).and_then(|mut f| f.write_all(&bytes_buf)) {
            context.sess.err(&format!("could not write to '{}': {}", file.display(), error))
        };
    }
}

// TODO: Maybe it would be wise to use syntax::attr here.
fn parse_attr<C, R>(attrs: &[Attribute], check: C, retrieve: R) -> (bool, String)
    where C: Fn(&Attribute) -> bool,
          R: Fn(&Attribute) -> String,
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

fn check_no_mangle(attr: &Attribute) -> bool {
    match attr.node.value.node {
        ast::MetaItem_::MetaWord(ref name) if *name == "no_mangle" => true,
        _ => false,
    }
}

// TODO: How do we do this without allocating so many Strings?
//     - With Some() of course!
fn retrieve_docstring(attr: &Attribute) -> String {
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

fn rust_to_c(typ: &str) -> String {
    // TODO: Function pointers.
    // TODO: const {}*
    //     - Is there an issue doing `const const type**`?
    if typ.starts_with("*mut") {
        // Remove the "*mut".
        let typ = &typ[4..].trim();
        format!("{}*", rust_to_c(typ))
    } else if typ.starts_with("*const") {
        // Remove the "*const".
        let typ = &typ[6..].trim();
        format!("const {}*", rust_to_c(typ))
    } else {
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
            typ => typ,
        }.to_owned()
    }
}

impl CheddarPass {
    fn parse_ty(&mut self, context: &EarlyContext, item: &Item) {
        let (_, docs) = parse_attr(&item.attrs, |_| true, retrieve_docstring);

        let new_type = item.ident.name.as_str();
        let old_type = match item.node {
            Item_::ItemTy(ref ty, ref generics) => {
                // rusty-cheddar ignores generics.
                if generics.is_parameterized() { return; }

                pprust::ty_to_string(&*ty)
            },
            _ => {
                context.sess.span_fatal(item.span, "`parse_ty` called on incorrect `Item_`");
            },
        };

        self.buffer.push_str(&docs);
        self.buffer.push_str(&format!("typedef {} {};\n\n", rust_to_c(&old_type), new_type));
    }

    fn parse_enum(&mut self, context: &EarlyContext, item: &Item) {
        let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, retrieve_docstring);
        // If it's not #[repr(C)] then it can't be called from C.
        if !repr_c { return; }
        self.buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        self.buffer.push_str(&format!("typedef enum {} {{\n", name));
        if let Item_::ItemEnum(ref definition, ref generics) = item.node {
            if generics.is_parameterized() {
                context.sess.span_err(item.span, "cheddar can not handle parameterized `#[repr(C)]` enums");
                return;
            }

            for var in &definition.variants {
                if !var.node.data.is_unit() {
                    context.sess.span_err(var.span, "cheddar can not handle `#[repr(C)]` enums with non-unit variants");
                    return;
                }

                let (_, docs) = parse_attr(&var.node.attrs, |_| true, retrieve_docstring);
                // TODO: Some way to indent the docs.
                //     - maybe have a prepend argument to retrieve_docstring then wrap it in a closure
                self.buffer.push_str(&docs);

                self.buffer.push_str(&format!("\t{},\n", pprust::variant_to_string(var)));
            }
        } else {
            context.sess.span_fatal(item.span, "`parse_enum` called in wrong `Item_`");
        }

        self.buffer.push_str(&format!("}} {};\n\n", name));
    }

    fn parse_struct(&mut self, context: &EarlyContext, item: &Item) {
        let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, retrieve_docstring);
        // If it's not #[repr(C)] then it can't be called from C.
        if !repr_c { return; }
        self.buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        self.buffer.push_str(&format!("typedef struct {} {{\n", name));

        if let Item_::ItemStruct(ref variants, ref generics) = item.node {
            if generics.is_parameterized() {
                context.sess.span_err(item.span, "cheddar can not handle parameterized `#[repr(C)]` structs");
                return;
            }

            // TODO: maybe .fields() and .is_struct() can help here?
            if let ast::VariantData::Struct(ref variant_vec, _) = *variants {
                for var in variant_vec {
                    let (_, docs) = parse_attr(&var.node.attrs, |_| true, retrieve_docstring);
                    self.buffer.push_str(&docs);

                    let name = match var.node.ident() {
                        Some(name) => name,
                        None => context.sess.span_fatal(var.span, "a tuple struct snuck through"),
                    };
                    let ty = pprust::ty_to_string(&*var.node.ty);
                    let ty = rust_to_c(&ty);
                    self.buffer.push_str(&format!("\t{} {};\n", ty, name));
                }
            } else {
                context.sess.span_err(item.span, "cheddar can not handle unit or tuple `#[repr(C)]` structs");
            }
        } else {
            context.sess.span_fatal(item.span, "`parse_struct` called on wrong `Item_`");
        }

        self.buffer.push_str(&format!("}} {};\n\n", name));
    }

    fn parse_fn(&mut self, context: &EarlyContext, item: &Item) {
        let (no_mangle, docs) = parse_attr(&item.attrs, check_no_mangle, retrieve_docstring);
        // If it's not #[no_mangle] then it can't be called from C.
        if !no_mangle { return; }

        let name = item.ident.name.as_str();

        if let Item_::ItemFn(ref fn_decl, _, _, abi, ref generics, _) = item.node {
            match abi {
                // If it doesn't have a C ABI it can't be called from C.
                Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {},
                _ => return,
            }
            if generics.is_parameterized() {
                context.sess.span_err(item.span, "cheddar can not handle parameterized extern functions");
                return;
            }

            let fn_decl: &ast::FnDecl = &*fn_decl;
            let output_type = &fn_decl.output;
            let output_type = match output_type {
                &ast::FunctionRetTy::NoReturn(span) => {
                    // TODO: are there cases when this is ok?
                    context.sess.span_err(span, "panics across a C boundary are naughty!");
                    return;
                },
                &ast::FunctionRetTy::DefaultReturn(_) => "void".to_owned(),
                &ast::FunctionRetTy::Return(ref ty) => {
                    let ty = pprust::ty_to_string(&*ty);
                    rust_to_c(&ty).to_owned()
                },
            };

            self.buffer.push_str(&docs);
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
            context.sess.span_fatal(item.span, "`parse_fn` called on wrong `Item_`");
        }
    }
}


fn file_name_from_plugin_args(reg: &mut rustc::plugin::Registry) -> Result<Option<PathBuf>, ()> {
    let args = reg.args();
    if args.is_empty() {
        Ok(None)
    } else {
        // All plugin arguments should be `MetaWord`s.
        // Last argument is the file name without the ".h" extension.
        // E.g.
        //     #![plugin(cheddar(target, debug, include, my_header))]
        let mut temp_pathbuf = PathBuf::new();
        let len = args.len();

        // Push the given directories.
        // Don't iterate over the last element since that needs to be converted into a file.
        for i in 0..len-1 {
            temp_pathbuf.push(match args[i].node {
                ast::MetaItem_::MetaWord(ref string) => {
                    let string_slice: &str = &string;
                    String::from(string_slice)
                },
                _ => {
                    reg.sess.span_err(args[i].span, "malformed plugin args");
                    return Err(());
                },
            })
        }

        // Create all the directories before we push the file name.
        if let Err(error) = fs::create_dir_all(&temp_pathbuf) {
            reg.sess.err(&format!("could not create directories in '{}': {}", temp_pathbuf.display(), error));
            return Err(());
        }

        // Push the header file name.
        temp_pathbuf.push(match args[len-1].node {
            ast::MetaItem_::MetaWord(ref string) => {
                    // TODO: There must be a better way.
                    let string_slice: &str = &string;
                    String::from(string_slice)
            },
            _ => {
                reg.sess.span_err(args[len-1].span, "malformed plugin args");
                return Err(());
            },
        });

        temp_pathbuf.set_extension("h");
        Ok(Some(temp_pathbuf))
    }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut rustc::plugin::Registry) {
    let file = match file_name_from_plugin_args(reg) {
        Err(_) => return,
        Ok(file) => file,
    };
    let cheddar = CheddarPass { buffer: String::new(), file: file };
    reg.register_early_lint_pass(box cheddar);
}
