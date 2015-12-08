#![feature(rustc_private)]
#![feature(plugin_registrar)]
#![feature(box_syntax)]
#![feature(stmt_expr_attributes)]

#![feature(plugin)]
#![plugin(clippy)]

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
use syntax::print::pprust;

// Internal
use std::fs;
use std::path::PathBuf;

// Traits
use std::io::Write;


pub struct CheddarPass {
    buffer: String,
    file: PathBuf,
}

type Result<T> = std::result::Result<T, (syntax::codemap::Span, String)>;

declare_lint!(CHEDDAR, Allow, "What does this actually do? Do I need it?");

impl lint::LintPass for CheddarPass {
    fn get_lints(&self) -> LintArray {
        lint_array!(CHEDDAR)
    }
}

impl lint::EarlyLintPass for CheddarPass {
    fn check_crate(&mut self, context: &EarlyContext, krate: &ast::Crate) {
        self.buffer.push_str(&format!(
            "#ifndef cheddar_gen_{0}_h\n#define cheddar_gen_{0}_h\n\n",
            self.file.file_stem().and_then(|p| p.to_str()).unwrap_or("default"),
        ));
        self.buffer.push_str("#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n");
        self.buffer.push_str("#include <stdint.h>\n#include <stdbool.h>\n\n");

        for item in &krate.module.items {
            // If it's not visible it can't be called from C.
            if let ast::Visibility::Inherited = item.vis { continue; }

            // Dispatch to correct method.
            // TODO: should these methods return Results?
            //     - Yes now that rust_to_c returns a Result
            let res = match item.node {
                // TODO: Check for ItemStatic and ItemConst as well.
                //     - How would this work?
                //     - Is it even possible?
                Item_::ItemTy(..) => self.parse_ty(context, item),
                Item_::ItemEnum(..) => self.parse_enum(context, item),
                Item_::ItemStruct(..) => self.parse_struct(context, item),
                Item_::ItemFn(..) => self.parse_fn(context, item),
                _ => Ok(()),
            };

            // Display any non-fatal errors, fatal errors are handled at cause.
            match res {
                Err((span, msg)) => context.sess.span_err(span, &msg),
                Ok(..) => {},
            };
        }

        self.buffer.push_str("#ifdef __cplusplus\n}\n#endif\n\n");
        self.buffer.push_str("#endif\n");

        let bytes_buf = self.buffer.clone().into_bytes();

        if let Err(error) = fs::File::create(&self.file).and_then(|mut f| f.write_all(&bytes_buf)) {
            context.sess.err(&format!("could not write to '{}': {}", self.file.display(), error))
        };
    }
}

// TODO: Maybe it would be wise to use syntax::attr here.
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
//     - path_to_c(&ast::Path)
//     - probably pull the FnDecl parsing logic out of parse_fn
//     - return Result<String, (Option<Span>, String)>
//         - since rusty-cheddar physically can't handle anything other than pointers, function
//           pointers, and paths
//     - -> Result<Some(String)> where None indicates a non-erroneous abort.
fn rust_to_c(ty: &ast::Ty) -> Result<String> {
    match ty.node {
        // standard pointers
        ast::Ty_::TyPtr(ref mutty) => ptr_to_c(mutty),
        // function pointers
        // ast::Ty_::TyBareFn(ref bare_fn) => fn_ptr_to_c(bare_fn),
        // ast::Ty_::TyPath(None, ref path) => path_to_c(path),
        // _ => Err((Some(ty.span), format!("cheddar can not handle the type `{}`", pprust::ty_to_string(ty)))),
        _ => {
            let ty = pprust::ty_to_string(ty);
            let new_type = if ty.starts_with("libc::") {
                // Strip off the libc::
                let ty = &ty[6..];
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
            } else {
                let ty: &str = &ty;
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
            };
            Ok(new_type.to_owned())
        },
    }
}

fn ptr_to_c(ty: &ast::MutTy) -> Result<String> {
    match ty.mutbl {
        // *const T
        ast::Mutability::MutImmutable => {
            let new_type = try!(rust_to_c(&ty.ty));
            // Prevent multiple const specifiers.
            if new_type.starts_with("const ") {
                Ok(format!("{}*", new_type))
            } else{
                Ok(format!("const {}*", new_type))
            }
        },
        // *mut T
        ast::Mutability::MutMutable => Ok(format!("{}*", try!(rust_to_c(&ty.ty)))),
    }
}

impl CheddarPass {
    fn parse_ty(&mut self, context: &EarlyContext, item: &Item) -> Result<()> {
        let (_, docs) = parse_attr(&item.attrs, |_| true, |attr| retrieve_docstring(attr, ""));

        let new_type = item.ident.name.as_str();
        let old_type = match item.node {
            Item_::ItemTy(ref ty, ref generics) => {
                // rusty-cheddar ignores generics.
                if generics.is_parameterized() { return Ok(()); }

                try!(rust_to_c(&*ty))
            },
            _ => {
                context.sess.span_fatal(item.span, "`parse_ty` called on incorrect `Item_`");
            },
        };

        self.buffer.push_str(&docs);
        self.buffer.push_str(&format!("typedef {} {};\n\n", old_type, new_type));

        Ok(())
    }

    fn parse_enum(&mut self, context: &EarlyContext, item: &Item) -> Result<()> {
        let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, |attr| retrieve_docstring(attr, ""));
        // If it's not #[repr(C)] then it can't be called from C.
        // This is usually by design so not an error.
        if !repr_c { return Ok(()); }
        self.buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        self.buffer.push_str(&format!("typedef enum {} {{\n", name));
        if let Item_::ItemEnum(ref definition, ref generics) = item.node {
            if generics.is_parameterized() {
                return Err((item.span, "cheddar can not handle parameterized `#[repr(C)]` enums".to_owned()));
            }

            for var in &definition.variants {
                if !var.node.data.is_unit() {
                    return Err((var.span, "cheddar can not handle `#[repr(C)]` enums with non-unit variants".to_owned()));
                }

                let (_, docs) = parse_attr(&var.node.attrs, |_| true, |attr| retrieve_docstring(attr, "\t"));
                self.buffer.push_str(&docs);

                self.buffer.push_str(&format!("\t{},\n", pprust::variant_to_string(var)));
            }
        } else {
            context.sess.span_fatal(item.span, "`parse_enum` called in wrong `Item_`");
        }

        self.buffer.push_str(&format!("}} {};\n\n", name));

        Ok(())
    }

    fn parse_struct(&mut self, context: &EarlyContext, item: &Item) -> Result<()> {
        let (repr_c, docs) = parse_attr(&item.attrs, check_repr_c, |attr| retrieve_docstring(attr, ""));
        // If it's not #[repr(C)] then it can't be called from C.
        // This is not an error though because it's almost always by design.
        if !repr_c { return Ok(()); }
        self.buffer.push_str(&docs);

        let name = item.ident.name.as_str();
        self.buffer.push_str(&format!("typedef struct {} {{\n", name));

        if let Item_::ItemStruct(ref variants, ref generics) = item.node {
            if generics.is_parameterized() {
                return Err((item.span, "cheddar can not handle parameterized `#[repr(C)]` structs".to_owned()));
            }

            if variants.is_struct() {
                for field in variants.fields() {
                    let (_, docs) = parse_attr(&field.node.attrs, |_| true, |attr| retrieve_docstring(attr, "\t"));
                    self.buffer.push_str(&docs);

                    let name = match field.node.ident() {
                        Some(name) => name,
                        None => context.sess.span_fatal(field.span, "a tuple struct snuck through"),
                    };
                    let ty = try!(rust_to_c(&*field.node.ty));
                    self.buffer.push_str(&format!("\t{} {};\n", ty, name));
                }
            } else {
                return Err((item.span, "cheddar can not handle unit or tuple `#[repr(C)]` structs".to_owned()));
            }
        } else {
            context.sess.span_fatal(item.span, "`parse_struct` called on wrong `Item_`");
        }

        self.buffer.push_str(&format!("}} {};\n\n", name));

        Ok(())
    }

    fn parse_fn(&mut self, context: &EarlyContext, item: &Item) -> Result<()> {
        let (no_mangle, docs) = parse_attr(&item.attrs, check_no_mangle, |attr| retrieve_docstring(attr, ""));
        // If it's not #[no_mangle] then it can't be called from C.
        if !no_mangle { return Ok(()); }

        let name = item.ident.name.as_str();

        if let Item_::ItemFn(ref fn_decl, _, _, abi, ref generics, _) = item.node {
            match abi {
                // If it doesn't have a C ABI it can't be called from C.
                Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {},
                _ => return Ok(()),
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
                ast::FunctionRetTy::Return(ref ty) => try!(rust_to_c(&*ty)),
            };

            self.buffer.push_str(&docs);
            self.buffer.push_str(&format!("{} {}(", output_type, name));

            let has_args = !fn_decl.inputs.is_empty();

            for arg in &fn_decl.inputs {
                let arg_name = pprust::pat_to_string(&*arg.pat);
                let arg_type = try!(rust_to_c(&*arg.ty));
                self.buffer.push_str(&format!("{} {}, ", arg_type, arg_name));
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

        Ok(())
    }
}


fn file_name_from_plugin_args(reg: &mut rustc_plugin::Registry) -> std::result::Result<Option<PathBuf>, ()> {
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
        #[allow(needless_range_loop)]
        for i in 0..len-1 {
            temp_pathbuf.push(match args[i].node {
                ast::MetaItem_::MetaWord(ref string) => {
                    let string_slice: &str = &string;
                    String::from(string_slice)
                },
                _ => {
                    reg.sess.span_err(args[i].span, "cheddar plugin args must be `MetaWord`s");
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
                    let string_slice: &str = &string;
                    String::from(string_slice)
            },
            _ => {
                reg.sess.span_err(args[len-1].span, "cheddar plugin args must be `MetaWord`s");
                return Err(());
            },
        });

        temp_pathbuf.set_extension("h");
        Ok(Some(temp_pathbuf))
    }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut rustc_plugin::registry::Registry) {
    let file = match file_name_from_plugin_args(reg) {
        // Error messages are done in `file_name_from_plugin_args`.
        Err(_) => return,
        Ok(file) => file,
    }
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

    let cheddar = CheddarPass { buffer: String::new(), file: file };
    reg.register_early_lint_pass(box cheddar);
}
