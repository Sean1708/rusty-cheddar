//! A compiler for C bindings.
//!
//! This simply generates a header file for the parts of a Rust API which are callable from C.

// TODO: we should use our own parse state which tracks what types are callable from C
//     - then we can give decent errors for ones that aren't
//     - will require multiple passes of the module
//         - each time a struct, enum, or typedef changes do another pass
//         - only store errors on the final pass
//             - maybe there will be some errors which will need to be stored before then
//     - search inside struct as well for whitelisted types
//     - possibly also search other crates when encountering a path

use compiler;
use compiler::Compiler;
use compiler::Result;
use compiler::Stop;
use compiler::utils;
use parse;
// TODO: get rid of the requirement on these, use compiler::utils instead.
use syntax;
use syntax::ast;
use syntax::print;

pub mod types;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Spec {
    /// Conform to the C89 standard.
    ///
    /// This will only use `/* ... */` style comments, will error when encountering fixed-width
    /// integers or booleans, and will mark diverging functions as `void`.
    C89,
    /// Conform to the C99 standard.
    ///
    /// This will include `stdint.h` and `stdbool.h` to allow fized-width integers and booleans but
    /// will mark diverging functions as `void`.
    C99,
    /// Conform to the C11 standard.
    ///
    /// This will include `stdint.h` and `stdbool.h` to allow fixed-width integers and booleans and
    /// will include `stdnoreturn.h` to mark diverging functions as `noreturn`.
    C11,
    // TODO: Gnu11,
}

impl Spec {
    /// Generate the required includes for the spec.
    fn includes(&self) -> String {
        let mut include_string = String::new();

        if *self >= Spec::C11 {
            include_string.push_str("#include <stdnoreturn.h>\n");
        }

        if *self >= Spec::C99 {
            include_string.push_str("#include <stdint.h>\n#include <stdbool.h>\n");
        }

        include_string
    }
}


pub struct C {
    /// The name to give the header file.
    name: String,
    /// Which standard the header file should conform to.
    spec: Spec,
    /// Storage for the header file.
    buffer: String,
    /// Custom C code which is placed after the `#include`s.
    custom_code: String,
}

impl C {
    /// Initialise a compiler which will compile a header file conforming to the C99 standard.
    pub fn c99() -> C {
        C {
            name: "header.h".into(),
            spec: Spec::C99,
            buffer: String::new(),
            custom_code: String::new(),
        }
    }

    /// Initialise a compiler which will compile a header file conforming to a given standard.
    pub fn with_spec(spec: Spec) -> C {
        C {
            name: "header.h".into(),
            spec: spec,
            buffer: String::new(),
            custom_code: String::new(),
        }
    }

    /// Give the header file a name.
    ///
    /// This name will be used in the include-guard and will be used to generated the name of the
    /// header file itself.
    pub fn name(&mut self, name: &str) -> &mut C {
        self.name = name.into();
        self
    }

    /// Inject custom code before the generated decalrations.
    ///
    /// The rough structure of the header file will be
    ///
    /// ```c
    /// #ifndef ...
    /// #define ...
    ///
    /// #ifdef ...
    /// extern "C" {
    /// #endif
    ///
    /// #include ...
    ///
    /// <custom code goes here>
    ///
    /// <generated code goes here>
    ///
    /// #ifdef ...
    /// }
    /// #endif
    ///
    /// #endif
    /// ```
    pub fn insert_code(&mut self, code: &str) -> &mut C {
        self.custom_code.push_str(code);
        self
    }
}

impl Compiler for C {
    fn compile_ty_item(&mut self, session: &mut parse::Session, item: &ast::Item) -> Result {
        let mut buffer = String::new();
        push_docs_from_attrs(&mut buffer, &item.attrs, "");

        let name = &item.ident.name.as_str();

        // use the first if we need our own error struct (hopefully not) otherwise the second
        let ty = try!(utils::ty_from_item(session, &item));

        // if the RHS type is generic (which isn't checked at parse) then ignore
        // TODO: this should be checkd at parse
        // TODO: further expansion would be to only write it if it's a whitelisted type
        if utils::ty_is_generic(&*ty) { return Err(Stop::Abort); };

        let type_string = try!(types::rust_to_c(session, &*ty, &name, self.spec));

        buffer.push_str(&format!("typedef {};\n\n", type_string));

        self.buffer.push_str(&buffer);
        Ok(())
    }

    fn compile_enum_item(&mut self, session: &mut parse::Session, item: &ast::Item) -> Result {
        let mut buffer = String::new();
        push_docs_from_attrs(&mut buffer, &item.attrs, "");

        let name = &item.ident.name.as_str();

        buffer.push_str(&format!("typedef enum {} {{\n", name));

        let enum_def = try!(utils::enum_from_item(session, &item));

        // utils::for_each_variant should fail on non-units variants
        try!(utils::for_each_variant(session, enum_def, |var| {
            push_docs_from_attrs(&mut buffer, &var.attrs, "\t");
            // variant_to_string requires a spanned Variant_ apparently
            // TODO: expose this as a utils function
            buffer.push_str(&format!("\t{},\n", print::pprust::variant_to_string(&syntax::codemap::spanned(syntax::codemap::BytePos(0), syntax::codemap::BytePos(0), var.clone()))));
        }));

        buffer.push_str(&format!("}} {};\n\n", name));

        self.buffer.push_str(&buffer);
        Ok(())
    }

    fn compile_struct_item(&mut self, session: &mut parse::Session, item: &ast::Item) -> Result {
        let mut buffer = String::new();
        push_docs_from_attrs(&mut buffer, &item.attrs, "");

        let name = &item.ident.name.as_str();
        buffer.push_str(&format!("typedef struct {}", name));

        let struct_vars = try!(utils::struct_from_item(session, &item));

        if !utils::is_opaque(&struct_vars) {
            buffer.push_str(" {\n");

            // TODO: `for_each_field`
            for field in struct_vars {
                push_docs_from_attrs(&mut buffer, &field.node.attrs, "\t");

                let id = field.node.ident();
                let name = match id {
                    Some(ref ident) => ident.name.as_str(),
                    None => unreachable!("a tuple struct snuck through"),
                };

                let ty = try!(types::rust_to_c(session, &*field.node.ty, &name, self.spec));
                buffer.push_str(&format!("\t{};\n", ty));
            }

            buffer.push_str("}");
        }

        buffer.push_str(&format!(" {};\n\n", name));

        self.buffer.push_str(&buffer);
        Ok(())
    }

    fn compile_fn_item(&mut self, session: &mut parse::Session, item: &ast::Item) -> Result {
        let mut buffer = String::new();
        push_docs_from_attrs(&mut buffer, &item.attrs, "");

        let fn_decl = try!(utils::fn_from_item(session, &item));

        // Handle the case when the return type is a function pointer (which requires that the
        // entire declaration is wrapped by the function pointer type) by first creating the name
        // and parameters, then passing that whole thing to `rust_to_c`.
        let mut buf_without_return = format!("{}(", &item.ident.name.as_str());

        let has_args = !fn_decl.inputs.is_empty();

        // TODO: `for_each_arg`
        for arg in &fn_decl.inputs {
            // TODO: use syntax::ast_utils::pat_is_ident to check this isn't a pattern
            let arg_name = &print::pprust::pat_to_string(&*arg.pat);
            let arg_type = try!(types::rust_to_c(session, &*arg.ty, arg_name, self.spec));
            buf_without_return.push_str(&arg_type);
            buf_without_return.push_str(", ");
        }

        if has_args {
            // Remove the trailing comma and space.
            buf_without_return.pop();
            buf_without_return.pop();
        }

        buf_without_return.push_str(")");

        let output_type = &fn_decl.output;
        let full_declaration = match *output_type {
            ast::FunctionRetTy::NoReturn(..) => {
                if self.spec >= Spec::C11 {
                    format!("noreturn void {}", buf_without_return)
                } else {
                    format!("void {}", buf_without_return)
                }
            },
            ast::FunctionRetTy::DefaultReturn(..) => format!("void {}", buf_without_return),
            ast::FunctionRetTy::Return(ref ty) => try!(types::rust_to_c(session, &*ty, &buf_without_return, self.spec)),
        };

        buffer.push_str(&full_declaration);
        buffer.push_str(";\n\n");

        self.buffer.push_str(&buffer);
        Ok(())
    }

    fn compile_bindings(&self) -> Vec<compiler::File> {
        vec![compiler::File {
            path: ::std::path::Path::new(&self.name).with_extension("h"),
            contents: wrap_guard(
                &wrap_extern(&format!(
                    "{}\n\n{}\n\n{}",
                    self.spec.includes(),
                    self.custom_code,
                    self.buffer,
                )),
                &self.name,
            ),
        }]
    }

    fn language(&self) -> String {
        format!("c-header-{:?}", self.spec)
    }
}

/// Push documentation onto the given buffer with the format:
///
/// ```c
/// /**
///  * docs go here
///  */
/// ```
///
/// and prepended by a given indent.
fn push_docs_from_attrs(buffer: &mut String, attrs: &[ast::Attribute], indent: &str) {
        let prepend = format!("{} * ", indent);
        let docs = utils::docs_from_attrs(attrs, &prepend, "");

        buffer.push_str(indent);
        buffer.push_str("/**\n");
        buffer.push_str(&docs);
        buffer.push_str(indent);
        buffer.push_str(" */\n");
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
    fn test_sanitise_id() {
        assert!(super::sanitise_id("") == "");
        assert!(super::sanitise_id("!@£$%^&*()_+") == "_");
        // https://github.com/Sean1708/rusty-cheddar/issues/29
        assert!(super::sanitise_id("filename.h") == "filenameh");
    }
}
