//! Functions for actually parsing the source file.

use compiler::Stop;
use syntax;
use syntax::ast;
use syntax::codemap::Span;

/// Store options and state required for parsing.
pub struct Session {
    /// The actual session used to emit errors, etc.
    syntax_session: ::syntax::parse::ParseSess,
    /// The module which contains the C API.
    pub module: Option<ast::Path>,
    // TODO:
    // /// The name of the compiler currently being used.
    // ///
    // /// This is used to give better error messages.
    // compiler: String,
}

impl Session {
    /// Construct a new empty session.
    pub fn new() -> Session {
        Session {
            syntax_session: syntax::parse::ParseSess::new(),
            module: None,
            // compiler: String::new(),
        }
    }

    /// Parse a crate with this session.
    pub fn parse_crate(&mut self, source: &super::Source) -> ast::Crate {
        source.parse_crate(&self.syntax_session)
    }

    // TODO: THESE SHOULDN'T PANIC!

    pub fn bug(&self, msg: &str) -> ! {
        self.syntax_session.span_diagnostic.bug(msg);
    }

    pub fn span_bug(&self, span: Span, msg: &str) -> ! {
        self.syntax_session.span_diagnostic.span_bug(span, msg);
    }

    #[allow(unused_must_use)]
    pub fn span_bug_with_note(&self, span: Span, msg: &str, note: &str) -> ! {
        // TODO: this needs to be a bug, not a fatal
        // self.syntax_session.span_diagnostic.struct_span_fatal(span, msg)
        //     .note(note)
        //     .emit();
        self.syntax_session.span_diagnostic.span_fatal_with_code(span, msg, note);
        panic!();
    }

    pub fn fatal(&self, msg: &str) -> ! {
        panic!(self.syntax_session.span_diagnostic.fatal(msg));
    }

    #[allow(unused_must_use)]
    pub fn fatal_with_help(&self, msg: &str, help: &str) -> ! {
        // self.syntax_session.span_diagnostic.struct_fatal(msg)
        //     .help(help)
        //     .emit();
        self.syntax_session.span_diagnostic.fatal(msg);
        self.syntax_session.span_diagnostic.help(help);
        panic!();
    }

    pub fn span_fatal(&self, span: Span, msg: &str) -> ! {
        panic!(self.syntax_session.span_diagnostic.span_fatal(span, msg));
    }

    pub fn err(&self, msg: &str) -> Stop {
        self.syntax_session.span_diagnostic.err(msg);
        Stop::Fail
    }

    pub fn span_err(&self, span: Span, msg: &str) -> Stop {
        self.syntax_session.span_diagnostic.span_err(span, msg);
        Stop::Fail
    }

    pub fn warn(&self, msg: &str) {
        self.syntax_session.span_diagnostic.warn(msg);
    }

    pub fn span_warn(&self, span: Span, msg: &str) {
        self.syntax_session.span_diagnostic.span_warn(span, msg);
    }

    pub fn note(&self, msg: &str) {
        self.syntax_session.span_diagnostic.note(msg);
    }

    pub fn span_note(&self, span: Span, msg: &str) {
        self.syntax_session.span_diagnostic.span_note(span, msg);
    }
}

/// Check that an expected path has been `pub use`d.
fn check_pub_use(item: &ast::Item, expected: &ast::Path) -> bool {
    if let ast::Item_::ItemUse(ref path) = item.node {
        // API has to be public to be used.
        if let ast::Visibility::Public = item.vis {
            // Easiest way to ensure all of API has been brought into scope.
            if let ast::ViewPath_::ViewPathGlob(ref path) = path.node {
                return path.segments == expected.segments;
            }
        }
    }

    false
}


// TODO: surely macros can help us out here
/// Recursively call `.compile_crate()` on all compilers.
fn apply_compile_crate(krate: &ast::Crate, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<(), Stop> {
    for compiler in compilers {
        try!(compiler.compiler.compile_crate(session, krate));
        try!(apply_compile_crate(krate, session, &mut compiler.dependencies));
    }

    Ok(())
}

/// Recursively call `.compile_mod()` on all compilers.
fn apply_compile_mod(module: &ast::Mod, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<(), Stop> {
    for compiler in compilers {
        try!(compiler.compiler.compile_mod(session, module));
        try!(apply_compile_mod(module, session, &mut compiler.dependencies));
    }

    Ok(())
}

/// Recursively call `.compile_item()` on all compilers.
fn apply_compile_item(item: &ast::Item, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<(), Stop> {
    for compiler in compilers {
        try!(compiler.compiler.compile_item(session, item));
        try!(apply_compile_item(item, session, &mut compiler.dependencies));
    }

    Ok(())
}

/// Recursively call `.compile_ty_item()` on all compilers.
fn apply_compile_ty_item(item: &ast::Item, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<(), Stop> {
    for compiler in compilers {
        try!(compiler.compiler.compile_ty_item(session, item));
        try!(apply_compile_ty_item(item, session, &mut compiler.dependencies));
    }

    Ok(())
}

/// Recursively call `.compile_enum_item()` on all compilers.
fn apply_compile_enum_item(item: &ast::Item, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<(), Stop> {
    for compiler in compilers {
        try!(compiler.compiler.compile_enum_item(session, item));
        try!(apply_compile_enum_item(item, session, &mut compiler.dependencies));
    }

    Ok(())
}

/// Recursively call `.compile_struct_item()` on all compilers.
fn apply_compile_struct_item(item: &ast::Item, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<(), Stop> {
    for compiler in compilers {
        try!(compiler.compiler.compile_struct_item(session, item));
        try!(apply_compile_struct_item(item, session, &mut compiler.dependencies));
    }

    Ok(())
}

/// Recursively call `.compile_fn_item()` on all compilers.
fn apply_compile_fn_item(item: &ast::Item, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<(), Stop> {
    for compiler in compilers {
        try!(compiler.compiler.compile_fn_item(session, item));
        try!(apply_compile_fn_item(item, session, &mut compiler.dependencies));
    }

    Ok(())
}


/// The main entry point when looking for a specific module.
///
/// Determines which module to parse, ensures it is `pub use`ed then hands off to `parse_mod`.
// TODO: NO SUPER!!!!!!!
pub fn parse_crate(krate: &ast::Crate, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<Vec<super::Binding>, ::std::io::Error> {
    try!(apply_compile_crate(krate, session, compilers)
        .map_err(|_| ::std::io::Error::new(::std::io::ErrorKind::Other, "failed to compile module")));

    let mut current_module = &krate.module;

    if let Some(ref module) = session.module {
        // First look to see if the module has been `pub use`d.
        if !krate.module.items.iter().any(|item| check_pub_use(&item, &module)) {
            session.fatal_with_help(
                &format!("module `{}` has not been brought into global scope", module),
                &format!("try putting `pub use {}::*` in your root source file", module),
            );
        }

        // For each module in the path, look for the corresponding module in the source.
        for module in &module.segments {
            let mut found = false;
            for item in &current_module.items {
                if let ast::Item_::ItemMod(ref new_module) = item.node {
                    if module.identifier == item.ident {
                        current_module = new_module;
                        found = true;
                        break;
                    }
                }
            }

            if !found {
                session.fatal(&format!(
                    "module `{}` could not be found",
                    module.identifier
                ));
            }
        }
    }

    parse_mod(&current_module, session, compilers)
}

/// The main parsing method.
///
/// Iterates through all items in the module and dispatches to the given compilers then pulls all
/// the compiled bindings together.
fn parse_mod(module: &ast::Mod, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<Vec<super::Binding>, ::std::io::Error> {
    try!(apply_compile_mod(module, session, compilers)
        .map_err(|_| ::std::io::Error::new(::std::io::ErrorKind::Other, "failed to compile module")));

    for item in &module.items {
        try!(apply_compile_item(item, session, compilers)
            .map_err(|_| ::std::io::Error::new(::std::io::ErrorKind::Other, "failed to compile module")));

        // Most bindings will require a C ABI, which in turn requires that the item is visible.
        if let ast::Visibility::Inherited = item.vis { continue; }

        // Dispatch to correct method.
        // TODO: this fails early, which is usually a bad idea for compilers
        let res = match item.node {
            // TODO: Check for ItemStatic and ItemConst as well.
            //     - How would this work?
            //     - Is it even possible?
            ast::Item_::ItemTy(..) => parse_ty(item, session, compilers),
            ast::Item_::ItemEnum(..) => parse_enum(item, session, compilers),
            ast::Item_::ItemStruct(..) => parse_struct(item, session, compilers),
            ast::Item_::ItemFn(..) => parse_fn(item, session, compilers),
            _ => Ok(()),
        };

        if let Err(Stop::Fail) = res {
            return Err(::std::io::Error::new(
                ::std::io::ErrorKind::Other,
                "parsing failure",
            ));
        }
    }

    Ok(compile_bindings(compilers))
}

fn compile_bindings(compilers: &mut [super::LanguageCompiler]) -> Vec<super::Binding> {
    let mut bindings = vec![];

    for compiler in compilers {
        let dependent_bindings = compile_bindings(&mut compiler.dependencies);
        bindings.push(super::Binding {
            language: compiler.compiler.language(),
            files: compiler.compiler.compile_bindings(),
            dependencies: dependent_bindings,
        })
    }

    bindings
}

fn parse_ty(item: &ast::Item, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<(), Stop> {
    apply_compile_ty_item(item, session, compilers)
}

fn parse_enum(item: &ast::Item, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<(), Stop> {
    // If it's not #[repr(C)] then it can't be called from C.
    if !has_c_repr(item, session) { return Ok(()); }

    if let ast::Item_::ItemEnum(_, ref generics) = item.node {
        if generics.is_parameterized() {
            return Err(session.span_err(
                item.span,
                "cheddar can not handle parameterized `#[repr(C)]` enums",
            ));
        }
    } else {
        session.span_bug_with_note(
            item.span,
            "`parse_enum` called on wrong `Item_`",
            "expected `syntax::ast::Item_::ItemEnum`",
        );
    }

    apply_compile_enum_item(item, session, compilers)
}

fn parse_struct(item: &ast::Item, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<(), Stop> {
    // If it's not #[repr(C)] then it can't be called from C.
    if !has_c_repr(item, session) { return Ok(()); }

    if let ast::Item_::ItemStruct(ref variants, ref generics) = item.node {
        if generics.is_parameterized() {
            return Err(session.span_err(
                item.span,
                "cheddar can not handle parameterized `#[repr(C)]` structs",
            ));
        }

        if !variants.is_struct() {
            return Err(session.span_err(
                item.span,
                "cheddar can not handle unit or tuple `#[repr(C)]` structs",
            ));
        }
    } else {
        session.span_bug_with_note(
            item.span,
            "`parse_struct` called on wrong `Item_`",
            "expected `syntax::ast::Item_::ItemStruct`",
        );
    }

    apply_compile_struct_item(item, session, compilers)
}

fn parse_fn(item: &ast::Item, session: &mut Session, compilers: &mut [super::LanguageCompiler]) -> Result<(), Stop> {
    // TODO: check it's no_mangle

    if let ast::Item_::ItemFn(_, _, _, abi, ref generics, _) = item.node {
        use syntax::abi::Abi;
        match abi {
            // If it doesn't have a C ABI it can't be called from C.
            Abi::C | Abi::Cdecl | Abi::Stdcall | Abi::Fastcall | Abi::System => {},
            _ => return Ok(()),
        }

        if generics.is_parameterized() {
            return Err(session.span_err(
                item.span,
                "cheddar can not handle parameterized extern functions",
            ));
        };
    } else {
        session.span_bug_with_note(
            item.span,
            "`parse_fn` called on wrong `Item_`",
            "expected `syntax::ast::Item_::ItemFn`",
        );
    }

    apply_compile_fn_item(item, session, compilers)
}

/// Check that an item has a C-compatible representation.
fn has_c_repr(item: &ast::Item, session: &mut Session) -> bool {
    item.attrs.iter().any(
        |attr| syntax::attr::find_repr_attrs(&session.syntax_session.span_diagnostic, attr).iter().any(
            |repr| repr.is_ffi_safe()
        )
    )
}
