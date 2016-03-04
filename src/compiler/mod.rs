//! Extensible compilation into arbitrary languages.
//!
//! The `Compiler` trait makes it relatively simple to compile bindings for any language, even the
//! C header compiler is just an implementation of this.

use parse::Session;
use syntax::ast::{Crate, Item, Mod};

pub mod utils;


pub type Result = ::std::result::Result<(), Stop>;

/// Indicate why the compilation was stopped early.
pub enum Stop {
    /// The compilation segment was aborted.
    ///
    /// This is used to indicate that the item should not be compiled and is therefore not an error
    /// but rather an indication that this segment of compilation should not continue.
    Abort,
    /// The compilation segment failed.
    Fail,
}

/// A description of the output file.
///
/// This may be useful for library writers who wish do do something special with the output code
/// before writing it to a file.
pub struct File {
    /// The path for this source file.
    pub path: ::std::path::PathBuf,
    /// The contents of this source file.
    pub contents: String,
}

/// Define a compiler for a custom language's bindings.
///
/// Each method is given an `&Item` but many convenience functions are available to make working
/// with these quick and simple.
pub trait Compiler {
    /// Called on the crate.
    ///
    /// This can be used to ensure certain invariants are upheld by the user, or to implement a
    /// custom parsing method.
    ///
    /// Does nothing by default.
    fn compile_crate(&mut self, _session: &mut Session, _krate: &Crate) -> Result {
        Ok(())
    }

    /// Called on the API module.
    ///
    /// This can be used to ensure certain invariants are upheld by the user, or to implement a
    /// custom parsing method.
    ///
    /// Does nothing by default.
    fn compile_mod(&mut self, _session: &mut Session, _module: &Mod) -> Result {
        Ok(())
    }

    /// Called on every item in the API module.
    ///
    /// This is called regardless of visibility, attributes, or generics. Useful for when you need
    /// complete control over the information in the item.
    ///
    /// Does nothing by default.
    fn compile_item(&mut self, _session: &mut Session, _item: &Item) -> Result {
        Ok(())
    }

    /// Compile a type definition.
    ///
    /// `pub type A = B;`
    ///
    /// # Needed
    ///
    /// - name_from_item
    /// - docs_from_item
    ///     - fn docs_from_item(&Item, prepend: &str, append: &str) -> String
    ///     - `docs_from_item(item, "\t/* ", " */")`
    ///     - or maybe docs_from_attrs
    /// - ty_from_item
    fn compile_ty_item(&mut self, session: &mut Session, type_item: &Item) -> Result;

    /// Compile an enum.
    ///
    /// The enum is guaranteed to be `#[repr(C)]` and public. The enum is guaranteed not to contain
    /// generics or non-unit variants.
    ///
    /// # Needed
    ///
    /// - name_from_item
    /// - docs_from_attrs
    /// - variants_from_item
    /// - name_from_ident
    /// - name_from_variant
    /// - variant_to_string
    fn compile_enum_item(&mut self, session: &mut Session, enum_item: &Item) -> Result;

    /// Compile a struct.
    ///
    /// The struct is guaranteed to be `#[repr(C)]` and public. The struct is guaranteed not to be
    /// a unit- or tuple- struct, and not contain any generics.
    ///
    /// # Needed
    ///
    /// - name_from_item
    /// - docs_from_attrs
    /// - fields_from_item
    /// - name_from_ident
    fn compile_struct_item(&mut self, session: &mut Session, struct_item: &Item) -> Result;

    /// Compile a function.
    ///
    /// The function is guaranteed to be m`#[no_mangle]`, of a C ABI, and public. The function is
    /// guaranteed not to contain any generics.
    ///
    /// # Needed
    ///
    /// - name_from_item
    /// - args_from_item
    /// - functionretty_from_item
    /// - item_returns
    /// - return_type_from_item
    /// - docs_from_item
    fn compile_fn_item(&mut self, session: &mut Session, fn_item: &Item) -> Result;

    /// Compile bindings for the language.
    ///
    /// If any `compile_*` function has returned `Error::Fail` then this will not be run.
    fn compile_bindings(&self) -> Vec<File>;

    /// The langauge that this compiler compiles for.
    ///
    /// Different sets of options can produces different identifiers (for example `C` includes the
    /// standard that it conforms to) allowing someone to compile different bindings for different
    /// versions of a language.
    ///
    /// If two compilers return the same value then one may overwrite the other.
    fn language(&self) -> String;

    /// Any dependencies that this compiler has.
    ///
    /// For example many languages are dependent on having a header file, so the `C` compiler would
    /// be set up with the correct options.
    ///
    /// Returns an empty vector by default.
    fn dependencies(&self) -> Vec<Box<Compiler>> {
        vec![]
    }
}
