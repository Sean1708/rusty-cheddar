use std::ffi::OsStr;
use std::fs::File;
use std::io::Write;
use std::process::Command;

/// Compares a generated header file to an expected one using `cmp_header.py`.
///
/// Do not put any boiler plate in the strings, only put the items you want to test.
///
/// For the rust file omit:
///
/// ```none
/// #![feature(plugin)]
/// #![plugin(cheddar)]
/// ```
///
/// For the header file omit:
///
/// ```none
/// #ifndef cheddar_gen_cheddar_h
/// #define cheddar_gen_cheddar_h
///
/// #ifdef __cplusplus
/// extern "C" {
/// #endif
///
/// #include <stdint.h>
/// #include <stdbool.h>
///
/// #ifdef __cplusplus
/// }
/// #endif
///
/// #endif
/// ```
macro_rules! cheddar_cmp_test {
    ($name:ident, $header:expr, $rust:expr) => {
        #[test]
        fn $name() {
            // Assumes tests are run from the package's Cargo.toml directory.
            let package_dir = std::env::current_dir()
                .expect("internal testing error: could not find path for package directory");
            let test_dir = package_dir.join("tests");
            let dir = test_dir.join(stringify!($name));
            let plugin_dir = package_dir
                .join("target")
                .join("debug");
            let cmp_script = test_dir.join("cmp_header.py");
            let source = concat!(stringify!($name), ".rs");

            // Create and move into a fresh directory.
            std::fs::create_dir_all(&dir)
                .expect("internal testing error: could not create directory");
                // .expect(concat!("internal testing error: ", stringify!($name), ": could not create directory"));
            std::env::set_current_dir(&dir)
                .expect("internal testing error: could not change directory");

            // Write the expected header and source files.
            File::create("expected.h")
                .expect("internal testing error: could not create expected header file")
                .write_all(concat!(
                    // Due to the way CppHeaderParser works we only need to add the #define.
                    "#define cheddar_gen_actual_h\n",
                    $header,
                ).as_bytes())
                .expect("internal testing error: could not write to expected header file");

            File::create(&source)
                .expect("internal testing error: could not create rust source file")
                .write_all(concat!(
                    "#![feature(plugin)]\n#![plugin(cheddar(actual))]\n",
                    $rust,
                ).as_bytes())
                .expect("internal testing error: could not write to rust source file");

            // Compile the header.
            let output = Command::new("rustc")
                .args(&[
                    OsStr::new("--crate-type=dylib"),
                    OsStr::new("-L"),
                    plugin_dir.as_os_str(),
                    OsStr::new("-Z"),
                    OsStr::new("no-trans"),
                    OsStr::new(&source),
                ])
                .output()
                .expect("internal testing error: could not run `rustc`");

            if !output.status.success() { panic!(
                "iternal testing error: compilation failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ); }

            // Compare the headers.
            let output = Command::new(&cmp_script)
                .arg("expected.h")
                .arg("actual.h")
                .output()
                .expect("internal testing error: could not run `cmp_header.py`");

            if !output.status.success() {
                if !output.stderr.is_empty() {
                    panic!(
                        "internal testing error: `cmp_header.py` failed: {}",
                        String::from_utf8_lossy(&output.stderr),
                    );
                } else {
                    panic!("{}", String::from_utf8_lossy(&output.stdout));
                }
            }
        }
    };
}

cheddar_cmp_test! { test_compilable_enums,
    "
    typedef enum Colours {
        Red,
        Orange,
        Yellow,
        Green,
        Blue,
        Indigo,
        Violet,
    } Colours;

    typedef enum TypesOfLabrador {
        Stupid = -8,
        Braindead,
    } TypesOfLabrador;
    ",
    "
    #[repr(C)]
    pub enum Colours {
        Red,
        Orange,
        Yellow,
        Green,
        Blue,
        Indigo,
        Violet,
    }

    // Won't appear in the output header file.
    #[allow(dead_code)]
    #[repr(C)]
    enum Planets {
        Mercury,
        Venus,
        Earth,
        Mars,
        Jupiter,
        Saturn,
        Neptune,
    }

    // Won't appear in the output header file.
    pub enum Days {
        Sunday,
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday,
    }

    #[repr(C)]
    pub enum TypesOfLabrador {
        Stupid = -8,
        Braindead,
    }
    "
}
