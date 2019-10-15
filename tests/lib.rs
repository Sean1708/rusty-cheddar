extern crate cheddar;

use std::process::Command;

macro_rules! inner_cheddar_cmp_test {
    ($name:ident $(#[$attr:meta])*, $compile:expr, $header:expr) => {
        #[test]
        $(#[$attr])*
        fn $name() {
            let expected = $header;

            let actual = match $compile {
                Ok(actual) => actual,
                Err(errors) => {
                    for error in errors {
                        println!("{:?}", error);
                    }
                    panic!("compilation errors");
                },
            };

            let cmp_script = std::env::current_dir()
                .map(|p| p.join("tests/cmp_header.py"))
                .expect("internal testing error: unable to find current directory");

            // Compare the headers.
            let output = Command::new(&cmp_script)
                .arg(&expected)
                .arg(&actual)
                .output()
                .expect("internal testing error: could not run `cmp_header.py`");

            if !output.status.success() {
                if !output.stderr.is_empty() {
                    panic!(
                        "internal testing error: `cmp_header.py` failed: {}",
                        String::from_utf8_lossy(&output.stderr),
                    );
                } else {
                    panic!("{}: {}", String::from_utf8_lossy(&output.stdout), actual);
                }
            }
        }
    };
}


/// Compares a generated header file to an expected one using `cmp_header.py`.
///
/// Do not put any boiler plate in the strings, only put the items you want to test.
///
/// For the rust file omit:
///
/// ```no_run
/// #![feature(plugin)]
/// #![plugin(cheddar)]
/// ```
///
/// For the header file omit:
///
/// ```C
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
/// ...
///
/// #ifdef __cplusplus
/// }
/// #endif
///
/// #endif
/// ```
macro_rules! cheddar_cmp_test {
    ($name:ident, custom $custom:expr, $header:expr, $rust:expr) => {
        inner_cheddar_cmp_test! {
            $name,
            cheddar::Cheddar::new().unwrap().source_string($rust).insert_code($custom).compile_code(),
            $header
        }
    };

    ($name:ident, api $api:expr, $header:expr, $rust:expr) => {
        inner_cheddar_cmp_test! {
            $name,
            cheddar::Cheddar::new().unwrap().source_string($rust).module($api).unwrap().compile_code(),
            $header
        }
    };

    ($name:ident, xfail, $header:expr, $rust:expr) => {
        inner_cheddar_cmp_test! {
            $name #[should_panic],
            cheddar::Cheddar::new().unwrap().source_string($rust).compile_code(),
            $header
        }
    };

    ($name:ident, $header:expr, $rust:expr) => {
        inner_cheddar_cmp_test! {
            $name,
            cheddar::Cheddar::new().unwrap().source_string($rust).compile_code(),
            $header
        }
    };
}



cheddar_cmp_test! { compilable_typedefs,
    "
    typedef int64_t Int64;
    ",
    "
    pub type Int64 = i64;
    // Shouldn't appear in output header file.
    type Float32 = f32;
    // Shouldn't appear in output header file.
    pub type AResult<T> = Result<T, ()>;
    "
}

cheddar_cmp_test! { compilable_enums,
    "
    typedef enum Colours {
        Colours_Red,
        Colours_Orange,
        Colours_Yellow,
        Colours_Green,
        Colours_Blue,
        Colours_Indigo,
        Colours_Violet,
    } Colours;

    typedef enum TypesOfLabrador {
        TypesOfLabrador_Stupid = -8,
        TypesOfLabrador_Braindead,
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

    // Shouldn't appear in the output header file.
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

    // Shouldn't appear in the output header file.
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

cheddar_cmp_test! { compilable_structs,
    "
    typedef struct Student {
        int32_t id;
        int32_t roll;
        double score;
    } Student;
    ",
    "
    #[repr(C)]
    pub struct Student {
        id: i32,
        roll: i32,
        score: f64,
    }

    // Shouldn't appear in the output header file.
    #[allow(dead_code)]
    #[repr(C)]
    struct Complex {
        real: f64,
        imag: f64,
    }

    // Shouldn't appear in output header file.
    pub struct Employee {
        id: i32,
        age: i16,
        salary: f64,
    }
    "
}

cheddar_cmp_test! { opaque_structs,
    "
    typedef struct Foo Foo;
    ",
    "
    #[repr(C)]
    pub struct Foo(Vec<Option<i32>>);
    "
}

cheddar_cmp_test! { compilable_functions,
    "
    int64_t add_i64(int64_t lhs, int64_t rhs);
    int get_errno(void);
    ",
    r#"
    #[no_mangle]
    pub extern fn add_i64(lhs: i64, rhs: i64) -> i64 {
        lhs + rhs
    }
    #[no_mangle]
    pub extern fn get_errno() -> libc::int {
      unsafe { *libc::__errno_location() }
    }

    // Shouldn't appear in the output header file.
    #[allow(dead_code)]
    #[no_mangle]
    extern fn add_i32(lhs: i32, rhs: i32) -> i32 {
        lhs + rhs
    }

    // Shouldn't appear in output header file.
    #[no_mangle]
    pub fn add_i16(lhs: i16, rhs: i16) -> i16 {
        lhs + rhs
    }

    // Shouldn't appear in output header file.
    pub fn add_i8(lhs: i8, rhs: i8) -> i8 {
        lhs + rhs
    }
    "#
}

cheddar_cmp_test! { compilable_function_pointers,
    "
    typedef int32_t* const* (*TwoIntPtrFnPtr)(double* argument);

    double cmp(double (*cmp_fn)(double lhs, double rhs), double lhs, double rhs);

    typedef bool (*Foo)(double, double);

    typedef void (*NoOp)(void);

    void (*signal(int sig, void (*func)(int)))(int);
    ",
    r#"
    pub type TwoIntPtrFnPtr = extern fn(argument: *mut f64) -> *const *mut i32;

    #[no_mangle]
    pub extern fn cmp(cmp_fn: extern fn(lhs: f64, rhs: f64) -> f64, lhs: f64, rhs: f64) -> f64 {
        cmp_fn(lhs, rhs)
    }

    pub type Foo = extern fn(f64, f64) -> bool;

    pub type NoOp = extern fn();

    #[no_mangle]
    pub extern fn signal(sig: libc::c_int, func: extern fn(libc::c_int)) -> extern fn(libc::c_int) {
        println!("{}", sig);
        func
    }
    "#
}

cheddar_cmp_test! { pure_rust_types,
    "
    typedef void MyVoid;
    typedef float Float32;
    typedef double Float64;
    typedef int8_t Int8;
    typedef int16_t Int16;
    typedef int32_t Int32;
    typedef int64_t Int64;
    typedef intptr_t Int;
    typedef uint8_t UInt8;
    typedef uint16_t UInt16;
    typedef uint32_t UInt32;
    typedef uint64_t UInt64;
    typedef uintptr_t UInt;
    typedef bool Bool;
    typedef double* FloatArray;
    typedef bool const* LogicArray;
    typedef int32_t**** FourPointers;
    typedef float const* const* TwoPointers;
    ",
    "
    pub type MyVoid = ();
    pub type Float32 = f32;
    pub type Float64 = f64;
    pub type Int8 = i8;
    pub type Int16 = i16;
    pub type Int32 = i32;
    pub type Int64 = i64;
    pub type Int = isize;
    pub type UInt8 = u8;
    pub type UInt16 = u16;
    pub type UInt32 = u32;
    pub type UInt64 = u64;
    pub type UInt = usize;
    pub type Bool = bool;
    pub type FloatArray = *mut f64;
    pub type LogicArray = *const bool;
    pub type FourPointers = *mut *mut *mut *mut i32;
    pub type TwoPointers = *const *const f32;
    "
}

cheddar_cmp_test! { libc_types,
    "
    typedef void CVoid;
    typedef float CFloat;
    typedef double CDouble;
    typedef char CChar;
    typedef signed char CSChar;
    typedef unsigned char CUChar;
    typedef short CShort;
    typedef unsigned short CUShort;
    typedef int CInt;
    typedef unsigned int CUInt;
    typedef long CLong;
    typedef unsigned long CULong;
    typedef long long CLongLong;
    typedef unsigned long long CULongLong;
    typedef FILE CFile;
    ",
    "
    // This probably isn't the best way to test considering most people will use the crates.io version.
    #![feature(libc)]
    extern crate libc;
    pub type CVoid = libc::c_void;
    pub type CFloat = libc::c_float;
    pub type CDouble = libc::c_double;
    pub type CChar = libc::c_char;
    pub type CSChar = libc::c_schar;
    pub type CUChar = libc::c_uchar;
    pub type CShort = libc::c_short;
    pub type CUShort = libc::c_ushort;
    pub type CInt = libc::c_int;
    pub type CUInt = libc::c_uint;
    pub type CLong = libc::c_long;
    pub type CULong = libc::c_ulong;
    pub type CLongLong = libc::c_longlong;
    pub type CULongLong = libc::c_ulonglong;
    pub type CFile = libc::FILE;
    "
}

cheddar_cmp_test! { std_os_raw_types,
    "
    typedef void CVoid;
    typedef char CChar;
    typedef double CDouble;
    typedef float CFloat;
    typedef int CInt;
    typedef long CLong;
    typedef long long CLongLong;
    typedef signed char CSChar;
    typedef short CShort;
    typedef unsigned char CUChar;
    typedef unsigned int CUInt;
    typedef unsigned long CULong;
    typedef unsigned long long CULongLong;
    typedef unsigned short CUShort;
    ",
    "
    pub type CVoid = std::os::raw::c_void;
    pub type CChar = std::os::raw::c_char;
    pub type CDouble = std::os::raw::c_double;
    pub type CFloat = std::os::raw::c_float;
    pub type CInt = std::os::raw::c_int;
    pub type CLong = std::os::raw::c_long;
    pub type CLongLong = std::os::raw::c_longlong;
    pub type CSChar = std::os::raw::c_schar;
    pub type CShort = std::os::raw::c_short;
    pub type CUChar = std::os::raw::c_uchar;
    pub type CUInt = std::os::raw::c_uint;
    pub type CULong = std::os::raw::c_ulong;
    pub type CULongLong = std::os::raw::c_ulonglong;
    pub type CUShort = std::os::raw::c_ushort;
    "
}

// Verify we don't accept module types without a full prefix.
cheddar_cmp_test! { module_no_prefix, xfail,
    "
    typedef CVoid void;
    ",
    "
    extern crate libc;
    use libc::FILE;
    use std::os::raw;
    pub type CVoid = raw::c_void;
    pub type CFile = FILE;
    "
}


cheddar_cmp_test! { module, api "api",
    "
    typedef float Float;
    ",
    "
    pub use api::*;
    mod api {
        pub type Float = f32;
    }
    "
}

cheddar_cmp_test! { inside_module, api "c::api",
    "
    typedef float Float;
    ",
    "
    pub use c::api::*;
    mod c {
        mod api {
            pub type Float = f32;
        }
    }
    "
}

cheddar_cmp_test! { custom,
    custom "
    typedef F64 MyF64;
    ",
    "
    typedef double F64;
    typedef F64 MyF64;
    ",
    "
    pub type F64 = f64;
    "
}

cheddar_cmp_test! { general_interplay,
    "
    typedef float Kg;
    typedef float Lbs;
    typedef float M;
    typedef float Ins;

    typedef enum Eye {
        Eye_Blue = -1,
        Eye_Green,
        Eye_Red,
    } Eye;

    typedef struct Person {
        int8_t age;
        Eye eyes;
        Kg weight;
        M height;
    } Person;

    Person Person_create(int8_t age, Eye eyes, float weight_lbs, float height_ins);
    void Person_describe(Person person);
    ",
    r#"
    pub type Kg = f32;
    pub type Lbs = f32;
    pub type M = f32;
    pub type Ins = f32;

    #[repr(C)]
    pub enum Eye {
        Blue = -1,
        Green,
        Red,
    }

    #[repr(C)]
    pub struct Person {
        age: i8,
        eyes: Eye,
        weight: Kg,
        height: M,
    }

    #[allow(non_snake_case)]
    #[no_mangle]
    pub extern fn Person_create(age: i8, eyes: Eye, weight_lbs: f32, height_ins: f32) -> Person {
        Person {
            age: age,
            eyes: eyes,
            weight: weight_lbs * 0.45,
            height: height_ins * 0.0254,
        }
    }

    #[allow(non_snake_case)]
    #[no_mangle]
    pub extern fn Person_describe(person: Person) {
        let eyes = match person.eyes {
            Eye::Blue => "blue",
            Eye::Green => "green",
            Eye::Red => "red",
        };
        println!(
            "The {}m {} year old weighed {}kg and had {} eyes.",
            person.height, person.age, person.weight, eyes,
        );
    }
    "#
}
