//! Demonstrates a C API based around passing pointers.
extern crate cheddar;

const RUST: &'static str = r#"
// This can't be exposed to C since it is generic _and_ contains a standard library struct.
struct Data<T> {
    x: Vec<T>,
    y: Vec<T>,
}

// So we expose it as an opaque pointer.
#[allow(non_camel_case_types)]
#[repr(C)]
/// This is a thing.
///
/// Oh look, doc comments are copied over!
pub struct datalib_data_f64(Data<f64>);

// We need a constructor function since C has no knowledge of the underlying data.
#[no_mangle]
pub extern fn datalib_data_f64_create() -> *mut datalib_data_f64 {
    let data = Data {
        x: vec![],
        y: vec![],
    };

    let data = datalib_data_f64(data);
    let data = Box::new(data);

    Box::into_raw(data)
}

// And a destructor so we don't leak all over the place.
#[no_mangle]
pub extern fn datalib_data_f64_destroy(data: *mut datalib_data_f64) {
    unsafe {
        drop(Box::from_raw(data));
    }
}

#[no_mangle]
pub extern fn datalib_data_f64_append(data: *mut datalib_data_f64, x: f64, y: f64) {
    unsafe {
        (*data).x.push(x);
        (*data).y.push(y);
    }
}
"#;

fn main() {
    let header = cheddar::Cheddar::new().expect("failed to read cargo manifest")
        .source_string(RUST)
        .compile_to_string()
        .expect("header could not be compiled");

    println!("RUST SOURCE FILE:\n{}\n", RUST);
    println!("GENERATED HEADER:\n\n{}", header);
}
