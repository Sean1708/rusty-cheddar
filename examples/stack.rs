//! Demonstrates a C API based on the stack.
extern crate cheddar;

const RUST: &'static str = r#"
#[repr(C)]
pub struct Student {
    // I'm too lazy to piss about with strings so students are now only known by an id number.
    id: i32,
    avg_mark: f64,
    // We don't store their name, but we do store their eye colour...
    eye_colour: Eye,
}

// Yes, this school caters solely to goths and anime characters.
#[repr(C)]
#[derive(Debug)]
pub enum Eye {
    Red,
    Black,
    BubbleGumPink,
}

#[no_mangle]
pub extern fn Student_print(student: Student, alt_colour: Eye) {
    println!("Student {} has {:?} eyes but I prefer {:?}.", student.id, student.eye_colour, alt_colour);
}

#[no_mangle]
pub extern fn Student_change_grade(student: Student, changer: extern fn(f64) -> f64) {
    student.avg_mark = changer(student.avg_mark);
}
"#;

fn main() {
    let header = cheddar::Cheddar::new().expect("failed to read cargo manifest")
        .source_string(RUST)
        .compile("SOME_HEADER_NAME")
        .expect("header could not be compiled");

    println!("RUST SOURCE FILE:\n{}\n", RUST);
    println!("GENERATED HEADER:\n\n{}", header);
}
