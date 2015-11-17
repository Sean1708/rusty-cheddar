/// For weights in kg.
pub type Kg = f32;
/// For weights in lbs.
pub type Lbs = f32;

pub type M = f32;
pub type Ins = f32;

/// Signifies the colour of a person's eyes.
///
/// We want to start counting from -1 for some reason.
#[repr(C)]
pub enum Eye {
    Blue = -1,
    Green,
    /// Yes, in this example people can have red eyes.
    Red,
}

/// The only thing you need to know about a person is their age.
///
/// ... and their eye colour, weight, and height.
///
/// ... in metric obviously.
#[repr(C)]
pub struct Person {
    age: i8,
    eyes: Eye,
    weight: Kg,
    height: M,
}

/// Apparantly we want to write this private function with a C ABI.
///
/// Maybe it's for a reason, maybe we're stupid. But rusty-cheddar don't care because it'll be
/// picked up by rustc instead.
#[no_mangle]
extern fn private_c(age: i8) {
    println!("Creating a {} year old!", age);
}

/// This function is public to Rust, but not to C so it doesn't go in the header.
pub fn public_rust(weight_lbs: f32) {
    println!("Who weighs {} lbs.", weight_lbs);
}

#[allow(non_snake_case)]
#[no_mangle]
/// Creates a Person while giving us some info.
///
/// Be carfeul to use C best practices such as namespacing in the identifier.
pub extern fn Person_create(age: i8, eyes: Eye, weight_lbs: f32, height_ins: f32) -> Person {
    private_c(age);
    public_rust(weight_lbs);
    Person {
        age: age,
        eyes: eyes,
        weight: weight_lbs * 0.45,
        height: height_ins * 0.0254,
    }
}

/// Prints a person without even having to use 3D printing technology.
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
