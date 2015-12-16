#![feature(plugin)]
#![plugin(cheddar(dir = "target/include", file = "header.h", hell = "j"))]

#[no_mangle]
pub extern fn add(l: u8, r: u8) -> u8 {
    l + r
}
