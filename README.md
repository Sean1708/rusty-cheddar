# rusty-cheddar

A rustc compiler drop-in for automatically generating C header files from Rust
source files.

## Installation

rusty-cheddar is not yet on [crates.io](https://crates.io) but installation is simple with `cargo
install`:

    cargo install --git https://github.com/Sean1708/rusty-cheddar

or if you're a Homebrew user let me plug [cargo-brew](https://github.com/Sean1708/cargo-brew):

    cargo brew --git https://github.com/Sean1708/rusty-cheddar

Note that compiler drop-ins will only work on nightly releases of Rust. If you want to run multiple
versions of Rust you might consider [multirust](https://github.com/brson/multirust) or it's [pure
Rust equivalent](https://github.com/Diggsey/multirust-rs).

## Usage

```none
$ cat test.rs
#[repr(C)]
pub struct Person {
    age: i32,
    height: f64,
}
$ cheddar test.rs
#ifndef cheddar_gen_cheddar_h
#define cheddar_gen_cheddar_h

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

typedef struct Person {
	int32_t age;
	double height;
} Person;

#ifdef __cplusplus
}
#endif

#endif
$ cheddar test.rs > test.h
```

Currently rusty-cheddar can only be run on a single rust file and does not yet support writing
output to a file. There are plans to make rusty-cheddar `cargo` aware and possibly even a full
`rustc` replacement, but there are ongoing issues with these. rusty-cheddar also targets C99 or
later (for sane single line comments and use of `stdint.h` and `stdbool.h`), if you really really
really really really have to use an older standard then please open an issue at the [repo] and I
will begrudgingly figure out how to implement support for it (after arguing with you lots and lots).

The usage is fairly simple, first you should write a rust file with a C callable API (rusty-cheddar
does not yet support any error checking or warnings but this is planned) and then just call
`cheddar` on the file. `cheddar` prints the header to stdout (for now, I'm working on a way to print
it to a suitable file) so you probably want to run it as `cheddar capi.rs > capi.h`, after checking
that it's correct.

In the examples below, boilerplate has been omitted from the header.

### Typedefs

rusty-cheddar converts `pub type A = B` into `typedef B A;`. Types containing generics are ignored.

Rust:

```rust
type UInt32 = u32;
pub type UInt64 = u64;
pub type MyOption<T> = Option<T>
```

Header:

```C
// Some boilerplate omitted.
typedef uint64_t UInt64;
// Some more boilerplate omitted.
```

### Enums

rusty-cheddar will convert public enums which are marked `#[repr(C)]`. If the enum is generic or
contains tuple or struct variants then `cheddar` will fail. rusty-cheddar should correctly handle
explicit discriminants.

Rust:

```rust
#[repr(C)]
pub enum Colours {
    Red = -6,
    Blue,
    Green = 7,
    Yellow,
}

// This would fail is it was #[repr(C)].
pub enum Tastes<T> {
    Savoury,
    Sweet,
}

// This would fail if it was public.
#[repr(C)]
enum Units {
    Kg(f64),
    M(f64),
    S(f64),
    A(f64),
    K(f64),
    Mol(f64),
    Cd(f64),
}
```

Header:

```C
// Some boilerplate omitted.
typedef enum Colours {
        Red = -6,
        Blue,
        Green = 7,
        Yellow,
} Colours;
// Some more boilerplate omitted.
```

### Structs

Structs are handled very similarly to enums, they must be public, marked `#[repr(C)]`, and they must not
contain generics (this currently only checked at the struct-level, generic fields are not checked).

Rust:

```rust
#[repr(C)]
pub struct Person {
    age: i32,
    height: f64,
    weight: f64,
}
```

Header:

```C
// Some boilerplate omitted.
typedef struct Person {
        int32_t age;
        double height;
        double weight;
} Person;
// Some more boilerplate omitted.
```

### Functions

For rusty-cheddar to pick up on a function declaration it must be public, marked `#[no_mangle]` and
have one of the following ABIs:

- C
- Cdecl
- Stdcall
- Fastcall
- System

I'm not totally up to speed on calling conventions so if you believe one of these has been including
in error, or if one has been omitted, then please open an issue at the [repo].

rusty-cheddar will fail on functions which are marked as diverging (`-> !`).

Rust:

```rust
use std::ops::Add;

#[no_mangle]
pub extern fn hello() {
    println!("Hello!");
}

fn add<O, R, L: Add<R, Output=O>>(l: L, r: R) -> O {
    l + r
}

#[no_mangle]
#[allow(non_snake_case)]
pub extern fn MyAdd_add_u8(l: u8, r: u8) -> u8 {
    add(l, r)
}

#[no_mangle]
#[allow(non_snake_case)]
pub extern fn MyAdd_add_u16(l: u16, r: u16) -> u16 {
    add(l, r)
}
```

Header:

```C
// Some boilerplate omitted.
void hello();

uint8_t MyAdd_add_u8(uint8_t l, uint8_t r);

uint16_t MyAdd_add_u16(uint16_t l, uint16_t r);
// Some more boilerplate omitted.
```

### Type Conversions

As of commit 976d215ad6c4cdc370dbda161f33fb8b4e02bcad the function which converts types handles all
numeric types and any user defined types correctly. Types defined in other crates (notably `libc`
and `std`) are not handled correctly and function pointers are not handled correctly.

## Contributing

Contributions to rusty-cheddar are more than welcome.

### Bugs

If you find a bug or have a feature request please open an issue. I can't guarantee that I'll fix it
but I'll give it a damn good go.

If you find the source code unclear in any way then I consider that a bug. I try to make my source
code as clear as possible but I'm not very good at it, so any help in that regard is appreciated.

### PRs

I love pull requests they tend to make my job much easier, so if you want to fix a bug or implement a
feature yourself then that would be great. If you're confused by anything or need some pointers on
how to proceed then feel free to open an issue so that I can help, otherwise
[this tutorial](https://github.com/nrc/stupid-stats) and
[these docs](http://manishearth.github.io/rust-internals-docs/syntax/ast/index.html) are a good
place to start.


[repo]: https://github.com/Sean1708/rusty-cheddar
