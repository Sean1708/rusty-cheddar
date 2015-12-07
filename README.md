# rusty-cheddar

[![Build Status](https://travis-ci.org/Sean1708/rusty-cheddar.svg)](https://travis-ci.org/Sean1708/rusty-cheddar)

A rustc compiler plugin to automatically generate C header files from Rust source files.

## Usage

Compiler plugins have not yet been stabilised so you must use a nightly compiler. If you wish to
build against stable Rust as well then I suggest [multirust](https://github.com/brson/multirust) or
[multirust-rs](https://github.com/Diggsey/multirust-rs).

rusty-cheddar targets C99 or later (for sane single line comments and use of `stdint.h` and
`stdbool.h`), if you really really really really really have to use an older standard then please
open an issue at the [repo] and I will begrudgingly figure out how to implement support for it
(after arguing with you lots and lots).

Using rusty-cheddar is very simple, first add the dependency to your `Cargo.toml`

```toml
[dependencies]
rusty-cheddar = "0.1"
```

then at the top of your `lib.rs`

```rust
#![feature(plugin)]
#![plugin(cheddar)]
```

rusty-cheddar will then create a `cheddar.h` file in your working directory containing the generated
header file. Note that rusty-cheddar emits very few warnings, it is up to the programmer to write a
library which can be correctly called from C.

You can optionally specify a path for the header file using plugin arguments. The last argument is
the name of the header file _without any extensions_ and any other arguments are directories which
do not have to exist.

```rust
#![plugin(cheddar(my_header))]
```

This will create `my_header.h` in the current working directory.

```rust
#![plugin(cheddar(target, include, my_header))]
```

This will first create the directories in `target/include` if they don't exist and will then create
`my_header.h` in `target/include`.

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

rusty-cheddar currently does not handle type paths (e.g. `mymod::MyType`), instead they must be `use`ed
first:

```rust
// pub type MyCType = mymod::MyType;  // This will put `typedef mymod::MyType MyCType;` into the header.
use mymod::MyType;
pub type MyCType = MyType;
```

The very important exception to this rule is `libc`, types used from `libc` _must_ be qualified
(e.g. `libc::c_void`) so that they can be converted properly.

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
[these docs](http://manishearth.github.io/rust-internals-docs/syntax/ast/index.html) are a good
place to start.

#### Tests

I have written a script for comparing two header files, using [CppHeaderParser]. Unfortunately due to
[this issue](https://bitbucket.org/senex/cppheaderparser/issues/26/named-structs-which-are-typedefed-to-the)
we can't compare structs, so for the time being you'll have to install
[my fork](https://bitbucket.org/Sean1708/cppheaderparser/overview):

```sh
# you'll have to download mercurial and a python (either 2 or 3 should work) first
cd '/path/to/where/you/want/to/store/the/source'
hg clone https://Sean1708@bitbucket.org/Sean1708/cppheaderparser
cd 'cppheaderparser'
pip install ply
pip install .
```


[repo]: https://github.com/Sean1708/rusty-cheddar
[CppHeaderParser]: https://bitbucket.org/senex/cppheaderparser
