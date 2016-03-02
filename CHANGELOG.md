# [master]

# [0.3.2] - 2016-03-02

## Changed

- rusty-cheddar now correctly converts types in `std::os::raw`

# [0.3.1] - 2015-01-20

## Changed

- the api can now be placed in any arbitrary module

## Fixed

- the include guard is sanitised to avoid illegal characters in a macro definition


# [0.3.0] - 2015-01-10

## Changed

- the whole fucking thing!
    - no longer requires nightly
    - works as a library which leverages syntex
    - see the README for the new interface

## Added

- the `cheddar` executable which acts as a thin wrapper around the library functionality


# [0.2.0] - 2015-12-28

## Added

- support for function pointers
- support for opaque structs
    - `#[repr(C)] pub struct Foo(Vec<T>);`
    - `typedef struct Foo Foo;`
- the ability to hide your C API behind a module
    - can only be one module deep at this point in time

## Changed

- plugin arguments
    - you must now use key value pairs to specify `file` and `dir`
    - old: `#![plugin(cheddar(path,to,file))]`
    - new: `#![plugin(cheddar(dir = "path/to", file = "file.h"))]`

[master]: https://github.com/Sean1708/rusty-cheddar/compare/v0.3.2...HEAD
[0.3.2]: https://github.com/Sean1708/rusty-cheddar/compare/v0.3.1...v0.3.2
[0.3.1]: https://github.com/Sean1708/rusty-cheddar/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/Sean1708/rusty-cheddar/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/Sean1708/rusty-cheddar/compare/v0.1.0...v0.2.0
