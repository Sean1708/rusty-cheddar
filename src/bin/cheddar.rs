extern crate cheddar;
#[macro_use] extern crate clap;

fn main() {
    let matches = clap::App::new("cheddar")
        .version(crate_version!())
        .author("Sean Marshallsay <srm.1708@gmail.com>")
        .about("create a C header file using a Rust source file")
        .arg(clap::Arg::with_name("FILE")
             .short("-f")
             .long("--file")
             .conflicts_with("STRING")
             .takes_value(true)
             .help("the root source file"))
        .arg(clap::Arg::with_name("STRING")
             .short("-s")
             .long("--string")
             .conflicts_with("FILE")
             .takes_value(true)
             .help("use a string as the source code"))
        .arg(clap::Arg::with_name("MODULE")
             .short("-m")
             .long("--module")
             .takes_value(true)
             .help("the module containing the C API"))
        .arg(clap::Arg::with_name("OUTPUT")
             .index(1)
             .help("set the output file name and path"))
        .get_matches();

    let mut cheddar = cheddar::Cheddar::new().expect("cargo manifest could not be read");

    if let Some(file) = matches.value_of("FILE") {
        cheddar.source_file(&file);
    } else if let Some(string) = matches.value_of("STRING") {
        cheddar.source_string(&string);
    }

    if let Some(module) = matches.value_of("MODULE") {
        if let Err(errs) = cheddar.module(&module) {
            for err in errs {
                cheddar.print_error(&err);
            }

            panic!("errors setting module");
        }
    }

    if let Some(output) = matches.value_of("OUTPUT") {
        cheddar.run_build(&output);
    } else {
        cheddar.run_build("cheddar.h");
    };
}
