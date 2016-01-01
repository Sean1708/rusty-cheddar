extern crate cheddar;
#[macro_use] extern crate clap;

fn main() {
    let matches = clap::App::new("cheddar")
        .version(&crate_version!())
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
        cheddar.module(&module);
    }

    if let Some(output) = matches.value_of("OUTPUT") {
        let path = std::path::Path::new(&output);

        if let Some(dir) = path.parent() {
            cheddar.directory(dir);
        }

        if let Some(file) = path.file_name() {
            cheddar.file(file);
        }
    }

    cheddar.compile();
}
