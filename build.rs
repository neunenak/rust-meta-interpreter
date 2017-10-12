extern crate includedir_codegen;

use includedir_codegen::Compression;

fn main() {
    includedir_codegen::start("WEBFILES")
        .dir("static", Compression::Gzip)
        .build("static.rs")
        .unwrap();
}
