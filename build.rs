use std::process::Command;

fn main() {
    Command::new("rustc")
        .args(["swua.rs", "--crate-type=cdylib", "-o"])
        .arg("build/libswua.so")
        .status()
        .unwrap();

    println!("cargo:rustc-link-search=native=build");
    println!("cargo:rustc-link-lib=swua");
}
