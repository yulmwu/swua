mod swua;

use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=swua.rs");

    let s = Command::new("rustc")
        .args(["swua.rs", "--crate-type=cdylib", "-o"])
        .arg("build/libswua.so")
        .status()
        .unwrap();

    assert!(s.success());

    println!("cargo:rustc-link-search=native=build");
    println!("cargo:rustc-link-lib=swua");
}
