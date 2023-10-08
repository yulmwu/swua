#[no_mangle]
pub extern "C" fn print(x: i64) -> i64 {
    println!("Print in Rust: {}", x);
    0
}
