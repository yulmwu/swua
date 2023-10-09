use std::ffi::{CStr, CString};

#[no_mangle]
pub extern "C" fn print(x: i64) -> i64 {
    println!("Print in Rust: {}", x);
    0
}

#[no_mangle]
pub extern "C" fn print_str(x: *const i8) -> *const i8 {
    let c_str = unsafe { CStr::from_ptr(x) };
    let str_slice: &str = c_str.to_str().unwrap();
    println!("out> {}", str_slice);

    let out_str = CString::new(format!("Hello {} from Rust!", str_slice)).unwrap();
    out_str.into_raw()
}
