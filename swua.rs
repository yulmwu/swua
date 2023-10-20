use std::ffi::{CStr, CString};

#[no_mangle]
pub extern "C" fn print(x: i64) -> i64 {
    println!("{x}");
    0
}

#[no_mangle]
pub extern "C" fn print_str(x: *const i8) -> *const i8 {
    let c_str = unsafe { CStr::from_ptr(x) };
    let str_slice: &str = c_str.to_str().unwrap();
    println!("{str_slice}");

    let out_str = CString::new(format!("Hello {} from Rust!", str_slice)).unwrap();
    out_str.into_raw()
}

#[no_mangle]
pub extern "C" fn print_array(x: *const i64, len: i64) -> i64 {
    let slice = unsafe { std::slice::from_raw_parts(x, len as usize) };
    println!("{slice:?}");

    0
}

#[no_mangle]
pub extern "C" fn print_struct(x: *const i64, len: i64) -> i64 {
    let slice = unsafe { std::slice::from_raw_parts(x, len as usize) };
    println!("{slice:?}");

    0
}
