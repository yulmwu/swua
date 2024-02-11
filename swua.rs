use std::ffi::{CStr, CString};

/// # print
///
/// hash: print_82c34a9dde377ee6
#[no_mangle]
pub extern "C" fn print_82c34a9dde377ee6(x: i64) -> i64 {
    println!("{x}");
    0
}

/// # print_float
///
/// hash: print_float_6c87b26222079ccc
#[no_mangle]
pub extern "C" fn print_float_6c87b26222079ccc(x: f64) -> i64 {
    println!("{x}");
    0
}

/// # print_str
///
/// hash: print_str_5d3b848661c3d55c
#[no_mangle]
pub extern "C" fn print_str_5d3b848661c3d55c(x: *const i8) -> *const i8 {
    let c_str = unsafe { CStr::from_ptr(x) };
    let str_slice: &str = c_str.to_str().unwrap();
    println!("{str_slice}");

    let out_str = CString::new(format!("Hello {} from Rust!", str_slice)).unwrap();
    out_str.into_raw()
}

/// # print_array
///
/// hash: print_array_a7ba925ba0eeb37c
#[no_mangle]
pub extern "C" fn print_array_a7ba925ba0eeb37c(x: *const i64, len: i64) -> i64 {
    let slice = unsafe { std::slice::from_raw_parts(x, len as usize) };
    println!("{slice:?}");

    0
}

/// # to_str
///
/// hash: to_str_e3bf0802549edfd5
#[no_mangle]
pub extern "C" fn to_str_e3bf0802549edfd5(x: i64) -> *const i8 {
    let out_str = CString::new(format!("{x}")).unwrap();
    out_str.into_raw()
}

/// # concat_str
///
/// hash: concat_str_6c4d78740014a8f5
#[no_mangle]
pub extern "C" fn concat_str_6c4d78740014a8f5(x: *const i8, y: *const i8) -> *const i8 {
    let x_str = unsafe { CStr::from_ptr(x) };
    let x_slice: &str = x_str.to_str().unwrap();

    let y_str = unsafe { CStr::from_ptr(y) };
    let y_slice: &str = y_str.to_str().unwrap();

    let out_str = CString::new(format!("{x_slice}{y_slice}")).unwrap();
    out_str.into_raw()
}
