use nucleo::Nucleo;

#[no_mangle]
pub extern "C" fn hello() {
    println!("Hello from Rust!");
}
