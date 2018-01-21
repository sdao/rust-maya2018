#![allow(dead_code)]
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub use self::root::*;
pub use self::root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::*;

use std::ffi::{CStr, CString};

impl<'a> From<&'a MString> for String {
    fn from(mstring: &MString) -> String {
        unsafe {
            let s_ptr = mstring.asUTF8();
            let s = CStr::from_ptr(s_ptr).to_str().expect("invalid MString from OpenMaya");
            String::from(s)
        }
    }
}
impl<'a> From<&'a str> for MString {
    fn from(string: &str) -> MString {
        let cstring = CString::new(string).unwrap();
        unsafe { MString::new1(cstring.as_ptr()) }
    }
}
