#![allow(non_snake_case, non_camel_case_types, non_upper_case_globals)]
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use std::ffi::{CStr, CString};
impl<'a> From<&'a root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString> for String {
    fn from(mstring: &root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString) -> String {
        unsafe {
            let s_ptr = mstring.asUTF8();
            let s = CStr::from_ptr(s_ptr).to_str().expect("invalid MString from OpenMaya");
            String::from(s)
        }
    }
}
impl<'a> From<&'a str> for root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString {
    fn from(string: &str) -> root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString {
        let cstring = CString::new(string).unwrap();
        unsafe {
            root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString::new1(cstring.as_ptr())
        }
    }
}

use std::borrow::Borrow;
pub trait Getter<T> {
    type Output;
    fn get(self, arr: &T) -> Self::Output;
}
pub trait Setter<'i, T> {
    type Input;
    fn set(self, arr: &mut T, input: &Self::Input);
}
pub trait Get {
    fn get<T>(&self, index: T) -> T::Output where T: Getter<Self>, Self: Sized {
        index.get(self)
    }
}
pub trait Set {
    fn set<'i, T, Input: Borrow<T::Input>>(&mut self, index: T, value: Input)
        where T: Setter<'i, Self>, Self: Sized
    {
        index.set(self, value.borrow())
    }
}

include!(concat!(env!("OUT_DIR"), "/maya.rs"));