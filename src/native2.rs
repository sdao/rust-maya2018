#![allow(non_snake_case, non_camel_case_types, non_upper_case_globals)]
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use std::ffi::{CStr, CString};
use std::os::raw::c_char;
unsafe trait UnsafeFrom<FromType> {
    fn unsafe_from(original: FromType) -> Self where Self: Sized;
}
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
unsafe impl<'a> UnsafeFrom<&'a root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString>
    for String
{
    fn unsafe_from(string: &'a root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString)
        -> String
    {
        String::from(string)
    }
}
unsafe impl UnsafeFrom<*const root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString>
    for String
{
    fn unsafe_from(string: *const root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString)
        -> String
    {
        String::from(&*string)
    }
}
unsafe impl UnsafeFrom<*mut root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString>
    for String
{
    fn unsafe_from(string: *mut root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString)
        -> String
    {
        String::from(&*string)
    }
}
unsafe impl<'a> UnsafeFrom<&'a str>
    for root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString
{
    fn unsafe_from(string: &str) -> root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString {
        root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString::from(string)
    }
}
unsafe impl UnsafeFrom<*const c_char> for String {
    fn unsafe_from(string: *const c_char)  -> String {
        unsafe {
            let s = CStr::from_ptr(string).to_str().expect("invalid const char* from OpenMaya");
            String::from(s)
        }
    }
}
unsafe impl UnsafeFrom<*mut c_char> for String {
    fn unsafe_from(string: *mut c_char)  -> String {
        unsafe {
            let s = CStr::from_ptr(string).to_str().expect("invalid const char* from OpenMaya");
            String::from(s)
        }
    }
}

use std::borrow::Borrow;
/// Used for implementing overloaded Get::get via generics.
pub trait Getter<T> {
    type Output;
    fn get(self, arr: &T) -> Self::Output;
}
/// Used for implementing overloaded Set::set via generics.
pub trait Setter<'i, T> {
    type Input: ?Sized;
    fn set(self, arr: &mut T, input: &Self::Input);
}
/// Trait for exposing get-indexing on Maya native arrays.
/// Maya arrays and array-like objects such as plugs implement this trait; use get() to index
/// into the array and retrieve the value at the given index in the same way that you would use
/// the [] index operator in C++. This is overloaded if the corresponding C++ implementation is
/// overloaded.
/// Due to limitations with index operator overloading in Rust, the [] operator isn't directly
/// supported, and you need to use get() instead.
pub trait Get {
    fn get<T>(&self, index: T) -> T::Output where T: Getter<Self>, Self: Sized {
        index.get(self)
    }
}
/// Trait for exposing set-indexing on Maya native arrays.
/// Maya arrays and array-like objects such as plugs implement this trait; use set() to place a
/// value at an index in the array in the same way that you would assign using the [] index operator
/// in C++. Note that you can provide a reference or a value to be placed; they will be converted
/// automatically depending on the C++ function signature. This is overloaded if the corresponding
/// C++ implementation is overloaded.
/// Due to limitations with index operator overloading in Rust, the [] operator isn't directly
/// supported, and you need to use set() instead.
pub trait Set {
    fn set<'i, T, Input: Borrow<T::Input>>(&mut self, index: T, value: Input)
        where T: Setter<'i, Self>, Self: Sized
    {
        index.set(self, value.borrow())
    }
}

include!(concat!(env!("OUT_DIR"), "/maya.rs"));