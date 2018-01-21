#![allow(dead_code)]
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub use self::root::*;
pub use self::root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::*;

use std::ffi::{CStr, CString};

/// Helper for thiscall macro.
/// thiscall_helper!(T, U, V, ...) returns a converter function.
/// This function takes a function
///     unsafe extern "C" fn(*mut This, T, U, V, ...) -> Return
/// and returns a function
///     unsafe extern "C" fn(*mut This, *mut Return, T, U, V, ...)
/// i.e. a function that returns by argument instead of as the actual return
/// value.
#[macro_export]
macro_rules! thiscall_helper {
    ($($T:ident),*) => {{
        use std;
        unsafe fn helper<This, Return, $($T),*>(
            member_func: unsafe extern "C" fn (This, $($T),*) -> Return)
            -> unsafe extern "C" fn (This, *mut Return, $($T),*)
        {
            let helper: Option<unsafe extern "C" fn(This, $($T),*) -> Return>
                    = Some(member_func);
            let func = helper.unwrap();
            let func_ptr = &func as
                    *const unsafe extern "C" fn(This, $($T),*) -> Return;
            let void_ptr = func_ptr as *const std::os::raw::c_void;
            let thiscall_ptr = void_ptr as
                    *const unsafe extern "C" fn(This, *mut Return, $($T),*);

            *thiscall_ptr
        }

        helper
    }}
}

/// Helper that calls a C++ member function returning a struct, working around any MSVC ABI
/// incompatibility.
/// The invocation
///     thiscall!(MyFunction, this, a, b, c, ...)
/// returns the result of MyFunction(this, a, b, c, ...), taking into account
/// MSVC ABI if in that environment; otherwise it calls it normally.
#[macro_export]
macro_rules! thiscall {
    ($member_func:path, $this:expr, $($arg:ident),*) => {{
        use std;
        if cfg!(target_env = "msvc") {
            let helper = thiscall_helper!($($arg),*);
            let thiscall = helper($member_func);
            let mut ret = std::mem::uninitialized();
            thiscall($this, &mut ret, $($arg),*);
            ret
        }
        else {
            $member_func($this, $($arg),*)
        }
    }};
    ($member_func:path, $this:expr) => {
        thiscall!($member_func, $this,)
    };
}

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
