#![allow(non_upper_case_globals, non_camel_case_types, non_snake_case)]

use std;
use native;
use OpenMaya::core::*;

pub trait MPxCommand {
    fn new() -> Box<MPxCommand> where Self: Sized;
    fn syntax() -> MSyntax where Self: Sized {
        MSyntax::new()
    }
    fn doIt(&self, args: &MArgList) -> MStatus;

    unsafe extern fn shim_creator() -> *mut std::os::raw::c_void where Self : Sized {
        let a: Box<MPxCommand> = Self::new();
        let b: Box<Box<MPxCommand>> = Box::new(a); // This Box is temporary so we can get raw ptr.
        let ptr: *mut Box<MPxCommand> = Box::into_raw(b);
        native::ShimCommand::Create(ptr as *mut std::os::raw::c_void,
            &native::ShimCommand_FunctionTable {
                doIt: Some(Self::shim_doIt),
                destruct: Some(Self::shim_destruct)
            })
    }
    unsafe extern fn shim_syntax_creator() -> native::MSyntax where Self : Sized {
        Self::syntax().clone()._native
    }
    unsafe extern fn shim_destruct(ptr: *mut std::os::raw::c_void) where Self: Sized {
        let a: *mut Box<MPxCommand> = ptr as *mut Box<MPxCommand>;
        Box::from_raw(a);
    }
    unsafe extern fn shim_doIt(ptr: *mut std::os::raw::c_void, args: *const native::MArgList)
        -> native::MStatus where Self: Sized
    {
        let cmd = &*(ptr as *mut Box<MPxCommand>);
        let args_copy = native::MArgList::new1(args);
        cmd.doIt(&MArgList::wrap(args_copy)).clone()._native
    }
}
