#![allow(non_upper_case_globals, non_camel_case_types, non_snake_case)]

use std;
use std::ffi::CString;
use native;
use OpenMaya::core::*;
use OpenMaya::mpx::*;

pub struct MFnPlugin {
    _native: native::MFnPlugin
}
impl MFnPlugin {
    pub fn new(obj: &mut MObject, vendor: &str, version: &str, api: &str) ->
        Result<MFnPlugin, MStatus>
    {
        let mut status = MStatus::new();
        let cvendor = CString::new(vendor).unwrap();
        let cversion = CString::new(version).unwrap();
        let capi = CString::new(api).unwrap();
        let plugin = unsafe { native::MFnPlugin::new1(
                &mut obj._native, cvendor.as_ptr(), cversion.as_ptr(), capi.as_ptr(),
                &mut status._native) };
        check_mstatus!(MFnPlugin { _native: plugin }, status)
    }
    pub fn registerCommand<T>(&mut self, name: &str) -> Result<(), MStatus>
        where T: MPxCommand + Sized
    {
        let name_mstring = &native::MString::from(name);
        let creator: Option<unsafe extern fn() -> *mut std::os::raw::c_void> = Some(T::shim_creator);
        let syntax_creator: Option<unsafe extern fn() -> native::MSyntax> = Some(T::shim_syntax_creator);
        let native_status = unsafe { thiscall!(native::MFnPlugin_registerCommand,
                &mut self._native, name_mstring, creator, syntax_creator) };
        let status = MStatus::wrap(native_status);
        check_mstatus!((), status)
    }
    pub fn deregisterCommand(&mut self, name: &str) -> Result<(), MStatus> {
        let name_mstring = &native::MString::from(name);
        let native_status = unsafe { thiscall!(native::MFnPlugin_deregisterCommand,
                &mut self._native, name_mstring) };
        let status = MStatus::wrap(native_status);
        check_mstatus!((), status)
    }
}
