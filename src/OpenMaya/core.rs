#![allow(non_upper_case_globals, non_camel_case_types, non_snake_case)]

use native;
use std::ffi::CStr;

pub mod MFn {
    pub use native::MFn_Type::*;
}
pub mod MStatusCode {
    pub use native::MStatus_MStatusCode::*;
}

pub struct MObject {
    _native: native::MObject
}
impl MObject {
    pub fn wrap(n: native::MObject) -> Self {
        Self { _native: n }
    }
    pub fn new() -> Self {
        Self { _native: unsafe { native::MObject::new() } }
    }
    pub fn hasFn(&self, fs: MFn::Type) -> bool {
        unsafe { self._native.hasFn(fs) }
    }
    pub fn isNull(&self) -> bool {
        unsafe { self._native.isNull() }
    }
    pub fn apiType(&self) -> MFn::Type {
        unsafe { self._native.apiType() }
    }
    pub fn apiTypeStr(&self) -> &str {
        unsafe {
            let s_ptr = self._native.apiTypeStr();
            CStr::from_ptr(s_ptr).to_str().expect("invalid apiTypeStr from OpenMaya")
        }
    }
}
impl PartialEq for MObject {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self._native.operator_eq(&other._native) }
    }
}
impl Clone for MObject {
    fn clone(&self) -> Self {
        Self { _native: unsafe { native::MObject::new1(&self._native) } }
    }
}
impl Drop for MObject {
    fn drop(&mut self) {
        unsafe { self._native.destruct(); }
    }
}

pub struct MStatus {
    _native: native::MStatus
}
impl MStatus {
    pub fn wrap(n: native::MStatus) -> Self {
        Self { _native: n }
    }
    pub fn new() -> Self {
        Self { _native: unsafe { native::MStatus::new() } }
    }
    pub fn new_code(code: MStatusCode::Type) -> Self {
        Self { _native: unsafe { native::MStatus::new1(code) } }
    }
    pub fn error(&self) -> bool {
        unsafe { self._native.error() }
    }
    pub fn clear(&mut self) {
        unsafe { self._native.clear() }
    }
    pub fn statusCode(&self) -> MStatusCode::Type {
        unsafe { self._native.statusCode() }
    }
    pub fn errorString(&self) -> String {
        unsafe { native::mstring_to_string(&self._native.errorString()) }
    }
    pub fn perror1(&self, s: &str) {
        let mstring = native::str_to_mstring(s);
        unsafe { self._native.perror1(&mstring) }
    }
    pub fn as_native(&self) -> native::MStatus {
        self.clone()._native
    }
}
impl PartialEq for MStatus {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self._native.operator_eq(&other._native) }
    }
}
impl Clone for MStatus {
    fn clone(&self) -> Self {
        Self { _native: unsafe { native::MStatus::new2(&self._native) } }
    }
}
macro_rules! check_mstatus {
    ($ret:expr, $status:expr) => {{
        match $status.error() {
            true => Err($status),
            false => Ok($ret)
        }
    }};
}

pub struct MDagPath {
    _native: native::MDagPath
}
impl MDagPath {
    pub fn wrap(n: native::MDagPath) -> Self {
        Self { _native: n }
    }
    pub fn new() -> Self {
        Self { _native: unsafe { native::MDagPath::new() }}
    }
    // XXX getAllPathsBelow
    pub fn hasFn(&self, apiType: MFn::Type) -> Result<bool, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let hasFn = self._native.hasFn(apiType, &mut status._native);
            check_mstatus!(hasFn, status)
        }
    }
    pub fn apiType(&self) -> Result<MFn::Type, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let apiType = self._native.apiType(&mut status._native);
            check_mstatus!(apiType, status)
        }
    }
    pub fn isValid(&self) -> Result<bool, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let isValid = self._native.isValid(&mut status._native);
            check_mstatus!(isValid, status)
        }
    }
    pub fn node(&self) -> Result<MObject, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let obj = {
                let status_ptr = &mut status._native;
                thiscall!(native::MDagPath_node, &self._native, status_ptr)
            };
            check_mstatus!(MObject::wrap(obj), status)
        }
    }
    pub fn transform(&self) -> Result<MObject, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let obj = {
                let status_ptr = &mut status._native;
                thiscall!(native::MDagPath_transform, &self._native, status_ptr)
            };
            check_mstatus!(MObject::wrap(obj), status)
        }
    }
    pub fn length(&self) -> Result<u32, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let length = self._native.length(&mut status._native);
            check_mstatus!(length, status)
        }
    }
    pub fn extendToShape(&mut self) -> Result<(), MStatus> {
        unsafe {
            let native_status = thiscall!(native::MDagPath_extendToShape,
                    &mut self._native,);
            let status = MStatus::wrap(native_status);
            check_mstatus!((), status)
        }
    }
    pub fn extendToShapeDirectlyBelow(&mut self, i: u32) -> Result<(), MStatus> {
        unsafe {
            let native_status = thiscall!(native::MDagPath_extendToShapeDirectlyBelow,
                    &mut self._native,
                    i);
            let status = MStatus::wrap(native_status);
            check_mstatus!((), status)
        }
    }
    pub fn numberOfShapesDirectlyBelow(&self) -> Result<u32, MStatus> {
        let mut num = 0u32;
        unsafe {
            let native_status = {
                let num_ptr = &mut num;
                thiscall!(native::MDagPath_numberOfShapesDirectlyBelow,
                        &self._native,
                        num_ptr)
            };
            let status = MStatus::wrap(native_status);
            check_mstatus!(num, status)
        }
    }
    pub fn push(&mut self, obj: &MObject) -> Result<(), MStatus> {
        unsafe {
            let obj_ptr = &obj._native;
            let native_status = thiscall!(native::MDagPath_push,
                    &mut self._native,
                    obj_ptr);
            let status = MStatus::wrap(native_status);
            check_mstatus!((), status)
        }
    }
    pub fn pop(&mut self, num: u32) -> Result<(), MStatus> {
        unsafe {
            let native_status = thiscall!(native::MDagPath_pop,
                    &mut self._native,
                    num);
            let status = MStatus::wrap(native_status);
            check_mstatus!((), status)
        }
    }
    pub fn childCount(&mut self) -> Result<u32, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let childCount = self._native.childCount(&mut status._native);
            check_mstatus!(childCount, status)
        }
    }
    pub fn child(&self, i: u32) -> Result<MObject, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let native_obj = {
                let status_ptr = &mut status._native;
                thiscall!(native::MDagPath_child,
                        &self._native,
                        i,
                        status_ptr)
            };
            check_mstatus!(MObject::wrap(native_obj), status)
        }
    }
    // XXX inclusiveMatrix
    // XXX exclusiveMatrix
    // XXX inclusiveMatrixInverse
    // XXX exclusiveMatrixInverse
    pub fn set(&mut self, src: &MDagPath) -> Result<(), MStatus> {
        unsafe {
            let src_ptr = &src._native;
            let native_status = thiscall!(native::MDagPath_set,
                    &mut self._native,
                    src_ptr);
            let status = MStatus::wrap(native_status);
            check_mstatus!((), status)
        }
    }
    pub fn pathCount(&self) -> Result<u32, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let pathCount = self._native.pathCount(&mut status._native);
            check_mstatus!(pathCount, status)
        }
    }
    pub fn getPath(&self, i: u32) -> Result<MDagPath, MStatus> {
        let mut path = MDagPath::new();
        unsafe {
            let native_status = {
                let path_ptr = &mut path._native;
                thiscall!(native::MDagPath_getPath,
                        &self._native,
                        path_ptr,
                        i)
            };
            let status = MStatus::wrap(native_status);
            check_mstatus!(path, status)
        }
    }
    pub fn fullPathName(&self) -> Result<String, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let mstring = {
                let status_ptr = &mut status._native;
                thiscall!(native::MDagPath_fullPathName,
                        &self._native,
                        status_ptr)
            };
            let name = native::mstring_to_string(&mstring);
            check_mstatus!(name, status)
        }
    }
    pub fn partialPathName(&self) -> Result<String, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let mstring = {
                let status_ptr = &mut status._native;
                thiscall!(native::MDagPath_partialPathName,
                        &self._native,
                        status_ptr)
            };
            let name = native::mstring_to_string(&mstring);
            check_mstatus!(name, status)
        }
    }
    pub fn isInstanced(&self) -> Result<bool, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let isInstanced = self._native.isInstanced(&mut status._native);
            check_mstatus!(isInstanced, status)
        }
    }
    pub fn instanceNumber(&self) -> Result<u32, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let instanceNumber = self._native.instanceNumber(&mut status._native);
            check_mstatus!(instanceNumber, status)
        }
    }
    pub fn isVisible(&self) -> Result<bool, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let isVisible = self._native.isVisible(&mut status._native);
            check_mstatus!(isVisible, status)
        }
    }
    pub fn isTemplated(&self) -> Result<bool, MStatus> {
        let mut status = MStatus::new();
        unsafe {
            let isTemplated = self._native.isTemplated(&mut status._native);
            check_mstatus!(isTemplated, status)
        }
    }
    // XXX getDrawOverrideInfo
}
impl PartialEq for MDagPath {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self._native.operator_eq(&other._native) }
    }
}
impl Clone for MDagPath {
    fn clone(&self) -> Self {
        Self { _native: unsafe { native::MDagPath::new1(&self._native) }}
    }
}
impl Drop for MDagPath {
    fn drop(&mut self) {
        unsafe { native::MDagPath_MDagPath_destructor(&mut self._native); }
    }
}

mod MGlobal {

}
