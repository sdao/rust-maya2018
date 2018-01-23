extern crate bindgen;
extern crate cc;
extern crate syn;
#[macro_use] extern crate quote;
extern crate regex;
extern crate rustfmt;

use std::env;
use std::path::PathBuf;
use std::io::prelude::*;
use std::fs::File;

use syn::{ItemImpl, ItemMod, ImplItemMethod, Type};
use regex::Regex;

enum MayaVisitMethodType {
    Constructor,
    Destructor,
    Operator,
    Normal
}
struct MayaVisit {
    tokens: Vec<quote::Tokens>,
    cur_namespace: Vec<syn::Ident>,
    impl_type: syn::Ident,
    impl_fns: Vec<quote::Tokens>,
    impl_traits: Vec<quote::Tokens>,
}
impl MayaVisit {
    pub fn new() -> Self {
        Self {
            tokens: vec![],
            cur_namespace: vec![],
            impl_type: syn::Ident::from("FAKE_IDENT"),
            impl_fns: vec![],
            impl_traits: vec![],
        }
    }
    fn process_type(ident: &syn::Ident) -> bool {
        // Skip MStrings because we'll wrap those in the API as str/String and not expose.
        // Skip MPx classes because we need to implement manual shims for those.
        let ident_name = ident.to_string();
        ident_name.starts_with("M") &&
                ident_name != "MString" &&
                !ident_name.starts_with("MPx")
    }
    fn process_method(ident: &syn::Ident) -> MayaVisitMethodType {
        let constructor_regex = Regex::new("^new[0-9]*$").unwrap();

        let ident_name = ident.to_string();
        if constructor_regex.is_match(&ident_name) {
            MayaVisitMethodType::Constructor
        }
        else if ident_name == "destruct" {
            MayaVisitMethodType::Destructor
        }
        else if ident_name.starts_with("operator_") {
            MayaVisitMethodType::Operator
        }
        else {
            MayaVisitMethodType::Normal
        }
    }
    /// Does the fn_arg represent a *const ident for the given ident?
    fn is_fn_arg_const_ptr(fn_arg: &syn::FnArg, ident: &syn::Ident) -> bool {
        match fn_arg {
            &syn::FnArg::Captured(ref fn_arg) => {
                match &fn_arg.ty {
                    &syn::Type::Ptr(ref ty) => {
                        let is_const = ty.const_token.is_some();
                        let is_ident = match &*ty.elem {
                            &syn::Type::Path(ref elem) => {
                                elem.path.segments.last().unwrap().value().ident == ident
                            },
                            _ => false
                        };
                        is_const && is_ident
                    },
                    _ => false
                }
            },
            _ => false
        }
    }
}
impl<'ast> syn::visit::Visit<'ast> for MayaVisit {
    fn visit_item_mod(&mut self, i: &'ast ItemMod) {
        self.cur_namespace.push(i.ident);
        syn::visit::visit_item_mod(self, i);
        self.cur_namespace.pop();
    }
    fn visit_item_impl(&mut self, i: &'ast ItemImpl) {
        match *i.self_ty {
            Type::Path(ref p) if MayaVisit::process_type(
                    &p.path.segments.last().unwrap().value().ident) =>
            {
                let impl_type = p.path.segments.last().unwrap().value().ident;
                self.impl_type = impl_type;
                self.impl_fns.clear();
                self.impl_traits.clear();
                syn::visit::visit_item_impl(self, i);
            
                let ns = &self.cur_namespace;
                let impl_fns = &self.impl_fns;
                let impl_traits = &self.impl_traits;
                self.tokens.push(quote! {
                    pub struct #impl_type {
                        _native: #( #ns :: )* #impl_type,
                    }
                    impl #impl_type {
                        #( #impl_fns )*
                    }
                    #( #impl_traits )*
                })
            },
            _ => {}
        }
    }
    fn visit_impl_item_method(&mut self, i: &'ast ImplItemMethod) {
        match MayaVisit::process_method(&i.sig.ident) {
            MayaVisitMethodType::Constructor => {
                let impl_type = &self.impl_type;
                let ns = &self.cur_namespace;

                let fn_ident = &i.sig.ident;
                let fn_inputs = &i.sig.decl.inputs;
                if fn_inputs.len() == 0 {
                    // Implement Default trait.
                    self.impl_traits.push(quote! {
                        impl Default for #impl_type {
                            fn default() -> Self {
                                Self { _native: unsafe { #( #ns :: )* #impl_type :: #fn_ident() } }
                            }
                        }
                    });
                }
                else if fn_inputs.len() == 1 &&
                    MayaVisit::is_fn_arg_const_ptr(fn_inputs.first().unwrap().value(), impl_type)
                {
                    // Implement Clone trait.
                    self.impl_traits.push(quote! {
                        impl Clone for #impl_type {
                            fn clone(&self) -> Self {
                                Self { _native: unsafe {
                                    #( #ns :: )* #impl_type :: #fn_ident(&self._native)
                                } }
                            }
                        }
                    });
                }
                else {
                    // Treat like a regular function. XXX
                }
            },
            MayaVisitMethodType::Destructor => {
                let impl_type = self.impl_type;
                self.impl_traits.push(quote! {
                    impl Drop for #impl_type {
                        fn drop(&mut self) {
                            unsafe { self._native.destruct() }
                        }
                    }
                });
            },
            MayaVisitMethodType::Operator => {
                let impl_type = self.impl_type;
                match i.sig.ident.to_string() {
                    _ => self.impl_traits.push(quote! {})
                };
            },
            MayaVisitMethodType::Normal => {
                let name = i.sig.ident;
                self.impl_fns.push(quote! {
                    pub fn #name() { unimplemented!() }
                })
            }
        }
    }
}

fn main() {
    // XXX: Default is Windows-specific.
    let maya = PathBuf::from(match env::var("MAYA_LOCATION") {
        Ok(loc) => String::from(loc.as_str()),
        Err(_) => String::from("C:/Program Files/Autodesk/Maya2018")
    });

    // Link Maya's libraries.
    println!("cargo:rustc-link-lib=dylib=OpenMaya");
    println!("cargo:rustc-link-lib=dylib=Foundation");
    println!("cargo:rustc-link-search=native={}", maya.join("lib").to_str().unwrap());

    // Add shims for implementing virtual classes.
    cc::Build::new()
        .cpp(true)
        .file("shim/shim.cpp")
        .include("shim")
        .include(maya.join("include").to_str().unwrap())
        .compile("shim");

    // The bindgen::Builder is the main entry point
    // to bindgen, and lets you build up options for
    // the resulting bindings.
    let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .header("wrapper.h")
        .clang_arg(format!("-isystem{}", maya.join("include").to_str().unwrap()))
        .clang_arg("-Ishim")
        .clang_arg("-x")
        .clang_arg("c++")
        .clang_arg("-std=c++14")
        .enable_cxx_namespaces()
        .derive_copy(false)
        .rustfmt_bindings(true)
        .generate_comments(true)
        .link("OpenMaya")
        .link("Foundation")
        .opaque_type("std.*") // We don't need C++ stdlib access.
        .whitelist_type("Shim.*")
        .whitelist_type(".*MArgList")
        .whitelist_type(".*MDagPath")
        .whitelist_type(".*MFnBase")
        .whitelist_type(".*MFnDagNode")
        .whitelist_type(".*MFnPlugin")
        .whitelist_type(".*MGlobal")
        .whitelist_type(".*MObject")
        .whitelist_type(".*MSelectionList")
        .whitelist_type(".*MSyntax")
        .constified_enum_module(".*MFn_Type")
        .constified_enum_module(".*MStatus_MStatusCode")
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");

    // Write the high-level Maya API bindings to the $OUT_DIR/maya.rs file.
    let mut visit = MayaVisit::new();
    let file = syn::parse_file(&bindings.to_string()).expect("Unable to parse file");
    syn::visit::visit_file(&mut visit, &file);
    let gen_lines: Vec<String> = visit.tokens.iter().map(|x| x.to_string()).collect();
    let gen = gen_lines.join("\n");
    let mut formatted = Vec::new();
    let (_, filemap, _) = rustfmt::format_input(
            rustfmt::Input::Text(gen),
            &rustfmt::config::Config::default(),
            Some(&mut formatted))
        .expect("Formatter failed!");
    File::create(&out_path.join("maya.rs"))
        .expect("Couldn't create file!")
        .write_all(filemap[0].1.to_string().as_bytes())
        .expect("Couldn't write Maya API!");
}
