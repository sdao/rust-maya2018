#![recursion_limit="128"]

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
use std::collections::{HashMap, HashSet};

use syn::{ItemImpl, ItemMod, ImplItemMethod, ItemType, ItemEnum, ItemStruct, Type};
use regex::Regex;

enum MayaVisitMethodType {
    Constructor,
    Destructor,
    Operator,
    Normal
}
enum MayaVisitPtrType {
    NotPtr,
    ConstPtr,
    MutPtr
}
struct MayaVisitFirstPass {
    pub structs: HashSet<syn::Ident>,
    pub types: HashSet<syn::Ident>
}
impl<'ast> syn::visit::Visit<'ast> for MayaVisitFirstPass {
    fn visit_item_struct(&mut self, i: &'ast ItemStruct) {
        // Skip MStrings because we'll wrap those in the API as str/String and not expose.
        // Skip MPx classes because we need to implement manual shims for those.
        let ident_name = i.ident.to_string();
        if ident_name.starts_with("M") &&
                !ident_name.starts_with("MString") &&
                !ident_name.starts_with("MPx")
        {
            self.structs.insert(i.ident);
        }
    }
    fn visit_item_type(&mut self, i: &'ast ItemType) {
        let ident_name = i.ident.to_string();
        if ident_name.starts_with("M") {
            self.types.insert(i.ident);
        }
    }
    fn visit_item_enum(&mut self, i: &'ast ItemEnum) {
        let ident_name = i.ident.to_string();
        if ident_name.starts_with("M") {
            self.types.insert(i.ident);
        }
    }
}
struct MayaVisitSecondPass {
    first_pass: MayaVisitFirstPass,
    tokens: Vec<quote::Tokens>,
    cur_namespace: Vec<syn::Ident>,
    impl_type: syn::Ident,
    impl_fns: Vec<quote::Tokens>,
    impl_traits: Vec<quote::Tokens>,
    impl_partialeq: HashMap<syn::Ident, Vec<quote::Tokens>>,
}
impl MayaVisitSecondPass {
    pub fn new(first_pass: MayaVisitFirstPass) -> Self {
        Self {
            first_pass: first_pass,
            tokens: vec![],
            cur_namespace: vec![],
            impl_type: syn::Ident::from("FAKE_IDENT"),
            impl_fns: vec![],
            impl_traits: vec![],
            impl_partialeq: HashMap::new()
        }
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
    /// Does the fn_arg represent a *const/mut ns1::ns2::ident for the given ident?
    fn is_ptr(fn_arg: &syn::FnArg, ident: &syn::Ident) -> bool {
        match fn_arg {
            &syn::FnArg::Captured(ref fn_arg) => {
                match MayaVisitSecondPass::quote_ty(&fn_arg.ty) {
                    Some((ty_ident, is_ptr)) => ident == &ty_ident && match is_ptr {
                        MayaVisitPtrType::NotPtr => false,
                        _ => true
                    },
                    None => false
                }
            },
            _ => false
        }
    }
    /// Convert type from bindgen to high-level API.
    /// Returns: Option((ident, is_ptr)) where ident is the new type token; is_ptr is
    /// whether the type is a *const/mut ident.
    fn quote_ty(ty: &syn::Type) -> Option<(syn::Ident, MayaVisitPtrType)> {
        match ty {
            &syn::Type::Path(ref ty) => {
                let name = ty.path.segments.last().unwrap().value().ident;
                match name.to_string().as_str() {
                    "c_char" => Some((syn::Ident::from("i8"), MayaVisitPtrType::NotPtr)),
                    "c_double" => Some((syn::Ident::from("f64"), MayaVisitPtrType::NotPtr)),
                    "c_float" => Some((syn::Ident::from("f32"), MayaVisitPtrType::NotPtr)),
                    "c_int" => Some((syn::Ident::from("i32"), MayaVisitPtrType::NotPtr)),
                    "c_long" => Some((syn::Ident::from("i64"), MayaVisitPtrType::NotPtr)),
                    "c_longlong" => Some((syn::Ident::from("i64"), MayaVisitPtrType::NotPtr)),
                    "c_schar" => Some((syn::Ident::from("i8"), MayaVisitPtrType::NotPtr)),
                    "c_short" => Some((syn::Ident::from("i16"), MayaVisitPtrType::NotPtr)),
                    "c_uchar" => Some((syn::Ident::from("u8"), MayaVisitPtrType::NotPtr)),
                    "c_uint" => Some((syn::Ident::from("u32"), MayaVisitPtrType::NotPtr)),
                    "c_ulong" => Some((syn::Ident::from("u64"), MayaVisitPtrType::NotPtr)),
                    "c_ulonglong" => Some((syn::Ident::from("u64"), MayaVisitPtrType::NotPtr)),
                    "c_ushort" => Some((syn::Ident::from("u16"), MayaVisitPtrType::NotPtr)),
                    _ => Some((name, MayaVisitPtrType::NotPtr))
                }
            },
            &syn::Type::Ptr(ref ty) => {
                let inner_ty = MayaVisitSecondPass::quote_ty(&ty.elem);
                match inner_ty {
                    Some((tokens, _)) => {
                        let is_const = ty.const_token.is_some();
                        let ptr_kind = if is_const {
                            MayaVisitPtrType::ConstPtr
                        }
                        else {
                            MayaVisitPtrType::MutPtr
                        };
                        Some((tokens, ptr_kind))
                    },
                    _ => None
                }
            },
            _ => None
        }
    }
    fn quote_binary_operator(&self, i: &ImplItemMethod, trait_name: &syn::Ident,
        fn_name: &syn::Ident) -> quote::Tokens
    {
        let impl_type = self.impl_type;
        let fn_ident = &i.sig.ident;
        let fn_inputs = &i.sig.decl.inputs;
        if let &syn::FnArg::Captured(ref fn_arg) = *fn_inputs.last().unwrap().value() {
            if let Some((rhs_ident, rhs_is_ptr)) =
                MayaVisitSecondPass::quote_ty(&fn_arg.ty)
            {
                if let &syn::ReturnType::Type(_, ref ret_ty) = &i.sig.decl.output {
                    if let Some((ret_ident, _)) =
                        MayaVisitSecondPass::quote_ty(&ret_ty)
                    {
                        let invoke_byval = match rhs_is_ptr {
                            MayaVisitPtrType::NotPtr => quote! {
                                unsafe { self_._native.#fn_ident(other) }
                            },
                            MayaVisitPtrType::ConstPtr => quote! {
                                unsafe { self_._native.#fn_ident(&other._native) }
                            },
                            MayaVisitPtrType::MutPtr => quote! {
                                unsafe { self_._native.#fn_ident(&mut other._native) }
                            },
                        };
                        let ret_byval = if self.first_pass.structs.contains(&ret_ident) {
                            quote! { #ret_ident { _native: #invoke_byval } }
                        }
                        else {
                            quote! { #invoke_byval }
                        };
                        let invoke_byref = match rhs_is_ptr {
                            MayaVisitPtrType::NotPtr => quote! {
                                unsafe { self_._native.#fn_ident(*other) }
                            },
                            MayaVisitPtrType::ConstPtr => quote! {
                                unsafe { self_._native.#fn_ident(&other._native) }
                            },
                            MayaVisitPtrType::MutPtr => quote! {
                                unsafe { self_._native.#fn_ident(&mut other._native) }
                            },
                        };
                        let ret_byref = if self.first_pass.structs.contains(&ret_ident) {
                            quote! { #ret_ident::wrap(#invoke_byref) }
                        }
                        else {
                            quote! { #invoke_byref }
                        };
                        // Because the Maya API is weird enough as-is...
                        // If the lhs or rhs of the argument is non-const, clone it before using.
                        let self_mut_fix = match *fn_inputs.first().unwrap().value() {
                            &syn::FnArg::SelfRef(ref self_arg) if self_arg.mutability.is_some() => {
                                quote! { let mut self_  = self.clone(); }
                            },
                            _ => quote! {
                                let self_ = self;
                            }
                        };
                        let other_mut_fix = match rhs_is_ptr {
                            MayaVisitPtrType::MutPtr => quote! {
                                let mut other = other.clone();
                            },
                            _ => quote! {}
                        };
                        // Since we're just auto-generating everything, generate all op traits
                        // for ref/non-ref combos.
                        return quote! {
                            impl ::std::ops::#trait_name<#rhs_ident> for #impl_type {
                                type Output = #ret_ident;
                                fn #fn_name(self, other: #rhs_ident) -> #ret_ident {
                                    #self_mut_fix
                                    #other_mut_fix
                                    #ret_byval
                                }
                            }
                            impl<'a> ::std::ops::#trait_name<#rhs_ident> for &'a #impl_type {
                                type Output = #ret_ident;
                                fn #fn_name(self, other: #rhs_ident) -> #ret_ident {
                                    #self_mut_fix
                                    #other_mut_fix
                                    #ret_byval
                                }
                            }
                            impl<'b> ::std::ops::#trait_name<&'b #rhs_ident> for #impl_type {
                                type Output = #ret_ident;
                                fn #fn_name(self, other: &'b #rhs_ident) -> #ret_ident {
                                    #self_mut_fix
                                    #other_mut_fix
                                    #ret_byref
                                }
                            }
                            impl<'a, 'b> ::std::ops::#trait_name<&'b #rhs_ident>
                                for &'a #impl_type
                            {
                                type Output = #ret_ident;
                                fn #fn_name(self, other: &'b #rhs_ident) -> #ret_ident {
                                    #self_mut_fix
                                    #other_mut_fix
                                    #ret_byref
                                }
                            }
                        };
                    }
                }
            }
        }

        return quote! {};
    }
    fn quote_index_operator(&self, i: &ImplItemMethod) -> quote::Tokens
    {
        let impl_type = self.impl_type;
        let fn_ident = &i.sig.ident;
        let fn_inputs = &i.sig.decl.inputs;
        if let &syn::FnArg::Captured(ref fn_arg) = *fn_inputs.last().unwrap().value() {
            if let Some((rhs_ident, rhs_is_ptr)) =
                MayaVisitSecondPass::quote_ty(&fn_arg.ty)
            {
                if let &syn::ReturnType::Type(_, ref ret_ty) = &i.sig.decl.output {
                    if let Some((ret_ident, ret_is_ptr)) =
                        MayaVisitSecondPass::quote_ty(&ret_ty)
                    {
                        let invoke_byval = match rhs_is_ptr {
                            MayaVisitPtrType::NotPtr => quote! {
                                self_._native.#fn_ident(other)
                            },
                            MayaVisitPtrType::ConstPtr => quote! {
                                self_._native.#fn_ident(&other._native)
                            },
                            MayaVisitPtrType::MutPtr => quote! {
                                self_._native.#fn_ident(&mut other._native)
                            },
                        };
                        let ret_byval = if self.first_pass.structs.contains(&ret_ident) {
                            quote! { #ret_ident::wrap(#invoke_byval) }
                        }
                        else {
                            quote! { #invoke_byval }
                        };
                        let invoke_byref = match rhs_is_ptr {
                            MayaVisitPtrType::NotPtr => quote! {
                                self_._native.#fn_ident(*other)
                            },
                            MayaVisitPtrType::ConstPtr => quote! {
                                self_._native.#fn_ident(&other._native)
                            },
                            MayaVisitPtrType::MutPtr => quote! {
                                self_._native.#fn_ident(&mut other._native)
                            },
                        };
                        let ret_byref = if self.first_pass.structs.contains(&ret_ident) {
                            quote! { #ret_ident::wrap(#invoke_byref) }
                        }
                        else {
                            quote! { #invoke_byref }
                        };
                        // Because the Maya API is weird enough as-is...
                        // If the lhs or rhs of the argument is non-const, clone it before using.
                        let self_mut_fix = match *fn_inputs.first().unwrap().value() {
                            &syn::FnArg::SelfRef(ref self_arg) if self_arg.mutability.is_some() => {
                                quote! { let mut self_  = self.clone(); }
                            },
                            _ => quote! {
                                let self_ = self;
                            }
                        };
                        let other_mut_fix = match rhs_is_ptr {
                            MayaVisitPtrType::MutPtr => quote! {
                                let mut other = other.clone();
                            },
                            _ => quote! {}
                        };
                        // Since we're just auto-generating everything, generate op traits for
                        // both ref and non-ref index types.
                        return match ret_is_ptr {
                            MayaVisitPtrType::NotPtr => quote! {
                                impl ::std::ops::Rem<#rhs_ident> for #impl_type {
                                    type Output = #ret_ident;
                                    fn rem(self, other: #rhs_ident) -> #ret_ident {
                                        #self_mut_fix
                                        #other_mut_fix
                                        unsafe { #ret_byval }
                                    }
                                }
                                impl<'a> ::std::ops::Rem<#rhs_ident> for &'a #impl_type {
                                    type Output = #ret_ident;
                                    fn rem(self, other: #rhs_ident) -> #ret_ident {
                                        #self_mut_fix
                                        #other_mut_fix
                                        unsafe { #ret_byval }
                                    }
                                }
                                impl<'b> ::std::ops::Rem<&'b #rhs_ident> for #impl_type {
                                    type Output = #ret_ident;
                                    fn rem(self, other: &'b #rhs_ident) -> #ret_ident {
                                        #self_mut_fix
                                        #other_mut_fix
                                        unsafe { #ret_byref }
                                    }
                                }
                                impl<'a, 'b> ::std::ops::Rem<&'b #rhs_ident> for &'a #impl_type {
                                    type Output = #ret_ident;
                                    fn rem(self, other: &'b #rhs_ident) -> #ret_ident {
                                        #self_mut_fix
                                        #other_mut_fix
                                        unsafe { #ret_byref }
                                    }
                                }
                            },
                            _ => quote! {
                                impl ::std::ops::Index<#rhs_ident> for #impl_type {
                                    type Output = #ret_ident;
                                    fn index(&self, other: #rhs_ident) -> &#ret_ident {
                                        #self_mut_fix
                                        #other_mut_fix
                                        unsafe { &*#ret_byval }
                                    }
                                }
                                impl<'b> ::std::ops::Index<&'b #rhs_ident> for #impl_type {
                                    type Output = #ret_ident;
                                    fn index(&self, other: &'b #rhs_ident) -> &#ret_ident {
                                        #self_mut_fix
                                        #other_mut_fix
                                        unsafe { &*#ret_byref }
                                    }
                                }
                            }
                        };
                    }
                }
            }
        }

        return quote! {};
    }
}
impl<'ast> syn::visit::Visit<'ast> for MayaVisitSecondPass {
    fn visit_item_mod(&mut self, i: &'ast ItemMod) {
        self.cur_namespace.push(i.ident);
        syn::visit::visit_item_mod(self, i);
        self.cur_namespace.pop();
    }
    fn visit_item_struct(&mut self, i: &'ast ItemStruct) {
        let struct_type = &i.ident;
        if self.first_pass.structs.contains(struct_type) {
            let ns = &self.cur_namespace;
            self.tokens.push(quote! {
                pub struct #struct_type {
                    _native: #( #ns :: )* #struct_type,
                }
            });
        }
    }
    fn visit_item_impl(&mut self, i: &'ast ItemImpl) {
        match *i.self_ty {
            Type::Path(ref p) if self.first_pass.structs.contains(
                    &p.path.segments.last().unwrap().value().ident) =>
            {
                let impl_type = p.path.segments.last().unwrap().value().ident;
                self.impl_type = impl_type;
                self.impl_fns.clear();
                self.impl_traits.clear();
                self.impl_partialeq.clear();
                syn::visit::visit_item_impl(self, i);

                let ns = &self.cur_namespace;
                let impl_fns = &self.impl_fns;
                let impl_traits = &self.impl_traits;
                let impl_partialeq = if self.impl_partialeq.is_empty() {
                    vec![]
                }
                else {
                    let mut partialeqs = vec![];
                    for (ident, tokens) in &self.impl_partialeq {
                        partialeqs.push(quote! {
                            impl PartialEq<#ident> for #impl_type {
                                #( #tokens )*
                            }
                        })
                    }
                    partialeqs
                };
                self.tokens.push(quote! {
                    impl #impl_type {
                        pub fn wrap(native: #( #ns :: )* #impl_type) -> Self {
                            Self { _native: native }
                        }
                        #( #impl_fns )*
                    }
                    #( #impl_traits )*
                    #( #impl_partialeq )*
                })
            },
            _ => {}
        }
    }
    fn visit_impl_item_method(&mut self, i: &'ast ImplItemMethod) {
        match MayaVisitSecondPass::process_method(&i.sig.ident) {
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
                                Self::wrap(unsafe { #( #ns :: )* #impl_type :: #fn_ident() })
                            }
                        }
                    });
                }
                else if fn_inputs.len() == 1 &&
                    MayaVisitSecondPass::is_ptr(fn_inputs.first().unwrap().value(), impl_type)
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
                let fn_ident = &i.sig.ident;
                let fn_inputs = &i.sig.decl.inputs;

                // == and != are special because they belong to PartialEq.
                if fn_ident.to_string().starts_with("operator_eq") {
                    if let &syn::FnArg::Captured(ref fn_arg) = *fn_inputs.last().unwrap().value() {
                        if let Some((rhs_ident, rhs_is_ptr)) =
                            MayaVisitSecondPass::quote_ty(&fn_arg.ty)
                        {
                            let rhs_access = match rhs_is_ptr {
                                MayaVisitPtrType::NotPtr => quote! { *other },
                                _ => quote! { &other._native }
                            };
                            let v = self.impl_partialeq.entry(rhs_ident).or_insert(vec![]);
                            v.push(quote! {
                                fn eq(&self, other: &#rhs_ident) -> bool {
                                    unsafe {
                                        self._native.#fn_ident(#rhs_access)
                                    }
                                }
                            });
                        }
                    }
                }
                if fn_ident.to_string().starts_with("operator_neq") {
                    if let &syn::FnArg::Captured(ref fn_arg) = *fn_inputs.last().unwrap().value() {
                        if let Some((rhs_ident, rhs_is_ptr)) =
                            MayaVisitSecondPass::quote_ty(&fn_arg.ty)
                        {
                            let rhs_access = match rhs_is_ptr {
                                MayaVisitPtrType::NotPtr => quote! { *other },
                                _ => quote! { &other._native }
                            };
                            let v = self.impl_partialeq.entry(rhs_ident).or_insert(vec![]);
                            v.push(quote! {
                                fn ne(&self, other: &#rhs_ident) -> bool {
                                    unsafe {
                                        self._native.#fn_ident(#rhs_access)
                                    }
                                }
                            });
                        }
                    }
                }
                // ! is a special unary operator.
                else if fn_ident.to_string().starts_with("operator_not") {
                    self.impl_traits.push(quote! {
                        impl ::std::ops::Not for #impl_type {
                            type Output = bool;
                            fn not(self) -> bool {
                                unsafe { self._native.#fn_ident() }
                            }
                        }
                        impl<'a> ::std::ops::Not for &'a #impl_type {
                            type Output = bool;
                            fn not(self) -> bool {
                                unsafe { self._native.#fn_ident() }
                            }
                        }
                    })
                }
                // [] is a special sorta-binary operator.
                else if fn_ident.to_string().starts_with("operator_index") {
                    let q = self.quote_index_operator(i);
                    self.impl_traits.push(q);
                }
                // Remaining operators (binary).
                else if fn_ident.to_string().starts_with("operator_add") {
                    let q = self.quote_binary_operator(
                            i, &syn::Ident::from("Add"), &syn::Ident::from("add"));
                    self.impl_traits.push(q);
                }
                else if fn_ident.to_string().starts_with("operator_sub") {
                    let q = self.quote_binary_operator(
                            i, &syn::Ident::from("Sub"), &syn::Ident::from("sub"));
                    self.impl_traits.push(q);
                }
                else if fn_ident.to_string().starts_with("operator_mul") {
                    let q = self.quote_binary_operator(
                            i, &syn::Ident::from("Mul"), &syn::Ident::from("mul"));
                    self.impl_traits.push(q);
                }
                else if fn_ident.to_string().starts_with("operator_div") {
                    let q = self.quote_binary_operator(
                            i, &syn::Ident::from("Div"), &syn::Ident::from("div"));
                    self.impl_traits.push(q);
                }
                else if fn_ident.to_string().starts_with("operator_or") {
                    let q = self.quote_binary_operator(
                            i, &syn::Ident::from("BitOr"), &syn::Ident::from("bitor"));
                    self.impl_traits.push(q);
                }
                else if fn_ident.to_string().starts_with("operator_xor") {
                    let q = self.quote_binary_operator(
                            i, &syn::Ident::from("BitXor"), &syn::Ident::from("bitxor"));
                    self.impl_traits.push(q);
                }
            },
            MayaVisitMethodType::Normal => {
                let name = i.sig.ident;
                self.impl_fns.push(quote! {
                    pub fn #name() { unimplemented!() }
                })
            }
        }
    }
    fn visit_item_type(&mut self, i: &'ast ItemType) {
        let ident = &i.ident;
        let ns = &self.cur_namespace;

        // Verbatim typedef.
        if self.first_pass.types.contains(ident) {
            self.tokens.push(quote! {
                pub type #ident = #( #ns :: )* #ident;
            });
        }
    }
    fn visit_item_enum(&mut self, i: &'ast ItemEnum) {
        let ident = &i.ident;
        let ns = &self.cur_namespace;
        
        // Typedef the actual enum.
        if self.first_pass.types.contains(ident) {
            self.tokens.push(quote! {
                pub type #ident = #( #ns :: )* #ident;
            });
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
        .whitelist_type(".*MVector")
        .rustified_enum(".*MFn_Type")
        .rustified_enum(".*MStatus_MStatusCode")
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
    let file = syn::parse_file(&bindings.to_string()).expect("Unable to parse file");
    let mut first_pass = MayaVisitFirstPass { structs: HashSet::new(), types: HashSet::new() };
    syn::visit::visit_file(&mut first_pass, &file);
    let mut second_pass = MayaVisitSecondPass::new(first_pass);
    syn::visit::visit_file(&mut second_pass, &file);
    let gen_lines: Vec<String> = second_pass.tokens.iter().map(|x| x.to_string()).collect();
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
