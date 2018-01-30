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
use std::collections::{BTreeMap, HashSet};

use syn::{ItemImpl, ItemMod, ImplItemMethod, ItemType, ItemEnum, ItemStruct, Type};
use quote::ToTokens;
use regex::Regex;

#[derive(PartialEq)]
enum MayaVisitMethodType {
    Constructor,
    Destructor,
    Operator,
    Normal
}
#[derive(PartialEq)]
enum MayaVisitPtrType {
    NotPtr,
    ConstPtr,
    MutPtr
}
struct MayaVisitFirstPass {
    pub structs: HashSet<syn::Ident>,
    pub impls: HashSet<syn::Ident>,
    pub types: HashSet<syn::Ident>,
    pub excluded: HashSet<syn::Ident>,
}
impl MayaVisitFirstPass {
    pub fn new() -> MayaVisitFirstPass {
        MayaVisitFirstPass {
            structs: HashSet::new(),
            impls: HashSet::new(),
            types: HashSet::new(),
            excluded: HashSet::new(),
        }
    }
}
impl<'ast> syn::visit::Visit<'ast> for MayaVisitFirstPass {
    fn visit_item_struct(&mut self, i: &'ast ItemStruct) {
        // Skip MPx classes because we need to implement manual shims for those.
        // Skip Maya internal classes/bindgen internal classes.
        let ident_name = i.ident.to_string();
        if ident_name.starts_with("M") &&
                !ident_name.starts_with("MPx") &&
                !ident_name.contains("bindgen") &&
                !ident_name.contains("Tapi") &&
                ident_name != "MString"
        {
            self.structs.insert(i.ident);
        }
        else {
            self.excluded.insert(i.ident);
        }
    }
    fn visit_item_impl(&mut self, i: &'ast ItemImpl) {
        match *i.self_ty {
            Type::Path(ref p) => {
                let impl_type = p.path.segments.last().unwrap().value().ident;
                self.impls.insert(impl_type);
            },
            _ => {}
        }
    }
    fn visit_item_type(&mut self, i: &'ast ItemType) {
        let ident_name = i.ident.to_string();
        if ident_name.starts_with("M") {
            self.types.insert(i.ident);
        }
        else {
            self.excluded.insert(i.ident);
        }
    }
    fn visit_item_enum(&mut self, i: &'ast ItemEnum) {
        let ident_name = i.ident.to_string();
        if ident_name.starts_with("M") {
            self.types.insert(i.ident);
        }
        else {
            self.excluded.insert(i.ident);
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
    impl_partialeq: BTreeMap<syn::Ident, Vec<quote::Tokens>>,
    impl_getters: BTreeMap<syn::Ident, (MayaVisitPtrType, quote::Tokens)>,
    impl_setters: BTreeMap<syn::Ident, quote::Tokens>,
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
            impl_partialeq: BTreeMap::new(),
            impl_getters: BTreeMap::new(),
            impl_setters: BTreeMap::new(),
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
    /// Is this an OpenMaya struct type?
    fn is_struct(&self, ident: &syn::Ident) -> bool {
        self.first_pass.structs.contains(ident)
    }
    /// Was this one of the OpenMaya excluded struct/enum/types?
    fn is_excluded(&self, ident: &syn::Ident) -> bool {
        self.first_pass.excluded.contains(ident)
    }
    /// Is this type implemented with the bindings we have?
    /// i.e. the type must be a Rust native type or an OpenMaya type with an impl.
    fn is_implemented(&self, ident: &syn::Ident) -> bool {
        if self.is_excluded(&ident) {
            return false;
        }
        if self.is_struct(&ident) && !self.first_pass.impls.contains(&ident) {
            return false;
        }
        return true;
    }
    /// Convert type from bindgen to high-level API.
    /// Returns: Option((ident, is_ptr)) where ident is the new type token; is_ptr is
    /// whether the type is a *const/mut ident.
    fn quote_ty(ty: &syn::Type) -> Option<(syn::Ident, MayaVisitPtrType)> {
        match ty {
            &syn::Type::Path(ref ty) => {
                let name = ty.path.segments.last().unwrap().value().ident;
                match name.to_string().as_str() {
                    "c_char" => None, // XXX need to handle chars
                    "c_double" => Some((syn::Ident::from("f64"), MayaVisitPtrType::NotPtr)),
                    "c_float" => Some((syn::Ident::from("f32"), MayaVisitPtrType::NotPtr)),
                    "c_int" => Some((syn::Ident::from("i32"), MayaVisitPtrType::NotPtr)),
                    "c_long" => Some((syn::Ident::from("i64"), MayaVisitPtrType::NotPtr)),
                    "c_longlong" => Some((syn::Ident::from("i64"), MayaVisitPtrType::NotPtr)),
                    "c_schar" => None, // XXX need to handle chars
                    "c_short" => Some((syn::Ident::from("i16"), MayaVisitPtrType::NotPtr)),
                    "c_uchar" => Some((syn::Ident::from("u8"), MayaVisitPtrType::NotPtr)),
                    "c_uint" => Some((syn::Ident::from("u32"), MayaVisitPtrType::NotPtr)),
                    "c_ulong" => Some((syn::Ident::from("u64"), MayaVisitPtrType::NotPtr)),
                    "c_ulonglong" => Some((syn::Ident::from("u64"), MayaVisitPtrType::NotPtr)),
                    "c_ushort" => Some((syn::Ident::from("u16"), MayaVisitPtrType::NotPtr)),
                    "c_void" => None, // XXX maybe handle in unsafe fn
                    "MInt64" => Some((syn::Ident::from("i64"), MayaVisitPtrType::NotPtr)),
                    "MString" => Some((syn::Ident::from("String"), MayaVisitPtrType::NotPtr)),
                    _ => Some((name, MayaVisitPtrType::NotPtr)),
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
    /// Substitutes the Rust identifier for one better-suited in an input argument context.
    /// Currently, just allows you to use &str instead of &String.
    /// Note that you can't use &str instead of String since the object must be moved.
    /// XXX can we auto-upgrade String sites to be &str since we're wrapping anwyays?
    fn inputize(ty_ident: &syn::Ident, is_ptr: &MayaVisitPtrType) -> syn::Ident {
        if *ty_ident == syn::Ident::from("String") && *is_ptr != MayaVisitPtrType::NotPtr {
            syn::Ident::from("str")
        }
        else {
            *ty_ident
        }
    }
    fn process_binary_operator(&mut self, i: &ImplItemMethod, trait_name: &syn::Ident,
        fn_name: &syn::Ident)
    {
        let impl_type = self.impl_type;
        let fn_ident = &i.sig.ident;
        let fn_inputs = &i.sig.decl.inputs;
        if let &syn::FnArg::Captured(ref fn_arg) = *fn_inputs.last().unwrap().value() {
            if let Some((rhs_ident, rhs_is_ptr)) =
                MayaVisitSecondPass::quote_ty(&fn_arg.ty)
            {
                let rhs_ident = MayaVisitSecondPass::inputize(&rhs_ident, &rhs_is_ptr);
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
                        let ret_byval = self.wrap(&invoke_byval, &ret_ident);
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
                        let ret_byref = self.wrap(&invoke_byref, &ret_ident);
                        // Because the Maya API is weird enough as-is...
                        // If the lhs or rhs of the argument is non-const, clone it before using.
                        let self_mut_fix = match *fn_inputs.first().unwrap().value() {
                            &syn::FnArg::SelfRef(ref self_arg)
                                if self_arg.mutability.is_some() => quote! {
                                let mut self_  = self.clone();
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
                        self.impl_traits.push(quote! {
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
                        });
                    }
                }
            }
        }
    }
    fn process_index_operator(&mut self, i: &ImplItemMethod) {
        let impl_type = self.impl_type;
        let fn_ident = &i.sig.ident;
        let fn_inputs = &i.sig.decl.inputs;
        if let &syn::FnArg::Captured(ref fn_arg) = *fn_inputs.last().unwrap().value() {
            if let Some((rhs_ident, rhs_is_ptr)) =
                MayaVisitSecondPass::quote_ty(&fn_arg.ty)
            {
                let rhs_ident = MayaVisitSecondPass::inputize(&rhs_ident, &rhs_is_ptr);
                if let &syn::ReturnType::Type(_, ref ret_ty) = &i.sig.decl.output {
                    if let Some((ret_ident, ret_is_ptr)) =
                        MayaVisitSecondPass::quote_ty(&ret_ty)
                    {
                        let invoke_byval = match rhs_is_ptr {
                            MayaVisitPtrType::NotPtr => quote! {
                                arr._native.#fn_ident(self_)
                            },
                            MayaVisitPtrType::ConstPtr => quote! {
                                arr._native.#fn_ident(&self_._native)
                            },
                            MayaVisitPtrType::MutPtr => quote! {
                                arr._native.#fn_ident(&mut self_._native)
                            },
                        };
                        let invoke_byref = match rhs_is_ptr {
                            MayaVisitPtrType::NotPtr => quote! {
                                arr._native.#fn_ident(*self_)
                            },
                            MayaVisitPtrType::ConstPtr => quote! {
                                arr._native.#fn_ident(&self_._native)
                            },
                            MayaVisitPtrType::MutPtr => quote! {
                                arr._native.#fn_ident(&mut self_._native)
                            },
                        };
                        // Because the Maya API is weird enough as-is...
                        // arr must be non-mut in getters; if func requires mut, then clone.
                        // self (indexer) must be non-mut everywhere.
                        let arr_mut_fix = match *fn_inputs.first().unwrap().value() {
                            &syn::FnArg::SelfRef(ref self_arg)
                                if self_arg.mutability.is_some() => quote! {
                                let mut arr = arr.clone();
                            },
                            _ => quote! {}
                        };
                        let indexer_mut_fix = match rhs_is_ptr {
                            MayaVisitPtrType::MutPtr => quote! {
                                let mut self_  = self.clone();
                            },
                            _ => quote! {
                                let self_ = self;
                            }
                        };
                        // Always return value, not pointer, from Getter. Prefer the native function
                        // that returns by value, but fall back to dereferencing the pointer and
                        // cloning in the case of one that doesn't return by value.
                        // Thus, NotPtr always gets to insert its getter but ConstPtr and MutPtr
                        // must check the priority order first.
                        match ret_is_ptr {
                            MayaVisitPtrType::NotPtr => {
                                // Getter. Return as-is.
                                let ret_byval = self.wrap(&invoke_byval, &ret_ident);
                                let ret_byref = self.wrap(&invoke_byref, &ret_ident);
                                self.impl_getters.insert(rhs_ident, (MayaVisitPtrType::NotPtr,
                                quote! {
                                    impl<'a> Getter<#impl_type> for #rhs_ident {
                                        type Output = #ret_ident;
                                        fn get(self, arr: &#impl_type) -> Self::Output {
                                            #arr_mut_fix
                                            #indexer_mut_fix
                                            unsafe { #ret_byval }
                                        }
                                    }
                                    impl<'b> Getter<#impl_type> for &'b #rhs_ident {
                                        type Output = #ret_ident;
                                        fn get(self, arr: &#impl_type) -> Self::Output {
                                            #arr_mut_fix
                                            #indexer_mut_fix
                                            unsafe { #ret_byref }
                                        }
                                    }
                                }));
                            },
                            MayaVisitPtrType::ConstPtr => {
                                // Getter. Must wrap pointer in value.
                                let insert_ok = match self.impl_getters.get(&rhs_ident) {
                                    Some(&(ref priority, _)) =>
                                        *priority != MayaVisitPtrType::NotPtr,
                                    _ => true
                                };
                                let retrieval = if self.is_struct(&ret_ident) {
                                    quote! {
                                        let mut ret = #ret_ident::default();
                                        ret._native.operator_assign(ptr);
                                        ret
                                    }
                                }
                                else {
                                    quote! { *ptr }
                                };
                                if insert_ok {
                                    self.impl_getters.insert(rhs_ident, (MayaVisitPtrType::ConstPtr,
                                    quote! {
                                        impl Getter<#impl_type> for #rhs_ident {
                                            type Output = #ret_ident;
                                            fn get(self, arr: &#impl_type) -> Self::Output {
                                                #arr_mut_fix
                                                #indexer_mut_fix
                                                unsafe {
                                                    let ptr = #invoke_byval;
                                                    #retrieval
                                                }
                                            }
                                        }
                                        impl<'b> Getter<#impl_type> for &'b #rhs_ident {
                                            type Output = #ret_ident;
                                            fn get(self, arr: &#impl_type) -> Self::Output {
                                                #arr_mut_fix
                                                #indexer_mut_fix
                                                unsafe {
                                                    let ptr = #invoke_byref;
                                                    #retrieval
                                                }
                                            }
                                        }
                                    }));
                                }
                            },
                            MayaVisitPtrType::MutPtr => {
                                // Getter. Must wrap pointer in value.
                                let insert_ok = match self.impl_getters.get(&rhs_ident) {
                                    Some(&(ref priority, _)) =>
                                        *priority != MayaVisitPtrType::NotPtr &&
                                        *priority != MayaVisitPtrType::ConstPtr,
                                    _ => true
                                };
                                let retrieval = if self.is_struct(&ret_ident) {
                                    quote! {
                                        let mut ret = #ret_ident::default();
                                        ret._native.operator_assign(ptr);
                                        ret
                                    }
                                }
                                else {
                                    quote! { *ptr }
                                };
                                if insert_ok {
                                    self.impl_getters.insert(rhs_ident, (MayaVisitPtrType::MutPtr,
                                    quote! {
                                        impl Getter<#impl_type> for #rhs_ident {
                                            type Output = #ret_ident;
                                            fn get(self, arr: &#impl_type) -> Self::Output {
                                                #arr_mut_fix
                                                #indexer_mut_fix
                                                unsafe {
                                                    let ptr = #invoke_byval;
                                                    #retrieval
                                                }
                                            }
                                        }
                                        impl<'b> Getter<#impl_type> for &'b #rhs_ident {
                                            type Output = #ret_ident;
                                            fn get(self, arr: &#impl_type) -> Self::Output {
                                                #arr_mut_fix
                                                #indexer_mut_fix
                                                unsafe {
                                                    let ptr = #invoke_byref;
                                                    #retrieval
                                                }
                                            }
                                        }
                                    }));
                                }

                                // Setter. Accepts both reference and value input via Borrow.
                                let ret_ident = MayaVisitSecondPass::inputize(
                                        &ret_ident, &ret_is_ptr);
                                let unwrap_value = self.unwrap(&quote! { value }, &ret_ident,
                                        MayaVisitPtrType::ConstPtr);
                                let assignment = if ret_ident == syn::Ident::from("str") {
                                    quote! { (*ptr).operator_assign(#unwrap_value); }
                                }
                                else if self.is_struct(&ret_ident) {
                                    quote! { (*ptr).operator_assign(#unwrap_value); }
                                }
                                else {
                                    quote! { *ptr = *#unwrap_value; }
                                };
                                self.impl_setters.insert(rhs_ident, quote! {
                                    impl<'i> Setter<'i, #impl_type> for #rhs_ident {
                                        type Input = #ret_ident;
                                        fn set(self, arr: &mut #impl_type, value: &Self::Input) {
                                            #indexer_mut_fix
                                            unsafe {
                                                let ptr = #invoke_byval;
                                                #assignment
                                            }
                                        }
                                    }
                                    impl<'b, 'i> Setter<'i, #impl_type> for &'b #rhs_ident {
                                        type Input = #ret_ident;
                                        fn set(self, arr: &mut #impl_type, value: &Self::Input) {
                                            #indexer_mut_fix
                                            unsafe {
                                                let ptr = #invoke_byref;
                                                #assignment
                                            }
                                        }
                                    }
                                });
                            }
                        };
                    }
                }
            }
        }
    }
    // Wraps the raw result of the given native expression with the given native type.
    fn wrap(&self, native_expr: &quote::Tokens, rust_ty: &syn::Ident) -> quote::Tokens {
        if *rust_ty == syn::Ident::from("String") {
            // Special, we auto-convert the MString into a Rust String.
            quote! { String::from(&#native_expr) }
        }
        else if *rust_ty == syn::Ident::from("str") {
            // Special, we auto-convert the MString into a Rust &str.
            quote! { String::from(&#native_expr).as_str() }
        }
        else if self.first_pass.structs.contains(&rust_ty) {
            quote! { #rust_ty::from_native(#native_expr) }
        }
        else {
            quote! { #native_expr }
        }
    }
    fn unwrap(&self, rust_expr: &quote::Tokens, rust_ty: &syn::Ident, is_ptr: MayaVisitPtrType)
        -> quote::Tokens
    {
        let ref_token = match is_ptr {
            MayaVisitPtrType::NotPtr => quote! {},
            MayaVisitPtrType::ConstPtr => quote! { & },
            MayaVisitPtrType::MutPtr => quote! { &mut },
        };
        if *rust_ty == syn::Ident::from("String") {
            // Special, we auto-convert from Rust String back into MString.
            quote! {
                #ref_token
                root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString::from(
                        (#rust_expr).as_str())
            }
        }
        else if *rust_ty == syn::Ident::from("str") {
            // Special, we auto-convert from Rust &str back into MString.
            quote! {
                #ref_token
                root::AUTODESK_NAMESPACE::MAYA_NAMESPACE::API_VERSION::MString::from(#rust_expr)
            }
        }
        else if self.first_pass.structs.contains(&rust_ty) {
            // Grab native either by value or by reference. By value consumes the original.
            match is_ptr {
                MayaVisitPtrType::NotPtr => {
                    quote! { #ref_token #rust_ty::into_native(#rust_expr) }
                },
                _ => {
                    quote! { #ref_token (#rust_expr)._native }
                }
            }
        }
        else {
            quote! { #rust_expr }
        }
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
                self.impl_getters.clear();
                self.impl_setters.clear();
                syn::visit::visit_item_impl(self, i);

                if !self.impl_getters.is_empty() {
                    self.impl_traits.push(quote! {
                        impl Get for #impl_type {}
                    })
                }
                if !self.impl_setters.is_empty() {
                    self.impl_traits.push(quote! {
                        impl Set for #impl_type {}
                    })
                }

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
                let impl_getters = self.impl_getters.values().map(|x| &x.1);
                let impl_setters = self.impl_setters.values();
                self.tokens.push(quote! {
                    impl #impl_type {
                        pub fn from_native(native: #( #ns :: )* #impl_type) -> Self {
                            Self { _native: native }
                        }
                        pub unsafe fn into_native(s: Self) -> #( #ns :: )* #impl_type {
                            ::std::mem::transmute(s)
                        }
                        #( #impl_fns )*
                    }
                    #( #impl_traits )*
                    #( #impl_partialeq )*
                    #( #impl_getters )*
                    #( #impl_setters )*
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
                    // Implement Default trait as well as new() constructor.
                    self.impl_traits.push(quote! {
                        impl Default for #impl_type {
                            fn default() -> Self {
                                Self::new()
                            }
                        }
                    });
                    self.impl_fns.push(quote! {
                        pub fn new() -> Self {
                            Self::from_native(unsafe { #( #ns :: )* #impl_type :: #fn_ident() })
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
                            let rhs_ident = MayaVisitSecondPass::inputize(&rhs_ident, &rhs_is_ptr);
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
                            let rhs_ident = MayaVisitSecondPass::inputize(&rhs_ident, &rhs_is_ptr);
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
                    self.process_index_operator(i);
                }
                // Remaining operators (binary).
                else if fn_ident.to_string().starts_with("operator_add") {
                    self.process_binary_operator(
                            i, &syn::Ident::from("Add"), &syn::Ident::from("add"));
                }
                else if fn_ident.to_string().starts_with("operator_sub") {
                    self.process_binary_operator(
                            i, &syn::Ident::from("Sub"), &syn::Ident::from("sub"));
                }
                else if fn_ident.to_string().starts_with("operator_mul") {
                    self.process_binary_operator(
                            i, &syn::Ident::from("Mul"), &syn::Ident::from("mul"));
                }
                else if fn_ident.to_string().starts_with("operator_div") {
                    self.process_binary_operator(
                            i, &syn::Ident::from("Div"), &syn::Ident::from("div"));
                }
                else if fn_ident.to_string().starts_with("operator_or") {
                    self.process_binary_operator(
                            i, &syn::Ident::from("BitOr"), &syn::Ident::from("bitor"));
                }
                else if fn_ident.to_string().starts_with("operator_xor") {
                    self.process_binary_operator(
                            i, &syn::Ident::from("BitXor"), &syn::Ident::from("bitxor"));
                }
            },
            MayaVisitMethodType::Normal => {
                let impl_type = &self.impl_type;
                let ns = &self.cur_namespace;

                let fn_name = i.sig.ident;
                let fn_inputs = &i.sig.decl.inputs;
                let (ret_ident, ret_is_ptr) = match &i.sig.decl.output {
                    &syn::ReturnType::Type(_, ref ret_ty) => {
                        if let Some((ret_ident, ret_is_ptr)) =
                            MayaVisitSecondPass::quote_ty(&ret_ty)
                        {
                            if !self.is_implemented(&ret_ident) {
                                eprintln!("{}::{} -- skipping because return type `{}` not \
                                        implemented in bindings", impl_type, fn_name, ret_ident);
                                return;
                            }
                            (Some(ret_ident), ret_is_ptr)
                        }
                        else {
                            // Unknown return type; skip function processing.
                            return;
                        }
                    },
                    &syn::ReturnType::Default => {
                        (None, MayaVisitPtrType::NotPtr)
                    }
                };

                // Conform the function to a Rust-style API.
                let mut preface_transforms = vec![];
                let mut postscript_transforms = vec![];
                let mut inputs = vec![];
                let mut outputs = vec![];
                let mut invoke_args = vec![];
                let mut self_call = false;
                for input_pair in fn_inputs.pairs() {
                    let arg = match input_pair {
                        syn::punctuated::Pair::Punctuated(arg, _) => arg,
                        syn::punctuated::Pair::End(arg) => arg
                    };
                    match arg {
                        &syn::FnArg::SelfRef(..) | &syn::FnArg::SelfValue(..) => {
                            inputs.push(quote! { #arg });
                            self_call = true;
                        },
                        &syn::FnArg::Captured(ref arg) => {
                            match MayaVisitSecondPass::quote_ty(&arg.ty) {
                                Some((ty_ident, ty_is_ptr)) => {
                                    let name = &arg.pat;
                                    if !self.is_implemented(&ty_ident) {
                                        eprintln!("{}::{} -- skipping because arg type \
                                                `{}: {}` not implemented in bindings",
                                                impl_type, fn_name, name.into_tokens(), ty_ident);
                                        return;
                                    }
                                    match ty_is_ptr {
                                        MayaVisitPtrType::MutPtr => {
                                            if ty_ident == syn::Ident::from("MDataBlock") ||
                                                    ty_ident == syn::Ident::from("MDGContext") {
                                                // Special case -- mutable inputs.
                                                // XXX -- may need to generalize check types here.
                                                let ty_ident = MayaVisitSecondPass::inputize(
                                                        &ty_ident, &MayaVisitPtrType::MutPtr);
                                                let unwrapped = self.unwrap(
                                                        &quote! { #name }, &ty_ident,
                                                        MayaVisitPtrType::MutPtr);
                                                inputs.push(quote! { #name: &mut #ty_ident });
                                                invoke_args.push(unwrapped);
                                            }
                                            else {
                                                // Normal case -- mutables are output parameters.
                                                // Note: the declaration make #name into a value,
                                                // not pointer, so we need to refer to it by
                                                // &mut #name.
                                                // Warning: DO NOT inputize in this case!
                                                let unwrapped = self.unwrap(
                                                        &quote! { &mut #name }, &ty_ident,
                                                        MayaVisitPtrType::MutPtr);
                                                preface_transforms.push(quote! {
                                                    let mut #name = #ty_ident::default();
                                                });
                                                outputs.push((quote! { #name }, ty_ident));
                                                invoke_args.push(unwrapped);
                                            }
                                        },
                                        MayaVisitPtrType::ConstPtr => {
                                            let ty_ident = MayaVisitSecondPass::inputize(
                                                    &ty_ident, &MayaVisitPtrType::ConstPtr);
                                            let unwrapped = self.unwrap(
                                                    &quote! { #name }, &ty_ident,
                                                    MayaVisitPtrType::ConstPtr);
                                            inputs.push(quote! { #name: &#ty_ident });
                                            invoke_args.push(unwrapped);
                                        },
                                        MayaVisitPtrType::NotPtr => {
                                            let ty_ident = MayaVisitSecondPass::inputize(
                                                    &ty_ident, &MayaVisitPtrType::NotPtr);
                                            let unwrapped = self.unwrap(
                                                    &quote! { #name }, &ty_ident,
                                                    MayaVisitPtrType::NotPtr);
                                            inputs.push(quote! { #name: #ty_ident });
                                            invoke_args.push(unwrapped);
                                        },
                                    }
                                }
                                None => return // don't know how to handle
                            };
                        }
                        _ => return // don't know how to handle
                    };
                }

                let invoke = if self_call {
                    quote! { self._native.#fn_name(#( #invoke_args ),*) }
                }
                else {
                    quote! { #( #ns ::)* #impl_type :: #fn_name(#( #invoke_args ),*) }
                };

                if let Some(ret_ident) = ret_ident {
                    match ret_is_ptr {
                        MayaVisitPtrType::NotPtr => {
                            // Need to wrap because the function returned a native arg by value.
                            let ret_wrapped = self.wrap(&quote! { __maya_ret }, &ret_ident);
                            outputs.push((ret_wrapped, ret_ident));
                        },
                        _ => {
                            // Manually wrap by constructing the wrapper ahead-of-time, assigning
                            // the pointer into the wrapper, and then returning the wrapper.
                            // (Strings and primitives can be copied after-the-fact.)
                            if ret_ident == syn::Ident::from("String") {
                                postscript_transforms.push(quote! {
                                    let __maya_ret_val = String::from(unsafe { &*__maya_ret });
                                });
                            }
                            else if self.is_struct(&ret_ident) {
                                postscript_transforms.push(quote! {
                                    let mut __maya_ret_val = #ret_ident::default();
                                    unsafe { __maya_ret_val._native.operator_assign(__maya_ret); }
                                });
                            }
                            else {
                                postscript_transforms.push(quote! {
                                    let __maya_ret_val = unsafe { *__maya_ret };
                                })
                            }
                            outputs.push((quote! { __maya_ret_val }, ret_ident));
                        }
                    }
                }

                // Look through the outputs to see if one of them is MStatus-typed.
                // If so, loft it into the Result type.
                let mut status = None;
                for i in 0..outputs.len() {
                    let (_, ret_ident) = outputs[i];
                    if ret_ident == syn::Ident::from("MStatus") {
                        let (status_quote, _) = outputs.remove(i);
                        status = Some(status_quote);
                        break;
                    }
                }
                let ret_decl = match (outputs.len(), &status) {
                    (0, &None) => quote! {},
                    (0, &Some(..)) => quote! { -> Result<(), MStatus> },
                    (1, &None) => {
                        let &(_, only_ty) = outputs.first().unwrap();
                        quote! { -> #only_ty }
                    },
                    (1, &Some(..)) => {
                        let &(_, only_ty) = outputs.first().unwrap();
                        quote! { -> Result<#only_ty, MStatus> }
                    },
                    (_, &None) => {
                        let tys = outputs.iter().map(|&(_, ty)| ty);
                        quote! { -> (#( #tys ),*) }
                    },
                    (_, &Some(..)) => {
                        let tys = outputs.iter().map(|&(_, ty)| ty);
                        quote! { -> Result<(#( #tys ),*), MStatus> }
                    },
                };
                let ret_statement = match (outputs.len(), status) {
                    (0, None) => quote! {},
                    (0, Some(status_quote)) => quote! {
                        let __maya_ret_status = #status_quote;
                        match __maya_ret_status.error() {
                            false => Ok(()),
                            true => Err(__maya_ret_status),
                        }
                    },
                    (1, None) => {
                        let &(ref only_ret, _) = outputs.first().unwrap();
                        quote! { #only_ret }
                    },
                    (1, Some(status_quote)) => {
                        let &(ref only_ret, _) = outputs.first().unwrap();
                        quote! {
                            let __maya_ret_status = #status_quote;
                            match __maya_ret_status.error() {
                                false => Ok(#only_ret),
                                true => Err(__maya_ret_status),
                            }
                        }
                    },
                    (_, None) => {
                        let rets = outputs.iter().map(|&(ref ret, _)| ret);
                        quote! { (#( #rets ),*) }
                    },
                    (_, Some(status_quote)) => {
                        let rets = outputs.iter().map(|&(ref ret, _)| ret);
                        quote! {
                            let __maya_ret_status = #status_quote;
                            match __maya_ret_status.error() {
                                false => Ok((#( #rets ),*)),
                                true => Err(__maya_ret_status),
                            }
                        }
                    },
                };

                self.impl_fns.push(quote! {
                    pub fn #fn_name(#( #inputs ),*) #ret_decl {
                        #( #preface_transforms )*
                        let __maya_ret = unsafe { #invoke };
                        #( #postscript_transforms )*
                        #ret_statement
                    }
                });
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
        .whitelist_type(".*MDagPathArray")
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
    let mut first_pass = MayaVisitFirstPass::new();
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
