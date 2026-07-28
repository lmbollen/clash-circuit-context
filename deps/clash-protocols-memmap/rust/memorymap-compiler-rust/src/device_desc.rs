// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Generating Rust types for device descriptions.
//!
//! This crate handles generating accessor methods for all the
//! registers in a device.

use std::collections::BTreeSet;

use memorymap_compiler::{
    input_language::RegisterAccess,
    ir::{
        monomorph::MonomorphVariants,
        types::{DeviceDescription, IrCtx, RegisterDescription, TypeRef},
    },
    storage::Handle,
};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    IdentType, ident,
    types::{TypeReferences, generate_type_ref},
};

/// Generate code for a device description
///
/// Takes into account monomorphization.
///
/// Returns the name of the device, generated code and any types referenced in the code.
pub fn generate_device_desc<'ir>(
    ctx: &'ir IrCtx,
    varis: &MonomorphVariants,
    handle: Handle<DeviceDescription>,
) -> (&'ir str, proc_macro2::TokenStream, TypeReferences) {
    let mut refs = TypeReferences {
        references: BTreeSet::new(),
        use_bitvec: false,
        use_index: false,
        use_signed: false,
        use_unsigned: false,
        use_mask: false,
    };
    let desc = &ctx.device_descs[handle];
    let name = &ctx.identifiers[desc.name];

    let name_ident = ident(IdentType::Device, name);

    let get_funcs = desc
        .registers
        .handles()
        .map(|reg| generate_reg_get_method(ctx, varis, &mut refs, reg))
        .collect::<Vec<_>>();
    let set_funcs = desc
        .registers
        .handles()
        .map(|reg| generate_reg_set_method(ctx, varis, &mut refs, reg))
        .collect::<Vec<_>>();

    let consts = desc
        .registers
        .handles()
        .map(|reg| generate_const(ctx, varis, &mut refs, reg));

    let code = quote! {
        pub struct #name_ident(pub *mut u8);

        impl #name_ident {
            #(#consts)*

            pub const unsafe fn new(addr: *mut u8) -> Self {
                Self(addr)
            }

            #(#get_funcs)*

            #(#set_funcs)*

        }
    };

    (name, code, refs)
}

fn generate_const(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    refs: &mut TypeReferences,
    reg: Handle<RegisterDescription>,
) -> TokenStream {
    let desc = &ctx.registers[reg];
    let ty = &ctx.type_refs[desc.type_ref];

    let variables = match ty {
        TypeRef::BitVector(handle)
        | TypeRef::Unsigned(handle)
        | TypeRef::Signed(handle)
        | TypeRef::Mask(handle) => {
            let width = generate_type_ref(ctx, varis, None, refs, *handle);
            Some(("WIDTH", width))
        }
        TypeRef::Index(handle) => {
            let size = generate_type_ref(ctx, varis, None, refs, *handle);
            Some(("SIZE", size))
        }
        TypeRef::Vector(len, _) => {
            let len = generate_type_ref(ctx, varis, None, refs, *len);
            Some(("LEN", len))
        }
        _ => None,
    };
    let Some((suffix, value)) = variables else {
        return TokenStream::new();
    };
    let desc_name = &ctx.identifiers[desc.name];
    let const_name = ident(IdentType::Constant, format!("{desc_name}_{suffix}"));
    quote! {
        pub const #const_name: usize = #value;
    }
}

fn generate_reg_get_method(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    refs: &mut TypeReferences,
    reg: Handle<RegisterDescription>,
) -> TokenStream {
    let reg = &ctx.registers[reg];
    if !matches!(
        reg.access,
        RegisterAccess::ReadOnly | RegisterAccess::ReadWrite
    ) {
        return quote! {};
    }

    let name = ident(IdentType::Variable, &ctx.identifiers[reg.name]);
    let offset = reg.address as usize;

    let desc = &reg.description;
    let desc = if desc.is_empty() {
        quote! {}
    } else {
        quote! { #[doc = #desc]}
    };

    let raw_ty = &ctx.type_refs[reg.type_ref];

    if let TypeRef::Vector(len, inner) = raw_ty {
        let unchecked_name = ident(IdentType::Method, format!("{name}_unchecked"));
        let iter_name = ident(IdentType::Method, format!("{name}_volatile_iter"));

        let scalar_ty = generate_type_ref(ctx, varis, None, refs, *inner);
        let size = generate_type_ref(ctx, varis, None, refs, *len);

        quote! {
            #desc
            pub fn #name(&self, idx: usize) -> Option<#scalar_ty> {
                if idx >= #size {
                    None
                } else {
                    Some(unsafe { self.#unchecked_name(idx)})
                }
            }

            #desc
            pub unsafe fn #unchecked_name(&self, idx: usize) -> #scalar_ty {
                unsafe {
                    let ptr = self.0.add(#offset).cast::<#scalar_ty>();
                    ptr.add(idx).read_volatile()
                }
            }

            #desc
            pub fn #iter_name(&self) -> impl DoubleEndedIterator<Item = #scalar_ty> + '_ {
                (0..#size).map(|i| unsafe {
                    self.#unchecked_name(i)
                })
            }
        }
    } else if ctx.tags[reg.tags].iter().any(|tag| tag == "zero-width") {
        let ty = generate_type_ref(ctx, varis, None, refs, reg.type_ref);
        quote! {
            #desc
            pub fn #name(&self) -> #ty {
                let _ = unsafe {
                    self.0.add(#offset).cast::<u8>().read_volatile()
                };
                unsafe { core::mem::transmute(()) }
            }
        }
    } else {
        let ty = generate_type_ref(ctx, varis, None, refs, reg.type_ref);
        quote! {
            #desc
            pub fn #name(&self) -> #ty {
                unsafe {
                    self.0.add(#offset).cast::<#ty>().read_volatile()
                }
            }
        }
    }
}

fn generate_reg_set_method(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    refs: &mut TypeReferences,
    reg: Handle<RegisterDescription>,
) -> TokenStream {
    let reg = &ctx.registers[reg];
    if !matches!(
        reg.access,
        RegisterAccess::WriteOnly | RegisterAccess::ReadWrite
    ) {
        return quote! {};
    }

    let name = ident(
        IdentType::Variable,
        format!("set_{}", &ctx.identifiers[reg.name]),
    );
    let offset = reg.address as usize;

    let desc = &reg.description;
    let desc = if desc.is_empty() {
        quote! {}
    } else {
        quote! { #[doc = #desc]}
    };

    let raw_ty = &ctx.type_refs[reg.type_ref];

    if let TypeRef::Vector(len, inner) = raw_ty {
        let unchecked_name = ident(IdentType::Method, format!("{name}_unchecked"));

        let scalar_ty = generate_type_ref(ctx, varis, None, refs, *inner);
        let size = generate_type_ref(ctx, varis, None, refs, *len);

        quote! {
            #desc
            pub fn #name(&self, idx: usize, val: #scalar_ty) -> Option<()> {
                if idx >= #size {
                    None
                } else {
                    unsafe { self.#unchecked_name(idx, val) };
                    Some(())
                }
            }

            #desc
            pub unsafe fn #unchecked_name(&self, idx: usize, val: #scalar_ty) {
                unsafe {
                    let ptr = self.0.add(#offset).cast::<#scalar_ty>();
                    ptr.add(idx).write_volatile(val);
                }
            }

        }
    } else if ctx.tags[reg.tags].iter().any(|tag| tag == "zero-width") {
        let ty = generate_type_ref(ctx, varis, None, refs, reg.type_ref);
        quote! {
            #desc
            pub fn #name(&self, _val: #ty) {
                unsafe {
                    self.0.add(#offset).cast::<u8>().write_volatile(0)
                }
            }
        }
    } else {
        let ty = generate_type_ref(ctx, varis, None, refs, reg.type_ref);
        quote! {
            #desc
            pub fn #name(&self, val: #ty) {
                unsafe {
                    self.0.add(#offset).cast::<#ty>().write_volatile(val)
                }
            }
        }
    }
}
