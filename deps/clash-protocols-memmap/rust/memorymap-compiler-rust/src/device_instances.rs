// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Generate a Rust struct for all the device instances in a design.
//!
//! Each device instance gets its own member in the struct, except for
//! skipped device types or skipped instances.

use std::collections::BTreeMap;

use memorymap_compiler::{
    ir::{
        deduplicate::HalShared,
        instance_names::InstanceNames,
        types::{IrCtx, TreeElem, TreeElemType},
    },
    storage::Handle,
};
use proc_macro2::{Literal, TokenStream};
use quote::quote;

use crate::{IdentType, ident};

/// Generate code for device instances in a HAL.
///
/// This results in a struct definition and an `impl` block with
/// a `new` method.
pub fn generate_device_instances(
    ctx: &IrCtx,
    shared: &HalShared,
    instance_names: &InstanceNames,
    hal_name: &str,
    tree_elems: impl Iterator<Item = Handle<TreeElem>> + Clone,
) -> TokenStream {
    let hal_ident = ident(IdentType::Module, hal_name);

    let (imports, fields) = tree_elems
        .filter_map(|handle| match ctx.tree_elem_types[handle.cast()] {
            TreeElemType::DeviceInstance { device_name } => {
                let elem = &ctx.tree_elems[handle];
                let addr = elem.absolute_addr;
                if ctx.tags[elem.tags].iter().any(|tag| tag == "no-generate") {
                    return None;
                }
                Some((handle, device_name, addr))
            }
            TreeElemType::Interconnect { .. } => None,
        })
        .map(move |(handle, dev_name, abs_addr)| {
            let name = &ctx.identifiers[dev_name];
            let dev_ident = ident(IdentType::Device, name);

            let instance_name_data = &instance_names.names[&handle];

            let instance_name_ident = if instance_name_data.num_conflicts > 1 {
                ident(
                    IdentType::Instance,
                    format!(
                        "{}_{}",
                        instance_name_data.name.as_str(ctx),
                        instance_name_data.conflict_idx
                    ),
                )
            } else {
                ident(IdentType::Instance, instance_name_data.name.as_str(ctx))
            };

            use std::str::FromStr;
            let addr = Literal::from_str(&format!("0x{abs_addr:X}")).unwrap();

            let field_def = quote! { pub #instance_name_ident: #dev_ident };
            let field_init =
                quote! { #instance_name_ident: unsafe { #dev_ident::new(#addr as *mut u8) } };
            let import = if shared.deduped_devices.iter().any(|dev| {
                let device_desc = &ctx.device_descs[*dev];
                let found = &ctx.identifiers[device_desc.name];
                found == name
            }) {
                (
                    dev_ident.to_string(),
                    quote! { use crate::shared_devices::#dev_ident; },
                )
            } else {
                (
                    dev_ident.to_string(),
                    quote! { use crate::hals::#hal_ident::devices::#dev_ident; },
                )
            };
            (import, (field_def, field_init))
        })
        .unzip::<_, _, BTreeMap<_, _>, Vec<_>>();
    let (field_defs, field_inits) = fields.into_iter().unzip::<_, _, Vec<_>, Vec<_>>();

    let imports = imports.values();
    quote! {

        #(#imports)*
        pub struct DeviceInstances {
            #(#field_defs,)*
        }

        impl DeviceInstances {
            pub const unsafe fn new() -> Self {
                DeviceInstances {
                    #(#field_inits,)*
                }
            }
        }
    }
}
