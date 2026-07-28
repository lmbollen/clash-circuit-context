// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use std::collections::BTreeSet;

use std::fmt::Write;

use crate::{IdentType, ident};
use memorymap_compiler::ir::instance_names::InstanceNames;
use memorymap_compiler::ir::types::TreeElemType;
use memorymap_compiler::{
    ir::{
        deduplicate::HalShared,
        types::{IrCtx, TreeElem},
    },
    storage::Handle,
};

pub fn generate_device_instances(
    ctx: &IrCtx,
    shared: &HalShared,
    instance_names: &InstanceNames,
    hal_name: &str,
    tree_elems: impl Iterator<Item = Handle<TreeElem>> + Clone,
) -> String {
    let mut device_type = String::new();
    let mut device_instance = String::new();
    let mut imports_shared = BTreeSet::new();
    let mut imports_local = BTreeSet::new();

    let hal_ident = ident(IdentType::Module, hal_name);

    writeln!(device_type, "typedef struct DeviceInstances {{").unwrap();

    writeln!(device_instance, "static const DeviceInstances hal = {{").unwrap();

    for handle in tree_elems {
        let (name, addr) = match ctx.tree_elem_types[handle.cast()] {
            TreeElemType::DeviceInstance { device_name } => {
                let elem = &ctx.tree_elems[handle];
                let addr = elem.absolute_addr;
                if ctx.tags[elem.tags].iter().any(|tag| tag == "no-generate") {
                    continue;
                }
                (device_name, addr)
            }
            TreeElemType::Interconnect {
                rel_addrs: _,
                components: _,
            } => continue,
        };
        let name = &ctx.identifiers[name];
        let dev_ident = ident(IdentType::Device, name);

        let instance_name_data = &instance_names.names[&handle];

        let instance_name = if instance_name_data.num_conflicts > 1 {
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

        let hex_addr = format!("0x{addr:X}");

        writeln!(device_type, "  {dev_ident} {instance_name};").unwrap();

        writeln!(
            device_instance,
            "  .{instance_name} = {{ .base = (volatile uint8_t *) {hex_addr} }},"
        )
        .unwrap();

        if shared.deduped_devices.iter().any(|dev| {
            let device_desc = &ctx.device_descs[*dev];
            let found = &ctx.identifiers[device_desc.name];
            found == name
        }) {
            imports_shared.insert(dev_ident.to_string());
        } else {
            imports_local.insert(dev_ident.to_string());
        };
    }

    writeln!(device_type, "}} DeviceInstances;").unwrap();

    writeln!(device_instance, "}};").unwrap();

    let mut code = String::new();
    for shared_dev in imports_shared {
        let dev_name = ident(IdentType::Module, shared_dev);
        writeln!(code, "#include \"shared_devices/{dev_name}.h\"").unwrap();
    }
    for local_dev in imports_local {
        let dev_name = ident(IdentType::Module, local_dev);
        writeln!(code, "#include \"hals/{hal_ident}/devices/{dev_name}.h\"").unwrap();
    }

    writeln!(code).unwrap();
    writeln!(code, "{device_type}").unwrap();
    writeln!(code, "{device_instance}").unwrap();

    code
}
