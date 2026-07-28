// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Generate information about instance names so that bindings can have
//! consistent naming.

use std::collections::BTreeMap;

use crate::{
    ir::types::{Identifier, IrCtx, PathComp, TreeElem, TreeElemType},
    storage::{Handle, HandleRange},
};

use super::deduplicate::DedupelicatedHal;

/// Helper function to collect all names of device instances in a HAL
/// to generate proper unique names on a best-fit basis.
///
/// This is done by counting all the instance names and creating a
/// mapping from handles to names for convenience.

#[derive(Default)]
pub struct InstanceNames {
    pub names: BTreeMap<Handle<TreeElem>, InstanceName>,
}

pub struct InstanceName {
    pub name: InstanceNameType,
    pub num_conflicts: usize,
    pub conflict_idx: usize,
}

#[derive(Clone)]
pub enum InstanceNameType {
    NamedPath(Handle<Vec<PathComp>>),
    DeviceName(Handle<Identifier>),
}

impl InstanceNameType {
    pub fn as_str<'ctx>(&self, ctx: &'ctx IrCtx) -> &'ctx str {
        match self {
            InstanceNameType::NamedPath(handle) => path_name(&ctx.paths[*handle]).unwrap(),
            InstanceNameType::DeviceName(handle) => &ctx.identifiers[*handle],
        }
    }
}

pub fn calculate_instance_names(
    names: &mut InstanceNames,
    ctx: &'_ IrCtx,
    hals: &[DedupelicatedHal],
) {
    for hal in hals {
        calculate_instance_names_in_hal(names, ctx, hal.tree_elem_range);
    }
}

pub fn calculate_instance_names_in_hal(
    names: &mut InstanceNames,
    ctx: &'_ IrCtx,
    tree_elems: HandleRange<TreeElem>,
) {
    let mut name_counts = BTreeMap::<&str, usize>::new();
    let mut mapping = BTreeMap::<Handle<TreeElem>, _>::new();

    for elem_handle in tree_elems.handles() {
        let device_name_handle = match &ctx.tree_elem_types[elem_handle.cast()] {
            TreeElemType::DeviceInstance { device_name } => *device_name,
            TreeElemType::Interconnect { .. } => continue,
        };
        let device_name = &ctx.identifiers[device_name_handle];

        let elem = &ctx.tree_elems[elem_handle];

        if ctx.tags[elem.tags].iter().any(|tag| tag == "no-generate") {
            continue;
        }

        if let Some(name) = path_name(&ctx.paths[elem.path]) {
            let conflict_idx = name_counts.entry(name).or_default();
            let name_type = InstanceNameType::NamedPath(elem.path);
            mapping.insert(elem_handle, name_type.clone());
            names.names.insert(
                elem_handle,
                InstanceName {
                    name: name_type,
                    num_conflicts: 0,
                    conflict_idx: *conflict_idx,
                },
            );
            *conflict_idx += 1;
        } else {
            let conflict_idx = name_counts.entry(device_name.as_str()).or_default();
            let name_type = InstanceNameType::DeviceName(device_name_handle);
            mapping.insert(elem_handle, name_type.clone());
            names.names.insert(
                elem_handle,
                InstanceName {
                    name: name_type,
                    num_conflicts: 0,
                    conflict_idx: *conflict_idx,
                },
            );
            *conflict_idx += 1;
        }
    }

    for elem_handle in tree_elems.handles() {
        let Some(instance_name) = names.names.get_mut(&elem_handle) else {
            continue;
        };

        let string_name = match &instance_name.name {
            InstanceNameType::NamedPath(handle) => path_name(&ctx.paths[*handle]).unwrap(),
            InstanceNameType::DeviceName(handle) => &ctx.identifiers[*handle],
        };

        let num_conflicts = name_counts[string_name];
        instance_name.num_conflicts = num_conflicts;
    }
}

/// Helper to get the last named component of a path, if it exists.
fn path_name(path: &[PathComp]) -> Option<&str> {
    let comp = path.last()?;
    match comp {
        PathComp::Named { loc: _, name } => Some(name.as_str()),
        PathComp::Unnamed(_) => None,
    }
}
