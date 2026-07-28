// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Compiling memory maps to Rust code!
//!
//! This crate provides the building blocks for generating Rust bindings
//! to Clash designs using `clash-protocols-memmap`.
//!
//! The building blocks provided are:
//!   - types (defintions, references)
//!   - device definitions (with registers)
//!   - device instances (components present in the design)

use std::collections::{BTreeSet, HashMap};

use heck::{ToPascalCase, ToShoutySnakeCase, ToSnakeCase};
use proc_macro2::{Ident, Span};

use memorymap_compiler::{
    ir::{
        monomorph::TypeRefVariant,
        types::{IrCtx, TypeRef},
    },
    storage::Handle,
};

pub mod device_desc;
pub mod device_instances;
pub mod types;

pub mod testing_utils;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IdentType {
    Type,
    TypeVariable,
    Device,
    Instance,
    Module,
    Method,
    Variable,
    Constant,
    // Just pass through the raw string
    Raw,
}

/// Generate a contextual identifier from a string.
pub fn ident(ident_type: IdentType, n: impl AsRef<str>) -> Ident {
    let s = n.as_ref();
    let s = s.split(".").last().unwrap();
    let s = match ident_type {
        IdentType::Type => s.to_pascal_case(),
        IdentType::TypeVariable => s.to_shouty_snake_case(),
        IdentType::Device => s.to_pascal_case(),
        IdentType::Instance => s.to_snake_case(),
        IdentType::Module => s.to_snake_case(),
        IdentType::Method => s.to_snake_case(),
        IdentType::Variable => s.to_snake_case(),
        IdentType::Constant => s.to_shouty_snake_case(),
        IdentType::Raw => s.to_string(),
    };

    Ident::new(&s, Span::call_site())
}

/// User provided annotations. Will be considered during code generation.
#[derive(Default)]
pub struct Annotations {
    pub type_annotation: HashMap<Handle<TypeRefVariant>, BTreeSet<String>>,
    pub type_imports: HashMap<Handle<TypeRefVariant>, BTreeSet<String>>,
    pub type_tags: HashMap<Handle<TypeRefVariant>, BTreeSet<String>>,
}

fn lookup_sub(variant: Option<&TypeRefVariant>, handle: Handle<TypeRef>) -> Handle<TypeRef> {
    let mut handle = handle;
    let subs = if let Some(var) = variant {
        &var.variable_substitutions
    } else {
        return handle;
    };
    while let Some(sub) = subs.get(&handle).copied() {
        handle = sub;
    }
    handle
}

fn po2_type(n: u64) -> u64 {
    let n = n.max(8);
    if n.is_power_of_two() {
        n
    } else {
        n.next_power_of_two()
    }
}

fn mono_variant_name(ctx: &IrCtx, var: &TypeRefVariant) -> (Ident, bool) {
    let desc = &ctx.type_descs[var.original_type_desc];
    let ty_name = &ctx.type_names[desc.name];
    let args = var
        .argument_mono_values
        .iter()
        .map(|arg| arg.map(|val| type_to_ident(ctx, val)));

    let has_mono_args = args.clone().any(|arg| arg.is_some());

    let mut arg_names = String::new();
    if has_mono_args {
        for arg in args.flatten() {
            arg_names.push('_');
            arg_names.push_str(&arg);
        }
    }

    let type_name_base = ident(IdentType::Type, &ty_name.base);
    (
        ident(IdentType::Raw, format!("{type_name_base}{arg_names}")),
        has_mono_args,
    )
}

fn type_to_ident(ctx: &IrCtx, ty: Handle<TypeRef>) -> String {
    match &ctx.type_refs[ty] {
        TypeRef::BitVector(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("bv{len}")
        }
        TypeRef::Unsigned(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("u{len}")
        }
        TypeRef::Signed(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("s{len}")
        }
        TypeRef::Index(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("i{len}")
        }
        TypeRef::Mask(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("mask{len}")
        }
        TypeRef::Bool => "bool".to_string(),
        TypeRef::Float => "f32".to_string(),
        TypeRef::Double => "f64".to_string(),
        TypeRef::Vector(size_handle, inner) => {
            let size = type_to_ident(ctx, *size_handle);
            let inner = type_to_ident(ctx, *inner);
            format!("vec_{size}_{inner}")
        }
        TypeRef::Tuple(handle_range) if handle_range.len == 0 => "unit".to_string(),
        TypeRef::Tuple(handle_range) => {
            let n = handle_range.len;
            let mut name = format!("tuple{n}");
            for handle in handle_range.handles() {
                let add = type_to_ident(ctx, handle);
                if !add.is_empty() {
                    name.push('_');
                    name.push_str(&add);
                }
            }
            name
        }
        TypeRef::Variable(handle) => {
            let name = &ctx.identifiers[*handle];
            format!("var_{name}")
        }
        TypeRef::Nat(n) => format!("{n}"),
        TypeRef::Reference { name, args } => {
            let type_name = &ctx.type_names[*name];
            let mut ident = type_name.base.clone();
            for arg in args.handles() {
                let add = type_to_ident(ctx, arg);
                if !add.is_empty() {
                    ident.push('_');
                    ident.push_str(&add);
                }
            }
            ident
        }
    }
}
