// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Generating Rust type bindings for Clash types.
//!
//! This includes both type definitions (usually referred to as descriptions)
//! and type references.
//!
//! A type definition needs the whole description of a type so that the Rust
//! compiler has all the information to properly represent such a type.
//!
//! A type reference is when a type is used somewhere, for example as the type
//! of a register or as a member in another type definition.

use std::{collections::BTreeSet, str::FromStr};

use memorymap_compiler::{
    input_language::TypeName,
    ir::{
        monomorph::{MonomorphVariants, TypeRefVariant},
        types::{IrCtx, TypeDefinition, TypeDescription, TypeRef},
    },
    storage::Handle,
};
use proc_macro2::{Literal, TokenStream};
use quote::{ToTokens, quote};

use crate::{Annotations, IdentType, ident, lookup_sub, mono_variant_name, po2_type};

/// Generate code for a type description, taking into account monomorp variants.
///
/// Returns the base-name of the type, the code and any types referenced by the code.
pub fn generate_type_desc<'ir>(
    ctx: &'ir IrCtx,
    varis: &MonomorphVariants,
    anns: &Annotations,
    handle: Handle<TypeDescription>,
) -> (&'ir str, proc_macro2::TokenStream, TypeReferences) {
    let mut refs = TypeReferences {
        references: BTreeSet::new(),
        use_bitvec: false,
        use_index: false,
        use_signed: false,
        use_unsigned: false,
        use_mask: false,
    };
    let desc = &ctx.type_descs[handle];
    let type_name = &ctx.type_names[desc.name];
    let variants = if let Some(variants) = varis.variants_by_type.get(&desc.name) {
        let mut all_imports = BTreeSet::new();

        let variant_toks = variants.iter().map(|var_handle| {
            generate_type_definition(ctx, varis, *var_handle, anns, &mut refs, desc)
        });

        for var in variants {
            if let Some(imports) = anns.type_imports.get(var) {
                all_imports.extend(imports)
            }
        }

        let imports = all_imports
            .into_iter()
            .map(|s| TokenStream::from_str(s).expect("invalid TokenStream for import"))
            .map(|import| quote! { use #import; });

        quote! {
            #(#imports)*
            #(#variant_toks)*
        }
    } else {
        quote! {
            compile_error!("Type `#type_name` has no monomorph variant");
        }
    };

    (&type_name.base, variants, refs)
}

fn generate_type_definition(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    var_handle: Handle<TypeRefVariant>,
    anns: &Annotations,
    refs: &mut TypeReferences,
    desc: &TypeDescription,
) -> TokenStream {
    let variant = &varis.type_ref_variants[var_handle];
    let raw_name = &ctx.type_names[desc.name];
    let (ty_name, has_mono_args) = mono_variant_name(ctx, variant);

    let args = {
        let arg_decls = desc
            .param_names
            .handles()
            .zip(&variant.argument_mono_values)
            .filter_map(|(handle, val)| {
                if val.is_none() {
                    let name = ident(IdentType::TypeVariable, &ctx.identifiers[handle]);
                    if ctx.type_param_nats.contains(&handle) {
                        Some(quote! { const #name: u128 })
                    } else {
                        Some(quote! { #name })
                    }
                } else {
                    None
                }
            });
        if variant.argument_mono_values.iter().any(|val| val.is_none()) {
            quote! { < #(#arg_decls,)* > }
        } else {
            quote! {}
        }
    };

    let attrs = {
        let repr = generate_repr(ctx, desc);
        let naming_attr = if has_mono_args {
            quote! { #[allow(non_camel_case_types)]}
        } else {
            TokenStream::new()
        };
        let annots = if let Some(code) = anns.type_annotation.get(&var_handle) {
            code
        } else {
            &BTreeSet::new()
        };
        let annots = annots
            .iter()
            .map(|s| TokenStream::from_str(s).expect("invalid TokenStream for type annotations"))
            .map(|toks| quote! { #toks });
        quote! {
            #naming_attr
            #repr
            #(#annots)*
        }
    };

    match &desc.definition {
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len == 0 => {
            quote! {
                #attrs
                pub struct #ty_name;
            }
        }
        TypeDefinition::DataType {
            names,
            constructors,
        } if constructors.len == 1 => {
            let con = &ctx.type_constructors[constructors.start];
            let con_name = &ctx.identifiers[names.start];

            if con_name != &raw_name.base {
                panic!(
                    "Single-constructor data types are required to have the constructor name match the type name. \
                    Type name: '{type_name:?}', Constructor name: '{constructor_name:?}'",
                    type_name = raw_name.base,
                    constructor_name = con_name,
                )
            }

            if let Some(names) = con.field_names {
                let fields_toks =
                    names
                        .handles()
                        .zip(con.field_types.handles())
                        .map(|(name, ty)| {
                            let id = ident(IdentType::Variable, &ctx.identifiers[name]);
                            let ty = generate_type_ref(ctx, varis, Some(variant), refs, ty);
                            quote! { pub #id: #ty }
                        });
                quote! {
                    #attrs
                    pub struct #ty_name #args {
                        #(#fields_toks,)*
                    }
                }
            } else {
                let fields_toks = con
                    .field_types
                    .handles()
                    .map(|f| generate_type_ref(ctx, varis, Some(variant), refs, f));
                let body = if con.field_types.len == 0 {
                    quote! {}
                } else {
                    quote! { ( #(pub #fields_toks,)* ) }
                };
                quote! {
                    #attrs
                    pub struct #ty_name #args #body;
                }
            }
        }
        TypeDefinition::DataType {
            names,
            constructors,
        } => {
            let cons_toks =
                names
                    .handles()
                    .zip(constructors.handles())
                    .map(|(name_handle, con_handle)| {
                        let con_name = &ctx.identifiers[name_handle];
                        let con = &ctx.type_constructors[con_handle];
                        let id = ident(IdentType::Type, con_name);

                        if let Some(field_names) = con.field_names {
                            let fields_toks = field_names
                                .handles()
                                .zip(con.field_types.handles())
                                .map(|(name, ty)| {
                                    let name = ident(IdentType::Variable, &ctx.identifiers[name]);
                                    let ty = generate_type_ref(ctx, varis, Some(variant), refs, ty);
                                    quote! { #name: #ty }
                                });
                            quote! {
                                #id {
                                    #(#fields_toks,)*
                                }
                            }
                        } else {
                            let fields_toks = con
                                .field_types
                                .handles()
                                .map(|ty| generate_type_ref(ctx, varis, Some(variant), refs, ty));
                            if con.field_types.len == 0 {
                                quote! { #id }
                            } else {
                                quote! { #id( #(#fields_toks,)* ) }
                            }
                        }
                    });
            quote! {
                #attrs
                pub enum #ty_name #args {
                    #(#cons_toks,)*
                }
            }
        }
        TypeDefinition::Newtype {
            name: _,
            constructor: _,
        } => {
            quote! { compile_error!("TODO"); }
        }
        TypeDefinition::Builtin(_builtin_type) => {
            quote! { compile_error!("TODO"); }
        }
        TypeDefinition::Synonym(handle) => {
            let ty = generate_type_ref(ctx, varis, Some(variant), refs, *handle);

            quote! { #attrs pub type #ty_name #args = #ty; }
        }
    }
}

fn generate_repr(ctx: &IrCtx, desc: &TypeDescription) -> TokenStream {
    match &desc.definition {
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len == 0 => quote! {},
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len == 1 => quote! { #[repr(C)] },
        TypeDefinition::DataType {
            names: _,
            constructors,
        } => {
            let fieldless = constructors
                .handles()
                .map(|handle| &ctx.type_constructors[handle])
                .all(|con| con.field_types.len == 0);
            let n = po2_type(constructors.len.ilog2() as u64);
            let repr = ident(IdentType::Raw, format!("u{n}"));

            if fieldless {
                quote! { #[repr(#repr) ]}
            } else {
                quote! { #[repr(C, #repr) ]}
            }
        }
        TypeDefinition::Newtype {
            name: _,
            constructor: _,
        } => {
            quote! { #[repr(transparent)] }
        }
        TypeDefinition::Builtin(_builtin_type) => quote! {},
        TypeDefinition::Synonym(_handle) => quote! {},
    }
}

/// Types referenced during code generation.
pub struct TypeReferences {
    pub references: BTreeSet<Handle<TypeName>>,
    pub use_bitvec: bool,
    pub use_index: bool,
    pub use_signed: bool,
    pub use_unsigned: bool,
    pub use_mask: bool,
}

/// Generate a type reference, using monomorphisation variants if needed.
pub fn generate_type_ref(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    variant: Option<&TypeRefVariant>,
    refs: &mut TypeReferences,
    ty: Handle<TypeRef>,
) -> TokenStream {
    let handle = lookup_sub(variant, ty);
    match &ctx.type_refs[handle] {
        TypeRef::BitVector(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let &TypeRef::Nat(n) = size {
                refs.use_bitvec = true;
                let n = n as usize;
                let len = proc_macro2::Literal::usize_unsuffixed(n.div_ceil(8));
                let n = proc_macro2::Literal::usize_unsuffixed(n);
                quote! { BitVector<#n, #len> }
            } else {
                quote! { compile_error!("BitVector with length not known after monomorphisation") }
            }
        }
        TypeRef::Unsigned(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let &TypeRef::Nat(n) = size {
                if n > 128 {
                    let msg = format!("Unsigned length {n} is outside of allowed range 0..=128!");
                    quote! { compile_error!(#msg) }
                } else {
                    refs.use_unsigned = true;
                    let n = (n as u8).div_ceil(8).next_power_of_two() * 8;
                    let backer = ident(IdentType::Raw, format!("u{n}")).into_token_stream();
                    let n = proc_macro2::Literal::u8_unsuffixed(n);
                    quote! { Unsigned<#n, #backer> }
                }
            } else {
                quote! { compile_error!("Unsigned with length not known after monomorphisation") }
            }
        }
        TypeRef::Signed(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let &TypeRef::Nat(n) = size {
                if n > 128 {
                    let msg = format!("Signed length {n} is outside of allowed range 0..=128!");
                    quote! { compile_error!(#msg) }
                } else {
                    refs.use_signed = true;
                    let n = (n as u8).div_ceil(8).next_power_of_two() * 8;
                    let backer = ident(IdentType::Raw, format!("i{n}")).into_token_stream();
                    let n = proc_macro2::Literal::u8_unsuffixed(n);
                    quote! { Signed<#n, #backer> }
                }
            } else {
                quote! { compile_error!("Signed with length not known after monomorphisation") }
            }
        }
        TypeRef::Index(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                refs.use_index = true;
                let n = *n as u128;
                let backer =
                    ident(IdentType::Raw, format!("u{}", index_size(n))).into_token_stream();
                let n = proc_macro2::Literal::u128_unsuffixed(n);
                quote! { Index<#n, #backer> }
            } else {
                quote! { compile_error!("Index with length not known after monomorphisation") }
            }
        }
        TypeRef::Mask(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let &TypeRef::Nat(n) = size {
                refs.use_mask = true;
                let n = n as usize;
                let len = proc_macro2::Literal::usize_unsuffixed(n.div_ceil(8));
                let n = proc_macro2::Literal::usize_unsuffixed(n);
                quote! { Mask<#n, #len> }
            } else {
                quote! { compile_error!("Mask with length not known after monomorphisation") }
            }
        }
        TypeRef::Bool => quote! { bool },
        TypeRef::Float => quote! { f32 },
        TypeRef::Double => quote! { f64 },
        TypeRef::Vector(len, inner) => {
            let len = generate_type_ref(ctx, varis, variant, refs, *len);
            let inner = generate_type_ref(ctx, varis, variant, refs, *inner);
            quote! { [#inner; #len] }
        }
        TypeRef::Tuple(handle_range) => {
            let tys = handle_range
                .handles()
                .map(|ty| generate_type_ref(ctx, varis, variant, refs, ty));
            quote! {
                ( #(#tys,)*)
            }
        }
        TypeRef::Variable(handle) => {
            let name = &ctx.identifiers[*handle];
            let name_ident = ident(IdentType::TypeVariable, name);
            quote! { #name_ident }
        }
        TypeRef::Nat(n) => {
            let n = Literal::u64_unsuffixed(*n);
            quote! { #n }
        }
        TypeRef::Reference { name, args } => {
            refs.references.insert(*name);
            let mono_ref = if let Some(var) = variant {
                var.monomorph_type_ref_substitutions.get(&handle)
            } else {
                None
            };
            let mono_ref = mono_ref.or_else(|| varis.type_refs.get(&handle));

            if let Some(mono_ref) = mono_ref.copied() {
                let mono_variant = &varis.type_ref_variants[mono_ref];
                let (name, _has_mono_arg) = mono_variant_name(ctx, mono_variant);
                let any_args = mono_variant
                    .argument_mono_values
                    .iter()
                    .any(|s| s.is_none());
                let args = args
                    .handles()
                    .zip(&mono_variant.argument_mono_values)
                    .filter_map(|(arg, arg_val)| {
                        if let Some(_val) = arg_val {
                            None
                        } else {
                            Some(arg)
                        }
                    })
                    .map(|arg| generate_type_ref(ctx, varis, variant, refs, arg));
                let args = if any_args {
                    quote! { < #(#args, )* > }
                } else {
                    quote! {}
                };
                quote! {
                    #name #args
                }
            } else {
                let ty_ident = ident(IdentType::Type, &ctx.type_names[*name].base);
                let args = if args.len > 0 {
                    let args = args
                        .handles()
                        .map(|arg| generate_type_ref(ctx, varis, variant, refs, arg));
                    quote! {
                        < #(#args,)* >
                    }
                } else {
                    quote! {}
                };

                quote! {
                    #ty_ident #args
                }
            }
        }
    }
}

fn index_size(n: u128) -> u32 {
    (u128::BITS - (n - 1).leading_zeros())
        .next_power_of_two()
        .max(8)
}
