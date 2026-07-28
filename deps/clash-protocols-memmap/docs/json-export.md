<!--
SPDX-FileCopyrightText: 2026 Google LLC

SPDX-License-Identifier: CC0-1.0
-->

# JSON Representation of memory maps

The code JSON export of memory maps can be seen in the module [`Protocols.MemoryMap.Json`](../clash-protocols-memmap/src/Protocols/MemoryMap/Json.hs).

## Top-level object

The result is an object with 3 or 4 keys.

The keys are

- `"devices"`, **object**, containing the definitions for any devices present in the memory map
- `"types"`, **object**, containing the definitions for any types used device registers in the memory map
- `"tree"`, **object**, the actual concrete device instances and interconnects for all components in the memory map

and depending on the export settings

- `"src_locations"`, **array**, information about where a component in the tree, a device or register definition or other elements are defined or referenced in the source code. Source locations could be stored "in-line" or in this separate array. Sometimes it's necessary to manually look at the generated JSON to find or check some information. Since each source location takes quite a bit of space and there are a lot of them, they can drown out the "interesting" information. During generation of the JSON, there is an option to keep source location information "in-line", right next to the thing it's referring to, or "separate", in which case all the source locations go into this array. This means that the whole array can be either skipped or, given the editor supports this, folded away.

The top-level object looks like this:
```json
{
  "devices": {...},
  "types": {...},
  "tree": {...},
  // optionally
  "src_locations": [...]
}
```

## `"devices"` object

Device names are unique within a memory map. The keys in this object are the names of the objects being described.

Associated to the keys are objects describing the device.
Those objects have the following keys:

- `"name"`, **string**, the same as the key of the `"devices"` object
- `"description"`, **string**, description of the device, empty if not provided in the source code
- `"src_location"`, **num** or **object**, the location in the source code where the device is being defined, either an object encoding the information directly or an index into the source location storage array
- `"register"`, **array**, registers of the device
- `"tags"`, **array** of **string**s

### `"register"` array

Each register of a device has data describing various parts of it stored in an object.

Such a register description object has the following keys:

- `"name"`, **string**, the name of the register
- `"description"`, **string**, as provided in the source code
- `"src_location"`, **num** or **object**, the location in the source code where the device is being defined, either an object encoding the information directly or an index into the source location storage array
- `"access"`, **string**, either `"read_only"`, `"read_write"`, `"write_only"`
- `"address"`, **num**, address of the register relative to the device base address
- `"type"`, **object**, describing a type reference, see the next section for more detail on the structure
- `"size"`, **num**, size of the register in bytes"
- `"reset"`, **currently not used, is a num but this will change in the future**
- `"tags"`, **array** of **string**


## `"types"` object

Type names are unique within a memory map. The keys in this object are the names of the objects being described.

Associated to the keys are objects describing the type.
Those objects have the following keys:

- `"name"`, **object**, name with module/package information
- `"type_args"`, **array** of **objects**, describing the kind of type parameters this type has, see the sub-section on type parameters
- `"definition"`, **object**, see the next section

The definition of a type is given as an object with a key identifying which kind of definition was used:

- `"builtin"`, **string**, either
  - `"bitvector"`
  - `"vector"`
  - `"bool"`
  - `"float"`
  - `"double"`
  - `"signed"`
  - `"unsigned"`
  - `"index"`
- `"type_synonym"`, **object**, describing a type reference, see the section [representing type references](#representing-type-references) for more detail on the structure
- `"newtype"`, **object**, describing a single constructor, see next section
- `"datatype"`, **array** of **object**s, describing constructors

### constructors

Constructors can be either "nameless" (meaning the fields have no names) or a "record" (meaning all fields have names) and both cases are represented with objects.

In the "nameless" case, the object has the following keys:

- `"name"`, **string**, the name of the constructor
- `"nameless"`, **array** of **objects**, representing [type references](#representing-type-references)

In the "record" case, the object has the following keys:

- `"name"`, **string**, the name of the constructor
- `"record"`, **array** of **object**s, where each object in the array represents a field
  - `"fieldname"`, **string**, name of the field
  - `"type"`, **object**, a [type reference](#representing-type-references)

### type parameters

Type parameters can either be type arguments or type-level naturals and have a name.

They are represented by an object with the following keys:

- `"name"`, **string**
- `"kind"`, **string**, either `"type"` or `"number"`

### representing type references

A type reference can be either a type instantiation, a variable reference, a natural numeric literal or an n-tuple of more type references.
Each of those options are represented using an object.

In the case of a type instantiation, the object has the following keys:

- `"type_reference"`, **object**, name with module/package information
- `"args"`, **array** of **objects**, more type reference objects

A variable reference is represented with the following keys:

- `"variable"`, **string**, name of the variable

A natural number literal is represented with the following keys:

- `"nat"`, **num**, literal value

Tuples are represented with the following keys:

- `"tuple"`, **arra** of **objects**, type references for the elements of the tuple


## `"tree"` object

The "tree" describes the components of the design. There are two types of tree elements, **interconnects** and **device instances**.

A device instance is an instantiation of a device (the definition of which can be found in the top-level `"devices"` object) at a specific address.

Such a device instance is represented with an object with the following keys:

- `"device_instance"`, **object**, information about the device instance with the following keys:
  - `"path"`, **array**, see [component paths](#component-paths)
  - `"device_name"`, **string**, name of the device, can be used as a key in the top-level `"devices"` object
  - `"absolute_address"`, **num**, absolute address of the component in the address space
  - `"tags"`, **array** of **object**s, see [tags with source locations](#tags-with-source-location)
  - `"src_location"`, **num** or **object**, the location in the source code where the device is being defined, either an object encoding the information directly or an index into the source location storage array
- `"interconnect"`, **object**, information about an interconnect with the following keys:
  - `"path"`, **array**, see [component paths](#component-paths)
  - `"absolute_address"`, **num**, absolute address of the component in the address space
  - `"components"`, **array** of **objects** with the following keys:
    - `"relative_address"`, **number**, relative address in the current interconnect
    - `"tree"`, **object**, a nested tree
  - `"src_location"`, **num** or **object**, the location in the source code where the device is being defined, either an object encoding the information directly or an index into the source location storage array
  - `"tags"`, **array** of **object**s, see [tags with source locations](#tags-with-source-location)

### component paths

A component path is a unique identifer for a component in the tree. A path consists of one or more path components. Path components can be either named (given a name in the hardware description) or unnamed (given by an increasing number).

Thus, a component path is an **array**, and each element of the array is either a **number** (unnamed) or an **object** with a `"name"` key containing a **string** and a `"src_location"` key, either containing a **number** or a [source location **object**](#source-locations)


An example `[0, {"name": "test", "src_location": 1}, 1]`

### tags with source locations

Tags inside the tree also have source location information, so they are represented as an **object** with two keys

- `"tag"`, **string**, tag as present in the source code
- `"src_location"`, either **num** or **object**, the location in the source code where the device is being defined, either an object encoding the information directly or an index into the source location storage array

## Source locations

Source locations, or rather, source code *spans* (with a start and an end), are represented using an object with the following keys:

- `"package"`, **string**, the Haskell/Clash package name
- `"module"`, **string**, the name of the module inside the package
- `"file"`, **string**, the path to the source file
- `"start_line"`, **num**, starting line of the span
- `"start_col"`, **num**, starting column of the span
- `"end_line"`, **num**, ending line of the span
- `"end_col"`, **num**, ending column of the span
