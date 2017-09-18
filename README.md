![Aesthetic Integration](https://storage.googleapis.com/imandra-assets/images/docs/iml_cme_mdp_model.svg)
---
## Overview

Generating test cases JSON files using `imandra-analyser`
```bash
imandra-analyser-cli strategy.yaml | python extract_packets.py generatedJsons
```

## Simple Binary Encoding  
The Simple Binary Encoding (SBE) is a binary encoding format for
serialization/deserialization of a sequence of hierachically structured
messages in a stream of raw binary data.  

In the SBE framework, one first defines an *XML schema* -- an XML description
the binary layout of the messages.  Typicaly, a special codegenerating tool
then reads this XML file, and produces a code (usually `Java` or `C++`) that
contains the message type declaraions and read/write routines for them. The
generated code can then be linked with the business logic software.  

In this chapter I'll describe such a code-generation tool fori the OCaml
programming language.
 
## SBE types codegeneration 

At the root of it, SBE relies on a small number of common types:
ASCII-characters, signed or unsigned integers of various sizes and IEE754
floating-point numbers. 
    
In the XML schema any such 'primitive' type might be augmented by one of the
following modifiers:

- It can have a `nullValue`, which denotes the null or none value for the
      field of that type. 
- It can have an associated `length` value, meaning that the given field is
      a sequence of values of that type. 
- It can have a `constant`  "presence" -- in that case the corresponding
      field is never read or written to the binary stream.  Instead, the field
      is always equal to a constant value, provided in the XML file.

The following conversions between the SBE simple types and their modifiers is
used.

| SBE Type    | OCaml type  |
| ----------- | ----------- |
| int8        | int         |
| int16       | int         |
| int32       | Int32.t     |
| int64       | Int63.t     |
| char        | char        |


| SBE type modifier | OCaml parametrized type |
| ----------------- | ----------------------- |
| length            | 'a list                 |
| nullValue         | 'a option               |

Three kinds of "complex" types can then be construced based on the primitive
types described above:

- The `composite` type is a sequence (a record) of fields of various types.  
- The `enum` represents a number of mutually-exclusive cases. Each case
      encoded with a constant `case-id`, provided in the XML schema.
- The `set` is a collection of boolean fields, packed into a single bit
      field.  

For `enum` and `set` types, an `encodingType` name must be provided.

The composite types are represented as OCaml record-types with
each record entry having the correstponding primitive type. 

```xml
<composite name="FLOAT">
    <type name="mantissa" primitiveType="int64"/>
    <type name="exponent" primitiveType="int"/>
</composite>
```
```ocaml
type t_FLOAT = {
     f_FLOAT_mantissa : int64;
     f_FLOAT_exponent : int
}
```

The enum types are represented as OCaml variant-types (2a) with each variant
case beaing a constant.

```xml
<enum name="LegSide" encodingType="uInt8">
    <validValue name="BuySide" >1</validValue>
    <validValue name="SellSide">2</validValue>
</enum>
```
```ocaml
type t_LegSide =
     | V_LegSide_BuySide
     | V_LegSide_SellSide
 ```

If the enum type has a nullable encodingType, then one extra case is added to the variant.  

```xml
<enum name="AggressorSide" encodingType="uInt8NULL">
    <validValue name="NoAggressor">0</validValue>
    <validValue name="Buy">1</validValue>
    <validValue name="Sell">2</validValue>
</enum>
```

```ocaml
type t_AggressorSide =
    | V_AggressorSide_NoAggressor
    | V_AggressorSide_Buy
    | V_AggressorSide_Sell
    | V_AggressorSide_Null
```

Finally, the set types are treated as records, but with all entries
being of the boolean type.

```xml
<set name="SettlPriceType" encodingType="uInt8">
    <choice name="Final">0</choice>
    <choice name="Actual">1</choice>
</set>
```

```ocaml
type t_SettlPriceType = {
    r_SettlPriceType_Final : bool;
    r_SettlPriceType_Actual : bool;
}
```
In the XML schema, the declaration of all the necessary simple and complex
types is folowed by the declaration of various messages. Each message contains
a block of fields that are always present in the message, followed by a number
of variable-sized groups. Each group is stored as a sequence of repeated
blocks, with each block containing the same fixed number of fields.

## The `cme_codegen` tool

The OCaml codegenerator takes as an input the XML schema file (set with `-i`
flag) and writes three files into a specified directory (set with `-d` flag)

```bash
$ _build/default/src-codegenerator/cme_codegen.bc -i templates.xml -d outputdir
$ ls outputdir
message_types.ml  readers.ml  writers.ml
```

The `message_types.ml` file contains all the OCaml type declarations. At the
very bottom of the file, the `message` type is declared -- it encompasses all
the messages in a single variant type.

The `readers.ml` and `writers.ml` files contain the reading and writing
routines for various types, for individual messages and for the top-level
`Message_types.message`. 


