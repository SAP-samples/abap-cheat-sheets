<a name="top"></a>

# Data Types and Data Objects

- [Data Types and Data Objects](#data-types-and-data-objects)
  - [Introduction](#introduction)
  - [Data Types and Objects: Definition](#data-types-and-objects-definition)
  - [ABAP Data Types](#abap-data-types)
    - [Elementary Data Types](#elementary-data-types)
    - [Complex Data Types](#complex-data-types)
    - [Reference Types](#reference-types)
    - [Declaring Data Types](#declaring-data-types)
    - [Generic Types](#generic-types)
    - [Global Data Types](#global-data-types)
  - [Data Objects](#data-objects)
    - [Declaring Data Objects](#declaring-data-objects)
    - [Assigning Values to Data Objects](#assigning-values-to-data-objects)
      - [Conversion Rules when Assigning Elementary Data Objects](#conversion-rules-when-assigning-elementary-data-objects)
    - [Creating Data Objects Using Inline Declaration](#creating-data-objects-using-inline-declaration)
    - [Assigning References to Data Reference Variables](#assigning-references-to-data-reference-variables)
    - [Creating Anonymous Data Objects](#creating-anonymous-data-objects)
    - [Constants and Immutable Variables](#constants-and-immutable-variables)
    - [Built-In Data Objects](#built-in-data-objects)
  - [ABAP Enumerated Types and Objects](#abap-enumerated-types-and-objects)
  - [Notes in a Nutshell](#notes-in-a-nutshell)
    - [Type Conversions, Compatibility and Assignments](#type-conversions-compatibility-and-assignments)
    - [Terms Related to Data Types and Objects](#terms-related-to-data-types-and-objects)
    - [Additions for Creating Data Types and Objects](#additions-for-creating-data-types-and-objects)
    - [Declaration Context of Data Types](#declaration-context-of-data-types)
  - [Excursions](#excursions)
    - [Getting Type Information and Creating Types/Data Objects at Runtime](#getting-type-information-and-creating-typesdata-objects-at-runtime)
    - [Ranges Tables](#ranges-tables)
    - [Typed Literals in ABAP SQL](#typed-literals-in-abap-sql)
    - [Non-Admissible Values of Literals](#non-admissible-values-of-literals)
  - [Executable Example](#executable-example)

## Introduction
[ABAP statements](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_statement_glosry.htm) usually work with [data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm), that is, with transient data that occupies memory space while the [data type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_type_glosry.htm) defines the technical properties of the data objects.
Since data types and data objects are closely related, this cheat sheet covers both topics. 
Note that the topics covered here are also partly covered in other ABAP cheat sheets. The purpose of this cheat sheet is to summarize the basics.

## Data Types and Objects: Definition

Data types
- Define technical properties of all data objects that have these data types, such as the maximum length of a [text field](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentext_field_glosry.htm). 
- Are descriptions only, with no data memory attached except for administrative information. 
- Can occur in [ABAP programs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm) as [bound data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbound_data_type_glosry.htm), that is, the type is a property of a data object, or as a [standalone data type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstand-alone_data_type_glosry.htm), that is, the data type is defined independently. 
- Can be defined locally in an ABAP program or globally in classes, interfaces and in the [ABAP Dictionary (DDIC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_dictionary_glosry.htm). 

Data objects: 
- Are objects (or [instances](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_glosry.htm)) of a data type (similar to objects/instances of classes in [ABAP Objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_objects_glosry.htm)).
- Occupy memory space and exist in different forms, for example, numeric or textual data can be contained in data objects.
- The type of data that a data object can receive is determined by its data type.
- Like data types, their existence and visibility depend on the declaration context.
- Are usually used in [ABAP statements](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_statement_glosry.htm) by specifying them in the [operand position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm).


> [!NOTE]
> There are several differentations that further distinguish and characterize data types and objects. See [here](#terms-related-to-data-types-and-objects-in-a-nutshell).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## ABAP Data Types

ABAP is rich in built-in data types and offers a wide range of options for defining data types and data objects in different contexts.
Data types can be divided into three groups: 
- [Elementary data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenelementary_data_type_glosry.htm)
- [Complex data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplex_data_type_glosry.htm)
- [Reference type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_type_glosry.htm)

For an overview, see the [ABAP Type Hierarchy](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_hierarchy.htm) in the ABAP Keyword Documentation.

### Elementary Data Types
- Elementary (or scalar) data types are based directly on a set of [built-in ABAP types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_abap_type_glosry.htm).
- Are not composed of other data types. 
- Are types for holding numeric values, text information, binary data and special types for date and time.
- Are further divided into elementary types of fixed and variable length.
  - Note: The length and the memory requirements of data objects of fixed length data types are fixed, that is, they cannot change at runtime. The length and memory requirements of data objects of variable length data types can actually change at runtime, depending on their contents.
- The following built-in elementary data types of fixed length are available:
  - Numeric types: Integers (`b`, `s`, `i`, `int8`), [decimal floating point numbers](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendecfloat_glosry.htm) (`decfloat16`, `decfloat34`), [binary floating point numbers](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbinfloat_glosry.htm) (`f`), and [packed numbers](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpacked_number_glosry.htm) (`p`)
  - Character-like types: text fields (`c`) and [numeric text fields](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennumeric_text_field_glosry.htm) (`n`)
  - Byte-like type: [byte fields](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbyte_field_glosry.htm) (`x`)
  - Character-like date and time types: date fields (`d`) and time fields (`t`)
  - Time stamp type for time stamp fields (`utclong`).
- Variable length:
  - Character-like type for [text strings](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentext_string_glosry.htm) (`string`)
  - Byte-like type for [byte strings](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbyte_string_glosry.htm) (`xstring`)

> [!NOTE]
> - The data types `c`, `n`, `x`, and `p` are incomplete, i.e., [generic data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengeneric_data_type_glosry.htm), with respect to their length. The type definition syntax has a special addition for this (`LENGTH`). In addition, `p` is also generic with respect to the number of decimal places (`DECIMALS` addition). See more about generic types in the following sections.
> - The other types can be considered as [complete data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplete_data_type_glosry.htm). They don't need any additional syntax elements for the definition.
> - The numeric data types `b` and `s` cannot be specified directly in ABAP programs for short integers. Alternative [built-in DDIC types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_ddic_type_glosry.htm) are available.
> - `decfloat16` and `decfloat34` for decimal floating point numbers can be regarded as more modern versions of `p` and `f`, combining their advantages.
> - Although they are character-like, `t` and `d` can be used for calculations.
> - See the ABAP Keyword Documentation [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuilt_in_types.htm) for more information about the initial values of the data types, the standard length, and so on.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Complex Data Types

- Are composed of other types. 
- The following complex data types are available: 
   - [Structured types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstructured_type_glosry.htm): Represent a sequence of arbitrary data types (i.e., they can be elementary, reference, or complex data types). The typical syntax element for the local definition of a structure is `... BEGIN OF ... END OF ...`.
   - [Table types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_type_glosry.htm): Consist of a sequence of any number of lines of the same data type. It can be any elementary type, reference type, or complex data type. The type definition includes other properties such as the [table category](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_category_glosry.htm) (defines how tables can be accessed) and [table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_key_glosry.htm) (to identify the table lines). The typical syntax element is `... TABLE OF ...`.
   - [Enumerated types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenum_type_glosry.htm): Specify a set of values in addition to the actual type properties. The typical syntax element is `... BEGIN OF ENUM ... END OF ENUM ...`. See more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenumerated_types_usage.htm) and further down.  
   - [Mesh types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmesh_type_glosry.htm): Special structured type that contains only table types with structured line types as components that can be linked using mesh associations. The typical syntax element is `... BEGIN OF MESH ... END OF MESH ...`. See more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes_mesh.htm).  
   - [BDEF derived types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_derived_type_glosry.htm): RAP-specific structured and table types. The typical syntax elements are `... TYPE STRUCTURE FOR ...` and `... TYPE TABLE FOR ...`. More information can be found [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrpm_derived_types.htm) and in the ABAP cheat sheet on ABAP EML.
- A data object of a complex type can be accessed as a whole or by component. 

> [!NOTE]
> Structured and table types are used in this cheat sheet as examples for complex types. For more information, see the ABAP Keyword Documentation and the ABAP cheat sheets for structures and internal tables.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Reference Types
- Describe data objects that contain [references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_glosry.htm) to other objects (data objects and instances of classes), which are known as [reference variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_variable_glosry.htm). 
- There are two kinds of references: [Data references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_glosry.htm) (references to data objects) and [object references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_reference_glosry.htm) (references to objects as instances of classes). 
- A reference type must be defined either in the ABAP program or in the ABAP Dictionary. There are no built-in reference types in ABAP. 
- The typical syntax element is `... REF TO ...`.

> [!NOTE]
> There are [generic ABAP types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengeneric_abap_type_glosry.htm). Generic data types are types that do not define all of the properties of a data object. They can only be used for the typing of [formal parameters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenformal_parameter_glosry.htm) and [field symbols](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm). See an example further down.
The only generic types that can be used after `TYPE REF TO` are `data` for the generic typing of data references, and `object`, for the generic typing of object references.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Declaring Data Types

Data types are defined in an ABAP program using the [`TYPES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes.htm) statement. Depending on the declaration context, the types can be accessed either locally or globally. As mentioned, data types can also be declared in the ABAP Dictionary. The focus here is on data type declarations in ABAP programs using `TYPES` statements.
You can use built-in and user-defined data types to create data types (and data objects). 

The following code snippet shows various syntax options for declaring data types:

Elementary types:

```abap
"Type declaration follows these patterns (LENGTH and DECIMALS only possible for particular types):
"TYPES data_type TYPE some_type [LENGTH len] [DECIMALS dec].

"Data type declarations based on built-in ABAP types

"Numeric types
TYPES te_i TYPE i.
TYPES te_int8 TYPE int8.
TYPES te_decfl16 TYPE decfloat16.
TYPES te_decfl34 TYPE decfloat34.
TYPES te_f TYPE f.
TYPES te_p_l4_d2 TYPE p LENGTH 4 DECIMALS 2.
"Note: LENGTH/DECIMALS must be specified when using the types c, p, n, x. in ABAP Objects contexts.

"Character-like types
"To combine TYPES statements, you can use chained statements, i.e. TYPES followed by a colon and 
"then listing the type declarations separated by a comma.
TYPES: te_c5  TYPE c LENGTH 5,
       te_n4  TYPE n LENGTH 4,
       te_str TYPE string.

"Byte-like types
TYPES te_x_l2 TYPE x LENGTH 2.
TYPES te_xstr TYPE xstring.

"Types for date and time
TYPES te_d TYPE d.
TYPES te_t TYPE t.
TYPES te_utc TYPE utclong.

"You might also stumble on a length specification in parentheses following the data type name.
"It is recommended that you use addition LENGTH instead of the parentheses.
TYPES te_cfour(4) TYPE c.
TYPES te_nfive(5) TYPE n.
TYPES te_xtwo(2) TYPE x.

"**** Data type declarations based on existing types or data objects ****

"Type declaration based on an existing type visible at this location;
"all properties of the specified data type are inherited.
TYPES te_another_i TYPE te_i.

"Anticipating the data object declaration needed to demonstrate the LIKE addition
DATA do_num TYPE i.

"LIKE addition:
"Type declaration based on an existing data object visible at this location;
"all properties of the type of the specified data object are inherited.
TYPES te_from_int LIKE do_num.

"**** Data type declarations based on globally available types or data objects ****

"DDIC Types
"Note that the built-in types b and s cannot be specified for type declarations.
"However, the value range for these types can be obtained by referencing the built-in DDIC
"types INT1 and INT2. These are data elements. 
TYPES te_int1 TYPE int1.
TYPES te_int2 TYPE int2.

"Referring to types in global classes
"In the example, the type exists in a global interface.
TYPES te_elem_from_itf TYPE zdemo_abap_get_data_itf=>occ_rate.

"Referring to a data object that exists in a global interface
TYPES te_dobj_from_itf LIKE zdemo_abap_objects_interface=>stat_str.

"Referring to a data object that exists in the public visibility section of a global class
TYPES te_dobj_from_cl LIKE zcl_demo_abap_objects=>public_string.

"Referring to a component of a DDIC table (also possible for views;
"the components have elementary types)
TYPES te_comp_ddic_tab TYPE zdemo_abap_carr-carrid.

"Type pools (ABAP program, administrated by the ABAP Dictionary; may only be created in
"standard ABAP; but is considered obsolete)
"However, the following example is accessible in ABAP for Cloud Development, too.
"The type pool contains the definitions of globally visible data types and constants.
TYPES te_tp TYPE abap_bool.
TYPES te_const_in_tp LIKE abap_true.
```

Structure and internal table types as examples for complex types:

```abap
"Structure type, can contain any type
TYPES: BEGIN OF ts_misc_comps,
          comp1 TYPE i,
          comp2 TYPE string,
          comp3 TYPE te_i,                                    "Existing type
          comp4 LIKE do_num,                                  "Referring to existing data object
          comp5 TYPE string_table,                            "Internal table type (available in DDIC)
          comp6 TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY, "Internal table type (based on database table)
          comp7 TYPE REF TO i,                                "Reference type
        END OF ts_misc_comps.

"Internal table types
"Note: The examples only use the implicit STANDARD for standard tables.
"Internal table type declaration based on a local structure type
TYPES tt_local_ts TYPE TABLE OF ts_misc_comps WITH EMPTY KEY.

"Internal table type declaration based on an elementary data type
TYPES tt_int TYPE TABLE OF i.

"Referring to existing types and data objects

"Anticipating the creation of structured data objects for the LIKE addition
DATA struc_local_ts TYPE ts_misc_comps.

"Structure type creation based on an existing structured data object
TYPES ts_w_like LIKE struc_local_ts.

"Anticipating the creation of an internal table for the LIKE addition
DATA itab_local_ts TYPE TABLE OF ts_misc_comps WITH EMPTY KEY.

"Internal table type declaration based on an existing internal table
TYPES tt_w_like LIKE itab_local_ts.

"Internal table type declaration based on the existing internal table type
TYPES tt_another_type TYPE tt_w_like.

"Structured types based on an internal table's line type
TYPES ts_type_line TYPE LINE OF tt_w_like.
TYPES ts_like_line LIKE LINE OF itab_local_ts.

"Internal table typed with internal table as line type
TYPES tt_like_table LIKE TABLE OF itab_local_ts.

"Referring to global types

"Structure type based on DDIC type
"In this case, a database table is specified whose line type is used as data type
"in this type declaration. You may also use a CDS view (or classic DDIC view in
"standard ABAP) or a dedicated structured type defined in the DDIC.
TYPES ts_ddic_tab TYPE zdemo_abap_carr.

"Internal table type based on internal type that exists in a gloabl interface
TYPES tt_tab_type_from_itf TYPE zdemo_abap_get_data_itf=>carr_tab.

"Internal table types with an elementary line type based on globally available types
"Elementary table type
TYPES tt_strtab TYPE string_table.
"Elementary line type; the type is available in a global interface
TYPES tt_elem_type_from_itf TYPE TABLE OF zdemo_abap_get_data_itf=>occ_rate.
```

Reference types:
```abap
"Declaring reference types with static types
TYPES tr_i TYPE REF TO i.
TYPES tr_str TYPE REF TO string.
TYPES tr_ddic_tab TYPE REF TO zdemo_abap_carr.
"Using the generic type data as static type
TYPES tr_data TYPE REF TO data.

"Referring to an existing reference type
TYPES tr_ref_i TYPE tr_i.

"Anticipating the creation of a data reference variable for showing the LIKE addition
DATA dref_i TYPE REF TO i.

"Creating a reference type based on a data reference variable
TYPES tr_like_ref_i LIKE dref_i.

"Creating a data object for the LIKE REF TO addition
DATA str TYPE string.

"Creating a reference type whose static type is inherited from the data type of the specified data object
TYPES tr_like_ref2str LIKE REF TO str.

"Reference table types
TYPES tr_tab_ref_i TYPE TABLE OF REF TO i.
DATA itab_str TYPE TABLE OF string.
TYPES tr_like_table_ref LIKE TABLE OF REF TO itab_str.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Generic Types

- Generic types are available with which [formal parameters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenformal_parameter_glosry.htm) of methods or [field symbols](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm) can be specified.
- At runtime, the actual data type is copied from the assigned [actual parameter](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenactual_parameter_glosry.htm) or
  memory area, i.e. they receive the complete data type only when an actual parameter
  is passed or a memory area is assigned.
- More information: 
  - [Generic ABAP Types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuilt_in_types_generic.htm)
  - [ABAP cheat sheet about dynamic programming](06_Dynamic_Programming.md) regarding field symbols and `ASSIGN` statements

> [!NOTE]
> The `TYPE REF TO` addition types as a reference variable. A generic type cannot be specified after `REF TO`. A typing with `TYPE REF TO data` and `TYPE REF TO object` is considered as completely typing.


The following code snippet demonstrates generic types with field symbols. For complete and generic typing of formal parameters, refer to the [ABAP Object Orientation](04_ABAP_Object_Orientation.md#complete-typing-of-formal-parameters) cheat sheet.

```abap
FIELD-SYMBOLS:
  "Any data type
  <data>           TYPE data,
  <any>            TYPE any,
  "Any data type can be assigned. Restrictions for formal parameters and 'data': no
  "numeric functions, no description functions, and no arithmetic expressions can be
  "passed to these parameters. However, you can bypass the restriction by applying the
  "CONV operator for the actual parameter.

  "Character-like types
  <c>              TYPE c,         "Text field with a generic length
  <clike>          TYPE clike,     "Character-like (c, n, string, d, t and character-like flat structures)
  <csequence>      TYPE csequence, "Text-like (c, string)
  <n>              TYPE n,         "Numeric text with generic length
  <x>              TYPE x,         "Byte field with generic length
  <xsequence>      TYPE xsequence, "Byte-like (x, xstring)

  "Numeric types
  <decfloat>       TYPE decfloat, "decfloat16, decfloat34
  <numeric>        TYPE numeric,  "Numeric ((b, s), i, int8, p, decfloat16, decfloat34, f)
  <p>              TYPE p,        "Packed number (generic length and number of decimal places)

  "Internal table types
  <any_table>      TYPE ANY TABLE,      "Internal table with any table type
  <hashed_table>   TYPE HASHED TABLE,
  <index_table>    TYPE INDEX TABLE,
  <sorted_table>   TYPE SORTED TABLE,
  <standard_table> TYPE STANDARD TABLE,
  <table>          TYPE table,          "Standard table

  "Other types
  <simple>         TYPE simple. "Elementary data type including enumerated types and
                                "structured types with exclusively character-like flat components  

"Data objects to work with
DATA: BEGIN OF s,
        c3        TYPE c LENGTH 3,
        c10       TYPE c LENGTH 10,
        n4        TYPE n LENGTH 4,
        str       TYPE string,
        time      TYPE t,
        date      TYPE d,
        dec16     TYPE decfloat16,
        dec34     TYPE decfloat34,
        int       TYPE i,
        pl4d2     TYPE p LENGTH 4 DECIMALS 2,
        tab_std   TYPE STANDARD TABLE OF string WITH EMPTY KEY,
        tab_so    TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line,
        tab_ha    TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line,
        xl1       TYPE x LENGTH 1,
        xstr      TYPE xstring,
        structure TYPE zdemo_abap_carr, "character-like flat structure
      END OF s.

"The following static ASSIGN statements demonstrate various assignments
"Note:
"- The statements commented out show impossible assignments.
"- If a static assignment is not successful, sy-subrc is not set and no
"  memory area is assigned. Dynamic assignments, however, set the value.

"----- Any data type -----
ASSIGN s-c3 TO <data>.
ASSIGN s-time TO <data>.
ASSIGN s-tab_std TO <data>.
ASSIGN s-xstr TO <any>.
ASSIGN s-pl4d2 TO <any>.
ASSIGN s-date TO <any>.
ASSIGN s TO <any>.

"----- Character-like types -----
ASSIGN s-c3 TO <c>.
ASSIGN s-c10 TO <c>.
"ASSIGN s-str TO <c>.

ASSIGN s-c10 TO <clike>.
ASSIGN s-str TO <clike>.
ASSIGN s-n4 TO <clike>.
ASSIGN s-date TO <clike>.
ASSIGN s-time TO <clike>.
ASSIGN s-structure TO <clike>.

ASSIGN s-c10 TO <csequence>.
ASSIGN s-str TO <csequence>.
"ASSIGN s-n4 TO <csequence>.

ASSIGN s-n4 TO <n>.
"ASSIGN s-int TO <n>.
"ASSIGN s-time TO <n>.

ASSIGN s-xl1 TO <x>.
"ASSIGN s-xstr TO <x>.

ASSIGN s-xl1 TO <xsequence>.
ASSIGN s-xstr TO <xsequence>.

"----- Numeric types -----
ASSIGN s-dec16 TO <numeric>.
ASSIGN s-dec34 TO <numeric>.
ASSIGN s-int TO <numeric>.
ASSIGN s-pl4d2 TO <numeric>.
"ASSIGN s-n4 TO <numeric>.

ASSIGN s-dec16 TO <decfloat>.
ASSIGN s-dec34 TO <decfloat>.

ASSIGN s-pl4d2 TO <p>.
"ASSIGN s-dec34 TO <p>.

"----- Internal table types -----
ASSIGN s-tab_std TO <any_table>.
ASSIGN s-tab_so TO <any_table>.
ASSIGN s-tab_ha TO <any_table>.

ASSIGN s-tab_std TO <index_table>.
ASSIGN s-tab_so TO <index_table>.
"ASSIGN s-tab_ha TO <index_table>.

"ASSIGN s-tab_std TO <sorted_table>.
ASSIGN s-tab_so TO <sorted_table>.
"ASSIGN s-tab_ha TO <sorted_table>.

ASSIGN s-tab_std TO <standard_table>.
ASSIGN s-tab_std TO <table>.
"ASSIGN s-tab_so TO <standard_table>.
"ASSIGN s-tab_so TO <table>.
"ASSIGN s-tab_ha TO <standard_table>.
"ASSIGN s-tab_ha TO <table>.

"ASSIGN s-tab_std TO <hashed_table>.
"ASSIGN s-tab_so TO <hashed_table>.
ASSIGN s-tab_ha TO <hashed_table>.

"----- Other types -----
ASSIGN s-c10 TO <simple>.
ASSIGN s-str TO <simple>.
ASSIGN s-dec34 TO <simple>.
ASSIGN s-date TO <simple>.
ASSIGN s-structure TO <simple>.
ASSIGN s-xl1 TO <simple>.
"ASSIGN s-tab_ha TO <simple>.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Global Data Types

- Global data types are created as [repository objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrepository_object_glosry.htm) in the ABAP Dictionary: 
  - The ABAP Dictionary (DDIC) serves as a persistent repository for type definitions represented by dictionary objects. 
  - These objects constitute global data types that are accessible by other repository objects.
  - DDIC types such as [DDIC data elements](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_element_glosry.htm) (elementary data types or reference types), [DDIC structures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_structure_glosry.htm), [DDIC table types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_table_type_glosry.htm). Find more information in the [ABAP Dictionary](26_ABAP_Dictionary.md) cheat sheet.
  - Furthermore, [database tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendatabase_table_glosry.htm) and [CDS entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_entity_glosry.htm) and their components can also be used as data types in ABAP programs. 
  - For many of the classic DDIC objects, which are still supported in [ABAP Cloud](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_cloud_glosry.htm), CDS-based successor objects are available.
  - Find a high-level overview in the [ABAP Dictionary](26_ABAP_Dictionary.md) cheat sheet.
  - Note that there are predefined global types available. However, in ABAP Cloud, only [released APIs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreleased_api_glosry.htm) can be used there.  
  - In [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm), you may stumble on the option to create global data types in [type pools](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_pool_glosry.htm), which is not possible in ABAP for Cloud Development. However, the predefined type pool `abap` can be used in ABAP for Cloud Development.
- Data types declared in interfaces and in the public visibility section of global classes are also globally visibile. Global classes and interfaces as such are global types to refer to. 
- Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_data_types.htm).

<p align="right"><a href="#top">⬆️ back to top</a></p>



## Data Objects

### Declaring Data Objects
    
The [`DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata.htm) statement declares a data object of any data type. The declaration of a data object is similar to that of a data type, with only slight differences in terms of the number of possible additions, and follows this syntax pattern:
`DATA dobj TYPE abap_type [LENGTH len] [DECIMALS dec] [VALUE val] [READ-ONLY].`

Note that `READ-ONLY` can only be used for class attributes.
The data type of a data object is always defined uniquely and cannot be changed at runtime of a program.

Data objects with elementary types:
```abap
"The elementary, built-in data types can be used as shown for data type declarations.
"Chained statements are also possible with DATA.
"Note that not all types as shown above are used here.
DATA: do_i       TYPE i,
      do_c_l5    TYPE c LENGTH 5,
      do_p_l3_d2 TYPE p LENGTH 3 DECIMALS 2,
      do_decfl16 TYPE decfloat16,
      do_str     TYPE string,
      "Specifying the length in parantheses instead of using the
      "LENGTH addition is not recommended
      do_ctwo(2) TYPE c,
      do_nthree(3) TYPE n,
      do_xfour(4) TYPE x.

"Referring to locally declared data types
TYPES te_string TYPE string.
DATA do_another_str TYPE te_string.

"Referring to other data objects
DATA do_like_dobj LIKE do_i.

"If the length is not specified explicitly for the ABAP types c, n, p, and x,
"the standard length is defined implicitly. Check the F2 information.
DATA do_c_std TYPE c.
DATA do_p_std TYPE p.

"If neither TYPE nor LIKE is specified, a data object with the bound
"data type 'c LENGTH 1' is created.
DATA do_c.

"VALUE addition
"Start values can be set for the data objects when they are declared.
"Without the addition VALUE, data objects are filled with their type-specific initial values.
"The start value can either be specified as a literal or as a predefined constant.
"Note: The VALUE addition is not to be confused with the VALUE operator that can be used to
"construct the content of complex data objects as shown below.
DATA do_c_l2 TYPE c LENGTH 2 VALUE 'hi'.
DATA do_i_val TYPE i VALUE 123.
DATA do_like_val LIKE do_i VALUE 9.

"Specifying a constant (data object that cannot be changed at runtime) after the VALUE addition
CONSTANTS con TYPE string VALUE `abcdef`.
DATA do_val_con TYPE string VALUE con.

"VALUE IS INITIAL addition: Explicitly specifying the type-specific initial value
DATA do_i_init TYPE i VALUE IS INITIAL.
DATA do_i_like_init LIKE do_i VALUE IS INITIAL.

"Data objects can also be created in the declaration part of classes and interfaces.
"There you can use the READ-ONLY addition for data object declarations in the public visibility section.
"In doing so, an attribute declared using CLASS-DATA or DATA can be read from outside of the class but
"can only be changed using methods of the class or its subclasses.
"The following attribute is taken from a executable example. It shows a read access in a control
"structure. If you wanted to assign a new value to the attribute outside of the class in which it is
"declared, a syntax error would be displayed.
"Note that when you are in the class itself, there is no need to specify the class name.
"read_only_attribute = ... would be sufficient. And changing the value would be possible within
"the class, too.
"Declaration in the example:
"CLASS-DATA: read_only_attribute TYPE string VALUE `Hallo` READ-ONLY.
IF zcl_demo_abap=>read_only_attribute = `abc`.
  ...  
ELSE.
  ...
ENDIF.
```

Structures and internal tables as examples for complex types:

```abap
"Find more examples in the ABAP cheat sheets on structures and internal tables.

"Creating a structure with DATA and providing start values with the VALUE addition.
"If not specified, then the components have their type-specific initial value.
DATA: BEGIN OF a_structure,
        comp1 TYPE i VALUE 1,
        comp2 TYPE string VALUE `hi`,
        comp3 TYPE string,
      END OF a_structure.

"Creating a structure based on a global type. In this case, it is a DDIC database
"table whose line type is used. You can also use a CDS view or a dedicated structured type
"from the DDIC, for example.
DATA struc_ddic_tab TYPE zdemo_abap_carr.

"Creating a structure as a constant. Providing values is mandatory.
CONSTANTS: BEGIN OF con_struc,
              comp1 TYPE i VALUE 1,
              comp2 TYPE string VALUE `hallo`,
              comp3 TYPE string VALUE `salut`,
            END OF con_struc.

"Using the constant as start value for a structure declaration.
DATA struc_w_val LIKE con_struc VALUE con_struc.

"Declaring a structure and explicitly specifying the type-specific
"initial values of the structure components as start values.
DATA struc_init_val LIKE con_struc VALUE IS INITIAL.

"Creating internal tables ...
"Based on a globally available DDIC database table whose line type is used
DATA itab_ddic_tab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
"Based on an elementary type
DATA itab_tab_i TYPE TABLE OF i.
"The table type is declared in a global interface
DATA itab_tab_type_from_itf TYPE zdemo_abap_get_data_itf=>carr_tab.
"Based on globally available DDIC internal table type; explicitly specifying as initial
DATA itab_ddic_tab_type TYPE string_table VALUE IS INITIAL.
"Based on locally available structured data object
DATA itab_like_struc LIKE TABLE OF struc_w_val WITH EMPTY KEY.
"Based on locally available internal table
DATA itab_like_another_itab LIKE itab_tab_i.

"Creating an internal table type locally
TYPES tt_ddic_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.
"... and an internal table based on it.
DATA itab_w_itab_type TYPE tt_ddic_tab.

"Creating a structure based on the line of an internal table (type)
DATA struc_from_itab_type TYPE LINE OF tt_ddic_tab.
DATA struc_like_line LIKE LINE OF itab_ddic_tab.
```


> [!NOTE]
> The above data objects are declared by assigning a dedicated name. These data objects can be addressed by that name. This is not true for [anonymous data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm), which can only be addressed through reference variables. This is covered [below](#assigning-references-to-data-reference-variables).


Data reference variables:

```abap
"Declaring data reference variables types with static types
DATA dref_int TYPE REF TO i.
DATA dref_str TYPE REF TO string.
DATA dref_ddic_tab TYPE REF TO zdemo_abap_carr.
"Using generic type data as static type
DATA dref_data TYPE REF TO data.

"Referring to an existing reference type
TYPES tr_int TYPE REF TO i.
DATA dref_tr_int TYPE tr_int.

"Creating a data reference variable based on a data reference variable
DATA dref_like LIKE dref_int.

"Creating a data object for the LIKE REF TO addition
DATA do_some_string TYPE string.

"Reference type is created whose static type is inherited from the data type of the specified data object
DATA dref_like_ref_str LIKE REF TO do_some_string.

"Reference tables
DATA dref_tab_i TYPE TABLE OF REF TO i.
DATA dref_tab_str LIKE TABLE OF REF TO do_some_string.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Assigning Values to Data Objects

An assignment passes the contents of a source to a target data object.

> [!NOTE]
> - There are conversion rules when assigning a source to a target data object that have different types. For more information, see the topic [Assignment and Conversion Rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rules.htm) in the ABAP Keyword Documentation, especially for complex types, since elementary types are usually demonstrated in the cheat sheet.
> - There are many ways to assigning values to data objects in ABAP. They occur in the context of various ABAP statements. Here, assignments with the assignment operator `=` are mostly used.
> - In older ABAP code, you may see `MOVE ... TO ...` statements for value assignments. These statements are obsolete. They are not to be confused with `MOVE-CORRESPONDING` statements for complex types. These are not obsolete.

The following code snippet shows several ways to assign values to data objects. 

```abap
"As mentioned, a start value can be directly assigned when declaring a data object.
DATA some_int TYPE i VALUE 123.

"Assignment using the assignement operator =
"The source of the assigment on the right side (also known as general expressions posisiton) 
"can be specified with many things.

"Single data object as source of the assignment
"In the following cases, the literal and data object have the exact type as the data 
"object on the left side. 
some_int = 456.
DATA num TYPE i.
num = some_int.

DATA str_a1 TYPE string VALUE `hallo`.
DATA str_a2 TYPE string.
str_a2 = str_a1.

"Functional method as source of the assignment
"In the following example, the method get_next of the class cl_abap_random_int
"returns an integer. Check the F2 information for get_next (return value of type i).
"A random integer that is in the specified value range is assigned to the data object
"on the left side.
num = cl_abap_random_int=>create(
        seed = cl_abap_random=>seed( ) min = 1
                                       max = 10 )->get_next( ).

"Built-in functions as source of the assignment
"There are plenty of functions available. Check the ABAP Keyword Documentation.

"Built-in numeric function
"The following built-in function calculates 2 to the power of 4. The
"result is assigned to the data object on the left side.
num = ipow( base = 2 exp = 4 ).

"Built-in string function
"The following function transforms the specified data object to upper case letters. The
"result is assigned to the data object on the left side.
str_a1 = to_upper( str_a2 ).

"Constructor expressions as source of the assignment
"There are various options and expressions available (with many additions). Check the
"ABAP Keyword Documentation and the ABAP cheat sheet about constructor expressions.
"Here, taking the VALUE operator as an example. This operator is very handy especially
"for complex types.

"Creating a structure
DATA some_struc TYPE zdemo_abap_carr.

"Assignment using the VALUE operator
"Note the # character that stands for the type. Here, the structure type can be derived
"from the context. Hence, the explicit name can but need not be specified.
some_struc = VALUE #( carrid = 'XY' carrname = 'XY Airways' ).

"Creating an internal table and assigning values
"Note that components that are not specified and assigned a value retain their
"type-specific initial value.
DATA some_itab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
some_itab = VALUE #( ( carrid = 'XY' carrname = 'XY Airways' )
                     ( carrid = 'AB' carrname = 'ABAP Airlines' ) ) .

"Table expressions as source of the assignment
"A structure is assigned an internal table line
some_struc = some_itab[ 2 ].

"Calculation expressions as source of the assignment
"Arithmetic expression
num = 1 + 2.

"A calculation assignment as follows
num += 1.
"is the short form of
num = num + 1.
"Syntax options: +=, -=, *= and /=, also &&= for the && operator

"String expressions as source of the assignment
str_a2 = str_a1 && ` blabla`. "Strings are appended using the && operator
str_a2 = |{ str_a1 } some more bla.|. "String templates. Note: Data objects are specified
                                      "in curly brackets. The content is converted to type string
                                      "Note that it must be convertible to type string.

"An elementary data object is assigned a component of a specific table line 
"using a table expression.
"Note: In the following case, the types of source and target are not the same
"(type c versus type string). As shown further down, such an assignment can lead to 
"unexpected or undesired results. Type-dependent conversions are made in accordance 
"with the conversion rules.
str_a2 = some_itab[ 2 ]-carrname.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Conversion Rules when Assigning Elementary Data Objects

- The following code examples explore and comment on assignments of data objects with various elementary types. Conversion results are added as comments in the code.
- They focus on conversion rules when assigning a source to a target data object of different elementary types.
- Not all types or conversion options are covered, and some example assignments may seem nonsensical.
- For more information, see [Assignment and Conversion Rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rules.htm) in the ABAP Keyword Documentation.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

```abap
*&---------------------------------------------------------------------*
*& Character-like source fields
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Source types: c / string
*&---------------------------------------------------------------------*

"The types c and string are demonstrated together because they have
"similar conversion rules. The type n is also similar to c.

DATA c5 TYPE c LENGTH 5.
DATA str TYPE string.
DATA error TYPE REF TO cx_root.

"Numeric target fields
"The general rule is that the source must contain a number. Various
"notations, i.e. mathematical, commercial, scientific, are possible
"depending on the target type. Otherwise, the CX_SY_CONVERSION_NO_NUMBER
"exception is raised. CX_SY_CONVERSION_OVERFLOW is raised if the number
"is not in the value range.

"--- i ---
DATA i TYPE i.

c5 = '123'.
str = `456`.
i = c5. "123
i = str. "456

"Only blanks in the source are interepreted as 0. This also applies to
"the other numeric types.
c5 = '   '.
str = ` `.
i = c5. "0
i = str. "0

c5 = '1.567'.
str = `3.499`.

"Automatic rounding
i = c5. "2
i = str. "3

c5 = 'abc'.
TRY.
    i = c5.
  CATCH cx_sy_conversion_no_number INTO error.
    DATA(error_text) = error->get_text( ). "Cannot be interpreted as a number
ENDTRY.

str = `11111111#`.
TRY.
    i = str.
  CATCH cx_sy_conversion_no_number INTO error.
    error_text = error->get_text( ). "Cannot be interpreted as a number
ENDTRY.

"Scientific numbers cannot be converted to i
DATA c30 TYPE c LENGTH 30.
c30 = '1.789E+3'.
TRY.
    i = c30.
  CATCH cx_sy_conversion_no_number INTO error.
    error_text = error->get_text( ). "Cannot be interpreted as a number
ENDTRY.

c30 = '99999999999999999999'.
TRY.
    i = c30.
  CATCH cx_sy_conversion_overflow INTO error.
    error_text = error->get_text( ). "Overflow
ENDTRY.

"Lossless assignment check
TRY.
    c5 = '1.567'.
    i = EXACT #( c5 ).
  CATCH cx_sy_conversion_rounding INTO error.
    error_text = error->get_text( ). "Conversion cannot be executed exactly, a rounding is necessary
ENDTRY.

"--- int8 ---
DATA int8 TYPE int8.

c5 = '123'.
int8 = c5. "123

"--- p ---
DATA pl8d2 TYPE p LENGTH 8 DECIMALS 2.

c5 = '123'.
str = `1`.
pl8d2 = c5. "123.00
pl8d2 = str. "1.00

c5 = '3.567'.
str = `1.43210`.
pl8d2 = c5. "3.57
pl8d2 = str. "1.43

"Lossless assignment check
TRY.
    c5 = '3.567'.
    pl8d2 = EXACT #( c5 ).
  CATCH cx_sy_conversion_rounding  INTO error.
    error_text = error->get_text( ). "Conversion cannot be executed exactly, a rounding is necessary
ENDTRY.

"--- decfloat16/decfloat34 ---
"Special conversion rules apply. Refer to the ABAP Keyword Documentation.
DATA dec16 TYPE decfloat16.
DATA dec34 TYPE decfloat34.

c5 = '1.012'.
str = `9.87`.
dec16 = c5. "1.012
dec16 = str. "9.87
dec34 = c5. "1.012
dec34 = str. "9.87
c30 = '1.12345678901234567890'.
str = `9.98765432109876543210`.
dec16 = c30. "1.123456789012346
dec16 = str. "9.987654321098765
dec34 = c30. "1.12345678901234567890
dec34 = str. "9.98765432109876543210

"Lossless assignment check
TRY.
    c30 = '1.12345678901234567890'.
    dec16 = EXACT #( c30 ).
  CATCH cx_sy_conversion_rounding  INTO error.
    error_text = error->get_text( ). "Conversion cannot be executed exactly, a rounding is necessary
ENDTRY.

"--- f ---
DATA f TYPE f.

c30 = '3.0788507711716639E-01'.
str = `7.8864684684943672E-01`.
f = c30. "3.0788507711716639E-01
f = str. "7.8864684684943676E-01

c5 = '123'.
str = `5`.
f = c5. "1.2300000000000000E+02
f = str. "5.0000000000000000E+00

"Character-like target fields
"--- c ---
"- Source is passed to the target left-aligned, including leading spaces
"  and excluding trailing spaces.
"- If the target is shorter, truncation is applied. If the target is longer,
"  spaces are padded on the right.
"- Exception: When the source is a string, trailing spaces are retained.
DATA c3 TYPE c LENGTH 3.
c5 = 'abcde'.
c3 = c5. "abc

c5 = ' a    '.
c3 = c5. "' a '

c30 = 'abcdefghijklmn'.
c5 = c30. "abcde

str = `  #  `.
c5 = str. "'  #  '

str = `xyzabc`.
c3 = str. "xyz

"Lossless assignment check
c30 = 'abcdefghijklmn'.
TRY.
    c5 = EXACT #( c30 ).
  CATCH cx_sy_conversion_data_loss  INTO error.
    error_text = error->get_text( ). "data loss occurred when converting
ENDTRY.

"--- n ---
"- Number characters are passed aligned to the right.
"- Other characters are ignored.
"- If the target is longer, 0 characters are padded. If it is shorted,
"  truncation is applied.
DATA n5 TYPE n LENGTH 5.

c3 = '12'.
n5 = c3. "00012

c5 = '3#4#5'.
n5 = c5. "00345

c30 = '1234567890'.
n5 = c30. "67890

c30 = '  123####4########'.
n5 = c30. "01234

"Lossless assignment check
c30 = '1234567890'.
TRY.
    n5 = EXACT #( c30 ).
  CATCH cx_sy_conversion_overflow INTO error.
    error_text = error->get_text( ). "Overflow
ENDTRY.

"--- string ---
"The passing is performed including leading blanks, and excluding
"trailing blanks.
DATA string TYPE string.

c5 = 'abcd'.
string = c5. "abcd

c30 = '    abc         '.
string = c30. "'    abc'

c30 = 'abc                   '.
string = c30. "abc

c30 = '      abc'.
string = c30. "'      abc'

"Byte-like target fields
"--- x ---
"- Each source character is interpreted as half a byte in hexadecimal representation.
"- Valid values are 0-9 and A-F.
"- Padding uses hexadecimal 0.
"- In case of an invalid character, the conversion stops, and padding with 0 is applied.

DATA x4 TYPE x LENGTH 4.

c5 = '12AB3'.
x4 = c5. "12AB3000

c5 = '89G'.
x4 = c5. "89000000

c5 = '1'.
x4 = c5. "10000000

"--- xstring ---
"The behavior is similar to type x.
DATA xstr TYPE xstring.

c5 = '1234'.
xstr = c5. "1234

"An odd number of the source means the remaining half-byte is
"padded with 0.
c5 = '12345'.
xstr = c5. "123450

"Data/time target fields
"--- d ---
"The conversion is applied like type c length 8.
"In case of type string as source type, trailing blanks are passed.
DATA d TYPE d.

c30 = '20250102'.
d = c30. "20250102

"Lossless assignment to check if the source has a valid date value
TRY.
    "The example date is valid.
    d = EXACT #( c30 ).
  CATCH cx_sy_conversion_no_date INTO error.
    error_text = error->get_text( ).
ENDTRY.

"Invalid date value
c30 = '202501##'.
d = c30. "202501##

"Lossless assignment to check if the source has a valid date value
TRY.
    d = EXACT #( c30 ).
  CATCH cx_sy_conversion_no_date INTO error.
    error_text = error->get_text( ). "Cannot be interpreted as a date
ENDTRY.

str = ``.
d = str. "00000000

"--- t ---
"- The conversion is applied like type c length 6.
"- Padding with 0 characters is applied if the target is longer.
"- The valid format is yyyymmdd. Otherwise, the result is an invalid time value.
DATA t TYPE t.

c30 = '123456'.
t = c30. "123456

str = `230157`.
t = str. "230157

"Invalid time value
c30 = '1234##'.
t = c30. "1234##

"Lossless assignment to check if the source has a valid time value
TRY.
    t = EXACT #( c30 ).
  CATCH cx_sy_conversion_no_time INTO error.
    error_text = error->get_text( ). "Cannot be interpreted as a time
ENDTRY.

str = `1234  `.
t = str. "1234

"--- utclong ---
"- The source must be a valid representatin of a UTC time stamp:
"  yyyy-mm-ddThh:mm:ss.fffffff
"- The CX_SY_CONVERSION_NO_DATE_TIME exception can be raised.
DATA utclong TYPE utclong.

c30 = '2025-01-01T12:34:56.111'.
utclong = c30. "2025-01-01 12:34:56.1110000

str = `2025-05-12T22:01:30.5`.
utclong = str. "2025-05-12 22:01:30.5000000

c30 = '2025-01-01 12:34:56.111'.
utclong = c30. "2025-01-01 12:34:56.1110000

"Invalid time stamps
TRY.
    c30 = '2025-01-01 99:99:99.111'.
    utclong = c30.
  CATCH cx_sy_conversion_no_date_time INTO error.
    error_text = error->get_text( ). "Invalid time stamp
ENDTRY.

TRY.
    c30 = 'ABC'.
    utclong = c30.
  CATCH cx_sy_conversion_no_date_time INTO error.
    error_text = error->get_text( ). "Invalid time stamp
ENDTRY.

*&---------------------------------------------------------------------*
*& Source type: n
*&---------------------------------------------------------------------*
"- When assigning data objects of type n to character-like data objects,
"  they function as character-like objects. The handling is similar to type c.
"- When assigning them to numeric data objects, they function numerically.

DATA n3 TYPE n LENGTH 3.

"--- i ---
n3 = '123'.
i = n3. "123

n3 = '000'.
i = n3. "0

"--- n ---
"Passing aligned to the right
DATA n2 TYPE n LENGTH 2.
n3 = '456'.
n2 = n3. "56

*&---------------------------------------------------------------------*
*& Numeric source fields
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Source type: i
*&---------------------------------------------------------------------*

"--- p ---
"- The conversion is performed to the internal representation of a packed number.
"- If the value range is too small, the CX_SY_CONVERSION_OVERFLOW exception is raised.
DATA pl5d1 TYPE p LENGTH 5 DECIMALS 1.

i = 123456.
pl5d1 = i. "123456.0

i = 999999999.

TRY.
    pl5d1 = i.
  CATCH cx_sy_conversion_overflow INTO error.
    error_text = error->get_text( ). "Overflow
ENDTRY.

"--- decfloat16/decfloat34 ---
"Conversion to the internal representation of a decimal floating point number with scaling 0
i = 999999999.
dec16 = i. "999999999
dec34 = i. "999999999
i = 1.
dec16 = i. "1
dec34 = i. "1

"--- f ---
"Conversion to the internal format of a binary floating point number
i = 999999999.
f = i. "9.9999999900000000E+08

i = 1.
f = i. "1.0000000000000000E+00

"--- c ---
"- Integer value of the integer is formatted in commercial notation and passed aligned to the right.
"- Negative value: - character is placed in the last position
"- Positive value: a blank is placed in the last position there.
"- Target field longer than the string of digits including the plus/minus sign: Padding with blanks
"  on the left.
"- Target field too short: Number representation is moved to the right by one place for positive values.
"  If still too short (and for negative values), truncation is applied on the left and the * character
"  is put in the first position of the target.

i = 1.
c3 = i. "' 1 '
c5 = i. "'   1 '

i = -1.
c3 = i. "' 1-'
c5 = i. "'   1-'

i = 12345.
c3 = i. "'*45'
c5 = i. "'12345'

i = -123456789.
c5 = i. "'*789-'
c30 = i. "'                    123456789-'

"--- n ---
"The absolute value of the integer number is passed aligned to the right. Padding
"is applied with 0.
i = 123.
n5 = i. "00123

i = -12.
n5 = i. "00012

i = 123456789.
n5 = i. "56789

i = -123456789.
n5 = i. "56789

"--- string ---
"- Passing is performed without gaps
"- The - character is put in the last position
"- A blank is set for positive values

i = 123.
str = i. "`123 `
DATA(len) = strlen( str ). "4
ASSERT len = 4.

i = -1234.
str = i. "`1234-`
len = strlen( str ). "5
ASSERT len = 5.

"--- d ---
"- Integer value between 1 and 3652060: Intrepretation as the number of days since 01.01.0001; the resulting
"  date is put in the target with the format yyyymmdd.
"- Value outside of the range: Target is padded with 0.

i = 739270.
d = i. "20250118

i = 1.
d = i. "00010102

i = 5555555.
d = i. "00000000

"--- t ---
"- Integer value is divided by 86400 (i.e. the number of seconds of a day), and the
"  integer remainder of the division is interpreted as the number of seconds since midnight.
"- The result is put in the target in the format hhmmss.

i = 60.
t = i. "000100

i = 86400.
t = i. "000000

i = 30000.
t = i. "082000

"--- utclong ---
"This conversion is not supported.
DATA dref TYPE REF TO data.
dref = NEW utclong( ).

TRY.
    dref->* = i.
  CATCH cx_sy_conversion_not_supported INTO error.
    error_text = error->get_text( ). "Cannot be converted
ENDTRY.

*&---------------------------------------------------------------------*
*& Source type: p
*&---------------------------------------------------------------------*

TYPES pl8d3_type TYPE p LENGTH 8 DECIMALS 3.
DATA pl8d3 TYPE pl8d3_type.

"--- i ---
"Rounding is performed commercially to an integer number
pl8d3 = CONV pl8d3_type( '0.815' ).
i = pl8d3. "1

pl8d3 = CONV pl8d3_type( '9.999' ).
i = pl8d3. "10

"--- p ---
DATA pl4d1 TYPE p LENGTH 4 DECIMALS 1.
pl8d3 = CONV pl8d3_type( '1.023' ).
pl4d1 = pl8d3. "1.0

"--- c ---
"- Formatting is applied in commercial notation
"- Passing to a target aligned to the right
"- Negative value: The - character is put in the last position
"- Positive value: A blank is put in the last position
"- Truncation is applied too

pl8d3 = CONV pl8d3_type( '1.023' ).
c3 = pl8d3. "*23

pl8d3 = CONV pl8d3_type( '-0.815' ).
c5 = pl8d3. "'.815-'

"--- n ---
"- Rounding is performed commercial to an integer number
"- The absolute value is passed to the target as string of
"  digits aligned to the right.

pl8d3 = CONV pl8d3_type( '4.123' ).
n5 = pl8d3. "00004

*&---------------------------------------------------------------------*
*& Source types: decfloat16/decfloat34
*&---------------------------------------------------------------------*

dec16 = CONV decfloat16( '1.987654321098765' ).
dec34 = CONV decfloat34( '4.567890123456789012345678901234567' ).

"--- i ---
"Rounding to an integer value
i = dec16. "2
i = dec34. "5

"--- p ---
"Rounding to the number of decimals places
pl8d2 = dec16. "1.99
pl8d2 = dec34. "4.57

"--- decfloat16/decfloat34 ---
"In case of a conversion of type decfloat34 to decfloat16, the
"mantissa is truncated. Commercial rounding is applied.
dec16 = dec34. "4.567890123456789

dec16 = CONV decfloat16( '1.987654321098765' ).

"--- c ---
"- Passing to the target aligned to the right
"- First, a conversion to the mathematical or scientific notation is performed.
c30 = dec16. "'             1.987654321098765'
c30 = dec34. "'4.5678901234567890123456789012'
c3 = dec34. "4.6

"--- n ---
"Rounding to an absolute integer value, which is passed as string of digits
"aligned to the right
n5 = dec16. "00002

*&---------------------------------------------------------------------*
*& Source type: f
*&---------------------------------------------------------------------*

f = CONV f( '1.2345678E+3' ).

"--- i ---
"Rounding up to an integer value
i = f. "1235

"--- p ---
"Rounding to the number of decimals places
pl8d2 = f. "1234.57

"--- decfloat16/decfloat34 ---
"Rounding to 17 places
f = CONV f( '1.23456781234567890123456789E+3' ).
dec16 = f. "1234.567812345679
dec34 = f. "1234.5678123456789

*&---------------------------------------------------------------------*
*& Byte-like source fields
*&---------------------------------------------------------------------*
"Note: Source fields of type xstring are handled similarly.

"change example values
DATA x2 TYPE x LENGTH 2 VALUE '6869'.
xstr = CONV xstring( `48656C6C6F20776F726C64` ).

"--- c ---
"- Values of each half-byte in the source are converted to the hexadecimal
"  characters 0-9 and A-F.
"- They are passed to the target aligned to the left.
"- If the target is longer than the number of characters passed, a padding is applied
"  with blanks. If it is too short, truncation is applied.
c5 = x2. "'6869 '
c30 = x2. "'6869                          '

"--- x ---
"- Bytes in the source are passed to the source aligned to the left.
"- If the target field is longer than the number of bytes passed, a padding with
"  hexadecimal 0 is applied on the right. If the target field is shorter, truncation is
"  applied.
x4 = x2. "68690000

*&---------------------------------------------------------------------*
*& Date, time and time stamps as source fields
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Source type: d
*&---------------------------------------------------------------------*
"Note: The conversion to the types t and utclong is not supported.

d = CONV d( '20250125' ).

"--- i ---
"The number of days since 01.01.0001 is calculated an converted to the
"corresponding integer value.

i = d. "739277

"--- c ---
"Passing to the target aligned to the left
DATA c4 TYPE c LENGTH 4.
c4 = d. "2025

"--- n ---
"Passing to the target aligned to the left; padding is applied if the
"target is longer
n5 = d. "20250
n3 = d. "202
DATA n10 TYPE n LENGTH 10.
n10 = d. "2025012500

*&---------------------------------------------------------------------*
*& Source type: t
*&---------------------------------------------------------------------*
"Note: The conversion to the types d and utclong is not supported.

t = CONV t( '123456' ).

"--- i ---
"The integer value is interpreted as a time specification in the format hhmmss.
"From this value, the value hh*3600+mm*60+ss is calculated, which represents the
"number of seconds passed since midnight.

i = t. "45296

DATA(seconds_since_midnight) = ( 12 * 3600 ) + ( 34 * 60 ) + 56.
ASSERT i = seconds_since_midnight.

"--- c ---
c4 = t. "1234

"--- n ---
"Passing to the target aligned to the left; padding is applied if target
"is longer
n5 = t. "12345
n3 = t. "123
n10 = t. "1234560000

*&---------------------------------------------------------------------*
*& Source type: utclong
*&---------------------------------------------------------------------*
"Note: The conversion to types i, int8, p, decfloat16, decfloat34, f,
"n, x, xstring, d, t is not supported.

utclong = CONV utclong( '2025-01-01 12:34:56.1110000' ).

"--- c ---
"- Conversion to a character string that represents the time stamp in the
"  notation yyyy-mm-dd hh:mm:ss.fffffff.
"- There is a blank between the date and time specification
"- If the target is shorter than 27 characters, truncation is applied on
"  the right. If it is longer, blanks are padded on the right.

c30 = utclong. "'2025-01-01 12:34:56.1110000   '
c5 = utclong. "2025-

"--- string ---
"Same conversion as for a target of type c. The resulting length of the
"target string is 27.
str = utclong. "2025-01-01 12:34:56.1110000
len = strlen( str ).
ASSERT len = 27.
```

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Data Objects Using Inline Declaration

An [inline declaration](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm) is made using the declaration operator [`DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_inline.htm). It can be specified in any designated [declaration position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_position_glosry.htm). The result of the declaration is used in the current operand position, is  statically visible from the current position, and is valid in the current context.

> [!NOTE]
> - In an assignment, if the data object is declared inline on the left side, there are many options for what can be placed on the right side as shown in the previous section. The data type of the variable is determined by the operand type. It must be possible to derive this type completely statically.
> - For more information about the possible declaration positions, see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_positions.htm).    
> - You can use the [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm) declaration operator to create [immutable variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenimmutable_variable_glosry.htm), as shown below.
> - [Programming guidelines for inline declarations (F1 documentation for Standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendeclaration_inline_guidl.htm)
> - Inline declarations are particularly useful in combination with constructor expressions, such as `VALUE`. Find more information and examples in other cheat sheets, for example, [Constructor Expressions](05_Constructor_Expressions.md).

 
```abap
"Data object declarations and assignements as shown above.
DATA str_b1 TYPE string VALUE `abc`.

DATA str_b2 TYPE string.
str_b2 = `def`.

"Using a declaration expression with the declaration operator DATA, a data object can be
"declared inline.
"The data type of the declared variable is determined by the right side.
"In doing so, a data object is assigned a value in one go.
"In the following case, it is a string literal with backquotes on the right side.
"The data type is derived and, hence, the data object is of type string.
DATA(str_b3) = `ghi`.

"In the following case, it is a text field literal with quotes. Type c is derived.
"The length is derived from the number of characters.
DATA(c_l3) = 'jkl'. "type c length 3
DATA(c_l4) = 'mnop'. "type c length 4
"Note the type conversion implications when making an assignment with these two data objects.
c_l3 = c_l4. "c_l3: 'mno'

"Structures and internal tables
"In declaration expressions, structures and internal tables can be declared inline and filled with,
"for example, the help of the VALUE operator.

"Structured data type
TYPES: BEGIN OF ts_struc,
          comp1 TYPE i,
          comp2 TYPE string,
        END OF ts_struc.

"In the following examples, structures are created. The structured data type is derived from the
"type specified before the parentheses.
DATA(struc_b1) = VALUE ts_struc( comp1 = 1 comp2 = `A` ).
"No components specified and values assigned means an initial structure. This syntax is also possible
"for declaring data objects with elementary types and explicitly specifiying initial values, but only 
"for initial values. See the CONV operator.
DATA(struc_b2) = VALUE ts_struc( ).
DATA(elem_init) = VALUE i( ).
"Note that components that are not specified and assigned a value remain initial.
DATA(struc_b3) = VALUE zdemo_abap_carr( carrid = 'AB' carrname = 'AB Airlines' ).
"An entire structure is assigned.
DATA(struc_b4) = struc_b1.

"Note: When the structure has already been declared, and you want to assign values, you can use
"the VALUE operator followed by the # character instead of the explicit type name.
"In that case, it is possible to derive the type from the context.
struc_b4 = VALUE #( comp1 = 2 comp2 = `b` ).

"Internal tables
"The internal table type is specified before the parentheses after the VALUE operator.
"The following example uses a table type that is globally available in the DDIC.
DATA(itab_b1) = VALUE string_table( ( `a` )
                                    ( `b` )
                                    ( `c` ) ).

"Using a local internal table type
TYPES tt_b1 TYPE TABLE OF ts_struc WITH EMPTY KEY.
DATA(itab_b2) = VALUE tt_b1( ( comp1 = 1 comp2 = `a` )
                              ( comp1 = 2 comp2 = `b` )
                              ( comp1 = 3 comp2 = `c` ) ).

"In the context of other ABAP statements such as LOOP, READ TABLE or ABAP SQL
"SELECT statements, inline declarations are useful for creating target variables with
"appropriate data types in place. This includes data reference variables and field
"symbols. Field symbols are not covered below.

"A work area/structure to hold the current internal table line is created inline.
LOOP AT itab_b2 INTO DATA(wa_b1).
  wa_b1-comp1 = 12345.
  ...
ENDLOOP.

"Using the REFERENCE addition, a data reference variable can be created inline.
LOOP AT itab_b2 REFERENCE INTO DATA(wa_ref_b1).
  wa_ref_b1->comp1 = 67890.
  ...
ENDLOOP.

"A structure to hold the internal table line read is created inline.
READ TABLE itab_b2 INTO DATA(wa_b2) INDEX 2.
"Data reference variable
READ TABLE itab_b2 REFERENCE INTO DATA(wa_ref_b2) INDEX 2.


DATA(strtab) = VALUE string_table( ( `aaa` ) ( `bbb` ) ( `ccc` ) ).

LOOP AT strtab ASSIGNING FIELD-SYMBOL(<fs>).
  <fs> = to_upper( <fs> ).
ENDLOOP.

READ TABLE strtab REFERENCE INTO DATA(dref) INDEX 1.
ASSERT dref->* = `AAA`.

"ABAP SQL statements
"A structure as target data object is created inline.
SELECT SINGLE * FROM zdemo_abap_carr INTO @DATA(struc_b5).
"NEW addition of the INTO clause creates a data reference variable
SELECT SINGLE * FROM zdemo_abap_carr INTO NEW @DATA(struc_ref).

"Internal table as target data object is created inline.
SELECT * FROM zdemo_abap_carr INTO TABLE @DATA(itab_b3).
SELECT * FROM zdemo_abap_carr INTO TABLE NEW @DATA(itab_ref).
```

Inline declaration is useful for specifying data objects as actual parameters in method calls. It automatically determines the appropriate type, eliminating the need for additional variable declarations. However, it is important to note that inline declarations cannot be used when a method call is included in an expression. For example, if a method has both returning and exporting parameters, and you want to make a method call and assignement as shown in the example commented out, while also using inline declarations for the exporting parameters, this is not possible. This is because the assignment involves a method call expression on the right-hand side.

```abap
"The following method calculates the difference in days, hours, minutes,
"and seconds of two time stamps.
"Check the F2 information of the 'diff' method. It has 4 exporting parameters.
"3 different types are involved. Using the inline declarations, the data
"objects receive the suitable types automatically. Extra variable declarations
"with suitable types can be avoided.
cl_abap_utclong=>diff( EXPORTING high     = CONV utclong( '2024-01-01 15:30:00' )
                                 low      = CONV utclong( '2023-12-24 14:20:40' )
                       IMPORTING days    = DATA(days)
                                 hours   = DATA(hours)
                                 minutes = DATA(minutes)
                                 seconds = DATA(seconds) ).

"Not possible
"DATA(result) = some_cl=>some_meth( IMPORTING some_param = DATA(act_param) ).                                  
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Assigning References to Data Reference Variables
    
- As with other data objects and types, there are [special assignment rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_references.htm) for [data reference variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm). See the ABAP Keyword Documentation.
- An initial reference variable contains the [null reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennull_reference_glosry.htm), which does not point to any objects. This means that it has neither a data type nor a class as a [dynamic type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendynamic_type_glosry.htm).
- The concepts of [upcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenup_cast_glosry.htm) and [downcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendown_cast_glosry.htm) enter the picture here. See the following code snippet.

```abap
"Declaring data reference variables with static types
"At this stage, initial reference variables contain null references.
DATA dref_1_i TYPE REF TO i.
DATA dref_2_str TYPE REF TO string.
"Generic type as static type
DATA dref_3_data TYPE REF TO data.

"References in data reference variables can point to existing data objects.
"For assigning the reference, you can use the REF operator.
"There is also an ABAP statement available doing the same: GET REFERENCE.
"It should not be used anymore, especially in ABAP for Cloud development.

"Creating data objects to refer to and providing a start value
DATA do_number TYPE i VALUE 987.
DATA do_string TYPE string VALUE `abc`.

"After the assignment, the data reference variable points to the values.
"The data type is derived (dynamic type).
dref_1_i = REF #( do_number ). "Dynamic type is the same as the static type in this case
dref_2_str = REF #( do_string ). "Dynamic type is the same as the static type in this case
"Dynamic types of the followig examples are more specific than static type,
"which is a generic type in this case.
dref_3_data = REF #( do_number ).
dref_3_data = REF #( do_string ).

"Note: Table expressions can be also specified within the parentheses.

"Inline declarations are also possible to create data reference variables
"and assigning values in one go. 
DATA(dref_4_data) = dref_3_data.
DATA(dref_5_str) = REF #( `hi` ).
DATA(dref_6_i) = REF #( do_number ).

"Assignments between two data reference variables mean that references are copied.
"The concepts of upcast and downcast enter the picture here.
"Two different assignment operators are used, as well as the casting operator CAST.

"--------------------- Upcasts --------------------
"Upcasts are possible for elementary data types
"- The static type of the target variable is more general or identical to the static type of the source variable.
"- Assignment operator used: =
"- Note that the operators for downcasts can also be used explicitly here, but it is usually not needed.
"- In this example, elementary data types are covered. An upcast works ...
"  - if the data types have identical type properties (i.e. the built-in types match as well as length and decimal places).
"  - the static type of the source variable is completely typed, and the static type of the target variable is generic.

"The following upcasts work. Both point to data objects of type i or string.
dref_1_i = dref_6_i.
"The source on the right side is completely typed (type i),
"the target on the left side is a generic type (type data).
dref_3_data = dref_1_i.

"--------------------- Downcasts --------------------
"- The static type of the target variable is more specific than the static type of the source variable.
"- The assignability is not checked until runtime.
"- Must always be performed explicitly using the casting operator ?= or the more modern casting operator CAST.

"The following example would result in a syntax error due to type incompatibility.
"dref_1_i = dref_3_data.

"In the following example, the source has a generic static type (data). The target type
"has a more specific type (type i).
"To suppress the syntax error, the CAST operator is needed.
"Note:
"- The assignability is still not checked. This is done at runtime.
"  In this example, it works since the dynamic type of the source is also of type i.
"- An advantage of the CAST operator compared to ?= is that the operator enables downcasts in operand positions,
"  which helps reduce helper variables.
dref_1_i = CAST #( dref_3_data ).

"If not caught, the following would result in a runtime error.
"dref_3_data points to a data object of type i, the static type is dref_2_str is string.
"So, the downcast does not work.
TRY.
    dref_2_str = CAST #( dref_3_data ).
  CATCH cx_sy_move_cast_error INTO DATA(e).    
ENDTRY.

"Old syntax using the ?= operator
dref_1_i ?= dref_3_data.

"For upcasts, the operators can be used, too, but they are usually not necessary.
"So, an assignment as follows is possible but not needed. Only using = is sufficient.
dref_1_i = CAST #( dref_6_i ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Anonymous Data Objects

[Anonymous data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm) are a topic related to data reference variables.
These data objects are [unnamed data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenunnamed_data_object_glosry.htm).
Most of the data objects in the snippets above are named data objects (excluding the data reference variables), meaning that they can be addressed by a specific name. Unnamed data objects are literals and anonymous data objects. Anonymous data objects can be addressed using data reference variables.
Unlike data objects created with the `DATA` statement, anonymous data objects are created at runtime. Data objects declared with `DATA` are created when the program is loaded.

There are several ways to create anonymous data objects:
- [`CREATE DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_data.htm) statements
- Using the instance operator [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm)
- Addition `NEW` of the [`INTO`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_into_target.htm) clause in ABAP SQL `SELECT` statements

```abap
"--------------------- CREATE DATA statements --------------------
"Note that there are many additions available. The examples show a selection.

"Creating an anonymous data object with an implicit type.
"If neither of the additions TYPE or LIKE are specified, the data reference variable
"must be completely typed.
DATA dref_7_str TYPE REF TO string.
CREATE DATA dref_7_str.

"Note: If you want to assign a value to the data object, this can't be done directly.
"The data reference variable must be dereferenced first using the dereferencing operator.
dref_7_str->* = `hi`.

"Creating an anonymous data object with an explicitly specified data type
DATA dref_8_data TYPE REF TO data.
CREATE DATA dref_8_data TYPE p LENGTH 8 DECIMALS 3.
dref_8_data->* = 1 / 3.

"Creating a named data object
DATA it TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.

"Creating an anomyous internal table.
"Using the LIKE addition to refer to an existing internal table
CREATE DATA dref_8_data LIKE it.

"Using the anonymous data object as target in the INTO clause of a SELECT statement
SELECT *
  FROM zdemo_abap_carr
  INTO TABLE @dref_8_data->*.

"Creating an anonymous hashed table by specifying the entire table type
CREATE DATA dref_8_data TYPE HASHED TABLE OF zdemo_abap_carr WITH UNIQUE KEY carrid.

"Using the anonymous data object as target in the INTO clause of a SELECT statement
SELECT *
    FROM zdemo_abap_carr
    INTO TABLE @dref_8_data->*.

"Creating an anonymous structure
CREATE DATA dref_8_data TYPE zdemo_abap_fli.

"Using the anonymous data object as target in the INTO clause of a SELECT statement
SELECT SINGLE *
    FROM zdemo_abap_fli
    INTO @dref_8_data->*.

"--------------------- NEW operator --------------------
"- Works like CREATE DATA dref TYPE type statements and can be used in general expression positions.
"- Allows to assign values to the new anonymous data objects in parentheses

"Creating data reference variables
DATA: dref_9_data TYPE REF TO data,
      dref_10_i   TYPE REF TO i.

"Assining a value to an anonymous data object of the type i
dref_9_data = NEW i( 555 ).

"The # character can be used instead of the complete type spefication
"when the type can be derived from the context.
dref_10_i = NEW #( 653 ).

"Inline declarations are handy.
"Creating a suitable anonymous data object in place. Here, the type must be specificed explicitly.
DATA(dref_11_i) = NEW i( 32 ).

"Creating an anonymous structure
DATA(dref_12_ddic_tab) = NEW zdemo_abap_carr( carrid = 'AB' carrname = 'AB Airlines'  ).

"The # character can be omitted when they type can be derived.
DATA dref_13_ddic_tab TYPE REF TO zdemo_abap_carr.
dref_13_ddic_tab = NEW #( carrid = 'AB' carrname = 'AB Airlines'  ).

"ABAP SQL SELECT statement
"Using the NEW addition in the INTO clause, an anonymous data object can be created in place.
SELECT *
  FROM zdemo_abap_carr
  INTO TABLE NEW @DATA(dref_14_inline).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Constants and Immutable Variables

[Constants](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstant_glosry.htm) are [named data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennamed_data_object_glosry.htm) whose value cannot be changed at runtime. You declare them with the [`CONSTANTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconstants.htm) statement. Unlike the `DATA` statement, a start value must be specified with the `VALUE` addition.

[Immutable variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenimmutable_variable_glosry.htm) can also be declared inline with the [declaration operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_operator_glosry.htm)
 [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm).

```abap
*&---------------------------------------------------------------------*
*& Constants
*&---------------------------------------------------------------------*

"As mentioned above, constants cannot be changed at runtime.
CONSTANTS con_str TYPE string VALUE `hallo`.

"Constants as start values for dobj declarations following value
CONSTANTS con_underscores TYPE string VALUE `__________`.
DATA str_w_con_as_start_value TYPE string VALUE con_underscores.

"Constant structure
CONSTANTS: BEGIN OF const_struct,
              int  TYPE i VALUE 1,
              char TYPE c LENGTH 4 VALUE 'ABAP',
           END OF const_struct.

*&---------------------------------------------------------------------*
*& Immutable variables
*&---------------------------------------------------------------------*

FINAL(do_final_inl) = 1.
DATA(do_data_inl) = 1 + do_final_inl.
"not possible
"do_final_inl = 2.

SELECT * FROM zdemo_abap_carr INTO TABLE @DATA(itab_data_inl).

DATA itab_like_inline LIKE itab_data_inl.

"Using an inline declaration as target of a LOOP statement
"A value is assigned multiple times, but it cannot be changed in any other write positions.
LOOP AT itab_data_inl INTO FINAL(wa_final).
  "The following is not possible
  "wa_final-carrid = 'd'.
  "only read access
  APPEND wa_final TO itab_like_inline.
ENDLOOP.

"SELECT statement with a an immutable target table declared inline
SELECT * FROM zdemo_abap_carr INTO TABLE @FINAL(itab_final_inl).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Built-In Data Objects

In [ABAP programs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm), you can use built-in data objects, including:

| Built-in data object | Details |
| -------- | ------- |
| ABAP system fields | These fields, filled by the [ABAP runtime framework](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_runtime_frmwk_glosry.htm), can be used to query system information and more. Typically, they should only be read, and not overwritten. The fields are components of the built-in structure `sy` (or `syst`). Prominent ones are `sy-subrc` (return code of many ABAP statements; typically, the value 0 indicates success), `sy-tabix` (row index of internal tables), and `sy-index` (loop pass index), which can be used in [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm). However, most of these fields should not be used in ABAP for Cloud Development (indicated by a syntax warning) because they refer to [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm) contexts (e.g. classic dynpros and lists), or their values are not relevant in a cloud context. More information about the purpose of the individual components is available at [ABAP System Fields (F1 documentation for Standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensystem_fields.htm).|
| `space` constant | It is of type `c`, length 1, and contains a blank character. |
| `me` self-reference  | Used in ABAP Objects, it's a local reference variable for instance method implementations. At runtime, it points to the instance executing the method. It is primarily used to be explicit about, for exmaple, using instance attributes of the class, especially if there is a local data object with the same name.|

Expand the following collapsible section for example code. To try it out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.


<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA str TYPE string VALUE `Data object 'str' declared in the private visibility section`.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "In ABAP for Cloud Development, the following statement will show a syntax warning saying that
    "sy should not be used. Here, it is used for demonstration purposes.
    "In the example, RTTI is used to get all component names of the built-in data object sy. In the loop,
    "ABAP statements are created (they represent simple assignments using the various sy components) and
    "output to the console. You can insert all the output DATA(...) = ... statements in the demo class's
    "main method implementation. The purpose is to demonstrate that most of the sy components should not be
    "used in ABAP for Cloud Development. Most of the statements will show a syntax warning in ABAP for Cloud
    "Development. Check the ABAP Keyword Documentation (for Standard ABAP) and the F2 information for the
    "purpose of the individual sy components.
    LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( sy ) )->components INTO DATA(co).
      DATA(sycomp) = to_lower( co-name ).
      DATA(code) = |DATA(sy{ sycomp }) = sy-{ sycomp }.|.
      out->write( code ).
    ENDLOOP.
    out->write( |\n| ).
    out->write( |\n| ).

    "Demonstrating prominent sy components that can be used in ABAP for Cloud Development

    "------------------------------------------------------------------------------
    "------------------ sy-subrc: Return code of ABAP statements ------------------
    "------------------------------------------------------------------------------

    "Many ABAP statements set a sy-subrc value. Check the ABAP Keyword Documentation
    "for individual statements. Usually, the value 0 indicates a successful execution.

    DATA(some_string) = `ABAP`.

    "FIND statements
    "Found
    FIND `P` IN some_string.
    ASSERT sy-subrc = 0.

    "Not found
    FIND `p` IN some_string RESPECTING CASE.
    ASSERT sy-subrc = 4.

    DATA(some_itab) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ( `d` ) ).

    "READ TABLE statements
    "Entry available
    READ TABLE some_itab INTO DATA(wa1) INDEX 3.
    ASSERT sy-subrc = 0.

    "Entry not available
    READ TABLE some_itab INTO DATA(wa2) INDEX 7.
    ASSERT sy-subrc = 4.

    "ABAP SQL statements
    DELETE FROM zdemo_abap_tab1.
    IF sy-subrc = 0.
      out->write( `DELETE: All rows were deleted.` ).
    ELSE.
      out->write( `DELETE: No row was deleted because it was already empty.` ).
    ENDIF.

    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 ) ( key_field = 2 ) ) ).
    IF sy-subrc = 0.
      out->write( `INSERT: All rows of the internal table were inserted.` ).
    ENDIF.

    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 3 ) ( key_field = 3 ) ) ) ACCEPTING DUPLICATE KEYS.
    IF sy-subrc = 4.
      out->write( `INSERT ... ACCEPTING DUPLICATE KEYS: sy-subrc has the value 4 in this case. Not all rows of the ` &&
                  `internal table were inserted because a row with the key already exists.` ).
    ENDIF.

    DELETE FROM zdemo_abap_tab1 WHERE key_field = 3.
    IF sy-subrc = 0.
      out->write( `DELETE: The row matching the WHERE condition was deleted.` ).
    ELSE.
      out->write( `DELETE: No match according to the WHERE condition.` ).
    ENDIF.

    DELETE FROM zdemo_abap_tab1 WHERE key_field = 3.
    IF sy-subrc = 0.
      out->write( `DELETE: The row matching the WHERE condition was deleted.` ).
    ELSE.
      out->write( `DELETE: No match according to the WHERE condition.` ).
    ENDIF.

    "------------------------------------------------------------------------------
    "--------------------------- sy-index: Loop indexes ---------------------------
    "------------------------------------------------------------------------------

    CLEAR some_string.

    "DO loops
    DO 5 TIMES.
      some_string = some_string && sy-index.
    ENDDO.

    ASSERT some_string = `12345`.

    CLEAR some_string.

    DO 10 TIMES.
      some_string = some_string && sy-index.
      IF sy-index = 7.
        EXIT.
      ENDIF.
    ENDDO.

    ASSERT some_string = `1234567`.

    CLEAR some_string.

    DATA number TYPE i.

    "WHILE loop
    WHILE number < 9.
      number = sy-index.
      some_string = some_string && number.
    ENDWHILE.

    ASSERT some_string = `123456789`.

    "------------------------------------------------------------------------------
    "------------------- sy-tabix: Row index of internal tables -------------------
    "------------------------------------------------------------------------------

    "Demo standard internal table with 5 entries
    DATA(std_itab) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ( `d` ) ( `e` ) ).

    "READ TABLE statement using a free key
    READ TABLE std_itab INTO DATA(wa3) WITH KEY table_line = `b`.
    ASSERT sy-tabix = 2.

    "Demo hashed internal table with 5 entries
    DATA(hashed_itab) = VALUE string_hashed_table( ( `a` ) ( `b` ) ( `c` ) ( `d` ) ( `e` ) ).

    "READ TABLE statement using a free key
    READ TABLE hashed_itab INTO DATA(wa4) WITH KEY table_line = `b`.
    "Hashed tables do not have a primary table index.
    ASSERT sy-tabix = 0.

    CLEAR some_string.

    "LOOP statements
    LOOP AT std_itab INTO DATA(wa5).
      some_string = some_string && sy-tabix.
    ENDLOOP.
    ASSERT some_string = `12345`.

    CLEAR some_string.
    "Step addition
    "In the example, the table is looped across backwards
    "indicated by the negative value. The step size 1 indicates
    "that each line is respected.
    LOOP AT std_itab INTO DATA(wa6) STEP -1.
      some_string = some_string && sy-tabix.
    ENDLOOP.
    ASSERT some_string = `54321`.

    CLEAR some_string.
    "Forward loop, step size = 2
    LOOP AT std_itab INTO DATA(wa7) STEP 2.
      some_string = some_string && sy-tabix.
    ENDLOOP.
    ASSERT some_string = `135`.

    CLEAR some_string.
    "FROM/TO additions
    LOOP AT std_itab INTO DATA(wa8) FROM 2 TO 4.
      some_string = some_string && sy-tabix.
    ENDLOOP.
    ASSERT some_string = `234`.

    CLEAR some_string.
    "STEP/FROM additions
    LOOP AT std_itab INTO DATA(wa9) STEP 2 FROM 2.
      some_string = some_string && sy-tabix.
    ENDLOOP.
    ASSERT some_string = `24`.

    CLEAR some_string.
    "Hashed table
    LOOP AT hashed_itab INTO DATA(wa10).
      some_string = some_string && sy-tabix.
    ENDLOOP.
    ASSERT some_string = `00000`.

    "------------------------------------------------------------------------------
    "------------------------ sy-dbcnt: Edited table rows -------------------------
    "------------------------------------------------------------------------------

    DELETE FROM zdemo_abap_tab1.
    DATA(dbcnt) = sy-dbcnt.

    out->write( |Dbtab rows deleted: { dbcnt }| ).

    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 ) ( key_field = 2 ) ) ).
    ASSERT sy-dbcnt = 2.

    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 3 ) ( key_field = 3 ) ) ) ACCEPTING DUPLICATE KEYS.
    ASSERT sy-dbcnt = 1.

    MODIFY zdemo_abap_tab1 FROM @( VALUE #( key_field = 1 char1 = 'aaa' ) ).
    ASSERT sy-dbcnt = 1.

    UPDATE zdemo_abap_tab1 SET char2 = 'bbb'.
    ASSERT sy-dbcnt = 3.

    DELETE FROM zdemo_abap_tab1 WHERE num1 IS INITIAL.
    ASSERT sy-dbcnt = 3.

    "------------------------------------------------------------------------------
    "------------- sy-fdpos: Occurrence in byte or character strings --------------
    "------------------------------------------------------------------------------
    "For example, relevant in comparison expressions such as CS (constains string).
    "If the comparison is true, sy-fdpos contains the offset of the found value. If it
    "is false, sy-fdpos contains the length of the searched string.

    some_string = `###abap###`.

    IF some_string CS `p`.
      out->write( |The substring is found. Offset of first finding: { sy-fdpos }| ).
    ELSE.
      out->write( |The substring is not found. Length of searched string: { sy-fdpos }| ).
      ASSERT sy-fdpos = strlen( some_string ).
    ENDIF.

    IF some_string CS `#`.
      out->write( |The substring is found. Offset of first finding: { sy-fdpos }| ).
    ELSE.
      out->write( |The substring is not found. Length of searched string: { sy-fdpos }| ).
      ASSERT sy-fdpos = strlen( some_string ).
    ENDIF.

    IF some_string CS `Y`.
      out->write( |The substring is found. Offset of first finding: { sy-fdpos }| ).
    ELSE.
      out->write( |The substring is not found. Length of searched string: { sy-fdpos }| ).
      ASSERT sy-fdpos = strlen( some_string ).
    ENDIF.

    "------------------------------------------------------------------------------
    "----------------------------- Constant space ---------------------------------
    "------------------------------------------------------------------------------
    "Has the data type c, length 1, and contains a blank character.

    "Note: Trailing blanks are ignored in most operand positions. Therfore, the constant
    "space should not be used in such positions.

    some_string = |{ space }{ space }|.
    ASSERT some_string = ``.

    some_string = `#` && space && space && space && space && space && `#`.
    ASSERT some_string = `##`.

    "Using a CONCATENATE statement, blanks are respected after the SEPARATED BY
    "addition only
    CONCATENATE space space INTO some_string SEPARATED BY space.
    ASSERT some_string = ` `.

    "Text field literal, concatentation without SEPARATED BY space
    DATA some_char TYPE c LENGTH 5.
    CONCATENATE '#' space space '#' INTO some_char.
    ASSERT some_char = '##'.

    CONCATENATE space space '#' space '#' INTO some_char.
    ASSERT some_char = '##'.

    "------------------------------------------------------------------------------
    "----------------------------- Self-reference me ------------------------------
    "------------------------------------------------------------------------------
    "When implementing instance methods, you can optionally make use of the implicitly available
    "object reference variable me which is always available at runtime and points to the respective
    "object itself. You can use it to refer to components of the instance of a particular class.

    "Intentionally creating a data object that has the same name as a data object
    "created in the private visibility section of the demo class.
    DATA str TYPE string VALUE `Local data object 'str'`.

    "Demo assignments
    "dobj1 contains the value of the locally declared data object
    DATA(dobj1) = str.
    "dobj2 contains the value of the class attribute
    DATA(dobj2) = me->str.

    out->write( data = dobj1 name = `dobj1` ).
    out->write( data = dobj2 name = `dobj2` ).

  ENDMETHOD.
ENDCLASS.
```

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## ABAP Enumerated Types and Objects
- ABAP supports the concept of enumerations. 
- Enumerations are a mixture of types and constants.
- An [enumerated type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenum_type_glosry.htm) specifies a value set in addition to the actual type properties. 
- [Enumerated objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenumerated_object_glosry.htm) - data objects with an enumerated type - are mainly used to check allowed values. This usually restricts the actual parameters passed to methods to the enumerated values defined in the class. [Enumerated variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenumerated_variable_glosry.htm) are variable enumerated objects. They can only contain the associated enumerated values. 
- [CDS enumerated types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_enum_type_glosry.htm) are also available. Find more information in the [ABAP Dictionary (DDIC)](26_ABAP_Dictionary.md) cheat sheet.

Syntax:

```abap
"The definition of an enumerated type in ABAP declares its enumerated constants (these are special enumerated objects).
"a) In the case below, no explicit base type is specified. Then, the standard base type of the constants is i. The 
"   enumerated values are counted up starting with 0 (a -> 0, b -> 1 ...).

TYPES: BEGIN OF ENUM t_enum,
         a,
         b,
         c,
         d,
       END OF ENUM t_enum.

"b) Explicit base type is specified and start values provided using the VALUE addition
"   Note that one value must be initial.

TYPES: basetype TYPE c LENGTH 2,
       BEGIN OF ENUM t_enum_base BASE TYPE basetype,         
          e VALUE IS INITIAL,
          f VALUE 'u',
          g VALUE 'v',
          h VALUE 'wx',
          i VALUE 'yz',
       END OF ENUM t_enum_base.

"c) Optionally an enumerated structure can be declared in the context of the type declaration.
"   A component of an enumerated structure: An enumerated constant that exists as a component 
"   of a constant structure, not as a single data object.
    
TYPES: BEGIN OF ENUM t_enum_struc STRUCTURE en_struc BASE TYPE basetype,
          j VALUE IS INITIAL,
          k VALUE 'hi',
          l VALUE 'ab',
          m VALUE 'ap',
        END OF ENUM t_enum_struc STRUCTURE en_struc.
```

Enumerated variables can be declared by referring to the enumerated type.
They can only be assigned the enumerated values defined there that exist as the content of enumerated constants or components of an enumerated structure.

```abap
"Using the enumerated type from above
DATA dobj_enum_a TYPE t_enum.

dobj_enum_a = a.

"Data object declared inline, the typed derived is t_enum
DATA(dobj_enum_b) = b.   

"Note: The technical data type of an enumerated value is the base type 
"of the enumerated type. You can use the base type of an enumerated type 
"in special conversions using CONV. A base type is flat, elementary, and 
"has a maximum length of 16 bytes.
DATA some_dobj TYPE c LENGTH 2 VALUE 'ap'.
"M
DATA(dobj_enum_c) = CONV t_enum_struc( some_dobj ). 
ASSERT dobj_enum_c = en_struc-m.

"Getting the base value of an enumerated type
"hi
DATA(base_value_k) = CONV basetype( en_struc-k ). 

"yz
DATA(base_value_i) = CONV basetype( i ). 

"1
DATA(base_value_b) = CONV i( b ). 

"Conversion not possible
DATA some_string TYPE string VALUE 'ap'.
"DATA(dobj_enum_d) = CONV t_enum_struc( some_string ).
```

Find more information on enumerated types in the (commented code of the) cheat sheet example and [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenumerated_types_usage.htm). 

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Notes in a Nutshell

### Type Conversions, Compatibility and Assignments
A value assignment means that the value of a data object is transferred to a target data object. If the data types of the source and target are compatible, the content is copied unchanged. If they are incompatible and a suitable [conversion rule](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rule_glosry.htm) exists, the content is converted.
The following cases must be distinguished with regard to the data type: 
- The source and target data types are compatible, i.e. all technical type properties match. The content is transferred from the source to the target without being converted. 
- The source and target data types are incompatible, but can be converted. The content of the source is converted according to the conversion rules and then transferred to the target. Two data types are convertible if a conversion rule exists for them. An exception is raised if the content of the source cannot be handled according to the conversion rules.
- If the data objects are neither compatible nor convertible, no assignment can take place. If the syntax check detects this state, a syntax error is raised, otherwise an [uncatchable exception](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenuncatchable_exception_glosry.htm) is raised when the program is executed.

See the conversion rules for the different data types here: [Assignment and Conversion Rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rules.htm)

> [!NOTE]
> - The [operands](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_glosry.htm) of many ABAP statements are assigned internally according to the assignment rules. 
> - Typically, assignements are made using the assignment operator `=`. If necessary and applicable, the type is converted implicitly. However, you can also use the conversion operator [`CONV`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_conv.htm) to convert types explicitly.
> - For [lossless assignments](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlossless_assignment_glosry.htm), the lossless operator [`EXACT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_exact.htm) can be used to perform checks before the conversion is performed to ensure that only valid values are assigned and that no values are lost in assignments.
> - In general, no checks are performed on assignments between compatible data objects. If a data object already contains an invalid value, for example, an invalid date or time in a date or time field, it is passed a valid value when the assignment is made to a compatible data object.
> - The `applies_to_data` method of the [RTTI](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm) class `cl_abap_datadescr` can be used to check type compatibility. See the executable example and the [Dynamic Programming cheat sheet](06_Dynamic_Programming.md).
    

```abap
*&---------------------------------------------------------------------*
*& Conversions
*&---------------------------------------------------------------------*

"Implicit conversions of the types c and string
DATA do_1_str TYPE string VALUE `abcdef`.
DATA do_2_c3 TYPE c LENGTH 3.
"'abc'
do_2_c3 = do_1_str.


"Implicit conversions of the types i and decfloat34
DATA do_4_i TYPE i.
DATA do_5_dcfl34 TYPE decfloat34 VALUE '4.56'.
"5
do_4_i = do_5_dcfl34.

"Explicitly converting decfloat34 to i
DATA do_6_dcfl34 TYPE decfloat34 VALUE '2.78'.
"3
DATA(do_7_i) = CONV i( do_6_dcfl34 ).

"Note the conversion rules in the ABAP Keyword Documentation, e.g. from 
"type i to string 
"`10-`
DATA(i2str) = CONV string( -10 ).

*&---------------------------------------------------------------------*
*& Compatibility
*&---------------------------------------------------------------------*

"1. Source and target are compatible, all technical type properties 
"   match
DATA some_dobj TYPE c LENGTH 1 VALUE 'A'.
DATA flag TYPE abap_bool VALUE 'X'.

"Assignment of compatible types; content transferred without 
"conversion
some_dobj = flag.

"2. Source and target are not compatible but can be converted.
"   Content of source is converted according to conversion rules
"   and then transferred to the target. Two data types are
"   convertible if a conversion rule exists for them.
DATA another_char TYPE c LENGTH 3 VALUE 'abc'.
DATA some_str     TYPE string VALUE `defghij`.

another_char = some_str.

"3. Data objects are neither compatible nor convertible. No
"   assignment possible. If recognized by the syntax check, a
"   syntax error is raised, otherwise an uncatchable exception
"   is raised when the program is executed.
DATA some_number TYPE i VALUE 123.
DATA itab_str    TYPE string_table.

"Type conversion not possible
"itab_str = some_number.
"some_number = itab_str.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Terms Related to Data Types and Objects

<table>

<tr>
<td> Terms </td> <td> Code Snippet </td>
</tr>

<tr>
<td> 

Standalone and bound data types

 </td>

 <td> 

``` abap
"----------------- Standalone and bound data types ----------------- 
"Standalone: Data type that is defined using the statement TYPES in an 
"            ABAP program, as a data type of the ABAP Dictionary or as 
"            an CDS entity.
"Bound: Data type that only exists as a property of a data object.

"Standalone data type
TYPES te_a_c10 TYPE c LENGTH 10.

"Bound data types
DATA do_a_c20 TYPE c LENGTH 20.
DATA do_b_like LIKE do_a_c20.
TYPES te_b_like LIKE do_a_c20.
``` 

 </td>
</tr>

<tr>
<td> 

Complex and elementary data type/object

 </td>

 <td> 

``` abap
"-------------- Complex and elementary data type/object --------------
"Elementary: Data type of fixed or variable length that is neither 
"            structured, nor a table type or a reference type.
"Complex: Made up of other data types, for example structured data 
"         type/objects, a table type/internal tables

"Elementary
DATA do_c_i TYPE i.

"Complex
DATA: BEGIN OF struc_a,
        comp1 TYPE i,
        comp2 TYPE string,
        comp3 TYPE zdemo_abap_carr, "structure
        comp4 TYPE string_table, "internal table
        comp5 TYPE REF TO i, "reference type
      END OF struc_a.
``` 

 </td>
</tr>

<tr>
<td> 

Complete and generic data type

 </td>

 <td> 

``` abap
"------------------ Complete and generic data type ------------------
"Complete: Non-generic data type
"Generic:
"- Data type that does not set all properties of a data object.
"- Can only be used for the typing of formal parameters and field symbols, 
"  except for data reference variables using the generic type 'data'.

"Complete data type
DATA do_d_i TYPE i.

"Field symbols typed with generic data types
"Note: A field symbol is a symbolic name for a data object to which actual 
"memory can be assigned at runtime. A field symbol can be used as a placeholder 
"for a data object at an operand position. For more information, see the ABAP 
"Cheat Sheet on dynamic programming. Field symbols are used in this example 
"to demonstrate generic types other than just data.
FIELD-SYMBOLS <fs_a> TYPE clike.
FIELD-SYMBOLS <fs_b> TYPE data.

"Field symbols with generic data types can be assigned appropriate values
DATA do_e_c5 TYPE c LENGTH 5 VALUE 'abcde'.
DATA do_f_str TYPE string VALUE `Hallo, how are you?`.
ASSIGN do_e_c5 TO <fs_a>.
ASSIGN do_f_str TO <fs_a>.

DATA(itab_a) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).
ASSIGN do_e_c5 TO <fs_b>.
ASSIGN do_f_str TO <fs_b>.
ASSIGN itab_a TO <fs_b>.
``` 

 </td>
</tr>

<tr>
<td> 

Variable and constant data object

 </td>

 <td> 

``` abap
"------------------ Variable and constant data object ------------------
"Variable: Named data object whose value can be changed during the runtime 
"          of an ABAP program.
"Constant: Named data object whose value cannot be changed at runtime.

"Variable
DATA do_g_i TYPE i VALUE 123.
do_g_i = 456.

CONSTANTS con_a_i TYPE i VALUE 789.
"Not changeable
"con_a_i = 321.
``` 

 </td>
</tr>

<tr>
<td> 

Static and dynamic data object

 </td>

 <td> 

```abap
"--------------------- Static and dynamic data object --------------------
"Static:
"- Data object for which all attributes, including memory use, are specified 
"  statically by the data type.
"- Apart from reference variables, all static data objects are flat.
"Dynamic:
"- Data object for which all properties apart from the memory consumption are 
"  statically determined by the data type.
"- Dynamic data objects are strings and internal tables. They belong to the 
"  deep data objects. Structures that contain dynamic components are also 
"  dynamic data objects.

"Static data object
DATA do_h_c3 TYPE c LENGTH 3.
"Dynamic data object
DATA do_i_str TYPE string.

"Assignments
do_h_c3 = 'abc'.

"only 'def' assigned -> length and memory use do not change
"Note: In newer ABAP releases, the following statement shows a syntax 
"warning that the value of the literal (intentionally specified 
"here like this) is not an admissable value for the target type. 
do_h_c3 = 'defghi'. 

"'j' assigned, but length and memory do not change
do_h_c3 = 'j'. 

"Memory consumption changes for dynamic data objects
do_i_str = `abc`.
do_i_str = `d`.
do_i_str = `efghijklmnopqrstuvwxyz`.
``` 

 </td>
</tr>

<tr>
<td> 

Static type and dynamic type

 </td>

 <td> 

``` abap
"------------------------ Static type and dynamic type -----------------------
"Both are data types of a reference variable (reference type) that determine 
"the objects a reference variable can point to.
"Static type: For data reference variables, the static type is a data type 
"             that is always more general than or the same as the dynamic type.
"Dynamic type: For a reference variable, the dynamic type is always more special 
"              than or equal to the static type.

"Static type
DATA dref_a_i TYPE REF TO i. "Static type is i
DATA dref_b_data TYPE REF TO data. "Static type can also be generic

"Creating data objects to refer to
DATA do_j_i TYPE i VALUE 3.
DATA do_k_str TYPE string VALUE `hallo`.

"Dynamic types
dref_a_i = REF #( do_j_i ). "Only type i possible; the dynamic type is the same

"The dynamic type is more special than the static type (which is the generic 
"type data in this case)
dref_b_data  = REF #( do_j_i ).
dref_b_data  = REF #( do_k_str ).
dref_b_data  = REF #( dref_a_i ).
``` 

 </td>
</tr>

<tr>
<td> 

Flat and deep data object

 </td>

 <td> 

``` abap
"------------------------ Flat and deep data object -----------------------
"Flat:
"- Property of a data type, where the content of its data objects represents 
"  the actual work data.
"- All elementary data types except string and xstring are flat
"Deep:
"- Dynamic data objects and reference variables are deep, and they contain 
"  references that refer to the actual content.
"- The handling of references is implicit for dynamic data objects (strings 
"  and internal tables), and explicit for reference variables.
"- Structures that do not contain any deep components are flat structures. 
"  Structures that contain at least one deep component are deep structures.

"Flat elementary data object
DATA do_l_i TYPE i.

"Flat structure
DATA: BEGIN OF struc_b_flat,
        comp1 TYPE i,
        comp2 TYPE c LENGTH 3,
      END OF struc_b_flat.

"Deep elementary data object
DATA do_m_str TYPE string.

"Deep structure
DATA: BEGIN OF struc_c_deep,
        comp1 TYPE i,
        comp2 TYPE c LENGTH 3,
        comp3 TYPE string,  "string as deep data object
        comp4 TYPE string_table, "internal table as deep data object
      END OF struc_c_deep.
``` 

 </td>
</tr>

<tr>
<td> 

Named and unnamed data object

 </td>

 <td> 

``` abap
"Named: Data object that can be identified via a name.
"Unnamed: Data object that cannot be addressed by a name. Unnamed data 
"         objects are literals and anonymous data objects.

"Named data objects
DATA do_n_i TYPE i.
CONSTANTS con_b_str TYPE string VALUE `hi`.

"Unnamed data objects
"Literal that is output. It cannot be addressed via a dedicated name.
out->write( `I'm a literal...` ).

"Anonymous data object created using the NEW operator
"Can be addressed using reference variables or field symbols.

DATA(dref_c_str) = NEW string( `hi` ).

"Anonymous data object created inline using the NEW addition to the INTO 
"clause of a SELECT statement
SELECT *
  FROM zdemo_abap_carr
  INTO TABLE NEW @DATA(dref_d_tab)
  UP TO 3 ROWS.
``` 

 </td>
</tr>

<tr>
<td> 

Compatibility, convertibility and conversion regarding source and target data objects

 </td>

 <td> 

``` abap
"--- Compatibility, convertibility and conversion regarding source and ---
"--- target data objects -------------------------------------------------
"1. Source and target are compatible, all technical type properties 
"   match
DATA some_dobj TYPE c LENGTH 1 VALUE 'A'.
DATA flag TYPE abap_bool VALUE 'X'.

"Assignment of compatible types; content transferred without
"conversion
some_dobj = flag.

"2. Source and target are not compatible but can be converted.
"   Content of source is converted according to conversion rules
"   and then transferred to the target. Two data types are
"   convertible if a conversion rule exists for them.
DATA another_char TYPE c LENGTH 3 VALUE 'abc'.
DATA some_str     TYPE string VALUE `defghij`.

another_char = some_str.

"3. Data objects are neither compatible nor convertible. No
"   assignment possible. If recognized by the syntax check, a
"   syntax error is raised, otherwise an uncatchable exception
"   is raised when the program is executed.
DATA some_number TYPE i VALUE 123.
DATA itab_str    TYPE string_table.

"Type conversion not possible
"itab_str = some_number.
"some_number = itab_str.  
``` 

 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Additions for Creating Data Types and Objects

The following table shows a selection of additions for creating data types and objects. The examples randomly show `DATA` or `TYPES` statements.


<table>

<tr>
<td> Addition </td> <td> Code Snippet </td>
</tr>

<tr>
<td> 

`TYPE`

 </td>

 <td> 


``` abap
"------------ TYPE ----------------

"--- Elementary types/data objects ---

"Built-in type
DATA elem1 TYPE string.
DATA elem2 TYPE i.
"Local elementary type
TYPES c3 TYPE c LENGTH 3.
DATA elem3 TYPE c3.
"Global elementary types (e.g. DDIC data elements, CDS simple types,
"elementary types declared in global classes/interfaces, components of
"local and global structures)
DATA elem4 TYPE abap_boolean.
DATA elem5 TYPE timestampl.
DATA elem6 TYPE zcl_demo_abap_dtype_dobj=>t_pub_text_c30.
DATA elem7 TYPE zdemo_abap_get_data_itf=>occ_rate.
DATA elem8 TYPE zdemo_abap_carr-carrid.
DATA elem9 TYPE zdemo_abap_carr_ve-carrname.

"--- Structured types/data objects ---

"Local structured type
TYPES: BEGIN OF struct_ty1,
          comp1 TYPE i,
          comp2 TYPE string,
          comp3 TYPE c LENGTH 3,
        END OF struct_ty1.

DATA struct1 TYPE struct_ty1.

DATA: BEGIN OF struct2,
        comp1 TYPE xstring,
        comp2 TYPE timestampl,
        comp3 TYPE abap_boolean,
      END OF struct2.

"Global structured types (e.g. DDIC database tables, DDIC structures,
"CDS entities, structured types declared in global classes/interfaces)

"Database table
DATA struct3 TYPE zdemo_abap_carr.
"View entity
DATA struct4 TYPE zdemo_abap_carr_ve.
TYPES struct_ty3 TYPE zdemo_abap_fli_ve.
DATA struct5 TYPE zdemo_abap_fli_ve.
"CDS abstract entity
TYPES struct_ty4 TYPE zdemo_abap_abstract_ent.
DATA struct6 TYPE zdemo_abap_abstract_ent.
"CDS table function
TYPES struct_ty5 TYPE zdemo_abap_table_function.
DATA struct7 TYPE zdemo_abap_table_function.
"Structured types declared in global classes/interfaces
DATA struct8 TYPE zcl_demo_abap_amdp=>carr_fli_struc.

"--- Table types ---

"Referring to global table types with TYPE
DATA itab1 TYPE string_table.
DATA itab2 TYPE xstring_table.
DATA itab3 TYPE string_hashed_table.

"Table types declared in global classes/interfaces
DATA itab5 TYPE zcl_demo_abap_amdp=>carr_tab.
``` 

 </td>
</tr>

<tr>
<td> 

`TYPE BEGIN OF`

 </td>

 <td> 


``` abap
"------------ TYPE BEGIN OF ----------------

"Structured types and data objects created with TYPE BEGIN OF ... END OF ...

TYPES: BEGIN OF some_struct_type,
          comp1 TYPE i,
          comp2 TYPE string,
          comp3 TYPE c LENGTH 3,
       END OF some_struct_type.

DATA: BEGIN OF some_struct,
        compa TYPE n LENGTH 5,
        compb TYPE xstring,
        compc TYPE p LENGTH 8 DECIMALS 2,
      END OF some_struct.
``` 

 </td>
</tr>

<tr>
<td> 

`TYPE TABLE OF`

 </td>

 <td> 


``` abap
"------------ TYPE TABLE OF ----------------

"Table types and internal tables created with TYPE TABLE OF
"The examples only include standard tables.

"Structured line types
"Local line/table type
TYPES: BEGIN OF struct_ty6,
          comp1 TYPE i,
          comp2 TYPE string,
          comp3 TYPE c LENGTH 3,
        END OF struct_ty6.
TYPES tab_ty1 TYPE TABLE OF struct_ty6 WITH EMPTY KEY.
DATA itab6 TYPE TABLE OF struct_ty6 WITH EMPTY KEY.

"Globally available line types (e.g. DDIC database tables, CDS entites, etc.)
DATA itab7 TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
DATA itab8 TYPE TABLE OF zdemo_abap_fli_ve WITH EMPTY KEY.
DATA itab9 TYPE TABLE OF zdemo_abap_table_function WITH EMPTY KEY.
DATA itab10 TYPE TABLE OF zcl_demo_abap_amdp=>carr_fli_struc WITH EMPTY KEY.

"Local and global elementary types
TYPES c20 TYPE c LENGTH 20.
DATA itab11 TYPE TABLE OF c20 WITH EMPTY KEY.
DATA itab12 TYPE TABLE OF string WITH EMPTY KEY.
DATA itab13 TYPE TABLE OF utclong WITH EMPTY KEY.
DATA itab14 TYPE TABLE OF zdemo_abap_carr-carrname WITH EMPTY KEY.
DATA itab15 TYPE TABLE OF zdemo_abap_get_data_itf=>occ_rate WITH EMPTY KEY.
``` 

 </td>
</tr>

<tr>
<td> 

`TYPE TABLE FOR/TYPE STRUCTURE FOR`

 </td>

 <td>

Find more information in the [ABAP for RAP: Entity Manipulation Language (ABAP EML)](08_EML_ABAP_for_RAP.md) cheat sheet.

 <br> 


``` abap
"------------ TYPE TABLE FOR/TYPE STRUCTURE FOR ----------------
"BDEF derived types in the context of RAP
DATA der_type1 TYPE TABLE FOR CREATE zdemo_abap_rap_ro_m.
TYPES der_type2 TYPE TABLE FOR UPDATE zdemo_abap_rap_ro_m.
DATA der_type3 TYPE TABLE FOR ACTION IMPORT zdemo_abap_rap_ro_m~multiply_by_2.
DATA der_type4 TYPE TABLE FOR READ IMPORT zdemo_abap_rap_ro_m.

TYPES der_type5 TYPE STRUCTURE FOR DELETE zdemo_abap_rap_ro_m.
DATA der_type6 TYPE STRUCTURE FOR PERMISSIONS REQUEST zdemo_abap_rap_ro_m.
``` 

 </td>
</tr>

<tr>
<td> 

`TYPE ... REF TO`

 </td>

 <td> 


``` abap
"------------ TYPE ... REF TO ----------------
"Reference types

"Reference to elemtary/structured types, including the
"generic type data
DATA ref1 TYPE REF TO i.
DATA ref2 TYPE REF TO string.
DATA ref3 TYPE REF TO zdemo_abap_carr.
DATA ref4 TYPE REF TO data.

"Classes and interfaces
DATA oref TYPE REF TO zcl_demo_abap_objects.
DATA iref TYPE REF TO zdemo_abap_get_data_itf.

"Table types
DATA ref5 TYPE TABLE OF REF TO data.
DATA ref6 TYPE TABLE OF REF TO i.
DATA ref7 TYPE TABLE OF REF TO zdemo_abap_carr.
DATA ref8 TYPE TABLE OF REF TO zcl_demo_abap_objects.
``` 

 </td>
</tr>


<tr>
<td> 

`TYPE LINE OF`

 </td>

 <td> 


``` abap
"------------ TYPE LINE OF ----------------
"Structured types based on the line type of internal tables

"Local table type
TYPES: BEGIN OF struct_ty7,
          comp1 TYPE i,
          comp2 TYPE string,
          comp3 TYPE c LENGTH 3,
        END OF struct_ty7.
TYPES tab_ty2 TYPE TABLE OF struct_ty7 WITH EMPTY KEY.

DATA struct9 TYPE LINE OF tab_ty2.
DATA struct10 TYPE LINE OF zcl_demo_abap_oo_inheritance_1=>t_log.
``` 

 </td>
</tr>

<tr>
<td> 

`LIKE`

 </td>

 <td> 


``` abap
"------------ LIKE ----------------
"Types/data objects resusing types of other data objects

"Elementary types/data objects
DATA copy_elem TYPE c LENGTH 3.

DATA like_elem1 LIKE copy_elem.
TYPES like_elem2 LIKE copy_elem.
DATA like_elem3 LIKE space.

DATA copy_struc1 TYPE zdemo_abap_carr.
DATA copy_struc2 TYPE zdemo_abap_fli_ve.

DATA like_elem4 LIKE copy_struc1-carrid.
DATA like_elem5 LIKE copy_struc2-fldate.
DATA like_elem6 LIKE sy-index.

"Structured types/data objects
DATA like_struc1 LIKE copy_struc1.
TYPES like_struc2 LIKE copy_struc2.

"Table types/internal tables
DATA copy_tab1 TYPE TABLE OF string.
DATA copy_tab2 TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.

DATA like_tab1 LIKE copy_tab1.
TYPES like_tab2 LIKE copy_tab2.
``` 

 </td>
</tr>

<tr>
<td> 

`LIKE ...`

 </td>

 <td> 


``` abap
"------------ LIKE ... ----------------

"--- Structured type/data object based on internal table using LIKE LINE OF ---
DATA like_line_tab1 TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
DATA like_line_tab2 TYPE TABLE OF string.

DATA struc_like_line LIKE LINE OF like_line_tab1.
DATA elem_like_line LIKE LINE OF like_line_tab2.

"--- Reference based on existing data object using LIKE REF TO ---
DATA str TYPE string.

DATA ref9 LIKE REF TO str.
DATA ref10 LIKE REF TO struc_like_line.

"--- Reference table based on existing data object using LIKE TABLE OF REF TO ---
DATA ref11 LIKE TABLE OF REF TO str.
DATA ref12 LIKE TABLE OF REF TO struc_like_line.
``` 

 </td>
</tr>

<tr>
<td> 

`BEGIN OF ENUM`

 </td>

 <td> 


``` abap
"------------ BEGIN OF ENUM ----------------
"Creating ABAP enumerated types and objects

"a) No explicit base type is specified, base type of the constants is i by default,
"   values are counted up (a = 0, b = 1, ...)
TYPES: BEGIN OF ENUM t_enum,
          a,
          b,
          c,
        END OF ENUM t_enum.

"b) Explicit base type is specified, start values are provided (one of them must be initial)
TYPES: basetype TYPE c LENGTH 2,
        BEGIN OF ENUM t_enum_base BASE TYPE basetype,
          d VALUE IS INITIAL,
          e VALUE 'ab',
          f VALUE 'cd',
          g VALUE 'ef',
        END OF ENUM t_enum_base.

"c) Optionally declaring an enumerated structure
TYPES: BEGIN OF ENUM t_enum_struc STRUCTURE en_struc BASE TYPE basetype,
          h VALUE IS INITIAL,
          i VALUE 'hi',
          j VALUE 'ab',
          k VALUE 'ap',
        END OF ENUM t_enum_struc STRUCTURE en_struc.


DATA enum1 TYPE t_enum.
DATA enum2 TYPE t_enum_base.
DATA enum3 TYPE t_enum_struc.
DATA enum LIKE en_struc.
``` 

 </td>
</tr>


<tr>
<td> 

Excursion: Dynamically creating data objects using `CREATE DATA`

 </td>

 <td> 

Find more information in the [Dynamic Programming](06_Dynamic_Programming.md) cheat sheet.

<br>

``` abap
"The examples show various statement patterns
DATA dref TYPE REF TO data.


"CREATE DATA dref TYPE (typename) ...
CREATE DATA dref TYPE ('STRING'). "Elementary data object
CREATE DATA dref TYPE ('ZDEMO_ABAP_CARR'). "Structure
CREATE DATA dref TYPE ('STRING_TABLE'). "Internal table

"CREATE DATA dref TYPE ... TABLE OF (typename) ...
CREATE DATA dref TYPE TABLE OF ('STRING'). "Internal table
CREATE DATA dref TYPE SORTED TABLE OF ('ZDEMO_ABAP_FLI') WITH UNIQUE KEY carrid.


"CREATE DATA dref TYPE REF TO (typename).
CREATE DATA dref TYPE REF TO ('STRING').
CREATE DATA dref TYPE REF TO ('ZDEMO_ABAP_FLI').

"CREATE DATA dref TYPE LINE OF (typename).
TYPES tab_ty3 TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.
CREATE DATA dref TYPE LINE OF ('TAB_TY3').

"CREATE DATA dref LIKE struc-(compname).
DATA struct11 TYPE zdemo_abap_fli.
CREATE DATA dref LIKE struct11-('CARRID').

"CREATE DATA dref TYPE (absolute_name).
CREATE DATA dref TYPE ('\TYPE=STRING').
"Getting an absolute type name; see more information further down
DATA(absolute_name) = cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_CARR' )->absolute_name.
CREATE DATA dref TYPE (absolute_name).

"CREATE DATA dref TYPE HANDLE type_description_object.
"Getting a type description object. Find more information about RTTI below.
DATA(tdo_elem) = cl_abap_elemdescr=>get_c( 4 ). "type c length 4
CREATE DATA dref TYPE HANDLE tdo_elem.
``` 

 </td>
</tr>

</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Declaration Context of Data Types

The declaration context of data types (and objects) determines the validity and visibility.

- Program-local data types with `TYPES` statements
  - Local declarations in procedures
    - For example, methods. The local data and types can only be addressed within the method itself.
  - Class and interface attributes
    - Data types and objects can be declared in the declaration part of classes and interfaces. In classes, the visibility sections, as the name implies, determine how the attributes are visible. For example, attributes declared in the `PUBLIC SECTION` are visible globally. 
    - Note the difference between [static](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_attribute_glosry.htm) and [instance attributes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_attribute_glosry.htm). Instance attributes of classes are bound to the lifetime of objects. They are created when an object is instantiated.
- Program-independent data types 
  - Declarations in the ABAP Dictionary, which is a special storage for the declarations of data types that are visible in all repository objects, provided that the [package](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpackage_glosry.htm) check allows this.
    - The data types of the ABAP Dictionary are not declared with the `TYPES` statement, but using ADT tools (and/or SAP GUI tools in systems where SAP GUI is available).
    - The DDIC has many more [built-in types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_ddic_type_glosry.htm) than ABAP. These types have other names. They cannot be used in ABAP programs. 
    - The DDIC provides many options for defining types, including elementary data types (defined as data elements), reference types, complex types such as structured types and table types. Note that the name of a database table or a view can be used in type declarations to address the line type of these repository objects (for example, a structure: `DATA a TYPE some_db_table.`).
    - Furthermore, CDS also artifacts constitute global data types that can be referred to in ABAP.
    - Note the following trap: Local declarations hide global declarations of the same name. 
    - Find more information in the [ABAP Dictionary](26_ABAP_Dictionary.md) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions

### Getting Type Information and Creating Types/Data Objects at Runtime

Using [Runtime Type Services (RTTS)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_services_glosry.htm "Glossary Entry")
you can ...
- get type information on data objects, data types or [instances](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_glosry.htm "Glossary Entry") at runtime ([Runtime Type Identification (RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm "Glossary Entry")).
- define and create new data types as [type description objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_object_glosry.htm) at runtime ([Runtime Type Creation (RTTC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_creation_glosry.htm "Glossary Entry")).

For more information, see the [Dynamic Programming](06_Dynamic_Programming.md) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Ranges Tables

- Internal tables that have the predefined columns `SIGN`, `OPTION`, `LOW`, and `HIGH` 
- Declared with the `TYPE RANGE OF` addition in `DATA` and `TYPES` statements 
- Used to store range conditions that can be evaluated in expressions using the `IN` operator (each row in the table represents a separate comparison)
- Find more information and a code snippet in the [Internal Table](./01_Internal_Tables.md#ranges-tables) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Typed Literals in ABAP SQL

Typed literal:
- Literal whose data types is defined by specifying a [built-in dictionary type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_builtin_types.htm) explicitly. 
- Available for most but not all ABAP Dictionary data types.
- Can be used in ABAP SQL and in ABAP CDS. 
- Advantages of typed literals over untyped literals
  - Allow type-safe use of literals
  - Eliminate the need for (implicit type) conversions and casts, which can lead to surprising or erroneous results; also consider the conversion costs in terms of performance (typed literals are passed to the database and evaluated there without ABAP-specific type conversions). 
  - For better readability (you can immediately see what type is being used)
- More information: Typed literals in [ABAP SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_typed_literals.htm) ([cast expressions in ABAP SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_cast.htm)) and [ABAP CDS](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_typed_literal_v2.htm)

```abap
"Specifying a built-in ABAP Dictionary type instead of an
"untyped literal as in the example below to foster readibility.
SELECT SINGLE
  FROM zdemo_abap_fli
  FIELDS *
  WHERE fldate = datn`20240102`
  INTO @DATA(wa_typed_literals).

"Specifying an untyped literal
SELECT SINGLE
  FROM zdemo_abap_fli
  FIELDS *
  WHERE fldate = '20240102'
  INTO @DATA(wa_untyped_literals).

"Miscellaneous typed literals in an ABAP SQL statement
"Note that typed literals can be specified in read
"positions where host variables are possible.
DATA(tmstamp) = CONV timestamp( '20240808112517' ).
DATA(some_string) = `Some string`.
SELECT SINGLE
  FROM zdemo_abap_fli
  FIELDS
    carrid,
    @some_string AS host_var,
    char`X` AS flag,
    int8`32984723948723` AS int8,
    raw`11` AS raw,
    numc`1234` AS numc,
    utclong`2024-01-01T10:01:02,2` AS utc,
    tims`101507` AS tims,
    curr`173.95` AS curr,
    "Multiple cast expressions splitting a time stamp into date and time parts
    CAST( CAST( div( @tmstamp, 1000000 ) AS CHAR ) AS DATS ) AS date,
    CAST( substring( CAST( @tmstamp AS CHAR ), 9, 6 ) AS TIMS ) AS time,
    "Untyped literal
    'ABAP' AS txt
  WHERE fldate = datn`20240102`
  INTO @DATA(wa_misc_typed_literals).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Non-Admissible Values of Literals

Syntax warnings are displayed when using literals that represent invalid values for target types. The following example demonstrates the assignment of literals using admissible and non-admissible values. You can copy and paste the code into a demo class in your SAP BTP ABAP Environment to explore the syntax warnings.


```abap
DATA char3 TYPE c LENGTH 3.

"Value is admissable and convertible
char3 = 'abc'.

"Non-admissable value assigned to the target (type c length 6)
char3 = 'defghi'.

DATA date TYPE d.

"Value is admissable and convertible
date = '20250101'.

"Non-admissable value assigned to the target
"Type i
date = 20250101.
"More characters than type d
date = '20250101234'.
"Fewer characters than type d
date = '202511'.
```    

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

[zcl_demo_abap_dtype_dobj](./src/zcl_demo_abap_dtype_dobj.clas.abap)

> [!NOTE]
> - The executable example ...
>   - covers, among others, the following topics:
>     - Declaring data types
>     - Declaring data objects, inline declarations
>     - Assignments
>     - Creating anonymous data objects
>     - Type conversions
>     - Getting type information (Runtime Type Information/RTTI)
>     - Constants, immutable variables
>     - Generic types
>     - Enumerated types and objects
>   - does not have as many things to be output compared to other ABAP cheat sheet executable examples. The focus is on syntax options and declarations. In the class, you can set breakpoints and use the debugger to check out data objects. You can also use the F2 information for the many types and data objects. Simply select a type or object in the code and press F2 in ADT to check out the information.
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)



