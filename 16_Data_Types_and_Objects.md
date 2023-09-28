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
  - [Data Objects](#data-objects)
    - [Declaring Data Objects](#declaring-data-objects)
    - [Assigning Values to Data Objects](#assigning-values-to-data-objects)
    - [Creating Data Objects Using Inline Declaration](#creating-data-objects-using-inline-declaration)
    - [Assigning References to Data Reference Variables](#assigning-references-to-data-reference-variables)
    - [Creating Anonymous Data Objects](#creating-anonymous-data-objects)
    - [Constants and Immutable Variables](#constants-and-immutable-variables)
  - [Type Conversion](#type-conversion)
  - [Terms Related to Data Types and Objects in a Nutshell](#terms-related-to-data-types-and-objects-in-a-nutshell)
  - [Notes on the Declaration Context](#notes-on-the-declaration-context)
  - [Excursions](#excursions)
    - [Enumerated Types and Objects](#enumerated-types-and-objects)
    - [Getting Type Information and Creating Types at Runtime](#getting-type-information-and-creating-types-at-runtime)
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
> **💡 Note**<br>
> - Note: Data types in the ABAP Dictionary are either created directly as [repository objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrepository_object_glosry.htm) ([DDIC data elements](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_element_glosry.htm)) or in a type pool (only in [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm)). [Database tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendatabase_table_glosry.htm), [CDS entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_entity_glosry.htm) and their components can also be used as data types in ABAP programs.
> - Their existence and visibility depends on the declaration context.

Data objects: 
- Are objects (or [instances](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_glosry.htm)) of a data type (similar to objects/instances of classes in [ABAP Objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_objects_glosry.htm)).
- Occupy memory space and exist in different forms, for example, numeric or textual data can be contained in data objects.
- The type of data that a data object can receive is determined by its data type.
- Like data types, their existence and visibility depend on the declaration context.
- Are usually used in [ABAP statements](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_statement_glosry.htm) by specifying them in the [operand position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm).


> **💡 Note**<br>
> There are several differentations that further distinguish and characterize data types and objects. See [here](#glossary-terms-in-a-nutshell).

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

> **💡 Note**<br>
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
   - [Structured types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstructured_type_glosry.htm): Represent a sequence of arbitrary data type (i.e., they can be elementary, reference, or complex data types). The typical syntax element for the local definition of a structure is `... BEGIN OF ... END OF ...`.
   - [Table types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_type_glosry.htm): Consist of a sequence of any number of lines of the same data type. It can be any elementary type, reference type, or complex data type. The type definition includes other properties such as the [table category](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_category_glosry.htm) (defines how tables can be accessed) and [table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_key_glosry.htm) (to identify the table lines). The typical syntax element is `... TABLE OF ...`.
   - [Enumerated types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenum_type_glosry.htm): Specify a set of values in addition to the actual type properties. The typical syntax element is `... BEGIN OF ENUM ... END OF ENUM ...`. See more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenumerated_types_usage.htm) and further down.  
   - [Mesh types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmesh_type_glosry.htm): Special structured type that contains only table types with structured line types as components that can be linked using mesh associations. The typical syntax element is `... BEGIN OF MESH ... END OF MESH ...`. See more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes_mesh.htm).  
   - [BDEF derived types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_derived_type_glosry.htm): RAP-specific structured and table types. The typical syntax elements are `... TYPE STRUCTURE FOR ...` and `... TYPE TABLE FOR ...`. More information can be found [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrpm_derived_types.htm) and in the ABAP cheat sheet on ABAP EML.
- A data object of a complex type can be accessed as a whole or by component. 

> **💡 Note**<br>
> Structured and table types are used in this cheat sheet as examples for complex types. For more information, see the ABAP Keyword Documentation and the ABAP cheat sheets for structures and internal tables.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Reference Types
- Describe data objects that contain [references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_glosry.htm) to other objects (data objects and instances of classes), which are known as [reference variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_variable_glosry.htm). 
- There are two kinds of references: [Data references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_glosry.htm) (references to data objects) and [object references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_reference_glosry.htm) (references to objects as instances of classes). 
- A reference type must be defined either in the ABAP program or in the ABAP Dictionary. There are no built-in reference types in ABAP. 
- The typical syntax element is `... REF TO ...`.

> **💡 Note**<br>
> There are [generic ABAP types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengeneric_abap_type_glosry.htm). Generic data types are types that do not define all of the properties of a data object. They can only be used for the typing of [formal parameters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenformal_parameter_glosry.htm) and [field symbols](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm). 
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
      do_ctwo(2) TYPE c.

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
IF zcl_some_class=>read_only_attribute = `abc`.
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


> **💡 Note**<br>
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

> **💡 Note**<br>
> - There are conversion rules when assigning a source to a target data object that have different types. For more information, see the topic [Assignment and Conversion Rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rules.htm) in the ABAP Keyword Documentation, especially for complex types, since elementary types are usually demonstrated in the cheat sheet.
> - There are many ways to assigning values to data objects in ABAP. Assignments with the assignment operator `=` are mostly used here.
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

### Creating Data Objects Using Inline Declaration

An [inline declaration](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm) is made using the declaration operator [`DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_inline.htm). It can be specified in any designated [declaration position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_position_glosry.htm). The result of the declaration is used in the current operand position, is  statically visible from the current position, and is valid in the current context.

> **💡 Note**<br>
> - In an assignment, if the data object is declared inline on the left side, there are many options for what can be placed on the right side as shown in the previous section. The data type of the variable is determined by the operand type. It must be possible to derive this type completely statically.
> - For more information about the possible declaration positions, see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_positions.htm).    
> - You can use the [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm) declaration operator to create [immutable variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenimmutable_variable_glosry.htm), as shown below.
> - [Programming guidelines for inline declarations (F1 for Standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendeclaration_inline_guidl.htm)

 
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

"ABAP SQL statements
"A structure as target data object is created inline.
SELECT SINGLE * FROM zdemo_abap_carr INTO @DATA(struc_b5).
"NEW addition of the INTO clause creates a data reference variable
SELECT SINGLE * FROM zdemo_abap_carr INTO NEW @DATA(struc_ref).

"Internal table as target data object is created inline.
SELECT * FROM zdemo_abap_carr INTO TABLE @DATA(itab_b3).
SELECT * FROM zdemo_abap_carr INTO TABLE NEW @DATA(itab_ref).
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

"Upcast is possible for elementary data types
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

"Downcasts
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
"CREATE DATA statements
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

"NEW operator
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
"As mentioned above, constants cannot be changed at runtime.
CONSTANTS con_str TYPE string VALUE `hallo`.

"Constants as start values for dobj declarations following value
CONSTANTS con_underscores TYPE string VALUE `__________`.
DATA str_w_con_as_start_value TYPE string VALUE con_underscores.

"Immutable variables
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

## Type Conversion
A value assignment means that the value of a data object is transferred to a target data object. If the data types of the source and target are compatible, the content is copied unchanged. If they are incompatible and a suitable [conversion rule](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rule_glosry.htm) exists, the content is converted.
The following cases must be distinguished with regard to the data type: 
- The source and target data types are compatible, i.e. all technical type properties match. The content is transferred from the source to the target without being converted. 
- The source and target data types are incompatible, but can be converted. The content of the source is converted according to the conversion rules and then transferred to the target. Two data types are convertible if a conversion rule exists for them. An exception is raised if the content of the source cannot be handled according to the conversion rules.
- If the data objects are neither compatible nor convertible, no assignment can take place. If the syntax check detects this state, a syntax error is raised, otherwise an [uncatchable exception](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenuncatchable_exception_glosry.htm) is raised when the program is executed.

See the conversion rules for the different data types here: [Assignment and Conversion Rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rules.htm)

> **💡 Note**<br>
> - The [operands](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_glosry.htm) of many ABAP statements are assigned internally according to the assignment rules. 
> - Typically, assignements are made using the assignment operator `=`. If necessary and applicable, the type is converted implicitly. However, you can also use the conversion operator [`CONV`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_conv.htm) to convert types explicitly.
> - For [lossless assignments](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlossless_assignment_glosry.htm), the lossless operator [`EXACT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_exact.htm) can be used to perform checks before the conversion is performed to ensure that only valid values are assigned and that no values are lost in assignments.
> - In general, no checks are performed on assignments between compatible data objects. If a data object already contains an invalid value, for example, an invalid date or time in a date or time field, it is passed a valid value when the assignment is made to a compatible data object.
> - The `applies_to_data` method of the [RTTI](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm) class `cl_abap_datadescr` can be used to check type compatibility. See the executable example.
    

```abap
"Implicit conversions of the types c and string
DATA do_1_str TYPE string VALUE `abcedf`.
DATA do_2_c3 TYPE c LENGTH 3.
do_2_c3 = do_1_str.
"Result: do_2_c3: 'abc'

"Implicit conversions of the types i and decfloat34
DATA do_4_i TYPE i.
DATA do_5_dcfl34 TYPE decfloat34 VALUE '4.56'.
do_4_i = do_5_dcfl34.
"Result: do_4_i: 5

"Explicitly converting decfloat34 to i
DATA do_6_dcfl34 TYPE decfloat34 VALUE '2.78'.
DATA(do_7_i) = CONV i( do_6_dcfl34 ).
"Result: do_7_i: 3

"A maybe surprising result of a conversion
"from type i to string according to the rules
DATA(i2str) = CONV string( -10 ).
"Result: i2str: `10-`
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Terms Related to Data Types and Objects in a Nutshell

```abap
"Standalone and bound data types
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

**********************************************************************

"Complex and elementary data type/object
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

**********************************************************************

"Complete and generic data types
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

**********************************************************************

"Variable and constant data objects
"Variable: Named data object whose value can be changed during the runtime 
"          of an ABAP program.
"Constant: Named data object whose value cannot be changed at runtime.

"Variable
DATA do_g_i TYPE i VALUE 123.
do_g_i = 456.

CONSTANTS con_a_i TYPE i VALUE 789.
"Not changeable
"con_a_i = 321.

**********************************************************************

"Static and dynamic data objects
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
DATA do_h_c5 TYPE c LENGTH 3.
"Dynamic data object
DATA do_i_str TYPE string.

"Assignments
do_h_c5 = 'abc'.
do_h_c5 = 'defghi'. "only 'def' assigned -> length and memory use do not change

"Memory consumption changes for dynamic data objects
do_i_str = `abc`.
do_i_str = `d`.
do_i_str = `efghijklmnopqrstuvwxyz`.

**********************************************************************

"Static type and dynamic type
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

**********************************************************************

"Flat and deep data objects
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

**********************************************************************

"Named and unnamed data object
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

**********************************************************************

"Compatibility, convertibility and conversion regarding source and 
"target data objects
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

## Notes on the Declaration Context

The declaration context of data types (and objects) determines the validity and visibility.

- Program-local data types with `TYPES` statements
  - Local declarations in procedures
    - For example, methods. The local data and types can only be addressed within the method itself.
  - Classe and interface attributes
    - Data types and objects can be declared in the declaration part of classes and interfaces. In classes, the visibility sections, as the name implies, determine how the attributes are visible. For example, attributes declared in the `PUBLIC SECTION` are visible globally. 
    - Note the difference between [static](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_attribute_glosry.htm) and [instance attributes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_attribute_glosry.htm). Instance attributes of classes are bound to the lifetime of objects. They are created when an object is instantiated.
- Program-independent data types 
  - Declarations in the ABAP Dictionary, which is a special storage for the declarations of data types that are visible in all repository objects, provided that the [package](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpackage_glosry.htm) check allows this.
    - The data types of the ABAP Dictionary are not declared with the `TYPES` statement, but using ADT tools (and/or SAP GUI tools in systems where SAP GUI is available).
    - The DDIC has many more [built-in types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_ddic_type_glosry.htm) than ABAP. These types have other names. They cannot be used in ABAP programs. 
    - The DDIC provides many options for defining types, including elementary data types (defined as data elements), reference types, complex types such as structured types and table types. Note that the name of a database table or a view can be used in type declarations to address the line type of these repository objects (for example, a structure: `DATA a TYPE some_db_table.`).
    - Note the following trap: Local declarations hide global declarations of the same name. 

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions
### Enumerated Types and Objects
- ABAP supports the concept of enumerations. 
- Enumerations are a mixture of types and constants.
- An [enumerated type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenum_type_glosry.htm) specifies a value set in addition to the actual type properties. 
- [Enumerated objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenumerated_object_glosry.htm) - data objects with an enumerated type - are mainly used to check allowed values. This usually restricts the actual parameters passed to methods to the enumerated values defined in the class. [Enumerated variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenumerated_variable_glosry.htm) are variable enumerated objects. They can only contain the associated enumerated values. 

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
DATA dobj_enum TYPE enum_type.

dobj_enum = a.
```

Find more information on enumerated types in the (commented code of the) cheat sheet example and [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenumerated_types_usage.htm). 

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Getting Type Information and Creating Types at Runtime
Using [Runtime Type Identification (RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm "Glossary Entry"), you can get type information on data objects, data types or [instances](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_glosry.htm "Glossary Entry") at runtime ([Runtime Type Identification (RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm "Glossary Entry")).
Using [Runtime Type Creation (RTTC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_creation_glosry.htm "Glossary Entry"), you an define and create new data types as [type description objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_object_glosry.htm) at runtime.

Find more information in the [cheat sheet about dynamic programming](06_Dynamic_Programming.md#runtime-type-services-rtts). 


## Executable Example

[zcl_demo_abap_dtype_dobj](./src/zcl_demo_abap_dtype_dobj.clas.abap)

> **💡 Note**<br>
> - [Here](README.md#-getting-started-with-the-examples) are the steps to import and run the code.
> - Compared to other ABAP cheat sheet executable examples, this one does not have as many things to be output. The focus is on syntax options and declarations. In the class, you can set breakpoints and use the debugger to check out data objects. You can also use the F2 information for the many types and data objects. Simply select a type or object in the code and press F2 in ADT to check out the information.
> - The executable example also covers topics not covered above such as built-in data objects and formal parameters typed with generic ABAP types.


