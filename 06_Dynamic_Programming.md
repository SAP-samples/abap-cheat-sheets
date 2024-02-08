<a name="top"></a>

# Dynamic Programming

- [Dynamic Programming](#dynamic-programming)
  - [Introduction](#introduction)
  - [Excursion: Field Symbols and Data References](#excursion-field-symbols-and-data-references)
    - [Field Symbols](#field-symbols)
    - [Data References](#data-references)
  - [Dynamic ABAP Statements](#dynamic-abap-statements)
    - [Dynamic ASSIGN Statements](#dynamic-assign-statements)
    - [Dynamically Specifying Data Types/Creating (Data) Objects](#dynamically-specifying-data-typescreating-data-objects)
    - [Accessing Structure Components Dynamically](#accessing-structure-components-dynamically)
    - [Dynamic Specifications in Statements for Processing Internal Tables](#dynamic-specifications-in-statements-for-processing-internal-tables)
    - [Dynamic ABAP SQL Statements](#dynamic-abap-sql-statements)
    - [Dynamic Invoke](#dynamic-invoke)
    - [Validating Input for Dynamic Specifications (CL\_ABAP\_DYN\_PRG)](#validating-input-for-dynamic-specifications-cl_abap_dyn_prg)
  - [Runtime Type Services (RTTS)](#runtime-type-services-rtts)
    - [Getting Type Information at Runtime](#getting-type-information-at-runtime)
    - [Dynamically Creating Data Types at Runtime](#dynamically-creating-data-types-at-runtime)
    - [Dynamically Creating Data Objects at Runtime](#dynamically-creating-data-objects-at-runtime)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)

## Introduction

-  Regarding "dynamic" in contrast to "static" aspects, [ABAP programs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm) can include both dynamic and static parts.
- Consider a [data object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm "Glossary Entry") in your program:
    - It can be declared as a [static data object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_data_object_glosry.htm), i. e. you provide all attributes by specifying the [data type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_type_glosry.htm) and more statically in the code.

        ```abap
        "Internal table declaration
        DATA itab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
        ```

   - The name `itab` of the data object is determined at compile time and remains stable throughout the execution of the program.
- However, there can also be use cases where the attributes of such a data object are not statically determined. This is where dynamic aspects enter the picture: Attributes, names, types etc. are not determined at compile time but rather at runtime.
- There are ABAP statements that include these dynamic aspects in the syntax. Assume you have a simple program and a UI that includes an input field storing the input in a data object named `dbtab`. As input, you expect the name of a database table to be provided. In the end, you want to retrieve all entries of the database table and store them in an internal table. This table should be displayed. So, there is random input at runtime and your program must be able to deal with it.
   - See the following `SELECT` statement. The `FROM` clause does not include a statically defined table to be selected from. Instead, there is a pair of parentheses including a data object. It is a character-like data object. Assume the data object holds the name of the database table. At runtime, the data retrieval happens from the database table that was inserted in the input field.

        ```abap
        DATA(dbtab) = `ZDEMO_ABAP_FLI`.
        
        SELECT *
          FROM (dbtab)
          INTO TABLE @DATA(some_itab).
        ```

- Further aspects for dynamic programming in ABAP enter the picture if you want to determine information about data types and data objects at runtime ([RTTI](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm)) or even create them ([RTTC](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_creation_glosry.htm)).

- In general, dynamic programming also comes with some downsides. For example, the ABAP compiler cannot check the dynamic programming feature like the `SELECT` statement mentioned above. There is no syntax warning or suchlike (note the `CL_ABAP_DYN_PRG` class that supports dynamic programming). The checks are performed only at runtime, which has an impact on the performance. Plus, the  testing of [procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm "Glossary Entry")
that include dynamic programming features may be difficult.


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursion: Field Symbols and Data References

[Field symbols](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm "Glossary Entry")
and [data references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_glosry.htm "Glossary Entry") are covered here since they are supporting elements for dynamic programming.

### Field Symbols

Field symbols ...

- are symbolic names for almost any data object or parts of existing data objects.
- can be assigned actual memory areas at program runtime (using `ASSIGN`). Note that you can only work with the field symbols if indeed they have been assigned before.
- can be used as placeholders for a data object at an [operand position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm).
   - Consider there is a data object in your program. A field symbol is also available that is assigned the memory area of this data object. Accessing a field symbol is like accessing the [named data object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennamed_data_object_glosry.htm) or part of the object itself.
- do not reserve physical space in the [data area](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_area_glosry.htm) of a program like a data object. Instead, they work as dynamic identifiers of a memory area in which a specific data object or part of an object is located.
- can be typed either with [generic data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengeneric_data_type_glosry.htm "Glossary Entry") or [complete data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplete_data_type_glosry.htm "Glossary Entry"). 
- are declared using the statement [`FIELD-SYMBOLS`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapfield-symbols.htm) or the [declaration operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_operator_glosry.htm) [`FIELD-SYMBOL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield-symbol_inline.htm). Their names must be included between angle brackets.

**Declaring field symbols**

Syntax:
``` abap
"Declaring field symbols using the FIELD-SYMBOLS statement
"and providing a complete/generic type

"Examples for complete types
FIELD-SYMBOLS: <fs_i>        TYPE i,
               <fs_fli>      TYPE zdemo_abap_fli,
               <fs_tab_type> TYPE LINE OF some_table_type,
               <fs_like>     LIKE some_data_object.

"Examples for generic types (see more examples further down)
FIELD-SYMBOLS <fs_c>         TYPE c.           "Text field with a generic length
FIELD-SYMBOLS <fs_cseq>      TYPE csequence.   "Text-like (c, string)
FIELD-SYMBOLS <fs_data>      TYPE data.        "Any data type
FIELD-SYMBOLS <fs_any_table> TYPE any table.   "Internal table with any table type

"Declaring field symbols inline
"In an inline declaration, the typing of the field symbol is done 
"with the generic type data.
"Example use case: Inline declaration of a field symbol for an internal table.
LOOP AT itab ASSIGNING FIELD-SYMBOL(<line>).
  ...
ENDLOOP.
```

> **💡 Note**<br>
>- After its declaration, a field symbol is initial, i. e. a memory area is not (yet) assigned to it (apart from the inline declaration). If you use an unassigned field symbol, an exception is raised.
>-   There are plenty of options for generic ABAP types. A prominent one
    is `data` that stands for any data type. See more information in the
    topic [Generic ABAP
    Types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuilt_in_types_generic.htm) and in a code snippet below.
>-   Field symbols cannot be declared in the declaration part of
    [classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_glosry.htm "Glossary Entry")
    and
    [interfaces](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoo_intf_glosry.htm "Glossary Entry").

**Assigning data objects**

[`ASSIGN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapassign.htm)
statements assign the memory area of a data object to a field symbol.
Once the memory area is assigned, you can work with the content.

``` abap
"Some data object declarations to be used
DATA: num      TYPE i,
      struc    TYPE zdemo_abap_fli,  "Demo database table
      itab_str TYPE string_table,
      itab_fli TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.
APPEND INITIAL LINE TO itab_fli.

"Declaring field symbols with complete types
FIELD-SYMBOLS: <fs_i>     TYPE i,
               <fs_struc> TYPE zdemo_abap_fli,
               <fs_tab>   TYPE string_table.

"Declaring field symbols with generic type
FIELD-SYMBOLS <fs_gen> TYPE data.

"Assigning data objects to field symbols
"Using field symbols with a static type
ASSIGN num   TO <fs_i>.
ASSIGN struc TO <fs_struc>.
ASSIGN itab_str TO <fs_tab>.
"Using field symbol with a generic type
ASSIGN num   TO <fs_gen>.
ASSIGN itab_fli TO <fs_gen>.
ASSIGN itab_fli[ 1 ] TO <fs_gen>.
"Assigning components
ASSIGN struc-carrid TO <fs_gen>.
ASSIGN itab_fli[ 1 ]-connid TO <fs_gen>.

"Inline declaration (the field symbol has the type data)
ASSIGN num TO FIELD-SYMBOL(<fs_inl>).

"CASTING addition for matching types of data object and field
"symbol when assigning memory areas
TYPES c_len_3 TYPE c LENGTH 3.
DATA(chars) = 'abcdefg'.

FIELD-SYMBOLS <fs1> TYPE c_len_3. 

"Implicit casting
ASSIGN chars TO <fs1> CASTING. "abc

FIELD-SYMBOLS <fs2> TYPE data.

"Explicit casting
ASSIGN chars TO <fs2> CASTING TYPE c_len_3. "abc

DATA chars_l4 TYPE c LENGTH 4.
ASSIGN chars TO <fs2> CASTING LIKE chars_l4. "abcd
```

> **💡 Note**<br>
> - If you use an unassigned field symbol, an exception is raised. Before using it, you can check the
    assignment with the following logical expression. The statement is true if the field symbol is assigned.
>    ``` abap
>    IF <fs> IS ASSIGNED.
>      ...
>    ENDIF.
>    
>    DATA(check) = COND #( WHEN <fs> IS ASSIGNED THEN `assigned` ELSE `not assigned` ).
>    ```
>- Using the statement `UNASSIGN`, you can explicitly remove the assignment of the field symbol. A `CLEAR` statement only initializes the value.
>   ``` abap
>   UNASSIGN <fs>.
>   ```
>- See more information on the addition `CASTING` [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapassign_casting.htm).

**Examples using field symbols**

``` abap
"Assignments
DATA num TYPE i VALUE 1.
FIELD-SYMBOLS <fs_i> TYPE i.
ASSIGN num TO <fs_i>.

<fs_i> = 2.
"The data object 'num' has now the value 2.

"Loops
"Here, field symbols are handy since you can avoid an
"actual copying of the table line.
SELECT * 
  FROM zdemo_abap_fli
  INTO TABLE @DATA(itab).

FIELD-SYMBOLS <fs1> LIKE LINE OF itab.

LOOP AT itab ASSIGNING <fs1>.
  <fs1>-carrid = ... "The field symbol represents a line of the table.
  <fs1>-connid = ... "Components are accessed with the component selector.
                     "Here, it is assumed that a new value is assigned.
  ...
ENDLOOP.

"Inline declaration of a field symbol. The field symbol is implcitly typed
"with the generic type data.
LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs2>).
  <fs2>-carrid = ...
  <fs2>-connid = ...
  ...
ENDLOOP.

"----------- Generic typing -----------
"- Generic types are available with which formal parameters of methods or field symbols
"  can be specified.
"- At runtime, the actual data type is copied from the assigned actual parameter or
"  memory area, i.e. they receive the complete data type only when an actual parameter
"  is passed or a memory area is assigned.

"The following code snippet demonstrates generic types with field symbols.
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
  <decfloat>       TYPE decfloat, "decfloat16, decfloat34)
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
  <simple>         TYPE simple, "Elementary data type including enumerated types and
                                "structured types with exclusively character-like flat components
  <object>         TYPE REF TO object. "object can only be specified after REF TO; can point to any object

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
        oref      TYPE REF TO object,
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

ASSIGN s-oref TO <object>.
s-oref = NEW zcl_demo_abap_objects( ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Data References

[Data references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_glosry.htm "Glossary Entry")
...

-   are references that point to any data object or to their parts (for example, components, lines of internal tables).
-   are contained in [data reference variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry")
    in ABAP programs.

[Data reference variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry")
...

- are data objects that contain a reference.
- are "opaque", i. e. the contained references cannot be accessed directly. To access the content, these variables must be dereferenced first.
- are [deep](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_glosry.htm "Glossary Entry") data objects like strings and   internal tables.
- are typed with the addition [`REF TO`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes_references.htm) followed by a [static type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_type_glosry.htm). Note the [dynamic type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendynamic_type_glosry.htm) in this context: The dynamic type of such a variable is the data type to which it actually points. This concept is particularly relevant in the context of assignments (see the assignment rules [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_references.htm)).
- can be typed with a complete or generic type. However, only `data` can be used as generic type.

> **💡 Note**<br>
> [Object references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_reference_glosry.htm "Glossary Entry")
and [object reference variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm "Glossary Entry") are not part of this cheat sheet. To get more details, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_reference_type.htm) or the cheat sheet [ABAP Object Orientation](04_ABAP_Object_Orientation.md).

**Declaring data reference variables**

``` abap
"Example declarations of data reference variables with static types.
"The static types can be complete or generic (but only data can be used). 
"Note that they do not yet point to a data object. At this stage, 
"initial reference variables contain null references.
DATA: ref_a TYPE REF TO i,                 "Complete data type
      ref_b TYPE REF TO some_dbtab,        "Complete data type
      ref_c LIKE REF TO some_data_object,
      ref_d TYPE REF TO data,              "Generic data type
      ref_e LIKE ref_a.                    "Referring to an existing data reference variable                  
```

As shown below, instead of the explicit declaration, inline declarations are also possible. 
See also the cheat sheet [Data Types and Data Objects](16_Data_Types_and_Objects.md).

**Assigning references to existing data objects** using the
[reference operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_operator_glosry.htm "Glossary Entry")
`REF`.
``` abap
"Declaring a data object
DATA num TYPE i VALUE 5.

"Declaring data reference variables
DATA ref1    TYPE REF TO i.
DATA ref_gen TYPE REF TO data.

"Creating data references to data objects.
"The # character stands for a data type that is determined in the 
"following hierarchy: 
"- If the data type required in an operand position is unique and 
"  known completely, the operand type is used.
"- If the operand type cannot be derived from the context, the 
"  data type of the data object within the parentheses is used.
"- If the data type of the data object within the parentheses is 
"  not known statically, the generic type data is used.

ref1    = REF #( num ).
ref_gen = REF #( num ).

"Creating a data reference variable inline. 
"Note: No empty parentheses can be specified after REF.
DATA(ref2) = REF #( num ).

"Data reference variable of type ref to data by specifying the 
"generic type data after REF 
DATA(ref3) = REF data( ... ).

"A non-generic type can be used; only if an upcast works (see 
"upcasts below)
DATA(ref3) = REF some_type( ... ).

"The older syntax GET REFERENCE having the same effect should 
"not be used anymore.
"GET REFERENCE OF num INTO ref1.
"GET REFERENCE OF num INTO DATA(ref5).
```

**Creating new data objects at runtime**: 
[Anonymous data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm "Glossary Entry") ...
- are dynamically created at runtime. 
- are relevant if the data type is only known when the program is executed.
- cannot be addressed by a name ("anonymous"). 
- expect a data reference variable when declared. The content of an anonymous data object can only be accessed using dereferenced variables as shown below or field symbols.
- can be created using the statement [`CREATE DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_data.htm), the instance operator [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm), or the addition [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_into_target.htm) of the `INTO` clause in a `SELECT` statement. 

> **💡 Note**<br>
> The following snippet covers statically defined types. Data objects can also be created with `CREATE DATA` dynamically using dynamic type definitions (the type name is specified within a pair of parentheses) and type description objects ([`TYPE HANDLE` addition](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcreate_data_handle.htm)) as shown further down. 
> Using [`CREATE OBJECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_object.htm) statements, you can create an object as an instance of a class and assign the reference to the object to an [object reference variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm). Find more information in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_object.htm).

```abap
"CREATE DATA statements
"Note that there are many additions available. The examples 
"show a selection. Behind TYPE and LIKE, the syntax offers 
"the same possibilities as the DATA statement.

"Creating an anonymous data object with an implicit type.
"If neither of the additions TYPE or LIKE are specified, the 
"data reference variable must be completely typed.
DATA dref_1 TYPE REF TO string.
CREATE DATA dref_1.

"Creating anonymous data objects with explicit data type 
"specification. 
"Data reference variable with a generic type to be used in 
"the following examples for the anonymous data object.
DATA dref_2 TYPE REF TO data.

"Elementary, built-in ABAP type
CREATE DATA dref_2 TYPE p LENGTH 8 DECIMALS 3.

"Anomyous internal table ...
"using the LIKE addition to refer to an existing internal table
DATA itab TYPE TABLE OF zdemo_abap_carr.
CREATE DATA dref_2 LIKE itab.

"by specifying the entire table type
CREATE DATA dref_2 TYPE HASHED TABLE OF zdemo_abap_carr 
  WITH UNIQUE KEY carrid.

"Anonymous structures
CREATE DATA dref_2 LIKE LINE OF itab.
CREATE DATA dref_2 TYPE zdemo_abap_carr.

"Creating reference variable
CREATE DATA dref_2 TYPE REF TO itab.

"NEW operator
"- Works like CREATE DATA dref TYPE type statements and can 
"  be used in general expression positions.
"- Allows to assign values to the new anonymous data objects 
"  in parentheses

"Creating data reference variables
DATA: dref_3 TYPE REF TO i,
      dref_4 TYPE REF TO data.

"# character after NEW if the data type can be identified 
"completely instead of the explicit type specification (only 
"non-generic types possible)
dref_3 = NEW #( 123 ).
dref_3 = NEW i( 456 ).
dref_4 = NEW zdemo_abap_carr( ). "not assigning any values
dref_4 = NEW string( `hi` ).

"Creating anonymous data objects inline
"In doing so, you can omit a prior declaration of a variable.
DATA(dref_5) = NEW i( 789 ).
DATA(dref_6) = NEW zdemo_abap_carr( carrid   = 'AB' 
                                    carrname = 'AB Airlines' ).

"ABAP SQL SELECT statements
"Using the NEW addition in the INTO clause, an anonymous data
"object can be created in place.
SELECT *
  FROM zdemo_abap_carr
  INTO TABLE NEW @DATA(dref_7). "internal table

SELECT SINGLE *
  FROM zdemo_abap_carr
  INTO NEW @DATA(dref_8). "structure
```  

**Assignments between two data reference variables**. As mentioned above regarding the assignment, note that static types of both data
reference variables must be compatible. As a result of an assignment, both the target reference variable and the source reference variable point to the same data object.

Excursion: Static vs. dynamic type, upcasts and downcasts
- Data reference variables have ...
  - a [static type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_type_glosry.htm "Glossary Entry"). This is the type you specify when declaring the variable, i. e. `i` is the static type in this example: `DATA ref TYPE REF TO i.`. The static type can also be a generic data type: `DATA ref TYPE REF TO data.`.
  - a [dynamic type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendynamic_type_glosry.htm "Glossary Entry"), the type of a data object to which the reference variable actually points to at runtime.
- For an assignment to work, the differentiation is particularly relevant since the following basic rule applies: The static type of the target reference variable must be more general than or the same as the dynamic type of the source reference variable.

- This is where the concepts of [upcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenup_cast_glosry.htm "Glossary Entry") and [downcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendown_cast_glosry.htm "Glossary Entry") enter the picture.
  - Up and down? This concept originates from the idea of moving up or down in an [inheritance tree](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninheritance_tree_glosry.htm). In an assignment between reference variables, the target variable inherits the dynamic type of the source variable.
  - Upcast: If the static type of the target variables is **less specific or the same** as the static type of the source variable, an assignment is possible. This includes, for example, assignments with the [assignment operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm) `=`.
  - Downcast: If the static type of the target variable is **more specific** than the static type of the source variable, a check must be made at runtime before the assignment is done. If you indeed want to trigger such a downcast, you must do it explicitly in your code. You can do this, for example, using the
  [constructor operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_operator_glosry.htm "Glossary Entry")
[`CAST`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_cast.htm). In older code, you may see the use of the [`?=`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove_cast.htm) operator.
  - In contrast to a downcast, an upcast does not have to be done explicitly. However, you can - but need not - use the mentioned operators for upcasts, too.


``` abap
"Examples demonstrating up- and downcasts

"Declaring data reference variables
DATA ref1 TYPE REF TO i.
DATA ref2 TYPE REF TO i.

ref1 = NEW #( 789 ).

"Assignments
ref2 = ref1.

"Casting

"Complete type
DATA(ref3) = NEW i( 321 ).

"Generic type
DATA ref4 TYPE REF TO data.

"Upcast
ref4 = ref3.

"Downcasts
DATA ref5 TYPE REF TO i.

"Generic type
DATA ref6 TYPE REF TO data.

ref6 = NEW i( 654 ).
ref5 = CAST #( ref6 ).

"Casting operator in older syntax
"ref5 ?= ref6.

"Note: The cast operators can also but need not be specified for upcasts.
ref4 = CAST #( ref3 ).
```

**Addressing data references**

Before addressing the content of data objects a data reference points to, you must dereference data reference variables. Use the
[dereferencing operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm "Glossary Entry")
`->*`. To check if dereferencing works, you can use a logical expression with [`IS BOUND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_bound.htm).

``` abap
"Creating data reference variables and assign values

DATA(ref_i)    = NEW i( 1 ).
DATA(ref_carr) = NEW zdemo_abap_carr( carrid = 'LH' carrname = 'Lufthansa' ).

"Generic type

DATA ref_gen TYPE REF TO data.
ref_gen = ref_i.                "Copying reference

"Accessing

"Variable number receives the content.
DATA(number) = ref_i->*.

"Content of referenced data object is changed.
ref_i->* = 10.

"Data reference used in a logical expression.
IF ref_i->* > 5.
  ...
ENDIF.

"Dereferenced generic type
DATA(calc) = 1 + ref_gen->*.

"Structure
"Complete structure
DATA(struc) = ref_carr->*.

"When dereferencing a data reference variable that has a structured
"data type, you can use the component selector -> to address individual components
DATA(carrid) = ref_carr->carrid.
ref_carr->carrid = 'UA'.

"This longer syntax with the dereferencing operator also works.
ref_carr->*-carrname = 'United Airlines'.

"Checking if a data reference variable can be dereferenced.
IF ref_carr IS BOUND.
  ...
ENDIF.

DATA(ref_bound) = COND #( WHEN ref_carr IS BOUND THEN ref_carr->carrid ELSE `is not bound` ).

"Explicitly removing a reference
"However, the garbage collector takes care of removing the references
"automatically once the data is not used any more by a reference.
CLEAR ref_carr.
```

**Excursion: Generic data references and field symbols**

```abap
"Non-generic type
DATA ref_int TYPE REF TO i.
ref_int = NEW #( ).
ref_int->* = 123.

"Generic type
DATA ref_generic TYPE REF TO data.
ref_generic = NEW i( ). "Syntax in modern ABAP
CREATE DATA ref_generic TYPE i. "Syntax for older ABAP releases

"As mentioned above, the content of anonymous data objects can only be 
"accessed using dereferenced data variables and field symbols.
"The only option to access the variable in older releases was via field symbols.
ASSIGN ref_generic->* TO FIELD-SYMBOL(<fs_generic>).
<fs_generic> = 123.

"An access as the following, as it is possible in modern ABAP, was not possible.
ref_generic->* = 123.

"In modern ABAP, variables and field symbols of the generic types
"'any' and 'data' can be used directly, for example, in LOOP and READ statements.
DATA dref TYPE REF TO data.
CREATE DATA dref TYPE TABLE OF zdemo_abap_carr.
SELECT *
  FROM zdemo_abap_carr
  INTO TABLE @dref->*.

"Note: In case of a fully generic type, an explicit or implicit index operation
"is not possible (indicated by the examples commented out).
LOOP AT dref->* ASSIGNING FIELD-SYMBOL(<loop>).
  ...
ENDLOOP.
"LOOP AT dref->* ASSIGNING FIELD-SYMBOL(<loop2>) FROM 1 TO 4.
"ENDLOOP.

"The following examples use a dynamic key specification. 
"See more syntax examples below.
READ TABLE dref->* ASSIGNING FIELD-SYMBOL(<read>) WITH KEY ('CARRID') = 'AA'.
"READ TABLE dref->* INDEX 1 ASSIGNING FIELD-SYMBOL(<read2>).

"Table expressions
DATA(line) = CONV zdemo_abap_carr( dref->*[ ('CARRID') = 'AA' ] ).
dref->*[ ('CARRID') = 'AA' ] = VALUE zdemo_abap_carr( BASE dref->*[ ('CARRID') = 'AA' ] carrid = 'XY' ).
dref->*[ ('CARRID') = 'XY' ]-('CARRID') = 'ZZ'.

"Table functions
DATA(num_tab_lines) = lines( dref->* ).
DATA(idx) = line_index( dref->*[ ('CARRID') = 'LH' ] ).
```

**Examples using data references**

Some example contexts of using data references are as follows:

*Overwriting data reference variables*:
``` abap
dref = NEW i( 1 ).

"ref is overwritten here because a new object is created
"with a data reference variable already pointing to a data object
dref = NEW i( 2 ).
```

*Retaining data references*:

``` abap
"This snippet shows that three data references are created
"with the same reference variable. Storing them in an internal table
"using the type TYPE TABLE OF REF TO prevents the overwriting.

DATA: dref TYPE REF TO data,
      itab TYPE TABLE OF REF TO data,
      num  TYPE i VALUE 0.

DO 3 TIMES.
  "Adding up 1 to demonstrate a changed data object.
  num += 1.

  "Creating data reference and assigning value.
  "In the course of the loop, the variable gets overwritten.
  dref = NEW i( num ).

  "Adding the reference to itab
  itab = VALUE #( BASE itab ( dref ) ).
ENDDO.
```

*Processing internal tables*:

``` abap
"Similar use case to using field symbols: In a loop across an internal table,
"you assign the content of the line in a data reference variable
"instead of actually copying the content to boost performance.
"Again, the inline declaration comes in handy.

"Filling an internal table.
SELECT * 
  FROM zdemo_abap_fli
  INTO TABLE @DATA(fli_tab).

LOOP AT fli_tab REFERENCE INTO DATA(ref).

  "A component of the table line may be addressed.
  "Note the object component selector; the dereferencing operator together
  "with the component selector is also possible: ->*-
  ref->carrid = ...
  ...
ENDLOOP.

"More statements are available that assign content to a data reference variable, 
"for example, READ TABLE.
READ TABLE fli_tab INDEX 1 REFERENCE INTO DATA(rt_ref).
```

*Data reference variables as part of structures and internal tables*:
``` abap
"Unlike field symbols, data reference variables can be used as
"components of structures or columns in internal tables.

"Structure
DATA: BEGIN OF struc,
        num TYPE i,
        ref TYPE REF TO i,
      END OF struc.

"Some value assignment

struc = VALUE #( num = 1 ref = NEW #( 2 ) ).

"Internal table

DATA itab LIKE TABLE OF struc WITH EMPTY KEY.
APPEND struc TO itab.
itab[ 1 ]-ref->* = 123.
```

> **✔️ Hint**<br>
> When to actually use either a field symbol
or a data reference variable? It depends on your use case. However, data
reference variables are more powerful as far as their usage options are
concerned, and they better fit into the modern (object-oriented) ABAP
world. Recommended read: [Accessing Data Objects Dynamically (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm "Guideline").

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Dynamic ABAP Statements

As already mentioned above, there are ABAP statements that support the dynamic specification of syntax elements.
In this context, you can usually use elementary, character-like data objects specified within a pair of parentheses. 
For example, `SORT` statements:
``` abap  
"Named, character-like data object specified within parentheses
"used by an ABAP statement
DATA(field_name) = 'CARRNAME'.
SORT itab BY (field_name).      

"Unnamed, character-like data object specified within parentheses
SORT itab BY ('CURRCODE').
``` 

Note that dynamically specifying syntax elements has downsides, too. Consider some erroneous character-like content of such data objects. There is no syntax warning. At runtime, it can lead to runtime errors. 
Some of the following code snippets use artifacts from the cheat sheet repository. The code snippets demonstrate a selection.

### Dynamic ASSIGN Statements

``` abap    
"Creating and populating various types/data objects to work with
TYPES: BEGIN OF st_type,
          col1 TYPE i,
          col2 TYPE string,
          col3 TYPE string,
        END OF st_type.
DATA st TYPE st_type.
DATA it TYPE TABLE OF st_type WITH EMPTY KEY.
st = VALUE #( col1 = 1 col2 = `aaa` col3 = `Z` ).
APPEND st TO it.
DATA(dref) = NEW st_type( col1 = 2 col2 = `b` col3 = `Y` ).
DATA dobj TYPE string VALUE `hallo`.
"The following examples use a field symbol with generic type
FIELD-SYMBOLS <fs> TYPE data.

"------- Specifying the memory area dynamically ------
"I.e. the memory area is not specified directly, but as content of a
"character-like data object in parentheses.
"Note:
"- When specified as unnamed data object, the compiler treats the
"  specifications like static assignments. Do not use named data objects
"  for ASSIGN statements in ABAP for Cloud Development. It is recommended
"  that existing named data objects are put in a structure. Then, the syntax
"  for assigning components dynamically can be used so as to avoid a syntax
"  warning.
"- Most of the following examples use an unnamed data object.
"- The specification of the name is not case-sensitive.

ASSIGN ('IT') TO <fs>.
ASSIGN ('ST') TO <fs>.

"Field symbol declared inline
"Note: The typing is performed with the generic type data.
ASSIGN ('DOBJ') TO FIELD-SYMBOL(<fs_inline>).

"The statements set the sy-subrc value. 
ASSIGN ('DOES_NOT_EXIST') TO <fs>.
IF sy-subrc <> 0.
  ...
ENDIF.

"The memory area can also be a dereferenced data reference
ASSIGN dref->* TO <fs>.

"------- Assigning components dynamically ------
"You can chain the names with the component selector (-), or, in
"case of reference variables, the object component selector (->).
ASSIGN st-('COL1') TO <fs>.
ASSIGN it[ 1 ]-('COL1') TO <fs>.
ASSIGN dref->('COL1') TO <fs>.
"The following example uses the dereferencing operator explicitly
"followed by the component selector.
ASSIGN dref->*-('COL1') TO <fs>.

"Using a named data object for the component specification
DATA columnname TYPE string VALUE `COL1`.
ASSIGN st-(columnname) TO <fs>.

"Fully dynamic specification
"If the compiler can fully determine the data object in ASSIGN statements
"in ABAP for Cloud Development, a warning is not issued.
ASSIGN ('ST-COL1') TO <fs>.

"Numeric expressions are possible. Its value is interpreted
"as the position of the component in the structure.
ASSIGN st-(3) TO <fs>.

"If the value is 0, the memory area of the entire structure is
"assigned to the field symbol.
ASSIGN st-(0) TO <fs>.

"The statements above replace the following, older statements.
ASSIGN COMPONENT 'COL1' OF STRUCTURE st TO <fs>.
ASSIGN COMPONENT 3 OF STRUCTURE st TO <fs>.

"------- Assigning attributes of classes or interfaces dynamically ------
"The following syntax pattern shows the possible specifications.
"... cref->(attr_name) ...  "object reference variable
"... iref->(attr_name) ...  "interface reference variable
"... (clif_name)=>(attr_name) ...  "class/interface name
"... (clif_name)=>attr ...
"... clif=>(attr_name) ...

"Creating an instance of a class
DATA(oref) = NEW zcl_demo_abap_objects( ).

"Assigning instance attributes using an object reference variable
"All visible attributes of objects can be assigned.
oref->string = `ABAP`. "Assigning a value to the attribute for demo purposes
ASSIGN oref->('STRING') TO <fs>.

"Assigning instance attributes using an interface reference variable
DATA iref TYPE REF TO zdemo_abap_objects_interface.
iref = oref.
ASSIGN iref->('STRING') TO <fs>.
iref->in_str = `hallo`.
ASSIGN iref->('IN_STR') TO <fs>.

"Assigning static attributes
"All visible static attributes in classes and interfaces can be assigned
"In the following example, a class and an interface are specified statically,
"and the attributes are specified dynamically.
ASSIGN zcl_demo_abap_objects=>('PUBLIC_STRING') TO <fs>.
ASSIGN zdemo_abap_objects_interface=>('CONST_INTF') TO <fs>.

"Specifying a class or interface dynamically, and attributes statically
ASSIGN ('ZCL_DEMO_ABAP_OBJECTS')=>public_string TO <fs>.
ASSIGN ('ZDEMO_ABAP_OBJECTS_INTERFACE')=>const_intf TO <fs>.

"Specifying a class or interface as well as attributes dynamically
ASSIGN ('ZCL_DEMO_ABAP_OBJECTS')=>('PUBLIC_STRING') TO <fs>.
ASSIGN ('ZDEMO_ABAP_OBJECTS_INTERFACE')=>('CONST_INTF') TO <fs>.

"Further dynamic syntax options are possible, for example,
"specifying the memory area after ASSIGN with a writable expression
"because the operand position after ASSIGN is a result position.
ASSIGN NEW zcl_demo_abap_objects( )->('PUBLIC_STRING') TO <fs>.

"ELSE UNASSIGN addition
"If ELSE UNASSIGN is specified in the context of dynamic assignments/accesses,
"no memory area is assigned to the field symbol. It is unassigned after
"the ASSIGN statement.
"Note: For the static variant of the ASSIGN statement, i.e. if the memory area
"to be assigned following the ASSIGN keyword is statically specified, the addition
"ELSE UNASSIGN is implicitly set and cannot be used explicitly.
DATA(hallo) = `Hallo world`.
ASSIGN ('HALLO') TO FIELD-SYMBOL(<eu>) ELSE UNASSIGN.
ASSERT sy-subrc = 0 AND <eu> IS ASSIGNED.
ASSIGN ('DOES_NOT_EXIST') TO <eu> ELSE UNASSIGN.
ASSERT sy-subrc = 4 AND <eu> IS NOT ASSIGNED.
```

> **💡 Note**<br>
> - The following `ASSIGN` statements set the `sy-subrc` value: dynamic assignments, dynamic component assignment, dynamic invokes, assignments of table expressions. 
> - The return code is not set for a static assignment and an assignment of the constructor operator `CAST`.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamically Specifying Data Types/Creating (Data) Objects

- For dynamic syntax elements in `CREATE OBJECT` statements, find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_object_explicit.htm) (note that parameters can be specified dynamically, too).
- In addition to character-like data objects for the type name specified in the parentheses, you can also use [absolute type names](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabsolute_typename_glosry.htm) (see the information about RTTI below).

``` abap
"Anonymous data objects are created using a type determined at
"runtime. See more information below. Note that the NEW operator
"cannot be used here.
DATA(some_type) = 'STRING'.
DATA dataref TYPE REF TO data.
CREATE DATA dataref TYPE (some_type).
CREATE DATA dataref TYPE TABLE OF (some_type).
CREATE DATA dataref TYPE REF TO (some_type).
"Using an absolute type name
CREATE DATA dataref TYPE ('\TYPE=STRING').

"Assigning a data object to a field symbol casting a dynamically
"specified type
TYPES clen5 TYPE c LENGTH 5.
DATA: dobj_c10    TYPE c LENGTH 10 VALUE '1234567890',
      some_struct TYPE zdemo_abap_fli.
FIELD-SYMBOLS <casttype> TYPE data.

ASSIGN dobj_c10 TO <casttype> CASTING TYPE ('CLEN5').  "12345
ASSIGN dobj_c10 TO <casttype> CASTING LIKE some_struct-('CARRID'). "123

"Dynamically creating an object as an instance of a class and
"assigning the reference to the object to an object reference
"variable. oref can be an object or interface reference variable.
"The reference variable is created here with the generic 'object'.
DATA oref_dyn TYPE REF TO object.
CREATE OBJECT oref_dyn TYPE ('ZCL_DEMO_ABAP_OBJECTS').
"Accessing an instance attribute
oref_dyn->('ANOTHER_STRING') = `hi`.

"Note: As covered further down and in the executable example,
"CREATE DATA and ASSIGN statements have the HANDLE addition
"after which dynamically created types can be specified. A type
"description object is expected.

"Getting type description object
DATA(tdo_elem) = cl_abap_elemdescr=>get_c( 4 ).
CREATE DATA dataref TYPE HANDLE tdo_elem.
dataref->* = dobj_c10. "1234
ASSIGN dobj_c10 TO <casttype> CASTING TYPE HANDLE tdo_elem. "1234
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Accessing Structure Components Dynamically

``` abap
"Creating and populating various types/data objects to work with
TYPES: BEGIN OF st_type,
          col1 TYPE i,
          col2 TYPE string,
          col3 TYPE string,
        END OF st_type.
DATA st TYPE st_type.
DATA it TYPE TABLE OF st_type WITH EMPTY KEY.
st = VALUE #( col1 = 1 col2 = `aaa` col3 = `Z` ).
APPEND st TO it.
DATA(dref) = NEW st_type( col1 = 2 col2 = `b` col3 = `Y` ).

"You can achieve the access using ASSIGN statements as shown above, or
"by statically specifying the structure and the (object) component selector
"followed by a character-like data object in parentheses.
"Write position
st-('COL1') = 123.
it[ 1 ]-('COL1') = 456.
dref->('COL1') = 789.

"Read position
"The example shows how you can retrieve the textual content of any component
"of any structure.
DATA(content_col2) = CONV string( st-('COL1') ).
DATA(content_col3) = |{ st-('COL3') }|.
DATA content_col1 LIKE st-col1.
content_col1 = st-('COL1').

DATA dref_comp TYPE REF TO data.
CREATE DATA dref_comp LIKE st-('COL3').
dref_comp->* = st-('COL3').

"If the component is not found, a catchable exception is raised.
TRY.
    DATA(col_not_existent) = |{ st-('COL123') }|.
  CATCH cx_sy_assign_illegal_component.
    ...
ENDTRY.

"Accessing components of generic structures dynamically,
"e.g. if you have a method parameter that is typed with the generic type
"data.
"The example uses a field symbol with the generic type data which is assigned
"a structure.
FIELD-SYMBOLS <gen> TYPE data.
ASSIGN st TO <gen>.

"As in the examples above, specifying components dynamically is possible.
<gen>-('COL2') = `ABAP`.
DATA(gen_comp) = CONV string( <gen>-('COL2') ).

"Excursion
"In the following example, a structure is assigned to a field symbol that
"has a generic type. The components of the structure are accessed dynamically in 
"a DO loop. The sy-index value is interpreted as the position of the component 
"in the structure. Plus, using RTTI - as also shown further down - the component 
"names are retrieved. Component names and the values are added to a string. As a 
"prerequisite, all component values must be convertible to type string.
DATA struc2string TYPE string.
FIELD-SYMBOLS <strco> TYPE data.
ASSIGN st TO <strco>.
IF sy-subrc = 0.
  TRY.
      DATA(comps) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <strco> ) )->components.
      DO.
        TRY.
            DATA(comp_name) = comps[ sy-index ]-name.
            struc2string = struc2string &&
                           COND #( WHEN sy-index <> 1 THEN `, ` ) &&
                           comp_name && `: "` &&
                           <strco>-(sy-index) && `"`.
          CATCH cx_sy_assign_illegal_component cx_sy_itab_line_not_found.
            EXIT.
        ENDTRY.
      ENDDO.
    CATCH cx_sy_move_cast_error.
  ENDTRY.
ENDIF.
"struc2string: COL1: "123", COL2: "ABAP", COL3: "Z"
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic Specifications in Statements for Processing Internal Tables

```abap
"Creating and populating various types/data objects to work with
TYPES: BEGIN OF demo_struct,
          col1 TYPE i,
          col2 TYPE string,
          col3 TYPE string,
        END OF demo_struct.
DATA itab_ek TYPE TABLE OF demo_struct WITH EMPTY KEY.
"Standard table and specification of primary and secondary table key
DATA itab TYPE TABLE OF demo_struct
  WITH NON-UNIQUE KEY col1
  WITH UNIQUE SORTED KEY sk COMPONENTS col2.
TYPES itab_type LIKE itab.
DATA itab_ref TYPE TABLE OF REF TO demo_struct WITH EMPTY KEY.
itab_ek = VALUE #( ( col1 = 1 col2 = `aaa` col3 = `zzz` )
                   ( col1 = 2 col2 = `bbb` col3 = `yyy` )
                   ( col1 = 3 col2 = `ccc` col3 = `xxx` ) ).
itab = itab_ek.
itab_ref = VALUE #( ( NEW demo_struct( col1 = 1 col2 = `aaa` col3 = `zzz` ) ) ).

"Notes
"- In statements using key specifications, secondary table key names (or alias names)
"  are usually specified. Also the primary table key using the predefined name
"  primary_key or its alias name can be used.
"- Many of the following statements provide similar additions offering dynamic
"  specifications, such as USING KEY and dynamic component name specifications.

"------- SORT ------
"Named data object specified within parenteses
DATA(field_name) = 'COL1'.
SORT itab_ek BY (field_name) DESCENDING.
"Unnamed data object specified within parenteses
SORT itab_ek BY ('COL2') ASCENDING.

"------- READ TABLE ------
"Reading by specifying keys dynamically
"Implicitly specifying the table key values in a work area (USING KEY addition)
DATA(wa_read) = VALUE demo_struct( col2 = `aaa` ).
READ TABLE itab FROM wa_read USING KEY ('SK') REFERENCE INTO DATA(read_ref).

"Explicitly specifying the key and key values (TABLE KEY addition)
"The component names can also be specified dynamically (which is done in most of the
"following examples for demonstration purposes). Note that each component of the table
"key must be specified.
READ TABLE itab WITH TABLE KEY ('SK') COMPONENTS ('COL2') = `aaa` REFERENCE INTO read_ref.
"Specifying the predefined name primary_key explicitly and dynamically
READ TABLE itab WITH TABLE KEY ('PRIMARY_KEY') COMPONENTS ('COL1') = 1 REFERENCE INTO read_ref.
"If the addition COMPONENTS is not specified, the primary table key is implicitly used.
READ TABLE itab WITH TABLE KEY ('COL1') = 1 REFERENCE INTO read_ref.

"Reading using a free key (WITH KEY addition)
READ TABLE itab WITH KEY ('COL3') = `yyy` REFERENCE INTO read_ref.
"The addition can also be used by specifying a secondary table key name
READ TABLE itab WITH KEY ('SK') COMPONENTS ('COL2') = `ccc` REFERENCE INTO read_ref.

"Reading based on a table index (INDEX addition)
"Not using the addition USING KEY means reading from the primary table index.
READ TABLE itab INDEX 1 USING KEY ('SK') REFERENCE INTO read_ref.

"More dynamic specification options when specifying the target as work area
"(COMPARING/TRANSPORTING additions)
"TRANSPORTING: Specifying which components shall be respected
READ TABLE itab INDEX 1 INTO DATA(workarea) TRANSPORTING ('COL1') ('COL3').

"COMPARING: If the content of the compared components is identical, sy-subrc is set
"to 0, and otherwise to 2. The line found is assigned to the work area independently
"of the result of the comparison.
workarea-('COL3') = `uvw`.
READ TABLE itab INDEX 1 INTO workarea COMPARING ('COL3') TRANSPORTING ('COL1') ('COL3').
IF sy-subrc <> 0.
  ...
ENDIF.

"------- Table expressions ------
"Similar to READ TABLE statements, you can specify table lines with 3 alternatives:
"index read, read using free key, table key
"Also there, dynamic specifications are possible regarding the key specifications.

"Reading based on index with dynamic key specifications
"Specifying the secondary table index of a sorted secondary key
DATA(wa_te1) = itab[ KEY ('SK') INDEX 1 ].
"Reading using a free key, the keys are specified dynamically
DATA(wa_te2) = itab[ ('COL2') = `bbb` ('COL3') = `yyy` ].

"Reading using a table key
"Specyfing the table key explicitly
"Note: Unlike READ TABLE statements, the name of the table key must be specified. The
"addition COMPONENTS can be omitted.
"In the following example, the component names are also specified dynamically.
DATA(wa_te3) = itab[ KEY ('SK') ('COL2') = `ccc` ].
"Specifying the COMPONENTS addition explicitly
DATA(wa_te4) = itab[ KEY ('PRIMARY_KEY') COMPONENTS ('COL1') = 1 ].

"Accessing components
"As shown above, chaininings with the (object) component selector are possible.
"The examples use index access and write positions.
itab[ 1 ]-('COL2') = `jkl`.
itab_ref[ 1 ]->('COL2') = `mno`.

"------- LOOP AT ------
"USING KEY addition: Overriding the standard order determined by the table category
LOOP AT itab REFERENCE INTO DATA(ref) USING KEY ('SK').
  ...
ENDLOOP.

"When the primary table key is specified, the loop behaves as if it was not specified.
"So, the following statement corresponds to the one below.
LOOP AT itab REFERENCE INTO ref USING KEY ('PRIMARY_KEY').
  ...
ENDLOOP.

LOOP AT itab REFERENCE INTO ref.
  ...
ENDLOOP.

"Dynamic WHERE condition
"You can specify a character-like data object or a standard table with character-like
"line type.
DATA(cond_loop) = `COL1 > 1`.
LOOP AT itab REFERENCE INTO ref WHERE (cond_loop).
  ...
ENDLOOP.

"------- INSERT ------
"The USING KEY addition (which accepts a dynamic specification) affects the order in which lines are inserted.

"Result of the following example when using the ...
"- secondary table key: order of itab entries 5 ... /4 ... /...
"- primary table key: order of itab entries 4 ... /5 ... /...
INSERT LINES OF VALUE itab_type( ( col1 = 4 col2 = `eee` col3 = `www` )
                                 ( col1 = 5 col2 = `ddd` col3 = `vvv` ) )
  USING KEY ('SK')
  "USING KEY ('PRIMARY_KEY')
  INTO itab INDEX 1.

"Excursion: Using LOOP AT statements with the USING KEY addition
"and exploring the table index
"Declaring demo tables to hold the internal table entries
DATA it_seckey_idx TYPE TABLE OF demo_struct WITH EMPTY KEY.
DATA it_primekey_idx LIKE it_seckey_idx.

"Visualizing the secondary table index
LOOP AT itab INTO DATA(wa_sk) USING KEY ('SK').
  APPEND wa_sk TO it_seckey_idx.
ENDLOOP.

"Visualizing the primary table index
LOOP AT itab INTO DATA(wa_pk) USING KEY ('PRIMARY_KEY').
  APPEND wa_pk TO it_primekey_idx.
ENDLOOP.

"------- MODIFY ------
"In the following example, a line is modified based on a work area and a table key.
"The component col1 is left out from the work area intentionally.
"If the primary table key was used, the value of sy-subrc would be 4, and no modification was done.
"The optional addition transporting is specified to denote what should be modified. In this example,
"the component is also specified dynamically.
MODIFY TABLE itab FROM VALUE #( col2 = `bbb` col3 = `uuu` ) USING KEY ('SK') TRANSPORTING ('COL3').

"In the following example, a line is modified based on a work area, an index specification and a
"table key.
"INDEX can also be positioned after FROM.
MODIFY itab INDEX 2 USING KEY ('SK') FROM VALUE #( col3 = `ttt` ) TRANSPORTING ('COL3').

"Dynamic WHERE clause (only to be used with the TRANSPORTING addition)
"The USING KEY addition is also possible. Check the ABAP Keyword Documentation
"for special rules that apply.
DATA(cond_mod) = `COL1 < 3`.
MODIFY itab FROM VALUE #( col3 = `sss` ) TRANSPORTING ('COL3') WHERE (cond_mod).

"------- DELETE ------
"A single line or multipled lines can be deleted.
"Note that DELETE ADJACENT DUPLICATES statements can also be specified using
"dynamic parts.

"Deleting based on a dynamically specified table key
"The values can be declared either implicitly in a work area after FROM or explicitly
"by listing the components of the table key after TABLE KEY.
"If the USING KEY addition is not specified, the primary table key is used by default.
DELETE TABLE itab FROM VALUE #( col2 = `eee` col3 = `www` ) USING KEY ('SK').

"Each component of the table key must be listed.
DELETE TABLE itab WITH TABLE KEY ('SK') COMPONENTS ('COL2') = `ddd`.

"Deleting based on the table index
DELETE itab INDEX 1 USING KEY ('SK').

"Deleting multiple lines and specifying the WHERE conditions dynamically
"The USING KEY addition is also possible.
DATA(condition_tab) = VALUE string_table( ( `COL1 < 3` )
                                          ( `OR` )
                                          ( `COL3 = ``www``` ) ).
DELETE itab WHERE (condition_tab).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic ABAP SQL Statements

```abap
"Dynamic SELECT list
DATA(select_list) = `CARRID, CONNID, FLDATE`.
DATA fli_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

SELECT (select_list)
  FROM zdemo_abap_fli
  INTO CORRESPONDING FIELDS OF TABLE @fli_tab.

"Dynamic FROM clause
DATA(table) = 'ZDEMO_ABAP_FLI'.
SELECT *
  FROM (table)
  INTO TABLE @fli_tab.

"Excursion: Compatible target data objects
"In the examples above, the data object/type is created statically.

"Creating an anonymous data object with a CREATE DATA statement
"and specifiying the type dynamically.
"You can use the dereferenced object reference variable as target.
DATA itab_dyn TYPE REF TO data.
CREATE DATA itab_dyn TYPE TABLE OF (table).

SELECT *
  FROM (table)
  INTO TABLE @itab_dyn->*.

"In older ABAP code, you may find assignments to a field symbol
"due to the reasons mentioned above.
FIELD-SYMBOLS <tab> TYPE ANY TABLE.
ASSIGN itab_dyn->* TO <tab>.

SELECT *
  FROM (table)
  INTO TABLE @<tab>.

"Similar to the NEW operator, you can use the addition NEW
"to create an anonymous data object in place. The advantage is
"that the data type is constructed in a suitable way.
SELECT *
  FROM (table)
  INTO TABLE NEW @DATA(dref_tab).

"Dynamic WHERE clause
"The example includes a WHERE clause that is created as an internal
"table with a character-like row type.
DATA(where_clause) = VALUE string_table( ( `CARRID = 'LH'` )
                                         ( `OR` )
                                         ( `CARRID = 'AA'` ) ).

SELECT *
  FROM zdemo_abap_fli
  WHERE (where_clause)
  INTO TABLE NEW @DATA(tab_dyn_where).

"Dynamic ORDER BY clause
SELECT *
  FROM zdemo_abap_fli
  ORDER BY (`FLDATE`)
  INTO TABLE NEW @DATA(tab_dyn_order).

"SELECT statement with miscellaneous dynamic specifications
SELECT (`CARRID, CONNID, FLDATE`)
  FROM (`ZDEMO_ABAP_FLI`)
  WHERE (`CARRID <> ``AA```)
  ORDER BY (`FLDATE`)
  INTO TABLE NEW @DATA(tab_dyn_misc).

"Further dynamic specifications in other ABAP SQL statements
"Creating a structure to be inserted into the database table
SELECT SINGLE *
      FROM (table)
      INTO NEW @DATA(dref_struc).
dref_struc->('CARRID') = 'YZ'.

INSERT (table) FROM @dref_struc->*.

dref_struc->('CURRENCY') = 'EUR'.
UPDATE (table) FROM @dref_struc->*.

dref_struc->('SEATSOCC') = 10.
MODIFY (table) FROM @dref_struc->*.

DELETE FROM (table) WHERE (`CARRID = 'YZ'`).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

**Excursion**: To take up the use case mentioned in the introduction about retrieving the content of a database table, storing it in an internal table, and
displaying it when the database table name is specified dynamically at
runtime, see the following code snippet. Note the comments.


```abap
CLASS zcl_example_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_example_class IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    "The example retrieves the content of a database table, storing it in an 
    "internal table, and displaying it when the database table name is 
    "specified dynamically at runtime.
    "Certainly, there are quite some ways to achieve it, and that work out
    "of the box. For example, in ABAP for Cloud Development, you can implement
    "the classrun if_oo_adt_classrun and output content using the out->write(...)
    "method. You can also inherit from cl_demo_classrun in your class. In
    "classic ABAP, you can, for example and additionally, use cl_demo_output or
    "ALV.
    "Notes:
    "- The following example is just ABAP code exploring dynamic programming
    "  aspects. Note the disclaimer in the README of the cheat sheet repository.
    "  It is an example that sets its focus on a dynamic SELECT statement and
    "  processing internal table content by dynamically accessing structure
    "  components.
    "- The ways mentioned above are way more powerful (e.g. in most cases also
    "  nested and deep data objects can be displayed for demo purposes).
    "- For simplicity, column contents are converted to string here if necessary,
    "  i.e. all column contents must be convertible to string.
    "- For display purposes, the snippet uses the classrun methods to display
    "  results sequentially - instead of displaying the internal table
    "  content retrieved by the SELECT statement directly.
    "- The example uses database tables from the cheat sheet repository. To fill
    "  them, you can use the method call zcl_demo_abap_aux=>fill_dbtabs( )..
    zcl_demo_abap_aux=>fill_dbtabs( ).

    "Data objects and types relevant for the example (length and offset for
    "content display)
    DATA str TYPE string.
    TYPES: BEGIN OF comp_struc,
             name TYPE string,
             len  TYPE i,
             off  TYPE i,
           END OF comp_struc.
    DATA it_comps TYPE TABLE OF comp_struc WITH EMPTY KEY.

    "Database table of type string containing names of database tables;
    "table is looped over to output content of all database tables
    DATA(dbtabs) = VALUE string_table( ( `ZDEMO_ABAP_CARR` )
                                       ( `ZDEMO_ABAP_FLI` )
                                       ( `ZDEMO_ABAP_FLSCH` ) ).

    LOOP AT dbtabs INTO DATA(dbtab).
      "Retrieving database content of a dynamically specified database table
      TRY.
          SELECT *
            FROM (dbtab)
            INTO TABLE NEW @DATA(itab)
            UP TO 5 ROWS.
        CATCH cx_sy_dynamic_osql_semantics INTO DATA(sql_error).
          CLEAR itab->*.
          out->write( |Table { dbtab } does not exist.| ).
      ENDTRY.

      IF sql_error IS INITIAL.
        "Getting table component names using RTTI methods
        TRY.
            DATA(type_descr_obj_tab) = CAST cl_abap_tabledescr(
              cl_abap_typedescr=>describe_by_data( itab->* ) ).
            DATA(tab_comps) = CAST cl_abap_structdescr(
              type_descr_obj_tab->get_table_line_type( ) )->get_components( ).
            LOOP AT tab_comps ASSIGNING FIELD-SYMBOL(<comp>).
              APPEND VALUE #( name = <comp>-name len = strlen( <comp>-name ) ) TO it_comps.
            ENDLOOP.
          CATCH cx_sy_move_cast_error INTO DATA(error).
            out->write( |{ error->get_text( ) }| ).
        ENDTRY.

        IF error IS INITIAL.
          out->write( |\n| ).
          out->write( |Retrieved content of database table { dbtab }:| ).
          "Implementation for properly aligning the content
          "The example is implemented to check the length of the column names as well as the
          "length of the values in the columns. It determines the length of the longest string
          "in each column. Depending on the length values, either the length of the column name
          "or the length of the longest string in a column is stored in an internal table that
          "contains information for calculating the offset.
          LOOP AT tab_comps ASSIGNING FIELD-SYMBOL(<len>).
            ASSIGN it_comps[ name = <len>-name ] TO FIELD-SYMBOL(<co>).
            DATA(max_content) = REDUCE i( INIT len = <co>-len
                                          FOR <line> IN itab->*
                                          NEXT len = COND #( WHEN strlen( CONV string( <line>-(<co>-name) ) ) > len
                                                             THEN strlen( CONV string( <line>-(<co>-name) ) )
                                                             ELSE len ) ).
            "Extend the length value to leave some more space
            IF max_content > <co>-len.
              <co>-len = max_content + 3.
            ELSE.
              <co>-len += 3.
            ENDIF.
          ENDLOOP.
          "Calculating offset values
          DATA max_str_len TYPE i.
          LOOP AT it_comps ASSIGNING FIELD-SYMBOL(<off>).
            DATA(tabix) = sy-tabix.
            READ TABLE it_comps INDEX tabix - 1 ASSIGNING FIELD-SYMBOL(<prev>).
            <off>-off = COND #( WHEN tabix = 1 THEN 0 ELSE <prev>-len + <prev>-off ).
            max_str_len += <off>-len.
          ENDLOOP.
          "Providing enough space so that table row content can be inserted based on
          "the offset specification
          SHIFT str BY max_str_len PLACES RIGHT.
          "Adding the column names first
          LOOP AT it_comps ASSIGNING FIELD-SYMBOL(<header>).
            str = insert( val = str sub = <header>-name off = <header>-off ).
          ENDLOOP.
          out->write( str ).
          "Processing all lines in the internal table containing the retrieved table rows
          LOOP AT itab->* ASSIGNING FIELD-SYMBOL(<wa>).
            CLEAR str.
            SHIFT str BY max_str_len PLACES RIGHT.
            DO.
              TRY.
                  str = insert( val = str sub = <wa>-(sy-index) off = it_comps[ sy-index ]-off ).
                CATCH cx_sy_assign_illegal_component cx_sy_range_out_of_bounds cx_sy_itab_line_not_found.
                  EXIT.
              ENDTRY.
            ENDDO.
            out->write( str ).
          ENDLOOP.
        ENDIF.
        out->write( |\n| ).
        CLEAR: str, it_comps.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic Invoke
The following code snippet shows dynamically specifying [procedure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm "Glossary Entry") calls.

``` abap
"Note: Dynamic method calls require a CALL METHOD statement.
"The following examples assume that there are no mandatory
"parameters defined for the method.
"Possible for methods of the same class, works like me->(meth)
CALL METHOD (meth).
"Class specified statically
CALL METHOD class=>(meth).
"Object reference variable specified statically;
"also possible for interface reference variables
CALL METHOD oref->(meth).

"The following statements are possible for all visible static methods
"Class dynamically specified
CALL METHOD (class)=>meth.
"Class and method dynamically specified
CALL METHOD (class)=>(meth).

"The following examples assume that there are parameters defined 
"for the method.
"Assigning actual parameters to the formal parameters statically
CALL METHOD class=>(meth) EXPORTING  p1 = a1 p2 = a2 ...
                          IMPORTING  p1 = a1 p2 = a2 ...

"Assigning actual parameters to the formal parameters dynamically
DATA ptab TYPE abap_parmbind_tab.
ptab = ...

CALL METHOD class=>(meth) PARAMETER-TABLE ptab.

"Notes on PARAMETER-TABLE ptab
"- The table (of type abap_parmbind_tab; line type is abap_parmbind) 
"  must be filled and have a line for all non-optional parameters.
"- Components: name -> formal parameter name
"              kind -> kind of parameter, e. g. importing
"              value -> pointer to appropriate actual parameter,
"                       is of type REF TO data
"The addition EXCEPTION-TABLE for exceptions is not dealt with here.

"Example that uses the PARAMETER-TABLE addition
"Creating an instance by specifying the type statically
"An example class of the cheat sheet repository is used.
DATA(oref1) = NEW zcl_demo_abap_objects( ).
"Calling an instance method
"The method multiplies an integer by 3.
"The calculation result is returned.
DATA(result) = oref1->triple( i_op = 2 ). "6

"Dynamic equivalent
"Creating an instance of a class by specifying the type
"dynamically
DATA oref2 TYPE REF TO object.
CREATE OBJECT oref2 TYPE ('ZCL_DEMO_ABAP_OBJECTS').

"Creating parameter table
DATA(ptab) = VALUE abap_parmbind_tab( ( name  = 'I_OP'
                                        kind  = cl_abap_objectdescr=>exporting
                                        value = NEW i( 3 ) )
                                      ( name  = 'R_TRIPLE'
                                        kind  = cl_abap_objectdescr=>returning
                                        value = NEW i( ) ) ).

"Dynamic method call and specifying a parameter table
CALL METHOD oref2->('TRIPLE') PARAMETER-TABLE ptab.
result = ptab[ name = 'R_TRIPLE' ]-('VALUE')->*. "9
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Validating Input for Dynamic Specifications (CL_ABAP_DYN_PRG)

You can use the `CL_ABAP_DYN_PRG` class to validate input for dynamic specifications.
There are several methods for different use cases. See the class documentation (click F2 on the class name in ADT) for more information.
The following examples show some of those methods. If the validation is successful, the methods in the examples return the input value.
Otherwise, an exception is raised.

```abap
"The following method checks database table names. The name is provided 
"with the val parameter. The packages formal parameter expects a table 
"containing the names of packages in which the specified table should be 
"included. Assuming you provide incorrect input for the table name, or 
"the table is not contained in the specified packages, you can expect an 
"exception to be raied.

TRY.
    DATA(dbtab) = cl_abap_dyn_prg=>check_table_name_tab( 
      val      = `ZDEMO_ABAP_FLI`
      packages = VALUE #( ( `TEST_ABAP_CHEAT_SHEETS` ) 
                          ( `TEST_SOME_PACK` ) ) ).

    SELECT SINGLE * FROM (dbtab) INTO NEW @DATA(ref_wa).
  CATCH cx_abap_not_a_table cx_abap_not_in_package.
    ...
ENDTRY.

"In the following examples, a method is used to check whether
"the input is allowed or not. For this, you specify an allowlist.
"Here, the relvant parameter expects a comma-separated list of
"allowed values.
TRY.
    DATA(value1) = cl_abap_dyn_prg=>check_allowlist( 
        val           = `A`
        allowlist_str = `A,B,C,D` ).
    
    ... "Here might go an ABAP SQL statement with a dynamic specification.
  CATCH cx_abap_not_in_allowlist.
    ...
ENDTRY.

"Another parameter of the method expects an internal table that
"contains the allowed values.
TRY.
    DATA(value2) = cl_abap_dyn_prg=>check_allowlist( 
        val            = `XYZ`
        allowlist_htab = VALUE #( ( `A` )
                                  ( `B` )
                                  ( `C` )
                                  ( `D` ) ) ).
    
    ... "Here might go an ABAP SQL statement with a dynamic specification.
  CATCH cx_abap_not_in_allowlist.
    ...
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Runtime Type Services (RTTS)

[RTTS](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_services_glosry.htm "Glossary Entry")
represent a hierarchy of [type description classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_class_glosry.htm "Glossary Entry")
containing methods for 
-   getting type information on data objects, data types or
    [instances](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_glosry.htm "Glossary Entry")
    at runtime ([Runtime Type Identification (RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm "Glossary Entry")).
-   defining and creating new data types as [type description objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_object_glosry.htm) at runtime ([Runtime Type Creation (RTTC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_creation_glosry.htm "Glossary Entry")).

The hierarchy of type description classes is as follows.

<pre>
CL_ABAP_TYPEDESCR
  |
  |--CL_ABAP_DATADESCR
  |   |
  |   |--CL_ABAP_ELEMDESCR
  |   |   |
  |   |   |--CL_ABAP_ENUMDESCR
  |   |
  |   |--CL_ABAP_REFDESCR
  |   |--CL_ABAP_COMPLEXDESCR
  |       |
  |       |--CL_ABAP_STRUCTDESCR
  |       |--CL_ABAP_TABLEDESCR
  |
  |--CL_ABAP_OBJECTDESCR
     |
     |--CL_ABAP_CLASSDESCR
     |--CL_ABAP_INTFDESCR
</pre>
So, the
[superclass](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensuperclass_glosry.htm "Glossary Entry")
`CL_ABAP_TYPEDESCR` has multiple
[subclasses](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubclass_glosry.htm "Glossary Entry"),
for example, to deal with each kind of type.
Working with this inheritance tree means making use of
[casts](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencast_glosry.htm "Glossary Entry"),
especially
[downcasts](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendown_cast_glosry.htm "Glossary Entry") when retrieving information at runtime.
Detailing out all the possibilities for the information retrieval and
type creation is beyond scope. Check the information, options and
various methods that can be used in the class documentation, e. g. using
F2 help information in
[ADT](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenadt_glosry.htm "Glossary Entry"),
for more details.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Getting Type Information at Runtime

With RTTI, you can determine data types at runtime using description methods in type description classes.
To get the type information, you can get a reference to a type description object of a type, that is, an instance of a type description class.
The type properties are represented by attributes that are accessible through the type description object.


> **💡 Note**<br>
> - For each type, there is exactly one type description object. 
> - For each type category (elementary type, table, and so on), there is a type description class (e.g. `CL_ABAP_STRUCTDESCR` for structures, as shown in the hierarchy tree above) that has special attributes (i.e. the properties of the respective types). 
> - References to type description objects can be used, for example, after the `TYPE HANDLE` addition of the `CREATE DATA` and `ASSIGN` statements.

The following examples show the retrieval of type information. Instead of the extra declaration of data reference variables, you can use
[inline declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry").
[Method chaining](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmethod_chaining_glosry.htm "Glossary Entry")
comes in handy, too.

```abap
"------------------------------------------------------------------------
"---------------- Getting general type information ----------------------
"------------------------------------------------------------------------

"Getting references to type description objects of a type ...

"... using the cl_abap_typedescr=>describe_by_name method.
"In this case, the name of the types are used.
"The following examples cover elementary, structured and
"internal table types.
TYPES elemtype TYPE n LENGTH 3.
TYPES structype TYPE zdemo_abap_carr.
TYPES strtabtype TYPE string_table.

"The reference in the type description object references an
"object from one of the classes CL_ABAP_ELEMDESCR, CL_ABAP_ENUMDESCR,
"CL_ABAP_REFDESCR, CL_ABAP_STRUCTDESCR, CL_ABAP_TABLEDESCR,
"CL_ABAP_CLASSDESCR, or CL_ABAP_INTFDESCR. You may want to check the
"content of the type description objects in the debugger.
DATA(tdo_by_name_elem) = cl_abap_typedescr=>describe_by_name( 'ELEMTYPE' ).
DATA(tdo_by_name_struc) = cl_abap_typedescr=>describe_by_name( 'STRUCTYPE' ).
DATA(tdo_by_name_itab) = cl_abap_typedescr=>describe_by_name( 'STRTABTYPE' ).

"Inline declarations are handy to avoid helper variables such as
"in the following example.
DATA tdo_by_name_elem_helper TYPE REF TO cl_abap_typedescr.
tdo_by_name_elem_helper = cl_abap_typedescr=>describe_by_name( 'ELEMTYPE' ).

"... using the cl_abap_typedescr=>describe_by_data method.
"In this case, the data objects are provided.
"The examples cover an elementary data object, a structure and and
"internal table.
DATA elemdobj TYPE elemtype.
DATA strucdobj TYPE structype.
DATA tabledobj TYPE strtabtype.

DATA(tdo_by_data_elem) = cl_abap_typedescr=>describe_by_data( 'ELEMTYPE' ).
DATA(tdo_by_data_struc) = cl_abap_typedescr=>describe_by_data( 'STRUCTYPE' ).
DATA(tdo_by_data_itab) = cl_abap_typedescr=>describe_by_data( 'STRTABTYPE' ).

"... using the cl_abap_typedescr=>describe_by_data_ref method
"In this case, a data reference variable is used.
"Note: As a result, the method returns a reference that points to
"an object in one of the classes CL_ABAP_ELEMDESCR, CL_ABAP_ENUMDESCR,
"CL_ABAP_REFDESCR, CL_ABAP_STRUCTDESCR, or CL_ABAP_TABLEDSECR.

DATA dref1 TYPE REF TO i.
dref1 = NEW #( ).
"Data reference pointing to CL_ABAP_ELEMDESCR
DATA(tdo_by_dref1) = cl_abap_typedescr=>describe_by_data_ref( dref1 ).
"Data reference pointing to CL_ABAP_TABLEDSECR
DATA(dref2) = NEW string_table( ).
DATA(tdo_by_dref2) = cl_abap_typedescr=>describe_by_data_ref( dref2 ).

"... using the cl_abap_typedescr=>describe_by_data_ref method
"In this case, an object reference variable is used.
DATA oref TYPE REF TO zcl_demo_abap_objects.
oref = NEW #( ).
DATA(tdo_by_oref) = cl_abap_typedescr=>describe_by_object_ref( oref ).

"------------------------------------------------------------------------
"--- Getting more detailed information by programmatically accessing ----
"--- the attributes -----------------------------------------------------
"------------------------------------------------------------------------

"------------------------------------------------------------------------
"--- Examples using a type description object for an elementary type ----
"------------------------------------------------------------------------
DATA(elem_kind) = tdo_by_name_elem->kind.  "E (elementary)
DATA(elem_absolute_name) = tdo_by_name_elem->absolute_name.  "Check in the debugger for such a local type
DATA(elem_relative_name) = tdo_by_name_elem->get_relative_name( ).  "ELEMTYPE
DATA(elem_is_ddic_type) = tdo_by_name_elem->is_ddic_type( ). "has the value abap_false

"Getting more information using type-specific type description classes (e.g. cl_abap_elemdescr for elementary
"types, as shown in the hierarchy tree above)
"Using casts, you can access special attributes (i.e. the properties of the respective types).

"Creating a data reference variable to hold the reference to
"the type description object
DATA ref_elemdescr TYPE REF TO cl_abap_elemdescr.
ref_elemdescr = CAST #( tdo_by_name_elem ).
"Using the older cast operator ?= (which is not used in the examples)
ref_elemdescr ?= tdo_by_name_elem.

"Alternatives using inline declaration
DATA(ref_elemdescr2) = CAST cl_abap_elemdescr( tdo_by_name_elem ).
"Using method chaining, you can omit extra declarations
DATA(ref_elemdescr3) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'ELEMTYPE' ) ).
DATA(elem_abs_name) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'ELEMTYPE' ) )->absolute_name.
"Implementing a check before the cast to the type-specific type description class
IF tdo_by_name_elem IS INSTANCE OF cl_abap_elemdescr.
  DATA(ref_elemdescr4) = CAST cl_abap_elemdescr( tdo_by_name_elem ).
ENDIF.

"Some examples for more detailed information that is accessible after
"the cast. Check the options after the object component selector ->.
DATA(elem_output_length) = ref_elemdescr->output_length. "3
"The following method checks compatibility of a specified data object
DATA(elem_applies1) = ref_elemdescr->applies_to_data( elemdobj ). "has the value abap_true
DATA test_dobj TYPE c LENGTH 3.
DATA(elem_applies2) = ref_elemdescr->applies_to_data( test_dobj ). "has the value abap_false

"-----------------------------------------------------------------
"--- Examples using a type description object for a structure ----
"-----------------------------------------------------------------
DATA ref_structdescr1 TYPE REF TO cl_abap_structdescr.
ref_structdescr1 = CAST #( tdo_by_name_struc ).

"Check examples before making the cast
"The examples also use inline declaration.
CASE TYPE OF tdo_by_name_struc.
  WHEN TYPE cl_abap_structdescr.
    DATA(ref_structdescr2) = CAST cl_abap_structdescr( tdo_by_name_struc ).
  WHEN OTHERS.
    ...
ENDCASE.

DATA(ref_structdescr3) = COND #( WHEN tdo_by_name_struc IS INSTANCE OF cl_abap_structdescr
                                  THEN CAST cl_abap_structdescr( tdo_by_name_struc ) ).

"More details accessible after the cast
DATA(struc_kind) = ref_structdescr1->struct_kind. "F (flat)
"The following attribute returns a table with component information, such as
"the component names and type kinds.
DATA(struc_components) = ref_structdescr1->components.
"The following method also returns a table with component information. In this case,
"more information is returned such as type description objects of each component and more.
DATA(struc_components_tab) = ref_structdescr1->get_components( ).

"-----------------------------------------------------------------------
"--- Examples using a type description object for an internal table ----
"-----------------------------------------------------------------------
DATA ref_tabledescr1 TYPE REF TO cl_abap_tabledescr.
ref_tabledescr1 = CAST #( tdo_by_name_itab ).
"Cast with inline declaration
DATA(ref_tabledescr2) = CAST cl_abap_tabledescr( tdo_by_name_itab ).

"Another internal table as an example
DATA itab TYPE SORTED TABLE OF zdemo_abap_carr WITH UNIQUE KEY carrid.
DATA(ref_tabledescr3) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab ) ).

DATA(itab_table_kind) = ref_tabledescr3->table_kind. "O (sorted table)
DATA(itab_has_unique_key) = ref_tabledescr3->has_unique_key. "has the value abap_true
"Returns a table with the names of internal table keys
DATA(itab_table_key) = ref_tabledescr3->key. "carrid
"Returns a table with a description of all table keys, e.g.
"all components of a key, key kind (U, unique, in the example case),
"information whether the key is the primary key etc.
DATA(itab_keys) = ref_tabledescr3->get_keys( ).
"If you want to get information about the line type, e.g. finding out about
"the component names, another cast is required.
"First, getting a reference to the type description object for the structured type.
DATA(itab_line_type) = ref_tabledescr3->get_table_line_type( ).
"Then, performing a cast to access the component information as shown above.
DATA(itab_line_info) = CAST cl_abap_structdescr( itab_line_type ).
DATA(itab_comps1) = itab_line_info->components.
DATA(itab_comps2) = itab_line_info->get_components( ).

"----------------------------------------------------------------------
"--- Examples using a type description object for a data reference ----
"----------------------------------------------------------------------
DATA ref_refdescr1 TYPE REF TO cl_abap_refdescr.
TYPES type_ref_i TYPE REF TO i.
DATA(tdo_ref) = cl_abap_typedescr=>describe_by_name( 'TYPE_REF_I' ).
ref_refdescr1 = CAST #( tdo_ref ).
DATA(ref_refdescr2) = CAST cl_abap_refdescr( tdo_ref ).

"Getting a reference to the type description object of a type used to
"type the reference
DATA(ref_type) = ref_refdescr1->get_referenced_type( ).
DATA(ref_abs_name) = ref_refdescr1->get_referenced_type( )->absolute_name. "\TYPE=I

"-------------------------------------------------------------------------
"--- Examples using a type description object for an object reference ----
"-------------------------------------------------------------------------
DATA ref_oref1 TYPE REF TO cl_abap_objectdescr.
ref_oref1 = CAST #( tdo_by_oref ).
"See the hierarchy of type description classes. You may also use
"cl_abap_classdescr.
DATA(ref_oref2) = CAST cl_abap_classdescr( tdo_by_oref ).

"Returns a table with information about the class attributes
DATA(class_attributes) = ref_oref1->attributes.

"Returns a table with information about the methods such as
"parameters, visibility and more
DATA(class_methods) = ref_oref1->methods.

"Returns a table with information about the interfaces implemented
DATA(class_intf) = ref_oref1->interfaces.

"Can class be instantiated
DATA(class_is_inst) = ref_oref1->is_instantiatable( ). "has the value abap_true

"Using an interface
DATA(tdo_intf) = cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_OBJECTS_INTERFACE' ).
DATA ref_iref TYPE REF TO cl_abap_intfdescr.
ref_iref = CAST #( tdo_intf ). "cl_abap_objectdescr is also possible

DATA(ref_intf_attr) = ref_iref->attributes.
DATA(ref_intf_meth) = ref_iref->methods.
DATA(ref_intf_attr_objdescr) = CAST cl_abap_objectdescr( tdo_intf )->attributes.
```

Excursions: 

```abap
"Casting and method chaining as above in contrast to the following 
"extra declarations. If the variables were not declared inline as 
"in the example, there would be even more lines of code.
DATA(a) = cl_abap_typedescr=>describe_by_data( some_struc ).
DATA(b) = CAST cl_abap_structdescr( a ).
DATA(c) = b->components.  

"get_included_view method: Getting type information for included 
"components, e.g. in case of deep structures
TYPES: BEGIN OF st,
         a TYPE i, "elementary type
         b TYPE zdemo_abap_carr, "structure
         c TYPE string_table, "internal table
       END OF st.

DATA(type_descr) = CAST cl_abap_structdescr(
  cl_abap_typedescr=>describe_by_name( 'ST' ) )->get_included_view( ).

"As mentioned earlier about type name specifications for statements 
"such as CREATE DATA, in addition to character-like data objects for 
"the type name specified in the parentheses, you can also use absolute 
"type names.

"Type to refer to 
TYPES type4abs TYPE p LENGTH 4 DECIMALS 3.

"Data and object reference variables with generic types
DATA dref4abs TYPE REF TO data.
DATA oref4abs TYPE REF TO object.

"Getting absolute names
DATA(abs_name_type) = cl_abap_typedescr=>describe_by_name( 
   'TYPE4ABS' )->absolute_name.
DATA(abs_name_cl) = cl_abap_typedescr=>describe_by_name(
   'ZCL_DEMO_ABAP_DYNAMIC_PROG' )->absolute_name.

"Data references
"Named data object holding the absolute name
CREATE DATA dref4abs TYPE (abs_name_type).

"Unnamed data object
CREATE DATA dref4abs TYPE ('\TYPE=STRING').

"Object references
"Named data object
CREATE OBJECT oref4abs TYPE (abs_name_cl).

"Unnamed data object
CREATE OBJECT oref4abs TYPE ('\CLASS=ZCL_DEMO_ABAP_DYNAMIC_PROG').
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamically Creating Data Types at Runtime
You can create data types at program runtime using methods of the type description classes of RTTS.
These types are only valid locally in the program. They are also anonymous, i.e. they are only accessible through type description objects.
As shown above, you can get a reference to a type description object of a type using the static methods of the class `CL_ABAP_TYPEDESCR`.
```abap
"For example, a structured type
DATA(type_descr_obj) = CAST cl_abap_structdescr(
  cl_abap_typedescr=>describe_by_name( 'SOME_STRUC_TYPE' ) ).
```

The focus here is on using RTTC methods such as `get...`. It is recommended that you use the `get` method instead of the `create` method. 

```abap
"----------------------------------------------------------------------
"--- Creating type description objects using elementary data types ----
"----------------------------------------------------------------------
"Conceptually, all elementary, built-in ABAP types already
"exist and can be accessed by the corresponding get_* methods.
"In ADT, click CTRL + space after cl_abap_elemdescr=>...
"to check out the options. The following examples show a
"selection.

DATA(tdo_elem_i) = cl_abap_elemdescr=>get_i( ).
DATA(tdo_elem_string) = cl_abap_elemdescr=>get_string( ).

"For the length specification of type c and others, there is
"an importing parameter available.
DATA(tdo_elem_c_l20) = cl_abap_elemdescr=>get_c( 20 ).

"Type p with two parameters to be specified.
DATA(tdo_elem_p) = cl_abap_elemdescr=>get_p( p_length   = 3
                                             p_decimals = 2 ).

"Note: Instead of calling get_i() and others having no importing
"parameters, you could also call the describe_by_name( ) method
"and pass the type names (I‚ STRING etc.) as arguments.
"DATA(tdo_elem_i_2) = CAST cl_abap_elemdescr(
"  cl_abap_typedescr=>describe_by_name( 'I' ) ).
"DATA(tdo_elem_string_2) = CAST cl_abap_elemdescr(
"  cl_abap_typedescr=>describe_by_name( 'STRING' ) ).

"----------------------------------------------------------------------
"--- Creating type description objects using structured data types ----
"----------------------------------------------------------------------
"They are created based on a component description table.

"A structured type such as the following shall be created dynamically
"using a type description object.
TYPES: BEGIN OF struc_type,
        a TYPE string,
        b TYPE i,
        c TYPE c LENGTH 5,
        d TYPE p LENGTH 4 DECIMALS 3,
      END OF struc_type.

"Creating a type description object using RTTC method
"Using the get method, you can create the type description object
"dynamically based on a component table. The component table is
"of type abap_component_tab. In this example, the component table
"is created inline.
DATA(tdo_struc) = cl_abap_structdescr=>get(
    VALUE #(
      ( name = 'A' type = cl_abap_elemdescr=>get_string( ) )
      ( name = 'B' type = cl_abap_elemdescr=>get_i( ) )
      ( name = 'C' type = cl_abap_elemdescr=>get_c( 5 ) )
      ( name = 'D' type = cl_abap_elemdescr=>get_p( p_length   = 4
                                                    p_decimals = 3 ) ) ) ).

"---------------------------------------------------------------------
"--- Creating type description objects using internal table types ----
"---------------------------------------------------------------------
"Note: Specifying the line type is mandatory, the rest is optional.

"An internal table type such as the following shall be created dynamically
"using a type description object.
TYPES std_tab_type_std_key TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

"Creating a type description object using RTTC method
"Not specifying the other optional parameters means that the
"default values are used, for example, standard table is the
"default value for p_table_kind.
DATA(tdo_tab_1) = cl_abap_tabledescr=>get(
        p_line_type  = cl_abap_elemdescr=>get_string( ) ).

"Another internal table type for which more parameter specifications
"are needed. The following internal table type shall be created using
"a type description object.
TYPES so_table_type TYPE SORTED TABLE OF zdemo_abap_flsch WITH UNIQUE KEY carrid connid.

"Creating a type description object using RTTC method
"The following example also demonstrates how comfortably constructor
"operators can be used at these positions.
DATA(tdo_tab_2) = cl_abap_tabledescr=>get(
        p_line_type  = CAST cl_abap_structdescr(
                         cl_abap_tabledescr=>describe_by_name( 'ZDEMO_ABAP_FLSCH' ) )
        p_table_kind = cl_abap_tabledescr=>tablekind_sorted
        p_key        = VALUE #( ( name = 'CARRID' ) ( name = 'CONNID' ) )
        p_unique     = cl_abap_typedescr=>true ).

" ... reference types
"Reference types such as the following shall be created using a
"type description object.
TYPES some_ref_type2t TYPE REF TO t.
TYPES some_ref_type2cl TYPE REF TO zcl_demo_abap_dynamic_prog.

"Using RTTC methods
"You can create a reference type from a base type. This base type
"may be a class, interface or data type.
DATA(tdo_ref_1) = cl_abap_refdescr=>get( cl_abap_elemdescr=>get_t( ) ).
DATA(tdo_ref_2) = cl_abap_refdescr=>get(
                    cl_abap_typedescr=>describe_by_name( 'ZCL_DEMO_ABAP_DYNAMIC_PROG' ) ).
"Alternative: get_by_name method
DATA(tdo_ref_3) = cl_abap_refdescr=>get_by_name( 'T' ).
DATA(tdo_ref_4) = cl_abap_refdescr=>get_by_name( 'ZCL_DEMO_ABAP_DYNAMIC_PROG' ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamically Creating Data Objects at Runtime

As shown above, anonymous data objects can be dynamically created using `CREATE DATA` statements in many ways by specifying the type ...
- statically: `CREATE DATA dref TYPE string.`
- dynamically: `CREATE DATA dref TYPE (some_type).`

Another way to dynamically create data objects with dynamic type specification is to use types created at runtime with RTTC methods.
The `CREATE DATA` statement provides the [`TYPE HANDLE`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcreate_data_handle.htm) addition after which you can specify type description objects. A reference variable of the static type of class `CL_ABAP_DATADESCR` or its subclasses that points to a type description object can be specified after `TYPE HANDLE`.

``` abap
DATA dref_cr TYPE REF TO data.

"Elementary data object
"Type description object for an elementary type
DATA(tdo_elem_c_l20) = cl_abap_elemdescr=>get_c( 20 ).
"Creating an elementary data object based on a type description object
CREATE DATA dref_cr TYPE HANDLE tdo_elem_c_l20.

"Structure
DATA(tdo_struc) = cl_abap_structdescr=>get(
        VALUE #(
          ( name = 'COMP1' type = cl_abap_elemdescr=>get_string( ) )
          ( name = 'COMP2' type = cl_abap_elemdescr=>get_i( ) )
          ( name = 'COMP3' type = cl_abap_elemdescr=>get_c( 3 ) ) ) ).

"Creating a structure based on a type description object
CREATE DATA dref_cr TYPE HANDLE tdo_struc.

"Internal table
"In the case below, it is a standard table with standard key by 
"default because the other parameters are not specified.
DATA(tdo_tab) = cl_abap_tabledescr=>get(
  p_line_type  = CAST cl_abap_structdescr( 
                       cl_abap_tabledescr=>describe_by_name( 'ZDEMO_ABAP_CARR' ) ) ).

"Creating an internal table based on a type description object
CREATE DATA dref_cr TYPE HANDLE tdo_tab.

"Data reference
DATA(tdo_ref) = cl_abap_refdescr=>get( cl_abap_elemdescr=>get_t( ) ). 
CREATE DATA dref_cr TYPE HANDLE tdo_ref.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information
- It is recommended that you also consult section [Dynamic Programming Techniques (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynamic_prog_technique_gdl.htm) in the ABAP Keyword Documentation since it provides important aspects that should be considered when dealing with dynamic programming in general (e. g. security aspects or runtime error prevention).
- There are even further dynamic programming techniques in the unrestricted ABAP language scope [Standard ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_abap_glosry.htm) such as the generation or execution of programs at runtime. They are not part of this cheat sheet. Find more details on the related syntax (e. g. `GENERATE SUBROUTINE POOL`, `READ REPORT` and `INSERT REPORT` in the ABAP Keyword Documentation for Standard ABAP: [Dynamic Program Development (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_language_dynamic.htm)

## Executable Example

[zcl_demo_abap_dynamic_prog](./src/zcl_demo_abap_dynamic_prog.clas.abap)

> **💡 Note**<br>
> - The executable example ...
>   - covers the following topics, among others:
>     - Field symbols and data references as supporting elements for dynamic programming
>     - Dynamic ABAP syntax components
>     - Runtime type services (RTTS), i. e. runtime type identification (RTTI) and runtime type creation (RTTC)
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](README.md#%EF%B8%8F-disclaimer)
