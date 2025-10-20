<a name="top"></a>

# Dynamic Programming

- [Dynamic Programming](#dynamic-programming)
  - [Introduction](#introduction)
  - [Excursion: Field Symbols and Data References](#excursion-field-symbols-and-data-references)
    - [Field Symbols](#field-symbols)
      - [Declaring Field Symbols](#declaring-field-symbols)
      - [Assigning Data Objects](#assigning-data-objects)
      - [Checking Field Symbol Assignment and Unassigning Field Symbols](#checking-field-symbol-assignment-and-unassigning-field-symbols)
      - [Examples Using Field Symbols](#examples-using-field-symbols)
      - [Generic Typing](#generic-typing)
    - [Data References](#data-references)
      - [Declaring Data Reference Variables](#declaring-data-reference-variables)
      - [Assigning References to Existing Data Objects](#assigning-references-to-existing-data-objects)
      - [Creating New Data Objects at Runtime (Anonymous Data Objects)](#creating-new-data-objects-at-runtime-anonymous-data-objects)
      - [Excursion with Object Reference Variables: Creating Objects as Instances of Classes and CREATE OBJECT Statements](#excursion-with-object-reference-variables-creating-objects-as-instances-of-classes-and-create-object-statements)
      - [Assignments Between Two Reference Variables (Static and Dynamic Type, Upcast and Downcast)](#assignments-between-two-reference-variables-static-and-dynamic-type-upcast-and-downcast)
      - [Addressing Data References](#addressing-data-references)
      - [Checking if Data Reference Variables Can Be Dereferenced](#checking-if-data-reference-variables-can-be-dereferenced)
      - [Excursion: Generic Data References and Field Symbols](#excursion-generic-data-references-and-field-symbols)
      - [Examples Using Data References](#examples-using-data-references)
  - [Dynamic ABAP Statements](#dynamic-abap-statements)
    - [Dynamic ASSIGN Statements](#dynamic-assign-statements)
      - [ASSIGN Statements and Setting sy-subrc](#assign-statements-and-setting-sy-subrc)
    - [Creating Anonymous Data Objects by Specifying the Type Dynamically](#creating-anonymous-data-objects-by-specifying-the-type-dynamically)
    - [Creating Instances of Classes by Specifying the Type Dynamically](#creating-instances-of-classes-by-specifying-the-type-dynamically)
    - [Accessing Structure Components Dynamically](#accessing-structure-components-dynamically)
    - [Dynamic Specifications in Statements for Processing Internal Tables](#dynamic-specifications-in-statements-for-processing-internal-tables)
    - [Dynamic ABAP SQL Statements](#dynamic-abap-sql-statements)
    - [Dynamic Method Calls](#dynamic-method-calls)
    - [Dynamic Function Module Calls](#dynamic-function-module-calls)
    - [Dynamic ABAP EML Statements](#dynamic-abap-eml-statements)
    - [Dynamically Calling Transformations](#dynamically-calling-transformations)
    - [Dynamic Formatting Option Specifications in String Templates](#dynamic-formatting-option-specifications-in-string-templates)
    - [Dynamic Parameter List in EXPORT and IMPORT Statements](#dynamic-parameter-list-in-export-and-import-statements)
  - [Security Considerations in Dynamic Programming Using External Input](#security-considerations-in-dynamic-programming-using-external-input)
  - [Runtime Type Services (RTTS)](#runtime-type-services-rtts)
    - [Getting Type Information at Runtime (RTTI)](#getting-type-information-at-runtime-rtti)
      - [RTTI: Attribute Access and Method Calls](#rtti-attribute-access-and-method-calls)
      - [Example: Exploring the RTTI Type Hierarchy](#example-exploring-the-rtti-type-hierarchy)
      - [Excursion: Inline Declaration, CAST Operator, Method Chaining](#excursion-inline-declaration-cast-operator-method-chaining)
      - [Absolute Names](#absolute-names)
      - [Constants of Type Description Classes](#constants-of-type-description-classes)
    - [Dynamically Creating Data Types (RTTC) and Data Objects at Runtime Using Type Description Objects](#dynamically-creating-data-types-rttc-and-data-objects-at-runtime-using-type-description-objects)
      - [Getting Type Description Objects](#getting-type-description-objects)
      - [Creating Elementary Types and Data Objects Dynamically](#creating-elementary-types-and-data-objects-dynamically)
      - [Creating Structured Types and Data Objects Dynamically](#creating-structured-types-and-data-objects-dynamically)
      - [Creating Table Types and Internal Tables Dynamically](#creating-table-types-and-internal-tables-dynamically)
      - [Creating Reference Types and Data Reference Variables Dynamically](#creating-reference-types-and-data-reference-variables-dynamically)
  - [Excursion: Dynamic Program Development in Standard ABAP](#excursion-dynamic-program-development-in-standard-abap)
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

- Further aspects of dynamic programming in ABAP enter the picture if you want to determine information about data types and data objects at runtime ([RTTI](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm)) or even create them ([RTTC](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_creation_glosry.htm)).

- In general, dynamic programming also comes with some downsides. For example:
  - The ABAP compiler cannot check the dynamic programming feature like the `SELECT` statement mentioned above. There is no syntax warning or suchlike. 
  - The checks are performed only at runtime, which has an impact on the performance. 
  - The testing of [procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm "Glossary Entry") that include dynamic programming features may be difficult.
  - ⚠️ Dynamic programming techniques can pose significant security risks if not used correctly. You should thoroughly check or escape any dynamic content received from external sources before using it in dynamic statements. You can achieve this using the system class `CL_ABAP_DYN_PRG` or the built-in `escape` function.


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursion: Field Symbols and Data References

[Field symbols](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm "Glossary Entry")
and [data references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_glosry.htm "Glossary Entry") are supporting elements for dynamic programming.

### Field Symbols

Field symbols ...

- are symbolic names for almost any data object or parts of existing data objects.
- can be assigned actual memory areas at program runtime (using `ASSIGN`). Note that you can only work with the field symbols if indeed they have been assigned before.
- can be used as placeholders for a data object at an [operand position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm).
   - Consider there is a data object in your program. A field symbol is also available that is assigned the memory area of this data object. Accessing a field symbol is like accessing the [named data object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennamed_data_object_glosry.htm) or part of the object itself.
- do not reserve physical space in the [data area](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_area_glosry.htm) of a program like a data object. Instead, they work as dynamic identifiers of a memory area in which a specific data object or part of an object is located.
- can be typed either with [generic data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengeneric_data_type_glosry.htm "Glossary Entry") or [complete data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplete_data_type_glosry.htm "Glossary Entry"). 
- are declared using the statement [`FIELD-SYMBOLS`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapfield-symbols.htm) or the [declaration operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_operator_glosry.htm) [`FIELD-SYMBOL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield-symbol_inline.htm). Their names must be included between angle brackets.

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Declaring Field Symbols

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

> [!NOTE] 
>- After its declaration, a field symbol is initial, i. e. a memory area is not (yet) assigned to it (apart from the inline declaration). If you use an unassigned field symbol, a runtime error occurs.     
>    ```abap
>    FIELD-SYMBOLS <fs> TYPE string.
>    "As the field symbol is unassigned, the following statement causes a runtime error.
>    <fs> = `hello`.
>    ```
>-   There are plenty of options for generic ABAP types. A prominent one
    is `data` that stands for any data type. See more information in the
    topic [Generic ABAP
    Types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuilt_in_types_generic.htm) and in a code snippet below.
>-   Field symbols cannot be declared in the declaration part of
    [classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_glosry.htm "Glossary Entry")
    and
    [interfaces](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoo_intf_glosry.htm "Glossary Entry").

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Assigning Data Objects

[`ASSIGN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapassign.htm)
statements assign the memory area of a data object to a field symbol.
Once the memory area is assigned, you can work with the content.

The table below includes a selection of `ASSIGN` statements, primarily featuring static assignments. More additions are covered further down. For more information, see the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENSET_FIELD_SYMBOLS.html).

> [!NOTE] 
> In the event of an unsuccessful static assignment, `sy-subrc` remains unchanged (with exceptions), and no memory area is allocated to the field symbol. After the statement, the field symbol is unassigned. The addition `ELSE UNASSIGN`, although it cannot be explicitly specified in static assignments, is used implicitly.


<table>

<tr>
<th> Subject </th> <th> Example </th>
</tr>

<tr>
<td> 

Assigning memory area to an existing field symbol with [complete data type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplete_data_type_glosry.htm)

 </td>

 <td> 


``` abap
DATA str_a TYPE string VALUE `ABAP`.
DATA int_a TYPE i VALUE 123.
DATA struc_a TYPE i_timezone.
DATA tab_a TYPE string_table.

FIELD-SYMBOLS <fs_str_a> TYPE string.
FIELD-SYMBOLS <fs_int_a> TYPE i.
FIELD-SYMBOLS <fs_struc_a> TYPE i_timezone.
FIELD-SYMBOLS <fs_tab_a> LIKE tab_a.

ASSIGN str_a TO <fs_str_a>.
ASSIGN int_a TO <fs_int_a>.
ASSIGN struc_a TO <fs_struc_a>.
ASSIGN tab_a TO <fs_tab_a>.
``` 

 </td>
</tr>

<tr>
<td> 

Assigning memory area to an existing field symbol with [generic data type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengeneric_data_type_glosry.htm)

 </td>

 <td> 


``` abap
DATA str_b TYPE string VALUE `ABAP`.
DATA int_b TYPE i VALUE 123.
DATA dec34_b TYPE decfloat34 VALUE '0.123'.
DATA struc_b TYPE i_timezone.
DATA strtab_b TYPE string_table.
DATA hashtab_b TYPE string_hashed_table.

FIELD-SYMBOLS <fs_any_b> TYPE any.
FIELD-SYMBOLS <fs_numeric_b> TYPE numeric.
FIELD-SYMBOLS <fs_clike_b> TYPE clike.
FIELD-SYMBOLS <fs_anytab_b> TYPE ANY TABLE.

ASSIGN str_b TO <fs_any_b>.
ASSIGN int_b TO <fs_any_b>.
ASSIGN dec34_b TO <fs_any_b>.
ASSIGN struc_b TO <fs_any_b>.
ASSIGN strtab_b TO <fs_any_b>.
ASSIGN hashtab_b TO <fs_any_b>.

ASSIGN int_b TO <fs_numeric_b>.
ASSIGN dec34_b TO <fs_numeric_b>.

ASSIGN str_b TO <fs_clike_b>.
ASSIGN struc_b TO <fs_clike_b>.

ASSIGN strtab_b TO <fs_anytab_b>.
ASSIGN hashtab_b TO <fs_anytab_b>.
``` 

 </td>
</tr>

<tr>
<td> 

Assigning memory area to a field symbol declared inline

 </td>

 <td> 


``` abap
DATA str_c TYPE string VALUE `ABAP`.
DATA int_c TYPE i VALUE 123.
DATA strtab_c TYPE string_table.

ASSIGN str_c TO FIELD-SYMBOL(<fs_str_c>).
ASSIGN int_c TO FIELD-SYMBOL(<fs_int_c>).

LOOP AT strtab_c ASSIGNING FIELD-SYMBOL(<fs_wa_c>).
 ...
ENDLOOP.

READ TABLE strtab_c ASSIGNING FIELD-SYMBOL(<fs_rt_c>) INDEX 1.
``` 

 </td>
</tr>

<tr>
<td> 

Assigning components

 </td>

 <td> 


``` abap
DATA: BEGIN OF struc_d,
        text TYPE c LENGTH 10,
        num  TYPE i,
      END OF struc_d.
DATA tab_d LIKE TABLE OF struc_d WITH EMPTY KEY.
struc_d = VALUE #( text = 'ABAP' num = 1 ).
APPEND struc_d TO tab_d.

FIELD-SYMBOLS <fs_txt_d> LIKE struc_d-text.

ASSIGN struc_d-text TO <fs_txt_d>.

"Note: With table expressions, sy-subrc is set.
ASSIGN tab_d[ 1 ]-num TO FIELD-SYMBOL(<fs_wa_d>).
ASSERT sy-subrc = 0.

ASSIGN tab_d[ 2 ]-num TO <fs_wa_d>.
ASSERT sy-subrc <> 0.
``` 

 </td>
</tr>

<tr>
<td> 

Offset and length specifications

 </td>

 <td> 

Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPASSIGN_MEM_AREA_STATIC_DOBJ.html).

<br>

``` abap
DATA txt_e TYPE c LENGTH 10 VALUE '0123456789'.
TYPES c1 TYPE c LENGTH 1.
FIELD-SYMBOLS <fs_txt_e> TYPE c1.

DO 11 TIMES.
  DATA(off) = sy-index - 1.
  ASSIGN txt_e+off(1) TO <fs_txt_e>.

  IF sy-index < 11.
    ASSERT <fs_txt_e> IS ASSIGNED.
  ELSE.
    ASSERT <fs_txt_e> IS NOT ASSIGNED.
  ENDIF.
ENDDO.

"The following example explores area limits of data objects that are assigned.
"The first ASSIGN statement assigns the area limits of the data object txt_e to a field symbol.
"In the second ASSIGN statement in a loop, the second field symbol takes over the area limits.
"This ASSIGN statement does not specify the offset.
"From a specific loop pass on, the assignment does not work anymore as a larger memory area
"is assigned. Therefore, the logical expression is then false.

FIELD-SYMBOLS: <fs_any1_e> TYPE any,
               <fs_any2_e> TYPE any.
"345
ASSIGN txt_e+3(3) TO <fs_any1_e>.

DATA strtab_e TYPE string_table.
DO 10 TIMES.
  ASSIGN <fs_any1_e>(sy-index) TO <fs_any2_e>.
  IF <fs_any2_e> IS ASSIGNED.
    APPEND |{ <fs_any2_e> } / sy-index = { sy-index }| TO strtab_e.
  ELSE.
    APPEND |Field symbol not assigned / sy-index = { sy-index }| TO strtab_e.
  ENDIF.
ENDDO.

*strtab_e table content: 
*3 / sy-index = 1                           
*34 / sy-index = 2                          
*345 / sy-index = 3                         
*3456 / sy-index = 4                        
*34567 / sy-index = 5                       
*345678 / sy-index = 6                      
*3456789 / sy-index = 7                     
*Field symbol not assigned / sy-index = 8   
*Field symbol not assigned / sy-index = 9   
*Field symbol not assigned / sy-index = 10  
``` 

 </td>
</tr>


<tr>
<td> 

`CASTING` addition for matching types of data object and field symbol

 </td>

 <td> 

Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapassign_casting.htm).

<br>

``` abap
DATA txt1_f TYPE c LENGTH 7 VALUE 'abcdefg'.
TYPES c3 TYPE c LENGTH 3.
FIELD-SYMBOLS <fs_c3_f> TYPE c3.

"Implicit casting
"abc
ASSIGN txt1_f TO <fs_c3_f> CASTING. 

FIELD-SYMBOLS <fs_data_f> TYPE data.

"Explicit casting
"abc
ASSIGN txt1_f TO <fs_data_f> CASTING TYPE c3. 

DATA txt2_f TYPE c LENGTH 4.
"abcd
ASSIGN txt1_f TO <fs_data_f> CASTING LIKE txt2_f. 
``` 

 </td>
</tr>


<tr>
<td> 

Assigning writable expressions

 </td>

 <td> 


``` abap
"Table expressions
"Note: sy-subrc is set. If the line is found, sy-subrc is set to 0.

DATA(strtab_f) = VALUE string_table( ( `ABAP` ) ).

ASSIGN strtab_f[ 1 ] TO FIELD-SYMBOL(<fs_wa_g>).
ASSERT sy-subrc = 0.

ASSIGN strtab_f[ 2 ] TO <fs_wa_g>.
ASSERT sy-subrc <> 0.

"Constructor expressions

"NEW
"Assigning instance attributes of a class
ASSIGN NEW zcl_demo_abap_objects( )->another_string TO FIELD-SYMBOL(<fs_attribute1_g>).

"CAST
TYPES: BEGIN OF struc_g,
         comp1 TYPE i,
         comp2 TYPE i,
       END OF struc_g.

DATA dref_g TYPE REF TO data.
dref_g = NEW struc_g( comp1 = 1 comp2 = 2 ).

ASSIGN CAST struc_g( dref_g )->comp1 TO FIELD-SYMBOL(<fs_comp1_g>).
ASSIGN CAST struc_g( dref_g )->comp2 TO FIELD-SYMBOL(<fs_comp2_g>).
ASSIGN CAST struc_g( dref_g )->* TO FIELD-SYMBOL(<fs_struc_g>).

DATA iref TYPE REF TO zdemo_abap_objects_interface.
DATA(oref) = NEW zcl_demo_abap_objects( ).
oref->another_string = `hello`.
iref = oref.
iref->in_str = `world`.

ASSIGN CAST zcl_demo_abap_objects( iref )->another_string TO FIELD-SYMBOL(<fs_attribute2_g>).
ASSIGN CAST zdemo_abap_objects_interface( oref )->in_str TO FIELD-SYMBOL(<fs_attribute3_g>).
``` 

 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Checking Field Symbol Assignment and Unassigning Field Symbols

- If you use an unassigned field symbol, an exception is raised. Before using it, you can check the assignment with the `IS ASSIGNED` logical expression. The statement is true if the field symbol is assigned.  
- Using the statement `UNASSIGN`, you can explicitly remove the assignment of the field symbol. A `CLEAR` statement only initializes the value.

> [!NOTE] 
> As covered further down, in dynamic assignments of field symbols, `sy-subrc` is set, unlike in static assignments. You can use the `ELSE UNASSIGN` addition in dynamic assignments.  While `ELSE UNASSIGN` can't be explicitly specified in static assignments, it is used implicitly. Note a potential pitfall when checking field symbol assignments with `IS ASSIGNED` in dynamic assignments. An example in the dynamic assignment section illustrates the pitfall.


```abap
FIELD-SYMBOLS <fs> TYPE string.
DATA(some_string) = `hello`.
ASSIGN some_string TO <fs>.

IF <fs> IS ASSIGNED.
  DATA(fs_assigned) = `is assigned`.
ELSE.
  fs_assigned = `is not assigned`.
ENDIF.

DATA(fs_assigned_w_cond) = COND #( WHEN <fs> IS ASSIGNED THEN `is assigned` ELSE `is not assigned` ).

UNASSIGN <fs>.
ASSERT <fs> IS NOT ASSIGNED.

FIELD-SYMBOLS <another_fs> TYPE i.
DATA num TYPE i VALUE 1.
ASSERT <another_fs> IS NOT ASSIGNED.
ASSIGN num TO <another_fs>.
ASSERT <another_fs> IS ASSIGNED.
"The following statement only initializes the value and does 
"not remove the assignment.
CLEAR <another_fs>.
ASSERT <another_fs> IS ASSIGNED.
ASSERT <another_fs> = 0.
ASSERT num = 0.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Examples Using Field Symbols

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

"READ TABLE statements
READ TABLE itab INDEX 1 ASSIGNING FIELD-SYMBOL(<rt>).  
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Generic Typing

``` abap
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
                                       "The types REF TO object and REF TO data are considered complete types.

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

s-oref = NEW cl_system_uuid( ).
ASSIGN s-oref TO <object>.
```

> [!NOTE] 
> After `TYPE REF TO`, the only generic types you can specify are `data` for fully generic data reference variables and `object` for fully generic object reference variables.

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Declaring Data Reference Variables

``` abap
"The following example shows a selection of declaration options with data reference 
"variables with static types. The static types can be complete or generic (but 
"only data can be used). 
"Note that the variables do not yet point to a data object. At this stage, 
"initial reference variables contain null references. 
DATA num type i.

DATA: ref_a TYPE REF TO i,                 "Complete data type
      ref_b TYPE REF TO zdemo_abap_carr,   "Complete data type
      ref_c LIKE REF TO num,
      ref_d TYPE REF TO data,              "Generic data type
      ref_e LIKE ref_a,                    "Referring to an existing data reference variable
      ref_f TYPE TABLE OF REF TO i,        "Reference table, complete data type
      ref_g TYPE TABLE OF REF TO data.     "Reference table, generic data type                 
```

As shown below, instead of the explicit declaration, inline declarations are also possible. 
See also the cheat sheet [Data Types and Data Objects](16_Data_Types_and_Objects.md).

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Assigning References to Existing Data Objects

Assigning references to existing data objects [reference operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_operator_glosry.htm "Glossary Entry")
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

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Creating New Data Objects at Runtime (Anonymous Data Objects) 
[Anonymous data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm "Glossary Entry") ...
- are dynamically created at runtime. 
- are relevant if the data type is only known when the program is executed.
- cannot be addressed by a name ("anonymous"). 
- expect a data reference variable when declared. The content of an anonymous data object can only be accessed using dereferenced variables as shown below or field symbols.
- can be created using the statement [`CREATE DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_data.htm), the instance operator [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm), or the addition [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_into_target.htm) of the `INTO` clause in a `SELECT` statement. 

> [!NOTE] 
> The following snippet covers statically defined types. Data objects can also be created with `CREATE DATA` dynamically using dynamic type definitions (the type name is specified within a pair of parentheses) and type description objects ([`TYPE HANDLE` addition](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcreate_data_handle.htm)) as shown further down. 

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Excursion with Object Reference Variables: Creating Objects as Instances of Classes and CREATE OBJECT Statements

- To create an object, a [reference variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_variable_glosry.htm "Glossary Entry")
must be declared.
- Such an [object reference variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm "Glossary Entry")
is required to access objects and their components. That means objects are not directly accessed but only via references that point to those objects. This object reference variable contains the reference to the object - after assigning the reference to the object.
- Using [`CREATE OBJECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_object.htm) statements, you can create an object as an instance of a class and assign the reference to the object to an [object reference variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm). 
- The instance operator [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm) basically replaces `CREATE OBJECT` statements. 
- However, `CREATE OBJECT` statements are still required and they are the only option for creating objects dynamically (`NEW` is not possible in that context) as shown further down.
- In the example, note the the built-in generic ABAP type `object` (`TYPE REF TO object`) that is used for the generic typing of object references. It is for any object type. `object` stands for the root class of the inheritance hierarchy. More information: [Generic ABAP Types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuilt_in_types_generic.htm)
- Find more information in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_object.htm) and the [ABAP Object Orientation](04_ABAP_Object_Orientation.md) cheat sheet

```abap
DATA oref_a TYPE REF TO zcl_demo_abap.
oref_a = NEW #( ).
"Using inline declaration
DATA(oref_b) = NEW zcl_demo_abap( ).
"Generic type
DATA oref_c TYPE REF TO object.
oref_c = NEW zcl_demo_abap( ).

"This object creation with the NEW operator corresponds to the older 
"syntax using CREATE OBJECT, and replaces it. See more examples in the 
"context of dynamic object creation that require the CREATE OBJECT 
"syntax further down.
DATA oref_d TYPE REF TO zcl_demo_abap.
CREATE OBJECT oref_d.
DATA oref_e TYPE REF TO object.
CREATE OBJECT oref_e TYPE zcl_demo_abap.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Assignments Between Two Reference Variables (Static and Dynamic Type, Upcast and Downcast)

As mentioned above regarding the assignment, note that static types of both data
reference variables must be compatible. As a result of an assignment, both the target reference variable and the source reference variable point to the same (data) object.

Excursion: Static vs. dynamic type, upcasts and downcasts
- Data reference variables have ...
  - a [static type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_type_glosry.htm "Glossary Entry"). This is the type you specify when declaring the variable, i. e. `i` is the static type in this example: `DATA ref TYPE REF TO i.`. The static type can also be a generic data type: `DATA ref TYPE REF TO data.`.
  - a [dynamic type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendynamic_type_glosry.htm "Glossary Entry"), the type of a (data) object to which the reference variable actually points to at runtime.
- For an assignment to work, the differentiation is particularly relevant since the following basic rule applies: The static type of the target reference variable must be more general than or the same as the dynamic type of the source reference variable.

- This is where the concept of [upcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenup_cast_glosry.htm "Glossary Entry") and [downcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendown_cast_glosry.htm "Glossary Entry") enters the picture.
  - This concept originates from the idea of moving up or down in an [inheritance tree](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninheritance_tree_glosry.htm). In an assignment between reference variables, the target variable inherits the dynamic type of the source variable.
  - **Upcast**: If the static type of the target variables is **less specific or the same** as the static type of the source variable, an assignment is possible. This includes, for example, assignments with the [assignment operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm) `=`.
  - **Downcast**: If the static type of the target variable is **more specific** than the static type of the source variable, a check must be made at runtime before the assignment is done. If you indeed want to trigger such a downcast, you must do it explicitly in your code. You can do this, for example, using the
  [constructor operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_operator_glosry.htm "Glossary Entry")
[`CAST`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_cast.htm). In older code, you may see the use of the [`?=`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove_cast.htm) operator.
  - In contrast to a downcast, an upcast does not have to be done explicitly. However, you can - but need not - use the mentioned operators for upcasts, too.

The code snippet below demonstrates upcasts and downcasts with data reference variables, but also object reference variables to visualize moving up and down an inheritance tree. The examples in the code snippet use object reference variables to illustrate the class hierarchy of the [Runtime Type Services (RTTS)](#runtime-type-services-rtts), which is covered in more detail further down. You can find the hierarchy tree of the classes [here](#runtime-type-services-rtts).

``` abap
*&---------------------------------------------------------------------*
*& Object reference variables
*&---------------------------------------------------------------------*

"Static and dynamic types
"Defining an object reference variable with a static type
DATA tdo TYPE REF TO cl_abap_typedescr.

"Retrieving type information
"The reference the reference variable points to is either cl_abap_elemdescr,
"cl_abap_enumdescr, cl_abap_refdescr, cl_abap_structdescr, or cl_abap_tabledescr.
"So, it points to one of the subclasses. The static type of tdo refers to
"cl_abap_typedescr, however, the dynamic type is one of the subclasses mentioned.
"in the case of the example, it is cl_abap_elemdescr. Check in the debugger.
DATA some_string TYPE string.
tdo = cl_abap_typedescr=>describe_by_data( some_string ).

"Some more object reference variables
DATA tdo_super TYPE REF TO cl_abap_typedescr.
DATA tdo_elem TYPE REF TO cl_abap_elemdescr.
DATA tdo_data TYPE REF TO cl_abap_datadescr.
DATA tdo_gen_obj TYPE REF TO object.

*&---------------------------------------------------------------------*
*& Upcasts
*&---------------------------------------------------------------------*

"Moving up the inheritance tree
"Assignments:
"- If the static type of target variable is less specific or the same, an assignment works.
"- The target variable inherits the dynamic type of the source variable.

"Static type of target variable is the same
tdo_super = tdo.

"Examples for static types of target variables that are less specific
"Target variable has the generic type object
tdo_gen_obj = tdo.

"Target variable is less specific because the direct superclass of cl_abap_elemdescr
"is cl_abap_datadescr
"Note: In the following three assignments, the target variable remains initial 
"since the source variables do not (yet) point to any object.
tdo_data = tdo_elem.

"Target variable is less specific because the direct superclass of cl_abap_datadescr
"is cl_abap_typedescr
tdo_super = tdo_data.

"Target variable is less specific because the class cl_abap_typedescr is higher up in
"the inheritance tree than cl_abap_elemdescr
tdo_super = tdo_elem.

"The casting happens implicitly. You can also excplicitly cast and use
"casting operators, but it is usually not required.
tdo_super = CAST #( tdo ).
tdo_super ?= tdo.

"In combination with inline declarations, the CAST operator can be used to provide a
"reference variable with a more general type.
DATA(tdo_inl_cast) = CAST cl_abap_typedescr( tdo_elem ).

CLEAR: tdo_super, tdo_elem, tdo_data, tdo_gen_obj.

*&---------------------------------------------------------------------*
*& Downcasts
*&---------------------------------------------------------------------*

"Moving down the inheritance tree
"Assignments:
"- If the static type of the target variable is more specific than the static type
"  of the source variable, performing a check whether it is less specific or the same
"  as the dynamic type of the source variable is required at runtime before the assignment
"- The target variable inherits the dynamic type of the source variable, however, the target
"  variable can accept fewer dynamic types than the source variable
"- Downcasts are always performed explicitly using casting operators

"Static type of the target is more specific
"object -> cl_abap_typedescr
tdo_super = CAST #( tdo_gen_obj ).
"cl_abap_typedescr -> cl_abap_datadescr
"Note: Here, the dynamic type of the source variable is cl_abap_elemdescr.
tdo_data = CAST #( tdo ).
"cl_abap_datadescr -> cl_abap_elemdescr
tdo_elem = CAST #( tdo_data ).
"cl_abap_typedescr -> cl_abap_elemdescr
tdo_elem = CAST #( tdo_super ).

*&---------------------------------------------------------------------*
*& Error prevention in downcasts
*&---------------------------------------------------------------------*

"In the examples above, the assignments work. The following code snippets
"deal with examples in which a downcast is not possible. An exception is
"raised.
DATA str_table TYPE string_table.
DATA tdo_table TYPE REF TO cl_abap_tabledescr.

"With the following method call, tdo points to an object with
"reference to cl_abap_tabledescr.
tdo = cl_abap_typedescr=>describe_by_data( str_table ).

"Therefore, the following downcast works.
tdo_table = CAST #( tdo ).

"You could also achieve the same in one statement and with inline
"declaration.
DATA(tdo_table_2) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( str_table ) ).

"Example for an impossible downcast
"The generic object reference variable points to cl_abap_elemdescr after the following
"assignment.
tdo_gen_obj = cl_abap_typedescr=>describe_by_data( some_string ).

"Without catching the exception, the runtime error MOVE_CAST_ERROR
"occurs. There is no syntax error at compile time. The static type of
"tdo_gen_obj is more general than the static type of the target variable.
"The error occurs when trying to downcast, and the dynamic type is used.
TRY.
    tdo_table = CAST #( tdo_gen_obj ).
  CATCH cx_sy_move_cast_error.
ENDTRY.
"Note: tdo_table still points to the reference as assigned above after trying
"to downcast in the TRY control structure.

"Using CASE TYPE OF and IS INSTANCE OF statements, you can check if downcasts
"are possible.
"Note: In case of ...
"- non-initial object reference variables, the dynamic type is checked.
"- initial object reference variables, the static type is checked.

*&---------------------------------------------------------------------*
*& IS INSTANCE OF
*&---------------------------------------------------------------------*

DATA some_tdo TYPE REF TO cl_abap_typedescr.
some_tdo = cl_abap_typedescr=>describe_by_data( str_table ).

IF some_tdo IS INSTANCE OF cl_abap_elemdescr.
  DATA(tdo_a) = CAST cl_abap_elemdescr( some_tdo ).
ELSE.
  "This branch is executed. The downcast is not possible.
  ...
ENDIF.

IF some_tdo IS INSTANCE OF cl_abap_elemdescr.
  DATA(tdo_b) = CAST cl_abap_elemdescr( some_tdo ).
ELSEIF some_tdo IS INSTANCE OF cl_abap_refdescr.
  DATA(tdo_c) = CAST cl_abap_refdescr( some_tdo ).
ELSEIF some_tdo IS INSTANCE OF cl_abap_structdescr.
  DATA(tdo_d) = CAST cl_abap_structdescr( some_tdo ).
ELSEIF some_tdo IS INSTANCE OF cl_abap_tabledescr.
  "In this example, this branch is executed. With the check,
  "you can make sure that the downcast is indeed possible.
  DATA(tdo_e) = CAST cl_abap_tabledescr( some_tdo ).
ELSE.
  ...
ENDIF.

DATA initial_tdo TYPE REF TO cl_abap_typedescr.

IF initial_tdo IS INSTANCE OF cl_abap_elemdescr.
  DATA(tdo_f) = CAST cl_abap_elemdescr( some_tdo ).
ELSEIF initial_tdo IS INSTANCE OF cl_abap_refdescr.
  DATA(tdo_g) = CAST cl_abap_refdescr( some_tdo ).
ELSEIF initial_tdo IS INSTANCE OF cl_abap_structdescr.
  DATA(tdo_h) = CAST cl_abap_structdescr( some_tdo ).
ELSEIF initial_tdo IS INSTANCE OF cl_abap_tabledescr.
  DATA(tdo_i) = CAST cl_abap_tabledescr( some_tdo ).
ELSE.
  "In this example, this branch is executed. The static
  "type of the initial object reference variable is used,
  "which is cl_abap_typedescr here.
  ...
ENDIF.

*&---------------------------------------------------------------------*
*& CASE TYPE OF
*&---------------------------------------------------------------------*

"The examples are desinged similarly to the IS INSTANCE OF examples.

DATA(dref) = REF #( str_table ).
some_tdo = cl_abap_typedescr=>describe_by_data( dref ).

CASE TYPE OF some_tdo.
  WHEN TYPE cl_abap_elemdescr.
    DATA(tdo_j) = CAST cl_abap_elemdescr( some_tdo ).
  WHEN TYPE cl_abap_refdescr.
    "In this example, this branch is executed. With the check,
    "you can make sure that the downcast is indeed possible.
    DATA(tdo_k) = CAST cl_abap_refdescr( some_tdo ).
  WHEN TYPE cl_abap_structdescr.
    DATA(tdo_l) = CAST cl_abap_structdescr( some_tdo ).
  WHEN TYPE cl_abap_tabledescr.
    DATA(tdo_m) = CAST cl_abap_tabledescr( some_tdo ).
  WHEN OTHERS.
    ...
ENDCASE.

"Example with initial object reference variable
CASE TYPE OF initial_tdo.
  WHEN TYPE cl_abap_elemdescr.
    DATA(tdo_n) = CAST cl_abap_elemdescr( some_tdo ).
  WHEN TYPE cl_abap_refdescr.
    DATA(tdo_o) = CAST cl_abap_refdescr( some_tdo ).
  WHEN TYPE cl_abap_structdescr.
    DATA(tdo_p) = CAST cl_abap_structdescr( some_tdo ).
  WHEN TYPE cl_abap_tabledescr.
    DATA(tdo_q) = CAST cl_abap_tabledescr( some_tdo ).
  WHEN OTHERS.
    "In this example, this branch is executed. The static
    "type of the initial object reference variable is used,
    "which is cl_abap_typedescr here.
    ...
ENDCASE.

*&---------------------------------------------------------------------*
*& Data reference variables
*&---------------------------------------------------------------------*

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
ref5 ?= ref6.

"Note: The cast operators can also but need not be specified for upcasts.
ref4 = CAST #( ref3 ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Addressing Data References

Before addressing the content of data objects a data reference points to, you must dereference data reference variables. Use the
[dereferencing operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm "Glossary Entry")
`->*`. When dereferencing a data reference variable that has a structured data type, you can use the component selector `->` to address individual components.


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

"Explicitly removing a reference
"However, the garbage collector takes care of removing the references
"automatically once the data is not used any more by a reference.
CLEAR ref_carr.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Checking if Data Reference Variables Can Be Dereferenced

To check if dereferencing works, you can use a logical expression with [`IS [NOT] BOUND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_bound.htm).

```abap
DATA(ref_carr) = NEW zdemo_abap_carr( carrid = 'LH' carrname = 'Lufthansa' ).

"Checking if a data reference variable can be dereferenced.
IF ref_carr IS BOUND.
  DATA(ref_bound) = `is bound`.
ELSE.
  ref_bound = `is not bound`.
ENDIF.

DATA(ref_bound_w_cond) = COND #( WHEN ref_carr IS BOUND THEN `is bound` ELSE `is not bound` ).

DATA some_ref TYPE REF TO string.

ASSERT some_ref IS NOT BOUND.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


#### Excursion: Generic Data References and Field Symbols

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
"Note: As the example uses a fully generic type, explicit or implicit index 
"operations are not allowed.
"dref->*[ 1 ]-('CARRID') = 'ZZ'.

"Table functions
DATA(num_tab_lines) = lines( dref->* ).
DATA(idx) = line_index( dref->*[ ('CARRID') = 'LH' ] ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Examples Using Data References

Some example contexts of using data references are as follows:

*Overwriting data reference variables*:

``` abap
DATA dref TYPE REF TO data.
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

*Internal tables of type REF TO data can contain any data references*:

```abap
DATA it_ref TYPE TABLE OF REF TO data.

"Such a table can hold any data type
it_ref = VALUE #( ( NEW i( 3 ) ) "Elementary type
                  ( NEW string( `hello` ) ) "Elementary type
                  ( NEW zdemo_abap_flsch( carrid = 'XY' connid = '1234' ) ) "Structured type
                  ( NEW string_table( ( `a` ) ( `b` ) ( `c` ) ) ) "Table type
                ).
```

> [!TIP]
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

*&---------------------------------------------------------------------*
*& Specifying the memory area dynamically
*&---------------------------------------------------------------------*

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

"Do not use named data objects in ABAP for Cloud Development because
"you cannot know what data object you get (e.g. a class attribute or a
"local variable). If you want to assign a data object from your local
"scope, it is recommended that you specify the data objects to
"be assigned as components of a structure. Then you can ensure that
"you assign the correct data object dynamically, using the dynamic
"component assignments as shown below.

DATA some_string TYPE string VALUE `hi`.
DATA(some_named_dobj) = 'SOME_STRING'.

"A warning is displayed for the following statement in ABAP for Cloud
"Development.
"ASSIGN (some_named_dobj) TO FIELD-SYMBOL(<fs>).

*&---------------------------------------------------------------------*
*& Assigning components dynamically
*&---------------------------------------------------------------------*

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
"If the compiler can fully determine the data object in ASSIGN
"statements in ABAP for Cloud Development, a warning is not issued.
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

"Looping across a structure
"The example assumes that all component values are convertible
"to string.
DATA struc_as_string TYPE string.

DO.
  ASSIGN st-(sy-index) TO <fs>.
  IF sy-subrc = 0.
    struc_as_string &&= |{ <fs> } |.
  ELSE.
    EXIT.
  ENDIF.
ENDDO.

CLEAR struc_as_string.

DO.
  ASSIGN COMPONENT sy-index OF STRUCTURE st TO <fs>.
  IF sy-subrc = 0.
    struc_as_string &&= |{ <fs> } |.
  ELSE.
    EXIT.
  ENDIF.
ENDDO.

*&---------------------------------------------------------------------*
*& Assigning attributes of classes or interfaces dynamically
*&---------------------------------------------------------------------*

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
"If ELSE UNASSIGN is specified and assignment does not work in the context 
"of dynamic assignments/accesses, no memory area is assigned to the field 
"symbol. It is unassigned after the ASSIGN statement.
"Note: For the static variant of the ASSIGN statement, i.e. if the memory area
"to be assigned following the ASSIGN keyword is statically specified, the addition
"ELSE UNASSIGN is implicitly set and cannot be used explicitly.
DATA(hello) = `Hello world`.
ASSIGN ('HELLO') TO FIELD-SYMBOL(<eu>) ELSE UNASSIGN.
ASSERT sy-subrc = 0 AND <eu> IS ASSIGNED.
"ASSIGN without ELSE UNASSIGN
ASSIGN ('DOES_NOT_EXIST') TO <eu>.
ASSERT sy-subrc = 4 AND <eu> IS ASSIGNED AND <eu> = `Hello world`.
"ASSIGN with ELSE UNASSIGN
ASSIGN ('DOES_NOT_EXIST') TO <eu> ELSE UNASSIGN.
ASSERT sy-subrc = 4 AND <eu> IS NOT ASSIGNED.

*&---------------------------------------------------------------------*
*& Assigments and casting a dynamically specified type
*&---------------------------------------------------------------------*

"Pattern: ASSIGN ... TO <fs> CASTING TYPE (type_name).

"Assigning a data object to a field symbol casting a dynamically
"specified type as also shown in the example above
TYPES clen5 TYPE c LENGTH 5.
DATA: dobj_c10    TYPE c LENGTH 10 VALUE '1234567890',
      some_struct TYPE zdemo_abap_fli.
FIELD-SYMBOLS <casttype> TYPE data.

ASSIGN dobj_c10 TO <casttype> CASTING TYPE ('CLEN5').  "12345
ASSIGN dobj_c10 TO <casttype> CASTING LIKE some_struct-('CARRID'). "123

TYPES: c1  TYPE c LENGTH 1,
        c3  TYPE c LENGTH 3,
        c10 TYPE c LENGTH 10,
        c20 TYPE c LENGTH 20,
        str TYPE string.
DATA abc TYPE c LENGTH 26 VALUE 'abcdefghijklmnopqrstuvwxyz'.
DATA(type_names) = VALUE string_table( ( `C1` ) ( `C3` ) ( `C10` ) ( `C20` ) ( `NOPE` ) ( `STR` ) ).
DATA assignment_results TYPE string_table.
FIELD-SYMBOLS <clike> TYPE clike.

LOOP AT type_names INTO DATA(type_name).
  TRY.
      ASSIGN abc TO <clike> CASTING TYPE (type_name).
      assignment_results = VALUE #( BASE assignment_results ( |Type: '{ type_name }'; Assignment result: '{ <clike> }'| ) ).
    CATCH cx_root INTO DATA(error).
      assignment_results = VALUE #( BASE assignment_results
      ( |Error! Exception raised: { cl_abap_typedescr=>describe_by_object_ref( error )->get_relative_name( ) }; | &&
        |'{ error->get_text( ) }'| ) ).
  ENDTRY.
ENDLOOP.

*Content of the assignment_results table:
*Type: 'C1'; Assignment result: 'a'
*Type: 'C3'; Assignment result: 'abc'
*Type: 'C10'; Assignment result: 'abcdefghij'
*Type: 'C20'; Assignment result: 'abcdefghijklmnopqrst'
*Error! Exception raised: CX_SY_ASSIGN_CAST_UNKNOWN_TYPE; 'ASSIGN ... CASTING failed; NOPE is an unknown type'
*Error! Exception raised: CX_SY_ASSIGN_CAST_ILLEGAL_CAST; 'ASSIGN ... CASTING failed: Incompatible type'

*&---------------------------------------------------------------------*
*& Assigments and dynamic casting using a type description object
*&---------------------------------------------------------------------*

"Note: As covered further down and in the executable example,
"CREATE DATA and ASSIGN statements have the HANDLE addition
"after which dynamically created types can be specified. A type
"description object is expected.

"Getting type description object
DATA(tdo_elem) = cl_abap_elemdescr=>get_c( 4 ).
ASSIGN dobj_c10 TO <casttype> CASTING TYPE HANDLE tdo_elem. "1234
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### ASSIGN Statements and Setting sy-subrc

The following code snippet focuses on `ASSIGN` statements and setting the `sy-subrc` value (0 = successful, 4 = unsuccessful):
- For static assignments that fail, `sy-subrc` remains unset. The `ELSE UNASSIGN` addition cannot be explicitly defined for static assignments but is applied implicitly.
- However, `sy-subrc` is set when using table expressions.
- `sy-subrc` is set for unsuccessful dynamic assignments, including dynamic component access and dynamic class or interface component access.
- In the case of dynamic assignments, you can explicitly specify the `ELSE UNASSIGN` addition.

```abap
"Demo data objects to work with
DATA str TYPE string VALUE `hello`.
DATA: BEGIN OF struct,
        comp1 TYPE i,
        comp2 TYPE string,
      END OF struct.
DATA itab LIKE TABLE OF struct WITH EMPTY KEY.
struct = VALUE #( comp1 = 1 comp2 = `ABAP` ).
APPEND struct TO itab.
DATA dref TYPE REF TO i.

FIELD-SYMBOLS <fs> TYPE data.

*&---------------------------------------------------------------------*
*& Static ASSIGN statements
*&---------------------------------------------------------------------*

ASSERT <fs> IS NOT ASSIGNED.

ASSIGN str TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

UNASSIGN <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS NOT ASSIGNED.

ASSIGN struct TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

ASSIGN struct-comp1 TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

ASSIGN itab TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

"Unsuccessful static assignment demonstrated with substring access
FIELD-SYMBOLS <year>  TYPE n.
FIELD-SYMBOLS <month> TYPE n.
FIELD-SYMBOLS <day>   TYPE n.

DATA(date) = cl_abap_context_info=>get_system_date( ).

ASSIGN date+0(4) TO <year>.
ASSERT sy-subrc = 0.
ASSERT <year> IS ASSIGNED.

ASSIGN date+4(2) TO <month>.
ASSERT sy-subrc = 0.
ASSERT <month> IS ASSIGNED.

DATA len TYPE i VALUE 3.
ASSIGN date+6(len) TO <day>.
"Despite the unsuccessful assignment, sy-subrc is 0.
ASSERT sy-subrc = 0.
ASSERT <day> IS NOT ASSIGNED.

"In static ASSIGN statements using table expressions, sy-subrc is set.
ASSIGN itab[ 1 ] TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

ASSIGN itab[ 2 ] TO <fs>.
ASSERT sy-subrc = 4.

"Pitfall!
"Although the previous assertion was not successfull,
"the field symbol is still assigned from the ASSIGN
"statement above.
"ASSERT <fs> IS NOT ASSIGNED.

ASSERT <fs> IS ASSIGNED.
ASSERT <fs> = itab[ 1 ].

"Assigning table components using table expressions
ASSIGN itab[ 1 ]-comp1 TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

ASSIGN itab[ 2 ]-comp1 TO <fs>.
ASSERT sy-subrc = 4.
ASSERT <fs> IS ASSIGNED.
ASSERT <fs> = itab[ 1 ]-comp1.

*&---------------------------------------------------------------------*
*& Dynamic ASSIGN statements
*&---------------------------------------------------------------------*

"Data reference variable is not bound
ASSERT dref IS NOT BOUND.
ASSIGN dref->* TO <fs>.
ASSERT sy-subrc = 4.
"Note: Despite the previous unsuccessful assignement,
"the field symbol is still assigned.
ASSERT <fs> IS ASSIGNED.
ASSERT <fs> = itab[ 1 ]-comp1.

"Data reference variable is bound
dref = NEW #( 1 ).
ASSERT dref IS BOUND.
ASSIGN dref->* TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

"To explicitly unassign a field symbol in dynamic assignments,
"you can specify the ELSE UNASSIGN addition.
CLEAR dref.
ASSERT dref IS NOT BOUND.
ASSIGN dref->* TO <fs> ELSE UNASSIGN.
ASSERT sy-subrc = 4.
ASSERT <fs> IS NOT ASSIGNED.

"Dynamic data object specification
ASSIGN ('STR') TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

ASSIGN ('NOPE') TO <fs>.
ASSERT sy-subrc = 4.
"The field symbol is still assigned
ASSERT <fs> IS ASSIGNED.

ASSIGN ('BLABLA') TO <fs> ELSE UNASSIGN.
ASSERT sy-subrc = 4.
ASSERT <fs> IS NOT ASSIGNED.

"Dynamic component assignment
ASSIGN struct-('COMP2') TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

ASSIGN struct-('NOPE') TO <fs> ELSE UNASSIGN.
ASSERT sy-subrc = 4.
ASSERT <fs> IS NOT ASSIGNED.

ASSIGN struct-(1) TO <fs> ELSE UNASSIGN.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

ASSIGN struct-(3) TO <fs> ELSE UNASSIGN.
ASSERT sy-subrc = 4.
ASSERT <fs> IS NOT ASSIGNED.

"Dynamic class component assignment
ASSIGN zcl_demo_abap_objects=>('PUBLIC_STRING') TO <fs> ELSE UNASSIGN.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

ASSIGN zcl_demo_abap_objects=>('NOPE') TO <fs> ELSE UNASSIGN.
ASSERT sy-subrc = 4.
ASSERT <fs> IS NOT ASSIGNED.

"Note that there are cases of invalid dynamic specifications in dynamic
"component assignments that raise exceptions and do not set sy-subrc.
TRY.
    ASSIGN itab[ 1 ]-('ZZZ') TO <fs>.
  CATCH cx_sy_assign_illegal_component.
ENDTRY.
ASSERT sy-subrc = 0.

"Assigning instance attributes of a class and directly creating the
"instance
"ELSE UNASSIGN cannot be specified here.
ASSIGN NEW zcl_demo_abap_objects( )->another_string TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

TRY.
    ASSIGN NEW zcl_demo_abap_objects( )->('NOPE') TO <fs>.
  CATCH cx_sy_assign_illegal_component.
ENDTRY.
"Despite the unsuccessful assignment, sy-subrc is 0 and the
"field symbol is still assigned.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

DATA iref TYPE REF TO zdemo_abap_objects_interface.
DATA(oref) = NEW zcl_demo_abap_objects( ).
oref->another_string = `hello`.
iref = oref.

"ELSE UNASSIGN cannot be specified here.
ASSIGN CAST zcl_demo_abap_objects( iref )->another_string TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.

TRY.
    ASSIGN CAST zcl_demo_abap_objects( iref )->('NOPE') TO <fs>.
  CATCH cx_sy_assign_illegal_component.
ENDTRY.
"Despite the unsuccessful assignment, sy-subrc is 0 and the
"field symbol is still assigned.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.
```


The following example:
- Highlights the setting of `sy-subrc` after a dynamic assignment and the `ELSE UNASSIGN` addition, only for dynamic assignments.
- Points out a potential issue when checking field symbol assignment using `IS ASSIGNED` in dynamic assignments.
- Demonstrates dynamic access to structure components sequentially (the `sy-index` value indicates the processed field from left to right in the structure). The number of loop iterations exceeds the number of structure components. The second loop uses the `ELSE UNASSIGN` addition. You can compare the results of the two internal tables.

```abap
DATA: BEGIN OF demo_struc,
    comp1 TYPE c LENGTH 1 VALUE 'a',
    comp2 TYPE c LENGTH 1 VALUE 'b',
    comp3 TYPE c LENGTH 1 VALUE 'c',
    comp4 TYPE c LENGTH 1 VALUE 'd',
    comp5 TYPE c LENGTH 1 VALUE 'e',
  END OF demo_struc.

FIELD-SYMBOLS <fs> TYPE data.
DATA fs_assignment_check_1 TYPE string_table.
DATA fs_assignment_check_2 TYPE string_table.

DO 7 TIMES.
  APPEND |****** Loop pass no. { sy-index } ******************************************| TO fs_assignment_check_1.
  APPEND |{ COND #( WHEN <fs> IS ASSIGNED THEN |Before dynamic assignment: <fs> is assigned, value "{ <fs> }"| ELSE `Check before dynamic assignment: <fs> is not assigned` ) }| TO fs_assignment_check_1.

  ASSIGN demo_struc-(sy-index) TO <fs>.

  APPEND |{ COND #( WHEN sy-subrc = 0 THEN |<fs> was successfully assigned, value "{ <fs> }"| ELSE `<fs> was not assigned` ) }| TO fs_assignment_check_1.
  APPEND |{ COND #( WHEN <fs> IS ASSIGNED THEN |<fs> is assigned, value "{ <fs> }"| ELSE `<fs> is not assigned` ) }| TO fs_assignment_check_1.
ENDDO.

DO 7 TIMES.
  APPEND |****** Loop pass no. { sy-index } ******************************************| TO fs_assignment_check_2.
  APPEND |{ COND #( WHEN <fs> IS ASSIGNED THEN |Before dynamic assignment: <fs> is assigned, value "{ <fs> }"| ELSE `Check before dynamic assignment: <fs> is not assigned` ) }| TO fs_assignment_check_2.

  ASSIGN demo_struc-(sy-index) TO <fs> ELSE UNASSIGN.

  APPEND |{ COND #( WHEN sy-subrc = 0 THEN |<fs> was successfully assigned, value "{ <fs> }"| ELSE `<fs> was not assigned` ) }| TO fs_assignment_check_2.
  APPEND |{ COND #( WHEN <fs> IS ASSIGNED THEN |<fs> is assigned, value "{ <fs> }"| ELSE `<fs> is not assigned` ) }| TO fs_assignment_check_2.
ENDDO.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Anonymous Data Objects by Specifying the Type Dynamically

- As shown above, you create anonymous data objects using `CREATE DATA` statements and assign the reference to the data object to a reference variable. 
- You can dynamically specify the type name in parentheses. 
- In addition to character-like data objects (such as literals and variables) for the type name specified in the parentheses, you can also use [absolute type names](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabsolute_typename_glosry.htm). 
- You can also use type description objects and the [`TYPE HANDLE` addition](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcreate_data_handle.htm) to create anonymous data objects dynamically. For this and the absolute names, find more information below in the section about RTTS.

``` abap
*&---------------------------------------------------------------------*
*& CREATE DATA statement patterns
*&---------------------------------------------------------------------*

"CREATE DATA dref TYPE (typename) ...
"CREATE DATA dref TYPE ... TABLE OF (typename) ...
"CREATE DATA dref TYPE REF TO (typename).
"CREATE DATA dref TYPE LINE OF (typename).
"CREATE DATA dref LIKE struc-(compname).
"CREATE DATA dref TYPE (absolute_name).
"CREATE DATA dref TYPE HANDLE type_description_object.

*&---------------------------------------------------------------------*
*& Specifying a type name dynamically
*&---------------------------------------------------------------------*

"Anonymous data objects are created using a type determined at
"runtime. In this case, the name of the data type is specified
"dynamically.
"Note that the NEW operator cannot be used here.

"Data reference variable used for the examples
DATA dref TYPE REF TO data.

"Example types and data objects

"Elementary type and data object
TYPES t_c3 TYPE c LENGTH 3.
DATA c3 TYPE c LENGTH 3.

"Structured type and data object
TYPES t_fli_struc TYPE zdemo_abap_fli.
DATA  fli_struc TYPE zdemo_abap_fli.

"Table type and internal table
TYPES t_carr_tab TYPE SORTED TABLE OF zdemo_abap_carr WITH UNIQUE KEY carrid.
DATA carr_tab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.

"Reference type and data reference variable
TYPES t_str_ref TYPE REF TO string.
DATA str_ref TYPE REF TO string.

*&---------------------------------------------------------------------*
*& Pattern: TYPE (typename) ...
*&---------------------------------------------------------------------*

"Creating an elementary data object
"Specifying a literal for the dynamic type name (used in most of the
"following examples)
CREATE DATA dref TYPE ('T_C3').

"Specifying a named data object
DATA(some_type) = 'T_C3'.
CREATE DATA dref TYPE (some_type).

"Structured data object
CREATE DATA dref TYPE ('T_FLI_STRUC').

"Internal table
CREATE DATA dref TYPE ('T_CARR_TAB').

"Data reference
CREATE DATA dref TYPE ('T_STR_REF').

*&---------------------------------------------------------------------*
*& Pattern: TYPE ... TABLE OF (typename) ...
*&---------------------------------------------------------------------*

"Creating internal tables
"Note that the syntax with CREATE DATA does not support the specification of
"secondary table keys. This is possible with RTTI and using a type description object.

CREATE DATA dref TYPE TABLE OF ('STRING').
CREATE DATA dref TYPE TABLE OF ('T_FLI_STRUC') WITH EMPTY KEY.

"Specifying the structured type dynamically, but the key values statically
CREATE DATA dref TYPE SORTED TABLE OF ('ZDEMO_ABAP_CARR') WITH UNIQUE KEY carrid.

"Specifying the structured type and the key values dynamically
"An internal table such as the following should be created by dynamically
"specifying the type and keys dynamically. The keys are specified in lines
"of an internal table with character-like line type.
DATA itab TYPE SORTED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid connid fldate.

DATA(key_table) = VALUE string_table( ( `CARRID` ) ( `CONNID` ) ( `FLDATE` ) ).
CREATE DATA dref TYPE SORTED TABLE OF ('ZDEMO_ABAP_FLI') WITH UNIQUE KEY (key_table).

*&---------------------------------------------------------------------*
*& Pattern: TYPE REF TO (typename)
*&---------------------------------------------------------------------*

"Creating data reference variables

CREATE DATA dref TYPE REF TO ('STRING').
CREATE DATA dref TYPE REF TO ('T_C3').
CREATE DATA dref TYPE REF TO ('T_FLI_STRUC').
CREATE DATA dref TYPE REF TO ('T_CARR_TAB').

*&---------------------------------------------------------------------*
*& Pattern: TYPE LINE OF (typename)
*&---------------------------------------------------------------------*

"Creating structures based on table types
CREATE DATA dref TYPE LINE OF ('T_CARR_TAB').

*&---------------------------------------------------------------------*
*& Pattern: LIKE struc-(dobjname)
*&---------------------------------------------------------------------*

CREATE DATA dref LIKE fli_struc-('CARRID').

*&---------------------------------------------------------------------*
*& Pattern: TYPE (absolute_name)
*&---------------------------------------------------------------------*

CREATE DATA dref TYPE ('\TYPE=STRING').
"Getting an absolute type name; see more information further down
DATA(absolute_name) = cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_CARR' )->absolute_name.
CREATE DATA dref TYPE (absolute_name).

*&---------------------------------------------------------------------*
*& Pattern: TYPE HANDLE type_description_object
*&---------------------------------------------------------------------*

"Getting a type description object. Find more information about RTTI below.
DATA(tdo_elem) = cl_abap_elemdescr=>get_c( 4 ). "type c length 4
CREATE DATA dref TYPE HANDLE tdo_elem.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Instances of Classes by Specifying the Type Dynamically

- [`CREATE OBJECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_object_explicit.htm) statements can be used to create instances of classes by specifying the type dynamically.
- It assigns the reference to the object to an object reference variable.
- The `NEW` operator cannot be used to create instances of classes by specifying the type dynamically.


```abap
DATA oref_dyn TYPE REF TO object.
CREATE OBJECT oref_dyn TYPE ('ZCL_DEMO_ABAP_OBJECTS').

DATA cl TYPE string VALUE `ZCL_DEMO_ABAP_OBJECTS`.
CREATE OBJECT oref_dyn TYPE (cl).

"Specifying a wrong/non-existent type name
TRY.
    CREATE OBJECT oref_dyn TYPE ('THIS_CLASS_DOES_NOT_EXIST').
  CATCH cx_sy_create_object_error.
ENDTRY.

"Using an absolute name for the dynamic type specification
"Getting the absolute name using RTTI
DATA(abs_name_cl) = cl_abap_typedescr=>describe_by_name( 'ZCL_DEMO_ABAP_OBJECTS' )->absolute_name.
CREATE OBJECT oref_dyn TYPE (abs_name_cl).
CREATE OBJECT oref_dyn TYPE ('\CLASS=ZCL_DEMO_ABAP_OBJECTS').

"Note: If the class has an instance constructor specified with importing
"parameters, you can (or must in the case of non-optional parameters) pass
"actual parameters.
"This can be done either statically using the addition EXPORTING, and
"specifying the parameters statically, or by dynamically specifying the
"parameters in a parameter table. See the dynamic method calls section for
"examples on the parameter table and the static parameter passing.
"The addition EXCEPTIONS and an exception table are also available
"to handle non-class-based exceptions.
"CREATE OBJECT oref_dyn TYPE (...) EXPORTING p1 = a1 ...
"CREATE OBJECT oref_dyn TYPE (...) PARAMETER-TABLE ...
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

"You can achieve the access using ASSIGN statements as shown above
"(using field symbols), ...
ASSIGN st-('COL1') TO FIELD-SYMBOL(<col1>).

"... or by statically specifying the structure and the (object) component
"selector followed by a character-like data object in parentheses.
"Write position
st-('COL1') = 123.
it[ 1 ]-('COL1') = 456.
dref->('COL1') = 789.

"Read position
"The example shows how you can retrieve the textual content of any component
"of any structure. As a prerequisite, the components must be convertible
"to type string in the example.
DATA content_col1 TYPE string.
content_col1 = st-('COL1').
DATA(content_col2) = CONV string( st-('COL2') ).
DATA(content_col3) = |{ st-('COL3') }|.

"The following example creates an anonymous data object. The dereferenced
"data reference variable is assigned the component value. Using the LIKE
"addition, the appropriate type is specified (and not the string type as 
"above for the structure component col1 that is of type i).
DATA dref_comp TYPE REF TO data.
CREATE DATA dref_comp LIKE st-('COL1').
dref_comp->* = st-('COL1').
"As shown further down, using RTTI to get the absolute type name of the 
"dereferenced data reference variable. 
DATA(type_used) = cl_abap_typedescr=>describe_by_data(
   dref_comp->* )->absolute_name. "\TYPE=I

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

"------------ Excursions ------------
"The following example "loops" across a structure
"Demo structure, all components are convertible to type string
TYPES: BEGIN OF ty_struc,
         comp1 TYPE c LENGTH 3,
         comp2 TYPE string,
         comp3 TYPE i,
         comp4 TYPE n LENGTH 4,
       END OF ty_struc.
DATA(struct) = VALUE ty_struc( comp1 = 'abc' comp2 = `ABAP` comp3 = 123 comp4 = '9876' ).
DATA looped_struc1 TYPE string.
DATA looped_struc2 TYPE string.

"In the loop, a string is populated, component by component.
"Two alternatives are demonstrated. The newer syntax with the component selector and dynamically
"specifying the position (which is represented by the sy-index value), and an ASSIGN COMPONENT
"statement.
DO.
  TRY.
      looped_struc1 = |{ looped_struc1 }{ COND #( WHEN sy-index <> 1 THEN `, ` ) }"{ struct-(sy-index) }"|.

      ASSIGN COMPONENT sy-index OF STRUCTURE struct TO FIELD-SYMBOL(<fs>).
      looped_struc2 = |{ looped_struc2 }{ COND #( WHEN sy-index <> 1 THEN ` / ` ) }"{ <fs> }"|.
    CATCH cx_sy_assign_illegal_component.
      EXIT.
  ENDTRY.
ENDDO.

*Result:
*"abc", "ABAP", "123", "9876"
*"abc" / "ABAP" / "123" / "9876"

"In the following example, a structure is assigned to a field symbol that
"has a generic type. The components of the structure are accessed dynamically in 
"a DO loop. The sy-index value is interpreted as the position of the component 
"in the structure. Plus, using RTTI - as also shown further down - the component 
"names are retrieved. Component names and the values are added to a string. As a 
"prerequisite, all component values must be convertible to type string.
DATA struc2string TYPE string.
FIELD-SYMBOLS <strco> TYPE data.
ASSIGN st TO <strco>.

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

*&---------------------------------------------------------------------*
*& SORT (1)
*&---------------------------------------------------------------------*

"Note: See more dynamic specifications with SORT statements
"further down.

"Named data object specified within parenteses
DATA(field_name) = 'COL1'.
SORT itab_ek BY (field_name) DESCENDING.
"Unnamed data object specified within parenteses
SORT itab_ek BY ('COL2') ASCENDING.

*&---------------------------------------------------------------------*
*& READ TABLE
*&---------------------------------------------------------------------*

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

"Specifying the WHERE condition dynamically
DATA(dyn_where_cond_str) = `col3 CS 'y'`.

READ TABLE itab INTO DATA(dyn_res) WHERE (dyn_where_cond_str).
ASSERT sy-tabix = 2.

DATA(dyn_where_cond_tab) = VALUE string_table( ( `col3` ) ( `CS` ) ( `'x'` ) ).

READ TABLE itab INTO dyn_res WHERE (dyn_where_cond_tab).
ASSERT sy-tabix = 3.

*&---------------------------------------------------------------------*
*& Table expressions
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& LOOP AT
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& INSERT
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& MODIFY
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& DELETE
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& SORT (2)
*&---------------------------------------------------------------------*

"Sorting by dynamically specified components in a sort table, i. e.an
"internal table of type abap_sortorder_tab.
"Notes:
"- Each line of this sort table specifies a component of the sort key.
"- If this table is initial, there is no sorting.
"- The sort priority is based on the order of the lines in the sort table.

TYPES: BEGIN OF struct,
          comp1 TYPE i,
          comp2 TYPE string,
          comp3 TYPE c LENGTH 3,
        END OF struct.

DATA it TYPE TABLE OF struct WITH EMPTY KEY.

it = VALUE #( ( comp1 = 1 comp2 = `B` comp3 = 'a' )
              ( comp1 = 1 comp2 = `A` comp3 = 'b' )
              ( comp1 = 2 comp2 = `D` comp3 = 'c' )
              ( comp1 = 2 comp2 = `C` comp3 = 'd' )
              ( comp1 = 3 comp2 = `F` comp3 = 'e' )
              ( comp1 = 3 comp2 = `E` comp3 = 'f' ) ).

DATA(it_original) = it.

"Note: The line type is abap_sortorder.
DATA(sort) = VALUE abap_sortorder_tab( ).

"No sorting because the sort table is initial.
SORT it BY (sort).

it = it_original.
"Note: Ascending is the default sort order. The following example flags
"the descending sort order explicitly.
sort = VALUE abap_sortorder_tab( ( name = `COMP1` descending = 'X' ) ).
SORT it BY (sort).

it = it_original.

sort = VALUE abap_sortorder_tab( ( name = `COMP1` descending = '' )
                                  ( name = `COMP2` descending = 'X' ) ).
SORT it BY (sort).

it = it_original.

"Sort priority based on the order of lines in the sort table
"In this example, the values of comp3 are set up so that a clear
"sort order is determined. Since the component is specified first in the
"sort table, this sorting has priority. Note the values of comp2 in the
"result table.
sort = VALUE abap_sortorder_tab( ( name = `COMP3` descending = 'X' )
                                  ( name = `COMP2` descending = 'X' ) ).
SORT it BY (sort).

"Specifying an invalid component name raises an exception
sort = VALUE abap_sortorder_tab( ( name = `XYZ` descending = 'X' ) ).

TRY.
    SORT it BY (sort).
  CATCH cx_sy_dyn_table_ill_comp_val INTO DATA(error).
ENDTRY.
ASSERT error IS NOT INITIAL.

it = it_original.
"Specifying an expression/functional method call whose result is a sort
"table of type abap_sortorder_tab
"In this case, BY is followed by the expression/functional method call,
"not enclosed in parentheses.

"The example shows expressions with sort tables created inline
SORT it BY VALUE abap_sortorder_tab( ( name = `COMP1` descending = 'X' ) ).

it = it_original.

DATA(comp_names) = VALUE string_table( ( `comp1` ) ( `comp2` ) ).

SORT it BY VALUE abap_sortorder_tab( FOR wa IN comp_names ( name = condense( to_upper( wa ) ) ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic ABAP SQL Statements

```abap
*&---------------------------------------------------------------------*
*& Dynamic SELECT list
*&---------------------------------------------------------------------*

DATA(select_list) = `CARRID, CONNID, FLDATE`.
DATA fli_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

SELECT (select_list)
  FROM zdemo_abap_fli
  INTO CORRESPONDING FIELDS OF TABLE @fli_tab.

*&---------------------------------------------------------------------*
*& Dynamic FROM clause
*&---------------------------------------------------------------------*

DATA(table) = 'ZDEMO_ABAP_FLI'.
SELECT *
  FROM (table)
  INTO TABLE @fli_tab.

*&---------------------------------------------------------------------*
*& Excursion: Compatible target data objects
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& Dynamic WHERE clause
*&---------------------------------------------------------------------*

"The example includes a WHERE clause that is created as an internal
"table with a character-like row type.
DATA(where_clause) = VALUE string_table( ( `CARRID = 'LH'` )
                                         ( `OR` )
                                         ( `CARRID = 'AA'` ) ).

SELECT *
  FROM zdemo_abap_fli
  WHERE (where_clause)
  INTO TABLE NEW @DATA(tab_dyn_where).

"A string as an alternative
DATA(where_clause_string) = `CARRID = 'LH' OR CARRID = 'AA'`.

SELECT *
  FROM zdemo_abap_fli
  WHERE (where_clause_string)
  INTO TABLE NEW @DATA(tab_dyn_where_str).

*&---------------------------------------------------------------------*
*& Dynamic ORDER BY clause
*&---------------------------------------------------------------------*

SELECT *
  FROM zdemo_abap_fli
  ORDER BY (`FLDATE`)
  INTO TABLE NEW @DATA(tab_dyn_order).

*&---------------------------------------------------------------------*
*& SELECT statement with miscellaneous dynamic specifications
*&---------------------------------------------------------------------*

SELECT (`CARRID, CONNID, FLDATE`)
  FROM (`ZDEMO_ABAP_FLI`)
  WHERE (`CARRID <> ``AA```)
  ORDER BY (`FLDATE`)
  INTO TABLE NEW @DATA(tab_dyn_misc).

*&---------------------------------------------------------------------*
*& Dynamic INSERT statement
*&---------------------------------------------------------------------*

"Creating a structure to be inserted into the database table
SELECT SINGLE *
      FROM (table)
      INTO NEW @DATA(dref_struc).
dref_struc->('CARRID') = 'YZ'.

INSERT (table) FROM @dref_struc->*.

*&---------------------------------------------------------------------*
*& Dynamic UPDATE statement
*&---------------------------------------------------------------------*

dref_struc->('CURRENCY') = 'EUR'.
UPDATE (table) FROM @dref_struc->*.

*&---------------------------------------------------------------------*
*& Dynamic MODIFY statement
*&---------------------------------------------------------------------*

dref_struc->('SEATSOCC') = 10.
MODIFY (table) FROM @dref_struc->*.

*&---------------------------------------------------------------------*
*& Dynamic DELETE statement
*&---------------------------------------------------------------------*

DELETE FROM (table) WHERE (`CARRID = 'YZ'`).

*&---------------------------------------------------------------------*
*& Dynamic UPDATE ... SET ... statement
*&---------------------------------------------------------------------*

"Inserting demo data into the database table to work with
TYPES carr_tab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
INSERT ('ZDEMO_ABAP_CARR') FROM TABLE @( VALUE carr_tab( ( carrid = 'WX' carrname = 'WX Airways' )
                                                         ( carrid = 'XY' carrname = 'Air XY' )
                                                         ( carrid = 'YZ' carrname = 'YZ Airlines' ) ) ).

"Note that erroneous dynamic specifications can lead to runtime errors
"In the following example, the final inverted comma is missing in the dynamic
"set clause.
DATA(set_clause) = `CURRCODE = 'EUR`.
DATA(where_cl) = `CARRID = 'WX' OR CARRID = 'XY' OR CARRID = 'YZ'`.

TRY.
    UPDATE ('ZDEMO_ABAP_CARR') SET (set_clause) WHERE (where_cl).
  CATCH cx_sy_dynamic_osql_syntax INTO DATA(error).
    DATA(error_text) = error->get_text( ).
ENDTRY.

"Correcting the dynamic specification
"The example sets the value for a component for all entries.
"The example additionally specifies a (dynamic) WHERE clause
"to restrict the range of entries where the update is performed.
"The database table is also specified dynamically.
set_clause = `CURRCODE = 'EUR'`.

UPDATE ('ZDEMO_ABAP_CARR') SET (set_clause) WHERE (where_cl).

*&---------------------------------------------------------------------*
*& Dynamic UPDATE ... INDICATORS ... statement 
*&---------------------------------------------------------------------*

"The statement changes values of specific fields without overwriting existing values of 
"other fields.

"Notes on the example:
"- A structured type is created with the WITH INDICATORS addition.
"- An internal table from which to update a database table is created.
"- The table includes the indicator structure comp_ind.
"- The table is populated, and two components are flagged as
"  to be updated.
"- Other fields remain unchanged. Note that key fields must be
"  included in ind_tab (indicator setting for key fields has
"  no effect).
"- The UPDATE statement includes dynamically specified
"  indicator syntax. Additionally, the database table is specified
"  dynamically.

"Structured type with WITH INDICATORS addition
TYPES ind_wa TYPE zdemo_abap_carr WITH INDICATORS comp_ind TYPE abap_bool.

DATA ind_tab TYPE TABLE OF ind_wa.

"Filling internal table; only CURRCODE and URL should be updated
ind_tab = VALUE #( ( carrid = 'WX'
                      carrname = 'WX Airways'
                      currcode = 'USD'
                      url = 'some_url_wx'
                      comp_ind-currcode = abap_true
                      comp_ind-url = abap_true )
                    ( carrid = 'XY'
                      carrname = 'Air XY'
                      currcode = 'USD'
                      url = 'some_url_xy'
                      comp_ind-currcode = abap_true
                      comp_ind-url = abap_true )
                    ( carrid = 'YZ'
                      carrname = 'YZ Airlines'
                      currcode = 'USD'
                      url = 'some_url_yz'
                      comp_ind-currcode = abap_true
                      comp_ind-url = abap_true ) ).

DATA(dyn_ind) = `SET STRUCTURE comp_ind`.

UPDATE ('ZDEMO_ABAP_CARR') FROM TABLE @ind_tab INDICATORS (dyn_ind).

DELETE FROM ('ZDEMO_ABAP_CARR') WHERE (where_cl).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

**Excursion**: To take up the use case mentioned in the introduction about retrieving the content of a database table, storing it in an internal table, and
displaying it when the database table name is specified dynamically at
runtime, see the following code snippet. 

> [!NOTE]  
> - The example retrieves the content of a database table, storing it in an internal table, and displaying it when the database table name is specified dynamically at runtime.
> - For demo purposes in test programs, there are quite some ways to achieve it, and that work out of the box. For example, in ABAP for Cloud Development, you can implement the classrun interface `if_oo_adt_classrun` and output content to the ADT console using the `out->write(...).` method. You can also inherit from `cl_demo_classrun` in your class. In classic ABAP, you can, for example and additionally, use `cl_demo_output`.
> - The following example is just ABAP code exploring dynamic programming aspects. Note the [Disclaimer](./README.md#%EF%B8%8F-disclaimer) in the *README* of the cheat sheet repository. It is an example that sets its focus on a dynamic `SELECT` statement and processing internal table content by dynamically accessing structure components.
> - For simplicity, column contents are converted to string here if necessary, i.e. all column contents must be convertible to string. Using the ways mentioned above are way more powerful. For example, in most cases also nested and deep data objects can be displayed for demo purposes.
> - To visualize the result, the snippet uses the classrun methods to display results sequentially - instead of displaying the internal table content retrieved by the `SELECT` statement directly.
> - The example uses database tables from the cheat sheet repository. To fill them, you can use the method call `zcl_demo_abap_aux=>fill_dbtabs( ).`.


```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
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

    "Internal table of type string containing names of database tables;
    "table is looped over to output content of all database tables
    DATA(dbtabs) = VALUE string_table( ( `ZDEMO_ABAP_CARR` )
                                       ( `ZDEMO_ABAP_FLI` )
                                       ( `ZDEMO_WRONG_TABLE` )
                                       ( `ZDEMO_ABAP_FLSCH` ) ).

    LOOP AT dbtabs INTO DATA(dbtab).

      "Checking for allowed input
      TRY.
          DATA(value1) = cl_abap_dyn_prg=>check_allowlist(
              val           = dbtab
              allowlist_str = `ZDEMO_ABAP_CARR,ZDEMO_ABAP_FLI,ZDEMO_ABAP_FLSCH` ).

          "Retrieving database content of a dynamically specified database table (up to 5 rows)
          TRY.
              SELECT *
                FROM (dbtab)
                INTO TABLE NEW @DATA(itab)
                UP TO 5 ROWS.
            CATCH cx_sy_dynamic_osql_semantics INTO DATA(sql_error).
              CLEAR itab->*.
              out->write( sql_error->get_text( ) ).
              CONTINUE.
          ENDTRY.
        CATCH cx_abap_not_in_allowlist INTO DATA(not_allowed).
          out->write( not_allowed->get_text( ) ).
          CONTINUE.
      ENDTRY.

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
                                        NEXT len = COND #( LET lv = |{ <line>-(<co>-name) }| IN
                                                           WHEN strlen( lv ) > len THEN strlen( lv )
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
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic Method Calls

- The following code snippet shows dynamically specifying method calls. Find three example classes below the syntax pattern snippet.
- The snippet covers methods. Dynamic method calls require `CALL METHOD` statements. 


Syntax patterns: 

``` abap
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
```


**Click to expand the expandable sections for example code**

<details>
  <summary>🟢 Example class 1</summary>
  <!-- -->

<br>

The following example class explores dynamic method calls with simple methods. You can create a demo class called `zcl_demo_abap` and copy and paste the following code. Once activated, you can choose *F9* in ADT to run the class. The example is not designed to display output in the console.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS inst_meth1.
    METHODS inst_meth2 IMPORTING text          TYPE string
                       RETURNING VALUE(result) TYPE string.
    CLASS-METHODS stat_meth1.
    CLASS-METHODS stat_meth2 IMPORTING text   TYPE string
                             EXPORTING result TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "The following examples use both named and unnamed data objects randomly,
    "i.e. ...=>(meth_name) or ...=>(`SOME_METH`), for example.
    DATA(cl_name) = `ZCL_DEMO_ABAP`.
    DATA(meth_name1) = `STAT_METH1`.

*&---------------------------------------------------------------------*
*& Calling static methods dynamically
*&---------------------------------------------------------------------*

    "-------- Method without mandatory parameters defined --------
    "The syntax is possible for methods of the same class.
    CALL METHOD (meth_name1).

    "The previous example method call works like me->(meth).
    CALL METHOD me->(meth_name1).

    "-------- Class specified statically, method specified dynamically --------
    CALL METHOD zcl_demo_abap=>(meth_name1).

    "-------- Class specified dynamically, method specified statically --------
    CALL METHOD (`ZCL_DEMO_ABAP`)=>stat_meth1.

    "-------- Class and method specified dynamically --------
    CALL METHOD (`ZCL_DEMO_ABAP`)=>(`STAT_METH1`).

    "-------- Specifying non-optional parameters --------
    CALL METHOD (`ZCL_DEMO_ABAP`)=>stat_meth2 EXPORTING text = `hallo`.

    "Specifying the output parameter is optional
    DATA res TYPE string.
    CALL METHOD (`ZCL_DEMO_ABAP`)=>stat_meth2 EXPORTING text = `hallo` IMPORTING result = res.
    ASSERT res = `HALLO`.

    "-------- Some examples for handling errors when calling methods wrongly --------

    "Instance method called using =>
    TRY.
        CALL METHOD zcl_demo_abap=>(`INST_METH1`).
      CATCH cx_sy_dyn_call_illegal_method.
    ENDTRY.

    "The example method declares a non-optional parameter.
    TRY.
        CALL METHOD (`ZCL_DEMO_ABAP`)=>stat_meth2.
      CATCH cx_sy_dyn_call_param_missing.
    ENDTRY.

    "Specifying a wrong parameter name
    TRY.
        CALL METHOD (`ZCL_DEMO_ABAP`)=>stat_meth2 EXPORTING hallo = `hallo`.
      CATCH cx_sy_dyn_call_param_missing.
    ENDTRY.

    "Assigning wrong, incompatible type
    TRY.
        CALL METHOD (`ZCL_DEMO_ABAP`)=>stat_meth2 EXPORTING text = VALUE string_table( ( `hi` ) ).
      CATCH cx_sy_dyn_call_illegal_type.
    ENDTRY.

    "Specifying wrong parameter kinds (the example method specifies importing
    "and exporting parameters, and not a returning parameter)
    TRY.
        CALL METHOD (`ZCL_DEMO_ABAP`)=>stat_meth2 EXPORTING text = `hallo` RECEIVING result = res.
      CATCH cx_sy_dyn_call_illegal_type.
    ENDTRY.

*&---------------------------------------------------------------------*
*& Calling instance methods dynamically
*&---------------------------------------------------------------------*

    "Creating an instance of a class by specifying the type dynamically
    DATA oref TYPE REF TO object.
    CREATE OBJECT oref TYPE ('ZCL_DEMO_ABAP').

    "--- Object reference variable specified statically, method specified dynamically ---
    "Note: This is a also possible for interface reference variables.
    CALL METHOD oref->(`INST_METH1`).

    "-------- Specifying non-optional parameters --------
    CALL METHOD oref->(`INST_METH2`) EXPORTING text = `abap`.

    CALL METHOD oref->(`INST_METH2`) EXPORTING text = `abap` RECEIVING result = res.
    ASSERT res = `ABAP`.

    "Note that calling static methods using object reference variables is also possible.
    CALL METHOD oref->(`STAT_METH1`).

    CALL METHOD oref->(`STAT_METH2`) EXPORTING text = `test` IMPORTING result = res.
    ASSERT res = `TEST`.

*&---------------------------------------------------------------------*
*& PARAMETER-TABLE addition
*&---------------------------------------------------------------------*

    "------- Static equivalents to the dynamic statement below -------
    DATA(oref_stat) = NEW zcl_demo_abap( ).
    res = oref_stat->inst_meth2( `abc` ).
    ASSERT res = `ABC`.
    "For demo purposes, including chained method call options:
    "Functional method call
    res = NEW zcl_demo_abap( )->inst_meth2( `def` ).
    ASSERT res = `DEF`.
    "Standalone statement
    NEW zcl_demo_abap( )->inst_meth2( EXPORTING text = `ghi` RECEIVING result = res ).
    ASSERT res = `GHI`.

    "------- Dynamic CALL METHOD statements using the PARAMETER-TABLE addition -------

    "Creating parameter table for an instance example method
    DATA(ptab) = VALUE abap_parmbind_tab( ( name  = 'TEXT'
                                            kind  = cl_abap_objectdescr=>exporting
                                            value = NEW string( `jkl` ) )
                                          ( name  = 'RESULT'
                                            kind  = cl_abap_objectdescr=>returning
                                            value = NEW string( ) ) ).

    CALL METHOD oref->(`INST_METH2`) PARAMETER-TABLE ptab.
    "Excursion: Accessing structure components dynamically
    res = ptab[ name = 'RESULT' ]-('VALUE')->*.
    ASSERT res = `JKL`.

    "Creating parameter table for a static example method
    ptab = VALUE abap_parmbind_tab( ( name  = 'TEXT'
                                      kind  = cl_abap_objectdescr=>exporting
                                      value = NEW string( `mno` ) )
                                    ( name  = 'RESULT'
                                      kind  = cl_abap_objectdescr=>importing
                                      value = NEW string( ) ) ).

    "Static/dynamic specification variants
    CALL METHOD (`ZCL_DEMO_ABAP`)=>(`STAT_METH2`) PARAMETER-TABLE ptab.
    res = ptab[ name = 'RESULT' ]-('VALUE')->*.
    ASSERT res = `MNO`.

    CALL METHOD zcl_demo_abap=>(`STAT_METH2`) PARAMETER-TABLE ptab.
    res = ptab[ name = 'RESULT' ]-('VALUE')->*.
    ASSERT res = `MNO`.

    CALL METHOD (`ZCL_DEMO_ABAP`)=>stat_meth2 PARAMETER-TABLE ptab.
    res = ptab[ name = 'RESULT' ]-('VALUE')->*.
    ASSERT res = `MNO`.

  ENDMETHOD.

  METHOD inst_meth1.
    ...
  ENDMETHOD.

  METHOD inst_meth2.
    result = to_upper( text ).
  ENDMETHOD.

  METHOD stat_meth1.
    ...
  ENDMETHOD.

  METHOD stat_meth2.
    result = to_upper( text ).
  ENDMETHOD.

ENDCLASS.
```

</details>  

<br>

<details>
  <summary>🟢 Example class 2</summary>
  <!-- -->

<br>


The following simplified example highlights several things in the context of a dynamic method call example: 
- Dynamic method call and assigning actual parameters to formal parameters statically
- Creating instances of classes dynamically, using generic types
- The concepts of static vs. dynamic type, upcast vs. downcast
- Dynamic ABAP does the same as static ABAP, but with dynamic ABAP, errors may not be discovered until runtime.
- Type compliance (see the [General Rules for Typing](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentyping_check_general.htm))

```abap
CLASS zcl_demo_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS some_method IMPORTING obj TYPE REF TO zcl_demo_abap_objects.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_test IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Creating an instance of a class dynamically
    "Here, an object reference variable of the generic type 'object'
    "is used. This generic type is the static type.
    "After the CREATE OBJECT statement in the example, the dynamic type 
    "is 'ref to zcl_demo_abap_objects'. It is the type which the variable 
    "points to at runtime.
    DATA oref TYPE REF TO object.
    CREATE OBJECT oref TYPE ('ZCL_DEMO_ABAP_OBJECTS').

    "In the example, the some_method method expects an object reference
    "variable with type 'ref to zcl_demo_abap_objects'.
    "A static method call such as the following is not possible. The compiler will
    "raise an error since there is no type compliance. This is because the
    "static type of the formal parameter is not compliant with the static
    "type of oref - even if the dynamic type to which the variable points
    "to at runtime is suitable.
    "some_method( oref ).

    "As a rule, static ABAP does the same as dynamic ABAP. So, the following
    "dynamic statement raises an error at runtime. There is no compiler
    "error shown at compile time.
    TRY.
        CALL METHOD ('SOME_METHOD') EXPORTING obj = oref.
      CATCH cx_sy_dyn_call_illegal_type.
    ENDTRY.

    "See also the following statement. No error at compile time
    "with the nonsense formal parameter.
    "It is checked at runtime and will consequently raise an issue.
    TRY.
        CALL METHOD ('SOME_METHOD') EXPORTING abcdef = oref.
      CATCH cx_sy_dyn_call_param_missing.
    ENDTRY.

    "If you have such a use case, and deal with generic/dynamic types, note
    "the general rules for typing in the ABAP Keyword Documentation.
    "A prior downcast (i.e. from the more general type 'object' to the less
    "specific type zcl_demo_abap_objects) can be done. In this context, note
    "that upcasts are possible (and implicitly done) when assigning the parameters, 
    "but downcasts must always be done explicitly, for example, using the CAST 
    "operator as follows (if you know the type to cast to).
    CALL METHOD ('SOME_METHOD') EXPORTING obj = CAST zcl_demo_abap_objects( oref ).
  ENDMETHOD.

  METHOD some_method.
    ...
  ENDMETHOD.

ENDCLASS.
```

</details>  

<br>

<details>
  <summary>🟢 Example class 3</summary>
  <!-- -->

<br>

The following example demonstrates dynamic method calls with methods defined in interfaces.

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Creating an instance of the local class that implements
    "an interface
    DATA(oref) = NEW lcl( ).

    "Static method call
    DATA(number) = oref->lif~get_number( ).
    out->write( number ).

    "Dynamic method call
    "Note the uppercase.
    TRY.
        CALL METHOD oref->('lif~get_number') RECEIVING number = number.
        out->write( number ).
      CATCH cx_sy_dyn_call_illegal_method INTO DATA(error).
        DATA(error_text) = error->get_text( ).
        out->write( error_text ).
    ENDTRY.

    CALL METHOD oref->('LIF~GET_NUMBER') RECEIVING number = number.
    out->write( number ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (*Local Types* tab in ADT)

 </td>

 <td> 

``` abap
INTERFACE lif.
  METHODS get_number RETURNING VALUE(number) TYPE i.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD lif~get_number.
    number = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                         min  = 1
                                         max  = 1000 )->get_next( ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>
</details>  


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic Function Module Calls

The following code snippet shows a dynamic function module call. Similar to dynamic method calls, a parameter table is used. It has the type `abap_func_parmbind_tab`. The function module name is specified as the value of a character-like data object.
See the section [Function Module Example](13_Program_Flow_Logic.md#function-module-example) in the [Program Flow Logic](13_Program_Flow_Logic.md) cheat sheet that includes the creation of a demo function module and static and dynamic function module calls.

```abap
DATA(func_name) = 'Z_DEMO_ABAP_TEST_FUNC_M'.
DATA(ptab) = VALUE abap_func_parmbind_tab( ( name  = 'NUM1'
                                              kind  = abap_func_exporting
                                              value = NEW i( 3 ) )
                                            ( name  = 'OPERATOR'
                                              kind  = abap_func_exporting
                                              value = NEW string( `+` ) )
                                            ( name  = 'NUM2'
                                              kind  = abap_func_exporting
                                              value = NEW i( 5 ) )
                                            ( name  = 'RESULT'
                                              kind  = abap_func_importing
                                              value = NEW string( ) ) ).

CALL FUNCTION func_name PARAMETER-TABLE ptab.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic ABAP EML Statements

In the context of [RAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_rap_glosry.htm), [ABAP EML](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_eml_glosry.htm) statements are available with dynamic forms. 
Find an example in the [ABAP EML](08_EML_ABAP_for_RAP.md#dynamic-forms-of-eml-statements) cheat sheet and in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeneml.htm).

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamically Calling Transformations

Find more information about calling transformations in the [Working with XML and JSON in ABAP](21_XML_JSON.md) cheat sheet.

```abap
TYPES: BEGIN OF line,
         char TYPE c LENGTH 3,
         int  TYPE i,
       END OF line,
       t_type TYPE TABLE OF line WITH EMPTY KEY.

DATA(tab) = VALUE t_type( ( char = 'aaa' int = 1 )
                          ( char = 'bbb' int = 2 )
                          ( char = 'ccc' int = 3 ) ).

"1. Dynamic specification of the transformation
"   If the transformation does not exist, an exception is raised.
"   The example specifies an ABAP data object (an internal table), which is
"   trasnformed to asXML. The result is of type xstring.
"   'itab' stands for the name of the XML element.
TRY.
    CALL TRANSFORMATION ('ID') SOURCE itab = tab
                               RESULT XML DATA(xml_tab).
  CATCH cx_invalid_transformation.
ENDTRY.

DATA(xml1) = cl_abap_conv_codepage=>create_in( )->convert( xml_tab ).

"2. Dynamic specification of ABAP data objects as source
"   The specification is done using an internal table of type abap_trans_srcbind_tab.
"   As above, the transformation is specified dynamically. Also here, the example specifies an an
"   internal table, which is transformed to asXML.

DATA t_name TYPE string VALUE `ID`.

DATA(srctab) = VALUE abap_trans_srcbind_tab( ( name = 'ITAB' value = REF #( tab ) ) ).
CALL TRANSFORMATION (t_name) SOURCE (srctab)
                             RESULT XML DATA(xml_tab2).

DATA(xml2) = cl_abap_conv_codepage=>create_in( )->convert( xml_tab2 ).

ASSERT xml2 = xml1.

"3. Dynamic specification of ABAP data objects as result
"   The specification is done using an internal table of type abap_trans_resbind_tab.
"   The example transforms to ABAP data. The transformation is also specified dynamically.

DATA tab2 LIKE tab.
DATA(restab) = VALUE abap_trans_resbind_tab( ( name = 'ITAB' value = REF #( tab2 ) ) ).

CALL TRANSFORMATION ('ID') SOURCE XML xml_tab
                           RESULT (restab).

ASSERT tab2 = tab.

"4. Example with all syntax elements specified dynamically
CALL TRANSFORMATION ('ID') SOURCE (srctab)
                           RESULT (restab).

ASSERT tab2 = tab.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic Formatting Option Specifications in String Templates

The following code snippet demonstrates a small selection of dynamic formatting option specifications in string templates.
For more details and a complete list of options, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abapcompute_string_format_options.htm), especially regarding the expected and supported input (attributes of the `CL_ABAP_FORMAT` class). General information on string templates can also be found there and in the [String Processing](07_String_Processing.md#string-templates) cheat sheet. 

```abap
"ALIGN
"Only to be used with WIDTH; only the associated values of the following attributes of the
"class CL_ABAP_FORMAT can be used (they are of type i): A_LEFT (1), A_RIGHT (2), A_CENTER (3)
DATA(some_string) = `##`.
DATA(s1) = |{ some_string WIDTH = 10 ALIGN = (1) }<---|.   "'##        <---'
DATA(right) = 2.
DATA(s2) = |{ some_string WIDTH = 10 ALIGN = (right) }<---|. "'        ##<---'


"The following example uses method chaining with methods of the class
"cl_abap_random_int to get a random integer value (in the range of 1 - 3).
"The get_next method has a returning parameter, and returns an integer value.
DO 5 TIMES.
  DATA(s3) = |{ some_string WIDTH = 10 ALIGN = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                  min  = 1 max = 3 )->get_next( ) }<---|.
ENDDO.

"CASE
"Values to be used: CL_ABAP_FORMAT=>C_RAW (for not changing the case; 0),
"CL_ABAP_FORMAT=>C_UPPER (1), CL_ABAP_FORMAT=>C_LOWER (2)
some_string = `AbAp`.
DATA(s4) = |{ some_string CASE = (1) }|. "ABAP
DATA(s5) = |{ some_string CASE = CONV i( '2' ) }|. "abap
DATA int_tab TYPE TABLE OF i WITH EMPTY KEY.
int_tab = VALUE #( ( 0 ) ( 1 ) ( 2 ) ).
DATA(s6) = |{ some_string CASE = int_tab[ 1 ] }|. "AbAp
```


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic Parameter List in EXPORT and IMPORT Statements

- Used in the context of exporting and importing data clusters
- Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENDATA_CLUSTER.html) and an executable example, including the code snippet below, in the [Working with XML and JSON in ABAP](21_XML_JSON.md) cheat sheet.

```abap
DATA buffer TYPE xstring.

"--------- Dynamic specification of the parameter list ---------
"Note:
"- The parameter list is specified in an index table with two columns.
"- The column names can have random names, but the type must be character-like.
"- The first column represents the parameter name, the second column represents
"  the name of the data object.
"- Note that special behavior applies. See the documentation.

TYPES: BEGIN OF param,
          name TYPE string,
          dobj TYPE string,
        END OF param,
        param_tab_type TYPE TABLE OF param WITH EMPTY KEY.

DATA: txt1 TYPE string VALUE `hello`,
      txt2 TYPE string VALUE `world`,
      txt3 TYPE string VALUE `ABAP`.

DATA(param_table) = VALUE param_tab_type(
  ( name = `txt1` dobj = `txt1` )
  ( name = `txt2` dobj = `txt2` )
  ( name = `txt3` dobj = `txt3` ) ).

EXPORT (param_table) TO DATA BUFFER buffer.

"The example reads the content into structure components.
DATA: BEGIN OF values,
        txt1 TYPE string,
        txt2 TYPE string,
        txt3 TYPE string,
      END OF values.

param_table = VALUE param_tab_type(
  ( name = `txt1` dobj = `values-txt1` )
  ( name = `txt2` dobj = `values-txt2` )
  ( name = `txt3` dobj = `values-txt3` ) ).

IMPORT (param_table) FROM DATA BUFFER buffer.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Security Considerations in Dynamic Programming Using External Input

Dynamic programming techniques can present a security risk, particularly when they incorporate external input. Consider a scenario where a user interface allows users to input values. Dynamic ABAP statements that use this input, such as in ABAP SQL statement clauses, can be vulnerable to SQL injections if the input is not properly checked for malicious content.

It is crucial to perform checks and handle dynamic programming techniques cautiously when including external content. You can use the `CL_ABAP_DYN_PRG` class. If escaping is necessary, you can also and additionally use the built-in function `escape` (which is recommended). The following example illustrates a selection and highlights various aspects. 

For more details, refer to the ABAP Keyword Documentation [here (Standard ABAP documentation)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynamic_programming_scrty.htm).

To try the example out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example uses  objects of the ABAP cheat sheets repository and is set up to display output in the console.
It covers the following aspects: 
- Dynamic `WHERE` clause and specifying the data object holding external input as operand and literal
- Verifying input for not allowed database table access
- Verifying input against a given allowlist
- Potential manipulation of ABAP SQL clauses
- Escaping
  

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "Filling demo database tables of the ABAP cheat sheet repository
    zcl_demo_abap_aux=>fill_dbtabs( ).

*&-------------------------------------------------------------------------*
*& Specifying the data object holding external input as operand and literal
*&-------------------------------------------------------------------------*
    
    "The example explores a dynamic WHERE clause. External content is used
    "in the WHERE clause, unchecked.

    "Assuming the data object 'input' holds external input inserted on a UI.
    DATA(input) = 'LH'.

    "Inserting the input value into a dynamic WHERE clause as literal
    DATA(cond1) = `CARRID = '` && input && `'`.
    SELECT SINGLE * FROM zdemo_abap_fli WHERE (cond1) INTO @DATA(db_entry).

    out->write( data = db_entry name = `db_entry` ).
    out->write( |\n\n| ).

    "Inserting the input value into a dynamic WHERE clause using the data
    "object name
    DATA(cond2) = `CARRID = @input`.
    SELECT SINGLE * FROM zdemo_abap_fli WHERE (cond2) INTO @db_entry.

    out->write( data =  db_entry name = `db_entry` ).
    out->write( |\n\n| ).

    "Assuming bad input is provided that is unchecked
    DATA(bad_input) = |LH' AND CONNID = '401|.

    "Inserting the input value as literal
    "Because of using the value as literal, the WHERE clause
    "can be manipulated, yielding a potentially different
    "result, thus posing a security risk.
    DATA(cond3) = `CARRID = '` && bad_input && `'`.
    SELECT SINGLE * FROM zdemo_abap_fli WHERE (cond3) INTO @db_entry.

    out->write( data =  db_entry name = `db_entry` ).
    out->write( |\n\n| ).

    "Inserting the input value using the data object name
    "In doing so, the WHERE clause becomes erroneous, the ABAP
    "SQL statement cannot be executed.
    DATA(cond4) = `CARRID = @bad_input`.
    TRY.
        SELECT SINGLE * FROM zdemo_abap_fli WHERE (cond4) INTO @db_entry.
        out->write( data =  db_entry name = `db_entry` ).
      CATCH cx_sy_dynamic_osql_error cx_sy_open_sql_data_error INTO DATA(select_error).
        out->write( select_error->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).
    out->write( |{ repeat( val = `*` occ = 70 ) }| ).

*&---------------------------------------------------------------------*
*& Accessing not allowed database tables
*&---------------------------------------------------------------------*

    "Assume the name of a database table is specified externally, and a
    "dynamic ABAP SQL statement uses this name. Potentially, users that
    "are actually not allowed to access the database table may get access.
    "The example uses the CL_ABAP_DYN_PRG class that checks a list of
    "allowed database tables.

    "The following methods check ...
    "- Database table names
    "- Whether the database table is contained in a/certain package/s
    "Assuming you provide incorrect input for the table name, or
    "the table is not contained in the specified packages, you should
    "expect an exception to be raied.

    "Assuming the following data object contains external input
    DATA(input_dbtab_name) = `zdemo_abap_fli`.

    "check_table_name_str method: Specifying a single package
    TRY.
        DATA(dbtab) = cl_abap_dyn_prg=>check_table_name_str(
          val      = to_upper( input_dbtab_name )
          packages = `ZABAP_CHEAT_SHEETS` ).

        SELECT SINGLE * FROM (dbtab) INTO NEW @DATA(ref_db_entry).
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_a_table cx_abap_not_in_package INTO DATA(error_input_dbtab1).
        out->write( error_input_dbtab1->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).

    "check_table_name_tab method: Specifying multiple packages in an internal
    "table
    TRY.
        dbtab = cl_abap_dyn_prg=>check_table_name_tab(
          val      = to_upper( input_dbtab_name )
          packages = VALUE #( ( `ZABAP_CHEAT_SHEETS` )
                              ( `ZSOME_PACKAGE` ) ) ).

        SELECT SINGLE * FROM (dbtab) INTO NEW @ref_db_entry.
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_a_table cx_abap_not_in_package INTO DATA(error_input_dbtab2).
        out->write( error_input_dbtab2->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).

    "Not existant database table/invalid name
    input_dbtab_name = `not_a_dbtab!!`.
    TRY.
        dbtab = cl_abap_dyn_prg=>check_table_name_tab(
          val      = to_upper( input_dbtab_name )
          packages = VALUE #( ( `ZABAP_CHEAT_SHEETS` )
                              ( `ZSOME_PACKAGE` ) ) ).

        SELECT SINGLE * FROM (dbtab) INTO NEW @ref_db_entry.
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_a_table cx_abap_not_in_package INTO DATA(error_input_dbtab3).
        out->write( error_input_dbtab3->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).

    "Database table not existant in packages specified (assuming you have imported
    "the ABAP cheat sheet repository, and the database table is available)
    input_dbtab_name = `zdemo_abap_fli`.
    TRY.
        dbtab = cl_abap_dyn_prg=>check_table_name_tab(
          val      = to_upper( input_dbtab_name )
          packages = VALUE #( ( `SAP_BASIS` ) ) ).

        SELECT SINGLE * FROM (dbtab) INTO NEW @ref_db_entry.
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_a_table cx_abap_not_in_package INTO DATA(error_input_dbtab4).
        out->write( error_input_dbtab4->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).
    out->write( |{ repeat( val = `*` occ = 70 ) }| ).

*&---------------------------------------------------------------------*
*& Verifying input against a given allowlist
*&---------------------------------------------------------------------*

    "Assume a SELECT statement dynamically specifies the column names
    "in the SELECT list. Table columns might be accessed although
    "they should not be.
    "You may check against an allowlist.

    "check_allowlist method
    "In the following examples, a method is used to check whether
    "the input is allowed or not. For this, you specify an allowlist.
    "Here, the relevant parameter expects a comma-separated list of
    "allowed values.

    "Assuming the following data object contains external input
    DATA(input_col_name) = `carrid`.

    TRY.
        DATA(value1) = cl_abap_dyn_prg=>check_allowlist(
            val           = to_upper( input_col_name )
            allowlist_str = `CARRID,CONNID,FLDATE` ).

        SELECT SINGLE (input_col_name) FROM zdemo_abap_fli INTO NEW @ref_db_entry.
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_in_allowlist INTO DATA(error_allowed1).
        out->write( error_allowed1->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).

    "The allowlist_htab formal parameter expects an internal table.
    input_col_name = `price`.
    TRY.
        DATA(value2) = cl_abap_dyn_prg=>check_allowlist(
            val           = to_upper( input_col_name )
            allowlist_htab = VALUE #( ( `CARRID` )
                                      ( `CONNID` )
                                      ( `FLDATE` ) ) ).

        SELECT SINGLE (input_col_name) FROM zdemo_abap_fli INTO NEW @ref_db_entry.
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_in_allowlist INTO DATA(error_allowed2).
        out->write( error_allowed2->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).
    out->write( |{ repeat( val = `*` occ = 70 ) }| ).

*&---------------------------------------------------------------------*
*& Potential manipulation of ABAP SQL clauses
*&---------------------------------------------------------------------*

    "In the following example, a dynamic WHERE clause is set up. For this,
    "it is assumed that the WHERE clause uses external input via input fields.
    "This is represented by the column and value data objects. It is assumed
    "that column holds the name of the table column, value a dedicated value in
    "the specified table column.
    "The cl_abap_dyn_prg class is used to check content in two ways:
    "- Checking if the provided column name is valid using the check_column_name
    "  method.
    "- Using the quote method for putting single quotes around the value and escaping
    "  single quotes.
    "In a DO loop, various example inputs are explored. The fourth loop pass includes
    "bad input without using the quote method. This way, an SQL injection takes
    "place, yielding a different result. In this case, all database table entries
    "are retrieved because the WHERE clause is as follows:
    "CARRID = 'LH' OR CARRID <> 'LH'.
    "This is prevented using the quote method, resulting in a non-functional SELECT
    "statement.

    DATA: column TYPE c LENGTH 30,
          value  TYPE c LENGTH 30.

    DO 4 TIMES.
      CASE sy-index.
        WHEN 1.
          "Working example
          column = 'carrid'.
          value = 'lh'.
        WHEN 2.
          "Invalid column name
          column = '?=('.
          value = 'lh'.
        WHEN 3.
          "Bad input, using cl_abap_dyn_prg
          column = 'carrid'.
          value = |'LH' OR CARRID <> 'LH'|.

        WHEN 4.
          "Bad input, not using cl_abap_dyn_prg
          column = 'carrid'.
          value = |'LH' OR CARRID <> 'LH'|.

      ENDCASE.

      out->write( |---------- Run { sy-index } ----------| ).

      TRY.
          cl_abap_dyn_prg=>check_column_name( column ).
        CATCH cx_abap_invalid_name INTO DATA(error_col_name).
          out->write( error_col_name->get_text( ) ).
      ENDTRY.

      DATA(cond_syntax) = to_upper( column ) && ` = ` &&
      COND #( WHEN sy-index <> 4 THEN cl_abap_dyn_prg=>quote( to_upper( value ) ) ELSE to_upper( value ) ).

      TRY.
          SELECT *
                 FROM zdemo_abap_flsch
                 WHERE (cond_syntax)
                 INTO TABLE @DATA(itab_flsch).

          out->write( itab_flsch ).
        CATCH cx_sy_dynamic_osql_error cx_sy_open_sql_data_error INTO DATA(error_select).
          out->write( error_select->get_text( ) ).
      ENDTRY.

      out->write( |\n\n| ).
    ENDDO.

    "Example manipulating the SET clause in an UPDATE statement
    "Inserting a database table entry to work with in the example
    INSERT zdemo_abap_carr FROM @( VALUE #( carrid = 'XY' carrname = 'XY Airways' currcode = 'EUR' url = 'some_url'  ) ).
    SELECT SINGLE * FROM zdemo_abap_carr WHERE carrid = 'XY' INTO @DATA(row4update).

    out->write( data =  row4update name = `row4update` ).
    out->write( |\n\n| ).

    "Assuming the carrier name is to be changed (that was previously created and retrieved
    "for demo purposes). The carrier name is provided via external input, represented by
    "the following data object assignment.
    DATA(input_carrname) = 'Air XY'.

    "Specifying a potentially dangerous dynamic SET clause by directly using external
    "input in the clause
    DATA(dyn_set_clause) = `CARRNAME = '` && input_carrname && `'`.

    UPDATE zdemo_abap_carr
      SET (dyn_set_clause)
      WHERE carrid = @row4update-carrid.

    SELECT SINGLE * FROM zdemo_abap_carr WHERE carrid = 'XY' INTO @row4update.
    out->write( data =  row4update name = `row4update` ).
    out->write( |\n\n| ).

    "Bad input, not using cl_abap_dyn_prg
    "In the example, the input is manipulated in a way that also changes
    "another field value.
    DATA(bad_input_carrname) = |XY Airways', URL = '#########|.
    dyn_set_clause = `CARRNAME = '` && bad_input_carrname && `'`.

    UPDATE zdemo_abap_carr
      SET (dyn_set_clause)
      WHERE carrid = @row4update-carrid.

    SELECT SINGLE * FROM zdemo_abap_carr WHERE carrid = 'XY' INTO @row4update.
    out->write( data =  row4update name = `row4update` ).
    out->write( |\n\n| ).

    "Bad input, using cl_abap_dyn_prg
    "Undoing the changes for the demo database table row
    MODIFY zdemo_abap_carr FROM @( VALUE #( carrid = 'XY' carrname = 'XY Airways' currcode = 'EUR' url = 'some_url' ) ).
    SELECT SINGLE * FROM zdemo_abap_carr WHERE carrid = 'XY' INTO @row4update.

    bad_input_carrname = |XY Airways', URL = '#########|.
    dyn_set_clause = `CARRNAME = ` && cl_abap_dyn_prg=>quote( bad_input_carrname ).

    TRY.
        UPDATE zdemo_abap_carr
          SET (dyn_set_clause)
          WHERE carrid = @row4update-carrid.
      CATCH cx_sy_open_sql_data_error INTO DATA(error_set).
        out->write( error_set->get_text( ) ).
    ENDTRY.

    out->write( |{ repeat( val = `*` occ = 70 ) }| ).

*&---------------------------------------------------------------------*
*& Escaping
*&---------------------------------------------------------------------*

    "In various contexts, a replacement of special characters may be important.
    "Such an escaping is applied on characters contained in a string according
    "to a set of rules.

    "The following example deals with Cross Site Scripting, e.g. manipulating
    "HTML pages and embedding scripts displayed in a browser. In ABAP, this
    "enters the picture, for example, when directly dealing with the Internet
    "Communication Framework.
    "The built-in function escape can be used to escape content in various contexts.
    "The cl_abap_dyn_prg class also offers methods to escape. However, the function
    "is recommended due to performance reasons.

    "Assuming building HTML code by using external input
    DATA your_name TYPE string.
    your_name = sy-uname.
    DATA(html) = `<p>Hello ` && your_name && `!</p>`.

    out->write( data =  html name = `html` ).
    out->write( |\n\n| ).

    "Embedding potentially malicious scripts into the code
    your_name = `<script>alert("Hmmm... potentially malicious code!");</script>`.
    html = `<p>Hello ` && your_name && `!</p>`.

    "Inserted this in an HTML and run in a browser, an alert will be displayed.
    out->write( data =  html name = `html` ).
    out->write( |\n\n| ).

    "Escaping may be done as follows
    "Check the various methods available for escaping with cl_abap_dyn_prg, as well as
    "the formats in the context of the escape function
    DATA(esc_js_cl) = `<p>Hello ` && cl_abap_dyn_prg=>escape_xss_javascript( html ) && `!</p>`.

    "Using the built-in function escape
    DATA(esc_js_fu) = `<p>Hello ` && escape( val = html format = cl_abap_format=>e_xss_js ) && `!</p>`.

    "Further character handling and escaping examples using the cl_abap_dyn_prg class
    Data(quote) = |10 o'clock|.
    DATA(handle_quotes) = cl_abap_dyn_prg=>quote( quote ).
    Data(backtick) = |The character ` is a backtick|.
    DATA(handle_backtick) = cl_abap_dyn_prg=>quote_str( backtick ).
    DATA(esc_quotes) = cl_abap_dyn_prg=>escape_quotes( quote ).
    DATA(esc_backticks) = cl_abap_dyn_prg=>escape_quotes_str( backtick ).
    "You may also do the escaping using string processing techniques, e.g.
    "using the replace function.
    DATA(esc_quotes_replace) = replace( val = quote sub = |'| with = |''| occ = 0 ).
    DATA(esc_backticks_replace) = replace( val = backtick sub = |`| with = |``| occ = 0 ).

    out->write( data =  esc_js_cl name = `esc_js_cl` ).
    out->write( data =  esc_js_fu name = `esc_js_fu` ).
    out->write( data =  handle_quotes name = `handle_quotes` ).
    out->write( data =  handle_backtick name = `handle_backtick` ).
    out->write( data =  esc_quotes name = `esc_quotes` ).
    out->write( data =  esc_backticks name = `esc_backticks` ).
    out->write( data =  esc_quotes_replace name = `esc_quotes_replace` ).
    out->write( data =  esc_backticks_replace name = `esc_backticks` ).
  ENDMETHOD.
ENDCLASS.
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

### Getting Type Information at Runtime (RTTI)

With RTTI, you can determine data types at runtime using description methods in type description classes.
To get the type information, you can get a reference to a type description object of a type, that is, an instance of a type description class.
The type properties are represented by attributes that are accessible through the type description object.


> [!NOTE]  
> - For each type, there is exactly one type description object. 
> - For each type category (elementary type, table, and so on), there is a type description class (e.g. `CL_ABAP_STRUCTDESCR` for structures, as shown in the hierarchy tree above) that has special attributes (i.e. the properties of the respective types). 
> - References to type description objects can be used, for example, after the `TYPE HANDLE` addition of the `CREATE DATA` and `ASSIGN` statements.


#### RTTI: Attribute Access and Method Calls

The following code example demonstrates a range of RTTI attribute accesses and method calls. It includes retrieving type information at runtime for:
- elementary types
- enumerated types 
- structures 
- internal tables
- data references 
- classes 
- interfaces 

Find more information in the section below. 

To try the example out, create a demo class named `zcl_demo_abap` and paste the code into it. 
The example is not set up to display output in the console. So, after activation, you may want to set a break point at the first position possible and choose *F9* in ADT to execute the class.  You can then walk through the example in the debugger. This will allow you to double-click on the variables and check out the contents. The example is similar to the one below, however, this only focuses on the method calls and attribute accesses without output preparation among others.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "------------------------------------- Elementary types/data objects -------------------------------------

    TYPES elem_type_ts TYPE timestampl.
    DATA ts TYPE elem_type_ts VALUE '20240101081317.81011'.

    DATA(tdo_a) = cl_abap_typedescr=>describe_by_data( ts ).
    "DATA(tdo_a) = cl_abap_typedescr=>describe_by_name( 'ELEM_TYPE_TS' ).

    "Cast to get more specific information
    DATA(tdo_elem) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( ts ) ).
    "DATA(tdo_elem) = CAST cl_abap_elemdescr( tdo_a ).

    DATA(type_category_elem) = tdo_elem->kind.
    DATA(type_kind_elem) = tdo_elem->type_kind.
    DATA(decimals_elem) = tdo_elem->decimals.
    DATA(output_length_elem) = tdo_elem->output_length.
    DATA(absolute_name_elem) = tdo_elem->absolute_name.
    DATA(relative_name_elem) = tdo_elem->get_relative_name( ).
    DATA(is_ddic_type_elem) = tdo_elem->is_ddic_type( ).
    DATA(applies_to_data_elem) = tdo_elem->applies_to_data( CONV timestamp( '20240808112458' ) ).
    DATA(applies_to_dataref_elem) = tdo_elem->applies_to_data_ref( REF #( '20240808112458' ) ).

    "Enumerated type
    TYPES: BEGIN OF ENUM enum_t,
             enum1,
             enum2,
           END OF ENUM enum_t.
    DATA(dobj_enum) = enum2.
    DATA(other_enum) = enum1.

    DATA(tdo_enum) = CAST cl_abap_enumdescr( cl_abap_typedescr=>describe_by_data( enum2 ) ).
    "DATA(tdo_enum) = CAST cl_abap_enumdescr( cl_abap_typedescr=>describe_by_name( 'ENUM_T' ) ).

    DATA(type_category_enum) = tdo_enum->kind.
    DATA(relative_name_enum) = tdo_enum->get_relative_name( ).
    ... "Explore more options by positioning the cursor behind -> and choosing CTRL + Space
    DATA(base_kind_enum) = tdo_enum->base_type_kind.
    DATA(members_enum) = tdo_enum->members.
    DATA(applies_to_data_enum) = tdo_enum->applies_to_data( other_enum ).

    "------------------------------------- Structure -------------------------------------

    TYPES: BEGIN OF demo_struc_type,
             comp1 TYPE c LENGTH 3,
             comp2 TYPE i,
             comp3 TYPE string,
           END OF demo_struc_type.
    DATA demo_struc TYPE demo_struc_type.

    DATA(tdo_c) = cl_abap_typedescr=>describe_by_data( demo_struc ).
    "DATA(tdo_c) = cl_abap_typedescr=>describe_by_name( 'DEMO_STRUC_TYPE' ).

    "Cast to get more specific information
    DATA(tdo_struc) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( demo_struc ) ).
    "DATA(tdo_struc) = CAST cl_abap_structdescr( tdo_c ).

    DATA(type_category_struc) = tdo_struc->kind.
    DATA(relative_name_struc) = tdo_struc->get_relative_name( ).
    ... "Explore more options by positioning the cursor behind -> and choosing CTRL + Space
    DATA(type_of_struc) = tdo_struc->struct_kind.
    DATA(struc_comps) = tdo_struc->components.
    DATA(struc_comps_more_details) = tdo_struc->get_components( ).
    DATA(struc_has_include) = tdo_struc->has_include.
    DATA(struc_incl_view) = tdo_struc->get_included_view( ).
    DATA(applies_to_data_struc) = tdo_struc->applies_to_data( `some string` ).

    "------------------------------------- Internal table -------------------------------------

    TYPES tab_type TYPE SORTED TABLE OF demo_struc_type
       WITH UNIQUE KEY comp1
       WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS comp2 comp3.
    DATA itab TYPE tab_type.

    DATA(tdo_d) = cl_abap_typedescr=>describe_by_data( itab ).
    "DATA(tdo_d) = cl_abap_typedescr=>describe_by_name( 'TAB_TYPE' ).

    "Cast to get more specific information
    DATA(tdo_itab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab ) ).
    "DATA(tdo_itab) = CAST cl_abap_tabledescr( tdo_d ).

    DATA(type_category_itab) = tdo_itab->kind.
    DATA(relative_name_itab) = tdo_itab->get_relative_name( ).
    ... "Explore more options by positioning the cursor behind -> and choosing CTRL + Space
    DATA(table_kind_itab) = tdo_itab->table_kind.
    DATA(table_keys_itab) = tdo_itab->key.
    DATA(table_keys_more_details_itab) = tdo_itab->get_keys( ).
    DATA(table_has_unique_key_itab) = tdo_itab->has_unique_key.
    DATA(table_key_alias_itab) = tdo_itab->get_key_aliases( ).
    DATA(line_type_itab) = tdo_itab->get_table_line_type( ).
    DATA(table_component_info_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) ).
    DATA(table_components_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) )->components.
    DATA(table_comps_more_info_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) )->get_components( ).
    DATA(applies_to_data_itab) = tdo_itab->applies_to_data( VALUE tab_type( ) ).

    "------------------------------------- Data reference -------------------------------------

    TYPES dref_type TYPE REF TO i.
    DATA dref TYPE dref_type.
    dref = NEW #( 1 ).
    DATA(dref_b) = REF #( 2 ).

    DATA(tdo_e) = cl_abap_typedescr=>describe_by_data( dref ).
    "DATA(tdo_e) = cl_abap_typedescr=>describe_by_name( 'DREF_TYPE' ).

    DATA(tdo_dref) = CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data( dref ) ).
    DATA(referenced_type_dref) = tdo_dref->get_referenced_type( ).
    ... "Explore more options by positioning the cursor behind -> and choosing CTRL + Space

    "Using the describe_by_data_ref method
    DATA(tdo_dref_b) = cl_abap_typedescr=>describe_by_data_ref( dref ).

    "------------------------------------- Class -------------------------------------

    "Creating an object reference variable
    DATA(oref) = NEW cl_abap_dyn_prg( ).

    "Using the describe_by_object_ref method
    DATA(tdo_f) = cl_abap_typedescr=>describe_by_object_ref( oref ).
    "DATA(tdo_f) = cl_abap_typedescr=>describe_by_name( 'CL_ABAP_DYN_PRG' ).

    "Cast using cl_abap_classdescr to get class descriptions
    DATA(tdo_cl) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( oref ) ).
    "DATA(tdo_cl) = CAST cl_abap_classdescr( tdo_f ).

    DATA(class_kind_cl) = tdo_cl->class_kind.
    DATA(attributes_cl) = tdo_cl->attributes.
    DATA(interfaces_cl) = tdo_cl->interfaces.
    DATA(events_cl) = tdo_cl->events.
    DATA(methods_cl) = tdo_cl->methods.
    "Accessing method/parameter information
    LOOP AT methods_cl INTO DATA(meth_wa).
      DATA(meth_name) = meth_wa-name.
      LOOP AT meth_wa-parameters INTO DATA(param_wa).
        "Getting type description object of method parameters
        DATA(meth_param_type) = tdo_cl->get_method_parameter_type(
                      p_method_name    =  meth_name
                      p_parameter_name = param_wa-name ).
      ENDLOOP.
    ENDLOOP.
    DATA(super_class_cl) = tdo_cl->get_super_class_type( ).
    DATA(super_class_name_cl) = super_class_cl->absolute_name.
    DATA(is_instantiable_cl) = tdo_cl->is_instantiatable( ).
    ... "Explore more options by positioning the cursor behind -> and choosing CTRL + Space

    "------------------------------------- Interface -------------------------------------

    "This example only uses the describe_by_name method
    DATA(tdo_g) = cl_abap_typedescr=>describe_by_name( 'IF_OO_ADT_CLASSRUN' ).

    DATA(tdo_intf) = CAST cl_abap_intfdescr( cl_abap_typedescr=>describe_by_name( 'IF_OO_ADT_CLASSRUN' ) ).
    "DATA(tdo_intf) = CAST cl_abap_intfdescr( tdo_g ).

    DATA(attributes_intf) = tdo_intf->attributes.
    DATA(methods_intf) = tdo_intf->methods.
    ... "Explore more options by positioning the cursor behind -> and choosing CTRL + Space

  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Example: Exploring the RTTI Type Hierarchy

The following example explores the RTTI type hierarchy and demonstrates how to retrieve various pieces of type information using RTTI attributes and methods. You can create a demo class (adapt the class name if needed), copy and paste the code, run the class with F9 in ADT, and check the output in the console.

The example includes demo objects that are added to an internal table. This table is then looped over to retrieve type information for all objects. To retrieve a type description object, you have multiple options. You can use the static methods of the `cl_abap_typedescr` class, which is the root class of the RTTI hierarchy. These methods include: 
- `describe_by_data`: Returns an object reference in one of the classes `cl_abap_elemdescr`, `cl_abap_enumdescr`, `cl_abap_refdescr`, `cl_abap_structdescr`, or `cl_abap_tabledsecr`. 
- `describe_by_object_ref`: Returns the type that an object reference variable points to. 
- `describe_by_data_ref`: Returns the type that a data reference variable points to. 
- `describe_by_name`: Returns a type description object when providing the relative or absolute name of a type. 

Notes: 
- The `*_ref` methods return objects of the dynamic type. 
- In the bigger example of the demo class (the first `LOOP` statement), type names are not used, but rather objects. First, an attempt is made to get a type description object using the `describe_by_object_ref` method to obtain an instance of `cl_abap_objectdescr`. If this fails, it means it is an instance of `cl_abap_datadescr`, which is the next subclass in the hierarchy. It can be retrieved using the `describe_by_data` method. `describe_by_name` is used in the smaller example in the demo class (the second `LOOP` statement). 
- The `describe_by_data` method also works for references, including object/interface reference variables. In these cases, the returned object points to `cl_abap_refdescr`. The `get_referenced_type` method can then be used to obtain more details about the actual reference. 
- The example also demonstrates the dynamic creation of data objects using the retrieved type description objects and the `HANDLE` addition to the `CREATE DATA` statement. It also shows dynamic creations using the dynamic specification of the type and the absolute name. The latter is also possible with the `CREATE OBJECT` statement to create objects dynamically. In ABAP for Cloud Development, absolute names having the pattern `\TYPE=%_...` (an internal technical name that is available for [bound data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbound_data_type_glosry.htm)) cannot be used for the dynamic creation. 
- To visualize the retrieved information, many values are added to a string table. Note that this example is tailored to cover all subclasses of the RTTI hierarchy, but it does not explore all available options for information retrieval. 
- The example uses artifacts from the ABAP cheat sheet repository.


```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Data objects to work with in the example
    DATA itab_refs TYPE TABLE OF REF TO data.
    DATA str_tab TYPE string_table.
    DATA dyn_dobj TYPE REF TO data.
    DATA dyn_obj TYPE REF TO object.
    DATA tdo TYPE REF TO cl_abap_typedescr.

    "Data objects of different kinds based on which type information shall be retrieved
    "Elementary type
    DATA elem_dobj TYPE c LENGTH 4 VALUE 'ABAP'.

    "Enumerated type
    TYPES: BEGIN OF ENUM enum_t,
             enum1,
             enum2,
             enum3,
           END OF ENUM enum_t.
    DATA(dobj_enum) = enum2.

    "Structured types
    DATA(struct) = VALUE zdemo_abap_carr( carrid = 'XY' carrname = 'XY Airlines' ).
    "BDEF derived type (structure)
    DATA struct_rap TYPE STRUCTURE FOR CREATE zdemo_abap_rap_ro_m.

    "Internal table types
    "Standard table with standard table key
    DATA(string_table) = VALUE string_table( ( `AB` ) ( `AP` ) ).
    "Local structured type as basis for a sorted internal table that
    "includes primary and secondary table key specifiactions (including
    "an alias name)
    TYPES: BEGIN OF struc_type,
             a TYPE c LENGTH 3,
             b TYPE i,
             c TYPE decfloat34,
           END OF struc_type.
    TYPES tab_type TYPE SORTED TABLE OF struc_type
      WITH UNIQUE KEY a
      WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS b c .
    DATA(sorted_tab) = VALUE tab_type( ( a = 'aaa' ) ).

    "Reference variables
    "Data reference variable
    DATA(dref) = NEW i( 123 ).
    "Object reference variable
    DATA(oref) = NEW zcl_demo_abap_objects( ).
    "Interface reference variable
    DATA iref TYPE REF TO zdemo_abap_objects_interface.
    iref = CAST #( oref ).

    "Adding the previous (data) objects to an internal table which is
    "looped over to retrieve type information for all
    itab_refs = VALUE #( ( REF #( elem_dobj ) ) "elementary type (1)
                         ( REF #( dobj_enum ) ) "enumerated type (2)
                         ( REF #( struct ) ) "flat structure (3)
                         ( REF #( struct_rap ) ) "structure typed with BDEF derived type (4)
                         ( REF #( string_table ) ) "internal table, elementary line type (5)
                         ( REF #( sorted_tab ) ) "internal table, local line type (6)
                         ( REF #( dref ) ) "data reference variable (7)
                         ( REF #( oref ) ) "object reference variable (8)
                         ( REF #( iref ) ) "interface reference variable (9)
                      ).


    LOOP AT itab_refs INTO DATA(type).
      DATA(tabix) = sy-tabix.
      TRY.
          "The reference returned points to an object from the class CL_ABAP_CLASSDESCR
          tdo = cl_abap_typedescr=>describe_by_object_ref( type->* ).
        CATCH cx_sy_dyn_call_illegal_type.
          "The reference returned points to an object from the class CL_ABAP_DATADESCR
          tdo = cl_abap_typedescr=>describe_by_data( type->* ).
      ENDTRY.

      "----------------- Exploring general type information -----------------
      "At this stage, with using the static methods above, you already get general type
      "information such as the type kind or the abosulte name. Check the type description
      "object in the debugger for more attributes.
      "When performing a down cast to more specific classes, you can access special
      "methods of the type object and get more detailed information.

      "Getting the type kind
      "For the constant values of type abap_typekind, see cl_abap_typedescr. For example, 'h'
      "stands for internal table.
      DATA(type_kind) = tdo->type_kind.
      INSERT |{ tabix } Type kind: { type_kind }| INTO TABLE str_tab.

      "Type category
      "For the constant values of type abap_typecategory, see cl_abap_typedescr.
      "C (class), E (elementary), I (interface), R (Reference), S (structure), T (table)
      DATA(type_category) = tdo->kind.
      INSERT |{ tabix } Type category: { type_category }| INTO TABLE str_tab.

      "Absolute name (used later for dynamic (data) object creation)
      "Note: In ABAP for Cloud Development, absolute names having the pattern \TYPE=%_...
      "cannot be used to create (data) objects dynamically.
      DATA(absolute_name) = tdo->absolute_name.
      INSERT |{ tabix } Absolute name: { absolute_name }| INTO TABLE str_tab.

      "Relative name
      "Types that are implicitly defined (e.g. created using DATA) do not have a relative
      "type name. Explicitly defined types are, for example, standard ABAP types, Dictionary
      "types, classes and interfaces.
      DATA(relative_name) = tdo->get_relative_name( ).
      IF relative_name IS NOT INITIAL.
        INSERT |{ tabix } Relative name: { relative_name }| INTO TABLE str_tab.
      ENDIF.

      "Checking if it is a DDIC type
      DATA(is_ddic_type) = tdo->is_ddic_type( ).
      IF is_ddic_type IS NOT INITIAL.
        INSERT |{ tabix } Is DDIC type: "{ is_ddic_type }"| INTO TABLE str_tab.
      ENDIF.

      "----------------- Exploring more specific information by casting -----------------
      "For checking the type before performing the cast, you can use statements with
      "CASE TYPE OF and IS INSTANCE. The example demonstrates both options.

      CASE TYPE OF tdo.

        WHEN TYPE cl_abap_datadescr.
          INSERT |{ tabix } Is instance of cl_abap_datadescr| INTO TABLE str_tab.

          "-----------------------------------------------------------------------
          "----------------------- Elementary types ------------------------------
          "-----------------------------------------------------------------------
          IF tdo IS INSTANCE OF cl_abap_elemdescr.
            INSERT |{ tabix } Is instance of cl_abap_elemdescr| INTO TABLE str_tab.

            "Enumerated types
            IF tdo IS INSTANCE OF cl_abap_enumdescr.
              INSERT |{ tabix } Is instance of cl_abap_enumdescr| INTO TABLE str_tab.

              DATA(enum) = CAST cl_abap_enumdescr( tdo ).

              "Various type-specific information retrieval
              "Base type of enumerated type
              DATA(enum_base_type_kind) = enum->base_type_kind.
              INSERT |{ tabix } Base type: { enum_base_type_kind }| INTO TABLE str_tab.

              "Elements of the enumerated type
              DATA(enum_elements) = enum->members.
              INSERT |{ tabix } Elements:| &&
              | { REDUCE string( INIT str = `` FOR <l> IN enum_elements NEXT str = |{ str }{ COND #( WHEN str IS NOT INITIAL THEN ` / ` ) }| &&
              |{ <l>-name } ({ CONV i( <l>-value ) })| ) }| INTO TABLE str_tab.

              "Checking the type compatibility of the data object
              DATA(applies_enum1) = enum->applies_to_data( enum2 ).
              DATA(applies_enum2) = enum->applies_to_data( `nope` ).
              DATA(applies_enum3) = enum->applies_to_data_ref( REF #( enum3 ) ).
              DATA(applies_enum4) = enum->applies_to_data_ref( REF #( `nope` ) ).

              INSERT |{ tabix } Applies: 1) "{ applies_enum1 }" 2) "{ applies_enum2 }"| &&
              | 3) "{ applies_enum3 }" 4) "{ applies_enum4 }"| INTO TABLE str_tab.

              "Dynamically creating data objects based on the ...
              TRY.
                  "... absolute name
                  CREATE DATA dyn_dobj TYPE (absolute_name).
                  "Assigning the value to the dynamically created data object
                  dyn_dobj->* = type->*.

                  "... type description object
                  CREATE DATA dyn_dobj TYPE HANDLE enum.
                  dyn_dobj->* = type->*.
                  INSERT |{ tabix } Dynamic data objects created, assignments done| INTO TABLE str_tab.
                CATCH cx_root INTO DATA(err_enum).
                  INSERT |{ tabix } Dynamic data object creation error: { err_enum->get_text( ) }| INTO TABLE str_tab.
              ENDTRY.

              "Elementary types other than enumerated types
            ELSE.
              DATA(elem) = CAST cl_abap_elemdescr( tdo ).

              "Note: General information such as (output) length, decimals etc. especially
              "for elementary types is already available without the cast.

              "Internal length
              DATA(elem_internal_length) = elem->length.
              "Output length
              DATA(elem_output_length) = elem->output_length.
              INSERT |{ tabix } Internal length: "{ elem_internal_length }", | &&
              |output length: "{ elem_output_length }"| INTO TABLE str_tab.

              "Checking the type compatibility of the data object
              DATA(applies_elem1) = elem->applies_to_data( 'ciao' ).
              DATA(applies_elem2) = elem->applies_to_data( abap_true ).
              DATA(applies_elem3) = elem->applies_to_data_ref( REF #( 'abap' ) ).
              DATA(applies_elem4) = elem->applies_to_data_ref( REF #( `nope` ) ).

              INSERT |{ tabix } Applies: 1) "{ applies_elem1 }" 2) "{ applies_elem2 }"| &&
              | 3) "{ applies_elem3 }" 4) "{ applies_elem4 }"| INTO TABLE str_tab.

              "Dynamically creating data objects based on the ...
              TRY.
                  "... absolute name
                  CREATE DATA dyn_dobj TYPE (absolute_name).
                  "Assigning the value to the dynamically created data object
                  dyn_dobj->* = type->*.

                  "... type description object
                  CREATE DATA dyn_dobj TYPE HANDLE elem.
                  dyn_dobj->* = type->*.
                  INSERT |{ tabix } Dynamic data objects created, assignments done| INTO TABLE str_tab.
                CATCH cx_root INTO DATA(err_elem).
                  INSERT |{ tabix } Dynamic data object creation error: { err_elem->get_text( ) }| INTO TABLE str_tab.
              ENDTRY.
            ENDIF.

            "-----------------------------------------------------------------------
            "----------------------- Reference types ------------------------------
            "-----------------------------------------------------------------------
          ELSEIF tdo IS INSTANCE OF cl_abap_refdescr.
            INSERT |{ tabix } Is instance of cl_abap_refdescr| INTO TABLE str_tab.

            "Getting a reference to the type's type description object using the
            "describe_by_data_ref, which can be used for data reference variables.
            "Note that the dynamic type is evaluated.

            "The following statement retrieves a type description object using the describe_by_data_ref
            "method, which can be used for data reference variables. An object is returned that points
            "to an object in one of these classes: cl_abap_elemdescr, cl_abap_enumdescr, cl_abap_refdescr,
            "cl_abap_structdescr, cl_abap_tabledsecr.
            "The method call is for demonstration purposes. With the returned object, the information
            "retrieval can also be performed as above.
            DATA(tdo_dref) = cl_abap_typedescr=>describe_by_data_ref( type->* ).

            "Using the type description object retrieved above (describe_by_data) and casting
            DATA(data_ref) = CAST cl_abap_refdescr( tdo ).

            "Getting a reference to the type's type description object that is used to
            "type the reference.
            DATA(dref_referenced_type) = data_ref->get_referenced_type( ).

            "Based on this, you can get further information of the dynamic type just like in the
            "other examples for the referenced type. Here, skipping further type evaluation.
            IF dref_referenced_type IS INSTANCE OF cl_abap_elemdescr.
              INSERT |{ tabix } The referenced type is an elementary type.| INTO TABLE str_tab.
            ELSE.
              INSERT |{ tabix } The referenced type is a type other than elementary.| INTO TABLE str_tab.
            ENDIF.

            "Checking the type compatibility
            DATA(applies_dref1) = data_ref->applies_to_data( REF #( 456 ) ).
            DATA(applies_dref2) = data_ref->applies_to_data( REF #( `hello` ) ).
            TYPES ref_int TYPE REF TO i.
            TYPES ref_str TYPE REF TO string.
            DATA(applies_dref3) = data_ref->applies_to_data_ref( NEW ref_int( ) ).
            DATA(applies_dref4) = data_ref->applies_to_data_ref( NEW ref_str( ) ).

            INSERT |{ tabix } Applies: 1) "{ applies_dref1 }" 2) "{ applies_dref2 }"| &&
            | 3) "{ applies_dref3 }" 4) "{ applies_dref4 }"| INTO TABLE str_tab.

            "Dynamically creating data objects based on the ...
            TRY.
                "... absolute name of the referenced data object
                DATA(absolute_name_ref) = dref_referenced_type->absolute_name.
                CREATE DATA dyn_dobj TYPE REF TO (absolute_name_ref).
                "Assigning the value to the dynamically created data object
                dyn_dobj->* = type->*.

                "... type description object
                CREATE DATA dyn_dobj TYPE HANDLE data_ref.
                dyn_dobj->* = type->*.
                INSERT |{ tabix } Dynamic data objects created, assignments done| INTO TABLE str_tab.
              CATCH cx_root INTO DATA(err_ref).
                INSERT |{ tabix } Dynamic data object creation error: { err_ref->get_text( ) }| INTO TABLE str_tab.
            ENDTRY.

            "Complex types
          ELSEIF tdo IS INSTANCE OF cl_abap_complexdescr.
            INSERT |{ tabix } Is instance of cl_abap_complexdescr| INTO TABLE str_tab.

            "-----------------------------------------------------------------------
            "----------------------- Structured types ------------------------------
            "-----------------------------------------------------------------------
            IF tdo IS INSTANCE OF cl_abap_structdescr.
              INSERT |{ tabix } Is instance of cl_abap_structdescr| INTO TABLE str_tab.

              DATA(struc) = CAST cl_abap_structdescr( tdo ).

              "Structure kind
              "For the constant values, see abap_structkind cl_abap_structdescr
              "For the constant values of type abap_structkind, see cl_abap_structdescr. For example, 'F'
              "stands for a flat structure.
              DATA(struc_kind) = struc->struct_kind.
              INSERT |{ tabix } Structure kind: { struc_kind }| INTO TABLE str_tab.

              "Structure components
              "The following attribute returns a table with component information, such as
              "the component names and type kinds.
              DATA(struc_components) = struc->components.
              INSERT |{ tabix } Components 1: | &&
              |{ REDUCE string( INIT str = `` FOR <comp1> IN struc_components NEXT str = |{ str }| &&
              |{ COND #( WHEN str IS NOT INITIAL THEN ` / ` ) }{ <comp1>-name } ({ <comp1>-type_kind })| ) }| INTO TABLE str_tab.

              "Structure components (more details)
              "The following method also returns a table with component information. In this case,
              "type description objects of each component and the component names are returned, which can
              "be further evaluated.
              DATA(struc_components_tab) = struc->get_components( ).
              INSERT |{ tabix } Components 2: | &&
              |{ REDUCE string( INIT str = `` FOR <comp2> IN struc_components_tab NEXT str = |{ str }| &&
              |{ COND #( WHEN str IS NOT INITIAL THEN ` / ` ) }{ <comp2>-name } ({ <comp2>-type->type_kind })| ) }| INTO TABLE str_tab.

              "Checking if the structure has includes
              DATA(struc_has_include) = struc->has_include.
              INSERT |{ tabix } Has include: "{ struc_has_include }"| INTO TABLE str_tab.
              IF struc_has_include = abap_true.
                "Returning the included view
                "Check the class documentation for more information
                DATA(struc_incl_view) = struc->get_included_view( ).
                INSERT |{ tabix } Included view: | &&
                |{ REDUCE string( INIT str = `` FOR <comp3> IN struc_incl_view NEXT str = |{ str }| &&
                |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ <comp3>-name }| ) }| INTO TABLE str_tab.

                "Returning component names of all components and substructures in included
                "structures that contain included structures
                DATA(struc_all_incl) = struc->get_symbols( ).
                INSERT |{ tabix } Included view: | &&
                |{ REDUCE string( INIT str = `` FOR <comp4> IN struc_all_incl NEXT str = |{ str }| &&
                |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ <comp4>-name }| ) }| INTO TABLE str_tab.
              ENDIF.

              "Checking the type compatibility of the data object
              DATA struct_test TYPE zdemo_abap_carr.
              DATA struct_rap_test TYPE STRUCTURE FOR CREATE zdemo_abap_rap_ro_m.
              DATA(applies_struc1) = struc->applies_to_data( struct_test ).
              DATA(applies_struc2) = struc->applies_to_data( struct_rap_test ).
              DATA(applies_struc3) = struc->applies_to_data_ref( REF #( struct_test ) ).
              DATA(applies_struc4) = struc->applies_to_data_ref( REF #( struct_rap_test ) ).

              INSERT |{ tabix } Applies: 1) "{ applies_struc1 }" 2) "{ applies_struc2 }" | &&
              |3) "{ applies_struc3 }" 4) "{ applies_struc4 }"| INTO TABLE str_tab.

              "Dynamically creating data objects based on the ...
              TRY.
                  "... absolute name
                  CREATE DATA dyn_dobj TYPE (absolute_name).
                  "Assigning the value to the dynamically created data object
                  dyn_dobj->* = type->*.

                  "... type description object
                  CREATE DATA dyn_dobj TYPE HANDLE struc.
                  dyn_dobj->* = type->*.
                  INSERT |{ tabix } Dynamic data objects created, assignments done| INTO TABLE str_tab.
                CATCH cx_root INTO DATA(err_struc).
                  INSERT |{ tabix } Dynamic data object creation error: { err_struc->get_text( ) }| INTO TABLE str_tab.
              ENDTRY.

              "-----------------------------------------------------------------------
              "----------------------- Table types ------------------------------
              "-----------------------------------------------------------------------
            ELSEIF tdo IS INSTANCE OF cl_abap_tabledescr.
              INSERT |{ tabix } Is instance of cl_abap_tabledescr| INTO TABLE str_tab.

              DATA(tab) = CAST cl_abap_tabledescr( tdo ).

              "Getting the table kind
              "For the constant values of type abap_tablekind, see cl_abap_tabledescr. For example, 'S'
              "stands for a standard table.
              DATA(tab_table_kind) = tab->table_kind.
              INSERT |{ tabix } Table kind: { tab_table_kind }| INTO TABLE str_tab.

              "Checking if the table has a unique key
              DATA(tab_has_unique_key) = tab->has_unique_key.
              INSERT |{ tabix } Has a unique key: "{ tab_has_unique_key }" | &&
              |{ COND #( WHEN tab_has_unique_key IS INITIAL THEN `(no unique key)` ) }| INTO TABLE str_tab.

              "Returning a table with the names of internal table keys
              DATA(tab_table_key) = tab->key.
              INSERT |{ tabix } Table keys: { REDUCE string( INIT str = `` FOR <key1> IN tab_table_key NEXT str = |{ str }| &&
              |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ <key1>-name }| ) }| INTO TABLE str_tab.

              "Returning a table with a description of all table keys, e.g. all components of a key,
              "key kind (U, unique, in the example case), information whether the key is the primary
              "key etc. For the constant values, see the cl_abap_tabledescr class.
              DATA(tab_keys) = tab->get_keys( ).

              INSERT |{ tabix } Table keys: { REDUCE string( INIT str = `` FOR <key2> IN tab_keys NEXT str = |{ str }| &&
              |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ REDUCE string( INIT str2 = `` FOR <key3> IN <key2>-components NEXT str2 = |{ str2 }| &&
              |{ COND #( WHEN str2 IS NOT INITIAL THEN `/` ) }{ <key3>-name }| ) } (is primary: "{ <key2>-is_primary }", |  &&
              |is unique: "{ <key2>-is_unique }", key kind: "{ <key2>-key_kind }", access kind: "{ <key2>-access_kind }")| ) }| INTO TABLE str_tab.

              DATA(tab_keys_aliases) = tab->get_key_aliases( ).
              IF tab_keys_aliases IS NOT INITIAL.
                INSERT |{ tabix } Table key aliases: { REDUCE string( INIT str = `` FOR <key4> IN tab_keys_aliases NEXT str = |{ str }| &&
                |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ <key4>-name } (table key) -> { <key4>-alias } (alias)| ) }|  INTO TABLE str_tab.
              ENDIF.

              "If you want to get information about the line type, e.g. finding out about the component
              "names, another cast is required. First, getting a reference to the type description object
              "for the structured type.
              DATA(tab_line_type) = tab->get_table_line_type( ).

              "Then, performing a cast to access the component information as shown above.
              "Note that the line type can also be of types other than structured line types.
              IF tab_line_type IS INSTANCE OF cl_abap_structdescr.
                DATA(tab_line_info) = CAST cl_abap_structdescr( tab_line_type ).
                "See more options for structures above.
                DATA(tab_comps) = tab_line_info->components.
                INSERT |{ tabix } Table components: { REDUCE string( INIT str = `` FOR <comp> IN tab_comps NEXT str = |{ str }| &&
                |{ COND #( WHEN str IS NOT INITIAL THEN ` / ` ) }{ <comp>-name } ({ <comp>-type_kind })| ) }| INTO TABLE str_tab.

              ELSEIF tab_line_type IS INSTANCE OF cl_abap_elemdescr.
                DATA(tab_elem_line_type) = CAST cl_abap_elemdescr( tab_line_type ).
                DATA(tab_elem_line_type_kind) = tab_elem_line_type->type_kind.
                INSERT |{ tabix } Elementary line type, type kind: { tab_elem_line_type_kind }| INTO TABLE str_tab.
              ENDIF.

              "Checking the type compatibility of the data object
              DATA tab_test1 TYPE string_table.
              DATA tab_test2 TYPE tab_type.

              DATA(applies_tab1) = tab->applies_to_data( tab_test1 ).
              DATA(applies_tab2) = tab->applies_to_data( tab_test2 ).
              DATA(applies_tab3) = tab->applies_to_data_ref( REF #( tab_test1 ) ).
              DATA(applies_tab4) = tab->applies_to_data_ref( REF #( tab_test2 ) ).

              INSERT |{ tabix } Applies: 1) "{ applies_tab1 }" 2) "{ applies_tab2 }" | &&
              |3) "{ applies_tab3 }" 4) "{ applies_tab4 }"| INTO TABLE str_tab.

              "Dynamically creating data objects based on the ...
              TRY.
                  "... absolute name
                  CREATE DATA dyn_dobj TYPE (absolute_name).
                  dyn_dobj->* = type->*.

                  "... type description object
                  CREATE DATA dyn_dobj TYPE HANDLE tab.
                  dyn_dobj->* = type->*.
                  INSERT |{ tabix } Dynamic data objects created, assignments done| INTO TABLE str_tab.
                CATCH cx_root INTO DATA(err_tab).
                  INSERT |{ tabix } Dynamic data object creation error: { err_tab->get_text( ) }| INTO TABLE str_tab.
              ENDTRY.
            ENDIF.
          ENDIF.

          "Object types
        WHEN TYPE cl_abap_objectdescr.
          INSERT |{ tabix } Is instance of cl_abap_objectdescr| INTO TABLE str_tab.

          "In this example, reference variables are used to retrieve type information of their dynamic type.
          "Here, and to find out about the dynamic type the reference refers to (i.e. class or interface), a cast
          "with cl_abap_refdescr and calling the get_referenced_type method is used to also find out about the
          "instance of cl_abap_intfdescr. In this example, the dynamic type in 'type->*' is evaluated, which is
          "cl_abap_classdescr for both because the interface reference variable was assigned accordingly above.
          DATA(referenced_type) = CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data( type->* ) )->get_referenced_type( ).

          "-----------------------------------------------------------------------
          "----------------------- Class descriptions ------------------------------
          "-----------------------------------------------------------------------
          IF referenced_type IS INSTANCE OF cl_abap_classdescr.
            INSERT |{ tabix } Is instance of cl_abap_classdescr| INTO TABLE str_tab.

            DATA(obj_ref) = CAST cl_abap_classdescr( tdo ).

            "Getting the class kind
            "For the constant values of type abap_classkind, see cl_abap_classdescr.
            "Common, simple class (C), abstract class (A), final class (F)
            DATA(obj_ref_class_kind) = obj_ref->class_kind.

            "Getting class attributes
            "You can check the following table in the debugger. There is plenty of information available
            "such as type kind, constant, read only etc.
            "The example writes the names, the visibility and static or instance attribute (is_class = abap_true
            "means it is a static attribute) to the string table.
            DATA(obj_ref_attributes) = obj_ref->attributes.

            INSERT |{ tabix } Attributes: { REDUCE string( INIT str = `` FOR <attr> IN obj_ref_attributes NEXT str = |{ str }| &&
            |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ <attr>-name } (vis: "{ <attr>-visibility }", static: "{ <attr>-is_class }")| ) }| INTO TABLE str_tab.

            "Getting the interfaces implemented
            DATA(obj_ref_interfaces) = obj_ref->interfaces.
            INSERT |{ tabix } Interfaces: { REDUCE string( INIT str = `` FOR <intf> IN obj_ref_interfaces NEXT str = |{ str }| &&
            |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ <intf>-name }| ) }| INTO TABLE str_tab.

            "Getting information about the methods
            "You can check the following table in the debugger. There is plenty of information available
            "such as parameters, visibility, abstract/final, static/instance and more.
            "The example only writes the method names to the string table.
            DATA(obj_ref_methods) = obj_ref->methods.
            INSERT |{ tabix } Methods: { REDUCE string( INIT str = `` FOR <meth> IN obj_ref_methods NEXT str = |{ str }| &&
            |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ <meth>-name }| ) }| INTO TABLE str_tab.

            "Getting a reference to the type description object and the absolute name
            "of the superclass
            "In this example, it is the root class object OBJECT.
            DATA(obj_ref_super_class) = obj_ref->get_super_class_type( ).
            DATA(obj_ref_super_class_name) = obj_ref_super_class->absolute_name.
            INSERT |{ tabix } Super class: { obj_ref_super_class_name }| INTO TABLE str_tab.

            "Checking the type compatibility of the object
            DATA(oref_test1) = NEW zcl_demo_abap_objects( ).
            DATA(oref_test2) = NEW cl_system_uuid( ).

            DATA(applies_obj1) = obj_ref->applies_to( oref_test1 ).
            DATA(applies_obj2) = obj_ref->applies_to( oref_test2 ).
            DATA(applies_obj3) = obj_ref->applies_to_class( 'ZCL_DEMO_ABAP_OBJECTS' ).
            DATA(applies_obj4) = obj_ref->applies_to_class( 'CL_SYSTEM_UUID' ).

            INSERT |{ tabix } Applies: 1) "{ applies_obj1 }" 2) "{ applies_obj2 }" | &&
            |3) "{ applies_obj3 }" 4) "{ applies_obj4 }"| INTO TABLE str_tab.

            "Dynamically creating objects based on the absolute name
            TRY.
                CREATE OBJECT dyn_obj TYPE (absolute_name).
                INSERT |{ tabix } Dynamic object created| INTO TABLE str_tab.
              CATCH cx_sy_create_object_error INTO DATA(err_obj).
                INSERT |{ tabix } Dynamic object creation error: { err_obj->get_text( ) }| INTO TABLE str_tab.
            ENDTRY.

            "The following example shows dynamically accessing public class attributes using the
            "dynamically created object. The names and the attribute content are added to the string table.
            "In this example (using an ABAP cheat sheet class), all attributes are convertible to string.
            IF  absolute_name CS '\CLASS=ZCL_DEMO_ABAP_OBJECTS' AND err_obj IS INITIAL.
              INSERT |{ tabix } Dynamic attribute access: { REDUCE string( INIT str = `` FOR <m> IN obj_ref_attributes NEXT str = |{ str }| &&
              |{ COND #( WHEN str IS NOT INITIAL AND <m>-visibility = 'U' THEN ` / ` ) }| &&
              |{ COND #( WHEN <m>-visibility = 'U' THEN <m>-name && ` ("` && CONV string( dyn_obj->(<m>-name) ) && `")` ) }| ) }| INTO TABLE str_tab.
            ENDIF.

            "-----------------------------------------------------------------------
            "----------------------- Interface descriptions ------------------------------
            "-----------------------------------------------------------------------
          ELSEIF referenced_type IS INSTANCE OF cl_abap_intfdescr.
            INSERT |{ tabix } Is instance of cl_abap_intfdescr| INTO TABLE str_tab.

            "In the example, the checked reference variable points to the class
            "as the interface reference variable was assigned an instance of a class.
            "Therefore, the example here does not work with 'tdo' but with the type
            "description object 'referenced_type'. With 'referenced_type', the
            "interface-specific information can be accessed using a cast.
            DATA(intf) = CAST cl_abap_intfdescr( referenced_type ).

            "Getting the absolute name
            DATA(intf_abs_name) = intf->absolute_name.
            INSERT |{ tabix } Absolute name (via cl_abap_intfdescr): { intf_abs_name }| INTO TABLE str_tab.

            "Relative name
            DATA(intf_rel_name) = intf->get_relative_name( ).
            INSERT |{ tabix } Relative name (via cl_abap_intfdescr): { intf_rel_name }| INTO TABLE str_tab.

            "Type kind
            "For the constant values of type abap_typekind, see cl_abap_typedescr.
            "+ stands for the internal type interface.
            DATA(intf_type_kind) = intf->type_kind.
            INSERT |{ tabix } Type kind (via cl_abap_intfdescr): { intf_type_kind }| INTO TABLE str_tab.

            "Type category
            "For the constant values of type abap_typecategory, see cl_abap_typedescr.
            "I stands for interface.
            DATA(intf_type_category) = intf->kind.
            INSERT |{ tabix } Type category (via cl_abap_intfdescr): { intf_type_category }| INTO TABLE str_tab.

            "Interface type
            "For the constant values of type abap_intfkind, see cl_abap_intfdescr.
            "F stands for flat interface
            DATA(intf_type) = intf->intf_kind.
            INSERT |{ tabix } Interface type: { intf_type }| INTO TABLE str_tab.

            "Interface attributes
            DATA(intf_attributes) = intf->attributes.
            INSERT |{ tabix } Attributes: { REDUCE string( INIT str = `` FOR <attrintf> IN intf_attributes NEXT str = |{ str }| &&
            |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ <attrintf>-name } (vis: "{ <attrintf>-visibility }", | &&
            |static: "{ <attrintf>-is_class }")| ) }| INTO TABLE str_tab.

            "Interface methods
            "You can check the following table in the debugger. There is plenty of information available
            "such as parameters, visibility, abstract/final, static/instance, and more.
            "The example only writes the methods names to the string table.
            DATA(intf_methods) = intf->methods.
            INSERT |{ tabix } Methods: { REDUCE string( INIT str = `` FOR <methintf> IN intf_methods NEXT str = |{ str }| &&
            |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ <methintf>-name }| ) }| INTO TABLE str_tab.

            "Checking the type compatibility
            DATA(intf_test1) = NEW zcl_demo_abap_objects( ).
            DATA(intf_test2) = NEW cl_system_uuid( ).

            DATA(applies_intf1) = intf->applies_to( intf_test1 ).
            DATA(applies_intf2) = intf->applies_to( intf_test2 ).
            DATA(applies_intf3) = intf->applies_to_class( 'ZCL_DEMO_ABAP_OBJECTS' ).
            DATA(applies_intf4) = intf->applies_to_class( 'CL_SYSTEM_UUID' ).

            INSERT |{ tabix } Applies: 1) "{ applies_intf1 }" 2) "{ applies_intf2 }"| &&
            | 3) "{ applies_intf3 }" 4) "{ applies_intf4 }"| INTO TABLE str_tab.

            "Creating an interface reference variable dynamically
            TRY.
                CREATE DATA dyn_dobj TYPE REF TO (intf_abs_name).
                INSERT |{ tabix } Dynamic data object created| INTO TABLE str_tab.
              CATCH cx_sy_create_data_error INTO DATA(err_intf).
                INSERT |{ tabix } Dynamic data object creation error: { err_intf->get_text( ) }| INTO TABLE str_tab.
            ENDTRY.

            "The following example shows dynamically creating an object which is assigned to the
            "previously created interface reference variable. Artifacts of the ABAP cheat sheet repository
            "are used.
            IF intf_abs_name CS '\INTERFACE=ZDEMO_ABAP_OBJECTS_INTERFACE'
            AND absolute_name CS '\CLASS=ZCL_DEMO_ABAP_OBJECTS'
            AND err_intf IS INITIAL.
              TRY.
                  CREATE OBJECT dyn_dobj->* TYPE (absolute_name).
                  INSERT |{ tabix } Dynamic object created| INTO TABLE str_tab.
                CATCH cx_sy_create_object_error INTO err_obj.
                  INSERT |{ tabix } Dynamic object creation error: { err_obj->get_text( ) }| INTO TABLE str_tab.
              ENDTRY.
            ENDIF.
          ENDIF.
      ENDCASE.
      INSERT `-----------------------------------` INTO TABLE str_tab.
    ENDLOOP.
    out->write( str_tab ).

**********************************************************************

    "----------- Exploring the describe_by_name method -----------
    "The method returns a type description object when providing the relative or
    "absolute name of a type.
    "The following example explores the RTTI type hierarchy based on relative names
    "and using the describe_by_name method. Similar to the example above, an internal
    "table that is filled with local and global type names instead of data objects is
    "looped over. The information retrieval can be performed via the type description
    "object as above, but it is not implemented here.

    CLEAR str_tab.
    DATA tdo_from_type_name TYPE REF TO cl_abap_typedescr.

    "Data types of different kinds based on which type
    "information shall be retrieved
    "Elementary type
    TYPES packed TYPE p LENGTH 8 DECIMALS 2.

    "Enumerated type
    TYPES: BEGIN OF ENUM enum_type,
             enum_a,
             enum_b,
             enum_c,
           END OF ENUM enum_type.

    "Structured types
    TYPES: BEGIN OF flat_struc_type,
             a TYPE c LENGTH 3,
             b TYPE i,
             c TYPE decfloat34,
           END OF flat_struc_type.

    TYPES str_der_type TYPE STRUCTURE FOR CREATE zdemo_abap_rap_ro_m.

    "Internal table types
    TYPES int_tab_type TYPE TABLE OF i WITH EMPTY KEY.
    TYPES sorted_tab_type TYPE SORTED TABLE OF flat_struc_type 
      WITH UNIQUE KEY a 
      WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS b c.
    TYPES itab_der_type TYPE TABLE FOR UPDATE zdemo_abap_rap_ro_m.

    "Reference types
    TYPES int_dref_type TYPE REF TO i.
    TYPES gen_dref_type TYPE REF TO data.
    "Class and interface names are specified directly

    DATA(type_names) = VALUE string_table( ( `PACKED` )                       "Elementary type (1)
                                           ( `TIMESTAMPL` )                   "Elementary type, global DDIC type/data element (2)
                                           ( `ENUM_TYPE` )                    "Enumerated type (3)
                                           ( `FLAT_STRUC_TYPE` )              "Structured type, flat structure (4)
                                           ( `STR_DER_TYPE` )                 "Structured type, BDEF derived type (5)
                                           ( `INT_TAB_TYPE` )                 "Table type, elementary line type (6)
                                           ( `SORTED_TAB_TYPE` )              "Table type, structured line type (7)
                                           ( `ITAB_DER_TYPE` )                "Table type, BDEF derived type (8)
                                           ( `INT_DREF_TYPE` )                "Reference type (9)
                                           ( `GEN_DREF_TYPE` )                "Reference type, generic type (10)
                                           ( `CL_ABAP_TYPEDESCR` )            "Class name (11)
                                           ( `CL_ABAP_CORRESPONDING` )        "Class name (12)
                                           ( `IF_OO_ADT_CLASSRUN` )           "Interface name (13)
                                           ( `ZDEMO_ABAP_OBJECTS_INTERFACE` ) "Interface name (14)
                                         ).

    LOOP AT type_names INTO DATA(type_name).
      DATA(tabix_type_names) = sy-tabix.
      tdo_from_type_name = cl_abap_typedescr=>describe_by_name( type_name ).
      CASE TYPE OF tdo_from_type_name.
        WHEN TYPE cl_abap_datadescr.
          INSERT |{ tabix_type_names } Is instance of cl_abap_datadescr| INTO TABLE str_tab.
          CASE TYPE OF tdo_from_type_name.
            WHEN TYPE cl_abap_elemdescr.
              INSERT |{ tabix_type_names } Is instance of cl_abap_elemdescr| INTO TABLE str_tab.
              IF tdo_from_type_name IS INSTANCE OF cl_abap_enumdescr.
                INSERT |{ tabix_type_names } Is instance of cl_abap_enumdescr| INTO TABLE str_tab.
              ENDIF.
            WHEN TYPE cl_abap_complexdescr.
              INSERT |{ tabix_type_names } Is instance of cl_abap_complexdescr| INTO TABLE str_tab.
              CASE TYPE OF tdo_from_type_name.
                WHEN TYPE cl_abap_structdescr.
                  INSERT |{ tabix_type_names } Is instance of cl_abap_structdescr| INTO TABLE str_tab.
                WHEN TYPE cl_abap_tabledescr.
                  INSERT |{ tabix_type_names } Is instance of cl_abap_tabledescr| INTO TABLE str_tab.
              ENDCASE.
            WHEN TYPE cl_abap_refdescr.
              INSERT |{ tabix_type_names } Is instance of cl_abap_refdescr| INTO TABLE str_tab.
          ENDCASE.
        WHEN TYPE cl_abap_objectdescr.
          INSERT |{ tabix_type_names } Is instance of cl_abap_objectdescr| INTO TABLE str_tab.
          CASE TYPE OF tdo_from_type_name.
            WHEN TYPE cl_abap_classdescr.
              INSERT |{ tabix_type_names } Is instance of cl_abap_classdescr| INTO TABLE str_tab.
            WHEN TYPE cl_abap_intfdescr.
              INSERT |{ tabix_type_names } Is instance of cl_abap_intfdescr| INTO TABLE str_tab.
          ENDCASE.
      ENDCASE.
      INSERT `-----------------------------------` INTO TABLE str_tab.
    ENDLOOP.
    out->write( |\n*************************************************************\n\n| ).
    out->write( str_tab ).
  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Excursion: Inline Declaration, CAST Operator, Method Chaining

As shown in the example above, you can use [inline declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry"), the [`CAST`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_cast.htm) operator for casting, and [method chaining](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmethod_chaining_glosry.htm "Glossary Entry") to write more concise code and avoid declaring helper variables. However, also consider the code's debuggability, maintainability, and readability.


```abap
DATA some_struc TYPE zdemo_abap_carr.

"An example as follows ...
DATA(a) = cl_abap_typedescr=>describe_by_data( some_struc ).
DATA(b) = CAST cl_abap_structdescr( a ).
DATA(c) = b->components.

"... instead of:
DATA d TYPE REF TO cl_abap_typedescr.
DATA e TYPE REF TO cl_abap_structdescr.
DATA f TYPE abap_compdescr_tab.
d = cl_abap_typedescr=>describe_by_data( some_struc ).
e = CAST cl_abap_structdescr( d ).
f = e->components.

"The same in one statement
DATA(g) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( some_struc ) )->components.

ASSERT c = f.
ASSERT c = g.
ASSERT f = g.
```

#### Absolute Names

As mentioned earlier about type name specifications for statements such as `CREATE DATA` and `CREATE OBJECT`, and as shown in the previous example, in addition to character-like data objects for the type name (the [relative type name](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrelative_type_name_glosry.htm)) specified in the parentheses, you can also use [absolute type names](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabsolute_typename_glosry.htm).


> [!NOTE]  
> In [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm), absolute names having the pattern `\TYPE=%_...` (an internal technical name that is available for [bound data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbound_data_type_glosry.htm); bound data types do not have a relative name) cannot be used for the dynamic creation. 


```abap
"Local type to refer to
TYPES type4abs TYPE p LENGTH 8 DECIMALS 2.
"Data and object reference variables with generic types
DATA dref4abs TYPE REF TO data.
DATA oref4abs TYPE REF TO object.

"----------- Getting absolute names -----------
DATA(abs_name_type) = cl_abap_typedescr=>describe_by_name(
    'TYPE4ABS' )->absolute_name.
DATA(abs_name_cl) = cl_abap_typedescr=>describe_by_name(
    'ZCL_DEMO_ABAP_DYNAMIC_PROG' )->absolute_name.

"----------- Data references -----------
"Named data object holding the absolute name
CREATE DATA dref4abs TYPE (abs_name_type).

"Unnamed data object
CREATE DATA dref4abs TYPE ('\TYPE=STRING').

"----------- Object references -----------
"Named data object
CREATE OBJECT oref4abs TYPE (abs_name_cl).

"Unnamed data object
CREATE OBJECT oref4abs TYPE ('\CLASS=ZCL_DEMO_ABAP_DYNAMIC_PROG').

"----------- Using relative names -----------
CREATE DATA dref4abs TYPE ('TYPE4ABS').
CREATE OBJECT oref4abs TYPE ('ZCL_DEMO_ABAP_DYNAMIC_PROG').

"----------- Using bound data types -----------
DATA packed_dobj TYPE p LENGTH 8 DECIMALS 2.
abs_name_type = cl_abap_typedescr=>describe_by_data( 
  packed_dobj )->absolute_name.

"In ABAP for Cloud Development, an exception is raised.
TRY.
    CREATE DATA dref4abs TYPE (abs_name_type).
  CATCH cx_sy_create_data_error.
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Constants of Type Description Classes

When working with type description classes, constant values defined in the classes are frequently involved in various contexts. The following table provides an overview of the constants and their values.

The example below illustrates some of these constant values by evaluating the type kind of several different data objects.

<details>
  <summary>🟢 Click to expand for an overview of constants and their values</summary>
  <!-- -->

<br>


<table>
	<tr>
		<th>Class</th>
		<th>Constant</th>
		<th>Value</th>
	</tr>
	<tr>
		<td rowspan="48">cl_abap_typedescr</td>
		<td>cl_abap_typedescr=>false</td>
		<td></td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>kind_class</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>kind_elem</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>kind_enum</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>kind_intf</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>kind_ref</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>kind_struct</td>
		<td>S</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>kind_table</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>true</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_any</td>
		<td>~</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_any_structure</td>
		<td>@</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_bref</td>
		<td>j</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_char</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_class</td>
		<td>*</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_clike</td>
		<td>&</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_csequence</td>
		<td>?</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_data</td>
		<td>#</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_date</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_decfloat</td>
		<td>/</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_decfloat16</td>
		<td>a</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_decfloat34</td>
		<td>e</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_dref</td>
		<td>l</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_enum</td>
		<td>k</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_float</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_hex</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_int</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_int1</td>
		<td>b</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_int2</td>
		<td>s</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_int8</td>
		<td>8</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_intf</td>
		<td>+</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_iref</td>
		<td>m</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_num</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_numeric</td>
		<td>%</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_oref</td>
		<td>r</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_packed</td>
		<td>P</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_simple</td>
		<td>$</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_string</td>
		<td>g</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_struct1</td>
		<td>u</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_struct2</td>
		<td>v</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_table</td>
		<td>h</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_time</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_utclong</td>
		<td>p</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_w</td>
		<td>w</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_xsequence</td>
		<td>!</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typekind_xstring</td>
		<td>y</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typepropkind_dbmaxlen</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>typepropkind_hasclient</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_typedescr=>undefined</td>
		<td>-</td>
	</tr>
	<tr>
		<td rowspan="59">cl_abap_classdescr</td>
		<td>cl_abap_classdescr=>changing</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>classkind_abstract</td>
		<td>A</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>classkind_common</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>classkind_final</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>exporting</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>false</td>
		<td></td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>importing</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>kind_class</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>kind_elem</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>kind_enum</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>kind_intf</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>kind_ref</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>kind_struct</td>
		<td>S</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>kind_table</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>private</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>protected</td>
		<td>O</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>public</td>
		<td>U</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>receiving</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>returning</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>true</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_any</td>
		<td>~</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_any_structure</td>
		<td>@</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_bref</td>
		<td>j</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_char</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_class</td>
		<td>*</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_clike</td>
		<td>&</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_csequence</td>
		<td>?</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_data</td>
		<td>#</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_date</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_decfloat</td>
		<td>/</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_decfloat16</td>
		<td>a</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_decfloat34</td>
		<td>e</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_dref</td>
		<td>l</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_enum</td>
		<td>k</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_float</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_hex</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_int</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_int1</td>
		<td>b</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_int2</td>
		<td>s</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_int8</td>
		<td>8</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_intf</td>
		<td>+</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_iref</td>
		<td>m</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_num</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_numeric</td>
		<td>%</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_oref</td>
		<td>r</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_packed</td>
		<td>P</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_simple</td>
		<td>$</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_string</td>
		<td>g</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_struct1</td>
		<td>u</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_struct2</td>
		<td>v</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_table</td>
		<td>h</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_time</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_utclong</td>
		<td>p</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_w</td>
		<td>w</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_xsequence</td>
		<td>!</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typekind_xstring</td>
		<td>y</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typepropkind_dbmaxlen</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>typepropkind_hasclient</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_classdescr=>undefined</td>
		<td>-</td>
	</tr>
	<tr>
		<td rowspan="48">cl_abap_datadescr</td>
		<td>cl_abap_datadescr=>false</td>
		<td></td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>kind_class</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>kind_elem</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>kind_enum</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>kind_intf</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>kind_ref</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>kind_struct</td>
		<td>S</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>kind_table</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>true</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_any</td>
		<td>~</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_any_structure</td>
		<td>@</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_bref</td>
		<td>j</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_char</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_class</td>
		<td>*</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_clike</td>
		<td>&</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_csequence</td>
		<td>?</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_data</td>
		<td>#</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_date</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_decfloat</td>
		<td>/</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_decfloat16</td>
		<td>a</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_decfloat34</td>
		<td>e</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_dref</td>
		<td>l</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_enum</td>
		<td>k</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_float</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_hex</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_int</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_int1</td>
		<td>b</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_int2</td>
		<td>s</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_int8</td>
		<td>8</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_intf</td>
		<td>+</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_iref</td>
		<td>m</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_num</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_numeric</td>
		<td>%</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_oref</td>
		<td>r</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_packed</td>
		<td>P</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_simple</td>
		<td>$</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_string</td>
		<td>g</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_struct1</td>
		<td>u</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_struct2</td>
		<td>v</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_table</td>
		<td>h</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_time</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_utclong</td>
		<td>p</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_w</td>
		<td>w</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_xsequence</td>
		<td>!</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typekind_xstring</td>
		<td>y</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typepropkind_dbmaxlen</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>typepropkind_hasclient</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_datadescr=>undefined</td>
		<td>-</td>
	</tr>
	<tr>
		<td rowspan="53">cl_abap_elemdescr</td>
		<td>cl_abap_elemdescr=>false</td>
		<td></td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>kind_class</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>kind_elem</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>kind_enum</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>kind_intf</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>kind_ref</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>kind_struct</td>
		<td>S</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>kind_table</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>true</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_any</td>
		<td>~</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_any_structure</td>
		<td>@</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_bref</td>
		<td>j</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_char</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_class</td>
		<td>*</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_clike</td>
		<td>&</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_csequence</td>
		<td>?</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_data</td>
		<td>#</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_date</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_decfloat</td>
		<td>/</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_decfloat16</td>
		<td>a</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_decfloat34</td>
		<td>e</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_dref</td>
		<td>l</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_enum</td>
		<td>k</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_float</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_hex</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_int</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_int1</td>
		<td>b</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_int2</td>
		<td>s</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_int8</td>
		<td>8</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_intf</td>
		<td>+</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_iref</td>
		<td>m</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_num</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_numeric</td>
		<td>%</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_oref</td>
		<td>r</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_packed</td>
		<td>P</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_simple</td>
		<td>$</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_string</td>
		<td>g</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_struct1</td>
		<td>u</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_struct2</td>
		<td>v</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_table</td>
		<td>h</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_time</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_utclong</td>
		<td>p</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_w</td>
		<td>w</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_xsequence</td>
		<td>!</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typekind_xstring</td>
		<td>y</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typepropkind_dbmaxlen</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>typepropkind_hasclient</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>type_c_max_length</td>
		<td>262143</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>type_n_max_length</td>
		<td>262143</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>type_p_max_decimals</td>
		<td>14</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>type_p_max_length</td>
		<td>16</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>type_x_max_length</td>
		<td>524287</td>
	</tr>
	<tr>
		<td>cl_abap_elemdescr=>undefined</td>
		<td>-</td>
	</tr>
	<tr>
		<td rowspan="53">cl_abap_enumdescr</td>
		<td>cl_abap_enumdescr=>false</td>
		<td></td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>kind_class</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>kind_elem</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>kind_enum</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>kind_intf</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>kind_ref</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>kind_struct</td>
		<td>S</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>kind_table</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>true</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_any</td>
		<td>~</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_any_structure</td>
		<td>@</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_bref</td>
		<td>j</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_char</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_class</td>
		<td>*</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_clike</td>
		<td>&</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_csequence</td>
		<td>?</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_data</td>
		<td>#</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_date</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_decfloat</td>
		<td>/</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_decfloat16</td>
		<td>a</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_decfloat34</td>
		<td>e</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_dref</td>
		<td>l</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_enum</td>
		<td>k</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_float</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_hex</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_int</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_int1</td>
		<td>b</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_int2</td>
		<td>s</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_int8</td>
		<td>8</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_intf</td>
		<td>+</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_iref</td>
		<td>m</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_num</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_numeric</td>
		<td>%</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_oref</td>
		<td>r</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_packed</td>
		<td>P</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_simple</td>
		<td>$</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_string</td>
		<td>g</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_struct1</td>
		<td>u</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_struct2</td>
		<td>v</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_table</td>
		<td>h</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_time</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_utclong</td>
		<td>p</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_w</td>
		<td>w</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_xsequence</td>
		<td>!</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typekind_xstring</td>
		<td>y</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typepropkind_dbmaxlen</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>typepropkind_hasclient</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>type_c_max_length</td>
		<td>262143</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>type_n_max_length</td>
		<td>262143</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>type_p_max_decimals</td>
		<td>14</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>type_p_max_length</td>
		<td>16</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>type_x_max_length</td>
		<td>524287</td>
	</tr>
	<tr>
		<td>cl_abap_enumdescr=>undefined</td>
		<td>-</td>
	</tr>
	<tr>
		<td rowspan="58">cl_abap_intfdescr</td>
		<td>cl_abap_intfdescr=>changing</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>exporting</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>false</td>
		<td></td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>importing</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>intfkind_flatt</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>intfkind_nested</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>kind_class</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>kind_elem</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>kind_enum</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>kind_intf</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>kind_ref</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>kind_struct</td>
		<td>S</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>kind_table</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>private</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>protected</td>
		<td>O</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>public</td>
		<td>U</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>receiving</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>returning</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>true</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_any</td>
		<td>~</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_any_structure</td>
		<td>@</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_bref</td>
		<td>j</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_char</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_class</td>
		<td>*</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_clike</td>
		<td>&</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_csequence</td>
		<td>?</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_data</td>
		<td>#</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_date</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_decfloat</td>
		<td>/</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_decfloat16</td>
		<td>a</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_decfloat34</td>
		<td>e</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_dref</td>
		<td>l</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_enum</td>
		<td>k</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_float</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_hex</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_int</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_int1</td>
		<td>b</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_int2</td>
		<td>s</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_int8</td>
		<td>8</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_intf</td>
		<td>+</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_iref</td>
		<td>m</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_num</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_numeric</td>
		<td>%</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_oref</td>
		<td>r</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_packed</td>
		<td>P</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_simple</td>
		<td>$</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_string</td>
		<td>g</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_struct1</td>
		<td>u</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_struct2</td>
		<td>v</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_table</td>
		<td>h</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_time</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_utclong</td>
		<td>p</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_w</td>
		<td>w</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_xsequence</td>
		<td>!</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typekind_xstring</td>
		<td>y</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typepropkind_dbmaxlen</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>typepropkind_hasclient</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_intfdescr=>undefined</td>
		<td>-</td>
	</tr>
	<tr>
		<td rowspan="56">cl_abap_objectdescr</td>
		<td>cl_abap_objectdescr=>changing</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>exporting</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>false</td>
		<td></td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>importing</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>kind_class</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>kind_elem</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>kind_enum</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>kind_intf</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>kind_ref</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>kind_struct</td>
		<td>S</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>kind_table</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>private</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>protected</td>
		<td>O</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>public</td>
		<td>U</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>receiving</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>returning</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>true</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_any</td>
		<td>~</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_any_structure</td>
		<td>@</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_bref</td>
		<td>j</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_char</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_class</td>
		<td>*</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_clike</td>
		<td>&</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_csequence</td>
		<td>?</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_data</td>
		<td>#</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_date</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_decfloat</td>
		<td>/</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_decfloat16</td>
		<td>a</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_decfloat34</td>
		<td>e</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_dref</td>
		<td>l</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_enum</td>
		<td>k</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_float</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_hex</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_int</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_int1</td>
		<td>b</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_int2</td>
		<td>s</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_int8</td>
		<td>8</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_intf</td>
		<td>+</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_iref</td>
		<td>m</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_num</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_numeric</td>
		<td>%</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_oref</td>
		<td>r</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_packed</td>
		<td>P</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_simple</td>
		<td>$</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_string</td>
		<td>g</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_struct1</td>
		<td>u</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_struct2</td>
		<td>v</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_table</td>
		<td>h</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_time</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_utclong</td>
		<td>p</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_w</td>
		<td>w</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_xsequence</td>
		<td>!</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typekind_xstring</td>
		<td>y</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typepropkind_dbmaxlen</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>typepropkind_hasclient</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_objectdescr=>undefined</td>
		<td>-</td>
	</tr>
	<tr>
		<td rowspan="48">cl_abap_refdescr</td>
		<td>cl_abap_refdescr=>false</td>
		<td></td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>kind_class</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>kind_elem</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>kind_enum</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>kind_intf</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>kind_ref</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>kind_struct</td>
		<td>S</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>kind_table</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>true</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_any</td>
		<td>~</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_any_structure</td>
		<td>@</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_bref</td>
		<td>j</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_char</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_class</td>
		<td>*</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_clike</td>
		<td>&</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_csequence</td>
		<td>?</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_data</td>
		<td>#</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_date</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_decfloat</td>
		<td>/</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_decfloat16</td>
		<td>a</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_decfloat34</td>
		<td>e</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_dref</td>
		<td>l</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_enum</td>
		<td>k</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_float</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_hex</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_int</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_int1</td>
		<td>b</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_int2</td>
		<td>s</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_int8</td>
		<td>8</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_intf</td>
		<td>+</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_iref</td>
		<td>m</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_num</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_numeric</td>
		<td>%</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_oref</td>
		<td>r</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_packed</td>
		<td>P</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_simple</td>
		<td>$</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_string</td>
		<td>g</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_struct1</td>
		<td>u</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_struct2</td>
		<td>v</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_table</td>
		<td>h</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_time</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_utclong</td>
		<td>p</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_w</td>
		<td>w</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_xsequence</td>
		<td>!</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typekind_xstring</td>
		<td>y</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typepropkind_dbmaxlen</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>typepropkind_hasclient</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_refdescr=>undefined</td>
		<td>-</td>
	</tr>
	<tr>
		<td rowspan="51">cl_abap_structdescr</td>
		<td>cl_abap_structdescr=>false</td>
		<td></td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>kind_class</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>kind_elem</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>kind_enum</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>kind_intf</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>kind_ref</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>kind_struct</td>
		<td>S</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>kind_table</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>structkind_flat</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>structkind_mesh</td>
		<td>M</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>structkind_nested</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>true</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_any</td>
		<td>~</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_any_structure</td>
		<td>@</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_bref</td>
		<td>j</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_char</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_class</td>
		<td>*</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_clike</td>
		<td>&</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_csequence</td>
		<td>?</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_data</td>
		<td>#</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_date</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_decfloat</td>
		<td>/</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_decfloat16</td>
		<td>a</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_decfloat34</td>
		<td>e</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_dref</td>
		<td>l</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_enum</td>
		<td>k</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_float</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_hex</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_int</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_int1</td>
		<td>b</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_int2</td>
		<td>s</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_int8</td>
		<td>8</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_intf</td>
		<td>+</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_iref</td>
		<td>m</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_num</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_numeric</td>
		<td>%</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_oref</td>
		<td>r</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_packed</td>
		<td>P</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_simple</td>
		<td>$</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_string</td>
		<td>g</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_struct1</td>
		<td>u</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_struct2</td>
		<td>v</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_table</td>
		<td>h</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_time</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_utclong</td>
		<td>p</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_w</td>
		<td>w</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_xsequence</td>
		<td>!</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typekind_xstring</td>
		<td>y</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typepropkind_dbmaxlen</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>typepropkind_hasclient</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_structdescr=>undefined</td>
		<td>-</td>
	</tr>
	<tr>
		<td rowspan="60">cl_abap_tabledescr</td>
		<td>cl_abap_tabledescr=>false</td>
		<td></td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>further_scnd_keys_no</td>
		<td>26453703</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>further_scnd_keys_not_spec</td>
		<td>26453701</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>further_scnd_keys_yes</td>
		<td>26453702</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>keydefkind_default</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>keydefkind_empty</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>keydefkind_tableline</td>
		<td>L</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>keydefkind_user</td>
		<td>U</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>kind_class</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>kind_elem</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>kind_enum</td>
		<td>E</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>kind_intf</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>kind_ref</td>
		<td>R</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>kind_struct</td>
		<td>S</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>kind_table</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>tablekind_any</td>
		<td>A</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>tablekind_hashed</td>
		<td>H</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>tablekind_index</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>tablekind_sorted</td>
		<td>O</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>tablekind_std</td>
		<td>S</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>true</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_any</td>
		<td>~</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_any_structure</td>
		<td>@</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_bref</td>
		<td>j</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_char</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_class</td>
		<td>*</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_clike</td>
		<td>&</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_csequence</td>
		<td>?</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_data</td>
		<td>#</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_date</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_decfloat</td>
		<td>/</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_decfloat16</td>
		<td>a</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_decfloat34</td>
		<td>e</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_dref</td>
		<td>l</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_enum</td>
		<td>k</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_float</td>
		<td>F</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_hex</td>
		<td>X</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_int</td>
		<td>I</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_int1</td>
		<td>b</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_int2</td>
		<td>s</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_int8</td>
		<td>8</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_intf</td>
		<td>+</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_iref</td>
		<td>m</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_num</td>
		<td>N</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_numeric</td>
		<td>%</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_oref</td>
		<td>r</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_packed</td>
		<td>P</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_simple</td>
		<td>$</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_string</td>
		<td>g</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_struct1</td>
		<td>u</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_struct2</td>
		<td>v</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_table</td>
		<td>h</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_time</td>
		<td>T</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_utclong</td>
		<td>p</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_w</td>
		<td>w</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_xsequence</td>
		<td>!</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typekind_xstring</td>
		<td>y</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typepropkind_dbmaxlen</td>
		<td>D</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>typepropkind_hasclient</td>
		<td>C</td>
	</tr>
	<tr>
		<td>cl_abap_tabledescr=>undefined</td>
		<td>-</td>
	</tr>
</table>


</details>  

<br>

```abap
DATA ref_tab TYPE TABLE OF REF TO data.

DATA flag TYPE abap_boolean.
DATA num TYPE i.

DATA: BEGIN OF struct1,
        comp1 TYPE i,
        comp2 TYPE c LENGTH 5,
      END OF struct1.

DATA: BEGIN OF struct2,
        comp1 TYPE i,
        comp2 TYPE string,
      END OF struct2.

DATA itab TYPE TABLE OF struct WITH EMPTY KEY.

APPEND REF #( flag ) TO ref_tab.
APPEND REF #( num ) TO ref_tab.
APPEND REF #( struct1 ) TO ref_tab.
APPEND REF #( struct2 ) TO ref_tab.
APPEND REF #( itab ) TO ref_tab.

LOOP AT ref_tab INTO DATA(wa).

  DATA(a) = cl_abap_typedescr=>describe_by_data_ref( wa )->type_kind.
  DATA(b) = cl_abap_typedescr=>describe_by_data( wa->* )->type_kind.

  CASE sy-tabix.
    WHEN 1.
      ASSERT a = cl_abap_typedescr=>typekind_char.
      ASSERT b = cl_abap_typedescr=>typekind_char.

      ASSERT a = 'C'.
      ASSERT b = 'C'.
    WHEN 2.
      ASSERT a = cl_abap_typedescr=>typekind_int.
      ASSERT b = cl_abap_typedescr=>typekind_int.

      ASSERT a = 'I'.
      ASSERT b = 'I'.
    WHEN 3.
      "Flat structure
      ASSERT a = cl_abap_typedescr=>typekind_struct1.
      ASSERT b = cl_abap_typedescr=>typekind_struct1.

      ASSERT a = 'u'.
      ASSERT b = 'u'.
    WHEN 4.
      "Deep structure as a component is of type string
      ASSERT a = cl_abap_typedescr=>typekind_struct2.
      ASSERT b = cl_abap_typedescr=>typekind_struct2.

      ASSERT a = 'v'.
      ASSERT b = 'v'.
    WHEN 5.
      ASSERT a = cl_abap_typedescr=>typekind_table.
      ASSERT b = cl_abap_typedescr=>typekind_table.

      ASSERT a = 'h'.
      ASSERT b = 'h'.
  ENDCASE.
ENDLOOP.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamically Creating Data Types (RTTC) and Data Objects at Runtime Using Type Description Objects

#### Getting Type Description Objects

[Type description objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_object_glosry.htm):

- They are instances of type description classes. See the hierarchy of type description classes above. 
- As already shown in several code snippets above, you can use the static methods of these type description classes to create type description objects. 
- You can create them from existing types or create new types. 
- Attributes of type description objects determine the technical properties of the type. 
- Type description objects are basically available for all types.
- `CREATE DATA` and `ASSIGN` statements include the `HANDLE` addition after which you can specify references to type description objects so as to create or assign data objects dynamically.
  - The focus here is on creating anonymous data objects dynamically using `CREATE DATA` statements. Apart from using type description objects and the `HANDLE` addition, you can - as shown above - perform a dynamic creation of data objects using a type name specified dynamically (`CREATE DATA dref TYPE (some_type).`).
  - After the [`HANDLE`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcreate_data_handle.htm) addition, a reference variable of the static type of class `CL_ABAP_DATADESCR` or its subclasses that points to a type description object are expected.

The following examples show snippets that have already been covered in several sections above. 
They use the following methods to get type description objects: 
- `cl_abap_typedescr=>describe_by_name` (based on an existing type name)
- `cl_abap_typedescr=>describe_by_data` (based on an existing data object)
- `...=>get*` (getting type description objects for elementary and other types; other `get*` methods are shown further down) 

```abap
*&---------------------------------------------------------------------*
*& Getting a type description object from an existing data type name
*& describe_by_name method
*&---------------------------------------------------------------------*

"Elementary and structured data object, internal table
TYPES ty_elem TYPE c LENGTH 5.
TYPES ty_struct TYPE zdemo_abap_fli.
TYPES ty_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

DATA(tdo_from_name1) = cl_abap_typedescr=>describe_by_name( 'TY_ELEM' ).
DATA(type_name) = `TY_STRUCT`.
DATA(tdo_from_name2) = cl_abap_typedescr=>describe_by_name( type_name ).
"As shown above, using a cast to get more details.
DATA(tdo_from_name3) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_name( 'TY_TAB' ) ).

*&---------------------------------------------------------------------*
*& Getting a type description object from an existing data object
*& describe_by_data method
*&---------------------------------------------------------------------*

"Elementary and structured data object, internal table
DATA elem_dobj TYPE c LENGTH 5.
DATA struct_dobj TYPE zdemo_abap_fli.
DATA tab_dobj TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

DATA(tdo_from_dobj1) = cl_abap_typedescr=>describe_by_data( elem_dobj ).
DATA(tdo_from_dobj2) = cl_abap_typedescr=>describe_by_data( struct_dobj ).
"As shown above, using a cast to get more details.
DATA(tdo_from_dobj3) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( tab_dobj ) ).

*&---------------------------------------------------------------------*
*& Getting a type description object for built-in elementary types
*& cl_abap_elemdescr=>get* methods
*&---------------------------------------------------------------------*

"Conceptually, all elementary, built-in ABAP types already
"exist and can be accessed by the corresponding get_* methods.
"In ADT, click CTRL + space after cl_abap_elemdescr=>...
"to check out the options.
DATA(tdo_elem_string) = cl_abap_elemdescr=>get_string( ).
DATA(tdo_elem_d) = cl_abap_elemdescr=>get_d( ).
DATA(tdo_elem_t) = cl_abap_elemdescr=>get_t( ).
DATA(tdo_elem_utcl) = cl_abap_elemdescr=>get_utclong( ).
DATA(tdo_elem_dec16) = cl_abap_elemdescr=>get_decfloat16( ).
DATA(tdo_elem_dec34) = cl_abap_elemdescr=>get_decfloat34( ).
DATA(tdo_elem_f) = cl_abap_elemdescr=>get_f( ).
DATA(tdo_elem_xstr) = cl_abap_elemdescr=>get_xstring( ).
DATA(tdo_elem_i) = cl_abap_elemdescr=>get_i( ).
DATA(tdo_elem_int1) = cl_abap_elemdescr=>get_int1( ).
DATA(tdo_elem_int2) = cl_abap_elemdescr=>get_int2( ).
DATA(tdo_elem_int8) = cl_abap_elemdescr=>get_int8( ).

"For the length specification of type c and others, there is
"an importing parameter available.
DATA(tdo_elem_c_l5) = cl_abap_elemdescr=>get_c( 5 ).
DATA(tdo_elem_n_l4) = cl_abap_elemdescr=>get_n( 4 ).
DATA(tdo_elem_x_l10) = cl_abap_elemdescr=>get_x( 10 ).

"Type p with two parameters to be specified.
DATA(tdo_elem_p) = cl_abap_elemdescr=>get_p( p_length   = 3
                                             p_decimals = 2 ).

"Instead of calling get_i() and others having no importing
"parameters, you can also call the describe_by_name( ) method
"and pass the type names (I‚ STRING etc.) as arguments.
DATA(tdo_elem_i2) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'I' ) ).
DATA(tdo_elem_string2) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'STRING' ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Creating Elementary Types and Data Objects Dynamically

```abap
DATA dref_elem TYPE REF TO data.

*&---------------------------------------------------------------------*
*& Creating a data object with elementary type dynamically
*& Built-in ABAP type
*&---------------------------------------------------------------------*

"An elementary type and data object such as the following shall be
"created dynamically using a type description object.
TYPES ty_c3 TYPE c LENGTH 3.
DATA elem_dobj_1a TYPE ty_c3.
DATA elem_dobj_1b TYPE c LENGTH 3.

"Creating a reference type dynamically using a type description object
DATA(tdo_elem_1) = cl_abap_elemdescr=>get_c( 3 ).

"Creating a data reference variable dynamically using a type description object
CREATE DATA dref_elem TYPE HANDLE tdo_elem_1.

"Getting type information and checking type compatibility
DATA(tdo_for_dref_elem_1) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( dref_elem->* ) ).
DATA(applies_to_elem_1a) = tdo_for_dref_elem_1->applies_to_data( elem_dobj_1a ).
DATA(applies_to_elem_1b) = tdo_for_dref_elem_1->applies_to_data( elem_dobj_1b ).
ASSERT applies_to_elem_1a = abap_true.
ASSERT applies_to_elem_1b = abap_true.

*&---------------------------------------------------------------------*
*& get_* methods for built-in types
*&---------------------------------------------------------------------*

"Character-like types
TYPES c5 TYPE c LENGTH 5.
DATA(tdo_elem_c5) = cl_abap_elemdescr=>get_c( 5 ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_c5.

TYPES n4 TYPE n LENGTH 4.
DATA(tdo_elem_n4) = cl_abap_elemdescr=>get_n( 4 ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_n4.

TYPES str TYPE string.
DATA(tdo_elem_string) = cl_abap_elemdescr=>get_string( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_string.

"Numeric types
TYPES ty_i TYPE i.
DATA(tdo_elem_i) = cl_abap_elemdescr=>get_i( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_i.

TYPES ty_int1 TYPE int1.
DATA(tdo_elem_int1) = cl_abap_elemdescr=>get_int1( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_int1.

TYPES ty_int2 TYPE int2.
DATA(tdo_elem_int2) = cl_abap_elemdescr=>get_int2( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_int2.

TYPES ty_int8 TYPE int8.
DATA(tdo_elem_int8) = cl_abap_elemdescr=>get_int8( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_int8.

TYPES ty_dec16 TYPE decfloat16.
DATA(tdo_elem_dec16) = cl_abap_elemdescr=>get_decfloat16( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_dec16.

TYPES ty_dec34 TYPE decfloat34.
DATA(tdo_elem_dec34) = cl_abap_elemdescr=>get_decfloat34( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_dec34.

TYPES float TYPE f.
DATA(tdo_elem_f) = cl_abap_elemdescr=>get_f( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_f.

TYPES pl16d14 TYPE p LENGTH 16 DECIMALS 14.
DATA(tdo_elem_p) = cl_abap_elemdescr=>get_p( p_length   = 16
                                             p_decimals = 14 ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_p.

"Date, time, time stamp
TYPES ty_date TYPE d.
DATA(tdo_elem_d) = cl_abap_elemdescr=>get_d( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_d.

TYPES ty_time TYPE t.
DATA(tdo_elem_t) = cl_abap_elemdescr=>get_t( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_t.

TYPES ty_utcl TYPE utclong.
DATA(tdo_elem_utcl) = cl_abap_elemdescr=>get_utclong( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_utcl.

"Byte-like types
TYPES x10 TYPE x LENGTH 10.
DATA(tdo_elem_x_l10) = cl_abap_elemdescr=>get_x( 10 ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_x_l10.

TYPES xstr TYPE xstring.
DATA(tdo_elem_xstr) = cl_abap_elemdescr=>get_xstring( ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_xstr.

*&---------------------------------------------------------------------*
*& Creating a data object with elementary type dynamically
*& DDIC data element
*&---------------------------------------------------------------------*

"Note: The types int1 and int2 in the previous code snippet represent
"DDIC data elements, too.

"A reference type and variable such as the following shall be created
"using a type description object.
TYPES ty_ts TYPE timestampl.
DATA elem_dobj_2a TYPE ty_ts.
DATA elem_dobj_2b TYPE timestampl.

"Creating a reference type dynamically using a type description object
"Note: The 'describe_by_name' method returns a reference of type ref to
"cl_abap_typedescr. To use the type description object with the
"CREATE DATA ... TYPE HANDLE ... statement, a cast is required.
"The 'describe_by_name' method is available with cl_abap_typedescr,
"cl_abap_elemdescr, and others.
DATA(tdo_elem_2) = CAST cl_abap_datadescr( cl_abap_elemdescr=>describe_by_name( 'TIMESTAMPL' ) ).
"Alternative using cl_abap_typedescr
DATA(tdo_elem_w_cl_abap_typedescr) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'TIMESTAMPL' ) ).

"Creating a data reference variable dynamically using a type description object
CREATE DATA dref_elem TYPE HANDLE tdo_elem_2.

"Getting type information and checking type compatibility
DATA(tdo_for_dref_elem_2) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( dref_elem->* ) ).
DATA(applies_to_elem_2a) = tdo_for_dref_elem_2->applies_to_data( elem_dobj_2a ).
DATA(applies_to_elem_2b) = tdo_for_dref_elem_2->applies_to_data( elem_dobj_2b ).
ASSERT applies_to_elem_2a = abap_true.
ASSERT applies_to_elem_2b = abap_true.

*&---------------------------------------------------------------------*
*& Excursions: Further methods of class cl_abap_elemdescr
*&---------------------------------------------------------------------*

"The following examples cover a selection. For more information, refer
"to the class documentation.

"---- 'describe_by_data' method ----
"To get the underlying type's type description object.
DATA a_number TYPE i.
DATA(tdo_elem_descr_by_data) = cl_abap_elemdescr=>describe_by_data( a_number ).
"Note: To use it with the CREATE DATA ... TYPE HANDLE ... statement, a cast is
"required.
DATA(cast_tdo_elem_descr_by_data) = CAST cl_abap_datadescr( tdo_elem_descr_by_data ).
CREATE DATA dref_elem TYPE HANDLE cast_tdo_elem_descr_by_data.

"---- 'describe_by_name' method ----
"To get the type description object based on a type passed by relative or absolute name
DATA(tdo_elem_descr_by_name) = cl_abap_elemdescr=>describe_by_name( 'LAND1' ).
DATA(cast_tdo_elem_descr_by_name) = CAST cl_abap_datadescr( tdo_elem_descr_by_name ).
CREATE DATA dref_elem TYPE HANDLE cast_tdo_elem_descr_by_name.

"---- 'describe_by_data_ref' method ----
"To get the underlying type's type description object, passed as reference.
DATA data_ref TYPE REF TO data.
data_ref = NEW i( 123 ).
DATA(tdo_elem_descr_by_data_ref) = cl_abap_elemdescr=>describe_by_data_ref( data_ref ).
DATA(cast_tdo_elem_desc_by_data_ref) = CAST cl_abap_datadescr( tdo_elem_descr_by_data_ref ).
CREATE DATA dref_elem TYPE HANDLE cast_tdo_elem_desc_by_data_ref.

"---- 'get_by_kind' method ----
"To get a type description object by type kind.
DATA(tdo_elem_get_by_kind_string) = cl_abap_elemdescr=>get_by_kind( cl_abap_elemdescr=>typekind_string ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_get_by_kind_string.

DATA(tdo_elem_get_by_kind_c5) = cl_abap_elemdescr=>get_by_kind( p_type_kind = cl_abap_elemdescr=>typekind_char p_length = 5 ).
CREATE DATA dref_elem TYPE HANDLE tdo_elem_get_by_kind_c5.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Creating Structured Types and Data Objects Dynamically

```abap
DATA dref_struc TYPE REF TO data.

*&---------------------------------------------------------------------*
*& Creating a structured type and data object dynamically
*& Flat structure
*&---------------------------------------------------------------------*

"A structured type and data object such as the following shall be created
"dynamically using a type description object.
TYPES: BEGIN OF flat_struc_type,
         num    TYPE i,              "Built-in ABAP types
         packed TYPE p LENGTH 16 DECIMALS 14,
         text   TYPE c LENGTH 20,
         date   TYPE d,
         land   TYPE land1,          "DDIC data elements
         ts     TYPE timestampl,
       END OF flat_struc_type.
DATA struc_1 TYPE flat_struc_type.

"Creating a reference type dynamically using a type description object
"Using the 'get' method, you can create the type description object
"dynamically based on a component table. The component table is
"of type abap_component_tab. In this example, the component table
"is created inline.
DATA(tdo_struc_1) = cl_abap_structdescr=>get(
    VALUE #(
      ( name = 'NUM' type = cl_abap_elemdescr=>get_i( ) )
      ( name = 'PACKED' type = cl_abap_elemdescr=>get_p( p_length = 16 p_decimals = 14 ) )
      ( name = 'TEXT' type = cl_abap_elemdescr=>get_c( 20 ) )
      ( name = 'DATE' type = cl_abap_elemdescr=>get_d( ) )
      ( name = 'LAND' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'LAND1' ) ) )
      ( name = 'TS' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'TIMESTAMPL' ) ) )
       ) ).

"Creating a data reference variable dynamically using a type description object
CREATE DATA dref_struc TYPE HANDLE tdo_struc_1.

"Getting type information and checking type compatibility
DATA(tdo_for_dref_struc_1) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( dref_struc->* ) ).
DATA(applies_to_struc_1) = tdo_for_dref_struc_1->applies_to_data( struc_1 ).
ASSERT applies_to_struc_1 = abap_true.

*&---------------------------------------------------------------------*
*& Creating a structured type and data object dynamically
*& Deep structure
*&---------------------------------------------------------------------*

"A structured type and data object such as the following shall be created
"dynamically using a type description object.
TYPES: BEGIN OF deep_struc_type,
         num          TYPE i,              "Built-in ABAP types
         str          TYPE string,
         flight_struc TYPE zdemo_abap_fli, "Structure based on the line type of a database table
         string_tab   TYPE string_table,   "Global table type
         local_tab    TYPE TABLE OF flat_struc_type WITH EMPTY KEY, "Table type based on local structured type
       END OF deep_struc_type.
DATA struc_2 TYPE deep_struc_type.

"Creating a reference type dynamically using a type description object
DATA(tdo_struc_2) = cl_abap_structdescr=>get(
    VALUE #(
      ( name = 'NUM' type = cl_abap_elemdescr=>get_i( ) )
      ( name = 'STR' type = cl_abap_elemdescr=>get_string( ) )
      ( name = 'FLIGHT_STRUC' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_FLI' ) ) )
      ( name = 'STRING_TAB' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'STRING_TABLE' ) ) )
      ( name = 'LOCAL_TAB' type = CAST cl_abap_datadescr( cl_abap_tabledescr=>get(
                                            p_line_type  = CAST cl_abap_structdescr( cl_abap_tabledescr=>describe_by_name( 'FLAT_STRUC_TYPE' ) )
                                            p_table_kind = cl_abap_tabledescr=>tablekind_std
                                            p_unique     = cl_abap_typedescr=>false
                                            p_key_kind   = cl_abap_tabledescr=>keydefkind_empty ) ) ) ) ).

"Creating a data reference variable dynamically using a type description object
CREATE DATA dref_struc TYPE HANDLE tdo_struc_2.

"Getting type information and checking type compatibility
DATA(tdo_for_dref_struc_2) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( dref_struc->* ) ).
DATA(applies_to_struc_2) = tdo_for_dref_struc_2->applies_to_data( struc_2 ).
ASSERT applies_to_struc_2 = abap_true.

*&---------------------------------------------------------------------*
*& Excursions: Further methods of class cl_abap_structdescr
*&---------------------------------------------------------------------*

"The following examples cover a selection. For more information, refer
"to the class documentation.

"---- 'describe_by_data' method ----
"To get the underlying type's type description object.
DATA some_struct TYPE zdemo_abap_carr.
DATA(tdo_struc_descr_by_data) = cl_abap_structdescr=>describe_by_data( some_struct ).
"Note: To use it with the CREATE DATA ... TYPE HANDLE ... statement, a cast is
"required.
DATA(cast_tdo_struc_descr_by_data) = CAST cl_abap_datadescr( tdo_struc_descr_by_data ).
CREATE DATA dref_struc TYPE HANDLE cast_tdo_struc_descr_by_data.

"---- 'describe_by_name' method ----
"To get the type description object based on a type passed by relative or absolute name
DATA(tdo_struc_descr_by_name) = cl_abap_structdescr=>describe_by_name( 'ZDEMO_ABAP_CARR' ).
DATA(cast_tdo_struc_descr_by_name) = CAST cl_abap_datadescr( tdo_struc_descr_by_name ).
CREATE DATA dref_struc TYPE HANDLE cast_tdo_struc_descr_by_name.

"---- 'describe_by_data_ref' method ----
"To get the underlying type's type description object, passed as reference.
DATA data_ref TYPE REF TO data.
data_ref = NEW zdemo_abap_flsch( carrid = 'LH' ).
DATA(tdo_struc_descr_by_data_ref) = cl_abap_structdescr=>describe_by_data_ref( data_ref ).
DATA(cast_tdo_struc_desc_by_dref) = CAST cl_abap_datadescr( tdo_struc_descr_by_data_ref ).
CREATE DATA dref_struc TYPE HANDLE cast_tdo_struc_desc_by_dref.

"---- 'get_bdef_derived_type' method ----
"To get a type description object for a BDEF derived type.
DATA struc_der_type_create TYPE STRUCTURE FOR CREATE zdemo_abap_rap_ro_m.
DATA(tdo_struc_bdef_derived_type) = cl_abap_structdescr=>get_bdef_derived_type(
                                       p_entity   = 'ZDEMO_ABAP_RAP_RO_M'
                                       p_kind     = abp_bdef_derived_type-create ).

CREATE DATA dref_struc TYPE HANDLE tdo_struc_bdef_derived_type.

"Getting type information and checking type compatibility
DATA(tdo_for_dref_struc_der_type) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( dref_struc->* ) ).
DATA(applies_to_struc_der_type) = tdo_for_dref_struc_der_type->applies_to_data( struc_der_type_create ).
ASSERT applies_to_struc_der_type = abap_true.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Creating Table Types and Internal Tables Dynamically 

```abap
DATA dref_itab TYPE REF TO data.

*&---------------------------------------------------------------------*
*& Creating table type and internal table with elementary line type and
*& standard table key
*&---------------------------------------------------------------------*

"An internal table type such as the following shall be created dynamically
"using a type description object.
TYPES std_tab_type_std_key TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA itab_1 TYPE std_tab_type_std_key.

"Creating the type dynamically (creating a type description object)
"Here, the cl_abap_tabledescr=>get( ... ) method is used. Note that specifying
"the line type is mandatory, the rest is optional.
"Not specifying the other optional parameters means that the default values are
"used, for example, standard table is the default value for p_table_kind.
DATA(tdo_tab_1) = cl_abap_tabledescr=>get(
        p_line_type  = cl_abap_elemdescr=>get_string( ) ).

"Creating an internal table dynamically using a type description object
CREATE DATA dref_itab TYPE HANDLE tdo_tab_1.

"Getting type information and checking type compatibility
DATA(tdo_1) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( dref_itab->* ) ).
DATA(applies_1) = tdo_1->applies_to_data( itab_1 ).
ASSERT applies_1 = abap_true.

*&---------------------------------------------------------------------*
*& Creating table type and internal table with structured line type and
*& custom table keys
*&---------------------------------------------------------------------*

"Another internal table type for which more parameter specifications
"are needed. The following internal table type shall be created using
"a type description object.
"The structured type of a demo DDIC database table is used.
TYPES so_table_type TYPE SORTED TABLE OF zdemo_abap_flsch WITH UNIQUE KEY carrid connid.
DATA itab_2 TYPE so_table_type.

"Creating the type dynamically by creating a type description object
"The following example also demonstrates how comfortably constructor
"operators can be used at these positions.
DATA(tdo_tab_2) = cl_abap_tabledescr=>get(
        p_line_type  = CAST cl_abap_structdescr( cl_abap_tabledescr=>describe_by_name( 'ZDEMO_ABAP_FLSCH' ) )
        p_table_kind = cl_abap_tabledescr=>tablekind_sorted
        p_key        = VALUE #( ( name = 'CARRID' ) ( name = 'CONNID' ) )
        p_unique     = cl_abap_typedescr=>true
        p_key_kind   = cl_abap_tabledescr=>keydefkind_user ).

"Creating an internal table dynamically using a type description object
CREATE DATA dref_itab TYPE HANDLE tdo_tab_2.

"Getting type information and checking type compatibility
DATA(tdo_2) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( dref_itab->* ) ).
DATA(applies_2) = tdo_2->applies_to_data( itab_2 ).
ASSERT applies_2 = abap_true.

*&---------------------------------------------------------------------*
*& Creating table type and internal table containing custom components
*& and custom table key
*&---------------------------------------------------------------------*

"A table type and data object such as the following shall be created
"dynamically using a type description object.
TYPES: BEGIN OF struc_type,
         a TYPE c LENGTH 5,   "Built-in ABAP types
         b TYPE i,
         c TYPE string,
         d TYPE land1,    "DDIC data element
       END OF struc_type.
DATA itab_3 TYPE HASHED TABLE OF struc_type WITH UNIQUE KEY a.

"Creating the type dynamically by creating a type description object
DATA(tdo_tab_3) = cl_abap_tabledescr=>get(
        p_line_type  = cl_abap_structdescr=>get( VALUE #(
          ( name = 'A' type = cl_abap_elemdescr=>get_c( 5 ) )
          ( name = 'B' type = cl_abap_elemdescr=>get_i( ) )
          ( name = 'C' type = cl_abap_elemdescr=>get_string( ) )
          ( name = 'D' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'LAND1' ) ) ) ) )
        p_table_kind = cl_abap_tabledescr=>tablekind_hashed
        p_key        = VALUE #( ( name = 'A' )  )
        p_unique     = cl_abap_typedescr=>true ).

"Creating an internal table dynamically using a type description object
CREATE DATA dref_itab TYPE HANDLE tdo_tab_3.

"Getting type information and checking type compatibility
DATA(tdo_3a) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( dref_itab->* ) ).
DATA(line_type_3a) = CAST cl_abap_structdescr( tdo_3a->get_table_line_type( ) ).
DATA(keys_3a) = tdo_3a->get_keys( ).

DATA(tdo_3b) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab_3 ) ).
DATA(line_type_3b) = CAST cl_abap_structdescr( tdo_3b->get_table_line_type( ) ).
DATA(keys_3b) = tdo_3b->get_keys( ).

DATA(applies_3) = tdo_3a->applies_to_data( itab_3 ).
ASSERT applies_3 = abap_true.

*&---------------------------------------------------------------------*
*& Creating table type and internal table containing custom components
*& and custom table key (primary and secondary table key)
*&---------------------------------------------------------------------*

"A table type and data object such as the following shall be created
"dynamically using a type description object.
TYPES: BEGIN OF struc_type_2,
         e TYPE c LENGTH 5,       "Built-in ABAP types
         f TYPE i,
         g TYPE land1,            "DDIC data element
         h TYPE zdemo_abap_flsch, "DDIC database table
         i TYPE REF TO string,    "Reference type
       END OF struc_type_2.
DATA itab_4 TYPE SORTED TABLE OF struc_type_2
  WITH NON-UNIQUE KEY e
  WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS f.
"Note that the default name primary_key does not need to be specified explicitly.
"So, the following declaration corresponds to the previous one.
DATA itab_4b TYPE SORTED TABLE OF struc_type_2
  WITH NON-UNIQUE KEY primary_key COMPONENTS e
  WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS f.

"Creating the type dynamically by creating a type description object
"Here, the cl_abap_tabledescr=>get_with_keys( ... )  method is used. Check the class documentation
"for more details.
DATA(tdo_tab_4) = cl_abap_tabledescr=>get_with_keys(
                      p_line_type  = cl_abap_structdescr=>get( VALUE #(
                        ( name = 'E' type = cl_abap_elemdescr=>get_c( 5 ) )
                        ( name = 'F' type = cl_abap_elemdescr=>get_i( ) )
                        ( name = 'G' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'LAND1' ) ) )
                        ( name = 'H' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_FLSCH' ) ) )
                        ( name = 'I' type = CAST cl_abap_datadescr( cl_abap_refdescr=>get_by_name( 'STRING' ) ) )
                        ) )
                      p_keys = VALUE #(
                        ( name = VALUE #( )         "In case of the primary table key, a name must be provided here
                                                    "for an alias. This example table does not specify an alias name.
                          is_primary = abap_true
                          access_kind = cl_abap_tabledescr=>tablekind_sorted
                          is_unique = abap_false
                          key_kind = cl_abap_tabledescr=>keydefkind_user
                          components = VALUE #( ( name = 'E' ) ) )
                        ( name = 'SEC_KEY'
                          is_primary = abap_false
                          access_kind = cl_abap_tabledescr=>tablekind_sorted
                          is_unique = abap_false
                          key_kind = cl_abap_tabledescr=>keydefkind_user
                          components = VALUE #( ( name = 'F' ) ) ) ) ).

"Creating an internal table dynamically using a type description object
CREATE DATA dref_itab TYPE HANDLE tdo_tab_4.

"Getting type information and checking type compatibility
DATA(tdo_4a) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( dref_itab->* ) ).
DATA(line_type_4a) = CAST cl_abap_structdescr( tdo_4a->get_table_line_type( ) ).
DATA(keys_4a) = tdo_4a->get_keys( ).

DATA(tdo_4b) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab_4 ) ).
DATA(line_type_4b) = CAST cl_abap_structdescr( tdo_4b->get_table_line_type( ) ).
DATA(keys_4b) = tdo_4b->get_keys( ).

DATA(applies_4) = tdo_4a->applies_to_data( itab_4 ).
ASSERT applies_4 = abap_true.

*&---------------------------------------------------------------------*
*& Creating table type and internal table containing custom components
*& and custom table key (primary and secondary table key), including
*& alias names
*&---------------------------------------------------------------------*

"A internal table such as the following shall be
"created dynamically using a type description object.
DATA itab_5 TYPE HASHED TABLE OF struc_type
  WITH UNIQUE KEY primary_key ALIAS pk COMPONENTS a
  WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS b c.

"Creating the type dynamically by creating a type description object
DATA(tdo_tab_5) = cl_abap_tabledescr=>get_with_keys(
                      p_line_type  = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( 'STRUC_TYPE' ) )
                      p_keys = VALUE #(
                        ( name = 'PK' "Alias name for the primary table key.
                          is_primary = abap_true
                          access_kind = cl_abap_tabledescr=>tablekind_hashed
                          is_unique = abap_true
                          key_kind = cl_abap_tabledescr=>keydefkind_user
                          components = VALUE #( ( name = 'A' ) ) )
                        ( name = 'SEC_KEY'
                          is_primary = abap_false
                          access_kind = cl_abap_tabledescr=>tablekind_sorted
                          is_unique = abap_false
                          key_kind = cl_abap_tabledescr=>keydefkind_user
                          components = VALUE #( ( name = 'B' ) ( name = 'C' ) ) ) )
                       p_key_aliases = VALUE #( ( name = 'PRIMARY_KEY' alias = 'PK' ) "Specifying the default primary table key name PRIMARY_KEY
                                                ( name = 'SEC_KEY'     alias = 'SK' ) ) ).

"Creating an internal table dynamically using a type description object
CREATE DATA dref_itab TYPE HANDLE tdo_tab_5.

DATA(tdo_5a) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( dref_itab->* ) ).
DATA(line_type_5a) = CAST cl_abap_structdescr( tdo_5a->get_table_line_type( ) ).
DATA(keys_5a) = tdo_5a->get_keys( ).
DATA(key_aliases_5a) = tdo_5a->get_key_aliases( ).

DATA(tdo_5b) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab_5 ) ).
DATA(line_type_5b) = CAST cl_abap_structdescr( tdo_5b->get_table_line_type( ) ).
DATA(keys_5b) = tdo_5b->get_keys( ).
DATA(key_aliases_5b) = tdo_5b->get_key_aliases( ).

DATA(applies_5) = tdo_5a->applies_to_data( itab_5 ).
ASSERT applies_5 = abap_true.

*&---------------------------------------------------------------------*
*& Excursions: Further methods of class cl_abap_tabledescr
*&---------------------------------------------------------------------*

"The following examples cover a selection. For more information, refer
"to the class documentation.

"---- 'describe_by_data' method ----
"To get the underlying type's type description object.
DATA some_table TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
DATA(tdo_tab_descr_by_data) = cl_abap_typedescr=>describe_by_data( some_table ).
"Note: To use it with the CREATE DATA ... TYPE HANDLE ... statement, a cast is
"required.
DATA(cast_tdo_tab_descr_by_data) = CAST cl_abap_datadescr( tdo_tab_descr_by_data ).
CREATE DATA dref_itab TYPE HANDLE cast_tdo_tab_descr_by_data.

"---- 'describe_by_name' method ----
"To get the type description object based on a type passed by relative or absolute name
DATA(tdo_tab_descr_by_name) = cl_abap_tabledescr=>describe_by_name( 'ZDEMO_ABAP_CARR' ).
DATA(cast_tdo_tab_descr_by_name) = CAST cl_abap_datadescr( tdo_tab_descr_by_name ).
CREATE DATA dref_itab TYPE HANDLE cast_tdo_tab_descr_by_name.

"---- 'describe_by_data_ref' method ----
"To get the underlying type's type description object, passed as reference.
DATA data_ref TYPE REF TO data.
TYPES ty_tab type table of zdemo_abap_carr WITH EMPTY KEY.
data_ref = NEW ty_tab( ( carrid = 'LH' ) ( carrid = 'AA' ) ).
DATA(tdo_tab_descr_by_data_ref) = cl_abap_tabledescr=>describe_by_data_ref( data_ref ).
DATA(cast_tdo_tab_desc_by_dref) = CAST cl_abap_datadescr( tdo_tab_descr_by_data_ref ).
CREATE DATA dref_itab TYPE HANDLE cast_tdo_tab_desc_by_dref.

"---- 'get_bdef_derived_type' method ----
"To get a type description object for a BDEF derived type.
DATA tab_der_type_create TYPE TABLE FOR CREATE zdemo_abap_rap_ro_m.
DATA(tdo_tab_bdef_derived_type) = cl_abap_tabledescr=>get_bdef_derived_type(
                                       p_entity   = 'ZDEMO_ABAP_RAP_RO_M'
                                       p_kind     = abp_bdef_derived_type-create ).

CREATE DATA dref_itab TYPE HANDLE tdo_tab_bdef_derived_type.

"Getting type information and checking type compatibility
DATA(tdo_for_dref_tab_der_type) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( dref_itab->* ) ).
DATA(applies_to_tab_der_type) = tdo_for_dref_tab_der_type->applies_to_data( tab_der_type_create ).
ASSERT applies_to_tab_der_type = abap_true.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Creating Reference Types and Data Reference Variables Dynamically 

```abap
DATA dref_ref TYPE REF TO data.

*&---------------------------------------------------------------------*
*& Creating a data reference with elementary type (built-in ABAP type)
*&---------------------------------------------------------------------*

"A reference type and variable such as the following shall be created
"using a type description object.
TYPES ty_ref_str TYPE REF TO string.
DATA dref_1a TYPE ty_ref_str.
DATA dref_1b TYPE REF TO string.

"Creating a reference type dynamically using a type description object
DATA(tdo_ref_1) = cl_abap_refdescr=>get( cl_abap_elemdescr=>get_string( ) ).

"Creating a data reference variable dynamically using a type description object
CREATE DATA dref_ref TYPE HANDLE tdo_ref_1.

"Getting type information and checking type compatibility
DATA(tdo_for_dref_ref_1) = CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data( dref_ref->* ) ).
DATA(applies_to_1a) = tdo_for_dref_ref_1->applies_to_data( dref_1a ).
DATA(applies_to_1b) = tdo_for_dref_ref_1->applies_to_data( dref_1b ).
ASSERT applies_to_1a = abap_true.
ASSERT applies_to_1b = abap_true.

*&---------------------------------------------------------------------*
*& Creating a data reference with elementary type (DDIC data element)
*&---------------------------------------------------------------------*

"A reference type and variable such as the following shall be created
"using a type description object.
TYPES ty_ref_ts TYPE REF TO timestampl.
DATA dref_2a TYPE ty_ref_ts.
DATA dref_2b TYPE REF TO timestampl.

"Creating a reference type dynamically using a type description object
DATA(tdo_ref_2) = cl_abap_refdescr=>get( cl_abap_typedescr=>describe_by_name( 'TIMESTAMPL' ) ).

"Creating a data reference variable dynamically using a type description object
CREATE DATA dref_ref TYPE HANDLE tdo_ref_2.

"Getting type information and checking type compatibility
DATA(tdo_for_dref_ref_2) = CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data( dref_ref->* ) ).
DATA(applies_to_2a) = tdo_for_dref_ref_2->applies_to_data( dref_2a ).
DATA(applies_to_2b) = tdo_for_dref_ref_2->applies_to_data( dref_2b ).
ASSERT applies_to_2a = abap_true.
ASSERT applies_to_2b = abap_true.

*&---------------------------------------------------------------------*
*& Creating a data reference with structured type
*&---------------------------------------------------------------------*

"A reference type and variable such as the following shall be created
"using a type description object.
TYPES ty_ref_db TYPE REF TO zdemo_abap_carr.
DATA dref_3a TYPE ty_ref_db.
DATA dref_3b TYPE REF TO zdemo_abap_carr.

"Creating a reference type dynamically using a type description object
"The example uses the structured type of a demo database table.
DATA(tdo_ref_3) = cl_abap_refdescr=>get( cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_CARR' ) ).

"Creating a data reference variable dynamically using a type description object
CREATE DATA dref_ref TYPE HANDLE tdo_ref_3.

"Getting type information and checking type compatibility
DATA(tdo_for_dref_ref_3) = CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data( dref_ref->* ) ).
DATA(applies_to_3a) = tdo_for_dref_ref_3->applies_to_data( dref_3a ).
DATA(applies_to_3b) = tdo_for_dref_ref_3->applies_to_data( dref_3b ).
ASSERT applies_to_3a = abap_true.
ASSERT applies_to_3b = abap_true.

*&---------------------------------------------------------------------*
*& Creating a data reference with table type
*&---------------------------------------------------------------------*

"A reference type and variable such as the following shall be created
"using a type description object.
"The example uses a table type based on a locally declared structured type.
TYPES: BEGIN OF struc_type,
         comp1 TYPE i,               "Built-in ABAP types
         comp2 TYPE c LENGTH 5,
         comp3 TYPE p LENGTH 16 DECIMALS 14,
         comp4 TYPE land1,           "DDIC data element
         comp5 TYPE zdemo_abap_carr, "Structure based on the line type of a database table
         comp6 TYPE string_table,    "Table type
       END OF struc_type,
       tab_type TYPE TABLE OF struc_type WITH EMPTY KEY.

TYPES ty_ref_tab TYPE REF TO tab_type.
DATA dref_4a TYPE ty_ref_tab.
DATA dref_4b TYPE REF TO tab_type.

"Creating a reference type dynamically using a type description object
DATA(tdo_ref_4) = cl_abap_refdescr=>get( cl_abap_typedescr=>describe_by_name( 'TAB_TYPE' ) ).

"Creating a data reference variable dynamically using a type description object
CREATE DATA dref_ref TYPE HANDLE tdo_ref_4.

"Getting type information and checking type compatibility
DATA(tdo_for_dref_ref_4) = CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data( dref_ref->* ) ).
DATA(applies_to_4a) = tdo_for_dref_ref_4->applies_to_data( dref_4a ).
DATA(applies_to_4b) = tdo_for_dref_ref_4->applies_to_data( dref_4b ).
ASSERT applies_to_4a = abap_true.
ASSERT applies_to_4b = abap_true.

*&---------------------------------------------------------------------*
*& Excursions: Further methods of class cl_abap_refdescr
*&---------------------------------------------------------------------*

"The following examples cover a selection. For more information, refer
"to the class documentation.

"---- 'get' method ----
"To create or reuse reference types, which are passed as type
"description objects.
"Note: There is also the 'create' method. However, the 'get' method is
"the recommended method.
DATA(tdo_ref_int) = cl_abap_elemdescr=>get_i( ).
DATA(tdo_ref_get_a) = cl_abap_refdescr=>get( tdo_ref_int ).
CREATE DATA dref_ref TYPE HANDLE tdo_ref_get_a.

"Object references can also be used
DATA(tdo_cl_abap_typedescr) = cl_abap_typedescr=>describe_by_name( 'CL_ABAP_TYPEDESCR' ).
DATA(tdo_ref_get_b) = cl_abap_refdescr=>get( tdo_cl_abap_typedescr ).
CREATE DATA dref_ref TYPE HANDLE tdo_ref_get_b.

"---- 'get_by_name' method ----
"To create reference types, which are passed as name.
"Note: There is also the 'create_by_name' method. However, the 'get_by_name'
"method is the recommended method.
DATA(tdo_ref_get_by_name) = cl_abap_refdescr=>get_by_name( 'TIMESTAMP' ).
CREATE DATA dref_ref TYPE HANDLE tdo_ref_get_by_name.

"---- 'describe_by_data' method ----
"To get a reference to the underlying type's type description object.
DATA some_number TYPE int8.
DATA(tdo_ref_descr_by_data) = cl_abap_refdescr=>describe_by_data( some_number ).
DATA(cast_tdo_ref_descr_by_data) = CAST cl_abap_datadescr( tdo_ref_descr_by_data ).
CREATE DATA dref_ref TYPE HANDLE cast_tdo_ref_descr_by_data.

"---- 'describe_by_data_ref' method ----
"To get a reference to the underlying type's type description object, passed as
"reference.
DATA some_data TYPE REF TO data.
some_data = NEW i( 123 ).
DATA(tdo_ref_descr_by_data_ref) = cl_abap_refdescr=>describe_by_data_ref( some_data ).
DATA(cast_tdo_ref_descr_by_data_ref) = CAST cl_abap_datadescr( tdo_ref_descr_by_data ).
CREATE DATA dref_ref TYPE HANDLE cast_tdo_ref_descr_by_data_ref.

"---- 'describe_by_name' method ----
"To get the type description object based on a type passed by relative or absolute name
DATA(tdo_ref_descr_by_name) = cl_abap_refdescr=>describe_by_name( 'TIMESTAMPL'  ).
DATA(cast_tdo_ref_descr_by_name) = CAST cl_abap_datadescr( tdo_ref_descr_by_name ).
CREATE DATA dref_ref TYPE HANDLE cast_tdo_ref_descr_by_name.

"---- 'describe_by_object_ref' method ----
"To get the type description object based on an an object type
DATA(oref) = NEW cl_system_uuid( ).
DATA(tdo_ref_descr_by_oref) = cl_abap_refdescr=>describe_by_object_ref( oref ).

"---- 'get_ref_to_data' method ----
"To get the type description object for the type REF TO DATA
DATA(tdo_ref_get_ref_to_data) = cl_abap_refdescr=>get_ref_to_data( ).

"---- 'get_ref_to_object' method ----
"To get the type description object for the type REF TO OBJECT
DATA(tdo_ref_get_ref_to_object) = cl_abap_refdescr=>get_ref_to_object( ).

"---- Note ----
"Note the 'get_referenced_type' method when casting to cl_abap_refdescr.
TYPES test_type TYPE c LENGTH 10.
DATA test_dobj TYPE c LENGTH 10.
DATA dref_dobj TYPE REF TO test_type.
dref_dobj = NEW #( ).

DATA(tdo_ref_get_elemdescr) = CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data( dref_dobj ) ).
DATA(tdo_ref_get_referenced_type) = tdo_ref_get_elemdescr->get_referenced_type( ).
DATA(cast_tdo_ref_get_ref_type) = CAST cl_abap_datadescr( tdo_ref_get_referenced_type ).
CREATE DATA dref_ref TYPE HANDLE cast_tdo_ref_get_ref_type.

DATA(tdo4applies) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( test_dobj ) ).
DATA(applies_to_dobj) = tdo4applies->applies_to_data( dref_ref->* ).
ASSERT applies_to_dobj = abap_true.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursion: Dynamic Program Development in Standard ABAP 

- [Standard ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_abap_glosry.htm) includes language elements that allow for the dynamic creation and maintenance of program source code.
- Creating executable code using dynamic programming techniques can be risky, especially when source code is determined at runtime or involves external data or user input. Therefore, these techniques should be used only in exceptional cases and require thorough validation and checks.
- In [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm), you can access and modify source code using classes available in the XCO library. Refer to the [Released ABAP Classes](22_Released_ABAP_Classes.md) ABAP cheat sheet for examples.


> [!CAUTION]
> - Dynamic programming techniques can pose serious security risks if used improperly. Always carefully check any dynamic content from external sources before incorporating it into dynamic statements. Prevent the injection of harmful ABAP code into programs, especially when using `GENERATE SUBROUTINE POOL` and `INSERT REPORT` statements, as they can create executable ABAP code.
> - When using ABAP statements to manipulate source code, apply them cautiously. These statements do not include inherent authorization checks, so developers must implement these checks. For example, use appropriate `AUTHORITY-CHECK` statements. Potential checks include verifying the current user's development authorization or determining if the current system is a development or production environment.
> - Find more information (note that the links refer to Standard ABAP) about:
>   - [Dynamic program development](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_language_dynamic.htm) and [security](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abengeneric_prog_scrty.htm) in the ABAP Keyword Documentation.
>   - the ABAP authorization concept in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenbc_authority_check.htm) and in the [SAP Help Portal](https://help.sap.com/docs/ABAP_PLATFORM_NEW/ad77b44570314f6d8c3a8a807273084c/4f4decf806b02892e10000000a42189b.html?locale=en-US).
> - The code examples are primarily intended to visualize the syntax and semantics of ABAP statements. They are not best practices or recommended implementations and are not meant to solve concrete programming tasks. Refer to the repository's [disclaimer](./README.md#%EF%B8%8F-disclaimer).

<br>

<details>
  <summary>🟢 Click to expand for more details and example code</summary>
  <!-- -->

<br>

<table>

<tr>
<th> ABAP Statement </th> <th> Notes/Example Code </th>
</tr>

<tr>
<td> 

`SYNTAX-CHECK`

 </td>

 <td> 

- Performs a syntax check for source code in internal tables.
- For examples like `GENERATE SUBROUTINE POOL`, the syntax check occurs when these statements are executed.
- Use the `PROGRAM` and `DIRECTORY ENTRY` options (with the specified data object matching the `TRDIR` database table structure) to set properties for the syntax check. At least one must be specified.
- Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsyntax-check_for_itab.htm).

The example executable program includes several `SYNTAX-CHECK` statements. The examples include a syntax check with and without error found. Plus, an example represents a demo implementation to check ABAP for Cloud Development readiness. To check out the code - ⚠️ you are aware of the security risks associated with dynamic programming techniques - you can create a demo executable program called `ZDEMO_ABAP_REPORT_DYNAMIC_PROG` in your sandbox environment and choose `F8` to execute it. The example is designed to write the content of data objects to a [classic list](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclassic_list_glosry.htm) using `WRITE` statements.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

```abap
PROGRAM.

DATA: tab TYPE string_table,
      msg TYPE string,
      lin TYPE i,
      wrd TYPE string.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: check_sysubrc.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD check_sysubrc.
    IF sy-subrc = 0.
      WRITE / `Syntax check: Ok` COLOR COL_POSITIVE.
    ELSE.
      WRITE / |sy-subrc: { sy-subrc }| COLOR COL_NEGATIVE.
      WRITE / |MESSAGE: { msg }| COLOR COL_NEGATIVE.
      WRITE / |LINE: { lin }| COLOR COL_NEGATIVE.
      WRITE / |WORD: { wrd }| COLOR COL_NEGATIVE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

  WRITE / `***************************** Example 1 *****************************`.
  SKIP.

  tab = VALUE #(
    ( `PROGRAM.`        )
    ( `TYPES a TYPE i.` )
    ( `DATA b TYPE a.`  )
    ( `b = 1.`          ) ).

  SYNTAX-CHECK FOR tab MESSAGE msg LINE lin WORD wrd PROGRAM sy-repid.

  lcl_demo=>check_sysubrc( ).

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

  "The following example includes a syntax error (missing period).

  SKIP.
  WRITE / `***************************** Example 2 *****************************`.
  SKIP.

  tab = VALUE #(
    ( `PROGRAM.`        )
    ( `TYPES a TYPE i.` )
    ( `DATA b TYPE a`   )
    ( `b = 1.`          ) ).

  SYNTAX-CHECK FOR tab MESSAGE msg LINE lin WORD wrd PROGRAM sy-repid.

  lcl_demo=>check_sysubrc( ).

*&---------------------------------------------------------------------*
*& Example 3
*&---------------------------------------------------------------------*

  "The following example performs a syntax check using the code skeleton of class.
  "It represents a self-contained example with which to check ABAP for Cloud Development
  "readiness, using a dynamically inserted code sample (i.e. not the actual code in
  "a class).
  "The assumption is that the demo class used is a class that has at least one method
  "specified in the global class, without any further implementations in includes.
  "The example uses a class from the ABAP cheat sheet repository.
  "Note that not every class may be suitable for the demo.
  "The class provides the skeleton for the source code that is constructed
  "dynamically in the 'program' data object, which is a string table.
  "The example uses demo code that is to be checked in the string table
  "'code_to_check'. It is designed to perform a code check for both
  "Standard ABAP and ABAP for Cloud Development. The SYNTAX-CHECK statement
  "uses the DIRECTORY ENTRY addition, which expects an operand that corresponds to the
  "structure of the TRDIR database table. The ABAP version check is determined by
  "the value of the 'uccheck' component. '5' means checks are performed for
  "ABAP for Cloud Development. The example uses a class because the program-initiating
  "statements PROGRAM or REPORT are not possible in ABAP for Cloud Development, i.e.
  "the syntax will return a message for these statements.
  "The code to be checked includes a simple assignment, however, it uses the
  "obsolete MOVE statement. While the statement may still be used (though not
  "recommended) without syntax error in Standard ABAP, it is not possible in ABAP for
  "Cloud Development. Consequently, the syntax check will show an error for the statement.
  "The example just uses a random method implementation of the class skeleton (other
  "method implementations, if any, are empty) in which the code to be checked
  "is inserted. So, not the actual class code is checked but only the dummy code
  "skeleton contained in the internal table, resembling the actual class, including code
  "inserted dynmically at runtime.

  SKIP.
  WRITE / `***************************** Example 3 *****************************`.
  SKIP.

  "Data objects for the SYNTAX-CHECK statement
  DATA: message  TYPE string,
        line     TYPE i,
        word     TYPE string,
        direntry TYPE trdir.

  "Demo code that is put in a random method implementation skeleton
  DATA(code_to_check) = VALUE string_table(
    ( `DATA some_random_number_a TYPE i VALUE 1.` )
    ( `DATA some_random_number_b TYPE i VALUE 2.` )
    ( `MOVE some_random_number_a TO some_random_number_b.` ) ).

  "Constructing include names used in the class code skeleton
  "The example uses an example class from the ABAP cheat sheet repository
  TYPES t_c32 TYPE c LENGTH 32.
  DATA(class_name) = CONV t_c32( 'ZCL_DEMO_ABAP_AUX' ).
  DATA include_name LIKE class_name.
  TYPES tt_c32 TYPE TABLE OF t_c32 WITH EMPTY KEY.

  DATA(class_includes) = VALUE tt_c32(
  ( '==============================CP' )
  ( '==============================CU' )
  ( '==============================CO' )
  ( '==============================CI' )
  ( '==============================CC' ) ).

  LOOP AT class_includes REFERENCE INTO DATA(ref).
    include_name = class_name.
    OVERLAY include_name WITH ref->*.
    ref->* = include_name.
  ENDLOOP.

  "Getting method name for the dummy implementation in the class code skeleton
  TRY.
      DATA dummy_implementations TYPE string_table.
      LOOP AT CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( class_name ) )->methods INTO DATA(mwa)
      WHERE is_inherited IS INITIAL AND name <> 'CONSTRUCTOR'.
        IF sy-tabix = 1.
          DATA(dummy_method) = mwa-name.
        ELSE.
          APPEND |  METHOD { mwa-name }. ENDMETHOD.| TO dummy_implementations.
        ENDIF.
      ENDLOOP.
    CATCH cx_sy_rtti_syntax_error.
      WRITE / |RTTI error with class { class_name }.| COLOR COL_NEGATIVE.
      RETURN.
  ENDTRY.

  IF dummy_implementations IS INITIAL.
    WRITE / |Class { class_name } is not a suitable class for this example.| COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

  "Putting the code together to have suitable internal table content
  "representing the code of a class, including the dummy method
  "implementation with the code to be checked
  APPEND LINES OF VALUE string_table(
     ( |  METHOD { dummy_method }.| )
     ( LINES OF code_to_check )
     ( `ENDMETHOD.` ) ) TO dummy_implementations.

  DATA(program) = VALUE string_table(
    ( `CLASS-POOL.` )
    ( |INCLUDE { class_includes[ 2 ] }.| ) "CU
    ( |INCLUDE { class_includes[ 3 ] }.| ) "CO
    ( |INCLUDE { class_includes[ 4 ] }.| ) "CI
    ( `ENDCLASS.` )
    ( |INCLUDE { class_includes[ 5 ] }IMP.| ) "CCIMP
    ( |INCLUDE { class_includes[ 5 ] }AU.| ) "CCAU
    ( |CLASS { class_name } IMPLEMENTATION.| )
    ( LINES OF dummy_implementations )
    ( ` ENDCLASS.` ) ).

  "Retrieving TRDIR entry for the class in question
  SELECT SINGLE *
     FROM trdir
     WHERE name = @( class_includes[ 1 ] ) "CP
     INTO @direntry.

  WRITE / |Class: { class_name }|.
  WRITE / |Dummy method: { dummy_method }|.
  SKIP.

  "In the loop, the SYNTAX-CHECK statement performs a syntax check for both
  "Standard ABAP and ABAP for Cloud Development.
  DO 2 TIMES.
    IF sy-index = 1.
      "Standard ABAP
      direntry-uccheck = 'X'.
      WRITE / `************* Syntax check for Standard ABAP *************`.
    ELSE.
      "ABAP for Cloud Development
      direntry-uccheck = '5'.
      WRITE / `************* Syntax check for ABAP for Cloud Development *************`.
    ENDIF.

    SYNTAX-CHECK FOR program MESSAGE message LINE line WORD word DIRECTORY ENTRY direntry.

    IF sy-subrc = 0.
      WRITE / `Syntac check: Ok` COLOR COL_POSITIVE.
      SKIP.
    ELSE.
      WRITE / |sy-subrc: { sy-subrc }| COLOR COL_NEGATIVE.
      WRITE / |MESSAGE: { message }| COLOR COL_NEGATIVE.
      WRITE / |LINE: { line }| COLOR COL_NEGATIVE.
      WRITE / |WORD: { word }| COLOR COL_NEGATIVE.
      SKIP.
    ENDIF.
    CLEAR: message, line, word.
  ENDDO.
```

</details>  
<br>
 </td>
</tr>

<tr>
<td> 

`READ REPORT`

 </td>

 <td> 

- Reads the active source code of an ABAP program into an internal table.
- The internal table's line type must be long enough to fit the longest code line.

The example executable program includes several `READ REPORT` statements to read the source code of executable programs, classes and method implementations. To check out the code, you can create a demo executable program called `ZDEMO_ABAP_REPORT_DYNAMIC_PROG` in your sandbox environment and choose `F8` to execute it. The example is designed to write the content of data objects to a [classic list](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclassic_list_glosry.htm) using `WRITE` statements.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

```abap
PROGRAM.

DATA itab TYPE string_table.

*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

"The example specifies the name of an executable program stored in a data object.

WRITE / `***************************** Example 1 *****************************` COLOR COL_HEADING.
SKIP.

DATA(this_program) = sy-repid.
READ REPORT this_program INTO itab.

IF sy-subrc = 0.
  WRITE / |The code was read into the internal table.| COLOR COL_POSITIVE.
  SKIP.
  SET BLANK LINES ON.
  "Not writing all code lines to the classic list
  LOOP AT itab INTO DATA(wa) TO 12.
    WRITE / wa.
  ENDLOOP.
  WRITE / |... (skipping the rest of the code; number of code lines: { lines( itab ) })|.

  SKIP.
ELSE.
  WRITE / |sy-subrc: { sy-subrc }| COLOR COL_NEGATIVE.
  WRITE / |The code was not read.| COLOR COL_NEGATIVE.
  SKIP.
ENDIF.

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

"The examples specify the names of executable programs as literals.

SKIP.
WRITE / `***************************** Example 2 *****************************` COLOR COL_HEADING.
SKIP.

READ REPORT 'ZDEMO_ABAP_REPORT_DYNAMIC_PROG' INTO itab.

IF sy-subrc = 0.
  WRITE / |The code was read into the internal table. Number of code lines: { lines( itab ) }| COLOR COL_POSITIVE.
  SKIP.
ELSE.
  WRITE / |sy-subrc: { sy-subrc }| COLOR COL_NEGATIVE.
  WRITE / |The code was not read into the internal table.| COLOR COL_NEGATIVE.
  SKIP.
ENDIF.

READ REPORT 'ZTHIS_PROG_MAY_NOT_EXIST' INTO itab.

IF sy-subrc = 0.
  WRITE / |The code was read into the internal table. Number of code lines: { lines( itab ) }| COLOR COL_POSITIVE.
  SKIP.
ELSE.
  WRITE / |sy-subrc: { sy-subrc }| COLOR COL_NEGATIVE.
  WRITE / |The code was not read into the internal table.| COLOR COL_NEGATIVE.
  SKIP.
ENDIF.

*&---------------------------------------------------------------------*
*& Example 3
*&---------------------------------------------------------------------*

"The examples reads the code of class includes.
"The example is set up to write the content of the internal table. Note
"that when writing the code lines, long code lines are cut.
"A class from the ABAP cheat sheet repository is used because it contains
"code in several includes.

SKIP.
WRITE / `***************************** Example 3 *****************************` COLOR COL_HEADING.
SKIP.

"Constructing include names
"The example uses an example class from the ABAP cheat sheet repository.
"Note that there are more includes available, e.g. also for each method as shown
"below. The example constructs the names manually. You may also use a system class as
"shown for the method include names.
TYPES t_c35 TYPE c LENGTH 35.
DATA(class_name) = CONV t_c35( 'ZCL_DEMO_ABAP_UNIT_TEST' ).
DATA include_name LIKE class_name.
TYPES tt_c35 TYPE TABLE OF t_c35 WITH EMPTY KEY.

DATA(class_includes) = VALUE tt_c35(
( '==============================CP' ) "Class pool
( '==============================CU' ) "Public section
( '==============================CO' ) "Protected section
( '==============================CI' ) "Private section
( '==============================CCDEF' ) "CCDEF include (Class-relevant Local Types tab in ADT)
( '==============================CCIMP' ) "CCIMP include (Local Types tab in ADT)
( '==============================CCAU' ) "Test include (Test Classes tab in ADT)
( '==============================CS' )  "Complete global class (Global Class tab in ADT)
).

LOOP AT class_includes REFERENCE INTO DATA(ref).
  include_name = class_name.
  OVERLAY include_name WITH ref->*.
  WRITE / |****************************************************| COLOR COL_NORMAL.
  WRITE / |* Include { include_name }| COLOR COL_NORMAL.
  WRITE / |****************************************************| COLOR COL_NORMAL.
  SKIP.

  READ REPORT include_name INTO itab.

  IF sy-subrc = 0.
    LOOP AT itab INTO DATA(incl_wa).
      WRITE / incl_wa.
    ENDLOOP.
    SKIP.
  ELSE.
    WRITE / |sy-subrc: { sy-subrc }| COLOR COL_NEGATIVE.
    WRITE / |The code was not read.| COLOR COL_NEGATIVE.
    SKIP.
  ENDIF.
ENDLOOP.

*&---------------------------------------------------------------------*
*& Example 4
*&---------------------------------------------------------------------*

"The examples reads the code of method implementations.
"A class from the ABAP cheat sheet repository is used because it contains
"code in several methods. Unlike the example above, a system class is used
"to retrieve method include names. The class also offers more methods with
"which manual construction as above is unnecessary.

SKIP.
WRITE / `***************************** Example 4 *****************************` COLOR COL_HEADING.
SKIP.

"Processing includes of individual methods of the class
DATA(methods) = cl_oo_classname_service=>get_all_method_includes( clsname = CONV #( class_name ) ).

LOOP AT methods INTO DATA(meth_wa).
  READ REPORT meth_wa-incname INTO itab.

  IF sy-subrc = 0.
    WRITE / |Class { meth_wa-cpdkey-clsname }, Method { meth_wa-cpdkey-cpdname }|.
    WRITE / |Include { meth_wa-incname } was read into the internal table.| COLOR COL_POSITIVE.
    SKIP.
  ELSE.
    WRITE / |Class { meth_wa-cpdkey-clsname }, Method { meth_wa-cpdkey-cpdname }|.
    WRITE / |Include { meth_wa-incname } was not read into the internal table.| COLOR COL_NEGATIVE.
    SKIP.
  ENDIF.
ENDLOOP.
```

</details>  
<br>
 </td>
</tr>

<tr>
<td> 

`GENERATE SUBROUTINE POOL`

 </td>

 <td> 

- Generates temporary [subroutine pools](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensubroutine_pool_glosry.htm).
- Takes source code for the subroutine pool from an internal table.
  - Internal table properties: Must have a character-like line type with a maximum of 255 characters per line, no secondary table keys, and must include a statement to initiate a program (e.g., `PROGRAM.`).
  - If the internal table code contains a syntax error, the subroutine pool is not generated. Specify additions for error handling (e.g., `MESSAGE` or `WORD`) to analyze syntax and generation errors.
- After `NAME`, specify a character-like variable or declare a data object inline (e.g., using `DATA(var)`).
- Note that subroutines are considered outdated for modularization. You might use a single subroutine with a local class.

The example executable program includes several `GENERATE SUBROUTINE POOL` statements. To check out the code - ⚠️ you are aware of the security risks associated with dynamic programming techniques - you can create a demo executable program called `ZDEMO_ABAP_REPORT_DYNAMIC_PROG` in your sandbox environment and choose `F8` to execute it. The example is designed to write the content of data objects to a [classic list](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclassic_list_glosry.htm) using `WRITE` statements.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

```abap
PROGRAM.

*&---------------------------------------------------------------------*
*& Exemplary authorization and security checks
*&---------------------------------------------------------------------*

WRITE / `***************************** Authorization check *****************************` COLOR COL_HEADING.
SKIP.

"Here go authorization and security checks. See the INSERT REPORT example
"for ideas.
...

"Checking whether the current system is a production system
"Alternatively, you can use the TR_SYS_PARAMS function module.
SELECT SINGLE @abap_true
        FROM t000
        WHERE cccategory = 'P'
        INTO @DATA(flag).

IF flag = abap_true.
  WRITE / `The system is a production system.` COLOR COL_NEGATIVE.
  RETURN.
ELSE.
  WRITE / `The system is not a production system.` COLOR COL_POSITIVE.
ENDIF.
SKIP.

"Checking whether the current user can create temporary programs.
AUTHORITY-CHECK OBJECT 'S_DEVELOP'
  ID 'DEVCLASS' FIELD '$TMP'
  ID 'OBJTYPE'  FIELD 'PROG'
  ID 'OBJNAME'  DUMMY
  ID 'P_GROUP'  DUMMY
  ID 'ACTVT'    FIELD '02'.
IF sy-subrc = 0.
  WRITE / `You have development authorization in the $TMP package.` COLOR COL_POSITIVE.
ELSE.
  WRITE / `You do not have development authorization in the $TMP package.` COLOR COL_NEGATIVE.
  RETURN.
ENDIF.
SKIP.

*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

WRITE / `***************************** Example 1 *****************************` COLOR COL_HEADING.
SKIP.

DATA tab1 TYPE string_table.

tab1 = VALUE #(
  ( `PROGRAM.`                        )
  ( `FORM test.`                      )
  ( ` WRITE / |Hello { sy-uname }.|.` )
  ( `ENDFORM.`                        ) ).

GENERATE SUBROUTINE POOL tab1 NAME DATA(prog1).

IF sy-subrc = 0.
  WRITE / `Example 1: Ok` COLOR COL_POSITIVE.
  SKIP.
  PERFORM ('TEST') IN PROGRAM (prog1) IF FOUND.
ELSE.
  WRITE / |sy-subrc: { sy-subrc }| COLOR COL_NEGATIVE.
  WRITE / `Example 1: Error` COLOR COL_NEGATIVE.
ENDIF.
SKIP.

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

"In this example, the code contained in the internal table intentionally
"has a syntax error. A period is missing at the end of the DATA statement.
"The GENERATE SUBROUTINE POOL statement uses more of the available
"additions.

SKIP.
WRITE / `***************************** Example 2 *****************************` COLOR COL_HEADING.
SKIP.

"missing period at end of line
DATA(tab2) = VALUE string_table(
  ( `PROGRAM.`                                            )
  ( `FORM test.`                                          )
  ( ` DATA some_string TYPE string VALUE ``hello```       )
  ( ` WRITE / |Value of some_string: "{ some_string }"|.` )
  ( `ENDFORM.`                                            ) ).

GENERATE SUBROUTINE POOL tab2 NAME DATA(prog2)
         MESSAGE DATA(mess) "Error message text of first syntax error
         INCLUDE DATA(incl) "Internal name beginning with %_ in case of successful generation
         LINE DATA(lin) "Line number of the first syntax error
         WORD DATA(wrd) "First token with an error
         OFFSET DATA(off) "Offset of the first token with errors
         MESSAGE-ID DATA(mid) "Key of the first error message stored in the TRMSG database table
         SHORTDUMP-ID DATA(sid). "Exception that is raised during generation

IF sy-subrc = 0.
  WRITE / `Example 2: Ok` COLOR COL_POSITIVE.
  PERFORM ('TEST') IN PROGRAM (prog2) IF FOUND.
ELSE.
  WRITE / |sy-subrc: { sy-subrc }| COLOR COL_NEGATIVE.
  WRITE / |MESSAGE: { mess }| COLOR COL_NEGATIVE.
  WRITE / |INCLUDE: { incl }| COLOR COL_NEGATIVE.
  WRITE / |LINE: { lin }| COLOR COL_NEGATIVE.
  WRITE / |WORD: { wrd }| COLOR COL_NEGATIVE.
  WRITE / `MESSAGE-ID:` COLOR COL_NEGATIVE.
  LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( mid ) )->components INTO DATA(comp).
    ASSIGN COMPONENT comp-name OF STRUCTURE mid TO FIELD-SYMBOL(<fs>).
    WRITE / |- { comp-name }: { <fs> }| COLOR COL_NEGATIVE.
    "Or using newer syntax as an alternative
*    WRITE / |- { comp-name }: { mid-(comp-name) }|.
  ENDLOOP.
  WRITE / |SHORTDUMP-ID: { sid }| COLOR COL_NEGATIVE.
ENDIF.

*&---------------------------------------------------------------------*
*& Example 3
*&---------------------------------------------------------------------*

"In this example, the subroutine pool implements a local class with a
"static and an instance method. These methods are called dynamically with
"the help of the class's absolute type name.

SKIP.
WRITE / `***************************** Example 3 *****************************` COLOR COL_HEADING.
SKIP.

DATA(tab3) = VALUE string_table(
  ( `PROGRAM.`                                                        )
  ( `CLASS lcl_test DEFINITION.`                                      )
  ( `  PUBLIC SECTION.`                                               )
  ( `    CLASS-METHODS static_double IMPORTING num TYPE i`            )
  ( `                                RETURNING VALUE(result) TYPE i.` )
  ( `    METHODS instance_triple IMPORTING num TYPE i`                )
  ( `                            RETURNING VALUE(result) TYPE i.`     )
  ( `ENDCLASS.`                                                       )
  ( `CLASS lcl_test IMPLEMENTATION.`                                  )
  ( `  METHOD static_double.`                                         )
  ( `    result = num * 2.`                                           )
  ( `  ENDMETHOD.`                                                    )
  ( `  METHOD instance_triple.`                                       )
  ( `    result = num * 3.`                                           )
  ( `  ENDMETHOD.`                                                    )
  ( `ENDCLASS.`                                                       ) ).

GENERATE SUBROUTINE POOL tab3 NAME DATA(prog3).

IF sy-subrc = 0.

  DATA(cl) = `\PROGRAM=` && prog3 && `\CLASS=LCL_TEST`.

  "Static method
  DATA res1 TYPE i.
  CALL METHOD (cl)=>static_double
    EXPORTING
      num    = 4
    RECEIVING
      result = res1.

  WRITE / |Result of calling static method (res1): { res1 }|.

  "Instance method
  DATA oref TYPE REF TO object.
  DATA res2 TYPE i.

  CREATE OBJECT oref TYPE (cl).

  CALL METHOD oref->('INSTANCE_TRIPLE')
    EXPORTING
      num    = 5
    RECEIVING
      result = res2.

  WRITE / |Result of calling instance method (res2): { res2 }|.

ELSE.
  WRITE / |sy-subrc: { sy-subrc }| COLOR COL_NEGATIVE.
  WRITE / `Example 3: Error` COLOR COL_NEGATIVE.
ENDIF.
```

</details>  
<br>
 </td>
</tr>

<tr>
<td> 

`INSERT REPORT`

 </td>

 <td> 

- Passes source code contained in an internal table to a specified ABAP program.
- If the program exists, its source code is overwritten; otherwise, a new program is created with the specified name but is not assigned to a package. In this case, you must create an object directory entry.
- Various additions are available to determine program properties.
- Find more information in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapinsert_report.htm).


> [!CAUTION]
> - Dynamic program development statements should be reserved for exceptional cases. Use utmost caution with `INSERT REPORT` and `GENERATE SUBROUTINE POOL` statements to create executable ABAP code.  
> - Dynamic code originating from external sources can inject malicious code. Any user with authorization for the program can execute the source code directly. `INSERT REPORT` can even overwrite existing programs.  
> - It is crucial to implement appropriate authorization and security checks. Some considerations and examples:
>   - Securing ABAP programs by checking the current user's development authorization.
>   - Preventing overwriting source code by verifying a program name's existence in the `TRDIR` database table.
>   - Preventing SQL injection in dynamic ABAP SQL statements when including external content, such as via a user interface, by using the `CL_ABAP_DYN_PRG` class. Injections may involve accessing unauthorized database tables or columns and manipulating dynamic WHERE conditions. Refer to the *Security Considerations in Dynamic Programming Using External Input* section for code examples.
>   - Using the `CL_ABAP_DYN_PRG` class to perform checks against specific allowlists or excludelists.
>   - Conduct custom code scans to detect prohibited patterns, like forbidden statements in dynamically created source code.
>   - Although not related to a checking mechanism, consider storing and backing up source code before making any overwrites.
> - A thorough understanding of program structures, names, and include programs is essential when using the statement.
> - For general information, check the [Security Notes](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_security.htm) in the ABAP Keyword Documentation.

The following example programs use `INSERT REPORT` statements. To check out the code - ⚠️ you are aware of the security risks associated with dynamic programming techniques - create a demo executable program named `ZDEMO_ABAP_REPORT_DYNAMIC_PROG` in your sandbox environment if you have not done so. The example modifies source code and is configured to write data object content to a [classic list](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclassic_list_glosry.htm) using `WRITE` statements. To run the program, choose `F8`. The `CL_DEMO_OUTPUT` class displays the output of the original source code as a backup. Close the output window. The list will then be displayed with content output using the `WRITE` statements. *Example 1* is similar to *Example 2*, but *Example 2* includes demo implementations regarding considerations for potential authorization and security checks. Make sure that you devise your own solutions for your specific use cases.

<details>
  <summary>🟢 Example 1</summary>
  <!-- -->

<br>

```abap
PROGRAM.

*&---------------------------------------------------------------------*
*& Dynamic source code to be inserted into an executable program
*&---------------------------------------------------------------------*

DATA(code) = VALUE string_table(
( `PROGRAM.` )
( `` )
( `WRITE / 'This program was overwritten.'.` )
).

*&---------------------------------------------------------------------*
*& Authorization and security checks
*&---------------------------------------------------------------------*

"Here go authorization and security checks. See the second INSERT REPORT
"example for ideas.
...

"This example assumes you created a demo executable program named
"ZDEMO_ABAP_REPORT_DYNAMIC_PROG in a sandbox environment.
DATA(prog) = CONV sy-repid( 'ZDEMO_ABAP_REPORT_DYNAMIC_PROG' ).
ASSERT sy-repid = prog.

*&---------------------------------------------------------------------*
*& Syntax check
*&---------------------------------------------------------------------*

DATA: msg TYPE string,
      lin TYPE i,
      wrd TYPE string.

SYNTAX-CHECK FOR code MESSAGE msg LINE lin WORD wrd PROGRAM prog.

IF sy-subrc = 0.
  WRITE / `Syntax check: Ok` COLOR COL_POSITIVE.
ELSE.
  WRITE / |sy-subrc: { sy-subrc }| COLOR COL_NEGATIVE.
  WRITE / |MESSAGE: { msg }| COLOR COL_NEGATIVE.
  WRITE / |LINE: { lin }| COLOR COL_NEGATIVE.
  WRITE / |WORD: { wrd }| COLOR COL_NEGATIVE.
  RETURN.
ENDIF.

*&---------------------------------------------------------------------*
*& Inserting source code into the program
*&---------------------------------------------------------------------*

INSERT REPORT prog FROM code.

IF sy-subrc = 0.
  WRITE / |Program { prog } was overwritten.| COLOR COL_POSITIVE.
  SKIP.

  DATA code_copy TYPE string_table.
  READ REPORT prog INTO code_copy.
  IF sy-subrc = 0.
    WRITE / |New code of the program { prog }:|.
    SKIP.

    SET BLANK LINES ON.
    LOOP AT code_copy INTO DATA(wa).
      WRITE / wa .
    ENDLOOP.
  ELSE.
    WRITE / `An error occured.` COLOR COL_NEGATIVE.
  ENDIF.
ENDIF.
```

</details>  
<br>
<details>
  <summary>🟢 Example 2</summary>
  <!-- -->

<br>

```abap
PROGRAM.

*&---------------------------------------------------------------------*
*& Dynamic source code to be inserted into an executable program
*&---------------------------------------------------------------------*

DATA(code) = VALUE string_table(
  ( `PROGRAM.` )
  ( `` )
  ( `WRITE / |The program { sy-repid } was overwritten.|.` ) ).

*&---------------------------------------------------------------------*
*& Exemplary authorization and security checks
*&---------------------------------------------------------------------*

"Here go authorization and security checks.
...
"The following example implementations demonstrate some considerations about
"authorization and security checks. They are not intended as best practice
"examples. Always develop your own solution for your program.

"This example assumes you created a demo executable program named
"ZDEMO_ABAP_REPORT_DYNAMIC_PROG in a sandbox environment.
DATA(prog) = CONV sy-repid( 'ZDEMO_ABAP_REPORT_DYNAMIC_PROG' ).
ASSERT sy-repid = prog.

"Checking whether the current system is a production system
"Alternatively, you can use the TR_SYS_PARAMS function module.
SELECT SINGLE @abap_true
        FROM t000
        WHERE cccategory = 'P'
        INTO @DATA(flag).

IF flag = abap_true.
  WRITE / `The system is a production system.` COLOR COL_NEGATIVE.
  RETURN.
ELSE.
  WRITE / `The system is not a production system.` COLOR COL_POSITIVE.
ENDIF.
SKIP.

"Exemplary authorization checks
"The demo program is only allowed for users who are allowed
"to use the ABAP Editor.
CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
  EXPORTING
    tcode  = 'SE38'
  EXCEPTIONS
    ok     = 1
    not_ok = 2
    OTHERS = 3.
IF sy-subrc < 2.
  WRITE / `You are allowed to use the ABAP Editor.` COLOR COL_POSITIVE.
ELSE.
  WRITE / `You are not allowed to use the ABAP Editor.` COLOR COL_NEGATIVE.
  RETURN.
ENDIF.
SKIP.

"Checking whether the current user can create temporary programs.
"You may also consider restricting program access on a package level,
"for example.
AUTHORITY-CHECK OBJECT 'S_DEVELOP'
  ID 'DEVCLASS' FIELD '$TMP'
  ID 'OBJTYPE'  FIELD 'PROG'
  ID 'OBJNAME'  DUMMY
  ID 'P_GROUP'  DUMMY
  ID 'ACTVT'    FIELD '02'.
IF sy-subrc = 0.
  WRITE / `You have development authorization in the $TMP package.` COLOR COL_POSITIVE.
ELSE.
  WRITE / `You do not have development authorization in the $TMP package.` COLOR COL_NEGATIVE.
  RETURN.
ENDIF.
SKIP.

"Verifying against a given allowlist/excludelist
"The demo statement demonstrates calling a method from the cl_abap_dyn_prg class
"to check input against an allowlist. The content of 'value' is not used further
"in the program. The example assumes that a demo executable program named
"ZDEMO_ABAP_REPORT_DYNAMIC_PROG is used. The allowlist includes further
"demo program names from the ABAP cheat sheet repository.
TRY.
    DATA(value) = cl_abap_dyn_prg=>check_allowlist(
        val           = prog
        allowlist_htab = VALUE #( ( `ZDEMO_ABAP_REPORT_DYNAMIC_PROG` )
                                  ( `ZDEMO_ABAP_LISTS` )
                                  ( `ZDEMO_ABAP_ALV` ) ) ).

    WRITE: / |The program { value } is in the allowlist.| COLOR COL_POSITIVE.
  CATCH cx_abap_not_in_allowlist INTO DATA(error_not_allowed).
    WRITE / |The program { value } is not in the allowlist.|.
    WRITE / error_not_allowed->get_text( ) COLOR COL_NEGATIVE.
    RETURN.
ENDTRY.
SKIP.

"Checking if the program exists to avoid overwriting.
"If the program does not exist, a new program with the specified name is created.
"The self-contained example is designed to overwrite the existing demo program.
SELECT SINGLE 'X'
  FROM trdir
  WHERE name = @prog
  INTO @DATA(prog_exists).

IF prog_exists IS INITIAL.
  WRITE / |The program { prog } does not exist.| COLOR COL_POSITIVE.
ELSE.
  WRITE / |The program { prog } already exists.| COLOR COL_NEGATIVE.
  WRITE / |The self-contained demo example { prog } is designed to overwrite the existing code.| COLOR COL_POSITIVE.
  "RETURN.
ENDIF.
SKIP.

"Reading and (potentially) backing up existing program code before overwriting.
"In the self-contained example, the source code is read using a READ REPORT
"statement and output using the demo class CL_DEMO_OUTPUT to avoid interfering
"with WRITE statements of the program.
"Your use case might also be to read the code, modify it in the internal table,
"and then overwrite the program with the updated content.
DATA code_copy TYPE string_table.
READ REPORT prog INTO code_copy.
cl_demo_output=>write_html( `<h3 style="color:green;">Close this output window.</h3>` ).
cl_demo_output=>write_html( `<p>The output shows the original source code read, e.g. for backup purposes.` ).
cl_demo_output=>line( ).
cl_demo_output=>display( code_copy ).

"Custom code scan to perform a basic check for excluded patterns.
"However, it may produce false positives, such as findings in literals. The scan
"in the example examines a limited set of patterns, including method calls and
"certain modification statements.
DATA(excluded_patterns) = VALUE string_table(
  ( `=>` )
  ( `->` )
  ( `CALL FUNCTION` )
  ( `INSERT` )
  ( `MODIFY` )
  ( `DELETE` )
  ( `GENERATE SUBROUTINE POOL` )
  ( `CALL TRANSACTION` )
  ( `EXEC SQL` ) ).

"Concatenating the string table containing the code for a more convenient
"string processing.
DATA(code_as_string) = concat_lines_of( table = code sep = ` ` ).

LOOP AT excluded_patterns INTO DATA(p).
  "Constructing a regex to search the source code
  p = |{ replace( val = p pcre = `\s` with = `\\s+` occ = 0 ) }|.

  FIND PCRE p IN code_as_string.
  IF sy-subrc = 0.
    WRITE / |The code scan found an excluded pattern: { p }| COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.
ENDLOOP.

WRITE / `Code scan: Ok` COLOR COL_POSITIVE.
SKIP.

*&---------------------------------------------------------------------*
*& Syntax check
*&---------------------------------------------------------------------*

DATA: msg TYPE string,
      lin TYPE i,
      wrd TYPE string.

SYNTAX-CHECK FOR code MESSAGE msg LINE lin WORD wrd PROGRAM prog.

IF sy-subrc = 0.
  WRITE / `Syntax check: Ok` COLOR COL_POSITIVE.
ELSE.
  WRITE / |sy-subrc: { sy-subrc }| COLOR COL_NEGATIVE.
  WRITE / |MESSAGE: { msg }| COLOR COL_NEGATIVE.
  WRITE / |LINE: { lin }| COLOR COL_NEGATIVE.
  WRITE / |WORD: { wrd }| COLOR COL_NEGATIVE.
  RETURN.
ENDIF.
SKIP.

*&---------------------------------------------------------------------*
*& Inserting source code into the program
*&---------------------------------------------------------------------*

INSERT REPORT prog FROM code.

IF sy-subrc = 0.
  WRITE / |Program { prog } was overwritten.| COLOR COL_POSITIVE.
  SKIP.

  READ REPORT prog INTO code_copy.
  IF sy-subrc = 0.
    WRITE / |New code of the program { prog }:|.
    SKIP.

    SET BLANK LINES ON.
    LOOP AT code_copy INTO DATA(wa).
      WRITE / wa .
    ENDLOOP.
  ELSE.
    WRITE / `An error occured with reading the program.` COLOR COL_NEGATIVE.
  ENDIF.
ELSE.
  WRITE / `An error occured with inserting the source code into the program.` COLOR COL_NEGATIVE.
ENDIF.
```

</details> 
<br>
 </td>
</tr>

</table>

</details>  


<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information
It is recommended that you consult the [Dynamic Programming Techniques (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynamic_prog_technique_gdl.htm) section in the ABAP Keyword Documentation since it provides important aspects that should be considered when dealing with dynamic programming in general (e. g. security aspects or runtime error prevention).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

[zcl_demo_abap_dynamic_prog](./src/zcl_demo_abap_dynamic_prog.clas.abap)

> [!NOTE] 
> - The executable example covers the following topics, among others:
>     - Field symbols and data references as supporting elements for dynamic programming
>     - Dynamic ABAP syntax components
>     - Runtime type services (RTTS), i. e. runtime type identification (RTTI) and runtime type creation (RTTC)
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)
