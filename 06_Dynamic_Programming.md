<a name="top"></a>

# Dynamic Programming

- [Dynamic Programming](#dynamic-programming)
  - [Introduction](#introduction)
  - [Excursion: Field Symbols and Data References](#excursion-field-symbols-and-data-references)
    - [Field Symbols](#field-symbols)
    - [Data References](#data-references)
  - [Dynamic ABAP Statements](#dynamic-abap-statements)
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

"Examples for generic types
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
    Types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuilt_in_types_generic.htm).
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
DATA: num   TYPE i,
      struc TYPE zdemo_abap_fli,  "Demo database table
      tab   TYPE string_table.

"Declaring field symbols with complete types
FIELD-SYMBOLS: <fs_i>     TYPE i,
               <fs_struc> TYPE zdemo_abap_fli,
               <fs_tab>   TYPE string_table.

"Declaring field symbols with generic type
FIELD-SYMBOLS <fs_gen> TYPE data.

"Assigning data objects to field symbols
"Note: In this case, the field symbols have an appropriate type.
ASSIGN num   TO <fs_i>.
ASSIGN struc TO <fs_struc>.
ASSIGN tab   TO <fs_tab>.
ASSIGN num   TO <fs_gen>.   "Could be any of the data objects

"Inline declaration is possible, too. The type
"is automatically derived.
ASSIGN num TO FIELD-SYMBOL(<fs_inl>).

"CASTING addition for matching types of data object and field
"symbol when assigning memory areas
TYPES c_len_3 TYPE c LENGTH 3.
DATA(chars) = 'abcdefg'.

FIELD-SYMBOLS <fs1> TYPE c_len_3.

"Implicit casting
ASSIGN chars TO <fs1> CASTING.

FIELD-SYMBOLS <fs2> TYPE data.

"Explicit casting
ASSIGN chars TO <fs2> CASTING TYPE c_len_3.

DATA chars_l4 TYPE c LENGTH 4.
ASSIGN chars TO <fs2> CASTING LIKE chars_l4.
```

> **💡 Note**<br>
> - If you use an unassigned field symbol, an exception is raised. Before using it, you can check the
    assignment with the following logical expression. The statement is true if the field symbol is assigned.
>    ``` abap
>    IF <fs> IS ASSIGNED.
>      ...
>    ENDIF.
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

"Inline declaration of a field symbol. It receives a suitable
"type automatically.
LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs2>).
  <fs2>-carrid = ...
  <fs2>-connid = ...
  ...
ENDLOOP.
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
[`CAST`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_cast.htm). In older code, you may see the use of
 [`?=`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove_cast.htm) operator.
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

"Explicitly removing a reference
"However, the garbage collector takes care of removing the references
"automatically once the data is not used any more by a reference.
CLEAR ref_carr.

******************************************************
"Excursion: Generic data references in older ABAP releases

"Non-generic type
DATA ref_int TYPE REF TO i.
ref_int = NEW #( ).
ref_int->* = 123.

"Generic type
DATA ref_generic TYPE REF TO data.
ref_generic = NEW i( ).

"In older ABAP releases, CREATE DATA statements were needed.
CREATE DATA ref_generic TYPE i.

"As mentioned above, the content of anonymous data objects can only be 
"accessed using dereferenced data variables and field symbols.
"The only option to access the variable in older releases was via field symbols.
ASSIGN ref_generic->* TO FIELD-SYMBOL(<fs_generic>).
<fs_generic> = 123.

"An access as the following, as it is possible in modern ABAP, was not possible.
ref_generic->* = 123.

"As shown in the example below, using dereferenced generic references is possible 
"in various places in modern ABAP.
DATA ref_sel TYPE REF TO data.
CREATE DATA ref_sel TYPE TABLE OF zdemo_abap_carr.

SELECT * 
  FROM zdemo_abap_carr
  INTO TABLE @ref_sel->*.
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
"using the TYPE TABLE OF REF TO prevents the overwriting.

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
In this context, you can usually use elementary, character-like data objects - the content is usually provided in capital letters - specified within a pair of parentheses. 
``` abap  
"Named, character-like data object specified within parentheses
"used by an ABAP statement
DATA(field_name) = 'CARRNAME'.
SORT itab BY (field_name).      

"Unnamed, character-like data object specified within parentheses
SORT itab BY ('CURRCODE').
``` 
      
In the `SELECT` list of an ABAP SQL `SELECT` statement, for example, you can use a standard table with a character-like row type. The dynamically specified syntax elements can be included as operands in various ABAP statements. The following code snippets are intended to provide a rough overview. 

Note that dynamically specifying syntax elements has downsides, too. Consider some erroneous character-like content of such data objects. There is no syntax warning. At runtime, it can lead to runtime errors. 

- Dynamic specifications in ...
    - statements for processing internal tables

      ``` abap    
      "SORT: Dynamically specifying component name to be sorted for
      
      "Populating an internal table
      SELECT *
        FROM zdemo_abap_carr
        INTO TABLE @DATA(itab).

      "Named data object specified within parenteses
      DATA(field_name) = 'CARRNAME'.
      SORT itab BY (field_name).      

      "Unnamed data object specified within parenteses
      SORT itab BY ('CURRCODE').

      "READ TABLE: Dynamically specifying keys
      READ TABLE itab INTO DATA(wa) WITH KEY (field_name) = ...

      "MODIFY: Dynamically specifying WHERE conditions
      DATA(condition) = `CARRID = 'LH'`.

      MODIFY itab FROM ... TRANSPORTING ... WHERE (condition).

      "DELETE: Dynamically specifying WHERE conditions
      DELETE itab USING KEY ... WHERE (condition).

      "LOOP: Dynamically specifying keys
      DATA(k) = `SOME_KEY`.

      LOOP AT itab INTO DATA(wa_lo) USING KEY (k).
        ...    
      ENDLOOP.
      ```

    - `ASSIGN` statements

      ``` abap    
      "Dynamically assigning components of structures   

      "Populating a structure
      SELECT SINGLE *
        FROM zdemo_abap_carr
        INTO @DATA(wa).
      
      "Declaring a field symbol
      FIELD-SYMBOLS <fs> type any.

      DATA(comp_name) = 'CARRNAME'. 
      ASSIGN wa-(comp_name) TO <fs>. "named data object
      ASSIGN wa-('CARRID') TO <fs>.  "unnamed data object 
      
      "The statements set sy-subrc value. No exception occurs in 
      "case of an unsuccessful assignment.
      ASSIGN wa-('XYZ') TO <fs>. 
      IF sy-subrc <> 0.
        ...
      ENDIF.

      "Numeric expressions are possible. Its value is interpreted 
      "as the position of the component in the structure.
      ASSIGN wa-(4) TO <fs>.

      "If the value is 0, the memory area of the entire structure is 
      "assigned to the field symbol.
      ASSIGN wa-(0) TO <fs>.

      "The statements above replace the following, older statements. 
      ASSIGN COMPONENT 'CARRID' OF STRUCTURE wa TO <fs>.
      ASSIGN COMPONENT 5 OF STRUCTURE wa TO <fs>.

      "Populating a structure that is referenced by a data reference 
      "variable
      SELECT SINGLE *
        FROM zdemo_abap_carr
        INTO NEW @DATA(ref_struc).

      "Note the object component selector. The field symbol is created 
      "inline here.
      ASSIGN ref_struc->('CARRNAME') TO FIELD-SYMBOL(<fs_inl>). 

      *************************************************************

      "Dynamically specifying attributes of classes/interfaces
      DATA(cl_name) = 'CL_SOME_CLASS'.
      DATA(dobj) = 'SOME_DOBJ'.

      ASSIGN cl_some_class=>(dobj) TO <fs>.
      ASSIGN (cl_name)=>some_dobj TO <fs>.
      ASSIGN (cl_name)=>(dobj) TO <fs>.

      "Class reference variable pointing to an object that contains 
      "attributes and that are specified dynamically.
      DATA cl_ref TYPE REF TO cl_some_class.
      cl_ref = NEW #( ).
      ASSIGN cl_ref->(SOME_ATTRIBUTE') TO FIELD-SYMBOL(<another_fs>).

      "If ELSE UNASSIGN is specified, no memory area is assigned to 
      "the field symbol. It has the state unassigned after the ASSIGN 
      "statement.
      ASSIGN cl_ref->('SOME_ATTRIBUTE') TO FIELD-SYMBOL(<attr>) ELSE UNASSIGN.
      ```

- Dynamically specifying data types/creating (data) objects

  > **💡 Note**<br>
  > - For dynamic syntax elements in `CREATE OBJECT` statements, find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_object_explicit.htm) (note that parameters can be specified dynamically, too).
  > - In addition to character-like data objects for the type name specified in the parentheses, you can also use [absolute type names](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabsolute_typename_glosry.htm) (see the information about RTTI below).

    ``` abap
    "Anonymous data objects are created using a type determined at 
    "runtime. See more information below. Note that the NEW operator 
    "cannot be used here.
    CREATE DATA dref TYPE (some_type).
    CREATE DATA dref TYPE TABLE OF (some_type).
    CREATE DATA dref TYPE REF TO (some_type).

    "Assigning a data object to a field symbol casting a dynamically 
    "specified type
    ASSIGN dobj TO <fs> CASTING TYPE (some_type).

    "Dynamically creating an object as an instance of a class and 
    "assigning the reference to the object to an object reference 
    "variable. oref can be an object or interface reference variable.
    "The reference variable is created here with the generic 'object'.
    DATA oref TYPE REF TO object. 
    CREATE OBJECT oref TYPE (some_class).

    "Note: As covered further down and in the executable example,
    "CREATE DATA/OBJECT and ASSIGN statements have the HANDLE addition
    "after which dynamically created types can be specified. A type 
    "description object is expected.
    CREATE DATA dref TYPE HANDLE type_descr_obj.
    CREATE OBJECT oref TYPE HANDLE type_descr_obj.
    ASSIGN dobj TO <fs> CASTING TYPE HANDLE type_descr_obj.
    ```

- Dynamically specifying clauses in ABAP SQL statements

    ``` abap
    "This snippet demonstrates a selection of possible dynamic 
    "specifications in ABAP SQL SELECT statements.
    "Dynamic SELECT list

    DATA(select_list) = `CARRID, CONNID, COUNTRYFR, COUNTRYTO`.

    SELECT (select_list)
      FROM zdemo_abap_fli
      INTO TABLE ...

    "Dynamic FROM clause

    DATA(table) = `ZDEMO_ABAP_FLI`.

    SELECT *
      FROM (table)
      INTO TABLE ...

    "Excursion: Dynamic FROM clause and compatible target data object
    DATA(db_table) = `ZDEMO_ABAP_CARR`.

    DATA itab TYPE REF TO data.
    CREATE DATA itab TYPE TABLE OF (db_table).

    SELECT * 
      FROM (db_table) 
      INTO TABLE @itab->*.

    "Similar to the NEW operator, you can use the addition NEW here 
    "to create an anonymous data object in place. The advantage is 
    "that the data type is constructed in a suitable way.
    SELECT *
      FROM (db_table)
      INTO TABLE NEW @DATA(dref_tab).

    "Dynamic WHERE clause
    "This is an example for using an internal table with a 
    "character-like row type
    DATA(where_clause) = VALUE string_table( ( `CARRID = 'LH'` )
                                             ( `OR CARRID = 'AA'` ) ).

    SELECT *
      FROM zdemo_abap_fli
      WHERE (where_clause)
      INTO TABLE ...
    ```

- Dynamic invoke: Dynamically specifying [procedure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm "Glossary Entry") calls

    ``` abap
    "Notes:
    "- Dynamic method calls require a CALL METHOD statement.
    "- The first 3 examples assume that there are no mandatory 
    "  parameters defined for the method.
    "- The example covers only static methods. Dynamic method calls 
    "  for instance methods are also possible.

    "Method dynamically specified
    CALL METHOD class=>(meth).

    "Class dynamically specified
    CALL METHOD (class)=>meth.

    "Class and method dynamically specified
    CALL METHOD (class)=>(meth).

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

Excursion: Checking the validity of dynamic specifications

The following example uses the `CL_ABAP_DYN_PRG` class, which supports dynamic programming by checking the validity of dynamic specifications.
See the class documentation for more information. There are several methods that can be used for different use cases. In the example below, a method is used to check the database table name. To do this, the database table name is provided (`val` parameter), as well as the package in which it should be included (`packages` parameter). You can pass the `packages` formal parameter a table containing the names of packages in which the specified table should be included. Assuming you provide incorrect input for the table name, or the table is not contained in the specified packages, you can expect an exception to be raied.

```abap
DATA dbtab TYPE string.
TRY.
    dbtab = cl_abap_dyn_prg=>check_table_name_tab( 
      val      = `ZDEMO_ABAP_FLI`
      packages = VALUE #( ( `TEST_ABAP_CHEAT_SHEETS` ) ) ).

    SELECT SINGLE * FROM (dbtab) INTO NEW @DATA(ref_wa).
  CATCH cx_abap_not_a_table cx_abap_not_in_package INTO DATA(err).
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

The following examples show the retrieval of type information. Instead of the
cumbersome extra declaration of data reference variables, you can use
[inline declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry").
[Method chaining](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmethod_chaining_glosry.htm "Glossary Entry")
comes in handy, too.

```abap
"Getting a reference to a type description object of a type.
"i.e. getting an instance of a type description class.
"To do so, the static methods of the class CL_ABAP_TYPEDESCR can be used.
"As shown below, the type decription object can be used to create data
"objects dynamically. Note that instances of classes are not covered.

"Type for which information should be retrieved
TYPES elem_type TYPE c LENGTH 5.

"Creating a data reference variable to hold the reference to
"the type description object
DATA type_descr_obj_elem TYPE REF TO cl_abap_elemdescr.

"Retrieving type information by creating an instance of a type description
"class. As the name implies, the describe_by_name method expects the name
"of the type. In the following example, the reference to the type object is
"assigned using a downcast to the reference variable of type CL_ABAP_ELEMDESCR
"created above.
type_descr_obj_elem = CAST #( cl_abap_typedescr=>describe_by_name( 'ELEM_TYPE' ) ).

"Using the older cast operator ?=
type_descr_obj_elem ?= cl_abap_typedescr=>describe_by_name( 'ELEM_TYPE' ).

"Inline declaration is handy to avoid helper variables.
DATA(type_descr_obj_inl_1) = CAST cl_abap_elemdescr(
  cl_abap_typedescr=>describe_by_name( 'ELEM_TYPE' ) ).

"You may want to check the type description object in the debugger.
"Various methods/attributes provide you with detailed information.
"The following examples show a selection:
"Kind/Type kind/Output length
DATA(kind) = type_descr_obj_inl_1->kind. "E (elementary)
DATA(type_kind) = type_descr_obj_inl_1->type_kind. "C
DATA(output_length) = type_descr_obj_inl_1->output_length. "5

"In the following example, the type properties are retrieved
"without casting. The data object has the type ref to
"cl_abap_typedescr. See the hierarchy tree above.
"The reference in the type description object references an
"object from one of the classes CL_ABAP_ELEMDESCR, CL_ABAP_ENUMDESCR,
"CL_ABAP_REFDESCR, CL_ABAP_STRUCTDESCR, CL_ABAP_TABLEDSECR,
"CL_ABAP_CLASSDESCR, or CL_ABAP_INTFDESCR.
"In the following case, it is CL_ABAP_ELEMDESCR.
"Note that in most of the following examples, the explicit
"casting is included when retrieving a reference to the type
"description object.
TYPES another_elem_type TYPE n LENGTH 3.
DATA(type_descr_obj_inl_2) = cl_abap_typedescr=>describe_by_name( 'ANOTHER_ELEM_TYPE' ).

"More types
"Structured data type (here, using the name of a database table)
DATA(type_descr_obj_struc) = CAST cl_abap_structdescr(
  cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_CARR' ) ).

"Various attributes/methods available for detailed information
"Kind
DATA(struc_kind) = type_descr_obj_struc->kind. "S

"Getting components of the structure (e.g. the component names
"and type description objects for the individual components)
DATA(comps_struc) = type_descr_obj_struc->get_components( ).

"The follwing attribute also lists the component names and types
"(but not the type desription objects as is the case above)
DATA(comps_struc2) = type_descr_obj_struc->components.

"Kind of structure
DATA(struct_kind) = type_descr_obj_struc->struct_kind. "F (flat)

"Internal table type
TYPES tab_type TYPE SORTED TABLE OF zdemo_abap_carr
  WITH UNIQUE KEY carrid.

DATA(type_descr_obj_tab) = CAST cl_abap_tabledescr(
  cl_abap_typedescr=>describe_by_name( 'TAB_TYPE' ) ).

"Kind
DATA(tab_kind) = type_descr_obj_tab->kind. "T

"The following method returns more information than the attribute
"below (e.g. key kind, i.e. if it is a unique key, etc.)
DATA(tab_keys) = type_descr_obj_tab->get_keys( ).
DATA(tab_keys2) = type_descr_obj_tab->key.

"Getting internal table components
"The method get_table_line_type returns a variable of type ref to
"cl_abap_datadescr. This way you can retrieve the table components.
"Method chaining is useful here.
DATA(tab_comps) = CAST cl_abap_structdescr(
  type_descr_obj_tab->get_table_line_type( ) )->get_components( ).

"Reference type
TYPES ref_str TYPE REF TO string.
DATA(type_descr_obj_ref) = CAST cl_abap_refdescr(
  cl_abap_typedescr=>describe_by_name( 'REF_STR' ) ).

"Returns the type description object of the referenced type
DATA(ref_type) = type_descr_obj_ref->get_referenced_type( ).

"Getting the absolute type name
DATA(ref_type_abs_name) =
  type_descr_obj_ref->get_referenced_type( )->absolute_name. "\TYPE=STRING

"Kind/Type kind
DATA(ref_kind) = type_descr_obj_ref->kind. "R
DATA(ref_type_type_kind) =
  type_descr_obj_ref->get_referenced_type( )->type_kind. "g (string)

"Getting a reference to a type description object of an existing
"data object. Instead of referring to the name of a type, referring
"to a data object here. The relevant method is describe_by_data.

"Elementary data object
DATA dobj_elem type i.
DATA(ty_des_obj_el) = CAST cl_abap_elemdescr(
  cl_abap_typedescr=>describe_by_data( dobj_elem ) ).

"Structure
DATA dobj_struc type zdemo_abap_carr.
DATA(ty_des_obj_struc) = CAST cl_abap_structdescr(
  cl_abap_typedescr=>describe_by_data( dobj_struc ) ).

"Internal table
DATA dobj_itab type table of zdemo_abap_carr with empty key.
DATA(ty_des_obj_itab) = CAST cl_abap_tabledescr(
  cl_abap_typedescr=>describe_by_data( dobj_itab ) ).

"Reference variable
DATA dref_var type ref to string.
DATA(ty_des_obj_dref) = CAST cl_abap_refdescr(
  cl_abap_typedescr=>describe_by_data( dref_var ) ).
```

Excursions: 

```abap
"Casting and method chaining as above in contrast to the following 
"extra declarations. If the variables were not declared inline as 
"in the example, there would be even more lines of code.
DATA(a) = cl_abap_typedescr=>describe_by_data( some_struc ).
DATA(b) = CAST cl_abap_structdescr( a ).
DATA(c) = b->components.  

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

The focus here is on using RTTC methods such as `get...`.

```abap
"Creating type description objects using ...
"... elementary data types
"Conceptually, all elementary, built-in ABAP types already 
"exist and can be accessed by the corresponding get_* methods.
"In ADT, click CTRL + space after cl_abap_elemdescr=>... 
"to check out the options. The following examples show a 
"selection.

DATA(tdo_elem_i) = cl_abap_elemdescr=>get_i( ).
DATA(tdo_elem_string) = cl_abap_elemdescr=>get_string( ).

"For the length specification of type c and others, there is 
"an importing parameter available.
DATA(tdo_elem_c_l20) = cl_abap_elemdescr=>get_c( 10 ).

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

"... structured data types
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

"... internal table types
"Note: Specifying the line type is mandatory, the rest is optional.

"An internal table type such as the following shall be created dynamically
"using a type description object.
TYPES std_tab_type_std_key TYPE STANDARD TABLE OF string 
  WITH DEFAULT KEY.

"Creating a type description object using RTTC method
"Not specifying the other optional parameters means that the
"default values are used, for example, standard table is the
"default value for p_table_kind.
DATA(tdo_tab_1) = cl_abap_tabledescr=>get(
        p_line_type  = cl_abap_elemdescr=>get_string( ) ).

"Another internal table type for which more parameter specifications 
"are needed. The following internal table type shall be created using 
"a type description object.
TYPES so_table_type TYPE SORTED TABLE OF zdemo_abap_flsch 
  WITH UNIQUE KEY carrid connid.

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
DATA(tdo_elem_c_l20) = cl_abap_elemdescr=>get_c( 10 ).
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
- There are even further dynamic programming techniques in the unrestricted language scope such as the
generation or execution of programs at runtime. They are not part of this cheat sheet. Find more details on the related syntax (e. g. `GENERATE SUBROUTINE POOL`, `READ REPORT` and `INSERT REPORT` in the ABAP Keyword Documentation for Standard ABAP: [Dynamic Program Development (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_language_dynamic.htm)

## Executable Example

[zcl_demo_abap_dynamic_prog](./src/zcl_demo_abap_dynamic_prog.clas.abap)

Note the steps outlined [here](README.md#-getting-started-with-the-examples) about how to import and run the code.