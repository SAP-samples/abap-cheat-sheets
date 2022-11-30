<a name="top"></a>

# Dynamic Programming


- [Dynamic Programming](#dynamic-programming)
  - [Notes on "Dynamic"](#notes-on-dynamic)
  - [Excursion: Field Symbols and Data References](#excursion-field-symbols-and-data-references)
    - [Field Symbols](#field-symbols)
    - [Data References](#data-references)
  - [Dynamic ABAP Statements](#dynamic-abap-statements)
  - [Runtime Type Services (RTTS)](#runtime-type-services-rtts)
  - [Further Information](#further-information)
  - [Executable Example](#executable-example)

## Notes on "Dynamic"

Some considerations regarding "dynamic" in contrast to "static" aspects:

-   ABAP programs can include both dynamic and static parts.
-   Consider a [data
    object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm "Glossary Entry")
    you declare in a program having dedicated technical properties like
    the [data
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_type_glosry.htm "Glossary Entry")
    or the actual name of the data object, i. e. these properties are
    already (statically) known to the program at compile time and they
    do not change throughout the program execution.
-   On the other hand, there can be use cases where these properties
    **are not known or not yet determined at compile time** at all.
-   They are **only known at a program's runtime**, i. e. the
    properties are defined and passed to programs at runtime.
-   Consider a program that does not work with a specific kind of table
    but should be able to work with any kind of table, for example, a
    user must input the table name first in a UI. The tables to be used
    in the program certainly have different properties, line types,
    field names, number of rows, and so on. Nevertheless, the program
    must be able to work with all of them, no matter what table is
    processed.
-   You might also need to determine information about data types and
    data objects at runtime or even create them.

Dynamic programming is a powerful means to make ABAP programs more
flexible and versatile. However, as implied above, dynamic programming
techniques must be handled with care and you must be aware of some
downsides, too. For example:

-   Dynamic features implemented in a program cannot be checked or
    analyzed by the ABAP compiler. The exact data is not known at
    compile time but only when the program is executed which has also an
    impact on performance since the checks must be carried out at
    runtime.
-   Testing
    [procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm "Glossary Entry")
    including dynamic parts is difficult.

<p align="right">(<a href="#top">back to top</a>)</p>

## Excursion: Field Symbols and Data References

[Field
symbols](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm "Glossary Entry")
and [data
references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_glosry.htm "Glossary Entry")
support dynamic programming and working with data objects whose
properties are only known at runtime.

### Field Symbols

Field symbols ...

-   can be considered as alias names for existing data objects.
-   can only be used if they are assigned to a data object first. And if
    assigned, you can access the content of variables via the field
    symbol name.
-   do not consume any space but act as a sort of label for the
    particular memory area that is used by a data object which the field
    symbol is assigned to.
-   can be used in ABAP programs as if working with the actual data
    object.
-   can be statically typed with both [complete data
    types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplete_data_type_glosry.htm "Glossary Entry")
    and [generic data
    types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengeneric_data_type_glosry.htm "Glossary Entry").
-   are especially helpful for accessing and editing data in structures
    or internal tables at runtime without the need to copy the data
    somewhere which boosts performance.

**Declaring field symbols**

Field symbols are declared with the `FIELD-SYMBOLS` statement.
You provide the name of the field symbol between angle brackets. You can
either type them with a complete data type or with a generic type.

> **💡 Note**<br>
>-   There are plenty of options for generic ABAP types. A prominent one
    is `data` that stands for any data type (the older generic
    type `any` has the same effect). See more information in the
    topic [Generic ABAP
    Types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuilt_in_types_generic.htm).
>-   Field symbols cannot be declared in the declaration part of
    [classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_glosry.htm "Glossary Entry")
    and
    [interfaces](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoo_intf_glosry.htm "Glossary Entry").
>-   Untyped field symbols are not supported in object-oriented contexts.
>-   Field symbols can also be [declared
    inline](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield-symbol_inline.htm).

Syntax:
``` abap
"Complete types
FIELD-SYMBOLS: <fs_i>        TYPE i,
               <fs_fli>      TYPE zdemo_abap_fli,
               <fs_tab_type> TYPE LINE OF some_table_type,
               <fs_like>     LIKE some_data_object.

"Generic types
FIELD-SYMBOLS <fs_data>      TYPE data.      "or: TYPE any
FIELD-SYMBOLS <fs_any_table> TYPE any table.
```

**Assigning data objects**

When assigning data objects to field symbols with the
[`ASSIGN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapassign.htm)
statement, field symbols are given all properties and values from the
data objects. In case of completely typed field symbols, you can only
assign data objects that have the same type. Further dynamic aspects
enter the picture with dynamic assignment. This is dealt with further
down.

Syntax:
``` abap
"Data objects.
DATA: number TYPE i,
      struc  TYPE sflight,
      tab    TYPE string_table.

"Field symbols with complete types
FIELD-SYMBOLS: <fs_i>     TYPE i,
               <fs_struc> TYPE sflight,
               <fs_tab>   TYPE string_table.

"Generic type
FIELD-SYMBOLS <fs_gen> TYPE data.

"Assigning data objects to field symbols
ASSIGN number TO <fs_i>.
ASSIGN struc  TO <fs_struc>.
ASSIGN tab    TO <fs_tab>.
ASSIGN number TO <fs_gen>.               "Could be any of the data objects
ASSIGN number TO FIELD-SYMBOL(<fs_inl>). "Field symbol declared inline

"You can also assign a particular component of a structure.
"Second component of the structure
ASSIGN COMPONENT 2 OF STRUCTURE struc TO <fs_gen>.

ASSIGN COMPONENT 'CARRID' OF STRUCTURE struc TO <fs_gen>.
```

> **💡 Note**<br>
> - When working with field symbols, you should make sure that they are assigned. Otherwise, a runtime error occurs. You can check the
    assignment with the following logical expression. The statement is true if the field symbol is assigned.
>    ``` abap
>    IF <fs_i> IS ASSIGNED.
>      ...
>    ENDIF.
>    ```
>- You can explicitly remove the assignment of the field symbol. After this, the field symbol does not point to any data object any more.
    Note that a `CLEAR` statement only initializes the value.
>   ``` abap
>   UNASSIGN <fs_i>.
>   ```
>- When assigning data objects to fields symbols, you should pay attention to compatible types of data object and field symbol. There
    is also an ABAP syntax with which you can carry out type casting for incompatible types. You can cast either implicitly or explicitly by specifying the concrete type. The addition [`TYPE HANDLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapassign_casting&sap-language=EN&sap-client=000&version=X&anchor=!ABAP_ADDITION_5@5@&tree=X)
    is relevant for [Runtime Type Services
    (RTTS)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrtti.htm).
>   ``` abap
>   TYPES c_len_3 TYPE c LENGTH 3.
>   DATA(chars) = 'abcdefg'.
>
>   FIELD-SYMBOLS <fs1> TYPE c_len_3.
>   "Implicit casting
>   ASSIGN chars TO <fs1> CASTING.
>
>   FIELD-SYMBOLS <fs2> TYPE data.
>   "Explicit casting
>   ASSIGN chars TO <fs2> CASTING TYPE c_len_3.
>   ```

**Using field symbols**

When accessing field symbols, you address the memory area of an existing data object. After an assignment, you might assign the data object another value:
``` abap
DATA: number TYPE i VALUE 1.
FIELD-SYMBOLS <fs_i> TYPE i.
ASSIGN number TO <fs_i>.

<fs_i> = 2.
"number has now the value 2
```
As mentioned, field symbols are often used when working with internal tables, for example, in `LOOP` statements. In this context,
field symbols are very handy. You can avoid an actual copying of content to a work area during the loop. In doing so, the loop is considerably faster especially when dealing with large tables. You can assign the field symbol using the `ASSIGNING` addition. With `ASSIGNING FIELD-SYMBOL(...)`, you can make use of a field symbol declared inline and assign the field symbol in one go.

``` abap
SELECT * FROM zdemo_abap_fli
  INTO TABLE @DATA(itab).

FIELD-SYMBOLS <fs1> LIKE LINE OF itab.

LOOP AT itab ASSIGNING <fs1>.
  <fs1>-carrid = ... "The field symbol represents a line of the table.
  <fs1>-connid = ... "Components are accessed with the component selector.
                     "E. g. a new value is assigned.
  ...
ENDLOOP.

"Inline declaration of field symbol
LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs2>).
  <fs2>-carrid = ...
  <fs2>-connid = ...
  ...
ENDLOOP.
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Data References

[Data references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_glosry.htm "Glossary Entry")
...

-   are similar to field symbols but you can do more with them compared
    to field symbols.
-   point to data objects in the memory, i. e. they include the data
    object's address of the memory location.
-   are contained in [data reference
    variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry")
    in ABAP programs.

[Data reference
variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry")
...

- contain values as every other data object. However, the direct value is here a reference (i. e. it points to the memory location of
    another data object) which means you cannot work with the value directly. You must dereference the reference first.
- are, despite only pointing to other data objects, data objects themselves that can, for example, also be used as components in
    structures or columns in internal tables (which is not possible with field symbols).

> **💡 Note**<br>
>-   Data reference variables are considered to be
    [deep](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_glosry.htm "Glossary Entry")
    like strings and internal tables since none of them have an assigned
    dedicated memory area. Internally, strings and internal tables are
    addressed using references.
>-   [Object
    references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_reference_glosry.htm "Glossary Entry")
    and [object reference
    variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm "Glossary Entry")
    are not part of this cheat sheet.  To get more details, refer to the
    ABAP Keyword Documentation or the cheat sheet [ABAP Object Orientation](04_ABAP_Object_Orientation.md).

**Declaring data reference variables**

Like field symbols, data reference variables can be declared with both a complete and a generic data type using `DATA` statements and the
addition `REF TO`. The type after `REF TO` represents the static data type.

When declared, data reference variables do not yet point to a data object.

Examples:
``` abap
DATA: ref1 TYPE REF TO i,                 "Complete data type
      ref2 TYPE REF TO some_dbtab,        "Complete data type
      ref3 LIKE REF TO some_data_object,
      ref4 TYPE REF TO data.              "Generic data type
```

**Assigning data references**

There are multiple options to assign data references:

**Creating data references to existing data objects**: Using the
[reference
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_operator_glosry.htm "Glossary Entry")
`REF`, you can get a data reference to an existing data object.
The older syntax [`GET REFERENCE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapget_reference.htm)
has the same effect as using the newer reference operator but should not
be used anymore.
``` abap
"Declaring a data object

DATA num TYPE i VALUE 5.

"Declaring data reference variables

DATA ref1    TYPE REF TO i.
DATA ref_gen TYPE REF TO data.

"Creating data references to data objects.
"The # sign means that the type is derived from the context.

ref1    = REF #( num ).
ref_gen = REF #( num ).

"You can also use inline declarations to omit the explicit declaration.

DATA(ref2) = REF #( num ).

"You can explicitly specify the data type after REF.

DATA(ref3) = REF string( `hallo` ).

"GET REFERENCE OF; do not use anymore
"GET REFERENCE OF num INTO ref1.
"GET REFERENCE OF num INTO DATA(ref4).
```

**Creating new data objects at runtime**: You create a [anonymous
data
object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm "Glossary Entry")
at runtime by placing the reference in the variable and providing the
desired type. You can use the [instance
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_operator_glosry.htm "Glossary Entry")
[`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm).
The older syntax [`CREATE DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_data.htm)
has the same effect as using the newer instance operator.

Examples:
``` abap
"Declaring data reference variables

DATA ref1    TYPE REF TO i.    "Complete type
DATA ref_gen TYPE REF TO data. "Generic type

"Creating anonymous data objects
"Using the # sign and the explicit type: see REF #( ) above.

ref1    = NEW #( ).
ref_gen = NEW string( ).

"For directly assigning values, insert the values within the parentheses.

ref1 = NEW #( 123 ).

"Using inline declarations to omit a prior declaration of a variable.

DATA(ref2) = NEW i( 456 ).

TYPES i_table TYPE STANDARD TABLE OF i WITH EMPTY KEY.

DATA(ref3) = NEW i_table( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).

"Older syntax.

DATA ref4 TYPE REF TO string.
DATA ref5 TYPE REF TO data.

CREATE DATA ref4.

"Note: TYPE ... needed because of generic type data
CREATE DATA ref5 TYPE p LENGTH 6 DECIMALS 2.

CREATE DATA ref5 LIKE ref4.
```

**Assigning/Copying existing data references**: You can copy a data reference into another one. Note that static types of both data
reference variables must be compatible and that only the reference is copied and not the data object as such. That means that, when copied, both data reference variables point to the same data object.

Notes:
- Data reference variables have both a
[static](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_type_glosry.htm "Glossary Entry")
and a [dynamic
type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendynamic_type_glosry.htm "Glossary Entry").
- When declaring a data reference variable, e. g. `DATA ref TYPE REF TO
i.`, you determine the static type. This type is either a
non-generic (`i` in the example) or a generic type (like
`data` or `any`; e. g. `DATA ref TYPE REF TO
data.`).
- The dynamic type is determined at runtime of the
program and is the data type of a referenced data object. Especially in
the context of assigning data references (and also object references),
this differentiation is relevant.
- The following basic rule applies: The
assignment of a data reference variable to another one is possible if
the static type of the target reference variable is more general than or
the same as the dynamic type of the source reference variable.
- If it can
be statically checked that an assignment is possible, the assignment is
done using the [assignment
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm "Glossary Entry")
`=` that triggers an
[upcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenup_cast_glosry.htm "Glossary Entry")
automatically. Otherwise, it is a
[downcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendown_cast_glosry.htm "Glossary Entry").
Here, the assignability is not checked until runtime. The downcast must
be triggered explicitly using [casting
operators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencasting_operator_glosry.htm "Glossary Entry"),
either with the [constructor
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_operator_glosry.htm "Glossary Entry")
[`CAST`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_cast.htm)
or the older
[`?=`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove_cast.htm),
for the assignment of data reference variables.
- See more information in
the topic [Assignment Rules for Reference
Variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_references.htm).

The following example demonstrates up- and downcasts with the assignment
of data reference variables typed with a complete and generic data type:

Syntax:

``` abap
"Declaring data reference variables

DATA ref1 TYPE REF TO i.

DATA ref2 TYPE REF TO i.

ref1 = NEW #( 789 ).

"Copying data reference
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

"Alternative syntax to the CAST operator
ref5 ?= ref6.
```

**Accessing data references**

The content of data objects a data reference refers to can only be
accessed via dereferencing data reference variables using the
[dereferencing
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm "Glossary Entry")
`->*`.

> **💡 Note**<br>
>-   When dereferencing a data reference variable that has a structured
    data type, you can use the [component
    selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomponent_selector_glosry.htm "Glossary Entry")
    `->` to access individual components.
>-   In older ABAP releases, you could not dereference data reference
    variables typed with a generic type. You had to do an assignment to
    a field symbol first.

Syntax:

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

"Individual component
DATA(carrid) = ref_carr->carrid.
ref_carr->carrid = 'UA'.

"This syntax also works but it is less comfortable.
ref_carr->*-carrname = 'United Airlines'.
```

> **💡 Note**<br>
> - You can check if a data reference can be dereferenced by using a
    logical expression with [`IS BOUND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_bound.htm):
>   ``` abap
>   IF ref IS BOUND.
>     ...
>   ENDIF.
>   ```
>- If you explicitly want to remove a reference from a data reference variable, you can use a `CLEAR` statement. However, the
   [garbage collector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengarbage_collector_glosry.htm "Glossary Entry")
    takes over the reference removal automatically once the data is not used any more by a reference.
>   ``` abap
>   CLEAR ref.
>   ```

**Using data references**

Some contexts of using data references are as follows:

**Overwriting data reference variables**: A data reference variable is overwritten when a new object is created with a data reference
variable already pointing to a data object.
``` abap
ref = NEW i( 1 ).
ref = NEW i( 2 ).
```
**Keeping data references**: If your use case is to retain the data references and you want to prevent that data references are overwritten
when using the same reference variable, you can put the reference variables in internal tables. The following code shows that three data
references are created with the same reference variable.

``` abap
DATA: ref    TYPE REF TO data,
      itab   TYPE TABLE OF REF TO data,
      number TYPE i VALUE 0.

DO 3 TIMES.
  "Adding up 1 to demonstrate a changed data object.
  number += 1.

  "Creating data reference and assigning value.
  "In the course of the loop, the variable gets overwritten.
  ref = NEW i( number ).

  "Adding the reference to itab
  itab = VALUE #( BASE itab ( ref ) ).
ENDDO.
```

**Processing internal tables**: Similar to using field symbols, you can avoid the copying of table rows into a work area, for example, in a
loop using data reference variables and a `REFERENCE INTO` statement. In doing so, the processing of internal tables is much faster
than copying table lines to a work area. In the code snippet, an inline declaration is used in the `LOOP` statement.

``` abap
"Fill an internal table.
SELECT * FROM zdemo_abap_fli
  INTO TABLE @DATA(fli_tab).

LOOP AT fli_tab REFERENCE INTO DATA(ref).

  "A component of the table line might be addressed.
  ref->carrid = ...

  ...
ENDLOOP.
```

**Data reference variables as part of structures and internal tables**: In contrast to field symbols, data reference variables can be used as components of structures or columns in internal tables.
``` abap
"Structure

DATA: BEGIN OF struc,
        num TYPE i,
        ref TYPE REF TO i,
      END OF struc.

"Some value assignment

struc2 = VALUE #( num = 1 ref = NEW #( 2 ) ).

"Internal table

DATA itab LIKE TABLE OF struc WITH EMPTY KEY.

"Some value assignment in the first table line
"assuming the table is filled and a line is available.

itab[ 1 ]-ref->* = 123.
```

> **✔️ Hint**<br>
> The question might now arise when to actually use either a field symbol
or a data reference variable. It depends on your use case. However, data
reference variables are more powerful as far as their usage options are
concerned, and they better fit into the modern (object-oriented) ABAP
world. Recommended read: [Accessing Data Objects
Dynamically](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm "Guideline").

<p align="right">(<a href="#top">back to top</a>)</p>

## Dynamic ABAP Statements

Dynamic aspects come particularly into the picture when considering the
options of dynamic ABAP statements. In this context, you can make use of
[tokens](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentoken_glosry.htm "Glossary Entry")
put within parentheses and included as operands in many ABAP statements
(e. g. `SORT table BY (field_name).`). The content of the token
is character-like and should be provided in capital letters. The content
is determined at runtime, e. g. a user entry in an input field whose
content is then part of an ABAP statement.

Note that especially in this context, static checks are not possible, i.
e. if you have an ABAP statement using such a token, it cannot be
determined at compile time whether the operand that is passed is valid.
This can cause runtime errors.

You can make use of the following dynamic token specification options:

1. **Dynamic specification of data objects and fields**

The names of data objects and fields are determined at runtime.

Examples:
``` abap
"The sorting is done by a field that is determined at runtime.

SORT itab BY (field_name).

"A field symbol is assigned a data object; here, an attribute of a class

ASSIGN class=>(attribute_name) TO FIELD-SYMBOL(<fs>).
```

2. **Dynamic specification of types**

The name of a data or object type is determined at runtime.

Examples:
``` abap
"Anonymous data objects are created using a type determined at runtime.
"Note that the NEW operator cannot be used here!

CREATE DATA ref TYPE (some_type).
CREATE DATA ref TYPE TABLE OF (some_type).

"Assigning a data object to a field symbol casting a type

ASSIGN dobj TO <fs> CASTING TYPE (some_type).

"Assigning a structure component dynamically to a field symbol that is declared inline

DATA struct TYPE zdemo_abap_flsch.

ASSIGN struct-('CARRID') TO FIELD-SYMBOL(<fs>).
```

3. **Dynamic specification of clauses in ABAP SQL statements**

For example, a token that includes the `WHERE` clause conditions in a `SELECT` statement. The token can also be an internal table of a character-like line type.

Examples:
``` abap
"Dynamic SELECT list

DATA(select_list) = `CARRID, CONNID, COUNTRYFR, COUNTRYTO`.

SELECT (select_list)
  FROM zdemo_abap_fli
  INTO TABLE @itab.

"Dynamic FROM clause

DATA(table) = `ZDEMO_ABAP_FLI`.

SELECT *
  FROM (table)
  INTO TABLE @itab.

"Dynamic WHERE clause
DATA(where_clause) = `CARRID = 'LH'`.

SELECT *
  FROM zdemo_abap_fli
  WHERE (where_clause) INTO TABLE @itab.
```

4. **Dynamic specification of [procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm "Glossary Entry")**

Names are specified dynamically, e. g. the names of classes and methods.

Examples:
``` abap
"Dynamic method calls
"Note that these calls require a CALL METHOD statement.

"Method dynamically specified.
CALL METHOD class=>(meth).

"Class dynamically specified.
CALL METHOD (class)=>meth.

"Class and method dynamically specified.
CALL METHOD (class)=>(meth).

"Specifying parameters
CALL METHOD class=>(meth) IMPORTING param = ... .

"Parameters and exceptions can also be specified dynamically in tables.

CALL METHOD class=>(meth) PARAMETER-TABLE ptab.

CALL METHOD class=>(meth) PARAMETER-TABLE ptab EXCEPTION-TABLE etab.
```

Regarding the addition `PARAMETER-TABLE`, you can assign [actual
parameters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenactual_parameter_glosry.htm "Glossary Entry")
to [formal
parameters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenformal_parameter_glosry.htm "Glossary Entry")
dynamically using the table `ptab` that is of type
`ABAP_PARMBIND_TAB`. The table must be filled and have a
line for all non-optional parameters. The line type is
`ABAP_PARMBIND`. The following fields are relevant:

- `name`: The name of the formal parameter.
- `kind`: Specifies the kind of parameter, e. g. importing or exporting parameter. You can make use of the constants defined in class `CL_ABAP_OBJECTDESCR`. Note that if the method signature has an importing parameter, it must be specified as exporting parameter here and vice versa.
- `value`: Specifies a data reference to the actual parameter.

Errors raise catchable exceptions of class `CX_SY_DYN_CALL_ERROR`. Using the addition `EXCEPTION-TABLE` and an internal table of type `ABAP_EXCPBIND_TAB`, you can handle non-[class-based
exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_based_exception_glosry.htm "Glossary Entry").

<p align="right">(<a href="#top">back to top</a>)</p>

## Runtime Type Services (RTTS)

[RTTS](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_services_glosry.htm "Glossary Entry")
represent a hierarchy of [type description
classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_class_glosry.htm "Glossary Entry")
containing methods for [Runtime Type Creation
(RTTC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_creation_glosry.htm "Glossary Entry")
and [Runtime Type Identification
(RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm "Glossary Entry").
Using these classes, you can

-   get type information on data objects, data types or
    [instances](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_glosry.htm "Glossary Entry")
    at runtime.
-   define and create new data types at runtime.

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
for example, to deal with each kind of type. Among them, there are, for
example, structures or tables. Working with this superclass and its
subclasses means making use of
[casts](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencast_glosry.htm "Glossary Entry"),
especially
[downcasts](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendown_cast_glosry.htm "Glossary Entry").
Detailing out all the possibilities for the information retrieval and
type creation is beyond scope. Check the information, options and
various methods that can be used in the class documentation, e. g. using
F2 help information in
[ADT](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenadt_glosry.htm "Glossary Entry"),
for more details.

The following examples show the retrieval of information. Instead of the
cumbersome extra declaration of data reference variables, you can use
[inline
declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry").
[Method
chaining](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmethod_chaining_glosry.htm "Glossary Entry")
comes in handy, too.
``` abap
"The properties of a type are retrieved.

DATA(some_type) = cl_abap_typedescr=>describe_by_data( var ).

"The components of a structure are retrieved.
"Like above, the describe_by_data method is used together with a variable.

DATA(components) = CAST cl_abap_structdescr(
  cl_abap_typedescr=>describe_by_data( some_struc )
      )->components.

"The attributes of a global class are retrieved. In contrast to the
"example above the describe_by_name method is used together with the actual name.

DATA(attributes) = CAST cl_abap_classdescr(
  cl_abap_classdescr=>describe_by_name( 'CL_SOME_CLASS' )
      )->attributes.
```

The following example demonstrates the creation of an internal table
type based on a [DDIC
type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_type_glosry.htm "Glossary Entry").
Furthermore, an internal table is created based on this type. The type
itself is a sorted table (constants can also be used here). Unique keys
are defined in a dedicated table of type
`ABAP_KEYDESCR_TAB` that is part of the
`cl_abap_tabledescr=>create` method call.

Note the [`TYPE HANDLE`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcreate_data_handle.htm)
addition as part of the `CREATE DATA` statement that is used
when referring to dynamically created data types.

``` abap
DATA(line_type) =  CAST cl_abap_structdescr(
  cl_abap_tabledescr=>describe_by_name( `ZDEMO_ABAP_CARR` ) ).

"Defining primary table keys of internal table type to be created

DATA(key_tab) = VALUE abap_keydescr_tab( ( name = 'CARRID' )
                                          ( name = 'CARRNAME' ) ).

"Creating internal table type

DATA(table_type) = cl_abap_tabledescr=>create(
    p_line_type  = line_type
    p_table_kind = cl_abap_tabledescr=>tablekind_sorted
    p_unique     = cl_abap_typedescr=>true
    p_key        = key_tab ).

"Create internal table based on the created table type

DATA ref_tab TYPE REF TO data.

CREATE DATA ref_tab TYPE HANDLE table_type.
```

<p align="right">(<a href="#top">back to top</a>)</p>

## Further Information
- It is recommended that you also consult section [Dynamic Programming Techniques (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynamic_prog_technique_gdl.htm) in the ABAP Keyword Documentation since it provides important aspects that should be considered when dealing with dynamic programming in general (e. g. security aspects or runtime error prevention).
- There are even further dynamic programming techniques in the unrestricted language scope like the
generation or execution of programs at runtime. They are not part of this cheat sheet. Find more details on the related syntax (e. g. `GENERATE SUBROUTINE POOL`, `READ REPORT` and `INSERT REPORT` in the ABAP Keyword Documentation for Standard ABAP: [Dynamic Program Development](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_language_dynamic.htm)

## Executable Example
[zcl_demo_abap_dynamic_prog](./src/zcl_demo_abap_dynamic_prog.clas.abap)

Note the steps outlined [here](README.md#getting-started-with-the-examples) about how to import and run the code.
