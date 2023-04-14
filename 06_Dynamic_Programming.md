<a name="top"></a>

# Dynamic Programming

- [Dynamic Programming](#dynamic-programming)
  - [Notes on Dynamic Programming](#notes-on-dynamic-programming)
  - [Excursion: Field Symbols and Data References](#excursion-field-symbols-and-data-references)
    - [Field Symbols](#field-symbols)
    - [Data References](#data-references)
  - [Dynamic ABAP Statements](#dynamic-abap-statements)
  - [Runtime Type Services (RTTS)](#runtime-type-services-rtts)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)

## Notes on Dynamic Programming

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
   - See the following `SELECT` statement. As also shown further down, the `FROM` clause does not include a statically defined table to be selected from. Instead, there is a pair of parentheses including a data object. Assume the data object holds the name of the database table. At runtime, the data retrieval happens from the database table that was inserted in the input field.

        ```abap
        SELECT *
        FROM (dbtab)
        INTO TABLE @DATA(some_itab).
        ```

- Further aspects for dynamic programming in ABAP enter the picture if you want to determine information about data types and data objects at runtime ([RTTI](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm)) or even create them ([RTTC](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_creation_glosry.htm)).

- In general, dynamic programming also comes with some downsides. For example, the ABAP compiler cannot check the dynamic programming feature like the `SELECT` statement mentioned above. There is no syntax warning or suchlike. The checks are performed at runtime only which has an impact on the performance. Plus, the  testing of [procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm "Glossary Entry")
that include dynamic programming features is difficult.


<p align="right">(<a href="#top">back to top</a>)</p>

## Excursion: Field Symbols and Data References

[Field symbols](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm "Glossary Entry")
and [data references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_glosry.htm "Glossary Entry") are dealt with here since they are supporting elements for dynamic programming.

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
FIELD-SYMBOLS <fs_data>      TYPE data.
FIELD-SYMBOLS <fs_any_table> TYPE any table.

"Declaring field symbols inline
"The typing of the field symbol is determined using the statically
"known type of the assigned memory area.
"Prominent use case: Inline declaration of a field symbol for an internal table.
LOOP AT itab ASSIGNING FIELD-SYMBOL(<line>).
  ...
ENDLOOP.
```

> **💡 Note**<br>
>- After its declaration, a field symbol is initial, i. e. a memory area is not (yet) assigned to it (apart from the inline declaration). If you use an unassigned field symbol, an exception is raised.
>-   There are plenty of options for generic ABAP types. A prominent one
    is `data` that stands for any data type (the older generic
    type `any` has the same effect). See more information in the
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
DATA: number TYPE i,
      struc  TYPE zdemo_abap_fli,  "Demo database table
      tab    TYPE string_table.

"Declaring field symbols with complete types
FIELD-SYMBOLS: <fs_i>     TYPE i,
               <fs_struc> TYPE zdemo_abap_fli,
               <fs_tab>   TYPE string_table.

"Declaring field symbols with generic type
FIELD-SYMBOLS <fs_gen> TYPE data.

"Assigning data objects to field symbols
"Note: In this case, the field symbols have an appropriate type.
ASSIGN number TO <fs_i>.
ASSIGN struc  TO <fs_struc>.
ASSIGN tab    TO <fs_tab>.
ASSIGN number TO <fs_gen>.               "Could be any of the data objects

"Inline declaration is possible, too. The type
"is automatically derived.
ASSIGN number TO FIELD-SYMBOL(<fs_inl>).

"You can also assign a particular component of a structure.
"Second component of the structure
ASSIGN COMPONENT 2 OF STRUCTURE struc TO <fs_gen>.

ASSIGN COMPONENT 'CARRID' OF STRUCTURE struc TO <fs_gen>.

"CASTING addition for matching types of data object and field symbol
"when assigning memory areas
TYPES c_len_3 TYPE c LENGTH 3.
DATA(chars) = 'abcdefg'.

FIELD-SYMBOLS <fs1> TYPE c_len_3.

"Implicit casting
ASSIGN chars TO <fs1> CASTING.

FIELD-SYMBOLS <fs2> TYPE data.

"Explicit casting
ASSIGN chars TO <fs2> CASTING TYPE c_len_3.
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

**Field symbols in use**

``` abap
"For example, in assignments
DATA number TYPE i VALUE 1.
FIELD-SYMBOLS <fs_i> TYPE i.
ASSIGN number TO <fs_i>.

<fs_i> = 2.
"The data object 'number' has now the value 2.

"Loops
"Here, field symbols are handy since you can avoid an
"actual copying of the table line to boost performance.
SELECT * FROM zdemo_abap_fli
  INTO TABLE @DATA(itab).

FIELD-SYMBOLS <fs1> LIKE LINE OF itab.

LOOP AT itab ASSIGNING <fs1>.
  <fs1>-carrid = ... "The field symbol represents a line of the table.
  <fs1>-connid = ... "Components are accessed with the component selector.
                     "Here, a new value is assigned.
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

-   are references that point to any data object or to their parts (for example, components, lines of internal tables).
-   are contained in [data reference
    variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry")
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
and [object reference variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm "Glossary Entry") are not part of this cheat sheet. To get more details, refer to the ABAP Keyword Documentation or the cheat sheet [ABAP Object Orientation](04_ABAP_Object_Orientation.md).

**Declaring data reference variables**

``` abap
"Example declarations of data reference variables
"Note that they do not yet point to a data object.
DATA: ref1 TYPE REF TO i,                 "Complete data type
      ref2 TYPE REF TO some_dbtab,        "Complete data type
      ref3 LIKE REF TO some_data_object,
      ref4 TYPE REF TO data.              "Generic data type
```

**Creating data references to existing data objects** using the
[reference operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_operator_glosry.htm "Glossary Entry")
`REF`.
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

"The older syntax GET REFERENCE having the same effect
"should not be used anymore.
"GET REFERENCE OF num INTO ref1.
"GET REFERENCE OF num INTO DATA(ref4).
```

**Creating new data objects at runtime**: You create an [anonymous
data object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm "Glossary Entry")
at runtime by assigning the reference to the data object of a data reference variable. You can use the [instance
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_operator_glosry.htm "Glossary Entry")
[`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm).
The older syntax [`CREATE DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_data.htm)
has the same effect as using the newer instance operator.

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

"CREATE DATA statements (older syntax)
"Unlike above, the older syntax is not commented out. CREATE DATA is used for dynamic 
"type specification as shown further down.
DATA ref4 TYPE REF TO string.
DATA ref5 TYPE REF TO data.

CREATE DATA ref4.

"Note: TYPE ... needed because of generic type data
CREATE DATA ref5 TYPE p LENGTH 6 DECIMALS 2.

CREATE DATA ref5 LIKE ref4.
```

**Assigning existing data references** to other data references. As mentioned above regarding the assignment, note that static types of both data
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
[`CAST`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_cast.htm). In older code, you might see
the operator [`?=`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove_cast.htm).
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

"Casting operator: Older syntax 
"ref5 ?= ref6.
```

**Addressing data references**

Before addressing the content of data objects a data reference points to, you must dereference data reference variables. Use the
[dereferencing operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm "Glossary Entry")
`->*`. To check if dereferncing works, you can use a logical expression with [`IS BOUND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_bound.htm).

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

"This syntax also works but it is less "comfortable".
ref_carr->*-carrname = 'United Airlines'.

"Checking if a data reference variable can be dereferenced.
IF ref_carr IS BOUND.
  ...
ENDIF.

"Explicitly removing a reference
"However, the garbage collector takes care of removing the references
"automatically once the data is not used any more by a reference.
CLEAR ref_carr.
```

**Data references in use**

Some example contexts of using data references are as follows:

**Overwriting data reference variables**:
``` abap
ref = NEW i( 1 ).

"ref is overwritten here because a new object is created
"with a data reference variable already pointing to a data object
ref = NEW i( 2 ).
```

**Retaining data references**:

``` abap
"This snippet shows that three data references are created
"with the same reference variable. Storing them in an internal table
"using the TYPE TABLE OF REF TO prevents the overwriting.

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

**Processing internal tables**:

``` abap
"Similar use case to using field symbols: In a loop across an internal table,
"you can store the content of the line in a data reference variable
"instead of actually copying the content to boost performance.

"Filling an internal table.
SELECT * FROM zdemo_abap_fli
  INTO TABLE @DATA(fli_tab).

LOOP AT fli_tab REFERENCE INTO DATA(ref).

  "A component of the table line might be addressed.
  ref->carrid = ...
  ...
ENDLOOP.
```

**Data reference variables as part of structures and internal tables**:
``` abap
"In contrast to field symbols, data reference variables can be used as
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
> The question might now arise when to actually use either a field symbol
or a data reference variable. It depends on your use case. However, data
reference variables are more powerful as far as their usage options are
concerned, and they better fit into the modern (object-oriented) ABAP
world. Recommended read: [Accessing Data Objects Dynamically (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm "Guideline").

<p align="right">(<a href="#top">back to top</a>)</p>

## Dynamic ABAP Statements

As already dealt with above, there are ABAP statements that support the dynamic specification of syntax elements.
In this context, you can usually use elementary, character-like data objects - the content is usually provided in capital letters - specified within a pair of parentheses. In the `SELECT` list of an ABAP SQL `SELECT` statement, for example, you can use a standard table with a character-like row type. The dynamically specified syntax elements can be included as operands in various. The following code snippets are intended to give you an idea and rough overview. Check the ABAP Keyword Documentation for dynamic syntax options. 

Note that dynamically specifying syntax elements has downsides, too. Consider some erroneous character-like content of such data objects. There is no syntax warning. At runtime, it can lead to  runtime errors. 

- Dynamically specifying components

    ``` abap    
    "SORT statements: Sorting an internal table by dynamically specifying the component
    
    "Populating an internal table
    SELECT *
      FROM zdemo_abap_carr
      INTO TABLE @DATA(itab).

    "Dynamically specifying component name to be sorted for
    "Named data object
    DATA(field_name) = 'CARRNAME'.
    SORT itab BY (field_name).

    "Unnamed data object
    SORT itab BY ('CURRCODE').

    "ASSIGN statements 

    "Declaring a field symbol
    FIELD-SYMBOLS <fs> type any.

    "Dynamically specifying components of structures
    ASSIGN itab[ 1 ]-('CURRCODE') to <fs>.

    DATA(struc) = itab[ 2 ].
    ASSIGN struc-(field_name) to <fs>.

    "Dynamically specifying components of structures that are referenced by
    "a data reference variable
    DATA(dref) = REF #( struc ).
    ASSIGN dref->('URL') to <fs>.

    "Dynamically specifying component of a class/interface
    "In the example, the field symbol is declared inline.
    ASSIGN zdemo_abap_objects_interface=>('CONST_INTF') TO FIELD-SYMBOL(<fs_inl>).
    ```

- Dynamically specifying data types

    ``` abap
    "Anonymous data objects are created using a type determined at runtime.
    "Note that the NEW operator cannot be used here.

    CREATE DATA ref TYPE (some_type).
    CREATE DATA ref TYPE TABLE OF (some_type).

    "Assigning a data object to a field symbol casting a dynamically specified type

    ASSIGN dobj TO <fs> CASTING TYPE (some_type).
    ```

- Dynamically specifying clauses in ABAP SQL statements

    ``` abap
    "This snippet demonstrates a selection of possible dynamic specifications in ABAP SQL SELECT statements.
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

    "Dynamic WHERE clause
    "This is an example for using an internal table with a character-like row type
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
    "- The first 3 examples assume that there are no mandatory parameters defined for the method.
    "- The example covers only static methods. Dynamic method calls for instance methods are also possible.

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
    "- The table (of type abap_parmbind_tab; line type is abap_parmbind) must
    "  be filled and have a line for all non-optional parameters.
    "- Components: name -> formal parameter name
    "              kind -> kind of parameter, e. g. importing
    "              value -> pointer to appropriate actual parameter,
    "                       is of type REF TO data
    "The addition EXCEPTION-TABLE for exceptions is not dealt with here.
    ```

<p align="right">(<a href="#top">back to top</a>)</p>

## Runtime Type Services (RTTS)

[RTTS](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_services_glosry.htm "Glossary Entry")
represent a hierarchy of [type description
classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_class_glosry.htm "Glossary Entry")
containing methods for [Runtime Type Creation
(RTTC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_creation_glosry.htm "Glossary Entry")
and [Runtime Type Identification
(RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm "Glossary Entry").
Using these classes, you can ...

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

The following examples show the retrieval of information. Instead of the
cumbersome extra declaration of data reference variables, you can use
[inline
declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry").
[Method
chaining](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmethod_chaining_glosry.htm "Glossary Entry")
comes in handy, too.
``` abap
"The properties of a type are retrieved using RTTI

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

The following example demonstrates the dynamic creation of data objects.
Note the [`TYPE HANDLE`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcreate_data_handle.htm)
addition as part of the `CREATE DATA` statement that is used when referring to dynamically created data types.

``` abap
"RTTC examples

"Creation of an anonymous data object using a type description object for a
"dictionary structure that is obtained using RTTI

"Declaring a data reference variable with a generic type
DATA dref TYPE REF TO data.

"Getting type description using RTTI
DATA(type) = CAST cl_abap_datadescr(
  cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_CARR' ) ).

"Creating an anonymous data object using the retrieved type description
CREATE DATA dref TYPE HANDLE type.

"Creating an internal table dynamically

"Getting type description using RTTI
DATA(line_type) =  CAST cl_abap_structdescr(
  cl_abap_tabledescr=>describe_by_name( `ZDEMO_ABAP_CARR` ) ).

"Defining primary table keys of internal table type to be created
DATA(itab_keys) = VALUE abap_keydescr_tab( ( name = 'CARRID' )
                                           ( name = 'CARRNAME' ) ).

"Creating internal table type using the create method of cl_abap_tabledescr
DATA(table_type) = cl_abap_tabledescr=>create(
    p_line_type  = line_type
    p_table_kind = cl_abap_tabledescr=>tablekind_sorted
    p_unique     = cl_abap_typedescr=>true
    p_key        = itab_keys ).

"Creating internal table based on the created table type
DATA ref_tab TYPE REF TO data.

CREATE DATA ref_tab TYPE HANDLE table_type.
```

<p align="right">(<a href="#top">back to top</a>)</p>

## More Information
- It is recommended that you also consult section [Dynamic Programming Techniques (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynamic_prog_technique_gdl.htm) in the ABAP Keyword Documentation since it provides important aspects that should be considered when dealing with dynamic programming in general (e. g. security aspects or runtime error prevention).
- There are even further dynamic programming techniques in the unrestricted language scope like the
generation or execution of programs at runtime. They are not part of this cheat sheet. Find more details on the related syntax (e. g. `GENERATE SUBROUTINE POOL`, `READ REPORT` and `INSERT REPORT` in the ABAP Keyword Documentation for Standard ABAP: [Dynamic Program Development (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_language_dynamic.htm)

## Executable Example
[zcl_demo_abap_dynamic_prog](./src/zcl_demo_abap_dynamic_prog.clas.abap)

Note the steps outlined [here](README.md#-getting-started-with-the-examples) about how to import and run the code.