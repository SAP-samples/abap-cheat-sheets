<a name="top"></a>

# Constructor Expressions

- [Constructor Expressions](#constructor-expressions)
  - [Introduction](#introduction)
  - [VALUE](#value)
  - [CORRESPONDING](#corresponding)
  - [NEW](#new)
  - [CONV](#conv)
  - [EXACT](#exact)
  - [REF](#ref)
  - [CAST](#cast)
  - [COND](#cond)
  - [SWITCH](#switch)
  - [FILTER](#filter)
  - [REDUCE](#reduce)
  - [Iteration Expressions with FOR](#iteration-expressions-with-for)
  - [LET Expressions](#let-expressions)
  - [Executable Example](#executable-example)

## Introduction

-   [Constructor
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_glosry.htm "Glossary Entry")
    include a [constructor
    operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_operator_glosry.htm "Glossary Entry")
    followed by the specification of a [data
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_type_glosry.htm "Glossary Entry")
    or [object
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_type_glosry.htm "Glossary Entry")
    (or a `#` character that stands for such a type) and
    specific parameters specified within parentheses. Example using the
    `VALUE` operator:

    ``` abap
    ... VALUE string( ... ) ...
    ... VALUE #( ... ) ...
    ```

-   As the name implies, these expressions construct results of a
    specific type and their content. Either the type is specified
    explicitly before the first parenthesis or the said `#`
    character can be specified if the type can be derived implicitly
    from the [operand
    position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm "Glossary Entry").
    The `#` character symbolizes the [operand
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_type_glosry.htm "Glossary Entry").
    If no type can be derived from the operand position, for some
    constructor operators, the type can also be derived from the
    arguments in the parentheses.
-   Why use them? Constructor expressions can make your code leaner and
    more readable since you can achieve the same with fewer statements.
-   Apart from the concept of deriving types from the context, another
    concept is very handy particularly in this context: [Inline
    declaration](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry").
    -   This means that you can declare a variable using
        `DATA(var)` (or an immutable variable
        [`FINAL(var)`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm))
        as an operand in the current [write
        position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwrite_position_glosry.htm "Glossary Entry").
        In doing so, such a variable declared inline can be given the
        appropriate type and result of the constructor expression in one
        go: `DATA(dec) = VALUE decfloat34( '1.23' )`.

> **✔️ Hint**<br>
> The construction of a result, i. e. a target [data
object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm "Glossary Entry"),
implies that the data object is initialized. However, for some
constructor operators, there is an addition with which the
initialization can be avoided.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## VALUE

-   Expressions with the
    [`VALUE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_value.htm)
    operator construct a result in place based on a data type.
-   This result can be initial values for any non-generic data types,
    structures or internal tables.
> **💡 Note**<br>
> Elementary data types and reference types cannot be
    explicitly specified for the construction of values here.
-   Regarding the type specifications before and parameters within the
    parentheses:
    -   No parameter specified within the parentheses: The return value
        is set to its type-specific initial value. This is possible for
        any non-generic data types. See more information
        [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenvalue_constructor_params_init.htm).
    -   Structured and internal table type before the parentheses or
        `#` stands for such types: Individual components of
        structures can be specified as named arguments while each
        component of the return value can be assigned a data object that
        has the same data type as the component, or whose data type can
        be converted to this data type. See more information
        [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenvalue_constructor_params_struc.htm).
        To construct internal tables, you have multiple options, for
        example, you can add individual table lines using an inner pair
        of parentheses. More syntax options, for example, using the
        additions `BASE` and `FOR` are possible, too.
        See more information
        [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenvalue_constructor_params_itab.htm).

Example: Structure

``` abap
"Creating a structured type
TYPES: BEGIN OF struc_type,
         a TYPE i,
         b TYPE c LENGTH 3,
       END OF struc_type.

DATA struc TYPE struc_type. "Structured data object

struc = VALUE #( a = 1 b = 'aaa' ). "Deriving the type using #
```


As mentioned above, the concept of [inline
declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declarations.htm)
enters the picture here, which simplifies ABAP programming. You can
construct a new data object (for example, using `DATA(...)`),
provide the desired type with the constructor expression and assign
values in one go.

``` abap
"Explicit type specification needed
DATA(structure) = VALUE struc_type( a = 2 b = 'bbb' ).
```

Note that initial values can be created by omitting the specification of
components or by providing no content within the parentheses.

``` abap
"Component b not specified, b remains initial
struc = VALUE #( a = 2 ).

"Explicit setting of initial value for a component
struc = VALUE #( a = 1 b = value #( ) ).

"The whole structure is initial
struc = VALUE #( ).

"Creating initial values for an elementary data type
DATA num1 TYPE i.

num1 = VALUE #( ).

"Inline declaration
DATA(num2) = VALUE i( ).
```

Regarding internal tables, the line specifications are enclosed in an
inner pair of parentheses `( ... )`. In the following example,
three lines are added to an internal table.

``` abap
"Creating an internal table type and an internal table
TYPES tab_type TYPE TABLE OF struc_type WITH EMPTY KEY.
DATA itab TYPE tab_type.

"Filling the internal table using the VALUE operator with #
itab = VALUE #( ( a = 1 b = 'aaa' )
                ( a = 2 b = 'bbb' )
                ( a = 3 b = 'ccc' ) ).

"Internal table declared inline, explicit type specification
DATA(itab2) = VALUE tab_type( ( a = 1 b = 'aaa' )
                              ( a = 2 b = 'bbb' )
                              ( a = 3 b = 'ccc' ) ).

"Unstructured line types work without component names.
"Here, the internal table type is a string table.
DATA(itab3) = VALUE string_table( ( `abc` ) ( `def` ) ( `ghi` ) ).
```

In case of
[deep](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_structure_glosry.htm "Glossary Entry")
and [nested
structures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennested_structure_glosry.htm "Glossary Entry")
or [deep
tables](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendeep_table_glosry.htm "Glossary Entry"),
the use of `VALUE` expressions is handy. The following example
demonstrates a nested structure.
``` abap
"Creating a nested structure
DATA: BEGIN OF nested_struc,
        a TYPE i,
        BEGIN OF struct,
          b TYPE i,
          c TYPE c LENGTH 3,
        END OF struct,
      END OF nested_struc.

"Filling the deep structure
nested_struc = VALUE #( a = 1 struct = VALUE #( b = 2 c = 'abc' ) ).
```

`BASE` addition: A constructor expression without the
`BASE` addition initializes the target variable. Hence, you can
use the addition if you do not want to construct a structure or internal
table from scratch but keep existing content.

``` abap
"Filling structure
struc = VALUE #( a = 1 b = 'aaa' ).

"struc is not initialized, only component b is modified, value of a is kept
struc = VALUE #( BASE struc b = 'bbb' ).

"Filling internal table with two lines
itab = VALUE #( ( a = 1 b = 'aaa' )
                ( a = 2 b = 'bbb' ) ).

"Two more lines are added instead of initializing the internal table
itab = VALUE #( BASE itab
                ( a = 3 b = 'ccc' )
                ( a = 4 b = 'ddd' ) ).
```

`LINES OF` addition: All or some lines of another table can be included in the target internal table (provided that they have
appropriate line types):
``` abap
itab = VALUE #( ( a = 1 b = 'aaa' )
                ( a = 2 b = 'bbb' )
                ( LINES OF itab2 )    "All lines of itab2
                ( LINES OF itab3 FROM 2 TO 5 ) ).  "Specific lines of itab3
```

Using the inline construction of structures and internal tables, you can
avoid the declaration of extra variables in many contexts, for example,
ABAP statements like
[`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_itab.htm)
for modifying internal tables or [ABAP
SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_glosry.htm "Glossary Entry")
statements like
[`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_dbtab.htm)
(which is not to be confused with the ABAP statement having the same
name) for modifying database tables.

Examples:
``` abap
"ABAP statements
"Modifiying individual internal table entries based on a structure created inline

"Modifying a table line
MODIFY TABLE some_itab FROM VALUE #( a = 1 ... ).

"Inserting a table line
INSERT VALUE #( a = 2 ... ) INTO TABLE some_itab.

"Deleting a table line
DELETE TABLE some_itab FROM VALUE #( a = 3 ).

"ABAP SQL statement
"Modifying multiple database table entries based on an internal table
"constructed inline within a host expression
MODIFY zdemo_abap_carr FROM TABLE @( VALUE #(
            ( carrid = 'XY'
              carrname = 'XY Airlines'
              currcode = 'USD'
              url =  'some_url' )
            ( carrid = 'ZZ'
              carrname = 'ZZ Airways'
              currcode = 'EUR'
              url =  'some_url' ) ) ).
```

> **💡 Note**<br>
> Some of the additions and concepts mentioned here are
also valid for other constructor expressions further down but not
necessarily mentioned explicitly. See the details on the syntactical
options of the constructor operators in the ABAP Keyword Documentation.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## CORRESPONDING

-   Expressions with the
    [`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm)
    operator construct structures and internal tables based on a data
    type (i. e. a [table
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_type_glosry.htm "Glossary Entry")
    or [structured
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstructured_type_glosry.htm "Glossary Entry")).
-   The components or columns of the target data object are filled using
    assignments of the parameters specified within the parentheses.
-   The assignments are made using identical names or based on [mapping
    relationships](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencorresponding_constr_mapping.htm)
-   Note: Pay attention to the [assignment and conversion
    rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rules.htm)
    to avoid errors when using the operator. Consider, for example, the
    impact of assigning the values of identically named fields having
    different types (e. g. one field is of type `c` and another
    field is of type `string`).

The following table includes a selection of various possible additions to
this constructor operator. There are more variants available like the
addition `EXACT`, using a lookup table, the option of discarding
duplicates or
[RAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_glosry.htm "Glossary Entry")-specific
variants that are not part of this cheat sheet. Find the details in
[this
topic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm).

| Addition  | Details |
|---|---|
| `BASE` | Keeps original values. Unlike, for example, the operator `VALUE`, a pair of parentheses must be set around `BASE`.  |
| `MAPPING`  | Enables the mapping of component names, i. e. a component of a source structure or source table can be assigned to a differently named component of a target structure or target table (e. g. `MAPPING c1 = c2`).  |
| `EXCEPT`  | You can specify components that should not be assigned content in the target data object. They remain initial. In doing so, you exclude identically named components in the source and target object that are not compatible or convertible from the assignment to avoid syntax errors or runtime errors.  |
| `DEEP`  | Relevant for deep tabular components. They are resolved at every hierarchy level and identically named components are assigned line by line.  |
| `[DEEP] APPENDING`  | Relevant for (deep) tabular components. It ensures that the nested target tables are not deleted. The effect without `DEEP` is that lines of the nested source table are added using `CORRESPONDING` without addition. The effect with `DEEP` is that lines of the nested source table are added using `CORRESPONDING` with the addition `DEEP`.  |

See the executable example for demonstrating the effect of the variants:
``` abap
"Assignment of a structure/internal table to another one having a different type
struc2 = CORRESPONDING #( struc1 ).

tab2 = CORRESPONDING #( tab1 ).

"BASE keeps original content, does not initialize the target
struc2 = CORRESPONDING #( BASE ( struc2 ) struc1 ).

tab2 = CORRESPONDING #( BASE ( tab2 ) tab1 ).

"MAPPING/EXACT are used for mapping/excluding components in the assignment
struc2 = CORRESPONDING #( struc1 MAPPING comp1 = comp2 ).

tab2 = CORRESPONDING #( tab1 EXCEPT comp1 ).

"Complex assignments with deep components using further additions
st_deep2 = CORRESPONDING #( DEEP st_deep1 ).

st_deep2 = CORRESPONDING #( DEEP BASE ( st_deep2 ) st_deep1 ).

st_deep2 = CORRESPONDING #( APPENDING ( st_deep2 ) st_deep1 ).

st_deep2 = CORRESPONDING #( DEEP APPENDING ( st_deep2 ) st_deep1 ).
```
> **✔️ Hint**<br>
> `CORRESPONDING` operator versus
[`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove-corresponding.htm):
Although the functionality is the same, note that, as the name implies,
constructor operators construct and - without the addition
`BASE` - target objects are initialized. Hence, the following
two statements are  not the same:
>``` abap
>struc2 = CORRESPONDING #( struc1 ).
>
>"Not matching components are not initialized
>MOVE-CORRESPONDING struc1 TO struc2.
>```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## NEW

-   Using the instance operator
    [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm),
    you can create [anonymous data
    objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm "Glossary Entry")
    or
    [instances](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_glosry.htm "Glossary Entry")
    of a class and also assign values to the new object. As a result,
    you get a [reference
    variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_variable_glosry.htm "Glossary Entry")
    that points to the created object. In doing so, the operator
    basically replaces [`CREATE DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_data.htm)
    and [`CREATE OBJECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_object.htm).
-   For the type specification preceding the parentheses, you can use
    -   non-generic data types which creates a [data reference
        variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry")
        pointing to the anonymous data object.
    -   classes which creates objects of these classes. The result is an
        [object reference
        variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm "Glossary Entry")
        pointing to an object.
-   Regarding the created object reference variables, you can use the
    [object component
    selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_component_select_glosry.htm "Glossary Entry")
    `->` in certain contexts to ...
    -   point to a class attribute: `... NEW class( ... )->attr`
    -   introduce
        [standalone](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_method_static_short.htm)
        and
        [functional](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_method_functional.htm)
        method calls, including [chained method
        calls](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenchained_method_call_glosry.htm "Glossary Entry")
        which is a big advantage because you do not need to declare an
        extra variable: `... NEW class( ... )->meth( ... ) ...`
-   Regarding the type specifications before and parameters within the
    parentheses:
    -   No parameter specified within the parentheses: An anonymous data
        object retains its type-specific initial value. In case of
        classes, no parameter specification means that no values are
        passed to the instance constructor of an object. However, in
        case of mandatory [input
        parameters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninput_parameter_glosry.htm "Glossary Entry"),
        the parameters must be specified.
    -   Single parameter specified: If the type specified before the
        parentheses is a non-generic elementary, structured, table, or a
        reference type (or such a type can be derived using
        `#`), a single data object can be specified as an
        unnamed argument. Note the [assignment
        rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rules.htm)
        regarding the value assignments within the parentheses and that
        a constructor expression itself can be specified there.
    -   Structures and internal tables specified: If the type specified
        before the parentheses is a structured data type or `#`
        stands for it, you can specify the individual components as
        named arguments (`comp1 = 1 comp2 = 2 ...`; see more
        information
        [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennew_constructor_params_struct.htm)).
        For the construction of anonymous internal tables, multiple
        options are available. Among them, there is the use of
        `LET` and `FOR` expressions and others. See more
        details
        [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennew_constructor_params_itab.htm).
    -   Classes: As mentioned, non-optional input parameters of the
        instance constructor of the instantiated class must be filled.
        No parameters are passed for a class without an explicit
        instance constructor. See more information:
        [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennew_constructor_params_class.htm).

Examples:
``` abap
"Data references
"Declaring data reference variables
DATA dref1 TYPE REF TO i.    "Complete type
DATA dref2 TYPE REF TO data. "Generic type

"Creating anonymous data objects
"Here, no parameters are specified within the parentheses meaning the
"data objects retain their initial values.
dref1 = NEW #( ).
dref2 = NEW string( ).

"Assigning single values; specified as unnamed data objects
dref1 = NEW #( 123 ).
dref2 = NEW string( `hallo` ).

"Using inline declarations to omit a prior declaration of a variable
DATA(dref3) = NEW i( 456 ).

DATA text TYPE string VALUE `world`.

"Another constructor expression specified within the parentheses
dref2 = NEW string( `Hello ` && text && CONV string( '!' ) ).

DATA dref4 TYPE REF TO string_table.
dref4 = NEW #( VALUE string_table( ( `a` ) ( `b` ) ) ).

"Structured type; named arguments within the parentheses
DATA(dref5) = NEW scarr( carrid = 'AA' carrname = 'American Airlines' ).

"Object references
"Declaring object reference variables
DATA oref1 TYPE REF TO cl1. "Assumption: class without constructor implementation
DATA oref2 TYPE REF TO cl2. "Assumption: class with constructor implementation

"Creating instances of classes
oref1 = NEW #( ).

"Listing the parameter bindings for the constructor method
"If there is only one parameter, the explicit specification of the
"parameter name is not needed and the value can be specified directly
oref2 = NEW #( p1 = ... p2 = ... ).

"Using inline declaration
DATA(oref3) = NEW cl2( p1 = ... p2 = ... ).

"Method chaining
... NEW some_class( ... )->meth( ... ).

"Chained attribute access
... NEW some_class( ... )->attr ...
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## CONV

-   The
    [`CONV`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_conv.htm)
    operator enforces conversions from one type to another and creates
    an appropriate result.
-   Note that the conversion is carried out according to [conversion
    rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rules.htm).
    -   Further [special
        rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconv_constructor_inference.htm)
        apply if the constructor expression is passed to an [actual
        parameter](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenactual_parameter_glosry.htm "Glossary Entry")
        with a generically typed [formal
        parameter](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenformal_parameter_glosry.htm "Glossary Entry").
-   The operator is particularly suitable for avoiding the declaration
    of helper variables.

Examples:
``` abap
"Result: 0.2
DATA(a) = CONV decfloat34( 1 / 5 ).

"Comparison with an expression without CONV; the result is 0, the data type is i
DATA(b) = 1 / 5.
```

Excursion: As outlined above, you can construct structures and internal
tables using the `VALUE` operator. Using this operator for
constructing [elementary data
objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenelementary_data_object_glosry.htm "Glossary Entry")
is not possible apart from creating a data object with an initial value,
for example `DATA(str) = VALUE string( ).`. The `CONV`
operator closes this gap. However, in some cases, the use of
`CONV` is redundant.

``` abap
DATA(c) = CONV decfloat34( '0.4' ).

"Instead of
DATA d TYPE decfloat34 VALUE '0.4'.
"or
DATA e TYPE decfloat34.
e = '0.4'.

"Redundant conversion
"Derives the string type automatically
DATA(f) = `hallo`.

"Produces a syntax warning
"DATA(g) = CONV string( `hallo` ).
```


<p align="right"><a href="#top">⬆️ back to top</a></p>

## EXACT

-   The
    [`EXACT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_exact.htm)
    operator enforces either a [lossless
    assignment](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlossless_move.htm)
    or a [lossless
    calculation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlossless_calculation.htm)
    depending on the data object specified within the parentheses and
    creates an appropriate result.
-   In case of calculations, [rules of lossless
    assignments](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove_exact.htm)
    apply. In other cases, the result is created according to the
    [conversion
    rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rules.htm)
    mentioned above and an additional check is performed in accordance
    with the [rules of lossless
    assignments](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove_exact.htm).

Examples:
``` abap
"Leads to a data loss when converting to a data object accepting only a single character
TRY.
  DATA(exact1) = EXACT abap_bool( 'XY' ).
  CATCH CX_SY_CONVERSION_DATA_LOSS INTO DATA(error1).
ENDTRY.

"The calculation cannot be executed exactly; a rounding is necessary
TRY.
  DATA(exact2) = EXACT decfloat34( 1 / 3 ).
  CATCH CX_SY_CONVERSION_ROUNDING INTO DATA(error2).
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## REF

-   The
    [`REF`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_ref.htm)
    operator creates a [data reference
    variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry")
    pointing to a specified data object.
-   The type specified after `REF` and directly before the first
    parenthesis determines the [static
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_type_glosry.htm "Glossary Entry")
    of the result.
-   The operator replaces [`GET
    REFERENCE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapget_reference.htm)
    and is particularly useful for avoiding the declaration of helper
    variables that are only necessary, for example, to specify data
    reference variables as actual parameters.
- The following can be specified after `REF` before the first parenthesis: A non-generic data type that satisfies the rules of [upcasts in data references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_references_data.htm), the generic type `data`, the `#` character if the type can be derived from the context.

Examples:
``` abap
"Data references
"Declaring data object and assign value

DATA num TYPE i VALUE 5.

"Declaring data reference variable

DATA dref_a TYPE REF TO i.

"Getting references

dref_a = REF #( num ).

"Inline declaration and explicit type specification
DATA(dref_b) = REF string( `hallo` ).

"Object references

DATA(oref_a) = NEW some_class( ).

DATA(oref_b) = REF #( oref_a ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## CAST

-   Using the
    [`CAST`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_cast.htm)
    operator, you can carry out
    [upcasts](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenup_cast_glosry.htm "Glossary Entry")
    and
    [downcasts](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendown_cast_glosry.htm "Glossary Entry")
    and create a reference variable of a static type as a result.
-   It replaces the
    [`?=`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove_cast.htm)
    operator and enables [chained method
    calls](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenchained_method_call_glosry.htm "Glossary Entry").
-   The operator is particularly helpful for avoiding the declaration of
    helper variables and more contexts.
-   Similar to the `NEW` operator, constructor expressions with
    `CAST` can be followed by the object component selector
    `->` to point to a class or interface attribute (`... CAST class( ... )->attr`) and methods (`... CAST class( ...
    )->meth( ... )`). Method chaining, standalone and
    functional method calls are possible, too. See more information
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_cast.htm).

[Run Time Type Identification
(RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm "Glossary Entry")
examples:
``` abap
"Getting component information
DATA(components) = CAST cl_abap_structdescr(
  cl_abap_typedescr=>describe_by_data( some_object ) )->components.

"Getting method information
DATA(methods) = CAST cl_abap_objectdescr(
  cl_abap_objectdescr=>describe_by_name( 'LOCAL_CLASS' ) )->methods.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## COND

-   The
    [`COND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconditional_expression_cond.htm)
    operator is used for either creating a result depending on [logical
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogical_expression_glosry.htm "Glossary Entry")
    or raising a [class-based
    exception](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_based_exception_glosry.htm "Glossary Entry")
    (which is specified within the parentheses after the addition
    [`THROW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconditional_expression_result.htm)).
-   There can be multiple logical expressions initiated by
    `WHEN` followed by the result specified after
    `THEN`. If none of the logical expressions are true, you can
    specify an `ELSE` clause at the end. If this clause is not
    specified, the result is the initial value of the specified or
    derived data type.
-   Note that all operands specified after `THEN` must be
    convertible to the specified or derived data type.

Example:
``` abap
DATA(b) = COND #( WHEN a BETWEEN 1 AND 3 THEN w
                  WHEN a > 4 THEN x
                  WHEN a IS INITIAL THEN y
                  ELSE z ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## SWITCH

The
[`SWITCH`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconditional_expression_switch.htm)
operator is fairly similar to the `COND` operator and works in
the style of
[`CASE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcase.htm)
statements, i. e. it uses the value of only a single variable that is
checked in the case distinction.

``` abap
DATA(b) = SWITCH #( a
                    WHEN 1 THEN w
                    WHEN 2 THEN x
                    WHEN 3 THEN y
                    ELSE z ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## FILTER

-   The
    `FILTER` operator constructs an internal table according to a specified type (which can be an explicitly specified, non-generic table type or the # character as a symbol for the [operand type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_type_glosry.htm) before the first parenthesis).
- The lines for the new internal table are taken from an
    existing internal table based on conditions specified in a `WHERE` clause. Note that the table type of the existing internal table must be convertible to the specified target type.
-   The conditions can either be based on single values or a [filter
    table](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_filter_table.htm).
- Additions:

|Addition |Details |
|---|---|
|`USING KEY`  | Specifies the [table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_key_glosry.htm "Glossary Entry") with which the `WHERE` condition is evaluated: either a [sorted key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensorted_key_glosry.htm "Glossary Entry") or a [hash key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhash_key_glosry.htm "Glossary Entry"). If the internal table has neither of them, a [secondary table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensecondary_table_key_glosry.htm "Glossary Entry") must be available for the internal table which must then be specified after `USING KEY`.  |
| `EXCEPT`   | The specification of `EXCEPT` means that those lines of the existing table are used that do not meet the condition specified in the `WHERE` clause. Hence, if `EXCEPT` is not specified, the lines of the existing table are used that meet the condition.  |


Examples:
```abap
"FILTER and conditions based on single values
"Assumption the component num is of type i.
DATA itab1 TYPE SORTED TABLE OF struc WITH NON-UNIQUE KEY num.
DATA itab2 TYPE STANDARD TABLE OF struc WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS num.
DATA itab3 TYPE HASHED TABLE OF struc WITH UNIQUE KEY num.

"The lines meeting the condition are respected.
"Note: The source table must have at least one sorted or hashed key.
"Here, the primary key is used
DATA(f1) = FILTER #( itab1 WHERE num >= 3 ).

"USING KEY primary_key explicitly specified; same as above
DATA(f2) = FILTER #( itab1 USING KEY primary_key WHERE num >= 3 ).

"EXCEPT addition
DATA(f3) = FILTER #( itab1 EXCEPT WHERE num >= 3 ).
DATA(f4) = FILTER #( itab1 EXCEPT USING KEY primary_key WHERE num >= 3 ).

"Secondary table key specified after USING KEY
DATA(f5) = FILTER #( itab2 USING KEY sec_key WHERE num >= 4 ).
DATA(f6) = FILTER #( itab2 EXCEPT USING KEY sec_key WHERE num >= 3 ).

"Note: In case of a hash key, exactly one comparison expression for each key component is allowed;
"only = as comparison operator possible.
DATA(f7) = FILTER #( itab3 WHERE num = 3 ).

"Using a filter table
"In the WHERE condition, the columns of source and filter table are compared. Those lines in the source table
"are used for which at least one line in the filter table meets the condition. EXCEPT and USING KEY are also possible.

DATA filter_tab1 TYPE SORTED TABLE OF i
  WITH NON-UNIQUE KEY table_line.

DATA filter_tab2 TYPE STANDARD TABLE OF i
  WITH EMPTY KEY
  WITH UNIQUE SORTED KEY line COMPONENTS table_line.

DATA(f8) = FILTER #( itab1 IN filter_tab1 WHERE num = table_line ).

"EXCEPT addition
DATA(f9) = FILTER #( itab1 EXCEPT IN filter_tab1 WHERE num = table_line ).

"USING KEY is specified for the filter table
DATA(f10) = FILTER #( itab2 IN filter_tab2 USING KEY line WHERE num = table_line ).

"USING KEY is specified for the source table, including EXCEPT
DATA(f11) = FILTER #( itab2 USING KEY sec_key EXCEPT IN filter_tab2 WHERE num = table_line ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## REDUCE

-   The
    [`REDUCE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_reduce.htm)
    operator creates a result of a specified or derived type from one or
    more [iteration
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeniteration_expression_glosry.htm "Glossary Entry").
-   It basically reduces sets of data objects to a single data object.
    For example, the numeric values of a table column are summed up. As
    a result, the total number is constructed.

The following example calculates the total of the numbers from 1 to 10
using the `REDUCE` operator:
``` abap
"sum: 55
DATA(sum) = REDUCE i( INIT s = 0
                      FOR  i = 1 UNTIL i > 10
                      NEXT s += i ) ).   
```

> **💡 Note**<br>
> -   `INIT ...`: A temporary variable is specified that sets an
    initial value for the result variable.
>-   `FOR ...`: Represents a loop. The loop is carried out until
    the condition is met after `UNTIL`.
>-   `NEXT ...`: Represents the assignment to the temporary
    variable after every iteration.
>-   Once the loop has finished, the target variable is assigned the
    resulting value.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Iteration Expressions with FOR

-   Using [iteration
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeniteration_expression_glosry.htm "Glossary Entry")
    with the language element
    [`FOR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfor.htm),
    you can carry out [conditional
    iterations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfor_conditional.htm)
    (including the ABAP words `UNTIL` and `WHILE` which
    have the semantics of ABAP statements
    [`DO`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdo.htm)
    and
    [`WHILE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapwhile.htm))
    or [table
    iterations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_iteration_glosry.htm "Glossary Entry")
    (having the semantics of [`LOOP AT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab_variants.htm);
    the expressions include the ABAP word `IN`).
-   Such expressions are possible in the following contexts:
    -   `REDUCE`: The reduction result is created in the
        iteration steps.
    -   `NEW` and `VALUE`: Used in the context of
        looping across internal tables. New table lines are created in
        the iteration steps and inserted into a target table.

`FOR ... WHILE`: The following example with `REDUCE`
has the same effect as the example using `UNTIL` shown above.

``` abap
DATA(sum) = REDUCE i( INIT y = 0
                      FOR n = 1 THEN n + 1 WHILE n < 11
                      NEXT y += n ).
```

`FOR ... UNTIL`: See the example in the `REDUCE`
section.

`FOR ... IN`:

-   The operand specified after `FOR` represents an iteration
    variable, i. e. a [work
    area](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwork_area_glosry.htm "Glossary Entry")
    that contains the data while looping across the table.
-   This variable is only visible within the `FOR`
    expression, i. e. it cannot be used outside of the expression.
-   The type of the variable is determined by the type of the internal
    table specified after `IN`.
-   One or more iteration expressions can be specified using
    `FOR`.
-   The components or the whole table line that is to be returned are
    specified within the pair of parentheses before the closing
    parenthesis.
-   In contrast to `LOOP` statements, the sequential processing
    cannot be debugged.

Some examples for looping across tables and storing values in target
tables:
``` abap
"Looping across table and storing the whole line in a new table;
"the target table must have the same table type as the source table itab;
"without the WHERE condition, all lines are respected

TYPES t_type LIKE itab.

... = VALUE t_type( FOR wa IN itab
                    "WHERE ( comp1 > 2 )
                    ( wa ) ).

"Storing specific components having different names by specifying the assignment
"individually; assumption: the target type is not compatible to the type of itab;
"a field mapping is provided; pay attention to potential type conversion

... = VALUE t_type( FOR wa IN itab
                    "WHERE ( comp1 > 2 )
                    ( compX = wa-comp1
                      compY = wa-comp2 ) ).

"Storing specific components having the same names;
"assumption: Target type is not compatible to the type of itab;
"if there are identically named components in the table types, you might
"also use CORRESPONDING

... = VALUE t_type( FOR wa IN itab
                    "WHERE ( comp1 > 2 )
                    ( CORRESPONDING #( wa ) ) ).

"Multiple iteration expressions

... = VALUE t_type( FOR wa1 IN itab1 WHERE ( comp1 = 4 )
                    FOR wa2 IN itab2 WHERE ( comp2 > 5 )
                    FOR wa3 IN itab3 WHERE ( comp3 < 3 )
                    ( compX = wa1-comp1
                      compY = wa2-comp2
                      compZ = wa3-comp3 ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## LET Expressions

-   [`LET`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaplet.htm)
    expressions allow you to declare local helper fields (variables or
    fields symbols) and assign values (the type is derived from the
    defined value) to be used in constructor expressions, for example,
    in iteration expressions using `FOR` or results specified in
    the conditional expressions of `COND` and `SWITCH`.
-   Note that the helper field is only valid in the context in which the
    `LET` expression is specified.

Examples:
``` abap
"Creating a string table using a LET expression

DATA(str_tab) = VALUE string_table( LET it = `be` IN
                    ( |To { it } is to do| )
                    ( |To { it } or not to { it }| )
                    ( |To do is to { it }| )
                    ( |Do { it } do { it } do| ) ).

"Conditional expressions

DATA(a) = COND #( LET b = c IN
                  WHEN b > x THEN ...
                  WHEN b < y THEN ...
                  ...
                  ELSE ... ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

[zcl_demo_abap_constructor_expr](./src/zcl_demo_abap_constructor_expr.clas.abap)

Note the steps outlined [here](README.md#-getting-started-with-the-examples) about how to import and run the code.
