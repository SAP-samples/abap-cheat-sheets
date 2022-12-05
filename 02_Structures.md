
# Working with Structures

- [Working with Structures](#working-with-structures)
  - [Structures ...](#structures-)
  - [Creating Structures and Structured Types](#creating-structures-and-structured-types)
  - [Variants of Structures](#variants-of-structures)
  - [Working with Structures](#working-with-structures-1)
    - [Accessing Components of Structures](#accessing-components-of-structures)
    - [Filling Structures](#filling-structures)
  - [Clearing Structures](#clearing-structures)
  - [Structures in Use in the Context of Tables](#structures-in-use-in-the-context-of-tables)
    - [Excursion: Including Structures](#excursion-including-structures)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)

## Structures ...

-   (or structured [data
    objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm "Glossary Entry"))
    are ABAP variables typed with [structured
    types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstructured_type_glosry.htm "Glossary Entry").
-   are [complex data
    types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplex_data_type_glosry.htm "Glossary Entry").
-   are composed of a sequence of other data objects which are called
    [components](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomponent_glosry.htm "Glossary Entry").
-   have components that can be of any type, that is, they can
    themselves be, for example, structures or [internal
    tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninternal_table_glosry.htm "Glossary Entry").
-   summarize different pieces of information, that is, data objects,
    that belong together. A typical example is the following: an address
    has several components like name, street, city, and so on.
-   mostly serve as line types of internal tables and [database
    tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendatabase_table_glosry.htm "Glossary Entry").
-   are often used to process data sequentially, such as the data stored
    in the rows and columns of a database table.
-   and structured types can typically be found in the [ABAP Dictionary
    (DDIC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_dictionary_glosry.htm "Glossary Entry"),
    sometimes in [global
    classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_class_glosry.htm "Glossary Entry");
    or they can be defined locally in ABAP programs.
    -   Most of the structures that are worked with in ABAP programs may
        be globally defined structures in the DDIC.
    -   Why? Apart from globally defined structures in the DDIC, each
        database table or
        [view](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenview_glosry.htm "Glossary Entry")
        constitutes a structured type. When a database table etc. is
        activated, a globally available structured type of the same name
        is created, too. Hence, in an ABAP program, a database table's
        name is typically used as type name to declare data objects, for
        example, structures or internal tables.
-   can be addressed as a whole. You can also address the individual
    components of structures.

## Creating Structures and Structured Types

> **💡 Note**<br>
> This cheat sheet focuses on locally defined structures and structured types.

The typical syntactical elements are [`BEGIN OF ... END OF ...`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes_struc.htm).
They are used in combination with
[`TYPES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes.htm)
to create a structured type and
[`DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata.htm)
to create a structure.

**Creating structured types**

The structures' components are listed between `... BEGIN OF ...` and `... END OF ...`.
``` abap
TYPES: BEGIN OF struc_type,
         comp1 TYPE ...,
         comp2 TYPE ...,
         comp3 TYPE ...,
         ...,
       END OF struc_type.
```
Alternatively, you could go with the following syntax, however, a chained statement with the colon `:` above is preferable due to
better readability.
``` abap
TYPES BEGIN OF struc_type.
  TYPES comp1 TYPE ... .
  TYPES comp2 TYPE ... .
  TYPES comp3 TYPE ... .
... .
TYPES END OF struc_type.
```
As mentioned above, the components of structures can be of any type.
They can be
[elementary](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenelementary_data_type_glosry.htm "Glossary Entry")
and also complex, that is, a structure component can be a structure or
internal table. Besides this, the components can be [reference
variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_variable_glosry.htm "Glossary Entry"),
too.

For the components, the additions
[`TYPE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_simple.htm)
and
[`LIKE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_referring.htm)
are possible.
> **💡 Note**<br>
> Setting default values with
[`VALUE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_options.htm)
is - in contrast to the creation of structures using a `DATA`
statement as shown further down - not possible in this case.

``` abap
TYPES: BEGIN OF struc_type,
         comp1 TYPE i,                       "elementary type
         comp2 TYPE c LENGTH 5,              "elementary type
         comp3 TYPE local_structured_type,   "local structured type

         comp4 TYPE itab_type,               "internal table type

         comp5 TYPE ddic_type,               "DDIC type
         comp6 TYPE REF TO i,                "reference
         comp7 LIKE data_object,             "deriving type from a data object
         ...,
       END OF struc_type.
```

**Creating structures**

There are multiple ways to create structures in a program using a `DATA` statement:

- Creating a structure either based on a local type or a globally available type from the DDIC:

    ``` abap
        DATA ls_from_local TYPE struc_type,
             ls_from_global TYPE ddic_type.
    ```

- Creating a structure by directly specifying the components using `... BEGIN OF ... END OF ...`. The syntax is similar to the `TYPES` statement. Here, default values can be set with
`VALUE`.

    ``` abap
    DATA: BEGIN OF struc,
            comp1 TYPE ...,
            comp2 TYPE ... VALUE ...,
            comp3 TYPE i VALUE 99,
            comp4 TYPE i VALUE IS INITIAL,
            comp5 TYPE local_structured_type,
            ...,
          END OF struc.
    ```

    Alternatively and similar to the `TYPES` statement, you
    could use the following syntax, however, a chained statement with the colon `:` as above is preferable due to better
    readability.

    ``` abap
    DATA BEGIN OF struc.
      DATA comp1 TYPE ... .
      DATA comp2 TYPE ... VALUE ... .
    ... .
    DATA END OF struc.
    ```
- Creating a structure by referring to a local structured data object using `LIKE` or to an internal table using `LIKE LINE OF`.
    ``` abap
    DATA struc1 LIKE other_struc.

    DATA struc2 LIKE LINE OF itab.
    ```
- Creating a structure by inline declaration, e. g. using `DATA( ... )`.
    ``` abap
    "Type is derived from the right-hand structure;
    "the content of struc is assigned, too.

    DATA(struc1) = struc.

    "Using the VALUE operator; structured type is specified before the
    "first parenthesis; component values can be assigned, too

    DATA(struc2) = VALUE struc_type( comp1 = ... ).

    "It is particularly handy for declaring the structure where you actually need it
    "without prior extra declaration of the structure in various contexts;
    "e. g. SELECT or LOOP AT statements; structured types are automatically
    "derived from the context

    SELECT SINGLE *
        FROM zdemo_abap_fli
        WHERE carrid = 'LH'
        INTO @DATA(struc3).

    LOOP AT itab INTO DATA(wa).
      ...
    ENDLOOP.
    ```

## Variants of Structures

Depending on the component type, the structure can be a [flat
structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenflat_structure_glosry.htm "Glossary Entry"),
a [nested
structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennested_structure_glosry.htm "Glossary Entry"),
or a [deep
structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_structure_glosry.htm "Glossary Entry").

**Flat structure**

Flat structures contain only elementary types that have a fixed length, that is, there are no internal tables, reference types or strings as components:
``` abap
DATA: BEGIN OF structure,
        comp1 TYPE i,
        comp2 TYPE c LENGTH 15,
        comp3 TYPE p LENGTH 8 DECIMALS 2,
        ...,
      END OF structure.
```
> **💡 Note**<br>
> Nesting does not play a role in this context. Even a nested structure is flat unless a substructure contains a deep component.

**Nested structure**

At least one component of a structure is a
[substructure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstructure_glosry.htm "Glossary Entry"),
that is, it refers to another structure. The following example has
multiple substructures.
``` abap
DATA: BEGIN OF address_n,
        BEGIN OF name,
          title   TYPE string VALUE `Mr.`,
          prename TYPE string VALUE `Duncan`,
          surname TYPE string VALUE `Pea`,
        END OF name,
        BEGIN OF street,
          name   TYPE string VALUE `Vegetable Lane`,
          number TYPE string VALUE `11`,
        END OF street,
        BEGIN OF city,
          zipcode TYPE string VALUE `349875`,
          name    TYPE string VALUE `Botanica`,
        END OF city,
      END OF address_n.
```

**Deep structure**

A deep structure contains at least one internal table, reference type, or string as a component.
``` abap
DATA: BEGIN OF address_d,
        name    TYPE string VALUE `Mr. Duncan Pea`,
        street  TYPE string VALUE `Vegetable Lane 11`,
        city    TYPE string VALUE `349875 Botanica`,
        details TYPE TABLE OF some_table WITH EMPTY KEY,
      END OF address_d.
```
Despite the fact that the following structure looks fairly simple, it is not to be considered a flat structure but a deep structure since it contains strings.
``` abap
DATA: BEGIN OF address,
        name   TYPE string VALUE `Mr. Duncan Pea`,
        street TYPE string VALUE `Vegetable Lane 11`,
        city   TYPE string VALUE `349875 Botanica`,
      END OF address.
```

## Working with Structures

### Accessing Components of Structures

The structure as a whole and also the individual components can be
addressed. To address the components, you use the [structure component
selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstructure_component_sel_glosry.htm "Glossary Entry")
`-`:
``` abap
structure-comp1 ...

address-name ...
```

Nested components can be addressed via chaining:
```
structure-substructure-comp1 ...

address_n-name-title ...
```

> **✔️ Hints**<br>
>- You can refer to structure components when creating new data types and data objects:
>   ``` abap
>    TYPES: lty_1 TYPE structured_type-comp1,
>           lty_2 LIKE structure-comp1.
>
>    DATA: lv_1 TYPE structured_type-comp1,
>          lv_2 LIKE structure-comp1.
>    ```
>- ADT and the ABAP Editor provide code completion for structure components after the component selector.
>- When declaring a variable with reference to a structured data object, the object component selector `->` can be used:
   `...dref->comp ...` (apart from the needlessly longer syntax `... dref->*-comp ...`).

### Filling Structures

Filling structure components using the component selector.
``` abap
address-name   = `Mr. Duncan Pea`.

address-street = `Vegetable Lane 11`.

address-city   = `349875 Botanica`.
```
Filling structure components using the
[`VALUE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_value.htm)
operator. Value assignments by addressing the structure
components individually can be very bulky. Hence, the use of the
`VALUE` operator is very handy for the value assignment,
especially for filling structure components at [operand position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm "Glossary Entry").
In the example below, the `#` sign is used before the
parentheses which means that the type of the operand can be implicitly
derived. This is not the case for the example further down in which a
new structure is [declared inline](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_inline.htm).
Hence, the type must be explicitly specified before the parentheses.
Note that components that are not specified and assigned a value remain
initial.
``` abap
address = VALUE #( name   = `Mr. Duncan Pea`
                   street = `Vegetable Lane 11`.
                   city   = `349875 Botanica` ).
```
Using the `VALUE` operator and inline declarations, structures can be created and filled in one go.
``` abap
TYPES address_type LIKE address.

DATA(add2) = VALUE address_type( name   = `Mrs. Jane Pea`
                                 street = `Vegetable Lane 11`.
                                 city   = `349875 Botanica` ).
```
Copying the content of a structure to another one that has the same type. For value assignments, generally
bear in mind that there are special [conversion](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_struc.htm)
and [comparison rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_rules_operands_struc.htm)
that apply to assignments involving structures..
``` abap
address = add2.

"When creating a new structure by inline declaration, the type of
"the right-hand structure is derived and the content is assigned.

DATA(add3) = add2.
```
Copying content of a structure to another one that has a different type. You can use statements with
[`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove-corresponding.htm)
and the
[`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm)
operator. Both are used to assign identically named components of
structures to each other. This syntax also works for structures that
have the same type.

Note the rules mentioned above and consider the result of the value
assignment when using either of the two options with
`MOVE-CORRESPONDING` and the `CORRESPONDING` operator.
See more information and syntax variants in the ABAP Keyword
Documentation:
[`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencorresponding_constr_arg_type.htm)
and
[`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencorresponding_constr_arg_type.htm).
In the examples below, the focus is only on flat structures.
``` abap
"Moves identically named components; content in other components
"of the targets structure are kept.

MOVE-CORRESPONDING struc TO diff_struc.

"Initializes target structure; moves identically named components

diff_struc = CORRESPONDING #( struc ).

"Same effect as the first MOVE-CORRESPONDING statement;
"addition BASE keeps existing content

diff_struc = CORRESPONDING #( BASE ( diff_struc ) struc ).

"MAPPING addition: Specifying components of a source structure that are
"assigned to the components of a target structure in mapping
"relationships.

diff_struc = CORRESPONDING #( BASE ( diff_struc )
                              struc MAPPING comp1 = compa ).

"EXCEPT addition: Excluding components from the assignment.

diff_struc = CORRESPONDING #( BASE ( diff_struc )
                              struc EXCEPT comp1 ).
```
**Excursion**: Copying content of a deep structure to another deep structure that has a different type. You can use statements with
[`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove-corresponding.htm)
and the
[`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm)
operator here, too. However, in the context of deep structures, more syntax variants are available. The focus of the following examples is on internal tables as structure components. See the executable example for visualizing the effect.

> **💡 Note**<br>
> Bear in mind that there are special conversion and comparison rules that apply to assignments involving structures and that affect the value assignments in the internal table components.

``` abap
"Nonidentical elementary component types are kept in target
"structure which is true for the below MOVE-CORRESPONDING statements;
"existing internal table content is replaced by content of
"the source table irrespective of identically named components

MOVE-CORRESPONDING deep_struc TO diff_deep_struc.

"Existing internal table content is replaced but the value
"assignment happens for identically named components only.

MOVE-CORRESPONDING deep_struc TO diff_deep_struc
        EXPANDING NESTED TABLES.

"Existing internal table content is kept; table content of the source
"structure are added but the value assignment happens like the first
"MOVE-CORRESPONDING statement without further syntax additions.

MOVE-CORRESPONDING deep_struc TO diff_deep_struc
        KEEPING TARGET LINES.

"Existing internal table content is kept; table content of the source
"structure are added; the value assignment happens like the statement
"MOVE-CORRESPONDING ... EXPANDING NESTED TABLES.

MOVE-CORRESPONDING deep_struc TO diff_deep_struc
    EXPANDING NESTED TABLES KEEPING TARGET LINES.

"Target structure is initialized; the value assignment for an internal
"table happens irrespective of identically named components.

diff_deep_struc = CORRESPONDING #( deep_struc ).

"Target structure is initialized; the value assignment for an internal
"table happens for identically named components only.

diff_deep_struc = CORRESPONDING #( DEEP deep_struc ).

"Nonidentical elementary component types are kept in target structure;
"internal table content is replaced; there, the value assignment
"happens like using the CORRESPONDING operator without addition.

diff_deep_struc = CORRESPONDING #( BASE ( diff_struc ) deep_struc ).

"Nonidentical elementary component types are kept in target structure;
"internal table content is replaced; there, the value assignment
"happens like using the CORRESPONDING operator with the addition DEEP.

diff_deep_struc = CORRESPONDING #( DEEP BASE ( diff_struc ) deep_struc ).

"Nonidentical elementary component types are kept in target structure;
"internal table content is kept, too, and table content of the
"source structure are added; there, the value assignment
"happens like using the CORRESPONDING operator without addition.

diff_deep_struc = CORRESPONDING #( APPENDING BASE ( diff_struc ) deep_struc ).

"Nonidentical elementary component types are kept in target structure;
"internal table content is kept, too, and table content of the
"source structure are added; there, the value assignment
"happens like using the CORRESPONDING operator with the addition DEEP.

diff_deep_struc = CORRESPONDING #( DEEP APPENDING BASE ( diff_struc ) deep_struc ).
```
                                  
## Clearing Structures
You can reset individual components to their initial value and clear the
complete structure using the keyword
[`CLEAR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclear.htm).
```
CLEAR structure-component.

CLEAR structure.
```

## Structures in Use in the Context of Tables
Structures are primarily used to process data from tables. In this
context, structures often assume the role of a [work
area](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwork_area_glosry.htm "Glossary Entry").

**Reading a line from a database table into a structure that has a matching type**. Note that, since database tables are flat, the
target structure must also be flat. In the example below, the addition
[`SINGLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_single.htm)
reads only a single row into the structure. The first entry that is
found according to the `WHERE` condition is returned.

> **💡 Note**<br>
> See more details on
[`SELECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect.htm)
statements in the ABAP Keyword Documentation. The ABAP cheat sheet [ABAP
SQL: Working with Persisted Data in Database
Tables](03_ABAP_SQL.md)
also provides information and more code snippets when using
`SELECT` statements.
``` abap
"Creating a structure with a matching type
DATA ls_fli1 TYPE zdemo_abap_fli.

SELECT SINGLE FROM zdemo_abap_fli
    FIELDS *
    WHERE carrid = 'LH'
    INTO @ls_fli1.

"Target structure declared inline
SELECT SINGLE FROM zdemo_abap_fli
    FIELDS *
    WHERE carrid = 'LH'
    INTO @DATA(ls_fli2).
```
**Reading a line from a database table into a structure that has a different type**.
``` abap
SELECT SINGLE FROM zdemo_abap_fli
    FIELDS *
    WHERE carrid = 'AA'
    INTO CORRESPONDING FIELDS OF @ls_fli_diff.
```
**Reading a line from an internal table into a structure** ...

... using a `SELECT` statement. Note the specified alias name and that ABAP variables like internal tables must be escaped using `@`. The addition `INTO CORRESPONDING FIELDS OF` also applies here.
``` abap
SELECT SINGLE FROM @itab AS itab_alias
    FIELDS *
    WHERE ...
    INTO @DATA(ls_struc).
    "INTO CORRESPONDING FIELDS OF @struc.
```
... using a `READ TABLE` statement. The code snippet below shows the reading of one line
into a [work
area](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwork_area_glosry.htm "Glossary Entry"),
[field
symbol](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm "Glossary Entry")
and a [data reference
variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry"),
all representing structured data objects and declared inline below. In
the example below, the reading of a line is based on the line number by
specifying `INDEX`. See the section `Determining the target area` in the cheat sheet [Working with Internal Tables](01_Internal_Tables.md#) for more details.
``` abap
READ TABLE itab INTO DATA(wa) INDEX 1.

READ TABLE itab ASSIGNING FIELD-SYMBOL(<fs>) INDEX 2.

READ TABLE itab REFERENCE INTO DATA(dref) INDEX 3.
```
... using a [table
expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_expression_glosry.htm "Glossary Entry").
The code snippet below shows the reading of one line into a structure
that is declared inline. The index is specified in square brackets.
``` abap
DATA(ls_table_exp) = itab[ 3 ].
```

> **💡 Note**<br>
> See more syntax variants and code snippets for `READ TABLE` statements and statements with table expressions in the ABAP cheat sheet `Working with Internal Tables`.

**Sequentially reading a line from** ...

... a database table into a structure. A `SELECT` loop can be specified by using the syntax `SELECT ... ENDSELECT.`.
In the simple example below, the line that is found and returned in a structure, which is declared inline, can be further processed.
A `SELECT` loop might also have an internal table as data source.
``` abap
SELECT FROM zdemo_abap_fli
    FIELDS *
    WHERE carrid = 'AZ'
    INTO @DATA(ls_sel_loop).
      IF sy-subrc = 0.
        ...
      ENDIF.
ENDSELECT.
```
... an internal table into a structure using a [`LOOP AT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab_variants.htm) statement. There are numerous options for specifying the condition on which the loop is based. The example below covers the option of reading all lines sequentially into a field symbol which is declared inline. When using a field symbol, components can be directly modified, for example.
``` abap
LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs>).
   <fs>-comp1 = ...
   ...
ENDLOOP.
```
**Inserting an individual row from a structure into a database table** using  ABAP SQL statements with
[`INSERT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinsert_dbtab.htm). The statements below can be considered as alternatives. The third statement demonstrates that the structure might also be created and filled directly instead of inserting a line from an existing structure. Note that a line with a certain key should not be inserted into the database table if a line with the same key already exists there.
``` abap
INSERT INTO dbtab VALUES @structure.

INSERT dbtab FROM @structure.

INSERT dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ).
```
**Updating an individual row from a structure in a database table** using ABAP SQL statements with [`UPDATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapupdate.htm). Note that with this syntax, the whole line and all components are changed.
``` abap
UPDATE dbtab FROM @structure.

UPDATE dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ).
```
If you want to update a database table row from a structure by specifying components to be changed without overwriting other components, you might use the following way. First, read the intended line from the database table into a structure. Then, use the `VALUE` operator with the addition `BASE` and specify the components to be changed.
``` abap
SELECT SINGLE *
  FROM dbtab
  WHERE ...
  INTO @DATA(wa).

UPDATE dbtab FROM @( VALUE #( BASE wa comp2 = ... comp4 = ... ) ).
```
**Updating or creating an individual row in a database table from a structure** using ABAP SQL statements with
[`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_dbtab.htm). If a line in the database table already exists with the same keys as specified in the structure, the line is updated. If a line does not exist with the keys specified in the structure, a new line is created in the database table.
``` abap
MODIFY dbtab FROM @structure.

MODIFY dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ).
```
**Adding rows to and updating individual rows in an internal table from a structure** using statements with `INSERT`,
`APPEND`, and `MODIFY`. Note that all statements, including `INSERT` and `MODIFY`, are ABAP statements in this context, not ABAP SQL statements.

Both `INSERT` and `APPEND` add one line (or more) to an internal table. While `APPEND` adds at the bottom of the
internal table, `INSERT` can be used to add lines at a specific position in tables. If you go without specifying the position, then the lines are added at the bottom of the table, too. However, when using `INSERT`, `sy-tabix` is not set as compared to `APPEND`.
`MODIFY` changes the content of an internal table entry.

Statements using the `VALUE` operator for directly creating and filling the structures are possible, too. For more information and code
snippets, refer to the cheat sheet [Working with Internal Tables](01_Internal_Tables.md#).
``` abap
INSERT structure INTO TABLE itab.

APPEND structure TO itab.

MODIFY TABLE itab FROM structure.
```

### Excursion: Including Structures

Although their use is not recommended according to the ABAP programming
guidelines, you might stumble upon statements with [`INCLUDE TYPE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinclude_type.htm)
and [`INCLUDE STRUCTURE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinclude_type.htm)
in the context of local structures. Structured data objects and types
that are created with `... BEGIN OF... END OF ...` can
include the said syntax to integrate components of another structure, no
matter if it is a locally defined or global structure, without the need
to create substructures. `INCLUDE TYPE` can be used to include a
structured type. `INCLUDE STRUCTURE` can be used to include a
structure.

> **💡 Note**<br>
> - They are not additions of `... BEGIN OF ... END OF ...` but individual ABAP statements.
> - If a chained statement is used for the structure declaration with the colon, the inclusion of other structures with these statements interrupts the chained statement, that is, the components of the included structures are included as direct components of the [superstructure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensuperstructure_glosry.htm "Glossary Entry").
>- Using the optional addition `AS` and the specification of a name, the included components can be addressed by this common name as if the components were actually components of a substructure.
>- Using the optional addition `RENAMING WITH SUFFIX` and the specification of a name, the included components are given a suffix name to avoid naming conflicts with other components.

The example below shows how structured types and data objects are included in another structure. First, three structured types as well as a structured data object based on one of those types are created. Then, the types and the structure are included in the structured type `address_type`. The demonstration class visualizes a structure that includes other structures this way.
``` abap
TYPES: BEGIN OF name_type,
        title   TYPE string,
        prename TYPE string,
        surname TYPE string,
      END OF name_type,
      BEGIN OF street_type,
        name   TYPE string,
        number TYPE string,
      END OF street_type,
      BEGIN OF city_type,
        zipcode TYPE string,
        name    TYPE string,
      END OF city_type.

DATA city_struc TYPE city_type.

TYPES BEGIN OF address_type.
      INCLUDE TYPE name_type AS name.
      INCLUDE TYPE street_type AS street RENAMING WITH SUFFIX _street.
      INCLUDE STRUCTURE city_struc AS city RENAMING WITH SUFFIX _city.
TYPES END OF address_type.
```

## More Information
See the topic
[Structures (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendata_objects_structure.htm)
for more information.

## Executable Example
[zcl_demo_abap_structures](./src/zcl_demo_abap_structures.clas.abap)

Note the steps outlined [here](README.md#🎬-getting-started-with-the-examples) about how to import and run the code.
