<a name="top"></a>

# Structures

- [Structures](#structures)
  - [Introduction](#introduction)
  - [Creating Structures and Structured Types](#creating-structures-and-structured-types)
  - [Variants of Structures](#variants-of-structures)
  - [Accessing (Components of) Structures](#accessing-components-of-structures)
  - [Populating Structures](#populating-structures)
    - [Using the VALUE Operator](#using-the-value-operator)
    - [Using the CORRESPONDING Operator and MOVE-CORRESPONDING Statements](#using-the-corresponding-operator-and-move-corresponding-statements)
  - [Clearing Structures](#clearing-structures)
  - [Processing Structures](#processing-structures)
  - [Excursion: Including Structures](#excursion-including-structures)
  - [Executable Example](#executable-example)

## Introduction
Structures ...

-   are [data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm "Glossary Entry")
    with [structured data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstructured_type_glosry.htm "Glossary Entry") (which is a [complex data type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplex_data_type_glosry.htm "Glossary Entry") because it is composed of other data types). 
-   consist of a sequence of [components](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomponent_glosry.htm "Glossary Entry") of any data type, that is, the components of a structure can be, for example, [elementary data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenelementary_data_object_glosry.htm), structures themselves, [internal tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninternal_table_glosry.htm "Glossary Entry") or [references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_glosry.htm).
- are used to combine different data objects that belong together. A typical example is an address. It has several components, such as name, street, city, and so on, that belong together.
- play an important role in the context of internal tables and [database tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendatabase_table_glosry.htm "Glossary Entry"). Structured types serve as [line types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrow_type_glosry.htm) for these tables. Most internal tables across [ABAP programs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm) may have structured line types. For database tables, there is no alternative to structured line types.
- can be created locally in an ABAP program. You can also create them as global [DDIC structures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_structure_glosry.htm) in the [ABAP Dictionary](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_dictionary_glosry.htm). Such a DDIC structure defines a globally available structured type ([DDIC type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_type_glosry.htm)).
  - Note: There are other structured types available globally, which may be the structured types most commonly used in ABAP programs:
    - [Database tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_db_table_glosry.htm) defined in the ABAP Dictionary can be used as data types just like DDIC structures in an ABAP program. This means that when you create a structure in your ABAP program, for example, you can simply use the name of a database table to address the line type of the table. The structure you created will then have the same structured type as the database table. Typically, you use the database tables to create structures of such a type, or internal tables of such a structured line type, to process data read from the database table in structures or internal tables.     
    -  A [CDS entity](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_entity_glosry.htm) such as a [CDS view](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_view_glosry.htm) also represents a structured data type and can be used as such in ABAP programs (but not in the ABAP Dictionary). The same applies to [DDIC views](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_view_glosry.htm) that are only available in [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm).
    - Structures and structured data types can also be defined in the public [visibility section](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenvisibility_section_glosry.htm) of [global classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_class_glosry.htm) or in [global interfaces](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_interface_glosry.htm) and then used globally.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Creating Structures and Structured Types

The typical language elements for creating structures and structured types locally in an ABAP program are [`BEGIN OF ... END OF ...`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes_struc.htm). They are used in combination with the [`TYPES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes.htm) keyword to create a structured type and the [`DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata.htm) keyword to create a structure.

> **💡 Note**<br>
> This cheat sheet focuses on locally defined structures and structured types.

**Creating structured types**

- The following statement defines a structured type introduced by `TYPES`. The type name is preceded by `BEGIN OF` (which marks the beginning of the structured type definition) and `END OF` (the end of the definition). 
- The components - at least one must be defined - are listed in between.
- Such structured type definitions are usually grouped together in a [chained statement](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenchained_statement_glosry.htm), i.e. `TYPES` is followed by a colon, and the components are separated by commas.


``` abap
TYPES: BEGIN OF struc_type,
         comp1 TYPE ...,
         comp2 TYPE ...,
         comp3 TYPE ...,
         ...,
       END OF struc_type.
```

Alternatively, you can also use the following syntax. However, a chained statement provides better readability.
``` abap
TYPES BEGIN OF struc_type.
  TYPES comp1 TYPE ... .
  TYPES comp2 TYPE ... .
  TYPES comp3 TYPE ... .
  ... .
TYPES END OF struc_type.
```


- The simplest structures and structured types have [elementary](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenelementary_data_type_glosry.htm "Glossary Entry")
components.
- As mentioned above, the components can be of any type, i.e. they can be of structured types themselves, internal table types, or [reference types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_type_glosry.htm). 
- You can use the [`TYPE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_simple.htm)
and [`LIKE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_referring.htm) additions for the types of the components. 
You can use the `LINE OF` addition to refer to a table type or an internal table. 


``` abap
TYPES: BEGIN OF struc_type,
         comp1 TYPE i,                 "elementary type           
         comp2 TYPE c LENGTH 5,        "elementary type
         comp3 TYPE structured_type,   "structured type
         comp4 TYPE itab_type,         "internal table type
         comp5 TYPE ddic_type,         "DDIC type
         comp6 TYPE REF TO i,          "data reference
         comp7 LIKE data_object,       "deriving type from a data object                  
         comp8 TYPE LINE OF itab_type, "component has structured type, type derived from internal table type 
         comp9 LIKE LINE OF itab,      "component has structured type, type derived from internal table
         comp10,                       "no TYPE/LIKE specification: component is of type c length 1          
         ...,
       END OF struc_type.
```


> **💡 Note**<br>
> Outside of classes, you can also refer to DDIC types using `LIKE` (`... comp11 LIKE ddic_type, ...`). If you actually want to refer to an existing data object, but due to typing errors you inadvertently specify a name that exists as DDIC type, errors may be unavoidable.


**Creating structures**

- To create a structure in an ABAP program, you can use the `DATA` keyword. - It works in the same way as the `TYPES` statement above. 
- Unlike the `TYPES` statement, you can use the [`VALUE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_options.htm) addition to set default values.  

``` abap
DATA: BEGIN OF struc,
        comp1 TYPE ...,
        comp2 TYPE ... VALUE ...,
        comp3 TYPE i VALUE 99,
        comp4 TYPE i VALUE IS INITIAL,  "Without the addition VALUE, or if IS INITIAL is specified, 
                                        "the content is initial.
        comp5 TYPE local_structured_type,
        ...,
      END OF struc.
```

Alternatively, you can use the following syntax. Similar to above, a chained statement provides better readability.

``` abap
DATA BEGIN OF struc.
  DATA comp1 TYPE ... .
  DATA comp2 TYPE ... VALUE ... .
... .
DATA END OF struc.
```

> **💡 Note**<br>
>-  The keywords [`CLASS-DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass-data.htm) and [`CONSTANTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconstants.htm) can also be used to create structures. In principle, they represent special cases of the general statement shown above. See the ABAP Keyword Documentation for more information. 
>- Structures can also be created [inline](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm) using [`DATA(...)`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_inline.htm) or [`FINAL(...)`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm), as shown below.

Creating structures using existing structured types:

``` abap
"Local structured type 
TYPES: BEGIN OF struc_type,
         comp1 TYPE i,                            
         comp2 TYPE c LENGTH 5,   
       END OF struc_type.

"Creating a structure using a local structured type
DATA struc_1 TYPE struc_type.

"Creating structures based on globally available types from the DDIC
"Note: When referring to such types, you cannot provide start values for the individual components. 
DATA: struc_2 TYPE some_ddic_structure,
      struc_3 TYPE some_ddic_table,
      struc_4 TYPE some_cds_view.

"Structure based on a structured type that is available in the public
"visibility section of a global class
DATA struc_5 TYPE cl_some_class=>struc_type.

"Creating structures by referring to local data objects and internal table types
DATA: struc_6 LIKE struc_1,
      struc_7 LIKE LINE OF some_itab,
      struc_8 TYPE LINE OF some_itab_type.
```


Creating structures by inline declaration using `DATA(...)` 
- This is particularly useful for declaring data objects at the [operand positions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm) where you actually need them. 
- In this way, you can avoid an extra declaration of the structure in different contexts.
- You can also use the [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm) declaration operator to create [immutable variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenimmutable_variable_glosry.htm).

``` abap
"Structures created inline instead of an extra declared variable
DATA struc_9 LIKE struc_1.
struc_9 = struc_1

"Type is derived from the right-hand structure; the content of struc is assigned, too.
DATA(struc_10) = struc_1.
FINAL(struc_11) = struc_9.

"Structures declared inline instead of an extra declared variable

"Example: SELECT statement
"Extra declaration
DATA struc_12 TYPE zdemo_abap_fli.

SELECT SINGLE *
  FROM zdemo_abap_fli
  WHERE carrid = 'LH'
  INTO @struc_12.

"Inline declaration
SELECT SINGLE *
  FROM zdemo_abap_fli
  WHERE carrid = 'LH'
  INTO @DATA(struc_13).

"Example: Loop over an internal table
DATA itab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.
... "itab is filled

"Extra declaration
DATA wa_1 LIKE LINE OF itab.

LOOP AT itab INTO wa_1.
  ...
ENDLOOP.

"Inline declaration
LOOP AT itab INTO DATA(wa_2).
  ...
ENDLOOP.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Variants of Structures

Depending on the component type, the structure can be a [flat structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenflat_structure_glosry.htm "Glossary Entry"),
a [nested structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennested_structure_glosry.htm "Glossary Entry"),
or a [deep structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_structure_glosry.htm "Glossary Entry").

- **Flat structures** contain only elementary types that have a fixed length, that is, there are no internal tables, reference types or strings as components. Nesting does not matter in this context. Even a nested structure is considered flat unless a substructure contains a deep component.
    ``` abap
    DATA: BEGIN OF struc,
            comp1 TYPE i,
            comp2 TYPE c LENGTH 15,
            comp3 TYPE p LENGTH 8 DECIMALS 2,
            ...,
          END OF struc.
    ```    

- **Nested structures**: At least one component of a structure is a [substructure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstructure_glosry.htm "Glossary Entry"),
that is, it refers to another structure. The following example has multiple substructures.
    ``` abap
    DATA: BEGIN OF address_n,
            BEGIN OF name,
              title   TYPE string VALUE `Mr.`,
              prename TYPE string VALUE `Duncan`,
              surname TYPE string VALUE `Pea`,
            END OF name,
            BEGIN OF street,
              name TYPE string VALUE `Vegetable Lane`,
              num  TYPE string VALUE `11`,
            END OF street,
            BEGIN OF city,
              zipcode TYPE string VALUE `349875`,
              name    TYPE string VALUE `Botanica`,
            END OF city,
        END OF address_n.
    ```

- **Deep structures**: Contains at least one internal table, reference type, or string as a component.
    ``` abap
    DATA: BEGIN OF address_d,
            name    TYPE string VALUE `Mr. Duncan Pea`, 
            street  TYPE string VALUE `Vegetable Lane 11`, 
            city    TYPE string VALUE `349875 Botanica`, 
            details TYPE TABLE OF some_table WITH EMPTY KEY, 
          END OF address_d.
    ```
  Although the following structure looks quite simple, it is not a flat structure, but a deep structure, because it contains strings.
    ``` abap
    DATA: BEGIN OF address,
            name   TYPE string VALUE `Mr. Duncan Pea`,
            street TYPE string VALUE `Vegetable Lane 11`,
            city   TYPE string VALUE `349875 Botanica`,
          END OF address.
    ```

> **💡 Note**<br>
>- The data types of DDIC types are all flat (not nested) structures. Exception: Components of type `string` can be contained.
>- [Work areas](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwork_area_glosry.htm) of ABAP SQL statements cannot contain any deep components other than strings among others.
>- Especially for assignments and comparisons of deep structures, the compatibility of the source and target structure must be taken into account.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Accessing (Components of) Structures

- Structures can be accessed as a whole. You can also address the individual components of structures at the appropriate operand positions. 
- To address the components, use the [structure component selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstructure_component_sel_glosry.htm "Glossary Entry")
`-`. 
- For variables with reference to a structured data object, the [object component selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_component_select_glosry.htm) `->` can be used: `...dref->comp ...`. The following syntax also works, but is less *convenient*: `... dref->*-comp ...`.
- ADT and the ABAP Editor provide code completion for structure components after the component selectors.
``` abap
"Addressing components via the structure component selector
... struc-comp1 ...
... struc-comp2 ...
... struc-comp3 ...

"Examples for addressing the whole structure and individual components
IF struc IS INITIAL. 
  ...
ENDIF.

IF struc-comp1 = 1. 
  ...
ENDIF.

DATA(complete_struc) = struc.
DATA(comp_value) = struc-comp2.

"Type and data declarations
TYPES: type_1 TYPE structured_type-comp1,
       type_2 LIKE struc-comp1.

DATA: var_1 TYPE structured_type-comp1,
      var_2 LIKE struc-comp1.

"Variables with reference to a structured data object
DATA ref_struc_1 TYPE REF TO structured_type.
ref_struc_1 = NEW #( ).
"Excursion: Creating a reference variable using inline declaration
DATA(ref_struc_2) = NEW structured_type( ).

... ref_struc_1->comp1 ...
... ref_struc_1->*-comp1 ...  "Using the dereferencing operator
... ref_struc_2->-comp2 ... 
... ref_struc_2->*-comp2 ...  "Using the dereferencing operator
```

Nested components can be addressed using chaining:
``` abap
... struc-substructure-comp1 ...
... address_n-name-title ...
```

> **💡 Note**<br>
> The [`ASSIGN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapassign_dynamic_components.htm) statement has special variants for dynamically accessing structure components.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Populating Structures 

You can copy the content of a structure to another using the [assignment operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm) `=`. 
In the following example, it is assumed that the target and source structure are of compatible types. In general, note that special [conversion](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_struc.htm) and [comparison rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_rules_operands_struc.htm) apply to value assignments involving structures.
``` abap
some_struc = another_struc.

"When creating a new structure by inline declaration, the type of
"the right-hand structure is derived and the content is assigned.

DATA(struc_inl) = some_struc.
```

To assign values to individual structure components, use the component selector.
``` abap
TYPES: BEGIN OF addr_struc,
        name   TYPE string,
        street TYPE string,
        city   TYPE string,
       END OF addr_struc.

DATA address TYPE addr_struc.

address-name   = `Mr. Duncan Pea`.

address-street = `Vegetable Lane 11`.

address-city   = `349875 Botanica`.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using the VALUE Operator

- The [`VALUE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_value.htm) operator can be used to construct the content of complex data objects such as structures or internal tables. 
- It is particularly useful because assigning values by addressing the structure components individually can be very cumbersome, especially when assigning values to structure components at the [operand position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm "Glossary Entry").
- If the type of the operand can be inferred implicitly, the `#` character can be used used before the parentheses. Otherwise, the type must be  specified explicitly. 
- The `VALUE` operator and inline declarations can be used to create and populate structures in one go.
- Note that there are special [conversion](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_struc.htm) and [comparison](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_rules_operands_struc.htm) rules for structures. See the ABAP Keyword Documentation for more details.


``` abap
"# used: type of the operand can be implicitly derived
address = VALUE #( name   = `Mr. Duncan Pea`
                   street = `Vegetable Lane 11`.
                   city   = `349875 Botanica` ).

"Declaring a structure inline
"Type used explicitly: type of the operand cannot be implicitly derived
DATA(addr) = VALUE addr_struc( name   = `Mr. Duncan Pea`
                               street = `Vegetable Lane 11`.
                               city   = `349875 Botanica` ).

"Nesting value operators
TYPES: BEGIN OF struc_nested,
        a TYPE i,
        BEGIN OF nested_1,
          b TYPE i,
          c TYPE i,
        END OF nested_1,
        BEGIN OF nested_2,
          d TYPE i,
          e TYPE i,
        END OF nested_2,
      END OF struc_nested.

DATA str_1 TYPE struc_nested.

str_1 = VALUE #( a        = 1 
                 nested_1 = VALUE #( b = 2 c = 3 ) 
                 nested_2 = VALUE #( d = 4 e = 5 ) ).

"Inline declaration
"Component a is not specified here, i.e. its value remains initial.
DATA(str_2) = VALUE struc_nested( nested_1 = VALUE #( b = 2 c = 3 ) 
                                  nested_2 = VALUE #( d = 4 e = 5 ) ).

"Apart from the VALUE operator, the NEW operator can be used to create
"a data reference variable (and populate the structure)
DATA(str_ref) = NEW struc_nested( a        = 1 
                                  nested_1 = VALUE #( b = 2 c = 3 ) 
                                  nested_2 = VALUE #( d = 4 e = 5 ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using the CORRESPONDING Operator and MOVE-CORRESPONDING Statements

- You can use statements with [`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove-corresponding.htm)
and the [`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm) operator to assign values to structure components, especially when assigning values from a source structure to a target structure which have incompatible types and/or differently named components. 
- Both are used to assign identically named components of structures to each other. 
- The syntax also works for structures of the same type.
- Also note the special [conversion](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_struc.htm) and [comparison](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_rules_operands_struc.htm) rules for structures in this context.

> **💡 Note**<br>
>- The [`CL_ABAP_CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencl_abap_corresponding.htm) system class is available for making assignments. See the ABAP Keyword Documentation for the details.
>- The `INTO` clause of ABAP SQL statements has the `CORRESPONDING` addition. There, the following basic rule applies, which affects the value assignment: Without the `CORRESPONDING ...` addition, column names do not matter, only the position. With the `CORRESPONDING ...` addition, the position of the columns does not matter, only the name. See examples in the ABAP SQL cheat sheet.

The following examples demonstrate the value assignment using `MOVE-CORRESPONDING` statements and the `CORRESPONDING` operator with various additions. 
The focus is on flat structures only.

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

diff_struc = CORRESPONDING #( BASE ( diff_struc ) struc MAPPING comp1 = compa ).

"EXCEPT addition: Excluding components from the assignment.

diff_struc = CORRESPONDING #( BASE ( diff_struc ) struc EXCEPT comp1 ).
```

Value assignments in deep structures 
- In the context of deep structures, there are additional syntax variants available for [`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove-corresponding.htm) statements and the [`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm).
- The follwing examples focus on internal tables as structure components. For the effect, see the executable example.

``` abap
"Nonidentical elementary component types are kept in target
"structure which is true for the below MOVE-CORRESPONDING statements;
"existing internal table content is replaced by content of
"the source table irrespective of identically named components

MOVE-CORRESPONDING deep_struc TO diff_deep_struc.

"Existing internal table content is replaced but the value
"assignment happens for identically named components only.

MOVE-CORRESPONDING deep_struc TO diff_deep_struc EXPANDING NESTED TABLES.

"Existing internal table content is kept; table content of the source
"structure are added but the value assignment happens like the first
"MOVE-CORRESPONDING statement without further syntax additions.

MOVE-CORRESPONDING deep_struc TO diff_deep_struc KEEPING TARGET LINES.

"Existing internal table content is kept; table content of the source
"structure are added; the value assignment happens like the statement
"MOVE-CORRESPONDING ... EXPANDING NESTED TABLES.

MOVE-CORRESPONDING deep_struc TO diff_deep_struc EXPANDING NESTED TABLES KEEPING TARGET LINES.

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

<p align="right"><a href="#top">⬆️ back to top</a></p>
                                  
## Clearing Structures
You can reset individual components to their initial values and clear the
entire structure using the [`CLEAR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclear.htm) keyword.
``` abap
CLEAR struc-component.

CLEAR struc.

"Note: An assignment using the VALUE operator without entries in the parentheses clears the structure. 
struc = VALUE #( ). 
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Processing Structures
Structures are primarily used to process data from tables. In this context, structures often take on the role of a [work area](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwork_area_glosry.htm "Glossary Entry"). 
The following code snippets cover only a selection. For more examples, see the cheat sheets about internal tables and ABAP SQL. 

**Reading a row from a database table into a structure that has a compatible type**. Note that, since database tables are flat, the
target structure must also be flat. In the example below, the [`SINGLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_single.htm)
addition reads only a single row into the structure. It returns the first entry that matches the `WHERE` condition.

``` abap
"Creating a structure with a compatible type
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
**Reading a row from a database table into a structure that has an incompatible type**. 
Components in the structure with identical names are filled.

``` abap
SELECT SINGLE FROM zdemo_abap_fli
  FIELDS *
  WHERE carrid = 'AA'
  INTO CORRESPONDING FIELDS OF @ls_fli_diff.
```
**Reading a line from an internal table into a structure** ...

... using a `SELECT` statement. Note the specified alias name and that ABAP variables like internal tables must be escaped with `@`. The addition `INTO CORRESPONDING FIELDS OF` also applies here.
``` abap
SELECT SINGLE FROM @itab AS itab_alias
  FIELDS *
  WHERE ...
  INTO @DATA(ls_struc).
  "INTO CORRESPONDING FIELDS OF @some_existing_struc.
```
... using a `READ TABLE` statement. The code snippet below shows the reading of a line into a [work area](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwork_area_glosry.htm "Glossary Entry"), a [field symbol](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm "Glossary Entry"), and a [data reference variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry"), all of which 
represent structured data objects that are declared inline. In the following example, a line is read based on the line number by
specifying `INDEX`. For more details, see the section *Determining the target area* in the cheat sheet [Internal Tables](01_Internal_Tables.md#).
``` abap
READ TABLE itab INTO DATA(wa) INDEX 1.

READ TABLE itab ASSIGNING FIELD-SYMBOL(<fs>) INDEX 2.

READ TABLE itab REFERENCE INTO DATA(dref) INDEX 3.
```
... using a [table expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_expression_glosry.htm "Glossary Entry").
The code snippet shows how to read a line into a structure declared inline. The index is given in square brackets.
``` abap
DATA(ls_table_exp) = itab[ 3 ].
```

**Sequentially reading** ...

... a row from a database table into a structure. A `SELECT` loop can be specified with the syntax `SELECT ... ENDSELECT.`.
In the following example, the row found and returned in a structure declared inline can be processed further.
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
... a line from an internal table into a structure using a [`LOOP AT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab_variants.htm) statement. There are many ways to specify the condition on which the loop is based. The following example covers the option of reading all lines sequentially into a field symbol  declared inline. When using a field symbol, you can, for example, directly modify components.
``` abap
LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs>).
  <fs>-comp1 = ...
  ...
ENDLOOP.
```

**Inserting a single row into a database table from a structure** using  ABAP SQL statements with
[`INSERT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinsert_dbtab.htm). The following statements can be considered as alternatives. The third statement shows that instead of inserting a row from an existing structure, you can create and fill a structure directly.
Note that you should avoid inserting a row with a particular key into the database table if a row with the same key already exists.
``` abap
INSERT INTO dbtab VALUES @struc.

INSERT dbtab FROM @struc.

INSERT dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ).
```
**Updating a single row in a database table from a structure** using ABAP SQL statements with [`UPDATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapupdate.htm). Note that this syntax changes the entire row and all of its components.
``` abap
UPDATE dbtab FROM @struc.

UPDATE dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ).
```
If you want to update a database table row from a structure by specifying components to be changed without overwriting other components, you can use the following method. First, read the desired row from the database table into a structure. Then, use the `VALUE` operator with the `BASE` addition and specify the components to be changed.
``` abap
SELECT SINGLE *
  FROM dbtab
  WHERE ...
  INTO @DATA(wa).

UPDATE dbtab FROM @( VALUE #( BASE wa comp2 = ... comp4 = ... ) ).
```
**Updating or creating a single row in a database table from a structure** using ABAP SQL statements with
[`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_dbtab.htm). If a row with the same key as specified in the structure already exists in the database table, the row is updated. If no row with the keys specified in the structure exists, a new row is created in the database table.
``` abap
MODIFY dbtab FROM @struc.

MODIFY dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ).
```
**Adding lines to and updating single lines in an internal table from a structure** using `INSERT`,
`APPEND`, and `MODIFY` statements. 
- Note that all statements, including `INSERT` and `MODIFY`, are ABAP statements in this context, not ABAP SQL statements.
- Both `INSERT` and `APPEND` add one or more lines to an internal table. While `APPEND` adds at the bottom of the
internal table, `INSERT` can be used to add lines at a specific position in the table. If you do not specify the position, the lines are also added at the bottom of the table. However, unlike `APPEND`, `INSERT` does not set `sy-tabix`. 
- `MODIFY` changes the content of an internal table entry.
- Statements using the `VALUE` operator to directly create and populate the structures are also possible. For more information and code
snippets, see the [Internal Tables](01_Internal_Tables.md#) cheat sheet.
``` abap
INSERT struc INTO TABLE itab.

APPEND struc TO itab.

MODIFY TABLE itab FROM struc.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursion: Including Structures

- Although their use is not recommended in the ABAP programming
guidelines, you may come across [`INCLUDE TYPE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinclude_type.htm)
and [`INCLUDE STRUCTURE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinclude_type.htm) statements 
in the context of local structures. 
- Structured data objects and types created with `... BEGIN OF... END OF ...` can use this syntax to include components of another structure, whether it is a locally defined or global structure, without creating  substructures. 
- `INCLUDE TYPE` can be used to include a structured type. 
- You can use `INCLUDE STRUCTURE` to include a structure.

> **💡 Note**<br>
> - They are not additions of `... BEGIN OF ... END OF ...` but individual ABAP statements.
> - If you use a chained statement with a colon to declare the structure, the inclusion of other structures with these statements interrupts the chained statement, that is, the components of the included structures are included as direct components of the [superstructure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensuperstructure_glosry.htm "Glossary Entry").
>- By using the optional `AS` addition and specifying a name, the included components can be addressed by this common name as if they were actually components of a substructure.
>- The optional `RENAMING WITH SUFFIX` addition, followed by a name, gives the included components a suffix name to avoid naming conflicts with other components.

The following example shows how structured types and data objects are included in another structure. First, three structured types and a structured data object based on one of these types are created. Then, the types and the structure are included in the structured type `address_type`. The executable example demonstrates a structure that includes other structures in this way.
``` abap
TYPES: BEGIN OF name_type,
        title   TYPE string,
        prename TYPE string,
        surname TYPE string,
      END OF name_type,
      BEGIN OF street_type,
        name TYPE string,
        num  TYPE string,
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


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example
[zcl_demo_abap_structures](./src/zcl_demo_abap_structures.clas.abap)

Follow the steps outlined [here](README.md#-getting-started-with-the-examples) to import and run the code.