<a name="top"></a>

# Structures

- [Structures](#structures)
  - [Introduction](#introduction)
  - [Globally Available Structures and Structured Types](#globally-available-structures-and-structured-types)
  - [Creating Structures and Structured Types Locally](#creating-structures-and-structured-types-locally)
    - [Creating Structured Types](#creating-structured-types)
    - [Creating Structures](#creating-structures)
    - [Creating Structures Using Existing Structured Types](#creating-structures-using-existing-structured-types)
    - [Creating Structures by Inline Declaration](#creating-structures-by-inline-declaration)
    - [Creating Constant and Immutable Structures](#creating-constant-and-immutable-structures)
    - [Creating Enumerated Structures](#creating-enumerated-structures)
    - [Creating Anonymous Structures](#creating-anonymous-structures)
  - [Variants of Structures](#variants-of-structures)
  - [Accessing (Components of) Structures](#accessing-components-of-structures)
    - [ASSIGN Statements](#assign-statements)
  - [Populating Structures](#populating-structures)
    - [Using the VALUE Operator](#using-the-value-operator)
    - [Using the NEW Operator](#using-the-new-operator)
    - [Using the CORRESPONDING Operator and MOVE-CORRESPONDING Statements](#using-the-corresponding-operator-and-move-corresponding-statements)
  - [Clearing Structures](#clearing-structures)
  - [Processing Structures](#processing-structures)
    - [Structures in ABAP SQL Statements](#structures-in-abap-sql-statements)
    - [Structures in Statements for Processing Internal Tables](#structures-in-statements-for-processing-internal-tables)
  - [Including Structures](#including-structures)
  - [Excursions](#excursions)
    - [sy Structure](#sy-structure)
    - [Getting Structured Type Information and Creating Structures at Runtime](#getting-structured-type-information-and-creating-structures-at-runtime)
    - [Boxed Components](#boxed-components)
  - [Executable Example](#executable-example)

## Introduction
Structures ...

-   are [data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm "Glossary Entry")
    with [structured data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstructured_type_glosry.htm "Glossary Entry") (which is a [complex data type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplex_data_type_glosry.htm "Glossary Entry") because it is composed of other data types). 
-   consist of a sequence of [components](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomponent_glosry.htm "Glossary Entry") of any data type, that is, the components of a structure can be, for example, [elementary data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenelementary_data_object_glosry.htm), structures themselves, [internal tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninternal_table_glosry.htm "Glossary Entry") or [references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_glosry.htm).
- are used to combine different data objects that belong together. A typical example is an address. It has several components, such as name, street, city, and so on, that belong together.
- play an important role in the context of internal tables and [database tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendatabase_table_glosry.htm "Glossary Entry"). Structured types serve as [line types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrow_type_glosry.htm) for these tables. Most internal tables across [ABAP programs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm) may have structured line types. For database tables, there is no alternative to structured line types.
- can be created locally in an ABAP program and globally. This cheat sheet focuses on locally defined structures and structured types.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Globally Available Structures and Structured Types

- Apart from the local declaration of a structured type, you can create such a type, for example, as global [DDIC structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_structure_glosry.htm) in the [ABAP Dictionary](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_dictionary_glosry.htm). Such a DDIC structure defines a globally available structured type ([DDIC type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_type_glosry.htm)).
- There are other structured types available globally, which may be the structured types most commonly used in ABAP programs:
    - [Database tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_db_table_glosry.htm) defined in the ABAP Dictionary can be used as data types just like DDIC structures in an ABAP program. This means that when you create a structure in your ABAP program, for example, you can simply use the name of a database table to address the line type of the table. The structure you created will then have the same structured type as the database table. Typically, you use the database tables to create structures of such a type, or internal tables of such a structured line type, to process data read from the database table in structures or internal tables.     
    - Various [CDS entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_entity_glosry.htm) are globally available structured types. For example, a [CDS view entity](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_v2_view_glosry.htm) represents a structured data type and can be used as such in ABAP programs (but not in the ABAP Dictionary). 
    - Structures and structured data types can be defined in the public [visibility section](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenvisibility_section_glosry.htm) of [global classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_class_glosry.htm) or in [global interfaces](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_interface_glosry.htm) and then used globally.

```abap
"Creating structures based on globally available structured types
"Database table
DATA struc_from_dbtab TYPE zdemo_abap_fli.
"CDS view entity
DATA struc_from_cds_ve TYPE zdemo_abap_fli_ve.
"CDS abstract entity
DATA struc_from_cds_abs TYPE zdemo_abap_abstract_ent.
"CDS table function
DATA struc_from_cds_tab_func TYPE zdemo_abap_table_function.

"Globally available structured type in the public visibility section of
"classes/interfaces
DATA struc_from_struc_type_in_cl TYPE zcl_demo_abap_amdp=>fli_struc.

"Creating structured types based on globally available structured types
TYPES ty_struc_from_dbtab TYPE zdemo_abap_fli.
TYPES ty_struc_from_cds_ve TYPE zdemo_abap_fli.
```

> [!NOTE] 
> - This cheat sheet focuses on locally defined structures and structured types.
> - Classic [DDIC views](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_view_glosry.htm) are not available in [ABAP Cloud](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_cloud_glosry.htm). They can only be used as structured types in [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Creating Structures and Structured Types Locally

The typical language elements for creating structures and structured types locally in an ABAP program are [`BEGIN OF ... END OF ...`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes_struc.htm). They are used in combination with the [`TYPES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes.htm) keyword to create a structured type and the [`DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata.htm) keyword to create a structure.

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Creating Structured Types

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

Alternatively, you can also use the following syntax. However, a chained statement may provide better readability.
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


> [!NOTE] 
> Outside of classes, you can also refer to DDIC types using `LIKE` (`... comp11 LIKE ddic_type, ...`). If you actually want to refer to an existing data object, but due to typing errors you inadvertently specify a name that exists as DDIC type, errors may be unavoidable.


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Structures

- To create a structure (i.e. a structured data object) in an ABAP program, you can use the `DATA` keyword. 
- It works in the same way as the `TYPES` statement above. 
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

Alternatively, you can use the following syntax. Similar to above, a chained statement may provide better readability.

``` abap
DATA BEGIN OF struc.
  DATA comp1 TYPE ... .
  DATA comp2 TYPE ... VALUE ... .
... .
DATA END OF struc.
```

> [!NOTE]  
>-  The keywords [`CLASS-DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass-data.htm) and [`CONSTANTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconstants.htm) can also be used to create structures. In principle, they represent special cases of the general statement shown above. See the ABAP Keyword Documentation for more information. 
>- Structures can also be created [inline](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm) using [`DATA(...)`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_inline.htm) or [`FINAL(...)`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm), as shown below.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Structures Using Existing Structured Types

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Structures by Inline Declaration 

- This is particularly useful for declaring data objects at the [operand positions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm) where you actually need them. 
- In this way, you can avoid an extra declaration of the structure in different contexts.
- You can use the declaration operator using `DATA(...)`. The [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm) declaration operator is used to create [immutable variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenimmutable_variable_glosry.htm).
- You can also create structures using the `VALUE` operator (and also fill them as shown below). Without specifying component values in the parentheses, you create an initial structure. 

``` abap
"Structures created inline instead of an extra declared variable
DATA struc_9 LIKE struc_1.
struc_9 = struc_1

"Type is derived from the right-hand structure; the content of struc is assigned, too.
DATA(struc_10) = struc_1.
FINAL(struc_11) = struc_9.

"Using the VALUE operator
"A structure declaration as follows (without providing component 
"value assignments) ...
DATA(struc_a) = VALUE struc_type( ).

"... is similar to the following declaration.
DATA struc_b TYPE struc_type.

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

###  Creating Constant and Immutable Structures

- Constant structures can be created with the `... BEGIN OF ... END OF ...` additions. Their values cannot be changed.
- As shown above, the [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm) declaration operator is used to create [immutable variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenimmutable_variable_glosry.htm).

```abap
CONSTANTS: BEGIN OF const_struct,
             num TYPE i VALUE 123,
             str TYPE string VALUE `ABAP`,
             n3  TYPE n LENGTH 3 VALUE '000',
             c5  TYPE c LENGTH 5 VALUE 'abcde',
           END OF const_struct.

DATA(num) = const_struct-num.
"const_struct-num = 456.

TYPES struct_type LIKE const_struct.
FINAL(final_struct) = VALUE struct_type( num = 987 str = `hello` n3 = '123' c5 = 'xyz' ).

DATA(num_from_final) = final_struct-num.
"final_struct-num = 1.

SELECT * FROM zdemo_abap_carr INTO TABLE @DATA(itab).

"The work area is specified as immutable variable. The variable's content cannot be changed
"in the loop, however, the variable is exchanged with the next loop pass.
LOOP AT itab INTO FINAL(wa).
  DATA(carrid) = wa-carrid.
  "wa-carrid = 'XY'.
ENDLOOP.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Enumerated Structures

Find more information on [enumerated types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenum_type_glosry.htm) in the [Data Types and Data Objects](16_Data_Types_and_Objects.md#abap-enumerated-types-and-objects) cheat sheet.

```abap
"When creating enumerated types, an enumerated structure can optionally be declared in 
"the context of the type declaration.
"A component of an enumerated structure: An enumerated constant that exists as a component
"of a constant structure, not as a single data object.
TYPES basetype TYPE i.
TYPES: BEGIN OF ENUM t_enum_struc STRUCTURE en_struc BASE TYPE basetype,
         a VALUE IS INITIAL,
         b VALUE 1,
         c VALUE 2,
         d VALUE 3,
       END OF ENUM t_enum_struc STRUCTURE en_struc.

DATA(enum_comp) = en_struc-b.

DATA(conv_enum_comp) = CONV basetype( en_struc-b ).
ASSERT conv_enum_comp = 1.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Anonymous Structures

Using the instance operator [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm) and [`CREATE DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_data.htm) statements, you can create [anonymous data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm "Glossary Entry"), such as anonymous structures. 
The `NEW` addition of the `INTO` clause of an ABAP SQL `SELECT` statement also creates an anonymous data object. 
As outlined below, you can access the components or the entire data objects by [dereferencing](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm). 
For more information, refer to the [Dynamic Programming](06_Dynamic_Programming.md) and [Constructor Expressions](05_Constructor_Expressions.md) cheat sheets.

```abap
"Without assigning component values in the parentheses, the anonymous
"structure is initial.
DATA(struc_ref_a) = NEW struc_type( ).

DATA struc_ref_b TYPE REF TO DATA.
struc_ref_b = NEW struc_type( ).

"Multiple syntax options are available for CREATE DATA
"statements. See the cheat sheets mentioned.
CREATE DATA struc_ref_b TYPE struc_type.

DATA struc_ref_c TYPE REF TO struc_type.
"Implicit data type definition
CREATE DATA struc_ref_c.

"NEW addition of the INTO clause of an ABAP SQL SELECT statement
SELECT SINGLE carrid, carrname
 FROM zdemo_abap_carr
 WHERE carrid = char`LH`
 INTO NEW @DATA(struc_ref_d).
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

- **Deep structures**: Contain at least one internal table, reference type, or string as a component.
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

> [!NOTE]  
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
... ref_struc_2->comp2 ... 
... ref_struc_2->*-comp2 ...  "Using the dereferencing operator
```

Nested components can be addressed using chaining:
``` abap
... struc-substructure-comp1 ...
... address_n-name-title ...
```

> [!NOTE]  
> There are syntax options for dynamically accessing structure components. See the [Dynamic Porgramming](06_Dynamic_Programming.md) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### ASSIGN Statements

```abap
"A field symbol is set using an assignment of a memory area to the 
"field symbol by ASSIGN statements.
"Particularly, field symbols and ASSIGN statements are supporting elements for 
"dynamic programming. ASSIGN statements have multiple additions. Find more information 
"and examples in the Dynamic Programming cheat sheet.

TYPES: BEGIN OF s,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 3,
         comp3 TYPE n LENGTH 5,
         comp4 TYPE string,
       END OF s.

DATA(demo_struc) = VALUE s( comp1 = 1 comp2 = 'abc' comp3 = '12345' comp4 = `ABAP` ).

"Defining a field symbol
FIELD-SYMBOLS <a> TYPE s.

"Assigning the entire structure to a field symbole
ASSIGN demo_struc TO <a>.

"Accessing a structure component via the field symbol
DATA(comp1) = <a>-comp1.

"Field symbol declared inline
"Note: The typing depends on the memory area specified. In this case,
"the field symbol <fd> has the structured type s. This is valid for static
"assignments. In case of dynamic assignments, the type is the generic type
"data.
ASSIGN demo_struc TO FIELD-SYMBOL(<b>).
comp1 = <b>-comp1.

"Accessing components of structures by assigning components to field symbols
"The field symbols is typed with the generic type data so that all components,
"which have random types, can be assigned.
FIELD-SYMBOLS <d> TYPE data.
ASSIGN demo_struc-comp1 TO <d>.
ASSIGN demo_struc-comp2 TO <d>.
ASSIGN demo_struc-comp3 TO <d>.
ASSIGN demo_struc-comp4 TO <d>.

"Accessing structures and components dynamically
"Note: In case of dynamic assignments, ...
"- sy-subrc is set.
"- the type of field symbols declared inline is the generic type data.
ASSIGN ('DEMO_STRUC') TO FIELD-SYMBOL(<c>).
ASSERT sy-subrc = 0.

"Using ASSIGN COMPONENT statements
"It is recommended to use the newer syntax, which specifies the component 
"selector followed by a data object in a pair of parentheses.
"After COMPONENT (and in the parentheses in the newer syntax), a character-like 
"or numeric data object is expected.
DATA(some_comp) = `COMP2`.
ASSIGN COMPONENT some_comp OF STRUCTURE demo_struc TO <d>.
ASSERT sy-subrc = 0.

some_comp = `COMP5`.
ASSIGN COMPONENT some_comp OF STRUCTURE demo_struc TO <d>.
ASSERT sy-subrc = 4.

"Numeric data objects
"The data object is implicitly converted to type i (if required) and
"interpreted as the position of the component in the structure.
ASSIGN COMPONENT 1 OF STRUCTURE demo_struc TO <d>.
ASSERT sy-subrc = 0.

ASSIGN COMPONENT 5 OF STRUCTURE demo_struc TO <d>.
ASSERT sy-subrc = 4.

"0 means that the entire structure is assigned
ASSIGN COMPONENT 0 OF STRUCTURE demo_struc TO <d>.
ASSERT sy-subrc = 0.
ASSERT <d> = demo_struc.

"Newer syntax using the component selector followed by content in
"a pair of parentheses.
ASSIGN demo_struc-('COMP4') TO <d>.
ASSIGN demo_struc-(some_comp) TO <d>.
ASSIGN demo_struc-(3) TO <d>.
ASSIGN demo_struc-(0) TO <d>.

"Iterating across all structure components
DO.
  ASSIGN demo_struc-(sy-index) TO <d>.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
ENDDO.
``` 

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Populating Structures 

You can copy the content of a structure to another using the [assignment operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm) `=`. 
In the following example, it is assumed that the target and source structures are of compatible types. In general, note that special [conversion](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_struc.htm) and [comparison rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_rules_operands_struc.htm) apply to value assignments involving structures.
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
                   street = `Vegetable Lane 11`
                   city   = `349875 Botanica` ).

"Declaring a structure inline
"Type used explicitly: type of the operand cannot be implicitly derived
DATA(addr) = VALUE addr_struc( name   = `Mr. Duncan Pea`
                               street = `Vegetable Lane 11`
                               city   = `349875 Botanica` ).


"Using the BASE addition to retain existing component values
addr = VALUE #( BASE addr street = `Some Street 1` ).
*NAME              STREET           CITY           
*Mr. Duncan Pea    Some Street 1    349875 Botanica

"Without the BASE addition, the components are initialized
addr = VALUE #( street = `Another Street 2` ).
*NAME       STREET              CITY   
*           Another Street 2           

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

### Using the NEW Operator

Using the instance operator [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm), you can create [anonymous data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm "Glossary Entry"), such as anonymous structures. You can access the components or the entire data objects by [dereferencing](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm). For more information, refer to the  [Dynamic Programming](06_Dynamic_Programming.md) and [Constructor Expressions](05_Constructor_Expressions.md) cheat sheets.

```abap
"Creating a data reference variable 
DATA addr_ref1 TYPE REF TO addr_struc.

"Populating the anonymous structure
addr_ref1 = NEW #( name   = `Mr. Duncan Pea`
                   street = `Vegetable Lane 11`
                   city   = `349875 Botanica` ).

addr_ref1->name = `Mrs. Jane Doe`.     

"Declaring an anonymous structure/a data reference variable inline    
DATA(addr_ref2) = NEW addr_struc( name   = `Mr. Duncan Pea`
                                  street = `Vegetable Lane 11`
                                  city   = `349875 Botanica` ).

addr_ref2->* = VALUE #( BASE addr_ref2->* name = `Mr. John Doe` ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using the CORRESPONDING Operator and MOVE-CORRESPONDING Statements

- You can use statements with [`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove-corresponding.htm)
and the [`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm) operator to assign values to structure components, especially when assigning values from a source structure to a target structure which have incompatible types and/or differently named components. 
- Both are used to assign identically named components of structures to each other. 
- The syntax also works for structures of the same type.
- Also note the special [conversion](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_struc.htm) and [comparison](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_rules_operands_struc.htm) rules for structures in this context.

> [!NOTE]  
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
- In the context of deep structures, there are additional syntax variants available for [`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove-corresponding.htm) statements and the [`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm) operator.
- The following examples focus on internal tables as structure components. Check out the syntax in action in the executable example.

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
entire structure using the [`CLEAR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclear.htm) keyword. Note that [`FREE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfree_dataobject.htm) statements also deletes the content, but they also release the initially allocated memory.
space. 

``` abap
CLEAR struc-component.

CLEAR struc.

"This statement additionally releases memory space.
FREE struc.

"Note: An assignment using the VALUE operator without entries in the parentheses clears the structure. 
struc = VALUE #( ). 

"The same applies to data reference variables pointing to structures.
struc_ref = NEW #( ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Processing Structures
Structures are primarily used to process data from tables. In this context, structures often take on the role of a [work area](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwork_area_glosry.htm "Glossary Entry"). 
The following code snippets cover only a selection. For more examples, see the cheat sheets about internal tables and ABAP SQL. 

### Structures in ABAP SQL Statements

The following code snippets cover a selection. Find more information and code snippets in the [ABAP SQL](03_ABAP_SQL.md) cheat sheet.


<table>
<tr>
<td> Subject </td> <td> Notes </td>
</tr>

<tr>
<td> 
Reading a row from a database table into a structure that has a compatible type
</td>
<td>

Note that, since database tables are flat, the
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
</td>
</tr>

<tr>
<td> 
Reading a row from a database table into a structure that has an incompatible type
</td>
<td>

Components in the structure with identical names are filled.

``` abap
SELECT SINGLE FROM zdemo_abap_fli
  FIELDS *
  WHERE carrid = 'AA'
  INTO CORRESPONDING FIELDS OF @ls_fli_diff.
```  
</td>
</tr>

<tr>
<td> 

Reading a line from an internal table into a structure using an ABAP SQL `SELECT` statement
</td>
<td>

Note the specified alias name and that ABAP variables like internal tables must be escaped with `@`. The addition `INTO CORRESPONDING FIELDS OF` also applies here.
``` abap
SELECT SINGLE FROM @itab AS itab_alias
  FIELDS *
  WHERE ...
  INTO @DATA(ls_struc).
  "INTO CORRESPONDING FIELDS OF @some_existing_struc.
```
</td>
</tr>


<tr>
<td> 
Sequentially reading a row from a database table into a structure
</td>
<td>

A `SELECT` loop can be specified with the syntax `SELECT ... ENDSELECT.`.
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
</td>
</tr>

<tr>
<td> 

Inserting a single row into a database table from a structure using  ABAP SQL statements with
[`INSERT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinsert_dbtab.htm)
</td>
<td>

The following statements can be considered as alternatives. The third statement shows that instead of inserting a row from an existing structure, you can create and fill a structure directly.
Note that you should avoid inserting a row with a particular key into the database table if a row with the same key already exists. Note that with this and the followig syntax, various options/expressions are possible.
``` abap
INSERT INTO dbtab VALUES @struc.

INSERT dbtab FROM @struc.

INSERT dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ).
```
</td>
</tr>

<tr>
<td> 

Updating a single row in a database table from a structure using ABAP SQL statements with [`UPDATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapupdate.htm)
</td>
<td>

Note that this syntax changes the entire row and all of its components.
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
</td>
</tr>

<tr>
<td> 

Updating or creating a single row in a database table from a structure using ABAP SQL statements with
[`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_dbtab.htm) 
</td>
<td>

If a row with the same key as specified in the structure already exists in the database table, the row is updated. If no row with the keys specified in the structure exists, a new row is created in the database table.
``` abap
MODIFY dbtab FROM @struc.

MODIFY dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ).
```
</td>
</tr>

<tr>
<td> 

Deleting a single row in a database table from a structure using ABAP SQL statements with
[`DELETE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdelete_dbtab.htm) 
</td>
<td>

If a row with the same key as specified in the structure already exists in the database table, the row is updated. If no row with the keys specified in the structure exists, a new row is created in the database table.
``` abap
DELETE dbtab FROM @struc.

DELETE dbtab FROM @( VALUE #( comp1 = ... ) ).
```
</td>
</tr>

</table>

### Structures in Statements for Processing Internal Tables

The following code snippets cover a selection. Find more information and code snippets in the [Internal Tables](01_Internal_Tables.md) cheat sheet.


<table>
<tr>
<td> Subject </td> <td> Notes </td>
</tr>



<tr>
<td>

Reading a line from an internal table into a structure using a `READ TABLE` statement
</td>
<td>

The code snippet below shows the reading of a line into a [work area](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwork_area_glosry.htm "Glossary Entry"), a [field symbol](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm "Glossary Entry"), and a [data reference variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry"), all of which 
represent structured data objects that are declared inline. In the following example, a line is read based on the line number by
specifying `INDEX`. For more details, see the section *Determining the target area* in the cheat sheet [Internal Tables](01_Internal_Tables.md#).
``` abap
READ TABLE itab INTO DATA(wa) INDEX 1.

READ TABLE itab ASSIGNING FIELD-SYMBOL(<fs>) INDEX 2.

READ TABLE itab REFERENCE INTO DATA(dref) INDEX 3.
``` 
</td>
</tr>


<tr>
<td>

Reading a line from an internal table into a structure using a [table expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_expression_glosry.htm "Glossary Entry")
</td>
<td>

The code snippet shows how to read a line into a structure declared inline. The index is given in square brackets. You can also specify table keys and free keys.
``` abap
DATA(ls_table_exp) = itab[ 3 ].
```
</td>
</tr>

<tr>
<td> 

Sequentially reading a line from an internal table into a structure using a [`LOOP AT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab_variants.htm) statement
</td>
<td>

There are many ways to specify the condition on which the loop is based. The following example covers the option of reading all lines sequentially into a field symbol  declared inline. When using a field symbol, you can, for example, directly modify components.
``` abap
LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs>).
  <fs>-comp1 = ...
  ...
ENDLOOP.
```
</td>
</tr>

<tr>
<td> 

Adding lines to and updating single lines in an internal table from a structure using `INSERT`,
`APPEND`, and `MODIFY` statements
</td>
<td>

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
</td>
</tr>

</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Including Structures

- [`INCLUDE TYPE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinclude_type.htm)
and [`INCLUDE STRUCTURE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinclude_type.htm) statements 
are used in the context of local structures. 
- Structured data objects and types created with `... BEGIN OF... END OF ...` can use this syntax to include components of another structure, whether it is a locally defined or global structure, without creating  substructures. 
- `INCLUDE TYPE` can be used to include a structured type. 
- You can use `INCLUDE STRUCTURE` to include a structure.

> [!NOTE]  
> - They are not additions of `... BEGIN OF ... END OF ...` but individual ABAP statements.
> - If you use a chained statement with a colon to declare the structure, the inclusion of other structures with these statements interrupts the chained statement, that is, the components of the included structures are included as direct components of the [superstructure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensuperstructure_glosry.htm "Glossary Entry").
>- By using the optional `AS` addition and specifying a name, the included components can be addressed by this common name as if they were actually components of a substructure.
>- The optional `RENAMING WITH SUFFIX` addition, followed by a name, gives the included components a suffix name to avoid naming conflicts with other components.

The following example shows how structured types and data objects are included in another structure. First, three structured types and a structured data object based on one of these types are created. Then, the types and the structure are included in the structured type `address_type`. As an excursion, [Runtime Type Identification](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm) is used to retrieve the component names of created structured type `address_type`. Refer to the [Getting Structured Type Information and Creating Structures at Runtime](#getting-structured-type-information-and-creating-structures-at-runtime) section. The executable example demonstrates a structure that includes other structures in this way.
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

DATA(component_names) = VALUE string_table( FOR wa IN CAST cl_abap_structdescr(
   cl_abap_typedescr=>describe_by_name( 'ADDRESS_TYPE' ) )->components ( CONV #( wa-name ) ) ).

*Content of COMPONENT_NAMES:
*TITLE         
*PRENAME       
*SURNAME       
*NAME_STREET   
*NUM_STREET    
*ZIPCODE_CITY  
*NAME_CITY     
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions

### sy Structure

- The `sy` (or `syst`) structure is a built-in data object. 
- The components of the structure represent ABAP system fields. 
- These fields, filled by the [ABAP runtime framework](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_runtime_frmwk_glosry.htm), can be used to query system information and more. 
- Typically, they should only be read, and not overwritten. 
- Prominent system fields are the following 
  - `sy-subrc`: Return code of many ABAP statements; typically, the value 0 indicates success
  - `sy-tabix`: Row index of internal tables
  - `sy-index`: Loop pass index
- These ones and others can be used in [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm). However, most of the fields should not be used in ABAP for Cloud Development (indicated by a syntax warning) because they refer to [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm) contexts (e.g. classic dynpros and lists), or their values are not relevant in a cloud context. 
- More information about the purpose of the individual components is available at [ABAP System Fields (F1 documentation for Standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensystem_fields.htm).


The following example demonstrates a selection of ABAP system fields. It uses artifacts from the ABAP cheat sheet repository. Note the comments in the code because a syntax warning will be displayed when inserting the code in a demo class that uses ABAP for Cloud Development. It is meant to emphasize that multiple system fields should not be used in ABAP for Cloud Development.  
To try the example out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console. 

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

    "In ABAP for Cloud Development, the following statement will show a syntax warning saying that
    "sy should not be used. Here, it is used for demonstration purposes.
    "In the example, RTTI is used to get all component names of the built-in data object sy. In the loop,
    "ABAP statements are created (they represent simple assignments using the various sy components) and
    "output to the console. You can copy all the output DATA(...) = ... statements from the console and 
    "paste them in the demo class's main method implementation. The purpose is to demonstrate that most of 
    "the sy components should not be used in ABAP for Cloud Development. Most of the statements will show 
    "a syntax warning in ABAP for Cloud Development. Check the ABAP Keyword Documentation (for Standard ABAP) 
    "and the F2 information for the purpose of the individual sy components.
    LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( sy ) )->components INTO DATA(co).
      DATA(sycomp) = to_lower( co-name ).
      DATA(code) = |DATA(sy{ sycomp }) = sy-{ sycomp }.|.
      out->write( code ).
    ENDLOOP.
    out->write( |\n| ).
    out->write( |\n| ).

    "Demonstrating prominent sy components that can be used in ABAP for Cloud Development

*&---------------------------------------------------------------------*
*& sy-subrc: Return code of ABAP statements
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& sy-index: Loop indexes
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& sy-tabix: Row index of internal tables
*&---------------------------------------------------------------------*
   
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

*&---------------------------------------------------------------------*
*& sy-dbcnt: Edited table rows
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& sy-fdpos: Occurrence in byte or character strings
*&---------------------------------------------------------------------*

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

  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Getting Structured Type Information and Creating Structures at Runtime

Using [Runtime Type Services (RTTS)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_services_glosry.htm "Glossary Entry")
you can ...
- get type information on data objects, data types or [instances](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_glosry.htm "Glossary Entry") at runtime ([Runtime Type Identification (RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm "Glossary Entry")).
- define and create new data types as [type description objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_object_glosry.htm) at runtime ([Runtime Type Creation (RTTC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_creation_glosry.htm "Glossary Entry")).

For more information, see the [Dynamic Programming](06_Dynamic_Programming.md) cheat sheet.

RTTI example: 
```abap
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

"Example: "Looping" across a structure
"For example, this may also be done using a DO loop and dynamic assignments.
"Demo structure, all components are convertible to type string
TYPES: BEGIN OF ty_struc,
         comp1 TYPE c LENGTH 3,
         comp2 TYPE string,
         comp3 TYPE i,
         comp4 TYPE n LENGTH 4,
       END OF ty_struc.
DATA(struct) = VALUE ty_struc( comp1 = 'abc' comp2 = `ABAP` comp3 = 123 comp4 = '9876' ).
DATA looped_struc TYPE string.

"In the loop, a string is populated, component by component.
LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( struct ) )->components INTO DATA(comp).
  looped_struc = |{ looped_struc }{ COND #( WHEN sy-tabix <> 1 THEN ` / ` ) }Name: "{ CONV string( comp-name ) }", Value: "{ struct-(comp-name) }"|.
ENDLOOP.

"Result:
"Name: "COMP1", Value: "abc" / Name: "COMP2", Value: "ABAP" / Name: "COMP3", Value: "123" / Name: "COMP4", Value: "9876"
```


<p align="right"><a href="#top">⬆️ back to top</a></p>


### Boxed Components


- In structures, boxed components represent nested structures managed by an internal reference.  
- Currently, static boxes are supported as boxed components, enabling [initial value sharing](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENINITIAL_VALUE_SHARING_GLOSRY.html). Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENSTATIC_BOXES.html).
- The relevant addition in a structured type declaration is `BOXED`. Syntax example: 
  ```abap
  TYPES: BEGIN OF struct, 
          text          TYPE c LENGTH 20, 
          nested_struct TYPE zdemo_abap_carr BOXED, 
         END OF struct.
  ```
- When used: 
  - Optimize memory consumption for structures used repeatedly, such as in internal tables with nested structures. Without boxed components, memory increases line by line, even if the nested structure is initial. With boxed components, memory does not increase when nested structures are initial, and only reads are performed.
  - Enhance runtime performance since assignments for components with active initial value sharing require only the internal reference, not additional data to be copied.
- Boxed components allocate memory when there is write access to at least one component or when a field symbol is assigned or data reference points to at least one component.

Expand the following collapsible section for more information and example code. 

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

The following example illustrates boxed components: 
- Two internal tables are created. One includes a nested structure as a boxed component, and the other includes a nested structure that is not a boxed component.
- The tables are populated in a loop under various conditions.
- The example demonstrates the impact of boxed components on memory usage.
- To try it out, proceed as follows:  
  - Create a demo class named `zcl_demo_abap`, paste the code into it, and activate it.
  - The example does not display output in the console.
  - It includes sections you can comment in and out. See notes in the examples.
  - The code sections compare the memory usage of boxed and non-boxed components:
    - *Comparison 1*: Empty nested boxed vs. empty nested non-boxed components
    - *Comparison 2*: All nested boxed vs. all nested non-boxed components populated
    - *Comparison 3*: Few nested boxed vs. few nested non-boxed components populated
  - For the comparison:
    - In ADT, add the *ABAP Memory (Debugger)* view. If not yet available, choose *Window* from the menu -> *Show View* -> *Other ...* -> filter for "memory" and add *ABAP Memory (Debugger)*.
    - Set a break-point at the `ASSERT` statement.
    - For *Comparison 1*, the first section is commented in. Run the class with *F9* in ADT. The first section deals with an internal table containing boxed components, where all are empty.
    - The debugger stops at the break-point. Open the *ABAP Memory (Debugger)* view.
    - Press *Refresh* in the view's top right corner. Check the *ABAP Application* values for used and allocated memory.
    - You may want to take a screenshot for comparison.
    - Stop debugging.
    - Comment out the first section and comment in the next one. Ensure no other sections within the loop are commented in. This section handles an internal table with nested, non-boxed components.
    - Repeat the process by setting the break-point and refreshing the view to compare memory values in the *ABAP Memory (Debugger)* view.
    - Compare the resulting values of the memory consumption. 
    - Repeat the steps for *Comparison 2* and *Comparison 3*.
- The following observations should be made regarding the memory consumption values, reflecting the impact of boxed components:
  - *Comparison 1*: Empty nested boxed vs. empty nested non-boxed components
    - The table with boxed components allocates significantly less memory than the one without. Non-boxed components have full memory allocation despite having no entries.
  - *Comparison 2*: All nested boxed vs. all nested non-boxed components populated
    - Memory for the table with boxed components is slightly higher due to extra administrative costs.
  - *Comparison 3*: Few nested boxed vs. few nested non-boxed components populated
    - Large tables with boxed components show considerably less memory usage when only a few components are populated compared to tables without boxed components.

<br>

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

    "Creating two demo internal tables
    "One with boxed components, the other with a nested, non-boxed components
    TYPES:
      BEGIN OF struc,
        comp1 TYPE c LENGTH 1024,
        comp2 TYPE c LENGTH 1024,
      END OF struc,

      BEGIN OF struc_w_boxed,
        id         TYPE i,
        boxed_comp TYPE struc BOXED,
      END OF struc_w_boxed,

      BEGIN OF struc_no_boxed,
        id    TYPE i,
        struc TYPE struc,
      END OF struc_no_boxed,

      tab_w_boxed  TYPE TABLE OF struc_w_boxed WITH EMPTY KEY,
      tab_no_boxed TYPE TABLE OF struc_no_boxed WITH EMPTY KEY.

    DATA: itab_w_boxed  TYPE tab_w_boxed,
          itab_no_boxed TYPE tab_no_boxed.

    "Populating internal tables
    "When running the example, only have one code snippet commented in, i.e.
    "the snippets between the sections
    "---- Comment in/out START ----
    "...
    "---- Comment in/out END ----
    DO 100000 TIMES.

*&---------------------------------------------------------------------*
*& Comparison 1
*&---------------------------------------------------------------------*
 
      "1) Internal table with boxed components: All boxed components empty

      "---- Comment in/out START ----
      INSERT INITIAL LINE INTO TABLE itab_w_boxed REFERENCE INTO DATA(wa1).
      wa1->id = sy-index.
      "---- Comment in/out END ----

**************************************************************************************************

      "2) Internal table with non-boxed components: All nested, non-boxed components empty

      "---- Comment in/out START ----
*      INSERT INITIAL LINE INTO TABLE itab_no_boxed REFERENCE INTO DATA(wa2).
*      wa2->id = sy-index.
      "---- Comment in/out END ----

**************************************************************************************************

*&---------------------------------------------------------------------*
*& Comparison 2
*&---------------------------------------------------------------------*

      "3) Internal table with boxed components: All boxed components filled

      "---- Comment in/out START ----
*      INSERT INITIAL LINE INTO TABLE itab_w_boxed REFERENCE INTO DATA(wa3).
*      wa3->id = sy-index.
*      wa3->boxed_comp-comp1 = sy-index.
*      wa3->boxed_comp-comp2 = sy-index.
      "---- Comment in/out END ----

**************************************************************************************************

      "4) Internal table with non-boxed components: All nested, non-boxed components filled

      "---- Comment in/out START ----
*      INSERT INITIAL LINE INTO TABLE itab_no_boxed REFERENCE INTO DATA(wa4).
*      wa4->id = sy-index.
*      wa4->struc-comp1 = sy-index.
*      wa4->struc-comp2 = sy-index.
      "---- Comment in/out END ----

**************************************************************************************************

*&---------------------------------------------------------------------*
*& Comparison 3
*&---------------------------------------------------------------------*

      "5) Internal table with boxed components: Only few boxed components filled

      "---- Comment in/out START ----
*      INSERT INITIAL LINE INTO TABLE itab_w_boxed REFERENCE INTO DATA(wa5).
*      wa5->id = sy-index.
*      IF sy-index <= 50.
*        wa5->boxed_comp-comp1 = sy-index.
*        wa5->boxed_comp-comp2 = sy-index.
*      ENDIF.
      "---- Comment in/out END ----

**************************************************************************************************

      "6) Internal table with non-boxed components: Only few nested, non-boxed components filled

      "---- Comment in/out START ----
*      INSERT INITIAL LINE INTO TABLE itab_no_boxed REFERENCE INTO DATA(wa6).
*      wa6->id = sy-index.
*      IF sy-index <= 50.
*        wa6->struc-comp1 = sy-index.
*        wa6->struc-comp2 = sy-index.
*      ENDIF.
      "---- Comment in/out END ----
    ENDDO.

    ASSERT 1 = 1.

  ENDMETHOD.

ENDCLASS.
```


</details>  





<p align="right"><a href="#top">⬆️ back to top</a></p>


## Executable Example
[zcl_demo_abap_structures](./src/zcl_demo_abap_structures.clas.abap)

> [!NOTE]  
> - The executable example covers the following topics, among others:
>     - Creating structures and structured types
>     - Variants of structures
>     - Accessing, populating, and clearing structures
>     - Structures in the context of tables
>     - Including structures
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](README.md#%EF%B8%8F-disclaimer)