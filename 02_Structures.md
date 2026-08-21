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
  - [Generic Structured Types](#generic-structured-types)
    - [Fully Generic Structured Types](#fully-generic-structured-types)
    - [User-Defined Partially Generic Structured Types](#user-defined-partially-generic-structured-types)
  - [Excursions](#excursions)
    - [sy Structure](#sy-structure)
    - [Getting Structured Type Information and Creating Structures at Runtime](#getting-structured-type-information-and-creating-structures-at-runtime)
    - [Boxed Components](#boxed-components)
    - [Recursive Structure References](#recursive-structure-references)
  - [Executable Example](#executable-example)

## Introduction
Structures ...

-   are [data objects](https://help.sap.com/docs/abap-cloud/abap-keyword/data-object)
    with [structured data types](https://help.sap.com/docs/abap-cloud/abap-keyword/structured-type) (which is a [complex data type](https://help.sap.com/docs/abap-cloud/abap-keyword/complex-data-type) because it is composed of other data types). 
-   consist of a sequence of [components](https://help.sap.com/docs/abap-cloud/abap-keyword/component) of any data type, that is, the components of a structure can be, for example, [elementary data objects](https://help.sap.com/docs/abap-cloud/abap-keyword/elementary-data-object), structures themselves, [internal tables](https://help.sap.com/docs/abap-cloud/abap-keyword/internal-table) or [references](https://help.sap.com/docs/abap-cloud/abap-keyword/reference).
- are used to combine different data objects that belong together. A typical example is an address. It has several components, such as name, street, city, and so on, that belong together.
- play an important role in the context of internal tables and [database tables](https://help.sap.com/docs/abap-cloud/abap-keyword/database-table). Structured types serve as [line types](https://help.sap.com/docs/abap-cloud/abap-keyword/line-type) for these tables. Most internal tables across [ABAP programs](https://help.sap.com/docs/abap-cloud/abap-keyword/abap-program) may have structured line types. For database tables, there is no alternative to structured line types.
- can be created locally in an ABAP program and globally. This cheat sheet focuses on locally defined structures and structured types.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Globally Available Structures and Structured Types

- Apart from the local declaration of a structured type, you can create such a type, for example, as global [DDIC structure](https://help.sap.com/docs/abap-cloud/abap-keyword/ddic-structure) in the [ABAP Dictionary](https://help.sap.com/docs/abap-cloud/abap-keyword/abap-dictionary). Such a DDIC structure defines a globally available structured type ([DDIC type](https://help.sap.com/docs/abap-cloud/abap-keyword/ddic-type)).
- There are other structured types available globally, which may be the structured types most commonly used in ABAP programs:
    - [Database tables](https://help.sap.com/docs/abap-cloud/abap-keyword/ddic-database-table) defined in the ABAP Dictionary can be used as data types just like DDIC structures in an ABAP program. This means that when you create a structure in your ABAP program, for example, you can simply use the name of a database table to address the line type of the table. The structure you created will then have the same structured type as the database table. Typically, you use the database tables to create structures of such a type, or internal tables of such a structured line type, to process data read from the database table in structures or internal tables.     
    - Various [CDS entities](https://help.sap.com/docs/abap-cloud/abap-keyword/cds-entity) are globally available structured types. For example, a [CDS view entity](https://help.sap.com/docs/abap-cloud/abap-keyword/cds-view-entity) represents a structured data type and can be used as such in ABAP programs (but not in the ABAP Dictionary). 
    - Structures and structured data types can be defined in the public [visibility section](https://help.sap.com/docs/abap-cloud/abap-keyword/visibility-section) of [global classes](https://help.sap.com/docs/abap-cloud/abap-keyword/global-class) or in [global interfaces](https://help.sap.com/docs/abap-cloud/abap-keyword/global-interface) and then used globally.

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
> - Classic [DDIC views](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenddic_view_glosry.html) are not available in [ABAP Cloud](https://help.sap.com/docs/abap-cloud/abap-keyword/abap-cloud). They can only be used as structured types in [classic ABAP](https://help.sap.com/docs/abap-cloud/abap-keyword/classic-abap).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Creating Structures and Structured Types Locally

The typical language elements for creating structures and structured types locally in an ABAP program are [`BEGIN OF ... END OF ...`](https://help.sap.com/docs/abap-cloud/abap-keyword/types-begin-of-struct-type). They are used in combination with the [`TYPES`](https://help.sap.com/docs/abap-cloud/abap-keyword/types) keyword to create a structured type and the [`DATA`](https://help.sap.com/docs/abap-cloud/abap-keyword/data) keyword to create a structure.

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Creating Structured Types

- The following statement defines a structured type introduced by `TYPES`. The type name is preceded by `BEGIN OF` (which marks the beginning of the structured type definition) and `END OF` (the end of the definition). 
- The components - at least one must be defined - are listed in between.
- Such structured type definitions are usually grouped together in a [chained statement](https://help.sap.com/docs/abap-cloud/abap-keyword/chained-statement), i.e. `TYPES` is followed by a colon, and the components are separated by commas.


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

- The simplest structures and structured types have [elementary](https://help.sap.com/docs/abap-cloud/abap-keyword/elementary-data-type)
components.
- As mentioned previously, the components can be of any type, i.e. they can be of structured types themselves, internal table types, or [reference types](https://help.sap.com/docs/abap-cloud/abap-keyword/reference-type). 
- You can use the [`TYPE`](https://help.sap.com/docs/abap-cloud/abap-keyword/data-type-abap-type)
and [`LIKE`](https://help.sap.com/docs/abap-cloud/abap-keyword/data-type-like) additions for the types of the components. 
You can use the `LINE OF` addition to refer to a table type or an internal table. 


``` abap
TYPES: BEGIN OF struc_type,
         comp1 TYPE i,                  "elementary type           
         comp2 TYPE c LENGTH 5,         "elementary type
         comp3 TYPE structured_type,    "structured type
         comp4 TYPE itab_type,          "internal table type
         comp5 TYPE ddic_type,          "DDIC type
         comp6 TYPE REF TO i,           "data reference
         comp7 LIKE data_object,        "deriving type from a data object                  
         comp8 TYPE LINE OF itab_type,  "component has structured type, type derived from internal table type 
         comp9 LIKE LINE OF itab,       "component has structured type, type derived from internal table
         comp10 TYPE REF TO struc_type, "recursive structure refererence        
         ...,
       END OF struc_type.
```


> [!NOTE] 
> - Outside of classes, you can also refer to DDIC types using `LIKE` (`... comp11 LIKE ddic_type, ...`). If you actually want to refer to an existing data object, but due to typing errors you inadvertently specify a name that exists as DDIC type, errors may be unavoidable.
> - It is possible to create [recursive structure references](#recursive-structure-references). These are components of a structured type that represent data references to the same structure in which they are defined (see the `comp10` component in the `struc_type` example). 


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Structures

- To create a structure (i.e. a structured data object) in an ABAP program, you can use the `DATA` keyword. 
- It works in the same way as the `TYPES` statement above. 
- Unlike the `TYPES` statement, you can use the [`VALUE`](https://help.sap.com/docs/abap-cloud/abap-keyword/data-data-options) addition to set default values.  

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
>-  The keywords [`CLASS-DATA`](https://help.sap.com/docs/abap-cloud/abap-keyword/class-data) and [`CONSTANTS`](https://help.sap.com/docs/abap-cloud/abap-keyword/constants) can also be used to create structures. In principle, they represent special cases of the general statement shown above. See the ABAP Keyword Documentation for more information. 
>- Structures can also be created [inline](https://help.sap.com/docs/abap-cloud/abap-keyword/inline-declaration) using [`DATA(...)`](https://help.sap.com/docs/abap-cloud/abap-keyword/data-inline-declaration-for-variables) or [`FINAL(...)`](https://help.sap.com/docs/abap-cloud/abap-keyword/final-inline-declaration-for-immutable-variables), as shown below.

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

- This is particularly useful for declaring data objects at the [operand positions](https://help.sap.com/docs/abap-cloud/abap-keyword/operand-position) where you actually need them. 
- In this way, you can avoid an extra declaration of the structure in different contexts.
- You can use the declaration operator using `DATA(...)`. The [`FINAL`](https://help.sap.com/docs/abap-cloud/abap-keyword/final-inline-declaration-for-immutable-variables) declaration operator is used to create [immutable variables](https://help.sap.com/docs/abap-cloud/abap-keyword/immutable-variable).
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
- As shown above, the [`FINAL`](https://help.sap.com/docs/abap-cloud/abap-keyword/final-inline-declaration-for-immutable-variables) declaration operator is used to create [immutable variables](https://help.sap.com/docs/abap-cloud/abap-keyword/immutable-variable).

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

Find more information on [enumerated types](https://help.sap.com/docs/abap-cloud/abap-keyword/enumerated-type) in the [Data Types and Data Objects](16_Data_Types_and_Objects.md#abap-enumerated-types-and-objects) cheat sheet.

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

Using the instance operator [`NEW`](https://help.sap.com/docs/abap-cloud/abap-keyword/new-instance-operator) and [`CREATE DATA`](https://help.sap.com/docs/abap-cloud/abap-keyword/create-data) statements, you can create [anonymous data objects](https://help.sap.com/docs/abap-cloud/abap-keyword/anonymous-data-object), such as anonymous structures. 
The `NEW` addition of the `INTO` clause of an ABAP SQL `SELECT` statement also creates an anonymous data object. 
As outlined below, you can access the components or the entire data objects by [dereferencing](https://help.sap.com/docs/abap-cloud/abap-keyword/dereferencing-operator-abendereferencing_operat_glosry). 
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

Depending on the component type, the structure can be a [flat structure](https://help.sap.com/docs/abap-cloud/abap-keyword/flat-structure),
a [nested structure](https://help.sap.com/docs/abap-cloud/abap-keyword/nested-structure),
or a [deep structure](https://help.sap.com/docs/abap-cloud/abap-keyword/deep-structure).

- **Flat structures** contain only elementary types that have a fixed length, that is, there are no internal tables, reference types or strings as components. Nesting does not matter in this context. Even a nested structure is considered flat unless a substructure contains a deep component.
    ``` abap
    DATA: BEGIN OF struc,
            comp1 TYPE i,
            comp2 TYPE c LENGTH 15,
            comp3 TYPE p LENGTH 8 DECIMALS 2,
            ...,
          END OF struc.
    ```    

- **Nested structures**: At least one component of a structure is a [substructure](https://help.sap.com/docs/abap-cloud/abap-keyword/substructure),
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
>- [Work areas](https://help.sap.com/docs/abap-cloud/abap-keyword/work-area) of ABAP SQL statements cannot contain any deep components other than strings among others.
>- Especially for assignments and comparisons of deep structures, the compatibility of the source and target structure must be taken into account.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Accessing (Components of) Structures

- Structures can be accessed as a whole. You can also address the individual components of structures at the appropriate operand positions. 
- To address the components, use the [structure component selector](https://help.sap.com/docs/abap-cloud/abap-keyword/structure-component-selector-abenstructure_component_sel_glosry)
`-`. 
- For variables with reference to a structured data object, the [object component selector](https://help.sap.com/docs/abap-cloud/abap-keyword/object-component-selector-abenobject_component_select_glosry) `->` can be used: `...dref->comp ...`. The following syntax also works, but is less *convenient*: `... dref->*-comp ...`.
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

You can copy the content of a structure to another using the [assignment operator](https://help.sap.com/docs/abap-cloud/abap-keyword/assignment-operator-abenassignment_operator_glosry) `=`. 
In the following example, it is assumed that the target and source structures are of compatible types. In general, note that special [conversion](https://help.sap.com/docs/abap-cloud/abap-keyword/conversion-rules-for-structures) and [comparison rules](https://help.sap.com/docs/abap-cloud/abap-keyword/rel-exp-comparing-structures) apply to value assignments involving structures.
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

- The [`VALUE`](https://help.sap.com/docs/abap-cloud/abap-keyword/value-value-operator) operator can be used to construct the content of complex data objects such as structures or internal tables. 
- It is particularly useful because assigning values by addressing the structure components individually can be very cumbersome, especially when assigning values to structure components at the [operand position](https://help.sap.com/docs/abap-cloud/abap-keyword/operand-position).
- If the type of the operand can be inferred implicitly, the `#` character can be used used before the parentheses. Otherwise, the type must be  specified explicitly. 
- The `VALUE` operator and inline declarations can be used to create and populate structures in one go.
- Note that there are special [conversion](https://help.sap.com/docs/abap-cloud/abap-keyword/conversion-rules-for-structures) and [comparison](https://help.sap.com/docs/abap-cloud/abap-keyword/rel-exp-comparing-structures) rules for structures. See the ABAP Keyword Documentation for more details.


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

Using the instance operator [`NEW`](https://help.sap.com/docs/abap-cloud/abap-keyword/new-instance-operator), you can create [anonymous data objects](https://help.sap.com/docs/abap-cloud/abap-keyword/anonymous-data-object), such as anonymous structures. You can access the components or the entire data objects by [dereferencing](https://help.sap.com/docs/abap-cloud/abap-keyword/dereferencing-operator-abendereferencing_operat_glosry). For more information, refer to the  [Dynamic Programming](06_Dynamic_Programming.md) and [Constructor Expressions](05_Constructor_Expressions.md) cheat sheets.

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

- You can use statements with [`MOVE-CORRESPONDING`](https://help.sap.com/docs/abap-cloud/abap-keyword/move-corresponding)
and the [`CORRESPONDING`](https://help.sap.com/docs/abap-cloud/abap-keyword/corresponding-component-operator) operator to assign values to structure components, especially when assigning values from a source structure to a target structure which have incompatible types and/or differently named components. 
- Both are used to assign identically named components of structures to each other. 
- The syntax also works for structures of the same type.
- Also note the special [conversion](https://help.sap.com/docs/abap-cloud/abap-keyword/conversion-rules-for-structures) and [comparison](https://help.sap.com/docs/abap-cloud/abap-keyword/rel-exp-comparing-structures) rules for structures in this context.

> [!NOTE]  
>- The [`CL_ABAP_CORRESPONDING`](https://help.sap.com/docs/abap-cloud/abap-keyword/cl-abap-corresponding-system-class) system class is available for making assignments. See the ABAP Keyword Documentation for the details.
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
- In the context of deep structures, there are additional syntax variants available for [`MOVE-CORRESPONDING`](https://help.sap.com/docs/abap-cloud/abap-keyword/move-corresponding) statements and the [`CORRESPONDING`](https://help.sap.com/docs/abap-cloud/abap-keyword/corresponding-component-operator) operator.
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
entire structure using the [`CLEAR`](https://help.sap.com/docs/abap-cloud/abap-keyword/clear) keyword. Note that [`FREE`](https://help.sap.com/docs/abap-cloud/abap-keyword/free) statements also deletes the content, but they also release the initially allocated memory.
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
Structures are primarily used to process data from tables. In this context, structures often take on the role of a [work area](https://help.sap.com/docs/abap-cloud/abap-keyword/work-area). 
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
target structure must also be flat. In the example below, the [`SINGLE`](https://help.sap.com/docs/abap-cloud/abap-keyword/select-single)
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
Sequentially passing rows of a read result into a structure
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
[`INSERT`](https://help.sap.com/docs/abap-cloud/abap-keyword/insert-writable-obj)
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

Updating a single row in a database table from a structure using ABAP SQL statements with [`UPDATE`](https://help.sap.com/docs/abap-cloud/abap-keyword/update-writable-obj)
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
[`MODIFY`](https://help.sap.com/docs/abap-cloud/abap-keyword/modify-writable-obj) 
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
[`DELETE`](https://help.sap.com/docs/abap-cloud/abap-keyword/delete-writable-obj) 
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

The code snippet below shows the reading of a line into a [work area](https://help.sap.com/docs/abap-cloud/abap-keyword/work-area), a [field symbol](https://help.sap.com/docs/abap-cloud/abap-keyword/field-symbol), and a [data reference variable](https://help.sap.com/docs/abap-cloud/abap-keyword/data-reference-variable), all of which 
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

Reading a line from an internal table into a structure using a [table expression](https://help.sap.com/docs/abap-cloud/abap-keyword/table-expression)
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

Sequentially reading a line from an internal table into a structure using a [`LOOP AT`](https://help.sap.com/docs/abap-cloud/abap-keyword/loop-at-itab) statement
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

- [`INCLUDE TYPE`](https://help.sap.com/docs/abap-cloud/abap-keyword/include-type-structure)
and [`INCLUDE STRUCTURE`](https://help.sap.com/docs/abap-cloud/abap-keyword/include-type-structure) statements 
are used in the context of local structures. 
- Structured data objects and types created with `... BEGIN OF... END OF ...` can use this syntax to include components of another structure, whether it is a locally defined or global structure, without creating  substructures. 
- `INCLUDE TYPE` can be used to include a structured type. 
- You can use `INCLUDE STRUCTURE` to include a structure.

> [!NOTE]  
> - They are not additions of `... BEGIN OF ... END OF ...` but individual ABAP statements.
> - If you use a chained statement with a colon to declare the structure, the inclusion of other structures with these statements interrupts the chained statement, that is, the components of the included structures are included as direct components of the [superstructure](https://help.sap.com/docs/abap-cloud/abap-keyword/superstructure).
>- By using the optional `AS` addition and specifying a name, the included components can be addressed by this common name as if they were actually components of a substructure.
>- The optional `RENAMING WITH SUFFIX` addition, followed by a name, gives the included components a suffix name to avoid naming conflicts with other components.

The following example shows how structured types and data objects are included in another structure. First, three structured types and a structured data object based on one of these types are created. Then, the types and the structure are included in the structured type `address_type`. As an excursion, [Runtime Type Identification](https://help.sap.com/docs/abap-cloud/abap-keyword/runtime-type-identification) is used to retrieve the component names of created structured type `address_type`. Refer to the [Getting Structured Type Information and Creating Structures at Runtime](#getting-structured-type-information-and-creating-structures-at-runtime) section. The executable example demonstrates a structure that includes other structures in this way.
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


## Generic Structured Types

### Fully Generic Structured Types

- [`ANY STRUCTURE`](https://help.sap.com/docs/abap-cloud/abap-keyword/type-any-structure) is a built-in [generic ABAP type](https://help.sap.com/docs/abap-cloud/abap-keyword/generic-abap-type) for fully generic structured types, similar to the `ANY TABLE` type for generic internal tables.
- It can be used to define [formal parameters](https://help.sap.com/docs/abap-cloud/abap-keyword/formal-parameter) and [field symbols](https://help.sap.com/docs/abap-cloud/abap-keyword/field-symbol).
- When defined, any structure can be assigned to field symbols and passed as [actual parameters](https://help.sap.com/docs/abap-cloud/abap-keyword/actual-parameter) to formal parameters in procedure calls.
- Criteria for typing formal parameters with `TYPE ANY STRUCTURE` include:
  - Any optional parameter must have a default value specified.
  - You can specify importing and changing parameters with fully generic structured types if they are mandatory or defined as default by specifying a default structure.
  - The typing is not allowed for exporting and returning parameters, as well as for [AMDP procedures](https://help.sap.com/docs/abap-cloud/abap-keyword/amdp-procedure).

The code snippet demonstrates that structures of any type can be assigned to a field symbol typed with `ANY STRUCTURE`.


```abap
FIELD-SYMBOLS <struc> TYPE ANY STRUCTURE.

TYPES: BEGIN OF ty_flat,
         comp1 TYPE c LENGTH 3,
         comp2 TYPE i,
         comp3 TYPE n LENGTH 5,
       END OF ty_flat,
       BEGIN OF ty_nested,
         comp1 TYPE c LENGTH 10,
         comp2 TYPE ty_flat,
       END OF ty_nested,
       BEGIN OF ty_deep,
         comp1 TYPE string,
         comp2 TYPE REF TO i,
         comp3 TYPE ty_nested,
         comp4 TYPE string_table,
       END OF ty_deep.

TYPES BEGIN OF ty_include.
INCLUDE TYPE ty_flat AS a.
INCLUDE TYPE ty_nested AS b RENAMING WITH SUFFIX _n.
TYPES END OF ty_include.

DATA: struc_flat    TYPE ty_flat,
      struc_nested  TYPE ty_nested,
      struc_deep    TYPE ty_deep,
      struc_include TYPE ty_include.

ASSIGN struc_flat TO <struc>.
ASSIGN struc_nested TO <struc>.
ASSIGN struc_deep TO <struc>.
ASSIGN struc_include TO <struc>.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


### User-Defined Partially Generic Structured Types

- Using [`ANY STRUCTURE CONTAINING component_list`](https://help.sap.com/docs/abap-cloud/abap-keyword/types-any-structure-containing), you can create partially generic structured types.  
- "Partially" means these types impose constraints by predefining a set of components (`component_list`) following the `CONTAINING` addition. These components must be included as part of the structure, while other components may vary.  
- `component_list` is not comma-separated and can consist of one or more components. The components can be either generic or non-generic.  
- The syntax can be used to create local types with `TYPES` statements. Unlike the fully generic type `ANY STRUCTURE`, `ANY STRUCTURE CONTAINING component_list` cannot be used to declare field symbols and formal parameters directly. Instead, the locally declared types must be used.  
- Rules:  
  - When assigning non-generic structures to partially generic structures, the order of components in the partially generic structure is irrelevant.  
  - The components of the non-generic structure must match the components of the generic structured type, meaning the component names must be identical, and their types must be compatible.  
  - The non-generic structure can include more components than those specified in the partially generic type.  
  - Two partially generic structured types are compatible if their constraints in the component list do not contradict in terms of component names and types.  
  - Strict compatibility checks apply to components with elementary types, particularly concerning dictionary types. See [here](https://help.sap.com/docs/abap-cloud/abap-keyword/mapping-of-dictionary-to-abap-types).  

The following example demonstrates the local creation of partially generic structured types, which are used to define field symbols. The example highlights the following aspects:  
- Various possible and impossible assignments of non-generic structures to field symbols typed with partially generic structured types  
- Strict compatibility checks enforced for elementary dictionary types  
- Include structure  

```abap
"Non-generic structured types
TYPES: BEGIN OF ts_1,
         comp1 TYPE i,
         comp2 TYPE i,
       END OF ts_1,
       BEGIN OF ts_2,
         comp2 TYPE i,
         comp1 TYPE i,
       END OF ts_2,
       BEGIN OF ts_3,
         comp1 TYPE i,
         comp2 TYPE i,
         comp3 TYPE i,
         comp4 TYPE i,
         comp5 TYPE i,
       END OF ts_3,
       BEGIN OF ts_4,
         comp1 TYPE i,
         comp2 TYPE int8,
       END OF ts_4.

DATA: struc_1 TYPE ts_1,
      struc_2 TYPE ts_2,
      struc_3 TYPE ts_3,
      struc_4 TYPE ts_4.

"Locally declared partially generic structured types
TYPES: ts_gen_1 TYPE ANY STRUCTURE CONTAINING comp1 TYPE i
                                              comp2 TYPE i,
       ts_gen_2 TYPE ANY STRUCTURE CONTAINING comp2 TYPE i
                                              comp3 TYPE i.

FIELD-SYMBOLS <gs_1> TYPE ts_gen_1.
FIELD-SYMBOLS <gs_2> TYPE ts_gen_2.

*&---------------------------------------------------------------------*
*& Various assignments with partially generic structured types
*&---------------------------------------------------------------------*

"Identical component list with compatible types
ASSIGN struc_1 TO <gs_1>.

"Component list matches, but different order
ASSIGN struc_2 TO <gs_1>.

"Component list of partially generic structure matches,
"assigned structure contains additional components
ASSIGN struc_3 TO <gs_1>.

"--- ERROR ---
"Component names match, but there is a type incompatibility;
"no assignment possible
"ASSIGN struc_4 TO <gs_1>.

"--- ERROR ---
"Not matching component list; no assignment possible
"ASSIGN struc_1 TO <gs_2>.

"--- ERROR ---
"Not matching component list; no assignment possible
"ASSIGN struc_2 TO <gs_2>.

"Component list of partially generic structure matches,
"assigned structure contains additional components
ASSIGN struc_3 TO <gs_2>.

"--- ERROR ---
"Not matching component list; no assignment possible
"ASSIGN struc_4 TO <gs_2>.

*&---------------------------------------------------------------------*
*& Strict compatibility checks enforced for elementary dictionary types
*&---------------------------------------------------------------------*

"Strict checks regarding elementary dictionary types
"The example uses the dictionary types timn/tims.
TYPES: BEGIN OF ts_time,
         time TYPE t,
       END OF ts_time.
DATA struc_time TYPE ts_time.

"Creating local timn/tims types
"The SELECT statements are just used to have a self-contained
"example to have a type to refer to.
SELECT SINGLE
  FROM i_timezone
  FIELDS tims`123456` AS tims
  INTO @data(tims).

SELECT SINGLE
  FROM i_timezone
  FIELDS timn`123456` AS timn
  INTO @data(timn).

TYPES ty_tims like tims.
TYPES ty_timn like timn.

TYPES ts_gen_tims TYPE ANY STRUCTURE
  CONTAINING time type ty_tims.
FIELD-SYMBOLS <gs_tims> TYPE ts_gen_tims.

ASSIGN struc_time TO <gs_tims>.

TYPES ts_gen_timn TYPE ANY STRUCTURE
  CONTAINING time TYPE ty_timn.
FIELD-SYMBOLS <gs_timn> TYPE ts_gen_timn.

"--- ERROR ---
"No assignment possible due to type incompatibility
"ASSIGN struc_time TO <gs_timn>.

*&---------------------------------------------------------------------*
*& Example with include structure
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ts_5,
         comp1 TYPE i,
       END OF ts_5,
       BEGIN OF ts_6.
       INCLUDE TYPE ts_5 AS substruct.
TYPES: comp2 TYPE i,
       comp3 TYPE i,
       END OF ts_6.

DATA struc_incl TYPE ts_6.

"comp1 can be accessed both by comp1 and substruct-comp1
struc_incl-comp1 = 1.
struc_incl-substruct-comp1 = 1.
struc_incl-comp2 = 2.
struc_incl-comp3 = 3.

TYPES ty_gts_5 TYPE ANY STRUCTURE CONTAINING substruct TYPE ts_5
                                            comp2 TYPE i.

FIELD-SYMBOLS <fts_5> TYPE ty_gts_5.
ASSIGN struc_incl TO <fts_5>.

"The structure is also compatible with the following generic
"structured type, as there is no special handling for component
"aliases, and it meets the constraints set by the generic structure,
"even though comp1 in the example is essentially specified twice.
TYPES ty_gts_6 TYPE ANY STRUCTURE CONTAINING substruct TYPE ts_5
                                             comp1 TYPE i.

FIELD-SYMBOLS <fts_6> TYPE ty_gts_6.
ASSIGN struc_incl TO <fts_6>.
```


Expand the following collapsible section for example code. To try it out, create a demo class named `zcl_demo_abap`, or reuse it if it already exists. Paste the code into it. If you choose a different class name, update the class name in the code snippet accordingly. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console. To explore the entire example, you have imported the ABAP cheat sheet GitHub repository since it uses some of its artifacts.

The following example contexts are included:
- Assigning non-generic structures to field symbols typed with partially generic structured types using `ASSIGN` statements, highlighting component definitions.
- Structure assignments that emphasize position-based and name-based movements.
- Structure assignments that emphasize enforced strict checks regarding elementary dictionary types.
- Assigning non-generic structures with included structures to partially generic structures.
- Using internal tables with a line type of partially generic structured type.
- Implementing an algorithm in three ways, emphasizing the benefits of using generic structured types: Processing a BDEF derived type with a static, dynamic, and generic implementation. 
  - In each method, content from an internal table typed with a BDEF derived type is processed, returning only instances where the `%control` structure is not initial. Additionally, `%cid_ref` values are extracted and returned.
  - Different tables of type `TABLE FOR UPDATE` and `TABLE FOR DELETE` are created as demo input for the methods.
  - The method with static implementation is only valid for one specific type, with the importing parameter `request_static` typed as a specific BDEF derived type. This method can process only one demo table.
  - The method with the dynamic implementation includes the importing parameter `request_dynamic`. It is typed as `any table`, accepting all types of tables. The method also includes a generic `WHERE` clause to be passed. In the example, the demo `WHERE` clause checks for non-initial `%control` components (`%control is not initial`). The demo tables of type `TABLE FOR UPDATE` can be processed in the example since these types include `%control`. However, any other tables can be passed, too. An internal table typed with `TABLE FOR DELETE` is passed, but it raises an exception as it cannot be processed (it has no `%control` component). The same applies to a demo string table passed.
  - The generic method features an importing parameter `request_generic`, based on a partially generic structure type declared with `ANY STRUCTURE CONTAINING`. The constraint is: The structure must have `%cid_ref` and `%control` components. The example defines `%control` with the fully generic structure typed `ANY STRUCTURE`, allowing it to accept all kinds of `%control` structures of BDEF derived types. This typing guarantees type safety, ensuring that all BDEF derived types with `%cid_ref` and `%control` components are accepted. Other tables, such as the demo table typed with `TABLE FOR DELETE` and the string table, cannot be passed.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_1,
             comp1 TYPE i,
             comp2 TYPE i,
           END OF ts_1,
           BEGIN OF ts_2,
             comp2 TYPE i,
             comp1 TYPE i,
           END OF ts_2,
           BEGIN OF ts_3,
             comp1 TYPE i,
             comp2 TYPE i,
             comp3 TYPE i,
             comp4 TYPE i,
             comp5 TYPE i,
           END OF ts_3,
           BEGIN OF ts_4,
             comp1 TYPE i,
             comp2 TYPE int8,
           END OF ts_4,
           gen_struc   TYPE ANY STRUCTURE CONTAINING %cid_ref TYPE abp_behv_cid
                                                     %control TYPE ANY STRUCTURE,
           request_tab TYPE INDEX TABLE OF gen_struc WITH FURTHER SECONDARY KEYS,
           cid_ref_tab TYPE SORTED TABLE OF abp_behv_cid WITH UNIQUE KEY table_line,
           der_update1 TYPE TABLE FOR UPDATE zdemo_abap_rap_draft_m,
           der_update2 TYPE TABLE FOR UPDATE zdemo_abap_rap_ro_m,
           der_update3 TYPE TABLE FOR UPDATE zdemo_abap_rap_ro_u,
           der_delete  TYPE TABLE FOR DELETE zdemo_abap_rap_ro_m.

    DATA: struc_1 TYPE ts_1,
          struc_2 TYPE ts_2,
          struc_3 TYPE ts_3,
          struc_4 TYPE ts_4,
          update1 TYPE der_update1,
          update2 TYPE der_update2,
          update3 TYPE der_update3,
          delete  TYPE der_delete.

    METHODS: assignments IMPORTING out TYPE REF TO if_oo_adt_classrun_out,
      move IMPORTING out TYPE REF TO if_oo_adt_classrun_out,
      assignments_ddic_types IMPORTING out TYPE REF TO if_oo_adt_classrun_out,
      include_structures IMPORTING out TYPE REF TO if_oo_adt_classrun_out,
      itabs_generic_struc_type IMPORTING out TYPE REF TO if_oo_adt_classrun_out,
      static_implementation IMPORTING out            TYPE REF TO if_oo_adt_classrun_out
                                      request_static TYPE der_update1
                            EXPORTING cid_ref_tab    TYPE cid_ref_tab
                            RETURNING VALUE(result)  TYPE REF TO data,
      dynamic_implementation IMPORTING out                     TYPE REF TO if_oo_adt_classrun_out
                                       request_dynamic         TYPE ANY TABLE
                                       VALUE(dyn_where_clause) TYPE string
                             EXPORTING cid_ref_tab             TYPE cid_ref_tab
                             RETURNING VALUE(result)           TYPE REF TO data,
      generic_implementation IMPORTING out             TYPE REF TO if_oo_adt_classrun_out
                                       request_generic TYPE request_tab
                             EXPORTING cid_ref_tab     TYPE cid_ref_tab
                             RETURNING VALUE(result)   TYPE REF TO data,
      populate_structures.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    out->write( |\n| ).
    out->write( `*********** assignments ***********` ).
    out->write( |\n| ).
    assignments( out ).
    out->write( |\n| ).
    out->write( `*********** assignments_ddic_types ***********` ).
    out->write( |\n| ).
    assignments_ddic_types( out ).
    out->write( |\n| ).
    out->write( `*********** move ***********` ).
    out->write( |\n| ).
    move( out ).
    out->write( |\n| ).
    out->write( `*********** include_structures ***********` ).
    out->write( |\n| ).
    include_structures( out ).
    out->write( |\n| ).
    out->write( `*********** itabs_generic_struc_type ***********` ).
    out->write( |\n| ).
    itabs_generic_struc_type( out ).
    out->write( |\n| ).
    out->write( `*********** static_implementation ***********` ).
    out->write( |\n| ).
    static_implementation( out = out
                           request_static = update1 ).
    "The following data objects cannot be passed with this approach.
    "static_implementation( out = out request_static = update2 ).
    "static_implementation( out = out request_static = update3 ).
    "static_implementation( out = out request_static = delete ).

    out->write( |\n| ).
    out->write( `*********** dynamic_implementation ***********` ).
    out->write( |\n| ).

    DATA(dyn_where_clause) = `%control is not initial`.

    dynamic_implementation( out = out
                            request_dynamic = update1
                            dyn_where_clause = dyn_where_clause ).

    out->write( |\n| ).

    dynamic_implementation( out = out
                            request_dynamic = update2
                            dyn_where_clause = dyn_where_clause ).

    out->write( |\n| ).

    dynamic_implementation( out = out
                            request_dynamic = update3
                            dyn_where_clause = dyn_where_clause ).

    out->write( |\n| ).

    "The following example passes a BDEF derived type. However, it does
    "not contain the %control component, so the WHERE clause will not be met.
    dynamic_implementation( out = out
                            request_dynamic = delete
                            dyn_where_clause = dyn_where_clause ).

    out->write( |\n| ).

    "The following nonsense example method call passes a string table that
    "cannot be processed.
    DATA(str_tab) = VALUE string_table( ( `abc` ) ( `def` ) ).

    dynamic_implementation( out = out
                            request_dynamic = str_tab
                            dyn_where_clause = dyn_where_clause ).

    out->write( |\n| ).
    out->write( `*********** generic_implementation ***********` ).
    out->write( |\n| ).
    generic_implementation( out = out
                            request_generic = update1 ).
    out->write( |\n| ).
    generic_implementation( out = out
                            request_generic = update2 ).
    out->write( |\n| ).
    generic_implementation( out = out
                            request_generic = update3 ).
    "The following data objects cannot be passed with this approach.
    "generic_implementation( out = out request_generic = str_tab ).
    "generic_implementation( out = out request_generic = delete ).

  ENDMETHOD.

  METHOD assignments.

    populate_structures( ).

    TYPES: ts_gen_1 TYPE ANY STRUCTURE CONTAINING comp1 TYPE i
                                                  comp2 TYPE i,
           ts_gen_2 TYPE ANY STRUCTURE CONTAINING comp2 TYPE i
                                                  comp3 TYPE i.

    FIELD-SYMBOLS <gs_1> TYPE ts_gen_1.
    FIELD-SYMBOLS <gs_2> TYPE ts_gen_2.

    "Identical component list with compatible types
    ASSIGN struc_1 TO <gs_1>.

    out->write( data = <gs_1> name = `<gs_1>` ).

    "Component list matches, but different order
    ASSIGN struc_2 TO <gs_1>.

    out->write( data = <gs_1> name = `<gs_1>` ).

    "Component list of partially generic structure matches,
    "assigned structure contains additional components
    ASSIGN struc_3 TO <gs_1>.

    out->write( data = <gs_1> name = `<gs_1>` ).

    "Component names match, but there is a type incompatibility;
    "no assignment possible
    "ASSIGN struc_4 TO <gs_1>.

    "Not matching component list; no assignment possible
    "ASSIGN struc_1 TO <gs_2>.

    "Not matching component list; no assignment possible
    "ASSIGN struc_2 TO <gs_2>.

    "Component list of partially generic structure matches,
    "assigned structure contains additional components
    ASSIGN struc_3 TO <gs_2>.

    out->write( data = <gs_2> name = `<gs_2>` ).

    "Not matching component list; no assignment possible
    "ASSIGN struc_4 TO <gs_2>.

  ENDMETHOD.

  METHOD move.

    populate_structures( ).

    out->write( data = struc_2 name = `struc_2` ).

    "Position-based movement
    struc_2 = struc_1.
    ASSERT struc_2-comp1 <> 3.
    ASSERT struc_2-comp2 <> 4.

    out->write( data = struc_2 name = `struc_2` ).

    FIELD-SYMBOLS <s1> TYPE data.
    FIELD-SYMBOLS <s2> TYPE data.
    ASSIGN struc_1 TO <s1>.
    ASSIGN struc_2 TO <s2>.

    out->write( data = <s1> name = `<s1>` ).
    out->write( data = <s2> name = `<s2>` ).

    <s2> = <s1>.

    out->write( data = <s1> name = `<s1>` ).
    out->write( data = <s2> name = `<s2>` ).

    TYPES ts_gen TYPE ANY STRUCTURE CONTAINING comp1 TYPE i.
    FIELD-SYMBOLS <s4> TYPE ts_gen.
    ASSIGN struc_1 TO <s4>.

    out->write( data = <s4> name = `<s4>` ).

    struc_2 = <s4>.

    out->write( data = struc_2 name = `struc_2` ).

    "Corresponding move
    populate_structures( ).

    ASSIGN struc_1 TO <s4>.

    MOVE-CORRESPONDING <s4> TO struc_2.

    out->write( data = struc_2 name = `struc_2` ).

    populate_structures( ).

    ASSIGN struc_1 TO <s4>.

    struc_2 = CORRESPONDING #( <s4> ).

    out->write( data = struc_2 name = `struc_2` ).
  ENDMETHOD.

  METHOD assignments_ddic_types.

    "Strict checks regarding elementary dictionary types
    "The example uses the dictionary types timn/tims.
    TYPES: BEGIN OF ts_time,
             time TYPE t,
           END OF ts_time.
    DATA(struc_time) = VALUE ts_time( time = CONV t( '123456' ) ).

    "Creating local timn/tims types
    "The nonsense SELECT statements with the typed literals are just
    "used to have a self-contained example for a type to refer to.
    SELECT SINGLE
      FROM i_timezone
      FIELDS tims`123456` AS tims
      INTO @DATA(tims).

    SELECT SINGLE
      FROM i_timezone
      FIELDS timn`123456` AS timn
      INTO @DATA(timn).

    TYPES ty_tims LIKE tims.
    TYPES ty_timn LIKE timn.

    TYPES ts_gen_tims TYPE ANY STRUCTURE
      CONTAINING time TYPE ty_tims.
    FIELD-SYMBOLS <gs_tims> TYPE ts_gen_tims.

    ASSIGN struc_time TO <gs_tims>.

    out->write( data = <gs_tims> name = `<gs_tims>` ).

    TYPES ts_gen_timn TYPE ANY STRUCTURE
      CONTAINING time TYPE ty_timn.
    FIELD-SYMBOLS <gs_timn> TYPE ts_gen_timn.

    "--- ERROR ---
    "No assignment possible due to type incompatibility
    "ASSIGN struc_time TO <gs_timn>.

  ENDMETHOD.

  METHOD populate_structures.
    struc_1 = VALUE #( comp1 = 1 comp2 = 2 ).
    struc_2 = VALUE #( comp2 = 3 comp1 = 4 ).
    struc_3 = VALUE #( comp1 = 5 comp2 = 6 comp3 = 7
                       comp4 = 8 comp5 = 9 ).
    struc_4 = VALUE #( comp1 = 10 comp2 = 11 ).
  ENDMETHOD.

  METHOD include_structures.

    TYPES: BEGIN OF s1,
             comp1 TYPE i,
           END OF s1,
           BEGIN OF s2.
             INCLUDE TYPE s1 AS substruc.
    TYPES: comp2 TYPE i,
             comp3 TYPE i,
           END OF s2.

    TYPES t_gs1 TYPE ANY STRUCTURE CONTAINING substruc TYPE s1
                                              comp2 TYPE i.

    DATA(struct_1) = VALUE s2( substruc-comp1 = 1
                               comp2 = 2
                               comp3 = 3 ).

    FIELD-SYMBOLS <fs1> TYPE t_gs1.

    ASSIGN struct_1 TO <fs1>.

    out->write( data = <fs1> name = `<fs1>` ).

    TYPES: BEGIN OF s3.
             INCLUDE TYPE s1 AS substruc RENAMING WITH SUFFIX _sub.
    TYPES:   comp2 TYPE i,
             comp3 TYPE i,
           END OF s3.

    DATA(struct_2) = VALUE s3( substruc-comp1 = 4
                               comp2 = 5
                               comp3 = 6 ).

    "Not compatible due to renaming with suffix
    "ASSIGN struct_3 TO <fs1>.

    TYPES t_gs2 TYPE ANY STRUCTURE CONTAINING comp2 TYPE i
                                              comp1_sub TYPE i.

    FIELD-SYMBOLS <fs2> TYPE t_gs2.

    ASSIGN struct_2 TO <fs2>.

    out->write( data = <fs2> name = `<fs2>` ).

    TYPES t_gs3 TYPE ANY STRUCTURE CONTAINING comp2 TYPE i
                                              substruc TYPE s1.

    FIELD-SYMBOLS <fs3> TYPE t_gs2.

    ASSIGN struct_2 TO <fs3>.

    out->write( data = <fs3> name = `<fs3>` ).
  ENDMETHOD.

  METHOD itabs_generic_struc_type.

    TYPES: BEGIN OF ts_5,
             comp_a TYPE i,
             comp_b TYPE c LENGTH 10,
             comp_c TYPE decfloat34,
             comp_d TYPE string,
           END OF ts_5,
           tab_type1 TYPE TABLE OF ts_5 WITH EMPTY KEY.

    TYPES: c10         TYPE c LENGTH 10,
           t_gs4       TYPE ANY STRUCTURE CONTAINING comp_b TYPE c10
                                               comp_c TYPE decfloat34
                                               comp_a TYPE i,
           tab_type_gs TYPE TABLE OF t_gs4 WITH EMPTY KEY.

    FIELD-SYMBOLS <tgs> TYPE tab_type_gs.

    DATA(itab1) = VALUE tab_type1(
    ( comp_a = 1 comp_b = 'a'
      comp_c = CONV decfloat34( '1.1' ) comp_d = `hello` )
    ( comp_a = 2 comp_b = 'b'
      comp_c = CONV decfloat34( '2.2' ) comp_d = `world` )
    ( comp_a = 3 comp_b = 'c'
      comp_c = CONV decfloat34( '3.3' ) comp_d = `ABAP` ) ).

    ASSIGN itab1 TO <tgs>.

    out->write( <tgs> ).

    TYPES: BEGIN OF ts_6,
             comp_a TYPE i,
             comp_b TYPE c LENGTH 10,
           END OF ts_6,
           tab_type2 TYPE TABLE OF ts_6 WITH EMPTY KEY.

    DATA(itab2) = VALUE tab_type2( ( comp_a = 4 comp_b = 'd' )
                                   ( comp_a = 5 comp_b = 'e' ) ).

    <tgs> = CORRESPONDING #( BASE ( <tgs> ) itab2 ).

    out->write( data = <tgs> name = `<tgs>` ).

    struc_1 = VALUE #( comp1 = 6 comp2 = 7 ).
    DATA itab3 LIKE TABLE OF struc_1 WITH EMPTY KEY.
    APPEND struc_1 TO itab3.

    <tgs> = CORRESPONDING #( BASE ( <tgs> )
      itab3 MAPPING comp_a = comp1 comp_b = comp2 ).

    out->write( data = <tgs> name = `<tgs>` ).

    LOOP AT <tgs> ASSIGNING FIELD-SYMBOL(<wa>).
      <wa>-comp_b = to_upper( <wa>-comp_b ).
      ASSIGN <wa>-('comp_d') TO FIELD-SYMBOL(<comp>).
      <comp> = to_upper( <comp> ).
    ENDLOOP.

    SORT <tgs> BY comp_a DESCENDING.

    out->write( data = <tgs> name = `<tgs>` ).

    READ TABLE <tgs> ASSIGNING FIELD-SYMBOL(<line>)
      WITH KEY comp_b = 'C'.

    IF <line> IS ASSIGNED.
      out->write( data = <line> name = `<line>` ).
    ENDIF.

    DATA(ref) = REF #( <tgs>[ 5 ] OPTIONAL ).

    out->write( data = ref->* name = `ref->*` ).
  ENDMETHOD.

  METHOD dynamic_implementation.

    CREATE DATA result LIKE request_dynamic.

    dyn_where_clause = cl_abap_dyn_prg=>escape_quotes( dyn_where_clause ).

    TRY.
        LOOP AT request_dynamic ASSIGNING FIELD-SYMBOL(<instance>)
         WHERE (dyn_where_clause).

          INSERT <instance> INTO TABLE result->*.
          INSERT <instance>-('%cid_ref') INTO TABLE cid_ref_tab.

        ENDLOOP.

        out->write( data = result->* name = `result->*` ).
        out->write( data = cid_ref_tab name = `cid_ref_tab` ).
      CATCH cx_sy_itab_dyn_loop INTO DATA(error).
        out->write( |Error: { error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD generic_implementation.
    CREATE DATA result LIKE request_generic.

    LOOP AT request_generic ASSIGNING FIELD-SYMBOL(<instance>)
      WHERE %control IS NOT INITIAL.

      INSERT <instance> INTO TABLE result->*.
      INSERT <instance>-%cid_ref INTO TABLE cid_ref_tab.

    ENDLOOP.

    out->write( data = result->* name = `result->*` ).
    out->write( data = cid_ref_tab name = `cid_ref_tab` ).
  ENDMETHOD.

  METHOD static_implementation.
    CREATE DATA result LIKE request_static.

    LOOP AT request_static ASSIGNING FIELD-SYMBOL(<instance>)
      WHERE %control IS NOT INITIAL.

      INSERT <instance> INTO TABLE result->*.
      INSERT <instance>-%cid_ref INTO TABLE cid_ref_tab.

    ENDLOOP.

    out->write( data = result->* name = `result->*` ).
    out->write( data = cid_ref_tab name = `cid_ref_tab` ).
  ENDMETHOD.

  METHOD constructor.

    update1 = VALUE #(
      ( %cid_ref = `cid1` %control-num1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid2` %control-num1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid3` %control-num1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid4` %control-num1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid5` )
      ( %cid_ref = `cid6` ) ).

    update2 = VALUE #(
      ( %cid_ref = `cid7` %control-field1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid8` %control-field1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid9` %control-field1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid10` %control-field1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid11` )
      ( %cid_ref = `cid12` ) ).

    update3 = VALUE #(
      ( %cid_ref = `cid13` %control-field1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid14` %control-field1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid15` %control-field1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid16` %control-field1 = if_abap_behv=>mk-on )
      ( %cid_ref = `cid17` )
      ( %cid_ref = `cid18` ) ).

    delete = VALUE #(
      ( %cid_ref = `cid19` )
      ( %cid_ref = `cid20` )
      ( %cid_ref = `cid21` )
      ( %cid_ref = `cid22` ) ).

  ENDMETHOD.
ENDCLASS.
```


</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p> 


## Excursions

### sy Structure

- The `sy` (or `syst`) structure is a built-in data object. 
- The components of the structure represent ABAP system fields. 
- These fields, filled by the [ABAP runtime framework](https://help.sap.com/docs/abap-cloud/abap-keyword/abap-runtime-framework), can be used to query system information and more. 
- Typically, they should only be read, and not overwritten. 
- Prominent system fields are the following 
  - `sy-subrc`: Return code of many ABAP statements; typically, the value 0 indicates success
  - `sy-tabix`: Row index of internal tables
  - `sy-index`: Loop pass index
- These ones and others can be used in [ABAP for Cloud Development](https://help.sap.com/docs/abap-cloud/abap-keyword/abap-for-cloud-development). However, most of the fields should not be used in ABAP for Cloud Development (indicated by a syntax warning) because they refer to [Standard ABAP](https://help.sap.com/docs/abap-cloud/abap-keyword/standard-abap) contexts (e.g. classic dynpros and lists), or their values are not relevant in a cloud context. 
- More information about the purpose of the individual components is available at [ABAP System Fields (F1 documentation for Standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abensystem_fields.html).


The following example demonstrates a selection of ABAP system fields. It uses artifacts from the ABAP cheat sheet repository. Note the comments in the code because a syntax warning will be displayed when inserting the code in a demo class that uses ABAP for Cloud Development. It is meant to emphasize that multiple system fields should not be used in ABAP for Cloud Development.  
To try the example out, create a demo class named `zcl_demo_abap`. If it already exists, reuse it. Otherwise, create a new class with a different name. Paste the code into it. If you choose a different class name, update the class name in the code snippet accordingly. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console. 

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

Using [Runtime Type Services (RTTS)](https://help.sap.com/docs/abap-cloud/abap-keyword/runtime-type-services)
you can ...
- get type information on data objects, data types or [instances](https://help.sap.com/docs/abap-cloud/abap-keyword/instance) at runtime ([Runtime Type Identification (RTTI)](https://help.sap.com/docs/abap-cloud/abap-keyword/runtime-type-identification)).
- define and create new data types as [type description objects](https://help.sap.com/docs/abap-cloud/abap-keyword/type-description-object) at runtime ([Runtime Type Creation (RTTC)](https://help.sap.com/docs/abap-cloud/abap-keyword/runtime-type-creation)).

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
- Currently, static boxes are supported as boxed components, enabling [initial value sharing](https://help.sap.com/docs/abap-cloud/abap-keyword/initial-value-sharing). Find more information [here](https://help.sap.com/docs/abap-cloud/abap-keyword/static-boxes).
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
  - To try the example out, create a demo class named `zcl_demo_abap`. If it already exists, reuse it. Otherwise, create a new class with a different name. Paste the code into it. If you choose a different class name, update the class name in the code snippet accordingly. Activate the class.
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


### Recursive Structure References

- Recursive structure references are components of a structured type that represent data references to the same structure in which they are defined. 
- They enable, for example, linked lists that can be processed in a type-safe manner.

```abap
TYPES: BEGIN OF struc_type, 
         num  TYPE i, 
         text TYPE string, 
         sref TYPE REF TO struc_type, 
       END OF struc_type.
```

Expand the following collapsible section for example code. To try it out, create a demo class named `zcl_demo_abap`, or reuse it if it already exists. Paste the code into it. If you choose a different class name, update the class name in the code snippet accordingly. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console. 


<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

- The example class includes three demos. It demonstrates a recursive structure reference (demo 1). Additionally, it incorporates another structure containing a component of type `REF TO data` (demos 2a and 2b).
- Structures used in the demos:
  - `struc_rec` (demo 1): A recursive structure with an integer value and a reference to itself.
  - `struc_data` (demos 2a and 2b): Similar to `struc_rec`, but includes a component of type `REF TO data`.
- It provides methods for adding elements and reversing linked lists.
  - The `prepend1` (for demo 1) and `prepend2` (for demos 2a and 2b) methods add elements to linked lists.
  - `reverse1` (for demo 1) and `reverse2` (for demos 2a and 2b) reverse those lists.
- The `main` method shows how to add elements to the lists and reverse them:
  - Demo 1: Illustrates the structure with a recursive reference. The `reverse1` method demonstrates type-safe access, while `reverse2` (for demos 2a and 2b) uses a cast that may raise an exception when handling the other structure.
  - Demos 2a and 2b: Show the structure without a recursive reference. Demo 2a represents a successful case, whereas demo 2b triggers an exception.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
  PRIVATE SECTION.
    "Components for demo 1
    TYPES:
      BEGIN OF struc_rec,
        value TYPE i,
        next  TYPE REF TO struc_rec,
      END OF struc_rec.

    DATA head1 TYPE REF TO struc_rec.

    METHODS prepend1 IMPORTING value TYPE i.
    METHODS reverse1.

    "Components for demos 2a/b
    TYPES:
      BEGIN OF struc_data,
        value TYPE i,
        next  TYPE REF TO data,
      END OF struc_data.

    DATA head2 TYPE REF TO struc_data.

    METHODS prepend2 IMPORTING value TYPE i.
    METHODS reverse2 RAISING cx_sy_move_cast_error.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    out->write( `-------- Demo 1 (uses a recursive structure reference) --------` ).

    DATA(list1) = NEW zcl_demo_abap( ).
    list1->prepend1( 1 ).
    list1->prepend1( 2 ).
    list1->prepend1( 3 ).

    out->write( list1->head1 ).

    list1->reverse1( ).

    out->write( list1->head1 ).

    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( `-------- Demo 2a (does not use a recursive structure reference) --------` ).

    DATA(list2a) = NEW zcl_demo_abap( ).
    list2a->prepend2( 4 ).
    list2a->prepend2( 5 ).
    list2a->prepend2( 6 ).

    out->write( list2a->head2 ).

    TRY.
        list2a->reverse2( ).
        out->write( list2a->head2 ).
      CATCH cx_sy_move_cast_error INTO DATA(err2a).
        out->write( err2a->get_text( ) ).
    ENDTRY.

    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( `-------- Demo 2b (does not use a recursive structure reference and raises an exception) --------` ).

    DATA(list2b) = NEW zcl_demo_abap( ).
    list2b->prepend2( 7 ).
    list2b->prepend2( 8 ).
    list2b->prepend2( 9 ).

    out->write( list2b->head2 ).

    "Manipulation for provoking an error
    DATA(error) = 'error'.
    list2b->head2->next = REF #( error ).

    TRY.
        list2b->reverse2( ).
        out->write( list2b->head2 ).
      CATCH cx_sy_move_cast_error INTO DATA(err2b).
        out->write( err2b->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD prepend1.
    head1 = NEW #( next = head1 value = value ).
  ENDMETHOD.

  METHOD reverse1.
    DATA last1 TYPE REF TO struc_rec.
    DATA(current1) = head1.

    WHILE current1 IS NOT INITIAL.
      DATA(next1) = current1->next.
      current1->next = last1.
      last1 = current1.
      current1 = next1.
    ENDWHILE.

    head1 = last1.
  ENDMETHOD.

  METHOD prepend2.
    head2 = NEW #( next = head2 value = value ).
  ENDMETHOD.

  METHOD reverse2.
    DATA last2 TYPE REF TO struc_data.
    DATA(current2) = head2.

    WHILE current2 IS NOT INITIAL.
      DATA next2 TYPE REF TO struc_data.
      next2 = CAST #( current2->next ).
      current2->next = last2.
      last2 = current2.
      current2 = next2.
    ENDWHILE.

    head2 = last2.
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