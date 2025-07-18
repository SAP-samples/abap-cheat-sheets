<a name="top"></a>

# ABAP Managed Database Procedures (AMDP)

- [ABAP Managed Database Procedures (AMDP)](#abap-managed-database-procedures-amdp)
  - [Introduction](#introduction)
  - [AMDP Classes](#amdp-classes)
  - [AMDP Methods](#amdp-methods)
  - [AMDP Procedures](#amdp-procedures)
  - [AMDP Functions](#amdp-functions)
    - [AMDP Table Functions](#amdp-table-functions)
      - [AMDP Table Functions for AMDP Methods](#amdp-table-functions-for-amdp-methods)
      - [AMDP Table Functions for CDS Table Functions](#amdp-table-functions-for-cds-table-functions)
    - [AMDP Scalar Functions](#amdp-scalar-functions)
      - [AMDP Scalar Functions for AMDP Methods](#amdp-scalar-functions-for-amdp-methods)
      - [AMDP Scalar Functions for CDS Scalar Functions](#amdp-scalar-functions-for-cds-scalar-functions)
  - [Using AMDP in ABAP for Cloud Development](#using-amdp-in-abap-for-cloud-development)
    - [Notes on Client Handling and Client Safety](#notes-on-client-handling-and-client-safety)
    - [Restrictions for AMDP Methods in ABAP for Cloud Development](#restrictions-for-amdp-methods-in-abap-for-cloud-development)
    - [Making AMDP Methods Client-Safe](#making-amdp-methods-client-safe)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)


This cheat sheet gathers basic information on [ABAP Managed Database
Procedures
(AMDP)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_managed_db_proc_glosry.htm "Glossary Entry").
Find more details
[here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp.htm)
in the ABAP Keyword Documentation.

## Introduction

-   AMDP are a class-based framework for managing and calling

    -   [database
        procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendatabase_procedure_glosry.htm "Glossary Entry")
        (which is a synonym for [stored
        procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstored_procedure_glosry.htm "Glossary Entry"), i.
        e. the procedures are stored in the database - the [SAP HANA
        database](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhana_database_glosry.htm "Glossary Entry")
        in this case - and executed there)
    -   [database
        functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendatabase_function_glosry.htm "Glossary Entry")
        (which are [SQLScript
        functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_script_function_glosry.htm "Glossary Entry")
        in the SAP HANA database)

    in [AS ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenas_abap_glosry.htm "Glossary Entry").

-   "ABAP managed" enters the picture in ABAP with the option of
    implementing special [AMDP
    procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp_procedure_glosry.htm "Glossary Entry")
    as database procedures and [AMDP
    functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp_function_glosry.htm "Glossary Entry")
    as database functions.
-   The implementations are programmed using a database-specific
    language. Currently, AMDP only supports database procedures and
    functions from the SAP HANA database. That is,
    [SQLScript](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_script_glosry.htm "Glossary Entry")
    is the programming language of choice.
-   AMDP procedures and functions are part of a dedicated [AMDP
    class](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp_class_glosry.htm "Glossary Entry")
    and declared and implemented as part of a method. The classes and
    methods have certain characteristics as outlined further down.
-   The AMDP framework replicates the procedure or function to the
    database system, i. e. despite the fact that the programming happens
    in an AMDP class (which is an [ABAP
    Repository](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_repository_glosry.htm "Glossary Entry")
    object as other global classes, too), the (SQLScript) code is
    executed only on the (SAP HANA) database and not in AS ABAP, i. e.
    method calls are sent to the database procedure or function.

> [!NOTE]
>- The use of AMDP is not recommended if the same task can be
        achieved using [ABAP
        SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_glosry.htm "Glossary Entry").
>- AMDP classes can only be edited with the [ABAP development tools for Eclipse
        (ADT)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenadt_glosry.htm "Glossary Entry").

<p align="right"><a href="#top">⬆️ back to top</a></p>

## AMDP Classes

-   An [AMDP class](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp_class_glosry.htm "Glossary Entry")
    is an [ABAP repository](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_repository_glosry.htm "Glossary Entry")
    object like other global classes.
-   However, an AMDP class includes the specification of the interface
    `IF_AMDP_MARKER_HDB` for the SAP HANA
    database (indicated by `HDB`), which is currently the
    only possible database.
-   An AMDP class can contain both (one or more) [AMDP
    methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp_method_glosry.htm "Glossary Entry")
    and non-AMDP methods.

Example for a [declaration  part](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_part_glosry.htm "Glossary Entry")
of an AMDP class:

```abap
CLASS cl_some_amdp_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "Specifying the interface is mandatory
    INTERFACES if_amdp_marker_hdb.
...
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## AMDP Methods

-   Can be created as [instance
    methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_method_glosry.htm "Glossary Entry")
    using <code>METHODS</code> or [static
    methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_method_glosry.htm "Glossary Entry")
    using <code>CLASS-METHODS</code> in any visibility section.
-   Cannot be identified as AMDP methods in the declaration part of the
    class since there are no specific additions to the methods.
    Exceptions: AMDP function implementations that implement any [CDS table functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_table_function_glosry.htm "Glossary Entry")
    as shown further down and method declarations using [`AMDP
    OPTIONS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_amdp_options.htm)
    that are not dealt with here.

AMDP method declarations in any visibility section like non-AMDP
methods:

```abap
...
PUBLIC SECTION.

  METHODS some_amdp_meth
    ... "Here go the parameters

PRIVATE SECTION.

  CLASS-METHODS another_amdp_meth
    ... "Here go the parameters

...
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## AMDP Procedures

Despite the fact that AMDP methods cannot be identified as such from the
[declaration
part](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_part_glosry.htm "Glossary Entry")
(apart from the exceptions mentioned above), the declaration part of
[AMDP
procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp_procedure_glosry.htm "Glossary Entry")
has special characteristics:

-   Parameters must be [passed by
    value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpass_by_value_glosry.htm "Glossary Entry")
    using <code>VALUE(...)</code>. [Passing by
    reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpass_by_reference_glosry.htm "Glossary Entry")
    is not allowed.
-   Parameter types ...
    -   must not be generic.
    -   can only be [elementary data
        types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenelementary_data_type_glosry.htm "Glossary Entry")
        and [table
        types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_type_glosry.htm "Glossary Entry")
        with a structured row type (and this type can only contain
        elementary data types as components).
-   Return values cannot be declared using <code>RETURNING</code>.
-   Only input parameters can be flagged as optional parameters.

Example for an AMDP procedure's declaration part:

```abap
...
PUBLIC SECTION.

  "Table type with a structured row type
  TYPES tab_type TYPE STANDARD TABLE OF dbtab WITH EMPTY KEY.

  METHODS amdp_meth
    IMPORTING VALUE(num) TYPE i,
    EXPORTING VALUE(tab) TYPE tab_type.

...
```

In contrast to the declaration part, the [implementation
part](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp_procedure_method_glosry.htm "Glossary Entry")
of an AMDP method has multiple special additions for AMDP purposes
following <code>METHOD</code> and the method name:

```abap
...
METHOD amdp_meth
  BY DATABASE PROCEDURE
  FOR HDB  
  LANGUAGE SQLSCRIPT  
  OPTIONS READ-ONLY
  USING db_object.   "see comments further down
                         
"Beginning of the SQLScript code (note that it is not ABAP code although it looks similar)

  ... "Here goes SQLScript code
      "Note that an AMDP method implementation must not be empty.

"End of the SQLScript code

ENDMETHOD.
...

"Comments:
"  BY DATABASE PROCEDURE   -> Flags the AMDP method as AMDP procedure
"  FOR HDB                 -> Definess the database system where the method is to be used;
"                             currently, only HDB (SAP HANA database) is possible
"  LANGUAGE SQLSCRIPT      -> Defines the programming language of the database system
"  OPTIONS READ-ONLY       -> Specifies database-specific options
"  USING db_object.        -> Optional addition; specifies database objects;
"                             can also be AMDP procedures and functions
```

Note:

-   In [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_for_sap_cloud_glosry.htm) (i.e. the [restricted ABAP language version](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrestricted_version_glosry.htm "Glossary Entry")
    scope), only reads are allowed. Hence, the addition `OPTIONS READ-ONLY` is mandatory. Furthermore, you must make sure
    that the database objects that are specified after `USING` are accessible.
-   Generally, in [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm) (i.e. the [unrestricted ABAP language
    version](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenunrestricted_version_glosry.htm "Glossary Entry") scope), more syntax options are allowed for AMDP method declaration
    and implementation parts. Check the ABAP Keyword Documentation for
    more details as covered further down.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## AMDP Functions

[Scalar](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp_scalar_function_glosry.htm "Glossary Entry") and [table functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_function_glosry.htm "Glossary Entry") can be managed as [AMDP functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp_function_glosry.htm "Glossary Entry").
[AMDP table functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp_table_function_glosry.htm "Glossary Entry"), as the name suggests, have a tabular [return value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreturn_value_glosry.htm "Glossary Entry"), while AMDP scalar functions have a scalar (i.e. an elementary) return value. These functions allow complex custom calculations to be executed directly on the SAP HANA database to boost performance, enhancing performance and enabling reuse.


<p align="right"><a href="#top">⬆️ back to top</a></p>

### AMDP Table Functions 

Two kinds of AMDP table functions are available:

- *AMDP table functions for AMDP methods*: Functions that can only be accessed in other AMDP methods (i. e. other AMDP functions or procedures) and cannot be called directly in ABAP
- *AMDP table functions for CDS table functions*: Functions that implement [CDS table functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_table_function_glosry.htm "Glossary Entry") that can be accessed in ABAP SQL (described further down)

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### AMDP Table Functions for AMDP Methods

Characteristics of method declaration parts of AMDP table functions for AMDP methods:

- Similar to AMDP procedures, the methods can be declared as static or instance methods in any visibility section. 
- The method parameters must include a return value using `RETURNING` and having a tabular data type.
- Additionally, the parameters can include elementary and tabular input parameters.
- No class-based exceptions can be declared using `RAISING`.

Example of an AMDP table function's declaration part:

```abap
...

PUBLIC SECTION.

  "Table type with a structured row type
  TYPES tab_type TYPE STANDARD TABLE OF dbtab WITH EMPTY KEY.

  METHODS amdp_func
    IMPORTING VALUE(num)       TYPE i,
              VALUE(some_elem) TYPE c LENGTH 3,
    RETURNING VALUE(tab)       TYPE tab_type.

...
```

The implementation part of an AMDP function is similar to the one of AMDP procedures as shown above. The difference is the use of `BY DATABASE FUNCTION` instead of `BY DATABASE PROCEDURE`:

```abap
...

METHOD amdp_func
  BY DATABASE FUNCTION    "<- Flags the AMDP method as AMDP function
  FOR HDB
  LANGUAGE SQLSCRIPT
  OPTIONS READ-ONLY
  USING db_object.

*Beginning of the SQLScript code (note that it is not ABAP code)

*Here goes SQLScript code
*AMDP table function to be called by other AMDP methods only  
  ... 

*End of the SQLScript code

ENDMETHOD.

...
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### AMDP Table Functions for CDS Table Functions

- Each [CDS table function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_table_function_glosry.htm "Glossary Entry") is linked with an AMDP function in which it is implemented using SQLScript.
- Can be used as data sources of ABAP SQL read statements.
- Characteristics for method declaration and implementation parts regarding *AMDP functions for CDS table functions*:
  - Method can only be declared as a static method in the public visibility section of an AMDP class using `CLASS-METHODS`.
  - For the declaration, there is a special form with the addition `FOR TABLE FUNCTION`.
  - The [parameter interface](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenparameter_interface_glosry.htm "Glossary Entry")
    is not specified. Instead, the input parameters are determined by the [input parameters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninput_parameter_glosry.htm "Glossary Entry") of the CDS table function (i. e. the names and data types - which are always elementary - specified there are used). As the
      return value, a standard table with an empty key is generated based on the structured row type including the components as specified in the CDS table function.

**Using AMDP table functions for CDS table functions**

To use CDS table functions, you need these development objects:

- CDS table function as CDS entity defined using `define table function`
- AMDP function implementing the table function as database function in an AMDP class
  - Declaration: `CLASS-METHODS table_func FOR TABLE FUNCTION some_ddl_source.`
  - Note that there is no parameter interface. Parameters are derived from the CDS table function.
- You can then use the CDS table function as source in CDS entities and for ABAP SQL `SELECT` statements, for example: `SELECT * FROM some_ddl_source INTO ...`. Furthermore, CDS table functions represent globally available structured types (but they are not usable for typing in the ABAP Dictionary).

**Notes on the syntax to create CDS table functions**

Example: 

```
//Here go annotations.
define table function some_ddl_source
  with parameters
    param  : abap.char(3)
  returns
  {
    client : abap.clnt;
    field1 : abap.char(5);
    field2 : abap.int4;
  }
  implemented by method amdp_class=>amdp_method;
```

- You define a [CDS DDL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_ddl_glosry.htm "Glossary Entry") source, i. e. a [CDS entity](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_entity_glosry.htm "Glossary Entry"), with the notation `define table function`.
- Optionally, you can specify input parameters using `... with parameters parameter1, parameter2, ...` in a comma-separated list. Elementary data types are expected.
- You must specify an element list using `... returns { element1; element2; ...; } ...`. The elements - elementary data types are expected - are separated by semicolons and determine the components of the structured data type of the tabular return value. The syntax allows to specify key elements using the `key` addition. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENCDS_F1_RETURN_LIST_ELEMENT.html).
- You must specify the `implemented by method` addition followed by a fully qualified method name in the form of `amdp_class=>amdp_method` using the names of the AMDP class and method.
- Find more information on table functions [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_table_functions.htm) and in the subtopics. Regarding the annotations that can be specified, in [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm) in particular, pay attention to client handling and client safety. 

<p align="right"><a href="#top">⬆️ back to top</a></p>

### AMDP Scalar Functions 

Two kinds of AMDP scalar functions are available:

- *AMDP scalar functions for AMDP methods*: Functions that can be accessed in other AMDP methods (i. e. other AMDP functions or procedures) and called directly in ABAP
- *AMDP scalar functions for CDS scalar functions*: Functions that implement [CDS scalar functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_scalar_function_glosry.htm) that can be accessed in ABAB CDS and ABAP SQL (described further down)

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### AMDP Scalar Functions for AMDP Methods

The characteristics of method declaration parts of *AMDP scalar functions for AMDP methods* are similar to AMDP table functions.
Among the differences are: 
- The return value and the input parameters must have an elementary type.
- The AMDP scalar function can be called like a regular method in ABAP.

> [!NOTE]
> The executable example in the *AMDP Scalar Functions for CDS Scalar Functions* section includes *AMDP scalar functions for AMDP methods*.

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### AMDP Scalar Functions for CDS Scalar Functions

- Unlike *AMDP scalar functions for AMDP methods*, *AMDP scalar functions for CDS scalar functions*:
  - Must be declared in the public visibility section of a class as static methods using `CLASS-METHODS`.
  - Cannot be called in ABAP like a regular method.
- For the declaration, there is a special form with the addition `FOR SCALAR FUNCTION`.
- No parameter interface is specified in the declaration. Parameters are derived from the associated CDS scalar function. Input parameters (optional) and a return value are available. All  must be typed with an elementary type.
- Where used: 
  - CDS view entities; different positions such as in the element list, in an `ON` or `WHERE` condition and others, are possible
  - ABAP SQL (possible for [SQL-based scalar functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_sql_scalar_glosry.htm))

> [!NOTE]
> - CDS scalar functions are available as [analytical scalar functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_ana_scalar_glosry.htm) and [SQL-based scalar functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_sql_scalar_glosry.htm), i.e. they are either evalauted by an SQL environment or an analytical runtime environment.
> - The example below focuses on SQL-based scalar functions. Analytical scalar functions are predelivered [system functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_system_func_glosry.htm) and cannot be user-defined. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENCDS_ANA_SCALAR_FUNCTION.html).

**Using AMDP scalar functions for CDS scalar functions**

To use CDS scalar functions, you need these three development objects:

- CDS scalar function as CDS entity defined using `define scalar function`
- [CDS scalar function implementation reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_dsfi_glosry.htm) that binds the scalar function to the SQL environment and to an AMDP function implementation
- AMDP function implementing the scalar function as database function in an AMDP class
  - Declaration: `CLASS-METHODS some_meth FOR SCALAR FUNCTION some_cds_scalar_func.` 
  - Note that there is no parameter interface. Parameters are derived from the CDS scalar function.

**Notes on the syntax to create CDS scalar functions**

Example: 

```abap
define scalar function zdemo_abap_scalar_func
  with parameters
    // Built-in DDIC types
    param1 : abap.char(3),
    param2 : abap.decfloat34,
    param3 : abap.int4,
    // DDIC data elements
    param4 : abap_boolean,
    param5 : land1,
    // CDS simple types
    param6 : calendar_fiscal_operation,
    // Generic types
    param7 : numeric,
    param8 : any,
    // Referencing types from input parameters
    param9 : type of param1,
    param10: type of param2,
    // Specifying a reference to a currency key, 
    // unit key, calculated unit, or none of them
    param11: numeric
      with reference type [ #CUKY ],
    param12: numeric
      with reference type [ #CUKY, #UNIT, #CALC ],
    param13: numeric
      with reference type [ #NONE ],
    // Mandatory reference to unit key and unit key
    param14: abap.quan(8,2)
      with reference type [ #UNIT ],
    param15: abap.curr(8,2)
      with reference type [ #CUKY ]
  returns abap.dec(8,2)
```

- Input parameters can optionally be specified with `with parameter` in a comma-separated list, 
- Non-optional scalar return value
- Typing options:
  - [Built-in DDIC types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_ddic_type_glosry.htm), [CDS simple types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_simple_type_glosry.htm), [DDIC data elements](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_element_glosry.htm)
  - Referencing types using `TYPE OF some_inp_param`: `some_inp_param` can be the name of any input parameter from the list, and that does not reference a type itself
  - Input parameters can be typed with the generic types `any` and `numeric`
  - `with reference type` addition: 
    - Specifies a reference type and refines what type is allowed for the actual parameter passed for the input and return parameters
    - Specification options include a reference to a currency key (`#CUKY`), a unit key (`#UNIT`), or a calculated unit (`#CALC`), or none of them (`#NONE`); for some types such as `abap.quan` and `abap.curr`, the reference type is required 
    - The addition also allows to specify reference types dynamically using a `case` statement; find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_scalar_typing.htm)


The following example includes the creation of the three required artifacts (CDS scalar function, CDS scalar function implementation reference, AMDP class including an *AMDP scalar function for CDS scalar function*, as well as an *AMDP scalar function for AMDP method*). The AMDP class implements the `if_oo_adt_classrun` interface, making the class executable. The simplified example illustrates AMDP scalar functions at a high level. After activating all artifacts, choose *F9* in ADT to run the class. The example is set up to display output in the console. 

> [!NOTE]
> As a prerequisite for exploring the example, you have imported the ABAP cheat sheet repository as the example uses some of its artifacts.

<details>
  <summary>🟢 Click to expand for implementation steps and example code</summary>
  <!-- -->

<br>

**Create a CDS scalar function definition**
   
- Create a new repository object in ADT, for example, by making a right-click on your demo package and choosing *New -> Other ABAP Repository Objects*.
- Filter for *Scalar Function Definition*.
- Walk through the wizard, provide the name `zdemo_abap_scalar_func`, and select the *defineScalarFunction* template.
- Insert the following code into the artifact and activate. The demo use case is to calculate the ratio of a share to a total.

```
define scalar function zdemo_abap_scalar_func
  with parameters
    num    : numeric,
    total  : type of num
  returns abap.dec( 8, 2 )
```

**Create a scalar function implementation reference**

- Create a new repository object in ADT, for example, by making a right-click on your demo package and choosing *New -> Other ABAP Repository Objects*.
- Filter for *Scalar Function Implementation Reference*.
- Walk through the wizard and make the following entries:
  - Provide the name `zdemo_abap_scalar_func_sql`. Note that the name must be identical to the CDS scalar function plus the suffix `_sql` (`_ana` is used for analytical scalar function).
  - Select *SQL Engine* as engine. 
  - *AMDP Reference*: `zcl_demo_abap=>execute_scalar_func`.
- Activate. At this stage, a warning indicates the AMDP method does not exist.

**Create an AMDP class that includes an AMDP function implementing the scalar function as database function**

- Create a demo class named `zcl_demo_abap`. 
- Insert the code below into the class. 
- After activation, choose *F9* in ADT to run the class. The example is set up to display output in the console.

> [!NOTE]
> - The demo class includes multiple AMDP methods demonstrating several aspects regarding AMDP scalar functions: 
>   - `get_max_fltime` demonstrates an *AMDP scalar function for AMDP method*. It returns the longest flight time among all flights of a certain carrier.
>   - `select_entries_w_max_fltime` demonstrates an AMDP procedure that includes calling the `get_max_fltime` AMDP scalar function. The effect is the same as shown with an ABAP SQL `SELECT` statement using `get_max_fltime`.
>   - `execute_scalar_func` demonstrates an *AMDP scalar function for CDS scalar function*. It calculates the ratio of a share to a total.
> - The example implementations are simplified to foucs on high-level functionality.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun,
      if_amdp_marker_hdb.

    CLASS-METHODS class_constructor.

    TYPES occ_rate_type TYPE p LENGTH 8 DECIMALS 2.

    "AMDP scalar function for AMDP method
    METHODS get_max_fltime
      AMDP OPTIONS READ-ONLY CDS SESSION CLIENT DEPENDENT
      IMPORTING VALUE(carrid)     TYPE zdemo_abap_flsch_ve-carrid
      RETURNING VALUE(max_fltime) TYPE zdemo_abap_flsch_ve-fltime.

    "AMDP procedure whose implementation includes calling an AMDP scalar function
    TYPES: BEGIN OF flight_struc_type,
             carrid   TYPE zdemo_abap_flsch_ve-carrid,
             connid   TYPE zdemo_abap_flsch_ve-connid,
             cityfrom TYPE zdemo_abap_flsch_ve-cityfrom,
             cityto   TYPE zdemo_abap_flsch_ve-cityto,
             fltime   TYPE zdemo_abap_flsch_ve-fltime,
           END OF flight_struc_type,
           flight_tab_type TYPE TABLE OF flight_struc_type WITH EMPTY KEY.

    METHODS select_entries_w_max_fltime
      AMDP OPTIONS READ-ONLY CDS SESSION CLIENT DEPENDENT
      IMPORTING VALUE(carrid)     TYPE zdemo_abap_flsch_ve-carrid
      EXPORTING VALUE(flight_tab) TYPE flight_tab_type.

    "AMDP scalar function for CDS scalar function
    CLASS-METHODS execute_scalar_func FOR SCALAR FUNCTION zdemo_abap_scalar_func.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------*
*& AMDP function implementation for an AMDP scalar function
*&---------------------------------------------------------------------*

    out->write( |AMDP function implementation for an AMDP scalar function\n\n| ).

    DATA(oref) = NEW zcl_demo_abap( ).

    DATA(max_fltime_lh) = oref->get_max_fltime( carrid = 'LH' ).
    out->write( max_fltime_lh ).

    DATA(max_fltime_az) = oref->get_max_fltime( carrid = 'AZ' ).
    out->write( max_fltime_az ).

    DATA(max_fltime_dl) = oref->get_max_fltime( carrid = 'DL' ).
    out->write( max_fltime_dl ).

    out->write( |\n{ repeat( val = `*` occ = 100 ) }\n| ).

*&---------------------------------------------------------------------*
*& AMDP function implementation for an AMDP scalar function in ABAP SQL
*&---------------------------------------------------------------------*

    out->write( |AMDP function implementation for an AMDP scalar function in ABAP SQL\n\n| ).

    DATA carrid_tab TYPE TABLE OF zdemo_abap_flsch_ve-carrid WITH EMPTY KEY.
    carrid_tab = VALUE #( ( 'LH' ) ( 'AZ' ) ( 'DL' ) ).

    LOOP AT carrid_tab INTO DATA(carrid).
      SELECT carrid, connid, cityfrom, cityto, fltime
             FROM zdemo_abap_flsch_ve
             WHERE carrid = @carrid AND
                   fltime = @( NEW zcl_demo_abap( )->get_max_fltime( carrid = carrid ) )
             INTO TABLE @DATA(selection_result).

      out->write( selection_result ).
      out->write( |\n\n| ).
    ENDLOOP.

    out->write( |\n{ repeat( val = `*` occ = 100 ) }\n| ).

*&---------------------------------------------------------------------*
*& AMDP procedure whose implementation includes calling an AMDP scalar
*& function
*&---------------------------------------------------------------------*

    out->write( |AMDP procedure whose implementation includes calling an AMDP scalar function\n\n| ).

    LOOP AT carrid_tab INTO carrid.
      NEW zcl_demo_abap( )->select_entries_w_max_fltime(
           EXPORTING carrid = carrid
           IMPORTING flight_tab = DATA(flights) ).

      out->write( flights ).
      out->write( |\n\n| ).
    ENDLOOP.

    out->write( |\n{ repeat( val = `*` occ = 100 ) }\n| ).

*&---------------------------------------------------------------------*
*& AMDP scalar function for CDS scalar function
*&---------------------------------------------------------------------*

    out->write( |AMDP scalar function for CDS scalar function\n\n| ).

    MODIFY zdemo_abap_tab1 FROM TABLE @( VALUE #(
      ( key_field = 1 num1 = 3 num2 = 10 )
      ( key_field = 2 num1 = 12 num2 = 123 )
      ( key_field = 3 num1 = 5 num2 = 39 )
      ( key_field = 4 num1 = 98 num2 = 9876 ) ) ).

    SELECT FROM zdemo_abap_tab1
       FIELDS key_field, num1, num2,
              zdemo_abap_scalar_func(
                num = num1,
                total = num2 ) AS rate
       ORDER BY key_field
       INTO TABLE @DATA(res_dbtab).

    out->write( res_dbtab ).

    out->write( |\n{ repeat( val = `*` occ = 100 ) }\n\n| ).

    "Selecting from the demo CDS view entity that uses the
    "AMDP scalar function
    "Comment in the code if you have created the CDS view entity.

*    SELECT FROM zdemo_abap_cds_ve_w_scalar
*      FIELDS *
*      ORDER BY carrid
*      INTO TABLE @DATA(res_cds_ve)
*      UP TO 15 ROWS.
*
*    out->write( res_cds_ve ).

  ENDMETHOD.

  METHOD select_entries_w_max_fltime BY DATABASE PROCEDURE
         FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY
         USING zdemo_abap_flsch_ve
               zcl_demo_abap=>get_max_fltime.

    flight_tab = select carrid, connid, cityfrom, cityto, fltime
                        from zdemo_abap_flsch_ve
                        where fltime = "ZCL_DEMO_ABAP=>GET_MAX_FLTIME"( carrid => :carrid );

  ENDMETHOD.

  METHOD execute_scalar_func BY DATABASE FUNCTION
         FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY.

    result = num / total * 100;

  ENDMETHOD.

  METHOD get_max_fltime BY DATABASE FUNCTION
         FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY
         USING zdemo_abap_flsch_ve.

    SELECT MAX(fltime)
           INTO max_fltime
           FROM zdemo_abap_flsch_ve
           WHERE carrid = :carrid;

  ENDMETHOD.

  METHOD class_constructor.
    "Preparing demo data
    zcl_demo_abap_aux=>fill_dbtabs( ).

    MODIFY zdemo_abap_flsch FROM TABLE @( VALUE #(
      ( carrid = 'AZ'
        connid =  0799
        countryfr =  'JP'
        cityfrom =  'TOKYO'
        airpfrom = 'TYO'
        countryto = 'IT'
        cityto = 'ROME'
        airpto = 'FCO'
        fltime = 940
        deptime = '124500'
        arrtime = '202500'
        distance =  6130
        distid = 'MI'
        fltype = ''
        period = 0 )
      ( carrid = 'DL'
        connid =  0199
        countryfr =  'US'
        cityfrom =  'NEW YORK'
        airpfrom = 'JFK'
        countryto = 'DE'
        cityto = 'FRANKFURT'
        airpto = 'FRA'
        fltime = 475
        deptime = '203500'
        arrtime = '103000'
        distance =  3851
        distid = 'MI'
        fltype = ''
        period = 1 )
      ( carrid = 'LH'
        connid =  0499
        countryfr =  'DE'
        cityfrom =  'FRANKFURT'
        airpfrom = 'FRA'
        countryto = 'US'
        cityto = 'NEW YORK'
        airpto = 'JFK'
        fltime = 455
        deptime = '123000'
        arrtime = '140500'
        distance =  6162
        distid = 'KM'
        fltype = 'X'
        period = 0 ) ) ).

    DELETE FROM zdemo_abap_tab1.
  ENDMETHOD.

ENDCLASS.
```


**Excursion: Create a CDS view entity that uses the *AMDP scalar function for CDS scalar function***

- Create a new repository object in ADT, for example, by making a right-click on your demo package and choosing *New -> Other ABAP Repository Objects*.
- Filter for *data definition*.
- Walk through the wizard and provide the name `zdemo_abap_cds_ve_w_scalar` for the CDS view entity.
- Insert the code below, and activate.
- You can choose `F8` to open the data preview. You can check the data after running the example class from above that populates the underlying database table (or you have already run an ABAP cheat sheet example class, or filled the table using `zcl_demo_abap_aux=>fill_dbtabs( ).`). The example class above includes an ABAP SQL `SELECT` statement. If you have created the CDS view entity, you can comment in the code in the class.

```
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity zdemo_abap_cds_ve_w_scalar
  as select from zdemo_abap_fli
{
  key carrid,
  key connid,
  key fldate,
      seatsmax,
      seatsocc,
      zdemo_abap_scalar_func(
        num     => seatsocc,
        total   => seatsmax ) as occrate
}
```

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Using AMDP in ABAP for Cloud Development

### Notes on Client Handling and Client Safety

> [!IMPORTANT]  
> - ABAP SQL features implicit client handling.
> - You cannot disable this feature in ABAP for Cloud Development, as you can in [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm). You can only access your own client in ABAP for Cloud Development.
> - Native SQL, unlike ABAP SQL, does not feature implicit client handling. AMDP, which uses Native SQL, can be used in ABAP for Cloud Development. It is essential to ensure AMDP accesses only client-safe repository objects, meaning it retrieves data solely from client-dependent sources providing data of one client, i.e. the current client. Cross-client access must be avoided. Client-independent data sources are implicitly client-safe.
> - Dedicated additions and annotations in the context of AMDP ensure client safety.

**ABAP SQL**
- An SAP system can have multiple clients, each distinguished by a unique client ID.
- Each client holds specific data. When you log in, you select a client and can only access its data. For example, if you log into client 000, you access only client 000's data.
- A client column manages client-dependent data. If a database table has a client column, it contains client-dependent data. There are also client-independent data sources accessed typically by system programs.
- ABAP SQL features implicit client handling, automatically managing client selection. When you execute an ABAP SQL statement, it uses the current client without needing separate specification. The `USING CLIENT` addition to ABAP SQL statements can alter this behavior by disabling implicit client handling, however, this addition is not available in ABAP for Cloud Development.

**Native SQL/AMDP**
- Unlike ABAP SQL, Native SQL requires you to explicitly pass the client as there is no implicit handling.
- Native SQL is sent directly to the database. 
- AMDP, which uses Native SQL, does not feature implicit client handling either.
- Although AMDP is allowed in ABAP for Cloud Development, Native SQL cannot access client-dependent data.
- When using AMDP in ABAP for Cloud Development, you must ensure access only to the current client. Client safety is essential.
- AMDP methods in ABAP for Cloud Development must be client-safe. This means SQLScript code should access data only within your client. It is required to only use artifacts that restrict access to a single client or are client-independent.
- Therefore, all objects in the `USING` list of AMDP methods must be client-safe, including CDS table functions implemented as AMDP methods.
- Additions and annotations are available, ensuring client safety by restricting access to data of the current client only.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Restrictions for AMDP Methods in ABAP for Cloud Development 

- The AMDP procedure implementation must be restricted to reads. The relevant additions are `AMDP OPTIONS READ-ONLY` in AMDP method declarations and `OPTIONS READ-ONLY` in AMDP method implementations.
- AMDP method implementations can only use SQLScript code.
- The data sources accessed must be released for ABAP for Cloud Development.
- AMDP methods must be client-safe.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Making AMDP Methods Client-Safe

To ensure client safety, the following prerequisites must be met. 

- **AMDP method declarations** must use one of the following additions and specify particular objects in the **`USING` list**:

    - `CDS SESSION CLIENT DEPENDENT`
      - Declares the AMDP method as client-dependent when it uses at least one client-dependent object.
      - Ensures client safety by specifying client-safe objects in the `USING` list that access data from the current client only.
      - Data sources that can be specified in the `USING` list:
        - Client-dependent CDS view entities filtered by the HANA session variable `CDS_CLIENT` (CDS session variable `$session.client`) 
        - Client-dependent DDIC database tables 
          - Note: AMDP methods with the addition `CDS SESSION CLIENT DEPENDENT` handle client safety automatically. A database view, that wraps the database table, is generated implicitly to access and select data for the client specified in the HANA session variable `CDS_CLIENT`, ensuring the data source is client-safe.          
        - Client-safe CDS table functions 
        - Other client-dependent or client-independent AMDP methods with the AMDP option `CDS SESSION CLIENT DEPENDENT` or `CLIENT INDEPENDENT` AMDP methods
        - Client-independent CDS objects and DDIC database tables        

    - `CLIENT INDEPENDENT`
      - Declares the AMDP method as client-independent when it uses only client-independent objects.
      - Client-independent data sources can be specified in the `USING` list: Client-independent CDS view entities, DDIC database tables, client-safe CDS table functions, AMDP methods (specifying `CLIENT INDEPENDENT`)


- **CDS table functions** must have one of these annotations for client safety:  
  - `@ClientHandling.type: CLIENT_DEPENDENT` (should be used together with the  `@ClientHandling.algorithm: #SESSION_VARIABLE` annotation)  
  - `@ClientHandling.type: #CLIENT_INDEPENDENT`  
  - Parameters should not be marked with `@Environment.systemField: #CLIENT`  
  - Note: The annotation `@ClientHandling.clientSafe` ensures client-dependent CDS table functions are client-safe by enforcing client-safety checks. In ABAP for Cloud Development, specifying this annotation is unnecessary as the checks are automatically enabled.

 - **SQLScript code** in implementations of AMDP methods
    - Must not access database objects not managed by ABAP or not included in the `USING` list.
    - Must not call built-in HANA functions that include a client ID parameter.
    - The client parameter from AMDP method signatures should be removed.  If removal is not possible, for example, due to compatibility reasons, ensure the client is excluded from the SQLScript code to prevent interference from external callers.

> [!NOTE]
> - Using pre-delivered repository objects in ABAP for Cloud Development:
>   - Use only pre-delivered AMDP methods with a [C4 contract](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENC4_CONTRACT_GLOSRY.html).
>   - Create and use your own wrapper CDS view entities for pre-delivered CDS view entities with a [C1 contract](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENC1_CONTRACT_GLOSRY.html).
> - More information: 
>   - The table in [this topic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENAMDP_CALL_MATRICES.html) shows an overview of specification options regarding client safety.
>   - [Client Handling in CDS Table Functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENCDS_FUNC_CLIENT_HANDLING.html)

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information 

- AMDP in ABAP for Cloud Development: [ABAP Keyword Documentation (ABAP for Cloud Development)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp.htm)
- AMDP in Standard ABAP: [ABAP Keyword Documentation (Standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenamdp.htm)
    > [!TIP]
    > Checking if AMDP is supported in classic ABAP: 
    >    ```abap
    >    IF NOT cl_abap_dbfeatures=>use_features(
    >        EXPORTING requested_features =
    >        VALUE #( ( cl_abap_dbfeatures=>call_amdp_method ) ) ).
    >
    >      "Result: Current database system does not support AMDP procedures
    >    RETURN.
    >    ENDIF.
    >    ```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

[zcl_demo_abap_amdp](./src/zcl_demo_abap_amdp.clas.abap)

> [!NOTE]
> - The executable example covers the following topics:
>   - AMDP procedures, calling AMDP procedures from SQLScript
>   - AMDP table functions for AMDP methods
>   - AMDP table functions for CDS table functions
> - The AMDP example for ABAP for Cloud Development is designed differently compared to the AMDP example for Standard ABAP. Instead of using demo database tables, CDS view entities are used in the `USING` list. Additionally, the client handling is adjusted for the AMDP methods by including appropriate additions in the AMDP method declaration part.
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)