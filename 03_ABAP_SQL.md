<a name="top"></a>

# ABAP SQL

- [ABAP SQL](#abap-sql)
  - [Introduction](#introduction)
  - [Excursion: Database Tables and Views](#excursion-database-tables-and-views)
  - [Read Operations Using SELECT](#read-operations-using-select)
    - [Basic Syntax](#basic-syntax)
      - [Notes on Modern SELECT Statements](#notes-on-modern-select-statements)
    - [Specifying the SELECT List or FIELDS Clause](#specifying-the-select-list-or-fields-clause)
    - [Specifying the FROM Clause](#specifying-the-from-clause)
    - [Specifying the Target and Related Additions](#specifying-the-target-and-related-additions)
    - [Specifying Clauses to Order, Group and Set Conditions for the Result Set](#specifying-clauses-to-order-group-and-set-conditions-for-the-result-set)
    - [Selecting Data by Evaluating the Content of Other Tables](#selecting-data-by-evaluating-the-content-of-other-tables)
    - [Combining Data of Multiple Data Sources](#combining-data-of-multiple-data-sources)
    - [Common Table Expressions (CTE)](#common-table-expressions-cte)
  - [SQL Conditions](#sql-conditions)
  - [SQL Operands](#sql-operands)
  - [SQL Expressions](#sql-expressions)
    - [Elementary SQL Expressions](#elementary-sql-expressions)
    - [Arithmetic Expressions](#arithmetic-expressions)
    - [Cast Expressions](#cast-expressions)
    - [String Expressions](#string-expressions)
    - [Case Expressions](#case-expressions)
    - [Aggregate Expressions](#aggregate-expressions)
    - [Window Expressions](#window-expressions)
    - [Null Expressions](#null-expressions)
  - [Built-In SQL Functions](#built-in-sql-functions)
    - [Numeric Functions](#numeric-functions)
    - [String Functions](#string-functions)
    - [coalesce Function](#coalesce-function)
    - [Conversion Functions](#conversion-functions)
    - [Date and Time Functions](#date-and-time-functions)
    - [UUID Function](#uuid-function)
  - [Create, Update, and Delete Operations](#create-update-and-delete-operations)
    - [INSERT](#insert)
    - [UPDATE](#update)
    - [MODIFY](#modify)
    - [DELETE](#delete)
    - [Using Constructor Expressions in ABAP SQL Statements](#using-constructor-expressions-in-abap-sql-statements)
    - [Example: Exploring Create, Update, and Delete Operations with ABAP SQL Statements](#example-exploring-create-update-and-delete-operations-with-abap-sql-statements)
  - [Dynamic ABAP SQL Statements](#dynamic-abap-sql-statements)
  - [CRUD Operations Using CDS Artifacts](#crud-operations-using-cds-artifacts)
  - [Excursions](#excursions)
    - [Evaluating ABAP System Fields after ABAP SQL Statements](#evaluating-abap-system-fields-after-abap-sql-statements)
    - [Typed Literals](#typed-literals)
    - [ABAP SQL and Client Handling](#abap-sql-and-client-handling)
    - [RAP-Specific ABAP SQL Variants](#rap-specific-abap-sql-variants)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)

## Introduction

-   [ABAP SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_glosry.htm) is a subset of [SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_glosry.htm "Glossary Entry")
    which is the standardized language for accessing databases.
-   The main ABAP SQL keywords to read and change data are the
    following:

    | Keyword  | Purpose                                                                   |
    | -------- | ------------------------------------------------------------------------- |
    | `SELECT` | Reads data from data sources such as database tables and others           |
    | `INSERT` | Adds rows to database tables                                              |
    | `UPDATE` | Changes the content of rows of database tables                            |
    | `MODIFY` | Inserts rows into database tables or changes the content of existing rows |
    | `DELETE` | Deletes rows from database tables                                         |

- ABAP SQL statements use the ABAP SQL interface. This interface transforms all ABAP SQL statements that access the standard database of an AS ABAP to  platform-dependent SQL and forwards the results to the database system.
- Generally bear in mind the [performance notes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_perfo.htm) when using
    ABAP SQL. The considerations there are not relevant for this cheat sheet since
    the focus is on syntax options.

> [!NOTE]
> - The syntax options for the `SELECT` statement are extensive. Make sure that you consult the ABAP Keyword Documentation for all available options. The cheat sheet and snippets demonstrate a selection.
> - The code examples in the cheat sheet primarily use DDIC database tables for [CRUD operations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencrud_glosry.htm). Note that there are also CDS artifacts that allow not only reading but also creating, updating, and deleting. See [this section](#crud-operations-using-cds-artifacts).


## Excursion: Database Tables and Views

<details>
  <summary>🟢 Click to expand for more details</summary>
<br>

This section provides bullet points on database tables and views which contain persisted data. Note that the code snippets in this cheat sheet focus on database tables as [data source](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_source_glosry.htm "Glossary Entry") for ABAP SQL statements.

**Database tables in [AS ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenas_abap_glosry.htm "Glossary Entry") ...**

- are objects of the [ABAP Dictionary (DDIC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_dictionary_glosry.htm "Glossary Entry"). The term *database table* describes a physical database table in the current [standard database](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_db_glosry.htm).
- are two-dimensional matrices consisting of rows and columns.
- contain a [table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_key_glosry.htm), i. e. a field or a combination of fields uniquely identifies every row in a table. A [primary key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprimary_key_glosry.htm) must exist for every database table.
  - Note the concept of [foreign keys](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenforeign_key_glosry.htm) in which one or more columns of a database table can be primary keys of another table. See more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_database_tables_forkey.htm).
- have a non-[nested structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennested_structure_glosry.htm)
    type. Plus, the definition of database tables consists of [technical](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_database_tables_techstruc.htm) and [semantic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_database_tables_semastruc.htm) properties.
- can be referenced as a data type and can be accessed using ABAP SQL.

Find more information in the respective (sub)topics in the ABAP Keyword Documentation [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_database_tables.htm).


**Views ...**

-   are further ABAP Dictionary objects for grouping columns from one or more database tables, among others.
-   usually realize a
    [join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenjoin_glosry.htm "Glossary Entry")
    with defined [join
    conditions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenjoin_condition_glosry.htm "Glossary Entry").
-   Note:
    -   Similar to database tables, the columns of such a view form a
        flat structure. The view's name can be used, for example, as [data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_type_glosry.htm) to declare
        [data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm), too.
    -   The views can be accessed by ABAP SQL, especially for reading
        purposes using `SELECT`.

**"Classic"** [DDIC
Views](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenddic_view_glosry.htm "Glossary Entry") ...

- are the oldest form of views and are not available in [ABAP Cloud](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_cloud_glosry.htm).
- can be accessed by ABAP SQL for read and write operations, however, writing is only supported if the view is created with only one database table.
- can only be created in the [ABAP Workbench](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_workbench_glosry.htm).

**"Modern" Views (since release 7.40)**

-   [External
    views](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexternal_view_glosry.htm "Glossary Entry")
    as proxies for [SAP HANA
    views](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenhana_view_glosry.htm "Glossary Entry")
    (attribute view, analytic view, calculation view)
    -   SAP HANA Views are entities of the SAP HANA database that are
        defined using the [SAP HANA
        Studio](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenhana_studio_glosry.htm "Glossary Entry").
    -   They are based on HANA-specific data types.
    -   Using external views of the ABAP dictionary, you can make those
        SAP HANA views "known" to the ABAP program. In doing so, the
        external views can be used like classic DDIC views as structured
        data types and as a source for reading operations with ABAP SQL.
    -   To be used only if the central database of the AS ABAP is an
        [SAP HANA
        database](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhana_database_glosry.htm "Glossary Entry").
-   [ABAP Core Data Services (ABAP
    CDS)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_view_glosry.htm "Glossary Entry")
    ...
    -   serve the purpose of defining semantically rich data models.
    -   have a lot more options than classic views, for example, they
        support [annotations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_annotation_glosry.htm). Data sources can be combined using [associations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_association_glosry.htm), views can be defined with input parameters, and more.
    -   are used like a classic database view as structured data types
        and used as a source for reading operations with ABAP SQL (using
        [`SELECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect.htm)).
    -   are created using [Data Definition
        Language (DDL)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddl_glosry.htm "Glossary Entry")
        in the
        [ADT](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenadt_glosry.htm "Glossary Entry")
        (that is, a source code editor, in contrast to a form-based
        editor).
    -   are, in contrast to external views, supported by all database
        systems (that support the ABAP CDS characteristics).
    -   Note that there are also CDS artifacts that allow not only reading but also creating, updating, and deleting. See [this section](#crud-operations-using-cds-artifacts).
</details>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Read Operations Using SELECT

### Basic Syntax

- You use ABAP SQL `SELECT` statements to retrieve data from one or more data sources (such as database tables, views or internal tables). Note that many syntax examples in this cheat sheet show a selection from `dbtab` denoting a database table as a source. 
- This can be done to create a multirow or single row result set by assigning the result set to a suitable data object, i. e. you can store the multirow read result in an internal table or the single row result in a structure.
- The `SELECT` statement includes several clauses that serve different purposes. 
- The following code snippet shows the basic syntax and mandatory clauses to be specified. Although its use is optional, a `WHERE` clause should be specified to further restrict the read result.
- However, there are plenty of additions and syntax variants, some of which are mentioned in the cheat sheet. In general, choose `F1` for the keywords and additions to get all the details in the ABAP Keyword Documentation.

``` abap
SELECT FROM source   "What data source to read from
  FIELDS field_list  "What columns should be read
  WHERE condition    "Specifies conditions on which a row/rows should be read
  INTO target.       "Specifies the target of the result set
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Notes on Modern SELECT Statements

- Especially in [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm), in older ABAP programs, etc. you may stumble on variants of the `SELECT` syntax (i.e. differently arranged or differently specified ABAP SQL syntax) that you should no longer use (strict syntax check modes enforce the use of specific ABAP SQL syntax) and/or are not possible in [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm). 
  - However, in certain cases, older syntax variants are still valid and usable. An example is the `SELECT` list, that can also be specified using the more modern `FIELDS` addition.
  - As mentioned, choose `F1` for the keywords and additions to get all the details in the ABAP Keyword Documentation.
  - Further information in the context of Standard ABAP: [Release-Dependent Syntax Check Modes (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_sql_strict_modes.htm).
- To mention some examples of modern ABAP SQL statements.
  - The `INTO` clause should be placed after the other clauses. 
  - [Host variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhost_variable_glosry.htm "Glossary Entry") or [host expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhost_expression_glosry.htm "Glossary Entry") are required for data objects and expressions, i. e. they must be preceded by `@` or `@( ... )`. Host variables represent data objects that are declared in ABAP programs. They are specified in an [operand position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm) of an ABAP SQL statement. Also see the [SQL Operands](#sql-operands) section.
  - The `SELECT` list, i. e. the fields that are specified, can also be specified following the `SELECT` keyword before the `FROM` clause - without `FIELDS`. The following two `SELECT`   statements are basically the same but differently arranged. The code snippets in the cheat sheet randomly use one syntax or the other. 
    ``` abap
    SELECT FROM dbtab
      FIELDS comp1, comp2, comp3
      ...

    SELECT comp1, comp2, comp3
      FROM dbtab
      ...
    ```
  - Regarding the target into which data is read: Instead of using a variable that is (extra) declared beforehand, you can also make use of [inline declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry"), for example `... INTO TABLE @DATA(itab).`, to comfortably create an appropriate variable in place. Note that in case of internal tables as targets, the resulting table is a standard table and has an empty key which might have an impact when further
    processing the internal table entries. Find more information in the ABAP cheat sheet [Internal Tables](01_Internal_Tables.md). The declaration operator [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm) can be used to declare immutable variables.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Specifying the SELECT List or FIELDS Clause  

- Using the `SELECT` list or `FIELDS` clause, you define the structure of the result set.
- You can do this by specifying the columns to be read from the data source individually, or by specifying `*` to read all columns.
- Syntax variants are possible.
- Note that you can specify the addition `SINGLE` after `SELECT`. With `SINGLE`, it means the result set is a single row result set. Otherwise, it is a multirow result set, or a `SELECT` loop is opened. An appropriate data object must be specified in the `INTO` clause.

| Syntax   |      Notes      |
|----------|-------------|
| `SELECT * ...` <br><br> `SELECT ... FIELDS * ...` | As outlined above, the `*` character defines all columns to be read from a data source (in the order specified there).  |
| `SELECT col1, col2, col3 ...` <br><br> `SELECT ... FIELDS col1, col2, col3 ...` | A comma-separated list of individual column names.   |
| `SELECT data_source~col1, data_source~col2, data_source~col3 ...`  <br><br> `SELECT ... FIELDS data_source~col1, data_source~col2, data_source~col3 ...` | A comma-separated list of individual column names. Here, the name of the data source is explicitly specified and precedes the column name, separated by a tilde. |
| `SELECT data_source~* ...` <br><br> `SELECT ... FIELDS data_source~* ...` | In this case, the name of the data source is followed by a tilde and the `*` character to specify all columns. Note that there are [special conditions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_list.htm#!ABAP_VARIANT_1@1@) when using this variant. |
| `SELECT col1 AS al1, col2 AS al2, col3 AS al3 ...` <br><br> `SELECT ... FIELDS col1 AS al1, col2 AS al2, col3 AS al3 ...` | Defining alias names for individual columns of the result set with [`AS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_list.htm); make sure that you use an alias name only once here. In the statement, the alias name can only be used after an `ORDER BY` clause; as shown further down, in some cases (e. g. when using SQL expressions) the specification of an alias name is required; setting an alias name for the data source is also possible (`SELECT FROM dbtab AS alias_name ...`). See the section on joins further down. |

> [!NOTE]
> - You have plenty of options regarding the specification of the columns in the `SELECT` list, among them, the outlined direct specification of the column name. SQL expressions can be specified, too. See more details [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_clause_col_spec.htm) and in the sections on SQL expressions further down.


``` abap
"All fields
SELECT * FROM dbtab
  WHERE ...
  INTO ...

"Comma-separated list
SELECT col1, col2, col3
  FROM dbtab
  WHERE ...
  INTO ...

"Comma-separated list, data source explicitly specified
SELECT dbtab~col1, dbtab~col2, col3
  FROM dbtab
  WHERE ...
  INTO ...

"Data source explicitly specified, all fields
SELECT dbtab~*
  FROM dbtab
  WHERE ...
  INTO ...

"Alias names
"Consider the following: You want to read data from a database table into a target data
"object but, for example, a name in the target is different. Provided that there will
"not be an issue regarding the type (conversion) when the values are assigned, you might
"specify an alias name for the database column to match a component's name in the target data object.
SELECT comp1 AS comp_a, comp2 AS comp_b, comp3 AS comp_c
  FROM dbtab
  WHERE ...
  INTO CORRESPONDING FIELDS OF TABLE @itab.

"Alias name also possible for the data source
SELECT ds~col1, ds~col2, ds~col3
  FROM dbtab AS ds
  WHERE ...
  INTO ...
```

Example code using a demo database table from the cheat sheet repository, and using the `FIELDS` addition. 

```abap
"The examples do not specify a WHERE clause.

"All fields
SELECT FROM zdemo_abap_carr
  FIELDS *
  INTO TABLE @DATA(itab).

"Comma-separated list
SELECT FROM zdemo_abap_carr
  FIELDS carrid, carrname, url
  INTO TABLE @itab.

"Comma-separated list, data source explicitly specified
SELECT FROM zdemo_abap_carr
  FIELDS zdemo_abap_carr~carrid, zdemo_abap_carr~carrname, url
  INTO TABLE @itab.

"Data source explicitly specified, all fields
SELECT FROM zdemo_abap_carr
  FIELDS zdemo_abap_carr~*
  INTO TABLE @itab.

"Alias names    
SELECT FROM zdemo_abap_carr
  FIELDS carrid AS comp_a, carrname AS comp_b, url AS comp_c
  INTO TABLE @DATA(itab_comp_alias).

"Alias name also possible for the data source
SELECT FROM zdemo_abap_carr AS ds
  FIELDS ds~carrid, ds~carrname, ds~url
  INTO TABLE @DATA(itab_db_alias).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Specifying the FROM Clause 

- The `FROM` clause specifies data sources from which data is read. 
- The data sources include: 
  - DDIC database tables (which are used as data source in most of the code snippets)
  - CDS entities such as [CDS view entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_v2_view_glosry.htm) and [CDS table functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_table_function_glosry.htm) 
    - The executable example of the [CDS View Entities](15_CDS_View_Entities.md) cheat sheet covers `SELECT` statements with CDS view entities as data sources. 
  - Internal tables
    - Note that the internal table must be specified as host variable prefixed by `@`, and an alias name must be specified. 
    - More information and code snippets: Section [SELECT Queries with Internal Tables as Data Sources](01_Internal_Tables.md#select-queries-with-internal-tables-as-data-sources) in the *Internal Tables* cheat sheet and in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_itab.htm)
  

``` abap
SELECT *
  FROM dbtab
  WHERE ...
  INTO ....

SELECT FROM @itab1 AS tab
  FIELDS ...
  WHERE ...
  INTO ....

SELECT *
  FROM cds_view
  WHERE ...
  INTO ....
```

Example using cheat sheet repository objects, a demo internal table, and the `FIELDS` addition:

``` abap
"Database table
SELECT FROM zdemo_abap_fli
  FIELDS *
  INTO TABLE @DATA(itab_db).

"CDS entities
"CDS view entity
SELECT *
  FROM zdemo_abap_fli_ve
  INTO TABLE @DATA(itab_ve).

"CDS table function
SELECT FROM zdemo_abap_table_function
  FIELDS *
  INTO TABLE @DATA(itab_tf).

"Internal table
TYPES: BEGIN OF s,
          num   TYPE i,
          chars TYPE c LENGTH 3,
        END OF s,
        t_type TYPE TABLE OF s WITH EMPTY KEY.

DATA(itab) = VALUE t_type( ( num = 1 chars = 'aaa' )
                           ( num = 2 chars = 'bbb' )
                           ( num = 3 chars = 'ccc' ) ).

SELECT FROM @itab AS it
  FIELDS *
  INTO TABLE @DATA(itab_it).
```


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Specifying the Target and Related Additions

This section demonstrates a selection of ABAP SQL `SELECT` statement patterns for specifying target-related additions to handle query results. There is a wide range of options available, some of which have already been covered above. 

Syntax options regarding the target:

<table>
<tr>
<td> Syntax </td> <td> Notes </td>
</tr>

<tr>
<td> 

`... INTO TABLE ...`
</td>
<td>

An internal table is expected for the result set. It may be declared inline to automatically have a suitable type, which also applies to  `... INTO ...`.

```abap
SELECT FROM dbtab
  FIELDS *
  WHERE ...
  INTO TABLE @itab.

SELECT FROM dbtab
  FIELDS *
  WHERE ...
  INTO TABLE @DATA(itab_inl).
```

</td>
</tr>

<tr>
<td> 

`... INTO ...`
</td>
<td>

Expects a structure when used without `TABLE`.

```abap
SELECT SINGLE comp1, comp2, comp3       
  FROM dbtab
  WHERE ...
  INTO @struc.  

SELECT SINGLE comp1, comp2, comp3       
  FROM dbtab
  WHERE ...
  INTO @DATA(struc_inl).    
```

</td>
</tr>

<tr>
<td> 

`... INTO CORRESPONDING FIELDS OF [TABLE] ...`
</td>
<td>

- Only the content of columns for which there are identically named components in the target are assigned. 
- However, if you want to read data into an existing data object and particular fields are specified in the `SELECT` list or `FIELDS` clause, and if the addition is **not** specified, you might stumble on undesired results. 
- The target data object must contain enough components and the content of the columns are assigned to the components of the target from left to right in the order specified. The content of surplus components of the target is not changed. 
- Plus, pay attention to [assignment rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_into_conversion.htm). 
- Basic rule: Without `CORRESPONDING ...`, column names do not play a role but only the position. With `CORRESPONDING ...`, the position of the columns does not play a role but only the name.

```abap
SELECT SINGLE comp1, comp2, comp3       
  FROM dbtab
  WHERE ...
  INTO CORRESPONDING FIELDS OF @struc.  

SELECT FROM dbtab
  FIELDS comp1, comp2, comp3               
  WHERE ...
  INTO CORRESPONDING FIELDS OF TABLE @itab.
```

</td>
</tr>

<tr>
<td> 

`... APPENDING [CORRESPONDING FIELDS OF] TABLE ...`
</td>
<td>

The addition `INTO` initializes the target object. When using the addition `APPENDING`, you can retain existing lines in internal tables. `APPENDING` is also possible with the addition `CORRESPONDING FIELDS OF`.

```abap
SELECT * FROM dbtab
  WHERE ...
  APPENDING TABLE @itab.

"APPENDING is also possible with the addition CORRESPONDING FIELDS OF
SELECT * FROM dbtab
  WHERE ...
  APPENDING CORRESPONDING FIELDS OF TABLE @diff_itab.
```

</td>
</tr>

<tr>
<td> <code>NEW</code> addition: Specifying an anonymous data object as target object </td>
<td>

- Specifying an [anonymous data object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm) as target object using the addition [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_into_target.htm#!ABAP_ALTERNATIVE_3@3@) 
- Only to be used after `INTO` and not `APPENDING`.

<br>

``` abap
"The examples declares target objects as anonymous data objects inline.
SELECT *
  FROM zdemo_abap_flsch
  INTO TABLE NEW @DATA(itab_ref).

SELECT SINGLE *
  FROM zdemo_abap_flsch
  INTO NEW @DATA(wa_ref).
```

</td>
</tr>


<tr>
<td> 

`... INTO ( dobj1, dob2, ... ) ...`
</td>
<td>

Apart from reading into structures and internal tables outlined above, you can also read into individual elementary data objects.
Here, the individual elementary data objects as target objects are specified in a comma-separated list (e. g. as existing host variables or declared inline) and put between a pair of parentheses.

Note:
- The comma-separated list must have the same number of elements as columns in the result set.
- The content of the columns in the result set is assigned to the data objects specified in the list from left to right in accordance with the order specified in the `SELECT` list.
- Note the [assignment rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_into_conversion.htm) also in this context.
- More information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinto_clause.htm#!ABAP_ALTERNATIVE_1@1@).

``` abap
"Elementary data objects as target data objects 
DATA id TYPE zdemo_abap_carr-carrid.
DATA name TYPE zdemo_abap_carr-carrname.

SELECT SINGLE FROM zdemo_abap_carr
  FIELDS carrid, carrname
  WHERE carrid = char`LH`
  INTO ( @id, @name ).

"Inline declarations with DATA/FINAL and the
"creation of anonymous data objects with NEW are
"possible
SELECT SINGLE FROM zdemo_abap_carr
  FIELDS carrid, carrname, url
  WHERE carrid = char`LH`
  INTO ( @id, @DATA(name2), NEW @FINAL(dref) ).
```


</td>
</tr>


</table>


More target-related additions: 

<table>
<tr>
<td> Subject </td> <td> Details/Code Snippet </td>
</tr>


<tr>
<td> 

`SINGLE` addition: Retrieving a single row into a structure </td>
<td>

``` abap
"SINGLE addition
"Here, all fields of a single row a read. Specifying an
"asterisk * indicates that all fields are to be read.
"Alternatively, you can list all the fields separated by comma.
"Note that if the selection covers more than one row, e. g. in case
"of a non-unique WHERE clause, one of these rows is included in
"the result.
SELECT SINGLE FROM dbtab
  FIELDS *
  WHERE ...
  INTO @struc.        "Existing structure of dbtab's row type

"Retrieving a selected set of fields of a single row
SELECT SINGLE FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO @DATA(struc2).    "Structure declared inline

"Alternative syntax without the FIELDS addition
"Here, the CORRESPONDING FIELDS OF addition is used. Only the content of
"columns that have identically named components in the target data object
"is assigned.
SELECT SINGLE comp1, comp2, comp3       "Selected set of fields
  FROM dbtab
  WHERE ...
  INTO CORRESPONDING FIELDS OF @struc.  "Existing structure
``` 

</td>
</tr>
<tr>
<td> Retrieving multiple rows into an internal table </td>
<td>

``` abap
SELECT FROM dbtab
  FIELDS *              "All fields
  WHERE ...
  INTO TABLE @itab.     "itab has an appropriate row type

"Alternative syntax without the FIELDS addition
SELECT comp1, comp2, comp3          "Selected set of fields
  FROM dbtab
  WHERE ...
  INTO TABLE @DATA(lv_itab).        "Internal table declared inline

"Selected set of fields, existing variable
"See the note on CORRESPONDING FIELDS OF above
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3                "Selected set of fields
  WHERE ...
  INTO CORRESPONDING FIELDS OF TABLE @itab.

"The addition INTO initializes the target object. When using the addition APPENDING, 
"you can retain existing lines in internal tables. 
SELECT * FROM dbtab
  WHERE ...
  APPENDING TABLE @itab.

"APPENDING is also possible with the addition CORRESPONDING FIELDS OF
SELECT * FROM dbtab
  WHERE ...
  APPENDING CORRESPONDING FIELDS OF TABLE @diff_itab.
```

</td>
</tr>

<tr>
<td> <code>SELECT</code> loop: Sequentially retrieving multiple rows </td>
<td>

- A `SELECT` loop can be opened if the assignment is made to a structure and the addition `SINGLE` is not used.
- If the row is found, the system field `sy-subrc` is set to `0`.
- The loop must be closed using `ENDSELECT`.
- To terminate the loop completely, you can use the statement [`EXIT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapexit_loop.htm).
- Note: As covered further down, when using the addition `PACKAGE SIZE` and storing the result in a table, a loop is opened, too.

<br>

``` abap
SELECT FROM dbtab
  FIELDS *
  WHERE ...
  INTO @struc.

    IF sy-subrc = 0.
      ...  "For example, making changes on data and adding the row to an internal table.

    ENDIF.

ENDSELECT.
```

</td>
</tr>
<tr>
<td> Checking the existence of a row in a database table </td>
<td>


``` abap
"The example uses @abap_true. Other specifications are possible, e.g. 'X'.
SELECT SINGLE @abap_true
  FROM dbtab
  WHERE ...
  INTO @DATA(exists).

IF exists = abap_true.
  ...
ENDIF.
```

</td>
</tr>
<tr>
<td> <code>DISTINCT</code> addition: Removing rows that occur more than once in a multirow result set   </td>
<td>

- Cannot be used with the addition `SINGLE`.
- See more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_clause.htm).

<br>

``` abap
"The following example assumes that there are multiple flights
"from a city of the specified carrier with the particular destination.
"Assume there are 5 flights available from Frankfurt (cityfrom) that 
"matches the WHERE clause. The first statement only returns one entry
"in the internal table, the second 5 (all).

"Using the DISTINCT addition
SELECT DISTINCT cityfrom
  FROM zdemo_abap_flsch
  WHERE carrid = 'LH' AND
        cityto = 'NEW YORK'
  INTO TABLE @DATA(itab_distinct).

"Not using the DISTINCT addition
SELECT cityfrom
  FROM zdemo_abap_flsch
  WHERE carrid = 'LH' AND
        cityto = 'NEW YORK'
  INTO TABLE @DATA(itab_no_distinct).
```

</td>
</tr>


<tr>
<td> <code>UP TO n ROWS</code> addition: Limiting the number of returned table rows </td>
<td>

[`UP TO n ROWS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_up_to_offset.htm)

<br>

``` abap
"A maximum of five rows are to be returned
"If the INTO clause is the last clause, the UP TO clause must be positioned after it.
SELECT * FROM dbtab
  WHERE ...
  INTO TABLE @DATA(itab_upto)
  UP TO 5 ROWS.
```

</td>
</tr>

<tr>
<td> <code>OFFSET n</code> addition: Returning only the table rows after a row with a specified count from the result set</td>
<td>

- [`OFFSET n`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_up_to_offset.htm#!ABAP_ADDITION_2@2@) 
- You can only use the addition, if an `ORDER BY` clause is specified.

<br>

``` abap
"In the example, data of all flights are retrieved, except for the 2 flights
"with the shortest flight time.
SELECT *
  FROM ztest_abap_flsch
  WHERE carrid = 'LH'
  ORDER BY fltime ASCENDING
  INTO TABLE @DATA(itab)
  OFFSET 2.
```

</td>
</tr>

<tr>
<td> <code>PACKAGE SIZE n</code> addition: Storing the result in packages of a specified number of rows </td>
<td>


The addition [`PACKAGE SIZE n`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinto_clause.htm#!ABAP_ONE_ADD@1@) can be specified after `INTO TABLE` and `APPENDING TABLE`. A `SELECT` loop ist opened. After `PACKAGE SIZE`, the number of rows is specified (which can be a host variable, host expression or a literal of type `i`) denoting the number of rows to be inserted in the target object per iteration.

<br>

``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO TABLE @DATA(itab_pack) PACKAGE SIZE n.
...
ENDSELECT.
```

</td>
</tr>



<tr>
<td> <code>INDICATORS [NOT] NULL STRUCTURE</code> addition: Specifying null indicators </td>
<td>

- The `INDICATORS ...` addition is used to specify indicators such as the [null indicator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennull_indicator_glosry.htm) and store information about which columns of the result set contain the null value and which do not.
- In the example, an appropriate target table is defined to also store information about which columns of the result set contain the null value and which do not.
- More syntax options are available for `INDICATORS ...`. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_indicators.htm).

<br>

``` abap
"The following example uses a left outer join to intentionally create null values. For
"this purpose, two demo database tables of the cheat sheet repository are cleared and
"populated with specific values to visualize null values.
DELETE FROM zdemo_abap_tab1.
DELETE FROM zdemo_abap_tab2.
MODIFY zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 char1 = 'a' char2 = 'y' )
                                              ( key_field = 2 char1 = 'b' char2 = 'z' ) ) ).
MODIFY zdemo_abap_tab2 FROM TABLE @( VALUE #( ( key_field = 1 char1 = 'a' )
                                              ( key_field = 2 char1 = 'a' )
                                              ( key_field = 3 char1 = 'b' )
                                              ( key_field = 4 ) ) ).

"Note that for the entry 'key_field = 4' no char1 value was passed.
"char1 is a shared column of the two database tables, and which is used in
"the ON condition of the join. Since there is no entry in char1 for 'key_field = 4',
"the joined values are null in that case.
"The example visualizes the null values. The INDICATORS addition is used to specify
"indicators such as the null indicator. In the example, an appropriate target table
"is defined to also store information about which columns of the result set contain
"the null value and which do not.
TYPES: BEGIN OF st4null,
         BEGIN OF s2,
           key_field TYPE zdemo_abap_tab2-key_field,
           char2     TYPE zdemo_abap_tab1-char2,
         END OF s2,
         BEGIN OF nulls,
           key_field TYPE c LENGTH 1,
           char2     TYPE c LENGTH 1,
         END OF nulls,
       END OF st4null.
DATA joined_tab_w_null_ind TYPE TABLE OF st4null WITH EMPTY KEY.

SELECT tab2~key_field, tab1~char2
  FROM zdemo_abap_tab2 AS tab2
  LEFT OUTER JOIN zdemo_abap_tab1 AS tab1 ON tab1~char1 = tab2~char1
  INTO TABLE @joined_tab_w_null_ind INDICATORS NULL STRUCTURE nulls.

*Internal table content:  
*S2                      NULLS
*KEY_FIELD    CHAR2      KEY_FIELD    CHAR2
*1            y
*KEY_FIELD    CHAR2      KEY_FIELD    CHAR2
*2            y
*KEY_FIELD    CHAR2      KEY_FIELD    CHAR2
*3            z
*KEY_FIELD    CHAR2      KEY_FIELD    CHAR2
*4                                    X
``` 

</td>
</tr>


</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Specifying Clauses to Order, Group and Set Conditions for the Result Set

<table>
<tr>
<td> Clause </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>GROUP BY</code> </td>
<td>

[`GROUP BY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapgroupby_clause.htm)
clause: Combining groups of table rows in the result set. You
can also use [SQL expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_expression_glosry.htm "Glossary Entry")
here. Multiple clause elements are separated by a comma. Find more information and syntax options in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapgroupby_clause.htm).

Note that the `GROUP BY` clause requires all columns that are
directly specified in the `SELECT` list or specified there as an
argument of an SQL expression to be specified. An exception to this is
[aggregate
functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenaggregate_function_glosry.htm "Glossary Entry")
in [aggregate
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenaggregate_expression_glosry.htm "Glossary Entry")
(except [grouping
functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengrouping_glosry.htm "Glossary Entry"))
as shown in the following example.

In the example below, the database table rows that have the same content in column `comp1` are combined. The lowest and highest values in column `comp2` are determined for each of these groups and placed into the combined row.

<br>

``` abap
SELECT FROM dbtab
  FIELDS comp1, MIN( comp2 ) AS min, MAX( comp2 ) AS max
  WHERE ...
  GROUP BY comp1
  INTO ...
```

</td>
</tr>
<tr>
<td> <code>HAVING</code> </td>
<td>


[`HAVING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaphaving_clause.htm)
clause: Limiting the number of table rows in groups in the
result by setting conditions on these rows. The rows for which a
logical expression is true are inserted in the target variable. Note
that `HAVING` can only be used together with `GROUP BY`.

<br>

``` abap
SELECT FROM dbtab
  FIELDS comp1, MIN( comp2 ) AS min, MAX( comp3 ) AS max
  WHERE ...
  GROUP BY comp1
  HAVING comp1 LIKE '%XYZ%' AND SUM( comp4 ) > 100
  INTO ...
```

</td>
</tr>

<tr>
<td> <code>ORDER BY</code> </td>
<td>

[`ORDER BY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaporderby_clause.htm)
clause: Sorting the result set by specified columns.

The following example shows the ordering of the result set based on the
content of the primary key of the [data source](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_source_glosry.htm "Glossary Entry").
You can also order by any columns and by explicitly specifying the sort order. There are more ordering options, for example, by using SQL expressions. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaporderby_clause.htm).

> [!NOTE]
>- Not specifying `ORDER BY` means that the order of entries in the result set is undefined.
>- If `ORDER BY` and `GROUP BY` clauses are used, all columns specified after `ORDER BY` must also be specified after `GROUP BY`.
>- If aggregate functions are specified after `SELECT`, all columns that are specified after `ORDER BY` and that do not have an alias name for an aggregate function must also be specified after `SELECT` and after the `GROUP BY` clause which is required in this case, too.


<br>

``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  ORDER BY PRIMARY KEY
           "comp2 ASCENDING
           "comp2 DESCENDING
  INTO ...
```
</td>
</tr>
<tr>
<td> <code>WHERE</code> </td>
<td>

[`WHERE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapwhere.htm) clause: Restricts the number of rows that are included in the result set using logical expressions. See further information on them in the following sections.
 
<br>

``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE comp1 = 'abc'
    AND comp2 < 123    
  INTO ...
```

</td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Selecting Data by Evaluating the Content of Other Tables

[`FOR ALL ENTRIES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_all_entries.htm)
addition:
- Components of an internal table can be used in the `WHERE` clause in logical expressions for comparisons with a column of the data source.
- The logical expression is evaluated for each individual row of the internal table.
- The result set of the `SELECT` statement is the union set of the result sets produced by the individual evaluations. Rows that occur more than once are removed from the result set automatically. The entire content of a row is respected.
- If `FOR ALL ENTRIES` is specified, there must be at least one comparison with a column of the internal table.
- For more information, especially restricitions and things to pay attention to (e. g. making sure that the internal table is not initial), see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_all_entries.htm). In modern ABAP, it is advisable to use internal tables as data sources in the `SELECT` statement, for example, in `WHERE` clauses or joins, instead of using the `FOR ALL ENTRIES` addition.

``` abap
"Checking that table is not initial
IF ( 0 < lines( itab2 ) ).

  SELECT comp1, comp2, comp3
    FROM dbtab
    FOR ALL ENTRIES IN @itab2      "Host variable before internal table
    WHERE comp1 = @itab2-comp1 ... "Relational expression on the right side of a comparison
    INTO TABLE @itab1

ENDIF.
```

**Checking the result set of a subquery** with the addition [`EXISTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_logexp_exists.htm)

See possible clauses and additions of a [subquery](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubquery_glosry.htm) in a condition in ABAP SQL [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_logexp_subquery.htm).

The following code snippet includes a parenthesized subquery following `EXISTS`. Data is only selected from `dbtab1` if the relational expression (`WHERE EXISTS ...`) is true, i. e. if the result set of the subquery contains at least one row. Note the components of the table that are referenced using a tilde.

``` abap
SELECT comp1, comp2, comp3
  FROM dbtab1 AS tab1
  WHERE EXISTS
    ( SELECT comp1 FROM dbtab2
      WHERE comp1 = tab1~comp1 AND comp2 = tab1~comp2 )
  INTO ...
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Combining Data of Multiple Data Sources

<table>
<tr>
<td> Subject </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> Using an inner join </td>
<td>

[Inner join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninner_join_glosry.htm):
- Columns of two or more data sources in a result set can be joined.
- Result set:
  - Columns of the rows in the result set of the left side with the columns of the rows in the result set of the right side are joined into a single result set.
  - Contains all combinations of rows for whose columns the join condition is true.
- If there are identical column names in multiple data sources, use the [column
selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_comp_selector_glosry.htm "Glossary Entry")
`~`.

<br>

``` abap
SELECT a~comp1, a~comp2, b~comp3, c~comp4
  FROM dbtab1 AS a
  INNER JOIN dbtab2 AS b ON a~comp1 = b~comp1 AND a~comp2 = b~comp2
  INNER JOIN dbtab3 AS c ON a~comp1 = c~comp1
  WHERE ...
  INTO ...
```

</td>
</tr>

<tr>
<td> Using an outer join </td>
<td>

[Outer join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenouter_join_glosry.htm):
- Realized by either a [left outer join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenleft_outer_join_glosry.htm) or
a [right outer join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenright_outer_join_glosry.htm).
- Result set:
  - Same result set as the inner join.
  - Difference: For each selected row on the left side as `LEFT OUTER JOIN` or on the right side as `RIGHT OUTER JOIN`, at least one row is created in the result set even if no rows on the other side meet the condition. The columns on the other side that do not meet the condition are filled with [null values](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennull_value_glosry.htm).

<br>

``` abap
"Example for a left outer join
SELECT a~comp1, a~comp2, b~comp3,
  FROM dbtab1 AS a
  LEFT OUTER JOIN dbtab2 AS b ON a~comp1 = b~comp1
  WHERE ...
  INTO ...
```

</td>
</tr>

<tr>
<td> Merging the result sets of multiple queries into a single result set </td>
<td>

... using the [set operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_set_operators_glosry.htm) [`UNION`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapunion.htm#!ABAP_VARIANT_1@1@). In this case, the rows of the result set of the query after `UNION` are inserted into the result set of the query in front of `UNION`.

<br>

``` abap
SELECT FROM dbtab1
  FIELDS ...
  WHERE ...
UNION
SELECT FROM dbtab2
  FIELDS ...
  WHERE ...
  INTO ...
```

</td>
</tr>

<tr>
<td> Returning distinct rows of a result set (1) </td>
<td>

... of a query specified before the `INTERSECT` addition that are also available in the result set of the query after the `INTERSECT` addition.

<br>

``` abap
"If you have imported the cheat sheet repository and already run an example class to fill
"the demo tables, you can check the contents of the result sets.
SELECT zdemo_abap_flsch~carrid, zdemo_abap_carr~carrname
    FROM zdemo_abap_flsch
    INNER JOIN zdemo_abap_carr ON zdemo_abap_carr~carrid = zdemo_abap_flsch~carrid
    ORDER BY zdemo_abap_flsch~carrid
    INTO TABLE @DATA(itab_no_intersect).

"Using INTERSECT; the result set contains distinct rows
SELECT zdemo_abap_flsch~carrid, zdemo_abap_carr~carrname
    FROM zdemo_abap_flsch
    INNER JOIN zdemo_abap_carr ON zdemo_abap_carr~carrid = zdemo_abap_flsch~carrid
INTERSECT
SELECT carrid, carrname
    FROM zdemo_abap_carr
    ORDER BY carrid
    INTO TABLE @DATA(itab_w_intersect).
```

</td>
</tr>

<tr>
<td> Returning distinct rows of a result set (2) </td>
<td>

... of a query specified before the `EXCEPT` addition that are not available in the result set of the query after the `EXCEPT` addition.

<br>

```abap
"If you have imported the cheat sheet repository and already run an example class to fill
"the demo tables, you can check the contents of the result sets.

"Selecting all carrier IDs from a database table that do not exist in an 
"internal table
TYPES: ty_demo_tab TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY.
DATA(itab) = VALUE ty_demo_tab( ( carrid = 'LH' ) ( carrid = 'LH' ) ( carrid = 'LH' ) 
                                ( carrid = 'AA' ) ( carrid = 'AA' ) ).


"Selecting all carrier IDs for comparison
SELECT carrid
    FROM zdemo_abap_carr
    INTO TABLE @DATA(all_carrids).

"Using EXCEPT; the result set excludes those carrier IDs present in the
"internal table
SELECT carrid
    FROM zdemo_abap_carr
    EXCEPT
        SELECT it~carrid
        FROM @itab AS it
            INNER JOIN zdemo_abap_carr ON zdemo_abap_carr~carrid = it~carrid
    ORDER BY carrid ASCENDING
    INTO TABLE @DATA(itab_w_except).
```

</td>
</tr>

</table>

> [!NOTE]
> - There are more join variants and syntax options available. See the ABAP Keyword Documentation on [joins](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_join.htm)
for more information.
> - There a more syntax options and contexts for `UNION`, `INTERSECT`, and `EXCEPT`. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapunion.htm).


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Common Table Expressions (CTE)

When to use [Common Table Expressions (CTE)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencommon_table_expression_glosry.htm):

-   Whenever you need intermediate results in a `SELECT`
    statement and especially if you need them more than once.
-   You get the option of selecting directly from a subquery (`SELECT FROM subquery`), which is not possible in ABAP SQL.

How they work:

-   The ABAP SQL keyword
    [`WITH`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapwith.htm)
    introduces the definition of CTEs.
-   Each CTE creates a tabular result set in a
    [subquery](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubquery_glosry.htm "Glossary Entry").
-   The result set of such a CTE can then be used in subsequent queries
    as data source; CTEs can be considered as temporary views, which
    only exist for the duration of the database access.
-   The CTEs (at least one) are then used in a final [main
    query](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmainquery_glosry.htm "Glossary Entry"), i.
    e. a `SELECT` statement accesses the result of the
    expressions.

Setup of a statement with CTE:

-   Introductory keyword `WITH`
-   A comma-separated list with at least one definition of a CTE
    -   Each CTE has a unique name with an initial `+` character
    -   An optional list of column names, which should be used in the
        result set, within parentheses
    -   `AS` followed by a subquery with `SELECT` which
        creates the tabular result set of the CTE
-   A closing main query with `SELECT` in which the previous
    CTEs are to be used as data source
-   If a `SELECT` loop is opened and data is written into a work
    area in the closing main query, the loop must be closed with
    `ENDWITH.` (which fulfills the same task as
    `ENDSELECT.`).

> [!NOTE]
>-   Each CTE must be used at least once, either in another CTE or in the
    main query. The main query must access at least one CTE.
>-   The result set of a CTE never has a [client
    column](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclient_column_glosry.htm "Glossary Entry").
>- See more information in [this topic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapwith.htm)
and further options and additions when using CTEs in the subtopics.

Example: The result sets of both common table expressions
`+connections` and `+sum_seats` are merged in the
subquery of the CTE `+result` in a join expression. An explicit
name list assigns names to the resulting columns. These names are used
in the main query to sort the results. For each flight connection of the
selected airline, the total number of occupied seats is stored in the
internal table.
``` abap
WITH
+connections AS (
  SELECT zdemo_abap_flsch~carrid, carrname, connid, cityfrom, cityto
    FROM zdemo_abap_flsch
    INNER JOIN zdemo_abap_carr
    ON zdemo_abap_carr~carrid = zdemo_abap_flsch~carrid
    WHERE zdemo_abap_flsch~carrid BETWEEN 'AA' AND 'JL' ),
+sum_seats AS (
  SELECT carrid, connid, SUM( seatsocc ) AS sum_seats
    FROM zdemo_abap_fli
    WHERE carrid BETWEEN 'AA' AND 'JL'
    GROUP BY carrid, connid ),
+result( name, connection, departure, arrival, occupied ) AS (
  SELECT carrname, c~connid, cityfrom, cityto, sum_seats
    FROM +connections AS c
    INNER JOIN +sum_seats AS s
    ON c~carrid = s~carrid AND c~connid = s~connid )
SELECT *
  FROM +result
  ORDER BY name, connection
  INTO TABLE @DATA(result).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


## SQL Conditions

You can formulate conditions in ABAP SQL statements, i. e. [logical expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogical_expression_glosry.htm "Glossary Entry"), especially in the `WHERE` clause to restrict the result. Note that without a `WHERE` clause, all rows are respected for the operation.

Find more details and examples in the [WHERE Conditions](31_WHERE_Conditions.md) cheat sheet.

Among the syntax options are: 

- `... a = b ...` /  `... a EQ b ...` 
- `... a <> b ...` / `... a NE b ...` 
- `... a < b ...` / `... a LT b ...`
- `... a > b ...` / `... a GT b ...` 
- `... a <= b ...` / `... a LE b ...` 
- `... a >= b ...` / `... a GE b ...`
- `... AND ...` / `... OR ...` / `... ( ... ) ...`
- `... a [=|<>|>|<|...] [ALL|ANY|SOME] ( SELECT ... ) ...`
- `... a [NOT] BETWEEN b AND c ...`
- `... a [NOT] LIKE b [ESCAPE c] ...`
- `... a IS [NOT] INITIAL ...`
- `... a [NOT] IN (b, c, ...)...`
- `... a [NOT] IN ( SELECT ... ) ...`
- `... ( a, b, ... ) IN ( ( d, e, ... ) ( f, g, ...) ... ) ...`
- `... ( a, b, ... ) IN ( SELECT ... ) ...`
- `... a [NOT] IN @ranges_table ...`
- `... EXISTS ( SELECT ... ) ...`
- `... a IS [NOT] NULL ...`
- `... (dynamic_where_clause) ...` 

<p align="right"><a href="#top">⬆️ back to top</a></p>

## SQL Operands

-   [SQL
operands](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_operand_glosry.htm "Glossary Entry") are elementary operands in ABAP SQL statements
-   Can be database table or view columns, a
    [literal](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenliteral_glosry.htm "Glossary Entry"),
    [host
    variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhost_variable_glosry.htm "Glossary Entry")
    (i. e. global or local data objects escaped using `@`:
    `@dobj`) or [host
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhost_expression_glosry.htm "Glossary Entry")
    (`@( ... )`)
    -   Regarding literals: They are not prefixed with the escape
        character `@`. The literals can be
        [typed](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentyped_literal_glosry.htm "Glossary Entry")
        (using the type name and content within a pair of backquotes:
        <code>char\`abc\`</code>) with [built-in ABAP Dictionary
        types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_builtin_types.htm)
        or untyped. See the [Typed Literals](#typed-literals) section further down.
    -   Regarding host expressions: Structures and internal tables are
        possible as host expressions for statements modifying the
        content of database tables as shown further down.
-   Find more information
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_operands.htm).


> [!NOTE]
> Questions about when to use what, what is possible in which contexts and positions, is beyond the scope of this cheat sheet. Check the details in the
respective topics in the ABAP Keyword Documentation. Find a general overview of important operand positions in ABAP SQL [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_operand_positions_oview.htm). Due to the rich variety of options, the cheat sheet covers a selection.


Example demonstrating possible operands:
``` abap
DATA number TYPE i VALUE 3.

SELECT FROM zdemo_abap_flsch
  FIELDS
  "Specifies a column of a data source directly using its name
  cityfrom,

  "Column selector ~ can be used to prefix every specified column.
  "Here, it is optional. It is non-optional, e. g., if multiple data
  "sources in an ABAP SQL statement are edited and the column name
  "is not unique.
  zdemo_abap_flsch~cityto,

  'Lufthansa' AS name, "Untyped literal

  char`X` AS flag, "Typed literal

  @number AS num, "Host variable

  @( cl_abap_context_info=>get_system_date( ) ) as date "Host expression

  WHERE carrid = 'LH'          "Untyped literal
    AND countryfr = char`DE`   "Typed literal

  "Data object created inline and escaped with @
  INTO TABLE @DATA(it)

  "The following clause shows all options having the same effect
  UP TO 3 ROWS.             "Untyped numeric literal
  "UP TO int4`3` ROWS.      "Typed numeric literal
  "UP TO @number ROWS.        "Host variable
  "UP TO @( 10 - 7 ) ROWS.  "Host expression
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


## SQL Expressions

- SQL expressions can be specified in various positions of ABAP SQL statements.
- They are passed to the database system for evaluation.
- For example, SQL expressions can be specified as columns in the
    `SELECT` list as demonstrated in most of the following examples.
- Find information on more possible positions and general information
    on SQL expressions
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapsql_expr.htm)
    and the subtopics there.

> [!NOTE]
> You can [enclose SQL expressions in parentheses](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_exp_parentheses.htm).
> ```abap
> SELECT SINGLE
>    carrid,
> 
>    "SQL expressions enclosed in parenthesis in the context
>    "of arithmetic expressions impacting the prioriry of
>    "calculations
>    ( 1 + 2 ) * ( 3 + 4 ) * ( 5 + 6 ) AS calc_w_par, "231
>    1 + 2 * 3 + 4 * 5 + 6 AS calc_no_par "33
> 
> FROM zdemo_abap_fli
> WHERE carrid = 'AA'
> INTO @DATA(using_parentheses).
> ```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Elementary SQL Expressions

- An elementary expression represents one of the four mentioned
    operands above: A value from the data source (i.e. the column name), values from an ABAP program passed to the database (literal, host
    variable or host expression).
-   As an example, see the `SELECT` list in the example above.
-   See more information
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_elem.htm).

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Arithmetic Expressions

[Arithmetic expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_arith.htm) perform arithmetic calculations using the operators `+`, `-`, `*`, `/`.

``` abap
SELECT SINGLE
  carrid,

  "Arithmethic expressions
  "operators + - *
  "Note that / is not allowed in integer expressions as the one below
  ( 1 + 2 ) * 3 AS calc,

  "/ used in an expression using type adjustment in ABAP SQL.
  "A cast expression converts the value of the operands to the
  "specified dictionary type. The result is a representation of the
  "source value in the specified type.
  CAST( 1 AS D34N ) / CAST( 2 AS D34N ) AS ratio

FROM zdemo_abap_carr
WHERE carrid = 'AA'
INTO @DATA(arithmetic_sql_expr).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Cast Expressions

- [Cast expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_cast.htm) are used to convert the value of operands to a dedicated dictionary type.
- Note that there are special [conversion rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_cast_rules.htm). 
- See the possible types in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_cast.htm).

```abap
SELECT SINGLE
    carrid,

    "A cast expression converts the value of the operands to the
    "specified dictionary type. The result is a representation of the
    "source value in the specified type.
    CAST( 1 AS D34N ) / CAST( 2 AS D34N ) AS ratio,
    CAST( connid AS INT4 ) AS connidnum,
    CAST( @( cl_abap_context_info=>get_system_date( ) ) AS CHAR ) AS dat

FROM zdemo_abap_fli
WHERE carrid = 'AA'
INTO @DATA(cast_expr).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### String Expressions

[String expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_string.htm) use the operator `&&` to concatenate character strings.

```abap
SELECT SINGLE
    carrid,

    "String expression using && to concatenate two character strings;
    "the result of the concatenation must not be longer than
    "255 characters.
    carrid && char`_` && carrname AS concat

FROM zdemo_abap_carr
WHERE carrid = 'AA'
INTO @DATA(string_expr).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Case Expressions

- [Case expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_case.htm) carry out either a simple (comparison of the values of a dedicated operand) or complex (searched case; evaluation of multiple logical expressions) case distinction.
- Not specifying `ELSE` means that the result is the [null value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennull_value_glosry.htm). The null value can be specified explicitly by the null expression `NULL`.

```abap
SELECT SINGLE
  carrid,

  "Simple case distinction
  "The expression compares the values of an operand with other
  "operands. Result: The first operand after THEN for which the
  "comparison is true. If no matches are found, the result specified
  "after ELSE is selected.
  CASE currcode
    WHEN 'EUR' THEN 'A'
    WHEN 'USD' THEN 'B'
    ELSE 'C'
  END AS case_simple,

  "Complex case distinction
  "The expression evaluates logical expressions. Result: The first
  "operand after THEN for which the logical expression is true. If no
  "logical expressions are true, the result specified after ELSE is
  "selected.
  CASE WHEN length( carrname ) <= 5 THEN 'small'
       WHEN length( carrname ) BETWEEN 6 AND 10 THEN 'mid'
       WHEN length( carrname ) BETWEEN 11 AND 15 THEN 'large'
       ELSE 'huge'
  END AS case_complex

FROM zdemo_abap_carr
WHERE carrid = 'AA'
INTO @DATA(case_expr).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Aggregate Expressions

-   [Aggregate expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_aggregate.htm) consist of [aggregate functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenaggregate_function_glosry.htm "Glossary Entry")
    and aggregate the values of multiple rows of the result set of a
    [query](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenquery_glosry.htm "Glossary Entry")
    into a single value
-   The example shows a selection. Find more information
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_aggregate.htm).

Example:
``` abap
"The example shows a selection of available functions
SELECT
  carrid,

  "Average value of the content of a column in a row set
  AVG( fltime ) AS fltime1,

  "AVG with data type specification for the result
  AVG( fltime AS DEC( 14,4 ) ) AS fltime2,

  "Maximum value of the results in a row set
  MAX( fltime ) AS max,

  "Minimum value
  MIN( fltime ) AS min,

  "Sum of the results in a row set.
  SUM( fltime ) AS sum,

  "Returns the number of rows in a row set.
  "The following two have the same meaning.
  COUNT( * ) AS count2,
  COUNT(*) AS count3,

  "Chains the results in a row set.
  "An optional separator can be specified
  STRING_AGG( airpfrom, ', ' ) AS string_agg

FROM zdemo_abap_flsch
WHERE carrid = 'LH'
GROUP BY carrid
INTO TABLE @DATA(agg_exp).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Window Expressions

How [window expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwindow_expression_glosry.htm "Glossary Entry") work:

-   Define a subset of the result set (i. e. the
    "[window](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwindow_glosry.htm "Glossary Entry")")
    of a database query that implements ABAP SQL
-   Apply a [window function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwindow_function_glosry.htm "Glossary Entry") -
    which evaluates the rows of the window and which can, for example,
    be an [aggregate
    function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenaggregate_function_glosry.htm "Glossary Entry")
    like
    [`AVG`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_agg_func&sap-language=EN&sap-client=000&version=X&anchor=!ABAP_VARIANT_1@1@&tree=X)
    to determine the average value - to the result set
-   I. e. a window is constructed by the rows of the result set for
    which all the window functions have the same result; a value is then
    determined for the rows of a window

Setup of a statement with window expressions:

-   Window function, e. g. an aggregate function like `AVG`,
    followed by `OVER( ... )` (the content in the parentheses
    defines the "window")
-   The content in the parentheses can contain the following additions:
    -   Optional `PARTITION BY`: Defines the windows using a
        comma-separated list of [SQL
        expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapsql_expr.htm);
        the window function is calculated for the rows of this window;
        note that if the addition is not specified, the window comprises
        all rows of the result set
    -   Optional `ORDER BY`: Introduces both an order (you can
        use `ASCENDING` and `DESCENDING`) and a frame
        (as outlined below) within the current window, which further
        restricts the rows for which the window function is calculated
    -   A window frame, which stands for a subset of rows inside a
        window, can optionally be defined if `ORDER BY` is
        specified; there are 3 options to define the starting and ending
        frame boundaries (see the example)

See more information on window expressions and the syntax
[here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_over.htm).

Examples:
``` abap
"Example 1: A simple window is constructed in the OVER clause;
"window functions - here aggregate functions - are applied
SELECT carrid, currency,
  SUM( paymentsum ) OVER( PARTITION BY carrid ) AS sum,
  AVG( price AS DEC( 14,2 ) ) OVER( PARTITION BY carrid ) AS avg,
  MAX( price ) OVER( PARTITION BY carrid ) AS max
  FROM zdemo_abap_fli
  ORDER BY carrid
  INTO TABLE @DATA(win).

"Example 2:
SELECT carrid, currency, fldate,
  "Sorts the rows by some columns and counts the number of rows from
  "the first row of the window to the current row.
  COUNT( * ) OVER( ORDER BY currency, fldate
                   ROWS BETWEEN
                   "UNBOUNDED PRECEDING: frame starts at the first row of the window
                   UNBOUNDED PRECEDING
                   "CURRENT ROW: determines starting or ending at the current row; here, it ends
                   AND CURRENT ROW ) AS count1,

  "If no window frame is used, the default window frame is
  "BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW,
  "i. e. the result of count1 equals the result of count2.
  COUNT( * ) OVER( ORDER BY currency, fldate ) AS count2,

  "Sorts the rows by some columns and counts the number of rows from
  "the current row to the last row of the window.
  "The result is reverse numbering.
  COUNT( * ) OVER( ORDER BY currency, fldate
                   ROWS BETWEEN CURRENT ROW
                   UNBOUND FOLLOWING:
                   "Determines the ending frame boundary, this addition specifies the last row of the window
                   AND UNBOUNDED FOLLOWING ) AS count_reverse,

  "Sorts the rows by some columns and calculates the rolling averages
  "of a subset of rows from column price. The subset consists of the
  "current row plus one preceding and one following row. Another use
  "case as below example that uses prices would be that, for example,
  "you can calculate the 3-day-average temperature for every day from
  "a list of temperature data.
  AVG( price AS DEC( 14,2 ) ) OVER( ORDER BY currency, fldate
       ROWS BETWEEN
       "n PRECEDING: for both start and end of frame; frame to start/end n rows above the current row
       1 PRECEDING
       "n FOLLOWING: for both start and end of frame; frame to start/end n rows beneath the current row
       AND 1 FOLLOWING ) AS avg

  FROM zdemo_abap_fli
  INTO TABLE @DATA(result).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Null Expressions

The [null value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennull_value_glosry.htm) is represented by the `NULL` operand in ABAP SQL statements. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_null.htm).

```abap
SELECT
    carrid,
    carrname,
    "The type of the null value is determined by the context.
    "When the null value is passed to the internal table,
    "it is converted to the initial value. In the first case,
    "it is ' '. In the second case, it is 0..
    CASE WHEN length( carrname ) > 12 THEN char`X`
      ELSE NULL
    END AS long_name,
    CAST( NULL AS INT1 ) AS null_val

FROM zdemo_abap_carr
INTO TABLE @DATA(null_expr).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Built-In SQL Functions     
- Built-in SQL functions can be called in ABAP SQL.
- The result is a value with the associated dictionary type. 
- The arguments of the functions can cover one or more SQL expressions. 
- For all available functions and for more information, refer to [this topic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_builtin_functions.htm) in the ABAP Keyword Documentation and the subtopics.
- The functions are also covered in other cheat sheets such as the [Built-In Functions](24_Builtin_Functions.md) cheat sheet.

### Numeric Functions

[Numeric functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_arith_func.htm)

``` abap
SELECT SINGLE
  carrname,

  "Division, result rounded to an integer
  "Result: 2
  div( 4, 2 ) AS div,

  "Division, 3rd argument: result is rounded to the specified
  "number of decimals
  "Result: 0.33
  division( 1, 3, 2 ) AS division,

  "Result is rounded to first greater integer
  "Result: 2
  ceil( decfloat34`1.333` ) AS ceil,

  "Result is the remainder of division
  "Result: 1
  mod( 3, 2 ) AS mod,

  "Result: Largest integer value not greater than the specified value
  "Result: 1
  floor( decfloat34`1.333` ) AS floor,

  "Returns the absolute number
  "Result: 2
  abs( int4`-2` ) AS abs,

  "Result is rounded to the specified position after the decimal separator
  "Result: 1.34
  round( decfloat34`1.337`, 2 ) AS round

  FROM zdemo_abap_carr
  WHERE carrid = 'AA'
  INTO @DATA(numeric_functions).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### String Functions

[String functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_string_func.htm)

``` abap
SELECT SINGLE
  carrid,    "LH
  carrname,  "Lufthansa
  url,       "http://www.lufthansa.com

  "Concatenates strings, ignores trailing blanks
  "Result: LHLufthansa
  concat( carrid, carrname ) AS concat,

  "Concatenates strings, number denotes the blanks that are inserted
  "Result: LH Lufthansa
  concat_with_space( carrid, carrname, 1 ) AS concat_with_space,

  "First letter of a word -> uppercase, all other letters -> lowercase;
  "note that a space and other special characters means a new word.
  "Result: Http://Www.Lufthansa.Com
  initcap( url ) AS initcap,

  "Position of the first occurrence of the substring specified
  "Result: 6
  instr( carrname,'a' ) AS instr,

  "String of length n starting from the left of an expression;
  "trailing blanks are ignored
  "Result: Luft
  left( carrname, 4 ) AS left,

  "Number of characters in an expression, trailing blanks are ignored
  "Result: 24
  length( url ) AS length,

  "Checks if expression contains a PCRE expression;
  "case-sensitive by default (case_sensitive parameter can be specified)
  "Notes on the result: 1 = found, 0 = not found
  "Result: 1
  like_regexpr( pcre  = '\..',  "Period that is followed by any character
                value = url ) AS like_regex,

  "Returns position of a substring in an expression,
  "3rd parameter = specifies offset (optional)
  "4th parameter = determines the number of occurrences (optional)
  "Result: 9
  locate( carrname, 'a', 0, 2 ) AS locate,

  "Searches a PCRE pattern, returns offset of match;
  "many optional parameters: occurrence, case_sensitive, start, group
  "Result: 21
  locate_regexpr( pcre = '\..', "Period followed by any character
                  value = url,
                  occurrence = 2 ) "2nd occurrence in the string
                  AS locate_regexpr,

  "Searches a PCRE pattern, returns offset of match + 1;
  "many optional parameters: occurrence, case_sensitive, start, group
   "Result: 2
  locate_regexpr_after( pcre = '.',  "Any character
                        value = url,
                        occurrence = 1 ) AS locate_regexpr_after,

  "Removes leading characters as specified in the 2nd argument,
  "trailing blanks are removed
  "Result: ufthansa
  ltrim( carrname, 'L' ) AS ltrim,

  "Counts all occurrences of found PCRE patterns
  "Result: 2
  occurrences_regexpr( pcre = '\..', "Period that is followed by any character
                       value = url ) AS occ_regex,

  "Replaces the 2nd argument with the 3rd in an expression
  "Result: Lufth#ns#
  replace( carrname, 'a', '#' ) AS replace,

  "Replaces a found PCRE expression;
  "more parameters possible: occurrence, case_sensitive, start
   "Result: http://www#ufthansa#om
  replace_regexpr( pcre = '\..', "Period that is followed by any character
                   value = url,
                   with = '#' ) AS replace_regex,

  "Extracts a string with the length specified starting from the right
  "Result: hansa
  right( carrname, 5 ) AS right,

  "Expands string to length n (2nd argument); trailing blanks produced
  "are replaced by the characters from the (3rd) argument
  "Note that if n is less than the string, the expression is truncated
  "on the right.
  "Result: Lufthansa###
  rpad( carrname, 12, '#' ) AS rpad,

  "All trailing characters that match the character of the 2nd argument
  "are removed; trailing blanks are removed, too
  "Result: Lufthans
  rtrim( carrname, 'a' ) AS rtrim,

  "Returns a substring; 2nd argument = position from where to start;
  "3rd argument: length of the extracted substring
  "Result: fth
  substring( carrname, 3, 3 ) AS substring,

  "Searches for a PCRE expression and returns the matched substring
  "More parameters possible: occurrence, case_sensitive, start, group
  "Result: .lu
  substring_regexpr( pcre = '\...', "Period that is followed by any two characters
                     value = url ) AS substring_regexpr,

  "All lower case letters are transformed to upper case letters
  "Result: LUFTHANSA
  upper( carrname ) AS upper

  FROM zdemo_abap_carr
  WHERE carrid = 'LH'
  INTO @DATA(string_functions).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


### coalesce Function

[coalesce](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_coalesce.htm)


```abap
"The null value is a special value that is returned by a database. It indicates an
"undefined value or result. Note that, in ABAP, there are no special null values. Do
"not confuse the null value with a type-dependent initial value. When using SELECT
"statements to read data, null values can be produced by, for example, outer joins.
"When the null values are passed to a data object, they are transformed to the
"type-dependent initial values. For more information, refer to the ABAP Keyword Documentation.
"The following example uses a left outer join to intentionally create null values. For
"this purpose, two demo database tables of the ABAP cheat sheet repository are cleared and
"populated with specific values to visualize null values.
DELETE FROM zdemo_abap_tab1.
DELETE FROM zdemo_abap_tab2.
MODIFY zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 char1 = 'a' char2 = 'y' )
                                              ( key_field = 2 char1 = 'b' char2 = 'z' ) ) ).
MODIFY zdemo_abap_tab2 FROM TABLE @( VALUE #( ( key_field = 1 char1 = 'a' )
                                              ( key_field = 2 char1 = 'a' )
                                              ( key_field = 3 char1 = 'b' )
                                              ( key_field = 4 ) ) ).

"Note that for the entry 'key_field = 4' no char1 value was passed.
"char1 is a shared column of the two database tables, and which is used in
"the ON condition of the join. Since there is no entry in char1 for 'key_field = 4',
"the joined values are null in that case.
"The coalesce function is used to replace null values produced by an outer join with
"a different value.
SELECT tab2~key_field,
       coalesce( tab1~char1, '-' ) AS coalesced1,
       coalesce( tab1~char2, '#' ) AS coalesced2,
       "A coalesce function is a short form of a complex
       "case distinction such as the following:
       CASE WHEN tab1~char1 IS NOT NULL THEN tab1~char1
        ELSE '?'
       END as coalesced3

    FROM zdemo_abap_tab2 AS tab2
    LEFT OUTER JOIN zdemo_abap_tab1 AS tab1 ON tab1~char1 = tab2~char1
    INTO TABLE @DATA(join_w_null).

*Example table content
*KEY_FIELD    COALESCED1    COALESCED2    COALESCED3
*1            a             y             a
*2            a             y             a
*3            b             z             b
*4            -             #             ?
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Conversion Functions

[Type conversion functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENSQL_TYPE_CONV_FUNC.html): 

<table>

<tr>
<td> Function </td> <td> Notes </td> <td> Code Snippet </td>
</tr>

<tr>
<td> 

`bintohex( ... )` 

 </td>

 <td> 

Converts byte strings (type `raw`; mapped to ABAP type `x`) to character strings (type `char`)


 </td>

<td>

The code snippet implements the following: 
- To have a self-contained example, a demo internal table with elementary line type (byte-like type `x length 10`) is created. 
- The table is filled with demo data. 
- An ABAP SQL `SELECT` statement that includes the `bintohex` function retrieves data from the internal table. Note that a warning would be displayed that the `SELECT` command is executed on the database. The warning is suppressed with a pragma.
- [Runtime Type Identification (RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm) (find more information in the Dynamic Programming cheat sheet) is used to demonstrate that the type of the `tline` (the alias name for `table_line`) is character-like. 
<br>

``` abap
TYPES x10  TYPE x LENGTH 10.
TYPES ty_raw_tab TYPE TABLE OF x10 WITH EMPTY KEY.

DATA(raw_tab) = VALUE ty_raw_tab( ( CONV x10( '68656C6C6F' ) )
                                  ( CONV x10( '776F726C64' ) )
                                  ( CONV x10( '41424150' ) ) ).

SELECT bintohex( table_line ) AS tline
    FROM @raw_tab AS tab
    INTO TABLE @DATA(conv_to_blob_tab) ##ITAB_DB_SELECT.

DATA(tdo_itab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( conv_to_blob_tab ) ).
DATA(table_components_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) )->components.
DATA(text_line_type_kind) = table_components_itab[ name = 'TLINE' ]-type_kind.
ASSERT text_line_type_kind = cl_abap_typedescr=>typekind_char.
``` 

 </td>
</tr>

<tr>
<td> 

`hextobin( ... )`

 </td>

 <td> 

Converts character strings (type `char` or `numc`) to byte strings (type `raw`; mapped to ABAP type `x`)


 </td>

<td> 

The code snippet implements the following: 
- To have a self-contained example, a demo internal table with elementary line type (character-like type `c length 10`) is created. 
- The table is filled with demo data. 
- An ABAP SQL `SELECT` statement that includes the `hextobin` function retrieves data from the internal table. Note that a warning would be displayed that the `SELECT` command is executed on the database. The warning is suppressed with a pragma.
- RTTI is used to demonstrate that the type of the `tline` (the alias name for `table_line`) is `x`. 

<br>

``` abap
TYPES c10 TYPE c LENGTH 10.
TYPES ty_c_tab TYPE TABLE OF c10 WITH EMPTY KEY.

DATA(c_tab) = VALUE ty_c_tab( ( '68656C6C6F' )
                              ( '776F726C64' )
                              ( '41424150' ) ).

SELECT hextobin( table_line ) AS tline
    FROM @c_tab AS tab
    INTO TABLE @DATA(hextobin_tab) ##ITAB_DB_SELECT.

DATA(tdo_itab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( hextobin_tab ) ).
DATA(table_components_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) )->components.
DATA(text_line_type_kind) = table_components_itab[ name = 'TLINE' ]-type_kind.
ASSERT text_line_type_kind = cl_abap_typedescr=>typekind_hex.
``` 

 </td>
</tr>


<tr>
<td> 

`to_blob( ... )`

 </td>

 <td> 

Converts from a byte field (type `raw`; mapped to ABAP type `x`) to a byte string (a blob, Binary Large Object; type `rawstring`; mapped to ABAP type `xstring`).

 </td>

<td> 

The code snippet implements the following: 
- To have a self-contained example, a demo internal table with elementary line type (byte-like type, `x length 10`) is created. 
- The table is filled with demo data. 
- An ABAP SQL `SELECT` statement that includes the `to_blob` function retrieves data from the internal table. Note that a warning would be displayed that the `SELECT` command is executed on the database. The warning is suppressed with a pragma.
- RTTI is used to demonstrate that the type of the `tline` (the alias name for `table_line`) is `xstring`. 

<br>

``` abap
TYPES x10  TYPE x LENGTH 10.
TYPES ty_raw_tab TYPE TABLE OF x10 WITH EMPTY KEY.

DATA(raw_tab) = VALUE ty_raw_tab( ( CONV x10( '68656C6C6F' ) )
                                    ( CONV x10( '776F726C64' ) )
                                    ( CONV x10( '41424150' ) ) ).

SELECT to_blob( table_line ) AS tline
    FROM @raw_tab AS tab
    INTO TABLE @DATA(conv_to_blob_tab) ##ITAB_DB_SELECT.

DATA(tdo_itab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( conv_to_blob_tab ) ).
DATA(table_components_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) )->components.
DATA(text_line_type_kind) = table_components_itab[ name = 'TLINE' ]-type_kind.
ASSERT text_line_type_kind = cl_abap_typedescr=>typekind_xstring.
``` 

 </td>
</tr>

<tr>
<td> 

`to_clob( ... )`

 </td>

 <td> 

- Converts a fixed-length character string (type `char` or `sstring`) to a clob (character large object; type `string`)
- The argument specified in the parentheses can be a table column, literal, host variable/constant, or an SQL expression

 </td>

<td> 

The example class, executable with F9 in ADT, implements the following: 
- To have a self-contained example, a demo internal table is created. Among others, it includes the `text` component that is of type `c` with length 255.
- The table is filled with lots of data. `text` receives random strings of a random length (1 - 255) to have data to work with.
- An ABAP SQL `SELECT` statement that includes the `to_clob` function retrieves data from the internal table. Note that a warning would be displayed that the `SELECT` command is executed on the database. The warning is suppressed with a pragma.
- The `to_clob` has an SQL expression as argument. In this case, it is an aggregate expression with `STRING_AGG`, which aggregates the value of multiple rows into a single value.
- Using RTTI, it is demonstrated that the type of the `text_line` component (the alias name for `text`) is `string`. It is then evaluated how many characters the random strings contain. The example is set up to show that the aggregated string exceeds 1333 characters, which is the maximum length of fields of type `sstring`.

<br>

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS get_random_string IMPORTING VALUE(length) TYPE i OPTIONAL
                              RETURNING VALUE(str)    TYPE string.
    TYPES: BEGIN OF demo_struct,
             num  TYPE i,
             uuid TYPE sysuuid_x16,
             text TYPE c LENGTH 255,
           END OF demo_struct.
    TYPES      ty_demo_tab TYPE SORTED TABLE OF demo_struct WITH UNIQUE KEY num uuid.

    METHODS get_random_table_content IMPORTING VALUE(table_entry_count) TYPE i OPTIONAL
                                     RETURNING VALUE(demo_tab)          TYPE ty_demo_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA(random_table) = get_random_table_content( 500 ).

    SELECT num, to_clob( STRING_AGG( text, ',' ) ) AS text_line
        FROM @random_table AS tab
        GROUP BY num
        ORDER BY num
        INTO TABLE @DATA(aggregated_data) ##ITAB_DB_SELECT.

    "Checking the type of the text_line component using RTTI
    DATA(tdo_itab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( aggregated_data ) ).
    DATA(table_components_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) )->components.
    DATA(text_line_type_kind) = table_components_itab[ name = 'TEXT_LINE' ]-type_kind.
    IF text_line_type_kind = cl_abap_typedescr=>typekind_string.
      out->write( `The text_line component is of type string.` ).
      out->write( |\n| ).
    ENDIF.

    DATA(lines_w_more_than_1333) = REDUCE i( INIT int = 0
                                             FOR line IN aggregated_data
                                             NEXT int =  COND #( WHEN strlen( line-text_line ) > 1333 THEN int + 1 ELSE int ) ).

    out->write( |{ lines_w_more_than_1333 } out of { lines( aggregated_data ) } lines of the internal table have a text_line value exceeding 1333 characters.| ).
    out->write( |\n| ).

  ENDMETHOD.

  METHOD get_random_string.
    IF length IS NOT SUPPLIED OR length > 255.
      length =  cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                            min  = 1
                                            max  = 255 )->get_next( ).
    ENDIF.

    DATA(characters) = `aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ1234567890`.
    DATA(off) = strlen( characters ) - 1.

    DO length TIMES.
      DATA(random_offset) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                  min  = 0
                                                  max  = off )->get_next( ).

      TRY.
          str &&= characters+random_offset(1).
        CATCH cx_sy_range_out_of_bounds.
      ENDTRY.
    ENDDO.
  ENDMETHOD.

  METHOD get_random_table_content.
    IF table_entry_count IS NOT SUPPLIED OR table_entry_count > 1000.
      table_entry_count =  cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                  min  = 1
                                                  max  = 1000 )->get_next( ).
    ENDIF.

    "Example implementation: For the string aggregation in the SELECT statement,
    "multiple lines should be created with the same key value in 'num'.
    DATA(value4num) = 1.
    DO table_entry_count TIMES.
      IF sy-index MOD 25 = 0.
        value4num += 1.
      ENDIF.

      INSERT VALUE #( num = value4num
                      uuid = xco_cp=>uuid( )->value
                      text = get_random_string( ) ) INTO TABLE demo_tab.
    ENDDO.
  ENDMETHOD.
ENDCLASS.
```


 </td>
</tr>


<tr>
<td> 

`unit_conversion( ... )`

 </td>

 <td> 

Converts units for a value passed to the `quantity` parameter

 </td>

<td> 

The code snippet implements the following: 
- To have a self-contained example, a demo internal table with elementary line type (packed number, `p length 16 decimals 14`) is created. 
- The table is filled with demo data. 
- Two ABAP SQL `SELECT` statements are included that specify the `unit_conversion` function. Data is retrieved from the internal table and converted. Note that a warning would be displayed that the `SELECT` command is executed on the database. The warning is suppressed with a pragma.
- The example covers the conversion of miles to kilometers and vice versa.


<br>

```abap
TYPES p_len16dec14 TYPE p LENGTH 16 DECIMALS 14.
TYPES ty_plen16dec14_tab TYPE TABLE OF p_len16dec14 WITH EMPTY KEY.

DATA(p_tab) = VALUE ty_plen16dec14_tab( ( CONV p_len16dec14( '1' ) )
                                        ( CONV p_len16dec14( '5.7' ) )
                                        ( CONV p_len16dec14( '4.2' ) )
                                        ( CONV p_len16dec14( '25.78' ) ) ).

SELECT unit_conversion( quantity = table_line,
                        source_unit = unit`MI`,
                        target_unit = unit`KM` ) AS miles_to_km
    FROM @p_tab AS tab
    INTO TABLE @DATA(unit_conv_mi2km_tab) ##ITAB_DB_SELECT.

SELECT unit_conversion( quantity = miles_to_km,
                        source_unit = unit`KM`,
                        target_unit = unit`MI` ) AS km_to_miles
    FROM @unit_conv_mi2km_tab AS tab
    INTO TABLE @DATA(unit_conv_km2mi_tab) ##ITAB_DB_SELECT.
```


 </td>
</tr>

<tr>
<td> 

`currency_conversion( ... )`

 </td>

 <td> 

- Converts currencies for a value passed to the `amount` parameter
- Multiple optional parameters can be specified (excluding `client` in ABAP for Cloud Development)

 </td>

<td> 

The code snippet implements the following: 
- To have a self-contained example, a demo internal table with elementary line type (packed number, `p length 16 decimals 2`) is created. 
- The table is filled with demo data. 
- An ABAP SQL `SELECT` statement that includes the `currency_conversion` function retrieves data from the internal table and converts currency values from Euro to US dollars. Note that a warning would be displayed that the `SELECT` command is executed on the database. The warning is suppressed with a pragma.

<br>

```abap
TYPES p_len16dec2 TYPE p LENGTH 16 DECIMALS 2.
TYPES ty_plen16dec2_tab TYPE TABLE OF p_len16dec2 WITH EMPTY KEY.

DATA(p_tab) = VALUE ty_plen16dec2_tab( ( CONV p_len16dec2( '1' ) )
                                        ( CONV p_len16dec2( '5.7' ) )
                                        ( CONV p_len16dec2( '4.2' ) )
                                        ( CONV p_len16dec2( '25.78' ) ) ).

SELECT currency_conversion( amount = table_line,
                            source_currency = char`EUR`,
                            target_currency = char`USD`,
                            exchange_rate_date = @( cl_abap_context_info=>get_system_date( ) ) ) AS eur2usd
    FROM @p_tab AS tab
    INTO TABLE @DATA(curr_unit_conv_eur2usd) ##ITAB_DB_SELECT.
```


 </td>
</tr>

<tr>
<td> 

`as_geo_json( ... )`

 </td>

 <td> 

Converts geometry input in the [Extended Well-Known Binary (EWKB) representation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENDDIC_GEO_DATA.html) to a geometry object in JSON format


 </td>

<td> 


```abap
SELECT as_geo_json( some_geo_field ) 
    FROM ... 
    WHERE ...
    INTO ... 
```


 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Date and Time Functions

Find more information in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_uuid.htm). The  [Built-In Functions](24_Builtin_Functions.md) cheat sheet covers code examples.

``` abap
SELECT SINGLE
  carrid,
  
  "Selection of time and date-related functions
  add_days( @( cl_abap_context_info=>get_system_date( ) ), 4 ) AS add_days,
  add_months( @( cl_abap_context_info=>get_system_date( ) ), 2 ) AS add_months,
  is_valid( @( cl_abap_context_info=>get_system_date( ) ) ) AS date_is_valid,
  is_valid( @( cl_abap_context_info=>get_system_time( ) ) ) AS time_is_valid
  "...

FROM zdemo_abap_carr
INTO @DATA(tab_w_date_time_func).
```


<p align="right"><a href="#top">⬆️ back to top</a></p>

### UUID Function

[UUID function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_uuid.htm)

``` abap
SELECT SINGLE
  carrid,
  
  "UUID
  uuid( ) AS uuid

FROM zdemo_abap_carr
INTO @DATA(tab_w_uuid).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Create, Update, and Delete Operations 

> [!NOTE]
> - The following sections include code patterns. To explore various syntax options with an executable example, see section [Example: Exploring ABAP SQL Statements Changing Data in Database Tables](#example-exploring-abap-sql-statements-changing-data-in-database-tables) below.
> - There are also CDS artifacts that allow not only reading but also creating, updating, and deleting. See [this section](#crud-operations-using-cds-artifacts).

### INSERT

- Inserts one or more rows into a database table specified.
- The rows to be inserted are taken from a structure, an internal table, or the result set of an embedded subquery.
- As mentioned above, structures and internal tables from which to insert content should be specified as host variables (with `@`) or host
expressions (with `@( ... )`).
- The system fields `sy-subrc` (0 = single row or all rows inserted successfully, 4 = row not or not all rows inserted) and `sy-dbcnt` (number of rows that are inserted) are set.
- More information: [`INSERT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinsert_dbtab.htm)

``` abap
"Inserting a single row into a database table
INSERT dbtab FROM @row.

"Alternative syntax, same effect
INSERT INTO dbtab VALUES @row.

"Line is created inline using the VALUE operator as part of a host expression
INSERT dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ).

"Inserting multiple lines from an internal table into a database table.
"Make sure that the internal table does not contain a line having the same key
"as an existing row in the database table. Otherwise, a runtime error occurs.
INSERT dbtab FROM TABLE @itab.

"Inserting lines from a table declared inline using the VALUE operator
"as part of a host expression
INSERT dbtab FROM TABLE @( VALUE #( ( comp1 = ... comp2 = ... )
                                    ( comp1 = ... comp2 = ... ) ) ).

"ACCEPTING DUPLICATE KEYS addition: To avoid the runtime error mentioned above,
"all lines that would produce duplicate entries in the database table
"regarding the keys are discarded and sy-subrc is set to 4.
INSERT dbtab FROM TABLE @itab ACCEPTING DUPLICATE KEYS.

"Inserting the result set of an embedded subquery
"Here, multiple result sets can be joined, e. g. using UNION.
INSERT dbtab FROM ( SELECT ... ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### UPDATE

- Changes the content of one or more rows of a database table specified.
- Similar to `INSERT`, `sy-subrc` and `sy-dbcnt` are set.
- More information: [`UPDATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapupdate.htm)

``` abap
"Changing content by overwriting entire rows based on a structure
UPDATE dbtab FROM @row.
UPDATE dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ). "Using a host expression

"Changing content by overwriting entire rows based on rows in an internal table
UPDATE dbtab FROM TABLE @itab.

"Using a host expression
UPDATE dbtab FROM TABLE @( VALUE #( ( comp1 = ... comp2 = ... )
                                    ( comp1 = ... comp2 = ... ) ) ).

*&---------------------------------------------------------------------*
*& SET addition
*&---------------------------------------------------------------------*

"Changing values of specific fields without overwriting other, non-specified 
"fields
"Changing values of specific fields in all table rows
"There are mutliple options for the value assignment. E.g. you can use
"a literal, host variable/expression, SQL function, and so on.
"The following statement does not use a WHERE clause. Therefore, 
"the comp2 values of all database table entries are changed.
UPDATE dbtab SET comp2 = 'X'.

"Changing values of specific fields in all found entries by restricting 
"the data sets to be changed using a WHERE clause
"Use a comma-separated list for the components after SET
UPDATE dbtab SET comp2 = 'X', comp3 = 'Y' WHERE comp4 > 100.

"Changing values of specific fields in a single database table entry 
"Assume the entry can be uniquely identified by specifying key values
"in the WHERE clause
UPDATE dbtab SET comp2 = 'X' WHERE key_field = 'Y'.

*&---------------------------------------------------------------------*
*& INDICATORS ... SET STRUCTURE addition
*&---------------------------------------------------------------------*

"Similar to SET, using the INDICATORS ... addition, you  can change content 
"of specific fields without overwriting existing values of other fields by 
"specifying set indicators.
"Example:
"- Structured type is created with WITH INDICATORS addition
"- Internal table from which to update dbtab is created;
"  it includes the indicator structure comp_ind
"- Internal table is filled; only one component is flagged as to be updated
"- Other fields remain unchanged; note that key fields must be included
"  in ind_tab (indicator setting for key fields has no effect)
TYPES ind_wa TYPE dbtab WITH INDICATORS comp_ind TYPE abap_bool.

DATA ind_tab TYPE TABLE OF ind_wa.

ind_tab = VALUE #(
       ( comp1 = ... comp2 = ... comp_ind-comp2 = abap_true )
       ( comp1 = ... comp2 = ... comp_ind-comp2 = abap_true ) ).

UPDATE dbtab FROM TABLE @ind_tab INDICATORS SET STRUCTURE comp_ind.

"In the following example, the logic is reversed using NOT.
UPDATE dbtab FROM TABLE @ind_tab INDICATORS NOT SET STRUCTURE comp_ind.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### MODIFY

- Inserts one or more rows into a database table specified or overwrites existing ones.
- As above, `sy-subrc` and `sy-dbcnt` are set.
- More information: [`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_dbtab.htm)

``` abap
"Inserting a single row into a database table or changing an existing row
MODIFY dbtab FROM @row.

"Using a host expression
MODIFY dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ).

"Inserting/Changing multiple rows
MODIFY dbtab FROM TABLE @itab.

"Using a host expression
MODIFY dbtab FROM TABLE @( VALUE #( ( comp1 = ... comp2 = ... )
                                    ( comp1 = ... comp2 = ... ) ) ).

"Inserting/Changing multiple rows based on a result set of an embedded subquery
MODIFY dbtab FROM ( SELECT ... ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### DELETE

- Deletes one or more rows from a database table specified.
- As above, `sy-subrc` and `sy-dbcnt` are set.
- More information: [`DELETE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdelete_dbtab.htm)

``` abap
"Variant DELETE FROM ...: Either all rows are deleted or restricted

"All rows are deleted
DELETE FROM dbtab.

"Rows are deleted based on a condition
DELETE FROM dbtab WHERE ....

"Note that there are further options available, e. g. ORDER BY, UP TO
"Variant DELETE ... FROM ...: Deleting a single row or multiple row
DELETE dbtab FROM @row.

"Using a host expression
DELETE dbtab FROM @( VALUE #( comp1 = ... ) ).

"Rows are deleted based on data in an internal table
DELETE dbtab FROM TABLE @itab.

"Using a host expression
DELETE dbtab FROM TABLE @( VALUE #( ( comp1 = ... )
                                    ( comp1 = ... ) ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using Constructor Expressions in ABAP SQL Statements

[Constructor expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_glosry.htm) can be very handy in ABAP SQL statements. 
For more information about constructor expressions, see the ABAP Keyword Documentation and the [Constructor Expressions cheat sheet](05_Constructor_Expressions.md). Many additions are available. 
The following examples show a selection. The previous code snippets already include the use of the `VALUE` operator with which structures and internal tables can be created in place.

```abap
*&---------------------------------------------------------------------*
*& VALUE
*&---------------------------------------------------------------------*

"VALUE operator as shown above, creating an internal table in place
INSERT dbtab FROM TABLE @( VALUE #( ( key_field = 1 comp1 = ... )
                                    ( key_field = 2 comp1 = ... )
                                    ( key_field = 3 comp1 = ... ) ) ).

"Inserting a table row from a row created in place
INSERT dbtab FROM @( VALUE #( key_field = 4 comp1 = ... ) ).


"FOR LOOP with VALUE
DATA(it) = VALUE some_it_type( ( key_field = 5 comp1 = ... )
                                ( key_field = 6 comp1 = ... )
                                ( key_field = 7 comp1 = ... ) ).

"In the following example, the internal table from above is looped across. You
"can imagine modifying the components (or not) when mapping the fields (as indicated 
"by the concatentation in the example). In doing so, the internal table values may or 
"may be not modified and inserted into the database table.
INSERT dbtab FROM TABLE @( VALUE #( FOR wa IN it ( key_field = wa-key_field
                                                   comp1 = wa-comp1 && 'XYZ'
                                                   ... ) ) ).

"Using a constructor expression with VALUE and BASE in an UPDATE statement
"The example assumes selecting an entry from a database, modifying it, and updating it again,
"but the non-modified entries shall remain unchanged.

SELECT SINGLE * FROM dbtab WHERE key_field = ... INTO @DATA(read_line).
"Assumed comp1 or comp3 components are not specified, but they retain their
"original content and are not initialized when writing to the database table.
UPDATE dbtab FROM @( VALUE #( BASE read_line comp2 = ... comp4 = ... ) ).

*&---------------------------------------------------------------------*
*& CORRESPONDING
*&---------------------------------------------------------------------*

"The following example assumes that some_itab has a different line type than dbtab.
"I.e. some_itab may have more components that are not available in dbtab. The
"corresponding fields with identical names are used. It is assumed that the components'
"types are compatible or convertible.
INSERT dbtab FROM TABLE @( CORRESPONDING #( some_itab ) ).

"This example assumes that field names are not identical. Using the CORRESPONDING operator
"and its additions, you can carry out a mapping and, for example, exclude components. It is 
"assumed that the components' types are compatible or convertible.
INSERT dbtab FROM TABLE @( CORRESPONDING #( another_itab MAPPING key_field = key comp1 = compZ EXCEPT comp2 ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example: Exploring Create, Update, and Delete Operations with ABAP SQL Statements

To try the following example out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example uses a database table of the ABAP cheat sheets repository and is set up to display output in the console.


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

*&---------------------------------------------------------------------*
*& INSERT
*&---------------------------------------------------------------------*
   
    "Deleting the contents of a demo database table to start with an empty database table
    DELETE FROM zdemo_abap_tab1.

    "Inserting a single row into a database table
    DATA(row_a) = VALUE zdemo_abap_tab1( key_field = 1 char1 = 'aaa' char2 = 'bbb' num1 = 10 num2 = 100 ).
    INSERT zdemo_abap_tab1 FROM @row_a.

    "Alternative syntax, same effect
    DATA(row_b) = VALUE zdemo_abap_tab1( key_field = 2 char1 = 'ccc' char2 = 'ddd' num1 = 20 num2 = 200 ).
    INSERT INTO zdemo_abap_tab1 VALUES @row_b.

    "Line is created inline using the VALUE operator as part of a host expression
    INSERT zdemo_abap_tab1 FROM @( VALUE #( key_field = 3 char1 = 'eee' char2 = 'fff' num1 = 30 num2 = 300 ) ).

    "Inserting multiple lines from an internal table into a database table.
    "Make sure that the internal table does not contain a line having the same key
    "as an existing row in the database table. Otherwise, a runtime error occurs.
    TYPES it_type TYPE TABLE OF zdemo_abap_tab1 WITH EMPTY KEY.
    DATA(it_a) = VALUE it_type( ( key_field = 4 char1 = 'ggg' char2 = 'hhh' num1 = 40 num2 = 400 )
                                ( key_field = 5 char1 = 'iii' char2 = 'jjj' num1 = 50 num2 = 500 ) ).
    INSERT zdemo_abap_tab1 FROM TABLE @it_a.

    "Inserting lines from a table declared inline using the VALUE operator
    "as part of a host expression
    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 6 char1 = 'kkk' char2 = 'lll' num1 = 60 num2 = 600 )
                                                  ( key_field = 7 char1 = 'mmm' char2 = 'nnn' num1 = 70 num2 = 700 ) ) ).

    "ACCEPTING DUPLICATE KEYS addition: To avoid the runtime error mentioned above,
    "all lines that would produce duplicate entries in the database table
    "regarding the keys are discarded and sy-subrc is set to 4.
    DATA(it_b) = VALUE it_type( ( key_field = 1 char1 = '###' char2 = '###' num1 = 0 num2 = 0 )
                                ( key_field = 8 char1 = 'ooo' char2 = 'ppp' num1 = 80 num2 = 800 ) ).
    INSERT zdemo_abap_tab1 FROM TABLE @it_b ACCEPTING DUPLICATE KEYS.
    ASSERT sy-subrc = 4.

    "Inserting the result set of an embedded subquery
    "Various options are available. The examples show a selection.
    "The subqueries use an internal table.

    "No restriction specified, all entries are inserted
    DATA(it_c) = VALUE it_type( ( key_field = 9 char1 = 'qqq' char2 = 'rrr' num1 = 90 num2 = 900 )
                                ( key_field = 10 char1 = 'sss' char2 = 'ttt' num1 = 100 num2 = 1000 )
                                ( key_field = 11 char1 = 'uuu' char2 = 'vvv' num1 = 110 num2 = 1100 ) ).

    INSERT zdemo_abap_tab1 FROM ( SELECT key_field, char1, char2, num1, num2 FROM @it_c AS itc ).

    "WHERE condition specified
    DATA(it_d) = VALUE it_type( ( key_field = 12 char1 = 'www' char2 = 'xxx' num1 = 120 num2 = 1200 )
                                ( key_field = 13 char1 = 'yyy' char2 = 'zzz' num1 = 130 num2 = 1300 )
                                ( key_field = 14 char1 = 'AAA' char2 = 'BBB' num1 = 140 num2 = 1400 ) ).

    INSERT zdemo_abap_tab1 FROM ( SELECT key_field, char1, char2, num1, num2 FROM @it_d AS itd WHERE key_field <= 13 ).

    "Using a subquery and replacing existing values in a CASE expression
    DATA(it_e) = VALUE it_type( ( key_field = 15 char1 = 'X' char2 = 'DDD' num1 = 150 num2 = 1500 )
                                ( key_field = 16 char1 = 'Y' char2 = 'FFF' num1 = 160 num2 = 1600 )
                                ( key_field = 17 char1 = 'Z' char2 = 'HHH' num1 = 170 num2 = 1700 ) ).

    INSERT zdemo_abap_tab1 FROM (
      SELECT key_field,
             CASE WHEN char1 = 'X' THEN 'CCC'
                  WHEN char1 = 'Y' THEN 'EEE'
                  ELSE 'GGG'
             END AS char1,
             char2, num1, num2
        FROM @it_e AS ite ).

    "Retrieving all database entries for display purposes
    SELECT * FROM zdemo_abap_tab1 INTO TABLE @DATA(itab_insert).
    out->write( data = itab_insert name = `itab_insert` ).

*&---------------------------------------------------------------------*
*& UPDATE
*&---------------------------------------------------------------------*

    "Preparing a demo database table
    DELETE FROM zdemo_abap_tab1.
    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 char1 = 'aaa' char2 = 'bbb' num1 = 10 num2 = 100 )
                                                  ( key_field = 2 char1 = 'ccc' char2 = 'ddd' num1 = 20 num2 = 200 )
                                                  ( key_field = 3 char1 = 'eee' char2 = 'fff' num1 = 30 num2 = 300 ) ) ).

    "Changing content by overwriting entire rows based on a structure
    DATA(row_c) = VALUE zdemo_abap_tab1( key_field = 1 char1 = 'ggg' char2 = 'hhh' num1 = 12 num2 = 123 ).
    UPDATE zdemo_abap_tab1 FROM @row_c.

    "The following example specifies a value for the key field that does not
    "exist in the database table. Consequently, no update takes place, the sy-subrc
    "value is set to 4.
    UPDATE zdemo_abap_tab1 FROM @( VALUE #( key_field = 4 char1 = 'iii' char2 = 'jjj' num1 = 44 num2 = 456 ) ).
    ASSERT sy-subrc = 4.

    "Changing content by overwriting entire rows based on rows in an internal table
    DATA(it_j) = VALUE it_type( ( key_field = 2 char1 = 'kkk' char2 = 'lll' num1 = 23 num2 = 234 )
                                ( key_field = 3 char1 = 'mmm' char2 = 'nnn' num1 = 34 num2 = 345 ) ).

    UPDATE zdemo_abap_tab1 FROM TABLE @it_j.

    "Using a host expression, internal table created in place
    INSERT zdemo_abap_tab1 FROM @( VALUE #( key_field = 4 char1 = 'ooo' char2 = 'ppp' num1 = 40 num2 = 400 ) ).
    "The following example does not specify two components. Initial values are used.
    UPDATE zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 4 char2 = 'qqq' num1 = 44 ) ) ).

    "INDICATORS addition: Changing content of specific fields without overwriting
    "existing values of other fields
    TYPES ind_wa TYPE zdemo_abap_tab1 WITH INDICATORS comp_ind TYPE abap_boolean.
    TYPES ind_tab TYPE TABLE OF ind_wa WITH EMPTY KEY.

    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 5 char1 = 'rrr' char2 = 'sss' num1 = 50 num2 = 500 )
                                                  ( key_field = 6 char1 = 'ttt' char2 = 'uuu' num1 = 60 num2 = 600 ) ) ).

    UPDATE zdemo_abap_tab1 FROM TABLE @( VALUE ind_tab( ( key_field = 5 char1 = 'vvv' char2 = 'www' num1 = 56 num2 = 567
                                                          comp_ind-char1 = abap_true comp_ind-char2 = abap_false
                                                          comp_ind-num1 = abap_true comp_ind-num2 = abap_false )
                                                        ( key_field = 6 char1 = 'xxx' char2 = 'yyy' num1 = 67 num2 = 678
                                                          comp_ind-char1 = abap_false comp_ind-char2 = abap_true
                                                          comp_ind-num1 = abap_false comp_ind-num2 = abap_true ) ) )
      INDICATORS SET STRUCTURE comp_ind.

    SELECT * FROM zdemo_abap_tab1 INTO TABLE @DATA(itab_update).
    out->write( data = itab_update name = `itab_update` ).

    "SET addition: Changing values of specific fields in all table rows
    "Preparing a demo database table
    DELETE FROM zdemo_abap_tab1.
    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 char1 = 'aaa' char2 = 'bbb' num1 = 10 num2 = 100 )
                                                  ( key_field = 2 char1 = 'ccc' char2 = 'ddd' num1 = 20 num2 = 200 )
                                                  ( key_field = 3 char1 = 'eee' char2 = 'fff' num1 = 30 num2 = 300 ) ) ).

    "The following example transforms the character string of a
    "component to upper case. Since no WHERE clause is specified,
    "the char1 components of all database table entries are affected
    UPDATE zdemo_abap_tab1 SET char1 = upper( char1 ).

    "Changing values of specific fields in all found entries by restricting
    "the data sets to be changed using a WHERE clause
    UPDATE zdemo_abap_tab1 SET char2 = concat( char2, '#' ), num1 = num1 + 1, num2 = num2 + 2 WHERE num1 > 15.

    "Changing values of specific fields in a single database table entry
    "assuming the entry can be uniquely identified by specifying key values
    "in the WHERE clause
    "Use a comma-separated list after SET to specify multiple components
    INSERT zdemo_abap_tab1 FROM @( VALUE #( key_field = 99 char1 = 'A' char2 = 'B' num1 = 99 num2 = 100 ) ).
    UPDATE zdemo_abap_tab1 SET char2 = 'X', num1 = 1, num2 = 2  WHERE key_field = 99.

    SELECT * FROM zdemo_abap_tab1 INTO TABLE @DATA(itab_update_set).
    out->write( data = itab_update_set name = `itab_update_set` ).

*&---------------------------------------------------------------------*
*& MODIFY
*&---------------------------------------------------------------------*

    "The examples include INSERT statements to prepare the database table.

    "Deleting the contents of a demo database table to start with an empty database table
    DELETE FROM zdemo_abap_tab1.

    "Inserting a single row into a database table
    DATA(row_d) = VALUE zdemo_abap_tab1( key_field = 1 char1 = 'aaa' char2 = 'bbb' num1 = 10 num2 = 100 ).
    MODIFY zdemo_abap_tab1 FROM @row_d.

    "Inserting a table row using a host expression and a row created in place
    MODIFY zdemo_abap_tab1 FROM @( VALUE #( key_field = 2 char1 = 'ccc' char2 = 'ddd' num1 = 20 num2 = 200 ) ).

    "Inserting a table row ...
    MODIFY zdemo_abap_tab1 FROM @( VALUE #( key_field = 3 char1 = 'eee' char2 = 'fff' num1 = 30 num2 = 300 ) ).
    "... and modifying it. No new row is inserted, the existing one is modified as the key already exists.
    MODIFY zdemo_abap_tab1 FROM @( VALUE #( key_field = 3 char1 = 'ggg' char2 = 'hhh' num1 = 34 num2 = 345 ) ).

    "Inserting table rows from an internal table
    MODIFY zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 4 char1 = 'iii' char2 = 'jjj' num1 = 40 num2 = 400 )
                                                  ( key_field = 5 char1 = 'kkk' char2 = 'lll' num1 = 50 num2 = 500 ) ) ).
    "Modifying/inserting from an internal table
    MODIFY zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 4 char1 = 'mmm' char2 = 'nnn' num1 = 45 num2 = 456 )
                                                  ( key_field = 5 char1 = 'ooo' char2 = 'ppp' num1 = 56 num2 = 567 )
                                                  ( key_field = 6 char1 = 'qqq' char2 = 'rrr' num1 = 60 num2 = 600 )
                                                  ( key_field = 7 char1 = 'sss' char2 = 'ttt' num1 = 70 num2 = 700 ) ) ).

    SELECT * FROM zdemo_abap_tab1 INTO TABLE @DATA(itab_modify).
    out->write( data = itab_modify name = `itab_modify` ).

*&---------------------------------------------------------------------*
*& DELETE
*&---------------------------------------------------------------------*

    "Deleting the contents of a demo database table to start with an empty database table
    DELETE FROM zdemo_abap_tab1.
    SELECT * FROM zdemo_abap_tab1 INTO TABLE @DATA(itab_delete).
    ASSERT itab_delete IS INITIAL.

    "Inserting demo data into the database table
    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 num1 = 10 )
                                                  ( key_field = 2 num1 = 20 )
                                                  ( key_field = 3 num1 = 30 )
                                                  ( key_field = 4 num1 = 40 )
                                                  ( key_field = 5 num1 = 50 )
                                                  ( key_field = 6 num1 = 60 )
                                                  ( key_field = 7 num1 = 70 )
                                                  ( key_field = 8 num1 = 80 ) ) ).

    "Rows are deleted based on a condition
    DELETE FROM zdemo_abap_tab1 WHERE key_field >= 7.

    "Deleting a single row based on entries in a structure
    "Keys are specified
    DELETE zdemo_abap_tab1 FROM @( VALUE #( key_field = 1 ) ).

    "There is no entry with key_field = 0.
    DELETE zdemo_abap_tab1 FROM @( VALUE zdemo_abap_tab1( num1 = 20 ) ).
    ASSERT sy-subrc = 4.

    "Deleting multiple rows based on entries in an internal table
    DELETE zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 2 )
                                                  ( key_field = 3 ) ) ).

    SELECT * FROM zdemo_abap_tab1 INTO TABLE @itab_delete.
    out->write( data = itab_delete name = `itab_delete` ).

*&-----------------------------------------------------------------------*
*& Exploring constructor expressions for internal tables created in place
*&-----------------------------------------------------------------------*

    "For more information about constructor expressions, see the ABAP Keyword Documentation and the
    "Constructor Expressions cheat sheet. Many additions are available. The examples show a
    "selection.
    DELETE FROM zdemo_abap_tab1.

    "--- VALUE ---
    "VALUE operator as shown above, creating an internal table in place
    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 char1 = 'aaa' char2 = 'bbb' num1 = 10 num2 = 100 )
                                                  ( key_field = 2 char1 = 'ccc' char2 = 'ddd' num1 = 20 num2 = 200 ) ) ).

    "FOR LOOP with VALUE
    DATA(it_f) = VALUE it_type( ( key_field = 3 char1 = 'ee' char2 = 'ff' num1 = 30 num2 = 300 )
                                ( key_field = 4 char1 = 'gg' char2 = 'hh' num1 = 40 num2 = 400 )
                                ( key_field = 5 char1 = 'ii' char2 = 'jj' num1 = 50 num2 = 500 ) ).

    "In the example, the internal table from above is looped across. The index value is
    "stored and used to modify field values of the internal table. In doing so, the modified
    "internal table values are inserted into the database table.
    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( FOR wa IN it_f INDEX INTO idx ( key_field = wa-key_field
                                                                                  char1 = wa-char1 && idx
                                                                                  char2 = wa-char2 && idx
                                                                                  num1 = wa-num1 + idx
                                                                                  num2 = wa-num2 + idx ) ) ).

    "Using a constructor expression with VALUE and BASE in an UPDATE statement
    "The example assumes selecting an entry from a database, modifying it, and updating it again,
    "but the non-modified entries shall remain unchanged.
    INSERT zdemo_abap_tab1 FROM @( VALUE #( key_field = 100 char1 = 'xxx' char2 = 'yyy' num1 = 100 num2 = 101 ) ).

    SELECT SINGLE * FROM zdemo_abap_tab1 WHERE key_field = 100 INTO @DATA(read_line).
    UPDATE zdemo_abap_tab1 FROM @( VALUE #( BASE read_line char2 = '#' num1 = 1 ) ).

    "--- CORRESPONDING ---
    TYPES: BEGIN OF s1,
             key_field TYPE i,
             char1     TYPE c LENGTH 5,
             num1      TYPE i,
           END OF s1,
           it_type_s1 TYPE TABLE OF s1 WITH EMPTY KEY,
           BEGIN OF s2,
             key     TYPE i,
             char    TYPE c LENGTH 5,
             number1 TYPE i,
             num2    TYPE p LENGTH 8 DECIMALS 2,
           END OF s2,
           it_type_s2 TYPE TABLE OF s2 WITH EMPTY KEY.

    "Identical component names in the internal table
    "The example includes compatible and convertible types.
    DATA(it_g) = VALUE it_type_s1( ( key_field = 6 char1 = 'kkk' num1 = 60 )
                                   ( key_field = 7 char1 = 'lll' num1 = 70 ) ).

    INSERT zdemo_abap_tab1 FROM TABLE @( CORRESPONDING #( it_g ) ).
    
    "Non-identical component names in the internal table; using the MAPPING/EXCEPT additions
    "The example includes compatible and convertible types.
    DATA(it_h) = VALUE it_type_s2( ( key = 8 char = 'mmm' number1 = 80 num2 = '1.23' )
                                   ( key = 9 char = 'nnn' number1 = 90 num2 = '4.56' ) ).

    INSERT zdemo_abap_tab1 FROM TABLE @( CORRESPONDING #( it_h MAPPING key_field = key char2 = char num1 = number1 EXCEPT num2 ) ).

    SELECT * FROM zdemo_abap_tab1 INTO TABLE @DATA(itab_constr).
    out->write( data = itab_constr name = `itab_constr` ).
  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Dynamic ABAP SQL Statements

- ABAP SQL statements offer syntax options for dynamic programming. 
- For example, you can specify the data source to read from dynamically. 
- Find more information and code snippets in the ABAP Keyword Documentation or the [ABAP cheat sheet on dynamic programming](06_Dynamic_Programming.md).

```abap
DATA(dbtab) = 'ZDEMO_ABAP_FLSCH'.

"Selecting from a dynamically specified database table.
SELECT *
  FROM (dbtab)
  WHERE ...
  INTO ...
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## CRUD Operations Using CDS Artifacts

- The code examples above primarily use DDIC database tables for [CRUD operations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencrud_glosry.htm).
- CDS artifacts are available that allow not only reading but also creating, updating, and deleting.
- **Table Entities**
  - Table entities are CDS entities that define database tables on the SAP HANA database linked to AS ABAP.
  - They are - with restrictions currently - considered successors of the classic DDIC database table, and also represent global structured types usable in ABAP.
  - Syntax: `define table entity ...`.
  - For more information, refer to the ABAP Data Models guide, for example, [here](https://help.sap.com/docs/ABAP_Cloud/aaae421481034feab3e71dd9e0f643bf/100ab51935544f18b4f4be9b4abb91e8.html).
- **Writable CDS View Entities**
  - Writable CDS view entities allow you to modify underlying database data using ABAP SQL.
  - Syntax: `define writable view entity ...`.
  - These entities, like many other CDS artifacts, represent global structured types usable in ABAP, but not in the ABAP Dictionary.
  - Note that there are definition restrictions. For example, you cannot define writable CDS view entities if selecting from multiple data sources or using joins.
  - Writable CDS external entities are available, too. They allow CRUD operations using external artifacts via service connections. Find more information [here](https://help.sap.com/docs/ABAP_Cloud/aaae421481034feab3e71dd9e0f643bf/51854921f45148f8bb4157fe8e16af48.html).

**Example**

- The following example demonstrates a table entity and a writable CDS view enitity.
- The writable CDS view entity uses the table entity as its data source. A DDIC database is also possible.


<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

- The example uses a zoo animal context. To explore the example, create three ABAP repository objects: 2 CDS artifacts, 1 example class. 
- For the CDS artifacts, right-click your package and choose *New -> Other ABAP Repository Objects* in ADT. Filter for *data definition*, and walk through the wizard. Use the example's artifact names (`zdemo_abap_animals_te` for the table entity, and `zdemo_abap_animals_we` for the writable CDS view entity), or use other names and adapt the code accordingly.
- After activation, choose *F9* in the class in ADT to execute it. The example is set up to display output in the console.
- The example class performs CRUD operations on both CDS artifacts. The example includes ABAP SQL `SELECT`, `INSERT`, `MODIFY`, `UPDATE`, and `DELETE` statements.

<br>

<table>

<tr>
<td> ABAP repository object </td> <td> Notes/Code </td>
</tr>

<tr>
<td> 

Table entity

 </td>

 <td> 


```abap
@ClientHandling.type: #CLIENT_INDEPENDENT
@AbapCatalog.deliveryClass: #APPLICATION_DATA
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS table entity'
define table entity zdemo_abap_animals_te 
{
  key id            : abap.int4;
  animal_name       : abap.char(20);
  species           : abap.char(20);
  age               : abap.int4;
  country_of_origin : abap.char(20);
  arrival_date      : abap.datn;
  is_carnivore      : abap_boolean;
}
``` 

 </td>
</tr>

<tr>
<td> 

Writable CDS view entity

 </td>

 <td> 

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Writable CDS view entity'
@Metadata.ignorePropagatedAnnotations: true
define writable view entity zdemo_abap_animals_we
  as select from zdemo_abap_animals_te
{
  key id,
  animal_name,
  species,
  age,
  country_of_origin,
  arrival_date,
  is_carnivore  
}
``` 

 </td>
</tr>

<tr>
<td> 

Class

 </td>

 <td> 


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

*&---------------------------------------------------------------------*
*& Using table entities and writable CDS view entities as data types
*& in ABAP
*&---------------------------------------------------------------------*

    DATA struc_table_entity TYPE zdemo_abap_animals_te.
    TYPES struc_type_table_entity TYPE zdemo_abap_animals_te.
    DATA(another_struc_table_ent) = VALUE zdemo_abap_animals_te( id = 1 animal_name = 'Zeus' ).

    DATA struc_writable_ve TYPE zdemo_abap_animals_we.
    TYPES struc_type_writable_ve TYPE zdemo_abap_animals_we.
    DATA(another_struc_writable_ve) = VALUE zdemo_abap_animals_we( id = 2 animal_name = 'Leo' ).

*&---------------------------------------------------------------------*
*& Performing CRUD operations using a table entity and ABAP SQL
*&---------------------------------------------------------------------*

    out->write( `---- Performing CRUD operations on table entities using ABAP SQL ----` ).
    out->write( |\n| ).

    DELETE FROM zdemo_abap_animals_te.

    "DATA animals_table type table of zdemo_abap_animals_te with empty key.

    INSERT zdemo_abap_animals_te FROM TABLE @( VALUE #(
      ( id = 1 animal_name = 'Zeus' species = 'Lion' age = 5 country_of_origin = '' arrival_date = '20220115' is_carnivore = abap_true )
      ( id = 2 animal_name = 'Leo' species = 'Lion' age = 7 country_of_origin = '' arrival_date = '20241205' is_carnivore = abap_true )
      ( id = 3 animal_name = 'Jumbo' species = 'Elephant' age = 10 country_of_origin = 'India' arrival_date = '20190526' is_carnivore = abap_false )
      ( id = 4 animal_name = 'Little' species = 'Elephant' age = 4 country_of_origin = 'India' arrival_date = '20240314' is_carnivore = abap_false )
      ( id = 5 animal_name = 'Daisy' species = 'Zebra' age = 4 country_of_origin = 'South Africa' arrival_date = '20220207' is_carnivore = abap_false )
      ( id = 6 animal_name = 'Buddy' species = 'Owl' age = 3 country_of_origin = 'Canada' arrival_date = '20210111' is_carnivore = abap_true )
      ( id = 7 animal_name = 'Sunny' species = 'Dolphin' age = 7 country_of_origin = 'Australia' arrival_date = '20190527' is_carnivore = abap_false )
      ( id = 8 animal_name = 'Nala' species = 'Giraffe' age = 6 country_of_origin = 'Tanzania' arrival_date = '20210807' is_carnivore = abap_false )
      ( id = 9 animal_name = 'Oscar' species = 'Tiger' age = 8 country_of_origin = 'Russia' arrival_date = '20191018' is_carnivore = abap_true )
      ( id = 10 animal_name = 'Charlie' species = 'Chimpanzee' is_carnivore = abap_false )
      ( id = 12  ) ) ).
    out->write( |sy-dbcnt after 1st INSERT: { sy-dbcnt }| ).

    SELECT COUNT(*) FROM zdemo_abap_animals_te INTO @DATA(number_of_entries).
    out->write( |Number of data entries after 1st INSERT: { number_of_entries }| ).

    INSERT zdemo_abap_animals_te FROM TABLE @( VALUE #(
      ( id = 1 animal_name = 'Lora' species = 'Parrot' age = 2 country_of_origin = 'Mexico' arrival_date = '20240712' is_carnivore = abap_false )
      ( id = 11 animal_name = 'Teddy' species = 'Koala' age = 4 country_of_origin = 'Australia' arrival_date = '20240225' is_carnivore = abap_false ) ) )
      ACCEPTING DUPLICATE KEYS.
    out->write( |sy-dbcnt after 2nd INSERT: { sy-dbcnt }| ).

    SELECT COUNT(*) FROM zdemo_abap_animals_te INTO @number_of_entries.
    out->write( |Number of data entries after 1st INSERT: { number_of_entries }| ).

    MODIFY zdemo_abap_animals_te FROM TABLE @( VALUE #(
      ( id = 12 animal_name = 'Peanut' species = 'Penguin' age = 6 country_of_origin = 'Antarctica' arrival_date = '20200410' is_carnivore = abap_false )
      ( id = 13 animal_name = 'Lora' species = 'Parrot' age = 2 country_of_origin = 'Mexico' arrival_date = '20240712' is_carnivore = abap_false )
      ( id = 14 animal_name = 'Jumpy' species = 'Kangaroo' age = 4 country_of_origin = 'Australia' arrival_date = '20211117' is_carnivore = abap_false ) ) ).
    out->write( |sy-dbcnt after MODIFY: { sy-dbcnt }| ).

    SELECT COUNT(*) FROM zdemo_abap_animals_te INTO @number_of_entries.
    out->write( |Number of data entries after MODIFY: { number_of_entries }| ).

    SELECT SINGLE * FROM zdemo_abap_animals_te WHERE id = 10 INTO @DATA(data_set).
    UPDATE zdemo_abap_animals_te FROM @( VALUE #( BASE data_set age = 3 country_of_origin = 'Uganda' arrival_date = '20231214' ) ).
    out->write( |sy-dbcnt after 1st UPDATE: { sy-dbcnt }| ).

    UPDATE zdemo_abap_animals_te SET country_of_origin = 'Kenya' WHERE country_of_origin IS INITIAL.
    out->write( |sy-dbcnt after 2nd UPDATE: { sy-dbcnt }| ).

    SELECT * FROM zdemo_abap_animals_te ORDER BY id INTO TABLE @DATA(itab_te).
    out->write( `Table entries retrieved from table entity:` ).
    out->write( itab_te ).

    DELETE FROM zdemo_abap_animals_te WHERE id > 10.
    out->write( |sy-dbcnt after DELETE: { sy-dbcnt }| ).

    SELECT COUNT(*) FROM zdemo_abap_animals_te INTO @number_of_entries.
    out->write( |Number of data entries after DELETE: { number_of_entries }| ).

    out->write( |\n| ).
    out->write( repeat( val = '_' occ = 100 ) ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Performing CRUD operations using a writable CDS view entity and ABAP SQL
*&---------------------------------------------------------------------*

    out->write( `---- Performing CRUD operations on a writable CDS view entity using ABAP SQL ----` ).
    out->write( |\n| ).

    SELECT COUNT(*) FROM zdemo_abap_animals_we INTO @DATA(we_number_of_entries).
    out->write( |Number of data entries accessed via writable CDS view entity: { we_number_of_entries }| ).

    INSERT zdemo_abap_animals_we FROM TABLE @( VALUE #(
      ( id = 15 )
      ( id = 16 animal_name = 'Fuzz' species = 'Polar bear' ) ) ).
    out->write( |sy-dbcnt after INSERT: { sy-dbcnt }| ).

    SELECT COUNT(*) FROM zdemo_abap_animals_we INTO @we_number_of_entries.
    out->write( |Number of data entries after INSERT: { we_number_of_entries }| ).

    MODIFY zdemo_abap_animals_we FROM @(
      VALUE #( id = 15 animal_name = 'Hunter' species = 'Jaguar' age = 3 country_of_origin = 'Brazil' arrival_date = '20230423' is_carnivore = abap_true ) ).
    out->write( |sy-dbcnt after MODIFY: { sy-dbcnt }| ).

    SELECT SINGLE * FROM zdemo_abap_animals_we WHERE id = 16 INTO @DATA(entry).
    UPDATE zdemo_abap_animals_we FROM @(
      VALUE #( BASE entry animal_name = 'Fuzzy' age = 7 country_of_origin = 'Canada' arrival_date = '20190223' is_carnivore = abap_true ) ).
    out->write( |sy-dbcnt after UPDATE: { sy-dbcnt }| ).

    DELETE zdemo_abap_animals_we FROM @( VALUE #( id = 10 ) ).
    out->write( |sy-dbcnt after 1st DELETE: { sy-dbcnt }| ).

    SELECT COUNT(*) FROM zdemo_abap_animals_we INTO @we_number_of_entries.
    out->write( |Number of data entries after 1st DELETE: { we_number_of_entries }| ).

    DELETE FROM zdemo_abap_animals_we WHERE id <= 5.
    out->write( |sy-dbcnt after 2nd DELETE: { sy-dbcnt }| ).

    SELECT COUNT(*) FROM zdemo_abap_animals_we INTO @we_number_of_entries.
    out->write( |Number of data entries after 2nd DELETE: { we_number_of_entries }| ).

    SELECT * FROM zdemo_abap_animals_we ORDER BY id INTO TABLE @DATA(itab_we).
    out->write( `Table entries retrieved via writable CDS view entity:` ).
    out->write( itab_we ).
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

</table>


</details>  


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions

### Evaluating ABAP System Fields after ABAP SQL Statements

As mentioned in several sections above, ABAP system fields such as `sy-subrc` and `sy-dbcnt` are set in the context of ABAP SQL statements, which can be evaluated, e.g. using a subsequent statement `IF sy-subrc = ...`. The following table shows a selection of values.

| Statement  | sy-subrc  | sy-dbcnt  |
|---|---|---|
| `SELECT`  | *0*: Values were passed successfully to a target data object <br><br> *4*: Result set is empty. Typically, this means that there are no entries found (matching conditions specified).  | The number of rows that were passed. |
| `INSERT`  | *0*: Single row (work area specified) or all rows (internal table specified) inserted <br><br> *4*: Row not or not all rows inserted (in case of multiple rows to be inserted, `ACCEPTING DUPLICATE KEYS` was specified) | The number of rows that were inserted.  |
| `MODIFY`  | *0*: Single row (work area specified) or all rows (internal table specified) inserted or modified <br><br> *4*: Row not or not all rows inserted or modified  | The number of rows that were processed.  |
| `UPDATE`  | *0*: Single row (work area specified) or all rows updated <br><br> *4*: Row not or not all rows updated | The number of rows that were updated.  |
| `DELETE`  | `DELETE FROM target` variant: <br> *0*: At least one row deleted (with `WHERE` clause specified), all or n rows deleted (without `WHERE` clause specified) <br><br> *4*: No row deleted <br><br>`DELETE target FROM` variant: <br> *0*: Row deleted (work area specified), all rows deleted (internal table specified) <br><br> *4*: Row not deleted (work area specified), not all specified rows deleted (internal table specified) | The number of rows that were deleted. |

The following example explores the setting of `sy-subrc` and `sy-dbcnt` by ABAP SQL statements using a demo database table from the cheat sheet repository.

```abap
"Clearing a demo database table
DELETE FROM zdemo_abap_tab1.

*&---------------------------------------------------------------------*
*& INSERT
*&---------------------------------------------------------------------*

INSERT zdemo_abap_tab1 FROM @( VALUE #( key_field = 1 ) ).

ASSERT sy-subrc = 0.
ASSERT sy-dbcnt = 1.

INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 2 )
                                              ( key_field = 3 ) ) ).

ASSERT sy-subrc = 0.
ASSERT sy-dbcnt = 2.

INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 2 )
                                              ( key_field = 3 ) ) ) ACCEPTING DUPLICATE KEYS.

ASSERT sy-subrc = 4.
ASSERT sy-dbcnt = 0.

INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 3 )
                                              ( key_field = 4 ) ) ) ACCEPTING DUPLICATE KEYS.

ASSERT sy-subrc = 4.
ASSERT sy-dbcnt = 1.

*&---------------------------------------------------------------------*
*& UPDATE
*&---------------------------------------------------------------------*

UPDATE zdemo_abap_tab1 FROM @( VALUE #( key_field = 1 num1 = 1 ) ).

ASSERT sy-subrc = 0.
ASSERT sy-dbcnt = 1.

UPDATE zdemo_abap_tab1 FROM @( VALUE #( key_field = 9999 num1 = 9999 ) ).

ASSERT sy-subrc = 4.
ASSERT sy-dbcnt = 0.

UPDATE zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 2 num1 = 2 )
                                              ( key_field = 3 num1 = 3 ) ) ).

ASSERT sy-subrc = 0.
ASSERT sy-dbcnt = 2.

UPDATE zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 4 num1 = 4 )
                                              ( key_field = 1111 num1 = 1111 ) ) ).

ASSERT sy-subrc = 4.
ASSERT sy-dbcnt = 1.

*&---------------------------------------------------------------------*
*& MODIFY
*&---------------------------------------------------------------------*

MODIFY zdemo_abap_tab1 FROM @( VALUE #( key_field = 1 num1 = 11 ) ).

ASSERT sy-subrc = 0.
ASSERT sy-dbcnt = 1.

MODIFY zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 2 num1 = 22 )     "Entry modified
                                              ( key_field = 5 num1 = 5 ) ) ). "Entry inserted

ASSERT sy-subrc = 0.
ASSERT sy-dbcnt = 2.

*&---------------------------------------------------------------------*
*& SELECT
*&---------------------------------------------------------------------*

SELECT *
  FROM zdemo_abap_tab1
  INTO TABLE @DATA(itab).

ASSERT sy-subrc = 0.
ASSERT sy-dbcnt = 5.
ASSERT sy-dbcnt = lines( itab ).

SELECT *
      FROM zdemo_abap_tab1
      WHERE key_field <= 3
      INTO TABLE @DATA(itab2).

ASSERT sy-subrc = 0.
ASSERT sy-dbcnt = 3.

SELECT *
  FROM zdemo_abap_tab1
  WHERE key_field > 10
  INTO TABLE @DATA(itab3).

ASSERT sy-subrc = 4.
ASSERT sy-dbcnt = 0.

*&---------------------------------------------------------------------*
*& DELETE
*&---------------------------------------------------------------------*

DELETE zdemo_abap_tab1 FROM @( VALUE #( key_field = 1 ) ).
ASSERT sy-subrc = 0.
ASSERT sy-dbcnt = 1.

DELETE zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 ) "Entry not existent
                                              ( key_field = 2 )
                                              ( key_field = 3 ) ) ).

ASSERT sy-subrc = 4.
ASSERT sy-dbcnt = 2.

DELETE FROM zdemo_abap_tab1 WHERE key_field >= 5.
ASSERT sy-subrc = 0.
ASSERT sy-dbcnt = 1.

"Only one entry left in the database table
DELETE FROM zdemo_abap_tab1.
ASSERT sy-subrc = 0.
ASSERT sy-dbcnt = 1.

SELECT *
  FROM zdemo_abap_tab1
  INTO TABLE @DATA(itab4).

ASSERT sy-subrc = 4.
ASSERT sy-dbcnt = 0.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Typed Literals
- [Built-in DDIC types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_ddic_type_glosry.htm) cannot be used directly in ABAP, e.g. for typing local data objects. 
- However, the types can be used in ABAP SQL, and also ABAP CDS, in the context of [typed literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentyped_literal_glosry.htm). Note that some special types cannot be used in this context.
- Advantages of typed literals over untyped literals:
  - Allow type-safe use of literals
  - Eliminate the need for (implicit type) conversions and casts, which can lead to surprising or erroneous results. Also consider the conversion costs in terms of performance (typed literals are passed to the database and evaluated there without ABAP-specific type conversions).
  - For better readability (you can immediately see what type is being used)
- More information:
  - [Typed literals in ABAP SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_typed_literals.htm)
  - [Typed literals in ABAP CDS](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_typed_literal_v2.htm)
  - They can also be used in casts in ABAP [SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_cast.htm) and [CDS](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_cast_expression_v2.htm).
  
```abap
"Miscellaneous typed literals in an ABAP SQL statement
"Note that typed literals can be specified in read
"positions where host variables are possible.
DATA(tmstamp) = CONV timestamp( '20240808112517' ).
DATA(some_string) = `Some string`.
SELECT SINGLE
  FROM zdemo_abap_fli
  FIELDS
    carrid,
    @some_string AS host_var,
    char`X` AS flag,
    int8`32984723948723` AS int8,
    raw`11` AS raw,
    numc`1234` AS numc,
    utclong`2024-01-01T10:01:02,2` AS utc,
    tims`101507` AS tims,
    curr`173.95` AS curr,
    "Multiple cast expressions splitting a time stamp into date and time parts
    CAST( CAST( div( @tmstamp, 1000000 ) AS CHAR ) AS DATS ) AS date,
    CAST( substring( CAST( @tmstamp AS CHAR ), 9, 6 ) AS TIMS ) AS time,
    "Untyped literal
    'ABAP' AS txt
  WHERE fldate = datn`20240102`
  INTO @DATA(misc_typed_literals).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### ABAP SQL and Client Handling

> [!IMPORTANT] 
> - ABAP SQL features implicit [client handling](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclient_handling_glosry.htm). 
> - While you can disable this in classic ABAP, it is not an option in ABAP Cloud, where you are limited to accessing your own client. 
> - Unlike ABAP SQL, there is no implicit client handling in Native SQL. AMDP, which uses Native SQL, is usable in ABAP Cloud, but it is crucial to ensure that AMDP only accesses client-safe repository objects, meaning it only accesses data from your own client. For this purpose, dedicated additions are provided.
  
**Notes on ABAP SQL:**
- An SAP system can have multiple clients, each distinguished by a unique [client ID](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclient_identifier_glosry.htm). 
- Each client can contain unique data. For instance, when you log into the system, you select a client and can only access the data within that specific client. If you log into client 000, for example, your access is restricted to the data of client 000, not any other clients.
  - Client-dependent data is managed through a client column. So, if a database table has a client column, it contains client-dependent data. However, there are also client-independent data sources, which contain data not specific to any client. These are typically accessed by [system programs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensystem_program_glosry.htm).
- ABAP SQL features implicit client handling, i.e. it automatically handles client selection. When you execute an ABAP SQL statement in a client, the system automatically uses the current client. There is no need to specify the client separately, as the compiler automatically manages client handling.
- See the following example using a SELECT statement selecting from a database table.
    ```abap
    "Using the current client implicitly
    SELECT * FROM zdemo_abap_carr INTO TABLE @DATA(itab1).

    "For example, it is not possible to specify the client explicitly in a WHERE clause.
    "The following statement shows a syntax error.
    "SELECT * FROM zdemo_abap_carr WHERE mandt = '123' INTO TABLE @DATA(itab2).
    ```  

- However, in classic ABAP, not in ABAP Cloud, you can use the [`USING CLIENT` (F1 docu for Standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselect_client.htm) addition that modifies this default behavior. It disables implicit client handling.
  - With the addition, you can specify other clients from which to retrieve data. Find more information in the ABAP Keyword Documentation.
  - You may also stumble on the `CLIENT SPECIFIED` addition. It is obsolete, only `USING CLIENT` should be used.  
  - The following (classic ABAP only) code shows a variety of syntax options. Note that it uses a data source of the cheat sheet repository. For exploration, you may want to use another data source. You may also want to check the commands that are passed by the [ABAP SQL Interface](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_interface_glosry.htm) when activating the SQL trace (transaction ST05) to explore the use of another client ID and more. 

    ```abap
    "---- The following code is for classic ABAP only. ----

    "SQL SELECT statements turning off the implicit client handling, and
    "specifying different clients to retrieve data from

    "Literal containing a client ID
    SELECT * FROM zdemo_abap_carr USING CLIENT '000' INTO TABLE @DATA(itab3).

    "Data object containing a client ID (must be of type c length 3)
    "Note: If the client does not exist, the result set is empty.
    TYPES c3 LENGTH 3.
    DATA client_id TYPE c3 VALUE '000'.
    SELECT * FROM zdemo_abap_carr USING CLIENT @client_id INTO TABLE @DATA(itab4).

    "Selecting from multiple clients
    "Using a ranges table
    DATA client_range TYPE RANGE OF c3.
    client_range = VALUE #( ( sign = 'I' option = 'EQ' low = '000' )
                            ( sign = 'I' option = 'BT' low = '100' high = '200' ) ).

    SELECT * FROM zdemo_abap_carr USING CLIENTS IN @client_range INTO TABLE @DATA(itab5).
    "Selecting data of all clients whose client IDs are in the column MANDT of the
    "system table T000
    SELECT * FROM zdemo_abap_carr USING CLIENTS IN t000 INTO TABLE @DATA(itab6).
    "Selecting data of all clients regardless of the content of the client column
    SELECT * FROM zdemo_abap_carr USING ALL CLIENTS INTO TABLE @DATA(itab7).
    ``` 

- Find more information on client handling [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_client_handling.htm).

**Notes on Native SQL/AMDP**
- Unlike ABAP SQL, Native SQL does not have implicit handling, so you must explicitly pass the client. 
- Native SQL is passed directly to the database.
- AMDP, which uses Native SQL, also does not support implicit client handling.
- While AMDP is permitted in ABAP Cloud, accessing client-dependent data via Native SQL is not supported.
- When using AMDP in ABAP Cloud, it is crucial to access only the current client. Client-safety must be ensured.
- AMDP methods in ABAP Cloud must be client-safe, meaning the SQLScript code should access data only in your client. Use only artifacts that limit access to a single client or those that are client-independent.
- Consequently, all objects in the `USING` list must be client-safe, including CDS table functions implemented as AMDP methods.
- There are additions to cover client-safe aspects, ensuring access only to your client data.
- Find more information about client safety in AMDP [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp_client_safety.htm).

**Notes on CDS view entities**
- In CDS view entities, client handling is done implicitly and automatically.
- The client dependency is determined by the data sources used. If at least one source is client-dependent, then the entire entity is client-dependent.
- More information: 
  - [Client Handling in CDS View Entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_v2_view_client_handling.htm)
  - [Client Safety of CDS View Entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_v2_view_client_safety.htm)
  - [Client Handling in CDS Table Functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_func_client_handling.htm)

<p align="right"><a href="#top">⬆️ back to top</a></p>

### RAP-Specific ABAP SQL Variants

There are [RAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenarap_glosry.htm)-specific variants of ABAP SQL statements that use the `MAPPING FROM ENTITY` addition. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmapping_from_entity.htm) and in the [ABAP for RAP: Entity Manipulation Language (ABAP EML)](08_EML_ABAP_for_RAP.md#abap-sql-statements-with-bdef-derived-types) cheat sheet. 

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information
- [This topic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql.htm) serves as the entry point for topics about ABAP SQL in the ABAP Keyword Documentation. For the full details, check the subtopics there, especially topics not covered in this cheat sheet.
- Find more topics in the ABAP Keyword Documentation such as the following: 
  - ABAP SQL statements can contain [SQL path expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_path_expression_glosry.htm). For more information, see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_path.htm). The executable example of the CDS view entities cheat sheet includes demo SQL statements.
  - Find [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_exceptions.htm) and overview on exceptions that can occur in the context of ABAP SQL statements.
  - As a rule, bear in mind performance aspects when using ABAP SQL statements. Find more information in the [Performance Notes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_perfo.htm). The code snippets here only focus on syntax options.
  - You can specify hierarchy data as a data source in ABAP SQL `SELECT` statements. Find more information and examples in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_data.htm). For working with hierarchies, see the [ABAP SQL: Working with Hierarchies cheat sheet](10_ABAP_SQL_Hierarchies.md).

## Executable Example
[zcl_demo_abap_sql](./src/zcl_demo_abap_sql.clas.abap)

> [!NOTE] 
> - The executable example covers the following topics, among others:
>     - Reading from database tables using `SELECT`
>     - Various additions to `SELECT` statements
>     - Changing data in database tables using `INSERT`, `UPDATE`, `MODIFY` and `DELETE`
>     - Excursions: Operands and expressions in ABAP SQL statements
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)