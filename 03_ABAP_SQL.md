<a name="top"></a>

# ABAP SQL

- [ABAP SQL](#abap-sql)
  - [Introduction](#introduction)
  - [Reading Data Using SELECT](#reading-data-using-select)
    - [Basic Syntax](#basic-syntax)
    - [Using SELECT for Multiple Purposes](#using-select-for-multiple-purposes)
    - [Clause Variations and Additions in SELECT Statements](#clause-variations-and-additions-in-select-statements)
    - [More Clauses](#more-clauses)
    - [Operands and Expressions in ABAP SQL Statements](#operands-and-expressions-in-abap-sql-statements)
      - [SQL operands](#sql-operands)
      - [SQL Expressions](#sql-expressions)
        - [Elementary Expressions](#elementary-expressions)
        - [SQL Functions](#sql-functions)
        - [More SQL Expressions](#more-sql-expressions)
        - [Window Expressions](#window-expressions)
    - [SQL Conditions](#sql-conditions)
    - [Selecting Data by Evaluating the Content of Other Tables](#selecting-data-by-evaluating-the-content-of-other-tables)
    - [Combining Data of Multiple Database Tables](#combining-data-of-multiple-database-tables)
      - [Common Table Expressions (CTE)](#common-table-expressions-cte)
  - [Changing Data in Database Tables](#changing-data-in-database-tables)
    - [Using `INSERT`](#using-insert)
    - [Using `UPDATE`](#using-update)
    - [Using `MODIFY`](#using-modify)
    - [Using `DELETE`](#using-delete)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)

## Introduction

-   [ABAP SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_glosry.htm) is a subset of [SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_glosry.htm "Glossary Entry")
    which is the standardized language for accessing databases.
-   The main ABAP SQL keywords to read and change data are the
    following:

    | Keyword  | Purpose                                                                   |
    | -------- | ------------------------------------------------------------------------- |
    | `SELECT` | Reads data from database tables                                           |
    | `INSERT` | Adds rows to database tables                                              |
    | `UPDATE` | Changes the content of rows of database tables                            |
    | `MODIFY` | Inserts rows into database tables or changes the content of existing rows |
    | `DELETE` | Deletes rows from database tables                                         |

- ABAP SQL statements use the ABAP SQL interface. This interface transforms all ABAP SQL statements that access the standard database of an AS ABAP to  platform-dependent SQL and forwards the results to the database system.
- Generally bear in mind the [performance notes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_perfo.htm) when using
    ABAP SQL. The considerations there are not relevant for this cheat sheet since
    the focus is on syntactical options.


<details>
  <summary>Excursion: Database Tables and Views</summary>
<br>

This section provides bullet points on database tables and views which contain persisted data. Note that the code snippets in this cheat sheet focus on database tables as [data source](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_source_glosry.htm "Glossary Entry") for ABAP SQL statements.

**Database tables in [AS ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenas_abap_glosry.htm "Glossary Entry") ...**

- are objects of the [ABAP Dictionary (DDIC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_dictionary_glosry.htm "Glossary Entry"). The term *database table* describes a physical database table in the current [standard database](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_db_glosry.htm).
- are two-dimensional matrices consisting of rows and columns.
- contain a [table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_key_glosry.htm), i. e. a field or a combination of fields uniquely identifies every row in a table. A [primary key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprimary_key_glosry.htm) must exist for every database table.
  - Note the concept of [foreign keys](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenforeign_key_glosry.htm) in which one or more columns of a database table can be primary keys of another table. See more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_database_tables_forkey.htm).
- have a [flat structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenflat_structure_glosry.htm "Glossary Entry")
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

- are the oldest form of views and are not available in SAP BTP ABAP environments.
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
</details>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Reading Data Using SELECT

### Basic Syntax

You use ABAP SQL `SELECT` statements to read data from one or more database tables (or views). This can be done to create a multirow or single row result set by assigning the result set to a suitable data object, i. e. you can store the multirow read result in an internal table or the single row result in a structure.
The `SELECT` statement includes several clauses that serve
different purposes. The following code snippet shows the basic syntax:
``` abap
SELECT FROM source   "What database table or view to read from
  FIELDS field_list  "What columns should be read
  WHERE condition    "Specifies conditions on which a row/rows should be read
  INTO @target.      "Data object to which the result set is assigned (preceded by @)
```
> **💡 Note**<br>
>-   There are further clauses available of which some are dealt with
    further down. In general, the recommendation is to hit `F1` for the keywords and additions to get all the details in the ABAP Keyword Documentation.
>-   Especially in older ABAP programs, you will see other forms of the
    `SELECT` syntax that you should no longer use. Strict syntax check modes might enforce the use
    of specific ABAP SQL syntax. For example, the `INTO` clause should be placed after the other clauses. Furthermore, [host variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhost_variable_glosry.htm "Glossary Entry") or [host expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhost_expression_glosry.htm "Glossary Entry") are required for data objects and expressions, i. e. they must be preceded by `@` or `@( ... )`. Further information: [Release-Dependent Syntax Check Modes (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_sql_strict_modes.htm).
>- Regarding host variables as in `SELECT ... INTO @target.` and since they are used in most examples below: A host variable is a data object that is
>     - declared in the ABAP program
>     - prefixed with the `@` character and
>     - specified in an [operand position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm) of an ABAP SQL statement.
> See more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhost_variable_glosry.htm).
>-  The `SELECT` list, i. e. the fields that are specified, can also be specified following the `SELECT`
    keyword before the `FROM` clause - without `FIELDS`. The
    following two `SELECT` statements are basically the same but differently arranged:
>      ``` abap
>      SELECT FROM dbtab
>        FIELDS comp1, comp2, comp3
>         ...
>
>       SELECT comp1, comp2, comp3
>         FROM dbtab
>         ...
>      ```
>-   Regarding the target into which data is read: Instead of using a
    variable that is (extra) declared beforehand, you can also make use
    of [inline
    declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry"),
    for example `... INTO TABLE @DATA(itab).`, to comfortably
    create an appropriate variable in place. Note that in case of
    internal tables as targets, the resulting table is a standard table
    and has an empty key which might have an impact when further
    processing the internal table entries. Find more information in the
    ABAP cheat sheet [Internal Tables](01_Internal_Tables.md). In newer ABAP releases, the declaration operator [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm) can be used to declare immutable variables.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using SELECT for Multiple Purposes

**Reading a single row into a structure**
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

"Reading a selected set of fields of a single row

SELECT SINGLE FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO @DATA(struc2).    "Structure declared inline

"Alternative syntax without the FIELDS addition
"Here, the CORRESPONDING FIELDS OF addition is used. Only the content of
"columns that have identically named components in the target data object
"is assigned.

SELECT SINGLE comp1, comp2, comp3       "Selected set of fields
  FROM dbtab
  WHERE ...
  INTO CORRESPONDING FIELDS OF @struc.  "Existing structure
```
> **💡 Note**<br>
>-  Although its use is optional, a `WHERE` clause should be specified to further restrict the read result.
>-  Regarding the addition `CORRESPONDING FIELDS OF` in the `INTO`
    clause: As mentioned, only the content of columns for which there are identically named components in the target are assigned. However, if you want to read data into an existing data object and particular fields are specified in the `SELECT` list and if the addition is **not** specified, you might stumble on undesired results. The target data object must contain enough components and the content of the columns are assigned to the components of the target from left to right in the order specified after `SELECT`. The content of surplus components of the target is not changed. Plus, pay attention to [assignment rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_into_conversion.htm). Basic rule: Without `CORRESPONDING ...`, column names do not play a role but only the position. With `CORRESPONDING ...`, the position of the columns does not play a role but only the name.
>-   Find more information regarding the addition `INTO` [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinto_clause.htm).

**Reading multiple rows into an internal table**.
``` abap
SELECT FROM dbtab
  FIELDS *              "All fields
  WHERE ...
  INTO TABLE @itab.     "itab has an appropriate row type

"Alternative syntax without the FIELDS addition

SELECT comp1, comp2, comp3          "Selected set of fields
  FROM dbtab
  WHERE ...
  INTO TABLE @DATA(lv_itab).        "Internal table declared inline

"Selected set of fields, existing variable
"See the note on CORRESPONDING FIELDS OF above

SELECT FROM dbtab
  FIELDS comp1, comp2, comp3                "Selected set of fields
  WHERE ...
  INTO CORRESPONDING FIELDS OF TABLE @itab.
```

**`SELECT` loop: Sequentially reading multiple rows**.
- A `SELECT` loop can be opened if the assignment is made to a structure and the addition `SINGLE` is not used.
- If the row is found, the system field `sy-subrc` is set to `0`.
- The loop must be closed using `ENDSELECT`.
- To terminate the loop completely, you can use the statement [`EXIT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapexit_loop.htm).
- Note: As touched on further down, when using the addition `PACKAGE SIZE` and storing the result in a table, a loop is opened, too.

``` abap
SELECT FROM dbtab
  FIELDS *
  WHERE ...
  INTO @struc.

    IF sy-subrc = 0.
      ...  "For example, making changes on data and adding the row to an internal table.

    ENDIF.

ENDSELECT.
```
<p align="right"><a href="#top">⬆️ back to top</a></p>

### Clause Variations and Additions in SELECT Statements

`SELECT`/`FROM` clauses:

**Checking the existence of a row in a database table**
``` abap
"Instead of @abap_true, you could also use 'X'.

SELECT SINGLE @abap_true
  FROM dbtab
  WHERE ...
  INTO @DATA(exists).

IF exists = abap_true.
  ...
ENDIF.
```

**Removing rows that occur more than once in a multirow result set** using the `DISTINCT` addition.
- Cannot be used with the addition `SINGLE`.
- See more information here [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_clause.htm).
``` abap
SELECT DISTINCT comp1
  FROM dbtab
  WHERE ...
  INTO TABLE @itab.
```

**SELECT list variants** (some of them are already outlined above)

The following specifications can also be combined:
- `SELECT * ...`: As outlined above, the `*` character defines all columns to be read from a data source (in the order specified there).
- `SELECT col1, col2, col3 ...`: A comma-separated list of individual column names.
- `SELECT data_source~col1, data_source~col2, data_source~col3 ...`: A comma-separated list of individual column names. Here, the name of the data source is explicitly specified and precedes the column name, separated by a tilde.
- `SELECT data_source~* ...`: In this case, the name of the data source is followed by a tilde and the `*` character to specify all columns. Note that there are [special conditions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_list.htm#!ABAP_VARIANT_1@1@) when using this variant.
- `SELECT col1 AS al1, col2 AS al2, col3 AS al3 ...`:
  - Defining alias names for individual columns of the result set with [`AS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_list.htm).
  - Make sure that you use an alias name only once here. In the statement, the alias name can only be used after an `ORDER BY` clause.
  - As shown further down, in some cases (e. g. when using SQL expressions) the specification of an alias name is required. Setting an alias name for the data source is also possible (`SELECT FROM dbtab AS alias_name ...`). See the section on joins further down.

> **💡 Note**<br>
> You have plenty of options regarding the specification of the columns in the `SELECT` list, among them, the outlined direct specification of the column name. SQL expressions can be specified, too. See more details [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_clause_col_spec.htm) and in the sections on SQL expressions further down.


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

SELECT FROM dbtab
  FIELDS comp1 AS comp_a, comp2 AS comp_b, comp3 AS comp_c
  WHERE ...
  INTO CORRESPONDING FIELDS OF TABLE @itab.

"Alias name also possible for the data source
SELECT ds~col1, ds~col2, ds~col3
  FROM dbtab AS ds
  WHERE ...
  INTO ...
```

**Reading data from a database table in another client** (not available in SAP BTP ABAP environments). Note that there are several variants of the `USING ...` addition for switching the [implicit client handling (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_sql_client_handling.htm) from the current client to other clients. See more information [here (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselect_client.htm).
``` abap
"Some examples; not available in SAP BTP ABAP environments

"Replaces the current client with the specified client
SELECT *
  FROM dbtab USING CLIENT '000'            
  WHERE ...
  INTO TABLE @itab.

"Selects data of any number of clients
SELECT *
  FROM dbtab USING ALL CLIENTS
  WHERE ...
  INTO TABLE @itab.
```

**Reading data from an internal table as data source** using `SELECT`. Note that an alias name must be specified for the internal table used as data source.
Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_itab.htm).

``` abap
SELECT *
  FROM @itab1 AS tab
  WHERE ...
  INTO TABLE @DATA(itab2).
```


`INTO` **clause**:

**Limiting the number of returned table rows** using the optional addition [`UP TO n
ROWS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_up_to_offset.htm).

``` abap
"A maximum of five rows are to be returned
"If the INTO clause is the last clause, the UP TO clause must be positioned after it.
SELECT * FROM dbtab
  WHERE ...
  INTO TABLE @DATA(itab_upto)
  UP TO 5 ROWS.
```

**Returning only the table rows after a row with a specified count from the result set** using the optional addition [`OFFSET n`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_up_to_offset.htm#!ABAP_ADDITION_2@2@). You can only use the addition, if an `ORDER BY` clause is specified.

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

**Reading into individual elementary data objects**.
Apart from reading into structures and internal tables outlined above, you can also read into individual elementary data objects.
Here, the individual elementary data objects as target objects are specified in a comma-separated list (e. g. as existing host variables or declared inline with `@DATA(...)`) and put between a pair of parentheses.
Note:
- The comma-separated list must have the same number of elements as columns in the result set.
- The content of the columns in the result set is assigned to the data objects specified in the list from left to right in accordance with the order specified in the `SELECT` list.
- Note the [assignment rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_into_conversion.htm) also in this context.
- More information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinto_clause.htm#!ABAP_ALTERNATIVE_1@1@).


``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO (@res1,@res2,@res3).
  "INTO (@DATA(res1),@DATA(res2),@DATA(res3)). "Using inline declarations
```

**Appending the result set to an existing internal table**.
The addition `INTO` initializes the target object. When using the addition `APPENDING`, you can retain existing lines in internal tables. `APPENDING` is also possible with the addition `CORRESPONDING FIELDS OF TABLE`.

``` abap
SELECT * FROM dbtab
  WHERE ...
  APPENDING TABLE @itab.

SELECT * FROM dbtab
  WHERE ...
  APPENDING CORRESPONDING FIELDS OF TABLE @diff_itab.
```

**Reading into packages of a specified number of rows** when reading into internal tables. The addition [`PACKAGE SIZE n`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinto_clause.htm#!ABAP_ONE_ADD@1@) can be specified after `INTO TABLE` and `APPENDING TABLE`. A `SELECT` loop ist opened. After `PACKAGE SIZE`, the number of rows is specified (which can be a host variable, host expression or a literal of type `i`) denoting the number of rows to be inserted in the target object per iteration.
``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO TABLE @DATA(itab_pack) PACKAGE SIZE n.
...
ENDSELECT.
```

**Specifying an [anonymous data object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm) as target object** using the addition [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_into_target.htm#!ABAP_ALTERNATIVE_3@3@). Only to be used after `INTO` and not `APPENDING`.

``` abap
"Here, the target object is an anonymous data object declared inline.
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO TABLE NEW @DATA(dref).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### More Clauses

[`GROUP BY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapgroupby_clause.htm)
clause: Combining groups of table rows in the result set. You
might also use [SQL expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_expression_glosry.htm "Glossary Entry")
here. Multiple clause elements are separated by a comma. Find more
information on SQL expressions further down.

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
``` abap
SELECT FROM dbtab
  FIELDS comp1, MIN( comp2 ) AS min, MAX( comp2 ) AS max
  WHERE ...
  GROUP BY comp1
  INTO ...
```

[`HAVING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaphaving_clause.htm)
clause: Limiting the number of table rows in groups in the
result by setting conditions on these rows. The rows for which a
logical expression is true are inserted in the target variable. Note
that `HAVING` can only be used together with `GROUP BY`.
``` abap
SELECT FROM dbtab
  FIELDS comp1, MIN( comp2 ) AS min, MAX( comp3 ) AS max
  WHERE ...
  GROUP BY comp1
  HAVING comp1 LIKE '%XYZ%' AND SUM( comp4 ) > 100
  INTO ...
```

[`ORDER BY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaporderby_clause.htm)
clause: Sorting the result set by specified columns.

The following example shows the ordering of the result set based on the
content of the primary key of the [data
source](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_source_glosry.htm "Glossary Entry").
You can also order by any columns and by explicitly specifying the sort
order. There are more ordering options, for example, by using SQL
expressions.
``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  ORDER BY PRIMARY KEY
           "comp2 ASCENDING
           "comp2 DESCENDING
  INTO ...
```

> **💡 Note**<br>
>- Not specifying `ORDER BY` means that the order of entries in the result set is undefined.
>- If `ORDER BY` and `GROUP BY` clauses are used, all columns specified after `ORDER BY` must also be specified after `GROUP BY`.
>- If aggregate functions are specified after `SELECT`, all columns that are specified after `ORDER BY` and that do not have an alias name for an aggregate function must also be specified after `SELECT` and after the `GROUP BY` clause which is required in this case, too.

[`WHERE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapwhere.htm) clause: Restricts the number of rows that are included in the result set using logical expressions. See further information on them in the following sections.
``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE comp1 = 'abc'
    AND comp2 < 123    
  INTO ...
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Operands and Expressions in ABAP SQL Statements

ABAP offers plenty of [SQL
operands](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_operand_glosry.htm "Glossary Entry")
and
[expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_expression_glosry.htm "Glossary Entry")
that are possible in ABAP SQL statements, not only in the context of
`SELECT` statements and the `SELECT` lists which are
mainly used for the following demonstration examples. Questions about
when to use what, what is possible in which contexts and positions, is
beyond the scope of this cheat sheet. Check the details in the
respective topics in the ABAP Keyword Documentation. Find a general
overview of important operand positions in ABAP SQL
[here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_operand_positions_oview.htm).
Due to the rich variety of options, the cheat sheet covers a selection.

#### SQL operands

-   Are elementary operands in an ABAP SQL statement
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
        or untyped. Typed literals are preferable for the following
        reasons: Using untyped literals means extra cost in terms of
        performance since they must be converted by the compiler. Plus,
        their use can result in errors at runtime whereas typed literals
        guarantee type compatibility at once.
    -   Regarding host expressions: Structures and internal tables are
        possible as host expressions for statements modifying the
        content of database tables as shown further down.
-   See more information
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_operands.htm).

Example demonstrating possible operands:
``` abap
DATA upto TYPE i VALUE 3.

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

  @upto AS num, "Host variable

  @( cl_abap_context_info=>get_system_date( ) ) as date "Host expression

  WHERE carrid = 'LH'          "Untyped literal
    AND countryfr = char`DE`   "Typed literal

  "Data object created inline and escaped with @
  INTO TABLE @DATA(it)

  "The following clause shows all options having the same effect
  UP TO 3 ROWS.             "Untyped numeric literal
  "UP TO int4`3` ROWS.      "Typed numeric literal
  "UP TO @upto ROWS.        "Host variable
  "UP TO @( 10 - 7 ) ROWS.  "Host expression
```

#### SQL Expressions

-   Expressions in an ABAP SQL statement that are passed to the database
    system for evaluation.
-   For example, SQL expressions can be specified as columns in the
    `SELECT` list as demonstrated in most of the following examples.
    Find information on more possible positions and general information
    on SQL expressions
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapsql_expr.htm)
    and the subtopics there.

##### Elementary Expressions

-   An elementary expression represents one of the four mentioned
    operands above: A value from the database (the column name) or
    values from an ABAP program passed to the database (literal, host
    variable or host expression).
-   As an example, see the `SELECT` list in the example above.
-   See more information
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_elem.htm).

##### SQL Functions

-   You can use built-in functions in ABAP SQL.
-   Result: Value with the associated dictionary type.
-   Arguments of the functions: Cover one or more SQL expressions.
-   See more information
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_builtin_functions.htm).

Example: [Numeric functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_arith_func.htm)
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

Example: [String functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_string_func.htm)

``` abap
SELECT SINGLE
  carrid,    "LH
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
  like_regexpr( pcre  = '\..',  "Period that is followed by any character
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
  locate_regexpr_after( pcre = '.',  "Any character
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

Example: [Special functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_special_functions.htm)

``` abap
SELECT SINGLE
  carrid,

  "Conversion functions
  "When used: Special conversions that cannot be handled in a general
  "CAST expression

  "Type conversion: string of fixed length (e.g. of type c) to variable
  "length string of type string
  to_clob( carrid ) AS clob,

  "Byte string -> character string
  bintohex( raw`3599421128650F4EE00008000978B976` ) AS bintohex,

  "Character string -> byte string
  hextobin( char`3599421128650F4EE00008000978B976` ) AS hextobin,

  "Byte field of type RAW to a byte string (BLOB) of type RAWSTRING
  to_blob( raw`3599421128650F4EE00008000978B976` ) AS blob,

  "Unit and currency conversion functions
  "More parameters are available.

  "Converts miles to kilometers
  unit_conversion( quantity = d34n`1`,
                   source_unit = unit`MI`,
                   target_unit = unit`KM` ) AS miles_to_km,

  "Converts Euro to US dollars using today's rate
  currency_conversion(
    amount = d34n`1`,
    source_currency = char`EUR`,
    target_currency = char`USD`,
    exchange_rate_date = @( cl_abap_context_info=>get_system_date( ) )
                     ) AS eur_to_usd,

  "Date and time functions
  add_days( @( cl_abap_context_info=>get_system_date( ) ), 4 ) AS add_days,
  add_months( @( cl_abap_context_info=>get_system_date( ) ), 2 ) AS add_months,
  is_valid( @( cl_abap_context_info=>get_system_date( ) ) ) AS date_is_valid,
  is_valid( @( cl_abap_context_info=>get_system_time( ) ) ) AS time_is_valid

FROM zdemo_abap_carr
INTO @DATA(special_functions).
```

[Aggregate expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_aggregate.htm)

-   Consist of [aggregate
    functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenaggregate_function_glosry.htm "Glossary Entry")
    and aggregate the values of multiple rows of the result set of a
    [query](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenquery_glosry.htm "Glossary Entry")
    into a single value
-   See more information
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

##### More SQL Expressions

-   [Arithmetic
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_arith.htm)
    to perform arithmetic calculations using the operators `+`,
    `-`, `*`, `/`,
-   [Cast
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_cast.htm)
    to convert the value of operands to a dedicated dictionary type.
    Note that there are special [conversion
    rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_cast_rules.htm).
-   [String
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_string.htm)
    using the operator `&&` to concatenate character strings.
-   [Case
    distinctions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_case.htm)
    to carry out either a simple (comparison of the values of a
    dedicated operand) or complex (searched case; evaluation of multiple
    logical expressions) case distinction.

The following example demonstrates the expressions mentioned above:
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
  CAST( 1 AS D34N ) / CAST( 2 AS D34N ) AS ratio,

  "String expression using && to concatenate two character strings;
  "the result of the concatenation must not be longer than
  "255 characters.
  carrid && carrname AS concat,

  "Case distinction
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
INTO @DATA(more_sql_expr).
```

##### Window Expressions

How [window expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwindow_expression_glosry.htm "Glossary Entry") work:

-   Defines a subset of the result set (i. e. the
    "[window](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwindow_glosry.htm "Glossary Entry")")
    of a database query that implements ABAP SQL
-   Applies a [window
    function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwindow_function_glosry.htm "Glossary Entry") -
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

### SQL Conditions

You can formulate conditions in ABAP SQL statements, i. e. [logical
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogical_expression_glosry.htm "Glossary Entry"),
especially in the `WHERE` clause to restrict the result. Note
that without a `WHERE` clause, all rows are respected for the
operation.

See below a selection of the operators that are possible when specifying
conditions. For more information, see the subtopics of the [SQL
Conditions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenasql_cond.htm)
topic.

| Operator   |      Meaning      |
|----------|:-------------:|
| `=`, `EQ`        |                  The content of two operands is equal.|
|  `<>`, `NE`        |               The content of two operands is not equal.|
| `<`, `LT`         |                The content of one operand is less than the content of the other operand.|
| `>`, `GT`          |               The content of one operand is greater than the content of the other operand.|
| `<=`, `LE`         |               The content of one operand is less than or equal to the content of the other operand.|
| `>=`, `GE`        |                The content of one operand is greater than or equal to the content of the other operand.|
| `... [NOT] BETWEEN ... AND ...` |  The value of an operand is (not) between the value of the two other operands.|
| `... [NOT] LIKE ...`         |      The content of an operand matches (does not match) a specified pattern. The pattern can be specified by using wildcard characters. `%` stands for any character string, including an empty string. `_` stands for any character.|
| `... IS [NOT] INITIAL ...`   |      The value of an operand is (not) the initial value of its built-in dictionary type.|
| `... EXISTS ...`           |              Checks the result set of a [subquery](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubquery_glosry.htm "Glossary Entry"). The expression is true if the result set contains at least one row. See more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_logexp_subquery.htm).|
| `... [NOT] IN ( ... )`      |       Checks whether the operands on the left side match a value from a set of values specified in parentheses. On the left side, a single operand or an operand list are possible. On the right side, a comma-separated lists or subqueries can be specified.|

> **💡 Note**<br>
>You can combine multiple logical expressions into one
logical expression using `AND` or `OR`. To further
detail out the desired condition, expressions within parentheses are
possible.

The clause parts that are commented out in the following code snippet
just demonstrate how the `WHERE` clause might look like.

``` abap
SELECT * 
  FROM dbtab
  WHERE comp1 = 'abc' "Equals some value
        
        "More example WHERE conditions:
        AND comp2 > 100 "Greater than some value; alternatively GT is possible
        
        "Not equals plus an additional condition that must be respected
        AND comp3 <> 100 AND comp4 = 'xyz'
        
        "(Not) between a value range
        AND comp5 BETWEEN 1 AND 10
        AND comp6 NOT BETWEEN 1 AND 10
        
        "A character literal has a certain pattern, preceded and
        "followed by any string.
        AND comp7 LIKE '%XYZ%'
        
        "The second character is not Y. _ stands for any character.
        AND comp8 NOT LIKE '_Y%'
        
        "Contains one of the values specified in the parentheses
        AND comp9 IN ( 'ABC', 'DEF', 'GHI' )
                        
        "Does not contain one of the values specified in the parentheses
        AND comp10 NOT IN ( 'JKL', 'MNO' )
        
        "Checking if an operand has an initial value
        AND comp11 IS INITIAL
        
        "Combination of logical expression using AND, OR and parentheses
        AND  ( comp12 = a AND comp13 < b ) OR ( comp14 > c AND comp15 <> d )
        
  INTO TABLE @DATA(itab_where).
```

### Selecting Data by Evaluating the Content of Other Tables

[`FOR ALL ENTRIES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_all_entries.htm)
addition:
- Components of an internal table can be used in the `WHERE` clause in logical expressions for comparisons with a column of the data source.
- The logical expression is evaluated for each individual row of the internal table.
- The result set of the `SELECT` statement is the union set of the result sets produced by the individual evaluations. Rows that occur more than once are removed from the result set automatically. The entire content of a row is respected.
- If `FOR ALL ENTRIES` is specified, there must be at least one comparison with a column of the internal table.
- For more information, especially restricitions and things to pay attention to (e. g. making sure that the internal table is not initial), see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_all_entries.htm).

``` abap
"Checking that table is not initial
IF ( 0 < lines( itab2 ) ).

  SELECT comp1, comp2, comp3
    FROM dbtab
    FOR ALL ENTRIES IN @itab2      "Host variable before internal table
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

### Combining Data of Multiple Database Tables

**Using an [inner join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninner_join_glosry.htm)**:
- Columns of two or more data sources in a result set can be joined.
- Result set:
  - Columns of the rows in the result set of the left side with the columns of the rows in the result set of the right side are joined into a single result set.
  - Contains all combinations of rows for whose columns the join condition is true.
- If there are identical column names in multiple data sources, use the [column
selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_comp_selector_glosry.htm "Glossary Entry")
`~`.
``` abap
SELECT a~comp1, a~comp2, b~comp3, c~comp4
  FROM dbtab1 AS a
  INNER JOIN dbtab2 AS b ON a~comp1 = b~comp1 AND a~comp2 = b~comp2
  INNER JOIN dbtab3 AS c ON a~comp1 = c~comp1
  WHERE ...
  INTO ...
```

**Using an [outer join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenouter_join_glosry.htm)**:
- Realized by either a [left outer join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenleft_outer_join_glosry.htm) or
a [right outer join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenright_outer_join_glosry.htm).
- Result set:
  - Same result set as the inner join.
  - Difference: For each selected row on the left side as `LEFT OUTER JOIN` or on the right side as `RIGHT OUTER JOIN`, at least one row is created in the result set even if no rows on the other side meet the condition. The columns on the other side that do not meet the condition are filled with [null values](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennull_value_glosry.htm).

``` abap
"Example for a left outer join
SELECT a~comp1, a~comp2, b~comp3,
  FROM dbtab1 AS a
  LEFT OUTER JOIN dbtab2 AS b ON a~comp1 = b~comp1
  WHERE ...
  INTO ...
```
> **💡 Note**<br>
> There are more join variants available. See the ABAP
Keyword Documentation on
[joins](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_join.htm)
for more information.

**Merging the result sets of multiple queries into a single result set** using the [set operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_set_operators_glosry.htm) [`UNION`](http://ldcialx.wdf.sap.corp:50018/sap/public/bc/abap/docu?object=abapunion&sap-language=EN&sap-client=000&version=A&tree=X). In this case, the rows of the result set of the query after `UNION` are inserted into the result set of the query in front of `UNION`.

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

#### Common Table Expressions (CTE)

When to use [Common Table Expressions (CTE)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencommon_table_expression_glosry.htm):

-   Whenever you need intermediate results in a `SELECT`
    statement and especially if you need them more than once.
-   You get the option of selecting directly from a subquery (`SELECT FROM subquery`), which is not possible in ABAP SQL.

How it works:

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

> **💡 Note**<br>
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

## Changing Data in Database Tables

### Using [`INSERT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinsert_dbtab.htm)

- Inserts one or more rows into a database table specified.
- The rows to be inserted are taken from a structure, an internal table, or the result set of an embedded subquery.
- As mentioned above, structures and internal tables from which to insert content should be specified as host variables (with `@`) or host
expressions (with `@( ... )`).
- The system fields `sy-subrc` (0 = single row or all rows inserted successfully, 4 = row not or not all rows inserted) and `sy-dbcnt` (number of rows that are inserted) are set.

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

### Using [`UPDATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapupdate.htm)
- Changes the content of one or more rows of a database table specified.
- Similar to `INSERT`, `sy-subrc` and `sy-dbcnt` are set.

``` abap
"Changing content by overwriting entire rows based on a structure

UPDATE dbtab FROM @row.
UPDATE dbtab FROM @( VALUE #( comp1 = ... comp2 = ... ) ). "Using a host expression

"Changing content by overwriting entire rows based on rows in an internal table

UPDATE dbtab FROM TABLE @itab.

"Using a host expression

UPDATE dbtab FROM TABLE @( VALUE #( ( comp1 = ... comp2 = ... )
                                    ( comp1 = ... comp2 = ... ) ) ).

"INDICATORS addition: Changing content of specific fields without overwriting
"existing values of other fields
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

"Reverses the logic

UPDATE dbtab FROM TABLE @ind_tab INDICATORS NOT SET STRUCTURE comp_ind.

"SET addition: Changing values of specific fields in all table rows
"There are mutliple options for the value assignment. E. g. you can use
"a literal, host variable/expression, SQL function, and so on.

UPDATE dbtab SET comp2 = ... .
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using [`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_dbtab.htm)
- Inserts one or more rows into a database table specified or overwrites existing ones.
- As above, `sy-subrc` and `sy-dbcnt` are set.

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

### Using [`DELETE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdelete_dbtab.htm)
- Deletes one or more rows from a database table specified.
- As above, `sy-subrc` and `sy-dbcnt` are set.

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

DELETE dbtab FROM TABLE @itab.

"Using a host expression

DELETE dbtab FROM TABLE @( VALUE #( ( comp1 = ... )
                                    ( comp1 = ... ) ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information
- Note that ABAP SQL statements offer syntax options for dynamic programming. For example, you can specify the data source to read from dynamically. See more information in the ABAP Keyword Documentation or the [ABAP cheat sheet on dynamic programming](06_Dynamic_Programming.md).
    ```abap
    DATA(dbtab) = 'ZDEMO_ABAP_FLSCH'.

    "Selecting from a dynamically specified database table.
    SELECT *
      FROM (dbtab)
      WHERE ...
      INTO ...
    ```
- [This topic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql.htm) serves as the entry point for topics about ABAP SQL in the ABAP Keyword Documentation. For the full details, check the subtopics there, especially topics not covered in this cheat sheet.

## Executable Example
[zcl_demo_abap_sql](./src/zcl_demo_abap_sql.clas.abap)

Note the steps outlined [here](README.md#-getting-started-with-the-examples) about how to import and run the code.
