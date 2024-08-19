<a name="top"></a>

# ABAP SQL

- [ABAP SQL](#abap-sql)
  - [Introduction](#introduction)
  - [Excursion: Database Tables and Views](#excursion-database-tables-and-views)
  - [Retrieving Data Using SELECT](#retrieving-data-using-select)
    - [Basic Syntax](#basic-syntax)
    - [SELECT List Variants](#select-list-variants)
    - [Data Sources of SELECT Queries](#data-sources-of-select-queries)
    - [Retrieving Single and Multiple Rows](#retrieving-single-and-multiple-rows)
    - [Miscellaneous Options Regarding the Result](#miscellaneous-options-regarding-the-result)
    - [Additional Clauses](#additional-clauses)
    - [Selecting Data by Evaluating the Content of Other Tables](#selecting-data-by-evaluating-the-content-of-other-tables)
    - [Combining Data of Multiple Tables](#combining-data-of-multiple-tables)
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
    - [More Functions](#more-functions)
  - [Changing Data in Database Tables](#changing-data-in-database-tables)
    - [Using INSERT](#using-insert)
    - [Using UPDATE](#using-update)
    - [Using MODIFY](#using-modify)
    - [Using DELETE](#using-delete)
    - [Using Constructor Expressions in ABAP SQL Statements](#using-constructor-expressions-in-abap-sql-statements)
    - [Example: Exploring ABAP SQL Statements Changing Data in Database Tables](#example-exploring-abap-sql-statements-changing-data-in-database-tables)
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
    | `SELECT` | Reads data from database tables                                           |
    | `INSERT` | Adds rows to database tables                                              |
    | `UPDATE` | Changes the content of rows of database tables                            |
    | `MODIFY` | Inserts rows into database tables or changes the content of existing rows |
    | `DELETE` | Deletes rows from database tables                                         |

- ABAP SQL statements use the ABAP SQL interface. This interface transforms all ABAP SQL statements that access the standard database of an AS ABAP to  platform-dependent SQL and forwards the results to the database system.
- Generally bear in mind the [performance notes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_perfo.htm) when using
    ABAP SQL. The considerations there are not relevant for this cheat sheet since
    the focus is on syntax options.

## Excursion: Database Tables and Views

<details>
  <summary>Expand to view the details</summary>
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
</details>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Retrieving Data Using SELECT

### Basic Syntax

You use ABAP SQL `SELECT` statements to retrieve data from one or more data sources (such as database tables, views or internal tables). This can be done to create a multirow or single row result set by assigning the result set to a suitable data object, i. e. you can store the multirow read result in an internal table or the single row result in a structure.
The `SELECT` statement includes several clauses that serve
different purposes. The following code snippet shows the basic syntax (see the note below for a different but interchangeable syntax):
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
    keyword before the `FROM` clause - without `FIELDS`. The following two `SELECT` statements are basically the same but differently arranged. The code snippets in the cheat sheet randomly use one syntax or the other. 
>      ``` abap
>      SELECT FROM dbtab
>        FIELDS comp1, comp2, comp3
>         ...
>
>       SELECT comp1, comp2, comp3
>         FROM dbtab
>         ...
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
    ABAP cheat sheet [Internal Tables](01_Internal_Tables.md). The declaration operator [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm) can be used to declare immutable variables.
>- Many syntax examples in this cheat sheet show a selection from `dbtab` denoting a database table as a source. However, other data sources can also be specified.    
>- The syntax options for the `SELECT` statement are extensive. Make sure that you consult the ABAP Keyword Documentation for all available options. The cheat sheet and snippets demonstrate a selection.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### SELECT List Variants

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Data Sources of SELECT Queries

- Most of the code snippets in this cheat sheet use database tables as the source in `SELECT` statements. 
- Among others, you can also use internal tables or CDS view entities as data source.
- Note that the internal table must be specified as host variable prefixed by `@`, and an alias name must be specified. 
- More information and code snippets: 
  - Section [SELECT Queries with Internal Tables as Data Sources](01_Internal_Tables.md#select-queries-with-internal-tables-as-data-sources) in the *Internal Tables* cheat sheet and in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_itab.htm)
  - The executable example of the [CDS View Entities](15_CDS_View_Entities.md) cheat sheet covers `SELECT` statements with CDS view entities as data sources. 

``` abap
SELECT *
  FROM @itab1 AS tab
  WHERE ...
  INTO ....

SELECT *
  FROM some_cds_view
  WHERE ...
  INTO ....
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Retrieving Single and Multiple Rows

<table>
<tr>
<td> Subject </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> Retrieving a single row into a structure </td>
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

> **💡 Note**<br>
>-  Although its use is optional, a `WHERE` clause should be specified to further restrict the read result.
>-  Regarding the addition `CORRESPONDING FIELDS OF` in the `INTO`
    clause: As mentioned, only the content of columns for which there are identically named components in the target are assigned. However, if you want to read data into an existing data object and particular fields are specified in the `SELECT` list and if the addition is **not** specified, you might stumble on undesired results. The target data object must contain enough components and the content of the columns are assigned to the components of the target from left to right in the order specified after `SELECT`. The content of surplus components of the target is not changed. Plus, pay attention to [assignment rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_into_conversion.htm). Basic rule: Without `CORRESPONDING ...`, column names do not play a role but only the position. With `CORRESPONDING ...`, the position of the columns does not play a role but only the name.
>-   Find more information regarding the addition `INTO` [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinto_clause.htm).

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
"Instead of @abap_true, you could also use 'X'.
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
SELECT DISTINCT comp1
  FROM dbtab
  WHERE ...
  INTO TABLE @itab.
```

</td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Miscellaneous Options Regarding the Result

This section demonstrates different patterns of the ABAP SQL `SELECT` statement for handling query results. There is a wide range of options available, some of which have already been covered above. This section covers a selection of additions and syntax options. For complete details and syntax options, you can refer to the ABAP Keyword Documentation.

<table>
<tr>
<td> Subject </td> <td> Details/Code Snippet </td>
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
<td> Storing the result in individual elementary data objects </td>
<td>

Apart from reading into structures and internal tables outlined above, you can also read into individual elementary data objects.
Here, the individual elementary data objects as target objects are specified in a comma-separated list (e. g. as existing host variables or declared inline with `@DATA(...)`) and put between a pair of parentheses.
Note:
- The comma-separated list must have the same number of elements as columns in the result set.
- The content of the columns in the result set is assigned to the data objects specified in the list from left to right in accordance with the order specified in the `SELECT` list.
- Note the [assignment rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_into_conversion.htm) also in this context.
- More information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinto_clause.htm#!ABAP_ALTERNATIVE_1@1@).


<br>

``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO (@res1,@res2,@res3).
  "INTO (@DATA(res4),@DATA(res5),@DATA(res6)). "Using inline declarations
```

</td>
</tr>

<tr>
<td> <code>APPENDING</code> addition: Appending the result set to an existing internal table </td>
<td>

The addition `INTO` initializes the target object. When using the addition `APPENDING`, you can retain existing lines in internal tables. `APPENDING` is also possible with the addition `CORRESPONDING FIELDS OF TABLE`.

<br>

``` abap
SELECT * FROM dbtab
  WHERE ...
  APPENDING TABLE @itab.

SELECT * FROM dbtab
  WHERE ...
  APPENDING CORRESPONDING FIELDS OF TABLE @diff_itab.
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
<td> <code>NEW</code> addition: Specifying an anonymous data object as target object </td>
<td>

- Specifying an [anonymous data object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm) as target object using the addition [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_into_target.htm#!ABAP_ALTERNATIVE_3@3@) 
- Only to be used after `INTO` and not `APPENDING`.

<br>

``` abap
"Here, the target object is an anonymous data object declared inline.
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO TABLE NEW @DATA(dref).
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

### Additional Clauses

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

> **💡 Note**<br>
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
- For more information, especially restricitions and things to pay attention to (e. g. making sure that the internal table is not initial), see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_all_entries.htm).

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

### Combining Data of Multiple Tables

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
> There are more join variants and syntax options available. See the ABAP Keyword Documentation on [joins](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_join.htm)
for more information.

**Merging the result sets of multiple queries into a single result set** using the [set operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_set_operators_glosry.htm) [`UNION`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapunion.htm#!ABAP_VARIANT_1@1@). In this case, the rows of the result set of the query after `UNION` are inserted into the result set of the query in front of `UNION`.

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

**Returning distinct rows of a result set (1)** of a query specified before the `INTERSECT` addition that are also available in the result set of the query after the `INTERSECT` addition.


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

**Returning distinct rows of a result set (2)** of a query specified before the `EXCEPT` addition that are not available in the result set of the query after the `EXCEPT` addition.

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

> **💡 Note**<br>
> There a more syntax options and contexts for `UNION`, `INTERSECT`, and `EXCEPT`. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapunion.htm).

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


## SQL Conditions

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
| `... [NOT] IN ...`      |       Checks whether the operands on the left side match a value from a set of values specified in parentheses. On the left side, a single operand or an operand list are possible. On the right side, a comma-separated lists or subqueries can be specified. It is also possible to specify a [ranges table](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenranges_table_glosry.htm) to evaluate [ranges conditions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenranges_condition_glosry.htm).|
| `... IS [NOT] NULL ...`      |   Checks whether the value of an operand is (not) the [null value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennull_value_glosry.htm). Find more information in the code snippet and in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_logexp_null.htm).   |

> **💡 Note**<br>
>You can combine multiple logical expressions into one
logical expression using `AND` or `OR`. To further
detail out the desired condition, expressions within parentheses are
possible. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_logexp_andornot.htm). 

Examples:

``` abap
"---- SQL conditions demonstrated with the WHERE clause ----
"Note:
"- For most of the self-contained examples, an internal table is used as the
"  data source of SELECT statements to work with simple data.
"- For some examples that are covered, such as subqueries, demo database tables
"  from the cheat sheet repository are used in addition.
"- Dynamic specifications are also possible. They are not covered here. See
"  the Dynamic Programming cheat sheet.

"---- Types and internal table to work with in the examples ----
"Note: You cannot use type string columns in WHERE conditions.
TYPES: BEGIN OF demo_struc,
          id   TYPE i,
          name TYPE c LENGTH 15,
          "name TYPE string,
        END OF demo_struc.
DATA itab TYPE SORTED TABLE OF demo_struc WITH UNIQUE KEY id.
"Populating internal table with data to work with in the examples
itab = VALUE #( ( id = 1 name = 'bear' )
                ( id = 2 name = 'camel' )
                ( id = 3 name = 'rabbit' )
                ( id = 4 name = 'zebra' )
                ( id = 5 name = 'dog' )
                ( id = 6 name = 'deer' )
                ( id = 7 name = 'squirrel' )
                ( id = 8 name = 'cheetah' )
                ( id = 9 name = 'elephant' )
                ( id = 10 name = 'donkey' )
                ( id = 11 name = 'fish' )
                ( id = 12 name = 'sheep' ) ).

"---- =, <>, >, >= (as a selection of possible comparison operators) ----
SELECT id FROM @itab AS tab WHERE name = 'bear' INTO TABLE @DATA(it). "1
SELECT id FROM @itab AS tab WHERE name <> 'bear' INTO TABLE @it. "2-12
SELECT id FROM @itab AS tab WHERE id > 10 INTO TABLE @it. "11,12
SELECT id FROM @itab AS tab WHERE id >= 10 INTO TABLE @it. "10,11,12

"---- Combining logical expressions using AND, OR and parentheses  ----
SELECT id FROM @itab AS tab WHERE id = 1 AND name = 'bear' INTO TABLE @it. "1
SELECT id FROM @itab AS tab WHERE name = 'bear' OR name = 'sheep' INTO TABLE @it. "1,12

"In the following example, the resulting table is initial. One of the expressions
"in parentheses is false (AND is used between the expressions in parentheses).
"In contrast, the example below returns an entry because of using OR.
SELECT id FROM @itab AS tab
  WHERE ( id = 1 AND name = 'bear' )
  AND ( id = 20 AND name = 'camel' )
  INTO TABLE @it.

SELECT id FROM @itab AS tab
  WHERE ( id = 1 AND name = 'bear' )
  OR ( id = 20 AND name = 'camel' )
  INTO TABLE @it. "1

"------------------------ [NOT] BETWEEN ------------------------
SELECT id FROM @itab AS tab WHERE id BETWEEN 1 AND 4 INTO TABLE @it. "1,2,3,4
"The condition with BETWEEN above corresponds to the following condition.
"The example makes use of a condition specified in parentheses to combine multiple
"expressions.
SELECT id FROM @itab AS tab WHERE ( id >= 1 AND id <= 4 ) INTO TABLE @it. "1,2,3,4
"Negation with NOT
SELECT id FROM @itab AS tab WHERE id NOT BETWEEN 1 AND 4 INTO TABLE @it. "5-12

"------------------------ IS [NOT] INITIAL ------------------------
SELECT id FROM @itab AS tab WHERE id IS NOT INITIAL INTO TABLE @it. "1-12

"------------------------ [NOT] LIKE ------------------------
"For (not) matching a specified pattern
"Note: % (any character string), _ (any character).
SELECT name FROM @itab AS tab
  WHERE name LIKE '%ee%'
  OR name LIKE '_o%'
  INTO TABLE @DATA(names). "dog,deer,cheetah,donkey,sheep

"ESCAPE addition for defining a single-character escape character
"In the following example, this character is #. It is placed before
"the % character in the specification after LIKE. In this case, %
"is escaped and does then not stand for any character string in the
"evaluation.
"Adding a table entry for this syntax example.
itab = VALUE #( BASE itab ( id = 13 name = '100%' ) ).
"Any character sequence followed by the % character
SELECT name FROM @itab AS tab
  WHERE name LIKE '%#%' ESCAPE '#'
  INTO TABLE @names. "100%

"Deleting the entry because it is not relevant for the further examples.
DELETE itab INDEX 13.

"------------------------ [NOT] IN (using a value set) ------------------------
"For (not) matching a value in a set of values specified in parentheses.

"Single operands on the left side of IN
SELECT id FROM @itab AS tab
  WHERE name IN ( 'camel', 'rabbit', 'dog', 'snake' )
  INTO TABLE @it. "2,3,5

"Negation NOT IN; note to use host variables/expressions for local/global data objects
DATA(animal) = 'sheep'.
SELECT id FROM @itab AS tab
  WHERE name NOT IN ( 'fish', @animal )
  INTO TABLE @it. "1-10

"Operand list (a parenthesized comma-separated list) on the left side of IN
"For (not) matching value tuples from a set of value tuples specified in parentheses on the right side.
"In the following example, two values are specified in the operand list on the left. Consequently,
"two values with appropriate types must be specified in parentheses on the right.
SELECT id FROM @itab AS tab
  WHERE ( id, name ) IN ( ( 1, 'bear' ), ( 3, 'rabbit' ), ( 8, 'zebra' ), ( 20, 'dog' ) )
  INTO TABLE @it. "1,3


"------------------------ [NOT] IN (using a subquery) ------------------------
"[NOT] IN for matching a value contained in the result set of a subquery

"In the following example, the subquery reads data from a demo database table.
"For a representative result, the table is cleared, and then filled with 'suitable'
"data sets.
DELETE FROM zdemo_abap_tab1.
MODIFY zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 11 num1 = 11 )
                                              ( key_field = 12 num1 = 12 )
                                              ( key_field = 13 num1 = 13 )
                                              ( key_field = 14 num1 = 14 ) ) ).

SELECT id FROM @itab AS tab
  WHERE id IN ( SELECT key_field FROM zdemo_abap_tab1 ) INTO TABLE @it. "11,12

"------------------------ [NOT] IN (using a ranges table) ------------------------
"[NOT] IN for checking whether the operands on the left side match a ranges condition in a ranges table

"Declaring a ranges table
DATA rangestab TYPE RANGE OF i.
"Populating a ranges table using the VALUE operator
rangestab = VALUE #( ( sign   = 'I' option = 'BT' low = 1 high = 3 )
                     ( sign   = 'I' option = 'GE' low = 10  ) ).

SELECT id FROM @itab AS tab WHERE id IN @rangestab INTO TABLE @it. "1,2,3,10,11,12


"You cannot use logical operators such as CP (conforms to pattern) in the WHERE clause.
"In a ranges table, they are possible.
"Note:
"- Regarding CP: * (any character sequence), + (any character), # (escape character)
"- An equivalent example above uses the LIKE addition.
DATA rt TYPE RANGE OF demo_struc-name.
rt = VALUE #( ( sign   = 'I' option = 'CP' low = '*ee*' ) "ee in a string
              ( sign   = 'I' option = 'CP' low = '+o*' ) ). "o in second position
SELECT name FROM @itab AS tab
  WHERE name IN @rt
  INTO TABLE @names. "dog,deer,cheetah,donkey,sheep

"------------------------ EXISTS ------------------------
"For checking the result set of a subquery.
"The following example reads all entries from the internal table if entries having
"the same key also exist in the database table.
"Note: The SELECT list in the subquery only contains a literal to determine that
"the entry exists. Specifying explicit column names is not relevant.
SELECT id FROM @itab AS tab WHERE
  EXISTS ( SELECT @abap_true FROM zdemo_abap_tab1 WHERE key_field = tab~id )
  INTO TABLE @it. "11,12

"------------------------ IS [NOT] NULL ------------------------
"The null value is a special value that is returned by a database. It indicates an
"undefined value or result. Note that, in ABAP, there are no special null values. Do
"not confuse the null value with a type-dependent initial value. When using SELECT
"statements to read data, null values can be produced by, for example, outer joins.
"When the null values are passed to a data object, they are transformed to the
"type-dependent initial values. For more information, refer to the ABAP Keyword Documentation.
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
"the joined values are null in that case. The WHERE clause uses the addition IS NULL.
"Therefore, the result only contains this entry. char2 is assigned the type-initial
"value in the result.
SELECT tab2~key_field, tab1~char2
    FROM zdemo_abap_tab2 AS tab2
    LEFT OUTER JOIN zdemo_abap_tab1 AS tab1 ON tab1~char1 = tab2~char1
    WHERE tab1~char1 IS NULL
    INTO TABLE @DATA(joined_tab).
*KEY_FIELD    CHAR2
*4

"The following example visualizes the null values. The INDICATORS addition of the
"INTO clause is used to specify indicators such as the null indicator. In the
"example, an appropriate target table is defined to also store information about
"which columns of the result set contain the null value and which do not.
"For more information on the syntax, refer to the ABAP Keyword Documentation.
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
*S2                      NULLS
*KEY_FIELD    CHAR2      KEY_FIELD    CHAR2
*1            y
*KEY_FIELD    CHAR2      KEY_FIELD    CHAR2
*2            y
*KEY_FIELD    CHAR2      KEY_FIELD    CHAR2
*3            z
*KEY_FIELD    CHAR2      KEY_FIELD    CHAR2
*4                                    X

"Negation IS NOT NULL
SELECT tab2~key_field, tab1~char2
  FROM zdemo_abap_tab2 AS tab2
  LEFT OUTER JOIN zdemo_abap_tab1 AS tab1 ON tab1~char1 = tab2~char1
  WHERE tab1~char1 IS NOT NULL
  INTO TABLE @joined_tab.
*KEY_FIELD    CHAR2
*1            y
*2            y
*3            z
```

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
        or untyped. Typed literals are preferable for the following
        reasons: Using untyped literals means extra cost in terms of
        performance since they must be converted by the compiler. Plus,
        their use can result in errors at runtime whereas typed literals
        guarantee type compatibility at once. For more information on [typed literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentyped_literal_glosry.htm), refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_typed_literals.htm) and the [Typed Literals in ABAP SQL](/16_Data_Types_and_Objects.md#typed-literals-in-abap-sql) section of the *Data Types and Data Objects* cheat sheet.
    -   Regarding host expressions: Structures and internal tables are
        possible as host expressions for statements modifying the
        content of database tables as shown further down.
-   Find more information
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_operands.htm).


> **💡 Note**<br>
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
- They that passed to the database system for evaluation.
- For example, SQL expressions can be specified as columns in the
    `SELECT` list as demonstrated in most of the following examples.
- Find information on more possible positions and general information
    on SQL expressions
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapsql_expr.htm)
    and the subtopics there.

> **💡 Note**<br>
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
INTO @DATA(cast_expr).
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
- The functions are also covered in other cheat sheets such as the *Misc Built-In Functions* cheat sheet.

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

### More Functions

More information:
- [Special functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_special_functions.htm)
- [UUID function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_uuid.htm)
- [Date and time functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_uuid.htm)
- It is also possible to call [SQL-based scalar functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_sql_scalar_glosry.htm). Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_cds_scalar_func.htm).

The following example shows a selection. Other cheat sheets such as *Misc Built-In Functions* show more snippets about date and time functions.
  
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
  is_valid( @( cl_abap_context_info=>get_system_time( ) ) ) AS time_is_valid,

  "UUID
  uuid( ) AS uuid

FROM zdemo_abap_carr
INTO @DATA(special_functions).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Changing Data in Database Tables

> **💡 Note**<br>
> The following sections include code patterns. To explore various syntax options with an executable example, see section [Example: Exploring ABAP SQL Statements Changing Data in Database Tables](#example-exploring-abap-sql-statements-changing-data-in-database-tables) below.

### Using INSERT

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

### Using UPDATE

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

"-------------------------- SET addition --------------------------
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

"--------------- INDICATORS ... SET STRUCTURE addition ---------------
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

### Using MODIFY

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

### Using DELETE

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
"--- VALUE ---
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

"The following example assumes that some_itab has a different line type than dbtab.
"I.e. some_itab may have more components that are not available in dbtab. The
"corresponding fields with identical names are used. It is assumed that the components'
"types are compatible and/or convertible.
INSERT dbtab FROM TABLE @( CORRESPONDING #( some_itab ) ).

"This example assumes that field names are not identical. Using the CORRESPONDING operator
"and its additions, you can carry out a mapping and, for example, exclude components. It is 
"assumed that the components' types are compatible and/or convertible.
INSERT dbtab FROM TABLE @( CORRESPONDING #( another_itab MAPPING key_field = key comp1 = compZ EXCEPT comp2 ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example: Exploring ABAP SQL Statements Changing Data in Database Tables

To try the following example out, create a demo class named `zcl_some_class` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example uses a database table of the ABAP cheat sheets repository and is set up to display output in the console.


```abap
CLASS zcl_some_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_some_class IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "--------------------------- INSERT ---------------------------

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

**********************************************************************

    "--------------------------- UPDATE ---------------------------

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

**********************************************************************

    "--------------------------- MODIFY ---------------------------
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

**********************************************************************

    "--------------------------- DELETE ---------------------------

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

**********************************************************************

    "-------- Exploring constructor expressions for internal tables created in place --------
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

### RAP-Specific ABAP SQL Variants

There are [RAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenarap_glosry.htm)-specific variants of ABAP SQL statements that use the `MAPPING FROM ENTITY` addition. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmapping_from_entity.htm) and in the [ABAP for RAP: Entity Manipulation Language (ABAP EML)](08_EML_ABAP_for_RAP.md#abap-sql-statements-with-bdef-derived-types) cheat sheet. 

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
- Find more topics in the ABAP Keyword Documentation such as the following: 
  - ABAP SQL statements can contain [SQL path expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_path_expression_glosry.htm). For more information, see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_path.htm). The executable example of the CDS view entities cheat sheet includes demo SQL statements.
  - Find [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_exceptions.htm) and overview on exceptions that can occur in the context of ABAP SQL statements.
  - As a rule, bear in mind performance aspects when using ABAP SQL statements. Find more information in the [Performance Notes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_perfo.htm). The code snippets here only focus on syntax options.
  - You can specify hierarchy data as a data source in ABAP SQL `SELECT` statements. Find more information and examples in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_data.htm). For working with hierarchies, see the [ABAP SQL: Working with Hierarchies cheat sheet](10_ABAP_SQL_Hierarchies.md).

## Executable Example
[zcl_demo_abap_sql](./src/zcl_demo_abap_sql.clas.abap)

> **💡 Note**<br>
> - The executable example covers the following topics, among others:
>     - Reading from database tables using `SELECT`
>     - Various additions to `SELECT` statements
>     - Changing data in database tables using `INSERT`, `UPDATE`, `MODIFY` and `DELETE`
>     - Excursions: Operands and expressions in ABAP SQL statements
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)