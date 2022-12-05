<a name="top"></a>

# ABAP SQL: Working with Persisted Data in Database Tables

- [ABAP SQL: Working with Persisted Data in Database Tables](#abap-sql-working-with-persisted-data-in-database-tables)
  - [Database Tables in AS ABAP in a Nutshell](#database-tables-in-as-abap-in-a-nutshell)
  - [ABAP SQL Intro](#abap-sql-intro)
  - [Reading Data Using SELECT](#reading-data-using-select)
    - [Basic Syntax](#basic-syntax)
    - [Using SELECT for Multiple Purposes](#using-select-for-multiple-purposes)
    - [Clause Variations and Additions in SELECT Statements](#clause-variations-and-additions-in-select-statements)
    - [Further Clauses](#further-clauses)
    - [Excursion: Operands and Expressions in ABAP SQL Statements](#excursion-operands-and-expressions-in-abap-sql-statements)
    - [Excursion: SQL Conditions](#excursion-sql-conditions)
    - [Using SELECT when Reading from Multiple Tables](#using-select-when-reading-from-multiple-tables)
      - [Excursion: Using Common Table Expressions (CTE)](#excursion-using-common-table-expressions-cte)
  - [Changing Data in Database Tables](#changing-data-in-database-tables)
    - [Using INSERT](#using-insert)
    - [Using UPDATE](#using-update)
    - [Using MODIFY](#using-modify)
    - [Using DELETE](#using-delete)
  - [Further Information](#further-information)
  - [Executable Example](#executable-example)


## Database Tables in AS ABAP in a Nutshell

Database tables in [AS
ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenas_abap_glosry.htm "Glossary Entry")
...

-   are objects of the [ABAP Dictionary
    (DDIC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_dictionary_glosry.htm "Glossary Entry")
-   consist of table rows and columns; each row represents a data record
    whose components (or fields) are available in columns; each
    component has a data type.
-   are [relational
    database](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrelational_database_glosry.htm "Glossary Entry")
    tables, i. e. information can be stored in multiple database tables
    that are related to each other.
    -   For example, there might be a table containing information on
        flight connections, flight destinations and times, another table
        is related to this one and includes further details on the
        flights like occupied seats in the plane or price details.
    -   Such tables define a relationship using [foreign
        key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_database_tables_forkeyrel.htm)
        relations.
-   have at least one key, i.e. the [primary
    key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprimary_key_glosry.htm "Glossary Entry"),
    to uniquely identify table rows; this might be one or more columns
    at the beginning of each database table.
-   are either
    cross-[client](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclient_glosry.htm "Glossary Entry")
    or client-specific to keep the data separated; client-specific
    tables, which are the vast majority of database tables, include a
    client field (often named `MANDT`) as their first key
    field.
    -   Note: ABAP SQL ensures that a statement only
        manipulates data from the current client.
-   have a [flat
    structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenflat_structure_glosry.htm "Glossary Entry")
    type.
-   are physically created on the database when activated - in contrast
    to [internal
    tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninternal_table_glosry.htm "Glossary Entry").
    Plus, a globally available [structured
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstructured_type_glosry.htm "Glossary Entry")
    of the same name is created, too. Hence, in an ABAP program, a
    database table's name can be used to declare data objects, for
    example, internal tables. These can be accessed by ABAP SQL, too.
-   are primarily processed through ABAP SQL statements that use
    [structures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstructure_glosry.htm "Glossary Entry")
    for single rows and internal tables for multiple rows to be
    processed.

<details>
  <summary>Excursion: Views</summary>
<br>

**Views ...**

-   are further ABAP Dictionary objects for grouping particular data.
-   combine columns of one or more database tables.
-   usually realize a
    [join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenjoin_glosry.htm "Glossary Entry")
    with defined [join
    conditions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenjoin_condition_glosry.htm "Glossary Entry").
-   Note:
    -   Similar to database tables, the columns of such a view form a
        flat structure. Hence, the view's name can be used to declare
        data objects, too.
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
        support annotations (provide information about views or
        individual fields), data sources can be combined using
        associations, unions are possible, or views can be defined with
        input parameters.
    -   are used like a classic database view as structured data types
        and used as a source for reading operations with ABAP SQL (using
        [`SELECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect.htm)).
    -   are created using [Data Definition
        Language (DDL)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddl_glosry.htm "Glossary Entry")
        in the
        [ADT](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenadt_glosry.htm "Glossary Entry")
        (that is, a source code editor, in contrast to a form-based
        editor)
    -   are, in contrast to External Views, supported by all database
        systems (that support the ABAP CDS characteristics).

> **💡 Note**<br>
> The code snippets below focus on database tables as [data
source](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_source_glosry.htm "Glossary Entry")
for ABAP SQL statements.
</details>

<p align="right">(<a href="#top">back to top</a>)</p>

## ABAP SQL Intro

-   ABAP-specific form of standard [Structured Query Language
    (SQL)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_glosry.htm "Glossary Entry")
    which is the common language to access database tables.
-   What happens behind the scenes when using an ABAP SQL statement?
    -   Generally speaking, tables in relational database systems have a
        programming interface allowing table access using standard SQL,
        however, these interfaces are not entirely uniform and can have
        individual characteristics.
    -   To make AS ABAP independent of the database used, the ABAP SQL
        statements are converted to the corresponding [Native
        SQL](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennative_sql_glosry.htm "Glossary Entry")
        statements of the current database system. In doing so, ABAP SQL
        allows a hassle-free and uniform access to the database tables
        no matter what database system is used.
-   The main ABAP SQL keywords to read and change data are the
    following:

    | Keyword  | Purpose                                                                   |
    | -------- | ------------------------------------------------------------------------- |
    | `SELECT` | Reads data from database tables                                           |
    | `INSERT` | Adds rows to database tables                                              |
    | `UPDATE` | Changes the content of rows of database tables                            |
    | `MODIFY` | Inserts rows into database tables or changes the content of existing rows |
    | `DELETE` | Deletes rows from database tables                                         |

-   For a good level of performance of your ABAP programs when using
    ABAP SQL, you should follow the rules in the performance notes
    outlined
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_perfo.htm).
    The considerations there are not relevant for this cheat sheet since
    the focus is on syntactical options.

## Reading Data Using SELECT

### Basic Syntax

You use ABAP SQL `SELECT` statements to read records from the
database, either by accessing a database table directly or via a view.
The `SELECT` statement includes several clauses that serve
different purposes. The following code snippet shows the basic syntax:
``` abap
SELECT FROM source   "What db table or view to read from
  FIELDS field_list  "What columns should be read
  WHERE condition    "Specifies conditions on which a row/rows should be read
  INTO target.       "Data object into which data should be read
```
> **💡 Note**<br>
>-   There are further clauses available of which some are dealt with
    further down.
>-   Especially in older ABAP programs, you will see other forms of the
    `SELECT` syntax that you should no longer use. Depending on
    the ABAP release in your on-premise system, strict syntax check modes might enforce the use
    of specific ABAP SQL syntax. For example, the `INTO` clause
    should be placed after the other clauses. This was not possible for
    older statements. Furthermore, [host
    variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhost_variable_glosry.htm "Glossary Entry")
    or [host
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhost_expression_glosry.htm "Glossary Entry")
    are required for variables and expressions, i. e. variables and
    expressions must be preceded by `@` or `@( ... )`.
    This is also true for other ABAP SQL statements further down.
    Further information: [Release-Dependent Syntax Check
    Modes](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_sql_strict_modes.htm).
>-   The list of fields can also directly follow the `SELECT`
    keyword and be positioned before the `FROM` clause. In this
    case, a separate `FIELDS` clause cannot be specified. The
    following two code snippets are basically the same:
>   ``` abap
>    SELECT FROM dbtab
>      FIELDS comp1, comp2, comp3
>      ...
>
>    SELECT comp1, comp2, comp3
>      FROM dbtab
>      ...
>   ```
>-   Regarding the target into which data is read: Instead of using a
    variable that is (extra) declared beforehand, you can also make use
    of [inline
    declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry"),
    for example `... INTO TABLE @DATA(itab).`, to comfortably
    create an appropriate variable in place. Note that in case of
    internal tables as targets, the resulting table is a standard table
    and has an empty key which might have an impact when further
    processing the internal table entries. Find more information in the
    ABAP cheat sheet [Working with Internal Tables](01_Internal_Tables.md).

<p align="right">(<a href="#top">back to top</a>)</p>

### Using SELECT for Multiple Purposes

**Reading a single row into a structure**. The read result can, for
example, be stored in an existing structure (`struc`) or a
structure that is declared inline. Specifying an asterisk (`*`) indicates
that all fields are to be read. Alternatively, you can list all the
fields separated by comma.
``` abap
"Reading all fields of a single row

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
"When reading into an existing target variable on the basis of a selected
"set of fields, use a CORRESPONDING addition in the INTO clause

SELECT SINGLE comp1, comp2, comp3       "Selected set of fields
  FROM dbtab
  WHERE ...
  INTO CORRESPONDING FIELDS OF @struc.  "Existing structure
```
> **💡 Note**<br>
>-   When listing the fields, only those fields that are really of
    interest should be read as a rule for performance reasons.
>-   It makes a lot of sense to further restrict the read result, for
    example and although it is optional, a `WHERE` clause should
    always be specified for performance reasons too to restrict the read
    result.
>-   The addition `CORRESPONDING FIELDS OF` in the `INTO`
    clause is required when using an existing variable as target and
    listing the fields, otherwise a type compatibility issue might arise
    because the variable is filled from left to right beginning with the
    first field in the list of fields.

**Reading multiple rows into an internal table**. The read result
can, for example, be stored in an existing internal table
(`itab`) or an internal table that is declared inline.
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

"Reading a selected set of fields into an existing variable

SELECT FROM dbtab
  FIELDS comp1, comp2, comp3                "Selected set of fields
  WHERE ...
  INTO CORRESPONDING FIELDS OF TABLE @itab.
```

`SELECT` **loop: Sequentially reading multiple rows into a structure**. If the row is found, the system field `sy-subrc` is set to `0`.
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

**Reading into an existing target variable that does not have a matching type**. If you choose to store the result in a variable that has
not a matching type, the `CORRESPONDING FIELDS OF` addition
should be used so as not to mess up the result. Note that this addition
is also valid for `SELECT` statements that use an existing
target variable and only a selected set of fields should be read into
the target variable.

``` abap
"Reading a single row into an existing structure that does not have a matching type

SELECT SINGLE FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO CORRESPONDING FIELDS OF @diff_struc.

"Reading multiple rows into an existing internal table that does not
"have a matching type. Note that the target table is initialized
"with this addition.

SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO CORRESPONDING FIELDS OF TABLE @diff_itab.
```

> **💡 Note**<br>
>-   If only `INTO` is used, the selected columns must be in the
    correct order fitting to the structure type of the target variable.
    Only the content of columns for which there are components of the
    same name in the structure of the target is read from the result
    set.
>-   If identically named components have different types, the system
    tries to convert the content of source fields into the type of the
    target field. In this case, there is a risk of data loss and runtime
    errors due to conversion errors.
>-   Find more information regarding the addition
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinto_clause.htm).

<p align="right">(<a href="#top">back to top</a>)</p>

### Clause Variations and Additions in SELECT Statements

`SELECT`/`FROM` clause:

**Checking the existence of a row in a database table**
``` abap
"Instead of @abap_true, you could use 'X'

SELECT SINGLE @abap_true
  FROM dbtab
  WHERE ...
  INTO @DATA(exists).

IF exists = abap_true.
  ...
ENDIF.
```

**Reading multiple rows into an internal table by excluding duplicate rows from the multiline result set** using `DISTINCT`.
The duplicate entries might occur due to a non-unique `WHERE` clause.
``` abap
SELECT DISTINCT comp1
  FROM dbtab
  WHERE ...
  INTO TABLE @itab.
```
**Setting new field names by specifying an alias name** with [`AS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_list.htm)].

The alias name can be helpful for a situation like this: Data from a
database table is to be read into an existing table but the line type
does not match, some fields might have different names. Using an alias
name, you can read the data into the corresponding field names of the
target table (provided that there will not be an issue regarding the
type).
``` abap
SELECT FROM dbtab
  FIELDS comp1 AS alias1, comp2 AS alias2, comp3 AS alias3
  WHERE ...
  INTO CORRESPONDING FIELDS OF TABLE @itab.
```

**Getting data from a database table in another client** (not available in SAP BTP ABAP environments). Note that there are several variants of the `USING CLIENT` addition, for example, you can also specify `ALL CLIENTS` to select from database tables in all clients. Furthermore, the `USING CLIENT` addition is also available for the ABAP SQL statements that modify database table entries further down.
``` abap
"Not available in SAP BTP ABAP environments
SELECT *
  FROM dbtab USING CLIENT '000'            
  WHERE ...
  INTO TABLE @itab.

SELECT *
  FROM dbtab USING ALL CLIENTS
  WHERE ...
  INTO TABLE @itab.
```

`INTO` **clause**:

**Restricting the absolute number of returned table rows** using the addition [`UP TO n
ROWS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_up_to_offset.htm).
In the example below, only five rows are to be returned at most.
``` abap
SELECT * FROM dbtab
  WHERE ...
  INTO TABLE @DATA(itab_upto)
  UP TO 5 ROWS.
```
**Appending the result set to an existing internal table**. By appending, you avoid the deletion of existing lines in internal tables.
``` abap
"itab has a matching line type
SELECT * FROM dbtab
  WHERE ...
  APPENDING TABLE @itab.
```

If the target table does not have a matching type, you can use the addition `CORRESPONDING FIELDS OF`.
``` abap
SELECT * FROM dbtab
  WHERE ...
  APPENDING CORRESPONDING FIELDS OF TABLE @diff_itab.
```
**Reading single fields into individual variables**. Note that the number of columns specified (here, in the `FIELDS` clause) must match the number of elements in the `INTO` clause.
``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO (@res1,@res2,@res3).
```
**Reading into packages when reading into internal tables**. The
[package
size](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinto_clause.htm)
defines how many rows should be selected in one iteration. This is handy
in case a very large amount of data has to be processed that might be
too large for the memory capacity of an internal table, thus avoiding
program termination. The package size is specified by an integer value.
``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE ...
  INTO TABLE @DATA(itab_pack) PACKAGE SIZE i.
...
ENDSELECT.
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Further Clauses

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

<p align="right">(<a href="#top">back to top</a>)</p>

### Excursion: Operands and Expressions in ABAP SQL Statements

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

**SQL operands**

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
  "is not unique. ]
   zdemo_abap_flsch~cityto,

  'Lufthansa' AS name, "Untyped literal

  char`X` AS flag, "Typed literal

  @upto as num, "Host variable

  @( cl_abap_context_info=>get_system_date( ) ) as date "Host expression

  WHERE carrid = 'LH'          "Untyped literal
    AND countryfr = char`DE`   "Typed literal

  "Data object created inline and escaped with @
  INTO TABLE @DATA(it)

  "The following clause shows all options having the same effect
  UP TO 3 ROWS.             "Untyped numeric literal
  "UP TO int4`3` ROWS.      "Typed numerice literal
  "UP TO @upto ROWS.        "Host variable
  "UP TO @( 10 - 7 ) ROWS.  "Host expression
```

**SQL Expressions**

-   Expressions in an ABAP SQL statement that are passed to the database
    system for evaluation.
-   For example, SQL expressions can be specified as columns in the
    `SELECT` as demonstrated in most of the following examples.
    Find information on more possible positions and general information
    on SQL expressions
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapsql_expr.htm)
    and the subtopics there.

**Elementary expressions**

-   An elementary expression represents one of the four mentioned
    operands above: A value from the database (the column name) or
    values from an ABAP program passed to the database (literal, host
    variable or host expression).
-   As an example, see the `SELECT` list in the example above.
-   See more information
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_elem.htm).

**SQL functions**

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
  like_regexpr( pcre  = '..',  "Period that is followed by any character
                value = url ) AS like_regex,

  "Returns position of a substring in an expression,
  "3rd parameter = specifies offset (optional)
  "4th parameter = determines the number of occurrences (optional)
  "Result: 9
  locate( carrname, 'a', 0, 2 ) AS locate,

  "Searches a PCRE pattern, returns offset of match;
  "many optional parameters: occurrence, case_sensitive, start, group
  "Result: 21
  locate_regexpr( pcre = '..', "Period followed by any character
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
  occurrences_regexpr( pcre = '..',
                       value = url ) AS occ_regex,

  "Replaces the 2nd argument with the 3rd in an expression
  "Result: Lufth#ns#
  replace( carrname, 'a', '#' ) AS replace,

  "Replaces a found PCRE expression;
  "more parameters possible: occurrence, case_sensitive, start
   "Result: http://www#ufthansa#om
  replace_regexpr( pcre = '..',
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
  "Result:.lu
  substring_regexpr( pcre = '...',
                     value = url ) AS substring_regexpr,

  "All lower case letters are transformed to upper case letters
  "Result: LUFTHANSA
  upper( carrname ) AS upper

  FROM zdemo_abap_carr
  WHERE carrid = 'LH'
  INTO @FINAL(string_functions).
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
INTO @FINAL(special_functions).
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
  INTO TABLE @FINAL(agg_exp).
```

**More SQL Expressions**

-   [Arithmetic
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_arith.htm)
    to perform arithmetic calculations using the operators `+`,
    `-`, [`*]`, `/`,
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

[Window expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwindow_expression_glosry.htm "Glossary Entry")

How they work:

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
                     "UNBOUNDED PRECEDING: frame starts at the
                     "first row of the window
                    UNBOUNDED PRECEDING
                     "CURRENT ROW: determines starting or ending
                     "at the current row; here, it ends
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
                    "UNBOUND FOLLOWING:
                    "Determines the ending frame boundary,
                    "this addition specifies the last row of the window
                   AND UNBOUNDED FOLLOWING ) AS count_reverse,

  "Sorts the rows by some columns and calculates the rolling averages
  "of a subset of rows from column price. The subset consists of the
  "current row plus one preceding and one following row. Another use
  "case as below example that uses prices would be that, for example,
  "you can calculate the 3-day-average temperature for every day from
  "a list of temperature data.
  AVG( price AS DEC( 14,2 ) ) OVER( ORDER BY currency, fldate
       ROWS BETWEEN
        "n PRECEDING: for both start and end of frame;
        "frame to start/end n rows above the current row
       1 PRECEDING
        "n FOLLOWING: for both start and end of frame;
        "frame to start/end n rows beneath the current row
       AND 1 FOLLOWING ) AS avg

  FROM zdemo_abap_fli
  INTO TABLE @DATA(result).
```

### Excursion: SQL Conditions

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
 | `... [NOT] LIKE ...`         |      The content of an operand matches (does not match) a specified pattern. The pattern can be specified by using wildcard characters. `%` stands for any character string, including an empty string.◾`_` stands for any character.|
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
SELECT FROM dbtab
  FIELDS comp1, comp2, comp3
  WHERE comp1 = 'abc' "Equals some value

        "More example WHERE conditions:
        "comp2 > 100 "Greater than some value; alternatively GT is possible

        "Not equals plus an additional condition that must be respected
        "comp2 <> 100 AND comp4 = 'xyz'

        "(Not) between a value range
        "comp1 BETWEEN 1 AND 10

        "A character literal has a certain pattern, preceded and
        "followed by any string.
        "comp1 LIKE '%XYZ%'

        "The second character is not Y. _ stands for any character.
        "comp1 NOT LIKE '_Y%'

        "Contains one of the values specified in the parentheses
        "comp1 IN ( 'ABC', 'DEF', 'GHI' )

        "Does not contain one of the values specified in the parentheses
        "comp1 NOT IN ( 'JKL', 'MNO' )

         "Checking if an operand has an initial value
         "comp1 IS INITIAL

        "Combination of logical expression using AND, OR and parentheses
        "( comp1 = a AND comp2 < b ) OR ( comp3> c AND comp4 <> d )

  INTO TABLE @DATA(itab_where).
```


### Using SELECT when Reading from Multiple Tables

[`FOR ALL ENTRIES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_all_entries.htm)
addition: Reading data from a database table depending on the
content of an internal table.
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


> **💡 Note**<br>
>- The entire logical expression after `WHERE` is evaluated for each individual line in the internal table.
>- There must be at least one comparison with a column of the internal
table in the `WHERE` clause.
>- Ensure that the internal table from which to read is not initial. It is recommended that you use a subquery, which is shown in the next example, and a `SELECT` statement that reads from the internal table (`... ( SELECT ... FROM itab2 WHERE ...`).

**Using a subquery** with the addition `EXISTS` to read data from a database table depending
on data of another database table. More information:
[`EXISTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_logexp_exists.htm). The components of the table are referenced by `~`.
``` abap
SELECT comp1, comp2, comp3
  FROM dbtab AS tab1
  WHERE EXISTS
   ( SELECT comp1 FROM dbtab2
     WHERE comp1 = tab1~comp1 AND comp2 = tab1~comp2 )
  INTO ...
```

**Combining data of multiple database tables ...**

**... using an inner join**. In this kind of join, columns with rows of the left-hand side and those of the right-hand side are only joined if the rows meet join conditions (`ON ...`). If there are no equivalent entries in the first or second table, the rows are not joined.

If the same column name appears in multiple data sources of a single
join expression, these sources must be identified in all other additions
of the `SELECT` statement using the [column
selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_comp_selector_glosry.htm "Glossary Entry")
`~`.
``` abap
SELECT a~comp1, a~comp2, b~comp3, c~comp4
  FROM dbtab1 AS a
  INNER JOIN dbtab2 AS b
   ON a~comp1 = b~comp1 AND a~comp2 = b~comp2
  INNER JOIN dbtab3 AS c
   ON a~comp1 = c~comp1
  WHERE ...
  INTO ...
```

**... using a left outer join**. The columns of each row on the right-hand side that do not meet the `ON` condition are filled with initial values and linked with the columns of the left-hand side. If the conditions of the `WHERE` clause are met, each row on the left-hand side of the left outer join produces at least one row in the selection, irrespective of the `ON` condition.
``` abap
SELECT a~comp1, a~comp2, b~comp3,
  FROM dbtab1 AS a
  LEFT OUTER JOIN dbtab2 AS b
   ON a~comp1 = b~comp1
  WHERE ...
  INTO ...
```

**... using a union**. The columns of the result set keep the names defined in the statement on the left of `UNION`. The result set of rows of the `SELECT` statement on the right of `UNION` are inserted into the results set of the `SELECT` statement on the left of `UNION`.
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
> **💡 Note**<br>
> There are more join variants available. See the ABAP
Keyword Documentation on
[joins](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_join.htm)
for more information.


#### Excursion: Using Common Table Expressions (CTE)

When used:

-   Whenever you need intermediate results in a `SELECT`
    statement and especially if you need them more than once.
-   You get the option of selecting directly from a subquery [SELECT
    FROM subquery], which is not possible in ABAP SQL.

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

Setup of a statement with CTEs:

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
            ON c~carrid = s~carrid AND
                c~connid = s~connid )
SELECT *
        FROM +result
        ORDER BY name, connection
        INTO TABLE @DATA(result).
```


<p align="right">(<a href="#top">back to top</a>)</p>

## Changing Data in Database Tables

### Using INSERT

Using the ABAP SQL statement
[`INSERT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinsert_dbtab.htm),
you can insert one or more rows into a database table (or a [DDIC table
view](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentable_view_glosry.htm "Glossary Entry")).
As mentioned above, structures and internal tables from which to insert
content should be specified as host variables (with `@`) or host
expressions (with `@( ... )`) depending on your ABAP release
and strict syntax enforcement. The examples below all use the preceding
`@`. The system fields `sy-subrc` (0 = single row or all
rows inserted successfully, 4 = row not or not all rows inserted) and
`sy-dbcnt` (number of rows that are inserted) are set.

``` abap
"Inserting a single row into a database table

INSERT dbtab FROM @row.
INSERT INTO dbtab VALUES @row. "Alternative syntax, same effect

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

<p align="right">(<a href="#top">back to top</a>)</p>

### Using UPDATE

Using the ABAP SQL statement
[`UPDATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapupdate.htm),
you can update one or more rows in a database table. Similar to
`INSERT`, `sy-subrc` and `sy-dbcnt` are set.
After `FROM`, you can specify a structure or an internal table
as host variable (with `@`) or a host expression (with `@( ... )`).


``` abap
"Changing content by overwriting entire rows based on a work area

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

UPDATE dbtab FROM TABLE @ind_tab
             INDICATORS SET STRUCTURE comp_ind.

"Reverses the logic

UPDATE dbtab FROM TABLE @ind_tab
             INDICATORS NOT SET STRUCTURE comp_ind.

"SET addition: Changing values of specific fields in all table rows
"There are mutliple options for the value assignment. E. g. you can use
"a literal, host variable/expression, SQL function, and so on.

UPDATE dbtab SET comp2 = ... .
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Using MODIFY

Using the ABAP SQL statement
[`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_dbtab.htm),
you can insert one or more rows in a database table or overwrite
existing ones. Similar to the statements above, `sy-subrc` and
`sy-dbcnt` are set. After `FROM`, you can specify a
structure or an internal table as host variable (with `@`) or a
host expression (with `@( ... )`).

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

<p align="right">(<a href="#top">back to top</a>)</p>

### Using DELETE

Using the ABAP SQL statement
[`DELETE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdelete_dbtab.htm),
you can delete one or more rows in a database table. Similar to the
statements above, `sy-subrc` and `sy-dbcnt` are set.

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

<p align="right">(<a href="#top">back to top</a>)</p>

## Further Information
Find more information on the topics covered here or not (e. g. topics like [table buffering](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensap_puffering.htm)) in the respective sections in the ABAP Keyword Documentation.


## Executable Example
[zcl_demo_abap_sql](./src/zcl_demo_abap_sql.clas.abap)

Note the steps outlined [here](README.md#🎬-getting-started-with-the-examples) about how to import and run the code.
