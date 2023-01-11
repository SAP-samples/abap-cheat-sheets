<a name="top"></a>

# Working with Internal Tables

- [Working with Internal Tables](#working-with-internal-tables)
  - [Internal Tables ...](#internal-tables-)
  - [Declaring Internal Tables](#declaring-internal-tables)
    - [Characteristics](#characteristics)
    - [Excursion: Primary, Secondary and Empty Table Keys](#excursion-primary-secondary-and-empty-table-keys)
  - [Working with Internal Tables](#working-with-internal-tables-1)
    - [Creating Internal Tables](#creating-internal-tables)
    - [Filling and Copying Internal Table Content](#filling-and-copying-internal-table-content)
      - [Excursions](#excursions)
    - [Reading from Internal Tables](#reading-from-internal-tables)
    - [Processing Multiple Internal Table Lines Sequentially](#processing-multiple-internal-table-lines-sequentially)
    - [Sorting Internal Tables](#sorting-internal-tables)
    - [Modifying Internal Table Content](#modifying-internal-table-content)
    - [Deleting Internal Table Content](#deleting-internal-table-content)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)


## Internal Tables ...

-   are [data
    objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm "Glossary Entry")
    in ABAP. Their [data
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_type_glosry.htm "Glossary Entry")
    is a [table
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_type_glosry.htm "Glossary Entry").
-   can be seen as collections of table lines.
-   usually take up data from a fixed structure and store it in the
    working memory in ABAP, i. e. the data is stored line by line in
    memory, and each line has the same structure.
-   are relevant ...
    -   whenever you want to process a data set with a fixed structure
        within a program.
    -   when managing multiple related data records of the same data
        type in a single variable.
    -   for storing and formatting data from a database table within a
        program. Note: Due to their existence in memory, the
        data access with internal tables is a lot faster than accessing
        the data on database tables.
-   are declared within ABAP source code.
-   are dynamic data objects, i. e. they can be processed in many
    different ways:
    -   table lines can, for example, be inserted, deleted, or updated.
    -   the way how to access the tables can vary, e. g. access by index
        or key, and they can be processed sequentially in a loop.
-   are only temporarily available in the memory; after the program has
    been terminated, the content of an internal table is not available
    any more.
-   are simple to manage for developers since the runtime system is
    responsible for the memory management, i. e. the runtime system
    calculates an appropriate initial memory allocation for the internal
    table when it is declared; when you add more data to the table, the
    table grows automatically; when you empty the table, the system
    automatically releases excess memory.
-   are characterized by their line types, table categories and key
    attributes.

<p align="right">(<a href="#top">back to top</a>)</p>

## Declaring Internal Tables

The relevant syntactical element is `TABLE OF` in combination
with
[`TYPES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes.htm)
(to declare an internal table type) and
[`DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata.htm)
(to create the internal table) and the additions
[`TYPE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_simple.htm)
or
[`LIKE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_referring.htm).
See more details and examples in section [Creating Internal Tables](#creating-internal-tables) further down.

Examples
``` abap
TYPES itab_type1 TYPE STANDARD TABLE OF data_type ...
TYPES itab_type2 LIKE SORTED   TABLE OF data_object ...
DATA  itab1      TYPE          TABLE OF data_type ...
DATA  itab2      TYPE HASHED   TABLE OF data_type ...
DATA  itab3      TYPE                   table_type ...
DATA  itab4      LIKE                   table ...`
```

> **💡 Note**<br>
>- If the table category is not specified (`... TYPE TABLE OF ...`), it is automatically `... TYPE STANDARD TABLE OF ...`.
>- Internal tables can be [declared
    inline](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry") in various contexts, for example, using `DATA(...)`.


<p align="right">(<a href="#top">back to top</a>)</p>

### Characteristics

Each internal table is characterized by three aspects. More details: [Internal Tables -
Overview](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenitab_oview.htm).


<details>
  <summary>Expand to view the characteristics</summary>
  <!-- -->

**Line Type**

- Defines how each line of the internal table is set up, i. e. it describes what columns the table has.
- It can be any ABAP data type, e. g. a structure or an internal table.
- In most cases, the line type is a structure, which means that every line in the internal table contains a column with the name and type of the corresponding structure component.
- In a simple case, the line consists of a flat structure with elementary data objects; however, it can also be a deep structure whose components can be structures themselves or even internal tables.

**Table Category**

- Determines how internal tables are managed and stored internally as well as how individual table entries will be accessed.
- Why relevant? The different approaches to accessing the data can make significant performance differences.
- Note: There are two ways of accessing internal tables:
   - Access by index: A line of an internal table is addressed by its line number.
   - Access by key: A line of an internal table is addressed by looking for particular values in particular columns. Note: The columns in which you search may be key columns, but it can also be non-key columns.
- There are three table categories:

| Category | Details | When to use | Hints |
|---|---|---|---|
| `STANDARD` | <ul><li>content is not stored in a particular sort order (but can be sorted with `SORT`), yet they have an internal linear index (that is also true for sorted tables, hence, both are called index tables)</li><li> can be accessed by index and key</li><li>the key is never unique, hence duplicate entries are always allowed; adding new lines is fairly quick since there is no check necessary if an entry already exists</li><li>a standard table can be even declared with an empty key if the key is not needed (addition `WITH EMPTY KEY`)</li></ul> | <ul><li>if accessing single entries via their index or sequential processing are the primary use cases</li><li>rule of thumb: if you add a lot to internal tables and do not read from them too often, standard tables are a good choice; if you read a lot from internal tables and modify them often, sorted and hashed tables are a good choice </li></ul>| <ul><li>avoid large standard tables if the table is accessed mainly via the primary table key</li><li>lines should be added using `APPEND`</li><li>Read, modify and delete operations should be done by specifying the index (i. e. using the `INDEX` ABAP word with the relevant ABAP command)</li><li>Standard tables have the least administration costs compared to hashed and sorted tables</li><li>if the access by key is the primary access type, large standard tables (i. e. more than 100 lines) are not the appropriate table category because of the linear search </li></ul>|
| `SORTED` | <ul><li>content of sorted tables is always and automatically sorted by the table key in ascending order</li><li>they have an internal linear index (that is also true for standard tables, hence, both are called index tables)</li><li>can be accessed by index and key</li><li>the key can be either unique or non-unique</li></ul> | <ul><li>if a fast access to single entries via their key is the primary use case</li><li>it is also suitable for partially sequential processing in loops (e. g. when specifying the table key and the `WHERE` condition) or index access; in principle, the data access is more efficient than with standard tables since the data is always sorted </li></ul>| <ul><li>Sorted tables have lower administration costs compared to hashed tables</li><li>Note: Depending on the length of the key and the number of lines in an internal table, data access in the context of a sorted table might be as fast as or even faster than using a hashed table. Hence, if the table is not too big and the memory space is critical, a sorted table is preferable to a hashed table.</li></ul> |
| `HASHED` | <ul><li>hashed tables do not have a linear index; they are managed using a special hash algorithm</li><li>can be accessed by a unique key</li></ul> | <ul><li>used for large tables (e. g. if you want to use an internal table which resembles a large database table or for generally processing large amounts of data)</li><li>if a key access is the primary use case and a unique key can be defined </li></ul>| <ul><li> duplicates are never allowed</li><li> the response time is constant and independent of the number of table entries since the table entries are accessed using a hash algorithm</li><li> hashed tables guarantee fast access but have the highest administration costs due to a greater memory overhead </li></ul>|

**Key Attributes**

- A table key identifies table lines.
- There are two possible key types: primary table key and secondary table key.
- A primary table key ...
   - is contained in every internal table.
   - is either a self-defined key or a standard key. You can make further specifications for the key, for example, whether the key is to be unique or non-unique, i. e. more than one line with the same key (duplicates) can exist in the internal table.
   - can also be empty, i. e. it does not contain any key fields.
   - has the predefined name `primary_key` with which it can also be addressed explicitly in various statements (but its use is optional). You can also specify an alias name for the primary key. Note that in table expressions, `primary_key` or an alias name must be specified if the primary key is to be used explicitly.
   - can also be composed of the entire line of the internal table. In this case, the pseudo component `table_line` can be used to denote the primary table key.
- A secondary table key ...
   - is optional.
   - is either a unique or non-unique sorted key or a unique hash key.
</details>





<p align="right">(<a href="#top">back to top</a>)</p>

### Excursion: Primary, Secondary and Empty Table Keys

<details>
  <summary>Expand to view the details</summary>
  <!-- -->

*Primary table keys*

Standard key:

- The standard key is a special primary table key.
- Standard key of an internal table with a ...
   - structured line type: The primary table key consists of all fields having character-like and byte-like data types.
   - non-structured/elementary line type: The whole table is the key (`table_line`).
- Note: An internal table with no explicit specification of keys implicitly has the standard table key as a primary table key.
- Why respecting standard keys matters:
  - A sorting of a table can lead to unexpected results.
  - Since the standard key might consist of many fields, it impacts the performance when accessing the internal table via the keys.
  - The key fields of the primary table key of sorted and hashed tables are always read-only, i. e. using the standard key with those table categories and then (inadvertently) modifying fields can cause unexpected runtime errors.
  - An explicit specification of keys has the advantage of providing a better readability and understandability of your code and you avoid setting the standard key by mistake.

Examples using `DATA` statements:
``` abap
"Standard table with implicit default key; all non-numeric table
"fields compose the primary table key

DATA it1 TYPE TABLE OF zdemo_abap_fli.

"explicitly specifying the standard table key; same as it1

DATA it2 TYPE STANDARD TABLE OF zdemo_abap_fli WITH DEFAULT KEY.

"Hashed table with unique standard table key

DATA it3 TYPE HASHED TABLE OF zdemo_abap_fli WITH UNIQUE DEFAULT KEY.

"Sorted table with non-unique standard table key

DATA it4 TYPE SORTED TABLE OF zdemo_abap_fli WITH NON-UNIQUE DEFAULT KEY.

"Elementary line type; the whole table line is the standard table key

DATA it5 TYPE TABLE OF i.
```

*Explicit declaration of the primary table key*

- By specifying the uniqueness, you can explicitly declare the primary table key.
- As mentioned above, the predefined name `primary_key` can be used followed by a list of components.
- An alias name for the primary key can be specified, too.

See the comments in the following examples for more information.

``` abap
"Explicitly specified primary table keys
"Standard tables: only NON-UNIQUE possible

DATA it6 TYPE TABLE OF zdemo_abap_fli WITH NON-UNIQUE KEY carrid.

"Standard tables: only KEY specified, NON-UNIQUE is added implicitly

DATA it7 TYPE TABLE OF zdemo_abap_fli WITH KEY carrid.

"Sorted tables: both UNIQUE and NON-UNIQUE possible

DATA it8 TYPE SORTED TABLE OF zdemo_abap_fli
  WITH UNIQUE KEY carrid connid.

DATA it9 TYPE SORTED TABLE OF zdemo_abap_fli
  WITH NON-UNIQUE KEY carrid connid cityfrom.

"Hashed: UNIQUE KEY must be specified

DATA it10 TYPE HASHED TABLE OF zdemo_abap_fli
  WITH UNIQUE KEY carrid.

"Explicitly specifying primary_key and listing the components; same as it6 and it7

DATA it11 TYPE TABLE OF zdemo_abap_fli
  WITH KEY primary_key COMPONENTS carrid.

"Same as it9

DATA it12 TYPE SORTED TABLE OF zdemo_abap_fli
  WITH NON-UNIQUE KEY primary_key COMPONENTS carrid connid cityfrom.

"An alias is only possible for sorted/hashed tables

DATA it13 TYPE SORTED TABLE OF zdemo_abap_fli
  WITH NON-UNIQUE KEY primary_key
  ALIAS p1 COMPONENTS carrid connid cityfrom.

"An alias is used for the key which is composed of the entire line

DATA it14 TYPE HASHED TABLE OF zdemo_abap_fli
  WITH UNIQUE KEY primary_key
  ALIAS p2 COMPONENTS table_line.
```
> **💡 Note**<br>
> The specification for the primary key can only be omitted for standard tables. The primary table key is then defined automatically as a non-unique standard key.

*Empty key*
-   A standard table can be specified with an empty key, i. e. it does
    not contain any key fields.
-   This is not possible for sorted and hashed tables. With these table
    categories, the primary table key must be specified explicitly and,
    thus, cannot be empty.
-   Internal tables with empty key are used if the order of the entries
    based on key values is of no relevance for the filling and
    accessing.
-   However, they should be used with care to avoid unexpected
    results e. g. when sorting those tables.
-   You might want to define a table with an empty key instead of not
    specifying a key definition at all since otherwise the standard key
    is used which must be handled with care, too, as mentioned above.
-   Declaration:
    -   Explicit declaration with the addition `EMPTY KEY`
    -   Implicit declaration when using the standard key if a structured
        line type does not contain non-numeric elementary components or
        if an unstructured line type is table-like.

> **💡 Note**<br>
> When using an [inline
        declaration](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry")
        like `... INTO TABLE @DATA(itab) ...` in
        `SELECT` statements, the resulting table is a standard
        table and has an empty key.

Examples:
``` abap
"Empty keys only possible for standard tables

DATA it15 TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

"The inline declaration produces a table with empty key

SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(it16) UP TO 3 ROWS.
```

*Secondary table keys*

- Secondary table keys can be optionally specified for all table categories.
- There are two kind of secondary table keys: unique or non-unique sorted keys or unique hash keys.
- Secondary keys always have a self-defined name. An alias can be defined for a secondary key, too.
- A secondary table index is created internally for each sorted secondary key. This enables index access to hashed tables via the secondary key. In this case, `sy-tabix` is set.
- Use cases of secondary table keys:
  - To improve the performance of data retrieval from internal tables and guarantee uniqueness when accessing data
  - To enable optimized access to standard tables (huge advantage: secondary keys can be added to existing standard tables, thus, gaining the benefits of the other table types with respect to performance)
  - Mainly used for very large internal tables (where only few modifications occur afterwards); not suitable for small internal tables (less than 50 lines) since each secondary key means additional administration costs (they consume additional memory)
- If you want to make use of this key in ABAP statements, for example, `READ`, `LOOP AT` or `MODIFY` statements, the key must be specified explicitly using the appropriate additions, for example, `WITH ... KEY ... COMPONENTS` or `USING KEY`.
- Find more details in the programming guidelines on secondary keys: [Secondary
    Key (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensecondary_key_guidl.htm "Guideline").

Examples:
``` abap
DATA it17 TYPE TABLE OF zdemo_abap_fli                      "standard table
  WITH NON-UNIQUE KEY carrid connid                         "primary key
  WITH UNIQUE SORTED KEY cities COMPONENTS cityfrom cityto. "secondary key

DATA it18 TYPE HASHED TABLE OF zdemo_abap_fli               "hashed table
  WITH UNIQUE KEY carrid connid
  WITH NON-UNIQUE SORTED KEY airports COMPONENTS airpfrom airpto.

DATA it19 TYPE SORTED TABLE OF zdemo_abap_fli              "sorted table
  WITH UNIQUE KEY carrid connid
  WITH UNIQUE HASHED KEY countries COMPONENTS countryfr countryto.

"primary_key explicitly specified + multiple secondary keys

DATA it20 TYPE TABLE OF zdemo_abap_fli
  WITH NON-UNIQUE KEY primary_key COMPONENTS carrid connid
  WITH NON-UNIQUE SORTED KEY cities COMPONENTS cityfrom cityto
  WITH UNIQUE HASHED KEY airports COMPONENTS airpfrom airpto.

"Alias names for secondary table keys (and primary table key, too)

DATA it21 TYPE SORTED TABLE OF zdemo_abap_fli
  WITH NON-UNIQUE KEY primary_key ALIAS k1 COMPONENTS carrid connid city
  WITH NON-UNIQUE SORTED KEY cities ALIAS s1 COMPONENTS cityfrom cityto
  WITH UNIQUE HASHED KEY airports ALIAS s2 COMPONENTS airpfrom airpto.

"Example for key usage using a LOOP AT statement; all are possible

LOOP AT it21 INTO DATA(wa) USING KEY primary_key.
"LOOP AT it21 INTO DATA(wa) USING KEY k1.
"LOOP AT it21 INTO DATA(wa) USING KEY cities.
"LOOP AT it21 INTO DATA(wa) USING KEY s1.
"LOOP AT it21 INTO DATA(wa) USING KEY airports.
"LOOP AT it21 INTO DATA(wa) USING KEY s2.
...
ENDLOOP.
```
</details>





<p align="right">(<a href="#top">back to top</a>)</p>

## Working with Internal Tables

### Creating Internal Tables

As a best practice for declaring internal tables, it is recommended that
an internal table with this pattern is created in a program:

-   Defining a structured data type (locally or globally; it is not
    needed if you refer to a globally available type, for example, a
    database table whose line type is automatically used when defining a
    an internal table type or creating the variable)
-   Defining an internal table type
-   Creating a variable, i. e. the internal table, that refers to that
    type.

You will also see internal tables that are declared by combining the
variable creation and table type definition in one go. If the structured
data and internal table types are globally available in the DDIC, a
local definition within a program is not needed.

Example:

The following example shows the pattern and various examples of declaring internal tables and types by including the local definition of
structured data and internal table types for demonstration purposes.

``` abap
"1. Defining line type locally

TYPES: BEGIN OF ls_loc,
        key_field TYPE i,
        char1     TYPE c LENGTH 10,
        char2     TYPE c LENGTH 10,
        num1      TYPE i,
        num2      TYPE i,
      END OF ls_loc.

"2. Defining internal table types
"All of the examples use the short form:
"TYPE TABLE OF instead of TYPE STANDARD TABLE OF

TYPES:
  "Standard table type based on locally defined structure type.
  tt_loc_str TYPE TABLE OF ls_loc WITH NON-UNIQUE KEY key_field,

  "Based on global structure type
  tt_gl_str TYPE TABLE OF demo_cs_struc WITH NON-UNIQUE KEY key_field,

  "Based on database table (could also be, e. g. a CDS view)
  "In this case, the line type of the table is automatically used.
  tt_gl_tab TYPE TABLE OF demo_cs_dbtab WITH NON-UNIQUE KEY key_field,

  "Based on an elementary type
  tt_el_type TYPE TABLE OF i.

"3. Creating internal tables ...
"... from locally defined table types

DATA: itab_a1 TYPE tt_loc_str,
      itab_a2 TYPE tt_gl_str,
      itab_a3 TYPE tt_gl_tab,
      itab_a4 TYPE tt_el_type.

"... from global table types
DATA itab_a5 TYPE string_table.

"Other declaration options with DATA
"To save the extra creation of the table type, you can include the table category
"and key info in the data declaration directly.
DATA itab_a6 TYPE TABLE OF ls_loc WITH NON-UNIQUE KEY key_field.

"Internal table based on an already existing internal table using LIKE.
DATA itab_a7 LIKE TABLE OF itab_a6.
```

<p align="right">(<a href="#top">back to top</a>)</p>

**Excursion: Declaring internal tables inline**

To produce leaner and more readable code and create variables in the
place where you need them, you can make use of [inline
declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry").
Such inline declarations are possible in appropriate [declaration
positions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_positions.htm)
if the operand type can be determined completely, for example, using a
`DATA` statement (or `FINAL` for immutable variables) as shown in the following examples:

``` abap
"Table declared inline in the context of an assignment
"The examples show the copying of a table including the content on the fly
"and creating the table in one step. The data type of the
"declared variable is determined by the right side.

DATA(it_inline1) = it.
DATA(it_inline2) = it_inline1.

"Using the VALUE operator and an internal table type

DATA(it_inline3) = VALUE table_type( ( ... ) ).

"Table declared inline in the context of a SELECT statement;
"a prior extra declaration of an internal table is not needed.

DATA it TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

SELECT * FROM zdemo_abap_fli INTO TABLE @it.

SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(it_inline4).
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Filling and Copying Internal Table Content

You can use the ABAP keywords
[`APPEND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapappend.htm)
and
[`INSERT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinsert_itab.htm)
to add lines to internal tables.

<details>
  <summary>Notes on the use</summary>
  <!-- -->

-   `APPEND` ...
    -   always adds lines at the bottom of the internal table.
    -   is unproblematic for standard tables for which lines are managed
        via an index. When using the statement, the system field
        `sy-tabix` is given the index of the recently added
        line. `sy-tabix` is always set on the index with respect
        to the primary table index.
    -   cannot be used for hashed tables. With regard to sorted tables,
        lines are only appended if they match the sort order and do not
        create duplicate entries if the primary table key is unique.
        Hence, `INSERT` should be used when adding lines to
        sorted tables.
-   `INSERT` ...
    -   can be used to add lines at a specific position in tables (by
        specifying the target index). In doing so, all the following
        lines are moved down one position.
    -   without specifying the position adds the lines at the bottom of
        the table in case of standard tables. However, when using
        `INSERT`, `sy-tabix` is not set unlike
        `APPEND`. In case of sorted tables, the line is
        automatically inserted at the right position.
    -   [Note:] In case of unique primary table keys in sorted
        and hashed tables, the table cannot have entries with duplicate
        keys. If a duplicate is inserted, the insertion fails and the
        system field `sy-subrc` is set to 4.
</details>

**Adding a line to the internal table**. The example shows both a structure that is created using the `VALUE` operator and added as well as
an existing structure that is added.

``` abap
APPEND VALUE #( comp1 = a comp2 = b ... ) TO itab.
APPEND lv_struc TO itab.

INSERT VALUE #( comp1 = a comp2 = b ... ) INTO TABLE itab.
INSERT lv_struc INTO itab.
```

**Adding an initial line** to the internal table without providing any field values.

``` abap
APPEND INITIAL LINE TO itab.

INSERT INITIAL LINE INTO TABLE itab.
```

Adding all lines from another internal table.

``` abap
APPEND LINES OF itab2 TO itab.

INSERT LINES OF itab2 INTO TABLE itab.
```

**Adding lines from another internal table with a specified index range**.
You do not need to use both `FROM` and `TO` in one
statement. You can also use just one of them. When using only
`FROM`, all lines are respected until the final table entry.
When using only `TO`, all lines are respected starting from the
first table entry.

``` abap
"i1/i2 represent integer values

APPEND LINES OF itab2 FROM i1 TO i2 TO itab.

APPEND LINES OF itab2 FROM i1 TO itab.

APPEND LINES OF itab2 TO i2 TO itab.

INSERT LINES OF itab2 FROM i1 TO i2 INTO itab.
```

**Inserting one line or multiple lines from another internal table at a specific position**. `FROM` and `TO` can be used here, too.

``` abap
INSERT lv_struc INTO itab2 INDEX i.

INSERT LINES OF itab2 INTO itab INDEX i.
```

<p align="right">(<a href="#top">back to top</a>)</p>

**Adding lines using constructor expressions**

As already touched on above, table lines that are constructed inline as
arguments of, for example, the `VALUE` operator can be added to
internal tables. The operator allows you to fill an internal table with
a compact expression. In the cases below, internal tables are filled
using constructor expressions in the context of
[assignments](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_glosry.htm "Glossary Entry").

In the example below, the internal table is filled by assigning an
internal table that is constructed inline using the `VALUE`
operator. The table constructed inline has two lines. `line`
represents an existing structure having an appropriate line type. The
other line is constructed inline.


> **💡 Note**<br>
> The extra pair of brackets represents a table line. The # sign denotes that the line type can be derived from the context. The assignment clears the existing content of the internal table on the left side.

``` abap
itab = VALUE #( ( line )
                ( comp1 = a comp2 = b ...  ) ).
```

*Excursion*: **Creating a internal table by inline declaration** and adding lines using a constructor expression
``` abap
"Internal table type
TYPES it_type LIKE itab.

"Inline declaration
"The # sign would not be possible here since the line type
"cannot be derived from the context.

DATA(it_in) = VALUE it_type( ( comp1 = a comp2 = b ... )
                             ( comp1 = c comp2 = d ...  ) ).
```
When using the assignments above (`itab = ...`), the internal table is initialized and the existing content is deleted. To add new
lines **without deleting existing content**, use the addition [`BASE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenvalue_constructor_params_itab.htm).
``` abap
itab = VALUE #( BASE itab ( comp1 = a comp2 = b ... )
                          ( comp1 = c comp2 = d ... ) ).
```

**Adding lines of other tables** using the addition `LINES OF`.
> **💡 Note**<br>
> Without the addition `BASE` existing content is deleted. The line type of the other internal table must match the one of
the target internal table.
``` abap
itab = VALUE #( ( comp1 = a comp2 = b ...)
                ( comp1 = a comp2 = b ...)
                ( LINES OF itab2 )
                ... ).
```
A simple assignment without a constructor expression that **copies the content from another internal table** that has the **same line type** (note that the existing content in `itab` is deleted).
``` abap
itab = itab2.
```
**Copying content from another internal table** that has a **different line
type** using the
[`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm)
operator. Note that the existing content is deleted.

As an alternative to the `CORRESPONDING` operator, statements
with
[`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove-corresponding.htm)
can be used.
``` abap
itab = CORRESPONDING #( itab3 ).

MOVE-CORRESPONDING itab3 TO itab.
```

Copying content from another internal table that has a different line type using the `CORRESPONDING` operator while **keeping existing
content**. The addition `KEEPING TARGET LINES` for the `MOVE-CORRESPONDING` statement preserves the table content.

``` abap
itab = CORRESPONDING #( BASE ( itab ) itab3 ).

MOVE-CORRESPONDING itab3 TO itab KEEPING TARGET LINES.
```

Using the `MAPPING` addition of the `CORRESPONDING` operator, you can specify components of a source table that are assigned to the components of a target table in **mapping relationships**. In elementary components, the assignment is made in accordance with the associated assignment rules.

``` abap
itab = CORRESPONDING #( itab3 MAPPING a = c b = d ).
```
Using the `EXCEPT` addition of the `CORRESPONDING` operator, you can **exclude components from the assignment**. This is particularly handy if there are identically named components in the source and target table that are not compatible or convertible. In doing so, you can avoid syntax errors or runtime errors. Instead of a component list, `EXCEPT` can also be followed by `*` to exclude all components that are not mentioned in a preceding mapping of components. If `EXCEPT *` is used without the
`MAPPING` addition, all components remain initial.
``` abap
itab = CORRESPONDING #( itab3 EXCEPT e ).

itab = CORRESPONDING #( itab3 EXCEPT * ).
```
Using the `DISCARDING DUPLICATES` addition of the `CORRESPONDING` operator, you can **prevent runtime errors if duplicate lines are assigned** to the target table that are defined to only accept unique keys. In that case, the duplicate line in the source table is ignored. The addition can also be specified with `MAPPING ...`.

``` abap
itab = CORRESPONDING #( itab2 DISCARDING DUPLICATES ).
```

Copying data from a **deep internal table** to another deep internal table. If used, the addition `BASE` keeps the content. See also the alternative `MOVE-CORRESPONDING` statements.
``` abap
itab_nested2 = CORRESPONDING #( DEEP itab_nested1 ).

itab_nested2 = CORRESPONDING #( DEEP BASE ( itab_nested2 ) itab_nested1 )

MOVE-CORRESPONDING itab_nested1 TO itab_nested2 EXPANDING NESTED TABLES.

MOVE-CORRESPONDING itab_nested1 TO itab_nested2 EXPANDING NESTED TABLES KEEPING TARGET LINES.
```

<p align="right">(<a href="#top">back to top</a>)</p>

#### Excursions

Adding multiple lines from a database table to an internal table using
[`SELECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect.htm),
for example, based on a condition. In the case below, the internal table
is created inline. If the variable exists, it is `... @itab`.
In this case, it is assumed that `itab` has the same line type
as the database table. Note the `@` character before the internal
table (see [host
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_host_expressions.htm)).
There are many more syntax options for `SELECT` statements.
``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2 ...
  WHERE ...
  INTO TABLE @DATA(itab_sel).
```

**Sequentially adding multiple rows** from a database table to an internal table using `SELECT ... ENDSELECT.`, for example, based on a
condition. In this case, the selected data is first stored in a structure which can be further processed and added to an internal table.

``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2 ...
  WHERE ...
  INTO @DATA(struc_sel).

  IF sy-subrc = 0.
    APPEND struc_sel TO itab.
  ...
  ENDIF.
ENDSELECT.
```
Adding multiple lines from a database table using `SELECT`, for example, based on a condition, if the database table has a different
line type as the internal table. The `*` sign means that all fields are selected. In the other examples, specific fields are defined.
The addition `APPENDING CORRESPONDING FIELDS INTO TABLE` adds the selected data to the bottom of the table without deleting existing
table entries. The addition `INTO CORRESPONDING FIELDS OF TABLE` adds lines and deletes existing table entries.
``` abap
SELECT FROM dbtab2
  FIELDS *
  WHERE ...
  APPENDING CORRESPONDING FIELDS OF TABLE @itab.

SELECT FROM dbtab2
  FIELDS *
  WHERE ...
  INTO CORRESPONDING FIELDS OF TABLE @itab.
```
Adding multiple lines **from an internal table to another internal table** using `SELECT`. Note the alias name that must be defined for the
internal table.
``` abap
SELECT comp1, comp2, ...
  FROM @itab2 AS it_alias
  INTO TABLE @DATA(itab_sel).
```
**Combining data of multiple tables** into an internal table using an [inner
join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninner_join_glosry.htm "Glossary Entry").
In below example, data of an internal and a database table is joined
with a `SELECT` statement and the addition [`INNER
JOIN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_join.htm).
Note the field list including fields from both tables. The fields are
referred to using `~`.
``` abap
SELECT it_alias~comp1, it_alias~comp2, dbtab~comp3 ...
  FROM @itab AS it_alias
  INNER JOIN dbtab ON it_alias~comp1 = dbtab~comp1
  INTO TABLE @DATA(it_join_result).
```

Filling an internal table from a database table using
[subqueries](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubquery_glosry.htm "Glossary Entry").
In both of the following examples, an internal table is filled from a
database table. In the first example, a subquery is specified in the
`WHERE` clause with the ABAP words ` NOT IN`. A check is
made to verify whether a value matches a value in a set of values
specified in parentheses. In the second example, an internal table is
filled depending on data in another table. A subquery is specified in
the `WHERE` clause with the ABAP word ` EXISTS`. In this
case, it checks the result of the subquery that consists of another
`SELECT` statement, i. e. a check is made if an entry exists in
a table based on the specified conditions.

``` abap
SELECT comp1, comp2, ...
  FROM dbtab
  WHERE comp1 NOT IN ( a, b, c ... )
  INTO TABLE @DATA(it_subquery_result1).

SELECT comp1, comp2, ...
  FROM dbtab
  WHERE EXISTS ( SELECT 'X' FROM @itab AS itab_alias
                 WHERE comp1 = dbtab~comp1 )
  INTO TABLE @DATA(it_subquery_result2).
```

Filling internal table from a table based on the existence of data in
another table using the addition [`FOR ALL
ENTRIES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_all_entries.htm).

> **💡 Note**<br>
> Ensure that the internal table from which to read is not initial. It is therefore recommended that a subquery is used as shown above: `... ( SELECT ... FROM @itab AS itab_alias WHERE ...`).

``` abap
IF itab IS NOT INITIAL.
SELECT dbtab~comp1, dbtab~comp2, ...
  FROM dbtab
  FOR ALL ENTRIES IN @itab
  WHERE comp1 = @itab-comp1
  INTO TABLE @DATA(it_select_result).
ENDIF.
```

Creating an internal table by copying data from another internal table
filtering out lines that do not match the `WHERE` condition using the [`FILTER`
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_filter.htm).

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
"FILTER an conditions based on single values
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

*Excursion:* Collecting values

Use the
[`COLLECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcollect.htm)
keyword, for example, to add the values of numeric components to the
corresponding values in an internal table. Use it mainly for internal
tables with a unique primary key, especially hashed tables.
``` abap
COLLECT VALUE dtype( comp1 = a comp2 = b ... ) INTO itab.
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Reading from Internal Tables

There are three different ways of specifying the line to be read:

- via index (index tables only)
- via table keys (only tables having keys defined)
- via free key

**Reading single lines**

*Determining the target area*

-   Copying a line to a data object using the addition `INTO`.
    After the copying, the found line exists in the internal table and
    in the data object separately from each other. So, if you change the
    data object or the table line, the change does not affect the other.
    However, you can modify the copied table line and use a
    `MODIFY` statement to modify the table based on the changed
    table line (see below). The addition `TRANSPORTING`
    specifies which components are to be respected for the copying. If
    it is not specified, all components are respected.
    ``` abap
    READ TABLE itab INTO dobj ...   "dobj must have the table's structure type

    READ TABLE itab INTO DATA(dobj_inl) ...

    READ TABLE itab INTO ... TRANSPORTING comp1 [comp2 ... ].
    ```

-   Assigning a line to a [field
    symbol](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm "Glossary Entry"),
    for example, using an inline declaration (`ASSIGNING <fs>`). If you then access the field symbol, it means
    accessing the found table line. There is no actual copying of
    content. Hence, modifying operations on the field symbol mean
    modifying the table line directly. Note that the addition
    `TRANSPORTING` is not possible since the entire table is
    assigned to the field symbol.

    ``` abap
    READ TABLE itab ASSIGNING <fs1> ...

    READ TABLE itab ASSIGNING FIELD-SYMBOL(<fs2>) ...
    ```

-   Reading a line into a [data reference
    variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry")
    using `REFERENCE INTO`. In this case, no copying takes place
    either. Modifications of the table are possible via the [data
    reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_glosry.htm "Glossary Entry");
    and the addition `TRANSPORTING` is not possible either.

    ``` abap
    READ TABLE itab REFERNCE INTO dref ...

    READ TABLE itab REFERNCE INTO DATA(dref_inl) ...
    ```

**What to use then?** Since all syntax options principally offer the same
functionality, it is up to you and your use case. For example,
performance or readability of the code play a role. See more information
in the programming guidelines on the [target
area (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentable_output_guidl.htm "Guideline").
One obvious use case for `INTO dobj` is when the table should
not be modified via the copied table line. However, the copying comes
with performance costs. Imagine your table contains lots of columns or
nested components. Not copying at all in such a case is more performant
(however, you can certainly restrict the fields to be copied using the
`TRANSPORTING` addition).

<p align="right">(<a href="#top">back to top</a>)</p>

*Reading a single line by index*

The following example shows `READ TABLE` statements to read a single line from an internal table by specifying the index. The addition `USING
KEY` can be used to specify a table key and, thus, determine the table index to be used explicitly. If the table has a sorted secondary
key, the addition can be specified and the line to be read is then determined from its secondary table index. If the primary table key is
specified using its name `primary_key`, the table must be an index table, and the behavior is the same as if `USING KEY` was
not specified.
``` abap
READ TABLE itab INTO wa INDEX i.

READ TABLE itab INTO wa INDEX i USING KEY primary_key.
```

Using a [table
expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_expressions.htm),
the read result is stored in a variable that might be declared inline.
The number in the square brackets represents the index. A line that is
not found results in an runtime error. Hence, to avoid an error, you can
use a [`TRY ... CATCH ... ENDTRY.`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptry.htm) block.

``` abap
DATA(lv1) = itab[ i ].

TRY.
  DATA(lv2) = itab[ i ].
  CATCH cx_sy_itab_line_not_found.
  ...
ENDTRY.

DATA(lv3) = itab[ KEY primary_key INDEX i ].

"Copying a table line via table expression and embedding in constructor expression
DATA(lv4) = VALUE #( itab[ i ] ).

"Reading into data reference variable using the REF operator
DATA(lv5_ref) = REF #( itab[ i ] ).
```

If you read a nonexistent line via a table expression, the exception
raising is not always desired. You can also embed the table expression
in a constructor expression using the addition `OPTIONAL`. In
doing so, an unsuccessful reading operation does not raise the
exception. The returned result is a line with initial values.
Alternatively, you can use the addition `DEFAULT` to return a
default line in case of an unsuccessful reading operation which might as
well be another table expression or constructor expression.

``` abap
DATA(line1) = VALUE #( itab[ i ] OPTIONAL ).

DATA(line2) = VALUE #( itab[ i ] DEFAULT itab[ i2 ]  ).
```

<p align="right">(<a href="#top">back to top</a>)</p>

*Reading single lines via table keys*

Lines can be read by explicitly specifying the table keys or the alias names if available.
```abap
"Example internal table with primary and secondary key and alias names
"Assumption: all components are of type i

DATA it TYPE SORTED TABLE OF struc
  WITH NON-UNIQUE KEY primary_key ALIAS pk COMPONENTS a b
  WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS c d.

"Table expressions

"key must be fully specified
line = it[ KEY primary_key COMPONENTS a = 1 b = 2 ].

"addition COMPONENTS is optional; same as above
line = it[ KEY primary_key a = 1 b = 2 ].

"primary key alias
line = it[ KEY pk a = 1 b = 2 ].

"secondary key
line = it[ KEY sec_key c = 3 d = 4 ].

"secondary key alias
line = it[ KEY sk c = 3 d = 4 ].

"READ TABLE statements
"primary key
READ TABLE it INTO wa WITH TABLE KEY primary_key COMPONENTS a = 1 b = 2.

"alias
READ TABLE it INTO wa WITH TABLE KEY pk COMPONENTS a = 1 b = 2.

"secondary key
READ TABLE it INTO wa WITH TABLE KEY sec_key COMPONENTS c = 3 d = 4.

"alias
READ TABLE it INTO wa WITH TABLE KEY sk COMPONENTS c = 3 d = 4.

"Reading a line based on keys specified in a work area
"Work area containing primary and secondary key values; the line type
"must be compatible to the internal table
DATA(pr_keys) = VALUE struc( a = 1 b = 2 ).

DATA(sec_keys) = VALUE struc( c = 3 d = 4 ).

READ TABLE it FROM pr_keys INTO wa.

"If USING KEY is not specified, the primary table key is used.
"If it is used, the specified table key is used.
READ TABLE it FROM pr_keys USING KEY primary_key INTO wa.

READ TABLE it FROM sec_keys USING KEY sec_key INTO wa.

"alias
READ TABLE it FROM sec_keys USING KEY sk INTO wa.
```

<p align="right">(<a href="#top">back to top</a>)</p>

**Reading a single line via free key**

The specified components used as keys need not belong to a table key.
``` abap
line = it[ b = 2 ].

READ TABLE it INTO wa WITH KEY b = 2.
```

*Addressing individual components*

When reading single lines in general, you can also address individual
components of the line using the [component
selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomponent_selector_glosry.htm "Glossary Entry")
`-` (or the [dereferencing
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm "Glossary Entry")
`->*` in case of data reference variables).
``` abap
DATA(comp1) = it[ b = 2 ]-c.

READ TABLE it INTO DATA(wa) WITH KEY b = 2.
DATA(comp2) = wa-c.

READ TABLE it ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY b = 2.
DATA(comp3) = <fs>-c.

READ TABLE it REFERENCE INTO DATA(dref) WITH KEY b = 2.
DATA(comp4) = dref->*-c.
"Note: dref->c, which is more comfortable, also works
```

<p align="right">(<a href="#top">back to top</a>)</p>

**Checking the existence and the index of a line in an internal table**

This is relevant if you are not interested in the content of a table
line but just want to find out if a line exists that matches to the
index or key specifications. You can do this using a [`READ TABLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapread_table.htm)
statement with the addition `TRANSPORTING NO FIELDS`. The
addition denotes that no actual content is to be read. If the search was
successful and an entry exists, the system field `sy-subrc` is
set to 0.

A newer way to check the existence of a line is the [predicate
function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpredicate_function_glosry.htm "Glossary Entry")
[`line_exists( )`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenline_exists_function.htm).
As an argument, this statement expects a [table
expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_expression_glosry.htm "Glossary Entry").
See more on table expressions below. Note that system fields are not set
with table expressions.
``` abap
"via key

READ TABLE it WITH KEY b = 2 TRANSPORTING NO FIELDS.

IF sy-subrc = 0.
  ...
ENDIF.

"via index

READ TABLE it INDEX 1 TRANSPORTING NO FIELDS.

IF sy-subrc = 0.
  ...
ENDIF.

"via key

IF line_exists( it[ b = 2 ] ).
  ...
ENDIF.

"via index

IF line_exists( it[ 1 ] ).
  ...
ENDIF.
```
If you want to find out about the index of a line in an internal table, you can also make use of the `READ TABLE` statement above. If
the line is found, the system field `sy-tabix` is set with the number of the index. Apart from that, the built-in function
[`line_index( )`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenline_index_function.htm) can be used. It returns the index of the searched line or 0 if the line does not exist.
``` abap
DATA(idx) = line_index( it[ b = 2 ] ).
```

`lines( )` is another built-in function with which you can check how many lines exist in an internal table. The function returns an integer value.

``` abap
DATA(number_of_lines) = lines( it ).
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Processing Multiple Internal Table Lines Sequentially

If you are not only interested in single table lines but in the entire
table content or particular parts, you can use [`LOOP
AT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab.htm)
statements to process table lines sequentially. Similar to above, you
can make use of the multiple target area options: work area, field
symbol, data reference. The following snippets only include the work
area. There are multiple additions available for `LOOP AT`
statements to further restrict the table content to be processed.

Simple form:
``` abap
LOOP AT it INTO wa. "Inline declarations possible: INTO DATA(wa)
  "No addition of the loop statement; all lines are processed
  "Statements in this block are relevant for each individual table line.
  ...
ENDLOOP.
```

The order in which tables are looped across depends on the table category. Index tables are looped across via the index in ascending
order. In case of hashed tables, the looping happens in the order in which the lines were added to the table. However, you can also sort the table before the loop. In the course of the loop, the system field `sy-tabix` is set to the number of the currently processed table
line. This is not true for hashed tables. There, `sy-tabix` is `0`. Note that if you want to work with the `sy-tabix` value, you
should do it right after the `LOOP` statement to not risk a potential overwriting in statements contained within the loop block.

*Restricting the table area to be looped across*

The additions of `LOOP` statements enter the picture if you want to restrict the table content to be respected for the loop because
you do not want to loop across the whole table.

``` abap
"FROM/TO: Only for index tables

"Specifying an index range
LOOP AT it INTO wa FROM 2 TO 5.
  ...
ENDLOOP.

"From specified line until the end
LOOP AT it INTO wa FROM 2.
  ...
ENDLOOP.

"From first line until the specified line
LOOP AT it INTO wa TO 5.
  ...
ENDLOOP.

"WHERE clause: Restricting lines based on logical expression

LOOP AT it INTO wa WHERE a > 1 AND b < 4.
  ...
ENDLOOP.

"No interest in table content; only relevant system fields are filled

"Mandatory WHERE clause
LOOP AT it TRANSPORTING NO FIELDS WHERE a < 5.
  ...
ENDLOOP.

"Table key specification (snippet uses example table from above)
"The specified table key affects the order in which the table lines
"are accessed and the evaluation of the other conditions.

LOOP AT it INTO wa USING KEY primary_key.
"LOOP AT it INTO wa USING KEY pk.            "primary key alias
"LOOP AT it INTO wa USING KEY sec_key.       "secondary key
"LOOP AT it INTO wa USING KEY sk.            "secondary key alias
  ...
ENDLOOP.
```

*Iterations with* `FOR`

Iteration expressions with [`FOR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfor.htm) as part of particular constructor expressions allow you to create content of an internal table by evaluating one or more source tables.

The examples below show iterations with `FOR` within a constructor expression with `VALUE`. A new table is created and
values for two fields are inserted in the new table that has the source table's internal table type. `ls` represents an iteration
variable that holds the data while looping across the table. The components, and thus the table line, that should be returned are
specified within the pair of parentheses before the closing parenthesis. Both examples set specific values for components. The second example also includes a `WHERE` clause to restrict the lines to be copied.

In contrast to `LOOP` statements, this sequential processing cannot be debugged.
``` abap
"Internal table type
TYPES ttype like it.

DATA(tab1) = VALUE ttype( FOR ls IN it ( a = ls-a b = 9 ) ).

DATA(tab2) = VALUE ttype( FOR ls IN it WHERE ( a < 7 )
                             ( a = ls-a b = ls-b + 5  ) ).
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Sorting Internal Tables

-   Sorted tables are stored in the memory in an automatically sorted
    order, hence, they cannot be sorted explicitly using
    [`SORT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapsort_itab.htm).
-   In case of standard and hashed tables, the order can be changed.
-   When using `SORT` statements, the sort order is either
    derived by the primary table key ([Note:] Secondary keys
    cannot be used for the sorting.) or by explicitly specifying the
    fields to be sorted by.
-   An explicit specification is the recommended way because it is
    easier to understand and can prevent undesired sorting results
    especially with tables with standard key.

*Sorting by primary key*
``` abap
"Implicit sorting by primary key and in ascending order by default

SORT itab.

"Optional additions to determine the sort order
"Explicit specification of ascending sort order

SORT itab ASCENDING.
SORT itab DESCENDING.
```

The effect of the sorting might have an unexpected result if you use the simple form of the statement and without an explicit specification of keys. If an internal table has a structured line type and (maybe inadvertently) the standard key as the primary table key, i. e. all character-like and byte-like components compose the primary table key, all of these components are respected when sorting the table.
``` abap
"Is basically the same as it2
DATA it1 TYPE TABLE OF zdemo_abap_fli.

DATA it2 TYPE STANDARD TABLE OF zdemo_abap_fli WITH DEFAULT KEY.

"Respect the standard key when sorting.
SORT it1.
```
Plus: Assume there are only elementary numeric components in an internal table with a structured line type. Then, the sorting has no effect because the primary table key is considered empty. This is certainly also true for tables declared with `EMPTY KEY`.

*Sorting by explicitly specifying components*

The sorting can be carried out using arbitrary components of the internal table. The specification of the sort order is also possible
(even component-wise). The explicit specification of components to sort by has the advantage that your code is easier to understand and you can prevent unexpected results when inadvertently using `SORT` without the `BY` addition in case of empty and standard table
keys.

``` abap
DATA it3 TYPE TABLE OF struc WITH NON-UNIQUE KEY a.

"Sorts by primary table key a
SORT itab.

"Specifying the component to sort for; here, it is the same as the key;
"this way, the sorting is easier to understand

SORT itab BY a.

"Syntax showing multiple component sorting with component-wise sort order

SORT itab BY a b ASCENDING c DESCENDING.

"Sorting respecting the entire line (e. g. in the context of tables with
"empty or standard keys)

SORT itab BY table_line.
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Modifying Internal Table Content

As touched on above, you can directly modify the content of internal table lines in the context of `READ TABLE` and `LOOP AT`
statements using field symbols and data reference variables. Direct modification is also possible using table expressions. Note that the key fields of the primary table key of sorted and hashed tables are always read-only. If you try to modify a key field, a runtime error occurs. However, this is not checked until runtime.

The following examples demonstrate the direct modification of recently read table lines:
``` abap
"Table declarations

DATA it_st TYPE TABLE OF struc WITH NON-UNIQUE KEY a.

DATA it_so TYPE SORTED TABLE OF struc WITH UNIQUE KEY a.

"Reading table line into target area

READ TABLE it_st ASSIGNING FIELD-SYMBOL(<fs>) INDEX 1.

READ TABLE it_so REFERENCE INTO DATA(dref) INDEX 2.

"Modification examples
"Modifying the entire table line while keeping the values of other components;
"this way is not possible for it_so because of key value change.

<fs> = VALUE #( BASE <fs> a = 1 b = 2 ).

"Modifying a single component via field symbol

<fs>-c = 3.

"Modification via dereferencing

ref->b = 4.

"Table expressions

it_st[ 1 ] = VALUE #( a = 1 b = 2 ).

it_st[ 2 ]-c = 3.

"Sorted table: no key field change

it_so[ 2 ]-d = 4.
```

> **💡 Note**<br>
> If you choose to modify recently read lines in a work area, for example, within a loop (`LOOP AT INTO dobj`), you
might modify the line and then modify the internal table based on this line using a `MODIFY` statement.

[`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_itab.htm)
statements offer multiple options for changing the content of single and multiple table lines by specifying the table key or a table index
without reading lines into a target area first.

``` abap
"Addition FROM ...; specified key values determine the line to be modified

"line: existing line including key values

MODIFY TABLE it FROM line.

"line constructed inline

MODIFY TABLE it FROM VALUE #( a = 1 b = 2 ... ).

"Respecting only specified fields with the addition TRANSPORTING
"In case of sorted/hashed tables, key values cannot be specified.

MODIFY TABLE it FROM line TRANSPORTING b c.

"Modification via index
"Note that it is only MODIFY, not MODIFY TABLE.
"Example: It modifies the line with number 1 in the primary table index.

MODIFY it FROM line INDEX 1.

"Without the addition TRANSPORTING, the entire line is changed.
"Example: It modifies specific values.

MODIFY it FROM line INDEX 1 TRANSPORTING b c.

"USING KEY addition
"If the addition is not specified, the primary table key is used;
"otherwise, it is the explicitly specified table key that is used.
"Example: It is the same as MODIFY it FROM line INDEX 1.

MODIFY it FROM line USING KEY primary_key INDEX 1.

"The statement below uses a secondary key and an index specification
"for the secondary table index. Only specific fields are modified.

MODIFY it FROM line USING KEY sec_key INDEX 1 TRANSPORTING c d.

"Modifying multiple lines in internal tables
"All lines matching the logical expression in the WHERE clause are modified
"as specified in line.
"The additions TRANSPORTING and WHERE are both mandatory; USING KEY is optional.

MODIFY it FROM line TRANSPORTING b c WHERE a < 5.
```
> **💡 Note**<br>
> The system field `sy-subrc` is set to `0` if at least one line was changed. It is set to `4` if no lines were changed.

<p align="right">(<a href="#top">back to top</a>)</p>

### Deleting Internal Table Content

Using [`DELETE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdelete_itab.htm) statements, you can delete single and multiple lines in internal tables.
``` abap
"Deleting via index
"Example: The first line in the table is deleted.

DELETE it INDEX 1.

"If USING KEY is not used, INDEX can only be used with index tables.
"If doing so, it determines the line from the primary table index.
"If a secondary key is specified, the secondary table index is respected
"Example: same as above

DELETE it INDEX 1 USING KEY primary_key.

"Deleting an index range; FROM or TO alone can also be specified

DELETE it FROM 2 TO 5.

"Deleting via keys
"The line must have a compatible type to the tables line type and
"include key values. The first found line with the corresponding keys
"is deleted.
"If the key is empty, no line is deleted.

DELETE TABLE it FROM line.

"Instead of specifying the keys using a data object ("line" above),
"the keys can be specified separately. All key values must be specified.
"Example: Respects keys from primary table index.

DELETE TABLE it WITH TABLE KEY a = 1.

"You can also specify secondary keys.
"Example: Same as above

DELETE TABLE it WITH TABLE KEY primary_key COMPONENTS a = 1.

DELETE TABLE it_sec WITH TABLE KEY sec_key COMPONENTS ...

"Deleting multiple lines based on a WHERE condition
"Specifying the additions USING KEY, FROM, TO is also possible.

DELETE it WHERE a < 6.
```

`DELETE ADJACENT DUPLICATES` statements allow you to delete all neighboring lines except for the first line that have the same content
in specific components. Usually, a suitable sorting before carrying out these statements is required.
``` abap
"Implicitly uses the primary table key

DELETE ADJACENT DUPLICATES FROM it.

"Deletion respecting the values of the entire line

DELETE ADJACENT DUPLICATES FROM it COMPARING ALL FIELDS.

"Only lines are delete with matching content in specific fields

DELETE ADJACENT DUPLICATES FROM it COMPARING a c.

"Deletion respecting a specified table key

"Same as first example above
DELETE ADJACENT DUPLICATES FROM it USING KEY primary_key.

DELETE ADJACENT DUPLICATES FROM it USING KEY sec_key.
```

> **💡 Note**<br>
> The system field `sy-subrc` is set to `0` if at least one line was deleted. It is set to `4` if no lines were deleted.

Using
[`CLEAR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclear.htm)
and
[`FREE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfree_dataobject.htm)
statements, you can delete the complete table content.

The difference between the two is the handling the initially requested
memory space for the table. When clearing a table using `CLEAR`,
the content is removed but the initially requested memory space remains
allocated. If the table is later filled again, the memory space is still
available, which is beneficial in terms of performance in contrast to
clearing an internal table using `FREE`. Such a statement also
clears the table content but, additionally, it releases the memory
space.

``` abap
CLEAR it.

"Additionally, it releases memory space.
FREE it.
```
<p align="right">(<a href="#top">back to top</a>)</p>

## More Information
Topic [Internal
Tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenitab.htm) in the ABAP Keyword Documentation.

## Executable Example
[zcl_demo_abap_internal_tables](./src/zcl_demo_abap_internal_tables.clas.abap)

Note the steps outlined [here](README.md#-getting-started-with-the-examples) about how to import and run the code.
