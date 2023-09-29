<a name="top"></a>

# Internal Tables

- [Internal Tables](#internal-tables)
  - [Introduction](#introduction)
  - [Basic Properties of Internal Tables](#basic-properties-of-internal-tables)
  - [Table Keys in Internal Tables (Primary, Secondary, Standard, Empty)](#table-keys-in-internal-tables-primary-secondary-standard-empty)
  - [Creating Internal Tables and Types](#creating-internal-tables-and-types)
  - [Filling and Copying Internal Tables](#filling-and-copying-internal-tables)
    - [Excursions with Internal Tables](#excursions-with-internal-tables)
  - [Reading from Internal Tables](#reading-from-internal-tables)
  - [Processing Multiple Internal Table Lines Sequentially](#processing-multiple-internal-table-lines-sequentially)
  - [Sorting Internal Tables](#sorting-internal-tables)
  - [Modifying Internal Table Content](#modifying-internal-table-content)
  - [Deleting Internal Table Content](#deleting-internal-table-content)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)


## Introduction

Internal Tables ...

- are [dynamic data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendynamic_data_object_glosry.htm), i.e. all properties apart from the memory consumption are determined statically by the [data type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_type_glosry.htm).
- consist of a variable sequence of lines of the same data type. 
- have a [table type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_type_glosry.htm) as its data type (it is a [complex data type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplex_data_type_glosry.htm)), which defines the following properties: 
  - [line type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrow_type_glosry.htm)
  - [table category](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_category_glosry.htm)
  - [table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_key_glosry.htm)
- are used when a variable data set of a random data type needs to be processed in a structured way.
- allow access to individual table lines via a [table index](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_index_glosry.htm) or a [table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_key_glosry.htm).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Basic Properties of Internal Tables

<details>
  <summary>Expand to view the details</summary>
  <!-- -->

**Line Type**

- Defines how each line of the internal table is set up, i. e. it describes what columns the table has.
- It can be any ABAP data type, e.g. an [elementary](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenelementary_data_type_glosry.htm) or complex data type as well as a [reference type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_type_glosry.htm).
- In most cases, the line type is [structured](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstructured_type_glosry.htm). In this case, the individual components of a line are also referred to as the columns of the internal table.
- In a simple case, the line consists of a [flat structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenflat_structure_glosry.htm) with elementary data objects; however, it can also be a [deep structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_structure_glosry.htm) whose components can be structures themselves or even internal tables.

**Table Category**

- Defines how internal tables are managed and stored internally, and how to access individual table entries.
- Why is it relevant? The use of a suitable table category should meet your requirements, i.e. if the internal tables are large, the different categories can have significant performance differences when accessing table content.
- Note: There are two ways to access internal tables:
   - Access by table index: A line of an internal table is accessed by its line number. This kind of access is the fastest way to access table lines.
   - Access by table key: A line of an internal table is accessed by searching for specific values in specific columns. Note: The columns in which you search can be key columns, but they can also be non-key columns.


| Category | Internally managed by | Access | Primary table key | When to use | Hints |
|---|---|---|---|---|---|
|`STANDARD`|Primary table index (that's why these tables are called [index tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenindex_table_glosry.htm))|<ul><li>Table index</li><li>Table key</li></ul>|<ul><li>Always non-unique, i.e. duplicate entries are always allowed</li><li>Definition of an empty key is possible if the key is not relevant(`WITH EMPTY KEY`)</li></ul>|<ul><li>If you primarily access the table content for sequential processing or via the table index.</li><li>Response time for accessing the table using the primary key: This kind of table access is optimized only for sorted and hashed tables. For standard tables, primary key access uses a linear search across all lines. That means that large standard tables (more than 100 lines) are not ideal if the you primarily access the table using the table key.</></ul>|<ul><li>There is no particular sort order, but the tables can be sorted using `SORT`.</li><li>Filling this kind of table: Lines are either appended at the end of the table or inserted at a specific position.</li><li>[Secondary table keys](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensecondary_table_key_glosry.htm) can be defined to make key access to standard tables more efficient.</li><li>Standard and sorted tables have the least [administration costs (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenadmin_costs_dyn_mem_obj_guidl.htm).</li></ul>|
|`SORTED`|Primary table index (that's why these tables are called [index tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenindex_table_glosry.htm))|<ul><li>Table index</li><li>Table key</li></ul>|<ul><li>Non-unique</li><li>Unique</li><br>... used to sort the table in ascending order.</ul>|<ul><li>Enables an optimized access to table content using table key and index.</li><li>If access via table key is the main access method, but no unique key can be defined.</li></ul>|<ul><li>Sorting is done automatically when lines are inserted or deleted. As a consequence, the table index must usually be reorganized. </li><li>The response time for accessing the table using the primary key depends logarithmically on the number of table entries, since a binary search is used.</li><li>Standard and sorted tables have the least administration costs.</li></ul>|
|`HASHED`|Hash algorithm |<ul><li>Table key</li><li>[Secondary table index](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensecondary_table_index_glosry.htm)</li></ul>|Always unique|<ul><li>For large internal tables.</li><li>Optimized for key access. Access to table content via table key is the main access method and a unique key can be defined.</li></ul>|<ul><li>The response time for primary key access is constant and independent of the number of entries in the table.</li><li>Hashed tables have the highest administration costs.</li></ul>|



**Key Attributes** 

- There are two types of table keys: a [primary table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprimary_table_key_glosry.htm) and [secondary table keys](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensecondary_table_key_glosry.htm).
- Table keys ...
  - are intended to provide an optimized access to the content of internal tables.
  - are either unique or non-unique, i.e. more than one line with the same key (duplicates) can exist in the internal table. Regarding the primary table key, the definition depends on the table category. For the secondary table key, the definition depends on the key type. For standard tables, the primary table key can also be defined as empty, i.e. it does not contain any key columns. Note that for standard tables, an optimized access is only possible with secondary table keys.
 - Type of keys:  
   - Sorted keys: 
     - Are either the primary table keys of sorted tables or the secondary table keys of any table.
     - Are managed internally by a table index. In the case of sorted tables, this is the primary table index. In the case of secondary table keys, a secondary table index is added.
     - Access via sorted keys means an optimized binary search.
   - Hashed keys:
     - Are either the primary table key of hashed tables or secondary table keys of any table.
     - Internally managed by a hash algorithm. 
     - There is no table index for a hashed key. 

**Further information**
- [Internal Tables - Overview](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenitab_oview.htm)
- [Programming guidelines: Internal Tables (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenadmin_costs_dyn_mem_obj_guidl.htm)
</details>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Table Keys in Internal Tables (Primary, Secondary, Standard, Empty)

<details>
  <summary>Expand to view the details</summary>
  <!-- -->

**Primary table key**

- Each internal table has a primary table key.
- Can be either a self-defined key or the [standard key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_key_glosry.htm).
- The primary table key is ...
  - sorted for sorted tables. 
  - hashed for hashed tables.
- Note that the key fields in sorted and hashed tables are read-only. This is not valid for standard tables.
- The specification of the primary key can be omitted only for standard tables. The primary table key is then automatically defined as a non-unique standard key.
- The primary table key has the predefined name `primary_key`, by which it can also be addressed explicitly. However, its use is optional, and it is usually not necessary to specify it explicitly. You can also specify an alias name for the primary key. 
- When accessing internal tables using the table key, the primary key is always used implicitly in processing statements if no secondary key is specified. Note that the primary table key must be specified  in table expressions if the primary key is to be used explicitly.

> **💡 Note**<br>
> The key can consist of individual key fields or the entire line of the internal table. In this case, the pseudo component `table_line` can be used to denote the primary table key. For non-structured line types, this is the only way to define the key.

**Standard key**
- The [standard key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_key_glosry.htm) is a special primary table key.
- It can be declared either explicitly or implicitly.
- Standard key of an internal table with a ...
   - structured line type: The primary table key consists of all fields with character-like and byte-like data types.
   - non-structured/elementary line type: The entire table is the key (`table_line`).
- An internal table with no explicit key specification implicitly has the standard table key as the primary table key.
- Why respecting standard keys is important:
  - Sorting of a table can produce unexpected results.
  - Since the standard key can consist of many fields, it affects the performance when accessing the internal table via the keys.
  - The key fields of the primary table key of sorted and hashed tables are always read-only, i.e. using the standard key with these table categories and then (unintentionally) modifying fields can cause unexpected runtime errors.
  - Specifying keys explicitly has the advantage of making the code more readable and preventing the standard key from being set by mistake.

**Empty key**
- The primary table key of a standard table can be empty, i.e. it does not contain any key fields.
- An empty key is not possible for sorted and hashed tables.
- When used: 
  - When the definition of a table key is not important.
  - To explicitly state that a table key is not required, instead of specifying no key definition. Otherwise, the standard key is used, which can lead to unexpected results as mentioned above.
- Declaration:
    -   Explicit declaration using the addition `EMPTY KEY`.
    -   Implicit declaration when using the standard key if a structured
        line type does not contain non-numeric elementary components or
        if an unstructured line type is tabular.
    - Note: When using an [inline declaration](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry") such as `... INTO TABLE @DATA(itab) ...` in `SELECT` statements, the resulting table is a standard table and has an empty key.

**Secondary table keys**

- Secondary table keys ...
  - are optional for all table categories.
  - can be unique/non-unique sorted keys or unique hash keys.
  - have a self-defined name. An alias name can also be specified.
- A secondary table index is created internally for each sorted secondary key. This allows index access to hashed tables via the secondary table key. In this case, `sy-tabix` is set.
- When accessing internal tables using the secondary table key, the key name (or the alias if specified) must be specified. They are not selected automatically. If no secondary key is specified in a processing statement, the primary key or primary table index is always used. If you want to make use of this key in ABAP statements, for example, `READ`, `LOOP AT` or `MODIFY` statements, you must specify the key explicitly using the appropriate additions, for example, `WITH ... KEY ... COMPONENTS` or `USING KEY`.
- Use cases:
  - To improve read performance.
  - For very large internal tables (that are filled once and changed very often) 
  - Standard tables, whose primary table keys cannot be unique, can be provided with a means of ensuring that unique table entries are read. 
 - Note that defining secondary table keys involves additional administration costs (additional memory consumption). Therefore, the use of secondary table keys should be reserved for cases where the benefits outweigh the extra costs. 
- For more details, see the programming guidelines for secondary keys: [Secondary
    Key (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensecondary_key_guidl.htm "Guideline").

> **💡 Note**<br>
> See examples of internal table declarations using the table keys mentioned above in the following section.
 
</details>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Creating Internal Tables and Types

You can declare internal tables and internal table types in [ABAP programs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm) using the [`TYPES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes.htm) and [`DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata.htm) statements. The relevant syntax elements for internal tables are `TABLE OF` in combination 
with the additions [`TYPE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_simple.htm)
or [`LIKE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_referring.htm).

``` abap
TYPES itab_type1 TYPE STANDARD TABLE OF data_type ...   "Standard table type
TYPES itab_type2 LIKE SORTED   TABLE OF data_object ... "Sorted table type

DATA  itab1      TYPE          TABLE OF data_type ...   "Standard table by default
DATA  itab2      TYPE HASHED   TABLE OF data_type ...   "Hashed table
DATA  itab3      TYPE                   itab_type1 ...  "Based on an existing internal table type 
DATA  itab4      LIKE                   itab1 ...       "Based on an existing internal table       
```

> **💡 Note**<br>
> If the table category is not specified (`... TYPE TABLE OF ...`), it is automatically `... TYPE STANDARD TABLE OF ...`.

The following code snippet contains various internal table declarations. It is intended to demonstrate a selection of the rich variety of possible internal tables mentioned in the previous sections, e.g. in *Table Keys in Internal Tables*.
In the examples, the internal tables are created using the structured type of a demo database table in the DDIC. The line type of the database table is automatically used when defining an internal table.


``` abap
"------------------ Standard table key ------------------

"Standard table without explicit primary table key specification. Note that STANDARD 
"is not explicitly specified.
"Implicitly, the standard key is used; all non-numeric table fields
"make up the primary table key.

DATA it1 TYPE TABLE OF zdemo_abap_fli.

"Explicitly specifying STANDARD for a standard table.
"Explicitly specifying the standard table key. It is the same as it1.

DATA it2 TYPE STANDARD TABLE OF zdemo_abap_fli WITH DEFAULT KEY.

"Hashed table with unique standard table key

DATA it3 TYPE HASHED TABLE OF zdemo_abap_fli WITH UNIQUE DEFAULT KEY.

"Sorted table with non-unique standard table key

DATA it4 TYPE SORTED TABLE OF zdemo_abap_fli WITH NON-UNIQUE DEFAULT KEY.

"Elementary line type; the whole table line is the standard table key

DATA it5 TYPE TABLE OF i.

"------------------ Primary table key ------------------

"Specifying the primary table key
"Standard tables: only a non-unique key possible
"The following two examples are the same. NON-UNIQUE can be ommitted but is implicitly added.

DATA it6 TYPE TABLE OF zdemo_abap_fli WITH NON-UNIQUE KEY carrid.
DATA it7 TYPE TABLE OF zdemo_abap_fli WITH KEY carrid.

"Sorted tables: both UNIQUE and NON-UNIQUE possible

DATA it8 TYPE SORTED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid connid.

DATA it9 TYPE SORTED TABLE OF zdemo_abap_fli WITH NON-UNIQUE KEY carrid connid cityfrom.

"Hashed tables: UNIQUE KEY must be specified

DATA it10 TYPE HASHED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid.

"Explicitly specifying the predefined name primary_key and listing the components.
"The example is the same as it6 and it7.

DATA it11 TYPE TABLE OF zdemo_abap_fli WITH KEY primary_key COMPONENTS carrid.

"The following example is the same as it9.

DATA it12 TYPE SORTED TABLE OF zdemo_abap_fli 
  WITH NON-UNIQUE KEY primary_key COMPONENTS carrid connid cityfrom.

"Specifying an alias name for a primary table key.
"Only possible for sorted/hashed tables.

DATA it13 TYPE SORTED TABLE OF zdemo_abap_fli
  WITH NON-UNIQUE KEY primary_key
  ALIAS p1 COMPONENTS carrid connid cityfrom.

"Specifying a key that is composed of the entire line using the predefined table_line.
"In the example, an alias name is defined for a primary table key. 

DATA it14 TYPE HASHED TABLE OF zdemo_abap_fli
  WITH UNIQUE KEY primary_key
  ALIAS p2 COMPONENTS table_line.

"------------------ Empty key ------------------

"Empty keys only possible for standard tables

DATA it15 TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

"Excursion: The inline declaration in a SELECT statement produces a standard table with empty key.

SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(it16).

"------------------ Secondary table key ------------------

"The following examples demonstrate secondary table keys that are possible for all table categories.

DATA it17 TYPE TABLE OF zdemo_abap_fli                      "standard table
  WITH NON-UNIQUE KEY carrid connid                         "primary table key
  WITH UNIQUE SORTED KEY cities COMPONENTS cityfrom cityto. "secondary table key

DATA it18 TYPE HASHED TABLE OF zdemo_abap_fli               "hashed table
  WITH UNIQUE KEY carrid connid
  WITH NON-UNIQUE SORTED KEY airports COMPONENTS airpfrom airpto.

DATA it19 TYPE SORTED TABLE OF zdemo_abap_fli              "sorted table
  WITH UNIQUE KEY carrid connid
  WITH UNIQUE HASHED KEY countries COMPONENTS countryfr countryto.

"Multiple secondary keys are possible 

DATA it20 TYPE TABLE OF zdemo_abap_fli
  WITH NON-UNIQUE KEY primary_key COMPONENTS carrid connid
  WITH NON-UNIQUE SORTED KEY cities COMPONENTS cityfrom cityto
  WITH UNIQUE HASHED KEY airports COMPONENTS airpfrom airpto.

"Alias names for secondary table keys (and also for the primary table key in the example)

DATA it21 TYPE SORTED TABLE OF zdemo_abap_fli
  WITH NON-UNIQUE KEY primary_key ALIAS k1 COMPONENTS carrid connid city
  WITH NON-UNIQUE SORTED KEY cities ALIAS s1 COMPONENTS cityfrom cityto
  WITH UNIQUE HASHED KEY airports ALIAS s2 COMPONENTS airpfrom airpto.

"Example for using table keys and alias names using a LOOP AT statement. 
"All of the statements below are possible.
"Note that if the secondary table key is not specified (and if the USING KEY addition is not 
"used in the example), the primary table key is respected by default.
"Further ABAP statements, such as READ or MODIFY, are available in which the key can be 
"explicitly specified to process internal tables.

LOOP AT it21 INTO DATA(wa) USING KEY primary_key.
"LOOP AT it21 INTO DATA(wa) USING KEY k1.
"LOOP AT it21 INTO DATA(wa) USING KEY cities.
"LOOP AT it21 INTO DATA(wa) USING KEY s1.
"LOOP AT it21 INTO DATA(wa) USING KEY airports.
"LOOP AT it21 INTO DATA(wa) USING KEY s2.
  ...
ENDLOOP.
```

As mentioned, the examples above demonstrate internal tables that are created using the structured type of a database table in the DDIC. 
The following example shows the pattern and various examples of declaring internal tables and types by including the local definition of structured data and internal table types for demonstration purposes.

Steps: 
1. Define a structured data type (locally or globally). 
   This is not necessary if you use, for example, the name of a database table or [CDS view](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_view_glosry.htm) in the internal table declaration. In these cases their line type is used automatically.
2. Define an internal table type.
3. Create the internal table, i.e. a data object that uses this type.

You can also create an internal table by ...
- combining the data object creation and table type definition in one step. 
- using an [inline declaration](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninline_declaration_glosry.htm "Glossary Entry"). Such inline declarations are possible at suitable [declaration
positions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_positions.htm)
if the operand type can be fully determined, for example, using a
`DATA` statement (or [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm) for immutable variables).


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

TYPES:
  "Standard table type based on locally defined structure type.
  tt_loc_str TYPE TABLE OF ls_loc WITH NON-UNIQUE KEY key_field,

  "Based on global structure type
  tt_gl_str TYPE TABLE OF zsome_global_struc_type WITH NON-UNIQUE KEY key_field,

  "Based on database table (could also be, e. g. a CDS view)
  "In this case, the line type of the table is automatically used.
  tt_gl_tab TYPE TABLE OF zdemo_abap_fli WITH NON-UNIQUE KEY carrid,

  "Based on an elementary type
  tt_el_type TYPE TABLE OF i.

"3. Creating internal tables ...
"... from locally defined table types

DATA: itab_a1 TYPE tt_loc_str,
      itab_a2 TYPE tt_gl_str,
      itab_a3 TYPE tt_gl_tab,
      itab_a4 TYPE tt_el_type.

"... from global internal table types
DATA itab_a5 TYPE string_table.

"Combining data object and table type definition 

DATA itab_a6 TYPE TABLE OF ls_loc WITH NON-UNIQUE KEY key_field.

"Internal table based on an already existing internal table using LIKE.
DATA itab_a7 LIKE TABLE OF itab_a6.

"Creating internal tables by inline declarations

"Table declared inline in the context of an assignment
"The examples show the copying of a table including the content on the fly
"and creating the table in one step. The data type of the
"declared variable is determined by the right side.

DATA(it_inline1) = itab_a1.
DATA(it_inline2) = it_inline1.

"Using the VALUE operator and an internal table type

DATA(it_inline3) = VALUE tt_loc_str( ( ... ) ).

"Table declared inline in the context of a SELECT statement;
"a prior extra declaration of an internal table is not needed.

SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(it_inline4).

"Instead of
DATA it_sel TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

SELECT * FROM zdemo_abap_fli INTO TABLE @it_sel.

"Using FINAL
SELECT * FROM zdemo_abap_fli INTO TABLE @FINAL(it_inline5).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Filling and Copying Internal Tables

You can use the ABAP keywords
[`APPEND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapappend.htm)
and
[`INSERT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinsert_itab.htm)
to add lines to internal tables.

<details>
  <summary>Notes on using <code>APPEND</code> and <code>INSERT</code></summary>
  <!-- -->

-   `APPEND` ...
    -   always adds lines at the bottom of the internal table.
    -   is not a problem for standard tables where lines are managed
        by an index. When the statement is used, the system field
        `sy-tabix` is set to the index of the recently added
        line. `sy-tabix` is always set to the index with respect
        to the primary table index.
    -   cannot be used for hashed tables. For sorted tables,
        lines are appended only if they match the sort order, and no
        duplicate entries are created if the primary table key is unique.
        Therefore, `INSERT` should be used when adding lines to
        sorted tables.
-   `INSERT` ...
    -   can be used to add lines at a specific position in tables (by
        specifying the target index). In doing so, all the following
        lines are moved down one position.
    -   without specifying the position adds the lines at the bottom of
        the table in case of standard tables. However, when using
        `INSERT`, `sy-tabix` is not set unlike
        `APPEND`. In case of sorted tables, the line is
        automatically inserted at the correct position.
    -   Note: In the case of unique primary table keys in sorted
        and hashed tables, the table cannot have entries with duplicate
        keys. If a duplicate is inserted, the insertion fails and the
        system field `sy-subrc` is set to 4.
</details>
<br>

**Adding a line to an internal table**. The example shows both a structure that is created using the `VALUE` operator as well as an existing structure that is added.

``` abap
APPEND VALUE #( comp1 = a comp2 = b ... ) TO itab.
APPEND lv_struc TO itab.

INSERT VALUE #( comp1 = a comp2 = b ... ) INTO TABLE itab.
INSERT lv_struc INTO itab.
```

**Adding an initial line** to an internal table without providing any field values.

``` abap
APPEND INITIAL LINE TO itab.

INSERT INITIAL LINE INTO TABLE itab.
```

**Adding all lines** from another internal table.

``` abap
APPEND LINES OF itab2 TO itab.

INSERT LINES OF itab2 INTO TABLE itab.
```

**Adding multiple lines from another internal table with a specified index range**.
- Both `FROM` and `TO` are not mandatory in one statement. it is possible to use only one of them. 
- If you use only ...
  - `FROM`, all lines up to the last table entry are respected.
  - `TO`, all lines starting with the first table entry are respected. 

``` abap
"i1/i2 represent integer values

APPEND LINES OF itab2 FROM i1 TO i2 TO itab.

APPEND LINES OF itab2 FROM i1 TO itab.

APPEND LINES OF itab2 TO i2 TO itab.

INSERT LINES OF itab2 FROM i1 TO i2 INTO itab.
```

**Inserting one line or multiple lines from another internal table at a specific position**. 
`FROM` and `TO` can be used here, too.

``` abap
INSERT lv_struc INTO itab2 INDEX i.

INSERT LINES OF itab2 INTO itab INDEX i.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

**Adding lines using constructor expressions**

As mentioned above, table lines that are constructed inline as
arguments to the `VALUE` operator, for example, can be added to
internal tables. In the following cases, internal tables are populated
using constructor expressions in the context of
[assignments](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_glosry.htm "Glossary Entry").

In the example below, the internal table is populated by assigning an
internal table that is constructed inline with the `VALUE`
operator. The inline constructed table has two lines. `line`
represents an existing structure with a compatible line type. The
other line is constructed inline.


> **💡 Note**<br>
> The extra pair of parentheses represents a table line. The `#` character indicates that the line type can be derived from the context. The assignment deletes the existing content of the internal table on the left side.

``` abap
itab = VALUE #( ( line )
                ( comp1 = a comp2 = b ...  ) ).
```

**Creating an internal table by inline declaration** and adding lines with a constructor expression.
``` abap
"Internal table type
TYPES it_type LIKE itab.

"Inline declaration
"The # character would not be possible here since the line type
"cannot be derived from the context.

DATA(it_in) = VALUE it_type( ( comp1 = a comp2 = b ... )
                             ( comp1 = c comp2 = d ...  ) ).
```

When you use the above assignments (`itab = ...`), the internal table is initialized and the existing content are deleted. To add new lines **without deleting existing content**, use the  [`BASE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenvalue_constructor_params_itab.htm) addition.

``` abap
itab = VALUE #( BASE itab ( comp1 = a comp2 = b ... )
                          ( comp1 = c comp2 = d ... ) ).
```

**Adding lines of other tables** using the `LINES OF` addition to the `VALUE` operator.
> **💡 Note**<br>
> Without the `BASE` addition, the existing content are deleted. It is assumed that the line type of the source table is compatible with that of the target table.
``` abap
itab = VALUE #( ( comp1 = a comp2 = b ...)
                ( comp1 = a comp2 = b ...)
                ( LINES OF itab2 )
                ... ).
```
A simple assignment without a constructor expression that **copies the content of another internal table** (note that the existing content in `itab` are deleted). The example below assumes that the source and target table have compatible line types. 
``` abap
itab = itab2.
```
> **💡 Note**<br>
> - Internal tables can only be assigned to internal tables. 
> - Internal tables can be assigned to each other if their line types are [compatible](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencompatible_glosry.htm) or [convertible](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconvertible_glosry.htm).
> - An assignment can trigger an uncatchable exception if, for example, the target table is assigned a duplicate of a unique primary table key or secondary table.
> More information: [Conversion Rules for Internal Tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_itab.htm)

**Copying the content of another internal table** using the
[`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm) operator. 
- Note that the existing content are deleted.
- As an alternative to the `CORRESPONDING` operator, you can use [`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove-corresponding.htm) statements. 
- The example assumes that the line types of the source and target table are not compatible. However, if the line types are compatible, the syntax will also work.

``` abap
itab = CORRESPONDING #( itab3 ).

MOVE-CORRESPONDING itab3 TO itab.
```

**Copying content and retaining existing content** using the `CORRESPONDING` operator. The `KEEPING TARGET LINES` addition of the `MOVE-CORRESPONDING` statement preserves the table content.

``` abap
itab = CORRESPONDING #( BASE ( itab ) itab3 ).

MOVE-CORRESPONDING itab3 TO itab KEEPING TARGET LINES.
```
**Assigning components using mapping relationships**
- You can use the `MAPPING` addition of the `CORRESPONDING` operator to specify components of a source table that are assigned to the components of a target table in mapping relationships. 
- For elementary components, the assignment is made according to the associated assignment rules.

``` abap
itab = CORRESPONDING #( itab3 MAPPING a = c b = d ).
```

**Excluding components from the assignment** using the `EXCEPT` addition to the `CORRESPONDING` operator.
- This is particularly useful if there are identically named components in the source and target tables that are not compatible or convertible. You can avoid syntax errors or runtime errors. 
- Instead of a component list, `EXCEPT` can also be followed by `*` to exclude all components that are not mentioned in a previous mapping of components. 
- If `EXCEPT *` is used without the `MAPPING` addition, all components remain initial.
``` abap
itab = CORRESPONDING #( itab3 EXCEPT e ).

itab = CORRESPONDING #( itab3 EXCEPT * ).
```

**Preventing runtime errors when duplicate lines are assigned** to the target table that is defined to accept only unique keys using the `DISCARDING DUPLICATES` addition of the `CORRESPONDING` operator. 
- In this case, the duplicate line is ignored in the source table. 
- The addition can also be specified with `MAPPING ...`.

``` abap
itab = CORRESPONDING #( itab2 DISCARDING DUPLICATES ).
```

**Copying data from deep internal tables**. 
- The `BASE` addition does not delete the existing content. 
- See also the alternative `MOVE-CORRESPONDING` statements.
``` abap
itab_nested2 = CORRESPONDING #( DEEP itab_nested1 ).

itab_nested2 = CORRESPONDING #( DEEP BASE ( itab_nested2 ) itab_nested1 )

MOVE-CORRESPONDING itab_nested1 TO itab_nested2 EXPANDING NESTED TABLES.

MOVE-CORRESPONDING itab_nested1 TO itab_nested2 EXPANDING NESTED TABLES KEEPING TARGET LINES.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Excursions with Internal Tables
For more details on ABAP SQL, see the cheat sheet on [ABAP SQL](03_ABAP_SQL.md).

**Adding multiple lines from a database table to an internal table** using
[`SELECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect.htm),
for example, based on a condition. In the case below, the internal table
is created inline. 
``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2 ...
  WHERE ...
  INTO TABLE @DATA(itab_sel).
```

**Sequentially adding multiple rows** from a database table to an internal table using `SELECT ... ENDSELECT.`, for example, based on a condition. In this case, the selected data is first stored in a structure that can be further processed and added to an internal table.

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

Adding multiple lines from a database table using `SELECT`, for example, based on a condition when the database table has a line type that is incompatible with the internal table. The `*` character means that all fields are selected. The other examples define specific fields.
The `APPENDING CORRESPONDING FIELDS INTO TABLE` addition appends the selected data to the end of the table without deleting existing
table entries. The `INTO CORRESPONDING FIELDS OF TABLE` addition adds lines and deletes existing table entries.
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
Adding multiple lines from an internal table to another internal table using `SELECT`. Note the alias name that must be defined for the
internal table.
``` abap
SELECT comp1, comp2, ...
  FROM @itab2 AS it_alias
  INTO TABLE @DATA(itab_sel).
```

**Combining data from multiple tables into one internal table** using an [inner
join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninner_join_glosry.htm "Glossary Entry").
The following example joins data of an internal and a database table 
using a `SELECT` statement and the [`INNER JOIN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_join.htm) addition. Note that the field list includes fields from both tables. The fields are referred to using `~`.
``` abap
SELECT it_alias~comp1, it_alias~comp2, dbtab~comp3 ...
  FROM @itab AS it_alias
  INNER JOIN dbtab ON it_alias~comp1 = dbtab~comp1
  INTO TABLE @DATA(it_join_result).
```

Filling an internal table from a database table using
[subqueries](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubquery_glosry.htm "Glossary Entry").
The following two examples fill an internal table from a database table. In the first example, a subquery is specified in the
`WHERE` clause with the `NOT IN` addition. It checks whether a value matches a value in a set of values
specified in parentheses. The second example fills an internal table depending on data in another table. A subquery with the `EXISTS` addition is specified in
the `WHERE` clause. In this
case, the result of the subquery, which is another
`SELECT` statement, is checked to see if an entry exists in
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
another table using the [`FOR ALL ENTRIES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_all_entries.htm) addition.

> **💡 Note**<br>
> Make sure that the internal table you are reading from is not initial. Therefore, it is recommended that you use a subquery as shown above: `... ( SELECT ... FROM @itab AS itab_alias WHERE ...`).

``` abap
IF itab IS NOT INITIAL.

  SELECT dbtab~comp1, dbtab~comp2, ...
    FROM dbtab
    FOR ALL ENTRIES IN @itab
    WHERE comp1 = @itab-comp1
    INTO TABLE @DATA(it_select_result).

ENDIF.
```

**Using the `FILTER` operator**

To create an internal table by copying data from another internal table and 
filtering out lines that do not meet the `WHERE` condition, you can use the [`FILTER`
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_filter.htm).

-   The
    `FILTER` operator constructs an internal table according to a specified type (which can be an explicitly specified, non-generic table type or the `#` character as a symbol for the [operand type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_type_glosry.htm) before the first parenthesis).
- The lines for the new internal table are taken from an
    existing internal table based on conditions specified in a `WHERE` clause. Note that the table type of the existing internal table must be convertible into the specified target type.
-   The conditions can be based on either single values or a [filter
    table](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_filter_table.htm).
- Additions:

|Addition |Details |
|---|---|
|`USING KEY`  | Specifies the [table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_key_glosry.htm "Glossary Entry") used to evaluate the `WHERE` condition: either a [sorted key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensorted_key_glosry.htm "Glossary Entry") or a [hash key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhash_key_glosry.htm "Glossary Entry"). If the internal table does not have either of these, it must have a [secondary table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensecondary_table_key_glosry.htm "Glossary Entry"), which must be specified after `USING KEY`.  |
| `EXCEPT`   | Specifying `EXCEPT` means that those lines of the existing table are used that do not meet the condition specified in the `WHERE` clause. If `EXCEPT` is not specified, those lines of the existing table that meet the condition are used.  |

Examples:
```abap
"FILTER on conditions based on single values
"Assumption: The component num is of type i.
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
"In the WHERE condition, the columns of source and filter table are compared. 
"Those lines in the source table are used for which at least one line in the filter 
"table meets the condition. EXCEPT and USING KEY are also possible.

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

**Collecting values**

Use the
[`COLLECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcollect.htm)
keyword, for example, to add the values of numeric components to the
corresponding values in an internal table. Use it mainly for internal
tables with a unique primary key, especially hashed tables.
``` abap
COLLECT VALUE dtype( comp1 = a comp2 = b ... ) INTO itab.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Reading from Internal Tables

There are three different ways to specify the line to read:

- by index (only index tables)
- by table keys (only tables for which a key is defined)
- by free key

The following code snippets include [`READ TABLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapread_table.htm) statements and [table expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_expressions.htm) to read from internal tables.

**Reading single lines**

*Determining the target area*

-   Copying a line to a data object using the addition `INTO`.
    After the copying, the line found exists separately in the internal table and
    in the data object. If you change the
    data object or the table line, the change does not affect the other.
    However, you can modify the copied table line and use a
    `MODIFY` statement to modify the table based on the modified
    table line (see below). The `TRANSPORTING` addition 
    specifies which components to copy. If
    it is not specified, all components are respected.
    ``` abap
    READ TABLE itab INTO dobj ...   "dobj must have the table's structure type

    READ TABLE itab INTO DATA(dobj_inl) ...

    READ TABLE itab INTO ... TRANSPORTING comp1 [comp2 ... ].
    ```

-   Assigning a line to a [field
    symbol](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm "Glossary Entry"),
    for example, using an inline declaration (`ASSIGNING <fs>`). When you then access the field symbol, it means that you access the found table line. There is no actual copying of
    content. Therefore, modifying the field symbol means
    modifying the table line directly. Note that you cannot use the 
    `TRANSPORTING` addition since the entire table is
    assigned to the field symbol.

    ``` abap
    READ TABLE itab ASSIGNING <fs1> ...

    READ TABLE itab ASSIGNING FIELD-SYMBOL(<fs2>) ...
    ```

-   Reading a line into a [data reference
    variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry")
    using `REFERENCE INTO`. In this case, no copying takes place. If you want to address the line, you must first [dereference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm) the data reference. You cannot use the addition `TRANSPORTING`.

    ``` abap
    READ TABLE itab REFERNCE INTO dref ...

    READ TABLE itab REFERNCE INTO DATA(dref_inl) ...
    ```

**Which to use then?** Since all syntax options provide the same
functionality, your use case, the
performance or readability of the code may play a role. For more information, see 
the programming guidelines for the [target
area (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentable_output_guidl.htm "Guideline").
A use case for `INTO dobj` is when the table should
not be changed using the copied table line. However, copying comes
at a performance cost. Imagine that your table contains many columns or
nested components. In this case, it is better not to copy at all (although you can use 
the `TRANSPORTING` addition to restrict the fields to be copied).

<p align="right"><a href="#top">⬆️ back to top</a></p>

*Reading a single line by index*

The following example shows `READ TABLE` statements to read a single line from an internal table by specifying the index. You can use the addition `USING KEY` to specify a table key and thus explicitly determine the table index to use. If the table has a sorted secondary
key, the addition can be specified and the line to be read is then determined from its secondary table index. If the primary table key is
specified by its name `primary_key`, the table must be an index table, and the behavior is the same as if `USING KEY` was
not specified.
Note that the examples only show reading into a work area. Other targets are possible as shown above. 
``` abap
READ TABLE itab INTO wa INDEX i.

READ TABLE itab INTO wa INDEX i USING KEY primary_key.
```

Using a [table
expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_expressions.htm),
the read result is stored in a variable that can be declared inline.
The number in the square brackets represents the index. A line that is
not found results in an runtime error. To avoid an error, you can
use a [`TRY ... CATCH ... ENDTRY.`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptry.htm) block.

``` abap
"In the examples, integer values are specified for the index.
DATA(lv1) = itab[ 1 ].

TRY.
  DATA(lv2) = itab[ 2 ].
  CATCH cx_sy_itab_line_not_found.
  ...
ENDTRY.

DATA(lv3) = itab[ KEY primary_key INDEX 3 ].

"Copying a table line via table expression and embedding in constructor expression
DATA(lv4) = VALUE #( itab[ 4 ] ).

"Reading into data reference variable using the REF operator
DATA(lv5_ref) = REF #( itab[ 5 ] ).
```

When you read a non-existent line using a table expression, you may not want to throw an exception. You can also embed the table expression
in a constructor expression using the `OPTIONAL` addition. This way, an unsuccessful read will not trigger the 
exception. The result returned is a line with initial values.
Alternatively, you can use the `DEFAULT` addition to return a
default line in case of an unsuccessful read operation, which can also be another table expression or constructor expression.

``` abap
DATA(line1) = VALUE #( itab[ 6 ] OPTIONAL ).

DATA(line2) = VALUE #( itab[ 7 ] DEFAULT itab[ 8 ]  ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

*Reading single lines using table keys*

Lines can be read by explicitly specifying the table keys or the alias names, if any.
```abap
"Example internal table with primary and secondary key and alias names
"Assumption: All components are of type i

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

**Reading a single line using a free key**

The specified components used as keys need not be part of a table key.
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
`->*` in the case of data reference variables).
``` abap
DATA(comp1) = it[ b = 2 ]-c.

READ TABLE it INTO DATA(wa) WITH KEY b = 2.
DATA(comp2) = wa-c.

READ TABLE it ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY b = 2.
DATA(comp3) = <fs>-c.

READ TABLE it REFERENCE INTO DATA(dref) WITH KEY b = 2.
DATA(comp4) = dref->c.
"Note: The syntax dref->*-c is also possible.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

**Checking the existence and the index of a line in an internal table**

This is relevant if you are not interested in the content of a table
line, but only want to find out whether a line exists that matches to the
index or key specifications. To do this, use a [`READ TABLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapread_table.htm)
statement with the `TRANSPORTING NO FIELDS` addition. The
addition indicates that no actual content is to be read. If the search was
successful and an entry exists, the system field `sy-subrc` is
set to 0.

A newer way to check the existence of a line is the [predicate
function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpredicate_function_glosry.htm "Glossary Entry")
[`line_exists( )`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenline_exists_function.htm).
This function expects a [table
expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_expression_glosry.htm "Glossary Entry") as an argument.
See below for more on table expressions. Note that table expressions do not set system fields.
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
the line is found, the system field `sy-tabix` is set to the number of the index. Otherwise, the built-in function
[`line_index( )`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenline_index_function.htm) can be used. It returns the index of the found line or 0 if the line does not exist.
``` abap
DATA(idx) = line_index( it[ b = 2 ] ).
```

`lines( )` is another built-in function that you can use to check how many lines exist in an internal table. It returns an integer value.

``` abap
DATA(number_of_lines) = lines( it ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Processing Multiple Internal Table Lines Sequentially

If you are interested not only in single table lines, but in the entire
table content or in specific parts of it, you can use [`LOOP
AT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab.htm)
statements to process table lines sequentially. As above, you
can use multiple options for target areas: work area, field
symbol, data reference. There are multiple additions to the `LOOP AT`
statements to further restrict the table content to be processed.

Simple form:
``` abap
"The target is an existing work area.
DATA wa LIKE LINE OF it.

LOOP AT it INTO wa. 
  "No addition of the loop statement; all lines are processed
  "Statements in this block are relevant for each individual table line.
  ...
ENDLOOP.

"Work area declared inline
LOOP AT itab INTO DATA(wa_inl).
  ...
ENDLOOP.

"Field symbols
FIELD-SYMBOLS <fs> LIKE LINE OF it.

LOOP AT it ASSIGNING <fs>.
  ...
ENDLOOP.

LOOP AT it ASSIGNING FIELD-SYMBOL(<fs_inl>).
  ...
ENDLOOP.

"Data reference variables
DATA dref TYPE REF TO dbtab.

LOOP AT it REFERENCE INTO dref.
  ...
ENDLOOP.

LOOP AT it REFERENCE INTO DATA(dref_inl).
  ...
ENDLOOP.
```

- The order in which tables are iterated depends on the table category. 
  - Note the [`STEP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab_cond.htm#!ABAP_ADDITION_3@3@) addition, which is also available for other ABAP statements.  
- Index tables are looped over in ascending order by the index. 
- Hashed tables are looped in the order in which the lines were added to the table. You can also sort the table before the loop. 
- During the loop, the system field `sy-tabix` is set to the number of the currently processed table
line. This is not true for hashed tables. There, `sy-tabix` is `0`. 
- Note that if you want to work with the value of `sy-tabix`, you
should do so immediately after the `LOOP` statement to avoid possible overwriting in statements contained in the loop block.

*Restricting the area of the table to be looped over*

The additions of `LOOP` statements come into play when you want to restrict the table content to be respected by the loop because
you do not want to loop over the entire table. Note that the examples only show work areas as target objects to hold the table line read. 
Other options are possible as shown above.

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

"STEP addition for defining the step size and the direction of the loop
"- Step size: Specified by the absolute value of an integer
"- Direction: Specified by a positive (forward loop) or negative 
"  (loop in reverse order) integer 

"Reversing the loop order using a negative integer
"Each line is read indicated by the absolute value 1
LOOP AT it INTO wa STEP -1.
  ...
ENDLOOP.

"Forward loop by specifiying a positive integer
"In the example, every second line is read.
"Note: Omitting STEP means STEP 1 by default.
LOOP AT it INTO wa STEP 2.
  ...
ENDLOOP.

"STEP with other additions
"The example uses the additions FROM and TO.
"Note: If the value after STEP is negative, the value
"after FROM must be greater than the value after TO.
LOOP AT it INTO wa FROM 6 TO 3 STEP -2.
  ...
ENDLOOP.
```

*Iterations with* `FOR`

Iteration expressions with [`FOR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfor.htm) as part of certain constructor expressions allow you to create content of an internal table by evaluating one or more source tables.

The following examples show iterations with `FOR` within a constructor expression with `VALUE`. A new table is created and
values for two fields are inserted into the new table, which has the internal table type of the source table. `ls` represents an iteration
variable that holds the data as the table is looped over. The components, and thus the table line to be returned, are
specified within the pair of parentheses before the closing parenthesis. Both examples set specific values for the components. The second example also includes a `WHERE` clause to restrict the lines to be copied.

Unlike `LOOP` statements, this sequential processing cannot be debugged.
``` abap
"Internal table type
TYPES ttype like it.

DATA(tab1) = VALUE ttype( FOR ls IN it ( a = ls-a b = 9 ) ).

DATA(tab2) = VALUE ttype( FOR ls IN it WHERE ( a < 7 )
                            ( a = ls-a b = ls-b + 5  ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Sorting Internal Tables

-   Sorted tables are stored in the memory in an automatically sorted
    order, hence, they cannot be sorted explicitly with
    [`SORT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapsort_itab.htm).
-   For standard and hashed tables, the order can be changed.
-   When using `SORT` statements, the sort order is derived either
    by the primary table key (Note: Secondary keys
    cannot be used for the sorting.) or by explicitly specifying the
    fields to be sorted by.
-   Explicit specification is the recommended way because it is
    easier to understand and can prevent unwanted sorting results,
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

The effect of sorting can have an unexpected result if you use the simple form of the statement and do not explicitly specify the keys. If an internal table has a structured line type and (perhaps inadvertently) the standard key as the primary table key, that is, all character-like and byte-like components make up the primary table key, all these components are taken into account when the table is sorted.
``` abap
"Is basically the same as it2
DATA it1 TYPE TABLE OF zdemo_abap_fli.

DATA it2 TYPE STANDARD TABLE OF zdemo_abap_fli WITH DEFAULT KEY.

"Respect the standard key when sorting.
SORT it1.
```
Plus: Suppose there are only elementary numeric components in an internal table with a structured line type. In this case, sorting has no effect because the primary table key is considered empty. This is certainly also true for tables declared with `EMPTY KEY`.

*Sorting by explicitly specifying components*

You can sort by any component of the internal table. It is also possible to specify the sort order 
(even component-wise). Explicitly specifying the components has the advantage that your code is easier to understand and you can avoid unexpected results if you accidentally use `SORT` without the `BY` addition on empty and standard table keys.

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Modifying Internal Table Content

As mentioned above, you can modify the content of internal table lines directly in the context of `READ TABLE` and `LOOP AT` statements using field symbols and data reference variables. You can also use table expressions for direct modification. Note that the key fields of the primary table key of sorted and hashed tables are always read-only. If you try to modify a key field, a runtime error occurs. However, this is not checked until runtime.

The following examples demonstrate direct modification of recently read table lines:
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
> If you want to modify recently read lines in a work area, for example, within a loop (`LOOP AT INTO dobj`), you can modify the line and then use a `MODIFY` statement to modify the internal table based on this line.

[`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_itab.htm)
statements provide multiple ways of changing the content of single and multiple table lines by specifying the table key or a table index,
without first reading the lines into a target area.

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Deleting Internal Table Content

You can use [`DELETE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdelete_itab.htm) statements to delete single and multiple lines in internal tables.
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

`DELETE ADJACENT DUPLICATES` statements allow you to delete all adjacent lines except for the first line that have the same content in certain components. You usually need to perform some appropriate sorting before using these statements.
``` abap
"Implicitly uses the primary table key

DELETE ADJACENT DUPLICATES FROM it.

"Deletion respecting the values of the entire line

DELETE ADJACENT DUPLICATES FROM it COMPARING ALL FIELDS.

"Only lines are deleted with matching content in specific fields

DELETE ADJACENT DUPLICATES FROM it COMPARING a c.

"Deletion respecting a specified table key

"Same as first example above
DELETE ADJACENT DUPLICATES FROM it USING KEY primary_key.

DELETE ADJACENT DUPLICATES FROM it USING KEY sec_key.
```

> **💡 Note**<br>
> The system field `sy-subrc` is set to `0` if at least one line has been deleted. It is set to `4` if no lines were deleted.

The 
[`CLEAR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclear.htm)
and
[`FREE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfree_dataobject.htm)
statements allow you to delete the entire table content.

The difference between the two is in the handling of the memory space originally allocated to the table. When a table is cleared with `CLEAR`,
the content are removed, but the memory space initially requested remains
allocated. If the table is filled again later, the memory space is still
available, which is a performance advantage over 
clearing an internal table with `FREE`. Such a statement also
deletes the table content, but it also releases the memory
space. 
Note that an assignment using the `VALUE` operator without entries in the parentheses clears the internal table. 

``` abap
CLEAR it.

"This statement additionally releases memory space.
FREE it.

"Assignment using the VALUE operator without entries in the parentheses
it = VALUE #( ). 
```
<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information
Topic [Internal
Tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenitab.htm) in the ABAP Keyword Documentation.

## Executable Example
[zcl_demo_abap_internal_tables](./src/zcl_demo_abap_internal_tables.clas.abap)

Follow the steps outlined [here](README.md#-getting-started-with-the-examples) to import and run the code.