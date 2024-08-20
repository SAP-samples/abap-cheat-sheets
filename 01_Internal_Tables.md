<a name="top"></a>

# Internal Tables

- [Internal Tables](#internal-tables)
  - [Introduction](#introduction)
  - [Basic Properties of Internal Tables](#basic-properties-of-internal-tables)
  - [Table Keys (Primary, Secondary, Standard, Empty) and Table Indexes](#table-keys-primary-secondary-standard-empty-and-table-indexes)
  - [Creating Internal Tables and Types](#creating-internal-tables-and-types)
  - [Populating Internal Tables](#populating-internal-tables)
    - [Copying Internal Tables](#copying-internal-tables)
    - [Using INSERT and APPEND Statements to Populate Internal Tables](#using-insert-and-append-statements-to-populate-internal-tables)
    - [Creating and Populating Internal Tables Using Constructor Expressions](#creating-and-populating-internal-tables-using-constructor-expressions)
      - [VALUE operator](#value-operator)
      - [CORRESPONDING Operator](#corresponding-operator)
      - [FILTER Operator](#filter-operator)
      - [NEW Operator](#new-operator)
    - [Example: Exploring Populating Internal Tables](#example-exploring-populating-internal-tables)
  - [Reading Single Lines from Internal Tables](#reading-single-lines-from-internal-tables)
    - [Determining the Target Area when Reading Single Lines](#determining-the-target-area-when-reading-single-lines)
    - [Reading a Single Line by Index](#reading-a-single-line-by-index)
    - [Reading a Single Line Using Table Keys](#reading-a-single-line-using-table-keys)
    - [Reading a Single Line Using a Free Key](#reading-a-single-line-using-a-free-key)
    - [Examples of Addressing Individual Components of Read Lines](#examples-of-addressing-individual-components-of-read-lines)
    - [Excursions with READ TABLE Statements](#excursions-with-read-table-statements)
      - [System Field Setting in READ TABLE Statements](#system-field-setting-in-read-table-statements)
      - [COMPARING and TRANSPORTING Additions: Comparing Fields and Specifying Fields for Transport](#comparing-and-transporting-additions-comparing-fields-and-specifying-fields-for-transport)
      - [CASTING and ELSE UNASSIGN Additions when Specifying Field Symbols as Target Areas](#casting-and-else-unassign-additions-when-specifying-field-symbols-as-target-areas)
      - [BINARY SEARCH Addition: Optimized Read Access When Specifying Free Keys](#binary-search-addition-optimized-read-access-when-specifying-free-keys)
    - [Example: Exploring READ TABLE Statements and Table Expressions](#example-exploring-read-table-statements-and-table-expressions)
  - [Getting Information about Internal Tables, Table Lines, Table Types](#getting-information-about-internal-tables-table-lines-table-types)
    - [Checking the Existence of a Line in an Internal Table](#checking-the-existence-of-a-line-in-an-internal-table)
    - [Checking the Index of a Line in an Internal Table](#checking-the-index-of-a-line-in-an-internal-table)
    - [Checking How Many Lines Exist in an Internal Table](#checking-how-many-lines-exist-in-an-internal-table)
    - [Getting Table (Type) Information at Runtime](#getting-table-type-information-at-runtime)
  - [Processing Multiple Internal Table Lines Sequentially](#processing-multiple-internal-table-lines-sequentially)
    - [Restricting the Area of a Table to Be Looped Over](#restricting-the-area-of-a-table-to-be-looped-over)
    - [Defining the Step Size and the Direction of Loop Passes](#defining-the-step-size-and-the-direction-of-loop-passes)
    - [Iteration Expressions](#iteration-expressions)
    - [Interrupting and Exiting Loops](#interrupting-and-exiting-loops)
  - [Operations with Internal Tables Using ABAP SQL SELECT Statements](#operations-with-internal-tables-using-abap-sql-select-statements)
    - [Internal Tables as Target Data Objects in SELECT Queries](#internal-tables-as-target-data-objects-in-select-queries)
    - [SELECT Queries with Internal Tables as Data Sources](#select-queries-with-internal-tables-as-data-sources)
      - [Excursion: Restrictions Regarding Selecting from Internal Tables](#excursion-restrictions-regarding-selecting-from-internal-tables)
  - [Sorting Internal Tables](#sorting-internal-tables)
  - [Modifying Internal Table Content](#modifying-internal-table-content)
  - [Deleting Internal Table Content](#deleting-internal-table-content)
    - [Deleting Adjacent Duplicate Lines](#deleting-adjacent-duplicate-lines)
    - [Deleting the Entire Internal Table Content](#deleting-the-entire-internal-table-content)
  - [Grouping Internal Tables](#grouping-internal-tables)
  - [Excursions](#excursions)
    - [Improving Read Performance with Secondary Table Keys](#improving-read-performance-with-secondary-table-keys)
    - [Example: Exploring Read Access Performance with Internal Tables](#example-exploring-read-access-performance-with-internal-tables)
    - [Searching and Replacing Substrings in Internal Tables with Character-Like Data Types](#searching-and-replacing-substrings-in-internal-tables-with-character-like-data-types)
    - [Ranges Tables](#ranges-tables)
    - [Comparing Content of Compatible Internal Tables](#comparing-content-of-compatible-internal-tables)
    - [BDEF Derived Types (ABAP EML)](#bdef-derived-types-abap-eml)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)


## Introduction

Internal Tables ...

- are tables that temporarily store variable data (i.e. any number of table lines of a fixed structure) in the working memory in ABAP.
- are [dynamic data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendynamic_data_object_glosry.htm), i.e. all properties apart from the memory consumption are determined statically by the [data type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_type_glosry.htm).
- consist of a variable sequence of lines of the same data type. 
- have a [table type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_type_glosry.htm) as its data type (it is a [complex data type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplex_data_type_glosry.htm)), which defines the following properties: 
  - [line type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrow_type_glosry.htm)
  - [table category](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_category_glosry.htm)
  - [table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_key_glosry.htm)
- are used when a variable data set of a random data type needs to be processed in a structured way, for example, storing and processing [database table](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendatabase_table_glosry.htm) content within an [ABAP program](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm).
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
|`STANDARD`|Primary table index (that's why these tables are called [index tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenindex_table_glosry.htm))|<ul><li>Table index</li><li>Table key</li></ul>|<ul><li>Always non-unique, i.e. duplicate entries are always allowed</li><li>Definition of an empty key is possible if the key is not relevant(`WITH EMPTY KEY`)</li></ul>|<ul><li>If you primarily access the table content for sequential processing or via the table index.</li><li>Response time for accessing the table using the primary key: This kind of table access is optimized only for sorted and hashed tables. For standard tables, primary key access uses a linear search across all lines. That means that large standard tables (more than 100 lines) are not ideal if the you primarily access the table using the table key.</li></ul>|<ul><li>There is no particular sort order, but the tables can be sorted using `SORT`.</li><li>Populating this kind of table: Lines are either appended at the end of the table or inserted at a specific position.</li><li>[Secondary table keys](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensecondary_table_key_glosry.htm) can be defined to make key access to standard tables more efficient.</li><li>Standard and sorted tables have the least [administration costs (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenadmin_costs_dyn_mem_obj_guidl.htm).</li></ul>|
|`SORTED`|Primary table index (that's why these tables are called [index tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenindex_table_glosry.htm))|<ul><li>Table index</li><li>Table key</li></ul>|<ul><li>Non-unique</li><li>Unique</li><br>... used to sort the table in ascending order.</ul>|<ul><li>Enables an optimized access to table content using table key and index.</li><li>If access via table key is the main access method, but no unique key can be defined.</li></ul>|<ul><li>Sorting is done automatically when lines are inserted or deleted. As a consequence, the table index must usually be reorganized. </li><li>The response time for accessing the table using the primary key depends logarithmically on the number of table entries, since a binary search is used.</li><li>Standard and sorted tables have the least administration costs.</li></ul>|
|`HASHED`|Hash algorithm |<ul><li>Table key</li><li>[Secondary table index](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensecondary_table_index_glosry.htm)</li></ul>|Always unique|<ul><li>For large internal tables.</li><li>Optimized for key access. Access to table content via table key is the main access method and a unique key can be defined.</li></ul>|<ul><li>The response time for primary key access is constant and independent of the number of entries in the table.</li><li>Hashed tables have the highest administration costs.</li></ul>|



**Key Attributes** 

- There are two types of table keys: a [primary table key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprimary_table_key_glosry.htm) and [secondary table keys](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensecondary_table_key_glosry.htm).
- Table keys ...
  - are intended to provide an optimized access to the content of internal tables.
  - are either unique or non-unique, i.e. more than one line with the same key (duplicates) can exist in the internal table or not. Regarding the primary table key, the definition depends on the table category. For the secondary table key, the definition depends on the key type. For standard tables, the primary table key can also be defined as empty, i.e. it does not contain any key columns. Note that for standard tables, an optimized access is only possible with secondary table keys.
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

## Table Keys (Primary, Secondary, Standard, Empty) and Table Indexes

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
- When accessing internal tables using the table key, the primary key is always used implicitly in processing statements if no secondary key is specified. Note that the primary table key must be specified in table expressions if the primary key is to be used explicitly.

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

> **💡 Primary table index**<br>
> - Index tables, such as sorted and standard tables, have a primary table index. 
> - Each table line receives a unique line number from the primary table index. 
> - The index updates each time a line is added or removed. 

**Secondary table keys**

- Secondary table keys ...
  - are optional for all table categories.
  - can be unique/non-unique sorted keys or unique hash keys.
  - have a self-defined name. An alias name can also be specified.
- A [secondary table index](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensecondary_table_index_glosry.htm) is created internally for each sorted secondary key. This allows index access to hashed tables via the secondary table key. In this case, `sy-tabix` is set.
- When accessing internal tables using the secondary table key, the key name (or the alias if specified) must be specified. They are not selected automatically. If no secondary key is specified in a processing statement, the primary key or primary table index is always used. If you want to make use of this key in ABAP statements, for example, `READ`, `LOOP AT` or `MODIFY` statements, you must specify the key explicitly using the appropriate additions, for example, `WITH ... KEY ... COMPONENTS` or `USING KEY`.
- Use cases:
  - To improve read performance.
  - For very large internal tables (that are populated once and changed very often) 
  - Standard tables, whose primary table keys cannot be unique, can be provided with a means of ensuring that unique table entries are read. 
 - Note that defining secondary table keys involves additional administration costs (additional memory consumption). Therefore, the use of secondary table keys should be reserved for cases where the benefits outweigh the extra costs. 
- For more details, see the programming guidelines for secondary keys: [Secondary
    Key (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensecondary_key_guidl.htm "Guideline").

> **💡 Note**<br>
> - See examples of internal table declarations using the table keys mentioned above in the following section.
> - See more information and examples that focus on table access using keys and index below.
 
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
> - If the table category is not specified (`... TYPE TABLE OF ...`), it is automatically `... TYPE STANDARD TABLE OF ...`.
> - Using [Runtime Type Creation (RTTC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_creation_glosry.htm "Glossary Entry"), you can define and  create new internal tables and table types as [type description objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_object_glosry.htm) at runtime. For more information, see the [Dynamic Programming](06_Dynamic_Programming.md) ABAP cheat sheet.

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
"Here, an internal table is created containing internal tables of the 
"type of itab_a6.
DATA itab_a7 LIKE TABLE OF itab_a6.

"Creating internal tables by inline declarations

"Table declared inline in the context of an assignment
"The examples show the copying of a table including the content on the fly
"and creating the table in one step. The data type of the
"declared variable is determined by the right side.
DATA(it_inline1) = itab_a1.
DATA(it_inline2) = it_inline1.

"Using FINAL for creating immutable variables
FINAL(it_final) = it_inline1.

"Using the VALUE operator and an internal table type
DATA(it_inline3) = VALUE tt_loc_str( ( ... ) ).

"Not providing any table lines means the table is initial 
"and has the same effect as the declaration of
"itab_a1 above.
DATA(it_inline4) = VALUE tt_loc_str( ).

"Table declared inline in the context of a SELECT statement;
"a prior extra declaration of an internal table is not needed.
SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(it_inline5).

"Instead of
DATA it_sel TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.
SELECT * FROM zdemo_abap_fli INTO TABLE @it_sel.

"Using FINAL
SELECT * FROM zdemo_abap_fli INTO TABLE @FINAL(it_inline6).
```

> **💡 Note**<br>
> Internal tables can also be created dynamically. Find more information in the [Dynamic Programming cheat sheet](06_Dynamic_Programming.md).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Populating Internal Tables

### Copying Internal Tables

To copy internal table content from one table to another, you can use the assignment operator. Such an assignment (without a constructor expression) deletes the existing content in the target internal table. The example below assumes that the source and target table have compatible line types. Using inline declaration is helpful to avoid an additional internal table declaration with an appropriate type.

``` abap
itab = itab2.

DATA(itab3) = itab.
FINAL(itab4) = itab.
```

> **💡 Note**<br>
> - Internal tables can only be assigned to internal tables. 
> - Internal tables can be assigned to each other if their line types are [compatible](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencompatible_glosry.htm) or [convertible](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconvertible_glosry.htm).
> - An assignment can trigger an uncatchable exception if, for example, the target table is assigned a duplicate of a unique primary table key or secondary table.
> - More information: [Conversion Rules for Internal Tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_itab.htm)

### Using INSERT and APPEND Statements to Populate Internal Tables

You can use the ABAP keywords
[`INSERT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinsert_itab.htm)
and [`APPEND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapappend.htm)
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
    - With `INSERT`, you can specify the position in the internal table at which lines are inserted after `INTO`. 
        - `... INTO TABLE itab ...`: Line is inserted in ...
          - standard tables as last line (i.e. appended) 
          - sorted tables in the sort order in accordance with primary key values
          - hashed table by the hash administration in accordance with primary key values
        - `... INTO itab INDEX n`: Possible for index tables. The line is inserted before the line with the line number `n` in the primary table index.
    -   Note: In the case of unique primary table keys, the table cannot have entries with duplicate
        keys. If a duplicate is inserted, the insertion fails and the
        system field `sy-subrc` is set to 4.
- What to use? The recommendation is the `INSERT` statement. It covers all table and key types. Consider potential issues when you change table/key types, and you use `APPEND` in your code.         
</details>
<br>

**Adding a line to an internal table**. The example shows both a structure that is created using the `VALUE` operator as well as an existing structure that is added.

``` abap
APPEND VALUE #( comp1 = a comp2 = b ... ) TO itab.
APPEND struc TO itab.

INSERT VALUE #( comp1 = a comp2 = b ... ) INTO TABLE itab.
INSERT struc INTO TABLE itab.
```

**Adding an initial line** to an internal table without providing any field values.

``` abap
APPEND INITIAL LINE TO itab.

INSERT INITIAL LINE INTO TABLE itab.
```

**Adding a line and assigning the added line to a field symbol or data reference variable**.
```abap
"When inserting single lines, you can specify the optional additions 
"ASSIGNING and REFERENCE INTO. If the insertion is successful, the 
"line is assigned to a field symbol or a data reference variable.
"The targets can also be created inline. 
APPEND VALUE #( comp1 = a comp2 = b ... ) TO itab ASSIGNING FIELD-SYMBOL(<fs>).
APPEND INITIAL LINE TO itab ASSIGNING <fs>.
INSERT INITIAL LINE INTO TABLE itab REFERENCE INTO DATA(dref).

"Note: Once assigned, you can then, for example, address individual components
"of the (initial) line and assign values, e.g. <fs>-comp = ... or dref->comp = ....
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

INSERT LINES OF itab2 FROM i1 TO i2 INTO TABLE itab.
```

**Inserting one line or multiple lines from another internal table at a specific position**. 
`FROM` and `TO` can be used here, too.

``` abap
INSERT struc INTO itab2 INDEX i.

INSERT LINES OF itab2 INTO itab INDEX i.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating and Populating Internal Tables Using Constructor Expressions
The constructor expressions can be specified in/with various positions/statements in ABAP. In most of the following snippets, simple assignments are demonstrated.

> **💡 Note**<br>
> The following sections cover a selection. There are more constructor expressions used in the context of internal tables (e.g. for creating internal tables). Find more details in the [Constructor Expressions](05_Constructor_Expressions.md) ABAP cheat sheet.

#### VALUE operator
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

"Creating string tables
DATA(str_tab_a) = VALUE string_table( ( `Hallo` ) ( `World` ) ).                            
DATA(str_tab_b) = VALUE string_table( ).                            
"In the previous statement, the internal table is declared
"inline, however, no content, no table lines are provided. 
"This means that an initial string table was created. This 
"way, the statement has the same effect as the following 
"statement.
DATA str_tab_c TYPE string_table.
```

When you use the above assignments (`itab = ...`), the internal table is initialized and the existing content is deleted. To add new lines **without deleting existing content**, use the  [`BASE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenvalue_constructor_params_itab.htm) addition.

``` abap
itab = VALUE #( BASE itab ( comp1 = a comp2 = b ... )
                          ( comp1 = c comp2 = d ... ) ).
```

**Adding lines of other tables** using the `LINES OF` addition to the `VALUE` operator.
> **💡 Note**<br>
> Without the `BASE` addition, the existing content is deleted. It is assumed that the line type of the source table is compatible with that of the target table.
``` abap
itab = VALUE #( ( comp1 = a comp2 = b ...)
                ( comp1 = a comp2 = b ...)
                ( LINES OF itab2 )
                ... ).
```

#### CORRESPONDING Operator

**Copying the content of another internal table** using the
[`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm) operator. 
- Note that the existing content is deleted.
- As an alternative to the `CORRESPONDING` operator, you can use [`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove-corresponding.htm) statements. 
- The example assumes that the line types of the source and target table are not compatible. However, if the line types are compatible, the syntax will also work.
- Several additions are possible. They can also be combined. Check the ABAP Keyword Documentation.
  
``` abap
itab = CORRESPONDING #( itab3 ).

MOVE-CORRESPONDING itab3 TO itab.
```

**Copying content and retaining existing content** using the `CORRESPONDING` operator. 
The `KEEPING TARGET LINES` addition of the `MOVE-CORRESPONDING` statement preserves the table content.

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

itab_nested2 = CORRESPONDING #( DEEP BASE ( itab_nested2 ) itab_nested1 ).

MOVE-CORRESPONDING itab_nested1 TO itab_nested2 EXPANDING NESTED TABLES.

MOVE-CORRESPONDING itab_nested1 TO itab_nested2 EXPANDING NESTED TABLES KEEPING TARGET LINES.
```

#### FILTER Operator

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

> **💡 Note**<br>
> More constructor expressions are available to deal with internal tables, for example the `REDUCE` operator. Find more information and examples in the [Constructor Expression cheat sheet](05_Constructor_Expressions.md).

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### NEW Operator

Using the instance operator [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm), you can create [anonymous data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm "Glossary Entry"), such as anonymous internal tables. You can access the lines, components or the entire data objects by [dereferencing](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm). For more information, refer to the  [Dynamic Programming](06_Dynamic_Programming.md) and [Constructor Expressions](05_Constructor_Expressions.md) cheat sheets.

```abap
TYPES: BEGIN OF s,
          a TYPE c LENGTH 3,
          b TYPE i,
        END OF s,
        tab_type TYPE TABLE OF s WITH EMPTY KEY.

"Creating and populating an anonymous data object
DATA(dref_tab) = NEW tab_type( ( a = 'aaa' b = 1 )
                               ( a = 'bbb' b = 2 ) ).

"Access by derefencing
DATA(copy_deref_itab) = dref_tab->*.
DATA(read_line) = dref_tab->*[ 2 ].
DATA(read_comp) = dref_tab->*[ 1 ]-a.
dref_tab->*[ 1 ]-a = 'zzz'.
ASSERT dref_tab->*[ 1 ]-a = 'zzz'.
INSERT VALUE s( a = 'yyy' b = 3 ) INTO TABLE dref_tab->*.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example: Exploring Populating Internal Tables

Expand the following collapsible section to view the code of an example. To try it out, create a demo class named `zcl_some_class` and paste the code into it. After activation, choose *F9* in ADT to execute the class. 
The example is set up to display output in the console, but only for few data objects. You may want to set a break point at the earliest possible position and walk through the example in the debugger. This will allow you to double-click on data objects and observe how the different statements affect their contents.

<details>
  <summary>Expand to view the code</summary>
  <!-- -->

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

    "Populating internal tables by adding a line (structure) using
    "APPEND ... TO/INSERT ... INTO

    TYPES: BEGIN OF st_a,
             num  TYPE i,
             str  TYPE string,
             char TYPE c LENGTH 2,
           END OF st_a,
           ty_tab_a TYPE TABLE OF st_a WITH EMPTY KEY.
    DATA it_a TYPE ty_tab_a.

    "Adding a line created inline
    APPEND VALUE #( num = 1 str = `A` char = 'bb' ) TO it_a.
    INSERT VALUE #( num = 2 str = `C` char = 'dd' ) INTO TABLE it_a.

    "Adding an existing line
    DATA(struc_a) = VALUE st_a( num = 3 str =  `E` char = 'ff' ).
    "Structure whose components are assigned individually using the
    "structure component selector
    DATA struc_b TYPE st_a.
    struc_b-num = 4.
    struc_b-str =  `G`.
    struc_b-char = 'hh'.

    APPEND struc_a TO it_a.
    INSERT struc_b INTO TABLE it_a.

**********************************************************************
    "INITIAL LINE addition
    "Adding an initial line

    APPEND INITIAL LINE TO it_a.
    INSERT INITIAL LINE INTO TABLE it_a.

**********************************************************************
    "ASSIGNING/REFERENCE INTO additions

    "Adding a line and assigning the added line to a field symbol or
    "data reference variable

    "Creating field symbol inline
    APPEND VALUE st_a( num = 5 str =  `I` char = 'jj' ) TO it_a ASSIGNING FIELD-SYMBOL(<fs_a>).
    "Addressing individual components
    ASSERT <fs_a>-num = 5.
    <fs_a>-num = 123.

    FIELD-SYMBOLS <fs_b> TYPE st_a.
    DATA(struc_c) = VALUE st_a( num = 6 ).
    INSERT struc_c INTO TABLE it_a ASSIGNING <fs_b>.
    <fs_b>-str =  `K`.

    "Adding an initial line
    "The examples use data reference variables.
    "Using inline declaration
    APPEND INITIAL LINE TO it_a REFERENCE INTO DATA(dref_a).
    dref_a->num = 7.
    DATA dref_b TYPE REF TO st_a.
    INSERT INITIAL LINE INTO TABLE it_a REFERENCE INTO dref_b.
    dref_b->num = 8.

    DO 3 TIMES.
      APPEND INITIAL LINE TO it_a REFERENCE INTO dref_b.
      dref_b->* = VALUE #( num = sy-index str = sy-index char = sy-index ).
    ENDDO.

**********************************************************************
    "LINES OF addition
    "Adding all lines from another internal table

    "Adding lines to one internal table that are all added to
    "another one
    DATA it_b TYPE ty_tab_a.
    INSERT VALUE #( num = 99 str =  `L` char = 'mm' ) INTO TABLE it_b.
    INSERT VALUE #( num = 100 str =  `N` char = 'oo' ) INTO TABLE it_b.

    APPEND LINES OF it_b TO it_a.
    INSERT LINES OF it_b INTO TABLE it_a.

**********************************************************************
    "Adding multiple lines from another internal table with
    "a specified index range

    APPEND LINES OF it_a FROM 5 TO 7 TO it_b.
    APPEND LINES OF it_a FROM 12 TO it_b. "further lines up to the last line
    APPEND LINES OF it_a TO 3 TO it_b.  "all lines from the start up to the specified index
    INSERT LINES OF it_a FROM 8 TO 10 INTO TABLE it_b.

**********************************************************************
    "INDEX addition
    "Inserting one line or multiple lines from another internal table at
    "a specific position. To be used for index tables.

    INSERT VALUE #( num = 9 str =  `P` char = 'qq' ) INTO it_b INDEX 2.
    INSERT LINES OF VALUE ty_tab_a( ( num = 10 str =  `R` ) ( num = 11 str =  `S` ) ) INTO it_b INDEX 5.
    "FROM and TO can also be used
    INSERT LINES OF it_a FROM 1 TO 3 INTO it_b INDEX 1.

**********************************************************************
    "Using the VALUE operator

    DATA(struc_d) = VALUE st_a( num = 11 str =  `T` char = 'uu' ).
    DATA it_c TYPE ty_tab_a.

    "Adding an existing line and a line created inline
    it_c = VALUE #( ( struc_d )
                    ( num = 11 str =  `V` char = 'ww' ) ).

    "Creating an internal table by inline declaration and adding lines with VALUE
    DATA(it_d) = VALUE ty_tab_a( ( num = 12 str =  `X` char = 'yy' )
                                 ( num = 13 str =  `Z` char = 'aa' )
                                 ( struc_d ) ).

    "******* BASE addition *******
    "Adding new lines without deleting existing content
    it_d =  VALUE #( BASE it_d ( num = 14 str =  `B` char = 'cc' )
                               ( num = 15 str =  `D` char = 'ee' ) ).

    "******* LINES OF addition *******
    "Adding lines of other tables
    it_d = VALUE #( ( LINES OF it_c ) ). "No BASE addition, existing content is deleted

    it_c = VALUE #( BASE it_c ( num = 16 str =  `F` char = 'gg' )
                              ( LINES OF it_d ) ).

**********************************************************************
    "CORRESPONDING operator / MOVE-CORRESPONDING statements

    "Creating and populating demo internal tables
    TYPES: BEGIN OF st_b,
             num    TYPE i,
             char   TYPE c LENGTH 2,
             comp_a TYPE string,
           END OF st_b,
           ty_tab_b TYPE TABLE OF st_b WITH EMPTY KEY,
           BEGIN OF st_c,
             num    TYPE i,
             char   TYPE c LENGTH 2,
             comp_b TYPE string,
           END OF st_c,
           ty_tab_c TYPE TABLE OF st_c WITH EMPTY KEY.

    DATA(it_e_original) = VALUE ty_tab_b( ( num = 1 char = 'aa' comp_a = `B` )
                                          ( num = 2 char = 'cc' comp_a = `D` ) ).
    DATA(it_f_original) = VALUE ty_tab_c( ( num = 3 char = 'ee' comp_b = `F` )
                                          ( num = 4 char = 'gg' comp_b = `H` ) ).

    DATA(it_e) = it_e_original.
    DATA(it_f) = it_f_original.

    "Copying the content of another internal table respecting identically
    "named components
    "it_f -> it_e
    it_e = CORRESPONDING #( it_f ).

    "it_e -> it_f
    it_e = it_e_original.
    MOVE-CORRESPONDING it_e TO it_f.

    "******* BASE addition / /KEEPING TARGET LINES addition *******
    "Copying content and retaining existing content
    it_e = it_e_original.
    it_f = it_f_original.

    it_e = CORRESPONDING #( BASE ( it_e ) it_f ).

    it_e = it_e_original.
    it_f = it_f_original.
    MOVE-CORRESPONDING it_e TO it_f KEEPING TARGET LINES.

    "******* MAPPING addition *******
    "Assigning components using mapping relationships
    it_e = it_e_original.
    it_f = it_f_original.

    it_e = CORRESPONDING #( it_f MAPPING comp_a = comp_b ).
    it_e = it_e_original.
    it_f = CORRESPONDING #( BASE ( it_f ) it_e MAPPING comp_b = comp_a ). "Retaining content with BASE

    "******* EXCEPT addition *******
    "Excluding components from the assignment
    it_e = it_e_original.
    it_f = it_f_original.

    it_e = CORRESPONDING #( it_f EXCEPT char ).

    it_e = it_e_original.
    it_f = CORRESPONDING #( it_e MAPPING comp_b = comp_a EXCEPT * ). "Mapping components

    "******* DISCARDING DUPLICATES addition *******
    "Preventing runtime errors when duplicate lines are assigned
    it_e = VALUE #( ( num = 1 char = 'aa' comp_a = `B` )
                    ( num = 1 char = 'cc' comp_a = `D` ) ).

    DATA it_g TYPE SORTED TABLE OF st_b WITH UNIQUE KEY num.

    "The statement commented out raises the runtime error ITAB_DUPLICATE_KEY.
    "it_g = CORRESPONDING #( it_e ).
    it_g = CORRESPONDING #( it_e DISCARDING DUPLICATES ).

    "******* DEEP addition / EXPANDING NESTED TABLES addition *******
    "Handling deep components such as nested internal tables

    "Creating and populating demo internal tables
    TYPES: BEGIN OF st_d,
             char_a TYPE c LENGTH 2,
             char_b TYPE c LENGTH 2,
           END OF st_d,
           BEGIN OF st_e,
             char_b TYPE c LENGTH 2,
             char_c TYPE c LENGTH 2,
           END OF st_e,
           BEGIN OF st_f,
             comp1 TYPE c LENGTH 2,
             comp2 TYPE c LENGTH 2,
             comp3 TYPE TABLE OF st_d WITH EMPTY KEY,
           END OF st_f,
           BEGIN OF st_g,
             comp2 TYPE c LENGTH 2,
             comp3 TYPE TABLE OF st_e WITH EMPTY KEY,
             comp4 TYPE c LENGTH 2,
           END OF st_g,
           ty_tab_d TYPE TABLE OF st_f WITH EMPTY KEY,
           ty_tab_e TYPE TABLE OF st_g WITH EMPTY KEY.

    DATA(it_h_original) = VALUE ty_tab_d(
      ( comp1 = 'a1' comp2 = 'a2' comp3 = VALUE #( ( char_a = 'a3' char_b = 'a4' ) ( char_a = 'a5' char_b = 'a6' ) ) )
      ( comp1 = 'b1' comp2 = 'b2' comp3 = VALUE #( ( char_a = 'b3' char_b = 'b4' ) ( char_a = 'b5' char_b = 'b6' ) ) ) ).

    DATA(it_i_original) = VALUE ty_tab_e(
      ( comp2 = 'c1' comp3 = VALUE #( ( char_b = 'c2' char_c = 'c3' ) ( char_b = 'c4' char_c = 'c5' ) ) comp4 = 'c6' )
      ( comp2 = 'd1' comp3 = VALUE #( ( char_b = 'd2' char_c = 'd3' ) ( char_b = 'd4' char_c = 'd5' ) ) comp4 = 'd6' ) ).

    DATA(it_h) = it_h_original.
    DATA(it_i) = it_i_original.

    "Compare the output of the examples
    out->write( `******* CORRESPONDING *******` ).
    "Note: The following example uses just CORRESPONDING. The outcome of the assignment
    "is a different one compared to using DEEP. Refer to the ABAP Keyword Documentation
    "for more details.
    it_h = CORRESPONDING #( it_i ).
    out->write( it_h ).

    out->write( `******* CORRESPONDING ... DEEP *******` ).
    it_h = CORRESPONDING #( DEEP it_i ).
    out->write( it_h ).

    out->write( `******* CORRESPONDING ... DEEP BASE *******` ).
    it_h = it_h_original.
    it_h = CORRESPONDING #( DEEP BASE ( it_h ) it_i ).
    out->write( it_h ).

    out->write( `******* MOVE-CORRESPONDING ... EXPANDING NESTED TABLES *******` ).
    it_h = it_h_original.
    MOVE-CORRESPONDING it_i TO it_h EXPANDING NESTED TABLES.
    out->write( it_h ).

    out->write( `******* MOVE-CORRESPONDING ... EXPANDING NESTED TABLES KEEPING TARGET LINES *******` ).
    it_h = it_h_original.
    MOVE-CORRESPONDING it_i TO it_h EXPANDING NESTED TABLES KEEPING TARGET LINES.
    out->write( it_h ).
  ENDMETHOD.
ENDCLASS.
```
</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Reading Single Lines from Internal Tables

There are three different ways to specify the line to read:

- by index (only index tables)
- by table keys (only tables for which a key is defined)
- by free key

The following code snippets include [`READ TABLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapread_table.htm) statements and [table expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_expressions.htm) to read from internal tables.

### Determining the Target Area when Reading Single Lines

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
    for example, using an inline declaration (`... ASSIGNING FIELD-SYMBOL(<fs>) ...`). When you then access the field symbol, it means that you access the found table line. There is no actual copying of
    content. Therefore, modifying the field symbol means
    modifying the table line directly. Note that you cannot use the 
    `TRANSPORTING` addition since the entire table is
    assigned to the field symbol.

    ``` abap
    READ TABLE itab ASSIGNING <fs1> ...                 "The field symbol must have an appropriate type.

    READ TABLE itab ASSIGNING FIELD-SYMBOL(<fs2>) ...   "The field symbol is created inline.
    ```

-   Reading a line into a [data reference
    variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry")
    using `REFERENCE INTO`. In this case, no copying takes place. If you want to address the line, you must first [dereference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm) the data reference. You cannot use the addition `TRANSPORTING`.

    ``` abap
    READ TABLE itab REFERENCE INTO dref ...

    READ TABLE itab REFERENCE INTO DATA(dref_inl) ...
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

### Reading a Single Line by Index

The following example shows `READ TABLE` statements to read a single line from an internal table by specifying the index. You can use the addition `USING KEY` to specify a table key and thus explicitly determine the table index to use. If the table has a sorted secondary
key, the addition can be specified and the line to be read is then determined from its secondary table index. If the primary table key is
specified by its name `primary_key`, the table must be an index table, and the behavior is the same as if `USING KEY` was
not specified.
Note that the examples only show reading into a work area. Other targets are possible as shown above. 
``` abap
"Primary table index is used by default
READ TABLE itab INTO wa INDEX i.

"Primary table index is used, primary key's default name specified explicitly 
READ TABLE itab INTO wa INDEX i USING KEY primary_key.

"Secondary table index is used (assuming a secondary table key sec_key exists) 
READ TABLE itab INTO wa INDEX i USING KEY sec_key.

"Note: You can also use alias names for the keys if specified.
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

DATA(line2) = VALUE #( itab[ 7 ] DEFAULT itab[ 1 ]  ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Reading a Single Line Using Table Keys

Lines can be read by explicitly specifying the table keys or the alias names, if any.
```abap
"Example internal table with primary and secondary table key and alias names
"Assumption: All components are of type i

DATA it TYPE SORTED TABLE OF struc
  WITH NON-UNIQUE KEY primary_key ALIAS pk COMPONENTS a b
  WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS c d.

"Table expressions

"Key must be fully specified
line = it[ KEY primary_key COMPONENTS a = 1 b = 2 ].

"The addition COMPONENTS is optional; same as above
line = it[ KEY primary_key a = 1 b = 2 ].

"Primary key alias
line = it[ KEY pk a = 1 b = 2 ].

"Secondary table key
line = it[ KEY sec_key c = 3 d = 4 ].

"Secondary table key alias
line = it[ KEY sk c = 3 d = 4 ].

"READ TABLE statements
"Primary table key
READ TABLE it INTO wa WITH TABLE KEY primary_key COMPONENTS a = 1 b = 2.

"Alias
READ TABLE it INTO wa WITH TABLE KEY pk COMPONENTS a = 1 b = 2.

"Secondary table key
READ TABLE it INTO wa WITH TABLE KEY sec_key COMPONENTS c = 3 d = 4.

"Alias
READ TABLE it INTO wa WITH TABLE KEY sk COMPONENTS c = 3 d = 4.

"Reading a line based on keys specified in a work area
"Work area containing primary and secondary table key values; the line type
"must be compatible to the internal table
DATA(pr_keys) = VALUE struc( a = 1 b = 2 ).

DATA(sec_keys) = VALUE struc( c = 3 d = 4 ).

READ TABLE it FROM pr_keys INTO wa.

"If USING KEY is not specified, the primary table key is used.
"If it is used, the specified table key is used.
READ TABLE it FROM pr_keys USING KEY primary_key INTO wa.

READ TABLE it FROM sec_keys USING KEY sec_key INTO wa.

"Alias
READ TABLE it FROM sec_keys USING KEY sk INTO wa.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Reading a Single Line Using a Free Key

The specified components used as keys need not be part of a table key.
``` abap
line = it[ b = 2 ].

"Note: Table keys are specified with the ... WITH TABLE KEY ... addition, 
"free keys with ... WITH KEY ....
READ TABLE it INTO wa WITH KEY b = 2.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Examples of Addressing Individual Components of Read Lines

When reading single lines in general, you can also address individual
components of the line using the [component
selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomponent_selector_glosry.htm "Glossary Entry")
`-` (or the [object component selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_component_select_glosry.htm) `->` or the [dereferencing
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm "Glossary Entry")
`->*` in the case of data reference variables).

You can also use [`ASSIGN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapassign.htm) statements to assign components (and more) to [field symbols](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm). See the [Example: Exploring READ TABLE Statements and Table Expressions](#example-exploring-read-table-statements-and-table-expressions) below.

``` abap
DATA(comp1) = it[ b = 2 ]-c.

READ TABLE it INTO DATA(wa) WITH KEY b = 2.
DATA(comp2) = wa-c.

READ TABLE it ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY b = 2.
DATA(comp3) = <fs>-c.

READ TABLE it REFERENCE INTO DATA(dref) WITH KEY b = 2.
DATA(comp4) = dref->c.

"It is also possible to specify the dereferencing operator
"with the component selector.
DATA(comp5) = dref->*-c.

"Note: When using field symbols and data reference variables as target areas, 
"and you modify their content (the examples above only show reads), you can directly 
"edit the line in the internal table (because the field symbols/data reference 
"variable points to the line). This does not apply to work areas that creates 
"local copies. However, you can modify the content of the work area and then, 
"for example, use a MODIFY statement to edit the line in the internal table as 
"shown further down. 
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Excursions with READ TABLE Statements

#### System Field Setting in READ TABLE Statements
- For example, for checking if a line is found (`sy-subrc`) and stored in the target area, and what the index of the line is (`sy-tabix`). 
- Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapread_table.htm)

```abap
TYPES: BEGIN OF demo_struc,
          comp1 TYPE i,
          comp2 TYPE c LENGTH 3,
        END OF demo_struc,
        ty_tab_demo TYPE TABLE OF demo_struc WITH EMPTY KEY.

DATA(it) = VALUE ty_tab_demo( ( comp1 = 1 comp2 = `abc` )
                              ( comp1 = 2 comp2 = `def` )
                              ( comp1 = 1 comp2 = `ghi` ) ).

READ TABLE it INTO DATA(wa) WITH KEY comp2 = `def`.

IF sy-subrc = 0.
  ... "line found (which is the case in the example)
ELSE.
  ... "line not found
ENDIF.

ASSERT sy-tabix = 2.

READ TABLE it INTO wa WITH KEY comp2 = `xyz`.
ASSERT sy-subrc = 4.
ASSERT sy-tabix = 0.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### COMPARING and TRANSPORTING Additions: Comparing Fields and Specifying Fields for Transport

`... TRANSPORTING NO FIELDS`: It is only checked whether the line exists. No target area is specified. As mentioned above, the system fields are filled. 

```abap
DATA(stringtab) = VALUE string_table( ( `abc` ) ( `def` ) ( `ghi` ) ).

READ TABLE stringtab WITH KEY table_line = `def` TRANSPORTING NO FIELDS.
ASSERT sy-subrc = 0.
ASSERT sy-tabix = 2.

READ TABLE stringtab WITH KEY table_line = `xyz` TRANSPORTING NO FIELDS.
ASSERT sy-subrc = 4.
ASSERT sy-tabix = 0.
```

`... TRANSPORTING ...`: Specifying fields to be transported. The addition cannot be used with the `ASSIGNING`
and `REFERENCE` additions.

```abap
TYPES: BEGIN OF demo_struc_tr,
          comp1 TYPE i,
          comp2 TYPE c LENGTH 3,
        END OF demo_struc_tr,
        ty_tab_demo_tr TYPE TABLE OF demo_struc_tr WITH EMPTY KEY.

DATA(it_tr) = VALUE ty_tab_demo_tr( ( comp1 = 1 comp2 = `abc` )
                                    ( comp1 = 2 comp2 = `def` ) ).

READ TABLE it_tr INTO DATA(line_tr) INDEX 1 TRANSPORTING comp2.
ASSERT line_tr-comp1 IS INITIAL.

"ALL FIELDS addition is available, explicitly stating that all fields
"should be transported.
READ TABLE it_tr INTO line_tr INDEX 1 TRANSPORTING ALL FIELDS.
```

`... COMPARING ...`:
- Can be used together with and in front of `TRANSPORTING ...`
- Compares specified components
- The `COMPARING ALL FIELDS` compares all components, `COMPARING NO FIELDS` compares no components
- Setting of `sy-subrc`: 0 is set if the content of compared components is identical,
  otherwise it is 2. Found lines are nevertheless assigned independently of the comparison.

```abap
TYPES: BEGIN OF demo_struc_compare,
          comp1 TYPE i,
          comp2 TYPE c LENGTH 3,
          comp3 TYPE string,
        END OF demo_struc_compare,
        ty_tab_demo_compare TYPE TABLE OF demo_struc_compare WITH EMPTY KEY.

DATA(it_compare) = VALUE ty_tab_demo_compare( ( comp1 = 1 comp2 = 'abc' comp3 = `zzz` )
                                              ( comp1 = 2 comp2 = 'def' comp3 = `yyy` )
                                              ( comp1 = 2 comp2 = 'ghi' comp3 = `yyy` ) ).

DO 3 TIMES.
  DATA(line_compare) = VALUE demo_struc_compare( comp1 = 1 ).
  READ TABLE it_compare INTO line_compare INDEX sy-index COMPARING comp1.
  CASE sy-index.
    WHEN 1.
      ASSERT sy-subrc = 0.
    WHEN 2.
      ASSERT sy-subrc = 2.
    WHEN 3.
      ASSERT sy-subrc = 2.
  ENDCASE.

  line_compare = VALUE demo_struc_compare( comp3 = `yyy` ).
  READ TABLE it_compare INTO line_compare INDEX sy-index COMPARING comp3 TRANSPORTING comp2 comp3.

  CASE sy-index.
    WHEN 1.
      ASSERT sy-subrc = 2.
      ASSERT line_compare-comp1 IS INITIAL.
    WHEN 2.
      ASSERT sy-subrc = 0.
      ASSERT line_compare-comp1 IS INITIAL.
    WHEN 3.
      ASSERT sy-subrc = 0.
      ASSERT line_compare-comp1 IS INITIAL.
  ENDCASE.
ENDDO.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### CASTING and ELSE UNASSIGN Additions when Specifying Field Symbols as Target Areas

```abap
TYPES c3 TYPE c LENGTH 3.
TYPES ty_tab_casting TYPE TABLE OF c3 WITH EMPTY KEY.
DATA(it_casting) = VALUE ty_tab_casting( ( 'abc' ) ( 'def' ) ).

"Field symbol created inline (i.e. with the generic type 'data' implicitly)
"In this case, the CASTING and ELSE UNASSIGN additions are not available.
READ TABLE it_casting ASSIGNING FIELD-SYMBOL(<a>) INDEX 1.

"******* CASTING addition *******
"To use the addition, the field symbol must be either completely typed, or
"typed with one of the generic built-in ABAP types c, n, p, or x.
TYPES c2 TYPE c LENGTH 2.
FIELD-SYMBOLS <b> TYPE c2.

READ TABLE it_casting ASSIGNING <b> CASTING INDEX 2.
ASSERT <b> = 'de'.

"******* ELSE UNASSIGN addition *******
"The field symbol is unassigned if no table line is found. The addition
"can be used together with the CASTING addition.
"The following example loops 3 times across an internal table that has
"two lines. The sy-index value is used as the index value of the READ TABLE
"statement. The example demonstrates that when a line is not found, the field
"symbol is unassigned. In case of the first READ TABLE statement that does not
"specify ELSE UNASSIGN, the field symbol remains assigned.

DATA string_a TYPE string.
FIELD-SYMBOLS <c> TYPE c2.
DO 3 TIMES.
  READ TABLE it_casting ASSIGNING FIELD-SYMBOL(<d>) INDEX sy-index.
  READ TABLE it_casting ASSIGNING FIELD-SYMBOL(<e>) ELSE UNASSIGN INDEX sy-index.
  READ TABLE it_casting ASSIGNING <c> CASTING ELSE UNASSIGN INDEX sy-index.
  IF sy-index = 3.
    ASSERT <d> = `def`.
    ASSERT <e> IS NOT ASSIGNED.
    ASSERT <c> IS NOT ASSIGNED.
  ENDIF.
ENDDO.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### BINARY SEARCH Addition: Optimized Read Access When Specifying Free Keys

You may stumble on the `BINARY SEARCH` addition to `READ TABLE` statements for an optimized access. However, it is recommended to use sorted tables or secondary table keys for an optimized access. Find information and an example in the expandable section below.

```abap
READ TABLE itab WITH KEY ... BINARY SEARCH ...
```


<details>
  <summary>Expand to view more information and a code snippet</summary>
  <!-- -->

- `READ TABLE` without `BINARY SEARCH`: Table is accessed linearly
- `READ TABLE` with `BINARY SEARCH`: Table is accessed using a binary search 
  - Using the `BINARY SEARCH` addition is particularly more efficient for larger tables when accessing data often. 
  - The table must be sorted in ascending order based on the keys being searched. 
  - `BINARY SEARCH` is suitable for standard tables that do not have a secondary key defined and when you need to make multiple read accesses to the table (however, note the costs of a previous sorting)  
  - `BINARY SEARCH` can only be used with index tables and not with hashed tables. If the table is sorted and the read access uses a free key, the addition can only be applied when the initial part of the table key is specified. I.e. if key components are `a`, `b`, and `c`, the addition can be used by specifying `a` alone, `a` and `b`, or `a`, `b`, and `c`. However, it is just that the syntax "works", the `BINARY SEARCH` specifiation has no effect and is redundant. Syntactically not possible with `BINARY SEARCH` (for example): `b` and `c` without `a`, or any other non-key component (because it cannot be sorted according to the non-key component). 
- Depending on the number of times you need to access the internal table, it is recommended to work with sorted tables or tables with secondary keys. If you only need to read one or a few data sets, consider the administrative costs of setting up the index.
- Note: The `BINARY SEARCH` addition is not available for table expressions. If `KEY ...` is specified, an optimized search is performed by default. There are no performance differences between using the `READ TABLE` statement and table expressions.

To try it out the following example, create a demo class named `zcl_some_class` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.

The example includes multiple reads on standard internal tables using a `READ TABLE` statement ...
- without `BINARY SEARCH`. 
- with `BINARY SEARCH` and a previous `SORT` statement.
- with a secondary table key whose components correspond to free key used in the previous statements.
The runtime of the reads is determined and stored in internal tables. The read operations are repeated several times to have a more accurate runtime evaluation. The fastest time is output. 
The example is intended to demonstrate the performance gain using the `BINARY SEARCH` addition (and also using a secondary table key).

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
    "Line type and internal table declarations
    TYPES: BEGIN OF demo_struc,
             idx TYPE i,
             str TYPE string,
             num TYPE i,
           END OF demo_struc.

    "Tables with empty primary table key
    DATA itab_std1 TYPE STANDARD TABLE OF demo_struc WITH EMPTY KEY.
    DATA itab_std2 LIKE itab_std1.
    "Table with empty primary table key, secondary table key specified
    DATA itab_sec TYPE STANDARD TABLE OF demo_struc
                 WITH EMPTY KEY
                 WITH NON-UNIQUE SORTED KEY sk COMPONENTS str num.
    DATA num_of_table_lines TYPE i VALUE 5000.
    DATA num_of_repetitions TYPE i VALUE 10.
    DATA num_of_reads TYPE i VALUE 3000.

    "Populating internal tables
    DO num_of_table_lines TIMES.
      INSERT VALUE #( idx = sy-index
                      str = |INDEX{ sy-index }|
                      num = sy-index ) INTO TABLE itab_std1.
    ENDDO.
    itab_std2 = itab_std1.
    itab_sec = itab_std1.

    DATA no_binary_search TYPE TABLE OF decfloat34 WITH EMPTY KEY.
    DATA with_sort_and_binary_search TYPE TABLE OF decfloat34 WITH EMPTY KEY.
    DATA with_secondary_key TYPE TABLE OF decfloat34 WITH EMPTY KEY.

    "Repeating the reads several times for a more accurate result

    DO num_of_repetitions TIMES.
      "---- Reading without the BINARY SEARCH addition ----
      DATA(ts1) = utclong_current( ).
      DO num_of_reads TIMES.
        READ TABLE itab_std1 WITH KEY str = `INDEX` && sy-index num = sy-index TRANSPORTING NO FIELDS.
      ENDDO.
      DATA(ts2) = utclong_current( ).
      cl_abap_utclong=>diff( EXPORTING high     = ts2
                                       low      = ts1
                              IMPORTING seconds = DATA(seconds) ).
      APPEND seconds TO no_binary_search.

      "---- Reading with the BINARY SEARCH addition ----
      ts1 = utclong_current( ).
      "Sorting the internal table when using BINARY SEARCH
      "In this simple example, the internal table is populated by having the free key components
      "to be searched in ascending order anyway. This is to emphasize the requirement to
      "sort the (standard) internal table when using BINARY SEARCH. Here, the SORT statement
      "is counted to the runtime.
      SORT itab_std2 BY str num.

      DO num_of_reads TIMES.
        READ TABLE itab_std2 WITH KEY str = `INDEX` && sy-index num = sy-index BINARY SEARCH TRANSPORTING NO FIELDS.
      ENDDO.
      ts2 = utclong_current( ).
      cl_abap_utclong=>diff( EXPORTING high     = ts2
                                       low      = ts1
                             IMPORTING seconds = seconds ).
      APPEND seconds TO with_sort_and_binary_search.

      "---- Excursion: Reading with READ TABLE using a secondary table key ----
      ts1 = utclong_current( ).
      DO num_of_reads TIMES.
        READ TABLE itab_sec WITH TABLE KEY sk COMPONENTS str = `INDEX` && sy-index num = sy-index TRANSPORTING NO FIELDS.
      ENDDO.
      ts2 = utclong_current( ).
      cl_abap_utclong=>diff( EXPORTING high     = ts2
                                       low      = ts1
                             IMPORTING seconds = seconds ).
      APPEND seconds TO with_secondary_key.
    ENDDO.

    SORT no_binary_search ASCENDING BY table_line.
    SORT with_sort_and_binary_search ASCENDING BY table_line.
    SORT with_secondary_key ASCENDING BY table_line.

    out->write( |Number of read repetitions: { num_of_repetitions }| ).
    out->write( |Number of reads per table: { num_of_reads }\n| ).
    out->write( `Fastest run of reads using READ TABLE with a free key, without the BINARY SEARCH addition:` ).
    out->write( no_binary_search[ 1 ] ).
    out->write( repeat( val = `-` occ = 70 ) ).
    out->write( `Fastest run of reads using SORT, and READ TABLE with a free key, and the BINARY SEARCH addition:` ).
    out->write( with_sort_and_binary_search[ 1 ] ).
    out->write( repeat( val = `-` occ = 70 ) ).
    out->write( `Fastest run of reads using READ TABLE and a secondary table key:` ).
    out->write( with_secondary_key[ 1 ] ).
  ENDMETHOD.
ENDCLASS.
```

</details>


### Example: Exploring READ TABLE Statements and Table Expressions

Expand the following collapsible section to view the code of an example. To try it out, create a demo class named `zcl_some_class` and paste the code into it. After activation, choose *F9* in ADT to execute the class. 
The example is not set up to display output in the console. You may want to set a break point at the first position possible and walk through the example in the debugger. This will allow you to double-click on data objects and observe how the different statements affect their contents.

<details>
  <summary>Expand to view the code</summary>
  <!-- -->

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

    "Creating and populating an internal table
    TYPES: BEGIN OF st_h,
             a TYPE i,
             b TYPE c LENGTH 2,
             c TYPE string,
           END OF st_h,
           ty_tab_f TYPE SORTED TABLE OF st_h WITH UNIQUE KEY a WITH NON-UNIQUE SORTED KEY sk COMPONENTS b.

    DATA(it_j) = VALUE ty_tab_f( ( a = 1 b = 'zz' c = `B` )
                                 ( a = 2 b = 'xx' c = `D` )
                                 ( a = 3 b = 'yy' c = `F` ) ).

    "******* 1) Reading a single line by index: READ TABLE statements  *******

    DATA struc_e TYPE st_h.
    "In the following example ...
    "- a work area is specified as target area.
    "- USING KEY is not specified, i.e. the primary table index is used by default.
    READ TABLE it_j INTO struc_e INDEX 2.

    "Reading into a work area that is created inline
    READ TABLE it_j INTO DATA(struc_f) INDEX 3.

    "Specifying other target areas: Field symbols and data reference variables
    "Here, the target areas are created inline
    READ TABLE it_j ASSIGNING FIELD-SYMBOL(<fs_c>) INDEX 1.
    READ TABLE it_j REFERENCE INTO DATA(dref_c) INDEX 1.

    "******* USING KEY addition *******
    "Reading by index and specifying which table index to use

    "In the following example, the primary key is specified explicitly and
    "addressed using the default name primary_key. It has the same effect
    "as the statement below because the primary table index is used by
    "default.
    READ TABLE it_j INTO struc_e INDEX 1 USING KEY primary_key.
    READ TABLE it_j INTO struc_e INDEX 1.

    "Specifying the secondary key to use the secondary table index
    READ TABLE it_j INTO struc_e INDEX 1 USING KEY sk.

    "Using alias names
    DATA it_j_alias TYPE SORTED TABLE OF st_h
     WITH UNIQUE KEY primary_key ALIAS pk COMPONENTS a
     WITH NON-UNIQUE SORTED KEY sk ALIAS sk_alias COMPONENTS b.

    it_j_alias = it_j.
    READ TABLE it_j_alias INTO struc_e INDEX 1 USING KEY pk.
    READ TABLE it_j_alias INTO struc_e INDEX 1 USING KEY sk_alias.
    "The following examples use the other key names
    READ TABLE it_j_alias INTO struc_e INDEX 1 USING KEY primary_key.
    READ TABLE it_j_alias INTO struc_e INDEX 1 USING KEY sk.

    "******* Excursion: System field setting with READ TABLE statements *******
    "Checking if a line is found and stored in the target area

    READ TABLE it_j INTO struc_e INDEX 999.
    IF sy-subrc = 0.
      ...
    ELSE.
      ... "This branch is executed in the example since the line is not found.
      ASSERT sy-tabix = 0.
    ENDIF.

    READ TABLE it_j INTO struc_e INDEX 1.
    ASSERT sy-subrc = 0.
    ASSERT sy-tabix = 1.

    "******* 2) Reading a single line by index: Table expressions *******

    "Many of the following examples show the assignment of a line
    "using a table expression that is specified on the right side
    "of an assignment.
    "The target area is declared inline on the left side of an assignment.
    DATA(struc_g) = it_j[ 1 ].

    "If a table line is not found, a catchable exception is raised. It can
    "be caught in a TRY control structure.
    TRY.
        struc_g = it_j[ 999 ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    "Table expression on the left side of an assignment
    "The following example does not work since the example is a key table.
    "it_j[ 1 ] = VALUE #( ).

    "Creating and populating a table without a key
    DATA it_j_no_key TYPE TABLE OF st_h WITH EMPTY KEY.
    it_j_no_key = VALUE #( ( a = 1 b = 'a' c = `b` )
                           ( a = 2 b = 'c' c = `d` )
                           ( a = 3 b = 'e' c = `f` ) ).

    it_j_no_key[ 1 ] = VALUE #( b = 'z' ).
    ASSERT it_j_no_key[ 1 ]-a IS INITIAL.
    ASSERT it_j_no_key[ 1 ]-b = 'z'.
    ASSERT it_j_no_key[ 1 ]-c IS INITIAL.

    it_j_no_key[ 2 ]-b = 'x'.
    it_j_no_key[ 3 ] = VALUE #( ).
    ASSERT it_j_no_key[ 3 ] IS INITIAL.

    "******* KEY addition *******
    "Reading by index and specifying which table index to use

    "Primary key's default name
    struc_g = it_j_alias[ KEY primary_key INDEX 1 ].
    "Primary key alias
    struc_g = it_j_alias[ KEY pk INDEX 1 ].
    "Secondary key
    struc_g = it_j_alias[ KEY sk INDEX 1 ].
    "Secondary key alias
    struc_g = it_j_alias[ KEY sk_alias INDEX 1 ].

    "******* Excursion: Other ways of assignments with table expressions *******
    "The examples only read by index. Reading by keys is also possible.

    "Assigning a line read by index to a field symbol
    FIELD-SYMBOLS <fs_d> TYPE st_h.
    ASSIGN it_j[ 1 ] TO <fs_d>.
    "The field symbol created inline has a generic type
    ASSIGN it_j[ 1 ] TO FIELD-SYMBOL(<fs_d_inl>).

    "Copying a table line via a table expression and embedding it in a
    "constructor expression
    struc_g = VALUE #( it_j[ 3 ] ).

    "Reading into data reference variable using the REF operator
    DATA(dref_d) = REF #( it_j[ 2 ] ).

    "******* OPTIONAL/DEFAULT additions *******
    "OPTIONAL: The default value is an initial data object with the data type
    DATA(struc_h) = VALUE #( it_j[ 999 ] OPTIONAL ).
    ASSERT struc_h = VALUE st_h( ).

    "DEFAULT: Specifying a default value for table lines not found
    "This value must be convertible to the type of the table expression.
    "It can also be a table expression.
    struc_h = VALUE #( it_j[ 999 ] DEFAULT it_j[ 1 ] ).
    struc_h = VALUE #( it_j[ 999 ] DEFAULT VALUE st_h( c = `null` ) ).

**********************************************************************

    "******* 1) Reading a single line using table keys: READ TABLE statements  *******

    "Creating and populating a demo internal table
    TYPES: BEGIN OF st_i,
             num  TYPE i,
             str  TYPE string,
             char TYPE c LENGTH 2,
           END OF st_i.

    DATA it_k TYPE SORTED TABLE OF st_i
      WITH NON-UNIQUE KEY primary_key ALIAS pk COMPONENTS num
      WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS char.

    it_k = VALUE #( ( num = 1 str = `A` char = 'zz' )
                    ( num = 2 str = `C` char = 'yy' )
                    ( num = 3 str = `E` char = 'xx' ) ).

    "The following examples use a work area as target. Other target areas are
    "possible.

    "Primary table key
    READ TABLE it_k INTO DATA(struc_i) WITH TABLE KEY primary_key COMPONENTS num = 3.
    "Primary table key alias
    READ TABLE it_k INTO struc_i WITH TABLE KEY pk COMPONENTS num = 2.
    "Secondary table key
    READ TABLE it_k INTO struc_i WITH TABLE KEY sec_key COMPONENTS char = 'xx'.
    "Secondary table key alias
    READ TABLE it_k INTO struc_i WITH TABLE KEY sk COMPONENTS char = 'yy'.

    "Reading a line based on keys specified in a work area
    "It is a work area containing primary and secondary table key values.
    "the line type must be compatible to the internal table.
    TYPES st_j LIKE LINE OF it_k.
    DATA(pr_key) = VALUE st_j( num = 1 ).
    DATA(sec_key) = VALUE st_j( char = 'yy' ).

    READ TABLE it_k FROM pr_key INTO struc_i.

    "If USING KEY is not specified, the primary table key is used by default.
    "Explicitly specifying the primary table key
    READ TABLE it_k FROM pr_key USING KEY primary_key INTO struc_i.
    "Primary table key alias
    READ TABLE it_k FROM pr_key USING KEY pk INTO struc_i.
    "Secondary table key
    READ TABLE it_k FROM sec_key USING KEY sec_key INTO struc_i.
    "Secondary table key alias
    READ TABLE it_k FROM sec_key USING KEY sk INTO struc_i.

    "******* 2) Reading a single line using table keys: Table expressions *******

    "The addition COMPONENTS is optional. Therefore, the statement below
    "has the same effect.
    "Explicitly specifying the primary table key
    struc_i = it_k[ KEY primary_key COMPONENTS num = 1 ].
    struc_i = it_k[ KEY primary_key num = 1 ].

    "Primary table key alias
    struc_i = it_k[ KEY pk num = 2 ].
    "Secondary table key
    struc_i = it_k[ KEY sec_key char = 'xx' ].
    "Secondary table key alias
    struc_i = it_k[ KEY sk char = 'zz' ].

**********************************************************************

    "Reading a single line using a free key
    "Note: Instead if READ TABLE ... WITH TABLE KEY ..., it is ... WITH KEY.

    READ TABLE it_k INTO DATA(struc_j) WITH KEY str = `A`.
    struc_j = it_k[ str = `C` ].

**********************************************************************

    "Examples for addressing individual components of read lines
    "The assertions emphasize the difference of work areas and field
    "symbols/data reference variables as target areas. Modifying the
    "contents of the field symbols/data reference variables means
    "modifying the internal table content.

    DATA(comp_a) = it_k[ str = `C` ]-num.

    READ TABLE it_k INTO DATA(struc_k) WITH KEY str = `A`.
    struc_k-num = 123.
    ASSERT NOT it_k[ str = `C` ]-num = 123.
    DATA(comp_b) = struc_k-num.

    READ TABLE it_k ASSIGNING FIELD-SYMBOL(<fs_e>) WITH KEY str = `C`.
    "Note: The example table is a sorted table with 'num' as part of
    "a unique key. The field value cannot be modified.
    "<fs_e>-num = 123.
    <fs_e>-char = 'hi'.
    ASSERT it_k[ str = `C` ]-char = 'hi'.
    DATA(comp_c) = <fs_e>-char.

    READ TABLE it_k REFERENCE INTO DATA(dref_e) WITH KEY str = `E`.
    dref_e->char = '##'.
    ASSERT it_k[ str = `E` ]-char = '##'.
    DATA(comp_d) = dref_e->num.

    "It is also possible to specify the dereferencing operator together
    "with the component selector.
    DATA(comp_e) = dref_e->*-char.

**********************************************************************

    "Excursion: Using ASSIGN statements to assign to field symbols

    TYPES ty_tab_h TYPE TABLE OF st_i WITH EMPTY KEY.
    DATA(it_l) = VALUE ty_tab_h( ( num = 1 str = `A` char = 'zz' )
                                 ( num = 2 str = `C` char = 'yy' )
                                 ( num = 3 str = `E` char = 'xx' ) ).

    FIELD-SYMBOLS <fs_f> TYPE st_i-num.
    ASSIGN it_l[ str = `C` ]-num TO <fs_f>.
    <fs_f> = 99.
    ASSERT it_l[ str = `C` ]-num = 99.

    FIELD-SYMBOLS <fs_g> TYPE st_i-char.
    ASSIGN it_l[ str = `C` ]-char TO <fs_g>.
    <fs_g> = 'aa'.

    "Field symbol created inline
    ASSIGN <fs_g> TO FIELD-SYMBOL(<fs_h>).
    <fs_h> = 'bb'.

    READ TABLE it_l REFERENCE INTO DATA(dref_f) WITH KEY str = `E`.
    ASSIGN dref_f->char TO FIELD-SYMBOL(<fs_i>).
    <fs_i> = 'cc'.

    "The following example use the generic type any so that anything
    "can be assigned.
    FIELD-SYMBOLS <fs_j> TYPE any.

    "Assigning component
    ASSIGN it_l[ str = `C` ]-num TO <fs_j>.
    <fs_j> = 123.

    "Assigning line
    ASSIGN it_l[ str = `C` ] TO <fs_j>.
    <fs_j> = VALUE st_i( num = 789 str = `U` char = 'vv' ).

    "Assigning the entire table
    ASSIGN it_l TO <fs_j>.
    <fs_j> = VALUE ty_tab_h( ( num = 1 str = `AB` char = 'ap' ) ).

**********************************************************************

    "COMPARING / TRANSPORTING additions to READ TABLE statements
    "Comparing fields and specifying fields to be transported

    DATA(it_m) = VALUE ty_tab_h( ( num = 1 str = `Z` char = '##' )
                                 ( num = 2 str = `Y` char = 'yy' )
                                 ( num = 3 str = `X` char = '##' )
                                 ( num = 4 str = `W` char = 'ww' )
                                 ( num = 5 str = `W` char = '##' )
                                 ( num = 6 str = `V` char = '##' )
                                 ( num = 7 str = `V` char = '##' )
                                 ( num = 7 str = `V` char = '##' )
                                 ( num = 8 str = `V` char = 'vv' ) ).

    "******* TRANSPORTING NO FIELDS addition *******
    "It is only checked whether the line exists. No target area is specified.
    "The system fields sy-subrc and sy-tabix are filled. Check also the
    "line_exists and line_index functions.

    READ TABLE it_m WITH KEY str = `X` TRANSPORTING NO FIELDS.
    ASSERT sy-subrc = 0.
    DATA(sysubrc) = sy-subrc.
    ASSERT sy-tabix = 3.
    DATA(sytabix) = sy-tabix.

    READ TABLE it_m WITH KEY str = `nope` TRANSPORTING NO FIELDS.
    ASSERT sy-subrc = 4.
    ASSERT sy-tabix = 0.

    "******* TRANSPORTING ... addition *******
    "Specifying fields to be transported; cannot be used with the ASSIGNING
    "and REFERENCE additions

    READ TABLE it_m INTO DATA(struc_l) INDEX 1 TRANSPORTING num char.
    ASSERT struc_l-str IS INITIAL.

    "If ALL FIELDS is specified, all fields are assigned, which corresponds to the
    "example below.
    READ TABLE it_m INTO struc_l INDEX 1 TRANSPORTING ALL FIELDS.
    READ TABLE it_m INTO struc_l INDEX 1.

    "******* COMPARING addition *******
    "- Can be used together with and in front of TRANSPORTING ...
    "- Compares the specified components
    "- ALL FIELDS compares all components, NO FIELDS compares no components
    "- Setting of sy-subrc: 0 is set if the content of compared components is identical,
    "  otherwise it is 2. Found lines are nevertheless assigned independently of the comparison.

    "The following examples use a WHILE loop to read all table lines (sy-index represents the
    "index value of the primary table index) into a work area.
    "The work area is filled before the read for the comparison. Depending on the comparison
    "result (by checking the sy-subrc value), the lines are added to different internal tables
    "for demonstration purposes. In addition, the 'num' component value is added to a string.
    "The examples explore several syntax options.

    DATA struc_m LIKE LINE OF it_m.
    DATA it_n LIKE it_m.
    DATA it_o LIKE it_m.
    DATA nums_subrc_0 TYPE string.
    DATA nums_subrc_2 TYPE string.
    DATA(subrc) = 0.

    "Specifying ALL FIELDS
    WHILE subrc = 0.
      DATA(idx) = sy-index.
      struc_m = VALUE #( num = 7 str = `V` char = '##' ).
      READ TABLE it_m INTO struc_m INDEX idx COMPARING ALL FIELDS TRANSPORTING ALL FIELDS.
      subrc = COND #( WHEN sy-subrc = 0 THEN 0 ELSE sy-subrc ).
      IF subrc = 0.
        APPEND struc_m TO it_n.
        nums_subrc_0 &&= struc_m-num.
      ELSEIF subrc = 2.
        APPEND struc_m TO it_o.
        nums_subrc_2 &&= struc_m-num.
        subrc = 0.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.

    ASSERT nums_subrc_0 = `77`.
    ASSERT nums_subrc_2 = `1234568`.
    CLEAR: subrc, struc_m, it_n, it_o, nums_subrc_0, nums_subrc_2.

    "Specifying specific fields for the comparison and transport
    WHILE subrc = 0.
      idx = sy-index.
      struc_m = VALUE #( num = 1234 str = `NOPE` char = '##' ).
      READ TABLE it_m INTO struc_m INDEX idx COMPARING char TRANSPORTING num.
      subrc = COND #( WHEN sy-subrc = 0 THEN 0 ELSE sy-subrc ).
      IF subrc = 0.
        APPEND struc_m TO it_n.
        nums_subrc_0 &&= struc_m-num.
      ELSEIF subrc = 2.
        APPEND struc_m TO it_o.
        nums_subrc_2 &&= struc_m-num.
        subrc = 0.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.

    ASSERT nums_subrc_0 = `135677`.
    ASSERT nums_subrc_2 = `248`.
    CLEAR: subrc, struc_m, it_n, it_o, nums_subrc_0, nums_subrc_2.

    WHILE subrc = 0.
      idx = sy-index.
      struc_m = VALUE #( num = 9999 char = '##' str = `V` ).
      READ TABLE it_m INTO struc_m INDEX idx COMPARING char str TRANSPORTING num.
      subrc = COND #( WHEN sy-subrc = 0 THEN 0 ELSE sy-subrc ).
      IF subrc = 0.
        APPEND struc_m TO it_n.
        nums_subrc_0 &&= struc_m-num.
      ELSEIF subrc = 2.
        APPEND struc_m TO it_o.
        nums_subrc_2 &&= struc_m-num.
        subrc = 0.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.

    ASSERT nums_subrc_0 = `677`.
    ASSERT nums_subrc_2 = `123458`.

**********************************************************************
    "CASTING / ELSE UNASSIGN additions to READ TABLE statements
    "Additions when assigning the read result to a field symbol

    TYPES c3 TYPE c LENGTH 3.
    TYPES ty_tab_g TYPE TABLE OF c3 WITH EMPTY KEY.
    DATA(itq) = VALUE ty_tab_g( ( 'abc' ) ( 'def' ) ).

    "Field symbol created inline (i.e. with the generic type 'data' implicitly)
    "In this case, the CASTING and ELSE UNASSIGN additions are not available.
    READ TABLE itq ASSIGNING FIELD-SYMBOL(<fs_k>) INDEX 1.

    "******* CASTING addition *******
    "To use the addition, the field symbol must be either completely typed, or
    "typed with one of the generic built-in ABAP types c, n, p, or x.
    TYPES c2 TYPE c LENGTH 2.
    FIELD-SYMBOLS <fs_l> TYPE c2.

    READ TABLE itq ASSIGNING <fs_l> CASTING INDEX 2.
    ASSERT <fs_l> = 'de'.

    "******* ELSE UNASSIGN addition *******
    "The field symbol is unassigned if no table line is found. The addition
    "can be used together with the CASTING addition.
    "The following example loops 3 times across an internal table that has
    "two lines. The sy-index value is used as the index value of the READ TABLE
    "statement. The example demonstrates that when a line is not found, the field
    "symbol is unassigned. In case of the first READ TABLE statement that does not
    "specify ELSE UNASSIGN, the field symbol remains assigned.

    DATA string_a TYPE string.
    FIELD-SYMBOLS <fs_o> TYPE c2.
    DO 3 TIMES.
      READ TABLE itq ASSIGNING FIELD-SYMBOL(<fs_m>) INDEX sy-index.
      READ TABLE itq ASSIGNING FIELD-SYMBOL(<fs_n>) ELSE UNASSIGN INDEX sy-index.
      READ TABLE itq ASSIGNING <fs_o> CASTING ELSE UNASSIGN INDEX sy-index.
      IF sy-index = 3.
        ASSERT <fs_m> = `def`.
        ASSERT <fs_n> IS NOT ASSIGNED.
        ASSERT <fs_o> IS NOT ASSIGNED.
      ENDIF.
    ENDDO.
  ENDMETHOD.
ENDCLASS.
```
</details>    

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Getting Information about Internal Tables, Table Lines, Table Types

### Checking the Existence of a Line in an Internal Table

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
"Read using the key
READ TABLE it WITH KEY b = 2 TRANSPORTING NO FIELDS.

IF sy-subrc = 0.
  ...
ENDIF.

"Read using the index
READ TABLE it INDEX 1 TRANSPORTING NO FIELDS.

IF sy-subrc = 0.
  ...
ENDIF.

"Read using the key
IF line_exists( it[ b = 2 ] ).
  ...
ENDIF.

"Read using the index
IF line_exists( it[ 1 ] ).
  ...
ENDIF.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Checking the Index of a Line in an Internal Table

If you want to find out about the index of a line in an internal table, you can also make use of the `READ TABLE` statement above. If
the line is found, the system field `sy-tabix` is set to the number of the index. Otherwise, the built-in function
[`line_index( )`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenline_index_function.htm) can be used. It returns the index of the found line or 0 if the line does not exist.

``` abap
DATA(itab) = VALUE string_table( ( `aaa` ) ( `bbb` ) ).
READ TABLE itab WITH KEY table_line = `bbb` TRANSPORTING NO FIELDS.
"2
DATA(tabix) = sy-tabix.

"1
DATA(idx) = line_index( itab[ table_line = `aaa` ] ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Checking How Many Lines Exist in an Internal Table

`lines( )` is another built-in function that you can use to check how many lines exist in an internal table. It returns an integer value.

``` abap
DATA(itab) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ( `d` ) ( `e` ) ).

"5
DATA(number_of_lines) = lines( itab ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Getting Table (Type) Information at Runtime

Using [Runtime Type Identification (RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm "Glossary Entry"),
you can get type information on internal tables and table types at runtime 

For more information, see the [Dynamic Programming](06_Dynamic_Programming.md) ABAP cheat sheet.

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

- The order in which tables are iterated depends on the table category. 
  - Note the [`STEP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab_cond.htm#!ABAP_ADDITION_3@3@) addition, which is also available for other ABAP statements.  
- Index tables are looped over in ascending order by the index. 
- Hashed tables are looped in the order in which the lines were added to the table. You can also sort the table before the loop. 
- During the loop, the system field `sy-tabix` is set to the number of the currently processed table
line. This is not true for hashed tables. There, `sy-tabix` is `0`. 
- Note that if you want to work with the value of `sy-tabix`, you
should do so immediately after the `LOOP` statement to avoid possible overwriting in statements contained in the loop block.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Restricting the Area of a Table to Be Looped Over

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

"No interest in the table content; only relevant system fields are populated

"Mandatory WHERE clause
LOOP AT it TRANSPORTING NO FIELDS WHERE a < 5.
  ...
ENDLOOP.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Defining the Step Size and the Direction of Loop Passes

Find more information in the [`STEP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab_cond.htm#!ABAP_ADDITION_3@3@) topic. The addition is also available for other ABAP statements.  

``` abap
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

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Iteration Expressions

Iteration expressions with [`FOR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfor.htm) as part of certain constructor expressions allow you to create content of an internal table by evaluating one or more source tables.

```abap
TYPES ty_int_tab TYPE TABLE OF i WITH EMPTY KEY.
DATA(int_table_a) = VALUE ty_int_tab( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).
DATA int_table_b TYPE ty_int_tab.
int_table_b = VALUE #( FOR wa_b IN int_table_a ( wa_b * 2 ) ).
"Table Content: 2 / 4 / 6 / 8 / 10

"Instead of, for example, a LOOP statement as follows:
DATA int_table_c TYPE ty_int_tab.
LOOP AT int_table_a INTO DATA(wa_c).
  INSERT wa_c * 3 INTO TABLE int_table_c.
ENDLOOP.
"Table content: 3 / 6 / 9 / 12 / 15
```

The expressions are covered in the cheat sheet [Constructor Expressions](05_Constructor_Expressions.md):
- [Iteration Expressions Using FOR](05_Constructor_Expressions.md#iteration-expressions-using-for)
- Special reduction operator `REDUCE` that is based on iteration expressions: [REDUCE](05_Constructor_Expressions.md#reduce)

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Interrupting and Exiting Loops

ABAP keywords such as `CONTINUE`, `CHECK`, and `EXIT`, are available to exit and interrupt loops. Find more information in the [Program Flow Logic](13_Program_Flow_Logic.md#interrupting-and-exiting-loops) cheat sheet.

In the following example, the loop is exited using the `EXIT` statement when a certain condition is met.
```abap
DATA(str_table) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ( `d` ) ( `e` ) ( `f` ) ).
LOOP AT str_table INTO DATA(wa).
  DATA(tabix) = sy-tabix.
  IF wa = `e`.
    EXIT.
  ENDIF.
ENDLOOP.
ASSERT tabix = 5.
```    

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Operations with Internal Tables Using ABAP SQL SELECT Statements

### Internal Tables as Target Data Objects in SELECT Queries

Adding multiple lines from a database table to an internal table using
[`SELECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect.htm),
for example, based on a condition. In the case below, the internal table
is created inline. 
``` abap
SELECT FROM dbtab
  FIELDS comp1, comp2 ...
  WHERE ...
  INTO TABLE @DATA(itab_sel).
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

Combining data from multiple database tables into one internal table using an [inner
join](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninner_join_glosry.htm "Glossary Entry").
The following example uses the [`INNER JOIN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect_join.htm) addition. Note that the field list includes fields from both tables. The fields are referred to using `~`.
``` abap
SELECT db1~comp1, db1~comp2, db2~comp_abc, db2~comp_xyz ...
  FROM db1
  INNER JOIN db2 ON db1~comp1 = db2~comp1
  INTO TABLE @DATA(it_join_result).
```

Populating an internal table from a database table using
[subqueries](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubquery_glosry.htm "Glossary Entry").
The following two examples populate an internal table from a database table. In the first example, a subquery is specified in the
`WHERE` clause with the `NOT IN` addition. It checks whether a value matches a value in a set of values
specified in parentheses. The second example populates an internal table depending on data in another table. A subquery with the `EXISTS` addition is specified in
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
  FROM db1
  WHERE EXISTS ( SELECT 'X' FROM db2
                 WHERE comp1 = db1~comp1 )
  INTO TABLE @DATA(it_subquery_result2).
```

Populating an internal table from a table based on the existence of data in
another table using the [`FOR ALL ENTRIES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_all_entries.htm) addition.

> **💡 Note**<br>
> Make sure that the internal table you are reading from is not initial. Therefore, it is recommended that you use a subquery as shown above: `... ( SELECT ... FROM ... WHERE ... ) ...`.

``` abap
IF itab IS NOT INITIAL.

  SELECT dbtab~comp1, dbtab~comp2, ...
    FROM dbtab
    FOR ALL ENTRIES IN @itab
    WHERE comp1 = @itab-comp1
    INTO TABLE @DATA(it_select_result).

ENDIF.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### SELECT Queries with Internal Tables as Data Sources

- You can use internal tables as data sources in `SELECT` statements.

   ``` abap
   SELECT comp1, comp2, ...
      FROM @itab AS it_alias
      WHERE ...
      INTO TABLE @DATA(itab_sel).
   ```
- Internal tables are specified as [host variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhost_variable_glosry.htm) prefixed by `@` and provided with an alias name.
- Deep and nested structured types are not allowed. Structured types cannot include strings, reference types, or internal tables. Exception: The type `string` is allowed if it is declared using a reference to the built-in dictionary type `sstring`.
- When used as data sources in `SELECT` statements, internal tables are treated like DDIC database tables.
  - This is the case even if they are not passed to the database.
  - They are considered as client-independent tables, and the first column is not considered as a client column. 
  - The ABAP types of the columns are mapped to appropriate DDIC built-in types.
- Using `SELECT` statements with internal tables has significant advantages:
  - You can leverage the extensive functionalities that ABAP SQL provides with `SELECT` statements, such as aggregate expressions.
  - They can serve as alternatives to `READ TABLE` or `LOOP AT` statements, provided the data can be processed on the AS ABAP by the ABAP SQL engine and the SQL functionality surpasses the standard functionality of these ABAP statements.

Notes and restrictions: 

- In ABAP, database data is buffered in a [table buffer](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_buffer_glosry.htm) (internally, this happens in internal tables in the [shared memory](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenshared_memory_glosry.htm) of the ABAP server). 
- During read access, it is checked if the data is in the buffer, and if so, a read happens directly from there. If not, the data is first loaded into the buffer. 
- The [ABAP SQL engine](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_engine_glosry.htm) is involved in the read process. It processes reads and is used when tabular data is read (with a `SELECT` statement). This includes both buffered data from database tables in the table buffer and also internal tables of the current internal session. 
- Which means the ABAP SQL engine processes queries located on the AS ABAP, ABAP SQL is executed in the said buffer on the AS ABAP, not directly on the database.
- However, if a `SELECT` statement includes elements the ABAP SQL engine cannot handle in case of internal tables, the internal table data transfers to a temporary database table for query execution. 
- Yet, only the data of one internal table can be transferred to the database. Thus, if a query involves multiple internal tables, it can only be executed if the ABAP SQL engine can manage it. This means that if data from more than one internal table must be transferred to the database, the query will not function. Similarly, joins of database tables and internal tables can only specify one internal table whose data can be passed to the database.
- If the compiler detects a statement the ABAP SQL engine cannot process, a syntax warning appears. To suppress this warning, use the pragma `##itab_db_select`.
- Find more information on ... 
  - the restrictions [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_engine_restr.htm).
  - the various ABAP SQL functionalities in the ABAP Keyword Documentation and in the [ABAP SQL cheat sheet](03_ABAP_SQL.md). The following code snippets cover a selection.

The following example explores various `SELECT` queries with internal tables as data sources. To try it out, create a demo class named `zcl_some_class` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example uses objects of the ABAP cheat sheets repository and is set up to display output in the console.

Example: 

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

    "----------- Exploiting ABAP SQL functionality with internal tables -----------

    TYPES int_tab_type TYPE TABLE OF i WITH EMPTY KEY.
    DATA(itab_a) = VALUE int_tab_type( ( 1 ) ( 32 ) ( 100 ) ( -24 ) ( 17 ) ( 99 ) ).

    "SELECT query with an internal table as data source
    "The example uses an aggregate expression. It is statically
    "detected that the query cannot be processed by the ABAP SQL
    "engine. The data must be passed to the database. Consequently,
    "a syntax warning is displayed. It can be suppressed by a pragma.
    SELECT MAX( table_line ) AS max_val
      FROM @itab_a AS it
      INTO @DATA(max_a).

    "100
    SELECT MAX( table_line ) AS max_val ##itab_db_select
     FROM @itab_a AS it
     INTO @DATA(max_b).

    out->write( max_a ).
    out->write( max_b ).

    "Using the LIKE addition in the WHERE clause to extract internal table
    "entries matching a specific pattern.
    TYPES: BEGIN OF s1,
             a TYPE c LENGTH 3,
             b TYPE i,
           END OF s1,
           it_type_1 TYPE TABLE OF s1 WITH EMPTY KEY.
    DATA(itab_b) = VALUE it_type_1( ( a = 'abc' b = 1 )
                                    ( a = 'zbc' b = 2 )
                                    ( a = 'bde' b = 3 )
                                    ( a = 'yde' b = 4 ) ).

    SELECT a, b
      FROM @itab_b AS it_alias
      WHERE a LIKE '%bc'
      INTO TABLE @DATA(select_like_result).

*A      B
*abc    1
*zbc    2

    out->write( select_like_result ).

    "----------- Using a SELECT loop with an internal table as data source -----------

    TYPES: BEGIN OF s2,
             comp1 TYPE c LENGTH 2,
             comp2 TYPE i,
           END OF s2,
           it_type_2 TYPE TABLE OF s2 WITH EMPTY KEY.

    DATA(itab_c) = VALUE it_type_2( ( comp1 = 'aa' comp2 = 2 )
                                    ( comp1 = 'zz' comp2 = 9 )
                                    ( comp1 = 'dd' comp2 = 1 )
                                    ( comp1 = 'rr' comp2 = 7 )
                                    ( comp1 = 'tt' comp2 = 5 )
                                    ( comp1 = 'bb' comp2 = 6 ) ).

    DATA itab_d TYPE int_tab_type.

    "The following SELECT loop specifies an internal table as data source.
    "The loop sequence is defined by a sort order. Such a functionality is
    "not available with LOOP AT.
    SELECT comp2
           FROM @itab_c AS it
           ORDER BY comp2 DESCENDING
           INTO @DATA(wa).
      INSERT wa INTO TABLE itab_d.
    ENDSELECT.

*9
*7
*6
*5
*2
*1

    out->write( itab_d ).

    "------------------- Joins with internal tables -------------------

    TYPES: BEGIN OF s3,
             a TYPE c LENGTH 3,
             b TYPE c LENGTH 3,
             c TYPE i,
           END OF s3,
           it_type_3 TYPE TABLE OF s3 WITH EMPTY KEY.

    DATA(itab_e) = VALUE it_type_3( ( a = 'aaa' b = 'bbb' c = 1 )
                                    ( a = 'ccc' b = 'ddd' c = 1 )
                                    ( a = 'eee' b = 'fff' c = 2 ) ).

    DATA(itab_f) = VALUE it_type_3( ( a = 'ggg' b = 'hhh' c = 1 )
                                    ( a = 'iii' b = 'jjj' c = 1 )
                                    ( a = 'kkk' b = 'lll' c = 3 ) ).

    "No syntax warning. The internal tables can be processed by the
    "ABAP SQL engine.
    SELECT it_alias1~a, it_alias2~b
      FROM @itab_e AS it_alias1
      INNER JOIN @itab_f AS it_alias2 ON it_alias1~c = it_alias2~c
      INTO TABLE @DATA(itab_g).

*A      B
*aaa    hhh
*aaa    jjj
*ccc    hhh
*ccc    jjj

    out->write( itab_g ).

    "Join with a database table and an internal table

    "Preparing a demo database table and an internal table
    DELETE FROM zdemo_abap_tab1.
    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 char1 = 'aaa' )
                                                  ( key_field = 2 char1 = 'bbb' )
                                                  ( key_field = 3 char1 = 'ccc' ) ) ).

    TYPES it_type_4 TYPE TABLE OF zdemo_abap_tab1 WITH EMPTY KEY.
    DATA(itab_h) = VALUE it_type_4( ( key_field = 1 char2 = 'zzz' )
                                    ( key_field = 2 char2 = 'yyy' ) ).

    SELECT db~key_field, db~char1, it~char2
      FROM zdemo_abap_tab1 AS db
      INNER JOIN @itab_h AS it ON it~key_field = db~key_field
      INTO TABLE @DATA(itab_i).

*KEY_FIELD    CHAR1    CHAR2
*1            aaa      zzz
*2            bbb      yyy

    out->write( itab_i ).
  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


#### Excursion: Restrictions Regarding Selecting from Internal Tables 

- This excursion is intended to underscore the restrictions mentioned above and in the [documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_engine_restr.htm) in more detail when selecting from internal tables. 
- Components having deep types, such as strings, cannot be included, for example, in the `SELECT` list or `WHERE` clause. 
- Note that only those fields are checked (and sent to the database) that are actually used (as shown in the example below). 
- However, the type string is allowed if it is declared using the built-in dictionary type `sstring` (for example, a component typed with a [data element](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_element_glosry.htm) or a [CDS simple type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_simple_type_glosry.htm) that uses `sstring`).

The following example demonstrates various `SELECT` statements. A demo internal table has a component that is typed with a CDS simple type, which can be created as follows:
- In ADT, right-click your pacakage, and choose *New -> Other Repository Object* 
- Insert *type* and select *Type* under *Core Data Services*.
- Choose *Next* and provide a name (e.g. `zdemo_abap_string`) and a description.
- Choose *Finish*.
- Find more information on CDS simple types [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_simple_types.htm).

The code of the CDS simple type may look as follows: 

```abap
@EndUserText.label: 'String type'
define type zdemo_abap_string: abap.sstring( 1333 );
```

Commented code example:

```abap
"------ Internal table having a component of type string ------
"Creating and populating an internal table that includes a component that is
"typed with type string
TYPES: BEGIN OF s1,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 3,
         comp3 TYPE string,
       END OF s1,
       tab_type_1 TYPE TABLE OF s1 WITH EMPTY KEY.
DATA(itab1) = VALUE tab_type_1( ( comp1 = 1 comp2 = 'aaa' comp3 = `ABAP` ) ).

"Columns of type string cannot be used in SELECT statements that
"select from internal tables (* selects all fields). Therefore, the
"following statements are commented out.

"SELECT SINGLE * FROM @itab1 AS it WHERE comp1 = 1 INTO @DATA(res1).
"SELECT SINGLE comp1 FROM @itab1 AS it where comp3 = `ABAP` INTO @DATA(res2).
"SELECT SINGLE comp3 FROM @itab1 AS it where comp1 = 1 INTO @DATA(res3).

"However, the following statements work. No component of type string is involved.
SELECT SINGLE comp1 FROM @itab1 AS it WHERE comp2 = 'aaa' INTO @DATA(res4).
SELECT SINGLE comp1, comp2 FROM @itab1 AS it INTO @DATA(res5).

"--- Internal table having a component typed with a CDS simple type (sstring) ---
"Creating and populating an internal table that includes a component that is typed
"with a CDS simple type (sstring). Note: Built-in DDIC types such as sstring cannot
"directly be used in ABAP statements, except for typed literals.
TYPES: BEGIN OF s2,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 3,
         comp3 TYPE zdemo_abap_string,
       END OF s2,
       tab_type_2 TYPE TABLE OF s2 WITH EMPTY KEY.
DATA(itab2) = VALUE tab_type_2( ( comp1 = 1 comp2 = 'aaa' comp3 = `ABAP` ) ).

"Unlike above, the following SELECT statements are possible
SELECT SINGLE * FROM @itab2 AS it WHERE comp1 = 1 INTO @DATA(res6).
SELECT SINGLE comp1 FROM @itab2 AS it WHERE comp3 = `ABAP` INTO @DATA(res7).
SELECT SINGLE comp1, comp3 FROM @itab2 AS it WHERE comp2 = 'aaa' AND comp3 = `ABAP` INTO @DATA(res8).
"Note: `ABAP` represents a literal of type string. When specifying it here
"like it is specified above, there is an implicit conversion. The following
"example uses a typed literal.
SELECT SINGLE comp1, comp2, comp3 FROM @itab2 AS it WHERE comp3 = sstring`ABAP` INTO @DATA(res9).
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


*Sorting by primary table key*
``` abap
"Implicit sorting by primary table key and in ascending order by default
SORT itab.

"Optional additions to determine the sort order
"As mentioned above, ASCENDING is used implicitly. Here, specifying it explicitly.
SORT itab ASCENDING.
SORT itab DESCENDING.
```

The effect of sorting can have an unexpected result if you use the simple form of the statement and do not explicitly specify the keys. If an internal table has a structured line type and (perhaps inadvertently) the standard key as the primary table key, that is, all character-like and byte-like components make up the primary table key, all these components are taken into account when the table is sorted.
``` abap
"Is basically the same as it2
DATA it1 TYPE TABLE OF zdemo_abap_fli.

DATA it2 TYPE STANDARD TABLE OF zdemo_abap_fli WITH DEFAULT KEY.

"Respecting the standard key when sorting
SORT it1.
```
Plus: Suppose there are only elementary numeric components in an internal table with a structured line type. In this case, sorting has no effect because the primary table key is considered empty. This is certainly also true for tables declared with `EMPTY KEY`.

*Sorting by explicitly specifying components*

You can sort by any component of the internal table. It is also possible to specify the sort order 
(even component-wise). Explicitly specifying the components has the advantage that your code is easier to understand and you can avoid unexpected results if you accidentally use `SORT` without the `BY` addition on empty and standard table keys.

``` abap
DATA it3 TYPE TABLE OF struc WITH NON-UNIQUE KEY a.

"Sorting by primary table key a
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

dref->b = 4.

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
> - The system field `sy-subrc` is set to `0` if at least one line was changed. It is set to `4` if no lines were changed.
> - `MODIFY`, `DELETE`, and `INSERT` statements can be specified with and without the `TABLE` addition. With `TABLE` means an index access. Without `TABLE` means an access via the table key.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Deleting Internal Table Content

You can use [`DELETE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdelete_itab.htm) statements to delete single and multiple lines in internal tables. The following additions can be used: `USING KEY` (for specifying a table key), `FROM`/`TO` (for specifying row ranges), `STEP` (for specifying the step size), and `WHERE` (for specifying conditions).

``` abap
"-------------- Deleting via index --------------

"Example: The first line in the table is deleted.
DELETE it INDEX 1.

"If USING KEY is not used, INDEX can only be used with index tables.
"If doing so, it determines the line from the primary table index.
"If a secondary key is specified, the secondary table index is respected
"Example: same as above
DELETE it INDEX 1 USING KEY primary_key.

"Deleting an index range; FROM or TO alone can also be specified
DELETE it FROM 2 TO 5.

"-------------- Deleting via keys --------------

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

"---------- Deleting multiple lines based on a WHERE condition ----------

"Specifying the additions USING KEY, FROM, TO is also possible.
DELETE it WHERE a < 6.

"Excursion: Deleting in a LIKE-like fashion you may know from
"ABAP SQL statements.
"The LIKE addition is not available for the WHERE clause in DELETE
"statements for internal tables as is the case for ABAP SQL DELETE statements.
DATA(str_table) = VALUE string_table( ( `abcZ` ) ( `Zdef` ) ( `gZhi` ) 
                                      ( `Zjkl` ) ( `Zmno` ) ( `pqrZ` ) ).    

"You can, for example, use logical operators such as CP (conforms to pattern)
"All lines that begin with Z are to be deleted.
DELETE str_table WHERE table_line CP `Z*`.
"Result: abcZ / gZhi / pqrZ

"---------- Deleting the current line inside a LOOP statement ----------

"The following example illustrates deleting the current table line
"using a DELETE statement within a LOOP statement. Lines with even
"numbers are deleted.
"Note:
"- The short form of the DELETE statement always deletes the
"  current first line implicitly. It is only possible within a LOOP
"  statement and the delete operation is performed on the same internal
"  table.
"- The field symbol (or reference variable) should not be used after
"  the DELETE statement any more.
DATA itab1 TYPE TABLE OF i WITH EMPTY KEY.
itab1 = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ( 6 ) ( 7 ) ( 8 ) ( 9 ) ( 10 ) ).

LOOP AT itab1 ASSIGNING FIELD-SYMBOL(<fs>).
  IF <fs> MOD 2 = 0.
    DELETE itab1.
  ENDIF.
ENDLOOP.

*Table content:
*1
*3
*5
*7
*9

"The following, similar example (uneven numbers are deleted) uses a
"table which is looped over by specifying the addition USING KEY.
"In this case (using LOOP ... USING KEY ...), the short form of the
"DELETE statement cannot be used. Use the DELETE statement with the
"addition USING KEY loop_key to delete the current first line.
"loop_key is a predefined name to be used with DELETE and within
"loops that specify LOOP ... USING KEY .... No other key name is
"possible here.
DATA itab2 TYPE TABLE OF i WITH NON-UNIQUE KEY table_line.
itab2 = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ( 6 ) ( 7 ) ( 8 ) ( 9 ) ( 10 ) ).

LOOP AT itab2 USING KEY primary_key REFERENCE INTO DATA(dref2).
  IF dref2->* MOD 2 <> 0.
    DELETE itab2 USING KEY loop_key.
  ENDIF.
ENDLOOP.

*Table content:
*2
*4
*6
*8
*10
```

### Deleting Adjacent Duplicate Lines

`DELETE ADJACENT DUPLICATES` statements allow you to delete all adjacent lines except for the first line that have the same content in certain components. You usually need to perform some appropriate sorting before using these statements.
``` abap
"Implicitly using the primary table key
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

### Deleting the Entire Internal Table Content

The 
[`CLEAR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclear.htm)
and
[`FREE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfree_dataobject.htm)
statements allow you to delete the entire table content.

The difference between the two is in the handling of the memory space originally allocated to the table. When a table is cleared with `CLEAR`,
the content is removed, but the memory space initially requested remains
allocated. If the table is populated again later, the memory space is still
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

"The same applies to data reference variables pointing to internal tables.
it_ref = NEW #( ).
```
<p align="right"><a href="#top">⬆️ back to top</a></p>

## Grouping Internal Tables

To group internal tables, there are additions for `LOOP AT` statements.

`LOOP AT ... GROUP BY` 
- Groups the lines of internal tables based on a group key. Each group key represents a group, and the lines read are members of that group. 
- Performs a loop across these groups. 
- You can execute a nested loop across the members of each group using `LOOP AT GROUP` statements.
- The statements offer a variety of syntax options, such as the following. For more information, refer to the ABAP Keyword Documentation:

  - `ASCENDING`/`DESCENDING` additions to specify the sort order in the group loop. You can also sort beforehand with `SORT` statements. 
  - `WITHOUT MEMBERS`: This creates groups (requiring group key binding) without assigning actual component values. Access to group lines is not possible. Use this when access is not necessary and you want to enhance read performance. 
  - Specifying the group key after `GROUP BY` 
    - This can be a single data object or multiple keys within parentheses, which define a structure with specific components. 
    - In simple cases, component values of the table line can be assigned to the group key or the components of the group key. However, expressions are also possible on the right side of the group key assignments. 
    - Storing group-specific information in structured group keys.     
      - `GROUP SIZE` to count group members, e.g., `gs = GROUP SIZE`. 
      - `GROUP INDEX` to index group members, e.g., `gi = GROUP INDEX`. 
      - You can choose component names (such as `gs` and `gi` in the previous example) freely. 
      - To use these components, you need a group key binding. 
  - Group key binding 
    - Can be specified at the end of the statement using `INTO` and a data object. You can also use data references (`REFERENCE INTO`) or field symbols (`ASSIGNING ...`). 
    - If specified, the current key's group key is assigned to the specified data object (data reference, or field symbol). You can then address the group in nested loops with `LOOP AT GROUP`. 
    - Note: You can access the read result (e.g., `dobj` in `LOOP AT itab INTO dobj`), but the component values are initial when the group key binding is specified. You can address the group and its component values in nested loops with `LOOP AT GROUP`. 
    - If you do not specify the group key binding, it defaults to a representative binding. In each loop pass, the first line of the current group is assigned to the target area. You can process this *representative* further in nested loops with `LOOP AT GROUP`. With representative binding, the `sy-tabix` value is set as if the loop was specified without grouping.

`LOOP AT GROUP`: 
- Allows for a nested loop across group members. 
- Only applicable with `LOOP ... GROUP BY` statements (provided `WITHOUT MEMBERS` is not specified) 
- After using `LOOP AT GROUP`, you can specify ... 
  - the read result that serves as a representative in representative binding: `LOOP AT it INTO DATA(wa) GROUP BY ... LOOP AT GROUP wa ...`. 
  - the group key binding: `LOOP AT it INTO DATA(wa) GROUP BY ... INTO DATA(gkb). ... LOOP AT GROUP gkb ...`. 
- Additional syntax options like a `WHERE` condition and further grouping are also available.

More information:
- [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab_group_by.htm)   
- Iteration expressions can also handle table grouping (`FOR ... IN GROUP`). For example, see the [Constructor Expressions](05_Constructor_Expressions.md) cheat sheet.
- [Internal Tables: Grouping](11_Internal_Tables_Grouping.md) cheat sheet 

The example class below demonstrates internal table grouping options. To try it out, create a demo class named `zcl_some_class` and paste the following code into it. After activation, choose *F9* in ADT to execute the class. The example is designed to display results in the console.

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
    TYPES: BEGIN OF demo_struct,
             comp1 TYPE c LENGTH 1,
             comp2 TYPE i,
             comp3 TYPE abap_boolean,
             comp4 TYPE string,
           END OF demo_struct,
           tab_type TYPE TABLE OF demo_struct WITH EMPTY KEY.
    DATA str_table TYPE string_table.

    "Populating a demo internal table as the basis of the syntax example
    "Note: The example loops only use data objects as targets, not data references
    "or field symbols.
    DATA(it) = VALUE tab_type( ( comp1 = 'd' comp2 = 0 comp3 = abap_false )
                               ( comp1 = 'a' comp2 = 1 comp3 = abap_true )
                               ( comp1 = 'a' comp2 = 2 comp3 = abap_false )
                               ( comp1 = 'e' comp2 = 11 comp3 = abap_true )
                               ( comp1 = 'b' comp2 = 5 comp3 = abap_true )
                               ( comp1 = 'b' comp2 = 6 comp3 = abap_false )
                               ( comp1 = 'a' comp2 = 3 comp3 = abap_false )
                               ( comp1 = 'b' comp2 = 4 comp3 = abap_true )
                               ( comp1 = 'c' comp2 = 10 comp3 = abap_true )
                               ( comp1 = 'e' comp2 = 1 comp3 = abap_false )
                               ( comp1 = 'd' comp2 = 7 comp3 = abap_true )
                               ( comp1 = 'a' comp2 = 4 comp3 = abap_true )
                               ( comp1 = 'e' comp2 = 111 comp3 = abap_true ) ).

    "The following example (and several others below) does not specify a nested loop.
    "It does not specify a group key binding either. This means that the work area
    "contains the first line of each group, representing the group in the loop
    "(representative binding). The comp4 component is assigned the sy-tabix value,
    "which is the number of the line in the table without the grouping.
    DATA ita LIKE it.
    LOOP AT it INTO DATA(waa) GROUP BY waa-comp1.
      waa-comp4 = sy-tabix.
      APPEND waa TO ita.
    ENDLOOP.
    out->write( data = ita name = `ita` ).

    "Specifying sort order
    DATA itb LIKE it.
    LOOP AT it INTO DATA(wab) GROUP BY wab-comp1 ASCENDING.
      wab-comp4 = sy-tabix.
      APPEND wab TO itb.
    ENDLOOP.
    out->write( data = itb name = `itb` ).

    "WITHOUT MEMBERS addition; a group key binding is required
    "after WITHOUT MEMBERS
    "The group key binding is added to a string table for visualizing its
    "content.
    "Note: The component values are initial when the group key binding is
    "specified.
    LOOP AT it INTO DATA(wac) GROUP BY wac-comp1 WITHOUT MEMBERS INTO DATA(keyc).
      ASSERT wac IS INITIAL.
      APPEND keyc TO str_table.
    ENDLOOP.
    out->write( data = str_table name = `str_table` ).

    "Using a structured group key
    "The following example just assigns component values to the group key. In this case,
    "the grouping is performed with more than just one criterion as in the previous examples.
    "As a result, table lines are added to the other table in descending order based on the
    "two component values.
    DATA itd LIKE it.
    LOOP AT it INTO DATA(wad) GROUP BY ( key1 = wad-comp1 key2 = wad-comp2 ) DESCENDING.
      APPEND wad TO itd.
    ENDLOOP.
    out->write( data = itd name = `itd` ).

    "In the following example, the group is sorted in ascending order. Note that the
    "group index value uses the original position in the group index. The group key
    "binding information is added to a string table for visualizing its content.
    CLEAR str_table.
    LOOP AT it INTO DATA(wae) GROUP BY ( key = wae-comp1 gi = GROUP INDEX gs = GROUP SIZE ) ASCENDING INTO DATA(keye).      
      APPEND |Key component: '{ keye-key }', group index: '{ keye-gi }', group size: '{ keye-gs }'| TO str_table.
    ENDLOOP.
    out->write( data = str_table name = `str_table` ).

    "LOOP AT GROUP: Nested loop across group members
    "Unlike the previous example, the example uses a nested loop across the groups (the group key binding is
    "specified after LOOP AT GROUP). There, the component values of the members can be accessed.
    DATA itf LIKE it.
    LOOP AT it INTO DATA(waf) GROUP BY ( key = waf-comp1 gi = GROUP INDEX gs = GROUP SIZE ) ASCENDING INTO DATA(keyf).      
      LOOP AT GROUP keyf INTO DATA(memberf).
        APPEND VALUE #( comp1 = memberf-comp1 comp2 = memberf-comp2 comp3 = memberf-comp3
        comp4 = |Key component: '{ keyf-key }', group index: '{ keyf-gi }', group size: '{ keyf-gs }'|
        ) TO itf.
      ENDLOOP.
    ENDLOOP.
    out->write( data = itf name = `itf` ).

    "The objective of this example is to extract the line with the highest value in a particular
    "column within a group from the original table to another.
    "The example uses representative binding, i.e. the representative of the group is specified
    "in the work area, not in a group key binding.
    DATA itg LIKE it.
    LOOP AT it INTO DATA(wag) GROUP BY wag-comp1 ASCENDING.
      LOOP AT GROUP wag INTO DATA(memberg) GROUP BY memberg-comp2 DESCENDING.
        APPEND memberg TO itg.
        EXIT.
      ENDLOOP.
    ENDLOOP.
    out->write( data = itg name = `itg` ).

    "The following example is similar to the previous example, and yields the same result.
    "Here, the group key binding is specified after LOOP AT GROUP.
    DATA ith LIKE it.
    LOOP AT it INTO DATA(wah) GROUP BY wah-comp1 ASCENDING.
      LOOP AT GROUP wah INTO DATA(memberh) GROUP BY memberh-comp2 DESCENDING.
        APPEND memberh TO ith.
        EXIT.
      ENDLOOP.
    ENDLOOP.
    ASSERT itg = ith.
    out->write( data = ith name = `ith` ).

    "Additional syntax options, like specifying a WHERE condition in both nested and outer
    "loops, are possible. The example below shows that the LOOP AT GROUP statement assigns
    "the value of sy-tabix to the value that would be set for the current line in the LOOP
    "without grouping.
    DATA iti LIKE it.
    LOOP AT it INTO DATA(wai) GROUP BY wai-comp1 ASCENDING.
      LOOP AT GROUP wai INTO DATA(memberi) WHERE comp3 = abap_true.
        APPEND VALUE #( comp1 = memberi-comp1 comp2 = memberi-comp2 comp3 = memberi-comp3
        comp4 = |sy-tabix: '{ sy-tabix }'|
        ) TO iti.
      ENDLOOP.
    ENDLOOP.
    out->write( data = iti name = `iti` ).
  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions

### Improving Read Performance with Secondary Table Keys

The following example creates two demo internal tables. One without a secondary
table key and the other with a secondary table key. Consider a scenario where you
have a standard internal table without a secondary table key, and you want to add a secondary table key later to improve read performance. The tables are populated with a lot of data. Then, in a `DO` loop, many reads are performed on the internal tables. One example uses a free key for the read, the other uses a secondary table key that includes the components used for the free key search. Before and after the reads, the current timestamp is stored in variables, from which the elapsed time is calculated. There should be a significant delta of the elapsed time.

```abap
CLASS zcl_some_class DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_some_class IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    TYPES: BEGIN OF demo_struc,
             idx TYPE i,
             str TYPE string,
             num TYPE i,
           END OF demo_struc.

    DATA itab TYPE TABLE OF demo_struc WITH NON-UNIQUE KEY idx.
    DATA itab_sec TYPE TABLE OF demo_struc
                  WITH NON-UNIQUE KEY idx
                  WITH NON-UNIQUE SORTED KEY sk
                       COMPONENTS str num.
    DATA runtime_tab TYPE TABLE OF decfloat34 WITH EMPTY KEY.
    "Constant values on the basis of which the test runs are performed
    CONSTANTS: num_of_table_lines TYPE i VALUE 5000,
               num_of_repetitions TYPE i VALUE 10,
               num_of_reads       TYPE i VALUE 1000.

    "Populating demo tables
    DO num_of_table_lines TIMES.
      INSERT VALUE #( idx = sy-index
                      str = |INDEX{ sy-index }|
                      num = sy-index ) INTO TABLE itab.
    ENDDO.
    itab_sec = itab.

    "To get a meaningful result, many read iterations are performed (as defined
    "by 'num_of_reads'). These iterations are performed multiple times as indicated
    "by 'num_of_repetitions'. The current timestamp is stored before and after the
    "read iterations. The difference between these two timestamps provides the
    "duration, in seconds, of how long the reads took. This value is stored in
    "an internal table. The fastest value is taken as reference value for the
    "comparison with the value from the second internal table below.

    "Reading from an internal table using a free key
    DO num_of_repetitions TIMES.
      DATA(ts1) = utclong_current( ).
      DO num_of_reads TIMES.
        "The free key corresponds to the secondary table key specified for the
        "table in the second example.
        READ TABLE itab WITH KEY str = `INDEX` && sy-index num = sy-index TRANSPORTING NO FIELDS.
      ENDDO.
      DATA(ts2) = utclong_current( ).
      cl_abap_utclong=>diff( EXPORTING high    = ts2
                                       low     = ts1
                             IMPORTING seconds = DATA(seconds) ).
      APPEND seconds TO runtime_tab.
    ENDDO.

    SORT runtime_tab BY table_line ASCENDING.
    out->write( `Elapsed time for the reads using a free key:` ).
    DATA(fastest_free_key) = runtime_tab[ 1 ].
    out->write( fastest_free_key ).
    out->write( repeat( val = `-` occ = 70 ) ).

    CLEAR runtime_tab.

    "Reading from an internal table using the secondary table key
    DO num_of_repetitions TIMES.
      ts1 = utclong_current( ).
      DO num_of_reads TIMES.
        READ TABLE itab_sec WITH TABLE KEY sk COMPONENTS str = `INDEX` && sy-index num = sy-index TRANSPORTING NO FIELDS.
      ENDDO.
      ts2 = utclong_current( ).
      cl_abap_utclong=>diff( EXPORTING high    = ts2
                                       low     = ts1
                             IMPORTING seconds = seconds ).
      APPEND seconds TO runtime_tab.
    ENDDO.

    SORT runtime_tab BY table_line ASCENDING.
    out->write( `Elapsed time for the reads using a secondary table key:` ).
    DATA(fastest_sec_key) = runtime_tab[ 1 ].
    out->write( fastest_sec_key ).
    out->write( |\n\n| ).
    DATA(percentage) = fastest_sec_key / fastest_free_key * 100.
    out->write( |In the test runs of this example, the fastest read access with the secondary table key takes approximately | &&
    |{ percentage DECIMALS = 2 }% of the time it takes for the fastest read using a free key.| ).
  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example: Exploring Read Access Performance with Internal Tables

- The following example explores the performance of read accesses to internal tables of various kinds in a simplified and self-contained manner. 
- It primarily addresses the scenario of large internal tables that are accessed frequently. 
- Read accesses are executed using `READ TABLE` statements, which access the tables by: 
  - Index (both primary and secondary table index) 
  - Key (primary table key, secondary table key, free key)
- To try this, create a demo class named `zcl_some_class` and insert the provided code. After activation, choose *F9* in ADT to execute the class. It may take some time to finish and display the output. The example is set up to display the results of the read performance test in the console.

> **💡 Note**<br>
> - This example is used for [exploration and experimentation ⚠️](./README.md#%EF%B8%8F-disclaimer). It is solely for demonstration purposes, and it is **not** a *tool* for proper and accurate runtime and performance testing. Due to its simplified nature, the results may not be entirely accurate. However, multiple test runs should reflect the notes below. 
> - The example concentrates on a few demo internal tables, constructed using various declaration options. 
> - The purpose of this example is to underscore the significance of choosing the right table categories for your internal tables, tailored to your specific use case and the frequency of table access.

Notes on ...

... primary table key and primary table index: 
- Each internal table has a primary key, which can be custom-defined or use the standard key. In standard tables, it is also possible to use an empty key. 
  - Be aware of the implications of specifying the standard key, either intentionally or unintentionally. The standard key combines all character-like components, such as `txt` in the example below, while the remaining components are of type `i`.
  - The primary key of a table is ... 
    - a hash key in hashed tables, 
    - a sorted key in sorted tables, 
    - always non-unique in standard tables. 
- Key access is optimized for hashed and sorted tables, but not for standard tables. 
  - You cannot modify key fields in hashed and sorted tables. 
  - In standard tables, key access is not optimized regarding the primary table index (for the secondary table index, it is optimized) because the search is linear. 
- Generally, the larger the table, the slower the key access. The benefit of hash tables is that they offer constant access time, even for very large tables.
- Sorted and standard tables have a primary table index, hence their designation as index tables. 
- The primary table index assigns a unique line number to each table line. 
  - The index updates whenever a line is added or removed. 
  - Unlike key access, access time via the index does not increase linearly with table size.

... secondary table key and secondary table index:
- Secondary table keys, which can be sorted or hashed, are available for all table categories. 
- They enhance table access efficiency and performance. 
- Declaring a secondary table key generates a corresponding secondary table index. However, the index for a non-unique key does not update immediately upon adding or deleting a line. The update happens when the internal table is accessed using the secondary table key. 
- In ABAP statements, you must specify the secondary table key explicitly. Otherwise, the primary table key is used implicitly. 
- Data access using the secondary table key is always optimized, even for standard tables. Thus, even older standard tables can gain from optimized access by adding secondary table keys later, without impacting existing table-related statements. 
- However, weigh the administrative costs of secondary table keys. Use them only in scenarios where they offer substantial benefits, like large internal tables that are filled once and rarely altered. Frequent modifications can lead to regular index updates, potentially impacting performance.

... the use of table categories:
- Standard tables: 
  - Suitable for ... 
    - small, sequentially accessed tables that are often accessed by index 
    - tables where sorting is not critical. However, you can explicitly sort using `SORT` statements, especially after table population.
    - tables that are populated frequently, as there is no need to check for unique entries regarding the primary table key. 
  - Access speed: 
    - Fast: By index, optimized key access with secondary table keys, and free keys using `READ TABLE ... BINARY SEARCH` statements (Note: It is recommended to use secondary table keys for an optimized access.)
    - Slow: Primary table and free key

- Sorted tables: 
  - Suitable for ...
    - large tables requiring consistent sorted content 
    - tables frequently accessed both by index and in sequence 
  - Access is fast by index, primary table key (always optimized), secondary table key (also optimized)

- Hashed tables: 
  - Suitable for ...
    - very large tables that are filled once and rarely altered 
    - tables where key-based access is the primary method 
    - tables that do not require index access on the primary table index 
  - Access is ... 
    - fast and optimized for both primary and secondary table keys 
    - consistent for large internal tables due to a special hash algorithm

Expand the following collapsible section to view the code of the example that you can copy and paste into a demo class and run choosing *F9* in ADT. Note that, when running the class, it may take a while to complete and display output in the console.

<details>
  <summary>Expand to view the code</summary>
  <!-- -->

```abap
CLASS zcl_some_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    "Internal table to process demo tables and storing information
    TYPES: BEGIN OF info_struc,
             id         TYPE i,
             name       TYPE string,
             runtime    TYPE decfloat34,
             operation  TYPE string,
             comment    TYPE string,
             lines      TYPE i,
             accesses   TYPE i,
             table_kind TYPE abap_tablekind,
             keys       TYPE abap_table_keydescr_tab,
             itab_ref   TYPE REF TO data,
           END OF info_struc,
           info_tab_type TYPE TABLE OF info_struc WITH EMPTY KEY.
    DATA info_tab TYPE TABLE OF info_struc WITH EMPTY KEY.

    "Object reference variable to retrieve type information
    "at runtime (RTTI)
    DATA tdo TYPE REF TO cl_abap_tabledescr.

    "Demo internal tables
    "The line type is used for all demo tables.
    TYPES: BEGIN OF s,
             idx TYPE i,
             txt TYPE c LENGTH 20,
             num TYPE i,
           END OF s.

    "------------------ Standard tables ------------------
    "Empty primary table key
    DATA it_std_empty_key TYPE TABLE OF s WITH EMPTY KEY.

    "Primary table key explicitly specified (only non-unique in case of standard tables)
    DATA it_std_w_nu_pr_key TYPE TABLE OF s WITH NON-UNIQUE KEY idx.

    "Secondary table key specified, primary key not specified explicitly
    "Therefore, the standard primary table key is used. It consists of all
    "character-like components (which is the 'txt' component only in this case).
    DATA it_std_w_std_pr_key_w_sec_key TYPE TABLE OF s WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS num.
    "Note: The following declaration corresponds to the previous one. Here, the standard key
    "is specified explicitly.
    "DATA it_std_w_std_pr_key_w_sec_key TYPE TABLE OF s WITH DEFAULT KEY WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS num.

    "Empty primary table key, secondary table key specified
    DATA it_std_w_emp_pr_key_w_sec_key TYPE TABLE OF s WITH EMPTY KEY WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS num.

    "------------------ Sorted tables ------------------
    DATA it_sorted TYPE SORTED TABLE OF s WITH UNIQUE KEY idx.

    "Secondary table key specified
    DATA it_sorted_w_sec_key TYPE SORTED TABLE OF s WITH UNIQUE KEY idx WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS num.

    "------------------ Hashed tables ------------------
    DATA it_hashed TYPE HASHED TABLE OF s WITH UNIQUE KEY idx.

    "Secondary table key specified
    DATA it_hashed_w_sec_key TYPE HASHED TABLE OF s WITH UNIQUE KEY idx WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS num.

    "------------------ Method declarations ------------------
    "Populating the internal tables with demo data
    "The method is supplied with the number of lines that should be added to the demo tables. Ideally, and to
    "demonstrate a meaningful result, it is a high integer value. Furthermore, the information table is
    "prepared for output purposes. It also contains a reference to the internal table content that is processed.
    METHODS prepare_itabs IMPORTING number_of_lines TYPE i.

    "For performing the read accesses
    "The method is supplied with integer values.
    "- Number of read accesses: Ideally, and to demonstrate a meaningful result, it is a high integer value (but not
    "  higher than the number of internal table lines in the example).
    "- Number of repetitions: The higher the number of repetitions (i.e. how often the read access is performed n times),
    "  the more accurate the performance check can be. In the result, the fastest read access time is stored in the
    "  information table, which is returned.
    METHODS read_itabs IMPORTING number_of_reads       TYPE i
                                 number_of_repetitions TYPE i
                       RETURNING VALUE(read_results)   TYPE info_tab_type.

    "Helper method to check that specific components are included in the internal
    "tables since READ statements are performed using these components.
    METHODS check_example_components RETURNING VALUE(are_included) TYPE abap_boolean.
ENDCLASS.

CLASS zcl_some_class IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    prepare_itabs( number_of_lines = 5000 ).

    "Specifying a fairly high number of read accesses to the internal tables,
    "and not just one or two repetitions of the runtime checks so as to get
    "a more precise result (the fastest access is returned and displayed in the
    "output table). The higher the numbers provided, the longer the example
    "runs.
    DATA(read_result) = read_itabs( number_of_reads = 1000
                                    number_of_repetitions = 20 ).

    out->write( read_result ).

  ENDMETHOD.

  METHOD prepare_itabs.

    "Populating internal tables with demo data
    DO number_of_lines TIMES.
      INSERT VALUE #( idx = sy-index txt = |INDEX{ sy-index }| num = sy-index ) INTO TABLE it_std_empty_key.
    ENDDO.

    "Copying the content to the other internal tables having the same line type
    "to have the same data basis for the runtime checks
    it_std_w_nu_pr_key = it_std_empty_key.
    it_std_w_std_pr_key_w_sec_key  = it_std_empty_key.
    it_std_w_emp_pr_key_w_sec_key = it_std_empty_key.
    it_sorted = it_std_empty_key.
    it_sorted_w_sec_key = it_std_empty_key.
    it_hashed = it_std_empty_key.
    it_hashed_w_sec_key = it_std_empty_key.

    "Populating the information table (including references to the tables)
    info_tab = VALUE #(
    ( name = `it_std_empty_key` itab_ref = REF #( it_std_empty_key ) lines = lines( it_std_empty_key ) comment = `Standard, empty primary t.key` )
    ( name = `it_std_w_nu_pr_key` itab_ref = REF #( it_std_w_nu_pr_key ) lines = lines( it_std_w_nu_pr_key ) comment = `Standard, expl. primary t.key` )
    ( name = `it_std_w_std_pr_key_w_sec_key` itab_ref = REF #( it_std_w_std_pr_key_w_sec_key ) lines = lines( it_std_w_std_pr_key_w_sec_key ) comment = `Standard, standard primary, w. secondary t.key` )
    ( name = `it_std_w_emp_pr_key_w_sec_key` itab_ref = REF #( it_std_w_emp_pr_key_w_sec_key ) lines = lines( it_std_w_emp_pr_key_w_sec_key ) comment = `Standard, empty primary, w. secondary t.key` )
    ( name = `it_sorted` itab_ref = REF #( it_sorted ) lines = lines( it_sorted ) comment = `Sorted, primary t.key only` )
    ( name = `it_sorted_w_sec_key` itab_ref = REF #( it_sorted_w_sec_key ) lines = lines( it_sorted_w_sec_key ) comment = `Sorted, w. secondary t.key` )
    ( name = `it_hashed` itab_ref = REF #( it_hashed ) lines = lines( it_hashed ) comment = `Hashed,  primary t.key only` )
    ( name = `it_hashed_w_sec_key` itab_ref = REF #( it_hashed_w_sec_key ) lines = lines( it_hashed_w_sec_key ) comment = `Hashed, w. secondary t.key` )
    ).

  ENDMETHOD.

  METHOD read_itabs.
    FIELD-SYMBOLS <itab_idx> TYPE INDEX TABLE.
    FIELD-SYMBOLS <itab_ha> TYPE HASHED TABLE.
    FIELD-SYMBOLS <any_tab> TYPE ANY TABLE.
    DATA ts1 TYPE utclong.
    DATA ts2 TYPE utclong.
    DATA seconds TYPE decfloat34.
    DATA result TYPE info_tab_type.

    IF info_tab IS INITIAL.
      RETURN.
    ELSE.
      ASSIGN info_tab[ 1 ]-itab_ref->* TO FIELD-SYMBOL(<tab>).
      "The number of read lines should not exceed the number of read accesses.
      IF number_of_reads > lines( <tab> ).
        RETURN.
      ENDIF.
    ENDIF.

    "Repeated runtime check for a more meaningful result
    DO number_of_repetitions TIMES.
      LOOP AT info_tab REFERENCE INTO DATA(dref).
        dref->id = sy-tabix.

        "Assigning number of read accesses
        dref->accesses = number_of_reads.

        "Getting type information (RTTI)
        tdo = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( dref->itab_ref->* ) ).

        "Adding table kind information
        "S: standard
        "H: hashed
        "O: sorted
        dref->table_kind = tdo->table_kind.

        "Adding table key information
        dref->keys = tdo->get_keys( ).

        "Assigning referenced internal tables to field symbols to work
        "with all kinds of tables in the read accesses
        IF dref->table_kind = 'H'.
          ASSIGN dref->itab_ref->* TO <itab_ha>.
          ASSIGN dref->itab_ref->* TO <any_tab>.
        ELSE.
          ASSIGN dref->itab_ref->* TO <itab_idx>.
          ASSIGN dref->itab_ref->* TO <any_tab>.
        ENDIF.

        "-------------------- Read access by primary table index --------------------

        "Only index tables have a primary table index
        IF <itab_idx> IS ASSIGNED.
          dref->operation = |Read access by primary table index|.

          ts1 = utclong_current( ).
          DO number_of_reads TIMES.
            "Note: The READ TABLE statements in the statements use the
            "TRANSPORTING NO FIELDS addition to only focus on the read
            "access, not storing the content.
            READ TABLE <itab_idx> INDEX sy-index TRANSPORTING NO FIELDS.
          ENDDO.
          ts2 = utclong_current( ).
          cl_abap_utclong=>diff( EXPORTING high    = ts2
                                           low     = ts1
                                 IMPORTING seconds = seconds ).
          dref->runtime = seconds.
          APPEND dref->* TO result.
        ENDIF.

        "-------------------- Read access by secondary table index --------------------

        "Checking whether there is a secondary table key, which is indicated by an initial
        "value of the 'is_primary' field.
        DATA(has_sec_table_index) = xsdbool( line_exists( dref->keys[ is_primary = '' ] ) ).

        IF has_sec_table_index = abap_true.
          dref->operation = |Read access by secondary table index|.

          "Getting a secondary key name for the dynamic READ TABLE
          "statement below
          DATA(sec_key_name) = dref->keys[ is_primary = '' ]-name.

          ts1 = utclong_current( ).
          DO number_of_reads TIMES.
            READ TABLE <any_tab> INDEX sy-index USING KEY (sec_key_name) TRANSPORTING NO FIELDS.
          ENDDO.
          ts2 = utclong_current( ).
          cl_abap_utclong=>diff( EXPORTING high    = ts2
                                           low     = ts1
                                 IMPORTING seconds = seconds ).
          dref->runtime = seconds.
          APPEND dref->* TO result.
        ENDIF.

        "-------------------- Read access by primary table key --------------------

        "Checking whether the internal table has a primary table key
        READ TABLE dref->keys WITH KEY is_primary = 'X' REFERENCE INTO DATA(line_ref).
        IF sy-subrc = 0.
          "The example is designed for this example's demo internal tables. These
          "tables all have a specific line type. In this case, the primary table
          "key should only consist of one component and have the name 'IDX'.
          DATA(key_check_lines) = xsdbool( lines( line_ref->components ) = 1 ).
          IF key_check_lines = abap_true.
            DATA(key_check_idx) = xsdbool( line_ref->components[ 1 ]-name = 'IDX' ).
          ENDIF.
          DATA(is_pr_key_only_idx) = xsdbool( key_check_lines = abap_true AND key_check_idx = abap_true ).
        ELSE.
          is_pr_key_only_idx = abap_false.
        ENDIF.

        IF is_pr_key_only_idx = abap_true.

          dref->operation = |Read access by primary table key|.
          ts1 = utclong_current( ).
          DO number_of_reads TIMES.
            "Note: The primary table key is always accessible by the default name 'primary_key'.
            READ TABLE <any_tab> WITH TABLE KEY primary_key COMPONENTS ('IDX') = sy-index TRANSPORTING NO FIELDS.
          ENDDO.
          ts2 = utclong_current( ).
          cl_abap_utclong=>diff( EXPORTING high    = ts2
                                           low      = ts1
                                 IMPORTING seconds = seconds ).
          dref->runtime = seconds.
          APPEND dref->* TO result.
        ENDIF.

        "-------------------- Read access by secondary table key --------------------

        "Checking whether the internal table has a secondary table key
        READ TABLE dref->keys WITH KEY is_primary = '' REFERENCE INTO DATA(line_ref_sec).
        IF sy-subrc = 0.
          "The example is designed for this example's demo internal tables. These
          "tables all have a specific line type. All tables with a secondary
          "table key have one component with the name 'NUM'.
          DATA(name_sec_key) = line_ref_sec->name.
          DATA(sec_key_check_lines) = xsdbool( lines( line_ref_sec->components ) = 1 ).
          IF sec_key_check_lines = abap_true.
            DATA(sec_key_check_num) = xsdbool( line_ref_sec->components[ 1 ]-name = 'NUM' ).
          ENDIF.
          DATA(is_sec_key_only_num) = xsdbool( sec_key_check_lines = abap_true AND sec_key_check_num = abap_true ).
        ELSE.
          is_sec_key_only_num = abap_false.
        ENDIF.

        IF is_sec_key_only_num = abap_true.

          dref->operation = |Read access by secondary table key|.
          ts1 = utclong_current( ).
          DO number_of_reads TIMES.
            READ TABLE <any_tab> WITH TABLE KEY (name_sec_key) COMPONENTS ('NUM') = sy-index TRANSPORTING NO FIELDS.
          ENDDO.
          ts2 = utclong_current( ).
          cl_abap_utclong=>diff( EXPORTING high    = ts2
                                           low     = ts1
                                 IMPORTING seconds = seconds ).
          dref->runtime = seconds.
          APPEND dref->* TO result.
        ENDIF.

        "-------------------- Read access by free key --------------------

        "The free key can contain components other than the key components.
        "In this example, the line type of the internal tables consists of three
        "components. One of the components constitutes the primary table key.
        "The free key used here consists of the other two components.

        "Checking whether the internal table includes the two components
        "that constitute the free key.
        IF check_example_components( ).

          dref->operation = |Read access by free key|.
          ts1 = utclong_current( ).
          DO number_of_reads TIMES.
            READ TABLE <any_tab> WITH KEY ('TXT') = `INDEX` &&  sy-index ('NUM') = sy-index TRANSPORTING NO FIELDS.
          ENDDO.
          ts2 = utclong_current( ).
          cl_abap_utclong=>diff( EXPORTING high    = ts2
                                           low     = ts1
                                 IMPORTING seconds = seconds ).
          dref->runtime = seconds.
          APPEND dref->* TO result.
        ENDIF.

        "---------- Read access to standard tables by free key, using BINARY SEARCH ----------

        "The example is intended to compare the runtime results of standard tables
        "declared with secondary table keys vs the access using BINARY SEARCH that
        "enables optimized access. Note that it is recommended to use secondary table
        "keys for an optimized access.

        IF dref->table_kind = 'S' AND <itab_idx> IS ASSIGNED.
          IF check_example_components( ).
            dref->operation = |Read access by free key and BINARY SEARCH|.
            ts1 = utclong_current( ).
            "A prior sorting is required (which is actually not needed in this example because
            "the internal tables are populated in a sorted order).
            SORT <itab_idx> BY ('TXT') ('NUM') ASCENDING.
            DO number_of_reads TIMES.
              READ TABLE <itab_idx> WITH KEY ('TXT') = `INDEX` && sy-index ('NUM') = sy-index BINARY SEARCH TRANSPORTING NO FIELDS.
            ENDDO.
            cl_abap_utclong=>diff( EXPORTING high    = utclong_current( )
                                             low      = ts1
                                   IMPORTING seconds = seconds ).
            dref->runtime = seconds.
            APPEND dref->* TO result.
          ENDIF.
        ENDIF.

        UNASSIGN: <itab_idx>, <itab_ha>, <any_tab>.
      ENDLOOP.
    ENDDO.

    "Retrieving the fastest run from the result table and adding this run to
    "the internal table that is returned
    LOOP AT result INTO DATA(wa) GROUP BY ( key1 = wa-id key2 = wa-operation ) ASCENDING.
      LOOP AT GROUP wa INTO DATA(member) GROUP BY member-runtime ASCENDING.
        "Clearing internal table content for output purposes
        CLEAR member-itab_ref.
        APPEND member TO read_results.
        EXIT.
      ENDLOOP.
    ENDLOOP.
    SORT read_results BY runtime ASCENDING.
  ENDMETHOD.

  METHOD check_example_components.
    "Checking whether tables contain the components 'TXT' and 'NUM'
    DATA(tab_line_type) = tdo->get_table_line_type( ).
    DATA(struc_comps) = CAST cl_abap_structdescr( tab_line_type )->components.
    are_included = xsdbool( line_exists( struc_comps[ name = 'TXT' ] ) AND line_exists( struc_comps[ name = 'NUM' ] ) ).
  ENDMETHOD.

ENDCLASS.
```

</details>  


<p align="right"><a href="#top">⬆️ back to top</a></p>


### Searching and Replacing Substrings in Internal Tables with Character-Like Data Types

You can use [`FIND ... IN TABLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfind_itab.htm) statements to search for substrings in internal tables (standard tables without secondary table keys; with character-like line type) line by line. 

``` abap
DATA(str_table) = VALUE string_table( ( `aZbzZ` ) ( `cdZze` ) ( `Zzzf` ) ( `ghz` ) ).

"Finding all occurrences in a table
"Note: res_tab is of type match_result_tab 
"You can also restrict the search range in an internal table; see an example in REPLACE ... IN TABLE
FIND ALL OCCURRENCES OF `Z`
  IN TABLE str_table
  RESULTS DATA(res_tab)
  RESPECTING CASE.

"4 entries in table res_tab (tables in SUBMATCHES are initial since no regular expression is used)
"1. line: 1, offset: 1, length: 1, submatches: (initial)
"2. line: 1, offset: 4, length: 1, ...
"3. line: 2, offset: 2, length: 1, ...
"4. line: 3, offset: 0, length: 1, ...

"Finding the first occurrence in a table
"Note: res_struc, which is declared inline here, is of type match_result 
FIND FIRST OCCURRENCE OF `Z`
  IN TABLE str_table
  RESULTS DATA(res_struc)
  RESPECTING CASE.

"Entries in structure res_struc 
"line: 1, offset: 1, length: 1, submatches: (initial)

"Alternative to the statement above (storing the information in individual data objects)
FIND FIRST OCCURRENCE OF `Z`
  IN TABLE str_table
  MATCH LINE DATA(line)    "1
  MATCH OFFSET DATA(off)   "1 
  MATCH LENGTH DATA(len)   "1
  RESPECTING CASE.
```

Replacements in internal tables with [`REPLACE ... IN TABLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapreplace_itab.htm):

``` abap
DATA(str_table_original) = VALUE string_table( ( `aZbzZ` ) ( `cdZze` ) ( `Zzzf` ) ( `ghz` ) ).
DATA(str_table) = str_table_original.

"Replacing all occurrences in a table
"RESULTS addition: Storing information in an internal table of type repl_result_tab
REPLACE ALL OCCURRENCES OF `Z`
  IN TABLE str_table
  WITH `#`
  RESULTS DATA(res_table)
  RESPECTING CASE.

"str_table: a#bz# / cd#ze / #zzf / ghz
"res_table:
"LINE  OFFSET  LENGTH
"1     1       1
"1     4       1
"2     2       1
"3     0       1

str_table = str_table_original.

"Replacing the first occurrence in a table
"RESULTS addition: Storing information in a structure of type repl_result
REPLACE FIRST OCCURRENCE OF `Z`
  IN TABLE str_table
  WITH `#`
  RESULTS DATA(res_structure)
  RESPECTING CASE.

"str_table: a#bzZ / cdZze / Zzzf / ghz
"res_structure:
"LINE  OFFSET  LENGTH
"1     1       1

str_table = str_table_original.

"Restricting the search range in an internal table
REPLACE ALL OCCURRENCES OF `Z`
  IN TABLE str_table
  FROM 1 TO 2
  WITH `#`
  RESPECTING CASE.

"str_table: a#bz# / cd#ze / Zzzf / ghz

str_table = str_table_original.

"Offsets can be optionally specified (also only the offset of start or end line possible)
REPLACE ALL OCCURRENCES OF `Z`
  IN TABLE str_table
  FROM 1 OFFSET 3 TO 2 OFFSET 2
  WITH `#`
  RESPECTING CASE.

"str_table: aZbz# / cdZze / Zzzf / ghz
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Ranges Tables

- Internal tables that have the predefined columns `SIGN`, `OPTION`, `LOW`, and `HIGH` 
  - `SIGN`: type c with length 1; indicates whether each line is included (`I`) or excluded (`E`) from the result set.
  - `OPTION`: type c with length 2; specifies the selection option for the condition. It uses comparison operators such as `EQ`, `NE`, `GE`, `GT`, `LE`, `LT`, `CP`, `NP`, `BT`, `NB`. Special rules apply for certain operators such as when using `CP` or `NP`, the `LOW` and `HIGH` columns must be character values.
  - `LOW`: The lower comparison value.
  - `HIGH`: The higher comparison value.
- Declared with the `TYPE RANGE OF` addition in `DATA` and `TYPES` statements 
- Used to store range conditions that can be evaluated in expressions using the `IN` operator (each row in the table represents a separate comparison)

```abap
"Populating an integer table with values from 1 to 20
TYPES int_tab_type TYPE TABLE OF i WITH EMPTY KEY.
DATA(inttab) = VALUE int_tab_type( FOR x = 1 WHILE x <= 20 ( x ) ).

"Declaring a ranges table
DATA rangestab TYPE RANGE OF i.

"Populating a ranges table using VALUE
rangestab = VALUE #( sign   = 'I'
                     option = 'BT' ( low = 1  high = 3 )
                                   ( low = 6  high = 8 )
                                   ( low = 12 high = 15 )
                     option = 'GE' ( low = 18 ) ).

"Using a SELECT statement and the IN addition to retrieve internal table
"content based on the ranges table specifications
SELECT * FROM @inttab AS tab
    WHERE table_line IN @rangestab
    INTO TABLE @DATA(result).
"result: 1, 2, 3, 6, 7, 8, 12, 13, 14, 15, 18, 19, 20
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Comparing Content of Compatible Internal Tables 

Using the methods of the `CL_ABAP_DIFF` class, you can compare the content of two compatible index tables. 

Find ...
- more information in the class documentation and in the [ABAP Keyword Documentation]([06_Dynamic_Programming.md](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencl_abap_diff.htm)). 
- a code snippet in the [Misc ABAP Classes](./22_Misc_ABAP_Classes.md#comparing-content-of-compatible-internal-tables) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### BDEF Derived Types (ABAP EML)

In the context of [ABAP RAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenarap_glosry.htm), the operands of [ABAP EML](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_eml_glosry.htm) statements and parameters of [RAP handler methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_handler_method_glosry.htm) and [RAP saver methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_saver_method_glosry.htm) are mainly special messenger tables for passing data and receiving results or messages: [BDEF derived types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_derived_type_glosry.htm) - special ABAP types (internal tables and structures) that are tailor-made for RAP purposes.

```abap
"Example declarations
"For an EML create operation
DATA create_tab TYPE TABLE FOR CREATE entity.

"For an update operation
DATA update_tab TYPE TABLE FOR UPDATE entity.

"Type declaration using a BDEF derived type
TYPES der_typ TYPE TABLE FOR DELETE entity.
```

Find more information in the [ABAP for RAP: Entity Manipulation Language (ABAP EML)](08_EML_ABAP_for_RAP.md) ABAP cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information
Topic [Internal Tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenitab.htm) in the ABAP Keyword Documentation.

## Executable Example
[zcl_demo_abap_internal_tables](./src/zcl_demo_abap_internal_tables.clas.abap)

> **💡 Note**<br>
> - The executable example covers the following topics, among others: Creating, populating, reading from, sorting, modifying internal tables
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)