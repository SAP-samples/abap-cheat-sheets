<a name="top"></a>

# Internal Tables

- [Internal Tables](#internal-tables)
  - [Introduction](#introduction)
    - [Basic Properties of Internal Tables](#basic-properties-of-internal-tables)
    - [Table Keys (Primary, Secondary, Standard, Empty) and Table Indexes](#table-keys-primary-secondary-standard-empty-and-table-indexes)
  - [Creating Internal Tables and Types](#creating-internal-tables-and-types)
    - [Specifying Keys in Internal Table Declarations](#specifying-keys-in-internal-table-declarations)
    - [Internal Tables Based on Locally Created Line/Table Types](#internal-tables-based-on-locally-created-linetable-types)
    - [Overview of Line and Table Type Options with Internal Tables](#overview-of-line-and-table-type-options-with-internal-tables)
    - [Creating Internal Tables By Inline Declaration](#creating-internal-tables-by-inline-declaration)
  - [Populating Internal Tables](#populating-internal-tables)
    - [Copying Internal Tables](#copying-internal-tables)
    - [Using INSERT and APPEND Statements to Populate Internal Tables](#using-insert-and-append-statements-to-populate-internal-tables)
    - [Creating and Populating Internal Tables Using Constructor Expressions](#creating-and-populating-internal-tables-using-constructor-expressions)
      - [VALUE Operator](#value-operator)
      - [CORRESPONDING Operator and MOVE-CORRESPONDING Statements](#corresponding-operator-and-move-corresponding-statements)
      - [FILTER Operator](#filter-operator)
      - [NEW Operator](#new-operator)
    - [Example: Exploring Populating Internal Tables](#example-exploring-populating-internal-tables)
  - [Reading Single Lines from Internal Tables](#reading-single-lines-from-internal-tables)
    - [Determining the Target Area when Reading Single Lines in READ TABLE Statements](#determining-the-target-area-when-reading-single-lines-in-read-table-statements)
    - [Reading Single Lines by Index](#reading-single-lines-by-index)
    - [Reading Single Lines Using Table Keys](#reading-single-lines-using-table-keys)
    - [Reading Single Lines Using a Free Key](#reading-single-lines-using-a-free-key)
    - [Examples of Addressing Individual Components of Read Lines](#examples-of-addressing-individual-components-of-read-lines)
    - [Excursions with READ TABLE Statements](#excursions-with-read-table-statements)
      - [System Field Setting in READ TABLE Statements](#system-field-setting-in-read-table-statements)
      - [Specifying a WHERE Condition in READ TABLE Statements](#specifying-a-where-condition-in-read-table-statements)
      - [COMPARING and TRANSPORTING Additions: Comparing Fields and Specifying Fields for Transport](#comparing-and-transporting-additions-comparing-fields-and-specifying-fields-for-transport)
      - [CASTING and ELSE UNASSIGN Additions when Specifying Field Symbols as Target Areas](#casting-and-else-unassign-additions-when-specifying-field-symbols-as-target-areas)
      - [BINARY SEARCH Addition: Optimized Read Access When Specifying Free Keys](#binary-search-addition-optimized-read-access-when-specifying-free-keys)
    - [Example: Exploring READ TABLE Statements and Table Expressions](#example-exploring-read-table-statements-and-table-expressions)
  - [Accessing Single Table Lines via Table Expressions](#accessing-single-table-lines-via-table-expressions)
  - [Processing Multiple Internal Table Lines Sequentially](#processing-multiple-internal-table-lines-sequentially)
    - [Restricting the Area of a Table to Be Looped Over](#restricting-the-area-of-a-table-to-be-looped-over)
    - [Defining the Step Size and the Direction of Loop Passes](#defining-the-step-size-and-the-direction-of-loop-passes)
    - [Iteration Expressions](#iteration-expressions)
    - [Interrupting and Exiting Loops](#interrupting-and-exiting-loops)
    - [Inserting and Deleting Lines in Internal Tables in Loops](#inserting-and-deleting-lines-in-internal-tables-in-loops)
  - [Modifying Internal Table Content](#modifying-internal-table-content)
    - [Modifying Read Table Lines](#modifying-read-table-lines)
    - [Modifying Table Lines Using ABAP MODIFY Statements](#modifying-table-lines-using-abap-modify-statements)
  - [Deleting Internal Table Content](#deleting-internal-table-content)
    - [Deleting Adjacent Duplicate Lines](#deleting-adjacent-duplicate-lines)
    - [Deleting the Entire Internal Table Content](#deleting-the-entire-internal-table-content)
  - [Sorting Internal Tables](#sorting-internal-tables)
  - [Grouping Internal Tables](#grouping-internal-tables)
  - [Collecting Values](#collecting-values)
  - [Getting Information about Internal Tables, Table Lines, Table Types](#getting-information-about-internal-tables-table-lines-table-types)
    - [Checking the Existence of a Line in an Internal Table](#checking-the-existence-of-a-line-in-an-internal-table)
    - [Checking the Index of a Line in an Internal Table](#checking-the-index-of-a-line-in-an-internal-table)
    - [Checking How Many Lines Exist in an Internal Table](#checking-how-many-lines-exist-in-an-internal-table)
    - [Getting Table (Type) Information at Runtime](#getting-table-type-information-at-runtime)
  - [Operations with Internal Tables Using ABAP SQL SELECT Statements](#operations-with-internal-tables-using-abap-sql-select-statements)
    - [Internal Tables as Target Data Objects in SELECT Queries](#internal-tables-as-target-data-objects-in-select-queries)
    - [SELECT Queries with Internal Tables as Data Sources](#select-queries-with-internal-tables-as-data-sources)
      - [Restrictions Regarding Internal Tables as Data Sources in ABAP SQL SELECT Statements](#restrictions-regarding-internal-tables-as-data-sources-in-abap-sql-select-statements)
      - [Excursion: Joining/Merging Internal Tables into Internal Tables](#excursion-joiningmerging-internal-tables-into-internal-tables)
  - [Excursions](#excursions)
    - [Improving Read Performance with Secondary Table Keys](#improving-read-performance-with-secondary-table-keys)
    - [Example: Exploring Read Access Performance with Internal Tables](#example-exploring-read-access-performance-with-internal-tables)
    - [Generic Table Types with Formal Parameters of Methods and Field Symbols](#generic-table-types-with-formal-parameters-of-methods-and-field-symbols)
    - [Searching and Replacing Substrings in Internal Tables with Character-Like Data Types](#searching-and-replacing-substrings-in-internal-tables-with-character-like-data-types)
    - [Ranges Tables](#ranges-tables)
    - [Comparing Content of Compatible Internal Tables](#comparing-content-of-compatible-internal-tables)
    - [BDEF Derived Types (ABAP EML)](#bdef-derived-types-abap-eml)
    - [Creating Internal Tables Dynamically](#creating-internal-tables-dynamically)
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

### Basic Properties of Internal Tables

<details>
  <summary>🟢 Click to expand for more details</summary>
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

### Table Keys (Primary, Secondary, Standard, Empty) and Table Indexes

<details>
  <summary>🟢 Click to expand for more details</summary>
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

> [!NOTE]  
> The key can consist of individual key fields or the entire line of the internal table. In this case, the pseudo component `table_line` can be used to denote the primary table key. For non-structured line types, this is the only way to define the key.

**Standard key**
- The [standard key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_key_glosry.htm) is a special primary table key.
- It can be declared either explicitly or implicitly.
- Standard key of an internal table with a ...
   - structured line type: The primary table key consists of all fields with character-like and byte-like data types.
   - non-structured/elementary line type: The entire table line is the key (`table_line`).
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


> [!NOTE]  
> Primary table index
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
- The table access using secondary table keys is always optimized.
- Use cases:
  - To enable different key accesses for an internal table
  - To enable an optimized key access to internal tables (i.e. to improve read performance), even for standard tables
  - To enable index access for hashed tables
  - Standard tables, whose primary table keys cannot be unique, can be provided with a means of ensuring that unique table entries are read. 
  - Typically used ... 
    - for large internal tables (that are populated once and infrequently changed) 
    - for smaller tables, especially standard tables, for ensuring uniqueness of entries (in standard tables, the primary key cannot be unique) 
    - if predominantly fast read accesses are required. Then non-unique sorted secondary table keys can be used, especially for existing internal tables because a later change of the table declaration does not affect exsisting statements.
    - if predominantly uniqueness is required. Then unique hash secondary table keys can be used. This is also true for smaller internal tables.
 - Note that defining secondary table keys involves additional administration costs such as additional memory consumption and regarding the key update
   - Unique secondary table keys are immediately updated, whereas non-unique secondary table keys are not (only when the table is accessed using the key).
   - The use of secondary table keys should be reserved for cases where the benefits outweigh the extra costs. So, it may not be advisable to use secondary table keys for very small tables or if you very often change the content of the table.
- For more details, see the programming guidelines for secondary keys: [Secondary
    Key (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensecondary_key_guidl.htm "Guideline").

> [!NOTE]  
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

> [!NOTE]  
> - If the table category is not specified (`... TYPE TABLE OF ...`), it is automatically `... TYPE STANDARD TABLE OF ...`.
> - Using [Runtime Type Creation (RTTC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_creation_glosry.htm "Glossary Entry"), you can define and  create new internal tables and table types as [type description objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_object_glosry.htm) at runtime. For more information, see the [Dynamic Programming](06_Dynamic_Programming.md) ABAP cheat sheet. Also find a snippet on creating internal tables dynamically by specifying the type dynamically [below](#creating-internal-tables-dynamically).

The following code snippets contain various internal table declarations. It is intended to demonstrate a selection of the rich variety of possible internal tables mentioned in the previous sections, e.g. in *Table Keys in Internal Tables*.
In the examples, many of the internal tables are created using the structured type of a demo database table in the DDIC. The line type of the database table is automatically used when defining an internal table.

### Specifying Keys in Internal Table Declarations

<table>
<tr>
<td> Creating internal tables with ... </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> Standard table keys </td>
<td>

``` abap
"Standard table without explicit primary table key specification.
"Note that STANDARD is not explicitly specified.
"Implicitly, the standard key is used; all non-numeric table fields
"make up the primary table key.
DATA it1 TYPE TABLE OF zdemo_abap_flsch.

"Explicitly specifying STANDARD for a standard table.
"Explicitly specifying the standard table key. The declaration
"corresponds to it1.
DATA it2 TYPE STANDARD TABLE OF zdemo_abap_flsch WITH DEFAULT KEY.

"Hashed table with unique standard table key
DATA it3 TYPE HASHED TABLE OF zdemo_abap_flsch WITH UNIQUE DEFAULT KEY.

"Sorted table with non-unique standard table key
DATA it4 TYPE SORTED TABLE OF zdemo_abap_flsch WITH NON-UNIQUE DEFAULT KEY.
``` 

</td>
</tr>
<tr>
<td> Primary table keys </td>
<td>


``` abap
"Specifying the primary table key
"In standard tables, only a non-unique key is possible.
"The following two examples are the same. NON-UNIQUE can be ommitted but
"is implicitly added.
DATA it5 TYPE TABLE OF zdemo_abap_flsch WITH NON-UNIQUE KEY carrid.
DATA it6 TYPE TABLE OF zdemo_abap_flsch WITH KEY carrid.

"Sorted tables: both UNIQUE and NON-UNIQUE possible
DATA it7 TYPE SORTED TABLE OF zdemo_abap_flsch WITH UNIQUE KEY carrid connid.
DATA it8 TYPE SORTED TABLE OF zdemo_abap_flsch WITH NON-UNIQUE KEY carrid connid cityfrom.

"Hashed tables: UNIQUE KEY must be specified
DATA it9 TYPE HASHED TABLE OF zdemo_abap_flsch WITH UNIQUE KEY carrid.

"Explicitly specifying the predefined name primary_key and listing the components.
"The example is the same as it5 and it6.
DATA it10 TYPE TABLE OF zdemo_abap_flsch WITH KEY primary_key COMPONENTS carrid.

"The following example is the same as it8.
DATA it11 TYPE SORTED TABLE OF zdemo_abap_flsch
  WITH NON-UNIQUE KEY primary_key COMPONENTS carrid connid cityfrom.

"Specifying an alias name for a primary table key.
"Only possible for sorted/hashed tables.
DATA it12 TYPE SORTED TABLE OF zdemo_abap_flsch
  WITH NON-UNIQUE KEY primary_key
  ALIAS p1 COMPONENTS carrid connid cityfrom.

"Specifying a key that is composed of the entire line using the
"predefined table_line.
"In the example, an alias name is defined for a primary table key.
DATA it13 TYPE HASHED TABLE OF zdemo_abap_flsch
  WITH UNIQUE KEY primary_key
  ALIAS p2 COMPONENTS table_line.
``` 

</td>
</tr>

<tr>
<td> Empty keys </td>
<td>


``` abap
"Empty keys are only possible for standard tables
DATA it14 TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY.

"Empty primary table key, secondary table key specified
DATA it15 TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY
  WITH UNIQUE SORTED KEY cities COMPONENTS cityfrom cityto.

"Excursion: The inline declaration in a SELECT statement
"produces a standard table with empty key.
SELECT * FROM zdemo_abap_flsch INTO TABLE @DATA(it16).
``` 

</td>
</tr>
<tr>
<td> Secondary table keys </td>
<td>


``` abap
"The following examples demonstrate secondary table keys that are
"possible for all table categories.
DATA it17 TYPE TABLE OF zdemo_abap_flsch                    "standard table
  WITH NON-UNIQUE KEY carrid connid                         "primary table key
  WITH UNIQUE SORTED KEY cities COMPONENTS cityfrom cityto. "secondary table key

DATA it18 TYPE HASHED TABLE OF zdemo_abap_flsch               "hashed table
  WITH UNIQUE KEY carrid connid
  WITH NON-UNIQUE SORTED KEY airports COMPONENTS airpfrom airpto.

DATA it19 TYPE SORTED TABLE OF zdemo_abap_flsch              "sorted table
  WITH UNIQUE KEY carrid connid
  WITH UNIQUE HASHED KEY countries COMPONENTS countryfr countryto.

"Multiple secondary keys are possible
DATA it20 TYPE TABLE OF zdemo_abap_flsch
  WITH NON-UNIQUE KEY primary_key COMPONENTS carrid connid
  WITH NON-UNIQUE SORTED KEY cities COMPONENTS cityfrom cityto
  WITH UNIQUE HASHED KEY airports COMPONENTS airpfrom airpto.

"Alias names for secondary table keys (and also for the primary
"table key in the example)
DATA it21 TYPE SORTED TABLE OF zdemo_abap_flsch
  WITH NON-UNIQUE KEY primary_key ALIAS k1 COMPONENTS carrid connid
  WITH NON-UNIQUE SORTED KEY cities ALIAS s1 COMPONENTS cityfrom cityto
  WITH UNIQUE HASHED KEY airports ALIAS s2 COMPONENTS airpfrom airpto.

"Excursion: Example of using table keys and alias names using a LOOP AT statement.
"All of the statements below are possible.
"Note that if the secondary table key is not specified (and if the USING KEY addition is not
"used in the example), the primary table key is respected by default. So, the first statement
"specifying the primary table key explicitly is the same as not specifying it.
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

</td>
</tr>

</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Internal Tables Based on Locally Created Line/Table Types

The examples above demonstrate internal tables that are created using the structured type of a database table in the DDIC. 
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
"Internal table creation steps based on a locally declared structure
"1. Defining line type locally
TYPES: BEGIN OF ls_loc,
          key_field TYPE i,
          char1     TYPE c LENGTH 10,
          char2     TYPE c LENGTH 10,
          num1      TYPE i,
          num2      TYPE i,
        END OF ls_loc.

"2. Defining internal table types
TYPES: tt_std    TYPE TABLE OF ls_loc WITH EMPTY KEY,
        tt_sorted TYPE SORTED TABLE OF ls_loc WITH NON-UNIQUE KEY key_field,
        tt_hashed TYPE HASHED TABLE OF ls_loc WITH UNIQUE KEY key_field.

"3. Creating internal tables from locally defined table types
DATA: it22 TYPE tt_std,
      it23 TYPE tt_sorted,
      it24 TYPE tt_hashed.

"Combining data object and table type definition
DATA it25 TYPE TABLE OF ls_loc WITH NON-UNIQUE KEY key_field.

"LIKE addition
"Internal table based on an already existing internal table using LIKE.
DATA it26 LIKE it25.
"In the following example, an internal table is created containing internal tables
"of the type of itab_a6.
DATA it27 LIKE TABLE OF it25.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Overview of Line and Table Type Options with Internal Tables

This section explores various line and table type options when declaring internal tables. Examples have already been covered in the previous sections. Among the options are, for example: 
- Elementary line types based on elementary built-in ABAP types, locally declared elementary types, globally available elementary types such as DDIC data elements
- Line types based on both locally declared and globally available structured types
  - Among the globally available line types are, for example, DDIC structures, database tables and CDS objects such as CDS view entities
  - Note that internal tables can be created as [deep tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_table_glosry.htm). That is, internal tables can contain [deep](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_glosry.htm) components (references, internal tables or strings). Structures can also be components of internal tables. For more information, see the [Variants of Structures](02_Structures.md#variants-of-structures) in the *Structures* cheat sheet.
- Table types based on both locally declared and globally available table types
- References

> [!NOTE]  
> Types declared in the public visibility section of classes/interfaces are also globally visible and can be used for the creation.

<table>
<tr>
<td> Subject </td> <td> Details/Code Snippet </td>
</tr>

<tr>
<td> Elementary line types </td>
<td>


``` abap
*&---------------------------------------------------------------------*
*& Internal tables declared using elementary line types
*&---------------------------------------------------------------------*
"Note: In this case, the whole table line is the standard table key.

"Elementary built-in ABAP types
DATA it_elem_1 TYPE TABLE OF i.
DATA it_elem_2 TYPE TABLE OF string WITH EMPTY KEY.
DATA it_elem_3 TYPE TABLE OF xstring WITH EMPTY KEY.
DATA it_elem_4 TYPE TABLE OF utclong WITH EMPTY KEY.

"Note: As described above, the table key can consist of components of the
"line type or of the entire line using the pseudo component table_line. In
"case of elementary line types, table_line is the only option to refer to the
"component/entire line.
DATA it_elem_5 TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.

"Locally declared elementary types
TYPES ty_c3 TYPE c LENGTH 3.
DATA it_elem_6 TYPE TABLE OF ty_c3 WITH EMPTY KEY.

"Apart from the globally available elementary built-in ABAP types, you can
"also use elementary types from the DDIC such as data elements that are
"visible anywhere. You can also refer to CDS simple types that
"constitute elementary data types.
DATA it_elem_7 TYPE TABLE OF timestamp WITH EMPTY KEY.
DATA it_elem_8 TYPE TABLE OF timestampl WITH EMPTY KEY.

"Globally available elementary types in the public visibility section of
"classes/interfaces
"Elementary type declared in an interface
DATA it_elem_9 TYPE TABLE OF zdemo_abap_get_data_itf=>occ_rate WITH EMPTY KEY.

"Note the syntax options when referring to components. In the following
"example, a database table field is referred to using the database table name,
"the component selector and the name of a field having an elementary data type.
DATA it_elem_10 TYPE TABLE OF zdemo_abap_fli-carrid WITH EMPTY KEY.
"CDS view entity component
DATA it_elem_11 TYPE TABLE OF zdemo_abap_carr_ve-url WITH EMPTY KEY.
``` 

</td>
</tr>

<tr>
<td> Structured types </td>
<td>


``` abap
*&---------------------------------------------------------------------*
*& Internal tables declared using structured types
*&---------------------------------------------------------------------*

"Locally declared structured type
TYPES: BEGIN OF local_struct,
          comp1 TYPE i,
          comp2 TYPE string,
          comp3 TYPE c LENGTH 3,
        END OF local_struct.

DATA it_struc_1 TYPE SORTED TABLE OF local_struct WITH UNIQUE KEY comp1.

"Globally available structured types
"E.g. DDIC database tables
DATA it_struc_2 TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

"CDS objects such as ...
"CDS view entity
DATA it_struc_3 TYPE TABLE OF zdemo_abap_carr_ve WITH EMPTY KEY.
"CDS abstract entity
DATA it_struc_4 TYPE TABLE OF zdemo_abap_abstract_ent WITH EMPTY KEY.
"CDS table function
DATA it_struc_5 TYPE TABLE OF zdemo_abap_table_function WITH EMPTY KEY.

"Globally available structured types in the public visibility section of
"classes/interfaces
DATA it_struc_6 TYPE TABLE OF zcl_demo_abap_amdp=>fli_struc WITH EMPTY KEY.

"Many of the previous examples demonstrate flat line types.
"Deep line types are also possible, that is, the line type
"can include components such as strings, references, and internal
"tables.
TYPES: BEGIN OF loc_deep_struct,
          key_field TYPE i,
          char      TYPE c LENGTH 10,
          str       TYPE string,
          dref      TYPE REF TO i,
          struct    TYPE zdemo_abap_flsch,
          str_tab   TYPE string_table,
          tab       TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY,
        END OF loc_deep_struct.

DATA it_struc_7 TYPE TABLE OF loc_deep_struct WITH EMPTY KEY.
``` 

</td>
</tr>
<tr>
<td> Table types </td>
<td>


``` abap
*&---------------------------------------------------------------------*
*& Internal tables declared using table types
*&---------------------------------------------------------------------*

"Locally declared table type (based on a locally declared structured type)
TYPES: BEGIN OF loc_struct,
          comp1 TYPE i,
          comp2 TYPE string,
          comp3 TYPE c LENGTH 3,
        END OF loc_struct,
        ty_tab_1 TYPE TABLE OF loc_struct WITH EMPTY KEY.

DATA it_tab_1 TYPE ty_tab_1.

"Locally declared table type (based on a globally available structured type)
TYPES ty_tab_2 TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.
DATA it_tab_2 TYPE ty_tab_2.

"Globally available table types
"The following examples show released table types. They have an
"elementary line type.
DATA it_tab_3 TYPE string_table.
DATA it_tab_4 TYPE string_hashed_table.
DATA it_tab_5 TYPE xstring_table.

"Globally available table types in the public visibility section of
"classes/interfaces
DATA it_tab_6 TYPE zdemo_abap_get_data_itf=>carr_tab.
DATA it_tab_7 TYPE zcl_demo_abap_amdp=>flsch_tab.

"Creating an internal table based on the table type of an existing
"internal table
DATA it_tab_8 LIKE it_tab_7.
``` 

</td>
</tr>
<tr>
<td> References as line types </td>
<td>


``` abap
DATA it_ref_1 TYPE TABLE OF REF TO i.
DATA it_ref_2 TYPE TABLE OF REF TO string.
DATA it_ref_3 TYPE TABLE OF REF TO zdemo_abap_carr.

DATA it_ref_4 TYPE TABLE OF REF TO data.

"Such a table can hold any data type
it_ref_4 = VALUE #( ( NEW i( 3 ) ) "Elementary type
                    ( NEW string( `hello` ) ) "Elementary type
                    ( NEW zdemo_abap_flsch( carrid = 'XY' connid = '1234' ) ) "Structured type
                    ( NEW string_table( ( `a` ) ( `b` ) ( `c` ) ) ) "Table type
                  ).
``` 

</td>
</tr>


</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Internal Tables By Inline Declaration 

```abap
"Table declared inline in the context of an assignment
"The examples show the copying of a table including the content
"on the fly and creating the table in one step. The data type of the
"declared variable is determined by the right side.
DATA string_tab TYPE string_table.

DATA(it_a) = string_tab.
DATA(it_b) = it_a.

"Using FINAL for creating immutable variables
FINAL(it_c) = it_a.
"For example, it is not possible to modify such a table in the following position.
"Find more information in the Data Types and Data Objects cheat sheet.
"APPEND INITIAL LINE TO it_c.

"As shown below and in other cheat sheets, constructor operators
"are handy when creating internal tables in place. The following
"examples uses the VALUE operator and an internal table type. You
"may also use the NEW operator to create anonymous data objects.
DATA(it_d) = VALUE string_table( ( `aaa` )
                                 ( `bbb` ) ).

TYPES it_type TYPE TABLE OF zdemo_abap_carr with empty key.
DATA(it_e) = VALUE it_type( ( carrid = 'XY' carrname = 'XY Airlines' )
                            ( carrid = 'YZ' carrname = 'Air YZ' ) ).

"Not providing any table lines means the table is initial
"and has the same effect as the declaration of it_g.
DATA(it_f) = VALUE string_table( ).
DATA it_g TYPE string_table.

DATA(it_h) = VALUE it_type( ).
DATA it_i TYPE it_type.

"Excursion
"Table declared inline in the context of a SELECT statement;
"a prior extra declaration of an internal table is not needed.
SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(it_j).

"Instead of
DATA it_k TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.
SELECT * FROM zdemo_abap_fli INTO TABLE @it_k.

"Using FINAL
SELECT * FROM zdemo_abap_fli INTO TABLE @FINAL(it_l).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>




## Populating Internal Tables

> [!NOTE]  
> Various ABAP statements populate internal tables. The following sections cover a selection. ABAP SQL `SELECT`  statements are covered further down and in the [ABAP SQL](03_ABAP_SQL.md) cheat sheet.

### Copying Internal Tables

To copy internal table content from one table to another, you can use the assignment operator. Such an assignment (without a constructor expression) deletes the existing content in the target internal table. The example below assumes that the source and target table have compatible line types. Using inline declaration is helpful to avoid an additional internal table declaration with an appropriate type.

``` abap
itab = itab2.

DATA(itab3) = itab.
FINAL(itab4) = itab.
```

> [!NOTE]  
> - Internal tables can only be assigned to internal tables. 
> - Internal tables can be assigned to each other if their line types are [compatible](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencompatible_glosry.htm) or [convertible](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconvertible_glosry.htm).
> - An assignment can trigger an uncatchable exception if, for example, the target table is assigned a duplicate of a unique primary table key or secondary table.
> - More information: [Conversion Rules for Internal Tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_itab.htm)

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using INSERT and APPEND Statements to Populate Internal Tables

You can use the ABAP keywords
[`INSERT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinsert_itab.htm)
and [`APPEND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapappend.htm)
to add lines to internal tables.

<details>
  <summary>🟢 Click to expand for notes on using <code>APPEND</code> and <code>INSERT</code></summary>
  <!-- -->

-   `APPEND` ...
    -   always adds lines at the bottom of the internal table.
    -   is particularly suitable for standard tables where lines are managed
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
- What to use (for standard tables)? With the `INSERT` statement, you are prepared for a potential change of internal table definitions. It covers all table and key types. Consider potential issues when you change table/key types, and you use `APPEND` in your code. The ABAP cheat sheet examples often use `APPEND` statements, as standard tables are frequently used and those considerations are not relevant here. Additionally, explore constructor expressions for adding lines to internal tables.

</details>
<br>

<table>
<tr>
<td> Subject </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> Adding a line to an internal table </td>
<td>

The example shows both a structure that is created using the `VALUE` operator as well as an existing structure that is added.

<br>

``` abap
APPEND VALUE #( comp1 = a comp2 = b ... ) TO itab.
APPEND struc TO itab.

INSERT VALUE #( comp1 = a comp2 = b ... ) INTO TABLE itab.
INSERT struc INTO TABLE itab.
```

</td>
</tr>

<tr>
<td> Adding an initial line </td>
<td>

... to an internal table without providing any field values.
<br>

``` abap
APPEND INITIAL LINE TO itab.

INSERT INITIAL LINE INTO TABLE itab.
```

</td>
</tr>
<tr>
<td> Adding a line and assigning the added line to a field symbol or data reference variable </td>
<td>

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

</td>
</tr>
<tr>
<td> Adding all lines from another internal table </td>
<td>

``` abap
APPEND LINES OF itab2 TO itab.

INSERT LINES OF itab2 INTO TABLE itab.
```

</td>
</tr>
<tr>
<td> Adding multiple lines from another internal table with a specified index range </td>
<td>

- Both `FROM` and `TO` are not mandatory in one statement. it is possible to use only one of them. 
- If you use only ...
  - `FROM`, all lines up to the last table entry are respected.
  - `TO`, all lines starting with the first table entry are respected. 

<br>

``` abap
APPEND LINES OF itab2 FROM 3 TO 5 TO itab.

APPEND LINES OF itab2 FROM 3 TO itab.

APPEND LINES OF itab2 TO 7 TO itab.

INSERT LINES OF itab2 FROM 5 TO 9 INTO TABLE itab.
```


</td>
</tr>

<tr>
<td> Inserting one line represented by a structure or multiple lines from another internal table at a specific position </td>
<td>

`FROM` and `TO` can be used here, too.

<br>

``` abap
INSERT struc INTO itab2 INDEX i.

INSERT LINES OF itab2 INTO itab INDEX i.
```

</td>
</tr>

</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating and Populating Internal Tables Using Constructor Expressions
The constructor expressions can be specified in/with various positions/statements in ABAP. In most of the following snippets, simple assignments are demonstrated.

> [!NOTE]  
> The following sections cover a selection. There are more constructor expressions used in the context of internal tables (e.g. for creating internal tables). Find more details in the [Constructor Expressions](05_Constructor_Expressions.md) cheat sheet.

#### VALUE Operator
As mentioned above, table lines that are constructed inline as
arguments to the `VALUE` operator, for example, can be added to
internal tables. In the following cases, internal tables are populated
using constructor expressions in the context of
[assignments](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_glosry.htm "Glossary Entry").

<table>
<tr>
<td> Subject </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> Populating an existing internal table by assigning an
internal table that is constructed inline </td>
<td>

In the example below, the internal table is populated by assigning an
internal table that is constructed inline with the `VALUE`
operator. The inline constructed table has two lines. `line`
represents an existing structure with a compatible line type. The
other line is constructed inline.

> [!NOTE]  
> - The extra pair of parentheses represents a table line. The `#` character indicates that the line type can be derived from the context. The assignment deletes the existing content of the internal table on the left side.
> - The existing content of the internal table is deleted, and the new content, which is created in place, is added.

<br>

``` abap
itab = VALUE #( ( line )
                ( comp1 = a comp2 = b ...  ) ).
```

</td>
</tr>

<tr>
<td> Creating an internal table by inline declaration and adding lines with a constructor expression </td>
<td>

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

</td>
</tr>
<tr>
<td> <code>BASE</code> addition: Adding new lines without deleting existing content </td>
<td>

When you use the above assignments to an existing internal table (`itab = ...`), the internal table is initialized and the existing content is deleted. To add new lines without deleting existing content, use the  [`BASE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenvalue_constructor_params_itab.htm) addition.

<br>

``` abap
itab = VALUE #( BASE itab ( comp1 = a comp2 = b ... )
                          ( comp1 = c comp2 = d ... ) ).
```

</td>
</tr>
<tr>
<td> <code>LINES OF</code> addition: Adding lines of other tables </td>
<td>

... using the `LINES OF` addition to the `VALUE` operator.
Without the `BASE` addition, the existing content is deleted. It is assumed that the line type of the source table is compatible with that of the target table. You have multiple syntax options following the `LINES OF` addition, e.g. you can further determine and restrict the lines to be added using `FROM` and `TO`.
<br>

``` abap
itab = VALUE #( ( comp1 = a comp2 = b ...)
                ( comp1 = a comp2 = b ...)
                "All lines
                ( LINES OF itab2 )
                "More syntax options
                ( LINES OF itab3 FROM 2 TO 5 )
                ( LINES OF itab4 FROM 3 )
                ( LINES OF itab5 TO 7 )
                ( LINES OF itab6 STEP 2 )
                ( LINES OF itab7 USING KEY primary_key FROM 3 TO 6 )
                ... ).
```

</td>
</tr>

<tr>
<td> Iteration expressions with <code>FOR</code> </td>
<td>

Using the `VALUE` operator and iteration expressions with [`FOR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfor.htm), you can create content of an internal table by evaluating one or more source tables. The expressions are covered in the [Constructor Expressions](05_Constructor_Expressions.md) cheat sheet.
<br>

```abap
TYPES ty_int_tab TYPE TABLE OF i WITH EMPTY KEY.
DATA(int_table_a) = VALUE ty_int_tab( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).
DATA int_table_b TYPE ty_int_tab.
int_table_b = VALUE #( FOR wa_b IN int_table_a ( wa_b * 2 ) ).
"Table Content: 2 / 4 / 6 / 8 / 10
```


</td>
</tr>

</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

#### CORRESPONDING Operator and MOVE-CORRESPONDING Statements

<table>
<tr>
<td> Subject </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> Copying the content of another (incompatible) internal table </td>
<td>

... using the
[`CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expr_corresponding.htm) operator. 
- Note that the existing content is deleted.
- As an alternative to the `CORRESPONDING` operator, you can use [`MOVE-CORRESPONDING`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove-corresponding.htm) statements. 
- The operator is particularly useful for incompatible source and target types. Value assignments are made based on identical components in the source and target. As shown further down, you can also specify mapping rules using the `MAPPING` addition.
- The example assumes that the line types of the source and target table are not compatible. However, if the line types are compatible, the syntax will also work.
- Several additions are possible. They can also be combined. Check the ABAP Keyword Documentation.

<br>

``` abap
itab = CORRESPONDING #( itab3 ).

MOVE-CORRESPONDING itab3 TO itab.
```

</td>
</tr>

<tr>
<td> <code>BASE</code>/<code>KEEPING TARGET LINES</code> additions: Copying content and retaining existing content </td>
<td>

... using the `CORRESPONDING` operator. 
The `KEEPING TARGET LINES` addition of the `MOVE-CORRESPONDING` statement preserves the table content.
<br>

``` abap
itab = CORRESPONDING #( BASE ( itab ) itab3 ).

MOVE-CORRESPONDING itab3 TO itab KEEPING TARGET LINES.
```

</td>
</tr>
<tr>
<td> <code>MAPPING</code> addition: Assigning components using mapping relationships </td>
<td>

- You can use the `MAPPING` addition of the `CORRESPONDING` operator to specify components of a source table that are assigned to the components of a target table in mapping relationships. 
- For elementary components, the assignment is made according to the associated assignment rules.

<br>

``` abap
itab = CORRESPONDING #( itab3 MAPPING a = c b = d ).
```

</td>
</tr>
<tr>
<td> <code>EXCEPT</code> addition: Excluding components from the assignment </td>
<td>

... using the `EXCEPT` addition to the `CORRESPONDING` operator.
- This is particularly useful if there are identically named components in the source and target tables that are not compatible or convertible. You can avoid syntax errors or runtime errors. 
- Instead of a component list, `EXCEPT` can also be followed by `*` to exclude all components that are not mentioned in a previous mapping of components. 
- If `EXCEPT *` is used without the `MAPPING` addition, all components remain initial.
<br>

``` abap
itab = CORRESPONDING #( itab3 EXCEPT e ).

itab = CORRESPONDING #( itab3 EXCEPT * ).
```

</td>
</tr>

<tr>
<td> <code>DISCARDING DUPLICATES</code> addition: Preventing runtime errors when duplicate lines are assigned </td>
<td>

... to the target table that is defined to accept only unique keys using the `DISCARDING DUPLICATES` addition of the `CORRESPONDING` operator. 
- In this case, the duplicate line is ignored in the source table. 
- The addition can also be specified with `MAPPING ...`.
<br>

``` abap
itab = CORRESPONDING #( itab2 DISCARDING DUPLICATES ).
```

</td>
</tr>

<tr>
<td> <code>DEEP</code>/<code>EXPANDING NESTED TABLES</code> additions: Copying data from deep internal tables </td>
<td>

- A deep internal table is a table with [deep](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_glosry.htm) line type, which means the table can itself contain internal tables as components, among others.
- The `BASE` addition does not delete the existing content. 
- See also the alternative `MOVE-CORRESPONDING` statements that use the `EXPANDING NESTED TABLES` addition.

<br>

``` abap
itab_nested2 = CORRESPONDING #( DEEP itab_nested1 ).

itab_nested2 = CORRESPONDING #( DEEP BASE ( itab_nested2 ) itab_nested1 ).

MOVE-CORRESPONDING itab_nested1 TO itab_nested2 EXPANDING NESTED TABLES.

MOVE-CORRESPONDING itab_nested1 TO itab_nested2 EXPANDING NESTED TABLES KEEPING TARGET LINES.
```

</td>
</tr>

<tr>
<td> <code>CORRESPONDING</code> with lookup table </td>
<td>

- You can construct an internal table by joining an internal table and a lookup table and comparing their components.
- Syntax pattern of the variant:  
    ```abap
    ... CORRESPONDING type|#( itab FROM lookup_tab 
                              USING [KEY key_name] a1 = b1 a2 = b2 ... 
                              [MAPPING ...] ) ...
    ```
- Find more information and code snippets in the [Constructor Expressions](05_Constructor_Expressions.md#corresponding-with-lookup-table) cheat sheet.

</td>
</tr>


</table>

> [!NOTE]  
> The `CL_ABAP_CORRESPONDING` class can be used for assignments. Find an example in the [Released ABAP Classes](22_Released_ABAP_Classes.md) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

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

> [!NOTE]  
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

"Access by dereferencing
DATA(copy_deref_itab) = dref_tab->*.
DATA(read_line) = dref_tab->*[ 2 ].
DATA(read_comp) = dref_tab->*[ 1 ]-a.
dref_tab->*[ 1 ]-a = 'zzz'.
ASSERT dref_tab->*[ 1 ]-a = 'zzz'.
INSERT VALUE s( a = 'yyy' b = 3 ) INTO TABLE dref_tab->*.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example: Exploring Populating Internal Tables

Expand the following collapsible section for example code. To try it out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. 
The example is set up to display output in the console, but only for few data objects. You may want to set a break point at the earliest possible position and walk through the example in the debugger. This will allow you to double-click on data objects and observe how the different statements affect their contents.

<details>
  <summary>🟢 Click to expand for a code example</summary>
  <!-- -->

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
Note that you can also use ABAP SQL `SELECT` statements to read from internal tables. This is covered further down.

### Determining the Target Area when Reading Single Lines in READ TABLE Statements

<table>

<tr>
<td> Target Area </td> <td> Notes </td>
</tr>

<tr>
<td> 

Data object

 </td>

 <td> 

- Used to copy a line to a data object using the addition `INTO`.
- After the copying, the line found exists separately in the internal table and in the data object. 
- If you change the data object or the table line, the change does not affect the other. 
- However, you can modify the copied table line and use a `MODIFY` statement to modify the table based on the modified table line (see below). 
- The `TRANSPORTING` addition specifies which components to copy. If it is not specified, all components are respected.
    
<br>

``` abap
"dobj must have the table's structure type
READ TABLE itab INTO dobj ...   

READ TABLE itab INTO DATA(dobj_inl) ...

READ TABLE itab INTO ... TRANSPORTING comp1 [comp2 ... ].
```

 </td>
</tr>

<tr>
<td> 

Field symbol

 </td>

 <td> 

- Assigning a line to a [field symbol](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm "Glossary Entry"),
  for example, using an inline declaration (`... ASSIGNING FIELD-SYMBOL(<fs>) ...`). 
- When you then access the field symbol, it means that you access the found table line. There is no actual copying of content. Therefore, modifying the field symbol means
    modifying the table line directly. 
- Note that you cannot use the `TRANSPORTING` addition since the entire table is assigned to the field symbol.

<br>

``` abap
"The field symbol must have an appropriate type.
READ TABLE itab ASSIGNING <fs1> ...                 

"Inline declaration
READ TABLE itab ASSIGNING FIELD-SYMBOL(<fs2>) ...   
```

 </td>
</tr>

<tr>
<td> 

Data reference variable

 </td>

 <td> 

- Reading a line into a [data reference variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_variable_glosry.htm "Glossary Entry") using `REFERENCE INTO`. 
- In this case, no copying takes place. 
- If you want to address the line, you must first [dereference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendereferencing_operat_glosry.htm) the data reference. 
- You cannot use the addition `TRANSPORTING`.

<br>

``` abap
"The data reference variable must have an appropriate type.
READ TABLE itab REFERENCE INTO dref ...

"Inline declaration
READ TABLE itab REFERENCE INTO DATA(dref_inl) ...
```

 </td>
</tr>

</table>

> [!TIP]
> Which to use then? Since all syntax options basically provide similar
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

### Reading Single Lines by Index

<table>
<tr>
<td> Statement </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>READ TABLE</code> </td>
<td>

The following example shows `READ TABLE` statements to read a single line from an internal table by specifying the index. You can use the addition `USING KEY` to specify a table key and thus explicitly determine the table index to use. If the table has a sorted secondary
key, the addition can be specified and the line to be read is then determined from its secondary table index. If the primary table key is
specified by its name `primary_key`, the table must be an index table, and the behavior is the same as if `USING KEY` was
not specified.
Note that the examples only show reading into a work area. Other targets are possible as shown above. 

<br>

``` abap
"Primary table index is used by default
READ TABLE itab INTO wa INDEX i.

"Primary table index is used, primary key's default name specified explicitly 
READ TABLE itab INTO wa INDEX i USING KEY primary_key.

"Secondary table index is used (assuming a secondary table key sec_key exists) 
READ TABLE itab INTO wa INDEX i USING KEY sec_key.

"Note: You can also use alias names for the keys if specified.
```

</td>
</tr>

<tr>
<td> Table expressions </td>
<td>

Using [table
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentable_expressions.htm),
the read result is stored in a variable that can be declared inline.
The number in the square brackets represents the index. A line that is
not found results in an runtime error. To avoid an error, you can
use a [`TRY ... CATCH ... ENDTRY.`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptry.htm) block.

Find more information about syntax related to table expressions [further down](#table-expressions).
<br>

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

</td>
</tr>
<tr>
<td> Avoiding an exception when using table expressions </td>
<td>

When you read a non-existent line using a table expression, you may not want to throw an exception. You can also embed the table expression
in a constructor expression using the `OPTIONAL` addition. This way, an unsuccessful read will not trigger the 
exception. The result returned is a line with initial values.
Alternatively, you can use the `DEFAULT` addition to return a
default line in case of an unsuccessful read operation, which can also be another table expression or constructor expression.

<br>

``` abap
DATA(line1) = VALUE #( itab[ 6 ] OPTIONAL ).

DATA(line2) = VALUE #( itab[ 7 ] DEFAULT itab[ 1 ]  ).
DATA(line3) = VALUE #( itab[ 8 ] DEFAULT VALUE #( ) ).
```


</td>
</tr>


</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Reading Single Lines Using Table Keys

Lines can be read by explicitly specifying the table keys or the alias names, if any.


<table>
<tr>
<td> Statement </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>READ TABLE</code> </td>
<td>

``` abap
"Example internal table with primary and secondary table key and alias names
"Assumption: All components are of type i

DATA it TYPE SORTED TABLE OF struc
  WITH NON-UNIQUE KEY primary_key ALIAS pk COMPONENTS a b
  WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS c d.

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

</td>
</tr>

<tr>
<td> Table expressions </td>
<td>

Find more information about syntax related to table expressions [further down](#table-expressions).


``` abap
line = it[ TABLE KEY primary_key COMPONENTS a = 1 b = 2 ].

"The addition COMPONENTS is optional; same as above
line = it[ TABLE KEY primary_key a = 1 b = 2 ].

"Primary key alias
line = it[ TABLE KEY pk a = 1 b = 2 ].

"Secondary table key
line = it[ TABLE KEY sec_key c = 3 d = 4 ].

"Secondary table key alias
line = it[ TABLE KEY sk c = 3 d = 4 ].

"Note the section further down about just using KEY (instead of TABLE KEY).
```

</td>
</tr>

</table>



<p align="right"><a href="#top">⬆️ back to top</a></p>

### Reading Single Lines Using a Free Key

The specified components used as keys need not be part of a table key.

<table>
<tr>
<td> Statement </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>READ TABLE</code> </td>
<td>

``` abap
"Note: Table keys are specified with the ... WITH TABLE KEY ... addition, 
"free keys with ... WITH KEY ....
READ TABLE it INTO wa WITH KEY b = 2.
```

</td>
</tr>

<tr>
<td> Table expressions </td>
<td>


``` abap
line = it[ b = 2 ].
```

</td>
</tr>

</table>



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

#### Specifying a WHERE Condition in READ TABLE Statements

- The syntax `READ TABLE ... WHERE ...` searches for the first line that matches a `WHERE` condition.
- The value of `sy-tabix` is set when a line is found, while `sy-subrc` is set to either 0 or 4.
- The `WHERE` condition can use comparison and predicate expressions (see the code snippet comments below for examples).
- Syntax options: 
  - The read result must be specified before the `WHERE` condition.
  - The `TRANSPORTING NO FIELDS` and `USING KEY` additions are possible.
  - Dynamic `WHERE` conditions are possible.
- If a `READ TABLE` statement can be expressed using `WITH KEY`, this approach - instead of using a `WHERE` condition - is recommended as it is more performant. A syntax warning occurs but can be suppressed with the pragma `##read_where_ok`.
- For optimized searches with a `WHERE` condition:
  - No optimized search in standard tables without a secondary table key.
  - For sorted and hashed keys (sorted/hashed tables, secondary table keys), the search is optimized as described [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenitab_where_optimization.htm), if specifications are transferable to a key access.

```abap
"Creating and populating demo internal tables
TYPES: BEGIN OF s_demo,
            comp1 TYPE i,
            comp2 TYPE c LENGTH 5,
            comp3 TYPE i,
            comp4 TYPE c LENGTH 5,
        END OF s_demo,
        t_type_so TYPE SORTED TABLE OF s_demo WITH UNIQUE KEY comp1 WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp3.

DATA(itab) = VALUE t_type_so( ( comp1 = 1 comp2 = 'lorem' comp3 = 30 comp4 = 'ipsum' )
                              ( comp1 = 2 comp2 = 'dolor' comp3 = 20 comp4 = 'sit' )
                              ( comp1 = 3 comp2 = 'amet' comp3 = 40 comp4 = 'hello' )
                              ( comp1 = 4 comp2 = 'world' comp3 = 50 comp4 = 'ABAP' )
                              ( comp1 = 5 comp2 = 'test' comp3 = 10 comp4 = '' ) ).

DATA wa TYPE s_demo.

*&---------------------------------------------------------------------*
*& WHERE condition with comparison expressions
*&---------------------------------------------------------------------*

"Examples: =/EQ, <>/NE, >/GT, </LT, >=,GE, <=/LE,
"          CO, CN, CA, NA, CS, NS, CP, NP,
"          [NOT] BETWEEN ... AND
"          [NOT] IN ranges_tables

READ TABLE itab INTO wa WHERE comp1 > 3.
ASSERT sy-tabix = 4.

"'or' also available in other lines in this component, but the first found line
"is returned (lines 1, 2, 4)
READ TABLE itab INTO wa WHERE comp2 CS 'or'.
ASSERT sy-tabix = 1.

"'d' occurs in lines 2, 4
READ TABLE itab INTO wa WHERE comp2 CS 'd'.
ASSERT sy-tabix = 2.

READ TABLE itab INTO wa WHERE comp2 CS 'd'.
ASSERT sy-tabix = 2.

*&---------------------------------------------------------------------*
*& WHERE condition with predicate expressions
*&---------------------------------------------------------------------*

"Examples: IS [NOT] INITIAL
"          IS [NOT] BOUND
"          IS [NOT] INSTANCE OF

READ TABLE itab INTO wa WHERE comp1 > 4 AND comp4 IS INITIAL.
ASSERT sy-tabix = 5.

*&---------------------------------------------------------------------*
*& WITH KEY instead of WHERE condition
*&---------------------------------------------------------------------*

"Syntax warning in READ TABLE ... WHERE ... statements
READ TABLE itab INTO wa WHERE comp4 IS INITIAL.
ASSERT sy-tabix = 5.

"For a better performance, the previous statement should be
"replaced by a READ TABLE ... WITH KEY ... statement.
READ TABLE itab INTO wa WITH KEY comp4 = ''.
ASSERT sy-tabix = 5.

"You can also suppress the syntax warning by a pragma.
READ TABLE itab INTO wa WHERE comp4 IS INITIAL ##read_where_ok.
ASSERT sy-tabix = 5.

*&---------------------------------------------------------------------*
*& Further additions
*&---------------------------------------------------------------------*

"TRANSPORTING NO FIELDS addition is possible
READ TABLE itab TRANSPORTING NO FIELDS WHERE comp2 CS 'd'.
ASSERT sy-tabix = 2.

"USING KEY addition
READ TABLE itab USING KEY primary_key INTO wa WHERE comp2 CS 't'.
ASSERT sy-tabix = 3.

READ TABLE itab USING KEY sk INTO wa WHERE comp3 > 40.
ASSERT sy-tabix = 5.

*&---------------------------------------------------------------------*
*& Excursions
*&---------------------------------------------------------------------*

"Note the comparison rules for character-like data types
READ TABLE itab INTO wa WHERE comp2 = 'lorem' ##read_where_ok.
ASSERT sy-tabix = 1.

"In the following case, the length of the comp2 value increased to match the
"length of the specified text field, i.e. the surplus characters from the right
"side text field are not truncated. As a consequence, the line is not found.

DATA(some_text) = 'loremXYZ'.

READ TABLE itab INTO wa WHERE comp2 = some_text ##read_where_ok.
ASSERT sy-tabix = 0 AND sy-subrc <> 0.

"When using READ TABLE ... WITH KEY ... the behavior is different. In that
"case, the surplus characters are truncated because of a conversion. Therefore,
"the following statement finds a line.
READ TABLE itab INTO wa WITH KEY comp2 = some_text.
ASSERT sy-tabix = 1 AND sy-subrc = 0.

"Note: The read target can only be placed before WHERE conditions.
"The following statements are not possible.
"READ TABLE itab WHERE comp2 CS 'd' INTO wa.
"READ TABLE itab WHERE comp2 CS 'd' TRANSPORTING NO FIELDS.

"READ TABLE ... WHERE ... statements can replace LOOP AT ... WHERE ...
"statements including EXIT.
LOOP AT itab INTO wa WHERE comp2 CS 'd'.
    ASSERT sy-tabix = 2.
    EXIT.
ENDLOOP.

*&---------------------------------------------------------------------*
*& Dynamic WHERE condition
*&---------------------------------------------------------------------*

"Character-like data objects or standard tables with character-like line type
"can be specified

DATA(dyn_where_cond_str) = `comp2 CS 'd'`.

READ TABLE itab INTO wa WHERE (dyn_where_cond_str).
ASSERT sy-tabix = 2.

DATA(dyn_where_cond_tab) = VALUE string_table( ( `comp2` ) ( `CS` ) ( `'d'` ) ).
READ TABLE itab INTO wa WHERE (dyn_where_cond_tab).
ASSERT sy-tabix = 2.
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
  <summary>🟢 Click to expand for more information and a code example</summary>
  <!-- -->

- `READ TABLE` without `BINARY SEARCH`: Table is accessed linearly
- `READ TABLE` with `BINARY SEARCH`: Table is accessed using a binary search 
  - Using the `BINARY SEARCH` addition is particularly more efficient for larger tables when accessing data often. 
  - The table must be sorted in ascending order based on the keys being searched. 
  - `BINARY SEARCH` is suitable for standard tables that do not have a secondary key defined and when you need to make multiple read accesses to the table (however, note the costs of a previous sorting)  
  - `BINARY SEARCH` can only be used with index tables and not with hashed tables. If the table is a sorted table and the read access uses a free key, the addition can only be applied when the initial part of the table key is specified. I.e. if key components are `a`, `b`, and `c`, the addition can be used by specifying `a` alone, `a` and `b`, or `a`, `b`, and `c`. However, it means that the syntax can be specified without errors. The `BINARY SEARCH` specifiation has no effect and is redundant. Syntactically not possible with `BINARY SEARCH` (for example): `b` and `c` without `a`, or any other non-key component (because it cannot be sorted according to the non-key component). 
- Depending on the number of times you need to access the internal table, it is recommended to work with sorted tables or tables with secondary keys. If you only need to read one or a few data sets, consider the administrative costs of setting up the index.
- Note: The `BINARY SEARCH` addition is not available for table expressions. If `KEY ...` is specified, an optimized search is performed by default. There are no performance differences between using the `READ TABLE` statement and table expressions.

To try it out the following example, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.

The example includes multiple reads on standard internal tables using a `READ TABLE` statement ...
- without `BINARY SEARCH`. 
- with `BINARY SEARCH` and a previous `SORT` statement.
- with a secondary table key whose components correspond to free key used in the previous statements.

The runtime of the reads is determined and stored in internal tables. The read operations are repeated several times to have a more accurate runtime evaluation. The fastest time is output. 
The example is intended to demonstrate the performance gain using the `BINARY SEARCH` addition (and also using a secondary table key).

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
    CONSTANTS: num_of_table_lines TYPE i VALUE 5000,
               num_of_repetitions TYPE i VALUE 10,
               num_of_reads TYPE i VALUE 3000.

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

Expand the following collapsible section for example code. To try it out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. 
The example is not set up to display output in the console. You may want to set a break point at the first position possible and walk through the example in the debugger. This will allow you to double-click on data objects and observe how the different statements affect their contents.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

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


## Accessing Single Table Lines via Table Expressions

The previous sections also covered table expressions. This section provides a summary of their use.

- Table expressions typically use the name of an internal table followed by square brackets with content `[ ... ]`, specifying a table line.
- They allow read and write access to internal tables at various positions.
- These expressions are a concise form of `READ TABLE` statements, enabling read and write operations in operand positions.
  - Unlike `READ TABLE`, table expressions do not change the `sy-tabix` system field (except with `ASSIGN`).
- After the brackets, you can specify chaining (component and object component selectors or additional square brackets). Without chaining, the entire line is respected.
- Note that not found lines raise the catchable exception `CX_SY_ITAB_LINE_NOT_FOUND`.

The following table demonstrates a selection of subjects where table expressions are applicable.


<table>
<tr>
<td> Subject </td> <td> Details/Code Snippet </td>
</tr>

<tr>
<td> Reading table lines by index </td>
<td>

- When only specifying the index number in the square brackets, it means referring to the primary table index.
- In this case, the internal table must be an index table.
- Using the `KEY ... INDEX ...` addition, you can specify the table index explicitly. Either use the predefined name `primary_key` for the primary key explicitly (or an alias name, if specified), or the secondary key name (or an alias name, if specified).
- The `KEY` addition here works like `USING KEY` in `READ TABLE` statements.
- Note that the demo internal tables in the snippet are used in the following examples of the section. 

<br>

``` abap
"Creating and populating demo internal tables
"Note: These demo tables are relevant for most of the code snippets
"in this section.
TYPES: BEGIN OF s_demo,
          comp1 TYPE i,
          comp2 TYPE i,
          comp3 TYPE i,
          comp4 TYPE c LENGTH 3,
        END OF s_demo,
        ttyp        TYPE SORTED TABLE OF s_demo WITH UNIQUE KEY comp1 WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp2 comp3,
        ttyp_hashed TYPE HASHED TABLE OF s_demo WITH UNIQUE KEY comp1 WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp2 comp3,
        ttyp2       TYPE SORTED TABLE OF s_demo WITH UNIQUE KEY comp1 comp2 WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp3.

DATA(itab) = VALUE ttyp( ( comp1 = 1 comp2 = 30 comp3 = 31 comp4 = 'aaa' )
                         ( comp1 = 2 comp2 = 20 comp3 = 21 comp4 = 'bbb' )
                         ( comp1 = 3 comp2 = 10 comp3 = 11 comp4 = 'ccc' ) ).

DATA itab_hashed TYPE ttyp_hashed.
itab_hashed = itab.
DATA itab_so TYPE ttyp2.
itab_so = itab.

DATA line TYPE s_demo.

"Reading table line by index
"Just specifying the index number means referring to the primary table index.
"In this case, the internal table must be an index table.

"In the example, the entire table line is assigned to a variable
line = itab[ 2 ].

"KEY ... INDEX ... additions
"For reading a line according to a table index.
"The following example has the same effect as above. Here, the default
"name of the primary key is specified explicitly.
line = itab[ KEY primary_key INDEX 2 ].

"Secondary table key specified, using secondary table index
line = itab[ KEY sk INDEX 1 ].

"This syntax is not possible for hashed tables.
"DATA(line_hashed_tab1) = itab_hashed[ 2 ].
"DATA(line_hashed_tab2) = itab_hashed[ KEY primary_key INDEX 2 ].
"Secondary table index access is possible for hashed tables
DATA(line_hashed_tab3) = itab_hashed[ KEY sk INDEX 2 ].
``` 

</td>
</tr>


<tr>
<td> Reading table lines by table key </td>
<td>

- `TABLE KEY` addition
  - Requires the table key to be fully specified, i.e. all components of the key (primary table key or secondary table key) must be specified, and no other components can be specified.
  - Works like the `WITH TABLE KEY` addition in `READ TABLE` statements. However, in table expressions, the `COMPONENTS` addition is optional and the key name must be specified.
  - Usually, specifying the primary table key explicitly (using predefined name `primary_key` or an alias, if available) is not required. You can instead also just specify a free key search (i.e. not using any `TABLE KEY`/`KEY` addition) and specify all primary table key components. Then, an optimized search is also performed for sorted and hashed tables. 
- `KEY` addition without `TABLE`:
  - This syntax option reads a line in accordance with a specified free key. 
  - It is not mandatory to specify all components of a sorted table key (the initial left part at least is required). The search is then partly optimized (unlike fully optimized when all key components are specified).
  - Example purposes of such a specification: Determining the existence of a line, or determining a line number for the starting point for a loop starting at that position.
  - Additionally, in case of sorted and secondary table keys, other components not being part of the key can be specified.
  - Same as above, specifying the addition `COMPONENTS` is optional.
  - Just using `KEY` and specifying all key components following the key name works like specifying `TABLE KEY`. The `TABLE KEY` addition just ensures that indeed all key components are specified. 
- Not using any `TABLE KEY`/`KEY` additions:
  - In case of sorted and hashed tables and without using any `TABLE KEY`/`KEY` additions, an optimized search is performed when some or all components of the primary table key are specified as free keys.
- Notes:
  - Binary searches cannot be enforced with table expressions (unlike `READ TABLE` statements).
  - Key names can also be specified dynamically using parentheses `(...)`.

<br>

``` abap
*&---------------------------------------------------------------------*
*& TABLE KEY addition
*&---------------------------------------------------------------------*

"Explicitly specifying the primary table key
line = itab[ TABLE KEY primary_key COMPONENTS comp1 = 1 ].

"The following statement is not possible as no other components can be specified.

"line = itab[ TABLE KEY primary_key COMPONENTS comp1 = 1 comp2 = 30 ].

"The addition COMPONENTS is optional; the following example is the same as above
line = itab[ TABLE KEY primary_key comp1 = 1 ].

"Specifying a secondary table key
line = itab[ TABLE KEY sk COMPONENTS comp2 = 20 comp3 = 21 ].

"Optional COMPONENTS addition
line = itab[ TABLE KEY sk comp2 = 20 comp3 = 21 ].

"Fully specifying the table key components is required with TABLE KEY. So, the
"following statement is not possible.

"line = itab[ TABLE KEY sk comp2 = 20 ].

*&---------------------------------------------------------------------*
*& KEY addition
*&---------------------------------------------------------------------*

"Using KEY and specifying all key components work like specifying TABLE KEY
line = itab[ KEY primary_key COMPONENTS comp1 = 1 ].
line = itab[ KEY primary_key comp1 = 1 ].
line = itab[ KEY sk COMPONENTS comp2 = 20 comp3 = 21 ].
line = itab[ KEY sk comp2 = 20 comp3 = 21 ].

"Unlike TABLE KEY, KEY does not enforce all key components to be specified
line = itab[ KEY sk comp2 = 20 ].

"In case of sorted and secondary table keys, other components not being part
"of the key can be specified
line = itab[ KEY primary_key comp1 = 1 comp4 = 'aaa' ].
line = itab[ KEY sk comp2 = 20 comp4 = 'bbb' ].

"The following statements are not possible. The initial, left part of
"the key must be specified. In the example case, it is comp2.

"line = itab[ KEY sk comp3 = 21 comp4 = 'bbb' ].
"line = itab[ KEY sk comp4 = 'bbb' ].

"The following statement triggers a syntax warning because the initial
"part of a table key is specified, but the key name is not specified.
"In this case, the search is not optimized as the component is not
"part of the primary table key of the sorted table. You may optimize
"it by specifying the key.

"line = itab[ comp2 = 10 ].

"The syntax warning can be suppressed by a pragma.
line = itab[ comp2 = 10 ] ##primkey[sk].

"Specifying the key name
line = itab[ KEY sk comp2 = 10 ].

*&---------------------------------------------------------------------*
*& No TABLE KEY/KEY additions
*&---------------------------------------------------------------------*

"Specifying a free key search, but including all components of the primary
"table key
"For a sorted table as in the example, the search is fully optimized.
line = itab[ comp1 = 1 ].

"Partly optimized (only a part of the primary table key of the sorted
"example table is specified)
line = itab_so[ comp1 = 1 ].
``` 

</td>
</tr>


<tr>
<td> Reading table lines using free keys </td>
<td>

- Free keys are specified without any `... KEY ...` addition. This corresponds to `WITH KEY` specifications in `READ TABLE` statements.
- However, in case of sorted and hashed tables, an optimized search is performed when some or all of the key components are specified as free keys (without any `... KEY ...` addition).
- Note that binary searches cannot be enforced with table expressions (unlike `READ TABLE` statements).

<br>

``` abap
"The search is and cannot be optimized as the component is not part of 
"the primary table key of the table. Plus, no appropriate 
"secondary table key can be applied.
line = itab[ comp4 = 'ccc' ].
``` 

</td>
</tr>


<tr>
<td> Using the read result in various positions </td>
<td>

This is to emphasize that table expressions can be used in various read positions. The snippet shows a few examples. Check the notes in the ABAP Keyword Documentation topics about the use. 

<br>

``` abap
DATA(first_line) = itab[ 1 ].

"Using table expressions as arguments of built-in functions
IF line_exists( itab[ 1 ] ).
    ...
ELSE.
    ...
ENDIF.

ASSERT line_index( itab[ comp4 = 'ccc' ] ) = 3.
``` 

</td>
</tr>


<tr>
<td> Assigning table lines to a field symbol </td>
<td>

``` abap
"Works like READ TABLE ... ASSIGNING ...
ASSIGN itab[ 2 ] TO FIELD-SYMBOL(<line>).
"Note: Table expressions do not set the sy-tabix
"value, except when used with ASSIGN.
ASSERT sy-tabix = 2.

"Note: Assigning a non-existent line results in sy-subrc = 4
"An exception is not raised.
ASSIGN itab[ 99 ] TO <line>.
ASSERT sy-subrc = 4.
``` 

</td>
</tr>

<tr>
<td> Data reference variables pointing to a table line </td>
<td>


``` abap
DATA ref TYPE REF TO data.
ref = NEW s_demo(  ).
ref->* = itab[ 1 ].
``` 

</td>
</tr>

<tr>
<td> Specifying table expressions as operands in constructor expressions with <code>VALUE</code> and <code>REF</code> </td>
<td>


``` abap
line = VALUE #( itab[ 2 ] ).
"Works like READ TABLE ... REFERENCE INTO ...
DATA(line_ref) = REF #( itab[ 3 ] ).
``` 

</td>
</tr>

<tr>
<td> Specifying a default value for lines that are not found or marking as optional to avoid an exception </td>
<td>

- You can specify default values for lines that are not found to avoid an exception.
- The first example shows catching the execption with a `TRY` control structure.
- The `OPTIONAL` and `DEFAULT` additions can be used in the context of statements using table expressions and constructor expressions (`VALUE` and `REF` are possible). 
- When using `OPTIONAL`, the type-specific initial value is set. After the `DEFAULT` addition, a value is expected that is convertible to the target type. You may also specify another table expression for an alternative table line to be searched.


<br>

``` abap
"Accessing a non-existent table line raises a catchable exception
TRY.
    line = itab[ 4 ].
    CATCH cx_sy_itab_line_not_found.
ENDTRY.

line = VALUE #( itab[ 4 ] OPTIONAL ).

line = VALUE #( itab[ 5 ] DEFAULT itab[ 1 ]  ).
line = VALUE #( itab[ 6 ] DEFAULT VALUE #( ) ).
``` 

</td>
</tr>

<tr>
<td> Field symbols and dereferenced data references specified before the square brackets </td>
<td>

The previous examples use concrete internal table names specified before the square brackets. Field symbols and dereferenced data references are also possible.


<br>

``` abap
ASSIGN itab TO FIELD-SYMBOL(<tab>).
line = <tab>[ 1 ].

DATA dref TYPE REF TO ttyp.
dref = NEW #(  ).

dref->* = itab.
line = dref->*[ 2 ].
``` 

</td>
</tr>

<tr>
<td> Reading multiple lines </td>
<td>

Using table expressions, you can read entire lines individually. This is just to emphasize that when reading multiple lines sequentially, `LOOP` statements and `FOR` loops are recommended.

<br>

``` abap
"Instead of something like this ...
DO.
    ASSIGN itab[ sy-index ] TO FIELD-SYMBOL(<fs>).
    IF sy-subrc = 0.
     ...
     "<fs>-comp1 = ...
    ELSE.
     EXIT.
    ENDIF.
ENDDO.

"... a LOOP statement, for example.
LOOP AT itab ASSIGNING <fs>.
    ...
    "<fs>-comp1 = ...
ENDLOOP.
``` 

</td>
</tr>

<tr>
<td> Reading individual components of table lines </td>
<td>


``` abap
"Reading individual components of table lines using chainings after the
"closing square bracket

"Read component via line read using ...
"... index
DATA(compa) = itab[ 1 ]-comp1.
"... table key
DATA(compb) = itab[ TABLE KEY primary_key comp1 = 1 ]-comp2.
DATA(compc) = itab[ TABLE KEY sk comp2 = 30 comp3 = 31 ]-comp1.
"... free key
DATA(compd) = itab[ comp4 = 'ccc' ]-comp1.
``` 

</td>
</tr>

<tr>
<td> Chaining table expressions in the context of nested internal tables </td>
<td>

- Table expressions can be chained if the table expression result is a table itself.
- Note the pitfall remarks further down.

<br>

``` abap
"Creating deep internal table
TYPES: BEGIN OF s_sub,
            comp1 TYPE i,
            comp2 TYPE i,
        END OF s_sub,
        tab_type_sub TYPE TABLE OF s_sub WITH EMPTY KEY,
        BEGIN OF s,
            compa TYPE i,
            compb TYPE TABLE OF tab_type_sub WITH EMPTY KEY,
        END OF s,
        tab_type TYPE TABLE OF s WITH EMPTY KEY.

"Expressions helpful when populating
DATA(deep_tab) = VALUE tab_type( ( compa = 1
                                   compb = VALUE #( ( VALUE #( ( comp1 = 3 comp2 = 4 ) ( comp1 = 5 comp2 = 6 ) ) )
                                                    ( VALUE #( ( comp1 = 7 comp2 = 8 ) ( comp1 = 9 comp2 = 10 ) ) ) ) )
                                    ( compa = 2
                                      compb = VALUE #( ( VALUE #( ( comp1 = 11 comp2 = 12 ) ( comp1 = 13 comp2 = 14 ) ) )
                                                       ( VALUE #( ( comp1 = 15 comp2 = 16 ) ( comp1 = 17 comp2 = 18 ) ) ) ) ) ).



DATA(num1) = deep_tab[ 2 ]-compb[ 1 ][ 2 ]-comp2.
ASSERT num1 = 14.

"A statement such as the previous one instead of, for example, multiple statements as follows.
READ TABLE deep_tab INDEX 2 INTO DATA(wa1).
READ TABLE wa1-compb INDEX 1 INTO DATA(wa2).
READ TABLE wa2 INTO DATA(wa3) INDEX 2.

DATA(num2) = wa3-comp2.

ASSERT num2 = num1.
``` 

</td>
</tr>

<tr>
<td> Table expression result having a reference type enabling chainings with the object component selector </td>
<td>


``` abap
DATA itab_ref TYPE TABLE OF REF TO s_demo WITH EMPTY KEY.
itab_ref = VALUE #( ( NEW s_demo( comp1 = 1 comp2 = 30 comp3 = 31 comp4 = 'aaa' ) ) ).

"Reading entire line by dereferencing
DATA(deref_line) = itab_ref[ 1 ]->*.
"Reading component by dereferencing
DATA(dref_compa) = itab_ref[ 1 ]->comp3.
"The following syntax is also possible (dereferencing operator followed
"by the component selector).
DATA(dref_compb) = itab_ref[ 1 ]->*-comp4.
``` 

</td>
</tr>

<tr>
<td> Table expressions in write positions: Writes on the entire line </td>
<td>

Note that you cannot perform writes on entire lines in the context of key tables as key values cannot be changed.

<br>

``` abap
"The demo table is a key table. Therefore, writes on entire lines produce runtime errors.
"itab[ 3 ] = VALUE #( ).

"Creating a standard table
DATA itab_std TYPE TABLE OF s_demo WITH NON-UNIQUE KEY comp1 WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp2 comp3.
itab_std = itab.

"Here, writes on entire lines are allowed.
itab_std[ 3 ] = VALUE #( comp1 = 123 comp4 = 'zzz' ).
CLEAR itab_std[ 3 ].
``` 

</td>
</tr>

<tr>
<td> Table expressions in write positions: Writes on individual components </td>
<td>


``` abap
itab[ 3 ]-comp4 = 'yyy'.
itab_ref[ 1 ]->comp3 = 123.
"No key value change allowed in key tables
"The following statement causes a runtime error.
"itab[ 1 ]-comp1 = 987.

"Key value change allowed for standard tables.
itab_std[ 3 ]-comp1 = 456.
``` 

</td>
</tr>

<tr>
<td> Dynamic key and component name specifications </td>
<td>

For more information about dynamic programming, refer to the [Dynamic Programming](06_Dynamic_Programming.md) cheat sheet.

<br>

``` abap
"Dynamic key and component specifications
line = itab[ KEY ('PRIMARY_KEY') COMPONENTS comp1 = 1 ].
line = itab[ KEY ('PRIMARY_KEY') comp1 = 1 ].
line = itab[ KEY ('SK') COMPONENTS comp2 = 20 comp3 = 21 ].
line = itab[ KEY ('SK') comp2 = 20 comp3 = 21 ].

line = itab[ KEY ('PRIMARY_KEY') ('COMP1') = 1 ].
line = itab[ KEY ('SK') ('COMP2') = 20 ('COMP3') = 21 ].

DATA(key_name) = 'PRIMARY_KEY'.
DATA(comp_name) = 'COMP1'.
line = itab[ KEY (key_name) (comp_name) = 1 ].
line = itab[ KEY primary_key (comp_name) = 1 ].
``` 

</td>
</tr>


<tr>
<td> Pitfalls regarding table expressions </td>
<td>

- The following example emphasizes that table expressions - expression enabling in modern ABAP as such - comes in very handy. 
- However, also take the maintainability, debuggability and readbility of your code into consideration. 
- The example includes the table expression chaining example from above (that may be fairly hard to understand) and a nonsensical, bad example overusing table expressions (affecting performance).
- Expand the following collapsible section for example code. To try it out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

``` abap
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

    "-------------- Chaining table expressions -------------

    "Creating a deep internal table
    TYPES: BEGIN OF s_sub,
             comp1 TYPE i,
             comp2 TYPE i,
           END OF s_sub,
           tab_type_sub TYPE TABLE OF s_sub WITH EMPTY KEY,
           BEGIN OF s,
             compa TYPE i,
             compb TYPE TABLE OF tab_type_sub WITH EMPTY KEY,
           END OF s,
           tab_type TYPE TABLE OF s WITH EMPTY KEY.

    "Expressions helpful when populating
    DATA(deep_tab) = VALUE tab_type( ( compa = 1
                                       compb = VALUE #( ( VALUE #( ( comp1 = 3 comp2 = 4 ) ( comp1 = 5 comp2 = 6 ) ) )
                                                        ( VALUE #( ( comp1 = 7 comp2 = 8 ) ( comp1 = 9 comp2 = 10 ) ) ) ) )
                                     ( compa = 2
                                       compb = VALUE #( ( VALUE #( ( comp1 = 11 comp2 = 12 ) ( comp1 = 13 comp2 = 14 ) ) )
                                                        ( VALUE #( ( comp1 = 15 comp2 = 16 ) ( comp1 = 17 comp2 = 18 ) ) ) ) ) ).

    "Chained table expressions in the context of reading a value from a
    "nested internal table
    "Such a chaining works if the table expression result is a table itself.
    "Such statements are fairly short, using few lines of code, however,
    "they may be hard to understand and debug.
    DATA(num1) = deep_tab[ 2 ]-compb[ 1 ][ 2 ]-comp2.

    out->write( num1 ).

    "A statement as above instead of multiple statements, for example, as follows.
    READ TABLE deep_tab INDEX 2 INTO DATA(wa1).
    READ TABLE wa1-compb INDEX 1 INTO DATA(wa2).
    READ TABLE wa2 INTO DATA(wa3) INDEX 2.
    DATA(num2) = wa3-comp2.

    out->write( num2 ).

    "-------------- Overusing table expression -------------
    "The following examples performes many reads and writes
    "using table expressions. The first example includes multiple
    "table expressions, while the second one is differently designed,
    "without many table expressions. The runtimes of the loops are
    "stored, demonstrating that the second example has a significantly
    "reduced runtime compared to the first example.

    CONSTANTS: num_of_repetitions     TYPE i VALUE 10,
               num_of_assignment_runs TYPE i VALUE 1000.

    TYPES: BEGIN OF struct,
             a TYPE i,
             b TYPE i,
             c TYPE i,
             d TYPE i,
             e TYPE i,
             f TYPE i,
           END OF struct.

    DATA it TYPE TABLE OF struct WITH EMPTY KEY.

    it = VALUE #( ( a = 0 b = 0 c = 0 d = 0 e = 0 f = 0 )
                  ( a = 1 b = 1 c = 1 d = 1 e = 1 f = 1 ) ).
    DATA(it_original) = it.

    DATA runtime_tab TYPE TABLE OF decfloat34 WITH EMPTY KEY.

    DO num_of_repetitions TIMES.
      DATA(ts1) = utclong_current( ).
      DO num_of_assignment_runs TIMES.
        it[ 1 ]-a = sy-index.
        it[ 1 ]-b = sy-index.
        it[ 1 ]-c = sy-index.
        it[ 2 ]-d = sy-index.
        it[ 2 ]-e = sy-index.
        it[ 2 ]-f = sy-index.
        it[ 1 ]-d = it[ 2 ]-d.
        it[ 1 ]-e = it[ 2 ]-e.
        it[ 1 ]-f = it[ 2 ]-f.
        INSERT it[ 1 ] INTO TABLE it.
        INSERT it[ 2 ] INTO TABLE it.
      ENDDO.
      DATA(ts2) = utclong_current( ).
      cl_abap_utclong=>diff( EXPORTING high    = ts2
                                       low     = ts1
                             IMPORTING seconds = DATA(seconds) ).

      APPEND seconds TO runtime_tab.
    ENDDO.

    SORT runtime_tab BY table_line ASCENDING.

    out->write( `Multiple table expressions, fastest run:` ).
    out->write( runtime_tab[ 1 ] ).

    CLEAR runtime_tab.
    it = it_original.

    DO num_of_repetitions TIMES.
      ts1 = utclong_current( ).
      DO num_of_assignment_runs TIMES.
        ASSIGN it[ 1 ] TO FIELD-SYMBOL(<fs1>).
        ASSIGN it[ 2 ] TO FIELD-SYMBOL(<fs2>).
        <fs1>-a = sy-index.
        <fs1>-b = sy-index.
        <fs1>-c = sy-index.
        <fs2>-d = sy-index.
        <fs2>-e = sy-index.
        <fs2>-f = sy-index.
        <fs1>-d = <fs2>-d.
        <fs1>-e = <fs2>-e.
        <fs1>-f = <fs2>-f.
        INSERT <fs1> INTO TABLE it.
        INSERT <fs2> INTO TABLE it.
      ENDDO.
      ts2 = utclong_current( ).
      cl_abap_utclong=>diff( EXPORTING high    = ts2
                                       low     = ts1
                             IMPORTING seconds = seconds ).

      APPEND seconds TO runtime_tab.
    ENDDO.

    SORT runtime_tab BY table_line ASCENDING.

    out->write( `Avoiding overusing table expressions, fastest run:` ).
    out->write( runtime_tab[ 1 ] ).
  ENDMETHOD.
ENDCLASS.
``` 


</details>  

</td>
</tr>
<tr>


</table>

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
should do so immediately after the `LOOP` statement (e.g. by storing the value in a helper variable) to avoid possible overwriting in statements contained in the loop block.

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

The expressions are covered in the [Constructor Expressions](05_Constructor_Expressions.md) cheat sheet:
- [Iteration Expressions Using FOR](05_Constructor_Expressions.md#iteration-expressions-using-for)
- Special reduction operator `REDUCE` that is based on iteration expressions: [REDUCE](05_Constructor_Expressions.md#reduce)

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Interrupting and Exiting Loops

ABAP keywords such as 
- `CONTINUE` (terminating the current loop pass, continuing with the next)
- `CHECK` (conditional termination termination of the current loop pass if the logical expression is false), and 
- `EXIT` (loop terminated completely), 

are available to exit and interrupt loops. Find more information in the [Program Flow Logic](13_Program_Flow_Logic.md#interrupting-and-exiting-loops) cheat sheet.

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

### Inserting and Deleting Lines in Internal Tables in Loops

- In a loop, you can change the values of the current table line's components. You can also delete or insert entire lines.
- The position of these new or deleted lines is determined by the table index or a sorted key. For hashed tables or hash keys, the position is determined by the insertion order (though this can be changed with the `SORT` statement).
- After the currently processed table line, ...
  - inserting a line: The new line is processed in the next loop pass (watch out for a non-terminating loop).
  - deleting a line: The deleted line is not processed in the next loop pass.
- Before the currently processed table line, ...
  - inserting a line: The internal loop counter (`sy-tabix`) increases accordingly (in the case of index tables, sorted keys).
  - deleting a line: The internal loop counter (`sy-tabix`) decreases accordingly.
- You cannot replace or clear the entire table body in a loop. If detected statically, a syntax error is displayed.

The following examples explore the insertion and deletion of lines when looping across internal tables: 

```abap
"Creating and populating a demo standard internal table to
"work with in the example loops
TYPES: BEGIN OF s_loop_mod,
            text TYPE string,
            num  TYPE i,
        END OF s_loop_mod,
        t_loop_mod TYPE TABLE OF s_loop_mod WITH EMPTY KEY.

"Inserting 10 entries into the demo table
DATA(itab_original) = VALUE t_loop_mod( FOR x = 1 WHILE x <= 10 ( text = x ) ).
DATA(itab) = itab_original.

*&---------------------------------------------------------------------*
*& Inserting a line after the current line
*&---------------------------------------------------------------------*

"The example inserts a line after the currently processed line
"using an INSERT statement and specifying the index value
"(sy-tabix value + 1). The 'num' component is assigned the
"current sy-tabix value.
"Note: In all statements, the sy-tabix value is stored in a
"variable right after the LOOP statement. Assume that multiple
"statements are included before the statement that actually uses
"the current sy-tabix value. Other statements that potentially
"change the sy-tabix value might interfere.
"An EXIT statement takes care of exiting the loop. In the example,
"all values of the 'num' component in the original table lines
"(except the first line) are initial as the loop is exited.
LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs>).
  DATA(tabix) = sy-tabix.
  <fs>-num = tabix.
  INSERT VALUE #( text = tabix ) INTO itab INDEX tabix + 1.
  IF tabix = 50.
   EXIT.
  ENDIF.
ENDLOOP.

*Internal table content (parts of it):
*TEXT    NUM
*1       1
*1       2
*2       3
*3       4
*4       5
*...
*48      49
*49      50
*50      0
*2       0
*3       0
*4       0
*...

*&---------------------------------------------------------------------*
*& Deleting a line after the current line
*&---------------------------------------------------------------------*

"The example deletes a line after the current line using a
"DELETE statement and the INDEX addition. The index value
"is specified using the current sy-tabix value + 1.
"The 'num' value in the resulting internal table includes
"the sy-tabix value.

itab = itab_original.
LOOP AT itab ASSIGNING <fs>.
  tabix = sy-tabix.
  <fs>-num = tabix.
  DELETE itab INDEX tabix + 1.
ENDLOOP.

*Internal table content:
*TEXT    NUM
*1       1
*3       2
*5       3
*7       4
*9       5

*&---------------------------------------------------------------------*
*& Inserting a line before the current line
*&---------------------------------------------------------------------*

"The example insert a line before the currently processed line using
"an INSERT statement. The current sy-tabix value is used as INDEX value,
"moving down the currently processed table line one position.
"In that case, the sy-tabix value increases accordingly.
"Logic:
"- For example, the first line is processed, sy-tabix has the value 1.
"- A line is inserted at this position, moving down the currently processed line
"  one position. The moved line is then in the second position (as a new line exists
"  in the first position).
"- In the next loop pass, the loop is continued with the third line, i.e.
"  sy-tabix has the value 3 in the second loop pass.
"The example includes modifications of the table components. The 'num' value
"is assigned the table index value after inserting the new line. The
"'num' value of existing table lines includes the value of the index before
"inserting the new line + 1.

itab = itab_original.
FIELD-SYMBOLS <line> TYPE s_loop_mod.
DATA new_line_counter TYPE i.
DATA tabix_copy TYPE i.

LOOP AT itab ASSIGNING <fs>.
  tabix = sy-tabix.
  new_line_counter += 1.

  "Asserting that sy-tabix value has changed accordingly.
  IF tabix <> 1.
    ASSERT tabix = tabix_copy + 2.
  ENDIF.

  DATA(new_line_text) = |---- New line { new_line_counter } ----|.
  INSERT VALUE #( text = new_line_text ) INTO itab INDEX tabix ASSIGNING <line>.

  DATA(idx_new) = line_index( itab[ text = new_line_text num = 0 ] ).
  <line>-num = idx_new.

  DATA(idx_existing) = line_index( itab[ text = <fs>-text num = 0 ] ).
  DATA(new_text) = |{ <fs>-text }(existing line, index before insertion: { tabix })|.
  <fs>-text = new_text.
  <fs>-num = idx_existing.

  tabix_copy = tabix.
ENDLOOP.

*Internal table content:
*TEXT                                              NUM
*---- New line 1 ----                              1
*1 (existing line, index before insertion: 1)      2
*---- New line 2 ----                              3
*2 (existing line, index before insertion: 3)      4
*---- New line 3 ----                              5
*3 (existing line, index before insertion: 5)      6
*---- New line 4 ----                              7
*4 (existing line, index before insertion: 7)      8
*---- New line 5 ----                              9
*5 (existing line, index before insertion: 9)      10
*---- New line 6 ----                              11
*6 (existing line, index before insertion: 11)     12
*---- New line 7 ----                              13
*7 (existing line, index before insertion: 13)     14
*---- New line 8 ----                              15
*8 (existing line, index before insertion: 15)     16
*---- New line 9 ----                              17
*9 (existing line, index before insertion: 17)     18
*---- New line 10 ----                             19
*10 (existing line, index before insertion: 19)    20

*&---------------------------------------------------------------------*
*& Deleting a line before the current line
*&---------------------------------------------------------------------*

"The example explores the deletion of a line before the currently
"processed line. The previous line in the table is deleted if
"the value of 'text' (an integer was inserted) is an even number.
"The DELETE statement specifies the index with the current sy-tabix
"value - 1. On deletion, the sy-tabix value is decreased accordingly.
"Before a potential deletion, the currently processed table line is
"copied to another table to visualize the current sy-tabix value in
"the 'num' component.

itab = itab_original.
DATA itab_copy LIKE itab.

LOOP AT itab ASSIGNING <fs>.
    tabix = sy-tabix.

    <fs>-num = tabix.
    INSERT <fs> INTO TABLE itab_copy.

    TRY.
      IF CONV i( <fs>-text ) MOD 2 = 0.
        DELETE itab INDEX tabix - 1.
      ENDIF.
     CATCH cx_sy_conversion_no_number.
    ENDTRY.

ENDLOOP.

*Internal table content (itab):
*TEXT    NUM
*2       2
*4       3
*6       4
*8       5
*10      6

*Internal table content (itab_copy):
*TEXT    NUM
*1       1
*2       2
*3       2
*4       3
*5       3
*6       4
*7       4
*8       5
*9       5
*10      6

*&---------------------------------------------------------------------*
*& Deleting the currently processed table line
*&---------------------------------------------------------------------*

"The example explores deleting the currently processed table line using
"a string table. So, the DELETE statement specifies the current sy-tabix
"value for INDEX. In that case, the next line moves up one position, and
"the sy-tabix value remains the same, i.e. when sy-tabix is 2, and the
"line is deleted, the value remains 2 and processes the line moved up.

"Creating and populating a demo internal table
DATA(str_tab) = VALUE string_table( ( `a` ) ( `#` ) ( `c` ) ( `#` ) ( `e` )
                                    ( `f` ) ( `g` ) ( `#` ) ( `i` ) ( `j` ) ).

LOOP AT str_tab REFERENCE INTO DATA(dref).
  tabix = sy-tabix.
  IF dref->* CS `#`.
    DELETE str_tab INDEX tabix.
  ENDIF.
ENDLOOP.

*Internal table content:
*a
*c
*e
*f
*g
*i
*j

*&---------------------------------------------------------------------*
*& Statements clearing the entire internal table are not allowed in loops
*&---------------------------------------------------------------------*

"The entire internal table cannot be deleted within loops.
"The following statements commented out are not possible.
LOOP AT str_tab REFERENCE INTO dref.
  "CLEAR str_tab.
  "str_tab = VALUE #( ).
ENDLOOP.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Modifying Internal Table Content

As mentioned above, you can modify the content of internal table lines directly in the context of `READ TABLE` and `LOOP AT` statements using field symbols and data reference variables. You can also use table expressions for direct modification (as also covered in section [Table Expressions](#table-expressions)). Note that the key fields of the primary table key of sorted and hashed tables are always read-only. If you try to modify a key field, a runtime error occurs. However, this is not checked until runtime.

### Modifying Read Table Lines

The following examples demonstrate direct modification of recently read table lines:
``` abap
"Declaring and populating demo internal tables
TYPES: BEGIN OF ty_struc,
          comp1 TYPE i,
          comp2 TYPE string,
          comp3 TYPE c LENGTH 3,
       END OF ty_struc.

DATA it_st TYPE TABLE OF ty_struc WITH NON-UNIQUE KEY comp1.
DATA it_so TYPE SORTED TABLE OF ty_struc WITH UNIQUE KEY comp1.
DATA it_ha TYPE HASHED TABLE OF ty_struc WITH UNIQUE KEY comp1.

it_st = VALUE #( ( comp1 = 1 comp2 = `AAAAAA` comp3 = 'bbb' )
                 ( comp1 = 2 comp2 = `CCCCCC` comp3 = 'ddd' )
                 ( comp1 = 3 comp2 = `EEEEEE` comp3 = 'fff' )
                 ( comp1 = 4 comp2 = `GGGGGG` comp3 = 'hhh' )
               ).

it_so = it_st.
it_ha = it_st.

*&---------------------------------------------------------------------*
*& Modifying internal table content by changing the content of READ 
*& TABLE statement target areas
*&---------------------------------------------------------------------*

"Reading table line into a target area
READ TABLE it_st INTO DATA(wa) INDEX 1.
READ TABLE it_so ASSIGNING FIELD-SYMBOL(<fs>) INDEX 2.
READ TABLE it_ha REFERENCE INTO DATA(dref) WITH TABLE KEY comp1 = 3. "No reading by index in case of hashed tables

*&---------------------------------------------------------------------*
*& Modification examples
*&---------------------------------------------------------------------*

"Modifying all non-key components using the VALUE operator and
"the BASE addition
<fs> = VALUE #( BASE <fs> comp2 = `IIIIII` comp3 = 'jjj' ).

"In the following example, the key value is assigned a new
"value. Key values are protected against change in case of key tables.
"A runtime error occurs.
"<fs> = VALUE #( comp1 = 5 comp2 = `IIIIII` comp3 = 'jjj' ).

dref->* = VALUE #( BASE dref->* comp2 = `KKKKKK` comp3 = 'lll' ).

"Same as above. Key values cannot be changed in this case.
"dref->* = VALUE #( comp1 = 5 comp2 = `MMMMMM` comp3 = 'nnn' ).

"Using a MODIFY statement outlined below for changing internal
"table content based on a read line in a work area
MODIFY TABLE it_st FROM VALUE #( BASE wa comp2 = `OOOOOO` comp3 = 'ppp' ).

"Modifying individual components
READ TABLE it_st INTO wa INDEX 2.
READ TABLE it_so ASSIGNING <fs> INDEX 3.
READ TABLE it_ha REFERENCE INTO dref WITH TABLE KEY comp1 = 4.

"Using VALUE/BASE
<fs> = VALUE #( BASE <fs> comp2 = `QQQQQQ` ).
dref->* = VALUE #( BASE dref->* comp2 = `RRRRRR` ).
MODIFY TABLE it_st FROM VALUE #( BASE wa comp2 = `SSSSSS` ).

"Using the component selector
<fs>-comp3 = 'ttt'.

READ TABLE it_st INTO wa INDEX 3.
wa-comp3 = 'uuu'.
MODIFY TABLE it_st FROM wa.

"Object component selector in case of dereferencing ...
dref->comp2 = `VVVVVV`.
"... which is a more comfortable option compared to using the
"dereferencing and component selector operators in the following way.
dref->*-comp3 = 'www'.

*&---------------------------------------------------------------------*
*& Modifying internal table content using table expressions
*&---------------------------------------------------------------------*

"Changing the entire table line of a standard table
"In standard tables, the key value change is allowed.
it_st[ 3 ] = VALUE #( comp1 = 9 comp2 = `XXXXXX` comp3 = 'yyy' ).
"As above, the sorted table is a key table having a unique key,
"therefore a write cannot be performed on the entire entry. Runtime
"errors can occur.
"it_so[ 3 ] = VALUE #( comp2 = `XXXXXX` comp3 = 'yyy' ).
"The same applies to hashed tables.
"it_ha[ comp2 = `OOOOOO` ] = VALUE #( comp2 = `XXXXXX` comp3 = 'yyy' ).

"Changing individual components
it_st[ 3 ]-comp2 = `ZZZZZZ`.
it_so[ 3 ]-comp3 = 'A1'.
it_ha[ comp2 = `CCCCCC` ]-comp2 = `B2`.
"As above, no key field change in key tables. Allowed in standard
"tables.
"it_so[ 3 ]-comp1 = 10.
"it_ha[ comp2 = `AAAAAA` ]-comp1 = `C3`.
it_st[ 1 ]-comp1 = 99.

*&---------------------------------------------------------------------*
*& Modifying table content in all table rows in a loop
*&---------------------------------------------------------------------*

"For more syntax options regarding loops, check the section above.
"Target area: field symbol
LOOP AT it_st ASSIGNING FIELD-SYMBOL(<lo>).
  <lo>-comp2 = sy-tabix.
ENDLOOP.

*&---------------------------------------------------------------------*
*& Modifying table content restricting the rows that are looped across
*&---------------------------------------------------------------------*

"Target area: data reference variable
LOOP AT it_st reference into data(lo) FROM 2 TO 3.
  lo->comp3 = sy-tabix.
ENDLOOP.

"Target area: work area
LOOP AT it_so into data(wa_lo) where comp1 < 4.
  wa_lo-comp2 = sy-tabix.
  MODIFY TABLE it_so FROM wa_lo.
ENDLOOP.
```

### Modifying Table Lines Using ABAP MODIFY Statements

[`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_itab.htm)
statements provide multiple ways of changing the content of single and multiple table lines by specifying the table key or a table index,
without first reading the lines into a target area. Do not confuse the ABAP statement `MODIFY` with the ABAP SQL statement [`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPMODIFY_DBTAB.html).

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

> [!NOTE] 
> - The system field `sy-subrc` is set to `0` if at least one line was changed. It is set to `4` if no lines were changed.
> - `MODIFY`, `DELETE`, and `INSERT` statements can be specified with and without the `TABLE` addition. With `TABLE` means an index access. Without `TABLE` means an access via the table key.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Deleting Internal Table Content

You can use [`DELETE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdelete_itab.htm) statements to delete single and multiple lines in internal tables. The following additions can be used: `USING KEY` (for specifying a table key), `FROM`/`TO` (for specifying row ranges), `STEP` (for specifying the step size), and `WHERE` (for specifying conditions).

``` abap
*&---------------------------------------------------------------------*
*& Deleting via index 
*&---------------------------------------------------------------------*

"Example: The first line in the table is deleted.
DELETE it INDEX 1.

"If USING KEY is not used, INDEX can only be used with index tables.
"If doing so, it determines the line from the primary table index.
"If a secondary key is specified, the secondary table index is respected
"Example: same as above
DELETE it INDEX 1 USING KEY primary_key.

"Deleting an index range; FROM or TO alone can also be specified
DELETE it FROM 2 TO 5.

*&---------------------------------------------------------------------*
*& Deleting via keys
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& Deleting multiple lines based on a WHERE condition
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& Deleting the current line inside a LOOP statement
*&---------------------------------------------------------------------*

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

> [!NOTE] 
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
- Note that sorting is unstable by default concerning preserving the order of table lines with identical sort keys. Sorting results can vary if the table is sorted multiple times.
- You can also sort dynamically. For more information, refer to the [Dynamic Programming](06_Dynamic_Programming.md) cheat sheet.


<table>
<tr>
<td> Subject </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> Sorting by primary table key </td>
<td>

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
Plus: Suppose there are only elementary numeric components in an internal table with a structured line type. In this case, sorting has no effect because the primary table key is considered empty. This is also applies to tables declared with `EMPTY KEY`.

</td>
</tr>

<tr>
<td> Sorting by explicitly specifying components </td>
<td>

You can sort by any component of the internal table. It is also possible to specify the sort order 
(even component-wise). Explicitly specifying the components has the advantage that your code is easier to understand and you can avoid unexpected results if you accidentally use `SORT` without the `BY` addition on empty and standard table keys.

<br>

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

</td>
</tr>

<tr>
<td> Stable sorting </td>
<td>

By default, the sorting is unstable concerning preserving the order of table lines with identical sort keys. To ensure a stable sorting with preserving the order of table lines with identical 
sort keys, you can use the `STABLE` addition.<br><br>
This code example explores the difference in sorting results when using `STABLE` versus not using it with the `SORT` statement. A `SELECT` statement retrieves data from an internal table, specifying an `ORDER BY` clause for two columns. `SORT` statements sort the internal table. Using `STABLE` demonstrates that the relative order of the columns specified in the `ORDER BY` clause is preserved. Without `STABLE`, this relative order is not necessarily maintained. The `CL_ABAP_DIFF` class is used to compare the internal table content.

<br>

``` abap
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
    TYPES: BEGIN OF demo_flights,
             carrid   TYPE c LENGTH 3,
             connid   TYPE n LENGTH 4,
             cityfrom TYPE c LENGTH 20,
             cityto   TYPE c LENGTH 20,
           END OF demo_flights,
           ty_flights TYPE TABLE OF demo_flights WITH EMPTY KEY.

    DATA(itab) = VALUE ty_flights(
      ( carrid = 'AA' connid = '0017' cityfrom = 'NEW YORK' cityto = 'SAN FRANCISCO' )
      ( carrid = 'AA' connid = '0064' cityfrom = 'SAN FRANCISCO' cityto = 'NEW YORK' )
      ( carrid = 'AZ' connid = '0555' cityfrom = 'ROME' cityto = 'FRANKFURT' )
      ( carrid = 'AZ' connid = '0788' cityfrom = 'ROME' cityto = 'TOKYO' )
      ( carrid = 'AZ' connid = '0789' cityfrom = 'TOKYO' cityto = 'ROME' )
      ( carrid = 'AZ' connid = '0790' cityfrom = 'ROME' cityto = 'OSAKA' )
      ( carrid = 'DL' connid = '0106' cityfrom = 'NEW YORK' cityto = 'FRANKFURT' )
      ( carrid = 'DL' connid = '1699' cityfrom = 'NEW YORK' cityto = 'SAN FRANCISCO' )
      ( carrid = 'DL' connid = '1984' cityfrom = 'SAN FRANCISCO' cityto = 'NEW YORK' )
      ( carrid = 'JL' connid = '0407' cityfrom = 'TOKYO' cityto = 'FRANKFURT' )
      ( carrid = 'JL' connid = '0408' cityfrom = 'FRANKFURT' cityto = 'TOKYO' )
      ( carrid = 'LH' connid = '0400' cityfrom = 'FRANKFURT' cityto = 'NEW YORK' )
      ( carrid = 'LH' connid = '0401' cityfrom = 'NEW YORK' cityto = 'FRANKFURT' )
      ( carrid = 'LH' connid = '0402' cityfrom = 'FRANKFURT' cityto = 'NEW YORK' )
      ( carrid = 'LH' connid = '2402' cityfrom = 'FRANKFURT' cityto = 'BERLIN' )
      ( carrid = 'LH' connid = '2407' cityfrom = 'BERLIN' cityto = 'FRANKFURT' )
      ( carrid = 'QF' connid = '0005' cityfrom = 'SINGAPORE' cityto = 'FRANKFURT' )
      ( carrid = 'QF' connid = '0006' cityfrom = 'FRANKFURT' cityto = 'SINGAPORE' )
      ( carrid = 'SQ' connid = '0988' cityfrom = 'SINGAPORE' cityto = 'TOKYO' )
      ( carrid = 'UA' connid = '0941' cityfrom = 'FRANKFURT' cityto = 'SAN FRANCISCO' )
      ( carrid = 'UA' connid = '3504' cityfrom = 'SAN FRANCISCO' cityto = 'FRANKFURT' )
      ( carrid = 'UA' connid = '3516' cityfrom = 'NEW YORK' cityto = 'FRANKFURT' )
      ( carrid = 'UA' connid = '3517' cityfrom = 'FRANKFURT' cityto = 'NEW YORK' ) ).

    SELECT carrid, connid, cityfrom, cityto
           FROM @itab AS tab
           ORDER BY carrid, connid
           INTO TABLE @DATA(flights).

    DATA(fl_stable) = flights.
    DATA(fl_non_stable) = flights.

    SORT fl_stable STABLE BY cityfrom cityto.
    SORT fl_non_stable BY cityfrom cityto.

    DATA is_identical TYPE abap_boolean.
    DATA(comparison) = cl_abap_diff=>create( ).
    TRY.
        DATA(comp_result) = comparison->diff( EXPORTING target = fl_stable
                                                        source = fl_non_stable
                                              IMPORTING flag_identical = is_identical ).
        IF is_identical = abap_true.
          out->write( `Comparison result: Identical` ).
        ELSE.
          out->write( `Comparison result: Not identical` ).
        ENDIF.
      CATCH cx_abap_diff INTO DATA(error).
        out->write( error->get_text( ) ).
    ENDTRY.
    out->write( comp_result ).
  ENDMETHOD.
ENDCLASS.
```

</td>
</tr>

</table>

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

The example class below demonstrates internal table grouping options. To try it out, create a demo class named `zcl_demo_abap` and paste the following code into it. After activation, choose *F9* in ADT to execute the class. The example is designed to display results in the console.

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

The following example compares a LOOP AT statement with a FOR loop in a constructor expression. The goal is to highlight their syntactical similarities and differences, such as the greater flexibility of `LOOP AT` statements in terms of additional specifications within the loop and the common use of `FOR` loops to create an internal table.


```abap
SELECT *
    FROM zdemo_abap_flsch
    INTO TABLE @DATA(fl_tab).

DATA memb LIKE fl_tab.
TYPES tab_type LIKE memb.
DATA member_tab TYPE TABLE OF tab_type WITH EMPTY KEY.
DATA tab TYPE string_table.

"-------------------------- LOOP AT statement --------------------------
LOOP AT fl_tab INTO DATA(flight)
    GROUP BY ( carrier = flight-carrid cityfr = flight-cityfrom
                size = GROUP SIZE index = GROUP INDEX )
                ASCENDING REFERENCE INTO DATA(group_ref).

    APPEND |carrier = "{ group_ref->carrier }", cityfr = "{ group_ref->cityfr }", size = "{ group_ref->size }", index = "{ group_ref->index }" | TO tab.

    CLEAR memb.
    LOOP AT GROUP group_ref ASSIGNING FIELD-SYMBOL(<flight>).
    memb = VALUE #( BASE memb ( <flight> ) ).
    ENDLOOP.

    APPEND memb TO member_tab.
ENDLOOP.

"-------------------------- FOR loop --------------------------
TYPES: BEGIN OF demo_struc,
            member LIKE memb,
            info   TYPE string,
        END OF demo_struc.

TYPES ty_itab_constr TYPE TABLE OF demo_struc WITH EMPTY KEY.

DATA(itab_constr) = VALUE ty_itab_constr( FOR GROUPS g OF fl IN fl_tab
        GROUP BY ( c = fl-carrid cifr = fl-cityfrom
                    size = GROUP SIZE index = GROUP INDEX ) ASCENDING
        LET m = VALUE tab_type( FOR <fl> IN GROUP g ( <fl> ) ) IN
        ( member = m info = |carrid = "{ g-c }", cityfrom = "{ g-cifr }", size = "{ g-size }", index = "{ g-index }" | ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Collecting Values

- You can use `COLLECT` statements, for example, to add the values of numeric components to the corresponding values in an internal table. 
- It is recommended that you use it mainly for internal tables with a unique primary key, especially hashed tables.
- Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcollect.htm)

``` abap
"This example demonstrates how to insert data from a database table
"into an internal table in a compressed way. Within a SELECT loop,
"a COLLECT statement is used to consolidate lines with identical
"primary key components (carrid and connid) by summing the number
"of occupied seats in the numeric component (seatsocc).
"Additionally, an internal table is filled by adding all read lines.
"This table is looped across to simulate the effect of the COLLECT
"statement.

DATA: BEGIN OF seats,
        carrid   TYPE zdemo_abap_fli-carrid,
        connid   TYPE zdemo_abap_fli-connid,
        seatsocc TYPE zdemo_abap_fli-seatsocc,
        END OF seats,
        seats_tab_col LIKE HASHED TABLE OF seats WITH UNIQUE KEY carrid connid,
        seats_tab_all LIKE TABLE OF seats WITH EMPTY KEY,
        seats_tab_loop_grp LIKE seats_tab_col.

SELECT carrid, connid, seatsocc
        FROM zdemo_abap_fli
        INTO @seats.
    COLLECT seats INTO seats_tab_col.
    APPEND seats TO seats_tab_all.
ENDSELECT.

LOOP AT seats_tab_all INTO DATA(wa) GROUP BY ( key1 = wa-carrid key2 = wa-connid ).
    INSERT VALUE #( carrid = wa-carrid connid = wa-connid ) INTO TABLE seats_tab_loop_grp ASSIGNING FIELD-SYMBOL(<fs>).
    LOOP AT GROUP wa INTO DATA(member).
    <fs>-seatsocc = <fs>-seatsocc + member-seatsocc.
    ENDLOOP.
ENDLOOP.

ASSERT seats_tab_loop_grp = seats_tab_col.
```

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
"Read using a key
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

"Note: No primary table index with hashed tables
DATA(hashed_tab) = VALUE string_hashed_table( ( `a` ) ( `b` ) ( `c` ) ).
"-1
DATA(hashed_primary_idx) = line_index( hashed_tab[ table_line = `c` ] ).

"Index access in hashed tables only using a secondary table index
TYPES: BEGIN OF s,
          comp1 TYPE i,
          comp2 TYPE i,
        END OF s,
        ttype TYPE HASHED TABLE OF s WITH UNIQUE KEY comp1 WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp2.

DATA(hashed_tab2) = VALUE ttype( ( comp1 = 1 comp2 = 10 )
                                 ( comp1 = 2 comp2 = 8 )
                                 ( comp1 = 3 comp2 = 9 ) ).

"3
DATA(hashed_secondary_idx) = line_index( hashed_tab2[ KEY sk comp2 = 10 ] ).
"1
hashed_secondary_idx = line_index( hashed_tab2[ KEY sk comp2 = 8 ] ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Checking How Many Lines Exist in an Internal Table

`lines( )` is another built-in function that you can use to check how many lines exist in an internal table. It returns an integer value.

``` abap
DATA(itab) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ( `d` ) ( `e` ) ).

"5
DATA(number_of_lines) = lines( itab ).

"Excursion: Finding out the number of lines in a table by specifying concrete
"component values, e.g. you want to find out how many lines exist in the table 
"that have the value 1 for comp2
TYPES: BEGIN OF struct,
          comp1 TYPE c LENGTH 3,
          comp2 TYPE i,
        END OF struct,
        tab_type TYPE TABLE OF struct WITH EMPTY KEY.

DATA(it) = VALUE tab_type( ( comp1 = 'a' comp2 = 1  )
                           ( comp1 = 'b' comp2 = 1  )
                           ( comp1 = 'c' comp2 = 1  )
                           ( comp1 = 'd' comp2 = 2  )
                           ( comp1 = 'e' comp2 = 3  )
                           ( comp1 = 'f' comp2 = 4  )
                           ( comp1 = 'g' comp2 = 5  ) ).

"7
DATA(line_num) = lines( it ).

"Finding out the number of lines in a table by component value, e.g.
"using constructor expressions and specifying a WHERE clause.
"The example creates an new internal table inline using VALUE and a FOR loop,
"specified with a WHERE clause. The lines function is applied to the
"table created inline.
"3
DATA(line_num_filtered1) = lines( VALUE tab_type( FOR wa IN it WHERE ( comp2 = 1 ) ( wa ) ) ).

"Using the REDUCE operator
"The example adds 1 to the resulting integer if the comp2 value of the iterated line is greater than 1.
"The lines function is not relevant in the example.
"4
DATA(line_num_filtered2) = REDUCE i( INIT var = 0
                                     FOR <line> IN it
                                     WHERE ( comp2 > 1 )
                                     NEXT var += 1 ).

"Using the FILTER operator
"Note: The source table must have at least one sorted key or a hash key for accessing.
"If the table does not have such a primary table key, a secondary table key must be available.
TYPES: tab_type_sorted TYPE TABLE OF struct with NON-UNIQUE SORTED KEY sec_key COMPONENTS comp2.
DATA it_sorted type tab_type_sorted.
it_sorted = it.

"The example creates an new internal table inline using FILTER,
"specified with a WHERE clause. The lines function is applied to the
"table created inline.
"3
DATA(line_num_filtered3) = lines( FILTER #( it_sorted USING KEY sec_key WHERE comp2 = 1 ) ).
"4
DATA(line_num_filtered4) = lines( FILTER #( it_sorted USING KEY sec_key WHERE comp2 > 1 ) ).

"Using LOOP statements
CLEAR number_of_lines.
"No WHERE condition as all lines shall be processed
"7
LOOP AT it REFERENCE INTO DATA(line).
  number_of_lines += 1.
ENDLOOP.

CLEAR number_of_lines.
"3
LOOP AT it transporting no fields where comp2 = 1.
  number_of_lines += 1.
ENDLOOP.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Getting Table (Type) Information at Runtime

Using [Runtime Type Identification (RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm "Glossary Entry"),
you can get type information on internal tables and table types at runtime. 

For more information, see the [Dynamic Programming](06_Dynamic_Programming.md) ABAP cheat sheet.

RTTI example: 
```abap
TYPES tab_type TYPE SORTED TABLE OF zdemo_abap_flsch
       WITH UNIQUE KEY carrid connid
       WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS countryfr cityto.
DATA itab TYPE tab_type.

DATA(tdo_d) = cl_abap_typedescr=>describe_by_data( itab ).
"DATA(tdo_d) = cl_abap_typedescr=>describe_by_name( 'TAB_TYPE' ).

"Cast to get more specific information
DATA(tdo_itab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab ) ).
"DATA(tdo_itab) = CAST cl_abap_tabledescr( tdo_d ).

DATA(type_category_itab) = tdo_itab->kind.
DATA(relative_name_itab) = tdo_itab->get_relative_name( ).
... "Explore more options by positioning the cursor behind -> and choosing CTRL + Space
DATA(table_kind_itab) = tdo_itab->table_kind.
DATA(table_keys_itab) = tdo_itab->key.
DATA(table_keys_more_details_itab) = tdo_itab->get_keys( ).
DATA(table_has_unique_key_itab) = tdo_itab->has_unique_key.
DATA(table_key_alias_itab) = tdo_itab->get_key_aliases( ).
DATA(line_type_itab) = tdo_itab->get_table_line_type( ).
DATA(table_component_info_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) ).
DATA(table_components_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) )->components.
DATA(table_comps_more_info_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) )->get_components( ).
DATA(applies_to_data_itab) = tdo_itab->applies_to_data( VALUE tab_type( ) ).
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

> [!NOTE] 
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

**General rule**: Use `SELECT` with internal tables as a data source only when SQL functionality, such as joins, exceeds ABAP statements.

**Technical considerations**:
- The [ABAP SQL in-memory engine](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_inmemeng_glosry.htm) manages read access with ABAP SQL `SELECT` statements for tabular data within the memory of an [Application Server ABAP (AS ABAP)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenas_abap_glosry.htm). The tabular data can include:
  - Database table data (such as data from DDIC database tables or CDS entities) buffered in the [table buffer](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenas_abap_glosry.htm) within AS ABAP. Buffering ability is determined by specifications in the artifacts. If the data isn't buffered, the engine can't handle it, and the SQL statement is processed on the database.
  - Internal tables present in the current internal session. They are treated like DDIC database tables. ABAP types are mapped to corresponding [built-in DDIC types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_ddic_type_glosry.htm).
- The ABAP SQL in-memory engine processes data on AS ABAP, not the database server. If a `SELECT` statement using internal tables as data sources includes elements beyond the engine's capability (for example, most subqueries aren't supported; see detailed restrictions [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENSQL_ENGINE_RESTR.html)), the internal table content is transferred to a temporary database table before the query executes. Only components involved in the read access are transferred.
- In such cases, the SQL statement runs directly on the database server, not within AS ABAP. If the compiler identifies a statement the ABAP SQL in-memory engine can't handle, a syntax warning occurs. You can suppress this warning using the pragma `##itab_db_select`.
- Currently, queries with multiple internal tables can only proceed if the ABAP SQL in-memory engine can manage them on AS ABAP directly. Transferring more than one internal table to the database is currently not supported.
- The result of the ABAP SQL in-memory engine processing is identical to processing the read access directly on the database.

**Using internal tables as data sources in ABAP SQL SELECT statements**:

- You must specify internal tables as [host variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhost_variable_glosry.htm) with an `@` prefix and give them an alias.
   ``` abap
   SELECT comp1, comp2, ...
      FROM @itab AS it_alias
      WHERE ...
      INTO TABLE @DATA(itab_sel).
   ```
- Internal tables are treated like DDIC database tables, which leads to specific behaviors such as:
  - They are handled like [client-independent](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclient_independent_glosry.htm) database tables, and the first table column is not automatically considered a client column. You can change this default behavior using the `DECLARE CLIENT` addition. For example, if the internal table specifies a key that is not specified at the beginning of the line type, you cannot use `SELECT` to retrieve data from the table. 
  ```abap
  TYPES: BEGIN OF s,
          comp1 TYPE i,
          comp2 TYPE i,
          comp3 TYPE i,
         END OF s.
  DATA itab TYPE TABLE OF s WITH KEY comp3.
  "SELECT SINGLE * FROM @itab AS tab INTO @DATA(line).
  ```
  - You cannot use deep and nested components of internal tables in `SELECT` statements.
  - The type string is supported only if declared as a reference to the built-in dictionary type `sstring`.
- A key advantage of using `SELECT` statements with internal tables as data sources is the access to ABAP SQL's extensive functionalities, like aggregate expressions.
- They can serve as an alternative to the `READ TABLE` or `LOOP AT` statements if the ABAP SQL in-memory engine can process the data without requiring a database transfer.
- Note the general rule: You should use `SELECT` with internal tables as a data source only when SQL functionality exceeds that of ABAP statements, such as in joins. For tasks achievable with ABAP statements, it is preferable to use them as they are optimized for internal tables and offer better performance.

The following example finds duplicate entries in internal tables. It uses a `SELECT` statement with the internal table as the data source. However, since the statement executes on the database, a syntax warning appears unless suppressed by the pragma. An advisable alternative is the internal-table-specific `LOOP AT` statement, which is optimized for operations on internal tables.


```abap
TYPES: BEGIN OF s_demo,
          comp1 TYPE i,
          comp2 TYPE c LENGTH 3,
        END OF s_demo,
        ty_tab_demo TYPE TABLE OF s_demo WITH EMPTY KEY.

DATA(it) = VALUE ty_tab_demo(
  ( comp1 = 1 comp2 = 'A' )
  ( comp1 = 1 comp2 = 'B' )
  ( comp1 = 1 comp2 = 'C' )
  ( comp1 = 2 comp2 = 'A' )
  ( comp1 = 2 comp2 = 'E' )
  ( comp1 = 1 comp2 = 'D' )
  ( comp1 = 3 comp2 = 'D' )
  ( comp1 = 3 comp2 = 'G' )
  ( comp1 = 4 comp2 = 'C' )
  ( comp1 = 5 comp2 = 'A' ) ).

"------------------ SELECT ------------------
"Note: The SELECT command is executed on the database. Therefore, the pseudo
"comment is include to suppress a syntax warning.
SELECT comp1, COUNT(*) AS count
  FROM @it AS it
  GROUP BY comp1
  HAVING COUNT(*) > 1
  INTO TABLE @DATA(duplicates_sql) ##ITAB_DB_SELECT.

"------------------ LOOP AT ------------------ 
TYPES: BEGIN OF duplicates_struc,
          comp1 TYPE s_demo-comp1,
          count TYPE i,
        END OF duplicates_struc,
        ty_tab_duplicates TYPE TABLE OF duplicates_struc WITH EMPTY KEY.

DATA duplicates_loop_at TYPE ty_tab_duplicates.
LOOP AT it INTO DATA(wa) GROUP BY ( comp = wa-comp1 size = GROUP SIZE ) ASCENDING REFERENCE INTO DATA(group_ref).

  IF group_ref->size > 1.
    APPEND VALUE #( comp1 = group_ref->comp count = group_ref->size ) TO duplicates_loop_at.
  ENDIF.

ENDLOOP.
```


**More information**:
- [Restrictions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_engine_restr.htm)
- See details on the various ABAP SQL functionalities in the ABAP Keyword Documentation and in the [ABAP SQL cheat sheet](03_ABAP_SQL.md). 

The following example explores various `SELECT` queries with internal tables as data sources. To try it out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example uses objects of the ABAP cheat sheets repository and is set up to display output in the console.

Example: 

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


#### Restrictions Regarding Internal Tables as Data Sources in ABAP SQL SELECT Statements

- This excursion is intended to underscore the restrictions mentioned above and in the [documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_engine_restr.htm) in more detail when selecting from internal tables. 
- Components having deep types cannot be included, for example, in the `SELECT` list or `WHERE` clause. 
- Among the non-allowed types of internal table components are strings (as they are deep types) and `utclong`.
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
*&---------------------------------------------------------------------*
*& Internal table having a component of type string
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& Internal table having a component typed with a CDS simple type (sstring)
*&---------------------------------------------------------------------*

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

#### Excursion: Joining/Merging Internal Tables into Internal Tables

The following code snippets demonstrate joining/merging the content of two simple internal tables into another table. There may be several ways to achieve this. Here, the intention is to give an idea and, in particular, to emphasize SQL functionalities also available for internal tables (note the restrictions mentioned above and in the documentation).

Assumptions:
- The target table is either created inline or exists and includes components from the source tables or components that can be mapped.
- One or more components are common between the source tables to perform a join or merge the table content.

The code snippet shows a selection of syntax options and includes the following statements:
- Joins with `SELECT` (`INNER JOIN`, `LEFT OUTER JOIN`, CTE)
- Loops with `LOOP AT` statements, `FOR` loops using the `VALUE` and `REDUCE` operators

```abap
"Creating two internal tables whose content will be joined. The shared
"value is represented by the key1 and key2 components.
"Sorted tables are used in the example (having key1/key2 as unique keys)
"to have unique values to perform joins.
TYPES: BEGIN OF s1,
            key1 TYPE i,
            a    TYPE c LENGTH 1,
            b    TYPE c LENGTH 1,
            c    TYPE c LENGTH 1,
        END OF s1,
        tab_type1 TYPE SORTED TABLE OF s1 WITH UNIQUE KEY key1,
        BEGIN OF s2,
            key2 TYPE i,
            d    TYPE c LENGTH 1,
            e    TYPE c LENGTH 1,
        END OF s2,
        tab_type2 TYPE SORTED TABLE OF s2 WITH UNIQUE KEY key2.

"Populating demo internal tables
DATA(itab1) = VALUE tab_type1( ( key1 = 1 a = 'a' b = 'b'  c = 'c' )
                               ( key1 = 2 a = 'd' b = 'e'  c = 'f' )
                               ( key1 = 3 a = 'g' b = 'h'  c = 'i' ) ).

DATA(itab2) = VALUE tab_type2( ( key2 = 1 d = 'j' e = 'k' )
                               ( key2 = 2 d = 'l' e = 'm' ) ).

"SELECT statement, inner join
"Note: With the inner join, the target table contains all
"combinations of rows for whose columns the join condition
"is true.
SELECT a~key1, a~a, a~b, b~d, b~e
    FROM @itab1 AS a
    INNER JOIN @itab2 AS b ON a~key1 = b~key2
    INTO TABLE @DATA(itab3).

*Result
*KEY1    A    B    D    E
*1       a    b    j    k
*2       d    e    l    m

"SELECT statement, left outer join
"In contrast to the inner join above, the target table here
"also contains the table row of the first table for which
"no equivalent row exists in the second table.
SELECT a~key1, a~a, a~b, b~d, b~e
    FROM @itab1 AS a
    LEFT OUTER JOIN @itab2 AS b ON a~key1 = b~key2
    INTO TABLE @DATA(itab4).

*Result
*KEY1    A    B    D    E
*1       a    b    j    k
*2       d    e    l    m
*3       g    h

*&---------------------------------------------------------------------*
*& Note: The following statements produce the same result as the
*& previous example (itab4).
*&---------------------------------------------------------------------*

"Common table expression
WITH +it1 AS ( SELECT a~key1, a~a, a~b FROM @itab1 AS a ),
     +it2 AS ( SELECT b~key2, b~d, b~e FROM @itab2 AS b )
SELECT +it1~key1, +it1~a, +it1~b, +it2~d, +it2~e FROM +it1 LEFT JOIN +it2 ON +it1~key1 = +it2~key2
INTO TABLE @DATA(itab5).

"LOOP statements
"Using the CORRESPONDING operator to assign identically named components, 
"BASE retains existing content
"The assignment with CORRESPONDING ... BASE ... includes a table expression
"in which table lines are read and inserted based on the key mapping. With the
"OPTIONAL addition, errors can be avoided if a line does not exist.
DATA itab6 LIKE itab4.
LOOP AT itab1 INTO DATA(wa1).
    INSERT CORRESPONDING #( wa1 ) INTO TABLE itab6 REFERENCE INTO DATA(ref).
    ref->* = CORRESPONDING #( BASE ( ref->* ) VALUE #( itab2[ key2 = ref->key1 ] OPTIONAL ) ).
ENDLOOP.
"Assume the second table's shared component was also key1. In the second CORRESPONDING
"you could then work with the EXCEPT addition to not overwrite the identically named
"component.

"Example similar to the previous one
"Also here, a table expression is used to read a line from
"the second internal table. The INSERT statement (without
"CORRESPONDING) includes the concrete value assignments
"with the VALUE operator.
DATA itab7 LIKE itab4.
LOOP AT itab1 INTO DATA(wa2).
    DATA(line) = VALUE #( itab2[ key2 = wa2-key1 ] OPTIONAL ).

    INSERT VALUE #( key1 = wa2-key1
                    a = wa2-a
                    b = wa2-b
                    d = line-d
                    e = line-e ) INTO TABLE itab7.
ENDLOOP.

"Example using a FOR loop with the VALUE operator
TYPES tt_type3 LIKE itab4.
DATA(itab8) = VALUE tt_type3( FOR wa3 IN itab1
                              ( key1 = wa3-key1
                                a = wa3-a
                                b = wa3-b
                                d = VALUE #( itab2[ key2 = wa3-key1 ]-d OPTIONAL )
                                e = VALUE #( itab2[ key2 = wa3-key1 ]-e OPTIONAL ) ) ).

"Similar example that includes a LET expression
DATA(itab9) = VALUE tt_type3( FOR wa4 IN itab1
                              LET tab_line = VALUE #( itab2[ key2 = wa4-key1 ] OPTIONAL ) IN
                              ( key1 = wa4-key1
                                a = wa4-a
                                b = wa4-b
                                d = tab_line-d
                                e = tab_line-e ) ).

"Example using a FOR loop with the REDUCE operator and LET
DATA(itab10) = REDUCE tt_type3( INIT tab = VALUE #( )
                                FOR wa5 IN itab1
                                LET tableline = VALUE #( itab2[ key2 = wa5-key1 ] OPTIONAL ) IN
                                NEXT tab = VALUE #( BASE tab
                                ( key1 = wa5-key1
                                  a = wa5-a
                                  b = wa5-b
                                  d = tableline-d
                                  e = tableline-e ) ) ).
```


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions

### Improving Read Performance with Secondary Table Keys

- Consider a scenario where you have a standard internal table that you access frequently using the primary table key or a free key. The table is declared without a secondary key. 
- In both cases, the read performance is slow because these accesses are not optimized in standard tables.
- You can add a secondary key to optimize access and improve read performance.
- The following example creates three demo internal tables: two without a secondary key, accessed by the primary key and a free key, and a third table declared with a secondary key. All tables are populated with many table lines. Then, in a `DO` loop, multiple reads are performed on the internal tables. Before and after the reads, the current timestamp is retrieved to evaluate runtime. Find additional details in the similarly structured example in the next section. The results should demonstrate a significant performance improvement for reads on an internal table using the secondary key.
- To try the example out, create a demo class named `zcl_demo_abap` and insert the provided code. After activation, choose *F9* in ADT to execute the class. The example is set up to display the results in the console.

> [!NOTE] 
> This example is for [exploration, experimentation, and demonstration](./README.md#%EF%B8%8F-disclaimer). It is not intended for accurate runtime or performance testing and is not a suitable method for such purposes. Due to its simplified nature, results may vary and not be entirely accurate, even across multiple runs.

<br>

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

```abap
"! <p class="shorttext"><strong>Improving Read Performance with Secondary Table Keys</strong><br/>
"! ABAP cheat sheet example class</p>
"!
"! <p>The example class includes several methods that explore both inefficient and potentially more
"! efficient code examples affecting performance.</p>
"! <p>In ADT, choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li>The example is for exploration, experimentation, and demonstration. It is not intended for accurate
"! runtime or performance testing and is not a suitable method for such purposes. Due to its simplified nature,
"! results may vary and not be entirely accurate, even across multiple runs.</li>
"! <li>Note the disclaimer in the ABAP cheat sheet repository.</li></ul>
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      primary_table_key RETURNING VALUE(result) TYPE decfloat34,
      free_key RETURNING VALUE(result) TYPE decfloat34,
      secondary_table_key RETURNING VALUE(result) TYPE decfloat34,
      store_runtime IMPORTING high TYPE utclong low  TYPE utclong,
      get_avg_runtime RETURNING VALUE(avg_runtime) TYPE decfloat34.

    "Demo internal table
    TYPES: BEGIN OF demo_struc,
             comp1 TYPE i,
             comp2 TYPE c LENGTH 20,
             comp3 TYPE c LENGTH 20,
             comp4 TYPE c LENGTH 20,
             comp5 TYPE c LENGTH 20,
             comp6 TYPE c LENGTH 20,
             comp7 TYPE c LENGTH 20,
           END OF demo_struc.

    DATA: itab_empty_key       TYPE TABLE OF demo_struc WITH EMPTY KEY,
          itab_w_primary_key   TYPE TABLE OF demo_struc WITH NON-UNIQUE KEY comp1 comp2 comp3,
          itab_w_secondary_key TYPE TABLE OF demo_struc WITH EMPTY KEY WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp1 comp2 comp3.

    DATA: ts1         TYPE utclong,
          runtime_tab TYPE TABLE OF decfloat34 WITH EMPTY KEY,
          description TYPE string,
          idx         TYPE i.

    CONSTANTS: loop_count TYPE i VALUE 5000.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Populating demo tables
    DO loop_count TIMES.
      INSERT VALUE #( comp1 = sy-index comp2 = sy-index comp3 = sy-index comp4 = sy-index
                      comp5 = sy-index comp6 = sy-index comp7 = sy-index ) INTO TABLE itab_empty_key.
    ENDDO.
    itab_w_primary_key = itab_empty_key.
    itab_w_secondary_key = itab_empty_key.

    DATA(res1) = primary_table_key( ).
    out->write( `Average runtime for reading lines of a standard table using the primary table key:` ).
    out->write( |{ res1 STYLE = SIMPLE }| ).

    DATA(res2) = free_key( ).
    out->write( `Average runtime for reading lines of a standard table using a free key:` ).
    out->write( |{ res2 STYLE = SIMPLE }| ).

    DATA(res3) = secondary_table_key( ).
    out->write( `Average runtime for reading lines of a standard table using the secondary table key:` ).
    out->write( |{ res3 STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD primary_table_key.
    "Reading lines from an internal table using the primary table key

    DO loop_count TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE itab_w_primary_key TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = idx comp2 = idx comp3 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    result = get_avg_runtime( ).
  ENDMETHOD.

  METHOD free_key.
    "Reading lines from an internal table using a free key
    "The free key corresponds to the primary and secondary table key components
    "of the other demo internal tables.

    DO loop_count TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE itab_empty_key TRANSPORTING NO FIELDS WITH KEY comp1 = idx comp2 = idx comp3 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    result = get_avg_runtime( ).
  ENDMETHOD.

  METHOD secondary_table_key.
    "Reading lines from an internal table using the secondary table key

    DO loop_count TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE itab_w_secondary_key TRANSPORTING NO FIELDS WITH TABLE KEY sk COMPONENTS comp1 = idx comp2 = idx comp3 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    result = get_avg_runtime( ).
  ENDMETHOD.

  METHOD store_runtime.
    cl_abap_utclong=>diff( EXPORTING high    = high
                                     low     = low
                           IMPORTING seconds = DATA(seconds) ).

    APPEND seconds TO runtime_tab.
  ENDMETHOD.

  METHOD get_avg_runtime.
    "Calculating the average runtime value

    DATA(sum) = REDUCE decfloat34( INIT s = VALUE #( )
                                   FOR wa IN runtime_tab
                                   NEXT s += wa ).

    TRY.
        avg_runtime = sum / lines( runtime_tab ).
      CATCH cx_sy_zerodivide.
    ENDTRY.

    CLEAR runtime_tab.
  ENDMETHOD.
ENDCLASS.
```

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example: Exploring Read Access Performance with Internal Tables

- The following example explores the performance of read accesses to internal tables of various kinds in a simplified and self-contained manner. 
- The scenario is: Lines of a large internal table (2000 table lines) are read frequently. 
- The read accesses are executed using `READ TABLE` statements, which access the tables by: 
  - Index (both primary and secondary table index) 
  - Key (primary table key, secondary table key, free key)
- To try the example out, create a demo class named `zcl_demo_abap` and insert the provided code. After activation, choose *F9* in ADT to execute the class. It may take some time to finish and display the output. The example is set up to display the results in the console.

> [!NOTE] 
> - This example is for [exploration, experimentation, and demonstration](./README.md#%EF%B8%8F-disclaimer) purposes only. It is not suitable for accurate runtime or performance testing. Due to its simplified nature (for example, the table line type is fairly simple, and the key components are all of type `i`) and various factors that can influence the runtime of an ABAP program, results may vary and may not be entirely accurate, even across multiple runs. The displayed results may not always accurately reflect the performance notes from the cheat sheet.
> - The example concentrates on a few demo internal tables, declared using various declaration options. 
> - The runtime analysis in this self-contained example is as follows: The code snippet for which the runtime should be measured is enclosed by two timestamp retrievals using `utclong_current( )`. The runtime is analysed by calculating the delta of the two timestamp values, indicating the used runtime for the code snippet. To have a more meaningful example regarding the runtime analysis, the snippets are executed multiple times, indicated by enclosing them in a `DO` loop. The runtime delta value is added to an internal table collecting all runtime delta values for the specific code snippet for all loop iterations. Finally, the average runtime is calculated based on the collected runtime delta values. This average runtime value is added to an internal table with other information. The approach with multiple iterations and the average runtime value aims to provide a balanced view of runtime. Note that this implementation is for exploration and demonstration purposes only.
> - Among the demonstrated read operations are the following:
>   - Index accesses (standard and sorted tables, hashed tables using the secondary table index)
>   - Primary table key accesses (various tables specified with a single and multiple key components; examples with full key, partial and left-aligned key, partial and not left-aligned key)
>   - Secondary table key access
>   - Free key access
>   - Dynamic key specifications
> - The following points should be reflected by the result: 
>   - Index access is generally very fast.
>   - (Full) primary table key access in hashed tables is fast.
>   - Standard tables with specified secondary keys offer optimized access.
>   - Both full and partial, left-aligned key accesses in sorted tables are optimized.
>   - Free key searches are generally slower (unless used with the BINARY SEARCH addition in standard tables).
>   - Key access in standard tables is relatively slow, except for secondary table keys.
>   - Dynamic key specifications require effort for evaluation.
> - The purpose of this example is to underscore the significance of choosing the right table categories for your internal tables, tailored to your specific use case and the frequency of table access.


<details>
  <summary>🟢 Click to expand for general notes</summary>
  <!-- -->

<br>

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
  - In sorted tables, optimized access is also possible for partial primary table key specifications. As a prerequisite, the key specification must be left-aligned and gap-free. For example, if the primary table key consists of components `a`, `b`, and `c`, optimized access is valid if only `a` and `b` are specified. If the keys are not left-aligned, such as in the case where `b` and `c` are specified without `a`, optimized access is not applicable, and a linear search will be used.
- Generally, the larger the table, the slower the key access. The benefit of hash tables is that they offer constant access time, even for very large tables.
- Sorted and standard tables have a primary table index, hence their designation as index tables. 
- The primary table index assigns a unique line number to each table line. 
  - The index updates whenever a line is added or removed. 
  - Unlike key access, access time via the index does not increase linearly with table size.

... secondary table key and secondary table index:
- Secondary table keys, which can be sorted or hashed, are available for all table categories. 
- They enhance table access efficiency and performance. 
- Declaring a secondary table key generates a corresponding secondary table index. However, the index for a non-unique key does not update immediately upon adding or deleting a line (for unique keys, it does). The update happens when the internal table is accessed using the secondary table key. 
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
  - Access is fast by index, primary table key (always optimized; also optimized for an incomplete, left-aligned and gap-free primary table key specification), secondary table key (also optimized)

- Hashed tables: 
  - Suitable for ...
    - very large tables that are filled once and rarely altered 
    - tables where key-based access is the primary method 
    - tables that do not require index access on the primary table index 
  - Access is ... 
    - fast and optimized for both primary and secondary table keys 
    - consistent for large internal tables due to a special hash algorithm


</details> 

<br>

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

```abap
"! <p class="shorttext"><strong>Exploring Read Access Performance with Internal Tables</strong><br/>
"! ABAP cheat sheet example class</p>
"!
"! <p>The example class includes several methods that explore both inefficient and potentially more
"! efficient code examples affecting performance.</p>
"! <p>In ADT, choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li>The example is for exploration, experimentation, and demonstration. It is not intended for accurate
"! runtime or performance testing and is not a suitable method for such purposes. Due to its simplified nature,
"! results may vary and not be entirely accurate, even across multiple runs.</li>
"! <li>Note the disclaimer in the ABAP cheat sheet repository.</li></ul>
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    "Constants
    CONSTANTS loop_count_2000 TYPE i VALUE 2000.

    "Components used for calculating the average runtime
    DATA: ts1         TYPE utclong,
          runtime_tab TYPE TABLE OF decfloat34 WITH EMPTY KEY,
          runtime     TYPE decfloat34,
          description TYPE string,
          idx         TYPE i.

    "Collects timestamps to calculate the average runtime
    METHODS store_runtime IMPORTING high TYPE utclong
                                    low  TYPE utclong.

    "Calculates the average runtime
    METHODS get_avg_runtime RETURNING VALUE(avg_runtime) TYPE decfloat34.

    TYPES: BEGIN OF ENUM enum_access,
             index,
             primary_table_key,
             secondary_table_key,
             free_key,
             secondary_table_index,
           END OF ENUM enum_access.

    TYPES: BEGIN OF ENUM enum_category,
             standard,
             sorted,
             hashed,
           END OF ENUM enum_category.

    TYPES: BEGIN OF result_struc,
             meth_name       TYPE string,
             avg_runtime     TYPE string,
             table_category  TYPE enum_category,
             access          TYPE enum_access,
             description     TYPE string,
             iteration_count TYPE i,
           END OF result_struc.

    DATA result_meth TYPE result_struc.
    DATA result_tab TYPE TABLE OF result_struc WITH EMPTY KEY.

    "Exemplary methods containing READ TABLE statements
    METHODS:
      "Standard tables
      std_01_index IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      std_02_pr_key_single_comp IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      std_03_mult_key_full IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      std_04_mult_key_partial_la IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      std_05_mult_key_partial_non IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      std_06_mult_key_dynamic IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      std_07_free_key IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      std_08_free_key_binary_search IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      std_09_sec_key_nonuni_sorted IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      std_10_sec_key_nonuni_sort_idx IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      std_10_sec_key_unique_sorted IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      std_11_sec_key_unique_hashed IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      "Sorted tables
      sorted_01_index IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      sorted_02_pr_key_single_comp IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      sorted_03_mult_key_full IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      sorted_04_mult_key_partial_la IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      sorted_05_mult_key_partial_non IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      sorted_06_mult_key_dynamic IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      sorted_07_free_key IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      "Hashed tables
      hashed_01_pr_key_single_comp IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      hashed_02_mult_key_full IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      hashed_03_mult_key_partial_la IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      hashed_04_mult_key_partial_non IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      hashed_05_mult_key_dynamic IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      hashed_06_free_key IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc,
      hashed_07_sec_key_nonun_so_idx IMPORTING name TYPE abap_methname RETURNING VALUE(result) TYPE result_struc.

    "Demo internal tables used by the examples
    TYPES: BEGIN OF ty_tab,
             comp1 TYPE i,
             comp2 TYPE i,
             comp3 TYPE i,
             comp4 TYPE c LENGTH 20,
             comp5 TYPE c LENGTH 20,
             comp6 TYPE c LENGTH 20,
             comp7 TYPE i,
           END OF ty_tab.

    CLASS-DATA: "Standard tables
      standard_tab                  TYPE TABLE OF ty_tab WITH NON-UNIQUE KEY comp1,
      standard_tab_mult_key_comp    TYPE TABLE OF ty_tab WITH NON-UNIQUE KEY comp1 comp2 comp3,
      "standard_tab_empty_key        TYPE TABLE OF ty_tab WITH EMPTY KEY,
      standard_tab_non_uni_so_sec_k TYPE TABLE OF ty_tab WITH EMPTY KEY WITH NON-UNIQUE SORTED KEY non_uni_so_key COMPONENTS comp1,
      standard_tab_uni_so_sec_k     TYPE TABLE OF ty_tab WITH EMPTY KEY WITH UNIQUE SORTED KEY uni_so_key COMPONENTS comp1,
      standard_tab_hashed_sec_k     TYPE TABLE OF ty_tab WITH EMPTY KEY WITH UNIQUE HASHED KEY hashed_key COMPONENTS comp1,

      "Sorted tables
      sorted_tab                    TYPE SORTED TABLE OF ty_tab WITH UNIQUE KEY comp1,
      sorted_tab_mult_key_comp      TYPE SORTED TABLE OF ty_tab WITH UNIQUE KEY comp1 comp2 comp3,
      sorted_tab_w_sec_key          TYPE SORTED TABLE OF ty_tab WITH UNIQUE KEY comp1 WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS comp2,

      "Hashed tables
      hashed_tab                    TYPE HASHED TABLE OF ty_tab WITH UNIQUE KEY comp1,
      hashed_tab_mult_key_comp      TYPE HASHED TABLE OF ty_tab WITH UNIQUE KEY comp1 comp2 comp3,
      hashed_tab_w_sec_key          TYPE HASHED TABLE OF ty_tab WITH UNIQUE KEY comp1 WITH NON-UNIQUE SORTED KEY non_uni_so_key COMPONENTS comp2.

ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "Dynamically calling all exemplary methods (excluding the main method)
    "The method names are retrieved using RTTI. Refer to the Dynamic Programming ABAP cheat sheet.
    DATA(oref) = NEW zcl_demo_abap( ).
    DATA(methods) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( 'ZCL_DEMO_ABAP' ) )->methods.
    DELETE methods WHERE name = 'IF_OO_ADT_CLASSRUN~MAIN'.
    SORT methods BY name ASCENDING.

    LOOP AT methods INTO DATA(meth_wa).
      TRY.
          CALL METHOD oref->(meth_wa-name) EXPORTING name = meth_wa-name RECEIVING result = result_meth.
          APPEND result_meth TO result_tab.
        CATCH cx_root.
      ENDTRY.
    ENDLOOP.

    SORT result_tab BY avg_runtime ASCENDING.
    out->write( result_tab ).
  ENDMETHOD.

  METHOD store_runtime.
    cl_abap_utclong=>diff( EXPORTING high    = high
                                     low     = low
                           IMPORTING seconds = DATA(seconds) ).

    APPEND seconds TO runtime_tab.
  ENDMETHOD.

  METHOD class_constructor.

    DO loop_count_2000 TIMES.
      INSERT VALUE #( comp1 = sy-index comp2 = sy-index comp3 = sy-index comp4 = sy-index
                      comp5 = sy-index comp6 = sy-index comp7 = sy-index  ) INTO TABLE sorted_tab.


    ENDDO.

    "Populating standard tables
    INSERT LINES OF sorted_tab INTO TABLE standard_tab.
    INSERT LINES OF sorted_tab INTO TABLE standard_tab_mult_key_comp.
    INSERT LINES OF sorted_tab INTO TABLE standard_tab_non_uni_so_sec_k.
    INSERT LINES OF sorted_tab INTO TABLE standard_tab_uni_so_sec_k.
    INSERT LINES OF sorted_tab INTO TABLE standard_tab_hashed_sec_k.

    "Populating sorted tables
    INSERT LINES OF sorted_tab INTO TABLE sorted_tab_mult_key_comp.
    INSERT LINES OF sorted_tab INTO TABLE sorted_tab_w_sec_key.

    "populating hashed tables
    INSERT LINES OF sorted_tab INTO TABLE hashed_tab.
    INSERT LINES OF sorted_tab INTO TABLE hashed_tab_mult_key_comp.
    INSERT LINES OF sorted_tab INTO TABLE hashed_tab_w_sec_key.
  ENDMETHOD.

  METHOD get_avg_runtime.
    "Calculating the average runtime value

    DATA(sum) = REDUCE decfloat34( INIT s = VALUE #( )
                                   FOR wa IN runtime_tab
                                   NEXT s += wa ).

    TRY.
        avg_runtime = sum / lines( runtime_tab ).
      CATCH cx_sy_zerodivide.
    ENDTRY.

    CLEAR runtime_tab.
  ENDMETHOD.

  METHOD std_01_index.
    description = `Read standard table lines by index`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab TRANSPORTING NO FIELDS INDEX idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = index
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD std_02_pr_key_single_comp.
    description = `Read standard table lines by primary table key (single key component)`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD std_03_mult_key_full.
    description = `Read standard table lines by the full primary table key (multiple key component)`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab_mult_key_comp TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = idx comp2 = idx comp3 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD std_04_mult_key_partial_la.
    description = `Read standard table lines by the partial, left-aligned primary table key`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp1 = idx comp2 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD std_05_mult_key_partial_non.
    description = `Read standard table lines by the partial, not left-aligned primary table key`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp2 = idx comp3 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD std_06_mult_key_dynamic.
    description = `Read standard table lines by primary table key, dynamic key specifications`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab_mult_key_comp TRANSPORTING NO FIELDS WITH TABLE KEY ('COMP1') = idx ('COMP2') = idx ('COMP3') = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD std_07_free_key.
    description = `Read standard table lines by free key`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab TRANSPORTING NO FIELDS WITH KEY comp7 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = free_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD std_08_free_key_binary_search.
    description = `Read standard table lines with free key and binary search`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      IF idx = 1.
        SORT standard_tab BY comp7 ASCENDING.
      ENDIF.
      READ TABLE standard_tab TRANSPORTING NO FIELDS WITH KEY comp7 = idx BINARY SEARCH.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = free_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD std_09_sec_key_nonuni_sorted.
    description = `Read standard table lines by secondary table key (non-unique sorted key)`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab_non_uni_so_sec_k TRANSPORTING NO FIELDS WITH TABLE KEY non_uni_so_key COMPONENTS comp1 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = secondary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD std_10_sec_key_nonuni_sort_idx.
    description = `Read standard table lines by secondary table index (non-unique sorted key)`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab_non_uni_so_sec_k TRANSPORTING NO FIELDS USING KEY non_uni_so_key INDEX idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = secondary_table_index
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD std_10_sec_key_unique_sorted.
    description = `Read standard table lines by secondary table key (unique sorted key)`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab_uni_so_sec_k TRANSPORTING NO FIELDS WITH TABLE KEY uni_so_key COMPONENTS comp1 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = secondary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD std_11_sec_key_unique_hashed.
    description = `Read standard table lines by secondary table key (unique hashed key)`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab_hashed_sec_k TRANSPORTING NO FIELDS WITH TABLE KEY hashed_key COMPONENTS comp1 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = standard
                      access      = secondary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD sorted_01_index.
    description = `Read sorted table lines by index`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab TRANSPORTING NO FIELDS INDEX idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = sorted
                      access      = index
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD sorted_02_pr_key_single_comp.
    description = `Read sorted table lines by primary table key (single key component)`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = sorted
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD sorted_03_mult_key_full.
    description = `Read sorted table lines by the full primary table key (multiple key components)`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab_mult_key_comp TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = idx comp2 = idx comp3 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = sorted
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD sorted_04_mult_key_partial_la.
    description = `Read sorted table lines by partial, left-aligned primary table key`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp1 = idx comp2 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = sorted
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD sorted_05_mult_key_partial_non.
    description = `Read sorted table lines by partial, not left-aligned primary table key`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp2 = idx comp3 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = sorted
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD sorted_06_mult_key_dynamic.
    description = `Read sorted table lines by primary table key, dynamic key specification`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab_mult_key_comp TRANSPORTING NO FIELDS WITH TABLE KEY ('COMP1') = idx ('COMP2') = idx ('COMP3') = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = sorted
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD sorted_07_free_key.
    description = `Read sorted table lines by free key`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab TRANSPORTING NO FIELDS WITH KEY comp7 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = sorted
                      access      = free_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.


  METHOD hashed_01_pr_key_single_comp.
    description = `Read hashed table lines by primary table key (single key component)`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE hashed_tab TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = hashed
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD hashed_02_mult_key_full.
    description = `Read hashed table lines by the full primary table key (multiple key components)`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE hashed_tab_mult_key_comp TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = idx comp2 = idx comp3 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = hashed
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD hashed_03_mult_key_partial_la.
    description = `Read hashed table lines by partial, left-aligned primary table key`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE hashed_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp1 = idx comp2 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = hashed
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD hashed_04_mult_key_partial_non.
    description = `Read hashed table lines by partial, not left-aligned primary table key`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE hashed_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp2 = idx comp3 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = hashed
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD hashed_05_mult_key_dynamic.
    description = `Read hashed table lines by primary table key, dynamic key specification`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE hashed_tab_mult_key_comp TRANSPORTING NO FIELDS WITH TABLE KEY ('COMP1') = idx ('COMP2') = idx ('COMP3') = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = hashed
                      access      = primary_table_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD hashed_06_free_key.
    description = `Read hashed table lines by free key`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE hashed_tab TRANSPORTING NO FIELDS WITH KEY comp7 = idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = hashed
                      access      = free_key
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.

  METHOD hashed_07_sec_key_nonun_so_idx.
    description = `Read hashed table lines by secondary table index (non-unique sorted key)`.
    DO loop_count_2000 TIMES.
      idx = sy-index.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE hashed_tab_w_sec_key TRANSPORTING NO FIELDS USING KEY non_uni_so_key INDEX idx.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    result = VALUE #( meth_name = name
                      avg_runtime = |{ runtime STYLE = SIMPLE }|
                      table_category = hashed
                      access      = secondary_table_index
                      description = description
                      iteration_count = idx ).
  ENDMETHOD.
ENDCLASS.
```

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Generic Table Types with Formal Parameters of Methods and Field Symbols


- [Formal parameters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenformal_parameter_glosry.htm) of methods or [field symbols](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm) can be specified with generic types.
- At runtime, the actual data type is copied from the assigned [actual parameter](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenactual_parameter_glosry.htm) or memory area, i.e. they receive the complete data type only when an actual parameter is passed or a memory area is assigned.
- Among them, there are generic table types. For more information, refer to the [Data Types and Objects](16_Data_Types_and_Objects.md) cheat sheet and the [documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuilt_in_types_generic.htm).
- The following example mainly demonstrates formal parameters of methods that are typed with generic table types. The method calls and the tables passed are only possible if the generic types fit. For example, you cannot pass a hashed table to a method whose importing parameter is typed with the generic type `INDEX TABLE`. Invalid method calls and table passing are commented out.

```abap
CLASS zcl_demo_abap DEFINITION
      PUBLIC
      FINAL
      CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS process_any_table IMPORTING itab TYPE ANY TABLE.
    CLASS-METHODS process_standard_tables1 IMPORTING itab TYPE STANDARD TABLE.
    CLASS-METHODS process_standard_tables2 IMPORTING itab TYPE TABLE.
    CLASS-METHODS process_sorted_tables IMPORTING itab TYPE SORTED TABLE.
    CLASS-METHODS process_hashed_tables IMPORTING itab TYPE HASHED TABLE.
    CLASS-METHODS process_index_tables IMPORTING itab TYPE INDEX TABLE.

ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA standard_tab TYPE TABLE OF string WITH EMPTY KEY.
    DATA sorted_tab TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.
    DATA hashed_tab TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

    "ANY TABLE
    process_any_table( standard_tab ).
    process_any_table( sorted_tab ).
    process_any_table( hashed_tab ).

    "(STANDARD) TABLE
    process_standard_tables1( standard_tab ).
    "process_standard_tables1( sorted_tab ).
    "process_standard_tables1( hashed_tab ).
    process_standard_tables2( standard_tab ).
    "process_standard_tables2( sorted_tab ).
    "process_standard_tables2( hashed_tab ).

    "SORTED TABLE
    "process_sorted_tables( standard_tab ).
    process_sorted_tables( sorted_tab ).
    "process_sorted_tables( hashed_tab ).

    "HASHED TABLE
    "process_hashed_tables( standard_tab ).
    "process_hashed_tables( sorted_tab ).
    process_hashed_tables( hashed_tab ).

    "INDEX TABLE
    process_index_tables( standard_tab ).
    process_index_tables( sorted_tab ).
    "process_index_tables( hashed_tab ).


    "Note: Field symbols can also be typed with generic types.
    FIELD-SYMBOLS <fs_std_table> TYPE table.
    ASSIGN standard_tab TO <fs_std_table>.
    "ASSIGN sorted_tab TO <fs_std_table>.
    "ASSIGN hashed_tab TO <fs_std_table>.

    FIELD-SYMBOLS <fs_index_table> TYPE INDEX TABLE.
    ASSIGN standard_tab TO <fs_index_table>.
    ASSIGN sorted_tab TO <fs_index_table>.
    "ASSIGN hashed_tab TO <fs_index_table>.

  ENDMETHOD.
  METHOD process_any_table.
  ENDMETHOD.

  METHOD process_hashed_tables.
  ENDMETHOD.

  METHOD process_index_tables.
  ENDMETHOD.

  METHOD process_sorted_tables.
  ENDMETHOD.

  METHOD process_standard_tables1.
  ENDMETHOD.

  METHOD process_standard_tables2.
  ENDMETHOD.

ENDCLASS.
```

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

You can compare internal tables: 
- Using the methods of the `CL_ABAP_DIFF` class to compare the content of two compatible index tables programmatically. 
  - Find more information in the class documentation and in the [ABAP Keyword Documentation]([06_Dynamic_Programming.md](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencl_abap_diff.htm)). A code snippet is available in the [Released ABAP Classes](22_Released_ABAP_Classes.md) cheat sheet.
- Using the [Table Comparison Tool](https://help.sap.com/docs/abap-cloud/abap-development-tools-user-guide/comparing-internal-tables-085ab2303ceb4478ad7958053c2ebeb3?locale=en-US&version=LATEST) in ADT.   

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

### Creating Internal Tables Dynamically

Find more information in the [Dynamic Programming cheat sheet](06_Dynamic_Programming.md).

```abap
DATA(some_type) = 'STRING'.
DATA dataref TYPE REF TO data.

"Creating an internal table using a CREATE DATA statement
"by specifying the type name dynamically.
"In the example, a standard table with elementary line type
"and standard key is created.
CREATE DATA dataref TYPE TABLE OF (some_type).

TYPES: BEGIN OF demo_struc,
          comp1 TYPE c LENGTH 10,
          comp2 TYPE i,
          comp3 TYPE i,
        END OF demo_struc.

"Internal table with structured line type and empty key.
CREATE DATA dataref TYPE TABLE OF ('DEMO_STRUC') WITH EMPTY KEY.

"Using a globally available table type
CREATE DATA dataref TYPE ('STRING_TABLE').
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


## More Information
Topic [Internal Tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenitab.htm) in the ABAP Keyword Documentation.

## Executable Example
[zcl_demo_abap_internal_tables](./src/zcl_demo_abap_internal_tables.clas.abap)

> [!NOTE] 
> - The executable example covers the following topics, among others: Creating, populating, reading from, sorting, modifying internal tables
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).

> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)
