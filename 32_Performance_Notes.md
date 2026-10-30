<a name="top"></a>

# ABAP Performance Notes

- [ABAP Performance Notes](#abap-performance-notes)
  - [Database Access](#database-access)
    - [Reducing the Result Set](#reducing-the-result-set)
    - [Reducing the Data Volume](#reducing-the-data-volume)
    - [Reducing Database Accesses](#reducing-database-accesses)
    - [Using CDS views](#using-cds-views)
    - [Dynamic and Static ABAP SQL Statements](#dynamic-and-static-abap-sql-statements)
    - [Using Buffering](#using-buffering)
    - [Using Indexes](#using-indexes)
  - [Processing Internal Tables](#processing-internal-tables)
    - [Single Line Access Using the Primary Table Key and Free Keys](#single-line-access-using-the-primary-table-key-and-free-keys)
    - [Multiline Access and Applying a WHERE Condition](#multiline-access-and-applying-a-where-condition)
    - [Reducing Content Transfer](#reducing-content-transfer)
    - [Block-wise Processing of Table Lines](#block-wise-processing-of-table-lines)
    - [Using Secondary Table Keys to Improve Read Performance](#using-secondary-table-keys-to-improve-read-performance)
    - [Applying a Sort Key when Sorting Internal Tables](#applying-a-sort-key-when-sorting-internal-tables)
    - [Dynamic and Static Specifications when Processing Internal Tables](#dynamic-and-static-specifications-when-processing-internal-tables)
    - [Overusing Table Expressions](#overusing-table-expressions)
  - [Miscellaneous Contexts](#miscellaneous-contexts)
    - [Using Strings](#using-strings)
    - [Avoiding Implicit Type Conversions](#avoiding-implicit-type-conversions)
    - [Passing by Reference and Value](#passing-by-reference-and-value)
    - [Parallel Processing](#parallel-processing)
  - [Executable Example](#executable-example)

Various factors can influence the runtime of an ABAP program. Key elements affecting performance include the efficiency of database interactions and the processing of internal tables. This cheat sheet explores a selection of performance-related examples. These examples aim to illustrate potentially inefficient techniques and use of statements.

> [!NOTE]
> - For performance and runtime analysis, it is recommended to use the available tools. This ABAP cheat sheet and the executable example do not address these tools. For more information, refer to the [SAP Help Portal documentation](https://help.sap.com/docs/abap-cloud/abap-development-tools-user-guide/profiling-abap-code?state=CLOUD&version=sap_btp) and the ["Explore ABAP Profiling with the ABAP Development Tools" video](https://www.youtube.com/watch?v=CSv-0YGfDS8).
> - Most examples in other ABAP cheat sheet documents and the executable examples in the ABAP cheat sheet repository do not consider performance aspects. They primarily focus on demonstrating syntax options and additions.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Database Access

### Reducing the Result Set

- It is recommended to keep the number of data sets read to a minimum. Avoid reading unnecessary table rows.
- You can minimize the result set by considering the following aspects:
	- While optional, using a `WHERE` clause is advisable. This ensures that only the necessary data transfers from the database system. Omit the `WHERE` clause only in cases when you really need the entire content.
	- The `HAVING` clause works with `GROUP BY` to further restrict grouped lines by applying additional conditions.
	- The `SINGLE` addition in the `SELECT` statement is designed for reading a single data set. 
	- Use the `UP TO n ROWS` addition if you need to process only a specified number of rows in your program.
	- Use the `DISTINCT` addition to filter out duplicate entries.

> [!NOTE]
> Complex conditions in the `WHERE` and `HAVING` clauses can impact performance. Statements that check for equality (`=`/`EQ`) and use `AND` are particularly efficient. Other operators like `NOT` and `OR` require more effort from the database to search for data sets.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Reducing the Data Volume

Minimizing the transferred data volume can be achieved by considering the following aspects:

- The hints mentioned earlier, such as using a `WHERE` clause, also apply here. They limit the number of rows returned, thereby minimizing the volume of data processed.
- Restricting the number of columns: Only read the data sets that are truly necessary for your program. It is advisable to specify concrete columns in the `SELECT` statement instead of using the asterisk `*` to select all columns. 
- When updating data with `UPDATE` statements, you can use the `SET` addition to specify specific columns to change. This approach is typically more efficient than modifying table line content in a work area - where only one or few columns are modified - and then applying `UPDATE` using that work area.

> [!NOTE]
> Using aggregate functions, such as `SUM`, `AVG` or `COUNT`, for calculations can effectively reduce the data volume transferred from the database instead of performing calculations in the program. These aggregate functions incur runtime costs on the database. Positive performance impact can depend on factors such as the efficiency of function use and the number of data sets processed.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Reducing Database Accesses

You can minimize database accesses by considering the following aspects:

- Block-wise operations instead of single operations
  - This is especially relevant for `INSERT`, `UPDATE`, and `DELETE` statements that involve internal tables as operands, enabling block-wise processing instead of processing database table content line by line.
- Avoiding repeated database access
  - The same data set should not be read multiple times in a program.
  - Avoid repeated database access in a loop, for example, in a `LOOP` statement. 
  - This also applies to ABAP EML statements. They should not be used in loops. Using them can have a performance impact because it can result in multiple single database accesses. There should be only one ABAP EML statement to read the necessary data, and then the data should be modified with an ABAP EML modify request.
- Avoiding nested `SELECT` loops
  - A `SELECT` loop results in a single database access, returning data line by line to the program. 
  - If you implement another `SELECT` within this loop, the number of accesses in the inner loop multiplies by the outer loop's accesses, negatively impacting performance, especially with large data sets.
- Using joins and subqueries
  - When accessing and combining data from multiple databases, joins are preferable to handling individual data retrievals separately.
  - Subqueries in the `WHERE` and `HAVING` clauses do not transfer data from the database system.
  - If your use case is to match entries of an internal table when selecting data from a data source, you can also use joins and subqueries with internal tables in `SELECT` statements in modern ABAP to avoid unnecessary data transfers from the database. You may also stumble on the (older) `FOR ALL ENTRIES` addition in `SELECT` statements. Here, internal table content is used to match entries. However, note that if the internal table is empty, all data will be read. Note that `FOR ALL ENTRIES` is considered deprecated. See the [ABAP SQL](03_ABAP_SQL.md) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using CDS views	

- ABAP CDS (Core Data Services) represent a framework for defining and consuming semantically rich data models on AS ABAP.  
- ABAP CDS can enhance database access performance by leveraging the capabilities of the [SAP HANA database](https://help.sap.com/docs/abap-cloud/abap-keyword/sap-hana-database).
- Data-intensive operations, such as complex calculations or joins, can be pushed down to the database layer and executed in the SAP HANA database instead of performing them in ABAP.
- Find more information in the [ABAP Data Models guide](https://help.sap.com/docs/abap-cloud/abap-data-models/abap-data-models?version=sap_btp).

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic and Static ABAP SQL Statements

- ABAP SQL statements provide syntax options for dynamic programming, such as specifying the data source to read from dynamically.  
- Dynamically specifying tokens requires runtime evaluation. Static token specification offers better performance.  
- For more information, refer to the Dynamic Programming cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using Buffering

- Data from DDIC database tables or CDS entities can be buffered in a table buffer. 
- Buffering capabilities are determined by the technical settings of the artifacts. For example, for a CDS entity, buffering is determined by annotations and a CDS entity buffer.  
- The table buffering is applied implicitly when the database is accessed using ABAP SQL. 
- Find more information in the [ABAP Keyword Documentation](https://help.sap.com/docs/abap-cloud/abap-keyword/abap-sql-table-buffering).

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using Indexes

- Indexes are special structures on the database that enhance query processing times and offer additional capabilities.
- Find more information in the [ABAP Keyword Documentation](https://help.sap.com/docs/abap-cloud/abap-keyword/ddic-indexes-in-database-tables).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Processing Internal Tables

> [!NOTE]
> - Generally, a suitable table type should be chosen based on aspects such as the frequency of table access and modifications.
> - On a high level, standard tables should be used for smaller data sets and sequential processing. Sorted tables may also be used for sequential processing and when you always need to keep content in a sorted order. They offer optimized access through the table key. For very large data sets that require minimal modifications, consider using hashed tables. The primary access method for hashed tables is key access, supported by a hash algorithm for consistent and optimized retrieval.
> - Standard tables are searched linearly by keys. They do not offer an optimized key access. However, standard tables (and the other table categories, too) can be enriched by secondary table keys, providing optimized access.
> - Find more information in the [Internal Tables](01_Internal_Tables.md) cheat sheet.
> - The [Internal Tables](01_Internal_Tables.md) cheat sheet includes sections dealing with performance-related aspects for internal table accesses (e.g. key access, using secondary table keys, etc.). You can find several executable examples there. These are more representative and detailed than those covered in the executable example of this cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Single Line Access Using the Primary Table Key and Free Keys

- Single line access using the primary table key refers to statements like `READ TABLE ... WITH TABLE KEY ...` or table expressions with the addition `TABLE KEY`.
- Notes on the table categories:
  - Standard: Table lines are usually searched linearly, with no optimized search, making this category fairly inefficient for larger tables.
  - Sorted: Access is optimized through the primary table key. However, as the table size increases, key access becomes slower. Even if the primary table key is only partially specified, optimized access may still be achieved if the specification is left-aligned, without gaps. For example, if the primary table key consists of components `a`, `b`, and `c`, optimized access is valid if only `a` and `b` are specified. If the keys are not left-aligned, such as in the case where `b` and `c` are specified without `a`, optimized access is not applicable, and a linear search will be used.
  - Hashed: Access time remains constant regardless of table size due to a hash algorithm, making it especially effective for very large tables. To take advantage of optimized access, the entire key must be specified.
- Single line access using a free key refers to statements like `READ TABLE ... WITH KEY ...` or table expressions without a `[TABLE] KEY` addition.  
	- The search with free keys is executed linearly. If the key specification for sorted tables partially covers the primary table key but not left-aligned and gap-free, the search will also be linear.  
	- In older ABAP code, you may encounter the `BINARY SEARCH` addition in `READ TABLE` statements for standard tables. This addition allows for a binary search. However, the table must be kept sorted in ascending order based on the search key, typically requiring a fairly costly `SORT` statement beforehand. It is advisable to use secondary table keys instead of the addition.

<p align="right"><a href="#top">⬆️ back to top</a></p>
 
### Multiline Access and Applying a WHERE Condition

- If not really required, it is recommended to avoid full iteration of internal tables, for example, in `LOOP` statements. 
- To restrict the table iteration, you can apply a `WHERE` condition and use other additions such as `FROM` and `TO` for index tables.
- `WHERE` conditions are available for multiple internal table processing statements such as `LOOP`, `MODIFY`, and `DELETE`.
- Similar to the notes mentioned in the single line access section, a performant table access is determined by table key specification. Standard tables are iterated without optimization (unless using secondary table keys).
- Further prerequisites for an optimzation in the `WHERE` conditions are these:
	- The specified conditions should be transferable to key and value pairs. The key fields should be compared for equality and combined by `AND`, if any.
	- The operands of the `WHERE` condition should be type-compliant. Otherwise, type conversion costs are inferred.
- For more information about optimizing the `WHERE` condition, refer to the [ABAP Keyword Documentation](https://help.sap.com/docs/abap-cloud/abap-keyword/itab-optimizing-where-condition).
- `READ TABLE` also supports the specification of a `WHERE` condition. If a `READ TABLE` statement can be expressed using `WITH KEY`, the use of `WITH KEY` is recommended as it is more performant. A syntax warning occurs but can be suppressed with the pragma `##read_where_ok`.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Reducing Content Transfer

Contexts of reducing content transfer are, for example, the following:

- Using the `TRANSPORTING` addition
	- Statements like `MODIFY`, `READ`, or `LOOP` support the `TRANSPORTING` addition, which lets you specify the exact fields to process, rather than processing all fields. For read operations, this is true for specifying work areas as the target area.
	- In some cases, `TRANSPORTING` can specify `NO FIELDS`, such as in a read operation when only system fields are needed.
  
- Using field symbols and data reference variables instead of work areas
	- Statements like `LOOP` and `READ` provide three options to specify the target area for the read table line (or no target area using `TRANSPORTING NO FIELDS`): work areas, field symbols, data reference variables. 
	- Depending on factors like the complexity of the internal table (e.g., nested components) or the number of reads, using work areas may be less efficient since the content of the table line is copied to the target variable, while other options behave like pointers. However, for simpler tables, reading into work areas may not be less efficient. Plus, the `TRANSPORTING` addition lets you exclude fields.
	- Consider using field symbols or data reference variables as the target area instead of work areas if:
		- Direct access to table lines is necessary (e.g., for modification).
		- You do not need to copy the table line. 
		- The table being processed includes nested components (e.g., nested structures or internal tables).

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Block-wise Processing of Table Lines

- Processing entire table lines at once, such as for insertions or deletions, is more efficient than handling them one at a time.
- Examples:
  - `INSERT` and `APPEND` statements allow you to add lines from one internal table to another using additions like `LINES OF`. This method is more efficient than copying content line by line.
  - `FIND ... IN TABLE` and `REPLACE ... IN TABLE` statements used to perform operations in internal tables with character-like line types in one step are more efficient than a line by line search and replacement.

			
<p align="right"><a href="#top">⬆️ back to top</a></p>


### Using Secondary Table Keys to Improve Read Performance

- Secondary table keys optimize access to internal tables.
- They are possible for all three table categories.
- It is recommended to use secondary table keys sparingly, ensuring the benefits outweigh the administrative costs.
- Example use cases and notes:
  - Using them for larger tables only is advisable, as smaller tables typically do not gain performance benefits that justify the administrative costs.
  - Enriching standard tables by providing optimized key access through secondary table keys.
  - Allowing standard tables to have unique entries with unique secondary table keys. In such cases, the use of secondary table keys may also be justified for smaller internal tables.
  - The use is recommended for large tables that are filled once, rarely changed, and frequently read. Updates on internal tables impacting table keys and indexes, especially regarding unique secondary table keys, incur runtime costs. Keeping the number of secondary table key components low is also beneficial.
  - Enriching already existing internal tables with secondary table keys. However, consider the implications of adding unique secondary table keys to existing internal tables. Existing code may produce errors when processing these tables. 

> [!NOTE]
> ABAP statements implicitly use the primary table key, while secondary table keys must be explicitly specified.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Applying a Sort Key when Sorting Internal Tables

- Using `SORT` statements can incur runtime costs due to implicit sorting.
- This is especially true for internal tables that use the standard key as the primary table key, such as `DATA itab TYPE TABLE OF zdemo_abap_carr.` that has no explicit key specification. 
- Sorting an internal table with the standard key (e.g., `SORT itab.`) is not optimal for performance. Explicitly specifying the sort key improves readability and clarity while preventing a probably unintended use of the standard key. This is particularly important when the internal table has many character-like fields that make up the standard key. It is advisable to specify the sort key as precisely as possible.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic and Static Specifications when Processing Internal Tables

- Statements that process internal tables provide syntax options for dynamic programming, such as specifying the table key dynamically.  
- Dynamic specifications require runtime evaluation. Static specifications offer better performance.  
- For more information, refer to the Dynamic Programming cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Overusing Table Expressions

- Table expressions - expression enabling in modern ABAP as such - are quite useful in various positions.
- It is recommended to avoid multiple accesses to the same table line when using multiple table expressions. Instead, it is advisable to reference the table line with a field symbol or data reference variable first and then perform the necessary accesses.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Miscellaneous Contexts

### Using Strings

Examples of string-related contexts:

- Fixed-length and variable-length strings
  - Strings (and internal tables) are addressed internally through references.
  - They support the concept of [sharing](https://help.sap.com/docs/abap-cloud/abap-keyword/sharing), which offers performance advantages.
  - Depending on the length of the fixed-length strings (e.g., type `c`), operations using variable-length strings (type `string`) may be faster.

- String templates vs. string literals
  - A string template with only literal text is evaluated as an expression at runtime.
  - If a string template does not contain an expression to evaluate, a text string literal with backquotes is advisable.
  
- Pattern-based searching
  - Particularly in basic pattern-based searches, it can be more efficient to use comparison operators and `IF` statements rather than regular expressions using, for example, `FIND ... PCRE` statements.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Avoiding Implicit Type Conversions

- Whenever possible, avoiding implicit type conversions during assignments is advisable.
- Type conversions can add runtime costs and may lead to unintended results.

<p align="right"><a href="#top">⬆️ back to top</a></p>

 
### Passing by Reference and Value

- Actual parameters passed by reference in procedure signatures can perform better than passing by value since they do not create and assign a local data object.  
- Passing by reference is especially suitable for larger data sets to be passed or when the value of the passed actual parameter does not need to be changed.

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Parallel Processing

- Tasks can be performed in parallel, particularly suitable for data-intensive operations.
- As a result, applications can achieve better performance by executing tasks simultaneously rather than sequentially.
- As an example, the `CL_ABAP_PARALLEL` class supports parallel processing. For more details, refer to the class documentation.

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Executable Example

The executable example explores a selection of performance-related aspects. The code snippets, for which runtime is measured, aim to illustrate potentially inefficient techniques and use of statements.

The example is simplified and set up as follows:

- The global class declares multiple components used across demo methods (they include implementations that are used to visualize performance-related aspects), including a variety of type and data object declarations such as internal tables (standard, sorted, and hashed tables, tables with secondary keys, and a deep table) that are used to illustrate specific examples.
- The demo methods are grouped into thematic subgroups: ABAP SQL (`SQL_*`), internal tables (`ITAB_*`), EML (`EML_*`), and miscellaneous topics (`MISC_*`). 
- Within a subgroup, the methods contain code snippets that compare an approach with a less efficient one. The idea is to mark the more efficient example with the suffix `a` and the less efficient one with `z` (for example, `sql_01_reduce_t_rows_a` shows an efficient way to minimize the result set of a `SELECT`, while `sql_01_reduce_t_rows_z` shows an inefficient one). This does not apply to each subgroup. Some subgroups compare more than two variants and use additional suffixes such as `b` or `c`. 
- A central control table (`t_info`) drives the example: 
  - It lists each method together with its subgroup, the number of repetitions per measurement (`ops`), and a short description. 
  - A loop runs all registered methods dynamically, so the benchmark set can be maintained in one place.
- The runtime analysis in this self-contained example works as follows: 
  - The code snippet to be measured is enclosed by two timestamp retrievals using `utclong_current( )`, and the runtime is the delta of the two values. 
  - To make the measurement more meaningful, each snippet is executed many times within a `DO` loop (`ops` iterations), and the whole measurement is repeated several times. 
  - The runtime deltas of the repetitions are collected in an internal table, and the median runtime is determined and divided by the number of operations to obtain a median runtime per operation. Any data setup needed for a snippet is prepared beforehand, outside the measured section. 
  - The results are written to the console grouped by category and subgroup and sorted by runtime. A `relative_to_min` column shows each variant relative to the fastest one in its subgroup, making the ranking easier to read.
- The parallelization example is measured differently from the other snippets and is therefore handled separately. It uses a local class implemented in the CCIMP include and compares parallel against sequential processing.
- Note the disclaimer below and in the ABAP Cheat Sheet repository's README. 


Overview of covered performance-related aspects:

- <strong>ABAP SQL statements</strong>	
	- Using a `WHERE` clause vs. reading all table rows, looping across the result set and filtering out data
	- Using `SELECT SINGLE` to read a single data set vs. reading all table rows and filtering out a single data set vs. `SELECT ... UP TO 1 ROWS`
	- Reading a specific number of datasets using `UP TO n ROWS` vs. reading all table rows and discarding unnecessary data
	- Specifying columns in the `SELECT` list vs. specifying `SELECT *`
	- Single database access vs. multiple accesses in a loop
	- Using joins, `WHERE EXISTS` and `FOR ALL ENTRIES` vs. repeated database access in a loop
	- Block-wise modify operation vs. line by line modify operation in a loop
	- Using a join vs. using a nested `SELECT` loop
	- Using an `UPDATE ... SET` statement vs. `UPDATE` using a work area
	- Static vs. dynamic token specification
- <strong>Internal table processing</strong>
	- Reading single lines using the primary table key: standard table vs. sorted table vs. hashed table
	- Reading single lines using the incomplete primary table key: hashed table vs. sorted table (partial key, both left-aligned and not left-aligned)
	- Reading single lines in a standard table using `BINARY SEARCH` and without the addition (both examples use a preceding `SORT` statement)
	- Loop across internal table by specifying a `WHERE` condition: standard table vs. sorted (full key + partial, left-aligned key + partial, not left-aligned key) vs. hashed (full key + partially specified key)
	- Loop across standard table: specifying a `WHERE` condition vs. no `WHERE` condition and filtering out data in the loop
	- Loop across deep standard table (includes a nested structure and internal table) specifying target areas: field symbol vs. data reference variable vs. work area
	- Iterating an internal table using `LOOP AT` vs. manual index-based access with a `DO` loop and `READ TABLE ... INDEX`
	- Modifying internal table content in a loop: direct modification using a field symbol vs. direct modification using a data reference variable vs. work area as target area and applying a `MODIFY` statement
	- Block-wise insertion into an internal table vs. line by line insertion in a loop
	- Restricting the transfer of table line content using the `TRANSPORTING` addition vs. not using the addition and overwriting all table line content based on a work area
	- Reading single lines from a standard table: using a secondary table key vs. using the primary table key (which corresponds to the secondary table key in the other table)
	- Loop across a standard table that specifies a secondary table key: performing only reads in the loop vs. performing reads and modifications
	- Sorting an internal table that specifies the standard key: explicitly specifying the sort key vs. implicit sorting by the standard key
	- Reading from an internal table by specifying the keys dynamically vs. static key specification
	- `READ TABLE ... WITH KEY` statement vs. `READ TABLE ... WHERE` statement
	- Find operations on an internal table with character-like line type: block-wise `FIND ... IN TABLE` vs. line by line search in a loop
	- Multiple reads and writes on the same table lines: using table lines assigned to field symbols vs. using individual table expressions
	- Replace operations on an internal table with character-like line type: block-wise `REPLACE ... IN TABLE` vs. line by line replacement in a loop
- <strong>Miscellaneous contexts</strong>
	- Concatenating a variable-length string of type `string` vs. concatenating a fixed-length string of type `c`
	- String specified as string literal using backquotes vs. string template only using characters, without embedded expression
	- Pattern-based search using a comparison operator vs. a `FIND PCRE` statement and a regular expression
	- Calculation of numbers that share the same type vs. calculation of numbers that have different types
	- Actual parameters passed by reference vs. actual parameters passed by value
	- ABAP EML update request using a `FOR` loop for a block-wise operation vs. EML statement in a loop
	- Parallel processing vs. sequential processing
 
> [!IMPORTANT]
> This example:
> - Is for [exploration, experimentation, and demonstration](./README.md#%EF%B8%8F-disclaimer) purposes only. It is not suitable for accurate runtime or performance measurement. Various factors (such as system, environment, system load, etc.) can influence the runtime of an ABAP program, so that results may vary and may not be entirely accurate, even across multiple runs. The displayed results may not always accurately reflect the performance-related notes from the cheat sheet. 
> - Aims to illustrate general performance aspects in various contexts, and highlight the described behavior and potential pitfalls using intentionally exaggerated example code snippets. Some results may be more representative and clear than others. Note that regarding internal table access examples in particular, the performance-related examples in the Internal Table cheat sheet are more representative and detailed.
> - Is intended to emphasize the importance of choosing appropriate approaches for a given use case.
> - Does not represent best practices regarding performance testing and is not a substitute or role model for profiling real applications with realistic data and workloads.
> - Takes some time to complete due to many loops included.
> Note the disclaimer in the ABAP cheat sheet repository's README.


To try the example out, create a demo class named `zcl_demo_abap` in the _SAP Business AI Platform, ABAP environment_ and paste the code into it. The example also includes a local class in the CCIMP include (Local Types tab in ADT). As a prerequisite, you have imported the ABAP Cheat Sheets GitHub repository as the example uses some of its artifacts.
After activation, choose F9 in ADT to execute the class. The example is set up to display output in the console. The class execution may take a while to complete.


<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->
<br>

<table>
<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

```abap
"! <p class="shorttext"><strong>Performance Notes</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class includes several methods that explore both inefficient and potentially more
"! efficient code examples affecting performance.</p>
"! <p>In ADT, choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li>As a prerequisite, you have imported the ABAP cheat sheet repository since the
"! example uses some of its artifacts.</li>
"! <li>The example is for exploration, experimentation, and demonstration. It is not intended for accurate
"! runtime or performance testing and is not a suitable method for such purposes. Due to its simplified nature,
"! results may vary and not be entirely accurate, even across multiple runs.</li>
"! <li>Note the disclaimer in the ABAP cheat sheet repository.</li></ul>
"! <p class="shorttext"><strong>Performance Notes</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class includes several methods that explore both inefficient and potentially more
"! efficient code examples affecting performance.</p>
"! <p>In ADT, choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li>As a prerequisite, you have imported the ABAP cheat sheet repository since the
"! example uses some of its artifacts.</li>
"! <li>The example is for exploration, experimentation, and demonstration. It is not intended for accurate
"! runtime or performance testing and is not a suitable method for such purposes. Due to its simplified nature,
"! results may vary and not be entirely accurate, even across multiple runs.</li>
"! <li>Note the disclaimer in the ABAP cheat sheet repository.</li>
"! <li>Within a subgroup, <em>_A</em> is generally the recommended approach and <em>_Z</em> the slower
"! antipattern. Some subgroups (for example ITAB_01, ITAB_02, ITAB_04) instead compare equivalent
"! table-access variants (standard/sorted/hashed); there the A/B/Z suffixes are only labels, not a
"! quality ranking. The per-subgroup column 'relative_to_min' shows the actual runtime ranking.</li></ul>
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS class_constructor.

    "Constants used for DO n TIMES statements
    CONSTANTS loop_count_5 TYPE i VALUE 5.
    CONSTANTS loop_count_15 TYPE i VALUE 15.
    CONSTANTS loop_count_30 TYPE i VALUE 30.
    CONSTANTS loop_count_100 TYPE i VALUE 100.
    CONSTANTS loop_count_250 TYPE i VALUE 250.
    CONSTANTS loop_count_500 TYPE i VALUE 500.
    CONSTANTS loop_count_1000 TYPE i VALUE 1000.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS repetitions TYPE i VALUE 5.

    "Components used for calculating the average runtime
    DATA: ts1         TYPE utclong,
          runtime_tab TYPE TABLE OF decfloat34 WITH EMPTY KEY,
          out         TYPE REF TO if_oo_adt_classrun_out.

    "Methods for the runtime measurement
    METHODS: store_runtime IMPORTING high TYPE utclong
                                     low  TYPE utclong,
      get_median_runtime RETURNING VALUE(median) TYPE decfloat34,
      write_section IMPORTING text TYPE string,
      prepare IMPORTING method_name TYPE abap_methname.

    "Exemplary methods containing (in)efficient demo implementations
    METHODS:
      sql_01_reduce_t_rows_a IMPORTING ops TYPE i,
      sql_01_reduce_t_rows_z IMPORTING ops TYPE i,
      sql_02_reduce_t_rows_a IMPORTING ops TYPE i,
      sql_02_reduce_t_rows_b IMPORTING ops TYPE i,
      sql_02_reduce_t_rows_z IMPORTING ops TYPE i,
      sql_03_reduce_t_rows_a IMPORTING ops TYPE i,
      sql_03_reduce_t_rows_z IMPORTING ops TYPE i,
      sql_04_reduce_data_volume_a IMPORTING ops TYPE i,
      sql_04_reduce_data_volume_z IMPORTING ops TYPE i,
      sql_05_reduce_db_access_a IMPORTING ops TYPE i,
      sql_05_reduce_db_access_z IMPORTING ops TYPE i,
      sql_06_reduce_db_access_a IMPORTING ops TYPE i,
      sql_06_reduce_db_access_b IMPORTING ops TYPE i,
      sql_06_reduce_db_access_c IMPORTING ops TYPE i,
      sql_06_reduce_db_access_z IMPORTING ops TYPE i,
      sql_07_reduce_db_access_a IMPORTING ops TYPE i,
      sql_07_reduce_db_access_z IMPORTING ops TYPE i,
      sql_08_reduce_db_access_a IMPORTING ops TYPE i,
      sql_08_reduce_db_access_z IMPORTING ops TYPE i,
      sql_09_update_a IMPORTING ops TYPE i,
      sql_09_update_z IMPORTING ops TYPE i,
      sql_11_dynamic_sql_a IMPORTING ops TYPE i,
      sql_11_dynamic_sql_z IMPORTING ops TYPE i,
      itab_01_table_key_read_a_std IMPORTING ops TYPE i,
      itab_01_table_key_read_b_so IMPORTING ops TYPE i,
      itab_01_table_key_read_z_ha IMPORTING ops TYPE i,
      itab_02_incomplete_t_key_a_so IMPORTING ops TYPE i,
      itab_02_incomplete_t_key_b_ha IMPORTING ops TYPE i,
      itab_02_incomplete_t_key_z_so IMPORTING ops TYPE i,
      itab_03_std_binary_search_a IMPORTING ops TYPE i,
      itab_03_std_binary_search_z IMPORTING ops TYPE i,
      itab_04_loop_a_std IMPORTING ops TYPE i,
      itab_04_loop_b_so_compl_key IMPORTING ops TYPE i,
      itab_04_loop_c_so_incompl_key IMPORTING ops TYPE i,
      itab_04_loop_d_so_incompl_k_la IMPORTING ops TYPE i,
      itab_04_loop_e_ha_compl_key IMPORTING ops TYPE i,
      itab_04_loop_z_ha_incompl_key IMPORTING ops TYPE i,
      itab_05_loop_no_where_a IMPORTING ops TYPE i,
      itab_05_loop_no_where_z IMPORTING ops TYPE i,
      itab_06_loop_target_z_wa IMPORTING ops TYPE i,
      itab_06_loop_target_a_fs IMPORTING ops TYPE i,
      itab_06_loop_target_b_dref IMPORTING ops TYPE i,
      itab_07_loop_modify_z_wa IMPORTING ops TYPE i,
      itab_07_loop_modify_a_fs IMPORTING ops TYPE i,
      itab_07_loop_modify_b_dref IMPORTING ops TYPE i,
      itab_08_loop_a IMPORTING ops TYPE i,
      itab_08_do_read_z IMPORTING ops TYPE i,
      itab_09_block_insert_a IMPORTING ops TYPE i,
      itab_09_block_insert_z IMPORTING ops TYPE i,
      itab_10_restrict_copy_a IMPORTING ops TYPE i,
      itab_10_restrict_copy_z IMPORTING ops TYPE i,
      itab_11_sec_key_a IMPORTING ops TYPE i,
      itab_11_sec_key_z IMPORTING ops TYPE i,
      itab_12_sec_key_freq_mod_a IMPORTING ops TYPE i,
      itab_12_sec_key_freq_mod_z IMPORTING ops TYPE i,
      itab_13_sort_itab_a IMPORTING ops TYPE i,
      itab_13_sort_itab_z IMPORTING ops TYPE i,
      itab_14_dyn_itab_spec_a IMPORTING ops TYPE i,
      itab_14_dyn_itab_spec_z IMPORTING ops TYPE i,
      itab_15_read_where_a IMPORTING ops TYPE i,
      itab_15_read_where_z IMPORTING ops TYPE i,
      itab_16_find_block_a IMPORTING ops TYPE i,
      itab_16_find_loop_z IMPORTING ops TYPE i,
      itab_18_replace_block_a IMPORTING ops TYPE i,
      itab_18_replace_loop_z IMPORTING ops TYPE i,
      itab_17_table_expr_mult_acc_a IMPORTING ops TYPE i,
      itab_17_table_expr_mult_acc_z IMPORTING ops TYPE i,
      eml_01_eml_in_loop_a IMPORTING ops TYPE i,
      eml_01_eml_in_loop_z IMPORTING ops TYPE i,
      misc_01_strings_a IMPORTING ops TYPE i,
      misc_01_strings_z IMPORTING ops TYPE i,
      misc_02_string_templates_a IMPORTING ops TYPE i,
      misc_02_string_templates_z IMPORTING ops TYPE i,
      misc_03_search_pattern_a IMPORTING ops TYPE i,
      misc_03_search_pattern_z IMPORTING ops TYPE i,
      misc_04_type_conversion_a IMPORTING ops TYPE i,
      misc_04_type_conversion_z IMPORTING ops TYPE i,
      misc_05_formal_parameters_a IMPORTING ops TYPE i,
      misc_05_formal_parameters_z IMPORTING ops TYPE i,
      misc_06_parallel_processing_a RETURNING VALUE(diff) TYPE tzntstmpl,
      misc_06_parallel_processing_z RETURNING VALUE(diff) TYPE tzntstmpl.

    "Demo types and data objects used by the examples
    TYPES: BEGIN OF ty_tab,
             comp1 TYPE i,
             comp2 TYPE i,
             comp3 TYPE i,
             comp4 TYPE c LENGTH 50,
             comp5 TYPE c LENGTH 50,
             comp6 TYPE i,
           END OF ty_tab.

    CLASS-DATA: base_tab                   TYPE TABLE OF ty_tab WITH EMPTY KEY,
                standard_tab               TYPE TABLE OF ty_tab WITH NON-UNIQUE KEY comp1,
                sorted_tab                 TYPE SORTED TABLE OF ty_tab WITH UNIQUE KEY comp1,
                hashed_tab                 TYPE HASHED TABLE OF ty_tab WITH UNIQUE KEY comp1,
                standard_tab_w_sec_key     TYPE TABLE OF ty_tab WITH EMPTY KEY WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS comp1,
                sorted_tab_mult_key_comp   TYPE SORTED TABLE OF ty_tab WITH UNIQUE KEY comp1 comp2 comp3,
                hashed_tab_mult_key_comp   TYPE HASHED TABLE OF ty_tab WITH UNIQUE KEY comp1 comp2 comp3,
                standard_tab_w_default_key TYPE TABLE OF ty_tab,
                key_tab                    TYPE TABLE OF zdemo_abap_tab1-key_field WITH EMPTY KEY,
                itab                       TYPE TABLE OF zdemo_abap_tab1 WITH EMPTY KEY,
                carr_tab                   TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY,
                str                        TYPE string,
                char_a                     TYPE c LENGTH 1000,
                char_b                     TYPE c LENGTH 1 VALUE '#',
                str_a                      TYPE string,
                str_b                      TYPE string VALUE `#`,
                num_1                      TYPE decfloat34 VALUE '1.2345',
                num_2                      TYPE decfloat34 VALUE '12345',
                num_3                      TYPE decfloat34 VALUE '12345',
                num_4                      TYPE decfloat34 VALUE '1.2345',
                num_5                      TYPE decfloat34 VALUE '1.2345',
                num_6                      TYPE decfloat34 VALUE '1.2345',
                res                        TYPE decfloat34,
                num_1b                     TYPE f VALUE '1.2345',
                num_2b                     TYPE i VALUE 12345,
                num_3b                     TYPE int8 VALUE 12345,
                num_4b                     TYPE p LENGTH 16 DECIMALS 8 VALUE '1.2345',
                num_5b                     TYPE decfloat16 VALUE '1.2345',
                num_6b                     TYPE decfloat34 VALUE '1.2345',
                res_b                      TYPE decfloat34.


    TYPES: BEGIN OF ty_wide,
             id   TYPE i,
             col1 TYPE c LENGTH 100,
             col2 TYPE c LENGTH 100,
             col3 TYPE c LENGTH 100,
             col4 TYPE c LENGTH 100,
             col5 TYPE c LENGTH 100,
           END OF ty_wide.

    CLASS-DATA: lt_wide   TYPE TABLE OF ty_wide WITH EMPTY KEY,
                str_tab_a TYPE string_table,
                str_tab_b TYPE string_table.

    TYPES: BEGIN OF s_info,
             method_name           TYPE abap_methname,
             subgroup              TYPE string,
             ops                   TYPE i,
             description           TYPE string,
             median_runtime_per_op TYPE decfloat34,
             relative_to_min       TYPE decfloat34,
           END OF s_info.
    CLASS-DATA t_info TYPE TABLE OF s_info WITH EMPTY KEY.

    TYPES: BEGIN OF ty_deep_tab,
             comp1 TYPE i,
             comp2 TYPE i,
             comp3 TYPE i,
             comp4 TYPE c LENGTH 50,
             comp5 TYPE c LENGTH 50,
             comp6 TYPE i,
             comp7 TYPE string_table,
             comp8 TYPE ty_tab,
           END OF ty_deep_tab.

    CLASS-DATA deep_standard_tab TYPE TABLE OF ty_deep_tab WITH EMPTY KEY.
    CLASS-DATA read_result TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m\\root.
    "CLASS-DATA result TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m\\root.

    CLASS-DATA upd TYPE TABLE FOR UPDATE zdemo_abap_rap_ro_m\\root.

    "Methods demonstrating parameter passing by value and reference
    METHODS: meth_pass_by_ref IMPORTING a TYPE i
                                        b TYPE i
                                        c LIKE deep_standard_tab
                                        d LIKE deep_standard_tab
                              EXPORTING e TYPE i
                                        f TYPE i
                                        g LIKE deep_standard_tab
                                        h LIKE deep_standard_tab,
      meth_pass_by_value IMPORTING VALUE(i) TYPE i
                                   VALUE(j) TYPE i
                                   VALUE(k) LIKE deep_standard_tab
                                   VALUE(l) LIKE deep_standard_tab
                         EXPORTING VALUE(m) TYPE i
                                   VALUE(n) TYPE i
                                   VALUE(o) LIKE deep_standard_tab
                                   VALUE(p) LIKE deep_standard_tab.

ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    me->out = out.

    t_info = VALUE #(
      "SQL
      ( subgroup = `SQL_01` method_name = 'SQL_01_REDUCE_T_ROWS_A'        ops = loop_count_5    description = `WHERE clause` )
      ( subgroup = `SQL_01` method_name = 'SQL_01_REDUCE_T_ROWS_Z'        ops = loop_count_5    description = `SELECT *, filter in LOOP (no WHERE)` )
      ( subgroup = `SQL_02` method_name = 'SQL_02_REDUCE_T_ROWS_A'        ops = loop_count_5    description = `SELECT SINGLE` )
      ( subgroup = `SQL_02` method_name = 'SQL_02_REDUCE_T_ROWS_B'        ops = loop_count_5    description = `SELECT *, then READ TABLE` )
      ( subgroup = `SQL_02` method_name = 'SQL_02_REDUCE_T_ROWS_Z'        ops = loop_count_5    description = `SELECT ... UP TO 1 ROWS` )
      ( subgroup = `SQL_03` method_name = 'SQL_03_REDUCE_T_ROWS_A'        ops = loop_count_5    description = `SELECT ... UP TO 1000 ROWS` )
      ( subgroup = `SQL_03` method_name = 'SQL_03_REDUCE_T_ROWS_Z'        ops = loop_count_5    description = `SELECT *, DELETE from index` )
      ( subgroup = `SQL_04` method_name = 'SQL_04_REDUCE_DATA_VOLUME_A'   ops = loop_count_5    description = `SELECT 2 fields only from table (assumption: only these two are needed)` )
      ( subgroup = `SQL_04` method_name = 'SQL_04_REDUCE_DATA_VOLUME_Z'   ops = loop_count_5    description = `SELECT * from same table (assumption: only two are needed)` )
      ( subgroup = `SQL_05` method_name = 'SQL_05_REDUCE_DB_ACCESS_A'     ops = loop_count_5    description = `Single SELECT` )
      ( subgroup = `SQL_05` method_name = 'SQL_05_REDUCE_DB_ACCESS_Z'     ops = loop_count_5    description = `SELECT SINGLE in loop` )
      ( subgroup = `SQL_06` method_name = 'SQL_06_REDUCE_DB_ACCESS_A'     ops = loop_count_5    description = `INNER JOIN with host variable itab` )
      ( subgroup = `SQL_06` method_name = 'SQL_06_REDUCE_DB_ACCESS_B'     ops = loop_count_5    description = `WHERE EXISTS subquery` )
      ( subgroup = `SQL_06` method_name = 'SQL_06_REDUCE_DB_ACCESS_C'     ops = loop_count_5    description = `FOR ALL ENTRIES` )
      ( subgroup = `SQL_06` method_name = 'SQL_06_REDUCE_DB_ACCESS_Z'     ops = loop_count_5    description = `SELECT in LOOP (row-by-row)` )
      ( subgroup = `SQL_07` method_name = 'SQL_07_REDUCE_DB_ACCESS_A'     ops = loop_count_5    description = `MODIFY FROM TABLE (block)` )
      ( subgroup = `SQL_07` method_name = 'SQL_07_REDUCE_DB_ACCESS_Z'     ops = loop_count_5    description = `MODIFY row-by-row in LOOP` )
      ( subgroup = `SQL_08` method_name = 'SQL_08_REDUCE_DB_ACCESS_A'     ops = loop_count_5    description = `LEFT OUTER JOIN` )
      ( subgroup = `SQL_08` method_name = 'SQL_08_REDUCE_DB_ACCESS_Z'     ops = loop_count_5    description = `Nested SELECT loop (ENDSELECT)` )
      ( subgroup = `SQL_09` method_name = 'SQL_09_UPDATE_A'               ops = loop_count_5    description = `UPDATE ... SET (set-based)` )
      ( subgroup = `SQL_09` method_name = 'SQL_09_UPDATE_Z'               ops = loop_count_5    description = `UPDATE row-by-row in SELECT loop` )
      ( subgroup = `SQL_11` method_name = 'SQL_11_DYNAMIC_SQL_A'          ops = loop_count_250  description = `Static SQL specification` )
      ( subgroup = `SQL_11` method_name = 'SQL_11_DYNAMIC_SQL_Z'          ops = loop_count_250  description = `Dynamic SQL specification` )
      "ITAB
      ( subgroup = `ITAB_01` method_name = 'ITAB_01_TABLE_KEY_READ_A_STD'   ops = loop_count_500 description = `READ TABLE standard, primary key` )
      ( subgroup = `ITAB_01` method_name = 'ITAB_01_TABLE_KEY_READ_B_SO'    ops = loop_count_500 description = `READ TABLE sorted, primary key` )
      ( subgroup = `ITAB_01` method_name = 'ITAB_01_TABLE_KEY_READ_Z_HA'    ops = loop_count_500 description = `READ TABLE hashed, primary key` )
      ( subgroup = `ITAB_02` method_name = 'ITAB_02_INCOMPLETE_T_KEY_A_SO'  ops = loop_count_500 description = `Sorted, partial key (left-aligned)` )
      ( subgroup = `ITAB_02` method_name = 'ITAB_02_INCOMPLETE_T_KEY_B_HA'  ops = loop_count_500 description = `Hashed, partial key` )
      ( subgroup = `ITAB_02` method_name = 'ITAB_02_INCOMPLETE_T_KEY_Z_SO'  ops = loop_count_500 description = `Sorted, partial key (not left-aligned)` )
      ( subgroup = `ITAB_03` method_name = 'ITAB_03_STD_BINARY_SEARCH_A'    ops = loop_count_500 description = `READ TABLE standard + BINARY SEARCH` )
      ( subgroup = `ITAB_03` method_name = 'ITAB_03_STD_BINARY_SEARCH_Z'    ops = loop_count_500 description = `READ TABLE standard, no BINARY SEARCH` )
      ( subgroup = `ITAB_04` method_name = 'ITAB_04_LOOP_A_STD'             ops = loop_count_500 description = `LOOP WHERE, standard table` )
      ( subgroup = `ITAB_04` method_name = 'ITAB_04_LOOP_B_SO_COMPL_KEY'    ops = loop_count_500 description = `LOOP WHERE, sorted, full key` )
      ( subgroup = `ITAB_04` method_name = 'ITAB_04_LOOP_C_SO_INCOMPL_KEY'  ops = loop_count_500 description = `LOOP WHERE, sorted, partial key (not left-aligned)` )
      ( subgroup = `ITAB_04` method_name = 'ITAB_04_LOOP_D_SO_INCOMPL_K_LA' ops = loop_count_500 description = `LOOP WHERE, sorted, partial key (left-aligned)` )
      ( subgroup = `ITAB_04` method_name = 'ITAB_04_LOOP_E_HA_COMPL_KEY'    ops = loop_count_500 description = `LOOP WHERE, hashed, full key` )
      ( subgroup = `ITAB_04` method_name = 'ITAB_04_LOOP_Z_HA_INCOMPL_KEY'  ops = loop_count_500 description = `LOOP WHERE, hashed, partial key` )
      ( subgroup = `ITAB_05` method_name = 'ITAB_05_LOOP_NO_WHERE_A'        ops = loop_count_500 description = `LOOP WHERE comp1 > 800` )
      ( subgroup = `ITAB_05` method_name = 'ITAB_05_LOOP_NO_WHERE_Z'        ops = loop_count_500 description = `LOOP + IF inside (no WHERE)` )
      ( subgroup = `ITAB_06` method_name = 'ITAB_06_LOOP_TARGET_A_FS'       ops = loop_count_500 description = `LOOP ASSIGNING field symbol (deep table)` )
      ( subgroup = `ITAB_06` method_name = 'ITAB_06_LOOP_TARGET_B_DREF'     ops = loop_count_500 description = `LOOP REFERENCE INTO data ref (deep table)` )
      ( subgroup = `ITAB_06` method_name = 'ITAB_06_LOOP_TARGET_Z_WA'       ops = loop_count_500 description = `LOOP INTO work area (deep table)` )
      ( subgroup = `ITAB_07` method_name = 'ITAB_07_LOOP_MODIFY_A_FS'       ops = loop_count_500 description = `LOOP ASSIGNING FS, direct modification` )
      ( subgroup = `ITAB_07` method_name = 'ITAB_07_LOOP_MODIFY_B_DREF'     ops = loop_count_500 description = `LOOP REFERENCE dref, modify via dref` )
      ( subgroup = `ITAB_07` method_name = 'ITAB_07_LOOP_MODIFY_Z_WA'       ops = loop_count_500 description = `LOOP INTO wa, MODIFY FROM wa` )
      ( subgroup = `ITAB_08` method_name = 'ITAB_08_LOOP_A'                 ops = loop_count_250  description = `LOOP AT (sequential cursor)` )
      ( subgroup = `ITAB_08` method_name = 'ITAB_08_DO_READ_Z'              ops = loop_count_250  description = `DO + READ TABLE INDEX (manual iteration)` )
      ( subgroup = `ITAB_09` method_name = 'ITAB_09_BLOCK_INSERT_A'         ops = loop_count_500 description = `APPEND LINES OF (block)` )
      ( subgroup = `ITAB_09` method_name = 'ITAB_09_BLOCK_INSERT_Z'         ops = loop_count_500 description = `APPEND in LOOP (row-by-row)` )
      ( subgroup = `ITAB_10` method_name = 'ITAB_10_RESTRICT_COPY_A'        ops = loop_count_500 description = `MODIFY TRANSPORTING (selected fields)` )
      ( subgroup = `ITAB_10` method_name = 'ITAB_10_RESTRICT_COPY_Z'        ops = loop_count_500 description = `MODIFY without TRANSPORTING (all fields)` )
      ( subgroup = `ITAB_11` method_name = 'ITAB_11_SEC_KEY_A'              ops = loop_count_500 description = `READ TABLE WITH TABLE KEY (secondary key)` )
      ( subgroup = `ITAB_11` method_name = 'ITAB_11_SEC_KEY_Z'              ops = loop_count_500 description = `READ TABLE WITH TABLE KEY (primary key)` )
      ( subgroup = `ITAB_12` method_name = 'ITAB_12_SEC_KEY_FREQ_MOD_A'     ops = loop_count_500 description = `Secondary key + LOOP with READ (no writes)` )
      ( subgroup = `ITAB_12` method_name = 'ITAB_12_SEC_KEY_FREQ_MOD_Z'     ops = loop_count_500 description = `Secondary key + LOOP with READ and writes` )
      ( subgroup = `ITAB_13` method_name = 'ITAB_13_SORT_ITAB_A'            ops = loop_count_500 description = `SORT BY comp1 (explicit key)` )
      ( subgroup = `ITAB_13` method_name = 'ITAB_13_SORT_ITAB_Z'            ops = loop_count_500 description = `SORT (implicit standard key)` )
      ( subgroup = `ITAB_14` method_name = 'ITAB_14_DYN_ITAB_SPEC_A'        ops = loop_count_500 description = `READ TABLE with static key specification` )
      ( subgroup = `ITAB_14` method_name = 'ITAB_14_DYN_ITAB_SPEC_Z'        ops = loop_count_500 description = `READ TABLE with dynamic key specification` )
      ( subgroup = `ITAB_15` method_name = 'ITAB_15_READ_WHERE_A'           ops = loop_count_100 description = `READ TABLE WITH KEY` )
      ( subgroup = `ITAB_15` method_name = 'ITAB_15_READ_WHERE_Z'           ops = loop_count_100 description = `READ TABLE WHERE (linear search)` )
      ( subgroup = `ITAB_16` method_name = 'ITAB_16_FIND_BLOCK_A'           ops = loop_count_100  description = `FIND IN TABLE (block)` )
      ( subgroup = `ITAB_16` method_name = 'ITAB_16_FIND_LOOP_Z'            ops = loop_count_100 description = `FIND in LOOP (row-by-row)` )
      ( subgroup = `ITAB_17` method_name = 'ITAB_17_TABLE_EXPR_MULT_ACC_A'   ops = loop_count_500  description = `Multiple reads and writes using table lines assigned to field symbols` )
      ( subgroup = `ITAB_17` method_name = 'ITAB_17_TABLE_EXPR_MULT_ACC_Z'   ops = loop_count_500 description = `Multiple reads and writes using individual table expressions` )
      ( subgroup = `ITAB_18` method_name = 'ITAB_18_REPLACE_BLOCK_A'        ops = loop_count_100  description = `REPLACE IN TABLE (block)` )
      ( subgroup = `ITAB_18` method_name = 'ITAB_18_REPLACE_LOOP_Z'         ops = loop_count_100 description = `REPLACE in LOOP (row-by-row)` )
      "EML
      ( subgroup = `EML_01`  method_name = 'EML_01_EML_IN_LOOP_A'           ops = loop_count_5    description = `MODIFY ENTITIES with FOR loop (block)` )
      ( subgroup = `EML_01`  method_name = 'EML_01_EML_IN_LOOP_Z'           ops = loop_count_5    description = `MODIFY ENTITIES per row in LOOP` )
      "MISC
      ( subgroup = `MISC_01` method_name = 'MISC_01_STRINGS_A'              ops = loop_count_1000 description = `Concatenate TYPE string` )
      ( subgroup = `MISC_01` method_name = 'MISC_01_STRINGS_Z'              ops = loop_count_1000 description = `Concatenate TYPE c` )
      ( subgroup = `MISC_02` method_name = 'MISC_02_STRING_TEMPLATES_A'     ops = loop_count_1000            description = `String assignment: backquote literal` )
      ( subgroup = `MISC_02` method_name = 'MISC_02_STRING_TEMPLATES_Z'     ops = loop_count_1000            description = `String assignment: template (no expression)` )
      ( subgroup = `MISC_03` method_name = 'MISC_03_SEARCH_PATTERN_A'       ops = loop_count_250   description = `Pattern search: IF ... CP` )
      ( subgroup = `MISC_03` method_name = 'MISC_03_SEARCH_PATTERN_Z'       ops = loop_count_250   description = `Pattern search: FIND PCRE` )
      ( subgroup = `MISC_04` method_name = 'MISC_04_TYPE_CONVERSION_A'      ops = loop_count_1000 description = `Arithmetic with same types (decfloat34)` )
      ( subgroup = `MISC_04` method_name = 'MISC_04_TYPE_CONVERSION_Z'      ops = loop_count_1000 description = `Arithmetic with mixed types (implicit conversions)` )
      ( subgroup = `MISC_05` method_name = 'MISC_05_FORMAL_PARAMETERS_A'    ops = loop_count_30  description = `Pass parameters by reference` )
      ( subgroup = `MISC_05` method_name = 'MISC_05_FORMAL_PARAMETERS_Z'    ops = loop_count_30  description = `Pass parameters by value` )

    ).

    "Run all benchmarks
    LOOP AT t_info REFERENCE INTO DATA(info).
      CLEAR runtime_tab.
      DO repetitions TIMES.
        prepare( info->method_name ).
        ts1 = utclong_current( ).
        CALL METHOD (info->method_name) EXPORTING ops = info->ops.
        store_runtime( high = utclong_current( ) low = ts1 ).
      ENDDO.
      info->median_runtime_per_op = get_median_runtime( ) / info->ops.
    ENDLOOP.

    "Compute relative_to_min per subgroup
    DATA sg TYPE string.
    DATA min_t TYPE decfloat34.
    LOOP AT t_info REFERENCE INTO info.
      IF info->subgroup <> sg.
        sg = info->subgroup.
        min_t = info->median_runtime_per_op.
        LOOP AT t_info REFERENCE INTO DATA(scan) WHERE subgroup = sg.
          IF scan->median_runtime_per_op < min_t.
            min_t = scan->median_runtime_per_op.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF min_t > 0.
        info->relative_to_min = info->median_runtime_per_op / min_t.
      ENDIF.
    ENDLOOP.

    "Output results
    TYPES: BEGIN OF s_result,
             method_name           TYPE abap_methname,
             description           TYPE string,
             ops                   TYPE i,
             median_runtime_per_op TYPE p LENGTH 8 DECIMALS 3,
             relative_to_min       TYPE p LENGTH 8 DECIMALS 3,
           END OF s_result.
    DATA t_sg_result TYPE TABLE OF s_result WITH EMPTY KEY.
    DATA curr_cat TYPE string.
    DATA curr_sg TYPE string.
    LOOP AT t_info REFERENCE INTO info.
      DATA(cat) = COND string(
        WHEN info->subgroup(3) = `SQL` THEN `SQL`
        WHEN info->subgroup(3) = `ITA` THEN `ITAB`
        WHEN info->subgroup(3) = `MIS` THEN `MISC`
        WHEN info->subgroup(3) = `EML` THEN `EML`
        ELSE info->subgroup(3)
      ).
      IF cat <> curr_cat.
        IF curr_sg IS NOT INITIAL.
          SORT t_sg_result BY median_runtime_per_op ASCENDING.
          out->write( t_sg_result ).
          out->write( |\n| ).
          out->write( |\n| ).
          CLEAR t_sg_result.
        ENDIF.
        curr_cat = cat.
        curr_sg = info->subgroup.
        write_section( cat ).
        out->write( |\n| ).
        out->write( |--- { curr_sg } { repeat( val = `-` occ = 100 ) }| ).
        out->write( |\n| ).
        out->write( |\n| ).
      ELSEIF info->subgroup <> curr_sg.
        SORT t_sg_result BY median_runtime_per_op ASCENDING.
        out->write( t_sg_result ).
        CLEAR t_sg_result.
        curr_sg = info->subgroup.
        out->write( |\n| ).
        out->write( |\n| ).
        out->write( |--- { curr_sg } { repeat( val = `-` occ = 100 ) }| ).
        out->write( |\n| ).
        out->write( |\n| ).
      ENDIF.
      APPEND VALUE #(
        method_name           = info->method_name
        description           = info->description
        ops                   = info->ops
        median_runtime_per_op = info->median_runtime_per_op * 1000000
        relative_to_min       = info->relative_to_min
      ) TO t_sg_result.
    ENDLOOP.
    IF t_sg_result IS NOT INITIAL.
      SORT t_sg_result BY median_runtime_per_op ASCENDING.
      out->write( t_sg_result ).
    ENDIF.

    "Parallel processing uses a different measurement approach
    out->write( |\n| ).
    out->write( |\n| ).
    write_section( `MISC - PARALLEL PROCESSING` ).
    out->write( |\n| ).
    out->write( |--- MISC_06 { repeat( val = `-` occ = 100 ) }| ).
    out->write( |\n| ).
    DATA(diff_par) = misc_06_parallel_processing_a( ).
    out->write( |\n| ).
    DATA(diff_no_par) = misc_06_parallel_processing_z( ).
    out->write( |\n| ).
    IF diff_par > 0 AND diff_no_par > 0.
      DATA min_diff LIKE diff_par.
      min_diff = nmin( val1 = diff_par val2 = diff_no_par ).
      IF min_diff IS NOT INITIAL.
        DATA(rel_par) = CONV decfloat34( diff_par ) / min_diff.
        DATA(rel_seq) = CONV decfloat34( diff_no_par ) / min_diff.
        out->write( |Relative to fastest - parallel (A):   { rel_par STYLE = SIMPLE }| ).
        out->write( |Relative to fastest - sequential (Z): { rel_seq STYLE = SIMPLE DECIMALS = 3 }| ).
      ENDIF.
    ENDIF.
    out->write( |\n| ).
  ENDMETHOD.

  METHOD store_runtime.
    cl_abap_utclong=>diff( EXPORTING high    = high
                                     low     = low
                           IMPORTING seconds = DATA(seconds) ).

    APPEND seconds TO runtime_tab.
  ENDMETHOD.

  METHOD write_section.
    out->write( |*&{ repeat( val = `-` occ = 120 ) }*| ).
    out->write( |*& { text }| ).
    out->write( |*&{ repeat( val = `-` occ = 120 ) }*| ).
    out->write( |\n| ).
  ENDMETHOD.

  METHOD prepare.
    CASE method_name.
      WHEN 'ITAB_09_BLOCK_INSERT_A'
        OR 'ITAB_09_BLOCK_INSERT_Z'.
        CLEAR str_tab_a.
        CLEAR str_tab_b.
        DO 1000 TIMES.
          APPEND sy-tabix TO str_tab_a.
        ENDDO.
      WHEN 'ITAB_16_FIND_BLOCK_A'
        OR 'ITAB_16_FIND_LOOP_Z'
        OR 'ITAB_18_REPLACE_BLOCK_A'
        OR 'ITAB_18_REPLACE_LOOP_Z'.
        CLEAR str_tab_a.
        DO 100 TIMES.
          APPEND |ABAP{ sy-index }| TO str_tab_a.
        ENDDO.
      WHEN 'SQL_04_REDUCE_DATA_VOLUME_A'
        OR 'SQL_04_REDUCE_DATA_VOLUME_Z'.
        CLEAR lt_wide.
        DO 1000 TIMES.
          INSERT VALUE ty_wide( id = sy-tabix col1 = 'X' col2 = 'X' col3 = 'X' col4 = 'X' col5 = 'X' ) INTO TABLE lt_wide.
        ENDDO.
      WHEN `MISC_03_SEARCH_PATTERN_A` OR
            `MISC_03_SEARCH_PATTERN_Z`.
        str = `abc_def_ghi`.
      WHEN `ITAB_17_TABLE_EXPR_MULT_ACC_A`
      OR `ITAB_17_TABLE_EXPR_MULT_ACC_Z`.
        standard_tab = base_tab.
      WHEN `MISC_01_STRINGS_A`
            OR `MISC_01_STRINGS_Z`.
        CLEAR str_a.
        CLEAR char_a.

    ENDCASE.
  ENDMETHOD.

  METHOD class_constructor.
    "On class execution, multiple tasks are performed preparing the demo.

    "Clearing and populating demo database tables
    zcl_demo_abap_aux=>fill_dbtabs( ).
    DELETE FROM zdemo_abap_tabca.

    DATA rap_tab TYPE TABLE OF zdemo_abap_tab1 WITH EMPTY KEY.

    DO 10 TIMES.
      APPEND VALUE #( key_field = sy-index num1 = sy-index num2 = sy-index ) TO rap_tab.
    ENDDO.

    DELETE FROM zdemo_abap_rapt1.
    INSERT zdemo_abap_rapt1 FROM TABLE @rap_tab.

    DATA demo_tab TYPE TABLE OF zdemo_abap_tab1 WITH EMPTY KEY.

    DO 10000 TIMES.
      APPEND VALUE #( key_field = sy-index char1 = sy-index char2 = sy-index num1 = sy-index num2 = sy-index ) TO demo_tab.
    ENDDO.

    DELETE FROM zdemo_abap_tab1.
    INSERT zdemo_abap_tab1 FROM TABLE @demo_tab.

    "Populating demo internal tables
    DO loop_count_100 TIMES.
      APPEND sy-index TO key_tab.
    ENDDO.

    carr_tab = VALUE #( ( carrid = 'AB' carrname = 'AB Airways' )
          ( carrid = 'CD' carrname = 'CD Airways' )
          ( carrid = 'EF' carrname = 'EF Airways' )
          ( carrid = 'GH' carrname = 'GH Airways' )
          ( carrid = 'IJ' carrname = 'IJ Airways' ) ).

    DO loop_count_1000 TIMES.
      INSERT VALUE #( comp1 = sy-index comp2 = sy-index comp3 = sy-index comp4 = sy-index comp5 = sy-index comp6 = sy-index ) INTO TABLE base_tab ASSIGNING FIELD-SYMBOL(<fs>).

      INSERT CORRESPONDING #( <fs> ) INTO TABLE deep_standard_tab ASSIGNING FIELD-SYMBOL(<deep>).
      <deep>-comp7 = VALUE #( ( `A` ) ( `B` ) ( `A` ) ( `P` ) ).
      <deep>-comp8 = <fs>.
    ENDDO.

    sorted_tab = base_tab.
    standard_tab = base_tab.
    hashed_tab = base_tab.
    standard_tab_w_sec_key = base_tab.
    sorted_tab_mult_key_comp = base_tab.
    hashed_tab_mult_key_comp = base_tab.
    standard_tab_w_default_key = base_tab.

    DO loop_count_5 TIMES.
      APPEND VALUE #( key_field = sy-index field3 = sy-index field4 = sy-index ) TO read_result.
    ENDDO.
    upd = VALUE #( FOR wa IN read_result ( %tky = wa-%tky field3 = wa-field3 + 1 field4 = wa-field4 + 1 ) ).
  ENDMETHOD.

  METHOD sql_11_dynamic_sql_a.
    "Using static specification in ABAP SQL statement

    DO ops TIMES.
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
        FROM zdemo_abap_tab1
         WHERE key_field < 1000
         INTO TABLE NEW @DATA(dref_fl).
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_11_dynamic_sql_z.
    "Dynamic specification in ABAP SQL statement

    DO ops TIMES.
**********************************************************************
      SELECT ('KEY_FIELD, CHAR1, CHAR2, NUM1, NUM2')
        FROM ('ZDEMO_ABAP_TAB1')
        WHERE (`KEY_FIELD < 1000`)
        INTO TABLE NEW @DATA(dref_fl2).
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_01_reduce_t_rows_a.
    "ABAP SQL statement specifying a WHERE clause

    DO ops TIMES.
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
        FROM zdemo_abap_tab1
        WHERE key_field < 1000
        INTO TABLE @DATA(it1).
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_01_reduce_t_rows_z.
    "ABAP SQL statement not specifying a WHERE clause
    "Instead, reading all data from the database table, processing
    "the content further in a loop, and filtering out data sets.

    DO ops TIMES.
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
        FROM zdemo_abap_tab1
        INTO TABLE @DATA(it2).

      LOOP AT it2 REFERENCE INTO DATA(dref2).
        IF dref2->key_field < 1000.
          DELETE it2 INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_02_reduce_t_rows_a.
    "Using SELECT SINGLE to read a single line

    DO ops TIMES.
**********************************************************************
      SELECT SINGLE key_field, char1, char2, num1, num2
              FROM zdemo_abap_tab1
              WHERE key_field = 5000
              INTO @DATA(line).
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_02_reduce_t_rows_b.
    "Reading all data sets from the database table, and reading
    "an entry using READ TABLE.

    DO ops TIMES.
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
         FROM zdemo_abap_tab1
         INTO TABLE @DATA(it3).

      READ TABLE it3 INTO DATA(line4) WITH KEY key_field = 5000.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_02_reduce_t_rows_z.
    "Specifying UP TO 1 ROWS to retrieve a single row

    DO ops TIMES.
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
        FROM zdemo_abap_tab1
        WHERE key_field = 5000
        INTO TABLE @DATA(it4)
        UP TO 1 ROWS.
**********************************************************************
    ENDDO.

  ENDMETHOD.


  METHOD sql_03_reduce_t_rows_a.
    "Reading a specific number of data sets from the database table using
    "the UP TO n ROWS addition

    DO ops TIMES.
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
         FROM zdemo_abap_tab1
         INTO TABLE @DATA(it5)
         UP TO 1000 ROWS.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_03_reduce_t_rows_z.
    "Reading all database table entries and further process the internal table
    "content

    DO ops TIMES.
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
             FROM zdemo_abap_tab1
             INTO TABLE @DATA(it6).

      DELETE it6 FROM 1001.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_04_reduce_data_volume_a.
    "Select only 2 of 6 fields from a wide in-memory table

    DO ops TIMES.
**********************************************************************
      SELECT id, col1 FROM @lt_wide AS tab INTO TABLE @DATA(it).
**********************************************************************
    ENDDO.

  ENDMETHOD.

  METHOD sql_04_reduce_data_volume_z.
    "SELECT * fetches all 6 fields

    DO ops TIMES.
**********************************************************************
      SELECT * FROM @lt_wide AS tab INTO TABLE @DATA(it).
**********************************************************************
    ENDDO.

  ENDMETHOD.

  METHOD sql_05_reduce_db_access_a.
    "Single database access

    DO ops TIMES.
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
            FROM zdemo_abap_tab1
            INTO TABLE @DATA(it9)
            UP TO 5 ROWS.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_05_reduce_db_access_z.
    "Repeated database access in a loop

    DO ops TIMES.
**********************************************************************
      DO 5 TIMES.
        SELECT SINGLE key_field, char1, char2, num1, num2
          FROM zdemo_abap_tab1
          INTO @DATA(fl_row).
      ENDDO.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_06_reduce_db_access_a.
    "Using an inner join

    DO ops TIMES.
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
          FROM zdemo_abap_tab1
          INNER JOIN @key_tab AS tab
          ON zdemo_abap_tab1~key_field = tab~table_line
          INTO CORRESPONDING FIELDS OF TABLE @itab.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_06_reduce_db_access_b.
    "Using WHERE EXISTS

    DO ops TIMES.
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
        FROM zdemo_abap_tab1
        WHERE EXISTS ( SELECT 'X' FROM @key_tab AS tab WHERE zdemo_abap_tab1~key_field = tab~table_line )
        INTO CORRESPONDING FIELDS OF TABLE @itab.
**********************************************************************
    ENDDO.


  ENDMETHOD.


  METHOD sql_06_reduce_db_access_c.
    "Using the FOR ALL ENTRIES addition

    DO ops TIMES.
**********************************************************************
      IF key_tab IS NOT INITIAL.
        SELECT key_field, char1, char2, num1, num2
         FROM zdemo_abap_tab1
         FOR ALL ENTRIES IN @key_tab
         WHERE key_field = @key_tab-table_line
         INTO CORRESPONDING FIELDS OF TABLE @itab.
      ENDIF.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_06_reduce_db_access_z.
    "Repeated database access in a loop

    DO ops TIMES.
**********************************************************************
      LOOP AT key_tab INTO DATA(wa).
        SELECT key_field, char1, char2, num1, num2
          FROM zdemo_abap_tab1
          WHERE key_field = @wa
          APPENDING CORRESPONDING FIELDS OF TABLE @itab.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_07_reduce_db_access_a.
    "Block-wise modify operation

    DO ops TIMES.
**********************************************************************
      MODIFY zdemo_abap_carr FROM TABLE @carr_tab.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_07_reduce_db_access_z.
    "Line by line modify operations

    DO ops TIMES.
**********************************************************************
      LOOP AT carr_tab INTO DATA(wa_carr).
        MODIFY zdemo_abap_carr FROM @wa_carr.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_08_reduce_db_access_a.
    "Join

    DO ops TIMES.
**********************************************************************
      SELECT fl~carrid, carr~carrname, fl~connid, fl~cityfrom, fl~cityto
         FROM zdemo_abap_flsch AS fl
            LEFT OUTER JOIN zdemo_abap_carr AS carr
              ON fl~carrid = carr~carrid
         INTO TABLE @DATA(it10).
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_08_reduce_db_access_z.
    "Nested SELECT loop

    DO ops TIMES.
**********************************************************************
      SELECT carrid, CAST( ' ' AS CHAR( 20 ) ) AS carrname,
       connid, cityfrom, cityto
       FROM zdemo_abap_flsch
       INTO @DATA(wa_fl).
        SELECT SINGLE carrname
               FROM zdemo_abap_carr
               WHERE carrid = @wa_fl-carrid
               INTO (@wa_fl-carrname).
        DATA it11 LIKE TABLE OF wa_fl WITH EMPTY KEY.
        it11 = VALUE #( BASE it11 ( wa_fl ) ).
      ENDSELECT.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_09_update_a.
    "UPDATE ... SET statement

    DO ops TIMES.
**********************************************************************
      UPDATE zdemo_abap_tab1 SET char1 = 'X' WHERE key_field < 20.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD sql_09_update_z.
    "UPDATE statement using a work area

    DO ops TIMES.
**********************************************************************
      SELECT *
        FROM zdemo_abap_tab1
        WHERE key_field < 20
        INTO @DATA(wa_tab).
        wa_tab-char1 = 'X'.
        UPDATE zdemo_abap_tab1 FROM @wa_tab.
      ENDSELECT.
**********************************************************************
    ENDDO.
  ENDMETHOD.

  METHOD itab_01_table_key_read_z_ha.
    "Reading from a hashed table using the primary table key

    DO ops TIMES.
**********************************************************************
      READ TABLE hashed_tab TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_01_table_key_read_b_so.
    "Reading from a sorted table using the primary table key

    DO ops TIMES.
**********************************************************************
      READ TABLE sorted_tab TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_01_table_key_read_a_std.
    "Reading from a standard table using the primary table key

    DO ops TIMES.
**********************************************************************
      READ TABLE standard_tab TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_02_incomplete_t_key_b_ha.
    "Reading from a hashed table using a free key
    "The primary table key is only partially used.

    DO ops TIMES.
**********************************************************************
      READ TABLE hashed_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp1 = sy-index comp2 = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_02_incomplete_t_key_a_so.
    "Reading from a sorted table using a free key
    "The primary table key is only partially used.

    DO ops TIMES.
**********************************************************************
      READ TABLE sorted_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp1 = sy-index comp2 = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_02_incomplete_t_key_z_so.
    "Reading from a sorted table using a free key
    "The primary table key is only partially used. The key is not left-aligned.

    DO ops TIMES.
**********************************************************************
      READ TABLE sorted_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp2 = sy-index comp3 = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD get_median_runtime.
    DATA sorted TYPE TABLE OF decfloat34 WITH EMPTY KEY.
    sorted = runtime_tab.
    SORT sorted BY table_line ASCENDING.
    DATA(n) = lines( sorted ).
    IF n > 0.
      READ TABLE sorted INDEX ( n DIV 2 + 1 ) INTO median.
    ENDIF.
    CLEAR runtime_tab.
  ENDMETHOD.

  METHOD itab_03_std_binary_search_a.
    "Reading from a standard table using a free key and the BINARY SEARCH addition.
    "A SORT statement is executed beforehand.

    SORT standard_tab BY comp2 ASCENDING.

    DO ops TIMES.
**********************************************************************
      READ TABLE standard_tab TRANSPORTING NO FIELDS WITH KEY comp2 = sy-index BINARY SEARCH.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_03_std_binary_search_z.
    "Reading from a standard table using a free key. Here, the BINARY SEARCH addition
    "is not specified. A SORT statement is executed beforehand.

    SORT standard_tab BY comp2 ASCENDING.

    DO ops TIMES.
**********************************************************************
      READ TABLE standard_tab TRANSPORTING NO FIELDS WITH KEY comp2 = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_04_loop_a_std.
    "A standard table is looped across, specifying a WHERE condition.

    DO ops TIMES.
**********************************************************************
      LOOP AT standard_tab REFERENCE INTO DATA(dref) WHERE comp1 = 800 AND comp2 = '800' AND comp3 = '800'.
        ...
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_04_loop_b_so_compl_key.
    "A sorted table is looped across, specifying a WHERE condition.
    "The full key is specified.

    DO ops TIMES.
**********************************************************************
      LOOP AT sorted_tab_mult_key_comp REFERENCE INTO DATA(dref) WHERE comp1 = 800 AND comp2 = '800' AND comp3 = '800'.
        ...
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_04_loop_c_so_incompl_key.
    "A sorted table is looped across, specifying a WHERE condition.
    "The key is partially specified, but not left-aligned.

    DO ops TIMES.
**********************************************************************
      LOOP AT sorted_tab_mult_key_comp REFERENCE INTO DATA(dref) WHERE comp2 = '800' AND comp3 = '800'.
        ...
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_04_loop_d_so_incompl_k_la.
    "A sorted table is looped across, specifying a WHERE condition.
    "The key is partially specified, left-aligned.

    DO ops TIMES.
**********************************************************************
      LOOP AT sorted_tab_mult_key_comp REFERENCE INTO DATA(dref) WHERE comp1 = 800 AND comp2 = '800' .
        ...
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_04_loop_e_ha_compl_key.
    "A hashed table is looped across, specifying a WHERE condition.
    "The key is fully specified.

    DO ops TIMES.
**********************************************************************
      LOOP AT hashed_tab_mult_key_comp REFERENCE INTO DATA(dref) WHERE comp1 = 800 AND comp2 = '800' AND comp3 = '800' .
        ...
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_04_loop_z_ha_incompl_key.
    "A hashed table is looped across, specifying a WHERE condition.
    "The key is partially specified.

    DO ops TIMES.
**********************************************************************
      LOOP AT hashed_tab_mult_key_comp REFERENCE INTO DATA(dref) WHERE comp1 = 800 AND comp2 = '800'.
        ...
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_05_loop_no_where_a.
    "A standard table is looped across, specifying a WHERE condition.

    DO ops TIMES.
**********************************************************************
      LOOP AT standard_tab REFERENCE INTO DATA(dref) WHERE comp1 > 800.
        ...
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_05_loop_no_where_z.
    "A standard table is looped across, not specifying a WHERE condition.
    "Instead, an IF constrol structure is implemented in the loop.

    DO ops TIMES.
**********************************************************************
      LOOP AT standard_tab REFERENCE INTO DATA(dref).
        IF dref->comp1 > 800.
          CONTINUE.
        ENDIF.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_06_loop_target_z_wa.
    "A deep standard table is looped across. The target area is a work area.

    DO ops TIMES.
**********************************************************************
      LOOP AT deep_standard_tab INTO DATA(wa) WHERE comp1 < 200.
        ...
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_06_loop_target_a_fs.
    "A deep standard table is looped across. The target area is a field symbol.

    DO ops TIMES.
**********************************************************************
      LOOP AT deep_standard_tab ASSIGNING FIELD-SYMBOL(<fs>) WHERE comp1 < 200.
        ...
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_06_loop_target_b_dref.
    "A deep standard table is looped across. The target area is a data reference variable.

    DO ops TIMES.
**********************************************************************
      LOOP AT deep_standard_tab REFERENCE INTO DATA(dref) WHERE comp1 < 200.
        ...
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_07_loop_modify_z_wa.
    "A standard table is looped across. The target area is a work area.
    "The read table line is modified using a MODIFY statement based
    "on the work area.

    DO ops TIMES.
**********************************************************************
      LOOP AT standard_tab INTO DATA(wa) WHERE comp1 < 800.
        wa-comp6 += 1.
        MODIFY standard_tab FROM wa.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_07_loop_modify_a_fs.
    "A standard table is looped across. The target area is a field symbol.
    "The read table line is directly modified using the field symbol.

    DO ops TIMES.
**********************************************************************
      LOOP AT standard_tab ASSIGNING FIELD-SYMBOL(<fs>) WHERE comp1 < 800.
        <fs>-comp6 += 1.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_07_loop_modify_b_dref.
    "A standard table is looped across. The target area is a data reference variable.
    "The read table line is directly modified using the data reference variable.

    DO ops TIMES.
**********************************************************************
      LOOP AT standard_tab REFERENCE INTO DATA(dref) WHERE comp1 < 800.
        dref->comp6 += 1.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_09_block_insert_a.
    "Block-wise insertion of table content

    DO ops TIMES.
**********************************************************************
      APPEND LINES OF str_tab_a TO str_tab_b.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_09_block_insert_z.
    "Line by line insertion of table content

    DO ops TIMES.
**********************************************************************
      LOOP AT str_tab_a INTO DATA(wa).
        APPEND wa TO str_tab_b.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_10_restrict_copy_a.
    "Restricting the copying of table line content using the TRANSPORTING addition

    DO ops TIMES.
**********************************************************************
      LOOP AT deep_standard_tab INTO DATA(wa) WHERE comp1 < 400.
        wa-comp6 += 1.
        MODIFY deep_standard_tab FROM wa TRANSPORTING comp6.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_10_restrict_copy_z.
    "Not using the TRANSPORTING addition
    "Instead, all fields except one are overwritten with the same content
    "available in the work area.

    DO ops TIMES.
**********************************************************************
      LOOP AT deep_standard_tab INTO DATA(wa) WHERE comp1 < 400.
        wa-comp6 += 1.
        MODIFY deep_standard_tab FROM wa.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_11_sec_key_a.
    "Reading single lines from a standard table using a secondary table key

    DO ops TIMES.
**********************************************************************
      READ TABLE standard_tab_w_sec_key TRANSPORTING NO FIELDS WITH TABLE KEY sec_key COMPONENTS comp1 = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_11_sec_key_z.
    "Reading single lines from a standard table using the primary table key
    "The primary table key of the demo internal table here corresponds to the secondary table
    "key in the itab_10_sec_key_a method.

    DO ops TIMES.
**********************************************************************
      READ TABLE standard_tab TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_12_sec_key_freq_mod_a.
    "A standard table is looped across. The table specifies a secondary table key.
    "Reads are performed in the loop.

    DO ops TIMES.
**********************************************************************
      LOOP AT standard_tab_w_sec_key ASSIGNING FIELD-SYMBOL(<fs>) TO 200.
        DATA(comp) = <fs>-comp6.
        READ TABLE standard_tab_w_sec_key TRANSPORTING NO FIELDS WITH TABLE KEY sec_key COMPONENTS comp1 = sy-tabix.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_12_sec_key_freq_mod_z.
    "A standard table is looped across. The table specifies a secondary table key.
    "The loop contains table modifications.

    DO ops TIMES.
**********************************************************************
      LOOP AT standard_tab_w_sec_key ASSIGNING FIELD-SYMBOL(<fs>) TO 200.
        <fs>-comp1 = sy-tabix.
        READ TABLE standard_tab_w_sec_key TRANSPORTING NO FIELDS WITH TABLE KEY sec_key COMPONENTS comp1 = sy-tabix.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_13_sort_itab_a.
    "A standard table declared with the standard key is sorted.
    "The sort key is explicitly specified in the SORT statement.

    DO ops TIMES.
**********************************************************************
      SORT standard_tab_w_default_key BY comp1 ASCENDING.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_13_sort_itab_z.
    "A standard table declared with the standard key is sorted.
    "The sort key is not explicitly specified in the SORT statement.
    "Instead, the standard key is used by default.

    DO ops TIMES.
**********************************************************************
      SORT standard_tab_w_default_key.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_14_dyn_itab_spec_a.
    "READ TABLE statement using static key specifications

    DO ops TIMES.
**********************************************************************
      READ TABLE standard_tab_w_sec_key TRANSPORTING NO FIELDS WITH TABLE KEY sec_key COMPONENTS comp1 = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_14_dyn_itab_spec_z.
    "READ TABLE statement using dynamic key specifications

    DO ops TIMES.
**********************************************************************
      READ TABLE standard_tab_w_sec_key TRANSPORTING NO FIELDS WITH TABLE KEY ('SEC_KEY') COMPONENTS ('COMP1') = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_08_loop_a.
    "Iterating an internal table with LOOP AT (sequential cursor access)

    DATA total TYPE i.
    DO ops TIMES.
**********************************************************************
      LOOP AT base_tab INTO DATA(wa).
        total = wa-comp1.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_08_do_read_z.
    "Iterating an internal table with DO and READ TABLE ... INDEX.
    "Manual index-based access instead of a single LOOP.

    DATA total TYPE i.
    DO ops TIMES.
**********************************************************************
      DO lines( base_tab ) TIMES.
        READ TABLE base_tab INTO DATA(wa) INDEX sy-index.
        total = wa-comp1.
      ENDDO.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD misc_01_strings_a.
    "Concatenating a variable-length string of type string

    DO ops TIMES.
**********************************************************************
      str_a &&= str_b.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD misc_01_strings_z.
    "Concatenating a fixed-length string of type c

    DO ops TIMES.
**********************************************************************
      char_a &&= char_b.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD misc_02_string_templates_a.
    "String specified as string literal using backquotes

    DO ops TIMES.
**********************************************************************
      str = `ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP`.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD misc_02_string_templates_z.
    "String template only using characters, without embedded expression

    DO ops TIMES.
**********************************************************************
      str = |ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP|.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD misc_04_type_conversion_a.
    "Calculation of numbers that share the same type

    DO ops TIMES.
**********************************************************************
      res = num_1 + num_2 + num_3 + num_4 + num_5 + num_6.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD misc_04_type_conversion_z.
    "Calculation of numbers that have different types triggering implicit
    "type conversions

    DO ops TIMES.
**********************************************************************
      res_b = num_1b + num_2b + num_3b + num_4b + num_5b + num_6b.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD meth_pass_by_ref.
    "Example method demonstrating actual parameters passed by reference

    e = a.
    f = b.
    g = c.
    h = d.
  ENDMETHOD.

  METHOD meth_pass_by_value.
    "Example method demonstrating actual parameters passed by value

    m = i.
    n = j.
    o = k.
    p = l.
  ENDMETHOD.

  METHOD misc_05_formal_parameters_a.
    "Actual parameters passed by reference

    DO ops TIMES.
**********************************************************************
      meth_pass_by_ref( a = sy-index
                        b = sy-index
                        c = deep_standard_tab
                        d = deep_standard_tab ).
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD misc_05_formal_parameters_z.
    "Actual parameters passed by value

    DO ops TIMES.
**********************************************************************
      meth_pass_by_value( i = sy-index
                          j = sy-index
                          k = deep_standard_tab
                          l = deep_standard_tab ).
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD misc_06_parallel_processing_a.
    "Demonstrating parallelization using the cl_abap_parallel class
    "The implementation includes inserting many entries to a database
    "table, executed in parallel. Then, the added entries are retrieved.
    "The inserted data contains a time stamp of the insertion.
    "The runtime delta of the first and last inserted entry is calculated
    "using cl_abap_tstmp=>subtract.

    DATA ref_tab TYPE cl_abap_parallel=>t_in_inst_tab.

    DO loop_count_5 TIMES.
      DATA(inst) = NEW lcl_parallel( ).
      APPEND inst TO ref_tab.
    ENDDO.

    DATA(parallel) = NEW cl_abap_parallel( ).

    parallel->run_inst( EXPORTING p_in_tab  = ref_tab
                        IMPORTING p_out_tab = DATA(result_info) ).

    SELECT id, num1, crea_date_time
      FROM zdemo_abap_tabca
      WHERE num1 = 1
      ORDER BY crea_date_time ASCENDING
      INTO TABLE @DATA(it).

    out->write( `MISC_06_PARALLEL_PROCESSING_A` ).
    IF it IS INITIAL.
      			out->write( `No database entries were written.` ).
      RETURN.
    ELSEIF lines( it ) < 500. "500 -> loop_count_5 * loop_count_100
      out->write( `Not all database entries were written.` ).
    ENDIF.

    DATA(timestamp1) = it[ 1 ]-crea_date_time.
    DATA(timestamp2) = it[ lines( it ) ]-crea_date_time.

    diff = cl_abap_tstmp=>subtract(
          tstmp1 = timestamp2
          tstmp2 = timestamp1 ).

    out->write( |Time difference of first and last inserted timestamp: { diff STYLE = SIMPLE }| ).
    out->write( |Table lines with value num1 = 1: { lines( it ) }| ).
  ENDMETHOD.

  METHOD misc_06_parallel_processing_z.
    "Sequential processing in contrast to the parallel processing
    "The implementation includes inserting many entries to a database
    "table, executed in parallel. Then, the added entries are retrieved.
    "The inserted data contains a time stamp of the insertion.
    "The runtime delta of the first and last inserted entry is calculated
    "using cl_abap_tstmp=>subtract.

    DATA(inst) = NEW lcl_parallel( ).
    DO loop_count_5 TIMES.
      inst->meth( ).
    ENDDO.

    SELECT id, num1, crea_date_time
      FROM zdemo_abap_tabca
      WHERE num1 = 2
      ORDER BY crea_date_time ASCENDING
      INTO TABLE @DATA(it).

    out->write( `MISC_06_PARALLEL_PROCESSING_Z` ).
    IF it IS INITIAL.
      out->write( `No database entries were written.` ).
      RETURN.
    ELSEIF lines( it ) < 500. "500 -> loop_count_5 * loop_count_100
      out->write( `Not all database entries were written.` ).
    ENDIF.

    DATA(timestamp1) = it[ 1 ]-crea_date_time.
    DATA(timestamp2) = it[ lines( it ) ]-crea_date_time.

    diff = cl_abap_tstmp=>subtract(
          tstmp1 = timestamp2
          tstmp2 = timestamp1 ).

    out->write( |Time difference of first and last inserted timestamp: { diff STYLE = SIMPLE }| ).
    out->write( |Table lines with value num1 = 1: { lines( it ) }| ).
  ENDMETHOD.

  METHOD itab_15_read_where_a.
    "READ TABLE ... WITH KEY as an alternative to a READ TABLE ... WHERE statement

    DO ops TIMES.
**********************************************************************
      READ TABLE sorted_tab TRANSPORTING NO FIELDS WITH KEY comp5 = sy-index.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_15_read_where_z.
    "READ TABLE ... WHERE statement, which can also be expressed by specifying a key
    "using a READ TABLE ... WITH KEY statement.
    "Without the pragma, a syntax warning would be displayed informing about performance
    "issues with the statement.

    DO ops TIMES.
**********************************************************************
      READ TABLE sorted_tab TRANSPORTING NO FIELDS WHERE comp5 = sy-index ##read_where_ok.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_16_find_block_a.
    "FIND ... IN TABLE performs a single block-wise search across the whole table

    DO ops TIMES.
**********************************************************************
      FIND ALL OCCURRENCES OF `ABAP`
        IN TABLE str_tab_a
        RESULTS DATA(res_tab)
        RESPECTING CASE.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_16_find_loop_z.
    "Searching row by row in a loop instead of a single block-wise FIND IN TABLE

    DO ops TIMES.
**********************************************************************
      LOOP AT str_tab_a ASSIGNING FIELD-SYMBOL(<fs>).
        FIND ALL OCCURRENCES OF `ABAP`
          IN <fs>
          RESULTS DATA(res_tab)
          RESPECTING CASE.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_18_replace_block_a.
    "REPLACE ... IN TABLE performs a single block-wise replacement across the whole table

    DO ops TIMES.
**********************************************************************
      REPLACE ALL OCCURRENCES OF `ABAP`
        IN TABLE str_tab_a
        WITH `ABAP`
        RESPECTING CASE.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_18_replace_loop_z.
    "Replacing row by row in a loop instead of a single block-wise REPLACE IN TABLE

    DO ops TIMES.
**********************************************************************
      LOOP AT str_tab_a ASSIGNING FIELD-SYMBOL(<fs>).
        REPLACE ALL OCCURRENCES OF `ABAP`
          IN <fs>
          WITH `ABAP`
          RESPECTING CASE.
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD eml_01_eml_in_loop_a.
    "EML update request using a FOR loop for a block-wise operation

    DO ops TIMES.
**********************************************************************
      MODIFY ENTITIES OF zdemo_abap_rap_ro_m
        ENTITY root
        UPDATE FIELDS ( field3 field4 ) WITH VALUE #( FOR key IN read_result ( %tky   = key-%tky
                                                                               field3 = key-field3 + 1
                                                                               field4 = key-field4 + 1 ) ).
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD eml_01_eml_in_loop_z.
    "Multiple individual EML update requests in a loop

    DO ops TIMES.
**********************************************************************
      LOOP AT upd ASSIGNING FIELD-SYMBOL(<fs>).
        MODIFY ENTITIES OF zdemo_abap_rap_ro_m
         ENTITY root
         UPDATE FIELDS ( field3 field4 ) WITH VALUE #( ( <fs> ) ).
      ENDLOOP.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD misc_03_search_pattern_a.
    "Simple pattern-based search using IF and CP

    DO ops TIMES.
**********************************************************************
      IF str CP `*f#_*`.
        ...
      ENDIF.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD misc_03_search_pattern_z.
    "Using FIND and a regular expression

    DO ops TIMES.
**********************************************************************
      FIND PCRE `.*f_.*` IN str.

      IF sy-subrc = 0.
        ...
      ENDIF.
**********************************************************************
    ENDDO.


  ENDMETHOD.

  METHOD itab_17_table_expr_mult_acc_a.
    DO ops TIMES.
**********************************************************************
      ASSIGN standard_tab[ 1 ] TO FIELD-SYMBOL(<fs1>).
      ASSIGN standard_tab[ 2 ] TO FIELD-SYMBOL(<fs2>).
      <fs1>-comp1 = sy-index.
      <fs1>-comp2 = sy-index.
      <fs1>-comp3 = sy-index.
      <fs2>-comp4 = sy-index.
      <fs2>-comp5 = sy-index.
      <fs2>-comp6 = sy-index.
      <fs1>-comp1 = <fs2>-comp1.
      <fs1>-comp2 = <fs2>-comp2.
      <fs1>-comp3 = <fs2>-comp3.
      INSERT <fs1> INTO TABLE standard_tab.
      INSERT <fs2> INTO TABLE standard_tab.
**********************************************************************
    ENDDO.
  ENDMETHOD.

  METHOD itab_17_table_expr_mult_acc_z.
    DO ops TIMES.
**********************************************************************
      standard_tab[ 1 ]-comp1 = sy-index.
      standard_tab[ 1 ]-comp2 = sy-index.
      standard_tab[ 1 ]-comp3 = sy-index.
      standard_tab[ 2 ]-comp4 = sy-index.
      standard_tab[ 2 ]-comp5 = sy-index.
      standard_tab[ 2 ]-comp6 = sy-index.
      standard_tab[ 1 ]-comp1 = standard_tab[ 2 ]-comp1.
      standard_tab[ 1 ]-comp2 = standard_tab[ 2 ]-comp2.
      standard_tab[ 1 ]-comp3 = standard_tab[ 2 ]-comp3.
      INSERT standard_tab[ 1 ] INTO TABLE standard_tab.
      INSERT standard_tab[ 2 ] INTO TABLE standard_tab.
**********************************************************************
    ENDDO.
  ENDMETHOD.

ENDCLASS.
```

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 
 
```abap
CLASS lcl_parallel DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_abap_parallel.
    METHODS meth.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_parallel IMPLEMENTATION.
  METHOD if_abap_parallel~do.
    DATA ts_long TYPE timestampl.
    DO zcl_demo_abap=>loop_count_100 TIMES.
      GET TIME STAMP FIELD ts_long.
      "Suboptimal database table insertion in a loop serving the example demonstration purpose
      INSERT zdemo_abap_tabca FROM @( VALUE #( id = xco_cp=>uuid( )->value num1 = 1 crea_date_time = ts_long ) ).
    ENDDO.
  ENDMETHOD.

  METHOD meth.
    DATA ts_long TYPE timestampl.
    DO zcl_demo_abap=>loop_count_100 TIMES.
      GET TIME STAMP FIELD ts_long.
      "Suboptimal database table insertion in a loop serving the example demonstration purpose
      INSERT zdemo_abap_tabca FROM @( VALUE #( id = xco_cp=>uuid( )->value num1 = 2 crea_date_time = ts_long ) ).
    ENDDO.
  ENDMETHOD.
ENDCLASS.
```

 </td>
</tr>

</table>

</details> 

<p align="right"><a href="#top">⬆️ back to top</a></p>