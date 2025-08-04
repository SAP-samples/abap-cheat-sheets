<a name="top"></a>

# ABAP Performance Notes

- [ABAP Performance Notes](#abap-performance-notes)
  - [Database Access](#database-access)
    - [Reducing the Result Set](#reducing-the-result-set)
    - [Reducing the Data Volume](#reducing-the-data-volume)
    - [Reducing Database Accesses](#reducing-database-accesses)
    - [Using CDS views](#using-cds-views)
    - [Dynamic and Statics ABAP SQL Statements](#dynamic-and-statics-abap-sql-statements)
    - [Using Buffering](#using-buffering)
    - [Using Indexes](#using-indexes)
  - [Processing Internal Tables](#processing-internal-tables)
    - [Single Line Access Using the Primary Table Key and Free Keys](#single-line-access-using-the-primary-table-key-and-free-keys)
    - [Multiline Access and Applying a WHERE Condition](#multiline-access-and-applying-a-where-condition)
    - [Reducing Content Transfer](#reducing-content-transfer)
    - [Block-wise Processing of Table Lines](#block-wise-processing-of-table-lines)
    - [Using Secondary Table Keys to Improve Read Performance](#using-secondary-table-keys-to-improve-read-performance)
    - [Applying a Sort Key when Sorting Internal Tables](#applying-a-sort-key-when-sorting-internal-tables)
    - [Clearing with CLEAR and FREE](#clearing-with-clear-and-free)
    - [Dynamic and Static Specifications when Processing Internal Tables](#dynamic-and-static-specifications-when-processing-internal-tables)
  - [Miscellaneous Contexts](#miscellaneous-contexts)
    - [Using DO and WHILE](#using-do-and-while)
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
  - If your use case is to match entries of an internal table when selecting data from a data source, you can also use joins and subqueries with internal tables in `SELECT` statements in modern ABAP to avoid unnecessary data transfers from the database. You may also stumble on the (older) `FOR ALL ENTRIES` addition in `SELECT` statements. Here, internal table content is used to match entries. However, note that if the internal table is empty, all data will be read.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using CDS views	

- ABAP CDS (Core Data Services) represent a framework for defining and consuming semantically rich data models on AS ABAP.  
- ABAP CDS can enhance database access performance by leveraging the capabilities of the [SAP HANA database](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhana_database_glosry.htm).
- Data-intensive operations, such as complex calculations or joins, can be pushed down to the database layer and executed in the SAP HANA database instead of performing them in ABAP.
- Find more information in the [ABAP Data Models guide](https://help.sap.com/docs/abap-cloud/abap-data-models/abap-data-models?version=sap_btp).

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Dynamic and Statics ABAP SQL Statements

- ABAP SQL statements provide syntax options for dynamic programming, such as specifying the data source to read from dynamically.  
- Dynamically specifying tokens requires runtime evaluation. Static token specification offers better performance.  
- For more information, refer to the Dynamic Programming cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using Buffering

- Data from DDIC database tables or CDS entities can be buffered in a table buffer. 
- Buffering capabilities are determined by the technical settings of the artifacts. For example, for a CDS entity, buffering is determined by annotations and a CDS entity buffer.  
- The table buffering is applied implicitly when the database is accessed using ABAP SQL. 
- Find more information in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENSAP_PUFFERING.html).

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using Indexes

- Indexes are special structures on the database that enhance query processing times and offer additional capabilities.
- Find more information in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENDDIC_DATABASE_TABLES_INDEX.html).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Processing Internal Tables

> [!NOTE]
> - Generally, a suitable table type should be chosen based on aspects such as the frequency of table access and modifications.
> - On a high level, standard tables should be used for smaller data sets and sequential processing. Sorted tables may also be used for sequential processing and when you always need to keep content in a sorted order. They offer optimized access through the table key. For very large data sets that require minimal modifications, consider using hashed tables. The primary access method for hashed tables is key access, supported by a hash algorithm for consistent and optimized retrieval.
> - Standard tables are searched linearly by keys. They do not offer an optimized key access. However, standard tables (and the other table categories, too) can be enriched by secondary table keys, providing optimized access.
> - Find more information in the Internal Tables cheat sheet.

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
- For more information about optimizing the `WHERE` condition, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenitab_where_optimization.html).
- `READ TABLE` also supports the specification of a `WHERE` condition. If a `READ TABLE` statement can be expressed using `WITH KEY`, the use of `WITH KEY` is recommended as it is more performant. A syntax warning occurs but can be suppressed with the pragma `##read_where_ok`.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Reducing Content Transfer

Contexts of reducing content transfer are, for example, the following:

- Using the `TRANSPORTING` addition
	- Statements like `MODIFY`, `READ`, or `LOOP` support the `TRANSPORTING` addition, which lets you specify the exact fields to process, rather than processing all fields.
	- In some cases, `TRANSPORTING` can specify `NO FIELDS`, such as in a read operation when only system fields are needed.
  
- Using field symbols and data reference variables instead of work areas
	- Statements like `LOOP` and `READ` provide three options to specify the target area for the read table line.
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

### Clearing with CLEAR and FREE

- The `CLEAR` and `FREE` statements allow you to delete the entire table content.
- The difference between the two is in the handling of the memory space originally allocated to the table. When a table is cleared with `CLEAR`, the content is removed, but the memory space initially requested remains allocated. 
- If the table is populated again later, the memory space is still available, which is a performance advantage over clearing an internal table with `FREE`. Such a statement also deletes the table content, but it also releases the memory space. 

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Dynamic and Static Specifications when Processing Internal Tables

- Statements that process internal table provide syntax options for dynamic programming, such as specifying the table key dynamically.  
- Dynamic specifications require runtime evaluation. Static specifications offer better performance.  
- For more information, refer to the Dynamic Programming cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Miscellaneous Contexts

### Using DO and WHILE

A `WHILE` loop may be faster than a `DO` loop (including an explicit `EXIT`).

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Using Strings

Examples of string-related contexts:

- Fixed-length and variable-length strings
  - Strings (and internal tables) are addressed internally through references.
  - They support the concept of [sharing](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensharing_glosry.htm), which offers performance advantages.
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

To try the example out, create a demo class named `zcl_demo_abap` in the SAP BTP ABAP Environment and paste the code into it. The example also includes a local class in the CCIMP include (Local Types tab in ADT). 
After activation, choose F9 in ADT to execute the class. The example is set up to display output in the console. The class execution may take a while to complete.

The simplified example is set up as follows: 
- The global class declares multiple components that are used in the demo, including various internal table declarations to illustrate specific examples, such as demonstrating secondary table keys.
- Several methods are implemented, containing code snippets that show both inefficient and rather efficient statements. For example, the method `sql_01_reduce_t_rows_a` shows an efficient example for minimizing the result set of `SELECT` statements. The method `sql_01_reduce_t_rows_z` shows an inefficient example.
- The runtime analysis in this self-contained example is as follows: The code snippet for which the runtime should be measured is enclosed by two timestamp retrievals using `utclong_current( )`. The runtime is analysed by calculating the delta of the two timestamp values, indicating the used runtime for the code snippet. To have a more meaningful example regarding the runtime analysis, the snippets are executed multiple times, indicated by enclosing them in a `DO` loop. The runtime delta value is added to an internal table collecting all runtime delta values for the specific code snippet for all loop iterations. Finally, the average runtime is calculated based on the collected runtime delta values. This average runtime value is output to the console for each code snippet, evaluated in each method. This approach aims to provide a balanced view of runtime. Note that this implementation is for exploration and demonstration purposes only. For proper analyses, the mentioned tools should be used.
- The parallelization example uses a local class implemented in the CCIMP include.

Overview of covered examples (see also the inline comments in the method implementations): 
- <strong>ABAP SQL statements</strong>	
	- Static vs. dynamic token specification
	- Using a `WHERE` clause vs. reading all table rows, looping across the result set and filtering out data
	- Using `SELECT SINGLE` to read a single data set vs. reading all table rows and filter out single data set vs. `SELECT ... UP TO 1 ROWS`
	- Reading a specific number of datase using `UP TO n ROWS` vs. reading all table rows and discarding unnecessary data
	- Specifying columns in the `SELECT` list vs. specifying `SELECT *`
	- Single database access vs. multiple accesses in a loop
	- Using joins, `WHERE EXISTS` and `FOR ALL ENTRIES` vs. repeated database access in a loop
	- Block-wise modify operation vs. line by line modify operation in a loop
	- Using a join vs. using a nested `SELECT` loop
	- Using an `UPDATE ... SET` statement vs. `UPDATE` using a work area
	- Using an aggregate function vs. reading all table rows and calculation of average value
- <strong>Internal table processing</strong>
	- Reading single lines using the primary table key: standard table vs. sorted table vs. hashed table	
	- Reading single lines using the incomplete primary table key: hashed table vs. sorted table (both partial, left-aligned key and partial, not left-aligned key)	
	- Reading single lines in standard table using `BINARY SEARCH` and without the addition (both examples use a preceding `SORT` statement)
	- Loop across internal table by specifying a `WHERE` condition: standard table vs. sorted (full key + partial,  left-aligned key + partial, not left-aligned key) vs. hashed (full key + partially specified key)
	- Loop across standard table:  Specifying a `WHERE` condition vs. no `WHERE` condition and filtering out data in the loop
	- Loop across deep standard table (includes a nested structure and internal table) specifying target areas: Work area vs. field symbol vs. data reference variable
	- `READ TABLE ... WITH` statement vs. `READ TABLE ... WHERE` statement 
	- Modifying internal table content in a loop: Work area as target area, modifying the target area, applying a `MODIFY` statement vs. direct modification using a field symbol vs. direct modification using a data reference variable
	- Block-wise insertion into an internal table vs. line by line insertion in a loop
	- Find and replace operations on an internal table with character-like data type vs. line by line search and replacement
	- Restricting the transfer of table line content using the `TRANSPORTING` addition vs. not using the addition and overwriting all table line content based on a work area
	- Reading single lines from a standard table: Using a secondary table key vs. using the primary table key (which corresponds to the secondary table key in the other table)
	- Loop across a standard table that specifies a secondary table key: Performing only reads in the loop vs. performing reads and modifications
	- Sorting an internal table that specifies the standard key: Explicitly specifying the sort key vs. implicit sorting by the standard key
	- Clearing an internal table by applying `CLEAR` vs `FREE` (the table is populated again after the statements)
	- Reading from an internal table by specifying the keys dynamically vs. static key specification
- <strong>Miscellaneous contexts</strong>
    - `WHILE` vs `DO`
    - Concatenating a variable-length string of type `string` vs. concatenating a fixed-length string of type `c`
	- String specified as string literal using backquotes vs. string template only using characters, without embedded expression
	- Pattern-based search using a comparison operator vs. a `FIND PCRE` statement and a regular expression
	- Calculation of numbers that share the same type vs. calculation of numbers that have different types
	- Actual parameters passed by reference vs. actual parameters passed by value
	- Parallel processing vs. sequential processing
	- ABAP EML update request using a `FOR` loop for a block-wise operation vs. EML statement in a loop

> [!NOTE]
> - As a prerequisite, you have imported the ABAP cheat sheet repository, as this example uses some of its artifacts.
> - Since the example contains multiple loops, the execution may take some time to complete.
> - This example is for exploration, experimentation, and demonstration purposes only. It is <strong>not</strong> suitable for accurate runtime or performance testing (refer to the note in the cheat sheet introduction). Due to its simplified nature and the various factors that can influence the runtime of an ABAP program, results may vary and may not be entirely accurate, even across multiple runs. The displayed results may not always accurately reflect the performance notes from the cheat sheets, and some may be clearer than others. You might run the example again.
> - The example code snippets do not claim to illustrate best practices. Instead, they aim to highlight potential pitfalls when handling data-intensive tasks in ABAP programs. Despite manually calculating runtime, without tool support, the results should indicate trends and reveal inefficient coding styles. 
> - Note the disclaimer in the ABAP cheat sheet repository's README.


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
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    "Constants used for DO n TIMES statements
    CONSTANTS loop_count_5 TYPE i VALUE 5.
    CONSTANTS loop_count_10 TYPE i VALUE 10.
    CONSTANTS loop_count_50 TYPE i VALUE 50.
    CONSTANTS loop_count_250 TYPE i VALUE 250.
    CONSTANTS loop_count_500 TYPE i VALUE 500.
    CONSTANTS loop_count_1000 TYPE i VALUE 1000.

    "Components used for calculating the average runtime
    DATA: ts1         TYPE utclong,
          runtime_tab TYPE TABLE OF decfloat34 WITH EMPTY KEY,
          runtime     TYPE decfloat34.

    "Collects timestamps to calculate the average runtime
    METHODS store_runtime IMPORTING high TYPE utclong
                                    low  TYPE utclong.

    "Calculates the average runtime
    METHODS get_avg_runtime RETURNING VALUE(avg_runtime) TYPE decfloat34.

    "Exemplary methods containing (in)efficient demo implementations
    METHODS:
      sql_01_reduce_t_rows_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_01_reduce_t_rows_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_02_reduce_t_rows_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_02_reduce_t_rows_b IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_02_reduce_t_rows_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_03_reduce_t_rows_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_03_reduce_t_rows_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_04_reduce_data_volume_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_04_reduce_data_volume_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_05_reduce_db_access_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_05_reduce_db_access_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_06_reduce_db_access_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_06_reduce_db_access_b IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_06_reduce_db_access_c IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_06_reduce_db_access_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_07_reduce_db_access_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_07_reduce_db_access_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_08_reduce_db_access_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_08_reduce_db_access_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_09_update_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_09_update_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_10_db_logic_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_10_db_logic_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_11_dynamic_sql_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      sql_11_dynamic_sql_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_01_table_key_read_a_std IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_01_table_key_read_b_so IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_01_table_key_read_z_ha IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_02_incomplete_t_key_a_so IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_02_incomplete_t_key_b_ha IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_02_incomplete_t_key_z_so IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_03_std_binary_search_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_03_std_binary_search_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_04_loop_a_std IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_04_loop_b_so_compl_key IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_04_loop_c_so_incompl_key IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_04_loop_d_so_incompl_k_la IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_04_loop_e_ha_compl_key IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_04_loop_z_ha_incompl_key IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_05_loop_no_where_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_05_loop_no_where_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_06_loop_target_a_wa IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_06_loop_target_b_fs IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_06_loop_target_z_dref IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_07_loop_modify_a_wa IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_07_loop_modify_b_fs IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_07_loop_modify_z_dref IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_08_block_insert_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_08_block_insert_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_09_restrict_copy_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_09_restrict_copy_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_10_sec_key_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_10_sec_key_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_11_sec_key_freq_mod_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_11_sec_key_freq_mod_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_12_sort_itab_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_12_sort_itab_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_13_clear_free_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_13_clear_free_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_14_dyn_itab_spec_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_14_dyn_itab_spec_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_15_read_where_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_15_read_where_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_16_find_replace_table_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      itab_16_find_replace_table_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_01_while_do_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_01_while_do_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_02_strings_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_02_strings_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_03_string_templates_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_03_string_templates_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_04_search_pattern_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_04_search_pattern_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_05_type_conversion_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_05_type_conversion_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_06_formal_parameters_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_06_formal_parameters_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_07_parallel_processing_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      misc_07_parallel_processing_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      eml_01_eml_in_loop_a IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname,
      eml_01_eml_in_loop_z IMPORTING out TYPE REF TO if_oo_adt_classrun_out name TYPE abap_methname.

    "Methods demonstrating parameter passing by value and reference
    METHODS: meth_pass_by_ref IMPORTING a TYPE i
                                        b TYPE i
                                        c TYPE string_table
                                        d TYPE string_table
                              EXPORTING e TYPE i
                                        f TYPE i
                                        g TYPE string_table
                                        h TYPE string_table,
      meth_pass_by_value IMPORTING VALUE(i) TYPE i
                                   VALUE(j) TYPE i
                                   VALUE(k) TYPE string_table
                                   VALUE(l) TYPE string_table
                         EXPORTING VALUE(m) TYPE i
                                   VALUE(n) TYPE i
                                   VALUE(o) TYPE string_table
                                   VALUE(p) TYPE string_table.

    "Demo internal tables used by the examples
    TYPES: BEGIN OF ty_tab,
             comp1 TYPE i,
             comp2 TYPE i,
             comp3 TYPE i,
             comp4 TYPE c LENGTH 50,
             comp5 TYPE c LENGTH 50,
             comp6 TYPE i,
           END OF ty_tab.

    CLASS-DATA: standard_tab               TYPE TABLE OF ty_tab WITH NON-UNIQUE KEY comp1,
                sorted_tab                 TYPE SORTED TABLE OF ty_tab WITH UNIQUE KEY comp1,
                hashed_tab                 TYPE HASHED TABLE OF ty_tab WITH UNIQUE KEY comp1,
                standard_tab_w_sec_key     TYPE TABLE OF ty_tab WITH EMPTY KEY WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS comp1,
                sorted_tab_mult_key_comp   TYPE SORTED TABLE OF ty_tab WITH UNIQUE KEY comp1 comp2 comp3,
                hashed_tab_mult_key_comp   TYPE HASHED TABLE OF ty_tab WITH UNIQUE KEY comp1 comp2 comp3,
                standard_tab_w_default_key TYPE TABLE OF ty_tab,
                key_tab                    TYPE TABLE OF zdemo_abap_tab1-key_field WITH EMPTY KEY,
                itab1                      TYPE TABLE OF zdemo_abap_tab1 WITH EMPTY KEY,
                flsch_tab                  TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY,
                carr_tab                   TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.

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
          CALL METHOD oref->(meth_wa-name) EXPORTING out = out name = meth_wa-name.
        CATCH cx_root.
      ENDTRY.

      "For better readability of the output and reference in the console, inserting a divider
      "after the final method of an example context. These methods include _Z in the name.
      FIND `_Z` IN meth_wa-name IGNORING CASE.
      IF sy-subrc = 0.
        out->write( repeat( val = `*` occ = 70 ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD store_runtime.
    cl_abap_utclong=>diff( EXPORTING high    = high
                                     low     = low
                           IMPORTING seconds = DATA(seconds) ).

    APPEND seconds TO runtime_tab.
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

    DO 1000 TIMES.
      APPEND VALUE #( key_field = sy-index char1 = sy-index char2 = sy-index num1 = sy-index num2 = sy-index ) TO demo_tab.
    ENDDO.

    DELETE FROM zdemo_abap_tab1.
    INSERT zdemo_abap_tab1 FROM TABLE @demo_tab.

    "Populating demo internal tables
    DO loop_count_250 TIMES.
      APPEND sy-index TO key_tab.
    ENDDO.

    carr_tab = VALUE #( ( carrid = 'AB' carrname = 'AB Airways' )
          ( carrid = 'CD' carrname = 'CD Airways' )
          ( carrid = 'EF' carrname = 'EF Airways' )
          ( carrid = 'GH' carrname = 'GH Airways' )
          ( carrid = 'IJ' carrname = 'IJ Airways' ) ).

    DO loop_count_1000 TIMES.
      INSERT VALUE #( comp1 = sy-index comp2 = sy-index comp3 = sy-index comp4 = sy-index comp5 = sy-index comp6 = sy-index ) INTO TABLE sorted_tab ASSIGNING FIELD-SYMBOL(<fs>).

      INSERT CORRESPONDING #( <fs> ) INTO TABLE deep_standard_tab ASSIGNING FIELD-SYMBOL(<deep>).
      <deep>-comp7 = VALUE #( ( `A` ) ( `B` ) ( `A` ) ( `P` ) ).
      <deep>-comp8 = <fs>.
    ENDDO.

    INSERT LINES OF sorted_tab INTO TABLE standard_tab.
    INSERT LINES OF sorted_tab INTO TABLE hashed_tab.
    INSERT LINES OF sorted_tab INTO TABLE standard_tab_w_sec_key.
    INSERT LINES OF sorted_tab INTO TABLE sorted_tab_mult_key_comp.
    INSERT LINES OF sorted_tab INTO TABLE hashed_tab_mult_key_comp.
    INSERT LINES OF sorted_tab INTO TABLE standard_tab_w_default_key.
  ENDMETHOD.

  METHOD sql_10_db_logic_a.
    "Using aggregate functions
    "This simplified example does not necessarily show better performance than the
    "other example. The purpose here is to leave calculation tasks to the database,
    "in contrast to the manual calculation.

    DO loop_count_1000 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT AVG( key_field ) AS avg,
             SUM( key_field ) AS sum,
             MAX( key_field ) AS max,
             MIN( key_field ) AS min,
             COUNT( key_field ) AS count
         FROM zdemo_abap_tab1
         INTO @DATA(result).
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.
    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_10_db_logic_z.
    "Reading all table rows, then further processing the data sets
    "to calculate values.

    DATA: BEGIN OF result,
            avg   TYPE decfloat34,
            sum   TYPE i,
            max   TYPE i,
            min   TYPE i,
            count TYPE i,
          END OF result.

    DO loop_count_1000 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT key_field
       FROM zdemo_abap_tab1
       INTO TABLE @DATA(tab).

      IF tab IS NOT INITIAL.
        LOOP AT tab ASSIGNING FIELD-SYMBOL(<fs>).
          result-count += 1.
          result-sum += <fs>-key_field.
          result-max = COND #( WHEN <fs>-key_field > result-max THEN <fs>-key_field ELSE result-max ).
          result-min = COND #( WHEN sy-tabix = 1 THEN <fs>-key_field
                               WHEN <fs>-key_field < result-min THEN <fs>-key_field
                               ELSE result-min ).
        ENDLOOP.
        result-avg = result-sum / result-count.
      ENDIF.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_11_dynamic_sql_a.
    "Using static specification in ABAP SQL statement

    DO loop_count_1000 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT SINGLE carrid, connid, countryfr, cityfrom
         FROM zdemo_abap_flsch
         WHERE carrid <> 'LH'
         INTO NEW @DATA(dref_fl).
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_11_dynamic_sql_z.
    "Dynamic specification in ABAP SQL statement

    DO loop_count_1000 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT SINGLE ('CARRID, CONNID, COUNTRYFR, CITYFROM')
        FROM ('ZDEMO_ABAP_FLSCH')
        WHERE (`CARRID <> 'LH'`)
        INTO NEW @DATA(dref_fl2).
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_01_reduce_t_rows_a.
    "ABAP SQL statement specifying a WHERE clause

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
        FROM zdemo_abap_tab1
        WHERE key_field < 500
        INTO TABLE @DATA(it1).
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_01_reduce_t_rows_z.
    "ABAP SQL statement not specifying a WHERE clause
    "Instead, reading all data from the database table, processing
    "the content further in a loop, and filtering out data sets.

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
        FROM zdemo_abap_tab1
        INTO TABLE @DATA(it2).

      LOOP AT it2 REFERENCE INTO DATA(dref2).
        IF dref2->key_field < 500.
          DELETE it2 INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_02_reduce_t_rows_a.
    "Using SELECT SINGLE to read a single line

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT SINGLE carrid, connid, cityfrom, countryfr, cityto, countryto
       FROM zdemo_abap_flsch
       WHERE carrid = char`LH` AND connid = char`0400`
       INTO @DATA(line).
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_02_reduce_t_rows_b.
    "Reading all data sets from the database table, and reading
    "an entry using READ TABLE.

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT carrid, connid, cityfrom, countryfr, cityto, countryto
        FROM zdemo_abap_flsch
        INTO TABLE @DATA(it3).

      READ TABLE it3 INTO DATA(line4) WITH KEY carrid = 'LH' connid = '0400'.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_03_reduce_t_rows_a.
    "Reading a specific number of data sets from the database table using
    "the UP TO n ROWS addition

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
         FROM zdemo_abap_tab1
         INTO TABLE @DATA(it5)
         UP TO 100 ROWS.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_03_reduce_t_rows_z.
    "Reading all database table entries and further process the internal table
    "content

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
             FROM zdemo_abap_tab1
             INTO TABLE @DATA(it6).

      DELETE it6 FROM 101.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_04_reduce_data_volume_a.
    "Specifying columns so that not all columns are read

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT carrid, connid, cityfrom, countryfr, cityto, countryto
       FROM zdemo_abap_flsch
       INTO TABLE @DATA(it7).
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_04_reduce_data_volume_z.
    "Specifying SELECT * to read all columns

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT *
       FROM zdemo_abap_flsch
       INTO TABLE @DATA(it8).
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_05_reduce_db_access_a.
    "Single database access

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT carrid, connid, cityfrom, countryfr, cityto, countryto
       FROM zdemo_abap_flsch
       INTO TABLE @DATA(it9)
       UP TO 5 ROWS.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_05_reduce_db_access_z.
    "Repeated database access in a loop

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      DO 5 TIMES.
        SELECT SINGLE carrid, connid, cityfrom, countryfr, cityto, countryto
           FROM zdemo_abap_flsch
           INTO @DATA(fl_row).
      ENDDO.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_06_reduce_db_access_a.
    "Using an inner join

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
          FROM zdemo_abap_tab1
          INNER JOIN @key_tab AS tab
          ON zdemo_abap_tab1~key_field = tab~table_line
          INTO CORRESPONDING FIELDS OF TABLE @itab1.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_06_reduce_db_access_b.
    "Using WHERE EXISTS

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT key_field, char1, char2, num1, num2
        FROM zdemo_abap_tab1
        WHERE EXISTS ( SELECT 'X' FROM @key_tab AS tab WHERE zdemo_abap_tab1~key_field = tab~table_line )
        INTO CORRESPONDING FIELDS OF TABLE @itab1.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.


  METHOD sql_06_reduce_db_access_c.
    "Using the FOR ALL ENTRIES addition

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      IF key_tab IS NOT INITIAL.
        SELECT key_field, char1, char2, num1, num2
         FROM zdemo_abap_tab1
         FOR ALL ENTRIES IN @key_tab
         WHERE key_field = @key_tab-table_line
         INTO CORRESPONDING FIELDS OF TABLE @itab1.
      ENDIF.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_06_reduce_db_access_z.
    "Repeated database access in a loop

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT key_tab INTO DATA(wa).
        SELECT key_field, char1, char2, num1, num2
          FROM zdemo_abap_tab1
          WHERE key_field = @wa
          APPENDING CORRESPONDING FIELDS OF TABLE @itab1.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_07_reduce_db_access_a.
    "Block-wise modify operation

    DELETE FROM zdemo_abap_carr.

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      MODIFY zdemo_abap_carr FROM TABLE @carr_tab.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_07_reduce_db_access_z.
    "Line by line modify operations

    DELETE FROM zdemo_abap_carr.

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT carr_tab INTO DATA(wa_carr).
        MODIFY zdemo_abap_carr FROM @wa_carr.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_08_reduce_db_access_a.
    "Join

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT fl~carrid, carr~carrname, fl~connid, fl~cityfrom, fl~cityto
         FROM zdemo_abap_flsch AS fl
            LEFT OUTER JOIN zdemo_abap_carr AS carr
              ON fl~carrid = carr~carrid
         INTO TABLE @DATA(it10).
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).

  ENDMETHOD.

  METHOD sql_08_reduce_db_access_z.
    "Nested SELECT loop

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
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
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_09_update_a.
    "UPDATE ... SET statement

    DO loop_count_10 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      UPDATE zdemo_abap_tab1 SET char1 = 'X' WHERE key_field < 20.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_09_update_z.
    "UPDATE statement using a work area

    DO loop_count_10 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT *
        FROM zdemo_abap_tab1
        WHERE key_field < 20
        INTO @DATA(wa_tab).
        wa_tab-char1 = 'X'.
        UPDATE zdemo_abap_tab1 FROM @wa_tab.
      ENDSELECT.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD sql_02_reduce_t_rows_z.
    "Specifying UP TO 1 ROWS to retrieve a single row

    DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SELECT carrid, connid, cityfrom, countryfr, cityto, countryto
        FROM zdemo_abap_flsch
        WHERE carrid = char`LH` AND connid = char`0400`
        INTO TABLE @DATA(it4)
        UP TO 1 ROWS.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime(  ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_01_table_key_read_z_ha.
    "Reading from a hashed table using the primary table key

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE hashed_tab TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_01_table_key_read_b_so.
    "Reading from a sorted table using the primary table key

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_01_table_key_read_a_std.
    "Reading from a standard table using the primary table key

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_02_incomplete_t_key_b_ha.
    "Reading from a hashed table using a free key
    "The primary table key is only partially used.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE hashed_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp1 = sy-index comp2 = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_02_incomplete_t_key_a_so.
    "Reading from a sorted table using a free key
    "The primary table key is only partially used.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp1 = sy-index comp2 = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_02_incomplete_t_key_z_so.
    "Reading from a sorted table using a free key
    "The primary table key is only partially used. The key is not left-aligned.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab_mult_key_comp TRANSPORTING NO FIELDS WITH KEY comp2 = sy-index comp3 = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
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

  METHOD itab_03_std_binary_search_a.
    "Reading from a standard table using a free key and the BINARY SEARCH addition.
    "A SORT statement is executed beforehand.

    SORT standard_tab BY comp2 ASCENDING.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab TRANSPORTING NO FIELDS WITH KEY comp2 = sy-index BINARY SEARCH.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_03_std_binary_search_z.
    "Reading from a standard table using a free key. Here, the BINARY SEARCH addition
    "is not specified. A SORT statement is executed beforehand.

    SORT standard_tab BY comp2 ASCENDING.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab TRANSPORTING NO FIELDS WITH KEY comp2 = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_04_loop_a_std.
    "A standard table is looped across, specifying a WHERE condition.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT standard_tab REFERENCE INTO DATA(dref) WHERE comp1 = 800 AND comp2 = '800' AND comp3 = '800'.
        ...
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_04_loop_b_so_compl_key.
    "A sorted table is looped across, specifying a WHERE condition.
    "The full key is specified.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT sorted_tab_mult_key_comp REFERENCE INTO DATA(dref) WHERE comp1 = 800 AND comp2 = '800' AND comp3 = '800'.
        ...
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_04_loop_c_so_incompl_key.
    "A sorted table is looped across, specifying a WHERE condition.
    "The key is partially specified, but not left-aligned.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT sorted_tab_mult_key_comp REFERENCE INTO DATA(dref) WHERE comp2 = '800' AND comp3 = '800'.
        ...
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_04_loop_d_so_incompl_k_la.
    "A sorted table is looped across, specifying a WHERE condition.
    "The key is partially specified, left-aligned.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT sorted_tab_mult_key_comp REFERENCE INTO DATA(dref) WHERE comp1 = 800 AND comp2 = '800' .
        ...
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_04_loop_e_ha_compl_key.
    "A hashed table is looped across, specifying a WHERE condition.
    "The key is fully specified.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT hashed_tab_mult_key_comp REFERENCE INTO DATA(dref) WHERE comp1 = 800 AND comp2 = '800' AND comp3 = '800' .
        ...
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_04_loop_z_ha_incompl_key.
    "A hashed table is looped across, specifying a WHERE condition.
    "The key is partially specified.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT hashed_tab_mult_key_comp REFERENCE INTO DATA(dref) WHERE comp1 = 800 AND comp2 = '800'.
        ...
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_05_loop_no_where_a.
    "A standard table is looped across, specifying a WHERE condition.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT standard_tab REFERENCE INTO DATA(dref) WHERE comp1 > 800.
        ...
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_05_loop_no_where_z.
    "A standard table is looped across, not specifying a WHERE condition.
    "Instead, an IF constrol structure is implemented in the loop.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT standard_tab REFERENCE INTO DATA(dref).
        IF dref->comp1 > 800.
          CONTINUE.
        ENDIF.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_06_loop_target_a_wa.
    "A deep standard table is looped across. The target area is a work area.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT deep_standard_tab INTO DATA(wa) WHERE comp1 < 200.
        ...
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_06_loop_target_b_fs.
    "A deep standard table is looped across. The target area is a field symbol.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT deep_standard_tab ASSIGNING FIELD-SYMBOL(<fs>) WHERE comp1 < 200.
        ...
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_06_loop_target_z_dref.
    "A deep standard table is looped across. The target area is a data reference variable.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT deep_standard_tab REFERENCE INTO DATA(dref) WHERE comp1 < 200.
        ...
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_07_loop_modify_a_wa.
    "A standard table is looped across. The target area is a work area.
    "The read table line is modified using a MODIFY statement based
    "on the work area.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT standard_tab INTO DATA(wa) WHERE comp1 < 800.
        wa-comp6 += 1.
        MODIFY standard_tab FROM wa.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_07_loop_modify_b_fs.
    "A standard table is looped across. The target area is a field symbol.
    "The read table line is directly modified using the field symbol.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT standard_tab ASSIGNING FIELD-SYMBOL(<fs>) WHERE comp1 < 800.
        <fs>-comp6 += 1.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_07_loop_modify_z_dref.
    "A standard table is looped across. The target area is a data reference variable.
    "The read table line is directly modified using the data reference variable.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT standard_tab REFERENCE INTO DATA(dref) WHERE comp1 < 800.
        dref->comp6 += 1.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_08_block_insert_a.
    "Block-wise insertion of table content

    "Preparing demo data
    DATA str_tab_a TYPE string_table.
    DATA str_tab_b TYPE string_table.
    DO 1000 TIMES.
      APPEND sy-tabix TO str_tab_a.
    ENDDO.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      APPEND LINES OF str_tab_a TO str_tab_b.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_08_block_insert_z.
    "Line by line insertion of table content

    "Preparing demo data
    DATA str_tab_a TYPE string_table.
    DATA str_tab_b TYPE string_table.
    DO 1000 TIMES.
      APPEND sy-tabix TO str_tab_a.
    ENDDO.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT str_tab_a INTO DATA(wa).
        APPEND wa TO str_tab_b.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_09_restrict_copy_a.
    "Restricting the copying of table line content using the TRANSPORTING addition

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT deep_standard_tab INTO DATA(wa) WHERE comp1 < 400.
        wa-comp6 += 1.
        MODIFY deep_standard_tab FROM wa TRANSPORTING comp6.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_09_restrict_copy_z.
    "Not using the TRANSPORTING addition
    "Instead, all fields except one are overwritten with the same content
    "available in the work area.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT deep_standard_tab INTO DATA(wa) WHERE comp1 < 400.
        wa-comp6 += 1.
        MODIFY deep_standard_tab FROM wa.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_10_sec_key_a.
    "Reading single lines from a standard table using a secondary table key

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab_w_sec_key TRANSPORTING NO FIELDS WITH TABLE KEY sec_key COMPONENTS comp1 = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_10_sec_key_z.
    "Reading single lines from a standard table using the primary table key
    "The primary table key of the demo internal table here corresponds to the secondary table
    "key in the itab_10_sec_key_a method.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab TRANSPORTING NO FIELDS WITH TABLE KEY comp1 = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_11_sec_key_freq_mod_a.
    "A standard table is looped across. The table specifies a secondary table key.
    "Reads are performed in the loop.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT standard_tab_w_sec_key ASSIGNING FIELD-SYMBOL(<fs>) TO 200.
        DATA(comp) = <fs>-comp6.
        READ TABLE standard_tab_w_sec_key TRANSPORTING NO FIELDS WITH TABLE KEY sec_key COMPONENTS comp1 = sy-tabix.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_11_sec_key_freq_mod_z.
    "A standard table is looped across. The table specifies a secondary table key.
    "The loop contains table modifications.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT standard_tab_w_sec_key ASSIGNING FIELD-SYMBOL(<fs>) TO 200.
        <fs>-comp1 = sy-tabix.
        READ TABLE standard_tab_w_sec_key TRANSPORTING NO FIELDS WITH TABLE KEY sec_key COMPONENTS comp1 = sy-tabix.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_12_sort_itab_a.
    "A standard table declared with the standard key is sorted.
    "The sort key is explicitly specified in the SORT statement.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SORT standard_tab_w_default_key BY comp1 ASCENDING.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_12_sort_itab_z.
    "A standard table declared with the standard key is sorted.
    "The sort key is not explicitly specified in the SORT statement.
    "Instead, the standard key is used by default.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      SORT standard_tab_w_default_key.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_13_clear_free_a.
    "Clearing and populating an internal table
    "The table is cleared using a CLEAR statement.

    "Preparing demo data
    DATA str_tab_a TYPE string_table.

    DO 1000 TIMES.
      APPEND sy-tabix TO str_tab_a.
    ENDDO.
    DATA(str_tab_b) = str_tab_a.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      CLEAR str_tab_a.
      APPEND LINES OF str_tab_b TO str_tab_a.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_13_clear_free_z.
    "Clearing and populating an internal table
    "The table is cleared using a FREE statement.

    "Preparing demo data
    DATA str_tab_a TYPE string_table.

    DO 10000 TIMES.
      APPEND sy-tabix TO str_tab_a.
    ENDDO.
    DATA(str_tab_b) = str_tab_a.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      FREE str_tab_a.
      APPEND LINES OF str_tab_b TO str_tab_a.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_14_dyn_itab_spec_a.
    "READ TABLE statement using static key specifications

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab_w_sec_key TRANSPORTING NO FIELDS WITH TABLE KEY sec_key COMPONENTS comp1 = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_14_dyn_itab_spec_z.
    "READ TABLE statement using dynamic key specifications

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE standard_tab_w_sec_key TRANSPORTING NO FIELDS WITH TABLE KEY ('SEC_KEY') COMPONENTS ('COMP1') = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_01_while_do_a.
    "WHILE statement for comparing WHILE and DO

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      DATA num TYPE i.
      WHILE num <= 1000.
        num += 1.
      ENDWHILE.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_01_while_do_z.
    "DO statement for comparing WHILE and DO

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      DATA num TYPE i.

      DO.
        num += 1.
        IF sy-index = 1000.
          EXIT.
        ENDIF.
      ENDDO.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_02_strings_a.
    "Concatenating a variable-length string of type string

    DO loop_count_1000 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      DATA: str_a TYPE string,
            str_b TYPE string VALUE '#'.

      str_a &&= str_b.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_02_strings_z.
    "Concatenating a fixed-length string of type c

    DO loop_count_1000 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      DATA: char_a TYPE c LENGTH 1000,
            char_b TYPE c LENGTH 1 VALUE '#'.

      char_a &&= char_b.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_03_string_templates_a.
    "String specified as string literal using backquotes

    DO 5000 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      DATA str TYPE string.
      str = `ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP`.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_03_string_templates_z.
    "String template only using characters, without embedded expression

    DO 5000 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      DATA str TYPE string.
      str = |ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP ABAP|.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_05_type_conversion_a.
    "Calculation of numbers that share the same type

    DO loop_count_1000 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      DATA: num1   TYPE decfloat34 VALUE '1.2345',
            num2   TYPE decfloat34 VALUE '12345',
            num3   TYPE decfloat34 VALUE '12345',
            num4   TYPE decfloat34 VALUE '1.2345',
            num5   TYPE decfloat34 VALUE '1.2345',
            num6   TYPE decfloat34 VALUE '1.2345',
            result TYPE decfloat34.

      result = num1 + num2 + num3 + num4 + num5 + num6.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_05_type_conversion_z.
    "Calculation of numbers that have different types triggering implicit
    "type conversions

    DO loop_count_1000 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      DATA: num1   TYPE f VALUE '1.2345',
            num2   TYPE i VALUE 12345,
            num3   TYPE int8 VALUE 12345,
            num4   TYPE p LENGTH 16 DECIMALS 8 VALUE '1.2345',
            num5   TYPE decfloat16 VALUE '1.2345',
            num6   TYPE decfloat34 VALUE '1.2345',
            result TYPE decfloat34.

      result = num1 + num2 + num3 + num4 + num5 + num6.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
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

  METHOD misc_06_formal_parameters_a.
    "Actual parameters passed by reference

    DATA(str_tab) = VALUE string_table( ( `A` ) ( `B` ) ( `A` ) ( `P` ) ).

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      meth_pass_by_ref( a = sy-index
                        b = sy-index
                        c = str_tab
                        d = str_tab ).
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_06_formal_parameters_z.
    "Actual parameters passed by value

    DATA(str_tab) = VALUE string_table( ( `A` ) ( `B` ) ( `A` ) ( `P` ) ).

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      meth_pass_by_value( i = sy-index
                          j = sy-index
                          k = str_tab
                          l = str_tab ).
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_07_parallel_processing_a.
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

    out->write( name ).
    IF lines( it ) < 250.
      out->write( `Not all database entries were written.` ).
      out->write( |Table lines with value num1 = 1: { lines( it ) }| ).
    ELSE.
      DATA(timestamp1) = it[ 1 ]-crea_date_time.
      DATA(timestamp2) = it[ lines( it ) ]-crea_date_time.

      DATA(diff) = cl_abap_tstmp=>subtract(
            tstmp1 = timestamp2
            tstmp2 = timestamp1 ).

      out->write( |{ diff STYLE = SIMPLE }| ).
      out->write( |Table lines with value num1 = 1: { lines( it ) }| ).
    ENDIF.
  ENDMETHOD.

  METHOD misc_07_parallel_processing_z.
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

    out->write( name ).
    IF lines( it ) < 250.
      out->write( `Not all database entries were written.` ).
      out->write( |Table lines with value num1 = 2: { lines( it ) }| ).
    ELSE.
      DATA(timestamp1) = it[ 1 ]-crea_date_time.
      DATA(timestamp2) = it[ lines( it ) ]-crea_date_time.

      DATA(diff) = cl_abap_tstmp=>subtract(
            tstmp1 = timestamp2
            tstmp2 = timestamp1 ).

      out->write( |{ diff STYLE = SIMPLE }| ).
      out->write( |Table lines with value num2 = 2: { lines( it ) }| ).
    ENDIF.
  ENDMETHOD.

  METHOD itab_15_read_where_a.
    "READ TABLE ... WITH KEY as an alternative to a READ TABLE ... WHERE statement

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab TRANSPORTING NO FIELDS WITH KEY comp5 = sy-index.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_15_read_where_z.
    "READ TABLE ... WHERE statement, which can also be expressed by specifying a key
    "using a READ TABLE ... WITH KEY statement.
    "Without the pragma, a syntax warning would be displayed informing about performance
    "issues with the statement.

    DO loop_count_500 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      READ TABLE sorted_tab TRANSPORTING NO FIELDS WHERE comp5 = sy-index ##read_where_ok.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_16_find_replace_table_a.
    "FIND IN TABLE and REPLACE IN TABLE statements to perform block-wise operations
    "in an internal table with character-like line type

    "Populating demo table
    DATA str_tab TYPE string_table.
    DO 100 TIMES.
      APPEND |ABP{ sy-index }| TO str_tab.
    ENDDO.
    DATA(str_tab_copy) = str_tab.

    DO loop_count_50 TIMES.
      str_tab = str_tab_copy.
      ts1 = utclong_current( ).
**********************************************************************
      FIND ALL OCCURRENCES OF `ABP`
        IN TABLE str_tab
        RESULTS DATA(res_tab)
        RESPECTING CASE.

      REPLACE ALL OCCURRENCES OF `ABP`
        IN TABLE str_tab
        WITH `ABAP`
        RESPECTING CASE.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD itab_16_find_replace_table_z.
    "Line by line search and replacement in an internal table
    "with character-like line type

    "Populating demo table
    DATA str_tab TYPE string_table.
    DO loop_count_50 TIMES.
      APPEND |ABP{ sy-index }| TO str_tab.
    ENDDO.
    DATA(str_tab_copy) = str_tab.

    DO loop_count_500 TIMES.
      str_tab = str_tab_copy.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT str_tab ASSIGNING FIELD-SYMBOL(<fs>).
        FIND ALL OCCURRENCES OF `ABP`
          IN <fs>
          RESULTS DATA(res_tab)
          RESPECTING CASE.
      ENDLOOP.

      LOOP AT str_tab ASSIGNING <fs>.
        REPLACE ALL OCCURRENCES OF `ABP`
          IN TABLE str_tab
          WITH `ABAP`
          RESPECTING CASE.
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD eml_01_eml_in_loop_a.
    "EML update request using a FOR loop for a block-wise operation

    DATA read_result TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m\\root.
    DO loop_count_5 TIMES.
      APPEND VALUE #( key_field = sy-index field3 = sy-index field4 = sy-index ) TO read_result.
    ENDDO.

    DO loop_count_5 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      MODIFY ENTITIES OF zdemo_abap_rap_ro_m
        ENTITY root
        UPDATE FIELDS ( field3 field4 ) WITH VALUE #( FOR key IN read_result ( %tky   = key-%tky
                                                                               field3 = key-field3 + 1
                                                                               field4 = key-field4 + 1 ) ).
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD eml_01_eml_in_loop_z.
    "Multiple individual EML update requests in a loop

    DATA result TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m\\root.
    DO loop_count_5 TIMES.
      APPEND VALUE #( key_field = sy-index field3 = sy-index field4 = sy-index ) TO result.
    ENDDO.
    DATA upd TYPE TABLE FOR UPDATE zdemo_abap_rap_ro_m\\root.
    upd = VALUE #( FOR wa IN result ( %tky = wa-%tky field3 = wa-field3 + 1 field4 = wa-field4 + 1 ) ).

    DO loop_count_5 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      LOOP AT upd ASSIGNING FIELD-SYMBOL(<fs>).
        MODIFY ENTITIES OF zdemo_abap_rap_ro_m
         ENTITY root
         UPDATE FIELDS ( field3 field4 ) WITH VALUE #( ( <fs> ) ).
      ENDLOOP.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_04_search_pattern_a.
  "Simple pattern-based search using IF and CP
  DATA(str) = `abc_def_ghi`.

  DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      IF str CP `*f#_*`.
       ...
      ENDIF.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD misc_04_search_pattern_z.
  "Using FIND and a regular expression

  DATA(str) = `abc_def_ghi`.

  DO loop_count_50 TIMES.
      ts1 = utclong_current( ).
**********************************************************************
      FIND PCRE `.*f_.*` in str.

      IF sy-subrc = 0.
       ...
      ENDIF.
**********************************************************************
      store_runtime( high = utclong_current( ) low = ts1 ).
    ENDDO.

    runtime = get_avg_runtime( ).
    out->write( name ).
    out->write( |{ runtime STYLE = SIMPLE }| ).
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
    DO 50 TIMES.
      GET TIME STAMP FIELD ts_long.
      "Suboptimal database table insertion in a loop serving the example demonstration purpose
      INSERT zdemo_abap_tabca FROM @( VALUE #( id = xco_cp=>uuid( )->value num1 = 1 crea_date_time = ts_long ) ).
    ENDDO.
  ENDMETHOD.

  METHOD meth.
    DATA ts_long TYPE timestampl.
    DO 50 TIMES.
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
