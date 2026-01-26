<a name="top"></a>

# WHERE Conditions in ABAP Statements

- [WHERE Conditions in ABAP Statements](#where-conditions-in-abap-statements)
  - [WHERE Conditions in ABAP SQL Statements](#where-conditions-in-abap-sql-statements)
  - [WHERE Conditions in ABAP Statements for Processing Internal Tables](#where-conditions-in-abap-statements-for-processing-internal-tables)
    - [Executable Example](#executable-example)

This cheat sheet focuses on `WHERE` conditions and explores various syntax options in ABAP statements that include `WHERE` for data filtering. This is relevant, for example, when retrieving data from a data source using ABAP SQL or when processing internal tables with ABAP statements. For all details and syntax options, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENABAP.html). Several aspects and code snippets in this cheat sheet are also available in other cheat sheets.

> [!NOTE]
> - Most examples in the cheat sheet use internal tables as data sources for ABAP SQL `SELECT` statements to have self-contained examples. Use `SELECT` with internal tables as data sources only when SQL functionalities like joins exceed ABAP statements. For more details, refer to the [Internal Tables](01_Internal_Tables.md) cheat sheet.
> - Some examples also use artifacts from the ABAP cheat sheet repository. To check out these examples, ensure you have imported the ABAP cheat sheet repository into your system.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## WHERE Conditions in ABAP SQL Statements 

- Using [logical expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogical_expression_glosry.htm "Glossary Entry"), you can limit the number of rows in the result set or those to be modified in a data source.
- A row is included in the result set or modified in the data source only if the logical expression is true.
- Certain restrictions apply to logical expressions. For example, columns of types like `string` and `rawstring` cannot be used.
- Regarding data retrieval, columns in the logical expressions do not need to be part of the result set.
- You can combine multiple logical expressions using `AND` or `OR`.
- Combining logical expressions in parentheses allows the refinement of conditions. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_logexp_andornot.htm).
- Conditions can be specified in ABAP SQL statements in these contexts:
  - Queries with `SELECT`: `WHERE` and `HAVING` clauses
  - Modifying operations: `WHERE` clause in `UPDATE` and `DELETE` statements

The following table provides an overview of SQL conditions. Note that the code snippets are from the executable example further down.

<table>

<tr>
<td> Syntax Variant </td> <td> Notes </td> <td> Code Snippet </td>
</tr>

<tr>
<td> 

`... a = b ...` <br> `... a EQ b ...` 

 </td>

 <td> 

 The content of two operands is equal.

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab 
  WHERE animal = 'bear' 
  INTO TABLE @it.

SELECT id FROM @itab AS tab 
  WHERE animal EQ 'bear' 
  INTO TABLE @it.  
``` 

 </td>
</tr>


<tr>
<td> 

`... a <> b ...` <br> `... a NE b ...` 

 </td>

 <td> 

The content of two operands is not equal.

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab 
  WHERE animal <> 'bear' 
  INTO TABLE @it.

SELECT id FROM @itab AS tab 
  WHERE animal NE 'bear' 
  INTO TABLE @it.
``` 

 </td>
</tr>

<tr>
<td> 

`... a < b ...` <br> `... a LT b ...`

 </td>

 <td> 

The content of the left operand is less than the content of the right operand.

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab 
  WHERE count < 15 
  INTO TABLE @it.

SELECT id FROM @itab AS tab 
  WHERE count LT 15 
  INTO TABLE @it.
``` 

 </td>
</tr>

<tr>
<td> 

`... a > b ...` <br> `... a GT b ...` 

 </td>

 <td> 

The content of the left operand is greater than the content of the right operand.

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab 
  WHERE count > 15 
  INTO TABLE @it.

SELECT id FROM @itab AS tab 
  WHERE count GT 15 
  INTO TABLE @it.
``` 

 </td>
</tr>

<tr>
<td> 

`... a <= b ...` <br> `... a LE b ...` 

 </td>

 <td> 

The content of the left operand is less than or equal to the content of the right operand.

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab 
  WHERE count <= 15 
  INTO TABLE @it.

SELECT id FROM @itab AS tab 
  WHERE count LE 15 
  INTO TABLE @it.
``` 

 </td>
</tr>

<tr>
<td> 

`... a >= b ...` <br> `... a GE b ...`

 </td>

 <td> 

The content of the left operand is greater than or equal to the content of the right operand.

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab 
  WHERE count >= 15 
  INTO TABLE @it.

SELECT id FROM @itab AS tab 
  WHERE count GE 15 
  INTO TABLE @it.
``` 

 </td>
</tr>

<tr>
<td> 

`... AND [NOT] ...` <br> `... OR [NOT] ...` <br> `... ( ... ) ...`

 </td>

 <td> 

Combining multiple logical expressions into one logical expression using `AND` or `OR` (inlcuding negations). To further detail out the desired condition, expressions within parentheses are possible.

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab
  WHERE animal = 'bear' AND count = 5
  INTO TABLE @it.

SELECT id FROM @itab AS tab
  WHERE animal = 'kangaroo' OR count = 4
  INTO TABLE @it.

SELECT id FROM @itab AS tab
  WHERE ( animal = 'bear' AND count = 5 )
  AND ( animal = 'lion' AND count = 20 )
  INTO TABLE @it.

SELECT id FROM @itab AS tab
  WHERE ( animal = 'bear' AND count = 5 )
  OR ( animal = 'lion' AND count = 20 )
  INTO TABLE @it.

SELECT id FROM @itab AS tab
  WHERE count > 10
  OR NOT ( animal = 'kangaroo' AND count = 8 )
  INTO TABLE @it.  
``` 

 </td>
</tr>

<tr>
<td> 

`... a [=|<>|>|<|...] [ALL|ANY|SOME] ( SELECT ... ) ...`

 </td>

 <td> 

The content of a single operand is compared with the result set of a scalar subquery using a comparison operator. Multiple subqueries can be included with `UNION`, `INTERSECT`, or `EXCEPT`. The additions `ALL`, `ANY`, and `SOME` can only be omitted if the subquery result set contains a single row. Otherwise, an exception occurs with a multi-row result set. One of the additions must be specified in the case of multi-row result set. For `ALL`, the expression is true if the comparison holds for all rows in the result set. For `ANY` or `SOME`, the expression is true if the comparison holds for at least one row. Using `=` or `EQ` with `ANY` or `SOME` is similar to using `IN (SELECT ...)`. 

 </td>

 <td> 

``` abap
"The following example assumes there is a single-row result set of the subquery.
SELECT id FROM @itab AS tab
  WHERE count = ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 = 40 )
  INTO TABLE @it.

"ALL addition
SELECT id FROM @itab AS tab
  WHERE count > ALL ( SELECT key_field FROM zdemo_abap_tab1 )
  INTO TABLE @it.    

"ANY addition
SELECT id FROM @itab AS tab
  WHERE count = ANY ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 <= 40 )
  INTO TABLE @it.

"SOME addition (yields the same result as the previous statement)
SELECT id FROM @itab AS tab
  WHERE count = SOME ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 <= 40 )
  INTO TABLE @it.
``` 

 </td>
</tr>

<tr>
<td> 

`... a [NOT] BETWEEN b AND c ...`

 </td>

 <td> 

The content of the left operand is (not) between the value of the two other operands. The syntax variant is like specifying `... [NOT] ( a >= b AND a <= c ) ...`.

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab
  WHERE count BETWEEN 1 AND 10
  INTO TABLE @it. 

"Negation with NOT
SELECT id FROM @itab AS tab
  WHERE count NOT BETWEEN 1 AND 10
  INTO TABLE @it.       
``` 

 </td>
</tr>

<tr>
<td> 

`... a [NOT] LIKE b [ESCAPE c] ...`

 </td>

 <td> 

Checks whether the content of the left operand matches (or does not match) a specified pattern. The pattern can be specified by using wildcard characters. `%` stands for any character string, including an empty string. `_` stands for any character. <br> Using the `ESCAPE` addition, you can specify a single-character escape character of length 1 (e.g. `#`) in front of a wildcard character or the escaped character itself. For example, to search for the pattern `100%`, you may use an expression such as the the following: `... LIKE '100#%' ESCAPE '#' ...`.

 </td>

 <td> 

``` abap
SELECT animal FROM @itab AS tab
  WHERE animal LIKE '%ee%'
  OR animal LIKE '_e%'
  INTO TABLE @DATA(animals).

"Negation with NOT
SELECT animal FROM @itab AS tab
  WHERE animal NOT LIKE '_e%'
  INTO TABLE @animals.

"ESCAPE addition
"The following example matches any character sequence followed 
"by the % character.
SELECT animal FROM @itab AS tab
  WHERE animal LIKE '%#%' ESCAPE '#'
  INTO TABLE @animals. 
``` 

 </td>
</tr>

<tr>
<td> 

`... a IS [NOT] INITIAL ...`

 </td>

 <td> 

Checks whether the content of an operand is (not) the initial value of its [built-in DDIC type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_ddic_type_glosry.htm). 

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab
  WHERE count IS INITIAL
  INTO TABLE @it. 

"Negation with NOT
SELECT id FROM @itab AS tab
  WHERE count IS NOT INITIAL
  INTO TABLE @it. 
``` 

 </td>
</tr>

<tr>
<td> 

`... a [NOT] IN (b, c, ...)...`

 </td>

 <td> 

Checks whether the content of the left operand is (not) contained in a set of a parenthesized, comma-separated list of values. You can also specify just one operand in the parentheses. 

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab
  WHERE animal IN ( 'elephant', 'gorilla', 'dog', 'snake' )
  INTO TABLE @it.
    
"Negation with NOT
SELECT id FROM @itab AS tab
  WHERE animal NOT IN ( 'chimpanzee', 'dog', 'snake' )
  INTO TABLE @it. 

"Blanks after the first parentheses and before the second are not mandatory.
"This also applies to blanks within the parentheses. However, choose either
"to use the blanks or not.
SELECT id FROM @itab AS tab
  WHERE animal IN ('elephant','gorilla','dog','snake')
  INTO TABLE @it.       
``` 

 </td>
</tr>

<tr>
<td> 

`... a [NOT] IN ( SELECT ... ) ...`

 </td>

 <td> 

Checks whether the content of the left operand is (not) contained in the result set of a scalar subquery. Note that multiple subqueries can be included using `UNION`, `INTERSECT`, `EXCEPT`.

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab
  WHERE count IN ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 <= 40 )
  INTO TABLE @it. 

"Negation with NOT
SELECT id FROM @itab AS tab
  WHERE count NOT IN ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 <= 40 )
  INTO TABLE @it. 
``` 

 </td>
</tr>

<tr>
<td> 

`... ( a, b, ... ) IN ( ( d, e, ... ) ( f, g, ...) ... ) ...`

 </td>

 <td> 

Checks whether each of the values of multiple operands in a parenthesized, comma-separated list on the left side of `IN` matches value tuples in the same place specified in parentheses on the right side of `IN`. Unlike the syntax option `... a [NOT] IN (b, c, ...) ...`, this syntax option allows [SQL expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_expression_glosry.htm) on the right side of `IN`. Note that a negation with `NOT` is not supported. 

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab
  WHERE ( id, animal ) IN ( ( 1, 'bear' ), ( 3, 'giraffe' ), ( 987, 'flamingo' ), ( 2, 'dog' ) )
  INTO TABLE @it. 
``` 

 </td>
</tr>

<tr>
<td> 

`... ( a, b, ... ) IN ( SELECT ... ) ...`

 </td>

 <td> 

Checks whether each value of multiple operands in a parenthesized, comma-separated list on the left side of `IN` matches the content of a subquery result set. The result set must contain the same number of elements as specified in the parentheses on the left side of `IN`. As above, the position of elements in the result set is relevant. Note that multiple subqueries can be included using `UNION`, `INTERSECT`, `EXCEPT`, and that a negation with `NOT` is not supported.

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab
  WHERE ( id, count ) IN ( SELECT key_field, num1 FROM zdemo_abap_tab1 )
  INTO TABLE @it. 
``` 

 </td>
</tr>

<tr>
<td> 

`... a [NOT] IN @ranges_table ...`

 </td>

 <td> 

Checks whether the operand on the left side  of `IN` matches (or does not match) [ranges conditions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenranges_condition_glosry.htm) specified in a [ranges table](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenranges_table_glosry.htm). Note that the operators `CP` and `NP` are transformed into `LIKE` conditions (as a consequence, the conditions for `CP` and `NP` are case-sensitive). 

 </td>

 <td> 

``` abap
DATA rangestab TYPE RANGE OF i.
"Value range between 1 and 10
rangestab = VALUE #( ( sign = 'I' option = 'BT' low = 1 high = 10 ) ).

SELECT id FROM @itab AS tab
  WHERE count IN @rangestab
  INTO TABLE @it. 

"Value range: Lower than 5 + greater than or equal to 25
rangestab = VALUE #( ( sign   = 'I' option = 'LT' low = 5 )
                     ( sign   = 'I' option = 'GE' low = 25  ) ).

SELECT id FROM @itab AS tab
  WHERE count IN @rangestab
  INTO TABLE @it.
``` 

 </td>
</tr>

<tr>
<td> 

`... EXISTS ( SELECT ... ) ...`

 </td>

 <td> 

Checks the result of a subquery. The comparison is true if the result set contains at least one row. Note that data source fields specified in the subquery are not relevant. You may also just use a single literal representing a column.  Note that multiple subqueries can be included using `UNION`, `INTERSECT`, `EXCEPT`.

 </td>

 <td> 

``` abap
SELECT id FROM @itab AS tab
  WHERE EXISTS ( SELECT 'X' FROM zdemo_abap_tab1 WHERE key_field = tab~id )
  INTO TABLE @it.
``` 

 </td>
</tr>

<tr>
<td> 

`... a IS [NOT] NULL ...`

 </td>

 <td> 

Checks whether the value of an operand is (not) the [null value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennull_value_glosry.htm). Find more information in the executable example (which also includes the `INDICATORS NULL STRUCTURE` addition to the `INTO` clause) and in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwhere_logexp_null.htm). 

 </td>

 <td> 

``` abap
SELECT tab2~key_field, tab1~char2
  FROM zdemo_abap_tab2 AS tab2
  LEFT OUTER JOIN zdemo_abap_tab1 AS tab1 ON tab1~char1 = tab2~char1
  WHERE tab1~char1 IS NULL
  INTO TABLE @DATA(joined_tab).

"Negation IS NOT NULL
SELECT tab2~key_field, tab1~char2
  FROM zdemo_abap_tab2 AS tab2
  LEFT OUTER JOIN zdemo_abap_tab1 AS tab1 ON tab1~char1 = tab2~char1
  WHERE tab1~char1 IS NOT NULL
  INTO TABLE @joined_tab.  
``` 

 </td>
</tr>

<tr>
<td> 

`... (dynamic_where_clause) ...` 

 </td>

 <td> 

Dynamic `WHERE` condition specified as parenthesized data objects. These data objects should contain the syntax of a logical expression. As `dynamic_where_clause`, a character-like data object or a standard table with character-like line type is expected. The syntax is not case-sensitive. For more information, see the [Dynamic Programming cheat sheet](06_Dynamic_Programming.md), also with respect to potential security risks regarding dynamic specifications.

 </td>

 <td> 

``` abap
DATA(dynamic_where_clause) = `count > 15`.

SELECT id FROM @itab AS tab
  WHERE (dynamic_where_clause)
  INTO TABLE @it. 

DATA(dyn_where_cl_as_tab) = VALUE string_table( ( `animal = 'kangaroo'` )
                                                ( `OR` )
                                                ( `count = 4` ) ).

SELECT id FROM @itab AS tab
  WHERE (dyn_where_cl_as_tab)
  INTO TABLE @it. 
``` 

 </td>
</tr>

</table>


> [!NOTE]
> - Some subqueries in the syntax variants must be scalar subqueries. This means that the subquery returns a single-column result set. The `SELECT` list of the subquery must only contain a single element.
> - See [this topic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPSQL_EXPR.html) in the ABAP Keyword Documentation what can be specified as operands on the left and right side. 
> - The comparisons are done on the database, so there is no type conversions in ABAP beforehand. Note that platform-dependent conversion behavior may be applied. For SAP HANA Platform-related conversion rules, see [this topic](https://help.sap.com/docs/SAP_HANA_PLATFORM/4fe29514fd584807ac9f2a04f6754767/b4b0eec1968f41a099c828a4a6c8ca0f.html).
> - See [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenwhere_logexp_compare_types.html) a list of comparable types in the ABAP Dictionary.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## WHERE Conditions in ABAP Statements for Processing Internal Tables

- The following ABAP statements and constructor operators, used for processing internal tables, allow `WHERE` conditions to be specified:
  - `LOOP AT`
  - `READ TABLE`
  - `DELETE` 
  - Table filtering using the `FILTER` constructor operator
  - `FOR` loops in the context of table comprehensions and reductions (using, e.g., the `VALUE` - having the exact semantics as `LOOP AT`; the `NEW` operator is also possible - and `REDUCE` operators)
- The variety of syntax options differs from that available for ABAP SQL statements.
- The following syntax options are supported for `WHERE` conditions for these: `LOOP AT`, `READ TABLE` (searches for the first line matching the `WHERE` condition in the internal table), `DELETE`, and `FOR` loops.   
  - Comparison expressions
    - Comparison operators as above (`=`, `EQ`, `<>`, `NE`, `>`, `GT`, `<`, `LT`, `>=`, `GE`, `<=`, `LE`)
    - Comparison operators for character-like data types `CO`, `CN`, `CA`, `NA`, `CS`, `NS`, `CP`, `NP` (see the [String Processing cheat sheet](07_String_Processing.md#comparison-operators-for-character-like-data-types-in-a-nutshell))
    - [Comparison operators for byte-like data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_bytes.htm), e.g. `BYTE-CO` and others; and [bit patterns](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENLOGEXP_BITMASKS.html)
    - `[NOT] BETWEEN ... AND ...`
    - `[NOT] IN ranges_table`
  - [Predicate expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpredicate_expression_glosry.htm)
    - `IS [NOT] INITIAL`
    - `IS [NOT] BOUND`
    - `IS [NOT] INSTANCE OF`
  - Dynamic `WHERE` conditions: Any character-like data object or standard table with character-like line type containing logical expressions can be specified within a pair of parentheses. 
  - Logical expressions can be combined with `AND`, `OR`, `EQUIV`, also using parenthesized expressions.
- `FILTER` constructor operator
  - To use the `FILTER` operator, certain prerequisites must be met:
    - The source table needs at least one sorted or hash key.
    - If the table lacks such a primary table key, a secondary table key must be available.
  - Only table key columns can be compared with single values in the `WHERE` condition. The filtering can also be based on a filter table.
  - The operator has restricted syntax options:
    - For source tables with hash keys: Only `=` is supported for key component comparisons.
    - For source tables with sorted keys: `=`, `EQ`, `<>`, `NE`, `<`, `LT`, `>`, `GT`, `<=`, `LE`, `>=`, `GE` are supported (at least the initial part of the key must be specified).
    - `IS [NOT] INITIAL` is supported.
    - Multiple comparisons can only be combined using `AND`.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Executable Example 

[zcl_demo_abap_where_conditions](./src/zcl_demo_abap_where_conditions.clas.abap)

> [!NOTE]
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)