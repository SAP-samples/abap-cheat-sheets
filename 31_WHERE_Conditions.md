<a name="top"></a>

# WHERE Conditions in ABAP Statements

- [WHERE Conditions in ABAP Statements](#where-conditions-in-abap-statements)
  - [WHERE Conditions in ABAP SQL Statements](#where-conditions-in-abap-sql-statements)
    - [Executable Example (ABAP SQL SELECT Statements)](#executable-example-abap-sql-select-statements)
  - [WHERE Conditions in ABAP Statements for Processing Internal Tables](#where-conditions-in-abap-statements-for-processing-internal-tables)
    - [Executable Example (Statements for Internal Tables)](#executable-example-statements-for-internal-tables)

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

### Executable Example (ABAP SQL SELECT Statements) 

To try the example out, create a demo class named `zcl_demo_abap` and paste the code into it. The example is not set up to display output in the console. Some results are included as comments for quick reference. After activation, you may want to set break points, choose *F9* in ADT to execute the class, and walk through the code to explore the effect of the statements. 

> [!NOTE]
> - Many ABAP SQL `SELECT` statements in the example use an internal table as the data source to work with simple data.
> - Some examples also use subqueries. In these cases, another internal table cannot currently serve as the data source in the subquery. Therefore, examples use demo database tables from the ABAP cheat repository. They are also used to demonstrate `IS [NOT] NULL`. As a prerequisite, you have imported the ABAP cheat sheet repository to run the example class.


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

*&---------------------------------------------------------------------*
*& Demo data preparation
*&---------------------------------------------------------------------*

    "Demo internal table preparation
    TYPES: BEGIN OF zoo_struc,
             id     TYPE i,
             animal TYPE c LENGTH 15,
             count  TYPE i,
           END OF zoo_struc.
    DATA itab TYPE SORTED TABLE OF zoo_struc WITH UNIQUE KEY id.
    "Populating internal table with data to work with in the examples
    itab = VALUE #( ( id = 1 animal = 'bear' count = 5 )
                    ( id = 2 animal = 'elephant' count = 12 )
                    ( id = 3 animal = 'giraffe' count = 30 )
                    ( id = 4 animal = 'zebra' count = 15 )
                    ( id = 5 animal = 'gorilla' count = 10 )
                    ( id = 6 animal = 'tiger' count = 3 )
                    ( id = 7 animal = 'kangaroo' count = 8 )
                    ( id = 8 animal = 'penguin' count = 50 )
                    ( id = 9 animal = 'chimpanzee' count = 20 )
                    ( id = 10 animal = 'lion' count = 4 )
                    ( id = 11 animal = 'flamingo' count = 25 )
                    ( id = 12 animal = 'eagle' count = 4 ) ).

    "Demo database table preparation
    DELETE FROM zdemo_abap_tab1.
    MODIFY zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 num1 = 10 )
                                                  ( key_field = 2 num1 = 20 )
                                                  ( key_field = 3 num1 = 30 )
                                                  ( key_field = 4 num1 = 40 )
                                                  ( key_field = 5 num1 = 50 ) ) ).

*&---------------------------------------------------------------------*
*& Comparison operators
*&---------------------------------------------------------------------*

    SELECT id FROM @itab AS tab WHERE animal = 'bear' INTO TABLE @DATA(it). "1
    SELECT id FROM @itab AS tab WHERE animal <> 'bear' INTO TABLE @it. "2-12
    SELECT id FROM @itab AS tab WHERE count > 15 INTO TABLE @it. "3,8,9,11
    SELECT id FROM @itab AS tab WHERE count >= 15 INTO TABLE @it. "3,4,8,9,11
    SELECT id FROM @itab AS tab WHERE count < 15 INTO TABLE @it. "1,2,5,6,7,10,12
    SELECT id FROM @itab AS tab WHERE count <= 15 INTO TABLE @it. "1,2,4,5,6,7,10,12

    SELECT id FROM @itab AS tab WHERE animal EQ 'bear' INTO TABLE @it.
    SELECT id FROM @itab AS tab WHERE animal NE 'bear' INTO TABLE @it.
    SELECT id FROM @itab AS tab WHERE count GT 15 INTO TABLE @it.
    SELECT id FROM @itab AS tab WHERE count GE 15 INTO TABLE @it.
    SELECT id FROM @itab AS tab WHERE count LT 15 INTO TABLE @it.
    SELECT id FROM @itab AS tab WHERE count LE 15 INTO TABLE @it.

*&---------------------------------------------------------------------*
*& Combining logical expressions using AND, OR and parentheses
*&---------------------------------------------------------------------*

    SELECT id FROM @itab AS tab
      WHERE animal = 'bear' AND count = 5
      INTO TABLE @it. "1

    SELECT id FROM @itab AS tab
      WHERE animal = 'kangaroo' OR count = 4
      INTO TABLE @it. "7,10,12

    "In the following example, the resulting table is initial. One of the expressions
    "in parentheses is false (AND is used between the expressions in parentheses).
    "In contrast, the example below returns an entry because of using OR.
    SELECT id FROM @itab AS tab
      WHERE ( animal = 'bear' AND count = 5 )
      AND ( animal = 'lion' AND count = 20 )
      INTO TABLE @it. "no entry

    SELECT id FROM @itab AS tab
      WHERE ( animal = 'bear' AND count = 5 )
      OR ( animal = 'lion' AND count = 20 )
      INTO TABLE @it. "1

    "Negations with NOT
    SELECT id FROM @itab AS tab
      WHERE count > 4
      AND NOT ( animal = 'kangaroo' AND count = 8 )
      INTO TABLE @it. "1,2,3,4,5,8,9,11

    SELECT id FROM @itab AS tab
      WHERE count > 4
      INTO TABLE @it.

    SELECT id FROM @itab AS tab
      WHERE count > 10
      OR NOT ( animal = 'kangaroo' AND count = 8 )
      INTO TABLE @it. "1,2,3,4,5,6,8,9,10,11,12

*&---------------------------------------------------------------------*
*& ... a [=|<>|>|<|...] [ALL|ANY|SOME] ( SELECT ... ) ...
*&---------------------------------------------------------------------*

    "Comparing a single operand with subquery result

    "--- No addition before the subquery ---
    "In this case, the subquery must only return a single row.
    "Otherwise, and if no ALL/ANY/SOME addition is specified, an exception
    "is raised, which is the case in the following example.
    TRY.
        SELECT id FROM @itab AS tab
          WHERE id = ( SELECT key_field FROM zdemo_abap_tab1 )
          INTO TABLE @it.
      CATCH cx_sy_open_sql_db INTO DATA(error).
        DATA(error_text) = error->get_text( ).
    ENDTRY.

    "The following example returns a single-row result set in the subquery.
    SELECT id FROM @itab AS tab
      WHERE count = ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 = 40 )
      INTO TABLE @it. "10,12

    "--- ALL addition before the subquery ---
    "The expression is true if the comparison is true for all rows of the result set.

    "The subquery returns 1, 2, 3, 4, 5. The ids of all those entries are finally
    "returned whose 'count' value is greater than 1-5.
    SELECT id FROM @itab AS tab
      WHERE count > ALL ( SELECT key_field FROM zdemo_abap_tab1 )
      INTO TABLE @it. "2,3,4,5,7,8,9,11

    "The subquery returns 1, 2, 3, 4. The ids of all those entries are finally returned
    "whose 'count' value does not match 1-4.
    SELECT id FROM @itab AS tab
      WHERE count <> ALL ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 <= 40 )
      INTO TABLE @it. "1,2,3,4,5,7,8,9,11

    "--- ANY/SOME additions before the subquery ---
    "The additions are interchangeable.

    SELECT id FROM @itab AS tab
      WHERE count = ANY ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 <= 40 )
      INTO TABLE @it. "6,10,12

    SELECT id FROM @itab AS tab
      WHERE count = SOME ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 <= 40 )
      INTO TABLE @it. "6,10,12

    "Using = or EQ with ANY or SOME is like using ... IN ( SELECT ... ) ...
    SELECT id FROM @itab AS tab
      WHERE count IN ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 <= 40 )
      INTO TABLE @it. "6,10,12

*&---------------------------------------------------------------------*
*& ... [NOT] BETWEEN ... AND ...
*&---------------------------------------------------------------------*

    SELECT id FROM @itab AS tab
      WHERE count BETWEEN 1 AND 10
      INTO TABLE @it. "1,5,6,7,10,12

    "The condition with BETWEEN above corresponds to the following condition.
    "The example makes use of a condition specified in parentheses to combine multiple
    "expressions.
    SELECT id FROM @itab AS tab
      WHERE ( count >= 1 AND count <= 10 )
      INTO TABLE @it. "1,5,6,7,10,12

    "Negation with NOT
    SELECT id FROM @itab AS tab
      WHERE count NOT BETWEEN 1 AND 10
      INTO TABLE @it. "2,3,4,8,9,11

*&---------------------------------------------------------------------*
*& ... IS [NOT] INITIAL ...
*&---------------------------------------------------------------------*

    SELECT id FROM @itab AS tab
      WHERE count IS INITIAL
      INTO TABLE @it. "no entry

    "Negation with NOT
    SELECT id FROM @itab AS tab
      WHERE count IS NOT INITIAL
      INTO TABLE @it. "1-12

*&---------------------------------------------------------------------*
*& ... [NOT] LIKE ...
*&---------------------------------------------------------------------*

    "Note: % (any character string), _ (any character).

    SELECT animal FROM @itab AS tab
      WHERE animal LIKE '%ee%'
      OR animal LIKE '_e%'
      INTO TABLE @DATA(animals). "bear,zebra,penguin,chimpanzee

    "Negation with NOT
    SELECT animal FROM @itab AS tab
      WHERE animal NOT LIKE '_e%'
      INTO TABLE @animals.
    "elephant,giraffe,gorilla,tiger,kangaroo,chimpanzee,lion,flamingo,eagle

    "ESCAPE addition for defining a single-character escape character
    "In the following example, this character is #. It is placed before
    "the % character in the specification after LIKE. In this case, %
    "is escaped and does then not stand for any character string in the
    "evaluation.
    "Adding a table entry for this syntax example.
    itab = VALUE #( BASE itab ( id = 13 animal = '100%' count = 0 ) ).
    "Any character sequence followed by the % character
    SELECT animal FROM @itab AS tab
      WHERE animal LIKE '%#%' ESCAPE '#'
      INTO TABLE @animals. "100%

    "Deleting the entry because it is not relevant for the further examples.
    DELETE itab INDEX 13.

*&---------------------------------------------------------------------*
*& ... a [NOT] IN (b, c, ...) ...
*&---------------------------------------------------------------------*

    "Checking if a single operand's value matches (or does not match) one of
    "the values within a specified set in parentheses.

    "The parenthesized value list includes not existent values.
    SELECT id FROM @itab AS tab
      WHERE animal IN ( 'elephant', 'gorilla', 'dog', 'snake' )
      INTO TABLE @it. "2,5

    "Just specifying a single value in the value set
    "Note to use host variables for local/global data objects.
    DATA(animal) = 'lion'.
    SELECT id FROM @itab AS tab
      WHERE animal IN ( @animal )
      INTO TABLE @it. "10

    "Negation with NOT
    SELECT id FROM @itab AS tab
      WHERE animal NOT IN ( 'chimpanzee', @animal, 'snake' )
      INTO TABLE @it.

    "Blanks after the first parentheses and before the second are not mandatory.
    "This also applies to blanks within the parentheses.
    SELECT id FROM @itab AS tab
      WHERE animal IN ('elephant','gorilla','dog','snake')
      INTO TABLE @it. "1-8,11,12

    "Specifying data source fields, host expressions or other SQL expressions
    "are not supported by this syntax variant.
    "Note that they are supported by the syntax variant IN with an operand list.
*SELECT id FROM @itab AS tab
*  WHERE counter IN ( @( 6 - 1 ), animal )
*  INTO TABLE @it.

*&---------------------------------------------------------------------*
*& ... a [NOT] IN ( SELECT ... ) ...
*&---------------------------------------------------------------------*
    "Checking if a single operand's value is contained in the result set of
    "a scalar subquery. UNION, INTERSECT, and EXCEPT can be specified for the
    "subquery.

    SELECT id FROM @itab AS tab
      WHERE count IN ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 <= 40 )
      INTO TABLE @it. "6,10,12

    "Negation with NOT
    SELECT id FROM @itab AS tab
      WHERE count NOT IN ( SELECT key_field FROM zdemo_abap_tab1 WHERE num1 <= 40 )
      INTO TABLE @it. "1,2,3,4,5,7,8,9,11

*&---------------------------------------------------------------------*
*& ... ( a, b, ... ) IN ( ( d, e, ... ) ( f, g, ...) ... ) ...
*&---------------------------------------------------------------------*

    "Checking if each of the values of multiple operands in a parenthesized,
    "comma-separated list on the left side of IN match value tuples in the same
    "place in parentheses on the right side of IN.
    "Unlike the syntax option ... a [NOT] IN (b, c, ...) ..., this syntax option
    "allows SQL expressions on the right side of IN.
    "Note that a negation with NOT is not supported.

    "In the following example, two values are specified in the operand list on the left.
    "Consequently, two values with appropriate types must be specified in parentheses
    "on the right.
    SELECT id FROM @itab AS tab
      WHERE ( id, animal ) IN ( ( 1, 'bear' ), ( 3, 'giraffe' ), ( 987, 'flamingo' ), ( 2, 'dog' ) )
      INTO TABLE @it. "1,3

    "The following example demonstrates that SQL expressions can be specified
    "with the syntax variant.
    DATA(flag) = abap_true.
    DATA(eagle) = 'eagle'.
    SELECT id FROM @itab AS tab
      WHERE ( id, animal ) IN ( ( @( 10 - 2 ), CASE @flag WHEN @abap_false THEN 'snake' ELSE 'penguin' END ),
                                ( @( 4 * 3 ), CAST( @eagle AS CHAR( 15 ) ) ), ( 987, 'flamingo' ), ( 2, 'dog' ) )
      INTO TABLE @it. "8,12

*&---------------------------------------------------------------------*
*& ... ( a, b, ... ) IN ( SELECT ... ) ...
*&---------------------------------------------------------------------*

    "Checking if each value of multiple operands in a parenthesized, comma-separated
    "list on the left side of IN matches the content of a subquery result set.
    "The result set must contain the same number of elements as specified in
    "the parentheses on the left side of IN. As above, the position of elements
    "in the result set is relevant. UNION, INTERSECT, and EXCEPT can be specified
    "for the subquery. Note that negation with NOT is not supported.

    SELECT id FROM @itab AS tab
      WHERE ( id, count ) IN ( SELECT key_field, num1 FROM zdemo_abap_tab1 )
      INTO TABLE @it. "3

*&---------------------------------------------------------------------*
*& ... a [NOT] IN @ranges_table ...
*&---------------------------------------------------------------------*

    "Checking if the value of an operand on the left side of IN matches (or does
    "not match) ranges conditions specified in a ranges table.

    "Populating a ranges table using the VALUE operator
    DATA rangestab TYPE RANGE OF i.
    "Value range between 1 and 10
    rangestab = VALUE #( ( sign = 'I' option = 'BT' low = 1 high = 10 ) ).

    SELECT id FROM @itab AS tab
      WHERE count IN @rangestab
      INTO TABLE @it. "1,5,6,7,10,12

    "Value range: Lower than 5 + greater than or equal to 25
    rangestab = VALUE #( ( sign   = 'I' option = 'LT' low = 5 )
                         ( sign   = 'I' option = 'GE' low = 25  ) ).

    SELECT id FROM @itab AS tab
      WHERE count IN @rangestab
      INTO TABLE @it. "3,6,8,10,11,12

    "Excursion
    "You cannot use logical operators such as CP (conforms to pattern) in the
    "WHERE clause. In a ranges table, they are possible.
    "Note:
    "- Regarding CP: * (any character sequence), + (any character), # (escape character)
    "- An equivalent example above uses the LIKE addition.

    DATA rt TYPE RANGE OF zoo_struc-animal.
    rt = VALUE #( ( sign = 'I' option = 'CP' low = '*ee*' ) "ee in a string
                  ( sign = 'I' option = 'CP' low = '+e*' ) ). "e in second position

    SELECT animal FROM @itab AS tab
      WHERE animal IN @rt
      INTO TABLE @animals. "bear,zebra,penguin,chimpanzee

*&---------------------------------------------------------------------*
*& ... EXISTS ( SELECT ... ) ...
*&---------------------------------------------------------------------*

    "Checking the result of a subquery. The comparison is true if the result
    "set contains at least one row. Note that the data source fields specified
    "in the subquery are not relevant. You may also just use a single literal
    "representing a column.
    "UNION, INTERSECT, and EXCEPT can be specified for the subquery.

    SELECT id FROM @itab AS tab
      WHERE EXISTS ( SELECT 'X' FROM zdemo_abap_tab1 WHERE key_field = tab~id )
      INTO TABLE @it. "1,2,3,4,5

*&---------------------------------------------------------------------*
*& ... a IS [NOT] NULL ...
*&---------------------------------------------------------------------*

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
*joined_tab:
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
*joined_tab_w_null_ind:
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
*joined_tab:
*KEY_FIELD    CHAR2
*1            y
*2            y
*3            z

*&---------------------------------------------------------------------*
*& ... (dynamic_where_clause) ...
*&---------------------------------------------------------------------*

    "Dynamic WHERE conditions can be specified as parenthesized data objects.
    "This data objects should contain the syntax of a logical expression.
    "As 'dynamic_where_clause', a character-like data object or a standard table
    "with character-like line type is expected. The syntax is not case-sensitive.
    "For more information, see the Dynamic Programming cheat sheet, also with
    "respect to potential security risks regarding dynamic specifications.

    DATA(dynamic_where_clause) = `count > 15`.

    SELECT id FROM @itab AS tab
      WHERE (dynamic_where_clause)
      INTO TABLE @it. "3,8,9,11

    DATA(dyn_where_cl_as_tab) = VALUE string_table( ( `animal = 'kangaroo'` )
                                                    ( `OR` )
                                                    ( `count = 4` ) ).

    SELECT id FROM @itab AS tab
      WHERE (dyn_where_cl_as_tab)
      INTO TABLE @it. "7,10,12

    "Wrong dynamic WHERE condition (no condition specified)
    dynamic_where_clause = `does not work`.

    TRY.
        SELECT id FROM @itab AS tab
          WHERE (dynamic_where_clause)
          INTO TABLE @it.
      CATCH cx_sy_dynamic_osql_syntax INTO DATA(dyn_error).
        DATA(dyn_error_text) = dyn_error->get_text( ).
    ENDTRY.

    "Wrong dynamic WHERE condition (an invalid single quote is specified after 4)
    dyn_where_cl_as_tab = VALUE string_table( ( `animal = 'kangaroo'` )
                                              ( `OR` )
                                              ( `count = 4'` ) ).

    TRY.
        SELECT id FROM @itab AS tab
          WHERE (dyn_where_cl_as_tab)
          INTO TABLE @it.
      CATCH cx_sy_dynamic_osql_syntax INTO dyn_error.
        dyn_error_text = dyn_error->get_text( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
```

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
    - [Comparison operators for byte-Like data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_bytes.htm), e.g. `BYTE-CO` and others; and [bit patterns](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENLOGEXP_BITMASKS.html)
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

### Executable Example (Statements for Internal Tables) 

> [!NOTE] 
> - The example snippets cover a selection and do not show all possible syntax variants, such as statements specifying table keys.
> - The evaluation of the `WHERE` condition depends on the table category and table key. Find more information on optimizing the `WHERE` condition [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENITAB_WHERE_OPTIMIZATION.html). These considerations are not relevant in the example snippets, as the focus is on syntax options.

To try the example out, create a demo class named `zcl_demo_abap` and paste the code into it. The example is not set up to display output. After activation, you may want to set break points, choose *F9* in ADT to execute the class, and walk through the code to explore the effect of the statements. 


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

*&---------------------------------------------------------------------*
*& Demo data types/objects
*&---------------------------------------------------------------------*

    TYPES: BEGIN OF demo_struc,
             id   TYPE i,
             text TYPE string,
             num  TYPE i,
             ref  TYPE REF TO string,
             oref TYPE REF TO object,
           END OF demo_struc,
           tab_type TYPE TABLE OF demo_struc WITH EMPTY KEY.

    DATA(itab) = VALUE tab_type(
      ( id = 1 text = `abc` num = 0 ref = REF #( `hello` ) oref = NEW cl_system_uuid( ) )
      ( id = 2 text = `abc` num = 20 )
      ( id = 3 text = `def` num = 0 ref = REF #( `world` ) oref = NEW cl_system_uuid( ) )
      ( id = 4 text = `efg` num = 40 )
      ( id = 5 text = `ghi` num = 50 ref = REF #( `ABAP` ) ) ).

    DATA str TYPE string.

*&---------------------------------------------------------------------*
*& LOOP AT statements
*&---------------------------------------------------------------------*

    "Comparison operators
    LOOP AT itab INTO DATA(wa) WHERE num = 0.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `13`.
    CLEAR str.

    LOOP AT itab INTO wa WHERE num > 20.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `45`.
    CLEAR str.

    "Combination of one or more logical expressions with the Boolean operators
    "NOT, AND, OR, EQUIV where parentheses are possible
    LOOP AT itab INTO wa WHERE id >= 3 AND num > 5.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `45`.
    CLEAR str.

    "Two logical expressions can be combined with EQUIV, which creates a new
    "logical expression. This expression is true if both expressions are true
    "or both expressions are false. If an expression returns a different
    "result than the other, the combined expression is false.
    "Example pattern: ... a = b EQUIV c = d ...
    "The combined expression is true if a/b and c/d are either both equal
    "or not.

    "The following loop yields 23 for str.
    "For the entry with id = 2, both expressions are false.
    "For the entry with id = 3, both expressions are true.
    LOOP AT itab INTO wa WHERE id > 2 EQUIV num < 10.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `23`.
    CLEAR str.

    LOOP AT itab INTO wa WHERE num >= 40 OR num = 0.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `1345`.
    CLEAR str.

    LOOP AT itab INTO wa WHERE NOT num = 0.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `245`.
    CLEAR str.

    LOOP AT itab INTO wa WHERE ( id < 5 AND num > 10 ) AND text = `abc`.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `2`.
    CLEAR str.

    "Comparison operators for character-like data types
    "contains only
    LOOP AT itab INTO wa WHERE text CO `abc`.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `12`.
    CLEAR str.

    "contains not only
    LOOP AT itab INTO wa WHERE text CN `abc`.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `345`.
    CLEAR str.

    "contains string
    LOOP AT itab INTO wa WHERE text CS `ef`.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `34`.
    CLEAR str.

    "contains no string
    LOOP AT itab INTO wa WHERE text NS `ef`.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `125`.
    CLEAR str.

    "contains any
    LOOP AT itab INTO wa WHERE text CA `xyzi`.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `5`.
    CLEAR str.

    "contains not any
    LOOP AT itab INTO wa WHERE text NA `a`.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `345`.
    CLEAR str.

    "conforms to pattern
    LOOP AT itab INTO wa WHERE text CP `*c`.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `12`.
    CLEAR str.

    "does not conform to pattern
    LOOP AT itab INTO wa WHERE text NA `*c`.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `345`.
    CLEAR str.

    "[NOT] BETWEEN ... AND ...
    LOOP AT itab INTO wa WHERE num BETWEEN 5 AND 45.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `24`.
    CLEAR str.

    LOOP AT itab INTO wa WHERE num NOT BETWEEN 5 AND 45.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `135`.
    CLEAR str.

    "[NOT] IN ranges_table
    DATA rangestab TYPE RANGE OF i.
    rangestab = VALUE #( ( sign = `I` option = `BT` low = 5 high = 45 ) ).

    LOOP AT itab INTO wa WHERE num IN rangestab.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `24`.
    CLEAR str.

    rangestab = VALUE #( ( sign   = `I` option = `LT` low = 5 )
                         ( sign   = `I` option = `GE` low = 40  ) ).

    LOOP AT itab INTO wa WHERE num IN rangestab.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `1345`.
    CLEAR str.

    "IS [NOT] INITIAL
    LOOP AT itab INTO wa WHERE num IS INITIAL.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `13`.
    CLEAR str.

    LOOP AT itab INTO wa WHERE num IS NOT INITIAL.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `245`.
    CLEAR str.

    "IS [NOT] BOUND
    LOOP AT itab INTO wa WHERE ref IS BOUND.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `135`.
    CLEAR str.

    LOOP AT itab INTO wa WHERE ref IS NOT BOUND.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `24`.
    CLEAR str.

    "IS [NOT] INSTANCE OF
    LOOP AT itab INTO wa WHERE oref IS INSTANCE OF cl_system_uuid.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `13`.
    CLEAR str.

    LOOP AT itab INTO wa WHERE oref IS NOT INSTANCE OF cl_system_uuid.
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `245`.
    CLEAR str.

    "Dynamic WHERE condition
    DATA(dynamic_where_cond) = `num > 20`.

    LOOP AT itab INTO wa WHERE (dynamic_where_cond).
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `45`.
    CLEAR str.

    DATA(dynamic_where_cond_tab) = VALUE string_table( ( `num > 20` )
                                                       ( `AND` )
                                                       ( `id > 4` ) ).

    LOOP AT itab INTO wa WHERE (dynamic_where_cond_tab).
      str &&= wa-id.
    ENDLOOP.
    ASSERT str = `5`.
    CLEAR str.

*&---------------------------------------------------------------------*
*& DELETE statements
*&---------------------------------------------------------------------*

    DATA(itab_copy) = itab.

    DELETE itab WHERE num > 20.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `123`.

    itab = itab_copy.

    DELETE itab WHERE num = 0.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `245`.
    itab = itab_copy.

    DELETE itab WHERE num > 5 AND id  >= 3.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `123`.
    itab = itab_copy.

    DELETE itab WHERE num >= 40 OR num = 0.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `2`.
    itab = itab_copy.

    DELETE itab WHERE NOT num = 0.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `13`.
    itab = itab_copy.

    DELETE itab WHERE ( id < 5 AND num > 10 ) AND text = `abc`.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `1345`.
    itab = itab_copy.

    "contains only
    DELETE itab WHERE text CO `abc`.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `345`.
    itab = itab_copy.

    "contains not only
    DELETE itab WHERE text CN `abc`.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `12`.
    itab = itab_copy.

    "contains string
    DELETE itab WHERE text CS `ef`.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `125`.
    itab = itab_copy.

    "contains no string
    DELETE itab WHERE text NS `ef`.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `34`.
    itab = itab_copy.

    "contains any
    DELETE itab WHERE text CA `xyzi`.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `1234`.
    itab = itab_copy.

    "contains not any
    DELETE itab WHERE text NA `a`.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `12`.
    itab = itab_copy.

    "conforms to pattern
    DELETE itab WHERE text CP `*c`.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `345`.
    itab = itab_copy.

    "does not conform to pattern
    DELETE itab WHERE text NA `*c`.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `12`.
    itab = itab_copy.

    DELETE itab WHERE num BETWEEN 5 AND 45.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `135`.
    itab = itab_copy.

    DELETE itab WHERE num NOT BETWEEN 5 AND 45.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `24`.
    itab = itab_copy.

    rangestab = VALUE #( ( sign = `I` option = `BT` low = 5 high = 45 ) ).
    DELETE itab WHERE num IN rangestab.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `135`.
    itab = itab_copy.

    DELETE itab WHERE num IS INITIAL.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `245`.
    itab = itab_copy.

    DELETE itab WHERE num IS NOT INITIAL.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `13`.
    itab = itab_copy.

    DELETE itab WHERE ref IS BOUND.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `24`.
    itab = itab_copy.

    DELETE itab WHERE ref IS NOT BOUND.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `135`.
    itab = itab_copy.

    DELETE itab WHERE oref IS INSTANCE OF cl_system_uuid.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `245`.
    itab = itab_copy.

    DELETE itab WHERE oref IS NOT INSTANCE OF cl_system_uuid.

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `13`.
    itab = itab_copy.

    dynamic_where_cond = `num > 20`.
    DELETE itab WHERE (dynamic_where_cond).

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `123`.
    itab = itab_copy.

    dynamic_where_cond_tab = VALUE string_table( ( `num > 20` )
                                                 ( `AND` )
                                                 ( `id > 4` ) ).
    DELETE itab WHERE (dynamic_where_cond_tab).

    ASSERT REDUCE string( INIT string = ``
                          FOR <line> IN itab
                          NEXT string &&= <line>-id ) = `1234`.
    itab = itab_copy.

*&---------------------------------------------------------------------*
*& READ TABLE statements
*&---------------------------------------------------------------------*

    READ TABLE itab INTO DATA(line) WHERE num > 20.
    ASSERT sy-tabix = 4.

    "The following statement issues a warning, which can be suppressed by a
    "pragma. For better performance, the WHERE clause should be replaced by
    "a key specification with a simple component or operand pair list.
    READ TABLE itab INTO line WHERE num = 0 ##read_where_ok.
    ASSERT sy-tabix = 1.

    READ TABLE itab INTO line WITH KEY num = 0.
    ASSERT sy-tabix = 1.


    READ TABLE itab INTO line WHERE num > 5 AND id  >= 3.
    ASSERT sy-tabix = 4.

    READ TABLE itab INTO line WHERE NOT num = 0.
    ASSERT sy-tabix = 2.

    READ TABLE itab INTO line WHERE ( id < 5 AND num > 10 ) AND text = `abc`.
    ASSERT sy-tabix = 2.

    "contains only
    READ TABLE itab INTO line WHERE text CO `abc`.
    ASSERT sy-tabix = 1.

    "contains not only
    READ TABLE itab INTO line WHERE text CN `abc`.
    ASSERT sy-tabix = 3.

    "contains string
    READ TABLE itab INTO line WHERE text CS `ef`.
    ASSERT sy-tabix = 3.

    "contains no string
    READ TABLE itab INTO line WHERE text NS `ef`.
    ASSERT sy-tabix = 1.

    "contains any
    READ TABLE itab INTO line WHERE text CA `xyzi`.
    ASSERT sy-tabix = 5.

    "contains not any
    READ TABLE itab INTO line WHERE text NA `a`.
    ASSERT sy-tabix = 3.

    "conforms to pattern
    READ TABLE itab INTO line WHERE text CP `*c`.
    ASSERT sy-tabix = 1.

    "does not conform to pattern
    READ TABLE itab INTO line WHERE text NA `*c`.
    ASSERT sy-tabix = 3.

    READ TABLE itab INTO line WHERE num BETWEEN 5 AND 45.
    ASSERT sy-tabix = 2.

    READ TABLE itab INTO line WHERE num NOT BETWEEN 5 AND 45.
    ASSERT sy-tabix = 1.

    rangestab = VALUE #( ( sign = `I` option = `BT` low = 5 high = 45 ) ).
    READ TABLE itab INTO line WHERE num IN rangestab.
    ASSERT sy-tabix = 2.

    "Same as above. Without the pragma, a warning is issued. It is
    "recommended that you specify a key and a component or operand
    "pair list.
    READ TABLE itab INTO line WHERE num IS INITIAL ##read_where_ok.
    ASSERT sy-tabix = 1.

    READ TABLE itab INTO line WHERE num IS NOT INITIAL.
    ASSERT sy-tabix = 2.

    READ TABLE itab INTO line WHERE ref IS BOUND.
    ASSERT sy-tabix = 1.

    READ TABLE itab INTO line WHERE ref IS NOT BOUND.
    ASSERT sy-tabix = 2.

    READ TABLE itab INTO line WHERE oref IS INSTANCE OF cl_system_uuid.
    ASSERT sy-tabix = 1.

    READ TABLE itab INTO line WHERE oref IS NOT INSTANCE OF cl_system_uuid.
    ASSERT sy-tabix = 2.

    dynamic_where_cond = `num > 20`.
    READ TABLE itab INTO line WHERE (dynamic_where_cond).
    ASSERT sy-tabix = 4.

    dynamic_where_cond_tab = VALUE string_table( ( `num > 20` )
                                                 ( `AND` )
                                                 ( `id > 4` ) ).
    READ TABLE itab INTO line WHERE (dynamic_where_cond_tab).
    ASSERT sy-tabix = 5.

*&---------------------------------------------------------------------*
*& FILTER constructor operator
*&---------------------------------------------------------------------*

    "Demo internal tables
    DATA fi_tab1 TYPE SORTED TABLE OF demo_struc WITH NON-UNIQUE KEY id.
    DATA fi_tab2 TYPE STANDARD TABLE OF demo_struc WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS id.
    DATA fi_tab3 TYPE HASHED TABLE OF demo_struc WITH UNIQUE KEY id.
    DATA fi_tab4 TYPE STANDARD TABLE OF demo_struc WITH UNIQUE HASHED KEY sec_hash_key COMPONENTS id text.

    fi_tab1 = itab.
    fi_tab2 = itab.
    fi_tab3 = itab.
    fi_tab4 = itab.

    "Filtering using single values
    "Using the primary table key without specifying USING KEY
    DATA(f1) = FILTER #( fi_tab1 WHERE id >= 4 ).

    "Using the primary table key by specifying the default name primary_key
    "and specifying USING KEY; the result in the example is the same as above
    DATA(f2) = FILTER #( fi_tab1 USING KEY primary_key WHERE id >= 4 ).

    "Using the secondary table key by specifying its name and USING KEY
    DATA(f3) = FILTER #( fi_tab2 USING KEY sec_key WHERE id < 3 ).

    "Note: When using a table with hash key, only the comparison operator =
    "can be used in the WHERE clause. The example uses the primary table key,
    "which is a hash key.
    DATA(f4) = FILTER #( fi_tab3 WHERE id = 3 ).

    "The example table used in the following example has two components specified
    "for the table key. The key must be specified in full listing all components
    "and using AND.
    "Note to use compatible types. Assume text is typed with type c length 10.
    "Then, the literal 'abc' would not be a type-compatible specification. You
    "can then use the CONV operator to convert the literal appropriately.
    DATA(f5) = FILTER #( fi_tab4 USING KEY sec_hash_key WHERE id = 2 AND text = `abc` ).

    "Examples with the EXCEPT addition
    DATA(f6) = FILTER #( fi_tab1 EXCEPT WHERE id >= 4 ).
    DATA(f7) = FILTER #( fi_tab1 EXCEPT USING KEY primary_key WHERE id >= 4 ).
    DATA(f8) = FILTER #( fi_tab2 EXCEPT USING KEY sec_key WHERE id >= 4 ).
    DATA(f9) = FILTER #( fi_tab4 EXCEPT USING KEY sec_hash_key WHERE id = 3 AND text = `def` ).

    "IS [NOT] INITIAL
    DATA(f10) = FILTER #( fi_tab1 WHERE id IS INITIAL ).
    DATA(f11) = FILTER #( fi_tab1 WHERE id IS NOT INITIAL ).

    "Filtering using a filter table

    "In the WHERE condition, the columns of source and filter table are compared.
    "Those lines in the source table are used for which at least one line in the
    "filter table meets the condition. EXCEPT and USING KEY are also possible.
    "The following examples use simple tables with elementary line types. Note
    "that the line types of the tables in a FILTER epxression need not be identical.

    "Declaring and filling filter tables
    DATA filter_tab1 TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.

    DATA filter_tab2 TYPE STANDARD TABLE OF i
        WITH EMPTY KEY
        WITH UNIQUE SORTED KEY line COMPONENTS table_line.

    filter_tab1 = VALUE #( ( 3 ) ( 5 ) ).
    filter_tab2 = filter_tab1.

    "No further additions specified
    DATA(f12) = FILTER #( fi_tab1 IN filter_tab1 WHERE id = table_line ).
    "Specifying EXCEPT addition
    DATA(f13) = FILTER #( fi_tab1 EXCEPT IN filter_tab1 WHERE id = table_line ).
    "Specifying USING KEY for the filter table
    DATA(f14) = FILTER #( fi_tab2 IN filter_tab2 USING KEY line WHERE id = table_line ).
    "Specifying USING KEY for the source table, including EXCEPT
    DATA(f15) = FILTER #( fi_tab2 USING KEY sec_key EXCEPT IN filter_tab2 WHERE id = table_line ).

*&---------------------------------------------------------------------*
*& Table comprehensions and reductions with VALUE ... FOR/REDUCE and
*& specifiying a WHERE condition
*&---------------------------------------------------------------------*

    TYPES int_tab_type TYPE TABLE OF i WITH EMPTY KEY.

    "Note: In the final pair of parentheses, the components are specified ( a = ... b = ... c = ... ).
    "Since the example uses a table type with an elementary table type, there is no component name
    "to be specified. So, the component value is assigned through the work area directly.

    "Table comprehensions with VALUE ... FOR
    DATA(it1) = VALUE int_tab_type( FOR w IN itab WHERE ( num = 0 ) ( w-id ) ).
    DATA(it2) = VALUE int_tab_type( FOR w IN itab WHERE ( num > 20 ) ( w-id ) ).
    DATA(it3) = VALUE int_tab_type( FOR w IN itab WHERE ( num > 5 AND id  >= 3 ) ( w-id ) ).
    DATA(it4) = VALUE int_tab_type( FOR w IN itab WHERE ( num >= 40 OR num = 0 ) ( w-id ) ).
    DATA(it5) = VALUE int_tab_type( FOR w IN itab WHERE ( NOT num = 0 ) ( w-id ) ).
    DATA(it6) = VALUE int_tab_type( FOR w IN itab WHERE ( ( id < 5 AND num > 10 ) AND text = `abc` ) ( w-id ) ).
    DATA(it7) = VALUE int_tab_type( FOR w IN itab WHERE ( text CO `abc` ) ( w-id ) ).
    DATA(it8) = VALUE int_tab_type( FOR w IN itab WHERE ( text CN `abc` ) ( w-id ) ).
    DATA(it9) = VALUE int_tab_type( FOR w IN itab WHERE ( text CS `ef` ) ( w-id ) ).
    DATA(it10) = VALUE int_tab_type( FOR w IN itab WHERE ( text NS `ef` ) ( w-id ) ).
    DATA(it11) = VALUE int_tab_type( FOR w IN itab WHERE ( text CA `xyzi` ) ( w-id ) ).
    DATA(it12) = VALUE int_tab_type( FOR w IN itab WHERE ( text NA `a` ) ( w-id ) ).
    DATA(it13) = VALUE int_tab_type( FOR w IN itab WHERE ( text CP `*c` ) ( w-id ) ).
    DATA(it14) = VALUE int_tab_type( FOR w IN itab WHERE ( text NA `*c` ) ( w-id ) ).
    DATA(it15) = VALUE int_tab_type( FOR w IN itab WHERE ( num BETWEEN 5 AND 45 ) ( w-id ) ).
    DATA(it16) = VALUE int_tab_type( FOR w IN itab WHERE ( num NOT BETWEEN 5 AND 45 ) ( w-id ) ).
    rangestab = VALUE #( ( sign = `I` option = `BT` low = 5 high = 45 ) ).
    DATA(it17) = VALUE int_tab_type( FOR w IN itab WHERE ( num IN rangestab ) ( w-id ) ).
    DATA(it18) = VALUE int_tab_type( FOR w IN itab WHERE ( num IS INITIAL ) ( w-id ) ).
    DATA(it19) = VALUE int_tab_type( FOR w IN itab WHERE ( num IS NOT INITIAL ) ( w-id ) ).
    DATA(it20) = VALUE int_tab_type( FOR w IN itab WHERE ( ref IS BOUND ) ( w-id ) ).
    DATA(it21) = VALUE int_tab_type( FOR w IN itab WHERE ( ref IS NOT BOUND ) ( w-id ) ).
    DATA(it22) = VALUE int_tab_type( FOR w IN itab WHERE ( oref IS INSTANCE OF cl_system_uuid ) ( w-id ) ).
    DATA(it23) = VALUE int_tab_type( FOR w IN itab WHERE ( oref IS NOT INSTANCE OF cl_system_uuid ) ( w-id ) ).
    dynamic_where_cond = `num > 20`.
    DATA(it24) = VALUE int_tab_type( FOR w IN itab WHERE (dynamic_where_cond) ( w-id ) ).
    dynamic_where_cond_tab = VALUE string_table( ( `num > 20` )
                                                 ( `AND` )
                                                 ( `id > 4` ) ).
    DATA(it25) = VALUE int_tab_type( FOR w IN itab WHERE ( (dynamic_where_cond_tab) ) ( w-id ) ).

    "Table reductions with REDUCE
    DATA(red1) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                      FOR r IN itab
                                      WHERE ( num = 0 )
                                      NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red2) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                      FOR r IN itab
                                      WHERE ( num > 20 )
                                      NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red3) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                      FOR r IN itab
                                      WHERE ( num > 5 AND id  >= 3 )
                                      NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red4) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                      FOR r IN itab
                                      WHERE ( num >= 40 OR num = 0 )
                                      NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red5) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                      FOR r IN itab
                                      WHERE ( NOT num = 0 )
                                      NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red6) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                      FOR r IN itab
                                      WHERE ( ( id < 5 AND num > 10 ) AND text = `abc` )
                                      NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red7) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                      FOR r IN itab
                                      WHERE ( text CO `abc` )
                                      NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red8) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                      FOR r IN itab
                                      WHERE ( text CN `abc` )
                                      NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red9) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                      FOR r IN itab
                                      WHERE ( text CS `ef` )
                                      NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red10) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( text NS `ef` )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red11) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( text CA `xyzi` )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red12) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( text NA `a` )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red13) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( text CP `*c` )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red14) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( text NA `*c` )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red15) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( num BETWEEN 5 AND 45 )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red16) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( num NOT BETWEEN 5 AND 45 )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    rangestab = VALUE #( ( sign = `I` option = `BT` low = 5 high = 45 ) ).
    DATA(red17) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( num IN rangestab )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red18) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( num IS INITIAL )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red19) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( num IS NOT INITIAL )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red20) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( ref IS BOUND )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red21) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( ref IS NOT BOUND )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red22) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( oref IS INSTANCE OF cl_system_uuid )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    DATA(red23) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( oref IS NOT INSTANCE OF cl_system_uuid )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    dynamic_where_cond = `num > 20`.
    DATA(red24) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE (dynamic_where_cond)
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

    dynamic_where_cond_tab = VALUE string_table( ( `num > 20` )
                                                 ( `AND` )
                                                 ( `id > 4` ) ).
    DATA(red25) = REDUCE int_tab_type( INIT tab = VALUE #( )
                                       FOR r IN itab
                                       WHERE ( (dynamic_where_cond_tab) )
                                       NEXT tab = VALUE #( BASE tab ( r-id ) ) ).

  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>