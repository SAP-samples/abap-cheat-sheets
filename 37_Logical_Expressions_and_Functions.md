<a name="top"></a>

# Logical Expressions and Functions

- [Logical Expressions and Functions](#logical-expressions-and-functions)
  - [Introduction](#introduction)
  - [Comparison Expressions with Comparison Operators](#comparison-expressions-with-comparison-operators)
    - [General Comparison Operators](#general-comparison-operators)
    - [Comparison Operators for Character-Like Data Types](#comparison-operators-for-character-like-data-types)
  - [Predicates](#predicates)
    - [Predicate Expressions](#predicate-expressions)
    - [Predicate Functions](#predicate-functions)
    - [Predicative Method Calls, Data Objects as Relational Expressions](#predicative-method-calls-data-objects-as-relational-expressions)
  - [Boolean Operators and Parentheses](#boolean-operators-and-parentheses)
  - [Boolean Functions](#boolean-functions)
  - [Excursion: Operands of Logical Expressions](#excursion-operands-of-logical-expressions)


This ABAP cheat sheet covers logical expressions and functions that are used to process data based on specific conditions and to control program flow.

> [!NOTE]
> Many topics covered here are also included in other ABAP cheat sheets. This ABAP cheat sheet is to summarize.

## Introduction

Logical expressions:
- Formulate conditions for operands in ABAP statements, as well as in ABAP CDS and SQL (such as `WHERE` and `ON` conditions, which are not covered here).
- Represent relational expressions, that is, expressions which result in a truth value (true or false). These include comparison expressions and predicates.
- Consist of a single relational expression or multiple relational expressions combined with Boolean operators (such as `AND` or `OR`).
- In ABAP, they are typically used in control statements that define a control structure, such as `IF`, executed based on conditions. They can also be used in other contexts, like conditional expressions with `COND` and `SWITCH`, to define `WHERE` conditions, or as arguments in Boolean functions like `xsdbool`.
- Can involve various operands, including data objects, built-in functions, functional methods (methods with a returning parameter), and calculation, constructor, or table expressions.

Logical functions:
- Are built-in functions.
- Include Boolean and predicate functions that return a truth value.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Comparison Expressions with Comparison Operators

- Comparison expressions are relational expressions whose result is a truth value.
- They can combine two or more operands using comparison operators like `=`.
- The comparison follows specific rules. 

> [!TIP]
> Find more details on the comparison rules [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_rules.htm).

> [!NOTE]
> ABAP includes special comparison operators for byte-like data types and bit patterns, which are not covered here. For more information, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENLOGEXP_BYTES.html).

<p align="right"><a href="#top">⬆️ back to top</a></p>

### General Comparison Operators

The following comparison operators can be used for all data types: 

<table>
<tr>
<th>Comparison Operator</th>
<th>Details</th>
<th>Example</th>
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
DATA(a) = 1.
DATA(b) = 1.

IF a = b.
  ...
ELSE.
  ...
ENDIF.

IF a EQ b.
  ...
ELSE.
  ...
ENDIF.

ASSERT a = b.
ASSERT a EQ b.
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
DATA(c) = 1.
DATA(d) = 2.

IF c <> d.
  ...
ELSE.
  ...
ENDIF.

IF c NE d.
  ...
ELSE.
  ...
ENDIF.

ASSERT c <> d.
ASSERT c NE d.
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
DATA(e) = 1.
DATA(f) = 3.

IF e < f.
  ...
ELSE.
  ...
ENDIF.

IF e LT f.
  ...
ELSE.
  ...
ENDIF.

ASSERT e < f.
ASSERT e LT f.
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
DATA(g) = 7.
DATA(h) = 2.

IF g > h.
  ...
ELSE.
  ...
ENDIF.

IF g GT h.
  ...
ELSE.
  ...
ENDIF.

ASSERT g > h.
ASSERT g GT h.
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
DATA(i) = 5.
DATA(j) = 5.

IF i <= j.
  ...
ELSE.
  ...
ENDIF.

IF i LE j.
  ...
ELSE.
  ...
ENDIF.

ASSERT i <= j.
ASSERT i LE j.
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
DATA(k) = 8.
DATA(l) = 7.

IF k >= l.
  ...
ELSE.
  ...
ENDIF.

IF k GE l.
  ...
ELSE.
  ...
ENDIF.

ASSERT k >= l.
ASSERT k GE l.
``` 

 </td>
</tr>

<tr>
<td> 

`... a [NOT] BETWEEN b AND c ...`

 </td>

 <td> 

Ternary comparison operator<br><br>
The content of the left operand is (not) between the value of the two other operands. The syntax variant is like specifying `... [NOT] ( a >= b AND a <= c ) ...`.

 </td>

 <td> 

``` abap
DATA(m) = 2.
DATA(n) = 1.
DATA(o) = 3.

IF m BETWEEN n AND o.
  ...
ELSE.
  ...
ENDIF.

IF m NOT BETWEEN n AND o.
  ...
ELSE.
  ...
ENDIF.

ASSERT m BETWEEN n AND o.
ASSERT o NOT BETWEEN m AND n.   
``` 

 </td>
</tr>

<tr>
<td> 

`... a [NOT] IN @ranges_table ...`

 </td>

 <td> 

Tabular comparison operator<br><br>
Checks whether the operand on the left side  of `IN` matches (or does not match) [ranges conditions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenranges_condition_glosry.htm) specified in a [ranges table](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenranges_table_glosry.htm). Note that the operators `CP` and `NP` are transformed into `LIKE` conditions (as a consequence, the conditions for `CP` and `NP` are case-sensitive). 

 </td>

 <td> 

``` abap
DATA int_tab TYPE TABLE OF i WITH EMPTY KEY.
int_tab = VALUE #( FOR x = 1 WHILE x <= 20 ( x ) ).

DATA rangestab TYPE RANGE OF i.
"Value range between 1 and 10
rangestab = VALUE #( ( sign = 'I' option = 'BT' low = 1 high = 10 ) ).

SELECT * FROM @int_tab AS tab
  WHERE table_line IN @rangestab
  INTO TABLE @DATA(result_tab).

ASSERT REDUCE string( INIT string = ``
                      FOR <line> IN result_tab
                      NEXT string &&= <line> && `|` ) = `1|2|3|4|5|6|7|8|9|10|`.

"Value range: Lower than 5 + greater than or equal to 25
rangestab = VALUE #( ( sign = 'I' option = 'LT' low = 5 )
                     ( sign = 'I' option = 'GE' low = 15  ) ).

SELECT * FROM @int_tab AS tab
  WHERE table_line IN @rangestab
  INTO TABLE @result_tab.

ASSERT REDUCE string( INIT string = ``
                      FOR <line> IN result_tab
                      NEXT string &&= <line> && `|` ) = `1|2|3|4|15|16|17|18|19|20|`.
``` 

 </td>
</tr>

</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Comparison Operators for Character-Like Data Types

<table>
<tr>
<th>Comparison Operator</th>
<th>Details</th>
<th>Example</th>
</tr>
<tr>
    <td><code>CA</code></td>
<td><i>Contains any</i><br><br>To determine whether any character of a given character set is contained
in a string. <br>Note: The search is case-sensitive. <code>sy-fdpos</code> contains the offset of the first character found, while 0 stands for the very first position. If nothing is found, <code>sy-fdpos</code> contains the length of the string.</td>
<td>

```abap
DATA(s1) = `cheers`.
IF s1 CA `aeiou`. ... "true; sy-fdpos: 2
IF s1 CA `xy`. ...    "false; sy-fdpos: 6
```    

</td>
</tr>
<tr>
<td><code>NA</code></td>
<td><i>Contains not any</i><br><br>To determine whether any character of a given character set is not contained
in a string. See the note above. </td>
<td>

```abap
DATA(s2) = `Hallo`.
IF s2 NA `bcdeh`. ... "true; sy-fdpos: 5
IF s2 NA `bcdeH`. ... "false; sy-fdpos: 0
```   

</td>
</tr>
<tr>
<td><code>CO</code></td>  
<td><i>Contains only</i><br><br>To determine whether a string contains only a certain set of characters. See the note above.</td>
<td>

```abap
DATA(s3) = `abcd`.
IF s3 CO `abcd`. ... "true; sy-fdpos: 4
IF s3 CO `abCd`. ... "false; sy-fdpos: 2
```   

</td>
</tr>
<tr>
<td><code>CN</code></td>
<td><i>Contains not only</i><br><br>To determine whether a string does not only contain a certain set of characters, i.e. whether a string contains characters other than those in the character set. See the note above.</td>
<td>

```abap
DATA(s4) = `abap`.
IF s4 CN `ab`. ...  "true; sy-fdpos: 3
IF s4 CN `abp`. ... "false; sy-fdpos: 4
```   

</td>
</tr>
<tr>
<td><code>CS</code></td>
<td><i>Contains string</i><br><br>For simple substring searches and determining whether a string contains a substring. <br>Note: The search is not case-sensitive. <code>sy-fdpos</code> contains the offset of the first substring found. If it is not found, <code>sy-fdpos</code> contains the length of the string searched.</td>
<td>

```abap
DATA(s5) = `Lorem ipsum dolor sit amet.`.
IF s5 CS `or`. ... "true; sy-fdpos: 1
IF s5 CS `zz`. ... "false; sy-fdpos: 27
```   

</td>
</tr>
<tr>
<td><code>NS</code></td>
<td><i>Contains no string</i><br><br>To determine whether a substring is not contained in a string. See the note for <code>CS</code>.</td>
<td>

```abap
DATA(s6) = `some test string`.
IF s6 NS `tests`. ... "true; sy-fdpos: 16
IF s6 NS `TEST`. ...  "false; sy-fdpos: 5
```   

</td>
</tr>
<tr>
<td><code>CP</code></td>
<td><i>Conforms to pattern</i><br><br>For simple pattern searches and determining whether a set of characters is contained in a string that matches a particular pattern. You can use the following special characters as patterns: <ul><li><code>*</code>: Any character sequence (including blanks)</li><li><code>+</code>: Any character (only one character, including blanks)</li><li><code>#</code>: Escape character. The following character is marked for an exact comparison.</li></ul>Patterns are not case-sensitive except for characters marked with
<code>#</code>. If a pattern is found, <code>sy-fdpos</code> returns the offset of the first occurrence. Otherwise, it contains the length of the string searched.
</td>
<td>

```abap
DATA(s7) = `abc_def_ghi`.

"Pattern: f is preceded by any character sequence, must be followed
"by '_' and then followed by any character sequence
IF s7 CP `*f#_*`. ... "true; sy-fdpos: 6

"Pattern: i is preceded by any character sequence, must be followed
"by any character or a blank
IF s7 CP `*i+`. ... "false; sy-fdpos: 11    
```   

</td>
</tr>
    <tr>
<td><code>NP</code></td>
<td><i>Does not conform to pattern</i><br><br>Negation of <code>CP</code>. See the previous notes.</td>
<td>

```abap
DATA(s8) = `abcDEFghi`.

"Pattern: c is preceded by any character sequence, must be followed
"by a small letter d, and then followed by any character sequence
IF s8 NP `*c#d*`. ... "true; sy-fdpos: 9   

"Pattern: c is preceded by any character sequence, must be followed
"by a capital letter D, and then followed by any character sequence
IF s8 NP `*c#D*`. ... "false; sy-fdpos: 2    
```   

</td>
</tr>
</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Predicates

Predicates are relational expressions that determine a truth value based on an operand. In ABAP, you can find these predicates in predicate expressions, predicate functions, and predicative method calls.

### Predicate Expressions

- Predicate expressions are relational expressions with the predicate operator `IS`. 
- The addition `NOT` (`IS NOT`) negates.

<table>
<tr>
<th>Predicate Expression</th>
<th>Details</th>
<th>Example</th>
</tr>

<tr>
<td> 

`IS [NOT] INITIAL` 

 </td>

 <td> 

- Checks whether the operand is initial or not. 
- The result is true if the operand contains its type-dependent initial value.
  
<br>

Note that you can also specify an individual data object that represents a relational expression, returning a truth value. The expression evaluates to true if the operand's content is not initial.

 </td>

 <td> 

``` abap
DATA(p) = 0.

IF p IS INITIAL.
  ...
ELSE.
  ...
ENDIF.

IF p IS NOT INITIAL.
  ...
ELSE.
  ...
ENDIF.

ASSERT p IS INITIAL.
p = 1.
ASSERT p IS NOT INITIAL.
``` 

 </td>
</tr>


<tr>
<td> 

`IS [NOT] BOUND` 

 </td>

 <td> 


- Checks if a reference variable contains a valid reference. 
- A data reference is valid if it can be dereferenced. 
- An object reference is valid if it points to an object.


 </td>

 <td> 

``` abap
"Data reference variable
DATA(dref) = NEW string( `ABAP` ).

IF dref IS BOUND.
  DATA(some_string) = dref->*.
ELSE.
  ...
ENDIF.

ASSERT dref IS BOUND.

CLEAR dref.

IF dref IS NOT BOUND.
  ...
ELSE.
  ...
ENDIF.

ASSERT dref IS NOT BOUND.

"Object reference variable
DATA oref TYPE REF TO cl_abap_zip.

IF oref IS NOT BOUND.
  ...
ELSE.
  ...
ENDIF.

ASSERT oref IS NOT BOUND.

oref = NEW #( ).

IF oref IS BOUND.
  ...
ELSE.
  ...
ENDIF.

ASSERT oref IS BOUND.
``` 

 </td>
</tr>

<tr>
<td> 

`IS [NOT] INSTANCE OF` 

 </td>

 <td> 


- Checks whether casts are possible.
- It returns true if a downcast is possible for a non-initial object reference variable. 
- This is especially useful for verifying types in generically typed field symbols or formal parameters and helps prevent runtime errors from failed casts, rather than applying exception handling.

 </td>

 <td> 

The following example uses local classes and interfaces to demonstrate predicate expressions.

**Global class**

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

    DATA(oref1) = NEW lcl1( ).
    DATA(oref2) = NEW lcl2( ).
    DATA(oref3) = NEW lcl3( ).
    DATA iref TYPE REF TO lif.
    iref = NEW lcl3( ).

    DATA oref1b TYPE REF TO lcl1.
    IF oref1 IS INSTANCE OF lcl1.
      oref1b = oref1.
    ELSE.
      ...
    ENDIF.

    ASSERT oref1 IS INSTANCE OF lcl1.

    ASSERT oref2 IS INSTANCE OF lcl1.
    ASSERT oref2 IS INSTANCE OF lcl2.

    oref1 = NEW lcl2( ).

    IF oref1 IS INSTANCE OF lcl2.
      oref2 = CAST #( oref1 ).
    ELSE.
      ...
    ENDIF.

    ASSERT oref3 IS INSTANCE OF lcl3.
    ASSERT iref IS INSTANCE OF lcl3.

    IF iref IS INSTANCE OF lcl3.
      iref = CAST #( oref3 ).
    ELSE.
      ...
    ENDIF.

    DATA oref_generic TYPE REF TO object.
    oref_generic = NEW lcl4( ).
    DATA flag TYPE abap_boolean.
    IF oref_generic IS NOT INSTANCE OF lcl1.
      flag = abap_false.
    ELSE.
      flag = abap_true.
    ENDIF.

    ASSERT flag = abap_false.

    DATA(oref4) = NEW lcl4( ).

    DATA(result) = oref4->meth( oref4 ).
    ASSERT result = abap_true.
    result = oref4->meth( oref3 ).
    ASSERT result = abap_false.

  ENDMETHOD.
ENDCLASS.
``` 

**CCIMP include (Local types tab)**

```abap
CLASS lcl1 DEFINITION.
ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
ENDCLASS.

INTERFACE lif.
ENDINTERFACE.

CLASS lcl3 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl4 DEFINITION.
  PUBLIC SECTION.
    METHODS meth IMPORTING oref          TYPE REF TO object
                 RETURNING VALUE(result) TYPE abap_boolean.
ENDCLASS.

CLASS lcl4 IMPLEMENTATION.
  METHOD meth.
    IF oref IS INSTANCE OF lcl4.
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

 </td>
</tr>

<tr>
<td> 

`IS [NOT] ASSIGNED` 

 </td>

 <td> 

Checks whether a memory area is assigned to a field symbol or not.

 </td>

 <td> 

``` abap
FIELD-SYMBOLS <fs> TYPE string.
DATA(some_str) = `hello`.
ASSIGN some_str TO <fs>.

IF <fs> IS ASSIGNED.
  ...
ELSE.
  ...
ENDIF.

ASSERT <fs> IS ASSIGNED.

UNASSIGN <fs>.

IF <fs> IS NOT ASSIGNED.
  ...
ELSE.
  ...
ENDIF.

ASSERT <fs> IS NOT ASSIGNED.

FIELD-SYMBOLS <another_fs> TYPE i.
DATA num TYPE i VALUE 1.
ASSERT <another_fs> IS NOT ASSIGNED.
ASSIGN num TO <another_fs>.
ASSERT <another_fs> IS ASSIGNED.
"The following statement only initializes the value and does
"not remove the assignment.
CLEAR <another_fs>.
ASSERT <another_fs> IS ASSIGNED.
ASSERT <another_fs> = 0.
ASSERT num = 0.
``` 

 </td>
</tr>

<tr>
<td> 

`IS [NOT] SUPPLIED` 

 </td>

 <td> 

- Checks whether an optional formal parameter of a procedure is filled or requested. 
- The expression returns true if an actual parameter was assigned to the formal parameter during the call. 
- These expressions can only be used in function modules and methods.

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS meth IMPORTING num1      TYPE i
                                 num2      TYPE i OPTIONAL
                                 num3      TYPE i DEFAULT 9
                       EXPORTING str       TYPE string
                                 str_table TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    meth(
      EXPORTING
        num1      = 1
      IMPORTING
        str       = DATA(str1)
        str_table = DATA(str_table1)
    ).

    meth(
      EXPORTING
        num1      = 2
        num2      = 3
      IMPORTING
        str       = DATA(str2)
        str_table = DATA(str_table2)
    ).

    meth(
      EXPORTING
        num1      = 4
        num3      = 5
      IMPORTING
        str       = DATA(str3)
        str_table = DATA(str_table3)
    ).

    meth(
      EXPORTING
        num1      = 6
        num2      = 7
        num3      = 8
      IMPORTING
        str       = DATA(str4)
        str_table = DATA(str_table4)
    ).

    ASSERT str1 = `109`.
    ASSERT str2 = `239`.
    ASSERT str3 = `405`.
    ASSERT str4 = `678`.

  ENDMETHOD.

  METHOD meth.
    APPEND |num1: "{ num1 }"| TO str_table.
    str &&= num1.

    IF num2 IS SUPPLIED.
      APPEND |num2 (is supplied): "{ num2 }"| TO str_table.
    ELSE.
      APPEND |num2 (is not supplied; initial value): "{ num2 }"| TO str_table.
    ENDIF.

    str &&= num2.

    IF num3 IS NOT SUPPLIED.
      APPEND |num3 (is not supplied; default value): "{ num3 }"| TO str_table.
    ELSE.
      APPEND |num3 (is supplied): "{ num3 }"| TO str_table.
    ENDIF.

    str &&= num3.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Predicate Functions

- Predicate functions are built-in logical functions that return a truth value.  
- ABAP supports predicate functions for character-like and table-like arguments.

<table>
<tr>
<th>Predicate Functions</th>
<th>Details</th>
<th>Example</th>
</tr>

<tr>
<td> 

<code>contains</code><br><code>contains_any_of</code><br><code>contains_any_not_of</code>

 </td>

 <td> 

<ul>
 <li><code>contains</code>
 <ul>
 <li>For checking a text string based on (optional) parameters</li>
 <li><code>val</code>: Text string to be searched</li>
 <li><code>sub</code>/<code>start</code>/<code>end</code>: Specifying a substring to be searched in <code>val</code></li>
 <li><code>off</code>/<code>len</code>: Specifying the search range</li>
  <li><code>case</code>: Specifying the case-sensitivity (the search is case-sensistive by default)</li>
 <li><code>pcre</code>: Specifying a regular expression</li>
  </ul>
 </li>
 <li><code>contains_any_of</code>/<code>contains_any_not_of</code>
  <ul>
  <li>The functions only check individual characters passed to <code>sub</code>/<code>start</code>/<code>end</code> (or, in case of the negation, any characters that are not contained).</li>
 <li>The <code>pcre</code> parameter is not available.</li>  
  </ul>
 </li> 
</ul>
 </ul>

 </td>

 <td> 

``` abap
"-------------------- contains --------------------
"Specifying the minimum mandatory parameters
"Unlike most of the following examples, this one uses an IF control structure to 
"visualize the truth value.
DATA cont1 TYPE abap_bool.
"abap_true
IF contains( val = `abdefghijklmn` sub = `ghi` ).
  cont1 = abap_true.
ELSE.
  cont1 = abap_false.
ENDIF.

"case (abap_true is the default)
"abap_false
DATA(cont2) = xsdbool( contains( val = `ABCDE` start = `ab` case = abap_true ) ). 

"abap_true
DATA(cont3) = xsdbool( contains( val = `ABCDE` start = `ab` case = abap_false ) ). 

"end
"abap_true
DATA(cont4) = xsdbool( contains( val = `UVWXYZ` end = `xyz` case = abap_false ) ). 

"start
"abap_false
DATA(cont5) = xsdbool( contains( val = `123` start = `2` ) ). 

"off/len can also be specified individually
"Not specifying off means 0 by default
"abap_false
DATA(cont6) = xsdbool( contains( val = `##ab## ##cd##` sub = `cd` len = 5 ) ).

"abap_true
DATA(cont7) = xsdbool( contains( val = `##ab## ##cd##` sub = `cd` off = 7 len = 5 ) ). 

"occ: False if there are more occurrences than specified for occ; i.e. in the following
"example, specifying the values 1, 2, 3 returns true
"abap_true is returned for the first 3 loop passes, abap_false for the fourth
DO 4 TIMES.
  DATA(cont8) = xsdbool( contains( val = `ab#ab#ab#cd#ef#gh` sub = `ab` occ = sy-index ) ).
ENDDO.

"pcre
"In the example, a blank is searched.
"abap_true
DATA(cont9) = xsdbool( contains( val = `Hallo world` pcre = `\s` ) ). 

"-------------------- contains_any_of --------------------
"abap_true
DATA(cont10) = xsdbool( contains_any_of( val = `abcdefg` sub = `xyza` ) ).

"abap_false
DATA(cont11) = xsdbool( contains_any_of( val = `abcdefg` sub = `xyz` ) ). 

DATA(hi) = `1hallo`.
DATA(abc) = `abcdefghijklmnopqrstuvwxyz`.
"abap_false
DATA(cont12) = xsdbool( contains_any_of( val = hi start = abc ) ).

"abap_true
DATA(cont13) = xsdbool( contains_any_of( val = hi end = abc ) ). 

"-------------------- contains_any_not_of --------------------
"abap_true
DATA(cont14) = xsdbool( contains_any_not_of( val = hi start = abc ) ).

"abap_false
DATA(cont15) = xsdbool( contains_any_not_of( val = hi end = abc ) ).
``` 

 </td>
</tr>

<tr>
<td> 

<code>matches</code> 

 </td>

 <td> 

- Compares a search range of a value with a regular expression. 
- More optional parameters are available (e.g. <code>case</code>, <code>off</code>, <code>len</code>).

 </td>

 <td> 

``` abap
"Checking validity of an email address
"abap_true
DATA(matches) = xsdbool( matches( val  = `jon.doe@email.com`
                                  pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ) ). 
``` 

 </td>
</tr>

<tr>
<td> 

<code>line_exists</code>

 </td>

 <td> 

- Checks whether a line exists in an internal table. 
- The functions expects a table expression to be specified.

 </td>

 <td> 

``` abap
TYPES: BEGIN OF s,
          comp1 TYPE i,
          comp2 TYPE c LENGTH 3,
        END OF s.
DATA itab TYPE TABLE OF s WITH EMPTY KEY.
itab = VALUE #( ( comp1 = 1 comp2 = 'aaa' ) ( comp1 = 2 comp2 = 'bbb' ) ( comp1 = 3 comp2 = 'ccc' ) ).
DATA(str_tab) = VALUE string_table( ( `abc` ) ( `def` ) ( `ghi` ) ).

IF line_exists( itab[ 1 ] ).
  ...
ELSE.
  ...
ENDIF.

"abap_true
DATA(line_exists1) = xsdbool( line_exists( itab[ 1 ] ) ). 

"abap_false
DATA(line_exists2) = xsdbool( line_exists( itab[ 4 ] ) ). 

"abap_true
DATA(line_exists3) = xsdbool( line_exists( itab[ comp1 = 2 ] ) ). 

"abap_true
DATA(line_exists4) = xsdbool( line_exists( str_tab[ 2 ] ) ). 

"abap_false
DATA(line_exists5) = xsdbool( line_exists( str_tab[ table_line = `xxx` ] ) ).
``` 

 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Predicative Method Calls, Data Objects as Relational Expressions

<table>
<tr>
<th>Context</th>
<th>Details</th>
<th>Example</th>
</tr>

<tr>
<td> 

Predicative method call <br> `... meth( ... ) ...` 

 </td>

 <td> 

- A predicative method call represents a relational expression. 
- Its only operand is a functional method call, which is a method with a return value passed via the returning parameter.
- The expression evaluates to true if the method's return value is not initial. A check is performed for the type-dependent initial value.

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS check_num_greater_than_10 IMPORTING num           TYPE numeric
                                            RETURNING VALUE(result) TYPE abap_boolean.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA is_greater TYPE abap_boolean.

    IF check_num_greater_than_10( CONV decfloat34( '9.9999999999' ) ).
      is_greater = abap_true.
    ELSE.
      is_greater = abap_false.
    ENDIF.

    ASSERT is_greater = abap_false.

    IF check_num_greater_than_10( CONV decfloat34( '10.000000001' ) ).
      is_greater = abap_true.
    ELSE.
      is_greater = abap_false.
    ENDIF.

    ASSERT is_greater = abap_true.

  ENDMETHOD.

  METHOD check_num_greater_than_10.
    IF num > 10.
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

Data object as relational expression <br>`... dobj ...` 

 </td>

 <td> 

- Individual data objects can represent relational expressions.  
- Like predicative method calls, the expression evaluates to true when the operand's content is not initial and false when it is initial. 
- The operand's concrete type is arbitrary since it checks for type-specific initial values.  
- Specifying only the data object is a short form of the predicate expression `... IS NOT INITIAL ...`.

 </td>

 <td> 

``` abap
DATA is_initial TYPE abap_boolean.
DATA(number) = 0.

IF number.
  is_initial = abap_false.
ELSE.
  is_initial = abap_true.
ENDIF.

ASSERT is_initial = abap_true.
ASSERT is_initial.
ASSERT NOT number.

number = 1.

IF number.
  is_initial = abap_false.
ELSE.
  is_initial = abap_true.
ENDIF.

ASSERT is_initial = abap_false.
ASSERT NOT is_initial.
ASSERT number.
``` 

 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>



## Boolean Operators and Parentheses

- Boolean operators allow to combine (`AND`, `OR`, `EQUIV`) and negate (`NOT`) logical expressions.
- You can use parentheses to refine conditions further.

<table>
<tr>
<th>Operator</th>
<th>Details</th>
<th>Example</th>
</tr>

<tr>
<td> 

`NOT` 

 </td>

 <td> 

- Negates a logical expression.
- Should not be confused with `NOT` additions in contexts such as the predicate operator `IS`. Possible syntax: `... IF NOT 1 IS NOT INITIAL. ...` (Here, 1 is evaluated as not initial, resulting in true. However, because of the Boolean operator, the logical expression is negated.)

 </td>

 <td> 

``` abap
DATA is_found TYPE abap_boolean.
FIND `C` IN `ABAP`.

IF NOT sy-subrc = 0.
  is_found = abap_false.
ELSE.
  is_found = abap_true.
ENDIF.

ASSERT is_found = abap_false.
``` 

 </td>
</tr>

<tr>
<td> 

`AND` 

 </td>

 <td> 

- Combines multiple logical expressions.
- If any of the logical expressions are false, the combined expression is false.

 </td>

 <td> 

``` abap
DATA(num1) = 1.
DATA(num2) = 1.
DATA(num3) = 1.
DATA result TYPE abap_boolean.

IF num1 IS NOT INITIAL AND num2 IS NOT INITIAL.
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_true.

IF num1 IS NOT INITIAL AND num2 IS NOT INITIAL AND num3 IS NOT INITIAL.
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_true.

num3 = 0.
IF num1 IS NOT INITIAL AND num2 IS NOT INITIAL AND num3 IS NOT INITIAL.
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_false.
``` 

 </td>
</tr>

<tr>
<td> 

`OR` 

 </td>

 <td> 

- Combines multiple logical expressions.
- The combined expression is true if at least one of the logical expressions is true. 
- It is false only if all logical expressions are false.

 </td>

 <td> 

``` abap
DATA(num1) = 1.
DATA(num2) = 1.
DATA(num3) = 1.
DATA result TYPE abap_boolean.

"2/2 logical expressions are true
IF num1 IS NOT INITIAL OR num2 IS NOT INITIAL.
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_true.

"3/3 logical expressions are true
IF num1 IS NOT INITIAL OR num2 IS NOT INITIAL OR num3 IS NOT INITIAL.
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_true.

"1/3 logical expressions are true
IF num1 IS NOT INITIAL OR num2 IS INITIAL OR num3 IS INITIAL.
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_true.

"0/3 logical expressions are true
IF num1 IS INITIAL OR num2 IS INITIAL OR num3 IS INITIAL.
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_false.
``` 

 </td>
</tr>

<tr>
<td> 

`EQUIV` 

 </td>

 <td> 

- You can combine two logical expressions with `EQUIV` to create a new logical expression.  
- This expression is true if both expressions are either true or false. If one expression differs from the other, the combined expression is false.
    

 </td>

 <td> 

``` abap
DATA(num1) = 1.
DATA(num2) = 1.
DATA(num3) = 1.
DATA(num4) = 1.
DATA result TYPE abap_boolean.

"The combined expression is true if a/b and c/d are either both equal
"or not.
IF num1 = num2 EQUIV num3 = num4.
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_true.

IF num1 <> num2 EQUIV num3 <> num4.
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_true.

"Logical expressions is not true for both
num4 = 2.
IF num1 = num2 EQUIV num3 = num4.
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_false.
``` 

 </td>
</tr>

<tr>
<td> 

`( ... )` 

 </td>

 <td> 

- Enclose logical expressions in parentheses to create a new logical expression.  
- This refines conditions and establishes processing priorities.  
- Parenthesized expressions can be combined with Boolean operators.  

 </td>

 <td> 

``` abap
DATA(num1) = 1.
DATA(num2) = 1.
DATA(num3) = 1.
DATA(num4) = 1.
DATA result TYPE abap_boolean.

IF ( num1 = num2 AND num3 = num4 ) AND NOT ( num1 <> num2 OR num3 <> num4 ).
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_true.
``` 

 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Boolean Functions

Like predicate functions, Boolean functions are built-in logical functions that evaluate a logical expression and return a truth value.

> [!NOTE]  
> The boolean function `boolx` is available for byte-like types. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBOOLE_FUNCTIONS.html). 


<table>
<tr>
<th>Function</th>
<th>Details</th>
<th>Example</th>
</tr>

<tr>
<td> <code>boolc</code> </td>
<td>
Boolean function that returns a truth value. In this case, it is a single-character value of type <code>string</code>. When true, it returns the string <code>X</code>. When false, it returns a blank. The result is not to be compared with <code>abap_true</code> and <code>abap_false</code> (because of <code>c</code> to <code>string</code> conversion). To get the technical type <code>c</code> (with the values 'X' and ''), you can use the <code>xsdbool</code> function.
</td>

<td>

``` abap
"boolc returns an X or a blank of type string
DATA(int) = 0.
"X
DATA(boolc1) = CONV abap_bool( boolc( int IS INITIAL ) ). 

"#X#
DATA(boolc2) = |#{ boolc( int IS INITIAL ) }#|. 

"# #
DATA(boolc3) = |#{ boolc( int IS NOT INITIAL ) }#|. 

"Using the translate function to return a value other than X/blank
"1
DATA(boolc4) = translate( val = boolc( int BETWEEN -3 AND 3 ) from = `X` to = `1` ). 

"0
DATA(boolc5) = translate( val = boolc( int <> 0 ) from = ` ` to = `0` ). 
``` 

</td>
</tr>

<tr>
<td> <code>xsdbool</code> </td>
<td>
Boolean function that returns a truth value. Similar to <code>boolc</code>, it returns the value <code>X</code> for true, and a blank for false. Unlike <code>boolc</code>, the return value is of type <code>c</code> of length 1, and can be compared with <code>abap_true</code> and <code>abap_false</code>. 
</td>

<td>

``` abap
"abap_true
DATA(xsdb1) = xsdbool( 3 > 1 ). 

"#X#
DATA(xsdb2) = |#{ xsdbool( 1 = 1 ) }#|. 

"##
DATA(xsdb3) = |#{ xsdbool( 1 <> 1 ) }#|. 

"Comparison with boolc
"not equal
IF boolc( 1 = 0 ) = xsdbool( 1 = 0 ).
  DATA(res) = `equal`.
ELSE.
  res = `not equal`.
ENDIF.

"Using xsdbool instead of, for example, an IF control
"structure or an expression with the COND operator
"abap_true
DATA(xsdb4) = xsdbool( -1 < 1 ). 

DATA truth_value1 TYPE abap_bool.
IF -1 < 1.
  truth_value1 = abap_true.
ELSE.
  truth_value1 = abap_false.
ENDIF.

DATA(truth_value2) = COND #( WHEN -1 < 1 THEN abap_true ELSE abap_false ).
``` 

</td>
</tr>


</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursion: Operands of Logical Expressions

In most cases, operands of logical expressions can include:

- Data objects
- Built-in functions
- Functional methods
- Calculation expressions
- Constructor expressions
- Table expressions


```abap
DATA(num) = 1.
DATA(str_table) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).
DATA result TYPE abap_boolean.

IF
"---- Data object/literals ----
  num = 1
AND
  'ABAP' IS NOT INITIAL
AND
"---- Built-in functions ----
  reverse( `PABA` ) = `ABAP`
AND
  condense( val = ` A B A P ` to = `` ) = `ABAP`
AND
  repeat( val = `*` occ = 5 ) = `*****`
AND
  count( val = `hello` sub = `l` ) = 2
AND
  strlen( `ABAP` ) = 4
AND
  ipow( base = 2 exp = 3 ) = 8
AND
  xsdbool( 3 > 1 ) = abap_true
AND
"---- Functional methods ----
  cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 1 max  = 10 )->get_next( ) BETWEEN 1 AND 10
AND
  cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 1 max  = 10 )->get_next( )
AND
"---- Calculation expressions ----
  1 + 2 = 3
AND
  5 = ( 11 - 5 ) - 1
AND
"---- Constructor expressions ----
  VALUE i( ) = 0
AND
  CONV string( 'abc    ' ) = `abc`
AND
  COND #( WHEN num = 1 THEN `A` ELSE `B` ) = `A`
AND
"---- Table expressions ----
str_table[ 2 ] = `b`
.
  result = abap_true.
ELSE.
  result = abap_false.
ENDIF.

ASSERT result = abap_true.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>