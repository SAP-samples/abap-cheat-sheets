<a name="top"></a>

# Logical Expressions and Functions

- [Logical Expressions and Functions](#logical-expressions-and-functions)
  - [Introduction](#introduction)
  - [Comparison Expressions with Comparison Operators](#comparison-expressions-with-comparison-operators)
    - [General Comparison Operators (=, EQ, \<\>, NE, \<, LT, \>, GT, \<=, LE, \>=, GE, BETWEEN ... AND, IN)](#general-comparison-operators--eq--ne--lt--gt--le--ge-between--and-in)
    - [Comparison Operators for Character-Like Data Types (CA, NA, CO, CN, CS, NS, CP, NP)](#comparison-operators-for-character-like-data-types-ca-na-co-cn-cs-ns-cp-np)
  - [Predicates](#predicates)
    - [Predicate Expressions](#predicate-expressions)
      - [IS \[NOT\] INITIAL](#is-not-initial)
      - [IS \[NOT\] BOUND](#is-not-bound)
      - [IS \[NOT\] INSTANCE OF](#is-not-instance-of)
      - [IS \[NOT\] ASSIGNED](#is-not-assigned)
      - [IS \[NOT\] SUPPLIED](#is-not-supplied)
      - [IS \[NOT\] COMPATIBLE WITH](#is-not-compatible-with)
    - [Predicate Functions (contains, contains\_any\_of, contains\_any\_not\_of, matches, line\_exists)](#predicate-functions-contains-contains_any_of-contains_any_not_of-matches-line_exists)
    - [Predicative Method Calls](#predicative-method-calls)
    - [Data Objects as Relational Expressions](#data-objects-as-relational-expressions)
  - [Boolean Operators (AND, OR, EQUIV), Negations (NOT) and Using Parentheses](#boolean-operators-and-or-equiv-negations-not-and-using-parentheses)
  - [Boolean Value Expressions](#boolean-value-expressions)
  - [Boolean Functions (boolc, xsdbool)](#boolean-functions-boolc-xsdbool)
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
> Find more details on the comparison rules [here](https://help.sap.com/docs/abap-cloud/abap-keyword/rel-exp-comparison-rules).

> [!NOTE]
> ABAP includes special comparison operators for byte-like data types and bit patterns, which are not covered here. For more information, refer to the [ABAP Keyword Documentation](https://help.sap.com/docs/abap-cloud/abap-keyword/rel-exp-comparison-operators-for-byte-like-data-types).

<p align="right"><a href="#top">⬆️ back to top</a></p>

### General Comparison Operators (=, EQ, <>, NE, <, LT, >, GT, <=, LE, >=, GE, BETWEEN ... AND, IN)

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
Checks whether the operand on the left side  of `IN` matches (or does not match) [ranges conditions](https://help.sap.com/docs/abap-cloud/abap-keyword/ranges-condition) specified in a [ranges table](https://help.sap.com/docs/abap-cloud/abap-keyword/ranges-table). Note that the operators `CP` and `NP` are transformed into `LIKE` conditions (as a consequence, the conditions for `CP` and `NP` are case-sensitive). 

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

"Value range: Lower than 5 + greater than or equal to 15
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

### Comparison Operators for Character-Like Data Types (CA, NA, CO, CN, CS, NS, CP, NP)

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


#### IS [NOT] INITIAL 

- Checks whether the operand is initial or not. 
- The result is true if the operand contains its type-dependent initial value.
- Note that you can also specify an individual data object that represents a relational expression, returning a truth value. The expression evaluates to true if the operand's content is not initial.

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### IS [NOT] BOUND

- Checks if a reference variable contains a valid reference. 
- A data reference is valid if it can be dereferenced. 
- An object reference is valid if it points to an object.


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

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### IS [NOT] INSTANCE OF

- Checks whether casts are possible.
- It returns true if a downcast is possible for a non-initial object reference variable. 
- This is especially useful for verifying types in generically typed field symbols or formal parameters and helps prevent runtime errors from failed casts, rather than applying exception handling.

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### IS [NOT] ASSIGNED

Checks whether a memory area is assigned to a field symbol or not.

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### IS [NOT] SUPPLIED

- Checks whether an optional formal parameter of a procedure is filled or requested. 
- The expression returns true if an actual parameter was assigned to the formal parameter during the call. 
- These expressions can only be used in function modules and methods.

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### IS [NOT] COMPATIBLE WITH

- `IS [NOT] COMPATIBLE WITH` is a predicate expression that checks whether a data object's type at runtime is compatible with a specified type or not.
- Note the concept of type compatibility: All technical type properties of two data objects must match.
- The predicate returns true if the runtime type is compatible.
- This expression is particularly useful for type compatibility checks in dynamic and generic programming, such as when working with generically typed parameters or field symbols.

Syntax patterns: 
```abap
... <dobj> IS [NOT] COMPATIBLE WITH type ... 
... <dobj> IS [NOT] COMPATIBLE WITH TYPE OF dobj2 ... 
... <dobj> IS [NOT] COMPATIBLE WITH LINE OF tab_type ... 
... <dobj> IS [NOT] COMPATIBLE WITH LINE TYPE OF itab ... 
... <dobj> IS [NOT] COMPATIBLE WITH REF TO type ... 
... <dobj> IS [NOT] COMPATIBLE WITH REF TYPE OF dobj2 ... 
... <dobj> IS [NOT] COMPATIBLE WITH ANY STRUCTURE ... 
... <dobj> IS [NOT] COMPATIBLE WITH [ANY|INDEX|SORTED|HASHED|STANDARD] TABLE ... 
```

> [!NOTE]  
> - If it can be statically determined that ... 
>   - type compatibility is always guaranteed, a syntax warning occurs. 
>   - a type compatibility check can never be true, the compiler issues a syntax error. 
> - Regarding `<name>`, the named data type must exist and be accessible. Class and interface names are not supported here. They are only applicable with the syntax `REF TO name`.
> - The predicate expression cannot be used in `WHERE` clauses in internal table processing statements.
> - Case distinctions using `CASE DATA TYPE OF` are another way of checking type compatibility. It is also an alternative to RTTI checks with the method `APPLIES_TO_DATA` (see the Dynamica Programming cheat sheet).


**Example 1**

The following example demonstrates:  
- Various `IF` statements using `IS [NOT] COMPATIBLE WITH` with the syntax variant to check for type compatibility based on a type.  
- Two example statements that illustrate compiler behavior when type compatibility is always guaranteed (syntax warning) and when a type compatibility check can never be true (syntax error).


```abap
DATA dobj1 TYPE c LENGTH 1.
DATA dobj2 TYPE c LENGTH 2.
TYPES c1 TYPE c LENGTH 1.
TYPES c2 TYPE c LENGTH 2.
FIELD-SYMBOLS <fs> TYPE any.
DATA str TYPE string.

ASSIGN dobj1 TO <fs>.

IF <fs> IS COMPATIBLE WITH c1.
  str &&= `A`.
ELSE.
  str &&= `Z`.
ENDIF.

IF <fs> IS NOT COMPATIBLE WITH c1.
  str &&= `Y`.
ELSE.
  str &&= `B`.
ENDIF.

IF <fs> IS COMPATIBLE WITH c2.
  str &&= `X`.
ELSE.
  str &&= `C`.
ENDIF.

IF <fs> IS NOT COMPATIBLE WITH c2.
  str &&= `D`.
ELSE.
  str &&= `W`.
ENDIF.

ASSIGN dobj2 TO <fs>.

IF <fs> IS COMPATIBLE WITH c1.
  str &&= `V`.
ELSE.
  str &&= `E`.
ENDIF.

IF <fs> IS COMPATIBLE WITH c2.
  str &&= `F`.
ELSE.
  str &&= `U`.
ENDIF.

"The previos examples used a field symbol to check compatibility.
"The following example will show a direct check of the data object.
"However, it will show a syntax warning since it can be statically determined
"that type compatibility is always guaranteed.
IF dobj1 IS COMPATIBLE WITH c1.
  str &&= `G`.
ELSE.
  str &&= `T`.
ENDIF.

ASSERT str = `ABCDEFG`.

"The following example does not work and will show a syntax error. It can be
"statically determined that a type compatibility check can never be true.
*IF dobj2 IS COMPATIBLE WITH c1.
*  str &&= `S`.
*ELSE.
*  str &&= `H`.
*ENDIF.
```

**Example 2**

The following example demonstrates the syntax variants for `IS [NOT] COMPATIBLE WITH` using demo data types and objects and `ASSERT` statements.
 

```abap
FIELD-SYMBOLS <fs> TYPE any.

"Elementary types/data objects
TYPES t_c1 TYPE c LENGTH 1.
TYPES t_c2 TYPE c LENGTH 2.
DATA c1 TYPE c LENGTH 1.
DATA bool TYPE abap_boolean.
DATA c2 TYPE c LENGTH 2.
DATA str TYPE string.
DATA num TYPE i.

"Structured types/data objects
TYPES: BEGIN OF ty_struc1,
				 comp1 TYPE c LENGTH 1,
				 comp2 TYPE i,
			 END OF ty_struc1.

TYPES: BEGIN OF ty_struc2,
         comp1 TYPE c LENGTH 2,
         comp2 TYPE i,
       END OF ty_struc2.
			
DATA struc1 TYPE ty_struc1.			
DATA struc2 TYPE ty_struc2.

"Table types/internal tables
TYPES tt_struc1 TYPE TABLE OF ty_struc1 WITH EMPTY KEY.
TYPES tt_struc2 TYPE TABLE OF ty_struc2 WITH EMPTY KEY.
TYPES tt_string TYPE TABLE OF string WITH EMPTY KEY.

DATA itab1 TYPE tt_struc1.
DATA itab2 TYPE tt_struc2.
DATA str_tab TYPE tt_string.

DATA itab_std TYPE STANDARD TABLE OF ty_struc1 WITH NON-UNIQUE KEY comp1.
DATA itab_so TYPE SORTED TABLE OF ty_struc1 WITH UNIQUE KEY comp1.
DATA itab_ha TYPE HASHED TABLE OF ty_struc1 WITH UNIQUE KEY comp1.

"Reference types/data reference variables
TYPES ty_ref_c1 LIKE REF TO c1.
DATA dref_c1 TYPE ty_ref_c1.
TYPES ty_ref_c2 TYPE REF TO t_c2.
DATA dref_c2 TYPE ty_ref_c2.

*&---------------------------------------------------------------------*
*& <dobj> IS [NOT] COMPATIBLE WITH <name>
*&---------------------------------------------------------------------*

ASSIGN c1 TO <fs>.

ASSERT <fs> IS COMPATIBLE WITH abap_boolean.
ASSERT <fs> IS COMPATIBLE WITH abap_bool.
ASSERT <fs> IS COMPATIBLE WITH ty_struc1-comp1.

ASSERT <fs> IS NOT COMPATIBLE WITH t_c2.
ASSERT <fs> IS NOT COMPATIBLE WITH string_table.
ASSERT <fs> IS NOT COMPATIBLE WITH timestampl.

ASSIGN num TO <fs>.

ASSERT <fs> IS COMPATIBLE WITH i.
ASSERT <fs> IS NOT COMPATIBLE WITH t_c2.
ASSERT <fs> IS COMPATIBLE WITH ty_struc1-comp2.

*&---------------------------------------------------------------------*
*& <dobj> IS [NOT] COMPATIBLE WITH TYPE OF <dobj2>
*&---------------------------------------------------------------------*

ASSIGN c1 TO <fs>.

ASSERT <fs> IS COMPATIBLE WITH TYPE OF bool.
ASSERT <fs> IS NOT COMPATIBLE WITH TYPE OF c2.
ASSERT <fs> IS COMPATIBLE WITH TYPE OF struc1-comp1.

ASSIGN 1 TO <fs>.

ASSERT num IS COMPATIBLE WITH TYPE OF <fs>.
ASSERT <fs> IS COMPATIBLE WITH TYPE OF num.
ASSERT <fs> IS NOT COMPATIBLE WITH TYPE OF c1.

*&---------------------------------------------------------------------*
*& <dobj> IS [NOT] COMPATIBLE WITH LINE OF <table>
*&---------------------------------------------------------------------*

ASSIGN struc1 TO <fs>.

ASSERT <fs> IS COMPATIBLE WITH LINE OF tt_struc1.
ASSERT <fs> IS NOT COMPATIBLE WITH LINE OF tt_struc2.

ASSIGN str TO <fs>.

ASSERT <fs> IS COMPATIBLE WITH LINE OF tt_string.
ASSERT <fs> IS NOT COMPATIBLE WITH LINE OF tt_struc1.

*&---------------------------------------------------------------------*
*& <dobj> IS [NOT] COMPATIBLE WITH LINE TYPE OF <dtable>
*&---------------------------------------------------------------------*

ASSIGN struc2 TO <fs>.

ASSERT <fs> IS COMPATIBLE WITH LINE TYPE OF itab2.
ASSERT <fs> IS NOT COMPATIBLE WITH LINE TYPE OF itab1.

ASSIGN str TO <fs>.

ASSERT <fs> IS COMPATIBLE WITH LINE TYPE OF str_tab.
ASSERT <fs> IS NOT COMPATIBLE WITH LINE TYPE OF itab1.

*&---------------------------------------------------------------------*
*& <dobj> IS [NOT] COMPATIBLE WITH REF TO <name>
*&---------------------------------------------------------------------*

ASSIGN dref_c1 TO <fs>.

ASSERT <fs> IS COMPATIBLE WITH REF TO t_c1.
ASSERT <fs> IS NOT COMPATIBLE WITH REF TO t_c2.

ASSIGN dref_c2 TO <fs>.

ASSERT <fs> IS COMPATIBLE WITH REF TO t_c2.
ASSERT <fs> IS NOT COMPATIBLE WITH REF TO t_c1.

*&---------------------------------------------------------------------*
*& <dobj> IS [NOT] COMPATIBLE WITH REF TYPE OF <dobj2>
*&---------------------------------------------------------------------*

ASSIGN dref_c1 TO <fs>.

ASSERT <fs> IS COMPATIBLE WITH REF TYPE OF c1.
ASSERT <fs> IS NOT COMPATIBLE WITH REF TYPE OF c2.

ASSIGN dref_c2 TO <fs>.

ASSERT <fs> IS COMPATIBLE WITH REF TYPE OF c2.
ASSERT <fs> IS NOT COMPATIBLE WITH REF TYPE OF c1.

*&---------------------------------------------------------------------*
*& <dobj> IS [NOT] COMPATIBLE WITH ANY STRUCTURE
*&---------------------------------------------------------------------*

ASSIGN struc1 TO <fs>.
ASSERT <fs> IS COMPATIBLE WITH ANY STRUCTURE.

ASSIGN struc2 TO <fs>.
ASSERT <fs> IS COMPATIBLE WITH ANY STRUCTURE.

APPEND INITIAL LINE TO itab1 ASSIGNING <fs>.
ASSERT <fs> IS COMPATIBLE WITH ANY STRUCTURE.

APPEND INITIAL LINE TO str_tab ASSIGNING <fs>.
"The table is a string table with elementary line type, i.e. the line type
"is not a structure, so the assertion fails.
ASSERT <fs> IS NOT COMPATIBLE WITH ANY STRUCTURE.

ASSIGN dref_c1 TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH ANY STRUCTURE.

ASSIGN num TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH ANY STRUCTURE.

*&--------------------------------------------------------------------------*
*& <dobj> IS [NOT] COMPATIBLE WITH (ANY|INDEX|SORTED|HASHED|STANDARD) TABLE
*&--------------------------------------------------------------------------*

"--- ANY TABLE ---

ASSIGN itab_std TO <fs>.
ASSERT <fs> IS COMPATIBLE WITH ANY TABLE.

ASSIGN itab_so TO <fs>.
ASSERT <fs> IS COMPATIBLE WITH ANY TABLE.

ASSIGN itab_ha TO <fs>.
ASSERT <fs> IS COMPATIBLE WITH ANY TABLE.

ASSIGN num TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH ANY TABLE.

"--- INDEX TABLE ---

ASSIGN itab_std TO <fs>.
ASSERT <fs> IS COMPATIBLE WITH INDEX TABLE.

ASSIGN itab_so TO <fs>.
ASSERT <fs> IS COMPATIBLE WITH INDEX TABLE.

ASSIGN itab_ha TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH INDEX TABLE.

ASSIGN num TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH INDEX TABLE.

"--- SORTED TABLE ---

ASSIGN itab_so TO <fs>.
ASSERT <fs> IS COMPATIBLE WITH SORTED TABLE.

ASSIGN itab_std TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH SORTED TABLE.

ASSIGN itab_ha TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH SORTED TABLE.

ASSIGN num TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH SORTED TABLE.

"--- HASHED TABLE ---

ASSIGN itab_ha TO <fs>.
ASSERT <fs> IS COMPATIBLE WITH HASHED TABLE.

ASSIGN itab_std TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH HASHED TABLE.

ASSIGN itab_so TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH HASHED TABLE.

ASSIGN num TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH HASHED TABLE.

"--- STANDARD TABLE ---

ASSIGN itab_std TO <fs>.
ASSERT <fs> IS COMPATIBLE WITH STANDARD TABLE.

ASSIGN itab_so TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH STANDARD TABLE.

ASSIGN itab_ha TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH STANDARD TABLE.

ASSIGN num TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH STANDARD TABLE.

"--- TABLE (it is the same as STANDARD TABLE) ---

ASSIGN itab_std TO <fs>.
ASSERT <fs> IS COMPATIBLE WITH table.

ASSIGN itab_so TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH table.

ASSIGN itab_ha TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH table.

ASSIGN num TO <fs>.
ASSERT <fs> IS NOT COMPATIBLE WITH table.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Predicate Functions (contains, contains_any_of, contains_any_not_of, matches, line_exists)

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


### Predicative Method Calls 

- A predicative method call is a method call such as `... meth( ... ) ...` 
- A predicative method call represents a relational expression. 
- Its only operand is a functional method call, which is a method with a return value passed via the returning parameter.
- The expression evaluates to true if the method's return value is not initial. A check is performed for the type-dependent initial value.


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


### Data Objects as Relational Expressions

- Individual data objects can represent relational expressions.  
- Like predicative method calls, the expression evaluates to true when the operand's content is not initial and false when it is initial. 
- The operand's concrete type is arbitrary since it checks for type-specific initial values.  
- Specifying only the data object is a short form of the predicate expression `... IS NOT INITIAL ...`.


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

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Boolean Operators (AND, OR, EQUIV), Negations (NOT) and Using Parentheses

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

## Boolean Value Expressions

- Boolean value expressions are logical expressions that produce a truth value, which can be assigned to a target data object.  
- The evaluation of the expression results in a truth value (either true or false) that can be used in various contexts accepting expressions.  
- For example, `dobj IS INITIAL` results in either true or false. It can be specified using a common `IF` statement, such as `IF dobj IS INITIAL.`, or it can be placed in expression positions and assigned to specific target data objects, such as `target = dobj IS INITIAL.`, allowing the data object `target` to receive the truth value.  
- Target data objects are those with specific elementary data types that have a canonical truth value representation (_Boolean-like data types_), where the false value indicates the type-specific initial value.
- Among the supported Boolean-like data types, which have a two-valued representation that aligns with the Boolean semantics of true and false, are the following:

    |**Type**|**True**|**False** |
    |:---|:---|:---|
    |`c` \(length 1\)|`'X'`|`' '` \(space\) |
    |`x` \(length 1\)|`01`|`00` |
    |`n` \(length 1\)|`'1'`|`'0'` |
    |`string`|``X``|\`\` \(initial string\) |
    |`i`, `int8`, `int1`, `int2` |`1`|`0` |
    |`decfloat16`, `decfloat34`, `f`, <br> `p` \(values in data objects of type `p` only with predecimals\)|`1`|`0`|

> [!NOTE] 
> - The following examples demonstrate assignments of Boolean value expressions to a target data object in the context of assignments using the operator `=` (here, Boolean value expressions appear on the right-hand side (e.g., `target = dobj IS INITIAL.`) and in the context of actual parameter assignments during method calls (e.g., `meth( dobj IS INITIAL )`).
> - Special cases: 
>   - Operator `=`
>      - The operator `=` serves dual purposes for both comparisons and assignments. To distinguish between its use in comparisons and assignments during multiple assignments  and when using unnamed formal parameters, consider using:
>          - `=` with the logical expression enclosed in parentheses 
>          - `EQ` instead of `=` 
>   - Predicative method calls (i.e., methods with a single return parameter) / single data objects as relational expressions (e.g., `IF dobj.`)
>      - Typically, a predicative method call evaluates to its return value, which is also true for a single data object that has its actual content evaluated. 
>      - However, in Boolean contexts - such as those initiated by `IF` statements or logical expressions like `NOT` - they do not evaluate to their usual meanings but instead implicitly use `IS NOT INITIAL` semantics. 
>      - Thus, predicative method calls and single data objects can serve as Boolean value expressions only in designated Boolean contexts. Refer to the  examples in _Example 3_ that illustrate this behavior.
> - If it is statically determined that a Boolean value expression is assigned to a non-Boolean-like type, a syntax error will occur. For example, attempting to assign a Boolean value expression to a data object of type `c` with length 2 (in dynamic programming) will result in a runtime error.  



**Example 1**

- The example explores the assignment of Boolean value expressions (they use general comparison operators) specified on the right-hand side to target data objects with the assignment operator `=`. 
- On the left-hand side of the assignment, target data objects with various Boolean-like data types are specified, inlcuding an example that specifies a target data object with a non-Boolean-like data type.
- The data objects with Boolean-like data types are assigned the type-specific true and false value representations.

```abap
"Demo types and data objects
TYPES t_c1 TYPE c LENGTH 1.
DATA c1 TYPE t_c1.
TYPES t_x1 TYPE x LENGTH 1.
DATA x1 TYPE t_x1.
TYPES t_n1 TYPE n LENGTH 1.
DATA n1 TYPE t_n1.
DATA str TYPE string.
DATA int TYPE i.
DATA dec34 TYPE decfloat34.
DATA float TYPE f.
TYPES t_pl8d2 TYPE p LENGTH 8 DECIMALS 2.
DATA packed TYPE t_pl8d2.

"Demo values which the Boolean value expression are based on
DATA(num1) = 10.
DATA(num2) = 20.

**********************************************************************

c1 = num1 <> num2.
ASSERT c1 = 'X'.

c1 = num1 EQ num2.
ASSERT c1 = ' '.

x1 = num1 <> num2.
ASSERT x1 = CONV t_x1( '01' ).

x1 = num1 EQ num2.
ASSERT x1 = CONV t_x1( '00' ).

n1 = num1 <> num2.
ASSERT n1 = CONV t_n1( '1' ).

n1 = num1 EQ num2.
ASSERT n1 = CONV t_n1( '0' ).

str = num1 <> num2.
ASSERT str = `X`.

str = num1 EQ num2.
ASSERT str = ``.

int = num1 <> num2.
ASSERT int = 1.

int = num1 EQ num2.
ASSERT int = 0.

dec34 = num1 <> num2.
ASSERT dec34 = CONV decfloat34( '1' ).

dec34 = num1 EQ num2.
ASSERT dec34 = CONV decfloat34( '0' ).

float = num1 <> num2.
ASSERT float = CONV f( '1' ).

float = num1 EQ num2.
ASSERT float = CONV f( '0' ).

packed = num1 <> num2.
ASSERT packed = CONV t_pl8d2( '1' ).

packed = num1 EQ num2.
ASSERT packed = CONV t_pl8d2( '0' ).

**********************************************************************

"Non-Boolean-like data type
TYPES t_c2 TYPE c LENGTH 2.
DATA c2 TYPE t_c2.

"The following assignment is not allowed because the target data object type
"is not a Boolean-like data type.
"c2 = num1 <> num2.
```

**Example 2**

- The example demonstrates passing Boolean value expressions as actual parameters, especially with unnamed formal parameters. For this purpose, the class includes methods with one importing parameter (with and without a Boolean-like data type).
- It also shows the special case of `=` as comparison and assignment operator when dealing with Boolean value expressions and multiple assignments.


```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS meth IMPORTING num        TYPE i
                 RETURNING VALUE(res) TYPE i.

    METHODS meth2 IMPORTING number     TYPE i
                  RETURNING VALUE(res) TYPE i.

    TYPES t_c2 TYPE c LENGTH 2.
    METHODS meth3 IMPORTING c2         TYPE t_c2
                  RETURNING VALUE(res) TYPE t_c2.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------*
*& Passing Boolean value expressions in method calls
*&---------------------------------------------------------------------*

    "Note:
    "- The demo methods have a single formal parameter 'num' and 'number'.
    "- The formal parameter 'num' of one method is intentionally named like a local variable
    "- The methods have only one importing parameter so that the actual parameter can be passed without
    "  explicitly specifying the formal parameter name.

    "Explicitly specifying the formal parameter name
    DATA(res_a) = meth( num = 5 ).
    DATA(res_b) = meth2( number = 6 ).
    ASSERT res_a = 5.
    ASSERT res_b = 6.

    "Not specifying the formal parameter name explicitly
    DATA(res_c) = meth( 7 ).
    DATA(res_d) = meth( 8 ).
    ASSERT res_c = 7.
    ASSERT res_d = 8.

    "Passing a Boolean value expression
    DATA(num) = 10.
    DATA(num_b) = 10.

    "The following example is not a Boolean value expression. The left-hand 'num' stands for the explicitly
    "specified formal parameter name.
    DATA(res_e) = meth( num = num_b ).
    ASSERT res_e = 10.

    DATA(res_f) = meth2( number = num ).
    ASSERT res_f = 10.

    "Passing a Boolean value expression by not specifying the formal parameter explicitly and
    "enclosing the expression in a pair of parentheses
    "The left-hand 'num' refers to the local variable.
    DATA(res_g) = meth( ( num = num_b ) ).
    ASSERT res_g = 1.
    DATA(res_h) = meth2( ( num = num_b ) ).
    ASSERT res_h = 1.

    "Passing a Boolean value expression by not specifying the formal parameter explicitly and
    "specifying the comparison operator EQ instead of =.
    "As above, the left-hand 'num' refers to the local variable.
    DATA(res_i) = meth( num EQ num_b ).
    ASSERT res_i = 1.

    DATA(res_j) = meth2( num EQ num_b ).
    ASSERT res_j = 1.

    "The following example shows the assignment of a Boolean value expression to the explicitly specified
    "formal parameter. 'num' is assigned the result of the expression. So, the specification does not represent
    "a multiple assignment.
    DATA(res_k) = meth( num = num = num_b ).
    ASSERT res_k = 1.

    "Mutliple assignment
    num = num = num_b.
    ASSERT num = 10.

    "The following example is not possible as the formal parameter 'num' does not exist.
    "DATA(res_l) = meth2( num = num = num_b ).
    DATA(res_m) = meth2( number = num = num_b ).
    ASSERT res_m = 1.

    "Passing a Boolean value expression by specifying the formal parameter explicitly
    "Using the comparison operator EQ instead of =, used as assignment operator.
    DATA(res_n) = meth( num = num EQ num_b ).
    ASSERT res_n = 1.

    DATA(res_o) = meth2( number = num EQ num_b ).
    ASSERT res_o = 1.

    "Using the comparison a parenhesized Boolean value expressions
    DATA(res_p) = meth( num = ( num = num_b ) ).
    ASSERT res_p = 1.

    DATA(res_q) = meth2( number = ( num = num_b ) ).
    ASSERT res_q = 1.

    "Note: In meth2, the formal parameter is of type c length 2, i.e. it is not a Boolean-like data type.
    "Therefore, a Boolean value expression cannot be passed.
    "DATA(res_z) = meth3( num EQ num_b ).

*&---------------------------------------------------------------------*
*& Multiple assignments
*&---------------------------------------------------------------------*

    DATA num_x TYPE i.
    DATA num_y TYPE i.
    DATA num_z TYPE i.

    DO 6 TIMES.
      num_x = 5.
      num_y = 6.
      num_z = 7.

      CASE sy-index.
        WHEN 1.
          "Multiple assignment; no Boolean value expression after the first = operator
          num_x = num_y = num_z.
          ASSERT num_x = 7.
        when 2.
          "Negation with NOT
          "In this case, it is a Boolean value expression after the first = operator.
          num_x = NOT num_y = num_z.
          ASSERT num_x = 1.
        WHEN 3.
          "Boolean value expression after the first = operator, indicated by EQ
          num_x = num_y EQ num_z.
          ASSERT num_x = 0.
        WHEN 4.
          "Negation of the previous example
          num_x = NOT num_y EQ num_z.
          ASSERT num_x = 1.
        WHEN 5.
          "Enclosing a Boolean value expression that uses the = operator in parentheses
          num_x = ( num_y = num_z ).
          ASSERT num_x = 0.
        WHEN 6.
          "Negation of the previous example
          num_x = NOT ( num_y = num_z ).
          ASSERT num_x = 1.
      ENDCASE.
    ENDDO.

    out->write( 'All asserts ok.' ).

  ENDMETHOD.

  METHOD meth.
    RETURN num.
  ENDMETHOD.

  METHOD meth2.
    RETURN number.
  ENDMETHOD.

  METHOD meth3.
    RETURN c2.
  ENDMETHOD.

ENDCLASS.
```

**Example 3**    

- The example explores a variety of Boolean value expressions (among them: general comparison operators, comparison operators for character-like data types, predicate expressions, functions and method calls, as well as single data objects as relational expressions) specified on the right-hand side of assignments with the assignment operator `=`. 
- On the left-hand side of the assignment, the target data object is typed with the Boolean-like data type `i`. The data object is assigned the type-specific true (1) and false (0) value representation.

```abap
DATA num TYPE i.

DATA(num_a) = 0.
DATA(num_b) = 10.

DATA(dref) = NEW string( `ABAP` ).
DATA(uuid) = NEW cl_system_uuid( ).

FIELD-SYMBOLS <fs> TYPE any.
DATA(some_str) = `hello`.
ASSIGN some_str TO <fs>.

*&---------------------------------------------------------------------*
*& General Comparison Operators
*&---------------------------------------------------------------------*

"Note the enclosing of the Boolean value expression in parentheses. Without the parentheses,
"it would be a multiple assignment (due to the semantics of the operator = for both comparisons
"and assignments).
num = ( num_a = num_b ). "0

"Multiple assignment
num = num_a = num_b. "num = 10
CLEAR num_a.

"Alternative syntax for the comparison operator = is EQ to differentiate
"the semantics.
num = num_a EQ num_b. " 0

num =  num_a <> num_b. "0
num =  num_a NE num_b. "0

num =  num_a < num_b. "1
num =  num_a LT num_b. "1

num =  num_a > num_b. "0
num =  num_a GT num_b. "0

num =  num_a <= num_b. "1
num =  num_a LE num_b. "1

num =  num_a >= num_b. "0
num =  num_a GE num_b. "0

num =  5 BETWEEN num_a AND num_b. "1
num =  15 BETWEEN num_a AND num_b. "0

DATA rangestab TYPE RANGE OF i.
rangestab = VALUE #( ( sign = 'I' option = 'BT' low = 1 high = 10 ) ).
num = 5 IN rangestab. "1
num = 15 IN rangestab. "0

*&---------------------------------------------------------------------*
*& Comparison Operators for Character-Like Data Types
*&---------------------------------------------------------------------*

DATA(s) = `cheers`.

num = s CA `aeiou`. "1
num = s CA `xy`. "0

s = `Hallo`.
num = s NA `bcdeh`. "1
num = s NA `bcdeH`. "0

s = `abcd`.
num = s CO `abcd`. "1
num = s CO `abCd`. "0

s = `abap`.
num = s CN `ab`. "1
num = s CN `abp`. "0

s = `Lorem ipsum dolor sit amet.`.
num = s CS `or`. "1
num = s CS `zz`. "0

s = `some test string`.
num = s NS `tests`. "1
num = s NS `TEST`. "0

s = `abc_def_ghi`.
num = s CP `*f#_*`. "1
num = s CP `*i+`. "0

s = `abcDEFghi`.
num = s NP `*c#d*`. "1
num = s NP `*c#D*`. "0

*&---------------------------------------------------------------------*
*& Predicate Expressions
*&---------------------------------------------------------------------*

num = num_a IS INITIAL. "1
num = num_b IS NOT INITIAL. "1

num = dref IS BOUND. "1
num = dref IS NOT BOUND. "0

num = dref IS BOUND. "1
num = dref IS NOT BOUND. "0

num = uuid IS INSTANCE OF cl_system_uuid. "1
num = uuid IS NOT INSTANCE OF cl_system_uuid. "0

num = <fs> IS ASSIGNED. "1
num = <fs> IS NOT ASSIGNED. "0

"Assumption: some_param is a parameter of the method, and it is optional.
"num = some_param IS SUPPLIED.
"num = some_param IS NOT SUPPLIED.

"Note: IS COMPATIBLE has many more specification options.
num = <fs> IS COMPATIBLE WITH i. "0
num = <fs> IS NOT COMPATIBLE WITH i. "1

*&---------------------------------------------------------------------*
*& Predicate Functions
*&---------------------------------------------------------------------*

num = contains( val = `ABCDE` start = `ab` case = abap_true ). "0
num = contains( val = `ABCDE` start = `ab` case = abap_false ). "1
num = contains_any_of( val = `abcdefg` sub = `xyza` ). "1
num = contains_any_of( val = `abcdefg` sub = `xyz` ). "0
num = contains_any_not_of( val = `1hallo` start = `abcdefghijklmnopqrstuvwxyz` ). "1
num = contains_any_not_of( val = `1hallo` end = `abcdefghijklmnopqrstuvwxyz` ). "0
num = matches( val = `jon.doe@email.com` pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ). "1

DATA(str_tab) = VALUE string_table( ( `abc` ) ( `def` ) ( `ghi` ) ).
num = line_exists( str_tab[ 1 ] ). "1
num = line_exists( str_tab[ 4 ] ). "0

*&---------------------------------------------------------------------*
*& Predicative Method Calls
*&---------------------------------------------------------------------*

"- Predicative method calls and single data objects can serve as Boolean value expressions
"  only in designated Boolean contexts.
"- Only in these contexts, the return value of the method call or the value of the data object
"  is implicitly interpreted as a Boolean value expression, and IS NOT INITIAL semantics is applied.
"- The example below uses the class cl_abap_random_int to create a random integer.

"Regular assignment of the return value of the method to the variable num.
"The predicative method call on the right does not represent a Boolean value expression.
num = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
																	min  = 1
																	max  = 10 )->get_next( ).

"In the following examples, the predicative method call on the right represents a Boolean value expression,
"and IS NOT INITIAL semantics is applied, since NOT initiates a logical expression.
"The self-contained, but nonsensical examples showcase the assignment of the type-specific true or false value.
"The min and max values are only specified like this to showcase the assignmment of the value 1 (true) to the
"variable num as the resulting value of the method call, which is always 0. NOT negates. Therefore, the Boolean
"value assigned is 1 (true).
num = NOT cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                      min  = 0
                                      max  = 0 )->get_next( ). "1

"Comparisons
"Without predicative method call and explicit IS NOT INITIAL specification.
num = 0 IS NOT INITIAL. "0 (false; 0 is initial)
num = NOT 0 IS NOT INITIAL. "1 (true; corresponds to the example with the result of the predicative method call above
                            "and to the following example that explicitly specifies IS NOT INITIAL)

"Explicit specification of IS NOT INITIAL with the method call.
num = NOT cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                      min  = 0
                                      max  = 0 )->get_next( ) IS NOT INITIAL. "1


"The min and max values in the following examples are chosen deliberately so that the random integer
"created cannot be 0, and, therefore, the Boolean value expression cannot be evaluated as true.
"That is, the actual IS NOT INITIAL evaluation will always result in true. Due to the negation with
"NOT, the Boolean value assigned is 0 (false).
num = NOT cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                      min  = 1
                                      max  = 10 )->get_next( ). "0

"Boolean context using the AND Boolean operator
"The nonsensical min and max value specification in the example can result in 0 or 1 assigned to the
"variable num due to random value creation. The Boolean value expression is evaluated as true if both
"method calls return a non-zero value.
num = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = -1 max = 1 )->get_next( )
      AND cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = -2 max = 2 )->get_next( ).

"Boolean context using the AND Boolean operator
num = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = -1 max = 1 )->get_next( )
      OR cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = -2 max = 2 )->get_next( ).


*&---------------------------------------------------------------------*
*& Single Data Objects as Relational Expressions
*&---------------------------------------------------------------------*

"See the notes in the previous section about predicative method calls.

"The following examples show regular assignments of the content of the right-hand variable
"to the variable num on the left-hand side.
"The single data object on the right does not represent a Boolean value expression.
num = num_b. "num = 10
"In the following example, the right-hand data object is a table, and the left-hand data
"object is a data object of type i.
"An assignment is not possible.
"num = str_tab.

"In the following examples, the single data object on the right represents a Boolean value expression,
"and IS NOT INITIAL semantics is applied, since NOT initiates a logical expression.
num = NOT str_tab. "0

CLEAR str_tab.
num = NOT str_tab. "1

"Boolean context using the AND Boolean operator
APPEND INITIAL LINE TO str_tab.

num = str_tab AND num_b. "1

CLEAR: str_tab, num_b.
num = str_tab AND num_b. "0
```


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Boolean Functions (boolc, xsdbool)

Like predicate functions, Boolean functions are built-in logical functions that evaluate a logical expression and return a truth value.

> [!NOTE]  
> The boolean function `boolx` is available for byte-like types. Find more information [here](https://help.sap.com/docs/abap-cloud/abap-keyword/boolc-boolx-xsdbool-boolean-functions). 


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