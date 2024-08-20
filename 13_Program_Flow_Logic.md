<a name="top"></a>

# Program Flow Logic

- [Program Flow Logic](#program-flow-logic)
  - [Introduction](#introduction)
  - [Expressions and Functions for Conditions](#expressions-and-functions-for-conditions)
  - [Control Structures](#control-structures)
    - [`IF` Statements](#if-statements)
      - [Excursion: `COND` Operator](#excursion-cond-operator)
    - [`CASE`: Case Distinctions](#case-case-distinctions)
      - [Excursion: `SWITCH` Operator](#excursion-switch-operator)
    - [Loops](#loops)
      - [`DO`: Unconditional Loops](#do-unconditional-loops)
      - [Interrupting and Exiting Loops](#interrupting-and-exiting-loops)
      - [`WHILE`: Conditional Loops](#while-conditional-loops)
      - [Loops Across Tables](#loops-across-tables)
  - [Calling Procedures](#calling-procedures)
    - [Methods of Classes](#methods-of-classes)
    - [Function Modules](#function-modules)
      - [Calling Function Modules](#calling-function-modules)
      - [Function Module Example](#function-module-example)
      - [Special Function Modules in Standard ABAP](#special-function-modules-in-standard-abap)
    - [Subroutines in Standard ABAP](#subroutines-in-standard-abap)
    - [Excursion: RETURN](#excursion-return)
  - [Interrupting the Program Execution](#interrupting-the-program-execution)
  - [Handling Exceptions](#handling-exceptions)
      - [Notes on Exception Classes](#notes-on-exception-classes)
    - [Raising Exceptions](#raising-exceptions)
  - [Excursion: Runtime Errors and Terminating Programs](#excursion-runtime-errors-and-terminating-programs)
  - [Executable Example](#executable-example)


This cheat sheet gathers information on program flow logic. Find more details
[here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_flow_logic.htm)
in the ABAP Keyword Documentation.

## Introduction

In ABAP, the flow of a program is controlled by [control structures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencontrol_structure_glosry.htm), [procedure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm) calls and the raising or handling of [exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenexception_glosry.htm).

Using control structures as an example, you can determine the conditions for further processing of code, for example, if at all or how often a statement block should be executed. Control structures - as, for example, realized by an `IF ... ELSEIF ... ELSE ... ENDIF.` statement - can include multiple [statement blocks](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatement_block_glosry.htm) that are executed depending on conditions.

In a very simple form, such an [`IF`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapif.htm) statement might look as follows:

```abap
DATA(num) = 1 + 1.

"A simple condition: Checking if the value of num is 2
IF num = 2.
  ... "Statement block
      "Here goes some code that should be executed if the condition is true.
ELSE.
  ... "Statement block
      "Here goes some code that should be executed if the condition is false.
      "For example, if num is 1, 8, 235, 0 etc., then do something else.
ENDIF.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Expressions and Functions for Conditions
- So, such control structures are executed depending on conditions as specified above: `... num = 2 ...` - a [logical expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogical_expression_glosry.htm).
- Control structures are generally controlled by logical expressions that define conditions for [operands](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_glosry.htm).
- The result of such an expression is either true or false.
- Logical expressions are either single [relational expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrelational_expression_glosry.htm) or expressions combined from one or more logical expressions with Boolean operators like `NOT`, `AND` and `OR`.

```abap
"Single relational expression
IF num = 1.
 ...
ENDIF.

"Multiple expressions
IF num = 1 AND flag = 'X'.
 ...
ENDIF.

IF num = 1 OR flag = 'X'.
 ...
ENDIF.

"Multiple expressions can be parenthesized explicitly
IF ( num = 1 AND flag = 'X' ) OR ( num = 2 AND flag = 'X' ).
 ...
ENDIF.
```

- The components of such relational expressions can be [comparisons](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomparison_glosry.htm) or [predicates](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpredicate_glosry.htm). Note that for [comparison expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomparison_expression_glosry.htm),
the comparisons are carried out according to [comparison rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_rules.htm).

The following code snippet shows a selection of possible expressions and operands of such expressions using a big `IF` statement. Certainly, such a huge statement is far from ideal. Here, the intention is to just cover many syntax options in one go for demonstration purposes. For more information on built-in functions, you can refer to the [Misc Built-In Functions](/24_Misc_Builtin_Functions.md) cheat sheet.

```abap
"Some declarations to be used in the IF statement below
DATA(num) = 2.                    "integer
DATA(empty_string) = ``.          "empty string
DATA(flag) = 'x'.
DATA(dref) = NEW string( `ref` ). "data reference variable

"Object reference variable
DATA oref TYPE REF TO object.
"Creating an object and assigning it to the reference variable
oref = NEW zcl_demo_abap_prog_flow_logic( ).

"Declaration of and assignment to a field symbol
FIELD-SYMBOLS <fs> TYPE string.
ASSIGN `hallo` TO <fs>.

"Creating an internal table of type string inline
DATA(str_table) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).

"The following IF statement includes multiple expressions combined by AND to demonstrate different options

"Comparisons
IF 2 = num    "equal, alternative EQ
AND 1 <> num  "not equal, alternative NE
AND 1 < num   "less than, alternative LT
AND 3 > num   "greater than, alternative GT
AND 2 >= num  "greater equal, alternative GE
AND 2 <= num  "less equal, alternative LE

"Checks whether the content of an operand is within a closed interval
AND num BETWEEN 1 AND 3
AND NOT num BETWEEN 5 AND 7   "NOT negates a logical expression
AND ( num >= 1 AND num <= 3 ) "Equivalent to 'num BETWEEN 1 AND 3';
                              "here, demonstrating the use of parentheses

"Comparison operators CO, CN ,CA, NA, CS, NS, CP, NP for character-like data types;
"see the cheat sheet on string processing

"Predicate Expressions
AND empty_string IS INITIAL  "Checks whether the operand is initial. The expression
                             "is true, if the operand contains its type-dependent initial value
AND num IS NOT INITIAL       "NOT negates

AND dref IS BOUND  "Checks whether a data reference variable contains a valid reference and
                   "can be dereferenced;
                   "Negation (IS NOT BOUND) is possible which is also valid for the following examples
AND oref IS BOUND  "Checks whether an object reference variable contains a valid reference

"IS INSTANCE OF checks whether for a
"a) non-initial object reference variable the dynamic type
"b) for an initial object reference variable the static type
"is more specific or equal to a comparison type.
AND oref IS INSTANCE OF zcl_demo_abap_prog_flow_logic
AND oref IS INSTANCE OF if_oo_adt_classrun

AND <fs> IS ASSIGNED  "Checks whether a memory area is assigned to a field symbol

"See the predicate expression IS SUPPLIED in the executable example.
"It is available in method implementations and checks whether a formal parameter
"of a procedure is filled or requested.

"Predicate function: Some examples
AND contains( val = <fs> pcre = `\D` )  "Checks whether a certain value is contained;
                                        "the example uses the pcre parameter for regular expressions;
                                        "it checks whether there is any non-digit character contained
AND matches( val = <fs> pcre = `ha.+` ) "Compares a search range of the argument for the val parameter;
                                        "the example uses the pcre parameter for regular expressions;
                                        "it checks whether the value matches the pattern 'ha'
                                        "and a sequence of any characters

"Predicate functions for table-like arguments
"Checks whether a line of an internal table specified in the table expression
"exists and returns the corresponding truth value.
AND line_exists( str_table[ 2 ] )

"Predicative method call
"The result of the relational expression is true if the result of the functional method call
"is not initial and false if it is initial. The data type of the result of the functional method call,
"i. e. the return value of the called function method, is arbitrary.
"A check is made for the type-dependent initial value.
AND check_is_supplied( )
"It is basically the short form of such a predicate expression:
AND check_is_supplied( ) IS NOT INITIAL

"Boolean Functions
"Determine the truth value of a logical expression specified as an argument;
"the return value has a data type dependent on the function and expresses
"the truth value of the logical expression with a value of this type.

"Function boolc: Returns a single-character character string of the type string.
"If the logical expression is true, X is returned. False: A blank is returned.
"Not to be compared with the constants abap_true and abap_false in relational expressions,
"since the latter convert from c to string and ignore any blanks. Note: If the logical
"expression is false, the result of boolc does not meet the condition IS INITIAL since
"a blank and no empty string is returned. If this is desired, the function xsdbool
"can be used instead of boolc.
AND boolc( check_is_supplied( ) ) = 'X'

"Result has the same ABAP type as abap_bool.
AND xsdbool( check_is_supplied( ) ) = abap_true

"Examples for possible operands

"Data objects as shown in the examples above
AND 2 = 2
AND num = 2

"Built-in functions
AND to_upper( flag ) = 'X'
AND NOT to_lower( flag ) = 'X'

"Numeric functions
AND ipow( base = num exp = 2 ) = 4

"Functional methods
"Assume such a method exists having one return value
AND addition( num1 = 1 num2 = 1 ) = 2

"Calculation expressions
AND 4 - 3 + 1 = num

"String expressions
AND `ha` && `llo` = <fs>

"Constructor expression
AND CONV i( '2.03' ) = num
AND VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ) = str_table

"Table expression
AND str_table[ 2 ] = `b`.

  ... "All of the logical expressions are true.

ELSE.

  ... "At least one of the logical expressions is false.

ENDIF.
```

> **💡 Note**<br>
> Logical expressions and functions can also be used in other ABAP statements.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Control Structures

### `IF` Statements

- As already shown above, `IF` statements define statement blocks that can be included in [branches](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbranch_glosry.htm).
- The statement blocks are executed depending on conditions.
- A maximum of one statement block is executed.
- The check is carried out from top to bottom. The statement block after the first logical expression that is true is executed.
- If none of the logical expressions are true, the statement block after the `ELSE` statement is executed.
- `ELSE` and `ELSEIF` statements are optional. However, it is recommended that you specify an `ELSE` so that at least one statement block is executed.
- If the end of the executed statement block is reached or if no statement block has been executed, the processing is continued after `ENDIF.`.


```abap
DATA(abap) = `ABAP`.
FIND `AB` IN abap.
IF sy-subrc = 0.
  "found
  ...
ELSE.
  "not found
  ...
ENDIF.

"IF statement with multiple included ELSEIF statements
DATA(current_utc_time) = cl_abap_context_info=>get_system_time( ).
DATA greetings TYPE string.

IF current_utc_time BETWEEN '050000' AND '115959'.
  greetings = |Good morning, it's { current_utc_time TIME = ISO }.|.
ELSEIF current_utc_time BETWEEN '120000' AND '175959'.
  greetings = |Good afternoon, it's { current_utc_time TIME = ISO }.|.
ELSEIF current_utc_time BETWEEN '180000' AND '215959'.
  greetings =  |Good evening, it's { current_utc_time TIME = ISO }.|.
ELSE.
  greetings = |Good night, it's { current_utc_time TIME = ISO }.|.
ENDIF.

```


Control structures can be nested.
```abap
DATA(num) = 1.
DATA(flag) = 'X'.

IF num = 1.

  IF flag = 'X'.
   ...
   ELSE.
    ...
  ENDIF.

ELSE.

  ... "statement block, e. g.
      "ASSERT 1 = 0.
      "Not to be executed in this example.

ENDIF.
```

> **💡 Note**<br>
> - Control structures can be nested. It is recommended that you do not include more than 5 nested control structures since the code will
>   get really hard to understand. Better go for outsourcing functionality into methods to reduce nested control control structures.
> - Keep the number of consecutive control structures low.
> - If you are convinced that a specified logical expression must always be true, you might include a statement like `ASSERT 1 = 0.` to go
>   sure - as implied in the example's `ELSE` statement above. However, an `ELSE` statement that is never executed might be a hint that
>   logical expressions might partly be redundant.

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Excursion: `COND` Operator

- The conditional operator [`COND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconditional_expression_cond.htm) can also be used to implement branches in operand positions that are based on logical expressions.
- Such conditional expressions have a result that is dependent on the logical expressions.
- The result's data type is specified after `COND` right before the first parenthesis. It can also be the `#` character as a symbol for the operand type if the type can be derived from the context. 
- All operands specified after `THEN` must be convertible to the result's data type.
- See also the [Constructor Expressions](05_Constructor_Expressions.md) cheat sheet for more information and examples.

```abap 
DATA greetings_cond TYPE string.
DATA(current_utc_time_cond) = cl_abap_context_info=>get_system_time( ).
greetings_cond = COND #( WHEN current_utc_time_cond BETWEEN '050000' AND '115959' THEN |Good morning, it's { current_utc_time_cond TIME = ISO }.|
                         WHEN current_utc_time_cond BETWEEN '120000' AND '175959' THEN |Good afternoon, it's { current_utc_time_cond TIME = ISO }.|
                         WHEN current_utc_time_cond BETWEEN '180000' AND '215959' THEN |Good evening, it's { current_utc_time_cond TIME = ISO }.|
                         ELSE |Good night, it's { current_utc_time_cond TIME = ISO }.| ).

```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### `CASE`: Case Distinctions

- [`CASE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcase.htm) statements are used for case distinctions.
- Such statements can also contain multiple statement blocks of which a maximum of one is executed depending on the value of the operand specified after `CASE`.
- The check is carried out from top to bottom. If the content of an operand specified after `WHEN` matches the content specified after `CASE`, the statement block is executed. Constant values should be specified as operands.
- The `WHEN` statement can include more than one operand using the syntax `WHEN op1 OR op2 OR op3 ...`.
- If no matches are found, the statement block is executed after the statement `WHEN OTHERS.` which is optional.
- If the end of the executed statement block is reached or no statement block is executed, the processing continues after `ENDCASE.`.


```abap
"Getting a random number
DATA(random_num) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                               min  = 1
                                               max  = 5 )->get_next( ).
DATA num TYPE string.

CASE random_num.
  WHEN 1.
    num = `The number is 1.`.
  WHEN 2.
    num = `The number is 2.`.
  WHEN 3 OR 4.
    num = `The number is either 3 or 4.`.
  WHEN OTHERS.
    num = `The number is not between 1 and 4.`.
ENDCASE.
```

Special control structure introduced by [`CASE TYPE OF`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcase_type.htm): Checks the type of object reference variables. An object reference variable with the static type of a class or an interface must be specified after `CASE TYPE OF`.

```abap
"The example shows the retrieval of type information at runtime (RTTI).
"For more information on RTTI, refer to the Dynamic Programming cheat
"sheet. The result of the method call is a type description object
"that points to one of the classes specified.
DATA stringtab TYPE TABLE OF string WITH EMPTY KEY.
DATA(type_description) = cl_abap_typedescr=>describe_by_data( stringtab ).

CASE TYPE OF type_description.
  WHEN TYPE cl_abap_elemdescr.
    ...
  WHEN TYPE cl_abap_refdescr.
    ...
  WHEN TYPE cl_abap_structdescr.
    ...
  WHEN TYPE cl_abap_tabledescr.
    "This is the class the type description object points to for 
    "the example data object.
    ...
  WHEN OTHERS.
    ...
ENDCASE.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Excursion: `SWITCH` Operator

The conditional operator [`SWITCH`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconditional_expression_switch.htm) can also be used to make case distinctions in operand positions. As mentioned above for `COND`, a result is constructed. The same criteria apply for `SWITCH` as for `COND` regarding the type. See also the ABAP Keyword Documentation and the [Constructor Expressions](05_Constructor_Expressions.md) cheat sheet for more information and examples.


```abap
DATA(random_num_switch) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                      min  = 1
                                                      max  = 5 )->get_next( ).

DATA(num_switch) = SWITCH #( random_num_switch
                             WHEN 1 THEN `The number is 1.`
                             WHEN 2 THEN `The number is 2.`
                             WHEN 3 OR 4 THEN `The number is either 3 or 4.`
                             ELSE `The number is not between 1 and 4.` ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Loops

#### `DO`: Unconditional Loops

- A statement block specified between `DO` and `ENDDO` is carried out multiple times.
- The loop is exited when a statement to terminate the loop is reached (`EXIT`, see further down). Otherwise, it is executed endlessly.

  ```abap
  DATA str_a TYPE string.
  DO.
    str_a &&= sy-index.
    IF sy-index = 5.
      EXIT.
    ENDIF.
  ENDDO.
  "str_a: 12345
  ```
- To restrict the loop passes, you can use the `TIMES` addition and specify the maximum number of loop passes.

  ```abap
  DATA str_b TYPE string.
  DO 9 TIMES.
    str_b &&= sy-index.
  ENDDO.
  "str_b: 123456789
  ```
- The value of the system field `sy-index` within the statement block contains the number of previous loop passes including the current pass.

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Interrupting and Exiting Loops

The following ABAP keywords are available for interrupting and exiting loops:

| Keyword  | Syntax  | Details  |
|---|---|---|
| [`CONTINUE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcontinue.htm)  | `CONTINUE.`  | The current loop pass is terminated immediately and the program flow is continued with the next loop pass. |
| [`CHECK`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcheck_loop.htm)  | `CHECK log_exp.`  | Conditional termination. If the logical expression `log_exp` is false, the current loop pass is terminated immediately and the program flow is continued with the next loop pass. |
| [`EXIT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapexit_loop.htm)  | `EXIT.`  | The loop is terminated completely. The program flow resumes after the closing statement of the loop.  |

```abap
"------------ CONTINUE ------------
DATA str_c TYPE string.
DO 15 TIMES.
  "Continue with the next loop pass if the number is even
  "Terminating the loop pass and continuing with the next loop pass if the condition specified
  "with the IF statement is true (if the number is even)
  IF sy-index MOD 2 = 0.
    CONTINUE.
  ELSE.
    str_c = |{ str_c }{ COND #( WHEN str_c IS NOT INITIAL THEN `, ` ) }{ sy-index }|.
  ENDIF.
ENDDO.
"str_c: 1, 3, 5, 7, 9, 11, 13, 15

"------------ CHECK ------------
DATA str_d TYPE string.
DO 15 TIMES.
  "Terminating the loop pass and continuing with the next loop pass if the condition is
  "true (if the number is odd)
  CHECK sy-index MOD 2 = 0.
  str_d = |{ str_d }{ COND #( WHEN str_d IS NOT INITIAL THEN `, ` ) }{ sy-index }|.
ENDDO.
"str_d: 2, 4, 6, 8, 10, 12, 14

"CHECK NOT
DATA str_e TYPE string.
DO 15 TIMES.
  "Here, it is checked whether the sy-index value is not an even number
  CHECK NOT sy-index MOD 2 = 0.
  str_e = |{ str_e }{ COND #( WHEN str_e IS NOT INITIAL THEN `, ` ) }{ sy-index }|.
ENDDO.
"str_e: 1, 3, 5, 7, 9, 11, 13, 15

"------------ EXIT ------------
DATA str_f TYPE string.
DO 15 TIMES.
  "Terminating the entire loop
  IF sy-index = 7.
    EXIT.
  ELSE.
    str_f = |{ str_f }{ COND #( WHEN str_f IS NOT INITIAL THEN `, ` ) }{ sy-index }|.
  ENDIF.
ENDDO.
"str_f: 1, 2, 3, 4, 5, 6
```


> **💡 Note**<br>
> - [`RETURN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapreturn.htm) statements immediately terminate the current processing block. However, according to the [guidelines (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexit_procedure_guidl.htm), `RETURN` should only be used to exit procedures like methods.
> - `EXIT` and `CHECK` might also be used for exiting procedures. However, their use inside loops is recommended.

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### `WHILE`: Conditional Loops

- Conditional loops introduced by `WHILE` and ended by `ENDWHILE` are repeated as long as a logical expression is true.
- These loops can also be exited using the statements mentioned above.
- Like in `DO` loops, the system field `sy-index` contains the number of previous loop passes including the current pass.
- Also check the conditional loops options using `FOR ... WHILE` with a constructor expression in the [Constructor Expressions](05_Constructor_Expressions.md) cheat sheet.

```abap
DATA int_itab TYPE TABLE OF i WITH EMPTY KEY.

WHILE lines( int_itab ) = 5.
  int_itab = VALUE #( BASE int_itab ( sy-index ) ).
ENDWHILE.

"Content of int_itab:
"1
"2
"3
"4
"5

"The following string replacement example uses a WHILE
"statement for demo purposes - instead of using a REPLACE ALL 
"OCCURRENCES statement or the replace function to replace all 
"occurrences. The WHILE loop exits when there are no more '#' 
"characters to be replaced in the string. The value of the data 
"object, which is checked in the logical expression, is then set 
"to make the logical expression false.
DATA(str_to_replace) = `Lorem#ipsum#dolor#sit#amet`.
DATA(subrc) = 0.
DATA(counter) = 0.
WHILE subrc = 0.
  REPLACE `#` IN str_to_replace WITH ` `.
  IF sy-subrc <> 0.
    subrc = 1.
  ELSE.
    counter += 1.
  ENDIF.
ENDWHILE.

"str_to_replace: Lorem ipsum dolor sit amet
"counter: 4
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Loops Across Tables
Further keywords for defining loops are as follows. They are not dealt with here since they are covered in other ABAP cheat sheets.

- [`LOOP ... ENDLOOP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab.htm) statements are meant for loops across internal tables. See also the cheat sheet on internal tables.
  - In contrast to the loops above, the system field `sy-index` is not set. Instead, the system field `sy-tabix` is set and which contains the table index of the current table line in the loop pass.
  - You can also realize loops using iteration expressions with `VALUE` and `REDUCE`. For more information, refer to the [Constructor Expressions](05_Constructor_Expressions.md) cheat sheet.
- [`SELECT ... ENDSELECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect.htm) statements loop across the result set of a database access. See also the cheat sheet on ABAP SQL.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Calling Procedures

[Procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm) can be explicitly called within an [ABAP program](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm), thereby influencing the program flow logic.

### Methods of Classes

In modern ABAP programs, only methods should be implemented (instead of [function modules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfunction_module_glosry.htm) and [subroutines](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubroutine_glosry.htm) in most cases).
Note that methods and calling methods are described in the context of the [ABAP Object Orientation cheat sheet](04_ABAP_Object_Orientation.md). Find more information and examples there.

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Function Modules

> **💡 Note**<br>
> In [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm), function modules can technically be used, but they are not recommended for new implementations. Many features available in standard ABAP, such as various includes, are not compatible with ABAP for Cloud Development (for example, [dynpro](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynpro_glosry.htm)-related functionality).

Function modules ... 
- are reusable cross-program procedures (i.e. processing blocks callable via an ABAP statement).
- are organized and implemented within function pools.
- are the precursor technology of public methods in global classes.
- are implemented (i.e. their functionality is implemented) between the following statements:
  ```abap
  FUNCTION ... .
    ...
  ENDFUNCTION.
  ```
- have a parameter interface that's similar to ABAP classes. Note: In ADT, the parameter interface of a function module is defined in ABAP pseudo syntax. These statements are not compiled like genuine ABAP statements and are not subject to the regular ABAP syntax checks. Find more information on the parameter inteface in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfunction.htm). 
- are called using `CALL FUNCTION` statements.

Function pools ...
- serve as a framework for function modules and are organized in include programs. 
  - The main program is implicitly created, while subordinate include programs are automatically generated, each with specific prefixes and suffixes.
- can contain multiple function modules. They can hold up to 99 function modules.
- are loaded when one of its function modules is called.
- are introduced by the `FUNCTION-POOL` statement. 
    
#### Calling Function Modules

Syntax to call function modules:
```abap
CALL FUNCTION func params.
```

- `func`: Character-like data object (for example, a literal) that contains the name of a function module in uppercase letters
    - Since all function modules have unique names, there is no need to specify the function pool.
- `params`: Parameter list or table 
- Incorrectly provided function module names or parameters are not checked until runtime
- Unlike method calls, you cannot specify inline declarations as actual parameters.
- Regarding dynamic function module calls: Static and dynamic function module calls are syntactically identical. In a static call, the function module is specified as a character literal or a constant, with parameters passed statically. Conversely, in a dynamic call, the function module's name is specified in a variable, with parameters passed dynamically. For dynamic calls, you can utilize the `CL_ABAP_DYN_PRG` as shown in the [Misc ABAP Classes cheat sheet](22_Misc_ABAP_Classes.md). 
- When a function module call is made, the system field `sy-subrc` is set to 0. If a non-class-based exception is raised and a value is assigned to handle it, this value updates `sy-subrc`.

Example function module calls with parameter passing and exception handling: 
```abap
"Handling non-class-based exception
DATA it TYPE some_table_type.
CALL FUNCTION 'SOME_FUNCTION_MODULE_A'
  EXPORTING
    param_a = 'somevalue'
  IMPORTING
    param_b = it
  EXCEPTIONS
    not_found = 4.

IF sy-subrc <> 0.
   ...
ENDIF.

"Handling class-based exception
TRY.
    CALL FUNCTION 'SOME_FUNCTION_MODULE_B'
      EXPORTING
          param_c = 'somevalue'
      IMPORTING
          param_d = it.
  CATCH cx_some_exception INTO DATA(exc).
    ...
ENDTRY.

"---------- Dynamic function method calls ----------

"Function module name contained in a variable
DATA(func_name) = 'SOME_FUNCTION_MODULE_C'.

CALL FUNCTION func_name ...

"For parameters in a parameter table, use the addition ... PARAMETER-TABLE ptab ...
"ptab: Sorted table of type abap_func_parmbind_tab (line type abap_func_parmbind)
"For exceptions, use the addition ... EXCEPTION-TABLE ...
DATA(ptab) = VALUE abap_func_parmbind_tab( ... ).

CALL FUNCTION func_name PARAMETER-TABLE ptab.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Function Module Example
Expand the following section to get a simple executable example:

<details>
  <summary>Expand to view the details</summary>
  <!-- -->

The following example demonstrates a simple function module: 
- The implementation in the function module represents a calculator. In the function module call, you specify two numbers and an operator. 
- The example includes static function module calls, and a dynamic function module call using the `PARAMETER-TABLE` addition.

To get started quickly with copiable code snippets, you can proceed as follows.
1. Create a function pool. 
   - In ADT, you can, for example, right-click the package in which you want to create the function pool and module.
   - Choose *New -> Other ABAP Repository Object*.
   - In the input field, enter *function*, select *ABAP Function Group* (which is the function pool), and choose *Next*.
   - Make entries in the *Name* (e.g. `Z_DEMO_ABAP_TEST_FUNC_P`) and *Description Field* (e.g. *Demo function group*) fields, and choose *Next*.
   - If prompted, select a transport request and choose *Finish*.
2. Create a function module.
   - In ADT, you can, for example, right-click the package in which you have created the function pool.
   - Choose *New -> Other ABAP Repository Object*.
   - In the input field, enter *function*, select *ABAP Function Module*, and choose *Next*.
   - Make entries in the *Name* (e.g. `Z_DEMO_ABAP_TEST_FUNC_M`), *Description Field* (e.g. *Demo function module*), and *Function Group* (e.g. the previously created `Z_DEMO_ABAP_TEST_FUNC_P`) fields and choose *Next*.
   - If prompted, select a transport request and choose *Finish*.
   - The function module is created and can be filled with an implementation.
   - You can copy and paste the code below to have a sample implementation.
3. To demonstrate the function module, you can create a class, for example, with the name `ZCL_DEMO_ABAP_FUNC_TEST`, and copy and paste the code below. 
   - In ADT, you can run the class by choosing *F9*. Some output is displayed in the ADT console, demonstrating the result of function module calls.


Code for the function module `Z_DEMO_ABAP_TEST_FUNC_M`:

```abap
FUNCTION z_demo_abap_test_func_m
  IMPORTING
    num1 TYPE i
    operator TYPE string
    num2 TYPE i
  EXPORTING
    result TYPE string
  RAISING
    cx_sy_arithmetic_error.





  "ABAP 'allows' zero division if both operands are 0.
  IF num1 = 0 AND num2 = 0.
    RAISE EXCEPTION TYPE cx_sy_zerodivide.
  ENDIF.

  DATA op TYPE c LENGTH 1.
  op = condense( val = operator to = `` ).

  result = SWITCH #( op
                     WHEN '+' THEN |{ num1 } + { num2 } = { num1 + num2 STYLE = SIMPLE }|
                     WHEN '-' THEN |{ num1 } - { num2 } = { num1 - num2 STYLE = SIMPLE }|
                     WHEN '*' THEN |{ num1 } * { num2 } = { num1 * num2 STYLE = SIMPLE }|
                     WHEN '/' THEN |{ num1 } / { num2 } = { CONV decfloat34( num1 / num2 ) STYLE = SIMPLE }|
                     ELSE `Use one of the operators + - * /` ).

ENDFUNCTION.
```

Code for the class `ZCL_DEMO_ABAP_FUNC_TEST` that can be run in ADT choosing *F9*:

```abap
CLASS zcl_demo_abap_func_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_func_test IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Calling a function module
    "For handling a possible wrong operator in the example, you may want to implement
    "a separate exception. The simple example just catches calculation errors
    "and uses a data object of type string for storing the calculation result.
    DATA calculation_result TYPE string.

    TRY.
        CALL FUNCTION 'Z_DEMO_ABAP_TEST_FUNC_M'
          EXPORTING
            num1     = 1
            operator = `+`
            num2     = 2
          IMPORTING
            result   = calculation_result.
      CATCH cx_sy_arithmetic_error INTO DATA(exc).
        calculation_result = exc->get_text( ).
    ENDTRY.

    out->write( calculation_result && |\n\n| ).

    "More calculation examples; calculations are stored in a table
    DATA calculation_result_table TYPE string_table.

    TYPES: BEGIN OF s,
             num1     TYPE i,
             operator TYPE string,
             num2     TYPE i,
           END OF s,
           it_type TYPE TABLE OF s WITH EMPTY KEY.
    DATA(itab) = VALUE it_type( ( num1 = 10 operator = `-` num2 = 12 )
                                ( num1 = 15 operator = `*` num2 = 4 )
                                ( num1 = 7 operator = `/` num2 = 2 )
                                ( num1 = 1 operator = `/` num2 = 0 )
                                ( num1 = 0 operator = `/` num2 = 0 )
                                ( num1 = 9999999 operator = `*` num2 = 9999999 ) ).

    LOOP AT itab INTO DATA(wa).
      TRY.
          CALL FUNCTION 'Z_DEMO_ABAP_TEST_FUNC_M'
            EXPORTING
              num1     = wa-num1
              operator = wa-operator
              num2     = wa-num2
            IMPORTING
              result   = calculation_result.
        CATCH cx_sy_arithmetic_error INTO exc.
          calculation_result = |{ wa-num1 } { wa-operator } { wa-num2 } -> Error: { exc->get_text( ) }|.
      ENDTRY.
      APPEND calculation_result TO calculation_result_table.
    ENDLOOP.

    out->write( calculation_result_table ).
    out->write( |\n\n\n| ).

    "----- Dynamic function module call ----

    DATA(func_name) = 'Z_DEMO_ABAP_TEST_FUNC_M'.
    DATA(ptab) = VALUE abap_func_parmbind_tab( ( name  = 'NUM1'
                                                 kind  = abap_func_exporting
                                                 value = NEW i( 3 ) )
                                               ( name  = 'OPERATOR'
                                                 kind  = abap_func_exporting
                                                 value = NEW string( `+` ) )
                                               ( name  = 'NUM2'
                                                 kind  = abap_func_exporting
                                                 value = NEW i( 5 ) )
                                               ( name  = 'RESULT'
                                                 kind  = abap_func_importing
                                                 value = NEW string( ) ) ).

    CALL FUNCTION func_name PARAMETER-TABLE ptab.

    out->write( data = ptab name = `ptab` ).
  ENDMETHOD.

ENDCLASS.
```

</details>

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Special Function Modules in Standard ABAP
Special function modules exist in [Standard ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_abap_glosry.htm) (and not in ABAP for Cloud Development), and for which special properties are specified in the [Function Builder](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfunction_builder_glosry.htm):

- Update function modules: 
  - Typically contain modifying database accesses and can be used to register for later execution
  - Are called with `CALL FUNCTION ... IN UPDATE TASK`
  - Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcall_function_update.htm) (note that the links in this section refer to the ABAP Keyword Documentation for Standard ABAP) and in the SAP LUW cheat sheet
- Remote function calls (RFC):
  - Remote-enabled function modules are called using the [RFC interface](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenrfc_interface_glosry.htm)
  - You can make these calls within the same system or a different one, determined by an [RFC destination](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenrfc_dest_glosry.htm). Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenrfc.htm)
  - The calls can be ...
    - synchronous ([sRFC](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensrfc_glosry.htm)): The calling program waits for the remote function to finish processing; called using `CALL FUNCTION ... DESTINATION` 
    - asynchronous ([aRFC](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenarfc_glosry.htm)): A remote function call that proceeds without waiting for the remotely called function to finish processing; called using `CALL FUNCTION ... STARTING NEW TASK`      
    - transactional 
      - transactional calls ([tRFC](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentrfc_2_glosry.htm)) are related to the concept of the SAP LUW. tRFC is considered obsolete.
      - Successor technology: Background RFC ([bgRFC](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenbgrfc_glosry.htm)), executed with the statement `CALL FUNCTION ... IN BACKGROUND UNIT`. Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcall_function_background_unit.htm).
      - The newer background Processing Framework ([bgPF](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenbgpf_glosry.htm)) encapsulates bgRFC to execute time-consuming methods asynchronously. Find more information [here](https://help.sap.com/docs/abap-cloud/abap-concepts/background-processing-framework).
 
<p align="right"><a href="#top">⬆️ back to top</a></p>

### Subroutines in Standard ABAP

- Obsolete procedures you may find in older ABAP programs. Before ABAP Objects was introduced, subroutines were mainly used for local modularization.
- They are implemented between the statements `FORM` and `ENDFORM`.
- Called using `PERFORM` statements
- Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_subroutines.htm). The [SAP LUW cheat sheet example](17_SAP_LUW.md) also uses subroutines in the context of an SAP LUW (these subroutines are called using `PERFORM ... ON COMMIT` and `... ROLLBACK`).


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Excursion: RETURN

Regarding the exiting of procedures, note the hint mentioned above. The use of `RETURN` is recommended.

`RETURN` terminates the current processing block. Usually, the statement is intended for leaving processing blocks early. 
In case of functional method, i.e. methods that have one returning parameter, the `RETURN` statement can also be specified with an expression. In doing so, the 
following statement
```abap
res = some_expr.
RETURN.
```
can also be specified as follows: 
```abap
RETURN some_expr.
```
Here, the expression result is passed to the returning parameter without naming it explicitly. As an expression, you can specify a constructor expression (and using type inference with `#` means using the type of the returning parameter).

Example:
```abap
CLASS zcl_some_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS multiply
      IMPORTING num1          TYPE i
                num2          TYPE i
      RETURNING VALUE(result) TYPE i.
ENDCLASS.
CLASS zcl_some_class IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA(res1) = multiply( num1 = 2 num2 = 3 ).
    DATA(res2) = multiply( num1 = 10 num2 = 10 ).
    DATA(res3) = multiply( num1 = 99999999 num2 = 99999999 ).
    out->write( res1 ). "6
    out->write( res2 ). "100
    out->write( res3 ). "0
  ENDMETHOD.
  METHOD multiply.
    TRY.        
        "result = num1 * num2.
        RETURN num1 * num2.
      CATCH cx_sy_arithmetic_error.
        RETURN VALUE #( ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Interrupting the Program Execution

Using [`WAIT UP TO`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapwait_up_to.htm) statements, you can interrupt the program execution by a specified number of seconds.

```abap
"First retrieval of the current time stamp
DATA(ts1) = utclong_current( ).
...
WAIT UP TO 1 SECONDS.
...
WAIT UP TO 3 SECONDS.
...
"Second retrieval of the current time stamp after the WAIT statements
DATA(ts2) = utclong_current( ).
"Calculating the difference of the two time stamps
cl_abap_utclong=>diff( EXPORTING high     = ts2
                                 low      = ts1
                        IMPORTING seconds = DATA(seconds) ).

"The value of the 'seconds' data object holding the delta of the time stamps 
"should be greater than 4.
ASSERT seconds > 4.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Handling Exceptions
- [Exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenexception_glosry.htm) ...
  - are events during the execution of an ABAP program that interrupt the program flow because it is not possible for the program to continue in a meaningful way. For such situations, you can implement an exception handling in which you can react on the situations appropriately. Consider, for example, the implementation of a simple calculation. If there is a division by zero, the program will be terminated with a [runtime error](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenruntime_error_glosry.htm) unless you handle the exception appropriately.
  - can be raised either by the program or by the [ABAP runtime framework](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_runtime_frmwk_glosry.htm). Exceptions raised by the latter are generally caused by error situations that cannot be detected by the static program check. The division by zero is such an example.
  - should, in modern ABAP, only be designed as [class-based exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_based_exception_glosry.htm), i. e. exceptions are represented by [objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_glosry.htm) of classes. Global exception classes usually use the naming convention `CX_...`.
  - are either [catchable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencatchable_exception_glosry.htm)  (they are based on predefined or self-defined exception classes) or [uncatchable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenuncatchable_exception_glosry.htm) (they directly produce runtime errors, i. e. error situations cannot be handled appropriately).

`TRY` control structures are meant for handling catchable exceptions locally:
- To be prepared for potential exceptions that are raised when executing statements, the statements can be included and executed within a *protected area*, a [`TRY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptry.htm) control structure.
- In doing so, it is possible for the ABAP runtime framework to catch exceptions and react on error situations.

  ```abap
  TRY.
      "statement block
  ENDTRY.
  ```

- The `TRY` control structure in the snippet above produces a syntax warning. A [`CATCH`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcatch_try.htm) block is expected, too.
- One or more class-based exceptions can be handled in one or more subsequent `CATCH` blocks. The `CATCH` statement must include an "appropriate" class-based exception. "Appropriate" means that, certainly, an exception class should be specified that is suitable for the error handling. In the following example, the predefined exception class `CX_SY_ZERODIVIDE` is specified that is, as the name implies, specific for the (potential) exception in case of a division by zero.

  ```abap
  TRY.
    "TRY block
    DATA(div1) = 1 / 0.

    "Predefined exception class cx_sy_zerodivide as suitable exception class to be used here.
    "If the exception is not handled, the program is terminated and the runtime error
    "COMPUTE_INT_ZERODIVIDE occurs.
    CATCH cx_sy_zerodivide.
      ... "CATCH block
  ENDTRY.

  "Example for catching an exception in the context of a table expression
  TRY.
    "Copying a line of an internal table
    DATA(line) = some_itab[ 12345 ].

    "Predefined exception class cx_sy_itab_line_not_found as suitable exception class to be used here.
    "If the exception is not handled, the program is terminated and the runtime error
    "ITAB_LINE_NOT_FOUND occurs.
    CATCH cx_sy_itab_line_not_found.
      ... "CATCH block
  ENDTRY.

  "Note on inheritance relationships in exception classes
  TRY.
    "TRY block
    DATA(div2) = 1 / 0.

    "A CATCH block is in this example not only valid for cx_sy_zerodivide as specified above
    "but also for all derived exceptions classes.
    "In the following CATCH block, the predefined exception class cx_sy_arithmetic_error
    "is specified. cx_sy_zerodivide is derived from cx_sy_arithmetic_error.
    "Hence, cx_sy_arithmetic_error can be specified and handle the exception, too.
    "Basically, using the exception root class cx_root would be also possible. However, 
    "choosing an appropriate exception class is recommended. See further down.

    CATCH cx_sy_arithmetic_error.
      ... "CATCH block
  ENDTRY.


  "Multiple classes in a list and CATCH blocks can be specified
  "Note: If there are multiple CATCH blocks for exceptions that are in an inheritance
  "relationship, you must pay attention that the more special exceptions are specified
  "before the more general ones.
  TRY.
      ... "TRY block
    CATCH cx_abc cx_bla cx_blabla.
      ... "CATCH block
    CATCH cx_la cx_lala.
      ... "CATCH block
    CATCH cx_lalala.
      ... "CATCH block
  ENDTRY.
  ```

- If the addition `INTO` is specified in the `CATCH` statement, a reference to the exception object is stored.
- This is relevant to determine the exact exception, for example. In the code snippet above, the exception class `CX_SY_ZERODIVIDE` is mentioned. Consider a calculator. It should not only be able to deal with error situations like zero division but also, for example, overflows in arithmetic operations. The predefined exception class `CX_SY_ARITHMETIC_OVERFLOW` is available. It is also derived from `CX_SY_ARITHMETIC_ERROR`. If you then specify the exception class `CX_SY_ARITHMETIC_ERROR` which is higher up in the inheritance hierarchy and can handle both error situations (`CX_SY_ARITHMETIC_OVERFLOW` and `CX_SY_ZERODIVIDE`), the concrete exception that was raised is unclear. Using the `INTO` clause and the stored exception object, it is possible to carry out certain tasks, for example, retrieving and displaying the exception text.

  ```abap
  DATA: exception TYPE REF TO cx_root. "Note the root class
  "Note: For a self-defined exception class, the object reference must be typed appropriately.

  TRY.
      ... "TRY block

    "Storing a reference to the exception object.
    "Note: The type is cx_root since attributes and methods of the root class that are defined there can be accessed.
    CATCH INTO exception.
      ... "CATCH block
  ENDTRY.

  "Inline creation of exception object reference and getting exception texts
  TRY.
      ... "TRY block

    "The object reference variable can be created inline, for example, using DATA(...).
    CATCH cx_sy_arithmetic_error INTO DATA(error_oref).
      ... "catch block
      "To get exception texts, you can call, for example, the method get_text
      DATA(error_text) = error_oref->get_text( ).

  ENDTRY.
  ```

- Regarding the program flow:
  - The statement block following `TRY.` is always processed. If an exception is raised within this `TRY` block, the system searches for an exception handler, i. e. a `CATCH` block that is able to handle the exception.
  - If there is no `CATCH` statement that is able to handle the catchable exception or if erroneous code is not within a `TRY` control structure at all, the exception is propagated to the caller.
     - Exceptions can be handled either in the context locally (using such a `TRY` control structure) or be propagated to the caller so that the caller is responsible for reacting appropriately (for example, in another `TRY` control structure) to the error situation. In doing so, you can better structure your code by reacting on error situations centrally instead of locally checking, for example, each procedure call individually.
  - If, at the end, the exception can nowhere be caught and handled, the program is terminated with a runtime error.
  - If the exception can be handled or no exception is raised in the `TRY` block and it reaches its end, the processing continues after `ENDTRY`.


> **💡 Note**<br>
> - Non-class-based exceptions are considered obsolete and should not be defined any more in new developments according to the [guidelines (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclass_exception_guidl.htm) and are not dealt with here.
>- For all exceptions that are raised by the ABAP runtime environment and that are not handled, there is a corresponding runtime error. For example, in the case of exception class `CX_SY_ZERODIVIDE`, it is the runtime error `COMPUTE_INT_ZERODIVIDE`. For self-defined exception classes, an exception that is not handled generally triggers the runtime error `UNCAUGHT_EXCEPTION`.
> - For `TRY` control structures, there are further additions available dealing with more advanced error handling, e. g. [resumable exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapresume.htm).

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Notes on Exception Classes
- To distinguish exception classes from *regular* classes, use the naming convention `CX` as prefix and not `CL`.
- All exception classes (also the self-defined ones) are directly or indirectly derived from three abstract subclasses: `CX_STATIC_CHECK`, `CX_DYNAMIC_CHECK` and `CX_NO_CHECK`. These three "exception class categories" have different properties.
- The class `CX_ROOT` is the root class. Directly deriving from `CX_ROOT` is not possible.
- Apart from global classes, exception classes can also be defined as local classes within an ABAP program.
- As mentioned, there are predefined exception classes like `CX_SY_ZERODIVIDE` for divisions by zero. However, you can create your own exception classes so that you can react on issues that are specific to your ABAP program. The exception class must, as stated above, be derived from one of the three abstract classes:
  - `CX_STATIC_CHECK`: For forcing users to handle exceptions.
  Generally speaking, exceptions that can occur in procedures should be handled locally there in the implementation or be declared explicitly in the [procedure interface](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenparameter_interface_glosry.htm) so that a caller knows which error situations can be expected. Exception classes of type `CX_STATIC_CHECK` enforce this. A check is carried out statically at compile time. Users of a procedure are then forced to either handle the exception locally in a `TRY` control structure or the users declare the exception themselves in their procedure interface to propagate the exception to their users. If this is not the case, a warning is produced.
      - As an example, a method signature might look as follows. The addition `RAISING` in method signatures is used to declare one or more class-based exceptions that can be propagated from the method to the caller.
      - When users of this method implement the method, they are made aware of the fact that an error situation can occur and a particular exception can be raised. The assumption is that `CX_SOME_ERROR` is derived from `CX_STATIC_CHECK`. Users of the method should then prepare the code accordingly.
      ```abap
      "Method definition using the RAISING parameter
      CLASS-METHODS: some_meth IMPORTING str TYPE string
                               RETURNING VALUE(ret_value) TYPE string
                               RAISING cx_some_error.

       ...

       "Method call: Somewhere in the code of a user that calls the method
       "Exception handled locally in a TRY control structure.
       TRY.
        DATA(val) = some_meth( str = `hallo` ).

        CATCH cx_some_error.
        ...
      ENDTRY.

      "If it was just like this without the TRY control structure, a warning would be produced.
      DATA(val2) = some_meth( str = `hi` ).
      ```

  - `CX_DYNAMIC_CHECK`: For exceptions that can be checked and avoided by preconditions. As a consequence and in contrast to an exception class derived from `CX_STATIC_CHECK`, exception classes of type `CX_DYNAMIC_CHECK` do not enforce the local handling and the declaration in procedure interfaces. However, an appropriate exception handling should be implemented in cases in which you cannot rule out the raising of the exceptions in your program logic. The checking if a local handling or an explicit declaration in procedure interfaces is available is carried out at runtime only ("dynamic check") and only in case the exception is indeed raised.
    - If it is determined at runtime that such an exception is neither locally handled nor an interface is declared appropriately - and the exception is raised - a new exception of type `CX_SY_NO_HANDLER` is raised. In this case, the attribute `PREVIOUS` contains a reference to the original exception.
    - Example: The predefined class `CX_SY_ZERODIVIDE` is derived from `CX_DYNAMIC_CHECK`. The operands of a calculation can be checked appropriately (e. g. in case of a division, the implementation should guarantee that the second operand is not 0) before carrying out the arithmetic operation. In doing so, the exception can be avoided.

  - `CX_NO_CHECK`: For error situations that can basically occur any time, cannot be locally handled in a meaningful way or cannot be avoided even following a check. An example for such an error situation might be a lack of memory. If the handling of such exceptions was checked statically or dynamically, it would basically mean to specify it in each procedure interface - not ideal for a clear program structuring.
    - Note that exceptions derived from `CX_NO_CHECK` are always declared implicitly in all procedure interfaces.

**Basic rule**: [Use a suitable exception category (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexception_category_guidl.htm).

> **💡 Note**<br>
> - Each exception has a an [exception text](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexception_text_glosry.htm) that describes the error situation and that you can retrieve as outlined above. It helps you analyze the error. Plus, imagine using exceptions in the context of user interfaces. If a user faces an error situation, such exception texts may be displayed on the UI.
> - Find more information on exception texts [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexception_texts.htm) in the ABAP Keyword Documentation.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Raising Exceptions

- Either the ABAP runtime framework raises predefined exceptions or you raise exceptions programmatically using dedicated statements. You can raise both predefined and self-defined exceptions.
- As the name implies, [`RAISE EXCEPTION`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapraise_exception_class.htm) statements raise class-based exceptions and thus interrupt the execution of the current statement block.
- The `COND` operator includes the optional addition `THROW` to raise class-based exceptions.

```abap
...
"RAISE EXCEPTION statement
"The TYPE addition specifies the type of the exception, i. e. the exception class.
"The statement is also possible without TYPE. In that case, you can use an existing exception object.
"Note that there are plenty of additions. Check the ABAP Keyword Documentation.
RAISE EXCEPTION TYPE cx_sy_zerodivide.
...

"THROW addition for the COND operator
... = COND #( WHEN ... THEN ...
              WHEN ... THEN ...
              ELSE THROW cx_some_error( ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursion: Runtime Errors and Terminating Programs
- Runtime errors are caused by uncatchable exceptions when a program is executed, when a catchable exception is not caught, or they can be forced by, for example, using [`ASSERT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapassert.htm) statements.
- Every runtime error terminates the program, which in turn raises a [database rollback](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendatabase_rollback_glosry.htm) and is documented by default in a [short dump](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenshort_dump_glosry.htm).

- Regarding `ASSERT` statements: `ASSERT` is followed by a logical expression. If the expression is false, the program is terminated and an uncatchable exception is raised resulting in the runtime error `ASSERTION_FAILED`. Note that each runtime error is identified by a name and assigned to a specific error situation.

```abap
"The ASSERT keyword is followed by a logical expression.
"If the expression is false, the program is terminated and an uncatchable exception is raised
"resulting in the runtime error ASSERTION_FAILED.

DATA(number) = 0.
ASSERT number IS INITIAL.
ASSERT number > -1.
ASSERT 1 = 1.

DATA(flag) = abap_false.
"Raises a runtime error
ASSERT flag = abap_true.
```

> **💡 Note**<br>
> - Each runtime error is identified by a name and assigned to a specific error situation.
> - In ADT, you will see a message popping up and informing you about the runtime error. You can check the details by choosing the "Show" button in the pop-up. Furthermore, you can check the content of the "Feed Reader" tab in ADT. There, just expand your project and find the runtime errors caused by you.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

[zcl_demo_abap_prog_flow_logic](./src/zcl_demo_abap_prog_flow_logic.clas.abap)

> **💡 Note**<br>
> - The executable example ...
>   - covers the following topics, among others:
>     - Control structures with `IF`, `CASE`, and `TRY`
>     - Excursions: `COND` and `SWITCH` operators 
>     - Expressions and functions for conditions
>     - Predicate expression with `IS SUPPLIED`
>     - Loops with `DO`, `WHILE`, and `LOOP`
>     - Terminating loop passes
>     - Handling exceptions
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)