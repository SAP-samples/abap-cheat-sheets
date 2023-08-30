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

The following code snippet shows a selection of possible expressions and operands of such expressions using a big `IF` statement. Certainly, such a huge statement is far from ideal. Here, the intention is to just cover many syntax options in one go for demonstration purposes.

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
AND boolc( check_is_supplied( ) ) = abap_true

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
"Method with exactly one return value
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

  "All of the logical expressions are true.

ELSE.

  "At least one of the logical expressions is false.

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
"IF statement with multiple included ELSEIF statements
IF log_exp1.

  ... "statement_block1

ELSEIF log_exp2.

  ... "statement_block2

ELSEIF log_exp3.

  ... "statement_block3

... "further ELSEIF statements

ELSE.

  ... "statement_blockn

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
> - If you are convinced that a specified logical expression must always be true, you might include a statement like `ASSERT 1 = 0` to go
>   sure - as implied in the example's `ELSE` statement above. However, an `ELSE` statement that is never executed might be a hint that
>   logical expressions might partly be redundant.

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Excursion: `COND` Operator

- The conditional operator [`COND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconditional_expression_cond.htm) can also be used to implement branches in operand positions that are based on logical expressions.
- Such conditional expressions have a result that is dependent on the logical expressions.
- The result's data type is specified after `COND` right before the first parenthesis. It can also be the `#` character as a symbol for the operand type if the type can be derived from the context. See the ABAP Keyword Documentation and the cheat sheet on constructor expressions for more information.
- All operands specified after `THEN` must be convertible to the result's data type.

```abap
... COND type( WHEN log_exp1 THEN result1
               WHEN log_exp2 THEN result2
               ...
               ELSE resultn ) ...
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
CASE operand.
  WHEN op1.
    ... "statement_block
  WHEN op2.
    ... "statement_block
  WHEN op3 OR op4.
    ... "statement_block
  WHEN OTHERS.
    ... "statement_block
ENDCASE.
```

Special control structure introduced by [`CASE TYPE OF`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcase_type.htm): Checks the type of object reference variables.

```abap
"oref must be an object reference variable with the static type of a class or an interface.
CASE TYPE OF oref.
  WHEN TYPE some_class.
    ... "statement_block
  WHEN TYPE some_intf.
    ... "statement_block
  WHEN OTHERS.
    ... "statement_block
ENDCASE.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Excursion: `SWITCH` Operator

The conditional operator [`SWITCH`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconditional_expression_switch.htm) can also be used to make case distinctions in operand positions. As mentioned above for `COND`, a result is constructed. The same criteria apply for `SWITCH` as for `COND` regarding the type. See the ABAP Keyword Documentation and the cheat sheet on constructor expressions for more information.


```abap
... SWITCH type( operand
                 WHEN const1 THEN result1
                 WHEN const2 THEN result2
                 ...
                 ELSE resultn ) ...
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Loops

#### `DO`: Unconditional Loops

- A statement block specified between `DO` and `ENDDO` is carried out multiple times.
- The loop is exited when a statement to terminate the loop is reached (`EXIT`, see further down). Otherwise, it is executed endlessly.

  ```abap
  DO.
    ... "statement_block
        "To be terminated with an EXIT statement.
  ENDDO.
  ```
- To restrict the loop passes, you can use the `TIMES` addition and specify the maximum number of loop passes.

  ```abap
  DO 5 TIMES.
    ... "statement_block
  ENDDO.
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

> **💡 Note**<br>
> - [`RETURN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapreturn.htm) statements immediately terminate the current processing block. However, according to the [guidelines (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexit_procedure_guidl.htm), `RETURN` should only be used to exit procedures like methods.
> - `EXIT` and `CHECK` might also be used for exiting procedures. However, their use inside loops is recommended.

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### `WHILE`: Conditional Loops

- Conditional loops introduced by `WHILE` and ended by `ENDWHILE` are repeated as long as a logical expression is true.
- These loops can also be exited using the statements mentioned above.
- Like in `DO` loops, the system field `sy-index` contains the number of previous loop passes including the current pass.

```abap
WHILE log_exp.
  ... "statement_block
ENDWHILE.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Loops Across Tables
Further keywords for defining loops are as follows. They are not dealt with here since they are touched on in other ABAP cheat sheets.

- [`LOOP ... ENDLOOP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaploop_at_itab.htm) statements are meant for loops across internal tables. See also the cheat sheet on internal tables.
  - In contrast to the loops above, the system field `sy-index` is not set. Instead, the system field `sy-tabix` is set and which contains the table index of the current table line in the loop pass.
  - You can also realize loops using iteration expressions with `VALUE` and `REDUCE`. See the example class for the internal table cheat sheet.
- [`SELECT ... ENDSELECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapselect.htm) statements loop across the result set of a database access. See also the cheat sheet on ABAP SQL.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Calling Procedures

Calling [procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm) would actually fit here in the context of dealing with program flow logic since they can be called explicitly in an [ABAP program](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm).
However, ...
1. in modern ABAP programs, only methods should be implemented (instead of [function modules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfunction_module_glosry.htm) and [subroutines](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubroutine_glosry.htm) in most cases) and
2. methods are described in the context of the ABAP cheat sheet on ABAP object orientation. Hence, see more details there.

Regarding the exiting of procedures, note the hint mentioned above. The use of `RETURN` is recommended.

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

DATA(flag) = abap_false.

ASSERT flag = abap_true.
```

> **💡 Note**<br>
> - Each runtime error is identified by a name and assigned to a specific error situation.
> - In ADT, you will see a message popping up and informing you about the runtime error. You can check the details by choosing the "Show" button in the pop-up. Furthermore, you can check the content of the "Feed Reader" tab in ADT. There, just expand your project and find the runtime errors caused by you.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

[zcl_demo_abap_prog_flow_logic](./src/zcl_demo_abap_prog_flow_logic.clas.abap)

Note the steps outlined [here](README.md#-getting-started-with-the-examples) about how to import and run the code.
