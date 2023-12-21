***********************************************************************
*
*              ABAP cheat sheet: Program flow logic
*
* -------------------------- PURPOSE ----------------------------------
* - Demonstration example for the topic program flow logic covering
*   the following:
* - Expressions and functions for conditions
* - Control structures with IF and CASE, including the COND and SWITCH
*   operators
* - Unconditional loops with DO
* - Conditional loops with WHILE
* - Handling exceptions
*
* ----------------------- GETTING STARTED -----------------------------
* - Open the class with the ABAP development tools for Eclipse (ADT).
* - Choose F9 to run the class.
* - Check the console output.
* - To understand the context and the ABAP syntax used, refer to the
*   notes included in the class as comments or refer to the respective
*   topic in the ABAP Keyword Documentation.
* - Due to the amount of console output, the examples contain numbers
*   (e.g. 1) ..., 2) ..., 3) ...) for the individual example sections.
*   Also, the variable name is displayed in most cases. So to find
*   the relevant output in the console easier and faster, just search
*   for the number/variable name in the console (CTRL+F in the console)
*   or use the debugger.
*
* ----------------------------- NOTE -----------------------------------
* The code presented in this class is intended only to support the ABAP
* cheat sheets. It is not intended for direct use in a production system
* environment. The code examples in the ABAP cheat sheets are primarily
* intended to provide a better explanation and visualization of the
* syntax and semantics of ABAP statements, not to solve concrete
* programming tasks. For production application programs, you should
* always work out your own solution for each individual case. There is
* no guarantee for the correctness or completeness of the code.
* Furthermore, there is no legal responsibility or liability for any
* errors or their consequences that may occur when using the the example
* code.
*
***********************************************************************
"! <p class="shorttext synchronized">ABAP cheat sheet: Program flow logic</p>
"! Example to demonstrate program flow logic.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_prog_flow_logic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.

    "Structured type for calculation example
    TYPES: BEGIN OF calc_results_struc,
             sy_tabix    LIKE sy-tabix,
             calculation TYPE string,
             res_if      TYPE string,
             res_case    TYPE string,
             res_cond    TYPE string,
             res_switch  TYPE string,
           END OF calc_results_struc.

    CLASS-METHODS:
      check_is_supplied IMPORTING num1       TYPE i DEFAULT 0
                                  num2       TYPE i DEFAULT 0
                        RETURNING VALUE(res) TYPE string_table,

      addition IMPORTING num1       TYPE i DEFAULT 0
                         num2       TYPE i DEFAULT 0
               RETURNING VALUE(res) TYPE i,

      calc IMPORTING num1       TYPE i DEFAULT 0
                     operator   TYPE c
                     num2       TYPE i DEFAULT 0
           RETURNING VALUE(res) TYPE calc_results_struc,

      validate_email IMPORTING email                 TYPE string
                     RETURNING VALUE(is_valid_email) TYPE abap_bool
                     RAISING   lcx_invalid_email,

      meth_with_return IMPORTING num        TYPE i
                       RETURNING VALUE(res) TYPE string,

      whats_my_user IMPORTING get_name    TYPE abap_bool
                    RETURNING VALUE(name) TYPE string
                    RAISING   lcx_static_exc_class,

      prep_calc_result CHANGING VALUE(res) TYPE string,

      power2_and_sqrt IMPORTING num           TYPE i
                      RETURNING VALUE(result) TYPE string
                      RAISING   cx_sy_arithmetic_overflow.

    CLASS-DATA: exception_text TYPE string,
                exception      TYPE REF TO cx_root.

ENDCLASS.



CLASS zcl_demo_abap_prog_flow_logic IMPLEMENTATION.


  METHOD addition.
    res = num1 + num2.
  ENDMETHOD.


  METHOD calc.

    DATA calc_if TYPE string.
    DATA calc_case TYPE string.
    DATA calc_cond TYPE string.
    DATA calc_switch TYPE string.

    "IF statements

    IF operator = `+`.
      calc_if = num1 + num2.
    ELSEIF operator = `-`.
      calc_if = num1 - num2.
    ELSEIF operator = `*`.
      calc_if = num1 * num2.
    ELSEIF operator = `/`.

      IF num2 = 0.
        calc_if = `Division by 0`.
      ELSE.
        calc_if = num1 / num2.
      ENDIF.
    ELSE.
      calc_if = |Check the operator { operator }.|.
    ENDIF.

    prep_calc_result( CHANGING res = calc_if ).

    "CASE

    CASE operator.
      WHEN '+'.
        calc_case = num1 + num2.
      WHEN '-'.
        calc_case = num1 - num2.
      WHEN '*'.
        calc_case = num1 * num2.
      WHEN '/'.

        CASE num2.
          WHEN 0.
            calc_case = `Division by 0`.
          WHEN OTHERS.
            calc_case = num1 / num2.
        ENDCASE.

      WHEN OTHERS.
        calc_case = |Check the operator { operator }.|.
    ENDCASE.

    prep_calc_result( CHANGING res = calc_case ).

    "COND

    calc_cond = COND #( WHEN operator = '+'
                  THEN num1 + num2
                  WHEN operator = '-'
                  THEN num1 - num2
                  WHEN operator = '*'
                  THEN num1 * num2
                  WHEN operator = '/' AND num2 = 0 THEN `Division by 0`
                  WHEN operator = '/' AND num2 <> 0 THEN num1 / num2
                  ELSE |Check the operator { operator }.|
     ).

    prep_calc_result( CHANGING res = calc_cond ).

    "SWITCH

    calc_switch = SWITCH #( operator
                  WHEN '+' THEN num1 + num2
                  WHEN '-' THEN num1 - num2
                  WHEN '*' THEN num1 * num2
                  WHEN '/' THEN SWITCH #( num2 WHEN 0 THEN `Division by 0` ELSE num1 / num2 )
                  ELSE |Check the operator { operator }.| ).

    prep_calc_result( CHANGING res = calc_switch ).

    res = VALUE #(  calculation = |{ num1 } { operator } { num2 }|
      res_if = calc_if
      res_case = calc_case
      res_cond = calc_cond
      res_switch = calc_switch
      ).

  ENDMETHOD.


  METHOD check_is_supplied.
    IF num1 IS SUPPLIED.
      APPEND `num1 is supplied` TO res.
    ELSE.
      APPEND `num1 is not supplied` TO res.
    ENDIF.

    IF num2 IS NOT SUPPLIED.
      APPEND `num2 is not supplied` TO res.
    ELSE.
      APPEND `num2 is supplied` TO res.
    ENDIF.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: Program Flow Logic\n\n| ).

**********************************************************************

    out->write( |1) Control Structure with IF\n| ).

    "Simple control structure realized by an IF ... ELSEIF ... ELSE ... ENDIF.
    "statement. Multiple statement blocks can be included, of which only 1 is
    "executed at most and depending on conditions.

    "Determining some operators for a calculation
    DATA(operators) = VALUE string_table( ( `+` ) ( `-` ) ( `?` ) ).

    "Getting a random operator from the table
    DATA(idx) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( operators ) )->get_next( ).

    DATA(operator) = operators[ idx ].
    DATA(num1) = 5.
    DATA(num2) = 7.

    IF operator = `+`.
      out->write( |The result of { num1 } { operator } { num2 } is { num1 + num2 }. | ).
    ELSEIF operator = `-`.
      out->write( |The result of { num1 } { operator } { num2 } is { num1 - num2 }. | ).
    ELSE.
      out->write( |The operator { operator } is not possible.| ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) IF: Checking sy-subrc` ) ).

    "A prominent use case for IF statements: Checking sy-subrc.
    "In the case below, a FIND statement is used. If there is a finding,
    "sy-subrc has the value 0.

    DATA(to_be_found) = `AB`.
    DATA(string_to_search) = `ABAP is great!`.

    FIND to_be_found IN string_to_search.

    IF sy-subrc = 0.
      out->write( |'{ to_be_found }' was found in the string '{ string_to_search }'.| ).
    ELSE.
      out->write( |'{ to_be_found }' was not found in the string '{ string_to_search }'.| ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Excursion: COND Operator` ) ).

    "The conditional operator COND can also be used to implement branches in operand positions
    "that are based on logical expressions. Such conditional expressions have a result that
    "is dependent on the logical expressions.
    "The example provides an output based on the time stamp.

    DATA(syst_time) = cl_abap_context_info=>get_system_time( ).

    DATA(greetings) = COND #( WHEN syst_time BETWEEN '050001' AND '120000'
                              THEN |It's { syst_time TIME = ISO }. Good morning, { sy-uname }.|
                              WHEN syst_time BETWEEN '170001' AND '210000'
                              THEN |It's { syst_time TIME = ISO }. Good evening, { sy-uname }.|
                              WHEN syst_time BETWEEN '210001' AND '050000'
                              THEN |It's { syst_time TIME = ISO }. Good night, { sy-uname }.|
                              ELSE |It's { syst_time TIME = ISO }. Hallo, { sy-uname }.| ).

    out->write( data = greetings name = `greetings` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Expressions and Functions for Conditions` ) ).

    "Control structures are generally controlled by logical expressions that
    "define conditions for operands. The result of such an expression is either true or false.
    "The example demonstrates a selection of possible expressions and operands of such expressions
    "using a big IF statement. It includes multiple expressions combined by AND to demonstrate
    "different options. Here, it is just meant to cover many syntax options in one go
    "for demonstration purposes.

    "Data declarations to be used in the IF statement below
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

    "Comparisons
    IF 2 = num    "equal, alternative EQ
    AND 1 <> num  "not equal, alternative NE
    AND 1 < num   "less than, alternative LT
    AND 3 > num   "greater than, alternative GT
    AND 2 >= num  "greater equal, alternative GE
    AND 2 <= num  "less equal, alternative LE

    "Checks whether the content of an operand operand is within a closed interval
    AND num BETWEEN 1 AND 3
    AND NOT num BETWEEN 5 AND 7   "NOT negates a logical expression
    AND ( num >= 1 AND num <= 3 ) "Equivalent to 'num BETWEEN 1 AND 3';
                                  "here, demonstrating the use of parentheses

    "CO, CN ,CA, NA, CS, NS, CP, NP  "Comparison operators for character-like data types;
                                     "see the cheat sheet on string processing

    "Predicate Expressions
    AND empty_string IS INITIAL  "Checks whether the operand operand is initial. The expression
                                 "is true, if the operand contains its type-dependent initial value
    AND num IS NOT INITIAL       "NOT negates

    AND dref IS BOUND  "Checks whether a data reference variable contains a valid reference and
                       "can be dereferenced;
                       "IS NOT BOUND is possible which is valid for the following examples, too
    AND oref IS BOUND  "Checks whether an object reference variable contains a valid reference

    "IS INSTANCE OF checks whether for a
    "a) non-initial object reference variable the dynamic type
    "b) for an initial object reference variable the static type
    "is more specific or equal to a comparison type.
    AND oref IS INSTANCE OF zcl_demo_abap_prog_flow_logic
    AND oref IS INSTANCE OF if_oo_adt_classrun

    AND <fs> IS ASSIGNED  "Checks whether a memory area is assigned to a field symbol

    "The predicate expression IS SUPPLIED is dealt with further down.

    "Predicate Functions
    AND contains( val = <fs> pcre = `\D` )  "Checks whether a certain value is contained;
                                            "the example uses the pcre parameter for regular expressions;
                                            "it checks whether there is any non-digit character contained
    AND matches( val = <fs> pcre = `ha.+` ) "Compares a search range of the argument for the val parameter
                                            "the example uses the pcre parameter for regular expressions;
                                            "it checks whether the value matches the pattern
                                            "'ha' and a sequence of any characters

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
    "since the latter converts from c to string and ignores any blanks. Note: If the logical
    "expression is false, the result of boolc does not meet the condition IS INITIAL, since
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
      out->write( `All of the logical expressions are true.` ).
    ELSE.
      out->write( `At least one of the logical expressions is false.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Predicate Expression with IS SUPPLIED` ) ).

    "The predicate expression IS SUPPLIED is available in method implementations
    "and checks whether a formal parameter of a procedure is filled or requested.
    "In this example, a method includes two importing parameters that are both
    "declared as non-mandatory. The method implementation includes a check with
    "IS SUPPLIED and stores the result. The result is returned and displayed.

    DATA(is_supplied) = check_is_supplied( num1 = 123 ).

    out->write( data = is_supplied name = `is_supplied` ).
    out->write( |\n| ).

    is_supplied = check_is_supplied( num2 = 456 ).

    out->write( data = is_supplied name = `is_supplied` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) Control Structure with CASE` ) ).

    "CASE statements are used for case distinctions. If the content of an operand
    "specified after WHEN matches the content specified after CASE, the statement
    "block is executed. Constant values should be specified as operands.
    "The example is basically the same as above.

    "Determining some operators for a calculation
    operators = VALUE string_table( ( `+` ) ( `-` ) ( `#` ) ).

    "Getting a random operator from the table
    idx = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( operators ) )->get_next( ).

    DATA(op) = operators[ idx ].
    DATA(n1) = 8.
    DATA(n2) = 3.

    "Simple calculation

    CASE op.
      WHEN '+'.
        out->write( |The result of { n1 } { op } { n2 } is { n1 + n2 }. | ).
      WHEN '-'.
        out->write( |The result of { n1 } { op } { n2 } is { n1 - n2 }. | ).
      WHEN OTHERS.
        out->write( |The operator { op } is not possible.| ).
    ENDCASE.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) CASE TYPE OF` ) ).

    "CASE TYPE OF: Checks the type of object reference variables

    "Creating an object reference variable and an object
    DATA oref_check TYPE REF TO object.
    oref_check = NEW zcl_demo_abap_prog_flow_logic( ).

    CASE TYPE OF oref_check.
      WHEN TYPE zcl_demo_abap_prog_flow_logic.
        out->write( `Type zcl_demo_abap_prog_flow_logic? True!` ).
      WHEN TYPE if_oo_adt_classrun.
        out->write( `Type if_oo_adt_classrun? True!` ).
      WHEN TYPE zcl_demo_abap_sql.
        out->write( `Type zcl_demo_abap_sql? True!` ).
      WHEN OTHERS.
        out->write( `Other type.` ).
    ENDCASE.

    out->write( |\n| ).

    "The same logic as above is realized in the following IF statements
    "using IS INSTANCE OF.
    "In the example, the type check for if_oo_adt_classrun 'comes first'.
    "This type is also true for the object reference variable. This class
    "implements the interface.
    IF oref_check IS INSTANCE OF if_oo_adt_classrun.
      out->write( `Type if_oo_adt_classrun? True!` ).
    ELSEIF oref_check IS INSTANCE OF zcl_demo_abap_prog_flow_logic.
      out->write( `Type zcl_demo_abap_prog_flow_logic? True!` ).
    ELSEIF oref_check IS INSTANCE OF zcl_demo_abap_sql.
      out->write( `Type zcl_demo_abap_sql? True!` ).
    ELSE.
      out->write( `Other type.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Excursion: SWITCH Operator` ) ).

    "The conditional operator SWITCH can also be used to make case
    "distinctions in operand positions. Such conditional expressions have
    "a result that is dependent on the logical expressions.
    "The example provides an output based on the date.

    DATA(syst_date) = cl_abap_context_info=>get_system_date( ).

    DATA(switch_res) = SWITCH #( syst_date+4(2)  "Extracting the month
                                  WHEN '01' THEN `January`
                                  WHEN '02' THEN `February`
                                  WHEN '03' THEN `March`
                                  WHEN '04' THEN `April`
                                  WHEN '05' THEN `May`
                                  WHEN '06' THEN `June`
                                  WHEN '07' THEN `July`
                                  WHEN '08' THEN `August`
                                  WHEN '09' THEN `September`
                                  WHEN '10' THEN `October`
                                  WHEN '11' THEN `November`
                                  WHEN '12' THEN `December`
                                  ELSE `Oops ...` ).

    out->write( data = switch_res name = `switch_res` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Loops (Iterations)` ) ).

    out->write( |9) Unconditional Loops with DO\n| ).

    "The example demonstrate the restriction of loop passes by specifying
    "a number (of maximum loop passes) and the TIMES addition in a DO loop.
    "The value of sy-index containing the number of loop passes is output, too.

    DATA do_counter TYPE i.
    DATA do_sy_index TYPE string.

    DO 10 TIMES.
      do_counter += 1.

      do_sy_index = do_sy_index && sy-index && ` `.
    ENDDO.

    out->write( data = do_counter name = `do_counter` ).
    out->write( |\n| ).
    out->write( data = do_sy_index name = `do_sy_index` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) Terminating Loops Completely Using EXIT` ) ).

    "Using the EXIT statement, you can terminate a loop completely.
    "The program flow resumes after the closing statement of the loop.

    CLEAR: do_counter, do_sy_index.

    DO.
      do_counter += 1.

      do_sy_index = do_sy_index && sy-index && ` `.
      IF sy-index = 5.
        EXIT.
      ENDIF.
    ENDDO.

    out->write( data = do_counter name = `do_counter` ).
    out->write( |\n| ).
    out->write( data = do_sy_index name = `do_sy_index` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) Terminating Loop Passes` ) ).

    "CONTINUE: The current loop pass is terminated immediately and the
    "          program flow is continued with the next loop pass.
    "CHECK: Conditional termination. If the specified logical expression
    "       is false, the current loop pass is terminated immediately and
    "       the program flow is continued with the next loop pass.

    CLEAR: do_counter, do_sy_index.

    DO.
      IF sy-index = 2. "skipped
        CONTINUE.
      ENDIF.

      CHECK sy-index <> 5. "skipped

      IF sy-index = 8. "terminates the loop completely
        EXIT.
      ENDIF.

      do_counter += 1.
      do_sy_index = do_sy_index && sy-index && ` `.
    ENDDO.

    out->write( data = do_counter name = `do_counter` ).
    out->write( |\n| ).
    out->write( data = do_sy_index name = `do_sy_index` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `12) Excursion: Terminating Procedures Using RETURN` ) ).

    "RETURN statements immediately terminate the current processing block.
    "However, according to the guidelines, RETURN should only be used to exit
    "procedures like methods.
    "The method implementation includes a check if the passed number is positive.
    "If so, the square root is calculated. Otherwise, the method call is terminated.
    "That means, the returned value for the second method call is initial.

    DATA(return1) = meth_with_return( 81 ).

    out->write( data = return1 name = `return1` ).
    out->write( |\n| ).

    DATA(return2) = meth_with_return( -9 ).

    out->write( data = return2 name = `return2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Conditional Loops with WHILE` ) ).

    out->write( |13) WHILE Example 1\n| ).

    "The following example highlights the setting of sy-index within loop passes.
    "The value is added to an internal table. The loop iteration stops when
    "the internal table has 10 lines.

    DATA int_itab TYPE TABLE OF i.

    WHILE lines( int_itab ) < 10.
      int_itab = VALUE #( BASE int_itab ( sy-index ) ).
    ENDWHILE.

    out->write( data = int_itab name = `int_itab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `14) WHILE Example 2` ) ).

    "In the following example, all occurrences of a certain substring
    "should be replaced by another string. Instead of, for example, using
    "a REPLACE ALL OCCURRENCES statement, a WHILE loop is used which
    "contains a FIND statement that stores offset and length of the found
    "substring. Then, a REPLACE statement is used and that only replaces
    "the substring according to the values. The condition for WHILE is
    "tailored in a way that the loop is terminated if there are no more
    "findings. The number of findings is also displayed - a number that
    "corresponds to the number of loop passes.

    DATA(while_string) = `##abap####abap#abap######abap###abapabap##abap`.
    DATA count_occ TYPE i.
    DATA count_occ_via_sy_index TYPE i.

    DATA(subrc) = 0. "separate dobj to store the sy-subrc value

    WHILE subrc = 0.
      FIND FIRST OCCURRENCE OF `abap` IN while_string
        MATCH LENGTH DATA(len)
        MATCH OFFSET DATA(off)
        MATCH COUNT DATA(cnt).
      "Note: cnt is always 1 since it is always the first occurrence.
      "      len is always 4 -> abap
      "      Only offset has different values

      "A separate data object subrc gets assigned the value of sy-subrc.
      "As long as there are findings in the string, the while loop continues.
      subrc = sy-subrc.

      IF subrc = 0.
        count_occ_via_sy_index = sy-index. "to hold the
        "To hold the total number of findings from left to right of the string
        count_occ = count_occ + cnt.
        REPLACE SECTION OFFSET off LENGTH len OF while_string WITH `ABAP`.
      ENDIF.
    ENDWHILE.

    out->write( data = count_occ_via_sy_index name = `count_occ_via_sy_index` ).
    out->write( |\n| ).
    out->write( data = count_occ name = `count_occ` ).
    out->write( |\n| ).
    out->write( data = while_string name = `while_string` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `15) WHILE Example 3` ) ).

    "The example demonstrates the 3 options for loop terminations
    "in the context of a WHILE ... ENDWHILE statement.

    DATA while_cnt TYPE i.
    DATA while_sy_index TYPE string.

    DATA(index) = 0.

    WHILE index <> 10.
      index = sy-index.

      IF sy-index = 2. "Skips loop pass
        CONTINUE.
      ENDIF.

      CHECK sy-index <> 5. "Skips loop pass

      IF sy-index = 8. "Terminates loop
        EXIT.
      ENDIF.

      while_cnt += 1.
      while_sy_index = while_sy_index && sy-index && ` `.
    ENDWHILE.

    out->write( data = while_cnt name = `while_cnt` ).
    out->write( |\n| ).
    out->write( data = while_sy_index name = `while_sy_index` ).


**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Loops across Tables` ) ).

    out->write( |16) Loop across Internal Table Using LOOP ... ENDLOOP\n\n| ).

    "LOOP ... ENDLOOP statements are meant for loops across internal tables.
    "For more examples, see the cheat sheet example on internal tables.
    "Note: In contrast to the loops above, the system field sy-index is not set.
    "Instead, the system field sy-tabix is set and which contains the table index
    "of the current table line in the loop pass.
    "The example demonstrates a loop across an internal table. It combines loops
    "and control structures mentioned before:
    "- An internal table includes two numbers and an operator based on which a
    "  a calculation is carried out.
    "- The calculation is done in a method. It is implemented in a way that
    "  IF and CASE statements as well as COND and SWITCH operators are used
    "  for the calculation.
    "- The calculation result is stored in an internal table which is output.
    "- All four results - which are all the same, of course, just demonstrating
    "  the variety of options to achieve the same regarding control structures -
    "  are available in the table. Plus, the value in sy-tabix is also added to
    "  the internal table representing the table index.

    TYPES: BEGIN OF calc_struc,
             num1     TYPE i,
             operator TYPE c LENGTH 1,
             num2     TYPE i,
           END OF calc_struc.

    DATA calc_itab TYPE TABLE OF calc_struc WITH EMPTY KEY.

    "Internal table containing numbers and operators on whose basis
    "calculations are to be carried out.
    calc_itab = VALUE #( ( num1 = 123 operator = '+' num2 = 456 )
                         ( num1 = -10 operator = '+' num2 = 9 )
                         ( num1 = 12 operator = '-' num2 = 89 )
                         ( num1 = -5 operator = '-' num2 = -12 )
                         ( num1 = 11 operator = '*' num2 = 10 )
                         ( num1 = -3 operator = '*' num2 = 3 )
                         ( num1 = 1 operator = '/' num2 = 5 )
                         ( num1 = -40 operator = '/' num2 = 2 )
                         ( num1 = 5 operator = '/' num2 = 0  )
                         ( num1 = 7 operator = '#' num2 = 4  ) ).

    DATA calc_results TYPE TABLE OF calc_results_struc WITH EMPTY KEY.

    LOOP AT calc_itab ASSIGNING FIELD-SYMBOL(<calc>).
      "Method call to calculate and return the result
      DATA(res) = calc( num1     = <calc>-num1
                        operator = <calc>-operator
                        num2     = <calc>-num2 ).

      "Adding the sy-tabix value to the table, too.
      res-sy_tabix = sy-tabix.
      APPEND res TO calc_results.
    ENDLOOP.

    out->write( data = calc_results name = `calc_results` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `17) SELECT Loop`  ) ).

    "SELECT ... ENDSELECT statements loop across the result set of a database access.
    "For more examples, see the cheat sheet example on ABAP SQL.
    "The example is meant to give an idea. The SELECT loop is purposely done across
    "an internal table which is also possible - just in the interest of not dealing
    "with a database table and not ensuring that a demo database table is filled.
    "As above, the example includes simple calculations.

    TYPES: BEGIN OF struc4loop,
             num         TYPE i,
             calc_result TYPE i,
           END OF struc4loop.

    TYPES ty_itab_select_loop TYPE TABLE OF struc4loop WITH EMPTY KEY.

    DATA(loop_pass) = 0.

    DATA(itab_select_loop) = VALUE ty_itab_select_loop( ( num = 1 )
                                                        ( num = 2 )
                                                        ( num = 3 )
                                                        ( num = 4 )
                                                        ( num = 5 )
                                                        ( num = 6 ) ).

    SELECT *
      FROM @itab_select_loop AS itab
      INTO @DATA(wa).

      IF sy-subrc = 0.
        "Loop pass stored and representing the table index value for the
        "ABAP SQL statement further down.
        loop_pass += 1.
        IF loop_pass <= 3.
          wa-calc_result = wa-num * 5.
        ELSEIF loop_pass = 6.
          "No calculation for this loop pass. Loop is terminated.
          EXIT.
        ELSE.
          wa-calc_result = wa-num * 100.
        ENDIF.
        "Inserting calculation result in table
        MODIFY itab_select_loop FROM wa INDEX loop_pass TRANSPORTING calc_result.
      ENDIF.
    ENDSELECT.

    out->write( data = itab_select_loop name = `itab_select_loop` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Exception Handling` ) ).

    out->write( |18) TRY Control Structures\n\n| ).

    "TRY control structures are meant for handling catchable exceptions locally
    "The example shows divisions. The predefined exception class cx_sy_zerodivide
    "as suitable exception class is used.
    "If the exception is not handled, the program is terminated and the runtime
    "error COMPUTE_INT_ZERODIVIDE occurs.
    "The third calculation is not carried out because the statement block is
    "left due to the previous erroneous 0 division.

    TRY.
        DATA(div1) = 4 / 2.
        out->write( data = div1 name = `div1` ).
        out->write( |\n| ).

        DATA(div2) = 4 / 0.
        out->write( data = div2 name = `div2` ).
        out->write( |\n| ).

        DATA(div3) = 9 / 3.
        out->write( data = div3 name = `div3` ).
        out->write( |\n| ).

      CATCH cx_sy_zerodivide.
        out->write( `0 division. The exception was caught.` ).
        out->write( |\n| ).
    ENDTRY.

    "The following example shows a catchable exception that is
    "raised if a line is not found when using table expressions.
    TRY.
        DATA(line) = str_table[ 12345 ].

        "The predefined exception class cx_sy_itab_line_not_found
        "as suitable exception class is used here.
        "If the exception is not handled, the program is terminated
        "and the runtime error ITAB_LINE_NOT_FOUND occurs.
      CATCH cx_sy_itab_line_not_found.
        out->write( `The line was not found. The exception was caught.` ).
    ENDTRY.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `19) Multiple CATCH Blocks` ) ).

    "It is possible to specify multiple exception classes in a list and
    "multiple CATCH blocks.
    "Note: If there are multiple CATCH blocks for exceptions that are in an inheritance
    "relationship, you must pay attention that the more special exceptions are specified
    "before the more general ones.
    "The calculation example shows multiple CATCH blocks that themselves have more than
    "one exception class specified. Here, local exception classes are specified just for
    "demonstration purposes. They are not relevant in this TRY control structure.

    "Filling internal table of type i as basis for calculations
    int_itab = VALUE #( ( 5 ) ( 0 ) ( 987654321 ) ).

    LOOP AT int_itab ASSIGNING FIELD-SYMBOL(<fs_int>).
      TRY.
          out->write( |--- Calculations with { <fs_int> } ---| ).

          DATA(calc1) = CONV decfloat34( 1 / <fs_int> ).

          out->write( data = calc1 name = `calc1` ).
          out->write( |\n| ).

          DATA(calc2) = ipow( base = <fs_int> exp = 2 ).

          out->write( data = calc2 name = `calc2` ).
          out->write( |\n| ).
        CATCH cx_sy_arithmetic_overflow lcx_calc_error.
          out->write( `Arithmetic overflow. The exception was caught.` ).
        CATCH cx_sy_zerodivide lcx_some_error.
          out->write( `0 division. The exception was caught.` ).
      ENDTRY.
      out->write( |\n| ).
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `20) Using an Exception Class Higher Up in the Inheritance Tree` ) ).

    "In the following CATCH block, the predefined exception class cx_sy_arithmetic_error
    "is specified. Both cx_sy_zerodivide and cx_sy_arithmetic_overflow are derived from
    "cx_sy_arithmetic_error which is an exception class higher up in the inheritance
    "tree. Hence, cx_sy_arithmetic_error can be specified and handle both exceptions, too.
    "The following example is basically the same as above. However, only one exception
    "class is specified.

    LOOP AT int_itab ASSIGNING FIELD-SYMBOL(<fs_int_inh>).
      TRY.
          out->write( |--- Calculations with { <fs_int_inh> } ---| ).

          calc1 = 1 / <fs_int_inh>.

          out->write( data = calc1 name = `calc1` ).
          out->write( |\n| ).

          calc2 = ipow( base = <fs_int_inh> exp = 2 ).

          out->write( data = calc2 name = `calc2` ).
          out->write( |\n| ).
        CATCH cx_sy_arithmetic_error.
          out->write( `Arithmetic error. The exception was caught.` ).
      ENDTRY.
       out->write( |\n| ).
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `21) Storing a Reference to the Exception Object` ) ).

    "You can use the addition INTO plus an object reference variable to store
    "a reference to an exception object. It is, for example, relevant to
    "determine the exact exception.
    "The following example is the same as above using the more general exception
    "class cx_sy_arithmetic_error. You can carry out certain tasks, for
    "example, retrieving and displaying the exception text. To retrieve exception
    "texts, you can call, for example, the method get_text.

    LOOP AT int_itab ASSIGNING FIELD-SYMBOL(<fs_int_into>).

      TRY.
          out->write( |--- Calculations with { <fs_int_into> } ---| ).

          calc1 = 1 / <fs_int_into>.

          out->write( data = calc1 name = `calc1` ).
          out->write( |\n| ).

          calc2 = ipow( base = <fs_int_into> exp = 2 ).

          out->write( data = calc2 name = `calc2` ).
          out->write( |\n| ).
        CATCH cx_sy_arithmetic_error INTO exception.
          "Note:
          "- The object reference variable is of type cx_root.
          "- You could also create the variable inline, e. g. ... INTO DATA(exc).

          "Retrieving and displaying exception text
          exception_text = exception->get_text( ).

          out->write( data = exception_text name = `exception_text` ).
      ENDTRY.
      out->write( |\n| ).
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `22) Raising Exceptions Programmatically` ) ).

    "The following examples demonstrate the ABAP statement RAISE EXCEPTION
    "using the addition TYPE. Note there are more additions available that
    "are not dealt with here.

    "ABAP 'allows' zero division if the first operand is also 0 as shown
    "here.
    DATA(division) = 0 / 0.

    out->write( data = division name = `division` ).
    out->write( |\n| ).

    "In this example, the appropriate exception - the predefined exception
    "class cx_sy_zerodivide - is raised.
    TRY.
        division = 0 / 0.

        "raise predefined exception for 0 division
        RAISE EXCEPTION TYPE cx_sy_zerodivide.

      CATCH cx_sy_zerodivide INTO DATA(error_oref).
        exception_text = error_oref->get_text( ).
        out->write( data = exception_text name = `exception_text` ).
        out->write( |\n| ).
    ENDTRY.

    "The following example just demonstrates a locally defined
    "exception class that must be raised programmatically.
    "In ADT, see the definition of the class in the include, i. e.
    "the tab 'Class-relevant Local Types'.
    TRY.
        RAISE EXCEPTION TYPE lcx_some_error.
      CATCH lcx_some_error INTO exception.
        exception_text = exception->get_text( ).

        out->write( `Default exception text for a local exception class:` ).
        out->write( data = exception_text name = `exception_text` ).
    ENDTRY.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `23) Nested TRY Control Structure` ) ).

    TRY.
        TRY.
            RAISE EXCEPTION TYPE lcx_some_error.
          CATCH lcx_some_error INTO DATA(error_inner_catch).
            out->write( `Inner CATCH` ).
            RAISE EXCEPTION error_inner_catch.
        ENDTRY.
      CATCH lcx_some_error.
        out->write( `Outer CATCH` ).
    ENDTRY.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `24) Raising an Exception in the Context of Conditional Expressions` ) ).

    "In this example, the optional addition THROW is used in a conditional
    "expression. A self-defined local exception class is used.

    DATA(number) = 3.

    TRY.
        DATA(cond_raise) = COND #( WHEN number = 1 THEN `one`
                                   WHEN number = 2 THEN `two`
                                   ELSE THROW lcx_some_error( ) ).

        out->write( data = cond_raise name = `cond_raise` ).
      CATCH lcx_some_error INTO exception.
        exception_text = exception->get_text( ).
        out->write( data = exception_text name = `exception_text` ).
    ENDTRY.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `25) RAISING Parameter in Method Delcarations` ) ).

    "In the following example, a string table is looped across. The table
    "includes valid and invalid email addresses. The validity is checked
    "in a method. In the method, a local exception class is raised if the
    "email address is invalid.

    str_table = VALUE string_table( ( `john.doe@email.com` )         "valid
                                    ( `john.doe@#email.com` )        "invalid
                                    ( `jane.doe@email.com` )         "valid
                                    ( `jane#doe@email.com` )         "invalid
                                    ( `max.mustermann@email12.com` ) "valid
                                    ( `max.mustermann@email12.c#m` ) "invalid
                                    ( `some_name@email.com` )        "valid
                                    ( `some_name@email.c` )          "invalid
                                  ).

    TYPES: BEGIN OF struc_email_check,
             email            TYPE string,
             is_email_valid   TYPE abap_bool,
             exception_raised TYPE abap_bool,
           END OF  struc_email_check.

    DATA itab_email_check TYPE TABLE OF struc_email_check WITH EMPTY KEY.

    LOOP AT str_table ASSIGNING FIELD-SYMBOL(<email>).
      TRY.
          DATA(email_valid) = validate_email( email = <email> ).
          DATA(exc_raised) = abap_false.
        CATCH lcx_invalid_email.
          email_valid = abap_false.
          exc_raised = abap_true.
      ENDTRY.

      APPEND VALUE #( email = <email>
                      is_email_valid = email_valid
                      exception_raised = exc_raised ) TO itab_email_check.
    ENDLOOP.

    out->write( data = itab_email_check name = `itab_email_check` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `26) Exception Classes Derived from CX_STATIC_CHECK` ) ).

    "Exception Classes of type cx_static_check force users to handle exceptions.
    "Exceptions that are declared in the method signature make users aware of which
    "potential exception might occur. And the exceptions should be respected when using
    "such a method.
    "In this simplified example, a method has a self-defined local exception class
    "specified for the RAISING addition. If an actual parameter has a certain
    "value, the method raises an exception.
    "You can comment in the following line of code to see the enforcement.
    "A syntax warning is displayed since there is no proper exception handling.

    "DATA(my_user_a) = whats_my_user( get_name = abap_false ).

    "Method call with a proper exception handling
    TRY.
        DATA(my_user_b) = whats_my_user( get_name = abap_true ).

        out->write( data = my_user_b name = `my_user_b` ).
        out->write( |\n| ).

        DATA(my_user_c) = whats_my_user( get_name = abap_false ).

        out->write( data = my_user_c name = `my_user_c` ).
      CATCH lcx_static_exc_class INTO exception.
        exception_text = exception->get_text( ).
        out->write( data = exception_text name = `exception_texts` ).
    ENDTRY.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `27) Exception Classes Derived from CX_DYNAMIC_CHECK` ) ).

    "Exception Classes derived from cx_dynamic_check are for exceptions that
    "can be checked and avoided by preconditions.
    "The checking if a local handling or an explicit declaration in procedure
    "interfaces is available is carried out at runtime only ("dynamic check")
    "and only in case the exception is indeed raised.
    "The following example includes two TRY control structures in which a
    "method is called that carries out calculations. In this case, the
    "method uses the imported value and carries out two calculations (the
    "value powered by 2 and the square root).
    "The method signature purposely only includes the exception class
    "cx_sy_arithmetic_overflow, i. e. calculation errors regarding the
    "square root calculation are 'ignored' here.
    "The two exception classes cx_sy_arithmetic_overflow and cx_sy_arg_out_of_domain
    "are both derived from cx_dynamic_check.

    "This TRY control structure only catches exception for
    "cx_sy_arithmetic_overflow. The example does purposely not
    "include an error for the square root calculation. If you
    "implemented the code outside of the TRY control structure,
    "there would not be a syntax warning. Actually, you could
    "prevent a calculation failure by setting appropriate values.
    TRY.
        DATA(res1) = power2_and_sqrt( num = 4 ).

        out->write( data = res1 name = `res1` ).
        out->write( |\n| ).

        DATA(res2) = power2_and_sqrt( num = 123456789 ).

        out->write( data = res2 name = `res2` ).
        out->write( |\n| ).

      CATCH cx_sy_arithmetic_overflow INTO exception.
        exception_text = exception->get_text( ).
        out->write( data = exception_text name = `exception_text` ).
        out->write( |\n| ).
    ENDTRY.

    "This TRY control structure demonstrates the following:
    "If it is determined at runtime that an exception derived from cx_dynamic_check
    "is neither locally handled nor an interface is declared appropriately - and the
    "exception is raised - a new exception of type cx_sy_no_handler is raised. In this
    "case, the attribute 'previous' contains a reference to the original exception.
    TRY.
        DATA(res3) = power2_and_sqrt( num = 16 ).

        out->write( data = res3 name = `res3` ).
        out->write( |\n| ).

        DATA(res4) = power2_and_sqrt( num = -1 ).

        out->write( data = res4 name = `res4` ).
        out->write( |\n| ).

        "The specification of the suitable exception class does not help here.
        "The error is raised while processing the method.
      CATCH cx_sy_arg_out_of_domain INTO exception.
        exception_text = exception->get_text( ).

        out->write( data = exception_text name = `exception_text` ).
        out->write( |\n| ).
      CATCH cx_sy_no_handler INTO exception.
        exception_text = exception->get_text( ).

        out->write( data = exception_text name = `exception_text` ).
        out->write( |\n| ).

        "Attribute 'previous'
        "In this case, the information of the actual runtime error is provided:
        "Here, it is COMPUTE_SQRT_DOMAIN.
        out->write( data = exception->previous name = `exception->previous` ).
        out->write( |\n| ).

        "For demo purposes, RTTI is used here to retrieve the relative name of the type,
        "i. e. the exception class that was raised.
        DATA(relative_name) = cl_abap_typedescr=>describe_by_object_ref( exception->previous )->get_relative_name( ).

        out->write( data = relative_name name = `relative_name` ).
    ENDTRY.

**********************************************************************

    "Excursion: Runtime Errors and Terminating Programs

    out->write( zcl_demo_abap_aux=>heading( `28) Excursion: Runtime Errors and Terminating Programs` ) ).

    "ASSERT statements are followed by a logical expression. If the expression is false,
    "the program is terminated and an uncatchable exception is raised resulting in the
    "runtime error ASSERTION_FAILED.
    "You can comment in the code that is commented out below to check out the effect.
    "In ADT, you will see a message popping up and informing you about the runtime error.
    "You can check the details by choosing the "Show" button in the pop-up. Furthermore,
    "you can check the content of the "Feed Reader" tab in ADT. There, just expand your
    "project and find the runtime errors caused by you.

    "Terminating a program using ASSERT
    ASSERT 1 = 1.

    "ASSERT 1 + 2 = 5 - 3.

    DATA(some_flag) = abap_false.

    "ASSERT some_flag = abap_true.

    "Not handling predefined exception classes
    "Caused runtime errors COMPUTE_INT_ZERODIVIDE and ITAB_LINE_NOT_FOUND

    "DATA(zero_division) = 1 / 0.
    "DATA(nope) = str_table[ 12345 ].

    "Not handling self-defined exception classes
    "Causes runtime error UNCAUGHT_EXCEPTION

    "DATA(is_email_valid) = validate_email( email = `john.doe@email.c##` ).

    out->write( `This text is displayed if you left all statements causing a runtime error commented out :)` ).
  ENDMETHOD.

  METHOD meth_with_return.
    IF num >= 0.
      DATA(sqr_res) = sqrt( num ).
    ELSE.
      RETURN.
    ENDIF.

    res = `The method call was not terminated. The square root of ` && num && ` is ` && sqr_res.
  ENDMETHOD.

  METHOD power2_and_sqrt.
    result = |{ num } powered by 2 = { ipow( base = num exp = 2 ) } / Square root of { num } = { sqrt( num ) }|.
  ENDMETHOD.


  METHOD prep_calc_result.
    FIND PCRE `-$` IN res.  "trailing minus

    IF sy-subrc = 0.
      SHIFT res BY 1 PLACES RIGHT CIRCULAR.
    ENDIF.

    "trailing .0
    IF res CP `*.0*`.
      SHIFT res RIGHT DELETING TRAILING ` `.
      SHIFT res LEFT  DELETING LEADING ` `.
      FIND PCRE `\.0$` IN res.

      IF sy-subrc = 0.
        REPLACE `.0` IN res WITH ``.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD validate_email.
    IF matches( val   = email
                pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ).
      is_valid_email = abap_true.
    ELSE.
      RAISE EXCEPTION TYPE lcx_invalid_email.
    ENDIF.
  ENDMETHOD.

  METHOD whats_my_user.
    IF get_name = abap_true.
      name = sy-uname.
    ELSE.
      RAISE EXCEPTION TYPE lcx_static_exc_class.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
