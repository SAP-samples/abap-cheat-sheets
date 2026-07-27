"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"! A demo method is included for ABAP Unit tests. It does not involve any dependent-on component (DOC).
"! The test class does not use ABAP frameworks for any test doubles. Values, on which the tests are
"! based, are hard-coded.
CLASS zcl_demo_aunit_no_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ENUM arithmetic_operation,
             addition,
             subtraction,
             multiplication,
             division,
           END OF ENUM arithmetic_operation.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate IMPORTING num1          TYPE i
                                num2          TYPE i
                                operation     TYPE arithmetic_operation
                      RETURNING VALUE(result) TYPE string
                      RAISING   cx_sy_arithmetic_error.
ENDCLASS.



CLASS zcl_demo_aunit_no_tdf IMPLEMENTATION.

  METHOD calculate.
    result = SWITCH #( operation WHEN addition THEN |{ num1 + num2 STYLE = SIMPLE }|
                                 WHEN subtraction THEN |{ num1 - num2 STYLE = SIMPLE }|
                                 WHEN multiplication THEN |{ num1 * num2 STYLE = SIMPLE }|
                                 WHEN division THEN |{ num1 / num2 STYLE = SIMPLE }| ).
  ENDMETHOD.

ENDCLASS.
