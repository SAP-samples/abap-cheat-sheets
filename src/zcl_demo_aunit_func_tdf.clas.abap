"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class demonstrates ABAP Unit tests for logic that depends on a function module. It provides a method to
"! perform arithmetic calculations based on an operator. It defines an enumeration for arithmetic operators and
"! leverages the function module ZFUNC_DEMO_AUNIT to execute calculations.
CLASS zcl_demo_aunit_func_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM operator,
             add,
             subtract,
             multiply,
             divide,
           END OF ENUM operator.

    "! Performs arithmetic operations based on the given operator and returns the result
    "!
    "! @parameter num1                 | <p class="shorttext synchronized" lang="en">First integer input for calculation</p>
    "! @parameter num2                 | <p class="shorttext synchronized" lang="en">Second integer input for calculation</p>
    "! @parameter operator             | <p class="shorttext synchronized" lang="en">Arithmetic operator (add, subtract, multiply, divide)</p>
    "! @parameter result               | <p class="shorttext synchronized" lang="en">Result of the arithmetic operation as a string</p>
    "! @raising cx_sy_arithmetic_error | <p class="shorttext synchronized" lang="en">Exception raised for arithmetic errors</p>
    METHODS calculate IMPORTING num1          TYPE i
                                operator      TYPE operator
                                num2          TYPE i
                      RETURNING VALUE(result) TYPE string
                      RAISING   cx_sy_arithmetic_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_func_tdf IMPLEMENTATION.
  METHOD calculate.
    CALL FUNCTION 'ZFUNC_DEMO_AUNIT'
      EXPORTING
        num1     = num1
        operator = operator
        num2     = num2
      IMPORTING
        result   = result.
  ENDMETHOD.

ENDCLASS.
