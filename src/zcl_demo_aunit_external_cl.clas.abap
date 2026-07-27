"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class provides a methods for performing arithmetic operations and demonstrates ABAP Unit
"! testing with an external test class ({@link ztcl_demo_aunit_external_cl}). It includes both public and
"! private calculation methods.
CLASS zcl_demo_aunit_external_cl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS ztcl_demo_aunit_external_cl.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM arithmetic_operation,
             addition,
             subtraction,
             multiplication,
             division,
           END OF ENUM arithmetic_operation.

    "! Performs arithmetic operations based on the specified operation type
    "!
    "! @parameter num1                 | <p class="shorttext synchronized" lang="en">First operand for the arithmetic operation</p>
    "! @parameter num2                 | <p class="shorttext synchronized" lang="en">Second operand for the arithmetic operation</p>
    "! @parameter operation            | <p class="shorttext synchronized" lang="en">Type of arithmetic operation to perform</p>
    "! @parameter result               | <p class="shorttext synchronized" lang="en">The result of the arithmetic operation</p>
    "! @raising cx_sy_arithmetic_error | <p class="shorttext synchronized" lang="en">Exception raised for errors during arithmetic operations</p>
    METHODS calculate IMPORTING num1          TYPE i
                                num2          TYPE i
                                operation     TYPE arithmetic_operation
                      RETURNING VALUE(result) TYPE string
                      RAISING   cx_sy_arithmetic_error.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Executes private arithmetic calculations based on the specified operation type
    "!
    "! @parameter num1                 | <p class="shorttext synchronized" lang="en">First operand for the arithmetic operation</p>
    "! @parameter num2                 | <p class="shorttext synchronized" lang="en">Second operand for the arithmetic operation</p>
    "! @parameter operation            | <p class="shorttext synchronized" lang="en">Type of arithmetic operation to perform</p>
    "! @parameter result               | <p class="shorttext synchronized" lang="en">The result of the arithmetic operation</p>
    "! @raising cx_sy_arithmetic_error | <p class="shorttext synchronized" lang="en">Exception raised for errors during arithmetic operations</p>
    METHODS calculate_private IMPORTING num1          TYPE i
                                        num2          TYPE i
                                        operation     TYPE arithmetic_operation
                              RETURNING VALUE(result) TYPE string
                              RAISING   cx_sy_arithmetic_error.
ENDCLASS.



CLASS zcl_demo_aunit_external_cl IMPLEMENTATION.

  METHOD calculate.
    result = SWITCH #( operation WHEN addition THEN |{ num1 + num2 STYLE = SIMPLE }|
                                 WHEN subtraction THEN |{ num1 - num2 STYLE = SIMPLE }|
                                 WHEN multiplication THEN |{ num1 * num2 STYLE = SIMPLE }|
                                 WHEN division THEN |{ num1 / num2 STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD calculate_private.
    result = SWITCH #( operation WHEN addition THEN |{ num1 + num2 STYLE = SIMPLE }|
                                 WHEN subtraction THEN |{ num1 - num2 STYLE = SIMPLE }|
                                 WHEN multiplication THEN |{ num1 * num2 STYLE = SIMPLE }|
                                 WHEN division THEN |{ num1 / num2 STYLE = SIMPLE }| ).
  ENDMETHOD.

ENDCLASS.
