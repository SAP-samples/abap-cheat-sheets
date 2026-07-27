"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class represents the class that includes the tests for class {@link zcl_demo_aunit_external_cl}.
"! {@link zcl_demo_aunit_external_cl} defines both a public and private method that should be tested. The
"! test class tests the private method via a bridge. To do so, this class is befriended with {@link zcl_demo_aunit_external_cl}
"! and its local class to enable instantiation of the global class in the local test class. In the test, the instance is passed,
"! along with demo values.
CLASS ztcl_demo_aunit_external_cl DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">Invokes a private calculation method for unit tests</p>
    "!
    "! @parameter cut                  | <p class="shorttext synchronized" lang="en">Instance reference of the class to test</p>
    "! @parameter num1                 | <p class="shorttext synchronized" lang="en">First operand for the arithmetic operation</p>
    "! @parameter num2                 | <p class="shorttext synchronized" lang="en">Second operand for the arithmetic operation</p>
    "! @parameter operation            | <p class="shorttext synchronized" lang="en">Type of arithmetic operation to perform</p>
    "! @parameter result               | <p class="shorttext synchronized" lang="en">String result of the arithmetic operation</p>
    "! @raising cx_sy_arithmetic_error | <p class="shorttext synchronized" lang="en">Exception raised for arithmetic errors</p>
    CLASS-METHODS call_private_calculate
      IMPORTING cut           TYPE REF TO zcl_demo_aunit_external_cl
                num1          TYPE i
                num2          TYPE i
                operation     TYPE zcl_demo_aunit_external_cl=>arithmetic_operation
      RETURNING VALUE(result) TYPE string
      RAISING   cx_sy_arithmetic_error.
ENDCLASS.



CLASS ztcl_demo_aunit_external_cl IMPLEMENTATION.
  METHOD call_private_calculate.
    result = cut->calculate_private(
      num1      = num1
      num2      = num2
      operation = operation ).
  ENDMETHOD.
ENDCLASS.
