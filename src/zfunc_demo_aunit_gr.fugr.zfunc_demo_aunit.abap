FUNCTION zfunc_demo_aunit.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(NUM1) TYPE  I
*"     REFERENCE(OPERATOR) TYPE  ZCL_DEMO_AUNIT_FUNC_TDF=>OPERATOR
*"     REFERENCE(NUM2) TYPE  I
*"  EXPORTING
*"     REFERENCE(RESULT) TYPE  STRING
*"  RAISING
*"      CX_SY_ARITHMETIC_ERROR
*"----------------------------------------------------------------------


  "Raising zero division exception if both operands are 0.
  IF num1 = 0 AND num2 = 0.
    RAISE EXCEPTION TYPE cx_sy_zerodivide.
  ENDIF.

  result = SWITCH #( operator
                     WHEN zcl_demo_aunit_func_tdf=>add THEN |{ num1 } + { num2 } = { num1 + num2 STYLE = SIMPLE }|
                     WHEN zcl_demo_aunit_func_tdf=>subtract THEN |{ num1 } - { num2 } = { num1 - num2 STYLE = SIMPLE }|
                     WHEN zcl_demo_aunit_func_tdf=>multiply THEN |{ num1 } * { num2 } = { num1 * num2 STYLE = SIMPLE }|
                     WHEN zcl_demo_aunit_func_tdf=>divide THEN |{ num1 } / { num2 } = { CONV decfloat34( num1 / num2 ) STYLE = SIMPLE }| ).
ENDFUNCTION.
