CLASS ltc_calculate DEFINITION DEFERRED.
CLASS zcl_demo_aunit_no_tdf DEFINITION LOCAL FRIENDS ltc_calculate.

CLASS ltc_calculate DEFINITION FINAL FOR TESTING
	DURATION SHORT
	RISK LEVEL HARMLESS.
	PRIVATE SECTION.
		DATA cut TYPE REF TO zcl_demo_aunit_no_tdf.

		METHODS setup.
		METHODS test_addition FOR TESTING RAISING cx_static_check.
		METHODS test_subtraction FOR TESTING RAISING cx_static_check.
		METHODS test_multiplication FOR TESTING RAISING cx_static_check.
		METHODS test_division FOR TESTING RAISING cx_static_check.
		METHODS test_division_by_zero FOR TESTING RAISING cx_static_check.
		METHODS test_overflow FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_calculate IMPLEMENTATION.
	METHOD setup.
		cut = NEW zcl_demo_aunit_no_tdf( ).
	ENDMETHOD.

	METHOD test_addition.
		DATA(result) = cut->calculate(
			num1      = 10
			num2      = 15
			operation = zcl_demo_aunit_no_tdf=>addition ).

		cl_abap_unit_assert=>assert_equals(
			act = result
			exp = `25` ).
	ENDMETHOD.

	METHOD test_subtraction.
		DATA(result) = cut->calculate(
			num1      = 20
			num2      = 7
			operation = zcl_demo_aunit_no_tdf=>subtraction ).

		cl_abap_unit_assert=>assert_equals(
			act = result
			exp = `13` ).
	ENDMETHOD.

	METHOD test_multiplication.
		DATA(result) = cut->calculate(
			num1      = 6
			num2      = 8
			operation = zcl_demo_aunit_no_tdf=>multiplication ).

		cl_abap_unit_assert=>assert_equals(
			act = result
			exp = `48` ).
	ENDMETHOD.

	METHOD test_division.
		DATA(result) = cut->calculate(
			num1      = 42
			num2      = 6
			operation = zcl_demo_aunit_no_tdf=>division ).

		cl_abap_unit_assert=>assert_equals(
			act = result
			exp = `7` ).
	ENDMETHOD.

	METHOD test_division_by_zero.
		TRY.
				cut->calculate(
					num1      = 1
					num2      = 0
					operation = zcl_demo_aunit_no_tdf=>division ).
				cl_abap_unit_assert=>fail( msg = `Expected arithmetic error for division by zero.` ).
			CATCH cx_sy_arithmetic_error.
		ENDTRY.
	ENDMETHOD.

	METHOD test_overflow.
		TRY.
				cut->calculate(
					num1      = 2147483647
					num2      = 1
					operation = zcl_demo_aunit_no_tdf=>addition ).
				cl_abap_unit_assert=>fail( msg = `Expected arithmetic overflow.` ).
			CATCH cx_sy_arithmetic_error.
		ENDTRY.
	ENDMETHOD.
ENDCLASS.

