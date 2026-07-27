CLASS ltc_calculate_func_td DEFINITION FINAL FOR TESTING
	DURATION SHORT
	RISK LEVEL HARMLESS.
  	PRIVATE SECTION.
    		DATA cut TYPE REF TO zcl_demo_aunit_func_tdf.
    		CLASS-DATA function_env TYPE REF TO if_function_test_environment.

    		CLASS-METHODS class_setup.
    		CLASS-METHODS class_teardown.
    		METHODS setup.

    		METHODS configure_result
    			IMPORTING first_number     TYPE i
    								op TYPE zcl_demo_aunit_func_tdf=>operator
    								second_number     TYPE i
    								calc_result   TYPE string.

    		METHODS configure_exception
    			IMPORTING first_number     TYPE i
    								op TYPE zcl_demo_aunit_func_tdf=>operator
    								second_number     TYPE i
    								exception TYPE REF TO cx_sy_arithmetic_error.

    		METHODS test_addition FOR TESTING.
    		METHODS test_subtraction FOR TESTING.
    		METHODS test_multiplication FOR TESTING.
    		METHODS test_division FOR TESTING.
    		METHODS test_exc_0_div_0 FOR TESTING.
    		METHODS test_exc_1_div_0 FOR TESTING.
    		METHODS test_exc_mult_overflow FOR TESTING.
    		METHODS test_without_helper_class FOR TESTING.
ENDCLASS.

CLASS ltc_calculate_func_td IMPLEMENTATION.

  	METHOD class_setup.
    		function_env = cl_function_test_environment=>create( VALUE #( ( 'ZFUNC_DEMO_AUNIT' ) ) ).			
  	ENDMETHOD.

  	METHOD class_teardown.
    		function_env->clear_doubles( ).
    		CLEAR function_env.
  	ENDMETHOD.

  	METHOD setup.
    		cut = NEW zcl_demo_aunit_func_tdf( ).
    		function_env->clear_doubles( ).
  	ENDMETHOD.


  METHOD test_without_helper_class.

    DATA(test_double) = function_env->get_double( 'ZFUNC_DEMO_AUNIT' ).

    DATA(input_conf) = test_double->create_input_configuration(
                                         )->set_importing_parameter( name = 'NUM1' value = 1
    )->set_importing_parameter( name = 'OPERATOR' value = zcl_demo_aunit_func_tdf=>add
    )->set_importing_parameter( name = 'NUM2' value = 1 ).

    DATA(output_conf) = test_double->create_output_configuration(
                                         )->set_exporting_parameter( name = 'RESULT' value = 2 ).

    test_double->configure_call(
     )->when( input_configuration = input_conf
     )->then_set_output( output_configuration = output_conf ).

    DATA(calc_result) = cut->calculate(
                      num1     = 1
                      operator = zcl_demo_aunit_func_tdf=>add
                      num2     = 1
                    ).

    cl_abap_unit_assert=>assert_equals( act = calc_result
                                        exp = 2 ).
  ENDMETHOD.


  	METHOD configure_result.
    		DATA(function_double) = function_env->get_double( function_name = 'ZFUNC_DEMO_AUNIT' ).

    		DATA(input_cfg) = function_double->create_input_configuration( ).
    		input_cfg->set_importing_parameter( name = 'NUM1' value = first_number ).
    		input_cfg->set_importing_parameter( name = 'OPERATOR' value = op ).
    		input_cfg->set_importing_parameter( name = 'NUM2' value = second_number ).

    		DATA(output_cfg) = function_double->create_output_configuration( ).
    		output_cfg->set_exporting_parameter( name = 'RESULT' value = calc_result ).

    		DATA(call_cfg) = function_double->configure_call( ).
    		DATA(then_cfg) = call_cfg->when( input_configuration = input_cfg ).
    		then_cfg->then_set_output( output_configuration = output_cfg ).
  	ENDMETHOD.

  	METHOD configure_exception.
    		DATA(function_double) = function_env->get_double( function_name = 'ZFUNC_DEMO_AUNIT' ).

    		DATA(input_cfg) = function_double->create_input_configuration( ).
    		input_cfg->set_importing_parameter( name = 'NUM1' value = first_number ).
    		input_cfg->set_importing_parameter( name = 'OPERATOR' value = op ).
    		input_cfg->set_importing_parameter( name = 'NUM2' value = second_number ).

    		DATA(call_cfg) = function_double->configure_call( ).
    		DATA(then_cfg) = call_cfg->when( input_configuration = input_cfg ).
    		then_cfg->then_raise_exception( exception = exception ).
  	ENDMETHOD.

  	METHOD test_addition.
    		configure_result(
    			first_number = 7
    			op = zcl_demo_aunit_func_tdf=>add
    			second_number = 5
    			calc_result = '12' ).

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate(
    				num1 = 7
    				operator = zcl_demo_aunit_func_tdf=>add
    				num2 = 5 )
    			exp = '12' ).
  	ENDMETHOD.

  	METHOD test_subtraction.
    		configure_result(
    			first_number = 10
    			op = zcl_demo_aunit_func_tdf=>subtract
    			second_number = 3
    			calc_result = '7' ).

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate(
    				num1 = 10
    				operator = zcl_demo_aunit_func_tdf=>subtract
    				num2 = 3 )
    			exp = '7' ).
  	ENDMETHOD.

  	METHOD test_multiplication.
    		configure_result(
    			first_number = 6
    			op = zcl_demo_aunit_func_tdf=>multiply
    			second_number = 4
    			calc_result = '24' ).

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate(
    				num1 = 6
    				operator = zcl_demo_aunit_func_tdf=>multiply
    				num2 = 4 )
    			exp = '24' ).
  	ENDMETHOD.

  	METHOD test_division.
    		configure_result(
    			first_number = 20
    			op = zcl_demo_aunit_func_tdf=>divide
    			second_number = 5
    			calc_result = '4' ).

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate(
    				num1 = 20
    				operator = zcl_demo_aunit_func_tdf=>divide
    				num2 = 5 )
    			exp = '4' ).
  	ENDMETHOD.

  	METHOD test_exc_0_div_0.
    		configure_exception(
    			first_number = 0
    			op = zcl_demo_aunit_func_tdf=>divide
    			second_number = 0
    			exception = NEW cx_sy_zerodivide( ) ).

    		TRY.
        			cut->calculate(
        				num1 = 0
        				operator = zcl_demo_aunit_func_tdf=>divide
        				num2 = 0 ).
        			cl_abap_unit_assert=>fail( msg = 'Expected cx_sy_arithmetic_error for 0 / 0' ).
      		CATCH cx_sy_arithmetic_error.
    		ENDTRY.
  	ENDMETHOD.

  	METHOD test_exc_1_div_0.
    		configure_exception(
    			first_number = 1
    			op = zcl_demo_aunit_func_tdf=>divide
    			second_number = 0
    			exception = NEW cx_sy_zerodivide( ) ).

    		TRY.
        			cut->calculate(
        				num1 = 1
        				operator = zcl_demo_aunit_func_tdf=>divide
        				num2 = 0 ).
        			cl_abap_unit_assert=>fail( msg = 'Expected cx_sy_arithmetic_error for 1 / 0' ).
      		CATCH cx_sy_arithmetic_error.
    		ENDTRY.
  	ENDMETHOD.

  	METHOD test_exc_mult_overflow.
    		configure_exception(
    			first_number = 2147483647
    			op = zcl_demo_aunit_func_tdf=>multiply
    			second_number = 2147483647
    			exception = NEW cx_sy_arithmetic_overflow( ) ).

    		TRY.
        			cut->calculate(
        				num1 = 2147483647
        				operator = zcl_demo_aunit_func_tdf=>multiply
        				num2 = 2147483647 ).
        			cl_abap_unit_assert=>fail( msg = 'Expected cx_sy_arithmetic_error for overflow multiplication' ).
      		CATCH cx_sy_arithmetic_error.
    		ENDTRY.
  	ENDMETHOD.

ENDCLASS.
