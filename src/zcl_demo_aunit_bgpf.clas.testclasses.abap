CLASS ltc_bgpf DEFINITION FINAL FOR TESTING
                  DURATION SHORT
                  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut           TYPE REF TO zcl_demo_aunit_bgpf.
    CLASS-DATA bgpf_env TYPE REF TO if_bgmc_test_envir_spy.

    DATA process   TYPE REF TO if_bgmc_process_spy.
    DATA operation TYPE REF TO zcl_demo_aunit_bgpf.

    METHODS test_execute FOR TESTING RAISING cx_static_check.
    METHODS test_execute_2 FOR TESTING RAISING cx_static_check.

    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    METHODS teardown.
    METHODS setup.

ENDCLASS.

CLASS ltc_bgpf IMPLEMENTATION.

  METHOD class_setup.
    bgpf_env = cl_bgmc_test_environment=>create_for_spying( ).

    "Clearing the database table that is filled in the cut
    "Purpose: Illustrating with an ABAP SQL SELECT on the database
    "in the test method that the test method execution does not
    "modify it.
    DELETE FROM ztaunitflights.
  ENDMETHOD.

  METHOD class_teardown.
    bgpf_env->destroy( ).
  ENDMETHOD.

  METHOD teardown.
    bgpf_env->clear( ).
  ENDMETHOD.

  METHOD test_execute.

    cut->execute( ).

    bgpf_env->assert_number_of_processes( 1 ).
    process = bgpf_env->get_process( 1 ).
    process->assert_number_of_operations( 1 ).
    process->assert_is_saved_for_processing( ).
    operation = CAST #( process->get_operation( 1 ) ).

    "Illustrating that the string was not transformed to upper case
    cl_abap_unit_assert=>assert_equals(
      act = operation->get_input( )
      exp = 'abc'
      msg = 'Single scheduled operation should carry input abc.' ).

    SELECT * FROM ztaunitflights
      INTO TABLE @DATA(itab).

    cl_abap_unit_assert=>assert_initial( itab ).

  ENDMETHOD.

  METHOD test_execute_2.
    DATA test_inputs TYPE string_table.

    cut->execute_2( ).
    bgpf_env->assert_number_of_processes( 2 ).

    DO 2 TIMES.
      process = bgpf_env->get_process( sy-index ).
      process->assert_number_of_operations( 1 ).
      process->assert_is_saved_for_processing( ).
      operation = CAST #( process->get_operation( 1 ) ).
      APPEND operation->get_input( ) TO test_inputs.
    ENDDO.

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( line_exists( test_inputs[ table_line = `Number 1` ] ) )
      msg = 'Batch scheduling should include Number 1 input.' ).

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( line_exists( test_inputs[ table_line = `Number 2` ] ) )
      msg = 'Batch scheduling should include Number 2 input.' ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

ENDCLASS.
