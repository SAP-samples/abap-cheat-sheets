CLASS ltc_occupancy_rate DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA cut TYPE REF TO zcl_demo_aunit_sql_tdf.
    CLASS-DATA sql_env TYPE REF TO if_osql_test_environment.
    CLASS-DATA flights_tab TYPE zif_demo_aunit_flights=>t_flight_data.

    CLASS-METHODS class_setup.
    METHODS setup.
    CLASS-METHODS class_teardown.
    METHODS test_rate FOR TESTING.
    METHODS test_rate_full FOR TESTING.
    METHODS test_rate_zero FOR TESTING.
    METHODS test_rate_round FOR TESTING.
    METHODS test_rate_non_existing_carrier FOR TESTING.
    METHODS test_rate_no_data FOR TESTING.
ENDCLASS.

CLASS ltc_occupancy_rate IMPLEMENTATION.

  METHOD class_setup.
    cut = NEW zcl_demo_aunit_sql_tdf( ).

    sql_env = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'ZTAUNITFLIGHTS' ) ) ).

    "Most example test methods use the same dataset on which the tests are based. Therefore, populating
    "the table once on test execution.
    flights_tab = VALUE #(  ( carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
     ( carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 )
     ( carrid = 'AA' connid = '1003' fldate = '20260803' seatsmax = 300 seatsocc = 280 )
     ( carrid = 'BB' connid = '2001' fldate = '20260801' seatsmax = 150 seatsocc = 150 )
     ( carrid = 'BB' connid = '2002' fldate = '20260802' seatsmax = 120 seatsocc = 120 )
     ( carrid = 'CC' connid = '3001' fldate = '20260801' seatsmax = 100 seatsocc = 0 )
     ( carrid = 'CC' connid = '3002' fldate = '20260802' seatsmax = 50 seatsocc = 0 )
     ( carrid = 'DD' connid = '4001' fldate = '20260801' seatsmax = 3 seatsocc = 1 ) ).

  ENDMETHOD.

  METHOD setup.
    sql_env->clear_doubles( ).
  ENDMETHOD.

  METHOD class_teardown.
    sql_env->destroy( ).
  ENDMETHOD.

  METHOD test_rate.
    sql_env->insert_test_data( flights_tab ).

    DATA(occupancy_rate) = cut->calculate_occupancy_rate( carrier_id = 'AA' ).

    cl_abap_unit_assert=>assert_equals(
     act = occupancy_rate
      exp = CONV decfloat34( '87.57' )
      msg = 'AA occupancy rate should be 87.57.' ).
  ENDMETHOD.

  METHOD test_rate_full.
    sql_env->insert_test_data( flights_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'BB' )
      exp = CONV decfloat34( '100' )
      msg = 'BB occupancy rate should be 100.' ).
  ENDMETHOD.

  METHOD test_rate_zero.
    sql_env->insert_test_data( flights_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'CC' )
      exp = CONV decfloat34( '0' )
      msg = 'CC occupancy rate should be 0.' ).
  ENDMETHOD.

  METHOD test_rate_round.
    sql_env->insert_test_data( flights_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'DD' )
      exp = CONV decfloat34( '33.33' )
      msg = 'DD occupancy rate should be rounded to 33.33.' ).
  ENDMETHOD.

  METHOD test_rate_non_existing_carrier.
    sql_env->insert_test_data( flights_tab ).

    cl_abap_unit_assert=>assert_equals(
       act = cut->calculate_occupancy_rate( carrier_id = 'XX' )
       exp = CONV decfloat34( '0' )
       msg = 'Unknown carrier should return 0 occupancy rate.' ).
  ENDMETHOD.

  METHOD test_rate_no_data.
    "No data inserted using the insert_test_data method
    cl_abap_unit_assert=>assert_initial(
      act = cut->calculate_occupancy_rate( carrier_id = 'AA' )
      msg = 'No data should return initial occupancy rate.' ).
  ENDMETHOD.

ENDCLASS.
