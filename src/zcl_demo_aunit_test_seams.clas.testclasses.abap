CLASS ltc_occupancy_rate DEFINITION FINAL FOR TESTING
	DURATION SHORT
	RISK LEVEL HARMLESS.
  	PRIVATE SECTION.
			DATA cut TYPE REF TO zcl_demo_aunit_test_seams.

			METHODS setup.
    		METHODS test_rate FOR TESTING.
    		METHODS test_rate_full FOR TESTING.
    		METHODS test_rate_zero FOR TESTING.
    		METHODS test_rate_round FOR TESTING.
    		METHODS test_rate_no_data FOR TESTING.
ENDCLASS.

CLASS ltc_occupancy_rate IMPLEMENTATION.
	METHOD setup.
		cut = NEW zcl_demo_aunit_test_seams( ).
  	ENDMETHOD.

  	METHOD test_rate.
    		TEST-INJECTION select_from_db.
      			flight_data = VALUE #(
      				( seatsmax = 180 seatsocc = 135 )
      				( seatsmax = 220 seatsocc = 198 )
      				( seatsmax = 300 seatsocc = 280 ) ).
    		END-TEST-INJECTION.

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate_occupancy_rate( carrier_id = 'AA' )
				exp = CONV decfloat34( '87.57' )
				msg = 'AA occupancy rate should be 87.57.' ).
  	ENDMETHOD.

  	METHOD test_rate_full.
    		TEST-INJECTION select_from_db.
      			flight_data = VALUE #(
      				( seatsmax = 150 seatsocc = 150 )
      				( seatsmax = 120 seatsocc = 120 ) ).
    		END-TEST-INJECTION.

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate_occupancy_rate( carrier_id = 'BB' )
				exp = CONV decfloat34( '100' )
				msg = 'BB occupancy rate should be 100.' ).
  	ENDMETHOD.

  	METHOD test_rate_zero.
    		TEST-INJECTION select_from_db.
      			flight_data = VALUE #(
      				( seatsmax = 100 seatsocc = 0 )
      				( seatsmax = 50 seatsocc = 0 ) ).
    		END-TEST-INJECTION.

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate_occupancy_rate( carrier_id = 'CC' )
				exp = CONV decfloat34( '0' )
				msg = 'CC occupancy rate should be 0.' ).
  	ENDMETHOD.

  	METHOD test_rate_round.
    		TEST-INJECTION select_from_db.
      			flight_data = VALUE #(
      				( seatsmax = 300 seatsocc = 100 ) ).
    		END-TEST-INJECTION.

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate_occupancy_rate( carrier_id = 'DD' )
				exp = CONV decfloat34( '33.33' )
				msg = 'DD occupancy rate should be rounded to 33.33.' ).
  	ENDMETHOD.

  	METHOD test_rate_no_data.
    		TEST-INJECTION select_from_db.
      			flight_data = VALUE #( ).
    		END-TEST-INJECTION.

    		cl_abap_unit_assert=>assert_equals(
          act = cut->calculate_occupancy_rate( carrier_id = 'CC' )
	  exp = CONV decfloat34( '0' )
	  msg = 'No seam data should return initial occupancy rate.' ).
  	ENDMETHOD.
ENDCLASS.


**********************************************************************

CLASS ltc_test_seams DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
		DATA cut TYPE REF TO zcl_demo_aunit_test_seams.

		METHODS setup.
    METHODS test_test_seams1 FOR TESTING.
    METHODS test_test_seams2 FOR TESTING.
    METHODS no_injection FOR TESTING.
ENDCLASS.

CLASS ltc_test_seams IMPLEMENTATION.
	METHOD setup.
    cut = NEW zcl_demo_aunit_test_seams( ).
  ENDMETHOD.

  METHOD test_test_seams1.

    TEST-INJECTION ts1.
      num = 1.
    END-TEST-INJECTION.

    TEST-INJECTION ts2.
    END-TEST-INJECTION.

    cl_abap_unit_assert=>assert_equals(
        act = cut->test_seams_demo( )
		exp = `BC`
		msg = 'Injected ts1 and empty ts2 should return BC.' ).

  ENDMETHOD.

  METHOD test_test_seams2.

    TEST-INJECTION ts2.
      str = `E`.
    END-TEST-INJECTION.

    cl_abap_unit_assert=>assert_equals(
        act = cut->test_seams_demo( )
		exp = `AE`
		msg = 'Injected ts2 should override D and return AE.' ).
  ENDMETHOD.

  METHOD no_injection.
    cl_abap_unit_assert=>assert_equals(
            act = cut->test_seams_demo( )
		    exp = `AD`
		    msg = 'Without injections, default seam code should return AD.' ).
  ENDMETHOD.

ENDCLASS.


