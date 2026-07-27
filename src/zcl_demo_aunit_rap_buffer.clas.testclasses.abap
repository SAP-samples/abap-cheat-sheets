CLASS ltc_rap_buffer DEFINITION FINAL FOR TESTING
	DURATION SHORT
	RISK LEVEL HARMLESS.
  	PRIVATE SECTION.
    		CLASS-DATA cut TYPE REF TO zcl_demo_aunit_rap_buffer.
    		CLASS-DATA txbuf_env TYPE REF TO if_botd_txbufdbl_bo_test_env.
    		CLASS-DATA test_double TYPE REF TO if_botd_txbufdbl_test_double.
    		
    		CLASS-METHODS class_setup.
    		METHODS setup.
    		CLASS-METHODS class_teardown.

    		METHODS test_read_existing_keys FOR TESTING.
    		METHODS test_read_non_existing_carrier FOR TESTING.
    		METHODS test_read_no_data FOR TESTING.
ENDCLASS.

CLASS ltc_rap_buffer IMPLEMENTATION.

  	METHOD class_setup.
    		cut = NEW zcl_demo_aunit_rap_buffer( ).
    				
    		txbuf_env = cl_botd_txbufdbl_bo_test_env=>create(
          environment_config = cl_botd_txbufdbl_bo_test_env=>prepare_environment_config(
          )->set_bdef_dependencies( bdef_dependencies = VALUE #( ( 'ZRAUNITFLIGHTS' ) ) ) ).		
  	ENDMETHOD.

  	METHOD setup.
    		 txbuf_env->clear_doubles( ).

    test_double =  txbuf_env->get_test_double( 'ZRAUNITFLIGHTS' ).
  	ENDMETHOD.

  	METHOD class_teardown.
    		txbuf_env->destroy( ).
  	ENDMETHOD.

  	METHOD test_read_existing_keys.
  		DATA keys4create TYPE TABLE FOR CREATE zraunitflights.
  		keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                             ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                     ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 )
                     ( %cid = `cid3` carrid = 'AA' connid = '1003' fldate = '20260803' seatsmax = 95 seatsocc = 50 ) ).
    	
    	test_double->insert_test_data( instances = keys4create ).
    	
    cut->adapt_planetype(
        EXPORTING keys = CORRESPONDING #( keys4create )
        IMPORTING flights = DATA(flights)
                  failed_resp = DATA(failed_resp) ).
    	    	
    	 cl_abap_unit_assert=>assert_initial( act = failed_resp ).
    	 cl_abap_unit_assert=>assert_equals( act = lines( flights ) exp = 3 ).
    	 cl_abap_unit_assert=>assert_equals( act = flights[ KEY id carrid = 'AA' connid = '1001' fldate = '20260801' ]-Planetype exp = 'B' ).
    	 cl_abap_unit_assert=>assert_equals( act = flights[ KEY id carrid = 'AA' connid = '1002' fldate = '20260802' ]-Planetype exp = 'C' ).
    	 cl_abap_unit_assert=>assert_equals( act = flights[ KEY id carrid = 'AA' connid = '1003' fldate = '20260803' ]-Planetype exp = 'A' ).
  	ENDMETHOD.

  	METHOD test_read_non_existing_carrier.

    DATA keys4create TYPE TABLE FOR CREATE zraunitflights.

    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                             ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                     ( %cid = `cid2` carrid = 'BB' connid = '2001' fldate = '20260802' seatsmax = 220 seatsocc = 198 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->adapt_planetype(
        EXPORTING keys = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' )
                                  ( carrid = 'BB' connid = '2001' fldate = '20260802' )
                                  ( carrid = 'CC' connid = '3001' fldate = '20260803' )
                                  ( carrid = 'DD' connid = '4001' fldate = '20260804' ) )
        IMPORTING flights = DATA(flights)
                  failed_resp = DATA(failed_resp) ).

    cl_abap_unit_assert=>assert_equals( act = lines( failed_resp-zraunitflights ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( flights ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = flights[ KEY id carrid = 'AA' connid = '1001' fldate = '20260801' ]-Planetype exp = 'B' ).
    cl_abap_unit_assert=>assert_equals( act = flights[ KEY id carrid = 'BB' connid = '2001' fldate = '20260802' ]-Planetype exp = 'C' ).
  	ENDMETHOD.

  METHOD test_read_no_data.
    cut->adapt_planetype(
        EXPORTING keys = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' )
                                  ( carrid = 'BB' connid = '2001' fldate = '20260802' )
                                  ( carrid = 'CC' connid = '3001' fldate = '20260803' ) )
        IMPORTING flights = DATA(flights)
                  failed_resp = DATA(failed_resp) ).

    cl_abap_unit_assert=>assert_initial( act = flights ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_resp-zraunitflights ) exp = 3 ).
  ENDMETHOD.

ENDCLASS.

