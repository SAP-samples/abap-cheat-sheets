CLASS ltc_zraunitflights DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA cut TYPE REF TO lhc_zraunitflights.
    CLASS-DATA txbuf_env TYPE REF TO if_botd_txbufdbl_bo_test_env.
    CLASS-DATA test_double TYPE REF TO if_botd_txbufdbl_test_double.

    DATA keys4create TYPE TABLE FOR CREATE zraunitflights.
    DATA result TYPE TABLE FOR ACTION RESULT zraunitflights~calc_occ_rate.
    DATA mapped TYPE RESPONSE FOR MAPPED EARLY zraunitflights.
    DATA failed TYPE RESPONSE FOR FAILED EARLY zraunitflights.
    DATA reported TYPE RESPONSE FOR REPORTED EARLY zraunitflights.
    DATA failed_late TYPE RESPONSE FOR FAILED LATE zraunitflights.
    DATA reported_late TYPE RESPONSE FOR REPORTED LATE zraunitflights.

    CLASS-METHODS class_setup.
    METHODS setup.
    CLASS-METHODS class_teardown.

    METHODS test_calc_occ_rate_action FOR TESTING.
    METHODS test_calc_occ_rate_act_no_in FOR TESTING.

    METHODS test_val_invalid_smaxLT0 FOR TESTING.
    METHODS test_val_invalid_soccGTsmax FOR TESTING.
    METHODS test_val_invalid_soccLT0 FOR TESTING.
    METHODS test_val_accepts_valid FOR TESTING.

ENDCLASS.

CLASS ltc_zraunitflights IMPLEMENTATION.
  METHOD class_setup.
    CREATE OBJECT cut FOR TESTING.

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

  METHOD test_calc_occ_rate_action.

    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                            ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260701' seatsmax = 100 seatsocc = 80 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->calc_occ_rate(
      EXPORTING
        keys     =  CORRESPONDING #( keys4create )
      CHANGING
        result   = result
        mapped   = mapped
        failed   = failed
        reported = reported
    ).

    cl_abap_unit_assert=>assert_equals(
      act = result[ 1 ]-%param
      exp = CONV decfloat34( '80' ) ).

    cl_abap_unit_assert=>assert_initial( act = failed ).

  ENDMETHOD.

  METHOD test_calc_occ_rate_act_no_in.
    cut->calc_occ_rate(
         EXPORTING
           keys     = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260701' ) )
         CHANGING
           result   = result
           mapped   = mapped
           failed   = failed
           reported = reported
       ).

    cl_abap_unit_assert=>assert_initial( act = result ).
    cl_abap_unit_assert=>assert_not_initial( act = failed ).
  ENDMETHOD.


  METHOD test_val_invalid_soccGTsmax.

    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                           ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260701' seatsmax = 100 seatsocc = 120 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->val(
      EXPORTING
        keys     = CORRESPONDING #( keys4create )
      CHANGING
        failed   = failed_late
        reported = reported_late
    ).

    cl_abap_unit_assert=>assert_not_initial( act = failed_late ).
    cl_abap_unit_assert=>assert_not_initial( act = reported_late ).

    cl_abap_unit_assert=>assert_equals(
      act = failed_late-zraunitflights[ 1 ]-%key
      exp = keys4create[ 1 ]-%key ).

    cl_abap_unit_assert=>assert_equals(
          act = reported_late-zraunitflights[ 1 ]-%msg->if_t100_dyn_msg~msgv1
          exp = 'Validation failed' ).
  ENDMETHOD.

  METHOD test_val_accepts_valid.
    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                           ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260701' seatsmax = 120 seatsocc = 120 )
                           ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260702' seatsmax = 150 seatsocc = 120 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->val(
      EXPORTING
        keys     = CORRESPONDING #( keys4create )
      CHANGING
        failed   = failed_late
        reported = reported_late
    ).

    cl_abap_unit_assert=>assert_initial( act = failed_late ).
    cl_abap_unit_assert=>assert_initial( act = reported_late ).
  ENDMETHOD.

  METHOD test_val_invalid_smaxLT0.
    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                           ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260701' seatsmax = -10 seatsocc = 120 )
                           ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260702' seatsmax = -1 seatsocc = 120 )
                           ( %cid = `cid3` carrid = 'AA' connid = '1003' fldate = '20260703' seatsmax = 0 seatsocc = 0 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->val(
      EXPORTING
        keys     = CORRESPONDING #( keys4create )
      CHANGING
        failed   = failed_late
        reported = reported_late
    ).

    cl_abap_unit_assert=>assert_not_initial( act = failed_late ).
    cl_abap_unit_assert=>assert_not_initial( act = reported_late ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( failed_late-zraunitflights )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( reported_late-zraunitflights )
      exp = 2 ).
  ENDMETHOD.

  METHOD test_val_invalid_soccLT0.
    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                           ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260701' seatsmax = 100 seatsocc = -1 )
                           ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260702' seatsmax = 150 seatsocc = -120 )
                           ( %cid = `cid3` carrid = 'AA' connid = '1003' fldate = '20260703' seatsmax = 180 seatsocc = 0 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->val(
      EXPORTING
        keys     = CORRESPONDING #( keys4create )
      CHANGING
        failed   = failed_late
        reported = reported_late
    ).

    cl_abap_unit_assert=>assert_not_initial( act = failed_late ).
    cl_abap_unit_assert=>assert_not_initial( act = reported_late ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( failed_late-zraunitflights )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( reported_late-zraunitflights )
      exp = 2 ).
  ENDMETHOD.

ENDCLASS.
