CLASS ltc_eml DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA cut TYPE REF TO zcl_demo_aunit_rap_eml.
    CLASS-DATA eml_env TYPE REF TO if_botd_mockemlapi_bo_test_env.

    CLASS-METHODS class_setup.
    METHODS setup.
    CLASS-METHODS class_teardown.

    METHODS test_create FOR TESTING.
    METHODS test_create_failed FOR TESTING.
    METHODS test_create_no_test_double FOR TESTING.

    METHODS test_read FOR TESTING.
    METHODS test_read_failed FOR TESTING.
    METHODS test_read_no_test_double FOR TESTING.

ENDCLASS.

CLASS ltc_eml IMPLEMENTATION.

  METHOD class_setup.
    cut = NEW zcl_demo_aunit_rap_eml( ).

    eml_env = cl_botd_mockemlapi_bo_test_env=>create(
  environment_config = cl_botd_mockemlapi_bo_test_env=>prepare_environment_config(
  )->set_bdef_dependencies( bdef_dependencies = VALUE #( ( 'ZRAUNITFLIGHTS' ) ) ) ).
  ENDMETHOD.

  METHOD setup.
    eml_env->clear_doubles( ).
  ENDMETHOD.

  METHOD class_teardown.
    eml_env->destroy( ).
  ENDMETHOD.

  METHOD test_read.
    DATA read_import_tab TYPE TABLE FOR READ IMPORT zraunitflights.
    DATA read_result_tab TYPE TABLE FOR READ RESULT zraunitflights.

    read_import_tab = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' )
                               ( carrid = 'AA' connid = '1002' fldate = '20260802' )
                               ( carrid = 'AA' connid = '1003' fldate = '20260803' ) ).

    read_result_tab = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                     ( carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 )
                     ( carrid = 'AA' connid = '1003' fldate = '20260803' seatsmax = 95 seatsocc = 50 ) ).

    DATA(eml_read_entpart_config_bldr) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
    DATA(eml_read_output_config_bldr) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    DATA(eml_read_entpart) = eml_read_entpart_config_bldr->build_entity_part( 'ZRAUNITFLIGHTS' )->set_instances_for_read( read_import_tab ).

    "Input configuration
    DATA(read_input) = eml_read_entpart_config_bldr->build_input_for_eml(  )->add_entity_part( eml_read_entpart ).

    "Output configuration
    DATA(read_output) = eml_read_output_config_bldr->build_output_for_eml( )->set_result_for_read( read_result_tab ).

    "Configuring the RAP BO test double
    DATA(test_double) = eml_env->get_test_double( 'ZRAUNITFLIGHTS' ).
    test_double->configure_call(  )->for_read( )->when_input( read_input )->then_set_output( read_output ).

    cut->demo_eml_read(
      EXPORTING
        keys        = read_import_tab
      IMPORTING
        read_result = DATA(cut_read_result)
        failed_resp = DATA(cut_failed_resp)
    ).

    cl_abap_unit_assert=>assert_initial( cut_failed_resp ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut_read_result ) exp = 3 ).
    ASSIGN cut_read_result[ 1 ] TO FIELD-SYMBOL(<fs>).
    cl_abap_unit_assert=>assert_equals( act = <fs>-carrid exp = 'AA' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-connid exp = '1001' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-fldate exp = '20260801' ).
    test_double->verify( )->read( read_input )->is_called_times( times = 1 ).
  ENDMETHOD.

  METHOD test_read_failed.
    DATA read_import_tab TYPE TABLE FOR READ IMPORT zraunitflights.
    DATA read_result_tab TYPE TABLE FOR READ RESULT zraunitflights.
    DATA failed_resp TYPE RESPONSE FOR FAILED EARLY zraunitflights.

    read_import_tab = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' ) ).
    read_result_tab = VALUE #( ).
    failed_resp-zraunitflights = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' ) ).

    DATA(eml_read_entpart_conf_bl) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
    DATA(eml_read_output_config_bldr) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    DATA(eml_read_entpart) = eml_read_entpart_conf_bl->build_entity_part( 'ZRAUNITFLIGHTS' )->set_instances_for_read( read_import_tab ).

    "Input configuration
    DATA(read_input) = eml_read_entpart_conf_bl->build_input_for_eml(  )->add_entity_part( eml_read_entpart ).

    "Output configuration
    DATA(read_output) = eml_read_output_config_bldr->build_output_for_eml( )->set_result_for_read( read_result_tab )->set_failed( failed_resp ).

    "Configuring the RAP BO test double
    DATA(test_double) = eml_env->get_test_double( 'ZRAUNITFLIGHTS' ).
    test_double->configure_call(  )->for_read( )->when_input( read_input )->then_set_output( read_output ).

    cut->demo_eml_read(
      EXPORTING
        keys        = read_import_tab
      IMPORTING
        read_result = DATA(cut_read_result)
        failed_resp = DATA(cut_failed_resp)
    ).

    cl_abap_unit_assert=>assert_initial( cut_read_result ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut_failed_resp-zraunitflights ) exp = 1 ).
    ASSIGN cut_failed_resp-zraunitflights[ 1 ] TO FIELD-SYMBOL(<fs>).
    cl_abap_unit_assert=>assert_equals( act = <fs>-carrid exp = 'AA' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-connid exp = '1001' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-fldate exp = '20260801' ).
    test_double->verify( )->read( read_input )->is_called_times( times = 1 ).
  ENDMETHOD.

  METHOD test_read_no_test_double.
    DATA read_import_tab TYPE TABLE FOR READ IMPORT zraunitflights.

    read_import_tab = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' ) ).

    cut->demo_eml_read(
         EXPORTING
           keys        = read_import_tab
         IMPORTING
           read_result = DATA(cut_read_result)
           failed_resp = DATA(cut_failed_resp)
       ).

    cl_abap_unit_assert=>assert_initial( cut_read_result ).
    cl_abap_unit_assert=>assert_initial( cut_failed_resp ).
  ENDMETHOD.

  METHOD test_create.

    "Preparing test data (RAP BO instances, response parameters)
    DATA create_tab_ro TYPE TABLE FOR CREATE zraunitflights.
    DATA mapped_resp TYPE RESPONSE FOR MAPPED EARLY zraunitflights.

    create_tab_ro = VALUE #( ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                             ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 ) ).

    mapped_resp-zraunitflights = VALUE #( ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801'  )
                                          ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260802'  ) ).

    DATA(eml_create_entpart_config_bldr) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    DATA(eml_output_config_builder) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    DATA(eml_create_entpart) = eml_create_entpart_config_bldr->build_entity_part( 'ZRAUNITFLIGHTS'
                                                         )->set_instances_for_create( create_tab_ro ).

    DATA(input) = eml_create_entpart_config_bldr->build_input_for_eml(  )->add_entity_part( eml_create_entpart ).

    DATA(output) = eml_output_config_builder->build_output_for_eml( )->set_mapped( mapped_resp ).

    DATA(test_double) = eml_env->get_test_double( 'ZRAUNITFLIGHTS' ).
    test_double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).

    cut->demo_eml_modify(
      EXPORTING
        keys             = create_tab_ro
      IMPORTING
        mapped_resp      = DATA(cut_mapped_response)
        failed_resp     = DATA(cut_failed_response)
    ).

    cl_abap_unit_assert=>assert_initial(  cut_failed_response ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut_mapped_response-zraunitflights ) exp = 2 ).
    ASSIGN cut_mapped_response-zraunitflights[ 1 ] TO FIELD-SYMBOL(<fs>).
    cl_abap_unit_assert=>assert_equals( act = <fs>-%cid exp = `cid1` ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-carrid exp = 'AA' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-connid exp = '1001' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-fldate exp = '20260801' ).
    test_double->verify( )->modify( input )->is_called_times( times = 1 ).

  ENDMETHOD.

  METHOD test_create_failed.

    "Preparing test data (RAP BO instances, response parameters)
    DATA create_tab_ro TYPE TABLE FOR CREATE zraunitflights.
    DATA failed_resp TYPE RESPONSE FOR FAILED EARLY zraunitflights.

    create_tab_ro = VALUE #( ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 ) ).

    failed_resp-zraunitflights = VALUE #( ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801'  ) ).

    DATA(eml_create_entpart_config_bldr) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    DATA(eml_output_config_builder) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    DATA(eml_create_entpart) = eml_create_entpart_config_bldr->build_entity_part( 'ZRAUNITFLIGHTS' )->set_instances_for_create( create_tab_ro ).

    DATA(input) = eml_create_entpart_config_bldr->build_input_for_eml(  )->add_entity_part( eml_create_entpart ).

    DATA(output) = eml_output_config_builder->build_output_for_eml( )->set_failed( failed_resp ).

    DATA(test_double) = eml_env->get_test_double( 'ZRAUNITFLIGHTS' ).
    test_double->configure_call( )->for_modify( )->when_input( input )->then_set_output( output ).

    cut->demo_eml_modify(
      EXPORTING
        keys             = create_tab_ro
      IMPORTING
        mapped_resp      = DATA(cut_mapped_response)
        failed_resp     = DATA(cut_failed_response)
    ).

    cl_abap_unit_assert=>assert_initial( cut_mapped_response ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut_failed_response-zraunitflights ) exp = 1 ).
    ASSIGN cut_failed_response-zraunitflights[ 1 ] TO FIELD-SYMBOL(<fs>).
    cl_abap_unit_assert=>assert_equals( act = <fs>-%cid exp = `cid1` ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-carrid exp = 'AA' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-connid exp = '1001' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-fldate exp = '20260801' ).
    test_double->verify( )->modify( input )->is_called_times( times = 1 ).

  ENDMETHOD.

  METHOD test_create_no_test_double.

    cut->demo_eml_modify(
          EXPORTING
            keys             = VALUE #( ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                                        ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 ) )
          IMPORTING
            mapped_resp      = DATA(cut_mapped_response)
            failed_resp     = DATA(cut_failed_response)
        ).

    cl_abap_unit_assert=>assert_initial( cut_mapped_response ).
    cl_abap_unit_assert=>assert_initial( cut_failed_response ).

  ENDMETHOD.

ENDCLASS.
