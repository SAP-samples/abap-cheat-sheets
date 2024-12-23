"!@testing zcl_demo_abap_unit_tdf
CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    "------------------- Local test class -------------------

    "Object reference variable for class under test
    CLASS-DATA cut TYPE REF TO zcl_demo_abap_unit_tdf.

    "----- 1) ABAP OO Test Double Framework  ------
    "Test method that demonstrates constructor injection
    METHODS test_tdf_constr_inj_get_discnt FOR TESTING.

    "Test method that demonstrates parameter injection
    METHODS test_tdf_param_inj_get_discnt FOR TESTING.

    "----------- 2) ABAP SQL Test Double Framework -----------
    METHODS test_sql_get_shortest_flight FOR TESTING RAISING cx_static_check.
    CLASS-DATA sql_env TYPE REF TO if_osql_test_environment.

    "----------- 3) ABAP CDS Test Double Framework-----------
    METHODS test_select_cds FOR TESTING RAISING cx_static_check.
    METHODS test_cds_standalone FOR TESTING RAISING cx_static_check.
    METHODS prepare_testdata_set_cds.
    CLASS-DATA cds_env TYPE REF TO if_cds_test_environment.
    DATA zdemo_abap_fli_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

    "----------- 4) Managing dependencies on RAP business objects -----------
    CONSTANTS entity TYPE abp_entity_name VALUE 'ZDEMO_ABAP_RAP_RO_M'.
    CLASS-DATA eml_env TYPE REF TO if_botd_mockemlapi_bo_test_env.
    "--- 4a) Test methods that demonstrate mocking EML APIs ---
    METHODS test_eml_read_root FOR TESTING.
    METHODS test_eml_rba FOR TESTING.
    METHODS test_eml_modify_root FOR TESTING.
    DATA td_eml TYPE REF TO if_botd_mockemlapi_test_double.

    "--- 4b) Test method that demonstrates transactional buffer test doubles ---
    METHODS test_eml_read_root_buffer_td FOR TESTING.

    CLASS-DATA buffer_env TYPE REF TO if_botd_txbufdbl_bo_test_env.

    "----------- Test fixture -----------
    "Creating a common start state for each test method, clearing doubles
    METHODS setup RAISING cx_static_check.
    "Includes the creation of test environments
    CLASS-METHODS class_setup RAISING cx_static_check.
    "Clearing generated test doubles
    CLASS-METHODS class_teardown.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD class_setup.
    "Creating an instance for the class under test
    cut = NEW zcl_demo_abap_unit_tdf( ).

    "Creating instances of test environments for the test execution
    "------ 2) SQL ------
    sql_env = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #( ( 'zdemo_abap_flsch' ) ) ).

    "------ 3) CDS ------
    cds_env = cl_cds_test_environment=>create( i_for_entity = 'zdemo_abap_cds_ve_agg_exp'
                                               test_associations = 'X'  ).

    "------ 4) ABAP EML ------
    "4a) Mocking ABAP EML APIs
    eml_env = cl_botd_mockemlapi_bo_test_env=>create(
      environment_config = cl_botd_mockemlapi_bo_test_env=>prepare_environment_config(
      )->set_bdef_dependencies( bdef_dependencies = VALUE #( ( 'ZDEMO_ABAP_RAP_RO_M' ) ) ) ).

    "4b) Transactional buffer test doubles
    buffer_env = cl_botd_txbufdbl_bo_test_env=>create(
      environment_config = cl_botd_txbufdbl_bo_test_env=>prepare_environment_config(
      )->set_bdef_dependencies( bdef_dependencies = VALUE #( ( 'ZDEMO_ABAP_RAP_RO_U' ) ) ) ).

  ENDMETHOD.

  METHOD class_teardown.
    "Clearing generated test double at the end of the test execution
    sql_env->destroy( ).
    cds_env->destroy( ).
    eml_env->destroy( ).
    buffer_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    sql_env->clear_doubles( ).
    cds_env->clear_doubles( ).
    eml_env->clear_doubles( ).
    buffer_env->clear_doubles( ).
  ENDMETHOD.

  METHOD test_sql_get_shortest_flight.
    "--- 2) ---
    "Preparing and inserting test data
    DATA test_data TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY.
    test_data = VALUE #(
     ( carrid = 'LH'
       connid =  0401
       fltime = 435 )
     ( carrid = 'LH'
       connid =  0402
       fltime = 455 )
     ( carrid = 'LH'
       connid =  2402
       fltime = 65 ) ).

    sql_env->insert_test_data( test_data ).

    "Calling method of the class under test
    DATA carrier TYPE zdemo_abap_flsch-carrid VALUE 'LH'.
    DATA(result) = cut->sql_get_shortest_flight_time( carrier ).

    "Verifying result
    cl_abap_unit_assert=>assert_equals(
          act = result
          exp = 65
          msg = `Not the shortest flight`
          quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

  METHOD prepare_testdata_set_cds.
    "Preparing and inserting test data
    zdemo_abap_fli_tab = VALUE #(
        ( carrid = 'XX'
          connid =  0407
          fldate =  '20231128'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   458
          paymentsum =   '563231.65'
          seatsmax_b = 30
          seatsocc_b = 27
          seatsmax_f =  20
          seatsocc_f = 20 )
        ( carrid = 'XX'
          connid =  0407
          fldate =  '20231019'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   452
          paymentsum =   '553552.12'
          seatsmax_b = 30
          seatsocc_b = 28
          seatsmax_f =  20
          seatsocc_f = 19 )
        ( carrid = 'XX'
          connid =  0408
          fldate =  '20231128'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    '747-400'
          seatsmax =   385
          seatsocc =   365
          paymentsum =   '470129.20'
          seatsmax_b = 31
          seatsocc_b = 28
          seatsmax_f =  21
          seatsocc_f = 20 )
        ( carrid = 'XX'
          connid =  0408
          fldate =  '20230123'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    '747-400'
          seatsmax =   385
          seatsocc =   372
          paymentsum =   '487715.90'
          seatsmax_b = 31
          seatsocc_b = 31
          seatsmax_f =  21
          seatsocc_f = 20 ) ).

    cds_env->insert_test_data( i_data = zdemo_abap_fli_tab ).
  ENDMETHOD.

  METHOD test_select_cds.
    "--- 3) ---
    "Preparing and inserting test data
    prepare_testdata_set_cds( ).

    "Calling method of the class under test
    DATA carrier TYPE zdemo_abap_fli-carrid VALUE 'XX'.
    DATA(act_result) = cut->cds_get_data_set( carrier ).

    "Verifying result
    cl_abap_unit_assert=>assert_equals(
          act = act_result
          exp = VALUE zdemo_abap_cds_ve_agg_exp( carrid = 'XX' currency = 'JPY' avg_seats_occ = '411.75'
                                                 avg_paysum = '518657.22' total_paysum = '2074628.87'
                                                 min_occ_seats = 365 max_occ_seats = 458
                                                 max_occ_seats_all = 458 cnt = 4 cnt_planetype = 2 )
          msg = `The values do not match the expected result`
          quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

  METHOD test_eml_read_root.
    "--- 4a) ---
    "Mocking ABAP EML API (read request)

    "Preparing test data (RAP BO instances)
    DATA read_tab_ro_import TYPE TABLE FOR READ IMPORT zdemo_abap_rap_ro_m.
    DATA read_tab_ro_result TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m.
    read_tab_ro_import = VALUE #( ( key_field = 2  ) ).
    read_tab_ro_result = VALUE #( ( key_field = 2 field1 = 'uuu' field2 = 'vvv' field3 = 20 field4 = 200  ) ).

    "Configuring input and output for the ABAP EML read request
    "Creating input/output configuration builders
    DATA(input_config_read) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
    DATA(output_config_read) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    "Creating input for the entities (here, only one entity is included)
    "Instead of the entity name, an alias name is also accepted.
    DATA(eml_read_input) = input_config_read->build_entity_part( entity
          )->set_instances_for_read( read_tab_ro_import ).

    "Input configuration
    DATA(input) = input_config_read->build_input_for_eml(  )->add_entity_part( eml_read_input ).

    "Output configuration
    DATA(output) = output_config_read->build_output_for_eml( )->set_result_for_read( read_tab_ro_result ).

    "Configuring the RAP BO test double
    td_eml = eml_env->get_test_double( entity ).
    td_eml->configure_call(  )->for_read(  )->when_input( input )->then_set_output( output ).

    "Calling method (containing the ABAP EML read request) of the class under test
    DATA(t_read_res_ro) = cut->eml_read_root( key = 2 ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
        act = lines( t_read_res_ro )
        exp = 1
        msg = `The number of lines does not match`
        quit = if_abap_unit_constant=>quit-no ).

    TYPES struc_read_ro TYPE STRUCTURE FOR READ RESULT zdemo_abap_rap_ro_m.
    cl_abap_unit_assert=>assert_equals(
              act = t_read_res_ro[ 1 ]
              exp = VALUE struc_read_ro( key_field = 2 field1 = 'uuu' field2 = 'vvv' field3 = 20 field4 = 200  )
              msg = `The values do not match the expected result`
              quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

  METHOD test_tdf_constr_inj_get_discnt.
    "--- 1a) ---
    "Method that demonstrates the ABAP OO Test Double Framework
    "Injection mechanism: Constructor injection

    "Creating a test double
    DATA(test_double) = CAST zcl_demo_abap_unit_dataprov(
      cl_abap_testdouble=>create( 'zcl_demo_abap_unit_dataprov' ) ).

    "Configuring a method call
    "It is used for the next method call (the actual method calling)
    "In this example, the method only has a returning parameter. Check
    "the class documentation for more static methods of the cl_abap_testdouble
    "class, e.g. for configuring other parameters.
    cl_abap_testdouble=>configure_call( test_double
      )->returning( 5 ).

    "Calling method of the external class
    test_double->get_discount( ).

    "Injecting the test double
    "Here, the instance constructor is provided with the test double
    "instance
    cut = NEW #( test_double ).

    "Calling method of the class under test
    DATA(result) = cut->td_constr_inj_calc_discount( 500 ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = 475
      msg = `The values do not match the expected result`
      quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

  METHOD test_tdf_param_inj_get_discnt.
    "--- 1b) ---
    "Method that demonstrates the ABAP OO Test Double Framework
    "Injection mechanism: Parameter injection

    "Creating a test double
    DATA(test_double_param_inj) = CAST zcl_demo_abap_unit_dataprov(
      cl_abap_testdouble=>create( 'zcl_demo_abap_unit_dataprov' ) ).

    "Configuring a method call, ignoring importing parameters
    cl_abap_testdouble=>configure_call( test_double_param_inj
      )->ignore_parameter( 'DAY_VALUE'
      )->ignore_parameter( 'TIME_VALUE'
      )->returning( 40 ).

    "Calling method of the external class
    "Dummy values are provided for the non-optional importing
    "parameters (the returning value is determined above)
    test_double_param_inj->get_discount_value(
      EXPORTING
        day_value      = 99
        time_value     = 99
    ).

    "Calling method of the class under test
    "In this case, the optional parameter is assigned the
    "test double replacing the DOC
    cut->td_param_inj_calc_discount(
      EXPORTING
        value     = 100
        date     = '20241201' "Sunday
        time     = '100000'
        data_prov = test_double_param_inj
      IMPORTING
        message   = DATA(msg)
        weekday   = DATA(weekday)
      RECEIVING
        result    = DATA(result)
    ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = 60
      msg = `The values do not match the expected result`
      quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

  METHOD test_eml_rba.
    "--- 4a) ---
    "Method that demonstrates mocking an ABAP EML API (read-by-association request)

    "Preparing test data (RAP BO instances)
    DATA read_tab_ch_import TYPE TABLE FOR READ IMPORT zdemo_abap_rap_ro_m\_child.
    DATA read_tab_ch_result TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m\_child.

    read_tab_ch_import = VALUE #( ( key_field = 2 ) ).

    read_tab_ch_result = VALUE #( ( key_field = 2 key_ch = 22 field_ch1 = 'www' field_ch2 = 222  )
                                  ( key_field = 2 key_ch = 23 field_ch1 = 'xxx' field_ch2 = 223  ) ).

    "Configuring input and output for the ABAP EML read-by-association request
    DATA(input_config_rba) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
    DATA(output_config_rba) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    "Creating input for the entities (here, only one entity is included)
    DATA(eml_rba_input) = input_config_rba->build_entity_part( entity
         )->set_instances_for_read_ba( read_tab_ch_import ).

    "Input configuration
    DATA(input_rba) = input_config_rba->build_input_for_eml( )->add_entity_part( eml_rba_input ).

    "Output configuration
    DATA(output_rba) = output_config_rba->build_output_for_eml(
       )->set_result_for_read_ba(
         source_entity_name = entity
         assoc_name         = '_CHILD'
         result             = read_tab_ch_result ).

    "Configuring the RAP BO test double
    td_eml = eml_env->get_test_double( entity ).
    td_eml->configure_call( )->for_read( )->when_input( input_rba )->then_set_output( output_rba ).

    "Calling method (containing the ABAP EML read-by-association request)
    "of the class under test
    DATA(t_read_res_ch) = cut->eml_rba( key = 2 ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
          act = lines( t_read_res_ch )
          exp = 2
          msg = `The number of lines does not match`
          quit = if_abap_unit_constant=>quit-no ).

    TYPES struc_read_ch TYPE STRUCTURE FOR READ RESULT zdemo_abap_rap_ro_m\_child.
    cl_abap_unit_assert=>assert_equals(
              act = t_read_res_ch[ 2 ]
              exp = VALUE struc_read_ch( key_field = 2 key_ch = 23 field_ch1 = 'xxx' field_ch2 = 223  )
              msg = `The values do not match the expected result`
              quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.


  METHOD test_eml_modify_root.
    "--- 4a) ---
    "Method that demonstrates mocking an ABAP EML API (create request)

    "Data object delcarations used in the test method
    DATA create_tab_ro TYPE TABLE FOR CREATE zdemo_abap_rap_ro_m.
    DATA mapped_resp TYPE RESPONSE FOR MAPPED EARLY zdemo_abap_rap_ro_m.
    DATA failed_resp TYPE RESPONSE FOR FAILED EARLY zdemo_abap_rap_ro_m.
    DATA reported_resp TYPE RESPONSE FOR REPORTED EARLY zdemo_abap_rap_ro_m.

    "Preparing test data (RAP BO instances, response parameters)
    create_tab_ro = VALUE #( ( %cid = `cid1` key_field = 3 field1 = 'aaa' field2 = 'bbb' field3 = 30 field4 = 300  )
                             ( %cid = `cid2` key_field = 4 field1 = 'ccc' field2 = 'ddd' field3 = 40 field4 = 400  ) ).

    mapped_resp-root = VALUE #( ( %cid = 'cid1' key_field = 3  ) ( %cid = 'cid2' key_field = 4  ) ).
    failed_resp-root = VALUE #( ).
    reported_resp-root = VALUE #( ).

    "Configuring input and output for the ABAP EML create request
    DATA(input_config_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    DATA(output_config_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Creating input for the entities (here, only one entity is included)
    DATA(eml_modify_input) = input_config_modify->build_entity_part( entity
      )->set_instances_for_create( create_tab_ro ).

    "Input configuration
    DATA(input) = input_config_modify->build_input_for_eml( )->add_entity_part( eml_modify_input ).

    "Output configuration
    DATA(output) = output_config_modify->build_output_for_eml( )->set_mapped( mapped_resp
      )->set_failed( failed_resp )->set_reported( reported_resp ).

    "Configuring the RAP BO test double
    td_eml = eml_env->get_test_double( entity ).
    td_eml->configure_call( )->for_modify(  )->when_input( input )->then_set_output( output ).

    "Calling method (containing the ABAP EML read request) of the class under test
    cut->eml_modify_root(
      EXPORTING
        instances =  VALUE #( ( %cid = `cid1` key_field = 3 field1 = 'aaa' field2 = 'bbb' field3 = 30 field4 = 300  )
                              ( %cid = `cid2` key_field = 4 field1 = 'ccc' field2 = 'ddd' field3 = 40 field4 = 400  ) )
      IMPORTING
        mapped    = DATA(m)
        failed    = DATA(f)
        reported  = DATA(r)
    ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
          act = lines( m-root )
          exp = 2
          msg = `The number of lines does not match`
          quit = if_abap_unit_constant=>quit-no ).

    cl_abap_unit_assert=>assert_equals(
              act = lines( f-root )
              exp = 0
              msg = `The number of lines does not match`
              quit = if_abap_unit_constant=>quit-no ).

    cl_abap_unit_assert=>assert_equals(
              act = lines( r-root )
              exp = 0
              msg = `The number of lines does not match`
              quit = if_abap_unit_constant=>quit-no ).

  ENDMETHOD.

  METHOD test_eml_read_root_buffer_td.
    "--- 4b) ---
    "Method that demonstrates transactional buffer test doubles

    "Creating test double
    "Note: You have more configuration options. Check, for example, the
    "methods configure_additional_behavior, set_fields_handler methods, etc.
    "as described in the documentation.
    DATA(double) =  buffer_env->get_test_double( 'zdemo_abap_rap_ro_u' ).

    "Preparing test data
    "Populating the transactional buffer with an ABAP EML create request
    MODIFY ENTITIES OF zdemo_abap_rap_ro_u
      ENTITY root
       CREATE FIELDS ( key_field field1 field2 field3 field4 )
       WITH VALUE #( ( %cid = `cid5` key_field = 5 field1 = 'eee' )
                     ( %cid = `cid6` key_field = 6 field1 = 'fff' ) )
     REPORTED DATA(reported)
     FAILED DATA(failed)
     MAPPED DATA(mapped).

    "Calling method (containing an ABAP EML read request) of the class under test
    cut->eml_read_root_buffer_td(
      EXPORTING
        key    = 5
      RECEIVING
        tab_ro_u = DATA(read_result) ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
              act = read_result[ 1 ]-key_field
              exp = 5
              msg = `The value does not match the expected result`
              quit = if_abap_unit_constant=>quit-no ).

    cl_abap_unit_assert=>assert_equals(
                  act = read_result[ 1 ]-field1
                  exp = 'eee'
                  msg = `The value does not match the expected result`
                  quit = if_abap_unit_constant=>quit-no ).

    "Another calling of the method of the class under test (non-existent instance
    "in the transactional buffer test double)
    cut->eml_read_root_buffer_td(
      EXPORTING
        key    = 1
      RECEIVING
        tab_ro_u = DATA(read_result_f) ).

    cl_abap_unit_assert=>assert_initial(
      EXPORTING
        act  = read_result_f
        msg = `An instance was found`
        quit = if_abap_unit_constant=>quit-no ).

  ENDMETHOD.

  METHOD test_cds_standalone.
    "--- 3) ---
    "Method that tests the implementation logic of a CDS view entity
    "Unlike the 'test_select_cds' method, this method does not call a method
    "in the class under test.

    "Preparing test data
    prepare_testdata_set_cds( ).

    "Retrieving data from the CDS view entity (test data is used here)
    SELECT * FROM zdemo_abap_cds_ve_agg_exp INTO TABLE @DATA(result).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
               act = result[ 1 ]-carrid
               exp = 'XX'
               msg = `The value does not match the expected result`
               quit = if_abap_unit_constant=>quit-no ).

    cl_abap_unit_assert=>assert_equals(
                      act = result[ 1 ]-avg_seats_occ
                      exp = '411.75'
                      msg = `The value does not match the expected result`
                      quit = if_abap_unit_constant=>quit-no ).

    cl_abap_unit_assert=>assert_equals(
                      act = result[ 1 ]-max_occ_seats
                      exp = 458
                      msg = `The value does not match the expected result`
                      quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

ENDCLASS.
