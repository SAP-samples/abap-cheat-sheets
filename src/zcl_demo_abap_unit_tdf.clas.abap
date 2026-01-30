"! <p class="shorttext"><strong>Creating Test Doubles Using ABAP Frameworks</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates managing dependencies (dependent-on-components, DOC) with ABAP Unit and
"! explores the creation of test doubles using ABAP frameworks. Additionally, the example explores test classes
"! not contained in the class to be tested, but rather in an external class.<br/>
"! Choose F9 in ADT to run the class. To run all unit tests of the class, choose Ctrl/Cmd + Shift + F10.<br/>
"! If the unit tests have not yet run, right-click the <em>Foreign Tests</em> entry in the ABAP Unit tab of ADT,
"! and choose <em>Run</em>.</p>
"!
"! <h2>Topics covered</h2>
"! <ul><li>ABAP OO Test Double Framework (test doubles injected using constructor and parameter injection)</li>
"! <li>ABAP SQL Test Double Framework</li>
"! <li>ABAP CDS Test Double Framework</li>
"! <li>Managing dependencies on RAP business objects (mocking ABAP EML APIs, transactional buffer test doubles)</li>
"! <li>This class does not include a test class in the Test Classes tab. The ABAP Unit example demonstrates the syntax
"! <em>&quot;! &#64;testing ...</em>. By using this syntax and specifying a class name, you can define a test relationship
"! between the test class and another class. Therefore, if you run unit tests for this class ({@link zcl_demo_abap_unit_tdf}),
"! it executes the test class within {@link zcl_demo_abap_unit_tdf}, which contains the syntax to establish the test
"! linkage.</li></ul>
"!
"! <h2>Running ABAP Unit tests</h2>
"! <ol><li>Open the class with the ABAP development tools for Eclipse (ADT).</li>
"! <li>Choose Ctrl/Cmd + Shift + F10 to launch all tests.
"! You can also right-click somewhere in the class and choose Run as -> ABAP Unit Test.
"! <li>As the test class is outside of this class in class {@link zcl_demo_abap_unit_tdf},
"! the ABAP Unit tab will show a <em>Foreign Tests</em> entry. If the unit tests have not run,
"! right click, the <em>Foreign Tests</em> entry, and choose <em>Run</em>.</li>
"! <li>The results of a test run are displayed in the ABAP Unit tab in ADT
"! and can be evaluated. The Failure Trace section provides information
"! on errors found.</li>
"! <li>If you are interested in test coverage, you can choose Ctrl/Cmd + Shift + F11,
"! or make a right-click, choose Run as -> ABAP Unit Test With..., select the Coverage
"! checkbox and choose Execute. You can then check the results in the ABAP Coverage tab,
"! what code is tested and what not.</li>
"! </ol>
"!
"! <h2>Information</h2>
"! <p>Find information on getting started with the example class and the disclaimer in
"! the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
CLASS zcl_demo_abap_unit_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    "--------------- 1) ABAP OO Test Double Framework ---------------
    "The examples in the test include use the cl_abap_testdouble class.

    "----------- 1a) Demonstrating constructor injection -----------

    "Specifying the instance constructor with an optional parameter
    "for the purpose of constructor injection as injection mechanism.
    "In this example, methods are called from another, non-final class.
    "They represent DOCs. Test doubles are injected when running unit
    "tests. The parameter expects an instance of this class.
    METHODS constructor
      IMPORTING oref_constr_inj TYPE REF TO zcl_demo_abap_unit_dataprov OPTIONAL.

    "Declaring an object reference variable that will be used to call
    "methods of an external class (the DOC)
    DATA oref_data_provider TYPE REF TO zcl_demo_abap_unit_dataprov.

    "Method that is tested and contains a DOC. In this case, it is an
    "external method that is called in the implementation part. The DOC
    "is replaced by a test double created using cl_abap_testdouble.
    METHODS td_constr_inj_calc_discount
      IMPORTING
        value         TYPE numeric
      RETURNING
        VALUE(result) TYPE decfloat34.

    "----------- 1b) Demonstrating parameter injection -----------

    "Method that is tested and contains a DOC. In this case, it is an
    "external method that is called in the implementation part. The DOC
    "is replaced by a test double created using cl_abap_testdouble.
    "The example demonstrates parameter injection. Therefore, an
    "optional importing parameter is included. When running the unit
    "test, 'data_prov' is bound, and the test double is injected.
    METHODS td_param_inj_calc_discount
      IMPORTING value         TYPE numeric
                date          TYPE d
                time          TYPE t
                data_prov     TYPE REF TO zcl_demo_abap_unit_dataprov OPTIONAL
      EXPORTING message       TYPE string
                weekday       TYPE string
      RETURNING VALUE(result) TYPE decfloat34.

    "----------- 2) ABAP SQL Test Double Framework -----------
    "The examples in the test include use the cl_osql_test_environment
    "class. Here, a database table represents the DOC.
    "The method includes a SELECT statement.

    METHODS sql_get_shortest_flight_time IMPORTING carrier                TYPE zdemo_abap_flsch-carrid
                                         RETURNING VALUE(shortest_flight) TYPE i.

    "----------- 3) ABAP CDS Test Double Framework -----------
    "The examples in the test include use the cl_cds_get_data_set_environment class.
    "Here, a CDS view entity represents the DOC. The method includes a SELECT
    "statement. Another test method is implemented in the test inlcude that
    "demonstrates the testing of a CDS view entity (without testing a method in the
    "class under test).
    METHODS cds_get_data_set IMPORTING carrier         TYPE zdemo_abap_cds_ve_agg_exp-carrid
                             RETURNING VALUE(agg_line) TYPE zdemo_abap_cds_ve_agg_exp.

    "----------- 4) Managing dependencies on RAP business objects -----------
    "----------- 4a) Demonstrating mocking ABAP EML APIs --------------------
    "----------- ABAP EML read operations -----------------------------------
    "The examples in the test include use the cl_botd_mockemlapi_bo_test_env class.

    "Populating database tables for ABAP EML read requests
    METHODS prep_dbtab_for_eml IMPORTING read_op TYPE abap_boolean DEFAULT abap_false.
    TYPES read_tab_ro TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m.
    TYPES read_tab_ch TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m\_child.

    "Method that includes an ABAP EML read request on the root entity
    METHODS eml_read_root IMPORTING key           TYPE zdemo_abap_rap_ro_m-key_field
                          RETURNING VALUE(tab_ro) TYPE read_tab_ro.

    "Method that includes an ABAP EML read-by-associaton request
    METHODS eml_rba IMPORTING key           TYPE zdemo_abap_rap_ro_m-key_field
                    RETURNING VALUE(tab_ch) TYPE read_tab_ch.

    "----------- ABAP EML modify operation -----------
    TYPES modify_tab_ro TYPE TABLE FOR CREATE zdemo_abap_rap_ro_m.
    TYPES ty_mapped TYPE RESPONSE FOR MAPPED EARLY zdemo_abap_rap_ro_m.
    TYPES ty_failed TYPE RESPONSE FOR FAILED EARLY zdemo_abap_rap_ro_m.
    TYPES ty_reported TYPE RESPONSE FOR REPORTED EARLY zdemo_abap_rap_ro_m.

    "Method that includes an ABAP EML modify request on the root entity
    METHODS eml_modify_root IMPORTING VALUE(instances) TYPE modify_tab_ro
                            EXPORTING mapped           TYPE ty_mapped
                                      failed           TYPE ty_failed
                                      reported         TYPE ty_reported.

    "----------- 4b) Demonstrating transactional buffer test doubles ---------
    "The examples in the test include use the cl_botd_txbufdbl_bo_test_env class.
    TYPES read_tab_ro_u TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_u.

    "Method that includes an ABAP EML read request on the root entity
    METHODS eml_read_root_buffer_td IMPORTING key             TYPE zdemo_abap_rap_ro_u-key_field
                                    RETURNING VALUE(tab_ro_u) TYPE read_tab_ro_u.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_unit_tdf IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: Creating Test Doubles Using ABAP Frameworks\n| ).

    out->write( `*********************************************************************` ).
    out->write( |*                                                                   *| ).
    out->write( `* ---> Choose Ctrl/Cmd + Shift + F10 to launch all unit tests  <--- *` ).
    out->write( |*                                                                   *| ).
    out->write( `* As the example is set up with a test class outside of this class  *` ).
    out->write( `* and if the unit tests have not yet run, right-click the Foreign   *` ).
    out->write( `* Tests entry in the ABAP Unit tab of ADT, and choose Run.          *` ).
    out->write( |*                                                                   *| ).
    out->write( |*********************************************************************\n\n| ).

    "This implementation includes method calls of those methods that are unit tested
    "in the test include.
    "The implementation is just for demonstration purposes to explore the effect of
    "the methods.

    "Populating demo database tables (only required for running the example class
    "with F9 and exploring the effect of various method calls)
    zcl_demo_abap_aux=>fill_dbtabs( ).

    "----- 1) Methods demonstrating the ABAP OO Test Double Framework in the test include -----
    "----- 1a) Using constructor injection -----
    DATA(td_constr_inj_calc_discount) = td_constr_inj_calc_discount( 100 ).
    out->write( data = td_constr_inj_calc_discount name = `td_constr_inj_calc_discount` ).
    out->write( |\n| ).

    td_constr_inj_calc_discount = td_constr_inj_calc_discount( 500 ).
    out->write( data = td_constr_inj_calc_discount name = `td_constr_inj_calc_discount` ).
    out->write( |\n| ).

    td_constr_inj_calc_discount = td_constr_inj_calc_discount( CONV decfloat34( '3.4' ) ).
    out->write( data = td_constr_inj_calc_discount name = `td_constr_inj_calc_discount` ).
    out->write( |\n| ).

    "----- 1a) Using parameter injection -----
    "Example with Sunday
    td_param_inj_calc_discount(
      EXPORTING
        value     = 100
        date     = '20241201'
        time     = '100000'
      IMPORTING
       weekday   = DATA(weekday)
       message   = DATA(message)
      RECEIVING
        result    = DATA(td_param_inj_calc_discount)
    ).

    out->write( data = td_param_inj_calc_discount name = `td_param_inj_calc_discount` ).
    out->write( data = weekday name = `weekday` ).
    out->write( data = message name = `message` ).
    out->write( |\n| ).

    "Example with a weekday, morning
    td_param_inj_calc_discount(
     EXPORTING
       value     = 100
       date     = '20241202'
       time     = '100000'
     IMPORTING
      weekday   = weekday
      message   = message
     RECEIVING
       result    = td_param_inj_calc_discount
   ).

    out->write( data = td_param_inj_calc_discount name = `td_param_inj_calc_discount` ).
    out->write( data = weekday name = `weekday` ).
    out->write( data = message name = `message` ).
    out->write( |\n| ).

    "Example with a weekday, afternoon
    td_param_inj_calc_discount(
     EXPORTING
       value    = 100
       date     = '20241203'
       time     = '150000'
     IMPORTING
      weekday   = weekday
      message   = message
     RECEIVING
       result    = td_param_inj_calc_discount
   ).

    out->write( data = td_param_inj_calc_discount name = `td_param_inj_calc_discount` ).
    out->write( data = weekday name = `weekday` ).
    out->write( data = message name = `message` ).
    out->write( |\n| ).

    "Example with a weekday, night
    td_param_inj_calc_discount(
     EXPORTING
       value    = 100
       date     = '20241204'
       time     = '230000'
     IMPORTING
      weekday   = weekday
      message   = message
     RECEIVING
       result    = td_param_inj_calc_discount
   ).

    out->write( data = td_param_inj_calc_discount name = `td_param_inj_calc_discount` ).
    out->write( data = weekday name = `weekday` ).
    out->write( data = message name = `message` ).
    out->write( |\n| ).

    "----- 2) Methods demonstrating the ABAP SQL Test Double Framework in the test include -----
    DATA(sql_shortest_flight_time) = sql_get_shortest_flight_time( 'LH' ).
    out->write( data = sql_shortest_flight_time name = `sql_shortest_flight_time` ).
    out->write( |\n| ).

    sql_shortest_flight_time = sql_get_shortest_flight_time( 'AA' ).
    out->write( data = sql_shortest_flight_time name = `sql_shortest_flight_time` ).
    out->write( |\n| ).

    "----- 3) Methods demonstrating the ABAP CDS Test Double Framework in the test include -----
    DATA(data_set_cds) = cds_get_data_set( 'LH' ).
    out->write( data = data_set_cds name = `data_set_cds` ).
    out->write( |\n\n| ).

    data_set_cds = cds_get_data_set( 'AA' ).
    out->write( data = data_set_cds name = `data_set_cds` ).
    out->write( |\n\n| ).

    "----- Methods demonstrating mocking ABAP EML APIs in the test include -----
    "Populating demo database tables
    prep_dbtab_for_eml( read_op = abap_true ).

    DATA(tab_ro) = eml_read_root( key = 1 ).
    out->write( tab_ro ).
    out->write( |\n\n| ).

    DATA(tab_ch) = eml_rba( key = 1 ).
    out->write( tab_ch ).
    out->write( |\n\n| ).

    prep_dbtab_for_eml( ).

    eml_modify_root(
      EXPORTING
        instances =  VALUE #( ( %cid = `cid1` key_field = 3 field1 = 'aaa' field2 = 'bbb' field3 = 30 field4 = 300  )
                              ( %cid = `cid2` key_field = 4 field1 = 'ccc' field2 = 'ddd' field3 = 40 field4 = 400  ) )
      IMPORTING
        mapped    = DATA(m)
        failed    = DATA(f)
        reported  = DATA(r)
    ).

    ASSERT f IS INITIAL.
    ASSERT r IS INITIAL.
    out->write( data = m-root name = `m-root` ).
    out->write( |\n\n| ).

    SELECT * FROM zdemo_abap_rapt1 INTO TABLE @DATA(itab).
    out->write( data = itab name = `itab` ).
  ENDMETHOD.

  METHOD constructor.
    "--------------------- 1a) ---------------------
    "Demonstrating the constructor injection
    "The parameter is only bound when you run the unit test.
    "In that case, the test double is injected, and method calls
    "in the implementations use data from the test double.
    "Otherwise, a new instance is created that is used to call
    "methods (a test double is not injected).
    IF oref_constr_inj IS BOUND.
      oref_data_provider = oref_constr_inj.
    ELSE.
      oref_data_provider = NEW #( ).
    ENDIF.
  ENDMETHOD.

  METHOD td_constr_inj_calc_discount.
    "--------------------- 1a) ---------------------
    "Method that demonstrates the ABAP OO Test Double Framework and
    "constructor injection in ABAP Unit.
    "When running the unit test, 'oref_data_provider' includes the test
    "double. The method expects a numeric value. 'get_discount' returns
    "another numeric value on whose basis a discount calculation is
    "performed. The value returned by the 'get_discount' method depends
    "on the current weekday and the UTC time.
    result = ( value * oref_data_provider->get_discount( ) ) / 100.
    result = value - result.
  ENDMETHOD.

  METHOD td_param_inj_calc_discount.
    "--------------------- 1b) ---------------------
    "Method that demonstrates the ABAP OO Test Double Framework and
    "parameter injection in ABAP Unit.
    "When running the unit test, the optional parameter 'data_prov' is
    "assigned, and therefore bound here. 'oref_data_provider' then includes
    "the test double.
    "The purpose of this method is similar to 'td_constr_inj_calc_discount'.
    "Here, the method expects a date and time besides a numeric value.
    IF data_prov IS BOUND.
      oref_data_provider = data_prov.
    ENDIF.

    "Getting the weekday
    DATA(day_value) = ( 5 + date MOD 7 ) MOD 7 + 1.
    weekday = SWITCH #( day_value
                        WHEN 1 THEN `Monday`
                        WHEN 2 THEN `Tuesday`
                        WHEN 3 THEN `Wednesday`
                        WHEN 4 THEN `Thursday`
                        WHEN 5 THEN `Friday`
                        WHEN 6 THEN `Saturday`
                        WHEN 7 THEN `Sunday` ).

    DATA(time_value) = COND #( WHEN time BETWEEN '000000' AND '045959' THEN 1 "Night discount
                               WHEN time BETWEEN '220000' AND '235959' THEN 1 "Night discount
                               WHEN time BETWEEN '050000' AND '115959' THEN 2 "Morning discount
                               WHEN time BETWEEN '180000' AND '215959' THEN 3 "Evening discount
                               ELSE 0                                         "No discount
                       ).

    weekday = |{ weekday } ({ SWITCH #( time_value WHEN 1 THEN `night` WHEN 2 THEN `morning` WHEN 3 THEN `evening` ELSE `afternoon` ) })|.

    DATA(disc) = oref_data_provider->get_discount_value( day_value = day_value time_value = time_value ).
    result = ( value * disc ) / 100.
    result = value - result.
    message = |Original value: { value }; discount: { disc }; value with discount: { result }; | &&
    |when: { weekday }, { date DATE = ISO }, { time TIME = ISO }|.
  ENDMETHOD.

  METHOD sql_get_shortest_flight_time.
    "--------------------- 2) ---------------------
    "When running the unit test, the DOC (database table) is replaced with a test double.

    "Getting the shortest flight time among a given carrier
    SELECT MIN( fltime ) AS fltime FROM zdemo_abap_flsch WHERE carrid = @carrier INTO @shortest_flight.
  ENDMETHOD.

  METHOD cds_get_data_set.
    "--------------------- 3) ---------------------
    "When running the unit test, the DOC (CDS view entity) is replaced with a test double.

    "Getting a line filtered by the carrier
    SELECT SINGLE * FROM zdemo_abap_cds_ve_agg_exp WHERE carrid = @carrier INTO @agg_line.
  ENDMETHOD.

  METHOD prep_dbtab_for_eml.
    "Preparing database tables for ABAP EML statements
    DELETE FROM zdemo_abap_rapt1.

    IF read_op = abap_true.
      DELETE FROM zdemo_abap_rapt2.
      INSERT zdemo_abap_rapt1 FROM TABLE @( VALUE #( ( key_field = 1 field1 = 'aaa' field2 = 'bbb' field3 = 10 field4 = 100  ) ) ).
      INSERT zdemo_abap_rapt2 FROM TABLE @( VALUE #( ( key_field = 1 key_ch = 11 field_ch1 = 'ccc' field_ch2 = 111  )
                                                     ( key_field = 1 key_ch = 12 field_ch1 = 'ddd' field_ch2 = 112  ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD eml_read_root.
    "--------------------- 4a) ---------------------
    "Method that includes an ABAP EML read request on the root entity
    "When running the unit test, the ABAP EML API is mocked.

    READ ENTITIES OF zdemo_abap_rap_ro_m
      ENTITY root
      ALL FIELDS WITH VALUE #( ( key_field = key ) )
      RESULT tab_ro.
  ENDMETHOD.

  METHOD eml_rba.
    "--------------------- 4a) ---------------------
    "Method that includes an ABAP EML read-by-assocation request
    "When running the unit test, the ABAP EML API is mocked.

    READ ENTITIES OF zdemo_abap_rap_ro_m
       ENTITY root
       BY \_child
       ALL FIELDS WITH VALUE #( ( key_field = key ) )
       RESULT tab_ch.
  ENDMETHOD.

  METHOD eml_modify_root.
    "--------------------- 4a) ---------------------
    "Method that includes an ABAP EML create request
    "When running the unit test, the ABAP EML API is mocked.

    MODIFY ENTITIES OF zdemo_abap_rap_ro_m
      ENTITY root
      CREATE
      FIELDS ( key_field field1 field2 field3 field4 )
      WITH instances
      MAPPED mapped
      FAILED failed
      REPORTED reported.

    COMMIT ENTITIES.
  ENDMETHOD.

  METHOD eml_read_root_buffer_td.
    "--------------------- 4b) ---------------------
    "Method that includes an ABAP EML read request
    "When running the unit test, a transactional buffer test double is
    "included.

    READ ENTITIES OF zdemo_abap_rap_ro_u
      ENTITY root
      ALL FIELDS WITH VALUE #( ( key_field = key ) )
      RESULT tab_ro_u.
  ENDMETHOD.

ENDCLASS.
