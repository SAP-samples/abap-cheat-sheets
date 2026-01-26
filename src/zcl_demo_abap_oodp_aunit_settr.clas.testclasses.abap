*&---------------------------------------------------------------------*
*& Local test double class
*&---------------------------------------------------------------------*

"The example uses manually created test doubles. You may also want to
"check out creating test doubles using ABAP frameworks. Find more
"information in the ABAP Unit Tests cheat sheet.

CLASS ltd_test_data DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_get_data PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd_test_data IMPLEMENTATION.
  METHOD lif_get_data~select_flight_data.

    flight_data = SWITCH #( carrier
      WHEN 'IJ' THEN VALUE #( ( carrid = carrier  seatsmax = 300 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 300 ) )
      WHEN 'KL' THEN VALUE #( ( carrid = carrier  seatsmax = 350 seatsocc = 300 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 250 )
                              ( carrid = carrier  seatsmax = 300 seatsocc = 250 ) ) ).

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Test class demonstrating setter injection
*&---------------------------------------------------------------------*

"Note that the example uses a local interface.

CLASS ltc_test DEFINITION
  FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_get_occ_rate FOR TESTING.
ENDCLASS.

"In this example, a test class is created in the test include. Since private
"attributes are not accessible in local test classes, the local test class
"is declared as a local friend of the global class.
CLASS zcl_demo_abap_oodp_aunit_settr DEFINITION LOCAL FRIENDS ltc_test.

CLASS ltc_test IMPLEMENTATION.
  METHOD test_get_occ_rate.
    DATA(ref_cut) = NEW zcl_demo_abap_oodp_aunit_settr( ).
    DATA ref_data_prov TYPE REF TO lif_get_data.
    ref_data_prov = NEW ltd_test_data( ).
    "Passing the test double as a parameter of a setter method
    ref_cut->setter_meth( ref_data_prov ).

    DATA(act_occ_rate) = ref_cut->get_occ_rate( carrier_id = 'IJ' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '70.00'
        msg = |The expected occupancy rate for carrier 'IJ' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).

    act_occ_rate = ref_cut->get_occ_rate( carrier_id = 'KL' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '80.00'
        msg = |The expected occupancy rate for carrier 'KL' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.
ENDCLASS.
