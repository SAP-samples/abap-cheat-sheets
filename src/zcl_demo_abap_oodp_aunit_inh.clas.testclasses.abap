*&---------------------------------------------------------------------*
*& Local test double class
*&---------------------------------------------------------------------*

"The example uses manually created test doubles. You may also want to
"check out creating test doubles using ABAP frameworks. Find more
"information in the ABAP Unit Tests cheat sheet.

CLASS ltd_test_data DEFINITION FOR TESTING
INHERITING FROM zcl_demo_abap_oodp_aunit_inh.
  PROTECTED SECTION.
    METHODS select_flight_data REDEFINITION.
ENDCLASS.

CLASS ltd_test_data IMPLEMENTATION.
  METHOD select_flight_data.

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
*& Test class demonstrating test double injection using inheritance
*& and method redefinition
*&---------------------------------------------------------------------*

CLASS ltc_test DEFINITION
  FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_get_occ_rate FOR TESTING.
ENDCLASS.

CLASS ltc_test IMPLEMENTATION.
  METHOD test_get_occ_rate.
    "Creating an object of the test double class that inherits
    "from the class under test
    DATA(ref_cut) = NEW ltd_test_data(  ).

    DATA(act_occ_rate) = ref_cut->get_occ_rate( 'IJ' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '70.00'
        msg = |The expected occupancy rate for carrier 'IJ' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).

    act_occ_rate = ref_cut->get_occ_rate( 'KL' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '80.00'
        msg = |The expected occupancy rate for carrier 'KL' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.
ENDCLASS.
