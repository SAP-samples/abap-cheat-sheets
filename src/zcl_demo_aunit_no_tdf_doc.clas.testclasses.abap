"! <p class="shorttext synchronized" lang="en">Local test double for discount retrieval</p>
"! This local test double provides the discount values needed for price calculation.
"! It partially implements the provider interface and uses a constructor importing parameter
"! to set the discount value. In this simplified setup, the value passed during object creation
"! is stored in a private attribute, and the get_discount implementation returns it.
"! This allows each test to create an object with a specific discount and inject it into the
"! code under test, eliminating external dependencies.
CLASS ltd_test_double_discount DEFINITION FOR TESTING
DURATION SHORT RISK LEVEL HARMLESS.
  	PUBLIC SECTION.
    		INTERFACES zif_demo_aunit_price.
    		METHODS constructor IMPORTING discount_value TYPE i.
  	PRIVATE SECTION.
    		DATA discount TYPE i.
ENDCLASS.

CLASS ltd_test_double_discount IMPLEMENTATION.
  METHOD constructor.
    discount = discount_value.
  ENDMETHOD.

  METHOD zif_demo_aunit_price~get_discount.
    discount_percentage = discount.
  ENDMETHOD.
ENDCLASS.

"! <p class="shorttext synchronized" lang="en">Local test class for the calculate_price method</p>
"! This test class contains scenarios that verify calculate_price for normal,
"! boundary, and invalid discount values.
CLASS ltc_calculate_price DEFINITION FINAL FOR TESTING
	DURATION SHORT
	RISK LEVEL HARMLESS.
  	PRIVATE SECTION.
      METHODS calculate_price_with_discount
        IMPORTING
          discount_value   TYPE i
          current_price_in TYPE decfloat34
        RETURNING
          VALUE(final_price) TYPE decfloat34.
      METHODS assert_price
        IMPORTING
          act_price TYPE decfloat34
          exp_price TYPE decfloat34
          msg       TYPE string.
    		METHODS test_discount_15 FOR TESTING.
    		METHODS test_discount_0 FOR TESTING.
    		METHODS test_discount_100 FOR TESTING.
    		METHODS test_discount_negative FOR TESTING.
    		METHODS test_discount_over_100 FOR TESTING.
    		METHODS test_rounding_2_dec_a FOR TESTING.
    		METHODS test_rounding_2_dec_b FOR TESTING.
ENDCLASS.

CLASS ltc_calculate_price IMPLEMENTATION.
  METHOD calculate_price_with_discount.
    DATA(cut) = NEW zcl_demo_aunit_no_tdf_doc(
      price = NEW ltd_test_double_discount( discount_value = discount_value ) ).

    final_price = cut->calculate_price( current_price = current_price_in ).
  ENDMETHOD.

  METHOD assert_price.
    cl_abap_unit_assert=>assert_equals(
      act = act_price
      exp = exp_price
      msg = msg ).
  ENDMETHOD.

  	METHOD test_discount_15.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 15
          current_price_in = CONV decfloat34( '200.40' ) )
        exp_price = CONV decfloat34( '170.34' )
        msg       = '15 percent discount should be applied.' ).
  	ENDMETHOD.

  	METHOD test_discount_0.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 0
          current_price_in = CONV decfloat34( '123.45' ) )
        exp_price = CONV decfloat34( '123.45' )
        msg       = '0 percent discount should keep current price unchanged.' ).
  	ENDMETHOD.

  	METHOD test_discount_100.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 100
          current_price_in = CONV decfloat34( '89.99' ) )
        exp_price = CONV decfloat34( '0.00' )
        msg       = '100 percent discount should result in zero.' ).
  	ENDMETHOD.

  	METHOD test_discount_negative.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = -10
          current_price_in = CONV decfloat34( '50.50' ) )
        exp_price = CONV decfloat34( '50.50' )
        msg       = 'Negative discount should be treated as invalid.' ).
  	ENDMETHOD.

  	METHOD test_discount_over_100.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 150
          current_price_in = CONV decfloat34( '75.25' ) )
        exp_price = CONV decfloat34( '75.25' )
        msg       = 'Discount above 100 should be treated as invalid.' ).
  	ENDMETHOD.

  	METHOD test_rounding_2_dec_a.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 10
          current_price_in = CONV decfloat34( '99.995' ) )
        exp_price = CONV decfloat34( '90.00' )
        msg       = 'Result should be rounded to 2 decimals.' ).
  	ENDMETHOD.
  	
  	METHOD test_rounding_2_dec_b.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 0
          current_price_in = CONV decfloat34( '99.958' ) )
        exp_price = CONV decfloat34( '99.96' )
        msg       = 'Rounding should also work with zero discount.' ).
  ENDMETHOD.	
ENDCLASS.

**********************************************************************

"! <p class="shorttext synchronized" lang="en">Local test double for flight data</p>
"! This local test double provides a manually created dataset for the code under test.
"! It populates a private internal table in the constructor and filters it by carrier
"! in the get_flight_data method. This way, tests can run with stable data and no
"! external dependencies.
CLASS ltd_test_double_occupancy DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    INTERFACES zif_demo_aunit_flights PARTIALLY IMPLEMENTED.
    METHODS constructor.
  PRIVATE SECTION.
    DATA flights TYPE zif_demo_aunit_flights=>t_flight_data.
ENDCLASS.

CLASS ltd_test_double_occupancy IMPLEMENTATION.
  METHOD constructor.

    flights = VALUE #( ( carrid = 'AA' seatsmax = 180 seatsocc = 135 )
                       ( carrid = 'AA' seatsmax = 220 seatsocc = 198 )
                       ( carrid = 'AA' seatsmax = 300 seatsocc = 280 )
                       ( carrid = 'BB' seatsmax = 150 seatsocc = 150 )
                       ( carrid = 'BB' seatsmax = 120 seatsocc = 120 )
                       ( carrid = 'CC' seatsmax = 100 seatsocc = 0 )
                       ( carrid = 'CC' seatsmax = 50 seatsocc = 0 )
                       ( carrid = 'DD' seatsmax = 3 seatsocc = 1 ) ).

  ENDMETHOD.

  METHOD zif_demo_aunit_flights~get_flight_data.
    LOOP AT flights ASSIGNING FIELD-SYMBOL(<flight>) WHERE carrid = carrier_id.
      APPEND <flight> TO flight_data.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

"! <p class="shorttext synchronized" lang="en">Local test class for the calculate_occupancy_rate method</p>
"! This test class contains scenarios that verify calculate_occupancy_rate for
"! full occupancy, empty occupancy, mixed loads, rounding behavior, and missing data,
"! using local test data.
"! The code-under-test reference is created once in setup with the occupancy test double.
CLASS ltc_occupancy_rate DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_demo_aunit_no_tdf_doc.

    METHODS setup.
    METHODS test_rate FOR TESTING.
    METHODS test_rate_full FOR TESTING.
    METHODS test_rate_zero FOR TESTING.
    METHODS test_rate_round FOR TESTING.
    METHODS test_rate_no_data FOR TESTING.
ENDCLASS.

CLASS ltc_occupancy_rate IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_demo_aunit_no_tdf_doc( flights = NEW ltd_test_double_occupancy( ) ).
  ENDMETHOD.

  METHOD test_rate.
    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'AA' )
      exp = CONV decfloat34( '87.57' )
      msg = 'AA occupancy rate should be 87.57.' ).
  ENDMETHOD.

  METHOD test_rate_full.
    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'BB' )
      exp = CONV decfloat34( '100' )
      msg = 'BB occupancy rate should be 100.' ).
  ENDMETHOD.

  METHOD test_rate_zero.
    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'CC' )
      exp = CONV decfloat34( '0' )
      msg = 'CC occupancy rate should be 0.' ).
  ENDMETHOD.

  METHOD test_rate_round.
    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'DD' )
      exp = CONV decfloat34( '33.33' )
      msg = 'DD occupancy rate should be rounded to 33.33.' ).
  ENDMETHOD.

  METHOD test_rate_no_data.
    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'XX' )
      exp = CONV decfloat34( '0' )
      msg = 'Missing carrier should return 0 occupancy rate.' ).
  ENDMETHOD.
ENDCLASS.
