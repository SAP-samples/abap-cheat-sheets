"! <p class="shorttext synchronized" lang="en">Local test class for the calculate_price method</p>
"! This test class contains scenarios that verify calculate_price for normal,
"! boundary, and invalid discount values.
"! The test method implementations demonstrate the ABAP OO Test Double Framework.
"! The injection mechanism used in the example is constructor injection.
CLASS ltc_calculate_price DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS setup.
    METHODS calculate_with_injected_double
      IMPORTING
        discount_percentage TYPE i
        current_price       TYPE decfloat34
      RETURNING
        VALUE(result)       TYPE decfloat34.

    METHODS assert_price
      IMPORTING
        act_price TYPE decfloat34
        exp_price TYPE decfloat34.

    METHODS test_discount_15 FOR TESTING.
    METHODS test_discount_0 FOR TESTING.
    METHODS test_discount_100 FOR TESTING.
    METHODS test_discount_negative FOR TESTING.
    METHODS test_discount_over_100 FOR TESTING.
    METHODS test_rounding_2_dec_a FOR TESTING.
    METHODS test_rounding_2_dec_b FOR TESTING.

    DATA test_double TYPE REF TO zif_demo_aunit_price.
ENDCLASS.


CLASS ltc_calculate_price IMPLEMENTATION.
  METHOD setup.
    "Creating a test double
    test_double = CAST zif_demo_aunit_price( cl_abap_testdouble=>create( 'zif_demo_aunit_price' ) ).
  ENDMETHOD.

  METHOD calculate_with_injected_double.
    cl_abap_testdouble=>configure_call( test_double )->returning( discount_percentage ).

    test_double->get_discount( ).

    DATA(cut) = NEW zcl_demo_aunit_abap_oo_tdf( test_double ).

    result = cut->calculate_price( current_price = current_price ).
  ENDMETHOD.

  METHOD assert_price.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = act_price
        exp = exp_price ).
  ENDMETHOD.

  METHOD test_discount_15.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 15
      current_price       = CONV decfloat34( '200.40' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '170.34' ) ).
  ENDMETHOD.

  METHOD test_discount_0.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 0
      current_price       = CONV decfloat34( '123.45' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '123.45' ) ).
  ENDMETHOD.

  METHOD test_discount_100.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 100
      current_price       = CONV decfloat34( '89.99' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '0.00' ) ).
  ENDMETHOD.

  METHOD test_discount_negative.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = -10
      current_price       = CONV decfloat34( '50.50' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '50.50' ) ).
  ENDMETHOD.

  METHOD test_discount_over_100.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 150
      current_price       = CONV decfloat34( '75.25' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '75.25' ) ).
  ENDMETHOD.

  METHOD test_rounding_2_dec_a.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 10
      current_price       = CONV decfloat34( '99.995' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '90.00' ) ).
  ENDMETHOD.

  METHOD test_rounding_2_dec_b.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 0
      current_price       = CONV decfloat34( '99.958' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '99.96' ) ).
  ENDMETHOD.
ENDCLASS.
