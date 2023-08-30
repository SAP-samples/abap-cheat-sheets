***********************************************************************
*                  Test Class Overview
*
* Note:
* - This test include contains multiple test classes and methods
*   for unit testing the production code in the global class.
* - To run the unit test, choose CTRL/CMD + Shift + F10. See
*   also the information in the global class (RUN ABAP UNIT TEST at the top).
* - Almost all test classes deliberately contain test methods which
*   produce errors in the test and that can be checked in the ABAP Unit tab.
*   Likewise, test methods are included for which the test does not fail.
* - All assertion methods include the parameters MSG (for displaying an
*   error text in the test results) and QUIT (which is specified to not
*   terminate test).
*
* Test classes
* Note the comments before the individual class declaration parts.
*
* - ltc_test_simple_1
*    - Simple example testing only one method; no special methods,
*      no dependent-on component (DOC).
*
* - ltc_test_simple_2
*    - Testing multiple simple methods, no DOCs
*    - Special methods setup and teardown
*
*- ltc_test_dummy
*    - Does not contain any methods of the production code to be tested.
*    - Demonstrates the use of various static methods of the cl_abap_unit_assert class
*
*- ltc_test_seam
*    - Testing one method for which a DOC has been identified
*    - Demonstrates the use of test seams and their injection into the production code.
*
*- ltc_test_doc_global_itf
*    - Testing one method for which a DOC has been identified and for which a global
*      interface exists
*    - A local test double class is included
*    - Demonstrates constructor injection as injection mechanism
*
*- ltc_test_local_itf
*    - Testing one method for which a DOC has been identified
*    - There is no global interface available. Instead, a local interface is created.
*    - A local test double class is included
*    - Demonstrates back door injection as injection mechanism
*
*- ltc_test_redef
*    - Testing one method for which a DOC has been identified
*    - A local test double class is created by redefining a method of
*      the class under test.
*
*- ltc_test_doc_setter_inj
*    - Testing one method for which a DOC has been identified and for which a global
*      interface exists
*    - A local test double class is included
*    - Demonstrates setter injection as injection mechanism
*
*- ltc_test_doc_param_inj
*    - Testing one method for which a DOC has been identified and for which a global
*      interface exists
*    - A local test double class is included
*    - Demonstrates parameter injection as injection mechanism
*
***********************************************************************

"In this example, multiple test classes are created in the test include.
"Because private attributes are not accessible in local test classes,
"the local test classes are declared as local friends of the global class.
"In the example, a combined friendship declaration for all test classes is placed
"at the top of the test include. Prepending the friendship declaration with
"test class definitions and the DEFERRED addition makes the the test classes
"'known' at this stage and can thus be specified as local friends there.
CLASS ltc_test_simple_1 DEFINITION DEFERRED.
CLASS ltc_test_simple_2 DEFINITION DEFERRED.
CLASS ltc_test_doc_seam DEFINITION DEFERRED.
CLASS ltc_test_doc_global_itf DEFINITION DEFERRED.
CLASS ltc_test_doc_local_itf DEFINITION DEFERRED.
CLASS ltc_test_doc_redef DEFINITION DEFERRED.
CLASS ltc_test_doc_setter_inj DEFINITION DEFERRED.
CLASS ltc_test_doc_param_inj DEFINITION DEFERRED.

CLASS zcl_demo_abap_unit_test DEFINITION LOCAL FRIENDS ltc_test_simple_1
                                                      ltc_test_simple_2
                                                      ltc_test_doc_seam
                                                      ltc_test_doc_global_itf
                                                      ltc_test_doc_local_itf
                                                      ltc_test_doc_redef
                                                      ltc_test_doc_setter_inj
                                                      ltc_test_doc_param_inj.

***********************************************************************
* Test class ltc_test_simple_1
*
* - Simple example testing only one method; no special methods are declared
* - The tested method does not have a dependent-on component (DOC). It has one
*   importing and a returning parameter.
* - The implementation of the test method provides some values against which
*   the production method is tested a few times (using a loop).
*   Expected values are also provided, on the basis of which the assertion is
*   performed.
* - There are values included for which the assertion deliberately fails to
*   demonstrate the errors in the ABAP Unit tab.
***********************************************************************
CLASS ltc_test_simple_1 DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.

  METHODS: test_get_digit_sum FOR TESTING.

ENDCLASS.

CLASS ltc_test_simple_1 IMPLEMENTATION.

METHOD test_get_digit_sum.
  "The method to be tested calculates the digit sum of an number.

  "Creating an object of class under test
  DATA(ref_cut) = NEW zcl_demo_abap_unit_test( ).

  "Filling an internal table and providing some values against which the production
  "method is tested a few times. There are values included for which the assertion
  "deliberately fails.
  "num1 = actual parameter for the method call
  "num2 = expected result of the method call
  DATA(tab_digit_sum) = VALUE zcl_demo_abap_unit_test=>nums_tab( ( num1 = 124 num2 = 7 )
                                                                  ( num1 = 57 num2 = 12 )
                                                                  ( num1 = 11111 num2 = 5 )
                                                                  ( num1 = 1000 num2 = 1 )
                                                                  ( num1 = 0 num2 = 0 )

                                                                  "To fail:
                                                                  ( num1 = 124 num2 = 8 ) "correct: 7
                                                                  ( num1 = 57 num2 = 13 ) "correct: 12
                                                                  ( num1 = 11111 num2 = 6 ) ). "correct: 5

  "Looping across the internal table and test the method with the values provided.
  LOOP AT tab_digit_sum ASSIGNING FIELD-SYMBOL(<a>).

    "Calling method that is to be tested
    DATA(digit_sum) = ref_cut->get_digit_sum( <a>-num1 ).

    "Assertion
    cl_abap_unit_assert=>assert_equals(
          act = digit_sum
          exp = <a>-num2
          msg = |The digit sum of { <a>-num1 } is not { <a>-num2 }. It is { digit_sum }.|
          quit = if_abap_unit_constant=>quit-no ).

  ENDLOOP.

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_simple_2
*
* - Test class used to test simple methods that do not have DOCs in the
*   production code.
* - Similar to the test class above, the implementation of the test method
*   provides some values against which the production method is tested a
*   few times (in some cases using a loop). Expected values are also provided,
*   on the basis of which the assertion is executed. There are values
*   included for which the assertion intentionally fails to display the errors
*   in the ABAP Unit tab.
* - The test class contains the declaration and implementation of the
*   special methods setup and teardown. The setup method involves creating
*   an object of the class under test. This object is used by all test methods.
* - The get_sum method in the production code is tested here. The original
*   implementation of the method includes a SELECT statement to retrieve
*   a single record. The statement sums the values of two fields, both are of
*   type i. For proper testability, the database table is prepared in the setup
*   method (an appropriate record is added to the table).
* - In the teardown method, the newly added record is deleted again.
* - Because of this change in persistent data, the test class should have the
*   RISK LEVEL DANGEROUS property. However, in this example, it is set to
*   HARMLESS because, depending on your system settings, you may not be allowed
*   to run unit tests with RISK LEVEL DANGEROUS.
***********************************************************************
CLASS ltc_test_simple_2 DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.

  TYPES: BEGIN OF nums,
            num1       TYPE i,
            num2       TYPE i,
            common_div TYPE zcl_demo_abap_unit_test=>int_tab_so,
            gcd        TYPE i,
          END OF nums,
          tab_type TYPE TABLE OF nums WITH EMPTY KEY.

  DATA: ref_cut TYPE REF TO zcl_demo_abap_unit_test.

  METHODS: test_get_common_div_gcd_ok FOR TESTING,
            test_get_common_div_gcd_fail FOR TESTING,

            test_get_digit_sum_ok FOR TESTING,
            test_get_digit_sum_fail FOR TESTING,

          test_get_sum_ok FOR TESTING,
          test_get_sum_fail FOR TESTING,

          setup,
          teardown.

ENDCLASS.


CLASS ltc_test_simple_2 IMPLEMENTATION.

METHOD setup.

  "Creating an object of the class under test
  ref_cut = NEW #( ).

  "Preparing demo database table for testing the get_sum method
  DELETE FROM zdemo_abap_tab1
    WHERE key_field = 987654321
    AND char1 = '@'.

  MODIFY zdemo_abap_tab1 FROM @(
    VALUE #( key_field = 987654321
              char1     = '@'
              num1      = 444
              num2      = 555 ) ).

ENDMETHOD.

METHOD teardown.
  "Removing test data inserted into the demo database table for testing the get_sum method
  DELETE FROM zdemo_abap_tab1
    WHERE key_field = 987654321
    AND char1 = '@'.
ENDMETHOD.

METHOD test_get_common_div_gcd_ok.
  "The method to be tested calculates the common divisors of two numbers
  "and the greatest common divisor.

  "Filling an internal table and providing some values against which the production
  "method is tested a few times (using a loop).
  "num1/num2 = numbers on which the calculation is based
  "common_div = table containing the expected common divisors
  "gcd = expected greatest common divisor
  DATA(tab) = VALUE tab_type(
    ( num1 = 6 num2 = 3 common_div = VALUE #( ( 1 ) ( 3 ) ) gcd = 3 )
    ( num1 = 24 num2 = 36 common_div = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 6 ) ( 12 ) ) gcd = 12 )
    ( num1 = 5 num2 = 7 common_div = VALUE #( ( 1 ) ) gcd = 1 )
    ( num1 = 21 num2 = 35 common_div = VALUE #( ( 1 ) ( 7 ) ) gcd = 7 ) ).

  "Looping across the internal table and test the method with the values provided.
  LOOP AT tab ASSIGNING FIELD-SYMBOL(<divs>).

    "Calling method that is to be tested
    ref_cut->get_common_div_and_gcd( EXPORTING a = <divs>-num1
                                                b = <divs>-num2
                                      IMPORTING common_divisors = DATA(a)
                                                gcd             = DATA(b) ).

    "Assertion
    cl_abap_unit_assert=>assert_equals(
        act = VALUE nums( common_div = a
                          gcd        = b )
        exp = VALUE nums( common_div = <divs>-common_div
                          gcd        = <divs>-gcd )
        msg = |Wrong result for { <divs>-num1 } and { <divs>-num2 }.|
        quit = if_abap_unit_constant=>quit-no ).

  ENDLOOP.

ENDMETHOD.

METHOD test_get_common_div_gcd_fail.
  "This method intentionally includes values to make the unit test fail.

  DATA(num1) = 6.
  DATA(num2) = 3.
  DATA(c_div) = VALUE zcl_demo_abap_unit_test=>int_tab_st( ( 1 ) ( 2 ) ). "correct: 1, 3
  DATA(g_c_d) = 4. "correct: 3

  "Calling method that is to be tested
  ref_cut->get_common_div_and_gcd( EXPORTING a = num1
                                              b = num2
                                    IMPORTING common_divisors = DATA(cd)
                                              gcd             = DATA(gr) ).

  "Assertion
  cl_abap_unit_assert=>assert_equals(
      act = VALUE nums( common_div = cd
                        gcd        = gr )
      exp = VALUE nums( common_div = c_div
                        gcd        = g_c_d )
      msg = |Wrong result for { num1 } and { num2 }.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

METHOD test_get_digit_sum_ok.
  "The method to be tested calculates the digit sum of a number.


  "Filling an internal table and providing some values against which the production
  "method is tested a few times (using a loop).
  "num1: Number on which the digit sum is calculated
  "num2: Expected result
  DATA(tab_digit_sum) = VALUE zcl_demo_abap_unit_test=>nums_tab( ( num1 = 124 num2 = 7 )
                                                                  ( num1 = 57 num2 = 12 )
                                                                  ( num1 = 11111 num2 = 5 )
                                                                  ( num1 = 1000 num2 = 1 )
                                                                  ( num1 = 0 num2 = 0 ) ).

  "Looping across the internal table and test the method with the values provided.
  LOOP AT tab_digit_sum ASSIGNING FIELD-SYMBOL(<b>).

    "Calling method that is to be tested
    DATA(digit_sum) = ref_cut->get_digit_sum( <b>-num1 ).

    "Assertion
    cl_abap_unit_assert=>assert_equals(
          act = digit_sum
          exp = <b>-num2
          msg = |The digit sum of { <b>-num1 } is not { digit_sum }.|
          quit = if_abap_unit_constant=>quit-no ).

  ENDLOOP.

ENDMETHOD.

METHOD test_get_digit_sum_fail.
  "This method intentionally includes values to make the unit test fail.

  DATA(num) = 123.
  DATA(dsum) = 7. "correct: 6

  "Calling method that is to be tested
  DATA(digit_sum) = ref_cut->get_digit_sum( num ).

  "Assertion
  cl_abap_unit_assert=>assert_equals(
        act = digit_sum
        exp = dsum
        msg = |The digit sum of { num } is not { dsum }.|
        quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

METHOD test_get_sum_ok.
  "The method to be tested calculates the sum of two numbers.
  "Note: The setup method has prepared data in the database table.
  "See the comments above.

  DATA(exp_sum) = 999.

  "Calling method that is to be tested
  DATA(act_sum) = ref_cut->get_sum( key = 987654321 char = '@' ).

  "Assertion
  cl_abap_unit_assert=>assert_equals(
      act = act_sum
      exp = exp_sum
      msg = |The expected sum { exp_sum } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

METHOD test_get_sum_fail.
  "This method intentionally includes values to make the unit test fail.

  DATA(exp_sum) = 998. "correct: 999
  DATA(act_sum) = ref_cut->get_sum( key = 987654321 char = '@' ).

  cl_abap_unit_assert=>assert_equals(
      act = act_sum
      exp = exp_sum
      msg = |The expected sum { exp_sum } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_dummy
*
* - This test class does not contain any methods of the production code
*   to be tested.
* - It only demonstrates the use of various static methods of the
*   cl_abap_unit_assert class.
* - Since no (private) components of the production code are used, the
*   test class is not made a local friend of the class under test.
***********************************************************************
CLASS ltc_test_dummy DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.

  METHODS: test_multiple_asserts_ok FOR TESTING,
            test_multiple_asserts_fail FOR TESTING.

ENDCLASS.

CLASS ltc_test_dummy IMPLEMENTATION.

METHOD test_multiple_asserts_ok.
  "This method does not test a method of the class under test.

  DATA(ref) = NEW zcl_demo_abap_unit_test( ).

  DATA(is_true) = cl_abap_unit_assert=>assert_equals(
          act = 1
          exp = 1
          msg = |assert_equals: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

  is_true = cl_abap_unit_assert=>assert_bound(
          act = ref
          msg = |assert_bound: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

  is_true = cl_abap_unit_assert=>assert_differs(
          act = `hallo`
          exp = `hallo!`
          msg = |assert_differs: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

  is_true = cl_abap_unit_assert=>assert_initial(
          act = 0
          msg = |assert_initial: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

  is_true = cl_abap_unit_assert=>assert_not_initial(
          act = 1
          msg = |assert_not_initial: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

  FIND `A` IN `ABAP`.

  is_true = cl_abap_unit_assert=>assert_subrc(
          exp = 0
          act = sy-subrc
          msg = |assert_subrc: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

METHOD test_multiple_asserts_fail.
  "This method intentionally includes values to make the unit test fail.

  DATA ref TYPE REF TO zcl_demo_abap_unit_test.

  DATA(is_true) = cl_abap_unit_assert=>assert_equals(
          exp = 'This a string that is checked.'
          act = 'This a string that is checked ... but it is not the expected string.'
          msg = |assert_equals: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

  is_true = cl_abap_unit_assert=>assert_bound(
          act = ref
          msg = |assert_bound: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

  is_true = cl_abap_unit_assert=>assert_differs(
          act = `hallo`
          exp = `hallo`
          msg = |assert_differs: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

  is_true = cl_abap_unit_assert=>assert_initial(
          act = 1
          msg = |assert_initial: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

  is_true = cl_abap_unit_assert=>assert_not_initial(
          act = 0
          msg = |assert_not_initial: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

  FIND `C` IN `ABAP`.

  is_true = cl_abap_unit_assert=>assert_subrc(
          exp = 0
          act = sy-subrc
          msg = |assert_subrc: Issue.|
          quit = if_abap_unit_constant=>quit-no ).

  IF 1 <> 2.
    cl_abap_unit_assert=>fail(
                msg = |fail: Issue.|
                quit = if_abap_unit_constant=>quit-no ).
  ENDIF.

ENDMETHOD.

ENDCLASS.


***********************************************************************
* Test class ltc_test_seam
*
* - This test class demonstrates the use of test seams and their injection
*   into the production code.
* - In the production code, the method implementation includes a
*   depended-on component (DOC), which in this case is a database access.
*   The DOC is replaced by a test double using a test seam.
* - The method implementation contains further simple examples for
*   test seam injections.
***********************************************************************
CLASS ltc_test_doc_seam DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.

  DATA: ref_cut TYPE REF TO zcl_demo_abap_unit_test,
        carrier_id TYPE zdemo_abap_fli-carrid.

  METHODS: test_get_occ_rate_seam_ok FOR TESTING,
            test_get_occ_rate_seam_fail FOR TESTING,
            setup.

ENDCLASS.


CLASS ltc_test_doc_seam IMPLEMENTATION.

METHOD setup.

  "Creating an object of the class under test
  ref_cut = NEW #( ).

ENDMETHOD.

METHOD test_get_occ_rate_seam_ok.
  "The method to be tested calculates the occupancy rate of flights

  "Creating test data
  carrier_id = 'AB'.
  DATA(expected_occupancy_rate) = '50.00'.

  "Injecting test seam into production code by replacing the code that is
  "contained in the TEST-SEAM ... TEST-SEAM-END block
  TEST-INJECTION select_flights.
    seats_table = VALUE #(
    ( carrid = carrier_id seatsmax = 100 seatsocc = 80 )
    ( carrid = carrier_id seatsmax = 100 seatsocc = 20 )
    ( carrid = carrier_id seatsmax = 200 seatsocc = 100 ) ).
  END-TEST-INJECTION.

  "Further test seam injections
  "The following example demonstrates an empty test seam in the production
  "code. During the unit test run, the following code is injected.
  TEST-INJECTION num1.
    var = 1.
  END-TEST-INJECTION.

  "empty injection

  "The following example demonstrates an empty test seam injection.
  "There is code in a TEST-SEAM ... END-TEST-SEAM block in the
  "production code. This code is replaced, and nothing is included
  "for the test, i.e. during the unit test 'unwanted' code in the production
  "code is ignored.
  TEST-INJECTION num2.
  END-TEST-INJECTION.

  "Calling method that is to be tested
  ref_cut->get_occ_rate_test_seam( EXPORTING carrier_id = carrier_id
                                    IMPORTING occupancy_rate = DATA(occupancy_rate)
                                              num1           = DATA(num1)
                                              num2           = DATA(num2) ).

  "Assertions
  cl_abap_unit_assert=>assert_equals(
      act = occupancy_rate
      exp = expected_occupancy_rate
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

  cl_abap_unit_assert=>assert_equals(
      act = num1
      exp = 999
      msg = |num1: { num1 }|
      quit = if_abap_unit_constant=>quit-no ).

  cl_abap_unit_assert=>assert_equals(
      act = num2
      exp = 0
      msg = |num2: { num2 }|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

METHOD test_get_occ_rate_seam_fail.
  "This method intentionally includes values to make the unit test fail.

  "Creating test data
  carrier_id = 'CD'.
  DATA(expected_occupancy_rate) = '60.00'.

  "Code injection
  TEST-INJECTION select_flights.
    seats_table = VALUE #(
      ( carrid = carrier_id seatsmax = 100 seatsocc = 50 )
      ( carrid = carrier_id seatsmax = 200 seatsocc = 100 )
      ( carrid = carrier_id seatsmax = 300 seatsocc = 100 ) ).
  END-TEST-INJECTION.

  TEST-INJECTION num1.
    var = 1.
  END-TEST-INJECTION.

  TEST-INJECTION num2.
  END-TEST-INJECTION.

  "Calling method that is to be tested
  ref_cut->get_occ_rate_test_seam( EXPORTING carrier_id = carrier_id
                                    IMPORTING occupancy_rate = DATA(occupancy_rate)
                                              num1           = DATA(num1)
                                              num2           = DATA(num2) ).

  "Assertions
  cl_abap_unit_assert=>assert_equals(
      act = occupancy_rate
      exp = expected_occupancy_rate
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

  cl_abap_unit_assert=>assert_equals(
      act = num1
      exp = 1
      msg = |num1: { num1 }|
      quit = if_abap_unit_constant=>quit-no ).

  cl_abap_unit_assert=>assert_equals(
      act = num2
      exp = 123
      msg = |num2: { num2 }|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_doc_global_itf
*
* - Tests one method of the global class; demonstrates constructor injection
* - In this case, a dependent-on component (DOC) has been identified (a
*   database access).
* - It is assumed that there is a global interface to overcome the DOC.
* - A local test double class is created. It implements the global
*   interface. The method implementation contains manually created test
*   data.
*
* Notes on constructor injection in this example:
* - This means that a test double is passed as a parameter to the instance
*   constructor of the class under test.
* - An interface reference variable is declared in the private section of
*   the class under test, and its type references the global interface.
* - A local test class is created here for the test double. It implements
*   the interface method required by the test. Note the PARTIALLY IMPLEMENTED
*   addition to the interface.
* - In this method implementation, local test data are created.
* - The global class/class under test has the following instance constructor
*   declaration:
*   - Has an optional importing parameter for the DOC.
*   - The parameter is typed with reference to the test double, i.e. an
*     object of the test double is passed.
* - Instance constructor implementation: When the unit test is executed, an
*   object of the test double is created in the test method. This object is
*   then passed to the constructor. A check is implemented to determine if
*   the reference variable is bound. It is indeed bound during the unit test,
*   and the test double is injected.
***********************************************************************

***********************************************************************
* Local test double class
***********************************************************************
CLASS ltd_test_data_global_intf DEFINITION FOR TESTING.
PUBLIC SECTION.

  "Note: Usually, you must implement all non-optional methods of interfaces.
  "Without the addition PARTIALLY IMPLEMENTED, there would be a syntax error.
  INTERFACES zdemo_abap_get_data_itf PARTIALLY IMPLEMENTED.

ENDCLASS.

CLASS ltd_test_data_global_intf IMPLEMENTATION.
METHOD zdemo_abap_get_data_itf~select_flight_data.

  CLEAR flight_data.

  "Providing test data
  flight_data = SWITCH #( carrier
    WHEN 'EF' THEN VALUE #( ( carrid = carrier  seatsmax = 100 seatsocc = 50 )
                            ( carrid = carrier  seatsmax = 200 seatsocc = 150 )
                            ( carrid = carrier  seatsmax = 300 seatsocc = 100 ) )
    WHEN 'GH' THEN VALUE #( ( carrid = carrier  seatsmax = 350 seatsocc = 200 )
                            ( carrid = carrier  seatsmax = 350 seatsocc = 250 )
                            ( carrid = carrier  seatsmax = 300 seatsocc = 150 ) ) ).

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_doc_global_itf
***********************************************************************
CLASS ltc_test_doc_global_itf DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.
  DATA: ref_cut TYPE REF TO zcl_demo_abap_unit_test,
        ref_data_prov TYPE REF TO zdemo_abap_get_data_itf,
        carrier_id TYPE zdemo_abap_fli-carrid.

  METHODS: setup,
            test_get_occ_rate_glo_if_ok FOR TESTING,
            test_get_occ_rate_glo_if_fail FOR TESTING.

ENDCLASS.

CLASS ltc_test_doc_global_itf IMPLEMENTATION.

METHOD setup.

  "Creating an instance of the local test double
  ref_data_prov = NEW ltd_test_data_global_intf( ).

  "Instance is provided for the constructor injection
  ref_cut = NEW #( ref_data_prov ).

ENDMETHOD.

METHOD test_get_occ_rate_glo_if_ok.
  "The method to be tested calculates the occupancy rate of flights.

  "(1) Calling method that is to be tested
  "Due to constructor injection, the test double is used.
  carrier_id = 'EF'.
  DATA(act_occ_rate) = ref_cut->get_occ_rate_global_itf( carrier_id ).

  "Assertion
  DATA(exp_value) = '50.00'.

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

  "(2) Calling method that is to be tested
  carrier_id = 'GH'.
  act_occ_rate = ref_cut->get_occ_rate_global_itf( carrier_id ).

  "Assertion
  exp_value = '60.00'.

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

METHOD test_get_occ_rate_glo_if_fail.
  "This method intentionally includes values to make the unit test fail.

  "(1) Calling method that is to be tested
  carrier_id = 'EF'.
  DATA(act_occ_rate) = ref_cut->get_occ_rate_global_itf( carrier_id ).

  "Assertion to fail
  DATA(exp_value) = '40.00'. "correct: 50.00

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

  "(2) Calling method that is to be tested
  carrier_id = 'GH'.
  act_occ_rate = ref_cut->get_occ_rate_global_itf( carrier_id ).

  "Assertion to fail
  exp_value = '90.00'. "correct: 60.00

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_local_itf
*
* - Tests one method of the global class; demonstrates back door injection
* - In this case, a dependent-on component (DOC) has been identified (a
*   database access). The method is similar to the one above.
* - It is assumed that there is no global interface to overcome the DOC.
*   Instead, a local interface is created in the local types
*   (CCIMP include): lif_get_data
* - Additionally, a local class is implemented in the CCIMP include that
*   implements the local interface. This local class provides the data
*   for the method call in the global class (get_occ_rate_local_itf).
* - A local test double class is created here. It implements the local
*   interface. The method implementation contains manually created test
*   data.
* - In addition, the test class includes a helper method to demonstrate
*   the separation of code recurring tasks into separate methods.
*
* Notes on back door injection in this example:
* - This means that a back door is created to inject a test double into
*   the class under test.
* - This back door is implemented by granting friendship to the test
*   class.
*   This makes internal attributes of the class under test accessible
*   to the test class.
* - In the production code, a reference variable with type reference to
*   the data provider is declared in the private section.
* - When the unit test is executed, the private attribute of the class
*   under test is changed.
* - An object of the test double is injected.
***********************************************************************

***********************************************************************
* Local test double class
***********************************************************************
CLASS ltd_test_data_local_itf DEFINITION FOR TESTING.
PUBLIC SECTION.
  INTERFACES lif_get_data PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd_test_data_local_itf IMPLEMENTATION.
METHOD lif_get_data~select_flight_data.

  CLEAR flight_data.

  "Providing test data
  flight_data = SWITCH #( carrier
    WHEN 'IJ' THEN VALUE #( ( carrid = carrier  seatsmax = 300 seatsocc = 200 )
                            ( carrid = carrier  seatsmax = 350 seatsocc = 200 )
                            ( carrid = carrier  seatsmax = 350 seatsocc = 300 ) )
    WHEN 'KL' THEN VALUE #( ( carrid = carrier  seatsmax = 350 seatsocc = 300 )
                            ( carrid = carrier  seatsmax = 350 seatsocc = 250 )
                            ( carrid = carrier  seatsmax = 300 seatsocc = 250 ) ) ).

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_doc_local_itf
***********************************************************************
CLASS ltc_test_doc_local_itf DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.

  DATA: ref_cut TYPE REF TO zcl_demo_abap_unit_test,
        ref_data_prov TYPE REF TO lif_get_data.

  METHODS: setup,

          "Helper method
          assert_occ_rate IMPORTING carrier_id TYPE zdemo_abap_fli-carrid
                                    occ_rate   TYPE lif_get_data=>occ_rate,

          "Test methods
          test_get_occ_rate_lo_itf_ok FOR TESTING,
          test_get_occ_rate_lo_itf_fail FOR TESTING.

ENDCLASS.


CLASS ltc_test_doc_local_itf IMPLEMENTATION.

METHOD setup.

  ref_cut = NEW #( ).

  ref_data_prov = NEW ltd_test_data_local_itf( ).

  "Back door injection
  ref_cut->data_provider_local_itf = ref_data_prov.

ENDMETHOD.

METHOD assert_occ_rate.

  DATA(act_occ_rate) = ref_cut->get_occ_rate_local_itf( carrier_id ).

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = occ_rate
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.


METHOD test_get_occ_rate_lo_itf_ok.

  assert_occ_rate( carrier_id = 'IJ'
                    occ_rate = '70.00' ).

  assert_occ_rate( carrier_id = 'KL'
                    occ_rate = '80.00' ).

ENDMETHOD.

METHOD test_get_occ_rate_lo_itf_fail.
  "This method intentionally includes values to make the unit test fail.

  assert_occ_rate( carrier_id = 'IJ'
                    occ_rate = '20.00' ). "correct: 70.00

  assert_occ_rate( carrier_id = 'KL'
                    occ_rate = '30.00' ). "correct: 80.00

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_redef
*
* - Tests one method of the global class; demonstrates constructor injection
* - In this case, a dependent-on component (DOC) has been identified (a
*   database access).
* - It is assumed that there are no global and local interfaces to overcome
*   the DOC.
* - A local test double class is created by redefining a method of
*   the class under test. The method implementation contains manually
*   created test data.
*
* Notes:
* - A local test double is created in a separate test class.
* - The test double is created by redefining a method of a class.
* - In this simple and self-contained example, the global class is
*   deliberately not declared as a final class, so that inheritance
*   from it is allowed. The global class implements a method that
*   particularly serves the purpose of the test. It is there only
*   to have a self-contained example and to keep the number
*   of separate artifacts low. You might imagine that the method is
*   contained in another global class, and the global class under test
*   uses that method.
**********************************************************************

***********************************************************************
* Local test double class
***********************************************************************
CLASS ltd_test_data_redef DEFINITION FOR TESTING
INHERITING FROM zcl_demo_abap_unit_test.

PROTECTED SECTION.
  METHODS select_flight_data REDEFINITION.

ENDCLASS.

CLASS ltd_test_data_redef IMPLEMENTATION.
METHOD select_flight_data.

  CLEAR flight_data.

  "Providing test data
  flight_data = SWITCH #( carrier
    WHEN 'MN' THEN VALUE #( ( carrid = carrier  seatsmax = 100 seatsocc = 50 )
                            ( carrid = carrier  seatsmax = 200 seatsocc = 150 )
                            ( carrid = carrier  seatsmax = 300 seatsocc = 100 ) )
    WHEN 'OP' THEN VALUE #( ( carrid = carrier  seatsmax = 350 seatsocc = 200 )
                            ( carrid = carrier  seatsmax = 350 seatsocc = 250 )
                            ( carrid = carrier  seatsmax = 300 seatsocc = 150 ) ) ).

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_doc_redef
***********************************************************************
CLASS ltc_test_doc_redef DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.
  DATA: ref_cut TYPE REF TO zcl_demo_abap_unit_test,
        carrier_id TYPE zdemo_abap_fli-carrid.

  METHODS: setup,
            test_get_occ_rate_redef_ok FOR TESTING,
            test_get_occ_rate_redef_fail FOR TESTING.

ENDCLASS.

CLASS ltc_test_doc_redef IMPLEMENTATION.

METHOD setup.

  ref_cut = NEW ltd_test_data_redef( ).

ENDMETHOD.

METHOD test_get_occ_rate_redef_ok.

  carrier_id = 'MN'.
  DATA(act_occ_rate) = ref_cut->get_occ_rate_using_meth( carrier_id ).

  DATA(exp_value) = '50.00'.

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

  carrier_id = 'OP'.
  act_occ_rate = ref_cut->get_occ_rate_using_meth( carrier_id ).

  exp_value = '60.00'.

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

METHOD test_get_occ_rate_redef_fail.

  carrier_id = 'MN'.
  DATA(act_occ_rate) = ref_cut->get_occ_rate_using_meth( carrier_id ).

  DATA(exp_value) = '40.00'. "correct: 50.00

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

  carrier_id = 'OP'.
  act_occ_rate = ref_cut->get_occ_rate_using_meth( carrier_id ).

  exp_value = '90.00'. "correct: 60.00

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_doc_setter_inj
*
* - Tests one method of the global class; demonstrates setter injection
* - In this case, a dependent-on component (DOC) has been identified (a
*   database access).
* - It is assumed that there is a global interface to overcome the DOC.
* - A local test double class is created. It implements the global
*   interface. The method implementation contains manually created test
*   data.
*
* Notes on setter injection in this example:
* - This means that an object of the test double class is passed as a
*   parameter to a setter method in the class under test.
* - An interface reference variable is declared in the private section of
*   the class under test, and its type references the global interface.
* - A local test class is created here for the test double. It implements
*   the interface method required by the test. Note the PARTIALLY IMPLEMENTED
*   addition to the interface.
* - In this method implementation, local test data are created.
* - The global class/class under test contains a setter method (setter_meth)
*   that has an importing parameter of type reference to the global interface.
* - When the unit test is executed (and only then), the setter method is
*   called in the test class. Before the call is made, an object of the
*   test double class is created. This object is passed to the setter method.
* - In the implementation of the setter method, the interface variable declared in
*   the private section of the class under test is assigned the object of
*   the test double class. When the method is 'usually' called
*   (i.e., not in the context of a unit test), for example, when the
*   class is run using F9, the instance constructor implementation involves
*   creating an object of the actual data provider that is assigned to
*   the interface variable. Therefore, when the unit test is executed, the
*   assigned object is replaced by the setter method and the test double
*   is used instead.
***********************************************************************

***********************************************************************
* Local test double class
***********************************************************************
CLASS ltd_test_data_setter_inj DEFINITION FOR TESTING.
PUBLIC SECTION.

  "Note: Usually, you must implement all non-optional methods of interfaces.
  "Without the addition PARTIALLY IMPLEMENTED, there would be a syntax error.
  INTERFACES zdemo_abap_get_data_itf PARTIALLY IMPLEMENTED.

ENDCLASS.

CLASS ltd_test_data_setter_inj IMPLEMENTATION.
METHOD zdemo_abap_get_data_itf~select_flight_data.

  CLEAR flight_data.

  "Providing test data
  flight_data = SWITCH #( carrier
    WHEN 'QR' THEN VALUE #( ( carrid = carrier  seatsmax = 100 seatsocc = 50 )
                            ( carrid = carrier  seatsmax = 200 seatsocc = 150 )
                            ( carrid = carrier  seatsmax = 300 seatsocc = 100 ) )
    WHEN 'ST' THEN VALUE #( ( carrid = carrier  seatsmax = 350 seatsocc = 200 )
                            ( carrid = carrier  seatsmax = 350 seatsocc = 250 )
                            ( carrid = carrier  seatsmax = 300 seatsocc = 150 ) ) ).

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_doc_setter_inj
***********************************************************************
CLASS ltc_test_doc_setter_inj DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.
  DATA: ref_cut TYPE REF TO zcl_demo_abap_unit_test,
        ref_data_prov TYPE REF TO zdemo_abap_get_data_itf,
        carrier_id TYPE zdemo_abap_fli-carrid.

  METHODS: setup,
            test_get_occ_rate_set_inj_ok FOR TESTING,
            test_get_occ_rate_set_inj_fail FOR TESTING.

ENDCLASS.

CLASS ltc_test_doc_setter_inj IMPLEMENTATION.

METHOD setup.

  ref_cut = NEW #( ).

  "Creating an instance of the local test double
  ref_data_prov = NEW ltd_test_data_setter_inj( ).

  "Setter injection
  "Passing the test double as a parameter of a setter method
  ref_cut->setter_meth( ref_data_prov ).

ENDMETHOD.

METHOD test_get_occ_rate_set_inj_ok.
  "The method to be tested calculates the occupancy rate of flights.

  "(1) Calling method that is to be tested
  "Due to constructor injection, the test double is used.
  carrier_id = 'QR'.
  DATA(act_occ_rate) = ref_cut->get_occ_rate_setter_inj( carrier_id ).

  "Assertion
  DATA(exp_value) = '50.00'.

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

  "(2) Calling method that is to be tested
  carrier_id = 'ST'.
  act_occ_rate = ref_cut->get_occ_rate_setter_inj( carrier_id ).

  "Assertion
  exp_value = '60.00'.

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

METHOD test_get_occ_rate_set_inj_fail.
  "This method intentionally includes values to make the unit test fail.

  "(1) Calling method that is to be tested
  carrier_id = 'QR'.
  DATA(act_occ_rate) = ref_cut->get_occ_rate_setter_inj( carrier_id ).

  "Assertion to fail
  DATA(exp_value) = '40.00'. "correct: 50.00

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

  "(2) Calling method that is to be tested
  carrier_id = 'ST'.
  act_occ_rate = ref_cut->get_occ_rate_setter_inj( carrier_id ).

  "Assertion to fail
  exp_value = '90.00'. "correct: 60.00

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_doc_param_inj
*
* - Tests one method of the global class; demonstrates parameter injection
* - In this case, a dependent-on component (DOC) has been identified (a
*   database access).
* - It is assumed that there is a global interface to overcome the DOC.
* - A local test double class is created. It implements the global
*   interface. The method implementation contains manually created test
*   data.
*
* Notes on parameter injection in this example:
* - This means that an object of the test double class is passed as a
*   parameter of the tested method in the class under test.
* - This parameter is optional. When the unit test is run, the parameter
*   is bound. An object of the test double class is passed in this case.
*   Otherwise, when the method is 'usually' called (i.e., not in the
*   context of a unit test), for example, when the class is run using F9,
*   the parameter is not bound. Then, an object of the actual data
*   provider is created. The method in the class under test contains a
*   check in the implementation (IF ... IS BOUND ...).
***********************************************************************

***********************************************************************
* Local test double class
***********************************************************************
CLASS ltd_test_data_param_inj DEFINITION FOR TESTING.
PUBLIC SECTION.

  "Note: Usually, you must implement all non-optional methods of interfaces.
  "Without the addition PARTIALLY IMPLEMENTED, there would be a syntax error.
  INTERFACES zdemo_abap_get_data_itf PARTIALLY IMPLEMENTED.

ENDCLASS.

CLASS ltd_test_data_param_inj IMPLEMENTATION.
METHOD zdemo_abap_get_data_itf~select_flight_data.

  CLEAR flight_data.

  "Providing test data
  flight_data = SWITCH #( carrier
    WHEN 'UV' THEN VALUE #( ( carrid = carrier  seatsmax = 100 seatsocc = 50 )
                            ( carrid = carrier  seatsmax = 200 seatsocc = 150 )
                            ( carrid = carrier  seatsmax = 300 seatsocc = 100 ) )
    WHEN 'WX' THEN VALUE #( ( carrid = carrier  seatsmax = 350 seatsocc = 200 )
                            ( carrid = carrier  seatsmax = 350 seatsocc = 250 )
                            ( carrid = carrier  seatsmax = 300 seatsocc = 150 ) ) ).

ENDMETHOD.

ENDCLASS.

***********************************************************************
* Test class ltc_test_doc_param_inj
***********************************************************************
CLASS ltc_test_doc_param_inj DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.
  DATA: ref_cut TYPE REF TO zcl_demo_abap_unit_test,
        ref_data_prov TYPE REF TO zdemo_abap_get_data_itf,
        carrier_id TYPE zdemo_abap_fli-carrid.

  METHODS: setup,
            test_get_occ_rate_par_inj_ok FOR TESTING,
            test_get_occ_rate_par_inj_fail FOR TESTING.

ENDCLASS.

CLASS ltc_test_doc_param_inj IMPLEMENTATION.

METHOD setup.

  ref_cut = NEW #( ).

  "Creating an instance of the local test double
  ref_data_prov = NEW ltd_test_data_param_inj( ).

ENDMETHOD.

METHOD test_get_occ_rate_par_inj_ok.
  "The method to be tested calculates the occupancy rate of flights.

  "(1) Calling method that is to be tested
  "Due to constructor injection, the test double is used.
  carrier_id = 'UV'.
  DATA(act_occ_rate) = ref_cut->get_occ_rate_param_inj( carrier_id = carrier_id
                                                        data_prov  = ref_data_prov ).

  "Assertion
  DATA(exp_value) = '50.00'.

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

  "(2) Calling method that is to be tested
  carrier_id = 'WX'.
  act_occ_rate = ref_cut->get_occ_rate_param_inj( carrier_id = carrier_id
                                                  data_prov  = ref_data_prov ).

  "Assertion
  exp_value = '60.00'.

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

METHOD test_get_occ_rate_par_inj_fail.
  "This method intentionally includes values to make the unit test fail.

  "(1) Calling method that is to be tested
  carrier_id = 'UV'.
  DATA(act_occ_rate) = ref_cut->get_occ_rate_param_inj( carrier_id = carrier_id
                                                        data_prov  = ref_data_prov ).

  "Assertion to fail
  DATA(exp_value) = '40.00'. "correct: 50.00

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

  "(2) Calling method that is to be tested
  carrier_id = 'WX'.
  act_occ_rate = ref_cut->get_occ_rate_param_inj( carrier_id = carrier_id
                                                  data_prov  = ref_data_prov ).

  "Assertion to fail
  exp_value = '90.00'. "correct: 60.00

  cl_abap_unit_assert=>assert_equals(
      act = act_occ_rate
      exp = exp_value
      msg = |The expected occupancy rate for carrier { carrier_id } is wrong.|
      quit = if_abap_unit_constant=>quit-no ).

ENDMETHOD.

ENDCLASS.
