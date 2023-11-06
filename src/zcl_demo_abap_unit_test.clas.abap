***********************************************************************
*
*       ABAP Cheat Sheet: ABAP Unit Tests
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate ABAP unit tests.
* - Topics covered: Test classes and test methods, special methods,
*   implementing and injecting test doubles (constructor injection,
*   back door injection, test seams)
*
* ----------------------- RUN ABAP UNIT TEST---------------------------
* - Open the class with the ABAP development tools for Eclipse (ADT).
* - Choose Ctrl/Cmd + Shift + F10 to launch all tests in the class.
*   You can also right-click somewhere in the class and choose
*   Run as -> ABAP Unit Test.
* - The results of a test run are displayed in the ABAP Unit tab in ADT
*   and can be evaluated. The Failure Trace section provides information
*   on errors found.
* - If you are interested in test coverage, you can choose
*   Ctrl/Cmd + Shift + F11, or make a right-click, choose Run as ->
*   ABAP Unit Test With..., select the Coverage checkbox and choose
*   Execute. You can then check the results in the ABAP Coverage tab,
*   what code is tested and what not.
*
* ----------------------- RUN CLASS -----------------------------
* - Open the class with the ABAP development tools for Eclipse (ADT).
* - Choose F9 to run the class.
* - Check the console output.
* - To understand the context and the ABAP syntax used, check the notes
*   included in the class as comments or refer to the respective topic
*   in the ABAP Keyword Documentation.
* - Due to the amount of output in the console, the examples include
*   numbers (e. g. 1) ..., 2) ..., 3) ...) for the individual example
*   sections. Plus, the variable name is displayed in most cases. Hence,
*   to easier and faster find the relevant output in the console, just
*   search in the console for the number/variable name (CTRL + F in the
*   console) or use the debugger.
*
* ----------------------------- NOTE -----------------------------------
* The code presented in this class is intended only to support the ABAP
* cheat sheets. It is not intended for direct use in a production system
* environment. The code examples in the ABAP cheat sheets are primarily
* intended to provide a better explanation and visualization of the
* syntax and semantics of ABAP statements, not to solve concrete
* programming tasks. For production application programs, you should
* always work out your own solution for each individual case. There is
* no guarantee for the correctness or completeness of the code.
* Furthermore, there is no legal responsibility or liability for any
* errors or their consequences that may occur when using the the example
* code.
*
***********************************************************************
"! <p class="shorttext synchronized">ABAP cheat sheet: Unit tests</p>
"! Example to demonstrate ABAP unit tests.<br>Choose F9 in ADT to run the class.
"! To run all unit tests of the class, choose Ctrl/Cmd + Shift + F10.
CLASS zcl_demo_abap_unit_test DEFINITION
PUBLIC
CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES: if_oo_adt_classrun.

    CLASS-METHODS: class_constructor.

    "Optional parameter for the instance constructor for the purpose of
    "constructor injection
    METHODS constructor
      IMPORTING iref_data_prov TYPE REF TO zdemo_abap_get_data_itf OPTIONAL.

  PROTECTED SECTION.

    TYPES carr_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

    METHODS: select_flight_data IMPORTING carrier            TYPE zdemo_abap_fli-carrid
                                RETURNING VALUE(flight_data) TYPE carr_tab.

  PRIVATE SECTION.

    TYPES: int_tab_so TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line,
           int_tab_st TYPE STANDARD TABLE OF i WITH EMPTY KEY,
           occ_rate   TYPE p LENGTH 4 DECIMALS 2,

           BEGIN OF nums_struc,
             num1 TYPE i,
             num2 TYPE i,
           END OF nums_struc,

           nums_tab TYPE TABLE OF nums_struc WITH EMPTY KEY.

    DATA: seats_table              TYPE zdemo_abap_get_data_itf=>carr_tab,
          flight_tab               TYPE TABLE OF zdemo_abap_fli,

          "Reference variable for back door injection
          "Note: In the example, it is a local interface declared in the
          "Local Types tab (the CCIMP include). To make the type known to
          "the global class, see the Class-relevant Local Types tab (the CCDEF include).
          data_provider_local_itf  TYPE REF TO lif_get_data,

          "Reference variable for constructor injection
          "In the example, the type refers to a global interface.
          data_provider_global_itf TYPE REF TO zdemo_abap_get_data_itf,

          "For demonstrating setter injection
          data_provider_setter_inj TYPE REF TO zdemo_abap_get_data_itf,

          "For demonstrating parameter injection
          data_provider_param_inj  TYPE REF TO zdemo_abap_get_data_itf..

    METHODS:
      "Calculates the sum of two numbers
      "This method demonstrates the use of the setup and teardown methods in the test class.
      get_sum IMPORTING key        TYPE zdemo_abap_tab1-key_field
                        char       TYPE zdemo_abap_tab1-char1
              RETURNING VALUE(sum) TYPE i,

      "Calculates the common divisors and the greatest common divisor of two numbers
      get_common_div_and_gcd IMPORTING a               TYPE i
                                       b               TYPE i
                             EXPORTING common_divisors TYPE int_tab_so
                                       gcd             TYPE i,

      "Calculates the digit sum of a number
      get_digit_sum IMPORTING num              TYPE i
                    RETURNING VALUE(digit_sum) TYPE i,

      "Multiple methods that all do the same (they calculate the occupancy rate of flights)
      "but serve different demonstration purposes for the ABAP unit tests in the example.
      "The method implementations involve a depended-on component (DOC). In this case,
      "it is a database access.
      "The methods are intentionally implemented in a similar way. Therefore, almost
      "all of the following methods will display the same output in the console when the
      "class is executed using F9.

      "Method to demonstrate test double injection using inheritance and method redefinition
      get_occ_rate_using_meth IMPORTING carrier_id            TYPE zdemo_abap_fli-carrid
                              RETURNING VALUE(occupancy_rate) TYPE occ_rate,

      "Method to demonstrate test double injection using test seams
      get_occ_rate_test_seam IMPORTING carrier_id     TYPE zdemo_abap_fli-carrid
                             EXPORTING occupancy_rate TYPE occ_rate
                                       num1           TYPE i
                                       num2           TYPE i,

      "Method to demonstrate test double injection using back door injection and a local interface
      get_occ_rate_local_itf IMPORTING carrier_id            TYPE zdemo_abap_fli-carrid
                             RETURNING VALUE(occupancy_rate) TYPE occ_rate,

      "Method to demonstrate test double injection using constructor injection and a global interface
      get_occ_rate_global_itf IMPORTING carrier_id            TYPE zdemo_abap_fli-carrid
                              RETURNING VALUE(occupancy_rate) TYPE occ_rate,

      "Method to demonstrate test double injection using setter injection and a global interface
      get_occ_rate_setter_inj IMPORTING carrier_id            TYPE zdemo_abap_fli-carrid
                              RETURNING VALUE(occupancy_rate) TYPE occ_rate,

      "Method for setter injection
      setter_meth IMPORTING data_prov TYPE REF TO zdemo_abap_get_data_itf,

      "Method to demonstrate test double injection using parameter injection and a global interface
      "An optional parameter is specified for passing the test double if the method is tested.
      get_occ_rate_param_inj IMPORTING carrier_id            TYPE zdemo_abap_fli-carrid
                                       data_prov             TYPE REF TO zdemo_abap_get_data_itf OPTIONAL
                             RETURNING VALUE(occupancy_rate) TYPE occ_rate.

ENDCLASS.



CLASS zcl_demo_abap_unit_test IMPLEMENTATION.


  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).

    "Preparing a demo database table for this example (get_sum method)
    DELETE FROM zdemo_abap_tab1.
    INSERT zdemo_abap_tab1 FROM @(
      VALUE #( key_field = 1 char1 = 'aaa' char2 = 'bbb' num1 = 25 num2 = 75 ) ).
  ENDMETHOD.


  METHOD constructor.

    "For demonstrating the back door injection
    data_provider_local_itf = NEW lcl_data_prov_local_itf( ).

    "For demonstrating the constructor injection
    IF iref_data_prov IS BOUND.
      "Note: The parameter is only bound when you run the unit test.
      "When you run the unit test and you debug, you will see that iref_data_prov
      "has a type reference to LTD_TEST_DATA_GLOBAL_INTF.

      data_provider_global_itf = iref_data_prov.

    ELSE.

      data_provider_global_itf = NEW lcl_data_prov_glo_itf( ).

    ENDIF.

    "Object creation for the method call in the get_occ_rate_setter_inj method
    data_provider_setter_inj = NEW lcl_data_prov_glo_itf( ).

  ENDMETHOD.


  METHOD get_common_div_and_gcd.
    "Calculates the common divisors and the greatest common divisor of two numbers

    CLEAR: common_divisors, gcd.

    CHECK a >= 1.
    CHECK b >= 1.

    IF a >= b.
      DATA(greater_num) = a.
      DATA(lower_num) = b.
    ELSE.
      greater_num = b.
      lower_num = a.
    ENDIF.

    "Getting common divisors
    DATA(div) = 1.

    WHILE div <= lower_num.
      IF lower_num MOD div = 0.
        DATA(divisor) = lower_num / div.
        INSERT divisor INTO TABLE common_divisors.
      ENDIF.

      div += 1.
    ENDWHILE.

    LOOP AT common_divisors ASSIGNING FIELD-SYMBOL(<i>).

      IF greater_num MOD <i> <> 0.
        DELETE common_divisors WHERE table_line = <i>.
      ENDIF.

    ENDLOOP.

    "Extracting the greatest common divisor from the list of common divisors
    gcd = common_divisors[ lines( common_divisors ) ].

  ENDMETHOD.


  METHOD get_digit_sum.
    "Calculates the digit sum of a number

    CLEAR digit_sum.

    CHECK num >= 0.

    DATA(converted_int) = CONV string( num ).
    DATA(len) = strlen( converted_int ).

    DO len TIMES.
      DATA(idx) = sy-index - 1.
      digit_sum = digit_sum + converted_int+idx(1).
    ENDDO.

  ENDMETHOD.


  METHOD get_occ_rate_global_itf.
    "Method to demonstrate test double injection using constructor injection
    "and a global interface

    DATA total_seatsmax_global_itf TYPE i.
    DATA total_seatsocc_global_itf TYPE i.

    "Assumption: The original code in this method was as follows (the line commented out).
    "It was identified as DOC (reading data from a database table).

    "DATA(flight_data) = select_flight_data( carrier = carrier_id ).

    "Instead of a method call like above and for a proper unit testing, a global interface
    "is provided.
    "In the example, an interface method is implemented in a local class in the local types
    "tab (CCIMP include): lcl_data_prov_glo_itf

    "When the class is executed using F9, the object used here refers to type lcl_data_prov_glo_itf.
    "When the unit test is executed, the object used here refers to type ltd_test_data_global_intf,
    "i.e. the local test double is injected.
    DATA(flight_data) = data_provider_global_itf->select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data ASSIGNING FIELD-SYMBOL(<l>).

      total_seatsmax_global_itf = total_seatsmax_global_itf + <l>-seatsmax.
      total_seatsocc_global_itf = total_seatsocc_global_itf + <l>-seatsocc.

    ENDLOOP.

    occupancy_rate = total_seatsocc_global_itf / total_seatsmax_global_itf * 100.

  ENDMETHOD.


  METHOD get_occ_rate_local_itf.
    "Method to demonstrate test double injection using back door
    "injection and a local interface

    DATA total_seatsmax_local_itf TYPE i.
    DATA total_seatsocc_local_itf TYPE i.

    "Assumption: The original code in this method was as follows (the line commented out).
    "It was identified as DOC (reading data from a database table).

    "DATA(flight_data) = select_flight_data( carrier = carrier_id ).

    "Instead of a method call like above and for a proper unit testing - a global interface
    "is not available - a local interface is created, and
    "an interface method is implemented. In this example, the local interface is
    "created in the local types tab (CCIMP include): lif_get_data
    "A local class (lcl_data_prov_local_itf) is also created in the CCIMP include. It
    "implements the local interface.

    "When the class is executed using F9, the object used here refers to type lcl_data_prov_local_itf.
    "When the unit test is executed, the object used here refers to type ltd_test_data_local_itf,
    "i.e. the local test double is injected.
    DATA(flight_data) = data_provider_local_itf->select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data ASSIGNING FIELD-SYMBOL(<k>).

      total_seatsmax_local_itf = total_seatsmax_local_itf + <k>-seatsmax.
      total_seatsocc_local_itf = total_seatsocc_local_itf + <k>-seatsocc.

    ENDLOOP.

    occupancy_rate = total_seatsocc_local_itf / total_seatsmax_local_itf * 100.

  ENDMETHOD.


  METHOD get_occ_rate_param_inj.
    "This method demonstrates test double injection using parameter injection.

    DATA total_seatsmax_param_inj TYPE i.
    DATA total_seatsocc_param_inj TYPE i.

    "Assumption: The original code in this method was as follows (the line commented out).
    "It was identified as DOC (reading data from a database table).

    "DATA(flight_data) = select_flight_data( carrier = carrier_id ).

    "Instead of a method call like above and for a proper unit testing, a global interface
    "is provided.
    "In the example, an interface method is implemented in a local class in the local types
    "tab (CCIMP include): lcl_data_prov_glo_itf

    "The method has an optional importing parameter. When the unit test is executed,
    "the parameter is bound. An object of the test double class is passed in that case.
    "Otherwise, when the class is executed using F9, an object of the actual data provider
    "is created.
    IF data_prov IS BOUND.
      data_provider_param_inj = data_prov.
    ELSE.
      data_provider_param_inj = NEW lcl_data_prov_glo_itf( ).
    ENDIF.

    DATA(flight_data) = data_provider_param_inj->select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data ASSIGNING FIELD-SYMBOL(<o>).

      total_seatsmax_param_inj = total_seatsmax_param_inj + <o>-seatsmax.
      total_seatsocc_param_inj = total_seatsocc_param_inj + <o>-seatsocc.

    ENDLOOP.

    occupancy_rate = total_seatsocc_param_inj / total_seatsmax_param_inj * 100.

  ENDMETHOD.


  METHOD get_occ_rate_setter_inj.
    "This method demonstrates test double injection using setting injection.
    "See the setter_meth method.

    DATA total_seatsmax_setter_inj TYPE i.
    DATA total_seatsocc_setter_inj TYPE i.

    "Assumption: The original code in this method was as follows (the line commented out).
    "It was identified as DOC (reading data from a database table).

    "DATA(flight_data) = select_flight_data( carrier = carrier_id ).

    "Instead of a method call like above and for a proper unit testing, a global interface
    "is provided.
    "In the example, an interface method is implemented in a local class in the local types
    "tab (CCIMP include): lcl_data_prov_glo_itf

    "See the comment in the setter_meth method
    DATA(flight_data) = data_provider_setter_inj->select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data ASSIGNING FIELD-SYMBOL(<n>).

      total_seatsmax_setter_inj = total_seatsmax_setter_inj + <n>-seatsmax.
      total_seatsocc_setter_inj = total_seatsocc_setter_inj + <n>-seatsocc.

    ENDLOOP.

    occupancy_rate = total_seatsocc_setter_inj / total_seatsmax_setter_inj * 100.

  ENDMETHOD.


  METHOD get_occ_rate_test_seam.
    "Method to demonstrate test double injection using test seams
    "Note: The code is just for demonstration purposes. Of course, the result can be
    "achieved more elegantly using SQL expressions, for example.

    TEST-SEAM select_flights.
      "DOC
      SELECT seatsmax, seatsocc
        FROM zdemo_abap_fli
        WHERE carrid = @carrier_id
        INTO CORRESPONDING FIELDS OF TABLE @seats_table.
    END-TEST-SEAM.

    DATA total_seatsmax_tm TYPE i.
    DATA total_seatsocc_tm TYPE i.

    LOOP AT seats_table ASSIGNING FIELD-SYMBOL(<j>).

      total_seatsmax_tm = total_seatsmax_tm + <j>-seatsmax.
      total_seatsocc_tm = total_seatsocc_tm + <j>-seatsocc.

    ENDLOOP.

    occupancy_rate = total_seatsocc_tm / total_seatsmax_tm * 100.

    "Further examples for test seams
    DATA(var) = 0.

    "Empty test seam; code is injected during unit test
    "Check the output when running the class using F9 and
    "the test results when running the unit test.
    TEST-SEAM num1.
    END-TEST-SEAM.

    IF var = 0.
      num1 = 1.
    ELSE.
      num1 = 999.
    ENDIF.

    num2 = 0.

    "Empty injection
    "See the test class: The code that is included in the test
    "seam should be excluded from the test. Therefore, the
    "test injection block in the test class is empty.
    "Check the output when running the class using F9 and
    "the test results when running the unit test.
    TEST-SEAM num2.
      num2 = 123.
    END-TEST-SEAM.

  ENDMETHOD.


  METHOD get_occ_rate_using_meth.
    "This method demonstrates test double injection using inheritance and method redefinition.

    DATA total_seatsmax_no TYPE i.
    DATA total_seatsocc_no TYPE i.

    "During the unit test, the redefined method in the test class is called.
    DATA(flight_data) = select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data ASSIGNING FIELD-SYMBOL(<m>).

      total_seatsmax_no = total_seatsmax_no + <m>-seatsmax.
      total_seatsocc_no = total_seatsocc_no + <m>-seatsocc.

    ENDLOOP.

    occupancy_rate = total_seatsocc_no / total_seatsmax_no * 100.

  ENDMETHOD.


  METHOD get_sum.
    "The method selects a record from a database table and sums the values
    "of two fields, both are of type i.

    SELECT SINGLE
            FROM zdemo_abap_tab1
            FIELDS num1 + num2 AS sum
            WHERE key_field = @key
            AND char1 = @char
            INTO @sum.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    "Note: The example includes a couple of implementations for the methods
    "declared above. And by choosing F9 in ADT, you can run the class and check the
    "output in the console.
    "However, the focus of the example is unit tests. Therefore, check the
    "test classes and methods in the test include (Test Classes tab in ADT).
    "Choose Ctrl/Cmd + Shift + F10 to launch all tests in the class and check the
    "test results in the ABAP Unit tab in ADT.

    out->write( |ABAP Cheat Sheet Example: ABAP Unit Tests\n\n| ).

    out->write( `************************************************************************` ).
    out->write( `---> Choose Ctrl/Cmd + Shift + F10 to launch all tests in the class <---` ).
    out->write( |************************************************************************\n| ).

    out->write( |1) get_sum Method\n\n| ).
    "This method demonstrates the use of the setup and teardown methods in the test class.

    DATA(sum) = get_sum( key = 1 char = 'aaa' ).

    out->write( data = sum name = `sum`  ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) get_common_div_and_gcd Method` ) ).

    "Filling an internal table with numbers on whose bases the common divisors and the
    "greatest common divisor are to be calculated
    DATA(tab) = VALUE nums_tab( ( num1 = 10 num2 = 20 )
                                ( num1 = 100 num2 = 200 )
                                ( num1 = 12 num2 = 6 )
                                ( num1 = 5 num2 = 1 )
                                ( num1 = 50 num2 = 50 )
                                ( num1 = 4 num2 = 8 ) ).

    LOOP AT tab ASSIGNING FIELD-SYMBOL(<a>).
      DATA(tabix) = sy-tabix.
      get_common_div_and_gcd( EXPORTING a = <a>-num1
                                        b = <a>-num2
                              IMPORTING common_divisors = DATA(common_divs) gcd = DATA(gcd) ).

      out->write( |Common divisors of { <a>-num1 } and { <a>-num2 }\n|  ).
      out->write( |\n| ).
      out->write( data = common_divs name = `common_divs`  ).
      out->write( |\n| ).
      out->write( |Greatest common divisor of { <a>-num1 } and { <a>-num2 }: { gcd } |  ).
      out->write( |\n| ).
      IF tabix < lines( tab ).
        out->write( `--------------------------` ).
        out->write( |\n| ).
      ENDIF.
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) get_digit_sum Method` ) ).

    "Filling an internal table with numbers on whose bases the digit sum is to be calculated
    DATA(tab_i) = VALUE int_tab_so( ( 12 )
                                    ( 123 )
                                    ( 3 )
                                    ( 8246 )
                                    ( 1001001 )
                                    ( 0 ) ).

    LOOP AT tab_i ASSIGNING FIELD-SYMBOL(<b>).
      DATA(digit_sum) = get_digit_sum( <b> ).

      out->write( |The digit sum of { <b> } is { digit_sum }.|  ).
      out->write( |\n| ).
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) get_occ_rate_using_meth Method` ) ).
    "In the test class, this method demonstrates test double injection
    "using inheritance and method redefinition.

    "Filling an internal table with carrier ids on whose bases the occupancy
    "rate is to be calculated.
    DATA(tab_str) = VALUE zdemo_abap_get_data_itf=>carr_tab( ( carrid = 'LH' )
                                                              ( carrid = 'AA' )
                                                              ( carrid = 'DL' ) ).

    LOOP AT tab_str ASSIGNING FIELD-SYMBOL(<c>).
      DATA(occupancy_rate) = get_occ_rate_using_meth( <c>-carrid ).

      out->write( |The occupancy rate for airline { <c>-carrid } is { occupancy_rate }%.|  ).
      out->write( |\n| ).
    ENDLOOP.
    out->write( zcl_demo_abap_aux=>heading( `5) get_occ_rate_test_seam Method` ) ).
    "This method demonstrates test double injection using test seams.

    LOOP AT tab_str ASSIGNING FIELD-SYMBOL(<d>).
      get_occ_rate_test_seam( EXPORTING carrier_id = <d>-carrid
                              IMPORTING occupancy_rate = DATA(occupancy_rate_test_seam)
                                        num1           = DATA(num1)
                                        num2           = DATA(num2) ).

      out->write( |The occupancy rate for airline { <d>-carrid } is { occupancy_rate_test_seam }%.|  ).
      out->write( |\n| ).
      out->write( |num1: { num1 }|  ).
      out->write( |\n| ).
      out->write( |num2: { num2 }|  ).
      out->write( |\n| ).
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) get_occ_rate_local_itf Method` ) ).
    "This method demonstrates test double injection using back door
    "injection and a local interface.

    LOOP AT tab_str ASSIGNING FIELD-SYMBOL(<e>).
      DATA(occupancy_rate_local_itf) = get_occ_rate_local_itf( <e>-carrid ).

      out->write( |The occupancy rate for airline { <e>-carrid } is { occupancy_rate_local_itf }%.|  ).
      out->write( |\n| ).
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) get_occ_rate_global_itf Method` ) ).
    "This method demonstrates test double injection using constructor
    "injection and a global interface.

    LOOP AT tab_str ASSIGNING FIELD-SYMBOL(<f>).
      DATA(occupancy_rate_global_itf) = get_occ_rate_global_itf( <f>-carrid ).

      out->write( |The occupancy rate for airline { <f>-carrid } is { occupancy_rate_global_itf }%.| ).
      out->write( |\n| ).
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) get_occ_rate_setter_inj Method` ) ).
    "This method demonstrates test double injection using setter injection and a global interface.

    LOOP AT tab_str ASSIGNING FIELD-SYMBOL(<g>).
      DATA(occupancy_rate_setter_inj) = get_occ_rate_setter_inj( <g>-carrid ).

      out->write( |The occupancy rate for airline { <g>-carrid } is { occupancy_rate_setter_inj }%.| ).
      out->write( |\n| ).
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `9) get_occ_rate_param_inj Method` ) ).
    "This method demonstrates test double injection using parameter injection and a global interface.

    LOOP AT tab_str ASSIGNING FIELD-SYMBOL(<h>).
      DATA(occupancy_rate_param_inj) = get_occ_rate_param_inj( carrier_id = <h>-carrid ).

      out->write( |The occupancy rate for airline { <h>-carrid } is { occupancy_rate_param_inj }%.| ).
      out->write( |\n| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD select_flight_data.
    "Method that is identified as DOC in the method implementations above.
    "This method is also used for demonstrating test double injection and method redefinition.

    SELECT seatsmax, seatsocc
      FROM zdemo_abap_fli
      WHERE carrid = @carrier
      INTO CORRESPONDING FIELDS OF TABLE @flight_data.
  ENDMETHOD.


  METHOD setter_meth.
    "Method to demonstrate the test double injection using setter injection

    "When the unit test is executed, an object of the test double class is passed as
    "a parameter. Then, the object used here refers to type ltd_test_data_setter_inj,
    "i.e. the local test double is injected.
    data_provider_setter_inj =  data_prov.
  ENDMETHOD.
ENDCLASS.
