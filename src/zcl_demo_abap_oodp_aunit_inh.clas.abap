"! <p class="shorttext"><strong>Test Double Injection Using Inheritance and Method Redefinition</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates inheritance and method redefinition as ABAP Unit test double injection
"! technique.<br/>
"! Choose F9 in ADT to run the class. To run ABAP Unit tests, choose <em>Ctrl/Cmd + Shift + F10.</em></p>
"!
"! <h2>Note</h2>
"! <p>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</p>
CLASS zcl_demo_abap_oodp_aunit_inh DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: carr_tab TYPE TABLE OF zdemoabapflights WITH EMPTY KEY,
           occ_rate TYPE p LENGTH 5 DECIMALS 2.

    CLASS-METHODS class_constructor.
    METHODS get_occ_rate IMPORTING carrier_id            TYPE zdemoabapflights-carrid
                         RETURNING VALUE(occupancy_rate) TYPE occ_rate.

  PROTECTED SECTION.
    METHODS select_flight_data IMPORTING carrier            TYPE zdemoabapflights-carrid
                               RETURNING VALUE(flight_data) TYPE carr_tab.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap_oodp_aunit_inh IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Test Double Injection Using Inheritance and Method Redefinition` ).

    "Filling an internal table with carrier IDs used to calculate
    "the occupancy rate.
    DATA(carrier_tab) = VALUE carr_tab( ( carrid = 'LH' )
                                        ( carrid = 'AA' )
                                        ( carrid = 'DL' ) ).

    LOOP AT carrier_tab INTO DATA(carr).
      DATA(oc_rate) = get_occ_rate( carr-carrid ).

      out->write( |The occupancy rate for airline { carr-carrid } is { oc_rate }%.|  ).
      out->write( |\n| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD class_constructor.
    "Clearing and filling demo database tables.
    zcl_demo_abap_oodp_aux=>fill_dbtabs( ).
  ENDMETHOD.

  METHOD get_occ_rate.
    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.

    "During the unit test, the redefined method in the test class is called.
    DATA(flight_data) = select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data INTO DATA(wa).
      total_seatsmax = total_seatsmax + wa-seatsmax.
      total_seatsocc = total_seatsocc + wa-seatsocc.
    ENDLOOP.

    occupancy_rate = total_seatsocc / total_seatsmax * 100.
  ENDMETHOD.

  METHOD select_flight_data.
    SELECT seatsmax, seatsocc
      FROM zdemoabapflights
      WHERE carrid = @carrier
      INTO CORRESPONDING FIELDS OF TABLE @flight_data.
  ENDMETHOD.
ENDCLASS.
