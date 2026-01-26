"! <p class="shorttext"><strong>Backdoor Injection</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates backdoor injection as ABAP Unit test double injection
"! technique.<br/>
"! Choose F9 in ADT to run the class. To run ABAP Unit tests, choose <em>Ctrl/Cmd + Shift + F10.</em></p>
"!
"! <h2>Note</h2>
"! <p>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</p>
CLASS zcl_demo_abap_oodp_aunit_backd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: carr_tab TYPE TABLE OF zdemoabapflights WITH EMPTY KEY,
           occ_rate TYPE p LENGTH 5 DECIMALS 2.

    CLASS-METHODS class_constructor.

    METHODS: constructor,
      "Method to demonstrate test double injection using back door injection
      get_occ_rate IMPORTING carrier_id            TYPE zdemoabapflights-carrid
                   RETURNING VALUE(occupancy_rate) TYPE occ_rate.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA data_provider TYPE REF TO lif_get_data.
ENDCLASS.


CLASS zcl_demo_abap_oodp_aunit_backd IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Backdoor Injection` ).

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

    "When the method to be tested is called "regularly" (for example, when executing the class using F9),
    "the object refers to the "regular" data provider (type ref to lcl_data_provider), which means it uses
    "production data. However, when the unit test is executed (for example, by choosing Ctrl/Cmd + Shift + F10),
    "the object refers to the test double (type ref to ltd_test_data), which is the local test double that gets
    "injected.
    DATA(flight_data) = data_provider->select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data INTO DATA(wa).
      total_seatsmax = total_seatsmax + wa-seatsmax.
      total_seatsocc = total_seatsocc + wa-seatsocc.
    ENDLOOP.

    occupancy_rate = total_seatsocc / total_seatsmax * 100.
  ENDMETHOD.

  METHOD constructor.
    data_provider = NEW lcl_data_provider( ).
  ENDMETHOD.

ENDCLASS.
