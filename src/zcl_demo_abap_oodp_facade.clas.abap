"! <p class="shorttext"><strong>Facade</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the facade design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_facade DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_oodp_facade IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Facade` ).

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Travel options 1` ).

    DATA(travel_object) = NEW lcl_travel_facade( ).
    DATA(travel_options_1) = travel_object->plan_travel(
                from      = 'Frankfurt'
                to        = 'Shanghai'
                arrival   = '20250511'
                departure = '20250521' ).

    out->write( travel_options_1 ).

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Travel options 2` ).

    DATA(travel_options_2) = travel_object->plan_travel(
                from      = 'Frankfurt'
                to        = 'Shanghai'
                arrival   = '20250605'
                departure = '20250617' ).

    out->write( travel_options_2 ).

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Travel options 3` ).

    DATA(travel_options_3) = travel_object->plan_travel(
                  from      = 'Frankfurt'
                  to        = 'Shanghai'
                  arrival   = '20250403'
                  departure = '20250410' ).

    out->write( travel_options_3 ).
  ENDMETHOD.
ENDCLASS.
