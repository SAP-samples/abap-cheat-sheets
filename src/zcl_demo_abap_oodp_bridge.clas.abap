"! <p class="shorttext"><strong>Bridge</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the bridge design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_bridge DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_oodp_bridge IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Bridge` ).

    DATA compact_car1 TYPE REF TO lcl_compact_car.
    DATA factory_a TYPE REF TO lcl_factory_a.
    factory_a = NEW #( ).
    compact_car1 = NEW lcl_compact_car( factory_a ).
    DATA(log1) = compact_car1->produce( ).

    out->write( log1 ).
    out->write( |\n\n| ).

    DATA(compact_car2) = NEW lcl_compact_car( NEW lcl_factory_b( ) ).
    DATA(log2) = compact_car2->produce( ).
    out->write( log2 ).
    out->write( |\n\n| ).

    DATA(suv1)    = NEW lcl_suv( NEW lcl_factory_c( ) ).
    DATA(log3) = suv1->produce( ).
    out->write( log3 ).
    out->write( |\n\n| ).

    DATA(suv2)    = NEW lcl_suv( NEW lcl_factory_a( ) ).
    DATA(log4) = suv2->produce( ).
    out->write( log4 ).
    out->write( |\n\n| ).

    DATA(sports_car1) = NEW lcl_sports_car( NEW lcl_factory_a( ) ).
    DATA(log5) = sports_car1->produce( ).
    out->write( log5 ).
    out->write( |\n\n| ).

    DATA(sports_car2) = NEW lcl_sports_car( NEW lcl_factory_b( ) ).
    DATA(log6) = sports_car2->produce( ).
    out->write( log6 ).
    out->write( |\n\n| ).

    DATA(sports_car3) = NEW lcl_sports_car( NEW lcl_factory_c( ) ).
    DATA(log7) = sports_car3->produce( ).
    out->write( log7 ).
  ENDMETHOD.
ENDCLASS.
