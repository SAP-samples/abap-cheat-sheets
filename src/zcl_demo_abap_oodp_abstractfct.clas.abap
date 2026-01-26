"! <p class="shorttext"><strong>Abstract Factory</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the abstract factory design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_abstractfct DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_oodp_abstractfct IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                                                 text = `Abstract Factory` ).

    DATA(italian_menu) = NEW menu_provider( customer_order=>place_order( menu_factory=>italian ) )->create_menu(  ).
    out->write( data = italian_menu name = `italian_menu` ).
    out->write( |\n| ).

    DATA(seafood_menu) = NEW menu_provider( customer_order=>place_order( menu_factory=>seafood ) )->create_menu(  ).
    out->write( data = seafood_menu name = `seafood_menu` ).
    out->write( |\n| ).

    DATA(vegan_menu) = NEW menu_provider( customer_order=>place_order( menu_factory=>vegan ) )->create_menu(  ).
    out->write( data = vegan_menu name = `vegan_menu` ).
    out->write( |\n| ).
  ENDMETHOD.
ENDCLASS.
