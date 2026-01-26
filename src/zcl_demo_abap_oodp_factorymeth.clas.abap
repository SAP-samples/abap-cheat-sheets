"! <p class="shorttext"><strong>Factory Method</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the factory method design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_factorymeth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_oodp_factorymeth IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Factory Method` ).

    "Saying hello in English
    DATA(oref_en) = lcl_factory_cl=>create_hello( lif_factory=>en ).
    DATA(hello_en) = oref_en->say_hello( ).
    out->write( hello_en ).

    "Saying hello in French
    DATA(oref_fr) = lcl_factory_cl=>create_hello( lif_factory=>fr ).
    DATA(hello_fr) = oref_fr->say_hello( ).
    out->write( hello_fr ).

    "Saying hello in Italian
    DATA(oref_it) = lcl_factory_cl=>create_hello( lif_factory=>it ).
    DATA(hello_it) = oref_it->say_hello( ).
    out->write( hello_it ).

    "Saying hello in Spanish
    DATA(oref_es) = lcl_factory_cl=>create_hello( lif_factory=>es ).
    DATA(hello_es) = oref_es->say_hello( ).
    out->write( hello_es ).

    "Saying hello in German
    DATA(oref_de) = lcl_factory_cl=>create_hello( lif_factory=>de ).
    DATA(hello_de) = oref_de->say_hello( ).
    out->write( hello_de ).

    "Default hello
    DATA(oref_default) = lcl_factory_cl=>create_hello( lif_factory=>init ).
    DATA(hello_default) = oref_default->say_hello( ).
    out->write( hello_default ).
  ENDMETHOD.
ENDCLASS.
