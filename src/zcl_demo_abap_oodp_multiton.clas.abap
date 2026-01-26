"! <p class="shorttext"><strong>Multiton</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the multiton design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_multiton DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_oodp_multiton IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Multiton` ).

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = |1) First call with key { lcl_multiton=>config_a }| ).

    DATA(oref_config_a_1) = lcl_multiton=>get_instance( key = lcl_multiton=>config_a ).
    oref_config_a_1->do_something( ).
    DATA(tab) = oref_config_a_1->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = |2) Second call with key { lcl_multiton=>config_a }| ).

    DATA(oref_config_a_2) = lcl_multiton=>get_instance( key = lcl_multiton=>config_a ).
    oref_config_a_2->do_something( ).
    tab = oref_config_a_2->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).
    out->write( |\n\n| ).

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = |3) Third call with key { lcl_multiton=>config_a }| ).

    DATA(oref_config_a_3) = lcl_multiton=>get_instance( key = lcl_multiton=>config_a ).
    oref_config_a_3->do_something( ).
    tab = oref_config_a_3->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).
    out->write( |\n\n| ).

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = |4) First call with key { lcl_multiton=>config_b }| ).

    DATA(oref_config_b_1) = lcl_multiton=>get_instance( key = lcl_multiton=>config_b ).
    oref_config_b_1->do_something( ).
    tab = oref_config_b_1->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).
    out->write( |\n\n| ).

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = |5) Second call with key { lcl_multiton=>config_b }| ).

    DATA(oref_config_b_2) = lcl_multiton=>get_instance( key = lcl_multiton=>config_b ).
    oref_config_b_2->do_something( ).
    tab = oref_config_b_2->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).
    out->write( |\n\n| ).

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = |6) Call with key { lcl_multiton=>config_c }| ).

    DATA(oref_config_c) = lcl_multiton=>get_instance( key = lcl_multiton=>config_c ).
    oref_config_c->do_something( ).
    tab = oref_config_c->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).
    out->write( |\n\n| ).

**********************************************************************

    "Using random keys
    "The base type of the enumerated type is i, and the example type has 12 components.
    "I.e. config_a corresponds to the value 0, ..., config_m corresponds to 11.
    "Here, the cl_abap_random_int class is used to create random integer values within
    "the range of 0 - 12. This random value is then passed when calling the get_instance
    "method. The output will show which enumerated type component is used.
    DO 5 TIMES.
      DATA(random_number) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                        min  = 0
                                                        max  = 11 )->get_next( ).

      DATA(enum) = CONV lcl_multiton=>t_enum( random_number ).

      zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = |{ 6 + sy-index }) Call with key { enum }| ).
      DATA(oref_config_random) = lcl_multiton=>get_instance( key = enum ).
      oref_config_random->do_something( ).
      tab = oref_config_random->get_config_change_tab( ).

      out->write( data = tab name = `tab` ).
      out->write( |\n\n| ).
    ENDDO.

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Log` ).

    "Log table that tracks the timestamp of the first instantiation per
    "key and the number of calls.
    DATA(log) = lcl_multiton=>log.
    out->write( data = log name = `log` ).

  ENDMETHOD.

ENDCLASS.
