"! <p class="shorttext"><strong>Observer</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the observer design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_observer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_oodp_observer IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Observer` ).

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Example 1` ).

*&-------------------------------------------------------------------------------------*
*& 1) Creating objects, observers not registered
*&-------------------------------------------------------------------------------------*

    out->write( |1) Creating objects, observers not registered\n\n| ).

    DATA(oref_observed) = NEW lcl( ).
    "Observers
    DATA(observer_add) = NEW lcl_addition( ).
    DATA(observer_subtract) = NEW lcl_subtraction( ).
    DATA(observer_multiply) = NEW lcl_multiplication( ).
    DATA(observer_divide) = NEW lcl_division( ).

    "Setting numbers representing a change of the object state
    "The method implementation includes a call to the notification method.
    oref_observed->set_numbers(
      value1 = 1
      value2 = 2 ).

    "At this stage, there are no observers registered.
    DATA(log) = lcl=>tab4display.

    out->write( data = log name = `log` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).
    CLEAR lcl=>tab4display.

*&-------------------------------------------------------------------------------------*
*& 2) Registering observers
*&-------------------------------------------------------------------------------------*

    out->write( |2) Registering observers\n\n| ).

    oref_observed->register( observer_add ).
    oref_observed->register( observer_subtract ).
    oref_observed->register( observer_multiply ).
    oref_observed->register( observer_divide ).

    oref_observed->set_numbers(
      value1 = 7
      value2 = 5 ).

    log = lcl=>tab4display.

    out->write( data = log name = `log` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

    CLEAR lcl=>tab4display.

*&-------------------------------------------------------------------------------------*
*& 3) Unregistering observers
*&-------------------------------------------------------------------------------------*

    out->write( |3) Unregistering observers\n\n| ).

    oref_observed->unregister( observer_multiply ).
    oref_observed->unregister( observer_divide ).

    oref_observed->set_numbers(
      value1 = 15
      value2 = 3 ).

    log = lcl=>tab4display.

    out->write( data = log name = `log` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

    CLEAR lcl=>tab4display.

*&-------------------------------------------------------------------------------------*
*& 4) More example values
*&-------------------------------------------------------------------------------------*

    out->write( |4) More example values\n\n| ).

    "Registering the unregistered observers again
    oref_observed->register( observer_multiply ).
    oref_observed->register( observer_divide ).

    "Creating an internal table holding two integer values per line
    TYPES: BEGIN OF struc_int,
             number1 TYPE i,
             number2 TYPE i,
           END OF struc_int.

    DATA numbers_tab TYPE TABLE OF struc_int WITH EMPTY KEY.

    numbers_tab = VALUE #( ( number1 = 1 number2 = 8 )
                           ( number1 = 50 number2 = 25 )
                           ( number1 = 100 number2 = 4 )
                           ( number1 = 24 number2 = 6 )
                           ( number1 = 35 number2 = 7 )
                           ( number1 = 2 number2 = 0 ) ).

    LOOP AT numbers_tab INTO DATA(wa).
      oref_observed->set_numbers(
        value1 = wa-number1
        value2 = wa-number2 ).

      APPEND INITIAL LINE TO lcl=>tab4display.
    ENDLOOP.

    log = lcl=>tab4display.

    out->write( data = log name = `log` ).
    out->write( |{ repeat( val = `*` occ = 75 ) }| ).
    out->write( |{ repeat( val = `*` occ = 75 ) }| ).
    out->write( |{ repeat( val = `*` occ = 75 ) }\n| ).

**********************************************************************
**********************************************************************
**********************************************************************

*&-------------------------------------------------------------------------------------*
*& Example 2
*&-------------------------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Example 2` ).

    out->write( |6) Registering event handlers\n\n| ).

    DATA(o1) = NEW lcl_evt( ).
    DATA iref1 TYPE REF TO lif_obs.
    iref1 = NEW lcl_obs_1( ).
    DATA iref2 TYPE REF TO lif_obs.
    iref2 = NEW lcl_obs_2( ).

    "Registering event handlers
    SET HANDLER iref1->handle_event FOR o1.
    SET HANDLER iref2->handle_event FOR o1.

    o1->set_text( `AB` ).

    log = lcl_evt=>tab4display.
    out->write( data = log name = `log` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

    CLEAR lcl_evt=>tab4display.

**********************************************************************

    out->write( |7) Unregistering an event handler\n\n| ).

    SET HANDLER iref2->handle_event FOR o1 ACTIVATION ' '.

    o1->set_text( `Hello AB` ).

    log = lcl_evt=>tab4display.
    out->write( data = log name = `log` ).
  ENDMETHOD.
ENDCLASS.
