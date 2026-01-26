"! <p class="shorttext"><strong>State</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the state design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_state DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap_oodp_state IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `State` ).

*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

    "In Example 1, the client transitions the states of the objects by
    "passing instances of the state classes to the set_state method.
    "The example begins with the creation of an object of the lcl_math
    "class. Its constructor expects an integer value, which serves as the
    "first operand for the calculation. The example method calls illustrate
    "several state transitions. When executing the execute method, the
    "second operand is provided as an argument, and the calculation is
    "performed based on the current state.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Example 1` ).

    DATA(oref) = NEW lcl_math( 15 ).

    "Performing additions by transitioning the state
    DATA(oref_add) = NEW lcl_state_addition( ).

    oref->set_state( oref_add ).
    DATA(result) = oref->execute( 5 ).
    out->write( result ).

    result = oref->execute( 10 ).
    out->write( result ).

    result = oref->execute( 40 ).
    out->write( result ).

    "Performing subtractions by transitioning the state
    DATA(oref_sub) = NEW lcl_state_subtraction( ).

    oref->set_state( oref_sub ).
    result = oref->execute( 10 ).
    out->write( result ).

    result = oref->execute( 80 ).
    out->write( result ).

    "Transitioning the state back to perform an addition
    oref->set_state( oref_add ).

    result = oref->execute( 25 ).
    out->write( result ).

    "Performing multiplications by transitioning the state
    DATA(oref_mul) = NEW lcl_state_multiplication( ).

    oref->set_state( oref_mul ).
    result = oref->execute( 5 ).
    out->write( result ).

    result = oref->execute( 4 ).
    out->write( result ).

    "Transitioning the state back to perform a subtraction
    oref->set_state( oref_sub ).
    result = oref->execute( 99 ).
    out->write( result ).

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

    "Example 2 is similar to Example 1, and yields the same results. Here,
    "the state transitions are performed by the class whose objects' behavior
    "changes, based on external input. In this case, the input is realized by
    "passing an enumeration type, which is evaluated in the class.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Example 2` ).

    DATA(oref_cond) = NEW lcl_math_cond( 15 ).

    oref_cond->set_state( lcl_math_cond=>plus ).

    result = oref_cond->execute( 5 ).
    out->write( result ).

    result = oref_cond->execute( 10 ).
    out->write( result ).

    result = oref_cond->execute( 40 ).
    out->write( result ).

    oref_cond->set_state( lcl_math_cond=>minus ).

    result = oref_cond->execute( 10 ).
    out->write( result ).

    result = oref_cond->execute( 80 ).
    out->write( result ).

    oref_cond->set_state( lcl_math_cond=>plus ).

    result = oref_cond->execute( 25 ).
    out->write( result ).

    oref_cond->set_state( lcl_math_cond=>multiply ).

    result = oref_cond->execute( 5 ).
    out->write( result ).

    result = oref_cond->execute( 4 ).
    out->write( result ).

    oref_cond->set_state( lcl_math_cond=>minus ).

    result = oref_cond->execute( 99 ).
    out->write( result ).
  ENDMETHOD.
ENDCLASS.
