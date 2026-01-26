"! <p class="shorttext"><strong>Strategy</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the strategy design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_strategy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_oodp_strategy IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Strategy` ).

*&---------------------------------------------------------------------*
*& 1) Example setup with an interface
*&---------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `1) Example setup with an interface` ).

    DATA(calc) = NEW lcl_calculator( ).

    calc->set_strategy_ref( NEW lcl_addition( ) ).
    DATA(result) = calc->calculate( num1 = 7 num2 = 4 ).
    out->write( |7 + 4 = { result }| ).

    calc->set_strategy_ref( NEW lcl_subtraction( ) ).
    result = calc->calculate( num1 = 25 num2 = 15 ).
    out->write( |25 - 15 = { result }| ).

    calc->set_strategy_ref( NEW lcl_multiplication( ) ).
    result = calc->calculate( num1 = 8 num2 = 4 ).
    out->write( |8 * 4 = { result }| ).

    calc->set_strategy_ref( NEW lcl_division( ) ).
    result = calc->calculate( num1 = 36 num2 = 6 ).
    out->write( |36 / 6 = { result }| ).

*&---------------------------------------------------------------------*
*& 2) Example setup with an abstract class
*&---------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `2) Example setup with an abstract class` ).

    DATA(calc_abs) = NEW lcl_calc( ).

    calc_abs->set_strategy_ref( NEW lcl_add( ) ).
    result = calc_abs->calculate( num1 = 12 num2 = 8 ).
    out->write( |12 + 8 = { result }| ).

    calc_abs->set_strategy_ref( NEW lcl_sub( ) ).
    result = calc_abs->calculate( num1 = 5 num2 = 10 ).
    out->write( |5 - 10 = { result }| ).

    calc_abs->set_strategy_ref( NEW lcl_mult( ) ).
    result = calc_abs->calculate( num1 = 10 num2 = 10 ).
    out->write( |10 * 10 = { result }| ).

    calc_abs->set_strategy_ref( NEW lcl_div( ) ).
    result = calc_abs->calculate( num1 = 81 num2 = 9 ).
    out->write( |81 / 9 = { result }| ).
  ENDMETHOD.
ENDCLASS.
