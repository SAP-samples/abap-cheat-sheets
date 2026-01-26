"! <p class="shorttext"><strong>Fluent Interface</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the fluent interface design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_fluent_intf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_oodp_fluent_intf IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Fluent Interface` ).

*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Example 1` ).

    "Adding strings
    "Retrieving the resulting string using the attribute 'str'
    DATA(str1) = lcl_string=>string( `Lorem` )->add( ` ` )->add( `ipsum` )->str.

    "Instead of extra method calls using the reference variable
    DATA(str1b_ref) = lcl_string=>string( `Lorem` ).
    str1b_ref->add( ` ` ).
    str1b_ref->add( `ipsum` ).
    DATA(str1b) = str1b_ref->str.

    "Retrieving the resulting string using the method 'get_string'
    DATA(str2) = lcl_string=>string( `Lorem` )->add( ` ` )->add( `ipsum` )->add( ` ` )->add( `dolor` )->add( ` ` )->add( `sit` )->add( ` ` )->add( `amet` )->get_string( ).

    "Preceding strings
    DATA(str3) = lcl_string=>string( `world` )->precede( ` ` )->precede( `Hello` )->str.
    DATA(str4) = lcl_string=>string( `B` )->add( `A` )->precede( `A` )->add( `P` )->str.

    "Splitting into string table
    DATA(tab1) = lcl_string=>string( `Lorem` )->add( `#` )->add( `ipsum` )->add( `#` )->add( `dolor` )->add( `#` )->add( `sit` )->add( `#` )->add( `amet` )->split_into_table( `#` ).
    DATA(tab2) = lcl_string=>string( `Lorem` )->add( ` ` )->add( `ipsum` )->split_into_table( ` ` ).

    "Replacements
    DATA(str5) = lcl_string=>string( `Lorem#ipsum#dolor#sit#amet` )->replace_all( sub = `#` with = ` ` )->str.
    DATA(str6) = lcl_string=>string( `Lorem#ipsum#dolor#sit#amet` )->replace_occ( sub = `#` with = ` ` occ = 1 )->str.
    DATA(str7) = lcl_string=>string( `Lorem#ipsum#dolor#sit#amet` )->replace_occ( sub = `#` with = ` ` occ = 2 )->str.
    DATA(str8) = lcl_string=>string( `Lorem#ipsum#dolor#sit#amet` )->replace_occ( sub = `#` with = ` ` occ = -2 )->str.
    DATA(tab3) = lcl_string=>string( `hello` )->add( `#` )->add( `world` )->replace_all( sub = `#` with = `,` )->split_into_table( `,` ).

    "Transforming to lowercase and uppercase
    DATA(str9) = lcl_string=>string( `ab` )->add( `ap` )->uppercase( )->str.
    DATA(str10) = lcl_string=>string( `AP` )->precede( `AB` )->lowercase( )->str.
    DATA(str11) = lcl_string=>string( `AB` )->lowercase( )->add( `ap` )->uppercase( )->str. "First lowercasing overridden

    "Reversing string
    DATA(str12) = lcl_string=>string( `OLL` )->add( `AH` )->lowercase( )->reverse_string( )->str.

    "Inserting string
    DATA(str13) = lcl_string=>string( `abcghi` )->insert_string( string = `def` off = 3 )->str.
    DATA(str14) = lcl_string=>string( `vwxyz` )->insert_string( string = `stu` off = 0 )->str.

    "Removing spaces
    "All spaces
    DATA(str15) = lcl_string=>string( ` a b  c` )->add( ` d  e   f     gh i   ` )->remove_all_spaces( )->str.
    "Leading and trailing spaces
    DATA(str16) = lcl_string=>string( `      ab c d   e f     g   h i     ` )->remove_leading_trailing_spaces( )->str.
    DATA(str17) = lcl_string=>string( `abc     ` )->remove_leading_trailing_spaces( )->add( `def` )->str.

    "Displaying results in the console
    out->write( data = str1 name = `str1` ).
    out->write( |\n| ).
    out->write( data = str1b name = `str1b` ).
    out->write( |\n| ).
    out->write( data = str2 name = `str2` ).
    out->write( |\n| ).
    out->write( data = str3 name = `str3` ).
    out->write( |\n| ).
    out->write( data = str4 name = `str4` ).
    out->write( |\n| ).
    out->write( data = tab1 name = `tab1` ).
    out->write( |\n| ).
    out->write( data = tab2 name = `tab2` ).
    out->write( |\n| ).
    out->write( data = str5 name = `str5` ).
    out->write( |\n| ).
    out->write( data = str6 name = `str6` ).
    out->write( |\n| ).
    out->write( data = str7 name = `str7` ).
    out->write( |\n| ).
    out->write( data = str8 name = `str8` ).
    out->write( |\n| ).
    out->write( data = tab3 name = `tab3` ).
    out->write( |\n| ).
    out->write( data = str9 name = `str9` ).
    out->write( |\n| ).
    out->write( data = str10 name = `str10` ).
    out->write( |\n| ).
    out->write( data = str11 name = `str11` ).
    out->write( |\n| ).
    out->write( data = str12 name = `str12` ).
    out->write( |\n| ).
    out->write( data = str13 name = `str13` ).
    out->write( |\n| ).
    out->write( data = str14 name = `str14` ).
    out->write( |\n| ).
    out->write( data = str15 name = `str15` ).
    out->write( |\n| ).
    out->write( data = str16 name = `str16` ).
    out->write( |\n| ).
    out->write( data = str17 name = `str17` ).
    out->write( |\n| ).

**********************************************************************

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Example 2` ).

    DATA(calc1) = NEW lcl_calc( 1 )->plus( 2 )->get_result( ).
    DATA(calc2) = NEW lcl_calc( 1 )->minus( 2 )->get_result( ).
    DATA(calc3) = NEW lcl_calc( 5 )->plus( 2 )->minus( 1 )->multiply( 3 )->get_result( ).
    DATA(calc4) = NEW lcl_calc( 10 )->multiply( 10 )->divide( 2 )->get_result( ).
    DATA(calc5) = NEW lcl_calc( 0 )->plus( 1 )->divide( 5 )->get_result( ).
    DATA(calc6) = NEW lcl_calc( '1.2' )->plus( '1.4' )->minus( '0.1' )->multiply( '2.5' )->divide( 2 )->get_result( ).

    "Arithmetic errors are just ignored in the example
    DATA(calc7) = NEW lcl_calc( 1 )->divide( 0 )->plus( 1 )->get_result( ).

    "Method chaining with a standalone statements
    NEW lcl_calc( 1 )->plus( 2 )->multiply( 5 )->minus( 5 )->divide( 2 )->get_result( RECEIVING result = DATA(calc8) ).

    IF NEW lcl_calc( 1 )->plus( 2 )->minus( 3 )->plus( 4 )->minus( 5 )->get_result( ) <= 0.
      DATA(if_statement) = `The result is equal to or lower than 0`.
    ELSE.
      if_statement = `The result is greater than 0`.
    ENDIF.

    out->write( data = calc1 name = `calc1` ).
    out->write( |\n| ).
    out->write( data = calc2 name = `calc2` ).
    out->write( |\n| ).
    out->write( data = calc3 name = `calc3` ).
    out->write( |\n| ).
    out->write( data = calc3 name = `calc3` ).
    out->write( |\n| ).
    out->write( data = calc4 name = `calc4` ).
    out->write( |\n| ).
    out->write( data = calc5 name = `calc5` ).
    out->write( |\n| ).
    out->write( data = calc6 name = `calc6` ).
    out->write( |\n| ).
    out->write( data = calc7 name = `calc7` ).
    out->write( |\n| ).
    out->write( data = calc8 name = `calc8` ).
    out->write( |\n| ).
    out->write( data = if_statement name = `if_statement` ).

  ENDMETHOD.

ENDCLASS.
