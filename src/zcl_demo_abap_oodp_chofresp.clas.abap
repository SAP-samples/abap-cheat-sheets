"! <p class="shorttext"><strong>Chain of Responsibility</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the chain of responsibility design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_chofresp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS process_chain IMPORTING type_description_object TYPE REF TO cl_abap_typedescr
                                    example                 TYPE string
                                    out                     TYPE REF TO if_oo_adt_classrun_out.
ENDCLASS.


CLASS zcl_demo_abap_oodp_chofresp IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Chain of Responsibility` ).

*&---------------------------------------------------------------------*
*& Example 1: Elementary type (local type)
*&---------------------------------------------------------------------*

    TYPES packed TYPE p LENGTH 16 DECIMALS 14.
    DATA(type_descr_obj) = cl_abap_typedescr=>describe_by_name( 'PACKED' ).

    process_chain(
      type_description_object = type_descr_obj
      example = `1) Elementary type (local type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Example 2: Elementary type (global DDIC type)
*&---------------------------------------------------------------------*

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'LAND1' ).

    process_chain(
      type_description_object = type_descr_obj
      example = `2) Elementary type (global DDIC type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Example 3: Structured type (local type)
*&---------------------------------------------------------------------*

    TYPES: BEGIN OF demo_struc,
             comp1 TYPE c LENGTH 3,
             comp2 TYPE i,
             comp3 TYPE string,
             comp4 TYPE n LENGTH 5,
           END OF demo_struc.

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'DEMO_STRUC' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `3) Structured type (local type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Example 4: Structured type (global type)
*&---------------------------------------------------------------------*

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'I_TIMEZONE' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `4) Structured type (global type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Example 5: Table type (local type)
*&---------------------------------------------------------------------*

    TYPES tab_type TYPE HASHED TABLE OF demo_struc
      WITH UNIQUE KEY comp1 comp2
      WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp4.

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'TAB_TYPE' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `5) Table type (local type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Example 6: Table type (global type)
*&---------------------------------------------------------------------*

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'STRING_TABLE' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `6) Table type (global type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Examples for types that are not covered by concrete handler classes
*&---------------------------------------------------------------------*

    "Enumerated type
    TYPES: BEGIN OF ENUM t_enum,
             a,
             b,
             c,
             d,
           END OF ENUM t_enum.

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'T_ENUM' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `7) Enumerated type`
      out                     = out
    ).

    "Reference type
    TYPES ref TYPE REF TO string.

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'REF' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `8) Reference type`
      out                     = out
    ).

    "Class
    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'CL_SYSTEM_UUID' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `9) Class`
      out                     = out
    ).

    "Interface
    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'IF_OO_ADT_CLASSRUN' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `10) Interface`
      out                     = out
    ).
  ENDMETHOD.

  METHOD process_chain.
    DATA: handler1 TYPE REF TO lif_chain_of_resp,
          handler2 TYPE REF TO lif_chain_of_resp,
          handler3 TYPE REF TO lif_chain_of_resp,
          handler4 TYPE REF TO lif_chain_of_resp,
          info_tab TYPE string_table.

    handler1 = NEW lcl_concrete_handler1( ).
    handler2 = NEW lcl_concrete_handler2( ).
    handler3 = NEW lcl_concrete_handler3( ).
    handler4 = NEW lcl_concrete_handler4( ).

    handler1->set_handler( handler2 ).
    handler2->set_handler( handler3 ).
    handler3->set_handler( handler4 ).

    DATA(info) = handler1->process( type_description_object ).

    IF info IS INITIAL.
      APPEND |-------------------- { example } --------------------| TO info.
    ELSE.
      INSERT LINES OF VALUE string_table( ( |-------------------- { example } --------------------| ) ( ) ) INTO info INDEX 1.
    ENDIF.

    out->write( info ).
    out->write( |\n{ repeat( val = `*` occ = 100 ) }\n\n| ).
  ENDMETHOD.
ENDCLASS.
