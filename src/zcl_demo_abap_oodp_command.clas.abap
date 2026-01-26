"! <p class="shorttext"><strong>Command</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the command design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_command DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-DATA log TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap_oodp_command IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Command` ).

*&---------------------------------------------------------------------*
*& Creating receiver, invoker and command objects and triggering
*& the execution of commands
*&---------------------------------------------------------------------*

    "Creating an object of the receiver class representing a
    "simplified image editor app
    "Here, start values for the image are supplied, which are
    "reflected in the current state that is retrieved.
    DATA(receiver) = NEW lcl_receiver( width = 800 height = 600 ).

    "For logging and display purposes, the current state of the
    "image is retrieved. The current state is retrieved and output
    "after each operation in the example.
    DATA(state) = receiver->get_current_state( ).
    out->write( state ).

    "Creating an object of the invoker class
    DATA(invoker) = NEW lcl_invoker( ).

    "Creating concrete commands by linking them to the receiver
    "In the example, a reference of the receiver class is passed as
    "argument of the instance constructor.

    "Resizing operation (1)
    "Here and in all other concrete command creations, values are
    "passed representing new values that should be applied for the
    "operation.

    DATA(cmd1) = NEW lcl_cmd_resize( receiver = receiver width = 1024 height = 768 ).

    "Passing the command object to the invoker that takes care of
    "executing the command. However, the invoker does not know about
    "the concrete details of the operation.
    invoker->exec_cmd( cmd1 ).

    state = receiver->get_current_state( ).
    out->write( state ).

    "Resizing operation (2)
    DATA(cmd2) = NEW lcl_cmd_resize( receiver = receiver width = 1920 height = 1080 ).
    invoker->exec_cmd( cmd2 ).

    state = receiver->get_current_state( ).
    out->write( state ).

    "Rotating operation (1)
    DATA(cmd3) = NEW lcl_cmd_rotate( receiver = receiver angle = 20 ).
    invoker->exec_cmd( cmd3 ).

    state = receiver->get_current_state( ).
    out->write( state ).

    "Rotating operation (2)
    DATA(cmd4) = NEW lcl_cmd_rotate( receiver = receiver angle = 45 ).
    invoker->exec_cmd( cmd4 ).

    state = receiver->get_current_state( ).
    out->write( state ).

    "Resizing operation (3)
    DATA(cmd5) = NEW lcl_cmd_resize( receiver = receiver width = 1280  height = 720  ).
    invoker->exec_cmd( cmd5 ).

    state = receiver->get_current_state( ).
    out->write( state ).

    "Rotating operation (3)
    DATA(cmd6) = NEW lcl_cmd_rotate( receiver = receiver angle = 180 ).
    invoker->exec_cmd( cmd6 ).

    state = receiver->get_current_state( ).
    out->write( state ).

*&---------------------------------------------------------------------*
*& Undo operations
*&---------------------------------------------------------------------*

    out->write( |\n{ repeat( val = `*` occ = 70 ) }\n| ).
    out->write( `********* Undo operations *********` ).

    DO 3 TIMES.
      invoker->undo( ).

      state = receiver->get_current_state( ).
      out->write( state ).
    ENDDO.

*&---------------------------------------------------------------------*
*& Redo operations
*&---------------------------------------------------------------------*

    out->write( |\n{ repeat( val = `*` occ = 70 ) }\n| ).
    out->write( `********* Redo operations *********` ).

    DO 5 TIMES.
      invoker->redo( ).

      state = receiver->get_current_state( ).
      out->write( state ).
    ENDDO.

*&---------------------------------------------------------------------*
*& Log
*&---------------------------------------------------------------------*

    out->write( |\n{ repeat( val = `*` occ = 70 ) }\n| ).
    out->write( `********* Log *********` ).
    out->write( log ).
  ENDMETHOD.
ENDCLASS.
