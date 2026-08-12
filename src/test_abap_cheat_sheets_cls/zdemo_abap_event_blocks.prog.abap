***********************************************************************
*
*        ABAP cheat sheet: Selection screens and classic lists
*        Example: Event blocks
*
* -------------------------- PURPOSE ----------------------------------
* - Example that demonstrates event blocks
* - The main purpose of the example to visualize the calling of the
*   events. Internal tables are filled during the events to log the
*   event name and the time stamp when it is called. The tables
*   are then output. It is implemented in such a way that it consists
*   only of a basic list and a details list. Note the selection screen
*   comments displayed.
* Includes:
* - Program constructor: LOAD-OF-PROGRAM
* - Reporting events: INITIALIZATION, START-OF-SELECTION
* - Selection screen events: AT SELECTION-SCREEN ...
* - List events: AT LINE-SELECTION, AT USER-COMMAND, TOP-OF-PAGE,
*                END-OF-PAGE
*
* Notes:
* A selection of additions is used. For more additions and details,
* see the ABAP Keyword Documentation.
*
* ----------------------- GETTING STARTED -----------------------------
* - Open the program with the ABAP development tools for Eclipse (ADT).
* - Choose F8 to run the program.
*
* ----------------------------- NOTE -----------------------------------
* The code presented in this class is intended only to support the ABAP
* cheat sheets. It is not intended for direct use in a production system
* environment. The code examples in the ABAP cheat sheets are primarily
* intended to provide a better explanation and visualization of the
* syntax and semantics of ABAP statements, not to solve concrete
* programming tasks. For production application programs, you should
* always work out your own solution for each individual case. There is
* no guarantee for the correctness or completeness of the code.
* Furthermore, there is no legal responsibility or liability for any
* errors or their consequences that may occur when using the the example
* code.
*
***********************************************************************

"LINE-COUNT: Specifies the page length for the basic list
"Number in parentheses: Lines that are reserved for the page footer,
"which can be specified in the END-OF-PAGE event block.
PROGRAM LINE-COUNT 20(3).

TYPES: BEGIN OF st,
         id          TYPE c LENGTH 3,
         event_block TYPE c LENGTH 50,
         time        TYPE utclong,
       END OF st.
DATA: counter_line             TYPE i,
      reporting_and_selscr_log TYPE TABLE OF st WITH EMPTY KEY,
      list_log                 TYPE TABLE OF st WITH EMPTY KEY,
      counter                  TYPE i,
      tstmp                    TYPE utclong.

SELECTION-SCREEN COMMENT /1(70) intro.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK blk.
  PARAMETERS: number1 TYPE string DEFAULT `1` VISIBLE LENGTH 5 .
  PARAMETERS: number2 TYPE string DEFAULT `2` VISIBLE LENGTH 5 .
SELECTION-SCREEN END OF BLOCK blk.

PARAMETERS: plus     RADIOBUTTON GROUP grp DEFAULT 'X',
            minus    RADIOBUTTON GROUP grp,
            multiply RADIOBUTTON GROUP grp,
            divide   RADIOBUTTON GROUP grp.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN COMMENT /1(70) note.

"Local class with methods used in the example in various places
CLASS demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      sel_screen_evt IMPORTING evt TYPE string,
      write_line IMPORTING evt TYPE string
                           utc TYPE utclong,
      counter,
      list_evt IMPORTING evt TYPE string,
      write_rep_sel_log,
      write_list_log.
    CLASS-DATA: li_wa  LIKE LINE OF list_log,
                sel_wa LIKE LINE OF reporting_and_selscr_log.
ENDCLASS.

CLASS demo IMPLEMENTATION.
  METHOD counter.
    IF counter = 0.
      counter = 1.
    ELSE.
      counter += 1.
    ENDIF.
  ENDMETHOD.

  METHOD sel_screen_evt.
    counter( ).
    APPEND VALUE #( id = counter event_block  = evt time = utclong_current( ) ) TO reporting_and_selscr_log.
  ENDMETHOD.

  METHOD list_evt.
    counter( ).
    APPEND VALUE #( id = counter event_block  = evt time = utclong_current( ) ) TO list_log.
  ENDMETHOD.

  METHOD write_line.
    counter( ).
    WRITE: /3(*) counter,
            15  evt,
            60  utc.
  ENDMETHOD.

  METHOD write_rep_sel_log.
    SKIP.
    WRITE / '********** Log entries for reporting and selection screen events **********' COLOR 2.

    FORMAT COLOR 2.
    WRITE: /3 'ID', 15 'Event Block', 60 'Time' .
    FORMAT COLOR OFF.
    LOOP AT reporting_and_selscr_log INTO sel_wa.
      WRITE: / sel_wa-id UNDER 'ID',
              sel_wa-event_block UNDER 'Event Block',
              sel_wa-time UNDER 'Time'.
    ENDLOOP.
  ENDMETHOD.

  METHOD write_list_log.
    WRITE / '********** Log entries for list events **********' COLOR 2.

    FORMAT COLOR 2.
    WRITE: /3 'ID', 15 'Event Block', 60 'Time' .
    FORMAT COLOR OFF.
    LOOP AT list_log INTO li_wa.
      WRITE: / li_wa-id UNDER 'ID',
              li_wa-event_block UNDER 'Event Block',
              li_wa-time UNDER 'Time'.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

LOAD-OF-PROGRAM.

  "Called when a program is loaded into the internal session
  "When you call an executable programs with SUBMIT, it is recommended that
  "you use the INITIALIZATION event for initializing data objects, since the
  "initial values for parameters and selection criteria are set after LOAD-OF-PROGRAM.

  demo=>sel_screen_evt( `LOAD-OF-PROGRAM` ).

**********************************************************************

INITIALIZATION.

  "Called immediately after LOAD-OF-PROGRAM and before the selection
  "screen processing of an existing standard selection screen.
  "You can initialize the input fields of the selection screen here.

  demo=>sel_screen_evt( `INITIALIZATION` ).
  intro = 'Enter two integers and select a radio button for a calculation.'.
  note = 'The Back/Exit/Cancel buttons raise the ON EXIT-COMMAND event.'.

**********************************************************************

AT SELECTION-SCREEN OUTPUT.
  "Called by the dynpro event PBO of a selection screen
  "Can be used for screen modifications.

  demo=>sel_screen_evt( `AT SELECTION-SCREEN OUTPUT` ).

  LOOP AT SCREEN INTO DATA(wa).
    IF wa-name = 'INTRO'.
      wa-intensified = '1'.
      MODIFY SCREEN FROM wa.
    ENDIF.

  ENDLOOP.

**********************************************************************

AT SELECTION-SCREEN ON RADIOBUTTON GROUP grp.

  "Called when the values for a radio button group was passed

  demo=>sel_screen_evt( `AT SELECTION-SCREEN ON RADIOBUTTON GROUP` ).

  CASE 'X'.
    WHEN plus.
      MESSAGE 'Event AT SELECTION-SCREEN ON RADIOBUTTON GROUP is called. The operator will be +' TYPE 'I'.
    WHEN minus.
      MESSAGE 'Event AT SELECTION-SCREEN ON RADIOBUTTON GROUP is called. The operator will be -' TYPE 'I'.
    WHEN multiply.
      MESSAGE 'Event AT SELECTION-SCREEN ON RADIOBUTTON GROUP is called. The operator will be *' TYPE 'I'.
    WHEN divide.
      MESSAGE 'Event AT SELECTION-SCREEN ON RADIOBUTTON GROUP is called. The operator will be /' TYPE 'I'.
  ENDCASE.

**********************************************************************

AT SELECTION-SCREEN ON BLOCK blk.

  "Called when all input for a block is passed to the program

  demo=>sel_screen_evt( `AT SELECTION-SCREEN ON BLOCK blk` ).

  CONDENSE number1 NO-GAPS.
  CONDENSE number2 NO-GAPS.
  FIND PCRE `\D` IN number1.
  DATA(subrc) = sy-subrc.
  FIND PCRE `\D` IN number2.
  IF subrc = 0
  OR sy-subrc = 0.
    MESSAGE 'Event AT SELECTION-SCREEN ON BLOCK is called. Provide correct integer values.' TYPE 'E'.
  ENDIF.

**********************************************************************

AT SELECTION-SCREEN ON number1.

  "Called when values for a parameter (which is the case here) or selection criteria were passed to the program

  demo=>sel_screen_evt( `AT SELECTION-SCREEN ON number1.` ).

  IF number1 IS INITIAL.
    MESSAGE 'Event AT SELECTION-SCREEN ON is called. Please make an entry for number1' TYPE 'E'.
  ENDIF.

**********************************************************************

AT SELECTION-SCREEN ON number2.

  "Called when values for a parameter (which is the case here) or selection criteria were passed to the program

  demo=>sel_screen_evt( `AT SELECTION-SCREEN ON number2.` ).

  IF number2 IS INITIAL.
    MESSAGE 'Please make an entry for number2' TYPE 'E'.
  ENDIF.

**********************************************************************

AT SELECTION-SCREEN ON EXIT-COMMAND.

  "Called in case of Back, Exit, or Cancel

  MESSAGE 'Event AT SELECTION-SCREEN ON EXIT-COMMAND is called. See you.' TYPE 'I'.

**********************************************************************

AT SELECTION-SCREEN.

  "Called as last event in the selection screen processing when all
  "input values are passed to the program

  demo=>sel_screen_evt( `AT SELECTION-SCREEN` ).

  IF number2 = 0 AND
  divide = 'X'.
    MESSAGE 'Event AT SELECTION-SCREEN is called. Zero division is not possible.' TYPE 'E'.
  ENDIF.

**********************************************************************

START-OF-SELECTION.

  "Called during the processing of an executable program after selection
  "screen processing

  "SET USER-COMMAND: The following statement is included for demonstration
  "purposes. It programmatically raises a list event with a specified function
  "code.
  "Such a statement can be used when creating a list. After the list has been
  "created, but before the current list is displayed, the runtime
  "framework responds as if a user action had been performed in the
  "displayed list with the specified function code.
  SET USER-COMMAND 'START'.

  demo=>sel_screen_evt( `START-OF-SELECTION` ).

  WRITE / 'This is the basic list showing the content of a log table (mostly reporting and selection screen events). Double-click a line in this list to display more events in a details list.' COLOR 4.
  SKIP.
  WRITE / '************************** Calculation result **************************' COLOR 2.

  IF plus = 'X'.
    WRITE / |{ number1 } + { number2 } = { number1 + number2 }|.
  ENDIF.

  IF minus = 'X'.
    WRITE / |{ number1 } - { number2 } = { number1 - number2 }|.
  ENDIF.

  IF multiply = 'X'.
    TRY.
        WRITE / |{ number1 } * { number2 } = { number1 * number2 }|.
      CATCH cx_sy_arithmetic_error INTO DATA(error).
        WRITE / |{ number1 } * { number2 } = ??? Error in multiplication: { error->get_text( ) }| COLOR COL_NEGATIVE.
    ENDTRY.
  ENDIF.

  IF divide = 'X'.
    TRY.
        WRITE / |{ number1 } / { number2 } = { CONV decfloat34( number1 / number2 ) DECIMALS = 3 }|.
        IF number1 = 0 AND number2 = 0.
          WRITE / `No zero division error? Note that ABAP "allows" zero division if the first number is also 0 :)` COLOR COL_NEGATIVE.
        ENDIF.
      CATCH cx_sy_zerodivide INTO error.
        WRITE / |{ number1 } / { number2 } = ??? Error in division: { error->get_text( ) }| COLOR COL_NEGATIVE.
    ENDTRY.
  ENDIF.

  IF reporting_and_selscr_log IS NOT INITIAL.
    demo=>write_rep_sel_log( ).
  ENDIF.

**********************************************************************

AT LINE-SELECTION.

  "Called when
  "- a screen list is displayed and
  "- the screen cursor is on a list line and
  "- a function is selected using the function code PICK

  demo=>list_evt( evt = `AT LINE-SELECTION` ).

  "SET USER-COMMAND: The following statement is included for demonstration
  "purposes. It programmatically raises a list event with a specified function
  "code.
  SET USER-COMMAND 'LINE'.

  IF sy-lsind <= 1.
    WRITE / 'This is a details list. It shows the content of another log table (mostly list events). Clicking in this list does nothing meaningful.' COLOR 4.
    WRITE / 'You may want to go back to the basic list, double-click a line there to come back here, and see more log entries added for list events.' COLOR 4.
    WRITE / 'When the counter reaches 40, the program is restarted :)' COLOR 4.
    SKIP.
    WRITE / `************** Some sy Components **************` COLOR 2.
    WRITE / |Content of clicked line (sy-lisel): "{ sy-lisel }"|.
    WRITE / |Number of clicked line (sy-lilli): "{ sy-lilli }"|.
    WRITE / |List level of the current list (sy-lsind): "{ sy-lsind }"|.
    SKIP.
    IF list_log IS NOT INITIAL.
      demo=>write_list_log( ).
    ENDIF.
  ENDIF.

**********************************************************************

AT USER-COMMAND.

  "Called when a function with a user-defined function code is selected when a screen list is displayed
  "Note: PICK does not raised the AT USER-COMMAND event. Instead, it raises the AT LINE-SELECTION event.

  CASE sy-ucomm.
    WHEN 'START'.
      demo=>list_evt( `AT USER-COMMAND (Function code: START)` ).
      MESSAGE  |AT USER-COMMAND event raised (Function code: START). Counter value: { counter }.| TYPE 'I'.
    WHEN 'LINE'.
      demo=>list_evt( `AT USER-COMMAND (Function code: LINE)` ).
      MESSAGE  |AT USER-COMMAND event raised (Function code: LINE). Counter value: { counter }.| TYPE 'I'.
      IF counter >= 40.
        MESSAGE  |The counter has reached the threshold. The program will be started again using a SUBMIT statement.| TYPE 'I'.
        SUBMIT (sy-repid) VIA SELECTION-SCREEN.
      ENDIF.
  ENDCASE.

**********************************************************************

TOP-OF-PAGE.

  "Called when a basic list is created and when a new page begins

  tstmp = utclong_current( ).
  demo=>counter( ).
  APPEND VALUE #( id = counter event_block  = |TOP-OF-PAGE| time = tstmp ) TO list_log.
  WRITE: / |********************* Title inserted at TOP-OF-PAGE event ({ counter }) *********************|.

**********************************************************************

TOP-OF-PAGE DURING LINE-SELECTION.

  "Called when details lists are created

  tstmp = utclong_current( ).
  demo=>counter( ).
  APPEND VALUE #( id = counter event_block  = |TOP-OF-PAGE DURING LINE-SELECTION| time = tstmp ) TO list_log.
  WRITE: / |********************* Title inserted at TOP-OF-PAGE DURING LINE-SELECTION event ({ counter }) *********************|.

**********************************************************************

END-OF-PAGE.

  "Called when the end of a page is reached, for example, if the specified number of lines in the
  "LINE-COUNT addition has been reached

  tstmp = utclong_current( ).
  demo=>counter( ).
  APPEND VALUE #( id = counter event_block  = |END-OF-PAGE| time = tstmp ) TO list_log.
  WRITE: / |********************* Footer inserted at END-OF-PAGE event ({ counter }) *********************|.
