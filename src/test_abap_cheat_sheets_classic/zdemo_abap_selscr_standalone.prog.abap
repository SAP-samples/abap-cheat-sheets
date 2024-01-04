***********************************************************************
*
*        ABAP cheat sheet: Selection screens and classic lists
*        Example: Creating standalone selection screens
*
* -------------------------- PURPOSE ----------------------------------
* - Example that demonstrates standalone selection screens
* - In the example, calculations are performed based on user input.
* Includes:
* - Creating selection screens as regular dynpros with
*   SELECTION-SCREEN BEGIN/END OF SCREEN ... statements
* - Creating selection screens as subscreen dynpros with the AS SUBSCREEN
*   addition
* - Calling standalone selection screen with CALL SELECTION-SCREEN
*   statements
* - Using variants of the SELECTION-SCREEN statements that do not create
*   selection screens, but are used to modify the layout
* - INCLUDE addition for reusing already created elements
*
* Notes:
* - A selection of additions is used. For more additions and details,
*   see the ABAP Keyword Documentation.
* - See the ABAP cheat sheet for other statements used here, for example,
*   the ones related to event blocks such as START-OF-SELECTION.
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

PROGRAM.

"Creating standalone selection screens as regular dynpros
"The dynpro number must be unique in the program. Do not use 1000.
SELECTION-SCREEN BEGIN OF SCREEN 9810.
  "Here go all PARAMETERS, SELECT-OPTIONS, and SELECTION-SCREEN statements
  "to define the screen elements of the standalone selection screen.
  SELECTION-SCREEN COMMENT /1(70) t1.
  PARAMETERS number1 TYPE i OBLIGATORY.
SELECTION-SCREEN END OF SCREEN 9810.

"AS WINDOW/TITLE: The following selection screen is displayed in a modal
"dialog box. Plus, a title is specified.
SELECTION-SCREEN BEGIN OF SCREEN 9820 TITLE title1 AS WINDOW.
  SELECTION-SCREEN COMMENT /1(70) t2.
  PARAMETERS number2 TYPE i OBLIGATORY.
SELECTION-SCREEN END OF SCREEN 9820.

"Creating a standalone selection screens as subscreen dynpro
"They can be included in other dynpros or selection screens, or in
"subscreen areas or tab pages. The following example covers the latter.
"Note: The selection screens as subscreen dynpros cannot be called
"explicitly.

"The following two selection screens are included in tab pages
"further down.
SELECTION-SCREEN BEGIN OF SCREEN 9830 AS SUBSCREEN.
  PARAMETERS: number3 TYPE i OBLIGATORY,
              number4 TYPE i OBLIGATORY.
SELECTION-SCREEN END OF SCREEN 9830.

SELECTION-SCREEN BEGIN OF SCREEN 9840 AS SUBSCREEN.
  PARAMETERS: plus     AS CHECKBOX DEFAULT 'X',
              minus    AS CHECKBOX DEFAULT 'X',
              multiply AS CHECKBOX DEFAULT 'X',
              divide   AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF SCREEN 9840.

"The following selection screen (as regular dynpro) is created
"as dummy, i.e. it is not included as the ones above. It is
"used to demonstrate the INCLUDE addition.
SELECTION-SCREEN BEGIN OF SCREEN 9850.
  SELECTION-SCREEN COMMENT /1(50) t3.
SELECTION-SCREEN END OF SCREEN 9850.

"The following declaration is required in the context of the
"TABBED BLOCK addition.
TABLES sscrfields.

"The following selection screen (as regular dynpro) is created to
"demonstrate ...
"- the use of subscreen dynpros. In this case, the subscreen dynpros
"  are used in the context of tab pages (TABBED BLOCK, TAB additions).
"- the reuse of available screen elements in the program using the
"  INCLUDE addition.
SELECTION-SCREEN BEGIN OF SCREEN 9860 TITLE title2.
  SELECTION-SCREEN INCLUDE COMMENT /1(70) t3.  "Reusing a screen element
  SELECTION-SCREEN: BEGIN OF TABBED BLOCK tabs FOR 10 LINES,
  "In such a TABBED BLOCK statement, only TAB statements are allowed
  "to include subscreen dynpros.
  "The USER-COMMAND addition specifies a function code. If a tab is
  "selected by users, the function code can be evaluated using the the
  "component ucomm of the structure sscrfields after the
  "AT SELECTION-SCREEN event.
  TAB (20) btn_nums USER-COMMAND uc_num,
  TAB (20) btn_op USER-COMMAND uc_op,
  END OF BLOCK tabs.
SELECTION-SCREEN END OF SCREEN 9860.

**********************************************************************

INITIALIZATION.
  title1 = 'First calculation'.
  title2 = 'Second calculation'.
  t1 = 'Insert an integer value for the first calculation.'.
  t2 = 'Provide the second number for the first calculation.'.
  t3 = 'Make entries in the tabs for the second calculation.'.
  btn_nums = 'Numbers'.
  btn_op = 'Operators'.

  "Instead of the following assignements, you can also use the DEFAULT,
  "SCREEN, PROGRAM additions of the ... TAB ... statement
  tabs-prog = sy-repid.
  tabs-dynnr = 9830.
  tabs-activetab = 'UC_NUM'.

**********************************************************************

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 9860.
      CASE sscrfields-ucomm.
        WHEN 'UC_NUM'.
          tabs-dynnr = 9830.
        WHEN 'UC_OP'.
          tabs-dynnr = 9840.
      ENDCASE.
  ENDCASE.

**********************************************************************

START-OF-SELECTION.

  "The following statements call the selection screens.
  CALL SELECTION-SCREEN 9810.

  IF sy-subrc = 0.
    "STARTING AT: Specifies that the selection screen is displayed as a
    "modal dialog box.
    CALL SELECTION-SCREEN 9820 STARTING AT 10 10.
  ENDIF.

  IF sy-subrc = 0.
    CALL SELECTION-SCREEN 9860.
  ENDIF.

  IF sy-subrc = 0.
    "Creating a classic list
    WRITE / |Results of the first calculation using the provided numbers { number1 } and { number2 } (all basic arithmetic operations):| COLOR COL_POSITIVE.
    WRITE / |{ number1 } + { number2 } = { number1 + number2 }|.
    WRITE / |{ number1 } - { number2 } = { number1 - number2 }|.

    TRY.
        WRITE / |{ number1 } * { number2 } = { number1 * number2 }|.
      CATCH cx_sy_arithmetic_error INTO DATA(error).
        WRITE / |{ number1 } * { number2 } = ??? Error in multiplication: { error->get_text( ) }| COLOR COL_NEGATIVE.
    ENDTRY.

    TRY.
        WRITE / |{ number1 } / { number2 } = { CONV decfloat34( number1 / number2 ) DECIMALS = 3 }|.
        IF number1 = 0 AND number2 = 0.
          WRITE / `No zero division error? Note that ABAP "allows" zero division if the first number is also 0 :)` COLOR COL_NEGATIVE.
        ENDIF.
      CATCH cx_sy_zerodivide INTO error.
        WRITE / |{ number1 } / { number2 } = ??? Error in division: { error->get_text( ) }| COLOR COL_NEGATIVE.
    ENDTRY.

    SKIP 2.
    WRITE / |Results of the second calculation using the provided numbers { number3 } and { number4 } (selected basic arithmetic operations):| COLOR COL_POSITIVE.

    IF plus IS INITIAL
    AND minus IS INITIAL
    AND multiply IS INITIAL
    AND divide IS INITIAL.
      WRITE / 'You did not select any operators from the tab. No calculation was performed.' COLOR COL_NEGATIVE.
    ELSE.
      IF plus = 'X'.
        WRITE / |{ number3 } + { number4 } = { number3 + number4 }|.
      ENDIF.

      IF minus = 'X'.
        WRITE / |{ number3 } - { number4 } = { number3 - number4 }|.
      ENDIF.

      IF multiply = 'X'.
        TRY.
            WRITE / |{ number3 } * { number4 } = { number3 * number4 }|.
          CATCH cx_sy_arithmetic_error INTO error.
            WRITE / |{ number3 } * { number4 } = ??? Error in multiplication: { error->get_text( ) }| COLOR COL_NEGATIVE.
        ENDTRY.
      ENDIF.

      IF divide = 'X'.
        TRY.
            WRITE / |{ number3 } / { number4 } = { CONV decfloat34( number3 / number4 ) DECIMALS = 3 }|.
            IF number3 = 0 AND number4 = 0.
              WRITE / `No zero division error? Note that ABAP "allows" zero division if the first number is also 0 :)` COLOR COL_NEGATIVE.
            ENDIF.
          CATCH cx_sy_zerodivide INTO error.
            WRITE / |{ number3 } / { number4 } = ??? Error in division: { error->get_text( ) }| COLOR COL_NEGATIVE.
        ENDTRY.
      ENDIF.
    ENDIF.
  ENDIF.
