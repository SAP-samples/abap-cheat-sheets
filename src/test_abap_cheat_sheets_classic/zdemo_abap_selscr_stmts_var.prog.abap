***********************************************************************
*
*        ABAP cheat sheet: Selection screens and classic lists
*        Example: Variants of SELECTION-SCREEN statements
*
* -------------------------- PURPOSE ----------------------------------
* Example that demonstrates variants of the SELECTION-SCREEN statements
* to modify the layout, create additional screen elements etc.
* Includes:
* - Adding blank lines (SKIP)
* - Creating a horizontal line (ULINE)
* - Providing text (COMMENT)
* - Creating a pushbutton (PUSHBUTTON)
* - Specifying the output position of a screen element (POSITION)
* - Defining a new line with multiple elements (BEGIN OF LINE)
* - Creating blocks (BLOCK)
* - Creating tabbed blocks (TABBED BLOCK/TAB)
* - Assigning a screen element to a modification group (MODIF ID)
* - Adding pushbuttons in the application toolbar (FUNCTION KEY)
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

"The following declaration is required in the context of the
"TABBED BLOCK, FUNCTION KEY, and USER-COMMAND additions.
TABLES sscrfields.

"Creating blocks (BLOCK)
"The WITH FRAME additions draws a frame around a block.
"Note that the block must be ended with a ... END OF BLOCK ... statement.
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME.
  "Providing text (COMMENT)
  SELECTION-SCREEN COMMENT /30(70) t1.
  "Assigning a screen element to a modification group (MODIF ID)
  "An identifier can be specified to assign a screen element to a
  "modification group. This identifier is assigned to the component
  "'group1' of the SCREEN structure. Using MODIFY SCREEN statements,
  "the elements can be modified before displaying
  "in the AT SELECTION-SCREEN OUTPUT event block.
  "Note: The additions is possible for PARAMETERS, SELECT-OPTIONS
  "and SELECTION-SCREEN.
  PARAMETERS my_name LIKE sy-uname MODIF ID nd.

  "Adding blank lines (SKIP)
  "Just SKIP, no further addition means it skips one line.
  SELECTION-SCREEN SKIP.
  PARAMETERS sap_rel LIKE sy-saprl MODIF ID nd.

  "Specifying the number of blank lines
  SELECTION-SCREEN SKIP 2.
  "Creating a horizontal line (ULINE)
  SELECTION-SCREEN ULINE /10(40).
  PARAMETERS            random_i TYPE i MODIF ID nd.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN ULINE.
SELECTION-SCREEN SKIP.

"Defining a new line with multiple elements (BEGIN OF LINE)
"Chained statements are handy.
SELECTION-SCREEN: BEGIN OF LINE,
"Specifying the output position of a screen element (POSITION)
"Only possible within BEGIN/END OF LINE
POSITION 2,
PUSHBUTTON (5) btn1 USER-COMMAND btn_1,
POSITION 9,
COMMENT (25) t2,
POSITION 35,
PUSHBUTTON (5) btn2 USER-COMMAND btn_2,
POSITION 42,
COMMENT (25) t3,
END OF LINE.
SELECTION-SCREEN SKIP.

"Radio buttons in a block with a frame and title
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE title1.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: rb1 RADIOBUTTON GROUP grp.
    SELECTION-SCREEN COMMENT (20) text1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: rb2 RADIOBUTTON GROUP grp.
    SELECTION-SCREEN COMMENT (20) text2.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block2.

SELECTION-SCREEN SKIP.

"Further statements with the MODIF ID addition

"The layout of the following screen element should be modified
"using the specified ID.
"Note: Outside of BEGIN/END OF LINE statements, the specified
"format must contain one position (/...)
SELECTION-SCREEN COMMENT /10(70) t4 MODIF ID mod.

SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME.
  "The layout of the following screen elements should be modified
  "using the specified ID.
  PARAMETERS: pa1 RADIOBUTTON GROUP gr MODIF ID mbl,
              pa2 RADIOBUTTON GROUP gr MODIF ID mbl,
              pa3 RADIOBUTTON GROUP gr,
              pa4 RADIOBUTTON GROUP gr.
SELECTION-SCREEN END OF BLOCK bl.

"Creating tabbed blocks (TABBED BLOCK/TAB)

"Creating standalone selection screens as subscreen dynpros
"They can be included in other dynpros or selection screens, or in
"subscreen areas or tab pages. This example covers the latter.
SELECTION-SCREEN BEGIN OF SCREEN 9990 AS SUBSCREEN.
  PARAMETERS: par_sub1 TYPE c LENGTH 10 LOWER CASE.
SELECTION-SCREEN END OF SCREEN 9990.

SELECTION-SCREEN BEGIN OF SCREEN 9991 AS SUBSCREEN.
  PARAMETERS: par_sub2 TYPE c LENGTH 10 LOWER CASE.
SELECTION-SCREEN END OF SCREEN 9991.

SELECTION-SCREEN: BEGIN OF TABBED BLOCK tabs FOR 2 LINES,
"The USER-COMMAND addition specifies a function code.
"If a tab is selected by users, the function code can
"be evaluated using the the component ucomm of the structure
"sscrfields after the AT SELECTION-SCREEN event.
TAB (20) tabtitl1 USER-COMMAND tab1,
TAB (20) tabtitl2 USER-COMMAND tab2,
END OF BLOCK tabs.

"Adding pushbuttons in the application toolbar (FUNCTION KEY)
"There are five inactive pushbuttons to which the function codes FC01, FC02
"up to FC05 are assigned. The ... FUNCTION KEY ... statement activates the
"pushbuttons for the specified codes.
"Note: To enable it, include the statement 'TABLES sscrfields.' in the code.
"When the button is clicked, the event AT SELECTION-SCREEN is raised and the
"function code is passed to the 'ucomm' component in the interface
"work area 'sscrfields' which can be evaluated and reacted upon accordingly,
"e.g. in a CASE control structure.
SELECTION-SCREEN: FUNCTION KEY 1,  "Stands for FC01
FUNCTION KEY 2,  "FC02
FUNCTION KEY 3,  "FC03
FUNCTION KEY 4.  "FC04

**********************************************************************

INITIALIZATION.
  title1 = 'Block including radio buttons + comments put in one line using BEGIN/END OF LINE'.
  t1 = 'This is a selection screen comment included in a block with frame.'.
  t2 = '<- Click to get the time'.
  t3 = '<- Click to get the date'.
  t4 = 'This text and the following radio button group was modified.'.
  text1 = 'Radio button A'.
  text2 = 'Radio button B'.
  btn1 = 'A'.
  btn2 = 'B'.
  "Relevant to the tabbed blocks
  tabtitl1 = 'Selection Screen A'.
  tabtitl2 = 'Selection Screen B'.
  tabs-prog = sy-repid.
  tabs-dynnr = 9990.
  tabs-activetab = 'TAB1'.
  "Relevant to the function keys
  "To provide text for the buttons, assign values to the component functxt_0n
  "(while n stands for the numbers 1 - 5). Otherwise, there won't be any button
  "text.
  sscrfields-functxt_01 = 'Get my name'.
  sscrfields-functxt_02 = 'Get ABAP release'.
  sscrfields-functxt_03 = 'Get random integer'.
  sscrfields-functxt_04 = 'Clear input fields'.

**********************************************************************

AT SELECTION-SCREEN OUTPUT.

  "Modifying the layout of screen elements
  LOOP AT SCREEN INTO DATA(wa).
    IF wa-group1 = 'MOD'.
      "Intensify the text output
      wa-intensified = '1'.
      MODIFY SCREEN FROM wa.
    ENDIF.

    IF wa-group1 = 'MBL'.
      "Make the screen element invisible
      wa-invisible = '1'.
      MODIFY SCREEN FROM wa.
    ENDIF.

    IF wa-group1 = 'ND'.
      "Disable the input for a field
      wa-input = '0'.
      MODIFY SCREEN FROM wa.
    ENDIF.
  ENDLOOP.

**********************************************************************

AT SELECTION-SCREEN.

  "Evaluating the function codes relevant to the
  "TABBED BLOCK, FUNCTION KEY, and USER-COMMAND additions.

  "Evaluating only if sy-dynnr has the value of the standard selection
  "screen
  CHECK sy-dynnr = 1000.

  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      my_name = sy-uname.
    WHEN 'FC02'.
      sap_rel = sy-saprl.
    WHEN 'FC03'.
      random_i = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = 100 )->get_next( ).
    WHEN 'FC04'.
      CLEAR: my_name, sap_rel, random_i, par_sub1, par_sub2.
    WHEN 'BTN_1'.
      MESSAGE |You clicked button A. The time is { sy-uzeit TIME = ISO }.| TYPE 'I'.
    WHEN 'BTN_2'.
      MESSAGE |You clicked button B. The date is { sy-datum DATE = ISO }.| TYPE 'I'.
    WHEN 'TAB1'.
      tabs-dynnr = 9990.
    WHEN 'TAB2'.
      tabs-dynnr = 9991.
  ENDCASE.

**********************************************************************

START-OF-SELECTION.

  "Creating a classic list
  WRITE / `This list displays the values provided on the selection screen.`.
  SKIP.
  WRITE / |my_name: "{ my_name }"|.
  WRITE / |sap_rel: "{ sap_rel }"|.
  WRITE / |random_i: "{ random_i }"|.
  WRITE / |Radio button selected (1): "{ COND #( WHEN rb1 = 'X' THEN 'rb1' ELSE 'rb2' ) }"| .
  WRITE / |Radio button selected (2): "{ COND #( WHEN pa1 = 'X' THEN 'pa1' WHEN pa2 = 'X' THEN 'pa2' WHEN pa3 = 'X' THEN 'pa3' ELSE 'pa4' ) }"| .
  WRITE / |par_sub1: "{ par_sub1 }"|.
  WRITE / |par_sub2: "{ par_sub2 }"|.
