***********************************************************************
*
*        ABAP cheat sheet: Selection screens and classic lists
*        Example: PARAMETERS statements
*
* -------------------------- PURPOSE ----------------------------------
* Example that demonstrates PARAMETERS statements in standard selection
* screens
*
* Notes:
* - A selection of additions is used. For more additions and details,
*   see the ABAP Keyword Documentation.
* - The MODIF ID addition is possible for PARAMETERS. This is also true
*   for SELECTION-SCREEN statements. See the details in that example.
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

****************************** Type options ******************************

"Displaying some explanatory text on the standard selection screen
"The actual text content is assigned to the variable in the INITIALIZATION
"event block.
SELECTION-SCREEN COMMENT /1(70) t1.

"Referring to a built-in type with TYPE
"In this case, a variable length character string can be inserted in the
"generated input field. Note the LOWER CASE addition further down.
PARAMETERS p_str TYPE string.

"Referring to an existing data object with the LIKE addition.
"When referencing to a data type from the ABAP Dictionary after LIKE,
"you can benefit from things such as input help (if available).
DATA dobj TYPE i.
PARAMETERS plikedo LIKE dobj.

"Dynamic spefication of data object in parentheses after LIKE
"You can reference to a data type from the ABAP Dictionary. some_dobj in
"the example may be the name of a component of a database table,
"provided in capital letters.
DATA some_dobj TYPE c LENGTH 50 VALUE 'ZDEMO_ABAP_CARR-CARRID'.
PARAMETERS p_dyn LIKE (some_dobj).

"Length specifications
"Similar to declarations with DATA, the length can be specified for data
"types with generic length (i.e. the types c, n, p, and x).
PARAMETERS p_c1 TYPE c LENGTH 1.
PARAMETERS p_n5 TYPE n LENGTH 5.

"You may also see specifications in which the length is specified in parentheses.
"For better readability, specifying LENGTH explicitly is recommended.
PARAMETERS pbracket(2) TYPE c.
"No length specified means LENGTH 1 by default
PARAMETERS p_no_len TYPE c.
"No explicit type specification means TYPE c by default
PARAMETERS plenonl1(40).
PARAMETERS plenonl2 LENGTH 40.
"No explicit type and length specification means TYPE c LENGTH 1 by default
PARAMETERS pnothing.

****************************** Value options ******************************

SELECTION-SCREEN COMMENT /1(70) t2.

"DEFAULT: Defining a start value (can also be a data object instead of a literal)
PARAMETERS pdefault TYPE i DEFAULT 12345.

"LOWER CASE: Prevents the effect of capitalizing the entry made when the content
"is transported to the data object
PARAMETERS p_upper TYPE string DEFAULT `Hello World`. "Value you insert will be capitalized.
PARAMETERS p_lower TYPE string DEFAULT `Hello World` LOWER CASE.

"Note: There are more additions available, e.g. for linking the parameter to search help,
"or checking against fixed values defined in the domain of the data type, and so on.
"You can also perform your custom input checks in event blocks.

****************************** Screen options ******************************

SELECTION-SCREEN COMMENT /1(70) t3.

"OBLIGATORY: Declaring the input field as a required field
"If there is no entry, the program cannot proceed when choosing Execute.
"A message is displayed.
PARAMETERS p_oblig TYPE string OBLIGATORY.

"NO-DISPLAY: Hiding the input field on the selection screen
"A value can be supplied when calling the program with SUBMIT and the WITH addition.
"Note that with the NO-DISPLAY addition, the parameter can have any data types except
"for reference/enumerated types, unlike in the other additions which require flat
"types (except type string).
PARAMETERS p_nodisp TYPE string NO-DISPLAY.

"VISIBLE LENGTH: Defining the visible length of the field
PARAMETERS p_vislen TYPE c LENGTH 5 VISIBLE LENGTH 3.

"AS CHECKBOX: Displaying input fields as checkbox
"Type c and length 1 is expected, but the explicit length specification is not allowed.
"The checkbox is selected if the value has the value X or x.
PARAMETERS p_check1 AS CHECKBOX.        "Implicit type c
"Explicit type specification (but no explicit length), select by default
PARAMETERS p_check2 TYPE c AS CHECKBOX DEFAULT 'X'.

"RADIOBUTTON GROUP: Defining a radio button group for parameters
"Note:
"- Group name can have a maximum of four characters
"- Regarding the data type, the same applies as for AS CHECKBOX
"- Only one parameter can be defined with the DEFAULT addition
"- If DEFAULT is not specified, the first parameter of the group is set to the value X
"- You can also use chained statements
PARAMETERS: p_radio1 RADIOBUTTON GROUP rbgr,
            p_radio2 TYPE c RADIOBUTTON GROUP rbgr,       "Explicit type specification
            p_radio3 RADIOBUTTON GROUP rbgr DEFAULT 'X'.  "Set this radio button as selected

"AS LISTBOX VISIBLE LENGTH: Creating a dropdown list box
"You can use the function module VRM_SET_VALUES by passing a suitable list at the
"events AT SELECTION-SCREEN OUTPUT or AT SELECTION-SCREEN ON VALUE-REQUEST.
PARAMETERS plistbox TYPE i AS LISTBOX VISIBLE LENGTH 10 OBLIGATORY.

*********************** Assigning function codes ***********************

"The AS CHECKBOX, RADIOBUTTON GROUP, AS LISTBOX additions can be combined
"with the addition USER-COMMAND. That means, on selection, the event
"AT SELECTION-SCREEN is raised and you can evaluate the function code there.
"Note: To enable it, include the statement 'TABLES sscrfields.' in the code.
"When the button is clicked, the event AT SELECTION-SCREEN is raised and the
"function code ('cmd', 'rbcm' in the example) is passed to the 'ucomm' component
"in the interface work area 'sscrfields' which can be evaluated and reacted upon
"accordingly.
"Before exploring the effect in the example, make entries for the
"obligatory input fields first, then choose the checkbox/radio buttons.

SELECTION-SCREEN COMMENT /1(70) t4.

TABLES sscrfields.
PARAMETERS pchckcmd AS CHECKBOX USER-COMMAND cmd.
PARAMETERS: prad1cmd RADIOBUTTON GROUP grp USER-COMMAND rbcm,
            prad2cmd RADIOBUTTON GROUP grp.

**********************************************************************
INITIALIZATION.

  t1 = 'PARAMETERS: Demonstrating various type options:'.
  t2 = 'Demonstrating value options:'.
  t3 = 'Demonstrating screen options:'.
  t4 = 'Click these screen elements to explore the USER-COMMAND addition.'.

**********************************************************************

AT SELECTION-SCREEN.

  CASE sscrfields-ucomm.
    WHEN 'CMD'.
      MESSAGE |Hallo { sy-uname }.| TYPE 'I'.
    WHEN 'RBCM'.
      MESSAGE |Today's date: { sy-datum DATE = ISO }| TYPE 'I'.
  ENDCASE.

**********************************************************************

AT SELECTION-SCREEN OUTPUT.

  "This is relevant for the AS LISTBOX VISIBLE LENGTH addition.
  "Note:
  "- The function module VRM_SET_VALUES is used
  "- Column TEXT: What is displayed in the list box
  "- Colum KEY: When a line is selected, the KEY value is added to the
  "  parameter
  "In the example, the numbers 1 - 10 are added as values.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = CONV vrm_id( 'PLISTBOX' )
      values = VALUE vrm_values(
                 FOR i = 1 UNTIL i > 10
                 ( key = i text = |Value { i }| ) ).

**********************************************************************

START-OF-SELECTION.

  "Creating a classic list
  WRITE / `This list displays the values provided on the selection screen.`.
  SKIP.
  WRITE / |p_str: "{ p_str }"| .
  WRITE / |plikedo: "{ plikedo }"| .
  WRITE / |p_dyn: "{ p_dyn }"| .
  WRITE / |p_c1: "{ p_c1 }"| .
  WRITE / |p_n5: "{ p_n5 }"| .
  WRITE / |pbracket: "{ pbracket }"| .
  WRITE / |p_no_len: "{ p_no_len }"| .
  WRITE / |plenonl1: "{ plenonl1 }"| .
  WRITE / |plenonl2: "{ plenonl2 }"| .
  WRITE / |pnothing: "{ pnothing }"| .
  WRITE / |pdefault: "{ pdefault }"| .
  WRITE / |p_upper: "{ p_upper }"| .
  WRITE / |p_lower: "{ p_lower }"| .
  WRITE / |p_oblig: "{ p_oblig }"| .
  WRITE / |p_nodisp: "{ p_nodisp }"| .
  WRITE / |p_vislen: "{ p_vislen }"| .
  WRITE / |p_check1: "{ p_check1 }"| .
  WRITE / |p_check2: "{ p_check2 }"| .
  WRITE / |Radio button selected: "{ COND #( WHEN p_radio1 = 'X' THEN 'p_radio1' WHEN p_radio2 = 'X' THEN 'p_radio2' ELSE 'p_radio3' ) }"| .
  WRITE / |plistbox: "{ plistbox }"| .
  WRITE / |pchckcmd: "{ pchckcmd }"| .
  WRITE / |Radio button selected: "{ COND #( WHEN prad1cmd = 'X' THEN 'prad1cmd' ELSE 'prad2cmd' ) }"| .
