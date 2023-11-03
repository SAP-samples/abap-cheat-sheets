***********************************************************************
*
*        ABAP cheat sheet: Selection screens and classic lists
*        Example: Classic lists
*
* -------------------------- PURPOSE ----------------------------------
* Example that demonstrates various ABAP statements in the context of
* classic lists
* Includes:
* - Creating lists: WRITE, FORMAT, ULINE, SET BLANK LINES, SKIP,
*                   NEW-LINE, HIDE, HOTSPOT, RESERVE/BACK
* - Reading and modifying in lists: READ LINE, MODIFY LINE
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

TYPES: BEGIN OF str,
         statement    TYPE string,
         line_content TYPE string,
       END OF str.

DATA: count        TYPE i,
      line         LIKE sy-linno,
      linno1       LIKE sy-linno,
      linno2       LIKE sy-linno,
      linno_int    LIKE sy-linno,
      linno_copy   LIKE sy-linno,
      linno_mod1   LIKE sy-linno,
      linno_mod2   LIKE sy-linno,
      linno_input  LIKE sy-linno,
      lisel_copy   LIKE sy-lisel,
      square       TYPE i,
      cube         TYPE i,
      int_for_back LIKE sy-colno,
      itab         TYPE string_table,
      str_tab      TYPE string_table,
      lisel_tab    TYPE string_table,
      str          TYPE string,
      input_int    TYPE c LENGTH 3 VALUE 3,
      read_line    TYPE c LENGTH 1,
      num          TYPE i,
      char         TYPE c LENGTH 10,
      tab          TYPE TABLE OF str WITH EMPTY KEY,
      var          LIKE input_int,
      int          TYPE i,
      li           LIKE LINE OF tab,
      chck1        TYPE c LENGTH 1 VALUE 'X',
      chck2        TYPE c LENGTH 1 VALUE ' ',
      input        TYPE c LENGTH 20 VALUE 'Make an entry here',
      dc34         TYPE decfloat34 VALUE '   1.2345    ',
      txt          TYPE string VALUE `abap`,
      some_text    TYPE string VALUE `Hi, `,
      abc          TYPE c LENGTH 5 VALUE 'abcde',
      dc           TYPE decfloat34 VALUE '1.2345'.

**********************************************************************

INITIALIZATION.

  itab = VALUE #( ( `a` ) ( `b` ) ( `` ) ( `` )  ( `c` ) ).

**********************************************************************

START-OF-SELECTION.

  "WRITE using an unnamed data object
  WRITE 'Hello ABAP.'.
  "Named data objects
  "Note: The text is written right after the previous text in one line.
  WRITE some_text && sy-uname.

  WRITE / '****************** WRITE: Positioning output ******************'.
  "/: Writes to a new line
  WRITE / 'new line'.
  "Specifying the output position (and output in new line)
  WRITE /5 'positioned'.
  "Specifying the length, no position specified means the output is
  "written from the first column on
  WRITE /(3) 'not displayed completely'.
  "Specifying both length and position
  WRITE /5(30) 'this is displayed completely'.
  "Position/length only specified as numeric literals as above, then the
  "addition AT can be omitted
  WRITE AT /3(12) 'lorem ipsum'.
  "Length specifications with * or **. In that case, the output length
  "depends on the data type of the data object. There are special rules.
  "See the ABAP Kwyword Documentation for the details.
  WRITE /(*) dc34.
  WRITE /1(**) dc34.

  WRITE / '****************** WRITE: Further specification options ******************'.
  "There are many further specification options, among them function calls, string
  "expressions, method calls to be specified after WRITE.

  "String template
  WRITE / |{ txt WIDTH = 20 ALIGN = RIGHT  CASE = UPPER }|.
  "The following statement uses a chained statement with colon.
  WRITE: / |{ 1 + 2 }|, "String template includes an arithmetic calculation
         / to_upper( txt ). "Function call
  "Concatenation using &&
  WRITE / `conc` && `atenated`.
  "Method call (returns a random integer between 1 and 10)
  "Note the * specification. The returned value is of type i. Specifying *
  "for the length means the length required to output the current value
  "is used. You may want to try the following code snippet by removing (*).
  WRITE /(*) cl_abap_random_int=>create(
               seed = cl_abap_random=>seed( )
               min  = 1
               max  = 10 )->get_next( ).

  WRITE / '****************** WRITE: UNDER addition ******************'.
  "UNDER: Puts the output in the position of previous output
  "Note: If the output is written in the same line in which the previous output
  "is displayed, this output is overwritten.

  WRITE /5(5) abc.
  WRITE / 'fghij' UNDER abc.

  WRITE / '****************** WRITE: NO-GAP addition ******************'.
  WRITE / 'g'.
  WRITE 'ap1'.                 "Output: g ap1
  WRITE: / 'g' NO-GAP, 'ap2'.  "Output: gap2

  WRITE / '****************** WRITE: QUICKINFO addition ******************'.
  "QUICKINFO: Creates a tooltip for the output
  WRITE / 'Hover over the following output and check the tooltip:'.
  WRITE: (*) sy-uname QUICKINFO 'User name',
         '/',
         (*) sy-datum QUICKINFO 'Current date',
         '/',
         (*)  sy-uzeit QUICKINFO 'Current time'.

  WRITE / '****************** WRITE: Special list elements ******************'.
  "INPUT
  WRITE / input INPUT.
  linno_input = sy-linno.

  "AS CHECKBOX
  "The value (blank, X) is stored in the list buffer and can be evaluated during
  "a list event.
  WRITE: / chck1 AS CHECKBOX, 'Checkbox 1',
         / chck2 AS CHECKBOX, 'Checkbox 2'.

  "AS ICON: Outputting icons
  "Check the type pool ICON for names of predefined icons.
  WRITE / icon_green_light AS ICON.
  WRITE / icon_red_light AS ICON.
  WRITE / icon_yellow_light AS ICON.
  WRITE / icon_activate AS ICON.

  "AS SYMBOL: Outputting symbols
  "Check the type pool SYM for names of predefined icons.
  WRITE / sym_left_hand AS SYMBOL.
  WRITE / sym_caution AS SYMBOL.

  "AS LINE: Outputting corners, crosses, lines, and T sections
  "Check the type pool LINE for names of predefined icons.
  WRITE: /10 line_horizontal_line AS LINE NO-GAP,
             line_space           AS LINE NO-GAP,
             line_vertical_line   AS LINE NO-GAP.

  WRITE / '****************** WRITE: Formatting options ******************'.
  "The following examples show a selection. Various other additions are
  "available that deal with currency, unit, date and time-related formatting,
  "among others. See the ABAP Keyword Documentation.
  WRITE /(10) 'X' RIGHT-JUSTIFIED.
  WRITE /(10) 'X' CENTERED.
  WRITE /(10) `   X`.
  WRITE /(10) `   X` LEFT-JUSTIFIED.
  WRITE /(*) dc DECIMALS 2.

  "Colors
  "The commented out value stands for the value that can also be directly
  "specified in the syntax (except 0) or contained in a data object.
  "There are several additions with many options.
  WRITE / 'COL_BACKGROUND (0)' COLOR COL_BACKGROUND. "0 (GUI dependent)
  WRITE / 'COL_HEADING (1)' COLOR COL_HEADING. "1 (gray-blue)
  WRITE / 'COL_HEADING (1)' COLOR 1.
  WRITE / 'COL_NORMAL (2)' COLOR COL_NORMAL. "2 (light gray)

  WRITE / 'COL_TOTAL (3)' COLOR COL_TOTAL. "3 (yellow)
  WRITE / 'COL_KEY (4)' COLOR COL_KEY. "4 (blue-green)
  WRITE / 'COL_POSITIVE (5)' COLOR COL_POSITIVE. "5 (green)
  WRITE / 'COL_NEGATIVE (6)' COLOR COL_NEGATIVE. "6 (red)
  WRITE / 'COL_GROUP (7)' COLOR COL_GROUP. "7 (purple/orange)
  WRITE / 'COLOR 7 OFF' COLOR 7 OFF. "default color

  "Setting the intensity of the background color.
  WRITE / 'COLOR 7 INTENSIFIED OFF' COLOR 7 INTENSIFIED OFF.

  "INVERSE: When ON, the foreground, i.e. the output is displayed
  "in the selected color.
  WRITE / 'COLOR 7 INVERSE ON' COLOR 7 INVERSE ON.

  WRITE / '****************** FORMAT ******************'.
  "FORMAT: For applying settings (COLOR, INTENSIFIED, INVERSE ...)
  "on all of the following output statements up to a definition
  "with new settings

  WRITE / '****************** FORMAT: FRAMES addition ******************'.
  "FRAMES: Defines whether the - and | characters are converted to line elements,
  "producing continuous lines.
  FORMAT FRAMES ON.
  WRITE: / '----',
         / '|  |',
         / '----'.
  FORMAT FRAMES OFF.

  WRITE / '****************** FORMAT: COLOR addition ******************'.

  "This example shows the already covered COLOR addition with FORMAT
  "statements.
  FORMAT COLOR COL_POSITIVE.
  WRITE / 'ABC'.
  WRITE / 'DEF'.
  WRITE / 'GHI'.
  FORMAT COLOR OFF.
  WRITE / 'This is the first WRITE after a new setting with FORMAT ... OFF'.

  WRITE / '****************** FORMAT: RESET addition ******************'.
  "This addition sets all formatting settings for which the corresponding addition is
  "not specified in the same FORMAT statement to the state OFF (exception: FRAMES).
  "In the following example, note the effect of the RESET addition (apart from
  "resetting the previously set COLOR and INVERSE addition:
  "- INTENSIFIED ON is the state after the program start by default.
  "- Now, with the reset, the state is INTENSIFIED OFF.
  "- See the effect for all the WRITE statements that follow and that have
  "  the COLOR addition specified.
  FORMAT COLOR 7 INVERSE ON.
  WRITE / 'ABC'.
  WRITE / 'DEF'.
  FORMAT RESET.
  WRITE / 'GHI'.
  WRITE / 'JKL'.

  WRITE / '****************** ULINE ******************'.
  "ULINE: Creating a horizontal line
  ULINE.
  "More additions are available such as for the position and length.
  ULINE AT 5(20).

  WRITE / '****************** SET BLANK LINES ******************'.
  "SET BLANK LINES: Specifying if blank lines created using WRITE are displayed
  WRITE / 'SET BLANK LINES OFF.' COLOR 2.
  SET BLANK LINES OFF.
  LOOP AT itab INTO DATA(wa).
    WRITE / wa.
  ENDLOOP.

  WRITE / 'SET BLANK LINES ON.' COLOR 3.
  SET BLANK LINES ON.
  LOOP AT itab INTO wa.
    WRITE / wa.
  ENDLOOP.

  ULINE.

  WRITE / '****************** SKIP ******************'.
  "SKIP: Positions the list cursor explicitly
  WRITE / 'SKIP' COLOR 4.
  SKIP.
  WRITE / 'SKIP 3' COLOR 5.
  SKIP 3.
  ULINE.
  WRITE / 'SKIP TO LINE' COLOR 6.
  WRITE / 'Text A'.
  WRITE / 'Text B'.
  line = sy-linno. "The line number of the previous output is retrieved.
  WRITE / 'Text C'.
  SKIP TO LINE line.
  WRITE / 'Text D' COLOR 7.
  WRITE / 'Text E'.
  ULINE.

  WRITE / '****************** NEW-LINE ******************'.
  "NEW-LINE: Setting list cursor to the first position of the next line.
  "Additions are available to affect the scrolling behavior.
  WRITE / 'Next statement is NEW-LINE'.
  NEW-LINE.
  WRITE 'WRITE statement without /'.
  ULINE.

  WRITE / '****************** HIDE/HOTSPOT ******************'.
  "The example contains a DO loop. In this loop, WRITE statements output
  "the sy-index value (1 to 10). Based on the sy-index value, calculations
  "are performed. The result is hidden using HIDE. When you click a line
  "in the basic list in this very section, the hidden values are displayed
  "in the details list.
  WRITE / 'For the values of square and cube, click a line in this section.'.
  linno1 = sy-linno.
  FORMAT HOTSPOT ON.
  DO 10 TIMES.
    square = sy-index ** 2.
    cube   = sy-index ** 3.
    WRITE / sy-index.
    "Storing the content of a the variables together with the current list line.
    HIDE: square, cube.
  ENDDO.
  FORMAT HOTSPOT OFF.
  linno2 = sy-linno.

  WRITE / '****************** RESERVE/BACK ******************'.
  "RESERVE is demonstrated here with BACK.
  "Consider the RESERVE 5 LINES statements as defining a block of
  "5 lines. When using BACK after RESERVE, e.g. after a loop, you
  "can put the suqsequent output to the first line of this block of lines.
  WRITE / 'RESERVE without BACK:'.
  int_for_back = sy-colno.
  ULINE AT /(int_for_back).

  RESERVE 5 LINES.

  DO 5 TIMES.
    WRITE / sy-index.
  ENDDO.

  int_for_back = sy-colno.

  WRITE AT int_for_back '   <- This is not number 1.'.
  SKIP.

  WRITE / 'RESERVE with BACK:'.
  int_for_back = sy-colno.
  ULINE AT /(int_for_back).

  RESERVE 5 LINES.

  DO 5 TIMES.
    WRITE / sy-index.
  ENDDO.

  int_for_back = sy-colno.

  BACK.
  WRITE AT int_for_back '   <- This should be number 1.'.

  SKIP 5.

  WRITE / '****************** READ LINE ******************'.
  "READ LINE: Assigning the content of a line stored in the
  "list buffer to the system field sy-lisel. Plus, it allows
  "other target fields to be specified.

  "In the example, you can provide an integer in an input field
  "on whose basis a READ LINE statement is executed.

  WRITE / 'Insert a number representing a line number from which to read: '.
  FORMAT INPUT.
  WRITE / input_int.
  linno_int = sy-linno.
  FORMAT INPUT OFF.

  WRITE / '****************** MODIFY LINE ******************'.
  "MODIFY LINE: Overwrites a line stored in the list buffer with the
  "content of the sy-lisel system field Plus, it allows additional
  "modifications.

  "In the example, you can click 7 times in the very section on whose
  "basis a MODIFY LINE statement is executed.
  WRITE / 'Click 7 times on any line in this section starting from "1 A" to explore the effect of MODIFY LINE statements.'.

  SKIP.
  linno_mod1 = sy-linno.
  DO 10 TIMES.
    num += 1.
    char &&= 'A'.
    WRITE /10(*) num.
    WRITE 15 char.
  ENDDO.
  linno_mod2 = sy-linno.

**********************************************************************

AT LINE-SELECTION.

  "The purpose of the following implementation is to avoid multiple
  "details lists when clicking lines.

  IF sy-lsind = 1
  AND sy-lilli BETWEEN linno_mod1 AND linno_mod2.

    CASE count.
      WHEN 0.
        MODIFY CURRENT LINE LINE VALUE FROM 'Overwritten'.
      WHEN 1.
        MODIFY CURRENT LINE FIELD VALUE char FROM 'BBBB'.
      WHEN 2.
        MODIFY CURRENT LINE FIELD VALUE num FROM '#' char FROM 'CCCC'.
      WHEN 3.
        MODIFY CURRENT LINE FIELD FORMAT char COLOR 3.
      WHEN 4.
        MODIFY CURRENT LINE LINE FORMAT COLOR 4.
      WHEN 5.
        MODIFY CURRENT LINE FIELD FORMAT num COLOR 5 char COLOR 6
                            LINE FORMAT COLOR 7.
      WHEN 6.
        linno_copy = linno_mod1.
        WHILE linno_copy <= linno_mod2.
          MODIFY LINE linno_copy LINE VALUE FROM ':)' LINE FORMAT COLOR 3.
          linno_copy += 1.
          IF sy-index = 11.
            EXIT.
          ENDIF.
        ENDWHILE.
      WHEN OTHERS.
        EXIT.
    ENDCASE.
    count += 1.

  ELSEIF sy-lsind = 1
  AND sy-lilli NOT BETWEEN linno_mod1 AND linno_mod2.
    WRITE / `************************ Information about the line you clicked ************************` COLOR 7 INTENSIFIED OFF.
    SKIP.
    WRITE / |Line content (sy-lisel): "{ sy-lisel }"|.
    WRITE / |Line number (sy-lilli): "{ sy-lilli }"|.
    SKIP.
    ULINE.
    SKIP.
    WRITE / `************************ HIDE statements ************************` COLOR 7 INTENSIFIED OFF.
    SKIP.

    IF sy-lilli NOT BETWEEN linno1 + 1 AND linno2.
      WRITE / |You have clicked on line { sy-lilli } and not on a line between line { linno1 + 1 } and line { linno2 }.|.
      WRITE / `For exploring the effect of HIDE and displaying the values of 'square' and 'cube', click a line in the HIDE/HOTSPOT section of the basic list.`.
    ELSE.
      WRITE: / |square: { square }|.
      WRITE: / |cube: { cube }|.
    ENDIF.

    "READ LINE
    CLEAR str_tab.
    CLEAR lisel_tab.
    SKIP.
    ULINE.
    SKIP.
    WRITE / `************************ READ LINE statements ************************` COLOR 7 INTENSIFIED OFF.
    SKIP.

    READ CURRENT LINE.
    IF sy-subrc <> 0.
      lisel_copy = sy-lisel.
      APPEND VALUE #( statement = |READ CURRENT LINE. (sy-lisel contains line content)| line_content = |ERROR: Line does not exist| ) TO tab.
    ELSE.
      lisel_copy = sy-lisel.
      APPEND VALUE #( statement = |READ CURRENT LINE. (sy-lisel contains line content)| line_content = |sy-lisel: "{ lisel_copy }"| ) TO tab.
    ENDIF.

    "Put the entire line content (current line) of the read result in a data object
    READ CURRENT LINE LINE VALUE INTO str.
    IF sy-subrc <> 0.
      APPEND VALUE #( statement = `READ CURRENT LINE LINE VALUE INTO ...` line_content = `ERROR: Line does not exist` ) TO tab.
    ELSE.
      APPEND VALUE #( statement = `READ CURRENT LINE LINE VALUE INTO ...` line_content = |str: "{ lisel_copy }"| ) TO tab.
    ENDIF.

    "Specific line
    TRY.
        "Put concrete field values (specified line) of the read result in data objects
        READ LINE linno_int FIELD VALUE input_int INTO var.
        APPEND VALUE #( statement = |READ LINE { linno_int } FIELD VALUE ... INTO var. (Line of the input field)| line_content = |var: "{ var }"| ) TO tab.
        int = var.

        "Read entire line content
        READ LINE int.
        IF sy-subrc = 0.
          lisel_copy = sy-lisel.
          APPEND VALUE #( statement = |READ LINE { int }. (sy-lisel contains line content)| line_content = |sy-lisel: "{ lisel_copy }"| ) TO tab.
        ELSE.
          APPEND VALUE #( statement = |READ LINE { int }.| line_content = |ERROR: Line { int } does not exist| ) TO tab.
        ENDIF.
      CATCH cx_root.
        APPEND VALUE #( statement = |READ LINE { var }.| line_content = |ERROR: That statement does not work. Insert an integer value.| ) TO tab.
    ENDTRY.

    IF tab IS NOT INITIAL.
      LOOP AT tab INTO li.
        WRITE: / li-statement COLOR 2,
               / li-line_content.
        SKIP.
      ENDLOOP.
    ENDIF.

    CLEAR: tab, input, str.

    ULINE.
    SKIP.
    WRITE / `************************ MODIFY LINE statements ************************` COLOR 7 INTENSIFIED OFF.
    SKIP.
    WRITE / `To explore MODIFY LINE statements, click the lines in the MODIFY LINE section of the basic list.`.
    SKIP.
  ENDIF.
