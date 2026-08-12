***********************************************************************
*
*        ABAP cheat sheet: Selection screens and classic lists
*        Example: SELECT-OPTIONS statements
*
* -------------------------- PURPOSE ----------------------------------
* Example that demonstrates SELECT-OPTIONS statements in standard
* selection screens
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

DATA int TYPE i.
DATA int_tab TYPE TABLE OF i WITH EMPTY KEY.
DATA selcriteria TYPE string_table.
DATA str TYPE string.
FIELD-SYMBOLS <fs> TYPE ANY TABLE.

"SELECT-OPTIONS statements demonstrating various additions

"The following statement creates two input fields that can be
"used to specify selection criteria on the selection screen.
"In addition, the Multiple Selection button is available to
"further specify the selection criteria, e.g. what to include,
"exclude, etc.. As in all examples, an internal table of type
"TYPE RANGE OF whose contents can be evaluated.
"The FOR addition is followed by an already declared data object.
SELECT-OPTIONS a FOR int.

"NO-DISPLAY: Hides
SELECT-OPTIONS b FOR int NO-DISPLAY.

"NO-EXTENSION: The Multiple Selection button is not created on
"the selection screen
SELECT-OPTIONS c FOR int NO-EXTENSION.

"NO INTERVALS: Only one input field. Intervals can still be
"selected using the Multiple Selection button.
SELECT-OPTIONS d FOR int NO INTERVALS.

"Additions can be combined
SELECT-OPTIONS e FOR int NO-EXTENSION NO INTERVALS.

"DEFAULT ... TO ...: Providing start values for the columns in
"the first line of the selection table (low and high values)
SELECT-OPTIONS f FOR int NO-EXTENSION DEFAULT 3 TO 10.
SELECT-OPTIONS g FOR int DEFAULT 5 TO 9.

"DEFAULT ... OPTION ... SIGN ...: Providing further start values.
"See the details in the ABAP cheat sheet.
SELECT-OPTIONS h FOR int DEFAULT 4 TO 8 OPTION NB SIGN I.

**********************************************************************

INITIALIZATION.

  "Providing start values for data objects
  "The following internal table is filled and used as a data source
  "for a SELECT statement.
  int_tab = VALUE #( FOR i = 1 UNTIL i > 20 ( i ) ).
  "This table is filled here with the names from above. The table is looped
  "over in the START-OF-SELECTION event block to cover all SELECT-OPTIONS
  "from above.
  selcriteria = VALUE #( ( `A` ) ( `B` ) ( `C` ) ( `D` )
                         ( `E` ) ( `F` ) ( `G` ) ( `H` ) ).

**********************************************************************

START-OF-SELECTION.

  "Creating a classic list
  WRITE / `Data was retrieved from an internal table using a SELECT statement based on selection criteria.`.
  WRITE / `The table has the following content:`.

  LOOP AT int_tab INTO int.
    WRITE |{ int }|.
  ENDLOOP.
  SKIP 2.

  LOOP AT selcriteria INTO str.
    CASE str.
        "For historical reasons, the selection table is a table with header line.
        "Therefore, if you want to address the table content (beyond the use in a
        "SELECT ... WHERE ... IN ... statement), use the syntax `...[]`.
      WHEN `A`.
        ASSIGN a[] TO <fs>.
      WHEN `B`.
        ASSIGN b[] TO <fs>.
      WHEN `C`.
        ASSIGN c[] TO <fs>.
      WHEN `D`.
        ASSIGN d[] TO <fs>.
      WHEN `E`.
        ASSIGN e[] TO <fs>.
      WHEN `F`.
        ASSIGN f[] TO <fs>.
      WHEN `G`.
        ASSIGN g[] TO <fs>.
      WHEN `H`.
        ASSIGN h[] TO <fs>.
    ENDCASE.

    WRITE / |Result for selection options specified for "{ str }":| COLOR 2.
    SKIP.

    SELECT *
           FROM @int_tab AS tab
           WHERE table_line IN @<fs>
           INTO @int.
      WRITE |{ int }|.
    ENDSELECT.

    IF <fs> IS INITIAL.
      WRITE `(No selection criteria were provided. Therefore, all table lines are respected.)`.
    ENDIF.

    SKIP.
  ENDLOOP.
