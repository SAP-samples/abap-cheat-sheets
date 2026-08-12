***********************************************************************
*
*        ABAP cheat sheet: Selection screens and classic lists
*
*
* -------------------------- PURPOSE ----------------------------------
* This program can be used for starting the selection screen and
* classic list programs contained in the repository.
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

TYPES: BEGIN OF reps_str,
         number TYPE i,
         report LIKE sy-repid,
       END OF reps_str.

DATA: rep     LIKE sy-repid,
      reports TYPE TABLE OF reps_str WITH EMPTY KEY,
      allowed TYPE string.

SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE title.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (25) lbl FOR FIELD report.
    PARAMETERS report TYPE i AS LISTBOX VISIBLE LENGTH 60.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl.

**********************************************************************

INITIALIZATION.
  title = 'Select and run a program'.
  lbl = 'Program:'.

  reports = VALUE #(
  ( number = 1 report = 'ZDEMO_ABAP_SELSCR_PARAMETERS' )
  ( number = 2 report = 'ZDEMO_ABAP_SELSCR_SELECT_OPT' )
  ( number = 3 report = 'ZDEMO_ABAP_SELSCR_STANDALONE' )
  ( number = 4 report = 'ZDEMO_ABAP_SELSCR_STMTS_VAR' )
  ( number = 5 report = 'ZDEMO_ABAP_LISTS' )
  ( number = 6 report = 'ZDEMO_ABAP_EVENT_BLOCKS' )
  ( number = 7 report = 'ZDEMO_ABAP_ALV' )
  ).

**********************************************************************

AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = CONV vrm_id( 'REPORT' )
      values = VALUE vrm_values(
                 ( key = 1 text = 'Selection screens: PARAMETERS (zdemo_abap_selscr_parameters)' )
                 ( key = 2 text = 'Selection screens: SELECT-OPTIONS (zdemo_abap_selscr_select_opt)' )
                 ( key = 3 text = 'Selection screens: SELECTION-SCREEN (zdemo_abap_selscr_standalone)' )
                 ( key = 4 text = 'Selection screens: SELECTION-SCREEN Variants (zdemo_abap_selscr_stmts_var)' )
                 ( key = 5 text = 'Lists: Creating Lists (zdemo_abap_lists)' )
                 ( key = 6 text = 'Event blocks (zdemo_abap_event_blocks)' )
                 ( key = 7 text = 'SAP List Viewer (ALV) (zdemo_abap_alv)' ) ).

**********************************************************************

START-OF-SELECTION.

  IF report BETWEEN 1 AND 7.
    "Dynamic programming check: Only submit program if its name is available
    "in the allow list
    TRY.
        rep = reports[ number = report ]-report.
        allowed = cl_abap_dyn_prg=>check_whitelist_tab(
           val      = rep
           whitelist = VALUE #( ( `ZDEMO_ABAP_SELSCR_PARAMETERS` )
                                ( `ZDEMO_ABAP_SELSCR_SELECT_OPT` )
                                ( `ZDEMO_ABAP_SELSCR_STANDALONE` )
                                ( `ZDEMO_ABAP_SELSCR_STMTS_VAR` )
                                ( `ZDEMO_ABAP_LISTS` )
                                ( `ZDEMO_ABAP_EVENT_BLOCKS` )
                                ( `ZDEMO_ABAP_ALV` ) ) ).

        SUBMIT (rep) AND RETURN VIA SELECTION-SCREEN.
      CATCH cx_root INTO DATA(err).
        MESSAGE |Error: { err->get_text( ) }| TYPE 'E'.
    ENDTRY.
  ELSE.
    MESSAGE 'Please select a program.' TYPE 'I' DISPLAY LIKE 'E'.
  ENDIF.
