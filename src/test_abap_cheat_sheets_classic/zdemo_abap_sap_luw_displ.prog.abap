*&---------------------------------------------------------------------*
*&   Demo program used in the context of the SAP LUW ABAP cheat sheet
*&
*&----------------------------- NOTE -----------------------------------*
*& The code presented in this class is intended only to support the ABAP
*& cheat sheets. It is not intended for direct use in a production system
*& environment. The code examples in the ABAP cheat sheets are primarily
*& intended to provide a better explanation and visualization of the
*& syntax and semantics of ABAP statements, not to solve concrete
*& programming tasks. For production application programs, you should
*& always work out your own solution for each individual case. There is
*& no guarantee for the correctness or completeness of the code.
*& Furthermore, there is no legal responsibility or liability for any
*& errors or their consequences that may occur when using the the example
*& code.
*&---------------------------------------------------------------------*
REPORT zdemo_abap_sap_luw_displ.

DATA: ok_code LIKE sy-ucomm,
      save_ok LIKE sy-ucomm,
      dynnr   LIKE sy-dynnr.

END-OF-SELECTION.

  "Checks if program is started separately
  SELECT COUNT(*)
    FROM zdemo_abap_logt
    INTO @DATA(entries).

  SELECT SINGLE id
    FROM zdemo_abap_logt
    WHERE id = 100
    INTO @DATA(exists).

  IF sy-subrc = 0 OR entries = 0.
    DELETE FROM zdemo_abap_logt.

    DATA(check) = abap_true.

    zcl_demo_abap_sap_luw_helper=>create_log_entries(
      VALUE #( ( id = 97
                 context = '-'
                 name = '-'
                 details = `Choose Start over from the menu.`
                 timestamp = utclong_current( ) ) ) ).
  ENDIF.

  zcl_demo_abap_sap_luw_helper=>create_log_entries(
    VALUE #( ( id = 100
               context = '-'
               name = '-'
               details = |New program executed: { sy-repid }|
               timestamp = utclong_current( ) ) ) ).

  CALL SCREEN 9780.

**********************************************************************

***************** Dialog modules for 9780 ****************************

MODULE status_9780 OUTPUT.
  SET PF-STATUS 'STATUS9780'.
  SET TITLEBAR 'TITLE9780'.

  "Checks for asynchronous update
  IF check = abap_false.
    DATA(idx) = 0.

    DO 5 TIMES. "A maximum of 5 seconds of waiting time
      SELECT SINGLE id, context
        FROM zdemo_abap_logt
        WHERE
        "Entry inserted ...
        id = 93 "by the final update function module
        OR id = 99 "in case of ROLLBACK WORK
        OR id = 96 "in case of the intended failure in SAP LUW
        OR context = 'Subroutine' "if subroutines are used
        INTO @DATA(wa).                             "#EC CI_CMPLX_WHERE

      IF sy-subrc = 0.
        EXIT.
      ELSE.
        idx = sy-index.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDDO.

    IF idx = 5.
      MESSAGE `The asynchronous update takes too long. ` &&
       `You may want to restart the example and try again.`
       TYPE 'I'.
    ENDIF.

  ENDIF.

  zcl_demo_abap_sap_luw_helper=>create_log_entries(
  VALUE #(
   ( id = 101
     context = 'PBO'
     name = 'status_9780'
     details = `Module called`
     timestamp = utclong_current( ) )
   ( id = 102
     context = 'PBO'
     name = 'status_9780'
     details = zcl_demo_abap_sap_luw_helper=>get_work_process_info( )
     timestamp = utclong_current( ) )
   ( id = 103
     context = 'PBO'
     name = 'status_9780'
     details = `SAP LUW Key: ` &&
     cl_system_transaction_state=>get_sap_luw_key( )
     timestamp = utclong_current( ) )
   ( id = 104
     context = 'PBO'
     name = 'status_9780'
     details = zcl_demo_abap_sap_luw_helper=>checking_transaction_state( )
     timestamp = utclong_current( ) ) ) ).

  zcl_demo_abap_sap_luw_helper=>cc_alv_display( 'CC_DB_FINAL' ).
  zcl_demo_abap_sap_luw_helper=>cc_alv_display( container = 'CC_WP'
                                          log = abap_true ).
  dynnr = sy-dynnr.
ENDMODULE.

MODULE user_command_9780 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'STARTOVER'.
      ROLLBACK WORK.
      DELETE FROM zdemo_abap_logt.
      SUBMIT ('ZDEMO_ABAP_SAP_LUW').
    WHEN OTHERS.
      DELETE FROM zdemo_abap_logt.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
