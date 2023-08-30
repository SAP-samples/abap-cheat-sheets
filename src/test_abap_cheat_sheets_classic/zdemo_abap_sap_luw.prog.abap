*&---------------------------------------------------------------------*
*&                   ABAP cheat sheet: SAP LUW
*&
*&-------------------------- PURPOSE ----------------------------------*
*& - Example to demonstrate ABAP syntax in the context of the SAP LUW.
*& - Topics covered: Using update function modules and subroutines as
*&   bundling techniques, synchronous/asynchronous/local updates
*&
*&----------------------- GETTING STARTED -----------------------------*
*& - Open the program with the ABAP development tools for Eclipse (ADT).
*& - Choose F8 to run the program.
*& - Select the radio buttons and choose 'Go' to progress with the program.
*&
*&----------------------------- NOTE -----------------------------------*
*&* The code presented in this class is intended only to support the ABAP
*&* cheat sheets. It is not intended for direct use in a production system
*&* environment. The code examples in the ABAP cheat sheets are primarily
*&* intended to provide a better explanation and visualization of the
*&* syntax and semantics of ABAP statements, not to solve concrete
*&* programming tasks. For production application programs, you should
*&* always work out your own solution for each individual case. There is
*&* no guarantee for the correctness or completeness of the code.
*&* Furthermore, there is no legal responsibility or liability for any
*&* errors or their consequences that may occur when using the the example
*&* code.
*&---------------------------------------------------------------------*
REPORT zdemo_abap_sap_luw.

DATA: tab_struc    TYPE zdemo_abap_luw_t,
      ok_code      LIKE sy-ucomm,
      save_ok      LIKE sy-ucomm,
      del_insert   TYPE abap_bool,
      perform      TYPE abap_bool,
      commit       TYPE abap_bool,
      commit_wait  TYPE abap_bool,
      local_update TYPE abap_bool,
      err          TYPE abap_bool,
      rollback     TYPE abap_bool,
      terminate    TYPE abap_bool,
      dynnr        TYPE sy-dynnr,
      counter      TYPE i,
      log_itab     TYPE zcl_demo_abap_sap_luw_helper=>log_type.

CLASS lcl DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF request4log,
             log      TYPE zdemo_abap_logt,
             called   TYPE abap_bool,
             wp_info  TYPE abap_bool,
             luw_key  TYPE abap_bool,
             tx_state TYPE abap_bool,
           END OF request4log.

    CLASS-METHODS: prep,
      create_log_entries IMPORTING req TYPE request4log.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD prep.
    DELETE FROM zdemo_abap_luw_t.

    INSERT zdemo_abap_luw_t FROM TABLE @( VALUE #(
    ( key_field  = 1
      ch         = 'DEMO DATA'
      num        = 11
      time_stamp = utclong_current( ) )
    ( key_field  = 2
      ch         = 'THAT WAS'
      num        = 22
      time_stamp = utclong_current( ) )
    ( key_field  = 3
      ch         = 'CREATED'
      num        = 33
      time_stamp = utclong_current( ) )
    ( key_field  = 4
      ch         = 'IN ADVANCE'
      num        = 44
      time_stamp = utclong_current( ) ) ) ).

    DELETE FROM zdemo_abap_logt.
  ENDMETHOD.

  METHOD create_log_entries.

    IF req-called = abap_true.
      counter += 1.
      INSERT VALUE #( id        = counter
                      context   = req-log-context
                      name      = req-log-name
                      details   = `Called!`
                      timestamp = utclong_current( )
                    ) INTO TABLE log_itab.
    ENDIF.

    IF req-luw_key = abap_true.
      counter += 1.
      INSERT VALUE #( id        = counter
                      context   = req-log-context
                      name      = req-log-name
                      details   = `SAP LUW key: ` &&
                      cl_system_transaction_state=>get_sap_luw_key( )
                      timestamp = utclong_current( )
                    ) INTO TABLE log_itab.

    ENDIF.

    IF req-wp_info = abap_true.
      counter += 1.
      INSERT VALUE #( id        = counter
                      context   = req-log-context
                      name      = req-log-name
                      details   =
                      zcl_demo_abap_sap_luw_helper=>get_work_process_info( )
                      timestamp = utclong_current( )
                    ) INTO TABLE log_itab.
    ENDIF.

    IF req-tx_state = abap_true.
      counter += 1.
      INSERT VALUE #(
        id        = counter
        context   = req-log-context
        name      = req-log-name
        details   =
        zcl_demo_abap_sap_luw_helper=>checking_transaction_state( )
        timestamp = utclong_current( )
      ) INTO TABLE log_itab.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM delete.
  zcl_demo_abap_sap_luw_helper=>subr_delete( ).

  lcl=>create_log_entries(
  VALUE #( log      = VALUE #( context = 'Subroutine'
                               name    = 'delete' )
           called   = abap_true
           luw_key  = abap_true
           tx_state = abap_true
           wp_info  = abap_true ) ).
ENDFORM.

FORM insert.

  DATA values TYPE zdemo_abap_luw_t.
  IMPORT values = values FROM MEMORY ID 'DEMO_INSERT'.
  IF sy-subrc = 0.
    values-time_stamp = utclong_current( ).
    zcl_demo_abap_sap_luw_helper=>subr_insert( values ).
  ENDIF.

  lcl=>create_log_entries(
  VALUE #( log      = VALUE #( context = 'Subroutine'
                               name    = 'insert' )
           called   = abap_true
           luw_key  = abap_true
           tx_state = abap_true
           wp_info  = abap_true ) ).
ENDFORM.

FORM update_log.
  lcl=>create_log_entries(
  VALUE #( log      = VALUE #( context = 'Subroutine'
                               name    = 'update_log' )
           called   = abap_true
           luw_key  = abap_true
           tx_state = abap_true
           wp_info  = abap_true ) ).

  zcl_demo_abap_sap_luw_helper=>subr_update_log( log_itab ).
ENDFORM.

FORM call_on_rollback.

  "No implementation here. Only to demonstrate that the
  "subroutine is called in the event of triggering a
  "rollback.

  lcl=>create_log_entries(
    VALUE #( log      = VALUE #( context = 'Subroutine'
                                 name    = 'call_on_rollback' )
             called   = abap_true
             luw_key  = abap_true
             tx_state = abap_true ) ).
ENDFORM.

END-OF-SELECTION.

  lcl=>prep( ).

  counter += 1.
  INSERT VALUE #(
    id        = counter
    context   = '-'
    name      = '-'
    details   = |Program { sy-repid } has been started.| &&
                `The first dynpro is about to be called.`
    timestamp = utclong_current( )
  ) INTO TABLE log_itab.

  counter += 1.
  INSERT VALUE #( id        = counter
                  context   = '-'
                  name      = '-'
                  details   = `SAP LUW Key: ` &&
                  cl_system_transaction_state=>get_sap_luw_key( )
                  timestamp = utclong_current( )
                ) INTO TABLE log_itab.

  CALL SCREEN 9750.

**********************************************************************

***************** Dialog modules for 9750 ****************************

MODULE status_9750 OUTPUT.
  SET PF-STATUS 'STATUS9750'.
  SET TITLEBAR 'TITLE9750'.

  lcl=>create_log_entries(
   VALUE #( log      = VALUE #( context = 'PBO'
                                name    = 'status_9750' )
            called   = abap_true
            luw_key  = abap_true
            tx_state = abap_true
            wp_info  = abap_true ) ).

  zcl_demo_abap_sap_luw_helper=>cc_alv_display( 'DBENTRIES' ).
  dynnr = sy-dynnr.
ENDMODULE.

MODULE user_command_9750 INPUT.
  lcl=>create_log_entries(
      VALUE #( log      = VALUE #( context = 'PAI'
                                   name    = 'user_command_9750' )
               called   = abap_true
               luw_key  = abap_true
               tx_state = abap_true
               wp_info  = abap_true ) ).

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'GO' OR 'ENTER'.

      IF del_insert = abap_true.
        CALL FUNCTION 'ZDEMO_ABAP_DELETE_FU' IN UPDATE TASK.

        MESSAGE `A function module has been registered ` &&
                `that clears the database table.` TYPE 'I'.

      ELSEIF local_update = abap_true.

        SET UPDATE TASK LOCAL.

        lcl=>create_log_entries(
          VALUE #( log      = VALUE #( context = 'PAI'
                                       name    = 'user_command_9750' )
                   luw_key  = abap_true
                   tx_state = abap_true
                   wp_info  = abap_true ) ).

        CALL FUNCTION 'ZDEMO_ABAP_DELETE_FU' IN UPDATE TASK.

        MESSAGE `Local update has been activated. A function ` &&
                `module has been registered that clears the database table.` &&
                ` No matter if you choose COMMIT WORK or COMMIT WORK AND ` &&
                `WAIT in the next step, it will be a synchronous update.` TYPE 'I'.

      ELSEIF perform = abap_true.

        PERFORM delete ON COMMIT.
        PERFORM call_on_rollback ON ROLLBACK.

        lcl=>create_log_entries(
          VALUE #( log      = VALUE #( context = 'PAI'
                                       name    = 'user_command_9750' )
                   luw_key  = abap_true
                   tx_state = abap_true
                   wp_info  = abap_true ) ).

        MESSAGE `Subroutines have been registered for commit ` &&
                `and rollback.` TYPE 'I'.

      ENDIF.

    WHEN OTHERS.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

***************** Dialog modules for 9760 ****************************

MODULE status_9760 OUTPUT.
  SET PF-STATUS 'STATUS9760'.
  SET TITLEBAR 'TITLE9760'.

  lcl=>create_log_entries(
      VALUE #( log      = VALUE #( context = 'PBO'
                                   name    = 'status_9760' )
               called   = abap_true
               luw_key  = abap_true
               tx_state = abap_true
               wp_info  = abap_true ) ).

  dynnr = sy-dynnr.
ENDMODULE.

MODULE user_command_9760 INPUT.
  lcl=>create_log_entries(
     VALUE #( log      = VALUE #( context = 'PAI'
                                  name    = 'user_command_9760' )
              called   = abap_true
              luw_key  = abap_true
              tx_state = abap_true
              wp_info  = abap_true ) ).

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'GO' OR 'ENTER'.

      IF perform = abap_true.
        "Registered subroutines cannot have any parameter interface.
        "Therefore, data can only be passed through external interfaces,
        "for example, the ABAP memory.
        EXPORT values = tab_struc TO MEMORY ID 'DEMO_INSERT'.
        PERFORM insert ON COMMIT.

        MESSAGE `A subroutine that inserts the entries in the ` &&
                `database table has been registered. ` &&
                `When you select a commit option in the next step, ` &&
                `COMMIT WORK and COMMIT WORK AND WAIT have ` &&
                `the same effect. The subroutines are executed in ` &&
                `the current work process in this case.` TYPE 'I'.
      ELSE.
        CALL FUNCTION 'ZDEMO_ABAP_MODIFY_FU' IN UPDATE TASK
          EXPORTING
            values = tab_struc.
        MESSAGE `An update function module that inserts the ` &&
                `entries in the database table has ` &&
                `been registered.` TYPE 'I'.
      ENDIF.
    WHEN OTHERS.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

***************** Dialog modules for 9770 ****************************

MODULE status_9770 OUTPUT.
  SET PF-STATUS 'STATUS9770'.
  SET TITLEBAR 'TITLE9770'.

  lcl=>create_log_entries(
     VALUE #( log      = VALUE #( context = 'PBO'
                                  name    = 'status_9770' )
              called   = abap_true
              luw_key  = abap_true
              tx_state = abap_true
              wp_info  = abap_true ) ).

  dynnr = sy-dynnr.

  IF perform = 'X'.
    LOOP AT SCREEN INTO DATA(scr).
      IF scr-name = 'EXCURSION_WP'
      OR scr-name = 'WAIT'
      OR scr-name = 'FAIL_LUW'
      OR scr-name = 'ERR'
      OR scr-name = 'TERMINATE'
      OR scr-name = 'ERROR_A'.
        scr-invisible    = '1'.
        MODIFY SCREEN FROM scr.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.

MODULE user_command_9770 INPUT.
  lcl=>create_log_entries(
      VALUE #( log      = VALUE #( context = 'PAI'
                                   name    = 'user_command_9770' )
               called   = abap_true
               luw_key  = abap_true
               tx_state = abap_true
               wp_info  = abap_true ) ).

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.

    WHEN 'GO' OR 'ENTER' OR 'OK'.

      CASE 'X'.

        WHEN commit.
          IF perform = abap_false.
            CALL FUNCTION 'ZDEMO_ABAP_LOG_FU' IN UPDATE TASK
              EXPORTING
                log = log_itab.
            MESSAGE `An update function module that inserts ` &&
                    `entries for the log has been registered. ` &&
                    `COMMIT WORK is about to be executed.` TYPE 'I'.
          ELSE.
            PERFORM update_log ON COMMIT.

            MESSAGE `A subroutine that inserts entries for the ` &&
                    `log has been registered ` &&
                    `COMMIT WORK is about to be executed.` TYPE 'I'.
          ENDIF.

          COMMIT WORK.

          SELECT COUNT(*) FROM zdemo_abap_luw_t INTO @DATA(entries).

          MESSAGE `COMMIT WORK has been executed. Current ` &&
                  `number of database table entries: ` &&
                  entries &&
                  `. If it is 4 and not 1, the update will be done ` &&
                  `asynchronously.` TYPE 'I'.
        WHEN commit_wait.
          IF perform = abap_false.
            CALL FUNCTION 'ZDEMO_ABAP_LOG_FU' IN UPDATE TASK
              EXPORTING
                log = log_itab.
            MESSAGE `An update function module that inserts ` &&
                    `entries for the log has been registered. ` &&
                    `COMMIT WORK AND WAIT is about to be executed.` TYPE 'I'.
          ELSE.
            PERFORM update_log ON COMMIT.

            MESSAGE `A subroutine that inserts entries for the ` &&
                    `log has been registered ` &&
                    `COMMIT WORK AND WAIT is about to be executed.` TYPE 'I'.
          ENDIF.

          COMMIT WORK AND WAIT.

          SELECT COUNT(*) FROM zdemo_abap_luw_t INTO @entries.

          MESSAGE `COMMIT WORK AND WAIT has been executed. Current ` &&
                  `number of database table entries: ` &&
                  entries &&
                  `. If it is 1, the update has been done synchronously.` TYPE 'I'.
        WHEN rollback.
          ROLLBACK WORK.

          MESSAGE `ROLLBACK WORK has been executed. A rollback has ` &&
                  `been triggered. The original database table content ` &&
                  `should be displayed.`
          TYPE 'I'.

          MESSAGE `Note: As an exception, and only for this demo ` &&
                  `example, the sample implementation includes a ` &&
                  `database update to the log table after the ` &&
                  `table after the rollback, just to show the progress of the ` &&
                  `execution up to that point.` TYPE 'I' DISPLAY LIKE 'E'.

          "Update log table
          zcl_demo_abap_sap_luw_helper=>create_log_entries( log_itab ).

          "Log information for triggering a rollback
          zcl_demo_abap_sap_luw_helper=>create_log_entries(
            VALUE #( ( id        = 99
                       context   = '-'
                       name      = '-'
                       details   = `Rollback triggered`
                       timestamp = utclong_current( ) ) ) ).
        WHEN err.
          CALL FUNCTION 'ZDEMO_ABAP_LOG_FU' IN UPDATE TASK
            EXPORTING
              log         = log_itab
              cause_error = abap_true.

          IF local_update = abap_true.
            MESSAGE `A function module has been registered that  ` &&
                    `causes an error (zero division). COMMIT WORK AND WAIT ` &&
                    `is about to be executed. You are directly informed ` &&
                    `about the error and the program is terminated. All ` &&
                    `changes are implicitly rolled back. You can check ` &&
                    `transaction ST22 for the error caused.` TYPE 'I'.
          ELSE.
            MESSAGE `Function module was registered that ` &&
                    `causes an error (zero division). COMMIT WORK AND WAIT` &&
                    ` will be excetued. All changes are implicitly rolled back.` &&
                    ` When you start over or leave the example in the next dynpro, ` &&
                    `you should be informed about the issue in the Business Workplace. ` &&
                    `Also check ST22.` TYPE 'I'.
          ENDIF.

          COMMIT WORK AND WAIT.

        WHEN terminate.
          MESSAGE `This is an error message of type A. ` &&
                  `It termiantes the program. All changes are ` &&
                  `implicitly rolled back. ` &&
                  `You may want to check the database table entries of ` &&
                  `the example that remain unchanged.` TYPE 'A'.
      ENDCASE.

    WHEN OTHERS.
      LEAVE PROGRAM.
  ENDCASE.

  SUBMIT ('ZDEMO_ABAP_SAP_LUW_DISPL').
ENDMODULE.
