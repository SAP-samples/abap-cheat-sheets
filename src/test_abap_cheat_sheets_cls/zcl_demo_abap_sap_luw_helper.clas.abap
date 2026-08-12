CLASS zcl_demo_abap_sap_luw_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES log_type TYPE SORTED TABLE OF zdemo_abap_logt WITH UNIQUE KEY id.

    CLASS-DATA: itab        TYPE STANDARD TABLE OF zdemo_abap_luw_t,
                log_entries TYPE STANDARD TABLE OF zdemo_abap_logt.

    CLASS-METHODS: get_work_process_info RETURNING VALUE(id_no) TYPE string,
      create_log_entries IMPORTING log TYPE log_type,
      checking_transaction_state RETURNING VALUE(tx_state) TYPE string,
      cc_alv_display IMPORTING container TYPE c
                               log       TYPE abap_bool DEFAULT abap_false,
      subr_delete,
      subr_insert IMPORTING values TYPE zdemo_abap_luw_t,
      subr_update_log IMPORTING log TYPE log_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: wp_pid   TYPE wppid,
                wp_index TYPE wpindex.
ENDCLASS.



CLASS ZCL_DEMO_ABAP_SAP_LUW_HELPER IMPLEMENTATION.


  METHOD cc_alv_display.
    DATA(cont) = NEW cl_gui_custom_container( container_name = container ).

    IF log = abap_false.
      SELECT key_field, ch, num, time_stamp
                 FROM zdemo_abap_luw_t
                 INTO CORRESPONDING FIELDS OF TABLE @Itab
                 ORDER BY key_field.

      TRY.
          cl_salv_table=>factory( EXPORTING r_container = cont
                                            container_name = CONV string( container )
                                  IMPORTING r_salv_table = DATA(alv)
                                  CHANGING t_table = itab ).

          DATA(columns) = alv->get_columns( ).
          DATA(col_tab) = columns->get( ).
          LOOP AT col_tab ASSIGNING FIELD-SYMBOL(<column>).
            DATA(column) = columns->get_column( <column>-columnname ).
            column->set_medium_text( CONV scrtext_m( <column>-columnname ) ).

            IF <column>-columnname = 'CLIENT'.
              <column>-r_column->set_visible( abap_false ).
            ELSE.
              <column>-r_column->set_visible( abap_true ).
            ENDIF.
          ENDLOOP.
          alv->display( ).
        CATCH cx_salv_msg cx_salv_not_found.
          MESSAGE 'ALV display not possible' TYPE 'I'
                  DISPLAY LIKE 'E'.
      ENDTRY.

    ELSE.

      SELECT id, context, name, details, timestamp
         FROM zdemo_abap_logt
         INTO CORRESPONDING FIELDS OF TABLE @log_entries
         ORDER BY id.

      TRY.
          cl_salv_table=>factory( EXPORTING r_container = cont
                                            container_name = CONV string( container )
                                  IMPORTING r_salv_table = alv
                                  CHANGING t_table = log_entries ).

          columns = alv->get_columns( ).
          col_tab = columns->get( ).
          LOOP AT col_tab ASSIGNING FIELD-SYMBOL(<col>).
            column = columns->get_column( <col>-columnname ).
            column->set_medium_text( CONV scrtext_m( <col>-columnname ) ).

            IF <col>-columnname = `DETAILS`.
              column->set_output_length( 70 ).
            ENDIF.

            IF <col>-columnname = 'CLIENT'.
              <col>-r_column->set_visible( abap_false ).
            ELSE.
              <col>-r_column->set_visible( abap_true ).
            ENDIF.
          ENDLOOP.
          alv->display( ).
        CATCH cx_salv_msg cx_salv_not_found.
          MESSAGE 'ALV display not possible' TYPE 'I'
                  DISPLAY LIKE 'E'.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD checking_transaction_state.
    tx_state = `Tx state: ` &&
    `Update task (` && SWITCH #( cl_system_transaction_state=>get_in_update_task( ) WHEN 1 THEN `1)` ELSE `0)` ) &&
    ` Local update (` && SWITCH #( cl_system_transaction_state=>get_update_task_local( ) WHEN 1 THEN `1)` ELSE `0)` ) &&
    ` Perf. on commit (` && SWITCH #( cl_system_transaction_state=>get_on_commit( ) WHEN 1 THEN `1)` ELSE `0)` ) &&
    ` Perf. on rollback (` && SWITCH #( cl_system_transaction_state=>get_on_rollback( ) WHEN 1 THEN `1)` ELSE `0)` ).
  ENDMETHOD.


  METHOD create_log_entries.
    INSERT zdemo_abap_logt FROM TABLE @log.
  ENDMETHOD.


  METHOD get_work_process_info.
    CALL FUNCTION 'TH_GET_OWN_WP_NO'
      IMPORTING
        wp_pid   = wp_pid
        wp_index = wp_index.

    id_no = |Work process ID: { wp_pid } / work process index: { wp_index }|.
  ENDMETHOD.


  METHOD subr_delete.
    DELETE FROM zdemo_abap_luw_t.
  ENDMETHOD.


  METHOD subr_insert.
    INSERT zdemo_abap_luw_t FROM @values.
  ENDMETHOD.


  METHOD subr_update_log.
    INSERT zdemo_abap_logt FROM TABLE @log.
  ENDMETHOD.
ENDCLASS.
