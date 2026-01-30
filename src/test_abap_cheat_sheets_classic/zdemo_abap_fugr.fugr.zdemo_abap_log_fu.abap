FUNCTION ZDEMO_ABAP_LOG_FU.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(LOG) TYPE  ZCL_DEMO_ABAP_SAP_LUW_HELPER=>LOG_TYPE
*"     VALUE(CAUSE_ERROR) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"----------------------------------------------------------------------


  MODIFY zdemo_abap_logt FROM TABLE @log.

  zcl_demo_abap_sap_luw_helper=>create_log_entries(
  VALUE #( ( id        = 90
             context   = `Function module`
             name      = `zdemo_abap_log_fu`
             details   = `Function module called`
             timestamp = utclong_current( ) )
           ( id        = 91
             context   = `Function module`
             name      = `zdemo_abap_log_fu`
             details   = zcl_demo_abap_sap_luw_helper=>get_work_process_info( )
             timestamp = utclong_current( ) )
           ( id        = 92
             context   = `Function module`
             name      = `zdemo_abap_log_fu`
             details   = `SAP LUW Key: ` && cl_system_transaction_state=>get_sap_luw_key( )
             timestamp = utclong_current( ) )
           ( id        = 93
             context   = `Function module`
             name      = `zdemo_abap_log_fu`
             details   = zcl_demo_abap_sap_luw_helper=>checking_transaction_state( )
             timestamp = utclong_current( ) ) ) ).

  IF cause_error = abap_true.
    DATA(error_not_caught) = 1 / 0.
  ENDIF.


ENDFUNCTION.
