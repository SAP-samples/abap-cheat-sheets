FUNCTION ZDEMO_ABAP_MODIFY_FU.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(VALUES) TYPE  ZDEMO_ABAP_LUW_T
*"----------------------------------------------------------------------

  values-time_stamp = utclong_current( ).

  MODIFY zdemo_abap_luw_t FROM @values.

  zcl_demo_abap_sap_luw_helper=>create_log_entries(
  VALUE #( ( id        = 80
             context   = 'Function module'
             name      = 'zdemo_abap_modify_fu'
             details   = `Function module called`
             timestamp = utclong_current( ) )
           ( id        = 81
             context   = 'Function module'
             name      = 'zdemo_abap_modify_fu'
             details   = zcl_demo_abap_sap_luw_helper=>get_work_process_info( )
             timestamp = utclong_current( ) )
           ( id        = 82
             context   = 'Function module'
             name      = 'zdemo_abap_modify_fu'
             details   = `SAP LUW Key: ` && cl_system_transaction_state=>get_sap_luw_key( )
             timestamp = utclong_current( ) )
           ( id        = 83
             context   = 'Function module'
             name      = 'zdemo_abap_modify_fu'
             details   = zcl_demo_abap_sap_luw_helper=>checking_transaction_state( )
             timestamp = utclong_current( ) ) ) ).



ENDFUNCTION.
