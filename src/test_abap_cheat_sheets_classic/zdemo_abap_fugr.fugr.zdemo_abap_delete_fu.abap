FUNCTION zdemo_abap_delete_fu.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"----------------------------------------------------------------------


  DELETE FROM zdemo_abap_luw_t.

  zcl_demo_abap_sap_luw_helper=>create_log_entries(
  VALUE #( ( id        = 70
             context   = 'Function module'
             name      = 'zdemo_abap_delete_fu'
             details   = `Function module called`
             timestamp = utclong_current( ) )
           ( id        = 71
             context   = 'Function module'
             name      = 'zdemo_abap_delete_fu'
             details   = zcl_demo_abap_sap_luw_helper=>get_work_process_info( )
             timestamp = utclong_current( ) )
           ( id        = 72
             context   = 'Function module'
             name      = 'zdemo_abap_delete_fu'
             details   = `SAP LUW Key: ` && cl_system_transaction_state=>get_sap_luw_key( )
             timestamp = utclong_current( ) )
           ( id        = 73
             context   = 'Function module'
             name      = 'zdemo_abap_delete_fu'
             details   = zcl_demo_abap_sap_luw_helper=>checking_transaction_state( )
             timestamp = utclong_current( ) ) ) ).


ENDFUNCTION.
