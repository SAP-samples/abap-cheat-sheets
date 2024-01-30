**********************************************************************
* Note:
*
* - This class is the RAP event handler class for zdemo_abap_rap_ro_m_as.
*
* - The RAP business events in this example are raised using RAISE
*   ENTITY EVENT statements in the save_modified saver method that
*   is implemented in the CCIMP include of the ABAP behavior pool
*   zbp_demo_abap_rap_ro_m_as.
*
**********************************************************************

CLASS zcl_demo_abap_rap_evt_handler DEFINITION
  PUBLIC ABSTRACT FINAL
  FOR EVENTS OF zdemo_abap_rap_ro_m_as.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_rap_evt_handler IMPLEMENTATION.
ENDCLASS.
