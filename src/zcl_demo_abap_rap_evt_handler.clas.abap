"! <p class="shorttext"><strong>RAP event handler class</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class is the RAP event handler class for {@link zdemo_abap_rap_ro_m_as}.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li>The RAP business events in this example are raised using RAISE
"! ENTITY EVENT statements in the save_modified saver method that
"! is implemented in the CCIMP include of the ABAP behavior pool
"! {@link zbp_demo_abap_rap_ro_m_as}.</li>
"! <li>Find the <strong>disclaimer</strong> in the ABAP Doc comment of
"! class {@link zcl_demo_abap_aux}.</li></ul>
CLASS zcl_demo_abap_rap_evt_handler DEFINITION
  PUBLIC ABSTRACT FINAL
  FOR EVENTS OF zdemo_abap_rap_ro_m_as.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_rap_evt_handler IMPLEMENTATION.
ENDCLASS.
