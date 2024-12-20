"! <p class="shorttext"><strong>RAP BO provider for a RAP demo scenario</strong><br/>ABAP cheat sheet example class </p>
"!
"! <p>The class represents a RAP BO provider (i. e. an ABAP behavior pool/ABP) for a RAP demo scenario:<br/>
"! Managed RAP BO with managed internal numbering and additional save, demonstrating the local consumption
"! of RAP business events.</p>
CLASS zbp_demo_abap_rap_ro_m_as DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zdemo_abap_rap_ro_m_as.
  PUBLIC SECTION.
    CLASS-DATA num_raised_events TYPE i.
ENDCLASS.

CLASS zbp_demo_abap_rap_ro_m_as IMPLEMENTATION.
ENDCLASS.
