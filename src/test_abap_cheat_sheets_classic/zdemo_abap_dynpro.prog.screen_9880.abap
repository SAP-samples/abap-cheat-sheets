PROCESS BEFORE OUTPUT.
 MODULE STATUS_9880.

PROCESS AFTER INPUT.
  MODULE USER_COMMAND_9880.
  "Field statement
  MODULE module_1.
  FIELD field_a.
  MODULE module_2.
  FIELD field_b.
  MODULE module_3.

  "FIELD statement, conditional dialog module call
  FIELD field_d MODULE module_4 ON INPUT.

  "CHAIN/FIELD statements
  CHAIN.
   FIELD field_e.
   FIELD field_f.
   MODULE module_5 ON CHAIN-INPUT.
  ENDCHAIN.

PROCESS ON HELP-REQUEST.
  FIELD field_help MODULE module_f1.

PROCESS ON VALUE-REQUEST.
  FIELD conn MODULE module_f4.
  FIELD carr MODULE module_dropdown.
