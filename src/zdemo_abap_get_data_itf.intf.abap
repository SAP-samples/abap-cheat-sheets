***********************************************************************
* ---------------------------- PURPOSE --------------------------------
* Interface to support the ABAP cheat sheet: ABAP Unit Tests
*
* ----------------------------- NOTE ----------------------------------
* The code presented in this class is only meant for supporting the ABAP
* cheat sheets. It is not intended for direct use in a
* production system environment. The code examples in the ABAP cheat
* sheets are primarily intended to provide a better explanation and
* visualization of the syntax and semantics of ABAP statements and not to
* solve concrete programming tasks. For production application programs,
* a dedicated solution should therefore always be worked out for each
* individual case. There is no guarantee for either the correctness or
* the completeness of the code. In addition, there is no legal
* responsibility or liability for possible errors or their consequences
* which occur through the use of the example code.
*
***********************************************************************
"! <p class="shorttext synchronized">Interface for ABAP cheat sheet example</p>
"! The interface supports the ABAP cheat sheet: ABAP Unit Tests.
INTERFACE zdemo_abap_get_data_itf
  PUBLIC.

  TYPES: carr_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY,
         occ_rate TYPE p LENGTH 4 DECIMALS 2.

  METHODS: select_flight_data IMPORTING carrier            TYPE zdemo_abap_fli-carrid
                              RETURNING VALUE(flight_data) TYPE carr_tab,
           say_hello RETURNING VALUE(hi) TYPE string.

ENDINTERFACE.
