"! <p class="shorttext"><strong>Interface supporting the ABAP Unit Tests example class </strong>
"! <br/>ABAP cheat sheet example interface</p>
"!
"! <h2>Note</h2>
"! <p>Find the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
INTERFACE zdemo_abap_get_data_itf
  PUBLIC.

  TYPES: carr_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY,
         occ_rate TYPE p LENGTH 4 DECIMALS 2.

  METHODS: select_flight_data IMPORTING carrier            TYPE zdemo_abap_fli-carrid
                              RETURNING VALUE(flight_data) TYPE carr_tab,
           say_hello RETURNING VALUE(hi) TYPE string.

ENDINTERFACE.
