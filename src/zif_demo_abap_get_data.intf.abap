"! <p class="shorttext"><strong>ABAP cheat sheet example interface</strong>
"! <br/>Interface supporting ABAP cheat sheet example classes</p>
"!
"! <h2>Note</h2>
"! <p>Find the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</p>
INTERFACE zif_demo_abap_get_data
  PUBLIC.

  TYPES: carr_tab TYPE TABLE OF zdemoabapflights WITH EMPTY KEY,
         occ_rate TYPE p LENGTH 4 DECIMALS 2.

  METHODS: select_flight_data IMPORTING carrier            TYPE zdemoabapflights-carrid
                              RETURNING VALUE(flight_data) TYPE carr_tab.

ENDINTERFACE.
