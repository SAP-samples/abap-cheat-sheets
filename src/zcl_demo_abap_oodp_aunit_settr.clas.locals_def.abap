INTERFACE lif_get_data.
  METHODS:
    select_flight_data IMPORTING carrier            TYPE zdemoabapflights-carrid
                       RETURNING VALUE(flight_data) TYPE zcl_demo_abap_oodp_aunit_settr=>carr_tab.
ENDINTERFACE.
