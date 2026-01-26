CLASS lcl_data_provider DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_demo_abap_get_data.
ENDCLASS.

CLASS lcl_data_provider IMPLEMENTATION.
  METHOD zif_demo_abap_get_data~select_flight_data.
    SELECT seatsmax, seatsocc
      FROM zdemo_abap_fli
      WHERE carrid = @carrier
      INTO CORRESPONDING FIELDS OF TABLE @flight_data.
  ENDMETHOD.
ENDCLASS.
