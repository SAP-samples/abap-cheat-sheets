CLASS lcl_data_provider DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_get_data.
ENDCLASS.

CLASS lcl_data_provider IMPLEMENTATION.
  METHOD lif_get_data~select_flight_data.
    SELECT seatsmax, seatsocc
      FROM zdemoabapflights
      WHERE carrid = @carrier
      INTO CORRESPONDING FIELDS OF TABLE @flight_data.
  ENDMETHOD.
ENDCLASS.
