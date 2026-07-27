INTERFACE zif_demo_aunit_flights
  PUBLIC .

  TYPES t_flight_data TYPE TABLE OF ztaunitflights WITH EMPTY KEY.

  METHODS get_flight_data IMPORTING carrier_id         TYPE ztaunitflights-carrid
                          RETURNING VALUE(flight_data) TYPE t_flight_data.

ENDINTERFACE.
