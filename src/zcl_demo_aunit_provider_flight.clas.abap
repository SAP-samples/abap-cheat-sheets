"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class represents a data provider class for an ABAP Unit demo. It implements the interface
"! {@link zif_demo_aunit_flights}, which defines a method for retrieving flight data.
CLASS zcl_demo_aunit_provider_flight DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES zif_demo_aunit_flights.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_provider_flight IMPLEMENTATION.

  METHOD zif_demo_aunit_flights~get_flight_data.
    SELECT seatsmax, seatsocc
       FROM ztaunitflights
       WHERE carrid = @carrier_id
       INTO CORRESPONDING FIELDS OF TABLE @flight_data.
  ENDMETHOD.

ENDCLASS.
