"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class demonstrates ABAP Unit tests for database-dependent logic without relying on productive database table data. It
"! contains a method that reads seat data from a database table for a specified carrier, aggregates seat totals, and calculates the
"! rounded occupancy rate.
CLASS zcl_demo_aunit_sql_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Calculates the occupancy rate for a given carrier</p>
    "!
    "! @parameter carrier_id     | <p class="shorttext synchronized" lang="en">ID of the carrier for which to calculate occupancy</p>
    "! @parameter occupancy_rate | <p class="shorttext synchronized" lang="en">Calculated occupancy rate as a decimal value</p>
    METHODS calculate_occupancy_rate IMPORTING carrier_id            TYPE ztaunitflights-carrid
                                     RETURNING VALUE(occupancy_rate) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_sql_tdf IMPLEMENTATION.
  METHOD calculate_occupancy_rate.
    SELECT seatsmax, seatsocc
     FROM ztaunitflights
     WHERE carrid = @carrier_id
     INTO TABLE @DATA(flight_data).

    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.

    LOOP AT flight_data ASSIGNING FIELD-SYMBOL(<flight>).
      total_seatsmax += <flight>-seatsmax.
      total_seatsocc += <flight>-seatsocc.
    ENDLOOP.

    IF total_seatsmax <> 0.
      occupancy_rate = round( val = total_seatsocc / total_seatsmax * 100 dec = 2 ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
