"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class is used to demonstrate ABAP Unit tests for CDS-dependent logic without relying on productive data. It includes a
"! method for reading flight seat data from the CDS entity zraunitflights and calculating the occupancy rate.
CLASS zcl_demo_aunit_cds_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    "! Calculates the occupancy rate based on total seats and occupied seats
    "!
    "! @parameter carrier_id     | <p class="shorttext synchronized" lang="en">ID of the airline carrier</p>
    "! @parameter occupancy_rate | <p class="shorttext synchronized" lang="en">Calculated occupancy rate as a decimal value</p>
    METHODS calculate_occupancy_rate IMPORTING carrier_id            TYPE zraunitflights-carrid
                                     RETURNING VALUE(occupancy_rate) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_cds_tdf IMPLEMENTATION.
  METHOD calculate_occupancy_rate.
    SELECT seatsmax, seatsocc
     FROM zraunitflights
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
