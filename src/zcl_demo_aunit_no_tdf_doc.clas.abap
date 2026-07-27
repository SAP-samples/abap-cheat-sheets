"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class is designed to showcase ABAP Unit tests using self-created test doubles for price and flight data providers. It
"! offers methods for calculating price based on discounts and occupancy rates from flight data. The class supports
"! constructor-based dependency injection to handle dependencies.
CLASS zcl_demo_aunit_no_tdf_doc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    "! Initializes the class with optional flight and price providers or defaults to the productive ones
    "!
    "! @parameter flights | <p class="shorttext synchronized" lang="en">Reference to the flights provider interface</p>
    "! @parameter price   | <p class="shorttext synchronized" lang="en">Reference to the price provider interface</p>
    METHODS constructor
      IMPORTING
        flights TYPE REF TO zif_demo_aunit_flights OPTIONAL
        price   TYPE REF TO zif_demo_aunit_price OPTIONAL.

    "! Calculates the final price based on current price and applicable discount
    "!
    "! @parameter current_price | <p class="shorttext synchronized" lang="en">The original price before discount</p>
    "! @parameter final_price   | <p class="shorttext synchronized" lang="en">The calculated final price after discount</p>
    METHODS calculate_price IMPORTING current_price      TYPE decfloat34
                            RETURNING VALUE(final_price) TYPE decfloat34.

    "! Calculates the occupancy rate based on flight data for a specific carrier
    "!
    "! @parameter carrier_id     | <p class="shorttext synchronized" lang="en">Identifier for the specific airline carrier</p>
    "! @parameter occupancy_rate | <p class="shorttext synchronized" lang="en">The calculated occupancy rate as a percentage</p>
    METHODS calculate_occupancy_rate IMPORTING carrier_id            TYPE ztaunitflights-carrid
                                     RETURNING VALUE(occupancy_rate) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA data_prov_flights TYPE REF TO zif_demo_aunit_flights.
    DATA data_prov_price TYPE REF TO zif_demo_aunit_price.
ENDCLASS.



CLASS zcl_demo_aunit_no_tdf_doc IMPLEMENTATION.
  METHOD constructor.
    data_prov_flights = COND #( WHEN flights IS BOUND THEN flights
                                ELSE NEW zcl_demo_aunit_provider_flight( ) ).
    data_prov_price = COND #( WHEN price IS BOUND THEN price
                              ELSE NEW zcl_demo_aunit_provider_price( ) ).
  ENDMETHOD.

  METHOD calculate_price.
    DATA(discount_percentage) = data_prov_price->get_discount( ).
    DATA(discount_factor) = CONV decfloat34( 1 - ( discount_percentage / 100 ) ).
    final_price = COND #( WHEN discount_factor < 0 OR discount_factor > 1
                          THEN round( val = current_price dec = 2 )
                          ELSE round( val = current_price * discount_factor dec = 2 ) ).
  ENDMETHOD.

  METHOD calculate_occupancy_rate.
    DATA(flight_data) = data_prov_flights->get_flight_data( carrier_id ).

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
