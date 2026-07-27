"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class is used to demonstrate ABAP Unit tests using the ABAP Object-Oriented Test Double Framework. It provides a constructor
"! for dependency injection and a method to calculate prices based on current prices and discounts, utilizing a discount provider
"! interface.
CLASS zcl_demo_aunit_abap_oo_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    "! Initializes the data provider with a fallback to a default provider
    "!
    "! @parameter iref_data_provider | <p class="shorttext synchronized" lang="en">Reference to the data provider interface for discounts</p>
    METHODS constructor
      IMPORTING
        iref_data_provider TYPE REF TO zif_demo_aunit_price.

    "! <p class="shorttext synchronized" lang="en">Calculates the final price after applying a discount</p>
    "!
    "! @parameter current_price | <p class="shorttext synchronized" lang="en">Current price before discount</p>
    "! @parameter final_price   | <p class="shorttext synchronized" lang="en">Final price after discount calculations</p>
    METHODS calculate_price IMPORTING current_price      TYPE decfloat34
                            RETURNING VALUE(final_price) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA data_prov TYPE REF TO zif_demo_aunit_price.
ENDCLASS.



CLASS zcl_demo_aunit_abap_oo_tdf IMPLEMENTATION.
  METHOD constructor.
    data_prov = COND #( WHEN iref_data_provider IS BOUND THEN iref_data_provider
                        ELSE NEW zcl_demo_aunit_provider_price( ) ).
  ENDMETHOD.

  METHOD calculate_price.
    DATA(discount_percentage) = data_prov->get_discount( ).
    DATA(discount_factor) = CONV decfloat34( 1 - ( discount_percentage / 100 ) ).
    final_price = COND #( WHEN discount_factor < 0 OR discount_factor > 1
                          THEN round( val = current_price dec = 2 )
                          ELSE round( val = current_price * discount_factor dec = 2 ) ).
  ENDMETHOD.
ENDCLASS.
