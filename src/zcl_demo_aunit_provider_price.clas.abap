"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class represents a data provider class for an ABAP Unit demo. It implements the
"! interface {@link zif_demo_aunit_price} for providing discount information related to
"! price calculations. It contains methods for retrieving the discount percentage.
CLASS zcl_demo_aunit_provider_price DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES zif_demo_aunit_price.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS discount TYPE i VALUE 10.
ENDCLASS.



CLASS zcl_demo_aunit_provider_price IMPLEMENTATION.
  METHOD zif_demo_aunit_price~get_discount.
    discount_percentage = discount.
  ENDMETHOD.
ENDCLASS.
