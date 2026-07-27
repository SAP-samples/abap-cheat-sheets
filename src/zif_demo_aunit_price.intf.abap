INTERFACE zif_demo_aunit_price
  PUBLIC .

  METHODS get_discount RETURNING VALUE(discount_percentage) TYPE i.

ENDINTERFACE.
