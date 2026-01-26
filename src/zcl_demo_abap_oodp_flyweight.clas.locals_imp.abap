CLASS lcl_customer DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*& Flyweight interface
*&---------------------------------------------------------------------*

INTERFACE lif_flyweight.
  METHODS notify IMPORTING customer_ref TYPE REF TO lcl_customer
                 RETURNING VALUE(msg)   TYPE string_table.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Class for holding a unique state
*&---------------------------------------------------------------------*

CLASS lcl_customer DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING id TYPE i,
      get_customer_info RETURNING VALUE(customer_info) TYPE zcl_demo_abap_oodp_flyweight=>ts_customer_info.
  PRIVATE SECTION.
    DATA: customer_info TYPE zcl_demo_abap_oodp_flyweight=>ts_customer_info,
          msg           TYPE string_table.
ENDCLASS.

CLASS lcl_customer IMPLEMENTATION.
  METHOD constructor.
    "For the self-contained example, the customer data, representing
    "the unique data, is designed as content of an internal table.
    "Based on an ID, the customer data is retrieved.
    SELECT SINGLE *
      FROM @zcl_demo_abap_oodp_flyweight=>customer_data AS tab
      WHERE customer_id = @id
      INTO @customer_info.

    "For simplicity, this example only covers the best case
    "scenario where actual data is avaialble.
    IF customer_info IS INITIAL.
      ASSERT 1 = 0.
    ENDIF.
  ENDMETHOD.

  METHOD get_customer_info.
    customer_info = me->customer_info.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete flyweight class
*&---------------------------------------------------------------------*

CLASS lcl_flyweight DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_flyweight.
    METHODS constructor IMPORTING text TYPE string.
  PRIVATE SECTION.
    DATA text TYPE string.
ENDCLASS.

CLASS lcl_flyweight IMPLEMENTATION.
  METHOD constructor.
    me->text = text.
  ENDMETHOD.

  METHOD lif_flyweight~notify.
    DATA(customer_info) = customer_ref->get_customer_info( ).

    msg = VALUE #( ( |Hallo, { customer_info-name }. { me->text }| )
                   ( |Customer info: ID: { customer_info-customer_id }, Name: { customer_info-name }, Birthday: { customer_info-birthday }, | &&
                     |Address: { customer_info-address }, Email: { customer_info-email }| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Flyweight factory class
*&---------------------------------------------------------------------*

CLASS lcl_flyweight_factory DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS get_flyweight_object IMPORTING event      TYPE string
                                       RETURNING VALUE(obj) TYPE REF TO lif_flyweight.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_flyweight,
             evt    TYPE string,
             fw_obj TYPE REF TO lif_flyweight,
           END OF ts_flyweight,
           tt_flyweight TYPE HASHED TABLE OF ts_flyweight WITH UNIQUE KEY evt.
    CLASS-DATA flyweight_tab TYPE tt_flyweight.
ENDCLASS.

CLASS lcl_flyweight_factory IMPLEMENTATION.
  METHOD get_flyweight_object.
    READ TABLE flyweight_tab WITH TABLE KEY evt = event INTO DATA(wa).
    IF sy-subrc = 0.
      obj = wa-fw_obj.
      "Adding information to a log table for display purposes.
      APPEND |Entry exists for key "{ event }". Object reused.| TO zcl_demo_abap_oodp_flyweight=>log.
    ELSE.
      CASE event.
        WHEN `SIGNUP`.
          obj = NEW lcl_flyweight( text = `Thanks for signing up.` ).
        WHEN `ORDER`.
          obj = NEW lcl_flyweight( text = `Your order has been confirmed.` ).
        WHEN `SHIPPING`.
          obj = NEW lcl_flyweight( text = `Your order has been shipped.` ).
        WHEN `NEW_YEAR`.
          obj = NEW lcl_flyweight( text = `Happy New Year to you!` ).
        WHEN OTHERS.
          "For simplicity, this example only covers the best case
          "scenario where only supported events are processed.
          "Here might go exception raising for unsupported events.
          ASSERT 1 = 0.
      ENDCASE.

      INSERT VALUE #( evt = event fw_obj = obj ) INTO TABLE flyweight_tab.

      "Adding information to a log table for display purposes.
      APPEND |Entry does not exist for key "{ event }". New object created and added to the internal table.| TO zcl_demo_abap_oodp_flyweight=>log.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
