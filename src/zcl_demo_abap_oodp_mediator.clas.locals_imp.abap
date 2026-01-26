*&---------------------------------------------------------------------*
*& Mediator interface
*&---------------------------------------------------------------------*

INTERFACE lif_mediator.
  TYPES: BEGIN OF ENUM ty_enum,
           item_added,
           item_reserved,
           item_removed,
           payment_done,
           notification,
         END OF ENUM ty_enum.

  METHODS: notify IMPORTING oref  TYPE REF TO object
                            event TYPE ty_enum
                            text  TYPE string OPTIONAL,
           log IMPORTING text TYPE string.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Interface for object group members
*&---------------------------------------------------------------------*

INTERFACE lif_member.
  METHODS set_mediator IMPORTING mediator_oref TYPE REF TO lif_mediator.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Member class 1 (Shopping cart)
*&---------------------------------------------------------------------*

CLASS lcl_shopping_cart DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_member.
    METHODS: add_product    IMPORTING product TYPE string,
      remove_product IMPORTING product TYPE string,
      get_cart       RETURNING VALUE(shopping_cart) TYPE string_table.
  PRIVATE SECTION.
    DATA: mediator  TYPE REF TO lif_mediator,
          shop_cart TYPE string_table,
          cl        TYPE string.
ENDCLASS.

CLASS lcl_shopping_cart IMPLEMENTATION.
  METHOD lif_member~set_mediator.
    mediator = mediator_oref.
    cl = cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ).
  ENDMETHOD.

  METHOD add_product.
    APPEND product TO shop_cart.

    IF mediator IS BOUND.
      mediator->log( |{ cl }: Product "{ product }" added to cart.| ).
      mediator->notify( oref  = me
                        event = lif_mediator=>item_added
                        text  = product ).
    ENDIF.
  ENDMETHOD.

  METHOD remove_product.
    DELETE shop_cart WHERE table_line = product.

    IF mediator IS BOUND.
      mediator->log( |{ cl }: Product "{ product }" removed from cart.|  ).
      mediator->notify( oref  = me
                        event = lif_mediator=>item_removed
                        text  = product ).
    ENDIF.
  ENDMETHOD.

  METHOD get_cart.
    shopping_cart = shop_cart.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Member class 2 (Payment Processor)
*&---------------------------------------------------------------------*

CLASS lcl_payment DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_member.
    METHODS process_payment IMPORTING products TYPE string_table.
  PRIVATE SECTION.
    DATA: mediator TYPE REF TO lif_mediator,
          cl       TYPE string.
ENDCLASS.

CLASS lcl_payment IMPLEMENTATION.
  METHOD lif_member~set_mediator.
    mediator = mediator_oref.
    cl = cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ).
  ENDMETHOD.

  METHOD process_payment.
    IF products IS INITIAL.
      IF mediator IS BOUND.
        mediator->log( |{ cl }: No items to process payment for.|  ).
        RETURN.
      ENDIF.
    ENDIF.

    DATA(prod) = concat_lines_of( table = products sep = `, ` ).

    IF mediator IS BOUND.
      mediator->log( |{ cl }: Processing payment for { lines( products ) } products ({ prod }) ...|  ).
      mediator->log( |{ cl }: Payment processed successfully.|  ).
      mediator->notify( oref  = me
                        event = lif_mediator=>payment_done
                        text  = prod ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Member class 3 (Stock Manager)
*&---------------------------------------------------------------------*

CLASS lcl_stock DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_member.
    METHODS: reserve_stock IMPORTING product TYPE string,
      release_stock IMPORTING product TYPE string.
  PRIVATE SECTION.
    DATA: mediator TYPE REF TO lif_mediator,
          cl       TYPE string.
ENDCLASS.

CLASS lcl_stock IMPLEMENTATION.
  METHOD lif_member~set_mediator.
    mediator = mediator_oref.
    cl = cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ).
  ENDMETHOD.

  METHOD reserve_stock.
    IF mediator IS BOUND.
      mediator->log( |{ cl }: Inventory reserved for "{ product }".|  ).
      mediator->notify( oref  = me
                        event = lif_mediator=>item_reserved
                        text  = product ).
    ENDIF.
  ENDMETHOD.

  METHOD release_stock.
    IF mediator IS BOUND.
      mediator->log( |{ cl }: Inventory released for "{ product }".|  ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Member class 4 (Notification Service)
*&---------------------------------------------------------------------*

CLASS lcl_notification DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_member.
    METHODS send_confirmation.
  PRIVATE SECTION.
    DATA: mediator TYPE REF TO lif_mediator,
          cl       TYPE string.
ENDCLASS.

CLASS lcl_notification IMPLEMENTATION.
  METHOD lif_member~set_mediator.
    mediator = mediator_oref.
    cl = cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ).
  ENDMETHOD.

  METHOD send_confirmation.
    IF mediator IS BOUND.
      mediator->log( |{ cl }: Order confirmation sent to customer.|  ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete Mediator
*&---------------------------------------------------------------------*

CLASS lcl_mediator DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_mediator.
    METHODS: constructor IMPORTING prod_m   TYPE REF TO lcl_shopping_cart
                                   pay_proc TYPE REF TO lcl_payment
                                   inv_m    TYPE REF TO lcl_stock
                                   notif    TYPE REF TO lcl_notification,
      checkout,
      get_log RETURNING VALUE(log) TYPE string_table.
  PRIVATE SECTION.
    DATA: product_manager   TYPE REF TO lcl_shopping_cart,
          payment_processor TYPE REF TO lcl_payment,
          inventory_manager TYPE REF TO lcl_stock,
          notification      TYPE REF TO lcl_notification,
          cl                TYPE string,
          prod              TYPE string_table,
          log_tab           TYPE string_table.
ENDCLASS.

CLASS lcl_mediator IMPLEMENTATION.
  METHOD constructor.
    product_manager   = prod_m.
    payment_processor = pay_proc.
    inventory_manager = inv_m.
    notification  = notif.

    product_manager->lif_member~set_mediator( me ).
    payment_processor->lif_member~set_mediator( me ).
    inventory_manager->lif_member~set_mediator( me ).
    notification->lif_member~set_mediator( me ).

    cl = cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ).
  ENDMETHOD.

  METHOD lif_mediator~notify.
    CASE event.
      WHEN lif_mediator=>item_added.
        lif_mediator~log( |{ cl }: Reserving stock ...|  ).

        IF inventory_manager IS BOUND.
          inventory_manager->reserve_stock( text ).
        ENDIF.
      WHEN lif_mediator=>item_reserved.
        lif_mediator~log( |{ cl }: Stock confirmed for "{ text }".| ).

        APPEND text TO prod.
      WHEN lif_mediator=>item_removed.
        lif_mediator~log( |{ cl }: Releasing inventory ...| ).

        IF inventory_manager IS BOUND.
          inventory_manager->release_stock( text ).
        ENDIF.

        DELETE prod WHERE table_line = text.
      WHEN lif_mediator=>payment_done.
        lif_mediator~log( |{ cl }: Payment completed, sending confirmation.| ).

        IF notification IS BOUND.
          notification->send_confirmation( ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD checkout.
    IF prod IS INITIAL.
      lif_mediator~log( |{ cl }: No items to checkout. Exiting.| ).
      RETURN.
    ENDIF.

    lif_mediator~log( |{ cl }: Initiating checkout ...| ).

    IF payment_processor IS BOUND.
      payment_processor->process_payment( prod ).
    ENDIF.
  ENDMETHOD.

  METHOD get_log.
    log = log_tab.
  ENDMETHOD.

  METHOD lif_mediator~log.
    APPEND text TO log_tab.
  ENDMETHOD.
ENDCLASS.
