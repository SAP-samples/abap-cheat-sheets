"! <p class="shorttext"><strong>Mediator</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the mediator design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_mediator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA example_number TYPE i.
ENDCLASS.


CLASS zcl_demo_abap_oodp_mediator IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Mediator` ).

*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Purchasing products| ).
    out->write( |\n| ).

    "Creating instances of concrete component classes
    DATA(product_manager) = NEW lcl_shopping_cart( ).
    DATA(payment_processor) = NEW lcl_payment( ).
    DATA(stock_manager) = NEW lcl_stock( ).
    DATA(notification_service) = NEW lcl_notification( ).

    "Creating mediator instance and link component classes
    DATA(shopping_mediator) = NEW lcl_mediator( prod_m   = product_manager
                                                pay_proc = payment_processor
                                                inv_m    = stock_manager
                                                notif    = notification_service ).

    "Adding products to the shopping cart
    product_manager->add_product( `Laptop` ).
    product_manager->add_product( `Mouse` ).
    product_manager->add_product( `Keyboard` ).

    "Displaying the contents of the current shopping cart
    out->write( |Contents of shopping cart { example_number })| ).
    DATA(cart_items) = product_manager->get_cart( ).
    LOOP AT cart_items INTO DATA(item).
      out->write( |  - { item }| ).
    ENDLOOP.
    out->write( |\n| ).

    "Triggering the checkout for processing the payment and
    "sending confirmation
    shopping_mediator->checkout( ).

    "Displaying the log
    out->write( `Log:` ).
    DATA(log) = shopping_mediator->get_log( ).
    out->write( log ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Adding and removing products| ).
    out->write( |\n| ).

    product_manager = NEW lcl_shopping_cart( ).
    payment_processor = NEW lcl_payment( ).
    stock_manager = NEW lcl_stock( ).
    notification_service = NEW lcl_notification( ).

    "Creating mediator instance and link component classes
    shopping_mediator = NEW lcl_mediator( prod_m   = product_manager
                                          pay_proc = payment_processor
                                          inv_m    = stock_manager
                                          notif    = notification_service ).

    product_manager->add_product( `Smartphone` ).
    product_manager->add_product( `Headphones` ).
    product_manager->add_product( `Phone Case` ).

    "Removing an item from the shopping cart
    product_manager->remove_product( `Headphones` ).

    out->write( |Contents of shopping cart { example_number })| ).
    cart_items = product_manager->get_cart( ).
    LOOP AT cart_items INTO item.
      out->write( |  - { item }| ).
    ENDLOOP.
    out->write( |\n| ).

    shopping_mediator->checkout( ).

    out->write( `Log:` ).
    log = shopping_mediator->get_log( ).
    out->write( log ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Example 3
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Empty shopping cart| ).
    out->write( |\n| ).

    product_manager = NEW lcl_shopping_cart( ).
    payment_processor = NEW lcl_payment( ).
    stock_manager = NEW lcl_stock( ).
    notification_service = NEW lcl_notification( ).

    "Creating mediator instance and link component classes
    shopping_mediator = NEW lcl_mediator( prod_m   = product_manager
                                          pay_proc = payment_processor
                                          inv_m    = stock_manager
                                          notif    = notification_service ).

    out->write( |Contents of shopping cart { example_number })| ).
    cart_items = product_manager->get_cart( ).
    LOOP AT cart_items INTO item.
      out->write( |  - { item }| ).
    ENDLOOP.
    out->write( |\n\n| ).

    shopping_mediator->checkout( ).

    out->write( `Log:` ).
    log = shopping_mediator->get_log( ).
    out->write( log ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Example 4
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Adding and removing multiple products| ).
    out->write( |\n| ).

    product_manager = NEW lcl_shopping_cart( ).
    payment_processor = NEW lcl_payment( ).
    stock_manager = NEW lcl_stock( ).
    notification_service = NEW lcl_notification( ).

    "Creating mediator instance and link component classes
    shopping_mediator = NEW lcl_mediator( prod_m   = product_manager
                                          pay_proc = payment_processor
                                          inv_m    = stock_manager
                                          notif    = notification_service ).

    "Adding and removing multiple products
    product_manager->add_product( `Monitor` ).
    product_manager->add_product( `USB Drive` ).
    product_manager->remove_product( `USB Drive` ).
    product_manager->add_product( `Smartphone` ).
    product_manager->add_product( `Headphones` ).
    product_manager->add_product( `Phone Case` ).
    product_manager->add_product( `Printer` ).
    product_manager->add_product( `Scanner` ).
    product_manager->remove_product( `Printer` ).
    product_manager->add_product( `Mouse` ).
    product_manager->remove_product( `Phone Case` ).

    out->write( |Contents of shopping cart { example_number })| ).
    cart_items = product_manager->get_cart( ).
    LOOP AT cart_items INTO item.
      out->write( |  - { item }| ).
    ENDLOOP.
    out->write( |\n\n| ).

    shopping_mediator->checkout( ).

    out->write( `Log:` ).
    log = shopping_mediator->get_log( ).
    out->write( log ).
  ENDMETHOD.
ENDCLASS.
