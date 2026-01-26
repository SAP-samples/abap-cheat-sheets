"! <p class="shorttext"><strong>Flyweight</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the flyweight design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_flyweight DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: BEGIN OF ts_customer_info,
             customer_id TYPE i,
             name        TYPE c LENGTH 30,
             email       TYPE c LENGTH 30,
             birthday    TYPE d,
             address     TYPE c LENGTH 50,
           END OF ts_customer_info.

    CLASS-METHODS class_constructor.

    CLASS-DATA: customer_data TYPE TABLE OF ts_customer_info WITH EMPTY KEY,
                log           TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap_oodp_flyweight IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Flyweight` ).

    DATA(customer_a) = NEW lcl_customer( 1 ).

    DATA(fw_a) = lcl_flyweight_factory=>get_flyweight_object( `SIGNUP` ).
    DATA(msg_a) = fw_a->notify( customer_a ).
    out->write( data = msg_a name = `msg_a` ).
    out->write( |\n| ).

    DATA(customer_b) = NEW lcl_customer( 2 ).
    DATA(fw_b) = lcl_flyweight_factory=>get_flyweight_object( `SIGNUP` ).
    DATA(msg_b) = fw_b->notify( customer_b ).
    out->write( data = msg_b name = `msg_b` ).
    out->write( |\n| ).

    DATA(fw_c) = lcl_flyweight_factory=>get_flyweight_object( `SIGNUP` ).
    DATA(msg_c) = fw_c->notify( NEW lcl_customer( 3 ) ).
    out->write( data = msg_c name = `msg_c` ).
    out->write( |\n| ).

    DATA(fw_d) = lcl_flyweight_factory=>get_flyweight_object( `ORDER` ).
    DATA(msg_d) = fw_d->notify( customer_a ).
    out->write( data = msg_d name = `msg_d` ).
    out->write( |\n| ).

    DATA(fw_e) = lcl_flyweight_factory=>get_flyweight_object( `ORDER` ).
    DATA(msg_e) = fw_e->notify( customer_b ).
    out->write( data = msg_e name = `msg_e` ).
    out->write( |\n| ).

    "Displaying log table
    out->write( log ).
    out->write( |\n| ).

    CLEAR log.

    "Checking out all events in the example
    DATA(event_tab) = VALUE string_table( ( `SIGNUP` )
                                          ( `ORDER` )
                                          ( `SHIPPING` )
                                          ( `NEW_YEAR` ) ).

    DATA msg_tab TYPE string_table.

    LOOP AT event_tab INTO DATA(evt).
      DO lines( customer_data ) TIMES.
        DATA(fw) = lcl_flyweight_factory=>get_flyweight_object( evt ).
        DATA(msg) = fw->notify( NEW lcl_customer( sy-index ) ).

        APPEND |-------------------- Example for event "{ evt }", customer ID "{ sy-index }" --------------------| TO msg_tab.
        APPEND LINES OF msg TO msg_tab.
        APPEND INITIAL LINE TO msg_tab.
      ENDDO.
    ENDLOOP.

    out->write( data = msg_tab name = `msg_tab` ).

    "Displaying log table
    out->write( log ).
  ENDMETHOD.

  METHOD class_constructor.
    customer_data = VALUE #(
      ( customer_id = 1 name = 'Jon Doe'       email = 'jon_doe@example.com'      birthday = '19801101' address = 'Some Street 1, London' )
      ( customer_id = 2 name = 'Jane Smith'    email = 'jane_smith@example.com'   birthday = '19900515' address = 'Another Ave 2, New York' )
      ( customer_id = 3 name = 'Alice Brown'   email = 'alice_brown@example.com'  birthday = '19870322' address = 'Red Road 3, Sydney' )
      ( customer_id = 4 name = 'Bob White'     email = 'bob_white@example.com'    birthday = '19751230' address = 'Blue Blvd 4, Paris' )
      ( customer_id = 5 name = 'Charlie Black' email = 'charlie_black@example.com' birthday = '19991111' address = 'Green Street 5, Berlin' )
      ( customer_id = 6 name = 'Diana Green'   email = 'diana_green@example.com'  birthday = '19860418' address = 'Yellow Lane 6, Tokyo' )
      ( customer_id = 7 name = 'Edward King'   email = 'edward_king@example.com'  birthday = '19930708' address = 'High Street 7, Toronto' )
      ( customer_id = 8 name = 'Fiona Blue'    email = 'fiona_blue@example.com'   birthday = '19820114' address = 'Blue Water 8, Auckland' )
      ( customer_id = 9 name = 'George Gray'   email = 'george_gray@example.com'  birthday = '19790925' address = 'Gray Path 9, Mumbai' )
      ( customer_id = 10 name = 'Hannah Gold'  email = 'hannah_gold@example.com'  birthday = '19881208' address = 'Golden Street 10, Cairo' ) ).
  ENDMETHOD.
ENDCLASS.
