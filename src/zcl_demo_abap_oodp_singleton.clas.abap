"! <p class="shorttext"><strong>Singleton</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the singleton design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_singleton DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    "Internal table to store and display object reference variable names and
    "time stamp values
    TYPES: BEGIN OF s_ts,
             name               TYPE string,
             timestamp_static   TYPE utclong,
             timestamp_instance TYPE utclong,
           END OF s_ts.
    DATA ts_tab TYPE TABLE OF s_ts WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_demo_abap_oodp_singleton IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Singleton` ).

    "Object creation as follows is not possible
    "DATA(oref) = NEW lcl_singleton( ).

    "Creating object
    DATA(oref1) = lcl_singleton=>get_obj( ).

    "Retrieving time stamps, and adding the values to the internal table created above
    "for display purposes
    oref1->get_timestamps( IMPORTING ts_static   = DATA(ts_static)
                                     ts_instance = DATA(ts_instance) ).
    APPEND VALUE #( name = `oref1` timestamp_static = ts_static timestamp_instance = ts_instance ) TO ts_tab.

    "Adding entries to a log table (represented by a private static attribute in the local class)
    oref1->add_log( |Text 1 added at { utclong_current( ) } (using oref1)| ).
    oref1->add_log( |Text 2 added at { utclong_current( ) } (using oref1)| ).

    "Creating more objects (however, the one created previously is returned) and adding entries
    "to the log table
    "Time stamp values are also added the to the internal table for display purposes
    DATA(oref2) = lcl_singleton=>get_obj( ).

    oref2->get_timestamps( IMPORTING ts_static   = ts_static
                                     ts_instance = ts_instance ).
    APPEND VALUE #( name = `oref2` timestamp_static = ts_static timestamp_instance = ts_instance ) TO ts_tab.

    oref2->add_log( |Text 3 added at { utclong_current( ) } (using oref2)| ).
    oref2->add_log( |Text 4 added at { utclong_current( ) } (using oref2)| ).

    oref1->add_log( |Text 5 added at { utclong_current( ) } (using oref1)| ).

    DATA(oref3) = lcl_singleton=>get_obj( ).

    oref3->get_timestamps( IMPORTING ts_static   = ts_static
                                     ts_instance = ts_instance ).
    APPEND VALUE #( name = `oref3` timestamp_static = ts_static timestamp_instance = ts_instance ) TO ts_tab.

    oref3->add_log( |Text 6 added at { utclong_current( ) } (using oref3)| ).
    oref3->add_log( |Text 7 added at { utclong_current( ) } (using oref3)| ).

    oref1->add_log( |Text 8 added at { utclong_current( ) } (using oref1)| ).
    oref2->add_log( |Text 9 added at { utclong_current( ) } (using oref2)| ).
    oref3->add_log( |Text 10 added at { utclong_current( ) } (using oref3)| ).

    DATA(oref4) = lcl_singleton=>get_obj( ).

    oref4->get_timestamps( IMPORTING ts_static   = ts_static
                                     ts_instance = ts_instance ).
    APPEND VALUE #( name = `oref4` timestamp_static = ts_static timestamp_instance = ts_instance ) TO ts_tab.

    oref4->add_log( |Text 11 added at { utclong_current( ) } (using oref4)| ).
    oref4->add_log( |Text 12 added at { utclong_current( ) } (using oref4)| ).

    "Retrieving the content of the log table per object
    "However, as it is one and the same object that is dealt with, the content is the same.
    DATA(log1) = oref1->get_log( ).
    DATA(log2) = oref2->get_log( ).
    DATA(log3) = oref3->get_log( ).

    out->write( log1 ).
    out->write( |\n| ).

    ASSERT log1 = log2.
    ASSERT log1 = log3.

    "Displaying the time stamps visualizing the singleton pattern
    SORT ts_tab BY name ASCENDING.
    out->write( ts_tab ).

  ENDMETHOD.

ENDCLASS.
