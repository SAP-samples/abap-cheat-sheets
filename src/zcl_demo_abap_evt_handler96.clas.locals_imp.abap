CLASS lhe_event DEFINITION INHERITING FROM cl_abap_behavior_event_handler.

  PRIVATE SECTION.

    METHODS on_updated FOR ENTITY EVENT
       updated FOR demo_abap~updated.

    METHODS on_deleted FOR ENTITY EVENT
       deleted FOR demo_abap~deleted.

    METHODS on_created FOR ENTITY EVENT
       created FOR demo_abap~created.

    DATA evt_log TYPE TABLE OF zdemoabaplog96 WITH EMPTY KEY.
ENDCLASS.

CLASS lhe_event IMPLEMENTATION.

  "Note:
  "- For this example, database table entries are created for the individual
  "  RAP BO instances that are imported into the event handler methods.
  "- The transactional phases are implicitly set when RAP business events are
  "  consumed locally. This means that RAP event handler methods are started in
  "  the modify phase when called. If database modifications are to be implemented
  "  in RAP event handler methods, you must explicitly activate the save phase to
  "  avoid causing errors detected by the controlled SAP LUW.

  METHOD on_created.
    cl_abap_tx=>save( ).
    LOOP AT created ASSIGNING FIELD-SYMBOL(<created>).
      TRY.
          APPEND VALUE #( id         = cl_system_uuid=>create_uuid_x16_static( )
                          instance_uuid = <created>-uuid
                          text       = 'CREATED'
                          timestamp  = utclong_current( ) ) TO evt_log.
        CATCH cx_uuid_error.
          ASSERT 1 = 2.
      ENDTRY.
      MODIFY zdemoabaplog96 FROM TABLE @evt_log.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_updated.
    cl_abap_tx=>save( ).
    LOOP AT updated  ASSIGNING FIELD-SYMBOL(<updated>).
      TRY.
          APPEND VALUE #( id         = cl_system_uuid=>create_uuid_x16_static( )
                         instance_uuid = <updated>-uuid
                         text       = <updated>-%param-text
                         timestamp  = <updated>-%param-timestamp ) TO evt_log.
        CATCH cx_uuid_error.
          ASSERT 1 = 2.
      ENDTRY.
      MODIFY zdemoabaplog96 FROM TABLE @evt_log.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_deleted.
    cl_abap_tx=>save( ).
    LOOP AT deleted ASSIGNING FIELD-SYMBOL(<deleted>).
      TRY.
          APPEND VALUE #( id         = cl_system_uuid=>create_uuid_x16_static( )
                          instance_uuid = <deleted>-uuid
                          text       = <deleted>-%param-text
                          timestamp  = <deleted>-%param-timestamp ) TO evt_log.
        CATCH cx_uuid_error.
          ASSERT 1 = 2.
      ENDTRY.
      MODIFY zdemoabaplog96 FROM TABLE @evt_log.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
