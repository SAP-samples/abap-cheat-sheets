CLASS lhe_event DEFINITION INHERITING FROM cl_abap_behavior_event_handler.

  PRIVATE SECTION.

    METHODS on_updated FOR ENTITY EVENT
       updated FOR root~updated.

    METHODS on_deleted FOR ENTITY EVENT
       deleted FOR root~deleted.

    METHODS on_created FOR ENTITY EVENT
       created FOR root~created.

    DATA evt_log TYPE TABLE OF zdemo_abap_draft WITH EMPTY KEY.
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
                          draftuuid  = cl_system_uuid=>create_uuid_x16_static( )
                          calc_result    = |Instance key: "{ <created>-id }" / Event CREATED raised|
                          crea_date_time = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) )
                        ) TO evt_log.
        CATCH cx_uuid_error INTO DATA(err).
          ASSERT err IS INITIAL.
      ENDTRY.
      MODIFY zdemo_abap_draft FROM TABLE @evt_log.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_updated.
    cl_abap_tx=>save( ).
    LOOP AT updated  ASSIGNING FIELD-SYMBOL(<updated>).
      TRY.
          APPEND VALUE #( id         = cl_system_uuid=>create_uuid_x16_static( )
                          draftuuid  = cl_system_uuid=>create_uuid_x16_static( )
                          calc_result    = |Instance key: "{ <updated>-id }" / %param: col1: "{ <updated>-%param-col1 }" col2: "{ <updated>-%param-col2 }"|
                          crea_date_time = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) )
                        ) TO evt_log.
        CATCH cx_uuid_error INTO DATA(err).
          ASSERT err IS INITIAL.
      ENDTRY.
      MODIFY zdemo_abap_draft FROM TABLE @evt_log.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_deleted.
    cl_abap_tx=>save( ).
    LOOP AT deleted ASSIGNING FIELD-SYMBOL(<deleted>).
      TRY.
          APPEND VALUE #( id         = cl_system_uuid=>create_uuid_x16_static( )
                          draftuuid  = cl_system_uuid=>create_uuid_x16_static( )
                          calc_result    = |Instance key: "{ <deleted>-id }" / %param: col1: "{ <deleted>-%param-col1 }" col2: "{ <deleted>-%param-col2 }"|
                          crea_date_time = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) )
                        ) TO evt_log.
        CATCH cx_uuid_error INTO DATA(err).
          ASSERT err IS INITIAL.
      ENDTRY.
      MODIFY zdemo_abap_draft FROM TABLE @evt_log.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
