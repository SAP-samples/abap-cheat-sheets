CLASS lhc_zr_demo_abap96 DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS: check_log FOR MODIFY
        IMPORTING keys FOR ACTION demo_abap~check_log,
      get_instance_authorizations FOR INSTANCE AUTHORIZATION
        IMPORTING keys REQUEST requested_authorizations FOR demo_abap RESULT result,
       get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR demo_abap RESULT result,
    det_save FOR DETERMINE ON SAVE
      IMPORTING keys FOR demo_abap~det_save.
ENDCLASS.

CLASS lhc_zr_demo_abap96 IMPLEMENTATION.

  METHOD check_log.

*&---------------------------------------------------------------------*
*& This handler method implementation for a static action selects
*& from the database table that is used as log table for information
*& that is stored there on raising local business events.
*& In the simplified example, the information is retrieved here to
*& demonstrate the effect of the event raising.
*& The functionality for retrieving entries from the database table
*& is outsourced to the ABP auxiliary class.
*& The simplified method implementation adds information to reported.
*& On button click in the UI, the log database table entries are
*& displayed, allowing you to display the flow of event raising, both
*& in terms of when they were raised, for which UUID (only partly
*& displayed) and the event kind (a string value: CREATED,
*& UPDATED or DELETED).
*&---------------------------------------------------------------------*

    zcl_demo_abap_abp_aux_cl96=>get_db_entries(
      IMPORTING
        count      = DATA(count)
        db_entries = DATA(itab)
    ).

    APPEND new_message_with_text( severity = if_abap_behv_message=>severity-information
                                  text = |Current entries in the log database table: { count }| ) TO reported-%other.

    LOOP AT itab INTO DATA(wa) STEP -1.
      APPEND new_message_with_text( severity = if_abap_behv_message=>severity-information
                                    "The entire ID should not be displayed (only the last 7 characters of the UUID value)
                                    text = |{ wa-text }: ...{ match( val = CONV string( wa-instance_uuid ) pcre = `.{7}$` ) } ({ wa-timestamp })| ) TO reported-%other.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_instance_authorizations.

*&---------------------------------------------------------------------*
*& As an alternative to authorization exploration using the
*& authorization object and to quickly explore the functionality,
*& you can comment in the following code and comment out the rest to
*& have an oversimplified option to check on the authorization
*& functionality. For the self-contained demo, boolean values are
*& used as indicator for enabling "operation execution.
*& You can comment out and in to explore different behavior.
*&---------------------------------------------------------------------*

*    CONSTANTS update_allowed TYPE abap_boolean VALUE  abap_true.
*    "CONSTANTS update_allowed TYPE abap_boolean VALUE abap_false.
*
*    CONSTANTS delete_allowed TYPE abap_boolean VALUE abap_true.
*    "CONSTANTS delete_allowed TYPE abap_boolean VALUE abap_false.
*
*    "Retrieving instances based on requested keys
*    READ ENTITIES OF zr_demo_abap96 IN LOCAL MODE
*      ENTITY demo_abap
*      FIELDS ( country ) WITH CORRESPONDING #( keys )
*      RESULT DATA(read_result)
*      FAILED failed.
*
*    "If read result is initial, stop further method execution.
*    CHECK read_result IS NOT INITIAL.
*
*    LOOP AT read_result INTO DATA(r).
*
*      "Authorization check for update operations
*      IF requested_authorizations-%update =  if_abap_behv=>mk-on.
*        IF update_allowed = abap_false.
*          APPEND VALUE #( %tky    = r-%tky
*                          %update = if_abap_behv=>mk-on
*                          %msg    = new_message_with_text(
*                          severity = if_abap_behv_message=>severity-error
*                          text     = |Update operation for "{ r-country }" not authorized.| )
*          ) TO reported-demo_abap.
*        ENDIF.
*      ENDIF.
*
*      IF requested_authorizations-%delete = if_abap_behv=>mk-on.
*        IF delete_allowed = abap_false.
*          APPEND VALUE #( %tky    = r-%tky
*                          %delete = if_abap_behv=>mk-on
*                          %msg    = new_message_with_text(
*                          severity = if_abap_behv_message=>severity-error
*                          text     = |Delete operation for "{ r-country }" not authorized.| )
*          ) TO reported-demo_abap.
*        ENDIF.
*      ENDIF.
*
*      APPEND VALUE #( LET upd_auth = COND #( WHEN update_allowed = abap_true
*                                             THEN if_abap_behv=>auth-allowed
*                                             ELSE if_abap_behv=>auth-unauthorized )
*                          del_auth = COND #( WHEN delete_allowed = abap_true
*                                             THEN if_abap_behv=>auth-allowed
*                                             ELSE if_abap_behv=>auth-unauthorized )
*                      IN %tky    = r-%tky
*                         %update = upd_auth
*                         %delete = del_auth
*                    ) TO result.
*    ENDLOOP.

**********************************************************************

*&---------------------------------------------------------------------*
*& The example is intended to show authorization granting on the
*& basis of a demo authorization object. If the required steps to
*& assign the authorization to your user have not (yet) been
*& performed, you can use the simplified implementation as an
*& alternative to still be able to explore authorization
*& functionality. The example instance authorization determines that
*& only instances whose country value falls in a specific value range
*& can be updated and/or deleted. Others cannot be updated and/or
*& deleted.
*&---------------------------------------------------------------------*

    DATA update_allowed TYPE abap_boolean.
    DATA delete_allowed TYPE abap_boolean.

    "Retrieving instances based on requested keys
    READ ENTITIES OF zr_demo_abap96 IN LOCAL MODE
      ENTITY demo_abap
      FIELDS ( country ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.

    CHECK read_result IS NOT INITIAL.

    LOOP AT read_result INTO DATA(r).

      "Authorization check for update operations
      IF requested_authorizations-%update =  if_abap_behv=>mk-on.
        "Note: In draft scenarios, the Edit action is treated like an update operation.
        "So, you can add another condition such as ... OR requested_authorizations-%action-Edit =  if_abap_behv=>mk-on.

        AUTHORITY-CHECK OBJECT 'ZAUTHOBB96'
          ID 'ZAUTHFLB96' FIELD r-country
          ID 'ACTVT'      FIELD '02'.

        IF sy-subrc = 0.
          update_allowed = abap_true.
        ELSE.
          update_allowed = abap_false.

          APPEND VALUE #( %tky    = r-%tky
                          %update = if_abap_behv=>mk-on
                          %msg    = new_message_with_text(
                          severity = if_abap_behv_message=>severity-error
                          text     = |Update operation for "{ r-country }" not authorized.| )
          ) TO reported-demo_abap.
        ENDIF.
      ENDIF.

      IF requested_authorizations-%delete = if_abap_behv=>mk-on.

       AUTHORITY-CHECK OBJECT 'ZAUTHOBB96'
        ID 'ZAUTHFLB96' FIELD r-country
        ID 'ACTVT'      FIELD '06'.

        IF sy-subrc = 0.
          delete_allowed = abap_true.
        ELSE.
          delete_allowed = abap_false.

          APPEND VALUE #( %tky    = r-%tky
                          %delete = if_abap_behv=>mk-on
                          %msg    = new_message_with_text(
                          severity = if_abap_behv_message=>severity-error
                          text     = |Delete operation for "{ r-country }" not authorized.| )
          ) TO reported-demo_abap.

        ENDIF.
      ENDIF.

      APPEND VALUE #( LET upd_auth = COND #( WHEN update_allowed = abap_true
                                             THEN if_abap_behv=>auth-allowed
                                             ELSE if_abap_behv=>auth-unauthorized )
                          del_auth = COND #( WHEN delete_allowed = abap_true
                                             THEN if_abap_behv=>auth-allowed
                                             ELSE if_abap_behv=>auth-unauthorized )
                      IN %tky    = r-%tky
                         %update = upd_auth
                         %delete = del_auth
                    ) TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_global_authorizations.

*&---------------------------------------------------------------------*
*& As an alternative to authorization exploration using the
*& authorization object and to quickly explore the functionality,
*& you can comment in the following code and comment out the rest to
*& have an oversimplified option to check on the authorization
*& functionality. For the self-contained demo, boolean values are
*& used as indicator for enabling "operation execution.
*& You can comment out and in to explore different behavior.
*&---------------------------------------------------------------------*

*    CONSTANTS create_allowed TYPE abap_boolean VALUE abap_true.
*    "CONSTANTS create_allowed TYPE abap_boolean VALUE abap_false.
*
*    CONSTANTS update_allowed TYPE abap_boolean VALUE abap_true.
*    "CONSTANTS update_allowed TYPE abap_boolean VALUE abap_false.
*
*    CONSTANTS delete_allowed TYPE abap_boolean VALUE abap_true.
*    "CONSTANTS delete_allowed TYPE abap_boolean VALUE abap_false.
*
*    IF requested_authorizations-%create = if_abap_behv=>mk-on.
*
*      IF create_allowed = abap_true.
*        result-%create = if_abap_behv=>auth-allowed.
*      ELSE.
*        result-%create = if_abap_behv=>auth-unauthorized.
*
*        APPEND VALUE #( %global = if_abap_behv=>mk-on
*                        %create = if_abap_behv=>mk-on
*                        %update = if_abap_behv=>mk-on
*                        %delete = if_abap_behv=>mk-on
*                        %msg    = new_message_with_text(
*                        severity = if_abap_behv_message=>severity-error
*                        text     = 'Create operation not authorized.' )
*                      ) TO reported-demo_abap.
*      ENDIF.
*    ENDIF.
*
*    IF requested_authorizations-%update =  if_abap_behv=>mk-on.
*      IF update_allowed = abap_true.
*        result-%update = if_abap_behv=>auth-allowed.
*      ELSE.
*        result-%update = if_abap_behv=>auth-unauthorized.
*
*        APPEND VALUE #( %global = if_abap_behv=>mk-on
*                        %update = if_abap_behv=>mk-on
*                        %create = if_abap_behv=>mk-on
*                        %delete = if_abap_behv=>mk-on
*                        %msg    = new_message_with_text(
*                        severity = if_abap_behv_message=>severity-error
*                        text     = 'Update operation not authorized.' )
*                      ) TO reported-demo_abap.
*      ENDIF.
*    ENDIF.
*
*    IF requested_authorizations-%delete = if_abap_behv=>mk-on.
*
*      IF delete_allowed = abap_true.
*
*        result-%delete = if_abap_behv=>auth-allowed.
*      ELSE.
*        result-%delete = if_abap_behv=>auth-unauthorized.
*
*        APPEND VALUE #( %global = if_abap_behv=>mk-on
*                        %delete = if_abap_behv=>mk-on
*                        %create = if_abap_behv=>mk-on
*                        %update = if_abap_behv=>mk-on
*                        %msg    = new_message_with_text(
*                        severity = if_abap_behv_message=>severity-error
*                        text     = 'Delete operation not authorized.' )
*                      ) TO reported-demo_abap.
*      ENDIF.
*    ENDIF.

**********************************************************************

    "Authorization check for create operations
    IF requested_authorizations-%create = if_abap_behv=>mk-on.

      AUTHORITY-CHECK OBJECT 'ZAUTHOBB96'
        ID 'ZAUTHFLB96' DUMMY
        ID 'ACTVT'      FIELD '01'.

      IF sy-subrc = 0.
        result-%create = if_abap_behv=>auth-allowed.
      ELSE.
        result-%create = if_abap_behv=>auth-unauthorized.

        APPEND VALUE #( %global = if_abap_behv=>mk-on
                              %msg      = new_message_with_text(
                                severity  = if_abap_behv_message=>severity-error
                                text      = 'Create operation not authorized.' )
                          ) TO reported-demo_abap.
      ENDIF.
    ENDIF.

    "Authorization check for update operations
    IF requested_authorizations-%update =  if_abap_behv=>mk-on.
      "Note: In draft scenarios, the Edit action is treated like an update operation.
      "So, you can add another condition such as ... OR requested_authorizations-%action-Edit =  if_abap_behv=>mk-on.

      AUTHORITY-CHECK OBJECT 'ZAUTHOBB96'
           ID 'ZAUTHFLB96' DUMMY
           ID 'ACTVT'      FIELD '02'.

      IF sy-subrc = 0.
        result-%update = if_abap_behv=>auth-allowed.
      ELSE.
        result-%update = if_abap_behv=>auth-unauthorized.

        APPEND VALUE #( %global = if_abap_behv=>mk-on
                              %msg      = new_message_with_text(
                                severity  = if_abap_behv_message=>severity-error
                                text      = 'Update operation not authorized.' )
                          ) TO reported-demo_abap.
      ENDIF.
    ENDIF.

    "Authorization check for delete operations
    IF requested_authorizations-%delete = if_abap_behv=>mk-on.

      AUTHORITY-CHECK OBJECT 'ZAUTHOBB96'
          ID 'ZAUTHFLB96' DUMMY
          ID 'ACTVT'      FIELD '06'.

      IF sy-subrc = 0.
        result-%delete = if_abap_behv=>auth-allowed.
      ELSE.
        result-%delete = if_abap_behv=>auth-unauthorized.

        APPEND VALUE #( %global = if_abap_behv=>mk-on
                              %msg      = new_message_with_text(
                                severity  = if_abap_behv_message=>severity-error
                                text      = 'Delete operation not authorized.' )
                          ) TO reported-demo_abap.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD det_save.

*&---------------------------------------------------------------------*
*& The determination implementation serves a specific demo purpose:
*& Providing a value for the country field.
*& The simplified demo just adds values depending on the num1 field
*& value. The purpose is to have a simple option to check out
*& instance authorization regarding the country field value.
*&---------------------------------------------------------------------*

    "Retrieving instances based on requested keys via the ABP auxiliary class
    zcl_demo_abap_abp_aux_cl96=>read_instances(
      EXPORTING
        keys        = keys
      RECEIVING
        read_result = DATA(read_result)
    ).

    CHECK read_result IS NOT INITIAL.

    DATA upd_tab TYPE TABLE FOR UPDATE zr_demo_abap96.
    LOOP AT read_result ASSIGNING FIELD-SYMBOL(<u>).
      <u>-Country = COND #( WHEN <u>-Num1 BETWEEN 0 AND 10 THEN zcl_demo_abap_abp_aux_cl96=>at
                            WHEN <u>-Num1 BETWEEN 11 AND 20 THEN zcl_demo_abap_abp_aux_cl96=>ch
                            WHEN <u>-Num1 BETWEEN 21 AND 30 THEN zcl_demo_abap_abp_aux_cl96=>de
                            WHEN <u>-Num1 BETWEEN 31 AND 40 THEN zcl_demo_abap_abp_aux_cl96=>es
                            WHEN <u>-Num1 BETWEEN 41 AND 50 THEN zcl_demo_abap_abp_aux_cl96=>fr
                            WHEN <u>-Num1 BETWEEN 51 AND 60 THEN zcl_demo_abap_abp_aux_cl96=>it
                            WHEN <u>-Num1 BETWEEN 61 AND 70 THEN zcl_demo_abap_abp_aux_cl96=>pt
                            WHEN <u>-Num1 BETWEEN 71 AND 80 THEN zcl_demo_abap_abp_aux_cl96=>ro
                            WHEN <u>-Num1 BETWEEN 81 AND 90 THEN zcl_demo_abap_abp_aux_cl96=>se
                            ELSE zcl_demo_abap_abp_aux_cl96=>sk ).
      APPEND VALUE #( %tky = <u>-%tky country = <u>-Country ) TO upd_tab.
    ENDLOOP.

    MODIFY ENTITIES OF zr_demo_abap96 IN LOCAL MODE
      ENTITY demo_abap
      UPDATE FIELDS ( Country ) WITH upd_tab.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_zr_demo_abap96 DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zr_demo_abap96 IMPLEMENTATION.

  METHOD save_modified.

*&---------------------------------------------------------------------*
*& The save_modified implementation includes the raising of three
*& local business events on create, update, and delete.
*& The latter ones include a parameter in the event specification.
*& The current timestamp is provided along with text illustrating
*& which event was raised.
*&---------------------------------------------------------------------*

    DATA(ts) = utclong_current( ).

    IF create-demo_abap IS NOT INITIAL.
      RAISE ENTITY EVENT zr_demo_abap96~created
        FROM VALUE #( FOR <cr> IN create-demo_abap (
                      %key = VALUE #( uuid = <cr>-uuid ) ) ).
    ENDIF.

    IF update-demo_abap IS NOT INITIAL.
      DATA evt_tab_up TYPE TABLE FOR EVENT zr_demo_abap96~updated.

      evt_tab_up = VALUE #( FOR <up> IN update-demo_abap (
                            %key   = VALUE #( uuid      = <up>-uuid )
                            %param = VALUE #( text      = 'UPDATED'
                                              timestamp = ts ) ) ).
      RAISE ENTITY EVENT zr_demo_abap96~updated FROM evt_tab_up.
    ENDIF.

    IF delete-demo_abap IS NOT INITIAL.
      RAISE ENTITY EVENT zr_demo_abap96~deleted
        FROM VALUE #( FOR <del> IN delete-demo_abap (
                      %key   = VALUE #( uuid      = <del>-uuid )
                      %param = VALUE #( text      = 'DELETED'
                                        timestamp = ts ) ) ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
