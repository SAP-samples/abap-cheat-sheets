***********************************************************************
*
*          RAP BO provider (i. e. ABAP behavior pool/ABP)
*                  for a RAP demo scenario
*
* - RAP scenario: unmanaged RAP BO, external numbering
* - Data model: Consists of a root entity and one child entity. The BDEF
*   defines the behavior for these two entities which are connected via
*   a CDS composition relation. The definitions in the BDEF determine
*   which methods must be implemented in this ABAP behavior pool (ABP).
*
* ----------------------------- NOTE -----------------------------------
* This simplified example is not a real life scenario and rather
* focuses on the technical side by giving an idea how the communication
* and data exchange between a RAP BO consumer, which is a class
* in this case, and RAP BO provider can work. Additionally, it shows
* how the methods for non-standard RAP BO operations might be
* self-implemented in an ABP. The example focuses on specific RAP aspects.
* For this reason, the example might not fully meet the requirements of
* the RAP BO contract. Although this unmanaged scenario attempts to
* replicate some of the functionality of the managed example, the
* functionality is not fully consistent with the managed scenario.
*
* For demonstration purposes, some of the operations are
* impacted by feature controls and instance authorization as specified
* in the BDEF.
*
* The code presented in this class is intended only to support the ABAP
* cheat sheets. It is not intended for direct use in a production system
* environment. The code examples in the ABAP cheat sheets are primarily
* intended to provide a better explanation and visualization of the
* syntax and semantics of ABAP statements, not to solve concrete
* programming tasks. For production application programs, you should
* always work out your own solution for each individual case. There is
* no guarantee for the correctness or completeness of the code.
* Furthermore, there is no legal responsibility or liability for any
* errors or their consequences that may occur when using the the example
* code.
*
***********************************************************************

***********************************************************************
*             Class lcl_buffer
*
* To have a self-contained and simple scenario, the transactional
* buffer is realized by internal tables here. These tables - one for
* the root entity, one for the child entity - are designed in a way
* to include RAP BO instance data as well content IDs and flags to
* specify if an instance is to be changed or deleted (which is relevant
* for the save method in this example).
*
* The purpose of this class is to create these internal tables and
* provide a method implementation for the preparation of the tables,
* i. e. to prepare the content of the transactional buffer which is
* accessed throughout the handler and saver method implementations.
* The method/s is/are called in the context of each EML request.
*
***********************************************************************
"Class that constitutes the transactional buffer
CLASS lcl_buffer DEFINITION.
  PUBLIC SECTION.

    "Structure and internal table types for the internal table serving
    "as transactional buffers for the root and child entities
    TYPES: BEGIN OF gty_buffer,
             instance TYPE zdemo_abap_rap_ro_u,
             cid      TYPE string,
             changed  TYPE abap_bool,
             deleted  TYPE abap_bool,
           END OF gty_buffer.

    TYPES: BEGIN OF gty_buffer_child,
             instance   TYPE zdemo_abap_rap_ch_u,
             cid_ref    TYPE string,
             cid_target TYPE string,
             changed    TYPE abap_bool,
             deleted    TYPE abap_bool,
           END OF gty_buffer_child.

    TYPES gtt_buffer TYPE TABLE OF gty_buffer WITH EMPTY KEY.
    TYPES gtt_buffer_child TYPE TABLE OF gty_buffer_child WITH EMPTY KEY.
    "Structure and internal table types to include the keys for buffer preparation methods
    TYPES: BEGIN OF root_keys,
             key_field TYPE zdemo_abap_rap_ro_u-key_field,
           END OF root_keys,
           BEGIN OF child_keys,
             key_field TYPE zdemo_abap_rap_ch_u-key_field,
             key_ch    TYPE zdemo_abap_rap_ch_u-key_ch,
             full_key  TYPE abap_bool,
           END OF child_keys,
           tt_root_keys  TYPE TABLE OF root_keys WITH EMPTY KEY,
           tt_child_keys TYPE TABLE OF child_keys WITH EMPTY KEY.

    CLASS-DATA:
      "Internal tables serving as transactional buffers for the root and child entities
      root_buffer  TYPE STANDARD TABLE OF gty_buffer WITH EMPTY KEY,
      child_buffer TYPE STANDARD TABLE OF gty_buffer_child WITH EMPTY KEY.

    "Buffer preparation methods
    CLASS-METHODS: prep_root_buffer IMPORTING keys TYPE tt_root_keys,
      prep_child_buffer IMPORTING keys TYPE tt_child_keys.

ENDCLASS.


CLASS lcl_buffer IMPLEMENTATION.

  "Buffer preparation for the root entity based on the requested key values
  METHOD prep_root_buffer.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<buffer>).
      "Logic:
      "- Line with the specific key values exists in the buffer for the root entity
      "- If it is true: Do nothing, buffer is prepared for the specific instance.
      "- Note: If the line is marked as deleted, the buffer should not be filled anew with the data.
      IF line_exists( lcl_buffer=>root_buffer[ instance-key_field = <buffer>-key_field ] ).
        "Do nothing, buffer is prepared for the specific instance.
      ELSE.
        "Checking if entry exists in the database table of the root entity based on the key value
        SELECT SINGLE @abap_true
          FROM zdemo_abap_rapt1
          WHERE key_field = @<buffer>-key_field
          INTO @DATA(exists).

        IF exists = abap_true.
          "If entry exists, retrieve it based on the shared key value
          DATA line TYPE zdemo_abap_rap_ro_u.

          SELECT SINGLE * FROM zdemo_abap_rapt1
            WHERE key_field = @<buffer>-key_field
            INTO CORRESPONDING FIELDS OF @line.

          IF sy-subrc = 0.
            "Adding line to the root buffer
            APPEND VALUE #( instance = line ) TO lcl_buffer=>root_buffer.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  "Buffer preparation for the child entity based on the requested key values
  METHOD prep_child_buffer.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<buffer_ch>).

      "The full_key flag is in this example only relevant if a read operation is executed on the child entity directly
      "and all key values should be considered for the data retrieval from the database table.
      IF <buffer_ch>-full_key = abap_true.
        "Logic:
        "- Line with specific key values exists in the buffer for the child entity
        "- If it is true: Do nothing, buffer is prepared for the specific instance.
        IF line_exists( lcl_buffer=>child_buffer[ instance-key_field = <buffer_ch>-key_field
                                                  instance-key_ch    = <buffer_ch>-key_ch ] ).
          "Buffer is prepared for the instance.
        ELSE.
          "Checking if entry exists in the database table of the child entity based on the shared key value
          SELECT SINGLE @abap_true
           FROM zdemo_abap_rapt2
           WHERE key_field = @<buffer_ch>-key_field
             AND key_ch    = @<buffer_ch>-key_ch
           INTO @DATA(exists).

          "If entry exists, retrieve all entries based on the key values
          IF exists = abap_true.

            DATA line_ch TYPE zdemo_abap_rap_ch_u.
            SELECT SINGLE * FROM zdemo_abap_rapt2
            WHERE key_field = @<buffer_ch>-key_field
                AND key_ch    = @<buffer_ch>-key_ch
            INTO CORRESPONDING FIELDS OF @line_ch.

            IF sy-subrc = 0.
              "Adding line to the child buffer if no line exists with all key values
              APPEND VALUE #( instance = line_ch ) TO lcl_buffer=>child_buffer.
            ENDIF.

          ENDIF.
        ENDIF.

      ELSE.

        "Logic:
        "- Line with specific keys exists in the buffer for the root entity and is marked for deletion
        "- If all is true: Doing nothing, buffer is prepared for the specific instance.
        "- Else: Retrieving all lines from the database table of the child entity having the shared key
        IF line_exists( lcl_buffer=>root_buffer[ instance-key_field = <buffer_ch>-key_field ] )
        AND VALUE #( lcl_buffer=>root_buffer[ instance-key_field = <buffer_ch>-key_field ]-deleted OPTIONAL ) IS NOT INITIAL.
          "Buffer is prepared for the instance.
        ELSE.
          "Checking if entry exists in the database table of the child entity based on the shared key value
          SELECT SINGLE @abap_true
            FROM zdemo_abap_rapt2
            WHERE key_field = @<buffer_ch>-key_field
            INTO @DATA(exists_ch).

          "If entry exists, retrieve all entries based on the shared key value
          IF exists_ch = abap_true.

            DATA ch_tab TYPE TABLE OF zdemo_abap_rap_ch_u WITH EMPTY KEY.
            SELECT * FROM zdemo_abap_rapt2
            WHERE key_field = @<buffer_ch>-key_field
            INTO CORRESPONDING FIELDS OF TABLE @ch_tab.

            IF sy-subrc = 0.

              LOOP AT ch_tab ASSIGNING FIELD-SYMBOL(<ch>).
                "Adding line to the child buffer if no line exists with all key values
                IF NOT line_exists( lcl_buffer=>child_buffer[ instance-key_field = <ch>-key_field
                                                              instance-key_ch = <ch>-key_ch ] ).
                  APPEND VALUE #( instance = <ch> ) TO lcl_buffer=>child_buffer.
                ENDIF.
              ENDLOOP.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

***********************************************************************
*             Local handler class lhc_root
*
* Contains handler method definitions and implementations as defined
* in the CDS behavior definition (BDEF).
*
***********************************************************************

CLASS lhc_root DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
    IMPORTING REQUEST requested_authorizations FOR root RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE root.

    METHODS read FOR READ
      IMPORTING keys FOR READ root RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK root.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE root.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE root.

    METHODS rba_child FOR READ
      IMPORTING keys_rba FOR READ root\_child FULL result_requested RESULT result LINK association_links.

    METHODS cba_child FOR MODIFY
      IMPORTING entities_cba FOR CREATE root\_child.

    METHODS multiply_by_2 FOR MODIFY
      IMPORTING keys FOR ACTION root~multiply_by_2.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR root RESULT result.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR root RESULT result.

    METHODS multiply_by_3 FOR MODIFY
      IMPORTING keys FOR ACTION root~multiply_by_3.

    METHODS get_global_features FOR GLOBAL FEATURES
    IMPORTING REQUEST requested_features FOR root RESULT result.

    METHODS set_z FOR MODIFY
      IMPORTING keys FOR ACTION root~set_z.

ENDCLASS.

CLASS lhc_root IMPLEMENTATION.

  METHOD get_global_authorizations.
    "No implementation. All users are authorized.
  ENDMETHOD.

  METHOD create.
    "Preparing the transactional buffer based on the input BDEF derived type.
    lcl_buffer=>prep_root_buffer( CORRESPONDING #( entities ) ).

    "Processing requested entities sequentially
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<create>).
      "Logic:
      "- Line with the specific key does not exist in the buffer for the root entity
      "- Line with the specific key exists in the buffer but it is marked as deleted
      "- If it is true: Add new instance to the buffer and, if needed, remove the instance marked as deleted beforehand
      IF NOT line_exists( lcl_buffer=>root_buffer[ instance-key_field = <create>-key_field ] )
      OR line_exists( lcl_buffer=>root_buffer[ instance-key_field = <create>-key_field deleted = abap_true ] ).

        "If it exists, removing instance that is marked for deletion from the transactional buffer since it gets replaced by a new one.
        DELETE lcl_buffer=>root_buffer WHERE instance-key_field = VALUE #(
            lcl_buffer=>root_buffer[ instance-key_field = <create>-key_field ]-instance-key_field OPTIONAL ) AND deleted = abap_true.

        "Adding new instance to the transactional buffer by considering %control values
        APPEND VALUE #( cid                = <create>-%cid
                        instance-key_field = <create>-key_field
                        instance-field1    = COND #( WHEN <create>-%control-field1 NE if_abap_behv=>mk-off
                                                     THEN <create>-field1 )
                        instance-field2    = COND #( WHEN <create>-%control-field2 NE if_abap_behv=>mk-off
                                                     THEN <create>-field2 )
                        instance-field3    = COND #( WHEN <create>-%control-field3 NE if_abap_behv=>mk-off
                                                     THEN <create>-field3 )
                        instance-field4    = COND #( WHEN <create>-%control-field4 NE if_abap_behv=>mk-off
                                                     THEN <create>-field4 )
                        changed            = abap_true
                        deleted            = abap_false ) TO lcl_buffer=>root_buffer.

        "Filling the MAPPED response parameter for the root entity
        INSERT VALUE #( %cid = <create>-%cid
                        %key = <create>-%key ) INTO TABLE mapped-root.

      ELSE.

        "Filling FAILED and REPORTED response parameters
        APPEND VALUE #( %cid        = <create>-%cid
                        %key        = <create>-%key
                        %create     = if_abap_behv=>mk-on
                        %fail-cause = if_abap_behv=>cause-unspecific
                    ) TO failed-root.

        APPEND VALUE #( %cid      = <create>-%cid
                        %key      = <create>-%key
                        %create   = if_abap_behv=>mk-on
                        %msg      = new_message_with_text(
                        severity  = if_abap_behv_message=>severity-error
                        text      = 'Create operation failed.' )
                    ) TO reported-root.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD update.
    "Preparing the transactional buffer based on the input BDEF derived type.
    lcl_buffer=>prep_root_buffer( CORRESPONDING #( entities ) ).

    "Processing requested entities sequentially
    "Note:
    "The example is implemented in a way that instances that failed in methods called before this method are not handled.
    "The instances that have failed before this method call are not available in this method's input parameter.
    "Hence, an adding to FAILED and REPORTED is not implemented here.
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<update>).

      "Logic:
      "- Line with the specific key exists in the buffer for the root entity and it is not marked as deleted
      "- If it is true: Updating the buffer based on the input BDEF derived type and considering %control values
      READ TABLE lcl_buffer=>root_buffer
         WITH KEY instance-key_field = <update>-key_field
                  deleted            = abap_false
         ASSIGNING FIELD-SYMBOL(<fs_up>).

      IF sy-subrc = 0.
        <fs_up>-instance-field1  = COND #( WHEN <update>-%control-field1 NE if_abap_behv=>mk-off
                                           THEN <update>-field1
                                           ELSE <fs_up>-instance-field1 ).

        <fs_up>-instance-field2  = COND #( WHEN <update>-%control-field2 NE if_abap_behv=>mk-off
                                           THEN <update>-field2
                                           ELSE <fs_up>-instance-field2 ).

        <fs_up>-instance-field3  = COND #( WHEN <update>-%control-field3 NE if_abap_behv=>mk-off
                                           THEN <update>-field3
                                           ELSE <fs_up>-instance-field3 ).

        <fs_up>-instance-field4  = COND #( WHEN <update>-%control-field4 NE if_abap_behv=>mk-off
                                           THEN <update>-field4
                                           ELSE <fs_up>-instance-field4 ).

        <fs_up>-changed  = abap_true.
        <fs_up>-deleted  = abap_false.
      ENDIF.


    ENDLOOP.
  ENDMETHOD.

  METHOD read.
    "Preparing the transactional buffer based on the input BDEF derived type.
    lcl_buffer=>prep_root_buffer( CORRESPONDING #( keys ) ).

    "Processing requested keys sequentially
    LOOP AT keys ASSIGNING FIELD-SYMBOL(<read>) GROUP BY <read>-%tky.
      "Logic:
      "- Line exists in the buffer and it is not marked as deleted
      "- If it is true: Adding the entries to the buffer based on the input BDEF derived type and considering %control values
      READ TABLE lcl_buffer=>root_buffer
        WITH KEY instance-key_field = <read>-key_field
                 deleted            = abap_false
        ASSIGNING FIELD-SYMBOL(<fs_r>).

      IF sy-subrc = 0.

        APPEND VALUE #( %tky   = <read>-%tky
                        field1 = COND #( WHEN <read>-%control-field1 NE if_abap_behv=>mk-off
                                         THEN <fs_r>-instance-field1 )
                        field2 = COND #( WHEN <read>-%control-field2 NE if_abap_behv=>mk-off
                                         THEN <fs_r>-instance-field2 )
                        field3 = COND #( WHEN <read>-%control-field3 NE if_abap_behv=>mk-off
                                         THEN <fs_r>-instance-field3 )
                        field4 = COND #( WHEN <read>-%control-field4 NE if_abap_behv=>mk-off
                                         THEN <fs_r>-instance-field4 )
                        ) TO result.

      ELSE.

        "Filling FAILED and REPORTED response parameters
        APPEND VALUE #( %tky         = <read>-%tky
                        %fail-cause  = if_abap_behv=>cause-not_found
                    ) TO failed-root.

        APPEND VALUE #( %tky = <read>-%tky
                        %msg = new_message_with_text(
                        severity  = if_abap_behv_message=>severity-error
                        text      = 'Read operation failed.' )
                    ) TO reported-root.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD lock.

    TRY.
        "Instantiating lock object
        DATA(lo_lock) = cl_abap_lock_object_factory=>get_instance(
                        iv_name = 'EZDEMO_ABAP_LOCK' ).

        "Processing requested keys sequentially
        LOOP AT keys REFERENCE INTO DATA(lr_key).
          TRY.
              lo_lock->enqueue( it_parameter = VALUE #(
                ( name = 'KEY_FIELD' value = REF #( lr_key->key_field ) ) ) ).
            CATCH cx_abap_foreign_lock.
              APPEND VALUE #( %key        = CORRESPONDING #( lr_key->* )
                              %fail-cause = if_abap_behv=>cause-locked )
              TO failed-root.
            CATCH cx_abap_lock_failure.
              APPEND VALUE #( %key        = CORRESPONDING #( lr_key->* )
                              %fail-cause = if_abap_behv=>cause-unspecific )
              TO failed-root.
          ENDTRY.
        ENDLOOP.
      CATCH cx_abap_lock_failure.
        APPEND VALUE #( %fail-cause = if_abap_behv=>cause-unspecific )
        TO failed-root.
    ENDTRY.
  ENDMETHOD.

  METHOD delete.
    "Preparing the transactional buffer based on the input BDEF derived type.
    lcl_buffer=>prep_root_buffer( CORRESPONDING #( keys ) ).
    "Preparing the child buffer to mark child instances when parent instances are marked for deletion.
    lcl_buffer=>prep_child_buffer( VALUE #( FOR wa IN keys ( key_field = wa-key_field ) ) ).

    "Processing requested keys sequentially
    "Note:
    "The example is implemented in a way that instances that failed in methods called before this method are not handled.
    "The instances that have failed before this method call are not available in this method's input parameter.
    "Hence, an adding to FAILED and REPORTED is not implemented here.
    LOOP AT keys ASSIGNING FIELD-SYMBOL(<delete>).
      "Logic:
      "- Line exists in the buffer and it is not marked as deleted
      "- If it is true: Flagging the instance as deleted
      READ TABLE lcl_buffer=>root_buffer
        WITH KEY instance-key_field = <delete>-key_field
                 deleted = abap_false
        ASSIGNING FIELD-SYMBOL(<fs_del>).

      IF sy-subrc = 0.
        <fs_del>-changed  = abap_false.
        <fs_del>-deleted  = abap_true.

        "When parent instances are marked for deletion, child instances with the shared key should be marked as well.
        LOOP AT lcl_buffer=>child_buffer ASSIGNING FIELD-SYMBOL(<del_ch>) WHERE instance-key_field = <delete>-key_field.
          <del_ch>-changed = abap_false.
          <del_ch>-deleted = abap_true.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD rba_Child.
    "Preparing the transactional buffers for both the root and child entity based on the input BDEF derived type.
    lcl_buffer=>prep_root_buffer( CORRESPONDING #( keys_rba ) ).
    lcl_buffer=>prep_child_buffer( CORRESPONDING #( keys_rba ) ).

    "Processing requested keys sequentially
    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<rba>) GROUP BY <rba>-key_field.
      "Logic:
      "- Line with the shared key value exists in the buffer for the root entity and it is not marked as deleted
      "- Line with the shared key value exists in the child buffer
      "- If it is true: Sequentially processing the child buffer entries (the example is set up in a way that there can be multiple entries)
      IF line_exists( lcl_buffer=>root_buffer[ instance-key_field = <rba>-key_field deleted = abap_false ] )
      AND line_exists( lcl_buffer=>child_buffer[ instance-key_field = <rba>-key_field deleted = abap_false ] ).

        LOOP AT lcl_buffer=>child_buffer ASSIGNING FIELD-SYMBOL(<ch>) WHERE instance-key_field = <rba>-key_field.

          "Filling the table for the LINK parameter
          INSERT VALUE #( source-%tky = <rba>-%tky
                          target-%tky = VALUE #( key_field = <ch>-instance-key_field
                                                 key_ch    = <ch>-instance-key_ch ) ) INTO TABLE association_links.

          "Filling the table for the RESULT parameter based on the FULL parameter
          "Note: If the FULL parameter is initial, only the LINK parameter should be provided
          IF result_requested = abap_true.
            APPEND VALUE #( key_field   = <ch>-instance-key_field
                            key_ch      = <ch>-instance-key_ch
                            field_ch1 = COND #( WHEN <rba>-%control-field_ch1 NE if_abap_behv=>mk-off
                                                THEN <ch>-instance-field_ch1 )
                            field_ch2 = COND #( WHEN <rba>-%control-field_ch2 NE if_abap_behv=>mk-off
                                                THEN <ch>-instance-field_ch2 )
                          ) TO result.
          ENDIF.
        ENDLOOP.

      ELSE.

        "Filling FAILED and REPORTED response parameters
        APPEND VALUE #( %tky          = <rba>-%tky
                        %fail-cause   = if_abap_behv=>cause-not_found
                        %assoc-_child = if_abap_behv=>mk-on
                    ) TO failed-root.


        APPEND VALUE #( %tky = <rba>-%tky
                        %msg = new_message_with_text(
                        severity  = if_abap_behv_message=>severity-error
                        text      = 'RBA (parent to child) operation failed.' )
                    ) TO reported-root.

      ENDIF.
    ENDLOOP.

    "Removing potential duplicate entries
    SORT association_links BY target ASCENDING.
    DELETE ADJACENT DUPLICATES FROM association_links COMPARING ALL FIELDS.

    SORT result BY %tky ASCENDING.
    DELETE ADJACENT DUPLICATES FROM result COMPARING ALL FIELDS.

  ENDMETHOD.

  METHOD cba_Child.
    "Preparing the transactional buffers for both the root and child entity based on the input BDEF derived type
    lcl_buffer=>prep_root_buffer( CORRESPONDING #( entities_cba ) ).
    lcl_buffer=>prep_child_buffer( CORRESPONDING #( entities_cba ) ).

    "Processing requested entities sequentially
    LOOP AT entities_cba ASSIGNING FIELD-SYMBOL(<cba>) GROUP BY <cba>-key_field.
      "Logic:
      "- Line with the shared key value exists in the buffer for the root entity and it is not marked as deleted
      "- If it is true: Sequentially processing the instances in the %target table
      IF line_exists( lcl_buffer=>root_buffer[ instance-key_field = <cba>-key_field deleted = abap_false ] ).

        "If it exists, removing instance that is marked for deletion from the child transactional buffer since it gets replaced by a new one.
        DELETE lcl_buffer=>child_buffer WHERE instance-key_field = <cba>-key_field AND deleted = abap_true.

        LOOP AT <cba>-%target ASSIGNING FIELD-SYMBOL(<ch>).

          "Adding instance to child buffer if it does not exist there and considering %control values
          "The example is implemented in a way that the RAP BO consumer need not specify the common key with the root entity.
          "Plus, the keys of the child entity should not be initial.
          IF NOT line_exists( lcl_buffer=>child_buffer[ instance-key_field = <cba>-key_field
                                                        instance-key_ch    = <ch>-key_ch ] )
          AND <ch>-key_ch IS NOT INITIAL.

            APPEND VALUE #( cid_ref            = <cba>-%cid_ref
                            cid_target         = <ch>-%cid
                            instance-key_field = <cba>-key_field
                            instance-key_ch    = <ch>-key_ch
                            instance-field_ch1 = COND #( WHEN <ch>-%control-field_ch1 NE if_abap_behv=>mk-off
                                                         THEN <ch>-field_ch1 )
                            instance-field_ch2 = COND #( WHEN <ch>-%control-field_ch2 NE if_abap_behv=>mk-off
                                                         THEN <ch>-field_ch2 )
                            changed            = abap_true
                        ) TO lcl_buffer=>child_buffer.

            "Filling MAPPED response parameter
            INSERT VALUE #( %cid = <ch>-%cid
                            %key = VALUE #( key_field = <cba>-key_field
                                            key_ch    = <ch>-key_ch ) ) INTO TABLE mapped-child.

          ELSE.

            "Filling FAILED and REPORTED response parameters
            APPEND VALUE #( %cid          = <cba>-%cid_ref
                            %tky          = <cba>-%tky
                            %assoc-_child = if_abap_behv=>mk-on
                            %fail-cause   = if_abap_behv=>cause-unspecific
                        ) TO failed-root.

            APPEND VALUE #( %cid = <cba>-%cid_ref
                            %tky = <cba>-%tky
                            %msg = new_message_with_text(
                            severity  = if_abap_behv_message=>severity-error
                            text      = 'Create-by-association (root to child) operation failed.' )
                        ) TO reported-root.

            APPEND VALUE #( %cid        = <ch>-%cid
                            %key        = VALUE #( key_field = <cba>-key_field key_ch = <ch>-key_ch )
                            %fail-cause = if_abap_behv=>cause-dependency
                            ) TO failed-child.

            APPEND VALUE #( %cid = <ch>-%cid
                            %key = VALUE #( key_field = <cba>-key_field key_ch = <ch>-key_ch )
                            %msg = new_message_with_text(
                                severity  = if_abap_behv_message=>severity-error
                                text      = 'Create-by-association (root to child) operation failed.' )
                            ) TO reported-child.

          ENDIF.
        ENDLOOP.

      ELSE.

        "Filling FAILED and REPORTED response parameters
        APPEND VALUE #( %cid          = <cba>-%cid_ref
                        %tky          = <cba>-%tky
                        %assoc-_child = if_abap_behv=>mk-on
                        %fail-cause   = if_abap_behv=>cause-not_found
                    ) TO failed-root.

        APPEND VALUE #( %cid = <cba>-%cid_ref
                        %tky = <cba>-%tky
                        %msg = new_message_with_text(
                        severity  = if_abap_behv_message=>severity-error
                        text      = 'Create-by-association (root to child) operation failed.' )
                    ) TO reported-root.

        LOOP AT <cba>-%target ASSIGNING FIELD-SYMBOL(<target>).
          APPEND VALUE #( %cid        = <target>-%cid
                          %key        = <target>-%key
                          %fail-cause = if_abap_behv=>cause-dependency
                          ) TO failed-child.

          APPEND VALUE #( %cid = <target>-%cid
                          %key = <target>-%key
                          %msg = new_message_with_text(
                              severity  = if_abap_behv_message=>severity-error
                              text      = 'Create-by-association (root to child) operation failed.' )
                          ) TO reported-child.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD multiply_by_2.

    "Retrieving instances based on requested keys
    READ ENTITIES OF zdemo_abap_rap_ro_u IN LOCAL MODE
      ENTITY root
      FIELDS ( field3 field4 ) WITH CORRESPONDING #( keys )
      RESULT DATA(result)
      FAILED failed.

    "If read result is initial, stop further method execution.
    CHECK result IS NOT INITIAL.

    "Setting %action value in failed response parameter
    LOOP AT failed-root ASSIGNING FIELD-SYMBOL(<f>).
      <f>-%action-multiply_by_2 = if_abap_behv=>mk-on.
    ENDLOOP.

    "Multiply integer values by 2
    MODIFY ENTITIES OF zdemo_abap_rap_ro_u IN LOCAL MODE
      ENTITY root
      UPDATE FIELDS ( field3 field4 ) WITH VALUE #( FOR key IN result ( %tky   = key-%tky
                                                                      field3 = key-field3 * 2
                                                                      field4 = key-field4 * 2 ) ).
  ENDMETHOD.

  METHOD get_instance_authorizations.

    "Retrieving instances based on requested keys
    READ ENTITIES OF zdemo_abap_rap_ro_u IN LOCAL MODE
      ENTITY root
      FIELDS ( field1 ) WITH CORRESPONDING #( keys )
      RESULT DATA(status)
      FAILED failed.

    "If the read result is initial, stop further method execution.
    CHECK status IS NOT INITIAL.

    LOOP AT status ASSIGNING FIELD-SYMBOL(<auth>).

      "If a specific field has a certain value, the deletion should be disallowed.
      IF requested_authorizations-%delete = if_abap_behv=>mk-on.
        APPEND VALUE #( %tky = <auth>-%tky
                        %op  = VALUE #( %delete = COND #( WHEN <auth>-field1 = 'X'
                                                          THEN if_abap_behv=>auth-unauthorized
                                                          ELSE if_abap_behv=>auth-allowed ) )
                      ) TO result.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_features.

    READ ENTITIES OF zdemo_abap_rap_ro_u IN LOCAL MODE
      ENTITY root
      FIELDS ( field3 field4 ) WITH CORRESPONDING #( keys )
      RESULT DATA(numbers)
      FAILED failed.

    "If the read result is initial, stop further method execution.
    CHECK numbers IS NOT INITIAL.

    LOOP AT numbers ASSIGNING FIELD-SYMBOL(<feat>).

      "If two fields have certain values, the execution of an action should be disabled for the instance.
      IF requested_features-%action-multiply_by_3 = if_abap_behv=>mk-on.
        APPEND VALUE #( %tky                            = <feat>-%tky
                        %features-%action-multiply_by_3 = COND #( WHEN <feat>-field3 = 0 OR <feat>-field4 = 0
                                                                  THEN if_abap_behv=>fc-o-disabled
                                                                  ELSE if_abap_behv=>fc-o-enabled )
                      ) TO result.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD multiply_by_3.
    "Retrieving instances based on requested keys
    READ ENTITIES OF zdemo_abap_rap_ro_u IN LOCAL MODE
      ENTITY root
      FIELDS ( field3 field4 ) WITH CORRESPONDING #( keys )
      RESULT DATA(result)
      FAILED failed.

    "Setting %action value in failed response parameter
    LOOP AT failed-root ASSIGNING FIELD-SYMBOL(<f>).
      <f>-%action-multiply_by_3 = if_abap_behv=>mk-on.
    ENDLOOP.

    "Multiply integer values by 3
    MODIFY ENTITIES OF zdemo_abap_rap_ro_u IN LOCAL MODE
      ENTITY root
      UPDATE FIELDS ( field3 field4 ) WITH VALUE #( FOR key IN result ( %tky   = key-%tky
                                                                        field3 = key-field3 * 3
                                                                        field4 = key-field4 * 3 ) ).
  ENDMETHOD.

  METHOD get_global_features.
    "The execution of an action should be disabled based on a certain time frame.
    DATA(time1) = CONV t( '070000' ).
    DATA(time2) = CONV t( '120000' ).

    result = VALUE #( %action-set_z = COND #( WHEN cl_abap_context_info=>get_system_time( ) BETWEEN time1 AND time2
                                              THEN if_abap_behv=>fc-o-enabled
                                              ELSE if_abap_behv=>fc-o-disabled )
                    ).

    IF result-%action-set_z = if_abap_behv=>fc-o-disabled.
      APPEND VALUE #( %msg    = new_message_with_text( text     = 'Execution of action currently not allowed.'
                                                       severity = if_abap_behv_message=>severity-error )
                      %global = if_abap_behv=>mk-on ) TO reported-root.
    ENDIF.
  ENDMETHOD.

  METHOD set_z.

    "Retrieving instances based on requested keys
    READ ENTITIES OF zdemo_abap_rap_ro_u IN LOCAL MODE
      ENTITY root
      FIELDS ( field3 field4 ) WITH CORRESPONDING #( keys )
      RESULT DATA(result)
      FAILED failed.

    "Setting %action value in failed response parameter
    LOOP AT failed-root ASSIGNING FIELD-SYMBOL(<f>).
      <f>-%action-set_z = if_abap_behv=>mk-on.
    ENDLOOP.

    "Setting a field value
    MODIFY ENTITIES OF zdemo_abap_rap_ro_u IN LOCAL MODE
      ENTITY root
      UPDATE FIELDS ( field2 ) WITH VALUE #( FOR key IN result ( %tky   = key-%tky
                                                                 field2 = 'Z' ) ).

  ENDMETHOD.

ENDCLASS.

***********************************************************************
*             Local saver class lsc_zdemo_abap_rap_ro_u
*
* Contains saver method definitions and implementations. The only
* methods that are implemented in this example are the save method (to
* persist the data to the database) and cleanup methods (to clear the
* transactional buffer).
*
* The save method is implemented in a way, that the database tables
* are modified based on the entries in the transactional buffer tables
* and the flags for changed and deleted.
* If the flag for changed is not initial, the instance gets created
* or updated respectively in the database table.
* If the flag for deleted is not initial, the database table entry
* is deleted.
*
***********************************************************************

CLASS lsc_zdemo_abap_rap_ro_u DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zdemo_abap_rap_ro_u IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.

    "Processing the saving of create and update operations
    "Only those entries should be saved to the database table whose flag for "changed" is not initial.
    DATA mod_tab TYPE TABLE OF zdemo_abap_rap_ro_u.

    IF line_exists( lcl_buffer=>root_buffer[ changed = abap_true ] ).
      LOOP AT lcl_buffer=>root_buffer ASSIGNING FIELD-SYMBOL(<cr>) WHERE changed = abap_true AND deleted = abap_false.
        APPEND CORRESPONDING #( <cr>-instance ) TO mod_tab.
      ENDLOOP.
      MODIFY zdemo_abap_rapt1 FROM TABLE @( CORRESPONDING #( mod_tab ) ).
    ENDIF.

    "Processing the saving of delete operations
    "Only those entries should be deleted from the database table whose flag "deleted" is not initial.
    DATA del_tab TYPE lcl_buffer=>tt_root_keys.

    IF line_exists( lcl_buffer=>root_buffer[ deleted = abap_true ] ).
      LOOP AT lcl_buffer=>root_buffer ASSIGNING FIELD-SYMBOL(<del>) WHERE deleted = abap_true.
        APPEND CORRESPONDING #( <del>-instance ) TO del_tab.
      ENDLOOP.
      DELETE zdemo_abap_rapt1 FROM TABLE @( CORRESPONDING #( del_tab ) ).

      "Processing database entries of child entity: When the parent instance is deleted,
      "the corresponding child instances are deleted.
      SELECT key_field, key_ch
        FROM zdemo_abap_rapt2 AS db
        WHERE EXISTS
        ( SELECT * FROM @del_tab AS it
          WHERE key_field = db~key_field )
          INTO TABLE @DATA(child_keys).

      "Deleting entries from database table
      DELETE zdemo_abap_rapt2 FROM TABLE @( CORRESPONDING #( child_keys ) ).
      "Deleting instances from child buffer
      DELETE lcl_buffer=>child_buffer WHERE instance-key_field = <del>-instance-key_field AND deleted = abap_true.
    ENDIF.

    "Processing the saving of create-by-association operations.
    DATA cba_tab TYPE TABLE OF zdemo_abap_rap_ch_u.

    IF line_exists( lcl_buffer=>child_buffer[ changed = abap_true ] ).
      LOOP AT lcl_buffer=>child_buffer ASSIGNING FIELD-SYMBOL(<cba>) WHERE changed = abap_true AND deleted = abap_false.
        APPEND CORRESPONDING #( <cba>-instance ) TO cba_tab.
      ENDLOOP.
      MODIFY zdemo_abap_rapt2 FROM TABLE @( CORRESPONDING #( cba_tab ) ).
    ENDIF.
  ENDMETHOD.

  METHOD cleanup.
    "Clearing the transactional buffer.
    CLEAR lcl_buffer=>root_buffer.
    CLEAR lcl_buffer=>child_buffer.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

***********************************************************************
*      Local handler class for the child entity lhc_child
*
* Contains handler method definitions and implementations for read
* and read-by-association operations.
*
***********************************************************************

CLASS lhc_child DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ child RESULT result.

    METHODS rba_Parent FOR READ
      IMPORTING keys_rba FOR READ child\_Parent FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_child IMPLEMENTATION.

  METHOD read.
    "Preparing the transactional buffer for child entity based on the input BDEF derived type.
    "Here, the full_key flag is set to consider all key values.
    "Purpose: The preparation method is set up to also consider the entries in the root buffer
    "when dealing with by-association operations which is not relevant in this case.
    lcl_buffer=>prep_child_buffer( VALUE #( FOR wa IN keys ( key_field = wa-key_field
                                                             key_ch    = wa-key_ch
                                                             full_key  = abap_true  ) ) ).

    "Processing the requested keys sequentially
    LOOP AT keys ASSIGNING FIELD-SYMBOL(<read>) GROUP BY <read>-%tky.

      "Logic:
      "- Line with the requested key values exists in the child buffer
      "- If it is true: Adding the line to the RESULT parameter considering %control values.
      READ TABLE lcl_buffer=>child_buffer
        WITH KEY instance-key_field = <read>-key_field
                 instance-key_ch    = <read>-key_ch
        ASSIGNING FIELD-SYMBOL(<fs_rc>).

      IF sy-subrc = 0 AND <fs_rc>-deleted = abap_false.

        APPEND VALUE #( %tky      = <read>-%tky
                        field_ch1 = COND #( WHEN <read>-%control-field_ch1 NE if_abap_behv=>mk-off
                                            THEN <fs_rc>-instance-field_ch1 )
                        field_ch2 = COND #( WHEN <read>-%control-field_ch2 NE if_abap_behv=>mk-off
                                            THEN <fs_rc>-instance-field_ch2 )
                    ) TO result.

      ELSE.

        "Filling FAILED and REPORTED response parameters
        APPEND VALUE #( %tky        = <read>-%tky
                        %fail-cause = if_abap_behv=>cause-not_found
                    ) TO failed-child.

        APPEND VALUE #( %tky =  <read>-%tky
                        %msg = new_message_with_text(
                        severity  = if_abap_behv_message=>severity-error
                        text      = 'Read operation failed (child entity).' )
                    ) TO reported-child.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD rba_Parent.
    "Preparing the transactional buffers for both the root and child entity based on the input BDEF derived type.
    lcl_buffer=>prep_root_buffer( CORRESPONDING #( keys_rba ) ).
    lcl_buffer=>prep_child_buffer( CORRESPONDING #( keys_rba ) ).

    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<rba>) GROUP BY <rba>-%tky.
      "Logic:
      "- Line with the shared key value exists in buffer for the root entity and is not marked as deleted
      "- Line with the full key exists in the child buffer
      "- If it is true: Adding the instance to the RESULT parameter considering %control values
      IF line_exists( lcl_buffer=>root_buffer[ instance-key_field = <rba>-key_field deleted = abap_false ] )
      AND line_exists( lcl_buffer=>child_buffer[ instance-key_field = <rba>-key_field instance-key_ch = <rba>-key_ch deleted = abap_false ] ).

        "Filling the LINK parameter
        INSERT VALUE #( target-%tky = CORRESPONDING #( <rba>-%tky )
                        source-%tky = VALUE #( key_field = <rba>-key_field
                                               key_ch    = <rba>-key_ch )
                    ) INTO TABLE association_links.

        IF result_requested = abap_true.
          READ TABLE lcl_buffer=>root_buffer
            WITH KEY instance-key_field = <rba>-key_field
            ASSIGNING FIELD-SYMBOL(<fs_rp>).

          IF sy-subrc = 0.

            APPEND VALUE #( %tky   = CORRESPONDING #( <rba>-%tky )
                            field1 = COND #( WHEN <rba>-%control-field1 NE if_abap_behv=>mk-off
                                             THEN <fs_rp>-instance-field1 )
                            field2 = COND #( WHEN <rba>-%control-field2 NE if_abap_behv=>mk-off
                                             THEN <fs_rp>-instance-field2 )
                            field3 = COND #( WHEN <rba>-%control-field3 NE if_abap_behv=>mk-off
                                             THEN <fs_rp>-instance-field3 )
                            field4 = COND #( WHEN <rba>-%control-field4 NE if_abap_behv=>mk-off
                                             THEN <fs_rp>-instance-field4 )
                            ) TO result.
          ENDIF.
        ENDIF.

      ELSE.

        "Filling FAILED and REPORTED response parameters
        APPEND VALUE #( %tky           = <rba>-%tky
                        %assoc-_parent = if_abap_behv=>mk-on
                        %fail-cause    = if_abap_behv=>cause-not_found
                    ) TO failed-child.


        APPEND VALUE #( %tky = <rba>-%tky
                        %msg = new_message_with_text(
                        severity  = if_abap_behv_message=>severity-error
                        text      = 'RBA (child to parent) failed.' )
                    ) TO reported-child.

      ENDIF.
    ENDLOOP.

    "Removing potential duplicate entries.
    SORT association_links BY target ASCENDING.
    DELETE ADJACENT DUPLICATES FROM association_links COMPARING ALL FIELDS.

    SORT result BY %tky ASCENDING.
    DELETE ADJACENT DUPLICATES FROM result COMPARING ALL FIELDS.

  ENDMETHOD.

ENDCLASS.
