*&---------------------------------------------------------------------*
*& The lcl_buffer class provides a self-contained, easily consumable
*& example for an unmanaged RAP BO. It uses an internal table to set
*& up the transactional buffer. This table includes RAP BO instance
*& data and flags that indicate whether to create, update, or delete
*& an instance. The flags are represented by an enumeration type.
*&
*& The class defines the table type, the internal table, and a method
*& to prepare the transactional buffer. In the example
*& implementations, this method is called within the context of EML
*& requests. Based on the internal table's content - considering both
*& the instance data and the flags - the underlying database table
*& gets modified.
*&---------------------------------------------------------------------*
CLASS lcl_buffer DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ENUM ty_operation,
             create,
             update,
             delete,
           END OF ENUM ty_operation.

    TYPES: BEGIN OF ty_buffer.
             INCLUDE TYPE zdemoabap97 AS data.
    TYPES:   flag TYPE ty_operation,
           END OF ty_buffer.

    TYPES tt_buffer TYPE SORTED TABLE OF ty_buffer WITH UNIQUE KEY id.
    CLASS-DATA root_buffer TYPE tt_buffer.
    TYPES tt_root_keys TYPE TABLE OF zdemoabap97-id WITH EMPTY KEY.
    CLASS-METHODS: prep_root_buffer IMPORTING keys TYPE tt_root_keys.
ENDCLASS.

CLASS lcl_buffer IMPLEMENTATION.
  METHOD prep_root_buffer.

    "If an instance with a given key is not available in the
    "transactional buffer, the instance is, if available, retrieved
    "from the database and added to the buffer table.
    SELECT *
      FROM zdemoabap97
      WHERE EXISTS
        ( SELECT 'X' FROM @keys AS tab WHERE id = tab~table_line )
      INTO TABLE @DATA(itab).

    LOOP AT itab ASSIGNING FIELD-SYMBOL(<i>).
      INSERT CORRESPONDING #( <i> ) INTO TABLE lcl_buffer=>root_buffer.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lhc_demo_abap DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE demo_abap.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE demo_abap.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE demo_abap.

    METHODS read FOR READ
      IMPORTING keys FOR READ demo_abap RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK demo_abap.

ENDCLASS.

CLASS lhc_demo_abap IMPLEMENTATION.

  METHOD create.

*&---------------------------------------------------------------------*
*& The example implementation prevents the creation of duplicate IDs.
*& If an instance is already in the buffer, it will add a failed
*& entry. Therefore, the implementation operates without a precheck
*& or validation for duplicate IDs.
*&---------------------------------------------------------------------*

    "Preparing the transactional buffer
    lcl_buffer=>prep_root_buffer( VALUE #( FOR k IN entities ( k-id ) ) ).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<create>).
      "Instance not in buffer
      IF NOT line_exists( lcl_buffer=>root_buffer[ id = <create>-id ] )
      "Instance marked for deletion
      OR line_exists( lcl_buffer=>root_buffer[ id = <create>-id flag = lcl_buffer=>delete ] ).

        "If the instance exists and is marked for deletion, removing it from the transactional
        "buffer because it gets replaced by a new one.
        DELETE lcl_buffer=>root_buffer WHERE id = VALUE #(
            lcl_buffer=>root_buffer[ id = <create>-id ]-id OPTIONAL ) AND flag = lcl_buffer=>delete.

        "Adding new instance to the transactional buffer by considering %control values
        INSERT VALUE #( "cid                = <create>-%cid
                        id = <create>-id
                        Number1    = COND #( WHEN <create>-%control-Number1 NE if_abap_behv=>mk-off
                                                     THEN <create>-Number1 )
                        Number2    = COND #( WHEN <create>-%control-Number2 NE if_abap_behv=>mk-off
                                                     THEN <create>-Number2 )
                        Operator    = COND #( WHEN <create>-%control-Operator NE if_abap_behv=>mk-off
                                                     THEN <create>-Operator )
                        flag = lcl_buffer=>create ) INTO TABLE lcl_buffer=>root_buffer.

        INSERT VALUE #( %cid = <create>-%cid
                        %key = <create>-%key ) INTO TABLE mapped-demo_abap.

      ELSE.

        "Filling FAILED and REPORTED response parameters
        APPEND VALUE #( %cid        = <create>-%cid
                        %key        = <create>-%key
                        %create     = if_abap_behv=>mk-on
                        %fail-cause = if_abap_behv=>cause-unspecific
                    ) TO failed-demo_abap.

        APPEND VALUE #( %cid      = <create>-%cid
                        %key      = <create>-%key
                        %create   = if_abap_behv=>mk-on
                        %msg      = new_message_with_text(
                        severity  = if_abap_behv_message=>severity-error
                        text      = 'Create operation failed.' )
                    ) TO reported-demo_abap.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD update.

*&---------------------------------------------------------------------*
*& The example implementation logic is that an update operation can
*& proceed if the line with the requested key exists in the buffer and
*& is not marked for deletion. The update will consider the %control
*& values from the input BDEF derived type.
*&---------------------------------------------------------------------*

    "Preparing the transactional buffer
    lcl_buffer=>prep_root_buffer( VALUE #( FOR k IN entities ( k-id ) ) ).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<update>).
      READ TABLE lcl_buffer=>root_buffer
         ASSIGNING FIELD-SYMBOL(<fs_up>)
         WHERE id = <update>-id AND flag <> lcl_buffer=>delete.

      IF sy-subrc = 0.
        <fs_up>-Number1  = COND #( WHEN <update>-%control-Number1 NE if_abap_behv=>mk-off
                                   THEN <update>-Number1
                                   ELSE <fs_up>-Number1 ).

        <fs_up>-Operator  = COND #( WHEN <update>-%control-Operator NE if_abap_behv=>mk-off
                                    THEN <update>-Operator
                                    ELSE <fs_up>-Operator ).

        <fs_up>-Number2  = COND #( WHEN <update>-%control-Number2 NE if_abap_behv=>mk-off
                                   THEN <update>-Number2
                                   ELSE <fs_up>-Number2 ).

        <fs_up>-flag =   lcl_buffer=>update.
      ELSE.
        APPEND VALUE #( %cid        = <update>-%cid_ref
                        %tky        = <update>-%tky
                        %update    = if_abap_behv=>mk-on
                        %fail-cause = if_abap_behv=>cause-not_found
                  ) TO failed-demo_abap.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.

*&---------------------------------------------------------------------*
*& The example implementation logic allows a delete operation to proceed
*& if the line with the requested key exists in the buffer and is not
*& marked as deleted. Therefore, instances to be deleted are marked
*& using the enumeration type.
*&---------------------------------------------------------------------*

    "Preparing the transactional buffer
    lcl_buffer=>prep_root_buffer( VALUE #( FOR k IN keys ( k-id ) ) ).

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<delete>).
      READ TABLE lcl_buffer=>root_buffer
         ASSIGNING FIELD-SYMBOL(<fs_del>)
         WHERE id = <delete>-id
                 AND flag <> lcl_buffer=>delete.

      IF sy-subrc = 0.
        <fs_del>-flag = lcl_buffer=>delete.
      ELSE.
        IF line_exists( lcl_buffer=>root_buffer[ id = <delete>-id flag = lcl_buffer=>delete  ] ).

          APPEND VALUE #( %cid        = <delete>-%cid_ref
                                    %tky        = <delete>-%tky
                                    %delete    = if_abap_behv=>mk-on
                                    %fail-cause = if_abap_behv=>cause-unspecific
                              ) TO failed-demo_abap.

          APPEND VALUE #( %cid      = <delete>-%cid_ref
                          %key      = <delete>-%key
                          %delete   = if_abap_behv=>mk-on
                          %msg      = new_message_with_text(
                            severity  = if_abap_behv_message=>severity-error
                            text      = 'Delete operation with existing key failed.' )
                        ) TO reported-demo_abap.

        ELSE.
          APPEND VALUE #( %cid        = <delete>-%cid_ref
                          %tky        = <delete>-%tky
                          %delete     = if_abap_behv=>mk-on
                          %fail-cause = if_abap_behv=>cause-not_found
                        ) TO failed-demo_abap.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD read.

*&---------------------------------------------------------------------*
*& In unmanaged RAP BOs, everything must be self-implemented,
*& including read operations, for which no explicit syntax is available
*& in the BDEF.
*& The example implementation allows a read operation to proceed if the
*& line with the requested key exists in the buffer and is not marked
*& as deleted. The read will account for the %control values from the
*& input BDEF derived type.
*&---------------------------------------------------------------------*

    "Preparing the transactional buffer
    lcl_buffer=>prep_root_buffer( VALUE #( FOR k IN keys ( k-id ) ) ).

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<read>) GROUP BY <read>-%tky.
      READ TABLE lcl_buffer=>root_buffer
        ASSIGNING FIELD-SYMBOL(<fs_r>)
        WHERE id = <read>-id AND flag <> lcl_buffer=>delete.

      IF sy-subrc = 0.

        APPEND VALUE #( %tky   = <read>-%tky
                        Number1 = COND #( WHEN <read>-%control-Number1 NE if_abap_behv=>mk-off
                                          THEN <fs_r>-Number1 )
                        Operator = COND #( WHEN <read>-%control-Operator NE if_abap_behv=>mk-off
                                          THEN <fs_r>-Operator )
                        Number2 = COND #( WHEN <read>-%control-Number2 NE if_abap_behv=>mk-off
                                          THEN <fs_r>-Number2 )
                        CalcResult = COND #( WHEN <read>-%control-CalcResult NE if_abap_behv=>mk-off
                                             THEN <fs_r>-calcresult )
                       ) TO result.

      ELSE.

        "Filling FAILED and REPORTED response parameters
        APPEND VALUE #( %tky         = <read>-%tky
                        %fail-cause  = if_abap_behv=>cause-not_found
                    ) TO failed-demo_abap.

        APPEND VALUE #( %tky = <read>-%tky
                        %msg = new_message_with_text(
                          severity  = if_abap_behv_message=>severity-error
                          text      = 'Read operation failed.' )
                    ) TO reported-demo_abap.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD lock.

*&---------------------------------------------------------------------*
*& The example implementation includes the instantiation of a lock
*& object and processing requested keys sequentially.
*&---------------------------------------------------------------------*

    TRY.
        DATA(lo_lock) = cl_abap_lock_object_factory=>get_instance(
                        iv_name = 'EZDEMOABAPL97' ).

        LOOP AT keys REFERENCE INTO DATA(lr_key).
          TRY.
              lo_lock->enqueue( it_parameter = VALUE #( ( name = 'id' value = REF #( lr_key->id ) ) ) ).
            CATCH cx_abap_foreign_lock.
              APPEND VALUE #( %key        = CORRESPONDING #( lr_key->* )
                              %fail-cause = if_abap_behv=>cause-locked ) TO failed-demo_abap.
            CATCH cx_abap_lock_failure.
              APPEND VALUE #( %key        = CORRESPONDING #( lr_key->* )
                              %fail-cause = if_abap_behv=>cause-unspecific ) TO failed-demo_abap.
          ENDTRY.
        ENDLOOP.
      CATCH cx_abap_lock_failure.
        APPEND VALUE #( %fail-cause = if_abap_behv=>cause-unspecific ) TO failed-demo_abap.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZR_DEMO_ABAP97 DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.
    METHODS finalize REDEFINITION.
    METHODS check_before_save REDEFINITION.
    METHODS save REDEFINITION.
    METHODS cleanup REDEFINITION.
    METHODS cleanup_finalize REDEFINITION.
ENDCLASS.

CLASS lsc_ZR_DEMO_ABAP97 IMPLEMENTATION.

  METHOD finalize.

*&---------------------------------------------------------------------*
*& The example implementation in this saver method takes on the role
*& similar to the determination in the RAP calculator managed demo:
*& Performing calculations based on the provided field values.
*& The calculation result value (which is intentionally of type string)
*& is assigned a specific string in case of specific errors.
*&---------------------------------------------------------------------*

    LOOP AT lcl_buffer=>root_buffer ASSIGNING FIELD-SYMBOL(<calc>) WHERE flag <> lcl_buffer=>delete.

      TRY.
          <calc>-calcresult = SWITCH #( <calc>-Operator
                                        WHEN '+' THEN |{ CONV decfloat34( <calc>-Number1 + <calc>-Number2 ) STYLE = SIMPLE }|
                                        WHEN '-' THEN |{ CONV decfloat34( <calc>-Number1 - <calc>-Number2 ) STYLE = SIMPLE }|
                                        WHEN '*' THEN |{ CONV decfloat34( <calc>-Number1 * <calc>-Number2 ) STYLE = SIMPLE }|
                                        WHEN '/' THEN |{ CONV decfloat34( <calc>-Number1 / <calc>-Number2 ) STYLE = SIMPLE }|
                                        WHEN 'P' THEN ipow( base = <calc>-Number1 exp = <calc>-Number2 )
                                        ELSE `Wrong operator` ).

          "If the first operand is also 0 in a zero division, the exception is not raised.
          "Therefore, explicitly raising the exception in this example.
          IF <calc>-Number1 = 0 AND <calc>-Number2 = 0 AND <calc>-Operator = `/`.
             RAISE EXCEPTION TYPE cx_sy_zerodivide.
          ENDIF.

        CATCH cx_sy_zerodivide.
          <calc>-calcresult = `Division by 0`.
        CATCH cx_sy_arithmetic_overflow.
          <calc>-calcresult = `Overflow error`.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_before_save.

*&---------------------------------------------------------------------*
*& The example implementation in this saver method takes on the role
*& similar to the validation in the RAP calculator managed demo:
*& Evaluating the calculation result value (which is intentionally
*& of type string). In case a specific string is available for the
*& calculation result, an error message is returned.
*&---------------------------------------------------------------------*

    LOOP AT lcl_buffer=>root_buffer ASSIGNING FIELD-SYMBOL(<fs>) WHERE flag <> lcl_buffer=>delete.

      IF <fs>-calcresult = `Wrong operator`.
        APPEND VALUE #( id = <fs>-id ) TO failed-demo_abap.

        APPEND VALUE #( id = <fs>-id
                        %msg = new_message_with_text( text     = 'Only + - * / P allowed as operators.'
                                                      severity = if_abap_behv_message=>severity-error )
                       "%element highlights the input field
                        %element-operator = if_abap_behv=>mk-on
                      ) TO reported-demo_abap.

      ELSEIF <fs>-calcresult = `Division by 0`.
        APPEND VALUE #( id = <fs>-id ) TO failed-demo_abap.

        APPEND VALUE #( id = <fs>-id
                        %msg        = new_message_with_text( text     = 'Zero division not possible.'
                                                             severity = if_abap_behv_message=>severity-error )
                        %element-operator = if_abap_behv=>mk-on
                        %element-number2 = if_abap_behv=>mk-on
                      ) TO reported-demo_abap.

      ELSEIF <fs>-calcresult = `Overflow error`.
        APPEND VALUE #( id = <fs>-id ) TO failed-demo_abap.

        APPEND VALUE #( id = <fs>-id
                        %msg       = new_message_with_text( text     = 'Check the numbers. Try smaller ones.'
                                                            severity = if_abap_behv_message=>severity-error )
                        %element-number1 = if_abap_behv=>mk-on
                        %element-number2 = if_abap_behv=>mk-on
                      ) TO reported-demo_abap.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD save.

*&---------------------------------------------------------------------*
*& The example implementation of the save method evaluates the
*& transactional buffer's content based on the flags (the component typed
*& with the enumeration type). If instances are marked for creation or
*& update, the method executes an ABAP SQL MODIFY statement.
*& If instances are marked for deletion, it executes an ABAP SQL
*& DELETE statement.
*&---------------------------------------------------------------------*

    DATA bo_instances TYPE TABLE OF zdemoabap97.

    bo_instances = VALUE #( FOR cu IN lcl_buffer=>root_buffer WHERE ( flag = lcl_buffer=>create OR flag = lcl_buffer=>update  ) ( CORRESPONDING #( cu ) ) ).

    IF bo_instances IS NOT INITIAL.
      MODIFY zdemoabap97 FROM TABLE @bo_instances.
    ENDIF.

    bo_instances = VALUE #( FOR d IN lcl_buffer=>root_buffer WHERE ( flag = lcl_buffer=>delete ) ( CORRESPONDING #( d ) ) ).

    IF bo_instances IS NOT INITIAL.
      DELETE zdemoabap97 FROM TABLE @bo_instances.
    ENDIF.
  ENDMETHOD.

  METHOD cleanup.
    CLEAR lcl_buffer=>root_buffer.
  ENDMETHOD.

  METHOD cleanup_finalize.
    CLEAR lcl_buffer=>root_buffer.
  ENDMETHOD.
ENDCLASS.
