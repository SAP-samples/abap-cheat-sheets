CLASS lhc_zdemo_abap_rap_ro_m_as DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR root RESULT result.

    METHODS calc FOR MODIFY
      IMPORTING keys FOR ACTION root~calc.

    METHODS det_modify FOR DETERMINE ON MODIFY
      IMPORTING keys FOR root~det_modify.

ENDCLASS.

CLASS lhc_zdemo_abap_rap_ro_m_as IMPLEMENTATION.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD calc.
    READ ENTITY IN LOCAL MODE zdemo_abap_rap_ro_m_as
              FIELDS ( num1 num2 arithm_op crea_date_time lchg_date_time ) WITH CORRESPONDING #( keys )
              RESULT DATA(lt_calc)
              FAILED DATA(f).

    DATA(timestamp) = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).

    LOOP AT lt_calc ASSIGNING FIELD-SYMBOL(<calc>).
      TRY.
          <calc>-calc_result = SWITCH #( <calc>-arithm_op
                                         WHEN `+` THEN |{ CONV decfloat34( <calc>-num1 + <calc>-num2 ) STYLE = SIMPLE }|
                                         WHEN `-` THEN |{ CONV decfloat34( <calc>-num1 - <calc>-num2 ) STYLE = SIMPLE }|
                                         WHEN `*` THEN |{ CONV decfloat34( <calc>-num1 * <calc>-num2 ) STYLE = SIMPLE }|
                                         WHEN `/` THEN |{ CONV decfloat34( <calc>-num1 / <calc>-num2 ) STYLE = SIMPLE }|
                                         WHEN `P` THEN |{ CONV decfloat34( ipow( base = <calc>-num1 exp = <calc>-num2 ) ) STYLE = SIMPLE }|
                                         ELSE `Wrong operator` ).

          "Handling the fact that ABAP allows division by zero if the dividend itself is zero.
          IF <calc>-num1 = 0 AND <calc>-num2 = 0 AND <calc>-arithm_op = `/`.
            <calc>-calc_result = `Division by 0`.
          ENDIF.
        CATCH cx_sy_zerodivide.
          <calc>-calc_result = `Division by 0`.
        CATCH cx_sy_arithmetic_overflow.
          <calc>-calc_result = `Overflow error`.
      ENDTRY.
    ENDLOOP.

    MODIFY ENTITY IN LOCAL MODE zdemo_abap_rap_ro_m_as
        UPDATE FIELDS ( calc_result )
        WITH CORRESPONDING #( lt_calc ).
  ENDMETHOD.

  METHOD det_modify.
    MODIFY ENTITY IN LOCAL MODE zdemo_abap_rap_ro_m_as
        EXECUTE calc
        FROM CORRESPONDING #( keys ).
  ENDMETHOD.

ENDCLASS.

CLASS lsc_zdemo_abap_rap_ro_m_as DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zdemo_abap_rap_ro_m_as IMPLEMENTATION.

  METHOD save_modified.
    IF create-root IS NOT INITIAL.
      RAISE ENTITY EVENT zdemo_abap_rap_ro_m_as~created
        FROM VALUE #( FOR <cr> IN create-root (
          %key = VALUE #( id = <cr>-id ) ) ).

      zbp_demo_abap_rap_ro_m_as=>num_raised_events = zbp_demo_abap_rap_ro_m_as=>num_raised_events + lines( create-root ).
    ENDIF.

    IF update-root IS NOT INITIAL.
      "Demonstrating the BDEF derived type TYPE TABLE FOR EVENT
      DATA evt_tab_up TYPE TABLE FOR EVENT zdemo_abap_rap_ro_m_as~updated.

      evt_tab_up = VALUE #( FOR <up> IN update-root INDEX INTO updidx (
              %key   = VALUE #( id = <up>-id )
              %param    = VALUE #( col1 = 'Event raised'
                                   col2 = |UPDATED ({ updidx })| ) ) ).
      RAISE ENTITY EVENT zdemo_abap_rap_ro_m_as~updated FROM evt_tab_up.

      zbp_demo_abap_rap_ro_m_as=>num_raised_events = zbp_demo_abap_rap_ro_m_as=>num_raised_events + lines( update-root ).
    ENDIF.

    IF delete-root IS NOT INITIAL.
      RAISE ENTITY EVENT zdemo_abap_rap_ro_m_as~deleted
        FROM VALUE #( FOR <del> IN delete-root INDEX INTO delidx (
          %key = VALUE #( id = <del>-id )
          %param         = VALUE #( col1 = 'Event raised'
                                    col2 = |DELETED ({ delidx })| ) ) ).

      zbp_demo_abap_rap_ro_m_as=>num_raised_events = zbp_demo_abap_rap_ro_m_as=>num_raised_events + lines( delete-root ).
    ENDIF.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
