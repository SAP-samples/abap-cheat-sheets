***********************************************************************
*
*          RAP BO provider (i. e. ABAP behavior pool/ABP)
*                  for a RAP demo scenario
*
* - RAP scenario: "RAP calculator" (managed, draft-enabled RAP BO with
*   late numbering)
* - Data model: Consists of a root entity alone.
*   The BDEF defines the behavior for this entity. The definitions in the
*   BDEF determine which methods must be implemented in the ABAP behavior
*   pool (ABP). Note that the view contains many annotations for the
*   SAP Fiori UI.
*
* ----------------------------- NOTE -----------------------------------
* This simplified example is not a real life scenario and rather
* focuses on the technical side by giving an idea how the communication
* and data exchange between a RAP BO consumer, which is a class
* in this case, and RAP BO provider can work. Additionally, it shows
* how the methods for non-standard RAP BO operations might be
* self-implemented in an ABP. The example is intentionally kept
* short and simple and focuses on specific RAP aspects. For this reason,
* the example might not fully meet the requirements of the RAP BO contract.
*
* You can also use side effects to trigger data
* changes (in terms of this example, the recalculation of the calculation
* result) and other things based on data changes in UI scenarios with
* draft-enabled BOs.
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

CLASS lhc_calc DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS delete_all FOR MODIFY
      IMPORTING keys FOR ACTION calc~delete_all.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR calc RESULT result.

    METHODS validate FOR VALIDATE ON SAVE
      IMPORTING keys FOR calc~validate.

    METHODS det_modify FOR DETERMINE ON MODIFY
      IMPORTING keys FOR calc~det_modify.

    METHODS calculation FOR MODIFY
      IMPORTING keys FOR ACTION calc~calculation.

ENDCLASS.

CLASS lhc_calc IMPLEMENTATION.

  METHOD delete_all.
    "Purpose: The method deletes all persisted database entries.

    DATA all_keys TYPE TABLE FOR DELETE zdemo_abap_rap_draft_m.

    SELECT id FROM zdemo_abap_tabca INTO CORRESPONDING FIELDS OF TABLE @all_keys.

    READ ENTITIES OF zdemo_abap_rap_draft_m IN LOCAL MODE
    ENTITY calc
      ALL FIELDS WITH CORRESPONDING #( all_keys )
        RESULT DATA(lt_del).

    IF lt_del IS NOT INITIAL.

      MODIFY ENTITY IN LOCAL MODE zdemo_abap_rap_draft_m
        DELETE FROM CORRESPONDING #( lt_del ).

      APPEND VALUE #( %msg = new_message_with_text( text     = 'All persisted calculations were deleted.'
                                                    severity = if_abap_behv_message=>severity-information )
                                ) TO reported-calc.

    ELSE.
      APPEND VALUE #( %msg = new_message_with_text( text     = 'No persisted calculations available.'
                                                    severity = if_abap_behv_message=>severity-information )
                                ) TO reported-calc.

    ENDIF.

  ENDMETHOD.

  METHOD get_global_authorizations.
    "Purposely kept without implementation.
  ENDMETHOD.

  METHOD validate.

    "Retrieving instances based on requested keys
    READ ENTITIES OF zdemo_abap_rap_draft_m IN LOCAL MODE
     ENTITY calc
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(result_validate)
     FAILED DATA(f).

    CHECK result_validate IS NOT INITIAL.

    "Various calculation errors are handled.
    LOOP AT result_validate ASSIGNING FIELD-SYMBOL(<fs>).

      APPEND VALUE #(  %tky        = <fs>-%tky
                       %state_area = 'VALIDATE_CALCULATION'
                    ) TO reported-calc.

      IF <fs>-calc_result = `Wrong operator`.
        APPEND VALUE #( %tky = <fs>-%tky ) TO failed-calc.

        APPEND VALUE #( %tky        = <fs>-%tky
                        %state_area = 'VALIDATE_CALCULATION'
                         %msg       = new_message_with_text( text     = 'Only + - * / P allowed as operators.'
                                                             severity = if_abap_behv_message=>severity-error )
                       "%element highlights the input field
                        %element-arithm_op = if_abap_behv=>mk-on
                      ) TO reported-calc.

      ELSEIF <fs>-calc_result = `Division by 0`.

        APPEND VALUE #( %tky = <fs>-%tky ) TO failed-calc.

        APPEND VALUE #( %tky        = <fs>-%tky
                        %state_area = 'VALIDATE_CALCULATION'
                        %msg        = new_message_with_text( text     = 'Zero division not possible.'
                                                             severity = if_abap_behv_message=>severity-error )
                        %element-arithm_op = if_abap_behv=>mk-on
                        %element-num2 = if_abap_behv=>mk-on
                      ) TO reported-calc.

      ELSEIF <fs>-calc_result = `Overflow error`.

        APPEND VALUE #( %tky = <fs>-%tky ) TO failed-calc.

        APPEND VALUE #( %tky        = <fs>-%tky
                        %state_area = 'VALIDATE_CALCULATION'
                         %msg       = new_message_with_text( text     = 'Check the numbers. Try smaller ones.'
                                                             severity = if_abap_behv_message=>severity-error )
                       %element-num1 = if_abap_behv=>mk-on
                       %element-num2 = if_abap_behv=>mk-on
                      ) TO reported-calc.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD det_modify.

    MODIFY ENTITIES OF zdemo_abap_rap_draft_m IN LOCAL MODE
       ENTITY calc
         EXECUTE calculation
         FROM CORRESPONDING #( keys ).

  ENDMETHOD.

  METHOD calculation.

    READ ENTITIES OF zdemo_abap_rap_draft_m IN LOCAL MODE
          ENTITY calc
            FIELDS ( num1 num2 arithm_op ) WITH CORRESPONDING #( keys )
            RESULT DATA(lt_calc)
            FAILED DATA(f).

    LOOP AT lt_calc ASSIGNING FIELD-SYMBOL(<calc>).

      TRY.
          <calc>-calc_result = SWITCH #( <calc>-arithm_op
                                          WHEN `+` THEN <calc>-num1 + <calc>-num2
                                          WHEN `-` THEN <calc>-num1  -  <calc>-num2
                                          WHEN `*` THEN  <calc>-num1  * <calc>-num2
                                          WHEN `/` THEN <calc>-num1  /  <calc>-num2
                                          WHEN `P` THEN ipow( base = <calc>-num1 exp = <calc>-num2 )
                                          ELSE `Wrong operator` ).
          "Bringing "-" to the front in case of negative values in the string
          IF <calc>-calc_result CA `-`.
            <calc>-calc_result = shift_right( val = <calc>-calc_result circular = 1 ).
          ENDIF.

          "Removing trailing .0 from the string
          REPLACE PCRE `\.0+\b` IN <calc>-calc_result WITH ``.

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

    MODIFY ENTITY IN LOCAL MODE zdemo_abap_rap_draft_m
            UPDATE FIELDS ( calc_result )
            WITH CORRESPONDING #( lt_calc ).

  ENDMETHOD.

ENDCLASS.

CLASS lsc_zdemo_abap_rap_draft_m DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS adjust_numbers REDEFINITION.

ENDCLASS.

CLASS lsc_zdemo_abap_rap_draft_m IMPLEMENTATION.

  METHOD adjust_numbers.

    "The newly created entity instances are given their final key
    "only shortly before saving in the database in the adjust_numbers method.
    "Until then, the business logic uses a temporary key that has to be replaced.
    "In this very simplified example, the key 'id' is purposely typed with the
    "type sysuuid_x16 which can accept the value used in %pid to finally ensure
    "that there is a unique key and the instance can be stored in the database.
    "Hence, the final key 'id' is in this example just the value used for %pid.
    LOOP AT mapped-calc ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-%key-id = <fs>-%pid.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
