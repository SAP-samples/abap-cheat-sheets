CLASS lhc_demo_abap DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS calculation FOR MODIFY
      IMPORTING keys FOR ACTION demo_abap~calculation.

    METHODS det_modify FOR DETERMINE ON MODIFY
      IMPORTING keys FOR demo_abap~det_modify.

    METHODS validate FOR VALIDATE ON SAVE
      IMPORTING keys FOR demo_abap~validate.

ENDCLASS.

CLASS lhc_demo_abap IMPLEMENTATION.

  METHOD det_modify.

*&---------------------------------------------------------------------*
*& In the example, the (re)calculation is triggered during the
*& determination call. The concrete calculation is implemented in an
*& internal action.
*&---------------------------------------------------------------------*

    MODIFY ENTITIES OF zr_demo_abap99 IN LOCAL MODE
      ENTITY demo_abap
          EXECUTE calculation
          FROM CORRESPONDING #( keys ).

  ENDMETHOD.

  METHOD validate.

    "Retrieving instances based on requested keys
    READ ENTITIES OF zr_demo_abap99 IN LOCAL MODE
     ENTITY demo_abap
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(result_validate).

    CHECK result_validate IS NOT INITIAL.

    CONSTANTS state_area TYPE string VALUE `VALIDATE_CALCULATION`.


    LOOP AT result_validate ASSIGNING FIELD-SYMBOL(<fs>).

      APPEND VALUE #(  %tky        = <fs>-%tky
                       %state_area = state_area
                    ) TO reported-demo_abap.

      IF <fs>-calcresult = `Wrong operator`.
        APPEND VALUE #( %tky = <fs>-%tky ) TO failed-demo_abap.

        APPEND VALUE #( %tky        = <fs>-%tky
                        %state_area = state_area
                         %msg       = new_message_with_text( text     = 'Only + - * / P allowed as operators.'
                                                             severity = if_abap_behv_message=>severity-error )
                       "Note that flagging the %element component highlights
                       "the input field on the UI.
                        %element-operator = if_abap_behv=>mk-on
                      ) TO reported-demo_abap.

      ELSEIF <fs>-calcresult = `Division by 0`.

        APPEND VALUE #( %tky = <fs>-%tky ) TO failed-demo_abap.

        APPEND VALUE #( %tky        = <fs>-%tky
                        %state_area = state_area
                        %msg        = new_message_with_text( text     = 'Zero division not possible.'
                                                             severity = if_abap_behv_message=>severity-error )
                        %element-operator = if_abap_behv=>mk-on
                        %element-number2 = if_abap_behv=>mk-on
                      ) TO reported-demo_abap.

      ELSEIF <fs>-calcresult = `Overflow error`.

        APPEND VALUE #( %tky = <fs>-%tky ) TO failed-demo_abap.

        APPEND VALUE #( %tky        = <fs>-%tky
                        %state_area = state_area
                         %msg       = new_message_with_text( text     = 'Check the numbers. Try smaller ones.'
                                                             severity = if_abap_behv_message=>severity-error )
                       %element-number1 = if_abap_behv=>mk-on
                       %element-number2 = if_abap_behv=>mk-on
                      ) TO reported-demo_abap.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD calculation.

*&---------------------------------------------------------------------*
*& "The method implementation includes the handling various
*& calculation errors. In the simplified example, the calculation
*& result value (which is intentionally of type string) is
*& evaluated. In case specific errors were raised (such as zero
*& division), a specific string is available for the calculation
*& result. Based on this string, an error message is returned.
*&---------------------------------------------------------------------*

    "Reading the fields of the instances required for calculation
    READ ENTITIES OF zr_demo_abap99 IN LOCAL MODE
            ENTITY demo_abap
              FIELDS ( Number1 Number2 Operator ) WITH CORRESPONDING #( keys )
              RESULT DATA(calc).

    LOOP AT calc ASSIGNING FIELD-SYMBOL(<calc>).
      TRY.
          <calc>-CalcResult = SWITCH #( <calc>-Operator
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
          <calc>-CalcResult = `Division by 0`.
        CATCH cx_sy_arithmetic_overflow.
          <calc>-CalcResult = `Overflow error`.
      ENDTRY.
    ENDLOOP.

    MODIFY ENTITY IN LOCAL MODE zr_demo_abap99
      UPDATE FIELDS ( calcresult )
      WITH CORRESPONDING #( calc ).
  ENDMETHOD.

ENDCLASS.

CLASS lsc_zr_demo_abap99 DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS adjust_numbers REDEFINITION.

ENDCLASS.

CLASS lsc_zr_demo_abap99 IMPLEMENTATION.

  METHOD adjust_numbers.

*&---------------------------------------------------------------------*
*& NOTE
*&
*& This example uses the demo number range object ZNR_DEMO99.
*& You can maintain the number range manually using an SAP Fiori app as
*& described in the ABAP cheat sheet. Alternatively, you can use the
*& released API cl_numberrange_intervals to maintain the number range
*& for the demo. To maintain the number range, you can run the utility
*& class zcl_demo_abap_rap_example_util in ADT using F9 (see the
*& setup_number_range method).
*& Preparing the number range is a prerequisite for the example. For
*& convenience (if the number range is not maintained), a simplified
*& implementation is included (just retrieving the max value of the key
*& + 1). Comment out/in the code appropriately.
*&---------------------------------------------------------------------*

    LOOP AT mapped-demo_abap ASSIGNING FIELD-SYMBOL(<fs>).
      TRY.
          cl_numberrange_runtime=>number_get( EXPORTING nr_range_nr = zcl_demo_abap_rap_example_util=>nrrangenr
                                                        object      = zcl_demo_abap_rap_example_util=>number_range_object99
                                              IMPORTING number      = DATA(number) ).

          <fs>-%key-id = number.
        CATCH cx_nr_object_not_found cx_number_ranges.
          ASSERT 1 = 2.
      ENDTRY.
    ENDLOOP.

**********************************************************************

*    SELECT MAX( id ) FROM zdemoabap99 INTO @DATA(max_id).
*
*    LOOP AT mapped-demo_abap ASSIGNING FIELD-SYMBOL(<fs>).
*      max_id += 1.
*      <fs>-%key-id = max_id.
*    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
