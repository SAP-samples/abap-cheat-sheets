***********************************************************************
*
*          RAP BO provider (i. e. ABAP behavior pool/ABP)
*                  for a RAP demo scenario
*
* - RAP scenario: managed RAP BO, external numbering
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
* self-implemented in an ABP. The example is is intentionally kept
* short and simple and focuses on specific RAP aspects. For this reason,
* the example might not fully meet the requirements of the RAP BO contract.
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

CLASS lhc_root DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR root RESULT result.

    METHODS multiply_by_2 FOR MODIFY
      IMPORTING keys FOR ACTION root~multiply_by_2.

    METHODS det_add_text FOR DETERMINE ON SAVE
      IMPORTING keys FOR root~det_add_text.

    METHODS val FOR VALIDATE ON SAVE
      IMPORTING keys FOR root~val.

ENDCLASS.

CLASS lhc_root IMPLEMENTATION.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD multiply_by_2.

    "Retrieving instances based on requested keys
    READ ENTITIES OF zdemo_abap_rap_ro_m IN LOCAL MODE
      ENTITY root
      FIELDS ( field3 field4 ) WITH CORRESPONDING #( keys )
      RESULT DATA(result)
      FAILED failed.

    "If read result is initial, stop further method execution.
    CHECK result IS NOT INITIAL.

    "Multiply integer values by 2
    MODIFY ENTITIES OF zdemo_abap_rap_ro_m IN LOCAL MODE
      ENTITY root
      UPDATE FIELDS ( field3 field4 ) WITH VALUE #( FOR key IN result ( %tky   = key-%tky
                                                                        field3 = key-field3 * 2
                                                                        field4 = key-field4 * 2 ) ).
  ENDMETHOD.

  METHOD det_add_text.

    READ ENTITIES OF zdemo_abap_rap_ro_m IN LOCAL MODE
      ENTITY root
       FIELDS ( field2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_res).

    "If read result is initial, stop further method execution.
    CHECK lt_res IS NOT INITIAL.

    "field2 is changed
    MODIFY ENTITIES OF zdemo_abap_rap_ro_m IN LOCAL MODE
        ENTITY root
          UPDATE FIELDS ( field2 )
          WITH VALUE #( FOR key IN lt_res ( %tky   = key-%tky
                                            field2 = |{ key-field2 }_#| ) ).

  ENDMETHOD.

  METHOD val.

    READ ENTITIES OF zdemo_abap_rap_ro_m IN LOCAL MODE
       ENTITY root
        FIELDS ( field3 ) WITH CORRESPONDING #( keys )
       RESULT DATA(lt_res).

    "If read result is initial, stop further method execution.
    CHECK lt_res IS NOT INITIAL.

    LOOP AT lt_res ASSIGNING FIELD-SYMBOL(<fs_res>).
      IF <fs_res>-field3 > 1000.
        APPEND VALUE #( %tky = <fs_res>-%tky
                        %fail-cause = if_abap_behv=>cause-disabled
                     )
                     TO failed-root.

        APPEND VALUE #( %tky = <fs_res>-%tky
                        %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Validation failed!' )
                      ) TO reported-root.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
