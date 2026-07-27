CLASS ltc_zraunitflights DEFINITION DEFERRED FOR TESTING.
CLASS lhc_zraunitflights DEFINITION INHERITING FROM cl_abap_behavior_handler FRIENDS ltc_zraunitflights.

  PRIVATE SECTION.
    METHODS calc_occ_rate FOR MODIFY
       keys FOR ACTION zraunitflights~calc_occ_rate RESULT result.
    METHODS val FOR VALIDATE ON SAVE
       keys FOR zraunitflights~val.

ENDCLASS.

CLASS lhc_zraunitflights IMPLEMENTATION.

  METHOD calc_occ_rate.

    READ ENTITY IN LOCAL MODE zraunitflights
      FIELDS ( Seatsmax Seatsocc ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.

    CHECK read_result IS NOT INITIAL.

    LOOP AT read_result INTO DATA(wa).
      APPEND VALUE #( %tky = wa-%tky
                      %param = round( val = wa-Seatsocc / wa-Seatsmax * 100 dec = 2 ) ) TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD val.

    READ ENTITY IN LOCAL MODE zraunitflights
       FIELDS ( Seatsmax Seatsocc ) WITH CORRESPONDING #( keys )
       RESULT DATA(read_result).

    CHECK read_result IS NOT INITIAL.

    LOOP AT read_result INTO DATA(wa).
      IF wa-Seatsmax < 0
      OR wa-Seatsocc > wa-Seatsmax
      OR wa-Seatsocc < 0.
        APPEND VALUE #( %tky = wa-%tky
                        %fail-cause = if_abap_behv=>cause-unspecific )
                     TO failed-zraunitflights.

        APPEND VALUE #( %tky = wa-%tky
                        %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Validation failed' )
                      ) TO reported-zraunitflights.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
