CLASS lhc_RootEntity DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS new_instances FOR MODIFY
      IMPORTING keys FOR ACTION RootEntity~new_instances.
    METHODS calc_inst FOR READ
      IMPORTING keys FOR FUNCTION RootEntity~calc_inst RESULT result.
    METHODS execute_functions FOR MODIFY
      IMPORTING keys FOR ACTION RootEntity~execute_functions.
    METHODS calc_stat FOR READ
      IMPORTING keys FOR FUNCTION RootEntity~calc_stat RESULT result.
    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE RootEntity.
    CONSTANTS characters TYPE string VALUE `abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789`.
    TYPES root_struct_type TYPE zr_rootentity95.
    TYPES child_tab_type TYPE TABLE OF zr_childentity95.
    METHODS get_random_values IMPORTING child_instances TYPE i OPTIONAL
                              EXPORTING root            TYPE root_struct_type
                                        child           TYPE child_tab_type.
ENDCLASS.

CLASS lhc_RootEntity IMPLEMENTATION.

  METHOD new_instances.

*&---------------------------------------------------------------------*
*& Purpose
*& The static factory action, specified with input parameter, is used
*& for a deep create operation (i.e. creating a root instance and n
*& related child instances). The action is intended as convenience
*& functionality to automatically create root and child instances.
*&
*& Implementation
*& The demo implementation includes an ABAP EML MODIFY statement that
*& performs a deep create. Before the statement, BDEF derived types
*& used by the statement are populated with random demo values.
*&---------------------------------------------------------------------*

    IF keys IS INITIAL.
      RETURN.
    ENDIF.

    DATA(random_num4text) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 0
                                              max  = strlen( characters ) - 1 ).

    DATA root_tab TYPE TABLE FOR CREATE zr_rootentity95.
    DATA child_tab TYPE TABLE FOR CREATE zr_rootentity95\_ChildEntity.


    "Deep create
    LOOP AT keys INTO DATA(k).
      DATA(tabix) = sy-tabix.

      APPEND VALUE #( %cid = k-%cid ) TO root_tab ASSIGNING FIELD-SYMBOL(<fs>).

      "Restriction to not create too many instances (10 child instances at max)
      IF k-%param-num > 10.
        k-%param-num = 10.
      ENDIF.
      IF k-%param-num <= 0.
        k-%param-num = 0.
      ENDIF.

      get_random_values(
        EXPORTING
          child_instances =  k-%param-num
        IMPORTING
          root            = DATA(root_instance)
          child           = DATA(child_instances)
      ).

      <fs> = CORRESPONDING #( BASE ( <fs> ) root_instance ).

      APPEND VALUE #( %cid_ref = k-%cid ) TO child_tab ASSIGNING FIELD-SYMBOL(<ch>).

      LOOP AT child_instances INTO DATA(child_ent).
        DATA(tabix_ch) = sy-tabix.

        APPEND VALUE #( %cid = |cid{ tabix }{ tabix_ch }|
            parentid = child_ent-parentid
            childtext = child_ent-childtext
        text          = child_ent-text
        num   = child_ent-num
         ) TO <ch>-%target.

      ENDLOOP.
    ENDLOOP.

    MODIFY ENTITIES OF zr_rootentity95 IN LOCAL MODE
      ENTITY RootEntity
      CREATE FIELDS ( roottext Text1 Text2 Num1 Num2 ) WITH root_tab
      CREATE BY \_ChildEntity FIELDS ( childtext Text Num ) WITH child_tab
      MAPPED mapped
      FAILED failed
      REPORTED reported.

    IF failed IS NOT INITIAL.
      LOOP AT failed-rootentity ASSIGNING FIELD-SYMBOL(<r>).
        APPEND VALUE #(  %cid = <r>-%cid
                          %fail-cause  = if_abap_behv=>cause-unspecific
                             )
                         TO failed-rootentity.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_random_values.

*&---------------------------------------------------------------------*
*& The purpose of this non-handler method is to provide text and numeric
*& fields with random values.
*&---------------------------------------------------------------------*

    DATA(random_num4text) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 0
                                              max  = strlen( characters ) - 1 ).

    DATA(random_num) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 1
                                              max  = 100 ).

    DO 3 TIMES.
      DATA(idx) = sy-index.
      DO 10 TIMES.
        DATA(index) = random_num4text->get_next( ).
        CASE idx.
          WHEN 1.
            root-roottext &&= characters+index(1).
          WHEN 2.
            root-Text1 &&= characters+index(1).
          WHEN 3.
            root-Text2 &&= characters+index(1).
        ENDCASE.
      ENDDO.
    ENDDO.
    root-Num1 = random_num->get_next( ).
    root-Num2 = random_num->get_next( ).

    DO child_instances TIMES.
      APPEND INITIAL LINE TO child ASSIGNING FIELD-SYMBOL(<ch>).

      DO 2 TIMES.
        idx = sy-index.
        DO 10 TIMES.
          index = random_num4text->get_next( ).
          CASE idx.
            WHEN 1.
              <ch>-childtext &&= characters+index(1).
            WHEN 2.
              <ch>-Text &&= characters+index(1).
          ENDCASE.
        ENDDO.
      ENDDO.
      <ch>-Num = random_num->get_next( ).
    ENDDO.
  ENDMETHOD.

  METHOD earlynumbering_create.

*&---------------------------------------------------------------------*
*& The implementation is required if unmanaged internal early
*& numbering is specified in the BDEF. The demo implementation
*& includes the use of the cl_numberrange_runtime class to retrieve
*& the numbers. Alternatively, to avoid the numbering using the
*& number range object, you can comment out the code and comment in
*& the dummy implementation after the divider.
*& Here, the max value of the key field is retrieved from the
*& database table. The next higher number is calculated by adding 1.
*&---------------------------------------------------------------------*

    DATA: entity TYPE STRUCTURE FOR CREATE zr_rootentity95,
          id_max TYPE zr_rootentity95-id.

    DATA(entities_wo_id) = entities.
    DELETE entities_wo_id WHERE id IS NOT INITIAL.

    "Getting numbers
    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            nr_range_nr       = zcl_demo_abap_rap_example_util=>nrrangenr
            object            = zcl_demo_abap_rap_example_util=>number_range_object95
            quantity          = CONV #( lines( entities_wo_id ) )
          IMPORTING
            number            = DATA(number_range_key)
            returncode        = DATA(number_range_return_code)
            returned_quantity = DATA(number_range_returned_quantity)
        ).

      CATCH cx_number_ranges INTO DATA(lx_number_ranges).
        LOOP AT entities_wo_id INTO entity.

          APPEND VALUE #( %cid = entity-%cid
                          %key = entity-%key
                        ) TO failed-rootentity.

          APPEND VALUE #( %cid   = entity-%cid
                            %key = entity-%key
                            %msg = new_message_with_text( text     = 'Numbering not possible'
                                                          severity = if_abap_behv_message=>severity-error )
                        ) TO reported-rootentity.
        ENDLOOP.
        EXIT.
    ENDTRY.

    CASE number_range_return_code.
      WHEN '1'.
        "The returned number is in a critical range
        LOOP AT entities_wo_id INTO entity.
          APPEND VALUE #( %cid = entity-%cid
                          %key = entity-%key
                          %msg = new_message_with_text( text     = 'Numbering not possible, return code 1'
                                                         severity = if_abap_behv_message=>severity-warning )
                        ) TO reported-rootentity.
        ENDLOOP.

      WHEN '2' OR '3'.
        "2: The last number of the interval was returned.
        "3: Fewer numbers are available than requested.
        LOOP AT entities_wo_id INTO entity.
          APPEND VALUE #( %cid = entity-%cid
                          %key = entity-%key
                          %msg = new_message_with_text( text     = 'Numbering not possible, return code 2/3'
                                                         severity = if_abap_behv_message=>severity-warning )
                        ) TO reported-rootentity.

          APPEND VALUE #( %cid        = entity-%cid
                          %key        = entity-%key
                          %fail-cause = if_abap_behv=>cause-conflict
                        ) TO failed-rootentity.
        ENDLOOP.
        EXIT.
    ENDCASE.

    "At this stage, all entities must have a number.
    ASSERT number_range_returned_quantity = lines( entities_wo_id ).

    id_max = number_range_key - number_range_returned_quantity.

    "Setting ID in mapped
    LOOP AT entities_wo_id INTO entity.
      id_max += 1.
      entity-id = id_max.

      APPEND VALUE #( %cid = entity-%cid
                      %key = entity-%key
                    ) TO mapped-rootentity.
    ENDLOOP.

**********************************************************************

*  SELECT SINGLE
*    FROM zrootentity95
*    FIELDS MAX( id )
*    INTO @DATA(max_id).
*
*  LOOP AT entities INTO DATA(entity).
*
*    max_id += 1.
*
*    APPEND VALUE #(
*        %cid      = entity-%cid
*        id  = max_id
*      ) TO mapped-rootentity.
*  ENDLOOP.


  ENDMETHOD.

  METHOD calc_inst.

*&---------------------------------------------------------------------*
*& Purpose
*& The function demonstrates a function action that is defined with
*& result and an CDS abstract entity as parameter.
*&
*& Implementation
*& The demo implementation includes retrieving the numeric values of
*& the num1 and num2 fields based on requested keys. An addition
*& is performed using these values. In the result, the calculation
*& result is returned.
*&---------------------------------------------------------------------*

    "Retrieving instances based on requested keys
    READ ENTITIES OF zr_rootentity95 IN LOCAL MODE
      ENTITY RootEntity
      FIELDS ( num1 num2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.

    CHECK read_result IS NOT INITIAL.

    LOOP AT read_result INTO DATA(wa).
      APPEND VALUE #( id = wa-id
                      %param-id = wa-id
                      %param-calculation = wa-num1 + wa-num2 ) TO result.
    ENDLOOP.


  ENDMETHOD.

  METHOD execute_functions.

*&---------------------------------------------------------------------*
*& Purpose
*& The action demonstrates a non-factory action that is defined
*& without any additions in the BDEF.
*&
*& Implementation
*& The demo implementation includes ABAP EML statements to execute
*& functions.
*&---------------------------------------------------------------------*

    "Retrieving instances based on requested keys
    READ ENTITIES OF zr_rootentity95 IN LOCAL MODE
      ENTITY RootEntity
      FIELDS ( num1 num2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(result)
      FAILED failed.

    "Setting %action value in the failed response parameter
    LOOP AT failed-RootEntity ASSIGNING FIELD-SYMBOL(<f>).
      <f>-%action-execute_functions = if_abap_behv=>mk-on.
    ENDLOOP.

    CHECK result IS NOT INITIAL.

    READ ENTITIES OF zr_rootentity95 IN LOCAL MODE
       ENTITY RootEntity
       EXECUTE calc_inst
       FROM CORRESPONDING #( keys )
       RESULT FINAL(calc_inst_result).

    LOOP AT calc_inst_result INTO DATA(wa_calc_inst).
      APPEND new_message_with_text( severity = if_abap_behv_message=>severity-information
                                    text = |CALC_INST: { wa_calc_inst-id ALPHA = OUT } (ID), { wa_calc_inst-%param-calculation } (CALC)| ) TO reported-%other.
    ENDLOOP.

    READ ENTITIES OF zr_rootentity95 IN LOCAL MODE
          ENTITY RootEntity
          EXECUTE calc_stat
          FROM VALUE #( ( %cid = keys[ 1 ]-%cid_ref ) )
          RESULT FINAL(calc_stat_result).

    LOOP AT calc_stat_result INTO DATA(wa_calc_stat).
      APPEND new_message_with_text( severity = if_abap_behv_message=>severity-information
                                    text = |CALC_STAT: num1 + num2 > 100 -> { wa_calc_stat-%param-calculation } instances| ) TO reported-%other.
    ENDLOOP.
  ENDMETHOD.

  METHOD calc_stat.

*&---------------------------------------------------------------------*
*& Purpose
*& The function demonstrates a static function that is defined with
*& result and an CDS abstract entity as parameter.
*&
*& Implementation
*& The demo implementation includes retrieving all persisted instances
*& from the underlying database table (for the root entity). An addition
*& is performed using the values contained in the num1 and num2 fields.
*& It is counted how many instances return a calculated value greater
*& than 100. This information is returned in the result.
*&---------------------------------------------------------------------*

    SELECT id
       FROM zrootentity95
       INTO TABLE @DATA(ids).

    IF ids IS INITIAL.
      LOOP AT keys ASSIGNING FIELD-SYMBOL(<k>).

        APPEND VALUE #( %cid              = <k>-%cid
                        %action-calc_stat = if_abap_behv=>mk-on
                        %fail-cause       = if_abap_behv=>cause-unspecific
        ) TO failed-rootentity.

        APPEND VALUE #( %cid = <k>-%cid
                        %msg = new_message_with_text(
                        severity = if_abap_behv_message=>severity-error
                        text     = 'calc_stat function failure' )
                      ) TO reported-rootentity.
      ENDLOOP.

    ELSE.

      READ ENTITIES OF zr_rootentity95 IN LOCAL MODE
        ENTITY RootEntity
        FIELDS ( num1 num2 ) WITH CORRESPONDING #( ids )
        RESULT DATA(read_result)
        FAILED failed.

      CHECK read_result IS NOT INITIAL.

      DATA instances_with_sum_gt_100 TYPE i.

      LOOP AT read_result INTO DATA(wa).
        IF ( wa-num1 + wa-num2 ) > 100.
          instances_with_sum_gt_100 += 1.
        ENDIF.
      ENDLOOP.

      LOOP AT keys INTO DATA(k).
        APPEND VALUE #( %cid = k-%cid
                        %param-id = |%cid value: { k-%cid }|
                        %param-calculation = |{ instances_with_sum_gt_100 }/{ lines( read_result ) }| ) TO result.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

