CLASS lhc_zr_demo_abap98 DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR demo_abap RESULT result.

    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR demo_abap RESULT result.

    METHODS addition FOR MODIFY
      IMPORTING keys FOR ACTION demo_abap~addition RESULT result.

    METHODS add_random_numbers FOR MODIFY
      IMPORTING keys FOR ACTION demo_abap~add_random_numbers RESULT result.

    METHODS copy_instance FOR MODIFY
      IMPORTING keys FOR ACTION demo_abap~copy_instance.

    METHODS count FOR MODIFY
      IMPORTING keys FOR ACTION demo_abap~count.

    METHODS create_random_strings FOR MODIFY
      IMPORTING keys FOR ACTION demo_abap~create_random_strings.

    METHODS increment_numbers FOR MODIFY
      IMPORTING keys FOR ACTION demo_abap~increment_numbers RESULT result.

    METHODS lowercase FOR MODIFY
      IMPORTING keys FOR ACTION demo_abap~lowercase RESULT result.

    METHODS precheck_lowercase FOR PRECHECK
      IMPORTING keys FOR ACTION demo_abap~lowercase.

    METHODS new_instance FOR MODIFY
      IMPORTING keys FOR ACTION demo_abap~new_instance.

    METHODS reverse_text FOR MODIFY
      IMPORTING keys FOR ACTION demo_abap~reverse_text RESULT result.

    METHODS uppercase FOR MODIFY
      IMPORTING keys FOR ACTION demo_abap~uppercase.
ENDCLASS.

CLASS lhc_zr_demo_abap98 IMPLEMENTATION.

  METHOD uppercase.

*&---------------------------------------------------------------------*
*& Purpose
*& The action demonstrates a non-factory action that is defined
*& without any additions in the BDEF.
*&
*& Implementation
*& Two text field values are transformed to uppercase.
*&---------------------------------------------------------------------*

    "Retrieving instances based on requested keys
    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      FIELDS ( text1 text2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(result)
      FAILED failed.

    "Setting %action value in the failed response parameter
    LOOP AT failed-demo_abap ASSIGNING FIELD-SYMBOL(<f>).
      <f>-%action-uppercase = if_abap_behv=>mk-on.
    ENDLOOP.

    "If read result is initial, stop further method execution.
    CHECK result IS NOT INITIAL.

    "Setting field value
    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      UPDATE FIELDS ( text1 text2 ) WITH VALUE #( FOR key IN result ( %tky   = key-%tky
                                                                      text1 = to_upper( key-text1 )
                                                                      text2 = to_upper( key-text2 ) ) ).
  ENDMETHOD.

  METHOD add_random_numbers.

*&---------------------------------------------------------------------*
*& Purpose
*& The action demonstrates a non-factory action that is defined with
*& output parameter in the BDEF.
*&
*& Implementation
*& Two numeric fields are assigned random integer values.
*&---------------------------------------------------------------------*

    "Retrieving instances based on requested keys
    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      FIELDS ( num1 num2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.

    "Setting %action value in the failed response parameter
    LOOP AT failed-demo_abap ASSIGNING FIELD-SYMBOL(<f>).
      <f>-%action-add_random_numbers = if_abap_behv=>mk-on.
    ENDLOOP.

    "If read result is initial, stop further method execution.
    CHECK read_result IS NOT INITIAL.

    DATA upd_tab TYPE TABLE FOR UPDATE zr_demo_abap98.
    LOOP AT read_result ASSIGNING FIELD-SYMBOL(<u>).
      "Populating derived type for update operation
      INSERT CORRESPONDING #( <u> ) INTO TABLE upd_tab ASSIGNING FIELD-SYMBOL(<comp>).
      "Setting field values
      <comp>-Num1 = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                min  = 1
                                                max  = 1000 )->get_next( ).

      <comp>-Num2 = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                min  = 1
                                                max  = 1000 )->get_next( ).

      "Populating result parameter
      INSERT VALUE #( %tky   = <u>-%tky
                      %param = CORRESPONDING #( <comp> ) ) INTO TABLE result.

    ENDLOOP.

    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      UPDATE FIELDS ( num1 num2 ) WITH upd_tab.
  ENDMETHOD.

  METHOD addition.

*&---------------------------------------------------------------------*
*& Purpose
*& The action demonstrates a non-factory action that is
*& defined with input and output parameters in the BDEF.
*&
*& Implementation
*& Via the preview UI, an integer value is supplied. This
*& value is used here to perform a calculation with the
*& existing values of two numeric fields.
*&---------------------------------------------------------------------*

    "Retrieving instances based on requested keys
    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      FIELDS ( num1 num2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.

    "Setting %action value in the failed response parameter
    LOOP AT failed-demo_abap ASSIGNING FIELD-SYMBOL(<f>).
      <f>-%action-add_random_numbers = if_abap_behv=>mk-on.
    ENDLOOP.

    "If read result is initial, stop further method execution.
    CHECK read_result IS NOT INITIAL.

    DATA upd_tab TYPE TABLE FOR UPDATE zr_demo_abap98.
    LOOP AT read_result ASSIGNING FIELD-SYMBOL(<u>).
      DATA(num) = keys[ KEY id %tky = <u>-%tky ]-%param-num.

      "Populating derived type for update operation
      INSERT CORRESPONDING #( <u> ) INTO TABLE upd_tab ASSIGNING FIELD-SYMBOL(<comp>).
      "Setting field value
      <comp>-Num1 += num.
      <comp>-Num2 += num.

      "Populating result parameter
      INSERT VALUE #( %tky   = <u>-%tky
                      %param = CORRESPONDING #( <comp> ) ) INTO TABLE result.

    ENDLOOP.

    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      UPDATE FIELDS ( num1 num2 ) WITH upd_tab.
  ENDMETHOD.

  METHOD count.

*&---------------------------------------------------------------------*
*& Purpose
*& The action demonstrates a non-factory static action that is
*& defined without any additions in the BDEF.
*&
*& Implementation
*& The example implementation does not operate on provided keys. It
*& retrieves and displays the number of entries currently available
*& in the database.
*&---------------------------------------------------------------------*

    SELECT COUNT( * ) FROM zdemoabap98 INTO @DATA(count).

    APPEND new_message_with_text( severity = if_abap_behv_message=>severity-information
                                  text = |Current entries in the database table: { count }| ) TO reported-%other.

  ENDMETHOD.

  METHOD new_instance.

*&---------------------------------------------------------------------*
*& Purpose
*& The action demonstrates a static factory action.
*&
*& Implementation
*& The example implementation creates new instances.
*&---------------------------------------------------------------------*

    "only %cid in keys
    "demo use case: creating new instances

    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
            ENTITY demo_abap
              CREATE FIELDS ( Text1 Text2 Num1 Num2 ) WITH
               VALUE #( FOR <instance> IN keys
                                     ( %cid      = <instance>-%cid
                                       %is_draft =

                                       "<instance>-%param-%is_draft
                                       if_abap_behv=>mk-off

*                                       Text1 = 'a'
*                                       Text2 = 'b'
                                       Num1 = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                    min  = 1
                                                    max  = 1000 )->get_next( )
                                       Num2 = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                    min  = 1
                                                    max  = 1000 )->get_next( ) ) )
              MAPPED mapped
              FAILED failed
              REPORTED reported.

    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
           ENTITY demo_abap
             FIELDS ( Text1 Text2 )
             WITH VALUE #( FOR <i> IN mapped-demo_abap ( uuid  = <i>-uuid ) )
           RESULT DATA(res).

    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
        ENTITY demo_abap
          EXECUTE create_random_strings
            FROM CORRESPONDING #( res ).
  ENDMETHOD.

  METHOD create_random_strings.

*&---------------------------------------------------------------------*
*& Purpose
*& The action demonstrates an internal action that is called by the
*& new_instance action.
*&
*& Implementation
*& The example implementation provides random string values for the
*& two text fields.
*&---------------------------------------------------------------------*

    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
        ENTITY demo_abap
          FIELDS ( Text1 Text2 )
          WITH CORRESPONDING #( keys )
        RESULT DATA(res).

    CONSTANTS characters TYPE string VALUE `abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789`.

    DATA(random_num) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 0
                                              max  = strlen( characters ) - 1 ).

    LOOP AT res ASSIGNING FIELD-SYMBOL(<fs>).
      DO 10 TIMES.
        DATA(index) = random_num->get_next( ).
        <fs>-Text1 &&= characters+index(1).
      ENDDO.
      DO 10 TIMES.
        index = random_num->get_next( ).
        <fs>-Text2 &&= characters+index(1).
      ENDDO.

    ENDLOOP.

    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
           ENTITY demo_abap
             UPDATE FIELDS ( Text1 Text2 )
             WITH CORRESPONDING #( res ).
  ENDMETHOD.

  METHOD copy_instance.

*&---------------------------------------------------------------------*
*& Purpose
*& The action demonstrates a factory action.
*&
*& Implementation
*& The example implementation copies instances and assigns new unique
*& key values.
*&---------------------------------------------------------------------*


    "see /dmo/bp_travel_m


*    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
*      ENTITY demo_abap
*      FIELDS ( Text1 Text2 Num1 Num2 ) WITH CORRESPONDING #( keys )
*      RESULT DATA(read_result)
*      FAILED failed.
*
*    "Setting %action value in the failed response parameter
*    LOOP AT failed-demo_abap ASSIGNING FIELD-SYMBOL(<f>).
*      <f>-%action-copy_instance = if_abap_behv=>mk-on.
*    ENDLOOP.
*
*    "If read result is initial, stop further method execution.
*    CHECK read_result IS NOT INITIAL.
*
*    DATA create_tab TYPE TABLE FOR CREATE zr_demo_abap98.
*    LOOP AT read_result ASSIGNING FIELD-SYMBOL(<instance>).
*      APPEND CORRESPONDING #( <instance> ) TO create_tab ASSIGNING FIELD-SYMBOL(<line>).
*      <line>-%data = read_result[ KEY id %tky = <instance>-%tky ]-%data.
*    ENDLOOP.
*
*    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
*              ENTITY demo_abap
*                CREATE AUTO FILL CID FIELDS ( Text1 Text2 Num1 Num2 ) WITH
*                 create_tab
*                MAPPED data(m)
**                FAILED failed
**                REPORTED reported
*                .
*
*mapped-demo_abap = m-demo_abap.


**********************************************************************
    "/dmo/bp_travel_m

    DATA create_tab TYPE TABLE FOR CREATE zr_demo_abap98.

    READ TABLE keys WITH KEY %cid = '' INTO DATA(key_with_inital_cid).
    CHECK key_with_inital_cid IS INITIAL.

    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      FIELDS ( Text1 Text2 Num1 Num2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.

    LOOP AT failed-demo_abap ASSIGNING FIELD-SYMBOL(<f>).
      <f>-%action-copy_instance = if_abap_behv=>mk-on.
    ENDLOOP.

    LOOP AT keys INTO DATA(key).
      READ TABLE read_result ASSIGNING FIELD-SYMBOL(<instance>) WITH KEY id COMPONENTS %tky = key-%tky.
      IF sy-subrc = 0.
        APPEND VALUE #( %cid  = key-%cid
                        %data = CORRESPONDING #( <instance> EXCEPT uuid ) )
          TO create_tab ASSIGNING FIELD-SYMBOL(<new_travel>).
      ELSE.
        APPEND CORRESPONDING #( key MAPPING %fail = DEFAULT VALUE #( cause = if_abap_behv=>cause-dependency ) ) TO failed-demo_abap ASSIGNING FIELD-SYMBOL(<d>).
        <d>-%action-copy_instance = if_abap_behv=>mk-on.
      ENDIF.
    ENDLOOP.

    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
        ENTITY demo_abap
          CREATE AUTO FILL CID
          FIELDS ( Text1 Text2 Num1 Num2 ) WITH
           create_tab
          MAPPED DATA(mapped_create).

    mapped-demo_abap = mapped_create-demo_abap.
  ENDMETHOD.

  METHOD lowercase.

*&---------------------------------------------------------------------*
*& Purpose
*& The action demonstrates a non-factory action that is defined with
*& precheck and output parameter in the BDEF. The precheck prevents
*& unwanted changes from reaching the application buffer. The precheck
*& implementation is contained in the precheck_lowercase method.
*&
*& Implementation
*& The example implementation transform the values of two text
*& fields to lowercase.
*&---------------------------------------------------------------------*


*   "Retrieving instances based on requested keys
*    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
*      ENTITY demo_abap
*      FIELDS ( text1 text2 ) WITH CORRESPONDING #( keys )
*      RESULT DATA(read_result)
*      FAILED failed.
*
*    "Setting %action value in the failed response parameter
*    LOOP AT failed-demo_abap ASSIGNING FIELD-SYMBOL(<f>).
*      <f>-%action-uppercase = if_abap_behv=>mk-on.
*    ENDLOOP.
*
*    "If read result is initial, stop further method execution.
*    CHECK read_result IS NOT INITIAL.
*
*    "Setting field value
*    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
*      ENTITY demo_abap
*      UPDATE FIELDS ( text1 text2 ) WITH VALUE #( FOR key IN result ( %tky   = key-%tky
*                                                                      text1 = to_lower( key-%param-text1 )
*                                                                      text2 = to_lower( key-%param-text2 ) ) ).
*


    "Retrieving instances based on requested keys
    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      FIELDS ( Text1 Text2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.

    "Setting %action value in the failed response parameter
    LOOP AT failed-demo_abap ASSIGNING FIELD-SYMBOL(<f>).
      <f>-%action-lowercase = if_abap_behv=>mk-on.
    ENDLOOP.

    "If read result is initial, stop further method execution.
    CHECK read_result IS NOT INITIAL.

    DATA upd_tab TYPE TABLE FOR UPDATE zr_demo_abap98.
    LOOP AT read_result ASSIGNING FIELD-SYMBOL(<u>).
      "Populating derived type for update operation
      INSERT CORRESPONDING #( <u> ) INTO TABLE upd_tab ASSIGNING FIELD-SYMBOL(<comp>).
      "Setting field values
      <comp>-Text1 = to_lower( <comp>-Text1 ).
      <comp>-Text2 = to_lower( <comp>-Text2 ).

      "Populating result parameter
      INSERT VALUE #( %tky   = <u>-%tky
                      %param = CORRESPONDING #( <comp> ) ) INTO TABLE result.

    ENDLOOP.

    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      UPDATE FIELDS ( Text1 Text2 ) WITH upd_tab.


  ENDMETHOD.

  METHOD precheck_lowercase.

*&---------------------------------------------------------------------*
*& In the example, the lowercase action is specified with precheck.
*& feature control in the BDEF. The precheck prevents unwanted changes
*& from reaching the application buffer. The simplified implementation
*& lets the precheck fail in case numbers are contained in the values
*& of the two text fields. Then, the execution of the
*& lowercase action is denied.
*&---------------------------------------------------------------------*


    "if either text1 or text2 or both include numbers, then action exection is denied

    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      FIELDS ( Text1 Text2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.

    DATA found TYPE abap_boolean.
    LOOP AT read_result ASSIGNING FIELD-SYMBOL(<instance>).
      CLEAR found.
      FIND PCRE `\d` IN <instance>-Text1.
      IF sy-subrc = 0.
        found = abap_true.
      ENDIF.

      FIND PCRE `\d` IN <instance>-Text2.
      IF sy-subrc = 0.
        found = abap_true.
      ENDIF.

      IF found = abap_true.

        ASSIGN keys[ KEY id %tky = <instance>-%tky ]-%cid_ref TO FIELD-SYMBOL(<cid>).
        "Fill FAILED/REPORTED response structures.
        APPEND VALUE #( %tky = <instance>-%tky
                        %cid = <cid>
                        %action-lowercase      = if_abap_behv=>mk-off
                        %fail-cause  = if_abap_behv=>cause-unspecific
                      )
                  TO failed-demo_abap.
        APPEND VALUE #( %msg      = new_message_with_text(
                        severity  = if_abap_behv_message=>severity-error
                         text      = `Action execution denied. Strings have numbers.` )
                          %tky = <instance>-%tky
                          %cid = <cid> )
                     TO reported-demo_abap.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_global_features.

*&---------------------------------------------------------------------*
*& In the example, the reverse_text action is specified with global
*& feature control in the BDEF. The simplified implementation prevents
*& the processing of the reverse_text action based on the current time.
*& You may want to switch the time span values by commenting out the
*& first time1 and time2 assignments and commenting in the second
*& assignments.
*&---------------------------------------------------------------------*


    "Global feature control determined by certain value
    "to switch the value, run the COUNT action

    DATA time1 TYPE t.
    DATA time2 TYPE t.

    time1 = '000000'.
    time2 = '070000'.

*    time1 = '070001'.
*    time2 = '235959'.

    result = VALUE #( %action-reverse_text = COND #( WHEN cl_abap_context_info=>get_system_time( ) BETWEEN time1 AND time2
                                                               THEN if_abap_behv=>fc-o-disabled
                                                               ELSE if_abap_behv=>fc-o-enabled

                                                                )
                    ).


*    result = VALUE #( %action-initalize = COND #( WHEN zbp_r_demo_abap98=>flag = abap_true THEN if_abap_behv=>fc-o-disabled
*                                                                               ELSE if_abap_behv=>fc-o-enabled ) ).

    IF result-%action-reverse_text = if_abap_behv=>fc-o-disabled.
      APPEND VALUE #( %msg    = new_message_with_text( text     = `Action execution is currently not allowed.`
                                                       severity = if_abap_behv_message=>severity-error )
                      %global = if_abap_behv=>mk-on
                    ) TO reported-demo_abap.
    ENDIF.


  ENDMETHOD.

  METHOD reverse_text.

*&---------------------------------------------------------------------*
*& Purpose
*& The action demonstrates a non-factory action that is defined with
*& global feature control and output parameter in the BDEF. The global
*& feature control - the implementation is contained in the
*& get_global_features method - prevents action execution based on the
*& current time.
*&
*& Implementation
*& The example implementation reverses the string values contained in
*& two text fields.
*&---------------------------------------------------------------------*

    "Retrieving instances based on requested keys
    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      FIELDS ( Text1 Text2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.

    "Setting %action value in the failed response parameter
    LOOP AT failed-demo_abap ASSIGNING FIELD-SYMBOL(<f>).
      <f>-%action-reverse_text = if_abap_behv=>mk-on.
    ENDLOOP.

    "If read result is initial, stop further method execution.
    CHECK read_result IS NOT INITIAL.

    DATA upd_tab TYPE TABLE FOR UPDATE zr_demo_abap98.
    LOOP AT read_result ASSIGNING FIELD-SYMBOL(<u>).
      "Populating derived type for update operation
      INSERT CORRESPONDING #( <u> ) INTO TABLE upd_tab ASSIGNING FIELD-SYMBOL(<comp>).
      "Setting field values
      <comp>-Text1 = reverse( <comp>-Text1 ).
      <comp>-Text2 = reverse( <comp>-Text2 ).

      "Populating result parameter
      INSERT VALUE #( %tky   = <u>-%tky
                      %param = CORRESPONDING #( <comp> ) ) INTO TABLE result.

    ENDLOOP.

    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      UPDATE FIELDS ( Text1 Text2 ) WITH upd_tab.

  ENDMETHOD.

  METHOD get_instance_features.

*&---------------------------------------------------------------------*
*& In the example, the increment_numbers action is specified with instance
*& feature control in the BDEF. When executing the increment_numbers
*& method, the get_instance_features method is called. The simplified
*& implementation prevents the change of two numeric field values when
*& the current values is greater than or equal to 100.
*&---------------------------------------------------------------------*

    "demo use case: when field1 has a specific value, then read only

    "Retrieving instances based on requested keys
    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      FIELDS ( Num1 Num2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.


    result = VALUE #( FOR wa IN read_result
                       ( %tky                   = wa-%tky
                         %features-%action-increment_numbers = COND #( WHEN wa-Num1 >= 100 OR wa-Num2 >= 100
                                                                       THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled ) ) ).
    "braucht man reported??
*  loop at result into data(res).
*
*    IF res-%action-increment_numbers = if_abap_behv=>fc-o-disabled.
*      APPEND VALUE #( %msg    = new_message_with_text( text     = `Num1 and/or Num2 value higher than 100. Action execution not allowed.`
*                                                       severity = if_abap_behv_message=>severity-information )
*                      "%cid = keys[ KEY id %tky = <instance>-%tky ]-
*                      %tky = res-%tky
*                    ) TO reported-demo_abap.
*    ENDIF.
*  endloop.


  ENDMETHOD.

  METHOD increment_numbers.

*&---------------------------------------------------------------------*
*& Purpose
*& The action demonstrates a non-factory action that is defined with
*& instance feature control and output parameter in the BDEF. The instance
*& feature control - the implementation is contained in the
*& get_instance_features method - prevents action if the values of the
*& two numeric fields are within a specific range.
*&
*& Implementation
*& The example implementation increments the values contained in
*& two numeric fields by one.
*&---------------------------------------------------------------------*

    "Retrieving instances based on requested keys
    READ ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      FIELDS ( Num1 Num2 ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.

    "Setting %action value in the failed response parameter
    LOOP AT failed-demo_abap ASSIGNING FIELD-SYMBOL(<f>).
      <f>-%action-reverse_text = if_abap_behv=>mk-on.
    ENDLOOP.

    "If read result is initial, stop further method execution.
    CHECK read_result IS NOT INITIAL.

    DATA upd_tab TYPE TABLE FOR UPDATE zr_demo_abap98.
    LOOP AT read_result ASSIGNING FIELD-SYMBOL(<u>).
      "Populating derived type for update operation
      INSERT CORRESPONDING #( <u> ) INTO TABLE upd_tab ASSIGNING FIELD-SYMBOL(<comp>).
      "Setting field values
      <comp>-Num1 += 1.
      <comp>-Num2 += 1.

      "Populating result parameter
      INSERT VALUE #( %tky   = <u>-%tky
                      %param = CORRESPONDING #( <comp> ) ) INTO TABLE result.

    ENDLOOP.

    MODIFY ENTITIES OF zr_demo_abap98 IN LOCAL MODE
      ENTITY demo_abap
      UPDATE FIELDS ( Num1 Num2 ) WITH upd_tab.
  ENDMETHOD.
ENDCLASS.
