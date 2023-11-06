***********************************************************************
*
*          RAP BO consumer for a RAP demo scenario:
*      ABAP EML in use: Managed RAP BO with external numbering
*
* -------------------------- PURPOSE ----------------------------------
* - This class is the RAP BO consumer for a RAP demo scenario that
*   demonstrates various RAP BO standard operations and non-standard
*   operations using ABAP EML in the context of a managed RAP business
*   object with RAP external numbering.
* - Topics covered: RAP BO operations like create (including a
*   determination on save), update, delete, executing an action, validation,
*   create-by-association (parent to child), read (root entity),
*   read-by-association (parent to child), read (child entity),
*   read-by-association (child to parent)
* - Underlying data model: Consists of a root entity and one child entity.
*   The  BDEF defines the behavior for these two entities which are connected
*   via a CDS composition relation. The definitions in the BDEF determine
*   which methods must be implemented in the ABAP behavior pool (ABP).
* - ABP for this scenario: zbp_demo_abap_rap_ro_m
*
* ----------------------- GETTING STARTED -----------------------------
* - Open the class with the ABAP development tools for Eclipse (ADT).
* - Choose F9 to run the class.
* - Check the console output.
* - To understand the context and the ABAP syntax used, check the notes
*   included in the class as comments or refer to the respective topic
*   in the ABAP Keyword Documentation.
* - Due to the amount of output in the console, the examples include
*   numbers (e. g. 1) ..., 2) ..., 3) ...) for the individual example
*   sections. Plus, the variable name is displayed in most cases. Hence,
*   to easier and faster find the relevant output in the console, just
*   search in the console for the number/variable name (CTRL+F in the
*   console) or use the debugger.
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
"! <p class="shorttext synchronized">ABAP cheat sheet: ABAP EML in a RAP scenario (managed BO)</p>
"! Example to demonstrate ABAP EML in the context of a RAP demo scenario (managed RAP BO with external numbering).
"! The class represents a RAP BO consumer.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_rap_ext_num_m DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    CLASS-METHODS:
      class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      failed   TYPE RESPONSE FOR FAILED zdemo_abap_rap_ro_m,
      reported TYPE RESPONSE FOR REPORTED zdemo_abap_rap_ro_m,
      mapped   TYPE RESPONSE FOR MAPPED zdemo_abap_rap_ro_m,
      op       TYPE string.
    CLASS-METHODS:
      initialize_dbtabs,
      "If there are entries in the response parameters following EML
      "requests, they should be processed for displaying purposes.
      extract_from_reported RETURNING VALUE(messages) TYPE string_table,
      extract_from_failed RETURNING VALUE(errors) TYPE string_table,
      fill_db_tab.

ENDCLASS.



CLASS zcl_demo_abap_rap_ext_num_m IMPLEMENTATION.


  METHOD class_constructor.
    initialize_dbtabs( ).
  ENDMETHOD.


  METHOD extract_from_failed.
    CLEAR errors.

    LOOP AT failed-root ASSIGNING FIELD-SYMBOL(<err>).
      CASE if_abap_behv=>mk-on.
        WHEN <err>-%op-%create.
          op = `create operation`.
        WHEN <err>-%op-%update.
          op = `update operation`.
        WHEN <err>-%op-%delete.
          op = `delete operation`.
        WHEN <err>-%op-%assoc-_child.
          op = `operation involving the child entity`.
        WHEN <err>-%op-%action-multiply_by_2.
          op = `executing action multiply_by_2`.
        WHEN OTHERS. op = `operation`.
      ENDCASE.

      APPEND `Error for instance with ` &&
           COND #( WHEN <err>-%cid IS NOT INITIAL THEN `%cid = `
                   && <err>-%cid
                   ELSE `key = ` && <err>-key_field ) &&
           `: Fail cause ` &&  <err>-%fail-cause && ` for ` &&  op
            && `.` TO errors.
    ENDLOOP.

    IF failed-child IS NOT INITIAL.
      LOOP AT failed-child ASSIGNING FIELD-SYMBOL(<err_ch>).
        APPEND `Error for child instance with key_field = ` &&
        <err_ch>-key_field && ` and key_ch = ` &&
        <err_ch>-key_ch && `: Fail cause `
        && <err_ch>-%fail-cause && `.` TO errors.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD extract_from_reported.
    CLEAR messages.

    LOOP AT reported-root ASSIGNING FIELD-SYMBOL(<rep>).
      IF <rep>-%global = if_abap_behv=>mk-on.
        APPEND <rep>-%msg->m_severity &&
                <rep>-%msg->if_t100_dyn_msg~msgv1 TO messages.
      ELSE.
        APPEND `Message for instance with ` &&
               COND #( WHEN <rep>-%cid IS NOT INITIAL
                       THEN `%cid = ` && <rep>-%cid
                       ELSE `key = ` && <rep>-key_field ) &&
               `: ` &&  <rep>-%msg->m_severity && ` ` &&
               <rep>-%msg->if_t100_dyn_msg~msgv1 TO messages.

      ENDIF.
    ENDLOOP.

    IF reported-child IS NOT INITIAL.
      LOOP AT reported-child ASSIGNING FIELD-SYMBOL(<rep_ch>).
        APPEND `Message for child instance with key_field = ` &&
        <rep_ch>-key_field && ` and key_ch = `
        && <rep_ch>-key_ch && `: ` && <rep_ch>-%msg->m_severity &&
        ` ` && <rep_ch>-%msg->if_t100_dyn_msg~msgv1 TO messages.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD fill_db_tab.
    MODIFY zdemo_abap_rapt1 FROM TABLE @( VALUE #(
          ( key_field = 4
             field1   = 'ggg'
             field2   = 'hhh'
             field3   = 40
             field4   = 41 ) ) ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: RAP BO Operations Using a Managed RAP BO\n\n| ).

**********************************************************************
*
* Create operation
*
**********************************************************************

    out->write( |1) Create operation\n\n| ).

    "Adding an entry to the database table to provoke an error for the
    "EML create request.
    fill_db_tab( ).

**********************************************************************
* Notes:
* - field4 is purposely not included in the FIELDS list
* - Effect:
*   - %control value for field4 is set to if_abap_behv=>mk-off
*   - Although the derived type (created inline here) includes a
*     value assignment for field4 in an instance, the field value is
*     not saved. The initial value is used.
* - The instance with key_field = 4 will not be saved since an entry
*   already exists in the database table with the same key.
* - Response parameters are specified to receive information.
* - A COMMIT ENTITIES statement triggers the saving of the instances.
* - The example BDEF includes the definition of a determination on
*   save for create operations. In this case, the determination
*   adds some text to the value in field2.
**********************************************************************

    MODIFY ENTITIES OF zdemo_abap_rap_ro_m
      ENTITY root
      CREATE FIELDS ( key_field field1 field2 field3 )
        WITH VALUE #( ( %cid = 'cid1'
                        key_field = 1
                        field1    = 'aaa'
                        field2    = 'bbb'
                        field3    = 10
                        field4    = 11 ) "Value not considered
                      ( %cid      = 'cid2'
                        key_field = 2
                        field1    = 'ccc'
                        field2    = 'ddd'
                        field3    = 20 )
                      ( %cid      = 'cid3'
                        key_field = 3
                        field1    = 'eee'
                        field2    = 'fff'
                        field3    = 30 )
                      ( %cid      = 'cid4' "Instance to fail
                        key_field = 4
                        field1    = 'error'
                        field2    = 'error'
                        field3    = 99 ) )
        MAPPED mapped
        FAILED failed
        REPORTED reported.

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Retrieving and displaying database content
    SELECT FROM zdemo_abap_rapt1
      FIELDS key_field, field1, field2, field3, field4
      ORDER BY key_field
      INTO TABLE @DATA(tab_root).

    out->write( data = tab_root name = `tab_root` ).
    out->write( |\n| ).

    "Displaying response information
    IF mapped-root IS NOT INITIAL.
      out->write( `Entries in MAPPED response parameter ` &&
                         `(root entity)` ).
      out->write( |\n| ).
      out->write( data = mapped-root name = `mapped-root` ).
      out->write( |\n| ).
    ENDIF.

    IF failed IS NOT INITIAL.
      out->write( `Entries in FAILED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_failed( ) name = `extract_from_failed( )` ).
      out->write( |\n| ).
    ENDIF.

    IF reported IS NOT INITIAL.
      out->write( `Entries in REPORTED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_reported( ) name = `extract_from_reported( )` ).
      out->write( |\n| ).
    ENDIF.

**********************************************************************
*
* Update operations
*
**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Update operation` ) ).

**********************************************************************
* Notes:
* - The EML request includes a create and an update request. The
*   create request is included to have a %cid to refer to for demo
*   purposes. This instance has not yet been persisted.
* - The EML statement for the create operation includes the ABAP
*   FROM ... (instead of FIELDS ( ... ) WITH ...) for demo purposes.
*   Here, the %control values must be set explicitly.
* - The update request purposely excludes field2 so as not to update
*   the value of this particular field.
**********************************************************************

    MODIFY ENTITIES OF zdemo_abap_rap_ro_m
        ENTITY root
        CREATE FROM VALUE #(
            %control-key_field = if_abap_behv=>mk-on
            %control-field1 = if_abap_behv=>mk-on
            %control-field2 = if_abap_behv=>mk-on
            %control-field3 = if_abap_behv=>mk-on
            %control-field4 = if_abap_behv=>mk-on
          ( %cid = 'cid5'
            key_field = 5
            field1    = 'iii'
            field2    = 'jjj'
            field3    = 50
            field4    = 51 ) )
        UPDATE FIELDS ( field1 field3 field4 )
          WITH VALUE #(
          "Update via cid_ref
          ( %cid_ref = 'cid5'
            field1   = 'up_kkk'
            field2   = 'up_lll' "Value not considered
            field3   = 500
            field4   = 501 )
          "Updates via key
          ( key_field = 1
            field1    = 'up_mmm'
            field3    = 100
            field4    = 101 )
          ( key_field = 2
            field1    = 'up_ooo'
            field3    = 200
            field4    = 201 )
          ( key_field = 99   "Instance to fail
            field1    = 'error'
            field3    = 99
            field4    = 99 ) )
        MAPPED mapped
        FAILED failed
        REPORTED reported.

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Retrieving and displaying database content
    SELECT FROM zdemo_abap_rapt1
      FIELDS key_field, field1, field2, field3, field4
      ORDER BY key_field
      INTO TABLE @tab_root.

    out->write( data = tab_root name = `tab_root` ).
    out->write( |\n| ).

    "Displaying response information
    IF mapped-root IS NOT INITIAL.
      out->write( `Entries in MAPPED response parameter ` &&
                         `(root entity)` ).
      out->write( |\n| ).
      out->write( data = mapped-root name = `mapped-root` ).
      out->write( |\n| ).
    ENDIF.

    IF failed IS NOT INITIAL.
      out->write( `Entries in FAILED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_failed( ) name = `extract_from_failed( )` ).
      out->write( |\n| ).
    ENDIF.

    IF reported IS NOT INITIAL.
      out->write( `Entries in REPORTED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_reported( ) name = `extract_from_reported( )` ).
      out->write( |\n| ).
    ENDIF.

**********************************************************************
*
* Delete operation
*
**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Delete operation` ) ).

**********************************************************************
* Notes:
* - The EML request includes a create and an delete request. The
*   create request is included to have a %cid to refer to for demo
*   purposes. This instance has not yet been persisted.
* - EML statements for delete operations can only be used with the
*   ABAP word FROM ....
**********************************************************************

    MODIFY ENTITIES OF zdemo_abap_rap_ro_m
        ENTITY root
        CREATE FIELDS ( key_field field1 field2 field3 field4 )
          WITH VALUE #(
          ( %cid      = 'cid_del'
            key_field = 6
            field1    = 'mmm'
            field2    = 'nnn'
            field3    = 60
            field4    = 61 ) )
        DELETE FROM VALUE #(
          "Deletion via %cid_ref
          ( %cid_ref = 'cid_del' )
          "Deletions via key
          ( key_field = 4 )
          ( key_field = 5 )
          "Instance to fail
          ( key_field = 100 ) )  "Key not available
        MAPPED mapped
        FAILED failed
        REPORTED reported.

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Retrieving and displaying database content
    SELECT FROM zdemo_abap_rapt1
      FIELDS key_field, field1, field2, field3, field4
      ORDER BY key_field
      INTO TABLE @tab_root.

    out->write( data = tab_root name = `tab_root` ).
    out->write( |\n| ).

    "Displaying response information
    IF mapped-root IS NOT INITIAL.
      out->write( `Entries in MAPPED response parameter ` &&
                         `(root entity)` ).
      out->write( |\n| ).
      out->write( data = mapped-root name = `mapped-root` ).
      out->write( |\n| ).
    ENDIF.

    IF failed IS NOT INITIAL.
      out->write( `Entries in FAILED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_failed( ) name = `extract_from_failed( )` ).
      out->write( |\n| ).
    ENDIF.

    IF reported IS NOT INITIAL.
      out->write( `Entries in REPORTED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_reported( ) name = `extract_from_reported( )` ).
      out->write( |\n| ).
    ENDIF.

**********************************************************************
*
* Action multiply_by_2
*
**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Action execution: mutliply_by_2` ) ).

**********************************************************************
* Notes:
* - The EML request includes a create request and a request to execute
*   an action. The create request is included to have a %cid to refer
*   to for demo purposes. This instance has not yet been persisted.
* - EML statements for executing actions can only be used with the
*   ABAP word FROM ....
* - As the name implies, the action multiplies field
*   values (field3 and field4) by 2 for requested instances.
**********************************************************************

    MODIFY ENTITIES OF zdemo_abap_rap_ro_m
        ENTITY root
        CREATE FIELDS ( key_field field1 field2 field3 field4 )
          WITH VALUE #(
          ( %cid = 'cid_x2'
            key_field = 7
            field1    = 'ooo'
            field2    = 'ppp'
            field3    = 70
            field4    = 71 ) )
        EXECUTE multiply_by_2 FROM VALUE #(
          "Executing action via %cid_ref
          ( %cid_ref = 'cid_x2' )
          "Executing action via key
          ( key_field  =  1  )
          ( key_field  =  2  )
          ( key_field  =  1234 ) ) "Instance to fail
        MAPPED mapped
        FAILED failed
        REPORTED reported.

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Retrieving and displaying database content
    SELECT FROM zdemo_abap_rapt1
      FIELDS key_field, field1, field2, field3, field4
      ORDER BY key_field
      INTO TABLE @tab_root.

    out->write( data = tab_root name = `tab_root` ).
    out->write( |\n| ).

    "Displaying response information
    IF mapped-root IS NOT INITIAL.
      out->write( `Entries in MAPPED response parameter ` &&
                         `(root entity)` ).
      out->write( |\n| ).
      out->write( data = mapped-root name = `mapped-root` ).
      out->write( |\n| ).
    ENDIF.

    IF failed IS NOT INITIAL.
      out->write( `Entries in FAILED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_failed( ) name = `extract_from_failed( )` ).
      out->write( |\n| ).
    ENDIF.

    IF reported IS NOT INITIAL.
      out->write( `Entries in REPORTED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_reported( ) name = `extract_from_reported( )` ).
      out->write( |\n| ).
    ENDIF.

**********************************************************************
*
* Create-by-association operation (from root to child entity)
*
**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Create-by-association operation (from parent to child)` ) ).

**********************************************************************
* Notes:
* - The EML request includes a create and create-by-association
*   request, i. e. a "deep create". An instance is created for the
*   parent entity and, in the same request and based on this
*   instance, instances are created for the child entity, too.
**********************************************************************

    MODIFY ENTITIES OF zdemo_abap_rap_ro_m
        ENTITY root
        CREATE FIELDS ( key_field field1 field2 field3 field4 )
              WITH VALUE #(
              ( %cid      = 'cid_cba'
                key_field = 9
                field1    = 'qqq'
                field2    = 'rrr'
                field3    = 90
                field4    = 91 ) )
        CREATE BY \_child
        FIELDS ( key_ch field_ch1 field_ch2 ) WITH VALUE #(
          "CBA operation via %cid_ref
          ( %cid_ref = 'cid_cba'
            %target = VALUE #( ( %cid      = 'cid_ch1'
                                 key_ch    = 9
                                 field_ch1 = 'aaa_ch'
                                 field_ch2 = 99 )
                               ( %cid      = 'cid_ch2'
                                 key_ch    = 10
                                 field_ch1 = 'bbb_ch'
                                 field_ch2 =  100 ) ) )
          "CBA operation via root key
          ( key_field = 1
            %target = VALUE #( ( %cid      = 'cid_ch3'
                                 key_ch    = 1
                                 field_ch1 = 'ccc_ch'
                                 field_ch2 = 11 )
                               ( %cid      = 'cid_ch4'
                                 key_ch    = 2
                                 field_ch1 = 'ddd_ch'
                                 field_ch2 = 22 ) ) )
          ( key_field = 2
            %target = VALUE #( ( %cid      = 'cid_ch5'
                                 key_ch    = 3
                                 field_ch1 = 'ccc_ch'
                                 field_ch2 = 33 )
                               ( %cid      = 'cid_ch6'
                                 key_ch    = 4
                                 field_ch1 = 'ddd_ch'
                                 field_ch2 = 44 ) ) )
          "Instance to fail
          ( key_field = 123
            %target = VALUE #( ( %cid      = 'cid_ch7'
                                 key_ch    = 1
                                 field_ch1 = 'error'
                                 field_ch2 = 2 )
                               ( %cid      = 'cid_ch8'
                                 key_ch    = 2
                                 field_ch1 = 'error'
                                 field_ch2 = 3 ) ) )
                                 )
          MAPPED mapped
          FAILED failed
          REPORTED reported.

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Retrieving and displaying database content
    SELECT FROM zdemo_abap_rapt1
      FIELDS key_field, field1, field2, field3, field4
      ORDER BY key_field
      INTO TABLE @tab_root.

    SELECT FROM zdemo_abap_rapt2
      FIELDS key_field, key_ch, field_ch1, field_ch2
      ORDER BY key_field, key_ch
      INTO TABLE @DATA(tab_child).

    out->write( data = tab_root name = `tab_root` ).
    out->write( |\n| ).
    out->write( data = tab_child name = `tab_child` ).
    out->write( |\n| ).

    "Displaying response information
    IF mapped IS NOT INITIAL.
      out->write( `Entries in MAPPED response parameter ` &&
                         `(root and child entity)` ).
      out->write( |\n| ).
      out->write( data = mapped name = `mapped` ).
      out->write( |\n| ).
    ENDIF.

    IF failed IS NOT INITIAL.
      out->write( `Entries in FAILED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_failed( ) name = `extract_from_failed( )` ).
      out->write( |\n| ).
    ENDIF.

    IF reported IS NOT INITIAL.
      out->write( `Entries in REPORTED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_reported( ) name = `extract_from_reported( )` ).
      out->write( |\n| ).
    ENDIF.

**********************************************************************
*
* Validation val
*
**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) Validation val` ) ).

**********************************************************************
* Notes:
* - The EML request includes a create request. The validation's
*   handler method is implementation in a way that the saving of
*   instances is disabled if a field value is not allowed. In this
*   example, the value of the integer in field3 shall not exceed 1000.
*   Here, the third instance will fail for the validation.
*   Consequently, all instances of this request are not saved to the
*   database. Either all is ok and will be saved or nothing.
* - Note that the response information for the validation is only
*   available in the response parameters of the COMMIT ENTITIES
*   statement. Here, the BDEF derived type is
*   ... TYPE RESPONSE FOR ... LATE ....
**********************************************************************

    MODIFY ENTITIES OF zdemo_abap_rap_ro_m
        ENTITY root
        CREATE FIELDS ( key_field field1 field2 field3 field4 )
          WITH VALUE #(
          ( %cid = 'cid_val1'
            key_field = 123
            field1    = 'sss'
            field2    = 'ttt'
            field3    = 1
            field4    = 2 )
          ( %cid = 'cid_val2'
            key_field = 456
            field1    = 'uuu'
            field2    = 'vvv'
            field3    = 3
            field4    = 4 )
          ( %cid = 'cid_val3'
            key_field = 789
            field1    = 'www'
            field2    = 'xxx'
            field3    = 1001
            field4    = 5 ) )
        MAPPED mapped
        FAILED failed
        REPORTED reported.

    COMMIT ENTITIES RESPONSES
      FAILED DATA(failed_late)
      REPORTED DATA(reported_late).

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Retrieving and displaying database content
    SELECT FROM zdemo_abap_rapt1
      FIELDS key_field, field1, field2, field3, field4
      ORDER BY key_field
      INTO TABLE @tab_root.

    out->write( data = tab_root name = `tab_root` ).
    out->write( |\n| ).

    "Displaying response information
    IF mapped IS NOT INITIAL.
      out->write( `Entries in MAPPED response parameter ` &&
                         `(root and child entity)` ).
      out->write( |\n| ).
      out->write( data = mapped name = `mapped` ).
      out->write( |\n| ).
    ENDIF.

    IF failed_late IS NOT INITIAL.
      out->write( `Entries in FAILED LATE response parameter` ).
      out->write( |\n| ).
      out->write( data = failed_late name = `failed_late` ).
      out->write( |\n| ).
    ENDIF.

    IF reported_late IS NOT INITIAL.
      out->write( `Entries in REPORTED LATE response parameter` ).
      out->write( |\n| ).
      out->write( data = reported_late name = `reported_late` ).
      out->write( |\n| ).
    ENDIF.

**********************************************************************
*
* Read operation (root entity)
*
**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) Read operation (root entity)` ) ).

**********************************************************************
* Notes:
* - The EML request includes a read request. The EML statement uses
*   the ABAP words ALL FIELDS WITH. In this case, as the name implies,
*   all field values are retrieved. The %control values for all fields
*   are set to if_abap_behv=>mk-on.
* - When using the ABAP words FIELDS ( ... ) WITH and specifying the
*   concrete fields to be read, only for those fields %control is
*   set accordingly.
* - Filling the parameter for RESULT is mandatory.
**********************************************************************

    READ ENTITIES OF zdemo_abap_rap_ro_m
        ENTITY root
        ALL FIELDS WITH VALUE #(
          ( key_field = 1 )
          ( key_field = 2 )
          ( key_field = 7 )
          ( key_field = 5 ) ) "Instance to fail
        RESULT DATA(result)
        FAILED failed
        REPORTED reported.

    "Displaying the read result
    out->write( data = result name = `result` ).
    out->write( |\n| ).

    "Displaying response information
    IF failed IS NOT INITIAL.
      out->write( `Entries in FAILED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_failed( ) name = `extract_from_failed( )` ).
      out->write( |\n| ).
    ENDIF.
    IF reported IS NOT INITIAL.
      out->write( `Entries in REPORTED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_reported( ) name = `extract_from_reported( )` ).
      out->write( |\n| ).
    ENDIF.

*********************************************************************
*
* Read operation (child entity)
*
**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Read operation (child entity)` ) ).

**********************************************************************
* Notes:
* - The EML request includes a read request. The read operation is
*   executed on the child entity directly by specifying the alias, as
*   it is defined in the BDEF, following the ABAP word ENTITY.
* - All field values are read using the addition ALL FIELDS WITH.
**********************************************************************

    READ ENTITIES OF zdemo_abap_rap_ro_m
          ENTITY child
          ALL FIELDS WITH VALUE #(
            ( key_field = 1 key_ch = 1 )
            ( key_field = 2 key_ch = 4 )
            "Instances to fail
            ( key_field = 9 )
            ( key_field = 9 key_ch = 11 ) )
          RESULT DATA(read_ch)
          FAILED failed
          REPORTED reported.

    "Displaying read result
    out->write( data = read_ch name = `read_ch` ).
    out->write( |\n| ).


    "Displaying response information
    IF failed IS NOT INITIAL.
      out->write( `Entries in FAILED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_failed( ) name = `extract_from_failed( )` ).
      out->write( |\n| ).
    ENDIF.
    IF reported IS NOT INITIAL.
      out->write( `Entries in REPORTED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_reported( ) name = `extract_from_reported( )` ).
      out->write( |\n| ).
    ENDIF.

**********************************************************************
*
* Read-by-association operation (from parent to child)
*
**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `9) Read-by-association operation (from parent to child)` ) ).

**********************************************************************
* Notes:
* - The EML request includes a read-by-association request from the
*   parent to the child.
* - All field values are read using the addition ALL FIELDS WITH.
* - Specifying the parameter for RESULT is mandatory.
* - Additionally, the optional association links are retrieved.
**********************************************************************

    READ ENTITIES OF zdemo_abap_rap_ro_m
          ENTITY root
          BY \_child
          ALL FIELDS WITH VALUE #(
            ( key_field = 2 )
            ( key_field = 9 )
            ( key_field = 999 ) ) "Instance to fail
          RESULT DATA(rba_result)
          LINK DATA(association_links)
          FAILED failed
          REPORTED reported.

    "Displaying read result and association links
    out->write( data = rba_result name = `rba_result` ).
    out->write( |\n| ).
    out->write( data = association_links name = `association_links` ).
    out->write( |\n| ).

    "Displaying response information
    IF failed IS NOT INITIAL.
      out->write( `Entries in FAILED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_failed( ) name = `extract_from_failed( )` ).
      out->write( |\n| ).
    ENDIF.
    IF reported IS NOT INITIAL.
      out->write( `Entries in REPORTED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_reported( ) name = `extract_from_reported( )` ).
      out->write( |\n| ).
    ENDIF.

**********************************************************************
*
* Read-by-association operation (from child to parent)
*
**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) Read-by-association operation (from child to parent)` ) ).

**********************************************************************
* Notes:
* - The EML request includes a read-by-association request from the
*   child to the parent.
* - All field values are read using the addition ALL FIELDS WITH.
* - Specifying the parameter for RESULT is mandatory.
* - Additionally, the optional association links are retrieved.
**********************************************************************

    READ ENTITIES OF zdemo_abap_rap_ro_m
      ENTITY child
      BY \_parent ALL FIELDS WITH VALUE #(
         ( key_field = 1 key_ch = 1 )
         ( key_field = 2 key_ch = 4 )
         "Instances to fail
         ( key_field = 1 key_ch = 3 )
         ( key_field = 543 key_ch = 1 ) )
       RESULT DATA(rba_parent)
       LINK DATA(association_links_parent)
       FAILED failed
       REPORTED reported.

    "Displaying read result and association links
    out->write( data = rba_parent name = `rba_parent` ).
    out->write( |\n| ).
    out->write( data = association_links_parent name = `association_links_parent` ).
    out->write( |\n| ).

    "Displaying response information
    IF failed IS NOT INITIAL.
      out->write( `Entries in FAILED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_failed( ) name = `extract_from_failed( )` ).
      out->write( |\n| ).
    ENDIF.
    IF reported IS NOT INITIAL.
      out->write( `Entries in REPORTED response parameter` ).
      out->write( |\n| ).
      out->write( data = extract_from_reported( ) name = `extract_from_reported( )` ).
      out->write( |\n| ).
    ENDIF.

**********************************************************************
*
* Excursion: Read and read-by-association operation using a dynamic
* EML statement
*
**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) Excursion: Read and read-by-association ` &&
                     `operations using dynamic EML` ) ).
    DATA:
      op_tab          TYPE abp_behv_retrievals_tab,
      read_dyn        TYPE TABLE FOR READ IMPORT zdemo_abap_rap_ro_m,
      read_dyn_result TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m,
      rba_dyn         TYPE TABLE FOR READ IMPORT
                        zdemo_abap_rap_ro_m\_child,
      rba_dyn_result  TYPE TABLE FOR READ RESULT
                        zdemo_abap_rap_ro_m\_child,
      rba_dyn_link    TYPE TABLE FOR READ LINK zdemo_abap_rap_ro_m\_child.

    read_dyn = VALUE #(
      ( %key-key_field = 1
        %control = VALUE #(
          field1 = if_abap_behv=>mk-on
          field2 = if_abap_behv=>mk-on
          field3 = if_abap_behv=>mk-on
          field4 = if_abap_behv=>mk-on ) )
      ( %key-key_field = 2
        %control = VALUE #(
          field1 = if_abap_behv=>mk-on
          field2 = if_abap_behv=>mk-on
          field3 = if_abap_behv=>mk-on
          field4 = if_abap_behv=>mk-on ) ) ).

    rba_dyn = VALUE #(
     ( %key-key_field = 1
       %control = VALUE #(
          key_ch    = if_abap_behv=>mk-on
          field_ch1 = if_abap_behv=>mk-on
          field_ch2 = if_abap_behv=>mk-on ) )
      ( %key-key_field = 2
        %control = VALUE #(
          key_ch    = if_abap_behv=>mk-on
          field_ch1 = if_abap_behv=>mk-on
          field_ch2 = if_abap_behv=>mk-on ) ) ).

    op_tab = VALUE #(
       ( op = if_abap_behv=>op-r-read
         entity_name = 'ZDEMO_ABAP_RAP_RO_M'
         instances   = REF #( read_dyn )
         results     = REF #( read_dyn_result ) )
       ( op = if_abap_behv=>op-r-read_ba
         entity_name = 'ZDEMO_ABAP_RAP_RO_M'
         sub_name    = '_CHILD'
         full        = abap_true
         instances   = REF #( rba_dyn )
         results     = REF #( rba_dyn_result )
         links       = REF #( rba_dyn_link ) ) ).

    READ ENTITIES OPERATIONS op_tab.

    out->write( `Read result (root)` ).
    out->write( |\n| ).
    out->write( data = read_dyn_result name = `read_dyn_result` ).
    out->write( |\n| ).
    out->write( `Read result (read-by-association)` ).
    out->write( |\n| ).
    out->write( data = rba_dyn_result name = `rba_dyn_result` ).
    out->write( |\n| ).
    out->write( `Links` ).
    out->write( |\n| ).
    out->write( data = rba_dyn_link name = `rba_dyn_link` ).
  ENDMETHOD.

  METHOD initialize_dbtabs.
    DELETE FROM zdemo_abap_rapt1.
    DELETE FROM zdemo_abap_rapt2.
  ENDMETHOD.
ENDCLASS.
