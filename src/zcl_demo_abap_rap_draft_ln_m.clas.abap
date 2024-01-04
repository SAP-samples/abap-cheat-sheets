***********************************************************************
*
*           RAP BO consumer for a RAP demo scenario
* ABAP EML in use: RAP calculator (managed, draft-enabled RAP BO with
* late numbering
*
* -------------------------- PURPOSE ----------------------------------
* - This class is the RAP BO consumer for a RAP demo scenario that
*   represents a calculator using RAP concepts, i. e. using ABAP EML in
*   the context of a managed and draft-enabled RAP business object with
*   RAP late numbering to carry out simple calculations. Here, a RAP BO
*   instance consists of a calculation ID (which is the key that is finally
*   set not until the RAP save sequence), two operands (having integer
*   values), the arithmetic operator and the result plus other
*   draft-related fields.
* - Underlying data model: Consists of a root entity alone.
*   The BDEF defines the behavior for this entity. The definitions in the
*   BDEF determine which methods must be implemented in the ABAP behavior
*   pool (ABP). Note that the view contains many annotations for the SAP
*   Fiori UI.
* - ABP for this scenario: zbp_demo_abap_rap_draft_m
*
* ----------------------- GETTING STARTED (1) -------------------------
* ----------------- Using this class as RAP BO consumer ---------------
*
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
* ----------------------- GETTING STARTED (2) -------------------------
* Using the preview version of an SAP Fiori Elements UI as RAP BO consumer
*
* Create a service binding:
* 1. Find the service definition ZDEMO_ABAP_RAP_CALC_SD in the imported
*    package in Business Services -> Service Definitions.
* 2. Right-click the service definition and choose New Service Binding.
* 3. In the New Service Binding pop-up, make the following entries:
*   - Name: ZDEMO_ABAP_RAP_CALC_SB
*   - Description: Service binding for demo
*   - Binding type: OData V4 - UI
*   - Service Definition: ZDEMO_ABAP_RAP_CALC_SD (should be already filled)
* 4. Choose Next.
* 5. Assign a transport request and choose Finish.
* 6. The service binding ZDEMO_ABAP_RAP_CALC_SB is opened. Activate the
*    service binding.
* 7. In the Service Version Details section, choose the Publish button
*    for the Local Service Endpoint. Once the service has been published,
*    you should see ZDEMO_ABAP_RAP_DRAFT_M in the Entity Set and Association
*    section.
* 8. Activate the service binding once the service has been published.
* 9. Select ZDEMO_ABAP_RAP_DRAFT_M and choose the Preview button.
* 10. The preview version of an SAP Fiori Elements app is displayed. If
*     prompted, provide your credentials.
* 11. The app and the managed, draft-enabled RAP BO can be explored. If no
*     columns are displayed, choose the 'Settings' button and select the
*     desired columns.
*     Choosing the 'Go' button refreshes the list. At first use, there
*     might not be any entry. You can create an entry choosing the 'Create'
*     button.
*     The late numbering aspects enter the picture when you, for
*     example, create a new instance, i. e. create a new calculation, and
*     you keep a draft version of it instead of saving it to the database.
*     The calculation ID which represents the key of the instance has an
*     initial value. Only when you save the instance to the database, the
*     final key is set.
*     The effect of side effects can be explored as follows: Make an entry
*     in an input field, click another input field (e.g. to make a new entry
*     there), and check how the value for the result changes.
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
"! <p class="shorttext synchronized">ABAP cheat sheet: ABAP EML in a RAP scenario (draft BO)</p>
"! Example to demonstrate ABAP EML in the context of a RAP demo scenario (managed and draft-enabled RAP business object with RAP late numbering).
"! The class represents a RAP BO consumer.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_rap_draft_ln_m DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    CLASS-METHODS:
      class_constructor.

protected section.
  PRIVATE SECTION.
    CLASS-DATA:
      activate_tab  TYPE TABLE FOR ACTION IMPORT
                      zdemo_abap_rap_draft_m~activate,
      activate_tab2 TYPE TABLE FOR ACTION IMPORT
                      zdemo_abap_rap_draft_m~activate,
      activate_tab3 TYPE TABLE FOR ACTION IMPORT
                      zdemo_abap_rap_draft_m~activate,
      edit_tab      TYPE TABLE FOR ACTION IMPORT
                      zdemo_abap_rap_draft_m~edit,
      read_tab      TYPE TABLE FOR READ IMPORT zdemo_abap_rap_draft_m,
      f             TYPE RESPONSE FOR FAILED zdemo_abap_rap_draft_m,
      r             TYPE RESPONSE FOR REPORTED zdemo_abap_rap_draft_m,
      m             TYPE RESPONSE FOR MAPPED zdemo_abap_rap_draft_m.

    CLASS-METHODS:
      initialize_dbtabs.

ENDCLASS.



CLASS ZCL_DEMO_ABAP_RAP_DRAFT_LN_M IMPLEMENTATION.


  METHOD class_constructor.
    initialize_dbtabs( ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( `ABAP Cheat Sheet Example: RAP Calculator Using Managed, ` &&
                      |Draft-Enabled RAP BO (Late Numbering)\n\n| ).
    out->write( |1) Creating Instances and Saving to the database\n| ).

    "Creating instances; draft indicator %is_draft is enabled
    MODIFY ENTITY zdemo_abap_rap_draft_m
     CREATE AUTO FILL CID
     FIELDS ( num1 arithm_op num2 )
     WITH VALUE #(
        ( %is_draft = if_abap_behv=>mk-on
          num1 = 1 arithm_op = '+' num2 = 2 )
        ( %is_draft = if_abap_behv=>mk-on
          num1 = 2 arithm_op = '*' num2 = 4 )
        ( %is_draft = if_abap_behv=>mk-on
          num1 = 3 arithm_op = '-' num2 = 5 )
        ( %is_draft = if_abap_behv=>mk-on
          num1 = 1 arithm_op = '/' num2 = 4 )
        ( %is_draft = if_abap_behv=>mk-on
          num1 = 2 arithm_op = 'P' num2 = 5 ) )
        FAILED f
        REPORTED r
        MAPPED m.

    "Displaying responses only if FAILED and REPORTED
    "response parameters are not initial
    IF f IS NOT INITIAL OR r IS NOT INITIAL.
      out->write( `Responses after MODIFY operation` ).
      out->write( |\n| ).

      IF m IS NOT INITIAL.
        out->write( data = m name = `m` ).
        out->write( |\n| ).
      ENDIF.

      IF f IS NOT INITIAL.
        out->write( data = f name = `f` ).
        out->write( |\n| ).
      ENDIF.

      IF r IS NOT INITIAL.
        out->write( data = r name = `r` ).
        out->write( |\n| ).
      ENDIF.
    ENDIF.

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Retrieving draft table entries
    SELECT id, num1, arithm_op, num2, calc_result, crea_date_time,
           lchg_date_time, draftentitycreationdatetime,
           draftentitylastchangedatetime
     FROM zdemo_abap_draft
     ORDER BY id
     INTO TABLE @DATA(draft_parent_before_act).

    "Retrieving database table entries
    SELECT id, num1, arithm_op, num2, calc_result, crea_date_time,
           lchg_date_time
     FROM zdemo_abap_tabca
     ORDER BY id
     INTO TABLE @DATA(db_tab_root_before_act).

    "Filling the derived type for the ACTIVATE method by
    "getting %pid values
    LOOP AT m-calc
      ASSIGNING FIELD-SYMBOL(<fs>).
      APPEND VALUE #( %pid = <fs>-%pid )
                             TO activate_tab.
    ENDLOOP.

    MODIFY ENTITY zdemo_abap_rap_draft_m
      EXECUTE activate AUTO FILL CID WITH activate_tab
      MAPPED m
      FAILED f
      REPORTED r.

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Retrieving draft table entries
    SELECT id, num1, arithm_op, num2, calc_result, crea_date_time,
           lchg_date_time, draftentitycreationdatetime,
           draftentitylastchangedatetime
     FROM zdemo_abap_draft
     ORDER BY id
     INTO TABLE @DATA(draft_parent_afer_act).

    "Retrieving database table entries
    SELECT id, num1, arithm_op, num2, calc_result, crea_date_time,
           lchg_date_time
     FROM zdemo_abap_tabca
     ORDER BY id
     INTO TABLE @DATA(db_tab_root_after_act).

    "Displaying entries
    out->write( |1a) Draft and database tables before ACTIVATE action\n| ).
    out->write( `Draft table before activation` ).
    out->write( |\n| ).
    out->write( data = draft_parent_before_act name = `draft_parent_before_act` ).
    out->write( |\n| ).
    out->write( `Database table before activation` ).
    out->write( |\n| ).
    out->write( data = db_tab_root_before_act name = `db_tab_root_before_act` ).

    out->write( zcl_demo_abap_aux=>heading( `1b) Draft and database tables after ` &&
                   `ACTIVATE action` ) ).
    out->write( `Draft table after activation` ).
    out->write( |\n| ).
    out->write( data = draft_parent_afer_act name = `draft_parent_afer_act` ).
    out->write( |\n| ).
    out->write( `Database table after activation` ).
    out->write( |\n| ).
    out->write( data = db_tab_root_after_act name = `db_tab_root_after_act` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Creating Invalid Instances` ) ).

    "Purposely creating invalid instances;
    "draft indicator %is_draft is enabled
    MODIFY ENTITY zdemo_abap_rap_draft_m
     CREATE AUTO FILL CID
     FIELDS ( num1 arithm_op num2 )
     WITH VALUE #(
        ( %is_draft = if_abap_behv=>mk-on
          num1 = 1 arithm_op = 'a' num2 = 1 )  "wrong operator
        ( %is_draft = if_abap_behv=>mk-on
          num1 = 1 arithm_op = '/' num2 = 0 ) "0 division
        ( %is_draft = if_abap_behv=>mk-on
          num1 = 2 arithm_op = 'P' num2 = 12345 ) ) "arithmetic overflow
        FAILED f
        REPORTED r
        MAPPED m.

    "Displaying responses only if FAILED and REPORTED
    "response parameters are not initial.
    IF f IS NOT INITIAL OR r IS NOT INITIAL.
      out->write( data = `Responses after MODIFY operation` ).
      out->write( |\n| ).

      IF m IS NOT INITIAL.
        out->write( data = m name = `m` ).
        out->write( |\n| ).
      ENDIF.

      IF f IS NOT INITIAL.
        out->write( data = f name = `f` ).
        out->write( |\n| ).
      ENDIF.

      IF r IS NOT INITIAL.
        out->write( data = r name = `r` ).
        out->write( |\n| ).
      ENDIF.

    ENDIF.

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Retrieving draft table entries
    SELECT id, num1, arithm_op, num2, calc_result, crea_date_time,
           lchg_date_time, draftentitycreationdatetime,
           draftentitylastchangedatetime
     FROM zdemo_abap_draft
     ORDER BY id
     INTO TABLE @draft_parent_before_act.

    "Retrieving database table entries
    SELECT id, num1, arithm_op, num2, calc_result, crea_date_time,
           lchg_date_time
     FROM zdemo_abap_tabca
     ORDER BY id
     INTO TABLE @db_tab_root_before_act.

    "Filling the derived type for the ACTIVATE method by
    "getting %pid values; here, another table is filled for later use
    LOOP AT m-calc
      ASSIGNING FIELD-SYMBOL(<fs2>).

      APPEND VALUE #( %pid = <fs2>-%pid )
                             TO activate_tab2.

      APPEND VALUE #( %pid = <fs2>-%pid )
                             TO activate_tab3.
    ENDLOOP.

    MODIFY ENTITY zdemo_abap_rap_draft_m
      EXECUTE activate AUTO FILL CID WITH activate_tab2
      MAPPED m
      FAILED f
      REPORTED r.

    "Displaying responses only if FAILED and REPORTED
    "response parameters are not initial.
    IF f IS NOT INITIAL OR r IS NOT INITIAL.
      out->write( data = `Responses after MODIFY operation` ).
      out->write( |\n| ).

      IF m IS NOT INITIAL.
        out->write( data = m name = `m` ).
        out->write( |\n| ).
      ENDIF.

      IF f IS NOT INITIAL.
        out->write( data = f name = `f` ).
        out->write( |\n| ).
      ENDIF.

      IF r IS NOT INITIAL.
        out->write( data = r name = `r` ).
        out->write( |\n| ).
      ENDIF.

    ENDIF.

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Retrieving draft table entries
    SELECT id, num1, arithm_op, num2, calc_result, crea_date_time,
           lchg_date_time, draftentitycreationdatetime,
           draftentitylastchangedatetime
     FROM zdemo_abap_draft
     ORDER BY id
     INTO TABLE @draft_parent_afer_act.

    "Retrieving database table entries
    SELECT id, num1, arithm_op, num2, calc_result, crea_date_time,
           lchg_date_time
     FROM zdemo_abap_tabca
     ORDER BY id
     INTO TABLE @db_tab_root_after_act.

    "Displaying entries
    out->write( zcl_demo_abap_aux=>heading( `2a) Draft and database tables before ` &&
                   `ACTIVATE action` ) ).
    out->write( `Draft table before activation` ).
    out->write( |\n| ).
    out->write( data = draft_parent_before_act name = `draft_parent_before_act` ).
    out->write( |\n| ).
    out->write( `Database table before activation` ).
    out->write( |\n| ).
    out->write( data = db_tab_root_before_act name = `db_tab_root_before_act` ).

    out->write( zcl_demo_abap_aux=>heading( `2b) Draft and database tables after ` &&
                   `ACTIVATE action` ) ).
    out->write( `Draft table after activation` ).
    out->write( |\n| ).
    out->write( data = draft_parent_afer_act name = `draft_parent_afer_act` ).
    out->write( |\n| ).
    out->write( `Database table after activation` ).
    out->write( |\n| ).
    out->write( data = db_tab_root_after_act name = `db_tab_root_after_act` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Correcting and Updating Invalid Instances` ) ).

    "Preparing the derived type for the read operation to
    "retrieve the field values; the draft indicator is enabled
    LOOP AT activate_tab3 ASSIGNING FIELD-SYMBOL(<fs3>).

      APPEND VALUE #( %pky = <fs3>-%pky
                      %is_draft = if_abap_behv=>mk-on
                      %control-id = if_abap_behv=>mk-on
                      %control-num1 = if_abap_behv=>mk-on
                      %control-arithm_op = if_abap_behv=>mk-on
                      %control-num2 = if_abap_behv=>mk-on
                      %control-calc_result = if_abap_behv=>mk-on
                      %control-crea_date_time = if_abap_behv=>mk-on
                      %control-lchg_date_time = if_abap_behv=>mk-on
                    ) TO read_tab.
    ENDLOOP.

    "Retrieving the entries of the invalid instances
    READ ENTITY zdemo_abap_rap_draft_m
      ALL FIELDS WITH read_tab
      RESULT DATA(result).

    "Correcting and updating the invalid instances
    MODIFY ENTITY zdemo_abap_rap_draft_m
      UPDATE FROM VALUE #(
        FOR wa IN result (
            %pid = wa-%pid
            %is_draft = if_abap_behv=>mk-on
            num2 = SWITCH #( wa-calc_result
              WHEN `Division by 0` THEN 2
              WHEN `Overflow error` THEN 3 )
            arithm_op = SWITCH #( wa-calc_result
              WHEN `Wrong operator` THEN '+' )
            %control-num2 = SWITCH #( wa-calc_result
              WHEN `Division by 0` THEN if_abap_behv=>mk-on
              WHEN `Overflow error` THEN if_abap_behv=>mk-on
              ELSE if_abap_behv=>mk-off )
            %control-arithm_op = SWITCH #( wa-calc_result
              WHEN `Wrong operator` THEN if_abap_behv=>mk-on
              ELSE if_abap_behv=>mk-off ) ) )
        FAILED f
        REPORTED r
        MAPPED m.

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    MODIFY ENTITY zdemo_abap_rap_draft_m
      EXECUTE activate AUTO FILL CID WITH activate_tab3
      MAPPED m
      FAILED f
      REPORTED r.

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Retrieving draft table entries
    SELECT id, num1, arithm_op, num2, calc_result, crea_date_time,
           lchg_date_time, draftentitycreationdatetime,
           draftentitylastchangedatetime
     FROM zdemo_abap_draft
     ORDER BY id
     INTO TABLE @draft_parent_afer_act.

    "Retrieving database table entries
    SELECT id, num1, arithm_op, num2, calc_result, crea_date_time,
           lchg_date_time
     FROM zdemo_abap_tabca
     ORDER BY id
     INTO TABLE @db_tab_root_after_act.

    "Displaying entries
    out->write( data = `Draft and database tables after ` &&
                   `ACTIVATE action` ).
    out->write( |\n| ).
    out->write( `Draft table after activation` ).
    out->write( |\n| ).
    out->write( data = draft_parent_afer_act name = `draft_parent_afer_act` ).
    out->write( |\n| ).
    out->write( `Database table after activation` ).
    out->write( |\n| ).
    out->write( data = db_tab_root_after_act name = `db_tab_root_after_act` ).
  ENDMETHOD.

  METHOD initialize_dbtabs.
    DELETE FROM zdemo_abap_tabca.
    DELETE FROM zdemo_abap_draft.
  ENDMETHOD.
ENDCLASS.
