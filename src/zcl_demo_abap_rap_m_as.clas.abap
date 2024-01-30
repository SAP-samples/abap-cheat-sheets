***********************************************************************
*
*             RAP BO consumer for a RAP demo scenario:
* Managed RAP BO with managed internal numbering and additional save
* demonstrating the local consumption of RAP business events
*
* -------------------------- PURPOSE ----------------------------------
* - Primarily, the example demonstrates the local consumption of RAP
*   business events.
* - For that purpose, the BDEF defines three events. Two of them are
*   specified with a parameter. The events are raised for create, update
*   and delete operations.
* - The example implementation in this class (the RAP BO consumer)
*   contains three ABAP EML modify requests: a RAP create, update and
*   delete operation. For each of the operations, an event is raised
*   using a RAISE ENTITY EVENT statement. The events are raised in the
*   save_modified RAP saver method in the CCIMP include of the behavior
*   pool.
* - When the events are raised, the RAP event handler methods are called
*   asynchronously. To demonstrate the effect of the events, a database
*   table representing a log table is populated.
* - In the output of the example, the content of internal tables is
*   displayed to demonstrate the effect of the RAP operations by
*   selecting from the database table where the RAP BO instances are
*   persisted to after each RAP operation. Additionally, the content
*   of an internal table is displayed including the entries that have been
*   inserted into the log database table by the event handler methods.
*   In this self-contained example, this 'log database table' is just a
*   database table that is used to store some entries triggered by the RAP
*   events for visualization purposes. You can imagine that, for example,
*   the sending of an email is triggered there, or the application log is
*   filled, and so on. The log table is used in another RAP example as
*   draft table. The draft concept is not relevant for this simplified
*   example here.
* - Note the comments in the example code. You can check out the
*   asynchronity by commenting out the WAIT statement further down.
*
* ----------------- RELATED ARTIFACTS OF THE EXAMPLE ------------------*
* - RAP BO consumer: zcl_demo_abap_rap_m_as (this class here)
* - RAP BO provider (ABAP behavior pool): zbp_demo_abap_rap_ro_m_as
* - RAP event handler: zcl_demo_abap_rap_evt_handler
* - BDEF: zdemo_abap_rap_ro_m_as
* - More artifacts are related such as database tables, CDS views, and
*   an abstract entity (zdemo_abap_abstract_ent; used for the parameter
*   specifications for the events in the BDEF)
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
"! <p class="shorttext synchronized">ABAP cheat sheet: Local consumption of RAP Business Events</p>
"! Example to demonstrate local consumption of RAP business events in the context of a RAP demo scenario (managed RAP BO with managed internal numbering and additional save).
"! The class represents a RAP BO consumer.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_rap_m_as DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    CLASS-METHODS:
      class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_rap_m_as IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    out->write( |ABAP Cheat Sheet Example: Local Consumption of RAP Business Events\n\n| ).
    "RAP create operation that creates multiple calculations
    "Note the AUTO FILL CID addition with which the required %cid values
    "for RAP BO instances are created automatically. Since the scenario is
    "managed internal numbering, the 'id' field is assigned an appropriate value
    "automatically by the RAP framework.
    MODIFY ENTITY zdemo_abap_rap_ro_m_as
      CREATE AUTO FILL CID
      FIELDS ( num1 arithm_op num2 )
      WITH VALUE #( ( num1 = 1
                      arithm_op = '+'
                      num2    = 2 )
                    ( num1 = 5
                      arithm_op = '-'
                      num2    = 30 )
                    ( num1 = 3
                      arithm_op = '*'
                      num2    = 3 )
                    ( num1 = 7
                      arithm_op = '/'
                      num2    = 5 )
                    ( num1 = 2
                      arithm_op = 'P'
                      num2    = 4 )
                    ( num1 = 10
                      arithm_op = 'P'
                      num2    = 1000000000 )
                    ( num1 = 2
                      arithm_op = '/'
                      num2    = 0 ) )
      MAPPED DATA(m_cr)
      FAILED DATA(f_cr)
      REPORTED DATA(r_cr).

    COMMIT ENTITIES.

    IF sy-subrc <> 0.
      out->write( `An issue occurred in the RAP save sequence.` ).
    ENDIF.

    "Getting and displaying persisted instances to show the effect of the
    "EML MODIFY statement
    SELECT id, num1, arithm_op, num2, calc_result, crea_date_time, lchg_date_time
     FROM zdemo_abap_tabca
     ORDER BY lchg_date_time
     INTO TABLE @DATA(dbtab_entries).

    out->write( `Database table entries after the create operation`
      )->write( dbtab_entries
      )->write( |\n| ).

**********************************************************************

    "RAP update operation
    "In the example, all RAP BO instances that were created above are
    "updated. Here, the second number is updated with a random
    "integer (from the value range 1 - 10). For this purpose, the
    "cl_abap_random_int class is used.
    "The reference to existing instances is made using entries in the
    "mapped reponse table from above (it contains all keys of the created
    "RAP BO instances).
    IF m_cr-root IS NOT INITIAL.
      MODIFY ENTITY zdemo_abap_rap_ro_m_as
        UPDATE FIELDS ( num2 )
        WITH VALUE #( FOR wa IN m_cr-root ( id   = wa-id
                                            num2 = cl_abap_random_int=>create(
                                                    seed = cl_abap_random=>seed( )
                                                    min  = 1
                                                    max  = 10 )->get_next( ) ) )
      FAILED DATA(f_upd)
      REPORTED DATA(r_upd).

      COMMIT ENTITIES.

      IF sy-subrc <> 0.
        out->write( `An issue occurred in the RAP save sequence.` ).
      ENDIF.

      "Getting and displaying persisted instances to show the effect of the
      "EML MODIFY statement
      SELECT id, num1, arithm_op, num2, calc_result, crea_date_time, lchg_date_time
       FROM zdemo_abap_tabca
       ORDER BY lchg_date_time
       INTO TABLE @dbtab_entries.

      out->write( `Database table entries after the update operation`
        )->write( dbtab_entries
        )->write( |\n| ).
    ENDIF.

**********************************************************************

    "RAP delete operation
    "As above, the reference to existing RAP BO instances is made
    "using entries in the mapped response table. In the example,
    "the first three instances created are deleted.
    IF lines( m_cr-root ) > 3.
      MODIFY ENTITY zdemo_abap_rap_ro_m_as
       DELETE FROM VALUE #( ( id = m_cr-root[ 1 ]-id )
                            ( id = m_cr-root[ 2 ]-id )
                            ( id = m_cr-root[ 3 ]-id ) )
       FAILED DATA(f_del)
       REPORTED DATA(r_del).

      COMMIT ENTITIES.

      IF sy-subrc <> 0.
        out->write( `An issue occurred in the RAP save sequence.` ).
      ENDIF.

      "Getting and displaying persisted instances to show the effect of the
      "EML MODIFY statement
      SELECT id, num1, arithm_op, num2, calc_result, crea_date_time, lchg_date_time
       FROM zdemo_abap_tabca
       ORDER BY lchg_date_time
       INTO TABLE @dbtab_entries.

      out->write( `Database table entries after the delete operation`
        )->write( dbtab_entries
        )->write( |\n| ).
    ENDIF.

**********************************************************************

    "Note:
    "- Due to the asynchronous call of the events, a WAIT statement
    "  is included to give the events some time in this self-contained example,
    "  i.e. so that all the database table entries that are created in the
    "  RAP event handler method can be retrieved and displayed.
    "- In the RAP event handler method implementation, note the cl_abap_tx=>save( ).
    "  method call. This explicit activation of the 'save' transactional phase
    "  is required because, when called, the methods are started in the
    "  'modify' transactional phase. In the modify phase, database modification
    "  statements are not allowed. If the save phase is not activated,
    "  a following database modification statement results in an error.

    "To explore the asynchronity of the event raising, you can comment out the
    "following WAIT statement.
    WAIT UP TO 2 SECONDS.
    DATA(tmstmp_after_wait) = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
    out->write( |Timestamp (e.g. for comparing with the timestamps of the events raised): { tmstmp_after_wait }| ).
    out->write( |\n| ).

    "The global class of the behavior pool contains a static attribute of type i.
    "The example is implemented as follows: When events are raised in the save_modified
    "method, this number is increased by 1 per event raised. The number represents
    "the expected events to be raised. It is used in the following example implementation.
    "As mentioned above, you can comment out the WAIT statement to potentially see
    "a different number here compared to the number of entries in the log table.
    out->write( |Number of expected events raised: { zbp_demo_abap_rap_ro_m_as=>num_raised_events }| ).
    out->write( |\n| ).

    "Getting and displaying database table entries that were inserted
    "in the event handler method implementations to demonstrate that
    "events have been triggered.
    SELECT calc_result, crea_date_time
     FROM zdemo_abap_draft
     ORDER BY crea_date_time
     INTO TABLE @DATA(evt_log_entries).

    out->write( |Entries in log table at this stage (after SELECT from database table): { lines( evt_log_entries ) }| ).
    out->write( |\n| ).

    DATA(flag) = abap_false.
    IF evt_log_entries IS INITIAL.
      out->write( `There are no entries in the log table.` &&
        ` Try and run the example again.` ).
      flag = abap_true.
    ELSEIF lines( evt_log_entries ) BETWEEN 1 AND zbp_demo_abap_rap_ro_m_as=>num_raised_events - 1.
      out->write( `Log database table entries created by the ` &&
          `raised events` )->write( `Note that not all expected log database table entries have been created yet by the ` &&
          `raised events`
          )->write( evt_log_entries ).
      flag = abap_true.
    ELSE.
      out->write( `Log database table entries created by the ` &&
        `raised events`
        )->write( evt_log_entries ).
    ENDIF.

    "The following implementation is included for exploring the asynchronity in the self-contained example, if you
    "have commented out the WAIT statement above, or if not all expected entries are available in the database table
    "yet. This is just to give it some more time and select from the database table again.
    IF flag = abap_true.
      out->write( |\n| ).
      out->write( |******************************************************| ).
      out->write( |\n| ).
      out->write( |Out of { zbp_demo_abap_rap_ro_m_as=>num_raised_events } events that are expected to be raised in the example implementation, | &&
                  |only { lines( evt_log_entries ) } events are available in the database table at this stage. So, waiting a bit more ...| ).
      out->write( |\n| ).

      WAIT UP TO 1 SECONDS.

      SELECT calc_result, crea_date_time
       FROM zdemo_abap_draft
       ORDER BY crea_date_time
       INTO TABLE @evt_log_entries.

      IF lines( evt_log_entries ) = zbp_demo_abap_rap_ro_m_as=>num_raised_events.
        out->write( `Log database table entries created by the ` &&
      `raised events after waiting some more time`
      )->write( evt_log_entries ).
      ELSE.
        out->write( |Hmm... still not all events are available in the database table.| ).
        out->write( evt_log_entries ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD class_constructor.
    DELETE FROM zdemo_abap_tabca.
    DELETE FROM zdemo_abap_draft.
    CLEAR zbp_demo_abap_rap_ro_m_as=>num_raised_events.
  ENDMETHOD.
ENDCLASS.
