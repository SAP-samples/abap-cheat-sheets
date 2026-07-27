"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class is demonstrates ABAP Unit tests for background processing logic using the ABAP Background
"! Processing Framework (bgPF). It shows how background processing can be inspected with framework test
"! spies instead of real asynchronous execution. The class can also be run using F9 as it implements the
"! if_oo_adt_classrun~main method, illustrating the effect of the background processing.
CLASS zcl_demo_aunit_bgpf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_bgmc_op_single.

    "! <p class="shorttext synchronized" lang="en">Executes the main background process and handles exceptions</p>
    "!
    "! @raising cx_bgmc | <p class="shorttext synchronized" lang="en">Exception class for background processing errors</p>
    METHODS execute RAISING cx_bgmc.

    "! Executes the main background process two times in a loop, handling exceptions
    "!
    "! @raising cx_bgmc | <p class="shorttext synchronized" lang="en">Exception class for background processing errors</p>
    METHODS execute_2 RAISING cx_bgmc.

**********************************************************************

    "! Deletes all entries from ztaunitflights when the class is instantiated
    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized" lang="en">Initializes the instance attribute with the provided string</p>
    "!
    "! @parameter str | <p class="shorttext synchronized" lang="en">The string to be set as the instance attribute</p>
    METHODS constructor
      IMPORTING
        str TYPE string OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Returns the input string that was set in the constructor</p>
    "!
    "! @parameter input | <p class="shorttext synchronized" lang="en">The input string initialized in the constructor</p>
    METHODS get_input
      RETURNING
        VALUE(input) TYPE string.
  PRIVATE SECTION.
    DATA string TYPE string.

    "! Transforms the instance attribute to upper case
    METHODS set_attribute.

    "! Modifies database table ztaunitflights with predefined flight records
    METHODS modify_dbtab.
ENDCLASS.

CLASS zcl_demo_aunit_bgpf IMPLEMENTATION.

  METHOD constructor.
    string = str.
  ENDMETHOD.

  METHOD if_bgmc_op_single~execute.
    set_attribute( ).

    cl_abap_tx=>save( ).

    modify_dbtab( ).
  ENDMETHOD.

  METHOD get_input.
    RETURN string.
  ENDMETHOD.

  METHOD set_attribute.
    IF string IS INITIAL.
      ASSERT 1 = 0.
    ENDIF.

    string = to_upper( string ).
  ENDMETHOD.

  METHOD modify_dbtab.
    MODIFY ztaunitflights FROM TABLE @( VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                          ( carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 )
                     ( carrid = 'AA' connid = '1003' fldate = '20260803' seatsmax = 95 seatsocc = 50 ) ) ).
  ENDMETHOD.

  METHOD class_constructor.
    DELETE FROM ztaunitflights.
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    TRY.
        execute( ).
      CATCH cx_bgmc INTO DATA(error).
        out->write( error->get_text( ) ).
        RETURN.
    ENDTRY.

    WAIT UP TO 1 SECONDS.
    SELECT * FROM ztaunitflights INTO TABLE @DATA(flights).
    out->write( flights ).
  ENDMETHOD.

  METHOD execute.
    TRY.
        cl_bgmc_process_factory=>get_default(
                    )->create(
                    )->set_name( 'Test process'
                    )->set_operation( NEW zcl_demo_aunit_bgpf( 'abc' )
                    )->save_for_execution( ).

        COMMIT WORK.

      CATCH cx_bgmc INTO DATA(error).
        ROLLBACK WORK.

        RAISE EXCEPTION error.
    ENDTRY.
  ENDMETHOD.

  METHOD execute_2.
    DO 2 TIMES.
      TRY.
          cl_bgmc_process_factory=>get_default(
                      )->create(
                      )->set_name( |Test process { sy-index }|
                      )->set_operation( NEW zcl_demo_aunit_bgpf( |Number { sy-index }| )
                      )->save_for_execution( ).

          COMMIT WORK.

        CATCH cx_bgmc INTO DATA(error).
          ROLLBACK WORK.

          RAISE EXCEPTION error.
      ENDTRY.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
