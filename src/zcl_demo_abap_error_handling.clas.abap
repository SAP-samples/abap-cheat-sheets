"! <p class="shorttext"><strong>Exceptions and Runtime Errors</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates syntax and concepts related to exceptions and runtime errors.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Information</h2>
"! <p>Find information on getting started with the example class and the disclaimer in
"! the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
CLASS zcl_demo_abap_error_handling DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    CLASS-METHODS meth_a
      IMPORTING num TYPE i
      RAISING   zcx_demo_abap_error_a.

    CLASS-METHODS meth_b
      IMPORTING num TYPE i
      RAISING   zcx_demo_abap_error_b.

    CLASS-METHODS meth_resumable
      IMPORTING num1              TYPE i
                num2              TYPE i
      RETURNING VALUE(div_result) TYPE string
      RAISING   RESUMABLE(zcx_demo_abap_error_b).

    CLASS-METHODS divide
      IMPORTING num1              TYPE i
                num2              TYPE i
      RETURNING VALUE(div_result) TYPE decfloat34
      RAISING   cx_sy_zerodivide.

    CLASS-METHODS get_uuid
      RETURNING VALUE(uuid) TYPE sysuuid_x16
      RAISING   cx_uuid_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_error_handling IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP cheat sheet example: Exceptions and Runtime Errors\n\n| ).
    out->write( `1) Exception Categories` ).

    "Method specifying an exception class that inherits from CX_DYNAMIC_CHECK

    "No explicit exception handling required
    "The following statement does not show a syntax warning.
    DATA(div_result1) = divide( num1 = 5 num2 = 2 ).
    out->write( data = div_result1 name = `div_result1` ).

    TRY.
        DATA(div_result2) = divide( num1 = 5 num2 = 0 ).
      CATCH cx_sy_zerodivide.
        out->write( `Exception caught` ).
    ENDTRY.

    "Method specifying an exception class that inherits from CX_STATIC_CHECK

    "Explicit exception handling required
    "The following statement (commented in) shows a syntax warning.
    "DATA(uuid1) = get_uuid( ).

    TRY.
        DATA(uuid2) = get_uuid( ).
      CATCH cx_uuid_error.
    ENDTRY.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Raising Class-Based Exceptions` ) ).

    "Note: The examples show a selection. More additions are available.
    "Some are demonstrated further down.

    "RAISE EXCEPTION statements

    "RAISE EXCEPTION statement with the TYPE addition, specifying
    "the name of a visible exception class; an exception
    "object is created (if necessary, see the ABAP Keyword Documentation
    "for more details)
    TRY.
        RAISE EXCEPTION TYPE cx_sy_zerodivide.
      CATCH cx_sy_zerodivide.
    ENDTRY.

    "RAISE EXCEPTION statement specifying an exception object (an object
    "reference variable pointing to an exception class)
    DATA(exc) = NEW cx_sy_zerodivide( ).
    TRY.
        RAISE EXCEPTION exc.
      CATCH cx_sy_zerodivide.
    ENDTRY.

    "Creating an exception object inline using the NEW operator
    TRY.
        RAISE EXCEPTION NEW cx_sy_zerodivide( ).
      CATCH cx_sy_zerodivide.
    ENDTRY.

    "Note: Instances of abstract classes cannot be created. So, the
    "following statements are not possible.
    "RAISE EXCEPTION NEW cx_sy_arithmetic_error( ).
    "RAISE EXCEPTION NEW cx_static_check( ).
    "RAISE EXCEPTION NEW cx_dynamic_check( ).
    "RAISE EXCEPTION NEW cx_no_check( ).
    "RAISE EXCEPTION NEW cx_root( ).

    "Dynamic creation of an exception object with CREATE OBJECT
    DATA dyn_exc TYPE REF TO cx_root.
    CREATE OBJECT dyn_exc TYPE ('CX_SY_ZERODIVIDE').
    TRY.
        RAISE EXCEPTION dyn_exc.
      CATCH cx_root.
    ENDTRY.

    "COND/SWITCH operators with THROW addition
    "THROW addition in conditional expressions with the COND and SWITCH operators
    "enabling raising class-based exceptions in operand positions
    "The addition works like RAISE EXCEPTION TYPE statements.

    "COND operator
    DATA(int1) = 1.
    DATA(int2) = 0.
    "The example considers ABAP "allowing" zero division when both operands are 0.
    TRY.
        DATA(res1) = COND decfloat34( WHEN ( int1 <> 0 AND int2 <> 0 ) OR ( int1 = 0 AND int2 <> 0 ) THEN int1 / int2
                                      ELSE THROW cx_sy_zerodivide( ) ).
      CATCH cx_sy_zerodivide.
    ENDTRY.

    "SWITCH operator
    "The following example shows SWITCH with the THROW addition
    "and uses cx_sy_zerodivide for demonstration purposes.
    DO.
      TRY.
          DATA(num) = SWITCH #( sy-index
                                WHEN 1 THEN `one`
                                WHEN 2 THEN `two`
                                WHEN 3 THEN `three`
                                WHEN 4 THEN `four`
                                ELSE THROW cx_sy_zerodivide( ) ).
          out->write( num ).
        CATCH cx_sy_zerodivide.
          out->write( `Exception caught` ).
          EXIT.
      ENDTRY.
    ENDDO.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Excursions with TRY control structures` ) ).

    "TRY control structures are meant for handling catchable exceptions locally
    "The example shows divisions. The predefined exception class cx_sy_zerodivide
    "as suitable exception class is used.
    "If the exception is not handled, the program is terminated and the runtime
    "error COMPUTE_INT_ZERODIVIDE occurs.
    "The third calculation is not carried out because the statement block is
    "left due to the previous erroneous 0 division.

    TRY.
        DATA(div1) = 4 / 2.
        out->write( data = div1 name = `div1` ).
        out->write( |\n| ).

        DATA(div2) = 4 / 0.
        out->write( data = div2 name = `div2` ).
        out->write( |\n| ).

        DATA(div3) = 9 / 3.
        out->write( data = div3 name = `div3` ).
        out->write( |\n| ).

      CATCH cx_sy_zerodivide.
        out->write( `0 division. The exception was caught.` ).
        out->write( |\n| ).
    ENDTRY.

    "It is possible to specify multiple exception classes in a list and
    "multiple CATCH blocks.
    "Note: If there are multiple CATCH blocks for exceptions that are in an inheritance
    "relationship, you must pay attention that the more special exceptions are specified
    "before the more general ones.
    "The calculation example shows multiple CATCH blocks that themselves have more than
    "one exception class specified. Here, local exception classes are specified just for
    "demonstration purposes. They are not relevant in this TRY control structure.

    DATA int_itab TYPE TABLE OF i WITH EMPTY KEY.

    "Filling internal table of type i as basis for calculations
    int_itab = VALUE #( ( 5 ) ( 0 ) ( 987654321 ) ).

    LOOP AT int_itab ASSIGNING FIELD-SYMBOL(<fs_int>).
      TRY.
          out->write( |--- Calculations with { <fs_int> } ---| ).
          DATA(calc1) = CONV decfloat34( 1 / <fs_int> ).
          out->write( data = calc1 name = `calc1` ).
          out->write( |\n| ).
          DATA(calc2) = ipow( base = <fs_int> exp = 2 ).
          out->write( data = calc2 name = `calc2` ).
          out->write( |\n| ).
        CATCH cx_sy_arithmetic_overflow .
          out->write( `Arithmetic overflow. The exception was caught.` ).
        CATCH cx_sy_zerodivide .
          out->write( `0 division. The exception was caught.` ).
      ENDTRY.
      out->write( |\n| ).
    ENDLOOP.

    "The following example shows a catchable exception that is
    "raised if a line is not found when using table expressions.
    DATA(str_table) = VALUE string_table(  ).
    TRY.
        DATA(line_tab) = str_table[ 12345 ].

        "The predefined exception class cx_sy_itab_line_not_found
        "as suitable exception class is used here.
        "If the exception is not handled, the program is terminated
        "and the runtime error ITAB_LINE_NOT_FOUND occurs.
      CATCH cx_sy_itab_line_not_found.
        out->write( `The line was not found. The exception was caught.` ).
    ENDTRY.

    "In the following CATCH block, the predefined exception class cx_sy_arithmetic_error
    "is specified. Both cx_sy_zerodivide and cx_sy_arithmetic_overflow are derived from
    "cx_sy_arithmetic_error which is an exception class higher up in the inheritance
    "tree. Hence, cx_sy_arithmetic_error can be specified and handle both exceptions, too.
    "The following example is basically the same as above. However, only one exception
    "class is specified.

    LOOP AT int_itab ASSIGNING FIELD-SYMBOL(<fs_int_inh>).
      TRY.
          out->write( |--- Calculations with { <fs_int_inh> } ---| ).
          calc1 = 1 / <fs_int_inh>.
          out->write( data = calc1 name = `calc1` ).
          out->write( |\n| ).
          calc2 = ipow( base = <fs_int_inh> exp = 2 ).
          out->write( data = calc2 name = `calc2` ).
          out->write( |\n| ).
        CATCH cx_sy_arithmetic_error.
          out->write( `Arithmetic error. The exception was caught.` ).
      ENDTRY.
      out->write( |\n| ).
    ENDLOOP.

    "Example demonstrating the exception root class cx_root specifying after CATCH to
    "catch all catchable exception.
    DO 3 TIMES.
      TRY.
          CASE sy-index.
            WHEN 1.
              RAISE EXCEPTION TYPE cx_sy_zerodivide.
            WHEN 2.
              RAISE EXCEPTION TYPE cx_uuid_error.
            WHEN 3.
              RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
          ENDCASE.
        CATCH cx_root.
          "Instead of explicit specification of potential exception classes involved
          "CATCH cx_sy_zerodivide cx_uuid_error cx_sy_itab_line_not_found.
      ENDTRY.
    ENDDO.

    "Evaluating exception information (get_text method)

    "You can use the addition INTO plus an object reference variable to store
    "a reference to an exception object. It is, for example, relevant to
    "determine the exact exception.
    "The following example is the same as above using the more general exception
    "class cx_sy_arithmetic_error. You can carry out certain tasks, for
    "example, retrieving and displaying the exception text. To retrieve exception
    "texts, you can call, for example, the method get_text.

    DATA exception TYPE REF TO cx_root.

    LOOP AT int_itab ASSIGNING FIELD-SYMBOL(<fs_int_into>).
      TRY.
          out->write( |--- Calculations with { <fs_int_into> } ---| ).
          calc1 = 1 / <fs_int_into>.
          out->write( data = calc1 name = `calc1` ).
          out->write( |\n| ).
          calc2 = ipow( base = <fs_int_into> exp = 2 ).
          out->write( data = calc2 name = `calc2` ).
          out->write( |\n| ).
        CATCH cx_sy_arithmetic_error INTO exception.
          "Note:
          "- The object reference variable is of type cx_root.
          "- You could also create the variable inline, e. g. ... INTO DATA(exc).

          "Retrieving and displaying exception text
          DATA(exception_text) = exception->get_text( ).

          out->write( data = exception_text name = `exception_text` ).
      ENDTRY.
      out->write( |\n| ).
    ENDLOOP.

    "As above, the following example demonstrates an exception class for
    "arithmetic operations that is higher up in the inheritance tree and
    "catches zero division and arithmetic overflow errors.
    DO 2 TIMES.
      TRY.
          IF sy-index = 1.
            DATA(div) = CONV decfloat34( 1 / 0 ).
          ELSE.
            DATA(powers) = ipow( base = 10 exp = 100 ).
          ENDIF.
        CATCH cx_sy_arithmetic_error INTO DATA(error_arithm).
          DATA(text) = error_arithm->get_text( ).
          "Using RTTI to retrieve the relative name of the class
          DATA(cl_name) = CAST cl_abap_classdescr(
            cl_abap_typedescr=>describe_by_object_ref( error_arithm ) )->get_relative_name( ).

          out->write( text ).
          out->write( cl_name ).
      ENDTRY.
    ENDDO.

    "Demonstrating the instance attribute 'previous' with
    "nested TRY control structures.
    "Consider the following scenario: A method is called and
    "an exception is raised. Once evaluated, another exception
    "is raised, and the previous exception (i.e. a reference to
    "the previous exception object) is passed on to the caller.
    "The caller can then evaluate the exceptions raised.
    "The following example includes nested TRY control structures.
    "In CATCH blocks, further exceptions are raised. There, the
    "exception object is passed. In a WHILE loop, several pieces
    "of information are retrieved, including the use of RTTI.
    "Once the information has been retrieved, the previous attribute
    "is used to assign the previous exception object. Note the cast.

    DATA info TYPE string_table.

    TRY.
        TRY.
            TRY.
                RAISE EXCEPTION NEW cx_sy_zerodivide( ).
              CATCH cx_sy_zerodivide INTO DATA(err1).
                RAISE EXCEPTION NEW cx_sy_arithmetic_overflow( previous = err1 ).
            ENDTRY.
          CATCH cx_sy_arithmetic_overflow INTO DATA(err2).
            RAISE EXCEPTION NEW cx_sy_itab_line_not_found( previous = err2 ).
        ENDTRY.
      CATCH cx_sy_itab_line_not_found INTO DATA(err3).
    ENDTRY.

    DATA(acc_err) = CAST cx_root( err3 ).
    WHILE acc_err IS BOUND.
      DATA(txt) = acc_err->get_text( ).

      acc_err->get_source_position(
        IMPORTING
          program_name = DATA(prog_name)
          include_name = DATA(incl_name)
          source_line  = DATA(src_line) ).

      "Using RTTI to gather more information on the exception
      "See the Dynamic Programming cheat sheet about RTTI.
      DATA(tdo) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( acc_err ) ).
      DATA(cl_relative_name) = tdo->get_relative_name( ).
      DATA(attributes) = tdo->attributes.
      DATA(superclass) = tdo->get_super_class_type( )->get_relative_name( ).

      "Populating the info table
      APPEND |--------------- { sy-index } ---------------| TO info.
      APPEND |Text: { txt }| TO info.
      APPEND |Position of raised exception: { prog_name } / { incl_name } / { src_line }| TO info.
      APPEND |Exception class name: { cl_relative_name }| TO info.
      APPEND |Superclass of exception class: { superclass }| TO info.

      "Using the 'previous' attribute to assign the previous exception
      "object
      acc_err = acc_err->previous.
      "If there was no cast to cx_root, a cast would be required here,
      "for example, as follows.
      "acc_err = CAST #( acc_err->previous ).
    ENDWHILE.

    out->write( data = info name = `info` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) CLEANUP` ) ).

    "The following example shows nested TRY control structures.
    "The inner TRY control structure includes a CLEANUP block.
    "The example demonstrates that an exception raised cannot
    "be handled by the TRY control structure where it is raised.
    "No meaningful 'cleanup' action is performed in the example's
    "CLEANUP block. For demonstration purposes, an info table is
    "populated demonstrating the execution of the block.

    DATA info_tab TYPE string_table.
    DATA cleanup TYPE REF TO cx_root.
    DO 2 TIMES.
      APPEND |---- Loop pass { sy-index } ----| TO info_tab.
      TRY.
          TRY.

              IF sy-index = 1.
                DATA(calc) = CONV decfloat34( 1 / 0 ).
              ELSE.
                DATA(strtab) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).
                DATA(line_4) = strtab[ 4 ].
              ENDIF.

            CATCH cx_sy_zerodivide INTO cleanup.
              APPEND `---- Catching cx_sy_zerodivide ----` TO info_tab.
              APPEND cleanup->get_text( ) TO info_tab.

            CLEANUP.
              APPEND `#### Executing CLEANUP block ####` TO info_tab.
              APPEND `d` TO strtab.

              "CLEANUP block must be executed completely
              "Leaving the block prematurely causes a runtime error. If it is statically known
              "that it is not possible to return to the block, a syntax error is shown.
              "So, the following statement is not allowed in the CLEANUP block.
              "RETURN.
          ENDTRY.
        CATCH cx_sy_itab_line_not_found INTO cleanup.
          APPEND `---- Catching cx_sy_itab_line_not_found ----` TO info_tab.
          APPEND cleanup->get_text( ) TO info_tab.
      ENDTRY.
    ENDDO.

    out->write( data = info_tab name = `info_tab` ).
    out->write( |\n| ).
    out->write( |\n| ).

    "CLEANUP ... INTO
    "The example is the same as above. Here, the CLEANUP statement is specified
    "with the addition INTO. If required, you can evaluate the exception information
    "as shown above.

    DATA info_tab_b TYPE string_table.
    DATA cleanup_b TYPE REF TO cx_root.
    DO 2 TIMES.
      APPEND |---- Loop pass { sy-index } ----| TO info_tab_b.
      TRY.
          TRY.

              IF sy-index = 1.
                DATA(calc_b) = CONV decfloat34( 1 / 0 ).
              ELSE.
                DATA(strtab_b) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).
                DATA(line_4_b) = strtab_b[ 4 ].
              ENDIF.

            CATCH cx_sy_zerodivide INTO cleanup_b.
              APPEND `---- Catching cx_sy_zerodivide ----` TO info_tab_b.
              APPEND cleanup_b->get_text( ) TO info_tab_b.

            CLEANUP INTO cleanup_b.
              APPEND `#### Executing CLEANUP block ####` TO info_tab_b.
              "Using RTTI to find out the absolute name of the class of the raised execption
              DATA(class_name) = cl_abap_classdescr=>get_class_name( p_object = cleanup_b ).
              APPEND class_name TO info_tab_b.
              APPEND `d` TO strtab_b.

              "CLEANUP block must be executed completely
              "Leaving the block prematurely causes a runtime error. If it is statically known
              "that it is not possible to return to the block, a syntax error is shown.
              "So, the following statement is not allowed in the CLEANUP block.
              "RETURN.
          ENDTRY.
        CATCH cx_sy_itab_line_not_found INTO cleanup_b.
          APPEND `---- Catching cx_sy_itab_line_not_found ----` TO info_tab_b.
          APPEND cleanup_b->get_text( ) TO info_tab_b.
      ENDTRY.
    ENDDO.

    out->write( data = info_tab_b name = `info_tab_b` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) RETRY` ) ).

    "The following example includes a division of 1 by another number.
    "In a DO loop 1 is divided by all numbers from 10 to -10.
    "When the zero division exception is caught, the exception cause is
    "removed by changing the first operand's value to 0, too, before the
    "RETRY statement. The RETRY statement triggers the execution of the TRY
    "control structure again, now resulting in 0 as division result. After
    "that, and to have a self-contained example, the operand value is changed
    "back to 1.
    DATA division_result_tab TYPE TABLE OF decfloat34 WITH EMPTY KEY.
    DATA(number1) = 1.
    DATA(number2) = 11.
    DATA division_result TYPE decfloat34.
    DO.
      number2 -= 1.
      TRY.
          division_result = number1 / number2.
          APPEND division_result TO division_result_tab.
        CATCH cx_sy_zerodivide.
          "Removing the exception cause by setting a value that
          "does not raise the zero division exception
          number1 = 0.
          DATA(retry_flag) = abap_true.
          "Processing the TRY control structure again
          RETRY.
      ENDTRY.

      "Resetting of number1 value to 1
      IF retry_flag = abap_true.
        number1 = 1.
        retry_flag = abap_false.
      ENDIF.

      IF sy-index = 21.
        EXIT.
      ENDIF.
    ENDDO.

    out->write( data = division_result_tab name = `division_result_tab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) RESUME` ) ).

    DATA restab TYPE string_table.
    TYPES ty_inttab TYPE TABLE OF i WITH EMPTY KEY.
    DATA(inttab) = REDUCE ty_inttab( INIT tab = VALUE ty_inttab( )
                                     FOR  i = -5 UNTIL i > 5
                                     NEXT tab = VALUE #( BASE tab ( i ) ) ).

    LOOP AT inttab INTO DATA(wa).
      TRY.
          DATA(divres) = meth_resumable(
            num1 = sy-tabix
            num2 = wa
          ).
          APPEND |{ sy-tabix } / { wa } = { divres }| TO restab.
        CATCH BEFORE UNWIND zcx_demo_abap_error_b INTO DATA(error_resume).
          DATA(is_resumable) = error_resume->is_resumable.
          IF is_resumable IS NOT INITIAL.
            APPEND |Exception raised. Is resumable? -> "{ is_resumable }"| TO restab.
            RESUME.
          ENDIF.
      ENDTRY.
    ENDLOOP.

    out->write( data = restab name = `restab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) MESSAGE` ) ).

    "The following examples demonstrate several MESSAGE statements.
    TYPES c50 TYPE c LENGTH 50.
    DATA message_attribute_tab TYPE TABLE OF c50 WITH EMPTY KEY.

    MESSAGE e001(zdemo_abap_messages) INTO DATA(msg).

    DATA(msgid) = sy-msgid.
    DATA(msgty) = sy-msgty.

    message_attribute_tab = VALUE #( BASE message_attribute_tab ( msgid ) ( msgty ) ( '--------' ) ).

    MESSAGE e004(zdemo_abap_messages)
            WITH 'A' 'B' 'C' 'D'
            INTO msg.

    msgid = sy-msgid.
    msgty = sy-msgty.
    DATA(msgv1) = sy-msgv1.
    DATA(msgv2) = sy-msgv2.
    DATA(msgv3) = sy-msgv3.
    DATA(msgv4) = sy-msgv4.

    message_attribute_tab = VALUE #( BASE message_attribute_tab ( msgid ) ( msgty ) ( msgv1 )
                                                                ( msgv2 ) ( msgv3 ) ( msgv4 )
                                                                ( '--------' ) ).

    MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
        TYPE 'E'
        NUMBER '005'
        WITH 'E' 'F' 'G' 'H'
        INTO msg.

    msgid = sy-msgid.
    msgty = sy-msgty.
    msgv1 = sy-msgv1.
    msgv2 = sy-msgv2.
    msgv3 = sy-msgv3.
    msgv4 = sy-msgv4.

    message_attribute_tab = VALUE #( BASE message_attribute_tab ( msgid ) ( msgty ) ( msgv1 )
                                                                ( msgv2 ) ( msgv3 ) ( msgv4 ) ).

    out->write( data = message_attribute_tab name = `message_attribute_tab` ).
    out->write( |\n| ).

    "Getting all messages of a message class
    "Note: When a message class/number is not found, the type
    "is put in first position followed by the specified message
    "name. Message numbers have a three-digit number.
    DATA messages TYPE string_table.
    DO 999 TIMES.
      MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
         TYPE 'E'
         NUMBER sy-index
         INTO DATA(msg_of_msgcl).

      FIND PCRE `^E:ZDEMO_ABAP_MESSAGES` IN msg_of_msgcl.
      IF sy-subrc <> 0.
        APPEND |Message number { sy-index }: "{ msg_of_msgcl }"| TO messages.
      ENDIF.
    ENDDO.

    out->write( data = messages name = `messages` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Syntax Variants of RAISE EXCEPTION/THROW` ) ).

    "In the example, the RAISE EXCEPTION/THROW statements are implemented in a method.
    "Depending on a value, specific statements are called.

    TYPES: BEGIN OF exception_info,
             idx      TYPE i,
             exc_text TYPE string,
             BEGIN OF source_position,
               prog        TYPE syrepid,
               incl        TYPE syrepid,
               source_line TYPE i,
             END OF  source_position,
           END OF exception_info,
           ty_tab_exception_info TYPE TABLE OF exception_info WITH EMPTY KEY.
    DATA tab_exception_info TYPE ty_tab_exception_info.

    "Exception class implementing interface if_t100_message
    DO.
      TRY.
          APPEND VALUE #( idx = sy-index ) TO tab_exception_info REFERENCE INTO DATA(line).
          meth_a( sy-index ).
        CATCH zcx_demo_abap_error_a INTO DATA(error_a).
          line->exc_text = error_a->get_text( ).
          error_a->get_source_position(
            IMPORTING
              program_name = line->source_position-prog
              include_name = line->source_position-incl
              source_line  = line->source_position-source_line ).
      ENDTRY.
      IF sy-index = 20.
        EXIT.
      ENDIF.
    ENDDO.

    out->write( data = tab_exception_info name = `tab_exception_info` ).

    CLEAR tab_exception_info.

    "Exception class implementing interface if_t100_dyn_message
    DO.
      TRY.
          APPEND VALUE #( idx = sy-index ) TO tab_exception_info REFERENCE INTO line.
          meth_b( sy-index ).
        CATCH zcx_demo_abap_error_b INTO DATA(error_b).
          line->exc_text = error_b->get_text( ).
          error_b->get_source_position(
            IMPORTING
              program_name = line->source_position-prog
              include_name = line->source_position-incl
              source_line  = line->source_position-source_line ).
      ENDTRY.
      IF sy-index = 15.
        EXIT.
      ENDIF.
    ENDDO.

    out->write( data = tab_exception_info name = `tab_exception_info` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `9) Excursions` ) ).

    "Exploring the inheritance tree of exception classes
    DATA inheritance_tree TYPE string_table.

    DATA(class_name_table) = VALUE string_table( ( `CX_SY_ZERODIVIDE` )
                                                 ( `CX_SY_ITAB_LINE_NOT_FOUND` )
                                                 ( `CX_SY_CONVERSION_OVERFLOW` )
                                                 ( `CX_SY_RTTI_TYPE_NOT_RELEASED` )
                                                 ( `CX_ROOT` )
                                                 ( `CX_STATIC_CHECK` )
                                                 ( `CL_ABAP_TABLEDESCR` ) "Excursion: Not an exception class; class name of an RTTI class
                                                 ( `CX_THIS_CLASS_DOES_NOT_EXIST` )
                                               ).

    LOOP AT class_name_table INTO DATA(classname).
      DO.
        TRY.

            cl_abap_typedescr=>describe_by_name( EXPORTING p_name = classname
             RECEIVING p_descr_ref = DATA(tdo_type)
              EXCEPTIONS type_not_found  = 4 ) .
            IF  sy-subrc <> 0.
              APPEND `--- Class not found ---` TO inheritance_tree.
              EXIT.
            ELSE.
              APPEND classname TO inheritance_tree.
              DATA(tdo_cl) = CAST cl_abap_classdescr( tdo_type ).
            ENDIF.

            "This method uses classic exceptions
            tdo_cl->get_super_class_type(
              RECEIVING p_descr_ref = DATA(tdo_super_class)
              EXCEPTIONS super_class_not_found = 4 ).

            IF sy-subrc <> 0.
              EXIT.
            ELSE.
              classname = tdo_super_class->get_relative_name( ).
            ENDIF.

          CATCH cx_sy_rtti_type_not_released.
            APPEND `--- Class not released ---` TO inheritance_tree.
            EXIT.
        ENDTRY.
      ENDDO.

      out->write( `-------------------` ).
      out->write( data = inheritance_tree name = `inheritance_tree` ).
      CLEAR inheritance_tree.
    ENDLOOP.

    out->write( |\n| ).
    out->write( |\n| ).
    "Exploration down the inheritance tree (from superclass to subclasses)
    "You can use the XCO library. The example only uses examples with few
    "subclasses and classes not high up in an inheritance tree to reduce
    "the program runtime.
    class_name_table = VALUE string_table( ( `CX_SY_ZERODIVIDE` )
                                           ( `CX_SY_RTTI_NO_CHECK` )
                                           ( `CL_ABAP_TYPEDESCR ` ) "Excursion: Not an exception class; class name of an RTTI class
                                           ( `CX_THIS_CLASS_DOES_NOT_EXIST` )
                                         ).

    LOOP AT class_name_table INTO classname.
      DATA(xco_handler) = xco_cp_abap=>class( CONV sxco_ao_object_name( classname ) ).
      IF xco_handler->exists( ).
        "Getting the names of the subclasses
        DATA(subclass_names) = xco_handler->subclasses->all->get_names( ).
        IF subclass_names IS INITIAL.
          out->write( data = |The class { classname } has no subclasses.| ).
        ELSE.
          out->write( data = subclass_names name = `subclass_names` ).
        ENDIF.
        out->write( `-------------------` ).
      ELSE.
        out->write( |The class { classname } does not exist.| ).
      ENDIF.
    ENDLOOP.

    out->write( |\n| ).
    out->write( |\n| ).

    "Evaluating the return value of a non-class-based exception and raising a class-based exception
    TRY.
        cl_abap_typedescr=>describe_by_name( EXPORTING p_name = 'CL_THAT_DOES_NOT_EXIST'
                                             RECEIVING p_descr_ref = DATA(tdo_ty)
                                             EXCEPTIONS type_not_found = 4 ).

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_demo_abap_error_b MESSAGE e005(zdemo_abap_messages) WITH 'Type not found'.
        ELSE.
          DATA(tdo_class) = CAST cl_abap_classdescr( tdo_ty ).

          "Getting more type information; find more details in the Dynamic Programming cheat sheet
          ...
        ENDIF.
      CATCH zcx_demo_abap_error_b INTO DATA(err).
        DATA(error_text) = err->get_text( ).
        out->write( data = error_text name = `error_text` ).
    ENDTRY.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) Programmatically Raising Runtime Erros` ) ).

    out->write( `Obviously, the statements raising a runtime error are commented out :)` ).


    "RAISE SHORTDUMP TYPE cx_sy_zerodivide.

    DATA(flag) = abap_true.
    "DATA(cond_w_throw_shortdump) = COND #( WHEN flag IS INITIAL THEN `works`
    "                                       ELSE THROW SHORTDUMP zcx_demo_abap_error_b( ) ).

    "DATA(switch_w_throw_shortdump) = SWITCH #( flag WHEN '' THEN `works`
    "                                           ELSE THROW SHORTDUMP zcx_demo_abap_error_b( ) ).


    DATA(number) = 0.
    ASSERT number IS INITIAL.
    ASSERT number > -1.
    number = 1.
    ASSERT number = 1.

    flag = abap_false.
    "ASSERT flag = abap_true.
  ENDMETHOD.

  METHOD meth_resumable.

    IF num2 = 0 AND num1 <> 0.
      RAISE RESUMABLE EXCEPTION TYPE zcx_demo_abap_error_b.
      div_result = `No result. Resumed!`.
    ELSE.
      div_result = num1 / num2.
    ENDIF.

    "Statement with COND operator using RESUMABLE
    "div_result = COND #( WHEN num2 = 0 AND num1 <> 0 THEN num1 / num2
    "                     ELSE THROW RESUMABLE zcx_demo_abap_error( ) ).

  ENDMETHOD.

  METHOD meth_a.
    "Note: A selection of additions is covered.

    DATA(flag) = 'X'.

    CASE num.
      WHEN 1.
        "TYPE addition
        "Raises an exception of a specified exception class
        RAISE EXCEPTION TYPE zcx_demo_abap_error_a.
      WHEN 2.
        "TYPE addition including EXPORTING
        "Assigns actual parameters to input parameters of the instance constructor
        "particularly for classes implementing if_t100_message. The example uses
        "a constant structure having the same name as the exception class.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_a
          EXPORTING
            textid = zcx_demo_abap_error_a=>zcx_demo_abap_error_a.
      WHEN 3.
        "Assigning values to replace placeholders of a message
        "In this case, replacing the placeholders &1 and &2 in a message.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_a
          EXPORTING
            textid  = zcx_demo_abap_error_a=>error_003
            p_003_a = `a`
            p_003_b = `b`.

      WHEN 4.
        "Assigning values to replace placeholders of a message
        "In this case, not all available parameters are filled.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_a
          EXPORTING
            textid  = zcx_demo_abap_error_a=>error_004
            p_004_a = `c`
            p_004_b = `d`
            p_004_c = `e`.

      WHEN 5.
        "Specifying an exception object
        DATA(cx_oref) = NEW zcx_demo_abap_error_a( textid = zcx_demo_abap_error_a=>error_005
            p_005_a = `Some`
            p_005_b = `error` ).

        RAISE EXCEPTION cx_oref.
      WHEN 6.
        "Eexception object created inline
        RAISE EXCEPTION NEW zcx_demo_abap_error_a( textid  = zcx_demo_abap_error_a=>error_004
                                                   p_004_a = `f`
                                                   p_004_b = `g` ).

      WHEN 7.
        "MESSAGE addition
        "For passing message specification to an exception object
        "Note that EXPORTING can also be specified
        "After MESSAGE, a message type is specified (which is not of relevance in ABAP for Cloud
        "Development). These types can be the following: A, E, I, S, W, or X. See the ABAP Keyword
        "Documentation for information.For example, E representing an error message.
        "The character is followed by the message number. The message class name is directly
        "specified within a pair of parentheses.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_a MESSAGE e002(zdemo_abap_messages).

      WHEN 8.
        "Specifying a message number that does not exist
        "The specified message type, message class, and message number are
        "used as short text in uppercase letters and separated by a colon
        RAISE EXCEPTION TYPE zcx_demo_abap_error_a MESSAGE e999(zdemo_abap_messages).
      WHEN 9.
        "MESSAGE, ID, TYPE, NUMBER additions
        "The message class, the message type, and the message number are specified.
        "Here, literals are specified. The example is an alternative to the one above.

        "addition WITH not supported when interface not implemented
        RAISE EXCEPTION TYPE zcx_demo_abap_error_a
          MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
          TYPE 'E'
          NUMBER '002'.

      WHEN 10.
        "Specifying the values using data objects, not as literals as above.
        "Note that uppercase letters are expected.
        DATA(msgid) = 'ZDEMO_ABAP_MESSAGES'.
        DATA(msgty) = 'E'.
        DATA(msgnum) = '002'.

        RAISE EXCEPTION TYPE zcx_demo_abap_error_a MESSAGE ID msgid TYPE msgty NUMBER msgnum.

      WHEN 11.
        "Intentionally using lowercase letters for demonstration purposes
        msgid = to_lower( 'ZDEMO_ABAP_MESSAGES'  ).
        msgty = 'E'.
        msgnum = '002'.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_a MESSAGE ID msgid TYPE msgty NUMBER msgnum.

      WHEN 12.
        "Using COND operator with various additions
        DATA(cond_w_throw_1) = COND #( WHEN flag IS INITIAL THEN `works`
                                       ELSE THROW zcx_demo_abap_error_a( ) ).

      WHEN 13.
        DATA(cond_w_throw_2) = COND #( WHEN flag IS INITIAL THEN `works`
                                       ELSE THROW zcx_demo_abap_error_a( textid  = zcx_demo_abap_error_a=>error_004
                                                                         p_004_a = `cond_a`
                                                                         p_004_b = `cond_b` )  ).
      WHEN 14.
        DATA(cond_w_throw_3) = COND #( WHEN flag IS INITIAL THEN `works`
                                       ELSE THROW zcx_demo_abap_error_a( MESSAGE e005(zdemo_abap_messages)
                                                                         p_005_a = `An exception raised with COND` ) ).
      WHEN 15.
        DATA(cond_w_throw_4) =  COND #( WHEN flag IS INITIAL THEN `works`
                                        ELSE THROW zcx_demo_abap_error_a( MESSAGE ID 'ZDEMO_ABAP_MESSAGES' TYPE 'E' NUMBER '002' ) ).

      WHEN 16.
        DATA(cond_w_throw_5) =  COND #( WHEN flag IS INITIAL THEN `works`
                                        ELSE THROW zcx_demo_abap_error_a( MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
                                                                          TYPE 'E'
                                                                          NUMBER '005'
                                                                          p_005_a = `An exception raised with COND`
                                                                          p_005_b = `additions MESSAGE ID/TYPE/NUMBER` ) ).

      WHEN 17.
        "Using SWITCH operator with various additions
        DATA(switch_w_throw_1) = SWITCH #( flag WHEN '' THEN `works`
                                           ELSE THROW zcx_demo_abap_error_a( ) ).
      WHEN 18.

        DATA(switch_w_throw_2) = SWITCH #( flag WHEN '' THEN `works`
                                           ELSE THROW zcx_demo_abap_error_a( textid  = zcx_demo_abap_error_a=>error_004
                                                                             p_004_a = `switch_a`
                                                                             p_004_b = `switch_b` )  ).
      WHEN 19.
        DATA(switch_w_throw_4) = SWITCH #( flag WHEN '' THEN `works`
                                           ELSE THROW zcx_demo_abap_error_a( MESSAGE e005(zdemo_abap_messages)
                                                                             p_005_a = `An exception raised with SWITCH` )  ).


      WHEN 20.

        DATA(switch_w_throw_3) = SWITCH #( flag WHEN '' THEN `works`
                                           ELSE THROW zcx_demo_abap_error_a( MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
                                                                             TYPE 'E'
                                                                             NUMBER '005'
                                                                             p_005_a = `An exception raised with SWITCH`
                                                                             p_005_b  = `additions MESSAGE ID/TYPE/NUMBER` ) ).

      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.

  METHOD meth_b.
    "Note: A selection of additions is covered.

    DATA(flag) = 'X'.

    CASE num.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_b.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_b MESSAGE e002(zdemo_abap_messages).
      WHEN 3.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_b
          MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
          TYPE 'E'
          NUMBER '002'.
      WHEN 4.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_b MESSAGE e005(zdemo_abap_messages) WITH 'Hello' 'world'.
      WHEN 5.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_b MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
                                                   TYPE 'E'
                                                   NUMBER '004'
                                                   WITH 'abc' 'def' 'ghi' 'jkl'.
      WHEN 6.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_b MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
                                                   TYPE 'E'
                                                   NUMBER '005'
                                                   WITH 'Message raised at/by'
                                                        cl_abap_context_info=>get_system_date( )
                                                        cl_abap_context_info=>get_system_time( )
                                                        cl_abap_context_info=>get_user_alias( ).

      WHEN 7.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_b MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
                                                   TYPE 'E'
                                                   NUMBER '005'
                                                   WITH 'only two out of four' 'parameters specified'.
      WHEN 8.

        MESSAGE e002(zdemo_abap_messages) INTO DATA(msg).
        RAISE EXCEPTION TYPE zcx_demo_abap_error_b USING MESSAGE.

      WHEN 9.

        MESSAGE e005(zdemo_abap_messages) WITH 'a' 'b' 'c' 'd' INTO msg.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_b USING MESSAGE.

      WHEN 10.

        DATA: mid     TYPE sy-msgid VALUE 'ZDEMO_ABAP_MESSAGES',
              mtype   TYPE sy-msgty VALUE 'E',
              msg_num TYPE sy-msgno VALUE '002'.

        MESSAGE ID mid TYPE mtype NUMBER msg_num INTO msg.
        RAISE EXCEPTION TYPE zcx_demo_abap_error_b USING MESSAGE.

      WHEN 11.
        "The following statements have the same effect as USING MESSAGE.
        MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
            TYPE 'E'
            NUMBER '004'
            WITH 'mno' 'pqr' 'stu' 'vwx'
            INTO msg.

        "Explictly passing the sy values
        RAISE EXCEPTION TYPE zcx_demo_abap_error_b MESSAGE ID sy-msgid
                                                   TYPE   sy-msgty
                                                   NUMBER sy-msgno
                                                   WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      WHEN 12.
        DATA(cond_w_throw_1) = COND #( WHEN flag IS INITIAL THEN `works`
                                       ELSE THROW zcx_demo_abap_error_b( MESSAGE e005(zdemo_abap_messages)
                                                                         WITH `An exception raised with COND,` `MESSAGE/WITH additions` ) ).

      WHEN 13.
        MESSAGE e005(zdemo_abap_messages) WITH 'Exception raised with COND,' 'USING MESSAGE addition' INTO msg.

        DATA(cond_w_throw_2) = COND #( WHEN flag IS INITIAL THEN `works`
                                       ELSE THROW zcx_demo_abap_error_b( USING MESSAGE ) ).

      WHEN 14.
        DATA(switch_w_throw_1) = SWITCH #( flag WHEN '' THEN `works`
                                           ELSE THROW zcx_demo_abap_error_b( MESSAGE e005(zdemo_abap_messages)
                                                                             WITH `An exception raised with SWITCH,` `MESSAGE/WITH additions` )  ).

      WHEN 15.
        MESSAGE e005(zdemo_abap_messages) WITH 'Exception raised with SWITCH,' 'USING MESSAGE addition' INTO msg.

        DATA(switch_w_throw_2) = SWITCH #( flag WHEN '' THEN `works`
                                           ELSE THROW zcx_demo_abap_error_b( USING MESSAGE ) ).

      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.

  METHOD divide.
    div_result = num1 / num2.
  ENDMETHOD.

  METHOD get_uuid.
    uuid = cl_system_uuid=>create_uuid_x16_static( ).
  ENDMETHOD.

ENDCLASS.
