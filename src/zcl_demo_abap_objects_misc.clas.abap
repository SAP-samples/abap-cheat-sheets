"! <p class="shorttext"><strong>ABAP object orientation (2)</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates syntax and concepts related to ABAP object orientation. It adds
"! to class {@link zcl_demo_abap_objects}, and includes additional syntax examples.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <p>Find information on <strong>getting started with the example class</strong> and the
"! <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
CLASS zcl_demo_abap_objects_misc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES zdemo_abap_objects_interface.
    METHODS constructor IMPORTING text TYPE string OPTIONAL.
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "------------ Demonstrating constructors ------------
    DATA instance_timestamp TYPE utclong.
    CLASS-DATA static_timestamp TYPE utclong.
    DATA instance_name TYPE string.
    CLASS-DATA stat_constr_call_count TYPE i.
    CLASS-DATA instance_constr_call_count TYPE i.
    "----------------------------------------------------

    "------------ Demonstrating optional parameters ------------
    METHODS meth_opt_1 IMPORTING num        TYPE i OPTIONAL
                       RETURNING VALUE(str) TYPE string.
    METHODS meth_opt_2 IMPORTING num        TYPE i DEFAULT 1
                       RETURNING VALUE(str) TYPE string.
    METHODS meth_opt_3 IMPORTING num1       TYPE i
                                 num2       TYPE i OPTIONAL
                                 num3       TYPE i DEFAULT 1
                       RETURNING VALUE(str) TYPE string.
    "-----------------------------------------------------------

    "------------ Demonstrating preferred parameters ------------
    METHODS meth_pref
      IMPORTING num1        TYPE i OPTIONAL
                num2        TYPE i OPTIONAL
                num3        TYPE i DEFAULT 1
                  PREFERRED PARAMETER num1
      RETURNING VALUE(text) TYPE string.
    "-----------------------------------------------------------

    "--- Demonstrating the excursion inline declarations, returning parameters ---
    CLASS-METHODS meth1 IMPORTING i_str        TYPE string
                                  i_tab        TYPE string_table OPTIONAL
                        EXPORTING e_dec        TYPE decfloat34
                                  e_tab        TYPE string_table
                        RETURNING VALUE(r_int) TYPE i.
    CLASS-METHODS meth2 RETURNING VALUE(r_tab) TYPE string_table.
    "-----------------------------------------------------------------------------

    "------------ Demonstrating self-reference me ------------
    DATA str TYPE string.
    METHODS meth RETURNING VALUE(text) TYPE string.
    "-----------------------------------------------------------

    "------------ Demonstrating method chaining ------------
    METHODS add_text IMPORTING str        TYPE string
                     RETURNING VALUE(ref) TYPE REF TO zcl_demo_abap_objects_misc.
    METHODS add_space RETURNING VALUE(ref) TYPE REF TO zcl_demo_abap_objects_misc.
    METHODS add_period RETURNING VALUE(ref) TYPE REF TO zcl_demo_abap_objects_misc.
    METHODS return_text RETURNING VALUE(str) TYPE string.
    METHODS display_text IMPORTING cl_run_ref TYPE REF TO if_oo_adt_classrun_out.
    DATA text TYPE string.
    "-------------------------------------------------------

    "------------ Demonstrating interface ------------
    ALIASES res FOR zdemo_abap_objects_interface~add_result.
    ALIASES add FOR zdemo_abap_objects_interface~addition.
    ALIASES subtr FOR zdemo_abap_objects_interface~subtraction.
    "-------------------------------------------------

    "------------ Demonstrating friendship ------------
    TYPES str4friend TYPE string.
    CLASS-METHODS get_hello RETURNING VALUE(hello) TYPE str4friend.
    "--------------------------------------------------

    "------------ Demonstrating formal parameters ------------
    "Local types and data objects used in the example
    TYPES c3 TYPE c LENGTH 3.
    TYPES der_type TYPE TABLE FOR CREATE zdemo_abap_rap_ro_m.
    DATA int TYPE i.
    DATA itab TYPE TABLE OF zdemo_abap_fli_ve WITH EMPTY KEY.

    "Various syntax options for completely typing formal parameters
    "Note: The example parameters are all specified for passing
    "actual parameters by reference.
    METHODS formal_params_compl_types IMPORTING
                                        "---- Non-generic built-in ABAP types ----
                                        i_a TYPE i
                                        i_b TYPE string
                                        "---- ABAP DDIC types ----
                                        i_c TYPE land1           "elementary type
                                        i_d TYPE timestampl      "elementary type
                                        i_e TYPE zdemo_abap_fli "structured type based on DDIC database table
                                        i_f TYPE string_hashed_table "table type
                                        "---- ABAP CDS types (all of the examples are structured types) ----
                                        i_g TYPE zdemo_abap_fli_ve "CDS view entity
                                        i_h TYPE zdemo_abap_abstract_ent "CDS abstract entity
                                        i_i TYPE zdemo_abap_table_function "CDS table function
                                        "---- Data types declared in public section of a class ----
                                        i_j TYPE zcl_demo_abap_dtype_dobj=>t_pub_text_c30 "elementary type
                                        i_k TYPE zcl_demo_abap_amdp=>carr_fli_struc "structured type
                                        i_l TYPE zcl_demo_abap_amdp=>carr_fli_tab "table type
                                        "---- Data types declared in an interface ----
                                        i_m TYPE zdemo_abap_get_data_itf=>occ_rate "elementary type
                                        i_n TYPE zdemo_abap_get_data_itf=>carr_tab "table type
                                        "---- Local types ----
                                        i_o TYPE c3 "elementary type
                                        i_p TYPE der_type "table type (BDEF derived type)
                                        "---- Note: Examples such as the following are not allowed type specifications of formal parameters. ----
                                        "---- In the following cases, extra (local) type declarations with TYPES are required before the --------
                                        "---- method declaration to type the formal parameters. -------------------------------------------------
                                        "i_no1 TYPE c LENGTH 3
                                        "i_no2 TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY
                                        "---- Reference types ----
                                        i_q TYPE REF TO i "Data reference
                                        i_r TYPE REF TO zdemo_abap_carr "Data reference
                                        i_s TYPE REF TO zcl_demo_abap_unit_test "Object reference
                                        i_t TYPE REF TO data "Data reference (considered as complete typing, too)
                                        i_u TYPE REF TO object "Object reference (considered as complete typing, too)
                                        "---- TYPE LINE OF addition (structured type based on a table type) ----
                                        i_v TYPE LINE OF zcl_demo_abap_amdp=>carr_fli_tab
                                        i_w TYPE LINE OF der_type
                                        "---- LIKE addition (types based on existing data objects) ----
                                        i_x LIKE int "Local data object
                                        i_y LIKE zcl_demo_abap_dtype_dobj=>comma "Constant specified in a class
                                        i_z LIKE zdemo_abap_objects_interface=>stat_str "Data object specified in an interface
                                        "---- LIKE LINE OF addition (types based on existing internal tables) ----
                                        i_1 LIKE LINE OF itab "Local internal table
                                        "---- LIKE REF TO addition (reference types based on existing data object) ----
                                        i_2 LIKE REF TO int "Local elementary data object
                                        i_3 LIKE REF TO itab "Local internal table
                                      .

    METHODS formal_params_generic_types IMPORTING
                                          "---- Any data type ----
                                          i_data           TYPE data
                                          i_any            TYPE any

                                          "---- Character-like types ----
                                          i_c              TYPE c         "Text field with a generic length
                                          i_clike          TYPE clike     "Character-like (c, n, string, d, t, and character-like flat structures)
                                          i_csequence      TYPE csequence "Text-like (c, string)
                                          i_n              TYPE n         "Numeric text with generic length
                                          i_x              TYPE x         "Byte field with generic length
                                          i_xsequence      TYPE xsequence "Byte-like (x, xstring)

                                          "---- Numeric types ----
                                          i_decfloat       TYPE decfloat "decfloat16 decfloat34
                                          i_numeric        TYPE numeric  "Numeric (i, int8, p, decfloat16, decfloat34, f, (b, s))
                                          i_p              TYPE p        "Packed number (generic length and number of decimal places)

                                          "---- Internal table types ----
                                          i_any_table      TYPE ANY TABLE      "Internal table with any table type
                                          i_hashed_table   TYPE HASHED TABLE
                                          i_index_table    TYPE INDEX TABLE
                                          i_sorted_table   TYPE SORTED TABLE
                                          i_standard_table TYPE STANDARD TABLE
                                          i_table          TYPE table          "Standard table

                                          "---- Other types ----
                                          i_simple         TYPE simple "Elementary data type including enumerated types and
                                          "structured types with exclusively character-like flat components
                                        .
    "-----------------------------------------------------------


ENDCLASS.



CLASS zcl_demo_abap_objects_misc IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP cheat sheet example: ABAP Object Orientation (2)\n\n| ).
    out->write( |1) Complete Typing of Formal Parameters\n\n| ).

    out->write( `No output for this section. See the signature of the formal_params_compl_types method, `
    && `which demonstrates multiple syntax variants for typing formal parameters completely.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Generic Typing of Formal Parameters` ) ).

    "Structure including various components of specific types
    "They represent actual parameters in the method call below
    DATA: BEGIN OF s,
            c3        TYPE c LENGTH 3,
            c10       TYPE c LENGTH 10,
            n4        TYPE n LENGTH 4,
            str       TYPE string,
            time      TYPE t,
            date      TYPE d,
            dec16     TYPE decfloat16,
            dec34     TYPE decfloat34,
            int       TYPE i,
            pl4d2     TYPE p LENGTH 4 DECIMALS 2,
            tab_std   TYPE STANDARD TABLE OF string WITH EMPTY KEY,
            tab_so    TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line,
            tab_ha    TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line,
            xl1       TYPE x LENGTH 1,
            xstr      TYPE xstring,
            structure TYPE zdemo_abap_carr, "character-like flat structure
          END OF s.

    "The following method call specifies various actual parameters for the
    "generic formal parameters.
    "Note the comments for allowed and not allowed example assignments of
    "actual parameters.
    formal_params_generic_types(
      "------------- Any data type -------------
      "--- data/any: Allowed (examples) ---
      i_data = s-c3
      "i_data = s-time
      "i_data = s-tab_std
      "i_data = s-xstr

      i_any = s-c3
      "i_any = s-time
      "i_any = s-tab_std
      "i_any = s-xstr

      "------------- Character-like types -------------
      "--- c: Allowed (examples) ---
      i_c = s-c3
      "i_c = s-c10
      "--- c: Not allowed (examples) ---
      "i_c = s-str
      "i_c = s-n4

      "--- clike: Allowed (examples) ---
      i_clike = s-c3
      "i_clike = s-c10
      "i_clike = s-str
      "i_clike = s-structure
      "i_clike = s-time
      "i_clike = s-date
      "i_clike = s-n4
      "--- clike: Not allowed (examples) ---
      "i_clike = s-xstr
      "i_clike = s-xl1
      "i_clike = s-pl4d2

      "--- csequence: Allowed (examples) ---
      i_csequence  = s-c3
      "i_csequence  = s-c10
      "i_csequence  = s-str
      "--- csequence: Not allowed (examples) ---
      "i_csequence  = s-time
      "i_csequence  = s-date
      "i_csequence  = s-structure

      "--- n: Allowed ---
      i_n = s-n4
      "--- n: Not allowed (examples) ---
      "i_n = s-c3
      "i_n = s-int

      "--- x: Allowed ---
      i_x = s-xl1
      "--- x: Not allowed (examples) ---
      "i_x = s-xstr
      "i_x = s-c3

      "--- xsequence: Allowed ---
      i_xsequence = s-xstr
      "i_xsequence = s-xl1
      "--- xsequence: Not allowed (examples) ---
      "i_xsequence = s-c3
      "i_xsequence = s-str

      "--- decfloat: Allowed ---
      i_decfloat = s-dec16
      "i_decfloat = s-dec34
      "--- decfloat: Not allowed (examples) ---
      "i_decfloat = s-int
      "i_decfloat = s-pl4d2

      "--- numeric: Allowed (examples) ---
      i_numeric  = s-int
      "i_numeric = s-dec16
      "i_numeric = s-dec34
      "i_numeric = s-pl4d2
      "--- numeric: Not allowed (examples) ---
      "i_numeric = s-n4
      "i_numeric = s-date

      "--- p: Allowed ---
      i_p = s-pl4d2
      "--- p: Not allowed (examples) ---
      "i_p = s-dec16
      "i_p = s-dec34

      "--- any table: Allowed ---
      i_any_table = s-tab_std
      "i_any_table = s-tab_ha
      "i_any_table = s-tab_so
      "--- any table: Not allowed (examples) ---
      "i_any_table = s-structure
      "i_any_table = s-c3

      "--- hashed table: Allowed ---
      i_hashed_table = s-tab_ha
      "--- hashed table: Not allowed ---
      "i_hashed_table = s-tab_std
      "i_hashed_table = s-tab_so

      "--- index table: Allowed ---
      i_index_table = s-tab_std
      "i_index_table = s-tab_so
      "--- index table: Not allowed ---
      "i_index_table = s-tab_ha

      "--- sorted table: Allowed ---
      i_sorted_table = s-tab_so
      "--- sorted table: Not allowed ---
      "i_sorted_table = s-tab_std
      "i_sorted_table = s-tab_ha

      "--- standard table/table: Allowed ---
      i_standard_table = s-tab_std
      i_table = s-tab_std
      "--- standard table/table: Not allowed ---
      "i_standard_table = s-tab_so
      "i_standard_table = s-tab_ha
      "i_table = s-tab_so
      "i_table = s-tab_ha

     "--- simple: Allowed (examples) ---
      i_simple = s-structure
      "i_simple = s-c3
      "i_simple = s-n4
      "i_simple = s-int
      "i_simple = s-pl4d2
      "i_simple = s-xstr
      "i_simple = s-str
      "--- simple: Not allowed (examples) ---
      "i_simple = s-tab_ha
      "i_simple = s-tab_so

       ).

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Defining Parameters as Optional` ) ).

    DATA(meth_opt_1_result_a) = meth_opt_1( ).
    DATA(meth_opt_1_result_b) = meth_opt_1( 2 ).

    DATA(meth_opt_2_result_a) = meth_opt_2( ).
    DATA(meth_opt_2_result_b) = meth_opt_2( 3 ).

    "The commented out statement is not possible as there is one
    "non-optional parameter.
    "DATA(meth_opt_3_result_a) = meth_opt_3( ).
    DATA(meth_opt_3_result_b) = meth_opt_3( 4 ).
    DATA(meth_opt_3_result_c) = meth_opt_3( num1 = 5 num2 = 6 ).
    DATA(meth_opt_3_result_d) = meth_opt_3( num1 = 7 num3 = 8 ).

    out->write( data = meth_opt_1_result_a name = `meth_opt_1_result_a` ).
    out->write( |\n| ).
    out->write( data = meth_opt_1_result_b name = `meth_opt_1_result_b` ).
    out->write( |\n| ).
    out->write( data = meth_opt_2_result_a name = `meth_opt_2_result_a` ).
    out->write( |\n| ).
    out->write( data = meth_opt_2_result_b name = `meth_opt_2_result_b` ).
    out->write( |\n| ).
    out->write( data = meth_opt_3_result_b name = `meth_opt_3_result_b` ).
    out->write( |\n| ).
    out->write( data = meth_opt_3_result_c name = `meth_opt_3_result_c` ).
    out->write( |\n| ).
    out->write( data = meth_opt_3_result_d name = `meth_opt_3_result_d` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Defining Input Parameters as Preferred` ) ).

    DATA(text1) = meth_pref( num1 = 3 num2 = 3 num3 = 3 ).
    out->write( text1 ).

    DATA(text2) = meth_pref( num1 = 3 num2 = 3 ).
    out->write( text2 ).

    DATA(text3) = meth_pref( num2 = 3 num3 = 3 ).
    out->write( text3 ).

    DATA(text4) = meth_pref( num1 = 3 num3 = 3 ).
    out->write( text4 ).

    DATA(text5) = meth_pref( num2 = 3 ).
    out->write( text5 ).

    DATA(text6) = meth_pref( num3 = 3 ).
    out->write( text6 ).

    DATA(text7) = meth_pref( ).
    out->write( text7 ).

    "Not specifying the name of the formal parameter. The
    "actual parameter is assigned to the preferred input
    "parameter.
    DATA(text8) = meth_pref( 3 ).
    out->write( text8 ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Constructors` ) ).

    "Notes:
    "- The static constructor is called only once, even when multiple class instances are created,
    "  leading to a constant static_timestamp value and stat_constr_call_count value, which remains 1.
    "- The instance_timestamp attribute shows different timestamps for each created instance.
    "- The static attribute instance_constr_call_count increases with each instance. Note that running
    "  the class with F9 in ADT also calls the instance and static constructors. Thus, the final
    "  instance_constr_call_count totals the number of DO loop passes plus 1, starting with 2 for inst1
    "  instead of 1.

    DATA itab_constr TYPE string_table.

    DO 5 TIMES.
      DATA(inst) = NEW zcl_demo_abap_objects_misc( |inst{ sy-index }| ).
      APPEND |-------------- Instance "{ inst->instance_name }" --------------| TO itab_constr.
      APPEND |instance_timestamp: { inst->instance_timestamp }| TO itab_constr.
      APPEND |static_timestamp: { inst->static_timestamp }| TO itab_constr.
      APPEND |instance_constr_call_count: { inst->instance_constr_call_count }| TO itab_constr.
      APPEND |stat_constr_call_count: { inst->stat_constr_call_count }| TO itab_constr.
      APPEND INITIAL LINE TO itab_constr.
    ENDDO.

    out->write( data = itab_constr name = `itab_constr` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) Excursion: Inline Declarations, Returning Parameters` ) ).
    "Note:
    "- Calling the method in the same class means specifying 'zcl_some_class=>' is optional here.
    "- There is no proper implementation in the method implementations.
    "- There is not output for this section as it is meant to visualize on syntax options.

    "Standalone method call
    "Specifying target data objects for all output parameters
    "Inline declarations are handy because you can create an
    "appropriately typed data object in place. No need to
    "create an extra variable, check on the type etc.
    zcl_demo_abap_objects_misc=>meth1(
      EXPORTING
        i_str = `ABAP`
      IMPORTING
        e_dec = DATA(a)
        e_tab = DATA(b)
      RECEIVING
        r_int = DATA(c)
    ).

    "Functional method call
    "The target data object of the returning parameter is specified on the left side of an assignment.
    "Note: In this case, you cannot specify inline declarations for the exporting parameters.
    DATA e TYPE decfloat34.
    DATA f TYPE string_table.
    DATA(g) = zcl_demo_abap_objects_misc=>meth1(
      EXPORTING
        i_str = `ABAP`
      IMPORTING
        "e_dec = DATA(h)
        "e_tab = DATA(i)
        e_dec = e
        e_tab = f
    ).

    "Benefits of returning parameters: They can, for example, be used in expressions
    "The following snippets show a selection (and ignore the available exporting
    "parameters).

    CASE zcl_demo_abap_objects_misc=>meth1( i_str = `ABAP` ).
      WHEN 0. ...
      WHEN 1. ...
      WHEN OTHERS. ...
    ENDCASE.

    IF zcl_demo_abap_objects_misc=>meth1( i_str = `ABAP` ) > 5.
      ...
    ELSE.
      ...
    ENDIF.

    "IF used with a predicative method call
    "The result of the relational expression is true if the result of the functional
    "method call is not initial and false if it is initial. The data type of the result
    "of the functional meth1od call, i. e. the return value of the called functional method,
    "is arbitrary. A check is made for the type-dependent initial value.
    IF zcl_demo_abap_objects_misc=>meth1( i_str = `ABAP` ).
      ...
    ELSE.
      ...
    ENDIF.

    DO zcl_demo_abap_objects_misc=>meth1( i_str = `ABAP` ) TIMES.
      ...
    ENDDO.

    "Method call result as actual parameter
    DATA(j) = zcl_demo_abap_objects_misc=>meth1( i_str = `ABAP` i_tab = zcl_demo_abap_objects_misc=>meth2( ) ).

    "Examples of returning parameters typed with a table type
    LOOP AT zcl_demo_abap_objects_misc=>meth2( ) INTO DATA(wa1).
      ...
    ENDLOOP.

    READ TABLE zcl_demo_abap_objects_misc=>meth2( ) INTO DATA(wa2) INDEX 1.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) Self-Reference me` ) ).

    str = `AP`.
    DATA(text) = meth( ).
    out->write( data = text name = `text` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Method Chaining and Chained Attribute Access` ) ).

    DATA(txt1) = NEW zcl_demo_abap_objects_misc(
                         )->add_text( `Hallo`
                         )->add_space(
                         )->add_text( xco_cp=>sy->user( )->name
                         )->add_period(
                         )->add_space(
                         )->add_text( `This`
                         )->add_space(
                         )->add_text( `is`
                         )->add_space(
                         )->add_text( `an`
                         )->add_space(
                         )->add_text( `example`
                         )->add_space(
                         )->add_text( `of`
                         )->add_space(
                         )->add_text( `method`
                         )->add_space(
                         )->add_text( `chaining`
                         )->add_period(
                         )->return_text( ).

    out->write( data = txt1 name = `txt1` ).
    out->write( |\n| ).

    "The following example chained method call includes a chained attribute
    "access at the end so that the target variable contains the content of
    "the attribute.

    "Example result: Today is 2025-03-05. It's 14:30:38. Have a nice day.
    DATA(txt2) = NEW zcl_demo_abap_objects_misc(
                      )->add_text( `Today`
                      )->add_space(
                      )->add_text( `is`
                      )->add_space(
                      )->add_text( xco_cp=>sy->date( )->as( xco_cp_time=>format->iso_8601_extended )->value
                      )->add_period(
                      )->add_space(
                      )->add_text( `It's`
                      )->add_space(
                      )->add_text( xco_cp=>sy->time( xco_cp_time=>time_zone->user
                                    )->as( xco_cp_time=>format->iso_8601_extended
                                    )->value
                      )->add_period(
                      )->add_space(
                      )->add_text( `Have`
                      )->add_space(
                      )->add_text( `a`
                      )->add_space(
                      )->add_text( `nice`
                      )->add_space(
                      )->add_text( `day`
                      )->add_period(
                      )->text.

    out->write( data = txt2 name = `txt2` ).
    out->write( |\n| ).

    "----------------------------------------------------------------
    "-------- Method chaining with a standalone statement -----------
    "----------------------------------------------------------------

    "In the example, the final method call in the chain receives
    "the classrun instance available in the implementation of the
    "if_oo_adt_classrun~main method. The method implementation
    "includes the writing to the console.

    "Console output: Lorem ipsum dolor sit amet
    NEW zcl_demo_abap_objects_misc( )->add_text( `Lorem`
                        )->add_space(
                        )->add_text( `ipsum`
                        )->add_space(
                        )->add_text( `dolor`
                        )->add_space(
                        )->add_text( `sit`
                        )->add_space(
                        )->add_text( `amet`
                        )->display_text( out ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `9) Demonstrating Upcasts and Downcasts Using the RTTS Inheritance Tree` ) ).

*Hierarchy tree of the classes:
*
*CL_ABAP_TYPEDESCR
*  |
*  |--CL_ABAP_DATADESCR
*  |   |
*  |   |--CL_ABAP_ELEMDESCR
*  |   |   |
*  |   |   |--CL_ABAP_ENUMDESCR
*  |   |
*  |   |--CL_ABAP_REFDESCR
*  |   |--CL_ABAP_COMPLEXDESCR
*  |       |
*  |       |--CL_ABAP_STRUCTDESCR
*  |       |--CL_ABAP_TABLEDESCR
*  |
*  |--CL_ABAP_OBJECTDESCR
*     |
*     |--CL_ABAP_CLASSDESCR
*     |--CL_ABAP_INTFDESCR

    "------------ Object reference variables ------------

    "Static and dynamic types
    "Defining an object reference variable with a static type
    DATA tdo TYPE REF TO cl_abap_typedescr.

    "Retrieving type information
    "The reference the reference variable points to is either cl_abap_elemdescr,
    "cl_abap_enumdescr, cl_abap_refdescr, cl_abap_structdescr, or cl_abap_tabledescr.
    "So, it points to one of the subclasses. The static type of tdo refers to
    "cl_abap_typedescr, however, the dynamic type is one of the subclasses mentioned.
    "in the case of the example, it is cl_abap_elemdescr. Check in the debugger.
    DATA some_string TYPE string.
    tdo = cl_abap_typedescr=>describe_by_data( some_string ).

    "Some more object reference variables
    DATA tdo_super TYPE REF TO cl_abap_typedescr.
    DATA tdo_elem TYPE REF TO cl_abap_elemdescr.
    DATA tdo_data TYPE REF TO cl_abap_datadescr.
    DATA tdo_gen_obj TYPE REF TO object.

    "------------ Upcasts ------------

    "Moving up the inheritance tree
    "Assignments:
    "- If the static type of target variable is less specific or the same, an assignment works.
    "- The target variable inherits the dynamic type of the source variable.

    "Static type of target variable is the same
    tdo_super = tdo.

    "Examples for static types of target variables that are less specific
    "Target variable has the generic type object
    tdo_gen_obj = tdo.

    "Target variable is less specific because the direct superclass of cl_abap_elemdescr
    "is cl_abap_datadescr
    "Note: In the following three assignments, the target variable remains initial
    "since the source variables do not (yet) point to any object.
    tdo_data = tdo_elem.

    "Target variable is less specific because the direct superclass of cl_abap_datadescr
    "is cl_abap_typedescr
    tdo_super = tdo_data.

    "Target variable is less specific because the class cl_abap_typedescr is higher up in
    "the inheritance tree than cl_abap_elemdescr
    tdo_super = tdo_elem.

    "The casting happens implicitly. You can also excplicitly cast and use
    "casting operators, but it is usually not required.
    tdo_super = CAST #( tdo ).
    tdo_super ?= tdo.

    "In combination with inline declarations, the CAST operator can be used to provide a
    "reference variable with a more general type.
    DATA(tdo_inl_cast) = CAST cl_abap_typedescr( tdo_elem ).

    CLEAR: tdo_super, tdo_elem, tdo_data, tdo_gen_obj.

    "------------ Downcasts ------------

    "Moving down the inheritance tree
    "Assignments:
    "- If the static type of the target variable is more specific than the static type
    "  of the source variable, performing a check whether it is less specific or the same
    "  as the dynamic type of the source variable is required at runtime before the assignment
    "- The target variable inherits the dynamic type of the source variable, however, the target
    "  variable can accept fewer dynamic types than the source variable
    "- Downcasts are always performed explicitly using casting operators

    "Static type of the target is more specific
    "object -> cl_abap_typedescr
    tdo_super = CAST #( tdo_gen_obj ).
    "cl_abap_typedescr -> cl_abap_datadescr
    "Note: Here, the dynamic type of the source variable is cl_abap_elemdescr.
    tdo_data = CAST #( tdo ).
    "cl_abap_datadescr -> cl_abap_elemdescr
    tdo_elem = CAST #( tdo_data ).
    "cl_abap_typedescr -> cl_abap_elemdescr
    tdo_elem = CAST #( tdo_super ).

    "------------ Error prevention in downcasts ------------

    "In the examples above, the assignments work. The following code snippets
    "deal with examples in which a downcast is not possible. An exception is
    "raised.
    DATA str_table TYPE string_table.
    DATA tdo_table TYPE REF TO cl_abap_tabledescr.

    "With the following method call, tdo points to an object with
    "reference to cl_abap_tabledescr.
    tdo = cl_abap_typedescr=>describe_by_data( str_table ).

    "Therefore, the following downcast works.
    tdo_table = CAST #( tdo ).

    "You could also achieve the same in one statement and with inline
    "declaration.
    DATA(tdo_table_2) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( str_table ) ).

    "Example for an impossible downcast
    "The generic object reference variable points to cl_abap_elemdescr after the following
    "assignment.
    tdo_gen_obj = cl_abap_typedescr=>describe_by_data( some_string ).

    "Without catching the exception, the runtime error MOVE_CAST_ERROR
    "occurs. There is no syntax error at compile time. The static type of
    "tdo_gen_obj is more generic than the static type of the target variable.
    "The error occurs when trying to downcast, and the dynamic type is used.
    TRY.
        tdo_table = CAST #( tdo_gen_obj ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.
    "Note: tdo_table sill points to the reference as assigned above after trying
    "to downcast in the TRY control structure.

    "Using CASE TYPE OF and IS INSTANCE OF statements, you can check if downcasts
    "are possible.
    "Note: In case of ...
    "- non-initial object reference variables, the dynamic type is checked.
    "- initial object reference variables, the static type is checked.

    "------------ IS INSTANCE OF ------------
    DATA some_tdo TYPE REF TO cl_abap_typedescr.
    some_tdo = cl_abap_typedescr=>describe_by_data( str_table ).

    IF some_tdo IS INSTANCE OF cl_abap_elemdescr.
      DATA(tdo_a) = CAST cl_abap_elemdescr( some_tdo ).
    ELSE.
      "This branch is executed. The downcast is not possible.
      ...
    ENDIF.

    IF some_tdo IS INSTANCE OF cl_abap_elemdescr.
      DATA(tdo_b) = CAST cl_abap_elemdescr( some_tdo ).
    ELSEIF some_tdo IS INSTANCE OF cl_abap_refdescr.
      DATA(tdo_c) = CAST cl_abap_refdescr( some_tdo ).
    ELSEIF some_tdo IS INSTANCE OF cl_abap_structdescr.
      DATA(tdo_d) = CAST cl_abap_structdescr( some_tdo ).
    ELSEIF some_tdo IS INSTANCE OF cl_abap_tabledescr.
      "In this example, this branch is executed. With the check,
      "you can make sure that the downcast is indeed possible.
      DATA(tdo_e) = CAST cl_abap_tabledescr( some_tdo ).
    ELSE.
      ...
    ENDIF.

    DATA initial_tdo TYPE REF TO cl_abap_typedescr.

    IF initial_tdo IS INSTANCE OF cl_abap_elemdescr.
      DATA(tdo_f) = CAST cl_abap_elemdescr( some_tdo ).
    ELSEIF initial_tdo IS INSTANCE OF cl_abap_refdescr.
      DATA(tdo_g) = CAST cl_abap_refdescr( some_tdo ).
    ELSEIF initial_tdo IS INSTANCE OF cl_abap_structdescr.
      DATA(tdo_h) = CAST cl_abap_structdescr( some_tdo ).
    ELSEIF initial_tdo IS INSTANCE OF cl_abap_tabledescr.
      DATA(tdo_i) = CAST cl_abap_tabledescr( some_tdo ).
    ELSE.
      "In this example, this branch is executed. The static
      "type of the initial object reference variable is used,
      "which is cl_abap_typedescr here.
      ...
    ENDIF.

    "------------ CASE TYPE OF ------------
    "The examples are desinged similarly to the IS INSTANCE OF examples.

    DATA(dref) = REF #( str_table ).
    some_tdo = cl_abap_typedescr=>describe_by_data( dref ).

    CASE TYPE OF some_tdo.
      WHEN TYPE cl_abap_elemdescr.
        DATA(tdo_j) = CAST cl_abap_elemdescr( some_tdo ).
      WHEN TYPE cl_abap_refdescr.
        "In this example, this branch is executed. With the check,
        "you can make sure that the downcast is indeed possible.
        DATA(tdo_k) = CAST cl_abap_refdescr( some_tdo ).
      WHEN TYPE cl_abap_structdescr.
        DATA(tdo_l) = CAST cl_abap_structdescr( some_tdo ).
      WHEN TYPE cl_abap_tabledescr.
        DATA(tdo_m) = CAST cl_abap_tabledescr( some_tdo ).
      WHEN OTHERS.
        ...
    ENDCASE.

    "Example with initial object reference variable
    CASE TYPE OF initial_tdo.
      WHEN TYPE cl_abap_elemdescr.
        DATA(tdo_n) = CAST cl_abap_elemdescr( some_tdo ).
      WHEN TYPE cl_abap_refdescr.
        DATA(tdo_o) = CAST cl_abap_refdescr( some_tdo ).
      WHEN TYPE cl_abap_structdescr.
        DATA(tdo_p) = CAST cl_abap_structdescr( some_tdo ).
      WHEN TYPE cl_abap_tabledescr.
        DATA(tdo_q) = CAST cl_abap_tabledescr( some_tdo ).
      WHEN OTHERS.
        "In this example, this branch is executed. The static
        "type of the initial object reference variable is used,
        "which is cl_abap_typedescr here.
        ...
    ENDCASE.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) Interface Implementation` ) ).

    "Note:
    "- The example demonstrates that the interface methods declared with DEFAULT
    "  IGNORE and DEFAULT FAIL are not required to be implemented. In this example
    "  class, the methods are not implemented. In the zcl_demo_abap_objects class,
    "  which also includes the interface, they are implemented (but do not contain
    "  code)
    "- So, if implementations are desired, you can manually add the implementations (if
    "  not automatically added using the ADT quickfix) for these methods.
    "- The following code demonstrates calls to implemented interface methods. The
    "  example also demonstrates the calling of the non-implemented methods, which
    "  can indeed be specified. Since there is no implementation in the example class,
    "  errors occur.

    "Examples using an object reference variable
    DATA(oref) = NEW zcl_demo_abap_objects_misc( ).

    oref->add( num1 = 1 num2 = 2 ).
    DATA(res1) = oref->res.

    out->write( data = res1 name = `res1` ).
    out->write( |\n| ).

    oref->subtr( num1 = 1 num2 = 2 ).
    DATA(res2) = oref->zdemo_abap_objects_interface~subtr_result.

    out->write( data = res2 name = `res2` ).
    out->write( |\n| ).

    "Referring to a type declared in the interface
    DATA char_a TYPE zdemo_abap_objects_interface~c3.
    DATA char_b TYPE zdemo_abap_objects_interface=>c3.

    "Calling non-implemented methods
    DATA(int_ig_a) = oref->zdemo_abap_objects_interface~meth_ignore( ).
    ASSERT int_ig_a = 0.

    TRY.
        DATA(int_fl_a) = oref->zdemo_abap_objects_interface~meth_fail( ).
      CATCH cx_sy_dyn_call_illegal_method INTO DATA(error).
        DATA(error_text) = error->get_text( ).
        out->write( data = error_text name = `error_text` ).
        out->write( |\n| ).
    ENDTRY.

    "Similar examples using an interface reference variable
    DATA iref TYPE REF TO zdemo_abap_objects_interface.
    iref = NEW zcl_demo_abap_objects_misc( ).

    iref->addition( num1 = 3 num2 = 5 ).
    DATA(res3) = iref->add_result.

    out->write( data = res3 name = `res3` ).
    out->write( |\n| ).

    iref->subtraction( num1 = 3 num2 = 5 ).
    DATA(res4) = iref->subtr_result.

    out->write( data = res4 name = `res4` ).
    out->write( |\n| ).

    "Referring to a type declared in the interface
    DATA char_c TYPE iref->c3.

    "Calling non-implemented methods
    DATA(int_ig_b) = iref->meth_ignore( ).
    ASSERT int_ig_b = 0.

    TRY.
        DATA(int_fl_b) = iref->meth_fail( ).
      CATCH cx_sy_dyn_call_illegal_method INTO error.
        error_text = error->get_text( ).
        out->write( data = error_text name = `error_text` ).
        out->write( |\n| ).
    ENDTRY.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) Friendship between Global and Local Classes` ) ).

    "Notes:
    "- Global class: When running the class, a method of the local class that is declared in the private
    "  section there is called. As a result of this method call, a string is assigned to an attribute
    "  that is also declared in the private section of the local class. This attribute is accessed by
    "  the global class, and finally displayed in the ADT console
    "- CCDEF include (Class-relevant Local Types tab in ADT): The LOCAL FRIENDS addition makes the local
    "  class a friend of the global class. The private components of the global class can then be accessed
    "  by the local class.
    "- CCIMP include (Local Types tab in ADT): The FRIENDS addition makes the global class a friend of the
    "  local class. The private components of the local class can then be accessed by the global class.
    "  A type declared in the private section of the global class is used to type an attribute.
    "  The method, which is also declared in the private section, includes a method call in the implementation.
    "  It is a method declared in the private section of the global class.

    local_class=>say_hello( ).
    DATA(hello) = local_class=>hello.

    out->write( data = hello name = `hello` ).

  ENDMETHOD.

  METHOD formal_params_compl_types.
    ... "No implementation
  ENDMETHOD.

  METHOD formal_params_generic_types.
    ... "No implementation
  ENDMETHOD.

  METHOD meth_opt_1.
    IF num IS SUPPLIED.
      str = |The parameter is supplied. Value: "{ num }".|.
    ELSE.
      str = |The parameter is not supplied. Initial value: "{ num }".|.
    ENDIF.
  ENDMETHOD.

  METHOD meth_opt_2.
    str = COND #( WHEN num IS SUPPLIED THEN |The parameter is supplied. Value: "{ num }".|
                     ELSE |The parameter is not supplied. Default value: "{ num }".|  ).
  ENDMETHOD.

  METHOD meth_opt_3.
    str = |num1: "{ num1 }" / |.

    str &&= |{ COND #( WHEN num2 IS SUPPLIED THEN |num2 (is supplied): "{ num2 }"|
                       ELSE |num2 (is not supplied; initial value): "{ num2 }"| ) } / |.

    str &&= |{ COND #( WHEN num3 IS SUPPLIED THEN |num3 (is supplied): "{ num3 }"|
                       ELSE |num3 (is not supplied; default value): "{ num3 }"| ) } |.
  ENDMETHOD.

  METHOD meth_pref.
    DATA(addition) = num1 + num2 + num3.
    text = |IS SUPPLIED: num1 "{ COND #( WHEN num1 IS SUPPLIED THEN 'X' ELSE '' ) }", | &&
           |num2 "{ COND #( WHEN num2 IS SUPPLIED THEN 'X' ELSE '' ) }", | &&
           |num3 "{ COND #( WHEN num3 IS SUPPLIED THEN 'X' ELSE '' ) }" / | &&
           |Addition result "{ addition }"|.
  ENDMETHOD.

  METHOD class_constructor.
    static_timestamp = utclong_current( ).
    stat_constr_call_count += 1.
  ENDMETHOD.

  METHOD constructor.
    instance_timestamp = utclong_current( ).
    instance_constr_call_count += 1.

    IF text IS SUPPLIED AND text IS NOT INITIAL.
      instance_name = text.
    ENDIF.
  ENDMETHOD.

  METHOD meth1.
    ... "No implementation
  ENDMETHOD.

  METHOD meth2.
    ... "No implementation
  ENDMETHOD.

  METHOD meth.
    "Declaring a local data object having the same
    "name as a data object declared in a visibility section
    DATA str TYPE string VALUE `AB`.

    "Addressing locally declared data object
    DATA(local_string) = str.

    "Addressing data object declared in private visibility section
    DATA(other_string) = me->str.

    text = local_string && other_string.
  ENDMETHOD.

  METHOD add_period.
    text &&= `.`.
    ref = me.
  ENDMETHOD.

  METHOD add_space.
    text &&= ` `.
    ref = me.
  ENDMETHOD.

  METHOD add_text.
    text &&= str.
    ref = me.
  ENDMETHOD.

  METHOD display_text.
    cl_run_ref->write( text ).
  ENDMETHOD.

  METHOD return_text.
    str = me->text.
  ENDMETHOD.

  METHOD add.
    res = num1 + num2.
  ENDMETHOD.

  METHOD subtr.
    zdemo_abap_objects_interface~subtr_result = num1 - num2.
  ENDMETHOD.

  METHOD zdemo_abap_objects_interface~double.
    ... "Not implemented and used in this example class.
  ENDMETHOD.

  METHOD zdemo_abap_objects_interface~halve.
    ... "Not implemented and used in this example class.
  ENDMETHOD.

  METHOD get_hello.
    hello = `Hello`.
  ENDMETHOD.

ENDCLASS.
