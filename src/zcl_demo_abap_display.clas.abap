***********************************************************************
*
*   Class for ABAP cheat sheet examples designed to support displaying
*   output in the ADT console
*
* -------------------------- NOTE -------------------------------------
* This helper class is only used to display complex types contained in
* the example classes of the ABAP cheat sheets in older ABAP releases. 
* In newer ABAP releases, this helper class is, in principle, not needed.
* You can use the write method of the classrun interface directly and
* display all types.
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
"! <p class="shorttext synchronized">Class supporting ABAP cheat sheet examples</p>
"! The class supports displaying output of the ABAP cheat sheet examples in the ADT console.
CLASS zcl_demo_abap_display DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_out TYPE REF TO if_oo_adt_classrun_out,
      display
        IMPORTING
          input         TYPE data
          name          TYPE string DEFAULT ``
        RETURNING
          VALUE(output) TYPE string,
      next_section
        IMPORTING
          heading TYPE string.

protected section.
  PRIVATE SECTION.
    DATA:
      mo_out TYPE REF TO if_oo_adt_classrun_out,
      offset TYPE i.

ENDCLASS.



CLASS ZCL_DEMO_ABAP_DISPLAY IMPLEMENTATION.

  METHOD constructor.
    mo_out = io_out.
  ENDMETHOD.

  METHOD display.
    "Checking data type
    DATA(type_descr) = cl_abap_typedescr=>describe_by_data( input ).
    CASE type_descr->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        DATA(struct_descr) = CAST cl_abap_structdescr( type_descr ).
        "Checking for complex output
        IF struct_descr->struct_kind = cl_abap_structdescr=>structkind_nested
        OR line_exists( struct_descr->components[ type_kind = cl_abap_typedescr=>typekind_table ] )
        OR line_exists( struct_descr->components[ type_kind = cl_abap_typedescr=>typekind_dref ] )
        OR line_exists( struct_descr->components[ type_kind = cl_abap_typedescr=>typekind_oref ] ).
          DATA(to_be_serialized) = abap_true.
        ELSE.
          DATA(display) = mo_out->get( data = input name = name ).
        ENDIF.
      WHEN cl_abap_typedescr=>kind_table.
        DATA(table_descr) = CAST cl_abap_tabledescr( type_descr ).
        TRY.
            DATA(line_type_struct_descr) = CAST cl_abap_structdescr( table_descr->get_table_line_type( ) ).
            "Checking for complex output
            IF line_type_struct_descr->struct_kind = cl_abap_structdescr=>structkind_nested
            OR line_exists( line_type_struct_descr->components[ type_kind = cl_abap_typedescr=>typekind_table ] )
            OR line_exists( line_type_struct_descr->components[ type_kind = cl_abap_typedescr=>typekind_dref ] )
            OR line_exists( line_type_struct_descr->components[ type_kind = cl_abap_typedescr=>typekind_oref ] ).
              to_be_serialized = abap_true.
            ELSE.
              display = mo_out->get( data = input name = name ).
            ENDIF.
          CATCH cx_sy_move_cast_error.
            to_be_serialized = abap_true.
        ENDTRY.
      WHEN cl_abap_typedescr=>kind_class.
        to_be_serialized = abap_true.
      WHEN cl_abap_typedescr=>kind_intf.
        to_be_serialized = abap_true.
      WHEN cl_abap_typedescr=>kind_elem.
        display = mo_out->get( data = COND string( WHEN name IS INITIAL THEN input ELSE `"` && name && `":` && cl_abap_char_utilities=>newline && input ) ).
      WHEN cl_abap_typedescr=>kind_ref.
        "Checking for data references
        IF type_descr->type_kind = cl_abap_typedescr=>typekind_dref.
          "Checking type of dereferenced data object
          DATA(type_check_dref) = cl_abap_typedescr=>describe_by_data( input->* ).
          "Processing (non-)elementary types
          IF type_check_dref->kind = type_descr->kind_elem.
            display = mo_out->get( data = COND string( WHEN name IS INITIAL THEN input->* ELSE `"` && name && `":` && cl_abap_char_utilities=>newline && input->* ) ).
          ELSE.
            to_be_serialized = abap_true.
          ENDIF.
        ELSE.
          to_be_serialized = abap_true.
        ENDIF.
    ENDCASE.

    "Processing complex output by serializiation
    FIND SUBSTRING `Data type not yet supported ...` IN display MATCH OFFSET DATA(off) MATCH LENGTH DATA(len).
    IF sy-subrc = 0 OR to_be_serialized = abap_true.
      "ABAP JSON serializing
      DATA(json) = /ui2/cl_json=>serialize( data             = input
                                            pretty_name      = /ui2/cl_json=>pretty_mode-low_case
                                            compress         = abap_false
                                            hex_as_base64    = abap_false
                                            format_output    = abap_true
                                            assoc_arrays     = abap_true
                                            assoc_arrays_opt = abap_true ).
      IF to_be_serialized = abap_true.
        IF name IS INITIAL.
          REPLACE PCRE `^` IN display WITH json && cl_abap_char_utilities=>newline.
        ELSE.
          REPLACE PCRE `^` IN display WITH `"` && name && `":` && cl_abap_char_utilities=>newline && json && cl_abap_char_utilities=>newline.
        ENDIF.
      "substring found
      ELSE.
        IF name IS INITIAL.
          REPLACE SECTION OFFSET off LENGTH len OF display WITH json && cl_abap_char_utilities=>newline.
        ELSE.
          REPLACE SECTION OFFSET off LENGTH len OF display WITH `"` && name && `":` && cl_abap_char_utilities=>newline && json && cl_abap_char_utilities=>newline.
        ENDIF.
      ENDIF.
      mo_out->write( display ).
    ELSE.
      mo_out->write( display ).
    ENDIF.
  ENDMETHOD.

  METHOD next_section.
    mo_out->write( `_________________________________________________________________________________`
                && cl_abap_char_utilities=>newline
                && cl_abap_char_utilities=>newline
                && heading
                && cl_abap_char_utilities=>newline ).
  ENDMETHOD.
ENDCLASS.