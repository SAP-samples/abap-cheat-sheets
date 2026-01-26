*&---------------------------------------------------------------------*
*& Abstract class
*&---------------------------------------------------------------------*
"- Serves as the common interface for all subclasses that inherit from this class.
"- Includes protected non-abstract methods ('convert_to_xstring', 'output_data')
"  that are implemented in the superclass.
"- Contains the abstract method 'parse', requiring subclasses to provide their own
"  implementation.
"- Includes the public, final template method 'template_method', which specifies
"  the sequence of method calls.

CLASS lcl_data_parser DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS template_method FINAL IMPORTING data TYPE string
                                            out  TYPE REF TO if_oo_adt_classrun_out OPTIONAL.
    DATA result TYPE string_table.
  PROTECTED SECTION.
    METHODS: parse ABSTRACT IMPORTING data          TYPE xstring
                            RETURNING VALUE(result) TYPE string_table,
      convert_to_xstring IMPORTING data        TYPE string
                         RETURNING VALUE(xstr) TYPE xstring,
      output_data IMPORTING data TYPE string_table
                            out  TYPE REF TO if_oo_adt_classrun_out OPTIONAL.
ENDCLASS.

CLASS lcl_data_parser IMPLEMENTATION.

  METHOD convert_to_xstring.
    TRY.
        xstr = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( data ).
      CATCH cx_sy_conversion_codepage.
        "If the conversion does not work, the program flow is interrupted.
        "The example does not include a proper error handling and raise a runtime error in case of a failure at this stage.
        "The example is designed for the demo input data.
        ASSERT 1 = 2.
    ENDTRY.
  ENDMETHOD.

  METHOD output_data.
    IF out IS SUPPLIED AND out IS BOUND.
      out->write( data ).
    ENDIF.
    "As an alternative to the output, the result is assigned
    "to a public instance attribute, for example, if the reference
    "variable cannot be supplied and so as to still be able to check
    "out the parsing result.
    result = data.
  ENDMETHOD.

  METHOD template_method.
    "Predefined steps in the template method:
    "- The XML and JSON data is provided as a string and converted to an xstring.
    "- The data is then parsed.
    "- The parsed data is output.

    DATA(xstr) = convert_to_xstring( data ).

    DATA(output) = parse( xstr ).

    IF out IS SUPPLIED AND out IS BOUND.
      output_data( data = output
                   out  = out ).
    ELSE.
      output_data( data = output ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete class 1 (XML parsing using iXML)
*&---------------------------------------------------------------------*

CLASS lcl_xml_parser_ixml DEFINITION INHERITING FROM lcl_data_parser.
  PROTECTED SECTION.
    METHODS parse REDEFINITION.
ENDCLASS.

CLASS lcl_xml_parser_ixml IMPLEMENTATION.
  METHOD parse.
    "For notes on the code example, see the XML and JSON cheat sheet.

    DATA(ixml_pa) = cl_ixml_core=>create( ).
    DATA(stream_factory_pa) = ixml_pa->create_stream_factory( ).
    DATA(document_pa) = ixml_pa->create_document( ).
    DATA(parser_pa) = ixml_pa->create_parser(
                        istream = stream_factory_pa->create_istream_xstring( string = data )
                        document = document_pa
                        stream_factory = stream_factory_pa ).

    DATA(parsing_check) = parser_pa->parse( ).
    IF parsing_check = 0.
      DATA(iterator_pa) = document_pa->create_iterator( ).
      DATA(node_pa) = iterator_pa->get_next( ).
      WHILE NOT node_pa IS INITIAL.
        DATA(node_type) = node_pa->get_type( ).
        CASE node_type.
          WHEN if_ixml_node=>co_node_element.
            APPEND |Element: "{ node_pa->get_name( ) }"| TO result.
            DATA(attributes_pa) = node_pa->get_attributes( ).
            IF NOT attributes_pa IS INITIAL.
              DO attributes_pa->get_length( ) TIMES.
                DATA(attr) = attributes_pa->get_item( sy-index - 1 ).
                APPEND |Attribute: "{ attr->get_name( ) } = { attr->get_value( ) }"| TO result.
              ENDDO.
            ENDIF.
          WHEN if_ixml_node=>co_node_text OR if_ixml_node=>co_node_cdata_section.
            APPEND |Text: "{ node_pa->get_value( ) }"| TO result.
            DATA(val) = to_upper( node_pa->get_value( ) ).
            node_pa->set_value( val ).
        ENDCASE.
        node_pa = iterator_pa->get_next( ).
      ENDWHILE.
    ELSE.
      "Parsing was not successful.
      "The example does not include a proper error handling.
      APPEND `--- Error ---` TO result.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete class 2 (XML parsing using sXML)
*&---------------------------------------------------------------------*

CLASS lcl_xml_parser_sxml DEFINITION INHERITING FROM lcl_data_parser.
  PROTECTED SECTION.
    METHODS parse REDEFINITION.
ENDCLASS.

CLASS lcl_xml_parser_sxml IMPLEMENTATION.
  METHOD parse.
    "For notes on the code, see the XML and JSON cheat sheet

    DATA(reader) = cl_sxml_string_reader=>create( data ).

    TRY.
        DO.
          reader->next_node( ).

          IF reader->node_type = if_sxml_node=>co_nt_final.
            EXIT.
          ENDIF.

          DATA(node_type) = SWITCH #( reader->node_type WHEN if_sxml_node=>co_nt_initial THEN `CO_NT_INITIAL`
                                                        WHEN if_sxml_node=>co_nt_element_open THEN `CO_NT_ELEMENT_OPEN`
                                                        WHEN if_sxml_node=>co_nt_element_close THEN `CO_NT_ELEMENT_CLOSE`
                                                        WHEN if_sxml_node=>co_nt_value THEN `CO_NT_VALUE`
                                                        WHEN if_sxml_node=>co_nt_attribute THEN `CO_NT_ATTRIBUTE`
                                                        ELSE `Error` ).

          DATA(name) = reader->name.
          DATA(value) = COND #( WHEN reader->node_type = if_sxml_node=>co_nt_value THEN reader->value ).
          APPEND |{ node_type }, { name }{ COND #( WHEN value IS NOT INITIAL THEN |, { value }| ) }| TO result.

          IF reader->node_type = if_sxml_node=>co_nt_element_open.
            DO.
              reader->next_attribute( ).
              IF reader->node_type <> if_sxml_node=>co_nt_attribute.
                EXIT.
              ENDIF.
              APPEND |CO_NT_ATTRIBUTE, { reader->name }{ COND #( WHEN reader->value IS NOT INITIAL THEN |, { reader->value }| ) }| TO result.
            ENDDO.
          ENDIF.
        ENDDO.
      CATCH cx_sxml_state_error INTO DATA(error).
        "The example does not include a proper error handling.
        APPEND |--- Error: { error->get_text( ) } ---| TO result.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete class 3 (JSON parsing using sXML)
*&---------------------------------------------------------------------*

"This class demonstrates the same code as lcl_xml_parser_sxml. It is
"included to add more example classes.

CLASS lcl_json_parser_sxml DEFINITION INHERITING FROM lcl_data_parser.
  PROTECTED SECTION.
    METHODS parse REDEFINITION.
ENDCLASS.

CLASS lcl_json_parser_sxml IMPLEMENTATION.
  METHOD parse.
    DATA(reader) = cl_sxml_string_reader=>create( data ).

    TRY.
        DO.
          reader->next_node( ).

          IF reader->node_type = if_sxml_node=>co_nt_final.
            EXIT.
          ENDIF.

          DATA(node_type) = SWITCH #( reader->node_type WHEN if_sxml_node=>co_nt_initial THEN `CO_NT_INITIAL`
                                                        WHEN if_sxml_node=>co_nt_element_open THEN `CO_NT_ELEMENT_OPEN`
                                                        WHEN if_sxml_node=>co_nt_element_close THEN `CO_NT_ELEMENT_CLOSE`
                                                        WHEN if_sxml_node=>co_nt_value THEN `CO_NT_VALUE`
                                                        WHEN if_sxml_node=>co_nt_attribute THEN `CO_NT_ATTRIBUTE`
                                                        ELSE `Error` ).

          DATA(name) = reader->name.
          DATA(value) = COND #( WHEN reader->node_type = if_sxml_node=>co_nt_value THEN reader->value ).
          APPEND |{ node_type }, { name }{ COND #( WHEN value IS NOT INITIAL THEN |, { value }| ) }| TO result.

          IF reader->node_type = if_sxml_node=>co_nt_element_open.
            DO.
              reader->next_attribute( ).
              IF reader->node_type <> if_sxml_node=>co_nt_attribute.
                EXIT.
              ENDIF.
              APPEND |CO_NT_ATTRIBUTE, { reader->name }{ COND #( WHEN reader->value IS NOT INITIAL THEN |, { reader->value }| ) }| TO result.
            ENDDO.
          ENDIF.
        ENDDO.
      CATCH cx_sxml_state_error INTO DATA(error).
        "The example does not include a proper error handling.
        APPEND |--- Error: { error->get_text( ) } ---| TO result.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete class 4 (JSON parsing using /ui2/cl_json)
*&---------------------------------------------------------------------*

"This class reads JSON data using the '/ui2/cl_json' class and the 'generate'
"method, which creates an ABAP data object from JSON. It is only included to
"add more demo concrete classes.
"Instead of the typical usage of the class to include JSON data in known (in
"this case, unknown) data objects, the implementation here explores the created
"data and experiments with dynamic programming techniques. It is a nonsensical
"example that just explores and experiments. It neither represents best practices
"nor claims to cover all JSON variants (beyond the demo JSON data covered in the
"global class). Uncaught exceptions may occur. It is particulary not meant to
"reinvent the wheel, especially with the availability of the sXML library for the
"purpose. Like the other classes, the example just adds items to a string table.

CLASS lcl_json_parser_ui2cljson DEFINITION INHERITING FROM lcl_data_parser.
  PROTECTED SECTION.
    METHODS parse REDEFINITION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_node,
             node_name     TYPE string,
             ref           TYPE REF TO data,
             is_nested     TYPE abap_boolean,
             nesting_level TYPE i,
             value         TYPE string,
             done          TYPE abap_boolean,
             kind          TYPE REF TO cl_abap_typedescr,
           END OF ty_node,
           ty_nodes TYPE TABLE OF ty_node WITH EMPTY KEY.

    METHODS:
      process_elementary IMPORTING data TYPE REF TO data
                         CHANGING  node TYPE ty_node,
      process_structure IMPORTING data          TYPE REF TO data
                                  is_nested     TYPE abap_boolean DEFAULT abap_false
                                  nesting_level TYPE i DEFAULT 0,
      process_table IMPORTING data          TYPE REF TO data
                              name          TYPE string OPTIONAL
                              is_nested     TYPE abap_boolean DEFAULT abap_false
                              nesting_level TYPE i DEFAULT 0.

    DATA: json_data  TYPE TABLE OF REF TO data,
          node_table TYPE ty_nodes,
          error      TYPE REF TO cx_root,
          tabix      LIKE sy-tabix.
ENDCLASS.

CLASS lcl_json_parser_ui2cljson IMPLEMENTATION.
  METHOD parse.
    DATA(abap_obj_from_json) = /ui2/cl_json=>generate( jsonx = data ).

    "Checking JSON data and adding the data to an internal table for further processing
    IF ( abap_obj_from_json IS INITIAL OR abap_obj_from_json IS NOT BOUND )
    OR ( abap_obj_from_json->* IS INITIAL ).
      APPEND `--- JSON parsing not possible ---` TO result.
      RETURN.
    ENDIF.

    TRY.
        DATA(tdo_json) = cl_abap_typedescr=>describe_by_data_ref( abap_obj_from_json ).

        CASE TYPE OF tdo_json.
          WHEN TYPE cl_abap_tabledescr.
            LOOP AT abap_obj_from_json->* ASSIGNING FIELD-SYMBOL(<json>).
              IF <json> IS BOUND.
                IF <json>->* IS INITIAL.
                  APPEND `--- INITIAL ---` TO result.
                ELSE.
                  APPEND <json> TO json_data.
                ENDIF.
              ELSE.
                APPEND `--- JSON parsing not possible ---` TO result.
              ENDIF.
            ENDLOOP.
          WHEN TYPE cl_abap_structdescr.
            LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( abap_obj_from_json ) )->components INTO DATA(comp_json).
              ASSIGN abap_obj_from_json->(comp_json-name)->* TO FIELD-SYMBOL(<co>).
              APPEND REF #( <co> ) TO json_data.
            ENDLOOP.
          WHEN OTHERS.
            APPEND |--- Not supported by the example implementation ---| TO result.
        ENDCASE.
      CATCH cx_root INTO error .
        APPEND |--- Error: { error->get_text( ) } ---| TO result.
    ENDTRY.

    "Processing JSON data in the node table
    LOOP AT json_data ASSIGNING FIELD-SYMBOL(<data>).
      DATA(tdo) = cl_abap_typedescr=>describe_by_data_ref( <data> ).
      CASE TYPE OF tdo.
        WHEN TYPE cl_abap_structdescr.
          process_structure( <data> ).
        WHEN TYPE  cl_abap_elemdescr.
          APPEND VALUE #( ref = <data> ) TO node_table REFERENCE INTO DATA(elem_ref).

          process_elementary( EXPORTING data = <data>
                              CHANGING  node = elem_ref->* ).
        WHEN OTHERS.
          APPEND |--- Not supported by the example implementation ---| TO result.
          RETURN.
      ENDCASE.
    ENDLOOP.

    "Processing nested nodes
    WHILE line_exists( node_table[ done = abap_false ] ).
      LOOP AT node_table ASSIGNING FIELD-SYMBOL(<node>) WHERE done = abap_false.
        tabix = sy-tabix.
        <node>-done = abap_true.

        IF <node>-kind IS BOUND.
          CASE TYPE OF <node>-kind.
            WHEN TYPE cl_abap_elemdescr.
              process_elementary( EXPORTING data = <node>-ref CHANGING node = <node> ).
            WHEN TYPE cl_abap_structdescr.
              process_structure( data = <node>-ref is_nested = <node>-is_nested nesting_level = <node>-nesting_level ).
            WHEN TYPE cl_abap_tabledescr.
              process_table( data = <node>-ref is_nested = <node>-is_nested nesting_level = <node>-nesting_level name = <node>-node_name ).
          ENDCASE.
        ELSE.
          IF <node>-ref IS INITIAL.
            <node>-value = `%%%NULL%%%`.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

    "Putting the collected values into a string table
    IF node_table IS NOT INITIAL.
      result = VALUE #( BASE result FOR wa IN node_table
        ( |{ repeat( val = ` ` occ = wa-nesting_level ) }{ COND #( WHEN wa-node_name IS NOT INITIAL THEN |{ wa-node_name } | ) }{ wa-value }| ) ).
    ENDIF.
  ENDMETHOD.

  METHOD process_elementary.
    node-value = |{ data->* }|.
  ENDMETHOD.

  METHOD process_structure.
    LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( data ) )->components INTO DATA(comp).
      ASSIGN data->(comp-name) TO FIELD-SYMBOL(<comp>).

      IF <comp> IS NOT INITIAL.
        DATA(refdescr) = cl_abap_refdescr=>describe_by_data( <comp>->* ).
      ELSE.
        CLEAR refdescr.
      ENDIF.

      IF is_nested = abap_false.
        APPEND VALUE #( node_name = comp-name
                        ref = <comp>
                        is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                        nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                        kind = refdescr
                       ) TO node_table.
      ELSE.
        tabix += 1.

        INSERT VALUE #( node_name = comp-name
                       ref           = <comp>
                       is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                       nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                       kind = refdescr
                      ) INTO node_table INDEX tabix.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD process_table.
    LOOP AT data->* ASSIGNING FIELD-SYMBOL(<t>).
      DATA(outer_index) = sy-tabix.

      TRY.
          LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( <t> ) )->components INTO DATA(comp).
            DATA(idx) = sy-tabix.

            ASSIGN <t>->(comp-name) TO FIELD-SYMBOL(<comp>).
            DATA(refdescr) = cl_abap_refdescr=>describe_by_data( <comp>->* ).

            IF is_nested = abap_false.
              IF idx = 1.
                "Inserting a divider for table nodes
                APPEND VALUE #( node_name = |--- { name }-{ outer_index } ---|
                                nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                                done          = abap_true
                              ) TO node_table.
              ENDIF.

              APPEND VALUE #( node_name = comp-name
                              ref           = <comp>
                              is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                                nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                                kind = refdescr
                             ) TO node_table.
            ELSE.
              tabix += 1.

              IF idx = 1.
                "Inserting a divider for table nodes
                INSERT VALUE #( node_name = |--- { name }-{ outer_index } ---|
                                nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                                done          = abap_true
                              ) INTO node_table INDEX tabix.

                tabix += 1.
              ENDIF.

              INSERT VALUE #( node_name = comp-name
                             ref         = <comp>
                             is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                             nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                             kind = refdescr
                            ) INTO node_table INDEX tabix.
            ENDIF.
          ENDLOOP.
        CATCH cx_sy_move_cast_error.
          IF is_nested = abap_false.
            APPEND VALUE #( ref           = <t>
                            is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                            nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                            kind = cl_abap_typedescr=>describe_by_data( <t>->* )
                           ) TO node_table.
          ELSE.
            tabix += 1.

            INSERT VALUE #( ref           = <t>
                           is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                           nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                           kind = cl_abap_typedescr=>describe_by_data( <t>->* )
                          ) INTO node_table INDEX tabix.
          ENDIF.

      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
