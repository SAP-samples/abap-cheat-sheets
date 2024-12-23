"! <p class="shorttext"><strong>Working with XML and JSON in ABAP</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates working with XML and JSON in ABAP.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <p>Find information on <strong>getting started with the example class</strong> and the
"! <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
CLASS zcl_demo_abap_xml_json DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun,
      "This interface is implemented for serializing and
      "deserialzing instances of classes (objects).
      if_serializable_object.
    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS format IMPORTING input         TYPE data
                                   xml           TYPE abap_bool DEFAULT abap_true
                         RETURNING VALUE(string) TYPE string.
    TYPES c50 TYPE c LENGTH 50.
    TYPES c50_tab_type TYPE TABLE OF c50 WITH EMPTY KEY.
    TYPES x30 TYPE x LENGTH 30.
    TYPES x30_tab_type TYPE TABLE OF x30 WITH EMPTY KEY.

    "The following instance attribues and methods are used for serializing and
    "deserialzing instances of classes (objects)
    DATA: attr_string_a      TYPE string,
          attr_string_b      TYPE string,
          attr_concat_string TYPE string,
          attr_lowercase_str TYPE string.
    METHODS: concatenate_string,
      lowercase_string,
      "The following method can only have output parameters.
      "For each output parameter of the serialize_helper method, you must specify
      "an identically named input parameter of the deserialize_helper method
      "with the same type.
      serialize_helper EXPORTING attr_string_a      TYPE string
                                 attr_string_b      TYPE string
                                 attr_concat_string TYPE string,
      "This method can only have input parameters.
      deserialize_helper IMPORTING attr_string_a      TYPE string
                                   attr_string_b      TYPE string
                                   attr_concat_string TYPE string.

ENDCLASS.



CLASS zcl_demo_abap_xml_json IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: Working with XML and JSON in ABAP\n\n| ).
    out->write( |1) Excursion: Converting string <-> xstring| ).
    "In the following examples, many operations are performed using binary data.
    "This excursion shows the conversion of string to xstring and the other way round
    "using a codepage. The examples use UTF-8.
    "For this purpose, you can, for example, use the cl_abap_conv_codepage class
    "and the XCO library.

    "Storing XML data in a data object of type string
    DATA(xml_string) =
      `<flights>` &&
      `    <flight>` &&
      `        <carrier>LH</carrier>` &&
      `        <flightnumber>400</flightnumber>` &&
      `        <departure>` &&
      `            <city>Frankfurt</city>` &&
      `            <airport>FRA</airport>` &&
      `            <time>2023-12-23T10:30:00</time>` &&
      `        </departure>` &&
      `        <arrival>` &&
      `            <city>Berlin</city>` &&
      `            <airport>SXF</airport>` &&
      `            <time>2023-12-23T11:35:00</time>` &&
      `        </arrival>` &&
      `    </flight>` &&
      `    <flight>` &&
      `        <carrier>DL</carrier>` &&
      `        <flightnumber>1984</flightnumber>` &&
      `        <departure>` &&
      `            <city>San Francisco</city>` &&
      `            <airport>SFO</airport>` &&
      `            <time>2023-07-19T10:00:00</time>` &&
      `        </departure>` &&
      `        <arrival>` &&
      `            <city>New York</city>` &&
      `            <airport>JFK</airport>` &&
      `            <time>2023-07-19T18:25:00</time>` &&
      `        </arrival>` &&
      `    </flight>` &&
      `</flights>`.

    "string -> xstring
    "Note: UTF-8 is used by default. Here, it is specified explicitly.
    "Conversion errors are caught using the cx_sy_conversion_codepage class.
    TRY.
        DATA(conv_xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( xml_string ).
      CATCH cx_sy_conversion_codepage.
    ENDTRY.

    "xstring -> string
    DATA(conv_string) = cl_abap_conv_codepage=>create_in( )->convert( conv_xstring ).

    "As an alternative, you can use methods of the XCO library.
    "string -> xstring
    DATA(conv_xstring_xco) = xco_cp=>string( xml_string
      )->as_xstring( xco_cp_character=>code_page->utf_8
      )->value.

    "xstring -> string
    DATA(conv_string_xco) = xco_cp=>xstring( conv_xstring_xco
      )->as_string( xco_cp_character=>code_page->utf_8
      )->value.

    conv_string = format( conv_string ).
    conv_string_xco = format( conv_string_xco ).
    out->write( |\n| ).
    out->write( `Results of the xstring to string conversions:`  ).
    out->write( |\n| ).
    out->write( conv_string  ).
    out->write( |\n| ).
    out->write( conv_string_xco  ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Processing XML Using Class Libraries` ) ).
    out->write( |Excursion: Usable iXML/sXML Classes and Interfaces\n\n| ).
    "Using a released CDS view, classes and interfaces are retrieved that
    "have 'ixml' and 'sxml' in the name for you to explore the released classes
    "and interfaces in this context.

    SELECT ReleasedObjectType, ReleasedObjectName, ReleaseState
      FROM i_apisforclouddevelopment
      WHERE releasestate = 'RELEASED'
      AND
      ( ReleasedObjectType = 'CLAS' OR ReleasedObjectType = 'INTF' )
      AND
      ( ReleasedObjectName LIKE '%IXML%' OR ReleasedObjectName LIKE '%SXML%' )
      INTO TABLE @DATA(released_xml_libs).

    out->write( zcl_demo_abap_aux=>no_output ).

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Creating XML Data Using iXML` ) ).
    "In the following example, XML data is created using the iXML library.
    "This is done by creating DOM nodes step by step. The nodes are created as
    "elements and attributes. Content is inserted into the XML data.
    "The example uses an appropriate attribute setting so that the result
    "represents XML data in asXML format.
    "Note: You can check out more examples in the system by choosing CTRL +
    "SHIFT + A in ADT and search for *demo_ixml*.
    "For more information on the classes/methods used, check the documentation
    "by choosing F2 in ADT when placing the cursor on a class/methods.

    "Creating one factory object of the access class cl_ixml_core using the
    "create method. The object is used to access the iXML library.
    DATA(ixml_cr) = cl_ixml_core=>create( ).

    "Creating a document
    DATA(document_cr) = ixml_cr->create_document( ).

    "Step-by-step creation of DOM nodes
    "Creating a root node
    DATA(root) = document_cr->create_element_ns( name   = 'abap'
                                                 prefix = 'asx' ).
    root->set_attribute_ns( name   =  'asx'
                            prefix = 'xmlns'
                            value  = 'http://www.sap.com/abapxml' ).
    root->set_attribute_ns( name  =  'version'
                            value = '1.0' ).
    document_cr->append_child( root ).
    DATA(xml_node1) = document_cr->create_element_ns( prefix = 'asx'
                                                      name   = 'values' ).
    root->append_child( xml_node1 ).
    DATA(xml_node2) = document_cr->create_element_ns( name = 'STRING' ).
    xml_node1->append_child( xml_node2 ).
    xml_node2->append_child( document_cr->create_text( 'Hello ABAP' ) ).

    "Creating a renderer (for rendering the XML document into the output stream)
    DATA xml_doc TYPE xstring.
    ixml_cr->create_renderer( document = document_cr
                              ostream  = ixml_cr->create_stream_factory( )->create_ostream_xstring( string = xml_doc )
                            )->render( ).

    "Getting XML data
    DATA(xml_output) = format( cl_abap_conv_codepage=>create_in( )->convert( xml_doc ) ).
    out->write( xml_output ).

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Parsing XML Data Using iXML` ) ).
    "Notes on the example:
    "- XML data is transformed to an input steam object and imported to a DOM object in one go using a parser object
    "- If the XML is successfully parsed, DOM object nodes are iterated and accessed
    "- Elements of the XML data and their attributes are processed, read and output
    "- The create_renderer method is used to render XML data into an output stream

    TRY.
        DATA(some_xml) = cl_abap_conv_codepage=>create_out( )->convert(
             `<?xml version="1.0"?>` &&
             `<node attr_a="123">` &&
             ` <subnode1>` &&
             ` <letter>A</letter>` &&
             ` <date format="mm-dd-yyyy">01-01-2024</date>` &&
             ` </subnode1>` &&
             ` <subnode2>`  &&
             ` <text attr_b="1" attr_c="a">abc</text>` &&
             ` <text attr_b="2" attr_c="b">def</text>` &&
             ` <text attr_b="3" attr_c="c">ghi</text>` &&
             ` <text attr_b="4" attr_c="d">jkl</text>` &&
             ` </subnode2>` &&
             `</node>` ).
      CATCH cx_sy_conversion_codepage.
    ENDTRY.

    "Creating one factory object of the access class cl_ixml_core using the
    "create method. It is used to access the iXML library.
    DATA(ixml_pa) = cl_ixml_core=>create( ).
    "Creaing an input stream that is used for the input of XML data
    DATA(stream_factory_pa) = ixml_pa->create_stream_factory( ).
    "Creating an XML document stored in DOM format in the memory
    DATA(document_pa) = ixml_pa->create_document( ).
    "Creating a parser
    "It requires the following input parameters: input stream to be parsed,
    "the XML document to which the stream is parsed, a factory required to create a stream
    DATA(parser_pa) = ixml_pa->create_parser(
                       istream = stream_factory_pa->create_istream_xstring( string = some_xml )
                       document = document_pa
                       stream_factory = stream_factory_pa ).

    IF parser_pa->parse( ) <> 0.
      "Processing errors
      DATA(error_pa_num) = parser_pa->num_errors( ).

      DO error_pa_num TIMES.
        DATA(error_pa) = parser_pa->get_error(
                    index        = sy-index - 1
                    min_severity = 3
                  ).
        DATA(reason_pa) = error_pa->get_reason( ).
        out->write( |Error: { reason_pa }| ).
      ENDDO.
    ELSE.
      "Processing document
      IF document_pa IS NOT INITIAL.
        DATA(iterator_pa) = document_pa->create_iterator( ).
        DATA(node_pa) = iterator_pa->get_next( ).
        WHILE NOT node_pa IS INITIAL.
          DATA(indent) = node_pa->get_height( ) * 2.
          "Rettrieving the node type
          CASE node_pa->get_type( ).
            WHEN if_ixml_node=>co_node_element.
              "Retrieving attributes
              DATA(attributes_pa) = node_pa->get_attributes( ).
              out->write( |Element:{ repeat( val = ` ` occ = indent + 2  ) }{ node_pa->get_name( ) }| ).
              IF NOT attributes_pa IS INITIAL.
                DO attributes_pa->get_length( ) TIMES.
                  DATA(attr) = attributes_pa->get_item( sy-index - 1 ).
                  out->write( |Attribute:{ repeat( val = ` ` occ = indent ) }{ attr->get_name( ) } = { attr->get_value( ) } | ).
                ENDDO.
              ENDIF.
            WHEN if_ixml_node=>co_node_text OR
            if_ixml_node=>co_node_cdata_section.
              out->write( |Text:{ repeat( val = ` ` occ = indent + 5 ) }{ node_pa->get_value( ) }| ).
          ENDCASE.
          "Retrieving the next node
          node_pa = iterator_pa->get_next( ).
        ENDWHILE.
      ENDIF.
    ENDIF.

    "Creating a renderer
    DATA xml_pa TYPE xstring.
    ixml_pa->create_renderer( document = document_pa
                              ostream = ixml_pa->create_stream_factory( )->create_ostream_xstring( string = xml_pa )
                            )->render( ).

    "Getting XML
    DATA(output_ixml_parsing) = cl_abap_conv_codepage=>create_in( )->convert( xml_pa ).

    out->write( |\n| ).
    out->write( output_ixml_parsing ).

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Creating XML Data Using sXML (Token-Based Rendering)` ) ).
    "For sXML, there are specialized writer classes, such as CL_SXML_STRING_WRITE.
    "Writers created with this class render XML data to a byte string.
    "The XML 1.0 format and UTF-8 are used by default in the create method.
    "Here, the parameters are specified explicitly.
    "Note: The interface IF_SXML_WRITER contains the components that are valid
    "for all readers (the abstract superclass CL_SXML_WRITER includes this
    "interface as well as implementations for all readers). In the example below,
    "a cast is required so as to access special methods (such as open_element).

    DATA(writer) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type     = if_sxml=>co_xt_xml10
                                                                       encoding = 'UTF-8' ) ).

    TRY.
        "Creating nodes (the order of the nodes is important)
        writer->open_element( name = 'flights' ).
        writer->open_element( name = 'flight' ).
        writer->open_element( name = 'carrier' ).
        writer->write_value( 'LH' ).
        writer->close_element( ).
        writer->open_element( name = 'flightnumber' ).
        writer->write_value( '400' ).
        writer->close_element( ).
        writer->close_element( ).
        writer->open_element( name = 'flight' ).
        writer->open_element( name = 'carrier' ).
        writer->write_value( 'DL' ).
        writer->close_element( ).
        writer->open_element( name = 'flightnumber' ).
        writer->write_value( '1984' ).
        writer->close_element( ).
        writer->close_element( ).
        writer->close_element( ).
      CATCH cx_sxml_state_error INTO DATA(error_token).
        out->write( error_token->get_text( ) ).
    ENDTRY.

    "Getting XML data
    "The XML data can be retrieved with the GET_OUTPUT method.
    "Also here, a cast is required. The result is of type xstring.
    DATA(xml_sxml) = CAST cl_sxml_string_writer( writer )->get_output(  ).

    DATA(output_sxml_token_rendering) = format( cl_abap_conv_codepage=>create_in( )->convert( xml_sxml ) ).
    out->write( output_sxml_token_rendering ).

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) Creating XML Data using sXML (Object-Oriented Rendering)` ) ).

    DATA(writer_oo) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type     = if_sxml=>co_xt_xml10
                                                                          encoding = 'UTF-8' ) ).

    TRY.
        writer_oo->write_node( writer_oo->new_open_element( name = 'flights' ) ).

        writer_oo->write_node( writer_oo->new_open_element( name = 'flight' ) ).
        writer_oo->write_node( writer_oo->new_open_element( name = 'carrier' ) ).
        DATA(val) = writer_oo->new_value( ).
        val->set_value( 'AZ' ).
        writer_oo->write_node( val ).
        writer_oo->write_node( writer_oo->new_close_element( ) ).
        writer_oo->write_node( writer_oo->new_open_element( name = 'flightnumber' ) ).
        val = writer_oo->new_value( ).
        val->set_value( '788' ).
        writer_oo->write_node( val ).
        writer_oo->write_node( writer_oo->new_close_element( ) ).
        writer_oo->write_node( writer_oo->new_close_element( ) ).

        writer_oo->write_node( writer_oo->new_open_element( name = 'flight' ) ).
        writer_oo->write_node( writer_oo->new_open_element( name = 'carrier' ) ).
        val = writer_oo->new_value( ).
        val->set_value( 'JL' ).
        writer_oo->write_node( val ).
        writer_oo->write_node( writer_oo->new_close_element( ) ).
        writer_oo->write_node( writer_oo->new_open_element( name = 'flightnumber' ) ).
        val = writer_oo->new_value( ).
        val->set_value( '407' ).
        writer_oo->write_node( val ).
        writer_oo->write_node( writer_oo->new_close_element( ) ).
        writer_oo->write_node( writer_oo->new_close_element( ) ).

        writer_oo->write_node( writer_oo->new_close_element( ) ).
      CATCH cx_sxml_state_error INTO DATA(error_oo).
        out->write( error_oo->get_text( ) ).
    ENDTRY.

    DATA(xml_sxml_oo_rendering) =  CAST cl_sxml_string_writer( writer_oo )->get_output(  ).

    DATA(output_sxml_oo_rendering) = format( cl_abap_conv_codepage=>create_in( )->convert( xml_sxml_oo_rendering ) ).
    out->write( output_sxml_oo_rendering ).

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) Parsing XML Data using sXML (Token-Based Parsing)` ) ).

    "Creating demo XML data to be used in the example
    TRY.
        DATA(xml_to_parse) = cl_abap_conv_codepage=>create_out( )->convert(
             `<?xml version="1.0"?>` &&
             `<node attr_a="123">` &&
             ` <subnode1>` &&
             ` <letter>A</letter>` &&
             ` <date format="mm-dd-yyyy">01-01-2024</date>` &&
             ` </subnode1>` &&
             ` <subnode2>`  &&
             ` <text attr_b="1" attr_c="a">abc</text>` &&
             ` <text attr_b="2" attr_c="b">def</text>` &&
             ` <text attr_b="3" attr_c="c">ghi</text>` &&
             ` <text attr_b="4" attr_c="d">jkl</text>` &&
             ` </subnode2>` &&
             `</node>` ).
      CATCH cx_sy_conversion_codepage.
    ENDTRY.

    "Creating an internal table for display purposes
    DATA: BEGIN OF node_info,
            node_type TYPE string,
            name      TYPE string,
            value     TYPE string,
          END OF node_info,
          nodes_tab LIKE TABLE OF node_info.

    "Creating reader
    "Note: See the comments for the writer above which is similar. For readers,
    "the interface IF_SXML_READER exists. In this example, no special methods
    "are used. Therefore, a cast is not carried out.
    DATA(reader) = cl_sxml_string_reader=>create( xml_to_parse ).
    "DATA(reader_cast) = CAST if_sxml_reader( cl_sxml_string_reader=>create( xml_oo ) ).

    "To iterate accros all nodes, you can call the NEXT_NODE method.
    TRY.
        DO.
          CLEAR node_info.
          reader->next_node( ).

          "When reaching the end of the XML data, the loop is exited.
          IF reader->node_type = if_sxml_node=>co_nt_final.
            EXIT.
          ENDIF.

          "You can access the properties of the node directly.
          "For display purposes, the property information is stored in an internal table.
          "The example here just uses simple demo JSON data. Not all properties are
          "retrieved and displayed.

          "Node type, see the interface if_sxml_node
          node_info-node_type = SWITCH #( reader->node_type WHEN if_sxml_node=>co_nt_initial THEN `CO_NT_INITIAL`
                                                            WHEN if_sxml_node=>co_nt_element_open THEN `CO_NT_ELEMENT_OPEN`
                                                            WHEN if_sxml_node=>co_nt_element_close THEN `CO_NT_ELEMENT_CLOSE`
                                                            WHEN if_sxml_node=>co_nt_value THEN `CO_NT_VALUE`
                                                            WHEN if_sxml_node=>co_nt_attribute THEN `CO_NT_ATTRIBUTE`
                                                            ELSE `Error` ).
          "Name of the element
          node_info-name = reader->name.
          "Character-like value (if it is textual data)
          node_info-value = COND #( WHEN reader->node_type = if_sxml_node=>co_nt_value THEN reader->value ).
          APPEND node_info TO nodes_tab.

          "Once the method is called, you can directly access the attributes of the reader with the required
          "properties of the node. When the parser is on the node of an element opening, you can use the method
          "NEXT_ATTRIBUTE to iterate across the XML element attributes.
          IF reader->node_type = if_sxml_node=>co_nt_element_open.
            DO.
              reader->next_attribute( ).
              IF reader->node_type <> if_sxml_node=>co_nt_attribute.
                EXIT.
              ENDIF.
              APPEND VALUE #( node_type  = `CO_NT_ATTRIBUTE`
                              name       = reader->name
                              value      = reader->value ) TO nodes_tab.
            ENDDO.
          ENDIF.
        ENDDO.

        out->write( nodes_tab ).
      CATCH cx_sxml_state_error INTO DATA(error_parse_token).
        out->write( error_parse_token->get_text( ) ).
    ENDTRY.

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Parsing XML Data using sXML (Object-Oriented Parsing)` ) ).

    CLEAR nodes_tab.
    DATA(reader_oo) = cl_sxml_string_reader=>create( xml_to_parse ).

    TRY.
        DO.
          "To iterate accros all nodes, you can call the READ_NEXT_NODE method.
          "When the end of the XML data is reached, the returned value is initial.
          DATA(node_oo) = reader_oo->read_next_node( ).
          IF node_oo IS INITIAL.
            EXIT.
          ENDIF.

          "In object-oriented parsing, methods for token-based parsing are wrapped.
          "An object-oriented access to the node is provided.
          "References to objects that represent the current node are returned.

          "Getting the node type
          DATA(n_type) = node_oo->type.

          "If the parser is currently on the node of an element opening,
          "the node object has the class CL_SXML_OPEN_ELEMENT that implements the
          "interface IF_SXML_OPEN_ELEMENT. With the methods included, you can
          "access the XML attributes of the element, e.g. using the GET_ATTRIBUTES
          "method to put the references for all attributes into an internal table.
          "To access the attributes, a downcast is required.

          CASE n_type.
            WHEN if_sxml_node=>co_nt_element_open.
              DATA(open_element) = CAST if_sxml_open_element( node_oo ).

              APPEND VALUE #( node_type = `open element`
                              name      = open_element->qname-name
                            ) TO nodes_tab.

              DATA(attributes) = open_element->get_attributes( ).

              LOOP AT attributes INTO DATA(attribute).
                APPEND VALUE #( node_type = `attribute`
                                name      = open_element->qname-name
                                value  = SWITCH #( attribute->value_type WHEN if_sxml_value=>co_vt_text THEN attribute->get_value( ) )
                              ) TO nodes_tab.
              ENDLOOP.

            WHEN  if_sxml_node=>co_nt_element_close.
              DATA(close_element) = CAST if_sxml_close_element( node_oo ).

              APPEND VALUE #( node_type = `close element`
                              name      = open_element->qname-name
                            ) TO nodes_tab.

            WHEN  if_sxml_node=>co_nt_value.
              DATA(value_node_oo) = CAST if_sxml_value_node( node_oo ).

              APPEND VALUE #( node_type = `value`
                              value     = SWITCH #( value_node_oo->value_type WHEN if_sxml_value=>co_vt_text THEN value_node_oo->get_value( ) )
                            ) TO nodes_tab.

            WHEN OTHERS.
              APPEND VALUE #( node_type = `Error` ) TO nodes_tab.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_state_error INTO DATA(error_parse_oo).
        out->write( error_parse_oo->get_text( ) ).
    ENDTRY.

    out->write( nodes_tab ).

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `XML Transformations` ) ).

    out->write( |9) Excursion: Available ABAP Cheat Sheet Transformations in the System\n\n| ).
    "Excursion using the XCO library. In this example, tranformation programs are retrieved.
    "A filter is applied. Because of a filter that is applied, only the ABAP cheat sheet
    "transformation programs are returned.

    DATA(filter) = xco_cp_abap_repository=>object_name->get_filter(
           xco_cp_abap_sql=>constraint->contains_pattern( 'ZDEMO_ABAP_%' ) ).

    DATA(filtered_transformations) = xco_cp_abap_repository=>objects->xslt->where( VALUE #( ( filter )
      ) )->in( xco_cp_abap=>repository )->get( ).

    IF filtered_transformations IS NOT INITIAL.
      TYPES cst TYPE TABLE OF sxco_tf_object_name WITH EMPTY KEY.
      DATA(cheat_sheet_transformations) = VALUE cst( FOR tr IN filtered_transformations ( tr->name ) ).
      out->write( cheat_sheet_transformations ).
      out->write( |\n| ).
      out->write( `The code contains an implementation that gets all the transformations in the system.` ).
      out->write( `You can check the content of the variable in the debugger. Among the transformations is the predefined identity transformation ID.` ).
    ENDIF.

    "Getting all transformations in the system
    "You can check the table content in the debugger.
    DATA(all_transformations) = xco_cp_abap_repository=>objects->xslt->all->in( xco_cp_abap=>repository )->get( ).
    IF all_transformations IS NOT INITIAL.
      DATA(all_transformations_in_system) = VALUE cst( FOR tr IN all_transformations ( tr->name ) ).
    ENDIF.

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) Transforming XML to XML Using XSLT` ) ).
    "In this example, XML is transformed to XML. For this purpose, a simple XSLT
    "program does the following:
    "- All nodes and attributes are copied from the source XML to the target XML
    "  without any changes.
    "- A match on two elements is performed. If matched, two new elements are created
    "  in the target XML. In this operation, calculations are carried out (free seats
    "  and the occupancy rate based on the values of maximum and occupied seats).
    "Note:
    "- The element names in the demo XML data are capitalized on purpose because the
    "  XML data is used in another example that uses the asXML format. In deserializations
    "  of XML data to ABAP data, the elements that are deserialized must be capitalized
    "  so that they can be identified.
    "Tranformations are performed using CALL TRANSFORMATION statements.

    "Creating demo XML data to be used in the example
    TRY.
        DATA(xml_flights) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert(
        `<FLIGHTS>` &&
        `    <FLIGHT>` &&
        `        <CARRIER>AA</CARRIER>` &&
        `        <CONNECTIONID>17</CONNECTIONID>` &&
        `        <MAXSEATS>385</MAXSEATS>` &&
        `        <OCCSEATS>369</OCCSEATS>` &&
        `    </FLIGHT>` &&
        `    <FLIGHT>` &&
        `        <CARRIER>LH</CARRIER>` &&
        `        <CONNECTIONID>400</CONNECTIONID>` &&
        `        <MAXSEATS>330</MAXSEATS>` &&
        `        <OCCSEATS>319</OCCSEATS>` &&
        `    </FLIGHT>` &&
        `</FLIGHTS>` ).
      CATCH cx_sy_conversion_codepage.
    ENDTRY.

    DATA xml_a TYPE xstring.

    CALL TRANSFORMATION zdemo_abap_xslt_fl
                        SOURCE XML xml_flights
                        RESULT XML xml_a.

    DATA(conv_xml_a) = format( cl_abap_conv_codepage=>create_in( )->convert( xml_a ) ).
    out->write( conv_xml_a ).

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) Transforming ABAP to XML Using XSLT` ) ).
    "In the example, data entries are retrieved from a database table. Appropriate names for the
    "table columns are used with the AS addition so that the transformation can be carried
    "out based on the selected data.
    "Note: The resulting XML has the asXML format.

    SELECT carrid AS carrier,
           connid AS connectionid,
           seatsmax AS maxSeats,
           seatsocc AS occSeats
      FROM zdemo_abap_fli
      WHERE carrid = 'AZ'
      INTO TABLE @DATA(fli_itab)
      UP TO 2 ROWS.

    DATA xml_b TYPE xstring.

    CALL TRANSFORMATION zdemo_abap_xslt_fl
                        SOURCE flights = fli_itab
                        RESULT XML xml_b.

    DATA(conv_xml_b) = format( cl_abap_conv_codepage=>create_in( )->convert( xml_b ) ).
    out->write( conv_xml_b ).

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `12) ABAP <-> XML using Simple Transformations (1)` ) ).
    "The following simple transformation examples transform ABAP to XML and back. The
    "Simple Transformation is implemented in a way to transform to the HTML format.
    "This example transforms string tables to html.

    DATA(string_table_a) = VALUE string_table( ( `abc` ) ( `def` ) ( `ghi` ) ).

    DATA xml_c TYPE xstring.

    "ABAP -> XML
    CALL TRANSFORMATION zdemo_abap_st_strhtml
                        SOURCE string_table = string_table_a
                        RESULT XML xml_c.

    DATA(conv_xml_c) = format( cl_abap_conv_codepage=>create_in( )->convert( xml_c ) ).
    out->write( `ABAP -> XML` ).
    out->write( conv_xml_c ).
    out->write( |\n| ).

    "XML -> ABAP
    DATA string_table_b TYPE string_table.
    CALL TRANSFORMATION zdemo_abap_st_strhtml
                              SOURCE XML xml_c
                              RESULT string_table = string_table_b.

    out->write( `XML -> ABAP` ).
    out->write( string_table_b ).

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `13) ABAP <-> XML using Simple Transformations (2)` ) ).
    "In this example, an internal table is transformed to XML using Simple Transformation.
    "HTML tags are inserted into the XML data as literals.

    SELECT carrid, carrname, currcode, url
     FROM zdemo_abap_carr
     INTO TABLE @DATA(carr_tab_a)
     UP TO 2 ROWS.

    "ABAP -> XML
    DATA str_a TYPE string.

    "The following CALL TRANSFORMATION statement includes the OPTIONS
    "addition. In this example, the XML header should not be added.
    "So, you can take the resulting html and test it in an HTML viewer.

    CALL TRANSFORMATION zdemo_abap_st_carrhtml
                        SOURCE carrier_info = carr_tab_a
                        RESULT XML str_a
                        OPTIONS xml_header = 'NO'.

    out->write( `ABAP -> XML` ).
    out->write( format( str_a ) ).
    out->write( |\n| ).

    "XML -> ABAP
    DATA carr_tab_b LIKE carr_tab_a.

    CALL TRANSFORMATION zdemo_abap_st_carrhtml
                        SOURCE XML str_a
                        RESULT carrier_info = carr_tab_b.

    out->write( `XML -> ABAP` ).
    out->write( carr_tab_b ).

***********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `ABAP <-> XML using XSLT (Using the Predefined Identity Transformation ID)` ) ).
    "The following examples demonstrate serializations (ABAP to XML) and deserializations (XML to ABAP)
    "using the predefined identity transformation ID.
    "Note: In doing so, ABAP data is transformed to their asXML representations that can be used as an
    "      intermediate format and which define a mapping between ABAP data and XML.

    out->write( |14) Elementary type\n\n| ).
    "The example uses type string as an elementary type.

    "ABAP -> XML
    DATA xml_d TYPE xstring.
    DATA(str_b) = `This is some string.`.

    CALL TRANSFORMATION id SOURCE txt = str_b
                           RESULT XML xml_d.

    DATA(conv_xml_d) = cl_abap_conv_codepage=>create_in( )->convert( xml_d ).
    out->write( `ABAP -> XML` ).
    out->write( format( conv_xml_d ) ).
    out->write( |\n| ).

    "XML -> ABAP
    DATA str_c TYPE string.
    CALL TRANSFORMATION id SOURCE XML xml_d
                           RESULT txt = str_c.

    out->write( `XML -> ABAP` ).
    out->write( str_c ).

************************************************************************

    out->write( zcl_demo_abap_aux=>heading( `15) Structures` ) ).

    SELECT SINGLE carrid, carrname, currcode, url
      FROM zdemo_abap_carr
      WHERE carrid = 'LH'
      INTO @DATA(carr_struc_a).

    "ABAP -> XML
    DATA xml_e TYPE xstring.
    CALL TRANSFORMATION id SOURCE structure = carr_struc_a
                           RESULT XML xml_e.

    DATA(conv_xml_e) = cl_abap_conv_codepage=>create_in( )->convert( xml_e ).
    out->write( `ABAP -> XML` ).
    out->write( format( conv_xml_e ) ).
    out->write( |\n| ).

    "XML -> ABAP
    DATA carr_struc_b LIKE carr_struc_a.
    CALL TRANSFORMATION id SOURCE XML xml_e
                           RESULT structure = carr_struc_b.

    out->write( `XML -> ABAP` ).
    out->write( carr_struc_b ).
    out->write( |\n| ).

************************************************************************

    out->write( zcl_demo_abap_aux=>heading( `16) Internal tables` ) ).

    SELECT carrid, connid, fldate, price, currency
      FROM zdemo_abap_fli
      WHERE carrid = 'JL'
      INTO TABLE @DATA(fli_tab_a)
      UP TO 2 ROWS.

    "ABAP -> XML
    DATA xml_f TYPE xstring.
    CALL TRANSFORMATION id SOURCE itab = fli_tab_a
                           RESULT XML xml_f.

    DATA(conv_xml_f) = cl_abap_conv_codepage=>create_in( )->convert( xml_f ).
    out->write( `ABAP -> XML` ).
    out->write( format( conv_xml_f ) ).
    out->write( |\n| ).

    "XML -> ABAP
    DATA fli_tab_b LIKE fli_tab_a.
    CALL TRANSFORMATION id SOURCE XML xml_f
                           RESULT itab = fli_tab_b.

    out->write( `XML -> ABAP` ).
    out->write( fli_tab_b ).

************************************************************************

    out->write( zcl_demo_abap_aux=>heading( `17) Data References` ) ).

    DATA(dref_a) = NEW i( 123 ).

    "ABAP -> XML
    DATA xml_g TYPE xstring.
    CALL TRANSFORMATION id SOURCE dref = dref_a
                           RESULT XML xml_g.

    DATA(conv_xml_g) = cl_abap_conv_codepage=>create_in( )->convert( xml_g ).

    out->write( `ABAP -> XML` ).
    out->write( format( conv_xml_g ) ).
    out->write( |\n| ).

    "XML -> ABAP
    DATA dref_b LIKE dref_a.
    CALL TRANSFORMATION id SOURCE XML xml_g
                           RESULT dref = dref_b.

    out->write( `XML -> ABAP` ).
    out->write( dref_b->* ).

************************************************************************

    out->write( zcl_demo_abap_aux=>heading( `18) Object References` ) ).
    "The following example demonstrates the serialization and deserialization of
    "instances of classes (objects). For example, to serialize instance attributes,
    "classes must implement the if_serializable_object interface. By default, all instance
    "attributes of an object are serialized, regardless of their visibility section.
    "However, you can change this behavior using the serialize_helper and deserialize_helper
    "instance methods. As a result of the transformation, you get an asXML representation of
    "the object.
    "The example is implemented as follows:
    "- The class implements the if_serializable_object interface.
    "- There are 4 instance attributes of type string.
    "- There are two instance methods:
    "  - One method concatenates two of the strings and assigns the resulting string to
    "    another string.
    "  - Another method concatenates two strings and converts the string to lowercase.
    "    The result is assigned to another string.
    "- Serialization preserves instance attribute values in the asXML representation of the
    "  object.
    "- During deserialization, the instance attribute values are transformed back and can
    "  be accessed.
    "- The example includes the implementation of the serialize_helper and deserialize_helper
    "  instance methods. Without implementation, all instance attributes would be
    "  serialized/deserialized. The sample implementation limits serialization/deserialization.
    "  The fourth instance attribute, which is converted to a lowercase string when calling the
    "  method, is not part of the serialization/deserialization. See the implementation of the
    "  serialize_helper and deserialize_helper instance methods. They explicitly specify what
    "  to serialize and deserialize.
    "- Note: For each output parameter of the serialize_helper method, you must specify an
    "  identically-named input parameter of the deserialize_helper method. The parameters must
    "  have the same type.

    DATA(oref_a) = NEW zcl_demo_abap_xml_json(  ).
    oref_a->attr_string_a = `AB`.
    oref_a->attr_string_b = `AP`.
    oref_a->concatenate_string( ).
    oref_a->lowercase_string( ).

    out->write( `Value of instance attribute attr_lowercase_str for the created instance (before serialization/deserialization):` ).
    out->write( oref_a->attr_lowercase_str ).
    out->write( |\n| ).

    "ABAP -> XML
    DATA xml_oref_a TYPE xstring.
    CALL TRANSFORMATION id SOURCE oref = oref_a
                           RESULT XML xml_oref_a.

    DATA(conv_xml_oref_a) = cl_abap_conv_codepage=>create_in( )->convert( xml_oref_a ).

    out->write( `ABAP -> XML` ).
    out->write( format( conv_xml_oref_a ) ).
    out->write( |\n| ).

    "XML -> ABAP
    DATA oref_b LIKE oref_a.
    CALL TRANSFORMATION id SOURCE XML xml_oref_a
                           RESULT oref = oref_b.

    out->write( `XML -> ABAP` ).
    out->write( oref_b->attr_string_a ).
    out->write( oref_b->attr_string_b ).
    out->write( oref_b->attr_concat_string ).
    out->write( |\n| ).
    IF oref_b->attr_lowercase_str IS INITIAL.
      out->write( `The instance attribute attr_lowercase_str is initial. The serialization/deserialization is restricted.` ).
    ENDIF.

************************************************************************

    out->write( zcl_demo_abap_aux=>heading( `19) CALL TRANSFORMATION Syntax: Specifying Transformations` ) ).
    "As already covered in the examples above, transformations are specified after
    "CALL TRANSFORMATION. They are either ...

    "... XSLT programs
    CALL TRANSFORMATION zdemo_abap_st_carrhtml SOURCE carrier_info = carr_tab_a
                                                    RESULT XML str_a.

    "... predefined identity transformations
    CALL TRANSFORMATION id SOURCE dref = dref_a
                           RESULT XML xml_g.

    "... dynamically specified transformation (valid for both XSLT and ST). In the examples above,
    "the transformation is specified statically. Dynamic specifications are possible. A
    "character-like data object in uppercase letters is expected in parentheses (either a named
    "or unnamed data objects).
    CALL TRANSFORMATION ('ID') SOURCE dref = dref_a
                               RESULT XML xml_g.

    "If a dynamically specified transformation is not found, an exception of the class
    "CX_INVALID_TRANSFORMATION is raied. The example uses a named data object.
    DATA(notr) = 'NON_EXISTENT_TRANSFORMATION'.
    TRY.
        CALL TRANSFORMATION (notr) SOURCE dref = dref_a
                                   RESULT XML xml_g.
      CATCH cx_invalid_transformation INTO DATA(error_non).
        out->write( error_non->get_text( ) ).
    ENDTRY.

    "... Simple Transformation
    CALL TRANSFORMATION zdemo_abap_st_carrhtml SOURCE XML str_a
                                                    RESULT carrier_info = carr_tab_b.

************************************************************************

    out->write( zcl_demo_abap_aux=>heading( `20) CALL TRANSFORMATION Syntax: Sources of Transformations` ) ).
    "The following examples use the predefined identity transformation ID.
    "The result is asXML data and stored in a variable of type xstring.
    "Multiple options and variants are possible. The examples cover a selection.

    out->write( `********** Source options for transforming XML data **********` ).
    out->write( |\n| ).

    "Source: XML data in a data object of type string. It is implicitly transformed to
    "XML in this case.
    "Note: It must have character-like XML data in XML 1.0 format.
    DATA(str_d) = `<TXT>Hello world</TXT>`.
    DATA xml_h TYPE xstring.
    CALL TRANSFORMATION id SOURCE XML str_d
                           RESULT XML xml_h.

    DATA(conv_xml_h) = cl_abap_conv_codepage=>create_in( )->convert( xml_h ).

    out->write( `Source: XML data in data object of type string` ).
    out->write( format( conv_xml_h ) ).
    out->write( |\n| ).

    "Source: XML data in a data object of type xstring
    DATA(xml_i) = cl_abap_conv_codepage=>create_out( )->convert( `<TEXT name="TXT">Hi ABAP</TEXT>` ).
    DATA xml_j TYPE xstring.
    CALL TRANSFORMATION id SOURCE XML xml_i
                           RESULT XML xml_j.

    DATA(conv_xml_j) = cl_abap_conv_codepage=>create_in( )->convert( xml_j ).
    out->write( `Source: XML data in a data object of type xstring` ).
    out->write( format( conv_xml_j ) ).
    out->write( |\n| ).

    "Source: Standard table with character-like data type
    DATA(stdtab_clike) = VALUE c50_tab_type( ( 'Hi' ) ( 'ABAP' ) ).
    DATA xml_k TYPE xstring.
    CALL TRANSFORMATION id SOURCE tab = stdtab_clike
                           RESULT XML xml_k.

    DATA(conv_xml_k) = cl_abap_conv_codepage=>create_in( )->convert( xml_k ).
    out->write( `Source: Standard table with character-like data type` ).
    out->write( format( conv_xml_k ) ).
    out->write( |\n| ).

    "Source: Standard table with byte-like data type
    DATA(stdtab_bytelike) = VALUE x30_tab_type( ( cl_abap_conv_codepage=>create_out( )->convert( `Hello` ) )
                                                ( cl_abap_conv_codepage=>create_out( )->convert( `ABAP` ) ) ).
    DATA xml_l TYPE xstring.
    CALL TRANSFORMATION id SOURCE xtab = stdtab_bytelike
                           RESULT XML xml_l.

    DATA(conv_xml_l) = cl_abap_conv_codepage=>create_in( )->convert( xml_l ).
    out->write( `Source: Standard table with byte-like data type` ).
    out->write( format( conv_xml_l ) ).
    out->write( |\n| ).

    "Furthermore, some references to iXML and sXML libraries are possible.
    "The following example covers sXML (an interface reference variable of type if_sxml_reader).
    DATA(sxml_reader) = cl_sxml_string_reader=>create( xml_h ).
    DATA xml_m TYPE xstring.
    CALL TRANSFORMATION id SOURCE XML sxml_reader
                           RESULT XML xml_m.

    DATA(conv_xml_m) = cl_abap_conv_codepage=>create_in( )->convert( xml_m ).
    out->write( `Source: Interface reference variable with TYPE REF TO if_sxml_reader` ).
    out->write( format( conv_xml_m ) ).
    out->write( |\n| ).

    out->write( `********** Source options for transforming ABAP data **********` ).
    out->write( |\n| ).
    "Using ... SOURCE ... without specifying XML
    "One or multiple ABAP data objects can be specified.

    "Source: Data object of type string
    "elem stands for the name of an XML element.
    DATA(str_e) = `abcdef`.
    DATA xml_n TYPE xstring.
    CALL TRANSFORMATION id SOURCE elem = str_e
                           RESULT XML xml_n.

    DATA(conv_xml_n) = cl_abap_conv_codepage=>create_in( )->convert( xml_n ).
    out->write( `Source: Character string in data object of type string` ).
    out->write( format( conv_xml_n ) ).
    out->write( |\n| ).

    "Source: Data object of type string
    DATA(str_f) = `some string`.
    DATA xml_o TYPE xstring.
    CALL TRANSFORMATION id SOURCE txt = str_f
                           RESULT XML xml_o.

    DATA(conv_xml_o) = cl_abap_conv_codepage=>create_in( )->convert( xml_o ).
    out->write( `Source: Character string of type string` ).
    out->write( format( conv_xml_o ) ).
    out->write( |\n| ).

    "Source: Multiple data objects
    DATA a_i TYPE i VALUE 123.
    DATA b_str TYPE string VALUE `Hallo`.
    DATA c_p TYPE p LENGTH 5 DECIMALS 2 VALUE `4.56`.
    DATA xml_p TYPE xstring.

    CALL TRANSFORMATION id SOURCE x = a_i
                                  y = b_str
                                  z = c_p
                           RESULT XML xml_p.

    DATA(conv_xml_p) = cl_abap_conv_codepage=>create_in( )->convert( xml_p ).
    out->write( `Source: Multiple ABAP data objects in a static parameter list` ).
    out->write( format( conv_xml_p ) ).
    out->write( |\n| ).

    "Source: Dynamic specification of ABAP data objects in an internal table of
    "type abap_trans_srcbind_tab
    DATA(srctab) = VALUE abap_trans_srcbind_tab(
      ( name = 'X' value = REF #( a_i ) )
      ( name = 'Y' value = REF #( b_str ) )
      ( name = 'Z' value = REF #( c_p ) ) ).
    DATA xml_q TYPE xstring.

    CALL TRANSFORMATION id SOURCE (srctab)
                           RESULT XML xml_q.

    DATA(conv_xml_q) = cl_abap_conv_codepage=>create_in( )->convert( xml_q ).
    out->write( `Source: Multiple ABAP data objects in an internal table` ).
    out->write( format( conv_xml_q ) ).

*************************************************************************

    out->write( zcl_demo_abap_aux=>heading( `21) CALL TRANSFORMATION Syntax: Results of Transformations` ) ).
    "As in the examples above, the predefined identity transformation is used here.

    "Creating demo XML data to be used in the example as the source.
    DATA(demo_xml) = cl_abap_conv_codepage=>create_out( )->convert( `<TEXT name="TXT">Hi ABAP</TEXT>` ).

    out->write( `********** Result options for transforming to XML data ********** ` ).
    out->write( |\n| ).

    "Result: Data object of type string
    "The example uses the OPTIONS addition. The XML header should not be added.
    DATA str_g TYPE string.
    CALL TRANSFORMATION id SOURCE XML demo_xml
                           RESULT XML str_g
                           OPTIONS xml_header = 'NO'.

    out->write( `Result: Data object of type string` ).
    out->write( str_g ).
    out->write( |\n| ).

    "Data object of type xstring
    DATA xml_r TYPE xstring.
    CALL TRANSFORMATION id SOURCE XML demo_xml
                           RESULT XML xml_r.

    DATA(conv_xml_r) = cl_abap_conv_codepage=>create_in( )->convert( xml_r ).
    out->write( `Result: Data object of type xstring` ).
    out->write( format( conv_xml_r ) ).
    out->write( |\n| ).

    "Data object declared inline (e.g. DATA(a) or FINAL(b)), which has then the type xstring
    CALL TRANSFORMATION id SOURCE XML demo_xml
                           RESULT XML DATA(xml_s).

    DATA(conv_xml_s) = cl_abap_conv_codepage=>create_in( )->convert( xml_s ).
    out->write( `Result: Data object declared inline (type xstring)` ).
    out->write( format( conv_xml_s ) ).
    out->write( |\n| ).

    "Standard table with character-like line type
    DATA stdtab_clike_b TYPE c50_tab_type.
    CALL TRANSFORMATION id SOURCE XML demo_xml
                           RESULT XML stdtab_clike_b
                           OPTIONS xml_header = 'NO'.

    out->write( `Result: Standard table with character-like line type` ).
    out->write( stdtab_clike_b ).
    out->write( |\n| ).

    "Standard table with byte-like line type
    DATA stdtab_bytelike_b TYPE x30_tab_type.
    CALL TRANSFORMATION id SOURCE XML demo_xml
                           RESULT XML stdtab_bytelike_b.

    out->write( `Result: Standard table with byte-like line type` ).
    out->write( stdtab_bytelike_b ).
    out->write( |\n| ).

    "Furthermore, some references to iXML and sXML libraries are possible.
    "The following example covers sXML (an object reference variable of type ref to cl_sxml_string_writer)
    "Other types are possible, for example JSON writers.
    DATA(writer4tr) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_xml10 ).
    CALL TRANSFORMATION id SOURCE XML demo_xml
                           RESULT XML writer4tr.

    DATA(xml_t) = writer4tr->get_output( ).
    DATA(conv_xml_t) = cl_abap_conv_codepage=>create_in( )->convert( xml_t ).
    out->write( `Result: Object reference variable with type ref to cl_sxml_string_writer` ).
    out->write( format( conv_xml_t ) ).
    out->write( |\n| ).

    out->write( `********** Result options for transforming to ABAP data **********` ).
    out->write( |\n| ).

    "Similar to above, multiple ABAP data objects can be specified as a static parameter list.
    "Here, the example from above is used. The tranformation is performed the other way round.
    DATA d_i LIKE a_i.
    DATA e_str LIKE b_str.
    DATA f_p LIKE c_p.

    CALL TRANSFORMATION id SOURCE XML xml_p
                           RESULT x = d_i
                                  y = e_str
                                  z = f_p.

    out->write( `Result: Multiple ABAP data objects in a static parameter list` ).
    out->write( d_i ).
    out->write( e_str ).
    out->write( f_p ).
    out->write( |\n| ).

    "Specifying an internal table of type abap_trans_resbind_tab
    DATA g_i LIKE a_i.
    DATA h_str LIKE b_str.
    DATA i_p LIKE c_p.

    "Note: Only bound parts are deserialized, i.e. the result table must be
    "filled accordingly.
    DATA(restab) = VALUE abap_trans_resbind_tab(
       ( name = 'X' value = REF #( d_i ) )
       ( name = 'Y' value = REF #( e_str ) )
       ( name = 'Z' value = REF #( f_p ) ) ).

    CALL TRANSFORMATION id SOURCE XML xml_q
                           RESULT (restab).

    out->write( `Result: Multiple ABAP data objects in an internal table` ).
    out->write( restab ).

************************************************************************

    out->write( zcl_demo_abap_aux=>heading( `22) Transforming JSON Data Using Transformations` ) ).
    "Note: When the identity transformation ID is used, the format is asJSON.

    "Elementary type
    DATA str_h TYPE string VALUE `Hello`.
    "ABAP -> JSON
    "Creating a JSON writer
    DATA(json_wr_a) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    CALL TRANSFORMATION id SOURCE hi = str_h
                           RESULT XML json_wr_a.

    DATA(json_a) = cl_abap_conv_codepage=>create_in( )->convert( json_wr_a->get_output( ) ).

    out->write( `ABAP -> JSON: Elementary type` ).
    out->write( json_a ).
    out->write( |\n| ).

    "JSON -> ABAP
    DATA str_i TYPE string.
    "Note: CALL TRANSFORMATION handles JSON sources implicitly.
    CALL TRANSFORMATION id SOURCE XML json_a
                           RESULT hi = str_i.

    out->write( `JSON -> ABAP: Elementary type` ).
    out->write( str_i ).
    out->write( |\n| ).

    "Structure
    SELECT SINGLE carrid, carrname, currcode, url
      FROM zdemo_abap_carr
      WHERE carrid = 'AZ'
      INTO @DATA(carr_struc_c).

    "ABAP -> JSON
    DATA(json_wr_b) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    CALL TRANSFORMATION id SOURCE structure = carr_struc_c
                           RESULT XML json_wr_b.

    DATA(json_b) = cl_abap_conv_codepage=>create_in( )->convert( json_wr_b->get_output( ) ).
    out->write( `ABAP -> JSON: Structure` ).
    out->write( json_b ).
    out->write( |\n| ).

    "JSON -> ABAP
    DATA carr_struc_d LIKE carr_struc_c.
    CALL TRANSFORMATION id SOURCE XML json_b
                           RESULT structure = carr_struc_d.

    out->write( `JSON -> ABAP: Structure` ).
    out->write( carr_struc_d  ).
    out->write( |\n| ).

    "Internal table
    SELECT carrid, carrname, currcode, url
      FROM zdemo_abap_carr
      INTO TABLE @DATA(carr_tab_c)
      UP TO 2 ROWS.

    "ABAP -> JSON
    "This examples uses a cast to get access to further methods.
    DATA(json_wr_c) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    DATA(json_wr_cast) = CAST if_sxml_writer( json_wr_c ).

    "With the following method calls, the result is formatted.
    json_wr_cast->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    json_wr_cast->set_option( option = if_sxml_writer=>co_opt_indent ).

    CALL TRANSFORMATION id SOURCE itab = carr_tab_c
                           RESULT XML json_wr_c.

    DATA(json_c) = cl_abap_conv_codepage=>create_in( )->convert( json_wr_c->get_output( ) ).
    out->write( `ABAP -> JSON: Internal table` ).
    out->write( json_c ).
    out->write( |\n| ).

    "JSON -> ABAP
    DATA carr_tab_d LIKE carr_tab_c.

    CALL TRANSFORMATION id SOURCE XML json_c
                           RESULT itab = carr_tab_d.

    out->write( `JSON -> ABAP: Internal table` ).
    out->write( carr_tab_d ).
    out->write( |\n| ).

    "JSON -> XML
    DATA(str_j) =
    `{` &&
    `"flights": [` &&
    `    {` &&
    `        "carrier": "LH",` &&
    `        "connectionid": "400",` &&
    `        "from": "Frankfurt",` &&
    `        "to": "Berlin"` &&
    `    },` &&
    `    {` &&
    `        "carrier": "DL",` &&
    `        "connectionid": "400",` &&
    `        "from": "San Francisco",` &&
    `        "to": "New York"` &&
    `    }` &&
    `]` &&
    `}`.

    DATA(json_d) = cl_abap_conv_codepage=>create_out( )->convert( str_j ).
    DATA(json_wr_d) = cl_sxml_string_reader=>create( json_d ).
    DATA json2xml_ct TYPE xstring.

    "JSON -> XML using CALL TRANSFORMATION
    CALL TRANSFORMATION id SOURCE XML json_wr_d
                           RESULT XML json2xml_ct.

    DATA(conv_json2xml_ct) = cl_abap_conv_codepage=>create_in( )->convert( json2xml_ct ).

    out->write( `JSON -> XML using CALL TRANSFORMATION` ).
    out->write( format( conv_json2xml_ct ) ).
    out->write( |\n| ).

    "JSON -> XML using sXML
    DATA(reader_a) = cl_sxml_string_reader=>create( cl_abap_conv_codepage=>create_out(
                        )->convert( str_j ) ).
    "XML writer (note the type specification in contrast to the previous examples)
    DATA(xml_wr) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_xml10 ).

    TRY.
        reader_a->next_node( ).
        "The reader parses the data in one go by calling the SKIP_NODE method.
        "The data is passed to an XML writer.
        reader_a->skip_node( xml_wr ).
      CATCH cx_sxml_parse_error INTO DATA(err_pa).
        out->write( err_pa->get_text( ) ).
    ENDTRY.

    DATA(json2xml_sxml) = cl_abap_conv_codepage=>create_in( )->convert( xml_wr->get_output( ) ).
    out->write( `JSON -> XML using sXML` ).
    out->write( format( json2xml_sxml ) ).
    out->write( |\n| ).

    "XML -> JSON using CALL TRANSFORMATION
    DATA(json_wr_f) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    CALL TRANSFORMATION id SOURCE XML json2xml_sxml
                           RESULT XML json_wr_f.

    DATA(xml2json_ct) = cl_abap_conv_codepage=>create_in( )->convert( json_wr_f->get_output( ) ).
    out->write( `XML -> JSON using CALL TRANSFORMATION` ).
    out->write( xml2json_ct ).
    out->write( |\n| ).

    "XML -> JSON using sXML
    DATA(reader_b) = cl_sxml_string_reader=>create( cl_abap_conv_codepage=>create_out( )->convert( xml2json_ct ) ).
    DATA(json_wr) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    TRY.
        reader_b->next_node( ).
        reader_b->skip_node( json_wr ).
      CATCH cx_sxml_parse_error INTO DATA(err_xj).
        out->write( err_xj->get_text( ) ).
    ENDTRY.

    DATA(xml2json_sxml) = cl_abap_conv_codepage=>create_in( )->convert( json_wr->get_output( ) ).
    out->write( `XML -> JSON using sXML` ).
    out->write( xml2json_sxml ).

************************************************************************

    out->write( zcl_demo_abap_aux=>heading( `23) Handling JSON Data with XCO Classes` ) ).
    "Note: Unlike above, the following snippets do not work with asJSON as intermediate
    "format.

    DATA: BEGIN OF carrier_struc,
            carrier_id    TYPE c LENGTH 3,
            connection_id TYPE n LENGTH 4,
            city_from     TYPE c LENGTH 20,
            city_to       TYPE c LENGTH 20,
          END OF carrier_struc.

    DATA carriers_tab LIKE TABLE OF carrier_struc WITH EMPTY KEY.

    carrier_struc = VALUE #( carrier_id = 'AA' connection_id = '17' city_from = 'New York' city_to = 'San Francisco' ).
    carriers_tab = VALUE #( ( carrier_id = 'AZ' connection_id = '788' city_from = 'Rome' city_to = 'Tokyo' )
                            ( carrier_id = 'JL' connection_id = '408' city_from = 'Frankfurt' city_to = 'Tokyo' )
                            ( carrier_id = 'LH' connection_id = '2402' city_from = 'Frankfurt' city_to = 'Berlin' ) ).

    "ABAP (structure) -> JSON using XCO
    DATA(struc2json_xco) = xco_cp_json=>data->from_abap( carrier_struc )->to_string( ).
    out->write( `ABAP (structure) -> JSON using XCO` ).
    out->write( format( input = struc2json_xco xml = abap_false ) ).
    out->write( |\n| ).

    "ABAP (internal table) -> JSON using XCO
    DATA(itab2json_xco) = xco_cp_json=>data->from_abap( carriers_tab )->to_string( ).
    out->write( `ABAP (internal table) -> JSON using XCO` ).
    out->write( format( input = itab2json_xco xml = abap_false ) ).
    out->write( |\n| ).

    "JSON -> ABAP (structure) using XCO
    DATA json2struc_xco LIKE carrier_struc.
    xco_cp_json=>data->from_string( struc2json_xco )->write_to( REF #( json2struc_xco ) ).
    out->write( `JSON -> ABAP (structure) using XCO` ).
    out->write( json2struc_xco ).
    out->write( |\n| ).

    "JSON -> ABAP (internal table) using XCO
    DATA json2itab_xco LIKE carriers_tab.
    xco_cp_json=>data->from_string( itab2json_xco )->write_to( REF #( json2itab_xco ) ).
    out->write( `JSON -> ABAP (internal table) using XCO` ).
    out->write( json2itab_xco ).
    out->write( |\n| ).

    "Creating JSON using XCO
    "Check out more methods that offer more options to build the JSON by clicking
    "CTRL + Space after '->' in ADT.
    DATA(json_builder_xco) = xco_cp_json=>data->builder( ).
    json_builder_xco->begin_object(
      )->add_member( 'CarrierId' )->add_string( 'DL'
      )->add_member( 'ConnectionId' )->add_string( '1984'
      )->add_member( 'CityFrom' )->add_string( 'San Francisco'
      )->add_member( 'CityTo' )->add_string( 'New York'
      )->end_object( ).

    "Getting JSON data
    DATA(json_created_xco) = json_builder_xco->get_data( )->to_string( ).

    out->write( `Creating JSON using XCO` ).
    out->write( format( input = json_created_xco xml = abap_false ) ).
    out->write( |\n| ).

    "Transforming the created JSON to ABAP (structure)
    "Note: The JSON was intentionally created without the underscores in the
    "name to demonstrate the 'apply' method. The following example demonstrates
    "a transformation of camel case and underscore notation. As above, check out
    "more options by clicking CTRL + Space after '...transformation->'.
    CLEAR json2struc_xco.
    xco_cp_json=>data->from_string( json_created_xco )->apply( VALUE #(
      ( xco_cp_json=>transformation->pascal_case_to_underscore ) ) )->write_to( REF #( json2struc_xco ) ).

    out->write( `JSON -> ABAP (structure) using XCO demonstrating the apply method` ).
    out->write( json2struc_xco ).

************************************************************************

    out->write( zcl_demo_abap_aux=>heading( `24) Handling JSON Data with the  /ui2/cl_json Class` ) ).

    TYPES: BEGIN OF demo_struc,
             carrier_id    TYPE c LENGTH 3,
             connection_id TYPE n LENGTH 4,
             city_from     TYPE c LENGTH 20,
             city_to       TYPE c LENGTH 20,
           END OF demo_struc.
    DATA itab TYPE TABLE OF demo_struc WITH EMPTY KEY.
    itab = VALUE #( ( carrier_id = 'AA' connection_id = '0017' city_from = 'New York' city_to = 'San Francisco' )
                    ( carrier_id = 'AZ' connection_id = '0789' city_from = 'Tokyo' city_to = 'Rome' ) ).

    "---------------- Serializing ----------------

    DATA(abap_to_json) = /ui2/cl_json=>serialize( data = itab ).
    "Note the many additional, optional parameters such as for formatting the
    "serialized JSON. For more information, see the class documentation.
    DATA(abap_to_json_pretty) = /ui2/cl_json=>serialize( data = itab
                                                         format_output = abap_true ).
    DATA(abap_to_json_pretty_name) = /ui2/cl_json=>serialize( data = itab
                                                              format_output = abap_true
                                                              pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    out->write( `---------- ABAP -> JSON ----------` ).
    out->write( abap_to_json ).
    out->write( |\n| ).
    out->write( `---------- ABAP -> JSON (pretty printed) ----------` ).
    out->write( abap_to_json_pretty ).
    out->write( |\n| ).
    out->write( `---------- ABAP -> JSON (camel case) ----------` ).
    out->write( abap_to_json_pretty_name ).
    out->write( |\n| ).

    "---------------- Deserializing ----------------

    DATA(json_to_abap) = abap_to_json.
    DATA itab_json_to_abap LIKE itab.

    /ui2/cl_json=>deserialize( EXPORTING json = json_to_abap
                               CHANGING data  = itab_json_to_abap ).

    out->write( `---------- JSON -> ABAP ----------` ).
    out->write( itab_json_to_abap ).
    out->write( |\n| ).

    "---------------- Deserializing: Applying name mapping ----------------
    "Creating an internal table with different field names
    TYPES: BEGIN OF demo_struc4map,
             carr TYPE c LENGTH 3,
             conn TYPE n LENGTH 4,
             from TYPE c LENGTH 20,
             to   TYPE c LENGTH 20,
           END OF demo_struc4map.
    DATA itab4map TYPE TABLE OF demo_struc4map WITH EMPTY KEY.

    /ui2/cl_json=>deserialize( EXPORTING json = json_to_abap
                               name_mappings = VALUE #( ( abap = 'CARR' json = `CARRIER_ID` )
                                                        ( abap = 'CONN' json = `CONNECTION_ID` )
                                                        ( abap = 'FROM' json = `CITY_FROM` )
                                                        ( abap = 'TO' json = `CITY_TO` ) )
                               CHANGING data  = itab4map ).

    out->write( `---------- JSON -> ABAP (Name mapping) ----------` ).
    out->write( itab4map ).
    out->write( |\n| ).

    "---------------- Deserializing: Using JSON as xstring ----------------

    DATA(json_xstring) = cl_abap_conv_codepage=>create_out( )->convert(
   `[` &&
   `    {` &&
   `        "carrier_id": "LH",` &&
   `        "connection_id": "400",` &&
   `        "city_from": "Frankfurt",` &&
   `        "city_to": "Berlin"` &&
   `    },` &&
   `    {` &&
   `        "carrier_id": "DL",` &&
   `        "connection_id": "1984",` &&
   `        "city_from": "San Francisco",` &&
   `        "city_to": "New York"` &&
   `    },` &&
   `    {` &&
   `        "carrier_id": "AZ",` &&
   `        "connection_id": "790",` &&
   `        "city_from": "Rome",` &&
   `        "city_to": "Osaka"` &&
   `    }` &&
   `]` ).

    DATA itab_json_xstr_to_abap LIKE itab.
    /ui2/cl_json=>deserialize( EXPORTING jsonx = json_xstring
                               CHANGING data  = itab_json_xstr_to_abap ).

    out->write( `---------- JSON (xstring) -> ABAP ----------` ).
    out->write( itab_json_xstr_to_abap ).
    out->write( |\n| ).

    "---------------- Deserializing: No equivalent ABAP type available ----------------

    "The example assumes that there is no equivalent ABAP type available for JSON data
    "that is to be deserialized. You can use the 'generate' method that has a
    "returning parameter of the generic type 'ref to data'.
    DATA(json) = `[{"CARRIER_ID":"AA","CONNECTION_ID":17,"CITY_FROM":"New York","CITY_TO":"San Francisco"},` &&
                 `{"CARRIER_ID":"AZ","CONNECTION_ID":789,"CITY_FROM":"Tokyo","CITY_TO":"Rome"}]`.

    DATA(dref) = /ui2/cl_json=>generate( json = json ).
    DATA(dref_xstr) = /ui2/cl_json=>generate( jsonx = json_xstring ).

    "You can further process the content, for example, with RTTS as outlined in the
    "Dynamic Programming cheat sheet.
    IF dref IS BOUND.
      out->write( `---------- JSON -> ABAP (unknown type) ----------` ).
      out->write( dref->* ).
      out->write( |\n| ).
    ENDIF.

    IF dref_xstr IS BOUND.
      out->write( `---------- JSON (xstring) -> ABAP (unknown type) ----------` ).
      out->write( dref_xstr->* ).
    ENDIF.

************************************************************************

    out->write( zcl_demo_abap_aux=>heading( `25) Excursion: Compressing and Decompressing Binary Data` ) ).
    "You may want to process or store binary data. The data can be very large.
    "You can compress the data in gzip format and decompress it for further processing using
    "the cl_abap_gzip class. Check out appropriate exceptions to be caught. The simple example
    "just specifies cx_root. See the class documentation for more information.
    "This example uses a data object of type xstring from a previous example.

    "Compressing binary data
    DATA xstr_comp TYPE xstring.
    TRY.
        cl_abap_gzip=>compress_binary( EXPORTING raw_in   = xml_oref_a
                                       IMPORTING gzip_out = xstr_comp ).
      CATCH cx_root INTO DATA(error_comp).
        out->write( error_comp->get_text( ) ).
    ENDTRY.

    "Decompressing binary data
    DATA xstr_decomp TYPE xstring.
    TRY.
        cl_abap_gzip=>decompress_binary( EXPORTING gzip_in = xstr_comp
                                         IMPORTING raw_out = xstr_decomp ).

      CATCH cx_root INTO DATA(error_decomp).
        out->write( error_decomp->get_text( ) ).
    ENDTRY.

    "Checking the xstring length of the variables used and comparing the result
    DATA(strlen_original_xstring) = xstrlen( xml_oref_a ).
    out->write( |Length of original binary data object: { strlen_original_xstring }| ).
    DATA(strlen_comp) = xstrlen( xstr_comp ).
    out->write( |Length of compressed binary data object: { strlen_comp }| ).
    DATA(strlen_decomp) = xstrlen( xstr_decomp ).
    out->write( |Length of decompressed binary data object: { strlen_decomp }| ).
    IF xml_oref_a = xstr_decomp.
      out->write( `The decompressed binary data object has the same value as the original binary data object.` ).
    ENDIF.

  ENDMETHOD.
  METHOD format.
    TRY.
        DATA(xstr) = cl_abap_conv_codepage=>create_out( )->convert( input ).
        DATA(reader) = cl_sxml_string_reader=>create( xstr ).
        DATA(writer) = CAST if_sxml_writer( cl_sxml_string_writer=>create(
          type = COND #( WHEN xml = abap_true THEN  if_sxml=>co_xt_xml10 ELSE if_sxml=>co_xt_json ) ) ).
        writer->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
        writer->set_option( option = if_sxml_writer=>co_opt_indent ).
        reader->next_node( ).
        reader->skip_node( writer ).
        string = cl_abap_conv_codepage=>create_in( )->convert( CAST cl_sxml_string_writer( writer )->get_output( ) ).
      CATCH cx_root.
        string = `Issue when formatting.`.
    ENDTRY.

  ENDMETHOD.

  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.

  METHOD concatenate_string.
    attr_concat_string = attr_string_a && attr_string_b.
  ENDMETHOD.

  METHOD deserialize_helper.
    me->attr_string_a = attr_string_a.
    me->attr_string_b = attr_string_b.
    me->attr_concat_string = attr_concat_string.
  ENDMETHOD.

  METHOD serialize_helper.
    attr_string_a = me->attr_string_a.
    attr_string_b = me->attr_string_b.
    attr_concat_string = me->attr_concat_string.
  ENDMETHOD.

  METHOD lowercase_string.
    attr_lowercase_str = to_lower( attr_string_a && attr_string_b ).
  ENDMETHOD.

ENDCLASS.
