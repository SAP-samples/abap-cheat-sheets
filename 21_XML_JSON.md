<a name="top"></a>

# Working with XML and JSON in ABAP

- [Working with XML and JSON in ABAP](#working-with-xml-and-json-in-abap)
  - [Introduction](#introduction)
  - [Working with XML](#working-with-xml)
    - [Processing XML Using Class Libraries](#processing-xml-using-class-libraries)
      - [iXML](#ixml)
      - [sXML](#sxml)
    - [XML Transformations](#xml-transformations)
    - [CALL TRANSFORMATION Syntax](#call-transformation-syntax)
      - [Exploring CALL TRANSFORMATION Syntax Using the Predefined Identity Transformation ID](#exploring-call-transformation-syntax-using-the-predefined-identity-transformation-id)
  - [Working with JSON](#working-with-json)
    - [Creating and Reading JSON Data Using sXML](#creating-and-reading-json-data-using-sxml)
    - [Transforming JSON Data Using Transformations](#transforming-json-data-using-transformations)
    - [Handling JSON Data with ABAP Classes](#handling-json-data-with-abap-classes)
  - [Excursions](#excursions)
    - [Serializing and Deserializing Objects](#serializing-and-deserializing-objects)
    - [Converting string \<-\> xstring](#converting-string---xstring)
    - [Compressing and Decompressing Binary Data](#compressing-and-decompressing-binary-data)
    - [Exporting and Importing Data Clusters](#exporting-and-importing-data-clusters)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)
  
## Introduction

This cheat sheet provides a high-level overview of working with XML and JSON in ABAP. It covers the following topics: 
- Processing XML using class libraries (iXML, sXML)
- XML Transformations using XSLT and Simple Transformations (ST), i.e. serializations (ABAP to XML) and deserializations (XML to ABAP)
- `CALL TRANSFORMATION` syntax
- Working with JSON data

> [!NOTE]  
> - The cheat sheet snippets and the executable example cover simple cases. Find more executable examples of the ABAP Keyword Documentation following the links in the [More Information](#more-information) section. 
> - For more detailed information, such as documentation on ST syntax, what asXML is, etc., also see the links in the [More Information](#more-information) section.


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Working with XML
### Processing XML Using Class Libraries

#### iXML
- *Integrated* XML Library
- Provides methods for ...
  - validating and parsing (i.e. reading) XML data in 1.0 format into a DOM representation (Document Object Model, which is a tree-like representation of the documents in the memory) 
  - processing the DOM representation (you can create or edit XML data)
  - rendering (i.e. writing) the DOM representation 
- To access XML data, you need input and output streams that are created using iXML methods. 
- Advantages: Easy access to the individual parts of an XML document possible, DTDs (Document Type Definitions) are possible
- Disadvantages: High memory consumption of the DOM (if the complete DOM is created)

The following code snippets demonstrate a selection of iXML methods for handling XML data. Note that the snippets use classes available for [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm). Find documentation on the classes for [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm) [here (F1 documentation for Standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_ixml_lib.htm).

Creating XML data using iXML:

```abap
"Creating one factory object of the access class cl_ixml_core using the
"create method. It is used to access the iXML library.
DATA(ixml_cr)     = cl_ixml_core=>create( ).

"Creating an XML document stored in DOM format in the memory
DATA(document_cr) = ixml_cr->create_document( ).

"Creating DOM nodes step by step. Here, elements, attributes, and content are
"and inserted into the XML. Using an appropriate attribute setting, the result
"represents XML data in the asXML format (see more information further down).

"Creating a root node
DATA(root) = document_cr->create_element_ns( name = 'abap'
                                              prefix = 'asx' ).
root->set_attribute_ns( name =  'asx'
                        prefix = 'xmlns'
                        value = 'http://www.sap.com/abapxml' ).
root->set_attribute_ns( name =  'version'
                        value = '1.0' ).
document_cr->append_child( root ).

DATA(xml_node1) = document_cr->create_element_ns( prefix = 'asx'
                                                  name   = 'values' ).
root->append_child( xml_node1 ).
DATA(xml_node2) = document_cr->create_element_ns( name = 'STRING' ).
xml_node1->append_child( xml_node2 ).
xml_node2->append_child( document_cr->create_text( 'Hello World' ) ).

"Creating a renderer for rendering the XML document into an output stream
"that is used to pass XML data from the renderer.
DATA xml_doc TYPE xstring.
ixml_cr->create_renderer( document = document_cr
                          ostream  = ixml_cr->create_stream_factory( )->create_ostream_xstring( string = xml_doc )
                        )->render( ).

"Result (xstring converted to string):
"<?xml version="1.0"?><asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
"<asx:values><STRING>Hello World</STRING></asx:values></asx:abap>
```

Parsing XML data using iXML:

```abap
"Creating demo XML data to be used in the example as string. Using the cl_abap_conv_codepage
"class, you can convert the string to xstring which is required for the example.
DATA(some_xml) = cl_abap_conv_codepage=>create_out( )->convert(
      `<?xml version="1.0"?>` &&
      `<node attr_a="123">` &&
      ` <subnode1>` &&
      `  <hallo>hi</hallo>` &&
      ` </subnode1>` &&
      ` <subnode2>` &&
      `  <letter>a</letter>` &&
      `  <date format="mm-dd-yyyy">01-01-2025</date>` &&
      ` </subnode2>` &&
      ` <subnode3>`  &&
      `  <text attr_b="1" attr_c="a">abc</text>` &&
      `  <text attr_b="2" attr_c="b">def</text>` &&
      `  <text attr_b="3" attr_c="c">ghi</text>` &&
      `  <text attr_b="4" attr_c="d">jkl</text>` &&
      ` </subnode3>` &&
      `</node>` ).

"Creating one factory object of the access class cl_ixml_core using the
"create method. It is used to access the iXML library.
DATA(ixml_pa) = cl_ixml_core=>create( ).
"Creaing an input stream that is used for the input of XML data
DATA(stream_factory_pa) = ixml_pa->create_stream_factory( ).
"Creating an XML document stored in DOM format in the memory
DATA(document_pa) = ixml_pa->create_document( ).
"Creating a parser. It requires the following input parameters: input stream to be parsed,
"the XML document to which the stream is parsed, a factory required to create a stream
DATA(parser_pa) = ixml_pa->create_parser(
                    istream = stream_factory_pa->create_istream_xstring( string = some_xml )
                    document = document_pa
                    stream_factory = stream_factory_pa ).

"Parsing XML data to a DOM representation in one go. It is put in the memory.
"Note: You can also parse sequentially, and not in one go.
DATA(parsing_check) = parser_pa->parse( ).
IF parsing_check = 0. "Parsing was successful

  "--------------------- Directly reading nodes ----------------------

  "Accessing the root element of the DOM. It can be used as the initial node
  "for accessing subnodes.
  "Note: Multiple methods are available to further process the nodes.
  DATA(root_element) = document_pa->get_root_element( ).
  "First subnode
  DATA(child_element) = root_element->get_first_child( ).
  "Gettng the value of that node
  DATA(child_element_value) = child_element->get_value( ).
  "Next adjacent node/getting the value
  DATA(next_element) = child_element->get_next( ).
  DATA(next_element_value) = next_element->get_value( ).
  "Direct access using element names
  "Result: First element searched for
  DATA(element_by_name) = document_pa->find_from_name( name = `letter` )->get_value( ).

  "--------------------- Reading using iterators ----------------------

  "i.e. going over the XML nodes one after another

  "Creating an iterator
  DATA(iterator_pa) = document_pa->create_iterator( ).


  DATA xml_nodes TYPE string_table.

  DATA(node_pa) = iterator_pa->get_next( ).
  "For the iteration, you can use the get_next method to process the nodes one after another.
  "Note: Here, all nodes are respected. You can also create filters to go over specific nodes.
  WHILE NOT node_pa IS INITIAL.
    "DATA(indent) = node_pa->get_height( ) * 2.
    "Rettrieving the node type
    DATA(node_type) = node_pa->get_type( ).
    CASE node_type.
      WHEN if_ixml_node=>co_node_element.
        APPEND |Element: "{ node_pa->get_name( ) }"| TO xml_nodes.
        "Retrieving attributes
        DATA(attributes_pa) = node_pa->get_attributes( ).
        IF NOT attributes_pa IS INITIAL.
          DO attributes_pa->get_length( ) TIMES.
            DATA(attr) = attributes_pa->get_item( sy-index - 1 ).
            APPEND |Attribute: "{ attr->get_name( ) } = { attr->get_value( ) }"| TO xml_nodes.
          ENDDO.
        ENDIF.
      WHEN if_ixml_node=>co_node_text OR if_ixml_node=>co_node_cdata_section.
        APPEND |Text: "{ node_pa->get_value( ) }"| TO xml_nodes.

        "When iterating the XML nodes, you may want to perform certain actions, e.g.
        "modifying values etc. Check out the many methods that are available.
        "This example just transform the textual content to uppercase using the set_value method.
        DATA(val) = to_upper( node_pa->get_value( ) ).
        node_pa->set_value( val ).
      WHEN OTHERS.
        ...
    ENDCASE.
    "Retrieving the next node
    node_pa = iterator_pa->get_next( ).
  ENDWHILE.

  "Creating a renderer. It renders the XML document into an output stream
  "that is used to pass XML data from the renderer.
  DATA xml_pa TYPE xstring.
  ixml_pa->create_renderer( document = document_pa
                            ostream = ixml_pa->create_stream_factory( )->create_ostream_xstring( string = xml_pa )
                          )->render( ).

ELSE.
  "Parsing was not successful.
  ...
ENDIF.

DATA(conv_string) = cl_abap_conv_codepage=>create_in( )->convert( xml_pa ).

*--- xml_nodes ---
*Element: "node"
*Attribute: "attr_a = 123"
*Element: "subnode1"
*Element: "hallo"
*Text: "hi"
*Element: "subnode2"
*Element: "letter"
*Text: "a"
*Element: "date"
*Attribute: "format = mm-dd-yyyy"
*Text: "01-01-2025"
*Element: "subnode3"
*Element: "text"
*Attribute: "attr_b = 1"
*Attribute: "attr_c = a"
*Text: "abc"
*Element: "text"
*Attribute: "attr_b = 2"
*Attribute: "attr_c = b"
*Text: "def"
*Element: "text"
*Attribute: "attr_b = 3"
*Attribute: "attr_c = c"
*Text: "ghi"
*Element: "text"
*Attribute: "attr_b = 4"
*Attribute: "attr_c = d"
*Text: "jkl"

*--- conv_string ---
*<?xml version="1.0" encoding="utf-8"?><node attr_a="123"><subnode1><hallo>HI</hallo></subnode1><subnode2><letter>A</letter>
*<date format="mm-dd-yyyy">01-01-2025</date></subnode2><subnode3><text attr_b="1" attr_c="a">ABC</text>
*<text attr_b="2" attr_c="b">DEF</text><text attr_b="3" attr_c="c">GHI</text><text attr_b="4" attr_c="d">JKL</text></subnode3></node>
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### sXML
- *Serial* XML Library 
- Provides XML readers and writers for different sources and targets
- Processes XML data sequentially
- Allows you to parse and render XML data in a token-based (iterating across all nodes, i.e. tokens, in the tree structure of the XML data) and object-oriented way (wrapping methods of the token-based access; providing an object-oriented way to access XML nodes)
- In contrast to iXML, ...
  - more formats are possible (standard XML 1.0, but also XOP, binary XML, and JSON).
  - no document is created in DOM format (if you do not need a DOM representation and DTDs, sXML is a more performant alternative to iXML).

Creating XML data using sXML:

```abap
*&---------------------------------------------------------------------*
*& Token-based rendering
*&---------------------------------------------------------------------*

"For sXML, there are specialized writer classes, such as CL_SXML_STRING_WRITE.
"Writers created with this class render XML data to a byte string.
"The XML 1.0 format and UTF-8 are used by default in the create method.
"Here, the parameters are specified explicitly.
"Note: The interface IF_SXML_WRITER contains the components that are valid
"for all readers (the abstract super class CL_SXML_WRITER includes this
"interface as well as implementations for all readers). In the example below,
"a cast is required so as to access special methods (such as open_element).

DATA(writer) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type     = if_sxml=>co_xt_xml10
                                                                   encoding = 'UTF-8' ) ).

"Creating/Rendering XML data using token-based rendering
"The example shows shows how XML data can be created using various available methods.
"Here, token-based rendering is used, i.e. each node is added to the XML data using 
"a method. There is a method for each node type.
TRY.
    "Creating nodes (the order of the nodes is crucial)
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
  CATCH cx_sxml_state_error.
    ...
ENDTRY.

"The XML data can be retrieved with the GET_OUTPUT method.
"Also here, a cast is required.
"The result is of type xstring.
DATA(xml) = CAST cl_sxml_string_writer( writer )->get_output(  ).

"The result looks like this (when converted to string and indented):
"<?xml version="1.0" encoding="utf-8"?>
"<flights>
"  <flight>
"    <carrier>LH</carrier>
"    <flightnumber>400</flightnumber>
"  </flight>
"  <flight>
"    <carrier>DL</carrier>
"    <flightnumber>1984</flightnumber>
"  </flight>
"</flights>

*&---------------------------------------------------------------------*
*& Object-oriented rendering
*&---------------------------------------------------------------------*

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
  CATCH cx_sxml_state_error.
    ...
ENDTRY.

DATA(xml_oo) =  CAST cl_sxml_string_writer( writer_oo )->get_output( ).

"The result looks like this (when converted to string and indented):
"<flights>
" <flight>
"  <carrier>AZ</carrier>
"  <flightnumber>788</flightnumber>
" </flight>
" <flight>
"  <carrier>JL</carrier>
"  <flightnumber>407</flightnumber>
" </flight>
"</flights>
```

Parsing XML data using sXML:

```abap
*&---------------------------------------------------------------------*
*& Token-based parsing
*&---------------------------------------------------------------------*

"Creating demo XML data to be used in the example
TRY.
    DATA(xml_to_parse) = cl_abap_conv_codepage=>create_out( )->convert(
      `<?xml version="1.0"?>` &&
      `<node attr_a="123">` &&
      ` <subnode1>` &&
      `  <hallo>hi</hallo>` &&
      ` </subnode1>` &&
      ` <subnode2>` &&
      `  <letter>a</letter>` &&
      `  <date format="mm-dd-yyyy">01-01-2025</date>` &&
      ` </subnode2>` &&
      ` <subnode3>`  &&
      `  <text attr_b="1" attr_c="a">abc</text>` &&
      `  <text attr_b="2" attr_c="b">def</text>` &&
      `  <text attr_b="3" attr_c="c">ghi</text>` &&
      `  <text attr_b="4" attr_c="d">jkl</text>` &&
      ` </subnode3>` &&
      `</node>` ).
  CATCH cx_sy_conversion_codepage.
ENDTRY.

"Creating an internal table for demonstration purposes
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
  CATCH cx_sxml_state_error INTO DATA(error_parse_token).
    DATA(error_text) = error_parse_token->get_text( ).
ENDTRY.

*--- nodes_tab ---
*NODE_TYPE              NAME        VALUE     
*CO_NT_ELEMENT_OPEN     node                  
*CO_NT_ATTRIBUTE        attr_a      123       
*CO_NT_ELEMENT_OPEN     subnode1              
*CO_NT_ELEMENT_OPEN     hallo                 
*CO_NT_VALUE            hallo       hi        
*CO_NT_ELEMENT_CLOSE    hallo                 
*CO_NT_ELEMENT_CLOSE    subnode1              
*CO_NT_ELEMENT_OPEN     subnode2              
*CO_NT_ELEMENT_OPEN     letter                
*CO_NT_VALUE            letter      a         
*CO_NT_ELEMENT_CLOSE    letter                
*CO_NT_ELEMENT_OPEN     date                  
*CO_NT_ATTRIBUTE        format      mm-dd-yyyy
*CO_NT_VALUE            date        01-01-2025
*CO_NT_ELEMENT_CLOSE    date                  
*CO_NT_ELEMENT_CLOSE    subnode2              
*CO_NT_ELEMENT_OPEN     subnode3              
*CO_NT_ELEMENT_OPEN     text                  
*CO_NT_ATTRIBUTE        attr_b      1         
*CO_NT_ATTRIBUTE        attr_c      a         
*CO_NT_VALUE            text        abc       
*CO_NT_ELEMENT_CLOSE    text                  
*CO_NT_ELEMENT_OPEN     text                  
*CO_NT_ATTRIBUTE        attr_b      2         
*CO_NT_ATTRIBUTE        attr_c      b         
*CO_NT_VALUE            text        def       
*CO_NT_ELEMENT_CLOSE    text                  
*CO_NT_ELEMENT_OPEN     text                  
*CO_NT_ATTRIBUTE        attr_b      3         
*CO_NT_ATTRIBUTE        attr_c      c         
*CO_NT_VALUE            text        ghi       
*CO_NT_ELEMENT_CLOSE    text                  
*CO_NT_ELEMENT_OPEN     text                  
*CO_NT_ATTRIBUTE        attr_b      4         
*CO_NT_ATTRIBUTE        attr_c      d         
*CO_NT_VALUE            text        jkl       
*CO_NT_ELEMENT_CLOSE    text                  
*CO_NT_ELEMENT_CLOSE    subnode3              
*CO_NT_ELEMENT_CLOSE    node                

*&---------------------------------------------------------------------*
*& Object-oriented parsing
*&---------------------------------------------------------------------*

"The example uses the XML from above
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
                            name      = attribute->qname-name
                            value  = SWITCH #( attribute->value_type WHEN if_sxml_value=>co_vt_text THEN attribute->get_value( ) )
                          ) TO nodes_tab.
          ENDLOOP.

        WHEN  if_sxml_node=>co_nt_element_close.
          DATA(close_element) = CAST if_sxml_close_element( node_oo ).

          APPEND VALUE #( node_type = `close element`
                          name      = close_element->qname-name
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
    DATA(error_parse_oo_text) = error_parse_oo->get_text( ).
ENDTRY.

*--- nodes_tab ---
*NODE_TYPE        NAME        VALUE     
*open element     node                  
*attribute        attr_a      123       
*open element     subnode1              
*open element     hallo                 
*value                        hi        
*close element    hallo                 
*close element    subnode1              
*open element     subnode2              
*open element     letter                
*value                        a         
*close element    letter                
*open element     date                  
*attribute        format      mm-dd-yyyy
*value                        01-01-2025
*close element    date                  
*close element    subnode2              
*open element     subnode3              
*open element     text                  
*attribute        attr_b      1         
*attribute        attr_c      a         
*value                        abc       
*close element    text                  
*open element     text                  
*attribute        attr_b      2         
*attribute        attr_c      b         
*value                        def       
*close element    text                  
*open element     text                  
*attribute        attr_b      3         
*attribute        attr_c      c         
*value                        ghi       
*close element    text                  
*open element     text                  
*attribute        attr_b      4         
*attribute        attr_c      d         
*value                        jkl       
*close element    text                  
*close element    subnode3              
*close element    node              
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### XML Transformations

To perform transformations in ABAP, you can use:

- XSL Transformations (XSLT)
  - Repository objects written in XSLT
  - XSLT can process ABAP data because ABAP data is first implicitly transformed into the asXML format. The result of this implicit transformation (the asXML format) is then used as the actual source for the XSL transformation.
- Identity transformations
  - Predefined XSL transformation with the name `ID`
  - Used to read and write the asXML (and also asJSON) format. 
  - When the transformation ID is called, the resulting intermediate formats (asXML, asJSON) are the direct output.
  - When used, for example, to transform ABAP data, such as a structure, to XML, the transformation does not change the structure itself. You can, however, change the structure by implementing your own XSLT.
- Simple Transformation (ST)
  - Repository objects written in an SAP-specific programming language for transformations between XML formats and ABAP data

The tranformations are called using `CALL TRANSFORMATION` statements.

Possible transformations, some of which are covered in the example: 

| Transformation  | XSLT  | ST |
|---|---|---|
| XML <-> XML  | X  | - |
| ABAP <-> XML | X  | X |
| ABAP <-> ABAP | X  | - |

> [!NOTE]  
> - asXML:
>   - *ABAP Serialization XML*
>   - Describes a format of XML data created when serializing ABAP data (ABAP -> XML) with the identity transformation 
>   - This format is a prerequisite for deserializations (XML -> ABAP) using identity transformations.
>   - Used as an intermediate format that defines a mapping between ABAP data and XML
>   - Therefore, if you want to deserialize XML data in ABAP, you must first transform it to the asXML format.
> - Make sure that you use appropriate exception classes for potential errors in transformations. See the section [Catchable Exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_transformation.htm) in the `CALL TRANSFORMATION` topic.
> - For serializing instances of classes, find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenasxml_class_instances.htm) in the ABAP Keyword Documentation. The classes must implement the `IF_SERIALIZABLE_OBJECT` interface (find a demo in the executable example and further down). 

<p align="right"><a href="#top">⬆️ back to top</a></p>

### CALL TRANSFORMATION Syntax

The following code snippets demonstrate a selection of possible syntax options when using [`CALL TRANSFORMATION`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_transformation.htm) statements.

> [!NOTE]  
> You can also transform ABAP to and from JSON data using transformations. Find examples in the [Transforming JSON Data Using Transformations](#transforming-json-data-using-transformations) section.



**Specifying transformations**

```abap
"Specifying the name of an XSL or Simple Transformation statically
CALL TRANSFORMATION zsome_transformation SOURCE ...
                                         RESULT ...

"Specifying the identity transformation with the predefined name ID
CALL TRANSFORMATION id SOURCE ...
                       RESULT ...

"Dynamic specification of the transformation
"If the transformation does not exist, an exception is raised.
TRY.
  CALL TRANSFORMATION ('ID') SOURCE ...
                             RESULT ...
CATCH cx_invalid_transformation.
  ...
ENDTRY.
```

**Specifying the source of the transformation**

There are multiple options. Check the executable example to explore them.

```abap
*&---------------------------------------------------------------------*
*& Transforming XML data
*&---------------------------------------------------------------------*

"Options for src: 
"- Data object of type string or xstring containing XML data in XML 1.0 format
"- Standard table with flat character-like or byte-like line type
"- Some references to iXML and sXML libraries are possible. For example, an 
"  interface reference variable of type if_sxml_reader.
CALL TRANSFORMATION ... SOURCE XML src
                        RESULT ...

*&---------------------------------------------------------------------*
*& Transforming ABAP data
*&---------------------------------------------------------------------*

"No XML specified after SOURCE
"Options after SOURCE: 
"- One or multiple ABAP data objects (abap1 in the snippet) can be specified as 
"  static parameter list, e.g. z1 = abap1 z2 = abap2 ...
"- When XSLT is used (such as ID), the data is serialized to asXML depending on 
"  the target
"- z1 etc. stands for names of XML elements
CALL TRANSFORMATION ... SOURCE z1 = abap1 ...
                        RESULT ...

"Dynamic specification of ABAP data objects in an internal table of type 
"abap_trans_srcbind_tab
DATA(srctab) = VALUE abap_trans_srcbind_tab( ( name = 'A' value = REF #( b ) ) ( ... ) ).
CALL TRANSFORMATION ... SOURCE (srctab)
                        RESULT ...
```

**Specifying the result of the transformation**

There are multiple options. Check the executable example to explore them in action.

```abap
*&---------------------------------------------------------------------*
*& Transforming to XML data
*&---------------------------------------------------------------------*

"Options for res: 
"- Data object of type string or xstring
"- Data object declared inline (e.g. DATA(a) or FINAL(b)), which has then the type xstring
"- Standard table with flat character-like or byte-like line type
"- References for objects in iXML and sXML (e.g. sXML writers)
CALL TRANSFORMATION ... SOURCE ...
                        RESULT XML res.

*&---------------------------------------------------------------------*
*& Transforming to ABAP data
*&---------------------------------------------------------------------*

"No XML specified after RESULT
"Similar to above, multiple ABAP data objects can be specified as a static parameter list.
CALL TRANSFORMATION ... SOURCE ...
                        RESULT y1 = abap1 ...

"An internal table of type abap_trans_resbind_tab can be specified.
DATA(restab) = VALUE abap_trans_resbind_tab( ( name = 'A' value = REF #( b ) ) ( ... ) ).       
CALL TRANSFORMATION ... SOURCE ...
                        RESULT (restab).
```

> [!NOTE]  
> More additions are available such as `PARAMETERS` (for parameter binding) and `OPTIONS` (for predefined transformation options). See the details in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_transformation.htm).

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Exploring CALL TRANSFORMATION Syntax Using the Predefined Identity Transformation ID

The following example explores various syntax options for `CALL TRANSFORMATION` statements, using the predefined identity transformation `ID`.

To try the example out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    TYPES: BEGIN OF line,
             char TYPE c LENGTH 3,
             int  TYPE i,
           END OF line,
           t_type TYPE TABLE OF line WITH EMPTY KEY.
    DATA(tab) = VALUE t_type( ( char = 'aaa' int = 1 )
                              ( char = 'bbb' int = 2 )
                              ( char = 'ccc' int = 3 ) ).

    out->write( `ABAP <-> XML using XSLT (Using the Predefined Identity Transformation ID)` ).

**********************************************************************

    out->write( `-------- Transforming ABAP data: ... SOURCE ... --------` ).
    "This section covers syntax options after SOURCE without XML.

    "ABAP (specifying ABAP data object) -> asXML/xstring
    "itab stands for the name of the XML element (check ... <ITAB> ... in the output)
    CALL TRANSFORMATION id SOURCE itab = tab
                           RESULT XML DATA(xml_tab_a).

    DATA(xml_a) = cl_abap_conv_codepage=>create_in( )->convert( xml_tab_a ).
    out->write( data = xml_a name = `xml_a` ).

    "ABAP (specifying ABAP data objects dynamically using an internal table
    "of type abap_trans_srcbind_tab) -> asXML/xstring

    DATA(srctab) = VALUE abap_trans_srcbind_tab( ( name = 'ITAB' value = REF #( tab ) ) ).
    CALL TRANSFORMATION id SOURCE (srctab)
                           RESULT XML DATA(xml_tab_b).

    DATA(xml_b) = cl_abap_conv_codepage=>create_in( )->convert( xml_tab_b ).
    out->write( data = xml_b name = `xml_b` ).

**********************************************************************

    out->write( `-------- Transforming XML data: ... SOURCE XML ... --------` ).
    "This section covers syntax options after SOURCE XML.
    "Apart from data objects of type string or xstring containing XML data
    "in XML 1.0 format, standard tables with flat character-like or byte-like
    "line type, you can also specify certain references to iXML and sXML libraries

    "sxml/ixml references as source
    "The examples use XML data from above
    DATA(sxml_reader) = cl_sxml_string_reader=>create( xml_tab_a ).

    CALL TRANSFORMATION id SOURCE XML sxml_reader
                           RESULT XML DATA(xml_tab_c).

    DATA(xml_c) = cl_abap_conv_codepage=>create_in( )->convert( xml_tab_c ).
    out->write( data = xml_c name = `xml_c` ).

    DATA(ixmlr) = cl_ixml_core=>create( ).
    DATA(ixml_doc1) = ixmlr->create_document( ).
    DATA(ixml_cr_streamr) = ixmlr->create_stream_factory( ).
    DATA(ixml_i_stream) = ixml_cr_streamr->create_istream_xstring( xml_tab_a ).

    CALL TRANSFORMATION id SOURCE XML ixml_i_stream
                           RESULT XML DATA(xml_tab_d).

    DATA(xml_d) = cl_abap_conv_codepage=>create_in( )->convert( xml_tab_d ).
    out->write( data = xml_d name = `xml_d` ).

**********************************************************************

    out->write( `-------- Transforming to XML data: ... RESULT XML ... --------` ).
    "This section covers syntax options after RESULT XML.
    "Note: The following examples always use ABAP data objects as source
    "(... SOURCE ...).

    "ABAP -> asXML/xstring
    "This syntax option has already been used above.
    "When declared inline, the target data object has the type xstring.
    CALL TRANSFORMATION id SOURCE itab = tab
                           RESULT XML DATA(xml_tab_e).

    "Specifying a data object of type xstring
    DATA xml_tab_f TYPE xstring.
    CALL TRANSFORMATION id SOURCE itab = tab
                           RESULT XML xml_tab_f.

    "ABAP -> asXML/string
    DATA str TYPE string.
    CALL TRANSFORMATION id SOURCE itab = tab
                           RESULT XML str.

    "ABAP -> asXML/table with a flat character-like line type
    TYPES c50 TYPE c LENGTH 50.
    TYPES c50_tab_type TYPE TABLE OF c50 WITH EMPTY KEY.
    DATA chartab TYPE c50_tab_type.

    "The following statement inlcudes the OPTIONS addition.
    CALL TRANSFORMATION id SOURCE itab = tab
                           RESULT XML chartab
                           OPTIONS xml_header = 'NO'.

    out->write( data = chartab name = `chartab` ).

    "ABAP -> asXML/table with a flat byte-like line type
    TYPES x50 TYPE x LENGTH 50.
    TYPES x50_tab_type TYPE TABLE OF x50 WITH EMPTY KEY.
    DATA xtab TYPE x50_tab_type.
    "The following statement inlcudes the OPTIONS addition
    CALL TRANSFORMATION id SOURCE itab = tab
                           RESULT XML xtab
                           OPTIONS xml_header = 'NO'.

    out->write( data = xtab name = `xtab` ).

    "iXML/sXML references
    "Using an iXML output stream
    DATA xstrg TYPE xstring.
    DATA(ixml) = cl_ixml_core=>create( ).
    DATA(ixml_doc) = ixml->create_document( ).
    DATA(ixml_cr_stream) = ixml->create_stream_factory( ).
    DATA(ixml_o_stream) = ixml_cr_stream->create_ostream_xstring( string =  xstrg ).

    CALL TRANSFORMATION id SOURCE itab = tab
                           RESULT XML ixml_o_stream.

    ixml->create_renderer( document = ixml_doc
                           ostream  = ixml_o_stream )->render( ).

    DATA(xml_from_ixml) = cl_abap_conv_codepage=>create_in( )->convert( xstrg ).
    out->write( data = xml_from_ixml name = `xml_from_ixml` ).

    "Using an sXML writer
    DATA(sxml) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type     = if_sxml=>co_xt_xml10
                                                                     encoding = 'UTF-8' ) ).
    CALL TRANSFORMATION id SOURCE itab = tab
                           RESULT XML sxml.

    DATA(xml_from_sxml) = CAST cl_sxml_string_writer( sxml )->get_output( ).
    DATA(conv_xml_from_sxml) = cl_abap_conv_codepage=>create_in( )->convert( xml_from_sxml ).
    out->write( data = conv_xml_from_sxml name = `conv_xml_from_sxml` ).

**********************************************************************

    out->write( `-------- Transforming to ABAP data: ... RESULT ... --------` ).

    "This section covers syntax options after RESULT without XML.
    "XML from above is used as source.
    DATA tab_b LIKE tab.
    CALL TRANSFORMATION id SOURCE XML xml_tab_a
                           RESULT itab = tab_b.
    out->write( data = tab_b name = `tab_b` ).

    "An internal table of type abap_trans_resbind_tab can be specified.
    DATA tab_c LIKE tab.
    DATA(restab) = VALUE abap_trans_resbind_tab( ( name = 'ITAB' value = REF #( tab_c ) ) ).

    CALL TRANSFORMATION id SOURCE XML xml_tab_a
                           RESULT (restab).

    out->write( data = tab_c name = `tab_c` ).
  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Working with JSON
- You can ..
  - create/render and read/parse JSON data in ABAP using the readers and writers in the sXML library. See the processing of XML data in the sXML section above. Parsing and rendering JSON data works in a similar way. However, instead of using XML readers/writers, you use JSON readers/writers.
  - transform ABAP to and from JSON data using transformations. You can directly transform ABAP <-> JSON using identity transformation (`ID`). In this context, note the intermediate format asJSON (see the notes on asXML above).
  - create and handle JSON data using the [XCO library](https://help.sap.com/docs/btp/sap-business-technology-platform/xco-library?version=Cloud) or `/ui2/cl_json`.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating and Reading JSON Data Using sXML

> [!NOTE]  
> - In ABAP, the sXML library processes JSON data using JSON-XML, an SAP-specific JSON data representation in XML format. This intermediate step is used for both reading and creating JSON data. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_json_xml.htm).
> - The following examples provide basic implementations to give you an idea. You should always work out your own solutions.
> - Both token-based and object-oriented rendering/parsing methods are available. These examples use the token-based approach.
> - For additional information and examples, see the [ABAP and JSON](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_json.htm) section of the ABAP Keyword Documentation.


Expand the following collapsible section for example classes. To try them out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. This example is set up to display output in the console.

**Creating JSON Data using the sXML library**

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

- The `create` method from the `cl_sxml_string_writer` class is used to create a JSON writer by setting the type to `if_sxml=>co_xt_json`. 
- Note the comments about the cast in the XML section. 
- Several elements and attributes are created using token-based rendering methods. 
- Finally,  the created JSON data is displayed.


```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "In this example, the following JSON data should be created.
    "[{"carrier_id":"LH","connection_id":"400","city_from":"Frankfurt","city_to":"Berlin"},
    " {"carrier_id":"AZ","connection_id":"790","city_from":"Rome","city_to":"Osaka"}]

    "Creating a JSON writer by specifying the type in the 'create' method appropriately
    "The example uses token-based rendering.
    DATA(json_writer) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
    TRY.
        json_writer->open_element( name = 'array' ).
        json_writer->open_element( name = 'object' ).

        json_writer->open_element( name = 'str' ).
        json_writer->write_attribute( name = 'name' value = 'carrier_id' ).
        json_writer->write_value( value = 'LH' ).
        json_writer->close_element( ).

        json_writer->open_element( name = 'str' ).
        json_writer->write_attribute( name = 'name' value = 'connection_id' ).
        json_writer->write_value( value = '400' ).
        json_writer->close_element( ).

        json_writer->open_element( name = 'str' ).
        json_writer->write_attribute( name = 'name' value = 'city_from' ).
        json_writer->write_value( value = 'Frankfurt' ).
        json_writer->close_element( ).

        json_writer->open_element( name = 'str' ).
        json_writer->write_attribute( name = 'name' value = 'city_to' ).
        json_writer->write_value( value = 'Berlin' ).
        json_writer->close_element( ).

        json_writer->close_element( ).

        json_writer->open_element( name = 'object' ).

        json_writer->open_element( name = 'str' ).
        json_writer->write_attribute( name = 'name' value = 'carrier_id' ).
        json_writer->write_value( value = 'AZ' ).
        json_writer->close_element( ).

        json_writer->open_element( name = 'str' ).
        json_writer->write_attribute( name = 'name' value = 'connection_id' ).
        json_writer->write_value( value = '790' ).
        json_writer->close_element( ).

        json_writer->open_element( name = 'str' ).
        json_writer->write_attribute( name = 'name' value = 'city_from' ).
        json_writer->write_value( value = 'Rome' ).
        json_writer->close_element( ).

        json_writer->open_element( name = 'str' ).
        json_writer->write_attribute( name = 'name' value = 'city_to' ).
        json_writer->write_value( value = 'Osaka' ).
        json_writer->close_element( ).

        json_writer->close_element( ).
        json_writer->close_element( ).
      CATCH cx_sxml_state_error INTO DATA(error).
        out->write( error->get_text( ) ).
    ENDTRY.

    DATA(json) = cl_abap_conv_codepage=>create_in( )->convert( CAST cl_sxml_string_writer( json_writer )->get_output( ) ).
    out->write( json ).
  ENDMETHOD.
ENDCLASS.
```
</details>  

<br>

**Reading JSON Data using the sXML library**

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

- An internal table is created for display purposes. This table is populated when iterating over all nodes. It includes information such as the node's value. 
- For more details on the *name* component and others, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_json_xml.htm).
- The `create` method from the `cl_sxml_string_reader` class is used to create a JSON reader. 
- After iterating through all the nodes, the retrieved information that has been added to the internal table is displayed.


```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Creating demo JSON data
    DATA(json) = cl_abap_conv_codepage=>create_out( )->convert(
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

    "Creating an internal table for display purposes
    DATA: BEGIN OF node_info,
            node_type TYPE string,
            name      TYPE string,
            value     TYPE string,
          END OF node_info,
          nodes_tab LIKE TABLE OF node_info.

    "Creating reader
    "In this example, no special methods are used. Therefore, a cast is not carried out.
    DATA(reader) = cl_sxml_string_reader=>create( json ).

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
          "NEXT_ATTRIBUTE to iterate across the JSON attributes.
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
      CATCH cx_sxml_state_error INTO DATA(error_parse_token).
        out->write( error_parse_token->get_text( ) ).
    ENDTRY.

    out->write( nodes_tab ).
  ENDMETHOD.
ENDCLASS.
```
</details>  


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Transforming JSON Data Using Transformations

The following code snippets show a selection of transformation options using the predefined identity transformation. Here, a JSON writer is specified as the target.

```abap
"ABAP (elementary type) -> JSON
DATA str_a TYPE string VALUE `Hello`.
"Creating a JSON writer (see the type specification; you can also specify other
"types, e.g. if_sxml=>co_xt_xml10 for an XML writer)
DATA(writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

CALL TRANSFORMATION id SOURCE hi = str_a
                       RESULT XML writer.

"Getting the output and converting to string
DATA(json) = cl_abap_conv_codepage=>create_in( )->convert( writer->get_output( ) ).
"json: {"HI":"Hello"}

"JSON -> ABAP
DATA str_b TYPE string.
"Note: CALL TRANSFORMATION handles JSON sources implicitly.
CALL TRANSFORMATION id SOURCE XML json
                       RESULT hi = str_b.
"str_b: Hello

"ABAP -> (un)formatted JSON
"In this example, an internal table is transformed to JSON.
TYPES: BEGIN OF demo_struc,
          comp1 TYPE i,
          comp2 TYPE string,
          comp3 TYPE abap_boolean,
        END OF demo_struc,
        tab_type TYPE TABLE OF demo_struc WITH EMPTY KEY.
DATA(it) = VALUE tab_type( ( comp1 = 1 comp2 = `abc` comp3 = abap_true )
                            ( comp1 = 2 comp2 = `def` comp3 = abap_false )
                            ( comp1 = 3 comp2 = `ghi` comp3 = abap_true ) ).

DATA(json_wr) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

CALL TRANSFORMATION id SOURCE itab = it
                       RESULT XML json_wr.

DATA(json_output_xstr) = json_wr->get_output( ).
DATA(json_unformatted) = cl_abap_conv_codepage=>create_in( )->convert( json_output_xstr ).

*{"ITAB":[{"COMP1":1,"COMP2":"abc","COMP3":"X"},{"COMP1":2,"COMP2":"def","COMP3":""},{"COMP1":3,"COMP2":"ghi","COMP3":"X"}]}

"A cast is included for the writer (if_sxml_writer) to access special methods.
"See comments in the sXML section.
DATA(json_wr_formatting) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).

"With the following method calls, the result is formatted.
json_wr_formatting->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
json_wr_formatting->set_option( option = if_sxml_writer=>co_opt_indent ).

CALL TRANSFORMATION id SOURCE itab = it
                       RESULT XML json_wr_formatting.

DATA(json_formatted) = cl_abap_conv_codepage=>create_in( )->convert( CAST cl_sxml_string_writer( json_wr_formatting )->get_output( ) ).

*{
* "ITAB":
* [
*  {
*   "COMP1":1,
*   "COMP2":"abc",
*   "COMP3":"X"
*  },
*  {
*   "COMP1":2,
*   "COMP2":"def",
*   "COMP3":""
*  },
*  {
*   "COMP1":3,
*   "COMP2":"ghi",
*   "COMP3":"X"
*  }
* ]
*}
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Handling JSON Data with ABAP Classes

**Using the XCO library**

The following snippets demonstrate creating and handling JSON data using the XCO library.

```abap
"Creating and populating a demo structure and internal table
DATA: BEGIN OF carrier_struc,
        carrier_id    TYPE c length 3,
        connection_id TYPE n length 4,
        city_from TYPE c length 20,
        city_to TYPE c length 20,
      END OF carrier_struc.

DATA carriers_tab LIKE TABLE OF carrier_struc WITH EMPTY KEY.

carrier_struc = VALUE #( carrier_id = 'AA' connection_id = '17' city_from = 'New York' city_to = 'San Francisco' ).
carriers_tab = VALUE #( ( carrier_id = 'AZ' connection_id = '788' city_from = 'Rome' city_to = 'Tokyo' )
                        ( carrier_id = 'JL' connection_id = '408' city_from = 'Frankfurt' city_to = 'Tokyo' ) ).

"ABAP (structure) -> JSON using XCO
DATA(struc2json_xco) = xco_cp_json=>data->from_abap( carrier_struc )->to_string( ).
"Result: {"CARRIER_ID":"AA","CONNECTION_ID":"0017","CITY_FROM":"New York","CITY_TO":"San Francisco"}

"ABAP (internal table) -> JSON using XCO
DATA(itab2json_xco) = xco_cp_json=>data->from_abap( carriers_tab )->to_string( ).
"Result: [{"CARRIER_ID":"AZ","CONNECTION_ID":"0788","CITY_FROM":"Rome","CITY_TO":"Tokyo"},
"         {"CARRIER_ID":"JL","CONNECTION_ID":"0408","CITY_FROM":"Frankfurt","CITY_TO":"Tokyo"}]

"JSON -> ABAP (structure) using XCO
DATA json2struc_xco LIKE carrier_struc.
xco_cp_json=>data->from_string( struc2json_xco )->write_to( REF #( json2struc_xco ) ).
"Result:
"CARRIER_ID    CONNECTION_ID    CITY_FROM    CITY_TO
"AA            0017             New York     San Francisco

"JSON -> ABAP (internal table) using XCO
DATA json2itab_xco LIKE carriers_tab.
xco_cp_json=>data->from_string( itab2json_xco )->write_to( REF #( json2itab_xco ) ).
"Result:
"CARRIER_ID    CONNECTION_ID    CITY_FROM    CITY_TO
"AZ            0788             Rome         Tokyo
"JL            0408             Frankfurt    Tokyo

"Creating JSON using XCO
"You can check out more methods that offer various options to build
"the JSON by clicking CTRL + Space after '->' in ADT.
"In the following example, JSON data similar to above is created.
"First, a JSON data builder is created. Then, using different methods,
"JSON data is created.
DATA(json_builder_xco) = xco_cp_json=>data->builder( ).
json_builder_xco->begin_object(
  )->add_member( 'CarrierId' )->add_string( 'DL'
  )->add_member( 'ConnectionId' )->add_string( '1984'
  )->add_member( 'CityFrom' )->add_string( 'San Francisco'
  )->add_member( 'CityTo' )->add_string( 'New York'
  )->end_object( ).

"Getting JSON data
DATA(json_created_xco) = json_builder_xco->get_data( )->to_string( ).
"Result: {"CarrierId":"DL","ConnectionId":"1984","CityFrom":"San Francisco","CityTo":"New York"}

"Transforming the created JSON to ABAP (structure)
"Note: The JSON was intentionally created without the underscores in the
"name to demonstrate the 'apply' method. The following example demonstrates
"a transformation of camel case and underscore notation. As above, check out
"more options by using CTRL + Space after '...transformation->'.
CLEAR json2struc_xco.
xco_cp_json=>data->from_string( json_created_xco )->apply( VALUE #(
  ( xco_cp_json=>transformation->pascal_case_to_underscore ) ) )->write_to( REF #( json2struc_xco ) ).
"Result
"CARRIER_ID    CONNECTION_ID    CITY_FROM        CITY_TO
"DL            1984             San Francisco    New York
```

**Using the /ui2/cl_json Class**

For more information, see the class documentation. Note that there are many additional and optional parameters available, some of which are explored in the example in the collapsible section below.


```abap
DATA(some_table) = VALUE string_table( ( `aaa` ) ( `bbb` ) ( `ccc` ) ).

*&---------------------------------------------------------------------*
*& ABAP -> JSON
*&---------------------------------------------------------------------*

DATA(abap_to_json) = /ui2/cl_json=>serialize( data = some_table ).

*&---------------------------------------------------------------------*
*& JSON -> ABAP
*&---------------------------------------------------------------------*

DATA json_to_abap_table TYPE string_table.
/ui2/cl_json=>deserialize( EXPORTING json = abap_to_json
                           CHANGING data  = json_to_abap_table ).
```

Expand the following collapsible section for example code. To try it out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. This example is set up to display output in the console.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

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
    out->write( `---------- ABAP -> JSON (pretty printed) ----------` ).
    out->write( abap_to_json_pretty ).
    out->write( `---------- ABAP -> JSON (camel case) ----------` ).
    out->write( abap_to_json_pretty_name ).

    "---------------- Deserializing ----------------

    DATA(json_to_abap) = abap_to_json.
    DATA itab_json_to_abap LIKE itab.

    /ui2/cl_json=>deserialize( EXPORTING json = json_to_abap
                               CHANGING data  = itab_json_to_abap ).

    out->write( `---------- JSON -> ABAP ----------` ).
    out->write( itab_json_to_abap ).

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
    ENDIF.

    IF dref_xstr IS BOUND.
      out->write( `---------- JSON (xstring) -> ABAP (unknown type) ----------` ).
      out->write( dref_xstr->* ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```
</details>  


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions 

### Serializing and Deserializing Objects

- To serialize and deserialize objects (i.e. instances of classes), you can use `CALL TRANSFORMATION` statements. As a prerequisite, the classes must implement the `IF_SERIALIZABLE_OBJECT` interface. The example uses the predefined identity transformation `ID`.
- Find more information and examples [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenasxml_class_instances.htm) in the ABAP Keyword Documentation. 


Expand the following collapsible section for example classes. To try them out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The examples are set up to display output in the console.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

- When the class runs, it creates three instances, and three instance attributes are assigned values for each instance: the current UTC timestamp, a random number, and a UUID. 
- These instances are then serialized and subsequently deserialized.
- The instance attributes are accessed, and their values are stored in an internal table and displayed.


**Example 1:** 

- The example class implements the `IF_SERIALIZABLE_OBJECT` interface, using the standard behavior to serialize and deserialize all instance attributes (i.e. the helper methods mentioned below are not implemented). 
- The values of all deserialized instance attributes are displayed.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun,
      if_serializable_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA timestamp TYPE utclong.
    DATA random_number TYPE i.
    DATA uuid TYPE sysuuid_x16.

ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA serialized_obj_tab TYPE string_table.
    TYPES: BEGIN OF deserialized_obj_struc,
             timestamp     TYPE utclong,
             random_number TYPE i,
             uuid          TYPE sysuuid_x16,
           END OF deserialized_obj_struc.
    DATA deserialized_obj_tab TYPE TABLE OF deserialized_obj_struc WITH EMPTY KEY.

    "Creating objects, assigning values to instance attributes, and serializing objects
    DO 3 TIMES.
      DATA(oref) = NEW zcl_demo_abap( ).
      oref->timestamp = utclong_current( ).
      oref->random_number = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                        min  = 1
                                                        max  = 100 )->get_next( ).

      TRY.
          oref->uuid = cl_system_uuid=>create_uuid_x16_static( ) .
        CATCH cx_uuid_error.
      ENDTRY.

      DATA serialized_obj TYPE string.
      CALL TRANSFORMATION id SOURCE obj = oref
                             RESULT XML serialized_obj.

      APPEND serialized_obj TO serialized_obj_tab.
    ENDDO.

    "Deserializing objects
    LOOP AT serialized_obj_tab INTO DATA(wa).
      DATA deserialized_obj TYPE REF TO zcl_demo_abap.
      CALL TRANSFORMATION id SOURCE XML wa
                             RESULT obj = deserialized_obj.

      "Addressing instance attributes after deserialization
      DATA(deserialized_timestamp) = deserialized_obj->timestamp.
      DATA(deserialized_random_number) = deserialized_obj->random_number.
      DATA(deserialized_uuid) = deserialized_obj->uuid.

      "Adding deserialized instance attribute values to an internal table
      APPEND VALUE #( timestamp = deserialized_timestamp
                      random_number = deserialized_random_number
                      uuid = deserialized_uuid
                    ) TO deserialized_obj_tab.
    ENDLOOP.

    out->write( deserialized_obj_tab ).
  ENDMETHOD.

ENDCLASS.
```

**Example 2:** 

- The example class implements the `IF_SERIALIZABLE_OBJECT` interface, along with the `SERIALIZE_HELPER` and `DESERIALIZE_HELPER` methods. 
- These methods allow you to modify the standard behavior. Both methods must be implemented together, or not at all. If neither is implemented, the standard behavior applies. 
- The methods must be declared as private instance methods. The `SERIALIZE_HELPER` method only has output parameters, while `DESERIALIZE_HELPER` only has input parameters. Any parameter specified for the `SERIALIZE_HELPER` method must have an identically named and typed parameter in the `DESERIALIZE_HELPER` method.
- For example, you can implement these methods if you want to exclude certain attributes from (de)serialization. 
- In the given example, one of the instance attributes is excluded from (de)serialization. As a result, the output only includes initial values for one of the attributes.


```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun,
      if_serializable_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA timestamp TYPE utclong.
    DATA random_number TYPE i.
    DATA uuid TYPE sysuuid_x16.

    METHODS:
      serialize_helper EXPORTING timestamp     TYPE utclong
                                 random_number TYPE i,
      deserialize_helper IMPORTING timestamp     TYPE utclong
                                   random_number TYPE i.

ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA serialized_obj_tab TYPE string_table.
    TYPES: BEGIN OF deserialized_obj_struc,
             timestamp     TYPE utclong,
             random_number TYPE i,
             uuid          TYPE sysuuid_x16,
           END OF deserialized_obj_struc.
    DATA deserialized_obj_tab TYPE TABLE OF deserialized_obj_struc WITH EMPTY KEY.

    "Creating objects, assigning values to instance attributes, and serializing objects
    DO 3 TIMES.
      DATA(oref) = NEW zcl_demo_abap( ).
      oref->timestamp = utclong_current( ).
      oref->random_number = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                        min  = 1
                                                        max  = 100 )->get_next( ).

      TRY.
          oref->uuid = cl_system_uuid=>create_uuid_x16_static( ) .
        CATCH cx_uuid_error.
      ENDTRY.

      DATA serialized_obj TYPE string.
      CALL TRANSFORMATION id SOURCE obj = oref
                             RESULT XML serialized_obj.

      APPEND serialized_obj TO serialized_obj_tab.
    ENDDO.

    "Deserializing objects
    LOOP AT serialized_obj_tab INTO DATA(wa).
      DATA deserialized_obj TYPE REF TO zcl_demo_abap.
      CALL TRANSFORMATION id SOURCE XML wa
                             RESULT obj = deserialized_obj.

      "Addressing instance attributes after deserialization
      DATA(deserialized_timestamp) = deserialized_obj->timestamp.
      DATA(deserialized_random_number) = deserialized_obj->random_number.
      DATA(deserialized_uuid) = deserialized_obj->uuid.

      "Adding deserialized instance attribute values to an internal table
      APPEND VALUE #( timestamp = deserialized_timestamp
                      random_number = deserialized_random_number
                      uuid = deserialized_uuid
                    ) TO deserialized_obj_tab.
    ENDLOOP.

    out->write( deserialized_obj_tab ).
  ENDMETHOD.

  METHOD serialize_helper.
    timestamp = me->timestamp.
    random_number = me->random_number.
  ENDMETHOD.

  METHOD deserialize_helper.
    me->timestamp = timestamp.
    me->random_number = random_number.
  ENDMETHOD.

ENDCLASS.
``` 

</details>  


### Converting string <-> xstring
In the code snippets above and in the executable example, many operations are performed using binary data.
This excursion shows the conversion of string <-> xstring using a codepage. The examples use UTF-8.
For example, you can use the `cl_abap_conv_codepage` class and the [XCO library](https://help.sap.com/docs/btp/sap-business-technology-platform/xco-library?version=Cloud). Using the `xco_cp` class of the XCO library, you can also process Base64 representations of raw binary data (see the snippet for `xco_cp` in the [String Processing](/22_Released_ABAP_Classes.md#string-processing) section of the *Released ABAP Classes* cheat sheet). 

```abap
DATA(xml_string) = `<TXT>ABAP</TXT>`.

"string -> xstring
"Note: UTF-8 is used by default. Here, it is specified explicitly.
"Exception class that can be used: cx_sy_conversion_codepage
TRY.
    DATA(conv_xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( xml_string ).
  CATCH cx_sy_conversion_codepage.
ENDTRY.
"conv_xstring: 3C5458543E414241503C2F5458543E

"xstring -> string
DATA(conv_string) = cl_abap_conv_codepage=>create_in( )->convert( conv_xstring ).
"conv_string: <TXT>ABAP</TXT>

"As an alternative, you can use methods of the XCO library. More methods are available
"in this context. Check the options when choosing CTRL + Space after '->'
"string -> xstring
DATA(conv_xstring_xco) = xco_cp=>string( xml_string
  )->as_xstring( xco_cp_character=>code_page->utf_8
  )->value.

"xstring -> string
DATA(conv_string_xco) = xco_cp=>xstring( conv_xstring_xco
  )->as_string( xco_cp_character=>code_page->utf_8
  )->value.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Compressing and Decompressing Binary Data
You may want to process or store binary data. The data may be very large. You can compress the data in gzip format and decompress it for further processing using the `cl_abap_gzip` class.

```abap
DATA(str) = `This is a data object of type string. It should be converted to xstring, compressed and decompressed.`.
DATA(xstr) = cl_abap_conv_codepage=>create_out( )->convert( str ).
DATA xstr_comp TYPE xstring.

"Compressing binary data
TRY.
    cl_abap_gzip=>compress_binary( EXPORTING raw_in   = xstr
                                   IMPORTING gzip_out = xstr_comp ).
  CATCH cx_parameter_invalid_range cx_sy_buffer_overflow cx_sy_compression_error.
ENDTRY.

"Comparing the length of the data objects
DATA(len_xstr) = xstrlen( xstr ). "101
DATA(len_xstr_comp) = xstrlen( xstr_comp ). "81

"Decompressing binary data
DATA xstr_decomp TYPE xstring.
TRY.
    cl_abap_gzip=>decompress_binary( EXPORTING gzip_in = xstr_comp
                                     IMPORTING raw_out = xstr_decomp ).
  CATCH cx_parameter_invalid_range cx_sy_buffer_overflow cx_sy_compression_error.
ENDTRY.

DATA(len_xstr_decomp) = xstrlen( xstr_decomp ). "101
DATA(conv_str) = cl_abap_conv_codepage=>create_in( )->convert( xstr_decomp ).

"abap_true
DATA(is_equal) = xsdbool( len_xstr = len_xstr_decomp AND str = conv_str ). 
```

### Exporting and Importing Data Clusters

- A data cluster groups data objects for temporary and persistent storage in a medium, such as an elementary data object of type `xstring`, or an internal table with a specific table type.
- Potential uses include:
  - Fast serialization and deserialization of data to and from `xstring` 
  - Storing the data cluster (in a compressed form) in a DDIC database table field
  - Passing data clusters through parameters in procedures for further evaluation, especially with large, complex data
- Related ABAP statements:
  - `EXPORT` to write data objects to the memory medium
  - `IMPORT` to read from the memory medium and extract the data objects

> [!NOTE]  
> - Regarding data clusters, the focus in this section is on the fast serialization and deserialization of data to and from `xstring`.
> - Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENDATA_CLUSTER.html).
> - More syntax options are available in [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm).
> - Various exceptions can be raised when using these statements; see the related subtopics in the [documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENDATA_CLUSTER.html).
> - Additions to `IMPORT` statements offer [conversion options](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPIMPORT_CONVERSION.html).

The following example covers:
- Exporting a data cluster, storing it in a byte string, and importing it back to a data object
- Activating compression using the `COMPRESSION ON` addition
- Specifying multiple data objects in the parameter list
- Alternative syntax for specifying parameters
- Dynamic specification of the parameter list
- Exporting and importing class objects

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_serializable_object.
    METHODS constructor IMPORTING text TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA timestamp TYPE utclong.
    DATA text TYPE string.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA buffer TYPE xstring.


    "---- Exporting data cluster, storing it in byte string, -------
    "---- and importing it to a data object again ------------------

    "The following example exports an internal table to a byte string buffer, and
    "imports the content to another internal table.
    "The parameter list includes one data object. Compression is deactivated
    "by default.

    SELECT *
     FROM zdemo_abap_fli
     INTO TABLE @DATA(itab1).

    EXPORT flights = itab1 TO DATA BUFFER buffer.

    "Determining the length of the xstring (to compare it with the
    "same example that activates compression).
    DATA(strlen_export) = xstrlen( buffer ).

    out->write( data = strlen_export name = `strlen_export` ).
    out->write( |\n| ).

    DATA itab2 LIKE itab1.
    IMPORT flights = itab2 FROM DATA BUFFER buffer.

    ASSERT itab2 = itab1.

    out->write( data = itab2 name = `itab2` ).
    out->write( |\n| ).

    "---------------- Activating compression ----------------
    EXPORT flights = itab1 TO DATA BUFFER buffer COMPRESSION ON.

    DATA(strlen_export_compr) = xstrlen( buffer ).

    ASSERT strlen_export_compr < strlen_export.
    out->write( data = strlen_export_compr name = `strlen_export_compr` ).
    out->write( |\n| ).

    "------ Multiple data objects specified in the parameter list -----
    "The following example writes and reads a byte string of two numbers.

    DATA num1 TYPE i.
    DATA num2 TYPE i.
    EXPORT int1 = 100 int2 = 200 TO DATA BUFFER buffer.
    IMPORT int1 = num1 int2 = num2 FROM DATA BUFFER buffer.

    "----------------- Exceptions -----------------
    "The following example includes a non-compatible data object to read
    "to. The example emphasizes that various exceptions can be
    "raised when using the statements. Find more details in the ABAP
    "Keyword Documentation.
    DATA str_table TYPE string_table.
    TRY.
        IMPORT int1 = num1 int2 = str_table FROM DATA BUFFER buffer.
      CATCH cx_sy_import_mismatch_error.
    ENDTRY.

    "--------- Alternative syntax for specifying the parameters ---------
    "EXPORT: Using ... FROM ... instead of ... = ...
    "IMPORT: Using ... TO ... instead of ... = ...
    EXPORT flights FROM itab1 TO DATA BUFFER buffer.
    IMPORT flights TO itab2 FROM DATA BUFFER buffer.
    EXPORT int1 = 100 int2 = 200 TO DATA BUFFER buffer.
    IMPORT int1 TO num1 int2 TO num2 FROM DATA BUFFER buffer.

    "--------- Dynamic specification of the parameter list ---------
    "Note:
    "- The parameter list is specified in an index table with two columns.
    "- The column names can have random names, but the type must be character-like.
    "- The first column represents the parameter name, the second column represents
    "  the name of the data object.
    "- Note that special behavior applies. See the documentation.

    TYPES: BEGIN OF param,
             name TYPE string,
             dobj TYPE string,
           END OF param,
           param_tab_type TYPE TABLE OF param WITH EMPTY KEY.

    DATA: txt1 TYPE string VALUE `hello`,
          txt2 TYPE string VALUE `world`,
          txt3 TYPE string VALUE `ABAP`.

    DATA(param_table) = VALUE param_tab_type(
      ( name = `txt1` dobj = `txt1` )
      ( name = `txt2` dobj = `txt2` )
      ( name = `txt3` dobj = `txt3` ) ).

    EXPORT (param_table) TO DATA BUFFER buffer.

    "The example reads the content into structure components.
    DATA: BEGIN OF values,
            txt1 TYPE string,
            txt2 TYPE string,
            txt3 TYPE string,
          END OF values.

    param_table = VALUE param_tab_type(
      ( name = `txt1` dobj = `values-txt1` )
      ( name = `txt2` dobj = `values-txt2` )
      ( name = `txt3` dobj = `values-txt3` ) ).

    IMPORT (param_table) FROM DATA BUFFER buffer.

    out->write( data = values name = `values` ).
    out->write( |\n| ).

    "--------- Exporting and importing objects of classes ----------
    "The following example explores:
    "- Objects of the example class are created
    "- The objects are serialized using a CALL TRANSFORMATION statement
    "- As a prerequisite, the class includes the IF_SERIALIZABLE_OBJECT interface
    "- In a DO loop, 3 objects of the class are created.
    "- After serializing the objects, the data is exported with compression activated.
    "- Then, the data is read and deserialized again.
    "- Instance attributes are accessed and added to an internal table for output
    "  purposes. The class is setup that instance attributes receive different values,
    "  for example, the current UTC timestamp is set.

    TYPES: BEGIN OF info,
             text      TYPE string,
             timestamp TYPE utclong,
           END OF info.

    DATA info_tab TYPE TABLE OF info WITH EMPTY KEY.

    DO 3 TIMES.
      DATA(obj_ref) = NEW zcl_demo_abap( |Instance { sy-index }| ).
      CALL TRANSFORMATION id SOURCE oref = obj_ref RESULT XML DATA(xml).

      EXPORT xml_data = xml TO DATA BUFFER buffer COMPRESSION ON.

      CLEAR obj_ref.

      IMPORT xml_data = xml FROM DATA BUFFER buffer.

      CALL TRANSFORMATION id SOURCE XML xml RESULT oref = obj_ref.
      DATA(text_deserialized) = obj_ref->text.
      DATA(timestamp_deserialized) = obj_ref->timestamp.
      APPEND VALUE #( text = text_deserialized timestamp = timestamp_deserialized ) TO info_tab.
    ENDDO.

    out->write( info_tab ).
    out->write( |\n| ).

    "--------- Exporting and importing data objects to an internal table as storage medium ----------
    "The following example exports an internal table to a data cluster in an internal table,
    "and imports it into another internal table.
    "Note that there are prerequisites for the internal table as storage medium. See the documentation.
    "As the width of the second column is restricted, the data is stored across multiple table lines.

    TYPES: BEGIN OF buffer_line,
             id    TYPE i,
             clstr TYPE x LENGTH 100,
           END OF  buffer_line.

    DATA tab_buffer TYPE TABLE OF buffer_line WITH EMPTY KEY.
    EXPORT itab = itab1 TO INTERNAL TABLE tab_buffer.

    out->write( |Lines in buffer table: { lines( tab_buffer ) }| ).
    out->write( |\n| ).

    IMPORT itab = itab2 FROM INTERNAL TABLE tab_buffer.

    ASSERT itab2 = itab1.

  ENDMETHOD.

  METHOD constructor.
    me->timestamp = utclong_current( ).

    IF text IS SUPPLIED AND text IS NOT INITIAL.
      me->text = text.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information

- [ABAP and XML (main topic in the ABAP Keyword Documentation)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_xml.htm)
- [sXML](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sxml_lib.htm)
- [iXML Library for ABAP Cloud](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_ixml_lib_cloud.htm)
  - [iXML Library Classic (F1 documentation for Standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_ixml_lib.htm)
- [XSLT](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_xslt.htm)
- [SAP XSLT Processor Reference](https://help.sap.com/docs/ABAP_PLATFORM_NEW/31bfc625c2674acdb9aa7547b62db9cc/3cb7463c32a3fe13e10000000a114084.html)
- [ST](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_st.htm)
  - [ST Examples](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenst_abexas.htm)
- [asXML](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_xslt_asxml.htm)
- [asJSON](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_asjson.htm)
  - [JSON Examples](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_json_abexas.htm)
- [`CALL TRANSFORMATION`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_transformation.htm)
  - [`CALL TRANSFORMATION` Examples](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencall_transformation_abexas.htm)

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example
[zcl_demo_abap_xml_json](./src/zcl_demo_abap_xml_json.clas.abap)

> [!NOTE]  
> - The executable example ...
>   - covers the following topics:
>     - Creating/Parsing XML Data Using iXML
>     - Creating/Parsing XML Data Using sXML
>     - XML Transformations using XSLT and Simple Transformations
>     - Serializations (ABAP -> XML) and Deserialization (XML -> ABAP) using the identity transformation ID (elementary types, structures, internal tables, data and object references)
>     - `CALL TRANSFORMATION` syntax options, sources and targets of transformations
>     - Working with JSON data, XCO classes for JSON
>     - Excursions: Converting string <-> xstring, compressing and decompressing binary data
>   - uses, apart from the predefined identity transformation (ID), demo XSLT and ST programs. They are not intended to be role models for proper XSLT/ST design. 
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)