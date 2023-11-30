<a name="top"></a>

# Working with XML and JSON in ABAP

- [Working with XML and JSON in ABAP](#working-with-xml-and-json-in-abap)
  - [Introduction](#introduction)
  - [Processing XML Using Class Libraries](#processing-xml-using-class-libraries)
    - [iXML](#ixml)
    - [sXML](#sxml)
  - [XML Transformations](#xml-transformations)
  - [CALL TRANSFORMATION Syntax](#call-transformation-syntax)
  - [Dealing with JSON](#dealing-with-json)
  - [Excursion: Converting string \<-\> xstring](#excursion-converting-string---xstring)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)
  
## Introduction

This cheat sheet provides a high-level overview of working with XML and JSON in ABAP. It covers the following topics: 
- Processing XML using class libraries (iXML, sXML)
- XML Transformations using XSLT and Simple Transformations (ST), i.e. serializations (ABAP to XML) and deserializations (XML to ABAP)
- `CALL TRANSFORMATION` syntax
- Dealing with JSON data

> **💡 Note**<br>
> - The cheat sheet snippets and the executable example cover simple cases. Find more executable examples of the ABAP Keyword Documentation following the links in the [More Information](#more-information) section. 
> - For more detailed information, such as documentation on ST syntax, what asXML is, etc., also see the links in the [More Information](#more-information) section.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Processing XML Using Class Libraries

### iXML
- *Integrated* XML Library
- Provides methods for ...
  - validating and parsing (i.e. reading) XML data in 1.0 format into a DOM representation (Document Object Model, which is a tree-like representation of the documents in the memory) 
  - processing the DOM representation (you can create or edit XML data)
  - rendering (i.e. writing) the DOM representation 
- To access XML data, you need input and output streams that are created using iXML methods. 
- Advantages: Easy access to the individual parts of an XML document possible, DTDs (Document Type Definitions) are possible
- Disadvantages: High memory consumption of the DOM (if the complete DOM is created)

The following code snippets demonstrate a selection of iXML methods for handling XML data. 

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
    `<hi>` &&
    `  <word1>hallo</word1>` &&
    `  <word2>how</word2>` &&
    `  <word3>are</word3>` &&
    `</hi>` ).

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

  "************* Directly reading nodes *************

  "Accessing the root element of the DOM. It can be used as the initial node
  "for accessing subnodes.
  "Note: Multiple methods are available to further process the nodes.
  DATA(root_element) = document_pa->get_root_element( ).
  "First subnode
  DATA(child_element) = root_element->get_first_child( ).
  "Gettng the value of that node
  DATA(child_element_value) = child_element->get_value( ). "hallo
  "Next adjacent node/getting the value
  DATA(next_element_value) = child_element->get_next( )->get_value( ). "how
  "Direct access using element names
  "Result: First element searched for
  DATA(element_by_name) = document_pa->find_from_name( name = `word3` )->get_value( ). "are
  
  "************* Reading using iterators *************
  "i.e. going over the XML nodes one after another

  "Creating an iterator
  DATA(iterator_pa) = document_pa->create_iterator( ).
  DO.
    "For the iteration, you can use the get_next method to process the nodes one after another.
    "Note: Here, all nodes are respected. You can also create filters to go over specific nodes.
    DATA(node_i) = iterator_pa->get_next( ).
    IF node_i IS INITIAL.
      "Exiting the loop when there are no more nodes to process. 
      EXIT.
    ELSE.
      ... "Do something, e.g. modify values          
    ENDIF.        
  ENDDO.

  "Creating a renderer. It renders the XML document into an output stream
  "that is used to pass XML data from the renderer.
  DATA xml_pa TYPE xstring.
  ixml_pa->create_renderer( document = document_pa
                            ostream = ixml_pa->create_stream_factory( )->create_ostream_xstring( string = xml_pa )
                          )->render( ).
  ...                        
ELSE.
  "Parsing was not successful.
  ...
ENDIF.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### sXML
- *Serial* XML Library 
- Provides XML readers and writers for different sources and targets
- Processes XML data sequentially
- Allows you to parse and render XML data in a token-based (iterating across all nodes, i.e. tokens, in the tree structure of the XML data) and object-oriented way (wrapping methods of the token-based access; providing an object-oriented way to access XML nodes)
- Unlike iXML, ...
  - more formats are possible (standard XML 1.0, but also XOP, binary XML, and JSON).
  - no document is created in DOM format (if you do not need to a DOM representation and DTDs, sXML is a more performant alternative to iXML).

Rendering XML data using sXML:

```abap
"************** Token-based rendering ************** 

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
    writer->open_element( name = 'flightNumber' ).
    writer->write_value( '400' ).
    writer->close_element( ).
    writer->close_element( ).
    writer->open_element( name = 'flight' ).
    writer->open_element( name = 'carrier' ).
    writer->write_value( 'DL' ).
    writer->close_element( ).
    writer->open_element( name = 'flightNumber' ).
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
"    <flightNumber>400</flightNumber>
"  </flight>
"  <flight>
"    <carrier>DL</carrier>
"    <flightNumber>1984</flightNumber>
"  </flight>
"</flights>

"************** Object-oriented rendering ************** 

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
    writer_oo->write_node( writer_oo->new_open_element( name = 'flightNumber' ) ).
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
    writer_oo->write_node( writer_oo->new_open_element( name = 'flightNumber' ) ).
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
"  <flightNumber>788</flightNumber>
" </flight>
" <flight>
"  <carrier>JL</carrier>
"  <flightNumber>407</flightNumber>
" </flight>
"</flights>
```

Parsing XML data using sXML:

```abap
"************** Token-based parsing ************** 

"Creating reader
"Note: See the comments for the writer above. It is similar here. For readers,
"the interface IF_SXML_READER exists. In this example, no special methods
"are used. Therefore, a cast is not carried out.
"The example uses the XML data from above.
DATA(reader) = cl_sxml_string_reader=>create( xml_oo ).
"DATA(reader_cast) = CAST if_sxml_reader( cl_sxml_string_reader=>create( xml_oo ) ).

"To iterate accros all nodes, you can call the NEXT_NODE method.
TRY.
    DO.
      "Check out other available methods in ADT by placing the cursor behind '...->'
      "and choose CTRL + Space.
      reader->next_node( ).

      "Exiting the loop when reaching the end of the XML data.
      IF reader->node_type = if_sxml_node=>co_nt_final.
        EXIT.
      ENDIF.

      "You can access the properties of the node directly.
      DATA(node_type) = reader->node_type.    "Node type, see the interface if_sxml_node
      DATA(prefix) = reader->prefix.          "Namespace prefix
      DATA(name) = reader->name.              "Name of the element
      DATA(value_type) = reader->value_type.  "Value type, see the interface if_sxml_value
      DATA(value) = reader->value.            "Character-like value (if it is textual data)
      DATA(value_raw) = reader->value_raw.    "Byte-like value (if it is raw data)

      ...

    ENDDO.
  CATCH cx_sxml_parse_error.
    ...
ENDTRY.

"************** Object-oriented parsing ************** 

"Creating demo XML data to be used in the example.
DATA(xml_oo_read) = cl_abap_conv_codepage=>create_out( )->convert(
      `<?xml version="1.0"?>` &&
      `<node atta="123">` &&
      ` <subnode1>` &&
      ` <status>A</status>` &&
      ` <date format="mm-dd-yyyy">01-01-2024</date>` &&
      ` </subnode1>` &&
      ` <subnode2>`  &&
      ` <text attb="1" attc="a">abc</text>` &&
      ` <text attb="2" attc="b">def</text>` &&
      ` <text attb="3" attc="c">ghi</text>` &&
      ` </subnode2>` &&
      `</node>` ).

"Creating reader
DATA(reader_oo) = cl_sxml_string_reader=>create( xml_oo_read ).
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

      "When the parser is currently on the node of an element opening,
      "the node object has the class CL_SXML_OPEN_ELEMENT that implements the
      "interface IF_SXML_OPEN_ELEMENT. With the methods included, you can
      "access the XML attributes of the element, e.g. using the GET_ATTRIBUTES
      "method to put the references for all attributes into an internal table.
      "To access the attributes, a downcast is required.
      IF n_type = if_sxml_node=>co_nt_element_open.
        DATA(attributes) = CAST if_sxml_open_element( node_oo )->get_attributes( ).
        ...
      ENDIF.
      ...
    ENDDO.
  CATCH cx_sxml_parse_error.
    ...
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## XML Transformations

To perform transformations in ABAP, you can use:

- XSL Transformations (XSLT)
  - Repository objects written in XSLT
  - XSLT can process ABAP data because ABAP data is first implicitly transformed into the asXML format. The result of this implicit transformation (the asXML format) is then used as the actual source for the XSL transformation.
- Identity transformations
  - SAP-delivered and predefined XSL transformation with the name `ID`
  - Used to read and write the asXML (and also asJSON) format. 
  - When the transformation ID is called, the resulting intermediate formats (asXML, asJSON) are the direct output.
  - When used, for example, to transform ABAP data, such as a structure, to XML, the transformation does not change the structure itself. You can, however, change the structure by implementing you own XSLT.
- Simple Transformation (ST)
  - Repository objects written in an SAP-specific programming language for transformations between XML formats and ABAP data

The tranformations are called using `CALL TRANSFORMATION` statements.

Possible transformations, some of which are covered in the example: 

| Transformation  | XSLT  | ST |
|---|---|---|
| XML <-> XML  | X  | - |
| ABAP <-> XML | X  | X |
| ABAP <-> ABAP | X  | - |

> **💡 Note**<br>
> asXML:
> - *ABAP Serialization XML*
> - Describes a format of XML data created when serializing ABAP data (ABAP -> XML) with the identity transformation 
> - This format is a prerequisite for deserializations (XML -> ABAP) using identity transformations.
> - Used as an intermediate format that defines a mapping between ABAP data and XML
> - Therefore, if you want to deserialize XML data in ABAP, you must first transform it to the asXML format.
> Make sure that you use appropriate exception classes for potential errors in transformations. See the section [Catchable Exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_transformation.htm) in the `CALL TRANSFORMATION` topic.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## CALL TRANSFORMATION Syntax

The following code snippets demonstrate a selection of possible syntax options when using [`CALL TRANSFORMATION`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_transformation.htm) statements.

Specifying transformations

```abap
"Specifying the name of an XSL or Simple Transformation statically
CALL TRANSFORMATION zsome_transformation SOURCE ...
                                         RESULT ...

"Specifying the identity transformation with the predefined name ID
CALL TRANSFORMATION id SOURCE ...
                       RESULT ...

"Dynamic specifiation of the transformation
"If the transformation does not exist, an exception is raised.
TRY.
  CALL TRANSFORMATION ('ID') SOURCE ...
                             RESULT ...
CATCH cx_invalid_transformation.
  ...
ENDTRY.

Specifying the source of the transformation
There are multiple options. Check the executable example to see them in action.

```abap
"**************** Transforming XML data ****************
"Options for src: 
"- Data object of type string or xstring containing XML data in XML 1.0 format
"- Standard table with flat character-like or byte-like line type
"- Some references to iXML and sXML libraries are possible. For example, an 
"  interface reference variable of type if_sxml_reader.
CALL TRANSFORMATION ... SOURCE XML src
                        RESULT ...

"**************** Transforming ABAP data ****************
"No XML specified after SOURCE
"Options for src: 
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

Specifying the result of the transformation
There are multiple options. Check the executable example to see them in action.

```abap
"**************** Transforming to XML data ****************
"Options for res: 
"- Data object of type string or xstring
"- Data object declared inline (e.g. DATA(a) or FINAL(b)), which has then the type xstring
"- Standard table with flat character-like or byte-like line type
"- References for objects in iXML and sXML (e.g. sXML writers)
CALL TRANSFORMATION ... SOURCE ...
                        RESULT XML res.

"**************** Transforming to ABAP data ****************
"No XML specified after RESULT
"Similar to above, multiple ABAP data objects can be specified as a static parameter list.
CALL TRANSFORMATION ... SOURCE ...
                        RESULT y1 = abap1 ...

"An internal table of type abap_trans_resbind_tab can be specified.
DATA(restab) = VALUE abap_trans_resbind_tab( ( name = 'A' value = REF #( b ) ) ( ... ) ).       
CALL TRANSFORMATION ... SOURCE ...
                        RESULT (restab).
```

> **💡 Note**<br>
> More additions are avaialble such as `PARAMETERS` (for parameter binding) and `OPTIONS` (for predefined transformation options). See the details in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_transformation.htm).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Dealing with JSON
- You can ..
  - create and read JSON data in ABAP using the readers and writers in the sXML Library. See the processing of XML data in the sXML section above. Parsing and rendering JSON data works in a similar way. However, instead of using XML readers/writers, you use JSON readers/writers.
  - transform ABAP to and from JSON data using transformations. You can directly transform ABAP <-> JSON using identity transformation (ID). In this context, note the intermediate format asJSON (see the notes on asXML above).

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

"ABAP -> formatted JSON
"In this example, an internal table is transformed to JSON. 
"A cast is included for the writer (if_sxml_writer) to access special methods. 
"See comments in the sXML section.
DATA(str_table) = value string_table( ( `abc` ) ( `def` ) ( `ghi` ) ).
DATA(json_wr) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
DATA(json_wr_cast) = CAST if_sxml_writer( json_wr ).

"With the following method calls, the result is formatted.
json_wr_cast->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
json_wr_cast->set_option( option = if_sxml_writer=>co_opt_indent ).

CALL TRANSFORMATION id SOURCE itab = str_table
                        RESULT XML json_wr.

DATA(json_formatted) = cl_abap_conv_codepage=>create_in( )->convert( json_wr->get_output( ) ).

*json_formatted:
*{
* "ITAB":
* [
*  "abc",
*  "def",
*  "ghi"
* ]
*}
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursion: Converting string <-> xstring
In the code snippets above and in the exexcutable example, many operations are performed using binary data.
This excursion shows the conversion of string <-> xstring using a codepage. The examples use UTF-8.
For example, you can use the `cl_abap_conv_codepage` class and the [XCO library](https://help.sap.com/docs/SAP_S4HANA_CLOUD/0f69f8fb28ac4bf48d2b57b9637e81fa/702b5328be1a4bc4852ce29b09506b04.html?locale=en-US).

```abap
DATA(xml_string) = `<TXT>ABAP</TXT>`.

"string -> xstring
"Note: UTF-8 is used by default. Here, it is specified explicitly.
"Exceptions are caught with cx_sy_conversion_codepage exception.
TRY.
    DATA(conv_xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( xml_string ).
  CATCH cx_sy_conversion_codepage.
ENDTRY.
"conv_xstring: 3C5458543E414241503C2F5458543E

"xstring -> string
DATA(conv_string) = cl_abap_conv_codepage=>create_in( )->convert( conv_xstring ).
"conv_string: <TXT>ABAP</TXT>

"As an alternative, you can use methods of the XCO library. More methods are available
"in this context. Check the CTRL + Space after '->'
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

## More Information

- [ABAP and XML (main topic in the ABAP Keyword Documentation)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_xml.htm)
- [sXML](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sxml_lib.htm)
- [XSLT](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_xslt.htm)
- [SAP XSLT Processor Reference](https://help.sap.com/docs/ABAP_PLATFORM_NEW/31bfc625c2674acdb9aa7547b62db9cc/3cb7463c32a3fe13e10000000a114084.html)
- [ST](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_st.htm)
  - [ST Examples](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenst_abexas.htm)
- [asXML](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_xslt_asxml.htm)
- [asJSON](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_asjson.htm)
- [`CALL TRANSFORMATION`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_transformation.htm)
  - [`CALL TRANSFORMATION` Examples](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencall_transformation_abexas.htm)
- [JSON Examples](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_json_abexas.htm)

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example
[zcl_demo_abap_xml_json](./src/zcl_demo_abap_xml_json.clas.abap)

> **💡 Note**<br>
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - The executable example ...
>   - does not cover all facets, techniques, and syntax options. 
>   - uses demo XSLT and ST programs (which are not intended to be role models for proper XSLT/ST design), apart from snippets that use the predefined identity transformation (ID). 
>   - provides a rough overview of how to work with XML and JSON in ABAP using simple contexts.