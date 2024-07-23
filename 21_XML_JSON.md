<a name="top"></a>

# Working with XML and JSON in ABAP

- [Working with XML and JSON in ABAP](#working-with-xml-and-json-in-abap)
  - [Introduction](#introduction)
  - [Processing XML Using Class Libraries](#processing-xml-using-class-libraries)
    - [iXML](#ixml)
    - [sXML](#sxml)
  - [XML Transformations](#xml-transformations)
  - [CALL TRANSFORMATION Syntax](#call-transformation-syntax)
  - [Working with JSON](#working-with-json)
  - [Excursions](#excursions)
    - [Serializing and Deserializing Objects](#serializing-and-deserializing-objects)
    - [Converting string \<-\> xstring](#converting-string---xstring)
    - [Compressing and Decompressing Binary Data](#compressing-and-decompressing-binary-data)
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
> - In addition to the options covered in the cheat sheet, there are other ways to work with XML/JSON (for example, the `/ui2/cl_json` class). 

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

  "--------------------- Directly reading nodes ----------------------  

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
  
  "--------------------- Reading using iterators ----------------------  

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
  - no document is created in DOM format (if you do not need a DOM representation and DTDs, sXML is a more performant alternative to iXML).

Creating XML data using sXML:

```abap
"--------------------- Token-based rendering ----------------------

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

"--------------------- Object-oriented rendering ----------------------

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
"--------------------- Token-based parsing ----------------------

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

"--------------------- Object-oriented parsing ----------------------

"Creating demo XML data to be used in the example.
DATA(xml_oo_read) = cl_abap_conv_codepage=>create_out( )->convert(
      `<?xml version="1.0"?>` &&
      `<node attr_a="123">` &&
      ` <subnode1>` &&
      ` <status>A</status>` &&
      ` <date format="mm-dd-yyyy">01-01-2024</date>` &&
      ` </subnode1>` &&
      ` <subnode2>`  &&
      ` <text attr_b="1" attr_c="a">abc</text>` &&
      ` <text attr_b="2" attr_c="b">def</text>` &&
      ` <text attr_b="3" attr_c="c">ghi</text>` &&
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

> **💡 Note**<br>
> - asXML:
>   - *ABAP Serialization XML*
>   - Describes a format of XML data created when serializing ABAP data (ABAP -> XML) with the identity transformation 
>   - This format is a prerequisite for deserializations (XML -> ABAP) using identity transformations.
>   - Used as an intermediate format that defines a mapping between ABAP data and XML
>   - Therefore, if you want to deserialize XML data in ABAP, you must first transform it to the asXML format.
> - Make sure that you use appropriate exception classes for potential errors in transformations. See the section [Catchable Exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_transformation.htm) in the `CALL TRANSFORMATION` topic.
> - For serializing instances of classes, find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenasxml_class_instances.htm) in the ABAP Keyword Documentation. The classes must implement the `IF_SERIALIZABLE_OBJECT` interface (find a demo in the executable example and further down). 

<p align="right"><a href="#top">⬆️ back to top</a></p>

## CALL TRANSFORMATION Syntax

The following code snippets demonstrate a selection of possible syntax options when using [`CALL TRANSFORMATION`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_transformation.htm) statements.

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
"--------------------- Transforming XML data ----------------------
"Options for src: 
"- Data object of type string or xstring containing XML data in XML 1.0 format
"- Standard table with flat character-like or byte-like line type
"- Some references to iXML and sXML libraries are possible. For example, an 
"  interface reference variable of type if_sxml_reader.
CALL TRANSFORMATION ... SOURCE XML src
                        RESULT ...

"--------------------- Transforming ABAP data ----------------------
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

There are multiple options. Check the executable example to see them in action.

```abap
"--------------------- Transforming to XML data ----------------------
"Options for res: 
"- Data object of type string or xstring
"- Data object declared inline (e.g. DATA(a) or FINAL(b)), which has then the type xstring
"- Standard table with flat character-like or byte-like line type
"- References for objects in iXML and sXML (e.g. sXML writers)
CALL TRANSFORMATION ... SOURCE ...
                        RESULT XML res.

"--------------------- Transforming to ABAP data ----------------------
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
> More additions are available such as `PARAMETERS` (for parameter binding) and `OPTIONS` (for predefined transformation options). See the details in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_transformation.htm).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Working with JSON
- You can ..
  - create and read JSON data in ABAP using the readers and writers in the sXML library. See the processing of XML data in the sXML section above. Parsing and rendering JSON data works in a similar way. However, instead of using XML readers/writers, you use JSON readers/writers.
  - transform ABAP to and from JSON data using transformations. You can directly transform ABAP <-> JSON using identity transformation (ID). In this context, note the intermediate format asJSON (see the notes on asXML above).
  - create and handle JSON data using the [XCO library](https://help.sap.com/docs/btp/sap-business-technology-platform/xco-library?version=Cloud).

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
"more options by clicking CTRL + Space after '...transformation->'.
CLEAR json2struc_xco.
xco_cp_json=>data->from_string( json_created_xco )->apply( VALUE #(
  ( xco_cp_json=>transformation->pascal_case_to_underscore ) ) )->write_to( REF #( json2struc_xco ) ).
"Result
"CARRIER_ID    CONNECTION_ID    CITY_FROM        CITY_TO
"DL            1984             San Francisco    New York
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions 

### Serializing and Deserializing Objects

- To serialize and deserialize objects (i.e. instances of classes), you can use `CALL TRANSFORMATION` statements. As a prerequisite, the classes must implement the `IF_SERIALIZABLE_OBJECT` interface.
- Find more information and examples [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenasxml_class_instances.htm) in the ABAP Keyword Documentation. 


Expand the following collapsible section to view the code of two simplified examples. To try them out, create a demo class named `zcl_some_class` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The examples are set up to display output in the console.

<details>
  <summary>Expand to view the details</summary>
  <!-- -->

- When the class runs, it creates three instances, and three instance attributes are assigned values for each instance: the current UTC timestamp, a random number, and a UUID. 
- These instances are then serialized and subsequently deserialized.
- The instance attributes are accessed, and their values are stored in an internal table and displayed.


**Example 1:** 

- The example class implements the `IF_SERIALIZABLE_OBJECT` interface, using the standard behavior to serialize and deserialize all instance attributes (i.e. the helper methods mentioned below are not implemented). 
- The values of all deserialized instance attributes are displayed.

```abap
CLASS zcl_some_class DEFINITION
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



CLASS zcl_some_class IMPLEMENTATION.

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
      DATA(oref) = NEW zcl_some_class( ).
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
      DATA deserialized_obj TYPE REF TO zcl_some_class.
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
CLASS zcl_some_class DEFINITION
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



CLASS zcl_some_class IMPLEMENTATION.

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
      DATA(oref) = NEW zcl_some_class( ).
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
      DATA deserialized_obj TYPE REF TO zcl_some_class.
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
For example, you can use the `cl_abap_conv_codepage` class and the [XCO library](https://help.sap.com/docs/btp/sap-business-technology-platform/xco-library?version=Cloud). Using the `xco_cp` class of the XCO library, you can also process Base64 representations of raw binary data (see the snippet for `xco_cp` in the [String Processing](/22_Misc_ABAP_Classes.md#string-processing) section of the Misc ABAP Classes cheat sheet). 

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

DATA(is_equal) = COND #( WHEN len_xstr = len_xstr_decomp
                         AND str = conv_str
                         THEN 'X'
                         ELSE '' ). "Result: X
```

## More Information

- [ABAP and XML (main topic in the ABAP Keyword Documentation)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_xml.htm)
- [sXML](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sxml_lib.htm)
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

> **💡 Note**<br>
> - The executable example ...
>   - covers the following topics:
>     - Creating/Parsing XML Data Using iXML
>     - Creating/Parsing XML Data Using sXML
>     - XML Transformations using XSLT and Simple Transformations
>     - Serializations (ABAP -> XML) and Deserialization (XML -> ABAP) using the identity transformation ID (elementary types, structures, internal tables, data and object references)
>     - `CALL TRANSFORMATION` syntax options, sources and targets of transformations
>     - Dealing with JSON data, XCO classes for JSON
>     - Excursions: Converting string <-> xstring, compressing and decompressing binary data
>   - uses, apart from the predefined identity transformation (ID), demo XSLT and ST programs. They are not intended to be role models for proper XSLT/ST design. 
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)