"! <p class="shorttext"><strong>Iterator</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the iterator design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_iterator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap_oodp_iterator IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Iterator` ).

*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Example 1` ).

    DATA: col       TYPE REF TO lcl_collection_strings,
          iterator  TYPE REF TO lif_iterator,
          iterator2 TYPE REF TO lif_iterator,
          item      TYPE string,
          position  TYPE i,
          count     TYPE i.

    "Creating a collection and adding items
    col = NEW lcl_collection_strings( ).
    col->add( `lorem` ).
    col->add( `ipsum` ).
    col->add( `dolor` ).
    col->add( `sit` ).
    col->add( `amet` ).

    "Checking the available methods
    "Getting the number of items in the collection
    count = col->lif_collection~get_number_of_items( ).
    out->write( |Number of items in the collection: { count }| ).

    "Getting an item from a specific position
    position = 2.
    item = col->lif_collection~get_item_at_position( position ).
    out->write( |Item at position { position }: "{ item }"| ).
    out->write( |\n| ).

    "Getting an iterator for the collection
    iterator = col->lif_collection~get_iterator( ).

    "Iterating accross the collection
    out->write( `Iterating accross the collection:` ).
    "Performing the iteration as long as there are items to be processed
    WHILE iterator->has_next( ) = abap_true.
      "Getting the next item
      item = iterator->get_next( ).
      "Determining the current position
      position = iterator->get_current_position( ).
      out->write( |{ position } { item }| ).
    ENDWHILE.
    out->write( |\n| ).

    "Checking the move backward and forward methods
    out->write( `Moving backward and forward:` ).

    item = iterator->get_previous( ).
    position  = iterator->get_current_position( ).
    out->write( |Move backward one step; position: "{ position }" item: "{ item }"| ).

    item = iterator->get_previous( ).
    position = iterator->get_current_position( ).
    out->write( |Move backward one step; position: "{ position }" item: "{ item }"| ).

    item = iterator->get_next( ).
    position  = iterator->get_current_position( ).
    out->write( |Move forward one step; position: "{ position }" item: "{ item }"| ).

    DO 3 TIMES.
      item = iterator->get_previous( ).
      position  = iterator->get_current_position( ).
      out->write( |Move backward one step; position: "{ position }" item: "{ item }"| ).
    ENDDO.

    item = iterator->get_next( ).
    position  = iterator->get_current_position( ).
    out->write( |Move forward one step; position: "{ position }" item: "{ item }"| ).

    out->write( |\n| ).

    "Resetting the iteration
    out->write( `Reset:` ).
    iterator->reset( ).
    position = iterator->get_current_position( ).
    out->write( |Position after reset: "{ position }"| ).

    out->write( `Get next item after reset:` ).
    item = iterator->get_next( ).
    position = iterator->get_current_position( ).
    out->write( |{ position } { item }| ).
    out->write( |\n| ).

    "Resetting and iterating
    out->write( `Iteration after reset:` ).
    iterator->reset( ).
    WHILE iterator->has_next( ) = abap_true.
      item = iterator->get_next( ).
      position = iterator->get_current_position( ).
      out->write( |{ position } { item }| ).
    ENDWHILE.
    out->write( |\n| ).

    "Two separate iterators iterating across the same collection
    out->write( `Two separate iterators iterating across the same collection:` ).
    iterator  = col->lif_collection~get_iterator( ).
    iterator2 = col->lif_collection~get_iterator( ).

    "Different movements
    iterator->get_next( ).
    iterator->get_next( ).
    iterator2->get_next( ).

    position = iterator->get_current_position( ).
    out->write( |First iterator, current position: { position }| ).

    position = iterator2->get_current_position( ).
    out->write( |Second iterator, current position: { position }| ).

    item = iterator->get_next( ).
    out->write( |First iterator, next element: "{ item }"| ).

    item = iterator2->get_next( ).
    out->write( |Second iterator, next element: "{ item }"| ).
    out->write( |\n| ).

    "Unavailable position
    out->write( `Unavailable position:` ).
    position = 10.
    item = col->lif_collection~get_item_at_position( position ).
    out->write( |{ position } { item }| ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Example 2` ).

    out->write( `--- itab1 --- ` ).
    out->write( |\n| ).

    TYPES: BEGIN OF struct,
             comp1 TYPE i,
             comp2 TYPE c LENGTH 3,
             comp3 TYPE string,
             comp4 TYPE n LENGTH 5,
           END OF struct,
           ty_tab TYPE TABLE OF struct WITH EMPTY KEY.

    DATA(itab1) = VALUE ty_tab( ( comp1 = 100 comp2 = 'abc' comp3 = `hello` comp4 = '123' )
                                ( comp1 = 200 comp2 = 'def' comp3 = `world` comp4 = '456' )
                                ( comp1 = 300 comp2 = 'ghi' comp3 = `ABAP` comp4 = '789' )
                                ( comp1 = 400 comp2 = 'jkl' comp3 = sy-uname comp4 = '12345' ) ).

    DATA(col_itab) = NEW lcl_collection_line2string( itab1 ).

    count = col_itab->lif_collection~get_number_of_items( ).
    out->write( |Number of items in the collection: { count }| ).

    position = 2.
    item = col_itab->lif_collection~get_item_at_position( position ).
    out->write( |Item at position { position }: { item }| ).
    out->write( |\n| ).

    "Getting an iterator for  the collection
    iterator = col_itab->lif_collection~get_iterator( ).

    "Iterating across the collection
    out->write( |Iterating across the collection| ).
    WHILE iterator->has_next( ) = abap_true.
      item = iterator->get_next( ).
      position  = iterator->get_current_position( ).
      out->write( |{ position } { item }| ).
    ENDWHILE.

    out->write( |\n| ).
    out->write( `--- itab2 --- ` ).
    out->write( |\n| ).

    "Using an internal table with elementary line type
    DATA(itab2) = VALUE string_table( ( `hello` ) ( `world` ) ( `ABAP` ) ).

    col_itab = NEW lcl_collection_line2string( itab2 ).

    count = col_itab->lif_collection~get_number_of_items( ).
    out->write( |Number of items in the collection: { count }| ).

    position = 2.
    item = col_itab->lif_collection~get_item_at_position( position ).
    out->write( |Item at position { position }: { item }| ).
    out->write( |\n| ).

    "Getting an iterator for the collection
    iterator = col_itab->lif_collection~get_iterator( ).

    "Iterating across the collection
    out->write( |Iterating across the collection| ).
    WHILE iterator->has_next( ) = abap_true.
      item = iterator->get_next( ).
      position  = iterator->get_current_position( ).
      out->write( |{ position } { item }| ).
    ENDWHILE.
    out->write( |\n| ).

    out->write( `--- Internal tables not supported by the example --- ` ).
    out->write( |\n| ).
    out->write( `--- itab3 --- ` ).
    out->write( |\n| ).

    TYPES: BEGIN OF struct2,
             comp1 TYPE i,
             comp2 TYPE string_table,
           END OF struct2,
           ty_tab2 TYPE TABLE OF struct2 WITH EMPTY KEY.

    DATA(itab3) = VALUE ty_tab2( ( comp1 = 100  ) ).

    TRY.
        col_itab = NEW lcl_collection_line2string( itab3 ).
      CATCH lcx_error INTO DATA(err).
        out->write( err->get_text( ) ).
    ENDTRY.
    out->write( |\n| ).
    out->write( `--- itab4 --- ` ).
    out->write( |\n| ).

    TYPES ty_tab_data TYPE TABLE OF REF TO data.
    DATA itab4 TYPE ty_tab_data.
    APPEND REF string( `nope` ) TO itab4.

    TRY.
        col_itab = NEW lcl_collection_line2string( itab4 ).
      CATCH lcx_error INTO err.
        out->write( err->get_text( ) ).
    ENDTRY.
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Example 3
*&---------------------------------------------------------------------*

    "The following example demonstrates a released ABAP class, used for
    "processing XML data. For more information, refer to the Working with
    "XML and JSON in ABAP cheat sheet.
    "The class reflects an iterator setup. XML data is processed, item
    "names and values are added to a string table.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Example 2` ).

    "Demo XML data
    DATA(some_xml) = cl_abap_conv_codepage=>create_out( )->convert(
          `<?xml version="1.0"?>` &&
          `<node>` &&
          ` <subnode1>` &&
          `  <hallo>hi</hallo>` &&
          ` </subnode1>` &&
          ` <subnode2>` &&
          `  <letter>a</letter>` &&
          `  <date>01-01-2025</date>` &&
          ` </subnode2>` &&
          ` <subnode3>`  &&
          `  <text>abc</text>` &&
          `  <text>def</text>` &&
          `  <text>ghi</text>` &&
          `  <text>jkl</text>` &&
          ` </subnode3>` &&
          `</node>` ).

    "Creating one factory object of the access class cl_ixml_core using the
    "create method. It is used to access the iXML library.
    DATA(ixml_pa) = cl_ixml_core=>create( ).
    "Creating an input stream that is used for the input of XML data
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
    IF parsing_check = 0.
      "Creating an iterator
      DATA(iterator_pa) = document_pa->create_iterator( ).

      "For the iteration, you can use the get_next method to process the nodes one after another.
      DATA(node_pa) = iterator_pa->get_next( ).

      WHILE NOT node_pa IS INITIAL.
        DATA(node_type) = node_pa->get_type( ).
        CASE node_type.
          WHEN if_ixml_node=>co_node_element.
            out->write( |Element: "{ node_pa->get_name( ) }"| ).
          WHEN if_ixml_node=>co_node_text OR if_ixml_node=>co_node_cdata_section.
            out->write( |Text: "{ node_pa->get_value( ) }"| ).
          WHEN OTHERS.
            ...
        ENDCASE.
        "Retrieving the next node
        node_pa = iterator_pa->get_next( ).
      ENDWHILE.
    ELSE.
      out->write( `Parsing was not successful` ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
