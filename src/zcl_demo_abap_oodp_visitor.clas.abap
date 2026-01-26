"! <p class="shorttext"><strong>Visitor</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the visitor design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_visitor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_oodp_visitor IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Visitor` ).

*&-------------------------------------------------------------------------------------*
*& 1) Creating an object of the class to be visited, getting data from a data source
*&-------------------------------------------------------------------------------------*

    DATA(oref_lcl) = NEW lcl_visited( ).

    "Getting data from a data source
    "The example uses the name of a view that contains timezone data.
    "Based on the name of the data source, a dynamic SELECT statement is used to
    "retrieve data and store it in an internal table.
    oref_lcl->set_itab_content_ref( data_source       = `I_TIMEZONE`
                                    number_of_entries = 10 ).

*&-------------------------------------------------------------------------------------*
*& 2) Visitor 1 (XML transformation)
*&-------------------------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Visitor 1: XML transformation of internal table content` ).

    "This visitor example includes the creation of an interface reference variable.
    "The other examples use an object reference variable with the visitor class
    "as static type directly. In doing so, the cast can be avoided to access the
    "attribute.
    DATA iref_visitor_xml TYPE REF TO lif_visitor.

    iref_visitor_xml = NEW lcl_visitor_xml( ).

    oref_lcl->receive_visitor( iref_visitor_xml ).

    "Transforming the internal table data to XML
    DATA(result_xml_transformation) = CAST lcl_visitor_xml( iref_visitor_xml )->lif_visitor~result_xstring.

    "Transformation XML -> ABAP for display purposes
    DATA tab_from_xml TYPE TABLE OF i_timezone WITH EMPTY KEY.
    CALL TRANSFORMATION id SOURCE XML result_xml_transformation
                           RESULT itab = tab_from_xml.

    out->write( data = tab_from_xml name = `tab_from_xml` ).

*&-------------------------------------------------------------------------------------*
*& 3) Visitor 2 (JSON serialization)
*&-------------------------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Visitor 2: JSON serialization` ).

    DATA(oref_visitor_json) = NEW lcl_visitor_json( ).

    oref_lcl->receive_visitor( oref_visitor_json ).

    "Serializing the internal table data
    DATA(result_json_serialization) = oref_visitor_json->lif_visitor~result_xstring.

    "Deserializing for display purposes
    DATA tab_from_json LIKE tab_from_xml.
    /ui2/cl_json=>deserialize( EXPORTING jsonx = result_json_serialization
                               CHANGING data  = tab_from_json ).

    out->write( data = tab_from_json name = `tab_from_json` ).
    out->write( |\n\n| ).

    "The example implementation also includes the assignment of the
    "interface attribute of type string.
    DATA(result_json_serialization_str) = oref_visitor_json->lif_visitor~result_string.

    out->write( result_json_serialization_str ).

*&-------------------------------------------------------------------------------------*
*& 4) Visitor 3 (Counting table entries)
*&-------------------------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Visitor 3: Counting the internal table entries` ).

    DATA(oref_visitor_count) = NEW lcl_visitor_count( ).

    oref_lcl->receive_visitor( oref_visitor_count ).

    "Getting the table entry count
    DATA(result_count) = oref_visitor_count->lif_visitor~result_string.

    out->write( result_count ).

*&-------------------------------------------------------------------------------------*
*& 5) Visitor 4 (Converting table content into string)
*&-------------------------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Visitor 4: Converting the internal table content into a string` ).

    DATA(oref_visitor_cont_str) = NEW lcl_visitor_cont_str( ).

    oref_lcl->receive_visitor( oref_visitor_cont_str ).

    "Getting the string holding the internal table content
    DATA(result_cont_str) = oref_visitor_cont_str->lif_visitor~result_string.

    out->write( result_cont_str ).
  ENDMETHOD.
ENDCLASS.
