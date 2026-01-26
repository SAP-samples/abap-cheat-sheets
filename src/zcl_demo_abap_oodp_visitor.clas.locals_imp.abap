CLASS lcl_visited DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*& Interface
*&---------------------------------------------------------------------*

INTERFACE lif_visitor.
  DATA result_xstring TYPE xstring.
  DATA result_string TYPE string.
  METHODS visit IMPORTING oref TYPE REF TO lcl_visited.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Class to be visited
*&---------------------------------------------------------------------*

CLASS lcl_visited DEFINITION.
  PUBLIC SECTION.
    METHODS: set_itab_content_ref IMPORTING VALUE(data_source) TYPE string
                                            number_of_entries  TYPE i OPTIONAL,
      get_itab_content_ref RETURNING VALUE(content) TYPE REF TO data,
      receive_visitor IMPORTING obj TYPE REF TO lif_visitor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA table_data_ref TYPE REF TO data.
ENDCLASS.

CLASS lcl_visited IMPLEMENTATION.

  METHOD set_itab_content_ref.
    data_source = to_upper( condense( val = data_source to = `` ) ).

    "The example implementation includes a dynamic SELECT statement.
    "The example goes without appropriate checks regarding dynamically
    "inserting content from outside. Refer to the Dynamic Programming
    "ABAP cheat sheet.
    TRY.
        IF number_of_entries IS SUPPLIED.
          SELECT * FROM (data_source) INTO TABLE NEW @table_data_ref UP TO @number_of_entries ROWS.
        ELSE.
          SELECT * FROM (data_source) INTO TABLE NEW @table_data_ref.
        ENDIF.
      CATCH cx_sy_dynamic_osql_semantics INTO DATA(error).
        "Note: An exception raised here indicates that the specified data source
        "cannot be accessed. In that case, adapt the code and specify a suitable
        "data source.
        RAISE SHORTDUMP error.
    ENDTRY.
  ENDMETHOD.

  METHOD receive_visitor.
    "Passing an object of this class to the concrete visitor.
    obj->visit( me ).
  ENDMETHOD.

  METHOD get_itab_content_ref.
    IF table_data_ref IS BOUND.
      TRY.
          "Is a table
          DATA(tdo) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data_ref( table_data_ref ) ).
        CATCH cx_root.
          "Is not a table
          RETURN.
      ENDTRY.
      content = table_data_ref.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete visitor 1: XML transformation
*&---------------------------------------------------------------------*

CLASS lcl_visitor_xml DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_visitor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_visitor_xml IMPLEMENTATION.
  METHOD lif_visitor~visit.


    DATA(tab_content) = oref->get_itab_content_ref( ).
    IF tab_content IS BOUND.
      CALL TRANSFORMATION id SOURCE itab = tab_content->*
                             RESULT XML lif_visitor~result_xstring.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete visitor 2: JSON serialization
*&---------------------------------------------------------------------*

CLASS lcl_visitor_json DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_visitor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_visitor_json IMPLEMENTATION.
  METHOD lif_visitor~visit.
    DATA(tab_content) = oref->get_itab_content_ref( ).
    IF tab_content IS BOUND.
      lif_visitor~result_string = /ui2/cl_json=>serialize( data = tab_content->* format_output = abap_true ).
      lif_visitor~result_xstring = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( lif_visitor~result_string ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete visitor 3: Counting table entries
*&---------------------------------------------------------------------*

CLASS lcl_visitor_count DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_visitor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_visitor_count IMPLEMENTATION.
  METHOD lif_visitor~visit.
    DATA(tab_content) = oref->get_itab_content_ref( ).
    IF tab_content IS BOUND.
      lif_visitor~result_string = lines( tab_content->* ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete visitor 4: Converting table content into string
*&---------------------------------------------------------------------*

CLASS lcl_visitor_cont_str DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_visitor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_visitor_cont_str IMPLEMENTATION.
  METHOD lif_visitor~visit.
    DATA(tab_content) = oref->get_itab_content_ref( ).
    IF tab_content IS BOUND.
      LOOP AT tab_content->* ASSIGNING FIELD-SYMBOL(<fs>).
        DO.
          ASSIGN <fs>-(sy-index) TO FIELD-SYMBOL(<comp>).
          IF sy-subrc = 0.
            lif_visitor~result_string &&= <comp> && `, ` .
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
        "Replacing the final comma in the string and adding a line break
        lif_visitor~result_string = replace( val = lif_visitor~result_string pcre = `,([^,]*)$` with = `` ).
        lif_visitor~result_string &&= |\n|.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
