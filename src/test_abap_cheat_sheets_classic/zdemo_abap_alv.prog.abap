***********************************************************************
*
*        ABAP cheat sheet: Selection screens and classic lists
*        Example: SAP List Viewer (ALV)
*
* -------------------------- PURPOSE ----------------------------------
* Example that demonstrates a selection of SAP List Viewer (ALV)-related
* options:
* - Layout-related options
*   - Setting the header title
*   - Optimizing column width
*   - Setting filter
*   - Setting short, medium and long texts of particular columns
*   - Setting the text alignment of a particular column
*   - Setting the visibility of a particular column
*   - Setting the color of table columns
*   - Setting the color of table cells
*   - Setting the color of table rows
*   - Setting table rows to a striped pattern
*   - Setting text properties such as alignment and blanks (not zeroes)
*   - Displaying horizontal and vertical grid lines
*   - Displaying as popup
* - Functionality
*   - Single click events/hotspots
*   - Double click events
*   - Custom functions
*   - Generic functions
*   - Adding aggregations
*   - Sepcifying selection types
*   - Specifying tooltips
*
* ----------------------- GETTING STARTED -----------------------------
* - Open the program with the ABAP development tools for Eclipse (ADT).
* - Choose F8 to run the program.
*
* ----------------------------- NOTE -----------------------------------
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

PROGRAM.

SELECTION-SCREEN: COMMENT 1(70) intro1,
COMMENT /1(70) intro2,
COMMENT /1(70) intro3,
SKIP.

SELECTION-SCREEN BEGIN OF BLOCK layout WITH FRAME TITLE t2.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS setheadr TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t4,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS opt TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t6,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS sort TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t7,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS colheadr TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t8,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS selct TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t10,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS colors TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t11,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS filter TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t12,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS txtprop TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t13,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS lines TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t14,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS tool TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t15,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS popup TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t16,
  END OF LINE.

SELECTION-SCREEN END OF BLOCK layout.

SELECTION-SCREEN BEGIN OF BLOCK ev_fu WITH FRAME TITLE t17.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS aggreg TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t18,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS hotspots TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t19,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS doublecl TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t20,
  END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS func TYPE c AS CHECKBOX.
  SELECTION-SCREEN: COMMENT 3(70) t21,
  END OF LINE.

SELECTION-SCREEN END OF BLOCK ev_fu.

"Adding a pushbutton in the application toolbar
TABLES sscrfields.
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.

TYPES: BEGIN OF flight_struc,
         carrid    TYPE zdemo_abap_fli-carrid,
         connid    TYPE zdemo_abap_fli-connid,
         fldate    TYPE zdemo_abap_fli-fldate,
         price     TYPE zdemo_abap_fli-price,
         currency  TYPE zdemo_abap_fli-currency,
         planetype TYPE zdemo_abap_fli-planetype,
         text      TYPE string,
         seatsmax  TYPE zdemo_abap_fli-seatsmax,
         seatsocc  TYPE zdemo_abap_fli-seatsocc,
         seatsfree TYPE i,
         occrate   TYPE p LENGTH 5 DECIMALS 2,
         seatstat  TYPE icon_d,
         colored   TYPE lvc_t_scol,
         hotspot   TYPE icon_d,
       END OF flight_struc.

DATA itab4alv TYPE TABLE OF flight_struc WITH EMPTY KEY.
DATA alv TYPE REF TO cl_salv_table.
DATA already_updated TYPE abap_bool.

**********************************************************************

"Local classes for modularization purposes

"Class to handle events
CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS click_hotspot
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.

    CLASS-METHODS double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.

    CLASS-METHODS func_click
      FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_events IMPLEMENTATION.

  METHOD click_hotspot.
    READ TABLE itab4alv INDEX row REFERENCE INTO DATA(sc_ref).
    IF sy-subrc = 0.
      ASSIGN sc_ref->(column) TO FIELD-SYMBOL(<fs_sc>).
      MESSAGE `Single click event. ` &&
      |Row: { row } { COND #( WHEN column IS NOT INITIAL THEN `Column: ` && column ) } | &&
      |{ COND #( WHEN <fs_sc> IS ASSIGNED THEN `Value: ` && <fs_sc> ) }| TYPE 'I'.
    ELSE.
      MESSAGE `Single click event` TYPE 'I'.
    ENDIF.
  ENDMETHOD.

  METHOD double_click.
    READ TABLE itab4alv INDEX row REFERENCE INTO DATA(dc_ref).
    IF sy-subrc = 0.
      ASSIGN dc_ref->(column) TO FIELD-SYMBOL(<fs_dc>).
      MESSAGE `Double click event. ` &&
      |Row: { row } { COND #( WHEN column IS NOT INITIAL THEN `Column: ` && column ) } | &&
      |{ COND #( WHEN <fs_dc> IS ASSIGNED THEN `Value: ` && <fs_dc> ) }| TYPE 'I'.
    ELSE.
      MESSAGE `Double click event` TYPE 'I'.
    ENDIF.
  ENDMETHOD.

  METHOD func_click.
    "Handling custom functions
    CASE e_salv_function.
      WHEN 'GET_SELECT'.
        "This function gets the selected table rows. A message is displayed
        "listing the selected rows, if any.
        DATA(selections) = alv->get_selections( ).
        DATA(selected_rows) = selections->get_selected_rows( ).

        IF lines( selected_rows ) IS INITIAL.
          MESSAGE 'None of the rows of the displayed internal table have been selected.' TYPE 'I'.
        ELSE.
          DATA acc_rows TYPE string.
          LOOP AT selected_rows REFERENCE INTO DATA(line).
            IF sy-tabix = 1.
              acc_rows = line->*.
            ELSE.
              acc_rows = acc_rows && `,` && line->*.
            ENDIF.
          ENDLOOP.
          CONDENSE acc_rows NO-GAPS.
          MESSAGE |Selected row{ COND #( WHEN lines( selected_rows ) > 1 THEN `s` ) }: { acc_rows }| TYPE 'I'.
        ENDIF.

      WHEN 'CALC'.
        "This function performs a calculation. It calculates the number of free seats
        "and the occupancy rate of the seats.
        "Using the 'refresh' method, the table is refreshed, and the calculated values
        "are diplayed in the respective columns.
        IF already_updated = abap_true.
          MESSAGE 'The values have already been calculated.' TYPE 'I'.
        ELSE.
          LOOP AT itab4alv REFERENCE INTO DATA(calc).
            calc->seatsfree = calc->seatsmax - calc->seatsocc.
            TRY.
                calc->occrate = |{ CONV decfloat34( ( calc->seatsocc / calc->seatsmax ) * 100 )  DECIMALS = 2 }|.
                IF calc->occrate >= 95.
                  calc->seatstat = icon_red_light.
                ELSEIF calc->occrate < 95 AND calc->occrate > 75.
                  calc->seatstat = icon_yellow_light.
                ELSE.
                  calc->seatstat = icon_green_light.
                ENDIF.
              CATCH cx_sy_arithmetic_error.
            ENDTRY.
          ENDLOOP.

          MESSAGE 'Values for the SEATSFREE and OCCRATE columns have been calculated and are updated. The SEATSTAT column is updated accordingly.' TYPE 'I'.
          already_updated = abap_true.
          alv->refresh( ).
        ENDIF.
      WHEN 'HIDE'.
        "This function hides and shows a specific table column.
        TRY.
            DATA(col) = alv->get_columns( )->get_column( 'HOTSPOT' ).
            IF col->is_visible( ).
              col->set_visible( abap_false ).
            ELSE.
              col->set_visible( abap_true ).
            ENDIF.
          CATCH cx_salv_not_found.
        ENDTRY.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

"Local class to handle the selected checkboxes
CLASS lcl_alv_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS set_header
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS optimize
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS sort
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS misc_column
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS set_filter
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS selection
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS coloring
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS aggregate
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS set_text_properties
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS display_grid_lines
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS display_as_popup
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS use_functions
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS enable_hotspots
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS handle_double_click
      IMPORTING checkbox TYPE abap_bool.

    CLASS-METHODS tooltips
      IMPORTING checkbox TYPE abap_bool.
ENDCLASS.

CLASS lcl_alv_demo IMPLEMENTATION.

  METHOD set_header.
    IF checkbox = abap_true.
      alv->get_display_settings( )->set_list_header( 'Flight Overview' ).
    ENDIF.
  ENDMETHOD.

  METHOD optimize.
    IF checkbox = abap_true.
      alv->get_columns( )->set_optimize( abap_true ).
    ELSE.
      alv->get_columns( )->set_optimize( abap_false ).
    ENDIF.
  ENDMETHOD.

  METHOD sort.
    IF checkbox = abap_true.
      "In this example, a particular column is sorted downwards.
      TRY.
          alv->get_sorts( )->add_sort( 'CARRID' )->set_sequence( if_salv_c_sort=>sort_down ).
        CATCH cx_root.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD misc_column.
    "In this example, several column-related options are covered:
    "- Setting short, medium and long texts of particular columns
    "- Setting the text alignment of a particular column
    "- Setting the visibility of a particular column (here, a column is hidden)
    IF checkbox = abap_true.
      DATA(columns) = alv->get_columns( )->get( ).
      LOOP AT columns REFERENCE INTO DATA(colref).
        DATA(column) = colref->r_column.

        "Setting short, medium and long texts
        column->set_long_text( |{ colref->columnname }| ).

        "For testing purposes, you can manually increase the column width for the
        "two columns and see how the name changes depending on the column size.
        "If optimize is also flagged, all column names are fully displayed.
        "In the case of the following two columns, the short text is displayed
        "because the values in the column consist of 3 characters only in the example.
        IF colref->columnname = 'SEATSMAX'.
          column->set_short_text( 'MAX' ).
          column->set_medium_text( 'STMAX' ).
        ENDIF.

        IF colref->columnname = 'SEATSOCC'.
          column->set_short_text( 'OCC' ).
          column->set_medium_text( 'STOCC' ).
        ENDIF.

        "Setting the text alignment
        IF colref->columnname = 'CURRENCY'.
          "Without this setting, the values of the CURRENCY column would be left-aligned by default.
          column->set_alignment( if_salv_c_alignment=>right ).
        ENDIF.

        "Setting the visibility
        IF colref->columnname = 'TEXT'.
          "without this setting the values of currency column would be left-aligned by default
          "left is the default
          column->set_visible( abap_false ).
        ENDIF.

        "Change Position of the Column within the ALV Output
        IF colref->columnname = 'SEATSOCC'.
          TRY.
              DATA(pos) = alv->get_columns( )->get_column_position( 'SEATSMAX' ).
              alv->get_columns( )->set_column_position( columnname = 'SEATSOCC' position = pos ).
            CATCH cx_salv_not_found.
          ENDTRY.
        ENDIF.

        "The following statements are intended for demonstration purposes. They showcase other
        "available methods in this context. You can, for example, set a breakpoint here and
        "check the values of the variables in the debugger. Many more methods are available for
        "different use cases. The following statements show a selection.
        IF colref->columnname = 'CURRENCY'.
          DATA(alignm) = column->get_alignment( ).
          DATA(col_name) = column->get_columnname( ).
          DATA(long_text) = column->get_long_text( ).
          DATA(medium_text) = column->get_medium_text( ).
          DATA(short_text) = column->get_short_text( ).
          DATA(ouput_length) = column->get_output_length( ).
          DATA(ddictype) = column->get_ddic_inttype( ).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD set_filter.
    IF checkbox = abap_true.
      "In this example, a filter is applied on a particular column. Depending on the
      "filter setting, table rows are displayed or not.
      TRY.
          alv->get_filters( )->add_filter( 'PRICE' )->add_selopt( sign = 'I' option = 'BT' low = '0' high = '1100' ).
        CATCH cx_salv_existing cx_salv_data_error cx_salv_not_found.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD aggregate.
    IF checkbox = abap_true.
      "In this example, an aggregation is added for a particular column.
      "In tis case, the average price is calculated. It is displayed in a row added
      "to the bottom of the table.
      TRY.
          alv->get_aggregations( )->add_aggregation( columnname  = 'PRICE'
                                                     aggregation = if_salv_c_aggregation=>average ).
        CATCH cx_salv_data_error cx_salv_not_found cx_salv_existing.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD selection.
    "In this example, the selection type is specified.
    DATA(sel) = alv->get_selections( ).
    IF checkbox = abap_true.
      "Mutliple rows/columns can be selected
      sel->set_selection_mode( if_salv_c_selection_mode=>row_column ).
    ELSE.
      "Individual selection
      sel->set_selection_mode( if_salv_c_selection_mode=>single ).
    ENDIF.
  ENDMETHOD.

  METHOD coloring.
    "In this example, multiple options are demonstrated to provide color settings:
    "- Setting the color of table columns
    "- Setting the color of table cells
    "- Setting the color of table rows
    "- Setting table rows to a striped pattern
    IF checkbox = abap_true.
      "Setting the color of table columns
      TRY.
          CAST cl_salv_column_table( alv->get_columns( )->get_column( 'CURRENCY' ) )->set_color( VALUE lvc_s_colo( col = 3 ) ).
        CATCH cx_salv_not_found.
      ENDTRY.

      "Setting the color of table cells
      "For this purpose, the internal table has a component of type
      "lvc_t_scol, which is used for cell coloring here.
      "In the example, the color for cells in a particular column are set
      "based on a condition. Intensified and inverse values are explicitly
      "set to 'off'.
      LOOP AT itab4alv REFERENCE INTO DATA(coloredcell) WHERE price < 300.
        APPEND VALUE #( fname     = 'PRICE'
                        color-col = col_positive
                        color-int = 0
                        color-inv = 0 ) TO coloredcell->colored.
      ENDLOOP.

      "Setting the color of table rows
      "Also here, the component of type lvc_t_scol is used. When the internal
      "table row is added, the name of the field is omitted so that the color is
      "set for the entire table row.
      LOOP AT itab4alv REFERENCE INTO DATA(coloredrow) WHERE carrid = 'LH'.
        APPEND VALUE #( color-col = col_group ) TO  coloredrow->colored.
      ENDLOOP.

      "Applying the color settings
      TRY.
          alv->get_columns( )->set_color_column( 'COLORED' ).
        CATCH cx_salv_data_error.
      ENDTRY.

      "Setting table rows to a striped pattern
      alv->get_display_settings( )->set_striped_pattern( abap_true ).
    ENDIF.
  ENDMETHOD.

  METHOD set_text_properties.
    IF checkbox = abap_true.
      "In this example, a selection of text property-specific options is demonstrated.
      TRY.
          "Setting the text alignment of table rows
          CAST cl_salv_column( alv->get_columns( )->get_column( 'SEATSOCC' ) )->set_alignment( if_salv_c_alignment=>left ).
          "Setting blanks instead of zeroes
          CAST cl_salv_column( alv->get_columns( )->get_column( 'SEATSFREE' ) )->set_zero( abap_false ).
          CAST cl_salv_column( alv->get_columns( )->get_column( 'OCCRATE' ) )->set_zero( abap_false ).
        CATCH cx_salv_not_found.
      ENDTRY.

    ENDIF.
  ENDMETHOD.

  METHOD display_grid_lines.
    "In this example, horizontal and vertical ALV grid lines are set to be either displayed
    "or not.
    IF checkbox = abap_true.
      alv->get_display_settings( )->set_horizontal_lines( abap_true ).
      alv->get_display_settings( )->set_vertical_lines( abap_true ).
    ELSE.
      alv->get_display_settings( )->set_horizontal_lines( abap_false ).
      alv->get_display_settings( )->set_vertical_lines( abap_false ).
    ENDIF.
  ENDMETHOD.

  METHOD display_as_popup.
    "In this example, the ALV output is displayed in a dialog box. Demo coordinates
    "are provided.
    IF checkbox = abap_true.
      alv->set_screen_popup( start_column = 10
                             end_column   = 100
                             start_line   = 4
                             end_line     = 15 ).
    ENDIF.
  ENDMETHOD.

  METHOD use_functions.
    "In this example, a custom GUI status is set. All functions are set.
    "The custom GUI status is included in the cheat sheet example program.
    "If the checkbox is not selected, the generic ALV functions are set.
    IF checkbox = abap_true.

      alv->set_screen_status(
        pfstatus      = 'DEMO_STATUS_ALV'
        report        = sy-repid
        set_functions = alv->c_functions_all ).

      "You can check the results of the following method calls in the debugger.
      DATA(getfunc) = alv->get_functions(  ).
      "Checking the activation status
      DATA(is_enabled) = getfunc->is_enabled( 'GET_SELECT' ).
      "Checking the  visibility
      DATA(is_visible) = getfunc->is_visible( 'GET_SELECT' ).

      "Registering an event handler for the custom functions
      SET HANDLER lcl_events=>func_click FOR alv->get_event( ).
    ELSE.
      alv->get_functions( )->set_default( abap_true ).
    ENDIF.
  ENDMETHOD.

  METHOD enable_hotspots.
    "This example demonstrates hotspots/single click events. Here,
    "the cell type of a particular column is set as hotspot. Plus,
    "an event handler is registered to handle the hotspot click event.
    IF checkbox = abap_true.
      TRY.
          CAST cl_salv_column_table( alv->get_columns( )->get_column( 'CONNID' ) )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          CAST cl_salv_column_table( alv->get_columns( )->get_column( 'HOTSPOT' ) )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          SET HANDLER lcl_events=>click_hotspot FOR alv->get_event( ).
        CATCH cx_salv_not_found.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD handle_double_click.
    "This example demonstrates the double click event. See the implementation
    "in the local class for events. The implementation simlpy raises a
    "message.
    IF checkbox = abap_true.
      SET HANDLER lcl_events=>double_click FOR alv->get_event( ).
    ENDIF.
  ENDMETHOD.

  METHOD tooltips.
    "This example demonstrates tooltips for the following use cases:
    "- A tooltip is added to a particular column. This column is set as icon column.
    "  When you hover over the icon in the column, the tooltip is diplayed.
    "- Tooltips are added for particular column headers.
    IF checkbox = abap_true.
      TRY.
          "Tooltip for icons
          CAST cl_salv_column_table( alv->get_columns( )->get_column( 'HOTSPOT' ) )->set_icon( if_salv_c_bool_sap=>true ).

          alv->get_functional_settings( )->get_tooltips( )->add_tooltip( type    = cl_salv_tooltip=>c_type_icon
                                                                         value   = CONV lvc_value( icon_abap )
                                                                         tooltip = `This is a tooltip for an icon` ).

          "Tooltips for column headers
          CAST cl_salv_column_table( alv->get_columns( )->get_column( 'CARRID' ) )->set_tooltip( 'Tooltip 1' ).
          CAST cl_salv_column_table( alv->get_columns( )->get_column( 'CONNID' ) )->set_tooltip( 'Tooltip 2' ).
          CAST cl_salv_column_table( alv->get_columns( )->get_column( 'FLDATE' ) )->set_tooltip( 'Tooltip 3' ).
          CAST cl_salv_column_table( alv->get_columns( )->get_column( 'PRICE' ) )->set_tooltip( 'Tooltip 4' ).
        CATCH cx_salv_not_found cx_salv_existing.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

INITIALIZATION.
  "Filling demo database tables to select from
  zcl_demo_abap_flight_tables=>fill_dbtabs( ).

  "Providing text
  intro1 = 'This example demonstrates the SAP List Viewer (ALV).'.
  intro2 = 'Select checkboxes to determine ALV-related aspects and functionality.'.
  intro3 = 'After you have selected checkboxes of your choice, choose Execute/F8.'.
  t2 = 'ALV layout-related options'.
  t4 = 'Set title'.
  t6 = 'Optimize column width'.
  t7 = 'Sort a column'.
  t8 = 'Misc settings (column names, visibility, positioning)'.
  t10 = 'Selection options'.
  t11 = 'Set colors'.
  t12 = 'Set filter'.
  t13 = 'Set text properties'.
  t14 = 'Display grid lines'.
  t15 = 'Tooltips'.
  t16 = 'Display as popup'.
  t17 = 'Functions and events'.
  t18 = 'Aggregate'.
  t19 = 'Hotspots (single-click events)'.
  t20 = 'Handle double clicks'.
  t21 = 'Use custom functions (else generic functions)'.
  sscrfields-functxt_01 = 'Select all checkboxes (no popup)'.
  sscrfields-functxt_02 = 'Deselect all checkboxes'.

**********************************************************************

AT SELECTION-SCREEN.

  "For convenience, buttons are included that select most checkboxes and
  "deselect all checkboxes when clicked.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      setheadr = abap_true.
      opt = abap_true.
      sort = abap_true.
      filter = abap_true.
      colheadr = abap_true.
      selct = abap_true.
      colors = abap_true.
      filter = abap_true.
      txtprop = abap_true.
      lines = abap_true.
      tool = abap_true.
      popup = abap_false.
      aggreg = abap_true.
      hotspots = abap_true.
      doublecl = abap_true.
      func = abap_true.
    WHEN 'FC02'.
      setheadr = abap_false.
      opt = abap_false.
      sort = abap_false.
      filter = abap_false.
      colheadr = abap_false.
      selct = abap_false.
      colors = abap_false.
      filter = abap_false.
      txtprop = abap_false.
      lines = abap_false.
      tool = abap_false.
      popup = abap_false.
      aggreg = abap_false.
      hotspots = abap_false.
      doublecl = abap_false.
      func = abap_false.
  ENDCASE.

**********************************************************************

START-OF-SELECTION.

  CLEAR alv.

  "Populating internal table
  SELECT carrid, connid, fldate, price, currency, planetype, seatsmax, seatsocc
    FROM zdemo_abap_fli
    ORDER BY carrid, connid
    INTO CORRESPONDING FIELDS OF TABLE @itab4alv.

  "Providing values for a particular column for demo purposes
  LOOP AT itab4alv REFERENCE INTO DATA(ref).
    ref->hotspot = icon_abap.
  ENDLOOP.

  "Instantiating an ALV table object
  "Note: In this example, the ALV is not included in a container. Therefore,
  "the other exporting parameters are irrelevant. The list_display parameter is
  "used to set the display type, which is set here based on user input.
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = alv
        CHANGING
          t_table      = itab4alv ).

      "The following methods include the actual implementations for the ALV display and functionality.
      "Based on the selection of the checkboxes on the selection screen, the implementation is either
      "used or not. See comments in the method implementations.
      lcl_alv_demo=>set_header( setheadr ).
      lcl_alv_demo=>optimize( opt ).
      lcl_alv_demo=>sort( sort ).
      lcl_alv_demo=>misc_column( colheadr ).
      lcl_alv_demo=>set_filter( filter ).
      lcl_alv_demo=>aggregate( aggreg ).
      lcl_alv_demo=>selection( selct ).
      lcl_alv_demo=>coloring( colors ).
      lcl_alv_demo=>set_text_properties( txtprop ).
      lcl_alv_demo=>display_grid_lines( lines ).
      lcl_alv_demo=>display_as_popup( popup ).
      lcl_alv_demo=>use_functions( func ).
      lcl_alv_demo=>enable_hotspots( hotspots ).
      lcl_alv_demo=>handle_double_click( doublecl ).
      lcl_alv_demo=>tooltips( tool ).

      "Displaying the ALV
      alv->display( ).
    CATCH cx_root INTO DATA(error).
      "Note: For simplicity, this example uses the root exception class. Always make
      "sure that you use appropriate exception classes.
      MESSAGE error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
