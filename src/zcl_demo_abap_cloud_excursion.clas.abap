***********************************************************************
*
*    ABAP cheat sheet: Excursions into ABAP for Cloud Development
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate a selected set of released APIs and libraries
*   with the restricted ABAP language version
* - NOTE: The example class includes constants in the private visbility
*         section. The names specified there are used by code snippets.
*         Make sure that you insert suitable values before running the
*         example class.
*
* ----------------------- GETTING STARTED -----------------------------
* - Open the class with the ABAP development tools for Eclipse (ADT).
* - Choose F9 to run the class.
* - Check the console output.
* - Due to the amount of output in the console, the examples include
*   numbers (e. g. 1) ..., 2) ..., 3) ...) for the individual example
*   sections. Plus, the variable name is displayed in several cases.
*   Hence, to easier and faster find the relevant output in the console,
*   just search in the console for the number/variable name (CTRL+F in
*   the console) or use the debugger.
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
"! <p class="shorttext synchronized">ABAP cheat sheet: Excursions into ABAP for Cloud Development</p>
"! Example to demonstrate released APIs and libraries with the restricted ABAP language version.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_cloud_excursion DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF details_struc,
             name    TYPE string,
             details TYPE string,
           END OF details_struc.

    CLASS-DATA details_tab TYPE TABLE OF details_struc WITH EMPTY KEY.
    CLASS-DATA str TYPE string.
    CLASS-DATA infos TYPE string_table.
    CLASS-METHODS heading IMPORTING text          TYPE string
                          RETURNING VALUE(output) TYPE string.

    "NOTE:
    "The names specified for the following constants are used by several
    "code snippets in the example class. Make sure that you insert suitable
    "values before running the example class.

    "Package name: Use the package name in which you cloned the ABAP cheat sheets,
    "for example, ZABAP_CHEAT_SHEETS.
    CONSTANTS package TYPE sxco_package VALUE 'ZABAP_CHEAT_SHEETS'.

    "The following constants are relevant for the ABAP repository object generation.
    "See the comments in the section further down.
    "Name of the package in which you want to generate the repository objects
    CONSTANTS package4gen TYPE sxco_package VALUE 'ZABAP_CHEAT_SHEETS'.
    "ID of a modifiable transport request
    CONSTANTS tr_req_id TYPE sxco_transport VALUE 'ID0A123456'.
    "Name of the data element to be created
    CONSTANTS gen_dtel TYPE sxco_ad_object_name VALUE 'ZDEMO_ABAP_DTEL'.
    "Name of the domain to be created
    CONSTANTS gen_doma TYPE sxco_ad_object_name VALUE 'ZDEMO_ABAP_STATUS'.
    "Name of the database table to be created
    CONSTANTS gen_tabl TYPE sxco_dbt_object_name VALUE 'ZDEMO_ABAP_BOOK'.
    "To enable the generation, specify "abap_true" for the contant value.
    CONSTANTS generation_ok TYPE abap_bool VALUE abap_false.

ENDCLASS.



CLASS zcl_demo_abap_cloud_excursion IMPLEMENTATION.


  METHOD heading.
    output = |\n_________________________________________________________________________________\n\n{ text }\n\n|.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    out->write( |ABAP Cheat Sheet Example: Excursions into ABAP for Cloud Development\n| ).
    out->write( `1) Restrictions in ABAP for Cloud Development` ).
    out->write( |\nYou can check the code of this section by commenting it in and out. Note the comments in the code.| ).
    "The following statements demonstrate a selection of restrictions in ABAP for Cloud Development.
    "Comment in the code lines (CTRL + 7) up to BREAK-POINT and check the syntax errors/warnings
    "displayed.
    "Notes about the errors/warnings:
    "- Using ABAP SQL statements, data is retrieved from database tables. The first table
    "  is a demo table provided by SAP. This table cannot be accessed directly (unlike in classic
    "  ABAP) and therefore cannot be used as a data source from which to select. The second
    "  is a database table from the ABAP Cheat Sheet GitHub repository. If you have imported
    "  the repository into the system, you can use it as a data source.
    "- The example includes dynamic ABAP SQL statements. This is just to emphasize that such
    "  statements can lead to unwanted results, since in this case you cannot select from the
    "  first database table whose name is specified dynamically. Therefore, there is no syntax
    "  error or warning at compile time, but an error at runtime. You can try the following:
    "  Comment out all code except the lines with the dynamic statements, activate the code
    "  and execute the class with F9. Note: Check out the CL_ABAP_DYN_PRG class, which supports
    "  dynamic programming by checking the validity of dynamic specifications.
    "- Further examples of obsolete and invalid syntax in ABAP for Cloud Development include
    "  the invalid MOVE ... TO ... statement and others. To set breakpoints in ADT, double-click
    "  the area to the left of the code line number.


*    SELECT carrid, connid FROM spfli WHERE carrid = 'LH' INTO TABLE @DATA(it1).
*    SELECT carrid, connid FROM zdemo_abap_fli WHERE carrid = 'LH' INTO TABLE @DATA(it2).
*    SELECT SINGLE carrid, connid FROM ('SPFLI') WHERE carrid = 'LH' INTO NEW @DATA(wa1).
*    SELECT SINGLE carrid, connid FROM ('ZDEMO_ABAP_FLI') WHERE carrid = 'LH' INTO NEW @DATA(wa2).
*
*    DATA(num1) = 1.
*    DATA(num2) = 1.
*    DATA(num3) = 2.
*
*    MOVE num3 TO num1.
*    num2 = num3.
*
*    DATA(it3) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).
*    DESCRIBE TABLE it3 LINES DATA(num_lines1).
*    DATA(num_lines2) = lines( it3 ).
*
*    DATA: ref1 TYPE REF TO i,
*          ref2 TYPE REF TO i.
*    ref1 = REF #( num1 ).
*    GET REFERENCE OF num1 INTO ref2.
*
*    DATA str_itab TYPE string_table.
*    READ REPORT 'ZCL_DEMO_ABAP_UNIT_TEST=======CCAU' INTO str_itab.
*    WRITE 'hi'.
*    BREAK-POINT.

**********************************************************************

    out->write( heading( `2) Using released APIs` ) ).
    "The following code uses several released APIs.
    "You can check out Released Objects in the Project Explorer in ADT in
    "ABAP Cloud.

    "In the example, a released API is used to return the current date in UTC.
    "You can use forward navigation by placing the cursor on the class,
    "choosing CTRL and clicking. In the opened class, go to the Properties
    "tab and choose API state to find the release contract for this class.
    DATA(date) = cl_abap_context_info=>get_system_date(  ).
    DATA(current_year) = date(4).
    DATA(up_to_year) = 2050.

    "The statement retrieves the leap years between the current year and
    "the specified year. A released CDS view is used as data source.
    SELECT calendaryear
      FROM i_calendaryear
      WHERE calendaryear BETWEEN @current_year AND @up_to_year
      AND IsLeapYear IS NOT INITIAL
      INTO TABLE @DATA(future_leap_years).

    "To display output, the example class uses a released API. You can implement
    "the interface if_oo_adt_classrun and the main method. The write method displays
    "content in the console when running the class using F9 in ADT.
    out->write( data = future_leap_years name = `future_leap_years` ).

    "Using a released data element
    DATA number TYPE int4.
    number = lines( future_leap_years ).

    out->write( |\nBetween { current_year } and { up_to_year }, there are { number } leap years.| ).

    "Using a released API as data source of a SELECT statement
    "Among others, the month names of specified languages are retrieved in the example.
    "The WHERE clause is specified in a way to only retrieve a selected set of months.
    "For this purpose, more released APIs (classes cl_abap_random_int, cl_abap_random)
    "are used. A minimum of 5 months and a maximum of all months is to be returned.
    DATA(number_of_months) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 5
                                     max = 12 )->get_next( ).

    SELECT CalendarMonth, CalendarMonthName, Language
      FROM i_calendarmonthtext
      WHERE Language IN ( 'E', 'I', 'D' )
      AND CalendarMonth <= @number_of_months
      ORDER BY Language, CalendarMonth
      INTO TABLE @DATA(months).

    "Using a released table type
    DATA string_tab TYPE string_table.

    "Inserting the retrieved data into a table of type string
    LOOP AT months INTO DATA(month_wa).
      APPEND |{ month_wa-CalendarMonth } { month_wa-CalendarMonthName } ({ month_wa-Language })| TO string_tab.
    ENDLOOP.

    "Creating a JSON string from a data object using a released API
    DATA(json_str) = xco_cp_json=>data->from_abap( months )->to_string( ).

    out->write( |\nNumber of months per language: { number_of_months }| ).
    out->write( |\nMonths returned:| ).
    out->write( data = string_tab name = `string_tab` ).
    out->write( |\nMonths returned (JSON string):| ).
    out->write( data = json_str name = `json_str` ).

    "Getting APIs for use in ABAP for Cloud Development
    "The released CDS view contains the relevant information. In the example,
    "the names of released classes are retrieved with a specific name pattern.
    SELECT ReleasedObjectType, ReleasedObjectName, ReleaseState
      FROM i_apisforclouddevelopment
      WHERE releasestate = 'RELEASED'
      AND ReleasedObjectType = 'CLAS'
      AND ReleasedObjectName LIKE 'CL_ABAP_RANDOM_P%'
      INTO TABLE @DATA(rel_cl_abap_random).

    out->write( |\nRead result:| ).
    out->write( data = rel_cl_abap_random name = `rel_cl_abap_random` ).

    "Getting the number of all released classes in the system
    SELECT COUNT(*)
      FROM i_apisforclouddevelopment
      WHERE releasestate = 'RELEASED'
      AND ReleasedObjectType = 'CLAS'
      INTO @DATA(num_rel_cl).

    "Getting the number of all released interfaces in the system
    SELECT COUNT(*)
      FROM i_apisforclouddevelopment
      WHERE releasestate = 'RELEASED'
      AND ReleasedObjectType = 'INTF'
      INTO @DATA(num_rel_intf).

    out->write( |\nThere are { num_rel_cl } released classes and { num_rel_intf } released interfaces in the system.|  ).

**********************************************************************

    out->write( heading( `Excursions into the XCO Library` ) ).
    "The following code snippets focus on the XCO library that provides
    "predefined functionality and can be used in ABAP for Cloud Development.
    "The examples cover a selection of options for you to explore. For more
    "detailed information and more code snippets, see the SAP Help Portal
    "documentation. In most cases, the examples deal with classes and methods
    "for getting information about repository objects.
    "Note:
    "- Due to the many methods and options, method chaining comes in handy.
    "- To further explore the options with the classes and methods, you can
    "  put the cursor behind => or -> and choose CTRL + Space to get input
    "  suggestions.

    out->write( |3) Getting Repository Object-Related Information| ).

    "Getting all accessible repository objects in the system (indicated by the
    "value provided for "in")
    "To further process the returned values, you can loop over them.
    DATA(all_obj) = xco_cp_abap_repository=>objects->all->in( xco_cp_abap=>repository )->get( ).

    out->write( |\nThere are { lines( all_obj ) } accessible repository objects in the system. Note that this number also includes custom artifacts.| ).

**********************************************************************

    "Getting all accessible database tables
    "You can specify concrete artifacts (check CTRL + Space after objects->)
    DATA(all_tables) = xco_cp_abap_repository=>objects->tabl->all->in( xco_cp_abap=>repository )->get( ).

    out->write( |\nThere are { lines( all_tables ) } accessible database tables in the system. Note that this number includes custom database tables.| ).

**********************************************************************

    "Checking the existence of a certain package
    "Note: The value provided for 'for' is specified in the private section
    "      of the class. Use the package name in which you cloned the ABAP
    "      cheat sheets, for example, ZABAP_CHEAT_SHEETS.

    "Creating a handler for the specified package
    "You can create a break point here and check out the content of the variable
    "in the debugger to find out what information is included.
    DATA(package_handle) = xco_cp_abap_repository=>package->for( package ).
    DATA(package_exists) = package_handle->exists( ).
    "Using method chaining, you can retreive the information in one go.
    DATA(package_exists_ch) = xco_cp_abap_repository=>package->for( package )->exists( ).

    IF package_exists IS NOT INITIAL
    AND package_exists_ch IS NOT INITIAL.
      out->write( |\nThe package { package } exists in the system.| ).
    ELSE.
      out->write( |\nThe package { package } does not exist in the system.| ).
    ENDIF.

**********************************************************************

    "Getting more information
    IF package_exists IS NOT INITIAL.
      DATA(package_name) = package_handle->name.
      DATA(software_comp) = package_handle->read( )-property-software_component->name.

      out->write( |\nThe package name is { package_name } (retrieved using the "name" attribute).| ).
      out->write( |\nIts software component is { software_comp }.| ).

      "Getting all repository objects contained in the package
      DATA(all_objs) = xco_cp_abap_repository=>objects->all->in( package_handle )->get( ).

      "Looping over the result for further processing
      "In this example, the name of the repository objects and their types are
      "extracted and inserted in a table for displaying purposes.
      LOOP AT all_objs ASSIGNING FIELD-SYMBOL(<a>).
        APPEND VALUE #( name = <a>->name->value
                        details = <a>->type->value ) TO details_tab.
      ENDLOOP.

      SORT details_tab BY name.

      out->write( |\nThe package { package } includes the following { lines( details_tab ) } repository objects:| ).
      out->write( details_tab ).
    ENDIF.

**********************************************************************

    "Refining the search using a filter and search pattern
    "The search patterns uses a name from the ABAP cheat sheets repository objects.
    DATA(filter1) = xco_cp_abap_repository=>object_name->get_filter(
        xco_cp_abap_sql=>constraint->contains_pattern( 'ZDEMO_ABAP_RAP_R%' ) ).

    "Getting all accessible BDEFs that start with ZDEMO_ABAP_RAP_R in the entire system
    "As shown above, loop over the handler to get more detailed information. You can also set
    "a break point to check out the content of the variable in the debugger.
    IF package_exists = abap_true.
      DATA(bdefs_in_package) = xco_cp_abap_repository=>objects->bdef->where( VALUE #( ( filter1 )
        ) )->in( xco_cp_abap=>repository )->get( ).

      out->write( |\nThere are { lines( bdefs_in_package ) } accessible BDEFs with the specified name pattern in the entire system.| ).
    ENDIF.

    "Getting all accessible interfaces with a particular name pattern in the entire system
    DATA(filter2) = xco_cp_abap_repository=>object_name->get_filter(
      xco_cp_abap_sql=>constraint->contains_pattern( 'IF_ABAP_BEHV%' ) ).

    DATA(all_intfs) = xco_cp_abap_repository=>objects->intf->where( VALUE #( ( filter2 )
      ) )->in( xco_cp_abap=>repository )->get( ).

    out->write( |\nThere are { lines( all_intfs ) } accessible interfaces with the specified name pattern in the entire system.| ).

    "Getting repository objects based on a particular software component
    "In the example, all classes are retrieved that have a particular name pattern
    "and have a particular software component.
    DATA(filter3) = xco_cp_system=>software_component->get_filter( xco_cp_abap_sql=>constraint->equal( 'SAP_BASIS' ) ).
    DATA(filter4) = xco_cp_abap_repository=>object_name->get_filter( xco_cp_abap_sql=>constraint->contains_pattern( '%CL_ABAP_RAND%' ) ).
    DATA(filtered_classes) = xco_cp_abap_repository=>objects->clas->where( VALUE #( ( filter3 ) ( filter4 )
      ) )->in( xco_cp_abap=>repository )->get( ).

    out->write( |\nThere are { lines( filtered_classes ) } accessible classes with the specified name pattern and software component.| ).

**********************************************************************

    out->write( heading( `4) Getting Database Table-Related Information` ) ).

    "Creating a filter with a search pattern
    DATA(pattern) = 'ZDEMO_ABAP_FL%'.
    DATA(filter5) = xco_cp_abap_repository=>object_name->get_filter(
      xco_cp_abap_sql=>constraint->contains_pattern( pattern ) ).

    "Getting all database tables whose names begin with the specified name pattern
    "and that are contained in the package specified above.
    DATA(dbtabs) = xco_cp_abap_repository=>objects->tabl->database_tables->where( VALUE #( ( filter5 )
      ) )->in( package_handle )->get( ).

    IF dbtabs IS NOT INITIAL.

      infos = VALUE #( ( |Information about database tables retrieved using search pattern { pattern }| ) ).
      LOOP AT dbtabs INTO DATA(db).
        "Getting the name of the database table
        DATA(a1_name) = db->name.
        "Getting information about technical properties (selection)
        DATA(a2_content) = db->content( )->get( ).
        DATA(a3_descr) = a2_content-short_description.
        DATA(a4_del_cl) = a2_content-delivery_class->value.
        DATA(a5_data_maint) = a2_content-data_maintenance->if_xco_printable~get_text( )->get_lines( )->join( )->value.
        "Getting all fields of the database table
        "The handler is further processed. In the example, only the field names are retreived.
        DATA(a6_fields) = db->fields->all->get( ).

        CLEAR str.
        LOOP AT a6_fields INTO DATA(fields).
          str = str && COND #( WHEN sy-tabix <> 1 THEN `, ` ELSE `` ) && fields->name.
        ENDLOOP.

        infos = VALUE #( BASE infos
         ( `*********************************` )
         ( |Db table name: { a1_name }| )
         ( |Description: { a3_descr }| )
         ( |Delivery class: { a4_del_cl }| )
         ( |Data maintenance: { a5_data_maint }| )
         ( |Field names: { str }| ) ).

      ENDLOOP.
      out->write( infos ).
      CLEAR infos.
    ELSE.
      out->write( |\nNo database tables found.| ).
    ENDIF.

**********************************************************************

    "Getting information about a particular database table
    out->write( |\nInformation about a particular demo database table| ).
    DATA(db_table) = CONV sxco_dbt_object_name( 'ZDEMO_ABAP_CARR' ).
    DATA(b1_handler) = xco_cp_abap_dictionary=>database_table( db_table ).
    DATA(b2_exists) = b1_handler->exists( ).
    IF b2_exists IS NOT INITIAL.
      DATA(b3_name) = b1_handler->name.
      DATA(b4_descr) = b1_handler->content( )->get_short_description( ).
      DATA(b5_del_cl) = b1_handler->content( )->get_delivery_class( )->value.
      DATA(b6_data_maint) = a2_content-data_maintenance->if_xco_printable~get_text( )->get_lines( )->join( )->value.
      DATA(b7_fields) = b1_handler->fields->all->get_names( ).
      DATA(b8_keys) = b1_handler->fields->key->get_names( ).
      DATA(b9_vis) = b1_handler->get_api_state( )->get_visibilities( ).
      DATA(b10_rel_state) = b1_handler->get_api_state( )->get_release_state( )->value.

      CLEAR str.
      "Getting information about the visibility of the repository object
      LOOP AT b9_vis INTO DATA(vis_b).
        CASE abap_true.
          WHEN vis_b->use_in_key_user_apps( ).
            str = |Key user: { vis_b->use_in_key_user_apps( ) }|.
          WHEN vis_b->use_in_sap_cloud_platform( ).
            str = str && | Cloud: { vis_b->use_in_sap_cloud_platform( ) }|.
        ENDCASE.
      ENDLOOP.

      infos = VALUE #(
       ( |Information about database table { db_table }| )
       ( `*********************************` )
       ( |Db table name: { b3_name }| )
       ( |Description: { b4_descr }| )
       ( |Delivery class: { b5_del_cl }| )
       ( |Data maintenance: { b6_data_maint }| )
       ( |Visibilities: { COND #( WHEN str IS INITIAL THEN `None` ELSE str ) }| )
       ( |Release state: { b10_rel_state }| )
       ( |Field names: { concat_lines_of( table = b7_fields sep = `, ` ) }| )
       ( |Key fields: { concat_lines_of( table = b8_keys sep = `, ` ) }| )
       ).

      out->write( infos ).
      CLEAR infos.
    ELSE.
      out->write( |\nThe database table { db_table } does not exist.| ).
    ENDIF.

**********************************************************************

    out->write( |\nInformation about a particular database table field| ).

    IF b2_exists IS NOT INITIAL.
      db_table = 'ZDEMO_ABAP_CARR'.
      DATA(db_field) = CONV sxco_ad_field_name( 'CARRID' ).
      DATA(c1_handler) = xco_cp_abap_dictionary=>database_table( db_table )->field( db_field )->content( ).

      "Getting attributes of the field
      DATA(c2_field_attributes) = c1_handler->get( ).
      DATA(c3_is_key) = c2_field_attributes-key_indicator.
      DATA(c4_is_not_null) = c2_field_attributes-not_null.
      "You can get the information also using a different way, as shown below.
      DATA(c5_built_in_t) = c2_field_attributes-type->get_built_in_type( )->abap_type->get_type_descriptor( )->type_kind.
      "Getting information using the handler created initially and for getting attributes individually.
      DATA(c6_field_name) = c1_handler->field->name.
      DATA(c7_db_name) = c1_handler->field->database_table->name.
      DATA(c8_is_key) = c1_handler->get_key_indicator( ).
      "You may want to check the details for the following variable in the debugger.
      DATA(c9_type_handler) = c1_handler->get_type( ).
      DATA(c10_built_in_t) = c9_type_handler->get_built_in_type( )->abap_type->get_type_descriptor( )->type_kind.
      DATA(c11_type_outp_len) = c9_type_handler->get_built_in_type( )->abap_type->get_type_descriptor( )->output_length.
      DATA(c12_type_abs_name) = c9_type_handler->get_built_in_type( )->abap_type->get_type_descriptor( )->absolute_name.

      infos = VALUE #(
           ( |Information about field { db_field } in database table { db_table }| )
           ( `*********************************` )
           ( |Field name: { c6_field_name }| )
           ( |Db table name: { c7_db_name }| )
           ( |Is key field: { c3_is_key } (Alternative retrieval: { c8_is_key })| )
           ( |Is not null: { c4_is_not_null }| )
           ( |Built-in type of field: { c5_built_in_t } (Alternative retrieval: { c10_built_in_t })| )
           ( |Output length: { c11_type_outp_len }| )
           ( |Absolute name of type: { c12_type_abs_name }| ) ).

      out->write( infos ).
      CLEAR infos.
    ELSE.
      out->write( |\nThe database table { db_table } does not exist.| ).
    ENDIF.

**********************************************************************

    out->write( heading( `5) Getting Data Element-Related Information` ) ).

    "Getting information about a released data element
    DATA(dtel) = CONV sxco_ad_object_name( 'MANDT' ).
    DATA(d1_handler) = xco_cp_abap_dictionary=>data_element( dtel ).
    DATA(d2_exists) = d1_handler->exists( ).
    IF d2_exists IS NOT INITIAL.
      DATA(d3_name) = d1_handler->name.
      DATA(d4_vis) = d1_handler->get_api_state( )->get_visibilities( ).
      DATA(d5_rel_state) = d1_handler->get_api_state( )->get_release_state( )->value.
      DATA(d6_descr) = d1_handler->content( )->get_short_description( ).
      DATA(d7_built_in_t) = d1_handler->content( )->get_underlying_built_in_type( )->abap_type->get_type_descriptor( )->type_kind.
      DATA(d8_type_outp_len) = d1_handler->content( )->get_underlying_built_in_type( )->abap_type->get_type_descriptor( )->output_length.

      CLEAR str.
      "Processing visibility information
      LOOP AT d4_vis INTO DATA(vis_d).
        CASE abap_true.
          WHEN vis_d->use_in_key_user_apps( ).
            str = |Key user: { vis_d->use_in_key_user_apps( ) }|.
          WHEN vis_d->use_in_sap_cloud_platform( ).
            str = str && | Cloud: { vis_d->use_in_sap_cloud_platform( ) }|.
        ENDCASE.
      ENDLOOP.

      infos = VALUE #(
         ( |Information about data element { dtel }| )
         ( `*********************************` )
         ( |Data element name: { d3_name }| )
         ( |Short description: { d6_descr }| )
         ( |Visibilities: { COND #( WHEN str IS INITIAL THEN `None` ELSE str ) }| )
         ( |Release state: { d5_rel_state }| )
         ( |Built-in type: { d7_built_in_t }| )
         ( |Output length: { d8_type_outp_len }| ) ).

      out->write( infos ).
      CLEAR infos.
    ELSE.
      out->write( |\nThe data element { dtel } does not exist.| ).
    ENDIF.

**********************************************************************

    out->write( heading( `6) Getting Table Type-Related Information` ) ).

    DATA(tab_type) = CONV sxco_ad_object_name( 'STRING_TABLE' ).
    DATA(e1_handler) = xco_cp_abap_dictionary=>table_type( tab_type ).
    DATA(e2_exists) = e1_handler->exists( ).
    IF e2_exists IS NOT INITIAL.
      DATA(e3_name) = e1_handler->name.
      DATA(e4_vis) = e1_handler->get_api_state( )->get_visibilities( ).
      DATA(e5_rel_state) = e1_handler->get_api_state( )->get_release_state( )->value.
      DATA(e6_descr) = e1_handler->content( )->get_short_description( ).
      DATA(e7_tab_key) = e1_handler->content( )->get_primary_key( )->key_definition->value.
      DATA(e8_tab_key) = e1_handler->content( )->get_primary_key( )->key_definition->if_xco_printable~get_text( )->get_lines( )->join( )->value.
      DATA(e9_type_abs_name) = e1_handler->content( )->get_row_type( )->get_built_in_type( )->abap_type->get_type_descriptor( )->absolute_name.

      CLEAR str.
      "Processing visibility information
      LOOP AT e4_vis INTO DATA(vis_e).
        CASE abap_true.
          WHEN vis_e->use_in_key_user_apps( ).
            str = |Key user: { vis_e->use_in_key_user_apps( ) }|.
          WHEN vis_e->use_in_sap_cloud_platform( ).
            str = str && | Cloud: { vis_e->use_in_sap_cloud_platform( ) }|.
        ENDCASE.
      ENDLOOP.

      infos = VALUE #(
         ( |Information about table type { tab_type }| )
         ( `*********************************` )
         ( |Table type name: { e3_name }| )
         ( |Short description: { e6_descr }| )
         ( |Visibilities: { COND #( WHEN str IS INITIAL THEN `None` ELSE str ) }| )
         ( |Release state: { e5_rel_state }| )
         ( |Table key: { e7_tab_key } (i.e. { e8_tab_key })| )
         ( |Absolute type name of underlying built-in type: { e9_type_abs_name }| ) ).

      out->write( infos ).
      CLEAR infos.
    ELSE.
      out->write( |\nThe table type { tab_type } does not exist.| ).
    ENDIF.

**********************************************************************

    out->write( heading( `7) Getting CDS View Entity-Related Information` ) ).

    DATA(cds) = CONV sxco_cds_object_name( 'ZDEMO_ABAP_CDS_VE_ASSOC' ).
    DATA(f1_handler) = xco_cp_cds=>view_entity( cds ).
    DATA(f2_exists) = f1_handler->exists( ).
    IF f2_exists IS NOT INITIAL.
      DATA(f3_name) = f1_handler->name.
      "You may want to check the details for the following variable in the debugger.
      DATA(f4_content) = f1_handler->content( )->get( ).
      "Fields of the CDS view entity
      DATA(f5_field_handler) = f1_handler->fields->all->get( ).
      DATA(f6_get_field_names) = f1_handler->fields->all->get_names( ).
      DATA(f7_field_names) = concat_lines_of( table = f6_get_field_names sep = `, ` ).

      CLEAR infos.
      "Getting field information using the field handler
      LOOP AT f5_field_handler INTO DATA(cds_field).
        DATA(f8_field_name) = cds_field->name.
        APPEND f8_field_name TO infos.
        "The demo CDS view entity does not have much included for displaying purposes.
        "Therefore, you may create a break point here and check the variable content in
        "the debugger for various pieces of information that are accessible.
        DATA(f9_field_content) = cds_field->content( )->get( ).
      ENDLOOP.

      DATA(f10_field_names) = concat_lines_of( table = infos sep = `, ` ).

      "Getting the associations of the view entity
      DATA(f11_assoc) = f1_handler->associations->all->get( ).

      CLEAR infos.
      LOOP AT f11_assoc INTO DATA(assoc).
        DATA(f12_assoc_name) = assoc->name.
        APPEND f12_assoc_name TO infos.
        "You may want to check the details for the following variable in the debugger.
        DATA(f13_assoc_details) = assoc->content( )->get( ).
      ENDLOOP.

      DATA(f14_assocs) = concat_lines_of( table = infos sep = `, ` ).

      infos = VALUE #(
         ( |Information about CDS view entity { cds }| )
         ( `*********************************` )
         ( |CDS view entity name: { f3_name }| )
         ( |Field names: { f7_field_names }| )
         ( |Alternative field name retrieval: { f10_field_names }| )
         ( |Association names: { f14_assocs }| ) ).

      out->write( infos ).
      CLEAR infos.
    ELSE.
      out->write( |\nThe CDS view entity { cds } does not exist.| ).
    ENDIF.

**********************************************************************

    out->write( |\nCDS View Entity Example with Parameters| ).

    cds = 'ZDEMO_ABAP_CDS_VE_SEL'.
    DATA(g1_handler) = xco_cp_cds=>view_entity( cds ).
    DATA(g2_exists) = g1_handler->exists( ).
    IF g2_exists IS NOT INITIAL.
      DATA(g3_name) = g1_handler->name.

      "Getting parameters
      DATA(g4_params) = g1_handler->parameters->all->get( ).
      LOOP AT g4_params INTO DATA(param).
        DATA(g5_param_name) = param->content( )->get_original_name( ).
        DATA(g6_param_built_in_type) = param->content( )->get_data_type( )->is_built_in_type( ).
        IF g6_param_built_in_type IS NOT INITIAL.
          DATA(g7_type_abs_name) = param->content( )->get_data_type( )->get_built_in_type( )->abap_type->get_type_descriptor( )->absolute_name.
        ENDIF.
        APPEND |{ g5_param_name } { COND #( WHEN g6_param_built_in_type IS NOT INITIAL THEN `(Absolute type name: ` && g7_type_abs_name && `)` ELSE `` ) } | TO infos.
      ENDLOOP.

      DATA(g8_all_params) = concat_lines_of( table = infos sep = `, ` ).

      infos = VALUE #(
       ( |Information about CDS view entity { cds }| )
       ( `*********************************` )
       ( |CDS view entity name: { g3_name }| )
       ( |Parameter: { g8_all_params }| ) ).

      out->write( infos ).
      CLEAR infos.
    ELSE.
      out->write( |\nThe CDS view entity { cds } does not exist.| ).
    ENDIF.

**********************************************************************

    out->write( |\nCDS View Entity Example with Compositions| ).

    cds = 'ZDEMO_ABAP_RAP_RO_M'.
    DATA(h1_handler) = xco_cp_cds=>view_entity( cds ).
    DATA(h2_exists) = h1_handler->exists( ).
    IF h2_exists IS NOT INITIAL.
      DATA(h3_name) = h1_handler->name.

      DATA(h4_comps) = h1_handler->compositions->all->get( ).
      LOOP AT h4_comps INTO DATA(comp).

        DATA(h5_entity_name) = comp->entity->name.
        DATA(h6_fields) = comp->entity->fields->all->get_names( ).
        DATA(h7_target_name) = comp->target.
        DATA(h8_cardinality_max) = comp->content( )->get( )-cardinality-max.
        DATA(h9_cardinality_min) = comp->content( )->get( )-cardinality-min.
        DATA(h10_alias) = comp->content( )->get( )-alias.

        APPEND |Entity: { h5_entity_name } Fields: { concat_lines_of( table = h6_fields sep = `, ` ) } | &&
        |Target: { h7_target_name } Cardinality: Min { h9_cardinality_min } Max { h8_cardinality_max } | &&
        |Alias: { h10_alias } | TO infos.
      ENDLOOP.

      DATA(h11_comps) = concat_lines_of( table = infos sep = `, ` ).

      infos = VALUE #(
       ( |Information about CDS view entity { cds }| )
       ( `*********************************` )
       ( |CDS view entity name: { h3_name }| )
       ( |Compositions: { h11_comps }| ) ).

      out->write( infos ).
      CLEAR infos.
    ELSE.
      out->write( |\nThe CDS view entity { cds } does not exist.| ).
    ENDIF.

**********************************************************************

    out->write( heading( `8) Getting Interface-Related Information` ) ).

    "Getting a list of all implementations of a given interface
    DATA(intf) = CONV sxco_ao_object_name( 'ZDEMO_ABAP_OBJECTS_INTERFACE' ).
    DATA(i1_handler) = xco_cp_abap=>interface( intf ).
    DATA(i2_exist) = i1_handler->exists( ).
    IF i2_exist IS NOT INITIAL.
      DATA(i3_name) = i1_handler->name.

      "Getting static data object declarations
      DATA(i4_stat_dobj) = i1_handler->components->class_data->all->get( ).

      CLEAR str.
      LOOP AT i4_stat_dobj INTO DATA(do).
        str = str && COND #( WHEN sy-tabix <> 1 THEN `, ` ELSE ``  ) && do->name.
      ENDLOOP.

      DATA(i5_stat_dobjs) = str.
      CLEAR str.

      "Getting instance methods
      DATA(i5_in_meth) = i1_handler->components->method->all->get( ).
      LOOP AT i5_in_meth INTO DATA(imeth).
        DATA(i6_in_meth_name) = imeth->name.

        str = str && COND #( WHEN sy-tabix <> 1 THEN ` / ` ELSE ``  ) && imeth->name.

        "Getting formal parameter names using get_names
        DATA(params) = imeth->parameters->all->get_names( ).

        "Getting formal parameters
        DATA(i7_formal_params) = imeth->parameters->all->get( ).
        DATA i8_p TYPE string.
        LOOP AT i7_formal_params INTO DATA(p).
          DATA(i9_pname) = p->name.
          DATA(i10_ptype) = p->content( )->get_kind( )->if_xco_printable~get_text( )->get_lines( )->join( )->value.
          APPEND |{ i9_pname } ({ i10_ptype })| TO infos.
        ENDLOOP.

        str = |{ str } (Parameters: { concat_lines_of( table = infos sep = ` ` ) })|.
        CLEAR infos.
      ENDLOOP.

      "Getting information about where the interface is implemented
      "You may want to check the details for the following variable in the debugger.
      DATA(i11_intf_impl_get) = i1_handler->implementations->all->get( ).
      DATA(i11_intf_impl_names) = i1_handler->implementations->all->get_names( ).

      infos = VALUE #(
       ( |Information about interface { intf }| )
       ( `*********************************` )
       ( |Interface name: { i3_name }| )
       ( |Static data objects (CLASS-DATA) in interface: { i5_stat_dobjs }| )
       ( |Instance methods (METHODS) in interface: { str }| )
       ( |Implementations of the interface exist here: { concat_lines_of( table = i11_intf_impl_names sep = `, ` ) }| ) ).

      out->write( infos ).
      CLEAR: infos, str.
    ELSE.
      out->write( |\nThe interface { intf } does not exist.| ).
    ENDIF.

**********************************************************************

    out->write( heading( `9) Getting Class-Related Information` ) ).

    DATA(cl) = CONV sxco_ao_object_name( 'ZCL_DEMO_ABAP_UNIT_TEST' ).
    DATA(j1_handler) = xco_cp_abap=>class( cl ).
    DATA(j2_exists) = j1_handler->exists( ).
    IF j2_exists IS NOT INITIAL.
      DATA(j3_name) = j1_handler->name.
      "Accessing definition part
      "You may want to check the details for the following variable in the debugger.
      DATA(j4_name) = j1_handler->definition->content( )->get( ).
      "Getting instance methods in the private visibility section
      DATA(j5_in_meth_priv) = j1_handler->definition->section-private->components->method->all->get( ).

      CLEAR details_tab.
      LOOP AT j5_in_meth_priv INTO DATA(in_meth_priv).
        DATA(j6_in_meth_priv_name) = in_meth_priv->name.
        APPEND VALUE #( name = j6_in_meth_priv_name ) TO details_tab.
        "Getting formal paramter information
        DATA(j7_params) = in_meth_priv->parameters->all->get( ).
        DATA j8_form_param TYPE string.
        CLEAR j8_form_param.
        LOOP AT j7_params INTO DATA(par).
          DATA(j9_par_name) = par->name.
          DATA(j10_par_kind) = par->content( )->get_kind( )->if_xco_printable~get_text( )->get_lines( )->join( )->value.
          DATA(j11_par_type) = par->content( )->get_typing_definition( )->get_value( ).
          j8_form_param = j8_form_param && |{ j9_par_name } (Kind: { j10_par_kind }, Type: { j11_par_type }) / |.
        ENDLOOP.
        REPLACE PCRE `/\s$` IN j8_form_param WITH ``.
        details_tab[ name = j6_in_meth_priv_name ]-details = j8_form_param.
      ENDLOOP.

      "Accessing impementation part of a particular method and getting source code
      DATA(method_name) = CONV  sxco_clas_method_name( 'CLASS_CONSTRUCTOR' ).
      DATA(j12_method_content) = j1_handler->implementation->method( method_name )->content( )->get( ).

      DATA(j13_method_source_code) = xco_cp=>strings( j12_method_content-source
        )->join( |{ cl_abap_char_utilities=>cr_lf }|
        )->value.

      "Getting the source code using the get_source method
      "You may want to check the details for the following variable in the debugger.
      DATA(j14_get_source) = j1_handler->implementation->method( 'IF_OO_ADT_CLASSRUN~MAIN' )->content( )->get_source( ).
      DATA(j15_method_source_code_b) = xco_cp=>strings( j14_get_source
           )->join( |{ cl_abap_char_utilities=>cr_lf }|
           )->value.

      out->write( |Information about class { cl }| ).
      out->write( |\nInstance methods and their formal parameters| ).
      out->write( details_tab ).
      CLEAR details_tab.
      out->write( |\nSource code of implementation part of method { method_name }| ).
      out->write( j13_method_source_code ).
    ELSE.
      out->write( |\nThe class { cl } does not exist.| ).
    ENDIF.

**********************************************************************

    out->write( |\nGetting Subclasses| ).

    cl = 'CL_ABAP_TYPEDESCR'.
    DATA(k1_handler) = xco_cp_abap=>class( cl ).
    DATA(k2_exists) = k1_handler->exists( ).
    IF k2_exists = abap_true.
      "You may want to check the details for the following variable in the debugger.
      DATA(k3_get_subclasses) = k1_handler->subclasses->all->get( ).
      "Getting the names of the subclasses
      DATA(k4_subclass_names) = k1_handler->subclasses->all->get_names( ).

      out->write( data = k4_subclass_names name = `k4_subclass_names` ).
    ELSE.
      out->write( |\nThe class { cl } does not exist.| ).
    ENDIF.

**********************************************************************

    out->write( heading( `10) Getting AMDP-Related Information` ) ).

    cl = 'ZCL_DEMO_ABAP_AMDP'.
    method_name = 'SELECT_GET_CARR_FLI'.
    DATA(l1_handler) = xco_cp_abap=>class( cl ).
    DATA(l2_exists) = l1_handler->exists( ).
    IF l2_exists IS NOT INITIAL.
      DATA(l3_name) = l1_handler->name.
      "Getting content of the method
      DATA(l4_content) = l1_handler->implementation->method( method_name )->content( )->get( ).
      DATA(l5_db_type) = l4_content-amdp-database_type->if_xco_abap_token~get_source( ).
      DATA(l6_db_lang) = l4_content-amdp-database_language->if_xco_abap_token~get_source( ).
      IF l4_content-amdp-database_options IS NOT INITIAL.
        DATA(l4_db_opt) = l4_content-amdp-database_options[ 1 ]->if_xco_abap_token~get_source( ).
      ENDIF.

      CLEAR str.
      LOOP AT l4_content-amdp-database_entities INTO DATA(amdp_ent).
        str = str && ` ` && amdp_ent->if_xco_printable~get_text( )->get_lines( )->join( )->value.
      ENDLOOP.

      DATA(l5_method_source_code) = l4_content-source.
      DATA(l6_meth_source_code) = xco_cp=>strings( l5_method_source_code )->join( |{ cl_abap_char_utilities=>cr_lf }| )->value.

      infos = VALUE #(
       ( |Information about AMDP class { cl }| )
       ( `*********************************` )
       ( |Class name: { l3_name }| )
       ( |Db type: { l5_db_type }| )
       ( |Db language: { l6_db_lang }| )
       ( |Db options: { l4_db_opt }| )
       ( |Db entities: { str }| ) ).

      out->write( infos ).
      CLEAR: infos, str.
      out->write( |\nSource code of implementation part of method { method_name }| ).
      out->write( l6_meth_source_code ).
    ELSE.
      out->write( |\nThe interface { intf } does not exist.| ).
    ENDIF.

**********************************************************************

    out->write( heading( `11) Getting Date and Time Information Using XCO` ) ).
    "Among others, the examples cover time and date-related information.

    "Creating a time stamp
    DATA(m_moment) = xco_cp_time=>moment(
      iv_year   = '2024'
      iv_month  = '01'
      iv_day    = '01'
      iv_hour   = '12'
      iv_minute = '34'
      iv_second = '55' ).

    "Getting user time zone
    DATA(m1_user_time_zone) = xco_cp_time=>time_zone->user->value.
    "Result is of type string
    DATA(m2_moment_string) = m_moment->as( xco_cp_time=>format->abap )->value.
    "Result of type string using other formatting
    DATA(m3_moment_format_a) = m_moment->as( xco_cp_time=>format->iso_8601_basic )->value.
    DATA(m4_moment_format_b) = m_moment->as( xco_cp_time=>format->iso_8601_extended )->value.
    "Current moment in the time zone of the current user
    DATA(m5_cur_moment4user) = xco_cp=>sy->moment( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->iso_8601_extended )->value.
    "Current moment in UTC
    DATA(m6_cur_moment_utc) = xco_cp=>sy->moment( xco_cp_time=>time_zone->utc )->as( xco_cp_time=>format->iso_8601_extended )->value.
    "Current UNIX timestamp
    DATA(m7_unix_tstmp) = xco_cp=>sy->unix_timestamp( )->value.
    "For the time, you can also use the TIME method
    DATA(m8_time) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->iso_8601_extended )->value.
    "Getting second, minute, hour information
    DATA(m9_seconds) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->second.
    DATA(m10_minutes) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->minute.
    DATA(m11_hours) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->hour.
    "Calculations
    DATA(m12_add_time) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->add( iv_hour = 1 iv_minute = 1 iv_second = 1 )->as( xco_cp_time=>format->iso_8601_extended )->value.
    DATA(m13_subtract_time) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->subtract( iv_hour = 1 iv_minute = 1 iv_second = 1 )->as( xco_cp_time=>format->iso_8601_extended )->value.
    "Dates
    DATA(m14_date) = xco_cp=>sy->date( )->as( xco_cp_time=>format->iso_8601_extended )->value.
    DATA(m15_day) = xco_cp=>sy->date( )->day.
    DATA(m16_month) = xco_cp=>sy->date( )->month.
    DATA(m17_year) = xco_cp=>sy->date( )->year.
    "Calculations
    DATA(m18_add_date) = xco_cp=>sy->date( )->add( iv_day = 1 iv_month = 1 iv_year = 1 )->as( xco_cp_time=>format->iso_8601_extended )->value.
    DATA(m19_subtract_date) = xco_cp=>sy->date( )->subtract( iv_day = 1 iv_month = 1 iv_year = 1 )->as( xco_cp_time=>format->iso_8601_extended )->value.
    "You can also get more pieces of information using "sy" such as the technical name of your user (see also the class cl_abap_context_info)
    DATA(m20_user_name) = xco_cp=>sy->user( )->name.

    out->write( data = m1_user_time_zone name = `m1_user_time_zone` ).
    out->write( |\n| ).
    out->write( data = m2_moment_string name = `m2_moment_string` ).
    out->write( |\n| ).
    out->write( data = m3_moment_format_a name = `m3_moment_format_a` ).
    out->write( |\n| ).
    out->write( data = m4_moment_format_b name = `m4_moment_format_b` ).
    out->write( |\n| ).
    out->write( data = m5_cur_moment4user name = `m5_cur_moment4user` ).
    out->write( |\n| ).
    out->write( data = m6_cur_moment_utc name = `m6_cur_moment_utc` ).
    out->write( |\n| ).
    out->write( data = m7_unix_tstmp name = `m7_unix_tstmp` ).
    out->write( |\n| ).
    out->write( data = m8_time name = `m8_time` ).
    out->write( |\n| ).
    out->write( data = m9_seconds name = `m9_seconds` ).
    out->write( |\n| ).
    out->write( data = m10_minutes name = `m10_minutes` ).
    out->write( |\n| ).
    out->write( data = m11_hours name = `m11_hours` ).
    out->write( |\n| ).
    out->write( data = m12_add_time name = `m12_add_time` ).
    out->write( |\n| ).
    out->write( data = m13_subtract_time name = `m13_subtract_time` ).
    out->write( |\n| ).
    out->write( data = m14_date name = `m14_date` ).
    out->write( |\n| ).
    out->write( data = m15_day name = `m15_day` ).
    out->write( |\n| ).
    out->write( data = m16_month name = `m16_month` ).
    out->write( |\n| ).
    out->write( data = m17_year name = `m17_year` ).
    out->write( |\n| ).
    out->write( data = m18_add_date name = `m18_add_date` ).
    out->write( |\n| ).
    out->write( data = m19_subtract_date name = `m19_subtract_date` ).
    out->write( |\n| ).
    out->write( data = m20_user_name name = `m20_user_name` ).

**********************************************************************

    out->write( heading( `12) Generating Repository Objects Using XCO` ) ).
    "The example covers the generation of a domain, data element and a
    "database table using XCO.
    "NOTE:
    "To carry out the object generation, make sure that you meet the following
    "prerequisites:
    "1) You have a modifiable transport request (suitable for demo content). In
    "   ADT, go to the Transport Organizer tab, check that you have a transport
    "   request available under "Modifiable". Otherwise, create a new transport
    "   request.
    "   Copy the ID of the transport request and insert it in the constant "tr_req_id"
    "   that is available in the private visibility section of the class.
    "
    "2) You have specified a package for demo content, for example, the
    "   package of the ABAP cheat sheets, which is "ZABAP_CHEAT_SHEETS" in the example.
    "   Make sure that you have assigned the constant "package4gen" a value
    "   with a valid package in which you want to create the demo repository
    "   objects.
    "
    "3) You have provided suitable names for the demo objects. Check and, if need be,
    "   change the constant values for gen_dtel, gen_doma, gen_tabl.
    "
    "4) To enable the generation, assign the constant "generation_ok" the value
    "   "abap_true".
    "
    "The generation of the objects is only carried out if all of the mentioned
    "prerequisites are met.

    "Checking validity of the specified transport request ID
    TRY.
        DATA(n1_handler) = xco_cp_cts=>transport->for( tr_req_id ).
        DATA(n2_exists) = n1_handler->exists( ).
        DATA(n3_status) = n1_handler->get_status( ).
      CATCH cx_root INTO DATA(e).
    ENDTRY.

    "Checking if repository objects with the names specified already exist in the system
    DATA(n6_exists_tabl) = xco_cp_abap_dictionary=>database_table( gen_tabl  )->exists( ).
    DATA(n7_exists_dtel) = xco_cp_abap_dictionary=>data_element( gen_dtel )->exists( ).
    DATA(n8_exists_doma) = xco_cp_abap_dictionary=>domain( gen_doma )->exists( ).

    CASE abap_true.
      WHEN n6_exists_tabl.
        DATA(flag) = abap_true.
      WHEN n7_exists_dtel.
        flag = abap_true.
      WHEN n8_exists_doma.
        flag = abap_true.
      WHEN OTHERS.
    ENDCASE.

    "Checking if package exists
    DATA(n9_exists_devc) = xco_cp_abap_repository=>package->for( package4gen )->exists( ).

    IF n2_exists IS NOT INITIAL
    AND n3_status = xco_cp_transport=>status->modifiable
    AND flag IS INITIAL
    AND n9_exists_devc = abap_true
    AND generation_ok = abap_true.

      DATA(n10_handler) = xco_cp_generation=>environment->dev_system( tr_req_id ).
      DATA(n11_put) = n10_handler->create_put_operation( ).

**********************************************************************
      "Domain
      DATA(n12_doma_spec) = n11_put->for-doma->add_object( gen_doma
        )->set_package( package4gen
        )->create_form_specification( ).
      n12_doma_spec->set_short_description( 'Demo domain' ).
      n12_doma_spec->set_format( xco_cp_abap_dictionary=>built_in_type->char( 10 ) ).
      n12_doma_spec->fixed_values->add_fixed_value( 'BOOKED'
        )->set_description( 'Booked' ).
      n12_doma_spec->fixed_values->add_fixed_value( 'CANCELED'
        )->set_description( 'Canceled' ).
**********************************************************************
      "Data element
      DATA(n13_dtel_spec) = n11_put->for-dtel->add_object( gen_dtel
        )->set_package( package4gen
        )->create_form_specification( ).
      n13_dtel_spec->set_short_description( 'Booking status' ).

      n13_dtel_spec->set_data_type( xco_cp_abap_dictionary=>domain( gen_doma ) ).
***********************************************************************
      "Database table
      DATA(n14_tabl_spec) = n11_put->for-tabl-for-database_table->add_object( gen_tabl
        )->set_package( package4gen
        )->create_form_specification( ).
      n14_tabl_spec->set_short_description( 'Booking request'
        )->set_delivery_class( xco_cp_database_table=>delivery_class->a
        )->set_data_maintenance( xco_cp_database_table=>data_maintenance->allowed ).
      n14_tabl_spec->add_field( 'CLIENT' )->set_type( xco_cp_abap_dictionary=>built_in_type->clnt
        )->set_key_indicator(
        )->set_not_null( ).
      n14_tabl_spec->add_field( 'ID' )->set_type( xco_cp_abap_dictionary=>built_in_type->char( 30 )
        )->set_key_indicator(
        )->set_not_null( ).
      "Using built-in ABAP types
      n14_tabl_spec->add_field( 'FROM_DATE' )->set_type( xco_cp_abap_dictionary=>built_in_type->dats ).
      n14_tabl_spec->add_field( 'TO_DATE' )->set_type( xco_cp_abap_dictionary=>built_in_type->dats ).
      "Using the created data element
      n14_tabl_spec->add_field( 'STATUS' )->set_type( xco_cp_abap_dictionary=>data_element( gen_dtel ) ).
      "Using a released data element
      n14_tabl_spec->add_field( 'USER_NAME' )->set_type( xco_cp_abap_dictionary=>data_element( 'SYUNAME' ) ).
**********************************************************************
      "Executing the generation
      TRY.
          n11_put->execute( ).
          out->write( `Generation successful. Check out the newly created repository objects in the Project Explorer. Refresh the target package.` ).
        CATCH cx_xco_gen_put_exception INTO DATA(err).
          out->write( err->get_text( ) ).
      ENDTRY.

    ELSE.
      CLEAR details_tab.
      out->write( `The following information is displayed if the generation has not been carried out. Before generating the ` &&
      `repository objects, all of the following prerequisites must be met.` ).
      details_tab = VALUE #(
      ( name = `Transport request` details = `n2_exists: "` && n2_exists && `" / The value for "n2_exists" should be "X". ` &&
      `Otherwise, it indicates that the transport request does not exist. In that case, provide a suitable transport ` &&
      `request ID for the constant in the private section.` )
      ( name = `Modifiable transport request` details = COND #( WHEN e IS INITIAL THEN `n3_status: "` && n3_status->value &&
      `" / The value for "n3_status" should be "` && xco_cp_transport=>status->modifiable->value &&
      `". Otherwise, it indicates that the transport request is not modifiable. In that case, provide a suitable transport ` &&
      `request ID for the constant in the private section.` ELSE `There is an issue with the transport request ID provided: ` &&
      e->get_text( ) ) )
      ( name = `Package` details = `n9_exists_devc: "` && n9_exists_devc && `" / The value for "n9_exists_devc" should be "X". ` &&
      `Otherwise, it indicates that the package name as specified does not exist in the system. In that case, provide an ` &&
      `existing package name for the constant in the private section.` )
      ( name = `Repository object names` details = `flag: "` && flag && `" / The value for "flag" should be "" (initial). Otherwise, ` &&
      `it indicates that one or more artifacts with the same names as provided already exist in the system. In that case, ` &&
      `provide other names.` )
      ( name = `generation_ok flag` details = `generation_ok: "` && generation_ok && `" / The value for generation_ok should ` &&
      `be "X". Otherwise, it indicates that the execution of the generation is disabled. In that case, change the value of the constant ` &&
      `in the private section to "abap_true" to carry out the generation.` ) ).
      out->write( details_tab ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
