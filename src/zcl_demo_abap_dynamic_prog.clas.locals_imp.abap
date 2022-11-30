CLASS lcl_det_at_runtime DEFINITION.

  PUBLIC SECTION.

   CLASS-METHODS:
    get_dyn_table_name RETURNING VALUE(tab) TYPE string,
    get_dyn_dobj RETURNING VALUE(dobj) TYPE string,
    get_dyn_field RETURNING VALUE(field) TYPE string,
    get_dyn_select_list RETURNING VALUE(list) TYPE string,
    get_dyn_where_clause RETURNING VALUE(clause_tab) TYPE string_table,
    get_dyn_class RETURNING VALUE(cl) TYPE string,
    get_random_type RETURNING VALUE(random_type) TYPE string.

  CLASS-DATA: string1 TYPE string,
              string2 TYPE string,
              string3 TYPE string.

   TYPES: type1 TYPE p LENGTH 8 DECIMALS 2, "elementary type
          type2 TYPE zdemo_abap_carr, "structure type
          type3 TYPE TABLE OF zdemo_abap_flsch, "internal table type
          type4 TYPE REF TO lcl_det_at_runtime. "reference type

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_det_at_runtime IMPLEMENTATION.
  METHOD get_dyn_table_name.

    "Providing DDIC table names in a string table to be selected from.
    DATA(flight_tables) = VALUE string_table(
     ( `ZDEMO_ABAP_CARR` ) ( `ZDEMO_ABAP_FLSCH` ) ( `ZDEMO_ABAP_FLI` ) ).

    "Getting random number to determine the table index at runtime.
    DATA(random) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( flight_tables ) ).
    DATA(idx) = random->get_next( ).

    "Returning parameter to receive the random table name.
    TRY.
        tab = flight_tables[ idx ].
      CATCH cx_sy_itab_line_not_found INTO DATA(error).
    ENDTRY.

  ENDMETHOD.

  METHOD get_dyn_dobj.

    "Providing strings with demo content.
    string1 = |Hallo, { sy-uname }. | &&
              |This is string1.|.
    string2 = |Hallo, { sy-uname }. | &&
              |This is string2.|.
    string3 = |Hallo, { sy-uname }. | &&
              |This is string3.|.

    "Filling table with data object names.
    DATA(str_tab) = VALUE string_table(
      ( `STRING1` ) ( `STRING2` ) ( `STRING3` ) ).

    "Getting random number to determine the table index at runtime.
    DATA(random) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( str_tab ) ).
    DATA(idx) = random->get_next( ).

    "Returning parameter to receive the random data object name.
    TRY.
        dobj = str_tab[ idx ].
      CATCH cx_sy_itab_line_not_found INTO DATA(error).
    ENDTRY.

  ENDMETHOD.

  METHOD get_dyn_field.

    "Getting list of components of DDIC type zdemo_abap_carr
    DATA(comp) = CAST cl_abap_structdescr(
                        cl_abap_typedescr=>describe_by_name(
                          'ZDEMO_ABAP_CARR' )
                            )->components.

    "Getting random number to determine the table index at runtime.
    "Starting from 2 to exclude MANDT field
    DATA(random) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 2
                                     max = lines( comp ) ).
    DATA(idx) = random->get_next( ).

    "Returning parameter to receive the random component name.
    TRY.
        field = comp[ idx ]-name.
      CATCH cx_sy_itab_line_not_found INTO DATA(error).
    ENDTRY.

  ENDMETHOD.

  METHOD get_dyn_select_list.

    "Providing SELECT lists in a string table to be selected from.
    DATA sel_list_tab TYPE string_table.
    sel_list_tab = VALUE #(
      ( `CARRID, CONNID, COUNTRYFR, COUNTRYTO` )
      ( `CARRID, CONNID, CITYFROM, CITYTO` )
      ( `CARRID, CONNID, AIRPFROM, AIRPTO` )
      ( `CARRID, CONNID, AIRPFROM, AIRPTO, ` &&
        `FLTIME, DEPTIME, ARRTIME, DISTANCE` )
     ).

    "Getting random number to determine the table index at runtime.
    DATA(random) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( sel_list_tab ) ).
    DATA(idx) = random->get_next( ).

    "Returning parameter to receive the random SELECT list.
    TRY.
        list = sel_list_tab[ idx ].
      CATCH cx_sy_itab_line_not_found INTO DATA(error).
    ENDTRY.

  ENDMETHOD.

  METHOD get_dyn_where_clause.

    "Providing WHERE clauses in a table to be selected from.
    DATA: BEGIN OF where_struc,
            where_clause_tab TYPE string_table,
          END OF where_struc.

    DATA where_itab LIKE TABLE OF where_struc WITH EMPTY KEY.

    where_itab = VALUE #(
      ( where_clause_tab = VALUE #( ( `CARRID = 'LH'` )
                                    ( `OR CARRID = 'AA'` ) ) )
      ( where_clause_tab = VALUE #( ( `CONNID BETWEEN 0 AND 300` ) ) )
      ( where_clause_tab = VALUE #( ( `CITYFROM LIKE '%FRA%'` ) ) )
      ( where_clause_tab =
          VALUE #( ( `DISTANCE > 500 AND DISTID = 'KM'` ) ) )  ).

    "Getting random number to determine the table index at runtime.
    DATA(random) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( where_itab ) ).
    DATA(idx) = random->get_next( ).

    "Returning parameter to receive the random WHERE clause.
    TRY.
        clause_tab = where_itab[ idx ]-where_clause_tab.
      CATCH cx_sy_itab_line_not_found INTO DATA(error).
    ENDTRY.

  ENDMETHOD.

  METHOD get_dyn_class.

    "Providing class names in a string table to be selected from.
    DATA(class_tab) = VALUE string_table(
      ( `LCL_DET_AT_RUNTIME` )
      ( `LCL_DUMMY` ) ).

    "Getting random number to determine the table index at runtime.
    DATA(random) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( class_tab ) ).
    DATA(idx) = random->get_next( ).

    "Returning parameter to receive the random class name.
    TRY.
        cl = class_tab[ idx ].
      CATCH cx_sy_itab_line_not_found INTO DATA(error).
    ENDTRY.

  ENDMETHOD.

  METHOD get_random_type.

    "Providing names of classes in a string table to be selected from.
    "Note that in this example types are defined in the public section
    "of a class and the program logic is included in another class.
    "To be able to refer to the types, the class name is added.
    DATA(str_tab) = VALUE string_table(
      ( `LCL_DET_AT_RUNTIME=>TYPE1` )
      ( `LCL_DET_AT_RUNTIME=>TYPE2` )
      ( `LCL_DET_AT_RUNTIME=>TYPE3` )
      ( `LCL_DET_AT_RUNTIME=>TYPE4` )
      ( `LCL_DET_AT_RUNTIME` )
      ( `IF_OO_ADT_CLASSRUN` ) ).

    "Getting random number to determine the table index at runtime.
    DATA(random) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( str_tab ) ).
    DATA(idx) = random->get_next( ).

    "Returning parameter to receive the random type name.
    TRY.
        random_type = str_tab[ idx ].
      CATCH cx_sy_itab_line_not_found INTO DATA(error).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dummy DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      meth_a IMPORTING imp TYPE i
             EXPORTING exp TYPE i
             RETURNING VALUE(str) TYPE string,
      meth_b CHANGING ch TYPE string
             RETURNING VALUE(str) TYPE string.
ENDCLASS.

CLASS lcl_dummy IMPLEMENTATION.
  METHOD meth_a.
    str = |Hallo from meth_a.|.
  ENDMETHOD.

  METHOD meth_b.
    str = |Hallo from meth_b.|.
  ENDMETHOD.

ENDCLASS.
