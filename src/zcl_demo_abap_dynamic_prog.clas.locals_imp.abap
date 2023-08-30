CLASS lcl_det_at_runtime DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA: string1              TYPE string,
                string2              TYPE string,
                string3              TYPE string,
                dyn_meth_call_result TYPE string.

    TYPES: type_p     TYPE p LENGTH 8 DECIMALS 2, "elementary type
           type_struc TYPE zdemo_abap_carr, "structure type
           "internal table type
           type_itab  TYPE SORTED TABLE OF zdemo_abap_flsch WITH NON-UNIQUE KEY carrid connid                         "primary key
                                                            WITH UNIQUE SORTED KEY cities COMPONENTS cityfrom cityto, "secondary key
           type_ref   TYPE REF TO lcl_det_at_runtime. "reference type

    TYPES: BEGIN OF struc_builtin,
             builtin_type TYPE c LENGTH 10,
             len          TYPE i,
             dec          TYPE i,
           END OF struc_builtin.

    TYPES: BEGIN OF struc_dyn,
            table        TYPE string,
            select_list  TYPE string,
            where_clause TYPE string_table,
            order_by     TYPE string,
            target       TYPE REF TO data,
            rows         TYPE i,
          END OF struc_dyn.

    CLASS-METHODS:
      get_dyn_table_name RETURNING VALUE(tab) TYPE string,
      get_dyn_dobj RETURNING VALUE(dobj) TYPE string,
      get_dyn_field RETURNING VALUE(field) TYPE string,
      get_dyn_select_list RETURNING VALUE(list) TYPE string,
      get_dyn_where_clause RETURNING VALUE(clause_tab) TYPE string_table,
      get_random_type RETURNING VALUE(random_type) TYPE string,
      get_builtin_type RETURNING VALUE(builtin_type) TYPE struc_builtin,
      get_dyn_class_meth EXPORTING cl   TYPE string
                                   meth TYPE string
                                   ptab TYPE abap_parmbind_tab,
      get_dyn_syntax_elements RETURNING VALUE(syntax_elements) TYPE struc_dyn,
      fill_string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_det_at_runtime IMPLEMENTATION.
  METHOD get_dyn_table_name.

    "Providing DDIC table names in a string table to be selected from.
    DATA(flight_tables) = VALUE string_table(
     ( `ZDEMO_ABAP_CARR` ) ( `ZDEMO_ABAP_FLSCH` ) ( `ZDEMO_ABAP_FLI` ) ).

    "Getting random number to determine the table index at runtime.
    DATA(idx) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( flight_tables ) )->get_next( ).

    "Returning parameter to receive the random table name.
    tab = VALUE #( flight_tables[ idx ] DEFAULT `ZDEMO_ABAP_CARR`  ).

  ENDMETHOD.

  METHOD get_builtin_type.

    "Providing the names of built-in types in a string table to be selected from.
    TYPES tabtyp TYPE TABLE OF struc_builtin-builtin_type WITH EMPTY KEY.

    DATA(built) = VALUE tabtyp(
     ( 'd' )
     ( 'decfloat16' )
     ( 'decfloat34' )
     ( 'f' )
     ( 'i' )
     ( 'string' )
     ( 't' )
     ( 'xstring' )
     ( 'c' )
     ( 'n' )
     ( 'x' )
     ( 'p' )
     ).

    "Getting random number to determine the table index at runtime
    DATA(idx) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( built ) )->get_next( ).

    "Providing the returning parameter with a random type name
    TRY.
        builtin_type = VALUE #( builtin_type = built[ idx ] dec = idx len = idx ).
      CATCH cx_sy_itab_line_not_found.
        builtin_type = VALUE #( builtin_type = `p` dec = 5 len = 5 ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_dyn_dobj.

    "Providing strings with demo content
    string1 = |Hallo, { sy-uname }. | &&
              |This is string1.|.
    string2 = |Hallo, { sy-uname }. | &&
              |This is string2.|.
    string3 = |Hallo, { sy-uname }. | &&
              |This is string3.|.

    "Filling table with data object names
    DATA(str_tab) = VALUE string_table(
      ( `STRING1` ) ( `STRING2` ) ( `STRING3` ) ).

    "Getting random number to determine the table index at runtime
    DATA(idx) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( str_tab ) )->get_next( ).

    "Providing the returning parameter with a random data object name
    dobj = VALUE #( str_tab[ idx ] DEFAULT |Hallo, { sy-uname }. This is a string.| ).

  ENDMETHOD.

  METHOD get_dyn_field.

    "Getting list of components of DDIC type zdemo_abap_carr
    DATA(comp) = CAST cl_abap_structdescr(
                        cl_abap_typedescr=>describe_by_name(
                          'ZDEMO_ABAP_CARR' )
                            )->components.

    "Getting random number to determine the table index at runtime;
    "starting from 2 to exclude MANDT field
    DATA(idx) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 2
                                     max = lines( comp ) )->get_next( ).

    "Providing the returning parameter with a random component name
    field = VALUE #( comp[ idx ]-name DEFAULT `CARRID`  ).

  ENDMETHOD.

  METHOD get_dyn_select_list.

    "Providing SELECT lists in a string table to be selected from
    DATA sel_list_tab TYPE string_table.
    sel_list_tab = VALUE #(
      ( `CARRID, CONNID, COUNTRYFR, COUNTRYTO` )
      ( `CARRID, CONNID, CITYFROM, CITYTO` )
      ( `CARRID, CONNID, AIRPFROM, AIRPTO` )
      ( `CARRID, CONNID, AIRPFROM, AIRPTO, ` &&
        `FLTIME, DEPTIME, ARRTIME, DISTANCE` )
     ).

    "Getting random number to determine the table index at runtime
    DATA(idx) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( sel_list_tab ) )->get_next( ).

    "Providing the returning parameter with a random SELECT list
    list = VALUE #( sel_list_tab[ idx ] DEFAULT `CARRID, CONNID, COUNTRYFR, COUNTRYTO`  ).

  ENDMETHOD.

  METHOD get_dyn_where_clause.

    "Providing WHERE clauses in a table to be selected from
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

    "Getting random number to determine the table index at runtime
    DATA(idx) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( where_itab ) )->get_next( ).

    "Providing the returning parameter with a random WHERE clause
    clause_tab = VALUE #( where_itab[ idx ]-where_clause_tab DEFAULT VALUE #( ( `CARRID = 'LH'` ) ( `OR CARRID = 'AA'` ) )  ).

  ENDMETHOD.

  METHOD get_random_type.

    "Providing names of classes in a string table to be selected from
    "In this example, some types are defined in the public section
    "of a local class. The class name is added here since the names
    "are used in the global class.
    DATA(str_tab) = VALUE string_table(
      ( `LCL_DET_AT_RUNTIME=>TYPE_P` )
      ( `LCL_DET_AT_RUNTIME=>TYPE_STRUC` )
      ( `LCL_DET_AT_RUNTIME=>TYPE_ITAB` )
      ( `LCL_DET_AT_RUNTIME=>TYPE_REF` )
      ( `LCL_DET_AT_RUNTIME` )
      ( `IF_OO_ADT_CLASSRUN` )
      ).

    "Getting random number to determine the table index at runtime
    DATA(idx) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( str_tab ) )->get_next( ).

    "Providing the returning parameter with a random type name
    random_type = VALUE #( str_tab[ idx ] DEFAULT `LCL_DET_AT_RUNTIME=>TYPE_STRUC` ).

  ENDMETHOD.

  METHOD get_dyn_class_meth.

    "Providing class names in a string table to be selected from
    DATA(class_tab) = VALUE string_table(
      ( `LCL_DEMO1` )
      ( `LCL_DEMO2` ) ).

    "Getting random number to determine the table index at runtime
    DATA(idx) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( class_tab ) )->get_next( ).

    "Provide the exporting parameter with the random class name
    cl = VALUE #( class_tab[ idx ] DEFAULT `LCL_DEMO1` ).

    "Getting method names using RTTI
    DATA(methods) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( cl ) )->methods.

    "Getting random number to determine the table index at runtime
    idx = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( ) min = 1
                                     max = lines( methods ) )->get_next( ).

    "Provide the exporting parameter with the random method name
    meth = VALUE #( methods[ idx ]-name DEFAULT `METH_A` ).

    "Data reference objects for the value parameter in the parameter table
    DATA(ref_imp) = NEW string( `hi` ).
    DATA(ref_exp) = NEW string( `hallo` ).
    DATA(ref_ch) = NEW string( `salut` ).
    DATA(ref_ret) = NEW string( `ciao` ).

    "Filling the parameter tables
    "Note: If the method signature has an importing parameter,
    "it must be specified as exporting parameter here.
    "Same is true for the exporting parameter in the signature
    "that must be specified as importing parameter.

    CASE meth.

      WHEN `METH_A`.

        ptab = VALUE #( ( name  = 'A'
                          kind  = cl_abap_objectdescr=>exporting
                          value = ref_exp )
                        ( name  = 'B'
                          kind  = cl_abap_objectdescr=>importing
                          value = ref_imp ) ).

      WHEN `METH_B`.

        ptab = VALUE #( ( name  = 'C'
                          kind  = cl_abap_objectdescr=>changing
                          value = ref_ch )
                        ( name  = 'D'
                          kind  = cl_abap_objectdescr=>returning
                          value = ref_ret ) ).

      WHEN `METH_C`.

        ptab = VALUE #( ( name  = 'E'
                          kind  = cl_abap_objectdescr=>exporting
                          value = ref_exp )
                        ( name  = 'F'
                          kind  = cl_abap_objectdescr=>importing
                          value = ref_imp ) ).

      WHEN `METH_D`.

        ptab = VALUE #( ( name  = 'G'
                          kind  = cl_abap_objectdescr=>changing
                          value = ref_ch )
                        ( name  = 'H'
                          kind  = cl_abap_objectdescr=>returning
                          value = ref_ret ) ).

    ENDCASE.

  ENDMETHOD.

  METHOD fill_string.
    dyn_meth_call_result = |Hallo { sy-uname }. The string was filled at { utclong_current( ) }.|.
  ENDMETHOD.

  METHOD get_dyn_syntax_elements.

    "FROM clause
    DATA(flight_tables) = VALUE string_table(
         ( `ZDEMO_ABAP_CARR` ) ( `ZDEMO_ABAP_FLSCH` ) ( `ZDEMO_ABAP_FLI` ) ).

    "Getting random number to determine the table index at runtime.
    DATA(idx_table) = cl_abap_random_int=>create(
          seed = cl_abap_random=>seed( )
          min  = 1
          max  = lines( flight_tables ) )->get_next( ).

    syntax_elements-table = VALUE #( flight_tables[ idx_table ] DEFAULT `ZDEMO_ABAP_CARR`  ).

    "SELECT list
    DATA(comp) = CAST cl_abap_structdescr(
      cl_abap_typedescr=>describe_by_name( syntax_elements-table ) )->components.

    "At least 3 components
    DATA(idx_comp) = cl_abap_random_int=>create(
          seed = cl_abap_random=>seed( )
          min  = 3
          max  = lines( comp ) )->get_next( ).

    DELETE comp FROM idx_comp + 1 TO lines( comp ).

    LOOP AT comp ASSIGNING FIELD-SYMBOL(<comp>).
      syntax_elements-select_list = syntax_elements-select_list && `, ` && <comp>-name.
    ENDLOOP.

    "Replacing initial comma
    REPLACE PCRE `^,\s` IN syntax_elements-select_list WITH ``.

    "WHERE clause
    "Excluding the client field
    DELETE comp WHERE name = 'MANDT'.

    "A maximum of 4 fields are to be respected because that is the maximum of possible fields
    "available in table zdemo_abap_carr (without the client field)
    DATA(idx_where) = cl_abap_random_int=>create(
          seed = cl_abap_random=>seed( )
          min  = 1
          max  = 4 )->get_next( ).

    "In the example, the WHERE clause consists of an internal table of type string.
    "The clause is set up with fields and only IS NOT INITIAL to be on the safe side for a
    "somewhat 'meaningful' clause and in the interest of simplicity.
    LOOP AT comp ASSIGNING FIELD-SYMBOL(<where>) TO idx_where.
      IF sy-tabix = 1.
        APPEND <where>-name && ` IS NOT INITIAL` TO syntax_elements-where_clause.
      ELSE.
        APPEND `OR ` && <where>-name && ` IS NOT INITIAL` TO syntax_elements-where_clause.
      ENDIF.
    ENDLOOP.

    "ORDER BY clause
    DATA(idx_order) = cl_abap_random_int=>create(
          seed = cl_abap_random=>seed( )
          min  = 1
          max  = lines( comp ) )->get_next( ).

    syntax_elements-order_by = VALUE #( comp[ idx_order ]-name DEFAULT `CARRID`  ).

    "INTO clause
    CREATE DATA syntax_elements-target TYPE TABLE OF (syntax_elements-table).

    "UP TO ... ROWS
    "A minimum of 2 and a maximum of 6 rows are to be retrieved by the SELECT statement
    syntax_elements-rows = cl_abap_random_int=>create(
            seed = cl_abap_random=>seed( )
            min  = 2
            max  = 5 )->get_next( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_demo1 DEFINITION.
"Note that this is just a demo class with demo methods to work with in the example.
  PUBLIC SECTION.

    CLASS-METHODS:
      meth_a IMPORTING a TYPE string
             EXPORTING b TYPE string,
      meth_b CHANGING  c        TYPE string
             RETURNING VALUE(d) TYPE string.

ENDCLASS.

CLASS lcl_demo1 IMPLEMENTATION.
  METHOD meth_a.
    b = |Hallo from meth_a. '{ a }' was input.|.

    "Filling an attribute for the output in the global class.
    lcl_det_at_runtime=>dyn_meth_call_result = b.
  ENDMETHOD.

  METHOD meth_b.
    c = `#` && c && `#`.
    d = |Hallo from meth_b. Actual parameter of changing parameter: '{ c }'|.

    "Filling an attribute for the output in the global class.
    lcl_det_at_runtime=>dyn_meth_call_result = d.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_demo2 DEFINITION.
"Note that this is just a demo class with demo methods to work with in the example.
  PUBLIC SECTION.

    CLASS-METHODS:
      meth_c IMPORTING e TYPE string
             EXPORTING f TYPE string,
      meth_d CHANGING  g        TYPE string
             RETURNING VALUE(h) TYPE string.

ENDCLASS.

CLASS lcl_demo2 IMPLEMENTATION.
  METHOD meth_c.
    f = |Hallo from meth_c. '{ e }' was input.|.

    "Filling an attribute for the output in the global class.
    lcl_det_at_runtime=>dyn_meth_call_result = f.
  ENDMETHOD.

  METHOD meth_d.
    g = `*` && g && `*`.
    h = |Hallo from meth_d. Actual parameter of changing parameter: '{ g }'|.

    "Filling an attribute for the output in the global class.
    lcl_det_at_runtime=>dyn_meth_call_result = h.
  ENDMETHOD.

ENDCLASS.
