"! <p class="shorttext"><strong>Builder</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the builder design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS get_table_info IMPORTING itab        TYPE ANY TABLE
                                 RETURNING VALUE(info) TYPE string_table.

    TYPES: BEGIN OF struc1,
             comp1 TYPE i,
             comp2 TYPE c LENGTH 10,
             comp3 TYPE decfloat34,
             comp4 TYPE string,
             comp5 TYPE n LENGTH 5,
           END OF struc1.

    TYPES p_type TYPE p DECIMALS 2 LENGTH 8.
    TYPES: BEGIN OF struc2,
             int    TYPE int8,
             packed TYPE p_type,
             float  TYPE f,
             date   TYPE d,
             time   TYPE t,
           END OF struc2.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_oodp_builder IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Builder` ).

    "Creating a string table that includes type names
    DATA(type_names) = VALUE string_table(
      ( `ZCL_DEMO_ABAP_OODP_BUILDER=>STRUC1` )
      ( `ZCL_DEMO_ABAP_OODP_BUILDER=>STRUC2` )
      "Note that the demo values are nonsensical. For example, the client
      "field shows different values, or the seats_occ value might be higher
      "than the value for seats_max.
      ( `ZDEMOABAPFLIGHTS` )
      "Elementary types
      ( `STRING` )
      ( `UTCLONG` )
      "Non-existent type
      ( `TYPE_THAT_DOES_NOT_EXIST` ) ).

    out->write( `1) Creating standard tables with non-unique primary table keys` ).
    out->write( |\n| ).

    "Specifying no key components, using default ones
    LOOP AT type_names INTO DATA(name).
      DATA(oref) = lcl_itab_provider=>create_itab(
                     type_name         = name
                     table_kind        = lcl_itab_provider=>standard_w_nonunique_pr_key
                     table_entry_count = 10 ).

      out->write( |------------- Table with line type { oref->type_name } -------------| ).
      out->write( |\n| ).

      IF oref->type_exists = abap_true.
        "The example is designed to retrieve Table type informationrmation so as to display
        "information such type and table keys of the internal table.
        DATA(info) = get_table_info( oref->itab->* ).
        out->write( `Table type information:` ).
        out->write( info ).
        out->write( |\n| ).
        out->write( oref->itab->*  ).
        out->write( |\n| ).
      ELSE.
        out->write( `Type does not exist. Internal table not created.` ).
        out->write( |\n| ).
      ENDIF.
    ENDLOOP.

    out->write( `------------- Key components explicitly specified -------------` ).
    out->write( |\n| ).

    "Key components explicitly specified (including a non-existent component name that is ignored)
    DATA(oref_key_specified) = lcl_itab_provider=>create_itab(
                   type_name         = `ZCL_DEMO_ABAP_OODP_BUILDER=>STRUC1`
                   table_kind        = lcl_itab_provider=>standard_w_nonunique_pr_key
                   key_components    = VALUE #( ( `COMP1` ) ( `COMP2` ) ( `FALSE_COMPONENT` ) )
                   table_entry_count = 3 ).

    out->write( `------------- Table with line type ZCL_DEMO_ABAP_OODP_BUILDER=>STRUC1 -------------` ).
    IF oref_key_specified->type_exists = abap_true.
      info = get_table_info( oref_key_specified->itab->* ).
      out->write( `Table type information:` ).
      out->write( info ).
      out->write( |\n| ).
      out->write( oref_key_specified->itab->*  ).
      out->write( |\n| ).
    ELSE.
      out->write( `Type does not exist. Internal table not created.` ).
      out->write( |\n| ).
    ENDIF.

    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `2) Creating standard tables with empty key` ).
    out->write( |\n| ).

    type_names = VALUE string_table(
       ( `ZCL_DEMO_ABAP_OODP_BUILDER=>STRUC2` )
       ( `D` ) ).

    LOOP AT type_names INTO name.
      DATA(oref_empty_key) = lcl_itab_provider=>create_itab(
                     type_name         = name
                     table_kind        = lcl_itab_provider=>standard_w_empty_key
                     "Specified key components are ignored in the example
                     "key_components    = VALUE #( ( `INT` ) )
                     table_entry_count = 3 ).

      out->write( |------------- Table with line type { oref_empty_key->type_name } -------------| ).
      out->write( |\n| ).

      IF oref_empty_key->type_exists = abap_true.
        info = get_table_info( oref_empty_key->itab->* ).
        out->write( `Table type information:` ).
        out->write( info ).
        out->write( |\n| ).
        out->write( oref_empty_key->itab->*  ).
        out->write( |\n| ).
      ELSE.
        out->write( `Type does not exist. Internal table not created.` ).
        out->write( |\n| ).
      ENDIF.
    ENDLOOP.

    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `3) Creating sorted tables with unique primary keys` ).
    out->write( |\n| ).

    type_names = VALUE string_table(
       ( `ZCL_DEMO_ABAP_OODP_BUILDER=>STRUC1` )
       ( `T` ) ).

    LOOP AT type_names INTO name.
      DATA(oref_sorted) = lcl_itab_provider=>create_itab(
                     type_name         = name
                     table_kind        = lcl_itab_provider=>sorted_w_unique_pr_key
                     table_entry_count = 3 ).

      out->write( |------------- Table with line type { oref_sorted->type_name } -------------| ).
      out->write( |\n| ).

      IF oref_sorted->type_exists = abap_true.
        info = get_table_info( oref_sorted->itab->* ).
        out->write( `Table type information:` ).
        out->write( info ).
        out->write( |\n| ).
        out->write( oref_sorted->itab->*  ).
        out->write( |\n| ).
      ELSE.
        out->write( `Type does not exist. Internal table not created.` ).
        out->write( |\n| ).
      ENDIF.
    ENDLOOP.

    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `4) Creating hashed tables with unique primary keys` ).
    out->write( |\n| ).

    type_names = VALUE string_table(
       ( `ZCL_DEMO_ABAP_OODP_BUILDER=>STRUC1` )
       ( `I` ) ).

    LOOP AT type_names INTO name.
      DATA(oref_hashed) = lcl_itab_provider=>create_itab(
                     type_name         = name
                     table_kind        = lcl_itab_provider=>hashed_w_unique_pr_key
                     table_entry_count = 3 ).

      out->write( |------------- Table with line type { oref_hashed->type_name } -------------| ).
      out->write( |\n| ).

      IF oref_hashed->type_exists = abap_true.
        info = get_table_info( oref_hashed->itab->* ).
        out->write( `Table type information:` ).
        out->write( info ).
        out->write( |\n| ).
        out->write( oref_hashed->itab->*  ).
        out->write( |\n| ).
      ELSE.
        out->write( `Type does not exist. Internal table not created.` ).
        out->write( |\n| ).
      ENDIF.
    ENDLOOP.

    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `5) Populating internal tables with random data` ).
    out->write( |\n| ).

    "Demo internal tables to be filled
    DATA it_std1 TYPE TABLE OF zcl_demo_abap_oodp_builder=>struc1 WITH EMPTY KEY.
    DATA it_std2 TYPE string_table.
    DATA it_std3 TYPE TABLE OF i.

    TYPES: BEGIN OF various_types,
             c1        TYPE c LENGTH 1,
             c5        TYPE c LENGTH 5,
             c10       TYPE c LENGTH 10,
             str       TYPE string,
             int       TYPE i,
             f         TYPE f,
             dec16     TYPE decfloat16,
             dec34     TYPE decfloat34,
             i8        TYPE int8,
             n5        TYPE n LENGTH 5,
             time      TYPE t,
             date      TYPE d,
             timestamp TYPE utclong,
             x1        TYPE x LENGTH 1,
             x5        TYPE x LENGTH 5,
             xstr      TYPE xstring,
             pl2d1     TYPE p LENGTH 2 DECIMALS 1,
             pl3d2     TYPE p LENGTH 3 DECIMALS 2,
             pl4d3     TYPE p LENGTH 4 DECIMALS 3,
             pl5d4     TYPE p LENGTH 5 DECIMALS 4,
             pl6d5     TYPE p LENGTH 6 DECIMALS 5,
             pl7d6     TYPE p LENGTH 7 DECIMALS 6,
             pl8d7     TYPE p LENGTH 8 DECIMALS 7,
             pl9d8     TYPE p LENGTH 9 DECIMALS 8,
             pl10d9    TYPE p LENGTH 10 DECIMALS 9,
             pl11d10   TYPE p LENGTH 11 DECIMALS 10,
             pl12d11   TYPE p LENGTH 12 DECIMALS 11,
             pl13d12   TYPE p LENGTH 13 DECIMALS 12,
             pl14d13   TYPE p LENGTH 14 DECIMALS 13,
             pl15d14   TYPE p LENGTH 15 DECIMALS 14,
           END OF various_types.

    DATA it_std4 TYPE TABLE OF various_types WITH EMPTY KEY.

    "Sorted tables
    DATA it_sorted1 TYPE SORTED TABLE OF zcl_demo_abap_oodp_builder=>struc1 WITH NON-UNIQUE KEY primary_key COMPONENTS comp1.
    DATA it_sorted2 TYPE SORTED TABLE OF zcl_demo_abap_oodp_builder=>struc2 WITH UNIQUE KEY int.
    DATA it_sorted3 TYPE SORTED TABLE OF utclong WITH NON-UNIQUE KEY table_line.

    "Hashed tables
    DATA it_hashed1 TYPE HASHED TABLE OF zcl_demo_abap_oodp_builder=>struc1 WITH UNIQUE KEY primary_key COMPONENTS comp1.
    TYPES n5 TYPE n LENGTH 5.
    DATA it_hashed2 TYPE HASHED TABLE OF n5 WITH UNIQUE KEY primary_key COMPONENTS table_line.

    TYPES table_refs TYPE TABLE OF REF TO data WITH EMPTY KEY.
    DATA(itab_refs) = VALUE table_refs(
      ( REF #( it_std1 ) )
      ( REF #( it_std2 ) )
      ( REF #( it_std3 ) )
      ( REF #( it_std4 ) )
      ( REF #( it_sorted1 ) )
      ( REF #( it_sorted2 ) )
      ( REF #( it_sorted3 ) )
      ( REF #( it_hashed1 ) )
      ( REF #( it_hashed2 ) ) ).

    LOOP AT itab_refs INTO DATA(ref).
      DATA(tabix) = sy-tabix.
      DATA(oref_populate_itab) = lcl_itab_provider=>populate_itab(
                                   itab              = ref->*
                                   table_entry_count = 3
                                 ).

      out->write( |------------- Internal table { tabix } -------------| ).
      out->write( |\n| ).

      info = get_table_info( oref_populate_itab->itab->* ).
      out->write( `Table type information:` ).
      out->write( info ).

      out->write( |\n| ).
      out->write( oref_populate_itab->itab->*  ).
      out->write( |\n| ).
    ENDLOOP.

    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `6) Other line types` ).
    out->write( |\n| ).

    "Other line types not supported in the example
*DATA(oref_not_working) = lcl_itab_provider=>populate_itab(
*                             itab              = itab_refs
*                             table_entry_count = 3  ).

    "Deep/nested line types are not supported
    "These components remain initial.
    TYPES: BEGIN OF deep_type,
             flag   TYPE abap_boolean,
             c5     TYPE c LENGTH 5,
             strtab TYPE string_table,
             struc  TYPE zcl_demo_abap_oodp_builder=>struc1,
           END OF deep_type.

    DATA deep_itab TYPE TABLE OF deep_type WITH EMPTY KEY.

    DATA(oref_deep) = lcl_itab_provider=>populate_itab(
                                 itab              = deep_itab
                                 table_entry_count = 3  ).

    out->write( oref_deep->itab->*  ).
    out->write( |\n| ).
    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `7) Exploring the random value creation methods` ).
    out->write( |\n| ).

    out->write( `---------------- Getting random strings ----------------` ).
    DO 5 TIMES.
      DATA(str) = lcl_itab_builder=>get_random_string( sy-index ).
      out->write( str ).
    ENDDO.

    out->write( `---------------- Getting random number sequences ----------------` ).
    DO 5 TIMES.
      DATA(number_set) = lcl_itab_builder=>get_random_number_sequence( sy-index ).
      out->write( number_set ).
    ENDDO.

    out->write( `---------------- Getting random packed numbers ----------------` ).
    DO 15 TIMES.
      DATA(random_p) = lcl_itab_builder=>get_random_p(
                         length = 8
                         decimals  = sy-index - 1 ).
      out->write( random_p->* ).
    ENDDO.

    out->write( `---------------- Getting random dates ----------------` ).
    DO 5 TIMES.
      DATA(random_d) = lcl_itab_builder=>get_random_d( ).
      out->write( random_d ).
    ENDDO.

    out->write( `---------------- Getting random times ----------------` ).
    DO 5 TIMES.
      DATA(random_t) = lcl_itab_builder=>get_random_t( ).
      out->write( random_t ).
    ENDDO.

    out->write( `---------------- Getting random UTC timestamps ----------------` ).
    DO 5 TIMES.
      DATA(random_utc) = lcl_itab_builder=>get_random_utclong( ).
      out->write( random_utc ).
    ENDDO.

    out->write( `---------------- Getting random data objects of type decfloat16 ----------------` ).
    DO 5 TIMES.
      DATA(random_dec16) = lcl_itab_builder=>get_random_dec16( ).
      out->write( random_dec16 ).
    ENDDO.

    out->write( `---------------- Getting random data objects of type decfloat34 ----------------` ).
    DO 5 TIMES.
      DATA(random_dec34) = lcl_itab_builder=>get_random_dec34( ).
      out->write( random_dec34 ).
    ENDDO.

    out->write( `---------------- Getting random data objects of type f ----------------` ).
    DO 5 TIMES.
      DATA(random_f) = lcl_itab_builder=>get_random_f( ).
      out->write( random_f ).
    ENDDO.

    out->write( `---------------- Getting random data objects of type x ----------------` ).

    DATA(xstr_1) = lcl_itab_builder=>get_random_x( 1 ).
    DATA x1 TYPE x LENGTH 1.
    x1 = xstr_1.
    out->write( x1 ).

    DATA(xstr_2) = lcl_itab_builder=>get_random_x( 2 ).
    DATA x2 TYPE x LENGTH 2.
    x2 = xstr_2.
    out->write( x2 ).

    DATA(xstr_8) = lcl_itab_builder=>get_random_x( 8 ).
    DATA x8 TYPE x LENGTH 8.
    x8 = xstr_8.
    out->write( x8 ).

    out->write( `---------------- Getting random data objects of type xstring ----------------` ).

    DATA(xstr_a) = lcl_itab_builder=>get_random_xstring( ).
    out->write( xstr_a ).
    DATA(xstr_b) = lcl_itab_builder=>get_random_xstring( ).
    out->write( xstr_b ).
    DATA(xstr_c) = lcl_itab_builder=>get_random_xstring( ).
    out->write( xstr_c ).

  ENDMETHOD.

  METHOD get_table_info.
    DATA(tab_type_info) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab ) ).

    "Getting the table kind
    "For the constant values of type abap_tablekind, see cl_abap_tabledescr. For example, 'S'
    "stands for a standard table.
    DATA(tab_table_kind) = tab_type_info->table_kind.
    INSERT |Table kind: { tab_table_kind }| INTO TABLE info.

    "Checking if the table has a unique key
    DATA(tab_has_unique_key) = tab_type_info->has_unique_key.
    INSERT |Has a unique key: "{ tab_has_unique_key }" | &&
    |{ COND #( WHEN tab_has_unique_key IS INITIAL THEN `(no unique key)` ) }| INTO TABLE info.

    "Returning a table with a description of all table keys, e.g. all components of a key,
    "key kind (U, unique, in the example case), information whether the key is the primary
    "key etc. For the constant values, see the cl_abap_tabledescr class.
    DATA(tab_keys) = tab_type_info->get_keys( ).

    INSERT |Table keys: { REDUCE string( INIT str = `` FOR <key2> IN tab_keys NEXT str = |{ str }| &&
    |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ REDUCE string( INIT str2 = `` FOR <key3> IN <key2>-components NEXT str2 = |{ str2 }| &&
    |{ COND #( WHEN str2 IS NOT INITIAL THEN `/` ) }{ <key3>-name }| ) } (is primary: "{ <key2>-is_primary }", |  &&
    |is unique: "{ <key2>-is_unique }", key kind: "{ <key2>-key_kind }", access kind: "{ <key2>-access_kind }")| ) }| INTO TABLE info.
  ENDMETHOD.

ENDCLASS.
