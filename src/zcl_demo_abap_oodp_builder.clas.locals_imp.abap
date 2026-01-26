"Class for the builder
"An abstract class is used as there are more general tasks performed by the subclasses.
"These tasks are implemented in non-abstract methods in the class.
CLASS lcl_itab_builder DEFINITION ABSTRACT CREATE PUBLIC.
  PUBLIC SECTION.
    DATA type_name TYPE string.
    DATA primary_key_components TYPE string_table.
    DATA itab TYPE REF TO data.
    DATA table_entries_to_create TYPE i.
    DATA type_exists TYPE abap_boolean.
    DATA components TYPE cl_abap_structdescr=>component_table.
    DATA is_elementary_line_type TYPE abap_boolean.
    DATA table_type_descr_obj TYPE REF TO cl_abap_tabledescr.
    DATA line_type TYPE REF TO cl_abap_datadescr.

    "----------------------- Abstract methods -----------------------
    METHODS build_type_info ABSTRACT RETURNING VALUE(type_exists) TYPE abap_boolean.
    METHODS build_components ABSTRACT.
    METHODS build_table_keys ABSTRACT.
    METHODS build_table_type ABSTRACT.
    METHODS build_data_object ABSTRACT.
    METHODS build_random_data ABSTRACT.

    "----------------------- Constants used in non-abstract methods -----------------------
    CONSTANTS character_set TYPE string VALUE `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`.
    CONSTANTS number_set TYPE string VALUE `0123456789`.
    CONSTANTS max_length TYPE i VALUE 10.
    CONSTANTS start_date TYPE d VALUE '20250101'.
    CONSTANTS start_time TYPE t VALUE '000000'.
    CONSTANTS max_table_entry_count TYPE i VALUE 50.
    CONSTANTS min_int_value TYPE i VALUE 1.
    CONSTANTS max_int_value TYPE i VALUE 100.
    CONSTANTS min_int8_value TYPE int8 VALUE 1.
    CONSTANTS max_int8_value TYPE int8 VALUE 100.
    CONSTANTS min_p_value TYPE p VALUE 0.
    CONSTANTS max_p_value TYPE p VALUE 9.

    "----------------------- Non-abstract methods used by subclasses -----------------------
    "They are intentionally included in the public visibility section for the demo in the
    "global class.
    METHODS check_type.
    METHODS handle_keys.
    METHODS handle_components.

    METHODS add_table_entries
      IMPORTING
        VALUE(table_entry_count) TYPE i.

    CLASS-METHODS get_random_string
      IMPORTING
        length           TYPE i OPTIONAL
        randomize_length TYPE abap_boolean DEFAULT abap_true
          PREFERRED PARAMETER length
      RETURNING
        VALUE(str)       TYPE string.

    CLASS-METHODS get_random_number_sequence
      IMPORTING
        length           TYPE i OPTIONAL
        randomize_length TYPE abap_boolean DEFAULT abap_true
          PREFERRED PARAMETER length
      RETURNING
        VALUE(numbers)   TYPE string.

    CLASS-METHODS get_random_i
      IMPORTING min_value     TYPE i DEFAULT min_int_value
                max_value     TYPE i DEFAULT max_int_value
      RETURNING VALUE(number) TYPE i.

    CLASS-METHODS get_random_int8
      IMPORTING min_value     TYPE int8 DEFAULT min_int8_value
                max_value     TYPE int8 DEFAULT max_int8_value
      RETURNING VALUE(number) TYPE i.

    CLASS-METHODS get_random_p
      IMPORTING length               TYPE i
                decimals             TYPE i
                min_value            TYPE p DEFAULT min_p_value
                max_value            TYPE p DEFAULT max_p_value
      RETURNING VALUE(packed_number) TYPE REF TO data.

    CLASS-METHODS get_random_n
      IMPORTING length          TYPE i
      RETURNING VALUE(random_n) TYPE string.

    CLASS-METHODS get_random_d
      RETURNING VALUE(random_d) TYPE d.

    CLASS-METHODS get_random_t
      RETURNING VALUE(random_t) TYPE t.

    CLASS-METHODS get_random_utclong
      RETURNING VALUE(random_utc) TYPE utclong.

    CLASS-METHODS get_random_dec16
      RETURNING VALUE(random_dec16) TYPE decfloat16.

    CLASS-METHODS get_random_dec34
      RETURNING VALUE(random_dec34) TYPE decfloat34.

    CLASS-METHODS get_random_f
      RETURNING VALUE(random_f) TYPE f.

    CLASS-METHODS get_random_x
      IMPORTING length          TYPE i
      RETURNING VALUE(random_x) TYPE xstring.

    CLASS-METHODS get_random_xstring
      RETURNING VALUE(random_xstring) TYPE xstring.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_itab_builder IMPLEMENTATION.

  METHOD get_random_string.
    IF length IS NOT SUPPLIED.
      DATA(len) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 1
                                              max  = max_length )->get_next( ).
    ELSE.
      IF length NOT BETWEEN 1 AND max_length.
        len = max_length.
      ELSE.
        len = length.
      ENDIF.

      IF randomize_length = abap_true.
        len = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                          min  = 1
                                          max  = len )->get_next( ).
      ENDIF.
    ENDIF.

    DO len TIMES.
      DATA(num) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 0
                                              max  = strlen( character_set ) - 1 )->get_next( ).
      str &&= character_set+num(1).
    ENDDO.
  ENDMETHOD.

  METHOD get_random_number_sequence.
    IF length IS NOT SUPPLIED.
      DATA(len) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 1
                                              max  = max_length )->get_next( ).
    ELSE.
      IF length NOT BETWEEN 1 AND max_length.
        len = max_length.
      ELSE.
        len = length.
      ENDIF.
      IF randomize_length = abap_true.
        len = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                          min  = 1
                                          max  = len )->get_next( ).
      ENDIF.
    ENDIF.

    DO len TIMES.
      DATA(num) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 0
                                              max  = strlen( number_set ) - 1 )->get_next( ).
      numbers &&= number_set+num(1).
    ENDDO.
  ENDMETHOD.

  METHOD get_random_i.
    RETURN cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                       min  = min_value
                                       max  = max_value )->get_next( ).
  ENDMETHOD.

  METHOD get_random_int8.
    RETURN cl_abap_random_int8=>create( seed = cl_abap_random=>seed( )
                                        min  = min_value
                                        max  = max_value )->get_next( ).
  ENDMETHOD.

  METHOD get_random_p.
    IF length NOT BETWEEN 1 AND 16.
      RETURN.
    ENDIF.

    IF decimals NOT BETWEEN 0 AND 14.
      RETURN.
    ENDIF.

    TRY.
        CASE decimals.
          WHEN 0.
            DATA(a) = cl_abap_random_packed=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed=>p31_0( min_value ) max = CONV cl_abap_random_packed=>p31_0( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE a.
            packed_number->* = a.
          WHEN 1.
             DATA(b) = cl_abap_random_packed_dec1=>create( seed = cl_abap_random=>seed( )
                min = COND #( WHEN length = 1 THEN CONV cl_abap_random_packed_dec1=>p31_1( '0.1' ) ELSE CONV cl_abap_random_packed_dec1=>p31_1( min_value ) )
                max = COND #( WHEN length = 1 THEN CONV cl_abap_random_packed_dec1=>p31_1( '0.9' ) ELSE CONV cl_abap_random_packed_dec1=>p31_1( max_value ) )
              )->get_next( ).
            CREATE DATA packed_number LIKE b.
            packed_number->* = b.
          WHEN 2.
            DATA(c) = cl_abap_random_packed_dec2=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec2=>p31_2( min_value ) max = CONV cl_abap_random_packed_dec2=>p31_2( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE c.
            packed_number->* = c.
          WHEN 3.
            DATA(d) = cl_abap_random_packed_dec3=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec3=>p31_3( min_value ) max = CONV cl_abap_random_packed_dec3=>p31_3( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE d.
            packed_number->* = d.
          WHEN 4.
            DATA(e) = cl_abap_random_packed_dec4=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec4=>p31_4( min_value ) max = CONV cl_abap_random_packed_dec4=>p31_4( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE e.
            packed_number->* = e.
          WHEN 5.
            DATA(f) = cl_abap_random_packed_dec5=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec5=>p31_5( min_value ) max = CONV cl_abap_random_packed_dec5=>p31_5( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE f.
            packed_number->* = f.
          WHEN 6.
            DATA(g) = cl_abap_random_packed_dec6=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec6=>p31_6( min_value ) max = CONV cl_abap_random_packed_dec6=>p31_6( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE g.
            packed_number->* = g.
          WHEN 7.
            DATA(h) = cl_abap_random_packed_dec7=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec7=>p31_7( min_value ) max = CONV cl_abap_random_packed_dec7=>p31_7( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE h.
            packed_number->* = h.
          WHEN 8.
            DATA(i) = cl_abap_random_packed_dec8=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec8=>p31_8( min_value ) max = CONV cl_abap_random_packed_dec8=>p31_8( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE i.
            packed_number->* = i.
          WHEN 9.
            DATA(j) = cl_abap_random_packed_dec9=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec9=>p31_9( min_value ) max = CONV cl_abap_random_packed_dec9=>p31_9( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE j.
            packed_number->* = j.
          WHEN 10.
            DATA(k) = cl_abap_random_packed_dec10=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec10=>p31_10( min_value ) max = CONV cl_abap_random_packed_dec10=>p31_10( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE k.
            packed_number->* = k.
          WHEN 11.
            DATA(l) = cl_abap_random_packed_dec11=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec11=>p31_11( min_value ) max = CONV cl_abap_random_packed_dec11=>p31_11( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE l.
            packed_number->* = l.
          WHEN 12.
            DATA(m) = cl_abap_random_packed_dec12=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec12=>p31_12( min_value ) max = CONV cl_abap_random_packed_dec12=>p31_12( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE m.
            packed_number->* = m.
          WHEN 13.
            DATA(n) = cl_abap_random_packed_dec13=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec13=>p31_13( min_value ) max = CONV cl_abap_random_packed_dec13=>p31_13( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE n.
            packed_number->* = n.
          WHEN 14.
            DATA(o) = cl_abap_random_packed_dec14=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec14=>p31_14( min_value ) max = CONV cl_abap_random_packed_dec14=>p31_14( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE o.
            packed_number->* = o.
        ENDCASE.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD get_random_n.
    IF length > max_length OR length < 1.
      DATA(len) = max_length.
    ELSE.
      len = length.
    ENDIF.
    random_n = get_random_number_sequence( len ).
  ENDMETHOD.

  METHOD get_random_d.
    DATA(int) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                            min  = -100
                                            max  = 100 )->get_next( ).
    random_d =  start_date + int.
  ENDMETHOD.

  METHOD get_random_t.
    DATA(int) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                            min  = -36000
                                            max  = 36000 )->get_next( ).
    random_t = start_time + int.
  ENDMETHOD.

  METHOD get_random_utclong.
    DATA(int) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                            min  = -100
                                            max  = 100 )->get_next( ).

    random_utc = utclong_add( val = utclong_current( )
                              days = int
                              hours = int
                              minutes = int
                              seconds = int ).
  ENDMETHOD.

  METHOD get_random_dec16.
    random_dec16 = cl_abap_random_decfloat16=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
  ENDMETHOD.

  METHOD get_random_dec34.
    random_dec34 = cl_abap_random_decfloat34=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
  ENDMETHOD.

  METHOD get_random_f.
    random_f = cl_abap_random_float=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
  ENDMETHOD.

  METHOD get_random_x.
    DATA(random_string) = get_random_string( length ).
    random_x = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( random_string ).
  ENDMETHOD.

  METHOD get_random_xstring.
    DATA(random_string) = get_random_string( ).
    random_xstring = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( random_string ).
  ENDMETHOD.

  METHOD add_table_entries.

    IF table_entry_count < 0.
      table_entry_count = 1.
    ENDIF.

    IF table_entry_count > max_table_entry_count.
      table_entry_count = max_table_entry_count.
    ENDIF.

    DO table_entry_count TIMES.
      INSERT INITIAL LINE INTO TABLE itab->* ASSIGNING FIELD-SYMBOL(<line>).

      LOOP AT components ASSIGNING FIELD-SYMBOL(<comp>).
        DATA(tabix) = sy-tabix.

        IF <comp>-type IS INSTANCE OF cl_abap_elemdescr.
          DATA(tdo_elem) = CAST cl_abap_elemdescr( <comp>-type ).
          DATA(type_kind) = tdo_elem->type_kind.
          DATA(output_length) = tdo_elem->output_length.
          DATA(length) = tdo_elem->length.
          DATA(decimals) = tdo_elem->decimals.

          CASE type_kind.
            WHEN cl_abap_typedescr=>typekind_char.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_string( output_length ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_string( output_length ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_string.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_string( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_string( ).
              ENDIF.
            WHEN  cl_abap_typedescr=>typekind_hex.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_x( output_length ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_x( output_length ).
              ENDIF.
            WHEN  cl_abap_typedescr=>typekind_xstring.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_xstring( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_xstring( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_num.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_n( output_length ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_n( output_length ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_time.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_t( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_t( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_date.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_d( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_d( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_utclong.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_utclong( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_utclong( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_int
            OR cl_abap_typedescr=>typekind_int1 OR
            cl_abap_typedescr=>typekind_int2.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_i(  ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_i(  ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_int8.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_int8( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_int8( ).
              ENDIF.
            WHEN  cl_abap_typedescr=>typekind_packed.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_p(
                                  length    = length
                                  decimals  = decimals
                                )->*.
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_p(
                                     length    = length
                                     decimals  = decimals
                                   )->*.
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_decfloat16.
              IF is_elementary_line_type = abap_true.
                <line> =  lcl_itab_builder=>get_random_dec16( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_dec16( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_decfloat34.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_dec34( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_dec34( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_float.
              IF is_elementary_line_type = abap_true.
                <line> =  lcl_itab_builder=>get_random_f( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_f( ).
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDLOOP.
    ENDDO.
  ENDMETHOD.

  METHOD check_type.
    cl_abap_typedescr=>describe_by_name( EXPORTING p_name = type_name
                                         RECEIVING p_descr_ref = DATA(tdo)
                                         EXCEPTIONS type_not_found = 4 ).

    IF sy-subrc <> 0.
      type_exists = abap_false.
      RETURN.
    ELSE.
      type_exists = abap_true.
      line_type = CAST #( tdo ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_components.
    CASE TYPE OF line_type.
      WHEN TYPE cl_abap_structdescr.
        components = CAST cl_abap_structdescr( line_type )->get_components( ).
      WHEN TYPE cl_abap_elemdescr.
        is_elementary_line_type = abap_true.
        "Build the components table manually with TABLE_LINE to enable a loop..
        DATA(tdo_elem) = CAST cl_abap_elemdescr( line_type ).
        components = VALUE #( ( name = `TABLE_LINE` type = tdo_elem ) ).
      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_keys.
    "Checking if the provided key names are components of the structured type
    "In case of elementary line types, TABLE_LINE is used.
    IF primary_key_components IS NOT INITIAL.
      IF is_elementary_line_type = abap_true.
        CLEAR primary_key_components.
        APPEND `TABLE_LINE` TO primary_key_components.
      ELSE.
        DATA(comp_names) = CAST cl_abap_structdescr( line_type )->components.
        LOOP AT primary_key_components INTO DATA(prkey).
          IF NOT line_exists( components[ name = prkey ] ).
            DELETE primary_key_components WHERE table_line = prkey.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    "If no key names are provided, and the type refers to a global structured type such as
    "DDIC database tables or CDS entites, the following implementation collects those components
    "that are specified as keys for the repository object.
    IF primary_key_components IS INITIAL AND is_elementary_line_type = abap_false.
      "Checking whether the type is a global type, available as repository object.
      DATA(filter) = xco_cp_abap_repository=>object_name->get_filter( xco_cp_abap_sql=>constraint->equal( type_name ) ).
      DATA(repo_objects) = xco_cp_abap_repository=>objects->where( VALUE #( ( filter ) ) )->in( xco_cp_abap=>repository )->get( ).

      LOOP AT repo_objects INTO DATA(obj).
        DATA(val) = obj->type->value.
        IF val = `DDLS`.
          EXIT.
        ELSEIF val = `TABL`.
          EXIT.
        ENDIF.
      ENDLOOP.

      CASE val.
        WHEN `TABL`.
          "Retrieving the key component names of DDIC database tables
          primary_key_components = xco_cp_abap_repository=>object->tabl->database_table->for( CONV #( type_name ) )->fields->key->get_names( ).
          "Assuming the first key field in the list is the client field, removing this field from the key components.
          DELETE primary_key_components INDEX 1.
        WHEN `DDLS`.
          "Retrieving the key component names of CDS entities
          DATA(ddls_key_spec) = xco_cp_abap_repository=>object->ddls->for( CONV #( type_name ) )->entity( )->fields->all->get_names( ).
          LOOP AT ddls_key_spec INTO DATA(k).
            DATA(is_key) = xco_cp_abap_repository=>object->ddls->for( CONV #( type_name ) )->view_entity( )->field( k )->content( )->get_key_indicator( ).
            IF is_key IS NOT INITIAL.
              APPEND k TO primary_key_components.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDIF.

    "If the primary key table is still empty, e.g. when referring to an CDS abstract entity, the following implementation
    "adds the first component as key component.
    IF primary_key_components IS INITIAL AND is_elementary_line_type = abap_false.
      comp_names = CAST cl_abap_structdescr( line_type )->components.
      IF lines( comp_names ) > 0.
        APPEND comp_names[ 1 ]-name TO primary_key_components.
      ENDIF.
    ENDIF.

    "If an elementary line type is used, and the primary key table is still empty, adding TABLE_LINE.
    IF primary_key_components IS INITIAL AND is_elementary_line_type = abap_true.
      APPEND `TABLE_LINE` TO primary_key_components.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Concrete example classes inheriting from the abstract class
"The example concrete builder classes construct internal table of different
"kinds. Among them, standard tables with non-unique primary table key,
"standard tables with empty key, sorted and hashed tables.
"Additionally, a concrete builder class creates random data for an internal
"table that is supplied when calling the object creation method.

CLASS lcl_builder_std_itab_w_prkey DEFINITION INHERITING FROM lcl_itab_builder CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: build_type_info REDEFINITION,
      build_table_type REDEFINITION,
      build_data_object REDEFINITION,
      build_random_data REDEFINITION,
      build_table_keys REDEFINITION,
      build_components REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_builder_std_itab_w_prkey IMPLEMENTATION.

  METHOD build_type_info.
    check_type( ).
  ENDMETHOD.

  METHOD build_components.
    CHECK type_exists = abap_true.
    handle_components( ).
  ENDMETHOD.

  METHOD build_table_keys.
    CHECK type_exists = abap_true.
    handle_keys( ).
  ENDMETHOD.

  METHOD build_table_type.
    CHECK type_exists = abap_true.

    table_type_descr_obj = cl_abap_tabledescr=>get(
          p_line_type  = line_type
          p_table_kind = cl_abap_tabledescr=>tablekind_std
          p_key        = VALUE #( FOR wa IN primary_key_components ( name = wa ) )
          p_unique     = cl_abap_typedescr=>false ).
  ENDMETHOD.

  METHOD build_data_object.
    CHECK type_exists = abap_true.
    CREATE DATA itab TYPE HANDLE table_type_descr_obj.
    "Note: As the keys are non-unique, no temporary table is created as in other concrete
    "builder classes.
  ENDMETHOD.

  METHOD build_random_data.
    CHECK type_exists = abap_true.
    add_table_entries( table_entries_to_create ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_builder_std_itab_empty_key DEFINITION INHERITING FROM lcl_itab_builder CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: build_type_info REDEFINITION,
      build_table_keys REDEFINITION,
      build_table_type REDEFINITION,
      build_data_object REDEFINITION,
      build_random_data REDEFINITION,
      build_components REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_builder_std_itab_empty_key IMPLEMENTATION.
  METHOD build_type_info.
    check_type( ).
  ENDMETHOD.

  METHOD build_components.
    CHECK type_exists = abap_true.
    handle_components( ).
  ENDMETHOD.

  METHOD build_table_keys.
    CHECK type_exists = abap_true.
    CLEAR primary_key_components.
  ENDMETHOD.

  METHOD build_table_type.
    CHECK type_exists = abap_true.

    table_type_descr_obj = cl_abap_tabledescr=>get(
           p_line_type  = line_type
           p_table_kind = cl_abap_tabledescr=>tablekind_std
           p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).
  ENDMETHOD.

  METHOD build_data_object.
    CHECK type_exists = abap_true.
    CREATE DATA itab TYPE HANDLE table_type_descr_obj.
  ENDMETHOD.

  METHOD build_random_data.
    CHECK type_exists = abap_true.
    add_table_entries( table_entries_to_create ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_builder_sorted_itab DEFINITION INHERITING FROM lcl_itab_builder CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: build_type_info REDEFINITION,
      build_table_keys REDEFINITION,
      build_table_type REDEFINITION,
      build_data_object REDEFINITION,
      build_random_data REDEFINITION,
      build_components REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    "Attributes for tables that stores content and the table type
    DATA temporary_itab TYPE REF TO data.
    DATA temporary_table_type TYPE REF TO cl_abap_tabledescr.
ENDCLASS.

CLASS lcl_builder_sorted_itab IMPLEMENTATION.
  METHOD build_type_info.
    check_type( ).
  ENDMETHOD.

  METHOD build_components.
    CHECK type_exists = abap_true.
    handle_components( ).
  ENDMETHOD.

  METHOD build_table_keys.
    CHECK type_exists = abap_true.
    handle_keys( ).
  ENDMETHOD.

  METHOD build_table_type.
    CHECK type_exists = abap_true.

    table_type_descr_obj = cl_abap_tabledescr=>get(
           p_line_type  = line_type
           p_table_kind = cl_abap_tabledescr=>tablekind_sorted
           p_key        = VALUE #( FOR wa IN primary_key_components ( name = wa ) )
           p_unique     = cl_abap_typedescr=>true ).

    "In the previous statement, the type description object for the created table type
    "is assigned. The following assignment is performed for a temporary table.
    "This is done to not interfere with unique primary table keys and non-modifiable components
    "when adding table entries (because the creation procedure shared by all objects is performed
    "using the 'itab' attribute. Therefore, a standard table with empty key is created using the
    "same line type. The table entries are added to this table. In a later step, the table entries
    "are copied back to the original internal table.
    temporary_table_type = cl_abap_tabledescr=>get(
            p_line_type  = line_type
            p_table_kind = cl_abap_tabledescr=>tablekind_std
            p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).

  ENDMETHOD.

  METHOD build_data_object.
    CHECK type_exists = abap_true.

    "Creating two internal tables
    "See the comment in the build_table_type method.
    CREATE DATA itab TYPE HANDLE temporary_table_type.
    CREATE DATA temporary_itab TYPE HANDLE table_type_descr_obj.
  ENDMETHOD.

  METHOD build_random_data.
    CHECK type_exists = abap_true.
    add_table_entries( table_entries_to_create ).

    "See the comment in the build_table_type method.
    LOOP AT itab->* ASSIGNING FIELD-SYMBOL(<a>).
      INSERT <a> INTO TABLE temporary_itab->*.
    ENDLOOP.

    CLEAR itab.
    CREATE DATA itab LIKE temporary_itab->*.

    LOOP AT temporary_itab->* ASSIGNING FIELD-SYMBOL(<b>).
      INSERT <b> INTO TABLE itab->*.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_builder_hashed_itab DEFINITION INHERITING FROM lcl_itab_builder CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: build_type_info REDEFINITION,
      build_table_keys REDEFINITION,
      build_table_type REDEFINITION,
      build_data_object REDEFINITION,
      build_random_data REDEFINITION,
      build_components REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    DATA temporary_itab TYPE REF TO data.
    DATA temporary_table_type TYPE REF TO cl_abap_tabledescr.
ENDCLASS.

CLASS lcl_builder_hashed_itab IMPLEMENTATION.

  METHOD build_type_info.
    check_type( ).
  ENDMETHOD.

  METHOD build_components.
    CHECK type_exists = abap_true.
    handle_components( ).
  ENDMETHOD.

  METHOD build_table_keys.
    CHECK type_exists = abap_true.
    handle_keys( ).
  ENDMETHOD.

  METHOD build_table_type.
    CHECK type_exists = abap_true.

    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    table_type_descr_obj = cl_abap_tabledescr=>get(
           p_line_type  = line_type
           p_table_kind = cl_abap_tabledescr=>tablekind_hashed
           p_key        = VALUE #( FOR wa IN primary_key_components ( name = wa ) )
           p_unique     = cl_abap_typedescr=>true ).

    temporary_table_type = cl_abap_tabledescr=>get(
            p_line_type  = line_type
            p_table_kind = cl_abap_tabledescr=>tablekind_std
            p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).
  ENDMETHOD.

  METHOD build_data_object.
    CHECK type_exists = abap_true.

    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    CREATE DATA itab TYPE HANDLE temporary_table_type.
    CREATE DATA temporary_itab TYPE HANDLE table_type_descr_obj.
  ENDMETHOD.

  METHOD build_random_data.
    CHECK type_exists = abap_true.
    add_table_entries( table_entries_to_create ).

    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    LOOP AT itab->* ASSIGNING FIELD-SYMBOL(<a>).
      INSERT <a> INTO TABLE temporary_itab->*.
    ENDLOOP.

    CLEAR itab.
    CREATE DATA itab LIKE temporary_itab->*.

    LOOP AT temporary_itab->* ASSIGNING FIELD-SYMBOL(<b>).
      INSERT <b> INTO TABLE itab->*.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_itab_data_provider DEFINITION INHERITING FROM lcl_itab_builder CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: build_type_info REDEFINITION,
      build_table_keys REDEFINITION,
      build_table_type REDEFINITION,
      build_data_object REDEFINITION,
      build_random_data REDEFINITION,
      build_components REDEFINITION.

    METHODS constructor IMPORTING itab TYPE ANY TABLE.
  PROTECTED SECTION.
  PRIVATE SECTION.
    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    DATA temporary_itab TYPE REF TO data.
    DATA temporary_table_type TYPE REF TO cl_abap_tabledescr.
ENDCLASS.

CLASS lcl_itab_data_provider IMPLEMENTATION.

  METHOD build_type_info.
    line_type = table_type_descr_obj->get_table_line_type( ).
  ENDMETHOD.

  METHOD build_components.
    handle_components( ).
  ENDMETHOD.

  METHOD build_table_keys.
    "The table type including the key components is provided by the table supplied.
    CLEAR primary_key_components.
  ENDMETHOD.

  METHOD build_table_type.
    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    temporary_table_type = cl_abap_tabledescr=>get(
            p_line_type  = line_type
            p_table_kind = cl_abap_tabledescr=>tablekind_std
            p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).
  ENDMETHOD.

  METHOD build_data_object.
    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    CREATE DATA itab TYPE HANDLE temporary_table_type.
    CREATE DATA temporary_itab TYPE HANDLE table_type_descr_obj.
  ENDMETHOD.

  METHOD build_random_data.
    add_table_entries( table_entries_to_create ).

    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    LOOP AT itab->* ASSIGNING FIELD-SYMBOL(<a>).
      INSERT <a> INTO TABLE temporary_itab->*.
    ENDLOOP.

    CLEAR itab.
    CREATE DATA itab LIKE temporary_itab->*.

    LOOP AT temporary_itab->* ASSIGNING FIELD-SYMBOL(<b>).
      INSERT <b> INTO TABLE itab->*.
    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    table_type_descr_obj = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab ) ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Class that includes factory methods that provide objects of the
"concrete builder classes
"The class is used by the consumers such as the global class in this
"example. It is responsible for a correct order of method calls so that
"an appropriate and correct object is returned.

CLASS lcl_itab_provider DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM table_kind,
             standard_w_nonunique_pr_key,
             sorted_w_unique_pr_key,
             hashed_w_unique_pr_key,
             standard_w_empty_key,
           END OF ENUM table_kind.

    CLASS-METHODS create_itab IMPORTING VALUE(type_name)  TYPE string
                                        table_kind        TYPE lcl_itab_provider=>table_kind DEFAULT standard_w_empty_key
                                        key_components    TYPE string_table OPTIONAL
                                        table_entry_count TYPE i DEFAULT 3
                              RETURNING VALUE(build)      TYPE REF TO lcl_itab_builder.

    CLASS-METHODS populate_itab IMPORTING itab              TYPE ANY TABLE
                                          table_entry_count TYPE i DEFAULT 3
                                RETURNING VALUE(build)      TYPE REF TO lcl_itab_builder.
ENDCLASS.

CLASS lcl_itab_provider IMPLEMENTATION.
  METHOD create_itab.
    type_name = to_upper( condense( val = type_name to = `` ) ).

    CASE table_kind.
      WHEN standard_w_nonunique_pr_key.
        build = NEW lcl_builder_std_itab_w_prkey( ).
      WHEN standard_w_empty_key.
        build = NEW lcl_builder_std_itab_empty_key( ).
      WHEN sorted_w_unique_pr_key.
        build = NEW lcl_builder_sorted_itab( ).
      WHEN hashed_w_unique_pr_key.
        build = NEW lcl_builder_hashed_itab( ).
      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

    build->type_name = type_name.
    build->table_entries_to_create = table_entry_count.
    build->primary_key_components = key_components.
    build->build_type_info( ).
    build->build_components( ).
    build->build_table_keys( ).
    build->build_table_type( ).
    build->build_data_object( ).
    build->build_random_data( ).
  ENDMETHOD.

  METHOD populate_itab.
    build = NEW lcl_itab_data_provider( itab ).
    build->table_entries_to_create = table_entry_count.
    build->build_type_info( ).
    build->build_components( ).
    build->build_table_type( ).
    build->build_data_object( ).
    build->build_random_data( ).
  ENDMETHOD.
ENDCLASS.
