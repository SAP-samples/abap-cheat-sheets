*&---------------------------------------------------------------------*
*& Interface
*&---------------------------------------------------------------------*

INTERFACE lif_chain_of_resp.
  METHODS: process IMPORTING type_descr  TYPE REF TO cl_abap_typedescr
                   RETURNING VALUE(info) TYPE string_table,
    set_handler IMPORTING iref TYPE REF TO lif_chain_of_resp.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Abstract handler class
*&---------------------------------------------------------------------*

CLASS lcl_base_handler DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_chain_of_resp.
  PROTECTED SECTION.
    DATA iref_handler TYPE REF TO lif_chain_of_resp.
ENDCLASS.

CLASS lcl_base_handler IMPLEMENTATION.
  METHOD lif_chain_of_resp~set_handler.
    me->iref_handler = iref.
  ENDMETHOD.

  METHOD lif_chain_of_resp~process.
    IF iref_handler IS INITIAL.
      APPEND `Cannot handle request` TO info.
    ELSE.
      info = iref_handler->process( type_descr ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete handler classes
*&---------------------------------------------------------------------*

CLASS lcl_concrete_handler1 DEFINITION INHERITING FROM lcl_base_handler.
  PUBLIC SECTION.
    METHODS lif_chain_of_resp~process REDEFINITION.
ENDCLASS.

CLASS lcl_concrete_handler1 IMPLEMENTATION.
  METHOD lif_chain_of_resp~process.
    DATA(cl_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->get_relative_name( ).

    IF type_descr IS INSTANCE OF cl_abap_elemdescr
    AND type_descr IS NOT INSTANCE OF cl_abap_enumdescr.
      APPEND |Class { cl_name } handled request.| TO info.
      APPEND `--- Type information ---` TO info.

      DATA(tdo_elem) = CAST cl_abap_elemdescr( type_descr ).
      DATA(type_category_elem) = tdo_elem->kind.
      DATA(type_kind_elem) = tdo_elem->type_kind.
      DATA(decimals_elem) = tdo_elem->decimals.
      DATA(output_length_elem) = tdo_elem->output_length.
      DATA(absolute_name_elem) = tdo_elem->absolute_name.
      DATA(relative_name_elem) = tdo_elem->get_relative_name( ).
      DATA(is_ddic_type_elem) = tdo_elem->is_ddic_type( ).

      APPEND |Type category: { type_category_elem }| TO info.
      APPEND |Type kind: { type_kind_elem }| TO info.
      APPEND |Decimals: { decimals_elem }| TO info.
      APPEND |Output length: { output_length_elem }| TO info.
      APPEND |Absolute name: { absolute_name_elem }| TO info.
      APPEND |Relative name: { relative_name_elem }| TO info.
      APPEND |Is DDIC type: { COND #( WHEN is_ddic_type_elem = abap_true THEN abap_true ELSE `-` ) }| TO info.
    ELSE.
      info = super->lif_chain_of_resp~process( type_descr ).
      IF info IS INITIAL.
        APPEND |Class { cl_name } could not handle request.| TO info.
      ELSE.
        INSERT |Class { cl_name } could not handle request.| INTO info INDEX 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_concrete_handler2 DEFINITION INHERITING FROM lcl_base_handler.
  PUBLIC SECTION.
    METHODS lif_chain_of_resp~process REDEFINITION.
ENDCLASS.

CLASS lcl_concrete_handler2 IMPLEMENTATION.
  METHOD lif_chain_of_resp~process.
    DATA(cl_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->get_relative_name( ).

    IF type_descr IS INSTANCE OF cl_abap_structdescr.
      APPEND |Class { cl_name } handled request.| TO info.
      APPEND `--- Type information ---` TO info.

      DATA(tdo_struc) = CAST cl_abap_structdescr( type_descr ).
      DATA(type_category_struc) = tdo_struc->kind.
      DATA(type_kind_struc) = tdo_struc->type_kind.
      DATA(absolute_name_struc) = tdo_struc->absolute_name.
      DATA(relative_name_struc) = tdo_struc->get_relative_name( ).
      DATA(is_ddic_type_struc) = tdo_struc->is_ddic_type( ).
      DATA(comps_struc) = tdo_struc->components.

      APPEND |Type category: { type_category_struc }| TO info.
      APPEND |Type kind: { type_kind_struc }| TO info.
      APPEND |Absolute name: { absolute_name_struc }| TO info.
      APPEND |Relative name: { relative_name_struc }| TO info.
      APPEND |Is DDIC type: { COND #( WHEN is_ddic_type_struc = abap_true THEN abap_true ELSE `-` ) }| TO info.
      APPEND |Components: { REDUCE string( INIT s = VALUE #( ) FOR wa IN comps_struc NEXT s &&= |{ COND #( WHEN s IS NOT INITIAL THEN ` ` ) }{ wa-name }| ) }| TO info.
    ELSE.
      info = super->lif_chain_of_resp~process( type_descr ).
      IF info IS INITIAL.
        APPEND |Class { cl_name } could not handle request.| TO info.
      ELSE.
        INSERT |Class { cl_name } could not handle request.| INTO info INDEX 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_concrete_handler3 DEFINITION INHERITING FROM lcl_base_handler.
  PUBLIC SECTION.
    METHODS lif_chain_of_resp~process REDEFINITION.
ENDCLASS.

CLASS lcl_concrete_handler3 IMPLEMENTATION.
  METHOD lif_chain_of_resp~process.
    DATA(cl_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->get_relative_name( ).

    IF type_descr IS INSTANCE OF cl_abap_tabledescr.
      APPEND |Class { cl_name } handled request.| TO info.
      APPEND `--- Type information ---` TO info.

      DATA(tdo_tab) = CAST cl_abap_tabledescr( type_descr ).
      DATA(type_category_tab) = tdo_tab->kind.
      DATA(type_kind_tab) = tdo_tab->type_kind.
      DATA(absolute_name_tab) = tdo_tab->absolute_name.
      DATA(relative_name_tab) = tdo_tab->get_relative_name( ).
      DATA(table_kind_itab) = tdo_tab->table_kind.
      DATA(table_has_unique_key_itab) = tdo_tab->has_unique_key.
      DATA(table_keys_itab) = tdo_tab->get_keys( ).
      DATA(table_is_ddic_typw) = tdo_tab->is_ddic_type( ).

      APPEND |Type category: { type_category_tab }| TO info.
      APPEND |Type kind: { type_kind_tab }| TO info.
      APPEND |Absolute name: { absolute_name_tab }| TO info.
      APPEND |Relative name: { relative_name_tab }| TO info.
      APPEND |Table kind: { table_kind_itab }| TO info.
      APPEND |Does table have a unique key: { COND #( WHEN table_has_unique_key_itab = abap_true THEN abap_true ELSE `-` ) }| TO info.
      APPEND |Table keys: { REDUCE string( INIT str = `` FOR <key2> IN table_keys_itab NEXT str = |{ str }| &&
             |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ REDUCE string( INIT str2 = `` FOR <key3> IN <key2>-components NEXT str2 = |{ str2 }| &&
             |{ COND #( WHEN str2 IS NOT INITIAL THEN `/` ) }{ <key3>-name }| ) } (is primary: "{ <key2>-is_primary }", |  &&
             |is unique: "{ <key2>-is_unique }", key kind: "{ <key2>-key_kind }", access kind: "{ <key2>-access_kind }")| ) }| TO info.
      APPEND |Is DDIC type: { COND #( WHEN table_is_ddic_typw = abap_true THEN abap_true ELSE `-` ) }| TO info.
      TRY.
          DATA(table_component_info_itab) = CAST cl_abap_structdescr( tdo_tab->get_table_line_type( ) )->components.
          APPEND |Table components: { REDUCE string( INIT s = VALUE #( ) FOR w IN table_component_info_itab NEXT s &&= |{ COND #( WHEN s IS NOT INITIAL THEN ` ` ) }{ w-name }| ) }| TO info.
        CATCH cx_sy_move_cast_error.
          APPEND |Table components: The table has not a structured line type.| TO info.
      ENDTRY.
    ELSE.
      info = super->lif_chain_of_resp~process( type_descr ).
      IF info IS INITIAL.
        APPEND |Class { cl_name } could not handle request.| TO info.
      ELSE.
        INSERT |Class { cl_name } could not handle request.| INTO info INDEX 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_concrete_handler4 DEFINITION INHERITING FROM lcl_base_handler.
  PUBLIC SECTION.
    METHODS: lif_chain_of_resp~process REDEFINITION.
ENDCLASS.

CLASS lcl_concrete_handler4 IMPLEMENTATION.
  METHOD lif_chain_of_resp~process.
    DATA(cl_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->get_relative_name( ).
    APPEND |Class { cl_name } handled request.| TO info.
    APPEND `--- Type information ---` TO info.

    IF type_descr IS INITIAL.
      APPEND `Cannot extract type information` TO info.
    ELSE.
      DATA(type_category) = type_descr->kind.
      DATA(type_kind) = type_descr->type_kind.
      DATA(absolute_name) = type_descr->absolute_name.
      DATA(relative_name) = type_descr->get_relative_name( ).

      APPEND |Type category: { type_category }| TO info.
      APPEND |Type kind: { type_kind }| TO info.
      APPEND |Absolute name: { absolute_name }| TO info.
      APPEND |Relative name: { relative_name }| TO info.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
