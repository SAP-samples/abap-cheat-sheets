*&---------------------------------------------------------------------*
*& Iterator interface
*&---------------------------------------------------------------------*

INTERFACE lif_iterator.
  METHODS: get_next RETURNING VALUE(item) TYPE string,
    get_previous RETURNING VALUE(item) TYPE string,
    has_next RETURNING VALUE(has_next) TYPE abap_boolean,
    get_current_position RETURNING VALUE(position) TYPE i,
    reset.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Collection interface
*&---------------------------------------------------------------------*

INTERFACE lif_collection.
  METHODS: get_iterator RETURNING VALUE(iterator) TYPE REF TO lif_iterator,
    get_number_of_items RETURNING VALUE(count) TYPE i,
    get_item_at_position IMPORTING idx         TYPE i
                         RETURNING VALUE(item) TYPE string.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Concrete iterator
*&---------------------------------------------------------------------*

CLASS lcl_iterator DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_iterator.
    METHODS constructor IMPORTING collection TYPE REF TO lif_collection.
  PRIVATE SECTION.
    DATA: coll  TYPE REF TO lif_collection,
          index TYPE i.
ENDCLASS.

CLASS lcl_iterator IMPLEMENTATION.
  METHOD constructor.
    coll = collection.
    index = 0.
  ENDMETHOD.

  METHOD lif_iterator~has_next.
    IF coll IS BOUND.
      DATA(num_items) = coll->get_number_of_items( ).
      has_next = xsdbool( index < num_items ).
    ELSE.
      has_next = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD lif_iterator~get_next.
    IF coll IS NOT BOUND.
      item = ``.
      RETURN.
    ENDIF.
    IF lif_iterator~has_next( ) = abap_true.
      index += 1.
      item = coll->get_item_at_position( index ).
    ELSE.
      item = ``.
    ENDIF.
  ENDMETHOD.

  METHOD lif_iterator~get_previous.
    IF coll IS NOT BOUND OR index <= 1.
      item = ``.
      RETURN.
    ENDIF.
    index -= 1.
    item = coll->get_item_at_position( index ).
  ENDMETHOD.

  METHOD lif_iterator~get_current_position.
    position = index.
  ENDMETHOD.

  METHOD lif_iterator~reset.
    index = 0.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete collection (Example 1)
*&---------------------------------------------------------------------*

CLASS lcl_collection_strings DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_collection.
    METHODS add IMPORTING item TYPE string.
  PRIVATE SECTION.
    DATA items TYPE string_table.
ENDCLASS.

CLASS lcl_collection_strings IMPLEMENTATION.
  METHOD lif_collection~get_iterator.
    iterator = NEW lcl_iterator( me ).
  ENDMETHOD.

  METHOD lif_collection~get_number_of_items.
    count = lines( items ).
  ENDMETHOD.

  METHOD lif_collection~get_item_at_position.
    IF idx BETWEEN 1 AND lines( items ).
      item = items[ idx ].
    ELSE.
      item = ``.
    ENDIF.
  ENDMETHOD.

  METHOD add.
    APPEND item TO items.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete collection (Example 2)
*&---------------------------------------------------------------------*

"Local exception class supporting the concrete collection class
CLASS lcx_error DEFINITION INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcx_error IMPLEMENTATION.
ENDCLASS.

CLASS lcl_collection_line2string DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_collection.
    METHODS constructor IMPORTING itab TYPE INDEX TABLE
                        RAISING   lcx_error.
  PRIVATE SECTION.
    DATA: it                 TYPE REF TO data,
          has_elem_line_type TYPE abap_boolean.
ENDCLASS.

CLASS lcl_collection_line2string IMPLEMENTATION.
  METHOD lif_collection~get_iterator.
    iterator = NEW lcl_iterator( me ).
  ENDMETHOD.

  METHOD lif_collection~get_number_of_items.
    count = lines( it->* ).
  ENDMETHOD.

  METHOD lif_collection~get_item_at_position.
    IF idx BETWEEN 1 AND lines( it->* ).
      FIELD-SYMBOLS <fs> TYPE INDEX TABLE.
      ASSIGN it->* TO <fs>.
      ASSIGN <fs>[ idx ] TO FIELD-SYMBOL(<line>).

      IF has_elem_line_type = abap_true.
        item = <line>.
      ELSE.
        DO.
          ASSIGN <line>-(sy-index) TO FIELD-SYMBOL(<comp>).
          "Older syntax
          "ASSIGN COMPONENT sy-index OF STRUCTURE <line> TO FIELD-SYMBOL(<comp>).
          IF sy-subrc = 0.
            item &&= COND #( WHEN sy-index = 1 THEN |{ <comp> }| ELSE |, { <comp> }| ).
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ELSE.
      item = ``.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    "Component check using RTTI
    "The example only processes internal tables with elementary components.
    DATA(tdo_tab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab ) ).

    TRY.
        DATA(table_comps) = CAST cl_abap_structdescr( tdo_tab->get_table_line_type( ) )->get_components( ).

        LOOP AT table_comps INTO DATA(wa) WHERE type->kind <> cl_abap_typedescr=>kind_elem.
          DATA(message) = xco_cp=>string( `Internal table not supported by the example (non-elementary component available)`
            )->as_message( xco_cp_message=>type->error )->value.
          RAISE EXCEPTION TYPE lcx_error MESSAGE ID message-msgid TYPE message-msgty
                                         NUMBER message-msgno WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4.
        ENDLOOP.

*"As an alternative to the LOOP statement, you may also choose to use the newer READ TABLE ... WHERE syntax.
*READ TABLE table_comps TRANSPORTING NO fieldS WHERE type->kind <> cl_abap_typedescr=>kind_elem.
*IF sy-subrc = 0.
*  DATA(msg) = xco_cp=>string( `Internal table not supported by the example` )->as_message( xco_cp_message=>type->error )->value.
*  RAISE EXCEPTION TYPE lcx_error MESSAGE ID msg-msgid TYPE msg-msgty NUMBER msg-msgno WITH msg-msgv1 msg-msgv2 msg-msgv3 msg-msgv4.
*ENDIF.

      CATCH cx_sy_move_cast_error.
        DATA(tab_line_type) = tdo_tab->get_table_line_type( ).
        IF tab_line_type IS INSTANCE OF cl_abap_elemdescr.
          has_elem_line_type = abap_true.
        ELSE.
          DATA(m) = xco_cp=>string( `Internal table not supported by the example (line type not supported)`
            )->as_message( xco_cp_message=>type->error )->value.
          RAISE EXCEPTION TYPE lcx_error MESSAGE ID m-msgid TYPE m-msgty
                                         NUMBER m-msgno WITH m-msgv1 m-msgv2 m-msgv3 m-msgv4.
        ENDIF.
    ENDTRY.

    CREATE DATA it LIKE itab.
    it->* = itab.
  ENDMETHOD.
ENDCLASS.
