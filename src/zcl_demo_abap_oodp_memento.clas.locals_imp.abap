CLASS lcl DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*& Memento class
*&---------------------------------------------------------------------*

CLASS lcl_memento DEFINITION FINAL CREATE PRIVATE FRIENDS lcl.
  PUBLIC SECTION.
    METHODS: get_tag RETURNING VALUE(ret_tag) TYPE string,
             get_timestamp RETURNING VALUE(ret_ts) TYPE string.
  PRIVATE SECTION.
    METHODS: constructor IMPORTING state TYPE string
                                   tag   TYPE string,
             get_state RETURNING VALUE(ret_state) TYPE string.
    DATA: state     TYPE string,
          tag       TYPE string,
          timestamp TYPE utclong.
ENDCLASS.

CLASS lcl_memento IMPLEMENTATION.
  METHOD constructor.
    me->state = state.
    me->tag = tag.
    me->timestamp = utclong_current( ).
  ENDMETHOD.

  METHOD get_state.
    ret_state = me->state.
  ENDMETHOD.

  METHOD get_tag.
    ret_tag = me->tag.
  ENDMETHOD.

  METHOD get_timestamp.
    ret_ts = me->timestamp.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class for which mementos should be created
*&---------------------------------------------------------------------*

CLASS lcl DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS get_obj RETURNING VALUE(obj) TYPE REF TO lcl.
    METHODS: get_state RETURNING VALUE(ret_state) TYPE string,
             change_state IMPORTING text      TYPE string
                                    add_space TYPE abap_boolean DEFAULT abap_true,
             save IMPORTING tag                 TYPE string
                  RETURNING VALUE(oref_memento) TYPE REF TO lcl_memento,
             restore IMPORTING memento TYPE REF TO lcl_memento.
  PRIVATE SECTION.
    CLASS-DATA oref TYPE REF TO lcl.
    DATA state TYPE string.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD get_state.
    ret_state = state.
  ENDMETHOD.

  METHOD save.
    oref_memento = NEW #( state = state
                          tag   = tag ).
  ENDMETHOD.

  METHOD restore.
    state = memento->get_state( ).
  ENDMETHOD.

  METHOD change_state.
    state = |{ state }{ COND #( WHEN state IS NOT INITIAL AND add_space = abap_true THEN ` ` ) }{ text }|.
  ENDMETHOD.

  METHOD get_obj.
    IF oref IS NOT BOUND.
      oref = NEW lcl( ).
    ENDIF.
    obj = oref.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class that manages mementos
*&---------------------------------------------------------------------*

CLASS lcl_memento_controller DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: save_memento IMPORTING tag TYPE string,
             undo CHANGING lcl_obj TYPE REF TO lcl,
             redo CHANGING lcl_obj TYPE REF TO lcl,
             restore_by_tag IMPORTING tag                   TYPE string
                            CHANGING  lcl_obj               TYPE REF TO lcl
                            RETURNING VALUE(restoration_ok) TYPE abap_boolean,
             get_memento_log RETURNING VALUE(states) TYPE string_table,
             constructor IMPORTING oref TYPE REF TO lcl.
  PRIVATE SECTION.
    DATA mementos_tab TYPE TABLE OF REF TO lcl_memento WITH EMPTY KEY.
    DATA redo_tab     TYPE TABLE OF REF TO lcl_memento WITH EMPTY KEY.
    DATA lcl_object TYPE REF TO lcl.
ENDCLASS.

CLASS lcl_memento_controller IMPLEMENTATION.
  METHOD save_memento.
    "Clearing the redo table when a new state is added
    CLEAR redo_tab.
    "Adding the memento to the table holding all stored mementos
    APPEND lcl_object->save( tag ) TO mementos_tab.
  ENDMETHOD.

  METHOD undo.
    DATA(count) = lines( mementos_tab ).

    IF count <= 1.
      RETURN.
    ENDIF.

    "Adding the last memento from the memento table to the redo table
    READ TABLE mementos_tab INTO DATA(memento_oref) INDEX count.
    IF sy-subrc = 0.
      APPEND memento_oref TO redo_tab.
      DELETE mementos_tab INDEX count.
    ENDIF.

    "Restoring the previous state
    READ TABLE mementos_tab INTO memento_oref INDEX lines( mementos_tab ).
    IF sy-subrc = 0.
      lcl_object->restore( memento_oref ).
      lcl_obj = lcl_object.
    ENDIF.
  ENDMETHOD.

  METHOD redo.
    DATA(count) = lines( redo_tab ).

    IF count = 0.
      RETURN.
    ENDIF.

    "Restoring the state from the redo table
    READ TABLE redo_tab INTO DATA(memento_oref) INDEX count.
    IF sy-subrc = 0.
      DELETE redo_tab INDEX count.
      "Adding the memento to the memento table
      APPEND memento_oref TO mementos_tab.
      lcl_object->restore( memento_oref ).
      lcl_obj = lcl_object.
    ENDIF.
  ENDMETHOD.

  METHOD restore_by_tag.
    LOOP AT mementos_tab INTO DATA(memento_oref).
      IF memento_oref->get_tag( ) = tag.
        lcl_object->restore( memento_oref ).
        lcl_obj = lcl_object.
        restoration_ok = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
    restoration_ok = abap_false.
  ENDMETHOD.

  METHOD get_memento_log.
    DATA idx TYPE i.
    LOOP AT mementos_tab INTO DATA(memento_wa).
      idx = sy-tabix.
      APPEND |{ idx }) Memento created with tag "{ memento_wa->get_tag( ) }" at { memento_wa->get_timestamp( ) }| TO states.
    ENDLOOP.

    IF lines( redo_tab ) > 0.
      APPEND INITIAL LINE TO states.
      APPEND `*********************** Redo table entries ************************` TO states.
      LOOP AT redo_tab INTO memento_wa.
        idx = sy-tabix.
        APPEND |{ idx }) Memento with tag "{ memento_wa->get_tag( ) }", created at { memento_wa->get_timestamp( ) }| TO states.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    IF lcl_object IS NOT BOUND.
      lcl_object = oref.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
