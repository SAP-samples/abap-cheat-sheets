*&---------------------------------------------------------------------*
*& Interface
*&---------------------------------------------------------------------*

INTERFACE lif_dao.

  TYPES: ts_carr TYPE zdemoabapcarrier,
         tt_carr TYPE TABLE OF zdemoabapcarrier WITH EMPTY KEY,
         BEGIN OF ENUM enum_op,
           update,
           insert,
         END OF ENUM enum_op.

  METHODS count_entries
    RETURNING VALUE(count) TYPE i.

  METHODS does_entry_exist
    IMPORTING carrid        TYPE ts_carr-carrid
    RETURNING VALUE(exists) TYPE abap_boolean.

  METHODS get_entry
    IMPORTING carrid       TYPE ts_carr-carrid
    RETURNING VALUE(entry) TYPE ts_carr.

  METHODS get_all_entries
    RETURNING VALUE(entries) TYPE tt_carr.

  METHODS create_entry
    IMPORTING entry       TYPE ts_carr
    RETURNING VALUE(done) TYPE abap_boolean.

  METHODS create_entries
    IMPORTING entries     TYPE tt_carr
    RETURNING VALUE(done) TYPE abap_boolean.

  METHODS upsert_entry
    IMPORTING entry            TYPE ts_carr
    RETURNING VALUE(operation) TYPE enum_op.

  METHODS delete_entry
    IMPORTING carrid      TYPE ts_carr-carrid
    RETURNING VALUE(done) TYPE abap_boolean.

ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Concrete class
*&---------------------------------------------------------------------*

CLASS lcl_dao DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES lif_dao.
    CLASS-METHODS get_dao RETURNING VALUE(dao) TYPE REF TO lif_dao.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA oref TYPE REF TO lif_dao.
ENDCLASS.

CLASS lcl_dao IMPLEMENTATION.

  METHOD lif_dao~count_entries.
    SELECT COUNT( * ) FROM zdemoabapcarrier INTO @count.
  ENDMETHOD.

  METHOD lif_dao~create_entries.
    INSERT zdemoabapcarrier FROM TABLE @entries ACCEPTING DUPLICATE KEYS.

    IF sy-subrc = 0.
      done = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD lif_dao~create_entry.
    INSERT zdemoabapcarrier FROM @entry.

    IF sy-subrc = 0.
      done = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD lif_dao~delete_entry.
    DELETE FROM zdemoabapcarrier WHERE carrid = @carrid.

    IF sy-subrc = 0.
      done = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD lif_dao~does_entry_exist.
    SELECT SINGLE @abap_true FROM zdemoabapcarrier WHERE carrid = @carrid INTO @exists.
  ENDMETHOD.

  METHOD lif_dao~get_all_entries.
    SELECT * FROM zdemoabapcarrier ORDER BY carrid INTO TABLE @entries.
  ENDMETHOD.

  METHOD lif_dao~get_entry.
    SELECT SINGLE * FROM zdemoabapcarrier WHERE carrid = @carrid INTO @entry.
  ENDMETHOD.

  METHOD lif_dao~upsert_entry.
    DATA(exists) = lif_dao~does_entry_exist( entry-carrid ).

    IF exists = abap_true.
      UPDATE zdemoabapcarrier FROM @entry.
      operation = lif_dao=>update.
    ELSE.
      INSERT zdemoabapcarrier FROM @entry.
      operation = lif_dao=>insert.
    ENDIF.
  ENDMETHOD.

  METHOD get_dao.
    IF oref IS NOT BOUND.
      oref = NEW lcl_dao( ).
    ENDIF.

    dao = oref.
  ENDMETHOD.
ENDCLASS.
