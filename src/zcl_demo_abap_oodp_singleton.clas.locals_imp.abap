CLASS lcl_singleton DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_obj RETURNING VALUE(obj) TYPE REF TO lcl_singleton.

    METHODS add_log IMPORTING text TYPE string.
    METHODS get_log RETURNING VALUE(log) TYPE string_table.
    METHODS get_timestamps EXPORTING ts_static   TYPE utclong
                                     ts_instance TYPE utclong.

    CLASS-METHODS class_constructor.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA oref TYPE REF TO lcl_singleton.
    CLASS-DATA log_table TYPE string_table.
    CLASS-DATA timestamp_static TYPE utclong.
    DATA timestamp_instance TYPE utclong.
ENDCLASS.

CLASS lcl_singleton IMPLEMENTATION.
  METHOD get_obj.
    IF oref IS NOT BOUND.
      oref = NEW lcl_singleton( ).
    ENDIF.

    obj = oref.
  ENDMETHOD.

  METHOD add_log.
    INSERT text INTO TABLE log_table.
  ENDMETHOD.

  METHOD get_log.
    log = log_table.
  ENDMETHOD.

  METHOD get_timestamps.
    ts_static = timestamp_static.
    ts_instance = timestamp_instance.
  ENDMETHOD.

  METHOD class_constructor.
    timestamp_static = utclong_current( ).
  ENDMETHOD.

  METHOD constructor.
    timestamp_instance = utclong_current( ).
  ENDMETHOD.
ENDCLASS.
