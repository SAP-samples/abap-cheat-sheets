"Class providing a string table representing a log table for
"display purposes in the example
CLASS lcl_log DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA log TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Base class of the implementation layer
*&---------------------------------------------------------------------*

CLASS lcl_car_factory DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS build ABSTRACT IMPORTING cartype TYPE string.
    METHODS paint ABSTRACT IMPORTING cartype TYPE string.
    METHODS control_quality ABSTRACT IMPORTING cartype TYPE string.
    METHODS test_drive ABSTRACT IMPORTING cartype TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_car_factory IMPLEMENTATION.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete implementations
*&---------------------------------------------------------------------*

CLASS lcl_factory_a DEFINITION INHERITING FROM lcl_car_factory.
  PUBLIC SECTION.
    METHODS build REDEFINITION.
    METHODS paint REDEFINITION.
    METHODS control_quality REDEFINITION.
    METHODS test_drive REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_factory_a IMPLEMENTATION.
  METHOD build.
    APPEND |Factory A: { cartype } built| TO lcl_log=>log.
  ENDMETHOD.

  METHOD paint.
    APPEND |Factory A: { cartype } painted| TO lcl_log=>log.
  ENDMETHOD.

  METHOD control_quality.
    APPEND |Factory A: { cartype } checked| TO lcl_log=>log.
  ENDMETHOD.

  METHOD test_drive.
    APPEND |Factory A: { cartype } test driven| TO lcl_log=>log.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_factory_b DEFINITION INHERITING FROM lcl_car_factory.
  PUBLIC SECTION.
    METHODS build REDEFINITION.
    METHODS paint REDEFINITION.
    METHODS control_quality REDEFINITION.
    METHODS test_drive REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_factory_b IMPLEMENTATION.
  METHOD build.
    APPEND |Factory B: { cartype } built| TO lcl_log=>log.
  ENDMETHOD.

  METHOD paint.
    APPEND |Factory B: { cartype } painted| TO lcl_log=>log.
  ENDMETHOD.

  METHOD control_quality.
    APPEND |Factory B: { cartype } checked| TO lcl_log=>log.
  ENDMETHOD.

  METHOD test_drive.
    APPEND |Factory B: { cartype } test driven| TO lcl_log=>log.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_factory_c DEFINITION INHERITING FROM lcl_car_factory.
  PUBLIC SECTION.
    METHODS build REDEFINITION.
    METHODS paint REDEFINITION.
    METHODS control_quality REDEFINITION.
    METHODS test_drive REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_factory_c IMPLEMENTATION.
  METHOD build.
    APPEND |Factory C: { cartype } built| TO lcl_log=>log.
  ENDMETHOD.

  METHOD paint.
    APPEND |Factory C: { cartype } painted| TO lcl_log=>log.
  ENDMETHOD.

  METHOD control_quality.
    APPEND |Factory C: { cartype } checked| TO lcl_log=>log.
  ENDMETHOD.

  METHOD test_drive.
    APPEND |Factory C: { cartype } test driven| TO lcl_log=>log.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Base class of the abstraction layer
*&---------------------------------------------------------------------*

CLASS lcl_car DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS constructor IMPORTING oref_factory TYPE REF TO lcl_car_factory.
    METHODS produce RETURNING VALUE(log) TYPE string_table.
  PROTECTED SECTION.
    DATA factory TYPE REF TO lcl_car_factory.
    DATA cartype TYPE string.
    METHODS car_type ABSTRACT RETURNING VALUE(cartype) TYPE string.
    METHODS paint.
    METHODS control_quality.
    METHODS test_drive.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_car IMPLEMENTATION.
  METHOD constructor.
    factory = oref_factory.
  ENDMETHOD.

  METHOD produce.
    cartype = car_type( ).

    APPEND |{ repeat( val = `-` occ = 80 ) }| TO lcl_log=>log.
    DATA(relative_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( factory ) )->get_relative_name( ).
    DATA(factory_name) = to_upper( match( val = relative_name pcre = `.\Z` ) ).
    APPEND |Starting production of { cartype } using factory { factory_name } (class { relative_name })| TO lcl_log=>log.

    factory->build( cartype ).
    paint( ).
    control_quality( ).
    test_drive( ).

    APPEND |Factory { factory_name }: { cartype } produced| TO lcl_log=>log.

    "Handling the log table for display purposes
    log = lcl_log=>log.
    CLEAR lcl_log=>log.
  ENDMETHOD.

  METHOD paint.
    factory->paint( cartype ).
  ENDMETHOD.

  METHOD control_quality.
    factory->control_quality( cartype ).
  ENDMETHOD.

  METHOD test_drive.
    factory->test_drive( cartype ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Refined abstractions
*&---------------------------------------------------------------------*

"test_drive, control_quality not refined
CLASS lcl_compact_car DEFINITION INHERITING FROM lcl_car.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS car_type REDEFINITION.
    METHODS paint REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_compact_car IMPLEMENTATION.
  METHOD car_type.
    cartype = `compact car`.
  ENDMETHOD.

  METHOD paint.
    APPEND |Compact-car-specific preparation step for painting| TO lcl_log=>log.
    super->paint( ).
  ENDMETHOD.
ENDCLASS.

"test_drive not refined
CLASS lcl_suv DEFINITION INHERITING FROM lcl_car.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS car_type REDEFINITION.
    METHODS paint REDEFINITION.
    METHODS control_quality REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_suv IMPLEMENTATION.
  METHOD car_type.
    cartype = `SUV`.
  ENDMETHOD.

  METHOD paint.
    APPEND |SUV-specific preparation step for painting| TO lcl_log=>log.
    super->paint( ).
  ENDMETHOD.

  METHOD control_quality.
    APPEND |SUV-specific preparation step for quality control| TO lcl_log=>log.
    super->control_quality( ).
  ENDMETHOD.
ENDCLASS.

"paint not refined
CLASS lcl_sports_car DEFINITION INHERITING FROM lcl_car.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS car_type REDEFINITION.
    METHODS test_drive REDEFINITION.
    METHODS control_quality REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_sports_car IMPLEMENTATION.
  METHOD car_type.
    cartype = `sports car`.
  ENDMETHOD.

  METHOD test_drive.
    APPEND |Sports-car-specific preparation step for test drive| TO lcl_log=>log.
    super->test_drive( ).
  ENDMETHOD.

  METHOD control_quality.
    APPEND |Sports-car-specific preparation step for quality control| TO lcl_log=>log.
    super->control_quality( ).
  ENDMETHOD.
ENDCLASS.
