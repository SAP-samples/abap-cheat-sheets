*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Interface for observers
*&---------------------------------------------------------------------*

INTERFACE lif_observer.
  METHODS: notify IMPORTING num1 TYPE i
                            num2 TYPE i
                            ts   TYPE utclong,
   calculate IMPORTING num1 TYPE i
                       num2 TYPE i
             RETURNING VALUE(result) TYPE string.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Class for observed object
*&---------------------------------------------------------------------*

CLASS lcl DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      set_numbers IMPORTING value1 TYPE i value2 TYPE i,
      register IMPORTING oref_observer TYPE REF TO lif_observer,
      unregister IMPORTING oref_observer TYPE REF TO lif_observer,
      notify_observers.

    "String table for display purposes
    CLASS-DATA tab4display TYPE string_table.
  PROTECTED SECTION.
    DATA: observer_tab TYPE TABLE OF REF TO lif_observer,
          num1         TYPE i,
          num2         TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD register.
    IF NOT line_exists( observer_tab[ table_line = oref_observer ] ).
      INSERT oref_observer INTO TABLE observer_tab.

      "Populating a string table for display purposes.
      APPEND |Object reference for { cl_abap_typedescr=>describe_by_object_ref( oref_observer )->get_relative_name( ) } inserted into observer table.| TO lcl=>tab4display.
    ENDIF.
  ENDMETHOD.
  METHOD unregister.
    DELETE TABLE observer_tab FROM oref_observer.

    "Populating a string table for display purposes.
    APPEND |Object reference for { cl_abap_typedescr=>describe_by_object_ref( oref_observer )->get_relative_name( ) } removed from observer table.| TO lcl=>tab4display.
  ENDMETHOD.

  METHOD notify_observers.
    LOOP AT observer_tab ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>->notify( num1 = me->num1 num2 = me->num2 ts = utclong_current( ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_numbers.
    num1 = value1.
    num2 = value2.

    "Populating a string table for display purposes.
    APPEND |The number values { num1 } and { num2 } were set in the observed object. Observers are about to be notified.| TO lcl=>tab4display.
    APPEND INITIAL LINE TO lcl=>tab4display.
    notify_observers( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Classes for observers
*&---------------------------------------------------------------------*

CLASS lcl_addition DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_observer.
ENDCLASS.

CLASS lcl_addition IMPLEMENTATION.
  METHOD lif_observer~notify.
    "Populating a string table for display purposes.
    APPEND |lcl_addition: Observer notified at { ts }.| TO lcl=>tab4display.

    lif_observer~calculate( num1 = num1 num2 = num2 ).
  ENDMETHOD.

  METHOD lif_observer~calculate.
    TRY.
        result = |{ num1 + num2 STYLE = SIMPLE }|.
      CATCH cx_sy_arithmetic_error INTO DATA(error).
        result = error->get_text( ).
    ENDTRY.

    "Populating a string table for display purposes.
    APPEND `lcl_addition: lif_observer~calculate called.` TO lcl=>tab4display.
    APPEND |{ num1 } + { num2 } = { result }| TO lcl=>tab4display.
    APPEND INITIAL LINE TO lcl=>tab4display.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_subtraction DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_observer.
ENDCLASS.

CLASS lcl_subtraction IMPLEMENTATION.
  METHOD lif_observer~notify.
    "Populating a string table for display purposes.
    APPEND |lcl_subtraction: Observer notified at { ts }.| TO lcl=>tab4display.

    lif_observer~calculate( num1 = num1 num2 = num2 ).
  ENDMETHOD.

  METHOD lif_observer~calculate.
    TRY.
        result = |{ num1 - num2 STYLE = SIMPLE }|.
      CATCH cx_sy_arithmetic_error INTO DATA(error).
        result = error->get_text( ).
    ENDTRY.

    "Populating a string table for display purposes.
    APPEND `lcl_subtraction: lif_observer~calculate called.` TO lcl=>tab4display.
    APPEND |{ num1 } - { num2 } = { result }| TO lcl=>tab4display.
    APPEND INITIAL LINE TO lcl=>tab4display.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_multiplication DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_observer.
ENDCLASS.

CLASS lcl_multiplication IMPLEMENTATION.
  METHOD lif_observer~notify.
    "Populating a string table for display purposes.
    APPEND |lcl_multiplication: Observer notified at { ts }.| TO lcl=>tab4display.

    lif_observer~calculate( num1 = num1 num2 = num2 ).
  ENDMETHOD.

  METHOD lif_observer~calculate.
    TRY.
        result = |{ num1 * num2 STYLE = SIMPLE }|.
      CATCH cx_sy_arithmetic_error INTO DATA(error).
        result = error->get_text( ).
    ENDTRY.

    "Populating a string table for display purposes.
    APPEND `lcl_multiplication: lif_observer~calculate called.` TO lcl=>tab4display.
    APPEND |{ num1 } * { num2 } = { result }| TO lcl=>tab4display.
    APPEND INITIAL LINE TO lcl=>tab4display.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_division DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_observer.
ENDCLASS.

CLASS lcl_division IMPLEMENTATION.
  METHOD lif_observer~notify.
    "Populating a string table for display purposes.
    APPEND |lcl_division: Observer notified at { ts }.| TO lcl=>tab4display.

    lif_observer~calculate( num1 = num1 num2 = num2 ).
  ENDMETHOD.

  METHOD lif_observer~calculate.
    TRY.
        result = |{ CONV decfloat34( num1 / num2 ) STYLE = SIMPLE }|.
      CATCH cx_sy_arithmetic_error INTO DATA(error).
        result = error->get_text( ).
    ENDTRY.

    "Populating a string table for display purposes.
    APPEND `lcl_division: lif_observer~calculate called.` TO lcl=>tab4display.
    APPEND |{ num1 } / { num2 } = { result }| TO lcl=>tab4display.
    APPEND INITIAL LINE TO lcl=>tab4display.
  ENDMETHOD.

ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

CLASS lcl_evt DEFINITION.
  PUBLIC SECTION.
    METHODS set_text IMPORTING txt TYPE string.
    EVENTS evt EXPORTING VALUE(info) TYPE string.

    CLASS-DATA tab4display TYPE string_table.
  PROTECTED SECTION.
    DATA text TYPE string.
ENDCLASS.

CLASS lcl_evt IMPLEMENTATION.
  METHOD set_text.
    text = txt.

    "Populating a string table for display purposes.
    APPEND |Object changed. "text" was assigned the value "{ text }". Event is about to be raised.| TO lcl_evt=>tab4display.
    APPEND INITIAL LINE TO lcl_evt=>tab4display.

    RAISE EVENT evt EXPORTING info = txt.
  ENDMETHOD.
ENDCLASS.

INTERFACE lif_obs.
  METHODS:
    add_text IMPORTING txt TYPE string,
    handle_event FOR EVENT evt OF lcl_evt IMPORTING info.
  DATA some_text TYPE string.
ENDINTERFACE.

CLASS lcl_obs_1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_obs.
ENDCLASS.

CLASS lcl_obs_1 IMPLEMENTATION.

  METHOD lif_obs~handle_event.
    "Populating a string table for display purposes.
    APPEND `lcl_obs_1: Event handled in lif_obs~handle_event.` TO lcl_evt=>tab4display.
    APPEND |Text "{ info }" is about to be processed.| TO lcl_evt=>tab4display.

    lif_obs~add_text( info ).
  ENDMETHOD.

  METHOD lif_obs~add_text.
    lif_obs~some_text = |{ txt }AP|.

    "Populating a string table for display purposes.
    APPEND |Text was processed in lif_obs~add_text. Result: "{ lif_obs~some_text }"| TO lcl_evt=>tab4display.
    APPEND INITIAL LINE TO lcl_evt=>tab4display.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_obs_2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_obs.
ENDCLASS.

CLASS lcl_obs_2 IMPLEMENTATION.

  METHOD lif_obs~handle_event.
    "Populating a string table for display purposes.
    APPEND `lcl_obs_2: Event handled in lif_obs~handle_event.` TO lcl_evt=>tab4display.
    APPEND |Text "{ info }" is about to be processed.| TO lcl_evt=>tab4display.

    lif_obs~add_text( info ).
  ENDMETHOD.

  METHOD lif_obs~add_text.
    lif_obs~some_text = |{ txt }CDEFGHIJKLMNOPQRSTUVWXYZ|.

    "Populating a string table for display purposes.
    APPEND |Text was processed in lif_obs~add_text. Result: "{ lif_obs~some_text }"| TO lcl_evt=>tab4display.
    APPEND INITIAL LINE TO lcl_evt=>tab4display.
  ENDMETHOD.
ENDCLASS.
