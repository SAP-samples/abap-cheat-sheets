 "Note: This ABAP stopwatch is not intended to be used productively.
 "The implementation is intended to be a playground for exploring
 "time-related functions.
 CLASS lcl_stopwatch DEFINITION.
  PUBLIC SECTION.
 CLASS-METHODS create
      RETURNING
        VALUE(sw_inst) TYPE REF TO lcl_stopwatch.

    METHODS stop.
    METHODS start.
    METHODS store.

    TYPES: BEGIN OF stored_times_struc,
             interval TYPE i,
             time_since_start TYPE string,
             time_delta  TYPE string,
           END OF stored_times_struc.
    TYPES stored_times_type TYPE SORTED TABLE OF stored_times_struc WITH UNIQUE KEY interval.
    METHODS get_stored_times RETURNING VALUE(time) TYPE stored_times_type .
    METHODS get_time RETURNING VALUE(time) TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA time_start TYPE utclong.
    DATA time_stop TYPE utclong.
    DATA stopwatch_is_on TYPE abap_bool.
    DATA interval_timer_is_on TYPE abap_bool.
    TYPES: BEGIN OF time_struc,
             days    TYPE i,
             hours   TYPE i,
             minutes TYPE int8,
             seconds TYPE decfloat34,
           END OF time_struc.
    DATA time_elapsed TYPE time_struc.
    TYPES cl2 TYPE c LENGTH 2.
    DATA storage TYPE TABLE OF utclong WITH EMPTY KEY.

    METHODS reset.
    METHODS get_elapsed_time
      IMPORTING start        TYPE utclong
                stop         TYPE utclong
      RETURNING VALUE(delta) TYPE time_struc.

    METHODS time_output_prep
      IMPORTING time       TYPE time_struc
      RETURNING VALUE(res) TYPE string.
ENDCLASS.

CLASS lcl_stopwatch IMPLEMENTATION.
  METHOD create.
    sw_inst = NEW #( ).
    sw_inst->reset( ).
  ENDMETHOD.

  METHOD start.
    "No multiple starts allowed
    ASSERT stopwatch_is_on = abap_false.
    ASSERT interval_timer_is_on = abap_false.
    reset( ).
    stopwatch_is_on = abap_true.
    time_start = utclong_current( ).
  ENDMETHOD.

  METHOD stop.
    ASSERT stopwatch_is_on = abap_true.
    time_stop = utclong_current( ).
    stopwatch_is_on = abap_false.
    interval_timer_is_on = abap_false.
    time_elapsed = get_elapsed_time( start = time_start
                                     stop = time_stop ).
  ENDMETHOD.

  METHOD reset.
    CLEAR: time_start,
           time_stop,
           time_elapsed,
           stopwatch_is_on,
           interval_timer_is_on,
           storage.
  ENDMETHOD.

  METHOD get_elapsed_time.
    cl_abap_utclong=>diff( EXPORTING high     = stop
                                     low      = start
                            IMPORTING days    = delta-days
                                      hours   = delta-hours
                                      minutes = delta-minutes
                                      seconds = delta-seconds ).
  ENDMETHOD.

  METHOD get_time.
    "When the interval timer is on, it is not allowed to
    "get the time. The get_stored_times method is to be used.
    ASSERT interval_timer_is_on = abap_false.
    IF stopwatch_is_on = abap_true.
      stop( ).
    ENDIF.
    time = time_output_prep( time_elapsed ).
  ENDMETHOD.

  METHOD time_output_prep.
    res = |{ COND #( WHEN time-days IS NOT INITIAL THEN time-days && ` days, ` ) }| &&
          |{ COND #( WHEN numofchar( CONV cl2( time-hours ) ) = 1 THEN `0` && time-hours ELSE time-hours ) }:| &&
          |{ COND #( WHEN numofchar( CONV cl2( time-minutes ) ) = 1 THEN `0` && time-minutes ELSE time-minutes ) }:| &&
          |{ COND #( WHEN find( val = CONV string( time-seconds ) sub = `.` ) = 1 THEN `0` && time-seconds ELSE time-seconds ) }|.
  ENDMETHOD.

  METHOD store.
    ASSERT stopwatch_is_on = abap_true.
    interval_timer_is_on = abap_true.
    APPEND utclong_current( ) TO storage.
  ENDMETHOD.

  METHOD get_stored_times.
    IF interval_timer_is_on = abap_true.
      stop( ).
    ENDIF.

    if lines( storage ) > 0.
    LOOP AT storage REFERENCE INTO DATA(ref).
      DATA(tabix) = sy-tabix.
      DATA(time_since_start) = get_elapsed_time( start = time_start stop = ref->* ).
      DATA(prep_time_since_start) = time_output_prep( time_since_start ).
      DATA(time_delta) = get_elapsed_time( start = COND #( WHEN tabix = 1 THEN time_start ELSE storage[ tabix - 1 ] ) stop = ref->* ).
      DATA(prep_time_delta) = time_output_prep( time_delta ).
      INSERT VALUE #( interval = tabix
                      time_since_start = prep_time_since_start
                      time_delta = prep_time_delta ) INTO TABLE time.
    ENDLOOP.
    endif.
  ENDMETHOD.
ENDCLASS.
