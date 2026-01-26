CLASS lcl_multiton DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "ABAP enumerated type serving as identifiers for unique
    "instances
    TYPES: BEGIN OF ENUM t_enum,
             config_a,
             config_b,
             config_c,
             config_e,
             config_f,
             config_g,
             config_h,
             config_i,
             config_j,
             config_k,
             config_l,
             config_m,
           END OF ENUM t_enum.

    "Factory method that returns distinct instances based on given key
    CLASS-METHODS get_instance
      IMPORTING key             TYPE t_enum
      RETURNING VALUE(instance) TYPE REF TO lcl_multiton.

    "Dummy methods
    METHODS do_something.
    METHODS get_config_change_tab RETURNING VALUE(tab) TYPE string_table.

    "Table type and internal table used to store values for display purposes
    TYPES: BEGIN OF ty_log_struc,
             ikey      TYPE t_enum,
             first_instantiation TYPE utclong,
             instance_request_count type i,
           END OF ty_log_struc.

    CLASS-DATA log TYPE SORTED TABLE OF ty_log_struc WITH UNIQUE KEY ikey.

  PRIVATE SECTION.

    "Internal table containing the unique instances that are mapped to keys
    TYPES: BEGIN OF ty_instance_struc,
             ikey     TYPE t_enum,
             instance TYPE REF TO lcl_multiton,
           END OF ty_instance_struc,
           ty_instance_tab TYPE HASHED TABLE OF ty_instance_struc WITH UNIQUE KEY ikey.

    CLASS-DATA instance_table TYPE ty_instance_tab.

    METHODS constructor IMPORTING key TYPE t_enum.

    DATA config_change TYPE string_table.
ENDCLASS.

CLASS lcl_multiton IMPLEMENTATION.

  METHOD get_instance.
    "Factory method the returns an distinct instance out of a fixed set of
    "instance based on a give key. The implementation returns that instance
    "when the corresponding key is provided. If it is not yet stored in the
    "internal table, a new instance is created, added to the table, and returned.

    READ TABLE instance_table INTO DATA(ref) WITH KEY ikey = key.
    IF ref-instance IS NOT BOUND.
      ref-instance = NEW #( key ).
      INSERT VALUE #( ikey = key instance = ref-instance ) INTO TABLE instance_table.
    ENDIF.
    instance = ref-instance.

    "Modifying an entry in an internal table to log the number of instance retrieval requests
    "for display purposes.
    log[ ikey = key ]-instance_request_count += 1.
  ENDMETHOD.

  METHOD constructor.
    DATA(timestamp) = utclong_current( ).

    "Adding entries to an internal table for display purposes.
    "The example simply adds text to a string table, including timestamp values,
    "to illustrate different values when calling the method.
    APPEND key TO config_change.
    APPEND |constructor called at { timestamp }| TO config_change.

    IF NOT line_exists( log[ ikey = key ] ).
      INSERT VALUE #( ikey = key
                      first_instantiation = timestamp ) INTO TABLE log.

      APPEND |Instance for key { key } first created at { timestamp }| TO config_change.
    ENDIF.

    "Assumption: Complex or data-intensive tasks are executed to create an initial state
    "of instances.
    CASE key.
      WHEN config_a.
        ...
      WHEN config_b.
        ...
      WHEN config_c.
        ...

        ...
    ENDCASE.

  ENDMETHOD.

  METHOD do_something.
    APPEND |do_something called at { utclong_current( ) }| TO config_change.
  ENDMETHOD.

  METHOD get_config_change_tab.
    APPEND |get_config_change_tab called at { utclong_current( ) }| TO config_change.

    tab = config_change.
  ENDMETHOD.

ENDCLASS.
