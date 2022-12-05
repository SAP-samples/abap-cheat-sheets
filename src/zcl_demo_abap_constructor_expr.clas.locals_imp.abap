CLASS local_class DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING txt TYPE string,
      double IMPORTING int TYPE
          REF TO i RETURNING VALUE(res) TYPE i.

    DATA: timestamp TYPE string,
          text      TYPE string.

    CLASS-DATA: no_of_instances TYPE i READ-ONLY.
ENDCLASS.

CLASS local_class IMPLEMENTATION.
  METHOD constructor.
    "Number of instances of the class are counted.
    no_of_instances = no_of_instances + 1.
    "Set a time stamp.
    DATA: ts TYPE timestampl.
    GET TIME STAMP FIELD ts.
    timestamp = |{ ts TIMESTAMP = SPACE }|.

    text = |{ txt }, { sy-uname }.|.
  ENDMETHOD.
  METHOD double.
    res = int->* * 2.
  ENDMETHOD.
ENDCLASS.
