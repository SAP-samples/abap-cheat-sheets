CLASS local_class DEFINITION FRIENDS zcl_demo_abap_objects_misc.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA hello TYPE zcl_demo_abap_objects_misc=>str4friend.
    CLASS-METHODS say_hello.

ENDCLASS.

CLASS local_class IMPLEMENTATION.
  METHOD say_hello.
    hello = |{ zcl_demo_abap_objects_misc=>get_hello( ) } { sy-uname }.|.
  ENDMETHOD.
ENDCLASS.
