INTERFACE lif_hello.
  TYPES: basetype TYPE i,
         BEGIN OF ENUM enum_langu BASE TYPE basetype,
           init VALUE IS INITIAL,
           en   VALUE 1,
           fr   VALUE 2,
           it   VALUE 3,
           es   VALUE 4,
           de   VALUE 5,
         END OF ENUM enum_langu.

  METHODS say_hello RETURNING VALUE(hi) TYPE string.
ENDINTERFACE.

CLASS lcl_en DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_hello.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_en IMPLEMENTATION.
  METHOD lif_hello~say_hello.
    hi = `Hi`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_fr DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_hello.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_fr IMPLEMENTATION.
  METHOD lif_hello~say_hello.
    hi = `Salut`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_it DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_hello.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_it IMPLEMENTATION.
  METHOD lif_hello~say_hello.
    hi = `Ciao`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_es DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_hello.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_es IMPLEMENTATION.
  METHOD lif_hello~say_hello.
    hi = `Hola`.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Class that does not implement the lif_hello interface
"The assumption is that functionality of the API is reused and integrated
"into the exsisting API. The non-compatible type is converted using an
"adapter class.

INTERFACE lif_hello_as_xstring.
  METHODS xstring_hello RETURNING VALUE(hi) TYPE xstring.
ENDINTERFACE.

CLASS lcl_de_xstring DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_hello_as_xstring.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_de_xstring IMPLEMENTATION.
  METHOD lif_hello_as_xstring~xstring_hello.
    hi = CONV xstring( `48616C6C6F` ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Adapter class

CLASS lcl_de_adapter DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_hello.
ENDCLASS.

CLASS lcl_de_adapter IMPLEMENTATION.
  METHOD lif_hello~say_hello.
    DATA(oref) = NEW lcl_de_xstring( ).
    DATA(hello_as_xstring) = oref->lif_hello_as_xstring~xstring_hello( ).
    hi = cl_abap_conv_codepage=>create_in( )->convert( hello_as_xstring ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Class containing a factory method

CLASS lcl_hello_factory DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS create_hello IMPORTING language     TYPE lif_hello=>enum_langu
                               RETURNING VALUE(hello) TYPE REF TO lif_hello.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_hello_factory IMPLEMENTATION.
  METHOD create_hello.
    hello = SWITCH #( language
                      WHEN lif_hello=>en THEN NEW lcl_en( )
                      WHEN lif_hello=>fr THEN NEW lcl_fr( )
                      WHEN lif_hello=>it THEN NEW lcl_it( )
                      WHEN lif_hello=>es THEN NEW lcl_es( )
                      "Calling the method in the adapter class
                      WHEN lif_hello=>de THEN NEW lcl_de_adapter( )
                      "E.g. raising an exception or returning a default object
                      ELSE NEW lcl_en( ) ).
  ENDMETHOD.
ENDCLASS.
