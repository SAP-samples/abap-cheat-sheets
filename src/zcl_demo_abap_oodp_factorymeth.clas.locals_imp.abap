INTERFACE lif_factory.
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
    INTERFACES lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_en IMPLEMENTATION.
  METHOD lif_factory~say_hello.
    hi = `Hi`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_fr DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_fr IMPLEMENTATION.
  METHOD lif_factory~say_hello.
    hi = `Salut`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_it DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_it IMPLEMENTATION.
  METHOD lif_factory~say_hello.
    hi = `Ciao`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_es DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_es IMPLEMENTATION.
  METHOD lif_factory~say_hello.
    hi = `Hola`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_de DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_de IMPLEMENTATION.
  METHOD lif_factory~say_hello.
    hi = `Hallo`.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_factory_cl DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS create_hello IMPORTING language     TYPE lif_factory=>enum_langu
                               RETURNING VALUE(hello) TYPE REF TO lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_factory_cl IMPLEMENTATION.
  METHOD create_hello.
    hello = SWITCH #( language
                      WHEN lif_factory=>en THEN NEW lcl_en( )
                      WHEN lif_factory=>fr THEN NEW lcl_fr( )
                      WHEN lif_factory=>it THEN NEW lcl_it( )
                      WHEN lif_factory=>es THEN NEW lcl_es( )
                      WHEN lif_factory=>de THEN NEW lcl_de( )
                      "E.g. raising an exception or returning a default object
                      ELSE NEW lcl_en( ) ).
  ENDMETHOD.
ENDCLASS.
