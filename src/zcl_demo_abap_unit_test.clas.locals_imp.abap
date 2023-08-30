******************************************************
* Local interface
******************************************************
INTERFACE lif_get_data.

  TYPES: carr_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY,
         occ_rate TYPE p LENGTH 4 DECIMALS 2.

  METHODS:
    select_flight_data IMPORTING carrier            TYPE zdemo_abap_fli-carrid
                       RETURNING VALUE(flight_data) TYPE carr_tab,

    "This method is included to demonstrate the PARTIALLY IMPLEMENTED
    "addition in the test class when implementing the test double
    say_hello RETURNING VALUE(hi) TYPE string.

ENDINTERFACE.

******************************************************
* Local class
* The class implements the local interface.
******************************************************
CLASS lcl_data_prov_local_itf DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_get_data.
ENDCLASS.

CLASS lcl_data_prov_local_itf IMPLEMENTATION.

  METHOD lif_get_data~select_flight_data.

    SELECT seatsmax, seatsocc
      FROM zdemo_abap_fli
      WHERE carrid = @carrier
      INTO CORRESPONDING FIELDS OF TABLE @flight_data.

  ENDMETHOD.

  METHOD lif_get_data~say_hello.
    hi = `Hello, ` && sy-uname && `.`.
  ENDMETHOD.

ENDCLASS.

******************************************************
* Local class that implements a global interface
* It serves the purpose of a data provider. It can be
* imagined as a global class, and a method that is
* implemented there and called by the class under test
* is identified as DOC. A local class is used in the
* example to keep the number of separate artifacts small.
******************************************************
CLASS lcl_data_prov_glo_itf DEFINITION.
  PUBLIC SECTION.
    INTERFACES zdemo_abap_get_data_itf.
ENDCLASS.

CLASS lcl_data_prov_glo_itf IMPLEMENTATION.

  METHOD zdemo_abap_get_data_itf~select_flight_data.

    SELECT seatsmax, seatsocc
      FROM zdemo_abap_fli
      WHERE carrid = @carrier
      INTO CORRESPONDING FIELDS OF TABLE @flight_data.

  ENDMETHOD.

  METHOD zdemo_abap_get_data_itf~say_hello.
    hi = `Hello, ` && sy-uname && `.`.
  ENDMETHOD.

ENDCLASS.
