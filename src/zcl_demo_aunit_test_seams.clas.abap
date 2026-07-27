"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class demonstrates ABAP Unit tests using ABAP test seams. It shows how TEST-SEAM and TEST-INJECTION statements can be
"! used for code isolation. The class includes a method for calculating occupancy rates with a seam around database selection logic.
"! Additionally, it features a demo method that illustrates how injected test code can function.
CLASS zcl_demo_aunit_test_seams DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    "! Calculates the occupancy rate based on the carrier's flight data
    "!
    "! @parameter carrier_id     | <p class="shorttext synchronized" lang="en">ID of the carrier to calculate occupancy for</p>
    "! @parameter occupancy_rate | <p class="shorttext synchronized" lang="en">Computed occupancy rate of the carrier flights</p>
    METHODS calculate_occupancy_rate IMPORTING carrier_id            TYPE ztaunitflights-carrid
                                     RETURNING VALUE(occupancy_rate) TYPE decfloat34.

    "! <p class="shorttext synchronized" lang="en">Tests behavior using injected test code</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en">Stores a demo string</p>
    METHODS test_seams_demo RETURNING VALUE(result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_test_seams IMPLEMENTATION.
  METHOD calculate_occupancy_rate.
    TEST-SEAM select_from_db.
      SELECT seatsmax, seatsocc
        FROM ztaunitflights
        WHERE carrid = @carrier_id
        INTO TABLE @DATA(flight_data).
    END-TEST-SEAM.

    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.

    LOOP AT flight_data ASSIGNING FIELD-SYMBOL(<flight>).
      total_seatsmax += <flight>-seatsmax.
      total_seatsocc += <flight>-seatsocc.
    ENDLOOP.

    IF total_seatsmax <> 0.
      occupancy_rate = round( val = total_seatsocc / total_seatsmax * 100 dec = 2 ).
    ENDIF.
  ENDMETHOD.

  METHOD test_seams_demo.

    DATA(num) = 0.

    "Empty test seam; code is injected during unit test
    "Check the output when running the class using F9 and
    "the test results when running the unit test.
    TEST-SEAM ts1.
    END-TEST-SEAM.

    IF num = 0.
      result &&= `A`.
    ELSE.
      result &&= `B`.
    ENDIF.

    DATA str TYPE string.
    str = `C`.

    "Empty injection
    "See the test class: The code that is included in the test
    "seam should be excluded from the test. Therefore, the
    "test injection block in the test class is empty.
    TEST-SEAM ts2.
      str = `D`.
    END-TEST-SEAM.

    result &&= str.

  ENDMETHOD.

ENDCLASS.
