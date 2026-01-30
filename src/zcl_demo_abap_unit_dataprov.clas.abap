"! <p class="shorttext"><strong>Class Supporting ABAP Unit Test Example</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class represents a dependent-on-component (DOC) and supports an ABAP Unit test example.
"! Methods of this class are called in another class: {@link zcl_demo_abap_unit_tdf}. The DOCs are replaced
"! by test doubles when running ABAP Unit tests.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Information</h2>
"! <p>Find information on getting started with the example class and the disclaimer in
"! the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
CLASS zcl_demo_abap_unit_dataprov DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_discount RETURNING VALUE(discount) TYPE decfloat34.
    METHODS get_discount_value IMPORTING day_value             TYPE i
                                         time_value            TYPE i
                               RETURNING VALUE(discount_value) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_unit_dataprov IMPLEMENTATION.
  METHOD get_discount.
    "Getting the weekday
    "1) Monday, 2) Tuesday, 3) Wednesday, 4) Thursday, 5) Friday, 6) Saturday, 7) Sunday
    DATA(weekday) = ( 5 + CONV d( xco_cp=>sy->date( xco_cp_time=>time_zone->utc
      )->as( xco_cp_time=>format->iso_8601_basic )->value ) MOD 7 ) MOD 7 + 1.

    "- Standard discount is granted at the weekend (Saturday, Sunday)
    "- On other weekdays, discount is granted depending on the daytime
    IF weekday = 6 OR weekday = 7.
      discount = '20'.
    ELSE.
      "Retrieving the current time in UTC
      DATA(utc_time) = CONV t( xco_cp=>sy->time( xco_cp_time=>time_zone->utc
         )->as( xco_cp_time=>format->iso_8601_basic )->value ).

      discount = COND #(  WHEN utc_time BETWEEN '000000' AND '045959' THEN '15' "Night discount
                          WHEN utc_time BETWEEN '220000' AND '235959' THEN '15' "Night discount
                          WHEN utc_time BETWEEN '050000' AND '115959' THEN '10' "Morning discount
                          WHEN utc_time BETWEEN '180000' AND '215959' THEN '5'  "Evening discount
                          ELSE 0                                                "No discount
                       ).
    ENDIF.
  ENDMETHOD.

  METHOD get_discount_value.
    CASE day_value.
        "Standard discount is granted at the weekend (Saturday, Sunday)
      WHEN 6 OR 7.
        discount_value = '20'.
        "On other weekdays, discount is granted depending on the daytime
      WHEN OTHERS.
        discount_value = SWITCH #( time_value
                                   WHEN 1 THEN '15' "Night discount
                                   WHEN 2 THEN '10' "Morning discount
                                   WHEN 3 THEN '5'  "Evening discount
                                   ELSE '0'         "No discount
         ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
