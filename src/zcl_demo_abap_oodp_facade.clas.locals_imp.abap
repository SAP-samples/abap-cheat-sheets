"Class that includes a method to get flights from a data source
"In this simple, self-contained example, the data source is
"simulated by an internal table that includes demo data.
CLASS lcl_flight_retrieval DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF flight_struc,
             from        TYPE c LENGTH 20,
             to          TYPE c LENGTH 20,
             flight_date TYPE d,
             flight_time TYPE t,
           END OF flight_struc,
           flight_tab_type TYPE TABLE OF flight_struc WITH EMPTY KEY.

    METHODS get_flights
      IMPORTING
        from           TYPE string
        to             TYPE string
        flight_date    TYPE d
      RETURNING
        VALUE(flights) TYPE flight_tab_type.
    CLASS-METHODS class_constructor.
  PRIVATE SECTION.
    CLASS-DATA flight_tab TYPE flight_tab_type.
ENDCLASS.

CLASS lcl_flight_retrieval IMPLEMENTATION.
  METHOD get_flights.
    SELECT * FROM @flight_tab AS tab
      WHERE from = @from AND to = @to AND flight_date = @flight_date
      INTO TABLE @flights.
  ENDMETHOD.
  METHOD class_constructor.
    flight_tab = VALUE #(
     ( from = 'Frankfurt' to = 'Shanghai' flight_date = '20250511' flight_time = '050000' )
     ( from = 'Frankfurt' to = 'Shanghai' flight_date = '20250511' flight_time = '200000' )
     ( from = 'Frankfurt' to = 'Shanghai' flight_date = '20250605' flight_time = '151500' )
     ( from = 'Frankfurt' to = 'Shanghai' flight_date = '20250725' flight_time = '070000' )
     ( from = 'Shanghai' to = 'Frankfurt' flight_date = '20250410' flight_time = '194500' )
     ( from = 'Shanghai' to = 'Frankfurt' flight_date = '20250521' flight_time = '123000' )
     ( from = 'Shanghai' to = 'Frankfurt' flight_date = '20250805' flight_time = '184500' ) ).
  ENDMETHOD.
ENDCLASS.

"An object of the lcl_flight_search class is required for searching flights. This object is
"created by the facade class. The lcl_flight_search class implementation includes the calling
"of a method of the lcl_flight_retrieval class to retrieve a list of available flights.
CLASS lcl_flight_search DEFINITION.
  PUBLIC SECTION.
    METHODS search_flights
      IMPORTING
        from               TYPE string
        to                 TYPE string
        arrival            TYPE d
        departure          TYPE d
      RETURNING
        VALUE(flight_list) TYPE string_table.
ENDCLASS.

CLASS lcl_flight_search IMPLEMENTATION.
  METHOD search_flights.
    DATA(flight_obj) = NEW lcl_flight_retrieval( ).
    DATA(result) = flight_obj->get_flights(
                    from        = from
                    to          = to
                    flight_date = arrival ).

    IF result IS INITIAL.
      APPEND |X  \| There's no flight from { from } to { to } available on { arrival }.| TO flight_list.
    ELSE.
      LOOP AT result INTO DATA(flight_wa).
        APPEND |OK \| Flight from { flight_wa-from } to { flight_wa-to } available on { flight_wa-flight_date } at { flight_wa-flight_time }.| TO flight_list.
      ENDLOOP.
    ENDIF.

    result = flight_obj->get_flights(
                  from        = to
                  to          = from
                  flight_date = departure ).

    IF result IS INITIAL.
      APPEND |X  \| There's no flight from { to } to { from } available on { departure }.| TO flight_list.
    ELSE.
      LOOP AT result INTO flight_wa.
        APPEND |OK \| Flight from { flight_wa-from } to { flight_wa-to } available on { flight_wa-flight_date } at { flight_wa-flight_time }.| TO flight_list.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

"Class to get hotels from a data source
"In this simple, self-contained example, the data source is
"simulated by an internal table that includes demo data with
"hotel details such as unavailable dates.
CLASS lcl_hotel_retrieval DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF hotel_struc,
             city         TYPE c LENGTH 20,
             hotel_name   TYPE c LENGTH 20,
             is_available TYPE abap_boolean,
           END OF hotel_struc,
           hotel_tab_type TYPE TABLE OF hotel_struc WITH EMPTY KEY.

    METHODS get_hotels
      IMPORTING
        city          TYPE string
        arrival       TYPE d
        departure     TYPE  d
      RETURNING
        VALUE(hotels) TYPE hotel_tab_type.
    CLASS-METHODS class_constructor.
  PRIVATE SECTION.
    CLASS-DATA hotel_tab TYPE hotel_tab_type.
    TYPES: BEGIN OF hotel_struc_availability,
             city             TYPE c LENGTH 20,
             hotel_name       TYPE c LENGTH 20,
             unavailable_date TYPE d,
           END OF hotel_struc_availability,
           hotel_tab_availability_type TYPE TABLE OF hotel_struc_availability WITH EMPTY KEY.
    CLASS-DATA hotel_tab_availability TYPE hotel_tab_availability_type.
    DATA date_tab TYPE TABLE OF d WITH EMPTY KEY.
    DATA flag TYPE abap_boolean.
ENDCLASS.

CLASS lcl_hotel_retrieval IMPLEMENTATION.
  METHOD get_hotels.
    SELECT * FROM @hotel_tab_availability AS tab
      WHERE city = @city
      INTO TABLE @DATA(hotel_list).

    LOOP AT hotel_list INTO DATA(waf) GROUP BY ( key = waf-hotel_name ) ASCENDING INTO DATA(keyf).
      LOOP AT GROUP keyf INTO DATA(memberf).
        APPEND memberf-unavailable_date TO date_tab.
      ENDLOOP.

      "Checking whether hotels are available during the travel time span
      LOOP AT date_tab INTO DATA(date).
        IF date >= arrival AND date <= departure.
          flag = abap_false.
          EXIT.
        ELSE.
          flag = abap_true.
        ENDIF.
      ENDLOOP.

      IF flag = abap_true.
        APPEND VALUE #( city = memberf-city hotel_name = memberf-hotel_name is_available = abap_true ) TO hotels.
      ENDIF.

      CLEAR flag.
      CLEAR date_tab.
    ENDLOOP.

  ENDMETHOD.
  METHOD class_constructor.
    hotel_tab_availability = VALUE #(
        ( city = 'Frankfurt' hotel_name = 'ABC' unavailable_date = '20250512' )
        ( city = 'Frankfurt' hotel_name = 'ABC' unavailable_date = '20250612' )
        ( city = 'Frankfurt' hotel_name = 'DEF' unavailable_date = '20250512' )
        ( city = 'Frankfurt' hotel_name = 'DEF' unavailable_date = '20250712' )
        ( city = 'Frankfurt' hotel_name = 'GHI' unavailable_date = '20250512' )
        ( city = 'Frankfurt' hotel_name = 'GHI' unavailable_date = '20250812' )
        ( city = 'Shanghai' hotel_name = 'JKL' unavailable_date = '20250512' )
        ( city = 'Shanghai' hotel_name = 'JKL' unavailable_date = '20250610' )
        ( city = 'Shanghai' hotel_name = 'MNO' unavailable_date = '20250712' )
        ( city = 'Shanghai' hotel_name = 'MNO' unavailable_date = '20250611' )
        ( city = 'Shanghai' hotel_name = 'PQR' unavailable_date = '20250408' )
        ( city = 'Shanghai' hotel_name = 'PQR' unavailable_date = '20250912' )
        ( city = 'Shanghai' hotel_name = 'PQR' unavailable_date = '20250612' ) ).
  ENDMETHOD.
ENDCLASS.

"An object of the lcl_hotel_search class is required for searching hotels. This object is
"created by the facade class. The lcl_hotel_search class implementation includes the calling
"of a method of the lcl_hotel_retrieval class to retrieve a list of available hotels.
CLASS lcl_hotel_search DEFINITION.
  PUBLIC SECTION.
    METHODS search_hotel
      IMPORTING
        destination       TYPE string
        arrival           TYPE d
        departure         TYPE d
      RETURNING
        VALUE(hotel_list) TYPE string_table.
ENDCLASS.

CLASS lcl_hotel_search IMPLEMENTATION.
  METHOD search_hotel.

    DATA(hotel_obj) = NEW lcl_hotel_retrieval( ).
    DATA(result) = hotel_obj->get_hotels(
                    city      = destination
                    arrival   = arrival
                    departure = departure ).

    IF result IS INITIAL.
      APPEND |X  \| There's no hotel available in { destination } during your trip from { arrival } to { departure }.| TO hotel_list.
    ELSE.
      LOOP AT result INTO DATA(hotel_wa).
        APPEND |OK \| Hotel "{ hotel_wa-hotel_name }" in { destination } is availble during your trip from { arrival } to { departure }.| TO hotel_list.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

"Class to get rental cars from a data source
"In this simple, self-contained example, the data source is
"simulated by an internal table that includes demo data with
"rental car details such as unavailable dates.
CLASS lcl_rental_car_retrieval DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF rental_car_struc,
             city         TYPE c LENGTH 20,
             car          TYPE c LENGTH 20,
             is_available TYPE abap_boolean,
           END OF rental_car_struc,
           rental_car_tab_type TYPE TABLE OF rental_car_struc WITH EMPTY KEY.

    METHODS get_rental_cars
      IMPORTING
        city               TYPE string
        arrival            TYPE d
        departure          TYPE  d
      RETURNING
        VALUE(rental_cars) TYPE rental_car_tab_type.
    CLASS-METHODS class_constructor.
  PRIVATE SECTION.
    CLASS-DATA rental_car_tab TYPE rental_car_tab_type.
    TYPES: BEGIN OF rental_car_struc_availability,
             city             TYPE c LENGTH 20,
             car              TYPE c LENGTH 20,
             unavailable_date TYPE d,
           END OF rental_car_struc_availability,
           rent_car_tab_availability_type TYPE TABLE OF rental_car_struc_availability WITH EMPTY KEY.
    CLASS-DATA rental_car_availability_tab TYPE rent_car_tab_availability_type.
    DATA date_tab TYPE TABLE OF d WITH EMPTY KEY.
    DATA flag TYPE abap_boolean.
ENDCLASS.

CLASS lcl_rental_car_retrieval IMPLEMENTATION.
  METHOD get_rental_cars.
    SELECT * FROM @rental_car_availability_tab AS tab
      WHERE city = @city
      INTO TABLE @DATA(rental_car_list).

    LOOP AT rental_car_list INTO DATA(waf) GROUP BY ( key = waf-car ) ASCENDING INTO DATA(keyf).
      LOOP AT GROUP keyf INTO DATA(memberf).
        APPEND memberf-unavailable_date TO date_tab.
      ENDLOOP.

      LOOP AT date_tab INTO DATA(date).
        IF date >= arrival AND date <= departure.
          flag = abap_false.
          EXIT.
        ELSE.
          flag = abap_true.
        ENDIF.
      ENDLOOP.

      IF flag = abap_true.
        APPEND VALUE #( city = memberf-city car = memberf-car is_available = abap_true ) TO rental_cars.
      ENDIF.

      CLEAR flag.
      CLEAR date_tab.
    ENDLOOP.
  ENDMETHOD.
  METHOD class_constructor.
    rental_car_availability_tab = VALUE #(
      ( city = 'Frankfurt' car = 'Car 1' unavailable_date = '20250512' )
      ( city = 'Frankfurt' car = 'Car 1' unavailable_date = '20250612' )
      ( city = 'Frankfurt' car = 'Car 2' unavailable_date = '20250512' )
      ( city = 'Frankfurt' car = 'Car 2' unavailable_date = '20250712' )
      ( city = 'Frankfurt' car = 'Car 3' unavailable_date = '20250512' )
      ( city = 'Frankfurt' car = 'Car 3' unavailable_date = '20250812' )
      ( city = 'Shanghai' car = 'Car 4' unavailable_date = '20250512' )
      ( city = 'Shanghai' car = 'Car 4' unavailable_date = '20250612' )
      ( city = 'Shanghai' car = 'Car 5' unavailable_date = '20250607' )
      ( city = 'Shanghai' car = 'Car 5' unavailable_date = '20250712' )
      ( city = 'Shanghai' car = 'Car 5' unavailable_date = '20250812' )
      ( city = 'Shanghai' car = 'Car 6' unavailable_date = '20250912' )
      ( city = 'Shanghai' car = 'Car 6' unavailable_date = '20251012' ) ).
  ENDMETHOD.
ENDCLASS.

"An object of the lcl_rental_car_search class is required for searching rental cars. This object is
"created by the facade class. The lcl_rental_car_search class implementation includes the calling
"of a method of the lcl_rental_car_retrieval class to retrieve a list of available rental cars.
CLASS lcl_rental_car_search DEFINITION.
  PUBLIC SECTION.
    METHODS search_rental_car
      IMPORTING
        destination            TYPE string
        arrival                TYPE d
        departure              TYPE d
      RETURNING
        VALUE(rental_car_list) TYPE string_table.
ENDCLASS.

CLASS lcl_rental_car_search IMPLEMENTATION.
  METHOD search_rental_car.
    DATA(rental_car_obj) = NEW lcl_rental_car_retrieval( ).
    DATA(res) = rental_car_obj->get_rental_cars(
                  city      = destination
                  arrival   = arrival
                  departure = departure ).

    IF res IS INITIAL.
      APPEND |X  \| There's no rental car available in { destination } during your trip from { arrival } to { departure }.| TO rental_car_list.
    ELSE.
      LOOP AT res INTO DATA(rental_car_wa).
        APPEND |OK \| "{ rental_car_wa-car }" is available as rental car in { destination } during your trip from { arrival } to { departure }.| TO rental_car_list.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

"Facade class that represents a simplified API, hiding the complexity from users and allowing
"them to achieve desired results without dealing with and knowing about underlying complexities.
"It includes various object creations and method calls.
CLASS lcl_travel_facade DEFINITION.
  PUBLIC SECTION.
    METHODS plan_travel
      IMPORTING
        from                       TYPE string
        to                         TYPE string
        arrival                    TYPE d
        departure                  TYPE d
      RETURNING
        VALUE(reservation_options) TYPE string_table.
ENDCLASS.

CLASS lcl_travel_facade IMPLEMENTATION.
  METHOD plan_travel.
    "Abort proceeding with the example when the departure is before the arrival.
    IF departure <= arrival.
      ASSERT 1 = 0.
    ENDIF.

    "Creating multipled objects
    DATA(flight_search) = NEW lcl_flight_search( ).
    DATA(hotel_search) = NEW lcl_hotel_search( ).
    DATA(rental_car_search) = NEW lcl_rental_car_search( ).

    "Adding travel search information to a string table for demonstration and output purposes
    APPEND |Travel options for: { from } - { to }, { arrival } - { departure }| TO reservation_options.
    DATA(flights) = flight_search->search_flights( from = from to = to arrival = arrival departure = departure ).
    APPEND `---------- FLIGHTS -----------------------------------------------------------------------------` TO reservation_options.
    APPEND LINES OF flights TO reservation_options.
    DATA(hotels) = hotel_search->search_hotel( destination = to arrival = arrival departure = departure  ).
    APPEND `---------- HOTELS ------------------------------------------------------------------------------` TO reservation_options.
    APPEND LINES OF hotels TO reservation_options.
    DATA(rental_cars) = rental_car_search->search_rental_car( destination = to arrival = arrival departure = departure ).
    APPEND `---------- RENTAL CARS -------------------------------------------------------------------------` TO reservation_options.
    APPEND LINES OF rental_cars TO reservation_options.
  ENDMETHOD.
ENDCLASS.
