*&--------------------------------------------------------------------*
*& Custom exception classes
*&--------------------------------------------------------------------*

CLASS cx_afternoon DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS cx_night DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

*&--------------------------------------------------------------------*
*& Class to demonstrate various method parameters
*& All formal parameters are passed by reference except the
*& returning parameter.
*&--------------------------------------------------------------------*
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      "No parameters
      hallo_static_ext,

      "One importing parameter
      powers_of_two IMPORTING i_pow TYPE i,

      "Two importing parameters
      "Specifying REFERENCE(p) is optional; a formal parameter
      "without VALUE(p) or REFERENCE(p) is REFERENCE(p) by default
      addition IMPORTING i_add1            TYPE i
                         REFERENCE(i_add2) TYPE i,

      "Two importing parameters, one of them is optional.
      addition_optional IMPORTING i_add_mand TYPE i
                                  i_add_opt  TYPE i OPTIONAL,

      "Importing and exporting parameters
      subtraction IMPORTING i_sub1       TYPE i
                            i_sub2       TYPE i
                  EXPORTING e_sub_result TYPE i,

      "One exporting parameter
      exporting_hallo EXPORTING text TYPE string,

      "Changing parameter
      square_root CHANGING i_sqr TYPE decfloat34,

      "Importing and returning parameters
      multiplication IMPORTING i_mult1              TYPE i
                               i_mult2              TYPE i
                     RETURNING VALUE(r_mult_result) TYPE i,

      "Importing and exporting parameters
      "for comparing the signature with method 'multiplication'
      multiplication_exp_param IMPORTING i_multa       TYPE i
                                         i_multb       TYPE i
                               EXPORTING e_mult_result TYPE i,

      "Includes RAISING
      division IMPORTING i_div1              TYPE i
                         i_div2              TYPE i
               RETURNING VALUE(r_div_result) TYPE decfloat34
               RAISING   cx_sy_arithmetic_error,

      check_daytime IMPORTING time      TYPE t
                    EXPORTING greetings TYPE string
                    RAISING   cx_afternoon cx_night,

      "Include parameters with generic types
      generic_data IMPORTING i_data TYPE data,
      generic_tab IMPORTING i_anytab TYPE ANY TABLE.

    CLASS-DATA: calc_result TYPE i,
                string      TYPE string,
                some_data   TYPE REF TO data.

ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.

  METHOD hallo_static_ext.
    string = |Hallo { sy-uname }. | &&
             |I'm a static method of class lcl_demo.|.
  ENDMETHOD.

  METHOD square_root.
    i_sqr = sqrt( i_sqr ).
  ENDMETHOD.

  METHOD powers_of_two.
    calc_result = i_pow * i_pow.
  ENDMETHOD.

  METHOD addition.
    calc_result = i_add1 + i_add2.
  ENDMETHOD.

  METHOD addition_optional.
    calc_result = i_add_mand + i_add_opt.
  ENDMETHOD.

  METHOD subtraction.
    e_sub_result = i_sub1 - i_sub2.
  ENDMETHOD.

  METHOD exporting_hallo.
    text = |Hallo { sy-uname }. | && |I'm a static method of class lcl_demo with one exporting parameter.|.
  ENDMETHOD.

  METHOD multiplication.
    r_mult_result = i_mult1 * i_mult2.
  ENDMETHOD.

  METHOD multiplication_exp_param.
    e_mult_result = i_multa * i_multb.
  ENDMETHOD.

  METHOD division.
    CLEAR string.

    TRY.
        r_div_result = i_div1 / i_div2.
      CATCH cx_sy_arithmetic_error INTO DATA(exc).
        string = exc->get_text( ).
    ENDTRY.

  ENDMETHOD.

  METHOD check_daytime.
   CLEAR string.

    "Morning: 5 am to 12 pm
    IF time BETWEEN '050001' AND '120000'.
      DATA(subrc) = 0.
    ENDIF.

    "Afternoon: 12 pm to 5 pm.
    IF time BETWEEN '120001' AND '170000'.
      subrc = 11.
    ENDIF.

    "Evening 5 pm to 9 pm.
    "Commented out on purpose to have a time range for OTHERS :)
    "IF time BETWEEN '170001' AND '210000'.
    " subrc = 22.
    "ENDIF.

    "Night: 9 pm to 4 am.
    IF time BETWEEN '210001' AND '050000'.
      subrc = 33.
    ENDIF.

    IF subrc <> 0.
      CASE subrc.
        WHEN 11.
          greetings = |Good afternoon.|.
        WHEN 33.
          greetings = |Good night.|.
        WHEN OTHERS.
          greetings = |It's neither morning, afternoon or night. | &&
                      |Hence, wishing you a good evening.|.
      ENDCASE.
    ELSE.
      greetings = |Good morning.|.
    ENDIF.

  ENDMETHOD.

  METHOD generic_data.
    "A data reference variable is created that has the type of the
    "imported variable. Its content is store in the variable
    "some_data in the public section to be able to access the content.
    CREATE DATA some_data LIKE i_data.
    some_data->* = i_data.
  ENDMETHOD.

  METHOD generic_tab.
    "See implementation of generic_data.
    "Here, an internal table is handled.
    CREATE DATA some_data LIKE i_anytab.
    some_data->* = i_anytab.
  ENDMETHOD.

ENDCLASS.

*&--------------------------------------------------------------------*
*& Class to demonstrate basics in the global class
*&--------------------------------------------------------------------*

CLASS local_class DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor.

    DATA: num_inst  TYPE i,
          uuid      TYPE sysuuid_x16,
          timestamp TYPE timestampl.

    CLASS-DATA: no_of_instances TYPE i READ-ONLY,
                num_stat        TYPE i VALUE 33.

    CONSTANTS: const_number TYPE i VALUE 11.

    TYPES type_i TYPE i.

ENDCLASS.

CLASS local_class IMPLEMENTATION.
  METHOD constructor.
    "Number of instances of the class are counted.
    no_of_instances = no_of_instances + 1.
    "Set a time stamp.
    GET TIME STAMP FIELD timestamp.
    "Increase the number.
    num_inst = num_inst + 1.
    "Get a random UUID.
    TRY.
        uuid = cl_system_uuid=>create_uuid_x16_static( ) .
      CATCH cx_uuid_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*&--------------------------------------------------------------------*
*& Class to demonstrate events
*&--------------------------------------------------------------------*

CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    DATA: greets TYPE string.

    "Events declaration.
    EVENTS: morning, afternoon, evening, night.

    "Event handler methods
    METHODS: morning_greets FOR EVENT morning OF lcl_events,
      afternoon_greets FOR EVENT afternoon OF lcl_events,
      evening_greets FOR EVENT evening OF lcl_events,
      night_greets FOR EVENT night OF lcl_events.

    "Method to raise events
    METHODS: greetings.
ENDCLASS.

CLASS lcl_events IMPLEMENTATION.

  METHOD greetings.

    DATA(syst_time) = cl_abap_context_info=>get_system_time( ).

    "Morning: 5 am to 12 pm
    IF syst_time BETWEEN '050001' AND '120000'.
      RAISE EVENT morning.

      "Afternoon: 12 pm to 5 pm.
    ELSEIF syst_time BETWEEN '120001' AND '170000'.
      RAISE EVENT afternoon.

      "Evening 5 pm to 9 pm.
    ELSEIF syst_time BETWEEN '170001' AND '210000'.
      RAISE EVENT evening.

      "Night: 9 pm to 5 am.
    ELSEIF syst_time BETWEEN '210001' AND '050000'.
      RAISE EVENT night.
    ENDIF.

  ENDMETHOD.

  METHOD morning_greets.
    greets = |Good morning, { sy-uname }.|.
  ENDMETHOD.

  METHOD afternoon_greets.
    greets = |Good afternoon, { sy-uname }.|.
  ENDMETHOD.

  METHOD evening_greets.
    greets = |Good evening, { sy-uname }.|.
  ENDMETHOD.

  METHOD night_greets.
    greets = |Good night, { sy-uname }.|.
  ENDMETHOD.

ENDCLASS.

*&--------------------------------------------------------------------*
*& Class to demonstrate constructors
*&--------------------------------------------------------------------*

CLASS lcl_constructors DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor IMPORTING num1 TYPE i
                                  num2 TYPE i RAISING cx_sy_zerodivide.

    DATA: uuid          TYPE sysuuid_x16,
          in_div_result TYPE i,
          in_text       TYPE string.


    CLASS-METHODS: class_constructor,
      add_1.

    CLASS-DATA: no_of_instances TYPE i READ-ONLY,
                stat_number     TYPE i,
                stat_text       TYPE string.

ENDCLASS.

CLASS lcl_constructors IMPLEMENTATION.

  METHOD constructor.
    "Get time stamp.
    DATA(ts1) = utclong_current( ).
    "Provide message.
    in_text = |The instance constructor of the class | &&
              |lcl_constructors was called on { ts1 }.|.

    "Count number of instances.
    no_of_instances = no_of_instances + 1.
    "Get random UUID.
    TRY.
        uuid = cl_system_uuid=>create_uuid_x16_static( ) .
      CATCH cx_uuid_error.
    ENDTRY.

    CLEAR in_div_result.
    "Do calculation.
    in_div_result = num1 / num2.
  ENDMETHOD.

  METHOD class_constructor.
    "Set a number.
    stat_number = 999.
    "Get time stamp.
    DATA(ts2) = utclong_current( ).
    "Provide message.
    stat_text = |The static constructor of the class | &&
                |lcl_constructors was called on { ts2 } and the | &&
                |value for the variable 'stat_number' was set to | &&
                |{ stat_number }.|.
  ENDMETHOD.

  METHOD add_1.
    stat_number += 1.
  ENDMETHOD.

ENDCLASS.

*&--------------------------------------------------------------------*
*& Classes to demonstrate inheritance, polymorphism and casting
*&--------------------------------------------------------------------*

"Class 1
CLASS lcl_class1 DEFINITION.

  PUBLIC SECTION.
    "Note: All methods are purposely included in the public section.
    "Otherwise, it cannot be called in the demo's main class.
    METHODS: constructor IMPORTING i_obj TYPE string OPTIONAL,
      get_string RETURNING VALUE(str) TYPE string,
      get_obj_name RETURNING VALUE(obj) TYPE string.

  PRIVATE SECTION.
    DATA: obj_name TYPE string.

ENDCLASS.

CLASS lcl_class1 IMPLEMENTATION.

  METHOD constructor.
    obj_name = i_obj.
  ENDMETHOD.

  METHOD get_obj_name.
    obj = obj_name.
  ENDMETHOD.

  METHOD get_string.
    str = `Hallo`.
  ENDMETHOD.

ENDCLASS.

"Class 2a
CLASS lcl_class2a DEFINITION INHERITING FROM lcl_class1.

  PUBLIC SECTION.

    METHODS: get_string REDEFINITION,
      get_number_2a RETURNING VALUE(num) TYPE i..

ENDCLASS.

CLASS lcl_class2a IMPLEMENTATION.

  METHOD get_string.
    str = |{ super->get_string( ) }, { sy-uname }!|.
  ENDMETHOD.

  METHOD get_number_2a.
    num = cl_abap_random_int=>create(
     seed = cl_abap_random=>seed( ) min = 1 max = 100 )->get_next( ).
  ENDMETHOD.

ENDCLASS.

"Class 2b
CLASS lcl_class2b DEFINITION INHERITING FROM lcl_class1 FINAL.

  PUBLIC SECTION.

    METHODS: get_string REDEFINITION,
      get_number_2b RETURNING VALUE(num) TYPE i.

ENDCLASS.

CLASS lcl_class2b IMPLEMENTATION.

  METHOD get_string.
    str = |{ super->get_string( ) } from lcl_class2b, { sy-uname }!|.
  ENDMETHOD.

  METHOD get_number_2b.
    num = cl_abap_random_int=>create(
     seed = cl_abap_random=>seed( ) min = 1 max = 100 )->get_next( ).
  ENDMETHOD.

ENDCLASS.

"Class 3a
CLASS lcl_class3a DEFINITION INHERITING FROM lcl_class2a FINAL.

  PUBLIC SECTION.

    METHODS: get_string REDEFINITION.

ENDCLASS.

CLASS lcl_class3a IMPLEMENTATION.

  METHOD get_string.
    str = |{ super->get_string( ) } How are you doing?|.
  ENDMETHOD.

ENDCLASS.

*&--------------------------------------------------------------------*
*& Classes to demonstrate a factory method in a singleton
*& and an abstract class.
*&--------------------------------------------------------------------*

"Using the addition CREATE PRIVATE, objects can only be created by the class itself.
CLASS lcl_singleton DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.

    METHODS: constructor,
      "Methods for setting and getting a time stamp.
      get_timestamp RETURNING VALUE(res_timestamp)
                                TYPE timestampl,
      set_timestamp.

    CLASS-METHODS:
      "Factory method that returns an instance of the class.
      get_instance RETURNING VALUE(res_instance) TYPE REF TO lcl_singleton.

    CLASS-DATA: "Holds the number of overall instances.
                no_of_instances TYPE i READ-ONLY.

  PRIVATE SECTION.
    CLASS-DATA: obj TYPE REF TO lcl_singleton.

    DATA: timestamp TYPE timestampl.

ENDCLASS.

CLASS lcl_singleton IMPLEMENTATION.

  METHOD get_instance.
    "Checking if an instance of the class already exists.
    "An instance should only be created if no instance exists
    "to make sure that there is only a single instance overall.
    IF obj IS NOT BOUND.
      obj = NEW #( ).
    ENDIF.
    "In case an instance already exists, the existing one is
    "always returned.
    res_instance = obj.
  ENDMETHOD.

  METHOD constructor.
    "Counts the number of instances of the class.
    no_of_instances = no_of_instances + 1.
  ENDMETHOD.

  METHOD get_timestamp.
    res_timestamp = timestamp.
  ENDMETHOD.

  METHOD set_timestamp.
    GET TIME STAMP FIELD timestamp.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_sub DEFINITION DEFERRED.

CLASS lcl_abstract DEFINITION ABSTRACT.

  PUBLIC SECTION.

    CLASS-METHODS: factory_method IMPORTING check_num  TYPE i
                                  RETURNING VALUE(obj) TYPE REF TO lcl_abstract.

    CLASS-DATA: message TYPE string.

    "Abstract method: There's no implementation in this class.
    METHODS: return_string ABSTRACT
      IMPORTING i_str             TYPE string
      RETURNING VALUE(res_string) TYPE string.

ENDCLASS.

CLASS lcl_sub DEFINITION INHERITING FROM lcl_abstract.

  PUBLIC SECTION.

    METHODS: return_string REDEFINITION.

ENDCLASS.


CLASS lcl_abstract IMPLEMENTATION.

  METHOD factory_method.
    "Purpose of factory method: An instance can only be created
    "if a certain condition is met.
    CASE check_num.
      WHEN 1.
        obj = NEW lcl_sub( ).
        message = `Great! I was able to create an instance.`.
      WHEN OTHERS.
        message = `What a pity. I'm not allowed to create an instance.`.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_sub IMPLEMENTATION.

  METHOD return_string.
    res_string = |I'm a returned string. | &&
                 |The object reference variable is { i_str }.|.
  ENDMETHOD.

ENDCLASS.
