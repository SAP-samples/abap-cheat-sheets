***********************************************************************
*
*           ABAP cheat sheet: Date, Time, and Time Stamp
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntax options for working with
*   date, time, and time stamp.
*
* ----------------------- GETTING STARTED -----------------------------
* - Open the class with the ABAP development tools for Eclipse (ADT).
* - Choose F9 to run the class.
* - Check the console output.
* - To understand the context and the ABAP syntax used, refer to the
*   notes included in the class as comments or refer to the respective
*   topic in the ABAP Keyword Documentation.
* - Due to the amount of console output, the examples contain numbers
*   (e.g. 1) ..., 2) ..., 3) ...) for the individual example sections.
*   Also, the variable name is displayed in most cases. So to find
*   the relevant output in the console easier and faster, just search
*   for the number/variable name in the console (CTRL+F in the console)
*   or use the debugger.
*
* ----------------------------- NOTE -----------------------------------
* The code presented in this class is intended only to support the ABAP
* cheat sheets. It is not intended for direct use in a production system
* environment. The code examples in the ABAP cheat sheets are primarily
* intended to provide a better explanation and visualization of the
* syntax and semantics of ABAP statements, not to solve concrete
* programming tasks. For production application programs, you should
* always work out your own solution for each individual case. There is
* no guarantee for the correctness or completeness of the code.
* Furthermore, there is no legal responsibility or liability for any
* errors or their consequences that may occur when using the the example
* code.
*
***********************************************************************
"! <p class="shorttext synchronized">ABAP cheat sheet: Date, Time, and Time Stamp</p>
"! Example to demonstrate handling and procesing dates, times, and time stamps in ABAP.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_date_time DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_date_time IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: Date, Time, and Time Stamp\n\n| ).

**********************************************************************

    out->write( |1) Retrieving the Time Zone\n\n| ).

    "Retrieving the time zone of a given user
    TRY.
        DATA(tz_user) = cl_abap_context_info=>get_user_time_zone( ).
      CATCH cx_abap_context_info_error.
    ENDTRY.

    "Using XCO
    "The reference created without '->value' is used in some XCO method calls,
    "as shown in some of the following code snippets.
    "User's time zone
    DATA(tz_user_xco) = xco_cp_time=>time_zone->user->value.
    "UTC
    DATA(tz_utc_xco) = xco_cp_time=>time_zone->utc->value.

    out->write( data = tz_user name = `tz_user` ).
    out->write( |\n| ).
    out->write( data = tz_user_xco name = `tz_user_xco` ).
    out->write( |\n| ).
    out->write( data = tz_utc_xco name = `tz_utc_xco` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Retrieving and Creating Dates` ) ).

    "--------------------- Retrieving the current date --------------------
    "Retrieving the current date with respect to UTC.
    "The result's base type is the DDIC type dats that is mapped to the
    "ABAP type d.
    DATA(utc_date) = cl_abap_context_info=>get_system_date( ).

    "Using XCO
    "Notes:
    "- The result of the following chained statement is of type string.
    "- With the 'as' method, a given format (available via xco_cp_time=>format)
    "  is applied to the time.
    "- The 'date' method has a parameter for specifying the time zone.
    "  By default, the user's time zone is used. For the specification, you
    "  can use xco_cp_time=>time_zone.
    "- The examples explore multiple formatting options that are specified in the
    "  'date' and 'as' methods.
    "Various formatting options
    DATA(date_xco_iso_basic) = xco_cp=>sy->date( )->as( xco_cp_time=>format->iso_8601_basic )->value.
    DATA(date_xco_iso_ext) = xco_cp=>sy->date( )->as( xco_cp_time=>format->iso_8601_extended )->value.
    DATA(date_xco_abap) = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value.
    "Specifying the user time zone explicitly (it is the default value)
    DATA(date_xco_expl_user_tz) = xco_cp=>sy->date( xco_cp_time=>time_zone->user
                                   )->as( xco_cp_time=>format->iso_8601_extended
                                   )->value.
    "Specifying UTC explicitly
    DATA(date_xco_expl_utc) = xco_cp=>sy->date( xco_cp_time=>time_zone->utc
                                   )->as( xco_cp_time=>format->iso_8601_extended
                                   )->value.

    "--------------------- Retrieving current date values (XCO) --------------------
    DATA(this_day) = xco_cp=>sy->date( )->day.
    DATA(this_month) = xco_cp=>sy->date( )->month.
    DATA(this_year) = xco_cp=>sy->date( )->year.

    "--------------------- Creating dates --------------------
    DATA date_cr1 TYPE d.
    date_cr1 = '20240101'.
    DATA date_cr2 TYPE d VALUE '20240202'.
    DATA(date_cr3) = CONV d( '20240303' ).

    "Using XCO
    "The result contains a reference. It can be used for further processing
    "with the XCO library.
    DATA(date_w_xco) = xco_cp_time=>date( iv_year = 2024
                                          iv_month = 3
                                          iv_day = 3 ).

    "Examples
    DATA(date_cr4) = date_w_xco->as( xco_cp_time=>format->iso_8601_extended )->value.
    DATA(day_from_date) = date_w_xco->day.
    DATA(month_from_date) = date_w_xco->month.

    out->write( data = utc_date name = `utc_date` ).
    out->write( |\n| ).
    out->write( data = date_xco_iso_basic name = `date_xco_iso_basic` ).
    out->write( |\n| ).
    out->write( data = date_xco_iso_ext name = `date_xco_iso_ext` ).
    out->write( |\n| ).
    out->write( data = date_xco_abap name = `date_xco_abap` ).
    out->write( |\n| ).
    out->write( data = date_xco_expl_user_tz name = `date_xco_expl_user_tz` ).
    out->write( |\n| ).
    out->write( data = date_xco_expl_utc name = `date_xco_expl_utc` ).
    out->write( |\n| ).
    out->write( data = this_day name = `this_day` ).
    out->write( |\n| ).
    out->write( data = this_month name = `this_month` ).
    out->write( |\n| ).
    out->write( data = this_year name = `this_year` ).
    out->write( |\n| ).
    out->write( data = date_cr4 name = `date_cr4` ).
    out->write( |\n| ).
    out->write( data = day_from_date name = `day_from_date` ).
    out->write( |\n| ).
    out->write( data = month_from_date name = `month_from_date` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Validity of Date Fields` ) ).
    "Before accessing date (or time) fields, ensure that the content of these
    "fields is valid to avoid unexpected results, such as incorrect calculations.
    "The ABAP runtime framework checks the validity of these fields in various
    "contexts, including lossless assignments and assignments to numeric fields.

    "Lossless assignements using the EXACT operator
    "Due to the invalid dates, an exception is raised.
    TRY.
        DATA(inv_date1) = EXACT d( '20240231' ).
      CATCH cx_sy_conversion_no_date INTO DATA(error1).
        out->write( data = error1->get_text( ) name = `error1->get_text( )` ).
        out->write( |\n| ).
    ENDTRY.

    TRY.
        DATA(inv_date2) = EXACT d( '2024XY31' ).
      CATCH cx_sy_conversion_no_date INTO DATA(error2).
        out->write( data = error2->get_text( ) name = `error2->get_text( )` ).
        out->write( |\n| ).
    ENDTRY.

    "Assignment of an invalid date of type d to type i; the initial value
    "is produced
    DATA(inv_date3) = CONV i( CONV d( '20240231' ) ).
    IF inv_date3 = 0.
      out->write( `inv_date3 = 0` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Character-Like Access to Date Fields` ) ).
    "Since the content is character-like, you can use string processing functionalities
    "to access date values. This also applies to time fields of type t.
    "The following examples show a selection.

    "Extracting date values using the string function substring.
    DATA some_date TYPE d VALUE '20240102'.
    DATA(year_sub) = substring( val = some_date off = 0 len = 4 ).
    DATA(month_sub) = substring( val = some_date off = 4 len = 2 ).
    DATA(day_sub) = substring( val = some_date off = 6 len = 2 ).

    "Replacing using the string function replace
    DATA(year_repl)  = replace( val = some_date off = 0 len = 4 with = `2025` ).

    "Offset and length specifications
    DATA(off_len_spec_day) = some_date+6(2).
    some_date+4(2) = '10'.

    out->write( data = year_sub name = `year_sub` ).
    out->write( |\n| ).
    out->write( data = month_sub name = `month_sub` ).
    out->write( |\n| ).
    out->write( data = day_sub name = `day_sub` ).
    out->write( |\n| ).
    out->write( data = year_repl name = `year_repl` ).
    out->write( |\n| ).
    out->write( data = off_len_spec_day name = `off_len_spec_day` ).
    out->write( |\n| ).
    out->write( data = some_date name = `some_date` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Numeric Access and Calculations` ) ).
    "When converting date fields to numeric values, the type d produces an integer
    "representing the number of days since 01.01.000
    "This is especially important when using date fields in calculations and converting
    "the values to numeric types.
    "The XCO library offers methods for performing calculations. In the code snippet,
    "examples use the value attribute. The return value of the method calls is of type
    "string.

    DATA date_calc_1 TYPE d VALUE '20240101'.
    DATA date_calc_2 TYPE d VALUE '20231227'.
    DATA date_calc_3 TYPE d VALUE '20231230'.
    DATA date_calc_4 TYPE d VALUE '20240220'.

    DATA(days_diff_1) = date_calc_1 - date_calc_2.
    DATA(days_diff_2) = date_calc_1 - date_calc_3.
    DATA(days_since_01_01_0001) = CONV i( date_calc_1 ).
    "Getting the weekday (1 = Monday, 2 = Tuesday, ...)
    DATA(weekday1) = ( 5 + date_calc_1  MOD 7 ) MOD 7 + 1.
    DATA(weekday2) = ( 5 + date_calc_3  MOD 7 ) MOD 7 + 1.
    DATA(date_w_first_day_of_month) =  CONV d( replace( val = `202403020` off = 6 len = 2 with = `01` ) ).
    DATA(date_w_last_day_of_prev_month) = CONV d( date_w_first_day_of_month - 1 ).

    "------------ Performing date additions and subtractions using the XCO library ------------

    "Adding days to the current date using the 'add' method
    DATA(xco_date_add_5days) = xco_cp=>sy->date( )->add( iv_day = 5
                                                 )->as( xco_cp_time=>format->iso_8601_extended
                                                 )->value.

    "The 'add' method has various parameters, adding 1 day/month/year
    DATA(xco_date_add_1_mult) = xco_cp=>sy->date( )->add( iv_day = 1 iv_month = 1 iv_year = 1
                                                  )->as( xco_cp_time=>format->iso_8601_extended
                                                  )->value.

    "Addition with a created date
    DATA(xco_date_add_1day_custom) = xco_cp_time=>date( iv_year = 2024 iv_month = 02 iv_day = 28
                                                      )->add( iv_day = 1
                                                      )->as( xco_cp_time=>format->iso_8601_extended
                                                      )->value.

    "Subtracting using the 'subtract' method
    DATA(xco_date_subtr_1_mult) = xco_cp=>sy->date( )->subtract( iv_day = 1 iv_month = 1 iv_year = 1
                                                    )->as( xco_cp_time=>format->iso_8601_extended
                                                    )->value.

    "Optional parameter io_calculation
    "io_calculation parameter: xco_cp_time=>date_calculation->preserving,
    "i.e. the date is calculated mathmatically and preserved. It is the
    "default.
    "In case of an invalid resulting date, an exception is raised.
    TRY.
        DATA(inv_date_a) = xco_cp_time=>date( iv_year  = '2024'
                                              iv_month = '08'
                                              iv_day   = '31'
                                            )->add( iv_month = 1
                                                    io_calculation = xco_cp_time=>date_calculation->preserving
                                            )->as( xco_cp_time=>format->iso_8601_extended
                                            )->value.
      CATCH cx_root INTO DATA(err_pre).
    ENDTRY.

    "io_calculation parameter: xco_cp_time=>date_calculation->ultimo,
    "Here, the actual last day of the month is considered. If the calculated
    "date is invalid, the ultimo is automatically used. In the example, one month is added to
    "the date. However, September does not have 31 days, so the result is adjusted to the actual
    "last day of September, which is 30 in this case.
    TRY.
        DATA(inv_date_b) = xco_cp_time=>date( iv_year  = '2024'
                                              iv_month = '08'
                                              iv_day   = '31'
                                            )->add( iv_month = 1
                                                    io_calculation = xco_cp_time=>date_calculation->ultimo
                                            )->as( xco_cp_time=>format->iso_8601_extended
                                            )->value.
      CATCH cx_root INTO DATA(err_ult).
    ENDTRY.

    out->write( data = days_diff_1 name = `days_diff_1` ).
    out->write( |\n| ).
    out->write( data = days_diff_2 name = `days_diff_2` ).
    out->write( |\n| ).
    out->write( data = days_since_01_01_0001 name = `days_since_01_01_0001` ).
    out->write( |\n| ).
    out->write( data = weekday1 name = `weekday1` ).
    out->write( |\n| ).
    out->write( data = weekday2 name = `weekday2` ).
    out->write( |\n| ).
    out->write( data = date_w_first_day_of_month name = `date_w_first_day_of_month` ).
    out->write( |\n| ).
    out->write( data = date_w_last_day_of_prev_month name = `date_w_last_day_of_prev_month` ).
    out->write( |\n| ).
    out->write( data = xco_date_add_5days name = `xco_date_add_5days` ).
    out->write( |\n| ).
    out->write( data = xco_date_add_1_mult name = `xco_date_add_1_mult` ).
    out->write( |\n| ).
    out->write( data = xco_date_add_1day_custom name = `xco_date_add_1day_custom` ).
    out->write( |\n| ).
    out->write( data = xco_date_subtr_1_mult name = `xco_date_subtr_1_mult` ).
    out->write( |\n| ).
    IF err_pre IS INITIAL.
      out->write( data = inv_date_a name = `inv_date_a` ).
    ELSE.
      out->write( data = err_pre->get_text( ) name = `err_pre->get_text( )` ).
    ENDIF.
    out->write( |\n| ).
    IF err_ult IS INITIAL.
      out->write( data = inv_date_b name = `inv_date_b` ).
    ELSE.
      out->write( data = err_ult->get_text( ) name = `err_ult->get_text( )` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) CL_ABAP_DATFM: Date Conversions` ) ).
    "Using the CL_ABAP_DATFM class, you can perform conversions with external
    "and internal representations of a time according to the date format, e.g.
    "the conversion of a date in a data object of type string to type d and vice
    "versa using a specific date format. Multiple methods are available, of which
    "two are covered in the example. For more information, refer to the class
    "documentation.
    "For the values of the date format for the conversion, see the fixed values
    "by exploring the F2 help for cl_abap_datfm=>ty_datfm.

    "Data type and obect declarations for the example. The example, explores
    "multiple date formats. For this purpose, each loop pass in a DO loop
    "uses a different date format. First, the internal time format is converted
    "to external time formats. In another loop, the conversions are performed
    "the other way round. The results are stored in an internal table.
    DATA(date4conversion) = CONV d( '20240202' ).
    DATA conv_date_str TYPE string.
    DATA conv_date_d TYPE d.
    DATA date_format TYPE cl_abap_datfm=>ty_datfm.
    TYPES: BEGIN OF date_ty_s,
             ext_date TYPE string,
             format   TYPE c LENGTH 1,
             int_date TYPE d,
           END OF date_ty_s.
    DATA date_tab TYPE TABLE OF date_ty_s WITH EMPTY KEY.

    "Conversion of d (internal) to string (external time format)
    DO 7 TIMES.
      TRY.
          cl_abap_datfm=>conv_date_int_to_ext(
            EXPORTING im_datint    = date4conversion
                      im_datfmdes  = SWITCH #( sy-index
                                               WHEN 1 THEN cl_abap_datfm=>get_datfm( )
                                               WHEN 2 THEN '1'
                                               WHEN 3 THEN '2'
                                               WHEN 4 THEN '3'
                                               WHEN 5 THEN '6'
                                               WHEN 6 THEN 'A'
                                               WHEN 7 THEN 'C' )
            IMPORTING ex_datext    = conv_date_str
                      ex_datfmused = date_format  ).
          date_tab = VALUE #( BASE date_tab ( ext_date = conv_date_str format = date_format ) ).
        CATCH cx_abap_datfm_format_unknown.
      ENDTRY.
    ENDDO.

    "Conversion of string (external) to d (internal time format)
    LOOP AT date_tab REFERENCE INTO DATA(ref_date).
      TRY.
          cl_abap_datfm=>conv_date_ext_to_int(
            EXPORTING im_datext    = ref_date->ext_date
                      im_datfmdes  = ref_date->format
            IMPORTING ex_datint    = conv_date_d
                      ex_datfmused = date_format ).
          ref_date->int_date = conv_date_d.
        CATCH cx_abap_datfm_no_date cx_abap_datfm_invalid_date
              cx_abap_datfm_format_unknown cx_abap_datfm_ambiguous.
      ENDTRY.
    ENDLOOP.

    "Note: When outputting the data object with the classrun,
    "20240202, which is of type d, is displayed as 2014-02-02 in
    "the console for better readability.
    out->write( data = date_tab name = `date_tab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) Examples for Time Processing` ) ).
    "The code snippet below provides examples of time processing, such
    "as retrieving the current time, accessing time values, creating time
    "values, and performing time calculations. You can also utilize the
    "XCO library in different scenarios.

    "--------------------- Retrieving the current time --------------------

    "Retrieving the current time in UTC.
    DATA(utc_time) = cl_abap_context_info=>get_system_time( ).

    "Using XCO
    "Note the optional time zone specification.
    DATA(time_w_xco) = xco_cp=>sy->time( xco_cp_time=>time_zone->user
                                        )->as( xco_cp_time=>format->iso_8601_extended
                                        )->value.

    "--------------------- Accessing time values --------------------

    "Note: As mentioned in a previous section on dates, the access to time fields
    "works similar as date fields. As an example, seconds, minutes, and hours are
    "extracted from a time field.
    DATA some_time TYPE t VALUE '123456'.

    DATA(hour_extr) = substring( val = some_time off = 0 len = 2 ).
    DATA(minute_extr) = substring( val = some_time off = 2 len = 2 ).
    DATA(second_extr) = substring( val = some_time off = 4 len = 2 ).

    "Retrieving the current seconds, minutes, hours using XCO
    DATA(sec_w_xco) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->second.
    DATA(min_w_xco) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->minute.
    DATA(hour_w_xco) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->hour.

    "--------------------- Creating times --------------------

    DATA time_cr1 TYPE t.
    time_cr1 = '095812'.
    DATA time_cr2 TYPE t VALUE '112400'.
    DATA(time_cr3) = CONV t( '120000' ).

    "Using XCO
    "The result contains a reference. It can be used for further processing
    "with the XCO library.
    DATA(xco_time) = xco_cp_time=>time( iv_hour = '08'
                                        iv_minute = '34'
                                        iv_second = '05' ).

    "Examples
    DATA(time_cr4) = xco_time->as( xco_cp_time=>format->iso_8601_extended )->value.
    DATA(hours_from_xco_time) = xco_time->hour.
    DATA(minutes_from_xco_time) = xco_time->minute.
    DATA(seconds_from_xco_time) = xco_time->second.

    "------------ Performing time calculations ------------

    "Retrieving seconds, minutes, and hours from a time value in a data object
    "of type t
    DATA time4calc TYPE t VALUE '115708'.
    DATA(seconds_total) = CONV i( time4calc ).
    DATA(calc_hours) = seconds_total DIV 3600.
    DATA(min_calc) = ( seconds_total - calc_hours * 3600 ) DIV 60.
    DATA(sec_calc) = ( seconds_total - calc_hours * 3600 ) - min_calc * 60.

    "Using the XCO library
    "See the snippet above in the date section as well as the class documentation.
    "Adding
    DATA(time_xco_add) = xco_cp=>sy->time( xco_cp_time=>time_zone->user
                                         )->add( iv_hour = 1 iv_minute = 1 iv_second = 1
                                         )->as( xco_cp_time=>format->iso_8601_extended
                                         )->value.
    "Subtracting
    DATA(time_xco_subtr) = xco_cp=>sy->time( xco_cp_time=>time_zone->user
                                           )->subtract( iv_hour = 1 iv_minute = 1 iv_second = 1
                                           )->as( xco_cp_time=>format->iso_8601_extended
                                           )->value.

    "------------ Conversions with the CL_ABAP_TIMEFM class ------------

    "Using the CL_ABAP_TIMEFM class, you can perform conversions with external
    "and internal representations of a time, e.g. conversion of a time in a data
    "object of type string to type t and vice versa. Multiple methods are available,
    "of which two are covered in the example. For more information, refer to the
    "class documentation.
    DATA(time4conversion) = CONV t( '123456' ).
    DATA conv_time_str TYPE string.
    DATA conv_time_t TYPE t.

    "Conversion of t (internal) to string (external time format)
    TRY.
        cl_abap_timefm=>conv_time_int_to_ext(
          EXPORTING time_int            = time4conversion
                    without_seconds     = abap_false
                    format_according_to = cl_abap_timefm=>iso "hh:mm:ss
          IMPORTING time_ext            = conv_time_str ).
      CATCH cx_parameter_invalid_range.
    ENDTRY.

    "Conversion of string (external) to t (internal time format)
    TRY.
        cl_abap_timefm=>conv_time_ext_to_int(
          EXPORTING time_ext = conv_time_str
          IMPORTING time_int = conv_time_t ).
      CATCH cx_abap_timefm_invalid.
    ENDTRY.

    out->write( data = utc_time name = `utc_time` ).
    out->write( |\n| ).
    out->write( data = time_w_xco name = `time_w_xco` ).
    out->write( |\n| ).
    out->write( data = hour_extr name = `hour_extr` ).
    out->write( |\n| ).
    out->write( data = minute_extr name = `minute_extr` ).
    out->write( |\n| ).
    out->write( data = second_extr name = `second_extr` ).
    out->write( |\n| ).
    out->write( data = sec_w_xco name = `sec_w_xco` ).
    out->write( |\n| ).
    out->write( data = min_w_xco name = `min_w_xco` ).
    out->write( |\n| ).
    out->write( data = hour_w_xco name = `hour_w_xco` ).
    out->write( |\n| ).
    out->write( data = time_cr4 name = `time_cr4` ).
    out->write( |\n| ).
    out->write( data = hours_from_xco_time name = `hours_from_xco_time` ).
    out->write( |\n| ).
    out->write( data = minutes_from_xco_time name = `minutes_from_xco_time` ).
    out->write( |\n| ).
    out->write( data = seconds_from_xco_time name = `seconds_from_xco_time` ).
    out->write( |\n| ).
    out->write( data = seconds_total name = `seconds_total` ).
    out->write( |\n| ).
    out->write( data = calc_hours name = `calc_hours` ).
    out->write( |\n| ).
    out->write( data = min_calc name = `min_calc` ).
    out->write( |\n| ).
    out->write( data = sec_calc name = `sec_calc` ).
    out->write( |\n| ).
    out->write( data = time_xco_add name = `time_xco_add` ).
    out->write( |\n| ).
    out->write( data = time_xco_subtr name = `time_xco_subtr` ).
    out->write( |\n| ).
    out->write( data = conv_time_str name = `conv_time_str` ).
    out->write( |\n| ).
    "Note: When outputting the data object of type t with the classrun,
    "it is displayed in the format hh:mm:ss in the console for better
    "readability.
    out->write( data = conv_time_t name = `conv_time_t` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Time Stamps of Type utclong` ) ).

    "Retrieving an UTC time stamp using the built-in function utclong_current
    "The return value has the type utclong.
    DATA(ts1) = utclong_current( ).

    "Using XCO
    "In the case of XCO, the return value is of type string.
    "Retrieving a time stamp in the user's time zone (which is the default)
    DATA(ts2) = xco_cp=>sy->moment( xco_cp_time=>time_zone->user
                                  )->as( xco_cp_time=>format->iso_8601_extended
                                  )->value.
    "Current time stamp in UTC
    DATA(ts3) = xco_cp=>sy->moment( xco_cp_time=>time_zone->utc
                                  )->as( xco_cp_time=>format->iso_8601_extended
                                  )->value.
    "Different formatting options
    DATA(ts_xco) = xco_cp=>sy->moment( xco_cp_time=>time_zone->utc ).
    DATA(ts4) = ts_xco->as( xco_cp_time=>format->abap )->value.
    DATA(ts5) = ts_xco->as( xco_cp_time=>format->iso_8601_basic )->value.
    DATA(ts6) = ts_xco->as( xco_cp_time=>format->iso_8601_extended )->value.

    out->write( data = ts1 name = `ts1` ).
    out->write( |\n| ).
    out->write( data = ts2 name = `ts2` ).
    out->write( |\n| ).
    out->write( data = ts3 name = `ts3` ).
    out->write( |\n| ).
    out->write( data = ts4 name = `ts4` ).
    out->write( |\n| ).
    out->write( data = ts5 name = `ts5` ).
    out->write( |\n| ).
    out->write( data = ts6 name = `ts6` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `9) Creating/Modifying a Time Stamp` ) ).

    "--------------------- Creating time stamps --------------------

    DATA ts7 TYPE utclong.
    ts7 = utclong_current( ).
    DATA ts8 TYPE utclong VALUE '2024-11-05 15:30:00'.
    DATA(ts9) = CONV utclong( '2024-11-03 05:30:00' ).

    "Using XCO
    "As above, the return value is of type string. Various methods, formatting options,
    "etc. are available.
    DATA(ts10) = xco_cp_time=>moment( iv_year   = '2024'
                                      iv_month  = '01'
                                      iv_day    = '01'
                                      iv_hour   = '12'
                                      iv_minute = '34'
                                      iv_second = '55'
                                    )->as( xco_cp_time=>format->iso_8601_extended
                                    )->value.

    "--------------------- Modifying time stamps (XCO) --------------------

    "As covered for date and time types, you can modify time stamps using string
    "processing functionalities. They are not covered here. XCO provides, for
    "example, the 'overwrite' method. Optional parameters are available.
    DATA(ts11) = xco_cp_time=>moment( iv_year   = '2024'
                                      iv_month  = '03'
                                      iv_day    = '05'
                                      iv_hour   = '02'
                                      iv_minute = '54'
                                      iv_second = '12' ).

    DATA(ts12) = ts11->overwrite( iv_year = '2025'
                                  iv_month = '07'
                                  iv_day = '15'
                                  iv_hour = '01'
                                )->as( xco_cp_time=>format->iso_8601_extended
                                )->value.

    out->write( data = ts7 name = `ts7` ).
    out->write( |\n| ).
    out->write( data = ts8 name = `ts8` ).
    out->write( |\n| ).
    out->write( data = ts9 name = `ts9` ).
    out->write( |\n| ).
    out->write( data = ts10 name = `ts10` ).
    out->write( |\n| ).
    out->write( data = ts12 name = `ts12` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) Time Stamp Calculations with the Built-In Function utclong_add` ) ).

    "With the built-in function utclong_add, at least one parameter must be specified
    "besides 'val'.
    "Note that there are no parameters for years and months.
    DATA(utc4calc) = CONV utclong( '2024-01-01 15:55:14.1173220' ).

    "Adding one hour
    DATA(ts13) = utclong_add( val = utc4calc
                              hours = 1 ).

    "Subtracting one hour by passing a negative integer value (no separate substract
    "function available)
    DATA(ts14) = utclong_add( val = utc4calc
                              hours = -1 ).

    "Using all parameters
    DATA(ts15) = utclong_add( val = utc4calc
                              days = 1
                              hours = 2
                              minutes = CONV int8( '13' )
                              seconds = CONV decfloat34( '53.12' ) ).

    out->write( data = ts13 name = `ts13` ).
    out->write( |\n| ).
    out->write( data = ts14 name = `ts14` ).
    out->write( |\n| ).
    out->write( data = ts15 name = `ts15` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) Time Stamp Calculations with XCO` ) ).

    "Creating two time stamps with XCO
    DATA(ts_ref1) =  xco_cp_time=>moment( iv_year   = '2024'
                                          iv_month  = '01'
                                          iv_day    = '01'
                                          iv_hour   = '11'
                                          iv_minute = '37'
                                          iv_second = '54' ).

    DATA(ts_ref2) =  xco_cp_time=>moment( iv_year   = '2024'
                                          iv_month  = '02'
                                          iv_day    = '02'
                                          iv_hour   = '10'
                                          iv_minute = '28'
                                          iv_second = '59' ).

    "Additions; various optional parameters are available
    DATA(ts18) = ts_ref1->add( iv_day = 1 iv_month = 2 iv_year = 3
                              )->as( xco_cp_time=>format->iso_8601_extended
                              )->value.

    DATA(ts19) = ts_ref2->add( iv_hour = 1 iv_minute = 2 iv_second = 4
                              )->as( xco_cp_time=>format->iso_8601_extended
                              )->value.

    "Adding an hour to the current time stamp
    DATA(ts20) = xco_cp=>sy->moment( )->add( iv_hour = 1
                                      )->as( xco_cp_time=>format->iso_8601_extended
                                      )->value.

    "Substractions
    DATA(ts21) = ts_ref1->subtract( iv_day = 1 iv_month = 2 iv_year = 3
                                  )->as( xco_cp_time=>format->iso_8601_extended
                                  )->value.

    DATA(ts22) = ts_ref2->subtract( iv_hour = 1 iv_minute = 2 iv_second = 4
                                  )->as( xco_cp_time=>format->iso_8601_extended
                                  )->value.

    "Substracting 1 year/month/day/hour/minute/second from the curernt time stamp
    DATA(ts23) = xco_cp=>sy->moment( )->subtract( iv_day = 1 iv_month = 1 iv_year = 1
                                                  iv_hour = 1 iv_minute = 1 iv_second = 1
                                     )->as( xco_cp_time=>format->iso_8601_extended
                                     )->value.

    "---- Excursion: Defining a time interval and checking if ----
    "---- a time stamp is within that time interval --------------

    "In the following example, a time interval is defined from the current time stamp
    "retrieved with XCO to a specified one. Using the 'contains' method and providing
    "another time stamp reference, it is checked whether that time stamp is contained
    "in the time interval. The return value is of type abap_bool. Using the lower_bound
    "and upper_bound attributes, the low and high values are retrieved.
    DATA(ts_ref3) =  xco_cp_time=>moment( iv_year   = '2030'
                                          iv_month  = '01'
                                          iv_day    = '01'
                                          iv_hour   = '10'
                                          iv_minute = '00'
                                          iv_second = '00' ).

    DATA(ts_ref4) =  xco_cp_time=>moment( iv_year   = '2028'
                                          iv_month  = '01'
                                          iv_day    = '01'
                                          iv_hour   = '11'
                                          iv_minute = '00'
                                          iv_second = '00' ).

    DATA(ts_interval) = xco_cp=>sy->moment( )->interval_to( ts_ref3 ).
    DATA(ts_in) = ts_interval->contains( ts_ref4 ).
    DATA(ts_interval_low) = ts_interval->lower_bound->as( xco_cp_time=>format->iso_8601_extended )->value.
    DATA(ts_interval_high) = ts_interval->upper_bound->as( xco_cp_time=>format->iso_8601_extended )->value.

    out->write( data = ts18 name = `ts18` ).
    out->write( |\n| ).
    out->write( data = ts19 name = `ts19` ).
    out->write( |\n| ).
    out->write( data = ts20 name = `ts20` ).
    out->write( |\n| ).
    out->write( data = ts21 name = `ts21` ).
    out->write( |\n| ).
    out->write( data = ts22 name = `ts22` ).
    out->write( |\n| ).
    out->write( data = ts23 name = `ts23` ).
    out->write( |\n| ).
    out->write( data = ts_in name = `ts_in` ).
    out->write( |\n| ).
    out->write( data = ts_interval_low name = `ts_interval_low` ).
    out->write( |\n| ).
    out->write( data = ts_interval_high name = `ts_interval_high` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `12) Calculating Time Stamp Differences Using the Built-In Function utclong_diff` ) ).

    DATA(ts16) = CONV utclong( '2024-01-01 05:30:00' ).
    DATA(ts17) = CONV utclong( '2024-01-01 06:30:00' ).
    "The return value has the type decfloat34. It contains the exact
    "difference in seconds.

    DATA(ts_diff1) = utclong_diff( high = ts17
                                   low = ts16 ).

    DATA(ts_diff2) = utclong_diff( high = ts16
                                   low = ts17 ).

    out->write( data = ts_diff1 name = `ts_diff1` ).
    out->write( |\n| ).
    out->write( data = ts_diff2 name = `ts_diff2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `13) CONVERT UTCLONG: Time Stamp (utclong) -> Local Date/Time` ) ).

    DATA ts_utc TYPE utclong VALUE '2024-11-03 05:30:00'.

    CONVERT UTCLONG ts_utc
            INTO DATE DATA(date_est)
                 TIME DATA(time_est)
                 TIME ZONE 'EST'.

    out->write( data = date_est name = `date_est` ).
    out->write( |\n| ).
    out->write( data = time_est name = `time_est` ).
    out->write( |\n| ).

    "More optional additions:
    "- FRACTIONAL SECONDS: Getting the fractions of seconds
    "- DAYLIGHT SAVING TIME: Determining if the time stamp is
    "  in a daylight saving time (the example uses time stamp
    "  in August)
    CONVERT UTCLONG CONV utclong( '2024-08-08 09:23:11.7681270' )
            INTO DATE date_est
                 TIME time_est
                 FRACTIONAL SECONDS DATA(sec_est)
                 DAYLIGHT SAVING TIME DATA(dsl_est)
                 TIME ZONE 'EST'.

    out->write( data = date_est name = `date_est` ).
    out->write( |\n| ).
    out->write( data = time_est name = `time_est` ).
    out->write( |\n| ).
    out->write( data = sec_est name = `sec_est` ).
    out->write( |\n| ).
    out->write( data = dsl_est name = `dsl_est` ).
    out->write( |\n| ).

    "If the time zone specified is initial, there is no conversion.
    CONVERT UTCLONG CONV utclong( '2024-08-08 09:23:11.7681270' )
            INTO DATE DATA(date_init)
                 TIME DATA(time_init)
                 TIME ZONE VALUE #( ).

    out->write( data = date_init name = `date_init` ).
    out->write( |\n| ).
    out->write( data = time_init name = `time_init` ).
    out->write( |\n| ).

    "Specifying a non-existent time zone raises a catchable exception.
    TRY.
        CONVERT UTCLONG CONV utclong( '2024-08-08 09:23:11.7681270' )
                INTO DATE DATA(date_nope)
                     TIME DATA(time_nope)
                     TIME ZONE 'NOPE'.
      CATCH cx_sy_conversion_no_date_time INTO DATA(err_conv).
    ENDTRY.

    IF err_conv IS INITIAL.
      out->write( data = date_nope name = `date_nope` ).
      out->write( |\n| ).
      out->write( data = time_nope name = `time_nope` ).
    ELSE.
      out->write( data = err_conv->get_text( ) name = `err_conv->get_text( )` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `14) CONVERT INTO UTCLONG: Local Date/Time -> Time Stamp (utclong)` ) ).

    DATA date2utcl TYPE d VALUE '20240101'.
    DATA time2utcl TYPE t VALUE '112458'.
    DATA utcl TYPE utclong.

    CONVERT DATE date2utcl
            TIME time2utcl
            TIME ZONE 'EST'
            INTO UTCLONG utcl.

    out->write( data = utcl name = `utcl` ).
    out->write( |\n| ).

    "Using optional additions
    "Check the details in the ABAP Keyword Documentation.
    "Specifying 'X' for DAYLIGHT SAVING TIME in the following
    "example raises a catchable exception.
    CONVERT DATE date2utcl
            TIME time2utcl
            FRACTIONAL SECONDS CONV decfloat34( '0.768127' )
            DAYLIGHT SAVING TIME ''
            TIME ZONE 'EST'
            INTO UTCLONG DATA(utcl_inl1).

    out->write( data = utcl_inl1 name = `utcl_inl1` ).
    out->write( |\n| ).


    "There is no time shift in case of an initial time zone specification.
    CONVERT DATE date2utcl
            TIME time2utcl
            TIME ZONE VALUE #( )
            INTO UTCLONG DATA(utcl_inl2).

    out->write( data = utcl_inl2 name = `utcl_inl2` ).
    out->write( |\n| ).

    "Ensure that valid values are passed
    "The following example explores ABAP statements with invalid values
    "that are passed. The valid time stamp, the exceptions raised and
    "the error messages are added to an internal table.
    DATA error_checks TYPE string_table.
    DATA date_test TYPE d.
    DATA time_test TYPE t.
    DATA frac_sec_test TYPE decfloat34.
    DATA dls_test TYPE abap_bool.
    DATA tz_test TYPE string.
    DO 6 TIMES.
      date_test = '20240101'.
      time_test = '112458'.
      frac_sec_test = '0.768127'.
      dls_test = abap_false.
      tz_test = `EST`.

      CASE sy-index.
        WHEN 1.
          "No data object change. The statement below should return a valid time stamp.
        WHEN 2.
          "Invalid date
          date_test = '20249999'.
        WHEN 3.
          "Invalid time
          time_test = '992458'.
        WHEN 4.
          "Invalid fractions of seconds
          frac_sec_test = '1'.
        WHEN 5.
          "Invalid specification for this particular example.
          dls_test = 'X'.
        WHEN 6.
          "Invalid time zone
          dls_test = `NOPE`.
      ENDCASE.

      TRY.
          CONVERT DATE date_test
                  TIME time_test
                  FRACTIONAL SECONDS frac_sec_test
                  DAYLIGHT SAVING TIME dls_test
                  TIME ZONE tz_test
                  INTO UTCLONG DATA(utcl_inl3).
          error_checks = VALUE #( BASE error_checks ( |({ sy-index }) Valid time stamp: { utcl_inl3 }| ) ).
        CATCH cx_root INTO DATA(err).
          error_checks = VALUE #( BASE error_checks
            ( |({ sy-index }) Exception | &&
              |{ replace( val = cl_abap_typedescr=>describe_by_object_ref( err )->absolute_name
                          sub = `\CLASS=`
                          with = `` ) } was raised: | &&
              |{ err->get_text( ) }| ) ).
      ENDTRY.
    ENDDO.

    out->write( data = error_checks name = `error_checks` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `15) CL_ABAP_UTCLONG: Utilities for Time Stamps (utclong)` ) ).

    "Check the class documentation. More methods are available.
    DATA(low_timestamp) = CONV utclong( '2024-01-01 05:30:00' ).
    DATA(high_timestamp) = CONV utclong( '2024-01-03 10:35:12' ).

    "'diff' method: Calculating time differences
    cl_abap_utclong=>diff( EXPORTING high    = high_timestamp
                                     low     = low_timestamp
                           IMPORTING days    = DATA(diff_days)
                                     hours   = DATA(diff_hours)
                                     minutes = DATA(diff_minutes)
                                     seconds = DATA(diff_seconds) ).

    "Converting a time stamp from a character-like format to utclong
    "Creating a time stamp in a string
    DATA(ts_string) = |{ utclong_current( ) TIMESTAMP = ENVIRONMENT TIMEZONE = 'UTC' }|.
    TRY.
        cl_abap_utclong=>read( EXPORTING string   = ts_string
                                         timezone = 'UTC'
                               IMPORTING value    = DATA(utc_ts) ).
      CATCH cx_abap_utclong_invalid.
    ENDTRY.

    out->write( data = diff_days name = `diff_days` ).
    out->write( |\n| ).
    out->write( data = diff_hours name = `diff_hours` ).
    out->write( |\n| ).
    out->write( data = diff_minutes name = `diff_minutes` ).
    out->write( |\n| ).
    out->write( data = diff_seconds name = `diff_seconds` ).
    out->write( |\n| ).
    out->write( data = utc_ts name = `utc_ts` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `16) GET TIME STAMP: Retrieving the Current Time Stamp` ) ).

    "Short form
    DATA ts_short TYPE timestamp.
    GET TIME STAMP FIELD ts_short.

    "Long form
    DATA ts_long TYPE timestampl.
    GET TIME STAMP FIELD ts_long.

    "Note: When declaring the target data object inline,
    "the short form (type timestamp) is automatically used.
    GET TIME STAMP FIELD DATA(ts_inl).

    out->write( data = ts_short name = `ts_short` ).
    out->write( |\n| ).
    out->write( data = ts_long name = `ts_long` ).
    out->write( |\n| ).
    out->write( data = ts_inl name = `ts_inl` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `17) CONVERT TIME STAMP: Time Stamp in Packed Numbers -> Local Date/Time` ) ).

    GET TIME STAMP FIELD DATA(tsf). "Type timestamp
    "Retrieving the time zone of a given user
    TRY.
        DATA(user_tz) = cl_abap_context_info=>get_user_time_zone( ).
      CATCH cx_abap_context_info_error.
    ENDTRY.

    CONVERT TIME STAMP tsf
            TIME ZONE user_tz
            INTO DATE DATA(dat)
                 TIME DATA(tim).

    out->write( data = dat name = `dat` ).
    out->write( |\n| ).
    out->write( data = tim name = `tim` ).
    out->write( |\n| ).

    "Specifying a data object of type timestampl instead of timestamp.
    "In this case, the fractions of seconds in the decimal places are ignored.
    "A time zone is specified for which information is retrieved below.
    CONVERT TIME STAMP CONV timestampl( '20240101081317.81011' )
            TIME ZONE 'EST'
            INTO DATE dat
                 TIME tim.

    out->write( data = dat name = `dat` ).
    out->write( |\n| ).
    out->write( data = tim name = `tim` ).
    out->write( |\n| ).

    "Excursion: Time zone information for EST
    "Regarding the DAYLIGHT SAVING TIME additions used in some snippets with this
    "particular time zone: Among others, the result shows that timezoneisactive
    "is true.
    SELECT SINGLE *
      FROM i_timezone
      WHERE TimeZoneID = 'EST'
      INTO @DATA(tz_info).

    out->write( data = tz_info name = `tz_info` ).
    out->write( |\n| ).

    "Addition DAYLIGHT SAVING TIME (using a time stamp in August)
    CONVERT TIME STAMP CONV timestamp( '20240808112458' )
            TIME ZONE 'EST'
            INTO DATE dat
                 TIME tim
                 DAYLIGHT SAVING TIME DATA(dst).

    out->write( data = dat name = `dat` ).
    out->write( |\n| ).
    out->write( data = tim name = `tim` ).
    out->write( |\n| ).
    out->write( data = dst name = `dst` ).
    out->write( |\n| ).

    "sy-subrc is set. Find more details in the ABAP Keyword Documentation.
    "Specified time zone is invalid -> sy-subrc = 8
    "Note that specifying all targets after INTO is not mandatory.
    CONVERT TIME STAMP CONV timestamp( '20240808112458' )
            TIME ZONE 'NOPE'
            INTO DATE dat.
    IF sy-subrc = 8.
      out->write( `sy-subrc = 8` ).
      out->write( |\n| ).
    ENDIF.

    "Specified time stamp is invalid -> sy-subrc = 12
    CONVERT TIME STAMP CONV timestamp( '20249999112458' )
            TIME ZONE 'NOPE'
            INTO DATE dat.
    IF sy-subrc = 12.
      out->write( `sy-subrc = 12` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `18) CONVERT INTO TIME STAMP: Local Date/Time -> Time Stamp in Packed Numbers` ) ).

    DATA date4conv TYPE d VALUE '20240101'.
    DATA time4conv TYPE t VALUE '112458'.
    DATA ts_conv TYPE timestamp.

    CONVERT DATE date4conv
            TIME time4conv
            INTO TIME STAMP ts_conv
            TIME ZONE 'EST'.

    out->write( data = ts_conv name = `ts_conv` ).
    out->write( |\n| ).

    "Using the long form with type timestampl as target type and
    "setting the daylight saving time explicitly using the
    "optional addition DAYLIGHT SAVING TIME. Find more
    "details in the ABAP Keyword Documentation.
    DATA tsl_conv TYPE timestampl.
    CONVERT DATE CONV d( '20240101' )
            TIME CONV t( '112458' )
            INTO TIME STAMP tsl_conv
            TIME ZONE 'EST'.

    out->write( data = tsl_conv name = `tsl_conv` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `19) CL_ABAP_TSTMP: Calculating and Converting Time Stamps in Packed Numbers` ) ).

    "The following code snippets show a selection of methods available.

    "Creating a time stamp of type timestamp
    GET TIME STAMP FIELD DATA(ts4tstmp).

    "Calculations for time stamps in packed numbers
    "Adding 1 hour
    DATA(ts_add1h) = cl_abap_tstmp=>add( tstmp = ts4tstmp
                                    secs  = 3600 ).

    "Subtracting 2 hours
    DATA(ts_subtr2h) = cl_abap_tstmp=>subtractsecs( tstmp = ts4tstmp
                                             secs  = 7200 ).

    "Type timestampl
    DATA tsl TYPE timestampl.
    GET TIME STAMP FIELD tsl.

    "Converting type timestampl to timestamp
    DATA(long2short) = cl_abap_tstmp=>move_to_short( tsl ).

    "Converting types timestamp/timestampl to UTCLONG
    DATA(ts2utcl) = cl_abap_tstmp=>tstmp2utclong( tsl ).

    "Converting type utclong to timestamp
    DATA(utcl2ts) = cl_abap_tstmp=>utclong2tstmp_short( ts2utcl ).

    "Converting type utclong to timestampl
    DATA(utcl2tsl) = cl_abap_tstmp=>utclong2tstmp( ts2utcl ).

    out->write( data = ts_add1h name = `ts_add1h` ).
    out->write( |\n| ).
    out->write( data = ts_subtr2h name = `ts_subtr2h` ).
    out->write( |\n| ).
    out->write( data = long2short name = `long2short` ).
    out->write( |\n| ).
    out->write( data = ts2utcl name = `ts2utcl` ).
    out->write( |\n| ).
    out->write( data = utcl2ts name = `utcl2ts` ).
    out->write( |\n| ).
    out->write( data = utcl2tsl name = `utcl2tsl` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `20) Excursion: Unix Time Stamps` ) ).
    "Unix time stamp: Seconds passed since 1970-01-01 00:00:00 (UTC).

    "Getting the current UNIX time stamp using XCO
    DATA(unix_tstmp) = xco_cp=>sy->unix_timestamp( )->value.

    "Getting the UNIX time stamp from a custom time stamp
    DATA(unix_custom) = xco_cp_time=>moment( iv_year   = '2024'
                                             iv_month  = '11'
                                             iv_day    = '03'
                                             iv_hour   = '07'
                                             iv_minute = '12'
                                             iv_second = '30'
                                           )->get_unix_timestamp( )->value.

    "Using the unix time stamp with the utclong_add function to calculate
    "the actual date
    DATA(ts_from_unix1) = utclong_add( val = CONV utclong( '1970-01-01 00:00:00' )
                                       seconds = unix_tstmp ).

    DATA(ts_from_unix2) = utclong_add( val = CONV utclong( '1970-01-01 00:00:00' )
                                       seconds = unix_custom ).


    "1704102123 (corresponds to Jan 01 2024 09:42:03, UTC)
    DATA(ts_from_unix3) = utclong_add( val = CONV utclong( '1970-01-01 00:00:00' )
                                       seconds = 1704102123 ).

    out->write( data = unix_tstmp name = `unix_tstmp` ).
    out->write( |\n| ).
    out->write( data = unix_custom name = `unix_custom` ).
    out->write( |\n| ).
    out->write( data = ts_from_unix1 name = `ts_from_unix1` ).
    out->write( |\n| ).
    out->write( data = ts_from_unix2 name = `ts_from_unix2` ).
    out->write( |\n| ).
    out->write( data = ts_from_unix3 name = `ts_from_unix3` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `21) Date, Time, and Time Stamp in String Templates` ) ).

    "DATE: Defining the format of a date
    "The output is just an example and depends on your settings.
    DATA(d_str1) = |The date is { cl_abap_context_info=>get_system_date( ) DATE = USER }.|.
    DATA(d_str2) = |{ cl_abap_context_info=>get_system_date( ) DATE = RAW }|.
    DATA(d_str3) = |{ cl_abap_context_info=>get_system_date( ) DATE = ISO }|.
    DATA(d_str4) = |{ cl_abap_context_info=>get_system_date( ) DATE = ENVIRONMENT }|.

    "TIME: Defining the format of a time
    "The output is just an example and depends on your settings.
    DATA(tm_str1) = |The time is { cl_abap_context_info=>get_system_time( ) TIME = ISO }.|.
    DATA(tm_str2) = |{ cl_abap_context_info=>get_system_time( ) TIME = RAW }|.
    DATA(tm_str3) = |{ cl_abap_context_info=>get_system_time( ) TIME = USER }|.
    DATA(tm_str4) = |{ cl_abap_context_info=>get_system_time( ) TIME = ENVIRONMENT }|.

    "TIMESTAMP: Defining the format of a time stamp
    "The output is just an example and depends on your settings.
    DATA(ts_str1) = |{ utclong_current( ) TIMESTAMP = SPACE }|.
    DATA(ts_str2) = |{ utclong_current( ) TIMESTAMP = ISO }|.
    DATA(ts_str3) = |{ utclong_current( ) TIMESTAMP = USER }|.
    DATA(ts_str4) = |{ utclong_current( ) TIMESTAMP = ENVIRONMENT }|.
    DATA(ts_str5) = |{ utclong_current( ) }|.

    "TIMEZONE: Defining the format of a time stamp using the rules for time zones
    DATA(tz_str1) = |{ utclong_current( ) TIMEZONE = 'UTC' }|.
    DATA(tz_str2) = |{ utclong_current( ) TIMEZONE = 'CET' COUNTRY = 'DE ' }|.
    DATA(tz_str3) = |{ utclong_current( ) TIMEZONE = 'EST' COUNTRY = 'US ' }|.

    out->write( data = d_str1 name = `d_str1` ).
    out->write( |\n| ).
    out->write( data = d_str2 name = `d_str2` ).
    out->write( |\n| ).
    out->write( data = d_str3 name = `d_str3` ).
    out->write( |\n| ).
    out->write( data = d_str4 name = `d_str4` ).
    out->write( |\n| ).
    out->write( data = tm_str1 name = `tm_str1` ).
    out->write( |\n| ).
    out->write( data = tm_str2 name = `tm_str2` ).
    out->write( |\n| ).
    out->write( data = tm_str3 name = `tm_str3` ).
    out->write( |\n| ).
    out->write( data = tm_str4 name = `tm_str4` ).
    out->write( |\n| ).
    out->write( data = ts_str1 name = `ts_str1` ).
    out->write( |\n| ).
    out->write( data = ts_str2 name = `ts_str2` ).
    out->write( |\n| ).
    out->write( data = ts_str3 name = `ts_str3` ).
    out->write( |\n| ).
    out->write( data = ts_str4 name = `ts_str4` ).
    out->write( |\n| ).
    out->write( data = ts_str5 name = `ts_str5` ).
    out->write( |\n| ).
    out->write( data = tz_str1 name = `tz_str1` ).
    out->write( |\n| ).
    out->write( data = tz_str2 name = `tz_str2` ).
    out->write( |\n| ).
    out->write( data = tz_str3 name = `tz_str3` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `22) Excursion: Typed Literals in ABAP SQL` ) ).

    SELECT SINGLE
      FROM i_timezone
      FIELDS *
      WHERE TimeZoneID = char`EST`
      INTO @DATA(wa_typed_literal).

    "Cast with a typed literal to cover a specification true to the
    "actually expected type. In the case of the example, the data type
    "char(6) is expected.
    SELECT SINGLE
      FROM i_timezone
      FIELDS *
      WHERE TimeZoneID = CAST( char`EST` AS CHAR( 6 ) )
      INTO @DATA(wa_typed_literal_cast).

    "Untyped literal
    SELECT SINGLE
      FROM i_timezone
      FIELDS *
      WHERE TimeZoneID = 'EST'
      INTO @DATA(wa_untyped_literal).

    "Various typed literals
    DATA(tmstamp) = CONV timestamp( '20240808112517' ).
    SELECT SINGLE
      FROM i_timezone
      FIELDS
        char`X` AS flag,
        int8`32984723948723` AS int8,
        raw`11` AS raw,
        numc`1234` AS numc,
        utclong`2024-01-01T10:01:02,2` AS utc,
        tims`101507` AS tims,
        curr`173.95` AS curr,
        "Multiple cast expressions splitting a time stamp into date and time parts
        CAST( CAST( div( @tmstamp, 1000000 ) AS CHAR ) AS DATS ) AS date,
        CAST( substring( CAST( @tmstamp AS CHAR ), 9, 6 ) AS TIMS ) AS time,
        'ABAP' AS txt "Untyped literal
      WHERE TimeZoneID = CAST( char`EST` AS CHAR( 6 ) )
      INTO @DATA(wa_some_typed_literals).

    out->write( data = wa_typed_literal name = `wa_typed_literal` ).
    out->write( |\n| ).
    out->write( data = wa_typed_literal_cast name = `wa_typed_literal_cast` ).
    out->write( |\n| ).
    out->write( data = wa_untyped_literal name = `wa_untyped_literal` ).
    out->write( |\n| ).
    out->write( data = wa_some_typed_literals name = `wa_some_typed_literals` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `23) Date and Time Functions in ABAP SQL` ) ).

    "The following demo ABAP SQL SELECT statement selects from a
    "CDS view. The FIELDS list contains many functions related to
    "date, time, and time stamp processing. For more details,
    "see the ABAP Keyword Documentation.
    "Some of the parameters are assigned typed literals.
    DATA da TYPE d VALUE '20240122'.
    DATA ti TYPE t VALUE '123456'.
    DATA utc TYPE utclong VALUE '2024-02-15 05:30:00'.
    DATA tmst TYPE timestamp VALUE '20240808112458'.
    DATA tmstlong TYPE timestampl VALUE '20240101081317.81011'.

    SELECT SINGLE FROM i_timezone
    FIELDS
      "---------------------- Date ----------------------
      "Generic date functions (types d, utclong)
      is_valid( @ti ) AS isvalid, "type t also possible
      "In the following examples in this 'section', d and utclong are possible.
      extract_year( @utc ) AS extr_year,
      extract_month( @da ) AS extr_month,
      extract_day( @utc ) AS extr_day,
      dayname( @da ) AS day_name,
      monthname( @utc ) AS month_name,
      weekday( @utc ) AS week_day,
      days_between( @utc,utclong`2024-02-25 08:14:26` ) AS days_bw,
      add_days( @da,2 ) AS add_days,
      add_months( @utc,3 ) AS add_months,

      "Functions for the type datn
      datn_days_between( datn`20240111`,datn`20240212` ) AS days_datn_bw,
      datn_add_days( datn`20240111`,4 ) AS days_datn_add,
      datn_add_months( datn`20240111`,5 ) AS months_datn_add,

      "Functions for the type dats
      dats_is_valid( dats`20240812` ) AS dats_valid,
      dats_days_between( dats`20240812`,dats`20240817` ) AS days_dats_bw,
      dats_add_days( dats`20240812`,4 ) AS days_dats_add,
      dats_add_months( dats`20240812`,3 ) AS months_dats_add,

      "---------------------- Time ----------------------
      "Generic time functions (types t and utclong)
      is_valid( @ti ) AS time_is_valid, "As above, types d and utclong also possible
      extract_hour( @utc ) AS extr_hour,
      extract_minute( @ti ) AS extr_min,
      extract_second( @utc ) AS extr_sec,

      "Function for the type tims
      tims_is_valid( tims`231256` ) AS tims_is_valid,

      "---------------------- Time Stamp ----------------------
      "Note: The type utclong can be used in the generic functions above.
      "Functions specific to the type utclong
      utcl_current( ) AS utcl_current, "generates a UTC time stamp
      utcl_add_seconds( @utc,5 ) AS sec_add_utc,
      utcl_seconds_between( utclong`2024-02-25 08:14:26`,utclong`2024-02-25 08:15:17` ) AS sec_bw_utc,

      "Functions specific to the type timetamp
      tstmp_is_valid( @tmst ) AS ts_is_valid,
      tstmp_current_utctimestamp( ) AS ts_current,
      "The following two functions have an optional parameter on_error.
      "Check the ABAP Keyword Documentation
      tstmp_seconds_between( tstmp1 = @tmst,
                              tstmp2 = CAST( dec`20240808112517` AS DEC( 15,0 ) ) ) AS sec_bw_ts,
      tstmp_add_seconds( tstmp    = @tmst,
                          seconds  = CAST( dec`10` AS DEC( 15,0 ) ) ) AS sec_add_ts,

      "---------------------- Functions for conversions ----------------------
      "Note: For the following functions, optional parameters are possible.
      "For more details, check the ABAP Keyword Documentation.
      tstmp_to_dats( tstmp = @tmst,
                      tzone =  CAST( char`EST` AS CHAR( 6 ) ) ) AS tstmp_to_dats,
      tstmp_to_tims( tstmp = @tmst,
                      tzone = CAST( char`EST` AS CHAR( 6 ) ) ) AS tstmp_to_tims,
      tstmp_to_dst( tstmp = @tmst,
                    tzone = CAST( char`EST` AS CHAR( 6 ) ) ) AS tstmp_to_dst,
      dats_tims_to_tstmp( date = @da,
                          time = @ti,
                          tzone = CAST( char`EST` AS CHAR( 6 ) ) ) AS dats_tims_to_tstmp,
      tstmpl_to_utcl( tstmpl = @tmstlong ) AS tstmpl_to_utcl,
      tstmpl_from_utcl( utcl = @utc ) AS tstmpl_from_utcl,
      dats_to_datn( dats = dats`20240812` ) AS dats_to_datn,
      dats_from_datn( datn = datn`20240111` ) AS dats_from_datn,
      tims_to_timn( tims = tims`231256` ) AS tims_to_timn,
      tims_from_timn( timn = timn`155432` ) AS tims_from_timn

    WHERE TimeZoneID = char`EST`
    INTO @DATA(wa).

    out->write( data = wa name = `wa` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `24) Excursion: ABAP Stopwatch` ) ).
    "This excursion example demonstrates an ABAP stopwatch. This stopwatch
    "is represented by a local class implemented in the CCIMP include of the
    "class ('Local Types' tab in ADT). With the stopwatch, the elapsed
    "runtime can be stored and ouput. The example is intended to be a
    "playground for exploring time-related functions, and does not claim
    "to be a runtime timer for non-demo use cases.
    "To start the stopwatch, an instance of the stopwatch class must be
    "created. This is done using the 'create' method that returns an
    "instance of the (local) stopwatch class. To start the stopwatch,
    "the 'start' method must be called. To stop it, call the 'stop' method.
    "The elapsed time can then be retrieved using the 'get_time' method.
    "To get the time after starting the stopwatch, you can also directly
    "call the 'get_time' method without explicitly stopping since it
    "includes the stop call.
    "The stopwatch also enables the storage of multiple time intervals. You
    "can use the 'store' method. The resulting time intervals can be
    "retrieved using the 'get_stored_times' method. It returns an internal
    "table that includes the times of each interval since the start and the
    "delta to previous time intervals.

    out->write( |----------- Starting and stopping -----------\n| ).

    "Creating a stopwatch instance
    DATA(stopwatch) = lcl_stopwatch=>create( ).
    "Starting stopwatch
    stopwatch->start( ).

    "Do something
    DO 100000 TIMES.
      ASSERT 1 = 1.
    ENDDO.

    "Stopping stopwatch
    stopwatch->stop( ).
    "Getting the time
    DATA(sw_time1) = stopwatch->get_time( ).
    out->write( sw_time1 ).

    "More stopwatch runs
    stopwatch->start( ).

    DO 1000000 TIMES.
      ASSERT 1 = 1.
    ENDDO.
    stopwatch->stop( ).

    DATA(sw_time2) = stopwatch->get_time( ).
    out->write( sw_time2 ).

    stopwatch->start( ).

    DO 10000000 TIMES.
      ASSERT 1 = 1.
    ENDDO.
    stopwatch->stop( ).

    DATA(sw_time3) = stopwatch->get_time( ).
    out->write( sw_time3 ).

    out->write( |\n--- Testing read performance with secondary table keys ---\n| ).

    "The following example creates two demo internal tables. One without a secondary table
    "key and the other with a secondary table key. Consider a scenario where you have an
    "internal table without a secondary table key, and you want to add a secondary table
    "key later to improve read performance. The tables are populated with a lot of data.
    "Then, in a DO loop, many reads are performed on the internal tables. One example uses
    "a free key for the read, the other uses a secondary table key. Using the stopwatch,
    "the elapsed times are retrieved. There should be a significant delta of the elapsed
    "time, showing that the access using the secondary table key is faster.
    TYPES: BEGIN OF demo_struc,
             idx TYPE i,
             str TYPE string,
             num TYPE i,
           END OF demo_struc.

    DATA itab TYPE HASHED TABLE OF demo_struc WITH UNIQUE KEY idx.
    DATA itab_sec TYPE HASHED TABLE OF demo_struc
                  WITH UNIQUE KEY idx
                  WITH NON-UNIQUE SORTED KEY sk
                       COMPONENTS str num.

    DO 500 TIMES.
      INSERT VALUE #( idx = sy-index
                      str = |INDEX{ sy-index }|
                      num = sy-index ) INTO TABLE itab.
    ENDDO.
    itab_sec = itab.

    stopwatch->start( ).
    DO 500 TIMES.
      "Reading into a data reference variable using using a free key.
      "This free key corresponds to the secondary table key specified
      "for the table in the second example.
      DATA(dref) = REF #( itab[ str = `INDEX250` num = 250 ] ).
    ENDDO.
    stopwatch->stop( ).
    DATA(time_free_key) = stopwatch->get_time( ).
    out->write( |{ time_free_key } (free key)| ).

    stopwatch->start( ).
    DO 500 TIMES.
      "Reading from an internal table using the secondary table key
      dref = REF #( itab_sec[ KEY sk str = `INDEX250` num = 250 ] ).
    ENDDO.
    stopwatch->stop( ).
    DATA(time_secondary_key) = stopwatch->get_time( ).
    out->write( |{ time_secondary_key } (secondary key)| ).

    out->write( |\n----------- Another demo run -----------\n| ).
    "In this demo run, the stopwatch is not explicitly stopped.
    "This is done implicitly when calling the get_time method.

    stopwatch->start( ).

    DO 100000 TIMES.
      ASSERT 1 = 1.
    ENDDO.

    DATA(sw_time4) = stopwatch->get_time( ).
    out->write( sw_time4 ).

    stopwatch->start( ).

    DO 1000000 TIMES.
      ASSERT 1 = 1.
    ENDDO.

    DATA(sw_time5) = stopwatch->get_time( ).
    out->write( sw_time5 ).
    stopwatch->start( ).

    DO 10000000 TIMES.
      ASSERT 1 = 1.
    ENDDO.

    DATA(sw_time6) = stopwatch->get_time( ).
    out->write( sw_time6 ).

    out->write( |\n----------- Storing multiple intervals -----------\n\n| ).
    stopwatch->start( ).

    DO 100000 TIMES.
      ASSERT 1 = 1.
    ENDDO.

    "Storing interval 1
    stopwatch->store( ).

    DO 1000000 TIMES.
      ASSERT 1 = 1.
    ENDDO.

    "Storing interval 2
    stopwatch->store( ).

    DO 10000000 TIMES.
      ASSERT 1 = 1.
    ENDDO.

    "Storing interval 3
    stopwatch->store( ).

    DO 100 TIMES.
      ASSERT 1 = 1.
    ENDDO.

    "Storing interval 4
    stopwatch->store( ).

    DO 10000 TIMES.
      ASSERT 1 = 1.
    ENDDO.

    ""Storing interval 5
    stopwatch->store( ).
    stopwatch->stop( ).

    "Getting stored times
    DATA(stored_times) = stopwatch->get_stored_times( ).
    out->write( stored_times ).
  ENDMETHOD.
ENDCLASS.
