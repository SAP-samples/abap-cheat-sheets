<a name="top"></a>

# Date, Time, and Time Stamp

- [Date, Time, and Time Stamp](#date-time-and-time-stamp)
  - [Data Types for Date, Time and Time Stamp](#data-types-for-date-time-and-time-stamp)
  - [Retrieving the Time Zone](#retrieving-the-time-zone)
  - [Date](#date)
    - [Retrieving and Creating Dates](#retrieving-and-creating-dates)
    - [Validity of Date Fields](#validity-of-date-fields)
    - [Character-Like Access to Date Fields](#character-like-access-to-date-fields)
    - [Numeric Access/Calculations](#numeric-accesscalculations)
    - [CL\_ABAP\_DATFM: Date Conversions](#cl_abap_datfm-date-conversions)
  - [Time](#time)
    - [Retrieving the Current Time](#retrieving-the-current-time)
    - [Accessing Time Values](#accessing-time-values)
    - [Creating Time Values](#creating-time-values)
    - [Performing Time Calculations](#performing-time-calculations)
    - [CL\_ABAP\_TIMEFM: Converting Time Values](#cl_abap_timefm-converting-time-values)
  - [Time Stamps](#time-stamps)
    - [Time Stamps of Type utclong](#time-stamps-of-type-utclong)
      - [Retrieving the Current Time Stamp](#retrieving-the-current-time-stamp)
      - [Creating/Modifying a Time Stamp](#creatingmodifying-a-time-stamp)
      - [Time Stamp Calculations with the Built-In Function utclong\_add](#time-stamp-calculations-with-the-built-in-function-utclong_add)
      - [Time Stamp Calculations with XCO](#time-stamp-calculations-with-xco)
      - [Calculating Time Stamp Differences Using the Built-In Function utclong\_diff](#calculating-time-stamp-differences-using-the-built-in-function-utclong_diff)
      - [CONVERT UTCLONG: Time Stamp (utclong) -\> Local Date/Time](#convert-utclong-time-stamp-utclong---local-datetime)
      - [CONVERT ... INTO UTCLONG: Local Date/Time -\> Time Stamp (utclong)](#convert--into-utclong-local-datetime---time-stamp-utclong)
      - [CL\_ABAP\_UTCLONG: Utilities for Time Stamps (utclong)](#cl_abap_utclong-utilities-for-time-stamps-utclong)
    - [Time Stamps in Packed Numbers (Types timestamp and timestampl)](#time-stamps-in-packed-numbers-types-timestamp-and-timestampl)
      - [GET TIME STAMP: Retrieving the Current Time Stamp](#get-time-stamp-retrieving-the-current-time-stamp)
      - [CONVERT TIME STAMP: Time Stamp in Packed Numbers -\> Local Date/Time](#convert-time-stamp-time-stamp-in-packed-numbers---local-datetime)
      - [CONVERT ... INTO TIME STAMP: Local Date/Time -\> Time Stamp in Packed Numbers](#convert--into-time-stamp-local-datetime---time-stamp-in-packed-numbers)
      - [CL\_ABAP\_TSTMP: Calculating and Converting Time Stamps in Packed Numbers](#cl_abap_tstmp-calculating-and-converting-time-stamps-in-packed-numbers)
    - [Excursion: Unix Time Stamps](#excursion-unix-time-stamps)
  - [Excursions](#excursions)
    - [Date, Time, and Time Stamps in String Templates](#date-time-and-time-stamps-in-string-templates)
    - [Date and Time Functions in ABAP SQL and ABAP CDS](#date-and-time-functions-in-abap-sql-and-abap-cds)
    - [Calendar-Related Information](#calendar-related-information)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)


This ABAP cheat sheet covers options of how to handle and process dates, times, and time stamps in ABAP. Note the different types when working with them in ABAP, such as for calculations, evaluations, or displaying on a user interface. As with most of the ABAP cheat sheets, the focus here is on [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm).

## Data Types for Date, Time and Time Stamp

The main data types for date, time, and time stamps in ABAP are as follows:
- [Built-in ABAP types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_abap_type_glosry.htm): `d`, `t`, `utclong`
- [Built-in DDIC types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_ddic_type_glosry.htm) such as `datn`, `timn`, `utclong` and more. These types are mapped to ABAP types (e.g. `datn` is mapped to `d`, `utclong` is mapped to the identically named ABAP type `utclong`). Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_builtin_types.htm#@@ITOC@@ABENDDIC_BUILTIN_TYPES_6).
  - Note that the built-in DDIC types are used in artifacts such as [DDIC database tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_db_table_glosry.htm) and [CDS entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_entity_glosry.htm), but not in ABAP programs (except for [typed literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentyped_literal_glosry.htm)).
   
    

**Built-in ABAP types**
| Type  | Length | Meaning  | Value Range  | Initial value  | Notes |
|---|---|---|---|---|---|
| `d`  | 8 characters  | For storing a calendar date (i.e. an exact day, week, or month of the Gregorian calendar) in a date field. A valid value has the format `yyyymmdd`.     | Any eight Unicode characters that can be encoded in UCS-2 are valid values. These values must be digits that conform to the calendar rules in the format `yyyymmdd`. yyyy (year): 0001 to 9999; mm (month): 01 to 12; dd (day): 01 to 31  | 00000000  | It is a character-like type and mostly used for input and output of dates. Regarding assignments of data objects with numeric data types and calculations: Valid values are converted to the number of days since 01.01.0001. | 
| `t` |  6 characters	 | For storing a time in a time field. A valid value has the format `hhmmss`.  | Any six Unicode characters that can be encoded in UCS-2 are valid values. These values must represent times in accordance with the 24-hour clock format, specifically `hhmmss`. hh (hours): 00 to 23; mm (minutes): 00 to 59; ss (seconds): 00 to 59.  | 000000  | It is a character-like type and mostly used for input and output of times. Regarding assignments of data objects with numeric data types and calculations: The content of the time field is converted to the number of seconds since 00:00:00. | 
| `utclong`  | 8 byte  | For storing a time stamp (i.e. a combined date/time specification). A time stamp field represents a unique time in UTC reference time (UTC: Coordinated Universal Time, which is the basis for representing worldwide time data).  | Internal 8-byte integer representation of a UTC time stamp exact to 100 nanoseconds, in ISO-8601 notation between 0001-01-01T00:00:00.0000000 and 9999-12-31T23:59:59.9999999  |  0 | Find more details, e.g. on the special initial value, [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenutclong.htm). | 


> [!NOTE]
> - Regarding DDIC types: When saving dates and times in a database, it is recommended that you use the `datn` and `timn` types in your implementations. These types are optimized for their corresponding functions and expressions, offering an advantage over the older `dats` and `tims` types, which require conversion to actual date and time types. If you need UTC time stamps to be stored in the database, the `utclong` DDIC type is recommended.
> - The [DDIC data elements](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_element_glosry.htm) `timestamp` and `timestampl` represent time stamps in [packed numbers](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpacked_number_glosry.htm). They are [released](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreleased_api_glosry.htm), and they are used in several ABAP statements and classes covered in the cheat sheet.
>   - `timestamp`
>     -  Short form; the time stamp is represented precisely to the second with no decimal places
>     -  Format: `yyyymmddhhmmss`
>   - `timestampl`
>     -  Long form; the time stamp is represented precisely to 100 ns with seven decimal places
>     -  Format: `yyyymmddhhmmss.sssssss` (in addition to the short form, the seven decimal places are fractions of a second)
> -  Many code snippets in the cheat sheet include examples that utilize the [XCO library](https://help.sap.com/docs/btp/sap-business-technology-platform/xco-library?version=Cloud), which offers various options for handling dates, times, and time stamps. The cheat sheet includes a selection. Note that, in most cases, the return value of the XCO calls in the snippets is of type `string`. For more detailed information, refer to the class documentation and the [SAP Help Portal](https://help.sap.com/docs/btp/sap-business-technology-platform/xco-library?version=Cloud).
> - In [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm), do not use the date and time-related system fields such as `sy-datum`, `sy-uzeit`, `sy-timlo`, `sy-datlo`, and others, as they relate to system-specific values that are not relevant in the cloud context, in cloud systems. User-related time and date values can be retrieved using the XCO library as shown below.

Example: 

```abap
"The example shows data objects with the date, time and time stamp types
"mentioned above. The example includes various ways of value assignments.

DATA some_date TYPE d.
some_date = '20240101'.

DATA(some_time) = CONV t( '123456' ).

FINAL(utc_ts) = CONV utclong( '2024-01-01 15:30:00' ).

DATA ts_short TYPE timestamp.
ts_short = '20240101082802'.

DATA ts_long TYPE timestampl VALUE '20240101082802.1700020'.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Retrieving the Time Zone

> [!NOTE]
> Regarding time zones: 
> - Find more information on time zones [here (F1 for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensystem_user_time_zones.htm). 
> - In case of [SAP BTP ABAP Environments](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensap_btp_abap_env_glosry.htm), the time zone is set to UTC by default. Find more information about maintaining user-specific language and regional settings in the SAP Fiori Launchpad [here](https://help.sap.com/docs/btp/sap-fiori-launchpad-for-sap-btp-abap-environment/maintaining-your-language-and-regional-settings).
> - CDS view that includes time zone information: `I_TIMEZONE`

```abap
"Retrieving the time zone of a given user
"e.g. UTC
TRY.
    DATA(tz) = cl_abap_context_info=>get_user_time_zone( ).
  CATCH cx_abap_context_info_error.
ENDTRY.

"Using XCO (the reference created without '->value' is used in some XCO method calls,
"as shown in some of the following code snippets)
"User's time zone
DATA(tz_w_xco) = xco_cp_time=>time_zone->user->value.
"UTC
DATA(tz_w_xco_utc) = xco_cp_time=>time_zone->utc->value.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Date

> [!NOTE]
> - [AS ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenas_abap_glosry.htm) always implicitly references the Gregorian calendar. For output purposes, dates can be converted to country-specific calendars. 
> - Regarding assignments of data objects with numeric data types and calculations: Valid values are converted to the number of days since 01.01.0001.  


### Retrieving and Creating Dates

To retrieve the current date, you can, for example, use the `get_system_date` method of the `cl_abap_context_info` class. You can also use the [XCO library](https://help.sap.com/docs/btp/sap-business-technology-platform/xco-library?version=Cloud).

```abap
*&---------------------------------------------------------------------*
*& Retrieving the current date
*&---------------------------------------------------------------------*

"Retrieving the current date with respect to UTC, e.g. 20240101
"The result's base type is the DDIC type dats that is mapped to the
"ABAP type d.
DATA(utc_date) = cl_abap_context_info=>get_system_date( ).

"Using XCO
"Notes:
"- The results of the following chained statements are of type string.
"- With the 'as' method, a given format (available via xco_cp_time=>format)
"  is applied to the time. Several options are available.
"- The 'date' method has an optional importing parameter for specifying the
"  time zone. By default, the user's time zone is used. For the specification,
"  you can use xco_cp_time=>time_zone.

"The following three examples:
"- do not specify the optional importing parameter for the 'date' method. In
"  this case, the user time zone is used implicitly.
"- specify a specific format in the 'as' method.

"e.g. 20250101
DATA(date_abap) = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value.

"e.g. 20250101
DATA(date_basic) = xco_cp=>sy->date( )->as( xco_cp_time=>format->iso_8601_basic )->value.

"e.g. 2025-01-01
DATA(date_ext) = xco_cp=>sy->date( )->as( xco_cp_time=>format->iso_8601_extended )->value.

"Specifying the time zone explicitly in the 'date' method
"The following method call retrieves the current user date.
DATA(date_user_tz) = xco_cp=>sy->date( xco_cp_time=>time_zone->user
                                )->as( xco_cp_time=>format->iso_8601_basic
                                )->value.

"Specifying the UTC time zone
DATA(date_utc_tz) = xco_cp=>sy->date( xco_cp_time=>time_zone->utc
                                )->as( xco_cp_time=>format->iso_8601_basic
                                )->value.

*&---------------------------------------------------------------------*
*& Retrieving the current date values using XCO
*&---------------------------------------------------------------------*

"e.g. 01
DATA(day) = xco_cp=>sy->date( )->day.
"e.g. 01
DATA(month) = xco_cp=>sy->date( )->month.
"e.g. 2024
DATA(year) = xco_cp=>sy->date( )->year.

*&---------------------------------------------------------------------*
*& Creating dates
*&---------------------------------------------------------------------*

DATA date_cr1 TYPE d.
date_cr1 = '20240101'.
DATA date_cr2 TYPE d VALUE '20240202'.
DATA(date_cr3) = CONV d( '20240303' ).

"Using XCO
"The result contains a reference. It can be used for further processing
"with the XCO library.
DATA(xco_date) = xco_cp_time=>date( iv_year = 2024
                                    iv_month = 3
                                    iv_day = 3 ).

"Examples
"2024-03-03
DATA(date_cr4) = xco_date->as( xco_cp_time=>format->iso_8601_extended )->value.
"03
DATA(day_from_date) = xco_date->day.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Validity of Date Fields

- Before accessing date (or time) fields, ensure that the content of these fields is valid to avoid unexpected results, such as incorrect calculations.
- The ABAP runtime framework checks the validity of these fields in various contexts, including lossless assignments and assignments to numeric fields (see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenchar_date_time_fields_validity.htm)):
  - Lossless assignments: If an invalid value is encountered in a source field, an exception will be raised instead of producing the initial value. 
  - Invalid values cannot be converted to time stamps
  - Assignments to numeric fields
- Note that this applies to time fields as well, although it is not explicitly mentioned in the section below.

The following examples show invalid date values:
```abap
"Lossless assignements using the EXACT operator
"Due to the invalid dates, an exception is raised.
TRY.
    DATA(inv_date1) = EXACT d( '20240231' ).
  CATCH cx_sy_conversion_no_date.
ENDTRY.

TRY.
    DATA(inv_date2) = EXACT d( '2024XY31' ).
  CATCH cx_sy_conversion_no_date.
ENDTRY.

"Assignment of an invalid date of type d to type i; the initial value is produced
"Note: In newer ABAP releases, the following statement shows a syntax 
"warning that the value of the literal (intentionally specified 
"here like this) does not match type d. 
DATA(inv_date3) = CONV i( CONV d( '20240231' ) ).
ASSERT inv_date3 = 0.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Character-Like Access to Date Fields

More information on string processing: [String processing cheat sheet](07_String_Processing.md)

```abap
"Since the content is character-like, you can use string processing functionalities
"to access date values. This also applies to time fields of type t.
"Some examples:
"Extracting date values using the string function substring.
DATA some_date TYPE d VALUE '20240102'.
DATA(year_sub) = substring( val = some_date off = 0 len = 4 ). "2024
DATA(month_sub) = substring( val = some_date off = 4 len = 2 ). "01
DATA(day_sub) = substring( val = some_date off = 6 len = 2 ). "02

"Replacing using the string function replace
DATA(year_repl)  = replace( val = some_date off = 0 len = 4 with = `2025` ). "20250102

"Offset and length specifications
"Read access
DATA(off_len_spec_year) = some_date(4). "2024
DATA(off_len_spec_year_2) = some_date+0(4). "2024
DATA(off_len_spec_month) = some_date+4(2). "01
DATA(off_len_spec_day) = some_date+6(2). "02
"Write access
some_date+4(2) = '10'. "20241002

"Excursion: Comparison rules for character-like date types
"The following example specifies an internal table that has a component
"of type d. Using a ranges table and SELECT statements different table
"entries are retrieved.
"Note the comparison rule for type d (and type t): Content is compared
"from left to right, and the first different character determines which
"operand is greater.

TYPES: BEGIN OF str_w_date,
          num  TYPE i,
          date TYPE d,
        END OF str_w_date,
        tab_type_w_date TYPE TABLE OF str_w_date WITH EMPTY KEY.

DATA(date_tab) = VALUE tab_type_w_date(
    ( num = 1 date = '20241017' )
    ( num = 2 date = '20241030' )
    ( num = 3 date = '20241101' )
    ( num = 4 date = '20241105' )
    ( num = 5 date = '20241112' )
    ( num = 6 date = '20241201' )
    ( num = 7 date = '20241215' )
    ( num = 8 date = '20241227' )
    ( num = 9 date = '20241231' )
    ( num = 10 date = '20250101' )
    ( num = 11 date = '20250110' )
    ( num = 12 date = '20250203' ) ).

"Declaring a ranges table
DATA rangestab TYPE RANGE OF d.

"Populating a ranges table using VALUE
"Include sign, including all December dates in the result set and all dates
"earlier than November
rangestab = VALUE #( sign   = 'I'
                     option = 'BT' ( low = '20241201' high = '20241231' )
                     option = 'LT' ( low = '20241101' ) ).

"Using a SELECT statement and the IN addition to retrieve internal table
"content based on the ranges table specifications
SELECT num FROM @date_tab AS dtab
    WHERE date IN @rangestab
    INTO TABLE @DATA(date_result1).
"The result includes: 1, 2, 6, 7, 8, 9

"Exclude sign, excluding all December dates in the result set
rangestab = VALUE #( sign   = 'E'
                     option = 'BT' ( low = '20241201' high = '20241231' ) ).

SELECT num FROM @date_tab AS dtab
    WHERE date IN @rangestab
    INTO TABLE @DATA(date_result2).
"The result includes: 1, 2, 3, 4, 5, 10, 11, 12
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Numeric Access/Calculations

- When converting date fields to numeric values, the type `d` produces an integer representing the number of days since 01.01.0001. 
- This is especially important when using date fields in calculations and converting the values to numeric types. 
- The XCO library offers methods for performing calculations. In the code snippet, examples use the `value` attribute. The return value of the method calls is of type `string`. For more information,  refer to the class documentation.

```abap
DATA date_calc_1 TYPE d VALUE '20240101'.
DATA date_calc_2 TYPE d VALUE '20231227'.
DATA date_calc_3 TYPE d VALUE '20231230'.
DATA date_calc_4 TYPE d VALUE '20240220'.

DATA(days_diff_1) = date_calc_1 - date_calc_2. "5
DATA(days_diff_2) = date_calc_1 - date_calc_3. "2
DATA(days_since_01_01_0001) = CONV i( date_calc_1 ). "738887
"Getting the weekday (1 = Monday, 2 = Tuesday, ...)
DATA(weekday1) = ( 5 + date_calc_1  MOD 7 ) MOD 7 + 1. "1 (Monday)
DATA(weekday2) = ( 5 + date_calc_3  MOD 7 ) MOD 7 + 1. "6 (Saturday)
DATA(date_w_first_day_of_month) =  CONV d( replace( val = `202403020` off = 6 len = 2 with = `01` ) ). "20240301
DATA(date_w_last_day_of_prev_month) = CONV d( date_w_first_day_of_month - 1 ). "20240229

*&---------------------------------------------------------------------*
*& Performing date additions and subtractions using the XCO library
*&---------------------------------------------------------------------*

"Adding days to the current date using the 'add' method
"e.g. 2024-03-16 (if the current date is 2024-03-11)
DATA(xco_date_add_5days) = xco_cp=>sy->date( )->add( iv_day = 5
                                             )->as( xco_cp_time=>format->iso_8601_extended
                                             )->value.

"The 'add' method has various parameters, adding 1 day/month/year
"e.g. 2025-04-12 (if the current date is 2024-03-11)
DATA(xco_date_add_1_mult) = xco_cp=>sy->date( )->add( iv_day = 1 iv_month = 1 iv_year = 1
                                              )->as( xco_cp_time=>format->iso_8601_extended
                                              )->value.

"Addition with a created date
"2024-02-29
DATA(xco_date_add_1day_custom) = xco_cp_time=>date( iv_year = 2024 iv_month = 02 iv_day = 28
                                                  )->add( iv_day = 1
                                                  )->as( xco_cp_time=>format->iso_8601_extended
                                                  )->value.

"Subtracting using the 'subtract' method
"e.g. 2023-02-10 (if the current date is 2024-03-11)
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
  CATCH cx_root.
ENDTRY.

"io_calculation parameter: xco_cp_time=>date_calculation->ultimo,
"Here, the actual last day of the month is considered. If the calculated
"date is invalid, the ultimo is automatically used. In the example, one month is added to
"the date. However, September does not have 31 days, so the result is adjusted to the actual
"last day of September, which is 30 in this case.
"Result: 2024-09-30
TRY.
    DATA(inv_date_b) = xco_cp_time=>date( iv_year  = '2024'
                                          iv_month = '08'
                                          iv_day   = '31'
                                        )->add( iv_month = 1
                                                io_calculation = xco_cp_time=>date_calculation->ultimo
                                        )->as( xco_cp_time=>format->iso_8601_extended
                                        )->value.
  CATCH cx_root.
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### CL_ABAP_DATFM: Date Conversions

```abap
"Using the CL_ABAP_DATFM class, you can perform conversions with external
"and internal representations of a time according to the date format, e.g.
"the conversion of a date in a data object of type string to type d and vice
"versa using a specific date format. Multiple methods are available, of which
"two are covered in the example. For more information, refer to the class
"documentation.
"Values of the date format for the conversion
"'1'  DD.MM.YYYY (Gregorian Date)
"'2'  MM/DD/YYYY (Gregorian Date)
"'3'  MM-DD-YYYY (Gregorian Date)
"'4'  YYYY.MM.DD (Gregorian Date)
"'5'  YYYY/MM/DD (Gregorian Date)
"'6'  YYYY-MM-DD (Gregorian Date, ISO 8601)
"'7'  GYY.MM.DD (Japanese Date)
"'8'  GYY/MM/DD (Japanese Date)
"'9'  GYY-MM-DD (Japanese Date)
"'A'  YYYY/MM/DD (Islamic Date 1)
"'B'  YYYY/MM/DD (Islamic Date 2)
"'C'  YYYY/MM/DD (Iranian Date)

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

"Content of date_tab
"Note: The first entry in the table in your system may be different. The currently 
"active date format is retrieved.
"EXT_DATE      FORMAT    INT_DATE
"02/02/2024    2         20240202
"02.02.2024    1         20240202
"02/02/2024    2         20240202
"02-02-2024    3         20240202
"2024-02-02    6         20240202
"1445/07/22    A         20240202
"1402/11/13    C         20240202
"Note: When outputting the data object with the classrun,
"20240202, which is of type d, is displayed as 2014-02-02 in 
"the console for better readability.

"Getting the currently active date format
DATA(datfm) = cl_abap_datfm=>get_datfm( ).
"Getting the date format of a specific country
DATA(country_datfm) = cl_abap_datfm=>get_country_datfm( 'DE' ).

"Checking whether the date format is valid
DATA(is_valid) = cl_abap_datfm=>check_date_format( '1' ).
ASSERT is_valid = abap_true.

is_valid = cl_abap_datfm=>check_date_format( 'D' ).
ASSERT is_valid = abap_false.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Time
The code snippet below provides examples of time processing, such as retrieving the current time, accessing time values, creating time values, and performing time calculations. You can also utilize the XCO library in different scenarios. Note that the return value in most of the code snippets using XCO is of type `string`.

### Retrieving the Current Time

```abap
*&---------------------------------------------------------------------*
*& Retrieving the current time
*&---------------------------------------------------------------------*

"Retrieving the current time in UTC, e.g. 152450
DATA(utc_time) = cl_abap_context_info=>get_system_time( ).

"Using XCO
"Notes:
"- The results of the following chained statements are of type string.
"- With the 'as' method, a given format (available via xco_cp_time=>format)
"  is applied to the time. Several options are available.
"- The 'time' method has an optional importing parameter for specifying the
"  time zone. By default, the user's time zone is used. For the specification,
"  you can use xco_cp_time=>time_zone.

"The following three examples
"- do not specify the optional importing parameter for the 'time' method. In
"  this case, the user time zone is used implicitly.
"- specify a specific format in the 'as' method.

"e.g. 160907
DATA(time_abap) = xco_cp=>sy->time( )->as( xco_cp_time=>format->abap )->value.

"e.g. 160907
DATA(time_basic) = xco_cp=>sy->time( )->as( xco_cp_time=>format->iso_8601_basic )->value.

"e.g. 16:09:07
DATA(time_ext) = xco_cp=>sy->time( )->as( xco_cp_time=>format->iso_8601_extended )->value.

"Specifying the time zone explicitly in the 'time' method
"The following method call retrieves the current user time.
DATA(time_user_tz) = xco_cp=>sy->time( xco_cp_time=>time_zone->user
                                )->as( xco_cp_time=>format->iso_8601_basic
                                )->value.

"Specifying the UTC time zone
DATA(time_utc_tz) = xco_cp=>sy->time( xco_cp_time=>time_zone->utc
                                )->as( xco_cp_time=>format->iso_8601_basic
                                )->value.
```

### Accessing Time Values

```abap
"Note: As mentioned in a previous section on dates, the access to time fields
"works similar as date fields. As an example, seconds, minutes, and hours are
"extracted from a time field.
DATA some_time TYPE t VALUE '123456'.

DATA(hour_extr) = substring( val = some_time off = 0 len = 2 ). "12
DATA(minute_extr) = substring( val = some_time off = 2 len = 2 ). "34
DATA(second_extr) = substring( val = some_time off = 4 len = 2 ). "56

"Offset and length specifications
"Read access
DATA(off_len_spec_hour) = some_time(2). "12
DATA(off_len_spec_hour_2) = some_time+0(2). "12
DATA(off_len_spec_minutes) = some_time+2(2). "34
DATA(off_len_spec_seconds) = some_time+4(2). "56
"Write access
some_time+4(2) = '10'. "123410

"Retrieving the current seconds, minutes, hours using XCO
"e.g. 59
DATA(sec_w_xco) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->second.
"e.g. 27
DATA(min_w_xco) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->minute.
"e.g. 10
DATA(hr_w_xco) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->hour.
```

### Creating Time Values

```abap
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
"08:34:05
DATA(time_cr4) = xco_time->as( xco_cp_time=>format->iso_8601_extended )->value.
DATA(hours_from_time) = xco_time->hour. "08
DATA(minutes_from_time) = xco_time->minute. "34
DATA(seconds_from_time) = xco_time->second. "05
```

### Performing Time Calculations

```abap
*&---------------------------------------------------------------------*
*& Performing time calculations
*&---------------------------------------------------------------------*

"Retrieving seconds, minutes, and hours from a time value in a data object
"of type t
DATA time4calc TYPE t VALUE '115708'.
DATA(seconds_total) = CONV i( time4calc ). "43028
DATA(calc_hours) = seconds_total DIV 3600. "11
DATA(min_calc) = ( seconds_total - calc_hours * 3600 ) DIV 60. "57
DATA(sec_calc) = ( seconds_total - calc_hours * 3600 ) - min_calc * 60. "8

"Using the XCO library
"See the snippet above in the date section as well as the class documentation.
"Adding
"e.g. 16:27:34 (if the current time is 15:26:33)
DATA(time_xco_add) = xco_cp=>sy->time( xco_cp_time=>time_zone->user
                                      )->add( iv_hour = 1 iv_minute = 1 iv_second = 1
                                      )->as( xco_cp_time=>format->iso_8601_extended
                                      )->value.
"Subtracting
"e.g. 14:27:45 (if the current time is 15:28:46)
DATA(time_xco_subtr) = xco_cp=>sy->time( xco_cp_time=>time_zone->user
                                       )->subtract( iv_hour = 1 iv_minute = 1 iv_second = 1
                                       )->as( xco_cp_time=>format->iso_8601_extended
                                       )->value.

*&---------------------------------------------------------------------*
*& Calculating the time delta between two time values
*&---------------------------------------------------------------------*

DATA: time1 TYPE t VALUE '210000',
      time2 TYPE t VALUE '040000'.

"Calculating the time difference by subtracting
"Note that the resulting data object is of type i.
"The time values are automatically converted to type i.
"Following the conversion rule of type t to i, the resulting value
"is calculated as follows: format hhmmss of type t -> integer values
"as a result of the calculation hh * 3600 + mm * 60 + ss.

"-61200
DATA(time_diff) = time2 - time1.

"75600
DATA(time1_conv2i) = CONV i( time1 ).
ASSERT time1_conv2i = ( 21 * 3600 ) + ( 00 * 60 ) + 00.

"14400
DATA(time2_conv2i) = CONV i( time2 ).
ASSERT time2_conv2i = ( 04 * 3600 ) + ( 00 * 60 ) + 00.
ASSERT time2_conv2i - time1_conv2i = time_diff.

*&----------------------------------------------------------------------------------*
*& Calculating the total values of the time difference in seconds, minutes and hours
*&----------------------------------------------------------------------------------*

"The MOD operator is used and works in a way that the positive remainder
"of the division of the left operand by the right is returned. Therefore,
"it is irrelevant whether the time difference value is either positive or
"negative.
"The value 86400 is used as right operand, representing the number of seconds
"per day/24 hours.
"25200
DATA(time_diff_seconds_total) = ( time2 - time1 ) MOD 86400.

"120.05
DATA(time_diff_minutes_total) = CONV decfloat34( ( ( time2 - time1 ) MOD 86400 ) / 60 ).

"2.000833333333333333333333333333333
DATA(time_diff_hours_total) = CONV decfloat34( ( ( ( time2 - time1 ) MOD 86400 ) / 3600 ) ).

"---- Representing the time difference in a data object of type t ----
DATA diff_time TYPE t.

DATA(diff) = ( time2 - time1 ) MOD 86400.

"The following calculations use the DIV operator
"This operator returns the integer part of the division of the left operand
"by the right
DATA(hours) = diff DIV 3600.
diff = diff MOD 3600.
DATA(minutes) = diff DIV 60.
DATA(seconds) = diff MOD 60.

"diff_time: '070000'
diff_time(2) = hours.
diff_time+2(2) = minutes.
diff_time+4(2) = seconds.

"More examples
time1 = '225958'.
time2 = '010001'.

"diff_time: '020003'
diff_time(2) = ( ( time2 - time1 ) MOD 86400 ) DIV 3600.
diff_time+2(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) DIV 60.
diff_time+4(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) MOD 60.

time1 = '010001'.
time2 = '225958'.

"diff_time: '215957'
diff_time(2) = ( ( time2 - time1 ) MOD 86400 ) DIV 3600.
diff_time+2(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) DIV 60.
diff_time+4(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) MOD 60.

"diff_time: '154342'
time1 = '132415'.
time2 = '050757'.
diff_time(2) = ( ( time2 - time1 ) MOD 86400 ) DIV 3600.
diff_time+2(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) DIV 60.
diff_time+4(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) MOD 60.

time1 = '050757'.
time2 = '132415'.

"diff_time: '081618'
diff_time(2) = ( ( time2 - time1 ) MOD 86400 ) DIV 3600.
diff_time+2(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) DIV 60.
diff_time+4(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) MOD 60.

"---- Excursion: / and DIV operators (and why DIV is used above) ----
time1 = '132415'.
time2 = '050757'.

"Compare the result of the following statements that use different operators
"15
DATA(hours_w_div) = ( ( time2 - time1 ) MOD 86400 ) DIV 3600.

"16 (result of type i, result is rounded)
DATA(hours_no_div) = ( ( time2 - time1 ) MOD 86400 ) / 3600.
"15.72833333333333333333333333333333
DATA(hours_no_div_dec) = CONV decfloat34( ( ( time2 - time1 ) MOD 86400 ) / 3600 ).
```

### CL_ABAP_TIMEFM: Converting Time Values

```abap
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
      IMPORTING time_ext            = conv_time_str ). "12:34:56
  CATCH cx_parameter_invalid_range.
ENDTRY.

"Conversion of string (external) to t (internal time format)
TRY.
    cl_abap_timefm=>conv_time_ext_to_int(
      EXPORTING time_ext            = conv_time_str
      IMPORTING time_int            = conv_time_t ).    "123456
  CATCH cx_abap_timefm_invalid.
ENDTRY.                                       
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Time Stamps

- Time stamps specify a combined date and time to determine a particular point in time. 
- They are precise to fractions of seconds. Note that the UTC reference time (Coordinated Universal Time, which is the basis for representing worldwide time data) is implicitly referenced.
- Time stamps of the ABAP runtime framework are POSIX time stamps, independent of time zone. Each day consists of 86400 seconds, and leap seconds are not supported.   
- In ABAP, there are two ways in which time stamps can be handled:
  - Time stamps of type `utclong` (which is recommended)
  - Time stamps in packed numbers (DDIC types `timestamp` and `timestampl`)
- More information: [Time Stamps](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentime_stamps.htm)

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Time Stamps of Type utclong
#### Retrieving the Current Time Stamp

More information: [`utclong_current`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenutclong_current.htm)

```abap
"Retrieving an UTC time stamp using the built-in function utclong_current
"The return value has the type utclong.
"e.g. 2024-01-01 15:45:46.2695940
DATA(ts1) = utclong_current( ).

"Using XCO
"Notes:
"- The results of the following chained statements are of type string.
"- With the 'as' method, a given format (available via xco_cp_time=>format)
"  is applied to the time. Several options are available.
"- The 'moment' method has an optional importing parameter for specifying the
"  time zone. By default, the user's time zone is used. For the specification,
"  you can use xco_cp_time=>time_zone.

"The following three examples
"- do not specify the optional importing parameter for the 'moment' method. In
"  this case, the user time zone is used implicitly.
"- specify a specific format in the 'as' method.

"e.g. 20250101162319
DATA(ts_abap) = xco_cp=>sy->moment( )->as( xco_cp_time=>format->abap )->value.

"e.g. 20250310T162320
DATA(ts_basic) = xco_cp=>sy->moment( )->as( xco_cp_time=>format->iso_8601_basic )->value.

"e.g. 2025-01-01T16:23:20
DATA(ts_ext) = xco_cp=>sy->moment( )->as( xco_cp_time=>format->iso_8601_extended )->value.

"Specifying the time zone explicitly in the 'moment' method
"The following method call retrieves the current user time stamp.
DATA(ts_user_tz) = xco_cp=>sy->moment( xco_cp_time=>time_zone->user
                                )->as( xco_cp_time=>format->iso_8601_basic
                                )->value.

"Specifying the UTC time zone
DATA(ts_utc_tz) = xco_cp=>sy->moment( xco_cp_time=>time_zone->utc
                                )->as( xco_cp_time=>format->iso_8601_basic
                                )->value.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Creating/Modifying a Time Stamp

```abap
*&---------------------------------------------------------------------*
*& Creating time stamps
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& Modifying time stamps (XCO)
*&---------------------------------------------------------------------*

"As covered for date and time types, you can modify time stamps using string 
"processing functionalities. They are not covered here. XCO provides, for 
"example, the 'overwrite' method. Optional parameters are available.
DATA(ts11) = xco_cp_time=>moment( iv_year   = '2024'
                                  iv_month  = '03'
                                  iv_day    = '05'
                                  iv_hour   = '02'
                                  iv_minute = '54'
                                  iv_second = '12' ).

"2025-07-15T01:54:12
DATA(ts12) = ts11->overwrite( iv_year = '2025' 
                              iv_month = '07' 
                              iv_day = '15' 
                              iv_hour = '01'
                            )->as( xco_cp_time=>format->iso_8601_extended 
                            )->value.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Time Stamp Calculations with the Built-In Function utclong_add

More information: [`utclong_add`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenutclong_add.htm)

```abap
"With the built-in function utclong_add, at least one parameter must be specified
"besides 'val'.
"Note that there are no parameters for years and months.
DATA(utc4calc) = CONV utclong( '2024-01-01 15:55:14.1173220' ).

"Adding one hour
"e.g. 2024-01-01 16:55:14.1173220
DATA(ts13) = utclong_add( val = utc4calc
                          hours = 1 ).

"Subtracting one hour by passing a negative integer value (no separate substract
"function available)
"e.g. 2024-01-01 14:55:14.1173220
DATA(ts14) = utclong_add( val = utc4calc
                          hours = -1 ).

"Using all parameters
"e.g. 2024-01-02 18:09:07.2373220
DATA(ts15) = utclong_add( val = utc4calc
                          days = 1 "type i
                          hours = 2 "type i
                          minutes = CONV int8( '13' )
                          seconds = CONV decfloat34( '53.12' ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Time Stamp Calculations with XCO

```abap
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
"2027-03-02T11:37:54
DATA(ts18) = ts_ref1->add( iv_day = 1 iv_month = 2 iv_year = 3
                          )->as( xco_cp_time=>format->iso_8601_extended
                          )->value.

"2024-02-02T11:31:03 (note the minute value as a consequence of the value
"passed for second)
DATA(ts19) = ts_ref2->add( iv_hour = 1 iv_minute = 2 iv_second = 4
                          )->as( xco_cp_time=>format->iso_8601_extended
                          )->value.

"Adding an hour to the current time stamp
"Note that you can pass time zone specifications for the moment method such
"as xco_cp_time=>time_zone->user.
DATA(ts20) = xco_cp=>sy->moment( )->add( iv_hour = 1
                               )->as( xco_cp_time=>format->iso_8601_extended
                               )->value.

"Substractions
"2020-10-31T11:37:54
DATA(ts21) = ts_ref1->subtract( iv_day = 1 iv_month = 2 iv_year = 3
                              )->as( xco_cp_time=>format->iso_8601_extended
                              )->value.

"2024-02-02T09:26:55
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
DATA(ts_in) = ts_interval->contains( ts_ref4 ). "If it is contained, the value is 'X'.
DATA(ts_interval_low) = ts_interval->lower_bound->as( xco_cp_time=>format->iso_8601_extended )->value.
DATA(ts_interval_high) = ts_interval->upper_bound->as( xco_cp_time=>format->iso_8601_extended )->value.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Calculating Time Stamp Differences Using the Built-In Function utclong_diff

More information: [`utclong_diff`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenutclong_diff.htm)

```abap
DATA(ts16) = CONV utclong( '2024-01-01 05:30:00' ).
DATA(ts17) = CONV utclong( '2024-01-01 06:30:00' ).
"The return value has the type decfloat34. It contains the exact difference in seconds.

DATA(ts_diff1) = utclong_diff( high = ts17
                               low = ts16 ). "3600

DATA(ts_diff2) = utclong_diff( high = ts16
                               low = ts17 ). "-3600
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### CONVERT UTCLONG: Time Stamp (utclong) -> Local Date/Time

More information: [`CONVERT UTCLONG`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconvert_utclong.htm)

```abap
DATA ts_utc TYPE utclong VALUE '2024-11-03 05:30:00'.

CONVERT UTCLONG ts_utc
        INTO DATE DATA(date_utc) "20241103
             TIME DATA(time_utc) "013000
             TIME ZONE 'EST'.

"More optional additions:
"- FRACTIONAL SECONDS: Getting the fractions of seconds
"- DAYLIGHT SAVING TIME: Determining if the time stamp is
"  in a daylight saving time (the example uses time stamp 
"  in August)
CONVERT UTCLONG CONV utclong( '2024-08-08 09:23:11.7681270' )
        INTO DATE date_utc "20240808
             TIME time_utc "052311
             FRACTIONAL SECONDS DATA(fract_sec_utc) "0.768127
             DAYLIGHT SAVING TIME DATA(dsl_utc) "X
             TIME ZONE 'EST'.

"If the time zone specified is initial, there is no conversion.
CONVERT UTCLONG CONV utclong( '2024-08-08 09:23:11.7681270' )
        INTO DATE date_utc "20240808
             TIME time_utc "092311
             TIME ZONE VALUE #( ).

"Specifying a non-existent time zone raises a catchable exception.
TRY.
    CONVERT UTCLONG CONV utclong( '2024-08-08 09:23:11.7681270' )
            INTO DATE date_utc
                 TIME time_utc
                 TIME ZONE 'NOPE'.
  CATCH cx_sy_conversion_no_date_time.
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### CONVERT ... INTO UTCLONG: Local Date/Time -> Time Stamp (utclong)

More information: [`CONVERT ... INTO UTCLONG`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconvert_utclong.htm)

```abap
DATA date2utcl TYPE d VALUE '20240101'.
DATA time2utcl TYPE t VALUE '112458'.
DATA utcl TYPE utclong.

"2024-01-01 16:24:58.0000000
CONVERT DATE date2utcl
        TIME time2utcl
        TIME ZONE 'EST'
        INTO UTCLONG utcl.

"Using optional additions
"Check the details in the ABAP Keyword Documentation.
"Specifying 'X' for DAYLIGHT SAVING TIME in the following
"example raises a catchable exception (CX_SY_CONVERSION_NO_DATE_TIME).
"2024-01-01 16:24:58.7681270
CONVERT DATE date2utcl
        TIME time2utcl
        FRACTIONAL SECONDS CONV decfloat34( '0.768127' )
        DAYLIGHT SAVING TIME ''
        TIME ZONE 'EST'
        INTO UTCLONG DATA(utcl_inl1).

"There is no time shift in case of an initial time zone specification.
"2024-01-01 11:24:58.0000000
CONVERT DATE date2utcl
        TIME time2utcl
        TIME ZONE VALUE #( )
        INTO UTCLONG DATA(utcl_inl2).

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

  "Note: In newer ABAP releases, some of the following statements 
  "show a syntax warning. The values of some literals (intentionally 
  "specified here like this) are not admissable values for the target
  "types. 
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

*Content of error_checks:                                                                                                                                                              
*(1) Valid time stamp: 2024-01-01 16:24:58.7681270                                                                                                                             
*(2) Exception CX_SY_CONVERSION_NO_DATE was raised: The '20249999 argument cannot be interpreted as a date                                                                     
*(3) Exception CX_SY_CONVERSION_NO_TIME was raised: The '992458' argument cannot be interpreted as a time                                                                      
*(4) Exception CX_SY_CONVERSION_NO_DATE_TIME was raised: The seconds fractions '1' must be in the range 0.0000000 to 0.9999999 (a maximum of seven decimal places).            
*(5) Exception CX_SY_CONVERSION_NO_DATE_TIME was raised: A combination of date '20240101', time '112458', time zone 'EST', and daylight savings time = 'X' is not a valid time.
*(6) Exception CX_PARAMETER_INVALID_RANGE was raised: Parameter has invalid value: Parameter DAYLIGHT SAVING TIME has invalid value N.            
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


#### CL_ABAP_UTCLONG: Utilities for Time Stamps (utclong)

```abap
"Check the class documentation. More methods are available.

DATA(low_timestamp) = CONV utclong( '2024-01-01 05:30:00' ).
DATA(high_timestamp) = CONV utclong( '2024-01-03 10:35:12' ).

"'diff' method: Calculating time differences
cl_abap_utclong=>diff( EXPORTING high     = high_timestamp
                                 low      = low_timestamp
                       IMPORTING days    = DATA(diff_days) "2
                                 hours   = DATA(diff_hours) "5
                                 minutes = DATA(diff_minutes) "5
                                 seconds = DATA(diff_seconds) ). "12

"Converting a time stamp from a character-like format to utclong
"Creating a time stamp in a string
DATA(ts_string) = |{ utclong_current( ) TIMESTAMP = ENVIRONMENT TIMEZONE = 'UTC' }|.
TRY.
    cl_abap_utclong=>read( EXPORTING string   = ts_string
                                     timezone = 'UTC'
                           IMPORTING value    = DATA(utc_ts) ).
    CATCH cx_abap_utclong_invalid.
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Time Stamps in Packed Numbers (Types timestamp and timestampl)

This section deals with time stamps in packed numbers (types `timestamp` and `timestampl`). Note that only a few ABAP statements can deal with these types. Most other statements just interpret the types as numbers. 

#### GET TIME STAMP: Retrieving the Current Time Stamp

More information: [`GET TIME STAMP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapget_time-stamp.htm)

```abap
"Short form
DATA ts_short TYPE timestamp.
"e.g. 20240101081317
GET TIME STAMP FIELD ts_short.

"Long form
DATA ts_long TYPE timestampl.
"e.g. 20240101081317.81011
GET TIME STAMP FIELD ts_long.

"Note: When declaring the target data object inline,
"the short form (type timestamp) is automatically used.
"e.g. 20240101081317
GET TIME STAMP FIELD DATA(ts_inl).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### CONVERT TIME STAMP: Time Stamp in Packed Numbers -> Local Date/Time

More information: [`CONVERT TIME STAMP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconvert_time-stamp.htm)

```abap
GET TIME STAMP FIELD DATA(tsf). "type timestamp
"Retrieving the time zone of a given user
TRY.
    DATA(user_tz) = cl_abap_context_info=>get_user_time_zone( ).
  CATCH cx_abap_context_info_error.
ENDTRY.

CONVERT TIME STAMP tsf
        TIME ZONE user_tz
        INTO DATE DATA(dat) "type d, e.g. 20240101
             TIME DATA(tim). "type t, e.g. 082152

"Specifying a data object of type timestampl instead of timestamp.
"In this case, the fractions of seconds in the decimal places are ignored.
"A time zone is specified for which information is retrieved below.
CONVERT TIME STAMP CONV timestampl( '20240101081317.81011' )
        TIME ZONE 'EST'
        INTO DATE dat "20240101
             TIME tim. "031317

"Excursion: Time zone information for EST
"Regarding the DAYLIGHT SAVING TIME additions used in some snippets with this 
"particular time zone: Among others, the result shows that timezoneisactive 
"is true.
SELECT SINGLE *
  FROM i_timezone
  WHERE TimeZoneID = 'EST'
  INTO @DATA(tz_info).

"Addition DAYLIGHT SAVING TIME (using a time stamp in August)
CONVERT TIME STAMP CONV timestamp( '20240808112458' )
        TIME ZONE 'EST'
        INTO DATE dat
            TIME tim
            DAYLIGHT SAVING TIME DATA(dst).
ASSERT dst = abap_true.

"sy-subrc is set. Find more details in the ABAP Keyword Documentation.
"Specified time zone is invalid -> sy-subrc = 8
"Note that specifying all targets after INTO is not mandatory.
CONVERT TIME STAMP CONV timestamp( '20240808112458' )
        TIME ZONE 'NOPE'
        INTO DATE dat.
ASSERT sy-subrc = 8.

"Specified time stamp is invalid -> sy-subrc = 12
CONVERT TIME STAMP CONV timestamp( '20249999112458' )
        TIME ZONE 'EST'
        INTO DATE dat.
ASSERT sy-subrc = 12.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### CONVERT ... INTO TIME STAMP: Local Date/Time -> Time Stamp in Packed Numbers

More information: [`CONVERT ... INTO TIME STAMP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconvert_date_time-stamp.htm)

```abap
DATA date4conv TYPE d VALUE '20240101'.
DATA time4conv TYPE t VALUE '112458'.
DATA ts_conv TYPE timestamp.

"20240101162458
CONVERT DATE date4conv
        TIME time4conv
        INTO TIME STAMP ts_conv
        TIME ZONE 'EST'.

"Using the long form with type timestampl as target type.
"20240808152458.0
DATA tsl_conv TYPE timestampl.
CONVERT DATE CONV d( '20240101' )
        TIME CONV t( '112458' )        
        INTO TIME STAMP tsl_conv
        TIME ZONE 'EST'.

"Note that you can optionally specify the DAYLIGHT SAVING TIME addition.         
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


#### CL_ABAP_TSTMP: Calculating and Converting Time Stamps in Packed Numbers

More information: Class documentation and [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencl_abap_tstmp.htm)


```abap
"The following code snippets show a selection of methods available.

"Creating a time stamp of type timestamp
"e.g. 20240101131220
GET TIME STAMP FIELD DATA(tsa).

"Calculations for time stamps in packed numbers
"Adding 1 hour
"e.g. 20240101141220.0000000
DATA(tsb) = cl_abap_tstmp=>add( tstmp = tsa
                                secs  = 3600 ).

"Subtracting 2 hours
"e.g. 20240101111220.0000000
DATA(tsc) = cl_abap_tstmp=>subtractsecs( tstmp = tsa
                                         secs  = 7200 ).

"Type timestampl
DATA tsl TYPE timestampl.
"e.g. 20240101131701.3309040
GET TIME STAMP FIELD tsl.

"Converting type timestampl to timestamp
"e.g. 20240101131701
DATA(tsd) = cl_abap_tstmp=>move_to_short( tsl ).

"Converting types timestamp/timestampl to UTCLONG
"e.g. 2024-01-01 13:19:23.8622560
DATA(ts2utcl) = cl_abap_tstmp=>tstmp2utclong( tsl ).

"Converting type utclong to timestamp
"e.g. 20240101132231
DATA(utcl2ts) = cl_abap_tstmp=>utclong2tstmp_short( ts2utcl ).

"Converting type utclong to timestampl
"e.g. 20240101132231.0667200
DATA(utcl2tsl) = cl_abap_tstmp=>utclong2tstmp( ts2utcl ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Excursion: Unix Time Stamps

Unix time stamp: Seconds passed since 1970-01-01 00:00:00 (UTC).

```abap
"Getting the current UNIX time stamp using XCO
DATA(unix_tstmp) = xco_cp=>sy->unix_timestamp( )->value.

"Getting the UNIX time stamp from a custom time stamp
"1730617950
DATA(unix_custom) = xco_cp_time=>moment( iv_year   = '2024'
                                         iv_month  = '11'
                                         iv_day    = '03'
                                         iv_hour   = '07'
                                         iv_minute = '12'
                                         iv_second = '30'
                                       )->get_unix_timestamp( )->value.

"Using the unix time stamp with the utclong_add function to calculate
"the actual date
"2024-11-03 07:12:30.0000000
DATA(ts_from_unix1) = utclong_add( val = CONV utclong( '1970-01-01 00:00:00' )
                                   seconds = 1730617950 ).

"1704102123 (corresponds to Jan 01 2024 09:42:03, UTC)
"2024-01-01 09:42:03.0000000
DATA(ts_from_unix2) = utclong_add( val = CONV utclong( '1970-01-01 00:00:00' )
                                   seconds = 1704102123 ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions

### Date, Time, and Time Stamps in String Templates

More information: [String Processing cheat sheet](07_String_Processing.md#string-templates) and the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_templates.htm)

The following examples show dates, times, and time stamps used as embedded expressions in string templates. Formatting options are available.

```abap
"DATE: Defining the format of a date
"The output is just an example and depends on your settings.
DATA(d_str) = |The date is { cl_abap_context_info=>get_system_date( ) DATE = USER }.|. "The date is 01/01/2024.
d_str = |{ cl_abap_context_info=>get_system_date( ) DATE = RAW }|. "20240101
d_str = |{ cl_abap_context_info=>get_system_date( ) DATE = ISO }|. "2024-01-01
d_str = |{ cl_abap_context_info=>get_system_date( ) DATE = ENVIRONMENT }|. "01/01/2024

"TIME: Defining the format of a time 
"The output is just an example and depends on your settings.
DATA(tm_str) = |The time is { cl_abap_context_info=>get_system_time( ) TIME = ISO }.|. "The time is 14:37:24.
tm_str = |{ cl_abap_context_info=>get_system_time( ) TIME = RAW }|. "143724
tm_str = |{ cl_abap_context_info=>get_system_time( ) TIME = USER }|. "14:37:24
tm_str = |{ cl_abap_context_info=>get_system_time( ) TIME = ENVIRONMENT }|. "14:37:24

"TIMESTAMP: Defining the format of a time stamp
"The output is just an example and depends on your settings.
DATA(ts_str) = |{ utclong_current( ) TIMESTAMP = SPACE }|. "2024-01-01 14:39:50.4069170
ts_str = |{ utclong_current( ) TIMESTAMP = ISO }|. "2024-01-01T14:39:50,4071110
ts_str = |{ utclong_current( ) TIMESTAMP = USER }|. "01/01/2024 14:39:50.4072010
ts_str = |{ utclong_current( ) TIMESTAMP = ENVIRONMENT }|. "01/01/2024 14:39:50.4073230
ts_str = |{ utclong_current( ) }|. "2024-01-01 14:39:50.4074060 

"TIMEZONE: Defining the format of a time stamp using the rules for time zones
DATA(tz_str) = |{ utclong_current( ) TIMEZONE = 'UTC' }|. "2024-12-30 14:43:20.6534640
tz_str = |{ utclong_current( ) TIMEZONE = 'CET' COUNTRY = 'DE ' }|. "30.12.2024 15:43:20,6536320
tz_str = |{ utclong_current( ) TIMEZONE = 'EST' COUNTRY = 'US ' }|. "12/30/2024 09:43:20.6889180 AM
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Date and Time Functions in ABAP SQL and ABAP CDS

> [!NOTE]
> - Date and time functions are available for both [ABAP SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_date_time_functions.htm) and [ABAP CDS](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_date_time_functions_v2.htm). They have the same names. See the [overview](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_date_time_functions.htm) to find out which functions are available. The followig code snippet uses ABAP SQL.
> - The following code snippets use [typed literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentyped_literal_glosry.htm) to have self-contained examples. For more information, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_sql_typed_literals.htm) and the [Typed Literals in ABAP SQL](/16_Data_Types_and_Objects.md#typed-literals-in-abap-sql) section of the *Data Types and Data Objects* cheat sheet.

```abap
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
  is_valid( @ti ) AS isvalid, "type t also possible; 1
  "In the following examples in this 'section', d and utclong are possible.
  extract_year( @utc ) AS extr_year, "2024
  extract_month( @da ) AS extr_month, "1
  extract_day( @utc ) AS extr_day, "15
  dayname( @da ) AS day_name, "Monday
  monthname( @utc ) AS month_name, "February
  weekday( @utc ) AS week_day, "3
  days_between( @utc,utclong`2024-02-25 08:14:26` ) AS days_bw, "10
  add_days( @da,2 ) AS add_days, "20240124
  add_months( @utc,3 ) AS add_months, "2024-05-15 05:30:00.0000000

  "Functions for the type datn
  datn_days_between( datn`20240111`,datn`20240212` ) AS days_datn_bw, "32
  datn_add_days( datn`20240111`,4 ) AS days_datn_add, "20240115
  datn_add_months( datn`20240111`,5 ) AS months_datn_add, "20240611

  "Functions for the type dats
  dats_is_valid( dats`20240812` ) AS dats_valid, "1
  dats_days_between( dats`20240812`,dats`20240817` ) AS days_dats_bw, "5
  dats_add_days( dats`20240812`,4 ) AS days_dats_add, "20240816
  dats_add_months( dats`20240812`,3 ) AS months_dats_add, "20241112

  "---------------------- Time ----------------------
  "Generic time functions (types t and utclong)
  is_valid( @ti ) AS time_is_valid, "As above, types d and utclong also possible; 1
  extract_hour( @utc ) AS extr_hour, "5
  extract_minute( @ti ) AS extr_min, "34
  extract_second( @utc ) AS extr_sec, "0

  "Function for the type tims
  tims_is_valid( tims`231256` ) AS tims_is_valid, "1

  "---------------------- Time Stamp ----------------------
  "Note: The type utclong can be used in the generic functions above.
  "Functions specific to the type utclong
  utcl_current( ) AS utcl_current, "generates a UTC time stamp; e.g. 2024-01-01 12:58:58.5070000
  utcl_add_seconds( @utc,5 ) AS sec_add_utc, "2024-02-15 05:30:05.0000000
  utcl_seconds_between( utclong`2024-02-25 08:14:26`,utclong`2024-02-25 08:15:17` ) AS sec_bw_utc,
  "51.0000000 

  "Functions specific to the type timetamp
  tstmp_is_valid( @tmst ) AS ts_is_valid, "1
  tstmp_current_utctimestamp( ) AS ts_current, "20240312125858 
  "The following two functions have an optional parameter on_error.
  "Check the ABAP Keyword Documentation
  tstmp_seconds_between( tstmp1 = @tmst,
                          tstmp2 = CAST( dec`20240808112517` AS DEC( 15,0 ) ) ) AS sec_bw_ts, "19
  tstmp_add_seconds( tstmp    = @tmst,
                      seconds  = CAST( dec`10` AS DEC( 15,0 ) ) ) AS sec_add_ts, "20240808112508 

  "---------------------- Functions for conversions ----------------------
  "Note: For the following functions, optional parameters are possible.
  "For more details, check the ABAP Keyword Documentation.
  tstmp_to_dats( tstmp = @tmst,
                  tzone =  CAST( char`EST` AS CHAR( 6 ) ) ) AS tstmp_to_dats, "20240808
  tstmp_to_tims( tstmp = @tmst,
                  tzone = CAST( char`EST` AS CHAR( 6 ) ) ) AS tstmp_to_tims, "072458
  tstmp_to_dst( tstmp = @tmst,
                tzone = CAST( char`EST` AS CHAR( 6 ) ) ) AS tstmp_to_dst, "X
  dats_tims_to_tstmp( date = @da,
                      time = @ti,
                      tzone = CAST( char`EST` AS CHAR( 6 ) ) ) AS dats_tims_to_tstmp, "20240122173456 
  tstmpl_to_utcl( tstmpl = @tmstlong ) AS tstmpl_to_utcl, "2024-01-01 08:13:17.8101100
  tstmpl_from_utcl( utcl = @utc ) AS tstmpl_from_utcl, "20240215053000.0000000 
  dats_to_datn( dats = dats`20240812` ) AS dats_to_datn, "20240812
  dats_from_datn( datn = datn`20240111` ) AS dats_from_datn, "20240111
  tims_to_timn( tims = tims`231256` ) AS tims_to_timn, "231256
  tims_from_timn( timn = timn`155432` ) AS tims_from_timn "155432

WHERE TimeZoneID = char`EST`
INTO @DATA(wa).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Calendar-Related Information

Find examples with classes for accessing calendar-related information in [this section](22_Released_ABAP_Classes.md#calendar-related-information) of the *Released ABAP Classes* cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information

[Date and Time Processing](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendate_time_processing.htm) in the ABAP Keyword Documentation.


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example
[zcl_demo_abap_date_time](./src/zcl_demo_abap_date_time.clas.abap)

> [!NOTE]
> - The executable example covers the handling and processing of date, time, and time stamps. The snippets of this cheat sheet and more are included. 
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)