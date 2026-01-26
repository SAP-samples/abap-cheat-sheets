"! <p class="shorttext"><strong>String processing</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates various syntax options for processing character strings.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <p>Find the following information in the ABAP Doc comment of class {@link zcl_demo_abap_aux}:</p>
"! <ul><li>How to get started with the example class</li>
"! <li>Structuring of (most of) the example classes</li>
"! <li>Disclaimer</li></ul>
CLASS zcl_demo_abap_string_proc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.
    METHODS:
      m01_create_strings_assign_val  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m02_chain_strings  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m03_string_templates  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m04_string_templates_format  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m05_string_length  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m06_concatenate  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m07_literal_operator  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m08_split  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m09_lowercase_uppercase  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m10_shift  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m11_condense  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m12_reverse  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m13_insert_substrings  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m14_overlay  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m15_process_substrings  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m16_search_characters  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m17_replace_characters  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m18_search_substrings_comp_op  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m19_find_statement  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m20_find_in_table  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m21_find_function  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m22_replace_statement  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m23_replace_section  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m24_replace_in_table  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m25_replace_function  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m26_search_pattern_logical_op  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m27_search_regex  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m28_replacements_using_regex  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m29_regex_system_classes  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m30_distance_function  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m31_repeat_function  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m32_cmin_cmax_functions  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m33_escape_function  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m34_string_processing_xco  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m35_byte_string_processing  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_string_proc IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_aux=>set_example_divider(
      out  = out
      text = `ABAP Cheat Sheet Example: String Processing`
    ).

    "Dynamically calling methods of the class
    "The method names are retrieved using RTTI. For more information, refer to the
    "Dynamic Programming ABAP cheat sheet.
    "Only those methods should be called that follow the naming convention M + digit.
    DATA(methods) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->methods.
    SORT methods BY name ASCENDING.

    "To call a particular method only, you can comment in the WHERE clause and
    "adapt the literal appropriately.
    LOOP AT methods INTO DATA(meth_wa)
    "WHERE name CS 'M01'
    .
      TRY.
          "The find function result indicates that the method name begins (offset = 0) with M and a digit.
          IF find( val = meth_wa-name pcre = `^M\d` case = abap_false ) = 0.
            CALL METHOD (meth_wa-name) EXPORTING out = out text = CONV string( meth_wa-name ).
          ENDIF.
        CATCH cx_root INTO DATA(error).
          out->write( error->get_text( ) ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD m01_create_strings_assign_val.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Creating Strings and Assigning Values| ).

    "Data object declarations providing default values
    DATA: flag   TYPE c LENGTH 1 VALUE 'X',    "Single quotes
          str_a1 TYPE string VALUE `Hallo, how are you?`. "Backquotes

    DATA: char_a1 TYPE c LENGTH 5,
          str_a2  TYPE string,
          str_a3  LIKE str_a2.

    "Examples for type n
    DATA zip_code TYPE n LENGTH 5 VALUE '12345'.

    DATA isbn_number TYPE n LENGTH 13 VALUE '1234567890123'.

    "Value assignments to existing data objects
    char_a1 = 'ab123'.

    str_a2 = `<p>Hallo!</p>`.

    "Escaping a backquote using another backquote
    str_a3 = `This is a backquote: ``.`.

    "If possible, avoid unnecessary type conversion; in principle,
    "every convertible type can be specified
    "Assigning a fixed-length string to a variable-length string.
    str_a2 = 'abc'.

    DATA str_a4 TYPE string VALUE 'X'. "Type c length 1

    DATA str_a5 TYPE string VALUE -1. "Type i

    "Inline declaration: data object declaration and
    "value assignment
    "Data type is automatically derived
    DATA(char_a2) = 'abcd'. "Type c length 4

    DATA(str_a6) = `efgh`. "Type string

    "Note: Variable is of type c length 4. Characters are truncated.
    "In newer ABAP releases, the following statement shows a syntax
    "warning that the value of the literal (intentionally specified
    "here like this) is not an admissable value for the target type.
    "Therefore, the example is provided differently to circumvent the
    "syntax warning.

    "char_a2 = 'ijklmnopq'.

    TYPES c_l9 TYPE c LENGTH 9.
    DATA some_char TYPE c_l9 VALUE 'ijklmnopq'.
    char_a2 = some_char.

    "Treating trailing blanks
    DATA(char_a3) = 'ab   '.

    DATA(str_a7)  = `cdefgh`.

    str_a7 = char_a3. "Trailing blanks are not respected.

    "Excursion: Chaining strings
    "Note the conversion result of str_a5 above (i to string)
    DATA(str_a8) = str_a4 && ` ` && str_a5 && `!`.

    out->write( data = str_a3 name = `str_a3` ).
    out->write( |\n| ).
    out->write( data = char_a2 name = `char_a2` ).
    out->write( |\n| ).
    out->write( data = str_a7 name = `str_a7` ).
    out->write( |\n| ).
    out->write( data = str_a8 name = `str_a8` ).
  ENDMETHOD.

  METHOD m02_chain_strings.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Chaining Strings| ).

    DATA(str_b1) = `Hallo`.
    DATA(str_b2) = `how`.
    DATA(str_b3) = `are`.

    "Chaining using && operator
    DATA(str_b4) = str_b1 && ` ` && sy-uname && `, ` && str_b2 && ` ` && str_b3 && ` you?`.

    "Chaining only character literals of the same type using & operator
    "Note: Such a combination of literals is possible up to 255 chars.
    DATA(char_b1) = 'AB' & 'AP  '. "Trailing blanks are ignored

    DATA(str_b5) = `AB` & `AP  `.

    out->write( data = str_b4 name = `str_b4` ).
    out->write( |\n| ).
    out->write( data = char_b1 name = `char_b1` ).
  ENDMETHOD.

  METHOD m03_string_templates.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: String Templates| ).

    "--- Constructing Strings ---

    "The expression must be convertible to a string. A blank (not
    "within the curly brackets) means a blank in the resulting string.
    DATA(str_c1) = `Hallo`.
    DATA(str_c2) = `how`.
    DATA(str_c3) = `are`.
    DATA(str_c4) = |{ str_c1 } { sy-uname }, | &&
                   |{ str_c2 } { str_c3 } you?|.

    out->write( data = str_c4 name = `str_c4` ).

    "--- String Templates (2): Control Characters ---

    "Interpretation of character combinations as control characters
    "\n interpreted as a line break
    DATA(str_c5) = |{ str_c1 }\n{ sy-uname },| &&
                   |\n{ str_c2 }\n{ str_c3 }\nyou?|.

    out->write( data = str_c5 name = `str_c5` ).
    out->write( |\n| ).

    "Excursion: Class CL_ABAP_CHAR_UTILITIES provides attributes and methods as utilities for string processing.
    "See the class documentation.
    "The following examples demonstrate that attributes that contain control characters can be replaced by
    "a representation of control characters in a string template.
    DATA(str_c6) = |{ str_c1 }{ cl_abap_char_utilities=>newline }{ sy-uname }|.
    DATA(str_c7) = |{ str_c1 }\n{ sy-uname }|.
    DATA(str_c8) = |{ str_c1 }{ cl_abap_char_utilities=>horizontal_tab }{ sy-uname }|.
    DATA(str_c9) = |{ str_c1 }\t{ sy-uname }|.
    DATA(str_c10) = |{ str_c1 }{ cl_abap_char_utilities=>cr_lf  }{ sy-uname }|.
    DATA(str_c11) = |{ str_c1 }\r\n{ sy-uname }|.
    ASSERT str_c10 = str_c11.

    out->write( data = str_c6 name = `str_c6` ).
    out->write( |\n| ).
    out->write( data = str_c7 name = `str_c7` ).
    out->write( |\n| ).
    out->write( data = str_c8 name = `str_c8` ).
    out->write( |\n| ).
    out->write( data = str_c9 name = `str_c9` ).


  ENDMETHOD.

  METHOD m04_string_templates_format.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: String Templates: Formatting Options| ).

*&---------------------------------------------------------------------*
*& DATE
*&---------------------------------------------------------------------*

    "Defining the format of a date
    "The output is just an example and depends on your settings.
    DATA(d) = |The date is { cl_abap_context_info=>get_system_date( ) DATE = USER }.|. "The date is 01/01/2024.
    d = |{ cl_abap_context_info=>get_system_date( ) DATE = RAW }|. "20240101
    d = |{ cl_abap_context_info=>get_system_date( ) DATE = ISO }|. "2024-01-01
    d = |{ cl_abap_context_info=>get_system_date( ) DATE = ENVIRONMENT }|. "01/01/2024

*&---------------------------------------------------------------------*
*& TIME
*&---------------------------------------------------------------------*

    "Defining the format of a time
    "The output is just an example and depends on your settings.
    DATA(tm) = |The time is { cl_abap_context_info=>get_system_time( ) TIME = ISO }.|. "The time is 14:37:24.
    tm = |{ cl_abap_context_info=>get_system_time( ) TIME = RAW }|. "143724
    tm = |{ cl_abap_context_info=>get_system_time( ) TIME = USER }|. "14:37:24
    tm = |{ cl_abap_context_info=>get_system_time( ) TIME = ENVIRONMENT }|. "14:37:24

*&---------------------------------------------------------------------*
*& TIMESTAMP
*&---------------------------------------------------------------------*

    "Defining the format of a time stamp
    "The output is just an example and depends on your settings.
    DATA(ts) = |{ utclong_current( ) TIMESTAMP = SPACE }|. "2024-01-01 14:39:50.4069170
    ts = |{ utclong_current( ) TIMESTAMP = ISO }|. "2024-01-01T14:39:50,4071110
    ts = |{ utclong_current( ) TIMESTAMP = USER }|. "01/01/2024 14:39:50.4072010
    ts = |{ utclong_current( ) TIMESTAMP = ENVIRONMENT }|. "01/01/2024 14:39:50.4073230
    ts = |{ utclong_current( ) }|. "2024-01-01 14:39:50.4074060

*&---------------------------------------------------------------------*
*& TIMEZONE
*&---------------------------------------------------------------------*

    "Defining the format of a time stamp using the rules for time zones
    DATA(tz) = |{ utclong_current( ) TIMEZONE = 'UTC' }|. "2024-12-30 14:43:20.6534640
    tz = |{ utclong_current( ) TIMEZONE = 'CET' COUNTRY = 'DE ' }|. "30.12.2024 15:43:20,6536320
    tz = |{ utclong_current( ) TIMEZONE = 'EST' COUNTRY = 'US ' }|. "12/30/2024 09:43:20.6889180 AM

*&---------------------------------------------------------------------*
*& CASE
*&---------------------------------------------------------------------*

    "Lowercase and uppercase
    DATA s1 TYPE string.
    DATA s2 TYPE string.
    s1 = `AbCdEfG`.
    s2 = |{ s1 CASE = LOWER }|. "abcdefg
    s2 = |{ s1 CASE = UPPER }|. "ABCDEFG

*&---------------------------------------------------------------------*
*& WIDTH/ALIGN
*&---------------------------------------------------------------------*

    s1 = `##`.
    s2 = |{ s1 WIDTH = 10 ALIGN = LEFT }<---|.   "'##        <---'
    s2 = |{ s1 WIDTH = 10 ALIGN = CENTER }<---|. "'    ##    <---'

*&---------------------------------------------------------------------*
*& PAD
*&---------------------------------------------------------------------*

    "Used to pad any surplus places in the result with the specified character.
    s2 = |{ s1 WIDTH = 10 ALIGN = RIGHT PAD = `.` }<---|. "'........##<---'

*&---------------------------------------------------------------------*
*& DECIMALS
*&---------------------------------------------------------------------*

    s1 = |{ CONV decfloat34( - 1 / 3 ) DECIMALS = 3 }|. "'-0.333'

*&---------------------------------------------------------------------*
*& SIGN
*&---------------------------------------------------------------------*

    "Defining the format of the +/- sign when the string represented
    "by the embedded expression represents a numeric value
    "- left without space, no +
    s1 =  |{ +1 SIGN = LEFT }|. "1
    "- and + left without space
    s1 =  |{ 1 SIGN = LEFTPLUS }|. "+1
    "- left without space, blank left for +
    s1 =  |{ 1 SIGN = LEFTSPACE }|. " 1
    "- right without space, no +
    s1 =  |{ -1 SIGN = RIGHT }|. "1-
    "- and + right without space
    s1 =  |{ 1 SIGN = RIGHTPLUS }|. "1+
    "- left without space, blank right for +
    s1 =  |{ +1 SIGN = RIGHTSPACE }|. "1

*&---------------------------------------------------------------------*
*& ZERO
*&---------------------------------------------------------------------*

    "Defining the format of the numeric value zero.
    "Only to be specified if the embedded expression has a numeric data type.
    s1 = |'{ 0 ZERO = NO }' and '{ 0 ZERO = YES }'|. "'' and '0'

*&---------------------------------------------------------------------*
*& XSD
*&---------------------------------------------------------------------*

    "Formatting is applied to an embedded expression (elementary data types) in asXML format that is
    "assigned to its data type. Check the information in the ABAP Keyword Documentation about the asXML
    "mapping of elementary ABAP types.
    DATA xstr TYPE xstring  VALUE `41424150`.
    DATA dat TYPE d VALUE '20240101'.
    DATA tim TYPE t VALUE '123456'.
    DATA(utc) = utclong_current( ). "e.g. 2024-01-01 13:51:38.5708800

    s1 = |{ xstr XSD = YES }|. "QUJBUA==
    s1 = |{ dat XSD = YES }|. "2024-01-01
    s1 = |{ tim XSD = YES }|. "12:34:56
    s1 = |{ utc XSD = YES }|. "2024-01-01T13:51:38.57088Z

*&---------------------------------------------------------------------*
*& STYLE
*&---------------------------------------------------------------------*

    "Defining the style of decimal floating point numbers;
    "see the details in the ABAP Keyword Documentation.
    DATA(dcfl34) = CONV decfloat34( '-123.45600' ).
    s1 = |{ dcfl34 }|. "-123.456
    "Creates the predefined format
    s1 = |{ dcfl34 STYLE = SIMPLE }|. "-123.456
    "+/- added to the right, removes trailing zeros
    s1 = |{ dcfl34 STYLE = SIGN_AS_POSTFIX  }|. "123.456-
    "Retains trailing zeros
    s1 = |{ dcfl34 STYLE = SCALE_PRESERVING }|. "-123.45600
    "Scientific notation; at least a two digit exponent with a plus/minus sign
    s1 = |{ dcfl34 STYLE = SCIENTIFIC }|. "-1.23456E+02
    "Scientific notation; only one integer digit with the value 0
    s1 = |{ dcfl34 STYLE = SCIENTIFIC_WITH_LEADING_ZERO }|. "-0.123456E+03
    "Scientific notation; exponent has 3 digits for decfloat16 and 4 digits for decfloat34
    s1 = |{ dcfl34 STYLE = SCALE_PRESERVING_SCIENTIFIC }|. "-1.2345600E+0002
    "Technical format
    s1 = |{ dcfl34 STYLE = ENGINEERING }|. "-123.456E+00

*&---------------------------------------------------------------------*
*& ALPHA
*&---------------------------------------------------------------------*

    "Adds or removes leading zeros from strings of digits; the data type
    "must be string, c, or n
    "Adding leading zeros
    "Additionally specifying WIDTH
    "Note: The specified length is only used if it is greater than
    "the length of provided string (without leading zeros)
    s1 = |{ '1234' ALPHA = IN WIDTH = 10 }|.                "0000001234
    s1 = |{ '00000000000000000000000012' ALPHA = IN WIDTH = 10 }|. "0000000012
    "Fixed-length string provided, WIDTH not specified
    s1 = |{ '   12' ALPHA = IN }|. "00012
    "Removing leading zeros
    s1 = |{ '00001234' ALPHA = OUT }|. "1234
    "Do not apply formatting
    s1 = |{ '00001234' ALPHA = RAW }|.                      "00001234

*&---------------------------------------------------------------------*
*& EXPONENT
*&---------------------------------------------------------------------*

    "Defining the exponent when formatting floating point numbers;
    "only usable with numeric data types; only affects data type f
    "or in combination with STYLE = scientific

    DATA(flp) = CONV f( 1 / 3 ).

    "0.33333333333333331
    s1 = |{ flp }|.
    "3.3333333333333331E-01
    s1 = |{ flp EXPONENT = -1 }|.
    "0.000000000033333333333333331E+10
    s1 = |{ flp EXPONENT = 10 }|.

    DATA(dec_num) = CONV decfloat34( '-123.45600' ).

    "-1.23456E+02
    s1 = |{ dec_num STYLE = SCIENTIFIC }|.
    "-1234.56E-01
    s1 = |{ dec_num STYLE = SCIENTIFIC EXPONENT = -1 }|.

*&---------------------------------------------------------------------*
*& CURRENCY
*&---------------------------------------------------------------------*

    "Defining the number of decimal places in the context of currencies;
    "only usable with numeric data types; some options such as DECIMALS
    "cannot be specified together with CURRENCY, SIGN can be specified
    "for +/-; check descriptions of the behavior of the various numeric
    "types in the ABAP Keyword Documentation

    "Type i: A decimal separator is inserted at the place determined by
    "the currency
                                                            "123456.78
    s1 = |{ 12345678 CURRENCY = 'EUR' }|.

    "Type f: Same effect as DECIMALS, number of decimal places is
    "determined by the currency
    "1.23
    s1 = |{ CONV f( '1.234' ) CURRENCY = 'EUR' }|.

    "Using CURRENCY and SIGN
    "+123.45
    s1 = |{ 12345 CURRENCY = 'EUR' SIGN = LEFTPLUS }|.

*&---------------------------------------------------------------------*
*& COUNTRY
*&---------------------------------------------------------------------*

    "Temporarily setting country-specific formatting (alternative to using
    "the ENVIRONMENT option)

    DATA land TYPE land1 VALUE 'DE '.
    "987.654.321 (for example)
    s1 = |{ 987654321 COUNTRY = land }|.

    "987,654,321 (for example)
    s1 = |{ 987654321 COUNTRY = 'US ' }|.

    "10/18/2025 (for example)
    s1 = |{ cl_abap_context_info=>get_system_date( ) COUNTRY = 'US ' }|.

    "14:21:12 (for example)
    s1 = |{ cl_abap_context_info=>get_system_time( ) COUNTRY = land }|.

    "Using non-existing country
    land = '&/§'.
    TRY.
        s1 = |{ 987654321 COUNTRY = land }|.
      CATCH cx_sy_strg_format INTO DATA(err).
        s1 = err->get_text( ).
    ENDTRY.

*&---------------------------------------------------------------------*
*& NUMBER
*&---------------------------------------------------------------------*

    "Defining the format of decimal representation (decimal and thousands
    "separators); only usable with numeric data types; only specific formats
    "can be specified (RAW being the default)

    DATA decimal_number TYPE decfloat34 VALUE '-123456.7890'.

    "Period as decimal separator, no thousands separators
    "-123456.789
    s1 = |{ decimal_number NUMBER = RAW }|.

    "Decimal/thousands separators based on user master record
    "-123.456,789 (for example)
    s1 = |{ decimal_number NUMBER = USER }|.

    "Decimal/thousands separators based on current language
    "environment
    "-123.456,789 (for example)
    s1 = |{ decimal_number NUMBER = ENVIRONMENT }|.

    out->write( zcl_demo_abap_aux=>no_output ).
  ENDMETHOD.

  METHOD m05_string_length.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Determining the Length of Strings| ).

    DATA(str_e1)  = `abc def ghi   `.
    DATA(char_e1) = 'abc def ghi   '.

    "strlen
    "Result depends on the type of the data object
    "Fixed-length string ignores trailing blanks
    DATA(length_e1) = strlen( str_e1 ).
    DATA(length_e2) = strlen( char_e1 ).

    "numofchar
    "To exclude trailing blanks in any case.
    DATA(length_e3) = numofchar( str_e1 ).
    DATA(length_e4) = numofchar( char_e1 ).

    "Excursion:
    "To emphasizes modern, expression-enabled ABAP, the expression
    "with the string function can be placed directly in the DO
    "statement instead of having an extra variable.
    DATA(str_e3) = `abcde`.
    DATA(length_e5) = strlen( str_e3 ).
    DATA(int_e1) = 0.

    DO length_e5 TIMES.
      int_e1 += 1.
    ENDDO.

    DATA(int_e2) = 0.

    DO strlen( str_e3 ) TIMES.
      int_e2 += 1.
    ENDDO.

    out->write( data = length_e1 name = `length_e1` ).
    out->write( |\n| ).
    out->write( data = length_e2 name = `length_e2` ).
    out->write( |\n| ).
    out->write( data = length_e3 name = `length_e3` ).
    out->write( |\n| ).
    out->write( data = length_e4 name = `length_e4` ).
    out->write( |\n| ).
    out->write( data = int_e1 name = `int_e1` ).
    out->write( |\n| ).
    out->write( data = int_e2 name = `int_e2` ).
  ENDMETHOD.

  METHOD m06_concatenate.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Concatenating Strings| ).

    DATA(str_f1) = `Hallo`.
    DATA(str_f2) = `world`.

    "Concatenation using && operator and string templates
    DATA(str_f3) = str_f1 && str_f2.
    DATA(str_f4) = str_f1 && ` ` && str_f2.
    DATA(str_f5) = |{ str_f1 } { str_f2 }|.

    "CONCATENATE statements
    CONCATENATE str_f1 str_f2 INTO DATA(str_f6).
    "Adding a separation sign using the addition SEPARATED BY
    CONCATENATE str_f1 str_f2 INTO DATA(str_f7) SEPARATED BY ` `.
    CONCATENATE str_f1 str_f2 INTO DATA(str_f8) SEPARATED BY `#`.

    DATA(char_f1) = '2 trailing blanks:  '.
    DATA(char_f2) = '3 trailing blanks:   '.
    DATA(char_f3) = '<-'.
    "Keeping trailing blanks in the result when concatenating
    "fixed-length strings. The ones of variable-length strings are
    "respected by default
    CONCATENATE char_f1 char_f2 char_f3
      INTO DATA(char_f4) RESPECTING BLANKS.
    "Trailing blanks are ignored
    CONCATENATE char_f1 char_f2 char_f3 INTO DATA(char_f5).

    "Example use case: Concatenating smaller text fragments
    "sequentially into a longer character sequence.
    DATA: itab_g    TYPE TABLE OF string,
          alphabet1 TYPE string.

    itab_g = VALUE #( ( `abc` ) ( `def` ) ( `ghi` ) ).

    LOOP AT itab_g ASSIGNING FIELD-SYMBOL(<abc>).
      alphabet1 = alphabet1 && <abc>.
      "Alternative:
      "CONCATENATE alphabet <abc> INTO alphabet.
    ENDLOOP.

    "Avoiding loops if your use case is to concatenate lines of an
    "internal table into a string in one go
    CONCATENATE LINES OF itab_g INTO DATA(alphabet2).

    ""Adding a separation sign using the addition SEPARATED BY
    CONCATENATE LINES OF itab_g INTO DATA(alphabet3)
                                SEPARATED BY ` `.

    "String function concat_lines_of
    DATA(alphabet4) = concat_lines_of( table = itab_g ).
    "sep parameter specifying the separation sign
    DATA(alphabet5) = concat_lines_of( table = itab_g sep = `,` ).

    out->write( data = str_f3 name = `str_f3` ).
    out->write( |\n| ).
    out->write( data = str_f4 name = `str_f4` ).
    out->write( |\n| ).
    out->write( data = str_f5 name = `str_f5` ).
    out->write( |\n| ).
    out->write( data = str_f6 name = `str_f6` ).
    out->write( |\n| ).
    out->write( data = str_f7 name = `str_f7` ).
    out->write( |\n| ).
    out->write( data = str_f8 name = `str_f8` ).
    out->write( |\n| ).
    out->write( data = char_f4 name = `char_f4` ).
    out->write( |\n| ).
    out->write( data = char_f5 name = `char_f5` ).
    out->write( |\n| ).
    out->write( data = alphabet1 name = `alphabet1` ).
    out->write( |\n| ).
    out->write( data = alphabet2 name = `alphabet2` ).
    out->write( |\n| ).
    out->write( data = alphabet3 name = `alphabet3` ).
    out->write( |\n| ).
    out->write( data = alphabet4 name = `alphabet4` ).
    out->write( |\n| ).
    out->write( data = alphabet5 name = `alphabet5` ).
  ENDMETHOD.

  METHOD m07_literal_operator.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Literal Operator| ).

    "Literal operator
    "Used to combine character literals of the same type into a single character literal
    DATA(abap) = 'AB' & 'AP'.

    "Note an upper limit of 255 characters
    "If you remove the comment, which results in a combined character literal
    "of 256 characters, a syntax error is displayed.
    DATA(c_limit_255) =
    '##################################################' & "50 x #
    '##################################################' &
    '##################################################' &
    '##################################################' &
    '##################################################' &
    '#####'
    "& '#'
    .

    "Trailing blanks are respected
    DATA(char_with_blanks) = 'AB' & 'AP' & '           '.

    "Using RTTI to get type information, retrieving the output length of the combined literal
    DATA(output_length) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( char_with_blanks ) )->output_length.
    out->write( data = output_length name = `output_length` ).
    out->write( |\n| ).

    DATA(ch1) = 'AB'.
    DATA(ch2) = 'AP'.
    "Not possible as the operands are not character literals but data objects
    "DATA(combined_ch) = ch1 & ch2.

    "Not to be confused with the concatenation operator && to concatenate
    "character-like operands; at runtime, any number of character-like operands
    "are possible
    "The result in the example is of type string.
    DATA(combined_ch_with_conc_op) = ch1 && ch2.

    "Concatenation similar to the example above
    "As the result is of type string using the concatenation operator, the
    "trailing blanks are not respected.
    DATA(char_with_blanks_conc_op) = 'AB' && 'AP' && '           '.

    DATA(len1) = strlen( char_with_blanks_conc_op ).
    DATA(len2) = numofchar( char_with_blanks_conc_op ).

    out->write( data = len1 name = `len1` ).
    out->write( |\n| ).
    out->write( data = len2 name = `len2` ).
  ENDMETHOD.

  METHOD m08_split.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Splitting Strings| ).


    DATA(str_g1) = `Hallo,world,12345`.

    SPLIT str_g1 AT `,` INTO DATA(str_g2) DATA(str_g3) DATA(str_g4).

    "Less data objects than possible splittings
    SPLIT str_g1 AT `,` INTO DATA(str_g5) DATA(str_g6).

    "Splitting string into an internal table
    DATA itab_g1 TYPE TABLE OF string.

    SPLIT str_g1 AT ',' INTO TABLE itab_g1.

    "Getting the value of a specific segment
    DATA(str_g7) = segment( val = str_g1 index = 2 sep = `,` ).

    "Example with segment
    "A string is split and the values of segments are retrieved. Here,
    "all segments are retrieved and inserted into an internal table
    "using a DO loop. If you specify an empty string, an exception of
    "the class CX_SY_STRG_PAR_VAL is raised. This is true for this
    "example since the DO loop inevitably runs into the error because
    "of not specifying an appropriate number of loops. Note that
    "if the index parameter of the segment function is positive, the
    "occurrences are counted from the left. If index is negative, the
    "occurrences are counted from the right.
    DATA itab_g2 TYPE TABLE OF string.
    DO.
      TRY.
          DATA(str_g8) = segment( val   = str_g1
                                   index = sy-index
                                   sep   = `,` ).

          APPEND |Segment value: '{ str_g8 }' | &&
                 |Segment index: '{ sy-index }'| TO itab_g2.

        CATCH cx_sy_strg_par_val.
          DATA(seg_nom) = |There are { sy-index - 1 } | &&
                          |segments in the string.|.
          EXIT.
      ENDTRY.
    ENDDO.

    "SPLIT statement specifying a colon after INTO
    DATA(some_text) = `Lorem ipsum dolor sit amet, consectetur adipiscing elit`.

    SPLIT some_text AT ` ` INTO: DATA(str1) DATA(str2) DATA(str3) DATA(str4), TABLE DATA(tab).

    out->write( data = str_g2 name = `str_g2` ).
    out->write( |\n| ).
    out->write( data = str_g3 name = `str_g3` ).
    out->write( |\n| ).
    out->write( data = str_g4 name = `str_g4` ).
    out->write( |\n| ).
    out->write( data = str_g5 name = `str_g5` ).
    out->write( |\n| ).
    out->write( data = str_g6 name = `str_g6` ).
    out->write( |\n| ).
    out->write( data = itab_g1 name = `itab_g1` ).
    out->write( |\n| ).
    out->write( data = str_g7 name = `str_g7` ).
    out->write( |\n| ).
    out->write( data = str_g8 name = `str_g8` ).
    out->write( |\n| ).
    out->write( data = itab_g2 name = `itab_g2` ).
    out->write( |\n| ).
    out->write( data = seg_nom name = `seg_nom` ).
    out->write( |\n| ).
    out->write( data = str1 name = `str1` ).
    out->write( |\n| ).
    out->write( data = str2 name = `str2` ).
    out->write( |\n| ).
    out->write( data = str3 name = `str3` ).
    out->write( |\n| ).
    out->write( data = str4 name = `str4` ).
    out->write( |\n| ).
    out->write( data = tab name = `tab` ).
  ENDMETHOD.

  METHOD m09_lowercase_uppercase.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Transforming to Lower and Upper Case| ).

    DATA(str_h1) = `It's a string`.
    DATA(str_h2) = str_h1.

    "The string functions store the result in a target variable.
    DATA(str_h3) = to_upper( str_h1 ).
    DATA(str_h4) = to_lower( str_h1 ).

    "TRANSLATE does the transformation on the source variable.
    TRANSLATE str_h1 TO UPPER CASE.
    TRANSLATE str_h2 TO LOWER CASE.

    "to_mixed/from_mixed functions
    "sep: Specifies the separator
    "case: A character-like text field. A small character specifies
    "that the first character of the string is in lower case. If the
    "specification is, for example, case = 'X', the first character
    "is capitalized.
    "min: A positive number that specifies the minimum number of
    "characters that must appear before the separator. The default
    "value is 1.
    DATA(str_h5) = `A_GREAT_STRING`.
    DATA(str_h6) = to_mixed( val = str_h5 sep = `_` ).
    DATA(str_h7) = to_mixed( val = str_h5 sep = `_` case = 'x' ).
    DATA(str_h8) = to_mixed( val = str_h5 sep = `_`
                             case = 'a' min = 3 ).

    DATA(str_h9) = from_mixed( val = `someGreatString` sep = ` `
                               case = 'a' min = 4 ).

    out->write( data = str_h3 name = `str_h3` ).
    out->write( |\n| ).
    out->write( data = str_h4 name = `str_h4` ).
    out->write( |\n| ).
    out->write( data = str_h1 name = `str_h1` ).
    out->write( |\n| ).
    out->write( data = str_h2 name = `str_h2` ).
    out->write( |\n| ).
    out->write( data = str_h6 name = `str_h6` ).
    out->write( |\n| ).
    out->write( data = str_h7 name = `str_h7` ).
    out->write( |\n| ).
    out->write( data = str_h8 name = `str_h8` ).
    out->write( |\n| ).
    out->write( data = str_h9 name = `str_h9` ).
  ENDMETHOD.

  METHOD m10_shift.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Shifting Content in Strings| ).

    DATA(str_i1) = `hallo`.
    DATA(str_i2) = str_i1.
    DATA(str_i3) = str_i1.
    DATA(str_i4) = str_i1.

    "No addition; string is shifted one place to the left
    SHIFT str_i2.

    "Shifting string by n places; without direction,
    "left by default
    SHIFT str_i3 BY 2 PLACES.

    "Direction explicitly specified
    "Variable-length strings are extended
    SHIFT str_i4 BY 3 PLACES RIGHT.

    DATA(char_i1) = 'world  '.
    DATA(char_i2) = char_i1.
    DATA(char_i3) = char_i1.
    DATA(str_i5) = `world  `.

    "Comparison of behavior for fixed- and variable-length strings
    SHIFT char_i1 BY 3 PLACES RIGHT.
    SHIFT str_i5 BY 3 PLACES RIGHT.

    "CIRCULAR addition: characters that are moved out of the string are
    "added at the other end again
    SHIFT char_i2 BY 3 PLACES RIGHT CIRCULAR.
    SHIFT char_i3 BY 2 PLACES LEFT CIRCULAR.

    DATA(str_i6) = ` hallo world `.
    DATA(str_i7) = str_i6.

    "Moving characters up to a specific character set
    SHIFT str_i6 UP TO 'or'.

    "Deleting leading and trailing characters with this sequence
    "of statements
    SHIFT str_i7 RIGHT DELETING TRAILING ` `.
    SHIFT str_i7 LEFT DELETING LEADING ` `.

    "String functions storing the result in a target variable
    DATA(str_i8) = `some string`.

    "shift_left
    DATA(str_i9) = shift_left( val = str_i8  places = 3 ).

    DATA(str_i10) = shift_left( val = str_i8  circular = 7 ).

    "shift_right
    "Note: When the parameter places is specified, the function
    "shift_right has a different behavior than the SHIFT statement.
    "Here, the length of the string is reduced. SHIFT extends the
    "length or it remains the same.
    DATA(str_i11) = shift_right( val = str_i8  places = 3 ).

    DATA(str_i12) = `shift_right and trailing blanks  `.

    "sub: Specifying a substring; all substrings in the string that
    "match the value are removed (sub also available for shift_left)
    DATA(str_i13) = shift_right( val = str_i12
                                 sub = ` and trailing blanks  ` ).

    DATA(str_i14) = shift_right( val = str_i12  sub = ` ` ).

    DATA(str_i15) = shift_right( val = str_i12 ). "Same effect as above

    out->write( |SHIFT statements:\n\n| ).
    out->write( data = str_i2 name = `str_i2` ).
    out->write( |\n| ).
    out->write( data = str_i3 name = `str_i3` ).
    out->write( |\n| ).
    out->write( data = str_i4 name = `str_i4` ).
    out->write( |\n| ).
    out->write( data = char_i1 name = `char_i1` ).
    out->write( |\n| ).
    out->write( data = str_i5 name = `str_i5` ).
    out->write( |\n| ).
    out->write( data = char_i2 name = `char_i2` ).
    out->write( |\n| ).
    out->write( data = char_i3 name = `char_i3` ).
    out->write( |\n| ).
    out->write( data = str_i6 name = `str_i6` ).
    out->write( |\n| ).
    out->write( data = str_i7 name = `str_i7` ).
    out->write( |\n| ).

    out->write( |String functions:\n\n| ).
    out->write( |\n| ).
    out->write( data = str_i9 name = `str_i9` ).
    out->write( |\n| ).
    out->write( data = str_i10 name = `str_i10` ).
    out->write( |\n| ).
    out->write( data = str_i11 name = `str_i11` ).
    out->write( |\n| ).
    out->write( data = str_i13 name = `str_i13` ).
    out->write( |\n| ).
    out->write( data = str_i14 name = `str_i14` ).
    out->write( |\n| ).
    out->write( data = str_i15 name = `str_i15` ).
  ENDMETHOD.

  METHOD m11_condense.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Condensing Strings| ).

    DATA(char_j1) = ' some text '.
    DATA(char_j2) = '    some     more text   '.
    DATA(char_j3) = ' a third text field literal '.

    "No addition: Removes leading and trailing blanks. This is also
    "true for multiple blanks. It also replaces sequences of multiple
    "blanks with a single blank.
    CONDENSE char_j1.
    CONDENSE char_j2.

    "NO-GAPS: Removes all blanks, also between words. When NO-GAPS
    "is used with variable-length strings, trailing blanks remain
    "removed.
    CONDENSE char_j3 NO-GAPS.

    "RESPECTING BLANKS: Avoiding condensing
    "A use case might be the assignment of strings with fixed- to
    "variable-length strings.
    DATA(char_j4) = '  abcef  '.
    DATA(char_j5) = '  ghij  '.
    DATA str_j TYPE string.

    "Result: '  abcef    ghij  '
    CONCATENATE char_j4 char_j5 INTO str_j RESPECTING BLANKS.

    "String function condense
    "The advantage of using the string functions is
    "that you can also specify random characters to be removed and
    "not only blanks.
    DATA(str_j1) = `  hi   there  `.

    "No parameters specified (i. e. their default values are provided);
    "works like CONDENSE statements without the NO-GAPS addition
    DATA(str_j2) = condense( str_j1 ).

    "Parameter 'from' specified with an initial string, 'del'/'to' not
    "specified: Removes leading and trailing blanks. The 'from'
    "parameter could also be specified with a text field literal:
    "from = ' '
    DATA(str_j3) = condense( val = str_j1 from = `` ).

    "Parameter 'to' specified with an initial string, 'from'/'del' not
    "specified: works like CONDENSE statements with the NO-GAPS
    "addition
    DATA(str_j4) = condense( val = str_j1 to = `` ).

    DATA(str_j5) = `ZZseeZZZyouZZ`.
    DATA(str_j6) = condense( val = str_j5 del = `Z` ).

    "Parameters 'from', 'to' and 'del' are specified: Leading and
    "trailing characters specified in 'del' are first removed. Then,
    "in the remaining string, all substrings composed of characters
    "specified in 'from' are replaced with the first character of the
    "string specified in the 'to' parameter (in the example, it is a
    "blank; the characters 'a', 'b', 'c' are not respected at all).
    DATA(str_j7) = condense( val  = str_j5
                             del  = `Z`
                             from = `Z`
                             to   = ` abc` ).

    out->write( |CONDENSE statements:\n| ).
    out->write( |\n| ).
    out->write( data = char_j1 name = `char_j1` ).
    out->write( |\n| ).
    out->write( data = char_j2 name = `char_j2` ).
    out->write( |\n| ).
    out->write( data = char_j3 name = `char_j3` ).
    out->write( |\n| ).
    out->write( data = str_j name = `str_j` ).
    out->write( |\n| ).
    out->write( zcl_demo_abap_aux=>heading( `String function condense:` ) ).
    out->write( data = str_j2 name = `str_j2` ).
    out->write( |\n| ).
    out->write( data = str_j3 name = `str_j3` ).
    out->write( |\n| ).
    out->write( data = str_j4 name = `str_j4` ).
    out->write( |\n| ).
    out->write( data = str_j6 name = `str_j6` ).
    out->write( |\n| ).
    out->write( data = str_j7 name = `str_j7` ).
  ENDMETHOD.

  METHOD m12_reverse.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Reversing Strings| ).


    DATA(str_k) = reverse( `ollah` ).

    out->write( data = str_k name = `str_k` ).
  ENDMETHOD.

  METHOD m13_insert_substrings.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Inserting Substrings into Strings| ).

    DATA(str_l1) = `abcghi`.

    "Inserting into specific position
    DATA(str_l2) = insert( val = str_l1 sub = `def` off = 3 ).

    "off is optional. If not specified (default value off = 0)
    "the result is like concatenating a string with &&
    DATA(str_l3) = insert( val = str_l1 sub = `def` ).

    DATA(str_l4) = `def` && str_l1.

    out->write( data = str_l2 name = `str_l2` ).
    out->write( |\n| ).
    out->write( data = str_l3 name = `str_l3` ).
    out->write( |\n| ).
    out->write( data = str_l4 name = `str_l4` ).

  ENDMETHOD.

  METHOD m14_overlay.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Overlaying Content| ).

    DATA(incl) = '==============================CP'.
    DATA(cl_name) = 'CL_SOME_CLASS                   '.

    "Addition ONLY is not specified: All blanks are replaced
    OVERLAY cl_name WITH incl.

    DATA(t1) = 'a.b.c.a.b.c.A'.
    DATA(t2) = 'z.x.y.Z.x.y.z'.

    "Addition ONLY is specified: All characters that are specified after ONLY and that
    "occur in the operand are replaced. Note that this is case-sensitive.
    OVERLAY t1 WITH t2 ONLY 'ab'.

    out->write( data = cl_name name = `cl_name` ).
    out->write( |\n| ).
    out->write( data = t1 name = `t1` ).
  ENDMETHOD.

  METHOD m15_process_substrings.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Processing Substrings| ).

    DATA(str_m1) = `Lorem ipsum dolor sit amet`.

    "Extracting substring starting at a specific position
    "'len' not specified means the rest of the remaining characters are
    "respected
    DATA(str_m2) = substring( val = str_m1 off = 6 ).

    "Extracting substring with a specific length
    "'off' is not specified and has the default value 0.
    DATA(str_m3) = substring( val = str_m1 len = 5 ).

    "Specifying both off and len parameters
    DATA(str_m4) = substring( val = str_m1 off = 6 len = 5 ).

    "Excursion: Getting last character of a string
    DATA(str_m5) = substring( val = str_m1
                              off = strlen( str_m1 ) - 1
                              len = 1 ). "t

    "Offset and length specification using the + sign after a variable
    "After +, it is the offset, length is specified within parentheses.
    DATA(str_m6) = str_m1+0(5).

    "* means respecting the rest of the remaining string
    DATA(str_m7) = str_m1+12(*).

    "Excursion: Write access on substrings in fixed-length strings
    DATA(char_m1) = 'Lorem ipsum dolor sit amet'.
    DATA(char_m2) = char_m1.
    DATA(char_m3) = char_m1.

    "Deleting content
    CLEAR char_m2+11(*).
    "Modifying string
    char_m3+0(5) = 'abcde'.

    "More string functions to access substrings
    "Note that lots of parameters are possible (not all covered here).
    DATA(str_m8) = `aa1bb2aa3bb4`.

    "Extracting a substring ...
    "... after a specified substring
    DATA(str_m9) = substring_after( val = str_m8 sub = `aa`  ).

    "... after a specified substring specifying the occurence in a
    "string and restricting the length
    DATA(str_m10) = substring_after( val = str_m8 sub = `aa`
                                     occ = 2 len = 4  ).

    "... before a specified substring
    DATA(str_m11) = substring_before( val = str_m8 sub = `b2` ).

    "... from a specified substring on. It includes the substring
    "specified in sub. len/off and other parameters are possible.
    DATA(str_m12) = substring_from( val = str_m8 sub = `a3` ).

    "... up to a specified substring. It includes the substring
    "specified in sub. len/off and other parameters are possible.
    "aa1bb2aa3b
    DATA(str_m13) = substring_to( val = str_m8 sub = `3b` ).

    out->write( data = str_m2 name = `str_m2` ).
    out->write( |\n| ).
    out->write( data = str_m3 name = `str_m3` ).
    out->write( |\n| ).
    out->write( data = str_m4 name = `str_m4` ).
    out->write( |\n| ).
    out->write( data = str_m5 name = `str_m5` ).
    out->write( |\n| ).
    out->write( data = str_m6 name = `str_m6` ).
    out->write( |\n| ).
    out->write( data = str_m7 name = `str_m7` ).
    out->write( |\n| ).
    out->write( data = char_m2 name = `char_m2` ).
    out->write( |\n| ).
    out->write( data = char_m3 name = `char_m3` ).
    out->write( |\n| ).
    out->write( data = str_m9 name = `str_m9` ).
    out->write( |\n| ).
    out->write( data = str_m10 name = `str_m10` ).
    out->write( |\n| ).
    out->write( data = str_m11 name = `str_m11` ).
    out->write( |\n| ).
    out->write( data = str_m12 name = `str_m12` ).
    out->write( |\n| ).
    out->write( data = str_m13 name = `str_m13` ).
  ENDMETHOD.

  METHOD m16_search_characters.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Searching Specific Characters in Strings Using Comparison Operators and String Functions| ).

    DATA(str_n1) = `cheers`.

    "CA (contains any)
    "sy-fdpos contains the offset of the first found character.
    "If nothing is found, sy-fdpos contains the length of the string.
    "Note that position 0 stands for the very first position.
    IF str_n1 CA `aeiou`.
      out->write( |CA: str_n1 contains any of the characters. | &&
       |The position of the first found character is { sy-fdpos }.| ).
      out->write( |\n| ).
    ELSE.
      out->write( |CA: str_n1 does not contain any of the characters. | &&
             |The length of str_n1 is { sy-fdpos }.| ).
      out->write( |\n| ).
    ENDIF.

    "NA (contains not any)
    IF str_n1 NA `xyz`.
      out->write( |NA: str_n1 does not contain any of the characters.| &&
             |The length of str_n1 is { sy-fdpos }.| ).
      out->write( |\n| ).
    ELSE.
      out->write( |NA: str_n1 contains any of the characters. | &&
       |The position of the first found character is { sy-fdpos }.| ).
      out->write( |\n| ).
    ENDIF.

    "Determining if a string is exclusively composed of a certain
    "character set
    IF str_n1 CO `rs`.
      out->write( |CO: str_n1 contains only the characters. |
      && |The length of str_n1 is { sy-fdpos }.| ).
      out->write( |\n| ).
    ELSE.
      out->write( |CO: str_n1 does not contain only the characters. |
      && |Offset of the first character in str_n1 that is not |
      && |contained in the second operand: { sy-fdpos }.| ).
      out->write( |\n| ).
    ENDIF.

    "Negation of CO
    IF str_n1 CN `chers`.
      out->write( |CN: str_n1 does not contain only the characters. |
       && |Offset of the first character in str_n1 that is |
       && |not contained in the second operand: { sy-fdpos }.| ).
      out->write( |\n| ).
    ELSE.
      out->write( |CN: str_n1 contains only the characters. |
      && |The length of str_n1 is { sy-fdpos }.| ).
      out->write( |\n| ).
    ENDIF.

    "String functions
    DATA(str_n2) = `Pieces of cakes.`.

    "find_end returns the sum of the offset of the occurrence
    DATA(res_n3) = find_end( val = str_n2 sub = `of` ). "9

    "find_any_of returns the offset of the occurrence of any character contained in substring
    "The search is always case-sensitive.
    DATA(res_n4) = find_any_of( val = str_n2 sub = `x523z4e` ).
    DATA(res_n5) = find_any_of( val = str_n2 sub = `zwq85t` ).

    "find_any_not_of: Negation of the one above
    "The search is always case-sensitive.
    DATA(res_n6) = find_any_not_of( val = str_n2 sub = `ieces` ).
    DATA(res_n7) = find_any_not_of( val = str_n2 sub = `P` ).

    "count returns the number of all occurrences
    DATA(res_n8) = count( val = str_n2 sub = `e` ).
    DATA(res_n9) = count( val = str_n2 sub = `x` ).

    "count_any_of
    DATA(res_n10) = count_any_of( val = str_n2 sub = `x523z4e` ).
    DATA(res_n11) = count_any_of( val = str_n2 sub = `eco` ).

    "count_any_not_of
    DATA(res_n12) = count_any_not_of( val = str_n2 sub = `fP` ).
    DATA(res_n13) = count_any_not_of( val = str_n2 sub = `Piecs ofak.` ).

    out->write( data = res_n3 name = `res_n3` ).
    out->write( |\n| ).
    out->write( data = res_n4 name = `res_n4` ).
    out->write( |\n| ).
    out->write( data = res_n5 name = `res_n5` ).
    out->write( |\n| ).
    out->write( data = res_n6 name = `res_n6` ).
    out->write( |\n| ).
    out->write( data = res_n7 name = `res_n7` ).
    out->write( |\n| ).
    out->write( data = res_n8 name = `res_n8` ).
    out->write( |\n| ).
    out->write( data = res_n9 name = `res_n9` ).
    out->write( |\n| ).
    out->write( data = res_n10 name = `res_n10` ).
    out->write( |\n| ).
    out->write( data = res_n11 name = `res_n11` ).
    out->write( |\n| ).
    out->write( data = res_n12 name = `res_n12` ).
    out->write( |\n| ).
    out->write( data = res_n13 name = `res_n13` ).
  ENDMETHOD.

  METHOD m17_replace_characters.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Replacing Specific Characters in Strings| ).

    DATA(str_o1) = `___abc_def_____ghi_`.

    "The replacement is done as follows: Each character specified in
    "'from' is replaced by the character in 'to' that is on the same
    "position, i. e. the second character in 'from' is replaced by the
    "second character specified in 'to'. If there is no equivalent in
    "'to', the character in 'from' is removed from the result.

    "abcdefgZZ
    DATA(str_o2) = translate( val = str_o1 from = `hi_`  to = `ZZ` ).

    "ZZZabcZdefZZZZZghiZ
    DATA(str_o3) = translate( val = str_o1 from = `_`  to = `ZZ` ).

    "TRANSLATE statement. The value after USING is interpreted as a
    "string composed of character pairs. Starting with the first pair,
    "a search is performed in text for the first character in every
    "pair and each occurrence is replaced with the second character of
    "the pair.
    "...Zbc.def.....Yhi.
    TRANSLATE str_o1 USING `_.aZgY`.

    out->write( data = str_o2 name = `str_o2` ).
    out->write( |\n| ).
    out->write( data = str_o3 name = `str_o3` ).
    out->write( |\n| ).
    out->write( data = str_o1 name = `str_o1` ).
  ENDMETHOD.

  METHOD m18_search_substrings_comp_op.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Substring Search: Simple Search Using Comparison Operators| ).

    DATA(str_p1) = `cheers`.

    "CS (contains string)
    "sy-fdpos contains the offset of the found substring.
    "If the substring is not found, sy-fdpos contains the length of the
    "searched string.
    IF str_p1 CS `rs`.
      out->write( |CS: The string contains the substring. |
      && |The offset is { sy-fdpos }.| ).
      out->write( |\n| ).
    ELSE.
      out->write( |CS: The string does not contain the substring. |
      && |The length of the string is { sy-fdpos }.| ).
      out->write( |\n| ).
    ENDIF.

    "NS (contains no string)
    IF str_p1 NS `abc`.
      out->write( |NS: The string does not contain the substring. |
      && |The length of the string is { sy-fdpos }.| ).
      out->write( |\n| ).
    ELSE.
      out->write( |NS: The string contains the substring. |
       && |The offset is { sy-fdpos }.| ).
      out->write( |\n| ).
    ENDIF.

  ENDMETHOD.

  METHOD m19_find_statement.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Substring Search in Strings Using FIND Statements| ).

    "The code examples demonstrate different additions.

    DATA(str_qa) = `She sells seashells by the seashore.`.

    "Determining if a substring is found
    "Simple find statement
    FIND `se` IN str_qa.

    IF sy-subrc = 0.
      out->write( `'se' found in the string` ).
    ELSE.
      out->write( `'se' not found in the string` ).
    ENDIF.

    out->write( |\n| ).

    "Addition SUBSTRING is optional
    FIND SUBSTRING `hi` IN str_qa.

    IF sy-subrc = 0.
      out->write( `'hi' Found in the string` ).
    ELSE.
      out->write( `'hi' not found in the string` ).
    ENDIF.

    out->write( |\n| ).
    out->write( |\n| ).

    "The following examples use the additions MATCH COUNT and MATCH OFFSET
    "to determine the number of occurrences and offset and for display purposes.

    "Addition FIRST OCCURRENCE OF: Explicit specification to search
    "for the first occurrence
    FIND FIRST OCCURRENCE OF `se` IN str_qa
      MATCH COUNT DATA(cnt_q1)
      MATCH OFFSET DATA(off_q2).

    "Omitting FIRST OCCURRENCE OF and ALL OCCURRENCES OF addition means
    "searching for the first occurrence by default; same effect as the
    "previous statement
    FIND `se` IN str_qa
      MATCH COUNT DATA(cnt_q3)
      MATCH OFFSET DATA(off_q4).

    "Addition ALL OCCURRENCES: Searching for all occurrences
    FIND ALL OCCURRENCES OF `se` IN str_qa
      MATCH COUNT DATA(cnt_q5)
      MATCH OFFSET DATA(off_q6). "value for the last occurrence

    "Addition IN SECTION ... OF:
    "Searching in a specified section; both additions OFFSET and LENGTH
    "are specified
    FIND ALL OCCURRENCES OF `se`
      IN SECTION OFFSET 9 LENGTH 5 OF str_qa
      MATCH COUNT DATA(cnt_q7)
      MATCH OFFSET DATA(off_q8).

    "Only LENGTH specified (OFFSET is 0 by default)
    FIND ALL OCCURRENCES OF `se`
      IN SECTION LENGTH 7 OF str_qa
      MATCH COUNT DATA(cnt_q9)
      MATCH OFFSET DATA(off_q10).

    "Only OFFSET specified (LENGTH: up to end of string)
    FIND ALL OCCURRENCES OF `se`
      IN SECTION OFFSET 7 OF str_qa
      MATCH COUNT DATA(cnt_q11).

    "Another string to be searched
    DATA(str_qb) = `abap ABAP abap`.

    "Further additional options for advanced evaluation options

    "Specifying the case-sensitivity of the search
    "Not specifying the CASE addition means RESPECTING CASE is used by default.
    "Here, it is explicitly specified.
    FIND FIRST OCCURRENCE OF `A` IN str_qb
      MATCH OFFSET DATA(off_q12)
      RESPECTING CASE.

    "Making search case-insensitive
    FIND FIRST OCCURRENCE OF `A` IN str_qb
      MATCH OFFSET DATA(off_q13)
      IGNORING CASE.

    "MATCH LENGTH addition
    "The example uses a regular expression: Non-greedy search for
    "a substring starting with lower case a up to an upper case P
    FIND FIRST OCCURRENCE OF PCRE `a.*?P` IN str_qb
      MATCH LENGTH DATA(len_q14) "9
      RESPECTING CASE.

    "RESULTS addition
    "Example: Because of using ALL OCCURRENCES, the data object declared
    "inline automatically has the type match_result_tab
    FIND ALL OCCURRENCES OF `ab` IN str_qb
      RESULTS DATA(res_q15)
      IGNORING CASE.

    "Because of searching for the first occurrence, the data object declared
    "inline automatically has the type match_result
    FIND FIRST OCCURRENCE OF `ab` IN str_qb
      RESULTS DATA(res_q16)
      IGNORING CASE.

    out->write( data = cnt_q1 name = `cnt_q1` ).
    out->write( |\n| ).
    out->write( data = off_q2 name = `off_q2` ).
    out->write( |\n| ).
    out->write( data = cnt_q3 name = `cnt_q3` ).
    out->write( |\n| ).
    out->write( data = off_q4 name = `off_q4` ).
    out->write( |\n| ).
    out->write( data = cnt_q5 name = `cnt_q5` ).
    out->write( |\n| ).
    out->write( data = off_q6 name = `off_q6` ).
    out->write( |\n| ).
    out->write( data = cnt_q7 name = `cnt_q7` ).
    out->write( |\n| ).
    out->write( data = off_q8 name = `off_q8` ).
    out->write( |\n| ).
    out->write( data = cnt_q9 name = `cnt_q9` ).
    out->write( |\n| ).
    out->write( data = off_q10 name = `off_q10` ).
    out->write( |\n| ).
    out->write( data = cnt_q11 name = `cnt_q11` ).
    out->write( |\n| ).
    out->write( data = off_q12 name = `off_q12` ).
    out->write( |\n| ).
    out->write( data = off_q13 name = `off_q13` ).
    out->write( |\n| ).
    out->write( data = len_q14 name = `len_q14` ).
    out->write( |\n| ).
    out->write( data = res_q15 name = `res_q15` ).
    out->write( |\n| ).
    out->write( data = res_q16 name = `res_q16` ).
  ENDMETHOD.

  METHOD m20_find_in_table.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Substring Search in Internal Tables Using FIND ... IN TABLE Statements| ).

    DATA(str_table_r) = VALUE string_table( ( `aZbzZ` ) ( `cdZze` ) ( `Zzzf` ) ( `ghz` ) ).

    "Finding all occurrences in a table
    "Note: res_tab is of type match_result_tab
    "You can also restrict the search range in an internal table; see an example
    "in the context of REPLACE ... IN TABLE
    FIND ALL OCCURRENCES OF `Z`
      IN TABLE str_table_r
      RESULTS DATA(res_r1)
      RESPECTING CASE.

    "Finding the first occurrence in a table
    "Note: res_struc, which is declared inline here, is of type match_result
    FIND FIRST OCCURRENCE OF `Z`
      IN TABLE str_table_r
      RESULTS DATA(res_r2)
      RESPECTING CASE.

    "Alternative to the statement above (storing the information in individual data objects)
    FIND FIRST OCCURRENCE OF `Z`
      IN TABLE str_table_r
      MATCH LINE DATA(line_r3)
      MATCH OFFSET DATA(off_r4)
      MATCH LENGTH DATA(len_r5)
      RESPECTING CASE.

    out->write( data = res_r1 name = `res_r1` ).
    out->write( |\n| ).
    out->write( data = res_r2 name = `res_r2` ).
    out->write( |\n| ).
    out->write( data = line_r3 name = `line_r3` ).
    out->write( |\n| ).
    out->write( data = off_r4 name = `off_r4` ).
    out->write( |\n| ).
    out->write( data = len_r5 name = `len_r5` ).

  ENDMETHOD.

  METHOD m21_find_function.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Substring Search in Strings Using the String Function find| ).

    DATA(str_s) = `Pieces of cakes.`.

    "Searching for substring
    "Returns offset of substring found
    DATA(res_s1) = find( val = str_s sub = `ca` ).

    "Substring not found returns -1
    DATA(res_s2) = find( val = str_s sub = `xy` ).

    "Actual parameter of sub must not be initial when using the find function
    TRY.
        DATA(res_s3) = find( val = str_s sub = `` ).
      CATCH cx_sy_strg_par_val.
        "Nope!
    ENDTRY.

    "The search is case-sensitive by default
    DATA(res_s4) = find( val = str_s sub = `OF` ).
    "Making search case-insensitive
    DATA(res_s5) = find( val = str_s sub = `OF` case = abap_false ).

    "Specifying occ
    DATA(res_s6) = find( val = str_s sub = `c` ).
    DATA(res_s7) = find( val = str_s sub = `c` occ = 2 ).
    DATA(res_s8) = find( val = str_s sub = `e` occ = -1 ).
    DATA(res_s9) = find( val = str_s sub = `e` occ = -3 ).

    "Specifying off and len
    "Specifying a subarea in which a string is searched
    DATA(res_s10) = find( val = str_s sub = `e` off = 5 ).
    DATA(res_s11) = find( val = str_s sub = `e` off = 5 len = 7 ).
    DATA(res_s12) = find( val = str_s sub = `e` len = 2  ).

    out->write( data = res_s1 name = `res_s1` ).
    out->write( |\n| ).
    out->write( data = res_s2 name = `res_s2` ).
    out->write( |\n| ).
    out->write( data = res_s3 name = `res_s3` ).
    out->write( |\n| ).
    out->write( data = res_s4 name = `res_s4` ).
    out->write( |\n| ).
    out->write( data = res_s5 name = `res_s5` ).
    out->write( |\n| ).
    out->write( data = res_s6 name = `res_s6` ).
    out->write( |\n| ).
    out->write( data = res_s7 name = `res_s7` ).
    out->write( |\n| ).
    out->write( data = res_s8 name = `res_s8` ).
    out->write( |\n| ).
    out->write( data = res_s9 name = `res_s9` ).
    out->write( |\n| ).
    out->write( data = res_s10 name = `res_s10` ).
    out->write( |\n| ).
    out->write( data = res_s11 name = `res_s11` ).
    out->write( |\n| ).
    out->write( data = res_s12 name = `res_s12` ).
    out->write( |\n| ).

    "Demonstrating a false range to be searched
    TRY.
        DATA(res_s13) = find( val = str_s sub = `e` off = 5 len = 15 ).
      CATCH cx_sy_range_out_of_bounds.
        out->write( `The exception cx_sy_range_out_of_bounds was raised.` ).
    ENDTRY.
  ENDMETHOD.

  METHOD m22_replace_statement.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Replacing Substrings in Strings Using REPLACE Statments| ).

    DATA(str_t) = `abap ABAP abap`.
    DATA(str_t1) = str_t.

    "Simple REPLACE statement
    "Omitting the FIRST OCCURRENCE and ALL OCCURRENCES OF additions means
    "replacing the first occurrence by default.
    REPLACE `ab` IN str_t1 WITH `##`.

    out->write( data = str_t1 name = `str_t1` ).
    out->write( |\n| ).
    DATA(str_t2) = str_t.

    "Addition SUBSTRING is optional; same effect as the statement above
    REPLACE SUBSTRING `ab` IN str_t2 WITH `##`.

    out->write( data = str_t2 name = `str_t2` ).
    out->write( |\n| ).
    DATA(str_t3) = str_t.

    "Addition FIRST OCCURRENCE OF: Explicit specification to replace the
    "first occurrence; same effect as the statements above
    REPLACE FIRST OCCURRENCE OF `ab` IN str_t3 WITH `##`.

    out->write( data = str_t3 name = `str_t3` ).
    out->write( |\n| ).
    DATA(str_t4) = str_t.

    "Addition ALL OCCURRENCES OF: All occurrences are replaced
    "Note that the replacement is case-sensitive by default.
    REPLACE ALL OCCURRENCES OF `ab` IN str_t4 WITH `##`.

    out->write( data = str_t4 name = `str_t4` ).
    out->write( |\n| ).
    DATA(str_t5) = str_t.

    "Further additional options for advanced evaluation options

    "IGNORING CASE addition: Making replacements case-insensitive
    REPLACE ALL OCCURRENCES OF `ab`
      IN str_t5 WITH `##`
      IGNORING CASE.

    out->write( data = str_t5 name = `str_t5` ).
    out->write( |\n| ).
    DATA(str_t6) = str_t.

    "REPLACEMENT COUNT addition
    REPLACE ALL OCCURRENCES OF `ab`
      IN str_t6 WITH `##`
      REPLACEMENT COUNT DATA(cnt_t7)
      IGNORING CASE.

    out->write( data = str_t6 name = `str_t6` ).
    out->write( |\n| ).
    out->write( data = cnt_t7 name = `cnt_t7` ).
    out->write( |\n| ).
    DATA(str_t8) = str_t.

    "REPLACEMENT OFFSET and LENGTH additions
    REPLACE FIRST OCCURRENCE OF `ap`
      IN str_t8 WITH `##`
      REPLACEMENT COUNT DATA(cnt_t9) "always 1 for replaced first occurrence
      REPLACEMENT OFFSET DATA(off_t10)
      REPLACEMENT LENGTH DATA(len_t11)
      IGNORING CASE.

    out->write( data = str_t8 name = `str_t8` ).
    out->write( |\n| ).
    out->write( data = cnt_t9 name = `cnt_t9` ).
    out->write( |\n| ).
    out->write( data = off_t10 name = `off_t10` ).
    out->write( |\n| ).
    out->write( data = len_t11 name = `len_t11` ).
    out->write( |\n| ).
    DATA(str_t12) = str_t.

    "SECTION ... OF addition: Replacing within a specified area
    REPLACE ALL OCCURRENCES OF `ap`
      IN SECTION OFFSET 4 LENGTH 5
      OF str_t12 WITH `##`
      REPLACEMENT COUNT DATA(cnt_t13)
      REPLACEMENT OFFSET DATA(off_t14)
      REPLACEMENT LENGTH DATA(len_t15)
      IGNORING CASE.

    out->write( data = str_t12 name = `str_t12` ).
    out->write( |\n| ).
    out->write( data = cnt_t13 name = `cnt_t13` ).
    out->write( |\n| ).
    out->write( data = off_t14 name = `off_t14` ).
    out->write( |\n| ).
    out->write( data = len_t15 name = `len_t15` ).
    out->write( |\n| ).
    DATA(str_t16) = str_t.

    "RESULTS additions with ...
    "... ALL OCCURRENCES OF
    "Note: The data object, which is declared inline here, is of type repl_result_tab.
    REPLACE ALL OCCURRENCES OF `ap`
      IN str_t16 WITH `##`
      RESULTS DATA(tab_t17)
      IGNORING CASE.

    out->write( data = str_t16 name = `str_t16` ).
    out->write( |\n| ).
    out->write( data = tab_t17 name = `tab_t17` ).
    out->write( |\n| ).
    DATA(str_t18) = str_t.

    "... FIRST OCCURRENCE OF
    "Note: The data object, which is declared inline here, is of type repl_result.
    REPLACE FIRST OCCURRENCE OF `ap`
      IN str_t18 WITH `##`
      RESULTS DATA(struc_t19)
      IGNORING CASE.

    out->write( data = str_t18 name = `str_t18` ).
    out->write( |\n| ).
    out->write( data = struc_t19 name = `struc_t19` ).
  ENDMETHOD.

  METHOD m23_replace_section.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Position-Based Replacements with REPLACE SECTION ... OF| ).

    DATA(str_u) = `abap ABAP abap`.
    DATA(str_u1) = str_u.

    "OFFSET and LENGTH specified
    REPLACE SECTION OFFSET 5 LENGTH 4 OF str_u1 WITH `#`.

    out->write( data = str_u1 name = `str_u1` ).
    out->write( |\n| ).
    DATA(str_u2) = str_u.

    "Only OFFSET specified, LENGTH: up to the end of the string
    REPLACE SECTION OFFSET 5 OF str_u2 WITH `#`.

    out->write( data = str_u2 name = `str_u2` ).
    out->write( |\n| ).
    DATA(str_u3) = str_u.

    "Only LENGTH specified, OFFSET: starting from the leftmost position
    REPLACE SECTION LENGTH 6 OF str_u3 WITH `#`.

    out->write( data = str_u3 name = `str_u3` ).
  ENDMETHOD.

  METHOD m24_replace_in_table.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Replacements in Internal Tables with REPLACE ... IN TABLE| ).

    DATA(tab_v) = VALUE string_table( ( `aZbzZ` ) ( `cdZze` ) ( `Zzzf` ) ( `ghz` ) ).
    DATA(tab_v1) = tab_v.

    "Replacing all occurrences in a table
    "RESULTS addition: Storing information in an internal table of type repl_result_tab
    REPLACE ALL OCCURRENCES OF `Z`
      IN TABLE tab_v1
      WITH `#`
      RESULTS DATA(res_v2)
      RESPECTING CASE.

    out->write( data = tab_v1 name = `tab_v1` ).
    out->write( |\n| ).
    out->write( data = res_v2 name = `res_v2` ).
    out->write( |\n| ).
    DATA(tab_v3) = tab_v.

    "Replacing the first occurrence in a table
    "RESULTS addition: Storing information in a structure of type repl_result
    REPLACE FIRST OCCURRENCE OF `Z`
      IN TABLE tab_v3
      WITH `#`
      RESULTS DATA(res_v4)
      RESPECTING CASE.

    out->write( data = tab_v3 name = `tab_v3` ).
    out->write( |\n| ).
    out->write( data = res_v4 name = `res_v4` ).
    out->write( |\n| ).
    DATA(tab_v5) = tab_v.

    "Restricting the search range in an internal table
    REPLACE ALL OCCURRENCES OF `Z`
      IN TABLE tab_v5
      FROM 1 TO 2
      WITH `#`
      RESPECTING CASE.

    out->write( data = tab_v5 name = `tab_v5` ).
    out->write( |\n| ).
    DATA(tab_v6) = tab_v.

    "Offsets can be optionally specified (also only the offset of start or end line possible)
    REPLACE ALL OCCURRENCES OF `Z`
      IN TABLE tab_v6
      FROM 1 OFFSET 3 TO 2 OFFSET 2
      WITH `#`
      RESPECTING CASE.

    out->write( data = tab_v6 name = `tab_v6` ).
  ENDMETHOD.

  METHOD m25_replace_function.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Replacing Substrings in Strings Using the String Function replace| ).

    DATA(str_w) = `abap ABAP abap`.

    "Note that here only the first occurrence is replaced.
    DATA(str_w_1) = replace( val = str_w sub = `ap` with = `#` ).

    "Making the search case-insensitive
    DATA(str_w_2) = replace( val = str_w sub = `AB` with = `#` case = abap_false ).

    "Setting occ
    DATA(str_w_3) = replace( val = str_w sub = `ab` with = `#` occ = 2 case = abap_false ).

    "Replacing all occurrences: Setting occ to 0
    DATA(str_w_4) = replace( val = str_w sub = `ab` with = `#` occ = 0 case = abap_false ).

    "Negative value for occ: Occurrences are counted from the right
    DATA(str_w_5) = replace( val = str_w sub = `ab` with = `#` occ = -1  ).

    "Setting off and len for determining a subarea for replacements
    "Note: When using off/len, sub and occ cannot be specified.
    "Specifying both off and len
    DATA(str_w_6) = replace( val = str_w  with = `#` off = 5 len = 3  ).

    "Specifying only off (len is 0 by default)
    DATA(str_w_7) = replace( val = str_w  with = `#` off = 2 ).

    "Note: When specifying only off and not specifying len or len = 0,
    "replace works like insert
    DATA(str_w_8) = insert( val = str_w sub = `#` off = 2  ).

    "Specifying only len (off is 0 by default): First segment of length in len is replaced
    DATA(str_w_9) = replace( val = str_w  with = `#` len = 3 ).

    "Special case
    "- off: equal to the length of the string
    "- len: not specified or 0
    "- Result: Value specified for 'with' is appended to the end of the string
    DATA(str_w_10) = replace( val = str_w  with = `#` off = strlen( str_w ) ).

    out->write( data = str_w_1 name = `str_w_1` ).
    out->write( |\n| ).
    out->write( data = str_w_2 name = `str_w_2` ).
    out->write( |\n| ).
    out->write( data = str_w_3 name = `str_w_3` ).
    out->write( |\n| ).
    out->write( data = str_w_4 name = `str_w_4` ).
    out->write( |\n| ).
    out->write( data = str_w_5 name = `str_w_5` ).
    out->write( |\n| ).
    out->write( data = str_w_6 name = `str_w_6` ).
    out->write( |\n| ).
    out->write( data = str_w_7 name = `str_w_7` ).
    out->write( |\n| ).
    out->write( data = str_w_8 name = `str_w_8` ).
    out->write( |\n| ).
    out->write( data = str_w_9 name = `str_w_9` ).
    out->write( |\n| ).
    out->write( data = str_w_10 name = `str_w_10` ).
  ENDMETHOD.

  METHOD m26_search_pattern_logical_op.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Simple Pattern-Based Searching Using Logical Operators| ).

    DATA(str_x) = `abc_def_ghi`.

    "CP (conforms to pattern)
    "*: Any character sequence (including blanks).
    "+: Any character (only one character, including blanks).
    "#: Escaping symbol. The following character is marked for an exact
    "comparison.
    "Note: Patterns are not case sensitive except for characters marked
    "by #. If a pattern is found, the system variable sy-fdpos returns
    "the offset of the first finding. Otherwise, it contains the length
    "of the searched string.
    IF str_x CP `*f#_*`.
      out->write( |CP: The string covers the pattern. |
      && |The offset is { sy-fdpos }.| ).
    ELSE.
      out->write( |CP: The string does not cover the pattern. |
      && |The length of the string is { sy-fdpos }.| ).
    ENDIF.

    out->write( |\n| ).

    "NP (does not conform to pattern)
    IF str_x NP `i+`.
      out->write( |NP: The string does not cover the pattern. |
      && |The length of the string is { sy-fdpos }.| ).
    ELSE.
      out->write( |NP: The string covers the pattern. |
      && |The offset is { sy-fdpos }.| ).
    ENDIF.
  ENDMETHOD.

  METHOD m27_search_regex.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Complex Searching Using Regular Expressions| ).

    "Also note the cheat sheet example on regular expressions in ABAP.

    DATA(str_y) = `Cathy's black cat was fast asleep on the mat. ` &&
                  `Later that day, the cat played with Matt.`.

    "Using string functions

    "Determining the position of the first finding
    "here, parameter occ is 1 by default
    DATA(off_y1) = find( val = str_y  pcre = `at.` ).

    "Determining the number of findings
    "Considers all 'a' characters not followed by 't',
    "all 'at' plus 'att'
    DATA(cnt_y2) = count( val = str_y  pcre = `at*` ).
    "Considers all 'at' plus 'att' and so on
    DATA(cnt_y3) = count( val = str_y  pcre = `at+` ).

    "String function match
    "Extracting a substring matching a given pattern
    DATA(str_y_email1) = `The email address is jon.doe@email.com.`.
    DATA(str_y4) = match( val   = str_y_email1
                          pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ).

    "Predicate function matches
    "Checking a string if it matches a given pattern
    DATA(str_y_email2) = `jon.doe@email.com`.

    IF matches( val   = str_y_email2
                pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ).
      DATA(str_y5) = |{ str_y_email2 } is a valid email address.|.
    ELSE.
      str_y5 = |{ str_y_email2 } is not a valid email address.|.
    ENDIF.

    "Example with a false email
    DATA(str_y_email3) = `jon.doe@email.abcdef`.

    IF matches( val   = str_y_email3
                pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ).
      DATA(str_y6) = |{ str_y_email3 } is a valid email address.|.
    ELSE.
      str_y6 = |{ str_y_email3 } is not a valid email address.|.
    ENDIF.

    "Examples with the FIND statement
    "Storing submatches in variables.
    "Pattern: anything before and after ' on '
    FIND PCRE `(.*)\son\s(.*)` IN str_y
      SUBMATCHES DATA(subm_y7) DATA(subm_y8)
      IGNORING CASE.

    "Determining the number of letters in a string
    FIND ALL OCCURRENCES OF PCRE `[A-Za-z]`
      IN str_y
      MATCH COUNT DATA(cnt_y9).

    "Extracting all findings of a certain pattern in a string and
    "storing them in an internal table
    DATA tab_y10 TYPE string_table.

    "Pattern: An 'a' followed by any two characters
    FIND ALL OCCURRENCES OF PCRE `a..` IN str_y
      RESULTS DATA(res_y11).

    "The internal table includes the offset and length information of the individual findings.
    "The substrings are extracted from the original string based on that information and
    "added to an internal table of type string.
    LOOP AT res_y11 ASSIGNING FIELD-SYMBOL(<fs_y>).
      APPEND substring( val = str_y off = <fs_y>-offset len = <fs_y>-length ) TO tab_y10.
    ENDLOOP.

    "Searching in an internal table and retrieving line, offset, length information
    DATA(tab_y12) = VALUE string_table( ( `Cathy's black cat on the mat played with the friend of Matt.` )
                                        ( `Later that day, the cat played with Matt.` ) ).

    "Pattern: any character + 'y' followed by any character that is not a word character
    "Only the first occurrence is searched. The search is specified as case-insensitive (which is not relevant here).
    FIND FIRST OCCURRENCE OF PCRE `.y\W` IN TABLE tab_y12
       MATCH LINE DATA(line_y13)
       MATCH OFFSET DATA(off_y14)
       MATCH LENGTH DATA(len_y15)
       IGNORING CASE.

    "Pattern: any character + 'y' followed by any character that is not a word character
    "Here, all occurrences are searched and the result is stored in an internal table specified
    "after the RESULTS addition. Since a group is included in the PCRE pattern denoted by the
    "parentheses (\W), the resulting internal table includes entries in the 'submatches'
    "component holding offset/length information for the particular match.
    FIND ALL OCCURRENCES OF PCRE `.y(\W)` IN TABLE tab_y12
      RESULTS DATA(res_y16)
      IGNORING CASE.

    "Extracting all findings of certain patterns in a string and
    "storing them in an internal table; the capturing groups are
    "also evaluated
    DATA tab_y17 TYPE string_table.
    DATA(str_y18) = `az.ay.bx.bw.cv.cu.dt.ds.ar.bq`.

    FIND ALL OCCURRENCES OF PCRE `(a.)|(b.)|(c.)`
      IN str_y18
      RESULTS DATA(res_y19)
      IGNORING CASE.

    LOOP AT res_y19 ASSIGNING FIELD-SYMBOL(<fs_y20>).
      LOOP AT <fs_y20>-submatches ASSIGNING FIELD-SYMBOL(<fs_y21>).
        IF <fs_y21>-offset <> -1.
          APPEND |Substring "{ substring( val = str_y18 off = <fs_y21>-offset len = <fs_y21>-length ) }" found, capturing group { sy-tabix }| TO tab_y17.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    out->write( data = off_y1 name = `off_y1` ).
    out->write( |\n| ).
    out->write( data = cnt_y2 name = `cnt_y2` ).
    out->write( |\n| ).
    out->write( data = cnt_y3 name = `cnt_y3` ).
    out->write( |\n| ).
    out->write( data = str_y4 name = `str_y4` ).
    out->write( |\n| ).
    out->write( data = str_y5 name = `str_y5` ).
    out->write( |\n| ).
    out->write( data = str_y6 name = `str_y6` ).
    out->write( |\n| ).
    out->write( data = subm_y7 name = `subm_y7` ).
    out->write( |\n| ).
    out->write( data = subm_y8 name = `subm_y8` ).
    out->write( |\n| ).
    out->write( data = cnt_y9 name = `cnt_y9` ).
    out->write( |\n| ).
    out->write( data = tab_y10 name = `tab_y10` ).
    out->write( |\n| ).
    out->write( data = line_y13 name = `line_y13` ).
    out->write( |\n| ).
    out->write( data = off_y14 name = `off_y14` ).
    out->write( |\n| ).
    out->write( data = len_y15 name = `len_y15` ).
    out->write( |\n| ).
    out->write( data = res_y16 name = `res_y16` ).
    out->write( |\n| ).
    out->write( data = tab_y17 name = `tab_y17` ).
  ENDMETHOD.

  METHOD m28_replacements_using_regex.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Replacing Using Regular Expressions| ).

    DATA(str_z) = `Cathy's black cat was fast asleep on the mat. ` &&
                   `Later that day, the cat played with Matt.`.

    "Considers all 'a' characters not followed by 't',
    "all 'at' plus 'att'
    "occ = 0 -> replaces all occurrences
    DATA(str_z1) = replace( val  = str_z
                            pcre = `at*`
                            with = `#`
                            occ  = 0 ).

    "Considers all 'at' plus 'att'
    DATA(str_z2) = replace( val  = str_z
                            pcre = `at+`
                            with = `#`
                            occ  = 0 ).

    "Replaces 2 'e' characters in a row
    DATA(str_z3) = replace( val  = str_z
                            pcre = `e{2}`
                            with = `#`
                            occ = 0 ).

    "Replaces 'ay'. Preceding d is optional ('day' is replaced too)
    DATA(str_z4) = replace( val  = str_z
                            pcre = `d?ay`
                            with = `#`
                            occ  = 0 ).

    "Subgroup specified, replacement happens if 'at' is followed
    "by 'h' or 't'
    DATA(str_z5) = replace( val  = str_z
                            pcre = `at(h|t)`
                            with = `#`
                            occ  = 0 ).

    "Replaces 'at' when followed by any whitespace character
    DATA(str_z6) = replace( val  = str_z
                            pcre = `at\s`
                            with = `#`
                            occ = 0 ).

    "Replacement starts at beginning of string that is followed by 'c'
    "Marked as not case sensitive
    "Instead of ^, you could also use \A
    DATA(str_z7) = replace( val  = str_z
                            pcre = `^c`
                            with = `#`
                            case = abap_false ).

    "Replacement starts at end of string
    "Instead of $, you could also use \Z
    DATA(str_z8) = replace( val  = str_z
                            pcre = `$`
                            with = ` Awesome!` ).

    "Replaces words starting with 'ma', ending with another character
    DATA(str_z9) = replace( val  = str_z
                            pcre = `\bma.\b`
                            with = `#`
                            case = abap_false
                            occ  = 0 ).

    "Replaces the beginning of words with 'ma' followed by another
    "character.
    "Marked as not case sensitive, hence 'Mat' is considered, too.
    DATA(str_z10) = replace( val  = str_z
                             pcre = `\bma.`
                             with = `#`
                             case = abap_false
                             occ  = 0 ).

    "Replaces a specified set of literals; if 'case = abap_false'
    "is not specified, case sensitivity is respected
    DATA(str_z11) = replace( val = str_z
                             pcre = `[eC'.,]`
                             with = `#`
                             occ = 0 ).

    "Replaces a specified value range
    DATA(str_z12) = replace( val  = str_z
                             pcre = `[a-eA-C0-9]`
                             with = `#`
                             occ  = 0 ).

    "Replaces a specified value range. The example is the negation
    "of the previous example.
    DATA(str_z13) = replace( val  = str_z
                             pcre = `[^a-eA-C0-9]`
                             with = `#`
                             occ = 0 ).

    DATA(str_zb) = `<p><i>Date:</i> 12/16/2022</p>` &&
                     `<br><p>Time: 10:30</p>`.

    "Replacements considering subgroups
    "Example switches the date format from US to European
    "Sequences of digits are specified as subgroups followed by /
    DATA(str_z15) = replace( val  = str_zb
                             pcre = `(\d+)/(\d+)/`
                             with = `$2.$1.` ).

    "Regex pitfall: Watch greediness when using PCRE expressions
    "Example: Replacing all HTML tags in a string
    DATA(str_z16) = replace( val  = str_zb
                             pcre = `<.*>`
                             with = ``
                             occ  = 0 ). "Whole string replaced

    "The following pattern considers '<' not followed by '>' which is
    "specified in a negated definition for a single character in the
    "brackets. Then '*' greedily, matches anything until the next '>'.
    DATA(str_z17) = replace( val  = str_zb
                             pcre = `<[^>]*>`
                             with = ``
                             occ = 0 ).

    "Positive lookahead: Replaces colons followed by digits
    DATA(str_z18) = replace( val  = str_zb
                             pcre = `:(?=\d+)`
                             with = `.`
                             occ  = 0 ).

    "Negative lookahead: Removes colons not followed by digits
    ":(?!(\d+))
    DATA(str_z19) = replace( val  = str_zb
                             pcre = `:(?!\d+)`
                             with = ``
                             occ  = 0 ).

    "Positive Lookbehind: Replaces a digit preceded by a blank
    DATA(str_z20) = replace( val  = str_zb
                             pcre = `(?<=\s)\d`
                             with = `0`
                             occ  = 0 ).

    "Negative lookbehind: Determines the position before closing p tags
    "not preceded by 4 digits
    DATA(str_z21) = replace( val  = str_zb
                             pcre = `(?<!\d{4})(<\/p>)`
                             with = `:00$1`
                             occ  = 0 ).

    DATA(str_zc) = `ab apppc app`.

    "Greedy search
    "The pattern matches anything before 'p'. The matching is carried
    "out as often as possible. Hence, in this example the match
    "stretches until the end of the string since 'p' is the final
    "character, i. e. this 'p' and anything before is replaced).
    DATA(str_z23) = replace( val  = str_zc
                             pcre = `.*p`
                             with = `#` ).

    "Non-greedy search (denoted by '?' below)
    "The pattern matches anything before 'p'. The matching proceeds
    "until the first 'p' is found and does not go beyond (matching as
    "few as possible). Hence, the first found 'p' including the content
    "before is replaced.
    DATA(str_z24) = replace( val  = str_zc
                             pcre = `.*?p`
                             with = `#` ).

    "Replacements with subgroups
    "Replaces 'PP' (case-insensitive) with '#', the content before and
    "after 'PP' is switched
    DATA(str_z25) = replace( val  = str_zc
                             pcre = `(.*?)PP(.*)`
                             with = `$2#$1`
                             case = abap_false ).

    "REPLACE statement: Changing the source field directly
    REPLACE PCRE `(.*?)PP(.*)` IN str_zc WITH `$2#$1` IGNORING CASE.

    "Replacements in internal tables
    DATA(itab_z) = VALUE string_table( ( `Cathy's black cat was fast asleep on the mat.` )
                                       ( `Later that day, the cat played with Matt.` )
                                       ( `How about that?` ) ).

    "Replaces all 'th' occurrences in words beginning with 'th'
    REPLACE ALL OCCURRENCES OF PCRE `\bth`
     IN TABLE itab_z WITH `#`
     REPLACEMENT COUNT DATA(cnt_z26)
     IGNORING CASE .

    out->write( data = |Original str_z: { str_z }\n| ).
    out->write( |\n| ).
    out->write( data = str_z1 name = `str_z1` ).
    out->write( |\n| ).
    out->write( data = str_z2 name = `str_z2` ).
    out->write( |\n| ).
    out->write( data = str_z3 name = `str_z3` ).
    out->write( |\n| ).
    out->write( data = str_z4 name = `str_z4` ).
    out->write( |\n| ).
    out->write( data = str_z5 name = `str_z5` ).
    out->write( |\n| ).
    out->write( data = str_z6 name = `str_z6` ).
    out->write( |\n| ).
    out->write( data = str_z7 name = `str_z7` ).
    out->write( |\n| ).
    out->write( data = str_z8 name = `str_z8` ).
    out->write( |\n| ).
    out->write( data = str_z9 name = `str_z9` ).
    out->write( |\n| ).
    out->write( data = str_z10 name = `str_z10` ).
    out->write( |\n| ).
    out->write( data = str_z11 name = `str_z11` ).
    out->write( |\n| ).
    out->write( data = str_z12 name = `str_z12` ).
    out->write( |\n| ).
    out->write( data = str_z3 name = `str_z13` ).
    out->write( |\n| ).
    out->write( data = |Original str_zb: { str_zb }\n| ).
    out->write( |\n| ).
    out->write( data = str_z15 name = `str_z15` ).
    out->write( |\n| ).
    out->write( data = str_z16 name = `str_z16` ).
    out->write( |\n| ).
    out->write( data = str_z17 name = `str_z17` ).
    out->write( |\n| ).
    out->write( data = str_z18 name = `str_z18` ).
    out->write( |\n| ).
    out->write( data = str_z19 name = `str_z19` ).
    out->write( |\n| ).
    out->write( data = str_z20 name = `str_z20` ).
    out->write( |\n| ).
    out->write( data = str_z21 name = `str_z21` ).
    out->write( |\n| ).
    out->write( data = |Original str_zc: { str_zc }\n| ).
    out->write( |\n| ).
    out->write( data = str_z23 name = `str_z23` ).
    out->write( |\n| ).
    out->write( data = str_z24 name = `str_z24` ).
    out->write( |\n| ).
    out->write( data = str_z25 name = `str_z25` ).
    out->write( |\n| ).
    out->write( data = str_zc name = `str_zc` ).
    out->write( |\n| ).
    out->write( data = itab_z name = `itab_z` ).
    out->write( |\n| ).
    out->write( data = |Number of replacements in itab (cnt_z26): { cnt_z26 }| ).
  ENDMETHOD.

  METHOD m29_regex_system_classes.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: System Classes for Regular Expressions| ).

    "Searching for all occurrences
    DATA(some_string) = `a1 # B2 ? cd . E3`.

    "Creating a regex instance for PCRE regular expressions
    "In the example, regex_inst has the type ref to cl_abap_regex.
    DATA(regex_inst) =  cl_abap_regex=>create_pcre( pattern = `\D\d`           "any-non digit followed by a digit
                                                    ignore_case = abap_true ).

    "Creating an instance of CL_ABAP_MATCHER using the method CREATE_MATCHER of the class CL_ABAP_REGEX
    DATA(matcher_1) = regex_inst->create_matcher( text = some_string ).

    "Searching for all occurrences using the 'find_all' method
    "In the example, result has the type match_result_tab containing the findings.
    DATA(result1) = matcher_1->find_all( ).

    out->write( data = result1 name = `result1` ).
    out->write( |\n| ).

    "You can also use method chaining to save lines of code
    DATA(result2) = cl_abap_regex=>create_pcre( pattern = `\s\w`       "any blank followed by any word character
                                                ignore_case = abap_true )->create_matcher( text = some_string )->find_all( ).

    out->write( data = result2 name = `result2` ).
    out->write( |\n| ).

    "Retrieving submatches using the 'get_submatch' method
    DATA str_tab_reg_find TYPE string_table.

    DATA(matcher_2) = cl_abap_regex=>create_pcre( pattern = 'A(.*?)B(.*?)C(.*?)'
                                                  ignore_case = abap_false )->create_matcher( text = 'A.12az.B.34by.C.56cx.D.78dw.E' ).

    IF matcher_2->match( ).
      DO.
        TRY.
            APPEND matcher_2->get_submatch( sy-index ) TO str_tab_reg_find.
          CATCH cx_sy_invalid_submatch cx_sy_no_current_match.
            EXIT.
        ENDTRY.
      ENDDO.
    ENDIF.

    out->write( data = str_tab_reg_find name = `str_tab_reg_find` ).
  ENDMETHOD.

  METHOD m30_distance_function.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Checking the Similarity of Strings| ).


    DATA(str_to_check) = `abap`.
    DATA(dist1) = distance( val1 = str_to_check val2 = `abap` ).
    DATA(dist2) = distance( val1 = str_to_check val2 = `axbap` ).
    DATA(dist3) = distance( val1 = str_to_check val2 = `yabyyapy` ).
    DATA(dist4) = distance( val1 = str_to_check val2 = `zabapzzzzzzzzzzzz` max = 5 ).

    out->write( data = dist1 name = `dist1` ).
    out->write( |\n| ).
    out->write( data = dist2 name = `dist2` ).
    out->write( |\n| ).
    out->write( data = dist3 name = `dist3` ).
    out->write( |\n| ).
    out->write( data = dist4 name = `dist4` ).
    out->write( |\n| ).

    "If the value of max is 0 or less, an exception is raised.
    TRY.
        DATA(dist5) = distance( val1 = str_to_check val2 = `#ab#ap#` max = 0 ).
      CATCH cx_sy_strg_par_val INTO DATA(dist_err).
        out->write( data = dist_err->get_text( ) name = `dist_err->get_text( )` ).
    ENDTRY.
  ENDMETHOD.

  METHOD m31_repeat_function.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Repeating Strings| ).

    DATA(repeat1) = repeat( val = `abap` occ = 5 ).
    DATA(repeat2) = |#{ repeat( val = ` ` occ = 10 ) }#|.
    DATA(repeat3) = COND #( WHEN repeat( val = `a` occ = 0 ) = `` THEN `Y` ELSE `Z` ).

    out->write( data = repeat1 name = `repeat1` ).
    out->write( |\n| ).
    out->write( data = repeat2 name = `repeat2` ).
    out->write( |\n| ).
    out->write( data = repeat3 name = `repeat2` ).
    out->write( |\n| ).

    "If occ has a negative value, an exception is raised.
    TRY.
        DATA(repeat4) = repeat( val = `X` occ = -3 ).
      CATCH cx_sy_strg_par_val INTO DATA(rep_err).
        out->write( data = rep_err->get_text( ) name = `rep_err->get_text( )` ).
    ENDTRY.
  ENDMETHOD.

  METHOD m32_cmin_cmax_functions.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Returning the Smallest/Biggest of a Set of Character-Like Arguments| ).

    DATA(min) =  cmin( val1 = `zzzzzzz`
                   val2 = `zzazzzzzzzz`
                   val3 = `zzzzabc` ).

    DATA(max) =  cmax( val1 = `abcdef`
                       val2 = `aaghij`
                       val3 = `aaaaklmn`
                       val4 = `aaaaaaopqrs`
                       val5 = `aaaaaaaaaatuvwxy`
                       val6 = `aaaaaaaaaaaaaz` ).

    out->write( data = min name = `min` ).
    out->write( |\n| ).
    out->write( data = max name = `max` ).
  ENDMETHOD.

  METHOD m33_escape_function.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Escaping Special Characters| ).

    "Context: URLs
    DATA(esc1) = escape( val    = '...test: 5@8...'
                         format = cl_abap_format=>e_url_full ).

    "Context: JSON
    DATA(esc2) = escape( val    = 'some "test" json \ with backslash and double quotes'
                         format = cl_abap_format=>e_json_string ).

    "Context: String templates
    DATA(esc3) = escape( val    = 'Special characters in string templates: |, \, {, }'
                         format = cl_abap_format=>e_string_tpl ).

    out->write( data = esc1 name = `esc1` ).
    out->write( |\n| ).
    out->write( data = esc2 name = `esc2` ).
    out->write( |\n| ).
    out->write( data = esc3 name = `esc3` ).
    out->write( |\n| ).

    "Invalid value for the format parameter
    TRY.
        DATA(esc4) = escape( val    = 'This will raise an exception due to an invalid format value.'
                             format = 123 ).
      CATCH cx_sy_strg_par_val INTO DATA(esc_err).
        out->write( data = esc_err->get_text( ) name = `esc_err->get_text( )` ).
    ENDTRY.

  ENDMETHOD.

  METHOD m34_string_processing_xco.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Excursion: String Processing Using the XCO Library| ).

    "--------- Extracting a substring from a string ---------
    DATA(abc) = `abcdefghijklmnopqrstuvwxyz`.

    "Creating an encapsulation of a string using XCO
    DATA(str) = xco_cp=>string( abc ).

    "Using the FROM and TO methods, you can determine
    "the character position. Note that the value includes the
    "character at the position specified.
    "The character index pattern for the example string above
    "is (the string has 26 characters in total):
    "a = 1, b = 2, c = 3 ... z = 26
    "a = -26, b = -25, c = -24 ... z = -1
    "Providing a value that is out of bounds means that
    "the first (or the last) character of the string is used
    "by default.
    "Note: When combining FROM and TO, e.g. with method
    "chaining ...->from( ...)->to( ... ), note that another
    "instance is created with the first 'from', and another
    "character index pattern is created based on the new
    "and adjusted string value.

    DATA(sub1) = str->from( 2 )->value.
    DATA(sub2) = str->from( -23 )->value.
    DATA(sub3) = str->from( -5 )->value.
    DATA(sub4) = str->to( 5 )->value.
    DATA(sub5) = str->to( -25 )->value.
    DATA(sub6) = str->from( 2 )->to( 6 )->value.
    DATA(sub7) = str->to( -10 )->from( 4 )->value.
    "Values that are out of bounds.
    DATA(sub8) = str->from( 0 )->to( 100 )->value.

    out->write( data = sub1 name = `sub1` ).
    out->write( |\n| ).
    out->write( data = sub2 name = `sub2` ).
    out->write( |\n| ).
    out->write( data = sub3 name = `sub3` ).
    out->write( |\n| ).
    out->write( data = sub4 name = `sub4` ).
    out->write( |\n| ).
    out->write( data = sub5 name = `sub5` ).
    out->write( |\n| ).
    out->write( data = sub6 name = `sub6` ).
    out->write( |\n| ).
    out->write( data = sub7 name = `sub7` ).
    out->write( |\n| ).
    out->write( data = sub8 name = `sub8` ).
    out->write( |\n| ).

    "--------- Splitting and joining ---------

    "Splitting a string into a string table
    DATA(str_table) = xco_cp=>string( `Hello.World.ABAP` )->split( `.` )->value.

    "Concatenating a string table into a string; specifying a delimiter
    str_table = VALUE #( ( `a` ) ( `b` ) ( `c` ) ).

    DATA(conc_str1) = xco_cp=>strings( str_table )->join( `, ` )->value.

    "Concatenating a string table into a string; specifying a delimiter and
    "reversing the table order
    DATA(conc_str2) = xco_cp=>strings( str_table )->reverse( )->join( ` / ` )->value.

    out->write( data = str_table name = `str_table` ).
    out->write( |\n| ).
    out->write( data = conc_str1 name = `conc_str1` ).
    out->write( |\n| ).
    out->write( data = conc_str2 name = `conc_str2` ).
    out->write( |\n| ).

    "--------- Prepending and appending strings ---------
    DATA(name) = xco_cp=>string( `Max Mustermann` ).
    DATA(address) = name->append( `, Some Street 1, 12345 Someplace` )->value.
    DATA(title) = name->prepend( `Mr. ` )->value.

    out->write( data = address name = `address` ).
    out->write( |\n| ).
    out->write( data = title name = `title` ).
    out->write( |\n| ).

    "--------- Transforming to lowercase and uppercase ---------
    DATA(to_upper) = xco_cp=>string( `abap` )->to_upper_case( )->value.
    DATA(to_lower) = xco_cp=>string( `HALLO WORLD` )->to_lower_case( )->value.

    out->write( data = to_upper name = `to_upper` ).
    out->write( |\n| ).
    out->write( data = to_lower name = `to_lower` ).
    out->write( |\n| ).

    "--------- Checking if a string starts/ends with a specific string ---------
    DATA check TYPE string.
    DATA(str_check) = xco_cp=>string( `Max Mustermann` ).

    "yes
    IF str_check->ends_with( `mann` ).
      check = `yes`.
    ELSE.
      check = `no`.
    ENDIF.

    out->write( data = check name = `check` ).
    out->write( |\n| ).

    "no
    IF str_check->starts_with( `John` ).
      check = `yes`.
    ELSE.
      check = `no`.
    ENDIF.

    out->write( data = check name = `check` ).
    out->write( |\n| ).

    "--------- Converting strings to xstrings using a codepage ---------
    DATA(xstr) = xco_cp=>string( `Some string` )->as_xstring( xco_cp_character=>code_page->utf_8 )->value.

    out->write( data = xstr name = `xstr` ).
    out->write( |\n| ).

    "--------- Camel case compositions and decompositions with split and join operations ---------
    "Pascal case is also possible
    DATA(comp) = xco_cp=>string( `some_value` )->split( `_` )->compose( xco_cp_string=>composition->camel_case )->value.
    DATA(decomp) = xco_cp=>string( `someValue` )->decompose( xco_cp_string=>decomposition->camel_case )->join( `_` )->value.

    out->write( data = comp name = `comp` ).
    out->write( |\n| ).
    out->write( data = decomp name = `decomp` ).
    out->write( |\n| ).

    "--------- Matching string against regular expression ---------
    DATA match TYPE string.

    "yes
    IF xco_cp=>string( ` 1` )->matches( `\s\d` ).
      match = 'yes'.
    ELSE.
      match = 'no'.
    ENDIF.

    out->write( data = match name = `match` ).
    out->write( |\n| ).

    "no
    IF xco_cp=>string( ` X` )->matches( `\s\d` ).
      match = 'yes'.
    ELSE.
      match = 'no'.
    ENDIF.

    out->write( data = match name = `match` ).

  ENDMETHOD.

  METHOD m35_byte_string_processing.

    zcl_demo_abap_aux=>set_example_divider(  out  = out text = |{ text }: Byte String Processing| ).

    "--- Determining the Length of xstrings ---

    DATA(hi) = `Hello world`.

    DATA(conv_xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( hi ).

    DATA(len_str) = strlen( CONV string( conv_xstring ) ).
    out->write( data = len_str name = `len_str` ).

    DATA(len_xstr) = xstrlen( conv_xstring ).
    out->write( data = len_xstr name = `len_xstr` ).

***********************************************************************

    "--- Character String and Byte String Processing with ABAP Statements ---

    DATA off TYPE i.
    DATA(abc_str) = `abc def ghi jkl mno pqr stu vwx yz`.
    DATA(copy_str) = abc_str.

    "-----------------------------------------------------------------------------
    "------------------------ FIND and REPLACE statements ------------------------
    "-----------------------------------------------------------------------------

    "------------------------ IN CHARACTER MODE addition ------------------------

    "Searching for the first blank in the string
    FIND ` ` IN abc_str IN CHARACTER MODE MATCH OFFSET off.

    "The following example is the same as the previous as the IN CHARACTER MODE
    "addition is optional. The FIRST OCCURRENCE OF addition is also optional.
    "Just using FIND without FIRST OCCURRENCE OF means searching for the first
    "occurrence by default.
    FIND FIRST OCCURRENCE OF ` ` IN abc_str MATCH OFFSET off.

    "Searching for all blanks in the string
    FIND ALL OCCURRENCES OF ` ` IN abc_str IN CHARACTER MODE RESULTS DATA(res).
    DATA(offsets_of_findings) = concat_lines_of(
      table = VALUE string_table( FOR wa IN res ( condense( val = CONV string( wa-offset ) to = `` ) ) )  sep = `, ` ).

    "Replacing the first blank in the string
    REPLACE ` ` IN abc_str WITH `#` IN CHARACTER MODE.

    abc_str = copy_str.

    "Replacing all blanks in the string
    REPLACE ALL OCCURRENCES OF ` ` IN abc_str WITH `#` IN CHARACTER MODE.

    abc_str = copy_str.

    "------------------------ IN BYTE MODE addition ------------------------

    "Converting to xstring
    DATA(abc_xstr) = cl_abap_conv_codepage=>create_out( )->convert( abc_str ).
    DATA(blank_xstr) = cl_abap_conv_codepage=>create_out( )->convert( ` ` ).
    DATA(repl_xstr) = cl_abap_conv_codepage=>create_out( )->convert( `#` ).
    DATA(copy_xstr) = abc_xstr.

    "Searching for the first byte that represents a blank in the UTF-8 code page
    FIND blank_xstr IN abc_xstr IN BYTE MODE MATCH OFFSET off.

    FIND ALL OCCURRENCES OF blank_xstr IN abc_xstr IN BYTE MODE RESULTS res.
    DATA(offsets_of_findings_xstr) = concat_lines_of(
      table = VALUE string_table( FOR wa IN res ( condense( val = CONV string( wa-offset ) to = `` ) ) )  sep = `, ` ).

    "Replacing the first byte that represents a blank in the UTF-8 code page
    REPLACE blank_xstr IN abc_xstr WITH repl_xstr IN BYTE MODE.

    abc_xstr = copy_xstr.

    "Replacing all bytes that represent a blank in the UTF-8 code page
    REPLACE ALL OCCURRENCES OF blank_xstr IN abc_xstr WITH repl_xstr IN BYTE MODE.

    "-----------------------------------------------------------------------------
    "--------------------------- CONCATENATE statements --------------------------
    "-----------------------------------------------------------------------------

    DATA(part_str1) = `abc`.
    DATA(part_str2) = `def`.

    CONCATENATE part_str1 part_str2 INTO DATA(concat_str) IN CHARACTER MODE.

    CONCATENATE part_str1 part_str2 INTO concat_str SEPARATED BY `/` IN CHARACTER MODE.

    "Same as above
    CONCATENATE part_str1 part_str2 INTO concat_str.
    CONCATENATE part_str1 part_str2 INTO concat_str SEPARATED BY `/`.

    DATA(part_xstr1) = cl_abap_conv_codepage=>create_out( )->convert( part_str1 ).
    DATA(part_xstr2) = cl_abap_conv_codepage=>create_out( )->convert( part_str2 ).
    DATA(sep_xstr) = cl_abap_conv_codepage=>create_out( )->convert( `/` ).

    CONCATENATE part_xstr1 part_xstr2 INTO DATA(concat_xstr) IN BYTE MODE.

    DATA(concat_xstr_converted) = cl_abap_conv_codepage=>create_in( )->convert( concat_xstr ).

    CONCATENATE part_xstr1 part_xstr2 INTO concat_xstr SEPARATED BY sep_xstr IN BYTE MODE.

    concat_xstr_converted = cl_abap_conv_codepage=>create_in( )->convert( concat_xstr ).

    "Creating a table of type xstring
    DATA(xstr_table) = VALUE xstring_table( ( part_xstr1 ) ( part_xstr2 ) ).

    CONCATENATE LINES OF xstr_table INTO DATA(concat_xstr_tab) IN BYTE MODE.

    DATA(concat_xstr_tab_converted) = cl_abap_conv_codepage=>create_in( )->convert( concat_xstr_tab ).

    "-----------------------------------------------------------------------------
    "------------------------------- SHIFT statements ----------------------------
    "-----------------------------------------------------------------------------

    DATA(test_string) = `abcdef`.
    DATA(copy) = test_string.

    SHIFT test_string IN CHARACTER MODE.

    test_string = copy.

    "Same as
    SHIFT test_string.

    test_string = copy.

    DATA(test_xstr) = cl_abap_conv_codepage=>create_out( )->convert( test_string ).

    SHIFT test_xstr IN BYTE MODE.

    "-----------------------------------------------------------------------------
    "------------------------------- SPLIT statements ----------------------------
    "-----------------------------------------------------------------------------

    test_string = `abc def`.

    SPLIT test_string AT space INTO DATA(test_str1) DATA(test_str2) IN CHARACTER MODE.
    SPLIT test_string AT space INTO TABLE DATA(tab_str) IN CHARACTER MODE.

    "Same as above
    SPLIT test_string AT space INTO test_str1 test_str2.
    SPLIT test_string AT space INTO TABLE tab_str.

    test_xstr = cl_abap_conv_codepage=>create_out( )->convert( test_string ).

    blank_xstr = cl_abap_conv_codepage=>create_out( )->convert( ` ` ).

    SPLIT test_xstr AT blank_xstr INTO DATA(xstr1) DATA(xstr2) IN BYTE MODE.
    SPLIT test_xstr AT blank_xstr INTO TABLE DATA(xstr_tab) IN BYTE MODE.
  ENDMETHOD.

ENDCLASS.
