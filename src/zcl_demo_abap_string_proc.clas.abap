***********************************************************************
*
*              ABAP cheat sheet: String processing
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntactical options for processing
*   character strings as outlined in the respective ABAP cheat sheet.
* - Topics covered: creating strings and assigning values, chaining strings,
*   string templates, concatenating/splitting/modifying strings, searching
*   and replacing, regular expressions
*
* ----------------------- GETTING STARTED -----------------------------
* - Open the class with the ABAP Development Tools (ADT).
* - Choose F9 to run the class.
* - Check the console output.
* - To understand the context and the ABAP syntax used, check the notes
*   included in the class as comments or refer to the respective topic
*   in the ABAP Keyword Documentation.
* - Due to the amount of output in the console, the examples include
*   numbers (e. g. 1) ..., 2) ..., 3) ...) for the individual example
*   sections. Plus, the variable name is displayed in most cases. Hence,
*   to easier and faster find the relevant output in the console, just
*   search in the console for the number/variable name (STRG+F in the
*   console) or use the debugger.
*
* ----------------------------- NOTE -----------------------------------
* The code presented in this class is only meant for supporting the ABAP
* cheat sheets. It is not intended for direct use in a
* production system environment. The code examples in the ABAP cheat
* sheets are primarily intended to provide a better explanation and
* visualization of the syntax and semantics of ABAP statements and not to
* solve concrete programming tasks. For production application programs,
* a dedicated solution should therefore always be worked out for each
* individual case. There is no guarantee for either the correctness or
* the completeness of the code. In addition, there is no legal
* responsibility or liability for possible errors or their consequences
* which occur through the use of the example code.
*
***********************************************************************
"! <p class="shorttext synchronized">ABAP cheat sheet: String processing</p>
"! Example to demonstrate string processing.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_string_proc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

protected section.
private section.
ENDCLASS.



CLASS ZCL_DEMO_ABAP_STRING_PROC IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA(output) = NEW zcl_demo_abap_display( out ).

    output->display( `Demo: String Processing` ).
    output->display( `1) Creating Strings and Assigning Values` ).

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
    char_a2 = 'ijklmnopq'.

    "Treating trailing blanks
    DATA(char_a3) = 'ab   '.

    DATA(str_a7)  = `cdefgh`.

    str_a7 = char_a3. "Trailing blanks are not respected.

    "Excursion: Chaining strings
    "Note the conversion result of str_a5 above (i to string)
    DATA(str_a8) = str_a4 && ` ` && str_a5 && `!`.

    output->display( input = str_a3 name = `str_a3` ).
    output->display( input = char_a2 name = `char_a2` ).
    output->display( input = str_a7 name = `str_a7` ).
    output->display( input = str_a8 name = `str_a8` ).

    output->next_section( `2) Chaining Strings` ).

    DATA(str_b1) = `Hallo`.
    DATA(str_b2) = `how`.
    DATA(str_b3) = `are`.

    "Chaining using && operator
    DATA(str_b4) = str_b1 && ` ` && sy-uname && `, ` && str_b2 && ` ` && str_b3 && ` you?`.

    "Chaining only character literals of the same type using & operator
    "Note: Such a combination of literals is possible up to 255 chars.
    DATA(char_b1) = 'AB' & 'AP  '. "Trailing blanks are ignored

    DATA(str_b5) = `AB` & `AP  `.

    output->display( input = str_b4 name = `str_b4` ).
    output->display( input = char_b1 name = `char_b1` ).

    output->next_section( `3) String Templates (1): Constructing Strings` ).

    "The expression must be convertible to a string. A blank (not
    "within the curly brackets) means a blank in the resulting string.
    DATA(str_c1) = `Hallo`. "String created with backquotes
    DATA(str_c2) = `how`.  "Strings created with pipes
    DATA(str_c3) = `are`.
    DATA(str_c4) = |{ str_c1 } { sy-uname }, | &&
                   |{ str_c2 } { str_c3 } you?|.

    "Interpretation of character combinations as control characters
    "\n interpreted as a line break
    DATA(str_c5) = |{ str_c1 }\n{ sy-uname },| &&
                   |\n{ str_c2 }\n{ str_c3 }\nyou?|.

    output->display( input = str_c4 name = `str_c4` ).
    output->display( input = str_c5 name = `str_c5` ).

    output->next_section( `4) String Templates (2): Formatting Options` ).
    "Time, date
    DATA(str_d1) =
    |Date: { cl_abap_context_info=>get_system_date( ) DATE = USER }\n| &&
    |Time: { cl_abap_context_info=>get_system_time( ) TIME = ISO }\n| &&
    |Timestamp: { utclong_current( ) TIMESTAMP = SPACE }|.

    "Upper, lower case
    DATA(str_d2) = |AbCdEfG|.
    DATA(str_d3) = |{ str_d2 CASE = LOWER }|.
    DATA(str_d4) = |{ str_d2 CASE = UPPER }|.

    "Width and alignment
    DATA(str_d5) = |{ 'Left'   WIDTH = 20 ALIGN = LEFT }<---|.
    DATA(str_d6) = |{ 'Center' WIDTH = 20 ALIGN = CENTER }<---|.
    DATA(str_d7) = |{ 'Right'  WIDTH = 20 ALIGN = RIGHT }<---|.
    DATA(str_d8) = |{ 'Left'   WIDTH = 20 ALIGN = LEFT  PAD = '.'  }<---|.
    DATA(str_d9) = |{ 'Center' WIDTH = 20 ALIGN = CENTER PAD = '.' }<---|.
    DATA(str_d10) = |{ 'Right' WIDTH = 20 ALIGN = RIGHT  PAD = '.' }<---|.

    "Numbers
    DATA(str_d11) = |{                   - 2 / 3   DECIMALS = 3 }, {
                        CONV decfloat34( - 2 / 3 ) DECIMALS = 3 }, {
                        CONV          f( - 2 / 3 ) DECIMALS = 3 }|.

    "Country-specific format
    "The example shows USA and Germany.
    "Note: This example is only possible in unrestricted language scope.
    "If you are in an environment allowing unrestricted language scope,
    "you can comment the following statements in. In that case, also
    "comment in the display methods for these data objects further down.
*    SET COUNTRY 'US' .
*    DATA(str_d12) = |{ 1000000 NUMBER = ENVIRONMENT }|.
*
*    SET COUNTRY 'DE' .
*    DATA(str_d13) = |{ 1000000 NUMBER = ENVIRONMENT }|.

    "Escaping \|{}  in string templates
    DATA(str_d14) = |\\ \| \{ \}|.

    output->display( input = str_d1 name = `str_d1` ).
    output->display( input = str_d3 name = `str_d3` ).
    output->display( input = str_d4 name = `str_d4` ).
    output->display( input = str_d5 name = `str_d5` ).
    output->display( input = str_d6 name = `str_d6` ).
    output->display( input = str_d7 name = `str_d7` ).
    output->display( input = str_d8 name = `str_d8` ).
    output->display( input = str_d9 name = `str_d9` ).
    output->display( input = str_d10 name = `str_d10` ).
    output->display( input = str_d11 name = `str_d11` ).
*    output->display( input = str_d12 name = `str_d12` ).
*    output->display( input = str_d13 name = `str_d13` ).
    output->display( input = str_d14 name = `str_d14` ).

    output->next_section( `5) Determining the Length of Strings` ).

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

    output->display( input = length_e1 name = `length_e1` ).
    output->display( input = length_e2 name = `length_e2` ).
    output->display( input = length_e3 name = `length_e3` ).
    output->display( input = length_e4 name = `length_e4` ).
    output->display( input = int_e1 name = `int_e1` ).
    output->display( input = int_e2 name = `int_e2` ).

    output->next_section( `6) Concatenating Strings` ).

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

    output->display( input = str_f3 name = `str_f3` ).
    output->display( input = str_f4 name = `str_f4` ).
    output->display( input = str_f5 name = `str_f5` ).
    output->display( input = str_f6 name = `str_f6` ).
    output->display( input = str_f7 name = `str_f7` ).
    output->display( input = str_f8 name = `str_f8` ).
    output->display( input = char_f4 name = `char_f4` ).
    output->display( input = char_f5 name = `char_f5` ).
    output->display( input = alphabet1 name = `alphabet1` ).
    output->display( input = alphabet2 name = `alphabet2` ).
    output->display( input = alphabet3 name = `alphabet3` ).
    output->display( input = alphabet4 name = `alphabet4` ).
    output->display( input = alphabet5 name = `alphabet5` ).

    output->next_section( `7) Splitting Strings` ).

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

    output->display( input = str_g2 name = `str_g2` ).
    output->display( input = str_g3 name = `str_g3` ).
    output->display( input = str_g4 name = `str_g4` ).
    output->display( input = str_g5 name = `str_g5` ).
    output->display( input = str_g6 name = `str_g6` ).
    output->display( input = itab_g1 name = `itab_g1` ).
    output->display( input = str_g7 name = `str_g7` ).
    output->display( input = str_g8 name = `str_g8` ).
    output->display( input = itab_g2 name = `itab_g2` ).
    output->display( input = seg_nom name = `seg_nom` ).

    output->next_section( `Modifying Strings` ).
    output->display( `8) Transforming to Lower and Upper Case` ).

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

    output->display( input = str_h3 name = `str_h3` ).
    output->display( input = str_h4 name = `str_h4` ).
    output->display( input = str_h1 name = `str_h1` ).
    output->display( input = str_h2 name = `str_h2` ).
    output->display( input = str_h6 name = `str_h6` ).
    output->display( input = str_h7 name = `str_h7` ).
    output->display( input = str_h8 name = `str_h8` ).
    output->display( input = str_h9 name = `str_h9` ).

    output->next_section( `9) Shifting Content in Strings` ).

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

    output->display( `SHIFT statements:` ).
    output->display( input = str_i2 name = `str_i2` ).
    output->display( input = str_i3 name = `str_i3` ).
    output->display( input = str_i4 name = `str_i4` ).
    output->display( input = char_i1 name = `char_i1` ).
    output->display( input = str_i5 name = `str_i5` ).
    output->display( input = char_i2 name = `char_i2` ).
    output->display( input = char_i3 name = `char_i3` ).
    output->display( input = str_i6 name = `str_i6` ).
    output->display( input = str_i7 name = `str_i7` ).

    output->display( `String functions:` ).
    output->display( input = str_i9 name = `str_i9` ).
    output->display( input = str_i10 name = `str_i10` ).
    output->display( input = str_i11 name = `str_i11` ).
    output->display( input = str_i13 name = `str_i13` ).
    output->display( input = str_i14 name = `str_i14` ).
    output->display( input = str_i15 name = `str_i15` ).

    output->next_section( `10) Condensing Strings` ).

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

    output->display( `CONDENSE statements:` ).
    output->display( input = char_j1 name = `char_j1` ).
    output->display( input = char_j2 name = `char_j2` ).
    output->display( input = char_j3 name = `char_j3` ).
    output->display( input = str_j name = `str_j` ).
    output->next_section( `String function condense:` ).
    output->display( input = str_j2 name = `str_j2` ).
    output->display( input = str_j3 name = `str_j3` ).
    output->display( input = str_j4 name = `str_j4` ).
    output->display( input = str_j6 name = `str_j6` ).
    output->display( input = str_j7 name = `str_j7` ).

    output->next_section( `11) Reversing Strings` ).

    DATA(str_k) = reverse( `ollah` ).

    output->display( input = str_k name = `str_k` ).

    output->next_section( `12) Inserting Substrings into Strings` ).

    DATA(str_l1) = `abcghi`.

    "Inserting into specific position
    DATA(str_l2) = insert( val = str_l1 sub = `def` off = 3 ).

    "off is optional. If not specified (default value off = 0)
    "the result is like concatenating a string with &&
    DATA(str_l3) = insert( val = str_l1 sub = `def` ).

    DATA(str_l4) = `def` && str_l1.

    output->display( input = str_l2 name = `str_l2` ).
    output->display( input = str_l3 name = `str_l3` ).
    output->display( input = str_l4 name = `str_l4` ).

    output->next_section( `13) Processing Substrings` ).

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

    output->display( input = str_m2 name = `str_m2` ).
    output->display( input = str_m3 name = `str_m3` ).
    output->display( input = str_m4 name = `str_m4` ).
    output->display( input = str_m5 name = `str_m5` ).
    output->display( input = str_m6 name = `str_m6` ).
    output->display( input = str_m7 name = `str_m7` ).
    output->display( input = char_m2 name = `char_m2` ).
    output->display( input = char_m3 name = `char_m3` ).
    output->display( input = str_m9 name = `str_m9` ).
    output->display( input = str_m10 name = `str_m10` ).
    output->display( input = str_m11 name = `str_m11` ).
    output->display( input = str_m12 name = `str_m12` ).
    output->display( input = str_m13 name = `str_m13` ).

    output->next_section( `Searching and Replacing in Strings` ).
    output->display( `14) Searching Specific Characters in Strings ` &&
                  `Using Comparison Operators` ).

    DATA(str_n1) = `cheers`.

    "CA (contains any)
    "sy-fdpos contains the offset of the first found character.
    "If nothing is found, sy-fdpos contains the length of the string.
    "Note that position 0 stands for the very first position.
    IF str_n1 CA `aeiou`.
      output->display( |CA: str_n1 contains any of the characters. | &&
       |The position of the first found character is { sy-fdpos }.| ).
    ELSE.
      output->display( |CA: str_n1 does not contain any of the characters. | &&
             |The length of str_n1 is { sy-fdpos }.| ).
    ENDIF.

    "NA (contains not any)
    IF str_n1 NA `xyz`.
      output->display( |NA: str_n1 does not contain any of the characters.| &&
             |The length of str_n1 is { sy-fdpos }.|
             ).
    ELSE.
      output->display( |NA: str_n1 contains any of the characters. | &&
       |The position of the first found character is { sy-fdpos }.| ).
    ENDIF.

    "String functions to determine the offset of any character ...
    "Note: If nothing is found, the value -1 is returned.
    "There are more parameters possible.
    "... contained in a substring
    DATA(off_n1) = find_any_of( val = str_n1  sub = `aeiou` ).
    output->display( input = off_n1 name = `off_n1` ).

    "... not contained in a substring
    DATA(off_n2) = find_any_not_of( val = str_n1  sub = `xyz` ).
    output->display( input = off_n2 name = `off_n2` ).

    "String functions to determine the total number of occurrences
    DATA(occ_n1) = count_any_of( val = str_n1  sub = `e` ).
    output->display( input = occ_n1 name = `occ_n1` ).

    DATA(occ_n2) = count_any_not_of( val = str_n1  sub = `s` ).
    output->display( input = occ_n2 name = `occ_n2` ).

    "Determining if a string is exclusively composed of a certain
    "character set
    IF str_n1 CO `rs`.
      output->display( |CO: str_n1 contains only the characters. |
      && |The length of str_n1 is { sy-fdpos }.| ).
    ELSE.
      output->display( |CO: str_n1 does not contain only the characters. |
      && |Offset of the first character in str_n1 that is not |
      && |contained in the second operand: { sy-fdpos }.| ).
    ENDIF.

    "Negation of CO
    IF str_n1 CN `chers`.
      output->display( |CN: str_n1 does not contain only the characters. |
       && |Offset of the first character in str_n1 that is |
       && |not contained in the second operand: { sy-fdpos }.|
        ).
    ELSE.
      output->display( |CN: str_n1 contains only the characters. |
      && |The length of str_n1 is { sy-fdpos }.| ).
    ENDIF.

    output->next_section( `15) Replacing Specific Characters in Strings` ).

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

    output->display( input = str_o2 name = `str_o2` ).
    output->display( input = str_o3 name = `str_o3` ).
    output->display( input = str_o1 name = `str_o1` ).

    output->next_section( `16) Searching for Substrings in Strings` ).
    DATA(str_p1) = `cheers`.

    "CS (contains string)
    "sy-fdpos contains the offset of the found substring.
    "If the substring is not found, sy-fdpos contains the length of the
    "searched string.
    IF str_p1 CS `rs`.
      output->display( |CS: str_p contains the substring. |
      && |The offset is { sy-fdpos }.| ).
    ELSE.
      output->display( |CS: str_p does not contain the substring. |
      && |The length of str_p is { sy-fdpos }.| ).
    ENDIF.

    "NS (contains no string)
    IF str_p1 NS `abc`.
      output->display( |NS: str_p does not contain the substring. |
      && |The length of str_p is { sy-fdpos }.| ).
    ELSE.
      output->display( |NS: str_p contains the substring. |
       && |The offset is { sy-fdpos }.| ).
    ENDIF.

    DATA(str_p2) = `Pieces of cakes.`.

    "Specifying case-sensitivity
    DATA(off_p1) = find( val = str_p2  sub = `OF` case = abap_false ).

    "No finding, result: -1
    DATA(off_p2) = find( val = str_p2  sub = `hallo` ).

    "Specifying the offset and length
    DATA(off_p3) = find( val = str_p2  sub = `ce` off = 1 len = 7 ).

    "Parameter occ: A positive value means the nth position from the
    "left, a negative value the nth position from the right.
    DATA(off_p4) = find( val = str_p2  sub = `es` occ = -1 ).

    "Determining how often a substring occurs
    DATA(cnt_p1) = count( val = str_p2  sub = `es` ).

    "FIND statements
    DATA(str_p3) = `abc def ghi abc`.

    "sy-subrc is set on which you can react.
    FIND `def` IN str_p3.

    IF sy-subrc = 0.
      DATA(str_p4) = `"def" was found`.
    ELSE.
      str_p4 = `"def" was not found`.
    ENDIF.

    "Addition SUBSTRING is optional
    FIND SUBSTRING `abc` IN str_p3.

    IF sy-subrc = 0.
      DATA(str_p5) = `"abc" was found`.
    ELSE.
      str_p5 = `"abc" was not found`.
    ENDIF.

    "Case-insensitive search
    FIND `aBC` IN str_p3 IGNORING CASE.

    IF sy-subrc = 0.
      DATA(str_p6) = `"aBC" was found ignoring the case`.
    ELSE.
      str_p6 = `"aBC" was not found ignoring the case`.
    ENDIF.

    "MATCH additions can be specified individually or combined
    FIND ALL OCCURRENCES OF `abc` IN str_p3
      MATCH COUNT DATA(cnt_p2)   "number of findings
      MATCH OFFSET DATA(off_p5)  "offset of last finding
      MATCH LENGTH DATA(len_p1). "length of last finding

    "Finding the first occurrence and returning the offset
    FIND FIRST OCCURRENCE OF `abc` IN str_p3 MATCH OFFSET DATA(off_p6).

    "Returning all of these pieces of information in a table for all
    "findings
    FIND ALL OCCURRENCES OF `abc` IN str_p3 RESULTS DATA(res_p1).

    "Restricting the search area (OFFSET/LENGTH can be specified
    "individually)
    FIND `abc` IN SECTION OFFSET 4 LENGTH 11 OF str_p3
      MATCH OFFSET DATA(off_p7). "12

    "Searching in internal tables; search results are returned in an
    "internal table
    DATA(str_table) = VALUE string_table( ( `ZxZ` ) ( `yZ` ) ( `Zz` ) ).

    FIND ALL OCCURRENCES OF `Z` IN TABLE str_table RESULTS
      DATA(res_p2) RESPECTING CASE.

    output->display( input = off_p1 name = `off_p1` ).
    output->display( input = off_p2 name = `off_p2` ).
    output->display( input = off_p3 name = `off_p3` ).
    output->display( input = off_p4 name = `off_p4` ).
    output->display( input = cnt_p1 name = `cnt_p1` ).
    output->display( input = str_p4 name = `str_p4` ).
    output->display( input = str_p5 name = `str_p5` ).
    output->display( input = str_p6 name = `str_p6` ).
    output->display( input = cnt_p2 name = `cnt_p2` ).
    output->display( input = off_p5 name = `off_p5` ).
    output->display( input = len_p1 name = `len_p1` ).
    output->display( input = off_p6 name = `off_p6` ).
    output->display( input = res_p1 name = `res_p1` ).
    output->display( input = off_p7 name = `off_p7` ).
    output->display( input = res_p2 name = `res_p2` ).

    output->next_section( `17) Replacing Substrings in Strings` ).

    DATA(str_q1) = `abc def ghi abc`.

    "replace function
    "occ: Specifies the number of occurrences of the substring. The
    "default value is 1, i. e. the first occurrence starting from the
    "left. Setting occ to 0 means that all occurrences are respected.
    DATA(str_q2) = replace( val  = str_q1
                            sub  = `abc`
                            with = `###` ).

    "Specifying case (case sensitivity) and occ
    DATA(str_q3) = replace( val  = str_q1
                            sub  = `ABC`
                            with = `###`
                            case = abap_false
                            occ  = 2 ).

    "All occurrences are respected
    DATA(str_q4) = replace( val  = str_q1
                            sub  = `abc`
                            with = `###`
                            occ  = 0 ).

    DATA(str_q5) = str_q1.
    DATA(str_q6) = str_q1.
    DATA(str_q7) = str_q1.
    DATA(str_q8) = str_q1.
    DATA(str_q9) = str_q1.
    DATA(str_q10) = str_q1.
    DATA(str_q11) = str_q1.
    DATA(str_q12) = str_q1.
    DATA(str_q13) = str_q1.

    "REPLACE statements with selected additions
    REPLACE `def` IN str_q5 WITH `###`. "abc ### ghi abc

    "### def ghi abc (explicitly using FIRST OCCURRENCE; first found
    "is replaced)
    REPLACE FIRST OCCURRENCE OF `abc` IN str_q6 WITH `###`.

    "### def ghi abc (first found is replaced)
    REPLACE `abc` IN str_q7 WITH `###`.

    "### def ghi abc (SUBSTRING is optional)
    REPLACE SUBSTRING `abc` IN str_q8 WITH `###`.

    "### def ghi ### (all occurrences are respected)
    REPLACE ALL OCCURRENCES OF `abc` IN str_q9 WITH `###`.

    REPLACE `aBC` IN str_q10 WITH `###` IGNORING CASE. "### def ghi abc

    "REPLACEMENT additions; can be specified individually or combined
    "### def ghi ###
    REPLACE ALL OCCURRENCES OF `abc` IN  str_q11 WITH `###`
      REPLACEMENT COUNT  DATA(cnt_q1)  "number of replacements
      REPLACEMENT OFFSET DATA(off_q1)  "offset of last replacement
      REPLACEMENT LENGTH DATA(len_q1). "length of last substr. inserted

    "Returning all of these pieces of information in a table for all
    " replacements
    REPLACE ALL OCCURRENCES OF `abc` IN  str_q12 WITH `###`
          RESULTS DATA(res_q1). "### def ghi ###

    "Position-based replacement (OFFSET/LENGTH can be specified
    "individually)
    "abc ### abc
    REPLACE SECTION OFFSET 4 LENGTH 7 OF str_q13 WITH `###`.

    "Replacements in internal tables
    DATA(str_tab) = VALUE string_table( ( `ZxZ` ) ( `yZ` ) ( `Zz` ) ).

    REPLACE ALL OCCURRENCES OF `Z`
      IN TABLE str_tab WITH ``
      RESPECTING CASE. "x / y / z

    output->display( input = str_q2 name = `str_q2` ).
    output->display( input = str_q3 name = `str_q3` ).
    output->display( input = str_q4 name = `str_q4` ).
    output->display( input = str_q5 name = `str_q5` ).
    output->display( input = str_q6 name = `str_q6` ).
    output->display( input = str_q7 name = `str_q7` ).
    output->display( input = str_q8 name = `str_q8` ).
    output->display( input = str_q9 name = `str_q9` ).
    output->display( input = str_q10 name = `str_q10` ).
    output->display( input = str_q11 name = `str_q11` ).
    output->display( input = cnt_q1 name = `cnt_q1` ).
    output->display( input = off_q1 name = `off_q1` ).
    output->display( input = len_q1 name = `len_q1` ).
    output->display( input = str_q12 name = `str_q12` ).
    output->display( input = res_q1 name = `res_q1` ).
    output->display( input = str_q13 name = `str_q13` ).
    output->display( input = str_tab name = `str_tab` ).

    output->next_section( `Pattern-Based Searching and Replacing in Strings` ).
    output->display( `18) Simple Pattern-Based Searching ` &&
      `Using Logical Operators` ).

    DATA(str_r1) = `abc_def_ghi`.

    "CP (conforms to pattern)
    "*: Any character sequence (including blanks).
    "+: Any character (only one character, including blanks).
    "#: Escaping symbol. The following character is marked for an exact
    "comparison.
    "Note: Patterns are not case sensitive except for characters marked
    "by #. If a pattern is found, the system variable sy-fdpos returns
    "the offset of the first finding. Otherwise, it contains the length
    "of the searched string.
    IF str_r1 CP `*f#_*`.
      output->display( |CP: str_r1 covers the pattern. |
      && |The offset is { sy-fdpos }.| ).
    ELSE.
      output->display( |CP: str_r1 does not cover the pattern. |
      && |The length of str_r1 is { sy-fdpos }.| ).
    ENDIF.

    "NP (does not conform to pattern)
    IF str_r1 NP `i+`.
      output->display( |NP: str_r1 does not cover the pattern. |
      && |The length of str_r1 is { sy-fdpos }.| ).
    ELSE.
      output->display( |NP: str_r1 covers the pattern. |
      && |The offset is { sy-fdpos }.| ).
    ENDIF.

    output->next_section( `19) Complex Searching Using ` &&
    `Regular Expressions` ).

    DATA(str_s1) = `Cathy's black cat was fast asleep on the mat. ` &&
                    `Later that day, the cat played with Matt.`.

    "Determining the position of the first finding
    "here, parameter occ is 1 by default
    DATA(off_s1) = find( val = str_s1  pcre = `at.` ).

    "Determining the number of findings
    "Considers all 'a' characters not followed by 't',
    "all 'at' plus 'att'
    DATA(cnt_s1) = count( val = str_s1  pcre = `at*` ).
    "Considers all 'at' plus 'att' and so on
    DATA(cnt_s2) = count( val = str_s1  pcre = `at+` ).

    "String function match
    "Extracting a substring matching a given pattern
    DATA(str_w_email) = `The email address is jon.doe@email.com.`.
    DATA(str_s2) = match( val   = str_w_email
                          pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ).

    "Predicate function matches
    "Checking a string if it matches a given pattern
    DATA(email) = `jon.doe@email.com`.

    IF matches( val   = email
                pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ).
      email = |{ email } is a valid email address.|.
    ELSE.
      email = |{ email } is not a valid email address.|.
    ENDIF.

    output->display( input = off_s1 name = `off_s1` ).
    output->display( input = cnt_s1 name = `cnt_s1` ).
    output->display( input = cnt_s2 name = `cnt_s2` ).
    output->display( input = str_s2 name = `str_s2` ).
    output->display( input = email name = `email` ).

    output->next_section( `20) Replacing Using Regular Expressions` ).

    DATA(str_t) = `Cathy's black cat was fast asleep on the mat. ` &&
                   `Later that day, the cat played with Matt.`.

    "Considers all 'a' characters not followed by 't',
    "all 'at' plus 'att'
    "occ = 0 -> replaces all occurrences
    DATA(str_t1) = replace( val  = str_t
                            pcre = `at*`
                            with = `#`
                            occ  = 0 ).

    "Considers all 'at' plus 'att'
    DATA(str_t2) = replace( val  = str_t
                            pcre = `at+`
                            with = `#`
                            occ  = 0 ).

    "Replaces 2 'e' characters in a row
    DATA(str_t3) = replace( val  = str_t
                            pcre = `e{2}`
                            with = `#`
                            occ = 0 ).

    "Replaces 'ay'. Preceding d is optional ('day' is replaced too)
    DATA(str_t4) = replace( val  = str_t
                            pcre = `d?ay`
                            with = `#`
                            occ  = 0 ).

    "Subgroup specified, replacement happens if 'at' is followed
    "by 'h' or 't'
    DATA(str_t5) = replace( val  = str_t
                            pcre = `at(h|t)`
                            with = `#`
                            occ  = 0 ).

    "Replaces 'at' when followed by any whitespace character
    DATA(str_t6) = replace( val  = str_t
                            pcre = `at\s`
                            with = `#`
                            occ = 0 ).

    "Replacement starts at beginning of string that is followed by 'c'
    "Marked as not case sensitive
    "Instead of ^, you could also use \A
    DATA(str_t7) = replace( val  = str_t
                            pcre = `^c`
                            with = `#`
                            case = abap_false ).

    "Replacement starts at end of string
    "Instead of $, you could also use \Z
    DATA(str_t8) = replace( val  = str_t
                            pcre = `$`
                            with = ` Awesome!` ).

    "Replaces words starting with 'ma', ending with another character
    DATA(str_t9) = replace( val  = str_t
                            pcre = `\bma.\b`
                            with = `#`
                            case = abap_false
                            occ  = 0 ).

    "Replaces the beginning of words with 'ma' followed by another
    "character.
    "Marked as not case sensitive, hence 'Mat' is considered, too.
    DATA(str_t10) = replace( val  = str_t
                             pcre = `\bma.`
                             with = `#`
                             case = abap_false
                             occ  = 0 ).

    "Replaces a specified set of literals; if 'case = abap_false'
    "is not specified, case sensitivity is respected
    DATA(str_t11) = replace( val = str_t
                             pcre = `[eC'.,]`
                             with = `#`
                             occ = 0 ).

    "Replaces a specified value range
    DATA(str_t12) = replace( val  = str_t
                             pcre = `[a-eA-C0-9]`
                             with = `#`
                             occ  = 0 ).

    "Replaces a specified value range. The example is the negation
    "of the previous example.
    DATA(str_t13) = replace( val  = str_t
                             pcre = `[^a-eA-C0-9]`
                             with = `#`
                             occ = 0 ).

    DATA(str_t14) = `<p><i>Date:</i> 12/16/2022</p>` &&
                     `<br><p>Time: 10:30</p>`.

    "Replacements considering subgroups
    "Example switches the date format from US to European
    "Sequences of digits are specified as subgroups followed by /
    DATA(str_t15) = replace( val  = str_t14
                             pcre = `(\d+)/(\d+)/`
                             with = `$2.$1.` ).

    "Regex pitfall: Watch greediness when using PCRE expressions
    "Example: Replacing all HTML tags in a string
    DATA(str_t16) = replace( val  = str_t14
                             pcre = `<.*>`
                             with = ``
                             occ  = 0 ). "Whole string replaced

    "The following pattern considers '<' not followed by '>' which is
    "specified in a negated definition for a single character in the
    "brackets. Then '*' greedily, matches anything until the next '>'.
    DATA(str_t17) = replace( val  = str_t14
                             pcre = `<[^>]*>`
                             with = ``
                             occ = 0 ).

    "Positive lookahead: Replaces colons followed by digits
    DATA(str_t18) = replace( val  = str_t14
                             pcre = `:(?=\d+)`
                             with = `.`
                             occ  = 0 ).

    "Negative lookahead: Removes colons not followed by digits
    ":(?!(\d+))
    DATA(str_t19) = replace( val  = str_t14
                             pcre = `:(?!\d+)`
                             with = ``
                             occ  = 0 ).

    "Positive Lookbehind: Replaces a digit preceded by a blank
    DATA(str_t20) = replace( val  = str_t14
                             pcre = `(?<=\s)\d`
                             with = `0`
                             occ  = 0 ).

    "Negative lookbehind: Determines the position before closing p tags
    "not preceded by 4 digits
    DATA(str_t21) = replace( val  = str_t14
                             pcre = `(?<!\d{4})(<\/p>)`
                             with = `:00$1`
                             occ  = 0 ).

    DATA(str_t22) = `ab apppc app`.

    "Greedy search
    "The pattern matches anything before 'p'. The matching is carried
    "out as often as possible. Hence, in this example the match
    "stretches until the end of the string since 'p' is the final
    "character, i. e. this 'p' and anything before is replaced).
    DATA(str_t23) = replace( val  = str_t22
                             pcre = `(.*)p`
                             with = `#` ).

    "Non-greedy search (denoted by '?' below)
    "The pattern matches anything before 'p'. The matching proceeds
    "until the first 'p' is found and does not go beyond (matching as
    "few as possible). Hence, the first found 'p' including the content
    "before is replaced.
    DATA(str_t24) = replace( val  = str_t22
                             pcre = `(.*?)p`
                             with = `#` ).

    "Replacements with subgroups
    "Replaces 'PP' (case-insensitive) with '#', the content before and
    "after 'PP' is switched
    DATA(str_t25) = replace( val  = str_t22
                             pcre = `(.*?)PP(.*)`
                             with = `$2#$1`
                             case = abap_false ).

    "REPLACE statement: Changing the source field directly
    REPLACE PCRE `(.*?)PP(.*)` IN str_t22 WITH `$2#$1` IGNORING CASE.

    "Replacements in internal tables
    DATA(itab_t) = VALUE
      string_table( ( `Cathy's black cat was fast asleep on the mat.` )
                    ( `Later that day, the cat played with Matt.` )
                    ( `How about that?` ) ).

    "Replaces all 'th' occurrences in words beginning with 'th'
    REPLACE ALL OCCURRENCES OF PCRE `\bth` IN TABLE itab_t WITH `#`
     IGNORING CASE REPLACEMENT COUNT DATA(count_t).

    output->display( input = |Original str_t: { str_t }| ).
    output->display( input = str_t1 name = `str_t1` ).
    output->display( input = str_t2 name = `str_t2` ).
    output->display( input = str_t3 name = `str_t3` ).
    output->display( input = str_t4 name = `str_t4` ).
    output->display( input = str_t5 name = `str_t5` ).
    output->display( input = str_t6 name = `str_t6` ).
    output->display( input = str_t7 name = `str_t7` ).
    output->display( input = str_t8 name = `str_t8` ).
    output->display( input = str_t9 name = `str_t9` ).
    output->display( input = str_t10 name = `str_t10` ).
    output->display( input = str_t11 name = `str_t11` ).
    output->display( input = str_t12 name = `str_t12` ).
    output->display( input = str_t13 name = `str_t13` ).
    output->display( input = |Original str_t14: { str_t14 }| ).
    output->display( input = str_t15 name = `str_t15` ).
    output->display( input = str_t16 name = `str_t16` ).
    output->display( input = str_t17 name = `str_t17` ).
    output->display( input = str_t18 name = `str_t18` ).
    output->display( input = str_t19 name = `str_t19` ).
    output->display( input = str_t20 name = `str_t20` ).
    output->display( input = str_t21 name = `str_t21` ).
    output->display( input = str_t23 name = `str_t23` ).
    output->display( input = str_t24 name = `str_t24` ).
    output->display( input = str_t25 name = `str_t25` ).
    output->display( input = str_t22 name = `str_t22` ).
    output->display( input = itab_t name = `itab_t` ).
    output->display( input = |Number of replacements in itab (count_t): { count_t }| ).

  ENDMETHOD.
ENDCLASS.
