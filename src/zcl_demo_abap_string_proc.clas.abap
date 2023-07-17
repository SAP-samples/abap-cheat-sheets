***********************************************************************
*
*              ABAP cheat sheet: String processing
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntactical options for processing
*   character strings.
* - Topics covered: Creating strings and assigning values, chaining strings,
*   string templates, concatenating/splitting/modifying strings, searching
*   and replacing, regular expressions
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
"! <p class="shorttext synchronized">ABAP cheat sheet: String processing</p>
"! Example to demonstrate string processing.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_string_proc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_string_proc IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA(output) = NEW zcl_demo_abap_display( out ).

    output->display( `ABAP Cheat Sheet Example: String Processing` ).
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

**********************************************************************

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

**********************************************************************

    output->next_section( `3a) String Templates (1): Constructing Strings` ).

    "The expression must be convertible to a string. A blank (not
    "within the curly brackets) means a blank in the resulting string.
    DATA(str_c1) = `Hallo`. "String created with backquotes
    DATA(str_c2) = `how`.  "Strings created with pipes
    DATA(str_c3) = `are`.
    DATA(str_c4) = |{ str_c1 } { sy-uname }, | &&
                   |{ str_c2 } { str_c3 } you?|.

**********************************************************************

    output->next_section( `3b) String Templates (2): Control Characters` ).

    "Interpretation of character combinations as control characters
    "\n interpreted as a line break
    DATA(str_c5) = |{ str_c1 }\n{ sy-uname },| &&
                   |\n{ str_c2 }\n{ str_c3 }\nyou?|.

    output->display( input = str_c4 name = `str_c4` ).
    output->display( input = str_c5 name = `str_c5` ).

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

    output->display( input = str_c6 name = `str_c6` ).
    output->display( input = str_c7 name = `str_c7` ).
    output->display( input = str_c8 name = `str_c8` ).
    output->display( input = str_c9 name = `str_c9` ).    

**********************************************************************

    output->next_section( `4) String Templates (3): Formatting Options` ).
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
    output->display( input = str_d14 name = `str_d14` ).

**********************************************************************

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

**********************************************************************

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

**********************************************************************

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

**********************************************************************

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

**********************************************************************

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

**********************************************************************

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

**********************************************************************

    output->next_section( `11) Reversing Strings` ).

    DATA(str_k) = reverse( `ollah` ).

    output->display( input = str_k name = `str_k` ).

**********************************************************************

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

**********************************************************************

    output->next_section( `13) Overlaying Content` ).

    DATA(incl) = '==============================CP'.
    DATA(cl_name) = 'CL_SOME_CLASS                   '.

    "Addition ONLY is not specified: All blanks are replaced
    OVERLAY cl_name WITH incl.

    DATA(t1) = 'a.b.c.a.b.c.A'.
    DATA(t2) = 'z.x.y.Z.x.y.z'.

    "Addition ONLY is specified: All characters that are specified after ONLY and that
    "occur in the operand are replaced. Note that this is case-sensitive.
    OVERLAY t1 WITH t2 ONLY 'ab'.

    output->display( input = cl_name name = `cl_name` ).
    output->display( input = t1 name = `t1` ).

**********************************************************************

    output->next_section( `14) Processing Substrings` ).

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

**********************************************************************

    output->next_section( `Searching and Replacing in Strings` ).
    output->display( `15) Searching Specific Characters in Strings ` &&
                     `Using Comparison Operators and String Functions` ).

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

    output->display( input = res_n3 name = `res_n3` ).
    output->display( input = res_n4 name = `res_n4` ).
    output->display( input = res_n5 name = `res_n5` ).
    output->display( input = res_n6 name = `res_n6` ).
    output->display( input = res_n7 name = `res_n7` ).
    output->display( input = res_n8 name = `res_n8` ).
    output->display( input = res_n9 name = `res_n9` ).
    output->display( input = res_n10 name = `res_n10` ).
    output->display( input = res_n11 name = `res_n11` ).
    output->display( input = res_n12 name = `res_n12` ).
    output->display( input = res_n13 name = `res_n13` ).

**********************************************************************

    output->next_section( `16) Replacing Specific Characters in Strings` ).

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

**********************************************************************

    output->next_section( `Searching for Substrings in Strings` ).
    output->display( `17) Substring Search: Simple Search Using Comparison Operators` ).

    DATA(str_p1) = `cheers`.

    "CS (contains string)
    "sy-fdpos contains the offset of the found substring.
    "If the substring is not found, sy-fdpos contains the length of the
    "searched string.
    IF str_p1 CS `rs`.
      output->display( |CS: The string contains the substring. |
      && |The offset is { sy-fdpos }.| ).
    ELSE.
      output->display( |CS: The string does not contain the substring. |
      && |The length of the string is { sy-fdpos }.| ).
    ENDIF.

    "NS (contains no string)
    IF str_p1 NS `abc`.
      output->display( |NS: The string does not contain the substring. |
      && |The length of the string is { sy-fdpos }.| ).
    ELSE.
      output->display( |NS: The string contains the substring. |
       && |The offset is { sy-fdpos }.| ).
    ENDIF.

**********************************************************************

    output->next_section( `18) Substring Search in Strings ` &&
    `Using FIND Statements` ).
    "The code examples demonstrate different additions.

    DATA(str_qa) = `She sells seashells by the seashore.`.

    "Determining if a substring is found
    "Simple find statement
    FIND `se` IN str_qa.

    IF sy-subrc = 0.
      output->display( `'se' found in the string` ).
    ELSE.
      output->display( `'se' not found in the string` ).
    ENDIF.

    "Addition SUBSTRING is optional
    FIND SUBSTRING `hi` IN str_qa.

    IF sy-subrc = 0.
      output->display( `'hi' Found in the string` ).
    ELSE.
      output->display( `'hi' not found in the string` ).
    ENDIF.

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

    output->display( input = cnt_q1 name = `cnt_q1` ).
    output->display( input = off_q2 name = `off_q2` ).
    output->display( input = cnt_q3 name = `cnt_q3` ).
    output->display( input = off_q4 name = `off_q4` ).
    output->display( input = cnt_q5 name = `cnt_q5` ).
    output->display( input = off_q6 name = `off_q6` ).
    output->display( input = cnt_q7 name = `cnt_q7` ).
    output->display( input = off_q8 name = `off_q8` ).
    output->display( input = cnt_q9 name = `cnt_q9` ).
    output->display( input = off_q10 name = `off_q10` ).
    output->display( input = cnt_q11 name = `cnt_q11` ).
    output->display( input = off_q12 name = `off_q12` ).
    output->display( input = off_q13 name = `off_q13` ).
    output->display( input = len_q14 name = `len_q14` ).
    output->display( input = res_q15 name = `res_q15` ).
    output->display( input = res_q16 name = `res_q16` ).

**********************************************************************

    output->next_section( `19) Substring Search in Internal Tables ` &&
    `Using FIND ... IN TABLE Statements` ).

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

    output->display( input = res_r1 name = `res_r1` ).
    output->display( input = res_r2 name = `res_r2` ).
    output->display( input = line_r3 name = `line_r3` ).
    output->display( input = off_r4 name = `off_r4` ).
    output->display( input = len_r5 name = `len_r5` ).

**********************************************************************

    output->next_section( `20) Substring Search in Strings ` &&
    `Using the String Function find` ).

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

    output->display( input = res_s1 name = `res_s1` ).
    output->display( input = res_s2 name = `res_s2` ).
    output->display( input = res_s3 name = `res_s3` ).
    output->display( input = res_s4 name = `res_s4` ).
    output->display( input = res_s5 name = `res_s5` ).
    output->display( input = res_s6 name = `res_s6` ).
    output->display( input = res_s7 name = `res_s7` ).
    output->display( input = res_s8 name = `res_s8` ).
    output->display( input = res_s9 name = `res_s9` ).
    output->display( input = res_s10 name = `res_s10` ).
    output->display( input = res_s11 name = `res_s11` ).
    output->display( input = res_s12 name = `res_s12` ).

    "Demonstrating a false range to be searched
    TRY.
        DATA(res_s13) = find( val = str_s sub = `e` off = 5 len = 15 ).
      CATCH cx_sy_range_out_of_bounds.
        output->display( `The exception cx_sy_range_out_of_bounds was raised.` ).
    ENDTRY.

***********************************************************************

    output->next_section( `21) Replacing Substrings in Strings Using REPLACE Statments` ).

    DATA(str_t) = `abap ABAP abap`.
    DATA(str_t1) = str_t.

    "Simple REPLACE statement
    "Omitting the FIRST OCCURRENCE and ALL OCCURRENCES OF additions means
    "replacing the first occurrence by default.
    REPLACE `ab` IN str_t1 WITH `##`.

    output->display( input = str_t1 name = `str_t1` ).
    DATA(str_t2) = str_t.

    "Addition SUBSTRING is optional; same effect as the statement above
    REPLACE SUBSTRING `ab` IN str_t2 WITH `##`.

    output->display( input = str_t2 name = `str_t2` ).
    DATA(str_t3) = str_t.

    "Addition FIRST OCCURRENCE OF: Explicit specification to replace the
    "first occurrence; same effect as the statements above
    REPLACE FIRST OCCURRENCE OF `ab` IN str_t3 WITH `##`.

    output->display( input = str_t3 name = `str_t3` ).
    DATA(str_t4) = str_t.

    "Addition ALL OCCURRENCES OF: All occurrences are replaced
    "Note that the replacement is case-sensitive by default.
    REPLACE ALL OCCURRENCES OF `ab` IN str_t4 WITH `##`.

    output->display( input = str_t4 name = `str_t4` ).
    DATA(str_t5) = str_t.

    "Further additional options for advanced evaluation options

    "IGNORING CASE addition: Making replacements case-insensitive
    REPLACE ALL OCCURRENCES OF `ab`
      IN str_t5 WITH `##`
      IGNORING CASE.

    output->display( input = str_t5 name = `str_t5` ).
    DATA(str_t6) = str_t.

    "REPLACEMENT COUNT addition
    REPLACE ALL OCCURRENCES OF `ab`
      IN str_t6 WITH `##`
      REPLACEMENT COUNT DATA(cnt_t7)
      IGNORING CASE.

    output->display( input = str_t6 name = `str_t6` ).
    output->display( input = cnt_t7 name = `cnt_t7` ).
    DATA(str_t8) = str_t.

    "REPLACEMENT OFFSET and LENGTH additions
    REPLACE FIRST OCCURRENCE OF `ap`
      IN str_t8 WITH `##`
      REPLACEMENT COUNT DATA(cnt_t9) "always 1 for replaced first occurrence
      REPLACEMENT OFFSET DATA(off_t10)
      REPLACEMENT LENGTH DATA(len_t11)
      IGNORING CASE.

    output->display( input = str_t8 name = `str_t8` ).
    output->display( input = cnt_t9 name = `cnt_t9` ).
    output->display( input = off_t10 name = `off_t10` ).
    output->display( input = len_t11 name = `len_t11` ).
    DATA(str_t12) = str_t.

    "SECTION ... OF addition: Replacing within a specified area
    REPLACE ALL OCCURRENCES OF `ap`
      IN SECTION OFFSET 4 LENGTH 5
      OF str_t12 WITH `##`
      REPLACEMENT COUNT DATA(cnt_t13)
      REPLACEMENT OFFSET DATA(off_t14)
      REPLACEMENT LENGTH DATA(len_t15)
      IGNORING CASE.

    output->display( input = str_t12 name = `str_t12` ).
    output->display( input = cnt_t13 name = `cnt_t13` ).
    output->display( input = off_t14 name = `off_t14` ).
    output->display( input = len_t15 name = `len_t15` ).
    DATA(str_t16) = str_t.

    "RESULTS additions with ...
    "... ALL OCCURRENCES OF
    "Note: The data object, which is declared inline here, is of type repl_result_tab.
    REPLACE ALL OCCURRENCES OF `ap`
      IN str_t16 WITH `##`
      RESULTS DATA(tab_t17)
      IGNORING CASE.

    output->display( input = str_t16 name = `str_t16` ).
    output->display( input = tab_t17 name = `tab_t17` ).
    DATA(str_t18) = str_t.

    "... FIRST OCCURRENCE OF
    "Note: The data object, which is declared inline here, is of type repl_result.
    REPLACE FIRST OCCURRENCE OF `ap`
      IN str_t18 WITH `##`
      RESULTS DATA(struc_t19)
      IGNORING CASE.

    output->display( input = str_t18 name = `str_t18` ).
    output->display( input = struc_t19 name = `struc_t19` ).

***********************************************************************

    output->next_section( `21) Position-Based Replacements with REPLACE SECTION ... OF` ).

    DATA(str_u) = `abap ABAP abap`.
    DATA(str_u1) = str_u.

    "OFFSET and LENGTH specified
    REPLACE SECTION OFFSET 5 LENGTH 4 OF str_u1 WITH `#`.

    output->display( input = str_u1 name = `str_u1` ).
    DATA(str_u2) = str_u.

    "Only OFFSET specified, LENGTH: up to the end of the string
    REPLACE SECTION OFFSET 5 OF str_u2 WITH `#`.

    output->display( input = str_u2 name = `str_u2` ).
    DATA(str_u3) = str_u.

    "Only LENGTH specified, OFFSET: starting from the leftmost position
    REPLACE SECTION LENGTH 6 OF str_u3 WITH `#`.

    output->display( input = str_u3 name = `str_u3` ).

***********************************************************************

    output->next_section( `22) Replacements in Internal Tables with REPLACE ... IN TABLE` ).

    DATA(tab_v) = VALUE string_table( ( `aZbzZ` ) ( `cdZze` ) ( `Zzzf` ) ( `ghz` ) ).
    DATA(tab_v1) = tab_v.

    "Replacing all occurrences in a table
    "RESULTS addition: Storing information in an internal table of type repl_result_tab
    REPLACE ALL OCCURRENCES OF `Z`
      IN TABLE tab_v1
      WITH `#`
      RESULTS DATA(res_v2)
      RESPECTING CASE.

    output->display( input = tab_v1 name = `tab_v1` ).
    output->display( input = res_v2 name = `res_v2` ).
    DATA(tab_v3) = tab_v.

    "Replacing the first occurrence in a table
    "RESULTS addition: Storing information in a structure of type repl_result
    REPLACE FIRST OCCURRENCE OF `Z`
      IN TABLE tab_v3
      WITH `#`
      RESULTS DATA(res_v4)
      RESPECTING CASE.

    output->display( input = tab_v3 name = `tab_v3` ).
    output->display( input = res_v4 name = `res_v4` ).
    DATA(tab_v5) = tab_v.

    "Restricting the search range in an internal table
    REPLACE ALL OCCURRENCES OF `Z`
      IN TABLE tab_v5
      FROM 1 TO 2
      WITH `#`
      RESPECTING CASE.

    output->display( input = tab_v5 name = `tab_v5` ).
    DATA(tab_v6) = tab_v.

    "Offsets can be optionally specified (also only the offset of start or end line possible)
    REPLACE ALL OCCURRENCES OF `Z`
      IN TABLE tab_v6
      FROM 1 OFFSET 3 TO 2 OFFSET 2
      WITH `#`
      RESPECTING CASE.

    output->display( input = tab_v6 name = `tab_v6` ).

***********************************************************************

    output->next_section( `23) Replacing Substrings in Strings Using the String Function replace` ).

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

    output->display( input = str_w_1 name = `str_w_1` ).
    output->display( input = str_w_2 name = `str_w_2` ).
    output->display( input = str_w_3 name = `str_w_3` ).
    output->display( input = str_w_4 name = `str_w_4` ).
    output->display( input = str_w_5 name = `str_w_5` ).
    output->display( input = str_w_6 name = `str_w_6` ).
    output->display( input = str_w_7 name = `str_w_7` ).
    output->display( input = str_w_8 name = `str_w_8` ).
    output->display( input = str_w_9 name = `str_w_9` ).
    output->display( input = str_w_10 name = `str_w_10` ).

***********************************************************************

    output->next_section( `Pattern-Based Searching and Replacing in Strings` ).
    output->display( `24) Simple Pattern-Based Searching ` &&
                     `Using Logical Operators` ).

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
      output->display( |CP: The string covers the pattern. |
      && |The offset is { sy-fdpos }.| ).
    ELSE.
      output->display( |CP: The string does not cover the pattern. |
      && |The length of the string is { sy-fdpos }.| ).
    ENDIF.

    "NP (does not conform to pattern)
    IF str_x NP `i+`.
      output->display( |NP: The string does not cover the pattern. |
      && |The length of the string is { sy-fdpos }.| ).
    ELSE.
      output->display( |NP: The string covers the pattern. |
      && |The offset is { sy-fdpos }.| ).
    ENDIF.

***********************************************************************

    output->next_section( `25) Complex Searching Using ` &&
    `Regular Expressions` ).

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

    output->display( input = off_y1 name = `off_y1` ).
    output->display( input = cnt_y2 name = `cnt_y2` ).
    output->display( input = cnt_y3 name = `cnt_y3` ).
    output->display( input = str_y4 name = `str_y4` ).
    output->display( input = str_y5 name = `str_y5` ).
    output->display( input = str_y6 name = `str_y6` ).
    output->display( input = subm_y7 name = `subm_y7` ).
    output->display( input = subm_y8 name = `subm_y8` ).
    output->display( input = cnt_y9 name = `cnt_y9` ).
    output->display( input = tab_y10 name = `tab_y10` ).
    output->display( input = line_y13 name = `line_y13` ).
    output->display( input = off_y14 name = `off_y14` ).
    output->display( input = len_y15 name = `len_y15` ).
    output->display( input = res_y16 name = `res_y16` ).
    output->display( input = tab_y17 name = `tab_y17` ).

***********************************************************************

    output->next_section( `26) Replacing Using Regular Expressions` ).

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

    output->display( input = |Original str_z: { str_z }| ).
    output->display( input = str_z1 name = `str_z1` ).
    output->display( input = str_z2 name = `str_z2` ).
    output->display( input = str_z3 name = `str_z3` ).
    output->display( input = str_z4 name = `str_z4` ).
    output->display( input = str_z5 name = `str_z5` ).
    output->display( input = str_z6 name = `str_z6` ).
    output->display( input = str_z7 name = `str_z7` ).
    output->display( input = str_z8 name = `str_z8` ).
    output->display( input = str_z9 name = `str_z9` ).
    output->display( input = str_z10 name = `str_z10` ).
    output->display( input = str_z11 name = `str_z11` ).
    output->display( input = str_z12 name = `str_z12` ).
    output->display( input = str_z3 name = `str_z13` ).
    output->display( input = |Original str_zb: { str_zb }| ).
    output->display( input = str_z15 name = `str_z15` ).
    output->display( input = str_z16 name = `str_z16` ).
    output->display( input = str_z17 name = `str_z17` ).
    output->display( input = str_z18 name = `str_z18` ).
    output->display( input = str_z19 name = `str_z19` ).
    output->display( input = str_z20 name = `str_z20` ).
    output->display( input = str_z21 name = `str_z21` ).
    output->display( input = |Original str_zc: { str_zc }| ).
    output->display( input = str_z23 name = `str_z23` ).
    output->display( input = str_z24 name = `str_z24` ).
    output->display( input = str_z25 name = `str_z25` ).
    output->display( input = str_zc name = `str_zc` ).
    output->display( input = itab_z name = `itab_z` ).
    output->display( input = |Number of replacements in itab (cnt_z26): { cnt_z26 }| ).

***********************************************************************

    output->next_section( `27) Excursion: System Classes for Regular Expressions` ).

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

    output->display( input = result1 name = `result1` ).

    "You can also use method chaining to save lines of code
    DATA(result2) = cl_abap_regex=>create_pcre( pattern = `\s\w`       "any blank followed by any word character
                                                ignore_case = abap_true )->create_matcher( text = some_string )->find_all( ).

    output->display( input = result2 name = `result2` ).

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

    output->display( input = str_tab_reg_find name = `str_tab_reg_find` ).

  ENDMETHOD.
ENDCLASS.