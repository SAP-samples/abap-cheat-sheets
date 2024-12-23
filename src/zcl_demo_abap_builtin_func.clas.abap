"! <p class="shorttext"><strong>Built-in Functions</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates built-in functions in ABAP.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Information</h2>
"! <p>Find information on getting started with the example class and the disclaimer in
"! the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
CLASS zcl_demo_abap_builtin_func DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_builtin_func IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP cheat sheet example: Built-in Functions\n\n| ).
    out->write( `1) Logical Functions` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& boolc
*&---------------------------------------------------------------------*

    "boolc returns an X or a blank of type string
    DATA(int) = 0.

    DATA(boolc1) = CONV abap_bool( boolc( int IS INITIAL ) ).
    out->write( data = boolc1 name = `boolc1` ).
    out->write( |\n| ).

    DATA(boolc2) = |#{ boolc( int IS INITIAL ) }#|.
    out->write( data = boolc2 name = `boolc2` ).
    out->write( |\n| ).

    DATA(boolc3) = |#{ boolc( int IS NOT INITIAL ) }#|.
    out->write( data = boolc3 name = `boolc3` ).
    out->write( |\n| ).

    "Using the translate function to return a value other than X/blank
    DATA(boolc4) = translate( val = boolc( int BETWEEN -3 AND 3 ) from = `X` to = `1` ).
    out->write( data = boolc4 name = `boolc4` ).
    out->write( |\n| ).

    DATA(boolc5) = translate( val = boolc( int <> 0 ) from = ` ` to = `0` ).
    out->write( data = boolc5 name = `boolc5` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& xsdbool
*&---------------------------------------------------------------------*


    DATA(xsdb1) = xsdbool( 3 > 1 ).
    out->write( data = xsdb1 name = `xsdb1` ).
    out->write( |\n| ).

    DATA(xsdb2) = |#{ xsdbool( 1 = 1 ) }#|.
    out->write( data = xsdb2 name = `xsdb2` ).
    out->write( |\n| ).

    DATA(xsdb3) = |#{ xsdbool( 1 <> 1 ) }#|.
    out->write( data = xsdb3 name = `xsdb3` ).
    out->write( |\n| ).

    "Comparison with boolc
    IF boolc( 1 = 0 ) = xsdbool( 1 = 0 ).
      DATA(res) = `equal`.
    ELSE.
      res = `not equal`.
    ENDIF.
    out->write( data = res name = `res` ).
    out->write( |\n| ).

    "Using xsdbool instead of, for example, an IF control
    "structure or an expression with the COND operator
    DATA(xsdb4) = xsdbool( -1 < 1 ).
    out->write( data = xsdb4 name = `xsdb4` ).
    out->write( |\n| ).

    DATA truth_value1 TYPE abap_bool.
    IF -1 < 1.
      truth_value1 = abap_true.
    ELSE.
      truth_value1 = abap_false.
    ENDIF.
    out->write( data = truth_value1 name = `truth_value1` ).
    out->write( |\n| ).

    DATA(truth_value2) = COND #( WHEN -1 < 1 THEN abap_true ELSE abap_false ).
    out->write( data = truth_value2 name = `truth_value2` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& contains, contains_any_of, contains_any_not_of
*&---------------------------------------------------------------------*

    "-------------------- contains --------------------
    "Specifying the minimum mandatory parameters
    "Unlike most of the following examples, this one uses an IF control structure to
    "visualize the truth value.
    DATA cont1 TYPE abap_bool.

    IF contains( val = `abdefghijklmn` sub = `ghi` ).
      cont1 = abap_true.
    ELSE.
      cont1 = abap_false.
    ENDIF.
    out->write( data = cont1 name = `cont1` ).
    out->write( |\n| ).

    "case (abap_true is the default)

    DATA(cont2) = xsdbool( contains( val = `ABCDE` start = `ab` case = abap_true ) ).
    out->write( data = cont2 name = `cont2` ).
    out->write( |\n| ).

    DATA(cont3) = xsdbool( contains( val = `ABCDE` start = `ab` case = abap_false ) ).
    out->write( data = cont3 name = `cont3` ).
    out->write( |\n| ).

    "end
    DATA(cont4) = xsdbool( contains( val = `UVWXYZ` end = `xyz` case = abap_false ) ).
    out->write( data = cont4 name = `cont4` ).
    out->write( |\n| ).

    "start
    DATA(cont5) = xsdbool( contains( val = `123` start = `2` ) ).
    out->write( data = cont5 name = `cont5` ).
    out->write( |\n| ).

    "off/len can also be specified individually
    "Not specifying off means 0 by default
    DATA(cont6) = xsdbool( contains( val = `##ab## ##cd##` sub = `cd` len = 5 ) ).
    out->write( data = cont6 name = `cont6` ).
    out->write( |\n| ).

    DATA(cont7) = xsdbool( contains( val = `##ab## ##cd##` sub = `cd` off = 7 len = 5 ) ).
    out->write( data = cont7 name = `cont7` ).
    out->write( |\n| ).

    "occ: False if there are more occurrences than specified for occ; i.e. in the following
    "example, specifying the values 1, 2, 3 returns true
    "abap_true is returned for the first 3 loop passes, abap_false for the fourth
    DO 4 TIMES.
      DATA(cont8) = xsdbool( contains( val = `ab#ab#ab#cd#ef#gh` sub = `ab` occ = sy-index ) ).
      out->write( data = cont8 name = `cont8` ).
      out->write( |\n| ).
    ENDDO.

    "pcre
    "In the example, a blank is searched.
    DATA(cont9) = xsdbool( contains( val = `Hallo world` pcre = `\s` ) ).
    out->write( data = cont9 name = `cont9` ).
    out->write( |\n| ).

    "-------------------- contains_any_of --------------------
    DATA(cont10) = xsdbool( contains_any_of( val = `abcdefg` sub = `xyza` ) ).
    out->write( data = cont10 name = `cont10` ).
    out->write( |\n| ).

    DATA(cont11) = xsdbool( contains_any_of( val = `abcdefg` sub = `xyz` ) ).
    out->write( data = cont11 name = `cont11` ).
    out->write( |\n| ).

    DATA(hi) = `1hallo`.
    DATA(abc) = `abcdefghijklmnopqrstuvwxyz`.
    DATA(cont12) = xsdbool( contains_any_of( val = hi start = abc ) ).
    out->write( data = cont12 name = `cont12` ).
    out->write( |\n| ).

    DATA(cont13) = xsdbool( contains_any_of( val = hi end = abc ) ).
    out->write( data = cont13 name = `cont13` ).
    out->write( |\n| ).

    "-------------------- contains_any_not_of --------------------
    DATA(cont14) = xsdbool( contains_any_not_of( val = hi start = abc ) ).
    out->write( data = cont14 name = `cont14` ).
    out->write( |\n| ).

    DATA(cont15) = xsdbool( contains_any_not_of( val = hi end = abc ) ).
    out->write( data = cont15 name = `cont15` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& matches
*&---------------------------------------------------------------------*

    "Checking validity of an email address
    "abap_true
    DATA(matches) = xsdbool( matches( val  = `jon.doe@email.com`
                                      pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ) ).
    out->write( data = matches name = `matches` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& line_exists
*&---------------------------------------------------------------------*

    TYPES: BEGIN OF s,
             comp1 TYPE i,
             comp2 TYPE c LENGTH 3,
           END OF s.
    DATA itab TYPE TABLE OF s WITH EMPTY KEY.
    itab = VALUE #( ( comp1 = 1 comp2 = 'aaa' ) ( comp1 = 2 comp2 = 'bbb' ) ( comp1 = 3 comp2 = 'ccc' ) ).
    DATA(str_tab) = VALUE string_table( ( `abc` ) ( `def` ) ( `ghi` ) ).


    DATA(line_exists1) = xsdbool( line_exists( itab[ 1 ] ) ).
    out->write( data = line_exists1 name = `line_exists1` ).
    out->write( |\n| ).


    DATA(line_exists2) = xsdbool( line_exists( itab[ 4 ] ) ).
    out->write( data = line_exists2 name = `line_exists2` ).
    out->write( |\n| ).


    DATA(line_exists3) = xsdbool( line_exists( itab[ comp1 = 2 ] ) ).
    out->write( data = line_exists3 name = `line_exists3` ).
    out->write( |\n| ).


    DATA(line_exists4) = xsdbool( line_exists( str_tab[ 2 ] ) ).
    out->write( data = line_exists4 name = `line_exists4` ).
    out->write( |\n| ).


    DATA(line_exists5) = xsdbool( line_exists( str_tab[ table_line = `xxx` ] ) ).
    out->write( data = line_exists5 name = `line_exists5` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Numeric Functions` ) ).

*&---------------------------------------------------------------------*
*& abs, sign, ceil, floor, trunc, frac, ipow
*&---------------------------------------------------------------------*

    "----------- abs: Returning the absolute value -----------

    DATA(abs1) = abs( CONV decfloat34( '-4.756' ) ).
    out->write( data = abs1 name = `abs1` ).
    out->write( |\n| ).

    DATA(abs2) = abs( -4 ).
    out->write( data = abs2 name = `abs2` ).
    out->write( |\n| ).

    "----------- sign: Evaluating the sign -----------
    DATA(sign1) = sign( -789 ).
    out->write( data = sign1 name = `sign1` ).
    out->write( |\n| ).

    DATA(sign2) = sign( 5 - 5 ).
    out->write( data = sign2 name = `sign2` ).
    out->write( |\n| ).

    DATA(sign3) = sign( -5 * -5 ).
    out->write( data = sign3 name = `sign3` ).
    out->write( |\n| ).

    "----- ceil: smallest integer not less than the value specified -----

    DATA(ceil1) = ceil( CONV decfloat34( '4.999' ) ).
    out->write( data = ceil1 name = `ceil1` ).
    out->write( |\n| ).

    DATA(ceil2) = ceil( CONV decfloat34( '4.001' ) ).
    out->write( data = ceil2 name = `ceil2` ).
    out->write( |\n| ).

    DATA(ceil3) = ceil( CONV decfloat34( '-4.999' ) ).
    out->write( data = ceil3 name = `ceil3` ).
    out->write( |\n| ).

    DATA(ceil4) = ceil( CONV decfloat34( '-4.001' ) ).
    out->write( data = ceil4 name = `ceil4` ).
    out->write( |\n| ).

    "----- floor: largest integer not less than the value specified -----

    DATA(floor1) = floor( CONV decfloat34( '4.999' ) ).
    out->write( data = floor1 name = `floor1` ).
    out->write( |\n| ).

    DATA(floor2) = floor( CONV decfloat34( '4.001' ) ).
    out->write( data = floor2 name = `floor2` ).
    out->write( |\n| ).

    DATA(floor3) = floor( CONV decfloat34( '-4.999' ) ).
    out->write( data = floor3 name = `floor3` ).
    out->write( |\n| ).

    DATA(floor4) = floor( CONV decfloat34( '-4.001' ) ).
    out->write( data = floor4 name = `floor4` ).
    out->write( |\n| ).

    "------------- trunc: integer part -------------

    DATA(trunc1) = trunc( CONV decfloat34( '4.999' ) ).
    out->write( data = trunc1 name = `trunc1` ).
    out->write( |\n| ).

    DATA(trunc2) = trunc( CONV decfloat34( '4.001' ) ).
    out->write( data = trunc2 name = `trunc2` ).
    out->write( |\n| ).

    DATA(trunc3) = trunc( CONV decfloat34( '-4.999' ) ).
    out->write( data = trunc3 name = `trunc3` ).
    out->write( |\n| ).

    DATA(trunc4) = trunc( CONV decfloat34( '-4.001' ) ).
    out->write( data = trunc4 name = `trunc4` ).
    out->write( |\n| ).

    "------------- frac: decimal places -------------

    DATA(frac1) = frac( CONV decfloat34( '4.999' ) ).
    out->write( data = frac1 name = `frac1` ).
    out->write( |\n| ).

    DATA(frac2) = frac( CONV decfloat34( '4.001' ) ).
    out->write( data = frac2 name = `frac2` ).
    out->write( |\n| ).

    DATA(frac3) = frac( CONV decfloat34( '-4.999' ) ).
    out->write( data = frac3 name = `frac3` ).
    out->write( |\n| ).

    DATA(frac4) = frac( CONV decfloat34( '-4.001' ) ).
    out->write( data = frac4 name = `frac4` ).
    out->write( |\n| ).

    "------------- ipow: Calculalting the power -------------

    DATA(ipow1) = ipow( base = 2 exp = 3 ).
    out->write( data = ipow1 name = `ipow1` ).
    out->write( |\n| ).

    DATA(ipow2) = ipow( base = 10 exp = 0 ).
    out->write( data = ipow2 name = `ipow2` ).
    out->write( |\n| ).

    "Exception is raised
    TRY.
        DATA(ipow3) = ipow( base = 10 exp = 100 ).
      CATCH cx_sy_arithmetic_overflow INTO DATA(error).
        out->write( error->get_text( ) ).
    ENDTRY.


*&---------------------------------------------------------------------*
*& nmin, nmax
*&---------------------------------------------------------------------*

    "A minimum of two, and a maximum of 9 arguments can be specified.
    "Numeric data objects and numeric expressions are possible

    DATA(nmin) =  nmin( val1 = CONV decfloat34( '1.34' )
                         val2 = CONV decfloat34( '56.7' )
                         val3 = CONV decfloat34( '890.123' )
                         val4 = CONV decfloat34( '0.999' ) ).
    out->write( data = nmin name = `nmin` ).
    out->write( |\n| ).

    DATA(nmax) =  nmax( val1 = CONV decfloat34( '1.34' )
                         val2 = CONV decfloat34( '56.7' )
                         val3 = CONV decfloat34( '890.123' )
                         val4 = CONV decfloat34( '0.999' ) ).
    out->write( data = nmax name = `nmax` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& acos, asin, atan, cos, sin, tan, cosh, sinh, tanh, exp, log, log10, sqrt
*&---------------------------------------------------------------------*

    "Calculating the square root

    DATA(sqrt1) = sqrt( CONV decfloat34( '9' ) ).
    out->write( data = sqrt1 name = `sqrt1` ).
    out->write( |\n| ).

    DATA(sqrt2) = sqrt( CONV decfloat34( '40.96' ) ).
    out->write( data = sqrt2 name = `sqrt2` ).
    out->write( |\n| ).

    "Calculating the logarithm to base 10

    DATA(log10) = log10( CONV decfloat34( '1000' ) ).
    out->write( data = log10 name = `log10` ).
    out->write( |\n| ).

    DATA(sine) = sin( '30' ).
    out->write( data = sine name = `sine` ).
    out->write( |\n| ).

    DATA(cosine) = cos( '45' ).
    out->write( data = cosine name = `cosine` ).
    out->write( |\n| ).

    DATA(tangent) = tan( '90' ).
    out->write( data = tangent name = `tangent` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& round, rescale
*&---------------------------------------------------------------------*

    "Rounding to decimal places

    DATA(round1) = round( val = CONV decfloat34( '1.2374' ) dec = 2 ).
    out->write( data = round1 name = `round1` ).
    out->write( |\n| ).

    DATA(round2) = round( val = CONV decfloat34( '1.2374' ) dec = 3 ).
    out->write( data = round2 name = `round2` ).
    out->write( |\n| ).

    "Rounding to precision

    DATA(round3) = round( val = CONV decfloat34( '1234567890123' ) prec = 10 ).
    out->write( data = round3 name = `round3` ).
    out->write( |\n| ).

    DATA(round4) = round( val = CONV decfloat34( '1234' ) prec = 3 ).
    out->write( data = round4 name = `round4` ).
    out->write( |\n| ).

    "Rescaling function
    "Similar to the round function, the dec (for scaling) or prec (for precision)
    "parameters must be specified. The input is rounded if required.

    DATA(rescale1) = rescale( val = CONV decfloat34( '1234.56789' ) dec = 0 ).
    out->write( data = rescale1 name = `rescale1` ).
    out->write( |\n| ).

    DATA(rescale2) = rescale( val = CONV decfloat34( '1234.56789' ) dec = 1 ).
    out->write( data = rescale2 name = `rescale2` ).
    out->write( |\n| ).

    DATA(rescale3) = rescale( val = CONV decfloat34( '1234.56789' ) prec = 3 ).
    out->write( data = rescale3 name = `rescale3` ).
    out->write( |\n| ).

    DATA(rescale4) = rescale( val = CONV decfloat34( '1234.56789' ) prec = 10 ).
    out->write( data = rescale4 name = `rescale4` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) String Functions` ) ).

*&---------------------------------------------------------------------*
*& numofchar, strlen, xstrlen
*&---------------------------------------------------------------------*

    "numofchar: Trailing blanks are not counted in both strings of fixed and variable length
    "strlen: Trailing blanks are not counted in strings of fixed length; in strings of
    "        variable length, they are counted

    DATA(numofchar1)   = numofchar( 'abc   ' ).
    out->write( data = numofchar1 name = `numofchar1` ).
    out->write( |\n| ).

    DATA(numofchar2)   = numofchar( `abc   ` ).
    out->write( data = numofchar2 name = `numofchar2` ).
    out->write( |\n| ).


    DATA(strlen1) = strlen( 'abc   ' ).
    out->write( data = strlen1 name = `strlen1` ).
    out->write( |\n| ).


    DATA(strlen2) = strlen( `abc   ` ).
    out->write( data = strlen2 name = `strlen2` ).
    out->write( |\n| ).

    "xstrlen for type xstring
    DATA(xstr) = CONV xstring( `480065006C006C006F00200077006F0072006C0064002100` ).

    DATA(len_xstr) = xstrlen( xstr ).
    out->write( data = len_xstr name = `len_xstr` ).
    out->write( |\n| ).

    "xstring -> string
    DATA(conv_str) = cl_abap_conv_codepage=>create_in( )->convert( xstr ).

*&---------------------------------------------------------------------*
*& cmin, cmax
*&---------------------------------------------------------------------*

    DATA(cmin) =  cmin( val1 = `zzzzzzz`
                        val2 = `zzazzzzzzzz` "smallest argument
                        val3 = `zzzzabc` ).

    out->write( data = cmin name = `cmin` ).
    out->write( |\n| ).

    DATA(cmax) =  cmax( val1 = `abcdef`      "biggest argument
                        val2 = `aaghij`
                        val3 = `aaaaklmn`
                        val4 = `aaaaaaopqrs`
                        val5 = `aaaaaaaaaatuvwxy`
                        val6 = `aaaaaaaaaaaaaz` ).
    out->write( data = cmax name = `cmax` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& find, find_end, find_any_of, find_any_not_of
*&---------------------------------------------------------------------*

    DATA(str) = `Pieces of cakes.`.

    "---------------- find ----------------
    "The find function searches for the substring specified and returns the offset

    DATA(find1) = find( val = str sub = `of` ).
    out->write( data = find1 name = `find1` ).
    out->write( |\n| ).

    DATA(find2) = find( val = str sub = `x` ).
    out->write( data = find2 name = `find2` ).
    out->write( |\n| ).

    "case
    DATA(find3) = find( val = str sub = `p` case = abap_false ).
    out->write( data = find3 name = `find3` ).
    out->write( |\n| ).

    "off/len
    DATA(find4) = find( val = str sub = `ca` off = 4 len = 5 ).
    out->write( data = find4 name = `find4` ).
    out->write( |\n| ).

    DATA(find5) = find( val = str sub = `ca` off = 4 len = 10 ).
    out->write( data = find5 name = `find5` ).
    out->write( |\n| ).

    "occ
    DATA(find6) = find( val = str sub = `es` occ = 1 ).
    out->write( data = find6 name = `find6` ).
    out->write( |\n| ).

    DATA(find7) = find( val = str sub = `es` occ = 2 ).
    out->write( data = find7 name = `find7` ).
    out->write( |\n| ).

    DATA(find8) = find( val = str sub = `es` occ = 3 ).
    out->write( data = find8 name = `find8` ).
    out->write( |\n| ).

    "pcre
    DATA(find9) = find( val = str pcre = `\.` ).
    out->write( data = find9 name = `find9` ).
    out->write( |\n| ).

    "---------------- find_end ----------------
    "find_end returns the sum of the offset of the occurrence plus the length of the match

    DATA(find_end1) = find_end( val = str sub = `of` ).
    out->write( data = find_end1 name = `find_end1` ).
    out->write( |\n| ).

    DATA(find_end2) = find_end( val = str pcre = `\s` ).
    out->write( data = find_end2 name = `find_end2` ).
    out->write( |\n| ).

    "---------------- find_any_of ----------------
    "find_any_of returns the offset of the occurrence of any character contained
    "in a substring. The search is always case-sensitive.

    DATA(find_any_of1) = find_any_of( val = str sub = `x523z4e` ).
    out->write( data = find_any_of1 name = `find_any_of1` ).
    out->write( |\n| ).

    DATA(find_any_of2) = find_any_of( val = str sub = `zwq85t` ).
    out->write( data = find_any_of2 name = `find_any_of2` ).
    out->write( |\n| ).

    "---------------- find_any_not_of ----------------
    "find_any_not_of is the negation of find_any_of

    DATA(find_any_not_of1) = find_any_not_of( val = str sub = `ieces` ).
    out->write( data = find_any_not_of1 name = `find_any_not_of1` ).
    out->write( |\n| ).

    DATA(find_any_not_of2) = find_any_not_of( val = str sub = `P` ).
    out->write( data = find_any_not_of2 name = `find_any_not_of2` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& count, count_any_of, count_any_not_of
*&---------------------------------------------------------------------*

    DATA(st) = `Pieces of cakes.`.

    "---------------- count ----------------

    DATA(count1) = count( val = st sub = `e` ).
    out->write( data = count1 name = `count1` ).
    out->write( |\n| ).

    DATA(count2)  = count( val = st sub = `x` ).
    out->write( data = count2 name = `count2` ).
    out->write( |\n| ).

    "case (case-sensitive by default)
    DATA(count3)  = count( val = st sub = `p` case = abap_false ).
    out->write( data = count3 name = `count3` ).
    out->write( |\n| ).

    "off/len (off is 0 by default; len is the length of sting by default minus offset)
    DATA(count4)  = count( val = st sub = `es` off = 3 ).
    out->write( data = count4 name = `count4` ).
    out->write( |\n| ).

    DATA(count5)  = count( val = st sub = `es` off = 9 ).
    out->write( data = count5 name = `count5` ).
    out->write( |\n| ).

    DATA(count6)  = count( val = st sub = `es` off = 3 len = 12 ).
    out->write( data = count6 name = `count6` ).
    out->write( |\n| ).

    DATA(count7)  = count( val = st sub = `es` len = 5 ).
    out->write( data = count7 name = `count7` ).
    out->write( |\n| ).

    "pcre
    DATA(count8)  = count( val = st pcre = `\s` ).
    out->write( data = count8 name = `count8` ).
    out->write( |\n| ).

    DATA(count9)  = count( val = st pcre = `.` ).
    out->write( data = count9 name = `count9` ).
    out->write( |\n| ).

    "---------------- count_any_of ----------------

    DATA(count_any_of1) = count_any_of( val = st sub = `x523z4e` ).
    out->write( data = count_any_of1 name = `count_any_of1` ).
    out->write( |\n| ).

    DATA(count_any_of2) = count_any_of( val = st sub = `eco` ).
    out->write( data = count_any_of2 name = `count_any_of2` ).
    out->write( |\n| ).

    "---------------- count_any_not_of ----------------

    DATA(count_any_not_of1) = count_any_not_of( val = st sub = `fP` ).
    out->write( data = count_any_not_of1 name = `count_any_not_of1` ).
    out->write( |\n| ).

    DATA(count_any_not_of2) = count_any_not_of( val = st sub = `Piecs ofak.` ).
    out->write( data = count_any_not_of2 name = `count_any_not_of2` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& distance
*&---------------------------------------------------------------------*

    DATA(str_to_check) = `abap`.

    DATA(dist1) = distance( val1 = str_to_check val2 = `abap` ).
    out->write( data = dist1 name = `dist1` ).
    out->write( |\n| ).

    DATA(dist2) = distance( val1 = str_to_check val2 = `axbap` ).
    out->write( data = dist2 name = `dist2` ).
    out->write( |\n| ).

    DATA(dist3) = distance( val1 = str_to_check val2 = `yabyyapy` ).
    out->write( data = dist3 name = `dist3` ).
    out->write( |\n| ).

    DATA(dist4) = distance( val1 = str_to_check val2 = `zabapzzzzzzzzzzzz` max = 5 ).
    out->write( data = dist4 name = `dist4` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& repeat
*&---------------------------------------------------------------------*

    "abapabapabapabapabap
    DATA(repeat1) = repeat( val = `abap` occ = 5 ).
    out->write( data = repeat1 name = `repeat1` ).
    out->write( |\n| ).

    DATA(repeat2) = |#{ repeat( val = ` ` occ = 10 ) }#|.
    out->write( data = repeat2 name = `repeat2` ).
    out->write( |\n| ).

    "Y (initial value returned)
    DATA(repeat3) = COND #( WHEN repeat( val = `a` occ = 0 ) = `` THEN `Y` ELSE `Z` ).
    out->write( data = repeat3 name = `repeat3` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& condense
*&---------------------------------------------------------------------*

    DATA(str_to_condense) = ` ab   cd `.

    "No parameters specified, i. e. their default values are provided.
    "Works like CONDENSE statement without the NO-GAPS addition.
    DATA(condense1) = condense( str_to_condense ).
    out->write( data = condense1 name = `condense1` ).
    out->write( |\n| ).

    "Parameters del/to not specified. from parameter with initial string
    "(could also be a text field literal: from = ' '). This way, leading and
    "trailing blanks are removed.
    DATA(condense2) = condense( val = str_to_condense from = `` ).
    out->write( data = condense2 name = `condense2` ).
    out->write( |\n| ).

    "Parameter to specified with an initial string. No other parameters.
    "Works like the CONDENSE statement with the NO-GAPS addition.
    DATA(condense3) = condense( val = str_to_condense  to = `` ).
    out->write( data = condense3 name = `condense3` ).
    out->write( |\n| ).

    "Parameter del specifies the leading/trailing characters to be removed.
    DATA(condense4) = condense( val = `##see###you##` del = `#` ).
    out->write( data = condense4 name = `condense4` ).
    out->write( |\n| ).

    "If from and to are specified along with del, leading/trailing characters
    "specified in del are first removed. Then, in the remaining string, all
    "substrings composed of characters specified in from are replaced with the
    "first character of the string specified in the to parameter.
    DATA(condense5) = condense( val  = `  Rock'xxx'Roller`
                                del  = `re `
                                from = `x`
                                to   = `n` ).
    out->write( data = condense5 name = `condense5` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& concat_lines_of
*&---------------------------------------------------------------------*

    DATA(stringtable) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).

    DATA(con1) = concat_lines_of( table = stringtable ).
    out->write( data = con1 name = `con1` ).
    out->write( |\n| ).

    DATA(con2) = concat_lines_of( table = stringtable sep = ` ` ).
    out->write( data = con2 name = `con2` ).
    out->write( |\n| ).

    DATA(con3) = concat_lines_of( table = stringtable sep = `/` ).
    out->write( data = con3 name = `con3` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& reverse
*&---------------------------------------------------------------------*

    DATA(reverse) = reverse( `paba` ).
    out->write( data = reverse name = `reverse` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& escape
*&---------------------------------------------------------------------*

    "Context: URLs
    DATA(esc1) = escape( val    = '...test: 5@8...'
                         format = cl_abap_format=>e_url_full ).
    out->write( data = esc1 name = `esc1` ).
    out->write( |\n| ).

    "Context: JSON
    DATA(esc2) = escape( val    = 'some "test" json \ with backslash and double quotes'
                         format = cl_abap_format=>e_json_string ).
    out->write( data = esc2 name = `esc2` ).
    out->write( |\n| ).

    "Context: String templates
    DATA(esc3) = escape( val    = 'Special characters in string templates: |, \, {, }'
                         format = cl_abap_format=>e_string_tpl ).
    out->write( data = esc3 name = `esc3` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& insert
*&---------------------------------------------------------------------*

    DATA(to_be_inserted) = `ABAP`.

    DATA(insert1) = insert( val = to_be_inserted sub = `#` ).
    out->write( data = insert1 name = `insert1` ).
    out->write( |\n| ).

    DATA(insert2) = insert( val = to_be_inserted sub = `#` off = 1 ).
    out->write( data = insert2 name = `insert2` ).
    out->write( |\n| ).

    DATA(insert3) = insert( val = to_be_inserted sub = `#` off = strlen( to_be_inserted ) ).
    out->write( data = insert3 name = `insert3` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& match
*&---------------------------------------------------------------------*

    DATA(match1) = match( val = `The email address is jon.doe@email.com.`
                          pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ).
    out->write( data = match1 name = `match1` ).
    out->write( |\n| ).

    "Find blank (without inlcuding it in the result indicated by \K) and
    "the following 2 characters, second occurrence
    DATA(match2) = match( val = `The email address is jon.doe@email.com.`
                          pcre = `\s\K..`
                          occ = 2 ).
    out->write( data = match2 name = `match2` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& replace
*&---------------------------------------------------------------------*

    DATA(to_be_replaced) = `Pieces of cakes.`.

    DATA(replace1) = replace( val = to_be_replaced sub = `es` with = `#` ).
    out->write( data = replace1 name = `replace1` ).
    out->write( |\n| ).

    "case
    DATA(replace2) = replace( val = to_be_replaced sub = `p` case = abap_false with = `#` ).
    out->write( data = replace2 name = `replace2` ).
    out->write( |\n| ).

    "occ
    DATA(replace3) = replace( val = to_be_replaced sub = ` ` occ = 2 with = `#` ).
    out->write( data = replace3 name = `replace3` ).
    out->write( |\n| ).

    "The value 0 in occ means respecting all occurrences.
    DATA(replace4) = replace( val = to_be_replaced sub = `e` occ = 0 with = `#` ).
    out->write( data = replace4 name = `replace4` ).
    out->write( |\n| ).

    "pcre
    DATA(replace5) = replace( val = to_be_replaced pcre = `\s` with = `#` ).
    out->write( data = replace5 name = `replace5` ).
    out->write( |\n| ).

    DATA(replace6) = replace( val = to_be_replaced pcre = `\s` occ = 2 with = `#` ).
    out->write( data = replace6 name = `replace6` ).
    out->write( |\n| ).

    "Replacement determined by offset/length specification only (no sub/pcre specification)
    DATA(replace7) = replace( val = to_be_replaced off = 5 with = `#` ).
    out->write( data = replace7 name = `replace7` ).
    out->write( |\n| ).

    DATA(replace8) = replace( val = to_be_replaced len = 5 with = `#` ).
    out->write( data = replace8 name = `replace8` ).
    out->write( |\n| ).

    DATA(replace9) = replace( val = to_be_replaced off = 3 len = 7 with = `#` ).
    out->write( data = replace9 name = `replace9` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& segment
*&---------------------------------------------------------------------*

    "index: Number of segment
    "sep: Substring specified is searched and used as limit
    DATA(segment1) = segment( val = `Hallo,world,123` index = 1 sep = `,` ).
    out->write( data = segment1 name = `segment1` ).
    out->write( |\n| ).

    DATA(segment2) = segment( val = `Hallo,world,123` index = -1 sep = `,` ).
    out->write( data = segment2 name = `segment2` ).
    out->write( |\n| ).

    DATA(segment3) = segment( val = `Hallo<br>world<br>123` index = 2 sep = `<br>` ).
    out->write( data = segment3 name = `segment3` ).
    out->write( |\n| ).

    "space: Each individual character is searched and used as limit
    DATA(to_be_segmented) = `a/b#c d.e`.

    DATA(segment4) = segment( val = `a/b#c d.e` index = 2 space = `. #/` ).
    out->write( data = segment4 name = `segment4` ).
    out->write( |\n| ).

    DATA segment_tab TYPE string_table.
    DO.
      TRY.
          INSERT segment( val   = to_be_segmented
                          index = sy-index
                          space = `. #/` ) INTO TABLE segment_tab.
        CATCH cx_sy_strg_par_val.
          EXIT.
      ENDTRY.
    ENDDO.
    out->write( data = segment_tab name = `segment_tab` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& shift_left, shift_right
*&---------------------------------------------------------------------*

    DATA(to_be_shifted) = ` hallo `.

    "------------------- shift_left -------------------
    DATA(shift_left1) = shift_left( val = to_be_shifted places = 3 ).
    out->write( data = shift_left1 name = `shift_left1` ).
    out->write( |\n| ).

    "circular parameter: characters that are moved out of the string are
    "added at the other end again
    DATA(shift_left2) = shift_left( val = to_be_shifted circular = 2 ).
    out->write( data = shift_left2 name = `shift_left2` ).
    out->write( |\n| ).

    DATA(shift_left3) = shift_left( val = to_be_shifted sub = ` hal` ).
    out->write( data = shift_left3 name = `shift_left3` ).
    out->write( |\n| ).

    "No parameter except val: Behaves as if sub was passed a blank character
    DATA(shift_left4) = shift_left( val = to_be_shifted ).
    out->write( data = shift_left4 name = `shift_left4` ).
    out->write( |\n| ).

    DATA(shift_left5) = shift_left( val = to_be_shifted sub = ` ` ).
    out->write( data = shift_left5 name = `shift_left5` ).
    out->write( |\n| ).

    "------------------- shift_right -------------------

    DATA(shift_right1) = shift_right( val = to_be_shifted places = 3 ).
    out->write( data = shift_right1 name = `shift_right1` ).
    out->write( |\n| ).

    DATA(shift_right2) = shift_right( val = to_be_shifted circular = 2 ).
    out->write( data = shift_right2 name = `shift_right2` ).
    out->write( |\n| ).

    DATA(shift_right3) = shift_right( val = to_be_shifted sub = `o ` ).
    out->write( data = shift_right3 name = `shift_right3` ).
    out->write( |\n| ).

    DATA(shift_right4) = shift_right( val = to_be_shifted ).
    out->write( data = shift_right4 name = `shift_right4` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& substring, substring_after, substring_before, substring_to, substring_from
*&---------------------------------------------------------------------*

    DATA(s4func) = `Lorem ipsum dolor sit amet`.

    "------------------- substring -------------------
    "Extracting substring starting at a specific position
    "'len' not specified means the rest of the remaining characters is
    "respected
    DATA(substr1) = substring( val = s4func off = 6 ).
    out->write( data = substr1 name = `substr1` ).
    out->write( |\n| ).

    "Extracting substring with a specific length
    "'off' is not specified and has the default value 0.
    DATA(substr2) = substring( val = s4func len = 5 ).
    out->write( data = substr2 name = `substr2` ).
    out->write( |\n| ).

    "Specifying both off and len parameters
    DATA(substr3) = substring( val = s4func off = 6 len = 5 ).
    out->write( data = substr3 name = `substr3` ).
    out->write( |\n| ).

    "------------------- substring_after -------------------
    "Extracting a substring ...
    "... after a specified substring
    DATA(substr_after1) = substring_after( val = s4func sub = `or` ).
    out->write( data = substr_after1 name = `substr_after1` ).
    out->write( |\n| ).

    "... after a specified substring specifying the occurence in a string
    "and restricting the length
    "occ/case
    DATA(substr_after2) = substring_after( val = s4func sub = `oR` occ = 2 len = 7 case = abap_false ).
    out->write( data = substr_after2 name = `substr_after2` ).
    out->write( |\n| ).

    "pcre
    DATA(substr_after3) = substring_after( val = s4func pcre = `\s.` occ = 2 ).
    out->write( data = substr_after3 name = `substr_after3` ).
    out->write( |\n| ).

    "------------------- substring_before -------------------
    "... before a specified substring
    DATA(substr_before) = substring_before( val = s4func sub = `um`  ).
    out->write( data = substr_before name = `substr_before` ).
    out->write( |\n| ).

    "------------------- substring_from -------------------
    "... from a specified substring on. It includes the substring specified
    "in sub. len/off and other parameters are possible.
    DATA(substr_from) = substring_from( val = s4func sub = `um` ).
    out->write( data = substr_from name = `substr_from` ).
    out->write( |\n| ).

    "Compared to substring_after
    DATA(substr_after4) = substring_after( val = s4func sub = `um` ).
    out->write( data = substr_after4 name = `substr_after4` ).
    out->write( |\n| ).

    "------------------- substring_to -------------------
    "... up to a specified substring. It includes the substring specified
    "in sub. len/off and other parameters are possible.
    DATA(substr_to) = substring_to( val = s4func sub = `um` ).
    out->write( data = substr_to name = `substr_to` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& to_upper, to_lower, from_mixed, to_mixed
*&---------------------------------------------------------------------*

    "------------------- to_upper -------------------
    DATA(upper1) = to_upper( `AbaP` ).
    out->write( data = upper1 name = `upper1` ).
    out->write( |\n| ).
    DATA(upper2) = to_upper( `abap` ).
    out->write( data = upper2 name = `upper2` ).
    out->write( |\n| ).

    "------------------- to_lower -------------------

    DATA(lower1) = to_lower( `AbaP` ).
    out->write( data = lower1 name = `lower1` ).
    out->write( |\n| ).
    DATA(lower2) = to_lower( `ABAP` ).
    out->write( data = lower2 name = `lower2` ).
    out->write( |\n| ).

    "------------------- from_mixed -------------------
    "sep: Inserts the first character specified in sep before each uppercase letter
    "from left to right, starting with the second position
    DATA(from_mixed1) = from_mixed( val = `ABAP` sep = `#` ).
    out->write( data = from_mixed1 name = `from_mixed1` ).
    out->write( |\n| ).

    "If 'case' is not specified or if the first character in the 'case' parameter is an
    "uppercase letter, the entire string is transformed to uppercase, otherwise to
    "lowercase.
    DATA(from_mixed2) = from_mixed( val = `AbaP` sep = `#` ).
    out->write( data = from_mixed2 name = `from_mixed2` ).
    out->write( |\n| ).

    "Underscore is the default separator
    DATA(from_mixed3) = from_mixed( val = `AbaP` ).
    out->write( data = from_mixed3 name = `from_mixed3` ).
    out->write( |\n| ).

    DATA(from_mixed4) = from_mixed( val = `AbaP` sep = `#` case = 'X' ).
    out->write( data = from_mixed4 name = `from_mixed4` ).
    out->write( |\n| ).

    DATA(from_mixed5) = from_mixed( val = `AbaP` sep = `#` case = 'x' ).
    out->write( data = from_mixed5 name = `from_mixed5` ).
    out->write( |\n| ).

    "min: Passing a positive number to specify a minimum number of characters
    "that must appear before an uppercase letter from the start of the string
    "or since the last insertion so that a separator is inserted. The default
    "value for 'min' is 1.
    DATA(from_mixed6) = from_mixed( val = `ABaP` sep = `#` min = 1 ).
    out->write( data = from_mixed6 name = `from_mixed6` ).
    out->write( |\n| ).

    DATA(from_mixed7) = from_mixed( val = `ABaaAaaaaAP` sep = `#` min = 3 ).
    out->write( data = from_mixed7 name = `from_mixed7` ).
    out->write( |\n| ).

    "------------------- to_mixed -------------------
    "Transforming all letters in the string to lowercase letters from the second
    "position on. From left to right from the second position on, it removes
    "occurrences of the first character specified in the 'sep' parameter from the
    "string and transforms the next letter to an uppercase letter.
    "Default separator _

    DATA(to_mixed1) = to_mixed( val = `Abc_de_fg_hi` ).
    out->write( data = to_mixed1 name = `to_mixed1` ).
    out->write( |\n| ).

    DATA(to_mixed2) = to_mixed( val = `Abc/de/fg/hi` sep = `/` ).
    out->write( data = to_mixed2 name = `to_mixed2` ).
    out->write( |\n| ).

    "Specifying the case parameter
    DATA(to_mixed3) = to_mixed( val = `AbcXdeXfgXhi` sep = `X` case = 'x' ).
    out->write( data = to_mixed3 name = `to_mixed3` ).
    out->write( |\n| ).

    "Specifying the min operator
    DATA(to_mixed4) = to_mixed( val = `Abc/de/fg/hi` sep = `/` min = 2 ).
    out->write( data = to_mixed4 name = `to_mixed4` ).
    out->write( |\n| ).

    DATA(to_mixed5) = to_mixed( val = `Abc/de/fghijklmno/pq` sep = `/` min = 5 ).
    out->write( data = to_mixed5 name = `to_mixed5` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& translate
*&---------------------------------------------------------------------*

    DATA(to_be_translated) = `___abc_def_____ghi_`.

    "Each character that occurs in the 'from' parameter is replaced by the character
    "that occurs in the same place in the 'to' parameter as in the 'from' parameter.
    "If 'to' is shorter than 'from', the surplus characters from 'from' are removed
    "from the string.
    DATA(translate1) = translate( val = to_be_translated from = `hi_` to = `#?` ).
    out->write( data = translate1 name = `translate1` ).
    out->write( |\n| ).

    DATA(translate2) = translate( val = to_be_translated from = `_`  to = `#?` ).
    out->write( data = translate2 name = `translate2` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Time Stamp Functions` ) ).


*&---------------------------------------------------------------------*
*& utclong_current
*&---------------------------------------------------------------------*

    "The return value has the type utclong.
    DATA(ts1) = utclong_current( ).
    out->write( data = ts1 name = `ts1` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& utclong_add
*&---------------------------------------------------------------------*

    DATA(utc4calc) = CONV utclong( '2024-01-01 15:55:14.1173220' ).

    "At least one parameter must be specified besides 'val'.
    "Note that there are no parameters for years and months.

    "Adding one hour
    DATA(ts2) = utclong_add( val = utc4calc
                             hours = 1 ).
    out->write( data = ts2 name = `ts2` ).
    out->write( |\n| ).

    "Subtracting one hour by passing a negative integer value (no
    "separate substract function is available)
    DATA(ts3) = utclong_add( val = utc4calc
                             hours = -1 ).
    out->write( data = ts3 name = `ts3` ).
    out->write( |\n| ).

    "Using all parameters
    DATA(ts4) = utclong_add( val = utc4calc
                             days = 1
                             hours = 2
                             minutes = CONV int8( '13' )
                             seconds = CONV decfloat34( '53.12' ) ).
    out->write( data = ts4 name = `ts4` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& utclong_diff
*&---------------------------------------------------------------------*

    DATA(ts5) = CONV utclong( '2024-01-01 05:30:00' ).
    DATA(ts6) = CONV utclong( '2024-01-01 06:30:00' ).

    "The return value has the type decfloat34. It contains the exact difference in seconds.

    DATA(ts_diff1) = utclong_diff( high = ts6
                                   low = ts5 ).
    out->write( data = ts_diff1 name = `ts_diff1` ).
    out->write( |\n| ).


    DATA(ts_diff2) = utclong_diff( high = ts5
                                   low = ts6 ).
    out->write( data = ts_diff2 name = `ts_diff2` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Table Functions` ) ).


*&---------------------------------------------------------------------*
*& lines
*&---------------------------------------------------------------------*

    DATA(strtab) = VALUE string_table( ( `aaa` ) ( `bbb` ) ( `ccc` ) ( `ddd` ) ( `eee` ) ).


    DATA(lines1) = lines( strtab ).
    out->write( data = lines1 name = `lines1` ).
    out->write( |\n| ).

    DELETE strtab INDEX 1.

    DATA(lines2) = lines( strtab ).
    out->write( data = lines2 name = `lines2` ).
    out->write( |\n| ).

    CLEAR strtab.

    DATA(lines3) = lines( strtab ).
    out->write( data = lines3 name = `lines3` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& line_index
*&---------------------------------------------------------------------*

    TYPES: BEGIN OF st,
             comp1 TYPE i,
             comp2 TYPE c LENGTH 3,
             comp3 TYPE c LENGTH 3,
           END OF st.
    DATA itab_em TYPE TABLE OF st WITH EMPTY KEY.
    DATA itab_sec TYPE HASHED TABLE OF st
                  WITH UNIQUE KEY comp1
                  WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp2.

    itab_em = VALUE #( ( comp1 = 1 comp2 = 'e' comp3 = 'z' )
                       ( comp1 = 2 comp2 = 'd' comp3 = 'y' )
                       ( comp1 = 3 comp2 = 'c' comp3 = 'x' )
                       ( comp1 = 4 comp2 = 'b' comp3 = 'w' )
                       ( comp1 = 5 comp2 = 'a' comp3 = 'v' ) ).

    itab_sec = itab_em.
    DATA(itab_str) = VALUE string_table( ( `aaa` ) ( `bbb` ) ( `ccc` ) ( `ddd` ) ( `eee` ) ).

    "Note:
    "The table expression must be determined by a key specification (explicit table
    "key, free key).

    "Using a free key

    DATA(line_index1) = line_index( itab_em[ comp1 = 1 ] ).
    out->write( data = line_index1 name = `line_index1` ).
    out->write( |\n| ).


    DATA(line_index2) = line_index( itab_em[ comp2 = 'd' ] ).
    out->write( data = line_index2 name = `line_index2` ).
    out->write( |\n| ).

    "Note: A hashed table does not have a primary table index. The result is -1.
    DATA(line_index3) = line_index( itab_sec[ KEY primary_key comp1 = 1 ] ).
    out->write( data = line_index3 name = `line_index3` ).
    out->write( |\n| ).

    "Hashed tables can be assigned a secondary table index using a secondary
    "table key.

    DATA(line_index4) = line_index( itab_sec[ KEY sk comp2 = 'd' ] ).
    out->write( data = line_index4 name = `line_index4` ).
    out->write( |\n| ).


    DATA(line_index5) = line_index( itab_sec[ KEY sk comp2 = 'a' ] ).
    out->write( data = line_index5 name = `line_index5` ).
    out->write( |\n| ).

    "Specifying the pseudo component table_line
    DATA(line_index6) = line_index( itab_str[ table_line = `aaa` ] ).
    out->write( data = line_index6 name = `line_index6` ).
    out->write( |\n| ).


    DATA(line_index7) = line_index( itab_str[ table_line = `zzz` ] ).
    out->write( data = line_index7 name = `line_index7` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) Built-In Functions for ABAP SQL` ) ).

*&---------------------------------------------------------------------*
*& Functions for Numeric Values
*&---------------------------------------------------------------------*

    SELECT SINGLE
      "Division, result rounded to an integer

      div( 4, 2 ) AS div,

      "Division, 3rd argument: result is rounded to the specified
      "number of decimals

      division( 1, 3, 2 ) AS division,

      "Result is rounded to first greater integer

      ceil( decfloat34`1.333` ) AS ceil,

      "Result is the remainder of division

      mod( 3, 2 ) AS mod,

      "Largest integer value not greater than the specified value

      floor( decfloat34`1.333` ) AS floor,

      "Returns the absolute number

      abs( int4`-2` ) AS abs,

      "Result is rounded to the specified position after the decimal separator

      round( decfloat34`1.337`, 2 ) AS round

      FROM zdemo_abap_carr
      WHERE carrid = 'LH'
      INTO @DATA(numeric_functions).

    out->write( data = numeric_functions name = `numeric_functions` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Functions for Strings
*&---------------------------------------------------------------------*

    SELECT SINGLE
      carrid,    "LH
      carrname,  "Lufthansa
      url,       "http://www.lufthansa.com

      "Concatenates strings, ignores trailing blanks
      "LHLufthansa
      concat( carrid, carrname ) AS concat,

      "Concatenates strings, number denotes the blanks that are inserted
      "LH Lufthansa
      concat_with_space( carrid, carrname, 1 ) AS concat_with_space,

      "First letter of a word -> uppercase, all other letters -> lowercase;
      "note that a space and other special characters means a new word.
      "Http://Www.Lufthansa.Com
      initcap( url ) AS initcap,

      "Position of the first occurrence of the substring specified

      instr( carrname,'a' ) AS instr,

      "String of length n starting from the left of an expression;
      "trailing blanks are ignored
      "Luft
      left( carrname, 4 ) AS left,

      "Number of characters in an expression, trailing blanks are ignored

      length( url ) AS length,

      "Checks if expression contains a PCRE expression;
      "case-sensitive by default (case_sensitive parameter can be specified)
      "Notes on the 1 = found, 0 = not found

      like_regexpr( pcre  = '\..',         "Period that is followed by any character
                    value = url ) AS like_regex,

      "Returns position of a substring in an expression,



      locate( carrname, 'a', 0, 2 ) AS locate,

      "Searches a PCRE pattern, returns offset of match;
      "many optional parameters: occurrence, case_sensitive, start, group

      locate_regexpr( pcre = '\..',        "Period followed by any character
                      value = url,
                      occurrence = 2 )
                      AS locate_regexpr,

      "Searches a PCRE pattern, returns offset of match + 1;
      "many optional parameters: occurrence, case_sensitive, start, group

      locate_regexpr_after( pcre = '.',     "Any character
                            value = url,
                            occurrence = 1 ) AS locate_regexpr_after,

      "Removes leading characters as specified in the 2nd argument,
      "trailing blanks are removed
      "ufthansa
      ltrim( carrname, 'L' ) AS ltrim,

      "Counts all occurrences of found PCRE patterns

      occurrences_regexpr( pcre = '\..',    "Period that is followed by any character
                            value = url ) AS occ_regex,

      "Replaces the 2nd argument with the 3rd in an expression
      "Lufth#ns#
      replace( carrname, 'a', '#' ) AS replace,

      "Replaces a found PCRE expression;
      "more parameters possible: occurrence, case_sensitive, start
      "http://www#ufthansa#om
      replace_regexpr( pcre = '\..',        "Period that is followed by any character
                        value = url,
                        with = '#' ) AS replace_regex,

      "Extracts a string with the length specified starting from the right
      "hansa
      right( carrname, 5 ) AS right,

      "Expands string to length n (2nd argument); trailing blanks produced
      "are replaced by the characters from the (3rd) argument
      "Note that if n is less than the string, the expression is truncated
      "on the right.
      "Lufthansa###
      rpad( carrname, 12, '#' ) AS rpad,

      "All trailing characters that match the character of the 2nd argument
      "are removed; trailing blanks are removed, too
      "Lufthans
      rtrim( carrname, 'a' ) AS rtrim,

      "Returns a substring; 2nd argument = position from where to start;

      "fth
      substring( carrname, 3, 3 ) AS substring,

      "Searches for a PCRE expression and returns the matched substring
      "More parameters possible: occurrence, case_sensitive, start, group
      ".lu
      substring_regexpr( pcre = '\...', "Period that is followed by any two characters
                          value = url ) AS substring_regexpr,

      "All lower case letters are transformed to upper case letters
      "LUFTHANSA
      upper( carrname ) AS upper

      FROM zdemo_abap_carr
      WHERE carrid = 'LH'
      INTO @DATA(string_functions).

    out->write( data = string_functions name = `string_functions` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Functions for Date, Time, and Time Stamps
*&---------------------------------------------------------------------*

    DATA da TYPE d VALUE '20240122'.
    DATA ti TYPE t VALUE '123456'.
    DATA utc TYPE utclong VALUE '2024-02-15 05:30:00'.
    DATA tmst TYPE timestamp VALUE '20240808112458'.
    DATA tmstlong TYPE timestampl VALUE '20240101081317.81011'.

    SELECT SINGLE FROM i_timezone
    FIELDS
      "---------------------- Date ----------------------
      "Generic date functions (types d, utclong)
      "type t also possible; 1
      is_valid( @ti ) AS isvalid,
      "In the following examples in this 'section', d and utclong are possible.

      extract_year( @utc ) AS extr_year,


      extract_month( @da ) AS extr_month,


      extract_day( @utc ) AS extr_day,

      "Monday
      dayname( @da ) AS day_name,

      "February
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
      "As above, types d and utclong also possible; 1
      is_valid( @ti ) AS time_is_valid,


      extract_hour( @utc ) AS extr_hour,


      extract_minute( @ti ) AS extr_min,


      extract_second( @utc ) AS extr_sec,

      "Function for the type tims

      tims_is_valid( tims`231256` ) AS tims_is_valid,

      "---------------------- Time Stamp ----------------------
      "Note: The type utclong can be used in the generic functions above.
      "Functions specific to the type utclong
      "Generates a UTC time stamp; e.g. 2024-01-01 12:58:58.5070000
      utcl_current( ) AS utcl_current,


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

      "X
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
    INTO @DATA(time_and_date_functions).

    out->write( data = time_and_date_functions name = `time_and_date_functions` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& More (Special) Functions
*&---------------------------------------------------------------------*

    SELECT SINGLE
      carrid,

      "Type conversion: string of fixed length (e.g. of type c) to variable
      "length string of type string
      to_clob( carrid ) AS clob,

      "Byte string -> character string
      bintohex( raw`1234` ) AS bintohex,

      "Character string -> byte string
      hextobin( char`1234` ) AS hextobin,

      "Byte field of type RAW to a byte string (BLOB) of type RAWSTRING
      to_blob( raw`1234` ) AS blob,

      "Unit and currency conversion functions
      "More parameters are available.

      "Converts miles to kilometers
      unit_conversion( quantity = d34n`1`,
                       source_unit = unit`MI`,
                       target_unit = unit`KM` ) AS miles_to_km,

      "Creating a unique UUID for each row
      uuid( ) AS uuid

      FROM zdemo_abap_carr
      WHERE carrid = char`LH`
      INTO @DATA(special_functions).

    out->write( data = special_functions name = `special_functions` ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& coalesce Function
*&---------------------------------------------------------------------*

    "The null value is a special value that is returned by a database. It indicates an
    "undefined value or result. Note that, in ABAP, there are no special null values. Do
    "not confuse the null value with a type-dependent initial value. When using SELECT
    "statements to read data, null values can be produced by, for example, outer joins.
    "When the null values are passed to a data object, they are transformed to the
    "type-dependent initial values. For more information, refer to the ABAP Keyword Documentation.
    "The following example uses a left outer join to intentionally create null values. For
    "this purpose, two demo database tables of the ABAP cheat sheet repository are cleared and
    "populated with specific values to visualize null values.
    DELETE FROM zdemo_abap_tab1.
    DELETE FROM zdemo_abap_tab2.
    MODIFY zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 char1 = 'a' char2 = 'y' )
                                                  ( key_field = 2 char1 = 'b' char2 = 'z' ) ) ).
    MODIFY zdemo_abap_tab2 FROM TABLE @( VALUE #( ( key_field = 1 char1 = 'a' )
                                                  ( key_field = 2 char1 = 'a' )
                                                  ( key_field = 3 char1 = 'b' )
                                                  ( key_field = 4 ) ) ).

    "Note that for the entry 'key_field = 4' no char1 value was passed.
    "char1 is a shared column of the two database tables, and which is used in
    "the ON condition of the join. Since there is no entry in char1 for 'key_field = 4',
    "the joined values are null in that case.
    "The coalesce function is used to replace null values produced by an outer join with
    "a different value.
    SELECT tab2~key_field,
           coalesce( tab1~char1, '-' ) AS coalesced1,
           coalesce( tab1~char2, '#' ) AS coalesced2
        FROM zdemo_abap_tab2 AS tab2
        LEFT OUTER JOIN zdemo_abap_tab1 AS tab1 ON tab1~char1 = tab2~char1
        INTO TABLE @DATA(join_w_null).

    out->write( data = join_w_null name = `join_w_null` ).
    out->write( |\n| ).


  ENDMETHOD.

  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.

ENDCLASS.
