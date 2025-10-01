<a name="top"></a>

# Built-In Functions

- [Built-In Functions](#built-in-functions)
  - [About Built-In Functions](#about-built-in-functions)
  - [Logical Functions](#logical-functions)
  - [Numeric Functions](#numeric-functions)
  - [String Functions](#string-functions)
  - [Time Stamp Functions](#time-stamp-functions)
  - [Table Functions](#table-functions)
  - [Built-In Functions for ABAP CDS and ABAP SQL](#built-in-functions-for-abap-cds-and-abap-sql)
    - [Functions for Numeric Values](#functions-for-numeric-values)
    - [Functions for Strings](#functions-for-strings)
    - [Functions for Date, Time, and Time Stamps](#functions-for-date-time-and-time-stamps)
    - [More (Special) Functions](#more-special-functions)
    - [coalesce Function](#coalesce-function)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)


This ABAP cheat sheet includes a variety of built-in functions in ABAP, along with code snippets to demonstrate their functionality. Many of the functions covered here are also included in other ABAP cheat sheets that focus on specific topics.

## About Built-In Functions
ABAP offers a range of predefined built-in functions for different purposes. These include numeric functions for calculating numeric values, string functions for processing strings, table functions for working with internal tables, and more. 

The functions can have one argument, which is a data object or an expression whose content is passed, or they can have multiple arguments, some of which may be optional in certain cases. Each function has a specific return value and can be specified in different positions. 

Built-in functions are also available in ABAP SQL and ABAP CDS.

> [!NOTE]
> - For more detailed information, refer to the topics linked in the [More Information](#more-information) section.
> - Avoid naming your methods the same as built-in functions within classes. Otherwise, the methods will "[hide](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuilt_in_functions_hiding.htm)" the built-in functions.
> - The examples in the ABAP cheat sheet are not comprehensive in terms of functions covered, syntax options and parameters used. Always refer to the ABAP Keyword Documentation for more details.
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)

## Logical Functions 

> [!NOTE]
> - Logical functions in ABAP return a [truth value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentruth_value_glosry.htm), either true or false. They are primarily used in [logical expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp.htm), for example, in control statements like `IF ... ELSE ... ENDIF`, and other statements that involve conditions. 
> - Note that ABAP does not have a Boolean data type for truth values, nor does it support [Boolean data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenboolean_data_object_glosry.htm). Instead, the `xsdbool` function can be used to represent truth values in various situations where the `abap_bool` type from the `abap` type pool, i.e. the values `abap_true` ('X') and `abap_false` (''), is expected. 
> - Many of the examples in this section utilize the `xsdbool` function to visualize the truth value, rather than using `IF` control structures, for example.
> - For more information, see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogic_functions.htm).


<table>
<tr>
<td> Function </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>boolc</code> </td>
<td>
Boolean function that returns a truth value. In this case, it is a single-character value of type <code>string</code>. When true, it returns the string <code>X</code>. When false, it returns a blank. The result is not to be compared with <code>abap_true</code> and <code>abap_false</code> (because of <code>c</code> to <code>string</code> conversion). To get the technical type <code>c</code> (with the values 'X' and ''), you can use the <code>xsdbool</code> function.
<br><br>

``` abap
"boolc returns an X or a blank of type string
DATA(int) = 0.
"X
DATA(boolc1) = CONV abap_bool( boolc( int IS INITIAL ) ). 

"#X#
DATA(boolc2) = |#{ boolc( int IS INITIAL ) }#|. 

"# #
DATA(boolc3) = |#{ boolc( int IS NOT INITIAL ) }#|. 

"Using the translate function to return a value other than X/blank
"1
DATA(boolc4) = translate( val = boolc( int BETWEEN -3 AND 3 ) from = `X` to = `1` ). 

"0
DATA(boolc5) = translate( val = boolc( int <> 0 ) from = ` ` to = `0` ). 
``` 

</td>
</tr>

<tr>
<td> <code>xsdbool</code> </td>
<td>
Boolean function that returns a truth value. Similar to <code>boolc</code>, it returns the value <code>X</code> for true, and a blank for false. Unlike <code>boolc</code>, the return value is of type <code>c</code> of length 1, and can be compared with <code>abap_true</code> and <code>abap_false</code>. 
<br><br>

``` abap
"abap_true
DATA(xsdb1) = xsdbool( 3 > 1 ). 

"#X#
DATA(xsdb2) = |#{ xsdbool( 1 = 1 ) }#|. 

"##
DATA(xsdb3) = |#{ xsdbool( 1 <> 1 ) }#|. 

"Comparison with boolc
"not equal
IF boolc( 1 = 0 ) = xsdbool( 1 = 0 ).
  DATA(res) = `equal`.
ELSE.
  res = `not equal`.
ENDIF.

"Using xsdbool instead of, for example, an IF control
"structure or an expression with the COND operator
"abap_true
DATA(xsdb4) = xsdbool( -1 < 1 ). 

DATA truth_value1 TYPE abap_bool.
IF -1 < 1.
  truth_value1 = abap_true.
ELSE.
  truth_value1 = abap_false.
ENDIF.

DATA(truth_value2) = COND #( WHEN -1 < 1 THEN abap_true ELSE abap_false ).
``` 

</td>
</tr>
<tr>
<td> <code>contains</code><br><code>contains_any_of</code><br><code>contains_any_not_of</code> </td>
<td>
 <ul>
 <li><code>contains</code>
 <ul>
 <li>For checking a text string based on (optional) parameters</li>
 <li><code>val</code>: Text string to be searched</li>
 <li><code>sub</code>/<code>start</code>/<code>end</code>: Specifying a substring to be searched in <code>val</code></li>
 <li><code>off</code>/<code>len</code>: Specifying the search range</li>
  <li><code>case</code>: Specifying the case-sensitivity (the search is case-sensistive by default)</li>
 <li><code>pcre</code>: Specifying a regular expression</li>
  </ul>
 </li>
 <li><code>contains_any_of</code>/<code>contains_any_not_of</code>
  <ul>
  <li>The functions only check individual characters passed to <code>sub</code>/<code>start</code>/<code>end</code> (or, in case of the negation, any characters that are not contained).</li>
 <li>The <code>pcre</code> parameter is not available.</li>  
  </ul>
 </li> 
</ul>
 </ul>

<br>

``` abap
"-------------------- contains --------------------
"Specifying the minimum mandatory parameters
"Unlike most of the following examples, this one uses an IF control structure to 
"visualize the truth value.
DATA cont1 TYPE abap_bool.
"abap_true
IF contains( val = `abdefghijklmn` sub = `ghi` ).
  cont1 = abap_true.
ELSE.
  cont1 = abap_false.
ENDIF.

"case (abap_true is the default)
"abap_false
DATA(cont2) = xsdbool( contains( val = `ABCDE` start = `ab` case = abap_true ) ). 

"abap_true
DATA(cont3) = xsdbool( contains( val = `ABCDE` start = `ab` case = abap_false ) ). 

"end
"abap_true
DATA(cont4) = xsdbool( contains( val = `UVWXYZ` end = `xyz` case = abap_false ) ). 

"start
"abap_false
DATA(cont5) = xsdbool( contains( val = `123` start = `2` ) ). 

"off/len can also be specified individually
"Not specifying off means 0 by default
"abap_false
DATA(cont6) = xsdbool( contains( val = `##ab## ##cd##` sub = `cd` len = 5 ) ).

"abap_true
DATA(cont7) = xsdbool( contains( val = `##ab## ##cd##` sub = `cd` off = 7 len = 5 ) ). 

"occ: False if there are more occurrences than specified for occ; i.e. in the following
"example, specifying the values 1, 2, 3 returns true
"abap_true is returned for the first 3 loop passes, abap_false for the fourth
DO 4 TIMES.
  DATA(cont8) = xsdbool( contains( val = `ab#ab#ab#cd#ef#gh` sub = `ab` occ = sy-index ) ).
ENDDO.

"pcre
"In the example, a blank is searched.
"abap_true
DATA(cont9) = xsdbool( contains( val = `Hallo world` pcre = `\s` ) ). 

"-------------------- contains_any_of --------------------
"abap_true
DATA(cont10) = xsdbool( contains_any_of( val = `abcdefg` sub = `xyza` ) ).

"abap_false
DATA(cont11) = xsdbool( contains_any_of( val = `abcdefg` sub = `xyz` ) ). 

DATA(hi) = `1hallo`.
DATA(abc) = `abcdefghijklmnopqrstuvwxyz`.
"abap_false
DATA(cont12) = xsdbool( contains_any_of( val = hi start = abc ) ).

"abap_true
DATA(cont13) = xsdbool( contains_any_of( val = hi end = abc ) ). 

"-------------------- contains_any_not_of --------------------
"abap_true
DATA(cont14) = xsdbool( contains_any_not_of( val = hi start = abc ) ).

"abap_false
DATA(cont15) = xsdbool( contains_any_not_of( val = hi end = abc ) ).
``` 

</td>
</tr>
<tr>
<td> <code>matches</code> </td>
<td>
Comparing a search range of a value with a regular expression. More optional parameters are available (e.g. <code>case</code>, <code>off</code>, <code>len</code>).
<br><br>

``` abap
"Checking validity of an email address
"abap_true
DATA(matches) = xsdbool( matches( val  = `jon.doe@email.com`
                                  pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ) ). 
```

</td>
</tr>

<tr>
<td> <code>line_exists</code> </td>
<td>
Checking whether a line exists in an internal table. A table expression must be specified.
<br><br>

``` abap
TYPES: BEGIN OF s,
          comp1 TYPE i,
          comp2 TYPE c LENGTH 3,
        END OF s.
DATA itab TYPE TABLE OF s WITH EMPTY KEY.
itab = VALUE #( ( comp1 = 1 comp2 = 'aaa' ) ( comp1 = 2 comp2 = 'bbb' ) ( comp1 = 3 comp2 = 'ccc' ) ).
DATA(str_tab) = VALUE string_table( ( `abc` ) ( `def` ) ( `ghi` ) ).

"abap_true
DATA(line_exists1) = xsdbool( line_exists( itab[ 1 ] ) ). 

"abap_false
DATA(line_exists2) = xsdbool( line_exists( itab[ 4 ] ) ). 

"abap_true
DATA(line_exists3) = xsdbool( line_exists( itab[ comp1 = 2 ] ) ). 

"abap_true
DATA(line_exists4) = xsdbool( line_exists( str_tab[ 2 ] ) ). 

"abap_false
DATA(line_exists5) = xsdbool( line_exists( str_tab[ table_line = `xxx` ] ) ).
```

</td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Numeric Functions 

<table>
<tr>
<td> Function </td> <td> Details/Code Snippet </td>
</tr>

<tr>
<td> <code>abs</code><br><code>sign</code><br><code>ceil</code><br><code>floor</code><br><code>trunc</code><br><code>frac</code><br><code>ipow</code> </td>
<td>

``` abap
"----------- abs: Returning the absolute value ----------- 
"4.756
DATA(abs1) = abs( CONV decfloat34( '-4.756' ) ). 

"4
DATA(abs2) = abs( -4 ). 

"----------- sign: Evaluating the sign ----------- 
"-1 if negative, 0 if 0, 1 if positive
"-1
DATA(sign1) = sign( -789 ).

"0
DATA(sign2) = sign( 5 - 5 ). 

"1
DATA(sign3) = sign( -5 * -5 ).

"----- ceil: smallest integer not less than the value specified -----
"5
DATA(ceil1) = ceil( CONV decfloat34( '4.999' ) ).

"5
DATA(ceil2) = ceil( CONV decfloat34( '4.001' ) ).

"-4
DATA(ceil3) = ceil( CONV decfloat34( '-4.999' ) ).

"-4
DATA(ceil4) = ceil( CONV decfloat34( '-4.001' ) ). 

"----- floor: largest integer not less than the value specified -----
"4
DATA(floor1) = floor( CONV decfloat34( '4.999' ) ). 

"4
DATA(floor2) = floor( CONV decfloat34( '4.001' ) ).

"-5
DATA(floor3) = floor( CONV decfloat34( '-4.999' ) ).

"-5
DATA(floor4) = floor( CONV decfloat34( '-4.001' ) ). 

"------------- trunc: integer part -------------
"4
DATA(trunc1) = trunc( CONV decfloat34( '4.999' ) ).

"4
DATA(trunc2) = trunc( CONV decfloat34( '4.001' ) ). 

"-4
DATA(trunc3) = trunc( CONV decfloat34( '-4.999' ) ). 

"-4
DATA(trunc4) = trunc( CONV decfloat34( '-4.001' ) ). 

"------------- frac: decimal places -------------
"0.999
DATA(frac1) = frac( CONV decfloat34( '4.999' ) ). 

"0.001
DATA(frac2) = frac( CONV decfloat34( '4.001' ) ). 

"-0.999
DATA(frac3) = frac( CONV decfloat34( '-4.999' ) ). 

"-0.001
DATA(frac4) = frac( CONV decfloat34( '-4.001' ) ). 

"------------- ipow: Calculalting the power -------------
"8
DATA(ipow1) = ipow( base = 2 exp = 3 ). 

"1
DATA(ipow2) = ipow( base = 10 exp = 0 ). 

"Exception is raised
TRY.
    DATA(ipow3) = ipow( base = 10 exp = 100 ).
  CATCH cx_sy_arithmetic_overflow. 
ENDTRY.
```

</td>
</tr>

<tr>
<td> <code>nmin</code><br><code>nmax</code> </td>
<td>
Numeric extremum functions that return the value of the largest or smallest of the passed arguments.
<br><br>

``` abap
"A minimum of two, and a maximum of 9 arguments can be specified.
"Numeric data objects and numeric expressions are possible
"0.999
DATA(nmin) =  nmin( val1 = CONV decfloat34( '1.34' )
                     val2 = CONV decfloat34( '56.7' )
                     val3 = CONV decfloat34( '890.123' )
                     val4 = CONV decfloat34( '0.999' ) ). 

"890.123
DATA(nmax) =  nmax( val1 = CONV decfloat34( '1.34' )
                     val2 = CONV decfloat34( '56.7' )
                     val3 = CONV decfloat34( '890.123' )
                     val4 = CONV decfloat34( '0.999' ) ). 
```

</td>
</tr>

<tr>
<td> <code>acos</code><br><code>asin</code><br><code>atan</code><br><code>cos</code><br><code>sin</code><br><code>tan</code><br><code>cosh</code><br><code>sinh</code><br><code>tanh</code><br><code>exp</code><br><code>log</code><br><code>log10</code><br><code>sqrt</code> </td>
<td>
Regarding the details of the result and type conversion of floating point functions, refer to the ABAP Keyword Documentation. The following snippet shows a small selection.
<br><br>

``` abap
"Calculating the square root
"3
DATA(sqrt1) = sqrt( CONV decfloat34( '9' ) ). 

"6.4
DATA(sqrt2) = sqrt( CONV decfloat34( '40.96' ) ). 

"Calculating the logarithm to base 10
"3
DATA(log10) = log10( CONV decfloat34( '1000' ) ). 

"-0.988031...
DATA(sine) = sin( '30' ). 

"0.525321...
DATA(cosine) = cos( '45' ). 

"-1.9952...
DATA(tangent) = tan( '90' ). 
```

</td>
</tr>

<tr>
<td> <code>round</code><br><code>rescale</code> </td>
<td>
Rounding functions expect a decimal floating point number as argument. The return value is of type <code>decfloat34</code>. The functions can be used to round to decimal places and <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprecision_glosry.htm">precisions</a>. In addition, rounding rules can be specified. For more details, refer to the ABAP Keyword Documentation.
<br><br>

``` abap
"Rounding to decimal places
"1.24
DATA(round1) = round( val = CONV decfloat34( '1.2374' ) dec = 2 ). 

"1.237
DATA(round2) = round( val = CONV decfloat34( '1.2374' ) dec = 3 ). 

"Rounding to precision
"1.234567890E+12
DATA(round3) = round( val = CONV decfloat34( '1234567890123' ) prec = 10 ). 

"1.23E+3
DATA(round4) = round( val = CONV decfloat34( '1234' ) prec = 3 ). 

"Rescaling function
"Similar to the round function, the dec (for scaling) or prec (for precision)
"parameters must be specified. The input is rounded if required.
"1235
DATA(rescale1) = rescale( val = CONV decfloat34( '1234.56789' ) dec = 0 ). 

"1234.6
DATA(rescale2) = rescale( val = CONV decfloat34( '1234.56789' ) dec = 1 ). 

"1.23E+3
DATA(rescale3) = rescale( val = CONV decfloat34( '1234.56789' ) prec = 3 ). 

"1234.567890
DATA(rescale4) = rescale( val = CONV decfloat34( '1234.56789' ) prec = 10 ). 
```

</td>
</tr>

<tr>
<td> <code>factorial</code><br><code>binomial</code> </td>
<td>

<code>factorial</code> calculates the factorial of positive integers. <code>binomial</code> computes the binomial coefficient. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENFACTORIAL_BINOMIAL_FUNCTIONS.html).

<br>

``` abap
TYPES: BEGIN OF demo_fact_struc,
         n             TYPE i,
         res        TYPE string,
         res_simple TYPE string,
       END OF demo_fact_struc.
DATA factorial_tab TYPE TABLE OF demo_fact_struc WITH EMPTY KEY.
DATA count TYPE i.
DO 26 TIMES.
  DATA(result) = factorial( count ).
  APPEND VALUE #( n           = count
                  res        = result
                  res_simple = |{ result STYLE = SIMPLE }| ) TO factorial_tab.
  count += 1.
ENDDO.


TYPES: BEGIN OF demo_bin_struc,
         n             TYPE i,
         k             TYPE i,
         res        TYPE string,
         res_simple TYPE string,
       END OF demo_bin_struc.
DATA bin_tab TYPE TABLE OF demo_bin_struc WITH EMPTY KEY.
DATA n_val TYPE i VALUE 21.
DATA k_val TYPE i VALUE 11.
DO 21 TIMES.
  n_val -= 1.
  k_val -= 1.
  DATA(bin_coeff) = binomial( n = n_val k = k_val ).
  APPEND VALUE #( n          = n_val
                  k          = k_val
                  res        = bin_coeff
                  res_simple = |{ bin_coeff STYLE = SIMPLE }| ) TO bin_tab.
ENDDO.
```

</td>
</tr>


</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## String Functions 

<table>
<tr>
<td> Function </td> <td> Details/Code Snippet </td>
</tr>

<tr>
<td> <code>numofchar</code><br><code>strlen</code><br><code>xstrlen</code> </td>
<td>
For determining the length of a string, i.e. the number of characters contained in a string.
<br><br>

``` abap
"numofchar: Trailing blanks are not counted in both strings of fixed and variable length
"strlen: Trailing blanks are not counted in strings of fixed length; in strings of
"        variable length, they are counted
"3
DATA(numofchar1)   = numofchar( 'abc   ' ). 

"3
DATA(numofchar2)   = numofchar( `abc   ` ). 

"3
DATA(strlen1) = strlen( 'abc   ' ). 

"6
DATA(strlen2) = strlen( `abc   ` ). 

"xstrlen for type xstring
DATA(xstr) = CONV xstring( `480065006C006C006F00200077006F0072006C0064002100` ).
"24
DATA(len_xstr) = xstrlen( xstr ). 
"xstring -> string
"Hello world!
DATA(conv_str) = cl_abap_conv_codepage=>create_in( )->convert( xstr ). 
```

</td>
</tr>

<tr>
<td> <code>cmin</code><br><code>cmax</code> </td>
<td>
Character-like extremum value functions return a string that contains the content of the smallest or biggest of a set of character-like arguments.
<br><br>

``` abap
DATA(cmin) =  cmin( val1 = `zzzzzzz`
                    val2 = `zzazzzzzzzz` "smallest argument
                    val3 = `zzzzabc` ).

DATA(cmax) =  cmax( val1 = `abcdef`      "biggest argument
                    val2 = `aaghij`
                    val3 = `aaaaklmn`
                    val4 = `aaaaaaopqrs`
                    val5 = `aaaaaaaaaatuvwxy`
                    val6 = `aaaaaaaaaaaaaz` ).
```

</td>
</tr>

<tr>
<td> <code>find</code><br><code>find_end</code><br><code>find_any_of</code><br><code>find_any_not_of</code> </td>
<td>
Search functions
<br><br>

``` abap
DATA(str) = `Pieces of cakes.`.

"---------------- find ----------------
"The find function searches for the substring specified and returns the offset
"7
DATA(find1) = find( val = str sub = `of` ). 

"-1 (no finding)
DATA(find2) = find( val = str sub = `x` ). 

"case
"0
DATA(find3) = find( val = str sub = `p` case = abap_false ). 

"off/len
"-1 (no finding)
DATA(find4) = find( val = str sub = `ca` off = 4 len = 5 ). 
"10
DATA(find5) = find( val = str sub = `ca` off = 4 len = 10 ). 

"occ
"4
DATA(find6) = find( val = str sub = `es` occ = 1 ). 

"13
DATA(find7) = find( val = str sub = `es` occ = 2 ). 

"-1 (no third occurrence)
DATA(find8) = find( val = str sub = `es` occ = 3 ). 

"pcre
"15
DATA(find9) = find( val = str pcre = `\.` ). 

"---------------- find_end ----------------
"find_end returns the sum of the offset of the occurrence plus the length of the match
"9 (7 + 2)
DATA(find_end1) = find_end( val = str sub = `of` ). 

"7 (6 + 1)
DATA(find_end2) = find_end( val = str pcre = `\s` ). 

"---------------- find_any_of ----------------
"find_any_of returns the offset of the occurrence of any character contained 
"in a substring. The search is always case-sensitive.
"2 (character e is found)
DATA(find_any_of1) = find_any_of( val = str sub = `x523z4e` ).

"-1 
DATA(find_any_of2) = find_any_of( val = str sub = `zwq85t` ). 

"---------------- find_any_not_of ----------------
"find_any_not_of is the negation of find_any_of
"0 (very first character in the searched string)
DATA(find_any_not_of1) = find_any_not_of( val = str sub = `ieces` ). 

"1
DATA(find_any_not_of2) = find_any_not_of( val = str sub = `P` ). 
```

</td>
</tr>

<tr>
<td> <code>count</code><br><code>count_any_of</code><br><code>count_any_not_of</code> </td>
<td>
Returning the number of all occurrences in a string
<br><br>

``` abap
DATA(st) = `Pieces of cakes.`.

"---------------- count ----------------
"3
DATA(count1) = count( val = st sub = `e` ). 

"0
DATA(count2)  = count( val = st sub = `x` ). 

"case (case-sensitive by default)
"1
DATA(count3)  = count( val = st sub = `p` case = abap_false ). 

"off/len (off is 0 by default; len is the length of sting by default minus offset)
"2
DATA(count4)  = count( val = st sub = `es` off = 3 ). 

"1
DATA(count5)  = count( val = st sub = `es` off = 9 ). 

"2
DATA(count6)  = count( val = st sub = `es` off = 3 len = 12 ). 

"0
DATA(count7)  = count( val = st sub = `es` len = 5 ). 

"pcre
"2
DATA(count8)  = count( val = st pcre = `\s` ).

"16
DATA(count9)  = count( val = st pcre = `.` ). 

"---------------- count_any_of ----------------
"3 (e characters)
DATA(count_any_of1) = count_any_of( val = st sub = `x523z4e` ). 

"6 (3 e, 2 c, 1 o)
DATA(count_any_of2) = count_any_of( val = st sub = `eco` ). 

"---------------- count_any_not_of ----------------
"14
DATA(count_any_not_of1) = count_any_not_of( val = st sub = `fP` ). 

"0 (all are included)
DATA(count_any_not_of2) = count_any_not_of( val = st sub = `Piecs ofak.` ). 
```

</td>
</tr>

<tr>
<td> <code>distance</code> </td>
<td>
Returning the Levenshtein distance between two strings, which reflects their similarity
<br><br>

``` abap
DATA(str_to_check) = `abap`.
"0
DATA(dist1) = distance( val1 = str_to_check val2 = `abap` ). 

"1
DATA(dist2) = distance( val1 = str_to_check val2 = `axbap` ). 

"4
DATA(dist3) = distance( val1 = str_to_check val2 = `yabyyapy` ). 

"5
DATA(dist4) = distance( val1 = str_to_check val2 = `zabapzzzzzzzzzzzz` max = 5 ). 
```

</td>
</tr>

<tr>
<td> <code>repeat</code> </td>
<td>
Repeating strings as many times as specified
<br><br>

``` abap 
"abapabapabapabapabap
DATA(repeat1) = repeat( val = `abap` occ = 5 ).  

"#          #
DATA(repeat2) = |#{ repeat( val = ` ` occ = 10 ) }#|. 

"Y (initial value returned)
DATA(repeat3) = COND #( WHEN repeat( val = `a` occ = 0 ) = `` THEN `Y` ELSE `Z` ). 
```

</td>
</tr>

<tr>
<td> <code>condense</code> </td>
<td>
Condensing strings
<br><br>

``` abap
DATA(str_to_condense) = ` ab   cd `.

"No parameters specified, i. e. their default values are provided.
"Works like CONDENSE statement without the NO-GAPS addition.
"ab cd
DATA(condense1) = condense( str_to_condense ). 

"Parameters del/to not specified. from parameter with initial string
"(could also be a text field literal: from = ' '). This way, leading and
"trailing blanks are removed.
"ab   cd
DATA(condense2) = condense( val = str_to_condense from = `` ). 

"Parameter to specified with an initial string. No other parameters.
"Works like the CONDENSE statement with the NO-GAPS addition.
"abcd
DATA(condense3) = condense( val = str_to_condense  to = `` ). 

"Parameter del specifies the leading/trailing characters to be removed.
"see###you
DATA(condense4) = condense( val = `##see###you##` del = `#` ). 

"If from and to are specified along with del, leading/trailing characters
"specified in del are first removed. Then, in the remaining string, all 
"substrings composed of characters specified in from are replaced with the 
"first character of the string specified in the to parameter.
"Rock'n'Roll
DATA(condense5) = condense( val  = `  Rock'xxx'Roller`
                            del  = `re `
                            from = `x`
                            to   = `n` ). 
```

</td>
</tr>

<tr>
<td> <code>concat_lines_of</code> </td>
<td>
Concatenating internal tables into strings
<br><br>

``` abap
DATA(stringtable) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).
"abc
DATA(con1) = concat_lines_of( table = stringtable ). 

"a b c
DATA(con2) = concat_lines_of( table = stringtable sep = ` ` ). 

"a/b/c
DATA(con3) = concat_lines_of( table = stringtable sep = `/` ). 
```

</td>
</tr>

<tr>
<td> <code>reverse</code> </td>
<td>
Reversing strings
<br><br>

``` abap
"abap
DATA(reverse) = reverse( `paba` ).
```

</td>
</tr>

<tr>
<td> <code>escape</code> </td>
<td>
Escaping special characters according to the specification in the <code>format</code> parameter.
Suitable values for the <code>format</code> parameter (which expects a data object of type <code>i</code>) are available in the <code>CL_ABAP_FORMAT</code> class (the constants starting with <code>E_</code>). 
<br><br>

``` abap
"Context: URLs
"...test%3A%205%408...
DATA(esc1) = escape( val    = '...test: 5@8...'
                     format = cl_abap_format=>e_url_full ).

"Context: JSON
"some \"test\" json \\ with backslash and double quotes
DATA(esc2) = escape( val    = 'some "test" json \ with backslash and double quotes'
                     format = cl_abap_format=>e_json_string ).

"Context: String templates
"Special characters in string templates: \|, \\, \{, \}
DATA(esc3) = escape( val    = 'Special characters in string templates: |, \, {, }'
                     format = cl_abap_format=>e_string_tpl ).
```

</td>
</tr>

<tr>
<td> <code>insert</code> </td>
<td>
Inserting strings
<br><br>

```abap
DATA(to_be_inserted) = `ABAP`.
"#ABAP
DATA(insert1) = insert( val = to_be_inserted sub = `#` ). 

"A#BAP
DATA(insert2) = insert( val = to_be_inserted sub = `#` off = 1 ). 

"ABAP#
DATA(insert3) = insert( val = to_be_inserted sub = `#` off = strlen( to_be_inserted ) ). 
```

</td>
</tr>

<tr>
<td> <code>match</code> </td>
<td>
Returning substrings that match regular expressions
<br><br>

``` abap
"jon.doe@email.com
DATA(match1) = match( val = `The email address is jon.doe@email.com.`
                      pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ). 

"Find blank (without inlcuding it in the result indicated by \K) and
"the following 2 characters, second occurrence
"ad
DATA(match2) = match( val = `The email address is jon.doe@email.com.`
                      pcre = `\s\K..` 
                      occ = 2 ). 
```

</td>
</tr>

<tr>
<td> <code>replace</code> </td>
<td>
Replacements in strings
<br><br>

``` abap
DATA(to_be_replaced) = `Pieces of cakes.`.

"Piec# of cakes.
DATA(replace1) = replace( val = to_be_replaced sub = `es` with = `#` ).

"case
"#ieces of cakes.
DATA(replace2) = replace( val = to_be_replaced sub = `p` case = abap_false with = `#` ).

"occ
"Pieces of#cakes.
DATA(replace3) = replace( val = to_be_replaced sub = ` ` occ = 2 with = `#` ).

"The value 0 in occ means respecting all occurrences.
"Pi#c#s of cak#s.
DATA(replace4) = replace( val = to_be_replaced sub = `e` occ = 0 with = `#` ).

"pcre
"Pieces#of cakes.
DATA(replace5) = replace( val = to_be_replaced pcre = `\s` with = `#` ).

"Pieces of#cakes.
DATA(replace6) = replace( val = to_be_replaced pcre = `\s` occ = 2 with = `#` ).

"Replacement determined by offset/length specification only (no sub/pcre specification)
"Piece#s of cakes.
DATA(replace7) = replace( val = to_be_replaced off = 5 with = `#` ).

"#s of cakes.
DATA(replace8) = replace( val = to_be_replaced len = 5 with = `#` ).

"Pie#cakes.
DATA(replace9) = replace( val = to_be_replaced off = 3 len = 7 with = `#` ).
```

</td>
</tr>

<tr>
<td> <code>segment</code> </td>
<td>
Returning the occurrence of a segment that is defined by limits, which  are not part of the segments. 
<br><br>

``` abap
"index: Number of segment
"sep: Substring specified is searched and used as limit
"Hallo
DATA(segment1) = segment( val = `Hallo,world,123` index = 1 sep = `,` ). 

"123
DATA(segment2) = segment( val = `Hallo,world,123` index = -1 sep = `,` ). 

"world
DATA(segment3) = segment( val = `Hallo<br>world<br>123` index = 2 sep = `<br>` ). 

"space: Each individual character is searched and used as limit
DATA(to_be_segmented) = `a/b#c d.e`.

"b
DATA(segment4) = segment( val = `a/b#c d.e` index = 2 space = `. #/` ). 

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

*Content of segment_tab
*a           
*b           
*c           
*d           
*e       
```

</td>
</tr>

<tr>
<td> <code>shift_left</code><br><code>shift_right</code> </td>
<td>
Shifting content
<br><br>

``` abap
DATA(to_be_shifted) = ` hallo `.

"------------------- shift_left -------------------
"'llo '
DATA(shift_left1) = shift_left( val = to_be_shifted places = 3 ). 

"circular parameter: characters that are moved out of the string are
"added at the other end again
"'allo  h'
DATA(shift_left2) = shift_left( val = to_be_shifted circular = 2 ). 

"'lo '
DATA(shift_left3) = shift_left( val = to_be_shifted sub = ` hal` ). 

"No parameter except val: Behaves as if sub was passed a blank character
"'hallo ' (works as the following example)
DATA(shift_left4) = shift_left( val = to_be_shifted ). 

"'hallo '
DATA(shift_left5) = shift_left( val = to_be_shifted sub = ` ` ). 

"------------------- shift_right -------------------
"' hal'
DATA(shift_right1) = shift_right( val = to_be_shifted places = 3 ). 

"'o  hall'
DATA(shift_right2) = shift_right( val = to_be_shifted circular = 2 ). 

"' hall'
DATA(shift_right3) = shift_right( val = to_be_shifted sub = `o ` ). 

"' hallo'
DATA(shift_right4) = shift_right( val = to_be_shifted ). 
```

</td>
</tr>

<tr>
<td> <code>substring</code><br><code>substring_after</code><br><code>substring_before</code><br><code>substring_to</code><br><code>substring_from</code> </td>
<td>
Processing substrings
<br><br>

``` abap
DATA(s4func) = `Lorem ipsum dolor sit amet`.

"------------------- substring -------------------
"Extracting substring starting at a specific position
"'len' not specified means the rest of the remaining characters is
"respected
"ipsum dolor sit amet
DATA(substr1) = substring( val = s4func off = 6 ). 

"Extracting substring with a specific length
"'off' is not specified and has the default value 0.
"Lorem
DATA(substr2) = substring( val = s4func len = 5 ). 

"Specifying both off and len parameters
"ipsum
DATA(substr3) = substring( val = s4func off = 6 len = 5 ). 

"------------------- substring_after -------------------
"Extracting a substring ...
"... after a specified substring
"em ipsum dolor sit amet (only the first occurrence is respected)
DATA(substr_after1) = substring_after( val = s4func sub = `or` ). 

"... after a specified substring specifying the occurence in a string
"and restricting the length
"occ/case 
"' sit am'
DATA(substr_after2) = substring_after( val = s4func sub = `oR` occ = 2 len = 7 case = abap_false ). 

"pcre
"olor sit amet
DATA(substr_after3) = substring_after( val = s4func pcre = `\s.` occ = 2 ). 

"------------------- substring_before -------------------
"... before a specified substring
"Lorem ips
DATA(substr_before) = substring_before( val = s4func sub = `um`  ). 

"------------------- substring_from -------------------
"... from a specified substring on. It includes the substring specified
"in sub. len/off and other parameters are possible.
"um dolor sit amet
DATA(substr_from) = substring_from( val = s4func sub = `um` ). 

"Compared to substring_after
"' dolor sit amet'
DATA(substr_after4) = substring_after( val = s4func sub = `um` ). 

"------------------- substring_to -------------------
"... up to a specified substring. It includes the substring specified
"in sub. len/off and other parameters are possible.
"Lorem ipsum
DATA(substr_to) = substring_to( val = s4func sub = `um` ). 
```

</td>
</tr>

<tr>
<td> <code>to_upper</code><br><code>to_lower</code><br><code>from_mixed</code><br><code>to_mixed</code> </td>
<td>
Transforming strings
<br><br>

``` abap
"------------------- to_upper -------------------
"ABAP
DATA(upper1) = to_upper( `AbaP` ).
DATA(upper2) = to_upper( `abap` ).

"------------------- to_lower -------------------
"abap
DATA(lower1) = to_lower( `AbaP` ).
DATA(lower2) = to_lower( `ABAP` ).

"------------------- from_mixed -------------------
"sep: Inserts the first character specified in sep before each uppercase letter 
"from left to right, starting with the second position
"A#B#A#P
DATA(from_mixed1) = from_mixed( val = `ABAP` sep = `#` ). 

"If 'case' is not specified or if the first character in the 'case' parameter is an 
"uppercase letter, the entire string is transformed to uppercase, otherwise to 
"lowercase.
"ABA#P
DATA(from_mixed2) = from_mixed( val = `AbaP` sep = `#` ). 

"Underscore is the default separator
"ABA_P
DATA(from_mixed3) = from_mixed( val = `AbaP` ). 

"ABA#P (same as previous example)
DATA(from_mixed4) = from_mixed( val = `AbaP` sep = `#` case = 'X' ).

"aba#p
DATA(from_mixed5) = from_mixed( val = `AbaP` sep = `#` case = 'x' ). 

"min: Passing a positive number to specify a minimum number of characters 
"that must appear before an uppercase letter from the start of the string 
"or since the last insertion so that a separator is inserted. The default 
"value for 'min' is 1.
"A#BA#P
DATA(from_mixed6) = from_mixed( val = `ABaP` sep = `#` min = 1 ). 

"ABAA#AAAAA#AP
DATA(from_mixed7) = from_mixed( val = `ABaaAaaaaAP` sep = `#` min = 3 ). 

"------------------- to_mixed -------------------
"Transforming all letters in the string to lowercase letters from the second 
"position on. From left to right from the second position on, it removes 
"occurrences of the first character specified in the 'sep' parameter from the 
"string and transforms the next letter to an uppercase letter. 
"Default separator _
"AbcDeFgHi
DATA(to_mixed1) = to_mixed( val = `Abc_de_fg_hi` ).

"AbcDeFgHi
DATA(to_mixed2) = to_mixed( val = `Abc/de/fg/hi` sep = `/` ).

"Specifying the case parameter
"abcDeFgHi
DATA(to_mixed3) = to_mixed( val = `AbcXdeXfgXhi` sep = `X` case = 'x' ).

"Specifying the min operator
"AbcDeFgHi
DATA(to_mixed4) = to_mixed( val = `Abc/de/fg/hi` sep = `/` min = 2 ).

"Abc/deFghijklmnoPq
DATA(to_mixed5) = to_mixed( val = `Abc/de/fghijklmno/pq` sep = `/` min = 5 ).
```

</td>
</tr>

<tr>
<td> <code>translate</code> </td>
<td>
Replacing characters
<br><br>

``` abap
DATA(to_be_translated) = `___abc_def_____ghi_`.

"Each character that occurs in the 'from' parameter is replaced by the character 
"that occurs in the same place in the 'to' parameter as in the 'from' parameter.
"If 'to' is shorter than 'from', the surplus characters from 'from' are removed 
"from the string.
"abcdefg#?
DATA(translate1) = translate( val = to_be_translated from = `hi_` to = `#?` ). 

"###abc#def#####ghi#
DATA(translate2) = translate( val = to_be_translated from = `_`  to = `#?` ).  
```

</td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Time Stamp Functions 

<table>
<tr>
<td> Function </td> <td> Details/Code Snippet </td>
</tr>

<tr>
<td> <code>utclong_current</code> </td>
<td>
Retrieving UTC time stamps
<br>

``` abap
"The return value has the type utclong.
"e.g. 2024-01-01 15:45:46.2695940
DATA(ts1) = utclong_current( ).
```

</td>
</tr>

<tr>
<td> <code>utclong_add</code> </td>
<td>
Adding values to time stamps
<br>

``` abap
DATA(utc4calc) = CONV utclong( '2024-01-01 15:55:14.1173220' ).

"At least one parameter must be specified besides 'val'.
"Note that there are no parameters for years and months.

"Adding one hour
"e.g. 2024-01-01 16:55:14.1173220
DATA(ts2) = utclong_add( val = utc4calc
                         hours = 1 ).

"Subtracting one hour by passing a negative integer value (no 
"separate substract function is available)
"e.g. 2024-01-01 14:55:14.1173220
DATA(ts3) = utclong_add( val = utc4calc
                         hours = -1 ).

"Using all parameters
"e.g. 2024-01-02 18:09:07.2373220
DATA(ts4) = utclong_add( val = utc4calc
                         days = 1 
                         hours = 2 
                         minutes = CONV int8( '13' )
                         seconds = CONV decfloat34( '53.12' ) ).
```

</td>
</tr>

<tr>
<td> <code>utclong_diff</code> </td>
<td>
Calculating the time difference between the values of two time stamp fields
<br>

``` abap
DATA(ts5) = CONV utclong( '2024-01-01 05:30:00' ).
DATA(ts6) = CONV utclong( '2024-01-01 06:30:00' ).

"The return value has the type decfloat34. It contains the exact difference in seconds.
"3600
DATA(ts_diff1) = utclong_diff( high = ts6
                               low = ts5 ). 

"-3600
DATA(ts_diff2) = utclong_diff( high = ts5
                               low = ts6 ). 
```

</td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Table Functions 

> [!NOTE]
> See the `line_exists` function in the [Logical Functions](#logical-functions) section.

<table>
<tr>
<td> Function </td> <td> Details/Code Snippet </td>
</tr>


<tr>
<td> <code>lines</code> </td>
<td>
Returning the number of lines in internal tables
<br><br>

``` abap
DATA(strtab) = VALUE string_table( ( `aaa` ) ( `bbb` ) ( `ccc` ) ( `ddd` ) ( `eee` ) ).

"5
DATA(lines1) = lines( strtab ). 

DELETE strtab INDEX 1.
"4
DATA(lines2) = lines( strtab ). 

CLEAR strtab.
"0
DATA(lines3) = lines( strtab ). 
```

</td>
</tr>

<tr>
<td> <code>line_index</code> </td>
<td>
Returning the number of the line found using table expressions with respect to the table index used
<br><br>

``` abap
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
"1
DATA(line_index1) = line_index( itab_em[ comp1 = 1 ] ). 

"2
DATA(line_index2) = line_index( itab_em[ comp2 = 'd' ] ). 

"Note: A hashed table does not have a primary table index. The result is -1.
DATA(line_index3) = line_index( itab_sec[ KEY primary_key comp1 = 1 ] ). 

"Hashed tables can be assigned a secondary table index using a secondary 
"table key.
"4
DATA(line_index4) = line_index( itab_sec[ KEY sk comp2 = 'd' ] ). 

"1
DATA(line_index5) = line_index( itab_sec[ KEY sk comp2 = 'a' ] ). 

"Specifying the pseudo component table_line
"1
DATA(line_index6) = line_index( itab_str[ table_line = `aaa` ] ). 

"0
DATA(line_index7) = line_index( itab_str[ table_line = `zzz` ] ). 
```

</td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Built-In Functions for ABAP CDS and ABAP SQL

> [!NOTE]
> - The examples only demonstrate ABAP SQL statements. Refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_builtin_functions.htm) for the complete picture.
> - As with the previous examples, the following examples showcase a variety of available functions.
> - The examples use [typed literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentyped_literal_glosry.htm) to ensure appropriate types are used and to provide self-contained examples.

### Functions for Numeric Values

```abap
SELECT SINGLE
  "Division, result rounded to an integer
  "2
  div( 4, 2 ) AS div, 

  "Division, 3rd argument: result is rounded to the specified
  "number of decimals
  "0.33
  division( 1, 3, 2 ) AS division,

  "Result is rounded to first greater integer
  "2
  ceil( decfloat34`1.333` ) AS ceil,

  "Result is the remainder of division
  "1
  mod( 3, 2 ) AS mod,

  "Largest integer value not greater than the specified value
  "1
  floor( decfloat34`1.333` ) AS floor,

  "Returns the absolute number
  "2
  abs( int4`-2` ) AS abs,

  "Result is rounded to the specified position after the decimal separator
  "1.34
  round( decfloat34`1.337`, 2 ) AS round

  FROM zdemo_abap_carr
  WHERE carrid = 'LH'
  INTO @DATA(numeric_functions).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Functions for Strings

```abap
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
  "6
  instr( carrname,'a' ) AS instr,

  "String of length n starting from the left of an expression;
  "trailing blanks are ignored
  "Luft
  left( carrname, 4 ) AS left,

  "Number of characters in an expression, trailing blanks are ignored
  "24
  length( url ) AS length,

  "Checks if expression contains a PCRE expression;
  "case-sensitive by default (case_sensitive parameter can be specified)
  "Notes on the 1 = found, 0 = not found
  "1
  like_regexpr( pcre  = '\..',         "Period that is followed by any character
                value = url ) AS like_regex,

  "Returns position of a substring in an expression,
  "3rd parameter = specifies offset (optional)
  "4th parameter = determines the number of occurrences (optional)
  "9
  locate( carrname, 'a', 0, 2 ) AS locate,

  "Searches a PCRE pattern, returns offset of match;
  "many optional parameters: occurrence, case_sensitive, start, group
  "21
  locate_regexpr( pcre = '\..',        "Period followed by any character
                  value = url,
                  occurrence = 2 )     "2nd occurrence in the string
                  AS locate_regexpr,

  "Searches a PCRE pattern, returns offset of match + 1;
  "many optional parameters: occurrence, case_sensitive, start, group
  "2
  locate_regexpr_after( pcre = '.',     "Any character
                        value = url,
                        occurrence = 1 ) AS locate_regexpr_after,

  "Removes leading characters as specified in the 2nd argument,
  "trailing blanks are removed
  "ufthansa
  ltrim( carrname, 'L' ) AS ltrim,

  "Counts all occurrences of found PCRE patterns
  "2
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
  "3rd argument: length of the extracted substring
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
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Functions for Date, Time, and Time Stamps

```abap
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
  "2024
  extract_year( @utc ) AS extr_year, 

  "1
  extract_month( @da ) AS extr_month, 

  "15
  extract_day( @utc ) AS extr_day, 

  "Monday
  dayname( @da ) AS day_name, 

  "February
  monthname( @utc ) AS month_name, 

  "3
  weekday( @utc ) AS week_day, 

  "10
  days_between( @utc,utclong`2024-02-25 08:14:26` ) AS days_bw, 

  "20240124
  add_days( @da,2 ) AS add_days,                        

  "2024-05-15 05:30:00.0000000
  add_months( @utc,3 ) AS add_months, 
  
  "Functions for the type datn
  "32
  datn_days_between( datn`20240111`,datn`20240212` ) AS days_datn_bw, 
  
  "20240115
  datn_add_days( datn`20240111`,4 ) AS days_datn_add,   
  
  "20240611
  datn_add_months( datn`20240111`,5 ) AS months_datn_add, 

  "Functions for the type dats
  "1
  dats_is_valid( dats`20240812` ) AS dats_valid, 
  
  "5
  dats_days_between( dats`20240812`,dats`20240817` ) AS days_dats_bw, 
  
  "20240816
  dats_add_days( dats`20240812`,4 ) AS days_dats_add,   
  
  "20241112
  dats_add_months( dats`20240812`,3 ) AS months_dats_add, 

  "---------------------- Time ----------------------
  "Generic time functions (types t and utclong)
  "As above, types d and utclong also possible; 1
  is_valid( @ti ) AS time_is_valid, 
  
  "5
  extract_hour( @utc ) AS extr_hour, 
  
  "34
  extract_minute( @ti ) AS extr_min, 
  
  "0
  extract_second( @utc ) AS extr_sec, 

  "Function for the type tims
  "1
  tims_is_valid( tims`231256` ) AS tims_is_valid, 

  "---------------------- Time Stamp ----------------------
  "Note: The type utclong can be used in the generic functions above.
  "Functions specific to the type utclong
  "Generates a UTC time stamp; e.g. 2024-01-01 12:58:58.5070000
  utcl_current( ) AS utcl_current, 
  
  "2024-02-15 05:30:05.0000000
  utcl_add_seconds( @utc,5 ) AS sec_add_utc, 

  "51.0000000
  utcl_seconds_between( utclong`2024-02-25 08:14:26`,utclong`2024-02-25 08:15:17` ) AS sec_bw_utc,
  
  "Functions specific to the type timetamp
  "1
  tstmp_is_valid( @tmst ) AS ts_is_valid, 
  
  "20240312125858
  tstmp_current_utctimestamp( ) AS ts_current, 
  
  "The following two functions have an optional parameter on_error.
  "Check the ABAP Keyword Documentation
  "19
  tstmp_seconds_between( tstmp1 = @tmst,
                         tstmp2 = CAST( dec`20240808112517` AS DEC( 15,0 ) ) ) AS sec_bw_ts, 
  
  "20240808112508
  tstmp_add_seconds( tstmp    = @tmst,
                     seconds  = CAST( dec`10` AS DEC( 15,0 ) ) ) AS sec_add_ts, 

  "---------------------- Functions for conversions ----------------------
  "Note: For the following functions, optional parameters are possible.
  "For more details, check the ABAP Keyword Documentation.
  "20240808
  tstmp_to_dats( tstmp = @tmst,
                 tzone =  CAST( char`EST` AS CHAR( 6 ) ) ) AS tstmp_to_dats, 
  
  "072458
  tstmp_to_tims( tstmp = @tmst,
                 tzone = CAST( char`EST` AS CHAR( 6 ) ) ) AS tstmp_to_tims, 
  
  "X
  tstmp_to_dst( tstmp = @tmst,
                tzone = CAST( char`EST` AS CHAR( 6 ) ) ) AS tstmp_to_dst, 
  
  "20240122173456
  dats_tims_to_tstmp( date = @da,
                      time = @ti,
                      tzone = CAST( char`EST` AS CHAR( 6 ) ) ) AS dats_tims_to_tstmp, 
  
  "2024-01-01 08:13:17.8101100
  tstmpl_to_utcl( tstmpl = @tmstlong ) AS tstmpl_to_utcl, 
  
  "20240215053000.0000000
  tstmpl_from_utcl( utcl = @utc ) AS tstmpl_from_utcl, 
  
  "20240812
  dats_to_datn( dats = dats`20240812` ) AS dats_to_datn, 
  
  "20240111
  dats_from_datn( datn = datn`20240111` ) AS dats_from_datn, 
  
  "231256
  tims_to_timn( tims = tims`231256` ) AS tims_to_timn,  

  "155432
  tims_from_timn( timn = timn`155432` ) AS tims_from_timn 

WHERE TimeZoneID = char`EST`
INTO @DATA(time_and_date_functions).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### More (Special) Functions

```abap
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
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### coalesce Function

```abap
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

*Example table content
*KEY_FIELD    COALESCED1    COALESCED2
*1            a             y         
*2            a             y         
*3            b             z         
*4            -             #       
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information

- [Built-in functions in ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuilt_in_functions.htm)
  - [Overview of functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuilt_in_functions_overview.htm)
- [Built-in functions that can be used by ABAP CDS and ABAP SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_builtin_functions.htm)

## Executable Example

[zcl_demo_abap_builtin_func](./src/zcl_demo_abap_builtin_func.clas.abap) 


> [!NOTE]
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)
