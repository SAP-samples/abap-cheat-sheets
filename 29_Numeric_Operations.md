<a name="top"></a>

# Numeric Operations in ABAP

- [Numeric Operations in ABAP](#numeric-operations-in-abap)
  - [Excursion: Numeric Data Types](#excursion-numeric-data-types)
    - [Numeric Values in Character-Like Fields](#numeric-values-in-character-like-fields)
  - [Arithmetic Expressions and Operators](#arithmetic-expressions-and-operators)
    - [ABAP Calculator Example](#abap-calculator-example)
    - [Calculation Type](#calculation-type)
    - [Calculation Assignments](#calculation-assignments)
    - [Lossless Calculations](#lossless-calculations)
  - [Numeric Functions](#numeric-functions)
  - [Calculations with System Classes](#calculations-with-system-classes)
  - [Calculations with Date, Time and Time Stamp Values](#calculations-with-date-time-and-time-stamp-values)
  - [Arithmetic Expressions and Built-in Functions in ABAP SQL and ABAP CDS](#arithmetic-expressions-and-built-in-functions-in-abap-sql-and-abap-cds)
  - [Executable Example](#executable-example)


This cheat sheet explores various aspects of numeric operations and calculations in ABAP, covering miscellaneous topics and code snippets.

> [!NOTE]  
> - Several topics and similar or the same code snippets in this cheat sheet are also found in other cheat sheets. For example, date and time calculations appear in the [Date, Time, and Time Stamp](23_Date_and_Time.md) cheat sheet. They are included here in the context of calculations in ABAP.
> - Find more information in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENCOMPUTE_EXPRESSIONS.html).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursion: Numeric Data Types 

- Calculations use numeric data objects or those convertible to numeric types.
- ABAP supports specific elementary numeric data types.
- These types have distinct characteristics and value ranges, allowing the use for various purposes.
- This cheat sheet covers only [built-in ABAP types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_abap_type_glosry.htm). [Built-in DDIC types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbuiltin_ddic_type_glosry.htm) are also available for defining types in the [ABAP Dictionary (DDIC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_dictionary_glosry.htm) and ABAP CDS. These types cannot be used directly in ABAP programs except for typed literals. They are mapped to built-in ABAP types; for example, the DDIC type `INT4` is mapped to type `i`, and `DECFLOAT16` is mapped to the built-in ABAP type `decfloat16`.
- Most types' values (except type `i`) cannot be directly specified in the program and must be represented by character literals interpretable as the respective type.

<table>

<tr>
<td> Built-in ABAP type </td> <td> Notes </td>
</tr>

<tr>
<td> 

`i` and `int8` for integers

 </td>

 <td> 

- Used to store [integer numbers](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninteger_number_glosry.htm) of various value ranges.
- The type `i` can hold 4-byte integers, typically used for counters or indexes (like `sy-index` or `sy-tabix`), quantities, and more.
- `int8` holds 8-byte integers and offers a larger value range than `i`.
- You can specify integer numbers with type `i` directly in ABAP programs as [numeric literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennumeric_literal_glosry.htm).
- You might encounter the types `b` (1-byte integers) and `s` (2-byte integers) for short integers. They cannot be directly specified in ABAP programs, but you can use the DDIC types `INT1` and `INT2`. 
- If these integer types do not meet your value range needs, consider using type `p` without decimal places or floating point types for larger ranges. However, calculations will be slower then.

 </td>
</tr>

<tr>
<td> 

`p` for packed numbers

 </td>

 <td> 

- Used to store fixed point numbers with a fixed number of decimal places after a period as the decimal separator.
- The number of decimal places is specified as a property of the data type.
- Packed numbers are typically used to represent lengths, weights, or currency values.
- If the value range of packed numbers is insufficient for your needs, you can use floating point types for greater ranges.
- The value range depends on the length and number of decimal places specified with the type declaration.
  - Length: Ranges from 1 to 16 bytes, with two places packed per byte. The `+` and `-` signs are in the last byte. The packed number consists of the length multiplied by 2 minus 1 digits.
  - Decimals: Up to 14 decimal places are possible; the decimal specification should not exceed the number of digits.

 </td>
</tr>

<tr>
<td> 

`f` for binary floating point numbers

 </td>

 <td> 

- Used to store binary floating point numbers, which are binary representations of numbers with mantissas and exponents.
- Can represent decimal numbers with up to 17 decimal places in the mantissa (one before the decimal point, 16 after), and allow a three-digit exponent.
- Expect a scientific notation for the numeric value assigned, or interpretable it as such.
- Represented internally as binary fractions, so not every decimal number can be represented exactly.
- The type may be used in exceptional cases when precision is less important and fast calculations are crucial. Comparisons and calculations with this type and other numeric types might yield unexpected results due to imprecision and possible rounding effects. Consider decimal floating point types for greater precision.

 </td>
</tr>

<tr>
<td> 

`decfloat16` and `decfloat34` for decimal floating point numbers

 </td>

 <td> 

- Can hold values with a maximum precision of 16 or 34 decimal places.
- Typically used when you require a high precision in decimal places and a wide value range. The types `p` and `f` only address one of these aspects.

 </td>
</tr>

</table>

> [!NOTE]
> - The built-in type `n` for numeric text fields is a character-like, not a numeric type, even though it only contains digits. It is not recommended for calculations. Use type `n` for purposes not involving calculations, like ID values or article numbers.
> - Generic types such as `numeric`, `p` and `decfloat` can be specified as types of field symbols and formal parameters of procedures such as methods of classes. 

The following code snippet explores:
- Various options of how to create data objects with numeric types and assign values
- Generic numeric types
- Two system classes that deal with numeric values: `cl_abap_math` (mathematical utility library) and `cl_abap_random*` (computing random numeric values)

```abap
*&---------------------------------------------------------------------*
*& Integer types: i, int8
*&---------------------------------------------------------------------*

DATA i_a TYPE i.
i_a = 2.
DATA i_b TYPE i VALUE 1.
"Inline declaration of the elementary type i
"The VALUE operator only works for elementary types to create initial values.
"So, the following example basically corresponds to the declaration of i_a.
DATA(i_c) = VALUE i( ).
"Integer numbers of type i can be specified directly in the program as numeric literals
"i_d is automatically typed with type i.
DATA(i_d) = 3.
"Immutable variable
FINAL(i_e) = 4.
"Using the CONV operator to explicitly convert a value to the type specified.
"Note: If '4' is not a character literal but a numeric literal like 4 (a numeric literal
"of type i), a syntax warning will appear because the conversion is unnecessary.
DATA(i_f) = CONV i( '4' ).

DATA int8_a TYPE int8.
DATA int8_b TYPE int8 VALUE 1.
DATA(int8_c) = VALUE int8( ).
DATA(int8_d) = CONV int8( 2 ).

*&---------------------------------------------------------------------*
*& Packed numbers of type p
*&---------------------------------------------------------------------*

"The values cannot be specified directly. Character literals must be used whose content
"can be interpreted as a packed number.

DATA p_d0 TYPE p LENGTH 16 VALUE '9639093946237986229144508806129-'.
DATA p_d1 TYPE p LENGTH 16 DECIMALS 1 VALUE '90854421485579917885733186954.8-'.
DATA p_d2 TYPE p LENGTH 16 DECIMALS 2 VALUE '8915050542191264935993694231.83-'.
DATA p_d3 TYPE p LENGTH 16 DECIMALS 3 VALUE '9506074954243163776128965613.326'.
DATA p_d4 TYPE p LENGTH 16 DECIMALS 4 VALUE '160413057636273347593653626.2245-'.
DATA p_d5 TYPE p LENGTH 16 DECIMALS 5 VALUE '40186196674188340559227708.43703-'.
DATA p_d6 TYPE p LENGTH 16 DECIMALS 6 VALUE '4561675521638812947212485.501355-'.
DATA p_d7 TYPE p LENGTH 16 DECIMALS 7 VALUE '814721091997334159140156.5732143'.
DATA p_d8 TYPE p LENGTH 16 DECIMALS 8 VALUE '22204917587256970330176.81515431-'.
DATA p_d9 TYPE p LENGTH 16 DECIMALS 9 VALUE '5185489996588937351174.098863656'.
DATA p_d10 TYPE p LENGTH 16 DECIMALS 10 VALUE '446453616374633295560.0486661184-'.
DATA p_d11 TYPE p LENGTH 16 DECIMALS 11 VALUE '54292568700375612882.85671756677'.
DATA p_d12 TYPE p LENGTH 16 DECIMALS 12 VALUE '6692515911726313748.455325155554'.
DATA p_d13 TYPE p LENGTH 16 DECIMALS 13 VALUE '278093280456662008.1493227920861'.
DATA p_d14 TYPE p LENGTH 16 DECIMALS 14 VALUE '62092769140182925.23049016628459'.

"Note: The length specification means data objects of type p can be 1 to 16 bytes long.
"Two places are packed in each byte. The last byte is packed with the plus/minus sign.
TYPES ty_p_l8d2 TYPE p LENGTH 8 DECIMALS 2.
DATA p_l8d2_a TYPE ty_p_l8d2.

"A packed number consists of the length multiplied by 2 minus 1 digits
"I.e., in the case of the example, 15 digits, of which 2 are reserved for the decimal
"places, are possible.
p_l8d2_a = '1234567890123.99-'.
p_l8d2_a = '1234567890123.99+'.
p_l8d2_a = '1234567890123'.
p_l8d2_a = '1234567890123.00'.
p_l8d2_a = '1234567890123.00+'.
"The following statements will show a syntax warning as the values are
"non-admissible.
"14 places before the period
"p_l8d2_a = '12345678901234'.
"More than two decimal places
"p_l8d2_a = '1.234'.

DATA(p_l8d2_b) = VALUE ty_p_l8d2( ).
DATA(p_l8d2_c) = CONV ty_p_l8d2( '1.23' ).

*&---------------------------------------------------------------------*
*& Decimal floating point numbers of the types decfloat16 and decfloat34
*&---------------------------------------------------------------------*

"16 decimal places
DATA dec16 TYPE decfloat16 VALUE '0.7805874561940696'.

"34 decimal places
DATA dec34 TYPE decfloat34 VALUE '0.0587780463975441530508753423121495'.

*&---------------------------------------------------------------------*
*& Binary floating point numbers of type f
*&---------------------------------------------------------------------*

DATA float TYPE f VALUE '2.1643779466775481E-01'.

"Note: Internally, type f is represented as binary fractions and a dual exponent.
"As a consequence, not every decimal number that is in the type's value range can
"be exactly represented. As a consequence, this may lead to unexpected results
"particulary in calculations and conversions.
float = CONV f( '0.815' ). "8.1499999999999995E-01
DATA(dec34_b) = CONV decfloat34( '0.815' ). "0.815

ASSERT float <> dec34_b.

*&---------------------------------------------------------------------*
*& Generic numeric types
*&---------------------------------------------------------------------*

"Generic types can be specified as types of field symbols and formal
"parameters of procedures such as methods of classes.
"The following examples use field symbols.

"Generic type 'numeric'
"It can accept all of the types above.
FIELD-SYMBOLS <number> TYPE numeric.
ASSIGN i_a TO <number>.
ASSIGN int8_a TO <number>.
ASSIGN p_d0 TO <number>.
ASSIGN p_d14 TO <number>.
ASSIGN dec16 TO <number>.
ASSIGN dec34 TO <number>.
ASSIGN float TO <number>.

"Generic type 'p'
"It can accept all packed number types.
FIELD-SYMBOLS <p> TYPE p.
ASSIGN p_d0 TO <p>.
ASSIGN p_d1 TO <p>.
ASSIGN p_d7 TO <p>.
ASSIGN p_d14 TO <p>.

"Generic type 'decfloat'
"It can accept the types decfloat16 and decfloat34.
FIELD-SYMBOLS <dec> TYPE decfloat.
ASSIGN dec16 TO <dec>.
ASSIGN dec34 TO <dec>.

*&---------------------------------------------------------------------*
*& Excursion: System classes
*&---------------------------------------------------------------------*

"Various system classes are available that deal with numeric types
"The following examples demonstrate:
"- cl_abap_math: Mathematical utility library
"- cl_abap_random*: Compute random numeric values
"Check the class documentation and the methods available. The example only
"covers a selection.

"cl_abap_math: Among others, the class offers attributes to retrieve
"the maximum and minimum possible values of different numeric types
DATA(min_int4) = cl_abap_math=>min_int4.
DATA(max_int4) = cl_abap_math=>max_int4.
DATA(max_int1) = cl_abap_math=>max_int1.
DATA(min_int1) = cl_abap_math=>min_int1.
DATA(max_int2) = cl_abap_math=>max_int2.
DATA(min_int2) = cl_abap_math=>min_int2.
DATA(max_int8) = cl_abap_math=>max_int8.
DATA(min_int8) = cl_abap_math=>min_int8.
DATA(max_decfloat16) = cl_abap_math=>max_decfloat16.
DATA(min_decfloat16) = cl_abap_math=>min_decfloat16.
DATA(max_decfloat34) = cl_abap_math=>max_decfloat34.
DATA(min_decfloat34) = cl_abap_math=>min_decfloat34.

"cl_abap_random*
"The following examples show random numeric value generation and value assignments.
"Note that some 'create' methods also offer to specify minimum and maximum values for
"the random number's value range.

DATA(random_i) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                             min = 1
                                             max = 100 )->get_next( ).
DATA(random_int8) = cl_abap_random_int8=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_dec16) = cl_abap_random_decfloat16=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_dec34) = cl_abap_random_decfloat34=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_f) = cl_abap_random_float=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p) = cl_abap_random_packed=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d1) = cl_abap_random_packed_dec1=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d2) = cl_abap_random_packed_dec2=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d3) = cl_abap_random_packed_dec3=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d4) = cl_abap_random_packed_dec4=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d5) = cl_abap_random_packed_dec5=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d6) = cl_abap_random_packed_dec6=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d7) = cl_abap_random_packed_dec7=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d8) = cl_abap_random_packed_dec8=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d9) = cl_abap_random_packed_dec9=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d10) = cl_abap_random_packed_dec10=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d11) = cl_abap_random_packed_dec11=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d12) = cl_abap_random_packed_dec12=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d13) = cl_abap_random_packed_dec13=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
DATA(random_p_d14) = cl_abap_random_packed_dec14=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Numeric Values in Character-Like Fields

- Most numeric types (except type `i`) cannot be directly specified in the program. These values use character literals that can be interpreted as their respective types.
- The following code snippet demonstrates mathematical, commercial, and scientific notations. Not all notations are possible for each numeric type.
- In this context, you can also check the [Formatting Options in String Templates](07_String_Processing.md#formatting-options-in-string-templates) in the *String Processing* cheat sheet.

```abap
"Mathematical notations
"String of digits, a maximum of one period as decimal separator,
"optional + or - sign on the left (which can be separated by the digits by blanks)
DATA(math_notation) = `- 123.45`.

"Commercial notation
"String of digits, a maximum of one period as decimal separator,
"optional + or - sign on the right (which can be separated by the digits by blanks)
DATA(comm_notation) = `123.45-`.

"Scientific notation
"String of digits, mantissa can include an optional + or -, a period as decimal separator,
"character e or E, and an exponent (which can optionally include a + or - sign and further digits)
DATA(sci_notation) = `-1.23456E03`.

"It depends on the type, which notation is possible.
DATA dec16_n TYPE decfloat16.
DATA dec34_n TYPE decfloat34.
DATA i_n TYPE i.
DATA int8_n TYPE int8.
DATA f_n TYPE f.
DATA p_l16dec14 TYPE p LENGTH 16 DECIMALS 14.

"decfloat16/decfloat34: All are possible
dec16_n = math_notation.
dec16_n = comm_notation.
dec16_n = sci_notation.
dec34_n = math_notation.
dec34_n = comm_notation.
dec34_n = sci_notation.

"f: All are possible (but the signs must not be separated by blanks)
TRY.
    f_n = math_notation.
  CATCH cx_sy_conversion_no_number INTO DATA(e_f_math_notation).
    DATA(e_f_math_notation_text) = e_f_math_notation->get_text( ).
ENDTRY.

DATA(math_notation_no_blanks) = `-123.45`.
f_n = math_notation_no_blanks.

f_n = comm_notation.
f_n = sci_notation.

"p, i, int8: Only mathematical and commercial notatations are allowed
i_n = math_notation.
i_n = comm_notation.

TRY.
    i_n = sci_notation.
  CATCH cx_sy_conversion_no_number INTO DATA(e_i_sci_notation).
    DATA(e_i_sci_notation_text) = e_i_sci_notation->get_text( ).
ENDTRY.

int8_n = math_notation.
int8_n = comm_notation.

TRY.
    int8_n = sci_notation.
  CATCH cx_sy_conversion_no_number INTO DATA(e_int8_sci_notation).
    DATA(e_int8_sci_notation_text) = e_int8_sci_notation->get_text( ).
ENDTRY.

p_l16dec14 = math_notation.
p_l16dec14 = comm_notation.

TRY.
    p_l16dec14 = sci_notation.
  CATCH cx_sy_conversion_no_number INTO DATA(e_p_sci_notation).
    DATA(e_p_sci_notation_text) = e_p_sci_notation->get_text( ).
ENDTRY.

"Using the EXACT operator to check if there is a lossless assignment.
"The example shows assignments to a data object of type decfloat34
"that accepts all notation variants.
dec34_n = EXACT #( `9                      ` ).
dec34_n = EXACT #( `9999                   ` ).
dec34_n = EXACT #( `-9                     ` ).
dec34_n = EXACT #( `- 99                   ` ).
dec34_n = EXACT #( `-9999                  ` ).
dec34_n = EXACT #( `9-                     ` ).
dec34_n = EXACT #( `9999-                  ` ).
dec34_n = EXACT #( `9999999 -              ` ).
dec34_n = EXACT #( `9.99                   ` ).
dec34_n = EXACT #( `.99                    ` ).
dec34_n = EXACT #( `-.99                   ` ).
dec34_n = EXACT #( `0.99999-               ` ).
dec34_n = EXACT #( `9E3                    ` ).
dec34_n = EXACT #( `-9E-3                  ` ).
dec34_n = EXACT #( `.9E-3                  ` ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Arithmetic Expressions and Operators

- Numeric calculations can be performed using [arithmetic expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenarithmetic_expression_glosry.htm) with [arithmetic operators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenarithmetic_operator_glosry.htm).
- Typically, the calculations are performed in operand positions and on the right side of assignments using the assignment operator `=`. Though more positions are possible, this cheat sheet focuses on simple assignments with `=`.
- In arithmetic expressions in ABAP, data objects can include:
  - Data objects of numeric data types (named and unnamed data objects)
  - Data objects whose content can be converted to numeric data types
  - Numeric functions returning a numeric value
  - Functional method calls returning a numeric value
- ABAP supports these arithmetic operators: `+`, `-`, `*`, `/`, `DIV`, `MOD`, and `**`. Notes:
  - `DIV`: Returns the integer part of the division of the left operand by the right, with a positive remainder. 
  - `MOD`: Returns a positive remainder of the division of the left operand by the right.
  - `**`: Raises the left operand to the power of the right (Note: Can produce type `f`; consider using the `ipow` function as an alternative if you want to avoid the type `f`.)
- Parentheses in arithmetic expressions prioritize the calculations inside them.
- You can combine one or more operands to perform calculations.
- You can prefix operands with `+` or `-` signs.
- Note that calculations can trigger exceptions. If not caught, these result in runtime errors. 
  - Examples: Division by zero and overflows (if values exceed the supported range).
- The result of arithmetic expressions is a numeric value. Note the [calculation type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencalculation_type_glosry.htm) as described below.


> [!NOTE]
> - If the first operand is also 0 in a zero division, an exception is not raised. Instead, the result is set to 0.
> - For more information on the `DIV` and `MOD` operators, refer to [this blog](https://community.sap.com/t5/technology-blogs-by-sap/understanding-div-and-mod-in-abap-and-beyond/ba-p/14015181).


The following code snippet explores:
- A selection of (im)possible data objects in arithmetic expressions
- Arithmetic expressions with all arithmetic operators
- Using parentheses
- Calculations raising exceptions
- Pitfalls when performing calculations regarding numeric types and conversion/rounding

```abap

*&---------------------------------------------------------------------*
*& Data objects in arithmetic expressions
*&---------------------------------------------------------------------*

DATA result TYPE i.

"Numeric literals (of type i)
result = 1 + 1.
ASSERT result = 2.

"Literals/unnamed data objects (here, it is a character literal of
"type c length 1, which has a numeric value and can be converted)
result = 1 + '2'.
ASSERT result = 3.

"Named data objects
DATA num TYPE i VALUE 3.
"Character-like data object whose content can be converted to a numeric value
DATA str TYPE string VALUE `7      `.

result = num + str.
ASSERT result = 10.

"Numeric functions
result = 1 + ipow( base = 2 exp = 3 ).
ASSERT result = 9.

"Numeric result of functional method call
"The method call returns a random integer from 1 - 10
result = 1 + cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                         min  = 1
                                         max  = 10 )->get_next( ).
ASSERT result BETWEEN 2 AND 11.

"Content of data object not convertible to numeric value
TRY.
    result = 1 + '#'.
  CATCH cx_sy_conversion_no_number INTO DATA(error).
    DATA(error_text) = error->get_text( ).
ENDTRY.

"Avoid non-convertible operands
"The following statement raises a shortdump
*    DATA dref TYPE REF TO data.
*    dref = NEW utclong( utclong_current( ) ).
*
*    result = 1 + dref->*.

*&---------------------------------------------------------------------*
*& Arithmetic expressions
*&---------------------------------------------------------------------*

"The following code examples demonstrate basic arithmetic expressions
"where all operands and the result are of type i.

DATA res TYPE i.

"Basic calculations using the operators +, -, *, /
res = 1 + 2.
res = 10 - 8.
res = 5 * -5.
res = 18 / 2.

"DIV operator
"Integer part of the division of the left operand by the right, with positive remainder
res = 7 DIV 3. "2 (meaning 2 as integer part of 2.333...)
res = 4 DIV 5. "0
res = 5 DIV 5. "1

"MOD operator
"Positive remainder of the division of the left operand by the right
res = 7 MOD 3. "1
res = 4 MOD 5. "4
res = 5 MOD 5. "0

"** operator: Left operand raised to the power of the right
"Here, special rules apply concerning the calculation type. The following
"examples use inline declarations for the result. That means, the resulting data object
"receives the calculation type. In this case, the type is decfloat34 if one type involved is
"decfloat34. In other cases, it is type f. The example includes RTTI methods to retrieve
"type information. Refer to the cl_abap_typedescr class for the constant values. E.g., 'F'
"stands for floating point numbers.
"Note: As an alternative, you can use the ipow function to avoid the creation of the type f.
DATA(power_result1) = 2 ** 5.
ASSERT cl_abap_typedescr=>describe_by_data( power_result1 )->type_kind = 'F'.
DATA(power_result2) = CONV decfloat34( '2' ) ** 5.
ASSERT cl_abap_typedescr=>describe_by_data( power_result2 )->type_kind = 'e'.

"Multiple operands specified on the right side
"Note that calculation rules apply regarding prioritized calculations.
res = 9 + 8 - 7. "10
res = 1 + 1 * 3. "4
res = 2 + 2 - -2 * 2 / 2 + 4. "10

"Using parentheses
"Arithmetic expressions in parentheses are calculated first
res = 3 + 3 / 3. "4
res = ( 3 + 3 ) / 3. "2

"Calculations raising exceptions
"The following examples show a selection of potential calculation errors
"such as zero division and overflow (i.e. if the resulting value is beyond the
"value range of a type).

"Zero division
TRY.
    res = 1 / 0.
  CATCH cx_sy_zerodivide INTO DATA(e1).
    DATA(e1_text) = e1->get_text( ).
ENDTRY.

"Note: Division by 0 does not raise an exception if the dividend is also 0.
"Then, the result is 0.
TRY.
    res = 0 / 0.
  CATCH cx_sy_zerodivide INTO DATA(e2).
    DATA(e2_text) = e2->get_text( ).
ENDTRY.
ASSERT e2 IS INITIAL.

"Overflow
"The example uses the maximum value of type i, and tries to add 1 to this
"value.
TRY.
    res = 2147483647 + 1.
  CATCH cx_sy_arithmetic_overflow INTO DATA(e3).
    DATA(e3_text) = e3->get_text( ).
ENDTRY.

TRY.
    res = 999999 * 999999.
  CATCH cx_sy_arithmetic_overflow INTO DATA(e4).
    DATA(e4_text) = e4->get_text( ).
ENDTRY.

"Note the pitfalls when performing calculations regarding numeric types and
"conversion/rounding.
"The examples use integer values.
res = 1 / 3.
ASSERT res = 0.

res = 2 / 3.
ASSERT res = 1.

res = 3 * ( 1 / 3 ).
ASSERT res = 0.

res = 3 * ( 2 / 3 ).
ASSERT res = 3.
``` 

<p align="right"><a href="#top">⬆️ back to top</a></p>

### ABAP Calculator Example

- The following example class demonstrates calculator functionality with the basic arithmetic operators `+`, `-`, `*`, and `/`.
- The class contains four methods, each requiring two numbers and an operator (represented as an enumerated type). The methods illustrate: Calculations using data objects of types `i` and `decfloat34`, as well as calculations with any numeric type (the method signature includes the generic type `numeric`). Additionally, each method features a different control structure, such as `IF` and `CASE` statements, and `COND` and `SWITCH` operators.
- To try the example out, create a demo class named `zcl_some_class` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.


```abap
CLASS zcl_some_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ENUM enum_operator,
             add,
             subtract,
             multiply,
             divide,
           END OF ENUM enum_operator.

    METHODS calculate_i IMPORTING num1          TYPE i
                                  operator      TYPE enum_operator
                                  num2          TYPE i
                        RETURNING VALUE(result) TYPE i
                        RAISING   cx_sy_arithmetic_error.

    METHODS calculate_decfloat34 IMPORTING num1          TYPE decfloat34
                                           operator      TYPE enum_operator
                                           num2          TYPE decfloat34
                                 RETURNING VALUE(result) TYPE decfloat34
                                 RAISING   cx_sy_arithmetic_error.

    METHODS calculate_numeric_cond IMPORTING num1          TYPE numeric
                                             operator      TYPE enum_operator
                                             num2          TYPE numeric
                                   RETURNING VALUE(result) TYPE decfloat34
                                   RAISING   cx_sy_arithmetic_error.

    METHODS calculate_numeric_switch IMPORTING num1          TYPE REF TO data
                                               operator      TYPE enum_operator
                                               num2          TYPE REF TO data
                                     RETURNING VALUE(result) TYPE decfloat34
                                     RAISING   cx_sy_arithmetic_error.

ENDCLASS.


CLASS zcl_some_class IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------*
*& Calculations with type i
*& Method implementation using IF statements
*&---------------------------------------------------------------------*

    TYPES: BEGIN OF s_int,
             num1     TYPE i,
             operator TYPE enum_operator,
             num2     TYPE i,
           END OF s_int,
           t_int TYPE TABLE OF s_int WITH EMPTY KEY.

    DATA(i_calculation_tab) = VALUE t_int(
      ( num1 = 1 operator = add num2 = 5 )
      ( num1 = -10 operator = add num2 = 7 )
      ( num1 = 8 operator = subtract num2 = 20 )
      ( num1 = 1000 operator = subtract num2 = 999 )
      ( num1 = 5 operator = multiply num2 = 5 )
      ( num1 = 10 operator = multiply num2 = 10 )
      ( num1 = 10 operator = divide num2 = 5 )
      ( num1 = 3 operator = divide num2 = 2 )
      "Failing calculations
      ( num1 = 1 operator = divide num2 = 0 )
      ( num1 = 0 operator = divide num2 = 0 )
      ( num1 = 2147483647 operator = add num2 = 1 )
      ( num1 = 99999999 operator = multiply num2 = 99999999 ) ).

    DATA calc_results TYPE string_table.

    LOOP AT i_calculation_tab INTO DATA(cal_i).
      TRY.
          DATA(res_i) = calculate_i( num1 = cal_i-num1 operator = cal_i-operator num2 = cal_i-num2 ).
          APPEND |{ cal_i-num1 } { SWITCH #( cal_i-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_i-num2 } = { res_i STYLE = SIMPLE }| TO calc_results.
        CATCH cx_sy_arithmetic_error INTO DATA(error_cal_i).
          APPEND |{ cal_i-num1 } { SWITCH #( cal_i-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_i-num2 } = ERROR! { error_cal_i->get_text( ) }| TO calc_results.
      ENDTRY.
    ENDLOOP.

    out->write( |----------- Calculations with type i -----------| ).
    out->write( calc_results ).
    CLEAR calc_results.

*&---------------------------------------------------------------------*
*& Calculations with type decfloat34
*& Method implementation using CASE statements
*&---------------------------------------------------------------------*

    TYPES: BEGIN OF s_decfloat34,
             num1     TYPE decfloat34,
             operator TYPE enum_operator,
             num2     TYPE decfloat34,
           END OF s_decfloat34,
           t_decfloat34 TYPE TABLE OF s_decfloat34 WITH EMPTY KEY.

    DATA(decfloat34_calculation_tab) = VALUE t_decfloat34(
      ( num1 = CONV decfloat34( '1.4' ) operator = add num2 = CONV decfloat34( '0.7' ) )
      ( num1 = CONV decfloat34( '0.576' ) operator = subtract num2 = CONV decfloat34( '0.924' ) )
      ( num1 = CONV decfloat34( '9283658473562' ) operator = multiply num2 = CONV decfloat34( '23423' ) )
      ( num1 = CONV decfloat34( '984.2435' ) operator = divide num2 = CONV decfloat34( '468.2346' ) )
      "Failing calculations
      ( num1 = CONV decfloat34( '1.0' ) operator = divide num2 = CONV decfloat34( '0.0' ) )
      ( num1 = CONV decfloat34( '0' ) operator = divide num2 = CONV decfloat34( '0' ) )
      ( num1 = cl_abap_math=>max_decfloat34 operator = multiply num2 = CONV decfloat34( '2' ) ) ).

    LOOP AT decfloat34_calculation_tab INTO DATA(cal_decfloat34).
      TRY.
          DATA(res_decfloat34) = calculate_decfloat34( num1 = cal_decfloat34-num1 operator = cal_decfloat34-operator num2 = cal_decfloat34-num2 ).
          APPEND |{ cal_decfloat34-num1 } { SWITCH #( cal_decfloat34-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_decfloat34-num2 } = { res_decfloat34 STYLE = SIMPLE }| TO calc_results.
        CATCH cx_sy_arithmetic_error INTO DATA(error_cal_decfloat34).
          APPEND |{ cal_decfloat34-num1 } { SWITCH #( cal_decfloat34-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_decfloat34-num2 } = ERROR! { error_cal_decfloat34->get_text( ) }| TO calc_results.
      ENDTRY.
    ENDLOOP.

    out->write( |\n| ).
    out->write( |----------- Calculations with type decfloat34 -----------| ).
    out->write( calc_results ).
    CLEAR calc_results.

*&---------------------------------------------------------------------*
*& Calculations with random numeric types
*& Method implementation using the COND operator
*&---------------------------------------------------------------------*

    TYPES: BEGIN OF s_numbers,
             num1     TYPE REF TO data,
             operator TYPE enum_operator,
             num2     TYPE REF TO data,
           END OF s_numbers,
           t_numbers TYPE TABLE OF s_numbers WITH EMPTY KEY.

    TYPES p_d3 TYPE p LENGTH 16 DECIMALS 3.

    DATA(numeric_calculation_tab) = VALUE t_numbers(
       ( num1 = NEW decfloat34( '1.4' ) operator = add num2 = NEW i( 7 ) )
       ( num1 = NEW decfloat16( '0.123454' ) operator = add num2 = NEW decfloat34( '0.7483265' ) )
       ( num1 = NEW p_d3( '123.456' ) operator = subtract num2 = NEW i( 5 ) )
       ( num1 = NEW f( '2.1643779466775481E-01' ) operator = subtract num2 = NEW decfloat16( '1.23' ) )
       ( num1 = NEW i( 100 ) operator = multiply num2 = NEW i( 50 ) )
       ( num1 = NEW f( '1.2' ) operator = multiply num2 = NEW i( 5 ) )
       ( num1 = NEW decfloat34( '5.4' ) operator = divide num2 = NEW p_d3( '1.123' ) )
       ( num1 = NEW i( 999 ) operator = divide num2 = NEW decfloat16( '111' ) )
       "Failing calculations
       ( num1 = NEW i( 1 ) operator = divide num2 = NEW decfloat34( '0' ) )
       ( num1 = NEW p_d3( '0' ) operator = divide num2 = NEW p_d3( '0' ) )
       ( num1 = NEW decfloat34( cl_abap_math=>max_decfloat34 ) operator = multiply num2 = NEW decfloat34( '2' ) ) ).

    LOOP AT numeric_calculation_tab INTO DATA(cal_numeric).
      TRY.
          DATA(res_numeric) = calculate_numeric_cond( num1 = cal_numeric-num1->* operator = cal_numeric-operator num2 = cal_numeric-num2->* ).
          APPEND |{ cal_numeric-num1->* } { SWITCH #( cal_numeric-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_numeric-num2->* } = { res_numeric STYLE = SIMPLE }| TO calc_results.
        CATCH cx_sy_arithmetic_error INTO DATA(error_cal_numeric).
          APPEND |{ cal_numeric-num1->* } { SWITCH #( cal_numeric-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_numeric-num2->* } = ERROR! { error_cal_numeric->get_text( ) }| TO calc_results.
      ENDTRY.
    ENDLOOP.

    out->write( |\n| ).
    out->write( |----------- Calculations with random numeric types (COND operator) -----------| ).
    out->write( calc_results ).
    CLEAR calc_results.

*&---------------------------------------------------------------------*
*& Calculations with random numeric types
*& Method implementation using the SWITCH operator
*&---------------------------------------------------------------------*

    numeric_calculation_tab = VALUE #(
       ( num1 = NEW decfloat34( '0.4' ) operator = add num2 = NEW i( 20 ) )
       ( num1 = NEW decfloat16( '0.827123454' ) operator = subtract num2 = NEW decfloat34( '1.89632' ) )
       ( num1 = NEW p_d3( '1.5' ) operator = multiply num2 = NEW f( '3.123' ) )
       ( num1 = NEW i( 4 ) operator = divide num2 = NEW p_d3( '1.2' ) )
       "Failing calculations
       ( num1 = NEW i( 1 ) operator = divide num2 = NEW decfloat34( '0' ) )
       ( num1 = NEW p_d3( '0' ) operator = divide num2 = NEW p_d3( '0' ) )
       ( num1 = NEW decfloat34( cl_abap_math=>max_decfloat34 ) operator = multiply num2 = NEW decfloat34( '2' ) ) ).

    LOOP AT numeric_calculation_tab INTO cal_numeric.
      TRY.
          res_numeric = calculate_numeric_cond( num1 = cal_numeric-num1->* operator = cal_numeric-operator num2 = cal_numeric-num2->* ).
          APPEND |{ cal_numeric-num1->* } { SWITCH #( cal_numeric-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_numeric-num2->* } = { res_numeric STYLE = SIMPLE }| TO calc_results.
        CATCH cx_sy_arithmetic_error INTO error_cal_numeric.
          APPEND |{ cal_numeric-num1->* } { SWITCH #( cal_numeric-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_numeric-num2->* } = ERROR! { error_cal_numeric->get_text( ) }| TO calc_results.
      ENDTRY.
    ENDLOOP.

    out->write( |\n| ).
    out->write( |----------- Calculations with random numeric types (SWITCH operator) -----------| ).
    out->write( calc_results ).

  ENDMETHOD.

  METHOD calculate_i.
    IF operator = add.
      result = num1 + num2.
    ELSEIF operator = subtract.
      result = num1 - num2.
    ELSEIF operator = multiply.
      result = num1 * num2.
    ELSEIF operator = divide.
      IF num1 = 0 AND num2 = 0.
        RAISE EXCEPTION TYPE cx_sy_zerodivide.
      ELSE.
        result = num1 / num2.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_decfloat34.
    CASE operator.
      WHEN add.
        result = num1 + num2.
      WHEN subtract.
        result = num1 - num2.
      WHEN multiply.
        result = num1 * num2.
      WHEN divide.
        CASE num1.
          WHEN 0.
            CASE num2.
              WHEN 0.
                RAISE EXCEPTION TYPE cx_sy_zerodivide.
            ENDCASE.
        ENDCASE.
        result = num1 / num2.
    ENDCASE.
  ENDMETHOD.

  METHOD calculate_numeric_cond.
    result = COND #( WHEN operator = add
                     THEN num1 + num2
                     WHEN operator = subtract
                     THEN num1 - num2
                     WHEN operator = multiply
                     THEN num1 * num2
                     WHEN operator = divide AND num2 = 0 THEN THROW cx_sy_zerodivide(  )
                     WHEN operator = divide AND num2 <> 0 THEN num1 / num2 ).
  ENDMETHOD.

  METHOD calculate_numeric_switch.
    result = SWITCH #( operator
                       WHEN add THEN num1->* + num2->*
                       WHEN subtract THEN num1->* - num2->*
                       WHEN multiply THEN num1->* * num2->*
                       WHEN divide THEN SWITCH #( num1->* WHEN 0 THEN SWITCH #( num2->* WHEN 0 THEN THROW cx_sy_zerodivide(  ) ) ELSE num1->* / num2->* ) ).
  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Calculation Type

- Arithmetic expressions have a calculation type that defines the calculation rules (particularly with regard to rounding of the types used).
- This type is determined at runtime from the identifiable data types involved (i.e., those known statically).
- The calculation type can influence the result before it is assigned to a data object, such as rounding.
- An inline-declared target data object receives the calculation type as its data type. Note: In these cases, type `p` (unless overridden by another rule, as shown in the example below) results in type `p` with length 8 and no decimal places.
- Generally, the largest data type involved determines the calculation type, with exceptions like type `f` in expressions with `**`.
- Note the conversion rules for elementary, non-numeric data types:
  - `d`, `t`, `x`, `xstring` are converted to `i`
  - `c`, `n`, `string` are converted to `p`
  - `utclong` is not supported
- It is advisable to ensure all operands and the target data object share the same numeric type to avoid unnecessary conversions (this is intentionally not taken into consideration in many of the examples).
- The `CONV` operator can be used to adjust the calculation type on the right side of an assignment.
- Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENARITH_TYPE.html).

Examples

```abap
*&---------------------------------------------------------------------*
*& Calculation type
*&---------------------------------------------------------------------*

"The following code snippets deal with the calculation type, which can be regarded as
"property of arithmetic expressions and depends on the numeric data types
"involved in the calculation.
"The calculation type is determined at runtime.
"Specific rules are determined by the calculation type.
"If the numeric types of the operands of the arithmetic expression can be
"determined statically, the calculation type is determined on the basis of
"specific rules. The examples use inline declarations for the result.
"In such cases, the target data object's type is derived from the calculation type.
"The rules apply in the following decreasing priority order.
"Many examples use RTTI methods to retrieve type kind information to check the 
"calculation type. For the type kind values, check the constants in class cl_abap_typedescr. 

"---- 1 ----
"The calculation type is a decfloat type if one of the types involved is decfloat34
"or decfloat16. If decfloat34 is involved, it is decfloat34.

"i + decfloat34
DATA(res_dec_a) = 1 + CONV decfloat34( '1.1' ).
ASSERT cl_abap_typedescr=>describe_by_data( res_dec_a )->type_kind = 'e'.

"decfloat16 + decfloat34
DATA(res_dec_b) = CONV decfloat16( '2.2' ) + CONV decfloat34( '1.1' ).
ASSERT cl_abap_typedescr=>describe_by_data( res_dec_b )->type_kind = 'e'.

"decfloat16 + i
DATA(res_dec_c) = CONV decfloat16( '2.2' ) + 5.
ASSERT cl_abap_typedescr=>describe_by_data( res_dec_c )->type_kind = 'a'.

"decfloat16 + int8
DATA(res_dec_d) = CONV decfloat16( '2.2' ) + CONV int8( 2 ).
ASSERT cl_abap_typedescr=>describe_by_data( res_dec_d )->type_kind = 'a'.

"decfloat34 + p
DATA pd3 TYPE p LENGTH 16 DECIMALS 3 VALUE '1.326'.
DATA(res_dec_e) = CONV decfloat34( '2.2' ) + pd3.
ASSERT cl_abap_typedescr=>describe_by_data( res_dec_e )->type_kind = 'e'.

"decfloat16 + p
DATA(res_dec_f) = CONV decfloat16( '2.2' ) + pd3.
ASSERT cl_abap_typedescr=>describe_by_data( res_dec_f )->type_kind = 'a'.

"decfloat34 + f
DATA(res_dec_g) = CONV decfloat34( '2.2' ) + CONV f( '2.23645E-01' ).
ASSERT cl_abap_typedescr=>describe_by_data( res_dec_g )->type_kind = 'e'.

"decfloat16 + f
DATA(res_dec_h) = CONV decfloat16( '2.2' ) + CONV f( '2.23645E-01' ).
ASSERT cl_abap_typedescr=>describe_by_data( res_dec_h )->type_kind = 'a'.

"---- 2 ----
"The calculation type is f if one of the types involved is f.

"f + i
DATA(res_f_a) = 1 + CONV f( '2.23645E-01' ).
ASSERT cl_abap_typedescr=>describe_by_data( res_f_a )->type_kind = 'F'.
"f + int8
DATA(res_f_b) = CONV f( '2.23645E-01' ) + CONV int8( 2 ).
ASSERT cl_abap_typedescr=>describe_by_data( res_f_b )->type_kind = 'F'.
"f + p
DATA(res_f_c) = 1 + CONV f( '2.23645E-01' ).
ASSERT cl_abap_typedescr=>describe_by_data( res_f_c )->type_kind = 'F'.

"f + decfloat34
"Note: Due to the higher priority of decfloat34, the calculation type and the
"result are of type decfloat34.
DATA(res_f_d) = CONV decfloat34( '1.1' ) + CONV f( '2.23645E-01' ).
ASSERT cl_abap_typedescr=>describe_by_data( res_dec_e )->type_kind = 'e'.

"f + decfloat16
DATA(res_f_e) = CONV decfloat16( '1.1' ) + CONV f( '2.23645E-01' ).
ASSERT cl_abap_typedescr=>describe_by_data( res_f_e )->type_kind = 'a'.

"---- 3 ----
"The calculation type is p if one of the data types involved is p.
DATA p_l16d3 TYPE p LENGTH 16 DECIMALS 3 VALUE '1.234'.

"Note: For the result of a computation with type p, the type p length 8 with 0 decimals is used
"implicitly. It is not possible to determine the required length and decimal places.
"Therefore, inline declarations should be avoided for such calculations.
"Specifying ##TYPE suppresses a syntax warning.

"p + i
DATA(res_p_a) = p_l16d3 + 1 ##TYPE.
ASSERT cl_abap_typedescr=>describe_by_data( res_p_a )->type_kind = 'P'.
ASSERT cl_abap_typedescr=>describe_by_data( res_p_a )->length = 8.
ASSERT cl_abap_typedescr=>describe_by_data( res_p_a )->decimals = 0.

"p + int8
DATA(res_p_b) = p_l16d3 + CONV int8( 1 ) ##TYPE.
ASSERT cl_abap_typedescr=>describe_by_data( res_p_b )->type_kind = 'P'.
ASSERT cl_abap_typedescr=>describe_by_data( res_p_b )->length = 8.
ASSERT cl_abap_typedescr=>describe_by_data( res_p_b )->decimals = 0.

"---- 4 ----
"The calculation type is int8 if one of the data types involved is int8.

DATA(res_int8_a) = CONV int8( 2 ) + 2.
ASSERT cl_abap_typedescr=>describe_by_data( res_int8_a )->type_kind = '8'.

"---- 5 ----
"The calculation type is i if one of the data types involved is i.

DATA(res_i_a) = 2 + 5.
ASSERT cl_abap_typedescr=>describe_by_data( res_i_a )->type_kind = 'I'.

"The standard type is used in case of generally typed operands
FIELD-SYMBOLS <num> TYPE numeric.
ASSIGN pd3 TO <num>.
DATA(res_gen_a) = <num> + 1.
DATA(t_kind_a) = cl_abap_typedescr=>describe_by_data( res_gen_a )->type_kind.

ASSIGN res_i_a TO <num>.
DATA(res_gen_b) = <num> + 1.
DATA(t_kind_b) = cl_abap_typedescr=>describe_by_data( res_gen_b )->type_kind.

ASSIGN res_f_a TO <num>.
DATA(res_gen_c) = <num> + 1.
DATA(t_kind_c) = cl_abap_typedescr=>describe_by_data( res_gen_c )->type_kind.

ASSERT t_kind_a = t_kind_b.
ASSERT t_kind_b = t_kind_c.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Calculation Assignments

- The following calculation assignments are supported in ABAP: `+=`, `-=`, `*=`, `/=` 
- They make code more concise and reflect the following patterns: 
  ```abap
  a = a + ( b ). 
  a = a - ( b ). 
  a = a * ( b ). 
  a = a / ( b ).

  "Using calculation assignments
  a += b.
  a -= b.
  a *= b.
  a /= b.
  ```

Example:

```abap
DATA int_calc TYPE i.

int_calc = 1.
int_calc = int_calc + 1. "2
int_calc += 1. "3

int_calc = 10.
int_calc = int_calc - 1. "9
int_calc -= 1. "8

int_calc = 5.
int_calc = int_calc * 2. "10
int_calc *= 3. "30

int_calc = 100.
int_calc = int_calc / 2. "50
int_calc /= 2. "25
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Lossless Calculations

- You can use the `EXACT` operator to verify lossless assignments and calculations.
- It is suitable for assignments between incompatible data objects, meaning those without matching type specifications.
- After an assignment with `EXACT`, the target data object will hold a valid value; otherwise, it raises an exception.
- Forced rounding during an assignment indicates it is not lossless. In the context of arithmetic expressions, a lossless calculation is performed.


```abap
"Lossless assignments
TYPES ty_pl8d3 TYPE p LENGTH 8 DECIMALS 3.

"The following examples show character literals whose content
"is assigned to the target data objects. Using the EXACT operator,
"it is checked if there is a lossless assignment. The example
"values are ok, and an exact move is possible.
DATA(p_a) = EXACT ty_pl8d3( '123.987' ).
DATA(p_b) = EXACT ty_pl8d3( '123.98' ).

"The following example raises an exception. The example character literal
"has four decimal places. Thus, a rounding is necessary, which means that
"a lossless assignment is not possible. Catching the CX_SY_CONVERSION_ROUNDING
"exception is necessary.
"DATA(p_c) = EXACT ty_p_l8d3( '123.9876' ).

TRY.
    DATA(p_d) = EXACT ty_pl8d3( '123.9876' ).
  CATCH cx_sy_conversion_rounding INTO DATA(error_p_rounding).
    DATA(error_p_rounding_text) = error_p_rounding->get_text( ).
ENDTRY.

"Lossless calculations using arithmetic expressions
"The first statement works, whereas the second statement raises an exception.
"A rounding to two decimal places is required.
TYPES ty_pl8d2 TYPE p LENGTH 8 DECIMALS 2.
DATA(p_e) = EXACT ty_pl8d2( 1 / 4 ).
"DATA(p_f) = EXACT ty_pl8d2( 1 / 3 ).

"Catching exceptions when rounding in lossless calculations
TRY.
    DATA(p_g) = EXACT ty_pl8d2( 1 / 3 ).
  CATCH cx_sy_conversion_rounding INTO error_p_rounding.
    error_p_rounding_text = error_p_rounding->get_text( ).
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Numeric Functions

Various built-in functions are available. The following code snippet shows a selection of numeric built-in functions.

```abap
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

"Numeric extremum functions that return the value of the largest
"or smallest of the passed arguments.

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

"--- acos, asin, atan, cos, sin, tan, cosh, sinh, tanh, exp, log, log10, sqrt ---
"The following examples cover a selection.

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

"------------- round -------------
"Rounding functions expect a decimal floating point number as argument.
"The return value is of type decfloat34. The functions can be used to round
"to decimal places and precisions. In addition, rounding rules can be specified.

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

"------------- rescale -------------
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

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Calculations with System Classes

- This section covers the `cl_abap_bigint` and `cl_abap_rational` classes.
- Example use cases for these classes include:
  - Handling large numbers with many digits and high precision
  - Calculating very small or large numbers
  - Avoiding the risk of overflows
  - Performing basic calculations and beyond
  - With `cl_abap_rational`, you can manage rational numbers to preserve fractions and ensure high precision.


The following code snippet explores a selection of various methods the classes offer. Refer to the class documentation for more information. More methods are available.

```abap
*&---------------------------------------------------------------------*
*& cl_abap_bigint
*&---------------------------------------------------------------------*

"Factories
DATA(bigint_int4) = cl_abap_bigint=>factory_from_int4( 10 ).
DATA(bigint_int4c) = cl_abap_bigint=>factory_from_string( `283469208407283452340` ).
DATA(bigint_int4d) = cl_abap_bigint=>factory_from_int8( CONV int8( 1234567890123456 ) ).

DATA(a1) = cl_abap_bigint=>factory_from_int4( -10 )->to_external( ).
DATA(a2) = cl_abap_bigint=>factory_from_int4( -10 )->to_external( iv_flg_minus_in_front = abap_true ).
DATA(a3) = cl_abap_bigint=>factory_from_int4( 100 )->to_utf8( ).
DATA(a4) = cl_abap_bigint=>factory_from_string( `123` )->to_df34( ).
DATA(a5) = cl_abap_bigint=>factory_from_int4( -10 )->to_string( ).
DATA(a6) = cl_abap_bigint=>factory_from_int4( 4 )->add( bigint_int4 )->to_string( ).
DATA(a7) = cl_abap_bigint=>factory_from_int4( 7 )->add_int4( 2 )->to_string( ).
DATA(a8) = cl_abap_bigint=>factory_from_int4( -10 )->abs( )->to_string( ).
DATA(a9) = cl_abap_bigint=>factory_from_int4( 19 )->compare_int4( 20 ).
DATA(a10) = cl_abap_bigint=>factory_from_int4( 100 )->compare( bigint_int4 ).
DATA(a11) = cl_abap_bigint=>factory_from_int4( 20 )->div( bigint_int4 ).
DATA(a12) = a11-quotient->to_string( ).
DATA(a13) = a11-remainder->to_string( ).
DATA(a14) = cl_abap_bigint=>factory_from_int4( 10 )->div_int4( 3 ).
DATA(a15) = a14-quotient->to_string( ).
DATA(a16) = a14-remainder.
DATA(a17) = cl_abap_bigint=>factory_from_int4( 10 )->div_by_two_power( CONV int8( 2 ) )->to_string( ).
DATA(a18) = cl_abap_bigint=>factory_from_int4( 5 )->div_to_df34( bigint_int4 ).
DATA(a19) = cl_abap_bigint=>factory_from_int4( 50 )->gcd( bigint_int4 )->to_string( ).
DATA(a20) = cl_abap_bigint=>factory_from_int4( 1000 )->get_number_of_bits( ).
DATA(a21) = cl_abap_bigint=>factory_from_int4( 10 )->is_equal( bigint_int4 ).

cl_abap_bigint=>factory_from_string( `123` )->is_int4(
  IMPORTING
    ev_int4_value  = DATA(a22)
  RECEIVING
    rv_flg_is_int4 = DATA(a23)
).
DATA(a24) = cl_abap_bigint=>factory_from_int4( 11 )->is_larger( bigint_int4 ).
DATA(a25) = cl_abap_bigint=>factory_from_int4( 10 )->is_larger_or_equal( bigint_int4 ).
DATA(a26) = cl_abap_bigint=>factory_from_int4( -10 )->is_negative( ).
DATA(a27) = cl_abap_bigint=>factory_from_int4( 0 )->is_zero( ).
DATA(a28) = cl_abap_bigint=>factory_from_int4( 123 )->mod( bigint_int4 )->to_string( ).
DATA(a29) = cl_abap_bigint=>factory_from_int4( 10 )->mod_int4( 3 ).
DATA(a30) = cl_abap_bigint=>factory_from_int4( 10 )->mul( bigint_int4 )->to_string( ).
DATA(a31) = cl_abap_bigint=>factory_from_int4( 5 )->mul_by_two_power( 2 )->to_string( ).
DATA(a32) = cl_abap_bigint=>factory_from_int4( 2 )->mul_int4( 5 )->to_string( ).
DATA(a33) = cl_abap_bigint=>factory_from_int4( 8 )->pow( 2 )->to_string( ).
DATA(a34) = cl_abap_bigint=>factory_from_int4( 9 )->sqrt( )->to_string( ).
DATA(a35) = cl_abap_bigint=>factory_from_int4( 18 )->sub( bigint_int4 )->to_string( ).
DATA(a36) = cl_abap_bigint=>factory_from_int4( 15 )->sub_int4( 9 )->to_string( ).
"Cloning
DATA(a37) = cl_abap_bigint=>factory_from_int4( 15 ).
DATA(a38) = cl_abap_bigint=>factory_from_int4( 5 ).
"Adding a number to another number to not get a new instance but the original instance
DATA(a39) = a37->add( a38 ).
ASSERT a39 = a37.
DATA(a40) = a37->to_string( ).
DATA(a41) = a39->to_string( ).
DATA(a42) = cl_abap_bigint=>factory_from_int4( 15 ).
DATA(a43) = cl_abap_bigint=>factory_from_int4( 5 ).

DATA(a44) = a42->clone( )->add( a43 ).
ASSERT a44 <> a42.
DATA(a45) = a42->to_string( ).
DATA(a46) = a44->to_string( ).
DATA(a47) = cl_abap_bigint=>factory_from_int4( 15 )->sub_int4( 9 )->clone( )->to_string( ).

*&---------------------------------------------------------------------*
*& cl_abap_rational
*&---------------------------------------------------------------------*

"Factories
DATA(r1) = cl_abap_rational=>factory_from_string( `-1/3` ).
DATA(r2) = cl_abap_rational=>factory_from_bigint( cl_abap_bigint=>factory_from_int4( 11 ) ).

TYPES p_l16d5 TYPE p LENGTH 16 DECIMALS 5.
DATA(r3) = cl_abap_rational=>factory_from_dec( CONV p_l16d5( '123456.789' ) ).
DATA(r4) = cl_abap_rational=>factory_from_decimal_string( `1.234567890` ).
DATA(r5) = cl_abap_rational=>factory_from_df34( `1.4` ).
DATA(r6) = cl_abap_rational=>factory_from_int4( 100 ).
DATA(r7) = cl_abap_rational=>factory_from_int8( CONV int8( 123 ) ).
DATA(r8) = cl_abap_rational=>factory_from_string( `-2/3` )->add_int4( 1 )->to_df34( ).
DATA(r9) = cl_abap_rational=>factory_from_string( `-2/3` )->add_int4( 1 )->to_string( ).
DATA r10 TYPE p_l16d5.
cl_abap_rational=>factory_from_string( `-2/3` )->add_int4( 1 )->to_dec( IMPORTING ev_decimal = r10 ).
"Methods that include cl_abap_bigint instances
DATA(r11) = cl_abap_rational=>factory_from_string( `-2/3` )->add_bigint( cl_abap_bigint=>factory_from_int4( 1 ) )->to_string( ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Calculations with Date, Time and Time Stamp Values


```abap
*&---------------------------------------------------------------------*
*& Date calculations
*&---------------------------------------------------------------------*

DATA date_calc_1 TYPE d VALUE '20240101'.
DATA date_calc_2 TYPE d VALUE '20231227'.
DATA date_calc_3 TYPE d VALUE '20231230'.
DATA date_calc_4 TYPE d VALUE '20240220'.

DATA(days_diff_1) = date_calc_1 - date_calc_2. "5
DATA(days_diff_2) = date_calc_1 - date_calc_3. "2
DATA(days_since_01_01_0001) = CONV i( date_calc_1 ). "738887

"------------ Performing date additions and subtractions using the XCO library ------------

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

DATA(xco_yesterday) = xco_cp=>sy->date( )->subtract( iv_day = 1
                                        )->as( xco_cp_time=>format->iso_8601_extended
                                        )->value.

"Addition with a custom date
"2024-02-29
DATA(xco_date_add_1day_custom) = xco_cp_time=>date( iv_year = '2024' iv_month = '02' iv_day = '28'
                                                  )->add( iv_day = 1
                                                  )->as( xco_cp_time=>format->iso_8601_extended
                                                  )->value.

"Subtraction with a custom date
DATA(xco_date_subtract_misc) = xco_cp_time=>date( iv_year = '2024' iv_month = '02' iv_day = '28'
                                                )->add( iv_day = 1 iv_month = 1 iv_year = 1
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
  CATCH cx_root INTO DATA(error_inv_date1).
    DATA(error_inv_date1_text) = error_inv_date1->get_text( ).
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
  CATCH cx_root INTO DATA(error_inv_date2).
    DATA(error_inv_date2_text) = error_inv_date2->get_text( ).
ENDTRY.

*&---------------------------------------------------------------------*
*& Time calculations
*&---------------------------------------------------------------------*

"------------ Calculating the time delta between two time values ------------

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

"---- Calculating the total values of the time difference in seconds, minutes and hours ----
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

"Addition with a custom time
"13:36:59
DATA(time_xco_cust_add) = xco_cp_time=>time( iv_hour   = '12'
                                             iv_minute = '34'
                                             iv_second = '56'
                                           )->add( iv_hour   = 1
                                                   iv_minute = 2
                                                   iv_second = 3
                                           )->as( xco_cp_time=>format->iso_8601_extended
                                           )->value.

"Subtraction with a custom time
"11:32:53
DATA(time_xco_cust_subtr) = xco_cp_time=>time( iv_hour   = '12'
                                               iv_minute = '34'
                                               iv_second = '56'
                                             )->subtract( iv_hour   = 1
                                                          iv_minute = 2
                                                          iv_second = 3
                                             )->as( xco_cp_time=>format->iso_8601_extended
                                             )->value.

*&---------------------------------------------------------------------*
*& Time stamp calculations
*&---------------------------------------------------------------------*

"Time stamp calculations with the built-in function utclong_add
"With the built-in function utclong_add, at least one parameter must be specified
"besides 'val'.
"Note that there are no parameters for years and months.
DATA(utc4calc) = CONV utclong( '2024-01-01 15:55:14.1173220' ).

"Adding one hour
"e.g. 2024-01-01 16:55:14.1173220
DATA(ts1) = utclong_add( val = utc4calc
                         hours = 1 ).

"Subtracting one hour by passing a negative integer value (no separate substract
"function available)
"e.g. 2024-01-01 14:55:14.1173220
DATA(ts3) = utclong_add( val = utc4calc
                         hours = -1 ).

"Using all parameters
"e.g. 2024-01-02 18:09:07.2373220
DATA(ts4) = utclong_add( val = utc4calc
                         days = 1 "type i
                         hours = 2 "type i
                         minutes = CONV int8( '13' )
                         seconds = CONV decfloat34( '53.12' ) ).

"Using XCO
"Addition with current time stamp
DATA(xco_ts_add) = xco_cp=>sy->moment( xco_cp_time=>time_zone->user
                                     )->add( iv_year        = 1
                                             iv_month       = 2
                                             iv_day         = 3
                                             iv_hour        = 4
                                             iv_minute      = 5
                                             iv_second      = 6
                                     )->as( xco_cp_time=>format->iso_8601_extended
                                     )->value.

"Subtraction with current time stamp
DATA(xco_ts_subtr) = xco_cp=>sy->moment( xco_cp_time=>time_zone->user
                                       )->subtract( iv_year        = 1
                                                    iv_month       = 2
                                                    iv_day         = 3
                                                    iv_hour        = 4
                                                    iv_minute      = 5
                                                    iv_second      = 6
                                       )->as( xco_cp_time=>format->iso_8601_extended
                                       )->value.

"Addition with a custom time stamp
"2026-03-18T16:40:02
DATA(xco_cust_ts_add) = xco_cp_time=>moment( iv_year   = '2025'
                                             iv_month  = '01'
                                             iv_day    = '15'
                                             iv_hour   = '12'
                                             iv_minute = '34'
                                             iv_second = '56'
                                           )->add( iv_year        = 1
                                                   iv_month       = 2
                                                   iv_day         = 3
                                                   iv_hour        = 4
                                                   iv_minute      = 5
                                                   iv_second      = 6
                                           )->as( xco_cp_time=>format->iso_8601_extended
                                           )->value.

"Subtraction with a custom time stamp
"2023-11-12T08:29:50
DATA(xco_cust_ts_subtr) = xco_cp_time=>moment( iv_year   = '2025'
                                               iv_month  = '01'
                                               iv_day    = '15'
                                               iv_hour   = '12'
                                               iv_minute = '34'
                                               iv_second = '56'
                                             )->subtract( iv_year        = 1
                                                          iv_month       = 2
                                                          iv_day         = 3
                                                          iv_hour        = 4
                                                          iv_minute      = 5
                                                          iv_second      = 6
                                             )->as( xco_cp_time=>format->iso_8601_extended
                                             )->value.

"Interval calculations
DATA(xco_moment1) = xco_cp_time=>moment( iv_year       = '2025'
                                         iv_month  = '01'
                                         iv_day    = '15'
                                         iv_hour   = '12'
                                         iv_minute = '34'
                                         iv_second = '56' ).

DATA(xco_moment2) = xco_cp_time=>moment( iv_year   = '2026'
                                         iv_month  = '02'
                                         iv_day    = '16'
                                         iv_hour   = '16'
                                         iv_minute = '38'
                                         iv_second = '52' ).

DATA(xco_ts_interval) = xco_moment1->interval_to( xco_moment2 ).
DATA(ts_interval_low) = xco_ts_interval->lower_bound->as( xco_cp_time=>format->iso_8601_extended )->value.
DATA(ts_interval_high) = xco_ts_interval->upper_bound->as( xco_cp_time=>format->iso_8601_extended )->value.

"Calculating time stamp differences using the built-in function utclong_diff

DATA(ts5) = CONV utclong( '2024-01-01 05:30:00' ).
DATA(ts6) = CONV utclong( '2024-01-01 06:30:00' ).
"The return value has the type decfloat34. It contains the exact difference in seconds.

DATA(ts_diff1) = utclong_diff( high = ts6
                               low = ts5 ). "3600

DATA(ts_diff2) = utclong_diff( high = ts5
                               low = ts6 ). "-3600

DATA(low_timestamp) = CONV utclong( '2024-01-01 05:30:00' ).
DATA(high_timestamp) = CONV utclong( '2024-01-03 10:35:12' ).

"'diff' method: Calculating time differences
cl_abap_utclong=>diff( EXPORTING high    = high_timestamp
                                 low     = low_timestamp
                       IMPORTING days    = DATA(diff_days) "2
                                 hours   = DATA(diff_hours) "5
                                 minutes = DATA(diff_minutes) "5
                                 seconds = DATA(diff_seconds) ). "12

"CL_ABAP_TSTMP: Calculating and converting time stamps in packed numbers

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
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Arithmetic Expressions and Built-in Functions in ABAP SQL and ABAP CDS

More information: 
- ABAP SQL: [Arithmetic expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENSQL_ARITH.html), [Numeric functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENSQL_ARITH_FUNC.html)
- ABAP CDS: [Arithmetic expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENCDS_ARITHMETIC_EXPRESSION_V2.html), [Numeric functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENCDS_SQL_FUNCTIONS_NUMERIC_V2.html)

The following example code snippet includes an ABAP SQL `SELECT` statement with a selection of arithmetic expressions and numeric functions. You can also check [aggregate expressions](03_ABAP_SQL.md#aggregate-expressions) that perform calculations as described in the *ABAP SQL* cheat sheet.

```abap
SELECT SINGLE
   "---- Arithmethic expressions ----
   1 + 2 AS add, "3
   10 - -8 AS subtract, "18
   3 * 5 AS multiply, "15
   "The / operator is not allowed in integer expressions.
   "4 / 2 AS divide_nope,
   "The following example uses typed literals to use decimal
   "floating point numbers.
   d34n`4.4` / d34n`2.2` AS divide1, "2
   "The following example uses cast expressions to convert integer values
   "so that a division can be performed.
   CAST( 1 AS D34N ) / CAST( 2 AS D34N ) AS divide2, "0.5
   "Using parentheses
   ( 1 + 2 ) * 3 AS parentheses, "9
   "---- Functions for numeric values ----
   "Division, result rounded to an integer
   div( 4, 2 ) AS div, "2
   "Division, 3rd argument: result is rounded to the specified
   "number of decimals
   division( 1, 3, 2 ) AS division, "0.33
   "Result is rounded to first greater integer
         ceil( decfloat34`1.333` ) AS ceil, "2
   "Result is the remainder of division
   mod( 3, 2 ) AS mod, "1
   "Largest integer value not greater than the specified value
   floor( decfloat34`1.333` ) AS floor, "1
   "Returns the absolute number
   abs( int4`-2` ) AS abs, "2
   "Result is rounded to the specified position after the decimal separator
   round( decfloat34`1.337`, 2 ) AS round "1.34
 FROM i_timezone "Released API (not relevant in the example)
 INTO @DATA(some_data).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

Expand the following collapsible section for an example class. It contains the code snippets from the cheat sheet. To try it out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The examples is set up to display output in the console only for the calculator snippet. To explore the effect of statements and content of variables, you may want to set break points and walk through the code in the debugger.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    "Type and methods for calculator example
    TYPES: BEGIN OF ENUM enum_operator,
             add,
             subtract,
             multiply,
             divide,
           END OF ENUM enum_operator.

    METHODS calculate_i IMPORTING num1          TYPE i
                                  operator      TYPE enum_operator
                                  num2          TYPE i
                        RETURNING VALUE(result) TYPE i
                        RAISING   cx_sy_arithmetic_error.

    METHODS calculate_decfloat34 IMPORTING num1          TYPE decfloat34
                                           operator      TYPE enum_operator
                                           num2          TYPE decfloat34
                                 RETURNING VALUE(result) TYPE decfloat34
                                 RAISING   cx_sy_arithmetic_error.

    METHODS calculate_numeric_cond IMPORTING num1          TYPE numeric
                                             operator      TYPE enum_operator
                                             num2          TYPE numeric
                                   RETURNING VALUE(result) TYPE decfloat34
                                   RAISING   cx_sy_arithmetic_error.

    METHODS calculate_numeric_switch IMPORTING num1          TYPE REF TO data
                                               operator      TYPE enum_operator
                                               num2          TYPE REF TO data
                                     RETURNING VALUE(result) TYPE decfloat34
                                     RAISING   cx_sy_arithmetic_error.

    "Methods demonstrating calculation assignments
    CLASS-METHODS create_calculation IMPORTING num        TYPE numeric
                                     RETURNING VALUE(ref) TYPE REF TO zcl_demo_abap.
    METHODS plus IMPORTING num        TYPE numeric
                 RETURNING VALUE(ref) TYPE REF TO zcl_demo_abap.
    METHODS minus IMPORTING num        TYPE numeric
                  RETURNING VALUE(ref) TYPE REF TO zcl_demo_abap.
    METHODS multiply_by IMPORTING num        TYPE numeric
                        RETURNING VALUE(ref) TYPE REF TO zcl_demo_abap.
    METHODS divide_by IMPORTING num        TYPE numeric
                      RETURNING VALUE(ref) TYPE REF TO zcl_demo_abap.
    METHODS get_result RETURNING VALUE(result) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA number TYPE decfloat34.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------*
*& Integer types: i, int8
*&---------------------------------------------------------------------*

    DATA i_a TYPE i.
    i_a = 2.
    DATA i_b TYPE i VALUE 1.
    "Inline declaration of the elementary type i
    "The VALUE operator only works for elementary types to create initial values.
    "So, the following example basically corresponds to the declaration of i_a.
    DATA(i_c) = VALUE i( ).
    "Integer numbers of type i can be specified directly in the program as numeric literals
    "i_d is automatically typed with type i.
    DATA(i_d) = 3.
    "Immutable variable
    FINAL(i_e) = 4.
    "Using the CONV operator to explicitly convert a value to the type specified.
    "Note: If '4' is not a character literal but a numeric literal like 4 (a numeric literal
    "of type i), a syntax warning will appear because the conversion is unnecessary.
    DATA(i_f) = CONV i( '4' ).

    DATA int8_a TYPE int8.
    DATA int8_b TYPE int8 VALUE 1.
    DATA(int8_c) = VALUE int8( ).
    DATA(int8_d) = CONV int8( 2 ).

*&---------------------------------------------------------------------*
*& Packed numbers of type p
*&---------------------------------------------------------------------*

    "The values cannot be specified directly. Character literals must be used whose content
    "can be interpreted as a packed number.

    DATA p_d0 TYPE p LENGTH 16 VALUE '9639093946237986229144508806129-'.
    DATA p_d1 TYPE p LENGTH 16 DECIMALS 1 VALUE '90854421485579917885733186954.8-'.
    DATA p_d2 TYPE p LENGTH 16 DECIMALS 2 VALUE '8915050542191264935993694231.83-'.
    DATA p_d3 TYPE p LENGTH 16 DECIMALS 3 VALUE '9506074954243163776128965613.326'.
    DATA p_d4 TYPE p LENGTH 16 DECIMALS 4 VALUE '160413057636273347593653626.2245-'.
    DATA p_d5 TYPE p LENGTH 16 DECIMALS 5 VALUE '40186196674188340559227708.43703-'.
    DATA p_d6 TYPE p LENGTH 16 DECIMALS 6 VALUE '4561675521638812947212485.501355-'.
    DATA p_d7 TYPE p LENGTH 16 DECIMALS 7 VALUE '814721091997334159140156.5732143'.
    DATA p_d8 TYPE p LENGTH 16 DECIMALS 8 VALUE '22204917587256970330176.81515431-'.
    DATA p_d9 TYPE p LENGTH 16 DECIMALS 9 VALUE '5185489996588937351174.098863656'.
    DATA p_d10 TYPE p LENGTH 16 DECIMALS 10 VALUE '446453616374633295560.0486661184-'.
    DATA p_d11 TYPE p LENGTH 16 DECIMALS 11 VALUE '54292568700375612882.85671756677'.
    DATA p_d12 TYPE p LENGTH 16 DECIMALS 12 VALUE '6692515911726313748.455325155554'.
    DATA p_d13 TYPE p LENGTH 16 DECIMALS 13 VALUE '278093280456662008.1493227920861'.
    DATA p_d14 TYPE p LENGTH 16 DECIMALS 14 VALUE '62092769140182925.23049016628459'.

    "Note: The length specification means data objects of type p can be 1 to 16 bytes long.
    "Two places are packed in each byte. The last byte is packed with the plus/minus sign.
    TYPES ty_p_l8d2 TYPE p LENGTH 8 DECIMALS 2.
    DATA p_l8d2_a TYPE ty_p_l8d2.

    "A packed number consists of the length multiplied by 2 minus 1 digits
    "I.e., in the case of the example, 15 digits, of which 2 are reserved for the decimal
    "places, are possible.
    p_l8d2_a = '1234567890123.99-'.
    p_l8d2_a = '1234567890123.99+'.
    p_l8d2_a = '1234567890123'.
    p_l8d2_a = '1234567890123.00'.
    p_l8d2_a = '1234567890123.00+'.
    "The following statements will show a syntax warning as the values are
    "non-admissible.
    "14 places before the period
    "p_l8d2_a = '12345678901234'.
    "More than two decimal places
    "p_l8d2_a = '1.234'.

    DATA(p_l8d2_b) = VALUE ty_p_l8d2( ).
    DATA(p_l8d2_c) = CONV ty_p_l8d2( '1.23' ).

*&---------------------------------------------------------------------*
*& Decimal floating point numbers of the types decfloat16 and decfloat34
*&---------------------------------------------------------------------*

    "16 decimal places
    DATA dec16 TYPE decfloat16 VALUE '0.7805874561940696'.

    "34 decimal places
    DATA dec34 TYPE decfloat34 VALUE '0.0587780463975441530508753423121495'.

*&---------------------------------------------------------------------*
*& Binary floating point numbers of type f
*&---------------------------------------------------------------------*

    DATA float TYPE f VALUE '2.1643779466775481E-01'.

    "Note: Internally, type f is represented as binary fractions and a dual exponent.
    "As a consequence, not every decimal number that is in the type's value range can
    "be exactly represented. As a consequence, this may lead to unexpected results
    "particulary in calculations and conversions.
    float = CONV f( '0.815' ).
    DATA(dec34_b) = CONV decfloat34( '0.815' ).

    ASSERT float <> dec34_b.

*&---------------------------------------------------------------------*
*& Generic numeric types
*&---------------------------------------------------------------------*

    "Generic types can be specified as types of field symbols and formal
    "parameters of procedures such as methods of classes.
    "The following examples use field symbols.

    "Generic type 'numeric'
    "It cCan accept all of the types above.
    FIELD-SYMBOLS <number> TYPE numeric.
    ASSIGN i_a TO <number>.
    ASSIGN int8_a TO <number>.
    ASSIGN p_d0 TO <number>.
    ASSIGN p_d14 TO <number>.
    ASSIGN dec16 TO <number>.
    ASSIGN dec34 TO <number>.
    ASSIGN float TO <number>.

    "Generic type 'p'
    "It can accept all packed number types.
    FIELD-SYMBOLS <p> TYPE p.
    ASSIGN p_d0 TO <p>.
    ASSIGN p_d1 TO <p>.
    ASSIGN p_d7 TO <p>.
    ASSIGN p_d14 TO <p>.

    "Generic type 'decfloat'
    "It can accept the types decfloat16 and decfloat34.
    FIELD-SYMBOLS <dec> TYPE decfloat.
    ASSIGN dec16 TO <dec>.
    ASSIGN dec34 TO <dec>.

*&---------------------------------------------------------------------*
*& Excursion: System classes
*&---------------------------------------------------------------------*

    "Various system classes are available that deal with numeric types
    "The following examples demonstrate:
    "- cl_abap_math: Mathematical utility library
    "- cl_abap_random*: Compute random numeric values
    "Check the class documentation and the methods available. The example only
    "covers a selection.

    "cl_abap_math: Among others, the class offers attributes to retrieve
    "the maximum and minimum possible values of different numeric types
    DATA(min_int4) = cl_abap_math=>min_int4.
    DATA(max_int4) = cl_abap_math=>max_int4.
    DATA(max_int1) = cl_abap_math=>max_int1.
    DATA(min_int1) = cl_abap_math=>min_int1.
    DATA(max_int2) = cl_abap_math=>max_int2.
    DATA(min_int2) = cl_abap_math=>min_int2.
    DATA(max_int8) = cl_abap_math=>max_int8.
    DATA(min_int8) = cl_abap_math=>min_int8.
    DATA(max_decfloat16) = cl_abap_math=>max_decfloat16.
    DATA(min_decfloat16) = cl_abap_math=>min_decfloat16.
    DATA(max_decfloat34) = cl_abap_math=>max_decfloat34.
    DATA(min_decfloat34) = cl_abap_math=>min_decfloat34.

    "cl_abap_random*
    "The following examples show random numeric value generation and value assignments.
    "Note that some 'create' methods also offer to specify minimum and maximum values for
    "the random number's value range.

    DATA(random_i) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                 min = 1
                                                 max = 100 )->get_next( ).
    DATA(random_int8) = cl_abap_random_int8=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_dec16) = cl_abap_random_decfloat16=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_dec34) = cl_abap_random_decfloat34=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_f) = cl_abap_random_float=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p) = cl_abap_random_packed=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d1) = cl_abap_random_packed_dec1=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d2) = cl_abap_random_packed_dec2=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d3) = cl_abap_random_packed_dec3=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d4) = cl_abap_random_packed_dec4=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d5) = cl_abap_random_packed_dec5=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d6) = cl_abap_random_packed_dec6=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d7) = cl_abap_random_packed_dec7=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d8) = cl_abap_random_packed_dec8=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d9) = cl_abap_random_packed_dec9=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d10) = cl_abap_random_packed_dec10=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d11) = cl_abap_random_packed_dec11=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d12) = cl_abap_random_packed_dec12=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d13) = cl_abap_random_packed_dec13=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(random_p_d14) = cl_abap_random_packed_dec14=>create( seed = cl_abap_random=>seed( ) )->get_next( ).

*&---------------------------------------------------------------------*
*& Notations
*&---------------------------------------------------------------------*

    "Mathematical notations
    "String of digits, a maximum of one period as decimal separator,
    "optional + or - sign on the left (which can be separated by the digits by blanks)
    DATA(math_notation) = `- 123.45`.

    "Commercial notation
    "String of digits, a maximum of one period as decimal separator,
    "optional + or - sign on the right (which can be separated by the digits by blanks)
    DATA(comm_notation) = `123.45-`.

    "Scientific notation
    "String of digits, mantissa can include an optional + or -, a period as decimal separator,
    "character e or E, and an exponent (which can optionally include a + or minus sign and further digits)
    DATA(sci_notation) = `-1.23456E03`.

    "It depends on the type, which notation is possible.
    DATA dec16_n TYPE decfloat16.
    DATA dec34_n TYPE decfloat34.
    DATA i_n TYPE i.
    DATA int8_n TYPE int8.
    DATA f_n TYPE f.
    DATA p_l16dec14 TYPE p LENGTH 16 DECIMALS 14.

    "decfloat16/decfloat34: All are possible
    dec16_n = math_notation.
    dec16_n = comm_notation.
    dec16_n = sci_notation.
    dec34_n = math_notation.
    dec34_n = comm_notation.
    dec34_n = sci_notation.

    "f: All are possible (but the signs must not be separated by blanks)
    TRY.
        f_n = math_notation.
      CATCH cx_sy_conversion_no_number INTO DATA(e_f_math_notation).
        DATA(e_f_math_notation_text) = e_f_math_notation->get_text( ).
    ENDTRY.

    DATA(math_notation_no_blanks) = `-123.45`.
    f_n = math_notation_no_blanks.

    f_n = comm_notation.
    f_n = sci_notation.

    "p, i, int8: Only mathematical and commercial notatations are allowed
    i_n = math_notation.
    i_n = comm_notation.

    TRY.
        i_n = sci_notation.
      CATCH cx_sy_conversion_no_number INTO DATA(e_i_sci_notation).
        DATA(e_i_sci_notation_text) = e_i_sci_notation->get_text( ).
    ENDTRY.

    int8_n = math_notation.
    int8_n = comm_notation.

    TRY.
        int8_n = sci_notation.
      CATCH cx_sy_conversion_no_number INTO DATA(e_int8_sci_notation).
        DATA(e_int8_sci_notation_text) = e_int8_sci_notation->get_text( ).
    ENDTRY.

    p_l16dec14 = math_notation.
    p_l16dec14 = comm_notation.

    TRY.
        p_l16dec14 = sci_notation.
      CATCH cx_sy_conversion_no_number INTO DATA(e_p_sci_notation).
        DATA(e_p_sci_notation_text) = e_p_sci_notation->get_text( ).
    ENDTRY.

    "Using the EXACT operator to check if there is a lossless assignment.
    "The example shows assignments to a data object of type decfloat34
    "that accepts all variants of notations.
    dec34_n = EXACT #( `9                      ` ).
    dec34_n = EXACT #( `9999                   ` ).
    dec34_n = EXACT #( `-9                     ` ).
    dec34_n = EXACT #( `- 99                   ` ).
    dec34_n = EXACT #( `-9999                  ` ).
    dec34_n = EXACT #( `9-                     ` ).
    dec34_n = EXACT #( `9999-                  ` ).
    dec34_n = EXACT #( `9999999 -              ` ).
    dec34_n = EXACT #( `9.99                   ` ).
    dec34_n = EXACT #( `.99                    ` ).
    dec34_n = EXACT #( `-.99                   ` ).
    dec34_n = EXACT #( `0.99999-               ` ).
    dec34_n = EXACT #( `9E3                    ` ).
    dec34_n = EXACT #( `-9E-3                  ` ).
    dec34_n = EXACT #( `.9E-3                  ` ).

*&---------------------------------------------------------------------*
*& Data objects in arithmetic expressions
*&---------------------------------------------------------------------*

    DATA result TYPE i.

    "Numeric literals (of type i)
    result = 1 + 1.
    ASSERT result = 2.

    "Literals/unnamed data objects (here, it is a character literal of
    "type c length 1, which has a numeric value and can be converted)
    result = 1 + '2'.
    ASSERT result = 3.

    "Named data objects
    DATA num TYPE i VALUE 3.
    "Character-like data object whose content can be converted to a numeric value
    DATA str TYPE string VALUE `7      `.

    result = num + str.
    ASSERT result = 10.

    "Numeric functions
    result = 1 + ipow( base = 2 exp = 3 ).
    ASSERT result = 9.

    "Numeric result of functional method call
    "The method call returns a random integer from 1 - 10
    result = 1 + cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                             min  = 1
                                             max  = 10 )->get_next( ).
    ASSERT result BETWEEN 2 AND 11.

    "Content of data object not convertible to numeric value
    TRY.
        result = 1 + '#'.
      CATCH cx_sy_conversion_no_number INTO DATA(error).
        DATA(error_text) = error->get_text( ).
    ENDTRY.

    "Avoid non-convertible operands
    "The following statement raises a shortdump
*    DATA dref TYPE REF TO data.
*    dref = NEW utclong( utclong_current( ) ).
*
*    result = 1 + dref->*.

*&---------------------------------------------------------------------*
*& Arithmetic expressions
*&---------------------------------------------------------------------*

    "The following code examples demonstrate basic arithmetic expressions
    "where all operands and the result are of type i.

    DATA res TYPE i.

    "Basic calculations using the operators +, -, *, /
    res = 1 + 2.
    res = 10 - 8.
    res = 5 * -5.
    res = 18 / 2.

    "DIV operator
    "Integer part of the division of the left operand by the right, with positive remainder
    res = 7 DIV 3.
    res = 4 DIV 5.
    res = 5 DIV 5.

    "MOD operator
    "Positive remainder of the division of the left operand by the right
    res = 7 MOD 3.
    res = 4 MOD 5.
    res = 5 MOD 5.

    "** operator: Left operand raised to the power of the right
    "Here, special rules apply concerning the calculation type. The following
    "examples use inline declarations for the result. That means, the resulting data object
    "receives the calculation type. In this case, the type is decfloat34 if one type involved is
    "decfloat34. In other cases, it is type f. The example includes RTTI methods to retrieve
    "type information. Refer to the cl_abap_typedescr class for the constant values. E.g., 'F'
    "stands for floating point numbers.
    "Note: As an alternative, you can use the ipow function to avoid the creation of the type f.
    DATA(power_result1) = 2 ** 5.
    ASSERT cl_abap_typedescr=>describe_by_data( power_result1 )->type_kind = 'F'.
    DATA(power_result2) = CONV decfloat34( '2' ) ** 5.
    ASSERT cl_abap_typedescr=>describe_by_data( power_result2 )->type_kind = 'e'.

    "Multiple operands specified on the right side
    "Note that calculation rules apply regarding prioritized calculations.
    res = 9 + 8 - 7.
    res = 1 + 1 * 3.
    res = 2 + 2 - -2 * 2 / 2 + 4.

    "Using parentheses
    "Arithmetic expressions in parentheses are calculated first
    res = 3 + 3 / 3.
    res = ( 3 + 3 ) / 3.

    "Calculations raising exceptions
    "The following examples show a selection of potential calculation errors
    "such as zero division and overflow (i.e. if the resulting value is beyond the
    "value range of a type)

    "Zero division
    TRY.
        res = 1 / 0.
      CATCH cx_sy_zerodivide INTO DATA(e1).
        DATA(e1_text) = e1->get_text( ).
    ENDTRY.

    "Note: Division by 0 does not raise an exception if the dividend is also 0.
    "Then, the result is 0.
    TRY.
        res = 0 / 0.
      CATCH cx_sy_zerodivide INTO DATA(e2).
        DATA(e2_text) = e2->get_text( ).
    ENDTRY.
    ASSERT e2 IS INITIAL.

    "Overflow
    "The example uses the maximum value of type i, and tries to add 1 to this
    "value.
    TRY.
        res = 2147483647 + 1.
      CATCH cx_sy_arithmetic_overflow INTO DATA(e3).
        DATA(e3_text) = e3->get_text( ).
    ENDTRY.

    TRY.
        res = 999999 * 999999.
      CATCH cx_sy_arithmetic_overflow INTO DATA(e4).
        DATA(e4_text) = e4->get_text( ).
    ENDTRY.

    "Note the pitfalls when performing calculations regarding numeric types and
    "conversion/rounding.
    "The examples use integer values, especially the result.
    res = 1 / 3.
    ASSERT res = 0.

    res = 2 / 3.
    ASSERT res = 1.

    res = 3 * ( 1 / 3 ).
    ASSERT res = 0.

    res = 3 * ( 2 / 3 ).
    ASSERT res = 3.

*&---------------------------------------------------------------------*
*& Calculation type
*&---------------------------------------------------------------------*

    "The following code snippets deal with the calculation type, which can be regarded as
    "property of arithmetic expressions and depends on the numeric data types
    "involved in the calculation.
    "The calculation type is determined at runtime.
    "Specific rules are determined by the calculation type.
    "If the numeric types of the operands of the arithmetic expression can be
    "determined statically, the calculation type is determined on the basis of
    "specific rules. The examples use inline declarations for the result.
    "In such cases, the target data object's type is derived from the calculation type.
    "The rules apply in the following decreasing priority order.
    "Many examples use RTTI methods to retrieve type kind information to check the
    "calculation type. For the type kind values, check the constants in class cl_abap_typedescr.

    "---- 1 ----
    "The calculation type is a decfloat type if one of the types involved is decfloat34
    "or decfloat16. If decfloat34 is involved, it is decfloat34.

    "i + decfloat34
    DATA(res_dec_a) = 1 + CONV decfloat34( '1.1' ).
    ASSERT cl_abap_typedescr=>describe_by_data( res_dec_a )->type_kind = 'e'.

    "decfloat16 + decfloat34
    DATA(res_dec_b) = CONV decfloat16( '2.2' ) + CONV decfloat34( '1.1' ).
    ASSERT cl_abap_typedescr=>describe_by_data( res_dec_b )->type_kind = 'e'.

    "decfloat16 + i
    DATA(res_dec_c) = CONV decfloat16( '2.2' ) + 5.
    ASSERT cl_abap_typedescr=>describe_by_data( res_dec_c )->type_kind = 'a'.

    "decfloat16 + int8
    DATA(res_dec_d) = CONV decfloat16( '2.2' ) + CONV int8( 2 ).
    ASSERT cl_abap_typedescr=>describe_by_data( res_dec_d )->type_kind = 'a'.

    "decfloat34 + p
    DATA pd3 TYPE p LENGTH 16 DECIMALS 3 VALUE '1.326'.
    DATA(res_dec_e) = CONV decfloat34( '2.2' ) + pd3.
    ASSERT cl_abap_typedescr=>describe_by_data( res_dec_e )->type_kind = 'e'.

    "decfloat16 + p
    DATA(res_dec_f) = CONV decfloat16( '2.2' ) + pd3.
    ASSERT cl_abap_typedescr=>describe_by_data( res_dec_f )->type_kind = 'a'.

    "decfloat34 + f
    DATA(res_dec_g) = CONV decfloat34( '2.2' ) + CONV f( '2.23645E-01' ).
    ASSERT cl_abap_typedescr=>describe_by_data( res_dec_g )->type_kind = 'e'.

    "decfloat16 + f
    DATA(res_dec_h) = CONV decfloat16( '2.2' ) + CONV f( '2.23645E-01' ).
    ASSERT cl_abap_typedescr=>describe_by_data( res_dec_h )->type_kind = 'a'.

    "---- 2 ----
    "The calculation type is f if one of the types involved is f.

    "f + i
    DATA(res_f_a) = 1 + CONV f( '2.23645E-01' ).
    ASSERT cl_abap_typedescr=>describe_by_data( res_f_a )->type_kind = 'F'.
    "f + int8
    DATA(res_f_b) = CONV f( '2.23645E-01' ) + CONV int8( 2 ).
    ASSERT cl_abap_typedescr=>describe_by_data( res_f_b )->type_kind = 'F'.
    "f + p
    DATA(res_f_c) = 1 + CONV f( '2.23645E-01' ).
    ASSERT cl_abap_typedescr=>describe_by_data( res_f_c )->type_kind = 'F'.

    "f + decfloat34
    "Note: Due to the higher priority of decfloat34, the calculation type and the
    "result are of type decfloat34.
    DATA(res_f_d) = CONV decfloat34( '1.1' ) + CONV f( '2.23645E-01' ).
    ASSERT cl_abap_typedescr=>describe_by_data( res_dec_e )->type_kind = 'e'.

    "f + decfloat16
    DATA(res_f_e) = CONV decfloat16( '1.1' ) + CONV f( '2.23645E-01' ).
    ASSERT cl_abap_typedescr=>describe_by_data( res_f_e )->type_kind = 'a'.

    "---- 3 ----
    "The calculation type is p if one of the data types involved is p.
    DATA p_l16d3 TYPE p LENGTH 16 DECIMALS 3 VALUE '1.234'.

    "Note: For the result of a computation with type p, the type p length 8 with 0 decimals is used
    "implicitly. It is not possible to determine the required length and decimal places.
    "Therefore, inline declarations should be avoided for such calculations.
    "Specifying ##TYPE suppresses a syntax warning.

    "p + i
    DATA(res_p_a) = p_l16d3 + 1 ##TYPE.
    ASSERT cl_abap_typedescr=>describe_by_data( res_p_a )->type_kind = 'P'.
    ASSERT cl_abap_typedescr=>describe_by_data( res_p_a )->length = 8.
    ASSERT cl_abap_typedescr=>describe_by_data( res_p_a )->decimals = 0.

    "p + int8
    DATA(res_p_b) = p_l16d3 + CONV int8( 1 ) ##TYPE.
    ASSERT cl_abap_typedescr=>describe_by_data( res_p_b )->type_kind = 'P'.
    ASSERT cl_abap_typedescr=>describe_by_data( res_p_b )->length = 8.
    ASSERT cl_abap_typedescr=>describe_by_data( res_p_b )->decimals = 0.

    "---- 4 ----
    "The calculation type is int8 if one of the data types involved is int8.

    DATA(res_int8_a) = CONV int8( 2 ) + 2.
    ASSERT cl_abap_typedescr=>describe_by_data( res_int8_a )->type_kind = '8'.

    "---- 5 ----
    "The calculation type is i if one of the data types involved is i.

    DATA(res_i_a) = 2 + 5.
    ASSERT cl_abap_typedescr=>describe_by_data( res_i_a )->type_kind = 'I'.

    "Standard type is used in case of generally typed operands
    FIELD-SYMBOLS <num> TYPE numeric.
    ASSIGN pd3 TO <num>.
    DATA(res_gen_a) = <num> + 1.
    DATA(t_kind_a) = cl_abap_typedescr=>describe_by_data( res_gen_a )->type_kind.

    ASSIGN res_i_a TO <num>.
    DATA(res_gen_b) = <num> + 1.
    DATA(t_kind_b) = cl_abap_typedescr=>describe_by_data( res_gen_b )->type_kind.

    ASSIGN res_f_a TO <num>.
    DATA(res_gen_c) = <num> + 1.
    DATA(t_kind_c) = cl_abap_typedescr=>describe_by_data( res_gen_c )->type_kind.

    ASSERT t_kind_a = t_kind_b.
    ASSERT t_kind_b = t_kind_c.

*&---------------------------------------------------------------------*
*& Calculation assignments
*&---------------------------------------------------------------------*

    DATA int_calc TYPE i.

    int_calc = 1.
    int_calc = int_calc + 1.
    int_calc += 1.

    int_calc = 10.
    int_calc = int_calc - 1.
    int_calc -= 1.

    int_calc = 5.
    int_calc = int_calc * 2.
    int_calc *= 3.

    int_calc = 100.
    int_calc = int_calc / 2.
    int_calc /= 2.

    "Example that uses the fluent interface pattern
    "The method implementations include calculation assignments.
    DATA(calc1) = create_calculation( 1 )->plus( 2 )->get_result( ).
    DATA(calc2) = create_calculation( 1 )->minus( 2 )->get_result( ).
    DATA(calc3) = create_calculation( 5 )->plus( 2 )->minus( 1 )->multiply_by( 3 )->get_result( ).
    DATA(calc4) = create_calculation( 10 )->multiply_by( 10 )->divide_by( 2 )->get_result( ).
    DATA(calc5) = create_calculation( 0 )->plus( 1 )->divide_by( 5 )->get_result( ).
    DATA(calc6) = create_calculation( '1.2' )->plus( '1.4' )->minus( '0.1' )->multiply_by( '2.5' )->divide_by( 2 )->get_result( ).

    "Arithmetic errors are just ignored in the example
    DATA(calc7) = create_calculation( 1 )->divide_by( 0 )->plus( 1 )->get_result( ).

*&---------------------------------------------------------------------*
*& Lossless assignments and calculations
*&---------------------------------------------------------------------*

    "Lossless assignments
    TYPES ty_pl8d3 TYPE p LENGTH 8 DECIMALS 3.

    "The following examples show character literals whose content
    "is assigned to the target data objects. Using the EXACT operator,
    "it is checked if there is a lossless assignment. The example
    "values are ok, and an exact move is possible.
    DATA(p_a) = EXACT ty_pl8d3( '123.987' ).
    DATA(p_b) = EXACT ty_pl8d3( '123.98' ).

    "The following example raises an exception. The example character literal
    "has four decimal places. Thus, a rounding is necessary, which means that
    "a lossless assignment is not possible. Catching the CX_SY_CONVERSION_ROUNDING
    "exception is necessary.
    "DATA(p_c) = EXACT ty_p_l8d3( '123.9876' ).

    TRY.
        DATA(p_d) = EXACT ty_pl8d3( '123.9876' ).
      CATCH cx_sy_conversion_rounding INTO DATA(error_p_rounding).
        DATA(error_p_rounding_text) = error_p_rounding->get_text( ).
    ENDTRY.

    "Lossless calculations using arithmetic expressions
    "The first statement works, whereas the second statement raises an exception.
    "A rounding to two decimal places is required.
    TYPES ty_pl8d2 TYPE p LENGTH 8 DECIMALS 2.
    DATA(p_e) = EXACT ty_pl8d2( 1 / 4 ).
    "DATA(p_f) = EXACT ty_pl8d2( 1 / 3 ).

    "Catching exceptions when rounding in lossless calculations
    TRY.
        DATA(p_g) = EXACT ty_pl8d2( 1 / 3 ).
      CATCH cx_sy_conversion_rounding INTO error_p_rounding.
        error_p_rounding_text = error_p_rounding->get_text( ).
    ENDTRY.

*&---------------------------------------------------------------------*
*& Numeric functions
*&---------------------------------------------------------------------*

    "----------- abs: Returning the absolute value -----------
    DATA(abs1) = abs( CONV decfloat34( '-4.756' ) ).
    DATA(abs2) = abs( -4 ).

    "----------- sign: Evaluating the sign -----------
    "-1 if negative, 0 if 0, 1 if positive
    DATA(sign1) = sign( -789 ).
    DATA(sign2) = sign( 5 - 5 ).
    DATA(sign3) = sign( -5 * -5 ).

    "----- ceil: smallest integer not less than the value specified -----
    DATA(ceil1) = ceil( CONV decfloat34( '4.999' ) ).
    DATA(ceil2) = ceil( CONV decfloat34( '4.001' ) ).
    DATA(ceil3) = ceil( CONV decfloat34( '-4.999' ) ).
    DATA(ceil4) = ceil( CONV decfloat34( '-4.001' ) ).

    "----- floor: largest integer not less than the value specified -----
    DATA(floor1) = floor( CONV decfloat34( '4.999' ) ).
    DATA(floor2) = floor( CONV decfloat34( '4.001' ) ).
    DATA(floor3) = floor( CONV decfloat34( '-4.999' ) ).
    DATA(floor4) = floor( CONV decfloat34( '-4.001' ) ).

    "------------- trunc: integer part -------------
    DATA(trunc1) = trunc( CONV decfloat34( '4.999' ) ).
    DATA(trunc2) = trunc( CONV decfloat34( '4.001' ) ).
    DATA(trunc3) = trunc( CONV decfloat34( '-4.999' ) ).
    DATA(trunc4) = trunc( CONV decfloat34( '-4.001' ) ).

    "------------- frac: decimal places -------------
    DATA(frac1) = frac( CONV decfloat34( '4.999' ) ).
    DATA(frac2) = frac( CONV decfloat34( '4.001' ) ).
    DATA(frac3) = frac( CONV decfloat34( '-4.999' ) ).
    DATA(frac4) = frac( CONV decfloat34( '-4.001' ) ).

    "------------- ipow: Calculalting the power -------------
    DATA(ipow1) = ipow( base = 2 exp = 3 ).
    DATA(ipow2) = ipow( base = 10 exp = 0 ).

    "Exception is raised
    TRY.
        DATA(ipow3) = ipow( base = 10 exp = 100 ).
      CATCH cx_sy_arithmetic_overflow.
    ENDTRY.

    "Numeric extremum functions that return the value of the largest
    "or smallest of the passed arguments.

    "A minimum of two, and a maximum of 9 arguments can be specified.
    "Numeric data objects and numeric expressions are possible
    DATA(nmin) =  nmin( val1 = CONV decfloat34( '1.34' )
                         val2 = CONV decfloat34( '56.7' )
                         val3 = CONV decfloat34( '890.123' )
                         val4 = CONV decfloat34( '0.999' ) ).

    DATA(nmax) =  nmax( val1 = CONV decfloat34( '1.34' )
                         val2 = CONV decfloat34( '56.7' )
                         val3 = CONV decfloat34( '890.123' )
                         val4 = CONV decfloat34( '0.999' ) ).

    "--- acos, asin, atan, cos, sin, tan, cosh, sinh, tanh, exp, log, log10, sqrt ---
    "The following examples cover a selection.

    "Calculating the square root
    DATA(sqrt1) = sqrt( CONV decfloat34( '9' ) ).
    DATA(sqrt2) = sqrt( CONV decfloat34( '40.96' ) ).

    "Calculating the logarithm to base 10
    DATA(log10) = log10( CONV decfloat34( '1000' ) ).
    DATA(sine) = sin( '30' ).
    DATA(cosine) = cos( '45' ).
    DATA(tangent) = tan( '90' ).

    "------------- round -------------
    "Rounding functions expect a decimal floating point number as argument.
    "The return value is of type decfloat34. The functions can be used to round
    "to decimal places and precisions. In addition, rounding rules can be specified.

    "Rounding to decimal places
    DATA(round1) = round( val = CONV decfloat34( '1.2374' ) dec = 2 ).
    DATA(round2) = round( val = CONV decfloat34( '1.2374' ) dec = 3 ).

    "Rounding to precision
    DATA(round3) = round( val = CONV decfloat34( '1234567890123' ) prec = 10 ).
    DATA(round4) = round( val = CONV decfloat34( '1234' ) prec = 3 ).

    "------------- rescale -------------
    "Similar to the round function, the dec (for scaling) or prec (for precision)
    "parameters must be specified. The input is rounded if required.
    DATA(rescale1) = rescale( val = CONV decfloat34( '1234.56789' ) dec = 0 ).
    DATA(rescale2) = rescale( val = CONV decfloat34( '1234.56789' ) dec = 1 ).
    DATA(rescale3) = rescale( val = CONV decfloat34( '1234.56789' ) prec = 3 ).
    DATA(rescale4) = rescale( val = CONV decfloat34( '1234.56789' ) prec = 10 ).

*&---------------------------------------------------------------------*
*& ABAP calculator
*&---------------------------------------------------------------------*

    "Calculations with type i
    "Method implementation using IF statements

    TYPES: BEGIN OF s_int,
             num1     TYPE i,
             operator TYPE enum_operator,
             num2     TYPE i,
           END OF s_int,
           t_int TYPE TABLE OF s_int WITH EMPTY KEY.

    DATA(i_calculation_tab) = VALUE t_int(
      ( num1 = 1 operator = add num2 = 5 )
      ( num1 = -10 operator = add num2 = 7 )
      ( num1 = 8 operator = subtract num2 = 20 )
      ( num1 = 1000 operator = subtract num2 = 999 )
      ( num1 = 5 operator = multiply num2 = 5 )
      ( num1 = 10 operator = multiply num2 = 10 )
      ( num1 = 10 operator = divide num2 = 5 )
      ( num1 = 3 operator = divide num2 = 2 )
      "Failing calculations
      ( num1 = 1 operator = divide num2 = 0 )
      ( num1 = 0 operator = divide num2 = 0 )
      ( num1 = 2147483647 operator = add num2 = 1 )
      ( num1 = 99999999 operator = multiply num2 = 99999999 ) ).

    DATA calc_results TYPE string_table.

    LOOP AT i_calculation_tab INTO DATA(cal_i).
      TRY.
          DATA(result_i) = calculate_i( num1 = cal_i-num1 operator = cal_i-operator num2 = cal_i-num2 ).
          APPEND |{ cal_i-num1 } { SWITCH #( cal_i-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_i-num2 } = { result_i STYLE = SIMPLE }| TO calc_results.
        CATCH cx_sy_arithmetic_error INTO DATA(error_cal_i).
          APPEND |{ cal_i-num1 } { SWITCH #( cal_i-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_i-num2 } = ERROR! { error_cal_i->get_text( ) }| TO calc_results.
      ENDTRY.
    ENDLOOP.

    out->write( |----------- Calculations with type i -----------| ).
    out->write( calc_results ).
    CLEAR calc_results.


    "Calculations with type decfloat34
    "Method implementation using CASE statements

    TYPES: BEGIN OF s_decfloat34,
             num1     TYPE decfloat34,
             operator TYPE enum_operator,
             num2     TYPE decfloat34,
           END OF s_decfloat34,
           t_decfloat34 TYPE TABLE OF s_decfloat34 WITH EMPTY KEY.

    DATA(decfloat34_calculation_tab) = VALUE t_decfloat34(
      ( num1 = CONV decfloat34( '1.4' ) operator = add num2 = CONV decfloat34( '0.7' ) )
      ( num1 = CONV decfloat34( '0.576' ) operator = subtract num2 = CONV decfloat34( '0.924' ) )
      ( num1 = CONV decfloat34( '9283658473562' ) operator = multiply num2 = CONV decfloat34( '23423' ) )
      ( num1 = CONV decfloat34( '984.2435' ) operator = divide num2 = CONV decfloat34( '468.2346' ) )
      "Failing calculations
      ( num1 = CONV decfloat34( '1.0' ) operator = divide num2 = CONV decfloat34( '0.0' ) )
      ( num1 = CONV decfloat34( '0' ) operator = divide num2 = CONV decfloat34( '0' ) )
      ( num1 = cl_abap_math=>max_decfloat34 operator = multiply num2 = CONV decfloat34( '2' ) ) ).

    LOOP AT decfloat34_calculation_tab INTO DATA(cal_decfloat34).
      TRY.
          DATA(res_decfloat34) = calculate_decfloat34( num1 = cal_decfloat34-num1 operator = cal_decfloat34-operator num2 = cal_decfloat34-num2 ).
          APPEND |{ cal_decfloat34-num1 } { SWITCH #( cal_decfloat34-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_decfloat34-num2 } = { res_decfloat34 STYLE = SIMPLE }| TO calc_results.
        CATCH cx_sy_arithmetic_error INTO DATA(error_cal_decfloat34).
          APPEND |{ cal_decfloat34-num1 } { SWITCH #( cal_decfloat34-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_decfloat34-num2 } = ERROR! { error_cal_decfloat34->get_text( ) }| TO calc_results.
      ENDTRY.
    ENDLOOP.

    out->write( |\n| ).
    out->write( |----------- Calculations with type decfloat34 -----------| ).
    out->write( calc_results ).
    CLEAR calc_results.

    "Calculations with random numeric types
    "Method implementation using the COND operator

    TYPES: BEGIN OF s_numbers,
             num1     TYPE REF TO data,
             operator TYPE enum_operator,
             num2     TYPE REF TO data,
           END OF s_numbers,
           t_numbers TYPE TABLE OF s_numbers WITH EMPTY KEY.

    TYPES p_d3 TYPE p LENGTH 16 DECIMALS 3.

    DATA(numeric_calculation_tab) = VALUE t_numbers(
       ( num1 = NEW decfloat34( '1.4' ) operator = add num2 = NEW i( 7 ) )
       ( num1 = NEW decfloat16( '0.123454' ) operator = add num2 = NEW decfloat34( '0.7483265' ) )
       ( num1 = NEW p_d3( '123.456' ) operator = subtract num2 = NEW i( 5 ) )
       ( num1 = NEW f( '2.1643779466775481E-01' ) operator = subtract num2 = NEW decfloat16( '1.23' ) )
       ( num1 = NEW i( 100 ) operator = multiply num2 = NEW i( 50 ) )
       ( num1 = NEW f( '1.2' ) operator = multiply num2 = NEW i( 5 ) )
       ( num1 = NEW decfloat34( '5.4' ) operator = divide num2 = NEW p_d3( '1.123' ) )
       ( num1 = NEW i( 999 ) operator = divide num2 = NEW decfloat16( '111' ) )
       "Failing calculations
       ( num1 = NEW i( 1 ) operator = divide num2 = NEW decfloat34( '0' ) )
       ( num1 = NEW p_d3( '0' ) operator = divide num2 = NEW p_d3( '0' ) )
       ( num1 = NEW decfloat34( cl_abap_math=>max_decfloat34 ) operator = multiply num2 = NEW decfloat34( '2' ) ) ).

    LOOP AT numeric_calculation_tab INTO DATA(cal_numeric).
      TRY.
          DATA(res_numeric) = calculate_numeric_cond( num1 = cal_numeric-num1->* operator = cal_numeric-operator num2 = cal_numeric-num2->* ).
          APPEND |{ cal_numeric-num1->* } { SWITCH #( cal_numeric-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_numeric-num2->* } = { res_numeric STYLE = SIMPLE }| TO calc_results.
        CATCH cx_sy_arithmetic_error INTO DATA(error_cal_numeric).
          APPEND |{ cal_numeric-num1->* } { SWITCH #( cal_numeric-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_numeric-num2->* } = ERROR! { error_cal_numeric->get_text( ) }| TO calc_results.
      ENDTRY.
    ENDLOOP.

    out->write( |\n| ).
    out->write( |----------- Calculations with random numeric types (COND operator) -----------| ).
    out->write( calc_results ).
    CLEAR calc_results.

    "Calculations with random numeric types
    "Method implementation using the SWITCH operator

    numeric_calculation_tab = VALUE #(
       ( num1 = NEW decfloat34( '0.4' ) operator = add num2 = NEW i( 20 ) )
       ( num1 = NEW decfloat16( '0.827123454' ) operator = subtract num2 = NEW decfloat34( '1.89632' ) )
       ( num1 = NEW p_d3( '1.5' ) operator = multiply num2 = NEW f( '3.123' ) )
       ( num1 = NEW i( 4 ) operator = divide num2 = NEW p_d3( '1.2' ) )
       "Failing calculations
       ( num1 = NEW i( 1 ) operator = divide num2 = NEW decfloat34( '0' ) )
       ( num1 = NEW p_d3( '0' ) operator = divide num2 = NEW p_d3( '0' ) )
       ( num1 = NEW decfloat34( cl_abap_math=>max_decfloat34 ) operator = multiply num2 = NEW decfloat34( '2' ) ) ).

    LOOP AT numeric_calculation_tab INTO cal_numeric.
      TRY.
          res_numeric = calculate_numeric_cond( num1 = cal_numeric-num1->* operator = cal_numeric-operator num2 = cal_numeric-num2->* ).
          APPEND |{ cal_numeric-num1->* } { SWITCH #( cal_numeric-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_numeric-num2->* } = { res_numeric STYLE = SIMPLE }| TO calc_results.
        CATCH cx_sy_arithmetic_error INTO error_cal_numeric.
          APPEND |{ cal_numeric-num1->* } { SWITCH #( cal_numeric-operator WHEN add THEN '+' WHEN subtract THEN '-' WHEN multiply THEN '*' WHEN divide THEN '/' ) } { cal_numeric-num2->* } = ERROR! { error_cal_numeric->get_text( ) }| TO calc_results.
      ENDTRY.
    ENDLOOP.

    out->write( |\n| ).
    out->write( |----------- Calculations with random numeric types (SWITCH operator) -----------| ).
    out->write( calc_results ).

*&---------------------------------------------------------------------*
*& cl_abap_bigint
*&---------------------------------------------------------------------*

    "Factories
    DATA(bigint_int4) = cl_abap_bigint=>factory_from_int4( 10 ).
    DATA(bigint_int4c) = cl_abap_bigint=>factory_from_string( `283469208407283452340` ).
    DATA(bigint_int4d) = cl_abap_bigint=>factory_from_int8( CONV int8( 1234567890123456 ) ).

    DATA(a1) = cl_abap_bigint=>factory_from_int4( -10 )->to_external( ).
    DATA(a2) = cl_abap_bigint=>factory_from_int4( -10 )->to_external( iv_flg_minus_in_front = abap_true ).
    DATA(a3) = cl_abap_bigint=>factory_from_int4( 100 )->to_utf8( ).
    DATA(a4) = cl_abap_bigint=>factory_from_string( `123` )->to_df34( ).
    DATA(a5) = cl_abap_bigint=>factory_from_int4( -10 )->to_string( ).
    DATA(a6) = cl_abap_bigint=>factory_from_int4( 4 )->add( bigint_int4 )->to_string( ).
    DATA(a7) = cl_abap_bigint=>factory_from_int4( 7 )->add_int4( 2 )->to_string( ).
    DATA(a8) = cl_abap_bigint=>factory_from_int4( -10 )->abs( )->to_string( ).
    DATA(a9) = cl_abap_bigint=>factory_from_int4( 19 )->compare_int4( 20 ).
    DATA(a10) = cl_abap_bigint=>factory_from_int4( 100 )->compare( bigint_int4 ).
    DATA(a11) = cl_abap_bigint=>factory_from_int4( 20 )->div( bigint_int4 ).
    DATA(a12) = a11-quotient->to_string( ).
    DATA(a13) = a11-remainder->to_string( ).
    DATA(a14) = cl_abap_bigint=>factory_from_int4( 10 )->div_int4( 3 ).
    DATA(a15) = a14-quotient->to_string( ).
    DATA(a16) = a14-remainder.
    DATA(a17) = cl_abap_bigint=>factory_from_int4( 10 )->div_by_two_power( CONV int8( 2 ) )->to_string( ).
    DATA(a18) = cl_abap_bigint=>factory_from_int4( 5 )->div_to_df34( bigint_int4 ).
    DATA(a19) = cl_abap_bigint=>factory_from_int4( 50 )->gcd( bigint_int4 )->to_string( ).
    DATA(a20) = cl_abap_bigint=>factory_from_int4( 1000 )->get_number_of_bits( ).
    DATA(a21) = cl_abap_bigint=>factory_from_int4( 10 )->is_equal( bigint_int4 ).

    cl_abap_bigint=>factory_from_string( `123` )->is_int4(
      IMPORTING
        ev_int4_value  = DATA(a22)
      RECEIVING
        rv_flg_is_int4 = DATA(a23)
    ).
    DATA(a24) = cl_abap_bigint=>factory_from_int4( 11 )->is_larger( bigint_int4 ).
    DATA(a25) = cl_abap_bigint=>factory_from_int4( 10 )->is_larger_or_equal( bigint_int4 ).
    DATA(a26) = cl_abap_bigint=>factory_from_int4( -10 )->is_negative( ).
    DATA(a27) = cl_abap_bigint=>factory_from_int4( 0 )->is_zero( ).
    DATA(a28) = cl_abap_bigint=>factory_from_int4( 123 )->mod( bigint_int4 )->to_string( ).
    DATA(a29) = cl_abap_bigint=>factory_from_int4( 10 )->mod_int4( 3 ).
    DATA(a30) = cl_abap_bigint=>factory_from_int4( 10 )->mul( bigint_int4 )->to_string( ).
    DATA(a31) = cl_abap_bigint=>factory_from_int4( 5 )->mul_by_two_power( 2 )->to_string( ).
    DATA(a32) = cl_abap_bigint=>factory_from_int4( 2 )->mul_int4( 5 )->to_string( ).
    DATA(a33) = cl_abap_bigint=>factory_from_int4( 8 )->pow( 2 )->to_string( ).
    DATA(a34) = cl_abap_bigint=>factory_from_int4( 9 )->sqrt( )->to_string( ).
    DATA(a35) = cl_abap_bigint=>factory_from_int4( 18 )->sub( bigint_int4 )->to_string( ).
    DATA(a36) = cl_abap_bigint=>factory_from_int4( 15 )->sub_int4( 9 )->to_string( ).
    "Cloning
    DATA(a37) = cl_abap_bigint=>factory_from_int4( 15 ).
    DATA(a38) = cl_abap_bigint=>factory_from_int4( 5 ).
    "Adding a number to another number to not get a new instance but the original instance
    DATA(a39) = a37->add( a38 ).
    ASSERT a39 = a37.
    DATA(a40) = a37->to_string( ).
    DATA(a41) = a39->to_string( ).
    DATA(a42) = cl_abap_bigint=>factory_from_int4( 15 ).
    DATA(a43) = cl_abap_bigint=>factory_from_int4( 5 ).

    DATA(a44) = a42->clone( )->add( a43 ).
    ASSERT a44 <> a42.
    DATA(a45) = a42->to_string( ).
    DATA(a46) = a44->to_string( ).
    DATA(a47) = cl_abap_bigint=>factory_from_int4( 15 )->sub_int4( 9 )->clone( )->to_string( ).

*&---------------------------------------------------------------------*
*& cl_abap_rational
*&---------------------------------------------------------------------*

    "Factories
    DATA(r1) = cl_abap_rational=>factory_from_string( `-1/3` ).
    DATA(r2) = cl_abap_rational=>factory_from_bigint( cl_abap_bigint=>factory_from_int4( 11 ) ).

    TYPES p_l16d5 TYPE p LENGTH 16 DECIMALS 5.
    DATA(r3) = cl_abap_rational=>factory_from_dec( CONV p_l16d5( '123456.789' ) ).
    DATA(r4) = cl_abap_rational=>factory_from_decimal_string( `1.234567890` ).
    DATA(r5) = cl_abap_rational=>factory_from_df34( `1.4` ).
    DATA(r6) = cl_abap_rational=>factory_from_int4( 100 ).
    DATA(r7) = cl_abap_rational=>factory_from_int8( CONV int8( 123 ) ).
    DATA(r8) = cl_abap_rational=>factory_from_string( `-2/3` )->add_int4( 1 )->to_df34( ).
    DATA(r9) = cl_abap_rational=>factory_from_string( `-2/3` )->add_int4( 1 )->to_string( ).
    DATA r10 TYPE p_l16d5.
    cl_abap_rational=>factory_from_string( `-2/3` )->add_int4( 1 )->to_dec( IMPORTING ev_decimal = r10 ).
    "Methods that include cl_abap_bigint instances
    DATA(r11) = cl_abap_rational=>factory_from_string( `-2/3` )->add_bigint( cl_abap_bigint=>factory_from_int4( 1 ) )->to_string( ).

*&---------------------------------------------------------------------*
*& Date calculations
*&---------------------------------------------------------------------*

    DATA date_calc_1 TYPE d VALUE '20240101'.
    DATA date_calc_2 TYPE d VALUE '20231227'.
    DATA date_calc_3 TYPE d VALUE '20231230'.
    DATA date_calc_4 TYPE d VALUE '20240220'.

    DATA(days_diff_1) = date_calc_1 - date_calc_2.
    DATA(days_diff_2) = date_calc_1 - date_calc_3.
    DATA(days_since_01_01_0001) = CONV i( date_calc_1 ).

    "------------ Performing date additions and subtractions using the XCO library ------------

    "Adding days to the current date using the 'add' method
    DATA(xco_date_add_5days) = xco_cp=>sy->date( )->add( iv_day = 5
                                                 )->as( xco_cp_time=>format->iso_8601_extended
                                                 )->value.

    "The 'add' method has various parameters, adding 1 day/month/year
    DATA(xco_date_add_1_mult) = xco_cp=>sy->date( )->add( iv_day = 1 iv_month = 1 iv_year = 1
                                                  )->as( xco_cp_time=>format->iso_8601_extended
                                                  )->value.

    DATA(xco_yesterday) = xco_cp=>sy->date( )->subtract( iv_day = 1
                                            )->as( xco_cp_time=>format->iso_8601_extended
                                            )->value.

    "Addition with a custom date
    DATA(xco_date_add_1day_custom) = xco_cp_time=>date( iv_year = '2024' iv_month = '02' iv_day = '28'
                                                      )->add( iv_day = 1
                                                      )->as( xco_cp_time=>format->iso_8601_extended
                                                      )->value.

    "Subtraction with a custom date
    DATA(xco_date_subtract_misc) = xco_cp_time=>date( iv_year = '2024' iv_month = '02' iv_day = '28'
                                                      )->add( iv_day = 1 iv_month = 1 iv_year = 1
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
      CATCH cx_root INTO DATA(error_inv_date1).
        DATA(error_inv_date1_text) = error_inv_date1->get_text( ).
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
      CATCH cx_root INTO DATA(error_inv_date2).
        DATA(error_inv_date2_text) = error_inv_date2->get_text( ).
    ENDTRY.

*&---------------------------------------------------------------------*
*& Time calculations
*&---------------------------------------------------------------------*

    "------------ Calculating the time delta between two time values ------------

    DATA: time1 TYPE t VALUE '210000',
          time2 TYPE t VALUE '040000'.

    "Calculating the time difference by subtracting
    "Note that the resulting data object is of type i.
    "The time values are automatically converted to type i.
    "Following the conversion rule of type t to i, the resulting value
    "is calculated as follows: format hhmmss of type t -> integer values
    "as a result of the calculation hh * 3600 + mm * 60 + ss.
    DATA(time_diff) = time2 - time1.
    DATA(time1_conv2i) = CONV i( time1 ).
    ASSERT time1_conv2i = ( 21 * 3600 ) + ( 00 * 60 ) + 00.
    DATA(time2_conv2i) = CONV i( time2 ).
    ASSERT time2_conv2i = ( 04 * 3600 ) + ( 00 * 60 ) + 00.
    ASSERT time2_conv2i - time1_conv2i = time_diff.

    "---- Calculating the total values of the time difference in seconds, minutes and hours ----
    "The MOD operator is used and works in a way that the positive remainder
    "of the division of the left operand by the right is returned. Therefore,
    "it is irrelevant whether the time difference value is either positive or
    "negative.
    "The value 86400 is used as right operand, representing the number of seconds
    "per day/24 hours.
    DATA(time_diff_seconds_total) = ( time2 - time1 ) MOD 86400.
    DATA(time_diff_minutes_total) = CONV decfloat34( ( ( time2 - time1 ) MOD 86400 ) / 60 ).
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

    diff_time(2) = hours.
    diff_time+2(2) = minutes.
    diff_time+4(2) = seconds.

    "More examples
    time1 = '225958'.
    time2 = '010001'.

    diff_time(2) = ( ( time2 - time1 ) MOD 86400 ) DIV 3600.
    diff_time+2(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) DIV 60.
    diff_time+4(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) MOD 60.

    time1 = '010001'.
    time2 = '225958'.

    diff_time(2) = ( ( time2 - time1 ) MOD 86400 ) DIV 3600.
    diff_time+2(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) DIV 60.
    diff_time+4(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) MOD 60.

    time1 = '132415'.
    time2 = '050757'.
    diff_time(2) = ( ( time2 - time1 ) MOD 86400 ) DIV 3600.
    diff_time+2(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) DIV 60.
    diff_time+4(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) MOD 60.

    time1 = '050757'.
    time2 = '132415'.

    diff_time(2) = ( ( time2 - time1 ) MOD 86400 ) DIV 3600.
    diff_time+2(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) DIV 60.
    diff_time+4(2) = ( ( ( time2 - time1 ) MOD 86400 ) MOD 3600 ) MOD 60.

    "---- Excursion: / and DIV operators (and why DIV is used above) ----
    time1 = '132415'.
    time2 = '050757'.

    "Compare the result of the following statements that use different operators
    DATA(hours_w_div) = ( ( time2 - time1 ) MOD 86400 ) DIV 3600.
    DATA(hours_no_div) = ( ( time2 - time1 ) MOD 86400 ) / 3600.
    DATA(hours_no_div_dec) = CONV decfloat34( ( ( time2 - time1 ) MOD 86400 ) / 3600 ).

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

    "Addition with a custom time
    DATA(time_xco_cust_add) = xco_cp_time=>time( iv_hour   = '12'
                                                 iv_minute = '34'
                                                 iv_second = '56'
                                               )->add( iv_hour   = 1
                                                       iv_minute = 2
                                                       iv_second = 3
                                               )->as( xco_cp_time=>format->iso_8601_extended
                                               )->value.

    "Subtraction with a custom time
    DATA(time_xco_cust_subtr) = xco_cp_time=>time( iv_hour   = '12'
                                                   iv_minute = '34'
                                                   iv_second = '56'
                                                 )->subtract( iv_hour   = 1
                                                              iv_minute = 2
                                                              iv_second = 3
                                                 )->as( xco_cp_time=>format->iso_8601_extended
                                                 )->value.

*&---------------------------------------------------------------------*
*& Time stamp calculations
*&---------------------------------------------------------------------*

    "Time stamp calculations with the built-in function utclong_add
    "With the built-in function utclong_add, at least one parameter must be specified
    "besides 'val'.
    "Note that there are no parameters for years and months.
    DATA(utc4calc) = CONV utclong( '2024-01-01 15:55:14.1173220' ).

    "Adding one hour
    DATA(ts1) = utclong_add( val = utc4calc
                             hours = 1 ).

    "Subtracting one hour by passing a negative integer value (no separate substract
    "function available)
    DATA(ts2) = utclong_add( val = utc4calc
                             hours = -1 ).

    "Using all parameters
    DATA(ts3) = utclong_add( val = utc4calc
                             days = 1 "type i
                             hours = 2 "type i
                             minutes = CONV int8( '13' )
                             seconds = CONV decfloat34( '53.12' ) ).

    "Using XCO
    "Addition with the current time stamp
    DATA(xco_ts_add) = xco_cp=>sy->moment( xco_cp_time=>time_zone->user
                                         )->add( iv_year        = 1
                                                 iv_month       = 2
                                                 iv_day         = 3
                                                 iv_hour        = 4
                                                 iv_minute      = 5
                                                 iv_second      = 6
                                         )->as( xco_cp_time=>format->iso_8601_extended
                                         )->value.

    "Subtraction with the current time stamp
    DATA(xco_ts_subtr) = xco_cp=>sy->moment( xco_cp_time=>time_zone->user
                                           )->subtract( iv_year        = 1
                                                        iv_month       = 2
                                                        iv_day         = 3
                                                        iv_hour        = 4
                                                        iv_minute      = 5
                                                        iv_second      = 6
                                           )->as( xco_cp_time=>format->iso_8601_extended
                                           )->value.

    "Addition with a custom time stamp
    DATA(xco_cust_ts_add) = xco_cp_time=>moment( iv_year   = '2025'
                                                 iv_month  = '01'
                                                 iv_day    = '15'
                                                 iv_hour   = '12'
                                                 iv_minute = '34'
                                                 iv_second = '56'
                                               )->add( iv_year        = 1
                                                       iv_month       = 2
                                                       iv_day         = 3
                                                       iv_hour        = 4
                                                       iv_minute      = 5
                                                       iv_second      = 6
                                               )->as( xco_cp_time=>format->iso_8601_extended
                                               )->value.

    "Subtraction with a custom time stamp
    DATA(xco_cust_ts_subtr) = xco_cp_time=>moment( iv_year   = '2025'
                                                   iv_month  = '01'
                                                   iv_day    = '15'
                                                   iv_hour   = '12'
                                                   iv_minute = '34'
                                                   iv_second = '56'
                                                 )->subtract( iv_year        = 1
                                                              iv_month       = 2
                                                              iv_day         = 3
                                                              iv_hour        = 4
                                                              iv_minute      = 5
                                                              iv_second      = 6
                                                 )->as( xco_cp_time=>format->iso_8601_extended
                                                 )->value.

    "Interval calculations
    DATA(xco_moment1) = xco_cp_time=>moment( iv_year       = '2025'
                                             iv_month  = '01'
                                             iv_day    = '15'
                                             iv_hour   = '12'
                                             iv_minute = '34'
                                             iv_second = '56' ).

    DATA(xco_moment2) = xco_cp_time=>moment( iv_year   = '2026'
                                             iv_month  = '02'
                                             iv_day    = '16'
                                             iv_hour   = '16'
                                             iv_minute = '38'
                                             iv_second = '52' ).

    DATA(xco_ts_interval) = xco_moment1->interval_to( xco_moment2 ).
    DATA(ts_interval_low) = xco_ts_interval->lower_bound->as( xco_cp_time=>format->iso_8601_extended )->value.
    DATA(ts_interval_high) = xco_ts_interval->upper_bound->as( xco_cp_time=>format->iso_8601_extended )->value.

    "Calculating time stamp differences using the built-in function utclong_diff

    DATA(ts4) = CONV utclong( '2024-01-01 05:30:00' ).
    DATA(ts5) = CONV utclong( '2024-01-01 06:30:00' ).
    "The return value has the type decfloat34. It contains the exact difference in seconds.

    DATA(ts_diff1) = utclong_diff( high = ts5
                                   low = ts4 ).

    DATA(ts_diff2) = utclong_diff( high = ts4
                                   low = ts5 ).

    DATA(low_timestamp) = CONV utclong( '2024-01-01 05:30:00' ).
    DATA(high_timestamp) = CONV utclong( '2024-01-03 10:35:12' ).

    "'diff' method: Calculating time differences
    cl_abap_utclong=>diff( EXPORTING high    = high_timestamp
                                     low     = low_timestamp
                           IMPORTING days    = DATA(diff_days)
                                     hours   = DATA(diff_hours)
                                     minutes = DATA(diff_minutes)
                                     seconds = DATA(diff_seconds) ).

    "CL_ABAP_TSTMP: Calculating and converting time stamps in packed numbers

    "Creating a time stamp of type timestamp
    GET TIME STAMP FIELD DATA(ts6).

    "Calculations for time stamps in packed numbers
    "Adding 1 hour
    DATA(ts7) = cl_abap_tstmp=>add( tstmp = ts6
                                    secs  = 3600 ).

    "Subtracting 2 hours
    DATA(ts8) = cl_abap_tstmp=>subtractsecs( tstmp = ts6
                                             secs  = 7200 ).

*&---------------------------------------------------------------------*
*& Arithmetic expressions and numeric functions in ABAP SQL
*&---------------------------------------------------------------------*

    SELECT SINGLE
      "---- Arithmethic expressions ----
      1 + 2 AS add,
      10 - -8 AS subtract,
      3 * 5 AS multiply,
      "The / operator is not allowed in integer expressions.
      "4 / 2 AS divide_nope,
      "The following example uses typed literals to use decimal
      "floating point numbers.
      d34n`4.4` / d34n`2.2` AS divide1,
      "The following example uses cast expressions to convert integer values
      "so that a division can be performed.
      CAST( 1 AS D34N ) / CAST( 2 AS D34N ) AS divide2,
      "Using parentheses
      ( 1 + 2 ) * 3 AS parentheses,
      "---- Functions for numeric values ----
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
      abs( int4`-2` ) AS abs, "2
      "Result is rounded to the specified position after the decimal separator
      round( decfloat34`1.337`, 2 ) AS round
    FROM i_timezone "Released API (not relevant in the example)
    INTO @DATA(some_data).
  ENDMETHOD.

  METHOD calculate_i.
    IF operator = add.
      result = num1 + num2.
    ELSEIF operator = subtract.
      result = num1 - num2.
    ELSEIF operator = multiply.
      result = num1 * num2.
    ELSEIF operator = divide.
      IF num1 = 0 AND num2 = 0.
        RAISE EXCEPTION TYPE cx_sy_zerodivide.
      ELSE.
        result = num1 / num2.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_decfloat34.
    CASE operator.
      WHEN add.
        result = num1 + num2.
      WHEN subtract.
        result = num1 - num2.
      WHEN multiply.
        result = num1 * num2.
      WHEN divide.
        CASE num1.
          WHEN 0.
            CASE num2.
              WHEN 0.
                RAISE EXCEPTION TYPE cx_sy_zerodivide.
            ENDCASE.
        ENDCASE.
        result = num1 / num2.
    ENDCASE.
  ENDMETHOD.

  METHOD calculate_numeric_cond.
    result = COND #( WHEN operator = add
                     THEN num1 + num2
                     WHEN operator = subtract
                     THEN num1 - num2
                     WHEN operator = multiply
                     THEN num1 * num2
                     WHEN operator = divide AND num2 = 0 THEN THROW cx_sy_zerodivide(  )
                     WHEN operator = divide AND num2 <> 0 THEN num1 / num2 ).
  ENDMETHOD.

  METHOD calculate_numeric_switch.
    result = SWITCH #( operator
                       WHEN add THEN num1->* + num2->*
                       WHEN subtract THEN num1->* - num2->*
                       WHEN multiply THEN num1->* * num2->*
                       WHEN divide THEN SWITCH #( num1->* WHEN 0 THEN SWITCH #( num2->* WHEN 0 THEN THROW cx_sy_zerodivide(  ) ) ELSE num1->* / num2->* ) ).
  ENDMETHOD.

  METHOD divide_by.
    TRY.
        number /= num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD minus.
    TRY.
        number -= num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD multiply_by.
    TRY.
        number *= num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD plus.
    TRY.
        number += num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD get_result.
    result = number.
  ENDMETHOD.

  METHOD create_calculation.
    ref = NEW zcl_demo_abap( ).
    ref->number = num.
  ENDMETHOD.
ENDCLASS.
```

</details>  