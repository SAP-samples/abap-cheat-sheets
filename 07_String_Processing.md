<a name="top"></a>

# String Processing

- [String Processing](#string-processing)
  - [Introduction](#introduction)
  - [Data Types for Character Strings](#data-types-for-character-strings)
  - [Declaring Character-Like Data Objects](#declaring-character-like-data-objects)
  - [Assigning Values](#assigning-values)
  - [String Templates](#string-templates)
  - [Determining the Length of Strings](#determining-the-length-of-strings)
  - [Concatenating Strings](#concatenating-strings)
  - [Splitting Strings](#splitting-strings)
  - [Modifying Strings](#modifying-strings)
  - [Processing Substrings](#processing-substrings)
  - [Searching and Replacing](#searching-and-replacing)
    - [Searching for Specific Characters](#searching-for-specific-characters)
    - [Replacing Specific Characters in Strings](#replacing-specific-characters-in-strings)
    - [Searching for Substrings in Strings (and Tables)](#searching-for-substrings-in-strings-and-tables)
    - [Replacing Substrings in Strings (and Tables)](#replacing-substrings-in-strings-and-tables)
  - [Pattern-Based Searching and Replacing in Strings](#pattern-based-searching-and-replacing-in-strings)
    - [Simple Pattern-Based Searching Using Comparison Operators](#simple-pattern-based-searching-using-comparison-operators)
    - [Complex Searching and Replacing Using Regular Expressions](#complex-searching-and-replacing-using-regular-expressions)
      - [Excursion: Common Regular Expressions](#excursion-common-regular-expressions)
      - [Searching Using Regular Expressions](#searching-using-regular-expressions)
        - [Excursion: System Classes for Regular Expressions](#excursion-system-classes-for-regular-expressions)
      - [Replacing Using Regular Expressions](#replacing-using-regular-expressions)
  - [Executable Example](#executable-example)


## Introduction

ABAP offers plenty of options for processing [character strings](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencharacter_string_glosry.htm "Glossary Entry").
The options include ABAP statements (e. g. [`FIND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfind.htm)),
[character string expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_expression_glosry.htm "Glossary Entry")
([concatenations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconcatenation_glosry.htm) and [string templates](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_template_glosry.htm "Glossary Entry"))
and built-in [string functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_function_glosry.htm "Glossary Entry")
(e. g. [`strlen`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlength_functions.htm)).

> **💡 Note**<br>
>-  Compared to statements, expressions and string functions can help make your ABAP code more
    concise and straightforward. For example, you can perform string operations directly in [operand
    position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm "Glossary Entry"),
    allowing you to avoid temporary variables.
>-   In ABAP statements, modification operations on strings are often performed in read/write positions, meaning that the source and target
    fields of an operation are the same. When working with string functions, the source field is passed as an input parameter and the modified value is returned as a [return value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreturn_value_glosry.htm "Glossary Entry"), meaning that the function itself does not modify the source field. Of course, you can assign the function to the source field to achieve its modification.
>-   In most cases, string functions provide the same functionality as the
    corresponding ABAP statements, or even more. The return value of string functions
    that return character strings is always of type `string`.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Data Types for Character Strings

ABAP provides the following built-in data types for data objects that contain character strings. They are distinguished as follows:

| Type | Details | Length | Value Range | Initial Value |
|---|---|---|---|---|
| `string` | For variable length character strings. [Data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm "Glossary Entry") of this type are [dynamic data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendynamic_data_object_glosry.htm "Glossary Entry"), i. e. the length of a variable can change during the execution of an ABAP program and thus it can contain character strings of different lengths. A data object of type `string` is called *text string* or, in short, just *string*. | No standard length; length is variable | Any [Unicode](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenunicode_glosry.htm) characters that can be encoded in ABAP language's code page [UCS-2](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenucs2_glosry.htm). The most common content are alphanumeric characters or special characters.  | Empty string with length 0 |
| `c` | For fixed length character strings. Data objects of this type are [static data objects](http://ldcialx.wdf.sap.corp:50018/sap/public/bc/abap/docu?sap-language=EN&object=abenstatic_data_object_glosry&version=X&sap-client=000), i. e. the length of a variable must be defined during its declaration and does not change during the execution of an ABAP program. Thus, it always contains character strings of the same length. A data object of type `c` is called *text field*.|Data objects of this type can contain a string of fixed length (between 1 and 262143 characters); standard length: 1 | Same as for `string` | A blank for each position |

In addition to these main data types for character strings, there are several other fixed length data types with special meanings:

-	`n` for fixed length numerical character strings
    - Data objects of this type are technically almost the same as text fields. However, the only valid characters are the digits 0 to 9. Validity is not checked for assigning values in a regular way but only for [lossless assignments](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlossless_assignment_glosry.htm). Thus, such numeric text fields can contain invalid data, but should only be used for digits that are not intended for arithmetic calculations, such as zip codes or article numbers. The initial value for each position is 0.
-	`d` and `t` for date and time fields
    - These data types have a predefiend length of 6 and 8. Data objects of these types are used for character representations of dates and times in a predefined format. You can use them directly in date and time calculations. However, these fields can also contain invalid values.

These data types are not covered further in this cheat sheet. The same is true for the [byte-like data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbyte_like_data_typ_glosry.htm "Glossary Entry") `x` and `xstring` that are closely related to `c` and `string` but contain raw [byte strings](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbyte_string_glosry.htm).

> **⚡ Differences between text strings (variable length) and text fields (fixed length)**<br>
>-   **Initial value**: The initial value of a text string is an
    empty string of length 0. The initial value of text field is represented by blanks at each position.
>-   **Internal representation**: Data objects of type `c` and `string` are both [elementary data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenelementary_data_object_glosry.htm "Glossary Entry").
    However, while text fields occupy a block of memory according to their length, text strings are so-called [deep](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_glosry.htm "Glossary Entry") data objects. Internally, they are managed by a [reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_glosry.htm "Glossary Entry") that points to the actual character. This fact has restrictive consequences for the use of strings as components of structures, but can also improve the performance of assignments due to the concept of [sharing](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensharing_glosry.htm "Glossary Entry") of deep data objects.
>-   **Length**: Theoretically, a text string can use up to 2 GB (one character occupies 2 bytes).
    The maximum length of a text field is 262143 characters.
>-   **Trailing blanks**: For text strings, trailing blanks are preserved in all operations. For text fields, it depends on the [operand
    position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm "Glossary Entry") whether trailing blanks are respected or not. In most operand positions, trailing blanks are truncated when working with text fields, even when using [text field literals](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentext_field_literal_glosry.htm). For example, if a text field is assigned to a text string, the resulting target string will never contain trailing blanks. See the *Condensing Strings* section in this context.
>-   **Flexibility**: Text strings are more flexible than text fields
    because you can easily shorten or lengthen them without
    worrying that, for example, parts of the character string will be
    truncated during processing. On the other hand, when accessing substrings of a string, you have to make sure that the string is long enough, whereas with text fields you always know their length.

So, when to use what? Text fields are useful when
actually specifying a maximum or mandatory length, e.g. a country code
that must be a maximum of two characters, or for input fields in
forms that should not exceed a certain length. If limiting a string
is not relevant, text strings are a good choice.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Declaring Character-Like Data Objects

- To work with character strings, you need character-like data objects based on the character-like types mentioned above.
- The simplest way of producing text in an ABAP program are [character literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencharacter_literal_glosry.htm).
The following code snippet shows a global class implementing the interface `if_oo_adt_classrun`. 
  - Using the `write` method, you can display output in the ADT console. In the example, two [untyped literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenuntyped_literal_glosry.htm) without a dedicated name ([unnamed data object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenunnamed_data_object_glosry.htm)) are included. 
  - In the case below, the data type of the character literals are defined by the delimiters.
- Text string literals are enclosed in backquotes (<code>\`...\`</code>) and have the data type `string`. 
- Text field literals are enclosed in single quotes (`'...'`) and have the data type `c`. 
- The literals can be (but should not according to the [programming guidelines on literals (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenliterals_guidl.htm)) used like constants of these types in [operand positions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm). They should be only used for start values when declaring [named data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennamed_data_object_glosry.htm).

```abap
CLASS zcl_some_test_class DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.

CLASS zcl_some_test_class IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    out->write( `I am a text string literal` ).  "text string literal of type string
    out->write( 'I am a text field literal' ).   "text field literal of type c
  ENDMETHOD.
ENDCLASS.
```

- [Named](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennamed_data_object_glosry.htm) character-like data types and objects can be declared like other types and objects using [`TYPES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes.htm), [`DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata.htm) [`CONSTANTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconstants.htm) and by referring to a character-like data type.
- In addition, character-like data objects can be declared inline with the operators `DATA` and [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm).

Syntax examples:
``` abap
"Type declarations using built-in types

TYPES: c_type   TYPE c LENGTH 3, "Explicit length specification
       str_type TYPE string.

"Data object declarations using built-in, local and DDIC types

DATA: flag  TYPE c LENGTH 1,   "Built-in type
      str1  TYPE string,       "Built-in type
      char1 TYPE c_type,       "Local type
      str2  LIKE str1,         "Deriving type from a local data object
      str3  TYPE str_type,     "Local type
      char2 TYPE s_toairp,     "DDIC type (used e. g. for a field in a demo table)
      char3 TYPE zdemo_abap_flsch-carrid. "Using the type of a DDIC table component

"You may also encounter declarations with type c and the length
"specified in parentheses. This is not recommended, to avoid confusion
"with the use of parentheses in dynamic programming.

DATA char(4) TYPE c.

"Just a TYPE c specification without length means LENGTH 1.
DATA char_len_one TYPE c.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Assigning Values

- When you declare character-like data objects, you can specify start values directly with the `VALUE` addition, e.g. `DATA chars TYPE c LENGTH 3 VALUE 'abc'.`. 
- You can do value assignments to data objects using the the [assignment operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm "Glossary Entry") `=`.
- As mentioned above, you can declare character-like data objects inline using the operators `DATA` or `FINAL`. 
- You can use the operators at many [write positions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwrite_position_glosry.htm "Glossary Entry"). 
- Unlike the `VALUE` addition of the declaration statements, inline declarations allow you to declare variables for the results of expressions or at other positions where character strings are returned.
- In the case below, a variable specified in parentheses preceded by `DATA` (or `FINAL`) on the left side of the assignment operator automatically derives a data type from the operand on the right. This helps to make your
programs leaner.

Syntax examples:
``` abap
"Data object declarations including default values with VALUE
"Note the chained statement: DATA followed by a colon, listing the data object declarations,
"separated by a comma.
DATA: flag TYPE c LENGTH 1 VALUE 'X',
      str1 TYPE string VALUE `Hallo!`.

"Examples for type n
DATA zip_code TYPE n LENGTH 5 VALUE '12345'.
DATA isbn_number TYPE n LENGTH 13 VALUE '1234567890123'.

"Constant; content cannot be changed at runtime
CONSTANTS pi TYPE p LENGTH 8 DECIMALS 14 VALUE '3.14159265358979'.

"More data object declarations
DATA: char1 TYPE c LENGTH 5,
      html  TYPE string,
      str2  LIKE html.

"Value assignments
char1 = 'ab123'.
html  = `<p>hallo</p>`.

"Escaping backquotes in text string literals with another one
str1  = `This is a backquote: ``.`.

"If possible, avoid unnecessary type conversion; in principle, every
"convertible type can be specified
str2 = 'abc'. "Fixed length string assigned to data object of type string
DATA str3 TYPE string VALUE 'X'. "type c length 1
DATA str4 TYPE string VALUE -1.  "type i

"Inline declarations
DATA(char2) = 'abcd'. "Type c length 4
DATA(str5)  = `efgh`.

"You can use FINAL to create immutable variables.
FINAL(final_string)  = `zyx`.

"Since char2 is of type c length 4 (the length is also derived),
"characters are truncated in the following example assignment
char2 = 'ijklmnopq'. "ijkl

"Trailing blanks after assigning fixed length to variable length string
DATA(char3) = 'ab   '.
DATA(str6)  = `cdefgh`.
str6 = char3. "'ab' (trailing blanks are not respected due to conversion rule)
```

- When assigning strings, not only data objects can be placed on the right
side. Various expressions and strings can be concatenated using the
[concatenation
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconcatenation_operator_glosry.htm "Glossary Entry")
`&&`. 
- Alternatively, you can concatenate strings using [string
templates](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_template_glosry.htm "Glossary Entry"), as described in the *Concatenating Strings* section.
``` abap
str5 = str3 && ` ` && str4 && `!`. "X 1-!
"Note the output for str4 that includes the conversion of type i to
"string above demonstrating a possibly inadvertent specification
"of an integer value for str4 that is of type string.
```

Note that there is also the [literal
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenliteral_operator_glosry.htm "Glossary Entry")
`&` that joins [text string
literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentext_string_literal_glosry.htm "Glossary Entry"),
however, with [significant
differences](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenliteral_operator.htm)
to `&&`.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## String Templates
- Using string templates, you can construct strings very elegantly from
literal text and - which is the primary use case - by including
embedded ABAP
[expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenexpression_glosry.htm "Glossary Entry")
within a pair of delimiters (`|...|`) if these expressions can be converted to `string`. 
- To embed expressions, you enclose them in curly brackets: `{ ... }`.

> **💡 Note**<br>
> String templates form a [string
expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_expression_glosry.htm "Glossary Entry")
that is compiled at runtime. Therefore, a string template that contains only
literal text is treated as an expression, which has a performance impact. In such a case, it is preferable to use a text string literal with backquotes.

Syntax examples:
``` abap
"Value assignment with string templates
"The expression must be convertible to a string. A blank (not within the curly brackets)
"means a blank in the resulting string.
DATA(s1) = |Hallo { cl_abap_context_info=>get_user_technical_name( ) }!|.

DATA(s2) = `How are you?`. "Literal text only with backquotes
DATA(s3) = |{ s1 } { s2 }|. "Hallo NAME! How are you?

"Chaining of string templates using &&
DATA(s4) = |{ s1 }| && ` ` && |{ s2 }|. "Hallo NAME! How are you?
```

- String templates interpret certain character combinations as [control
characters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_templates_separators.htm).
- For example, `\n` is interpreted as a newline. A new line is
started. 
- String templates also support various [formatting
options](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcompute_string_format_options.htm).
- Refer to the ABAP Keyword Documentation for all options.

Syntax examples:
``` abap
"Control characters
s4 = |{ s1 }\n{ s2 }\nSee you.|. "\n is interpreted as a line feed

"Excursion: Class CL_ABAP_CHAR_UTILITIES provides attributes and methods as utilities for string processing.
"See the class documentation
"The following examples demonstrate that attributes that contain control characters can be replaced by
"a representation of control characters in a string template.
ASSERT cl_abap_char_utilities=>newline        = |\n|.
ASSERT cl_abap_char_utilities=>horizontal_tab = |\t|.
ASSERT cl_abap_char_utilities=>cr_lf          = |\r\n|.

"Various formatting options
"Time and date
"Formatting according to the user master data
DATA(d) = |The date is { cl_abap_context_info=>get_system_date( ) DATE = USER }.|.

"Formatting in accordance with ISO 8601
DATA(tm) = |The time is { cl_abap_context_info=>get_system_time( ) TIME = ISO }.|.

"Formatting to UTC; date and time are represented in ISO 8601
DATA(ts) = |Timestamp: { utclong_current( ) TIMESTAMP = SPACE }.|.

"Lowercase and uppercase
s1 = |AbCdEfG|.
s2 = |{ s1 CASE = LOWER }|. "abcdefg
s2 = |{ s1 CASE = UPPER }|. "ABCDEFG

"Width and alignment
s1 = `##`.
s2 = |{ s1 WIDTH = 10 ALIGN = LEFT }<---|.   "'##        <---'
s2 = |{ s1 WIDTH = 10 ALIGN = CENTER }<---|. "'    ##    <---'

"PAD: Used to pad any surplus places in the result with the specified character.
s2 = |{ s1 WIDTH = 10 ALIGN = RIGHT PAD = `.` }<---|. "'........##<---'

"Numbers
s1 = |{ CONV decfloat34( - 1 / 3 ) DECIMALS = 3 }|. "'-0.333'
```

> **💡 Note**<br>
> Escape `\|{}` in string templates using `\`, i. e. `\\` means `\`.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Determining the Length of Strings

- To determine the length of a string, you can use the string function
[`strlen`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlength_functions.htm).
- Note that the result depends on the type of the string, i. e. the result for a data object of type `string` includes trailing blanks. A
fixed-length string does not include them. 
- To exclude trailing blanks in all cases, regardless of the data type, you can use the built-in
[`numofchar`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlength_functions.htm) function.

Syntax examples:
``` abap
"strlen
DATA(len_c)   = strlen( 'abc   ' ). "3
DATA(len_str) = strlen( `abc   ` ). "6

"numofchar
len_c   = numofchar( 'abc   ' ). "3
len_str = numofchar( `abc   ` ). "3
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Concatenating Strings

- Two or more strings can be concatenated using the concatenation operator
`&&` and string templates. Alternatively, you can use
[`CONCATENATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconcatenate.htm)
statements. 
- It is also possible to concatenate lines from internal tables
into a string to avoid a loop. 
- A more modern way is to use
the string function
[`concat_lines_of`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconcatenation_functions.htm).

Syntax examples:
``` abap
"&& and string template
DATA(s1) = `AB` && `AP`. "ABAP
DATA(s2) = `ab` && `ap` && ` ` && s1. "abap ABAP
DATA(s3) = |{ s1 }. { s2 }!|. "ABAP. abap ABAP!

"CONCATENATE statements
CONCATENATE s1 s2 INTO s3. "ABAPabap ABAP

"Multiple data objects and target declared inline
CONCATENATE s1 ` ` s2 INTO DATA(s5). "ABAP abap ABAP

CONCATENATE s1 s2 s5 INTO DATA(s6). "ABAPabap ABAPABAP abap ABAP

"You can also add a separation sign using the addition SEPARATED BY
CONCATENATE s1 s2 INTO s3 SEPARATED BY ` `. "ABAP abap ABAP

CONCATENATE s1 s2 INTO s3 SEPARATED BY `#`. "ABAP#abap ABAP

"Keeping trailing blanks in the result when concatenating fixed length
"strings. The ones of variable length strings are respected by default.
CONCATENATE 'a  ' 'b  ' 'c  ' INTO DATA(ch) RESPECTING BLANKS. "'a  b  c  '

"Concatenating lines of internal tables into a string
CONCATENATE LINES OF itab INTO t SEPARATED BY ` `.

"Using concat_lines_of
s1 = concat_lines_of( table = itab ). "Without separator
s1 = concat_lines_of( table = itab sep = ` ` ). "With separator
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Splitting Strings

- You can use
[`SPLIT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapsplit.htm)
statements to split strings in multiple segments. 
- The result of the
split can be stored in separate data objects or internal tables that
have a character-like line type. 
- Note that if the number of specified targets is
less than the number of segments returned by the split, the last target receives the remaining unsplit segements. If more targets are specified, the targets that do not receive a segment are
initialized. 
- Therefore, specifying individual targets with `SPLIT`
statements is useful if the number of expected segments is known.
Otherwise, splitting into tables is a good choice.
- If you want to get the value of a particular segment, you can use the
string function
[`segment`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensegment_functions.htm).

Syntax examples:
``` abap
DATA(s1) = `Hallo,world,123`.
DATA: s2 TYPE string, 
      s3 TYPE string,
      s4 TYPE string.

SPLIT s1 AT `,` INTO s2 s3 s4. "s2 = Hallo / s3 = world / s4 = 123

"Less data objects than possible splittings
SPLIT s1 AT `,` INTO s2 s3. "s2 = Hallo / s3 = world,123

"Splitting into internal table
DATA itab TYPE TABLE OF string.
SPLIT s1 AT ',' INTO TABLE itab. "Strings are added to itab in individual lines without comma

"String function segment returning the occurrence of a segment
"index parameter: number of segment
s2 = segment( val = s1 index = 2 sep = `,` ). "world
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Modifying Strings
**Transforming to Lowercase and Uppercase**

- The string functions
[`to_lower`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencase_functions.htm)
and
[`to_upper`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencase_functions.htm)
transform characters of a string to either lowercase or uppercase and
store the result in a target variable. 
- If you want to apply the transformation to the source directly, you can use
[`TRANSLATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptranslate.htm)
statements.

Syntax examples:
``` abap
"String functions
DATA(s1) = to_upper( `abap` ). "ABAP
s1 = to_lower( `SOME_FILE.Txt` ). "some_file.txt

"TRANSLATE statements
s1 = `Hallo`.
TRANSLATE s1 TO UPPER CASE. "HALLO
TRANSLATE s1 TO LOWER CASE. "hallo
```

**Shifting Content**

- You can shift content within a string to a specific position on the left
or right of a string.
[`SHIFT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapshift.htm)
statements have various additions for specific use cases.
- In a more modern way, you can use the string functions
[`shift_left`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenshift_functions.htm)
and
[`shift_right`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenshift_functions.htm), which store the result in a variable. 
  - These functions provide additional
functionality. The `sub` parameter can be used to specify a
substring. All substrings in the string that match the value specified
in `sub` on either the left or right side of the string are
removed.

Syntax examples:
``` abap
"SHIFT statements
"Note that all results below refer to s1 = `hallo`.
DATA(s1) = `hallo`. "Type string

SHIFT s1. "No addition; string shifted one place to the left: allo
SHIFT s1 BY 2 PLACES. "Without direction, left by default: llo
SHIFT s1 BY 3 PLACES RIGHT. "With direction, variable length strings are extended: '   hallo'

"Note that all results below refer to ch4 = 'hallo'.
DATA(ch4) = 'hallo'. "Type c length 5

SHIFT ch4 BY 3 PLACES RIGHT. "Fixed length string: '   ha'

"CIRCULAR addition: characters that are moved out of the string are
"added at the other end again
SHIFT ch4 BY 3 PLACES LEFT CIRCULAR. "lohal
SHIFT ch4 UP TO `ll`. "Shift characters up to a specific character set: llo

"Deleting leading and trailing characters
DATA(s2) = `   hallo   `.
DATA(s3) = s2.

SHIFT s2 LEFT DELETING LEADING ` `. "'hallo   '
SHIFT s3 RIGHT DELETING TRAILING ` `. "'      hallo' (length is kept)

"Removing trailing blanks in strings without leading blanks;
"you can use the following sequence of statements
DATA(s4) = `hallo   `.
SHIFT s4 RIGHT DELETING TRAILING ` `. "'   hallo'
SHIFT s4 LEFT DELETING LEADING ` `. "'hallo'

"String functions with parameters
s1 = `hallo`.

s2 = shift_left( val = s1 places = 3 ). "lo
s2 = shift_left( val = s1 circular = 2 ). "lloha

"Note that shift_right does not extend a variable length string.
s2 = shift_right( val = s1 places = 3 ). "ha
s2 = shift_right( val = `abc   ` sub = ` ` ). "'abc'
s2 = shift_right( val = `abc   ` ). "'abc' (same result as above)
```

**Condensing Strings**

- You can use
[`CONDENSE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcondense.htm)
statements or the string function
[`condense`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencondense_functions.htm)
to remove blanks from strings. 
- The advantage of using the string function
is that you can specify any character to remove, not just blanks.

Syntax examples:
``` abap
"CONDENSE statements
DATA(s1) = ` ab cd `.
DATA(s2) = `    ef   gh ij   `.
DATA(s3) = ` kl  mn   op `.

CONDENSE s1. "Trailing and leading blanks are removed: 'ab cd'
CONDENSE s2. "It also replaces sequences of multiple blanks with a single blank: 'ef gh ij'
CONDENSE s3 NO-GAPS. "Removes all blanks: 'klmnop'

"String function condense
s1 = ` ab   cd `.

"No parameters specified, i. e. their default values are provided.
"Works like CONDENSE statement without the NO-GAPS addition.
s2 = condense( s1 ). "ab cd

"Parameters del/to not specified. from parameter with initial string
"(could also be a text field literal: from = ' '). This way, leading and
"trailing blanks are removed.
s2 = condense( val = s1 from = `` ). "ab   cd

"Parameter to specified with an initial string. No other parameters.
"Works like CONDENSE statement with the NO-GAPS addition.
s2 = condense( val = s1  to = `` ). "abcd

"Parameter del specifies the leading/trailing characters to be removed.
s2 = condense( val = `##see###you##` del = `#` ). "see###you

"If from and to are specified along with del, leading/trailing
"characters specified in del are first removed. Then, in the remaining string, all
"substrings composed of characters specified in from are replaced with
"the first character of the string specified in the to parameter
s2 = condense( val  = `  Rock'xxx'Roller`
               del  = `re `
               from = `x`
               to   = `n` ). "Rock'n'Roll
```

**Reversing Strings**

The string function
[`reverse`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreverse_functions.htm)
reverses a string:
``` abap
"Result: 'abap'
DATA(s1) = reverse( `paba` ).
```

**Inserting Substrings into Strings**

- The string function
[`insert`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninsert_functions.htm)
inserts a substring at any position within a given string. You can use various parameters to construct the string you want:
  - `val`: Original string.
  - `sub`: Substring.
  - `off`: Optionally sets the offset, i.e. the position where the substring should be added. The default value is 0. When using the function with the default value, the result is like concatenating a  string with `&&` (like `res = sub && text`).
- Inserting substrings can also be accomplished using the string function `replace` or `REPLACE` statements, which are are covered below.

Syntax examples:
``` abap
"Result: 'abcdefghi'
DATA(s1) = insert( val = `abcghi` sub = `def` off = 3 ).

"Result: 'defabcghi'
s1 = insert( val = `abcghi` sub = `def` ).
```

**Overlaying Content**

You can use [`OVERLAY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapoverlay.htm) statements to replace characters in one variable with characters in another variable that are in the same place there.

Syntax examples:
``` abap
DATA(incl) = '==============================CP'.
DATA(cl_name) = 'CL_SOME_CLASS                   '.

"Addition ONLY is not specified: All blanks are replaced
OVERLAY cl_name WITH incl. 
"cl_name: CL_SOME_CLASS=================CP

DATA(txt1) = 'a.b.c.a.b.c.A'.
DATA(txt2) = 'z.x.y.Z.x.y.z'.

"Addition ONLY is specified: All characters that are specified after ONLY and that 
"occur in the operand are replaced. Note that this is case-sensitive.
OVERLAY txt1 WITH txt2 ONLY 'ab'.
"txt1: z.x.c.Z.x.c.A
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Processing Substrings

- The string function
[`substring`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm) allows you to specify the position (parameter `off`) and the length
(`len`) of a substring to be extracted from a given
string (`val`). 
  - At least one of the two parameters `off`
or `len` must be specified. The default value of `off`
is 0, i.e. when using the default value, the substring is extracted
from the beginning of the string. 
  - If `len` is not specified, the rest of the remaining characters are respected. If the offset
and length are greater than the actual length of the string, the
exception `CX_SY_RANGE_OUT_OF_BOUNDS` is raised.
- You may also encounter the syntax for accessing substrings by specifying the offset
and length using the `+` character after a variable. 
  - The length is specified in parentheses. Specifying an asterisk (`*`) means
that the rest of the remaining string is respected. 
  - This syntax option
even allows write access to substrings for fixed-length strings. Read
access is possible for both fixed-length and variable-length strings.
  - However, this syntax can be confused with the use of tokens in the
context of  dynamic programming.
- There are other string functions available for dealing with substrings, such as 
[`substring_after`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm),
[`substring_before`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm),
[`substring_from`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm)
and
[`substring_to`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm).
  - These functions offer more options in terms of parameters, such as the use of [PCRE regular
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpcre_regex_glosry.htm "Glossary Entry"),
which are covered below.

Syntax examples:
``` abap
DATA(s1) = `Lorem ipsum dolor sit amet`. "Type string

"Extracting substring starting at a specific position
"'len' not specified means the rest of the remaining characters are
"respected
DATA(s2) = substring( val = s1 off = 6 ). "ipsum dolor sit amet

"Extracting substring with a specific length
"'off' is not specified and has the default value 0.
s2 = substring( val = s1 len = 5 ). "Lorem

"Specifying both off and len parameters
s2 = substring( val = s1 off = 6 len = 5 ). "ipsum

DATA(txt) = 'Lorem ipsum dolor sit amet'. "Type c

"Offset and length specification using the + sign after a variable
DATA(ch6) = txt+0(5). "Lorem

"* means respecting the rest of the remaining string
DATA(ch7) = txt+12(*). "dolor sit amet

CLEAR txt+11(*). "Lorem ipsum

txt+0(5) = 'Hallo'. "Hallo ipsum dolor sit amet

"Further string functions
s1 = `aa1bb2aa3bb4`.

"Extracting a substring ...
"... after a specified substring
s2 = substring_after( val = s1 sub = `aa` ). "1bb2aa3bb4 (only the first occurrence is respected)

"... after a specified substring specifying the occurence in a string
"and restricting the length
s2 = substring_after( val = s1 sub = `aA` occ = 2 len = 4 case = abap_false ). "3bb4

"... before a specified substring
s2 = substring_before( val = s1 sub = `b2`  ). "aa1b

"... from a specified substring on. It includes the substring specified
"in sub. len/off and other parameters are possible.
s2 = substring_from( val = s1 sub = `a3` ). "a3bb4

"... up to a specified substring. It includes the substring specified
"in sub. len/off and other parameters are possible.
s2 = substring_to( val = s1 sub = `3b` ). "aa1bb2aa3b
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Searching and Replacing

- In ABAP, there are many ways to perform search and replace
operations on strings. These include the use of [comparison
operators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomp_operator_glosry.htm "Glossary Entry")
or the ABAP statements
[`FIND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfind.htm)
and
[`REPLACE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapreplace.htm),
or the more modern built-in string functions
[`find`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensearch_functions.htm)
and
[`replace`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreplace_functions.htm),
among others, with their considerable number of additions and parameters. 
- Many of these options support rather simple operations
on single characters only or more complex, pattern-based
operations on character sequences using [PCRE regular
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpcre_regex_glosry.htm "Glossary Entry").

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Searching for Specific Characters

-   You can use the [comparison
    operators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomp_operator_glosry.htm "Glossary Entry")
    [`CA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains any) or its negation
    [`NA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains not any) in [comparison
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomparison_expression_glosry.htm "Glossary Entry")
    to determine whether any character of a given character set is contained
    in a string. Such an expression is true if at least one character is
    found.
    -   The search is case-sensitive.
    -   The system variable `sy-fdpos` contains the offset of
        the first character found. If nothing is found,
        `sy-fdpos` contains the length of the string.
    -   Note that offset 0 represents the very first position.
-   The string functions
    [`find_any_of`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensearch_functions.htm)
    and its negation
    [`find_any_not_of`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensearch_functions.htm)
    return the offset of the occurrence of any character contained in a
    substring. They are special variants of the string function `find`, which is shown below.
    -   If nothing is found, the value -1 is returned.
    -   Other optional parameters are possible. For example, the
        specification of `occ` determines the search
        direction, i.e. a positive value means that the search is performed
        from left to right. A negative value means to search from right
        to left.
-   If you are not interested in the position of characters, but rather how
    often they occur in a string, you can use the string function
    [`count`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencount_functions.htm), as well as the special variants `count_any_of` and its negation `count_any_not_of`.
-   To determine whether a string contains only a certain set of characters,
    you can use the comparison operators
    [`CO`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains only) or its negation
    [`CN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains not only) in comparison expressions.
    -   For `CO`, a comparison is true if the left operand
        contains only characters that are also contained in the right
        operand. If the comparison returns false, you can use `sy-fdpos` to get the position
        of the first character from text that is not contained in the
        character. 
     - For `CN`, a
        comparison is true if a string contains characters other than those in the character set.

Syntax examples:
``` abap
DATA(s1) = `cheers`.
IF s1 CA `aeiou` ... "true, sy-fdpos = 2
IF s1 NA `xyz`... "true, sy-fdpos = 6

IF s1 CO `rs` ... "false, sy-fdpos = 0
IF s1 CN `cheers` ... "false, sy-fdpos = 6
```

Built-in functions:

``` abap
"Note that the functions may contain more parameters than those covered in the snippet.
DATA(str) = `Pieces of cakes.`.
DATA res TYPE i.

"find_end returns the sum of the offset of the occurrence
res = find_end( val = str sub = `of` ). "9

"find_any_of returns the offset of the occurrence of any character contained in substring
"The search is always case-sensitive.
res = find_any_of( val = str sub = `x523z4e` ). "2 (character e is found)
res = find_any_of( val = str sub = `zwq85t` ). "-1

"find_any_not_of: Negation of the one above
"The search is always case-sensitive.
res = find_any_not_of( val = str sub = `ieces` ). "0 (very first character in the searched string)
res = find_any_not_of( val = str sub = `P` ). "1

"count returns the number of all occurrences
res = count( val = str sub = `e` ). "3
res = count( val = str sub = `x` ). "0

"count_any_of
res = count_any_of( val = str sub = `x523z4e` ). "3
res = count_any_of( val = str sub = `eco` ). "6

"count_any_not_of
res = count_any_not_of( val = str sub = `fP` ). "14
res = count_any_not_of( val = str sub = `Piecs ofak.` ). "0
```


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Replacing Specific Characters in Strings

- You can use the string function
[`translate`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentranslate_functions.htm)
to replace certain characters with others. 
  - The 
`from` parameter specifies the characters to be placed in a string, and
the `to` parameter specifies the target characters. 
  - Note: The
replacement is performed as follows: Each character specified in
`from` is replaced by the character in `to` that is at
the same position, i.e. the second character in `from` is
replaced by the second character specified in `to`. If there is
no equivalent in `to`, the character in `from` is
removed from the result.
- You can use 
[`TRANSLATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptranslate.htm)
statements to perform replacements directly on the source field.

Syntax examples:
``` abap
DATA(s1) = `___abc_def_____ghi_`.
DATA(s2) = translate( val = s1 from = `hi_` to = `##` ). "abcdefg##
s2 = translate( val = s1 from = `_`  to = `##` ).  "###abc#def#####ghi#

"TRANSLATE statement. The value after USING is interpreted as a string composed of character pairs.
"Starting with the first pair, a search is performed in text for the
"first character in every pair and each occurrence is replaced with the
"second character of the pair.
TRANSLATE s1 USING `_.a#g+`. "...#bc.def.....+hi.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Searching for Substrings in Strings (and Tables)

-   For simple substring searches, you can use the [comparison
    operators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomp_operator_glosry.htm "Glossary Entry")
    [`CS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains string) or its negation
    [`NS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains no string) in [comparison
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomparison_expression_glosry.htm "Glossary Entry").
    The search is not case-sensitive. 
- The system variable
    `sy-fdpos` contains the offset of the found substring. If
    the substring is not found, `sy-fdpos` contains the length
    of the searched string.

``` abap
DATA(s3) = `cheers`.

IF s3 CS `rs` ... "true, sy-fdpos = 4 (offset) 

IF s3 CS `xy`...  "false, sy-fdpos = 6 (length of string) 

IF s3 NS `ee`...  "false, sy-fdpos = 2 (offset)

IF s3 NS `xy`...  "true, sy-fdpos = 6 (length of string) 
```

For more complex and iterative searches, you may want to use `FIND` statements or the string functions below.

- `FIND`
  - Used to search for a character sequence.
  - Has a rich set of additions, a selection of which is covered in this cheat sheet. See [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfind.htm) for more information. Byte string processing is not included (there are special additions).
  - Sets the system fields `sy-subrc`: 0 (search pattern found at least once) or 4 (search pattern not found).

Syntax Overview (see the syntax diagram in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfind.htm)):

``` abap
FIND 
  FIRST OCCURRENCE OF "(or) ALL OCCURRENCES OF
    "1. Only the first occurrence is searched
    "2. All occurrences are searched
    "Note: If none of these two additions is specified, only the first occurrence is searched for.
  
  SUBSTRING some_substring "(or) PCRE some_regex  
    "1. Searching for exactly one string, specifying SUBSTRING is optional (e.g. for emphasis);
    "   some_substring is a character-like operand; note: Trailing blanks are not ignored if it is of type string
    "2. Searching for a substring matching a regular expression; only the PCRE addition should be used;  
    "   some_regex = character-like operand; note: PCRE syntax is compiled in an extended mode, i.e. unescaped whitespaces
    "   are ignored; if the regex is too complex, a catchable exception of the class CX_SY_REGEX_TOO_COMPLEX is raised

IN 
  SECTION 
    OFFSET off 
    LENGTH len
  OF   
    "- Restricting the search to a specific section from an offset specified in off with the length len 
    "- When using SECTION, at least one of the two options must be specified
    "  - No OFFSET specification: offset 0 is used implicitly
    "  - No LENGTH specification: search from specified offset to end of string
    "  - Note: off and len are of type i; it must be a positive integer value; 
    "    exception: len = -1 (same effect as not using the LENGTH addition)
    "- Without the addition SECTION ... OF, the entire data object dobj is searched

"Character-like data object
dobj 

  "Further additional options for advanced evaluation options:
  "Specifying whether the search is case-sensitive; not specified means RESEPECTING CASE by default
  RESPECTING CASE "(or) IGNORING CASE  

  "Determining the number of sequences found, number stored in cnt that is of type i (e.g. a variable declared inline)
  "When searching for the first occurrence, the value is always 1 (not found -> 0)
  MATCH COUNT cnt 

  "Determining position of sequences found
  "Note: off holds the position of the last occurrence when searching for all occurrences and if 
  "there are multiple occurrences (not found -> 0 or the previous value of a finding is retained). 
  MATCH OFFSET off 

  "Determining the length of sequences found
  "Note: Similar to above, not finding an occurrence means 0 for len or the previous value of a finding is retained 
  MATCH LENGTH len
    
  "Storing offset, length, submatches (only relevant for regular expressions) information in a table or a structure
  "tab: of type MATCH_RESULT_TAB; especially for using with ALL OCCURRENCES
  "struc: of type MATCH_RESULT; especially for using with FIRST OCCURRENCE
  "Note on submatches: table of type SUBMATCH_RESULT_TAB; holds offset and length information of substrings of occurrences
  "that are stored in subgroup registers of regular expressions; in FIND IN TABLE statements, the additional component LINE
  "is available
  RESULTS tab "(or) RESULTS struc
 
  "Storing content of subgroup register of a regular expression in character-like data objects;
  "only to be used if a regular expression pattern is specified.
  "Note: Only the last occurrence is evaluated when using ALL OCCURRENCES; the number of the operands specified should match 
  "the number of subgroups specified 
  SUBMATCHES sub1 sub2 ...
.
```

Examples: 

``` abap
"Note: The code snippets mainly use inline declarations.

DATA(str) = `She sells seashells by the seashore.`.

"Determining if a substring is found
"Simple find statement
FIND `se` IN str.

IF sy-subrc = 0.
  "found
ELSE.
  "not found
ENDIF.

"Addition SUBSTRING is optional
FIND SUBSTRING `hi` IN str.

IF sy-subrc = 0.
  "found
ELSE.
  "not found
ENDIF.

"The following examples use the additions MATCH COUNT and MATCH OFFSET to determine 
"the number of occurrences and offset

"Addition FIRST OCCURRENCE OF: Explicit specification to search for the first occurrence
FIND FIRST OCCURRENCE OF `se` IN str
  MATCH COUNT DATA(cnt2)   "1 (always 1 when searching and finding the first occurrence)
  MATCH OFFSET DATA(off2). "4

"Omitting FIRST OCCURRENCE OF and ALL OCCURRENCES OF addition means searching for the 
"first occurrence by default; same effect as the previous statement
FIND `se` IN str
  MATCH COUNT DATA(cnt1)   "1
  MATCH OFFSET DATA(off1). "4

"Addition ALL OCCURRENCES: Searching for all occurrences
FIND ALL OCCURRENCES OF `se` IN str
  MATCH COUNT DATA(cnt3)   "3
  MATCH OFFSET DATA(off3). "27 (value for the last occurrence)

"Addition IN SECTION ... OF:
"Searching in a specified section; both additions OFFSET and LENGTH are specified
FIND ALL OCCURRENCES OF `se`
  IN SECTION OFFSET 9 LENGTH 5 OF str
  MATCH COUNT DATA(cnt4)   "1
  MATCH OFFSET DATA(off4). "10

"Only LENGTH specified (OFFSET is 0 by default)
FIND ALL OCCURRENCES OF `se`
  IN SECTION LENGTH 7 OF str
  MATCH COUNT DATA(cnt5)   "1
  MATCH OFFSET DATA(off5). "4

"Only OFFSET specified (LENGTH: up to end of string)
FIND ALL OCCURRENCES OF `se`
  IN SECTION OFFSET 7 OF str
  MATCH COUNT DATA(cnt6).  "2

"Another string to be searched
DATA(str_abap) = `abap ABAP abap`.

"Further additional options for advanced evaluation options

"Specifying the case-sensitivity of the search
"Not specifying the CASE addition means RESPECTING CASE is used by default. 
"Here, it is explicitly specified.
FIND FIRST OCCURRENCE OF `A` IN str_abap
  MATCH OFFSET DATA(off7)  "5
  RESPECTING CASE.

"Making search case-insensitive
FIND FIRST OCCURRENCE OF `A` IN str_abap
  MATCH OFFSET DATA(off8)  "0
  IGNORING CASE.

"MATCH LENGTH addition
"The example uses a regular expression: Non-greedy search for
"a substring starting with lower case a up to an upper case P
FIND FIRST OCCURRENCE OF PCRE `a.*?P` IN str_abap
  MATCH LENGTH DATA(len8) "9
  RESPECTING CASE.

"RESULTS addition
"Example: Because of using ALL OCCURRENCES, the data object declared inline automatically 
"has the type match_result_tab
FIND ALL OCCURRENCES OF `ab` IN str_abap
  RESULTS DATA(res9)
  IGNORING CASE.

"3 entries in table res9 (tables in SUBMATCHES are initial since no regular expression is used)
"line: always 0 (it's not a table); length: always 2 (search for concrete occurrence of `se`)
"1. line: 0, offset: 0, length: 2, submatches: (initial)
"2. line: 0, offset: 5, length: 2, ...
"3. line: 0, offset: 10, length: 2, ...

"Example: Because of using FIRST OCCURRENCE, the data object declared inline automatically 
"has the type match_result
FIND FIRST OCCURRENCE OF `ab` IN str_abap
  RESULTS DATA(res10)
  IGNORING CASE.

"res10: line: 0, offset: 0, length: 2, submatches: (initial)
```

You can use [`FIND ... IN TABLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfind_itab.htm) statements to search for substrings in internal tables (standard tables without secondary table keys; with character-like line type) line by line. 

``` abap
DATA(str_table) = VALUE string_table( ( `aZbzZ` ) ( `cdZze` ) ( `Zzzf` ) ( `ghz` ) ).

"Finding all occurrences in a table
"Note: res_tab is of type match_result_tab 
"You can also restrict the search range in an internal table; see an example in REPLACE ... IN TABLE
FIND ALL OCCURRENCES OF `Z`
  IN TABLE str_table
  RESULTS DATA(res_tab)
  RESPECTING CASE.

"4 entries in table res_tab (tables in SUBMATCHES are initial since no regular expression is used)
"1. line: 1, offset: 1, length: 1, submatches: (initial)
"2. line: 1, offset: 4, length: 1, ...
"3. line: 2, offset: 2, length: 1, ...
"4. line: 3, offset: 0, length: 1, ...

"Finding the first occurrence in a table
"Note: res_struc, which is declared inline here, is of type match_result 
FIND FIRST OCCURRENCE OF `Z`
  IN TABLE str_table
  RESULTS DATA(res_struc)
  RESPECTING CASE.

"Entries in structure res_struc 
"line: 1, offset: 1, length: 1, submatches: (initial)

"Alternative to the statement above (storing the information in individual data objects)
FIND FIRST OCCURRENCE OF `Z`
  IN TABLE str_table
  MATCH LINE DATA(line)    "1
  MATCH OFFSET DATA(off)   "1 
  MATCH LENGTH DATA(len)   "1
  RESPECTING CASE.
```

**Built-in search functions**
- Built-in search functions, such as `find`, are available for searching strings.
- They return a return value of type i and contain multiple (optional) parameters.
- `FIND` covers the same functionality and more with the many addition options.
- Fore more information, see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensearch_functions.htm)

Parameters of the `find` function:
- `val`: 
  - Character-like data object
  - Note: If a fixed length string is specified, any trailing blanks are ignored. 
- `sub`: 
  - Contains what is searched for
  - A character like expression position; expects arguments with elementary types
  - Similar to above, trailing blanks are ignored in fixed length strings
- `case`: 
  - Search is case-sensitive by default
- `occ`: 
  - Specifies the occurrence of a match
  - Must be of type `i`
  - Values: 
    - 1: default value, searches for the first occurrence from the left
    - any positive value: searches for the nth occurrence from the left
    - any negative value: searches for the nth occurrence from the right
    - 0: raises an exception (`CX_SY_STRG_PAR_VAL`), note: in the context of the `replace` function, 0 means replace all occurrences
  - Note: Specifying `occ` affects the default values of `off` and `len`
- `off`: 
  - Specifies the offset   
  - Must be of type `i` 
  - The default value is 0 (search from the beginning of the string)
  - Exception `CX_SY_RANGE_OUT_OF_BOUNDS` is raised for a negative offset specified and an offset that is longer than the searched string
- `len`: 
  - Specifies the length 
  - Must be of type `i`
  - The default value is the length of the string (minus a defined offset in `off`)
  - The exception `CX_SY_RANGE_OUT_OF_BOUNDS` is raised if the offset is negative and a range is not contained in the searched string
- `pcre`: Regular expression

``` abap
DATA(str) = `Pieces of cakes.`.
DATA res TYPE i.

"Searching for substring
"Returns offset of substring found
res = find( val = str sub = `ca` ). "10

"Substring not found returns -1
res = find( val = str sub = `xy` ). "-1

"Actual parameter of sub must not be initial when using the find function
TRY.
    res = find( val = str sub = `` ).
  CATCH cx_sy_strg_par_val. 
    ...
ENDTRY.

"The search is case-sensitive by default
res = find( val = str sub = `OF` ). "-1
"Making search case-insensitive
res = find( val = str sub = `OF` case = abap_false ). "7

"Specifying occ
res = find( val = str sub = `c` ). "3
res = find( val = str sub = `c` occ = 2 ).  "10
res = find( val = str sub = `e` occ = -1 ). "13
res = find( val = str sub = `e` occ = -3 ). "2

"Specifying off and len
"Specifying a subarea in which a string is searched
res = find( val = str sub = `e` off = 5 ). "13
res = find( val = str sub = `e` off = 5 len = 7 ). "-1
res = find( val = str sub = `e` len = 2  ). "-1

TRY.
    res = find( val = str sub = `e` off = 5 len = 15 ).
  CATCH cx_sy_range_out_of_bounds.
    ...
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Replacing Substrings in Strings (and Tables)

- [`REPLACE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapreplace.htm) and [`REPLACE ... IN TABLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapreplace_itab.htm) statements have a similar syntax as `FIND` and `FIND ... IN TABLE` statements. Refer to the ABAP Keyword Documentation for all possible additions. The following code snippets cover a selection.
- `sy-subrc` is set: 0 (search pattern or section was replaced by the specified content, result was not truncated on the right), 2 (search pattern or section was replaced, result was truncated on the right), 4	(search pattern was not found).
-  `REPLACE` statements can be used to directly replace strings (including substrings, which is not possible with the string function).   

``` abap
"Examples for pattern-based replacements in which data objects are searched for character strings 
"specified in a pattern and the occurrences are replaced

DATA(str_original) = `abap ABAP abap`.
DATA(str) = str_original.

"Simple REPLACE statement
"Omitting the FIRST OCCURRENCE and ALL OCCURRENCES OF additions means
"replacing the first occurrence by default.
REPLACE `ab` IN str WITH `##`. "##ap ABAP abap

str = str_original.

"Addition SUBSTRING is optional
REPLACE SUBSTRING `ab` IN str WITH `##`. "##ap ABAP abap

str = str_original.

"Addition FIRST OCCURRENCE OF: Explicit specification to replace the
"first occurrence; same effect as the statements above
REPLACE FIRST OCCURRENCE OF `ab` IN str WITH `##`. "##ap ABAP abap

str = str_original.

"Addition ALL OCCURRENCES OF: All occurrences are replaced
"Note that the replacement is case-sensitive by default.
REPLACE ALL OCCURRENCES OF `ab` IN str WITH `##`. "##ap ABAP ##ap

str = str_original.

"Further additional options for advanced evaluation options

"IGNORING CASE addition: Making replacements case-insensitive
REPLACE ALL OCCURRENCES OF `ab`
  IN str WITH `##`
  IGNORING CASE. "##ap ##AP ##ap

str = str_original.

"REPLACEMENT COUNT addition
REPLACE ALL OCCURRENCES OF `ab`
  IN str WITH `##`
  REPLACEMENT COUNT DATA(cnt1) "3
  IGNORING CASE.

str = str_original.

"REPLACEMENT OFFSET and LENGTH additions
REPLACE FIRST OCCURRENCE OF `ap`
  IN str WITH `##`
  REPLACEMENT COUNT DATA(cnt2) "1 (always 1 for replaced first occurrence)
  REPLACEMENT OFFSET DATA(off2) "2
  REPLACEMENT LENGTH DATA(len2) "2
  IGNORING CASE. "ab## ABAP abap

str = str_original.

"SECTION ... OF addition: Replacing within a specified area
REPLACE ALL OCCURRENCES OF `ap`
  IN SECTION OFFSET 4 LENGTH 5
  OF str WITH `##`
  REPLACEMENT COUNT DATA(cnt3)  "1
  REPLACEMENT OFFSET DATA(off3) "2
  REPLACEMENT LENGTH DATA(len3) "2
  IGNORING CASE.   "abap AB## abap

str = str_original.

"RESULTS additions with ...
"... ALL OCCURRENCES OF
"Note: repl_tab, which is declared inline here, is of type repl_result_tab
REPLACE ALL OCCURRENCES OF `ap`
  IN str WITH `##`
  RESULTS DATA(repl_tab)
  IGNORING CASE. "ab## AB## ab##

"repl_tab:
"LINE  OFFSET  LENGTH
"0     2       2
"0     7       2
"0     12      2

str = str_original.

"... FIRST OCCURRENCE OF
"Note: repl_struc, which is declared inline here, is of type repl_result
REPLACE FIRST OCCURRENCE OF `ap`
  IN str WITH `##`
  RESULTS DATA(repl_struc)
  IGNORING CASE.

"repl_struc:
"LINE  OFFSET  LENGTH
"0     2       2
```

You can use [`REPLACE SECTION ... OF`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfind_section_of.htm) statements for position-based replacements, that is, to replace a section in a string starting at a specified offset for a specified length.

``` abap
DATA(str_original) = `abap ABAP abap`.
DATA(str) = str_original.

"OFFSET + LENGTH specified
REPLACE SECTION OFFSET 5 LENGTH 4 OF str WITH `#`. "abap # abap

str = str_original.

"Only OFFSET (LENGTH: up to the end of the string)
REPLACE SECTION OFFSET 5 OF str WITH `#`. "abap #

str = str_original.

"Only LENGTH (OFFSET: starting from the leftmost position)
REPLACE SECTION LENGTH 6 OF str WITH `#`. "#BAP abap
```

Replacements in internal tables with `REPLACE ... IN TABLE`:
``` abap
DATA(str_table_original) = VALUE string_table( ( `aZbzZ` ) ( `cdZze` ) ( `Zzzf` ) ( `ghz` ) ).
DATA(str_table) = str_table_original.

"Replacing all occurrences in a table
"RESULTS addition: Storing information in an internal table of type repl_result_tab
REPLACE ALL OCCURRENCES OF `Z`
  IN TABLE str_table
  WITH `#`
  RESULTS DATA(res_table)
  RESPECTING CASE.

"str_table: a#bz# / cd#ze / #zzf / ghz
"res_table:
"LINE  OFFSET  LENGTH
"1     1       1
"1     4       1
"2     2       1
"3     0       1

str_table = str_table_original.

"Replacing the first occurrence in a table
"RESULTS addition: Storing information in a structure of type repl_result
REPLACE FIRST OCCURRENCE OF `Z`
  IN TABLE str_table
  WITH `#`
  RESULTS DATA(res_structure)
  RESPECTING CASE.

"str_table: a#bzZ / cdZze / Zzzf / ghz
"res_structure:
"LINE  OFFSET  LENGTH
"1     1       1

str_table = str_table_original.

"Restricting the search range in an internal table
REPLACE ALL OCCURRENCES OF `Z`
  IN TABLE str_table
  FROM 1 TO 2
  WITH `#`
  RESPECTING CASE.

"str_table: a#bz# / cd#ze / Zzzf / ghz

str_table = str_table_original.

"Offsets can be optionally specified (also only the offset of start or end line possible)
REPLACE ALL OCCURRENCES OF `Z`
  IN TABLE str_table
  FROM 1 OFFSET 3 TO 2 OFFSET 2
  WITH `#`
  RESPECTING CASE.

"str_table: aZbz# / cdZze / Zzzf / ghz
```

- The string function
[`replace`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreplace_functions.htm),
allows you to store the result of a substring replacement in a separate
variable. 
- What makes it particularly powerful in particular is that it
returns a value, so it can be used at almost any [read
positions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenread_position_glosry.htm "Glossary Entry").
- The parameters of the `replace` string functions are similar to those of the `find` function. In addition, there is the `with` parameter for the replacement. Setting `occ` to `0` means that all occurrences are respected for the replacement.

Syntax examples:
``` abap
DATA(str) = `abap ABAP abap`.
DATA res TYPE string.

"Note that here only the first occurrence is replaced.
res = replace( val = str sub = `ap` with = `#` ). "ab# ABAP abap

"Making the search case-insensitive
res = replace( val = str sub = `AB` with = `#` case = abap_false ). "#ap ABAP abap

"Setting occ
res = replace( val = str sub = `ab` with = `#` occ = 2 case = abap_false ). "abap #AP abap

"Replacing all occurrences: Setting occ to 0
res = replace( val = str sub = `ab` with = `#` occ = 0 case = abap_false ). "#ap #AP #ap

"Negative value for occ: Occurrences are counted from the right
res = replace( val = str sub = `ab` with = `#` occ = -1  ). "abap ABAP #ap

"Setting off and len for determining a subarea for replacements
"Note: When using off/len, sub and occ cannot be specified.
"Specifying both off and len
res = replace( val = str  with = `#` off = 5 len = 3  ). "abap #P abap

"Specifying only off (len is 0 by default)
res = replace( val = str  with = `#` off = 2 ). "ab#ap ABAP abap

"Note: When specifying only off and not specifying len or len = 0,
"replace works like insert
res = insert( val = str sub = `#` off = 2  ). "ab#ap ABAP abap

"Specifying only len (off is 0 by default): First segment of length in len is replaced
res = replace( val = str  with = `#` len = 3 ). "#p ABAP abap

"Special case
"- off: equal to the length of the string
"- len: not specified or 0
"- Result: Value specified for 'with' is appended to the end of the string
res = replace( val = str  with = `#` off = strlen( str ) ). "abap ABAP abap#
```

## Pattern-Based Searching and Replacing in Strings

You can perform complex search and replace operations based on
patterns. [PCRE regular
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpcre_regex_glosry.htm "Glossary Entry")
help you process strings effectively.
> **💡 Note**<br>
> Do not use [POSIX
regular
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenposix_regex_glosry.htm "Glossary Entry")
anymore, they are obsolete.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Simple Pattern-Based Searching Using Comparison Operators

For simple patterns, you can use the [comparison
operators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomp_operator_glosry.htm "Glossary Entry")
[`CP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
(conforms to pattern) or its negation
[`NP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
(does not conform to pattern) in [comparison
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomparison_expression_glosry.htm "Glossary Entry")
to determine whether a set of characters is contained in a string that
matches a particular pattern. You can use the following
special characters as patterns:

| Special Character | Details |
|---|---|
| `*` | Any character sequence (including blanks). |
| `+` | Any character (only one character, including blanks). |
| `#` | Escape character. The following character is marked for an exact comparison. |

Patterns are not case-sensitive except for characters marked with
`#`. If a pattern is found, the system variable
`sy-fdpos` returns the offset of the first occurrence.
Otherwise, it contains the length of the searched string.
``` abap
DATA(s1) = `abc_def_ghi`.

"Pattern: f is preceded by any character sequence, must be followed
"by '_' and then followed by any character sequence
IF s1 CP `*f#_*`. ... "true; sy-fdpos = 6

"Pattern: 'i' is not followed by another character
IF s1 NP `i+`. ... "true; sy-fdpos = 11 (length of searched string)
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Complex Searching and Replacing Using Regular Expressions

#### Excursion: Common Regular Expressions

There are several ways to perform complex searches in strings using PCRE expressions. They can be quite complex. The following overview shows common PCRE expressions with simple examples.
For more information, see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenregex_pcre_syntax_specials.htm).

Characters and character types

| Expression | Represents | Example Regex | Example String | Matches | Does not Match |
|---|---|---|---|---|---|
| `x` | Specific character | `a` | abcdef | a | Anything else |
| `.` | Anything except a line break | `.` | ab 1# | a, b, the blank, 1, # | ab, 1# |
| `\d` | Any digit (0-9), alternative: `[0-9]` | `\d` | a1-b2 3-4c9 | 1, 2, 3, 4, 9 | a, b, c, the blank and hyphens |
| `\D` | Any non-digit, alternative: `[^0-9]` | `\D` | a1-b2 3-4c9 | a, b, c, the blank and hyphens | 1, 2, 3, 4, 9 |
| `\s` | Any whitespace character such as a blank, tab and new line | `\s` | (hi X ) | The blanks | h, i, X, (, ) |
| `\S` | Any character that is not a whitespace | `\S` | (hi X ) | h, i, X, (, ) | The blanks |
| `\w` | Any word character (letter, digit or the underscore), alternative: `[a-zA-Z0-9_]` | `\w` | (ab 12_c) | a, b, c, 1, 2, _ | (, ), the blank |
| `\W` | Any character that is not a word character, alternative: `[^a-zA-Z0-9_]` | `\W` | (ab 12_c) | (, ), the blank | a, b, c, 1, 2, _ |
| `\` | To include special characters like `[] \ / ^`, use `\` to escape them. Use `\.` to match a period ("."). | `.\.` | ab.cd.ef |  a<ins>**b.**</ins>c<ins>**d.**</ins>ef | ab<ins>**.c**</ins>d<ins>**.e**</ins>f |

Repetitions and Alternatives

| Expression | Represents | Example Regex | Example String | Matches | Does not Match |
|---|---|---|---|---|---|
| `x*` | Zero or more repetitions of `x` | `ab*` | abc abbc abbbc a ac | <ins>**ab**</ins>c <ins>**abb**</ins>c <ins>**abbb**</ins>c <ins>**a**</ins> <ins>**a**</ins>c | <ins>**abc**</ins> <ins>**abbc**</ins> <ins>**abbbc**</ins> a <ins>**ac**</ins> |
| `x+` | One or more repetitions of `x` | `ab+` | abc abbc abbbc a ac | <ins>**ab**</ins>c <ins>**abb**</ins>c <ins>**abbb**</ins>c a ac | ... <ins>**a**</ins> <ins>**a**</ins>c |
| `x{m,n}` | Between `m` and `n` repetitions of `x` | `ab{2,3}` | abc abbc abbbc a ac | abc <ins>**abb**</ins>c <ins>**abbb**</ins>c a ac  | <ins>**ab**</ins>c ... |
| `x{m}` | Exactly `m` repetitions | `ab{3}` | abc abbc abbbc a ac | abc abbc <ins>**abbb**</ins>c a ac | abc <ins>**abb**</ins>c ... |
| `x{m,}` | Exactly `m` or more repetitions | `ab{2,}` | abc abbc abbbc a ac | abc <ins>**abb**</ins>c <ins>**abbb**</ins>c a ac  | <ins>**ab**</ins>c ... |
| `x?` | Optional `x`, i.e. zero or one time  | `ab?` | abc abbc abbbc a ac | <ins>**ab**</ins>c <ins>**ab**</ins>bc <ins>**ab**</ins>bbc <ins>**a**</ins> <ins>**a**</ins>c  | ... <ins>**ac**</ins>  |
| `x\|y` | Matching alternatives, i. e. `x` or `y` | 1. `b\|2` <br> 2. `b(a\|u)t`  | 1. abc 123 <br> 2. bit bat but bet |  1. b, 2 <br> 2. bat, but | 1. a, c, 1, 3 <br> 2. bit, bet | 
| `x*?` | `x*` captures greedily, i.e. as much as possible, while `x*?` captures non-greedily, i.e. as few as possible  | 1. `bc*?` <br> 2. `a.*?#`  | 1. abcd abccccd ab<br> 2. abc#defgh#i | 1. a<ins>**b**</ins>cd a<ins>**b**</ins>ccccd a<ins>**b**</ins><br> 2. <ins>**abc#**</ins>defgh#i | 1. a<ins>**bc**</ins>d a<ins>**bcccc**</ins>d a<ins>**b**</ins> (result for `bc*`) <br> 2. <ins>**abc#defgh#**</ins>i (result for `a.*#`) |
| `x+?` | Same as above: `x+` (greedy), `x+?` (non-greedy) | 1. `bc+?` <br> 2. `<.+?>` | 1. abcd abccccd ab<br> 2. &lt;span>Hallo&lt;/span> html. | 1. a<ins>**bc**</ins>d a<ins>**bc**</ins>cccd ab<br> 2. <ins>**&lt;span>**</ins>Hallo<ins>**&lt;/span>**</ins> html. | 1. a<ins>**bc**</ins>d a<ins>**bcccc**</ins>d ab (result for `bc+`) <br> 2. <ins>**&lt;span>Hallo&lt;/span>**</ins> html.  (result for `<.+>`) |

Character Sets, Ranges, Subgroups and Lookarounds
| Expression | Represents | Example Regex | Example String | Matches | Does not Match |
|---|---|---|---|---|---|
| `[xy]` | Character set, matches a single character present in the list | `b[iu]` | bit bat but bet | <ins>**bi**</ins>t bat <ins>**bu**</ins>t bet  | bit <ins>**ba**</ins>t but <ins>**be**</ins>t |
| `[x-y]` | Character range, matches a single character in the specified range, note that ranges may be locale-dependent | `a[a-c0-5]` | aa1 ab2 ba3 cac4 da56 a7 |<ins>**aa**</ins>1 <ins>**ab**</ins>2 b<ins>**a3**</ins> c<ins>**ac**</ins>4 d<ins>**a5**</ins>6 a7 | aa1 ab2 ba3 cac4 da56 <ins>**a7**</ins> |
| `[^xy]` | Negation, matches any single character not present in the list | `[^Ap]` | ABap | B, a | A, p |
| `[^x-y]` | Negation, matches any single character not within the range | `[^A-Ca-c1-4]` | ABCDabcd123456 | D, d, 5, 6 | A, B, C, a, b, c, 1, 2, 3, 4 |
| `(...)` | Capturing group to group parts of patterns together | `b(a\|u)t` | bit bat but bet | bat, but | bit, bet |
| `(?=...)` | Positive lookahead, returns characters that are followed by a specified pattern without including this pattern | `a(?=b)` | abc ade | <ins>**a**</ins>bc ade | abc <ins>**a**</ins>de |
| `(?!...)` | Negative lookahead, returns characters that are not followed by a specified pattern without including this pattern | `a(?!b)` | abc ade | abc <ins>**a**</ins>de | <ins>**a**</ins>bc ade |
| `(?<=...)` | Positive lookbehind, returns characters that are preceded by a specified pattern without including this pattern | `(?<=\s)c` | ab c abcd | ab <ins>**c**</ins> abcd (it is preceded by a blank) | ab c ab<ins>**c**</ins>d |
| `(?<!...)` | Negative lookbehind, returns characters that are not preceded by a specified pattern without including this pattern | `(?<!\s)c` | ab c abcd | ab c ab<ins>**c**</ins>d (it is not preceded by a blank) | ab <ins>**c**</ins> abcd |
| `\n` | Backreference, refers to a previous capturing group; n represents the number of the group index that starts with 1 | `(a.)(\w*)\1` | abcdefabghij | <ins>**abcdefab**</ins>ghij <br>Note: Capturing group 1 holds `ab` in the example. The second capturing group captures all word characters until `ab` is found. | <ins>**ab**</ins>cdefabghij |
| `\K` | Resets the starting point of a match, i.e. findings are excluded from the final match | `a.\Kc` | abcd | ab<ins>**c**</ins>d | <ins>**abc**</ins>d |

> **💡 Note**<br>
> - Subgroups are useful in replacements. By using an expression with `$` and a number, such as `$1`, you can refer to a specific group. For example, you have a string `abcde`. A PCRE expression might be
`(ab|xy)c(d.)`, where two subgroups are specified within two pairs of parentheses. In a replacement pattern, you can refer to the first group with `$1` and the second group with `$2`. Thus, the replacement pattern `$2Z$1` results in `deZab`.
> - `(?:x)` creates a group but it is not captured. Example regular expression: `(?:ab)(ap)`. Example string: 'abap'. It matches 'abap', but `$1` will only contain 'ap'. 

Anchors and Positions

| Expression | Represents | Example Regex | Example String | Matches | Does not Match |
|---|---|---|---|---|---|
| `^` | Start of line, alternative: `\A` | `^.` or `\A.` | abc def | <ins>**a**</ins>bc def | abc <ins>**d**</ins>ef  |
| `$` | End of line, alternative: `\Z` | `.$` or `.\Z` | abc def | abc de<ins>**f**</ins> | <ins>**a**</ins>bc def |
| `\b` | Start or end of word | 1. `\ba.` <br>2. `\Dd\b` <br>3. `\b.d\b` | abcd a12d ed | 1. <ins>**ab**</ins>cd <ins>**a1**</ins>2d ed <br>2. ab<ins>**cd**</ins> a12d <ins>**ed**</ins> <br> 3. abcd a12d <ins>**ed**</ins> | 1. ab<ins>**cd**</ins> a1<ins>**2d**</ins> ed <br> 2. abcd a1<ins>**2d**</ins> ed <br> 3. <ins>**abcd**</ins> <ins>**a12d**</ins> ed |
| `\B` | Negation of `\b`, not at the start or end of words | `\Be\B` | see an elefant | s<ins>**e**</ins>e an el<ins>**e**</ins>fant  | s<ins>**ee**</ins> an <ins>**e**</ins>lefant |

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Searching Using Regular Expressions

- Multiple string functions support PCRE expressions by offering the
 `pcre` parameter, which you can use to specify such an expression. 
`FIND` and `REPLACE` statements support regular
expressions with the `PCRE` addition. 
- The string function
[`match`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmatch_functions.htm)
works only with regular expressions. It returns a substring that
matches a regular expression within a string. 
- For comparisons, you can
also use the [predicate
function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpredicate_function_glosry.htm "Glossary Entry")
[`matches`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmatches_functions.htm), which returns true or false if a string matches a given pattern or not.

Syntax examples:
``` abap
DATA(s1) = `Cathy's black cat on the mat played with Matt.`.

"Determining the position of the first occurrence
"Here, the parameter occ is 1 by default.
DATA(int) = find( val = s1 pcre = `at.` ). "1

"Determining the number of all occurrences.
"Respects all 'a' characters not followed by 't', all 'at' plus 'att'
int = count( val = s1  pcre = `at*` ). "6

"Respects all 'at' plus 'att'
int = count( val = s1 pcre = `at+` ). "4

"Extracting a substring matching a given pattern
DATA(s2) = match( val = `The email address is jon.doe@email.com.`
                  pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ). "jon.doe@email.com

"Predicate function matches
"Checking the validitiy of an email address
IF matches( val  = `jon.doe@email.com`
            pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ).  "true
...
ENDIF.

"Examples with the FIND statement
"SUBMATCHES addition: Storing submatches in variables
"Pattern: anything before and after ' on '
FIND PCRE `(.*)\son\s(.*)` IN s1 IGNORING CASE SUBMATCHES DATA(a) DATA(b).
"a: 'Cathy's black cat' / b: 'the mat played with Matt.'.

"Determining the number of letters in a string
FIND ALL OCCURRENCES OF PCRE `[A-Za-z]` IN s1 MATCH COUNT DATA(c). "36

"Searching in an internal table and retrieving line, offset, length information
DATA(itab) = value string_table( ( `Cathy's black cat on the mat played with the friend of Matt.` ) ).
"Pattern: 't' at the beginning of a word followed by another character
FIND FIRST OCCURRENCE OF PCRE `\bt.` IN TABLE itab
  IGNORING CASE MATCH LINE DATA(d) MATCH OFFSET DATA(e) MATCH LENGTH DATA(f). "d: 1, e: 21, f: 2
```
<p align="right"><a href="#top">⬆️ back to top</a></p>

##### Excursion: System Classes for Regular Expressions


- You can create an object-oriented representation of regular expressions using the `CL_ABAP_REGEX` system class.
- For example, the `CREATE_PCRE` method creates instances of regular expressions with PCRE syntax.
- The instances can be used, for example, with the `CL_ABAP_MATCHER` class, which applies the regular expressions.
- A variety of methods and parameters can be specified to accomplish various things and to further specify the handling of the regular expression.
- More information can be found [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenregex_system_classes.htm) and in the class documentation (choose F2 on the class in ADT).


``` abap
DATA(str) = `a1 # B2 ? cd . E3`.

"Creating a regex instance for PCRE regular expressions
"In the example, regex_inst has the type ref to cl_abap_regex. 
DATA(regex_inst) =  cl_abap_regex=>create_pcre( pattern = `\D\d`           "any-non digit followed by a digit
                                                ignore_case = abap_true ).

"Creating an instance of CL_ABAP_MATCHER using the method CREATE_MATCHER of the class CL_ABAP_REGEX
"You can also specify internal tables with the 'table' parameter and more.
DATA(matcher) = regex_inst->create_matcher( text = str ).

"Finding all results using the 'find_all' method
"In the example, result has the type match_result_tab containing the findings.
DATA(result) = matcher->find_all( ).

"Using method chaining
DATA(res) = cl_abap_regex=>create_pcre( pattern = `\s\w`       "any blank followed by any word character
                                        ignore_case = abap_true )->create_matcher( text = str )->find_all( ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Replacing Using Regular Expressions

- To perform replacement operations using regular expressions, you can use both
the string function `replace` and `REPLACE` statements with the `pcre` parameter or the `PCRE` addition. 
- Like the `find` function, among others, and
`FIND` statements, the `replace` function and
`REPLACE` statements offer a number of parameters and additions that you can use to further restrict the area to be replaced. 
- For more detailed information, refer to the ABAP
Keyword Documentation. 
- The executable example covers many of the PCRE expressions listed above.

Syntax examples:
``` abap
DATA(s1) = `ab apppc app`.
DATA s2 TYPE string.

"Replaces 'p' with 2 - 4 repetitions, all occurences
s2 = replace( val = s1 pcre = `p{2,4}` with = `#` occ = 0 ). "ab a#c a#

"Replaces any single character not present in the list, all occurences
s2 = replace( val = s1 pcre = `[^ac]` with = `#` occ = 0 ). " "a##a###c#a##

"Replaces first occurence of a blank
s2 = replace( val = s1 pcre = `\s` with = `#` ). "ab#apppc app

"Greedy search
"The pattern matches anything before 'p'. The matching is carried out as
"often as possible. Hence, in this example the search stretches until the
"end of the string since 'p' is the final character, i. e. this 'p' and
"anything before is replaced.
s2 = replace( val = s1 pcre = `.*p` with = `#` ). "#

"Non-greedy search
"The pattern matches anything before 'p'. The matching proceeds until
"the first 'p' is found and does not go beyond. It matches as few as
"possible. Hence, the first found 'p' including the content before
"is replaced.
s2 = replace( val = s1 pcre = `.*?p` with = `#`  ). "#ppc app

"Replacements with subgroups
"Replaces 'pp' (case-insensitive here) with '#', the content before and after 'pp' is switched
s2 = replace( val  = s1
              pcre = `(.*?)PP(.*)`
              with = `$2#$1`
              case = abap_false ). "pc app#ab a

"Changing the source field directly with a REPLACE statement; same as above
REPLACE PCRE `(.*?)PP(.*)` IN s1 WITH `$2#$1` IGNORING CASE. "pc app#ab a
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

[zcl_demo_abap_string_proc](./src/zcl_demo_abap_string_proc.clas.abap)

Follow the steps outlined [here](README.md#-getting-started-with-the-examples) to import and run the code.
