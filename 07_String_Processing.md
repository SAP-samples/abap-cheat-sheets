<a name="top"></a>

# String Processing

- [String Processing](#string-processing)
  - [Introduction](#introduction)
  - [Data Types for Character Strings](#data-types-for-character-strings)
    - [Differences Between Text Strings (Variable Length) and Text Fields (Fixed Length)](#differences-between-text-strings-variable-length-and-text-fields-fixed-length)
  - [Declaring Character-Like Data Objects](#declaring-character-like-data-objects)
  - [Assigning Values](#assigning-values)
  - [String Templates](#string-templates)
    - [Control Characters in String Templates](#control-characters-in-string-templates)
    - [Formatting Options in String Templates](#formatting-options-in-string-templates)
  - [Determining the Length of Strings](#determining-the-length-of-strings)
  - [Concatenating Strings](#concatenating-strings)
    - [Concatenation Assignment Operator \&\&=](#concatenation-assignment-operator-)
    - [Literal Operator](#literal-operator)
  - [Splitting Strings](#splitting-strings)
  - [Modifying Strings](#modifying-strings)
    - [Transforming to Lowercase and Uppercase](#transforming-to-lowercase-and-uppercase)
    - [Shifting Content](#shifting-content)
    - [Condensing Strings](#condensing-strings)
    - [Reversing Strings](#reversing-strings)
    - [Inserting Substrings into Strings](#inserting-substrings-into-strings)
    - [Overlaying Content](#overlaying-content)
  - [Accessing and Processing Substrings](#accessing-and-processing-substrings)
  - [Searching and Replacing](#searching-and-replacing)
    - [Searching for Specific Characters](#searching-for-specific-characters)
    - [Replacing Specific Characters in Strings](#replacing-specific-characters-in-strings)
    - [Searching for Substrings in Strings](#searching-for-substrings-in-strings)
    - [Searching for Substrings in Tables](#searching-for-substrings-in-tables)
    - [String Function find](#string-function-find)
    - [Replacing Substrings in Strings](#replacing-substrings-in-strings)
      - [Position-Based Replacements](#position-based-replacements)
    - [Replacing Substrings in Tables](#replacing-substrings-in-tables)
    - [String Function replace](#string-function-replace)
  - [Pattern-Based Searching and Replacing in Strings](#pattern-based-searching-and-replacing-in-strings)
    - [Simple Pattern-Based Searching Using Comparison Operators](#simple-pattern-based-searching-using-comparison-operators)
    - [Complex Searching and Replacing Using Regular Expressions](#complex-searching-and-replacing-using-regular-expressions)
  - [More String Functions](#more-string-functions)
    - [Checking the Similarity of Strings](#checking-the-similarity-of-strings)
    - [Repeating Strings](#repeating-strings)
    - [Returning the Smallest/Biggest of a Set of Character-Like Arguments](#returning-the-smallestbiggest-of-a-set-of-character-like-arguments)
    - [Escaping Special Characters](#escaping-special-characters)
  - [Excursions](#excursions)
    - [Comparison Operators for Character-Like Data Types in a Nutshell](#comparison-operators-for-character-like-data-types-in-a-nutshell)
    - [Classes for String Processing](#classes-for-string-processing)
    - [Byte String Processing](#byte-string-processing)
      - [Determining the Length of xstrings](#determining-the-length-of-xstrings)
      - [Character String and Byte String Processing with ABAP Statements](#character-string-and-byte-string-processing-with-abap-statements)
      - [SET BIT and GET BIT Statements](#set-bit-and-get-bit-statements)
  - [Executable Example](#executable-example)


## Introduction

ABAP offers plenty of options for processing [character strings](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencharacter_string_glosry.htm "Glossary Entry").
The options include ABAP statements (e. g. [`FIND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfind.htm)),
[character string expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_expression_glosry.htm "Glossary Entry")
([concatenations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconcatenation_glosry.htm) and [string templates](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_template_glosry.htm "Glossary Entry"))
and built-in [string functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_function_glosry.htm "Glossary Entry")
(e. g. [`strlen`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlength_functions.htm)).

> [!NOTE]  
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
| `c` | For fixed length character strings. Data objects of this type are [static data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_data_object_glosry.htm), i. e. the length of a variable must be defined during its declaration and does not change during the execution of an ABAP program. Thus, it always contains character strings of the same length. A data object of type `c` is called *text field*.|Data objects of this type can contain a string of fixed length (between 1 and 262143 characters); standard length: 1 | Same as for `string` | A blank for each position |

In addition to these main data types for character strings, there are several other fixed length data types with special meanings:

-	`n` for fixed length numerical character strings
    - Data objects of this type are technically almost the same as text fields. However, the only valid characters are the digits 0 to 9. Validity is not checked for assigning values in a regular way but only for [lossless assignments](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlossless_assignment_glosry.htm). Thus, such numeric text fields can contain invalid data, but should only be used for digits that are not intended for arithmetic calculations, such as zip codes or article numbers. The initial value for each position is 0.
-	`d` and `t` for date and time fields
    - These data types have a predefiend length of 6 and 8. Data objects of these types are used for character representations of dates and times in a predefined format. You can use them directly in date and time calculations. However, these fields can also contain invalid values.

These data types are not covered further in this cheat sheet. Note that there are the [byte-like data types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbyte_like_data_typ_glosry.htm "Glossary Entry") `x` and `xstring` that are closely related to `c` and `string` but contain raw [byte strings](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbyte_string_glosry.htm).

### Differences Between Text Strings (Variable Length) and Text Fields (Fixed Length)
- **Initial value**: The initial value of a text string is an
    empty string of length 0. The initial value of text field is represented by blanks at each position.
-   **Internal representation**: Data objects of type `c` and `string` are both [elementary data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenelementary_data_object_glosry.htm "Glossary Entry").
    However, while text fields occupy a block of memory according to their length, text strings are so-called [deep](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_glosry.htm "Glossary Entry") data objects. Internally, they are managed by a [reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_glosry.htm "Glossary Entry") that points to the actual character. This fact has restrictive consequences for the use of strings as components of structures, but can also improve the performance of assignments due to the concept of [sharing](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensharing_glosry.htm "Glossary Entry") of deep data objects.
- **Length**: Theoretically, a text string can use up to 2 GB (one character occupies 2 bytes).
    The maximum length of a text field is 262143 characters.
- **Trailing blanks**: For text strings, trailing blanks are preserved in all operations. For text fields, it depends on the [operand
    position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm "Glossary Entry") whether trailing blanks are respected or not. In most operand positions, trailing blanks are truncated when working with text fields, even when using [text field literals](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentext_field_literal_glosry.htm). For example, if a text field is assigned to a text string, the resulting target string will never contain trailing blanks. See the *Condensing Strings* section in this context.
- **Flexibility**: Text strings are more flexible than text fields
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
CLASS zcl_demo_abap DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
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
      tstmp TYPE timestampl,   "DDIC type 
      char3 TYPE zdemo_abap_flsch-carrid. "Using the type of a DDIC table component

"You may also encounter declarations with type c and the length
"specified in parentheses. This is not recommended, to avoid confusion
"with the use of parentheses in dynamic programming.

DATA char(4) TYPE c.

"Just a TYPE c specification without length means LENGTH 1.
DATA char_len_one TYPE c.
"No type and length specification: TYPE c LENGTH 1 by default
DATA char_no_type_len.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Assigning Values

- When you declare character-like data objects, you can specify start values directly with the `VALUE` addition, e.g. `DATA chars TYPE c LENGTH 3 VALUE 'abc'.`. 
- Various ABAP statements assign values. You can do value assignments to data objects using the [assignment operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm "Glossary Entry") `=`.
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
"Note: In newer ABAP releases, the following statement shows a syntax 
"warning that the value of the literal (intentionally specified 
"here like this) is not an admissable value for the target type. 
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


<p align="right"><a href="#top">⬆️ back to top</a></p>

## String Templates
- Using string templates, you can construct strings very elegantly from
literal text and - which is the primary use case - by including
embedded ABAP
[expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenexpression_glosry.htm "Glossary Entry")
within a pair of delimiters (`|...|`) if these expressions can be converted to `string`. 
- To embed expressions, you enclose them in curly brackets: `{ ... }`.
- Among the expressions that can be specified in the curly brackets are data objects and [functional method calls](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfunctional_method_call_glosry.htm) that have a [return value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreturn_value_glosry.htm). The expression result must be convertible to type `string`.

> [!NOTE]  
> - String templates form a [string
expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_expression_glosry.htm "Glossary Entry")
that is compiled at runtime. Therefore, a string template that contains only
literal text is treated as an expression, which has a performance impact. In such a case, it is preferable to use a text string literal with backquotes.
> - It is possible to dynamically specify formatting options. For more information, refer to the [Dynamic Formatting Option Specifications in String Templates](/06_Dynamic_Programming.md#dynamic-formatting-option-specifications-in-string-templates) section of the *Dynamic Programming* cheat sheet.
> - Escape `\|{}` in string templates using `\`, i. e. `\\` means `\`.

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

"Selection of possible expressions:
"Data objects, as in the examples above
DATA(dobj1) = `Hallo`.
DATA(dobj2) = '!'.

"Hallo!
DATA(s5) = |{ dobj1 }{ dobj2 }|. 

"NOT INITIAL
DATA(s6) = |{ COND #( WHEN s5 IS INITIAL THEN `INITIAL` ELSE `NOT INITIAL` ) }|. 

"Functional method calls, built-in functions
"User alias: XXXX...
DATA(s7) = |User alias: { cl_abap_context_info=>get_user_alias( ) }|. 

"Some random number: 39 (example)
DATA(s8) = |Some random number: { cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 1 max = 100 )->get_next( ) }|. 

"Length of string s5: 6
DATA(s9) = |Length of string s5: { strlen( s5 ) }|.

"Current UTC time stamp: 2024-01-11 14:27:54.1514090 (example)
DATA(s10) = |Current UTC time stamp: { utclong_current( ) }|.

"HALLO!
DATA(s11) = |{ to_upper( s5 ) }|.

"\ | { }
DATA(s12) = |\\ \| \{ \}|.
```

### Control Characters in String Templates
- String templates interpret certain character combinations as [control
characters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_templates_separators.htm).
- For example, `\n` is interpreted as a newline. A new line is
started. 

The following syntax examples demonstrate a selection:
```abap
"\n is interpreted as a line feed
DATA(s1) = `Hello`.
DATA(s2) = `World`.
DATA(s3) = |{ s1 }\n{ s2 }\nHow are you\n?|.

*Hello
*World
*How are you
*?

"Excursion: The CL_ABAP_CHAR_UTILITIES class provides attributes and methods as utilities for string processing.
"The following examples demonstrate that attributes that contain control characters can be replaced by
"a representation of control characters in a string template.
ASSERT cl_abap_char_utilities=>newline        = |\n|.
ASSERT cl_abap_char_utilities=>horizontal_tab = |\t|.
ASSERT cl_abap_char_utilities=>cr_lf          = |\r\n|.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Formatting Options in String Templates
- String templates support various formatting options.
- The following syntax examples demonstrate a selection. For information about all options, refer to [this topic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcompute_string_format_options.htm) in the ABAP Keyword Documentation.

```abap
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
DATA dat type d value '20240101'.
DATA tim type t value '123456'.
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
s1 = |{ '1234' ALPHA = IN WIDTH = 10 }|. "0000001234
s1 = |{ '00000000000000000000000012' ALPHA = IN WIDTH = 10 }|. "0000000012
"Fixed-length string provided, WIDTH not specified
s1 = |{ '   12' ALPHA = IN }|. "00012
"Removing leading zeros
s1 = |{ '00001234' ALPHA = OUT }|. "1234
"Do not apply formatting
s1 = |{ '00001234' ALPHA = RAW }|. "00001234
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Determining the Length of Strings

- To determine the length of a string, you can use the string function
[`strlen`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlength_functions.htm).
- Note that the result depends on the type of the string, i. e. the result for a data object of type `string` includes trailing blanks. A
fixed-length string does not include them. 
- To exclude trailing blanks in all cases, regardless of the data type, you can use the built-in
[`numofchar`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlength_functions.htm) function.
- [`xstrlen`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendescriptive_functions_binary.htm) returns the number of bytes of a byte-like argument.

Syntax examples:
``` abap
"strlen
DATA(len_c_a)   = strlen( 'abc   ' ).    "3
DATA(len_c_b)   = strlen( '   abc   ' ). "6
DATA(len_str_a) = strlen( `abc   ` ).    "6
DATA(len_str_b) = strlen( `   abc   ` ). "9

"numofchar
len_c_a   = numofchar( 'abc   ' ).    "3
len_c_b   = numofchar( '   abc   ' ). "6
len_str_a = numofchar( `abc   ` ).    "3
len_str_a = numofchar( `   abc   ` ). "6

DATA(xstr) = CONV xstring( `480065006C006C006F00200077006F0072006C0064002100` ).
DATA(len_xstr) = xstrlen( xstr ). "24
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Concatenating Strings

- Two or more strings can be concatenated using the concatenation operator
`&&` and string templates. Alternatively, you can use
[`CONCATENATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconcatenate.htm)
statements. 
- It is also possible to concatenate lines from internal tables with character-like line type
into a string to avoid a loop. 
- A more modern way is to use
the string function
[`concat_lines_of`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconcatenation_functions.htm).

Syntax examples:
``` abap
"&& and string template
"abcd
DATA(s1) = `ab` && `cd`.
"efgh abcd
DATA(s2) = `ef` && `gh` && ` ` && s1.
"abcd. efgh abcd!
DATA(s3) = |{ s1 }. { s2 }!|.

"CONCATENATE statements
"uvwxyz
DATA(s4) = `uvw`.
DATA(s5) = `xyz`.
CONCATENATE s4 s5 INTO s3.

"Multiple data objects and target declared inline
"uvw xyz
CONCATENATE s4 ` ` s5 INTO DATA(s6).

"abcdefgh abcduvwxyz
CONCATENATE s1 s2 s4 s5 INTO DATA(s7).

"You can also add a separation sign using the addition SEPARATED BY
"uvw xyz
CONCATENATE s4 s5 INTO DATA(s8) SEPARATED BY ` `.

"uvw#xyz
CONCATENATE s4 s5 INTO DATA(s9) SEPARATED BY `#`.

"Keeping trailing blanks in the result when concatenating fixed length
"strings. The ones of variable length strings are respected by default.
CONCATENATE 'a  ' 'b  ' 'c  ' INTO DATA(ch) RESPECTING BLANKS. "'a  b  c  '

"Concatenating lines of internal tables with character-like line type into a string
DATA(itab) = VALUE string_table( ( `abc` ) ( `def` ) ( `ghi` ) ).

"abcdefghi
CONCATENATE LINES OF itab INTO DATA(conc_tab1).

"abc def ghi
CONCATENATE LINES OF itab INTO DATA(conc_tab2) SEPARATED BY ` `.

"abc,def,ghi
CONCATENATE LINES OF itab INTO DATA(conc_tab3) SEPARATED BY `,`.

"Using concat_lines_of
s1 = concat_lines_of( table = itab ). "Without separator
s1 = concat_lines_of( table = itab sep = ` ` ). "With separator
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Concatenation Assignment Operator &&=

- You can use the concatenation assignment operator `&&=` to concatenate strings in an assignment.

```abap
"This assignment using the concatenation assignment operator ...
a &&= b.

"... has the same effect as the following.
a = a && b.
```

- `a` can be variables and writable expressions (such as certain constructor expressions, e.g. `CAST`, or table expressions). It cannot be an inline declaration.
- `b` can be character-like data objects, constructor expressions, string expressions (such as string templates), table expressions or functions

```abap
DATA a1 TYPE string VALUE `a`.
DATA a2 TYPE string VALUE `a`.
DATA b TYPE string VALUE `b`.

a1 &&= b.
a2 = a2 && b.

ASSERT a1 = a2.

*&---------------------------------------------------------------------*
*& Left-hand data object
*&---------------------------------------------------------------------*
"The left-hand data object can also be represented by a writable expression

"Constructor expression with CAST
DATA ref TYPE REF TO data.
ref = NEW string( `h` ).
CAST string( ref )->* &&= `ello`. "hello

"Table expression
DATA(str_table) = VALUE string_table( ( `AB` ) ).
str_table[ 1 ] &&= `AP`. "ABAP

*&---------------------------------------------------------------------*
*& Right-hand data object
*&---------------------------------------------------------------------*

"The right-hand data object can be represented by a character-like data object, as
"shown in the previous examples, or whose content can be converted, as well as ...

"... constructor expressions
DATA txt TYPE string.
txt = `abc`.
"abcdef
txt &&= CONV string( 'def' ).

txt = `ghi`.
"The REDUCE operator itself uses the concatenation assignment operator
"in the assignment of NEXT.
"ghijkl
txt &&= REDUCE string( INIT str = VALUE #( )
                       FOR line IN VALUE string_table( ( `j` ) ( `k` ) ( `l` ) )
                       NEXT str &&= line ).

txt = `mno`.
"mnopqr
txt &&= COND #( WHEN txt = `mno` THEN `pqr` ELSE `stu` ).

"... string expressions such as string templates
CLEAR txt.
DO 5 TIMES.
  "1, 2, 3, 4, 5
  txt &&= |{ COND #( WHEN sy-index <> 1 THEN `, ` ) }{ sy-index }|.
ENDDO.

"... table expressions
DATA(tab) = VALUE string_table( ( `v` ) ( `w` ) ( `x` ) ).
txt = `a`.
"av
txt &&= tab[ 1 ].

"Content of tab[ 2 ]: wx
tab[ 2 ] &&= tab[ 3 ].

"... functions
txt = `a`.
"abbbbb
txt &&= repeat( val = `b` occ = 5 ).

DATA(strtab) = VALUE string_table( ( `b` ) ( `c` ) ( `d` ) ).
txt = `a`.
"abcd
txt &&= concat_lines_of( table = strtab ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Literal Operator 

The [literal operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenliteral_operator_glosry.htm "Glossary Entry") `&` combines [text string literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentext_string_literal_glosry.htm "Glossary Entry"), however, with [significant differences](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenliteral_operator.htm) to `&&`, which the code snippet addresses.


```abap
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
"15
DATA(output_length) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( char_with_blanks ) )->output_length.

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
"4
DATA(len1) = strlen( char_with_blanks_conc_op ).
"4
DATA(len2) = numofchar( char_with_blanks_conc_op ).
``` 

## Splitting Strings

- You can use
[`SPLIT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapsplit.htm)
statements to split strings in multiple segments. 
- The result of the
split can be stored in separate data objects or internal tables that
have a character-like line type. 
- Note that if the number of specified targets is
less than the number of segments returned by the split, the last target receives the remaining unsplit segments. If more targets are specified, the targets that do not receive a segment are
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

"SPLIT statement specifying a colon after INTO
DATA(some_text) = `Lorem ipsum dolor sit amet, consectetur adipiscing elit`.

SPLIT some_text AT ` ` INTO: DATA(str1) DATA(str2) DATA(str3) DATA(str4), TABLE DATA(tab).

"Result
"str1: Lorem
"str2: ipsum
"str3: dolor
"str4: sit amet, consectetur adipiscing elit

"tab:
*Lorem        
*ipsum        
*dolor        
*sit          
*amet,        
*consectetur  
*adipiscing   
*elit
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Modifying Strings
### Transforming to Lowercase and Uppercase

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

"For the transformation of the source directly, you can 
"also specify an existing, changeable data object as 
"the source in a simple assignment as follows.
s1 = to_upper( s1 ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Shifting Content

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
DATA(s5) = s4.
SHIFT s4 RIGHT DELETING TRAILING ` `. "'   hallo'
SHIFT s4 LEFT DELETING LEADING ` `. "'hallo'

"To remove trailing blanks, you can also use the following method of the
"cl_abap_string_utilities class.
cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = s5 ).
ASSERT s4 = s5.
"Note the c2str_preserving_blanks method for preserving trailing blanks 
"when assigning text fields to data objects of type string.

"String functions with parameters
s1 = `hallo`.

s2 = shift_left( val = s1 places = 3 ). "lo
s2 = shift_left( val = s1 circular = 2 ). "lloha

"Note that shift_right does not extend a variable length string.
s2 = shift_right( val = s1 places = 3 ). "ha
s2 = shift_right( val = `abc   ` sub = ` ` ). "'abc'
s2 = shift_right( val = `abc   ` ). "'abc' (same result as above)
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Condensing Strings

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Reversing Strings

The string function
[`reverse`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreverse_functions.htm)
reverses a string:
``` abap
"Result: 'abap'
DATA(s1) = reverse( `paba` ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Inserting Substrings into Strings

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Overlaying Content

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

## Accessing and Processing Substrings

- The string function
[`substring`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm) allows you to specify the position (parameter `off`) and the length
(`len`) of a substring to be extracted from a given
string (`val`). 
  - At least one of the two parameters `off`
or `len` must be specified. The default value of `off`
is 0, i.e. when using the default value, the substring is extracted
from the beginning of the string. 
  - If `len` is not specified, the rest of the remaining characters is respected. If the offset
and length are greater than the actual length of the string, the
exception `CX_SY_RANGE_OUT_OF_BOUNDS` is raised.
- You can also access substrings by specifying certain data objects in certain positions using the data object name (field symbols and dereferenced data reference variables are also possible) and the length in parentheses. 
  - Additionally, you can specify the offset using the `+` character after a variable. 
  - Specifying the length with an asterisk (`*`) means that the rest of the remaining string is respected. 
  - This syntax option even allows write access to substrings for fixed-length strings. Read access is possible for both fixed-length and variable-length strings.
  - However, this syntax can be confused with the use of tokens in the context of  dynamic programming.
- There are other string functions available for dealing with substrings, such as 
[`substring_after`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm),
[`substring_before`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm),
[`substring_from`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm)
and
[`substring_to`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm).
  - These functions offer more options in terms of parameters, such as the use of [PCRE regular
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpcre_regex_glosry.htm "Glossary Entry").
- As also shown further down, using the built-in function `match`, you can extract substrings matching a given pattern.

Syntax examples:
``` abap
DATA(s1) = `Lorem ipsum dolor sit amet`. "Type string

"Extracting substring starting at a specific position
"'len' not specified means the rest of the remaining characters is
"respected
DATA(s2) = substring( val = s1 off = 6 ). "ipsum dolor sit amet

"Extracting substring with a specific length
"'off' is not specified and has the default value 0.
s2 = substring( val = s1 len = 5 ). "Lorem

"Specifying both off and len parameters
s2 = substring( val = s1 off = 6 len = 5 ). "ipsum

DATA(txt) = 'Lorem ipsum dolor sit amet'. "Type c

"Offset and length specification for substring access
"Length specification only in parentheses, no offset
"specification means starting from 0
DATA(ch6) = txt(5). "Lorem

"Offset specification using the + sign after a variable
"The following example corresponds to the previous one
"(offset 0 explicitly specified)
DATA(ch7) = txt+0(5). "Lorem

DATA(ch8) = txt+6(5). "ipsum

"* means respecting the rest of the remaining string
DATA(ch9) = txt+12(*). "dolor sit amet

"Using the syntax in various contexts, e.g. modifying the
"text field
CLEAR txt+11(*). "Lorem ipsum

txt = 'Lorem ipsum dolor sit amet'.
txt(5) = 'Hallo'. "Hallo ipsum dolor sit amet
txt+7(3) = '###'. "Hallo i###m dolor sit amet

"Field symbol
TYPES abc TYPE c LENGTH 26.
FIELD-SYMBOLS <fs> TYPE abc.
DATA(alphabet) = 'abcdefghijklmnopqrstuvwxyz'.
ASSIGN alphabet TO <fs>.
DATA(part_of_abc_1) = <fs>(3). "abc
<fs>+12(*) = '###'. "abcdefghijkl###

"Data reference variable
alphabet = 'abcdefghijklmnopqrstuvwxyz'.
DATA(dref_str) = NEW abc( alphabet ).
DATA(part_of_abc_2) = dref_str->*+20(*). "uvwxyz
dref_str->*+25(1) = '#'. "abcdefghijklmnopqrstuvwxy#

"Further string functions
s1 = `aa1bb2aa3bb4`.

"Extracting a substring ...
"... after a specified substring
s2 = substring_after( val = s1 sub = `aa` ). "1bb2aa3bb4 (only the first occurrence is respected)

"... after a specified substring specifying the occurrence in a string
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

"Extracting a substring matching a given pattern
"AB
s2 = match( val = `ABAP`
            pcre = `.b`         "Any character followed by b/B
            case = abap_false ).
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
"Find an overview in the section about the string function 'find'.
DATA(str) = `Pieces of cakes.`.
DATA res TYPE i.

"find_end returns the sum of the offset of the occurrence plus the length of the match
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
- If you want to replace single characters or fixed sequences of characters with a specified string, you can also use `REPLACE` statements and the `replace` function.

Syntax examples:
``` abap
DATA(s1) = `___abc_def_____ghi_`.
DATA(s2) = translate( val = s1 from = `hi_` to = `?!+` ). "+++abc+def+++++g?!+

"'from' specifies three characters, while 'to' only specifies two
s2 = translate( val = s1 from = `hi_` to = `?!` ). "abcdefg?!

"'to' specifies more characters than 'from'
s2 = translate( val = s1 from = `_`  to = `#?!` ). "###abc#def#####ghi#

"TRANSLATE statement. The value after USING is interpreted as a string composed of character pairs.
"Starting with the first pair, a search is performed in text for the
"first character in every pair and each occurrence is replaced with the
"second character of the pair.
TRANSLATE s1 USING `_.a#g+`. "...#bc.def.....+hi.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Searching for Substrings in Strings

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
  FIRST OCCURRENCE OF 
  "or
  ALL OCCURRENCES OF
    "1. Only the first occurrence is searched
    "2. All occurrences are searched
    "Note: If none of these two additions is specified, only the first occurrence is searched for.
  
  SUBSTRING some_substring 
  "or 
  some_substring
  "or 
  PCRE some_regex  
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

### Searching for Substrings in Tables
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

### String Function find
- Built-in search functions, such as `find`, are available for searching strings.
- They return a return value of type i and contain multiple (optional) parameters.
- `FIND` covers the same functionality and more with the many addition options (e.g. searching in tables).
- For more information, see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensearch_functions.htm)

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
"Searching the first 'e' character from the end
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

### Replacing Substrings in Strings

- [`REPLACE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapreplace.htm) and [`REPLACE ... IN TABLE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapreplace_itab.htm) statements have a similar syntax as `FIND` and `FIND ... IN TABLE` statements. Refer to the ABAP Keyword Documentation for all possible additions. The following code snippets cover a selection.
- `sy-subrc` is set: 0 (search pattern or section was replaced by the specified content, result was not truncated on the right), 2 (search pattern or section was replaced, result was truncated on the right), 4	(search pattern was not found).
-  `REPLACE` statements can be used to directly replace strings (including substrings, which is not possible with the string function `replace` covered below).   

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Position-Based Replacements

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Replacing Substrings in Tables

Replacements in internal tables with `REPLACE ... IN TABLE` (see the notes above for searching in internal tables):
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

<p align="right"><a href="#top">⬆️ back to top</a></p>

### String Function replace
- The string function
[`replace`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreplace_functions.htm),
allows you to store the result of a substring replacement in a separate
variable. 
- What makes it especially powerful is that it
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

"The following examples only highlight that expressions and functions 
"can be specified for the parameter assignment. Find more information on
"regular expressions in the Regular Expressions cheat sheet.
"The examples replace the last character of a string. Note the possibility of 
"invalid access to string position regarding offset specifications that can raise
"the CX_SY_RANGE_OUT_OF_BOUNDS exception (e.g. assume 'str' is initial).
"res: abap ABAP aba#

res = reverse( val = replace( val = reverse( str ) off = 0 len = 1 with = `#` ) ).
res = replace( val = str off = strlen( str ) - 1 len = 1 with = `#` ).
res = replace( val = str pcre = `.\Z` with = `#` ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Pattern-Based Searching and Replacing in Strings

You can perform complex search and replace operations based on
patterns. [PCRE regular
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpcre_regex_glosry.htm "Glossary Entry")
help you process strings effectively.

> [!NOTE]  
> Do not use [POSIX
regular
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenposix_regex_glosry.htm "Glossary Entry")
anymore. They are obsolete.

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

- For more information refer to the [Regular Expressions in ABAP](28_Regular_Expressions.md) cheat sheet.
- It covers the following topics: 
  - An excursion covering common regular expressions (the focus is on PCRE)
  - Regular expressions used in ABAP in the following contexts:
    - `FIND` and `REPLACE` statements (with the `PCRE` addition) 
    - Built-in functions in ABAP with the `pcre` parameter, such as `find`, `find_end`, `count`, `match`, `replace`, `substring_from`, `substring_after`, `substring_before`, `substring_to`  
    - Built-in functions in ABAP SQL and CDS (e.g. `like_regexpr`, `locate_regexpr`, `locate_regexpr_after`, `occurrences_regexpr`, `replace_regexpr`, `substring_regexpr` in ABAP SQL)
    - `CL_ABAP_REGEX` and `CL_ABAP_MATCHER` classes
  
Examples:

```abap
DATA(some_string) = `aa bb cc dd ee`.
DATA(original_string) = some_string.

*&---------------------------------------------------------------------*
*& Statements
*&---------------------------------------------------------------------*

FIND ALL OCCURRENCES OF PCRE `\s` IN some_string RESULTS DATA(findings).
*LINE    OFFSET    LENGTH    SUBMATCHES
*0       2         1         OFFSET     LENGTH
*
*0       5         1         OFFSET     LENGTH
*
*0       8         1         OFFSET     LENGTH
*
*0       11        1         OFFSET     LENGTH
*

"aa#bb#cc#dd#ee
REPLACE ALL OCCURRENCES OF PCRE `\s` IN some_string WITH `#`.

some_string = original_string.

*&---------------------------------------------------------------------*
*& Classes
*&---------------------------------------------------------------------*

"The result is the same as 'findings'
DATA(regex_cl1) = cl_abap_regex=>create_pcre( pattern = `\s` )->create_matcher( text = some_string )->find_all( ).

*&---------------------------------------------------------------------*
*& Built-in Functions
*&---------------------------------------------------------------------*

"2
DATA(find_first_occ) = find( val = some_string pcre = `\s` ).

"8
DATA(find_third_occ) = find( val = some_string pcre = `\s` occ = 3 ).

"3
DATA(find_end_first_occ) = find_end( val = some_string pcre = `\s` ).

"4
DATA(count)  = count( val = some_string pcre = `\s` ).

"aa b
DATA(match) = match( val = some_string
                      pcre = `a.\s.` ).

"aa#bb cc dd ee
DATA(replace_first_occ) = replace( val = some_string pcre = `\s` with = `#` ).

some_string = original_string.

"aa#bb#cc#dd#ee
DATA(replace_all_occ) = replace( val = some_string pcre = `\s` with = `#` occ = 0 ).

"The following examples always use the first occurrence
"` bb cc dd ee`
DATA(substr_from) = substring_from( val = some_string pcre = `\s` ).
    
"`aa `
DATA(substr_to) = substring_to( val = some_string pcre = `\s` ).
    
"`aa`
DATA(substr_before) = substring_before( val = some_string pcre = `\s` ).
    
"bb cc dd ee
DATA(substr_after) = substring_after( val = some_string pcre = `\s` ).

******************************************************************************

"The following snippets demonstrates more examples using regular expressions

"String created with string template and control characters for
"new line, tab, and return
DATA(a) = |A B\nC\tD\rE|.
"Copies for more examples
DATA(b) = a.
DATA(c) = a.

"Replacing any whitespace character
REPLACE ALL OCCURRENCES OF PCRE `\s` IN a WITH `#`.
*A#B#C#D#E

"Replacing any character that is not a whitespace character
REPLACE ALL OCCURRENCES OF PCRE `\S` IN b WITH `#`.

*# #
*#      #
*#

"Note that '.' representing any character includes spaces
"but not new lines, for example
REPLACE ALL OCCURRENCES OF PCRE `.` IN c WITH `#`.

*###
*###
*#

FIND PCRE `\n` IN c.
ASSERT sy-subrc = 0.
FIND PCRE `\r` IN c.
ASSERT sy-subrc = 0.

FIND PCRE `\t` IN c.
ASSERT sy-subrc = 4.
FIND ` ` IN c.
ASSERT sy-subrc = 4.

"In the followig string (that includes HTML tags), the content within
"the p tags shall be replaced. A new line is inlcuded.
"The first example just uses a non-greedy search with '.'.
"The example also demonstrates the replacement with groups. The regular
"expression contains three groups, and two of them shall be included in
"the repalcement.
DATA(html_a) = |<p>Hallo\n</p><p>Ciao!</p><p>Salut.</p>|.
DATA(html_b) = html_a.

REPLACE ALL OCCURRENCES OF PCRE `(<p>)(.*?)(<\/p>)` IN html_a WITH `$1Hi$3`.
*<p>Hallo
*</p><p>Hi</p><p>Hi</p>

"Regular expression: any character or a new line with zero or more repretitions
REPLACE ALL OCCURRENCES OF PCRE `(<p>)(.|\n)*?(<\/p>)` IN html_b WITH `$1Hi$3`.
*<p>Hi</p><p>Hi</p><p>Hi</p>
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More String Functions
As also covered in the [Built-In Functions](24_Builtin_Functions.md) cheat sheet, the following sections show more string functions available.

### Checking the Similarity of Strings

- [`distance`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendistance_functions.htm) returns the Levenshtein distance between two strings, which reflects their similarity.
- Unlike other string functions, the return value has the type `i`.
- Optional addition `max`: Positive integer. The calculation of the Levenshtein distance will stop if the calculated value is greater than this integer.

```abap
DATA(str_to_check) = `abap`.
DATA(dist1) = distance( val1 = str_to_check val2 = `abap` ). "0
DATA(dist2) = distance( val1 = str_to_check val2 = `axbap` ). "1
DATA(dist3) = distance( val1 = str_to_check val2 = `yabyyapy` ). "4
DATA(dist4) = distance( val1 = str_to_check val2 = `zabapzzzzzzzzzzzz` max = 5 ). "5

"If the value of max is 0 or less, an exception is raised.
TRY.
    DATA(dist5) = distance( val1 = str_to_check val2 = `#ab#ap#` max = 0 ).
  CATCH cx_sy_strg_par_val.
    ...
ENDTRY.
```  

### Repeating Strings
- [`repeat`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrepeat_functions.htm) returns a string that contains the content of a specified string for parameter `val` as many times as specified in the parameter `occ`.
- An empty string is returned when `occ` has the value 0 or `val` is empty.

```abap
DATA(repeat1) = repeat( val = `abap` occ = 5 ).  "abapabapabapabapabap
DATA(repeat2) = |#{ repeat( val = ` ` occ = 10 ) }#|. "#          #
DATA(repeat3) = COND #( WHEN repeat( val = `a` occ = 0 ) = `` THEN `Y` ELSE `Z` ). "Y (initial value returned)

"If occ has a negative value, an exception is raised.
TRY.
    DATA(repeat4) = repeat( val = `X` occ = -3 ).
  CATCH cx_sy_strg_par_val.
    ...
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Returning the Smallest/Biggest of a Set of Character-Like Arguments
- [`cmin/cmax`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencmax_cmin_functions.htm) returns a string that contains the content of the smallest or biggest of a set of character-like arguments
- 'Set' means at least two arguments and a maximum of nine argeuments are passed (`valn` operators) for comparison.
- The comparison is made from left to right, and the first different character found determines the smaller or bigger argument.

```abap
DATA(min) =  cmin( val1 = `zzzzzzz`
                   val2 = `zzazzzzzzzz` "smallest argument
                   val3 = `zzzzabc` ).

DATA(max) =  cmax( val1 = `abcdef`      "biggest argument
                   val2 = `aaghij`
                   val3 = `aaaaklmn`
                   val4 = `aaaaaaopqrs`
                   val5 = `aaaaaaaaaatuvwxy`
                   val6 = `aaaaaaaaaaaaaz` ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Escaping Special Characters
- [`escape`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenescape_functions.htm) returns a string that is provided for the `val` parameter by escaping special characters according to the specification in the `format` parameter.
- Suitable values for the `format` parameter (which expects a data object of type `i`) are available in the `CL_ABAP_FORMAT` class (the constants starting with `E_`).
- Special rules apply to different contexts, such as URLS and JSON. Also note the prevention of Cross Site Scripting (XSS) attacks on web applications. For more information, refer to the [documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenescape_functions.htm).

```abap
"Context: URLs
DATA(esc1) = escape( val    = '...test: 5@8...'
                     format = cl_abap_format=>e_url_full ).
"...test%3A%205%408...

"Context: JSON
DATA(esc2) = escape( val    = 'some "test" json \ with backslash and double quotes'
                     format = cl_abap_format=>e_json_string ).
"some \"test\" json \\ with backslash and double quotes


"Context: String templates
DATA(esc3) = escape( val    = 'Special characters in string templates: |, \, {, }'
                     format = cl_abap_format=>e_string_tpl ).
"Special characters in string templates: \|, \\, \{, \}

"Invalid value for the format parameter
TRY.
    DATA(esc4) = escape( val    = 'This will raise an exception due to an invalid format value.'
                         format = 123 ).
    CATCH cx_sy_strg_par_val.
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions 

### Comparison Operators for Character-Like Data Types in a Nutshell 

The comparison operators have already been covered in different sections above. This is a summary of the operators.

<table>
<tr>
<th>Comparison Operator</th>
<th>Details</th>
<th>Example</th>
</tr>
<tr>
    <td><code>CA</code></td>
<td><i>Contains any</i><br>To determine whether any character of a given character set is contained
in a string. <br>Note: The search is case-sensitive. <code>sy-fdpos</code> contains the offset of the first character found, while 0 stands for the very first position. If nothing is found, <code>sy-fdpos</code> contains the length of the string.</td>
<td>

```abap
DATA(s1) = `cheers`.
IF s1 CA `aeiou`. ... "true; sy-fdpos: 2
IF s1 CA `xy`. ...    "false; sy-fdpos: 6
```    

</td>
</tr>
<tr>
<td><code>NA</code></td>
<td><i>Contains not any</i><br>To determine whether any character of a given character set is not contained
in a string. See the note above. </td>
<td>

```abap
DATA(s2) = `Hallo`.
IF s2 NA `bcdeh`. ... "true; sy-fdpos: 5
IF s2 NA `bcdeH`. ... "false; sy-fdpos: 0
```   

</td>
</tr>
<tr>
<td><code>CO</code></td>  
<td><i>Contains only</i><br>To determine whether a string contains only a certain set of characters. See the note above.</td>
<td>

```abap
DATA(s3) = `abcd`.
IF s3 CO `abcd`. ... "true; sy-fdpos: 4
IF s3 CO `abCd`. ... "false; sy-fdpos: 2
```   

</td>
</tr>
<tr>
<td><code>CN</code></td>
<td><i>Contains not only</i><br>To determine whether a string does not only contain a certain set of characters, i.e. whether a string contains characters other than those in the character set. See the note above.</td>
<td>

```abap
DATA(s4) = `abap`.
IF s4 CN `ab`. ...  "true; sy-fdpos: 3
IF s4 CN `abp`. ... "false; sy-fdpos: 4
```   

</td>
</tr>
<tr>
<td><code>CS</code></td>
<td><i>Contains string</i><br>For simple substring searches and determining whether a string contains a substring. <br>Note: The search is not case-sensitive. <code>sy-fdpos</code> contains the offset of the first substring found. If it is not found, <code>sy-fdpos</code> contains the length of the string searched.</td>
<td>

```abap
DATA(s5) = `Lorem ipsum dolor sit amet.`.
IF s5 CS `or`. ... "true; sy-fdpos: 1
IF s5 CS `zz`. ... "false; sy-fdpos: 27
```   

</td>
</tr>
<tr>
<td><code>NS</code></td>
<td><i>Contains no string</i><br>To determine whether a substring is not contained in a string. See the note for <code>CS</code>.</td>
<td>

```abap
DATA(s6) = `some test string`.
IF s6 NS `tests`. ... "true; sy-fdpos: 16
IF s6 NS `TEST`. ...  "false; sy-fdpos: 5
```   

</td>
</tr>
<tr>
<td><code>CP</code></td>
<td><i>Conforms to pattern</i><br>For simple pattern searches and determining whether a set of characters is contained in a string that matches a particular pattern. You can use the following special characters as patterns: <ul><li><code>*</code>: Any character sequence (including blanks)</li><li><code>+</code>: Any character (only one character, including blanks)</li><li><code>#</code>: Escape character. The following character is marked for an exact comparison.</li></ul>Patterns are not case-sensitive except for characters marked with
<code>#</code>. If a pattern is found, <code>sy-fdpos</code> returns the offset of the first occurrence. Otherwise, it contains the length of the string searched.
</td>
<td>

```abap
DATA(s7) = `abc_def_ghi`.

"Pattern: f is preceded by any character sequence, must be followed
"by '_' and then followed by any character sequence
IF s7 CP `*f#_*`. ... "true; sy-fdpos: 6

"Pattern: i is preceded by any character sequence, must be followed
"by any character or a blank
IF s7 CP `*i+`. ... "false; sy-fdpos: 11    
```   

</td>
</tr>
    <tr>
<td><code>NP</code></td>
<td><i>Does not conform to pattern</i><br>Negation of <code>CP</code>. See the previous notes.</td>
<td>

```abap
DATA(s8) = `abcDEFghi`.

"Pattern: c is preceded by any character sequence, must be followed
"by a small letter d, and then followed by any character sequence
IF s8 NP `*c#d*`. ... "true; sy-fdpos: 9   

"Pattern: c is preceded by any character sequence, must be followed
"by a capital letter D, and then followed by any character sequence
IF s8 NP `*c#D*`. ... "false; sy-fdpos: 2    
```   

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Classes for String Processing 
As also covered in the [Released ABAP Classes](22_Released_ABAP_Classes.md) cheat sheet, the following list shows a selected set of classes that support string processing.

- `CL_ABAP_CHAR_UTILITIES`: As previously mentioned, this class provides utilities for string processing, such as attributes that represent new lines and horizontal tabs.

    ``` abap
    DATA(tabbed) = `#` && cl_abap_char_utilities=>horizontal_tab && `#`.

    "The following attributes can be replaced by a representation of
    "the control characters in a string template.
    ASSERT cl_abap_char_utilities=>newline        = |\n|.
    ASSERT cl_abap_char_utilities=>horizontal_tab = |\t|.
    ASSERT cl_abap_char_utilities=>cr_lf          = |\r\n|.
    ``` 

- `CL_ABAP_STRING_UTILITIES`: For processing text strings, such as handling trailing blanks in character strings (i.e. data objects of type <code>string</code>).

    ``` abap
    DATA(string) = `ABAP   `.
    "Removing trailing blanks
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = string ).
    "`ABAP`

    "Preserving trailing blanks when assigning text fields to data objects of
    "type string
    DATA(chars) = 'ABAP   '.
    cl_abap_string_utilities=>c2str_preserving_blanks( EXPORTING source = chars 
                                                      IMPORTING dest   = DATA(str_w_blanks) ).
    "`ABAP   `
    DATA(str_no_blanks) = CONV string( chars ).
    "`ABAP`
    ``` 

- `XCO_CP`: The Extension Components Library (XCO) library provides [released APIs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreleased_api_glosry.htm) and offers various development utilities. Find more information [here](https://help.sap.com/docs/btp/sap-business-technology-platform/overview-of-xco-modules). The following code snippet demonstrates several methods of the class that deal with string processing.

    ```abap
    "--------- Extracting a substring from a string ---------
    DATA(some_string) = `abcdefghijklmnopqrstuvwxyz`.

    "Creating an encapsulation of a string using XCO
    DATA(str) = xco_cp=>string( some_string ).

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

    "bcdefghijklmnopqrstuvwxyz
    DATA(sub1) = str->from( 2 )->value.

    "defghijklmnopqrstuvwxyz
    DATA(sub2) = str->from( -23 )->value.

    "vwxyz
    DATA(sub3) = str->from( -5 )->value.

    "abcde
    DATA(sub4) = str->to( 5 )->value.

    "ab
    DATA(sub5) = str->to( -25 )->value.

    "Result of 1st 'from' method call: bcdefghijklmnopqrstuvwxyz
    "Based on this result, the 'to' method call is
    "applied.
    "bcdefg
    DATA(sub6) = str->from( 2 )->to( 6 )->value.

    "Result of 1st 'to' method call: abcdefghijklmnopq
    "Based on this result, the 'from' method call is
    "applied.
    "defghijklmnopq
    DATA(sub7) = str->to( -10 )->from( 4 )->value.

    "Values that are out of bounds.
    "In the example, the first and last character of the
    "string are used.
    "abcdefghijklmnopqrstuvwxyz
    DATA(sub8) = str->from( 0 )->to( 100 )->value.

    "--------- Splitting and joining ---------

    "Splitting a string into a string table
    DATA(str_table) = xco_cp=>string( `Hello.World.ABAP` )->split( `.` )->value.
    "Hello
    "World
    "ABAP

    "Concatenating a string table into a string; specifying a delimiter
    str_table = VALUE #( ( `a` ) ( `b` ) ( `c` ) ).
    "a, b, c
    DATA(conc_str1) = xco_cp=>strings( str_table )->join( `, ` )->value.

    "Concatenating a string table into a string; specifying a delimiter and
    "reversing the table order
    "c / b / a
    DATA(conc_str2) = xco_cp=>strings( str_table )->reverse( )->join( ` / ` )->value.

    "--------- Prepending and appending strings ---------
    DATA(name) = xco_cp=>string( `Max Mustermann` ).

    "Max Mustermann, Some Street 1, 12345 Someplace
    DATA(address) = name->append( `, Some Street 1, 12345 Someplace` )->value.

    "Mr. Max Mustermann
    DATA(title) = name->prepend( `Mr. ` )->value.

    "--------- Transforming to lowercase and uppercase ---------
    "ABAP
    DATA(to_upper) = xco_cp=>string( `abap` )->to_upper_case( )->value.

    "hallo world
    DATA(to_lower) = xco_cp=>string( `HALLO WORLD` )->to_lower_case( )->value.

    "--------- Checking if a string starts/ends with a specific string ---------
    DATA check TYPE string.
    DATA(str_check) = xco_cp=>string( `Max Mustermann` ).

    "yes
    IF str_check->ends_with( `mann` ).
      check = `yes`.
    ELSE.
      check = `no`.
    ENDIF.

    "no
    IF str_check->starts_with( `John` ).
      check = `yes`.
    ELSE.
      check = `no`.
    ENDIF.

    "--------- Converting strings to xstrings using a codepage ---------
    "536F6D6520737472696E67
    DATA(xstr) = xco_cp=>string( `Some string` )->as_xstring( xco_cp_character=>code_page->utf_8 )->value.

    "--------- Camel case compositions and decompositions with split and join operations ---------
    "Pascal case is also possible
    "someValue
    DATA(comp) = xco_cp=>string( `some_value` )->split( `_` )->compose( xco_cp_string=>composition->camel_case )->value.

    "Camel case decomposition
    "some_value
    DATA(decomp) = xco_cp=>string( `someValue` )->decompose( xco_cp_string=>decomposition->camel_case )->join( `_` )->value.

    "--------- Matching string against regular expression ---------
    DATA match TYPE string.

    "yes
    IF xco_cp=>string( ` 1` )->matches( `\s\d` ).
      match = 'yes'.
    ELSE.
      match = 'no'.
    ENDIF.

    "no
    IF xco_cp=>string( ` X` )->matches( `\s\d` ).
      match = 'yes'.
    ELSE.
      match = 'no'.
    ENDIF.
    ```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Byte String Processing

#### Determining the Length of xstrings

The built-in function [`xstrlen`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendescriptive_functions_binary.htm) returns the number of bytes of a byte-like argument.

``` abap
DATA(hi) = `Hello world`.

"48656C6C6F20776F726C64
DATA(conv_xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( hi ).
"22
DATA(len_str) = strlen( CONV string( conv_xstring ) ).

"11
DATA(len_xstr) = xstrlen( conv_xstring ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Character String and Byte String Processing with ABAP Statements

- Several ABAP statements include the `IN CHARACTER MODE` and `IN BYTE MODE` additions.
- These additions determine whether operations process character strings or byte strings.
- If neither option is specified, the default is character string processing (i.e. `IN CHARACTER MODE`).
- Byte string processing with the `IN BYTE MODE` addition is supported by the following ABAP statements: `CONCATENATE`, `FIND`, `REPLACE`, `SHIFT`, `SPLIT`.
- In byte string processing, the data objects used must be byte-like. For character string processing, only character-like data objects are allowed.

The following code snippet explores various statements with the `IN CHARACTER MODE` and `IN BYTE MODE` additions.

```abap
DATA off TYPE i.
DATA(abc_str) = `abc def ghi jkl mno pqr stu vwx yz`.
DATA(copy_str) = abc_str.

*&---------------------------------------------------------------------*
*& FIND and REPLACE statements
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& IN CHARACTER MODE addition
*&---------------------------------------------------------------------*

"Searching for the first blank in the string
"3
FIND ` ` IN abc_str IN CHARACTER MODE MATCH OFFSET off.

"The following example is the same as the previous as the IN CHARACTER MODE
"addition is optional. The FIRST OCCURRENCE OF addition is also optional.
"Just using FIND without FIRST OCCURRENCE OF means searching for the first
"occurrence by default.
"3
FIND FIRST OCCURRENCE OF ` ` IN abc_str MATCH OFFSET off.

"Searching for all blanks in the string
FIND ALL OCCURRENCES OF ` ` IN abc_str IN CHARACTER MODE RESULTS DATA(res).
DATA(offsets_of_findings) = concat_lines_of( 
  table = VALUE string_table( FOR wa IN res ( condense( val = CONV string( wa-offset ) to = `` ) ) )  sep = `, ` ).
"3, 7, 11, 15, 19, 23, 27, 31

"Replacing the first blank in the string
"abc#def ghi jkl mno pqr stu vwx yz
REPLACE ` ` IN abc_str WITH `#` IN CHARACTER MODE.

abc_str = copy_str.

"Replacing all blanks in the string
"abc#def#ghi#jkl#mno#pqr#stu#vwx#yz
REPLACE ALL OCCURRENCES OF ` ` IN abc_str WITH `#` IN CHARACTER MODE.

abc_str = copy_str.

*&---------------------------------------------------------------------*
*& IN BYTE MODE addition
*&---------------------------------------------------------------------*

"Converting to xstring
"6162632064656620676869206A6B6C206D6E6F20707172207374752076777820797A
DATA(abc_xstr) = cl_abap_conv_codepage=>create_out( )->convert( abc_str ).
"20
DATA(blank_xstr) = cl_abap_conv_codepage=>create_out( )->convert( ` ` ).
"23
DATA(repl_xstr) = cl_abap_conv_codepage=>create_out( )->convert( `#` ).
DATA(copy_xstr) = abc_xstr.

"Searching for the first byte that represents a blank in the UTF-8 code page
"3
FIND blank_xstr IN abc_xstr IN BYTE MODE MATCH OFFSET off.

FIND ALL OCCURRENCES OF blank_xstr IN abc_xstr IN BYTE MODE RESULTS res.
"3, 7, 11, 15, 19, 23, 27, 31
DATA(offsets_of_findings_xstr) = concat_lines_of( 
  table = VALUE string_table( FOR wa IN res ( condense( val = CONV string( wa-offset ) to = `` ) ) )  sep = `, ` ).

"Replacing the first byte that represents a blank in the UTF-8 code page
"6162632364656620676869206A6B6C206D6E6F20707172207374752076777820797A
REPLACE blank_xstr IN abc_xstr WITH repl_xstr IN BYTE MODE.

abc_xstr = copy_xstr.

"Replacing all bytes that represent a blank in the UTF-8 code page
"6162632364656623676869236A6B6C236D6E6F23707172237374752376777823797A
REPLACE ALL OCCURRENCES OF blank_xstr IN abc_xstr WITH repl_xstr IN BYTE MODE.

*&---------------------------------------------------------------------*
*& CONCATENATE statements
*&---------------------------------------------------------------------*

DATA(part_str1) = `abc`.
DATA(part_str2) = `def`.

"abcdef
CONCATENATE part_str1 part_str2 INTO DATA(concat_str) IN CHARACTER MODE.
"abc/def
CONCATENATE part_str1 part_str2 INTO concat_str SEPARATED BY `/` IN CHARACTER MODE.

"Same as above
CONCATENATE part_str1 part_str2 INTO concat_str.
CONCATENATE part_str1 part_str2 INTO concat_str SEPARATED BY `/`.

DATA(part_xstr1) = cl_abap_conv_codepage=>create_out( )->convert( part_str1 ).
DATA(part_xstr2) = cl_abap_conv_codepage=>create_out( )->convert( part_str2 ).
DATA(sep_xstr) = cl_abap_conv_codepage=>create_out( )->convert( `/` ).

"616263646566
CONCATENATE part_xstr1 part_xstr2 INTO DATA(concat_xstr) IN BYTE MODE.
"abcdef
DATA(concat_xstr_converted) = cl_abap_conv_codepage=>create_in( )->convert( concat_xstr ).
"6162632F646566
CONCATENATE part_xstr1 part_xstr2 INTO concat_xstr SEPARATED BY sep_xstr IN BYTE MODE.
"abc/def
concat_xstr_converted = cl_abap_conv_codepage=>create_in( )->convert( concat_xstr ).

"Creating a table of type xstring
DATA(xstr_table) = VALUE xstring_table( ( part_xstr1 ) ( part_xstr2 ) ).
"616263646566
CONCATENATE LINES OF xstr_table INTO DATA(concat_xstr_tab) IN BYTE MODE.
"abcdef
DATA(concat_xstr_tab_converted) = cl_abap_conv_codepage=>create_in( )->convert( concat_xstr_tab ).

*&---------------------------------------------------------------------*
*& SHIFT statements
*&---------------------------------------------------------------------*

DATA(str) = `abcdef`.
DATA(copy) = str.

"bcdef
SHIFT str IN CHARACTER MODE.

str = copy.

"Same as
SHIFT str.

str = copy.
"616263646566
DATA(xstr) = cl_abap_conv_codepage=>create_out( )->convert( str ).

"6263646566
SHIFT xstr IN BYTE MODE.

*&---------------------------------------------------------------------*
*& SPLIT statements
*&---------------------------------------------------------------------*

str = `abc def`.

"`abc` / `def`
SPLIT str AT space INTO DATA(str1) DATA(str2) IN CHARACTER MODE.
SPLIT str AT space INTO TABLE DATA(tab_str) IN CHARACTER MODE.

"Same as above
SPLIT str AT space INTO str1 str2.
SPLIT str AT space INTO TABLE tab_str.

"61626320646566
xstr = cl_abap_conv_codepage=>create_out( )->convert( str ).
"20
blank_xstr = cl_abap_conv_codepage=>create_out( )->convert( ` ` ).

"`616263` / `646566`
SPLIT xstr AT blank_xstr INTO DATA(xstr1) DATA(xstr2) IN BYTE MODE.
SPLIT xstr AT blank_xstr INTO TABLE DATA(xstr_tab) IN BYTE MODE.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### SET BIT and GET BIT Statements

- Unlike the ABAP statements in the previous section, the following statements are are only for byte string processing:
  - [`SET BIT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPSET_BIT.html)
    - Syntax pattern: `SET BIT pos OF byte_string [TO value].`
    - In a byte-like data object, the bit is set to 1 by default at a specified position, or to 0 or 1 as specified after `TO`.
    - Alternatively, you can use the built-in function [`bit-set`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBIT_FUNCTIONS.html).
  - [`GET BIT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPGET_BIT.html) 
    - Syntax pattern: `GET BIT pos OF byte_string INTO value.`
    - Reads a bit at a specified position in a byte string into a target data object.


Expand the following collapsible section for example code, which experiments with byte string processing (converting a hexadecimal value to the character-like representation of the binary values by reading bits, getting hexadecimal values from the character-like representation of the binary values, setting bits). To try it out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.

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
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "Converting an example text string to xstring
    "The conversion is performed using the default code page UTF-8.
    "48656C6C6F2041424150
    DATA(demo_string) = `Hello ABAP`.
    DATA(xstr) = cl_abap_conv_codepage=>create_out( )->convert( demo_string ).

    TYPES: BEGIN OF struc,
             bin       TYPE string,
             hex_value TYPE xstring,
           END OF struc.
    DATA tab TYPE TABLE OF struc WITH EMPTY KEY.

    "Determining the length of the hexadecimal value 48656C6C6F2041424150
    "and multiplying by 8 for reading the bits
    DATA(length_xstr) = xstrlen( xstr ) * 8.

    "Data object to store the binary representation of hexadecimal values
    DATA binary_rep TYPE string.

    "Reading the bits from front to back
    "The example is implemented in a way to perform a byte-wise storing
    "of the binary representations of hexadecimal values in an internal table
    "to visualize individual bytes. After 8 bits, the next byte is processed.
    WHILE sy-index <= length_xstr.
      GET BIT sy-index OF xstr INTO DATA(bit).
      binary_rep &&= |{ bit }|.
      IF sy-index MOD 8 = 0.
        APPEND VALUE #( bin = binary_rep ) TO tab.
        CLEAR binary_rep.
      ENDIF.
    ENDWHILE.

    "Getting hexadecimal values from the character-like representation of
    "the binary values
    LOOP AT tab REFERENCE INTO DATA(line).
      DATA(num) = 0.
      DO strlen( line->bin ) TIMES.
        DATA(pos) = strlen( line->bin ) - sy-index.
        num += COND i( WHEN line->bin+pos(1) = '1' THEN 2 ** ( sy-index - 1 ) ).
      ENDDO.

      line->hex_value = CONV xstring( num ).
    ENDLOOP.

    "Table reduction: Concatenating the individual hexadecimal values
    DATA(hex_value) = REDUCE xstring( INIT xs = ``
                                      FOR <li> IN tab
                                      NEXT xs &&= <li>-hex_value ).

    out->write( data = tab name = `tab` ).
    out->write( |\n| ).
    ASSERT hex_value = xstr.
    out->write( data = hex_value name = `hex_value` ).
    out->write( |\n| ).

    "Converting xstring to string
    DATA(conv_string) = cl_abap_conv_codepage=>create_in( )->convert( hex_value ).
    ASSERT conv_string = demo_string.
    out->write( data = conv_string name = `conv_string` ).
    out->write( |\n| ).

    "SET BIT
    "Adding `!` to the demo string
    "The binary representation of `!` corresponds to 00100001.
    "The hex_val data object has eight bits. In a loop, the
    "binary representation is looped across. Depending on the
    "value 0 or 1, SET BIT statements set values accordingly.
    "Finally, the resulting data object is assigned to the demo
    "xstring, and converted to string.

    DATA hex_val    TYPE x LENGTH 1.
    DATA(excl_mark) = `00100001`.

    DATA(length_str) = strlen( excl_mark ).

    DO 8 TIMES.
      DATA(idx_for_position) = sy-index - 1.
      DATA(value_at_position) = SWITCH #( sy-index WHEN 1 THEN excl_mark+0(1) ELSE excl_mark+idx_for_position(1) ).

      CASE value_at_position.
        WHEN `0`.
          SET BIT sy-index OF hex_val TO 0.
        WHEN `1`.
          SET BIT sy-index OF hex_val.
      ENDCASE.
    ENDDO.

    xstr = xstr && hex_val.
    conv_string = cl_abap_conv_codepage=>create_in( )->convert( xstr ).
    ASSERT conv_string = |{ demo_string }!|.
    out->write( data = conv_string name = `conv_string` ).

  ENDMETHOD.

ENDCLASS.
```


</details>  


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

[zcl_demo_abap_string_proc](./src/zcl_demo_abap_string_proc.clas.abap)

> [!NOTE]  
> - The executable example ...
>   - covers the following topics:
>     - Creating strings and assigning values
>     - String templates
>     - Operations with strings operations: chaining, concatenating, splitting, modifying 
>     - Searching and replacing
>     - Regular expressions 
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)
