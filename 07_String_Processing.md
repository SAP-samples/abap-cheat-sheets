<a name="top"></a>

# String Processing

- [String Processing](#string-processing)
  - [Introduction](#introduction)
    - [Variable Length and Fixed Length Character Strings](#variable-length-and-fixed-length-character-strings)
  - [Declaring Character-Like Data Types and Objects](#declaring-character-like-data-types-and-objects)
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
    - [Searching for Substrings in Strings](#searching-for-substrings-in-strings)
    - [Replacing Substrings in Strings](#replacing-substrings-in-strings)
  - [Pattern-Based Searching and Replacing in Strings](#pattern-based-searching-and-replacing-in-strings)
    - [Simple Pattern-Based Searching Using Comparison Operators](#simple-pattern-based-searching-using-comparison-operators)
    - [Complex Searching and Replacing Using Regular Expressions](#complex-searching-and-replacing-using-regular-expressions)
    - [Searching Using Regular Expressions](#searching-using-regular-expressions)
    - [Replacing Using Regular Expressions](#replacing-using-regular-expressions)
  - [Executable Example](#executable-example)


## Introduction

ABAP offers plenty of options for processing [character
strings](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencharacter_string_glosry.htm "Glossary Entry").
The options include ABAP statements (e. g.
[`FIND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfind.htm)),
[character string
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_expression_glosry.htm "Glossary Entry")
(e. g. [string
templates](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_template_glosry.htm "Glossary Entry"))
and built-in [string
functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_function_glosry.htm "Glossary Entry")
(e. g.
[`strlen`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlength_functions.htm)).

> **💡 Note**<br>
>-   Expressions and string functions can help make your ABAP code more
    concise and straightforward. For example, string operations can be
    done directly in [operand
    position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm "Glossary Entry")
    and, thus, you can avoid temporary variables.
>-   In ABAP statements, modify operations on strings are frequently done
    on the source field which is the target field at the same time.
    String functions never modify the source field. Instead, the
    modified string is provided as a [return
    value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreturn_value_glosry.htm "Glossary Entry").
>-   In most cases, string functions offer the same functionality as the
    corresponding ABAP statements. The return value of string functions
    that return character strings is always of type `string`.

<p align="right">(<a href="#top">back to top</a>)</p>

### Variable Length and Fixed Length Character Strings

Built-in character-like types in ABAP are as follows:

| Type | Length | Value Range | Initial Value |
|---|---|---|---|
| `string` | Variable, i. e. the length of [data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm "Glossary Entry") of this type can change during the execution during the execution of ABAP programs (hence, they are also referred to as [dynamic data objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendynamic_data_object_glosry.htm "Glossary Entry")); no standard length | Any characters | Empty string with length 0 |
| `c` | Data objects of this type contain a string of fixed length (between 1 and 262143 characters); standard length: 1 | Any characters | blank for every position |
| `n` | Same as for c | Any characters; valid values are only the digits 0 to 9.<br><br> Note that the restrictions for this type to only accept digits are not enforced, hence, fields may contain invalid data. The type is especially used for digits that are not meant for arithmetic calculations like zip codes or article numbers. | "0" for every position |

> **💡 Note**<br>
> [Byte-like data
types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbyte_like_data_typ_glosry.htm "Glossary Entry")
(`x`, `xstring`) and types that are specializations of
the type `c` (`n`, `d` referring to date and
`t` referring to time) are not covered in this cheat sheet.

> **⚡ Differences between variable length and fixed length strings**<br>
>-   **Initial value**: The initial value of variable length strings is an
    empty string with length 0. Fixed length strings have a standard
    length of 1 character.
>-   **Performance**: Data objects of both type `c` and
    `string` are considered as [elementary data
    objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenelementary_data_object_glosry.htm "Glossary Entry").
    However, data objects of type `string` are internally
    managed as
    [references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_glosry.htm "Glossary Entry")
    and are, thus, considered as
    [deep](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_glosry.htm "Glossary Entry").
    This fact enables the performance boosting concept of
    [sharing](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensharing_glosry.htm "Glossary Entry")
    for data objects of this type when making value assignments.
>-   **Length**: Theoretically, a variable length string can use up to 2 GB.
    The maximum length of a fixed length string is 262143 characters.
>-   **Handling trailing blanks**: Usually, the [operand
    position](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenoperand_position_glosry.htm "Glossary Entry")
    determines whether trailing blanks are respected or not. Fixed
    length strings usually ignore trailing blanks; variable length
    strings respect them. For example, if a fixed length string is
    assigned to a variable length string, the target string does not
    contain any trailing blanks. Note in this context the section
    *Condensing Strings*.
>-   **Flexibility**: Variable length strings are more flexible than fixed
    length strings because you can easily shorten or extend them without
    worrying that, for example, parts of the character string are
    truncated when processing.
>-   **Specifying values** for character strings in the ABAP source code
    ([literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenliteral_glosry.htm "Glossary Entry"))
    with the types `c` and `string`: With quotes
    (`'...'`), you create [text field
    literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentext_field_literal_glosry.htm "Glossary Entry")
    of type `c`, with backquotes (<code>\`...\`</code>), you create [text
    string
    literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentext_string_literal_glosry.htm "Glossary Entry")
    of type `string`.

So, when to actually use what? Fixed length strings make sense when
actually determining a maximum or mandatory length, e. g. a country code
that must consist of a maximum of two characters or input fields in
forms that should not exceed a certain length. If restricting a string
is not relevant, variable length strings are a good choice.

<p align="right">(<a href="#top">back to top</a>)</p>

## Declaring Character-Like Data Types and Objects

Character-like data types and objects are declared like other types and
objects using
[`TYPES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptypes.htm)
and
[`DATA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata.htm)
statements. This cheat sheet focuses on the built-in types `c`
and especially on `string` in most examples.

Syntax examples:
``` abap
"Type declarations using built-in types

TYPES: c_type   TYPE c LENGTH 3, "Explicit length specification
        str_type  TYPE string.

"Data object declarations using built-in, local and DDIC types

DATA: flag  TYPE c LENGTH 1,   "Built-in type
      str1  TYPE string,       "Built-in type
      char1 TYPE c_type,       "Local type
      str2  LIKE str1,         "Deriving type from a local data object
      str3  TYPE str_type,     "Local type
      char2 TYPE s_toairp,     "DDIC type (used e. g. for a field in a demo table)
      char3 TYPE spfli-carrid. "Using the type of a DDIC table component

"You might also stumble upon declarations with type c and the length
"specified in parentheses. It is not recommended so as not to confuse
"with the use of parentheses in dynamic programming.

DATA char(4) TYPE c.
```

<p align="right">(<a href="#top">back to top</a>)</p>

## Assigning Values

When declaring character-like data objects, you can directly provide
default values. For the assignment of values, you can, for example, use
the [assignment
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm "Glossary Entry")
`=`. To do both data object declaration and value assignment in
one go, you can make use of [inline
declaration](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_inline.htm)
that supports declarations in [write
positions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwrite_position_glosry.htm "Glossary Entry").
In doing so, a variable specified in parentheses preceded by
`DATA` on the left side of the assignment operator automatically
derives a data type from the operand on the right. This helps make your
programs leaner.

Syntax examples:
``` abap
"Data object declarations including default values with VALUE
DATA: flag TYPE c LENGTH 1 VALUE 'X',
      str1 TYPE string VALUE `Hallo!`.

"Examples for type n
DATA zip_code TYPE n LENGTH 5 VALUE '12345'.
DATA isbn_number TYPE n LENGTH 13 VALUE '1234567890123'.

"More data object declarations
DATA: char1  TYPE c LENGTH 5,
      html   TYPE string,
      str2   LIKE html.

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

"Since char2 is of type c length 4 (the length is also derived),
"characters are truncated in the following example assignment
char2 = 'ijklmnopq'. "ijkl

"Trailing blanks after assigning fixed length to variable length string
DATA(char3) = 'ab   '.
DATA(str6)  = `cdefgh`.
str6 = char3. "'ab' (trailing blanks are not respected due to conversion rule)
```

When assigning strings, not only data objects can be placed on the right
side. Various expressions and strings can be chained using the
[concatenation
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconcatenation_operator_glosry.htm "Glossary Entry")
`&&`. Alternatively, you might chain strings using [string
templates](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_template_glosry.htm "Glossary Entry")
and as outlined in the section [Concatenating Strings]. `Concatenating Strings`.
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

<p align="right">(<a href="#top">back to top</a>)</p>

## String Templates
Using string templates, you can construct strings very elegantly from
literal text and - which is the primary use case - by also including
embedded ABAP
[expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenexpression_glosry.htm "Glossary Entry")
within a pair of delimiter characters: `|...|` if these expressions can be converted to `string`. To embed expressions, you include them within curly brackets: `{ ... }`.

> **💡 Note**<br>
> String templates form a [string
expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_expression_glosry.htm "Glossary Entry")
that is compiled at runtime. Hence, a string template that only contains
literal text is handled like an expression which has impact on the
performance. In such a case, using a text string literal with backquotes
is preferable.

Syntax examples:
``` abap
"Value assignment with string templates
"The expression must be convertible to a string. A blank (not within the curly brackets)
"means a blank in the resulting string.
DATA(s1) = |Hallo { cl_abap_context_info=>get_user_technical_name( ) }!|.

DATA(s2) = `How are you?`. "Literal text only with backquotes
DATA(s3) = |{ s1 } { s2 }|. "Hallo ...! How are you?

"Chaining of string templates using &&
DATA(s4) = |{ s1 }| && ` ` && |{ s2 }|. "Hallo ...! How are you?
```

String templates interpret certain character combinations as [control
characters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstring_templates_separators.htm).
For example, `n` is interpreted as line feed. A new line is
set. Plus, string templates support various [formatting
options](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcompute_string_format_options.htm).
Check the ABAP Keyword Documentation for all options.

Syntax examples:
``` abap
"Control characters
s4 = |{ s1 }\n{ s2 }\nSee you.|. "\n is interpreted as a line feed

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
> Escape `|{}` in string templates using `\`, i. e. `\\` means `\`.

<p align="right">(<a href="#top">back to top</a>)</p>

## Determining the Length of Strings

To determine the length of a string you can use the string function
[`strlen`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlength_functions.htm).
Note that the result depends on the type of the string, i. e. the result
for a data object of type `string` includes trailing blanks. A
fixed length string does not include them. To exclude trailing blanks in
all cases irrespective of the data type, you can use the built-in
function
[`numofchar`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlength_functions.htm).

Syntax examples:
``` abap
"strlen
DATA(len_c)   = strlen( 'abc   ' ). "3
DATA(len_str) = strlen( `abc   ` ). "6

"numofchar
len_c   = numofchar( 'abc   ' ). "3
len_str = numofchar( `abc   ` ). "3
```

<p align="right">(<a href="#top">back to top</a>)</p>

## Concatenating Strings

Two or more strings can be concatenated using the concatenation operator
`&&` and string templates. Alternatively, you can use
[`CONCATENATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconcatenate.htm)
statements. It is also possible to concatenate lines of internal tables
into a string to avoid a loop. In a more modern way, you can make use of
the string function
[`concat_lines_of`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconcatenation_functions.htm).

Syntax examples:
``` abap
"&& and string template
s1 = `AB` && `AP`. "ABAP
s2 = `ab` && `ap` && ` ` && s1. "abap ABAP
s3 = |{ s1 }. { s2 }!|. "ABAP. abap ABAP!

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

<p align="right">(<a href="#top">back to top</a>)</p>

## Splitting Strings

You can use
[`SPLIT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapsplit.htm)
statements to split strings in multiple segments. The result of the
splitting can be stored in separate data objects or internal tables that
have a character-like line type. Note that if the specified targets are
fewer than the segments retrieved by the splitting, the last target is
given the rest of the segments that have not yet been split. If there
are more targets specified, the targets not given a segment are
initialized. Hence, specifying individual targets with `SPLIT`
statements is useful if the number of expected segments is known.
Otherwise, splitting into tables is a good choice.

If you want to get the value of a specific segment, you can use the
string function
[`segment`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensegment_functions.htm).

Syntax examples:
``` abap
s1 = `Hallo,world,123`.

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

<p align="right">(<a href="#top">back to top</a>)</p>

## Modifying Strings
**Transforming to Lowercase and Uppercase**

The string functions
[`to_lower`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencase_functions.htm)
and
[`to_upper`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencase_functions.htm)
transform characters of a string to either lowercase or uppercase and
store the result in a target variable. If the transformation should be
applied to the source directly, you can use
[`TRANSLATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptranslate.htm)
statements.

Syntax examples:
``` abap
"String functions
s1 = to_upper( `abap` ). "ABAP
s1 = to_lower( `SOME_FILE.Txt` ). "some_file.txt

"TRANSLATE statements
s1 = `Hallo`.
TRANSLATE s1 TO UPPER CASE. "HALLO
TRANSLATE s1 TO LOWER CASE. "hallo
```

**Shifting Content**

You can shift content within a string to a specific position on the left
or right of a string.
[`SHIFT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapshift.htm)
statements have different additions for specific use cases.

In a more modern way, you can use the string functions
[`shift_left`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenshift_functions.htm)
and
[`shift_right`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenshift_functions.htm)
that store the result in a variable. These functions offer an additional
functionality. The `sub` parameter can be used to specify a
substring. All substrings in the string that match the specified value
in `sub` either on the left or right side of the string are
removed.

Syntax examples:
``` abap
"SHIFT statements
"Note that all results below refer to s1 = `hallo`.
s1 = `hallo`. "Type string

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
s2 = `   hallo   `.
s3 = s2.

SHIFT s2 LEFT DELETING LEADING ` `. "'hallo   '
SHIFT s3 RIGHT DELETING TRAILING ` `. "'      hallo' (length is kept)

"Removing trailing blanks for strings without leading blanks with this sequence
s4 = `hallo   `.
SHIFT s4 RIGHT DELETING TRAILING ` `. "'   hallo'
SHIFT s4 LEFT  DELETING LEADING ` `. "'hallo'

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

You can use
[`CONDENSE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcondense.htm)
statements or the string function
[`condense`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencondense_functions.htm)
to remove blanks in strings. The advantage of using the string function
is that you can also specify any characters to be removed and not only
blanks.

Syntax examples:
``` abap
"CONDENSE statements
s1 = ` ab cd `.
s2 = `    ef   gh ij   `.
s3 = ` kl  mn   op `.

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
s2 = condense( val  = `  Rock'xxx'Roller` del  = `re `
               from = `x` to = `n` ). "Rock'n'Roll
```

**Reversing Strings**

The string function
[`reverse`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreverse_functions.htm)
reverses a string:
```
"Result: 'abap'
s1 = reverse( `paba` ).
```

**Inserting Content**

The string function
[`insert`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninsert_functions.htm)
inserts a substring into any position within a given string. Using
various parameters, you can construct your desired string:

- `val`: Original string.
- `sub`: Substring.
- `off`: Optionally sets the offset, i. e. the position where  to add the substring. The default value is 0. When using the function with the default value, the result is like concatenating a  string with `&&` (like `res = sub && text`).

Syntax examples:
```
"Result: 'abcdefghi'
s1 = insert( val = `abcghi` sub = `def` off = 3 ).

"Result: 'defabcghi'
s1 = insert( val = `abcghi` sub = `def` ).
```

<p align="right">(<a href="#top">back to top</a>)</p>

## Processing Substrings

Using the string function
[`substring`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm),
you can specify the position (parameter `off`) and the length
(`len`) of a substring that should be extracted from a given
string (`val`). At least one of the two parameters `off`
or `len` must be specified. The default value of `off`
is 0, i. e. when using the default value, the substring is extracted
from the beginning of the string. Not specifying `len` means
that the rest of the remaining characters are respected. If the offset
and length are greater than the actual length of the string, the
exception `CX_SY_RANGE_OUT_OF_BOUNDS` is raised.

You might also stumble on the syntax for accessing substrings via offset
and length specification using the `+` character after a variable. Here, the
length is specified within parentheses. Providing an asterisk (`*`) means
that the rest of the remaining string is respected. This syntax option
even enables write access on substrings for fixed length strings. Read
access is possible for both fixed length and variable length strings.
However, this syntax might be confused with the use of tokens in the
context of  dynamic programming.

There are further string functions available to deal with substrings,
for example,
[`substring_after`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm),
[`substring_before`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm),
[`substring_from`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm)
and
[`substring_to`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubstring_functions.htm).

These functions offer more options in terms of parameters, for example,
a [PCRE regular
expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpcre_regex_glosry.htm "Glossary Entry")
which are dealt with further down.

Syntax examples:
```
s1 = `Lorem ipsum dolor sit amet`. "Type string

"Extracting substring starting at a specific position
"'len' not specified means the rest of the remaining characters are
"respected
s2 = substring( val = s1 off = 6 ). "ipsum dolor sit amet

"Extracting substring with a specific length
"'off' is not specified and has the default value 0.
s2 = substring( val = s1 len = 5 ). "Lorem

"Specifying both off and len parameters
s2 = substring( val = s1 off = 6 len = 5 ). "ipsum

DATA(ch5) = 'Lorem ipsum dolor sit amet'. "Type c

"Offset and length specification using the + sign after a variable
DATA(ch6) = ch2+0(5). "Lorem

"* means respecting the rest of the remaining string
DATA(ch7) = ch2+12(*). "dolor sit amet

CLEAR ch5+11(*). "Lorem ipsum

ch5+0(5) = 'Hallo'. "Hallo ipsum dolor sit amet

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

<p align="right">(<a href="#top">back to top</a>)</p>

## Searching and Replacing

In ABAP, there are a lot of options to carry out search and replace
operations with strings. This includes the use of [comparison
operators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomp_operator_glosry.htm "Glossary Entry")
or the prominent ABAP statements
[`FIND`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapfind.htm)
and
[`REPLACE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapreplace.htm),
or the more modern built-in string functions
[`find`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensearch_functions.htm)
and
[`replace`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreplace_functions.htm),
among others, with their considerable amount of additions or parameters
respectively. Many of these options support rather simple operations
with respect to only single characters or more complex, pattern-based
operations on character sequences using [PCRE regular
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpcre_regex_glosry.htm "Glossary Entry").

<p align="right">(<a href="#top">back to top</a>)</p>

### Searching for Specific Characters

-   You can use the [comparison
    operators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomp_operator_glosry.htm "Glossary Entry")
    [`CA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains any) or its negation
    [`NA`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains not any) in [comparison
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomparison_expression_glosry.htm "Glossary Entry")
    to determine if any character of a given character set is contained
    in a string. Such an expression is true if at least one character is
    found.
    -   The search is case-sensitive.
    -   The system variable `sy-fdpos` contains the offset of
        the first found character. If nothing is found,
        `sy-fdpos` contains the length of the string.
    -   Note that offset 0 stands for the very first position.
-   The string functions
    [`find_any_of`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensearch_functions.htm)
    or its negation
    [`find_any_not_of`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensearch_functions.htm)
    return the offset of the occurrence of any character contained in a
    substring.
    -   If nothing is found, the value -1 is returned
    -   There are further optional parameters possible. For example, the
        specification of `occ` determines the search
        direction, i. e. a positive value means the search is performed
        from left to right. A negative value means searching from right
        to left.
-   If the position of characters is not of interest but rather how
    often characters occur in a string, you can use the string functions
    [`count_any_of`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensearch_functions.htm)
    or its negation
    [`count_any_not_of`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensearch_functions.htm).
-   To determine if a string only contains a specific set of characters,
    you can use the comparison operators
    [`CO`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains only) or its negation
    [`CN`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains not only) in comparison expressions.
    -   Regarding `CO`, a comparison is true if the left operand
        only contains characters that are also contained in the right
        operand. If the comparison is not true, you can get the position
        of the first character from text that is not contained in the
        character set via `sy-fdpos`. Regarding `CN`, a
        comparison is true if a string not only contains characters from
        the character set.

Syntax examples:
``` abap
s1 = `cheers`.
IF s1 CA `aeiou` ... "true, sy-fdpos = 2
IF s1 NA `xyz`... "true, sy-fdpos = 6

s2 = find_any_of( val = s1 sub = `aeiou` ). "2
s2 = find_any_not_of( val = s1 sub = `c` ). "1

s2 = count_any_of( val = s1  sub = `e` ). "2
s2 = count_any_not_of( val = s1  sub = `s` ). "5

IF s1 CO `rs` ... "not true, sy-fdpos = 0
IF s1 CN `cheers` ... "not true, sy-fdpos = 6
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Replacing Specific Characters in Strings

You can use the string function
[`translate`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentranslate_functions.htm)
to replace specific characters by others. Here, the parameter
`from` denotes the characters to be placed in a string and
`to` specifies the target characters. Note: The
replacement is done as follows: Each character specified in
`from` is replaced by the character in `to` that is on
the same position, i. e. the second character in `from` is
replaced by the second character specified in `to`. If there is
no equivalent in `to`, the character in `from` is
removed from the result.

Using
[`TRANSLATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptranslate.htm)
statements, you can carry out replacements directly on the source field.

Syntax examples:
``` abap
s1 = `___abc_def_____ghi_`.
s2 = translate( val = s1 from = `hi_` to = `##` ). "abcdefg##
s2 = translate( val = s1 from = `_`  to = `##` ). "###abc#def#####ghi#

"TRANSLATE statement. The value after USING is interpreted as a string composed of character pairs.
"Starting with the first pair, a search is performed in text for the
"first character in every pair and each occurrence is replaced with the
"second character of the pair.
TRANSLATE s1 USING `_.a#g+`. "...#bc.def.....+hi.
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Searching for Substrings in Strings

-   For simple substring searches, you can use the [comparison
    operators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomp_operator_glosry.htm "Glossary Entry")
    [`CS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains string) or its negation
    [`NS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
    (contains no string) in [comparison
    expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomparison_expression_glosry.htm "Glossary Entry").
    Here, the search is not case-sensitive. The system variable
    `sy-fdpos` contains the offset of the found substring. If
    the substring is not found, `sy-fdpos` contains the length
    of the searched string.
-   For more complex and iterating search operations, it can be
    beneficial to use `FIND` statements or the string function
    [`find`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensearch_functions.htm).
    If you are only interested in the offset of a substring within a
    string, the string function offers more options than using the
    logical operator, for example, you can specify if the search should
    be case-sensitive or not. You can also further restrict the search
    using parameters.
-   Find out how often a substring occurs using the string function
    [`count`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencount_functions.htm).
    Special variants are available:
    [`count_any_of`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencount_functions.htm),
    [`count_any_not_of`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencount_functions.htm).
-   Since the string function `find` is derived from the ABAP
    statement `FIND`, `FIND` covers the same
    functionality as mentioned above and beyond using the many addition
    options.

Syntax examples:
``` abap
s3 = `cheers`.

IF s3 CS `rs` ... "true, sy-fdpos = 4

IF s3 NA `xyz`... "not true, sy-fdpos = 6

"String function find
s1 = `Pieces of cakes.`.

s2 = find( val = s1 sub = `OF` case = abap_false  ). "7

s2 = find( val = s1 sub = `hallo` ). "-1 (no occurrence returns -1)

s2 = find( val = s1 sub = `ce` off = 1 len = 7 ). "3

"Parameter occ: Positive value means the nth position from the left,
"a negative value the nth position from the right
s2 = find( val = s1 sub = `es` occ = -1 ). "13

"String function count
s2 = count( val = s1 sub = `es` ). "2

"FIND statements with selected additions
s1 = `abc def ghi abc`.

FIND `def` IN s1.
IF sy-subrc = 0. "If there is an occurrence, sy-subrc is set to 0.
 ... "Some action
ENDIF.
FIND SUBSTRING `abc` IN s1. "Addition SUBSTRING is optional

FIND `aBC` IN s1 IGNORING CASE. "Case-insensitive search

"MATCH additions can be specified individually or combined
FIND ALL OCCURRENCES OF `abc` IN s1
  MATCH COUNT DATA(fcnt)  "2 (number of occurrences)
  MATCH OFFSET DATA(foff) "12 (offset of last occurrence)
  MATCH LENGTH DATA(flen). "3 (length of last occurrence)

"Finding the first occurrence
FIND FIRST OCCURRENCE OF `abc` IN s1 MATCH OFFSET foff. "0

"Returning all of these pieces of information in a table for all occurrences
FIND ALL OCCURRENCES OF `abc` IN s1 RESULTS DATA(fres).

"Restricting the search area (OFFSET/LENGTH can be specified individually)
FIND `abc` IN SECTION OFFSET 4 LENGTH 11 OF s1
  MATCH OFFSET foff. "12

"Searching in internal tables; search results are returned in an internal table
DATA(str_table) = VALUE string_table( ( `ZxZ` ) ( `yZ` ) ( `Zz` ) ).

FIND ALL OCCURRENCES OF `Z` IN TABLE str_table RESULTS
  DATA(findings) RESPECTING CASE.
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Replacing Substrings in Strings

Using the string function
[`replace`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreplace_functions.htm),
you can store the result of a substring replacement in a separate
variable. What makes it very powerful in particular is the fact that it
returns a value and can, thus, be used in almost all [read
positions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenread_position_glosry.htm "Glossary Entry").
Using
[`REPLACE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapreplace.htm)
statements, you can carry out replacements on strings directly
(including substrings, which is not possible with the string function).
Regarding the multiple additions `REPLACE` offers,
`REPLACE` is syntactically similar to `FIND`. Regarding
the parameters of `replace`:
- `with`: The replacement text.
- `sub`: Specifies the substring to be replaced.
- `case`: Sets the case sensitivity.
- `occ`: Specifies the number of occurrences of a substring. The default value is `1`, i. e. the first occurrence starting from the left. Setting `occ` to `0` means that all occurrences are respected for the replacement.

Syntax examples:
```
s1 = `abc def ghi abc`.

s2 = replace( val = s1 sub = `def` with = `###` ). "abc ### ghi abc

s2 = replace( val = s1 sub = `ABC` with = `###` case = abap_false occ = 2 ). "abc def ghi ###

s2 = replace( val = s1 sub = `abc` with = `###` occ = 0 ). "### def ghi ###

"REPLACE statements with selected additions
"Note that all results below refer to s1 = `abc def ghi abc`.
REPLACE `def` IN s1 WITH `###`. "abc ### ghi abc

REPLACE FIRST OCCURRENCE OF `abc` IN s1 WITH `###`. "### def ghi abc

REPLACE `abc` IN s1 WITH `###`. "### def ghi abc (first found is replaced)

REPLACE SUBSTRING `abc` IN s1 WITH `###`. "### def ghi abc (SUBSTRING is optional)

REPLACE ALL OCCURRENCES OF `abc` IN s1 WITH `###`. "### def ghi ###

REPLACE `aBC` IN s1 WITH `###` IGNORING CASE. "### def ghi abc

"REPLACEMENT additions; can be specified individually or combined
REPLACE ALL OCCURRENCES OF `abc` IN  s1 WITH `###`   "### def ghi ###
  REPLACEMENT COUNT  DATA(cnt)  "2 (number of replacements)
  REPLACEMENT OFFSET DATA(off)  "12 (offset of last replacement)
  REPLACEMENT LENGTH DATA(len). "3 (length of last substring inserted)

"Returning all of these pieces of information in a table for all replacements
REPLACE ALL OCCURRENCES OF `abc` IN  s1 WITH `###`
      RESULTS DATA(res). "### def ghi ###

"Position-based replacement (OFFSET/LENGTH can be specified individually )
REPLACE SECTION OFFSET 4 LENGTH 7 OF s1 WITH `###`. "abc ### abc

"Replacements in internal tables
DATA(str_tab) = VALUE string_table( ( `ZxZ` ) ( `yZ` ) ( `Zz` ) ).

REPLACE ALL OCCURRENCES OF `Z`
        IN TABLE str_tab WITH ``
        RESPECTING CASE. "x / y / z
```

## Pattern-Based Searching and Replacing in Strings

You can carry out complex search and replace operations based on
patterns. [PCRE regular
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpcre_regex_glosry.htm "Glossary Entry")
help you process strings effectively.
> **💡 Note**<br>
> Do not use [POSIX
regular
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenposix_regex_glosry.htm "Glossary Entry")
any more since they are obsolete.

<p align="right">(<a href="#top">back to top</a>)</p>

### Simple Pattern-Based Searching Using Comparison Operators

For simple patterns, you can use the [comparison
operators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomp_operator_glosry.htm "Glossary Entry")
[`CP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
(conforms to pattern) or its negation
[`NP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_strings.htm)
(does not conform to pattern) in [comparison
expressions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomparison_expression_glosry.htm "Glossary Entry")
to determine if a set of characters is contained in a string that
matches a certain pattern. For the patterns, you can use the following
special characters:

| Special Character | Details |
|---|---|
| `*` | Any character sequence (including blanks). |
| `+` | Any character (only one character, including blanks). |
| `#` | Escape character. The following character is marked for an exact comparison. |

Patterns are not case-sensitive except for characters marked by
`#`. If a pattern is found, the system variable
`sy-fdpos` returns the offset of the first occurrence.
Otherwise, it contains the length of the searched string.
``` abap
s1 = `abc_def_ghi`.

"Pattern: f is preceded by any character sequence, must be followed
"by '_' and then followed by any character sequence
IF s1 CP `*f#_*`. ... "true; sy-fdpos = 6

"Pattern: 'i' is not followed by another character
IF s1 NP `i+`. ... "true; sy-fdpos = 11 (length of searched string)
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Complex Searching and Replacing Using Regular Expressions

**Excursion: Common Regular Expressions**

There are various options to carry out complex searching in strings using PCRE expressions. They can be fairly complex. The following overview shows common PCRE expressions with simple examples.

Characters and character types

| Expression | Represents | Example | Matches | Does not Match |
|---|---|---|---|---|
| `.` | Any single character | `.` | a, 9, Ä, # | aa, empty |
| `\d` | Any digit (0-9) | `\d` | 1, 3, 7 | A, b, c |
| `\D` | Any character that is not a digit, equivalent to `[^0-9]` | `\D` | D, e, f | 4, 5, 8 |
| `\s` | Any whitespace character such as a blank, tab and new line | `\s` <br>(example string: hi there) | the blank in between | hi |
| `\S` | Any character that is not a whitespace | `\S` <br>(example string: a 1) | a, 1 | the blank in between |
| `\w` | Any word character (letter, digit or the underscore), equivalent to `[a-zA-Z0-9_]` | `\w` <br>(example string: ab 12) | a, b, 1, 2 | the blank in between |
| `\W` | Any character that is not a word character, equivalent to `[^a-zA-Z0-9_]` | `\W` <br>(example string: cd 34) | the blank in between | c, d, 3, 4 |
| `\...` | To include special characters like <code>[] \ / ^</code>, use `\` to escape them. Use `\.` to match a period ("."). | `\\` | `\` | `/` |


Repetitions and Alternatives

| Expression | Represents | Example | Matches | Does not Match |
|---|---|---|---|---|
| `r*` | Zero or more repetitions of `r` | `ab*` | a, ab, abb, abbb | b, aba |
| `r+` | One or more repetitions of `r` | `ab+` | ab, abb, abbb | a, b, aba |
| `r{m,n}` | Between `m` and `n` repetitions | `a{2,4}` | aa, aaa, aaaa | a, aaaaa, aba |
| `r{m}` | Exactly `m` repetitions | `a{3}` | aaa | a, aa, aaaa, bbb |
| `r?` | Optional `r` | `ab?a` | aa, aba | abba, aca |
| `r\|s` | Matching alternatives, i. e. `r` or `s` | `a+\|b+` | a, b, aa, bb, aaa | ab, aabb |


Character Sets, Ranges, Subgroups and Lookarounds
| Expression | Represents | Example | Matches | Does not Match |
|---|---|---|---|---|
| `[aA1-]` | Character set, matches a single character present in the list | `[aA1-]` | a, A, 1, - | b, B, cc, 3 |
| `[a-z0-9]` | Character range, matches a single character in the specified range, note that ranges may be locale-dependent | `[a-c0-5]` | b, c, 2, 4 | d, Z, x, 9 |
| `[^aA1]` | Negation, matches any single character not present in the list | `[^aA1]` | b, C, 3, - | a, A, 1 |
| `[^0-9]` | Negation, matches any single character not within the range | `[^0-9]` | a, B, c | 1, 2, 3 |
| `(...)` | Capturing group to group parts of patterns together | `a(b\|c)a` | aba, aca | aa, abca |
| `(?=...)` | Positive lookahead, returns characters that are followed by a specified pattern without including this pattern | `a(?=b)` <br>(example string: abc ade) | the first a | the second a |
| `(?!...)` | Negative lookahead, returns characters that are not followed by a specified pattern without including this pattern | `a(?!b)` <br>(example string: abc ade) | the second a | the first a |
| `(?<=...)` | Positive lookbehind, returns characters that are preceded by a specified pattern without including this pattern | `(?<=\s)c` <br>(example string: ab c abcd) | the first c since it is preceded by a blank | the second c |
| `(?<!...)` | Negative lookbehind, returns characters that are not preceded by a specified pattern without including this pattern | `(?<!\s)c` <br>example string: ab c abcd) | the second c since it is not preceded by a blank | the first c |

> **💡 Note**<br>
> Subgroups are handy in replacements. Using an expression with `$` and a number, e. g. `$1`, you can refer to a particular group. For example, you have a string `abcde`. A PCRE expression might be
`(ab|xy)c(d.)`, i. e. there are two subgroups specified within two pairs of parentheses. In a replacement pattern, you can refer to the first group using `$1` and the second group using `$2`. Hence, the replacement pattern `$2Z$1` results in `deZab`.

Anchors and Positions

| Expression | Represents | Example | Matches | Does not Match |
|---|---|---|---|---|
| `^` | Beginning of line, alternative: `\A` | `^a.` <br>(example string: abcde) | ab | bc |
| `$` | End of line, alternative: `\Z` | `$` <br>(example string: abcde) | the position right after e | any other position |
| `\b` | beginning and end of word | 1) `\ba\d` <br>2) `\Dd\b` <br>(example string: abcd a12d) | 1) `a1` <br>2) `cd` | 1) `ab` <br> 2) `2d` |

<p align="right">(<a href="#top">back to top</a>)</p>

### Searching Using Regular Expressions

Multiple string functions support PCRE expressions by offering the
parameter `pcre` with which you can specify such an expression.
`FIND` and `REPLACE` statements support regular
expressions with the `PCRE` addition.

The string function
[`match`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmatch_functions.htm)
exclusively works with regular expressions. It returns a substring that
matches a regular expression within a string. For comparisons, you could
also use the [predicate
function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpredicate_function_glosry.htm "Glossary Entry")
[`matches`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmatches_functions.htm)
that returns true or false if a string matches a given pattern or not.

Syntax examples:
``` abap
s1 = `Cathy's black cat on the mat played with Matt.`.

"Determining the position of the first occurrence
"Here, the parameter occ is 1 by default.
s2 = find( val = s1 pcre = `at.` ). "1

"Determining the number of all occurrences.
"Respects all 'a' characters not followed by 't', all 'at' plus 'att'
s2 = count( val = s1  pcre = `at*` ). "6

"Respects all 'at' plus 'att'
s2 = count( val = s1 pcre = `at+` ). "4

"Extracting a substring matching a given pattern
s1 = `The email address is jon.doe@email.com.`.
s2 = match( val = s1
            pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ). "jon.doe@email.com

"Predicate function matches
"Checking the validitiy of an email address
s1 = `jon.doe@email.com`.
IF matches( val  = s1
            pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ).  "true
...
ENDIF.

"Examples with the FIND statement
"Storing submatches in variables.
"Pattern: anything before and after ' on '
FIND PCRE `(.*)\son\s(.*)` IN s1 IGNORING CASE SUBMATCHES DATA(a) DATA(b).
"a = 'Cathy's black cat' / b = 'the mat played with Matt'.

"Determinging the number of letters in a string
FIND ALL OCCURRENCES OF PCRE `[A-Za-z]` IN s1 MATCH COUNT DATA(c). "36

"Searching in an internal table and retrieving line, offset, length information
DATA(itab) = value string_table( ( `Cathy's black cat on the mat played with the friend of Matt.` ) ).
"Pattern: 't' at the beginning of a word followed by another character
FIND FIRST OCCURRENCE OF PCRE `\bt.` IN TABLE itab
  IGNORING CASE MATCH LINE DATA(d) MATCH OFFSET DATA(e) MATCH LENGTH DATA(f). "d: 1, e: 21, f: 2
```
<p align="right">(<a href="#top">back to top</a>)</p>

### Replacing Using Regular Expressions

To carry out replacement operations using regular expressions both
string function `replace` and `REPLACE` statements can
be used with the `pcre` parameter or the `PCRE` addition
respectively. Similar to the `find` function, among others, and
`FIND` statements, the `replace` function and
`REPLACE` statements offer a variety of parameters or additions
respectively to further restrict the area to be replaced. Check the ABAP
Keyword Documentation for a more detailed insight. The executable
example covers numerous PCRE expressions listed above with the
`replace` function.

Syntax examples:
``` abap
s1 = `ab apppc app`.

"Replaces 'p' with 2 - 4 repetitions, all occurences
s2 = replace( val = s1 pcre = `p{2,4}` with = `#` occ = 0 ). "ab a#c a#

"Replaces any single character not present in the list, all occurences)
s2 = replace( val = s1 pcre = `[^ac]` with = `#` occ = 0 ). " "a##a###c#a##

"Replaces first occurence of a blank
s2 = replace( val = s1 pcre = `\s` with = `#` ). "ab#apppc app

"Greedy search
"The pattern matches anything before 'p'. The matching is carried out as
"often as possible. Hence, in this example the search stretches until the
"end of the string since 'p' is the final character, i. e. this 'p' and
"anything before is replaced.
s2 = replace( val = s1 pcre = `(.*)p` with = `#` ). "#

"Non-greedy search (denoted by '?' below)
"The pattern matches anything before 'p'. The matching proceeds until
"the first 'p' is found and does not go beyond. It matches as few as
"possible. Hence, the first found 'p' including the content before
"is replaced.
s2 = replace( val = s1 pcre = `(.*?)p` with = `#`  ). "#ppc app

"Replacements with subgroups
"Replaces 'pp' (case-insensitive here) with '#', the content before and after 'pp' is switched
s2 = replace( val  = s1       pcre = `(.*?)PP(.*)`
              with = `$2#$1` case = abap_false ). "pc app#ab a

"Changing the source field directly with a REPLACE statement; same as above
REPLACE PCRE `(.*?)PP(.*)` IN s1 WITH `$2#$1` IGNORING CASE. "pc app#ab a
```

<p align="right">(<a href="#top">back to top</a>)</p>

## Executable Example
[zcl_demo_abap_string_proc](./src/zcl_demo_abap_string_proc.clas.abap)

Note the steps outlined [here](README.md#getting-started-with-the-examples) about how to import and run the code.
