<a name="top"></a>

# Regular Expressions in ABAP

- [Regular Expressions in ABAP](#regular-expressions-in-abap)
  - [Introduction](#introduction)
  - [Excursion: Common Regular Expressions](#excursion-common-regular-expressions)
    - [Characters and Character Types](#characters-and-character-types)
    - [Escaped Characters and Special Variants](#escaped-characters-and-special-variants)
    - [Quantifiers, Repetitions and Alternatives](#quantifiers-repetitions-and-alternatives)
    - [Character Sets and Ranges](#character-sets-and-ranges)
    - [Anchors and Positions](#anchors-and-positions)
    - [Capturing Groups, Replacements and Backreferences](#capturing-groups-replacements-and-backreferences)
    - [Lookarounds](#lookarounds)
    - [Case Conversions in Replacement Patterns](#case-conversions-in-replacement-patterns)
    - [Setting Options and Control Verbs](#setting-options-and-control-verbs)
    - [Callouts](#callouts)
    - [Conditional Patterns](#conditional-patterns)
  - [ABAP Statements Using Regular Expressions](#abap-statements-using-regular-expressions)
  - [Built-In Functions in ABAP Using Regular Expressions](#built-in-functions-in-abap-using-regular-expressions)
  - [Built-In Functions in ABAP SQL and CDS Using Regular Expressions](#built-in-functions-in-abap-sql-and-cds-using-regular-expressions)
  - [System Classes for Regular Expressions: CL\_ABAP\_REGEX and CL\_ABAP\_MATCHER](#system-classes-for-regular-expressions-cl_abap_regex-and-cl_abap_matcher)
  - [Executable Example](#executable-example)

## Introduction

Regular expressions
- Often abbreviated as regex  
- Define a pattern of literal and special characters that describe a set of character strings 
- Find one or more occurrences of the character strings defined by the expression
- Offer a powerful way to perform complex searching, replacing, and matching
- Can be used in ABAP in the following contexts (with a focus on PCRE):
  - `FIND` and `REPLACE` statements (with the `PCRE` addition) 
  - Built-in functions in ABAP with the `pcre` parameter, such as `find`, `find_end`, `count`, `match`, `matches`, `replace`, `contains`, `substring_from`, `substring_after`, `substring_before`, `substring_to` 
  - Built-in functions in ABAP SQL and CDS (e.g. `like_regexpr`, `locate_regexpr`, `locate_regexpr_after`, `occurrences_regexpr`, `replace_regexpr`, `substring_regexpr` in ABAP SQL)
  - `CL_ABAP_REGEX` and `CL_ABAP_MATCHER` classes (note that you can also use objects of `CL_ABAP_REGEX` in `FIND` and `REPLACE` statements)
- Are supported in ABAP with the following syntaxes:
  - Perl Compatible Regular Expressions (PCRE)
     - ABAP supports PCRE syntax via the PCRE2 library in the ABAP kernel, and ABAP SQL supports PCRE by using the PCRE1 library in the SAP HANA database.  
  - XPath 
  - XSD 
    - XPath and XSD regular expressions are internally and automatically converted into PCRE syntax. 
    - The syntax of regular expressions is generally standardized but varies with different syntax flavors.  
  - POSIX 
    - Obsolete; `REGEX` and `regex` are obsolete additions/parameters 
    - PCRE regular expressions perform better 
    - A syntax warning for POSIX can be hidden with the pragma `##regex_posix`

> **💡 Note**<br>
> - You can perform complex searches using regular expressions. For simple pattern-based searches, refer to comparison operators (`CP`, `NP`) in the [String Processing](07_String_Processing.md) cheat sheets.
> - The cheat sheet and examples focus on PCRE regular expressions. For other syntax types, find more information and links in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENREGEX_SYNTAX.html). 
> - In a system supporting [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm), you can check out the `demo_regex_toy` program for experimenting with regular expressions in ABAP.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursion: Common Regular Expressions

> **💡 Note**<br>  
> - The sections below provide an overview of common PCRE expressions with examples. This is not a comprehensive overview; only selected regular expressions are included.
> - For a complete guide to PCRE syntax, refer to the [official documentation](https://perldoc.perl.org/perlre). Note that ABAP-specific restrictions or modifications may apply to the standard syntax.
> - The code snippets use `replace` functions to show the effects of PCRE regular expressions. Many examples use the `occ` parameter with the assignment `occ = 0` to replace all occurrences.
> - When using regular expressions, note that exceptions such as of type `cx_sy_invalid_regex` or `cx_sy_regex_too_complex` can occur.

### Characters and Character Types

| Expression | Represents | Example Regex | Example String | Matches | Does not Match |
|---|---|---|---|---|---|
| `x` | Specific character | `a` | abcdef | a | Anything else |
| `.` | Any character except a line break (unless in dotall mode, then any character) | `.` | ab 1# | a, b, the blank, 1, # | ab, 1# |
| `\d` | Any digit (0-9), alternative: `[0-9]` | `\d` | a1-b2 3-4c9 | 1, 2, 3, 4, 9 | a, b, c, the blank and hyphens |
| `\D` | Any non-digit, alternative: `[^0-9]` | `\D` | a1-b2 3-4c9 | a, b, c, the blank and hyphens | 1, 2, 3, 4, 9 |
| `\s` | Any whitespace character such as a blank, tab and new line | `\s` | (hi X ) | The blanks | h, i, X, (, ) |
| `\S` | Any character that is not a whitespace | `\S` | (hi X ) | h, i, X, (, ) | The blanks |
| `\w` | Any word character (letter, digit or the underscore), alternative: `[a-zA-Z0-9_]` | `\w` | (ab 12_c) | a, b, c, 1, 2, _ | (, ), the blank |
| `\W` | Any character that is not a word character, alternative: `[^a-zA-Z0-9_]` | `\W` | (ab 12_c) | (, ), the blank | a, b, c, 1, 2, _ |

Examples:


```abap
DATA string_chars_types TYPE string.

"Specific character
"#bcdef
string_chars_types = replace( val = `abcdef` pcre = `a` with = `#` occ = 0 ).

"Any character except a line break
"_______ (7 replacements before the line break)
"____ (4 replacements after the line break)
string_chars_types = replace( val = |ab 1#?C\n D23| pcre = `.` with = `_` occ = 0 ).

"Any digit
"a#-b# #-#c#
string_chars_types = replace( val = `a1-b2 3-4c9` pcre = `\d` with = `#` occ = 0 ).

"Any non-digit
"#1##2#3#4#9
string_chars_types = replace( val = `a1-b2 3-4c9` pcre = `\D` with = `#` occ = 0 ).

"Any whitespace character such as a blank, tab and new line
"ab#cd#ef#gh
string_chars_types = replace( val = |ab cd\tef\ngh| pcre = `\s` with = `#` occ = 0 ).

"Any character that is not a
"String template representation of the result: |## ##\t##\n##|
string_chars_types = replace( val = |ab cd\tef\ngh| pcre = `\S` with = `#` occ = 0 ).

"Any word character (letter, digit or the underscore)
"(## ####)
string_chars_types = replace( val = `(ab 12_c)` pcre = `\w` with = `#` occ = 0 ).

"Any character that is not a word character
"#ab#12_c#
string_chars_types = replace( val = `(ab 12_c)` pcre = `\W` with = `#` occ = 0 ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Escaped Characters and Special Variants

Note that some of the example strings are represented by string templates.

| Expression | Represents | Example Regex | Example String | Matches | Does not Match |
|---|---|---|---|---|---|
| `\...` | To include special characters like `[] \ / ^`, use `\` to escape them. Use `\.` to match a period ("."). | `.\.` | ab.cd.ef |  a<ins>**b.**</ins>c<ins>**d.**</ins>ef | ab<ins>**.c**</ins>d<ins>**.e**</ins>f |
| `\n` | Line feed (negation `\N` for non-line feeds) | `\n\D` | <code>\|\nabc\n123\|</code> | <code>\|\na\|</code> | <code>\|\n1\|</code> |
| `\t` | Tab | `\t\d` | <code>\|\tabc\t123\|</code> | <code>\|\t1\|</code> | <code>\|\ta\|</code> |
| `\r` | Carriage return | `\r\s` | <code>\|\rabc\r 123\|</code> | <code>\|\r \|</code> | <code>\|\ra\|</code> |
| `\R` | Line feed sequence, ensuring that regular expression matches all kinds of newlines, such as `\n`, `\r`, or `\r\n` (carriage return followed by line feed)  | `\R.` | <code>\|abc\rdef\nghi\r\njkl mno\|</code> | <code>\|\rd\|</code>, <code>\|\ng\|</code>, <code>\|\r\nj\|</code> | <code>\| m\|</code> |
| `\x{...}` | Character with hex code | `\x{00A0}#` (non-breaking space and #) | <code>\|#{ cl_abap_conv_codepage=>create_in( codepage = \`UTF-16BE\` )->convert( source = CONV xstring( \`00A0\` ) ) }#\|</code> | The non-breaking space plus the second # character | The first # character |
| `\N{U+...}` | Character with Unicode code point | `\N{U+00A0}#` (non-breaking space and # as above) | See the example below | See the example below | |
| `\p{..}` | Character with a specified Unicode character property; see the [documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenregex_pcre_syntax_specials.html) for options; among them, there are, for example, properties for lowercase (`Ll`) and uppercase (`Lu`) letters; negation: `\P{..}` | 1. `\p{Ll}+` <br> 2. `\p{Lu}+` <br> 3. `\P{Lu}+`  | Hello ABAP | 1. H<ins>**ello**</ins> ABAP <br> 2. <ins>**H**</ins>ello <ins>**ABAP**</ins> <br> 3. H<ins>**ello&nbsp;**</u></ins>ABAP (the space is also matched) | 1. Upper case letter sequences <br> 2. Lower case letter sequences <br> 3. Upper case letter sequences |

Examples:


```abap
DATA string_esc_chars TYPE string.

"Special characters
"a#b#c#d#e#f#g
string_esc_chars = replace( val = `a[b]c\d/e^f.g` pcre = `\[|\]|\\|\/|\^|\.` with = `#` occ = 0 ).

"Line feeds
"a#b#c
string_esc_chars = replace( val = |a\nb\nc| pcre = `\n` with = `#` occ = 0 ).

"Line feed negation
"String template representation of the result: |#\n#\n#|
string_esc_chars = replace( val = |a\nb\nc| pcre = `\N` with = `#` occ = 0 ).

"Tabs
"a#b#c
string_esc_chars = replace( val = |a\tb\tc| pcre = `\t` with = `#` occ = 0 ).

"Carriage return
"d#e#f
string_esc_chars = replace( val = |d\re\rf| pcre = `\r` with = `#` occ = 0 ).

"Characters with hex code
"The example string includes a non-breaking space.
"#x#
string_esc_chars = replace( 
  val = |#{ cl_abap_conv_codepage=>create_in( codepage = `UTF-16BE` )->convert( source = CONV xstring( `00A0` ) ) }#|
  pcre = `\x{00A0}` with = `x` occ = 0 ).

"Characters with Unicode code point
"As above, the example string includes a non-breaking space.
"The PCRE syntax uses the control verb (*UTF) to enable UTF mode.
"#y#
string_esc_chars = replace( 
  val = |#{ cl_abap_conv_codepage=>create_in( codepage = `UTF-16BE` )->convert( source = CONV xstring( `00A0` ) ) }#|
  pcre = `(*UTF)\N{U+00A0}` with = `y` occ = 0 ).

"Characters with a specified Unicode character property
"H# ABAP
string_esc_chars = replace( val = `Hello ABAP` pcre = `\p{Ll}+` with = `#` occ = 0 ).

"#ello #
string_esc_chars = replace( val = `Hello ABAP` pcre = `\p{Lu}+` with = `#` occ = 0 ).

"#ello#
string_esc_chars = replace( val = `Hello ABAP` pcre = `\P{Ll}+` with = `#` occ = 0 ).

"H#ABAP
string_esc_chars = replace( val = `Hello ABAP` pcre = `\P{Lu}+` with = `#` occ = 0 ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Quantifiers, Repetitions and Alternatives

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


Examples:


```abap
DATA string_quan_rep_alt TYPE string.

"Zero or more repetitions
"#c #c #c # #c
string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab*` with = `#` occ = 0 ).

"One or more repetitions
"#c #c #c a ac
string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab+` with = `#` occ = 0 ).

"Between x and y repetitions
"abc #c #c a ac
string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab{2,3}` with = `#` occ = 0 ).

"Exactly n repetitions
"abc abbc #c a ac
string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab{3}` with = `#` occ = 0 ).

"Exactly n or more repetitions
"abc #c #c a ac
string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab{2,}` with = `#` occ = 0 ).

"Optional matches
"#c #bc #bbc # #c
string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab?` with = `#` occ = 0 ).

"Alternative matches
"#z#y#xdwevfu
string_quan_rep_alt = replace( val = `azbycxdwevfu` pcre = `a|b|c` with = `#` occ = 0 ).

"Capturing non-greedily (zero or more repetitions)
"a#cd a#ccccd a#
string_quan_rep_alt = replace( val = `abcd abccccd ab` pcre = `bc*?` with = `#` occ = 0 ).

"_defghxi
string_quan_rep_alt = replace( val = `abcxdefghxi` pcre = `\A.*?x` with = `_` ).

"Example to compare with greedy capturing
"_i
string_quan_rep_alt = replace( val = `abcxdefghxi` pcre = `\A.*x` with = `_` ).

"Capturing non-greedily (one or more repetitions)
"a#d a#cccd ab
string_quan_rep_alt = replace( val = `abcd abccccd ab` pcre = `bc+?` with = `#` occ = 0 ).

"Example to compare with greedy capturing
"a#d a#d ab
string_quan_rep_alt = replace( val = `abcd abccccd ab` pcre = `bc+` with = `#` occ = 0 ).

"#Hallo#
string_quan_rep_alt = replace( val = `<span>Hallo</span>` pcre = `<.+?>` with = `#` occ = 0 ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Character Sets and Ranges

| Expression | Represents | Example Regex | Example String | Matches | Does not Match |
|---|---|---|---|---|---|
| `[...]` | Character set, matches a single character present in the list | `b[iu]` | bit bat but bet | <ins>**bi**</ins>t bat <ins>**bu**</ins>t bet  | bit <ins>**ba**</ins>t but <ins>**be**</ins>t |
| `[x-y]` | Character range, matches a single character in the specified range, note that ranges may be locale-dependent | `a[a-c0-5]` | aa1 ab2 ba3 cac4 da56 a7 |<ins>**aa**</ins>1 <ins>**ab**</ins>2 b<ins>**a3**</ins> c<ins>**ac**</ins>4 d<ins>**a5**</ins>6 a7 | aa1 ab2 ba3 cac4 da56 <ins>**a7**</ins> |
| `[^...]` | Negation, matches any single character not present in the list | `[^Ap]` | ABap | B, a | A, p |
| `[^x-y]` | Negation, matches any single character not within the range | `[^A-Ca-c1-4]` | ABCDabcd123456 | D, d, 5, 6 | A, B, C, a, b, c, 1, 2, 3, 4 |

Examples:


```abap
DATA string_char_sets_ranges TYPE string.

"Defining a character set
"#t bat #t bet
string_char_sets_ranges = replace( val = `bit bat but bet` pcre = `b[iu]` with = `#` occ = 0 ).

"Defining a character range
"#1 #2 b# c#4 d#6 a7
string_char_sets_ranges = replace( val = `aa1 ab2 ba3 cac4 da56 a7` pcre = `a[a-c0-5]` with = `#` occ = 0 ).

"Matching any single character not present in the list
"A##p
string_char_sets_ranges = replace( val = `ABap` pcre = `[^Ap]` with = `#` occ = 0 ).

"Matching any single character not within the range
"ABC#abc#1234##
string_char_sets_ranges = replace( val = `ABCDabcd123456` pcre = `[^A-Ca-c1-4]` with = `#` occ = 0 ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Anchors and Positions

| Expression | Represents | Example Regex | Example String | Matches | Does not Match |
|---|---|---|---|---|---|
| `\A` | Start of a subject | `\A.` | abc def | <ins>**a**</ins>bc def | abc <ins>**d**</ins>ef  |
| `^` | Start of a subject (particularly relevant in multi-line mode) | `^.` | See above and more details in the option setting section |  |   |
| `\Z` | End of a subject | `.\Z`  | abc def | abc de<ins>**f**</ins> | <ins>**a**</ins>bc def |
| `$` | End of a subject (particularly relevant in multi-line mode) | `.$`  | See above and more details in the option setting section |  |   |
| `\b` | Start or end of word | 1. `\ba.` <br>2. `\Dd\b` <br>3. `\b.d\b` | abcd a12d ed | 1. <ins>**ab**</ins>cd <ins>**a1**</ins>2d ed <br>2. ab<ins>**cd**</ins> a12d <ins>**ed**</ins> <br> 3. abcd a12d <ins>**ed**</ins> | 1. ab<ins>**cd**</ins> a1<ins>**2d**</ins> ed <br> 2. abcd a1<ins>**2d**</ins> ed <br> 3. <ins>**abcd**</ins> <ins>**a12d**</ins> ed |
| `\B` | Negation of `\b`, not at the start or end of words | `\Be\B` | see an elefant | s<ins>**e**</ins>e an el<ins>**e**</ins>fant  | s<ins>**ee**</ins> an <ins>**e**</ins>lefant |
| `\K` | Resets the starting point of a match, i.e. findings are excluded from the final match | `a.\Kc` | abcd | ab<ins>**c**</ins>d | <ins>**abc**</ins>d |


Examples:


```abap
DATA string_anchors_pos TYPE string.

"Start of subject, syntax \A
"#abc def
string_anchors_pos = replace( val = `abc def` pcre = `\A` with = `#` occ = 0 ).

"Start of subject, syntax ^; find more information below regarding multi-line mode
"#abc
"#def
"#ghi
string_anchors_pos = replace( val = |abc\ndef\nghi| pcre = `(?m)^` with = `#` occ = 0 ).

"The following examples uses ^ without enabling the multi-line mode
"#abc
"def
"ghi
string_anchors_pos = replace( val = |abc\ndef\nghi| pcre = `^` with = `#` occ = 0 ).

"End of subject, syntax \Z
"abc def#
string_anchors_pos = replace( val = `abc def` pcre = `\Z` with = `#` occ = 0 ).

"End of subject, syntax $
"The example uses multi-line mode
"abc#
"def#
"ghi#
string_anchors_pos = replace( val = |abc\ndef\nghi| pcre = `(?m)$` with = `#` occ = 0 ).

"Start or end of word
"#cd #2d ed
string_anchors_pos = replace( val = `abcd a12d ed` pcre = `\ba.` with = `#` occ = 0 ).

"ab# a12d #
string_anchors_pos = replace( val = `abcd a12d ed` pcre = `\Dd\b` with = `#` occ = 0 ).

"abcd a12d #
string_anchors_pos = replace( val = `abcd a12d ed` pcre = `\b.d\b` with = `#` occ = 0 ).

"Not at the start or end of words
"s#e an el#fant
string_anchors_pos = replace( val = `see an elefant` pcre = `\Be\B` with = `#` occ = 0 ).

"Resetting the starting point of a match
"ab#d
string_anchors_pos = replace( val = `abcd` pcre = `a.\Kc` with = `#` occ = 0 ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Capturing Groups, Replacements and Backreferences

| Expression | Represents | Example Regex | Example String | Matches | Does not Match |
|---|---|---|---|---|---|
| `(...)` | Capturing group to group parts of patterns together | `b(a\|u)t` | bit bat but bet | bat, but | bit, bet |
| `(?<name>...)`, `(?'name'...)` | Named capturing group | `(?<x>..)(?'y'..)` | abcd | `$x` refers to *ab*, `$y` to *cd* <br> The reference can also be made using curly brackets: `${x}`, `${y}` | `$x` refers to *cd*, `$y` to *ab* |
| `$id`, `${id}`  | Represents the substitute for a capturing group, `id` stands for a number or name of a capturing group | `(..)(..)` | abcd | `$1` refers to *ab*, `$2` to *cd* <br> `$0` refers to the content of the whole match <br> As above, the reference can also be made using curly brackets: `${0}`, `${1}`, `${2}`  | `$1` refers to *cd*, `$2` to *ab* |
| `(?:...)` | Creates a group but it is not captured | `(?:ab)(ap)` | abap | It matches *abap*, but in a replacement and when referring to the subgroup `$1`, it refers to *ap*. |  ab |
| `\1` | Backreference, refers to a previous capturing group; 1 represents the number of the group index (the group index starts with 1); more back reference syntax options are possible such as the specification of named groups | `(a.)(\w*)\1` | abcdefabghij | <ins>**abcdefab**</ins>ghij <br>Note: Capturing group 1 holds `ab` in the example. The second capturing group captures all word characters until `ab` is found. | <ins>**ab**</ins>cdefabghij |

Examples:


```abap
DATA string_capt_group TYPE string.

"bit # # bet
string_capt_group = replace( val = `bit bat but bet` pcre = `b(a|u)t` with = `#` occ = 0 ).

"------- Replacements with capturing groups -------

"ABAP
string_capt_group = replace( val = `APAB` pcre = `(..)(..)` with = `$2$1` ).

"$0 representing the content of the whole match
"#abcd#
string_capt_group = replace( val = `abcd` pcre = `(..)(..)` with = `#$0#` ).

"Alternative replacement syntax with the curly brackets
"ABAP
string_capt_group = replace( val = `APAB` pcre = `(..)(..)` with = `${2}${1}` ).

"------- Named capturing groups -------


"PCRE syntax (?<name>...), replacement syntax $name
"Examplatory result: Day: 28, month: 11, year: 2024
string_capt_group = replace( val = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value
                              pcre = `(?<yr>\d{4})(?<mo>\d{2})(?<d>\d{2})`
                              with = `Day: $d, month: $mo, year: $yr` ).


"Alternative PCRE syntax (?'name'...), replacement syntax ${name}
"Examplatory result: 28.11.2024
string_capt_group = replace( val = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value
                              pcre = `(?'yr'\d{4})(?'mo'\d{2})(?'d'\d{2})`
                              with = `${d}.${mo}.${yr}` ).

"Creating a group but not capturing it
"#cd#
string_capt_group = replace( val = `abcd` pcre = `(?:..)(..)` with = `#$1#` ).

"The following example raises an exception. $2 cannot be referred to.
TRY.
    string_capt_group = replace( val = `abcd` pcre = `(?:..)(..)` with = `#$1#$2#` ).
  CATCH cx_sy_invalid_regex_format.
ENDTRY.

"#abcd#
string_capt_group = replace( val = `abcd` pcre = `(?:..)(..)` with = `#$0#` ).

"------- Back reference -------

"In the example, the capturing group 1 holds `ab`. The second capturing group captures
"all word characters until `ab` is found, including `ab`. The reference to the first
"capturing group is made using \1.
"#ghij
string_capt_group = replace( val = `abcdefabghij` pcre = `(a.)(\w*)\1` with = `#` ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Lookarounds

| Expression | Represents | Example Regex | Example String | Matches | Does not Match |
|---|---|---|---|---|---|
| `(?=...)` | Positive lookahead, returns characters that are followed by a specified pattern without including this pattern | `a(?=b)` | abc ade | <ins>**a**</ins>bc ade | abc <ins>**a**</ins>de |
| `(?!...)` | Negative lookahead, returns characters that are not followed by a specified pattern without including this pattern | `a(?!b)` | abc ade | abc <ins>**a**</ins>de | <ins>**a**</ins>bc ade |
| `(?<=...)` | Positive lookbehind, returns characters that are preceded by a specified pattern without including this pattern | `(?<=\s)c` | ab c abcd | ab <ins>**c**</ins> abcd (it is preceded by a blank) | ab c ab<ins>**c**</ins>d |
| `(?<!...)` | Negative lookbehind, returns characters that are not preceded by a specified pattern without including this pattern | `(?<!\s)c` | ab c abcd | ab c ab<ins>**c**</ins>d (it is not preceded by a blank) | ab <ins>**c**</ins> abcd |

Examples:

```abap
DATA string_look_arounds TYPE string.

"Positive lookahead
"#bc ade
string_look_arounds = replace( val = `abc ade` pcre = `a(?=b)` with = `#` occ = 0 ).

"Negative lookahead
"abc #de
string_look_arounds = replace( val = `abc ade` pcre = `a(?!b)` with = `#` occ = 0 ).

"Positive lookbehind
"ab # abcd
string_look_arounds = replace( val = `ab c abcd` pcre = `(?<=\s)c` with = `#` occ = 0 ).

"Negative lookbehind
"ab c ab#d
string_look_arounds = replace( val = `ab c abcd` pcre = `(?<!\s)c` with = `#` occ = 0 ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Case Conversions in Replacement Patterns

You can use the following syntax options for case conversion in replacement patterns:

- `\u`: Converts the first character after this marker to uppercase.
- `\U`: Converts all characters to uppercase until the next `\L` or `\E` marker.
- `\l`: Converts the first character after this marker to lowercase.
- `\L`: Converts all characters to lowercase until the next `\U` or `\E` marker.
- `\E`: Ends the current uppercase or lowercase transformation.


Examples:


```abap
DATA string_case_conv TYPE string.

"\u syntax
"abcDefg
string_case_conv = replace( val = `abcdefg` pcre = `c(.*)` with = `c\u$1` ).

"\U syntax
"abcDEFG
string_case_conv = replace( val = `abcdefg` pcre = `c(.*)` with = `c\U$1` ).

"\l syntax
"HIJKlMNO
string_case_conv = replace( val = `HIJKLMNO` pcre = `K(.*)` with = `K\l$1` ).

"\L syntax
"HIJKlmno
string_case_conv = replace( val = `HIJKLMNO` pcre = `K(.*)` with = `K\L$1` ).

"\E syntax
"abcDEfg
string_case_conv = replace( val = `abcdefg` pcre = `c(..)(..)` with = `c\U$1\E$2` ).

"The following example is a comparison to similar syntax without specifying \E
"abcDEFG
string_case_conv = replace( val = `abcdefg` pcre = `c(..)(..)` with = `c\U$1$2` ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Setting Options and Control Verbs

There are various [setting options](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenregex_pcre_syntax_specials.html) for specifying PCRE syntax. The following examples demonstrate a selection:

*Multi- and single-line modes*
- `(?m)` enables multi-line mode.
- `(?s)` enables single-line mode.
- Note in this context:
  - The `.` character does not match new lines unless single-line mode is set.
  - The anchors `^` and `$` become especially relevant in multi-line mode.
  - These settings apply to the subsequent part of the group in which they are specified, such as the top-level pattern or a capturing group.


Examples:


```abap
DATA string_opt_set TYPE string.

"----------- Examples with `.` and single line mode -----------

"`.` matches new lines feeds when the single line option is set
"Result (9 replacements):
"###
"###
"###
string_opt_set = replace( val = |abc\ndef\nghi| pcre = `.` with = `#` occ = 0 ).

"Result (11 replacements including the new line feeds):
"###########
string_opt_set = replace( val = |abc\ndef\nghi| pcre = `(?s).` with = `#` occ = 0 ).

"##
string_opt_set = replace( val = |\n\n| pcre = `(?s).` with = `#` occ = 0 ).

"----------- Examples for the start of subjects -----------

"Multi-line mode not enabled
"Result:
"#bc
"def
"ghi
string_opt_set = replace( val = |abc\ndef\nghi| pcre = `^.` with = `#` occ = 0 ).

"Multi-line mode enabled
"Result:
"#bc
"#ef
"#hi
string_opt_set = replace( val = |abc\ndef\nghi| pcre = `(?m)^.` with = `#` occ = 0 ).

"\A for comparison
"Result:
"#bc
"def
"ghi
string_opt_set = replace( val = |abc\ndef\nghi| pcre = `(?m)\A.` with = `#` occ = 0 ).

"----------- Examples for the end of subjects -----------

"Result:
"abc
"def
"gh#
string_opt_set = replace( val = |abc\ndef\nghi| pcre = `.$` with = `#` occ = 0 ).

"Result:
"ab#
"de#
"gh#
string_opt_set = replace( val = |abc\ndef\nghi| pcre = `(?m).$` with = `#` occ = 0 ).

"\Z syntax (the result is the same as with `.$`)
string_opt_set = replace( val = |abc\ndef\nghi| pcre = `(?m).\Z` with = `#` occ = 0 ).
```

*Extended Mode*

- Regular expressions with PCRE can be processed in normal or extended mode.
- In extended mode, unescaped whitespaces in the pattern are ignored.
- The extended mode is the default for ABAP statements and built-in functions, but you can disable it in the regex pattern with `(?-x)`. The `create_pcre` method of the `CL_ABAP_REGEX` class includes an `extended` parameter for this purpose.
- To handle whitespaces in regular expressions, escape it with `\ `, match it using `\s`, or enable extended mode with `(?-x)`.

The following code snippet shows the extended mode enabled by default and then explicitly disabled in a PCRE regular expression.


```abap
DATA some_string TYPE string.

"Extended mode is enabled by default
"No replacement in the example.
"Result: abc def
some_string = replace( val = `abc def` pcre = `abc def` with = `#` ).

"The following example works because there is no whitespace in the source string.
"The whitespace in the regular expression is ignored.
"Result: #
some_string = replace( val = `abcdef` pcre = `abc def` with = `#` ).

"To match whitespace characters, you can use the pattern \s
"Result: #
some_string = replace( val = `abc def` pcre = `abc\sdef` with = `#` ).

"Escaping a whitespace using \
"Result: #
some_string = replace( val = `abc def` pcre = `abc\ def` with = `#` ).
"The following example also performs the replacement as the unescaped whitespaces
"are ignored.
some_string = replace( val = `abc def` pcre = `abc\         def` with = `#` ).

"Disabling the extended mode so that whitespaces are not ignored
"Result: #
some_string = replace( val = `abc def` pcre = `(?-x)abc def` with = `#` ).
```

*Control verbs*

Control verbs, for example, for ...

- enabling UTF mode using `(*UTF)`. In PCRE syntax, you can define UTF-16 character strings.
- line breaks such as the following:
  - `(*CR)`: Only carriage returns
  - `(*LF)`	Only line feed
  - `(*CRLF)`: Only carriage returns followed by a line feed
  - `(*ANYCRLF)`: All of the above-mentioned options


```abap
DATA ctrl_verb_string TYPE string.

"Using (*UTF); the source string contains a non-breakable space that is to be replaced
"Result: #x#
ctrl_verb_string = replace( 
  val = |#{ cl_abap_conv_codepage=>create_in( codepage = `UTF-16BE` )->convert( source = CONV xstring( `00A0` ) ) }#|
  pcre = `(*UTF)\N{U+00A0}` with = `x` occ = 0 ).

"Line breaks
"The results are demonstrated using string templates.
"In the examples, the multi-line mode is enabled in addition.

"|_abc\ndef\r_ghi\r_\njkl|
ctrl_verb_string = replace( val = |abc\ndef\rghi\r\njkl|
                            pcre = `(*CR)(?m)^` with = `_` occ = 0 ).

ctrl_verb_string = replace( val = |abc\ndef\rghi\r\njkl|
                            pcre = `(*LF)(?m)^` with = `_` occ = 0 ).

"|_abc\n_def\rghi\r\n_jkl|
ctrl_verb_string = replace( val = |abc\ndef\rghi\r\njkl|
                            pcre = `(*CRLF)(?m)^` with = `_` occ = 0 ).

"|_abc\n_def\r_ghi\r\n_jkl|
ctrl_verb_string = replace( val = |abc\ndef\rghi\r\njkl|
                            pcre = `(*ANYCRLF)(?m)^` with = `_` occ = 0 ).
``` 


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Callouts

- Callouts invoke ABAP code during pattern matching.  
- They access the current matcher state for further processing and can influence regex processing.  
- In ABAP, object-oriented representations of PCRE regular expressions can use callouts.  
- Relevant PCRE syntax includes `(?Cn)` (*n* is a number) and `(?C"text")`.  

The following example shows how to use callouts to call an ABAP method from a PCRE regular expression. It creates an object-oriented representation of a PCRE regex using the `CL_ABAP_REGEX` class, applying different PCRE syntaxes for callout specifications. The class implements the `IF_ABAP_MATCHER_CALLOUT` interface, and the `callout` method uses a demo class instance as the callout handler. If the regex matches, the method is called for each callout position. The `callout` method populates a string table with accessible details. This demonstrates that regex processing can be influenced, and in the example, processing stops when a condition is met. As a result, `found` is false because the regex is not fully processed. For more details, refer to the class documentation.

To try the example out, create a demo class named `zcl_some_class` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.

```abap
CLASS zcl_some_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_abap_matcher_callout.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA callout TYPE i.
    CLASS-DATA callout_tab TYPE string_table.
ENDCLASS.



CLASS zcl_some_class IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    DATA(text_to_search) = `abcdefghijklmnopq`.
    DATA(regex) = cl_abap_regex=>create_pcre( pattern = `(...)(?C1)(..)(?C2)(....)(?C3)(.)(?C"D")(....)(?C"E")(...)(?C"F")` ).
    DATA(matcher) = regex->create_matcher( text = text_to_search ).
    DATA(handler) = NEW zcl_some_class( ).
    matcher->set_callout( handler ).
    DATA(found) = matcher->match( ).

    IF found = abap_false.
      out->write( |Pattern not found.\n\n| ).
    ENDIF.

    out->write( callout_tab ).

  ENDMETHOD.

  METHOD if_abap_matcher_callout~callout.
    DATA(ts) = utclong_current( ).
    callout += 1.

    IF callout_string = `F`.
      callout_result = if_abap_matcher_callout=>c_callout_result-abort.
    ELSE.
      callout_result = if_abap_matcher_callout=>c_callout_result-pass.

      APPEND INITIAL LINE TO callout_tab.
      APPEND |----------------- Callout { callout } -----------------| TO callout_tab.
      APPEND INITIAL LINE TO callout_tab.
      APPEND |callout_num: { callout_num }| TO callout_tab.
      APPEND |callout_string: { callout_string }| TO callout_tab.
      APPEND |regex: { regex  }| TO callout_tab.
      APPEND |subject: { subject }| TO callout_tab.
      APPEND |current_subject_pos: { current_subject_pos }| TO callout_tab.
      APPEND |current_pattern_pos: { current_pattern_pos }| TO callout_tab.
      APPEND |capture_last: { capture_last }| TO callout_tab.
      APPEND |capture_last_len: { capture_last_len }| TO callout_tab.
      APPEND |capture_last_off: { capture_last_off }| TO callout_tab.
      APPEND |start_match_off: { start_match_off }| TO callout_tab.

      TRY.
          APPEND |Content of submatch: { subject+capture_last_off(capture_last_len) }| TO callout_tab.
        CATCH cx_sy_range_out_of_bounds.
      ENDTRY.
      APPEND |This callout was called at { ts }| TO callout_tab.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Conditional Patterns

- Used to check whether certain capture groups are matched
- When matched or not matched, you can specify replacement patterns; you can also specify replacement patterns with conditions
- Relevant PCRE syntax for searches: `(?(condition)yes-pattern)` or `(?(condition)yes-pattern|no-pattern)`
- The `condition` can, for example, be the number or name of a capturing group.
- See the code snippet for replacement patterns.

```abap
DATA string_cond_pattern TYPE string.

"------- Conditions in the search pattern -------

"Example pattern:
"- The first capturing group matches a digit.
"- The '?' character specifies that the capturing group participates
"  in the match optionally.
"- A condition is specified, introduced by '(?...' and referring to
"  the first capturing group (the digit) indicated by 1.
"- If the condition is met, i.e. a digit is found, then the yes pattern
"  is applied ('.pdf'), which participates in the match.
"- If the condition is not met, i.e. a digit is not found, then the no
"  pattern is applied ('.md'), which participates in the match.
"- As a result, '2.pdf' and the two '.md' occurrences (also the one
"  without the digit) are replaced.

"1.txt,a.txt,#,b.pdf,3.gif,c.gif,4#,d#,5.jpg,e.jpg
string_cond_pattern = replace( val = `1.txt,a.txt,2.pdf,b.pdf,3.gif,c.gif,4.md,d.md,5.jpg,e.jpg`
                                pcre = `(\d)?(?(1)\.pdf|\.md)` with = `#` occ = 0 ).

"------- Conditions in the replacement pattern -------

"Search pattern:
"- The '?' character specifies that the capturing group participates in the match optionally.
"Replacements pattern:
"- '$' followed by the specification of the capturing group that is referred to in curly brackets
"- The capturing group number is followed by a ':' and '+...' that denotes the value if true,
"  the value following the other colon denotes the value if false

"This is very good.
string_cond_pattern = replace( val = `correct`   pcre = `(in)?correct` with = `This is ${1:+not:very} good.` ).

"This is not good.
string_cond_pattern = replace( val = `incorrect` pcre = `(in)?correct` with = `This is ${1:+not:very} good.` ).

"Syntax {n:-default}
"Shortcut for the PCRE replacement pattern by specifying
"a default if the capture group does not participate in the match

"abcd
"'ab' participates in the match, so the default replacement pattern is not
"applied
string_cond_pattern = replace( val = `abcd` pcre = `(ab)?cd` with = `${1:-yz}cd` ).

"yzcd
"'ab' does not participate in the match, so the default replacement pattern is
"applied
string_cond_pattern = replace( val = `cd` pcre = `(ab)?cd` with = `${1:-yz}cd` ).
```

## ABAP Statements Using Regular Expressions

The following example demonstrates a selection of syntax options with `FIND` and `REPLACE`. There are multiple options as there is a rich variety of additions. For more details, refer to the [String Processing](07_String_Processing.md) cheat sheet.

```abap
"-----------------------------------------------------------------------------
"------------------------------- FIND ----------------------------------------
"-----------------------------------------------------------------------------

DATA(str) = `Cathy's black cat on the mat played with Matt.`.

"----- ALL OCCURRENCES addition: Finding all occurrences -----
"The MATCH COUNT addition stores the number of occurrences in a data object.

"Determining the number of letters in a string
"a: 36
FIND ALL OCCURRENCES OF PCRE `[A-Za-z]` IN str MATCH COUNT DATA(a).

"----- RESULTS addition: Finding all occurrences -----
"The example also uses the ALL OCCURRENCES addition. The findings are
"stored in a table of type match_result_tab.
FIND ALL OCCURRENCES OF PCRE `\s` IN str RESULTS DATA(b).

*LINE    OFFSET    LENGTH    SUBMATCHES
*0       7         1         OFFSET     LENGTH
*
*0       13        1         OFFSET     LENGTH
*
*0       17        1         OFFSET     LENGTH
*
*0       20        1         OFFSET     LENGTH
*
*0       24        1         OFFSET     LENGTH
*
*0       28        1         OFFSET     LENGTH
*
*0       35        1         OFFSET     LENGTH
*
*0       40        1         OFFSET     LENGTH

"----- SUBMATCHES addition: Storing capturing group content in variables -----
"Pattern: anything before and after ' on '
FIND PCRE `(.*)\son\s(.*)` IN str IGNORING CASE SUBMATCHES DATA(c) DATA(d).
"a: 'Cathy's black cat' / b: 'the mat played with Matt.'.

"Using the RESULTS addition to get offset and length information of
"all capturing groups, among others
FIND PCRE `(.*)\son\s(.*)` IN str IGNORING CASE RESULTS DATA(e).

*LINE    OFFSET    LENGTH    SUBMATCHES
*0       0         46        OFFSET    LENGTH
*                            0         17
*                            21        25

"----- MATCH OFFSET/LENGTH additions -----
"The following examples find the last occurrence of a comma and the following content.
"\K denotes that the comma is excluded from the result. Using the offset and length
"values, the part of the string is extracted. The examples underscore that you can
"achieve the same thing differently with different PCRE syntax patterns (which also
"applies to ABAP demonstrated with the use of the substring function and specifying
"the syntax string+off(len) to extract the substring).

str = `abc,def,ghi,jkl,mno,pqr,stu,vwx,yz`.

"Negative lookahead
FIND PCRE `,(?!.*,)\K.*` IN str MATCH OFFSET DATA(off_a) MATCH LENGTH DATA(len_a).
"yz
DATA(content_a) = substring( val = str off = off_a len = len_a ).

"Positive lookahead
FIND PCRE `,(?=[^,]*$)\K.*` IN str MATCH OFFSET DATA(off_b) MATCH LENGTH DATA(len_b).
DATA(content_b) = str+off_b(len_b).

ASSERT content_a = `yz`.
ASSERT content_b = content_a.

"----- IN TABLE addition: Searching in internal tables  -----
"The internal table must have a character-like line type.

"Searching in an internal table and retrieving line, offset, length information
"The example only searches for the first occurrence. See another example below
"that uses the ALL OCCURRENCES and RESULTS additions.
DATA(itab) = VALUE string_table( ( `Cathy's black cat on the mat played with the friend of Matt.` ) ).
"Pattern: 't' at the beginning of a word followed by another character
"d: 1, e: 21, f: 2
FIND FIRST OCCURRENCE OF PCRE `\bt.` IN TABLE itab
  IGNORING CASE MATCH LINE DATA(f) MATCH OFFSET DATA(g) MATCH LENGTH DATA(h).

"----- Searching in internal tables  -----
"The RESULTS addition stores findings in an internal table of type match_result_tab when
"ALL OCCURRENCES is used.
"Submatches (i.e. length and offset values of the submatches) are stored in internal
"tables themselves. Therefore, the example uses nested loops and the substring function
"to retrieve the strings.

"The objective of the following example is to extract the content of the segments that
"are positioned within /.../ in a URL. The segments are stored in an internal table.
DATA(url) = `https://help.sap.com/docs/abap-cloud/abap-concepts/controlled-sap-luw/`.
DATA url_parts TYPE string_table.

FIND ALL OCCURRENCES OF PCRE `(?<=\/)([^\/]+)(?=\/)` IN url RESULTS DATA(res).

"Details on the regular expression:
"- Positive lookbehind (?<=\/) that determines that the content is preceded by `/`
"- Positive lookahead (?=\/) that determines that the content is followed by `/
"- ([^\/]+) in between determines that any sequence of characters that are not `/` are matched
"- The match is put in parentheses to store the submatch

LOOP AT res INTO DATA(finding).
  LOOP AT finding-submatches INTO DATA(sub).
    DATA(url_part) = substring( val = url off = sub-offset len = sub-length ).
    APPEND url_part TO url_parts.
  ENDLOOP.
ENDLOOP.

"The following statement uses nested iteration expressions with FOR instead of nested
"LOOP statements.
DATA(url_parts_for_loop) = VALUE string_table( FOR wa1 IN res
                                               FOR wa2 IN wa1-submatches
                                               ( substring( val = url off = wa2-offset len = wa2-length ) ) ).

ASSERT url_parts = url_parts_for_loop.
*Content:
*help.sap.com
*docs
*abap-cloud
*abap-concepts
*controlled-sap-luw

"-----------------------------------------------------------------------------
"------------------------------- REPLACE -------------------------------------
"-----------------------------------------------------------------------------

DATA(str_replace) = `ab apppc app`.
DATA(str_replace_copy) = str_replace.

"Changing the source field directly with a REPLACE statement; same as above
"pc app#ab a
REPLACE PCRE `(.*?)PP(.*)` IN str_replace WITH `$2#$1` IGNORING CASE.

str_replace = str_replace_copy.

"ALL OCCURRENCES addition
"ab#apppc#app
REPLACE ALL OCCURRENCES OF PCRE `\s` IN str_replace WITH `#`.

str_replace = str_replace_copy.

"ab a## a#
REPLACE ALL OCCURRENCES OF PCRE `p.` IN str_replace WITH `#`
  REPLACEMENT COUNT DATA(repl_cnt) "3
  RESULTS DATA(repl_res).
"repl_res:
"LINE    OFFSET    LENGTH
"0       4         1
"0       5         1
"0       8         1

DATA(str_table_original) = VALUE string_table( ( `a1bc2` ) ( `d3ef` ) ( `4ghi` ) ( `jkl` ) ).
DATA(str_table) = str_table_original.

"Replacing all occurrences in a table
"RESULTS addition: Storing information in an internal table of type repl_result_tab
REPLACE ALL OCCURRENCES OF PCRE `\d`
  IN TABLE str_table
  WITH `#`
  RESULTS DATA(res_table).

"str_table: a#bc# / d#ef / #ghi / jkl
"res_table:
"LINE    OFFSET    LENGTH
"1       1         1
"1       4         1
"2       1         1
"3       0         1

str_table = str_table_original.

"Replacing the first occurrence in a table
"RESULTS addition: Storing information in a structure of type repl_result
REPLACE FIRST OCCURRENCE OF PCRE `\d`
  IN TABLE str_table
  WITH `#`
  RESULTS DATA(res_structure).

"str_table: a#bc2 / d3ef / 4ghi / jkl
"res_structure:
"LINE  OFFSET  LENGTH
"1     1       1

str_table = str_table_original.

"Restricting the search range in an internal table
REPLACE ALL OCCURRENCES OF PCRE `\d`
  IN TABLE str_table
  FROM 1 TO 2
  WITH `#`.

"str_table: a#bc# / d#ef / 4ghi / jkl

str_table = str_table_original.

"Offsets can be optionally specified (also only the offset of start or end line possible)
REPLACE ALL OCCURRENCES OF PCRE `\d`
  IN TABLE str_table
  FROM 1 OFFSET 3 TO 2 OFFSET 2
  WITH `#`.

"str_table: a1bc# / d#ef / 4ghi / jkl
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Built-In Functions in ABAP Using Regular Expressions

Examples:


```abap
DATA(text) = `Pieces of cakes.`.

"---------------- find ----------------
"The find function searches for the subtexting specified and returns the offset

"15
DATA(find) = find( val = text pcre = `\.` ).

"---------------- find_end ----------------
"find_end returns the sum of the offset of the occurrence plus the length of the match

"7 (6 + 1)
DATA(find_end) = find_end( val = text pcre = `\s` ).

"---------------- count ----------------

"2
DATA(count_a)  = count( val = text pcre = `\s` ).

"16
DATA(count_b)  = count( val = text pcre = `.` ).

"---------------- match ----------------

"jon.doe@email.com
DATA(match_a) = match( val = `The email address is jon.doe@email.com.`
                        pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ).

"Find blank (without inlcuding it in the result indicated by \K) and
"the following 2 characters, second occurrence
"ad
DATA(match_b) = match( val = `The email address is jon.doe@email.com.`
                        pcre = `\s\K..`
                        occ = 2 ).

"---------------- replace ----------------
"Pieces#of cakes.
DATA(replace_a) = replace( val = text pcre = `\s` with = `#` ).

"Pieces of#cakes.
DATA(replace_b) = replace( val = text pcre = `\s` occ = 2 with = `#` ).

"---------------- substring_* ----------------

text = `Lorem ipsum dolor sit amet`.

"Extracting a substring ...
"... after a matching regular expression

"dolor sit amet
DATA(substring_after) = substring_after( val = text pcre = `\s` occ = 2 ).


"... before a matching regular expression
"Lorem ipsum
DATA(substring_before) = substring_before( val = text pcre = `\s` occ = 2 ).

"... from a matching regular expression on including the match
"' dolor sit amet'
DATA(substring_from) = substring_from( val = text pcre = `\s` occ = 2  ).

"... up to a matching regular expression including the match
"'Lorem ipsum '
DATA(substring_to) = substring_to( val = text pcre = `\s` occ = 2 ).

"---------------- Predicate functions: contains, matches ----------------
"The built-in functions 'contains' returns a truth value. In the following
"examples, the truth value is demonstrated using the xsdbool function.

"X
DATA(contains) = xsdbool( contains( val = `abc def` pcre = `\s`  ) ).

"The built-in functions 'matches' compares a search range of the textual argument
"with a regular expression.

"X
DATA(matches) = xsdbool( matches( val  = `jon.doe@email.com`
                                  pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Built-In Functions in ABAP SQL and CDS Using Regular Expressions

The following example demonstrates [built-in functions using regular expressions in ABAP SQL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENSQL_FUNCTIONS_STRING.html). 
For more information on the built-in functions for ABAP CDS, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_builtin_functions.htm). The example assumes that the ABAP cheat sheet repository has been imported into the system as it uses some of its repository objects.

```abap
"Populating demo database tables
zcl_demo_abap_aux=>fill_dbtabs( ).

SELECT SINGLE
  carrid,    "LH
  carrname,  "Lufthansa
  url,       "http://www.lufthansa.com

  "Checks if expression contains a PCRE expression;
  "case-sensitive by default (case_sensitive parameter can be specified)
  "Notes on the 1 = found, 0 = not found
  "1
  like_regexpr( pcre  = '\..',         "Period that is followed by any character
                value = url ) AS like_regex,

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

  "Counts all occurrences of found PCRE patterns
  "2
  occurrences_regexpr( pcre = '\..',    "Period that is followed by any character
                        value = url ) AS occ_regex,

  "Replaces a found PCRE expression;
  "more parameters possible: occurrence, case_sensitive, start
  "http://www#ufthansa#om
  replace_regexpr( pcre = '\..',        "Period that is followed by any character
                    value = url,
                    with = '#' ) AS replace_regex,

  "Searches for a PCRE expression and returns the matched substring
  "More parameters possible: occurrence, case_sensitive, start, group
  ".lu
  substring_regexpr( pcre = '\...', "Period that is followed by any two characters
                      value = url ) AS substring_regexpr

  FROM zdemo_abap_carr
  WHERE carrid = 'LH'
  INTO @DATA(builtin_func_regex).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## System Classes for Regular Expressions: CL_ABAP_REGEX and CL_ABAP_MATCHER

- `CL_ABAP_REGEX`
  - Creates an object-oriented representation of regular expressions.
  - The `create_pcre` method creates instances of regular expressions with PCRE syntax.
  - Instances of the class can be used with the ...
    - `CL_ABAP_MATCHER` class to create a matcher, which applies regular expressions or 
    - `REGEX` addition (not `PCRE`) in `FIND` and `REPLACE` statements.
  - A variety of methods and parameters can be specified to accomplish various things and to further specify the handling of the regular expression.

- `CL_ABAP_MATCHER`
  - Applies regular expressions to strings or internal tables.
  - Offers methods to search, replace, or match regular expressions.
  - Instances of the class can be created with ...
    - methods of the class itself or
    - the `create_matcher` method of the `CL_ABAP_REGEX` class.  
  - You get access to the processing state of the regular expression using `get...` methods.
  - Supports callouts as outlined in [this section](#callouts), requiring the `IF_ABAP_MATCHER_CALLOUT` interface implementation.
  - For reusing regular expressions, use `CL_ABAP_MATCHER` instances created with `CL_ABAP_REGEX`.

- Find more information in the class documentation (press F2 on the class in ADT).


Examples:


``` abap
"Example string
DATA(str) = `a1 # B2 ? cd . E3`.

"----------- Creating an instance of a regular expression -----------

"Creating an instance of a regular expression with PCRE syntax
"using cl_abap_regex

"Example pattern: Any-non digit followed by a digit
DATA(regex) = cl_abap_regex=>create_pcre( pattern = `\D\d`
                                          ignore_case = abap_true ).

"----------- Creating matchers -----------

"Two ways are possible (both ways return references of type
"ref to cl_abap_matcher):
"- create_matcher method of the cl_abap_regex class
"- create_pcre method of the cl_abap_matcher class
"Note that several importing parameters are available to enable
"further settings of the regular expression, e.g. ignoring the
"case, using the extended mode, etc. The examples pass a string
"to the 'text' parameter. You can also specify internal tables
"with the 'table' parameter and more.

"Creating a matcher using the create_matcher method of the cl_abap_regex class
DATA(matcher_1) = regex->create_matcher( text = str ).
"Creating a matcher in one go using method chaining
DATA(matcher_2) = cl_abap_regex=>create_pcre( pattern = `\D\d`
                                              ignore_case = abap_true
                                            )->create_matcher( text = str ).

"Creating a matcher using the create_pcre method of the cl_abap_matcher class
DATA(matcher_3) = cl_abap_matcher=>create_pcre( pattern = `\D\d`
                                                text    = str
                                                ignore_case = abap_true ).

"----------- Exploring searching and replacing -----------

"--- Finding all occurrences using the find_all method ---
"In the example, result has the type match_result_tab containing the findings.
DATA(result_fa1) = matcher_1->find_all( ).

*LINE    OFFSET    LENGTH    SUBMATCHES
*0       0         2         OFFSET     LENGTH
*
*0       5         2         OFFSET     LENGTH
*
*0       15        2         OFFSET     LENGTH

DATA(result_fa2) = matcher_2->find_all( ).
ASSERT result_fa2 = result_fa1.

"Getting the result in one go using method chaining with cl_abap_matcher
DATA(result_fa3) = cl_abap_matcher=>create_pcre( pattern = `\D\d`
                                                text    = str
                                                ignore_case = abap_true
                                              )->find_all( ).
ASSERT result_fa3 = result_fa1.

"--- Example with submatches ---

str = `XabcdXXefgXXhXXijklmnXX`.

DATA(result_fa4) = cl_abap_matcher=>create_pcre( pattern = `X(.*?)X`
                                                 text    = str
                                                 ignore_case = abap_true
                                               )->find_all( ).

*LINE    OFFSET    LENGTH    SUBMATCHES
*0       0         6         OFFSET    LENGTH
*                            1         4
*0       6         5         OFFSET    LENGTH
*                            7         3
*0       11        3         OFFSET    LENGTH
*                            12        1
*0       14        8         OFFSET    LENGTH
*                            15        6

"--- Replacing all occurrences using the 'replace_all' method ---

DATA(matcher_repl_1) = cl_abap_regex=>create_pcre( pattern = `X(.*?)X`
                                                 )->create_matcher( text = str ).

"4
DATA(repl_count_1) = matcher_repl_1->replace_all( newtext = `#$1#` ).

"#abcd##efg##h##ijklmn#X
DATA(repl_result_1) = matcher_repl_1->text.

"Using cl_abap_matcher
DATA(matcher_repl_2) = cl_abap_matcher=>create_pcre( pattern = `X(.*?)X`
                                                     text    = str ).
DATA(repl_count_2) = matcher_repl_2->replace_all( newtext = `#$1#` ).
DATA(repl_result_2) = matcher_repl_2->text.

"---- Sequential processing of the regular expression ---
"---- using the find_next method ------------------------
"The example explores various other methods, and writes
"information to a string table.

str = `a1bc2def3ghij45klm67opqr8stuvwx90yz`.

DATA(matcher_fn) = cl_abap_matcher=>create_pcre( pattern = `\d(\D.)`
                                                 text    = str ).

DATA strtab TYPE string_table.
WHILE matcher_fn->find_next( ) = abap_true.
  APPEND |---- Finding { sy-index } -----| TO strtab.

  "Type match_result
  DATA(match_result) = matcher_fn->get_match( ).

  DATA(offset) = matcher_fn->get_offset( ).
  DATA(length) = matcher_fn->get_length( ).
  DATA(matched_content) = str+offset(length).

  APPEND |Match offset: { offset }| TO strtab.
  APPEND |Match length: { length }| TO strtab.
  APPEND |Match content: { matched_content }| TO strtab.

  "Type match_result
  DATA(subgroup) = matcher_fn->get_match( )-submatches.

  LOOP AT subgroup INTO DATA(wa).
    DATA(sub_tabix) = sy-tabix.
    DATA(submatch_line) = wa.
    DATA(submatch_offset) = wa-offset.
    DATA(submatch_length) = wa-length.
    DATA(submatch) = matcher_fn->get_submatch( sub_tabix ).
    APPEND |Submatch { sub_tabix } offset: { submatch_offset }| TO strtab.
    APPEND |Submatch { sub_tabix } length: { submatch_length }| TO strtab.
    APPEND |Submatch { sub_tabix } content: { submatch }| TO strtab.
  ENDLOOP.

ENDWHILE.

"---- Using an object of type cl_abap_regex in ABAP ---
"---- statements with the REGEX addition --------------

DATA(result_find_all_1) = cl_abap_matcher=>create_pcre( pattern = `\d(\D.)`
                                                        text = str
                                                      )->find_all( ).
DATA(result_find_all_2) = cl_abap_regex=>create_pcre( pattern = `\d(\D.)`
                                           )->create_matcher( text = str
                                           )->find_all( ).

DATA(reg_expr) = cl_abap_regex=>create_pcre( pattern = `\d(\D.)` ).

FIND ALL OCCURRENCES OF REGEX reg_expr IN str RESULTS DATA(result_find_all_3).

*LINE    OFFSET    LENGTH    SUBMATCHES
*0       1         3         OFFSET    LENGTH
*                            2         2
*0       4         3         OFFSET    LENGTH
*                            5         2
*0       8         3         OFFSET    LENGTH
*                            9         2
*0       14        3         OFFSET    LENGTH
*                            15        2
*0       19        3         OFFSET    LENGTH
*                            20        2
*0       24        3         OFFSET    LENGTH
*                            25        2
*0       32        3         OFFSET    LENGTH
*                            33        2

ASSERT result_find_all_3 = result_find_all_1.
ASSERT result_find_all_3 = result_find_all_2.

"Note that the REGEX addition is obsolete when using (POSIX) syntax patterns
"A syntax warning is displayed for the following example.
"FIND ALL OCCURRENCES OF REGEX `\d(\D.)` IN str RESULTS DATA(result_8).

"The syntax warning can be suppressed using a pragma
FIND ALL OCCURRENCES OF REGEX `\d(\D.)` IN str RESULTS DATA(result_find_all_4) ##REGEX_POSIX.

"Using PCRE instead
FIND ALL OCCURRENCES OF PCRE `\d(\D.)` IN str RESULTS DATA(result_find_all_5).
ASSERT result_find_all_5 = result_find_all_3.

"---------------- Exploring more parameters of the create_pcre method ----------------
"See the class documentation for more parameters and information.

"--- enable_multiline parameter ---

str = |abc\ndef\nghi\njkl|.

DATA(matcher_no_ml) = cl_abap_matcher=>create_pcre( pattern = `^`
                                                    text    = str ).
"1
DATA(repl_count_no_ml) = matcher_no_ml->replace_all( newtext = `#` ).
"|#abc\ndef\nghi\njkl|
DATA(repl_result_no_ml) = matcher_no_ml->text.

DATA(matcher_w_ml) = cl_abap_matcher=>create_pcre( pattern = `^`
                                                   text    = str
                                                   enable_multiline = abap_true ).
"4
DATA(repl_count_w_ml) = matcher_w_ml->replace_all( newtext = `#` ).
"|#abc\n#def\n#ghi\n#jkl|
DATA(repl_result_w_ml) = matcher_w_ml->text.

"--- table/ignore_case parameters ---

data(str_table) = VALUE string_table( ( `abZdez` ) ( `zZfghZ` ) ( `ijkZZz` ) ( `zzzzZ` ) ).

DATA(matcher_tab) = cl_abap_matcher=>create_pcre( pattern = `z+`
                                                  table   = str_table
                                                  ignore_case = abap_true ).
"6
DATA(repl_count_tab) = matcher_tab->replace_all( newtext = `#` ).
"ab#de# / #fgh# / ijk# / #
DATA(repl_result_tab) = matcher_tab->table.

"--- extended parameter ---

str = `abc def`.

DATA(matcher_w_extended) = cl_abap_matcher=>create_pcre( pattern = `abc def`
                                                         text    = str ).

"No replacement in the following example as the extended mode is
"enabled by default.
"0
DATA(repl_count_w_extended) = matcher_w_extended->replace_all( newtext = `#` ).
"abc def
DATA(repl_result_w_extended) = matcher_w_extended->text.

"Disabling the extended mode so that whitespaces are not ignored
DATA(matcher_not_extended) = cl_abap_matcher=>create_pcre( pattern = `abc def`
                                                           text    = str
                                                           extended = abap_false ).

"1
DATA(repl_count_not_extended) = matcher_not_extended->replace_all( newtext = `#` ).
"#
DATA(repl_result_not_extended) = matcher_not_extended->text.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

[zcl_demo_abap_regex](./src/zcl_demo_abap_regex.clas.abap)

> **💡 Note**<br>
> - The [executable example](./src/zcl_demo_abap_string_proc.clas.abap) of the [String Processing](07_String_Processing.md) cheat sheet also includes examples with regular expressions.
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)