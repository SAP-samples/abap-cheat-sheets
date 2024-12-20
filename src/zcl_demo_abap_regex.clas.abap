"! <p class="shorttext"><strong>Regular Expressions in ABAP</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates regular expressions in ABAP.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Information</h2>
"! <p>Find information on getting started with the example class and the disclaimer in
"! the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
CLASS zcl_demo_abap_regex DEFINITION
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

CLASS zcl_demo_abap_regex IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP cheat sheet example: Regular Expressions in ABAP\n\n| ).
    out->write( `1) Characters and Character Types` ).

    DATA string_chars_types TYPE string.

    "Specific character
    string_chars_types = replace( val = `abcdef` pcre = `a` with = `#` occ = 0 ).

    out->write( string_chars_types ).

    "Any character except a line break
    string_chars_types = replace( val = |ab 1#?C\n D23| pcre = `.` with = `_` occ = 0 ).
    out->write( string_chars_types ).

    "Any digit
    string_chars_types = replace( val = `a1-b2 3-4c9` pcre = `\d` with = `#` occ = 0 ).
    out->write( string_chars_types ).

    "Any non-digit
    string_chars_types = replace( val = `a1-b2 3-4c9` pcre = `\D` with = `#` occ = 0 ).
    out->write( string_chars_types ).

    "Any whitespace character such as a blank, tab and new line
    string_chars_types = replace( val = |ab cd\tef\ngh| pcre = `\s` with = `#` occ = 0 ).
    out->write( string_chars_types ).

    "Any character that is not a
    string_chars_types = replace( val = |ab cd\tef\ngh| pcre = `\S` with = `#` occ = 0 ).
    out->write( string_chars_types ).

    "Any word character (letter, digit or the underscore)
    string_chars_types = replace( val = `(ab 12_c)` pcre = `\w` with = `#` occ = 0 ).
    out->write( string_chars_types ).

    "Any character that is not a word character
    string_chars_types = replace( val = `(ab 12_c)` pcre = `\W` with = `#` occ = 0 ).
    out->write( string_chars_types ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Escaped Characters and Special Variants` ) ).

    DATA string_esc_chars TYPE string.

    "Special characters
    string_esc_chars = replace( val = `a[b]c\d/e^f.g` pcre = `\[|\]|\\|\/|\^|\.` with = `#` occ = 0 ).
    out->write( string_esc_chars ).

    "Line feeds
    string_esc_chars = replace( val = |a\nb\nc| pcre = `\n` with = `#` occ = 0 ).
    out->write( string_esc_chars ).

    "Line feed negation
    string_esc_chars = replace( val = |a\nb\nc| pcre = `\N` with = `#` occ = 0 ).
    out->write( string_esc_chars ).

    "Tabs
    string_esc_chars = replace( val = |a\tb\tc| pcre = `\t` with = `#` occ = 0 ).
    out->write( string_esc_chars ).

    "Carriage return
    string_esc_chars = replace( val = |d\re\rf| pcre = `\r` with = `#` occ = 0 ).
    out->write( string_esc_chars ).

    "Characters with hex code
    "The example string includes a non-breaking space.
    string_esc_chars = replace(
      val = |#{ cl_abap_conv_codepage=>create_in( codepage = `UTF-16BE` )->convert( source = CONV xstring( `00A0` ) ) }#|
      pcre = `\x{00A0}` with = `x` occ = 0 ).
    out->write( string_esc_chars ).

    "Characters with Unicode code point
    "As above, the example string includes a non-breaking space.
    "The PCRE syntax uses the control verb (*UTF) to enable UTF mode.
    string_esc_chars = replace(
      val = |#{ cl_abap_conv_codepage=>create_in( codepage = `UTF-16BE` )->convert( source = CONV xstring( `00A0` ) ) }#|
      pcre = `(*UTF)\N{U+00A0}` with = `y` occ = 0 ).
    out->write( string_esc_chars ).

    "Characters with a specified Unicode character property
    string_esc_chars = replace( val = `Hello ABAP` pcre = `\p{Ll}+` with = `#` occ = 0 ).
    out->write( string_esc_chars ).

    string_esc_chars = replace( val = `Hello ABAP` pcre = `\p{Lu}+` with = `#` occ = 0 ).
    out->write( string_esc_chars ).

    string_esc_chars = replace( val = `Hello ABAP` pcre = `\P{Ll}+` with = `#` occ = 0 ).
    out->write( string_esc_chars ).

    string_esc_chars = replace( val = `Hello ABAP` pcre = `\P{Lu}+` with = `#` occ = 0 ).
    out->write( string_esc_chars ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Quantifiers, Repetitions and Alternatives` ) ).

    DATA string_quan_rep_alt TYPE string.

    "Zero or more repetitions
    string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab*` with = `#` occ = 0 ).
    out->write( string_quan_rep_alt ).

    "One or more repetitions
    string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab+` with = `#` occ = 0 ).
    out->write( string_quan_rep_alt ).

    "Between x and y repetitions
    string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab{2,3}` with = `#` occ = 0 ).
    out->write( string_quan_rep_alt ).

    "Exactly n repetitions
    string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab{3}` with = `#` occ = 0 ).
    out->write( string_quan_rep_alt ).

    "Exactly n or more repetitions
    string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab{2,}` with = `#` occ = 0 ).
    out->write( string_quan_rep_alt ).

    "Optional matches
    string_quan_rep_alt = replace( val = `abc abbc abbbc a ac` pcre = `ab?` with = `#` occ = 0 ).
    out->write( string_quan_rep_alt ).

    "Alternative matches
    string_quan_rep_alt = replace( val = `azbycxdwevfu` pcre = `a|b|c` with = `#` occ = 0 ).
    out->write( string_quan_rep_alt ).

    "Capturing non-greedily (zero or more repetitions)
    string_quan_rep_alt = replace( val = `abcd abccccd ab` pcre = `bc*?` with = `#` occ = 0 ).
    out->write( string_quan_rep_alt ).

    string_quan_rep_alt = replace( val = `abcxdefghxi` pcre = `\A.*?x` with = `_` ).
    out->write( string_quan_rep_alt ).

    "Example to compare with greedy capturing
    string_quan_rep_alt = replace( val = `abcxdefghxi` pcre = `\A.*x` with = `_` ).
    out->write( string_quan_rep_alt ).

    "Capturing non-greedily (one or more repetitions)
    string_quan_rep_alt = replace( val = `abcd abccccd ab` pcre = `bc+?` with = `#` occ = 0 ).
    out->write( string_quan_rep_alt ).

    "Example to compare with greedy capturing
    string_quan_rep_alt = replace( val = `abcd abccccd ab` pcre = `bc+` with = `#` occ = 0 ).
    out->write( string_quan_rep_alt ).

    string_quan_rep_alt = replace( val = `<span>Hallo</span>` pcre = `<.+?>` with = `#` occ = 0 ).
    out->write( string_quan_rep_alt ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Character Sets and Ranges` ) ).

    DATA string_char_sets_ranges TYPE string.

    "Defining a character set
    string_char_sets_ranges = replace( val = `bit bat but bet` pcre = `b[iu]` with = `#` occ = 0 ).
    out->write( string_char_sets_ranges ).

    "Defining a character range
    string_char_sets_ranges = replace( val = `aa1 ab2 ba3 cac4 da56 a7` pcre = `a[a-c0-5]` with = `#` occ = 0 ).
    out->write( string_char_sets_ranges ).

    "Matching any single character not present in the list
    string_char_sets_ranges = replace( val = `ABap` pcre = `[^Ap]` with = `#` occ = 0 ).
    out->write( string_char_sets_ranges ).

    "Matching any single character not within the range
    string_char_sets_ranges = replace( val = `ABCDabcd123456` pcre = `[^A-Ca-c1-4]` with = `#` occ = 0 ).
    out->write( string_char_sets_ranges ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Anchors and Positions` ) ).

    DATA string_anchors_pos TYPE string.

    "Start of subject, syntax \A
    string_anchors_pos = replace( val = `abc def` pcre = `\A` with = `#` occ = 0 ).
    out->write( string_anchors_pos ).

    "Start of subject, syntax ^; find more information below regarding multi-line mode
    string_anchors_pos = replace( val = |abc\ndef\nghi| pcre = `(?m)^` with = `#` occ = 0 ).
    out->write( string_anchors_pos ).

    "The following examples uses ^ without enabling the multi-line mode
    string_anchors_pos = replace( val = |abc\ndef\nghi| pcre = `^` with = `#` occ = 0 ).
    out->write( string_anchors_pos ).

    "End of subject, syntax \Z
    string_anchors_pos = replace( val = `abc def` pcre = `\Z` with = `#` occ = 0 ).
    out->write( string_anchors_pos ).

    "End of subject, syntax $
    "The example uses multi-line mode
    string_anchors_pos = replace( val = |abc\ndef\nghi| pcre = `(?m)$` with = `#` occ = 0 ).
    out->write( string_anchors_pos ).

    "Start or end of word
    string_anchors_pos = replace( val = `abcd a12d ed` pcre = `\ba.` with = `#` occ = 0 ).
    out->write( string_anchors_pos ).

    string_anchors_pos = replace( val = `abcd a12d ed` pcre = `\Dd\b` with = `#` occ = 0 ).
    out->write( string_anchors_pos ).

    string_anchors_pos = replace( val = `abcd a12d ed` pcre = `\b.d\b` with = `#` occ = 0 ).
    out->write( string_anchors_pos ).

    "Not at the start or end of words
    string_anchors_pos = replace( val = `see an elefant` pcre = `\Be\B` with = `#` occ = 0 ).
    out->write( string_anchors_pos ).

    "Resetting the starting point of a match
    string_anchors_pos = replace( val = `abcd` pcre = `a.\Kc` with = `#` occ = 0 ).
    out->write( string_anchors_pos ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) Capturing Groups, Replacements and Backreferences` ) ).

    DATA string_capt_group TYPE string.

    string_capt_group = replace( val = `bit bat but bet` pcre = `b(a|u)t` with = `#` occ = 0 ).
    out->write( string_capt_group ).

    "------- Replacements with capturing groups -------

    string_capt_group = replace( val = `APAB` pcre = `(..)(..)` with = `$2$1` ).
    out->write( string_capt_group ).

    "$0 representing the content of the whole match
    string_capt_group = replace( val = `abcd` pcre = `(..)(..)` with = `#$0#` ).
    out->write( string_capt_group ).

    "Alternative replacement syntax with the curly brackets
    string_capt_group = replace( val = `APAB` pcre = `(..)(..)` with = `${2}${1}` ).
    out->write( string_capt_group ).

    "------- Named capturing groups -------

    "PCRE syntax (?<name>...), replacement syntax $name
    string_capt_group = replace( val = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value
                                  pcre = `(?<yr>\d{4})(?<mo>\d{2})(?<d>\d{2})`
                                  with = `Day: $d, month: $mo, year: $yr` ).

    out->write( string_capt_group ).

    "Alternative PCRE syntax (?'name'...), replacement syntax ${name}
    string_capt_group = replace( val = xco_cp=>sy->date( )->as( xco_cp_time=>format->abap )->value
                                  pcre = `(?'yr'\d{4})(?'mo'\d{2})(?'d'\d{2})`
                                  with = `${d}.${mo}.${yr}` ).
    out->write( string_capt_group ).

    "Creating a group but not capturing it
    string_capt_group = replace( val = `abcd` pcre = `(?:..)(..)` with = `#$1#` ).
    out->write( string_capt_group ).

    "The following example raises an exception. $2 cannot be referred to.
    TRY.
        string_capt_group = replace( val = `abcd` pcre = `(?:..)(..)` with = `#$1#$2#` ).
      CATCH cx_sy_invalid_regex_format INTO DATA(error).
        out->write( error->get_text( ) ).
    ENDTRY.

    string_capt_group = replace( val = `abcd` pcre = `(?:..)(..)` with = `#$0#` ).
    out->write( string_capt_group ).

    "------- Back reference -------

    "In the example, the capturing group 1 holds `ab`. The second capturing group captures
    "all word characters until `ab` is found, including `ab`. The reference to the first
    "capturing group is made using \1.
    string_capt_group = replace( val = `abcdefabghij` pcre = `(a.)(\w*)\1` with = `#` ).
    out->write( string_capt_group ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) Lookarounds` ) ).

    DATA string_look_arounds TYPE string.

    "Positive lookahead
    string_look_arounds = replace( val = `abc ade` pcre = `a(?=b)` with = `#` occ = 0 ).
    out->write( string_look_arounds ).

    "Negative lookahead
    string_look_arounds = replace( val = `abc ade` pcre = `a(?!b)` with = `#` occ = 0 ).
    out->write( string_look_arounds ).

    "Positive lookbehind
    string_look_arounds = replace( val = `ab c abcd` pcre = `(?<=\s)c` with = `#` occ = 0 ).
    out->write( string_look_arounds ).

    "Negative lookbehind
    string_look_arounds = replace( val = `ab c abcd` pcre = `(?<!\s)c` with = `#` occ = 0 ).
    out->write( string_look_arounds ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Case Conversions in Replacement Patterns` ) ).

    DATA string_case_conv TYPE string.

    "\u syntax
    string_case_conv = replace( val = `abcdefg` pcre = `c(.*)` with = `c\u$1` ).
    out->write( string_case_conv ).

    "\U syntax
    string_case_conv = replace( val = `abcdefg` pcre = `c(.*)` with = `c\U$1` ).
    out->write( string_case_conv ).

    "\l syntax
    string_case_conv = replace( val = `HIJKLMNO` pcre = `K(.*)` with = `K\l$1` ).
    out->write( string_case_conv ).

    "\L syntax
    string_case_conv = replace( val = `HIJKLMNO` pcre = `K(.*)` with = `K\L$1` ).
    out->write( string_case_conv ).

    "\E syntax
    string_case_conv = replace( val = `abcdefg` pcre = `c(..)(..)` with = `c\U$1\E$2` ).
    out->write( string_case_conv ).

    "The following example is a comparison to similar syntax without specifying \E
    string_case_conv = replace( val = `abcdefg` pcre = `c(..)(..)` with = `c\U$1$2` ).
    out->write( string_case_conv ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `9) Setting Options and Control Verbs` ) ).

    DATA string_opt_set TYPE string.

    "----------- Examples with `.` and single line mode -----------

    "`.` matches new lines feeds when the single line option is set
    string_opt_set = replace( val = |abc\ndef\nghi| pcre = `.` with = `#` occ = 0 ).
    out->write( string_opt_set ).

    "Result (11 replacements including the new line feeds):
    string_opt_set = replace( val = |abc\ndef\nghi| pcre = `(?s).` with = `#` occ = 0 ).
    out->write( string_opt_set ).

    string_opt_set = replace( val = |\n\n| pcre = `(?s).` with = `#` occ = 0 ).
    out->write( string_opt_set ).

    "----------- Examples for the start of subjects -----------

    "Multi-line mode not enabled
    string_opt_set = replace( val = |abc\ndef\nghi| pcre = `^.` with = `#` occ = 0 ).
    out->write( string_opt_set ).

    "Multi-line mode enabled
    string_opt_set = replace( val = |abc\ndef\nghi| pcre = `(?m)^.` with = `#` occ = 0 ).
    out->write( string_opt_set ).

    "\A for comparison
    string_opt_set = replace( val = |abc\ndef\nghi| pcre = `(?m)\A.` with = `#` occ = 0 ).
    out->write( string_opt_set ).

    "----------- Examples for the end of subjects -----------

    string_opt_set = replace( val = |abc\ndef\nghi| pcre = `.$` with = `#` occ = 0 ).
    out->write( string_opt_set ).

    string_opt_set = replace( val = |abc\ndef\nghi| pcre = `(?m).$` with = `#` occ = 0 ).
    out->write( string_opt_set ).

    "\Z syntax (the result is the same as with `.$`)
    string_opt_set = replace( val = |abc\ndef\nghi| pcre = `(?m).\Z` with = `#` occ = 0 ).
    out->write( string_opt_set ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) Extended Mode` ) ).

    DATA some_string TYPE string.

    "Extended mode is enabled by default
    "No replacement in the example.
    some_string = replace( val = `abc def` pcre = `abc def` with = `#` ).
    out->write( some_string ).

    "The following example works because there is no whitespace in the source string.
    "The whitespace in the regular expression is ignored.
    some_string = replace( val = `abcdef` pcre = `abc def` with = `#` ).
    out->write( some_string ).

    "To match whitespace characters, you can use the pattern \s
    some_string = replace( val = `abc def` pcre = `abc\sdef` with = `#` ).
    out->write( some_string ).

    "Escaping a whitespace using \
    some_string = replace( val = `abc def` pcre = `abc\ def` with = `#` ).
    out->write( some_string ).

    "The following example also performs the replacement as the unescaped whitespaces
    "are ignored.
    some_string = replace( val = `abc def` pcre = `abc\         def` with = `#` ).
    out->write( some_string ).

    "Disabling the extended mode so that whitespaces are not ignored
    some_string = replace( val = `abc def` pcre = `(?-x)abc def` with = `#` ).
    out->write( some_string ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) Control verbs` ) ).

    DATA ctrl_verb_string TYPE string.

    "Using (*UTF); the source string contains a non-breakable space that is to be replaced
    ctrl_verb_string = replace(
      val = |#{ cl_abap_conv_codepage=>create_in( codepage = `UTF-16BE` )->convert( source = CONV xstring( `00A0` ) ) }#|
      pcre = `(*UTF)\N{U+00A0}` with = `x` occ = 0 ).
    out->write( ctrl_verb_string ).

    "Line breaks
    "The results are demonstrated using string templates.
    "In the examples, the multi-line mode is enabled in addition.

    ctrl_verb_string = replace( val = |abc\ndef\rghi\r\njkl|
                                pcre = `(*CR)(?m)^` with = `_` occ = 0 ).
    out->write( ctrl_verb_string ).

    ctrl_verb_string = replace( val = |abc\ndef\rghi\r\njkl|
                                pcre = `(*LF)(?m)^` with = `_` occ = 0 ).
    out->write( ctrl_verb_string ).

    ctrl_verb_string = replace( val = |abc\ndef\rghi\r\njkl|
                                pcre = `(*CRLF)(?m)^` with = `_` occ = 0 ).
    out->write( ctrl_verb_string ).

    ctrl_verb_string = replace( val = |abc\ndef\rghi\r\njkl|
                                pcre = `(*ANYCRLF)(?m)^` with = `_` occ = 0 ).
    out->write( ctrl_verb_string ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `12) Callouts` ) ).

    "The following example shows how to use callouts to call an ABAP method from a PCRE
    "regular expression. It creates an object-oriented representation of a PCRE regex using
    "the CL_ABAP_REGEX class, applying different PCRE syntaxes for callout specifications.
    "The class implements the IF_ABAP_MATCHER_CALLOUT interface, and the callout method uses
    "a demo class instance as the callout handler. If the regex matches, the method is called
    "for each callout position. The callout method populates a string table with accessible
    "details. This demonstrates that regex processing can be influenced, and in the example,
    "processing stops when a condition is met. As a result, found is false because the regex
    "is not fully processed. For more details, refer to the class documentation.

    DATA(text_to_search) = `abcdefghijklmnopq`.
    DATA(regex) = cl_abap_regex=>create_pcre( pattern = `(...)(?C1)(..)(?C2)(....)(?C3)(.)(?C"D")(....)(?C"E")(...)(?C"F")` ).
    DATA(matcher) = regex->create_matcher( text = text_to_search ).
    DATA(handler) = NEW zcl_demo_abap_regex( ).
    matcher->set_callout( handler ).
    DATA(found) = matcher->match( ).

    IF found = abap_false.
      out->write( |Pattern not found.\n\n| ).
    ENDIF.

    out->write( callout_tab ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `13) Conditional Patterns` ) ).

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

    string_cond_pattern = replace( val = `1.txt,a.txt,2.pdf,b.pdf,3.gif,c.gif,4.md,d.md,5.jpg,e.jpg`
                                    pcre = `(\d)?(?(1)\.pdf|\.md)` with = `#` occ = 0 ).
    out->write( string_cond_pattern ).

    "------- Conditions in the replacement pattern -------

    "Search pattern:
    "- The '?' character specifies that the capturing group participates in the match optionally.
    "Replacements pattern:
    "- '$' followed by the specification of the capturing group that is referred to in curly brackets
    "- The capturing group number is followed by a ':' and '+...' that denotes the value if true,
    "  the value following the other colon denotes the value if false

    string_cond_pattern = replace( val = `correct`   pcre = `(in)?correct` with = `This is ${1:+not:very} good.` ).
    out->write( string_cond_pattern ).

    string_cond_pattern = replace( val = `incorrect` pcre = `(in)?correct` with = `This is ${1:+not:very} good.` ).
    out->write( string_cond_pattern ).

    "Syntax {n:-default}
    "Shortcut for the PCRE replacement pattern by specifying
    "a default if the capture group does not participate in the match

    "'ab' participates in the match, so the default replacement pattern is not
    "applied
    string_cond_pattern = replace( val = `abcd` pcre = `(ab)?cd` with = `${1:-yz}cd` ).
    out->write( string_cond_pattern ).

    "'ab' does not participate in the match, so the default replacement pattern is
    "applied
    string_cond_pattern = replace( val = `cd` pcre = `(ab)?cd` with = `${1:-yz}cd` ).
    out->write( string_cond_pattern ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `14) ABAP Statements Using Regular Expressions` ) ).

    "-----------------------------------------------------------------------------
    "------------------------------- FIND ----------------------------------------
    "-----------------------------------------------------------------------------

    DATA(str) = `Cathy's black cat on the mat played with Matt.`.

    "----- ALL OCCURRENCES addition: Finding all occurrences -----
    "The MATCH COUNT addition stores the number of occurrences in a data object.

    "Determining the number of letters in a string
    FIND ALL OCCURRENCES OF PCRE `[A-Za-z]` IN str MATCH COUNT DATA(a).
    out->write( a ).

    "----- RESULTS addition: Finding all occurrences -----
    "The example also uses the ALL OCCURRENCES addition. The findings are
    "stored in a table of type match_result_tab.
    FIND ALL OCCURRENCES OF PCRE `\s` IN str RESULTS DATA(b).
    out->write( b ).

    "----- SUBMATCHES addition: Storing capturing group content in variables -----
    "Pattern: anything before and after ' on '
    FIND PCRE `(.*)\son\s(.*)` IN str IGNORING CASE SUBMATCHES DATA(c) DATA(d).
    out->write( c ).
    out->write( d ).

    "Using the RESULTS addition to get offset and length information of
    "all capturing groups, among others
    FIND PCRE `(.*)\son\s(.*)` IN str IGNORING CASE RESULTS DATA(e).
    out->write( e ).

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
    DATA(content_a) = substring( val = str off = off_a len = len_a ).
    out->write( off_a ).
    out->write( len_a ).
    out->write( content_a ).

    "Positive lookahead
    FIND PCRE `,(?=[^,]*$)\K.*` IN str MATCH OFFSET DATA(off_b) MATCH LENGTH DATA(len_b).
    DATA(content_b) = str+off_b(len_b).
    out->write( off_b ).
    out->write( len_b ).
    out->write( content_b ).

    ASSERT content_a = `yz`.
    ASSERT content_b = content_a.

    "----- IN TABLE addition: Searching in internal tables  -----
    "The internal table must have a character-like line type.

    "Searching in an internal table and retrieving line, offset, length information
    "The example only searches for the first occurrence. See another example below
    "that uses the ALL OCCURRENCES and RESULTS additions.
    DATA(itab) = VALUE string_table( ( `Cathy's black cat on the mat played with the friend of Matt.` ) ).

    "Pattern: 't' at the beginning of a word followed by another character
    FIND FIRST OCCURRENCE OF PCRE `\bt.` IN TABLE itab
      IGNORING CASE MATCH LINE DATA(f) MATCH OFFSET DATA(g) MATCH LENGTH DATA(h).
    out->write( f ).
    out->write( g ).
    out->write( h ).

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
    out->write( res ).

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

    out->write( url_parts ).


    "The following statement uses nested iteration expressions with FOR instead of nested
    "LOOP statements.
    DATA(url_parts_for_loop) = VALUE string_table( FOR wa1 IN res
                                                   FOR wa2 IN wa1-submatches
                                                   ( substring( val = url off = wa2-offset len = wa2-length ) ) ).

    ASSERT url_parts = url_parts_for_loop.

    "-----------------------------------------------------------------------------
    "------------------------------- REPLACE -------------------------------------
    "-----------------------------------------------------------------------------

    DATA(str_replace) = `ab apppc app`.
    DATA(str_replace_copy) = str_replace.

    "Changing the source field directly with a REPLACE statement; same as above
    REPLACE PCRE `(.*?)PP(.*)` IN str_replace WITH `$2#$1` IGNORING CASE.
    out->write( str_replace ).

    str_replace = str_replace_copy.

    "ALL OCCURRENCES addition
    REPLACE ALL OCCURRENCES OF PCRE `\s` IN str_replace WITH `#`.
    out->write( str_replace ).

    str_replace = str_replace_copy.

    REPLACE ALL OCCURRENCES OF PCRE `p.` IN str_replace WITH `#`
      REPLACEMENT COUNT DATA(repl_cnt) "3
      RESULTS DATA(repl_res).
    out->write( repl_cnt ).
    out->write( repl_res ).

    DATA(str_table_original) = VALUE string_table( ( `a1bc2` ) ( `d3ef` ) ( `4ghi` ) ( `jkl` ) ).
    DATA(str_table) = str_table_original.

    "Replacing all occurrences in a table
    "RESULTS addition: Storing information in an internal table of type repl_result_tab
    REPLACE ALL OCCURRENCES OF PCRE `\d`
      IN TABLE str_table
      WITH `#`
      RESULTS DATA(res_table).

    out->write( res_table ).

    str_table = str_table_original.

    "Replacing the first occurrence in a table
    "RESULTS addition: Storing information in a structure of type repl_result
    REPLACE FIRST OCCURRENCE OF PCRE `\d`
      IN TABLE str_table
      WITH `#`
      RESULTS DATA(res_structure).

    out->write( res_structure ).

    str_table = str_table_original.

    "Restricting the search range in an internal table
    REPLACE ALL OCCURRENCES OF PCRE `\d`
      IN TABLE str_table
      FROM 1 TO 2
      WITH `#`.

    out->write( str_table ).

    str_table = str_table_original.

    "Offsets can be optionally specified (also only the offset of start or end line possible)
    REPLACE ALL OCCURRENCES OF PCRE `\d`
      IN TABLE str_table
      FROM 1 OFFSET 3 TO 2 OFFSET 2
      WITH `#`.

    out->write( str_table ).


**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `15) Built-In Functions in ABAP Using Regular Expressions` ) ).

    DATA(text) = `Pieces of cakes.`.

    "---------------- find ----------------
    "The find function searches for the subtexting specified and returns the offset

    DATA(find) = find( val = text pcre = `\.` ).
    out->write( find ).

    "---------------- find_end ----------------
    "find_end returns the sum of the offset of the occurrence plus the length of the match

    DATA(find_end) = find_end( val = text pcre = `\s` ).
    out->write( find_end ).

    "---------------- count ----------------

    DATA(count_a)  = count( val = text pcre = `\s` ).
    out->write( count_a ).

    DATA(count_b)  = count( val = text pcre = `.` ).
    out->write( count_b ).

    "---------------- match ----------------

    DATA(match_a) = match( val = `The email address is jon.doe@email.com.`
                            pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ).

    out->write( match_a ).

    "Find blank (without inlcuding it in the result indicated by \K) and
    "the following 2 characters, second occurrence
    DATA(match_b) = match( val = `The email address is jon.doe@email.com.`
                            pcre = `\s\K..`
                            occ = 2 ).

    out->write( match_b ).

    "---------------- replace ----------------

    DATA(replace_a) = replace( val = text pcre = `\s` with = `#` ).

    out->write( replace_a ).

    DATA(replace_b) = replace( val = text pcre = `\s` occ = 2 with = `#` ).

    out->write( replace_b ).

    "---------------- substring_* ----------------

    text = `Lorem ipsum dolor sit amet`.

    "Extracting a substring ...
    "... after a matching regular expression
    DATA(substring_after) = substring_after( val = text pcre = `\s` occ = 2 ).

    out->write( substring_after ).

    "... before a matching regular expression
    DATA(substring_before) = substring_before( val = text pcre = `\s` occ = 2 ).

    out->write( substring_before ).

    "... from a matching regular expression on including the match
    DATA(substring_from) = substring_from( val = text pcre = `\s` occ = 2  ).

    out->write( substring_from ).

    "... up to a matching regular expression including the match
    DATA(substring_to) = substring_to( val = text pcre = `\s` occ = 2 ).

    out->write( substring_to ).

    "---------------- Predicate functions: contains, matches ----------------
    "The built-in functions 'contains' returns a truth value. In the following
    "examples, the truth value is demonstrated using the xsdbool function.

    DATA(contains) = xsdbool( contains( val = `abc def` pcre = `\s`  ) ).
    out->write( contains ).

    "The built-in functions 'matches' compares a search range of the textual argument
    "with a regular expression.

    DATA(matches) = xsdbool( matches( val  = `jon.doe@email.com`
                                      pcre = `\w+(\.\w+)*@(\w+\.)+(\w{2,4})` ) ).

    out->write( matches ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `16) Built-In Functions in ABAP SQL and CDS Using Regular Expressions` ) ).

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

    out->write( builtin_func_regex ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `17) System Classes for Regular Expressions: CL_ABAP_REGEX and CL_ABAP_MATCHER` ) ).
    "Note: The example results are not output except one.

    "Example test_stringing
    DATA(test_string) = `a1 # B2 ? cd . E3`.

    "----------- Creating an instance of a regular expression -----------

    "Creating an instance of a regular expression with PCRE syntax
    "using cl_abap_regex

    "Example pattern: Any-non digit followed by a digit
    DATA(regular_expr) = cl_abap_regex=>create_pcre( pattern = `\D\d`
                                              ignore_case = abap_true ).

    "----------- Creating matchers -----------

    "Two ways are possible (both ways return references of type
    "ref to cl_abap_matcher):
    "- create_matcher method of the cl_abap_regex class
    "- create_pcre method of the cl_abap_matcher class
    "Note that several importing parameters are available to enable
    "further settings of the regular expression, e.g. ignoring the
    "case, using the extended mode, etc. The examples pass a test_stringing
    "to the 'text' parameter. You can also specify internal tables
    "with the 'table' parameter and more.

    "Creating a matcher using the create_matcher method of the cl_abap_regex class
    DATA(matcher_1) = regular_expr->create_matcher( text = test_string ).
    "Creating a matcher in one go using method chaining
    DATA(matcher_2) = cl_abap_regex=>create_pcre( pattern = `\D\d`
                                                  ignore_case = abap_true
                                                )->create_matcher( text = test_string ).

    "Creating a matcher using the create_pcre method of the cl_abap_matcher class
    DATA(matcher_3) = cl_abap_matcher=>create_pcre( pattern = `\D\d`
                                                    text    = test_string
                                                    ignore_case = abap_true ).

    "----------- Exploring searching and replacing -----------

    "--- Finding all occurrences using the find_all method ---
    "In the example, result has the type match_result_tab containing the findings.
    DATA(result_fa1) = matcher_1->find_all( ).

    DATA(result_fa2) = matcher_2->find_all( ).
    ASSERT result_fa2 = result_fa1.

    "Getting the result in one go using method chaining with cl_abap_matcher
    DATA(result_fa3) = cl_abap_matcher=>create_pcre( pattern = `\D\d`
                                                    text    = test_string
                                                    ignore_case = abap_true
                                                  )->find_all( ).
    ASSERT result_fa3 = result_fa1.

    "--- Example with submatches ---

    test_string = `XabcdXXefgXXhXXijklmnXX`.

    DATA(result_fa4) = cl_abap_matcher=>create_pcre( pattern = `X(.*?)X`
                                                     text    = test_string
                                                     ignore_case = abap_true
                                                   )->find_all( ).

    "--- Replacing all occurrences using the 'replace_all' method ---

    DATA(matcher_repl_1) = cl_abap_regex=>create_pcre( pattern = `X(.*?)X`
                                                     )->create_matcher( text = test_string ).

    DATA(repl_count_1) = matcher_repl_1->replace_all( newtext = `#$1#` ).

    DATA(repl_result_1) = matcher_repl_1->text.

    "Using cl_abap_matcher
    DATA(matcher_repl_2) = cl_abap_matcher=>create_pcre( pattern = `X(.*?)X`
                                                         text    = test_string ).
    DATA(repl_count_2) = matcher_repl_2->replace_all( newtext = `#$1#` ).
    DATA(repl_result_2) = matcher_repl_2->text.

    "---- Sequential processing of the regular expression ---
    "---- using the find_next method ------------------------
    "The example explores various other methods, and writes
    "information to a test_stringing table.

    test_string = `a1bc2def3ghij45klm67opqr8stuvwx90yz`.

    DATA(matcher_fn) = cl_abap_matcher=>create_pcre( pattern = `\d(\D.)`
                                                     text    = test_string ).

    DATA test_stringtab TYPE string_table.
    WHILE matcher_fn->find_next( ) = abap_true.
      APPEND |---- Finding { sy-index } -----| TO test_stringtab.

      "Type match_result
      DATA(match_result) = matcher_fn->get_match( ).

      DATA(offset) = matcher_fn->get_offset( ).
      DATA(length) = matcher_fn->get_length( ).
      DATA(matched_content) = test_string+offset(length).

      APPEND |Match offset: { offset }| TO test_stringtab.
      APPEND |Match length: { length }| TO test_stringtab.
      APPEND |Match content: { matched_content }| TO test_stringtab.

      "Type match_result
      DATA(subgroup) = matcher_fn->get_match( )-submatches.

      LOOP AT subgroup INTO DATA(wa).
        DATA(sub_tabix) = sy-tabix.
        DATA(submatch_line) = wa.
        DATA(submatch_offset) = wa-offset.
        DATA(submatch_length) = wa-length.
        DATA(submatch) = matcher_fn->get_submatch( sub_tabix ).
        APPEND |Submatch { sub_tabix } offset: { submatch_offset }| TO test_stringtab.
        APPEND |Submatch { sub_tabix } length: { submatch_length }| TO test_stringtab.
        APPEND |Submatch { sub_tabix } content: { submatch }| TO test_stringtab.
      ENDLOOP.

    ENDWHILE.

    out->write( test_stringtab ).

    "---- Using an object of type cl_abap_regex in ABAP ---
    "---- statements with the REGEX addition --------------

    DATA(result_find_all_1) = cl_abap_matcher=>create_pcre( pattern = `\d(\D.)`
                                                            text = test_string
                                                          )->find_all( ).
    DATA(result_find_all_2) = cl_abap_regex=>create_pcre( pattern = `\d(\D.)`
                                               )->create_matcher( text = test_string
                                               )->find_all( ).

    DATA(reg_expr) = cl_abap_regex=>create_pcre( pattern = `\d(\D.)` ).

    FIND ALL OCCURRENCES OF REGEX reg_expr IN test_string RESULTS DATA(result_find_all_3).

    ASSERT result_find_all_3 = result_find_all_1.
    ASSERT result_find_all_3 = result_find_all_2.

    "Note that the REGEX addition is obsolete when using (POSIX) syntax patterns
    "A syntax warning is displayed for the following example.
    "FIND ALL OCCURRENCES OF REGEX `\d(\D.)` IN test_string RESULTS DATA(result_8).

    "The syntax warning can be suppressed using a pragma
    FIND ALL OCCURRENCES OF REGEX `\d(\D.)` IN test_string RESULTS DATA(result_find_all_4) ##REGEX_POSIX.

    "Using PCRE instead
    FIND ALL OCCURRENCES OF PCRE `\d(\D.)` IN test_string RESULTS DATA(result_find_all_5).
    ASSERT result_find_all_5 = result_find_all_3.

    "---------------- Exploring more parameters of the create_pcre method ----------------
    "See the class documentation for more parameters and information.

    "--- enable_multiline parameter ---

    test_string = |abc\ndef\nghi\njkl|.

    DATA(matcher_no_ml) = cl_abap_matcher=>create_pcre( pattern = `^`
                                                        text    = test_string ).

    DATA(repl_count_no_ml) = matcher_no_ml->replace_all( newtext = `#` ).

    DATA(repl_result_no_ml) = matcher_no_ml->text.

    DATA(matcher_w_ml) = cl_abap_matcher=>create_pcre( pattern = `^`
                                                       text    = test_string
                                                       enable_multiline = abap_true ).

    DATA(repl_count_w_ml) = matcher_w_ml->replace_all( newtext = `#` ).

    DATA(repl_result_w_ml) = matcher_w_ml->text.

    "--- table/ignore_case parameters ---

    DATA(test_string_table) = VALUE string_table( ( `abZdez` ) ( `zZfghZ` ) ( `ijkZZz` ) ( `zzzzZ` ) ).

    DATA(matcher_tab) = cl_abap_matcher=>create_pcre( pattern = `z+`
                                                      table   = test_string_table
                                                      ignore_case = abap_true ).

    DATA(repl_count_tab) = matcher_tab->replace_all( newtext = `#` ).

    DATA(repl_result_tab) = matcher_tab->table.

    "--- extended parameter ---

    test_string = `abc def`.

    DATA(matcher_w_extended) = cl_abap_matcher=>create_pcre( pattern = `abc def`
                                                             text    = test_string ).

    "No replacement in the following example as the extended mode is
    "enabled by default.

    DATA(repl_count_w_extended) = matcher_w_extended->replace_all( newtext = `#` ).

    DATA(repl_result_w_extended) = matcher_w_extended->text.

    "Disabling the extended mode so that whitespaces are not ignored
    DATA(matcher_not_extended) = cl_abap_matcher=>create_pcre( pattern = `abc def`
                                                               text    = test_string
                                                               extended = abap_false ).

    DATA(repl_count_not_extended) = matcher_not_extended->replace_all( newtext = `#` ).

    DATA(repl_result_not_extended) = matcher_not_extended->text.

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
