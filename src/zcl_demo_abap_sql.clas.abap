***********************************************************************
*
*                   ABAP cheat sheet: ABAP SQL
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntax options for working with
*   persisted data in database tables using ABAP SQL.
* - Topics covered: reading from database tables using SELECT, changing
*   data in database tables using INSERT, UPDATE, MODIFY and DELETE
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
"! <p class="shorttext synchronized">ABAP cheat sheet: ABAP SQL</p>
"! Example to demonstrate ABAP SQL.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_sql DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.
    CLASS-METHODS:
      class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS: select_from_dbtab.

    CLASS-DATA:
      struct   TYPE zdemo_abap_flsch,
      itab     TYPE TABLE OF zdemo_abap_flsch,
      itab_res TYPE TABLE OF zdemo_abap_carr.

ENDCLASS.



CLASS zcl_demo_abap_sql IMPLEMENTATION.


  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: ABAP SQL\n\n| ).
    out->write( |Using SELECT for multiple purposes\n| ).
    out->write( |1) Reading a single row from database table into a structure\n\n| ).

    "Note that, although it is optional, a WHERE clause should always be
    "specified for performance reasons and to restrict the read result.
    "In the following SELECT statements, a simple WHERE condition is
    "used to limit the number of found results.

    "Reading all fields

    "Reading into existing structure
    SELECT SINGLE FROM zdemo_abap_flsch
      FIELDS *
      WHERE carrid = 'LH' AND connid = '400'
      INTO @struct.

    "Alternative syntax (no FIELDS), target variable declared inline
    SELECT SINGLE *
      FROM zdemo_abap_flsch
      WHERE carrid = 'AA' AND connid = '17'
      INTO @DATA(struct_1a).

    out->write( data = struct name = `struct` ).
    out->write( |\n| ).
    out->write( data = struct_1a name = `struct_1a` ).
    out->write( |\n| ).

    "Reading selected fields

    SELECT SINGLE carrid, connid, cityfrom, cityto
      FROM zdemo_abap_flsch
      WHERE carrid = 'AZ' AND connid = '555'
      INTO @DATA(struct_1b).

    "Alternative syntax (with FIELDS)
    SELECT SINGLE
      FROM zdemo_abap_flsch
      FIELDS carrid, connid, cityfrom, cityto
      WHERE carrid = 'DL' AND connid = '106'
      INTO @DATA(struct_1c).

    "When reading a selected set of fields into an existing target
    "variable, the CORRESPONDING FIELDS OF addition in the INTO clause
    "should be used. Other, not selected fields remain initial.
    DATA struct_1d LIKE struct.

    SELECT SINGLE carrid, connid, cityfrom, cityto
      FROM zdemo_abap_flsch
      WHERE carrid = 'DL' AND connid = '106'
      INTO CORRESPONDING FIELDS OF @struct_1d.

    out->write( data = struct_1b name = `struct_1b` ).
    out->write( |\n| ).
    out->write( data = struct_1c name = `struct_1c` ).
    out->write( |\n| ).
    out->write( data = struct_1d name = `struct_1d` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Reading mutliple rows into an internal table` ) ).

    "Reading all fields into an existing internal table
    SELECT FROM zdemo_abap_flsch
      FIELDS *
      WHERE carrid = 'DL'
      INTO TABLE @itab.

    "Alternative syntax (no FIELDS), reading of a selected set of
    "fields, internal table is declared inline
    SELECT carrid, connid, cityfrom, cityto
      FROM zdemo_abap_flsch
      WHERE carrid = 'AZ'
      INTO TABLE @DATA(itab_2a).

    "When reading a selected set of fields into an existing target
    "variable, the CORRESPONDING FIELDS OF addition in the INTO clause
    "should be used. Other, not selected fields remain initial.
    DATA itab_2b LIKE itab.

    SELECT carrid, connid, cityfrom, cityto
      FROM zdemo_abap_flsch
      WHERE carrid = 'AZ'
      INTO CORRESPONDING FIELDS OF TABLE @itab_2b.

    out->write( data = itab name = `itab` ).
    out->write( |\n| ).
    out->write( data = itab_2a name = `itab_2a` ).
    out->write( |\n| ).
    out->write( data = itab_2b name = `itab_2b` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) SELECT loop: Sequentially reading multiple rows` ) ).

    "In the example below, the individual rows that are read are
    "modified before they are appended to an internal table.

    DATA itab3 LIKE itab.

    SELECT FROM zdemo_abap_flsch
     FIELDS *              "All fields
     WHERE carrid = 'DL'
     INTO @DATA(struct3).

      "Further processing of the structure if the reading is successful
      IF sy-subrc = 0.
        "Modification: Converting miles to kilometers
        IF struct3-distid = 'MI'.
          struct3-distance = struct3-distance * '1.609344'.
          struct3-distid = 'KM'.
        ENDIF.

        "Appending structure to an internal table
        APPEND struct3 TO itab3.
      ENDIF.
    ENDSELECT.

    out->write( data = itab3 name = `itab3` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) INTO CORRESPONDING FIELDS OF: Reading into existing` &&
    ` target variables that have a line type not matching the type of the data source` ) ).

    "Note: The addition CORRESPONDING FIELDS OF is needed when using
    "an existing variable to read data into, otherwise a type
    "compatibility issue might arise because the SELECT statement fills
    "the variable from left to right beginning with the first
    "component. In the example below, the identically named fields have
    "a matching type.

    "Creating structure type, structure and internal table.
    TYPES: BEGIN OF struc_type,
             carrid   TYPE zdemo_abap_flsch-carrid,
             connid   TYPE zdemo_abap_flsch-connid,
             cityfrom TYPE zdemo_abap_flsch-cityfrom,
             cityto   TYPE zdemo_abap_flsch-cityto,
           END OF struc_type.

    DATA struc4 TYPE struc_type.
    DATA itab4 TYPE TABLE OF struc_type.

    "Reading into a structure that has not a matching type
    SELECT SINGLE FROM zdemo_abap_flsch
      FIELDS carrid, connid, cityfrom, cityto
      WHERE carrid = 'AZ' AND connid = '555'
      INTO CORRESPONDING FIELDS OF @struc4.

    "Reading into an internal table that has not a matching type
    SELECT FROM zdemo_abap_flsch
      FIELDS *
      WHERE carrid = 'AZ'
      INTO CORRESPONDING FIELDS OF TABLE @itab4.

    out->write( data = struc4 name = `struc4` ).
    out->write( |\n| ).
    out->write( data = itab4 name = `itab4` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Clause variations and additions in SELECT statements` ) ).

    "SELECT/FROM clause variants
    out->write( |SELECT/FROM clause variants\n| ).

    out->write( |5) Checking the existence of a row in a database table\n| ).

    "Instead of @abap_true, you could also use 'X' in the example below.

    SELECT SINGLE @abap_true
     FROM zdemo_abap_flsch
     WHERE carrid = 'AZ' AND connid = '555'
     INTO @DATA(exists).

    IF exists = abap_true.
      out->write( `A line was found.` ).
    ELSE.
      out->write( `Nothing found.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) DISTINCT addition: Removing duplicative rows from the result set` ) ).

    "The example shows the comparison of statements with and without
    "the use of DISTINCT. When used without DISTINCT, the result
    "shows multiple entries whereas the statement with DISTINCT
    "filters the duplicates out.

    "DISTINCT addition
    SELECT DISTINCT cityfrom
      FROM zdemo_abap_flsch
      WHERE carrid = 'LH' AND
            cityto = 'NEW YORK'
      INTO TABLE @DATA(itab_6a).

    "Similar statement not using DISTINCT
    SELECT cityfrom
      FROM zdemo_abap_flsch
      WHERE carrid = 'LH' AND
            cityto = 'NEW YORK'
      INTO TABLE @DATA(itab_6b).

    out->write( data = itab_6a name = `itab_6a` ).
    out->write( |\n| ).
    out->write( data = itab_6b name = `itab_6b` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) SELECT list variants` ) ).

    "Example 1: All fields
    SELECT * FROM zdemo_abap_flsch
      WHERE carrid = 'JL'
      INTO TABLE @DATA(itab_7a).

    "Example 2: Selected set of fields using a comma-separated list
    SELECT carrid, connid, cityfrom, cityto  FROM zdemo_abap_flsch
      WHERE carrid = 'JL'
      INTO TABLE @DATA(itab_7b).

    "Example 3: Data source is explicitly specified;
    "the last column in the select list is purposely specified without it; not mandatory in the case below
    SELECT zdemo_abap_flsch~carrid,
           zdemo_abap_flsch~connid,
           zdemo_abap_flsch~airpfrom,
           airpto
      FROM zdemo_abap_flsch
      WHERE carrid = 'JL'
      INTO TABLE @DATA(itab_7c).

    "Example 4: Data source is explicitly specified; all fields
    SELECT zdemo_abap_flsch~*
      FROM zdemo_abap_flsch
      WHERE carrid = 'JL'
      INTO TABLE @DATA(itab_7d).

    "Example 5: Alias names defined for fields
    "Data is read into a target variable declared inline
    SELECT FROM zdemo_abap_flsch
      FIELDS carrid AS carr,
             connid AS conn,
             cityfrom AS ctyfr,
             cityto
      WHERE carrid = 'JL'
      INTO TABLE @DATA(itab_7e).

    "Example 6: Data is read from a database table into an existing
    "table but the line type does not match. The fields also have
    "different names (but the same type). Due to the use of alias
    "names, the fields are read into the corresponding fields.
    TYPES: BEGIN OF struc_type_diff,
             carr_id   TYPE zdemo_abap_flsch-carrid,
             conn_id   TYPE zdemo_abap_flsch-connid,
             city_from TYPE zdemo_abap_flsch-cityfrom,
             city_to   TYPE zdemo_abap_flsch-cityto,
           END OF struc_type_diff.

    DATA itab_7f TYPE TABLE OF struc_type_diff.

    "In the simple case below, the addition CORRESPONDING FIELDS OF is not
    "even necessary.
    SELECT FROM zdemo_abap_flsch
      FIELDS carrid   AS carr_id,
             connid   AS conn_id,
             cityfrom AS city_from,
             cityto   AS city_to
      WHERE carrid = 'AZ'
      INTO CORRESPONDING FIELDS OF TABLE @itab_7f.

    "Example 7: Alias for the data source
    SELECT ds~carrid, ds~connid
      FROM zdemo_abap_flsch AS ds
      WHERE carrid = 'JL'
      INTO TABLE @DATA(itab_7g).

    out->write( data = itab_7a name = `itab_7a` ).
    out->write( |\n| ).
    out->write( data = itab_7b name = `itab_7b` ).
    out->write( |\n| ).
    out->write( data = itab_7c name = `itab_7c` ).
    out->write( |\n| ).
    out->write( data = itab_7d name = `itab_7d` ).
    out->write( |\n| ).
    out->write( data = itab_7e name = `itab_7e` ).
    out->write( |\n| ).
    out->write( data = itab_7f name = `itab_7f` ).
    out->write( |\n| ).
    out->write( data = itab_7g name = `itab_7g` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Reading from an internal table using SELECT` ) ).

    "Note: The internal table from which to be read must be specified
    "as host variable. The internal table should have an explicitly
    "defined primary key.

    DATA itab_read1 TYPE TABLE OF zdemo_abap_flsch
      WITH NON-UNIQUE KEY mandt carrid connid.

    "Reading from database table to fill an internal table.
    SELECT FROM zdemo_abap_flsch
      FIELDS mandt, carrid, connid, cityfrom, cityto
      WHERE carrid = 'AA'
      INTO TABLE @itab_read1.

    "Reading from internal table.
    SELECT FROM @itab_read1 AS itab
      FIELDS *
      WHERE carrid = 'AA'
      INTO TABLE @DATA(itab_read2).

    out->write( data = itab_read2 name = `itab_read2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( 'INTO clause variants' ) ).
    out->write( |9) UP TO: Limiting the number of returned table rows\n\n| ).

    "Restricting the absolute number of returned table rows
    "by specifying a number n in the addition UP TO n ROWS.
    "In this case, the addition ORDER BY is also specified (but need not be specified).
    "The rows of the hit list are sorted on the database server and only the number of
    "sorted rows specified for UP TO n ROWS are passed to the result set. If the addition
    "ORDER BY is not specified, n arbitrary rows that meet the WHERE condition are passed
    "to the result set. If the ORDER BY clause does not sort the result set uniquely,
    "it is not possible to define which rows are in the result set.
    "Other examples here do not use the ORDER BY clause.

    SELECT *
      FROM zdemo_abap_flsch
      WHERE carrid = 'LH'
      ORDER BY carrid
      INTO TABLE @DATA(itab_up)
      UP TO 2 ROWS.

    out->write( data = itab_up name = `itab_up` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) OFFSET: Returning only the table rows after a row with a specified count from the result set` ) ).

    "In the example, data of all flights are retrieved, except for the 2 flights
    "with the shortest flight time.
    "To compare the result sets, there is one example with and one without the addition.

    SELECT *
      FROM zdemo_abap_flsch
      WHERE carrid = 'LH'
      ORDER BY fltime ASCENDING
      INTO TABLE @DATA(itab_no_off).

    out->write( data = itab_no_off name = `itab_no_off` ).
    out->write( |\n| ).

    SELECT *
      FROM zdemo_abap_flsch
      WHERE carrid = 'LH'
      ORDER BY fltime ASCENDING
      INTO TABLE @DATA(itab_w_off)
      OFFSET 2.

    out->write( data = itab_w_off name = `itab_w_off` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) Reading into individual elementary data objects` ) ).

    "The field list and the INTO list must have the
    "same number of elements.
    "In the example, a structure and internal table are created
    "to process the individually read fields within a SELECT loop.

    "Structure and internal table to include the read result
    DATA struct_ind TYPE struc_type.

    DATA itab_ind TYPE TABLE OF struc_type.

    SELECT FROM zdemo_abap_flsch
      FIELDS carrid, connid, cityfrom, cityto
      WHERE carrid = 'JL'
      INTO (@DATA(carr_id),@DATA(conn_id),@DATA(city_from),
            @DATA(city_to)).

      IF sy-subrc = 0.
        "Filling structure components with the individual values
        struct_ind = VALUE #( carrid   = carr_id
                              connid   = conn_id
                              cityfrom = city_from
                              cityto   = city_to ).

        "Appending structure to internal table
        APPEND struct_ind TO itab_ind.
      ENDIF.
    ENDSELECT.

    out->write( data = itab_ind name = `itab_ind` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `12) Appending the result set to an existing internal table` ) ).

    "APPEDNING TABLE

    "In the example, the existing internal table has the same line type
    "as the database table. The internal table from the previous
    "example is used to have a table with entries.

    SELECT *
      FROM zdemo_abap_flsch
      WHERE carrid = 'JL'
      APPENDING TABLE @itab_up.

    out->write( data = itab_up name = `itab_up` ).
    out->write( |\n| ).

    "APPENDING CORRESPONDING FIELDS OF TABLE

    "In the example, the existing internal table has not a matching
    "line type as the database table. First, an internal table table
    "is filled using the INTO CORRESPONDING ... addition. Then, a
    "statement with an APPENDING CORRESPONDING ... addition ensures
    "that the existing content is kept and the target variable
    "receives the read data in the corresponding fields.

    DATA itab_corr TYPE TABLE OF struc_type.

    "INTO CORRESPONDING FIELDS OF: Filling internal table anew
    SELECT *
      FROM zdemo_abap_flsch
      WHERE carrid = 'LH'
      INTO CORRESPONDING FIELDS OF TABLE @itab_corr
      UP TO 2 ROWS.

    "APPENDING CORRESPONDING FIELDS OF: Adding to existing table lines
    SELECT *
      FROM zdemo_abap_flsch
      WHERE carrid = 'JL'
      APPENDING CORRESPONDING FIELDS OF TABLE @itab_corr.

    out->write( data = itab_corr name = `itab_corr` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `13) Reading into packages of a specified number of rows` ) ).

    "After PACKAGE SIZE, the number of rows is specified denoting the number
    "of rows to be inserted in the target object per iteration The internal
    "table that is displayed shows all entries, i.e. all packages. Furthermore,
    "a string table is filled and displayed to visualize the package size of each
    "internal table per iteration.

    DATA itab_pack TYPE TABLE OF zdemo_abap_flsch.

    DATA pack_table TYPE string_table.

    SELECT FROM zdemo_abap_flsch
      FIELDS carrid, connid, cityfrom, cityto
      WHERE carrid <> 'AZ' AND carrid <> 'DL'
      INTO TABLE @DATA(itab_package)
      PACKAGE SIZE 3.

      IF sy-subrc = 0.
        APPEND |Internal table lines processed: | &&
               |{ lines( itab_package ) }| TO pack_table.

        "Adding internal table content to another internal table
        itab_pack = CORRESPONDING #( BASE ( itab_pack )
                                     itab_package ).
      ENDIF.
    ENDSELECT.

    out->write( data = pack_table name = `pack_table` ).
    out->write( |\n| ).
    out->write( data = itab_pack name = `itab_pack` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `14) Specifying an anonymous data object as target object` ) ).

    SELECT *
      FROM zdemo_abap_flsch
      WHERE carrid = 'LH'
      INTO TABLE NEW @DATA(dref)
      UP TO 2 ROWS.

    out->write( data = dref->* name = `dref->*` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Excursion: ABAP SQL - Operands and Expressions` ) ).
    out->write( |15) SQL operands\n\n| ).

    "SQL operands are elementary operands in an ABAP SQL statement.
    "Can be database table or view columns, a literal, host variables
    "(i. e. global or local data objects escaped using @) or host
    "expressions (@( ... )).
    "The literals can be typed (using the type name and content within
    "a pair of backquotes: char`abc`) with built-in ABAP Dictionary
    "types or untyped.
    "Regarding host expressions: Structures and internal tables are
    "possible as host expressions for statements modifying the content
    "of database tables as shown further down.
    "The example below demonstrates possible operands.

    DATA upto TYPE i VALUE 3.

    SELECT FROM zdemo_abap_flsch
    FIELDS
    "Specifies a column of a data source directly using its name
     cityfrom,

     "Column selector ~ can be used to prefix every specified column.
     "Here, it is optional. It is non-optional, e. g., if multiple data
     "sources in an ABAP SQL statement are edited and the column name
     "is not unique.
     zdemo_abap_flsch~cityto,

     'Lufthansa' AS name, "Untyped literal

     char`X` AS flag, "Typed literal

     @upto AS num, "Host variable

     @( cl_abap_context_info=>get_system_date( ) ) AS date "Host expression

    WHERE carrid = 'LH'        "Untyped literal
      AND countryfr = char`DE` "Typed literal

    "Data object created inline and escaped with @
    INTO TABLE @DATA(sql_operands)

    "The following shows all options having the same effect
    UP TO 3 ROWS.         "Untyped numeric literal
    "UP TO int4`3` ROWS.  "Typed numerice literal
    "UP TO @upto ROWS.    "Host variable
    "UP TO @( 10 - 7 ) ROWS. "Host expression

    out->write( data = sql_operands name = `sql_operands` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `16) Numeric functions ` ) ).

    "You can use built-in functions in ABAP SQL.
    "Result: Value with the associated dictionary type.
    "Arguments of the functions: Cover one or more SQL expressions.

    SELECT SINGLE
       carrname,

       "Division, result rounded to an integer
       div( 4, 2 ) AS div,

       "Division, 3rd argument: result is rounded to the specified
       "number of decimals
       division( 1, 3, 2 ) AS division,

       "Result is rounded to first greater integer
       ceil( decfloat34`1.333` ) AS ceil,

       "Result is the remainder of division
       mod( 3, 2 ) AS mod,

       "Result: Largest integer value not greater than the specified
       "value
       floor( decfloat34`1.333` ) AS floor,

       "Returns the absolute number
       abs( int4`-2` ) AS abs,

       "Result is rounded to the specified position after the decimal
       "separator
       round( decfloat34`1.337`, 2 ) AS round
       FROM zdemo_abap_carr
       WHERE carrid = 'AA'
       INTO @DATA(numeric_functions).

    out->write( data = numeric_functions name = `numeric_functions` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `17) String functions` ) ).

    SELECT SINGLE
     carrid,  "LH
     carrname, "Lufthansa
     url, "http://www.lufthansa.com

     "Concatenates strings, ignores trailing blanks
     concat( carrid, carrname ) AS concat,

     "Concatenates strings, number denotes the blanks that are inserted
     concat_with_space( carrid, carrname, 1 ) AS concat_with_space,

     "First letter of a word -> upper case, all other letters ->
     "lower case; note that a space and other special characters means
     "a new word.
     initcap( url ) AS initcap,

     "Position of the first occurrence of the substring specified
     instr( carrname,'a' ) AS instr,

     "String of length n starting from the left of an expression;
     "trailing blanks are ignored
     left( carrname, 4 ) AS left,

     "Number of characters in an expression, trailing blanks are
     "ignored
     length( url ) AS length,

     "Checks if expression contains a PCRE expression;
     "case-sensitive by default (case_sensitive parameter can be
     "specified)
     "Notes on the result: 1 = found, 0 = not found
     "PCRE below: Searches a period that is followed by any character
     like_regexpr( pcre  = '\..',
                   value = url ) AS like_regex,

     "Returns position of a substring in an expression,
     "3rd parameter = specifies offset (optional)
     "4th parameter = determines the number of occurrences (optional)
     locate( carrname, 'a', 0, 2 ) AS locate,

     "Searches a PCRE pattern, returns offset of match;
     "many optional parameters: occurrence, case_sensitive, start,
     "group
     locate_regexpr( pcre = '\..', "Period followed by any character
                     value = url,
                     occurrence = 2 ) "2nd occurrence in the string
                     AS locate_regexpr,

     "Searches a PCRE pattern, returns offset of match + 1;
     "many optional parameters: occurrence, case_sensitive, start,
     "group
     locate_regexpr_after( pcre = '.',  "Any character
                           value = url,
                           occurrence = 1 ) AS locate_regexpr_after,

     "Removes leading characters as specified in the 2nd argument,
     "trailing blanks are removed
     ltrim( carrname, 'L' ) AS ltrim,

     "Counts all occurrences of found PCRE patterns
     occurrences_regexpr( pcre = '\..',
                          value = url ) AS occ_regex,

     "Replaces the 2nd argument with the 3rd in an expression
     replace( carrname,'a','#' ) AS replace,

     "Replaces a found PCRE expression;
     "more parameters possible: occurrence, case_sensitive, start
     replace_regexpr( pcre = '\..',
                      value = url,
                      with = '#' ) AS replace_regex,

     "Extracts a string with the length specified starting from the
     "right
     right( carrname, 5 ) AS right,

     "Expands string to length n (2nd argument); trailing blanks
     "produced are replaced by the characters from the (3rd) argument
     "Note that if n is less than the string, the expression is
     "truncated on the right.
     rpad( carrname, 12, '#' ) AS rpad,

     "All trailing characters that match the character of the 2nd
     "argument are removed; trailing blanks are removed, too
     rtrim( carrname, 'a' ) AS rtrim,

     "Returns a substring; 2nd argument = position from where to start;
     "3rd argument: length of the extracted substring
     substring( carrname, 3, 3 ) AS substring,

     "Searches for a PCRE expression and returns the matched substring
     "More parameters possible: occurrence, case_sensitive, start, group
     substring_regexpr( pcre = '\...',
                        value = url ) AS substring_regexpr,

     "All lower case letters are transformed to upper case letters
     upper( carrname ) AS upper
     FROM zdemo_abap_carr
     WHERE carrid = 'LH'
     INTO @DATA(string_functions).

    out->write( data = string_functions name = `string_functions` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `18) Special functions` ) ).

    SELECT SINGLE
      carrid,

      "Conversion functions
      "When used: Special conversions that cannot be handled in a
      "general CAST expression

      "Type conversion: string of fixed length (e.g. of type c) to
      "variable length string of type string
      to_clob( carrid ) AS clob,

      "Byte string -> character string
      bintohex( raw`3599421128650F4EE00008000978B976` ) AS bintohex,

      "Character string -> byte string
      hextobin( char`3599421128650F4EE00008000978B976` ) AS hextobin,

      "Byte field of type RAW to a byte string (BLOB) of type RAWSTRING
      to_blob( raw`3599421128650F4EE00008000978B976` ) AS blob,

      "Unit conversion function
      "More parameters are available.

      "Converts miles to kilometers
      unit_conversion( quantity = d34n`1`,
                       source_unit = unit`MI`,
                       target_unit = unit`KM` ) AS miles_to_km,

      "Date and time functions
      "There are plenty of functions; the below functions are a
      "selection.

      add_days( @( cl_abap_context_info=>get_system_date( ) ), 4
              ) AS add_days,
      add_months( @( cl_abap_context_info=>get_system_date( ) ), 2
              ) AS add_months,
      is_valid( @( cl_abap_context_info=>get_system_date( ) ) ) AS date_is_valid,
      is_valid( @( cl_abap_context_info=>get_system_time( ) ) ) AS time_is_valid

     FROM zdemo_abap_carr
     WHERE carrid = 'AA'
     INTO @DATA(special_functions).

    "Retrieving type information using RTTI to demonstrate the effect
    "of type conversions like to_clob etc.
    "type_kind: g (character string with variable length),
    "C (character string of fixed length), X (binary), y (byte string)
    DATA(components) = CAST cl_abap_structdescr(
     cl_abap_typedescr=>describe_by_data( special_functions )
      )->components.

    out->write( data = components name = `components` ).
    out->write( |\n| ).
    out->write( data = special_functions name = `special_functions` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `19) Aggregate Expressions` ) ).

    "Consist of aggregate functions and aggregate the values of
    "multiple rows of the result set of a query into a single value.
    "The example shows a selection of available functions.

    SELECT
     carrid,

     "Average value of the content of a column in a row set
     AVG( fltime ) AS fltime1,

     "AVG with data type specification for the result
     AVG( fltime AS DEC( 14,4 ) ) AS fltime2,

     "Maximum value of the results in a row set
     MAX( fltime ) AS max,

     "Minimum value
     MIN( fltime ) AS min,

     "Sum of the results in a row set.
     SUM( fltime ) AS sum,

     "Returns the number of rows in a row set.
     "The following two have the same meaning.
     COUNT( * ) AS count2,
     COUNT(*) AS count3,

     "Chains the results in a row set.
     "An optional separator can be specified
     STRING_AGG( airpfrom, ', ' ) AS string_agg

     FROM zdemo_abap_flsch
     WHERE carrid = 'LH'
     GROUP BY carrid
     INTO TABLE @DATA(agg_exp).

    out->write( data = agg_exp name = `agg_exp` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `20) More SQL Expressions` ) ).

    "Arithmetic expressions to perform arithmetic calculations
    "Cast expressions to convert the value of operands to a dedicated
    " dictionary type. Note that there are special conversion rules.
    "String expressions using the operator && to concatenate character
    " strings.
    "Case distinctions to carry out either a simple (comparison of the
    " values of a dedicated operand) or complex (searched case;
    " evaluation of multiple logical expressions) case distinction.

    SELECT SINGLE
      carrid,

      "Arithmethic expressions
      "operators + - *
      "Note that / is not allowed in integer expressions as the
      "one below.
      ( 1 + 2 ) * 3 AS calc,

      "/ used in an expression using type adjustment in ABAP SQL.
      "A cast expression converts the value of the operands to the
      "specified dictionary type. The result is a representation of
      "the source value in the specified type.
      CAST( 1 AS D34N ) / CAST( 2 AS D34N ) AS ratio,

      "String expression using && to concatenate two character strings;
      "the result of the concatenation must not be longer than
      "255 characters.
      carrid && carrname AS concat,

      "Case distinction
      "Simple case distinction
      "The expression compares the values of an operand with other
      "operands. Result: The first operand after THEN for which the
      "comparison is true. If no matches are found, the result
      "specified after ELSE is selected.
      CASE currcode
           WHEN 'EUR' THEN 'A'
           WHEN 'USD' THEN 'B'
           ELSE 'C'
      END AS case_simple,

      "Complex case distinction
      "The expression evaluates logical expressions. Result: The first
      "operand after THEN for which the logical expression is true. If
      "no logical expressions are true, the result specified after ELSE
      "is selected.
      CASE WHEN length( carrname ) <= 5 THEN 'small'
           WHEN length( carrname ) BETWEEN 6 AND 10 THEN 'mid'
           WHEN length( carrname ) BETWEEN 11 AND 15 THEN 'large'
           ELSE 'huge'
      END AS case_complex
     FROM zdemo_abap_carr
     WHERE carrid = 'AA'
     INTO @DATA(more_sql_expr).

    out->write( data = more_sql_expr name = `more_sql_expr` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `21) Window expressions (1)` ) ).

    "A simple window is constructed in the OVER clause,
    "window functions - here aggregate functions - are applied.
    "To narrow the entries in the resulting table, duplicates
    "are deleted.

    SELECT carrid, currency,
     SUM( paymentsum ) OVER( PARTITION BY carrid ) AS sum,
     AVG( price AS DEC( 14,2 ) ) OVER( PARTITION BY carrid ) AS avg,
     MAX( price ) OVER( PARTITION BY carrid ) AS max
     FROM zdemo_abap_fli
     ORDER BY carrid
     INTO TABLE @DATA(win).

    DELETE ADJACENT DUPLICATES FROM win COMPARING ALL FIELDS.

    out->write( data = win name = `win` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `22) Window expressions (2)` ) ).

    SELECT carrid, currency, fldate,
      "Sorts the rows by some columns and counts the number of rows from
      "the first row of the window to the current row.
      COUNT( * ) OVER( ORDER BY currency, fldate
                        ROWS BETWEEN
                        "UNBOUNDED PRECEDING: frame starts at the
                        "first row of the window
                        UNBOUNDED PRECEDING
                        "CURRENT ROW: determines starting or ending
                        "at the current row; here, it ends
                        AND CURRENT ROW ) AS count1,

      "If no window frame is used, the default window frame is
      "BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW,
      "i. e. the result of count1 equals count2
      COUNT( * ) OVER( ORDER BY currency, fldate ) AS count2,

      "Sorts the rows by some columns and counts the number of rows from
      "the current row to the last row of the window.
      "The result is reverse numbering.
      COUNT( * ) OVER( ORDER BY currency, fldate
                       ROWS BETWEEN CURRENT ROW
                       "UNBOUND FOLLOWING:
                       "Determines the ending frame boundary,
                       "this addition specifies the last row of the
                       "window
                       AND UNBOUNDED FOLLOWING ) AS count_reverse,

      "Sorts the rows by some columns and calculates the rolling
      "averages of a subset of rows from column price. The subset
      "consists of the current row plus one preceding and one following
      "row. A better use case as below example would be that, for
      "example, you can calculate the 3-day-average temperature for
      "every day from a list of temperature data.
      AVG( price AS DEC( 14,2 ) ) OVER( ORDER BY currency, fldate
           ROWS BETWEEN
           "n PRECEDING: for both start and end of frame;
           "frame to start/end n rows above the current row
           1 PRECEDING
           "n FOLLOWING: for both start and end of frame;
           "frame to start/end n rows beneath the current row
           AND 1 FOLLOWING ) AS avg

      FROM zdemo_abap_fli
      INTO TABLE @DATA(win_order).

    out->write( data = win_order name = `win_order` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `SQL conditions` ) ).
    out->write( |23) SQL conditions (1)\n\n| ).
    "The example demonstrates a WHERE clause with =, >, <, <=, >=, AND

    SELECT * FROM zdemo_abap_fli
          WHERE carrid = 'LH'    "or EQ
            AND price > 700      "or GT
            AND seatsocc < 320   "or LT
            AND seatsmax <= 330  "or LE
            AND seatsmax_b >= 30 "or GE
          INTO TABLE @DATA(itab_comp_op).

    out->write( data = itab_comp_op name = `itab_comp_op` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `24) SQL conditions (2)` ) ).

    "The example demonstrates a WHERE clause with
    "BETWEEN, NOT BETWEEN, OR

    SELECT * FROM zdemo_abap_fli
      WHERE seatsmax BETWEEN 350 AND 400            "#EC CI_CMPLX_WHERE
         OR price NOT BETWEEN 100 AND 1500
      INTO TABLE @DATA(it_sql_cond).

    out->write( data = it_sql_cond name = `it_sql_cond` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `25) SQL conditions (3)` ) ).

    "The example demonstrates a WHERE clause with character literals:
    "- LIKE '%FRAN%': Condition is true if the column cityfrom contains
    "  a string containing the pattern 'FRAN'
    "- NOT LIKE '_X%': Condition is true if the column airpto contains
    "  a value whose second character is not 'X'.
    "- IN ( ... ): condition is true if the column cityto contains
    "  one of the values specified within the brackets
    "- NOT IN ( ... ): condition is true if the column cityto does not
    "  contain one of the values specified within the brackets
    SELECT * FROM zdemo_abap_flsch
      WHERE cityfrom LIKE '%FRAN%'
        AND airpto NOT LIKE '_X%'
        AND cityto IN ( 'BERLIN', 'NEW YORK', 'LONDON' )
        AND cityto NOT IN ( 'SYDNEY' )
      INTO TABLE @DATA(itab_like_in).


    out->write( data = itab_like_in name = `itab_like_in` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Further clauses in SELECT statements` ) ).

    out->write( |26) GROUP BY: Combining groups of table rows in the result set\n\n| ).
    "In the example, the database table rows that have the same content
    "in column CARRID are combined. The lowest and highest values in
    "column PRICE are determined for each of these groups and placed
    "into the combined row. Note that the GROUP BY clause requires all
    "columns that are directly specified in the SELECT list or
    "specified there as an argument of an SQL expression to be
    "specified. Aggregate functions in aggregate expressions are an
    "exception, as shown below.

    SELECT FROM zdemo_abap_fli
      FIELDS carrid,
             MIN( price ) AS min_price,
             MAX( price ) AS max_price
      GROUP BY carrid
      INTO TABLE @DATA(itab_gr).

    out->write( data = itab_gr name = `itab_gr` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `27) HAVING: Limiting the number of rows` &&
    ` in groups in the result set` ) ).

    "The addition HAVING limits the number of rows in groups in the
    "result set of a query by using a logical expression on these rows.
    "The logical expression evaluates the content of row groups. Those
    "rows are placed in the result set for which the logical expression
    "is true.

    SELECT FROM zdemo_abap_flsch
      FIELDS carrid, connid, cityfrom, cityto
      WHERE carrid = 'LH'
      GROUP BY carrid, connid, cityfrom, cityto
      HAVING SUM( fltime ) > 100
      INTO TABLE @DATA(itab_hav).

    out->write( data = itab_hav name = `itab_hav` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `28) ORDER BY: Sorting the result set by ` &&
    `specified columns` ) ).

    "The following example shows the ordering of the result set based
    "on the content of the primary key of the data source. You can also
    "order by any columns and by explicitly specifying the sort order.
    "There are more options to order, for example, by using SQL
    "expressions.

    "Example 1: Sorting the result set by primary key
    SELECT *
      FROM zdemo_abap_flsch
      WHERE carrid <> 'UA'
      ORDER BY PRIMARY KEY
      INTO TABLE @DATA(itab_ord1)
      UP TO 3 ROWS.

    "Example 2: Sorting by arbitrary field and specifying the sort order
    SELECT *
      FROM zdemo_abap_flsch
      WHERE carrid <> 'UA'
      ORDER BY fltime DESCENDING
      INTO TABLE @DATA(itab_ord2)
      UP TO 3 ROWS.

    out->write( data = itab_ord1 name = `itab_ord1` ).
    out->write( |\n| ).
    out->write( data = itab_ord2 name = `itab_ord2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `WHERE clause variants: Selecting data by evaluating the content of other tables` ) ).
    out->write( |29) FOR ALL ENTRIES addition\n\n| ).

    "In the example, only those entries should be read from the
    "database table if entries exist in the internal table that meet
    "the conditions specified in the WHERE clause. Note that you should
    "ensure that the internal table is not initial.
    "Check the ABAP Keyword Documentation for various restrictions that
    "apply when using this addition. The following example is just for
    "demonstrating the syntax - as are all examples.

    "Filling an internal table
    SELECT * FROM zdemo_abap_fli
      WHERE seatsmax < 300
      INTO TABLE @DATA(cond_tab).

    IF ( 0 < lines( cond_tab ) ).
      SELECT carrid, connid, cityfrom, cityto      "#EC CI_NO_TRANSFORM
        FROM zdemo_abap_flsch
        FOR ALL ENTRIES IN @cond_tab
        WHERE carrid = @cond_tab-carrid
        AND connid = @cond_tab-connid
        INTO TABLE @DATA(itab_forall).
    ENDIF.

    out->write( data = itab_forall name = `itab_forall` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `30) Checking the result set of a subquery` ) ).

    "In the example, all available flights leaving from a city with
    "FRAN in the name (San Francisco, Frankfurt) existing in another
    "database table and for which further comparisons are true
    "(matching CARRID and CONNID) are read into an internal table.

    SELECT carrid, connid, fldate
      FROM zdemo_abap_fli AS zdemo_abap_fli
      WHERE EXISTS
      ( SELECT carrid FROM zdemo_abap_flsch
          WHERE carrid = zdemo_abap_fli~carrid
          AND connid = zdemo_abap_fli~connid
          AND cityfrom LIKE '%FRAN%' )
      ORDER BY carrid, connid, fldate
      INTO TABLE @DATA(itab_sub).

    out->write( data = itab_sub name = `itab_sub` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Combining Data of Multiple Database Tables` ) ).
    out->write( |31) Inner join\n\n| ).
    "Result set:
    "- Columns of the rows in the result set of the left side with the columns
    "  of the rows in the result set of the right side are joined into a single
    "  result set.
    "- Contains all combinations of rows for whose columns the join condition is true.

    "Example 1
    SELECT p~carrid, p~connid, p~cityto, f~fldate
      FROM zdemo_abap_flsch AS p
      INNER JOIN zdemo_abap_fli AS f
        ON p~carrid = f~carrid AND p~connid = f~connid
      WHERE p~cityfrom = 'NEW YORK'
      ORDER BY p~carrid
      INTO TABLE @DATA(itab_in1).

    "Example 2: Multiple inner joins
    SELECT p~carrid, s~carrname, p~connid, p~cityto, f~fldate
      FROM zdemo_abap_flsch AS p
      INNER JOIN zdemo_abap_fli AS f
        ON p~carrid = f~carrid AND p~connid = f~connid
      INNER JOIN zdemo_abap_carr AS s
        ON p~carrid = s~carrid
      WHERE p~cityfrom = 'FRANKFURT'
      ORDER BY p~carrid
      INTO TABLE @DATA(itab_in2).

    out->write( data = itab_in1 name = `itab_in1` ).
    out->write( |\n| ).
    out->write( data = itab_in2 name = `itab_in2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `32) Left outer join` ) ).

    "Result set:
    "- Same result set as the inner join.
    "- Difference: For each selected row on the left side as LEFT OUTER JOIN,
    "  at least one row is created in the result set even if no rows on the
    "  other side meet the condition. The columns on the other side that do
    "  not meet the condition are filled with null values.
    "In the example, all rows from the left-hand side (zdemo_abap_carr)
    "are returned as well as the matching rows from the right-hand side
    "(zdemo_abap_flsch). However, the ON condition
    ""p~cityfrom = 'FRANKFURT'" is not met for several entries in
    "zdemo_abap_flsch and a CONNID does not exist.

    SELECT s~carrid, s~carrname, p~connid
      FROM zdemo_abap_carr AS s
      LEFT OUTER JOIN zdemo_abap_flsch AS p
        ON s~carrid = p~carrid AND p~cityfrom = 'FRANKFURT'
      WHERE s~carrid <> 'UA'
      ORDER BY s~carrid
      INTO TABLE @DATA(itab_lo).

    out->write( data = itab_lo name = `itab_lo` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `33) Merging the result sets of multiple queries into a single result set using UNION` ) ).

    "Effect: The rows of the result set of the query after UNION are
    "inserted into the result set of the query in front of UNION.
    "The example demonstrates the union of two tables and
    "visualizes those columns that do not exist in the other table by
    "setting the value '-'. Here, a CAST is required for the column CONNID.

    SELECT FROM zdemo_abap_carr
           FIELDS carrname,
                  CAST( '-' AS CHAR( 4 ) ) AS connid,
                  '-' AS cityfrom,
                  '-' AS cityto
           WHERE carrid = 'LH'
      UNION
        SELECT FROM zdemo_abap_flsch
               FIELDS '-' AS carrname,
                      CAST( connid AS CHAR( 4 ) ) AS connid,
                      cityfrom,
                      cityto
               WHERE carrid = 'LH'
       ORDER BY carrname DESCENDING, connid, cityfrom, cityto
       INTO TABLE @DATA(itab_union).

    out->write( data = itab_union name = `itab_union` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `34) Common Table Expressions (CTE) (1)` ) ).

    "The result sets of both common table expressions +connections
    "and +sum_seats are merged in the subquery of the CTE +result in
    "a join expression. An explicit name list assigns names to the
    "resulting columns. These names are used in the main query to sort
    "the results. For each flight connection of the selected airline,
    "the total number of occupied seats is output from the database
    "table.

    WITH
      +connections AS (
        SELECT zdemo_abap_flsch~carrid, carrname, connid, cityfrom, cityto
               FROM zdemo_abap_flsch
               INNER JOIN zdemo_abap_carr
                 ON zdemo_abap_carr~carrid = zdemo_abap_flsch~carrid
               WHERE zdemo_abap_flsch~carrid BETWEEN 'AA' AND 'JL' ),
      +sum_seats AS (
        SELECT carrid, connid, SUM( seatsocc ) AS sum_seats
               FROM zdemo_abap_fli
               WHERE carrid BETWEEN 'AA' AND 'JL'
               GROUP BY carrid, connid ),
      +result( name, connection, departure, arrival, occupied ) AS (
        SELECT carrname, c~connid, cityfrom, cityto, sum_seats
               FROM +connections AS c
                 INNER JOIN +sum_seats AS s
                   ON c~carrid = s~carrid AND
                      c~connid = s~connid )
      SELECT *
             FROM +result
             ORDER BY name, connection
             INTO TABLE @DATA(itab_cte).

    out->write( data = itab_cte name = `itab_cte` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `35) CTE and a SELECT Loop (2)` ) ).
    "The example shows a WITH statement, whose main query creates a
    "tabular result set. Since the data is written into work area
    "rather than to an internal table, a SELECT loop is opened, which
    "must be closed with ENDWITH.

    WITH
      +carriers AS ( SELECT FROM zdemo_abap_carr
                            FIELDS carrid, carrname )
      SELECT FROM zdemo_abap_flsch AS s
               INNER JOIN +carriers AS c
                 ON s~carrid = c~carrid
             FIELDS c~carrname, s~connid
             WHERE s~carrid = 'LH'
             INTO @DATA(wa_cte_loop)
             UP TO 3 ROWS.
      out->write( data = wa_cte_loop name = `wa_cte_loop` ).
      out->write( |\n| ).
    ENDWITH.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Changing data in database tables` ) ).

    "Deleting database table to work with
    DELETE FROM zdemo_abap_carr.

    "Creating table rows to be inserted in the database table
    DATA(row1) = VALUE zdemo_abap_carr( carrid = 'AF'
                                        carrname = 'Air France'
                                        currcode = 'EUR'
                                        url =  'http://www.airfrance.fr' ).

    DATA(row2) = VALUE zdemo_abap_carr( carrid = 'UA'
                                        carrname = 'United Airlines'
                                        currcode = 'USD'
                                        url =  'http://www.ual.com' ).

**********************************************************************

    out->write( |36) INSERT: Inserting individual line into a database table\n\n| ).

    "Inserting from an existing structure
    INSERT INTO zdemo_abap_carr VALUES @row1.

    "Alternative syntax having the same effect as the statement above
    INSERT zdemo_abap_carr FROM @row2.

    "Inserting from a structure created inline using a
    "constructor expression with VALUE within a host expression
    INSERT zdemo_abap_carr FROM @( VALUE #( carrid = 'SR'
                                            carrname = 'Swiss'
                                            currcode = 'CHF'
                                            url =  'http://www.swiss.com' ) ).


    select_from_dbtab( ).
    out->write( data = itab_res name = `itab_res` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `37) INSERT: Inserting multiple rows into a database table` ) ).

    "Creating and filling an internal table
    DATA itab_insert TYPE TABLE OF zdemo_abap_carr.

    itab_insert = VALUE #( ( carrid = 'BA'
                             carrname = 'British Airways'
                             currcode = 'GBP'
                             url =  'http://www.british-airways.com' )
                           ( carrid = 'FJ'
                             carrname = 'Air Pacific'
                             currcode = 'USD'
                             url =  'http://www.airpacific.com' ) ).


    "Inserting from existing internal table
    INSERT zdemo_abap_carr FROM TABLE @itab_insert.

    "Inserting from an internal table created inline using
    "a constructor expression with VALUE within a host expression
    INSERT zdemo_abap_carr FROM TABLE @( VALUE #( ( carrid = 'ET'
                                                    carrname = 'Ethiopian Airlines'
                                                    currcode = 'ETB'
                                                    url =  'http://www.ethiopianairlines.com' )
                                                  ( carrid = 'QF'
                                                    carrname = 'Qantas Airways'
                                                    currcode = 'AUD'
                                                    url =  'http://www.qantas.com.au' ) ) ).

    select_from_dbtab( ).
    out->write( data = itab_res name = `itab_res` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `38) INSERT: Inserting multiple rows into a database table accepting duplicate keys` ) ).

    "ACCEPTING DUPLICATE KEYS addition: To avoid a runtime error when
    "inserting entries from an internal table having duplicate keys,
    "all lines that would produce duplicate entries in the database
    "table regarding the keys are discarded and sy-subrc is set to 4.

    "Creating and filling an internal table
    DATA itab_adk TYPE TABLE OF zdemo_abap_carr.

    itab_adk = VALUE #( ( carrid = 'SQ'
                          carrname = 'Singapore Airlines'
                          currcode = 'SGD'
                          url =  'http://www.singaporeair.com' )
                        ( carrid = 'SQ'
                          carrname = 'Singapore Airlines'
                          currcode = 'SGD'
                          url =  'http://www.singaporeair.com' ) ).

    INSERT zdemo_abap_carr FROM TABLE @itab_adk ACCEPTING DUPLICATE KEYS.

    DATA(subrc) = sy-subrc.

    select_from_dbtab( ).
    out->write( data = itab_res name = `itab_res` ).
    out->write( |\n| ).
    out->write( data = subrc name = `subrc` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `39) INSERT: Using a subquery` ) ).

    "The purpose of this abstract example is just to visualize that
    "subqueries are possible in INSERT statements. In the example,
    "the goal is just to get one entry from table zdemo_abap_flsch.
    "Since only MANDT and CARRID are shared fields in the two database
    "tables other values in zdemo_abap_carr remain empty. The line is
    "further processed in the following example.

    INSERT zdemo_abap_carr FROM ( SELECT carrid
                                  FROM zdemo_abap_flsch
                                  WHERE carrid = 'LH' AND
                                        connid = '0400' ).

    select_from_dbtab( ).
    out->write( data = itab_res name = `itab_res` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `40) UPDATE: Changing content of existing rows` ) ).

    "Creating and filling structure
    "In the case below, all field values except the key field are updated.
    DATA(row_upd) = VALUE zdemo_abap_carr(
                            carrid   = 'LH'
                            carrname = 'Lufthansa'
                            currcode = 'EUR'
                            url      =  'http://www.lufthansa.com' ).

    "Creating and filling internal table
    DATA itab_update LIKE itab_insert.

    itab_update = VALUE #( ( carrid = 'BA'
                             carrname = 'British Airways'
                             currcode = 'GBP'
                             url =  'http://www.britishairways.com' ) "updated
                           ( carrid = 'FJ'
                             carrname = 'Fiji Airways'   "updated
                             currcode = 'USD'
                             url =  'http://www.fijiairways.com' ) )."updated


    UPDATE zdemo_abap_carr FROM @row_upd.

    UPDATE zdemo_abap_carr FROM TABLE @itab_update.

    select_from_dbtab( ).
    out->write( data = itab_res name = `itab_res` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `41) UPDATE: Changing values of specific fields in all table rows` ) ).

    "Using the SET addition, you can change the values of specific
    "fields in all table rows without overwriting existing values in
    "other fields.
    "In the example, the field CURRCODE is set as specified for all
    "rows for which the WHERE condition is true.

    UPDATE zdemo_abap_carr
      SET currcode = 'EUR'
      WHERE carrid <> 'UA' AND carrid <> 'ET'.

    select_from_dbtab( ).
    out->write( data = itab_res name = `itab_res` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `42) INDICATORS addition to UPDATE statements: ` &&
    `Changing values of specific fields without overwriting ` &&
    `existing values of other fields ` ) ).

    "Example:
    "- Structured type is created with WITH INDICATORS addition
    "- Internal table from which to update a database table is created; it
    "  includes the indicator structure comp_ind
    "- Internal table is filled; only one component is flagged as
    "  to be updated
    "- Other fields remain unchanged; note that key fields must be
    "  included in ind_tab (indicator setting for key fields has
    "  no effect)

    "Structured type with WITH INDICATORS addition
    TYPES ind_wa TYPE zdemo_abap_carr WITH INDICATORS comp_ind TYPE abap_bool.

    DATA ind_tab TYPE TABLE OF ind_wa.

    "Filling internal table; only CURRCODE should be updated
    ind_tab = VALUE #( ( carrid = 'QF'
                         carrname = 'Qantas Airways'
                         currcode = 'AUD'
                         comp_ind-currcode = abap_true )
                       ( carrid = 'SQ'
                         carrname = 'Singapore Airlines'
                         currcode = 'SGD'
                         comp_ind-currcode = abap_true ) ).

    UPDATE zdemo_abap_carr
      FROM TABLE @ind_tab
      INDICATORS SET STRUCTURE comp_ind.

    select_from_dbtab( ).
    out->write( data = itab_res name = `itab_res` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `43) MODIFY: Inserting and changing rows` ) ).
    "The example only uses host expressions.

    "Modifying an entry based on a row. Here, a new entry is created in
    "the database table since no row exists having the key.
    "The example uses a structure created inline using a constructor
    "expression with VALUE within a host expression.
    MODIFY zdemo_abap_carr FROM @(
          VALUE #( carrid = 'AZ'
                   carrname = 'ITA Airways'
                   currcode = 'EUR'
                   url =  'http://www.ita-airways.com' ) ).

    "Storing the value of system field sy-dbcnt to determine how many
    "table rows were modified.
    DATA(dbcnt) = sy-dbcnt.

    "Modifying entries based on an internal table. Here, no new entry
    "is created. Existing entries are updated.
    MODIFY zdemo_abap_carr FROM TABLE @( VALUE #( ( carrid = 'BA'
                                                    carrname = 'British Airways'
                                                    currcode = 'GBP'                      "modified
                                                    url =  'http://www.britishairways.co.uk' ) "mod
                                                  ( carrid = 'QF'
                                                    carrname = 'Qantas Airways'
                                                    currcode = 'AUD'
                                                    url =  'http://www.qantas.com' ) ) ). "modified

    "Adding the value of sy-dbcnt to the value from above to get
    "the total number of modified rows in this example.
    dbcnt = dbcnt + sy-dbcnt.

    select_from_dbtab( ).
    out->write( data = itab_res name = `itab_res` ).
    out->write( |\n| ).
    out->write( |{ dbcnt } table rows were modified.| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `44) DELETE: Deleting table rows` ) ).
    "Note that you specify the key fields only.

    "Deleting an entry based on a row. Here, the example uses a
    "structure created inline and by only specifying the key value
    "using a constructor expression with VALUE within a host
    "expression.
    DELETE zdemo_abap_carr FROM @( VALUE #( carrid = 'QF' ) ).

    "Deleting multiple entries based on an internal table.
    "Same as above, the internal table is created inline.
    DELETE zdemo_abap_carr FROM TABLE @( VALUE #( ( carrid = 'AF' )
                                                  ( carrid = 'AZ' )
                                                  ( carrid = 'LH' ) ) ).

    select_from_dbtab( ).
    out->write( data = itab_res name = `itab_res` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `45) DELETE: Deleting table rows based on a condition` ) ).

    DELETE FROM zdemo_abap_carr WHERE currcode <> 'EUR'.

    select_from_dbtab( ).
    out->write( data = itab_res name = `itab_res` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `46) DELETE: Delete complete table` ) ).

    DELETE FROM zdemo_abap_carr.

    select_from_dbtab( ).
    out->write( data = itab_res name = `itab_res` ).

  ENDMETHOD.


  METHOD select_from_dbtab.
    SELECT *
      FROM zdemo_abap_carr
      ORDER BY carrid
      INTO TABLE @itab_res.
  ENDMETHOD.
ENDCLASS.
