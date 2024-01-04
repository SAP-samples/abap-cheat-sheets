***********************************************************************
*
*                 ABAP cheat sheet: Internal tables
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntax options for working with
*   internal tables.
* - Topics covered: Creating, filling, reading from, sorting, modifying
*   internal tables
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
"! <p class="shorttext synchronized">ABAP cheat sheet: Internal tables</p>
"! Example to demonstrate working with internal tables.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_internal_tables DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.
    CLASS-METHODS: class_constructor.

  PRIVATE SECTION.
    "Creating structured data types.
    TYPES: "Line types for internal tables
      BEGIN OF struc1,
        a TYPE i,
        b TYPE c LENGTH 3,
        c TYPE c LENGTH 3,
        d TYPE c LENGTH 3,
      END OF struc1,
      BEGIN OF struc2,
        a TYPE i,
        b TYPE c LENGTH 3,
        e TYPE c LENGTH 3,
        f TYPE string,
      END OF struc2,
      "Types for demonstrating nested internal tables
      BEGIN OF l_type1,
        key_field TYPE i,
        char1     TYPE c LENGTH 10,
        char2     TYPE c LENGTH 10,
        num1      TYPE i,
        num2      TYPE i,
      END OF l_type1,
      BEGIN OF l_type2,
        key_field TYPE i,
        num1      TYPE i,
        num2      TYPE i,
      END OF l_type2,
      BEGIN OF nested1,
        key_field TYPE i,
        char1     TYPE c LENGTH 10,
        tab       TYPE STANDARD TABLE OF l_type2 WITH EMPTY KEY,
      END OF nested1,
      BEGIN OF nested2,
        key_field TYPE i,
        char2     TYPE c LENGTH 10,
        tab       TYPE STANDARD TABLE OF l_type1 WITH EMPTY KEY,
      END OF nested2,
      "Declaring internal table types.
      "Types for demonstrating nested internal tables
      ty_nested1 TYPE STANDARD TABLE OF nested1 WITH EMPTY KEY,
      ty_nested2 TYPE STANDARD TABLE OF nested2 WITH EMPTY KEY.

    CLASS-DATA:
      "Declaring internal tables.
      "Internal tables for demonstrating nested internal tables
      itab_nested1 TYPE ty_nested1,
      itab_nested2 TYPE ty_nested2,
      "Internal tables for CORRESPONDING/MOVE-CORRESPONDING demo
      tab1         TYPE TABLE OF struc1 WITH NON-UNIQUE KEY a,
      tab2         TYPE TABLE OF struc2 WITH NON-UNIQUE KEY a,
      tab3         TYPE SORTED TABLE OF struc1 WITH UNIQUE KEY a,
      tab4         TYPE SORTED TABLE OF struc2 WITH UNIQUE KEY a.

    CLASS-METHODS:
      fill_dbtabs,
      fill_itabs_for_corresponding.
ENDCLASS.



CLASS ZCL_DEMO_ABAP_INTERNAL_TABLES IMPLEMENTATION.


  METHOD class_constructor.
    fill_dbtabs( ).
  ENDMETHOD.


  METHOD fill_dbtabs.
    "Initializing and filling of database tables to have data to work with

    DELETE FROM zdemo_abap_tab1.
    DELETE FROM zdemo_abap_tab2.

    MODIFY zdemo_abap_tab1 FROM TABLE @( VALUE #(
    ( key_field = 100 char1 = 'aaa' char2 = 'bbb' num1 = 1 num2 = 2 )
    ( key_field = 200 char1 = 'ccc' char2 = 'ddd' num1 = 3 num2 = 4 )
    ( key_field = 300 char1 = 'eee' char2 = 'fff' num1 = 5 num2 = 6 )
    ( key_field = 400 char1 = 'ggg' char2 = 'hhh' num1 = 7 num2 = 8 ) ) ).
    MODIFY zdemo_abap_tab2 FROM TABLE @( VALUE #(
    ( key_field = 500 char1 = 'iii' num1 = 10 numlong = 1000 )
    ( key_field = 600 char1 = 'kkk' num1 = 12 numlong = 2000 )
    ( key_field = 700 char1 = 'mmm' num1 = 14 numlong = 3000 )
    ( key_field = 800 char1 = 'ooo' num1 = 15 numlong = 4000 ) ) ).
  ENDMETHOD.


  METHOD fill_itabs_for_corresponding.
    tab1 = VALUE #( ( a = 1 b = 'aaa' c = 'aaa' d = 'A' )
                    ( a = 2 b = 'bbb' c = 'bbb' d = 'B' ) ).

    tab2 = VALUE #( ( a = 3 b = 'ccc' e = 'ccc' f = `CCC` )
                    ( a = 4 b = 'ddd' e = 'ddd' f = `DDD` ) ).

    tab3 = VALUE #( ( LINES OF tab1 ) ).

    tab4 = VALUE #( ( a = 1 b = 'xxx' e = 'yyy' f = `ZZZ` )
                    ( LINES OF tab2 ) ).

    itab_nested1 = VALUE #(
      ( key_field = 1 char1 = 'aaa'
        tab       = VALUE #( ( key_field = 1 num1 = 2 num2 = 3 )
                             ( key_field = 2 num1 = 3 num2 = 4 )
                             ( key_field = 3 num1 = 4 num2 = 5 ) ) )
      ( key_field = 2 char1 = 'bbb'
        tab       = VALUE #( ( key_field = 4 num1 = 5 num2 = 6 )
                             ( key_field = 5 num1 = 6 num2 = 7 )
                             ( key_field = 6 num1 = 7 num2 = 8 ) ) ) ).

    itab_nested2 = VALUE #(
    ( key_field = 99  char2 = 'yyy' tab = VALUE #(
                                                   ( key_field = 10    char1 = 'aaa'
                                                     char2     = 'bbb' num1  = 100  num2 = 200 )
                                                   ( key_field = 20    char1 = 'ccc'
                                                     char2     = 'ddd' num1  = 300  num2 = 400 )
                                                   ( key_field = 30    char1 = 'eee'
                                                     char2     = 'fff' num1  = 500  num2 = 600 ) ) )
    ( key_field = 100 char2 = 'zzz' tab = VALUE #(
                                                   ( key_field = 40    char1 = 'ggg'
                                                     char2     = 'hhh' num1  = 700  num2 = 800 )
                                                   ( key_field = 50    char1 = 'iii'
                                                     char2     = 'jjj' num1  = 900  num2 = 1000 )
                                                   ( key_field = 60    char1 = 'kkk'
                                                     char2     = 'lll' num1  = 1100 num2 = 1200 ) ) ) ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: Internal Tables\n\n| ).
    out->write( |Filling and Copying Internal Table Content\n| ).
    out->write( |1) Adding single lines using APPEND/INSERT\n\n| ).

    "Two internal tables, a standard and sorted internal table.
    "Both have the same line type and one field as (non-)unique key.
    DATA it_st TYPE TABLE OF struc1 WITH NON-UNIQUE KEY a.
    DATA it_so TYPE SORTED TABLE OF struc1 WITH UNIQUE KEY a.

    "APPEND
    "Standard table
    APPEND VALUE #( a = 1 b = 'aaa' c = 'bbb' d = 'ccc' ) TO it_st.

    "A line is created and filled to be used for the APPEND statement.
    "The line type matches the line type of the internal table.
    DATA(line) = VALUE struc1( a = 2 b = 'd' c = 'e' d = 'f' ).

    "Sorted table
    "APPEND works here with a sorted table. At this stage, the
    "internal table is empty, so there is no issue (lines are only
    "appended if they match the sort order and do not create
    "duplicate entries if the primary table key is unique).
    APPEND line TO it_so.

    "INSERT
    "INSERT has same effect as APPEND with standard tables
    INSERT VALUE #( a = 2 b = 'ddd' c = 'eee' d = 'fff' )
      INTO TABLE it_st.

    INSERT VALUE #( a = 1 b = 'a' c = 'b' d = 'c' ) INTO TABLE it_so.

    out->write( data = it_st name = `it_st` ).
    out->write( |\n| ).
    out->write( data = it_so name = `it_so` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Adding initial line` ) ).

    APPEND INITIAL LINE TO it_st.

    INSERT INITIAL LINE INTO TABLE it_so.

    out->write( data = it_st name = `it_st` ).
    out->write( |\n| ).
    out->write( data = it_so name = `it_so` ).

**********************************************************************

out->write( zcl_demo_abap_aux=>heading( `3) Adding mutliple lines of an internal table to another one` ) ).

    "No additions: All lines are added to the target internal table
    APPEND LINES OF it_so TO it_st.

    "Creating a new itab and filling it.
    DATA it_so2 LIKE it_so.

    INSERT VALUE #( a = 3 b = 'g' c = 'h' d = 'i' ) INTO TABLE it_so2.

    INSERT VALUE #( a = 4 b = 'j' c = 'k' d = 'l' ) INTO TABLE it_so2.

    "Inserting all lines of previously created internal table.
    INSERT LINES OF it_so2 INTO TABLE it_so.

    out->write( data = it_st name = `it_st` ).
    out->write( |\n| ).
    out->write( data = it_so name = `it_so` ).

**********************************************************************

out->write( zcl_demo_abap_aux=>heading( `4) Adding lines of an internal table to another one by specifying the index range.` ) ).


    "When using only FROM, all lines are respected until the final
    "table entry. When using only TO, all lines are respected
    "starting from the first table entry.
    APPEND LINES OF it_so FROM 2 TO 3 TO it_st.

    INSERT LINES OF it_so FROM 3 INTO TABLE it_st.

    APPEND LINES OF it_so TO 2 TO it_st.

    out->write( data = it_st name = `it_st` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Inserting lines of an internal table into another one at a specific position` ) ).

    "Inserting a single line
    INSERT VALUE #( a = 10 b = 'ggg' c = 'hhh' d = 'iii' )
      INTO it_st INDEX 1.

    "Inserting multiple lines
    INSERT LINES OF it_so2 INTO it_st INDEX 2.

    out->write( data = it_st name = `it_st` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) Adding lines using constructor expressions` ) ).

    "Creating a line to be added to an internal table.
    line = VALUE #( a = 1 b = 'aaa' c = 'bbb' d = 'ccc' ).

    "Table on the right is constructed inline using VALUE and assigned
    "Note: This way, existing table content is cleared.
    it_st = VALUE #( (   line )
                     ( a = 2 b = 'ddd' c = 'eee' d = 'fff' ) ).

    out->write( data = it_st name = `it_st` ).

**********************************************************************

out->write( zcl_demo_abap_aux=>heading( `7) Creating a new table inline and adding lines using a constructor expression` ) ).

    "Internal table type
    TYPES it_type LIKE it_st.

    "Creating an internal table inline and filling in one go
    DATA(it_st2) = VALUE it_type( ( a = 3     b = 'ggg'
                                    c = 'hhh' d = 'iii' )
                                  ( a = 4     b = 'jjj'
                                    c = 'kkk' d = 'lll' ) ).

    out->write( data = it_st2 name = `it_st2` ).

**********************************************************************

  out->write( zcl_demo_abap_aux=>heading( `8) Adding lines using constructor expressions and keeping existing table content` ) ).

    "BASE addition: existing table content is not removed
    it_st = VALUE #( BASE it_st ( a = 5 b = 'mmm' c = 'nnn' d = 'ooo' )
                                ( a = 6 b = 'ppp' c = 'qqq' d = 'rrr' )
                   ).

    out->write( data = it_st name = `it_st` ).

**********************************************************************

out->write( zcl_demo_abap_aux=>heading( `9) Adding lines from other internal tables using constructor expressions` ) ).

    "With LINES OF itab specified within the pair of parentheses,
    "all lines of the internal table are added; here, in the same
    "expression another line is added as well
    it_st = VALUE #( BASE it_st ( LINES OF it_st2 )
                   ( a = 7 b = 'sss' c = 'ttt' d = 'uuu' )
                   ).

    out->write( data = it_st name = `it_st` ).

**********************************************************************

out->write( zcl_demo_abap_aux=>heading( `10) Copying table content (without constructor expression)` ) ).

    "Assignment of a table to another one having a matching line type
    it_st = it_st2.

    out->write( data = it_st name = `it_st` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) CORRESPONDING Operator and MOVE-CORRESPONDING` ) ).
    out->write( |Internal table content before assignments\n\n| ).

    "Note: Before the following statements, the table content is reset
    "to this state to work with the same set of values.
    fill_itabs_for_corresponding( ).

    out->write( data = tab1 name = `tab1` ).
    out->write( |\n| ).
    out->write( data = tab2 name = `tab2` ).
    out->write( |\n| ).
    out->write( data = tab3 name = `tab3` ).
    out->write( |\n| ).
    out->write( data = tab4 name = `tab4` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Copying content from another table that has a different line type ...` ) ).

    out->write( |12) ... and deleting existing table content using the CORRESPONDING operator\n\n| ).

    tab1 = CORRESPONDING #( tab2 ).

    out->write( data = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `13) ... and deleting existing table content using MOVE-CORRESPONDING` ) ).

    MOVE-CORRESPONDING tab2 TO tab1.

    out->write( data = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `14) ... and keeping existing table content using the CORRESPONDING operator` ) ).

    tab1 = CORRESPONDING #( BASE ( tab1 ) tab2 ).

    out->write( data = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

      out->write( zcl_demo_abap_aux=>heading( `15) ... and keeping existing table content using MOVE-CORRESPONDING` ) ).


    MOVE-CORRESPONDING tab2 TO tab1 KEEPING TARGET LINES.

    out->write( data = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

out->write( zcl_demo_abap_aux=>heading( `16) ... respecting component mapping` ) ).

    "Specifying components of a source table that are assigned to the
    "components of a target table in mapping relationships
    tab1 = CORRESPONDING #( tab2 MAPPING c = e d = f ).

    out->write( data = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `17) ... excluding components` ) ).

    "Excluding components from the assignment
    tab1 = CORRESPONDING #( tab2 EXCEPT b ).

    out->write( data = tab1 name = `tab1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `18) ... excluding components and using MAPPING` ) ).

    "EXCEPT * means that all components remain initial not specified
    "for mapping
    tab1 = CORRESPONDING #( tab2 MAPPING d = f EXCEPT * ).

    out->write( data = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `19) ... discarding duplicates` ) ).

    "Preventing runtime errors if duplicate lines are assigned to
    "target table that is defined to only accept unique keys.
    "Note: Other tables than above are used here.
    tab3 = CORRESPONDING #( BASE ( tab3 ) tab4 DISCARDING DUPLICATES ).

    out->write( data = tab3 name = `tab3` ).
    out->write( |\n| ).

    fill_itabs_for_corresponding( ).

    tab3 = CORRESPONDING #( BASE ( tab3 ) tab4 DISCARDING DUPLICATES
                            MAPPING d = f EXCEPT b ).

    out->write( data = tab3 name = `tab3` ).

**********************************************************************

out->write( zcl_demo_abap_aux=>heading( `20) Copying data from a deep internal table to another deep internal table` ) ).

    out->write( `Original table content` ).
    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = itab_nested1 name = `itab_nested1` ).
    out->write( |\n| ).
    out->write( data = itab_nested2 name = `itab_nested2` ).

**********************************************************************

      out->write( zcl_demo_abap_aux=>heading( `21) ... deleting existing content (CORRESPONDING operator)` ) ).

    itab_nested2 = CORRESPONDING #( DEEP itab_nested1 ).

    out->write( data = itab_nested2 name = `itab_nested2` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

      out->write( zcl_demo_abap_aux=>heading( `22) ... deleting existing content (MOVE-CORRESPONDING)` ) ).


    MOVE-CORRESPONDING itab_nested1 TO itab_nested2
     EXPANDING NESTED TABLES.

    out->write( data = itab_nested2 name = `itab_nested2` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

      out->write( zcl_demo_abap_aux=>heading( `23) ... keeping existing content (CORRESPONDING operator)` ) ).

    itab_nested2 = CORRESPONDING #( DEEP BASE ( itab_nested2 )
                                                itab_nested1 ).

    out->write( data = itab_nested2 name = `itab_nested2` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `24) ... keeping existing content (MOVE-CORRESPONDING)` ) ).

    MOVE-CORRESPONDING itab_nested1 TO itab_nested2
     EXPANDING NESTED TABLES KEEPING TARGET LINES.

    out->write( data = itab_nested2 name = `itab_nested2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Filling internal tables: Excursions` ) ).
    out->write( |25) Selecting multiple rows from a database table into an internal table\n\n| ).

    SELECT FROM zdemo_abap_tab1
      FIELDS key_field, char1, char2, num1, num2
      WHERE num1 > 3
      INTO TABLE @DATA(itab_select1).

    out->write( data = itab_select1 name = `itab_select1` ).

**********************************************************************

     out->write( zcl_demo_abap_aux=>heading(  `26) Sequentially adding multiple rows from a database table to an internal table` ) ).


    DATA itab TYPE TABLE OF zdemo_abap_tab1 WITH NON-UNIQUE KEY client key_field.

    SELECT FROM zdemo_abap_tab1
      FIELDS *
      WHERE num1 > 3
      INTO @DATA(struc_select).

      IF sy-subrc = 0.
        "Some modifications on the read lines (capitalizing letters)
        struc_select-char1 = to_upper( struc_select-char1 ).
        struc_select-char2 = to_upper( struc_select-char2 ).

        "Adding modified line to an internal table
        APPEND struc_select TO itab.
      ENDIF.
    ENDSELECT.

    out->write( data = itab name = `itab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `27) Adding multiple rows from a database table ` &&
      `to an internal table that has a different line type than the ` &&
      `database table and keeping existing table content` ) ).

    SELECT FROM zdemo_abap_tab2
      FIELDS *
      WHERE num1 > 10
      APPENDING CORRESPONDING FIELDS OF TABLE @itab.

    out->write( data = itab name = `itab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `28) Adding multiple rows from a database table ` &&
      `to an internal table that has a different line type than the ` &&
      `database table and deleting existing table content` ) ).

    SELECT FROM zdemo_abap_tab2
      FIELDS *
      WHERE num1 > 10
      INTO CORRESPONDING FIELDS OF TABLE @itab.

    out->write( data = itab name = `itab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `29) Adding multiple rows from an internal table ` &&
      `to an internal table using SELECT` ) ).

    SELECT key_field, char1, char2, num1, num2
      FROM @itab AS itab_alias
      INTO TABLE @DATA(itab_clone).

    out->write( data = itab_clone name = `itab_clone` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `30) Combining data of multiple tables into an` &&
      ` internal table using an inner join` ) ).

    "Filling table to be selected from
    itab =  VALUE #( ( key_field = 500 char1 = 'uuu' char2 = 'vvv'
                       num1      = 501 num2  = 502 )
                     ( key_field = 600 char1 = 'www' char2 = 'xxx'
                       num1      = 601 num2  = 602 ) ).

    "SELECT list includes fields from both tables
    "If there are no equivalent entries in the first or second table,
    "the rows are not joined.
    SELECT itab_alias1~key_field, itab_alias1~char2,
           zdemo_abap_tab2~numlong
      FROM @itab AS itab_alias1
      INNER JOIN zdemo_abap_tab2
        ON itab_alias1~key_field = zdemo_abap_tab2~key_field
      INTO TABLE @DATA(join_result).

    out->write( data = join_result name = `join_result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `31) Filling internal table ` &&
      `using a subquery (1)` ) ).

    "A subquery is specified in the WHERE clause
    "Here, data is selected from a database table depending on
    "whether the value of a certain field is not among the
    "values specified in parentheses.
    SELECT key_field, char1, numlong
      FROM zdemo_abap_tab2
      WHERE char1 NOT IN ( 'iii', 'mmm', 'ooo', 'ppp' )
      INTO TABLE @DATA(subquery_result1).

    out->write( data = subquery_result1 name = `subquery_result1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `32) Filling internal table ` &&
      `using a subquery (2)` ) ).

    "A subquery using EXISTS in the WHERE clause.
    "In the example, data is selected from a database table depending
    "on the existence of data in an internal table. Only if a line
    "with a matching value of the specified field exists in both
    "database and internal table, data is read.
    SELECT key_field, numlong
      FROM zdemo_abap_tab2
      WHERE EXISTS
         ( SELECT 'X' FROM @itab AS itab_alias2
           WHERE key_field = zdemo_abap_tab2~key_field )
      INTO TABLE @DATA(subquery_result2).

    out->write( data = subquery_result2 name = `subquery_result2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `33) Filling an internal table from a table ` &&
      `depending on the existence of data in another internal table ` &&
      `using the addition FOR ALL ENTRIES` ) ).

    "In the example, data is selected from a database table depending
    "on the existence of data in an internal table. Only if a line
    "with a matching value of the specified field exists in both
    "database and internal table, data is read.
    "Ensure that the internal table from which to read is not initial.
    IF ( 0 < lines( itab ) ).
      SELECT key_field, char1, numlong
        FROM zdemo_abap_tab2
        FOR ALL ENTRIES IN @itab
        WHERE key_field = @itab-key_field
        INTO TABLE @DATA(select_result).
    ENDIF.

    out->write( data = select_result name = `select_result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `34) Adding content from a database to internal` &&
      ` table by using alias names in the SELECT list` ) ).

    DATA itab2 TYPE TABLE OF zdemo_abap_tab2 WITH EMPTY KEY.

    "Specifying alias names can help fill an existing internal
    "table that has not a matching line type to the database table.
    "Here, two fields are specified with an alias name to match the
    "names of components contained in the existing internal table.
    "The individual types of the fields match, too.
    SELECT key_field, char2 AS char1, num2 AS num1
        FROM zdemo_abap_tab1
      INTO CORRESPONDING FIELDS OF TABLE @itab2 UP TO 3 ROWS.

    out->write( data = itab2 name = `itab2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `35) FILTER: Filtering internal table by condition` ) ).

    "This section covers multiple examples demonstrating the syntactical variety
    "of the FILTER operator.

    TYPES: BEGIN OF fi_str,
             a TYPE i,
             b TYPE c LENGTH 3,
             c TYPE c LENGTH 3,
           END OF fi_str.

    "basic form, condition created with single values
    "itab must have at least one sorted key or one hash key used for access.
    "This variant of the filter operator is not possible for an internal table itab without a sorted key or hash key.
    DATA fi_tab1 TYPE SORTED TABLE OF fi_str WITH NON-UNIQUE KEY a.
    DATA fi_tab2 TYPE STANDARD TABLE OF fi_str WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS a.
    DATA fi_tab3 TYPE HASHED TABLE OF fi_str WITH UNIQUE KEY a.

    "Filling internal tables
    fi_tab1 = VALUE #( ( a = 1 b = 'aaa' c = 'abc' )
                       ( a = 2 b = 'bbb' c = 'def' )
                       ( a = 3 b = 'ccc' c = 'hij' )
                       ( a = 4 b = 'ddd' c = 'klm' )
                       ( a = 5 b = 'eee' c = 'nop' ) ).

    fi_tab2 = fi_tab1.
    fi_tab3 = fi_tab1.

    "The lines meeting the condition are respected.
    "Note: The source table must have at least one sorted or hashed key.
    "Here, the primary key is used
    DATA(f1) = FILTER #( fi_tab1 WHERE a >= 3 ).

    out->write( data = f1 name = `f1` ).
    out->write( |\n| ).

    "USING KEY primary_key explicitly specified; same as above
    DATA(f2) = FILTER #( fi_tab1 USING KEY primary_key WHERE a >= 3 ).

    out->write( data = f2 name = `f2` ).
    out->write( |\n| ).

    "EXCEPT addition
    DATA(f3) = FILTER #( fi_tab1 EXCEPT WHERE a >= 3 ).

    out->write( data = f3 name = `f3` ).
    out->write( |\n| ).

    DATA(f4) = FILTER #( fi_tab1 EXCEPT USING KEY primary_key WHERE a >= 3 ).

    out->write( data = f4 name = `f4` ).
    out->write( |\n| ).

    "Secondary table key specified after USING KEY
    DATA(f5) = FILTER #( fi_tab2 USING KEY sec_key WHERE a >= 4 ).

    out->write( data = f5 name = `f5` ).
    out->write( |\n| ).

    DATA(f6) = FILTER #( fi_tab2 EXCEPT USING KEY sec_key WHERE a >= 3 ).

    out->write( data = f6 name = `f6` ).
    out->write( |\n| ).

    "Note: In case of a hash key, exactly one comparison expression for each key
    "component is allowed; only = as comparison operator possible.
    DATA(f7) = FILTER #( fi_tab3 WHERE a = 3 ).

    out->write( data = f7 name = `f7` ).
    out->write( |\n| ).

    "Using a filter table
    "In the WHERE condition, the columns of source and filter table are compared.
    "Those lines in the source table are used for which at least one line in the
    "filter table meets the condition. EXCEPT and USING KEY are also possible.

    "Declaring and filling filter tables
    DATA filter_tab1 TYPE SORTED TABLE OF i
      WITH NON-UNIQUE KEY table_line.

    DATA filter_tab2 TYPE STANDARD TABLE OF i
        WITH EMPTY KEY
        WITH UNIQUE SORTED KEY line COMPONENTS table_line.

    filter_tab1 = VALUE #( ( 3 ) ( 5 ) ).
    filter_tab2 = filter_tab1.

    DATA(f8) = FILTER #( fi_tab1 IN filter_tab1 WHERE a = table_line ).

    out->write( data = f8 name = `f8` ).
    out->write( |\n| ).

    "EXCEPT addition
    DATA(f9) = FILTER #( fi_tab1 EXCEPT IN filter_tab1 WHERE a = table_line ).

    out->write( data = f9 name = `f9` ).
    out->write( |\n| ).

    "USING KEY is specified for the filter table
    DATA(f10) = FILTER #( fi_tab2 IN filter_tab2 USING KEY line WHERE a = table_line ).

    out->write( data = f10 name = `f10` ).
    out->write( |\n| ).

    "USING KEY is specified for the source table, including EXCEPT
    DATA(f11) = FILTER #( fi_tab2 USING KEY sec_key EXCEPT IN filter_tab2 WHERE a = table_line ).

    out->write( data = f11 name = `f11` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `36) Inserting data into an internal table ` &&
      `using a COLLECT statement` ) ).

    "Internal table to work with
    DATA itab_num TYPE SORTED TABLE OF l_type2
       WITH UNIQUE KEY key_field.

    itab_num = VALUE #( ( key_field = 1 num1 = 2 num2 = 3 )
                        ( key_field = 2 num1 = 4 num2 = 5 )
                        ( key_field = 3 num1 = 6 num2 = 7 ) ).

    "Values of numeric components are added to the
    "corresponding values in an internal table
    COLLECT VALUE l_type2( key_field = 1 num1 = 10 num2 = 10 )
      INTO itab_num.

    out->write( data = itab_num name = `itab_num` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `37) Reading from internal tables` ) ).

    "Filling internal tables
    it_st = VALUE #( ( a = 1 b = 'aaa' c = 'bbb' d = 'ccc' )
                     ( a = 2 b = 'ddd' c = 'eee' d = 'fff' )
                     ( a = 3 b = 'ggg' c = 'hhh' d = 'iii' )
                     ( a = 4 b = 'jjj' c = 'kkk' d = 'lll' ) ).

    "Declaring demo sorted/hashed tables having primary and
    "secondary keys as well as alias names defined
    DATA it_so_sec TYPE SORTED TABLE OF struc1
      WITH NON-UNIQUE KEY primary_key ALIAS pk COMPONENTS a
      WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS b.

    DATA it_ha_sec TYPE HASHED TABLE OF struc1
      WITH UNIQUE KEY primary_key ALIAS pkh COMPONENTS a
      WITH NON-UNIQUE SORTED KEY sec_key_h ALIAS skh COMPONENTS b.

    "Filling internal table
    it_so_sec = VALUE #( ( a = 1 b = 'bbb' c = '###' d = '###' )
                         ( a = 2 b = 'ccc' c = '###' d = '###' )
                         ( a = 3 b = 'aaa' c = 'zzz' d = '###' )
                         ( a = 4 b = 'ddd' c = '###' d = '###' ) ).

    "Filling internal table with the content above
    it_ha_sec = it_so_sec.

    out->write( `Original table content` ).
    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = it_so_sec name = `it_so_sec` ).
    out->write( |\n| ).
    out->write( data = it_ha_sec name = `it_ha_sec` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `38) Reading a single line into target area` ) ).

    "The examples anticipate the reading of a line by index since the
    "syntax requires to specify the reading via index or key. Both
    "inline declarations and existing target areas are demonstrated.

    "Work area
    READ TABLE it_so_sec INTO DATA(wa1) INDEX 1.
    DATA wa2 LIKE LINE OF it_so_sec.

    "The addition TRANSPORTING specifies which components are to be
    "respected for the copying. If it is not specified, all components
    "are respected.
    READ TABLE it_so_sec INTO wa2 INDEX 2 TRANSPORTING a b c.

    "Field symbol
    READ TABLE it_so_sec ASSIGNING FIELD-SYMBOL(<fs>) INDEX 3.

    FIELD-SYMBOLS <fs2> LIKE LINE OF it_so_sec.
    READ TABLE it_st ASSIGNING <fs2> INDEX 1.

    "Data reference variable
    READ TABLE it_so_sec REFERENCE INTO DATA(dref) INDEX 4.

    DATA dref2 LIKE REF TO wa2.
    READ TABLE it_so_sec REFERENCE INTO dref2 INDEX 2.

    out->write( data = wa1 name = `wa1` ).
    out->write( |\n| ).
    out->write( data = wa2 name = `wa2` ).
    out->write( |\n| ).
    out->write( data = <fs> name = `<fs>` ).
    out->write( |\n| ).
    out->write( data = <fs2> name = `<fs2>` ).
    out->write( |\n| ).
    out->write( data = dref->* name = `dref->*` ).
    out->write( |\n| ).
    out->write( data = dref2->* name = `dref2->*` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Reading a single line via index ...` ) ).
    out->write( |39) ... using READ TABLE\n\n| ).

    "Primary table index used implicitly
    READ TABLE it_so_sec INTO DATA(wa3) INDEX 1.

    "Primary table index used implicitly; result here: same as above
    READ TABLE it_so_sec INTO DATA(wa4) INDEX 1 USING KEY primary_key.

    "Primary table key alias; result here: same as above
    READ TABLE it_so_sec INTO DATA(wa5) INDEX 1 USING KEY pk.

    "Secondary table key; secondary table index used
    READ TABLE it_so_sec INTO DATA(wa6) INDEX 1 USING KEY sec_key.

    "Secondary table key alias; secondary table index used
    "result here: same as above
    READ TABLE it_so_sec INTO DATA(wa7) INDEX 1 USING KEY sk.

    "Index access for hashed tables using secondary table index
    READ TABLE it_ha_sec INTO DATA(wa8) INDEX 1 USING KEY sec_key_h.

    out->write( data = wa3 name = `wa3` ).
    out->write( |\n| ).
    out->write( data = wa4 name = `wa4` ).
    out->write( |\n| ).
    out->write( data = wa5 name = `wa5` ).
    out->write( |\n| ).
    out->write( data = wa6 name = `wa6` ).
    out->write( |\n| ).
    out->write( data = wa7 name = `wa7` ).
    out->write( |\n| ).
    out->write( data = wa8 name = `wa8` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `40) ... table expressions (1)` ) ).

    "Reading via index; primary table index is used implicitly
    DATA(lv1) = it_so_sec[ 2 ].

    "Note: A line that is not found results in an runtime error.
    DATA(idx) = 10.

    TRY.
        DATA(lv2) = it_so_sec[ idx ].
      CATCH cx_sy_itab_line_not_found.
        DATA(error) = |Line with index { idx } does not exist.|.
    ENDTRY.

    "Reading via index and specifying the table index (via the key)
    "to be read from
    DATA(lv3) = it_so_sec[ KEY primary_key INDEX 1 ].

    DATA(lv4) = it_so_sec[ KEY sec_key INDEX 4 ].

    "Hashed table example (secondary table index)
    DATA(lv5) = it_ha_sec[ KEY sec_key_h INDEX 3 ].

    out->write( data = lv1 name = `lv1` ).
    out->write( |\n| ).

    IF lv2 IS NOT INITIAL.
      out->write( data = lv2 name = `lv2` ).
      out->write( |\n| ).
    ENDIF.

    IF error IS NOT INITIAL.
      out->write( data = error name = `error` ).
      out->write( |\n| ).
    ENDIF.

    out->write( data = lv3 name = `lv3` ).
    out->write( |\n| ).
    out->write( data = lv4 name = `lv4` ).
    out->write( |\n| ).
    out->write( data = lv5 name = `lv5` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `41) ... table expressions (2)` ) ).

    "Copying a table line via table expression and embedding in
    "a constructor expression
    DATA(lv6) = VALUE #( it_so_sec[ 2 ] ).

    "Reading into data reference variable using the REF operator
    DATA(dref3) = REF #( it_so_sec[ 4 ] ).

    "OPTIONAL/DEFAULT additions: An unsuccessful reading operation
    "does not raise the exception; returns either an initial or
    "default line in case of an unsuccessful reading operation
    DATA(lv7) = VALUE #( it_so_sec[ 10 ] OPTIONAL ).

    DATA(lv8) = VALUE #( it_so_sec[ 10 ] DEFAULT it_so_sec[ 2 ] ).

    out->write( data = lv6 name = `lv6` ).
    out->write( |\n| ).
    out->write( data = dref3->* name = `dref3->*` ).
    out->write( |\n| ).
    out->write( data = lv7 name = `lv7` ).
    out->write( |\n| ).
    out->write( data = lv8 name = `lv8` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Reading a single line via table keys ...` ) ).
    out->write( |42) ... using READ TABLE (1)\n| ).

    "Primary table key (COMPONENTS addition is optional)
    READ TABLE it_so_sec INTO DATA(wa9)
      WITH TABLE KEY primary_key COMPONENTS a = 1.

    READ TABLE it_so_sec INTO DATA(wa10) WITH TABLE KEY a = 2.

    "Primary table key alias
    READ TABLE it_so_sec INTO DATA(wa11)
      WITH TABLE KEY pk COMPONENTS a = 3.

    "Secondary table key
    READ TABLE it_so_sec INTO DATA(wa12)
      WITH TABLE KEY sec_key COMPONENTS b = 'ddd'.

    "Secondary table key alias
    READ TABLE it_so_sec INTO DATA(wa13)
      WITH TABLE KEY sk COMPONENTS b = 'ccc'.

    out->write( data = wa9 name = `wa9` ).
    out->write( |\n| ).
    out->write( data = wa10 name = `wa10` ).
    out->write( |\n| ).
    out->write( data = wa11 name = `wa11` ).
    out->write( |\n| ).
    out->write( data = wa12 name = `wa12` ).
    out->write( |\n| ).
    out->write( data = wa13 name = `wa13` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `43) ... using READ TABLE (2)` ) ).

    "Reading a line based on keys specified in a work area
    "Here, the work area contains primary and secondary key values.
    "The line type is compatible to the internal table.
    DATA(pr_keys) = VALUE struc1( a = 2 ).

    DATA(sec_keys) = VALUE struc1( b = 'aaa' ).

    "Primary table key is used implicitly
    READ TABLE it_so_sec FROM pr_keys INTO DATA(wa14).

    "If USING KEY is not specified, the primary table key is used.
    "If it is used, the specified table key is used.
    "Secondary table key
    READ TABLE it_so_sec FROM sec_keys
      USING KEY sec_key INTO DATA(wa15).

    "Primary table key; result: same as wa14
    READ TABLE it_so_sec FROM pr_keys
      USING KEY primary_key INTO DATA(wa16).

    out->write( data = wa14 name = `wa14` ).
    out->write( |\n| ).
    out->write( data = wa15 name = `wa15` ).
    out->write( |\n| ).
    out->write( data = wa16 name = `wa16` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `44) ... using table expressions` ) ).
    "Primary table key (COMPONENTS addition is optional)
    DATA(lv9) = it_so_sec[ KEY primary_key COMPONENTS a = 1 ].

    DATA(lv10) = it_so_sec[ KEY primary_key a = 1 ].

    DATA(lv11) = it_so_sec[ KEY pk a = 2 ]. "Primary table key alias

    "Secondary table key (COMPONENTS mandatory)
    DATA(lv12) = it_so_sec[ KEY sec_key COMPONENTS b = 'aaa' ].

    DATA(lv13) = it_so_sec[ KEY sk COMPONENTS b = 'ddd' ]. "Alias

    out->write( data = lv9 name = `lv9` ).
    out->write( |\n| ).
    out->write( data = lv10 name = `lv10` ).
    out->write( |\n| ).
    out->write( data = lv11 name = `lv11` ).
    out->write( |\n| ).
    out->write( data = lv12 name = `lv12` ).
    out->write( |\n| ).
    out->write( data = lv13 name = `lv13` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `45) Reading a single line via free key` ) ).
    "Note: If there a multiple matching entries, the first found
    "is returned.
    READ TABLE it_so_sec INTO DATA(wa17) WITH KEY c = '###'.

    DATA(lv14) = it_so_sec[ c = 'zzz' ].

    out->write( data = wa17 name = `wa17` ).
    out->write( |\n| ).
    out->write( data = lv14 name = `lv14` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `46) Excursion: Addressing individual components` ) ).
    "Addressing a component using the component selector
    DATA(comp1) = it_so_sec[ 1 ]-b.

    READ TABLE it_so_sec ASSIGNING FIELD-SYMBOL(<fs3>) INDEX 2.

    DATA(comp2) = <fs3>-c.

    READ TABLE it_so_sec REFERENCE INTO DATA(dref4) INDEX 3.

    DATA(comp3) = dref->*-a.

    "Same effect as above but less to write
    DATA(comp4) = dref->b.

    out->write( data = comp1 name = `comp1` ).
    out->write( |\n| ).
    out->write( data = comp2 name = `comp2` ).
    out->write( |\n| ).
    out->write( data = comp3 name = `comp3` ).
    out->write( |\n| ).
    out->write( data = comp4 name = `comp4` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `47) Checking if a line exists in an internal table` ) ).

    "Defining the key
    DATA(key1) = 2.

    "Internal table function
    IF line_exists( it_so_sec[ a = key1 ] ).
      out->write( |Line { key1 } exists in internal table.| ).
    ELSE.
      out->write( |Line { key1 } does not exist in internal table.| ).
    ENDIF.

    out->write( |\n| ).

    "Alternative using READ TABLE (sy-subrc is checked)
    "When using the addition TRANSPORTING NO FIELDS, no field values
    "are read. Only the system fields are filled.
    READ TABLE it_so_sec WITH KEY a = key1 TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      out->write( |Line { key1 } exists in internal table.| ).
    ELSE.
      out->write( |Line { key1 } does not exist in internal table.| ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `48) Checking the index of a ` &&
      `specific line` ) ).

    DATA(key2) = 4.

    DATA(idx_of_line1) = line_index( it_so_sec[ a = key2 ] ).

    DATA(key3) = 10.

    DATA(idx_of_line2) = line_index( it_so_sec[ a = key3 ] ).

    "Alternative using READ TABLE
    "The table index is written to the sy-tabix system field
    READ TABLE it_so_sec WITH KEY a = key2 TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      DATA(tab_idx1) = sy-tabix.
    ENDIF.

    READ TABLE it_so_sec WITH KEY a = key3 TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      DATA(tab_idx2) = sy-tabix.
    ENDIF.

    IF idx_of_line1 <> 0.
      out->write( |The index of the line with key = { key2 } | &&
                    |is { idx_of_line1 } in the internal table.| ).
    ELSE.
      out->write( |The line with key = { key2 } does not exist | &&
                    |in the internal table.| ).
    ENDIF.

    out->write( |\n| ).

    IF idx_of_line2 <> 0.
      out->write( |The index of the line with key = { key3 } | &&
                    |is { idx_of_line2 } in the internal table.| ).
    ELSE.
      out->write( |The line with key = { key3 } does not exist | &&
                    |in the internal table.| ).
    ENDIF.

  out->write( |\n| ).

    IF tab_idx1 <> 0.
      out->write( |The index of the line with key = { key2 }  | &&
                    |is { tab_idx1 } in the internal table.| ).
    ELSE.
      out->write( |The line with key = { key2 } does not exist | &&
                    |in the internal table.| ).
    ENDIF.

out->write( |\n| ).

    IF tab_idx2 <> 0.
      out->write( |The index of the line with key = { key3 } | &&
                    |is { tab_idx2 } in the internal table.| ).
    ELSE.
      out->write( |The line with key = { key3 } does not exist | &&
                    |in the internal table.| ).
    ENDIF.

out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `49) Checking how many lines are in an` &&
      ` internal table` ) ).
    DATA(itab_lines) = lines( it_so_sec ).

    out->write( |The internal table consists of { itab_lines } lines.| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Processing multiple internal table lines ` &&
      `sequentially` ) ).
    out->write( |50) Reading a complete table by sequentially reading all lines\n\n| ).

    "No further addition: All lines are respected.
    LOOP AT it_so_sec ASSIGNING FIELD-SYMBOL(<fs4>).
      "Modifying a component to visualize the reading of all lines.
      <fs4>-b = 'ZZZ'.
    ENDLOOP.

    out->write( data = it_so_sec name = `it_so_sec` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading(  `51) LOOP AT statements with different targets` ) ).

    "The following examples demonstrate the different targets that
    "are possible for LOOP AT statements. In the example above,
    "a field symbol is created inline.
    "As above, there are no additions to the loop statement, i.e. all lines
    "are processed.

    DATA(lines_in_table) = lines( it_so_sec ).
    out->write( |There should be { lines_in_table } iterations per loop.| ).
    out->write( |\n| ).

    "Target: Existing work area
    out->write( `---- Loop target: Existing work area ----` ).
    out->write( |\n| ).
    DATA wa_lo LIKE LINE OF it_so_sec.

    LOOP AT it_so_sec INTO wa_lo.
      IF sy-tabix = 1.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.
    ENDLOOP.
    out->write( |\n| ).
    out->write( `---- Loop target: Work area created inline ----` ).
    out->write( |\n| ).
    LOOP AT it_so_sec INTO DATA(wa_inl).
      IF sy-tabix = 1.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.
    ENDLOOP.
    out->write( |\n| ).
    out->write( `---- Loop target: Existing field symbol ----` ).
    out->write( |\n| ).
    FIELD-SYMBOLS <fs_lo> LIKE LINE OF it_so_sec.

    LOOP AT it_so_sec ASSIGNING <fs>.
      IF sy-tabix = 1.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.
    ENDLOOP.
    out->write( |\n| ).
    out->write( `---- Loop target: Field symbol created inline ----` ).
    out->write( |\n| ).
    LOOP AT it_so_sec ASSIGNING FIELD-SYMBOL(<fs_inl>).
      IF sy-tabix = 1.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.
    ENDLOOP.
    out->write( |\n| ).
    out->write( `---- Loop target: Existing data reference variable ----` ).
    out->write( |\n| ).
    DATA dref_lo TYPE REF TO struc1 .

    LOOP AT it_so_sec REFERENCE INTO dref_lo.
      IF sy-tabix = 1.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.
    ENDLOOP.
    out->write( |\n| ).
    out->write( `---- Loop target: Data reference variable created inline ----` ).
    out->write( |\n| ).
    LOOP AT it_so_sec REFERENCE INTO DATA(dref_inl).
      IF sy-tabix = 1.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        out->write( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `52) Reading multiple lines by an index range` ) ).

    "Specific lines in an index range are respected
    "Note: FROM/TO alone can specified, too.
    LOOP AT it_so_sec ASSIGNING FIELD-SYMBOL(<fs5>) FROM 2 TO 3.
      "Modifying a component to visualize the reading of specific lines.
      <fs5>-c = 'YYY'.
    ENDLOOP.

    out->write( data = it_so_sec name = `it_so_sec` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `53) Reading multiple lines by condition` ) ).

    LOOP AT it_so_sec ASSIGNING FIELD-SYMBOL(<fs6>) WHERE a < 3.
      "Modifying a component to visualize the reading of specific lines.
      <fs6>-d = 'XXX'.
    ENDLOOP.

    out->write( data = it_so_sec name = `it_so_sec` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `54) Looping across a table without an interest` &&
      ` in the table content` ) ).

    "Here, only the system fields are set.
    LOOP AT it_so_sec TRANSPORTING NO FIELDS WHERE a < 3.
      DATA(num) = sy-tabix.
    ENDLOOP.

    out->write( |There are { num } lines in the table fulfilling the condition.| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `55) Loop with table key specification` ) ).

    DATA it_st_em TYPE TABLE OF struc1 WITH EMPTY KEY.

    "Looping across hashed table using a secondary key. The loop starts
    "according to the secondary table index. The lines are added to
    "another internal table having a matching type. It basically
    "visualizes the order of the table lines in the secondary table
    "index.
    LOOP AT it_ha_sec ASSIGNING FIELD-SYMBOL(<fs7>) USING KEY sec_key_h.
      APPEND <fs7> TO it_st_em.
    ENDLOOP.

    out->write( data = it_st_em name = `it_st_em` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `STEP addition in LOOP AT statements` ) ).
    out->write( |56) Reversing loop order\n\n| ).

    DATA(it_abc) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ( `d` ) ( `e` ) ( `f` ) ).
    DATA it_abc_result TYPE string_table.

    "Reversing the loop order with the addition STEP
    "Each line is read indicated by absolute value 1
    LOOP AT it_abc ASSIGNING FIELD-SYMBOL(<step>) STEP -1.
      APPEND <step> TO it_abc_result.
    ENDLOOP.

    out->write( data = it_abc_result name = `it_abc_result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `57) Forward loop and defining step size` ) ).

    "Forward loop indicated by positive integer, every second line is processed
    CLEAR it_abc_result.
    LOOP AT it_abc ASSIGNING FIELD-SYMBOL(<sec>) STEP 2.
      APPEND <sec> TO it_abc_result.
    ENDLOOP.

    out->write( data = it_abc_result name = `it_abc_result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `58) STEP addition combined with FROM/TO` ) ).

    "Combining the STEP addition with other additions, e.g. FROM and TO
    "Note: If the value after STEP is negative, the value after FROM
    "must be greater than the value after TO.
    CLEAR it_abc_result.
    LOOP AT it_abc ASSIGNING FIELD-SYMBOL(<from_to>) FROM 6 TO 3 STEP -2.
      APPEND <from_to> TO it_abc_result.
    ENDLOOP.

    out->write( data = it_abc_result name = `it_abc_result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Creating and filling tables using table ` &&
      `iterations with FOR and VALUE` ) ).
    out->write( |59) Retrieving values of one column in an internal table.\n\n| ).

    "Creating internal table type
    TYPES ty_numbers TYPE TABLE OF i WITH EMPTY KEY.

    "Table comprehension: Content of an internal table is created by
    "evaluating a table using a table iteration with an iteration
    "expressions within a constructor expression.
    DATA(lv_num_a) = VALUE ty_numbers( FOR ls1 IN it_ha_sec
                                       ( ls1-a ) ).

    out->write( data = lv_num_a name = `lv_num_a` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `60) Retrieving values of one column in ` &&
      `an internal table based on conditions` ) ).

    DATA(lv_num_b) = VALUE ty_numbers( FOR ls2 IN it_ha_sec
                                       WHERE ( a < 3 ) ( ls2-a ) ).

    out->write( data = lv_num_b name = `lv_num_b` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `61) Looping across 2 tables ` &&
      `and retrieving values based on conditions` ) ).
    "Internal table type
    TYPES tabtype LIKE it_so_sec.

    DATA(itab_for_2tab) =
      VALUE tabtype(
                     FOR ls3 IN it_ha_sec
                     FOR ls4 IN it_so_sec WHERE ( a = ls3-a )
                                                ( a = ls3-a b = ls4-b c = ls3-c d = ls4-d ) ).

    out->write( data = itab_for_2tab name = `itab_for_2tab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `62) Retrieving and changing values from an ` &&
      `internal tables sequentially` ) ).
    DATA(it_changed) = VALUE tabtype( FOR ls5 IN it_so_sec
                                      ( a = ls5-a b = 'WWW' c = 'VVV' d = 'UUU' ) ).

    out->write( data = it_changed name = `it_changed` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `63) Sorting internal tables` ) ).

    "Creating structured data types
    TYPES: BEGIN OF s1,
             a TYPE i,
             b TYPE string,
             c TYPE c LENGTH 1,
             d TYPE i,
           END OF s1.

    TYPES: BEGIN OF s2,
             a TYPE i,
             b TYPE i,
           END OF s2.

    "Creating internal tables
    DATA it1 TYPE TABLE OF s1 WITH NON-UNIQUE KEY a.
    DATA it2 TYPE TABLE OF s1 WITH DEFAULT KEY.

    "Filling internal tables
    it1 = VALUE #( ( a = 1 b = `c` c = 'z' d = 4 )
                   ( a = 3 b = `b` c = 'f' d = 3 )
                   ( a = 2 b = `d` c = 'r' d = 9 )
                   ( a = 4 b = `a` c = 'p' d = 3 )
                   ( a = 5 b = `b` c = 'x' d = 2 )
                   ( a = 5 b = `a` c = 'x' d = 0 )
                   ( a = 1 b = `c` c = 'y' d = 8 ) ).

    it2 = it1.

    out->write( `Original internal table content ` &&
      `(it1 and it2 have the same content)` ).
    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = it1 name = `it1` ).
    out->write( |\n| ).
    out->write( data = it2 name = `it2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `64) Sorting by primary table key` ) ).

    "Primary key: component a
    SORT it1.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `65) Sorting by primary table key in ascending` &&
      ` order` ) ).

    "The sorting result is the same as above (where ASCENDING is used
    "implicitly). Here, it is explicitly specified.
    SORT it1 ASCENDING.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `66) Sorting by primary table key respecting all ` &&
      `non-numeric fields` ) ).

    "Primary key: standard table key (all non-numeric fields)
    SORT it2.

    out->write( data = it2 name = `it2` ).

    "The following code is commented out on purpose because it
    "produces a syntax warning. The primary table key is empty.
    "A sorting has no effect.
    "SORT it3.
    "out->write( data = it3 name = `it3` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `67) Sorting by primary table key in ` &&
      `descending order` ) ).

    "Sorting in descending order and by primary table key
    SORT it1 DESCENDING.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `68) Sorting by explicitly specified component (1)` ) ).
    "Here, the component is the primary table key.
    "The sorting result is the same as above.
    SORT it1 BY a DESCENDING.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `69) Sorting by explicitly specified component (2)` ) ).

    "Sorting by arbitrary, non-key field
    SORT it1 BY d DESCENDING.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `70) Sorting by multiple explicitly specified` &&
      ` components` ) ).

    "Sorting by multiple components and specifying the sort order
    SORT it1 BY b ASCENDING c DESCENDING.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `71) Sorting by respecting the values of all` &&
      ` components` ) ).

    "Sorting by considering the values of each field of the table line
    SORT it1 BY table_line.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `72) Modifying internal table content` ) ).
    out->write( |Internal table content before modifications\n| ).
    out->write( |\n| ).

    "Standard table
    out->write( data = it_st name = `it_st` ).
    out->write( |\n| ).

    "Sorted table
    out->write( data = it_so_sec name = `it_so_sec` ).
    out->write( |\n| ).

    "Hashed table
    out->write( data = it_ha_sec name = `it_ha_sec` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `73) Directly modifying recently read table lines` ) ).

    "READ TABLE
    "Reading table line into target area (field symbol)
    READ TABLE it_so_sec ASSIGNING FIELD-SYMBOL(<fs9>) INDEX 1.
    "Directly modifying an individual component value and
    "the entire line (except the key values in sorted/hashed tables)
    <fs9>-c = 'ABC'.
    <fs9> = VALUE #( BASE <fs9> d = 'DEF' ).

    "Table expressions
    it_st[ 1 ]-c = 'GHI'. "Individual component
    it_st[ 1 ] = VALUE #( BASE it_st[ 1 ] b = 'JKL' d = 'MNO' ).

    out->write( data = it_so_sec[ 1 ] name = `it_so_sec[ 1 ]` ).
    out->write( |\n| ).
    out->write( data = it_st[ 1 ] name = `it_st[ 1 ]` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `74) Modifying internal table content using MODIFY` ) ).
    "Modifying table lines via key values
    "Line that is used to modify internal table
    line = VALUE #( a = 2 b = 'zzz' c = 'yyy' ).

    "Standard table
    "With the addition FROM wa, the key values in wa determine the line
    "to be modified.
    "Note: Component d is not specified in "line". The value is
    "initialized.
    MODIFY TABLE it_st FROM line.

    "Example in which the work area is constructed inline.
    "Components b and c not specified. The values are initialized.
    MODIFY TABLE it_st FROM VALUE #( a = 3 d = 'xxx' ).

    "Addition TRANSPORTING: Only specified fields are respected
    "Note: In case of sorted/hasehd tables, key values cannot be
    "specified.
    MODIFY TABLE it_st
      FROM VALUE #( a = 4 b = '###' c = '###' d = '###' )
      TRANSPORTING b c.

    "Modifying table lines via index
    "Note: It is only MODIFY, not MODIFY TABLE as above.
    "The following statement modifies the line with number 1 in the
    "primary table index. Without the addition TRANSPORTING, the
    "entire line is changed.
    MODIFY it_st
      FROM VALUE #( a = 1 b = 'aaa' c = 'aaa' d = 'aaa' )
      INDEX 1.

    "USING KEY: Determines the table key and thus which table index
    "to respect
    MODIFY it_so_sec
      FROM VALUE #( a = 1 b = 'EEE' c = 'EEE' d = 'EEE' )
      INDEX 1
      USING KEY primary_key
      TRANSPORTING c d.

    "Note: Without TRANSPORTING, the statement would overwrite the
    "secondary key which is not allowed.
    MODIFY it_ha_sec
      FROM VALUE #( a = 1 b = 'FFF' c = 'FFF' d = 'FFF' )
      INDEX 1
      USING KEY sec_key_h
      TRANSPORTING d.

    out->write( data = it_st name = `it_st` ).
    out->write( |\n| ).
    out->write( data = it_so_sec name = `it_so_sec` ).
    out->write( |\n| ).
    out->write( data = it_ha_sec name = `it_ha_sec` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `75) Deleting internal table content using DELETE` ) ).
    "Deleting via index
    "Primary table index is used implicitly.
    DELETE it_st INDEX 1.

    "If USING KEY is not used, INDEX can only be used with index
    "tables. If a secondary key is specified, the secondary table
    "index is respected.
    "The following example has the same effect as above.
    DELETE it_st INDEX 1 USING KEY primary_key.

    "Hashed table. The secondary table index is respected.
    DELETE it_ha_sec INDEX 1 USING KEY sec_key_h.

    "Deleting multiple lines by specifying an index range
    "FROM or TO alone can also be specified
    DELETE it_so_sec FROM 2 TO 3.

    "Deleting via keys
    "When using the addition FROM wa, the line wa must have a
    "compatible type to the table's line type and include key values.
    "The first found line with the corresponding keys is deleted.
    "If the key is empty, no line is deleted.
    DELETE TABLE it_so_sec FROM VALUE #( a = 4 ).

    "Explicitly specifying the table key
    DELETE TABLE it_so_sec WITH TABLE KEY a = 1.

    DELETE TABLE it_ha_sec
      WITH TABLE KEY sec_key_h COMPONENTS b = 'bbb'.

    "Deleting multiple lines based on conditions
    "Note: Specifying the additions USING KEY/FROM/TO is also possible
    DELETE it_st WHERE a > 3.

    out->write( data = it_st name = `it_st` ).
    out->write( |\n| ).
    out->write( data = it_so_sec name = `it_so_sec` ).
    out->write( |\n| ).
    out->write( data = it_ha_sec name = `it_ha_sec` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `76) Deleting adjacent duplicate entries` ) ).
    out->write( `Original table content (restored before` &&
      ` each of the following examples)` ).
    out->write( |\n| ).
    out->write( |\n| ).

    it_st = VALUE #( ( a = 1 b = 'BBB' c = '###' d = '###' )
                     ( a = 2 b = '###' c = '###' d = '###' )
                     ( a = 1 b = '###' c = '###' d = '###' )
                     ( a = 3 b = '###' c = '###' d = '###' )
                     ( a = 4 b = '###' c = 'CCC' d = '###' )
                     ( a = 1 b = 'BBB' c = '###' d = '###' )
                     ( a = 2 b = 'BBB' c = '###' d = '###' )
                     ( a = 4 b = 'BBB' c = '###' d = '###' )
                     ( a = 2 b = 'BBB' c = '###' d = '###' )
                     ( a = 3 b = '###' c = '###' d = '###' ) ).

    SORT it_st BY table_line.

    "Filling another table so that the same content above
    "is available for the examples below.
    it_st2 = it_st.

    out->write( data = it_st2 name = `it_st2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `77) Deleting adjacent duplicates based on` &&
      ` primary table key` ) ).

    "Note: Using the primary table key can have unexpected consequences
    "if the primary table key is the standard key or if it is empty.
    DELETE ADJACENT DUPLICATES FROM it_st2.

    out->write( data = it_st2 name = `it_st2` ).

    it_st2 = it_st.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `78) Deleting adjacent duplicates by comparing ` &&
      `all field values` ) ).

    DELETE ADJACENT DUPLICATES FROM it_st2 COMPARING ALL FIELDS.

    out->write( data = it_st2 name = `it_st2` ).

    it_st2 = it_st.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `79) Deleting adjacent duplicates by comparing ` &&
      `specific field values` ) ).

    DELETE ADJACENT DUPLICATES FROM it_st2 COMPARING a c.

    out->write( data = it_st2 name = `it_st2` ).

    it_st2 = it_st.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `80) Deleting adjacent duplicates by using a` &&
      ` table key` ) ).

    "In this case, the result is the same as in the first example.
    DELETE ADJACENT DUPLICATES FROM it_st2 USING KEY primary_key.

    out->write( data = it_st2 name = `it_st2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `81) Deleting the entire internal table content` ) ).

    CLEAR it_st.

    "Additionally, FREE releases memory space.
    FREE it_st2.

    "Excursion: Assigning an empty constructor expression with VALUE clears
    "the internal table.
    DATA(it_str) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).

    it_str = VALUE #( ).

    out->write( data = it_st name = `it_st` ).
    out->write( |\n| ).
    out->write( data = it_st2 name = `it_st2` ).
    out->write( |\n| ).
    out->write( data = it_str name = `it_str` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Excursions` ) ).
    out->write( |82) Secondary table keys and hashed tables\n\n| ).

    "Declaring a hashed table
    DATA hashed_tab
        TYPE HASHED TABLE OF zdemo_abap_tab1
           WITH UNIQUE KEY primary_key COMPONENTS key_field
           WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS char1 char2.

    "Retrieving data to work with
    SELECT * FROM zdemo_abap_tab1 INTO TABLE @hashed_tab UP TO 3 ROWS.

    "Integer table to display the table index
    DATA int_itab TYPE TABLE OF i.

    "Note: There is no primary table index in hashed tables.
    LOOP AT hashed_tab INTO DATA(hwa) USING KEY primary_key.
      APPEND sy-tabix TO int_itab.
    ENDLOOP.

    out->write( data = int_itab name = `int_itab` ).
    out->write( |\n| ).

    CLEAR int_itab.

    "Demonstrating the secondary table index when using
    "the secondary key
    LOOP AT hashed_tab INTO DATA(hwa2) USING KEY sec_key.
      APPEND sy-tabix TO int_itab.
    ENDLOOP.

    out->write( data = int_itab name = `int_itab` ).
    out->write( |\n| ).

    "Retrieving a table line via index access to the secondary index
    "of the sorted secondary key
    DATA(line_of_ht) = hashed_tab[ KEY sec_key INDEX 2 ].

    out->write( data = line_of_ht name = `line_of_ht` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `83) Empty keys in internal table created inline` ) ).
    "This example visualizes the fact that when using an inline
    "construction like INTO TABLE @DATA(itab) in SELECT statements, the
    "resulting table has an empty table key. Here, the key information
    "is retrieved using RTTI. The output shows the key information:
    "the information on the first internal table includes the key as
    "specified (key_field as the primary key, non-unique - since
    "key_kind is U and is_unique is not flagged. The result for the
    "other internal table shows that there is no key name at all and
    "key_kind is E (= empty).

    "An internal table representing an existing table having table keys
    "defined in contrast to an internal table created inline.
    DATA it_with_key TYPE TABLE OF zdemo_abap_tab1
           WITH NON-UNIQUE KEY key_field.

    "Retrieving data to work with
    SELECT * FROM zdemo_abap_tab1 INTO TABLE @it_with_key UP TO 3 ROWS.
    SELECT * FROM zdemo_abap_tab1 INTO TABLE @DATA(it_inline)
      UP TO 3 ROWS.

    "Using RTTI to retrieve the key information
    DATA(k1) = CAST cl_abap_tabledescr(
                        cl_abap_typedescr=>describe_by_data(
                          it_with_key )
                            )->get_keys( ).


    out->write( data = k1 name = `k1` ).
    out->write( |\n| ).

    DATA(k2) = CAST cl_abap_tabledescr(
                        cl_abap_typedescr=>describe_by_data(
                          it_inline )
                            )->get_keys( ).

    out->write( data = k2 name = `k2` ).
  ENDMETHOD.
ENDCLASS.
