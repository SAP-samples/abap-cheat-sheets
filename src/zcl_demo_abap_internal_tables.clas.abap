***********************************************************************
*
*           ABAP cheat sheet: Internal tables
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntactical options for working with
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

protected section.
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
    ( key_field = 400 char1 = 'ggg' char2 = 'hhh' num1 = 7 num2 = 8 )
         ) ).
    MODIFY zdemo_abap_tab2 FROM TABLE @( VALUE #(
    ( key_field = 500 char1 = 'iii'  num1 = 10 numlong = 1000 )
    ( key_field = 600 char1 = 'kkk'  num1 = 12 numlong = 2000 )
    ( key_field = 700 char1 = 'mmm'  num1 = 14 numlong = 3000 )
    ( key_field = 800 char1 = 'ooo'  num1 = 15 numlong = 4000 )
        ) ).

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
        tab = VALUE #( ( key_field = 1 num1 = 2 num2 = 3 )
                       ( key_field = 2 num1 = 3 num2 = 4 )
                       ( key_field = 3 num1 = 4 num2 = 5 ) ) )
      ( key_field = 2 char1 = 'bbb'
        tab = VALUE #( ( key_field = 4 num1 = 5 num2 = 6 )
                       ( key_field = 5 num1 = 6 num2 = 7 )
                       ( key_field = 6 num1 = 7 num2 = 8 ) ) ) ).

    itab_nested2 = VALUE #(
    ( key_field = 99 char2 = 'yyy' tab = VALUE #(
      ( key_field = 10 char1 = 'aaa'
        char2 = 'bbb' num1 = 100 num2 = 200 )
      ( key_field = 20 char1 = 'ccc'
        char2 = 'ddd' num1 = 300 num2 = 400 )
      ( key_field = 30 char1 = 'eee'
        char2 = 'fff' num1 = 500 num2 = 600 ) ) )
    ( key_field = 100 char2 = 'zzz' tab = VALUE #(
      ( key_field = 40 char1 = 'ggg'
        char2 = 'hhh' num1 = 700 num2 = 800 )
      ( key_field = 50 char1 = 'iii'
        char2 = 'jjj' num1 = 900 num2 = 1000 )
      ( key_field = 60 char1 = 'kkk'
        char2 = 'lll' num1 = 1100 num2 = 1200 ) ) ) ).

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA(output) = NEW zcl_demo_abap_display( out ).

    output->display( `ABAP Cheat Sheet Example: Internal Tables` ).
    output->display( `Filling and Copying Internal Table Content` ).
    output->display( `1) Adding single lines using APPEND/INSERT` ).

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

    output->display( input = it_st name = `it_st` ).
    output->display( input = it_so name = `it_so` ).

**********************************************************************

    output->next_section( `2) Adding initial line` ).

    APPEND INITIAL LINE TO it_st.

    INSERT INITIAL LINE INTO TABLE it_so.

    output->display( input = it_st name = `it_st` ).
    output->display( input = it_so name = `it_so` ).

**********************************************************************
    
    output->next_section( `3) Adding mutliple lines of an internal table to` &&
    ` another one` ).

    "No additions: All lines are added to the target internal table
    APPEND LINES OF it_so TO it_st.

    "Creating a new itab and filling it.
    DATA it_so2 LIKE it_so.

    INSERT VALUE #( a = 3 b = 'g' c = 'h' d = 'i' ) INTO TABLE it_so2.

    INSERT VALUE #( a = 4 b = 'j' c = 'k' d = 'l' ) INTO TABLE it_so2.

    "Inserting all lines of previously created internal table.
    INSERT LINES OF it_so2 INTO TABLE it_so.

    output->display( input = it_st name = `it_st` ).
    output->display( input = it_so name = `it_so` ).

**********************************************************************

    output->next_section( `4) Adding lines of an internal table to` &&
    ` another one by specifying the index range.` ).

    "When using only FROM, all lines are respected until the final
    "table entry. When using only TO, all lines are respected
    "starting from the first table entry.
    APPEND LINES OF it_so FROM 2 TO 3 TO it_st.

    INSERT LINES OF it_so FROM 3 INTO TABLE it_st.

    APPEND LINES OF it_so TO 2 TO it_st.

    output->display( input = it_st name = `it_st` ).

**********************************************************************    

    output->next_section( `5) Inserting lines of an internal table` &&
    ` into another one at a specific position` ).

    "Inserting a single line
    INSERT VALUE #( a = 10 b = 'ggg' c = 'hhh' d = 'iii' )
      INTO it_st INDEX 1.

    "Inserting multiple lines
    INSERT LINES OF it_so2 INTO it_st INDEX 2.

    output->display( input = it_st name = `it_st` ).

**********************************************************************

    output->next_section( `6) Adding lines using constructor expressions` ).

    "Creating a line to be added to an internal table.
    line = VALUE #( a = 1 b = 'aaa' c = 'bbb' d = 'ccc' ).

    "Table on the right is constructed inline using VALUE and assigned
    "Note: This way, existing table content is cleared.
    it_st = VALUE #( ( line )
                     ( a = 2 b = 'ddd' c = 'eee' d = 'fff' ) ).

    output->display( input = it_st name = `it_st` ).

**********************************************************************

    output->next_section( `7) Creating a new table inline and adding lines` &&
    ` using a constructor expression` ).

    "Internal table type
    TYPES it_type LIKE it_st.

    "Creating an internal table inline and filling in one go
    DATA(it_st2) = VALUE it_type( ( a = 3     b = 'ggg'
                                    c = 'hhh' d = 'iii' )
                                  ( a = 4     b = 'jjj'
                                    c = 'kkk' d = 'lll' ) ).

    output->display( input = it_st2 name = `it_st2` ).

**********************************************************************

    output->next_section( `8) Adding lines using constructor expressions ` &&
    `and keeping existing table content` ).

    "BASE addition: existing table content is not removed
    it_st = VALUE #( BASE it_st ( a = 5 b = 'mmm' c = 'nnn' d = 'ooo' )
                                ( a = 6 b = 'ppp' c = 'qqq' d = 'rrr' )
                   ).

    output->display( input = it_st name = `it_st` ).

**********************************************************************

    output->next_section( `9) Adding lines from other internal tables using` &&
    ` constructor expressions` ).

    "With LINES OF itab specified within the pair of parentheses,
    "all lines of the internal table are added; here, in the same
    "expression another line is added as well
    it_st = VALUE #( BASE it_st ( LINES OF it_st2 )
                                ( a = 7 b = 'sss' c = 'ttt' d = 'uuu' )
                   ).

    output->display( input = it_st name = `it_st` ).

**********************************************************************

    output->next_section( `10) Copying table content (without constructor ` &&
                  `expression)` ).

    "Assignment of a table to another one having a matching line type
    it_st = it_st2.

    output->display( input = it_st name = `it_st` ).

**********************************************************************

    output->next_section( `11) CORRESPONDING Operator and MOVE-CORRESPONDING` ).
    output->display( `Internal table content before assignments` ).

    "Note: Before the following statements, the table content is reset
    "to this state to work with the same set of values.
    fill_itabs_for_corresponding( ).

    output->display( input = tab1 name = `tab1` ).
    output->display( input = tab2 name = `tab2` ).
    output->display( input = tab3 name = `tab3` ).
    output->display( input = tab4 name = `tab4` ).

**********************************************************************

    output->next_section( `Copying content from another table that has ` &&
        `a different line type ...` ).
    output->display( `11a) ... and deleting existing table content ` &&
    `using the CORRESPONDING operator` ).

    tab1 = CORRESPONDING #( tab2 ).

    output->display( input = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    output->next_section( `11b) ... and deleting existing table content ` &&
    `using MOVE-CORRESPONDING` ).

    MOVE-CORRESPONDING tab2 TO tab1.

    output->display( input = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    output->next_section( `11c) ... and keeping existing table ` &&
    `content using the CORRESPONDING operator` ).

    tab1 = CORRESPONDING #( BASE ( tab1 ) tab2 ).

    output->display( input = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    output->next_section( `11d) ... and keeping existing table ` &&
    `content using MOVE-CORRESPONDING`  ).

    MOVE-CORRESPONDING tab2 TO tab1 KEEPING TARGET LINES.

    output->display( input = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    output->next_section( `11e) ... respecting component ` &&
    `mapping`  ).

    "Specifying components of a source table that are assigned to the
    "components of a target table in mapping relationships
    tab1 = CORRESPONDING #( tab2 MAPPING c = e d = f ).

    output->display( input = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    output->next_section( `11f) ... excluding components` ).

    "Excluding components from the assignment
    tab1 = CORRESPONDING #( tab2 EXCEPT b ).

    output->display( input = tab1 name = `tab1` ).

    "EXCEPT * means that all components remain initial not specified
    "for mapping
    tab1 = CORRESPONDING #( tab2 MAPPING d = f EXCEPT * ).

    output->display( input = tab1 name = `tab1` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    output->next_section( `11h) ... discarding duplicates` ).

    "Preventing runtime errors if duplicate lines are assigned to
    "target table that is defined to only accept unique keys.
    "Note: Other tables than above are used here.
    tab3 = CORRESPONDING #( BASE ( tab3 ) tab4 DISCARDING DUPLICATES ).

    output->display( input = tab3 name = `tab3` ).

    fill_itabs_for_corresponding( ).

    tab3 = CORRESPONDING #( BASE ( tab3 ) tab4 DISCARDING DUPLICATES
                            MAPPING d = f EXCEPT b ).

    output->display( input = tab3 name = `tab3` ).

**********************************************************************

    output->next_section( `11i) Copying data from a deep ` &&
    `internal table to another deep internal table` ).
    output->display( `Original table content` ).

    output->display( input = itab_nested1 name = `itab_nested1` ).
    output->display( input = itab_nested2 name = `itab_nested2` ).

**********************************************************************

    output->next_section( `11j) ... deleting ` &&
    `existing content (CORRESPONDING operator)` ).

    itab_nested2 = CORRESPONDING #( DEEP itab_nested1 ).

    output->display( input = itab_nested2 name = `itab_nested2` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    output->next_section( `11k) ... deleting ` &&
    `existing content (MOVE-CORRESPONDING)` ).

    MOVE-CORRESPONDING itab_nested1 TO itab_nested2
     EXPANDING NESTED TABLES.

    output->display( input = itab_nested2 name = `itab_nested2` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    output->next_section( `11l) ... keeping ` &&
    `existing content (CORRESPONDING operator)` ).

    itab_nested2 = CORRESPONDING #( DEEP BASE ( itab_nested2 )
                                                itab_nested1 ).

    output->display( input = itab_nested2 name = `itab_nested2` ).

    fill_itabs_for_corresponding( ).

**********************************************************************

    output->next_section( `11m) ... keeping ` &&
    `existing content (MOVE-CORRESPONDING)` ).

    MOVE-CORRESPONDING itab_nested1 TO itab_nested2
     EXPANDING NESTED TABLES KEEPING TARGET LINES.

    output->display( input = itab_nested2 name = `itab_nested2` ).

**********************************************************************

    output->next_section( `Filling internal tables: Excursions` ).
    output->display( `12) Selecting multiple rows from a database ` &&
    `table into an internal table` ).

    SELECT FROM zdemo_abap_tab1
      FIELDS key_field, char1, char2, num1, num2
      WHERE num1 > 3
      INTO TABLE @DATA(itab_select1).

    output->display( input = itab_select1 name = `itab_select1` ).

**********************************************************************

    output->next_section( `13) Sequentially adding multiple rows from ` &&
    `a database table to an internal table` ).

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

    output->display( input = itab name = `itab` ).

**********************************************************************

    output->next_section( `14) Adding multiple rows from a database table ` &&
    `to an internal table that has a different line type than the ` &&
    `database table and keeping existing table content` ).

    SELECT FROM zdemo_abap_tab2
      FIELDS *
      WHERE num1 > 10
      APPENDING CORRESPONDING FIELDS OF TABLE @itab.

    output->display( input = itab name = `itab` ).

**********************************************************************

    output->next_section( `15) Adding multiple rows from a database table ` &&
    `to an internal table that has a different line type than the ` &&
    `database table and deleting existing table content` ).

    SELECT FROM zdemo_abap_tab2
      FIELDS *
      WHERE num1 > 10
      INTO CORRESPONDING FIELDS OF TABLE @itab.

    output->display( input = itab name = `itab` ).

**********************************************************************

    output->next_section( `16) Adding multiple rows from an internal table ` &&
    `to an internal table using SELECT` ).

    SELECT key_field, char1, char2, num1, num2
      FROM @itab AS itab_alias
      INTO TABLE @DATA(itab_clone).

    output->display( input = itab_clone name = `itab_clone` ).

**********************************************************************

    output->next_section( `17) Combining data of multiple tables into an` &&
    ` internal table using an inner join` ).

    "Filling table to be selected from
    itab =  VALUE #( ( key_field = 500 char1 = 'uuu' char2 = 'vvv'
                       num1 = 501 num2 = 502 )
                     ( key_field = 600 char1 = 'www' char2 = 'xxx'
                       num1 = 601 num2 = 602 ) ).

    "SELECT list includes fields from both tables
    "If there are no equivalent entries in the first or second table,
    "the rows are not joined.
    SELECT itab_alias1~key_field, itab_alias1~char2,
           zdemo_abap_tab2~numlong
      FROM @itab AS itab_alias1
      INNER JOIN zdemo_abap_tab2
        ON itab_alias1~key_field = zdemo_abap_tab2~key_field
      INTO TABLE @DATA(join_result).

    output->display( input = join_result name = `join_result` ).

**********************************************************************

    output->next_section( `18) Filling internal table ` &&
    `using a subquery (1)` ).

    "A subquery is specified in the WHERE clause
    "Here, data is selected from a database table depending on
    "whether the value of a certain field is not among the
    "values specified in parentheses.
    SELECT key_field, char1, numlong
      FROM zdemo_abap_tab2
      WHERE char1 NOT IN ( 'iii', 'mmm', 'ooo', 'ppp' )
      INTO TABLE @DATA(subquery_result1).

    output->display( input = subquery_result1 name = `subquery_result1` ).

**********************************************************************

    output->next_section( `19) Filling internal table ` &&
    `using a subquery (2)` ).

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

    output->display( input = subquery_result2 name = `subquery_result2` ).

**********************************************************************

    output->next_section( `20) Filling an internal table from a table ` &&
    `depending on the existence of data in another internal table ` &&
    `using the addition FOR ALL ENTRIES` ).

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

    output->display( input = select_result name = `select_result` ).

**********************************************************************

    output->next_section( `21) Adding content from a database to internal` &&
    ` table by using alias names in the SELECT list` ).

    DATA itab2 TYPE TABLE OF zdemo_abap_tab2 WITH EMPTY KEY.

    "Specifying alias names can help fill an existing internal
    "table that has not a matching line type to the database table.
    "Here, two fields are specified with an alias name to match the
    "names of components contained in the existing internal table.
    "The individual types of the fields match, too.
    SELECT key_field, char2 AS char1, num2 AS num1
        FROM zdemo_abap_tab1
      INTO CORRESPONDING FIELDS OF TABLE @itab2 UP TO 3 ROWS.

    output->display( input = itab2 name = `itab2` ).

**********************************************************************

    output->next_section( `22) FILTER: Filtering internal table by condition` ).

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
    fi_tab1 = VALUE #( ( a = 1  b = 'aaa' c = 'abc' )
                       ( a = 2  b = 'bbb' c = 'def' )
                       ( a = 3  b = 'ccc' c = 'hij' )
                       ( a = 4  b = 'ddd' c = 'klm' )
                       ( a = 5  b = 'eee' c = 'nop' ) ).

    fi_tab2 = fi_tab1.
    fi_tab3 = fi_tab1.

    "The lines meeting the condition are respected.
    "Note: The source table must have at least one sorted or hashed key.
    "Here, the primary key is used
    DATA(f1) = FILTER #( fi_tab1 WHERE a >= 3 ).

    output->display( input = f1 name = `f1` ).

    "USING KEY primary_key explicitly specified; same as above
    DATA(f2) = FILTER #( fi_tab1 USING KEY primary_key WHERE a >= 3 ).

    output->display( input = f2 name = `f2` ).

    "EXCEPT addition
    DATA(f3) = FILTER #( fi_tab1 EXCEPT WHERE a >= 3 ).

    output->display( input = f3 name = `f3` ).

    DATA(f4) = FILTER #( fi_tab1 EXCEPT USING KEY primary_key WHERE a >= 3 ).

    output->display( input = f4 name = `f4` ).

    "Secondary table key specified after USING KEY
    DATA(f5) = FILTER #( fi_tab2 USING KEY sec_key WHERE a >= 4 ).

    output->display( input = f5 name = `f5` ).

    DATA(f6) = FILTER #( fi_tab2 EXCEPT USING KEY sec_key WHERE a >= 3 ).

    output->display( input = f6 name = `f6` ).

    "Note: In case of a hash key, exactly one comparison expression for each key
    "component is allowed; only = as comparison operator possible.
    DATA(f7) = FILTER #( fi_tab3 WHERE a = 3 ).

    output->display( input = f7 name = `f7` ).

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

    output->display( input = f8 name = `f8` ).

    "EXCEPT addition
    DATA(f9) = FILTER #( fi_tab1 EXCEPT IN filter_tab1 WHERE a = table_line ).

    output->display( input = f9 name = `f9` ).

    "USING KEY is specified for the filter table
    DATA(f10) = FILTER #( fi_tab2 IN filter_tab2 USING KEY line WHERE a = table_line ).

    output->display( input = f10 name = `f10` ).

    "USING KEY is specified for the source table, including EXCEPT
    DATA(f11) = FILTER #( fi_tab2 USING KEY sec_key EXCEPT IN filter_tab2 WHERE a = table_line ).

    output->display( input = f11 name = `f11` ).

**********************************************************************

    output->next_section(  `25) Inserting data into an internal table ` &&
    `using a COLLECT statement` ).

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

    output->display( input = itab_num name = `itab_num` ).

**********************************************************************

    output->next_section( `Reading from internal tables` ).

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

    output->display( `Original table content` ).

    output->display( input = it_so_sec name = `it_so_sec` ).
    output->display( input = it_ha_sec name = `it_ha_sec` ).

**********************************************************************

    output->next_section( `26) Reading a single line into target area` ).

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

    output->display( input = wa1 name = `wa1` ).
    output->display( input = wa2 name = `wa2` ).
    output->display( input = <fs> name = `<fs>` ).
    output->display( input = <fs2> name = `<fs2>` ).
    output->display( input = dref->* name = `dref->*` ).
    output->display( input = dref2->* name = `dref2->*` ).

**********************************************************************

    output->next_section( `Reading a single line via index ...` ).
    output->display( `27) ... using READ TABLE` ).

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

    output->display( input = wa3 name = `wa3` ).
    output->display( input = wa4 name = `wa4` ).
    output->display( input = wa5 name = `wa5` ).
    output->display( input = wa6 name = `wa6` ).
    output->display( input = wa7 name = `wa7` ).
    output->display( input = wa8 name = `wa8` ).

**********************************************************************

    output->next_section( `28) ... table expressions (1)` ).

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

    output->display( input = lv1 name = `lv1` ).

    IF lv2 IS NOT INITIAL.
      output->display( input = lv2 name = `lv2` ).
    ENDIF.

    IF error IS NOT INITIAL.
      output->display( input = error name = `error` ).
    ENDIF.

    output->display( input = lv3 name = `lv3` ).
    output->display( input = lv4 name = `lv4` ).
    output->display( input = lv5 name = `lv5` ).

**********************************************************************

    output->next_section( `29) ... table expressions (2)` ).

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

    output->display( input = lv6 name = `lv6` ).
    output->display( input = dref3->* name = `dref3->*` ).
    output->display( input = lv7 name = `lv7` ).
    output->display( input = lv8 name = `lv8` ).

**********************************************************************

    output->next_section( `Reading a single line via table keys ...` ).
    output->display( `30) ... using READ TABLE (1)` ).

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

    output->display( input = wa9 name = `wa9` ).
    output->display( input = wa10 name = `wa10` ).
    output->display( input = wa11 name = `wa11` ).
    output->display( input = wa12 name = `wa12` ).
    output->display( input = wa13 name = `wa13` ).

**********************************************************************

    output->next_section( `31) ... using READ TABLE (2)` ).

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

    output->display( input = wa14 name = `wa14` ).
    output->display( input = wa15 name = `wa15` ).
    output->display( input = wa16 name = `wa16` ).

**********************************************************************

    output->next_section( `32) ... using table expressions` ).
    "Primary table key (COMPONENTS addition is optional)
    DATA(lv9) = it_so_sec[ KEY primary_key COMPONENTS a = 1 ].

    DATA(lv10) = it_so_sec[ KEY primary_key a = 1 ].

    DATA(lv11) = it_so_sec[ KEY pk a = 2 ]. "Primary table key alias

    "Secondary table key (COMPONENTS mandatory)
    DATA(lv12) = it_so_sec[ KEY sec_key COMPONENTS b = 'aaa' ].

    DATA(lv13) = it_so_sec[ KEY sk COMPONENTS b = 'ddd' ]. "Alias

    output->display( input = lv9 name = `lv9` ).
    output->display( input = lv10 name = `lv10` ).
    output->display( input = lv11 name = `lv11` ).
    output->display( input = lv12 name = `lv12` ).
    output->display( input = lv13 name = `lv13` ).

**********************************************************************

    output->next_section( `33) Reading a single line via free key` ).
    "Note: If there a multiple matching entries, the first found
    "is returned.
    READ TABLE it_so_sec INTO DATA(wa17) WITH KEY c = '###'.

    DATA(lv14) = it_so_sec[ c = 'zzz' ].

    output->display( input = wa17 name = `wa17` ).
    output->display( input = lv14 name = `lv14` ).

**********************************************************************

    output->next_section( `34) Excursion: Addressing individual components` ).
    "Addressing a component using the component selector
    DATA(comp1) = it_so_sec[ 1 ]-b.

    READ TABLE it_so_sec ASSIGNING FIELD-SYMBOL(<fs3>) INDEX 2.

    DATA(comp2) = <fs3>-c.

    READ TABLE it_so_sec REFERENCE INTO DATA(dref4) INDEX 3.

    DATA(comp3) = dref->*-a.

    "Same effect as above but less to write
    DATA(comp4) = dref->b.

    output->display( input = comp1 name = `comp1` ).
    output->display( input = comp2 name = `comp2` ).
    output->display( input = comp3 name = `comp3` ).
    output->display( input = comp4 name = `comp4` ).

**********************************************************************

    output->next_section( `35) Checking if a line exists in an internal table` ).

    "Defining the key
    DATA(key1) = 2.

    "Internal table function
    IF line_exists( it_so_sec[ a = key1 ] ).
      output->display( |Line { key1 } exists in internal table.| ).
    ELSE.
      output->display( |Line { key1 } does not exist in internal table.| ).
    ENDIF.

    "Alternative using READ TABLE (sy-subrc is checked)
    "When using the addition TRANSPORTING NO FIELDS, no field values
    "are read. Only the system fields are filled.
    READ TABLE it_so_sec WITH KEY a = key1 TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      output->display( |Line { key1 } exists in internal table.| ).
    ELSE.
      output->display( |Line { key1 } does not exist in internal table.| ).
    ENDIF.

**********************************************************************

    output->next_section( `36) Checking the index of a ` &&
    `specific line` ).

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
      output->display( |The index of the line with key = { key2 } | &&
                    |is { idx_of_line1 } in the internal table.| ).
    ELSE.
      output->display( |The line with key = { key2 } does not exist | &&
                    |in the internal table.| ).
    ENDIF.

    IF idx_of_line2 <> 0.
      output->display( |The index of the line with key = { key3 } | &&
                    |is { idx_of_line2 } in the internal table.| ).
    ELSE.
      output->display( |The line with key = { key3 } does not exist | &&
                    |in the internal table.| ).
    ENDIF.

    IF tab_idx1 <> 0.
      output->display( |The index of the line with key = { key2 }  | &&
                    |is { tab_idx1 } in the internal table.| ).
    ELSE.
      output->display( |The line with key = { key2 } does not exist | &&
                    |in the internal table.| ).
    ENDIF.

    IF tab_idx2 <> 0.
      output->display( |The index of the line with key = { key3 } | &&
                    |is { tab_idx2 } in the internal table.| ).
    ELSE.
      output->display( |The line with key = { key3 } does not exist | &&
                    |in the internal table.| ).
    ENDIF.

    output->next_section( `37) Checking how many lines are in an` &&
    ` internal table` ).
    DATA(itab_lines) = lines( it_so_sec ).

    output->display( |The internal table consists of { itab_lines } | &&
                     |lines.| ).

**********************************************************************

    output->next_section( `Processing multiple internal table lines ` &&
                     `sequentially` ).
    output->display( `38a) Reading a complete table by sequentially ` &&
                  `reading all lines` ).

    "No further addition: All lines are respected.
    LOOP AT it_so_sec ASSIGNING FIELD-SYMBOL(<fs4>).
      "Modifying a component to visualize the reading of all lines.
      <fs4>-b = 'ZZZ'.
    ENDLOOP.

    output->display( input = it_so_sec name = `it_so_sec` ).

**********************************************************************

    output->display( `38b) LOOP AT statements with different targets` ).

    "The following examples demonstrate the different targets that
    "are possible for LOOP AT statements. In the example above,
    "a field symbol is created inline.
    "As above, there are no additions to the loop statement, i.e. all lines
    "are processed.

    DATA(lines_in_table) = lines( it_so_sec ).
    output->display( |There should be { lines_in_table } iterations per loop.| ).

    "Target: Existing work area
    output->display( `---- Loop target: Existing work area ----` ).
    DATA wa_lo LIKE LINE OF it_so_sec.

    LOOP AT it_so_sec INTO wa_lo.
      IF sy-tabix = 1.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.
    ENDLOOP.

    output->display( `---- Loop target: Work area created inline ----` ).
    LOOP AT it_so_sec INTO DATA(wa_inl).
      IF sy-tabix = 1.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.
    ENDLOOP.

    output->display( `---- Loop target: Existing field symbol ----` ).
    FIELD-SYMBOLS <fs_lo> LIKE LINE OF it_so_sec.

    LOOP AT it_so_sec ASSIGNING <fs>.
      IF sy-tabix = 1.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.

    ENDLOOP.

    output->display( `---- Loop target: Field symbol created inline ----` ).
    LOOP AT it_so_sec ASSIGNING FIELD-SYMBOL(<fs_inl>).
      IF sy-tabix = 1.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.
    ENDLOOP.

    output->display( `---- Loop target: Existing data reference variable ----` ).
    DATA dref_lo TYPE REF TO struc1 .

    LOOP AT it_so_sec REFERENCE INTO dref_lo.
      IF sy-tabix = 1.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.
    ENDLOOP.

    output->display( `Loop target: Data reference variable created inline` ).
    LOOP AT it_so_sec REFERENCE INTO DATA(dref_inl).
      IF sy-tabix = 1.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ELSEIF sy-tabix = lines_in_table.
        output->display( |This text is displayed when reaching line { sy-tabix }.| ).
      ENDIF.
    ENDLOOP.

**********************************************************************

    output->next_section( `39) Reading multiple lines by an index range` ).

    "Specific lines in an index range are respected
    "Note: FROM/TO alone can specified, too.
    LOOP AT it_so_sec ASSIGNING FIELD-SYMBOL(<fs5>) FROM 2 TO 3.
      "Modifying a component to visualize the reading of specific lines.
      <fs5>-c = 'YYY'.
    ENDLOOP.

    output->display( input = it_so_sec name = `it_so_sec` ).

**********************************************************************

    output->next_section( `40) Reading multiple lines by condition` ).

    LOOP AT it_so_sec ASSIGNING FIELD-SYMBOL(<fs6>) WHERE a < 3.
      "Modifying a component to visualize the reading of specific lines.
      <fs6>-d = 'XXX'.
    ENDLOOP.

    output->display( input = it_so_sec name = `it_so_sec` ).

**********************************************************************

    output->next_section( `41) Looping across a table without an interest` &&
                     ` in the table content` ).

    "Here, only the system fields are set.
    LOOP AT it_so_sec TRANSPORTING NO FIELDS WHERE a < 3.
      DATA(num) = sy-tabix.
    ENDLOOP.

    output->display( |There are { num } lines in the table | &&
                     |fulfilling the condition.| ).

**********************************************************************

    output->next_section( `42) Loop with table key specification` ).

    DATA it_st_em TYPE TABLE OF struc1 WITH EMPTY KEY.

    "Looping across hashed table using a secondary key. The loop starts
    "according to the secondary table index. The lines are added to
    "another internal table having a matching type. It basically
    "visualizes the order of the table lines in the secondary table
    "index.
    LOOP AT it_ha_sec ASSIGNING FIELD-SYMBOL(<fs7>) USING KEY sec_key_h.
      APPEND <fs7> TO it_st_em.
    ENDLOOP.

    output->display( input = it_st_em name = `it_st_em` ).

**********************************************************************

    output->next_section( `Creating and filling tables using table ` &&
                  `iterations with FOR and VALUE` ).
    output->display( `43) Retrieving values of one column in ` &&
                  `an internal table.` ).

    "Creating internal table type
    TYPES ty_numbers TYPE TABLE OF i WITH EMPTY KEY.

    "Table comprehension: Content of an internal table is created by
    "evaluating a table using a table iteration with an iteration
    "expressions within a constructor expression.
    DATA(lv_num_a) = VALUE ty_numbers( FOR ls1 IN it_ha_sec
                                        ( ls1-a ) ).

    output->display( input = lv_num_a name = `lv_num_a` ).

**********************************************************************

    output->next_section( `44) Retrieving values of one column in ` &&
    `an internal table based on conditions` ).

    DATA(lv_num_b) = VALUE ty_numbers( FOR ls2 IN it_ha_sec
                                       WHERE ( a < 3 ) ( ls2-a ) ).

    output->display( input = lv_num_b name = `lv_num_b` ).

**********************************************************************

    output->next_section( `45) Looping across 2 tables ` &&
                  `and retrieving values based on conditions` ).
    "Internal table type
    TYPES tabtype LIKE it_so_sec.

    DATA(itab_for_2tab) =
      VALUE tabtype(
        FOR ls3 IN it_ha_sec
        FOR ls4 IN it_so_sec WHERE ( a = ls3-a )
        ( a = ls3-a b = ls4-b c = ls3-c d = ls4-d ) ).

    output->display( input = itab_for_2tab name = `itab_for_2tab` ).

**********************************************************************

    output->next_section( `46) Retrieving and changing values from an ` &&
                  `internal tables sequentially` ).
    DATA(it_changed) = VALUE tabtype( FOR ls5 IN it_so_sec
                       ( a = ls5-a b = 'WWW' c = 'VVV' d = 'UUU'  ) ).

    output->display( input = it_changed name = `it_changed` ).


    output->next_section( `Sorting internal tables` ).

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

    output->display( `Original internal table content ` &&
    `(it1 and it2 have the same content)` ).

    output->display( input = it1 name = `it1` ).
    output->display( input = it2 name = `it2` ).

**********************************************************************

    output->next_section( `47) Sorting by primary table key` ).

    "Primary key: component a
    SORT it1.

    output->display( input = it1 name = `it1` ).

**********************************************************************

    output->next_section( `48) Sorting by primary table key in ascending` &&
                  ` order` ).

    "The sorting result is the same as above (where ASCENDING is used
    "implicitly). Here, it is explicitly specified.
    SORT it1 ASCENDING.

    output->display( input = it1 name = `it1` ).

**********************************************************************

    output->next_section( `49) Sorting by primary table key respecting all ` &&
    `non-numeric fields` ).

    "Primary key: standard table key (all non-numeric fields)
    SORT it2.

    output->display( input = it2 name = `it2` ).

    "The following code is commented out on purpose because it
    "produces a syntax warning. The primary table key is empty.
    "A sorting has no effect.
    "SORT it3.
    "output->display( input = it3 name = `it3` ).

**********************************************************************

    output->next_section( `50) Sorting by primary table key in ` &&
                  `descending order` ).

    "Sorting in descending order and by primary table key
    SORT it1 DESCENDING.

    output->display( input = it1 name = `it1` ).

**********************************************************************

    output->next_section( `51) Sorting by explicitly specified component (1)` ).
    "Here, the component is the primary table key.
    "The sorting result is the same as above.
    SORT it1 BY a DESCENDING.

    output->display( input = it1 name = `it1` ).

**********************************************************************

    output->next_section( `52) Sorting by explicitly specified component (2)` ).

    "Sorting by arbitrary, non-key field
    SORT it1 BY d DESCENDING.

    output->display( input = it1 name = `it1` ).

**********************************************************************

    output->next_section( `53) Sorting by multiple explicitly specified` &&
                  ` components` ).

    "Sorting by multiple components and specifying the sort order
    SORT it1 BY b ASCENDING c DESCENDING.

    output->display( input = it1 name = `it1` ).

**********************************************************************

    output->next_section( `54) Sorting by respecting the values of all` &&
                  ` components` ).

    "Sorting by considering the values of each field of the table line
    SORT it1 BY table_line.

    output->display( input = it1 name = `it1` ).

**********************************************************************

    output->next_section( `Modifying internal table content` ).
    output->display( `Internal table content before modifications` ).

    "Standard table
    output->display( input = it_st name = `it_st` ).

    "Sorted table
    output->display( input = it_so_sec name = `it_so_sec` ).

    "Hashed table
    output->display( input = it_ha_sec name = `it_ha_sec` ).

**********************************************************************

    output->next_section( `55) Directly modifying recently read table lines` ).

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

    output->display( input = it_so_sec[ 1 ] name = `it_so_sec[ 1 ]` ).
    output->display( input = it_st[ 1 ] name = `it_st[ 1 ]` ).

**********************************************************************

    output->next_section( `56) Modifying internal table content using MODIFY` ).
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

    output->display( input = it_st name = `it_st` ).
    output->display( input = it_so_sec name = `it_so_sec` ).
    output->display( input = it_ha_sec name = `it_ha_sec` ).

**********************************************************************

    output->next_section( `57) Deleting internal table content using DELETE` ).
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

    output->display( input = it_st name = `it_st` ).
    output->display( input = it_so_sec name = `it_so_sec` ).
    output->display( input = it_ha_sec name = `it_ha_sec` ).

**********************************************************************

    output->next_section( `Deleting adjacent duplicate entries` ).
    output->display( `Original table content (restored before` &&
                  ` each of the following examples)` ).

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

    output->display( input = it_st2 name = `it_st2` ).

**********************************************************************

    output->display( `58) Deleting adjacent duplicates based on` &&
                  ` primary table key` ).

    "Note: Using the primary table key can have unexpected consequences
    "if the primary table key is the standard key or if it is empty.
    DELETE ADJACENT DUPLICATES FROM it_st2.

    output->display( input = it_st2 name = `it_st2` ).

    it_st2 = it_st.

**********************************************************************

    output->next_section( `59) Deleting adjacent duplicates by comparing ` &&
                  `all field values` ).

    DELETE ADJACENT DUPLICATES FROM it_st2 COMPARING ALL FIELDS.

    output->display( input = it_st2 name = `it_st2` ).

    it_st2 = it_st.

**********************************************************************

    output->next_section( `60) Deleting adjacent duplicates by comparing ` &&
                  `specific field values` ).

    DELETE ADJACENT DUPLICATES FROM it_st2 COMPARING a c.

    output->display( input = it_st2 name = `it_st2` ).

    it_st2 = it_st.

**********************************************************************

    output->next_section( `61) Deleting adjacent duplicates by using a` &&
                  ` table key` ).

    "In this case, the result is the same as in the first example.
    DELETE ADJACENT DUPLICATES FROM it_st2 USING KEY primary_key.

    output->display( input = it_st2 name = `it_st2` ).

**********************************************************************

    output->next_section( `62) Deleting the entire internal table content` ).

    CLEAR it_st.

    "Additionally, FREE releases memory space.
    FREE it_st2.

    "Excursion: Assigning an empty constructor expression with VALUE clears
    "the internal table.
    DATA(it_str) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).

    it_str = VALUE #( ).        

    output->display( input = it_st name = `it_st` ).
    output->display( input = it_st2 name = `it_st2` ).
    output->display( input = it_str name = `it_str` ).

**********************************************************************

    output->next_section( `Excursions` ).
    output->display( `63) Secondary table keys and hashed tables` ).

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

    output->display( input = int_itab name = `int_itab` ).

    CLEAR int_itab.

    "Demonstrating the secondary table index when using
    "the secondary key
    LOOP AT hashed_tab INTO DATA(hwa2) USING KEY sec_key.
      APPEND sy-tabix TO int_itab.
    ENDLOOP.

    output->display( input = int_itab name = `int_itab` ).

    "Retrieving a table line via index access to the secondary index
    "of the sorted secondary key
    DATA(line_of_ht) = hashed_tab[ KEY sec_key INDEX 2 ].

    output->display( input = line_of_ht name = `line_of_ht` ).

**********************************************************************

    output->next_section( `64) Empty keys in internal table created inline` ).
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


    output->display( input = k1 name = `k1` ).

    DATA(k2) = CAST cl_abap_tabledescr(
                        cl_abap_typedescr=>describe_by_data(
                          it_inline )
                            )->get_keys( ).

    output->display( input = k2 name = `k2` ).

  ENDMETHOD.
ENDCLASS.
