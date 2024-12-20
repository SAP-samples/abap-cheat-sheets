"! <p class="shorttext"><strong>Dynamic programming</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates working with internal tables.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <p>Find information on <strong>getting started with the example class</strong> and the
"! <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
CLASS zcl_demo_abap_internal_tables DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS fill_dbtabs.
ENDCLASS.



CLASS zcl_demo_abap_internal_tables IMPLEMENTATION.

  METHOD class_constructor.
    zcl_demo_abap_aux=>fill_dbtabs( ).
    fill_dbtabs( ).
  ENDMETHOD.


  METHOD fill_dbtabs.
    "Initializing and populating database tables to have data to work with

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

  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP cheat sheet example: Internal tables\n\n| ).

    out->write( |1) Creating Internal Tables By Inline Declaration\n\n| ).

    "Table declared inline in the context of an assignment
    "The examples show the copying of a table including the content
    "on the fly and creating the table in one step. The data type of the
    "declared variable is determined by the right side.
    "The table type here is a predefined and globally available table type.
    DATA string_tab TYPE string_table.

    DATA(it_1) = string_tab.
    DATA(it_2) = it_1.

    "Using FINAL for creating immutable variables
    FINAL(it_3) = it_1.
    "For example, it is not possible to modify such a table in the following position.
    "APPEND INITIAL LINE TO it_3.

    "As shown below and in other cheat sheets, constructor operators
    "are handy when creating internal tables in place. The following
    "examples uses the VALUE operator and an internal table type.
    DATA(it_4) = VALUE string_table( ( `aaa` )
                                     ( `bbb` ) ).

    "Not providing any table lines means the table is initial
    "and has the same effect as the declaration of it6.
    DATA(it_5) = VALUE string_table( ).
    DATA it_6 TYPE string_table.

    "Excursion
    "Table declared inline in the context of a SELECT statement;
    "a prior extra declaration of an internal table is not needed.
    SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(it_7).

    "Instead of
    DATA it_8 TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.
    SELECT * FROM zdemo_abap_fli INTO TABLE @it_8.

    "Using FINAL
    SELECT * FROM zdemo_abap_fli INTO TABLE @FINAL(it_9).

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Populating internal tables by adding a line (structure) using APPEND ... TO/INSERT ... INTO` ) ).

    TYPES: BEGIN OF st_a,
             num  TYPE i,
             str  TYPE string,
             char TYPE c LENGTH 2,
           END OF st_a,
           ty_tab_a TYPE TABLE OF st_a WITH EMPTY KEY.
    DATA it_a TYPE ty_tab_a.

    "Adding a line created inline
    APPEND VALUE #( num = 1 str = `A` char = 'bb' ) TO it_a.
    INSERT VALUE #( num = 2 str = `C` char = 'dd' ) INTO TABLE it_a.

    "Adding an existing line
    DATA(struc_a) = VALUE st_a( num = 3 str =  `E` char = 'ff' ).
    "Structure whose components are assigned individually using the
    "structure component selector
    DATA struc_b TYPE st_a.
    struc_b-num = 4.
    struc_b-str =  `G`.
    struc_b-char = 'hh'.

    APPEND struc_a TO it_a.
    INSERT struc_b INTO TABLE it_a.

    out->write( data = it_a name = `it_a` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Adding an initial line` ) ).

    APPEND INITIAL LINE TO it_a.
    INSERT INITIAL LINE INTO TABLE it_a.

    out->write( data = it_a name = `it_a` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Adding a line and assigning the added line to a field symbol or data reference variable` ) ).

    "Creating field symbol inline
    APPEND VALUE st_a( num = 5 str =  `I` char = 'jj' ) TO it_a ASSIGNING FIELD-SYMBOL(<fs_a>).
    "Addressing individual components
    ASSERT <fs_a>-num = 5.
    <fs_a>-num = 123.

    FIELD-SYMBOLS <fs_b> TYPE st_a.
    DATA(struc_c) = VALUE st_a( num = 6 ).
    INSERT struc_c INTO TABLE it_a ASSIGNING <fs_b>.
    <fs_b>-str =  `K`.

    "Adding an initial line
    "The examples use data reference variables.
    "Using inline declaration
    APPEND INITIAL LINE TO it_a REFERENCE INTO DATA(dref_a).
    dref_a->num = 7.
    DATA dref_b TYPE REF TO st_a.
    INSERT INITIAL LINE INTO TABLE it_a REFERENCE INTO dref_b.
    dref_b->num = 8.

    DO 3 TIMES.
      APPEND INITIAL LINE TO it_a REFERENCE INTO dref_b.
      dref_b->* = VALUE #( num = sy-index str = sy-index char = sy-index ).
    ENDDO.

    out->write( data = it_a name = `it_a` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Adding all lines from another internal table (LINES OF addition)` ) ).

    "Adding lines to one internal table that are all added to
    "another one
    DATA it_b TYPE ty_tab_a.
    INSERT VALUE #( num = 99 str =  `L` char = 'mm' ) INTO TABLE it_b.
    INSERT VALUE #( num = 100 str =  `N` char = 'oo' ) INTO TABLE it_b.

    APPEND LINES OF it_b TO it_a.
    INSERT LINES OF it_b INTO TABLE it_a.

    out->write( data = it_a name = `it_a` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) Adding multiple lines from another internal table with a specified index range` ) ).

    APPEND LINES OF it_a FROM 5 TO 7 TO it_b.
    APPEND LINES OF it_a FROM 12 TO it_b. "further lines up to the last line
    APPEND LINES OF it_a TO 3 TO it_b.  "all lines from the start up to the specified index
    INSERT LINES OF it_a FROM 8 TO 10 INTO TABLE it_b.

    out->write( data = it_b name = `it_b` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) Inserting one line or multiple lines from another internal table at a specific position (INDEX addition)` ) ).

    "To be used for index tables.

    INSERT VALUE #( num = 9 str =  `P` char = 'qq' ) INTO it_b INDEX 2.
    INSERT LINES OF VALUE ty_tab_a( ( num = 10 str =  `R` ) ( num = 11 str =  `S` ) ) INTO it_b INDEX 5.
    "FROM and TO can also be used
    INSERT LINES OF it_a FROM 1 TO 3 INTO it_b INDEX 1.

    out->write( data = it_b name = `it_b` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Adding lines using the VALUE operator` ) ).

    DATA(struc_d) = VALUE st_a( num = 11 str =  `T` char = 'uu' ).
    DATA it_c TYPE ty_tab_a.

    "Populating an existing internal table by assigning an internal table that is constructed inline

    "Adding an existing line and a line created inline
    it_c = VALUE #( ( struc_d )
                    ( num = 11 str =  `V` char = 'ww' ) ).

    out->write( data = it_c name = `it_c` ).
    out->write( |\n| ).

    "Creating an internal table by inline declaration and adding lines with VALUE
    DATA(it_d) = VALUE ty_tab_a( ( num = 12 str =  `X` char = 'yy' )
                                 ( num = 13 str =  `Z` char = 'aa' )
                                 ( struc_d ) ).

    out->write( data = it_d name = `it_d` ).
    out->write( |\n| ).

    "******* BASE addition *******
    "Adding new lines without deleting existing content
    it_d =  VALUE #( BASE it_d ( num = 14 str =  `B` char = 'cc' )
                               ( num = 15 str =  `D` char = 'ee' ) ).

    out->write( data = it_d name = `it_d` ).
    out->write( |\n| ).

    "******* LINES OF addition *******
    "Adding lines of other tables
    it_d = VALUE #( ( LINES OF it_c ) ). "No BASE addition, existing content is deleted

    it_c = VALUE #( BASE it_c ( num = 16 str =  `F` char = 'gg' )
                              ( LINES OF it_d ) ).

    out->write( data = it_d name = `it_d` ).
    out->write( |\n| ).
    out->write( data = it_c name = `it_c` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `9) CORRESPONDING operator / MOVE-CORRESPONDING statements` ) ).

    "Creating and populating demo internal tables
    TYPES: BEGIN OF st_b,
             num    TYPE i,
             char   TYPE c LENGTH 2,
             comp_a TYPE string,
           END OF st_b,
           ty_tab_b TYPE TABLE OF st_b WITH EMPTY KEY,
           BEGIN OF st_c,
             num    TYPE i,
             char   TYPE c LENGTH 2,
             comp_b TYPE string,
           END OF st_c,
           ty_tab_c TYPE TABLE OF st_c WITH EMPTY KEY.

    DATA(it_e_original) = VALUE ty_tab_b( ( num = 1 char = 'aa' comp_a = `B` )
                                          ( num = 2 char = 'cc' comp_a = `D` ) ).
    DATA(it_f_original) = VALUE ty_tab_c( ( num = 3 char = 'ee' comp_b = `F` )
                                          ( num = 4 char = 'gg' comp_b = `H` ) ).

    DATA(it_e) = it_e_original.
    DATA(it_f) = it_f_original.

    "Copying the content of another internal table respecting identically
    "named components

    "it_f -> it_e
    it_e = CORRESPONDING #( it_f ).

    out->write( `CORRESPONDING` ).
    out->write( data = it_e name = `it_e` ).
    out->write( |\n| ).

    "it_e -> it_f
    it_e = it_e_original.
    MOVE-CORRESPONDING it_e TO it_f.

    out->write( `MOVE-CORRESPONDING` ).
    out->write( data = it_f name = `it_f` ).
    out->write( |\n| ).

    "******* BASE addition / KEEPING TARGET LINES addition *******
    "Copying content and retaining existing content
    it_e = it_e_original.
    it_f = it_f_original.

    out->write( `CORRESPONDING ... BASE ...` ).
    it_e = CORRESPONDING #( BASE ( it_e ) it_f ).

    out->write( data = it_e name = `it_e` ).
    out->write( |\n| ).

    it_e = it_e_original.
    it_f = it_f_original.

    out->write( `MOVE-CORRESPONDING ... KEEPING TARGET LINES ...` ).
    MOVE-CORRESPONDING it_e TO it_f KEEPING TARGET LINES.

    out->write( data = it_f name = `it_f` ).
    out->write( |\n| ).

    "******* MAPPING addition *******
    "Assigning components using mapping relationships
    it_e = it_e_original.
    it_f = it_f_original.

    out->write( `CORRESPONDING ... MAPPING ...` ).
    it_e = CORRESPONDING #( it_f MAPPING comp_a = comp_b ).

    out->write( data = it_e name = `it_e` ).
    out->write( |\n| ).


    out->write( `CORRESPONDING ... BASE ... MAPPING ...` ).
    it_e = it_e_original.
    it_f = CORRESPONDING #( BASE ( it_f ) it_e MAPPING comp_b = comp_a ). "Retaining content with BASE

    out->write( data = it_f name = `it_f` ).
    out->write( |\n| ).

    "******* EXCEPT addition *******
    "Excluding components from the assignment
    it_e = it_e_original.
    it_f = it_f_original.

    out->write( `CORRESPONDING ... EXCEPT ...` ).
    it_e = CORRESPONDING #( it_f EXCEPT char ).

    out->write( data = it_e name = `it_e` ).
    out->write( |\n| ).

    it_e = it_e_original.
    "EXCEPT * means that all components remain initial not specified
    "for mapping
    out->write( `CORRESPONDING ... MAPPING ... EXCEPT ...` ).
    it_f = CORRESPONDING #( it_e MAPPING comp_b = comp_a EXCEPT * ). "Mapping components

    out->write( data = it_f name = `it_f` ).
    out->write( |\n| ).

    "******* DISCARDING DUPLICATES addition *******
    "Preventing runtime errors when duplicate lines are assigned
    it_e = VALUE #( ( num = 1 char = 'aa' comp_a = `B` )
                    ( num = 1 char = 'cc' comp_a = `D` ) ).

    DATA it_g TYPE SORTED TABLE OF st_b WITH UNIQUE KEY num.

    "The statement commented out raises the runtime error ITAB_DUPLICATE_KEY.
    "it_g = CORRESPONDING #( it_e ).

    out->write( `CORRESPONDING ... DISCARDING DUPLICATES ...` ).
    it_g = CORRESPONDING #( it_e DISCARDING DUPLICATES ).

    out->write( data = it_g name = `it_g` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) DEEP addition to the CORRESPONDING operator / EXPANDING NESTED TABLES addition to MOVE-CORRESPONDING statements` ) ).

    "Handling deep components such as nested internal tables

    "Creating and populating demo internal tables
    TYPES: BEGIN OF st_d,
             char_a TYPE c LENGTH 2,
             char_b TYPE c LENGTH 2,
           END OF st_d,
           BEGIN OF st_e,
             char_b TYPE c LENGTH 2,
             char_c TYPE c LENGTH 2,
           END OF st_e,
           BEGIN OF st_f,
             comp1 TYPE c LENGTH 2,
             comp2 TYPE c LENGTH 2,
             comp3 TYPE TABLE OF st_d WITH EMPTY KEY,
           END OF st_f,
           BEGIN OF st_g,
             comp2 TYPE c LENGTH 2,
             comp3 TYPE TABLE OF st_e WITH EMPTY KEY,
             comp4 TYPE c LENGTH 2,
           END OF st_g,
           ty_tab_d TYPE TABLE OF st_f WITH EMPTY KEY,
           ty_tab_e TYPE TABLE OF st_g WITH EMPTY KEY.

    DATA(it_h_original) = VALUE ty_tab_d(
      ( comp1 = 'a1' comp2 = 'a2' comp3 = VALUE #( ( char_a = 'a3' char_b = 'a4' ) ( char_a = 'a5' char_b = 'a6' ) ) )
      ( comp1 = 'b1' comp2 = 'b2' comp3 = VALUE #( ( char_a = 'b3' char_b = 'b4' ) ( char_a = 'b5' char_b = 'b6' ) ) ) ).

    DATA(it_i_original) = VALUE ty_tab_e(
      ( comp2 = 'c1' comp3 = VALUE #( ( char_b = 'c2' char_c = 'c3' ) ( char_b = 'c4' char_c = 'c5' ) ) comp4 = 'c6' )
      ( comp2 = 'd1' comp3 = VALUE #( ( char_b = 'd2' char_c = 'd3' ) ( char_b = 'd4' char_c = 'd5' ) ) comp4 = 'd6' ) ).

    DATA(it_h) = it_h_original.
    DATA(it_i) = it_i_original.

    "Compare the output of the examples
    out->write( `******* CORRESPONDING *******` ).
    "Note: The following example uses just CORRESPONDING. The outcome of the assignment
    "is a different one compared to using DEEP. Refer to the ABAP Keyword Documentation
    "for more details.
    it_h = CORRESPONDING #( it_i ).
    out->write( it_h ).

    out->write( `******* CORRESPONDING ... DEEP *******` ).
    it_h = CORRESPONDING #( DEEP it_i ).
    out->write( it_h ).

    out->write( `******* CORRESPONDING ... DEEP BASE *******` ).
    it_h = it_h_original.
    it_h = CORRESPONDING #( DEEP BASE ( it_h ) it_i ).
    out->write( it_h ).

    out->write( `******* MOVE-CORRESPONDING ... EXPANDING NESTED TABLES *******` ).
    it_h = it_h_original.
    MOVE-CORRESPONDING it_i TO it_h EXPANDING NESTED TABLES.
    out->write( it_h ).

    out->write( `******* MOVE-CORRESPONDING ... EXPANDING NESTED TABLES KEEPING TARGET LINES *******` ).
    it_h = it_h_original.
    MOVE-CORRESPONDING it_i TO it_h EXPANDING NESTED TABLES KEEPING TARGET LINES.
    out->write( it_h ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) CORRESPONDING with lookup table` ) ).

    "The following examples construct an internal tables by joining an internal table
    "and a lookup table and comparing their components.
    TYPES:
      BEGIN OF s_lk1,
        character TYPE c LENGTH 1,
        text      TYPE string,
      END OF s_lk1,
      it_type         TYPE STANDARD TABLE OF s_lk1 WITH EMPTY KEY,
      lookup_tab_type TYPE HASHED TABLE OF s_lk1 WITH UNIQUE KEY character.

    DATA(it_lk1) = VALUE it_type( ( character = 'a' ) ( character = 'b' ) ( character = 'c' ) ( character = 'd' )
                                ( character = 'e' ) ( character = 'f' ) ).
    DATA(it_lk1_copy) = it_lk1.

    DATA(lookup_tab) = VALUE lookup_tab_type( ( character = 'a' text = `lorem` )
                                               ( character = 'c' text = `ipsum` )
                                               ( character = 'e' text = `dolor` )
                                               ( character = 'f' text = `sit` ) ).

    "In the following example assignment, the internal table used for the comparison
    "is also the target table.
    it_lk1 = CORRESPONDING #( it_lk1 FROM lookup_tab USING character = character ).

    out->write( data = it_lk1 name = `it_lk1` ).
    out->write( |\n| ).

    "In the following example, the internal table used for the comparison
    "is not the target table. Instead, a new table is created inline.
    "The pragma suppresses a syntax warning.

    DATA(it_lk2) = CORRESPONDING it_type( it_lk1_copy FROM lookup_tab USING character = character ) ##operator.
    ASSERT it_lk2 = it_lk1.
    out->write( data = it_lk2 name = `it_lk2` ).
    out->write( |\n| ).

    "Example assignments to demonstrate the KEY and MAPPING additions
    TYPES:
      BEGIN OF s_lk2,
        a TYPE string,
        b TYPE string,
        c TYPE string,
        d TYPE string,
        e TYPE string,
        f TYPE string,
      END OF s_lk2,
      BEGIN OF s_lk3,
        a TYPE string,
        b TYPE string,
        c TYPE string,
        d TYPE string,
        e TYPE string,
        g TYPE string,
      END OF s_lk3,
      BEGIN OF s_lk4,
        h TYPE string,
        i TYPE string,
        j TYPE string,
        k TYPE string,
        l TYPE string,
        m TYPE string,
      END OF s_lk4.
    DATA:
      it_lk3       TYPE STANDARD TABLE OF s_lk2,
      it_lk4       TYPE STANDARD TABLE OF s_lk2,
      it_lk5       TYPE STANDARD TABLE OF s_lk2,
      lookup_table TYPE STANDARD TABLE OF s_lk3 WITH NON-UNIQUE SORTED KEY sk COMPONENTS c d,
      it_lk6       TYPE STANDARD TABLE OF s_lk4.

    it_lk3 = VALUE #( ( a = `1a`  b = `1b`
                     c = `---` d = `---`
                     e = `---` f = `---` )
                   ( a = `2a`  b = `2b`
                     c = `---` d = `---`
                     e = `---` f = `---` )
                   ( a = `3a`  b = `3b`
                     c = `---` d = `---`
                     e = `---` f = `---` ) ).

    it_lk4 = it_lk3.
    it_lk5 = it_lk3.

    lookup_table = VALUE #( ( a = `4a` b = `4b`
                              c = `1a` d = `1b`
                              e = `5a` g = `5b` )
                            ( a = `6a` b = `6b`
                              c = `3a` d = `3b`
                              e = `7a` g = `7b` ) ).

    "Notes on the example assignment:
    "- Internal table used for the comparison is also the target table
    "- The lookup table specifies a sorted secondary table key.
    "- The key is used after the USING KEY addition.
    "- All key components must be specified.
    "- Regarding the result:
    "  - Only the first and third lines are found in the lookup table.
    "  - Therefore, the values of the identically named components in it3
    "    are assigned (which is only one component in the example).
    "  - The assignment excludes the components c and d of the lookup table,
    "    although there are identically named components in it3. The components
    "    used in the condition specification are ignored. The other components
    "    retain their original values.
    "  - In the lookup table, no line is available with the values a = `2a` b = `2b`.
    "    Therefore, the result does not include values from the lookup table. The
    "    original component values of the line in it3 are used for the result.

    it_lk3 = CORRESPONDING #( it_lk3 FROM lookup_table USING KEY sk c = a d = b ).

    out->write( data = it_lk3 name = `it_lk3` ).
    out->write( |\n| ).

    "Notes on the example assignment:
    "- See above. Here, the MAPPING addition is included. It is used to specify
    "  mapping relationships for the assignments. The example specifies a mapping
    "  relationship for all available components in the demo tables. In doing so,
    "  the default mapping is overridden, and, all previously ignored components
    "  are not ignored anymore.
    "- As a consequence, all component values in the first and third lines are
    "  are affected and assigned values.
    "- As above, the second line retains the original values of it4 as there is
    "  no line found in the lookup table.

    it_lk4 = CORRESPONDING #( it_lk4 FROM lookup_table USING KEY sk c = a d = b MAPPING a = a b = b c = c d = d f = g ).

    out->write( data = it_lk4 name = `it_lk4` ).
    out->write( |\n| ).
    "Notes on the example assignment:
    "- The target table does not have the same type as it5. But, despite having differently
    "  named components, the types are compatible, and an assignment can be performed.
    "- As not the same internal table is used for the search in the CORRESPONDING expression and
    "  the target, a syntax warning would occur (a temporary copy of it5 must be created) if not
    "  hidden by the pragma.

    it_lk6 = CORRESPONDING #( it_lk5 FROM lookup_table USING KEY sk c = a d = b ) ##operator.

    out->write( data = it_lk6 name = `it_lk6` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `12) Creating anonymous internal tables with the NEW operator` ) ).

    TYPES: BEGIN OF s,
             a TYPE c LENGTH 3,
             b TYPE i,
           END OF s,
           tab_type TYPE TABLE OF s WITH EMPTY KEY.

    "Creating and populating an anonymous data object
    DATA(dref_tab) = NEW tab_type( ( a = 'aaa' b = 1 )
                                   ( a = 'bbb' b = 2 ) ).

    "Access by derefencing
    DATA(copy_deref_itab) = dref_tab->*.
    DATA(read_line) = dref_tab->*[ 2 ].
    DATA(read_comp) = dref_tab->*[ 1 ]-a.
    dref_tab->*[ 1 ]-a = 'zzz'.
    ASSERT dref_tab->*[ 1 ]-a = 'zzz'.
    INSERT VALUE s( a = 'yyy' b = 3 ) INTO TABLE dref_tab->*.
    out->write( data = dref_tab->* name = `dref_tab->*` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `13) FILTER: Filtering internal table by condition` ) ).

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

    out->write( zcl_demo_abap_aux=>heading( `14) READ TABLE: Reading a single line by index` ) ).

    "Creating and populating an internal table
    TYPES: BEGIN OF st_h,
             a TYPE i,
             b TYPE c LENGTH 2,
             c TYPE string,
           END OF st_h,
           ty_tab_f TYPE SORTED TABLE OF st_h WITH UNIQUE KEY a WITH NON-UNIQUE SORTED KEY sk COMPONENTS b.

    DATA(it_j) = VALUE ty_tab_f( ( a = 1 b = 'zz' c = `B` )
                                 ( a = 2 b = 'xx' c = `D` )
                                 ( a = 3 b = 'yy' c = `F` ) ).

    DATA struc_e TYPE st_h.
    "In the following example ...
    "- a work area is specified as target area.
    "- USING KEY is not specified, i.e. the primary table index is used by default.
    READ TABLE it_j INTO struc_e INDEX 2.

    "Reading into a work area that is created inline
    READ TABLE it_j INTO DATA(struc_f) INDEX 3.

    "Specifying other target areas: Field symbols and data reference variables
    "Here, the target areas are created inline
    READ TABLE it_j ASSIGNING FIELD-SYMBOL(<fs_c>) INDEX 1.
    READ TABLE it_j REFERENCE INTO DATA(dref_c) INDEX 1.

    "******* USING KEY addition *******
    "Reading by index and specifying which table index to use

    "In the following example, the primary key is specified explicitly and
    "addressed using the default name primary_key. It has the same effect
    "as the statement below because the primary table index is used by
    "default.
    READ TABLE it_j INTO struc_e INDEX 1 USING KEY primary_key.
    READ TABLE it_j INTO struc_e INDEX 1.

    "Specifying the secondary key to use the secondary table index
    READ TABLE it_j INTO struc_e INDEX 1 USING KEY sk.

    "Using alias names
    DATA it_j_alias TYPE SORTED TABLE OF st_h
     WITH UNIQUE KEY primary_key ALIAS pk COMPONENTS a
     WITH NON-UNIQUE SORTED KEY sk ALIAS sk_alias COMPONENTS b.

    it_j_alias = it_j.
    READ TABLE it_j_alias INTO struc_e INDEX 1 USING KEY pk.
    READ TABLE it_j_alias INTO struc_e INDEX 1 USING KEY sk_alias.
    "The following examples use the other key names
    READ TABLE it_j_alias INTO struc_e INDEX 1 USING KEY primary_key.
    READ TABLE it_j_alias INTO struc_e INDEX 1 USING KEY sk.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `15) System field setting with READ TABLE statements` ) ).

    READ TABLE it_j INTO struc_e INDEX 999.
    IF sy-subrc = 0.
      ...
    ELSE.
      ... "This branch is executed in the example since the line is not found.
      ASSERT sy-tabix = 0.
    ENDIF.

    READ TABLE it_j INTO struc_e INDEX 1.
    ASSERT sy-subrc = 0.
    ASSERT sy-tabix = 1.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `16) READ TABLE: Reading a single line using table keys` ) ).

    "Creating and populating a demo internal table
    TYPES: BEGIN OF st_i,
             num  TYPE i,
             str  TYPE string,
             char TYPE c LENGTH 2,
           END OF st_i.

    DATA it_k TYPE SORTED TABLE OF st_i
      WITH NON-UNIQUE KEY primary_key ALIAS pk COMPONENTS num
      WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS char.

    it_k = VALUE #( ( num = 1 str = `A` char = 'zz' )
                    ( num = 2 str = `C` char = 'yy' )
                    ( num = 3 str = `E` char = 'xx' ) ).

    "The following examples use a work area as target. Other target areas are
    "possible.

    "Primary table key
    READ TABLE it_k INTO DATA(struc_i) WITH TABLE KEY primary_key COMPONENTS num = 3.
    "Primary table key alias
    READ TABLE it_k INTO struc_i WITH TABLE KEY pk COMPONENTS num = 2.
    "Secondary table key
    READ TABLE it_k INTO struc_i WITH TABLE KEY sec_key COMPONENTS char = 'xx'.
    "Secondary table key alias
    READ TABLE it_k INTO struc_i WITH TABLE KEY sk COMPONENTS char = 'yy'.

    "Reading a line based on keys specified in a work area
    "It is a work area containing primary and secondary table key values.
    "the line type must be compatible to the internal table.
    TYPES st_j LIKE LINE OF it_k.
    DATA(pr_key) = VALUE st_j( num = 1 ).
    DATA(sec_key) = VALUE st_j( char = 'yy' ).

    READ TABLE it_k FROM pr_key INTO struc_i.

    "If USING KEY is not specified, the primary table key is used by default.
    "Explicitly specifying the primary table key
    READ TABLE it_k FROM pr_key USING KEY primary_key INTO struc_i.
    "Primary table key alias
    READ TABLE it_k FROM pr_key USING KEY pk INTO struc_i.
    "Secondary table key
    READ TABLE it_k FROM sec_key USING KEY sec_key INTO struc_i.
    "Secondary table key alias
    READ TABLE it_k FROM sec_key USING KEY sk INTO struc_i.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `17) READ TABLE: Reading a single line using a free key` ) ).

    "Note: Instead if READ TABLE ... WITH TABLE KEY ..., it is ... WITH KEY.

    READ TABLE it_k INTO DATA(struc_j) WITH KEY str = `A`.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `18) Examples for addressing individual components of read lines` ) ).

    "Examples for addressing individual components of read lines
    "The assertions emphasize the difference of work areas and field
    "symbols/data reference variables as target areas. Modifying the
    "contents of the field symbols/data reference variables means
    "modifying the internal table content.

    READ TABLE it_k INTO DATA(struc_k) WITH KEY str = `A`.
    struc_k-num = 123.
    DATA(comp_b) = struc_k-num.

    READ TABLE it_k ASSIGNING FIELD-SYMBOL(<fs_e>) WITH KEY str = `C`.
    "Note: The example table is a sorted table with 'num' as part of
    "a unique key. The field value cannot be modified.
    "<fs_e>-num = 123.
    <fs_e>-char = 'hi'.
    DATA(comp_c) = <fs_e>-char.

    READ TABLE it_k REFERENCE INTO DATA(dref_e) WITH KEY str = `E`.
    dref_e->char = '##'.
    DATA(comp_d) = dref_e->num.

    "It is also possible to specify the dereferencing operator together
    "with the component selector.
    DATA(comp_e) = dref_e->*-char.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `19) READ TABLE: COMPARING / TRANSPORTING additions` ) ).

    "Comparing fields and specifying fields to be transported
    TYPES ty_tab_h TYPE TABLE OF st_i WITH EMPTY KEY.
    DATA(it_m) = VALUE ty_tab_h( ( num = 1 str = `Z` char = '##' )
                                 ( num = 2 str = `Y` char = 'yy' )
                                 ( num = 3 str = `X` char = '##' )
                                 ( num = 4 str = `W` char = 'ww' )
                                 ( num = 5 str = `W` char = '##' )
                                 ( num = 6 str = `V` char = '##' )
                                 ( num = 7 str = `V` char = '##' )
                                 ( num = 7 str = `V` char = '##' )
                                 ( num = 8 str = `V` char = 'vv' ) ).

    "******* TRANSPORTING NO FIELDS addition *******
    "It is only checked whether the line exists. No target area is specified.
    "The system fields sy-subrc and sy-tabix are filled. Check also the
    "line_exists and line_index functions.

    READ TABLE it_m WITH KEY str = `X` TRANSPORTING NO FIELDS.
    ASSERT sy-subrc = 0.
    DATA(sysubrc) = sy-subrc.
    ASSERT sy-tabix = 3.
    DATA(sytabix) = sy-tabix.

    READ TABLE it_m WITH KEY str = `nope` TRANSPORTING NO FIELDS.
    ASSERT sy-subrc = 4.
    ASSERT sy-tabix = 0.

    "******* TRANSPORTING ... addition *******
    "Specifying fields to be transported; cannot be used with the ASSIGNING
    "and REFERENCE additions

    READ TABLE it_m INTO DATA(struc_l) INDEX 1 TRANSPORTING num char.
    ASSERT struc_l-str IS INITIAL.

    "If ALL FIELDS is specified, all fields are assigned, which corresponds to the
    "example below.
    READ TABLE it_m INTO struc_l INDEX 1 TRANSPORTING ALL FIELDS.
    READ TABLE it_m INTO struc_l INDEX 1.

    "******* COMPARING addition *******
    "- Can be used together with and in front of TRANSPORTING ...
    "- Compares the specified components
    "- ALL FIELDS compares all components, NO FIELDS compares no components
    "- Setting of sy-subrc: 0 is set if the content of compared components is identical,
    "  otherwise it is 2. Found lines are nevertheless assigned independently of the comparison.

    "The following examples use a WHILE loop to read all table lines (sy-index represents the
    "index value of the primary table index) into a work area.
    "The work area is filled before the read for the comparison. Depending on the comparison
    "result (by checking the sy-subrc value), the lines are added to different internal tables
    "for demonstration purposes. In addition, the 'num' component value is added to a string.
    "The examples explore several syntax options.

    DATA struc_m LIKE LINE OF it_m.
    DATA it_n LIKE it_m.
    DATA it_o LIKE it_m.
    DATA nums_subrc_0 TYPE string.
    DATA nums_subrc_2 TYPE string.
    DATA(subrc) = 0.

    "Specifying ALL FIELDS
    WHILE subrc = 0.
      DATA(idx) = sy-index.
      struc_m = VALUE #( num = 7 str = `V` char = '##' ).
      READ TABLE it_m INTO struc_m INDEX idx COMPARING ALL FIELDS TRANSPORTING ALL FIELDS.
      subrc = COND #( WHEN sy-subrc = 0 THEN 0 ELSE sy-subrc ).
      IF subrc = 0.
        APPEND struc_m TO it_n.
        nums_subrc_0 &&= struc_m-num.
      ELSEIF subrc = 2.
        APPEND struc_m TO it_o.
        nums_subrc_2 &&= struc_m-num.
        subrc = 0.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.

    ASSERT nums_subrc_0 = `77`.
    ASSERT nums_subrc_2 = `1234568`.
    CLEAR: subrc, struc_m, it_n, it_o, nums_subrc_0, nums_subrc_2.

    "Specifying specific fields for the comparison and transport
    WHILE subrc = 0.
      idx = sy-index.
      struc_m = VALUE #( num = 1234 str = `NOPE` char = '##' ).
      READ TABLE it_m INTO struc_m INDEX idx COMPARING char TRANSPORTING num.
      subrc = COND #( WHEN sy-subrc = 0 THEN 0 ELSE sy-subrc ).
      IF subrc = 0.
        APPEND struc_m TO it_n.
        nums_subrc_0 &&= struc_m-num.
      ELSEIF subrc = 2.
        APPEND struc_m TO it_o.
        nums_subrc_2 &&= struc_m-num.
        subrc = 0.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.

    ASSERT nums_subrc_0 = `135677`.
    ASSERT nums_subrc_2 = `248`.
    CLEAR: subrc, struc_m, it_n, it_o, nums_subrc_0, nums_subrc_2.

    WHILE subrc = 0.
      idx = sy-index.
      struc_m = VALUE #( num = 9999 char = '##' str = `V` ).
      READ TABLE it_m INTO struc_m INDEX idx COMPARING char str TRANSPORTING num.
      subrc = COND #( WHEN sy-subrc = 0 THEN 0 ELSE sy-subrc ).
      IF subrc = 0.
        APPEND struc_m TO it_n.
        nums_subrc_0 &&= struc_m-num.
      ELSEIF subrc = 2.
        APPEND struc_m TO it_o.
        nums_subrc_2 &&= struc_m-num.
        subrc = 0.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.

    ASSERT nums_subrc_0 = `677`.
    ASSERT nums_subrc_2 = `123458`.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `20) READ TABLE: CASTING / ELSE UNASSIGN additions` ) ).

    "Additions when assigning the read result to a field symbol

    TYPES c3 TYPE c LENGTH 3.
    TYPES ty_tab_g TYPE TABLE OF c3 WITH EMPTY KEY.
    DATA(itq) = VALUE ty_tab_g( ( 'abc' ) ( 'def' ) ).

    "Field symbol created inline (i.e. with the generic type 'data' implicitly)
    "In this case, the CASTING and ELSE UNASSIGN additions are not available.
    READ TABLE itq ASSIGNING FIELD-SYMBOL(<fs_k>) INDEX 1.

    "******* CASTING addition *******
    "To use the addition, the field symbol must be either completely typed, or
    "typed with one of the generic built-in ABAP types c, n, p, or x.
    TYPES c2 TYPE c LENGTH 2.
    FIELD-SYMBOLS <fs_l> TYPE c2.

    READ TABLE itq ASSIGNING <fs_l> CASTING INDEX 2.
    ASSERT <fs_l> = 'de'.

    "******* ELSE UNASSIGN addition *******
    "The field symbol is unassigned if no table line is found. The addition
    "can be used together with the CASTING addition.
    "The following example loops 3 times across an internal table that has
    "two lines. The sy-index value is used as the index value of the READ TABLE
    "statement. The example demonstrates that when a line is not found, the field
    "symbol is unassigned. In case of the first READ TABLE statement that does not
    "specify ELSE UNASSIGN, the field symbol remains assigned.

    DATA string_a TYPE string.
    FIELD-SYMBOLS <fs_o> TYPE c2.
    DO 3 TIMES.
      READ TABLE itq ASSIGNING FIELD-SYMBOL(<fs_m>) INDEX sy-index.
      READ TABLE itq ASSIGNING FIELD-SYMBOL(<fs_n>) ELSE UNASSIGN INDEX sy-index.
      READ TABLE itq ASSIGNING <fs_o> CASTING ELSE UNASSIGN INDEX sy-index.
      IF sy-index = 3.
        ASSERT <fs_m> = `def`.
        ASSERT <fs_n> IS NOT ASSIGNED.
        ASSERT <fs_o> IS NOT ASSIGNED.
      ENDIF.
    ENDDO.

   out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `21) READ TABLE: Specifying a WHERE Condition` ) ).

    "Creating and populating demo internal tables
    TYPES: BEGIN OF s_where,
             comp1 TYPE i,
             comp2 TYPE c LENGTH 5,
             comp3 TYPE i,
             comp4 TYPE c LENGTH 5,
           END OF s_where,
           t_type_so TYPE SORTED TABLE OF s_where WITH UNIQUE KEY comp1 WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp3.

    DATA(itab_wh) = VALUE t_type_so( ( comp1 = 1 comp2 = 'lorem' comp3 = 30 comp4 = 'ipsum' )
                                  ( comp1 = 2 comp2 = 'dolor' comp3 = 20 comp4 = 'sit' )
                                  ( comp1 = 3 comp2 = 'amet' comp3 = 40 comp4 = 'hello' )
                                  ( comp1 = 4 comp2 = 'world' comp3 = 50 comp4 = 'ABAP' )
                                  ( comp1 = 5 comp2 = 'test' comp3 = 10 comp4 = '' ) ).

    DATA wa_wh TYPE s_where.

    "--------- WHERE condition with comparison expressions ---------

    "Examples: =/EQ, <>/NE, >/GT, </LT, >=,GE, <=/LE,
    "          CO, CN, CA, NA, CS, NS, CP, NP,
    "          [NOT] BETWEEN ... AND
    "          [NOT] IN ranges_tables

    READ TABLE itab_wh INTO wa_wh WHERE comp1 > 3.
    ASSERT sy-tabix = 4.

    "'or' also available in other lines in this component, but the first found line
    "is returned (lines 1, 2, 4)
    READ TABLE itab_wh INTO wa_wh WHERE comp2 CS 'or'.
    ASSERT sy-tabix = 1.

    "'d' occurs in lines 2, 4
    READ TABLE itab_wh INTO wa_wh WHERE comp2 CS 'd'.
    ASSERT sy-tabix = 2.

    READ TABLE itab_wh INTO wa_wh WHERE comp2 CS 'd'.
    ASSERT sy-tabix = 2.

    "--------- WHERE condition with predicate expressions ---------

    "Examples: IS [NOT] INITIAL
    "          IS [NOT] BOUND
    "          IS [NOT] INSTANCE OF

    READ TABLE itab_wh INTO wa_wh WHERE comp1 > 4 AND comp4 IS INITIAL.
    ASSERT sy-tabix = 5.

    "--------- WITH KEY instead of WHERE condition ---------

    "Syntax warning in READ TABLE ... WHERE ... statements
    "READ TABLE itab_wh INTO wa_wh WHERE comp4 IS INITIAL.
    "ASSERT sy-tabix = 5.

    "For a better performance, the previous statement should be
    "replaced by a READ TABLE ... WITH KEY ... statement.
    READ TABLE itab_wh INTO wa_wh WITH KEY comp4 = ''.
    ASSERT sy-tabix = 5.

    "You can also suppress the syntax warning by a pragma.
    READ TABLE itab_wh INTO wa_wh WHERE comp4 IS INITIAL ##read_where_ok.
    ASSERT sy-tabix = 5.

    "------------------- Further additions -------------------

    "TRANSPORTING NO FIELDS addition is possible
    READ TABLE itab_wh TRANSPORTING NO FIELDS WHERE comp2 CS 'd'.
    ASSERT sy-tabix = 2.

    "USING KEY addition
    READ TABLE itab_wh USING KEY primary_key INTO wa_wh WHERE comp2 CS 't'.
    ASSERT sy-tabix = 3.

    READ TABLE itab_wh USING KEY sk INTO wa_wh WHERE comp3 > 40.
    ASSERT sy-tabix = 5.

    "------------------- Excursions -------------------

    "Note the comparison rules for character-like data types
    READ TABLE itab_wh INTO wa_wh WHERE comp2 = 'lorem' ##read_where_ok.
    ASSERT sy-tabix = 1.

    "In the following case, the length of the comp2 value increased to match the
    "length of the specified text field, i.e. the surplus characters from the right
    "side text field are not truncated. As a consequence, the line is not found.

    DATA(some_text) = 'loremXYZ'.

    READ TABLE itab_wh INTO wa_wh WHERE comp2 = some_text ##read_where_ok.
    ASSERT sy-tabix = 0 AND sy-subrc <> 0.

    "When using READ TABLE ... WITH KEY ... the behavior is different. In that
    "case, the surplus characters are truncated because of a conversion. Therefore,
    "the following statement finds a line.
    READ TABLE itab_wh INTO wa_wh WITH KEY comp2 = some_text.
    ASSERT sy-tabix = 1 AND sy-subrc = 0.

    "Note: The read target can only be placed before WHERE conditions.
    "The following statements are not possible.
    "READ TABLE itab WHERE comp2 CS 'd' INTO wa.
    "READ TABLE itab WHERE comp2 CS 'd' TRANSPORTING NO FIELDS.

    "READ TABLE ... WHERE ... statements can replace LOOP AT ... WHERE ...
    "statements including EXIT.
    LOOP AT itab_wh INTO wa_wh WHERE comp2 CS 'd'.
      ASSERT sy-tabix = 2.
      EXIT.
    ENDLOOP.

    "------------------- Dynamic WHERE condition -------------------

    "Character-like data objects or standard tables with character-like line type
    "can be specified

    DATA(dyn_where_cond_str) = `comp2 CS 'd'`.

    READ TABLE itab_wh INTO wa_wh WHERE (dyn_where_cond_str).
    ASSERT sy-tabix = 2.

    DATA(dyn_where_cond_tab) = VALUE string_table( ( `comp2` ) ( `CS` ) ( `'d'` ) ).
    READ TABLE itab_wh INTO wa_wh WHERE (dyn_where_cond_tab).
    ASSERT sy-tabix = 2.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `22) Table expressions: Reading table lines by index` ) ).

    "Creating and populating demo internal tables
    "Note: These demo tables are relevant for most of the code snippets
    "in this section.
    TYPES: BEGIN OF s_demo,
             comp1 TYPE i,
             comp2 TYPE i,
             comp3 TYPE i,
             comp4 TYPE c LENGTH 3,
           END OF s_demo,
           ttyp        TYPE SORTED TABLE OF s_demo WITH UNIQUE KEY comp1 WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp2 comp3,
           ttyp_hashed TYPE HASHED TABLE OF s_demo WITH UNIQUE KEY comp1 WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp2 comp3,
           ttyp2       TYPE SORTED TABLE OF s_demo WITH UNIQUE KEY comp1 comp2 WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp3.

    DATA(itab) = VALUE ttyp( ( comp1 = 1 comp2 = 30 comp3 = 31 comp4 = 'aaa' )
                             ( comp1 = 2 comp2 = 20 comp3 = 21 comp4 = 'bbb' )
                             ( comp1 = 3 comp2 = 10 comp3 = 11 comp4 = 'ccc' ) ).

    DATA itab_hashed TYPE ttyp_hashed.
    itab_hashed = itab.
    DATA itab_so TYPE ttyp2.
    itab_so = itab.

    DATA line TYPE s_demo.

    "------ Reading table line by index------
    "Just specifying the index number means referring to the primary table index.
    "In this case, the internal table must be an index table.

    "In the example, the entire table line is assigned to a variable
    line = itab[ 2 ].

    "KEY ... INDEX ... additions
    "For reading a line according to a table index.
    "The following example has the same effect as above. Here, the default
    "name of the primary key is specified explicitly.
    line = itab[ KEY primary_key INDEX 2 ].

    "Secondary table key specified, using secondary table index
    line = itab[ KEY sk INDEX 1 ].

    "This syntax is not possible for hashed tables.
    "DATA(line_hashed_tab1) = itab_hashed[ 2 ].
    "DATA(line_hashed_tab2) = itab_hashed[ KEY primary_key INDEX 2 ].
    "Secondary table index access is possible for hashed tables
    DATA(line_hashed_tab3) = itab_hashed[ KEY sk INDEX 2 ].

   out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `23) Table expressions: Reading table lines by table key` ) ).

    "------------------ TABLE KEY addition ------------------

    "Explicitly specifying the primary table key
    line = itab[ TABLE KEY primary_key COMPONENTS comp1 = 1 ].

    "The following statement is not possible as no other components can be specified.

    "line = itab[ TABLE KEY primary_key COMPONENTS comp1 = 1 comp2 = 30 ].

    "The addition COMPONENTS is optional; the following example is the same as above
    line = itab[ TABLE KEY primary_key comp1 = 1 ].

    "Specifying a secondary table key
    line = itab[ TABLE KEY sk COMPONENTS comp2 = 20 comp3 = 21 ].

    "Optional COMPONENTS addition
    line = itab[ TABLE KEY sk comp2 = 20 comp3 = 21 ].

    "Fully specifying the table key components is required with TABLE KEY. So, the
    "following statement is not possible.

    "line = itab[ TABLE KEY sk comp2 = 20 ].

    "------------------ KEY addition ------------------

    "Using KEY and specifying all key components work like specifying TABLE KEY
    line = itab[ KEY primary_key COMPONENTS comp1 = 1 ].
    line = itab[ KEY primary_key comp1 = 1 ].
    line = itab[ KEY sk COMPONENTS comp2 = 20 comp3 = 21 ].
    line = itab[ KEY sk comp2 = 20 comp3 = 21 ].

    "Unlike TABLE KEY, KEY does not enforce all key components to be specified
    line = itab[ KEY sk comp2 = 20 ].

    "In case of sorted and secondary table keys, other components not being part
    "of the key can be specified
    line = itab[ KEY primary_key comp1 = 1 comp4 = 'aaa' ].
    line = itab[ KEY sk comp2 = 20 comp4 = 'bbb' ].

    "The following statements are not possible. The initial, left part of
    "the key must be specified. In the example case, it is comp2.

    "line = itab[ KEY sk comp3 = 21 comp4 = 'bbb' ].
    "line = itab[ KEY sk comp4 = 'bbb' ].

    "The following statement triggers a syntax warning because the initial
    "part of a table key is specified, but the key name is not specified.
    "In this case, the search is not optimized as the component is not
    "part of the primary table key of the sorted table. You may optimize
    "it by specifying the key.

    "line = itab[ comp2 = 10 ].

    "The syntax warning can be suppressed by a pragma.
    line = itab[ comp2 = 10 ] ##primkey[sk].

    "Specifying the key name
    line = itab[ KEY sk comp2 = 10 ].

    "------------------ No TABLE KEY/KEY additions ------------------

    "Specifying a free key search, but including all components of the primary
    "table key
    "For a sorted table as in the example, the search is fully optimized.
    line = itab[ comp1 = 1 ].

    "Partly optimized (only a part of the primary table key of the sorted
    "example table is specified)
    line = itab_so[ comp1 = 1 ].

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `24) Table expressions: Reading table lines using free keys` ) ).

    "The search is and cannot be optimized as the component is not part of
    "the primary table key of the sorted table. Plus, no appropriate
    "secondary table key can be applied.
    line = itab[ comp4 = 'ccc' ].

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `25) Table expressions: Assigning table lines to a field symbol, data reference variables pointing to a table line` ) ).

    "Assigning table lines to a field symbol

    "Works like READ TABLE ... ASSIGNING ...
    ASSIGN itab[ 2 ] TO FIELD-SYMBOL(<line>).
    "Note: Table expressions do not set the sy-tabix
    "value, except when used with ASSIGN.
    ASSERT sy-tabix = 2.

    "Note: Assigning a non-existent line results in sy-subrc = 4
    "An exception is not raised.
    ASSIGN itab[ 99 ] TO <line>.
    ASSERT sy-subrc = 4.

    "Data reference variables pointing to a table line
    DATA dref_te TYPE REF TO data.
    dref_te = NEW s_demo(  ).
    dref_te->* = itab[ 1 ].

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `26) Table expressions: Specifying table expressions as operands in constructor expressions with VALUE and REF` ) ).

    line = VALUE #( itab[ 2 ] ).
    "Works like READ TABLE ... REFERENCE INTO ...
    DATA(line_ref) = REF #( itab[ 3 ] ).

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `27) Table expressions: Specifying a default value for lines that are not found to avoid an exception` ) ).

    TRY.
        line = itab[ 4 ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    line = VALUE #( itab[ 4 ] OPTIONAL ).

    line = VALUE #( itab[ 5 ] DEFAULT itab[ 1 ]  ).
    line = VALUE #( itab[ 6 ] DEFAULT VALUE #( ) ).

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `28) Table expressions: Field symbols and dereferenced data references specified before the square brackets` ) ).

    ASSIGN itab TO FIELD-SYMBOL(<tab>).
    line = <tab>[ 1 ].

    DATA dref_t TYPE REF TO ttyp.
    dref_t = NEW #(  ).

    dref_t->* = itab.
    line = dref_t->*[ 2 ].

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `29) Table expressions: Reading individual components of table lines` ) ).

    "Read component via line read using ...
    "... index
    DATA(compa) = itab[ 1 ]-comp1.
    "... table key
    DATA(compb) = itab[ TABLE KEY primary_key comp1 = 1 ]-comp2.
    DATA(compc) = itab[ TABLE KEY sk comp2 = 30 comp3 = 31 ]-comp1.
    "... free key
    DATA(compd) = itab[ comp4 = 'ccc' ]-comp1.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `30) Table expressions: Chaining table expressions in the context of nested internal tables` ) ).

    "Creating deep internal table
    TYPES: BEGIN OF s_sub,
             comp1 TYPE i,
             comp2 TYPE i,
           END OF s_sub,
           tab_type_sub TYPE TABLE OF s_sub WITH EMPTY KEY,
           BEGIN OF s_super,
             compa TYPE i,
             compb TYPE TABLE OF tab_type_sub WITH EMPTY KEY,
           END OF s_super,
           table_type TYPE TABLE OF s_super WITH EMPTY KEY.

    "Expressions helpful when populating
    DATA(deep_tab) = VALUE table_type( ( compa = 1
                                       compb = VALUE #( ( VALUE #( ( comp1 = 3 comp2 = 4 ) ( comp1 = 5 comp2 = 6 ) ) )
                                                        ( VALUE #( ( comp1 = 7 comp2 = 8 ) ( comp1 = 9 comp2 = 10 ) ) ) ) )
                                        ( compa = 2
                                          compb = VALUE #( ( VALUE #( ( comp1 = 11 comp2 = 12 ) ( comp1 = 13 comp2 = 14 ) ) )
                                                           ( VALUE #( ( comp1 = 15 comp2 = 16 ) ( comp1 = 17 comp2 = 18 ) ) ) ) ) ).

    DATA(num1) = deep_tab[ 2 ]-compb[ 1 ][ 2 ]-comp2.
    ASSERT num1 = 14.

    "Such a statement instead of, for example, multiple statements as follows.
    READ TABLE deep_tab INDEX 2 INTO DATA(wa1).
    READ TABLE wa1-compb INDEX 1 INTO DATA(wa2).
    READ TABLE wa2 INTO DATA(wa3) INDEX 2.

    DATA(num2) = wa3-comp2.

    ASSERT num2 = num1.

    "Table expression result having a reference type enabling chainings with the object component selector

    DATA itab_ref TYPE TABLE OF REF TO s_demo WITH EMPTY KEY.
    itab_ref = VALUE #( ( NEW s_demo( comp1 = 1 comp2 = 30 comp3 = 31 comp4 = 'aaa' ) ) ).

    "Reading entire line by dereferencing
    DATA(deref_line) = itab_ref[ 1 ]->*.
    "Reading component by dereferencing
    DATA(dref_compa) = itab_ref[ 1 ]->comp3.
    "The following syntax is also possible (dereferencing operator followed
    "by the component selector).
    DATA(dref_compb) = itab_ref[ 1 ]->*-comp4.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `31) Table expressions in write positions: Writes on the entire line and writes on individual components` ) ).

    "The demo table is a key table. Therefore, writes on entire lines produce runtime errors.
    "itab[ 3 ] = VALUE #( ).

    "Creating a standard table
    DATA itab_std TYPE TABLE OF s_demo WITH NON-UNIQUE KEY comp1 WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp2 comp3.
    itab_std = itab.

    "Here, writes on entire lines are allowed.
    itab_std[ 3 ] = VALUE #( comp1 = 123 comp4 = 'zzz' ).
    CLEAR itab_std[ 3 ].

    "Table expressions in write positions: Writes on individual components
    itab[ 3 ]-comp4 = 'yyy'.
    itab_ref[ 1 ]->comp3 = 123.
    "No key value change allowed in key tables
    "The following statement causes a runtime error.
    "itab[ 1 ]-comp1 = 987.

    "Key value change allowed for standard tables.
    itab_std[ 3 ]-comp1 = 456.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `32) Checking the Existence of a Line in an Internal Table` ) ).

    "Read using a key
    READ TABLE itab_std WITH KEY comp1 = 2 TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      DATA(tab_idx2) = sy-tabix.
    ENDIF.

    "Read using the index
    READ TABLE itab_std INDEX 1 TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      ...
    ENDIF.

    "Read using the key
    IF line_exists( itab_std[ comp1 = 2 ] ).
      ...
    ENDIF.

    "Read using the index
    IF line_exists( itab_std[ 2 ] ).
      ...
    ENDIF.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `33) Checking the index of a line in an internal table` ) ).

    DATA(itab_idx) = VALUE string_table( ( `aaa` ) ( `bbb` ) ).
    READ TABLE itab_idx WITH KEY table_line = `bbb` TRANSPORTING NO FIELDS.

    DATA(table_index) = sy-tabix.

    table_index = line_index( itab_idx[ table_line = `aaa` ] ).

    "Note: No primary table index with hashed tables
    DATA(hashed_tab) = VALUE string_hashed_table( ( `a` ) ( `b` ) ( `c` ) ).

    table_index = line_index( hashed_tab[ table_line = `c` ] ).

    "Index access in hashed tables only using a secondary table index
    DATA hashed_tab2 TYPE TABLE OF string WITH EMPTY KEY WITH NON-UNIQUE SORTED KEY sk COMPONENTS table_line.
    hashed_tab2 = hashed_tab.

    DATA(hashed_secondary_idx) = line_index( hashed_tab2[ KEY sk table_line = `c` ] ).

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `34) Checking How Many Lines Exist in an Internal Table` ) ).


    DATA(itab_li1) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ( `d` ) ( `e` ) ).

    DATA(number_of_lines) = lines( itab_li1 ).

    "Excursion: Finding out the number of lines in a table by specifying concrete
    "component values, e.g. you want to find out how many lines exist in the table
    "that have the value 1 for comp2
    TYPES: BEGIN OF struct,
             comp1 TYPE c LENGTH 3,
             comp2 TYPE i,
           END OF struct,
           ttype TYPE TABLE OF struct WITH EMPTY KEY.

    DATA(itab_li2) = VALUE ttype( ( comp1 = 'a' comp2 = 1  )
                               ( comp1 = 'b' comp2 = 1  )
                               ( comp1 = 'c' comp2 = 1  )
                               ( comp1 = 'd' comp2 = 2  )
                               ( comp1 = 'e' comp2 = 3  )
                               ( comp1 = 'f' comp2 = 4  )
                               ( comp1 = 'g' comp2 = 5  ) ).


    DATA(line_num) = lines( itab_li2 ).

    "Finding out the number of lines in a table by component value, e.g.
    "using constructor expressions and specifying a WHERE clause.
    "The example creates an new internal table inline using VALUE and a FOR loop,
    "specified with a WHERE clause. The lines function is applied to the
    "table created inline.

    DATA(line_num_filtered1) = lines( VALUE ttype( FOR wa IN itab_li2 WHERE ( comp2 = 1 ) ( wa ) ) ).

    "Using the REDUCE operator
    "The example adds 1 to the resulting integer if the comp2 value of the iterated line is greater than 1.
    "The lines function is not relevant in the example.

    DATA(line_num_filtered2) = REDUCE i( INIT var = 0
                                         FOR <tline> IN itab_li2
                                         WHERE ( comp2 > 1 )
                                         NEXT var += 1 ).

    "Using the FILTER operator
    "Note: The source table must have at least one sorted key or a hash key for accessing.
    "If the table does not have such a primary table key, a secondary table key must be available.
    TYPES: tab_type_sorted TYPE TABLE OF struct WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS comp2.
    DATA it_sorted TYPE tab_type_sorted.
    it_sorted = itab_li2.

    "The example creates an new internal table inline using FILTER,
    "specified with a WHERE clause. The lines function is applied to the
    "table created inline.

    DATA(line_num_filtered3) = lines( FILTER #( it_sorted USING KEY sec_key WHERE comp2 = 1 ) ).

    DATA(line_num_filtered4) = lines( FILTER #( it_sorted USING KEY sec_key WHERE comp2 > 1 ) ).

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `35) Getting Table (Type) Information at Runtime` ) ).

    TYPES tab_info_type TYPE SORTED TABLE OF zdemo_abap_flsch
           WITH UNIQUE KEY carrid connid
           WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS countryfr cityfrom.
    DATA it_rtti TYPE tab_info_type.

    DATA(tdo_d) = cl_abap_typedescr=>describe_by_data( it_rtti ).
    "DATA(tdo_d) = cl_abap_typedescr=>describe_by_name( 'TAB_INFO_TYPE' ).

    "Cast to get more specific information
    DATA(tdo_itab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( it_rtti ) ).
    "DATA(tdo_itab) = CAST cl_abap_tabledescr( tdo_d ).

    DATA(type_category_itab) = tdo_itab->kind.
    DATA(relative_name_itab) = tdo_itab->get_relative_name( ).
    ... "Explore more options by positioning the cursor behind -> and choosing CTRL + Space
    DATA(table_kind_itab) = tdo_itab->table_kind.
    DATA(table_keys_itab) = tdo_itab->key.
    DATA(table_keys_more_details_itab) = tdo_itab->get_keys( ).
    DATA(table_has_unique_key_itab) = tdo_itab->has_unique_key.
    DATA(table_key_alias_itab) = tdo_itab->get_key_aliases( ).
    DATA(line_type_itab) = tdo_itab->get_table_line_type( ).
    DATA(table_component_info_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) ).
    DATA(table_components_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) )->components.
    DATA(table_comps_more_info_itab) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) )->get_components( ).
    DATA(applies_to_data_itab) = tdo_itab->applies_to_data( VALUE tab_type( ) ).

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Processing Multiple Internal Table Lines Sequentially` ) ).

    out->write( `36) Loop statements with different targets` ).

    TYPES: BEGIN OF s_loop,
             compa TYPE i,
             compb TYPE string,
             compc TYPE i,
           END OF s_loop,
           ttype_loop TYPE SORTED TABLE OF s_loop WITH UNIQUE KEY primary_key ALIAS pk COMPONENTS compa
           WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS compc.

    DATA(it_loop) = VALUE ttype_loop( ( compa = 1 compb = `aaa` compc = 50 )
    ( compa = 2 compb = `bbb` compc = 20 )
    ( compa = 3 compb = `ccc` compc = 40 )
    ( compa = 4 compb = `ddd` compc = 30 )
    ( compa = 5 compb = `eee` compc = 10 )
    ).

    DATA(tabix_counter) = 0.

    "The target is an existing work area.
    DATA wal LIKE LINE OF it_loop.

    LOOP AT it_loop INTO wal.
      "No addition of the loop statement; all lines are processed
      "Statements in this block are relevant for each individual table line.

      tabix_counter += 1.
    ENDLOOP.

    ASSERT tabix_counter = lines( it_loop ).
    CLEAR tabix_counter.

    "Work area declared inline
    LOOP AT it_loop INTO DATA(wal_inl).
      tabix_counter += 1.
    ENDLOOP.

    ASSERT tabix_counter = lines( it_loop ).
    CLEAR tabix_counter.

    "Field symbols
    FIELD-SYMBOLS <fslo> LIKE LINE OF it_loop.

    LOOP AT it_loop ASSIGNING <fslo>.
      tabix_counter += 1.
    ENDLOOP.

    ASSERT tabix_counter = lines( it_loop ).
    CLEAR tabix_counter.

    LOOP AT it_loop ASSIGNING FIELD-SYMBOL(<fslo_inl>).
      tabix_counter += 1.
    ENDLOOP.

    ASSERT tabix_counter = lines( it_loop ).
    CLEAR tabix_counter.

    "Data reference variables
    DATA dref_lo TYPE REF TO s_loop.

    LOOP AT it_loop REFERENCE INTO dref_lo.
      tabix_counter += 1.
    ENDLOOP.

    ASSERT tabix_counter = lines( it_loop ).
    CLEAR tabix_counter.

    LOOP AT it_loop REFERENCE INTO DATA(dref_lo_inl).
      tabix_counter += 1.
    ENDLOOP.

    ASSERT tabix_counter = lines( it_loop ).
    CLEAR tabix_counter.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `37) Loop statements with different table key specifications` ) ).
    "The specified table key affects the order in which the table lines
    "are accessed and the evaluation of the other conditions.

    DATA loop_str TYPE string.

    LOOP AT it_loop INTO wal USING KEY primary_key.
      loop_str &&= wal-compa.
    ENDLOOP.
    out->write( loop_str ).
    CLEAR loop_str.

    LOOP AT it_loop INTO wal USING KEY pk.
      loop_str &&= wal-compa.
    ENDLOOP.
    out->write( loop_str ).
    CLEAR loop_str.

    LOOP AT it_loop INTO wal USING KEY sec_key.
      loop_str &&= wal-compa.
    ENDLOOP.
    out->write( loop_str ).
    CLEAR loop_str.

    LOOP AT it_loop INTO wal USING KEY sk.
      loop_str &&= wal-compa.
    ENDLOOP.
    out->write( loop_str ).
    CLEAR loop_str.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `38) Restricting the Area of a Table to Be Looped Over` ) ).
    "FROM/TO: Only for index tables

    "Specifying an index range
    LOOP AT it_loop INTO wal FROM 2 TO 4.
      loop_str &&= wal-compa.
    ENDLOOP.
    out->write( loop_str ).
    CLEAR loop_str.

    "From specified line until the end
    LOOP AT it_loop INTO wal FROM 2.
      loop_str &&= wal-compa.
    ENDLOOP.
    out->write( loop_str ).
    CLEAR loop_str.

    "From first line until the specified line
    LOOP AT it_loop INTO wal TO 4.
      loop_str &&= wal-compa.
    ENDLOOP.
    out->write( loop_str ).
    CLEAR loop_str.

    LOOP AT it_loop INTO wal WHERE compa >= 3 AND compb IS NOT INITIAL.
      loop_str &&= wal-compa.
    ENDLOOP.
    out->write( loop_str ).
    CLEAR loop_str.

    "No interest in the table content; only relevant system fields are populated

    "Mandatory WHERE clause
    LOOP AT it_loop TRANSPORTING NO FIELDS WHERE compa < 4.
      loop_str &&= sy-tabix.
    ENDLOOP.
    out->write( loop_str ).
    CLEAR loop_str.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `39) Defining the Step Size and the Direction of Loop Passes` ) ).

    "STEP addition for defining the step size and the direction of the loop
    "- Step size: Specified by the absolute value of an integer
    "- Direction: Specified by a positive (forward loop) or negative
    "  (loop in reverse order) integer

    "Reversing the loop order using a negative integer
    "Each line is read indicated by the absolute value 1
    LOOP AT it_loop INTO wal STEP -1.
      loop_str &&= wal-compa.
    ENDLOOP.
    out->write( loop_str ).
    CLEAR loop_str.

    "Forward loop by specifiying a positive integer
    "In the example, every second line is read.
    "Note: Omitting STEP means STEP 1 by default.
    LOOP AT it_loop INTO wal STEP 2.
      loop_str &&= wal-compa.
    ENDLOOP.
    out->write( loop_str ).
    CLEAR loop_str.
*
    "STEP with other additions
    "The example uses the additions FROM and TO.
    "Note: If the value after STEP is negative, the value
    "after FROM must be greater than the value after TO.
    LOOP AT it_loop INTO wal FROM 5 TO 1 STEP 2.
      loop_str &&= wal-compa.
    ENDLOOP.
    out->write( loop_str ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `40) Interrupting and Exiting Loops` ) ).

    DATA(str_table) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ( `d` ) ( `e` ) ( `f` ) ).
    LOOP AT str_table INTO DATA(wa_exit).
      DATA(tab_idx) = sy-tabix.
      IF wa_exit = `e`.
        EXIT.
      ENDIF.
    ENDLOOP.
    ASSERT tab_idx = 5.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `41) Iteration Expressions` ) ).

    TYPES ty_int_tab TYPE TABLE OF i WITH EMPTY KEY.
    DATA(int_table_a) = VALUE ty_int_tab( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).
    DATA int_table_b TYPE ty_int_tab.
    int_table_b = VALUE #( FOR wa_b IN int_table_a ( wa_b * 2 ) ).

    out->write( data = int_table_b name = `int_table_b` ).
    out->write( |\n| ).

    "Instead of, for example, a LOOP statement as follows:
    DATA int_table_c TYPE ty_int_tab.
    LOOP AT int_table_a INTO DATA(wa_c).
      INSERT wa_c * 3 INTO TABLE int_table_c.
    ENDLOOP.
    out->write( data = int_table_c name = `int_table_c` ).
    out->write( |\n| ).

    "Table comprehension: Content of an internal table is created by
    "evaluating a table using a table iteration with an iteration
    "expressions within a constructor expression.
    DATA(lv_num_a) = VALUE ty_int_tab( FOR ls1 IN it_loop
                                       ( ls1-compa ) ).

    out->write( data = lv_num_a name = `lv_num_a` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `41) Retrieving values of one column in ` &&
      `an internal table based on conditions` ) ).

    DATA(lv_num_b) = VALUE ty_int_tab( FOR ls2 IN it_loop
                                       WHERE ( compa < 3 ) ( ls2-compc ) ).

    out->write( data = lv_num_b name = `lv_num_b` ).
    out->write( |\n| ).

    out->write( zcl_demo_abap_aux=>heading( `42) Looping across 2 tables ` &&
         `and retrieving values based on conditions` ) ).
    "Internal table type
    DATA(it_int) = VALUE ty_int_tab( FOR x = 1 WHILE x <= 4 ( x ) ).

    DATA(itab_for_2tab) =
      VALUE  ttype_loop(
                     FOR ls3 IN it_int
                     FOR ls4 IN it_loop WHERE ( compa = ls3 )
                                              ( compa = ls3 compb = ls4-compb ) ).

    out->write( data = itab_for_2tab name = `itab_for_2tab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `43) Inserting and Deleting Lines in Internal Tables in Loops` ) ).

    "Inserting and Deleting Lines in Internal Tables in Loops

    "Creating and populating a demo standard internal table to
    "work with in the example loops
    TYPES: BEGIN OF s_loop_mod,
             text TYPE string,
             num  TYPE i,
           END OF s_loop_mod,
           t_loop_mod TYPE TABLE OF s_loop_mod WITH EMPTY KEY.

    "Inserting 10 entries into the demo table
    DATA(itab_original) = VALUE t_loop_mod( FOR x = 1 WHILE x <= 10 ( text = x ) ).
    DATA(itab_loop) = itab_original.

    "---------- Inserting a line after the current line ----------

    "The example inserts a line after the currently processed line
    "using an INSERT statement and specifying the index value
    "(sy-tabix value + 1). The 'num' component is assigned the
    "current sy-tabix value.
    "Note: In all statements, the sy-tabix value is stored in a
    "variable right after the LOOP statement. Assume that multiple
    "statements are included before the statement that actually uses
    "the current sy-tabix value. Other statements that potentially
    "change the sy-tabix value might interfere.
    "An EXIT statement takes care of exiting the loop. In the example,
    "all values of the 'num' component in the original table lines
    "(except the first line) are initial as the loop is exited.
    LOOP AT itab_loop ASSIGNING FIELD-SYMBOL(<fs>).
      DATA(tabix) = sy-tabix.
      <fs>-num = tabix.
      INSERT VALUE #( text = tabix ) INTO itab_loop INDEX tabix + 1.
      IF tabix = 50.
        EXIT.
      ENDIF.
    ENDLOOP.

    out->write( data = itab_loop name = `itab_loop` ).
    out->write( |\n| ).

    "---------- Deleting a line after the current line ----------

    "The example deletes a line after the current line using a
    "DELETE statement and the INDEX addition. The index value
    "is specified using the current sy-tabix value + 1.
    "The 'num' value in the resulting internal table includes
    "the sy-tabix value.

    itab_loop = itab_original.
    LOOP AT itab_loop ASSIGNING <fs>.
      tabix = sy-tabix.
      <fs>-num = tabix.
      DELETE itab_loop INDEX tabix + 1.
    ENDLOOP.

    out->write( data = itab_loop name = `itab_loop` ).
    out->write( |\n| ).

    "---------- Inserting a line before the current line ----------

    "The example insert a line before the currently processed line using
    "an INSERT statement. The current sy-tabix value is used as INDEX value,
    "moving down the currently processed table line one position.
    "In that case, the sy-tabix value increases accordingly.
    "Logic:
    "- For example, the first line is processed, sy-tabix has the value 1.
    "- A line is inserted at this position, moving down the currently processed line
    "  one position. The moved line is then in the second position (as a new line exists
    "  in the first position).
    "- In the next loop pass, the loop is continued with the third line, i.e.
    "  sy-tabix has the value 3 in the second loop pass.
    "The example includes modifications of the table components. The 'num' value
    "is assigned the table index value after inserting the new line. The
    "'num' value of existing table lines includes the value of the index before
    "inserting the new line + 1.

    itab_loop = itab_original.
    FIELD-SYMBOLS <line_loop> TYPE s_loop_mod.
    DATA new_line_counter TYPE i.
    DATA tabix_copy TYPE i.

    LOOP AT itab_loop ASSIGNING <fs>.
      tabix = sy-tabix.
      new_line_counter += 1.

      "Asserting that sy-tabix value has changed accordingly.
      IF tabix <> 1.
        ASSERT tabix = tabix_copy + 2.
      ENDIF.

      DATA(new_line_text) = |---- New line { new_line_counter } ----|.
      INSERT VALUE #( text = new_line_text ) INTO itab_loop INDEX tabix ASSIGNING <line_loop>.

      DATA(idx_new) = line_index( itab_loop[ text = new_line_text num = 0 ] ).
      <line_loop>-num = idx_new.

      DATA(idx_existing) = line_index( itab_loop[ text = <fs>-text num = 0 ] ).
      DATA(new_text) = |{ <fs>-text }(existing line, index before insertion: { tabix })|.
      <fs>-text = new_text.
      <fs>-num = idx_existing.

      tabix_copy = tabix.
    ENDLOOP.

    out->write( data = itab_loop name = `itab_loop` ).
    out->write( |\n| ).

    "---------- Deleting a line before the current line ----------

    "The example explores the deletion of a line before the currently
    "processed line. The previous line in the table is deleted if
    "the value of 'text' (an integer was inserted) is an even number.
    "The DELETE statement specifies the index with the current sy-tabix
    "value - 1. On deletion, the sy-tabix value is decreased accordingly.
    "Before a potential deletion, the currently processed table line is
    "copied to another table to visualize the current sy-tabix value in
    "the 'num' component.

    itab_loop = itab_original.
    DATA itab_copy LIKE itab_loop.

    LOOP AT itab_loop ASSIGNING <fs>.
      tabix = sy-tabix.

      <fs>-num = tabix.
      INSERT <fs> INTO TABLE itab_copy.

      TRY.
          IF CONV i( <fs>-text ) MOD 2 = 0.
            DELETE itab_loop INDEX tabix - 1.
          ENDIF.
        CATCH cx_sy_conversion_no_number .
      ENDTRY.

    ENDLOOP.

    out->write( data = itab_loop name = `itab_loop` ).
    out->write( |\n| ).
    out->write( data = itab_copy name = `itab_copy` ).
    out->write( |\n| ).

    "---------- Deleting the currently processed table line ----------

    "The example explores deleting the currently processed table line using
    "a string table. So, the DELETE statement specifies the current sy-tabix
    "value for INDEX. In that case, the next line moves up one position, and
    "the sy-tabix value remains the same, i.e. when sy-tabix is 2, and the
    "line is deleted, the value remains 2 and processes the line moved up.

    "Creating and populating a demo internal table
    DATA(str_tab) = VALUE string_table( ( `a` ) ( `#` ) ( `c` ) ( `#` ) ( `e` )
                                        ( `f` ) ( `g` ) ( `#` ) ( `i` ) ( `j` ) ).

    LOOP AT str_tab REFERENCE INTO DATA(dref).
      tabix = sy-tabix.
      IF dref->* CS `#`.
        DELETE str_tab INDEX tabix.
      ENDIF.
    ENDLOOP.

    out->write( data = str_tab name = `str_tab` ).
    out->write( |\n| ).

    "---------- Statements clearing the entire internal table are not allowed in loops ----------

    "The entire internal table cannot be deleted within loops.
    "The following statements commented out are not possible.
    LOOP AT str_tab REFERENCE INTO dref.
      "CLEAR str_tab.
      "str_tab = VALUE #( ).
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `44) Selecting multiple rows from a database table into an internal table` ) ).

    SELECT FROM zdemo_abap_tab1
      FIELDS key_field, char1, char2, num1, num2
      WHERE num1 > 3
      INTO TABLE @DATA(itab_select1).

    out->write( data = itab_select1 name = `itab_select1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading(  `45) Sequentially adding multiple rows from a database table to an internal table` ) ).

    DATA itab_sql TYPE TABLE OF zdemo_abap_tab1 WITH NON-UNIQUE KEY client key_field.

    SELECT FROM zdemo_abap_tab1
      FIELDS *
      WHERE num1 > 3
      INTO @DATA(struc_select).

      IF sy-subrc = 0.
        "Some modifications on the read lines (capitalizing letters)
        struc_select-char1 = to_upper( struc_select-char1 ).
        struc_select-char2 = to_upper( struc_select-char2 ).

        "Adding modified line to an internal table
        APPEND struc_select TO itab_sql.
      ENDIF.
    ENDSELECT.

    out->write( data = itab_sql name = `itab_sql` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `46) Adding multiple rows from a database table ` &&
      `to an internal table that has a different line type than the ` &&
      `database table and keeping existing table content` ) ).

    SELECT FROM zdemo_abap_tab2
      FIELDS *
      WHERE num1 > 10
      APPENDING CORRESPONDING FIELDS OF TABLE @itab_sql.

    out->write( data = itab_sql name = `itab_sql` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `47) Adding multiple rows from a database table ` &&
      `to an internal table that has a different line type than the ` &&
      `database table and deleting existing table content` ) ).

    SELECT FROM zdemo_abap_tab2
      FIELDS *
      WHERE num1 > 10
      INTO CORRESPONDING FIELDS OF TABLE @itab_sql.

    out->write( data = itab_sql name = `itab_sql` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `48) Adding multiple rows from an internal table ` &&
      `to an internal table using SELECT` ) ).

    SELECT key_field, char1, char2, num1, num2
      FROM @itab_sql AS itab_alias
      INTO TABLE @DATA(itab_clone).

    out->write( data = itab_clone name = `itab_clone` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `49) Combining data of multiple tables into an` &&
      ` internal table using an inner join` ) ).

    "Filling table to be selected from
    itab_sql =  VALUE #( ( key_field = 500 char1 = 'uuu' char2 = 'vvv'
                       num1      = 501 num2  = 502 )
                     ( key_field = 600 char1 = 'www' char2 = 'xxx'
                       num1      = 601 num2  = 602 ) ).

    "SELECT list includes fields from both tables
    "If there are no equivalent entries in the first or second table,
    "the rows are not joined.
    SELECT itab_alias1~key_field, itab_alias1~char2,
           zdemo_abap_tab2~numlong
      FROM @itab_sql AS itab_alias1
      INNER JOIN zdemo_abap_tab2
        ON itab_alias1~key_field = zdemo_abap_tab2~key_field
      INTO TABLE @DATA(join_result).

    out->write( data = join_result name = `join_result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `50) Filling internal table ` &&
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

    out->write( zcl_demo_abap_aux=>heading( `51) Filling internal table ` &&
      `using a subquery (2)` ) ).

    "A subquery using EXISTS in the WHERE clause.
    "In the example, data is selected from a database table depending
    "on the existence of data in an internal table. Only if a line
    "with a matching value of the specified field exists in both
    "database and internal table, data is read.
    SELECT key_field, numlong
      FROM zdemo_abap_tab2
      WHERE EXISTS
         ( SELECT 'X' FROM @itab_sql AS itab_alias2
           WHERE key_field = zdemo_abap_tab2~key_field )
      INTO TABLE @DATA(subquery_result2).

    out->write( data = subquery_result2 name = `subquery_result2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `52) Filling an internal table from a table ` &&
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
        FOR ALL ENTRIES IN @itab_sql
        WHERE key_field = @itab_sql-key_field
        INTO TABLE @DATA(select_result).
    ENDIF.

    out->write( data = select_result name = `select_result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `53) Adding content from a database to internal` &&
      ` table by using alias names in the SELECT list` ) ).

    DATA itab_sql2 TYPE TABLE OF zdemo_abap_tab2 WITH EMPTY KEY.

    "Specifying alias names can help fill an existing internal
    "table that has not a matching line type to the database table.
    "Here, two fields are specified with an alias name to match the
    "names of components contained in the existing internal table.
    "The individual types of the fields match, too.
    SELECT key_field, char2 AS char1, num2 AS num1
        FROM zdemo_abap_tab1
      INTO CORRESPONDING FIELDS OF TABLE @itab_sql2 UP TO 3 ROWS.

    out->write( data = itab_sql2 name = `itab_sql2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `54) SELECT Queries with Internal Tables as Data Sources` ) ).

    TYPES int_tab_type TYPE TABLE OF i WITH EMPTY KEY.
    DATA(itab_a) = VALUE int_tab_type( ( 1 ) ( 32 ) ( 100 ) ( -24 ) ( 17 ) ( 99 ) ).

    "SELECT query with an internal table as data source
    "The example uses an aggregate expression. It is statically
    "detected that the query cannot be processed by the ABAP SQL
    "engine. The data must be passed to the database. Consequently,
    "a syntax warning is displayed. It can be suppressed by a pragma.
*    SELECT MAX( table_line ) AS max_val
*      FROM @itab_a AS it
*      INTO @DATA(max_a).

    SELECT MAX( table_line ) AS max_val ##itab_db_select
     FROM @itab_a AS it
     INTO @DATA(max_b).

    out->write( data = max_b name = `max_b` ).
    out->write( |\n| ).

    "Using the LIKE addition in the WHERE clause to extract internal table
    "entries matching a specific pattern.
    TYPES: BEGIN OF s1,
             a TYPE c LENGTH 3,
             b TYPE i,
           END OF s1,
           it_type_1 TYPE TABLE OF s1 WITH EMPTY KEY.
    DATA(itab_b) = VALUE it_type_1( ( a = 'abc' b = 1 )
                                    ( a = 'zbc' b = 2 )
                                    ( a = 'bde' b = 3 )
                                    ( a = 'yde' b = 4 ) ).

    SELECT a, b
      FROM @itab_b AS it_alias
      WHERE a LIKE '%bc'
      INTO TABLE @DATA(select_like_result).

    out->write( data = select_like_result name = `select_like_result` ).
    out->write( |\n| ).

    "----------- Using a SELECT loop with an internal table as data source -----------

    TYPES: BEGIN OF s2,
             comp1 TYPE c LENGTH 2,
             comp2 TYPE i,
           END OF s2,
           it_type_2 TYPE TABLE OF s2 WITH EMPTY KEY.

    DATA(itab_c) = VALUE it_type_2( ( comp1 = 'aa' comp2 = 2 )
                                    ( comp1 = 'zz' comp2 = 9 )
                                    ( comp1 = 'dd' comp2 = 1 )
                                    ( comp1 = 'rr' comp2 = 7 )
                                    ( comp1 = 'tt' comp2 = 5 )
                                    ( comp1 = 'bb' comp2 = 6 ) ).

    DATA itab_d TYPE int_tab_type.

    "The following SELECT loop specifies an internal table as data source.
    "The loop sequence is defined by a sort order. Such a functionality is
    "not available with LOOP AT.
    SELECT comp2
           FROM @itab_c AS it
           ORDER BY comp2 DESCENDING
           INTO @DATA(wa_select).
      INSERT wa_select INTO TABLE itab_d.
    ENDSELECT.

    out->write( data = itab_d name = `itab_d` ).
    out->write( |\n| ).

    "------------------- Joins with internal tables -------------------

    TYPES: BEGIN OF s3,
             a TYPE c LENGTH 3,
             b TYPE c LENGTH 3,
             c TYPE i,
           END OF s3,
           it_type_3 TYPE TABLE OF s3 WITH EMPTY KEY.

    DATA(itab_e) = VALUE it_type_3( ( a = 'aaa' b = 'bbb' c = 1 )
                                    ( a = 'ccc' b = 'ddd' c = 1 )
                                    ( a = 'eee' b = 'fff' c = 2 ) ).

    DATA(itab_f) = VALUE it_type_3( ( a = 'ggg' b = 'hhh' c = 1 )
                                    ( a = 'iii' b = 'jjj' c = 1 )
                                    ( a = 'kkk' b = 'lll' c = 3 ) ).

    "No syntax warning. The internal tables can be processed by the
    "ABAP SQL engine.
    SELECT it_alias1~a, it_alias2~b
      FROM @itab_e AS it_alias1
      INNER JOIN @itab_f AS it_alias2 ON it_alias1~c = it_alias2~c
      INTO TABLE @DATA(itab_g).

    out->write( data = itab_g name = `itab_g` ).
    out->write( |\n| ).

    "Join with a database table and an internal table

    "Preparing a demo database table and an internal table
    DELETE FROM zdemo_abap_tab1.
    INSERT zdemo_abap_tab1 FROM TABLE @( VALUE #( ( key_field = 1 char1 = 'aaa' )
                                                  ( key_field = 2 char1 = 'bbb' )
                                                  ( key_field = 3 char1 = 'ccc' ) ) ).

    TYPES it_type_4 TYPE TABLE OF zdemo_abap_tab1 WITH EMPTY KEY.
    DATA(itab_h) = VALUE it_type_4( ( key_field = 1 char2 = 'zzz' )
                                    ( key_field = 2 char2 = 'yyy' ) ).

    SELECT db~key_field, db~char1, it~char2
      FROM zdemo_abap_tab1 AS db
      INNER JOIN @itab_h AS it ON it~key_field = db~key_field
      INTO TABLE @DATA(itab_i).


    out->write( data = itab_i name = `itab_i` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `55) Excursion: Joining/Merging Internal Tables into Internal Tables` ) ).

    "Excursion: Joining/Merging Internal Tables into Internal Tables

    "Creating two internal tables whose content will be joined. The shared
    "value is represented by the key1 and key2 components.
    "Sorted tables are used in the example (having key1/key2 as unique keys)
    "to have unique values to perform joins.
    TYPES: BEGIN OF struct1,
             key1 TYPE i,
             a    TYPE c LENGTH 1,
             b    TYPE c LENGTH 1,
             c    TYPE c LENGTH 1,
           END OF struct1,
           tab_type1 TYPE SORTED TABLE OF struct1 WITH UNIQUE KEY key1,
           BEGIN OF struct2,
             key2 TYPE i,
             d    TYPE c LENGTH 1,
             e    TYPE c LENGTH 1,
           END OF struct2,
           tab_type2 TYPE SORTED TABLE OF struct2 WITH UNIQUE KEY key2.

    "Populating demo internal tables
    DATA(itab1) = VALUE tab_type1( ( key1 = 1 a = 'a' b = 'b'  c = 'c' )
                                   ( key1 = 2 a = 'd' b = 'e'  c = 'f' )
                                   ( key1 = 3 a = 'g' b = 'h'  c = 'i' ) ).

    DATA(itab2) = VALUE tab_type2( ( key2 = 1 d = `j` e = `k` )
                                   ( key2 = 2 d = `l` e = `m` ) ).

    "SELECT statement, inner join
    "Note: With the inner join, the target table contains all
    "combinations of rows for whose columns the join condition
    "is true.
    SELECT a~key1, a~a, a~b, b~d, b~e
        FROM @itab1 AS a
        INNER JOIN @itab2 AS b ON a~key1 = b~key2
        INTO TABLE @DATA(itab3).

    out->write( data = itab3 name = `itab3` ).
    out->write( |\n| ).

    "SELECT statement, left outer join
    "In contrast to the inner join above, the target table here
    "also contains the table row of the first table for which
    "no equivalent row exists in the second table.
    SELECT a~key1, a~a, a~b, b~d, b~e
        FROM @itab1 AS a
        LEFT OUTER JOIN @itab2 AS b ON a~key1 = b~key2
        INTO TABLE @DATA(itab4).

    out->write( data = itab4 name = `itab4` ).
    out->write( |\n| ).

    "Common table expression
    WITH +it1 AS ( SELECT a~key1, a~a, a~b FROM @itab1 AS a ),
         +it2 AS ( SELECT b~key2, b~d, b~e FROM @itab2 AS b )
    SELECT +it1~key1, +it1~a, +it1~b, +it2~d, +it2~e FROM +it1 LEFT JOIN +it2 ON +it1~key1 = +it2~key2
    INTO TABLE @DATA(itab5).

    out->write( data = itab5 name = `itab5` ).
    out->write( |\n| ).

    "LOOP statements
    "Using the CORRESPONDING operator to assign identically named components,
    "BASE retains existing content
    "The assignment with CORRESPONDING ... BASE ... includes a table expression
    "in which table lines are read and inserted based on the key mapping. With the
    "OPTIONAL addition, errors can be avoided if a line does not exist.
    DATA itab6 LIKE itab4.
    LOOP AT itab1 INTO DATA(w1).
      INSERT CORRESPONDING #( w1 ) INTO TABLE itab6 REFERENCE INTO DATA(ref).
      ref->* = CORRESPONDING #( BASE ( ref->* ) VALUE #( itab2[ key2 = ref->key1 ] OPTIONAL ) ).
    ENDLOOP.

    out->write( data = itab6 name = `itab6` ).
    out->write( |\n| ).

    "Assume the second table's shared component was also key1. In the second CORRESPONDING
    "you could then work with the EXCEPT addition to not overwrite the identicall named
    "component.

    "Example similar to the previous one
    "Also here, a table expression is used to read a line from
    "the second internal table. The INSERT statement (without
    "CORRESPONDING) includes the concrete value assignments
    "with the VALUE operator.
    DATA itab7 LIKE itab4.
    LOOP AT itab1 INTO DATA(w2).
      DATA(lin) = VALUE #( itab2[ key2 = w2-key1 ] OPTIONAL ).

      INSERT VALUE #( key1 = w2-key1
                      a = w2-a
                      b = w2-b
                      d = lin-d
                      e = lin-e ) INTO TABLE itab7.
    ENDLOOP.

    out->write( data = itab7 name = `itab7` ).
    out->write( |\n| ).

    "Example using a FOR loop with the VALUE operator
    TYPES tt_type3 LIKE itab4.
    DATA(itab8) = VALUE tt_type3( FOR w3 IN itab1
                                  ( key1 = w3-key1
                                    a = w3-a
                                    b = w3-b
                                    d = VALUE #( itab2[ key2 = w3-key1 ]-d OPTIONAL )
                                    e = VALUE #( itab2[ key2 = w3-key1 ]-e OPTIONAL ) ) ).

    out->write( data = itab8 name = `itab8` ).
    out->write( |\n| ).

    "Similar example that includes a LET expression
    DATA(itab9) = VALUE tt_type3( FOR w4 IN itab1
                                  LET tab_line = VALUE #( itab2[ key2 = w4-key1 ] OPTIONAL ) IN
                                  ( key1 = w4-key1
                                    a = w4-a
                                    b = w4-b
                                    d = tab_line-d
                                    e = tab_line-e ) ).

    out->write( data = itab9 name = `itab9` ).
    out->write( |\n| ).

    "Example using a FOR loop with the REDUCE operator and LET
    DATA(itab10) = REDUCE tt_type3( INIT tab = VALUE #( )
                                    FOR w5 IN itab1
                                    LET tableline = VALUE #( itab2[ key2 = w5-key1 ] OPTIONAL ) IN
                                    NEXT tab = VALUE #( BASE tab
                                    ( key1 = w5-key1
                                      a = w5-a
                                      b = w5-b
                                      d = tableline-d
                                      e = tableline-e ) ) ).

    out->write( data = itab10 name = `itab10` ).
    out->write( |\n| ).

**********************************************************************

    "Sorting internal tables

    out->write( zcl_demo_abap_aux=>heading( `56) Sorting internal tables` ) ).

    "Creating structured data types
    TYPES: BEGIN OF struc_sort1,
             a TYPE i,
             b TYPE string,
             c TYPE c LENGTH 1,
             d TYPE i,
           END OF struc_sort1.

    TYPES: BEGIN OF struc_sort2,
             a TYPE i,
             b TYPE i,
           END OF struc_sort2.

    "Creating internal tables
    DATA it1 TYPE TABLE OF struc_sort1 WITH NON-UNIQUE KEY a.
    DATA it2 TYPE TABLE OF struc_sort1 WITH DEFAULT KEY.

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

    out->write( zcl_demo_abap_aux=>heading( `57) Sorting by primary table key` ) ).

    "Primary key: component a
    SORT it1.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `58) Sorting by primary table key in ascending` &&
      ` order` ) ).

    "The sorting result is the same as above (where ASCENDING is used
    "implicitly). Here, it is explicitly specified.
    SORT it1 ASCENDING.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `59) Sorting by primary table key respecting all ` &&
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

    out->write( zcl_demo_abap_aux=>heading( `60) Sorting by primary table key in ` &&
      `descending order` ) ).

    "Sorting in descending order and by primary table key
    SORT it1 DESCENDING.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `61) Sorting by explicitly specified component (1)` ) ).
    "Here, the component is the primary table key.
    "The sorting result is the same as above.
    SORT it1 BY a DESCENDING.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `62) Sorting by explicitly specified component (2)` ) ).

    "Sorting by arbitrary, non-key field
    SORT it1 BY d DESCENDING.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `63) Sorting by multiple explicitly specified` &&
      ` components` ) ).

    "Sorting by multiple components and specifying the sort order
    SORT it1 BY b ASCENDING c DESCENDING.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `64) Sorting by respecting the values of all` &&
      ` components` ) ).

    "Sorting by considering the values of each field of the table line
    SORT it1 BY table_line.

    out->write( data = it1 name = `it1` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `65) Modifying Internal Table Content: Direct modification of recently read table lines` ) ).



    "direct modification of recently read table lines:

    "Declaring and populating demo internal tables
    TYPES: BEGIN OF ty_struc,
             comp1 TYPE i,
             comp2 TYPE string,
             comp3 TYPE c LENGTH 3,
           END OF ty_struc.

    DATA it_st TYPE TABLE OF ty_struc WITH NON-UNIQUE KEY comp1.
    DATA it_so TYPE SORTED TABLE OF ty_struc WITH UNIQUE KEY comp1.
    DATA it_ha TYPE HASHED TABLE OF ty_struc WITH UNIQUE KEY comp1.

    it_st = VALUE #( ( comp1 = 1 comp2 = `AAAAAA` comp3 = 'bbb' )
                     ( comp1 = 2 comp2 = `CCCCCC` comp3 = 'ddd' )
                     ( comp1 = 3 comp2 = `EEEEEE` comp3 = 'fff' )
                     ( comp1 = 4 comp2 = `GGGGGG` comp3 = 'hhh' )
                   ).

    it_so = it_st.
    it_ha = it_st.

    "---- Modifying internal table content by changing the ----
    "---- content of READ TABLE statement target areas --------
    "Reading table line into a target area
    READ TABLE it_st INTO DATA(workarea) INDEX 1.
    READ TABLE it_so ASSIGNING FIELD-SYMBOL(<f>) INDEX 2.
    READ TABLE it_ha REFERENCE INTO DATA(drf) WITH TABLE KEY comp1 = 3. "No reading by index in case of hashed tables

    "------ Modification examples -------
    "Modifying all non-key components using the VALUE operator and
    "the BASE addition
    <f> = VALUE #( BASE <f> comp2 = `IIIIII` comp3 = 'jjj' ).

    "In the following example, the key value is assigned a new
    "value. Key values are protected against change in case of key tables.
    "A runtime error occurs.
    "<f> = VALUE #( comp1 = 5 comp2 = `IIIIII` comp3 = 'jjj' ).

    drf->* = VALUE #( BASE drf->* comp2 = `KKKKKK` comp3 = 'lll' ).

    "Same as above. Key values cannot be changed in this case.
    "drf->* = VALUE #( comp1 = 5 comp2 = `MMMMMM` comp3 = 'nnn' ).

    "Using a MODIFY statement outlined below for changing internal
    "table content based on a read line in a work area
    MODIFY TABLE it_st FROM VALUE #( BASE workarea comp2 = `OOOOOO` comp3 = 'ppp' ).

    "Modifying individual components
    READ TABLE it_st INTO workarea INDEX 2.
    READ TABLE it_so ASSIGNING <f> INDEX 3.
    READ TABLE it_ha REFERENCE INTO drf WITH TABLE KEY comp1 = 4.

    "Using VALUE/BASE
    <f> = VALUE #( BASE <f> comp2 = `QQQQQQ` ).
    drf->* = VALUE #( BASE drf->* comp2 = `RRRRRR` ).
    MODIFY TABLE it_st FROM VALUE #( BASE workarea comp2 = `SSSSSS` ).

    "Using the component selector
    <f>-comp3 = 'ttt'.

    READ TABLE it_st INTO workarea INDEX 3.
    workarea-comp3 = 'uuu'.
    MODIFY TABLE it_st FROM workarea.

    "Object component selector in case of dereferencing ...
    drf->comp2 = `VVVVVV`.
    "... which is a more comfortable option compared to using the
    "dereferencing and component selector operators in the following workareay.
    drf->*-comp3 = 'www'.

    "---- Modifying internal table content using table expressions -----

    "Changing the entire table line of a standard table
    "In standard tables, the key value change is allowed.
    it_st[ 3 ] = VALUE #( comp1 = 9 comp2 = `XXXXXX` comp3 = 'yyy' ).
    "As above, the sorted table is a key table having a unique key,
    "therefore a write cannot be performed on the entire entry. Runtime
    "errors can occur.
    "it_so[ 3 ] = VALUE #( comp2 = `XXXXXX` comp3 = 'yyy' ).
    "The same applies to hashed tables.
    "it_ha[ comp2 = `OOOOOO` ] = VALUE #( comp2 = `XXXXXX` comp3 = 'yyy' ).

    "Changing individual components
    it_st[ 3 ]-comp2 = `ZZZZZZ`.
    it_so[ 3 ]-comp3 = 'A1'.
    it_ha[ comp2 = `CCCCCC` ]-comp2 = `B2`.
    "As above, no key field change in key tables. Allowed in standard
    "tables.
    "it_so[ 3 ]-comp1 = 10.
    "it_ha[ comp2 = `AAAAAA` ]-comp1 = `C3`.
    it_st[ 1 ]-comp1 = 99.

    "---- Modifying table content in all table rows in a loop ----
    "For more syntax options regarding loops, check the section above.
    "Target area: field symbol
    LOOP AT it_st ASSIGNING FIELD-SYMBOL(<lo>).
      <lo>-comp2 = sy-tabix.
    ENDLOOP.

    "---- Modifying table content restricting the rows that are looped across ----
    "Target area: data reference variable
    LOOP AT it_st REFERENCE INTO DATA(lo) FROM 2 TO 3.
      lo->comp3 = sy-tabix.
    ENDLOOP.

    "Target area: work area
    LOOP AT it_so INTO DATA(workarea_lo) WHERE comp1 < 4.
      workarea_lo-comp2 = sy-tabix.
      MODIFY TABLE it_so FROM workarea_lo.
    ENDLOOP.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `66) Modifying Internal Table Content: MODIFY statements` ) ).

    "Creating structured data types.
    TYPES: "Line types for internal tables
      BEGIN OF struc1,
        a TYPE i,
        b TYPE c LENGTH 3,
        c TYPE c LENGTH 3,
        d TYPE c LENGTH 3,
      END OF struc1.

    "Declaring demo sorted/hashed tables having primary and
    "secondary keys as well as alias names defined

    DATA it_std TYPE TABLE OF struc1 WITH NON-UNIQUE KEY a.
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

    DATA(mod_line) = VALUE struc1( a = 2 b = 'zzz' c = 'yyy' ).

    "Standard table
    "With the addition FROM wa, the key values in wa determine the line
    "to be modified.
    "Note: Component d is not specified in "line". The value is
    "initialized.
    MODIFY TABLE it_std FROM mod_line.

    "Example in which the work area is constructed inline.
    "Components b and c not specified. The values are initialized.
    MODIFY TABLE it_std FROM VALUE #( a = 3 d = 'xxx' ).

    "Addition TRANSPORTING: Only specified fields are respected
    "Note: In case of sorted/hasehd tables, key values cannot be
    "specified.
    MODIFY TABLE it_std
      FROM VALUE #( a = 4 b = '###' c = '###' d = '###' )
      TRANSPORTING b c.

    "Modifying table lines via index
    "Note: It is only MODIFY, not MODIFY TABLE as above.
    "The following statement modifies the line with number 1 in the
    "primary table index. Without the addition TRANSPORTING, the
    "entire line is changed.
    MODIFY it_std
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

    out->write( zcl_demo_abap_aux=>heading( `67) Deleting internal table content` ) ).

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
    DELETE it_std WHERE a > 3.

    out->write( data = it_st name = `it_st` ).
    out->write( |\n| ).
    out->write( data = it_so_sec name = `it_so_sec` ).
    out->write( |\n| ).
    out->write( data = it_ha_sec name = `it_ha_sec` ).
    out->write( |\n| ).

    "Excursion: Deleting in a LIKE-like fashion you may know from
    "ABAP SQL statements.
    "The LIKE addition is not available for the WHERE clause in DELETE
    "statements for internal tables as is the case for ABAP SQL DELETE statements.
    DATA(stringtable) = VALUE string_table( ( `abcZ` ) ( `Zdef` ) ( `gZhi` )
                                            ( `Zjkl` ) ( `Zmno` ) ( `pqrZ` ) ).

    "You can, for example, use logical operators such as CP (conforms to pattern)
    "All lines that begin with Z are to be deleted.
    DELETE stringtable WHERE table_line CP `Z*`.

    out->write( data = stringtable name = `stringtable` ).
    out->write( |\n| ).

    "---------- Deleting the current line inside a LOOP statement ----------

    "The following example illustrates deleting the current table line
    "using a DELETE statement within a LOOP statement. Lines with even
    "numbers are deleted.
    "Note:
    "- The short form of the DELETE statement always deletes the
    "  current first line implicitly. It is only possible within a LOOP
    "  statement and the delete operation is performed on the same internal
    "  table.
    "- The field symbol (or reference variable) should not be used after
    "  the DELETE statement any more.
    DATA itab_del_loop1 TYPE TABLE OF i WITH EMPTY KEY.
    itab_del_loop1 = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ( 6 ) ( 7 ) ( 8 ) ( 9 ) ( 10 ) ).

    LOOP AT itab_del_loop1 ASSIGNING FIELD-SYMBOL(<fs_del_loop>).
      IF <fs_del_loop> MOD 2 = 0.
        DELETE itab_del_loop1.
      ENDIF.
    ENDLOOP.

    out->write( data = itab_del_loop1 name = `itab_del_loop1` ).
    out->write( |\n| ).

    "The following, similar example (uneven numbers are deleted) uses a
    "table which is looped over by specifying the addition USING KEY.
    "In this case (using LOOP ... USING KEY ...), the short form of the
    "DELETE statement cannot be used. Use the DELETE statement with the
    "addition USING KEY loop_key to delete the current first line.
    "loop_key is a predefined name to be used with DELETE and within
    "loops that specify LOOP ... USING KEY .... No other key name is
    "possible here.
    DATA itab_del_loop2 TYPE TABLE OF i WITH NON-UNIQUE KEY table_line.
    itab_del_loop2 = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ( 6 ) ( 7 ) ( 8 ) ( 9 ) ( 10 ) ).

    LOOP AT itab_del_loop2 USING KEY primary_key REFERENCE INTO DATA(dref2).
      IF dref2->* MOD 2 <> 0.
        DELETE itab_del_loop2 USING KEY loop_key.
      ENDIF.
    ENDLOOP.

    out->write( data = itab_del_loop2 name = `itab_del_loop2` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `68) Deleting adjacent duplicate entries` ) ).
    out->write( `Original table content (restored before` &&
      ` each of the following examples)` ).
    out->write( |\n| ).
    out->write( |\n| ).

    it_std = VALUE #( ( a = 1 b = 'BBB' c = '###' d = '###' )
                     ( a = 2 b = '###' c = '###' d = '###' )
                     ( a = 1 b = '###' c = '###' d = '###' )
                     ( a = 3 b = '###' c = '###' d = '###' )
                     ( a = 4 b = '###' c = 'CCC' d = '###' )
                     ( a = 1 b = 'BBB' c = '###' d = '###' )
                     ( a = 2 b = 'BBB' c = '###' d = '###' )
                     ( a = 4 b = 'BBB' c = '###' d = '###' )
                     ( a = 2 b = 'BBB' c = '###' d = '###' )
                     ( a = 3 b = '###' c = '###' d = '###' ) ).

    SORT it_std BY table_line.

    "Filling another table so that the same content above
    "is available for the examples below.
    DATA(it_std2) = it_std.

    out->write( data = it_std2 name = `it_std2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `69) Deleting adjacent duplicates based on` &&
      ` primary table key` ) ).

    "Note: Using the primary table key can have unexpected consequences
    "if the primary table key is the standard key or if it is empty.
    DELETE ADJACENT DUPLICATES FROM it_std2.

    out->write( data = it_std2 name = `it_std2` ).

    it_std2 = it_std.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `70) Deleting adjacent duplicates by comparing ` &&
      `all field values` ) ).

    DELETE ADJACENT DUPLICATES FROM it_std2 COMPARING ALL FIELDS.

    out->write( data = it_std2 name = `it_std2` ).

    it_std2 = it_std.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `71) Deleting adjacent duplicates by comparing ` &&
      `specific field values` ) ).

    DELETE ADJACENT DUPLICATES FROM it_std2 COMPARING a c.

    out->write( data = it_std2 name = `it_std2` ).

    it_std2 = it_std.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `72) Deleting adjacent duplicates by using a` &&
      ` table key` ) ).

    "In this case, the result is the same as in the first example.
    DELETE ADJACENT DUPLICATES FROM it_std2 USING KEY primary_key.

    out->write( data = it_std2 name = `it_std2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `73) Deleting the entire internal table content` ) ).

    CLEAR it_std.

    "Additionally, FREE releases memory space.
    FREE it_std2.

    "Excursion: Assigning an empty constructor expression with VALUE clears
    "the internal table.
    DATA(it_stdr) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).

    it_stdr = VALUE #( ).

    "Same applies to NEW
    DATA(it_stdr_new) = NEW string_table( ( `a` ) ( `b` ) ( `c` ) ).
    it_stdr_new = NEW #( ).

    out->write( data = it_std name = `it_std` ).
    out->write( |\n| ).
    out->write( data = it_std2 name = `it_std2` ).
    out->write( |\n| ).
    out->write( data = it_stdr name = `it_stdr` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `74) Grouping Internal Tables` ) ).


    TYPES: BEGIN OF demo_struct,
             comp1 TYPE c LENGTH 1,
             comp2 TYPE i,
             comp3 TYPE abap_boolean,
             comp4 TYPE string,
           END OF demo_struct,
           tab_type_for_grouping TYPE TABLE OF demo_struct WITH EMPTY KEY.
    DATA string_table TYPE string_table.

    "Populating a demo internal table as the basis of the syntax example
    "Note: The example loops only use data objects as targets, not data references
    "or field symbols.
    DATA(it) = VALUE tab_type_for_grouping( ( comp1 = 'd' comp2 = 0 comp3 = abap_false )
                               ( comp1 = 'a' comp2 = 1 comp3 = abap_true )
                               ( comp1 = 'a' comp2 = 2 comp3 = abap_false )
                               ( comp1 = 'e' comp2 = 11 comp3 = abap_true )
                               ( comp1 = 'b' comp2 = 5 comp3 = abap_true )
                               ( comp1 = 'b' comp2 = 6 comp3 = abap_false )
                               ( comp1 = 'a' comp2 = 3 comp3 = abap_false )
                               ( comp1 = 'b' comp2 = 4 comp3 = abap_true )
                               ( comp1 = 'c' comp2 = 10 comp3 = abap_true )
                               ( comp1 = 'e' comp2 = 1 comp3 = abap_false )
                               ( comp1 = 'd' comp2 = 7 comp3 = abap_true )
                               ( comp1 = 'a' comp2 = 4 comp3 = abap_true )
                               ( comp1 = 'e' comp2 = 111 comp3 = abap_true ) ).

    "The following example (and several others below) does not specify a nested loop.
    "It does not specify a group key binding either. This means that the work area
    "contains the first line of each group, representing the group in the loop
    "(representative binding). The comp4 component is assigned the sy-tabix value,
    "which is the number of the line in the table without the grouping.
    DATA ita LIKE it.
    LOOP AT it INTO DATA(waa) GROUP BY waa-comp1.
      waa-comp4 = sy-tabix.
      APPEND waa TO ita.
    ENDLOOP.
    out->write( data = ita name = `ita` ).
    out->write( |\n| ).

    "Specifying sort order
    DATA itb LIKE it.
    LOOP AT it INTO DATA(wab) GROUP BY wab-comp1 ASCENDING.
      wab-comp4 = sy-tabix.
      APPEND wab TO itb.
    ENDLOOP.
    out->write( data = itb name = `itb` ).
    out->write( |\n| ).

    "WITHOUT MEMBERS addition; a group key binding is required
    "after WITHOUT MEMBERS
    "The group key binding is added to a string table for visualizing its
    "content.
    "Note: The component values are initial when the group key binding is
    "specified.
    LOOP AT it INTO DATA(wac) GROUP BY wac-comp1 WITHOUT MEMBERS INTO DATA(keyc).
      ASSERT wac IS INITIAL.
      APPEND keyc TO string_table.
    ENDLOOP.
    out->write( data = string_table name = `string_table` ).
    out->write( |\n| ).

    "Using a structured group key
    "The following example just assigns component values to the group key. In this case,
    "the grouping is performed with more than just one criterion as in the previous examples.
    "As a result, table lines are added to the other table in descending order based on the
    "two component values.
    DATA itd LIKE it.
    LOOP AT it INTO DATA(wad) GROUP BY ( key1 = wad-comp1 key2 = wad-comp2 ) DESCENDING.
      APPEND wad TO itd.
    ENDLOOP.
    out->write( data = itd name = `itd` ).
    out->write( |\n| ).

    "In the following example, the group is sorted in ascending order. Note that the
    "group index value uses the original position in the group index. The group key
    "binding information is added to a string table for visualizing its content.
    CLEAR str_table.
    LOOP AT it INTO DATA(wae) GROUP BY ( key = wae-comp1 gi = GROUP INDEX gs = GROUP SIZE ) ASCENDING INTO DATA(keye).
      APPEND |Key component: '{ keye-key }', group index: '{ keye-gi }', group size: '{ keye-gs }'| TO string_table.
    ENDLOOP.
    out->write( data = string_table name = `string_table` ).
    out->write( |\n| ).

    "LOOP AT GROUP: Nested loop across group members
    "Unlike the previous example, the example uses a nested loop across the groups (the group key binding is
    "specified after LOOP AT GROUP). There, the component values of the members can be accessed.
    DATA itf LIKE it.
    LOOP AT it INTO DATA(waf) GROUP BY ( key = waf-comp1 gi = GROUP INDEX gs = GROUP SIZE ) ASCENDING INTO DATA(keyf).
      LOOP AT GROUP keyf INTO DATA(memberf).
        APPEND VALUE #( comp1 = memberf-comp1 comp2 = memberf-comp2 comp3 = memberf-comp3
        comp4 = |Key component: '{ keyf-key }', group index: '{ keyf-gi }', group size: '{ keyf-gs }'|
        ) TO itf.
      ENDLOOP.
    ENDLOOP.
    out->write( data = itf name = `itf` ).
    out->write( |\n| ).

    "The objective of this example is to extract the line with the highest value in a particular
    "column within a group from the original table to another.
    "The example uses representative binding, i.e. the representative of the group is specified
    "in the work area, not in a group key binding.
    DATA itg LIKE it.
    LOOP AT it INTO DATA(wag) GROUP BY wag-comp1 ASCENDING.
      LOOP AT GROUP wag INTO DATA(memberg) GROUP BY memberg-comp2 DESCENDING.
        APPEND memberg TO itg.
        EXIT.
      ENDLOOP.
    ENDLOOP.
    out->write( data = itg name = `itg` ).
    out->write( |\n| ).

    "The following example is similar to the previous example, and yields the same result.
    "Here, the group key binding is specified after LOOP AT GROUP.
    DATA ith LIKE it.
    LOOP AT it INTO DATA(wah) GROUP BY wah-comp1 ASCENDING.
      LOOP AT GROUP wah INTO DATA(memberh) GROUP BY memberh-comp2 DESCENDING.
        APPEND memberh TO ith.
        EXIT.
      ENDLOOP.
    ENDLOOP.
    ASSERT itg = ith.
    out->write( data = ith name = `ith` ).
    out->write( |\n| ).

    "Additional syntax options, like specifying a WHERE condition in both nested and outer
    "loops, are possible. The example below shows that the LOOP AT GROUP statement assigns
    "the value of sy-tabix to the value that would be set for the current line in the LOOP
    "without grouping.
    DATA iti LIKE it.
    LOOP AT it INTO DATA(wai) GROUP BY wai-comp1 ASCENDING.
      LOOP AT GROUP wai INTO DATA(memberi) WHERE comp3 = abap_true.
        APPEND VALUE #( comp1 = memberi-comp1 comp2 = memberi-comp2 comp3 = memberi-comp3
        comp4 = |sy-tabix: '{ sy-tabix }'|
        ) TO iti.
      ENDLOOP.
    ENDLOOP.
    out->write( data = iti name = `iti` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `75) Collecting Values` ) ).

    "This example demonstrates how to insert data from a database table
    "into an internal table in a compressed way. Within a SELECT loop,
    "a COLLECT statement is used to consolidate lines with identical
    "primary key components (carrid and connid) by summing the number
    "of occupied seats in the numeric component (seatsocc).
    "Additionally, an internal table is filled by adding all read lines.
    "This table is looped across to simulate the effect of the COLLECT
    "statement.

    DATA: BEGIN OF seats,
            carrid   TYPE zdemo_abap_fli-carrid,
            connid   TYPE zdemo_abap_fli-connid,
            seatsocc TYPE zdemo_abap_fli-seatsocc,
          END OF seats,
          seats_tab_col      LIKE HASHED TABLE OF seats WITH UNIQUE KEY carrid connid,
          seats_tab_all      LIKE TABLE OF seats WITH EMPTY KEY,
          seats_tab_loop_grp LIKE seats_tab_col.

    SELECT carrid, connid, seatsocc
            FROM zdemo_abap_fli
            INTO @seats.
      COLLECT seats INTO seats_tab_col.
      APPEND seats TO seats_tab_all.
    ENDSELECT.

    out->write( data = seats_tab_all name = `seats_tab_all` ).
    out->write( |\n| ).

    LOOP AT seats_tab_all INTO DATA(wa_coll) GROUP BY ( key1 = wa_coll-carrid key2 = wa_coll-connid ).
      INSERT VALUE #( carrid = wa_coll-carrid connid = wa_coll-connid ) INTO TABLE seats_tab_loop_grp ASSIGNING FIELD-SYMBOL(<fsgr>).
      LOOP AT GROUP wa_coll INTO DATA(member).
        <fsgr>-seatsocc = <fsgr>-seatsocc + member-seatsocc.
      ENDLOOP.
    ENDLOOP.

    ASSERT seats_tab_loop_grp = seats_tab_col.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Excursions` ) ).
    out->write( |76) Secondary table keys and hashed tables\n\n| ).

    "Declaring a hashed table
    DATA hashed_table
        TYPE HASHED TABLE OF zdemo_abap_tab1
           WITH UNIQUE KEY primary_key COMPONENTS key_field
           WITH NON-UNIQUE SORTED KEY sec_key COMPONENTS char1 char2.

    "Retrieving data to work with
    SELECT * FROM zdemo_abap_tab1 INTO TABLE @hashed_table UP TO 3 ROWS.

    "Integer table to display the table index
    DATA int_itab TYPE TABLE OF i.

    "Note: There is no primary table index in hashed tables.
    LOOP AT hashed_table INTO DATA(hwa) USING KEY primary_key.
      APPEND sy-tabix TO int_itab.
    ENDLOOP.

    out->write( data = int_itab name = `int_itab` ).
    out->write( |\n| ).

    CLEAR int_itab.

    "Demonstrating the secondary table index when using
    "the secondary key
    LOOP AT hashed_table INTO DATA(hwa2) USING KEY sec_key.
      APPEND sy-tabix TO int_itab.
    ENDLOOP.

    out->write( data = int_itab name = `int_itab` ).
    out->write( |\n| ).

    "Retrieving a table line via index access to the secondary index
    "of the sorted secondary key
    DATA(line_of_ht) = hashed_table[ KEY sec_key INDEX 2 ].

    out->write( data = line_of_ht name = `line_of_ht` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `77) Empty keys in internal table created inline` ) ).
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

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `78) Ranges tables` ) ).

    "Populating an integer table with values from 1 to 20
    TYPES intgr_tab_type TYPE TABLE OF i WITH EMPTY KEY.
    DATA(inttab) = VALUE intgr_tab_type( FOR x = 1 WHILE x <= 20 ( x ) ).

    "Declaring a ranges table
    DATA rangestab TYPE RANGE OF i.

    "Populating a ranges table using VALUE
    rangestab = VALUE #( sign   = 'I'
                         option = 'BT' ( low = 1  high = 3 )
                                       ( low = 6  high = 8 )
                                       ( low = 12 high = 15 )
                         option = 'GE' ( low = 18 ) ).

    "Using a SELECT statement and the IN addition to retrieve internal table
    "content based on the ranges table specifications
    SELECT * FROM @inttab AS tab
        WHERE table_line IN @rangestab
        INTO TABLE @DATA(result).

    out->write( data = result name = `result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `79) Creating Internal Tables Dynamically` ) ).

    DATA(some_type) = 'STRING'.
    DATA dataref TYPE REF TO data.

    "Creating an internal table using a CREATE DATA statement
    "by specifying the type name dynamically.
    "In the example, a standard table with elementary line type
    "and standard key is created.
    CREATE DATA dataref TYPE TABLE OF (some_type).

    TYPES: BEGIN OF demo_struc,
             comp1 TYPE c LENGTH 10,
             comp2 TYPE i,
             comp3 TYPE i,
           END OF demo_struc.

    "Internal table with structured line type and empty key.
    CREATE DATA dataref TYPE TABLE OF ('DEMO_STRUC') WITH EMPTY KEY.

    "Using a globally available table type
    CREATE DATA dataref TYPE ('STRING_TABLE').

    out->write( zcl_demo_abap_aux=>no_output ).

  ENDMETHOD.
ENDCLASS.
