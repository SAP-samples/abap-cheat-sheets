"! <p class="shorttext"><strong>Constructor expressions</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the use of constructor expressions.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li>Topics covered:<ul>
"! <li>Operators VALUE, CORRESPONDING, NEW, CONV, EXACT, REF,
"! CAST, COND, SWITCH, FILTER, REDUCE</li>
"! <li>Iteration expressions with FOR</li>
"! <li>LET expressions</li></ul>
"! <li>Find information on <strong>getting started with the example class</strong> and the
"! <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</li></ul>
CLASS zcl_demo_abap_constructor_expr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF line1,
             col1 TYPE i,
             col2 TYPE i,
           END OF line1,
           BEGIN OF line2,
             col2 TYPE i,
             col3 TYPE i,
             col4 TYPE i,
           END OF line2,
           BEGIN OF s1_type,
             comp1 TYPE c LENGTH 1,
             comp2 TYPE string,
             comp3 TYPE i,
           END OF s1_type,
           BEGIN OF s2_type,
             comp1 TYPE string,
             comp2 TYPE c LENGTH 1,
             comp3 TYPE i,
             comp4 TYPE i,
           END OF s2_type.

    CLASS-DATA:
      "Deep structures as examples to demonstrate the CORRESPONDING operator
      BEGIN OF struc1,
        comp1 TYPE c LENGTH 1 VALUE 'W',
        BEGIN OF struc_nested,
          comp1 TYPE c LENGTH 1 VALUE 'X',
          BEGIN OF comp2,
            col1 TYPE c LENGTH 1 VALUE 'Y',
            col2 TYPE c LENGTH 1 VALUE 'Z',
          END OF comp2,
        END OF struc_nested,
        itab  TYPE TABLE OF line1 WITH EMPTY KEY,
      END OF struc1,
      BEGIN OF struc2,
        BEGIN OF struc_nested,
          comp1 TYPE string,
          comp2 TYPE string,
          comp3 TYPE string,
        END OF struc_nested,
        itab  TYPE TABLE OF line2 WITH EMPTY KEY,
        comp4 TYPE i,
      END OF struc2,
      s1   TYPE s1_type,
      s2   TYPE s2_type,
      tab1 TYPE TABLE OF s1_type WITH EMPTY KEY,
      tab2 TYPE TABLE OF s2_type WITH EMPTY KEY,
      tab3 TYPE TABLE OF s2_type WITH EMPTY KEY,
      tab4 TYPE SORTED TABLE OF s2_type WITH NON-UNIQUE KEY comp3,
      nl   TYPE string..

    CLASS-METHODS:
      fill_deep_structures,
      fill_struc_and_tab.

ENDCLASS.



CLASS zcl_demo_abap_constructor_expr IMPLEMENTATION.


  METHOD fill_deep_structures.
    "Clearing all contents of struc2
    CLEAR struc2.
    "Filling nested tables in deep structures
    struc2-struc_nested = VALUE #( comp1 = `aaa`
                                   comp2 = `bbb`
                                   comp3 = `ccc` ).

    struc1-itab = VALUE #(
      ( col1 = 111 col2 = 222 )
      ( col1 = 333 col2 = 444
     ) ).

    struc2-itab = VALUE #(
      ( col2 = 1 col3 = 2 col4 = 3 )
      ( col2 = 4 col3 = 5 col4 = 6 )
      ( col2 = 7 col3 = 8 col4 = 9 )
     ).

    "Filling individual component that is not shared by both structures
    struc2-comp4 = 999.
  ENDMETHOD.


  METHOD fill_struc_and_tab.
    CLEAR: s1, s2, tab1, tab2, tab3.

    s1 = VALUE #( comp1 = 'A' comp2 = `bbb` comp3 = 1 ).

    s2 = VALUE #( comp1 = `ccc` comp2 = 'D' comp3 = 2 comp4 = 3 ).

    tab1 = VALUE #(
      ( comp1 = 'A' comp2 = `bbb` comp3 = 1 )
      ( comp1 = 'B' comp2 = `ccc` comp3 = 2 )
      ( comp1 = 'C' comp2 = `ddd` comp3 = 3 ) ).

    tab2 = VALUE #(
      ( comp1 = `eee` comp2 = 'F' comp3 = 4 comp4 = 5 )
      ( comp1 = `ggg` comp2 = 'H' comp3 = 6 comp4 = 7 )
      ( comp1 = `iii` comp2 = 'J' comp3 = 8 comp4 = 9 ) ).

    tab3 = VALUE #(
      ( comp1 = `aaa` comp2 = 'B' comp3 = 1 comp4 = 2 )
      ( comp1 = `ccc` comp2 = 'D' comp3 = 3 comp4 = 4 )
      ( comp1 = `eee` comp2 = 'F' comp3 = 5 comp4 = 6 )
      ( comp1 = `ggg` comp2 = 'H' comp3 = 7 comp4 = 8 )
      ( comp1 = `iii` comp2 = 'J' comp3 = 9 comp4 = 10 ) ).

    tab4 = tab3.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( `ABAP Cheat Sheet Example: Constructor Expressions` ).

    out->write( |\nVALUE\n| ).
    out->write( |1) Structures: Populating a flat structure\n\n| ).

    "A flat structure is created based on a data type defined with a
    "TYPES statement. The structure is then filled using a constructor
    "expression with VALUE by specifying the components and assigning
    "values. Here, the type can be inferred, hence, a # character can be used.

    TYPES: BEGIN OF struc_type,
             num   TYPE i,
             char1 TYPE c LENGTH 3,
             char2 TYPE c LENGTH 3,
           END OF struc_type.

    DATA struc TYPE struc_type.

    "Filling structure
    struc = VALUE #( num = 1 char1 = 'aaa'  char2 = 'abc' ).

    out->write( data = struc name = `struc` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Structures: Omitting value assignment to components / BASE addition` ) ).

    "The same structure is then filled purposely omitting components, i.
    "e. these components remain initial.

    struc = VALUE #( char1 = 'bbb' ).

    out->write( data = struc name = `struc` ).

    "You can use the BASE addition to retain existing content
    "Compare with the BASE example further down regarding internal tables: There are
    "no extra parentheses within the outer pair of parentheses.
    struc = VALUE #( BASE struc char2 = 'xyz' ).

    out->write( |\n| ).
    out->write( data = struc name = `struc` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Structures: Inline declaration, explicit type specification` ) ).

    "The example demonstrates a variable that is declared inline. Here,
    "the result is a structure which is filled using a constructor
    "expression with VALUE and by specifying the components and assigning
    "values in parentheses. The structure type is specified explicitly.
    "The # symbol would not work since no type can be inferred from the
    "specified parameters.

    DATA(struc_inl) = VALUE struc_type( num   = 3
                                        char1 = 'ccc'
                                        char2 = 'def' ).

    out->write( data = struc_inl name = `struc_inl` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Internal tables: Declaration and population` ) ).

    "The example demonstrates the declaration of an internal table. The
    "internal table is then filled using a constructor expression with
    "VALUE.
    "The type can be inferred here and need not be specified explicitly.
    "Note the extra pair of parentheses in which the components are
    "specified and assigned values. In the example, 3 lines are added to
    "the table. For one line, some components are purposely not assigned.

    DATA itab TYPE TABLE OF struc_type WITH EMPTY KEY.

    itab = VALUE #( ( num = 1 char1 = 'aaa'  char2 = 'abc' )
                    ( num = 2 char1 = 'bbb'  char2 = 'def' )
                    ( num = 3 char1 = 'ccc'                 ) ).

    out->write( data = itab name = `itab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Internal tables: Inline declaration, explicit type specification` ) ).

    "The example demonstrates an internal table declared inline that is
    "filled using a constructor expression with VALUE by specifying the
    "internal table type explicitly. Note that the internal table type
    "cannot be generic in this context.

    TYPES: itab_type TYPE STANDARD TABLE OF struc_type
                        WITH NON-UNIQUE KEY num.

    DATA(itab2) = VALUE itab_type(
                    ( num = 4 char1 = 'ddd' char2 = 'ghi' )
                    ( num = 5 char1 = 'eee'  char2 = 'jkl' ) ).


    DATA(str_table) = VALUE string_table( ( `this` )
                                          ( `is a` )
                                          ( `table` )
                                          ( `of type string` ) ).

    out->write( data = itab2 name = `itab2` ).
    out->write( |\n| ).
    out->write( data = str_table name = `str_table` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) LINES OF addition` ) ).

    "Using the LINES OF addition, you can add lines of other tables.
    "Note: The line type of the other internal table must match the one of
    "the target internal table. Using FROM/TO, the table line selection can
    "be further restricted. Without FROM/TO, all lines of the table are
    "respected.

    itab2 = VALUE #( ( num = 6 char1 = 'fff'  char2 = 'mno' )
                     ( LINES OF itab )
                     ( LINES OF itab FROM 1 TO 2 ) ).

    out->write( data = itab2 name = `itab2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) BASE addition for keeping existing data` ) ).

    "Using the BASE addition, you can keep existing content of the source
    "internal table.

    itab2 = VALUE #( BASE itab2 ( num = 7 char1 = 'ggg' char2 = 'pqr' ) ).

    out->write( data = itab2 name = `itab2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Assignemnt with the VALUE operator without specifying content in parentheses` ) ).

    "Using the VALUE operator without populating anything in the parentheses,
    "data objects are initialized.

    "elementary types
    DATA(some_num) = 123.
    some_num = VALUE #( ).

    DATA(another_num) = VALUE i( ).

    DATA(some_str) = `hallo`.
    some_str = VALUE #( ).

    "Initializing internal table/structure
    str_table = VALUE #( ).
    struc = VALUE #( ).

    out->write( data = some_num name = `some_num` ).
    out->write( |\n| ).
    out->write( data = another_num name = `another_num` ).
    out->write( |\n| ).
    out->write( data = some_str name = `some_str` ).
    out->write( |\n| ).
    out->write( data = str_table name = `str_table` ).
    out->write( |\n| ).
    out->write( data = struc name = `struc` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `9) Short form for internal tables with structured line types` ) ).

    TYPES: BEGIN OF stype,
             a TYPE i,
             b TYPE c LENGTH 3,
             c TYPE string,
           END OF stype.
    TYPES tabtype TYPE TABLE OF stype WITH EMPTY KEY.

    DATA(itable) = VALUE tabtype( b = 'aaa'           ( a = 1 c = `xxx` )
                                                      ( a = 2 c = `yyy` )
                                  b = 'bbb' c = `zzz` ( a = 3 )
                                                      ( a = 4 ) ).

    out->write( data = itable name = `itable` ).
    out->write( |\n| ).

    "This option can be handy in various contexts, for example, in a
    "ranges table.
    TYPES int_tab_type TYPE TABLE OF i WITH EMPTY KEY.
    "Populating an integer table with values from 1 to 20 (see iteration
    "expressions with FOR further down)
    DATA(inttab) = VALUE int_tab_type( FOR x = 1 WHILE x <= 20 ( x ) ).

    DATA rangetab TYPE RANGE OF i.

    "Populating a range table using VALUE and the short form
    rangetab = VALUE #( sign   = 'I'
                        option = 'BT' ( low = 1  high = 3 )
                                      ( low = 6  high = 8 )
                                      ( low = 12 high = 15 )
                        option = 'GE' ( low = 18 ) ).

    "Using a SELECT statement to retrieve internal table content
    "based on the range table specifications
    SELECT * FROM @inttab AS tab
        WHERE table_line IN @rangetab
        INTO TABLE @DATA(result_tab).

    out->write( data = result_tab name = `result_tab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Excursions: VALUE operator in use with ABAP statements and ABAP SQL statements` ) ).

    "The following examples use ABAP and ABAP SQL statements in which table lines
    "are constructed inline using the VALUE operator.

    out->write( `10) Modifying internal table from a structure created inline` && |\n\n| ).

    MODIFY TABLE itab2 FROM VALUE #( num   = 7 char1 = 'hhh' char2 = 'stu' ).

    out->write( data = itab2 name = `itab2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) Inserting a table line that is created inline into an internal table` ) ).

    INSERT VALUE #( num = 8 char1 = 'iii'  char2 = 'vwx' ) INTO TABLE itab2.

    out->write( data = itab2 name = `itab2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `12) Deleting a table entry based on a line created inline` ) ).

    DELETE TABLE itab2 FROM VALUE #( num = 3 ).

    out->write( data = itab2 name = `itab2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `13) Modifying a database table based on an internal table created inline` ) ).

    "Deleting demo database table entries for the following example
    DELETE FROM zdemo_abap_carr.

    MODIFY zdemo_abap_carr FROM TABLE @( VALUE #(
           ( carrid = 'CO'
             carrname = 'Continental Airlines'
             currcode = 'USD'
             url =  'http://www.continental.com' )
           ( carrid = 'SQ'
             carrname = 'Singapore Airlines'
             currcode = 'SGD'
             url =  'http://www.singaporeair.com' )
           ) ).

    "Retrieving table entries for display purposes
    SELECT FROM zdemo_abap_carr
           FIELDS carrid, carrname, currcode, url
           ORDER BY carrid
           INTO TABLE @DATA(itab_carr).

    out->write( data = itab_carr name = `itab_carr` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Excursion: Deep structures and tables` ) ).
    out->write( |14) Deep structure\n| ).

    "The example demonstrates the use of constructor expressions with
    "VALUE in the context of a deep structure. Here, a structure is declared
    "inline and filled. The structure type includes a nested structure. The
    "nested structure is filled using a nested VALUE expression.

    TYPES: BEGIN OF deep_struc_ty,
             num   TYPE i,
             char1 TYPE c LENGTH 3,
             BEGIN OF substruc,
               int TYPE i,
               str TYPE string,
             END OF substruc,
           END OF deep_struc_ty.

    DATA(deep_struc) = VALUE deep_struc_ty( num = 1
                                            char1 = 'aaa'
                                            substruc = VALUE #( int = 123 str = `hallo` ) ).

    out->write( data = deep_struc name = `deep_struc` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `15) Deep internal table` ) ).

    "A deep internal table is created. Also here, nested VALUE
    "expressions are demonstrated.

    TYPES: BEGIN OF deep_struc_ty2,
             char TYPE c LENGTH 3,
             tab  TYPE TABLE OF i WITH EMPTY KEY,
           END OF deep_struc_ty2.

    TYPES: itab_deep_type TYPE STANDARD TABLE OF deep_struc_ty2
            WITH NON-UNIQUE KEY char.

    DATA(deep_itab) = VALUE itab_deep_type(
      ( char = 'aaa' tab = VALUE #( ( 1 ) ( 2 ) ( 3 ) ) )
      ( char = 'bbb' tab = VALUE #( ( 4 ) ( 5 ) ( 6 ) ) ) ).

    out->write( data = deep_itab name = `deep_itab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `CORRESPONDING` ) ).
    out->write( |Simple Examples with structures and internal tables\n| ).

    "Method to fill demo structures and internal tables
    "with values to work with
    fill_struc_and_tab( ).

    out->write( `16) Original structure and table content` && |\n\n| ).

    "Displaying the original structures and tables that are filled in the
    "course of a method call. The structures and tables are filled anew
    "throughout the examples so that all CORRESPONDING expressions are based
    "on the same values.

    out->write( data = s1 name = `s1` ).
    out->write( |\n| ).
    out->write( data = s2 name = `s2` ).
    out->write( |\n| ).
    out->write( data = tab1 name = `tab1` ).
    out->write( |\n| ).
    out->write( data = tab2 name = `it_st` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `17) CORRESPONDING without addition` ) ).

    "The target structure and table have a different type but identically
    "named components. The identically named components are filled. Note
    "that the target variables are initialized here. Also note the effect
    "of an automatic conversion of a variable-length string to a
    "fixed-length string (one component is typed with c, the other,
    "identically named component, is of type string).

    s2 = CORRESPONDING #( s1 ).

    tab2 = CORRESPONDING #( tab1 ).

    out->write( data = s2 name = `s2` ).
    out->write( |\n| ).
    out->write( data = tab2 name = `tab2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `18) BASE addition for keeping original content` ) ).

    "The BASE addition keeps the original content. Structure: The non-
    "identical component name retains its value. Internal table: Existing
    "table lines are kept.

    fill_struc_and_tab( ).

    s2 = CORRESPONDING #( BASE ( s2 ) s1 ).

    tab2 = CORRESPONDING #( BASE ( tab2 ) tab1 ).

    out->write( data = s2 name = `s2` ).
    out->write( |\n| ).
    out->write( data = tab2 name = `tab2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `19) MAPPING/EXCEPT additions` ) ).

    "The example demonstrates the additions MAPPING and EXCEPT. MAPPING:
    "One component of the target structure is assigned the value of a
    "dedicated component of the source structure. EXCEPT: All corresponding
    "components are assigned except a specific one.

    fill_struc_and_tab( ).

    s2 = CORRESPONDING #( s1 MAPPING comp4 = comp3 ).

    tab2 = CORRESPONDING #( tab1 EXCEPT comp1 ).

    out->write( data = s2 name = `s2` ).
    out->write( |\n| ).
    out->write( data = tab2 name = `tab2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `CORRESPONDING: Demonstrating various` &&
    ` additions using deep structures` ) ).

    out->write( `20) Original content of deep structures` && |\n\n| ).

    "Displaying the original deep structures and tables that are filled in
    "the course of a method call. The deep structures and tables are filled
    "anew throughout the examples so that all CORRESPONDING expressions are
    "based on the same values.

    "Method to fill demo deep structures and internal tables
    "with values to work with
    fill_deep_structures( ).

    out->write( data = struc1 name = `struc1` ).
    out->write( |\n| ).
    out->write( data = struc2 name = `struc2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `21) CORRESPONDING without addition` ) ).

    "CORRESPONDING operator without addition
    "Existing contents of identically named components are replaced.
    "Existing contents of components in the target structure that are not
    "available in the source structure are initialized.
    "Contents of the nested structure struc_nested is converted to
    "string. Note that the two components of the nested structure in
    "component struc_nested-comp2 of struc1 are drawn together when being
    "converted to string.
    "Contents of struc2-itab are replaced by table contents of struc1-
    "itab. Note the value assignment, for example, for col2 in struc2-itab.
    "Despite the fact that there is no identically named component comp1 in
    "the target structure, values are assigned starting with the first
    "column of the source structure. Check the conversion rules for
    "internal tables.

    struc2 = CORRESPONDING #( struc1 ).

    out->write( data = struc2 name = `struc2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `22) DEEP addition` ) ).

    "CORRESPONDING operator with the addition DEEP
    "Existing contents of identically named components are replaced.
    "Existing contents of components in the target structure that are not
    "available in the source structure are initialized.
    "Contents of the nested structure struc_nested is converted to
    "string. Note that the two components of the nested structure in
    "component struc_nested-comp2 of struc1 are drawn together when being
    "converted to string.
    "Contents of struc2-itab are replaced by table contents of struc1-
    "itab. Due to the addition DEEP, the value assignment happens for
    "identically named components in the nested table. Hence, only col2 as
    "the only shared and identically named component is filled.

    fill_deep_structures( ).

    struc2 = CORRESPONDING #( DEEP struc1 ).

    out->write( data = struc2 name = `struc2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `23) BASE addition` ) ).

    "CORRESPONDING operator with the addition BASE
    "Existing contents of identically named components are replaced.
    "Existing contents of components in the target structure that are not
    "available in the source structure are kept.
    "Contents of the nested structure struc_nested is converted to
    "string. Note that the two components of the nested structure in
    "component struc_nested-comp2 of struc1 are drawn together when being
    "converted to string.
    "Contents of struc2-itab are replaced by table contents of struc1-
    "itab. The value assignment in the nested table happens like using the
    "CORRESPONDING operator without addition. Note the value assignment, for
    "example, for col2 in struc2-itab. Despite the fact that there is no
    "identically named component col1 in the target structure, values are
    "assigned starting with the first column of the source structure. Check
    "the conversion rules for internal tables.

    fill_deep_structures( ).

    struc2 = CORRESPONDING #( BASE ( struc2 ) struc1 ).

    out->write( data = struc2 name = `struc2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `24) DEEP BASE addition` ) ).

    "CORRESPONDING operator with the additions DEEP BASE
    "Existing contents of identically named components are replaced.
    "Existing contents of components in the target structure that are not
    "available in the source structure are kept.
    "Contents of the nested structure struc_nested is converted to
    "string. Note that the two components of the nested structure in
    "component struc_nested-comp2 of struc1 are drawn together when being
    "converted to string.
    "Contents of struc2-itab are replaced by table contents of struc1-
    "itab. The value assignment in the nested table happens like using the
    "CORRESPONDING operator with the addition DEEP. That is, the value
    "assignment happens for identically named components in the nested table.
    "Hence, only col2 as the only shared and identically named component is filled.

    fill_deep_structures( ).

    struc2 = CORRESPONDING #( DEEP BASE ( struc2 ) struc1 ).

    out->write( data = struc2 name = `struc2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `25) APPENDING addition` ) ).

    "CORRESPONDING operator with the addition APPENDING
    "Existing contents of identically named components are replaced.
    "Existing contents of components in the target structure that are not
    "available in the source structure are kept.
    "Contents of the nested structure struc_nested is converted to
    "string. Note that the two components of the nested structure in
    "component struc_nested-comp2 of struc1 are drawn together when being
    "converted to string.
    "Contents of struc2-itab are kept and contents of struc1-itab are
    "added. The value assignment concerning the added lines happens like
    "using the CORRESPONDING operator without addition. Note the value
    "assignment, for example, for col2 in struc2-itab. Despite the fact that
    "there is no identically named component col1 in the target structure,
    "values are assigned starting with the first column of the source
    "structure. Check the conversion rules for internal tables.

    fill_deep_structures( ).

    struc2 = CORRESPONDING #( APPENDING ( struc2 ) struc1 ).

    out->write( data = struc2 name = `struc2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `26) DEEP APPENDING` ) ).

    "CORRESPONDING operator with the additions DEEP APPENDING
    "Existing contents of identically named components are replaced.
    "Existing contents of components in the target structure that are not
    "available in the source structure are kept.
    "Contents of the nested structure struc_nested is converted to
    "string. Note that the two components of the nested structure in
    "component struc_nested-comp2 of struc1 are drawn together when being
    "converted to string.
    "Contents of struc2-itab are kept and contents of struc1-itab are
    "added. The value assignment concerning the added lines happens like
    "using the CORRESPONDING operator with the addition DEEP. That is, the
    "value assignment happens for identically named components in the nested
    "table. Hence, only col2 as the only shared and identically named
    "component is filled.

    fill_deep_structures( ).

    struc2 = CORRESPONDING #( DEEP APPENDING ( struc2 ) struc1 ).

    out->write( data = struc2 name = `struc2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `NEW` ) ).
    out->write( `27) Creating Anonymous Data Objects` && |\n\n| ).

    "The examples show the creation of anonymous data objects.
    "First, data reference variables are declared using a DATA statement.
    "Here, one variable is declared with a complete type, the other one with
    "a generic type.
    "Then, anonymous data objects are created using the NEW operator. For
    "one example, the type can be inferred. In the other example, the type
    "is specified explicitly.
    "The next examples show the direct assigning of values.
    "Furthermore, inline declarations can be used to avoid the prior
    "declaration of a variable.

    "Declaring data reference variables
    DATA dref1 TYPE REF TO i. "Complete type
    DATA dref2 TYPE REF TO data. "Generic type

    "Creating anonymous data objects
    "Here, no parameters are specified within the parentheses meaning the
    "data objects retain their initial values.
    dref1 = NEW #( ).
    dref2 = NEW string( ).

    IF dref1->* = 0 AND dref2->* = ``.
      DATA(val) = `Initial values`.
    ELSE.
      val = `No initial values`.
    ENDIF.

    "Directly assigning values within the parentheses.
    dref1 = NEW #( 123 ).
    dref2 = NEW string( `hallo` ).

    "Inline declaration, explicit type specification
    DATA(dref3) = NEW i( 456 ).

    "Another constructor expression specified within the parentheses
    DATA tx TYPE string VALUE `world`.
    DATA(dref4) = NEW string( `Hello ` && tx && CONV string( '!' ) ).

    DATA dref5 TYPE REF TO string_table.
    dref5 = NEW #( VALUE string_table( ( `a` ) ( `b` ) ) ).

    "Structured type; named arguments within the parentheses
    DATA(dref6) = NEW zdemo_abap_carr( carrid   = 'AA'
                                       carrname = 'American Airlines' ).

    out->write( data = val name = `val` ).
    out->write( |\n| ).
    out->write( data = dref1 name = `dref1` ).
    out->write( |\n| ).
    out->write( data = dref2 name = `dref2` ).
    out->write( |\n| ).
    out->write( data = dref3 name = `dref3` ).
    out->write( |\n| ).
    out->write( data = dref4 name = `dref4` ).
    out->write( |\n| ).
    out->write( data = dref5 name = `dref5` ).
    out->write( |\n| ).
    out->write( data = dref6 name = `dref6` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `28) Creating Instances of Classes` ) ).

    "The example demonstrates the creation of instances of classes.
    "First, an object reference variable is declared with a DATA statement.
    "As a next step, an instance of a class is created. The type can be
    "inferred here. The class has a constructor method defined. Hence, the
    "parentheses contain the parameter binding for the constructor method.
    "Here, it is only one parameter. That means the explicit specification
    "of the parameter name is not needed and the value can be specified
    "directly: oref1 = NEW #( `Hallo` ).
    "The next examples show object reference variables that are declared
    "inline. Here, the type (i. e. the class name) must be specified
    "explicitly.
    "The last example shows the method chaining that is possible with
    "expressions using the NEW operator. The demo class has a method that
    "has a returning parameter. In this case, the parameter of the method
    "is of type REF TO i.

    "Creating an object reference variable
    DATA oref1 TYPE REF TO local_class.

    "Creating an instance of a class;
    "providing parameter bindings for the constructor method
    "in the parentheses
    oref1 = NEW #( txt = `Hallo` ).

    out->write( data = oref1 name = `oref1` ).
    out->write( |\n| ).

    "Creating an instance of a class, object reference variable
    "is declared inline, explicit type specification
    DATA(oref2) = NEW local_class( `Salut` ).

    out->write( data = oref2 name = `oref2` ).
    out->write( |\n| ).

    "Method chaining
    DATA(result) = NEW local_class( `Ciao` )->double( int = NEW #( 5 ) ).

    out->write( data = result name = `result` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `29) CONV` ) ).

    "The examples show the effect of the CONV operator.
    "A variable of type i is declared and assigned a value. Then,
    "calculations are carried out. The result is stored in a variable
    "declared inline. The first result is 0 because the derived type is i.
    "The second calculation returns the precise value resulting from the
    "division because the CONV
    "expression triggers the conversion of the result (decfloat34).
    "The next examples demonstrate logical expressions using character-like
    "data types.
    "A variable is of type abap_bool, i. e. a single character is expected.
    "In this case, the variable is initial, i. e. the content is a blank.
    "Another variable is of type string. A blank is assigned to this
    "variable.
    "A logical expression compares the two variables. Without the conversion
    "using the CONV operator, the two are not equal due to the comparison
    "rules for character-like data types (trailing blanks are not respected
    "in variable-length strings). When the string is converted, the
    "comparison results to true.

    "Declaring data object and assign value
    DATA num TYPE i VALUE 1.

    "Effect of a calculation ...
    "... without conversion
    DATA(i) = num / 4.

    "... with conversion using an appropriate type
    DATA(dec_num) = CONV decfloat34( num / 4 ).

    out->write( data = i name = `i` ).
    out->write( |\n| ).
    out->write( data = dec_num name = `dec_num` ).
    out->write( |\n| ).

    "Declaring data objects
    DATA(txt) = VALUE abap_bool( ).

    DATA(str) = ` `.

    "Comparing the data objects with and without conversion
    out->write( `Without conversion:` ).

    IF txt = str.
      out->write( `txt is equal to str.` ).
    ELSE.
      out->write( `txt is not equal to str.` ).
    ENDIF.

    out->write( |\n| ).
    out->write( `With conversion:` ).

    IF txt = CONV abap_bool( str ).
      out->write( `txt is equal to converted str.` ).
    ELSE.
      out->write( `txt is not equal to converted str.` ).
    ENDIF.

    "Example with internal table types
    TYPES inttab_type TYPE TABLE OF i WITH EMPTY KEY.
    DATA int_itab TYPE SORTED TABLE OF i WITH NON-UNIQUE DEFAULT KEY.
    FIELD-SYMBOLS <it> TYPE inttab_type.
    int_itab = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).

    "The following assignment is not possible due to incompatible types.
    "The internal table has the same line type, but it has a different
    "table type and key.
    "ASSIGN itab TO <fs>.

    "Using CONV to convert the internal table to the required table type.
    DATA(conv_itab) = CONV inttab_type( int_itab ).
    ASSIGN conv_itab TO <it>.

    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = <it> name = `<it>` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `30) Constructing Data Objects with the CONV Operator` ) ).

    DATA(decnum1) = CONV decfloat34( '0.4' ).

    "Instead of
    DATA decnum2 TYPE decfloat34 VALUE '0.4'.
    "or
    DATA decnum3 TYPE decfloat34.
    decnum3 = '0.4'.

    out->write( `No output for this section. See the code.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `31) EXACT` ) ).

    "-------------- Lossless assignments -------------

    "Note: An assignment is made in accordance with conversion rules. Check
    "the ABAP Keyword Documentation for these rules. An assignment is only
    "made if no values are lost. Otherwise, an error occurs. Either it is
    "detected by static code checks or at runtime raising a catchable exception.
    TYPES clen3 TYPE c LENGTH 3.
    DATA(as1) = EXACT clen3( abap_true ).
    DATA(as2) = EXACT clen3( 'XY' ).
    "DATA(as3) = EXACT clen3( 'abcd' ).

    out->write( data = as1 name = `as1` ).
    out->write( |\n| ).
    out->write( data = as2 name = `as2` ).
    out->write( |\n| ).


    "Catching exception
    TRY.
        DATA(as4) = EXACT clen3( 'abcd' ).
        out->write( data = as4 name = `as4` ).
      CATCH cx_sy_conversion_data_loss INTO DATA(dl_err).
        out->write( data = dl_err->get_text( ) name = `dl_err->get_text( )` ).
    ENDTRY.
    out->write( |\n| ).

    "-------------- Lossless calculations -------------

    "The first statement works, whereas the second statement raises an exception.
    "A rounding to two decimal places is required.
    TYPES packednum TYPE p LENGTH 8 DECIMALS 2.
    DATA(calc1) = EXACT packednum( 1 / 4 ).
    "DATA(calc2) = EXACT packednum( 1 / 3 ).

    out->write( data = calc1 name = `calc1` ).
    out->write( |\n| ).

    "Catching exceptions when rounding in lossless calculations
    TRY.
        DATA(calc3) = EXACT packednum( 1 / 3 ).
        out->write( data = calc3 name = `calc3` ).
      CATCH cx_sy_conversion_rounding INTO DATA(lc_err).
        out->write( data = lc_err->get_text( ) name = `lc_err->get_text( )` ).
    ENDTRY.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `32) REF` ) ).

    "The example includes the declaration of a data object and some data
    "reference variables. One data reference variable is typed with a
    "complete type, the other one is typed with a generic type. Then, data
    "references to the data object declared before are created. Using the #
    "symbol means that the type can be automatically derived.
    "You can also use inline declarations to omit the explicit declaration.
    "Another example shows the explicit specification of the data type after
    "REF.
    "Furthermore, an object reference is created using inline declaration.

    "Declaring data object
    DATA number TYPE i VALUE 5.

    "Declaring data reference variables
    DATA dref_a TYPE REF TO i.    "Complete type
    DATA dref_b TYPE REF TO data. "Generic type

    "Creating data references to data objects
    dref_a = REF #( number ).
    dref_b = REF #( number ).

    "Data reference variable declared inline
    DATA(dref_c) = REF #( number ).

    "Type specified explicitly
    DATA(dref_d) = REF string( `hallo` ).

    "Object references
    DATA(oref_a) = NEW local_class( `Ciao` ).
    DATA(oref_b) = REF #( oref_a ).

    out->write( data = dref_a name = `dref_a` ).
    out->write( |\n| ).
    out->write( data = dref_b name = `dref_b` ).
    out->write( |\n| ).
    out->write( data = dref_c name = `dref_c` ).
    out->write( |\n| ).
    out->write( data = dref_d name = `dref_d` ).
    out->write( |\n| ).
    out->write( data = oref_b name = `oref_b` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `33) CAST` ) ).

    "The example demonstrates the CAST operator in the context of Run Time
    "Type Identification (RTTI).
    "First, the components of a structure are retrieved. Secondly, the
    "method information of a local class is retrieved. The syntax
    "particularly shows the advantages of downcasts carried out with the
    "CAST operator that also enables method chaining. An example is added
    "that demonstrates the use of the older ?= operator with which extra
    "variables are needed. Furthermore, simple downcasts are demonstrated
    "using data references.

    "RTTI examples
    "Using CAST
    "Retrieving structure components
    DATA(components_s2) =
      CAST cl_abap_structdescr(
        cl_abap_typedescr=>describe_by_data( s2 ) )->components.

    "Retrieving the methods of a local class
    DATA(methods) =
         CAST cl_abap_objectdescr(
                  cl_abap_objectdescr=>describe_by_name( 'LOCAL_CLASS' )
                    )->methods.

    "Excursion: Using the older cast operator ?=
    "Retrieving structure components
    "Note: More lines of code, helper variables needed
    DATA structdescr TYPE REF TO cl_abap_structdescr.

    structdescr ?= cl_abap_typedescr=>describe_by_data( s1 ).

    DATA(components_s1) = structdescr->components.

    "Casting with data references
    DATA dref_i TYPE REF TO i. "Complete type

    DATA dref_data TYPE REF TO data. "Generic type

    dref_data = NEW i( 123 ).

    dref_i = CAST #( dref_data ).

    out->write( data = components_s2 name = `components_s2` ).
    out->write( |\n| ).
    out->write( data = methods name = `methods` ).
    out->write( |\n| ).
    out->write( data = components_s1 name = `components_s1` ).
    out->write( |\n| ).
    out->write( data = dref_i name = `dref_i` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `34) COND` ) ).

    DATA(day_or_night) = COND #( WHEN cl_abap_context_info=>get_system_time( ) BETWEEN '050000' AND '220000'
                                 THEN `day`
                                 ELSE `night` ).

    out->write( data = day_or_night name = `day_or_night` ).
    out->write( |\n| ).

    "A constructor expression as above instead of, for example, an IF statement as follows.
    IF cl_abap_context_info=>get_system_time( ) BETWEEN '050000' AND '220000'.
      day_or_night = `day`.
    ELSE.
      day_or_night = `night`.
    ENDIF.

    out->write( data = day_or_night name = `day_or_night` ).
    out->write( |\n| ).

    "Multiple logical expressions initiated by WHEN
    "Also LET expressions are possible. See more details further down.
    DATA(time_of_day) = COND #( LET time = cl_abap_context_info=>get_system_time( ) IN
                                WHEN time BETWEEN '050001' AND '120000' THEN |Good morning, it's { time TIME = ISO }.|
                                WHEN time BETWEEN '120001' AND '180000' THEN |Good afternoon, it's { time TIME = ISO }.|
                                WHEN time BETWEEN '180001' AND '220000' THEN |Good evening, it's { time TIME = ISO }.|
                                ELSE |Good night, it's { time TIME = ISO }.| ).

    out->write( data = time_of_day name = `time_of_day` ).
    out->write( |\n| ).

    "THROW addition to raise an exception (working like RAISE EXCEPTION TYPE statements)
    "by specifying an exception class
    "Note: It is possible to ...
    "- specify the THROW addition also after THEN.
    "- make exceptions resumable using the RESUMABLE addition.
    DATA(number1) = 0.
    DATA(number2) = 0.
    TRY.
        "The example raises the exception because both operands have the value 0.
        DATA(div) = COND decfloat34( WHEN number1 <> 0 AND number2 <> 0 THEN number1 / number2
                                     WHEN number1 = 0  AND number2 <> 0 THEN number1 / number2
                                     ELSE THROW cx_sy_zerodivide( ) ).
        out->write( data = div name = `div` ).
      CATCH cx_sy_zerodivide.
        DATA(two_zeros) = `Zero division`.
        out->write( data = two_zeros name = `two_zeros` ).
    ENDTRY.

    out->write( |\n| ).

    "Excursion for the example above: The following statement does not result in an
    "error in ABAP (zero division 'allowed' if the first operand has also the value 0).
    div = 0 / 0.

    "THROW SHORTDUMP addition to raise a runtime error (working like RAISE SHORTDUMP
    "TYPE statements) by specifying an exception class; a message can be also passed,
    "and input parameters can be filled
*    TRY.
*        div = COND decfloat34( WHEN number1 <> 0 AND number2 <> 0 THEN number1 / number2
*                               WHEN number1 = 0  AND number2 <> 0 THEN number1 / number2
*                               ELSE THROW SHORTDUMP cx_sy_zerodivide( ) ).
*        out->write( data = div name = `div` ).
*      CATCH cx_sy_zerodivide.
*    ENDTRY.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `35) SWITCH` ) ).

    "The example demonstrates the use of the SWITCH operator. Here,
    "calculations are carried out. For this
    "purpose, a string table is created that includes arithmetic operators
    "and, purposely, one entry that does not fit. The internal table is looped
    "across. Based on the current arithmetic operator, the calculation is
    "carried out. A string is displayed accordingly. The code is embedded in
    "a TRY ... CATCH ... ENDTRY block to take care of arithmetic errors. For
    "example, you might insert the value 0 for the variable num2 to provoke 0
    "division. The CONV operator is also used in the example to respect
    "decimals.

    DATA operator_tab TYPE TABLE OF string.

    operator_tab = VALUE #( ( `+` ) ( `-` ) ( `*` ) ( `/` ) ( `#` ) ).

    DATA(num1) = 2.
    DATA(num2) = 4.

    LOOP AT operator_tab ASSIGNING FIELD-SYMBOL(<fs>).
      TRY.
          DATA(calc_result) =
              SWITCH string( <fs>
                         WHEN '+' THEN |{ num1 + num2 STYLE = SIMPLE }|
                         WHEN '-' THEN |{ num1 - num2 STYLE = SIMPLE }|
                         WHEN '*' THEN |{ num1 * num2 STYLE = SIMPLE }|
                         WHEN '/' THEN |{ CONV decfloat34( num1 / num2 ) STYLE = SIMPLE }|
                         ELSE `That doesn't work.` ).
          out->write( |{ num1 } { <fs> } { num2 } = { calc_result }| ).
        CATCH cx_sy_arithmetic_error INTO DATA(error).
          out->write( |Arithmetic error. { error->get_text( ) }| ).
      ENDTRY.
    ENDLOOP.
    out->write( |\n| ).
    out->write( |\n| ).

    "A constructor expression as above instead of, for example, a CASE statement as follows.
    DATA(operator) = '+'.
    CASE operator.
      WHEN '+'.
        calc_result = |{ num1 + num2 STYLE = SIMPLE }|.
      WHEN '-'.
        calc_result = |{ num1 - num2 STYLE = SIMPLE }|.
      WHEN '*'.
        calc_result = |{ num1 * num2 STYLE = SIMPLE }|.
      WHEN '/'.
        calc_result = |{ CONV decfloat34( num1 / num2 ) STYLE = SIMPLE }|.
      WHEN OTHERS.
        calc_result = `Wrong operator.`.
    ENDCASE.

    out->write( data = calc_result name = `calc_result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `36) FILTER` ) ).

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
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading(  `37) Iteration Expressions with FOR` ) ).

    "Data objects and types to work with in the examples
    TYPES: BEGIN OF s,
             col1 TYPE c LENGTH 5,
             col2 TYPE i,
             col3 TYPE i,
           END OF s.
    TYPES it_type TYPE TABLE OF s WITH EMPTY KEY.
    DATA(itab4for) = VALUE it_type( ( col1 = 'a' col2 = 1 col3 = 30 )
                                    ( col1 = 'bb' col2 = 2 col3 = 10 )
                                    ( col1 = 'ccc' col2 = 3 col3 = 20 ) ).

    "-------------- Table iterations --------------

    DATA(it1) = VALUE it_type( FOR wa IN itab4for ( col1 = wa-col1 && 'z'
                                                  col2 = wa-col2 + 1 ) ).

    out->write( data = it1 name = `it1` ).
    out->write( |\n| ).

    "LOOP AT equivalent
    CLEAR it1.
    LOOP AT itab4for INTO DATA(wa_loop).
      APPEND VALUE #( col1 = wa_loop-col1 && 'z'
                      col2 = wa_loop-col2 + 1 ) TO it1.
    ENDLOOP.

    out->write( data = it1 name = `it1` ).
    out->write( |\n| ).

    "The following example shows more syntax options
    "- Field symbol specifed after FOR
    "- LET expressions after FOR: Denotes that the LET
    "  expressions is evaluated for each loop pass
    "- INDEX INTO addition (the variable that follows implicitly
    "  has the type i): Storing the sy-tabix value for each
    "  loop pass
    DATA(it2) = VALUE it_type( FOR <line> IN itab4for INDEX INTO idx
                                 LET idxplus1 = idx + 1 IN
                                 ( col1 = <line>-col1 col2 = idx col3 = idxplus1 ) ).

    out->write( data = it2 name = `it2` ).
    out->write( |\n| ).

    "Similar to the example above, the following example uses the INDEX INTO
    "addition, as well as a LET expression with multiple local variables
    DATA(it3) = VALUE string_table( FOR <str> IN itab4for INDEX INTO idx
                                    LET col1            = |COL1: "{ <str>-col1 }"|
                                        col2            = |COL2: "{ <str>-col2 }"|
                                        col3            = |COL3: "{ <str>-col3 }"|
                                        str_to_be_added = |Table index { idx } -> { col1 } / { col2 } / { col3 }|
                                    IN ( str_to_be_added ) ).

    out->write( data = it3 name = `it3` ).
    out->write( |\n| ).

    "---------- Excursions ----------

    "Merging tables
    "In the following example, the content of two existing internal tables is merged.
    "In the simple example, the index is used for the table index. You can also imagine
    "that you merge two internal tables, both having multiple columns. You could refer
    "to the specific component values, for example, using a free key in a table expression
    "such as ... VALUE #( some_itab[ comp_x = wa-comp_y ]-comp_z DEFAULT ... ) ...
    TYPES inttabtype TYPE TABLE OF i WITH EMPTY KEY.
    DATA(integertable) = VALUE inttabtype( ( 99 ) ( 100 ) ).

    DATA(it4) = VALUE it_type( FOR wa IN itab4for INDEX INTO idx
                                 ( col1 = wa-col1 col2 = VALUE #( integertable[ idx ] DEFAULT 0 ) ) ).

    out->write( data = it4 name = `it4` ).
    out->write( |\n| ).

    "Retaining non-specified column values using the BASE addition
    "In the example, the original value of col3 is retained.
    DATA(it5) = VALUE it_type( FOR wa IN itab4for ( VALUE #( BASE wa col1 = wa-col1 && 'y'
                                                                   col2 = wa-col2 + 3 ) ) ).

    out->write( data = it5 name = `it5` ).
    out->write( |\n| ).

    "Using the CORRESPONDING operator to handle different types
    TYPES: BEGIN OF s2,
             col1 TYPE c LENGTH 5,
             col2 TYPE i,
             str  TYPE string,
           END OF s2.
    TYPES itab_type_2 TYPE TABLE OF s2 WITH EMPTY KEY.

    DATA(it6) = VALUE itab_type_2( FOR wa IN itab4for ( CORRESPONDING #( wa ) ) ).

    out->write( data = it6 name = `it6` ).
    out->write( |\n| ).

    "Multiple FOR expressions that work like nested loops
    DATA(it7) = VALUE string_table( FOR wa1 IN itab4for
                                    FOR wa2 IN integertable
                                    ( |Comp. 1st itab: "{ wa1-col1 }", comp. 2nd itab: "{ wa2 }"| ) ).

    out->write( data = it7 name = `it7` ).
    out->write( |\n| ).

    "LOOP AT equivalent
    CLEAR it7.
    LOOP AT itab4for INTO DATA(wa3).
      LOOP AT integertable INTO DATA(wa4).
        it7 = VALUE #( BASE it7 ( |Comp. 1st itab: "{ wa3-col1 }", comp. 2nd itab: "{ wa4 }"| ) ).
      ENDLOOP.
    ENDLOOP.

    out->write( data = it7 name = `it7` ).
    out->write( |\n| ).

    "More additions can be specified such as WHERE, USING KEY, FROM/TO, STEP

    "WHERE condition
    "The WHERE condition must be placed in parentheses.
    DATA(it8) = VALUE it_type( FOR wa IN itab4for WHERE ( col2 < 3 ) ( col1 = wa-col1 && 'w'
                                                                     col2 = 5
                                                                     col3 = wa-col2 ) ).

    out->write( data = it8 name = `it8` ).
    out->write( |\n| ).

    "FROM/TO additions
    DATA(it9) = VALUE it_type( FOR wa IN itab4for FROM 2 TO 3 ( col1 = wa-col1 && 'v'
                                                              col2 = 6
                                                              col3 = wa-col2 + 5   ) ).

    out->write( data = it9 name = `it9` ).
    out->write( |\n| ).

    "STEP addition
    DATA(it10) = VALUE it_type( FOR wa IN itab4for STEP -1 ( col1 = wa-col1 && 'u'
                                                           col2 = 7
                                                           col3 = wa-col2 + 8 ) ).

    out->write( data = it10 name = `it10` ).
    out->write( |\n| ).

    "USING KEY addition
    DATA(it11) = VALUE it_type( FOR wa IN itab4for USING KEY primary_key ( col1 = wa-col1 && 't'
                                                                         col2 = 9
                                                                         col3 = wa-col2 + 10 ) ).

    out->write( data = it11 name = `it11` ).
    out->write( |\n| ).

    "---------- Conditional iterations ----------

    "FOR ... WHILE ...
    DATA(it12) = VALUE it_type( FOR x = 1 WHILE x < 4
                                  ( col1 = x col2 = x + 1 col3 = x + 2 ) ).

    out->write( data = it12 name = `it12` ).
    out->write( |\n| ).

    "FOR ... UNTIL ...
    "The THEN addition is also possible for ... WHILE ...
    DATA(it13) = VALUE it_type( FOR y = 31 THEN y - 10 UNTIL y < 10
                                  ( col1 = y col2 = y + 1 col3 = y + 2 ) ).

    out->write( data = it13 name = `it13` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `38) More Examples with Iteration Expressions with FOR` ) ).

    "The examples demonstrate iteration expressions with FOR. The examples
    "are based on demo internal tables that are filled using a method. The
    "tables are displayed to show the original content of the internal
    "tables that are to be processed.

    out->write( |Original table content\n\n| ).

    "Method to fill demo internal tables with values to work with.
    "Tables are displayed showing the values.
    fill_struc_and_tab( ).

    out->write( data = tab1 name = `tab1` ).
    out->write( |\n| ).
    out->write( data = tab2 name = `tab2` ).
    out->write( |\n| ).
    out->write( data = tab3 name = `tab3` ).

    out->write( |\n| ).
    out->write( `************ FOR ... IN ... (LOOP Semantics) ************` ).
    out->write( |\n| ).

    "Examples demonstrating FOR ... IN ... that has the semantics of LOOP.
    "1) An internal table is looped across. The whole line is stored in a
    "new table which is declared inline. The target table must have the
    "same table type as the internal table that is looped across. The
    "example is without a WHERE condition, i. e. all lines of the source
    "table are considered.
    "2) A new table is created that has a different table type. However,
    "both table types have identically named components. The example shows
    "how you can fill all identically named components of the target table.
    "The example includes a WHERE condition to restrict the lines to be
    "considered. Pay attention to potential type conversions.
    "3) A new table is created that has a different table type. Here, it is
    "shown that the components and their assignment might be specified
    "individually. A WHERE clause is included. Pay attention to potential
    "type conversions.

    "1) Storing the whole line in a new table.
    "The target table must have the same table type as the source table.
    "Without the WHERE condition, all lines are considered.

    TYPES t_type1 LIKE tab3.

    DATA(for1) = VALUE t_type1( FOR w IN tab3 ( w ) ).

    "2) Storing specific components having the same names.
    "The target type is not compatible to the type of the source table.
    "Identically named components exist.

    TYPES t_type2 LIKE tab1.

    DATA(for2) = VALUE t_type2( FOR w IN tab3
                                WHERE ( comp4 > 7 )
                                ( CORRESPONDING #( w ) ) ).

    "3) Specify components individually and providing a mapping

    DATA(for3) = VALUE t_type2( FOR w IN tab3
                                WHERE ( comp4 > 7 )
                                ( comp1 = w-comp1
                                  comp2 = `hallo`
                                  comp3 = w-comp4 ) ).

    out->write( data = for1 name = `for1` ).
    out->write( |\n| ).
    out->write( data = for2 name = `for2` ).
    out->write( |\n| ).
    out->write( data = for3 name = `for3` ).
    out->write( |\n| ).

    "The example demonstrates multiple iteration expressions with FOR. Here,
    "a new table is created that is declared inline. Three tables are
    "respected. The lines to be considered are restricted by a WHERE
    "clause.
    "A component name mapping takes care of assigning specific values to
    "non-identically named components.

    "Declaring an internal table type
    TYPES: BEGIN OF line_type2,
             compX TYPE c LENGTH 1,
             compY TYPE string,
             compZ TYPE i,
           END OF line_type2,
           t_type3 TYPE STANDARD TABLE OF line_type2 WITH EMPTY KEY.

    "Nested iteration expressions with FOR
    DATA(for4) = VALUE t_type3(
      FOR w1 IN tab1 WHERE ( comp1 = 'A' )
      FOR w2 IN tab2 WHERE ( comp4 > 6 )
      FOR w3 IN tab3 WHERE ( comp3 < 4 )
      ( compX = w1-comp1
        compY = w2-comp1
        compZ = w3-comp3 ) ).

    out->write( data = for4 name = `for4` ).

    out->write( |\n| ).
    out->write( `************ FOR ... WHILE/UNTIL ... (DO/WHILE Semantics) ************` ).
    out->write( |\n| ).

    "Examples demonstrating FOR ... WHILE/UNTIL ... that has the semantics
    "of DO/WHILE.
    "The example demonstrates the construction of internal tables using
    "condition iterations with a constructor expression
    "and the corresponding variant of the value operator VALUE. Two internal
    "tables with different iterations, one using FOR ... WHILE ..., the
    "other FOR ... UNTIL ..., are created.

    "Declaring and internal table type
    TYPES:
      BEGIN OF line_type3,
        col1 TYPE i,
        col2 TYPE i,
        col3 TYPE i,
      END OF line_type3,
      t_type4 TYPE STANDARD TABLE OF line_type3 WITH EMPTY KEY.

    "FOR ... WHILE ...
    DATA(for5) = VALUE t_type4(
          FOR x = 11 THEN x + 10 WHILE x < 40
          ( col1 = x col2 = x + 1 col3 = x + 2 ) ).

    "FOR ... UNTIL ...
    DATA(for6) = VALUE t_type4(
          FOR y = 31 THEN y - 10 UNTIL y < 10
          ( col1 = y col2 = y + 1 col3 = y + 2 ) ).

    out->write( data = for5 name = `for5` ).
    out->write( |\n| ).
    out->write( data = for6 name = `for6` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `39) REDUCE` ) ).

    "Data objects and types to work with in the examples
    TYPES: BEGIN OF st,
             col1 TYPE c LENGTH 5,
             col2 TYPE i,
             col3 TYPE i,
           END OF st.
    TYPES tab_type TYPE TABLE OF st WITH EMPTY KEY.
    DATA(itab4reduce) = VALUE tab_type( ( col1 = 'a' col2 = 1 col3 = 30 )
                                  ( col1 = 'bb' col2 = 2 col3 = 10 )
                                  ( col1 = 'ccc' col2 = 3 col3 = 20 ) ).

    "---------- Table iterations ----------

    "Calculating the sum of values in a table column
    DATA(sum_val) = REDUCE i( INIT len = 0
                              FOR <l> IN itab4reduce
                              NEXT len = len + <l>-col2 ).

    out->write( data = sum_val name = `sum_val` ).
    out->write( |\n| ).

    "Getting the longest string in a table column
    DATA(long_str) = REDUCE st-col1( INIT string = VALUE #( )
                                    FOR <l> IN itab4reduce
                                    NEXT string =  COND #( WHEN strlen( <l>-col1 ) > strlen( string )
                                                        THEN <l>-col1
                                                        ELSE string ) ).

    out->write( data = long_str name = `long_str` ).
    out->write( |\n| ).

    "Getting the maximum value (other than, for example, using a SORT statement)
    "Unlike above, a variable is used instead of a field symbol.
    DATA(max_val) = REDUCE i( INIT max = 0
                              FOR line IN itab4reduce
                              NEXT max =  COND #( WHEN line-col2 > max
                                                  THEN line-col2
                                                  ELSE max ) ).

    out->write( data = max_val name = `max_val` ).
    out->write( |\n| ).

    "Creating a new internal table using REDUCE
    "In the example, the sum of two values is calculated.
    "A VALUE expression with the BASE addition is used to
    "add a line to a table (retaining the existing lines).
    DATA(itstr) = REDUCE string_table( INIT strtab = VALUE string_table( )
                                       FOR workarea IN itab4reduce
                                       NEXT strtab = VALUE #( BASE strtab
                                        ( |The sum of { workarea-col2 } and { workarea-col3 } is { workarea-col2 + workarea-col3 }.| ) ) ).

    out->write( data = itstr name = `itstr` ).
    out->write( |\n| ).

    "More additions are possible, such as specifying a WHERE condition (which
    "must be specified in parentheses). The following example creates a new
    "internal table based on a WHERE condition.
    TYPES: BEGIN OF s3,
             num1 TYPE i,
             num2 TYPE i,
             sum  TYPE i,
           END OF s3.
    TYPES s3_tab_type TYPE TABLE OF s3 WITH EMPTY KEY.
    DATA(itred) = REDUCE s3_tab_type( INIT tab = VALUE s3_tab_type( )
                                      FOR workarea IN itab4reduce
                                      WHERE ( col2 < 3 )
                                      NEXT tab = VALUE #( BASE tab
                                       ( num1 = workarea-col2 num2 = workarea-col3 sum = workarea-col2 + workarea-col3 ) ) ).

    out->write( data = itred name = `itred` ).
    out->write( |\n| ).

    "---------- Conditional iterations ----------

    "UNTIL addition
    "Iteratively calculating the sum from 1 to 10
    DATA(reduce_until) = REDUCE i( INIT sum = 0
                                   FOR  int = 1 UNTIL int > 10
                                   NEXT sum += int ).

    out->write( data = reduce_until name = `reduce_until` ).
    out->write( |\n| ).

    "WHILE addition
    "The example corresponds to the previous one.
    DATA(reduce_while) = REDUCE i( INIT sum = 0
                                   FOR  int = 1 WHILE int <= 10
                                   NEXT sum += int ).

    out->write( data = reduce_while name = `reduce_while` ).
    out->write( |\n| ).

    "THEN addition
    "The following example constructs a text string. The THEN addition is used
    "to decrement the iteration variable. Additionally, a LET expression is used
    "to specify a helper variable.
    "Result: Counting downwards starting with 10: 10 9 8 7 6 5 4 3 2 1
    DATA(count) = REDUCE string( LET start = 10 IN
                                 INIT text = |Counting downwards starting with { start }:|
                                 FOR n = start THEN n - 1 WHILE n > 0
                                 NEXT text &&= | { n }| ).

    out->write( data = count name = `count` ).
    out->write( |\n| ).

    "Example similar to the previous one. Using UNTIL, a text string is enlarged until
    "it has reached a specific size.
    "Result: ab abap abapap abapapap abapapapap abapapapapap abapapapapapap
    DATA(abap_str) =  REDUCE string( INIT text = ``
                                     FOR t = `ab` THEN t && `ap` UNTIL strlen( t ) > 15
                                     NEXT text &&= |{ t } | ).

    out->write( data = abap_str name = `abap_str` ).
    out->write( |\n| ).

    "---------- Excursion: Grouping lines in internal tables with VALUE/REDUCE ----------

    "The following examples show equivalents of LOOP AT GROUP ... GROUP BY ... statements.
    "Find more information and examples about grouping in the ABAP Keyword Documentation.

    "Internal table to work with in the examples
    DATA(itab4grp) = VALUE tab_type( ( col1 = 'a' col2 = 1 col3 = 2 )
                                      ( col1 = 'a' col2 = 3 col3 = 4 )
                                      ( col1 = 'a' col2 = 5 col3 = 6 )
                                      ( col1 = 'b' col2 = 7 col3 = 8 )
                                      ( col1 = 'b' col2 = 9 col3 = 10 )
                                      ( col1 = 'c' col2 = 11 col3 = 12 ) ).


    "Constructing a result using VALUE
    "The following example returns the values of identified groups in an internal table
    "Table lines are evaluated by grouping all lines that meet the condition
    "specified in GROUP BY (group key binding). The group key is stored in the variable
    "after FOR GROUPS (gr). The constructed result just consists of the group keys in
    "the example. The content of the members is not relevant.
    DATA(it_val_1) = VALUE string_table( FOR GROUPS gr OF wr IN itab4grp
                                         GROUP BY wr-col1 ASCENDING
                                         WITHOUT MEMBERS
                                         ( |{ gr }| ) ).

    out->write( data = it_val_1 name = `it_val_1` ).
    out->write( |\n| ).


    "As above, the following example returns the values of identified groups in an internal table.
    "Additionally, a LET expression (that itself contains an iteration expression) is specified
    "to collect column values by group in an internal table. The lines of this (string) table
    "are concatenated and inserted in the target table.
    DATA(it_val_2) = VALUE string_table(
        FOR GROUPS grp OF wr IN itab4grp
        GROUP BY wr-col1 ASCENDING
        LET members = VALUE string_table(
        FOR grpd IN GROUP grp ( |{ grpd-col2 }, { grpd-col3 }| ) ) IN
        ( |{ grp }: { concat_lines_of( table = members sep = ` / ` ) }| ) ).

    out->write( data = it_val_2 name = `it_val_2` ).
    out->write( |\n| ).

    "Constructing a result using REDUCE
    "The example is similar to the previous one by filling a string table.
    "The example uses a group key expression specified after GROUP BY.
    "In the group key expression, additional components of a structured
    "group key are specified which return specific information (group size,
    "group index).
    DATA(it_reduced) = REDUCE string_table(
        INIT li = VALUE string_table( )
        FOR GROUPS group OF grt IN itab4grp
        GROUP BY ( grpkey = grt-col1
                    size = GROUP SIZE
                    index = GROUP INDEX ) ASCENDING
        LET mem = VALUE string_table( FOR grpr IN GROUP group ( |{ grpr-col2 }, { grpr-col3 }| ) ) IN
        NEXT li = VALUE string_table( BASE li ( |Group key: "{ group-grpkey }" \| | &&
                                                |group size: {  group-size  } \| | &&
                                                |group index: { group-index } \| members: | &&
                                                |{ concat_lines_of( table = mem sep = ` / ` ) }| ) ) ).

    out->write( data = it_reduced name = `it_reduced` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `40) LET Expressions` ) ).

    "Data type and object to work with in the example
    TYPES: BEGIN OF st_type,
             comp1 TYPE c LENGTH 5,
             comp2 TYPE i,
             comp3 TYPE i,
           END OF st_type.
    DATA it TYPE TABLE OF st_type WITH EMPTY KEY.
    it = VALUE #( ( comp1 = 'a' comp2 = 1 comp3 = 30 )
                  ( comp1 = 'bb' comp2 = 2 comp3 = 10 )
                  ( comp1 = 'ccc' comp2 = 3 comp3 = 20 ) ).

    "Constructing a data object with elementary data type using the CONV operator
    "One or more helper variables are possible specified after LET
    DATA(hi) = CONV string(
            LET name = cl_abap_context_info=>get_user_technical_name( )
                date = cl_abap_context_info=>get_system_date( )
            IN |Hi { name }. Today's date is { date DATE = ISO }.| ).

    out->write( data = hi name = `hi` ).
    out->write( |\n| ).

    "Construction similar to the previous example
    "Depending on the time, a string is created. In the example, a LET expression
    "is specified for each constructor expression.
    DATA(timeofday) = CONV string(
            LET time = cl_abap_context_info=>get_system_time( ) IN
            COND string( LET good = `Good` ending = `ing` IN
                        WHEN time BETWEEN '050001' AND '120000' THEN good && ` morn` && ending  "Good morning
                        WHEN time BETWEEN '120001' AND '180000' THEN good && ` afternoon`
                        WHEN time BETWEEN '180001' AND '220000' THEN good && ` even` && ending
                        ELSE good && ` night`  ) ).

    out->write( data = timeofday name = `timeofday` ).
    out->write( |\n| ).

    "Getting a particular column name of an existing internal table using RTTI
    "An internal table (it contains information on the table's structured type; the
    "component names, among others) is assigned to a data object that is declared
    "inline. This is an example of making code more concise with constructor expressions
    "and inline declarations. Assume you use extra declarations for the data objects, or
    "use the older ?= operator for the casts. Many more lines of code are required.
    DATA(components) = CAST cl_abap_structdescr( CAST cl_abap_tabledescr(
        cl_abap_typedescr=>describe_by_data( it ) )->get_table_line_type( ) )->components.
    DATA(comp2_a) = components[ 2 ]-name.

    out->write( data = comp2_a name = `comp2_a` ).
    out->write( |\n| ).

    "Achieving the result from above even in one statement using LET
    DATA(comp2_b) = CONV abap_compname(
        LET comps = CAST cl_abap_structdescr( CAST cl_abap_tabledescr(
                     cl_abap_typedescr=>describe_by_data( it ) )->get_table_line_type( ) )->components
        IN comps[ 2 ]-name ).

    out->write( data = comp2_b name = `comp2_b` ).
    out->write( |\n| ).

    "Constructing a structure using local variables
    "The example uses the NEW operator to create an anonymous data object
    DATA(new_struc) = NEW st_type( LET n = 2 ch = 'AP' IN
                                    comp1 = 'AB' && ch comp2 = 2 * n comp3 = 3 * n ).

    out->write( data = new_struc->* name = `new_struc->*` ).
    out->write( |\n| ).

    "Constructing an internal table using local variables
    "The example uses the VALUE operator.
    "Note the parentheses ( ... ) representing table lines.
    DATA(itab_value) = VALUE string_table( LET lin = 1 IN
                                            ( |Line { lin }| )
                                            ( |Line { lin + 1 }| )
                                            ( |Line { lin + 2 }| ) ).

    out->write( data = itab_value name = `itab_value` ).
    out->write( |\n| ).

    "Using a local field symbol in LET expressions
    "- The right-hand side value must be the result of a writable expression, i.e.
    "  an operand that can be written to
    "- This value is then assigned to the local field symbol (as if ASSIGN is used)
    "- In the examples above, a specification such as ... LET <a> = 1 IN ... is not
    "  possible as they are not writable expressions.
    "- Writable expressions:
    "  - Constructor expressions NEW class( ... )->attr and CAST type( ... )->dobj
    "  - Table expressions itab[ ... ] and their chainings, e.g. itab[ 1 ]-comp
    "In the following example, an internal table is looped over. A string is created
    "from the table line content. In the constructor expression, a LET expression is
    "specified that uses a field symbol. It is assigned the line of the internal table.
    "The sy-index value represents the table index value.
    DATA str_tab TYPE string_table.
    DO lines( it ) TIMES.
      DATA(concatenated_tab) = CONV string(
          LET <li>  = it[ sy-index ]
              comma =   `, `
          IN  |{ <li>-comp1 }{ comma }{ <li>-comp2 }{ comma }{ <li>-comp3 }| ).
      str_tab = VALUE #( BASE str_tab ( concatenated_tab ) ).
    ENDDO.

    out->write( data = str_tab name = `str_tab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `41) More LET Examples` ) ).

    "The examples demonstrate LET expressions in different contexts.

    "1) LET within a constructor expression with VALUE: The temporary
    "variable is assigned a value of type string and all lines of the
    "resulting table (a table of type string) receive the content of this
    "variable in the specified position.

    DATA(stringtable) = VALUE string_table( LET be = `be` IN
                      ( |To { be } is to do| )
                      ( |To { be } or not to { be }| )
                      ( |To do is to { be }| )
                      ( |Do { be } do { be } do| ) ).

    out->write( data = stringtable name = `stringtable` ).
    out->write( |\n| ).

    "2) LET within a constructor expression with COND: 12 o'clock is
    "specified as value for the LET expression. Based on this value, checks
    "are carried out and an appropriate result is returned.

    DATA(system_time) = cl_abap_context_info=>get_system_time( ).

    DATA(systime) =
      COND #( LET tm = '120000' IN
              WHEN system_time < tm THEN
                |{ system_time TIME = ISO } AM|
              WHEN system_time > tm AND system_time < '240000' THEN
                |{ CONV t( system_time - 12 * 3600 ) TIME = ISO } PM|
              WHEN system_time = tm THEN |High Noon|
              ELSE |?| ).

    out->write( data = systime name = `systime` ).
    out->write( |\n| ).

    "3) An internal table that includes three components is created and
    "filled. A loop across this table is carried out. The purpose of the
    "constructor expression is to construct a string by concatenating the
    "values in the table columns. Here, the LET expression includes a field
    "symbol representing the table line. The field symbol receives the
    "concatenation result which is then stored in the variable that is
    "declared inline. The content is then added to an internal table of type
    "string which is then output to show the result of the loop.

    TYPES:
      BEGIN OF date,
        year  TYPE c LENGTH 4,
        month TYPE c LENGTH 2,
        day   TYPE c LENGTH 2,
      END OF date,
      dates TYPE TABLE OF date WITH EMPTY KEY.

    DATA stringtab TYPE TABLE OF string WITH EMPTY KEY.

    DATA(dates) = VALUE dates(
      ( year = '2020' month = '07' day = '16' )
      ( year = '2021' month = '08' day = '31' )
      ( year = '2022' month = '09' day = '07' ) ).

    DO lines( dates ) TIMES.
      DATA(isodate) = CONV string(
        LET <date>  = dates[ sy-index ]
            separator   =   '-'
         IN  <date>-year && separator && <date>-month &&
             separator && <date>-day ).

      "Adding line to table
      stringtab = VALUE #( BASE stringtab ( isodate ) ).
    ENDDO.

    out->write( data = stringtab name = `stringtab` ).
  ENDMETHOD.
ENDCLASS.
