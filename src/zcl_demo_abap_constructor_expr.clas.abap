***********************************************************************
*
*           ABAP cheat sheet: Constructor expressions
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate the use of constructor expressions as outlined
*   in the respective ABAP cheat sheet.
* - Topics covered: Operators VALUE, CORRESPONDING, NEW, CONV, EXACT, REF,
*   CAST, COND, SWITCH, FILTER, REDUCE, Iteration Expressions with FOR,
*   LET Expressions
*
* ----------------------- GETTING STARTED -----------------------------
* - Open the class with the ABAP Development Tools (ADT).
* - Choose F9 to run the class.
* - Check the console output.
* - To understand the context and the ABAP syntax used, check the notes
*   included in the class as comments or refer to the respective topic
*   in the ABAP Keyword Documentation.
* - Due to the amount of output in the console, the examples include
*   numbers (e. g. 1) ..., 2) ..., 3) ...) for the individual example
*   sections. Plus, the variable name is displayed in most cases. Hence,
*   to easier and faster find the relevant output in the console, just
*   search in the console for the number/variable name (STRG+F in the
*   console) or use the debugger.
*
* ----------------------------- NOTE -----------------------------------
* The code presented in this class is only meant for supporting the ABAP
* cheat sheets. It is not intended for direct use in a
* production system environment. The code examples in the ABAP cheat
* sheets are primarily intended to provide a better explanation and
* visualization of the syntax and semantics of ABAP statements and not to
* solve concrete programming tasks. For production application programs,
* a dedicated solution should therefore always be worked out for each
* individual case. There is no guarantee for either the correctness or
* the completeness of the code. In addition, there is no legal
* responsibility or liability for possible errors or their consequences
* which occur through the use of the example code.
*
***********************************************************************
"! <p class="shorttext synchronized">ABAP cheat sheet: Constructor expressions</p>
"! Example to demonstrate the use of constructor expressions.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_constructor_expr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

protected section.
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
      tab4 TYPE SORTED TABLE OF s2_type WITH NON-UNIQUE KEY comp3.

    CLASS-METHODS:
      fill_deep_structures,
      fill_struc_and_tab.

ENDCLASS.



CLASS ZCL_DEMO_ABAP_CONSTRUCTOR_EXPR IMPLEMENTATION.


  METHOD fill_deep_structures.
    "Clearing all contents of struc2
    CLEAR struc2.
    "Filling nested tables in deep structures
    struc2-struc_nested = VALUE #( comp1 = `aaa`
                                   comp2 = `bbb`
                                   comp3 = `ccc`).

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

    DATA(output) = NEW zcl_demo_abap_display( out ).

    output->display( `Demo: Constructor Expressions` ).


    output->display( `1. VALUE` ).
    output->display( `1.1 VALUE: Structures` ).

    "1) A flat structure is created based on a data type defined with a
    "TYPES statement. The structure is then filled using a constructor
    "expression with VALUE by specifying the components and assigning
    "values. Here, the type can be inferred, hence, a # symbol can be used.

    output->display( `1.1.1 Flat structure` ).

    TYPES: BEGIN OF struc_type,
             num   TYPE i,
             char1 TYPE c LENGTH 3,
             char2 TYPE c LENGTH 3,
           END OF struc_type.

    DATA struc TYPE struc_type.

    "Filling structure
    struc = VALUE #( num = 1 char1 = 'aaa'  char2 = 'abc' ).

    output->display( input = struc name = `struc` ).

    "2) The same structure is then filled purposely omitting components, i.
    "e. these components remain initial.

    output->next_section( `1.1.2 Omitting value assignment to components` ).

    struc = VALUE #( char1 = 'bbb' ).

    output->display( input = struc name = `struc` ).

    "3) The example demonstrates a variable that is declared inline. Here,
    "the result is a structure which is filled using a constructor
    "expression with VALUE and by specifying the components and assigning
    "values in parentheses. The structure type is specified explicitly.
    "The # symbol would not work since no type can be inferred from the
    "specified parameters.

    output->next_section( `1.1.3 Structure declared inline, explicit type specification` ).

    DATA(struc_inl) = VALUE struc_type( num   = 3
                                        char1 = 'ccc'
                                        char2 = 'def' ).

    output->display( input = struc_inl name = `struc_inl` ).


    output->next_section( `1.2 VALUE: Internal tables` ).

    "1) The example demonstrates the declaration of an internal table. The
    "internal table is then filled using a constructor expression with
    "VALUE.
    "The type can be inferred here and need not be specified explicitly.
    "Note the extra pair of parentheses in which the components are
    "specified and assigned values. In the example, 3 lines are added to
    "the table. For one line, some components are purposely not assigned.

    output->display( `1.2.1 Declaration and filling of an internal table` ).

    DATA itab TYPE TABLE OF struc_type WITH EMPTY KEY.

    itab = VALUE #( ( num = 1 char1 = 'aaa'  char2 = 'abc' )
                    ( num = 2 char1 = 'bbb'  char2 = 'def' )
                    ( num = 3 char1 = 'ccc'                  ) ).

    output->display( input = itab name = `itab` ).

    "2) The example demonstrates an internal table declared inline that is
    "filled using a constructor expression with VALUE by specifying the
    "internal table type explicitly. Note that the internal table type
    "cannot be generic in this context.

    output->next_section( `1.2.2 Table declared inline, explicitly specified table type` ).

    TYPES: itab_type TYPE STANDARD TABLE OF struc_type
                        WITH NON-UNIQUE KEY num.

    DATA(itab2) = VALUE itab_type(
                    ( num = 4 char1 = 'ddd' char2 = 'ghi' )
                    ( num = 5 char1 = 'eee'  char2 = 'jkl' ) ).

    output->display( input = itab2 name = `itab2` ).

    "3) Using the LINES OF addition, you can add lines of other tables.
    "Note: The line type of the other internal table must match the one of
    "the target internal table. Using FROM/TO, the table line selection can
    "be further restricted. Without FROM/TO, all lines of the table are
    "respected.

    output->next_section( `1.2.3 LINES OF addition` ).

    itab2 = VALUE #( ( num = 6 char1 = 'fff'  char2 = 'mno' )
                     ( LINES OF itab )
                     ( LINES OF itab FROM 1 TO 2 ) ).

    output->display( input = itab2 name = `itab2` ).

    "4) Using the BASE addition, you can keep existing content of the source
    "internal table.

    output->next_section( `1.2.4 BASE addition for keeping existing data` ).

    itab2 = VALUE #( BASE itab2 ( num = 7 char1 = 'ggg'
                                  char2 = 'pqr' ) ).

    output->display( input = itab2 name = `itab2` ).

    "5) Various examples using ABAP (SQL) statements in which table lines
    "are constructed inline.

    output->next_section( `ABAP statements and ABAP SQL statements` ).
    output->display( `1.2.5 Modifying internal table from a structure created inline` ).

    MODIFY TABLE itab2 FROM VALUE #( num   = 7
                                     char1 = 'hhh'
                                     char2 = 'stu' ).

    output->display( input = itab2 name = `itab2` ).


    output->next_section( `1.2.6 Inserting a table line that is created inline into an internal table` ).

    INSERT VALUE #( num = 8 char1 = 'iii'  char2 = 'vwx' )
      INTO TABLE itab2.

    output->display( input = itab2 name = `itab2` ).


    output->next_section( `1.2.7 Deleting a table entry based on a line created inline` ).

    DELETE TABLE itab2 FROM VALUE #( num = 3 ).

    output->display( input = itab2 name = `itab2` ).

    "Delete demo database table entries for demo below
    DELETE FROM zdemo_abap_carr.

    output->next_section( `1.2.8 Modifying a database table based on an internal table created inline` ).

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

    output->display( input = itab_carr name = `itab_carr` ).

    output->next_section( `1.3 Excursion: Deep structures and tables` ).

    "1) The example demonstrates the use of constructor expressions with
    "VALUE in the context of a deep structure. Here, a structure is declared
    "inline and filled. The structure type includes a nested structure. The
    "nested structure is filled using a nested VALUE expression.

    output->display( `1.3.1 Deep structure` ).

    TYPES: BEGIN OF deep_struc_ty,
             num   TYPE i,
             char1 TYPE c LENGTH 3,
             BEGIN OF substruc,
               int TYPE i,
               str TYPE string,
             END OF substruc,
           END OF deep_struc_ty.

    DATA(deep_struc) = VALUE deep_struc_ty( num = 1 char1 = 'aaa'

    substruc = VALUE #( int = 123 str = `hallo` ) ).

    output->display( input = deep_struc name = `deep_struc` ).

    "2) A deep internal table is created. Also here, nested VALUE
    "expressions are demonstrated.

    output->next_section( `1.3.2 Deep internal table` ).

    TYPES: BEGIN OF deep_struc_ty2,
             char TYPE c LENGTH 3,
             tab  TYPE TABLE OF i WITH EMPTY KEY,
           END OF deep_struc_ty2.

    TYPES: itab_deep_type TYPE STANDARD TABLE OF deep_struc_ty2
            WITH NON-UNIQUE KEY char.

    DATA(deep_itab) = VALUE itab_deep_type(
      ( char = 'aaa' tab = VALUE #( ( 1 ) ( 2 ) ( 3 ) ) )
      ( char = 'bbb' tab = VALUE #( ( 4 ) ( 5 ) ( 6 ) ) ) ).

    output->display( input = deep_itab name = `deep_itab` ).

    output->next_section( `2. CORRESPONDING` ).

    output->display( `2.1 CORRESPONDING: Simple Examples with ` &&
    `Structures and Internal Tables` ).

    "1) Displaying the original structures and tables that are filled in the
    "course of a method call. The structures and tables are filled anew
    "throughout the examples so that all CORRESPONDING expressions are based
    "on the same values.

    "Method to fill demo structures and internal tables
    "with values to work with
    fill_struc_and_tab( ).

    output->display( `2.1.1 Original structure and table content` ).
    output->display( input = s1 name = `s1` ).
    output->display( input = s2 name = `s2` ).
    output->display( input = tab1 name = `tab1` ).
    output->display( input = tab2 name = `it_st` ).

    "2) The target structure and table have a different type but identically
    "named components. The identically named components are filled. Note
    "that the target variables are initialized here. Also note the effect
    "of an automatic conversion of a variable-length string to a
    "fixed-length string (one component is typed with c, the other,
    "identically named component, is of type string).

    output->next_section( `2.1.2 CORRESPONDING without addition` ).

    s2 = CORRESPONDING #( s1 ).

    tab2 = CORRESPONDING #( tab1 ).

    output->display( input = s2 name = `s2` ).
    output->display( input = tab2 name = `tab2` ).

    fill_struc_and_tab( ).

    "3) The BASE addition keeps the original content. Structure: The non-
    "identical component name retains its value. Internal table: Existing
    "table lines are kept.

    output->next_section( `2.1.3 BASE addition for keeping original content` ).

    s2 = CORRESPONDING #( BASE ( s2 ) s1 ).

    tab2 = CORRESPONDING #( BASE ( tab2 ) tab1 ).

    output->display( input = s2 name = `s2` ).
    output->display( input = tab2 name = `tab2` ).

    fill_struc_and_tab( ).

    "4) The example demonstrates the additions MAPPING and EXCEPT. MAPPING:
    "One component of the target structure is assigned the value of a
    "dedicated component of the source structure. EXCEPT: All corresponding
    "components are assigned except a specific one.

    output->next_section( `2.1.4 MAPPING/EXCEPT additions` ).

    s2 = CORRESPONDING #( s1 MAPPING comp4 = comp3 ).

    tab2 = CORRESPONDING #( tab1 EXCEPT comp1 ).

    output->display( input = s2 name = `s2` ).
    output->display( input = tab2 name = `tab2` ).

    output->next_section( `2.2 CORRESPONDING: Demonstrating Various` &&
    ` Additions Using Deep Structures` ).

    "Displaying the original deep structures and tables that are filled in
    "the course of a method call. The deep structures and tables are filled
    "anew throughout the examples so that all CORRESPONDING expressions are
    "based on the same values.

    "Method to fill demo deep structures and internal tables
    "with values to work with
    fill_deep_structures( ).

    output->display( `2.2.1 Original content of deep structures` ).
    output->display( input = struc1 name = `struc1` ).
    output->display( input = struc2 name = `struc2` ).

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

    output->next_section( `2.2.2 CORRESPONDING without addition` ).

    struc2 = CORRESPONDING #( struc1 ).

    output->display( input = struc2 name = `struc2` ).

    fill_deep_structures( ).

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

    output->next_section( `2.2.3 DEEP addition` ).

    struc2 = CORRESPONDING #( DEEP struc1 ).

    output->display( input = struc2 name = `struc2` ).

    fill_deep_structures( ).

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

    output->next_section( `2.2.4 BASE addition` ).

    struc2 = CORRESPONDING #( BASE ( struc2 ) struc1 ).

    output->display( input = struc2 name = `struc2` ).

    fill_deep_structures( ).

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

    output->next_section( `2.2.5 DEEP BASE addition` ).

    struc2 = CORRESPONDING #( DEEP BASE ( struc2 ) struc1 ).

    output->display( input = struc2 name = `struc2` ).

    fill_deep_structures( ).

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

    output->next_section( `2.2.6 APPENDING addition` ).

    struc2 = CORRESPONDING #( APPENDING ( struc2 ) struc1 ).

    output->display( input = struc2 name = `struc2` ).

    fill_deep_structures( ).

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

    output->next_section( `2.2.7 DEEP APPENDING` ).

    struc2 = CORRESPONDING #( DEEP APPENDING ( struc2 ) struc1 ).

    output->display( input = struc2 name = `struc2` ).

    output->next_section( `3. NEW` ).
    output->display( `3.1 NEW: Creating Anonymous Data Objects` ).

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
      DATA(val) = `Initial values!`.
    ELSE.
      val = `No initial values!`.
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

    output->display( input = val name = `val` ).
    output->display( input = dref1 name = `dref1` ).
    output->display( input = dref2 name = `dref2` ).
    output->display( input = dref3 name = `dref3` ).
    output->display( input = dref4 name = `dref4` ).
    output->display( input = dref5 name = `dref5` ).
    output->display( input = dref6 name = `dref6` ).

    output->next_section( `3.2 NEW: Creating Instances of Classes` ).

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

    output->display( input = oref1 name = `oref1` ).

    "Creating an instance of a class, object reference variable
    "is declared inline, explicit type specification
    DATA(oref2) = NEW local_class( `Salut` ).

    output->display( input = oref2 name = `oref2` ).

    "Method chaining
    DATA(result) = NEW local_class( `Ciao` )->double(
                        int = NEW #( 5 ) ).

    output->display( input = result name = `result` ).

    output->next_section( `4. CONV` ).

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

    output->display( input = i name = `i` ).
    output->display( input = dec_num name = `dec_num` ).

    "Declaring data objects
    DATA(txt) = VALUE abap_bool(  ).

    DATA(str) = ` `.

    "Comparing the data objects with and without conversion
    output->display( `Without conversion:`  ).

    IF txt = str.
      output->display( `txt is equal to str.`  ).
    ELSE.
      output->display( `txt is not equal to str.`  ).
    ENDIF.

    output->display( `With conversion:`  ).

    IF txt = CONV abap_bool( str ).
      output->display( `txt is equal to converted str.`  ).
    ELSE.
      output->display( `txt is not equal to converted str.`  ).
    ENDIF.

    output->next_section( `5. EXACT` ).

    "The examples show the effect of the EXACT operator that enforces either
    "a lossless assignment or a lossless calculation.
    "1) Demonstrates data loss when converting to a data object that expects
    "only a single character.
    "2) Demonstrates an impossible lossless calculation. A rounding is
    "necessary in this case.
    "3) The example compares the effect of the EXACT and CONV operator.

    "Example 1
    TRY.
        DATA(ex1) = EXACT abap_bool( 'XY' ).
      CATCH cx_sy_conversion_data_loss INTO DATA(e1).
        DATA(ex2) = e1->value.
        DATA(t1) = e1->get_text( ).
    ENDTRY.

    "Example 2
    TRY.
        DATA(ex3) = EXACT decfloat34( 1 / 3 ).
      CATCH cx_sy_conversion_rounding INTO DATA(e2).
        DATA(ex4) = e2->value.
        DATA(t2) = e2->get_text( ).
    ENDTRY.

    "Example 3
    "Comparing the effect of CONV and EXACT
    TYPES numtext TYPE n LENGTH 20.

    TRY.
        DATA(ex5) = EXACT numtext( '2 Apples + 5 Oranges' ).
      CATCH cx_sy_conversion_error INTO DATA(e3).
        DATA(t3) = e3->get_text( ).
    ENDTRY.

    DATA(conv_comp) = CONV numtext( '2 Apples + 5 Oranges' ).

    IF ex1 IS INITIAL.
      output->display( |ex2: { ex2 }; { t1 }|  ).
    ELSE.
      output->display( ex1 ).
    ENDIF.

    IF ex3 IS INITIAL.
      output->display( |ex4: { ex4 }; { t2 }|  ).
    ELSE.
      output->display( input = ex3 name = `ex3` ).
    ENDIF.

    IF ex5 IS INITIAL.
      output->display( input = t3 name = `t3` ).
    ELSE.
      output->display( input = ex5 name = `ex5` ).
    ENDIF.

    output->display( input = conv_comp name = `conv_comp` ).

    output->next_section( `6. REF` ).

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

    output->display( input = dref_a name = `dref_a` ).
    output->display( input = dref_b name = `dref_b` ).
    output->display( input = dref_c name = `dref_c` ).
    output->display( input = dref_d name = `dref_d` ).
    output->display( input = oref_b name = `oref_b` ).

    output->next_section( `7. CAST` ).

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

    "Using ?=
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

    output->display( input = components_s2 name = `components_s2` ).
    output->display( input = methods name = `methods` ).
    output->display( input = components_s1 name = `components_s1` ).
    output->display( input = dref_i name = `dref_i` ).

    output->next_section( `8. COND` ).

    "The example demonstrates the use of the COND operator. The syntax
    "includes several WHEN and THEN expressions.
    "The example returns a string based on the current time.

    DATA(syst_time) = cl_abap_context_info=>get_system_time( ).

    DATA(greets) =
      COND #( WHEN syst_time BETWEEN '050001' AND '120000'
              THEN |It's { syst_time TIME = ISO }. | &&
                   |Good morning, { sy-uname }.|
              WHEN syst_time BETWEEN '120001' AND '170000'
              THEN |It's { syst_time TIME = ISO }. | &&
                   |Good afternoon, { sy-uname }.|
              WHEN syst_time BETWEEN '170001' AND '210000'
              THEN |It's { syst_time TIME = ISO }. | &&
                   |Good evening, { sy-uname }.|
              WHEN syst_time BETWEEN '210001' AND '050000'
              THEN |It's { syst_time TIME = ISO }. | &&
                   |Good night, { sy-uname }.|
              ELSE |Hallo, { sy-uname }.|
              ).

    output->display( input = greets name = `greets` ).

    output->next_section( `9. SWITCH` ).

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
                         WHEN '+' THEN CONV decfloat34( num1 + num2 )
                         WHEN '-' THEN CONV decfloat34( num1 - num2 )
                         WHEN '*' THEN CONV decfloat34( num1 * num2 )
                         WHEN '/' THEN CONV decfloat34( num1 / num2 )
                         ELSE `That doesn't work.` ).
          output->display( |{ num1 } { <fs> } { num2 } = { calc_result }| ).
        CATCH cx_sy_arithmetic_error INTO DATA(error).
          output->display( |Arithmetic error. { error->get_text( ) }| ).
      ENDTRY.
    ENDLOOP.

    output->next_section( `10. FILTER` ).

    "The examples demonstrate the use of the FILTER operator.
    "1) Method to fill internal table with demo values. The tables are
    "displayed to show the original values of the internal table that is to
    "be filtered.
    "2) Filters an internal table based on a condition specified after
    "WHERE. A new internal table is created that is declared inline.
    "3) A new table is created based on filtering a table. Here, the
    "addition EXCEPT is used that excludes data according to a condition
    "specified after WHERE.
    "4) The filtering is based on entries in a filter table.

    "1) Method to fill demo internal table with values to work with.
    "The table is displayed showing the values.
    fill_struc_and_tab( ).

    "2) Filtering an internal table based on a condition.
    DATA(filter1) = FILTER #( tab4 WHERE comp3 > 5 ).

    "3) Filtering an internal table using EXCEPT addition
    DATA(filter2) = FILTER #( tab4 EXCEPT WHERE comp3 < 3 ).

    "4) Filtering an internal table using a filter table.
    "Creating and filling a filter table.
    DATA extract_tab TYPE SORTED TABLE OF i
      WITH NON-UNIQUE KEY table_line.

    extract_tab = VALUE #( ( 3 ) ( 5 ) ).

    DATA(filter3) = FILTER #( tab4 IN extract_tab
                              WHERE comp3 = table_line ).

    output->display( input = tab4 name = `tab4` ).
    output->display( input = filter1 name = `filter1` ).
    output->display( input = filter2 name = `filter2` ).
    output->display( input = filter3 name = `filter3` ).

    output->next_section( `11. Iteration Expressions with FOR` ).

    "The examples demonstrate iteration expressions with FOR. The examples
    "are based on demo internal tables that are filled using a method. The
    "tables are displayed to show the original content of the internal
    "tables that are to be processed.

    "Method to fill demo internal tables with values to work with.
    "Tables are displayed showing the values.
    fill_struc_and_tab( ).

    output->display( `11.1 Original table content` ).
    output->display( input = tab1 name = `tab1` ).
    output->display( input = tab2 name = `tab2` ).
    output->display( input = tab3 name = `tab3` ).

    output->next_section( `11.2 FOR ... IN ... (LOOP Semantics)` ).

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

    DATA(for1) = VALUE t_type1( FOR wa IN tab3 ( wa ) ).

    "2) Storing specific components having the same names.
    "The target type is not compatible to the type of the source table.
    "Identically named components exist.

    TYPES t_type2 LIKE tab1.

    DATA(for2) = VALUE t_type2( FOR wa IN tab3
                                WHERE ( comp4 > 7 )
                                ( CORRESPONDING #( wa ) ) ).

    "3) Specify components individually and providing a mapping

    DATA(for3) = VALUE t_type2( FOR wa IN tab3
                                WHERE ( comp4 > 7 )
                                ( comp1 = wa-comp1
                                  comp2 = `hallo`
                                  comp3 = wa-comp4 ) ).

    output->display( input = for1 name = `for1` ).
    output->display( input = for2 name = `for2` ).
    output->display( input = for3 name = `for3` ).

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
      FOR wa1 IN tab1 WHERE ( comp1 = 'A' )
      FOR wa2 IN tab2 WHERE ( comp4 > 6 )
      FOR wa3 IN tab3 WHERE ( comp3 < 4 )
      ( compX = wa1-comp1
        compY = wa2-comp1
        compZ = wa3-comp3  ) ).

    output->display( input = for4 name = `for4` ).

    output->next_section( `11.2 FOR ... WHILE/UNTIL ... ` &&
    `(DO/WHILE Semantics)` ).

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
          ( col1 = x col2 = x + 1 col3 = x + 2  ) ).

    "FOR ... UNTIL ...
    DATA(for6) = VALUE t_type4(
          FOR y = 31 THEN y - 10 UNTIL y < 10
          ( col1 = y col2 = y + 1 col3 = y + 2  ) ).

    output->display( input = for5 name = `for5` ).
    output->display( input = for6 name = `for6` ).

    output->next_section( `12. REDUCE` ).

    "The examples demonstrate the REDUCE operator using values contained in
    "an internal table column. Here, the table is of type string.
    "1) The values of the columns are sequentially concatenated into a
    "string.
    "INIT ...: A temporary variable is specified that sets an initial
    "value for the result variable
    "FOR ...: Represents a loop, the loop is carried out for all table
    "entries.
    "NEXT ...: Represents the assignment to the temporary variable after
    "every iteration.
    "Once the loop has finished, the target variable is assigned the
    "resulting value.
    "2) Also here, the table rows are reduced to a text string using a
    "chaining after NEXT. The auxiliary variable sep declared after
    "INIT is initial for the first read row and is filled with a blank here
    "for the evaluation of further rows.

    "Creating and filling a string table
    DATA tab TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    tab = VALUE #( ( `h` ) ( `a` ) ( `l` ) ( `l` ) ( `o` ) ).

    "Example 1
    DATA(a_word) =
          REDUCE string( INIT text = ``
            FOR word IN tab
            NEXT text = |{ text }{ word }|  ).

    "Example 2
    tab = VALUE #( ( `Some` ) ( `cool` ) ( `stuff` )
                   ( `using` ) ( `REDUCE` ) ).

    DATA(sentence) =
          REDUCE string( INIT text = `` sep = ``
            FOR word IN tab
            NEXT text = |{ text }{ sep }{ word }| sep = ` ` ) && '.'.

    output->display( input = a_word name = `a_word` ).
    output->display( input = sentence name = `sentence` ).

    "The examples demonstrate summations using the REDUCE operator.
    "1) Example using FOR ... UNTIL .... It calculates the total of the
    "numbers from 1 to 10. The resulting number is stored in a variable that
    "is declared inline.
    "2) The example has the same output as the first example. Here, a table
    "column is reduced. The table that is of type i is filled with numbers
    "from 1 to 10 (using a constructor expression with FOR ... WHILE ...).
    "The reduction is then carried out based on the numbers contained in
    "this table.

    "Example 1
    DATA(sum1) = REDUCE i( INIT b = 0
                            FOR n = 1 UNTIL n > 10
                            NEXT b += n ).

    "Example 2
    DATA itab_i TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    itab_i = VALUE #( FOR j = 1 WHILE j <= 10 ( j ) ).

    DATA(sum2) = REDUCE i( INIT x = 0
                           FOR z IN itab_i
                           NEXT x = x + z ).

    output->display( input = sum1 name = `sum1` ).
    output->display( input = sum2 name = `sum2` ).

    "The examples demonstrate the concatenation of strings
    "1) without the addition THEN
    "2) with the addition THEN
    "3) in the context of a non-arithmetic expression.

    "1) Concatenation without THEN
    DATA(conc1) = REDUCE string( INIT text = `Count up:`
                                 FOR n = 1 UNTIL n > 10
                                 NEXT text &&= | { n }| ).

    "2) Concatenation with THEN
    DATA(conc2) = REDUCE string( INIT text = `Count down:`
                                 FOR n = 10 THEN n - 1 WHILE n > 0
                                 NEXT text &&= | { n }| ).

    "3) Non-arithmetic expression
    DATA(conc3) = REDUCE string( INIT text = ``
                                 FOR t = `x` THEN t && `y`
                                             UNTIL strlen( t ) > 10
                                 NEXT text &&= |{ t } | ).

    output->display( input = conc1 name = `conc1` ).
    output->display( input = conc2 name = `conc2` ).
    output->display( input = conc3 name = `conc3` ).

    output->next_section( `13. LET Expressions` ).

    "The examples demonstrate LET expressions in different contexts.

    "1) LET within a constructor expression with VALUE: The temporary
    "variable is assigned a value of type string and all lines of the
    "resulting table (a table of type string) receive the content of this
    "variable in the specified position.

    DATA(str_tab) = VALUE string_table( LET it = `be` IN
                      ( |To { it } is to do| )
                      ( |To { it } or not to { it }| )
                      ( |To do is to { it }| )
                      ( |Do { it } do { it } do| ) ).

    output->display( input = str_tab name = `str_tab` ).

    "2) LET within a constructor expression with COND: 12 o'clock is
    "specified as value for the LET expression. Based on this value, checks
    "are carried out and an appropriate result is returned.

    DATA(system_time) = cl_abap_context_info=>get_system_time( ).

    DATA(time) =
      COND #( LET tm = '120000' IN
              WHEN system_time < tm THEN
                |{ system_time TIME = ISO } AM|
              WHEN system_time > tm AND system_time < '240000' THEN
                |{ CONV t( system_time - 12 * 3600 ) TIME = ISO } PM|
              WHEN system_time = tm THEN |High Noon|
              ELSE |?| ).

    output->display( input = time name = `time` ).

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
             separator && <date>-day  ).

      "Adding line to table
      stringtab = VALUE #( BASE stringtab ( isodate ) ).
    ENDDO.

    output->display( input = stringtab name = `stringtab` ).

  ENDMETHOD.
ENDCLASS.
