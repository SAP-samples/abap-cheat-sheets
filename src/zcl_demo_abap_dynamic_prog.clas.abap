***********************************************************************
*
*               ABAP cheat sheet: Dynamic programming
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntax options and concepts related
*   to dynamic programming.
* - Topics covered: Field symbols and data references (both as supporting
*   elements for dynamic programming), dynamic ABAP syntax components,
*   runtime type services (RTTS), i. e. runtime type identification (RTTI)
*   and runtime type creation (RTTC)
* - To provide a "real" dynamic determination at runtime for several code
*   examples in this class, the example class includes local classes
*   in the CCIMP include (local types tab in ADT) whose methods return
*   character-like content to be used in the ABAP statements. The content
*   is predefined in these classes but the content that is actually used
*   in the end is random.
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
"! <p class="shorttext synchronized">ABAP cheat sheet: Dynamic programming</p>
"! Example to demonstrate syntax related to dynamic programming.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_dynamic_prog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    CLASS-METHODS:
      class_constructor.
ENDCLASS.



CLASS zcl_demo_abap_dynamic_prog IMPLEMENTATION.


  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: Dynamic Programming\n\n| ).

**********************************************************************

    out->write( |Excursion: Field Symbols\n\n| ).
    out->write( |1) Declaring Field Symbols\n\n| ).

    "Some data declarations and type definitions used further down
    DATA: str TYPE string.

    TYPES: BEGIN OF struc, "Structured data type
             num1 TYPE i,
             num2 TYPE i,
           END OF struc,
           tab_type TYPE TABLE OF struc. "Internal table type

    "Field symbol declarations
    "- Name of the field symbol goes between angle brackets
    "- Type: either a complete data type or a generic type.

    "Complete types
    "Here, a chained statement with a colon is used.
    FIELD-SYMBOLS: <fs_i>        TYPE i,
                   <fs_flsch>    TYPE zdemo_abap_flsch,
                   <fs_tab_type> TYPE LINE OF tab_type,
                   <fs_like>     LIKE str.

    "Generic types
    "There are plenty of options for generic ABAP types. Check the
    "keyword docu.
    FIELD-SYMBOLS <fs_c>         TYPE c.           "Text field with a generic length
    FIELD-SYMBOLS <fs_cseq>      TYPE csequence.   "Text-like (c, string)
    FIELD-SYMBOLS <fs_data>      TYPE data.        "Any data type
    FIELD-SYMBOLS <fs_any_table> TYPE ANY TABLE.   "Internal table with any table type

    "Declaring field symbols inline
    "Prominent use case: Inline declaration of a field symbol for an internal table
    "following ASSIGNING.
    DATA demo_itab TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY.

    LOOP AT demo_itab ASSIGNING FIELD-SYMBOL(<line>).
      ...
    ENDLOOP.

    out->write( `No output for this section. You can set a break point and check the content of data objects in the debugger.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Assigning Data Objects to Field Symbols` ) ).

    "ASSIGN statements assigns the memory area of a data object to a field symbol.
    "Once the memory area is assigned, you can work with the content.
    "You can also assign a particular component of a structure. Either you
    "specify the position of the component or the name of the component.

    "Data objects.
    DATA: num_a   TYPE i,
          struc_a TYPE zdemo_abap_fli,
          tab_a   TYPE string_table.

    "Field symbols with complete types
    FIELD-SYMBOLS: <fs_i_a>     TYPE i,
                   <fs_struc_a> TYPE zdemo_abap_fli,
                   <fs_tab_a>   TYPE string_table.

    "Field symbols with generic types, see more examples bwloe
    FIELD-SYMBOLS <fs_data_a>   TYPE data.
    FIELD-SYMBOLS <fs_anytab_a> TYPE ANY TABLE.

    "Assigning data objects to field symbols
    ASSIGN num_a    TO <fs_i_a>.
    ASSIGN struc_a  TO <fs_struc_a>.
    ASSIGN tab_a    TO <fs_tab_a>.
    ASSIGN: num_a   TO <fs_data_a>,
            struc_a TO <fs_data_a>,
            tab_a   TO <fs_data_a>,
            tab_a   TO <fs_anytab_a>.

    "Inline declaration is possible, too. The field symbol is implicitly typed
    "with the generic type data.
    ASSIGN num_a TO FIELD-SYMBOL(<fs_inl>).

    "Generic typing
    "- Generic types are available with which formal parameters of methods or field symbols
    "  can be specified.
    "- At runtime, the actual data type is copied from the assigned actual parameter or
    "  memory area, i.e. they receive the complete data type only when an actual parameter
    "  is passed or a memory area is assigned.

    FIELD-SYMBOLS:
      "Any data type
      <data>           TYPE data,
      <any>            TYPE any,
      "Any data type can be assigned. Restrictions for formal parameters and 'data': no
      "numeric functions, no description functions, and no arithmetic expressions can be
      "passed to these parameters. However, you can bypass the restriction by applying the
      "CONV operator for the actual parameter.

      "Character-like types
      <c>              TYPE c,         "Text field with a generic length
      <clike>          TYPE clike,     "Character-like (c, n, string, d, t and character-like flat structures)
      <csequence>      TYPE csequence, "Text-like (c, string)
      <n>              TYPE n,         "Numeric text with generic length
      <x>              TYPE x,         "Byte field with generic length
      <xsequence>      TYPE xsequence, "Byte-like (x, xstring)

      "Numeric types
      <decfloat>       TYPE decfloat, "decfloat16, decfloat34)
      <numeric>        TYPE numeric,  "Numeric ((b, s), i, int8, p, decfloat16, decfloat34, f)
      <p>              TYPE p,        "Packed number (generic length and number of decimal places)

      "Internal table types
      <any_table>      TYPE ANY TABLE,      "Internal table with any table type
      <hashed_table>   TYPE HASHED TABLE,
      <index_table>    TYPE INDEX TABLE,
      <sorted_table>   TYPE SORTED TABLE,
      <standard_table> TYPE STANDARD TABLE,
      <table>          TYPE table,          "Standard table

      "Other types
      <simple>         TYPE simple, "Elementary data type including enumerated types and
      "structured types with exclusively character-like flat components
      <object>         TYPE REF TO object. "object can only be specified after REF TO; can point to any object

    "Data objects to work with
    DATA: BEGIN OF s,
            c3        TYPE c LENGTH 3,
            c10       TYPE c LENGTH 10,
            n4        TYPE n LENGTH 4,
            str       TYPE string,
            time      TYPE t,
            date      TYPE d,
            dec16     TYPE decfloat16,
            dec34     TYPE decfloat34,
            int       TYPE i,
            pl4d2     TYPE p LENGTH 4 DECIMALS 2,
            tab_std   TYPE STANDARD TABLE OF string WITH EMPTY KEY,
            tab_so    TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line,
            tab_ha    TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line,
            xl1       TYPE x LENGTH 1,
            xstr      TYPE xstring,
            structure TYPE zdemo_abap_carr, "character-like flat structure
            oref      TYPE REF TO object,
          END OF s.

    "The following static ASSIGN statements demonstrate various assignments
    "Note:
    "- The statements commented out show impossible assignments.
    "- If a static assignment is not successful, sy-subrc is not set and no
    "  memory area is assigned. Dynamic assignments, however, set the value.

    "----- Any data type -----
    ASSIGN s-c3 TO <data>.
    ASSIGN s-time TO <data>.
    ASSIGN s-tab_std TO <data>.
    ASSIGN s-xstr TO <any>.
    ASSIGN s-pl4d2 TO <any>.
    ASSIGN s-date TO <any>.

    "----- Character-like types -----
    ASSIGN s-c3 TO <c>.
    ASSIGN s-c10 TO <c>.
    "ASSIGN s-str TO <c>.

    ASSIGN s-c10 TO <clike>.
    ASSIGN s-str TO <clike>.
    ASSIGN s-n4 TO <clike>.
    ASSIGN s-date TO <clike>.
    ASSIGN s-time TO <clike>.
    ASSIGN s-structure TO <clike>.

    ASSIGN s-c10 TO <csequence>.
    ASSIGN s-str TO <csequence>.
    "ASSIGN s-n4 TO <csequence>.

    ASSIGN s-n4 TO <n>.
    "ASSIGN s-int TO <n>.
    "ASSIGN s-time TO <n>.

    ASSIGN s-xl1 TO <x>.
    "ASSIGN s-xstr TO <x>.

    ASSIGN s-xl1 TO <xsequence>.
    ASSIGN s-xstr TO <xsequence>.

    "----- Numeric types -----
    ASSIGN s-dec16 TO <numeric>.
    ASSIGN s-dec34 TO <numeric>.
    ASSIGN s-int TO <numeric>.
    ASSIGN s-pl4d2 TO <numeric>.
    "ASSIGN s-n4 TO <numeric>.

    ASSIGN s-dec16 TO <decfloat>.
    ASSIGN s-dec34 TO <decfloat>.

    ASSIGN s-pl4d2 TO <p>.
    "ASSIGN s-dec34 TO <p>.

    "----- Internal table types -----
    ASSIGN s-tab_std TO <any_table>.
    ASSIGN s-tab_so TO <any_table>.
    ASSIGN s-tab_ha TO <any_table>.

    ASSIGN s-tab_std TO <index_table>.
    ASSIGN s-tab_so TO <index_table>.
    "ASSIGN s-tab_ha TO <index_table>.

    "ASSIGN s-tab_std TO <sorted_table>.
    ASSIGN s-tab_so TO <sorted_table>.
    "ASSIGN s-tab_ha TO <sorted_table>.

    ASSIGN s-tab_std TO <standard_table>.
    ASSIGN s-tab_std TO <table>.
    "ASSIGN s-tab_so TO <standard_table>.
    "ASSIGN s-tab_so TO <table>.
    "ASSIGN s-tab_ha TO <standard_table>.
    "ASSIGN s-tab_ha TO <table>.

    "ASSIGN s-tab_std TO <hashed_table>.
    "ASSIGN s-tab_so TO <hashed_table>.
    ASSIGN s-tab_ha TO <hashed_table>.

    "----- Other types -----
    ASSIGN s-c10 TO <simple>.
    ASSIGN s-str TO <simple>.
    ASSIGN s-dec34 TO <simple>.
    ASSIGN s-date TO <simple>.
    ASSIGN s-structure TO <simple>.
    ASSIGN s-xl1 TO <simple>.
    "ASSIGN s-tab_ha TO <simple>.

    ASSIGN s-oref TO <object>.
    s-oref = NEW zcl_demo_abap_objects( ).
    ASSIGN s-oref TO <object>.
    s-oref = cl_abap_random_int=>create( ).
    ASSIGN s-oref TO <object>.

    out->write( `No output for this section. You can set a break point and check the content of data objects in the debugger.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Checking Field Symbol Assignment` ) ).

    "When working with field symbols, you should make sure that they are
    "assigned. Otherwise, a runtime error occurs.
    "You can make use of a logical expression with IS [NOT] ASSIGNED.
    "The example includes data object declarations. One data object is
    "assigned, the other is not. Consequently, the expression is
    "true for the one and false for the other.

    DATA num_b TYPE i VALUE 123.

    FIELD-SYMBOLS: <fs_i_b>   TYPE i,
                   <fs_str_b> TYPE string.

    ASSIGN num_b TO <fs_i_b>.

    IF <fs_i_b> IS ASSIGNED.
      out->write( `Field symbol <fs_i_b> is assigned.` ).
    ELSE.
      out->write( `Field symbol <fs_i_b is> not assigned.` ).
    ENDIF.

    out->write( |\n| ).

    IF <fs_str_b> IS ASSIGNED.
      out->write( `Field symbol <fs_str_b> is assigned.` ).
    ELSE.
      out->write( `Field symbol <fs_str_b> is not assigned.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Unassigning Data Objects from Field Symbols` ) ).

    "If you use an unassigned field symbol, an exception is raised. Before
    "using it, you can check the assignment with the following logical
    "expression. The statement is true if the field symbol is assigned.
    "Using the statement UNASSIGN, you can explicitly remove the assignment
    "of the field symbol.

    DATA num_c TYPE i VALUE 123.

    FIELD-SYMBOLS: <fs_i_c> TYPE i.

    ASSIGN num_c TO <fs_i_c>.

    IF <fs_i_c> IS ASSIGNED.
      out->write( `1. Field symbol <fs_i_c> is assigned.` ).
    ELSE.
      out->write( `1. Field symbol <fs_i_c> is not assigned.` ).
    ENDIF.

    out->write( |\n| ).

    UNASSIGN <fs_i_c>.

    IF <fs_i_c> IS ASSIGNED.
      out->write( `2. Field symbol <fs_i_c> is assigned.` ).
    ELSE.
      out->write( `2. Field symbol <fs_i_c> is not assigned.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Type Casting with Field Symbols` ) ).

    "The example demonstrates the CASTING addition. Various additions after
    "CASTING are possible.

    TYPES type_d_l9 TYPE c LENGTH 9.

    DATA: dobj_d_l5   TYPE c LENGTH 5,
          dobj_d_l10  TYPE c LENGTH 10 VALUE '1234567890',
          type_name_d TYPE string VALUE 'TYPE_D_L9'.

    FIELD-SYMBOLS: <fs_d1> TYPE data,
                   <fs_d2> TYPE type_d_l9.

    "Casting to a statically, completely specified type
    "CASTING addition without any more additions: Field symbol inherits
    "the data type of the data object. The field symbol must be either
    "completely typed or with one of the generic built-in ABAP types
    "c, n, p, or x. The other field symbol declared in the example
    "cannot be used.
    ASSIGN dobj_d_l10 TO <fs_d2> CASTING.

    out->write( data = <fs_d2> name = `<fs_d2>` ).
    out->write( |\n| ).

    ASSIGN dobj_d_l10 TO <fs_d1> CASTING TYPE type_d_l9.

    out->write( data = <fs_d1> name = `<fs_d1>` ).
    out->write( |\n| ).

    "Casting to a generic type
    ASSIGN dobj_d_l10 TO <fs_d1> CASTING TYPE c.

    out->write( data = <fs_d1> name = `<fs_d1>` ).
    out->write( |\n| ).

    "Casting to a static field type
    ASSIGN dobj_d_l10 TO <fs_d1> CASTING LIKE dobj_d_l5.

    out->write( data = <fs_d1> name = `<fs_d1>` ).
    out->write( |\n| ).

    "Casting to a dynamic field type
    ASSIGN dobj_d_l10 TO <fs_d1> CASTING LIKE <fs_d1>.

    out->write( data = <fs_d1> name = `<fs_d1>` ).
    out->write( |\n| ).

    "Anticipating dynamic specification of data types
    "for the CASTING addition.
    "The type name is specified as a character-like data
    "object within parentheses.
    ASSIGN dobj_d_l10 TO <fs_d1> CASTING TYPE (type_name_d).

    out->write( data = <fs_d1> name = `<fs_d1>` ).
    out->write( |\n| ).

    "Anticipating RTTS
    "A type description object is created which can be
    "specified after the TYPE HANDLE additions.
    DATA(sometype) = CAST cl_abap_datadescr(
      cl_abap_typedescr=>describe_by_name( 'TYPE_D_L9' ) ).
    ASSIGN dobj_d_l10 TO <fs_d1> CASTING TYPE HANDLE sometype.

    out->write( data = <fs_d1> name = `<fs_d1>` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) Addressing Field Symbols` ) ).

    "The example includes multiple data objects that are assigned to field
    "symbols. It is demonstrated that field symbols are addressed in various
    "occasions. Among them: Changing the value of data objects assigned to
    "field symbols, the use of field symbols in expressions, structures, and
    "internal tables.

    DATA: num_e   TYPE i VALUE 456,
          struc_e TYPE zdemo_abap_carr,
          tab_e   TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.

    SELECT SINGLE *
      FROM zdemo_abap_carr
      WHERE carrid = 'LH'
      INTO @struc_e.

    FIELD-SYMBOLS: <fs_i_e>      TYPE i,
                   <fs_struc_e>  TYPE zdemo_abap_carr,
                   <fs_tab_e>    LIKE tab_e,
                   <fs_anytab_e> TYPE ANY TABLE.

    "Without an assignment, this would result in a runtime error:
    "<fs_i_e> = 1.

    ASSIGN num_e   TO <fs_i_e>.
    ASSIGN struc_e TO <fs_struc_e>.
    ASSIGN tab_e   TO <fs_tab_e>.
    ASSIGN tab_e   TO <fs_anytab_e>.

    "Changing values
    <fs_i_e> = 789.

    out->write( data = <fs_i_e> name = `<fs_i_e>` ).
    out->write( |\n| ).
    out->write( data = num_e name = `num_e` ).
    out->write( |\n| ).

    "Use in expressions
    DATA(calc_e) = <fs_i_e> + 211.

    out->write( data = calc_e name = `calc_e` ).
    out->write( |\n| ).

    IF <fs_i_e> < 1000.
      out->write( `The value of <fs_i_e> is less than 1000` ).
    ELSE.
      out->write( `The value of <fs_i_e> is greater than 1000` ).
    ENDIF.

    out->write( |\n| ).
    out->write( |\n| ).

    "Structure
    out->write( data = <fs_struc_e> name = `<fs_struc_e>` ).
    out->write( |\n| ).

    DATA(comp_e1) = <fs_struc_e>-carrid.

    out->write( data = comp_e1 name = `comp_e1` ).
    out->write( |\n| ).

    <fs_struc_e>-url = 'www.lh.com'.

    out->write( data = <fs_struc_e>-url name = `<fs_struc_e>-url` ).
    out->write( |\n| ).

    "Internal table
    SELECT *
      FROM zdemo_abap_carr
      ORDER BY carrid
      INTO TABLE @<fs_tab_e>
      UP TO 3 ROWS.

    out->write( data = <fs_tab_e> name = `<fs_tab_e>` ).
    out->write( |\n| ).

    TRY.
        DATA(comp_e2) = <fs_tab_e>[ 2 ]-carrname.
        out->write( data = comp_e2 name = `comp_e2` ).
      CATCH cx_sy_itab_line_not_found INTO DATA(error_e).
    ENDTRY.

    out->write( |\n| ).

    SELECT *
      FROM zdemo_abap_carr
      ORDER BY carrid
      INTO TABLE @<fs_anytab_e>
      UP TO 3 ROWS.

    out->write( data = <fs_anytab_e> name = `<fs_anytab_e>` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) Using Field Symbols when Processing Internal Tables` ) ).

    "By using field symbols in the context of loops across internal tables,
    "you can avoid an actual copying of content to a work area during
    "the loop.
    "The example includes multiple loops. First, internal tables are
    "declared. One of them is filled. Then, field symbols are declared to
    "which data objects are assigned. In the first loop, a previously
    "declared field symbol is used as target area to hold the table line
    "that is processed. In the course of the loop, some values are changed. The
    "components are accessed using the component selector '-'. At the end of
    "the loop, another internal table is filled using the currently
    "processed line for the second loop. The second loop (the loop is
    "carried out based on
    "an internal table a field symbol points to) uses a directly declared
    "field symbol using ASSIGNING FIELD-SYMBOL(<...>). Also here, some
    "values are changed and another internal table is filled. This table
    "is of a generic type. At the end, this internal table is output, too.

    DATA: tab_f1 TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY,
          tab_f2 LIKE tab_f1,
          tab_f3 LIKE tab_f1.

    SELECT *
      FROM zdemo_abap_fli
      ORDER BY carrid
      INTO TABLE @tab_f1
      UP TO 3 ROWS.

    FIELD-SYMBOLS: <fs_struc_f>  LIKE LINE OF tab_f1,
                   <fs_tab_f>    LIKE tab_f1,
                   <fs_anytab_f> TYPE ANY TABLE.

    ASSIGN tab_f2 TO <fs_tab_f>.
    ASSIGN tab_f3 TO <fs_anytab_f>.

    LOOP AT tab_f1 ASSIGNING <fs_struc_f>.
      <fs_struc_f>-connid = '99'.
      <fs_struc_f>-fldate = cl_abap_context_info=>get_system_date( ).
      <fs_struc_f>-price = <fs_struc_f>-price + 100.
      <fs_struc_f>-currency = 'EUR'.
      CLEAR: <fs_struc_f>-paymentsum,
             <fs_struc_f>-seatsocc,
             <fs_struc_f>-seatsocc_b,
             <fs_struc_f>-seatsocc_f.

      "Filling another itab
      <fs_tab_f> = VALUE #( BASE <fs_tab_f> ( <fs_struc_f> ) ).
    ENDLOOP.

    out->write( data = tab_f1 name = `tab_f1` ).
    out->write( |\n| ).

    "The following example shows a field symbol declared inline.
    LOOP AT <fs_tab_f> ASSIGNING FIELD-SYMBOL(<fs_struc_f2>).
      <fs_struc_f2>-connid = '100'.
      <fs_struc_f2>-fldate = cl_abap_context_info=>get_system_date( ) + 1.
      <fs_struc_f2>-price = <fs_struc_f2>-price - 50.
      <fs_struc_f2>-currency = 'USD'.

      "Filling another itab
      <fs_anytab_f> = VALUE #( BASE <fs_anytab_f> ( <fs_struc_f2> ) ).
    ENDLOOP.

    out->write( data = <fs_tab_f> name = `<fs_tab_f>` ).
    out->write( |\n| ).
    out->write( data = <fs_anytab_f> name = `<fs_anytab_f>` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Structure Assigned to Field Symbol: Accessing Structure Components` ) ).

    "In this example, all components of a structure are processed using
    "field symbols and an ASSIGN COMPONENT ... OF STRUCTURE ... statement.
    "First, a field symbol is declared with a generic type. A structure is
    "filled with values from a demo table. The structure is assigned to the
    "field symbol. Using a DO loop, all components are processed. The
    "sy-index value represents the position of the component in the
    "structure. Once all components have been processed (i. e. if sy-subrc
    "does not return '0' for a sy-index value), the loop is exited. The output
    "shows all components and their values.
    "See more examples for accessing structure components below.

    FIELD-SYMBOLS <comp> TYPE data.

    DATA comp_tab TYPE string_table.

    SELECT SINGLE carrid, carrname, currcode, url
      FROM zdemo_abap_carr
      WHERE carrid = 'LH'
      INTO @DATA(struct).

    FIELD-SYMBOLS <struct> TYPE data.

    ASSIGN struct TO <struct>.

    DO.
      "sy-index represents the position of a structure component
      ASSIGN <struct>-(sy-index) TO <comp>.

      "Old syntax
      "ASSIGN COMPONENT sy-index OF STRUCTURE <struct> TO <comp>.

      IF sy-subrc <> 0.
        "If all components are processed, the loop is exited.
        EXIT.
      ELSE.
        out->write( |sy-index: { sy-index }, component content:| ).
        out->write( <comp> ).
        out->write( |\n| ).
      ENDIF.

    ENDDO.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Data References` ) ).
    out->write( |9) Declaring Data References\n\n| ).

    "Like field symbols, data reference variables can be declared with both
    "a complete and a generic data type using DATA statements and the
    "addition REF TO. The type after REF TO represents the static data type.
    "The example shows multiple ways of declaring a data reference variable
    "using both complete and generic data types.

    DATA: some_string TYPE string.

    TYPES: ref_type TYPE REF TO zdemo_abap_flsch.

    DATA: ref_a1 TYPE REF TO i, "Complete data type
          ref_a2 TYPE REF TO zdemo_abap_carr, "Complete data type
          ref_a3 LIKE REF TO some_string,
          ref_a4 LIKE ref_a1,
          ref_a5 TYPE ref_type,
          ref_a6 TYPE REF TO data. "Generic data type

    out->write( `No output for this section. You can set a break point and check the content of data objects in the debugger.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) Creating Data References ` &&
    `to Existing Data Objects` ) ).

    "The example includes data reference variables with both complete and
    "generic type. When using the REF operator, the '#' sign means that the
    "type is derived from the data object. You can also explicitly specify
    "the data type after REF before the parenthesis. Within the parentheses,
    "you can provide a value.

    "Declaring data object
    DATA number_b TYPE i VALUE 5.

    "Declaring data reference variables
    DATA ref_b1     TYPE REF TO i.
    DATA ref_data_b TYPE REF TO data.

    "Creating data references to data objects.
    "The '#' sign means that the type is derived from the data object.
    ref_b1     = REF #( number_b ).
    ref_data_b = REF #( number_b ).

    "You can also use inline declarations to omit the explicit declaration.
    DATA(ref_b2) = REF #( number_b ).

    "You can explicitly specify the data type after REF.
    "DATA(ref_b3) = REF #( g ).

    out->write( `No output for this section. You can set a break point and check the content of data objects in the debugger.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) Dynamically Creating Data Objects at Runtime Using Static Type Definitions` ) ).

    "The example code shows the creation of anonymous data objects. They
    "can be created using the statement CREATE DATA, the instance operator
    "NEW, or the addition NEW of the INTO clause in a SELECT statement.
    "A data reference variable is expected when anonymous objects are
    "declared. They cannot be addressed by a name (hence anonymous).
    "Note:
    "- The examples cover static type definitions. As shown further down,
    "  there are options to dynamically specify the type definitions.
    "- To output the content of the data reference variables, they
    "  must be dereferenced first. The details are shown further down.

    "CREATE DATA statements
    "Note that there are many additions available. The examples show a selection.
    "Behind TYPE and LIKE, the syntax offers the same possibilities as the DATA statement.

    "Creating an anonymous data object with an implicit type.
    "If neither of the additions TYPE or LIKE are specified, the data reference variable
    "must be completely typed.
    DATA dref_c1 TYPE REF TO string.
    CREATE DATA dref_c1.

    "Creating anonymous data objects with explicit data type specification.
    "Data reference variable with a generic type to be used in the following examples
    "for the anonymous data object.
    DATA dref_c2 TYPE REF TO data.

    "Elementary, built-in ABAP type
    CREATE DATA dref_c2 TYPE p LENGTH 8 DECIMALS 3.

    "Anomyous internal table ...
    "using the LIKE addition to refer to an existing internal table
    DATA itab_c TYPE TABLE OF zdemo_abap_carr.
    CREATE DATA dref_c2 LIKE itab_c.

    "by specifying the entire table type
    CREATE DATA dref_c2 TYPE HASHED TABLE OF zdemo_abap_carr WITH UNIQUE KEY carrid.

    "Anonymous structures
    CREATE DATA dref_c2 LIKE LINE OF itab_c.
    CREATE DATA dref_c2 TYPE zdemo_abap_carr.

    "Creating reference variable
    TYPES elem_type_c TYPE c LENGTH 3.
    CREATE DATA dref_c2 TYPE REF TO elem_type_c.

    "NEW operator
    "- Works like CREATE DATA dref TYPE type statements and can be used in general
    "  expression positions.
    "- Allows to assign values to the new anonymous data objects in parentheses

    "Creating data reference variables
    DATA: dref_c3 TYPE REF TO i,
          dref_c4 TYPE REF TO data.

    "# character after NEW if the data type can be identified completely
    "instead of the explicit type specification (only non-generic types)
    dref_c3 = NEW #( 123 ).
    dref_c3 = NEW i( 456 ).
    dref_c4 = NEW zdemo_abap_carr( ). "not assigning any values
    dref_c4 = NEW string( `hi` ).

    "Creating anonymous data objects inline
    "In doing so, you can omit a prior declaration of a variable.
    DATA(dref_c5) = NEW i( 789 ).
    DATA(dref_c6) = NEW zdemo_abap_carr( carrid = 'AB' carrname = 'AB Airlines' ).

    "ABAP SQL SELECT statements
    "Using the NEW addition in the INTO clause, an anonymous data object
    "can be created in place.
    SELECT *
      FROM zdemo_abap_carr
      INTO TABLE NEW @DATA(dref_c7)   "Internal table
      UP TO 3 ROWS.

    SELECT SINGLE *
      FROM zdemo_abap_carr
      WHERE carrid = 'LH'
      INTO NEW @DATA(dref_c8). "Structure

    out->write( data = dref_c6->* name = `dref_c6->*` ).
    out->write( |\n| ).
    out->write( data = dref_c7->* name = `dref_c7->*` ).
    out->write( |\n| ).
    out->write( data = dref_c8->* name = `dref_c8->*` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `12) Data References and Assignments` ) ).

    "Regarding the assignment, note that static types of both data
    "reference variables must be compatible. As a result of an assignment,
    "both the target reference variable and the source reference variable
    "point to the same data object.
    "Upcast/downcasts: For an assignment to work, the basic rule applies:
    "The static type of the target reference variable must be more general
    "than or the same as the dynamic type of the source reference variable.
    "In the example below:
    "Upcast: The target data reference variable is of generic type, the
    "source variable is of complete type. The assignment is done with the
    "assignment operator '='.
    "Downcast: The target data reference variable is of complete type, the
    "source variable is of generic type. The assignment is done with casting
    "operators, either with the constructor operator CAST or the older ?=.

    "Declaring data reference variables
    DATA ref_d1 TYPE REF TO i.
    DATA ref_d2 TYPE REF TO i.

    ref_d1 = NEW #( 789 ).

    "Assigning data reference
    ref_d2 = ref_d1.

    "Casting
    "Complete type
    DATA(ref_d3) = NEW i( 321 ).

    "Generic type
    DATA ref_data_d1 TYPE REF TO data.

    "Upcast
    ref_data_d1 = ref_d3.

    "Downcasts
    DATA ref_d5 TYPE REF TO i.

    "Generic type
    DATA ref_data_d2 TYPE REF TO data.

    ref_data_d2 = NEW i( 654 ).

    ref_d5 = CAST #( ref_data_d2 ).

    ref_d5 ?= ref_data_d2.

    out->write( data = ref_d2->* name = `ref_d2->*` ).
    out->write( |\n| ).
    out->write( data = ref_data_d1->* name = `ref_data_d1->*` ).
    out->write( |\n| ).
    out->write( data = ref_d5->* name = `ref_d5->*` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `13) Addressing Data References ` ) ).

    "Before addressing the content of data objects a data reference points
    "to, you must dereference data reference variables. Use the
    "dereferencing operator ->*.
    "The example includes multiple occasions in which data reference are
    "addressed: Changing the content of a referenced data object, the use in
    "logical expressions, structures, and internal tables.

    "Creating data reference variables and assigning values
    DATA(ref_e1) = NEW i( 1 ).
    DATA(ref_e2) = NEW zdemo_abap_carr( carrid   = 'LH'
                                        carrname = 'Lufthansa' ).

    "Generic type
    DATA ref_data_e TYPE REF TO data.

    "Copying reference
    ref_data_e = ref_e1.

    "Addressing
    "Variable receives the content.
    DATA(some_num) = ref_e1->*.

    out->write( data = ref_e1->* name = `ref_e1->*` ).
    out->write( |\n| ).

    "Content of referenced data object is changed
    ref_e1->* = 10.

    out->write( data = ref_e1->* name = `ref_e1->*` ).
    out->write( |\n| ).

    "Data reference used in a logical expression
    IF ref_e1->* > 5.
      out->write( `The value of ref_e1 is greater than 5.` ).
    ELSE.
      out->write( `The value of ref_e1 is lower than 5.` ).
    ENDIF.

    out->write( |\n| ).

    "Dereferenced generic type
    DATA(calc) = 1 + ref_data_e->*.

    out->write( data = calc name = `calc` ).
    out->write( |\n| ).

    "Complete structure
    DATA(struc) = ref_e2->*.

    out->write( data = ref_e2->* name = `ref_e2->*` ).
    out->write( |\n| ).

    "Individual structure component
    "When dereferencing a data reference variable that has a structured
    "data type, you can use the component selector -> to address individual components.
    DATA(carrid) = ref_e2->carrid.

    ref_e2->carrid = 'UA'.

    out->write( data = ref_e2->carrid name = `ref_e2->carrid` ).
    out->write( |\n| ).

    "The following syntax also works (dereferencing operator and the component selector).
    ref_e2->*-carrname = 'United Airlines'.

    out->write( data = ref_e2->*-carrname name = `ref_e2->*-carrname` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `14) Checking if Data Reference ` &&
   `Can Be Dereferenced` ) ).

    "You can check if a data reference can be dereferenced by using
    "a logical expression with IS [NOT] BOUND.
    "The example shows both a data reference that is bound and not bound.

    DATA(ref_f1) = NEW string( `hello` ).
    DATA ref_f2 TYPE REF TO i.

    out->write( `IF statement:` ).
    IF ref_f1 IS BOUND.
      out->write( `ref_f1 is bound.` ).
    ELSE.
      out->write( `ref_f1 is not bound.` ).
    ENDIF.

    out->write( |\n| ).
    out->write( `COND operator:` ).
    DATA(is_bound) = COND #( WHEN ref_f1 IS BOUND THEN `ref_f1 is bound.` ELSE `ref_f1 is not bound.` ).
    out->write( is_bound ).
    out->write( |\n| ).

    out->write( `IF statement:` ).
    IF ref_f2 IS BOUND.
      out->write( `ref_f2 is bound.` ).
    ELSE.
      out->write( `ref_f2 is not bound.` ).
    ENDIF.

    out->write( |\n| ).
    out->write( `COND operator:` ).
    is_bound = COND #( WHEN ref_f2 IS BOUND THEN `ref_f2 is bound.` ELSE `ref_f2 is not bound.` ).
    out->write( is_bound ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `15) Explicitly Removing a Reference` ) ).

    "Note that the garbage collector takes care of removing the references
    "automatically once the data is not used any more by a reference.

    DATA(ref_g1) = NEW string( `hello` ).

    IF ref_g1 IS INITIAL.
      out->write( `Before CLEAR: ref_g1 is initial.` ).
    ELSE.
      out->write( `Before CLEAR: ref_g1 is not intial.` ).
    ENDIF.

    out->write( |\n| ).

    IF ref_g1 IS BOUND.
      out->write( `Before CLEAR: ref_g1 is bound.` ).
    ELSE.
      out->write( `Before CLEAR: ref_g1 is not bound.` ).
    ENDIF.

    out->write( |\n| ).

    CLEAR ref_g1.

    IF ref_g1 IS INITIAL.
      out->write( `After CLEAR: ref_g1 is initial.` ).
    ELSE.
      out->write( `After CLEAR: ref_g1 is not initial.` ).
    ENDIF.

    out->write( |\n| ).

    IF ref_g1 IS BOUND.
      out->write( `After CLEAR: ref_g1 is bound.` ).
    ELSE.
      out->write( `After CLEAR: ref_g1 is not bound.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `16) Overwriting Data Reference Variables` ) ).

    "A data reference variable is overwritten if a new object is created
    "with a data reference variable already pointing to a data object

    DATA ref_h1 TYPE REF TO data.

    ref_h1 = NEW i( 111 ).

    out->write( data = ref_h1->* name = `ref_h1->*` ).
    out->write( |\n| ).

    ref_h1 = NEW string( `ref_h1 overwritten.` ).

    out->write( data = ref_h1->* name = `ref_h1->*` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `17) Retaining Data References` ) ).

    "Storing data reference variables in an internal table using
    "TYPE TABLE OF REF TO prevents the overwriting.
    "The example demonstrates that three data references are created with
    "the same reference variable in the course of a DO loop. There, the data
    "reference is overwritten. However, due to saving the data reference
    "variable in an internal table, the content is preserved.

    DATA: ref_data_i TYPE REF TO data,
          itab_i     TYPE TABLE OF REF TO data,
          number_i   TYPE i VALUE 0.

    DO 3 TIMES.
      "Adding up 1 to demonstrate a changed data object
      number_i += 1.

      "Creating a data reference and assigning value
      "In the course of the loop, the variable is overwritten.
      ref_data_i = NEW i( number_i ).

      "Adding the reference to an internal table
      itab_i     = VALUE #( BASE itab_i ( ref_data_i ) ).
    ENDDO.

    out->write( data = itab_i name = `itab_i` ).
    out->write( |\n| ).
    out->write( data = `The derefenced value of the data reference - which ` &&
    `was changed in the course of the loop - in the second table ` &&
    `entry is ` && itab_i[ 2 ]->* && `.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `18) Processing Internal Tables Using ` &&
    `Data References ` ) ).

    "Similar use case to using field symbols: In a loop across an internal table,
    "you can store the content of the line in a data reference variable
    "instead of actually copying the content to boost performance.
    "In the example, an internal table is created including values of
    "database table. A data reference variable is declared as target area of
    "the loop. In the course of the loop, some values are changed.

    SELECT *
      FROM zdemo_abap_flsch
      WHERE distid = 'MI'
      ORDER BY carrid
      INTO TABLE @DATA(flsch_tab)
      UP TO 3 ROWS.

    "The table line is written into a data reference variable
    "that is declared inline.
    LOOP AT flsch_tab REFERENCE INTO DATA(ref_k).
      ref_k->connid = '123'.
      ref_k->distance =  ref_k->distance * '1.609344'.
      ref_k->distid =  'KM' .
    ENDLOOP.

    out->write( data = flsch_tab name = `flsch_tab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `19) Data References as Part of ` &&
   `Structures and Internal Tables` ) ).

    "In contrast to field symbols, data reference variables can be used as
    "components of structures or columns in internal tables.
    "The example includes the declaration of a structure that contains a
    "data reference variable.
    "The structure is filled. Based on this structure, an
    "internal table is created. In a DO loop, the internal table is filled.

    "Declaring a structure
    DATA: BEGIN OF struc_l,
            number_l TYPE i,
            ref_l    TYPE REF TO i,
          END OF struc_l,
          some_int TYPE i VALUE 0.

    "Filling structure
    struc_l = VALUE #( number_l = 1 ref_l = NEW #( 2 ) ).

    out->write( data = struc_l ).
    out->write( |\n| ).

    "Declaring an internal table
    DATA itab_l LIKE TABLE OF struc_l WITH EMPTY KEY.

    "Filling internal table.
    DO 3 TIMES.
      some_int += 1.

      "Filling structure
      struc_l = VALUE #( number_l = some_int
                         ref_l    = NEW #( some_int ) ).

      "Filling internal table
      itab_l  = VALUE #( BASE itab_l ( struc_l ) ).
    ENDDO.

    out->write( data = itab_l name = `itab_l` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `20) Excursion: Generic Data References` ) ).

    "In modern ABAP, variables and field symbols of the generic types
    "'any' and 'data' can be used directly, for example, in LOOP and READ statements.
    DATA dref_gen TYPE REF TO data.
    CREATE DATA dref_gen TYPE TABLE OF zdemo_abap_carr.
    SELECT *
      FROM zdemo_abap_carr
      INTO TABLE @dref_gen->*.

    "Note: In case of a fully generic type, an explicit or implicit index operation
    "is not possible (indicated by the examples commented out).
    LOOP AT dref_gen->* ASSIGNING FIELD-SYMBOL(<loop>).
      ...
    ENDLOOP.
    "LOOP AT dref->* ASSIGNING FIELD-SYMBOL(<loop2>) FROM 1 TO 4.
    "ENDLOOP.

    "The following examples use a dynamic key specification.
    "See more syntax examples below.
    READ TABLE dref_gen->* ASSIGNING FIELD-SYMBOL(<read>) WITH KEY ('CARRID') = 'AA'.
    "READ TABLE dref->* INDEX 1 ASSIGNING FIELD-SYMBOL(<read2>).
    out->write( data = <read> name = `<read>` ).
    out->write( |\n| ).

    "Table expressions
    DATA(line) = CONV zdemo_abap_carr( dref_gen->*[ ('CARRID') = 'AA' ] ).

    out->write( data = line name = `line` ).
    out->write( |\n| ).

    dref_gen->*[ ('CARRID') = 'AA' ] = VALUE zdemo_abap_carr( BASE dref_gen->*[ ('CARRID') = 'AA' ] carrid = 'XY' ).

    out->write( data = dref_gen->*[ ('CARRID') = 'XY' ] name = `dref_gen->*[ ('CARRID') = 'XY' ]` ).
    out->write( |\n| ).

    dref_gen->*[ ('CARRID') = 'XY' ]-('CARRID') = 'AA'.

    out->write( data = dref_gen->*[ ('CARRID') = 'AA' ]-('CARRNAME') name = `dref_gen->*[ ('CARRID') = 'AA' ]-('CARRNAME')` ).
    out->write( |\n| ).

    "Table functions
    DATA(num_tab_lines) = lines( dref_gen->* ).

    out->write( data = num_tab_lines name = `num_tab_lines` ).
    out->write( |\n| ).

    DATA(idx) = line_index( dref_gen->*[ ('CARRID') = 'LH' ] ).

    out->write( data = idx name = `idx` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Dynamic ABAP Statements` ) ).
    out->write( |21) Dynamic ASSIGN Statements (1) -  Specifying the Memory Area Dynamically\n\n| ).
    "The memory area is not specified directly, but as content of a
    "character-like data object in parentheses.
    "Note:
    "- When specified as unnamed data object, the compiler treats the
    "  specifications like static assignments. Do not use named data objects
    "  for ASSIGN statements in ABAP for Cloud Development. It is recommended
    "  that existing named data objects are put in a structure. Then, the syntax
    "  for assigning components dynamically can be used so as to avoid a syntax
    "  warning.
    "- Most of the following examples use an unnamed data object.
    "- The specification of the name is not case-sensitive.

    "Creating and populating various types/data objects to work with
    TYPES: BEGIN OF st_type,
             col1 TYPE i,
             col2 TYPE string,
             col3 TYPE string,
           END OF st_type.
    DATA structure TYPE st_type.
    DATA it TYPE TABLE OF st_type WITH EMPTY KEY.
    structure = VALUE #( col1 = 1 col2 = `aaa` col3 = `Z` ).
    APPEND structure TO it.
    DATA(struc_ref) = NEW st_type( col1 = 2 col2 = `b` col3 = `Y` ).
    DATA dobj TYPE string VALUE `hallo`.
    "The following examples use a field symbol with generic type
    FIELD-SYMBOLS <fs> TYPE data.

    ASSIGN ('IT') TO <fs>.
    ASSIGN ('SRUCTURE') TO <fs>.

    "Field symbol declared inline
    "Note: The typing is performed with the generic type data.
    ASSIGN ('DOBJ') TO FIELD-SYMBOL(<fs_inline>).

    "The statements set the sy-subrc value.
    ASSIGN ('DOES_NOT_EXIST') TO <fs>.
    IF sy-subrc <> 0.
      out->write( `Dynamic assignment not successful` ).
    ENDIF.

    "The memory area can also be a dereferenced data reference
    ASSIGN struc_ref->* TO <fs>.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `22) Dynamic ASSIGN Statements (2) - Assigning Components Dynamically` ) ).

    "You can chain the names with the component selector (-), or, in
    "case of reference variables, the object component selector (->).
    ASSIGN structure-('COL1') TO <fs>.
    ASSIGN it[ 1 ]-('COL1') TO <fs>.
    ASSIGN struc_ref->('COL1') TO <fs>.
    "The following example uses the dereferencing operator explicitly
    "followed by the component selector.
    ASSIGN struc_ref->*-('COL1') TO <fs>.

    "Using a named data object for the component specification
    DATA columnname TYPE string VALUE `COL1`.
    ASSIGN structure-(columnname) TO <fs>.

    "Fully dynamic specification
    "If the compiler can fully determine the data object in ASSIGN structureatements
    "in ABAP for Cloud Development, a warning is not issued.
    ASSIGN ('STRUCTURE-COL1') TO <fs>.

    "Numeric expressions are possible. Its value is interpreted
    "as the position of the component in the structureructure.
    ASSIGN structure-(3) TO <fs>.

    "If the value is 0, the memory area of the entire structureructure is
    "assigned to the field symbol.
    ASSIGN structure-(0) TO <fs>.

    "The structureatements above replace the following, older structureatements.
    ASSIGN COMPONENT 'COL1' OF STRUCTURE structure TO <fs>.
    ASSIGN COMPONENT 3 OF STRUCTURE structure TO <fs>.

    "More examples
    SELECT SINGLE * FROM zdemo_abap_carr INTO @DATA(wa).
    "Reading into data reference variable
    SELECT SINGLE * FROM zdemo_abap_carr INTO NEW @DATA(ref_m).

    DATA(comp_name) = lcl_det_at_runtime=>get_dyn_field( ).

    ASSIGN wa-(comp_name) TO FIELD-SYMBOL(<fs_m6>).

    ASSIGN wa-('CARRNAME') TO FIELD-SYMBOL(<fs_m7>).

    IF sy-subrc = 0.
      DATA(subrc1) = sy-subrc.
    ENDIF.

    "No exception occurs in case of an unsuccessful assignment.
    ASSIGN wa-('CRRNM') TO FIELD-SYMBOL(<fs_m8>).

    IF sy-subrc <> 0.
      DATA(subrc2) = sy-subrc.
    ENDIF.

    "Numeric expressions are possible. Its value is interpreted as the position
    "of the component in the structure.
    ASSIGN wa-(4) TO FIELD-SYMBOL(<fs_m9>).

    "If the value is 0, the memory area of the entire structure is assigned to the field symbol.
    ASSIGN wa-(0) TO FIELD-SYMBOL(<fs_m10>).

    "Old syntax
    ASSIGN COMPONENT 'CARRID' OF STRUCTURE wa TO FIELD-SYMBOL(<fs_m11>).
    ASSIGN COMPONENT 5 OF STRUCTURE wa TO FIELD-SYMBOL(<fs_m12>).

    "Dynamically specifying components of structures that are referenced by
    "a data reference variable

    ASSIGN ref_m->('CARRNAME') TO FIELD-SYMBOL(<fs_m13>).

    out->write( data = <fs_m6> name = `<fs_m6>` ).
    out->write( |\n| ).
    out->write( data = <fs_m7> name = `<fs_m7>` ).
    out->write( |\n| ).
    out->write( data = subrc1 name = `subrc1` ).
    out->write( |\n| ).
    out->write( data = subrc2 name = `subrc2` ).
    out->write( |\n| ).
    out->write( data = <fs_m9> name = `<fs_m9>` ).
    out->write( |\n| ).
    out->write( data = <fs_m10> name = `<fs_m10>` ).
    out->write( |\n| ).
    out->write( data = <fs_m11> name = `<fs_m11>` ).
    out->write( |\n| ).
    out->write( data = <fs_m12> name = `<fs_m12>` ).
    out->write( |\n| ).
    out->write( data = <fs_m13> name = `<fs_m13>` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `23) Dynamic ASSIGN Statements (3) - Assigning Attributes of Classes or Interfaces Dynamically (1)` ) ).
    "The following syntax pattern shows the possible specifications.
    "... cref->(attr_name) ...  "object reference variable
    "... iref->(attr_name) ...  "interface reference variable
    "... (clif_name)=>(attr_name) ...  "class/interface name
    "... (clif_name)=>attr ...
    "... clif=>(attr_name) ...

    "Creating an instance of a class
    DATA(objref) = NEW zcl_demo_abap_objects( ).

    "Assigning instance attributes using an object reference variable
    "All visible attributes of objects can be assigned.
    objref->string = `ABAP`. "Assigning a value to the attribute for demo purposes
    ASSIGN objref->('STRING') TO <fs>.

    "Assigning instance attributes using an interface reference variable
    DATA intref TYPE REF TO zdemo_abap_objects_interface.
    intref = objref.
    ASSIGN intref->('STRING') TO <fs>.
    intref->in_str = `hallo`.
    ASSIGN intref->('IN_STR') TO <fs>.

    "Assigning static attributes
    "All visible static attributes in classes and interfaces can be assigned
    "In the following example, a class and an interface are specified statically,
    "and the attributes are specified dynamically.
    ASSIGN zcl_demo_abap_objects=>('PUBLIC_STRING') TO <fs>.
    ASSIGN zdemo_abap_objects_interface=>('CONST_INTF') TO <fs>.

    "Specifying a class or interface dynamically, and attributes statically
    ASSIGN ('ZCL_DEMO_ABAP_OBJECTS')=>public_string TO <fs>.
    ASSIGN ('ZDEMO_ABAP_OBJECTS_INTERFACE')=>const_intf TO <fs>.

    "Specifying a class or interface as well as attributes dynamically
    ASSIGN ('ZCL_DEMO_ABAP_OBJECTS')=>('PUBLIC_STRING') TO <fs>.
    ASSIGN ('ZDEMO_ABAP_OBJECTS_INTERFACE')=>('CONST_INTF') TO <fs>.

    "Further dynamic syntax options are possible, for example,
    "specifying the memory area after ASSIGN with a writable expression
    "because the operand position after ASSIGN is a result position.
    ASSIGN NEW zcl_demo_abap_objects( )->('PUBLIC_STRING') TO <fs>.

    out->write( `No output for this section. You can set a break point and check the content of data objects in the debugger.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `24) Dynamic ASSIGN Statements (4) - Assigning Attributes of Classes or Interfaces Dynamically (2)` ) ).

    "The following examples demonstrate a selection of various dynamic specifications
    "that are possible with ASSIGN statements.

    "Dynamic specification of attributes of classes/interfaces that are assigned to a field symbol.
    DATA(dobj_name) = lcl_det_at_runtime=>get_dyn_dobj( ).

    ASSIGN lcl_det_at_runtime=>(dobj_name) TO FIELD-SYMBOL(<fs_m1>).

    out->write( |Data object name determined at runtime: { dobj_name } | ).
    out->write( |\n| ).
    out->write( |The content of the data object is: { <fs_m1> } | ).
    out->write( |\n| ).
    out->write( |\n| ).

    dobj_name = lcl_det_at_runtime=>get_dyn_dobj( ).

    "Completely dynamic assign
    ASSIGN ('lcl_det_at_runtime')=>(dobj_name) TO FIELD-SYMBOL(<fs_m2>).
    out->write( data = <fs_m2> name = `<fs_m2>` ).
    out->write( |\n| ).

    ASSIGN ('zdemo_abap_objects_interface')=>('const_intf') TO FIELD-SYMBOL(<fs_m3>).
    out->write( data = <fs_m3> name = `<fs_m3>` ).
    out->write( |\n| ).

    "Class/interface reference variables pointing to an object that contains attributes
    "and that are specified dynamically.
    DATA iref TYPE REF TO zdemo_abap_objects_interface.
    DATA(cl_ref) = NEW zcl_demo_abap_objects( ).
    iref = cl_ref.

    ASSIGN iref->('const_intf') TO FIELD-SYMBOL(<fs_m4>).

    out->write( data = <fs_m4> name = `<fs_m4>` ).
    out->write( |\n| ).

    ASSIGN cl_ref->('another_string') TO FIELD-SYMBOL(<fs_m5>).

    out->write( data = <fs_m5> name = `<fs_m5>` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `25) Dynamic ASSIGN Statements (5) - Setting sy-subrc/ELSE UNASSIGN Addition` ) ).

    "In dynamic assignments, the statement ASSIGN sets the return code sy-subrc.
    "If ELSE UNASSIGN is specified, no memory area is assigned to the field symbol. It has the state unassigned after the ASSIGN statement.

    DATA(attr) = VALUE string_table( ( `another_string` ) ( `public_string` ) ( `this_will_fail` ) ).

    LOOP AT attr INTO DATA(attribute).

      ASSIGN cl_ref->(attribute) TO FIELD-SYMBOL(<attr>) ELSE UNASSIGN.
      IF sy-subrc = 0.
        out->write( |Successful assignment for attribute "{ attribute }". sy-subrc = { sy-subrc }. | ).
        out->write( |\n| ).
        out->write( data = <attr> name = `<attr>` ).
      ELSE.
        out->write( |Assignment not successful for attribute "{ attribute }". sy-subrc = { sy-subrc }. | ).
      ENDIF.

      out->write( |\n| ).

      IF <attr> IS ASSIGNED.
        out->write( `The field symbol is assigned.` ).
        out->write( `--------------------` ).
      ELSE.
        out->write( `The field symbol is not assigned.` ).
      ENDIF.

      out->write( |\n| ).

    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `26) Dynamic ASSIGN Statements (6) - Type Casting` ) ).

    "As covered above, the CASTING addition of the ASSIGN statement
    "has dynamic syntax elements.
    DATA dobj_c_l5 TYPE c LENGTH 5 VALUE 'abcde'.
    TYPES dtype_c_l2 TYPE c LENGTH 2.
    FIELD-SYMBOLS <fs_dyn_as> TYPE data.

    "A text literal with the name of a type is specified within the parentheses.
    ASSIGN dobj_c_l5 TO <fs_dyn_as> CASTING TYPE ('DTYPE_C_L2').

    out->write( data = <fs_dyn_as> name = `<fs_dyn_as1>` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `27) Accessing Structure Components Dynamically (1)` ) ).

    "You can achieve the access using ASSIGN statements as shown above, or
    "by statically specifying the structure and the (object) component selector
    "followed by a character-like data object in parentheses.
    "Write position
    structure-('COL1') = 123.
    it[ 1 ]-('COL1') = 456.
    struc_ref->('COL1') = 789.

    "Read position
    "The example shows how you can retrieve the textual content of any component
    "of any structure.
    DATA(content_col2) = CONV string( structure-('COL1') ).
    DATA(content_col3) = |{ structure-('COL3') }|.
    DATA content_col1 LIKE structure-col1.
    content_col1 = structure-('COL1').

    DATA dref_comp TYPE REF TO data.
    CREATE DATA dref_comp LIKE structure-('COL3').
    dref_comp->* = structure-('COL3').

    "If the component is not found, a catchable exception is raised.
    TRY.
        DATA(col_not_existent) = |{ structure-('COL123') }|.
      CATCH cx_sy_assign_illegal_component INTO DATA(error_column).
        out->write( error_column->get_text( ) ).
    ENDTRY.

    "Accessing components of generic structures dynamically,
    "e.g. if you have a method parameter that is typed with the generic type
    "data.
    "The example uses a field symbol with the generic type data which is assigned
    "a structure.
    FIELD-SYMBOLS <gen> TYPE data.
    ASSIGN structure TO <gen>.

    "As in the examples above, specifying components dynamically is possible.
    <gen>-('COL2') = `ABAP`.
    DATA(gen_comp) = CONV string( <gen>-('COL2') ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `28) Accessing Structure Components Dynamically (2) - Excursion` ) ).

    "In the following example, a structure is assigned to a field symbol that
    "has a generic type. The components of the structure are accessed dynamically in
    "a DO loop. The sy-index value is interpreted as the position of the component
    "in the structure. Plus, using RTTI - as also shown further down - the component
    "names are retrieved. Component names and the values are added to a string. As a
    "prerequisite, all component values must be convertible to type string.
    DATA struc2string TYPE string.
    FIELD-SYMBOLS <strco> TYPE data.
    ASSIGN structure TO <strco>.
    IF sy-subrc = 0.
      TRY.
          DATA(components) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <strco> ) )->components.
          DO.
            TRY.
                DATA(co) = components[ sy-index ]-name.
                struc2string = struc2string &&
                               COND #( WHEN sy-index <> 1 THEN `, ` ) &&
                               co && `: "` &&
                               <strco>-(sy-index) && `"`.
              CATCH cx_sy_assign_illegal_component cx_sy_itab_line_not_found.
                EXIT.
            ENDTRY.
          ENDDO.
        CATCH cx_sy_move_cast_error.
      ENDTRY.
    ENDIF.

    out->write( data = struc2string name = `struc2string` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Dynamically Creating Data Objects at Runtime Using Dynamic Type Definitions` ) ).
    out->write( |29) Miscellaneous Data Objects (1)\n\n| ).

    "In an example above, anonymous data objects are created using static
    "type definitions. In this example, anonymous data objects are created
    "using a type determined at runtime.
    "The values of an internal table of type string represent type names.
    "The type name is used for the dynamic type specification in CREATE
    "DATA statements that use various additions. The following is
    "created dynamically: elementary data object, structure, internal
    "table, data reference. For output purposes, the newly created data
    "objects are assigned values.
    "Note:
    "- The NEW operator cannot be used here.
    "- The creation of a data object based on a type description object is shown
    "  below (TYPE HANDLE addition).
    "- Dynamic type specifications for ASSIGN statements together with the
    "  CASTING addition are shown above.

    DATA(type_names) = VALUE string_table( ( `I` )
                                           ( `STRING` )
                                           ( `ZDEMO_ABAP_CARR` ) ).

    DATA dataref TYPE REF TO data.
    DATA some_str TYPE string VALUE `some string`.
    DATA some_structure TYPE zdemo_abap_carr.

    LOOP AT type_names REFERENCE INTO DATA(refwa).
      out->write( |***** Loop iteration { sy-tabix }. Type: { refwa->* } *****| ).

      CASE refwa->*.
        WHEN `I`.
          CREATE DATA dataref TYPE (refwa->*).
          dataref->* = 123.

          out->write( data = dataref->* name = `dataref->*` ).
          out->write( |\n| ).

          CREATE DATA dataref TYPE TABLE OF (refwa->*).

          INSERT 1 INTO TABLE dataref->*.
          INSERT 2 INTO TABLE dataref->*.

          out->write( data = dataref->* name = `dataref->*` ).
          out->write( |\n| ).

          CREATE DATA dataref TYPE REF TO (refwa->*).
          dataref->* = REF i( 456 ).

          out->write( data = dataref->* name = `dataref->*` ).
          out->write( |\n| ).
        WHEN `STRING`.
          CREATE DATA dataref TYPE (refwa->*).
          dataref->* = `hello`.

          out->write( data = dataref->* name = `dataref->*` ).
          out->write( |\n| ).

          CREATE DATA dataref TYPE TABLE OF (refwa->*).
          INSERT `hello` INTO TABLE dataref->*.
          INSERT `abap` INTO TABLE dataref->*.

          out->write( data = dataref->* name = `dataref->*` ).
          out->write( |\n| ).

          CREATE DATA dataref TYPE REF TO (refwa->*).
          dataref->* = REF string( `hi` ).

          out->write( data = dataref->* name = `dataref->*` ).
          out->write( |\n| ).
        WHEN `ZDEMO_ABAP_CARR`.
          CREATE DATA dataref TYPE (refwa->*).
          SELECT SINGLE * FROM zdemo_abap_carr INTO @dataref->*.

          out->write( data = dataref->* name = `dataref->*` ).
          out->write( |\n| ).

          CREATE DATA dataref TYPE TABLE OF (refwa->*).
          SELECT * FROM zdemo_abap_carr INTO TABLE @dataref->* UP TO 3 ROWS.

          out->write( data = dataref->* name = `dataref->*` ).
          out->write( |\n| ).

          CREATE DATA dataref TYPE REF TO (refwa->*).
          SELECT SINGLE * FROM zdemo_abap_carr INTO NEW @dataref->*.

          out->write( data = dataref->* name = `dataref->*` ).
          out->write( |\n| ).
      ENDCASE.
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `30) Elementary Data Object (2)` ) ).

    "The example demonstrates the following:
    "- The method call takes care of providing the name of a built-in data type and more
    "- An elementary data object is created based on the data type
    "- For demonstration purposes, RTTI (as shown further down in more detail) is used
    "  to check on the created data object by retrieving the type description.

    DATA(b_type) = lcl_det_at_runtime=>get_builtin_type( ).

    DATA ref_bt TYPE REF TO data.

    TRY.
        CASE b_type-builtin_type.
          WHEN 'd' OR 'decfloat16' OR 'decfloat34' OR 'f' OR 'i'
                   OR 'string' OR 't' OR 'xstring'.

            CREATE DATA ref_bt TYPE (b_type-builtin_type).
          WHEN 'c' OR 'n' OR 'x'.
            CREATE DATA ref_bt TYPE (b_type-builtin_type) LENGTH b_type-len.
          WHEN 'p'.
            CREATE DATA ref_bt TYPE p LENGTH b_type-len DECIMALS b_type-dec.
          WHEN OTHERS.
            out->write( `That didn't work.` ).
        ENDCASE.

        "Getting type information using RTTI
        DATA(descr_builtin_type) = CAST cl_abap_elemdescr(
          cl_abap_typedescr=>describe_by_data( ref_bt->* ) ).

        out->write( |Built-in type determined at runtime: { b_type-builtin_type } | ).
        out->write( |\n| ).
        out->write( `Type information of created data object at runtime:` ).
        out->write( |\n| ).
        out->write( |\n| ).
        out->write( data = descr_builtin_type name = `descr_builtin_type` ).

      CATCH cx_root.
        out->write( `Something went wrong.` ).
    ENDTRY.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `31) Structure (3)` ) ).

    "The example demonstrates the following:
    "- The method call takes care of providing the name of a database table name.
    "- A structured data object is created based on the dynamically determined type.
    "  It is used as target data object for a SELECT statement. As shown further
    "  down in more detail, clauses of SELECT statements can be specified
    "  dynamically.

    "Retrieving table name
    DATA(type4struc) = lcl_det_at_runtime=>get_dyn_table_name( ).

    DATA ref_dynstruc TYPE REF TO data.

    "Creating structured data object
    CREATE DATA ref_dynstruc TYPE (type4struc).

    "Dynamic specification of the FROM clause
    SELECT SINGLE *
         FROM (type4struc)
         INTO @ref_dynstruc->*.

    out->write( |Structured data type/database table name determined at runtime: { type4struc } | ).
    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = ref_dynstruc->* name = `ref_dynstruc->*`  ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `32) Internal Table (4)` ) ).

    "The example demonstrates the following:
    "- The method call takes care of providing the name of a database table name.
    "- An internal table is created based on the dynamically determined type.
    "  It is used as target data object for a SELECT statement.
    "- The UP TO ... ROWS addition is provided with a random number in the example.
    "- AS in the example above, the SELECT statement includes the dynamic
    "  specification of the FROM clause.

    "Retrieving table name
    DATA(type_name) = lcl_det_at_runtime=>get_dyn_table_name( ).

    DATA ref_n TYPE REF TO data.

    "Creating internal table based on the type determined at runtime
    CREATE DATA ref_n TYPE TABLE OF (type_name).

    "Specifying random number for up to clause
    DATA(random_upto) = cl_abap_random_int=>create(
         seed = cl_abap_random=>seed( ) min = 2
                                        max = 6 )->get_next( ).

    "Dynamic specification of the FROM clause
    SELECT *
      FROM (type_name)
      INTO TABLE @ref_n->*
      UP TO @random_upto ROWS.

    out->write( |Table/type name determined at runtime: { type_name } | ).
    out->write( |\n| ).
    out->write( |At most, { random_upto } lines should have been read from the database table.| ).
    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = ref_n->* name = `ref_n->*` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `33) Absolute Type Names for Dynamically Specifying Types` ) ).

    "In addition to character-like data objects for the type name specified within the
    "parentheses, you can also use absolute type names for statements such as CREATE DATA.
    "The absolute type name is retrieved using RTTI. See more on RTTI further down.

    "Type to refer to
    TYPES type4abs TYPE p LENGTH 4 DECIMALS 3.
    "Data and object reference variables
    DATA dref4abs TYPE REF TO data.
    DATA oref4abs TYPE REF TO object.
    "Getting absolute names using RTTI
    DATA(abs_name_type) = cl_abap_typedescr=>describe_by_name( 'TYPE4ABS' )->absolute_name.
    DATA(abs_name_cl) = cl_abap_typedescr=>describe_by_name( 'ZCL_DEMO_ABAP_DYNAMIC_PROG' )->absolute_name.

    "Data references
    ""Named data object holding the absolute name
    CREATE DATA dref4abs TYPE (abs_name_type).
    "Unnamed data object
    CREATE DATA dref4abs TYPE ('\TYPE=STRING').

    "Object references
    "Named data object
    CREATE OBJECT oref4abs TYPE (abs_name_cl).
    "Unnamed data object
    CREATE OBJECT oref4abs TYPE ('\CLASS=ZCL_DEMO_ABAP_DYNAMIC_PROG').

    out->write( `No output for this section. You can set a break point and check the content of data objects in the debugger.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Dynamically Specifying Components/Clauses in Statements for Processing Internal Tables with ...` ) ).
    out->write( |34) SORT\n\n| ).

    "A field is determined at runtime on whose basis a sorting is done on an
    "internal table.

    DATA(field_name) = lcl_det_at_runtime=>get_dyn_field( ).

    SELECT *
     FROM zdemo_abap_carr
     ORDER BY carrid
     INTO TABLE @DATA(carr_itab)
     UP TO 3 ROWS.

    SORT carr_itab BY (field_name).

    out->write(  |Field name determined at runtime | &&
    |by which the sorting was done: { field_name } | ).
    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = carr_itab name = `carr_itab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `35) READ TABLE (1)` ) ).

    "Dynamic key specification in READ TABLE statements

    TYPES: BEGIN OF s,
             comp  TYPE string,
             value TYPE string,
           END OF s.

    TYPES comps_type TYPE TABLE OF s WITH EMPTY KEY.

    "Providing components and values for READ TABLE statement
    DATA(comps) = VALUE comps_type( ( comp = `CARRID` value = `LH` )
                                    ( comp = `CONNID` value = `0555` )
                                    ( comp = `SEATSOCC` value = `115` )
                                    ( comp = `CARRID` value = `XY` ) ). "not found

    SELECT *
      FROM zdemo_abap_fli
      INTO TABLE @DATA(itab_read_tab_dyn).

    LOOP AT comps INTO DATA(wa_comps).
      READ TABLE itab_read_tab_dyn
        INTO DATA(read_line)
        WITH KEY (wa_comps-comp) = wa_comps-value.

      IF sy-subrc = 0.
        out->write( data = wa_comps-comp name = `wa_comps-comp` ).
        out->write( |\n| ).
        out->write( data = wa_comps-value name = `wa_comps-value` ).
        out->write( |\n| ).
        out->write( data = read_line name = `read_line` ).
        out->write( |\n| ).
        CLEAR read_line.
      ELSE.
        out->write( data = wa_comps-comp name = `wa_comps-comp` ).
        out->write( |\n| ).
        out->write( data = wa_comps-value name = `wa_comps-value` ).
        out->write( |\n| ).
        out->write( `Line not found.` ).
        out->write( |\n| ).
      ENDIF.

    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `36) READ TABLE (2)` ) ).

    "Data objects and types to work with
    TYPES: BEGIN OF demo_struct,
             col1 TYPE i,
             col2 TYPE string,
             col3 TYPE string,
           END OF demo_struct.
    "Standard table and specification of primary and secondary table key
    DATA itab TYPE TABLE OF demo_struct
      WITH NON-UNIQUE KEY col1
      WITH UNIQUE SORTED KEY sk COMPONENTS col2.
    TYPES itab_type LIKE itab.
    DATA itab_ref TYPE TABLE OF REF TO demo_struct WITH EMPTY KEY.
    itab = VALUE #( ( col1 = 1 col2 = `aaa` col3 = `zzz` )
                    ( col1 = 2 col2 = `bbb` col3 = `yyy` )
                    ( col1 = 3 col2 = `ccc` col3 = `xxx` ) ).
    itab_ref = VALUE #( ( NEW demo_struct( col1 = 1 col2 = `aaa` col3 = `zzz` ) ) ).

    "Notes
    "- In statements using key specifications, secondary table key names (or alias names)
    "  are usually specified. Also the primary table key using the predefined name
    "  primary_key or its alias name can be used.
    "- Many of the following statements provide similar additions offering dynamic
    "  specifications, such as USING KEY and dynamic component name specifications.

    "Reading by specifying keys dynamically
    "Implicitly specifying the table key values in a work area (USING KEY addition)
    DATA(wa_read) = VALUE demo_struct( col2 = `aaa` ).
    READ TABLE itab FROM wa_read USING KEY ('SK') REFERENCE INTO DATA(read_ref).

    "Explicitly specifying the key and key values (TABLE KEY addition)
    "The component names can also be specified dynamically (which is done in most of the
    "following examples for demonstration purposes). Note that each component of the table
    "key must be specified.
    READ TABLE itab WITH TABLE KEY ('SK') COMPONENTS ('COL2') = `aaa` REFERENCE INTO read_ref.
    "Specifying the predefined name primary_key explicitly and dynamically
    READ TABLE itab WITH TABLE KEY ('PRIMARY_KEY') COMPONENTS ('COL1') = 1 REFERENCE INTO read_ref.
    "If the addition COMPONENTS is not specified, the primary table key is implicitly used.
    READ TABLE itab WITH TABLE KEY ('COL1') = 1 REFERENCE INTO read_ref.

    "Reading using a free key (WITH KEY addition)
    READ TABLE itab WITH KEY ('COL3') = `yyy` REFERENCE INTO read_ref.
    "The addition can also be used by specifying a secondary table key name
    READ TABLE itab WITH KEY ('SK') COMPONENTS ('COL2') = `ccc` REFERENCE INTO read_ref.

    "Reading based on a table index (INDEX addition)
    "Not using the addition USING KEY means reading from the primary table index.
    READ TABLE itab INDEX 1 USING KEY ('SK') REFERENCE INTO read_ref.

    "More dynamic specification options when specifying the target as work area
    "(COMPARING/TRANSPORTING additions)
    "TRANSPORTING: Specifying which components shall be respected
    READ TABLE itab INDEX 1 INTO DATA(workarea) TRANSPORTING ('COL1') ('COL3').

    "COMPARING: If the content of the compared components is identical, sy-subrc is set
    "to 0, and otherwise to 2. The line found is assigned to the work area independently
    "of the result of the comparison.
    workarea-('COL3') = `uvw`.
    READ TABLE itab INDEX 1 INTO workarea COMPARING ('COL3') TRANSPORTING ('COL1') ('COL3').
    IF sy-subrc <> 0.
      ...
    ENDIF.

    out->write( `No output for this section. You can set a break point and check the content of data objects in the debugger.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `37) Table Expressions` ) ).

    "Similar to READ TABLE statements, you can specify table lines with 3 alternatives:
    "index read, read using free key, table key
    "Also there, dynamic specifications are possible regarding the key specifications.

    "Reading based on index with dynamic key specifications
    "Specifying the secondary table index of a sorted secondary key
    DATA(wa_te1) = itab[ KEY ('SK') INDEX 1 ].
    "Reading using a free key, the keys are specified dynamically
    DATA(wa_te2) = itab[ ('COL2') = `bbb` ('COL3') = `yyy` ].

    "Reading using a table key
    "Specyfing the table key explicitly
    "Note: Unlike READ TABLE statements, the name of the table key must be specified. The
    "addition COMPONENTS can be omitted.
    "In the following example, the component names are also specified dynamically.
    DATA(wa_te3) = itab[ KEY ('SK') ('COL2') = `ccc` ].
    "Specifying the COMPONENTS addition explicitly
    DATA(wa_te4) = itab[ KEY ('PRIMARY_KEY') COMPONENTS ('COL1') = 1 ].

    "Accessing components
    "As shown above, chaininings with the (object) component selector are possible.
    "The examples use index access and write positions.
    itab[ 1 ]-('COL2') = `jkl`.
    itab_ref[ 1 ]->('COL2') = `mno`.

    out->write( `No output for this section. You can set a break point and check the content of data objects in the debugger.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `38) LOOP (1)` ) ).

    "Dynamic specification of the key in LOOP statements
    "In the example, the loop can be executed with the entries 'SKEY' and 'PRIMARY_KEY'.
    "This is not case sensitive. Any other entries produce a runtime error.

    DATA(keys) = VALUE string_table( ( `PRIMARY_KEY` ) ( `SKEY` ) ).

    DATA itab_loop TYPE TABLE OF i
              WITH NON-UNIQUE KEY primary_key COMPONENTS table_line
              WITH NON-UNIQUE SORTED KEY skey COMPONENTS table_line.

    itab_loop = VALUE #( ( 3 ) ( 2 ) ( 1 ) ).

    DATA itab_dyn_key LIKE itab_loop.

    LOOP AT keys INTO DATA(k).

      LOOP AT itab_loop INTO DATA(wa_lo) USING KEY (k).
        APPEND wa_lo TO itab_dyn_key.
      ENDLOOP.

      out->write( |Loop over internal table using key "{ k }".| ).
      out->write( |\n| ).
      out->write( data = itab_dyn_key name = `itab_dyn_key` ).
      out->write( |\n| ).
      CLEAR itab_dyn_key.

    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `39) LOOP (2)` ) ).

    "USING KEY addition: Overriding the standard order determined by the table category
    LOOP AT itab REFERENCE INTO DATA(ref) USING KEY ('SK').
      ...
    ENDLOOP.

    "When the primary table key is specified, the loop behaves as if it was not specified.
    "So, the following statement corresponds to the one below.
    LOOP AT itab REFERENCE INTO ref USING KEY ('PRIMARY_KEY').
      ...
    ENDLOOP.

    LOOP AT itab REFERENCE INTO ref.
      ...
    ENDLOOP.

    "Dynamic WHERE condition
    "You can specify a character-like data object or a standard table with character-like
    "line type.
    DATA(cond_loop) = `COL1 > 1`.
    LOOP AT itab REFERENCE INTO ref WHERE (cond_loop).
      ...
    ENDLOOP.

    out->write( `No output for this section. You can set a break point and check the content of data objects in the debugger.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `40) INSERT` ) ).

    "The USING KEY addition (which accepts a dynamic specification) affects the order in which lines are inserted.

    "Result of the following example when using the ...
    "- secondary table key: order of itab entries 5 ... /4 ... /...
    "- primary table key: order of itab entries 4 ... /5 ... /...
    INSERT LINES OF VALUE itab_type( ( col1 = 4 col2 = `eee` col3 = `www` )
                                     ( col1 = 5 col2 = `ddd` col3 = `vvv` ) )
      USING KEY ('SK')
      "USING KEY ('PRIMARY_KEY')
      INTO itab INDEX 1.

    "Excursion: Using LOOP AT statements with the USING KEY addition
    "and exploring the table index
    "Declaring demo tables to hold the internal table entries
    DATA it_seckey_idx TYPE TABLE OF demo_struct WITH EMPTY KEY.
    DATA it_primekey_idx LIKE it_seckey_idx.

    "Visualizing the secondary table index
    LOOP AT itab INTO DATA(wa_sk) USING KEY ('SK').
      APPEND wa_sk TO it_seckey_idx.
    ENDLOOP.

    out->write( data = it_seckey_idx name = `it_seckey_idx` ).
    out->write( |\n| ).

    "Visualizing the primary table index
    LOOP AT itab INTO DATA(wa_pk) USING KEY ('PRIMARY_KEY').
      APPEND wa_pk TO it_primekey_idx.
    ENDLOOP.

    out->write( data = it_primekey_idx name = `it_primekey_idx` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `41) MODIFY (1)` ) ).

    "Dynamic WHERE condition in MODIFY statements
    "Note:
    "- The addition WHERE can only be specified together with the addition TRANSPORTING.
    "- Invalid logical expressions raise an exception from the class CX_SY_ITAB_DYN_LOOP.

    TYPES:
      BEGIN OF line,
        col1 TYPE c LENGTH 1,
        col2 TYPE i,
      END OF line.

    DATA itab_mod_tab_dyn TYPE SORTED TABLE OF line
                   WITH UNIQUE KEY col1.

    itab_mod_tab_dyn = VALUE #( ( col1 = 'A' col2 = 1 )
                                ( col1 = 'B' col2 = 10 )
                                ( col1 = 'C' col2 = 100 ) ).

    out->write( `Internal table content before modifications:` ).
    out->write( |\n| ).
    out->write( data = itab_mod_tab_dyn name = `itab_mod_tab_dyn` ).
    out->write( |\n| ).

    "Providing conditions for MODIFY statement
    DATA(conditions) = VALUE string_table( ( `col2 < 5` )
                                           ( `col2 = 10` )
                                           ( `colxyz > 50` ) ). "to fail

    LOOP AT itab_mod_tab_dyn INTO DATA(wa_mod_dyn).
      TRY.

          DATA(condition) = conditions[ sy-tabix ].

          MODIFY itab_mod_tab_dyn
            FROM VALUE line( col2 = wa_mod_dyn-col2 * 2 )
            TRANSPORTING col2
            WHERE (condition).

        CATCH cx_sy_itab_dyn_loop.
          out->write( |Invalid WHERE condition "{ condition }".| ).
      ENDTRY.
    ENDLOOP.

    out->write( `Internal table content after modifications:` ).
    out->write( |\n| ).
    out->write( data = itab_mod_tab_dyn name = `itab_mod_tab_dyn` ).


**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `42) MODIFY (2)` ) ).

    "In the following example, a line is modified based on a work area and a table key.
    "The component col1 is left out from the work area intentionally.
    "If the primary table key was used, the value of sy-subrc would be 4, and no modification was done.
    "The optional addition transporting is specified to denote what should be modified. In this example,
    "the component is also specified dynamically.
    MODIFY TABLE itab FROM VALUE #( col2 = `bbb` col3 = `uuu` ) USING KEY ('SK') TRANSPORTING ('COL3').

    "In the following example, a line is modified based on a work area, an index specification and a
    "table key.
    "INDEX can also be positioned after FROM.
    MODIFY itab INDEX 2 USING KEY ('SK') FROM VALUE #( col3 = `ttt` ) TRANSPORTING ('COL3').

    "Dynamic WHERE clause (only to be used with the TRANSPORTING addition)
    "The USING KEY addition is also possible. Check the ABAP Keyword Documentation
    "for special rules that apply.
    DATA(cond_mod) = `COL1 < 3`.
    MODIFY itab FROM VALUE #( col3 = `sss` ) TRANSPORTING ('COL3') WHERE (cond_mod).

    out->write( data = itab name = `itab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `43) DELETE (1)` ) ).

    "Dynamic WHERE condition in DELETE statements

    DATA itab_del_tab_dyn TYPE TABLE OF i WITH EMPTY KEY
                   WITH NON-UNIQUE SORTED KEY skey COMPONENTS table_line.

    itab_del_tab_dyn = VALUE #( ( 100 )
                                ( 200 )
                                ( 300 )
                                ( 400 )
                                ( 500 )
                                ( 600 ) ).

    out->write( `Internal table content before modifications:` ).
    out->write( |\n| ).
    out->write( data = itab_del_tab_dyn name = `itab_del_tab_dyn` ).
    out->write( |\n| ).

    DO 3 TIMES.
      TRY.

          CASE sy-index.
            WHEN 1.
              condition = `table_line <= 200`.
            WHEN 2.
              condition = `table_line >= 500`.
            WHEN 3.
              condition = `col1 = 600`.
          ENDCASE.

          DELETE itab_del_tab_dyn
            USING KEY skey
            WHERE (condition).

          out->write( |Condition: { condition }| ).
          out->write( |\n| ).
          out->write( data = itab_del_tab_dyn name = `itab_del_tab_dyn` ).
          out->write( |\n| ).
        CATCH cx_sy_itab_dyn_loop.
          out->write( |Invalid WHERE condition "{ condition }".| ).
          out->write( |\n| ).
      ENDTRY.
    ENDDO.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `44) DELETE (2)` ) ).

    "A single line or multipled lines can be deleted.
    "Note that DELETE ADJACENT DUPLICATES statements can also be specified using
    "dynamic parts.

    "Deleting based on a dynamically specified table key
    "The values can be declared either implicitly in a work area after FROM or explicitly
    "by listing the components of the table key after TABLE KEY.
    "If the USING KEY addition is not specified, the primary table key is used by default.
    DELETE TABLE itab FROM VALUE #( col2 = `eee` col3 = `www` ) USING KEY ('SK').

    "Each component of the table key must be listed.
    DELETE TABLE itab WITH TABLE KEY ('SK') COMPONENTS ('COL2') = `ddd`.

    "Deleting based on the table index
    DELETE itab INDEX 1 USING KEY ('SK').

    "Deleting multiple lines and specifying the WHERE conditions dynamically
    "The USING KEY addition is also possible.
    DATA(condition_tab) = VALUE string_table( ( `COL1 < 3` )
                                              ( `OR` )
                                              ( `COL3 = ``www``` ) ).
    DELETE itab WHERE (condition_tab).

    out->write( data = itab name = `itab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Dynamically Specifying Clauses in ABAP SQL SELECT Statements` ) ).
    out->write( |45) SELECT List\n\n| ).

    "In the example, the SELECT list that is used in a SELECT statement is
    "determined at runtime.
    "Note: Check out the CL_ABAP_DYN_PRG class, which supports dynamic programming by
    "checking the validity of dynamic specifications. See an example below.

    DATA(select_list) = lcl_det_at_runtime=>get_dyn_select_list( ).

    DATA sel_table TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY.

    SELECT (select_list)
     FROM zdemo_abap_flsch
     ORDER BY carrid
     INTO CORRESPONDING FIELDS OF TABLE @sel_table
     UP TO 3 ROWS.

    out->write( |SELECT list determined at runtime: { select_list } | ).
    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = sel_table name = `sel_table` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `46) FROM Clause` ) ).

    "In the example, the FROM clause that is used in a SELECT statement is
    "determined at runtime. Here, the number of entries of a database table
    "is counted.

    DATA(tab_name) = lcl_det_at_runtime=>get_dyn_table_name( ).

    SELECT COUNT(*)
     FROM (tab_name)
     INTO @DATA(count).

    out->write( |Table name determined at runtime: { tab_name } | ).
    out->write( |\n| ).
    out->write( |The table { tab_name } has { count } entries.| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `47) WHERE Clause` ) ).

    "In the example, the WHERE clause that is used in a SELECT statement is
    "determined at runtime. Here, the WHERE clause is based on a string
    "table.

    DATA(where_clause) = lcl_det_at_runtime=>get_dyn_where_clause( ).

    SELECT *
     FROM zdemo_abap_flsch
     WHERE (where_clause)
     ORDER BY carrid
     INTO TABLE @DATA(where_tab)
     UP TO 5 ROWS.

    out->write( |WHERE clause determined at runtime:| ).
    out->write( |\n| ).
    out->write( data =  where_clause name = `where_clause` ).
    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = where_tab name = `where_tab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `48) ORDER BY Clause` ) ).

    DATA(order_by) = 'FLTIME'.

    SELECT *
     FROM zdemo_abap_flsch
     ORDER BY (order_by)
     INTO TABLE @DATA(order_by_tab)
     UP TO 5 ROWS.

    out->write( |ORDER BY clause determined at runtime:| ).
    out->write( |\n| ).
    out->write( data = order_by name = `order_by` ).
    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = order_by_tab name = `order_by_tab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `49) Multiple Dynamically Specified Clauses in an ABAP SQL SELECT Statement` ) ).

    "In this example, multiple clauses in a SELECT statement are
    "determined at runtime to demonstrate the rich variety of possibilities.
    "Note: The rows and target table specifications are not real dynamic
    "specifications in the SELECT statement in the sense of syntax elements
    "enclosed by parentheses. Here, they are just included to provide some
    "more 'dynamic' touch of the statement :)

    "Getting all clauses of the SELECT statement
    DATA(dyn_syntax_elem) = lcl_det_at_runtime=>get_dyn_syntax_elements( ).

    IF dyn_syntax_elem-table IS NOT INITIAL
    AND dyn_syntax_elem-select_list IS NOT INITIAL
    AND dyn_syntax_elem-where_clause IS NOT INITIAL
    AND dyn_syntax_elem-order_by IS NOT INITIAL
    AND dyn_syntax_elem-target IS BOUND
    AND dyn_syntax_elem-rows IS NOT INITIAL.

      out->write( `Dynamically determined syntax elements:` ).
      out->write( |\n| ).
      out->write( data = dyn_syntax_elem name = `dyn_syntax_elem` ).
      out->write( |\n| ).

      SELECT (dyn_syntax_elem-select_list)
        FROM (dyn_syntax_elem-table)
        WHERE (dyn_syntax_elem-where_clause)
        ORDER BY (dyn_syntax_elem-order_by)
        INTO CORRESPONDING FIELDS OF TABLE @dyn_syntax_elem-target->*
        UP TO @dyn_syntax_elem-rows ROWS.

      out->write( data = dyn_syntax_elem-target->* name = `dyn_syntax_elem-target->*` ).

    ELSE.
      out->write( `There's an issue with syntax elements.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `50) Dynamic Specifications in Other ABAP SQL Statements` ) ).

    "Creating a structure to be inserted into the database table
    DATA(table) = 'ZDEMO_ABAP_CARR'.
    DATA(wherecl) = 'CARRID = ''ZZ'''.
    SELECT SINGLE *
          FROM (table)
          INTO NEW @DATA(refstruc).
    refstruc->('CARRID') = 'ZZ'.

    INSERT (table) FROM @refstruc->*.

    SELECT SINGLE *
      FROM (table)
      WHERE (wherecl)
      INTO NEW @DATA(res1).
    out->write( data = res1->* name = `res1->*` ).
    out->write( |\n| ).

    refstruc->('CARRNAME') = 'ZZ Airlines'.
    UPDATE (table) FROM @refstruc->*.

    SELECT SINGLE *
      FROM (table)
      WHERE (wherecl)
      INTO NEW @DATA(res2).
    out->write( data = res2->* name = `res2->*` ).
    out->write( |\n| ).


    refstruc->('CURRCODE') = 'GBP'.
    MODIFY (table) FROM @refstruc->*.

    SELECT SINGLE *
      FROM (table)
      WHERE (wherecl)
      INTO NEW @DATA(res3).
    out->write( data = res3->* name = `res3->*` ).
    out->write( |\n| ).

    DELETE FROM (table) WHERE (wherecl).

    SELECT *
      FROM (table)
      INTO TABLE NEW @DATA(res4).
    out->write( data = res4->* name = `res4->*` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `51) Validating Input for Dynamic Specifications` ) ).
    "The following examples use methods of the CL_ABAP_DYN_PRG class, which supports
    "dynamic programming by checking the validity for dynamic specifications.
    "Check out the class documentation for more information.

    "In the following example, several table names are provided in a string table that
    "is looped over. Some wrong table names are intentionally included.
    "You can provide the "packages" formal parameter with a table that contains
    "the names of packages in which the tables should be included. The
    "assumption is that you imported the repository into the package
    "specified. If you imported it into a package with a different name,
    "change the value accordingly. Otherwise, none of the tables is found.

    out->write( `****** Checking if the input is a database table name, inlcuded in given packages ******` ).
    out->write( |\n| ).

    DATA(table_names) = VALUE string_table( ( `ZDEMO_ABAP_CARR` )
                                            ( `ZDEMO_ABAP_FLI` )
                                            ( `NO_DBTAB` )
                                            ( `ZDEMO_ABAP_FLSCH` )
                                            ( `ZDEMO_ABAP_FLI2` )
                                            ( `WRONG_DBTAB` ) ).

    DATA(check_packages) = VALUE string_hashed_table( ( `ZABAP_CHEAT_SHEETS` ) ).

    DATA dbtab TYPE string.

    LOOP AT table_names INTO DATA(wa_tab).
      TRY.
          dbtab = cl_abap_dyn_prg=>check_table_name_tab( val      = to_upper( wa_tab )
                                                         packages = check_packages ).

          SELECT SINGLE * FROM (dbtab) INTO NEW @DATA(ref_wa).

          out->write( |Result for { wa_tab }:| ).
          out->write( ref_wa->* ).
          out->write( |\n| ).
        CATCH cx_abap_not_a_table cx_abap_not_in_package INTO DATA(err).
          out->write( |Result for { wa_tab }:| ).
          out->write( err->get_text( ) ).
          out->write( |\n| ).
      ENDTRY.
    ENDLOOP.

    out->write( |\n| ).
    out->write( `****** Checking if input can be a column name and does not contain invalid characters ******` ).
    out->write( |\n| ).

    "The following example uses a method with which the validity of column names
    "of database tables can be checked.

    DATA(to_be_checked) = VALUE string_table( ( `CARRID` )
                             ( `CONNID` )
                             ( `SOME_COLUMN` )
                             ( `??NOPE??` )
                             ( `...!` ) ).

    LOOP AT to_be_checked INTO DATA(chk).
      TRY.
          DATA(col_name) = cl_abap_dyn_prg=>check_column_name( val    = chk
                                                               strict = abap_true ).
          out->write( |{ col_name } is allowed.| ).
        CATCH cx_abap_invalid_name INTO err.
          out->write( err->get_text( ) ).
      ENDTRY.
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `52) Dynamic Invoke` ) ).

    "In the example, both class and method are determined at runtime for
    "the method call. The suitable parameter table is filled in the
    "method get_dyn_class_meth. The example is implemented in a way that
    "all the methods that are called store some text in a string. This
    "string is displayed to see the effect of the dynamic method call.

    lcl_det_at_runtime=>get_dyn_class_meth( IMPORTING cl = DATA(cl_name)
                                                      meth = DATA(meth_name)
                                                      ptab = DATA(p_tab) ).

    CALL METHOD (cl_name)=>(meth_name) PARAMETER-TABLE p_tab.

    out->write( |Class name determined at runtime: { cl_name } | ).
    out->write( |\n| ).
    out->write( |Method name determined at runtime: { meth_name } | ).
    out->write( |\n| ).

    out->write( `Result of method call (text stored in a variable):` ).
    out->write( |\n| ).
    out->write( data = lcl_det_at_runtime=>dyn_meth_call_result name = `lcl_det_at_runtime=>dyn_meth_call_result` ).

    "Further method calls
    "The class and method to be used is determined here by just providing
    "the character-like content (the name) via a data object in a predefined way.

    DATA method TYPE string VALUE `FILL_STRING`.

    "Note that the method has no parameters in this example.
    "Similar to above. The method stores some text in a string which is
    "displayed to see the effect of the dynamic method call.
    CALL METHOD lcl_det_at_runtime=>(method).

    out->write( data = lcl_det_at_runtime=>dyn_meth_call_result name = `lcl_det_at_runtime=>dyn_meth_call_result` ).
    out->write( |\n| ).

    DATA class TYPE string VALUE `LCL_DET_AT_RUNTIME`.

    CALL METHOD (class)=>fill_string.

    out->write( data = lcl_det_at_runtime=>dyn_meth_call_result name = `lcl_det_at_runtime=>dyn_meth_call_result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `53) RTTI: Getting Type Information at Runtime/Getting a Reference to a Type Description Object` ) ).

    "Getting a reference to a type description object of a type.
    "i.e. getting an instance of a type description class
    "As shown below, the type decription object can be used
    "to create data objects dynamically.

    "Type for which information should be retrieved
    TYPES: elem_type TYPE c LENGTH 5.

    "Creating a data reference variable to hold the reference to
    "the type description object
    DATA type_descr_obj_elem TYPE REF TO cl_abap_elemdescr.

    "Retrieving type information by creating an instance of a type description class
    "As the name implies, the describe_by_name method expects the name of the type
    "The following example uses the CAST operator for the necessary downcast.
    type_descr_obj_elem = CAST #( cl_abap_typedescr=>describe_by_name( 'ELEM_TYPE' ) ).

    "Using the older ?= operator
    type_descr_obj_elem ?= cl_abap_typedescr=>describe_by_name( 'ELEM_TYPE' ).

    "Inline declaration is handy to avoid helper variables.
    DATA(type_descr_obj_elem_inl) = CAST cl_abap_elemdescr(
      cl_abap_typedescr=>describe_by_name( 'ELEM_TYPE' ) ).

    "You may also want to check the type description object in the debugger.
    out->write( data = type_descr_obj_elem_inl name = `type_descr_obj_elem_inl` ).

    "Various methods/attributes (note that they vary depending on the type) provide
    "you with detailed information.
    "The following examples show a selection.
    "Kind/Type kind/Output length
    DATA(kind_elem) = type_descr_obj_elem_inl->kind.
    DATA(type_kind_elem) = type_descr_obj_elem_inl->type_kind.
    DATA(output_length_elem) = type_descr_obj_elem_inl->output_length.

    out->write( data = kind_elem name = `kind_elem` ).
    out->write( |\n| ).
    out->write( data = type_kind_elem name = `type_kind_elem` ).
    out->write( |\n| ).
    out->write( data = output_length_elem name = `output_length_elem` ).
    out->write( |\n| ).

    "In the following example, the type properties are retrieved
    "without casting. The data object has the type ref to
    "cl_abap_typedescr. See the hierarchy tree of type description classes.
    "The reference in the type description object references an
    "object from one of the classes CL_ABAP_ELEMDESCR, CL_ABAP_ENUMDESCR,
    "CL_ABAP_REFDESCR, CL_ABAP_STRUCTDESCR, CL_ABAP_TABLEDSECR,
    "CL_ABAP_CLASSDESCR, or CL_ABAP_INTFDESCR.
    "In the following case, it is CL_ABAP_ELEMDESCR.
    "Note that in most of the RTTI examples in this class, the explicit
    "casting is included when retrieving a reference to the type
    "description object.
    TYPES another_elem_type TYPE n LENGTH 3.
    DATA(type_descr_obj_elem_inl_2) = cl_abap_typedescr=>describe_by_name( 'ANOTHER_ELEM_TYPE' ).

    out->write( data = type_descr_obj_elem_inl_2->kind name = `type_descr_obj_elem_inl_2->kind` ).
    out->write( |\n| ).
    out->write( data = type_descr_obj_elem_inl_2->type_kind name = `type_descr_obj_elem_inl_2->type_kind` ).
    out->write( |\n| ).

    "More types
    "Structured data type (here, using the name of a database table)
    DATA(type_descr_obj_struc) = CAST cl_abap_structdescr(
      cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_CARR' ) ).

    "Various attributes/methods available for detailed information
    "Kind
    DATA(struc_kind) = type_descr_obj_struc->kind.
    "Components of the structure (e.g. the component names and type description
    "objects for the individual components)
    DATA(comps_struc) = type_descr_obj_struc->get_components( ).
    "The attribute also lists the component names and types (but not the type
    "desription objects)
    DATA(comps_struc2) = type_descr_obj_struc->components.
    "Kind of structure
    DATA(struct_kind) = type_descr_obj_struc->struct_kind.

    out->write( data = struc_kind name = `struc_kind` ).
    out->write( |\n| ).
    out->write( data = comps_struc name = `comps_struc` ).
    out->write( |\n| ).
    out->write( data = comps_struc2 name = `comps_struc2` ).
    out->write( |\n| ).
    out->write( data = struct_kind name = `struct_kind` ).
    out->write( |\n| ).

    "Internal table type
    TYPES table_type TYPE SORTED TABLE OF zdemo_abap_carr WITH UNIQUE KEY carrid.

    DATA(type_descr_obj_tab) = CAST cl_abap_tabledescr(
      cl_abap_typedescr=>describe_by_name( 'TABLE_TYPE' ) ).

    "Kind
    DATA(tab_kind) = type_descr_obj_tab->kind.
    "The following method returns more information than the attribute below
    "(e.g. key kind (unique) etc.)
    DATA(tab_keys) = type_descr_obj_tab->get_keys( ).
    DATA(tab_keys2) = type_descr_obj_tab->key. "Attribute; lists the keys
    "Getting internal table components
    "The method get_table_line_type returns a variable of type ref to cl_abap_datadescr.
    "This way you can retrieve the table components. Method chaining comes in handy.
    DATA(tab_comps) = CAST cl_abap_structdescr(
      type_descr_obj_tab->get_table_line_type( ) )->get_components( ).

    DATA component_table TYPE string_table.
    LOOP AT tab_comps INTO DATA(wa_comp).
      APPEND |Name: "{ wa_comp-name }" Type kind: "{ wa_comp-type->type_kind }" Type length: "{ wa_comp-type->length }"| TO component_table.
    ENDLOOP.

    out->write( data = tab_kind name = `tab_kind` ).
    out->write( |\n| ).
    out->write( data = tab_keys name = `tab_keys` ).
    out->write( |\n| ).
    out->write( data = tab_keys2 name = `tab_keys2` ).
    out->write( |\n| ).
    out->write( data = component_table name = `component_table` ).
    out->write( |\n| ).

    "Reference type
    TYPES ref_str TYPE REF TO string.
    DATA(type_descr_obj_ref) = CAST cl_abap_refdescr(
      cl_abap_typedescr=>describe_by_name( 'REF_STR' ) ).

    "Kind
    DATA(ref_kind) = type_descr_obj_ref->kind.
    "Returns type description object of the referenced type
    DATA(ref_type) = type_descr_obj_ref->get_referenced_type( ).
    "Absolute type name
    DATA(ref_type_abs_name) =
      type_descr_obj_ref->get_referenced_type( )->absolute_name.
    "Type kind
    DATA(ref_type_type_kind) =
      type_descr_obj_ref->get_referenced_type( )->type_kind.

    out->write( data = ref_kind name = `ref_kind` ).
    out->write( |\n| ).
    out->write( data = ref_type name = `ref_type` ).
    out->write( |\n| ).
    out->write( data = ref_type_abs_name name = `ref_type_abs_name` ).
    out->write( |\n| ).
    out->write( data = ref_type_type_kind name = `ref_type_type_kind` ).

    "Getting a reference to a type description object of an existing data object.
    "Instead of referring to the name of a type, referring to a data object here.
    "The relevant method is describe_by_data

    "Elementary data object
    DATA dobj_elem TYPE i.
    DATA(ty_des_obj_el) = CAST cl_abap_elemdescr(
      cl_abap_typedescr=>describe_by_data( dobj_elem ) ).

    "Structure
    DATA dobj_struc TYPE zdemo_abap_carr.
    DATA(ty_des_obj_struc) = CAST cl_abap_structdescr(
      cl_abap_typedescr=>describe_by_data( dobj_struc ) ).

    "Internal table
    DATA dobj_itab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
    DATA(ty_des_obj_itab) = CAST cl_abap_tabledescr(
      cl_abap_typedescr=>describe_by_data( dobj_itab ) ).

    "Reference variable
    DATA dref_var TYPE REF TO string.
    DATA(ty_des_obj_dref) = CAST cl_abap_refdescr(
      cl_abap_typedescr=>describe_by_data( dref_var ) ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `54) RTTI: Getting Type Information at Runtime for Miscellaneous Types` ) ).

    "The example demonstrates RTTI as follows:
    "- The method call takes care of providing the name of a type. It is implemented
    "  in a way that various types can be returned, i. e. elementary, structure,
    "  internal table, reference, class, interface.
    "- If the retrieved type is not a class or interface, a data object is created
    "  based on the type determined at runtime.
    "- The type description is retrieved using the method cl_abap_typedescr=>describe_by_data.
    "  Note the casts for the information retrieval statements.
    "- Depending on the type kind, various pieces of information are retrieved. There
    "  are plenty of options. In ADT, you can use the input help. Just position the
    "  cursor after the reference variable and ->, e.g. el->, and hit CTRL-Space.
    "  A dropdown appears showing you the variety you can explore. Check the class
    "  documentation for more information.
    "- If the retrieved type is a class or interface, the type description is
    "  retrieved using cl_abap_typedescr=>describe_by_name.
    "- The example for a class type includes the creation of an object based
    "  on a type determined at runtime using a CREATE OBJECT statement.

    "Retrieving type
    DATA(get_type) = lcl_det_at_runtime=>get_random_type( ).

    out->write( |Type name determined at runtime: { get_type }| ).
    out->write( |\n| ).

    DATA dref TYPE REF TO data.

    IF get_type <> `LCL_DET_AT_RUNTIME`
    AND get_type <> `IF_OO_ADT_CLASSRUN`.
      TRY.
          CREATE DATA dref TYPE (get_type).
        CATCH cx_sy_create_data_error.
          out->write( `Create data error!` ).
      ENDTRY.

      "Retrieving type information
      "When referring to a concrete data object name, you can use this method:
      DATA(some_type) = cl_abap_typedescr=>describe_by_data( dref->* ).

      "Elementary type
      IF some_type->kind = cl_abap_typedescr=>kind_elem.
        DATA(el) = CAST cl_abap_elemdescr( some_type ).
        out->write( data = el name = `el` ).
        "Various attributes and methods possible
        out->write( data = el->type_kind name = `el->type_kind` ).
        out->write( |\n| ).
        out->write( data = el->absolute_name name = `el->absolute_name` ).
        out->write( |\n| ).
        out->write( data = el->get_relative_name( ) name = `el->get_relative_name( )` ).
        out->write( |\n| ).

        "Structure
      ELSEIF some_type->kind = cl_abap_typedescr=>kind_struct.
        DATA(stru) = CAST cl_abap_structdescr( some_type ).
        out->write( data = stru->absolute_name name = `stru->absolute_name` ).
        out->write( |\n| ).
        out->write( data = stru->components name = `stru->components` ).
        out->write( |\n| ).
        out->write( data = stru->struct_kind name = `stru->struct_kind` ).
        out->write( |\n| ).
        out->write( data = stru->get_components( ) name = `stru->get_components( )` ).
        out->write( |\n| ).

        "Internal table
      ELSEIF some_type->kind =  cl_abap_typedescr=>kind_table.
        DATA(tab) = CAST cl_abap_tabledescr( some_type ).
        out->write( data = tab->absolute_name name = `tab->absolute_name` ).
        out->write( |\n| ).
        out->write( data = tab->table_kind name = `tab->table_kind` ).
        out->write( |\n| ).
        out->write( data = tab->get_keys( ) name = `tab->get_keys` ).
        out->write( |\n| ).
        out->write( data = tab->get_table_line_type( ) name = `tab->get_table_line_type( )` ).
        out->write( |\n| ).

        "Reference
      ELSEIF some_type->kind =  cl_abap_typedescr=>kind_ref.
        DATA(ref_descr) = CAST cl_abap_refdescr( some_type ).
        out->write( data = ref_descr->absolute_name name = `ref_descr->absolute_name` ).
        out->write( |\n| ).
        out->write( data = ref_descr->get_referenced_type( ) name = `ref_descr->get_referenced_type( )` ).
        out->write( |\n| ).
      ELSE.
        out->write( `Others ...` ).
      ENDIF.

    ELSE.

      "Retrieving type information
      "Here, using the type name and not a concrete data object as above.
      some_type = cl_abap_typedescr=>describe_by_name( get_type ).

      "Class
      IF some_type->kind =  cl_abap_typedescr=>kind_class.
        DATA(class_desc) = CAST cl_abap_classdescr( some_type ).
        out->write( data = class_desc->absolute_name name = `class_desc->absolute_name` ).
        out->write( |\n| ).
        out->write( data = class_desc->attributes name = `class_desc->attributes` ).
        out->write( |\n| ).
        out->write( data = class_desc->methods name = `class_desc->methods` ).
        out->write( |\n| ).

        "Creating an object based on a type determined at runtime
        DATA oref TYPE REF TO object.

        TRY.
            CREATE OBJECT oref TYPE (get_type).
            "Retrieving type information
            DATA(descr_ref) = cl_abap_typedescr=>describe_by_object_ref( oref ).
            out->write( data = descr_ref->absolute_name name = `descr_ref->absolute_name` ).
            out->write( |\n| ).
            out->write( data = descr_ref->kind name = `descr_ref->kind` ).
            out->write( |\n| ).
          CATCH cx_root.
            out->write( `Error` ).
        ENDTRY.

        "Interface
      ELSEIF some_type->kind =  cl_abap_typedescr=>kind_intf.
        DATA(if_descr) = CAST cl_abap_intfdescr( some_type ).
        out->write( data = if_descr->absolute_name name = `if_descr->absolute_name` ).
        out->write( |\n| ).
        out->write( data = if_descr->methods name = `class_desc->methods` ).
        out->write( |\n| ).
      ELSE.
        out->write( `Others ...` ).
      ENDIF.
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `55) RTTC: Dynamically Creating Data Types at Runtime` ) ).

    "You can create data types at program runtime using methods of the type
    "description classes of RTTS. These types are only valid locally in the
    "program. They are also anonymous, i.e. they are only accessible through
    "type description objects. As shown above, you can get a reference to a
    "type description object of a type using the static methods of the class
    "CL_ABAP_TYPEDESCR. The focus here is on using RTTC methods such as get*.

    "Creating type description objects using ...
    "... elementary data types
    "Conceptually, all elementary, built-in ABAP types already exist and can
    "be accessed by the corresponding get_* methods.
    "In ADT, click CTRL + space after cl_abap_elemdescr=>... to check out the options.
    "The following examples show a selection.
    DATA(tdo_elem_i) = cl_abap_elemdescr=>get_i( ).
    DATA(tdo_elem_string) = cl_abap_elemdescr=>get_string( ).
    "For the length specification of type c, there is an importing parameter available.
    DATA(tdo_elem_c_l20) = cl_abap_elemdescr=>get_c( 10 ).
    "Type p with two parameters to be specified.
    DATA(tdo_elem_p) = cl_abap_elemdescr=>get_p( p_length = 3 p_decimals = 2 ).

    "Instead of calling get_i() and others having no importing parameters, you could also call
    "the describe_by_name( ) method and pass the type names (I‚ STRING etc.) as arguments.
    "DATA(tdo_elem_i_2) = CAST cl_abap_elemdescr(
    "  cl_abap_typedescr=>describe_by_name( 'I' ) ).
    "DATA(tdo_elem_string_2) = CAST cl_abap_elemdescr(
    "  cl_abap_typedescr=>describe_by_name( 'STRING' ) ).

    "... structured data types
    "They are created based on a component description table.

    "A structured type such as the following shall be created using a
    "type description object.
    TYPES:
      BEGIN OF struc_type,
        a TYPE string,
        b TYPE i,
        c TYPE c LENGTH 5,
        d TYPE p LENGTH 4 DECIMALS 3,
      END OF struc_type.

    "Creating a type description object using RTTC method
    "Using the get method, you can create the type description object
    "dynamically based on a component table. The component table is of type
    "abap_component_tab. In this example, the component table is created inline.
    DATA(tdo_struc) = cl_abap_structdescr=>get(
        VALUE #(
          ( name = 'A' type = cl_abap_elemdescr=>get_string( ) )
          ( name = 'B' type = cl_abap_elemdescr=>get_i( ) )
          ( name = 'C' type = cl_abap_elemdescr=>get_c( 5 ) )
          ( name = 'D' type = cl_abap_elemdescr=>get_p( p_length = 4 p_decimals = 3 ) ) ) ).

    "... internal table types
    "Note: Specifying the line type is mandatory, the rest is optional.

    "An internal table type such as the following shall be created using a
    "type description object.
    TYPES std_tab_type_std_key TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    "Creating a type description object using RTTC method
    "Not specifying the other optional parameters means that the
    "default values are used, for example, standard table is the
    "default value for p_table_kind.
    DATA(tdo_tab_1) = cl_abap_tabledescr=>get(
            p_line_type  = cl_abap_elemdescr=>get_string( ) ).

    "Another internal table type for which more parameter specifications are needed
    "The following internal table type shall be created using a type description object.
    TYPES so_table_type TYPE SORTED TABLE OF zdemo_abap_flsch WITH UNIQUE KEY carrid connid.

    "Creating a type description object using RTTC method
    "The following example also demonstrates how comfortably constructor
    "operators can be used at these positions.
    DATA(tdo_tab_2) = cl_abap_tabledescr=>get(
            p_line_type  = CAST cl_abap_structdescr( cl_abap_tabledescr=>describe_by_name( 'ZDEMO_ABAP_FLSCH' ) )
            p_table_kind = cl_abap_tabledescr=>tablekind_sorted
            p_key        = VALUE #( ( name = 'CARRID' ) ( name = 'CONNID' ) )
            p_unique     = cl_abap_typedescr=>true ).

    " ... reference types
    "Reference types such as the following shall be created using a
    "type description object.
    TYPES some_ref_type2t TYPE REF TO t.
    TYPES some_ref_type2cl TYPE REF TO zcl_demo_abap_dynamic_prog.

    "Using RTTC methods
    "You can create a reference type from a base type. This base type
    "may be class, interface or data type.
    DATA(tdo_ref_1) = cl_abap_refdescr=>get( cl_abap_elemdescr=>get_t( ) ).
    DATA(tdo_ref_2) = cl_abap_refdescr=>get( cl_abap_typedescr=>describe_by_name( 'ZCL_DEMO_ABAP_DYNAMIC_PROG' ) ).
    "Alternative: get_by_name method
    DATA(tdo_ref_3) = cl_abap_refdescr=>get_by_name( 'T' ).
    DATA(tdo_ref_4) = cl_abap_refdescr=>get_by_name( 'ZCL_DEMO_ABAP_DYNAMIC_PROG' ).

    out->write( `No output for this section. You can set a break point and check the content of data objects in the debugger.` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `56) Dynamically Creating Data Objects at Runtime Using Type Description Objects (1) - Miscellaneous` ) ).

    "Using the TYPE HANDLE addition to CREATE DATA statements, you can
    "dynamically create data objects at runtime based on type description objects.
    "The following example uses type description objects from the previous example.
    "For output purposes, the created data objects are assigned values.

    DATA dref_typ_obj TYPE REF TO data.

    "Elementary data object
    CREATE DATA dref_typ_obj TYPE HANDLE tdo_elem_i.
    dref_typ_obj->* = 5 + 4.

    out->write( data = dref_typ_obj->* name = `dref_typ_obj->*` ).
    out->write( |\n| ).

    "Structured data object
    CREATE DATA dref_typ_obj TYPE HANDLE tdo_struc.
    dref_typ_obj->('A') = `hello`.
    dref_typ_obj->('B') = 4 + 3.
    dref_typ_obj->('C') = 'abcde'.
    dref_typ_obj->('D') = '1.234'.

    out->write( data = dref_typ_obj->* name = `dref_typ_obj->*` ).
    out->write( |\n| ).

    "Internal table
    CREATE DATA dref_typ_obj TYPE HANDLE tdo_tab_2.
    SELECT * FROM zdemo_abap_flsch INTO TABLE @dref_typ_obj->* UP TO 3 ROWS.

    out->write( data = dref_typ_obj->* name = `dref_typ_obj->*` ).
    out->write( |\n| ).

    "Reference
    CREATE DATA dref_typ_obj TYPE HANDLE tdo_ref_3.
    dref_typ_obj->* = NEW t( '120000' ).

    out->write( data = dref_typ_obj->* name = `dref_typ_obj->*` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `57) Dynamically Creating Data Objects at Runtime Using Type Description Objects (2) - Structure` ) ).

    "This example includes the dynamic definition of a structure with three components
    "using the GET method of the CL_ABAP_STRUCTDESCR class.

    DATA: struct_type TYPE REF TO cl_abap_structdescr,
          dref_struc  TYPE REF TO data.

    DATA column1 TYPE c LENGTH 5.
    DATA column2 TYPE c LENGTH 5.
    DATA column3 TYPE c LENGTH 5.

    "Potential component names
    DATA(comp_names) = VALUE string_table( ( `A` ) ( `B` ) ( `C` ) ( `D` ) ( `E` ) ( `F` ) ).

    "The structure should contain 3 components.
    DO 3 TIMES.

      "Getting a random integer that represents the table index
      "The line (the component name) is deleted from the table so as to
      "guarantee unique component names.
      DATA(num) = cl_abap_random_int=>create(
               seed = cl_abap_random=>seed( ) min = 1
                                              max = lines( comp_names ) )->get_next( ).
      CASE sy-index.
        WHEN 1.
          column1 = comp_names[ num ].
        WHEN 2.
          column2 = comp_names[ num ].
        WHEN 3.
          column3 = comp_names[ num ].
      ENDCASE.

      DELETE comp_names INDEX num.

    ENDDO.

    "All components should be typed with c length 3
    struct_type = cl_abap_structdescr=>get(
              VALUE #(
                ( name = column1 type = cl_abap_elemdescr=>get_c( 3 ) )
                ( name = column2 type = cl_abap_elemdescr=>get_c( 3 ) )
                ( name = column3 type = cl_abap_elemdescr=>get_c( 3 ) ) ) ).

    "Creating structured data object
    CREATE DATA dref_struc TYPE HANDLE struct_type.

    "Assigning values to the structure components
    dref_struc->(column1) = 'abc'.
    dref_struc->(column2) = 'def'.
    dref_struc->(column3) = 'ghi'.

    out->write( data = dref_struc->* name = `dref_struc->*` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `58) Dynamically Creating Data Objects at Runtime Using Type Description Objects (3) - Internal Table` ) ).

    "In the example an internal table type is created based on a DDIC type.
    "See the comments in the code.

    "Retrieving table name
    DATA(table_name) = lcl_det_at_runtime=>get_dyn_table_name( ).

    "Retrieving type information using RTTI
    DATA(st) =  CAST cl_abap_structdescr(
            cl_abap_tabledescr=>describe_by_name( table_name ) ).

    "Declaring an internal table to hold the components;
    "it will include the component name and the component type
    DATA comp_table TYPE cl_abap_structdescr=>component_table.

    "Looping across the retrieved field list to extract information
    "In principle, you could also just use method get_components( ) :)
    LOOP AT st->components ASSIGNING FIELD-SYMBOL(<field>).

      "Adding name of the component and its type, which is retrieved using the
      "get_component_type method, are added to the internal table that holds the components
      APPEND VALUE #( name = <field>-name
                      type = st->get_component_type( <field>-name ) ) TO comp_table.

      "The SELECT statement further down includes a dynamic specification
      "of the ORDER BY clause.
      "In this case, just using the second field since MANDT is the first.
      IF sy-tabix = 2.
        DATA(dyn_order_by) = <field>-name.
      ENDIF.

    ENDLOOP.

    "Creating an internal table type
    "Note: The parameter p_key is not filled here, i. e. the default key is used.
    DATA(itab_type) = cl_abap_tabledescr=>create(
        p_line_type  = st
        p_table_kind = cl_abap_tabledescr=>tablekind_sorted
        p_unique     = cl_abap_typedescr=>true ).

    "Creating an internal table based on the created table type
    DATA ref_tab TYPE REF TO data.
    CREATE DATA ref_tab TYPE HANDLE itab_type.

    "Filling an internal table
    SELECT *
      FROM (table_name)
      ORDER BY (dyn_order_by)
      INTO CORRESPONDING FIELDS OF TABLE @ref_tab->*
      UP TO 3 ROWS.

    out->write( |Type/Database table name determined at runtime: { table_name }| ).
    out->write( |\n| ).
    out->write( |Internal table entries (ordered by { dyn_order_by }):| ).
    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = ref_tab->* name = `ref_tab->*` ).

  ENDMETHOD.
ENDCLASS.
