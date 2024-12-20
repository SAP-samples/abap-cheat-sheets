"! <p class="shorttext"><strong>Dynamic programming</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates syntax and concepts related to dynamic programming.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li>Topics covered: Field symbols and data references (both as supporting elements for dynamic
"! programming), dynamic ABAP syntax components, runtime type services (RTTS), i.e., runtime type
"! identification (RTTI) and runtime type creation (RTTC).</li>
"! <li>To provide true dynamic determination at runtime for several code examples in this class,
"! the example class includes local classes in the CCIMP include (local types tab in ADT) whose
"! methods return character-like content for use in ABAP statements. The content is predefined
"! in these classes, but the actual content used is random.</li>
"! <li>Find information on <strong>getting started with the example class</strong> and the
"! <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</li></ul>
CLASS zcl_demo_abap_dynamic_prog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    CLASS-METHODS:
      class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS inst_meth1.
    METHODS inst_meth2 IMPORTING text          TYPE string
                       RETURNING VALUE(result) TYPE string.
    CLASS-METHODS stat_meth1.
    CLASS-METHODS stat_meth2 IMPORTING text   TYPE string
                             EXPORTING result TYPE string.
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

    out->write( zcl_demo_abap_aux=>no_output ).

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

    out->write( zcl_demo_abap_aux=>no_output ).

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

    out->write( zcl_demo_abap_aux=>no_output ).

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

    out->write( zcl_demo_abap_aux=>no_output ).

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
    out->write( |21) Dynamic ASSIGN Statements (1) - Specifying the Memory Area Dynamically\n\n| ).
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

    out->write( zcl_demo_abap_aux=>no_output ).

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

    "Note: For the static variant of the ASSIGN statement, i.e. if the memory area
    "to be assigned following the ASSIGN keyword is statically specified, the addition
    "ELSE UNASSIGN is implicitly set and cannot be used explicitly.
    DATA(hallo) = `Hallo world`.
    ASSIGN ('HALLO') TO FIELD-SYMBOL(<eu>) ELSE UNASSIGN.
    ASSERT sy-subrc = 0 AND <eu> IS ASSIGNED.
    ASSIGN ('DOES_NOT_EXIST') TO <eu> ELSE UNASSIGN.
    ASSERT sy-subrc = 4 AND <eu> IS NOT ASSIGNED.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `26a) Dynamic ASSIGN Statements (6) - Type Casting` ) ).

    "As covered above, the CASTING addition of the ASSIGN statement
    "has dynamic syntax elements.
    DATA dobj_c_l5 TYPE c LENGTH 5 VALUE 'abcde'.
    TYPES dtype_c_l2 TYPE c LENGTH 2.
    FIELD-SYMBOLS <fs_dyn_as> TYPE data.

    "A text literal with the name of a type is specified within the parentheses.
    ASSIGN dobj_c_l5 TO <fs_dyn_as> CASTING TYPE ('DTYPE_C_L2').

    out->write( data = <fs_dyn_as> name = `<fs_dyn_as1>` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `26b) Dynamic ASSIGN Statements (7) - Type Casting` ) ).

    "Assigning a data object to a field symbol casting a dynamically
    "specified type as also shown in the example above
    TYPES clen5 TYPE c LENGTH 5.
    DATA: dobj_c10    TYPE c LENGTH 10 VALUE '1234567890',
          some_struct TYPE zdemo_abap_fli.
    FIELD-SYMBOLS <casttype> TYPE data.

    ASSIGN dobj_c10 TO <casttype> CASTING TYPE ('CLEN5').  "12345
    ASSIGN dobj_c10 TO <casttype> CASTING LIKE some_struct-('CARRID'). "123

    TYPES: c1  TYPE c LENGTH 1,
           c3  TYPE c LENGTH 3,
           c10 TYPE c LENGTH 10,
           c20 TYPE c LENGTH 20,
           str TYPE string.
    DATA abc TYPE c LENGTH 26 VALUE 'abcdefghijklmnopqrstuvwxyz'.
    DATA(typenames) = VALUE string_table( ( `C1` ) ( `C3` ) ( `C10` ) ( `C20` ) ( `NOPE` ) ( `STR` ) ).
    DATA assignment_results TYPE string_table.
    FIELD-SYMBOLS <c_like> TYPE clike.

    LOOP AT typenames INTO DATA(typename).
      TRY.
          ASSIGN abc TO <c_like> CASTING TYPE (typename).
          assignment_results = VALUE #( BASE assignment_results ( |Type: '{ typename }'; Assignment result: '{ <c_like> }'| ) ).
        CATCH cx_root INTO DATA(error).
          assignment_results = VALUE #( BASE assignment_results
          ( |Error! Exception raised: { cl_abap_typedescr=>describe_by_object_ref( error )->get_relative_name( ) }; | &&
            |'{ error->get_text( ) }'| ) ).
      ENDTRY.
    ENDLOOP.

    out->write( data = assignment_results name = `assignment_results` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `26c) Dynamic ASSIGN Statements (8) - Type Casting` ) ).

    "Note: As covered further down CREATE DATA and ASSIGN statements have the HANDLE addition
    "after which dynamically created types can be specified. A type description object is expected.

    "Getting type description object
    DATA(tdo_elem) = cl_abap_elemdescr=>get_c( 4 ).
    ASSIGN dobj_c10 TO <casttype> CASTING TYPE HANDLE tdo_elem.

    out->write( data = <casttype> name = `<casttype>` ).

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

    out->write( zcl_demo_abap_aux=>heading( `32a) Internal Table (4)` ) ).

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

    out->write( zcl_demo_abap_aux=>heading( `32b) Creating Anonymous Data Objects by Specifying the Type Dynamically (Summarizing Syntax Patterns)` ) ).

    "------------ Specifying a type name dynamically ------------

    "Anonymous data objects are created using a type determined at
    "runtime. In this case, the name of the data type is specified
    "dynamically.
    "Note that the NEW operator cannot be used here.

    "Data reference variable used for the examples
    DATA data_ref TYPE REF TO data.

    "Example types and data objects

    "Elementary type and data object
    TYPES t_c3 TYPE c LENGTH 3.
    DATA c3 TYPE c LENGTH 3.

    "Structured type and data object
    TYPES t_fli_struc TYPE zdemo_abap_fli.
    DATA  fli_struc TYPE zdemo_abap_fli.

    "Table type and internal table
    TYPES t_carr_tab TYPE SORTED TABLE OF zdemo_abap_carr WITH UNIQUE KEY carrid.
    DATA carr_tab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.

    "Reference type and data reference variable
    TYPES t_str_ref TYPE REF TO string.
    DATA str_ref TYPE REF TO string.

    "----- Pattern: TYPE (typename) ... -----

    "Creating an elementary data object
    "Specifying a literal for the dynamic type name (used in most of the
    "following examples)
    CREATE DATA data_ref TYPE ('T_C3').

    "Specifying a named data object
    DATA(c3_type) = 'T_C3'.
    CREATE DATA data_ref TYPE (c3_type).

    "Structured data object
    CREATE DATA data_ref TYPE ('T_FLI_STRUC').

    "Internal table
    CREATE DATA data_ref TYPE ('T_CARR_TAB').

    "Data reference
    CREATE DATA data_ref TYPE ('T_STR_REF').

    "----- Pattern: TYPE ... TABLE OF (typename) ... -----

    "Creating internal tables

    CREATE DATA data_ref TYPE TABLE OF ('STRING').
    CREATE DATA data_ref TYPE TABLE OF ('T_FLI_STRUC') WITH EMPTY KEY.

    "Specifying the structured type dynamically, but the key values statically
    CREATE DATA data_ref TYPE SORTED TABLE OF ('ZDEMO_ABAP_CARR') WITH UNIQUE KEY carrid.

    "Specifying the structured type and the key values dynamically
    "An internal table such as the following should be created by dynamically
    "specifying the type and keys dynamically. The keys are specified in lines
    "of an internal table with character-like line type.
    DATA itab_compare TYPE SORTED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid connid fldate.

    DATA(key_table) = VALUE string_table( ( `CARRID` ) ( `CONNID` ) ( `FLDATE` ) ).
    CREATE DATA data_ref TYPE SORTED TABLE OF ('ZDEMO_ABAP_FLI') WITH UNIQUE KEY (key_table).

    "----- Pattern: TYPE REF TO (typename) -----

    "Creating data reference variables

    CREATE DATA data_ref TYPE REF TO ('STRING').
    CREATE DATA data_ref TYPE REF TO ('T_C3').
    CREATE DATA data_ref TYPE REF TO ('T_FLI_STRUC').
    CREATE DATA data_ref TYPE REF TO ('T_CARR_TAB').

    "----- Pattern: TYPE LINE OF (typename) -----

    "Creating structures based on table types
    CREATE DATA data_ref TYPE LINE OF ('T_CARR_TAB').

    "----- Pattern: LIKE struc-(dobjname) -----

    CREATE DATA data_ref LIKE fli_struc-('CARRID').

    "----- Pattern: TYPE (absolute_name) -----

    CREATE DATA data_ref TYPE ('\TYPE=STRING').
    "Getting an absolute type name; see more information further down
    DATA(absolute_name) = cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_CARR' )->absolute_name.
    CREATE DATA data_ref TYPE (absolute_name).

    "----- Pattern: TYPE HANDLE type_description_object -----

    "Getting a type description object. Find more information about RTTI below.
    DATA(typedescrobj) = cl_abap_elemdescr=>get_c( 4 ). "type c length 4
    CREATE DATA data_ref TYPE HANDLE typedescrobj.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `33a) Creating Instances of Classes by Specifying the Type Dynamically` ) ).

    DATA oref_dyn TYPE REF TO object.
    CREATE OBJECT oref_dyn TYPE ('ZCL_DEMO_ABAP_OBJECTS').

    DATA cl TYPE string VALUE `ZCL_DEMO_ABAP_OBJECTS`.
    CREATE OBJECT oref_dyn TYPE (cl).

    "Specifying a wrong/non-existent type name
    TRY.
        CREATE OBJECT oref_dyn TYPE ('THIS_CLASS_DOES_NOT_EXIST').
      CATCH cx_sy_create_object_error.
    ENDTRY.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `33b) Absolute Type Names for Dynamically Specifying Types` ) ).

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

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Dynamically Specifying Components/Clauses in Statements for Processing Internal Tables with ...` ) ).
    out->write( |34a) SORT (1)\n\n| ).

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

    out->write( zcl_demo_abap_aux=>heading( `34b) SORT (2)` ) ).


    "Sorting by dynamically specified components in a sort table, i. e.an
    "internal table of type abap_sortorder_tab.
    "Notes:
    "- Each line of this sort table specifies a component of the sort key.
    "- If this table is init_sortial, there is no sorting.
    "- The sort priorit_sorty is based on the order of the lines in the sort table.

    TYPES: BEGIN OF struct,
             comp1 TYPE i,
             comp2 TYPE string,
             comp3 TYPE c LENGTH 3,
           END OF struct.

    DATA it_sort TYPE TABLE OF struct WITH EMPTY KEY.

    it_sort = VALUE #( ( comp1 = 1 comp2 = `B` comp3 = 'a' )
                  ( comp1 = 1 comp2 = `A` comp3 = 'b' )
                  ( comp1 = 2 comp2 = `D` comp3 = 'c' )
                  ( comp1 = 2 comp2 = `C` comp3 = 'd' )
                  ( comp1 = 3 comp2 = `F` comp3 = 'e' )
                  ( comp1 = 3 comp2 = `E` comp3 = 'f' ) ).

    DATA(it_sort_original) = it_sort.

    "Note: The line type is abap_sortorder.
    DATA(sort) = VALUE abap_sortorder_tab( ).

    "No sorting because the sort table is init_sortial.
    SORT it_sort BY (sort).

    out->write( data = it_sort name = `it_sort` ).
    out->write( |\n| ).

    it_sort = it_sort_original.
    "Note: Ascending is the default sort order. The following example flags
    "the descending sort order explicit_sortly.
    sort = VALUE abap_sortorder_tab( ( name = `COMP1` descending = 'X' ) ).
    SORT it_sort BY (sort).

    out->write( data = it_sort name = `it_sort` ).
    out->write( |\n| ).

    it_sort = it_sort_original.

    sort = VALUE abap_sortorder_tab( ( name = `COMP1` descending = '' )
                                      ( name = `COMP2` descending = 'X' ) ).
    SORT it_sort BY (sort).

    out->write( data = it_sort name = `it_sort` ).
    out->write( |\n| ).

    it_sort = it_sort_original.

    "Sort priorit_sorty based on the order of lines in the sort table
    "In this example, the values of comp3 are set up so that a clear
    "sort order is determined. Since the component is specified first in the
    "sort table, this sorting has priorit_sorty. Note the values of comp2 in the
    "result table.
    sort = VALUE abap_sortorder_tab( ( name = `COMP3` descending = 'X' )
                                      ( name = `COMP2` descending = 'X' ) ).
    SORT it_sort BY (sort).

    out->write( data = it_sort name = `it_sort` ).
    out->write( |\n| ).

    "Specifying an invalid component name raises an exception
    sort = VALUE abap_sortorder_tab( ( name = `XYZ` descending = 'X' ) ).

    TRY.
        SORT it_sort BY (sort).
      CATCH cx_sy_dyn_table_ill_comp_val INTO DATA(err).
    ENDTRY.
    ASSERT err IS NOT INITIAL.

    it_sort = it_sort_original.
    "Specifying an expression/functional method call whose result is a sort
    "table of type abap_sortorder_tab
    "In this case, BY is followed by the expression/functional method call,
    "not enclosed in parentheses.

    "The example shows expressions wit_sorth sort tables created inline
    SORT it_sort BY VALUE abap_sortorder_tab( ( name = `COMP1` descending = 'X' ) ).

    out->write( data = it_sort name = `it_sort` ).
    out->write( |\n| ).

    it_sort = it_sort_original.

    DATA(compnames) = VALUE string_table( ( `comp1` ) ( `comp2` ) ).

    SORT it_sort BY VALUE abap_sortorder_tab( FOR sortwa IN compnames ( name = condense( to_upper( sortwa ) ) ) ).

    out->write( data = it_sort name = `it_sort` ).
    out->write( |\n| ).

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

    out->write( zcl_demo_abap_aux=>no_output ).

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

    out->write( zcl_demo_abap_aux=>no_output ).

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

    out->write( zcl_demo_abap_aux=>no_output ).

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
    out->write( |\n| ).

    "--------------------- Dynamic UPDATE ... SET ... statement ---------------------

    "Inserting demo data into the database table to work with
    TYPES carr_tab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
    INSERT ('ZDEMO_ABAP_CARR') FROM TABLE @( VALUE carr_tab( ( carrid = 'WX' carrname = 'WX Airways' )
                                                             ( carrid = 'XY' carrname = 'Air XY' )
                                                             ( carrid = 'YZ' carrname = 'YZ Airlines' ) ) ).

    "Note that erroneous dynamic specifications can lead to runtime errors
    "In the following example, the final inverted comma is missing in the dynamic
    "set clause.
    DATA(set_clause) = `CURRCODE = 'EUR`.
    DATA(where_cl) = `CARRID = 'WX' OR CARRID = 'XY' OR CARRID = 'YZ'`.

    TRY.
        UPDATE ('ZDEMO_ABAP_CARR') SET (set_clause) WHERE (where_cl).
      CATCH cx_sy_dynamic_osql_syntax INTO DATA(err_sql).
        DATA(error_text) = err_sql->get_text( ).
    ENDTRY.

    "Correcting the dynamic specification
    "The example sets the value for a component for all entries.
    "The example additionally specifies a (dynamic) WHERE clause
    "to restrict the range of entries where the update is performed.
    "The database table is also specified dynamically.
    set_clause = `CURRCODE = 'EUR'`.

    UPDATE ('ZDEMO_ABAP_CARR') SET (set_clause) WHERE (where_cl).

    SELECT *
      FROM ('ZDEMO_ABAP_CARR')
      INTO TABLE NEW @DATA(res5).
    out->write( data = res5->* name = `res5->*` ).
    out->write( |\n| ).

    "--------------------- Dynamic UPDATE ... INDICATORS ... statement ---------------------

    "The statement changes values of specific fields without overwriting existing values of
    "other fields.

    "Notes on the example:
    "- A structured type is created with the WITH INDICATORS addition.
    "- An internal table from which to update a database table is created.
    "- The table includes the indicator structure comp_ind.
    "- The table is populated, and two components are flagged as
    "  to be updated.
    "- Other fields remain unchanged. Note that key fields must be
    "  included in ind_tab (indicator setting for key fields has
    "  no effect).
    "- The UPDATE statement includes dynamically specified
    "  indicator syntax. Additionally, the database table is specified
    "  dynamically.

    "Structured type with WITH INDICATORS addition
    TYPES ind_wa TYPE zdemo_abap_carr WITH INDICATORS comp_ind TYPE abap_bool.

    DATA ind_tab TYPE TABLE OF ind_wa.

    "Filling internal table; only CURRCODE and URL should be updated
    ind_tab = VALUE #( ( carrid = 'WX'
                          carrname = 'WX Airways'
                          currcode = 'USD'
                          url = 'some_url_wx'
                          comp_ind-currcode = abap_true
                          comp_ind-url = abap_true )
                        ( carrid = 'XY'
                          carrname = 'Air XY'
                          currcode = 'USD'
                          url = 'some_url_xy'
                          comp_ind-currcode = abap_true
                          comp_ind-url = abap_true )
                        ( carrid = 'YZ'
                          carrname = 'YZ Airlines'
                          currcode = 'USD'
                          url = 'some_url_yz'
                          comp_ind-currcode = abap_true
                          comp_ind-url = abap_true ) ).

    DATA(dyn_ind) = `SET STRUCTURE comp_ind`.

    UPDATE ('ZDEMO_ABAP_CARR') FROM TABLE @ind_tab INDICATORS (dyn_ind).

    SELECT *
      FROM ('ZDEMO_ABAP_CARR')
      INTO TABLE NEW @DATA(res6).
    out->write( data = res6->* name = `res6->*` ).

    DELETE FROM ('ZDEMO_ABAP_CARR') WHERE (where_cl).

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
        CATCH cx_abap_not_a_table cx_abap_not_in_package INTO DATA(e).
          out->write( |Result for { wa_tab }:| ).
          out->write( e->get_text( ) ).
          out->write( |\n| ).
      ENDTRY.
    ENDLOOP.

    "In the following example, the check_allowlist method is used to check
    "whether the input is allowed or not. Similar to above, an internal
    "is filled with demo input. Here, a parameter is used that expects
    "a comma-separated list of allowed values.

    out->write( `****** Checking if input is allowed or not ******` ).
    out->write( |\n| ).

    DATA(to_be_checked) = VALUE string_table( ( `A` )
                                              ( `B` )
                                              ( `C` )
                                              ( `D` )
                                              ( `HALLO` )
                                              ( `ABAP` ) ).

    LOOP AT to_be_checked INTO DATA(chk).
      TRY.
          DATA(value) = cl_abap_dyn_prg=>check_allowlist( val      = chk
                                                          allowlist_str = `A,B,C,ABAP` ).
          out->write( |{ value } is allowed.| ).
        CATCH cx_abap_not_in_allowlist INTO e.
          out->write( e->get_text( ) ).
      ENDTRY.
    ENDLOOP.

    out->write( |\n| ).

    "The following example is similar to the one above. However, in this case,
    "a parameter is used that expects an internal table containing the allowed
    "values.

    LOOP AT to_be_checked INTO chk.
      TRY.
          value = cl_abap_dyn_prg=>check_allowlist( val      = chk
                                                    allowlist_htab = VALUE #( ( `X` )
                                                                              ( `B` )
                                                                              ( `HALLO` )
                                                                              ( `Y` )
                                                                              ( `ABAP` ) ) ).
          out->write( |{ value } is allowed.| ).
        CATCH cx_abap_not_in_allowlist INTO e.
          out->write( e->get_text( ) ).
      ENDTRY.
    ENDLOOP.

    out->write( |\n| ).
    out->write( `****** Checking if input can be a column name and does not contain invalid characters ******` ).
    out->write( |\n| ).

    "The following example uses a method with which the validity of column names
    "of database tables can be checked.

    to_be_checked = VALUE #( ( `CARRID` )
                             ( `CONNID` )
                             ( `SOME_COLUMN` )
                             ( `??NOPE??` )
                             ( `...!` ) ).

    LOOP AT to_be_checked INTO chk.
      TRY.
          DATA(col_name) = cl_abap_dyn_prg=>check_column_name( val    = chk
                                                               strict = abap_true ).
          out->write( |{ col_name } is allowed.| ).
        CATCH cx_abap_invalid_name INTO e.
          out->write( e->get_text( ) ).
      ENDTRY.
    ENDLOOP.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `52) Dynamic Method Calls (1)` ) ).

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
    out->write( |\n| ).

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

    out->write( zcl_demo_abap_aux=>heading( `52b) Dynamic Method Calls (2)` ) ).
    "Another example for dynamic invoke, using an instance method.

    "Example that uses the PARAMETER-TABLE addition
    "Creating an instance by specifying the type statically
    "An example class of the cheat sheet repository is used.
    DATA(oref1) = NEW zcl_demo_abap_objects( ).
    "Calling an instance method
    "The method multiplies an integer by 3.
    "The calculation result is returned.
    DATA(result) = oref1->triple( i_op = 2 ).

    out->write( data = result name = `result` ).
    out->write( |\n| ).

    "Dynamic equivalent
    "Creating an instance of a class by specifying the type
    "dynamically
    DATA oref2 TYPE REF TO object.
    CREATE OBJECT oref2 TYPE ('ZCL_DEMO_ABAP_OBJECTS').

    "Creating parameter table
    DATA(ptab) = VALUE abap_parmbind_tab( ( name  = 'I_OP'
                                            kind  = cl_abap_objectdescr=>exporting
                                            value = NEW i( 3 ) )
                                          ( name  = 'R_TRIPLE'
                                            kind  = cl_abap_objectdescr=>returning
                                            value = NEW i( ) ) ).

    "Dynamic method call and specifying a parameter table
    CALL METHOD oref2->('TRIPLE') PARAMETER-TABLE ptab.
    result = ptab[ name = 'R_TRIPLE' ]-('VALUE')->*.

    out->write( data = result name = `result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `52c) Dynamic Method Calls (Syntax Pattern Overview) (3)` ) ).

    "The following examples use both named and unnamed data objects randomly,
    "i.e. ...=>(meth_name) or ...=>(`SOME_METH`), for example.
    DATA(classname) = `ZCL_DEMO_ABAP_DYNAMIC_PROG`.
    DATA(meth_name1) = `STAT_METH1`.

    "------------------------------------------------------------------------
    "---------------- Calling static methods dynamically --------------------
    "------------------------------------------------------------------------

    "-------- Method without mandatory parameters defined --------
    "The syntax is possible for methods of the same class.
    CALL METHOD (meth_name1).

    "The previous example method call works like me->(meth).
    CALL METHOD me->(meth_name1).

    "-------- Class specified statically, method specified dynamically --------
    CALL METHOD zcl_demo_abap_dynamic_prog=>(meth_name1).

    "-------- Class specified dynamically, method specified statically --------
    CALL METHOD (`ZCL_DEMO_ABAP_DYNAMIC_PROG`)=>stat_meth1.

    "-------- Class and method specified dynamically --------
    CALL METHOD (`ZCL_DEMO_ABAP_DYNAMIC_PROG`)=>(`STAT_METH1`).

    "-------- Specifying non-optional parameters --------
    CALL METHOD (`ZCL_DEMO_ABAP_DYNAMIC_PROG`)=>stat_meth2 EXPORTING text = `hallo`.

    "Specifying the output parameter is optional
    DATA res TYPE string.
    CALL METHOD (`ZCL_DEMO_ABAP_DYNAMIC_PROG`)=>stat_meth2 EXPORTING text = `hallo` IMPORTING result = res.
    ASSERT res = `HALLO`.

    "-------- Some examples for handling errors when calling methods wrongly --------

    "Instance method called using =>
    TRY.
        CALL METHOD zcl_demo_abap_dynamic_prog=>(`INST_METH1`).
      CATCH cx_sy_dyn_call_illegal_method.
    ENDTRY.

    "The example method does not specify non-optional parameters.
    TRY.
        CALL METHOD (`ZCL_DEMO_ABAP_DYNAMIC_PROG`)=>stat_meth2.
      CATCH cx_sy_dyn_call_param_missing.
    ENDTRY.

    "Specifying a wrong parameter name
    TRY.
        CALL METHOD (`ZCL_DEMO_ABAP_DYNAMIC_PROG`)=>stat_meth2 EXPORTING hallo = `hallo`.
      CATCH cx_sy_dyn_call_param_missing.
    ENDTRY.

    "Assigning wrong, non-compatible type
    TRY.
        CALL METHOD (`ZCL_DEMO_ABAP_DYNAMIC_PROG`)=>stat_meth2 EXPORTING text = VALUE string_table( ( `hi` ) ).
      CATCH cx_sy_dyn_call_illegal_type.
    ENDTRY.

    "Specifying wrong parameter kinds (the example method specifies importing
    "and exporting parameters, and not a returning parameter)
    TRY.
        CALL METHOD (`ZCL_DEMO_ABAP_DYNAMIC_PROG`)=>stat_meth2 EXPORTING text = `hallo` RECEIVING result = res.
      CATCH cx_sy_dyn_call_illegal_type.
    ENDTRY.

    "------------------------------------------------------------------------
    "---------------- Calling instance methods dynamically ------------------
    "------------------------------------------------------------------------

    "Creating an instance of a class by specifying the type dynamically
    DATA object_ref TYPE REF TO object.
    CREATE OBJECT object_ref TYPE ('ZCL_DEMO_ABAP_DYNAMIC_PROG').

    "--- Object reference variable specified statically, method specified dynamically ---
    "Note: This is a also possible for interface reference variables.
    CALL METHOD object_ref->(`INST_METH1`).

    "-------- Specifying non-optional parameters --------
    CALL METHOD object_ref->(`INST_METH2`) EXPORTING text = `abap`.

    CALL METHOD object_ref->(`INST_METH2`) EXPORTING text = `abap` RECEIVING result = res.
    ASSERT res = `ABAP`.

    "Note that calling static methods using object reference variables is also possible.
    CALL METHOD object_ref->(`STAT_METH1`).

    CALL METHOD object_ref->(`STAT_METH2`) EXPORTING text = `test` IMPORTING result = res.
    ASSERT res = `TEST`.

    "------------------------------------------------------------------------
    "------------------- PARAMETER-TABLE addition ---------------------------
    "------------------------------------------------------------------------

    "------- Static equivalents to the dynamic statement below -------
    DATA(object_ref_stat) = NEW zcl_demo_abap_dynamic_prog( ).
    res = object_ref_stat->inst_meth2( `abc` ).
    ASSERT res = `ABC`.
    "For demo purposes, including chained method call options:
    "Functional method call
    res = NEW zcl_demo_abap_dynamic_prog( )->inst_meth2( `def` ).
    ASSERT res = `DEF`.
    "Standalone statement)
    NEW zcl_demo_abap_dynamic_prog( )->inst_meth2( EXPORTING text = `ghi` RECEIVING result = res ).
    ASSERT res = `GHI`.

    "------- Dynamic CALL METHOD statements using the PARAMETER-TABLE addition -------

    "Creating parameter table for an instance example method
    DATA(paramtab) = VALUE abap_parmbind_tab( ( name  = 'TEXT'
                                            kind  = cl_abap_objectdescr=>exporting
                                            value = NEW string( `jkl` ) )
                                          ( name  = 'RESULT'
                                            kind  = cl_abap_objectdescr=>returning
                                            value = NEW string( ) )
                                             ).

    CALL METHOD object_ref->(`INST_METH2`) PARAMETER-TABLE paramtab.
    "Excursion: Accessing structure components dynamically
    res = paramtab[ name = 'RESULT' ]-('VALUE')->*.
    ASSERT res = `JKL`.

    "Creating parameter table for a static example method
    paramtab = VALUE abap_parmbind_tab( ( name  = 'TEXT'
                                      kind  = cl_abap_objectdescr=>exporting
                                      value = NEW string( `mno` ) )
                                    ( name  = 'RESULT'
                                      kind  = cl_abap_objectdescr=>importing
                                      value = NEW string( ) ) ).

    "Demonstrating static/dynamic specification variants
    CALL METHOD (`ZCL_DEMO_ABAP_DYNAMIC_PROG`)=>(`STAT_METH2`) PARAMETER-TABLE paramtab.
    res = paramtab[ name = 'RESULT' ]-('VALUE')->*.
    ASSERT res = `MNO`.

    CALL METHOD zcl_demo_abap_dynamic_prog=>(`STAT_METH2`) PARAMETER-TABLE paramtab.
    res = paramtab[ name = 'RESULT' ]-('VALUE')->*.
    ASSERT res = `MNO`.

    CALL METHOD (`ZCL_DEMO_ABAP_DYNAMIC_PROG`)=>stat_meth2 PARAMETER-TABLE paramtab.
    res = paramtab[ name = 'RESULT' ]-('VALUE')->*.
    ASSERT res = `MNO`.

    out->write( zcl_demo_abap_aux=>no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `52d) Dynamic Formatting Option Specifications in String Templates` ) ).

    "ALIGN
    "Only to be used with WIDTH; only the associated values of the following attributes of the
    "class CL_ABAP_FORMAT can be used (they are of type i): A_LEFT (1), A_RIGHT (2), A_CENTER (3)
    DATA(demo_string) = `##`.
    DATA(s1) = |{ demo_string WIDTH = 10 ALIGN = (1) }<---|.

    out->write( data = s1 name = `s1` ).
    out->write( |\n| ).

    DATA(right) = 2.
    DATA(s2) = |{ demo_string WIDTH = 10 ALIGN = (right) }<---|.

    out->write( data = s2 name = `s2` ).
    out->write( |\n| ).

    "The following example uses method chaining with methods of the class
    "cl_abap_random_int to get a random integer value (in the range of 1 - 3).
    "The get_next method has a returning parameter, and returns an integer value.
    DO 5 TIMES.
      DATA(s3) = |{ demo_string WIDTH = 10 ALIGN = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                      min  = 1 max = 3 )->get_next( ) }<---|.
    ENDDO.

    out->write( data = s3 name = `s3` ).
    out->write( |\n| ).

    "CASE
    "Values to be used: CL_ABAP_FORMAT=>C_RAW (for not changing the case; 0),
    "CL_ABAP_FORMAT=>C_UPPER (1), CL_ABAP_FORMAT=>C_LOWER (2)
    demo_string = `AbAp`.
    DATA(s4) = |{ demo_string CASE = (1) }|.

    out->write( data = s4 name = `s4` ).
    out->write( |\n| ).

    DATA(s5) = |{ demo_string CASE = CONV i( '2' ) }|.

    out->write( data = s5 name = `s5` ).
    out->write( |\n| ).

    DATA int_tab TYPE TABLE OF i WITH EMPTY KEY.
    int_tab = VALUE #( ( 0 ) ( 1 ) ( 2 ) ).
    DATA(s6) = |{ demo_string CASE = int_tab[ 1 ] }|.

    out->write( data = s2 name = `s2` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `52e) Dynamic Parameter List in EXPORT and IMPORT Statements` ) ).

    DATA buffer TYPE xstring.

    "--------- Dynamic specification of the parameter list ---------
    "Note:
    "- The parameter list is specified in an index table with two columns.
    "- The column names can have random names, but the type must be character-like.
    "- The first column represents the parameter name, the second column represents
    "  the name of the data object.
    "- Note that special behavior applies. See the documentation.

    TYPES: BEGIN OF param,
             name TYPE string,
             dobj TYPE string,
           END OF param,
           param_tab_type TYPE TABLE OF param WITH EMPTY KEY.

    DATA: txt1 TYPE string VALUE `hello`,
          txt2 TYPE string VALUE `world`,
          txt3 TYPE string VALUE `ABAP`.

    DATA(param_table) = VALUE param_tab_type(
      ( name = `txt1` dobj = `txt1` )
      ( name = `txt2` dobj = `txt2` )
      ( name = `txt3` dobj = `txt3` ) ).

    EXPORT (param_table) TO DATA BUFFER buffer.

    "The example reads the content into structure components.
    DATA: BEGIN OF values,
            txt1 TYPE string,
            txt2 TYPE string,
            txt3 TYPE string,
          END OF values.

    param_table = VALUE param_tab_type(
      ( name = `txt1` dobj = `values-txt1` )
      ( name = `txt2` dobj = `values-txt2` )
      ( name = `txt3` dobj = `values-txt3` ) ).

    IMPORT (param_table) FROM DATA BUFFER buffer.

    out->write( data = values name = `values` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `52f) Security Considerations in Dynamic Programming Using External Input` ) ).

    "Filling demo database tables of the ABAP cheat sheet repository
    zcl_demo_abap_aux=>fill_dbtabs( ).

    "--------------------------------------------------------------------
    "--- Specifying the data object holding external input as operand ---
    "--- and literal ----------------------------------------------------
    "--------------------------------------------------------------------

    "The example explores a dynamic WHERE clause. External content is used
    "in the WHERE clause, unchecked.

    "Assuming the data object 'input' holds external input inserted on a UI.
    DATA(input) = 'LH'.

    "Inserting the input value into a dynamic WHERE clause as literal
    DATA(cond1) = `CARRID = '` && input && `'`.
    SELECT SINGLE * FROM zdemo_abap_fli WHERE (cond1) INTO @DATA(db_entry).

    out->write( data = db_entry name = `db_entry` ).
    out->write( |\n\n| ).

    "Inserting the input value into a dynamic WHERE clause using the data
    "object name
    DATA(cond2) = `CARRID = @input`.
    SELECT SINGLE * FROM zdemo_abap_fli WHERE (cond2) INTO @db_entry.

    out->write( data =  db_entry name = `db_entry` ).
    out->write( |\n\n| ).

    "Assuming bad input is provided that is unchecked
    DATA(bad_input) = |LH' AND CONNID = '401|.

    "Inserting the input value as literal
    "Because of using the value as literal, the WHERE clause
    "can be manipulated, yielding a potentially different
    "result, thus posing a security risk.
    DATA(cond3) = `CARRID = '` && bad_input && `'`.
    SELECT SINGLE * FROM zdemo_abap_fli WHERE (cond3) INTO @db_entry.

    out->write( data =  db_entry name = `db_entry` ).
    out->write( |\n\n| ).

    "Inserting the input value using the data object name
    "In doing so, the WHERE clause becomes erroneous, the ABAP
    "SQL statement cannot be executed.
    DATA(cond4) = `CARRID = @bad_input`.
    TRY.
        SELECT SINGLE * FROM zdemo_abap_fli WHERE (cond4) INTO @db_entry.
        out->write( data =  db_entry name = `db_entry` ).
      CATCH cx_sy_dynamic_osql_error cx_sy_open_sql_data_error INTO DATA(select_error).
        out->write( select_error->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).
    out->write( |{ repeat( val = `*` occ = 70 ) }| ).

    "--------------------------------------------------------------------
    "------------ Accessing not allowed database tables -----------------
    "--------------------------------------------------------------------
    "Assume the name of a database table is specified externally, and a
    "dynamic ABAP SQL statement uses this name. Potentially, users that
    "are actually not allowed to access the database table may get access.
    "The example uses the CL_ABAP_DYN_PRG class that checks a list of
    "allowed database tables.

    "The following methods check ...
    "- Database table names
    "- Whether the database table is contained in a/certain package/s
    "Assuming you provide incorrect input for the table name, or
    "the table is not contained in the specified packages, you should
    "expect an exception to be raied.

    "Assuming the following data object contains external input
    DATA(input_dbtab_name) = `zdemo_abap_fli`.

    "check_table_name_str method: Specifying a single package
    TRY.
        DATA(databasetable) = cl_abap_dyn_prg=>check_table_name_str(
          val      = to_upper( input_dbtab_name )
          packages = `ZABAP_CHEAT_SHEETS` ).

        SELECT SINGLE * FROM (databasetable) INTO NEW @DATA(ref_db_entry).
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_a_table cx_abap_not_in_package INTO DATA(error_input_dbtab1).
        out->write( error_input_dbtab1->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).

    "check_table_name_tab method: Specifying multiple packages in an internal
    "table
    TRY.
        dbtab = cl_abap_dyn_prg=>check_table_name_tab(
          val      = to_upper( input_dbtab_name )
          packages = VALUE #( ( `ZABAP_CHEAT_SHEETS` )
                              ( `ZSOME_PACKAGE` ) ) ).

        SELECT SINGLE * FROM (dbtab) INTO NEW @ref_db_entry.
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_a_table cx_abap_not_in_package INTO DATA(error_input_dbtab2).
        out->write( error_input_dbtab2->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).

    "Not existant database table/invalid name
    input_dbtab_name = `not_a_dbtab!!`.
    TRY.
        dbtab = cl_abap_dyn_prg=>check_table_name_tab(
          val      = to_upper( input_dbtab_name )
          packages = VALUE #( ( `ZABAP_CHEAT_SHEETS` )
                              ( `ZSOME_PACKAGE` ) ) ).

        SELECT SINGLE * FROM (dbtab) INTO NEW @ref_db_entry.
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_a_table cx_abap_not_in_package INTO DATA(error_input_dbtab3).
        out->write( error_input_dbtab3->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).

    "Database table not existant in packages specified (assuming you have imported
    "the ABAP cheat sheet repository, and the database table is available)
    input_dbtab_name = `zdemo_abap_fli`.
    TRY.
        dbtab = cl_abap_dyn_prg=>check_table_name_tab(
          val      = to_upper( input_dbtab_name )
          packages = VALUE #( ( `SAP_BASIS` ) ) ).

        SELECT SINGLE * FROM (dbtab) INTO NEW @ref_db_entry.
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_a_table cx_abap_not_in_package INTO DATA(error_input_dbtab4).
        out->write( error_input_dbtab4->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).
    out->write( |{ repeat( val = `*` occ = 70 ) }| ).

    "--------------------------------------------------------------------
    "------------ Verifying input against a given allowlist  ------------
    "--------------------------------------------------------------------

    "Assume a SELECT statement dynamically specifies the column names
    "in the SELECT list. Table columns might be accessed although
    "they should not be.
    "You may check against an allowlist.

    "check_allowlist method
    "In the following examples, a method is used to check whether
    "the input is allowed or not. For this, you specify an allowlist.
    "Here, the relevant parameter expects a comma-separated list of
    "allowed values.

    "Assuming the following data object contains external input
    DATA(input_col_name) = `carrid`.

    TRY.
        DATA(value1) = cl_abap_dyn_prg=>check_allowlist(
            val           = to_upper( input_col_name )
            allowlist_str = `CARRID,CONNID,FLDATE` ).

        SELECT SINGLE (input_col_name) FROM zdemo_abap_fli INTO NEW @ref_db_entry.
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_in_allowlist INTO DATA(error_allowed1).
        out->write( error_allowed1->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).

    "The allowlist_htab formal parameter expects an internal table.
    input_col_name = `price`.
    TRY.
        DATA(value2) = cl_abap_dyn_prg=>check_allowlist(
            val           = to_upper( input_col_name )
            allowlist_htab = VALUE #( ( `CARRID` )
                                      ( `CONNID` )
                                      ( `FLDATE` ) ) ).

        SELECT SINGLE (input_col_name) FROM zdemo_abap_fli INTO NEW @ref_db_entry.
        out->write( data =  ref_db_entry name = `ref_db_entry` ).
      CATCH cx_abap_not_in_allowlist INTO DATA(error_allowed2).
        out->write( error_allowed2->get_text( ) ).
    ENDTRY.
    out->write( |\n\n| ).
    out->write( |{ repeat( val = `*` occ = 70 ) }| ).

    "--------------------------------------------------------------------
    "------------ Potential manipulation of ABAP SQL clauses ------------
    "--------------------------------------------------------------------

    "In the following example, a dynamic WHERE clause is set up. For this,
    "it is assumed that the WHERE clause uses external input via input fields.
    "This is represented by the column and value data objects. It is assumed
    "that column holds the name of the table column, value a dedicated value in
    "the specified table column.
    "The cl_abap_dyn_prg class is used to check content in two ways:
    "- Checking if the provided column name is valid using the check_column_name
    "  method.
    "- Using the quote method for putting single quotes around the value and escaping
    "  single quotes.
    "In a DO loop, various example inputs are explored. The fourth loop pass includes
    "bad input without using the quote method. This way, an SQL injection takes
    "place, yielding a different result. In this case, all database table entries
    "are retrieved because the WHERE clause is as follows:
    "CARRID = 'LH' OR CARRID <> 'LH'.
    "This is prevented using the quote method, resulting in a non-functional SELECT
    "statement.

    DATA: column TYPE c LENGTH 30,
          val    TYPE c LENGTH 30.

    DO 4 TIMES.
      CASE sy-index.
        WHEN 1.
          "Working example
          column = 'carrid'.
          val = 'lh'.
        WHEN 2.
          "Invalid column name
          column = '?=('.
          val = 'lh'.
        WHEN 3.
          "Bad input, using cl_abap_dyn_prg
          column = 'carrid'.
          val = |'LH' OR CARRID <> 'LH'|.

        WHEN 4.
          "Bad input, not using cl_abap_dyn_prg
          column = 'carrid'.
          val = |'LH' OR CARRID <> 'LH'|.

      ENDCASE.

      out->write( |---------- Run { sy-index } ----------| ).

      TRY.
          cl_abap_dyn_prg=>check_column_name( column ).
        CATCH cx_abap_invalid_name INTO DATA(error_col_name).
          out->write( error_col_name->get_text( ) ).
      ENDTRY.

      DATA(cond_syntax) = to_upper( column ) && ` = ` &&
      COND #( WHEN sy-index <> 4 THEN cl_abap_dyn_prg=>quote( to_upper( value ) ) ELSE to_upper( value ) ).

      TRY.
          SELECT *
                 FROM zdemo_abap_flsch
                 WHERE (cond_syntax)
                 INTO TABLE @DATA(itab_flsch).

          out->write( itab_flsch ).
        CATCH cx_sy_dynamic_osql_error cx_sy_open_sql_data_error INTO DATA(error_select).
          out->write( error_select->get_text( ) ).
      ENDTRY.

      out->write( |\n\n| ).
    ENDDO.

    "Example manipulating the SET clause in an UPDATE statement
    "Inserting a database table entry to work with in the example
    INSERT zdemo_abap_carr FROM @( VALUE #( carrid = 'XY' carrname = 'XY Airways' currcode = 'EUR' url = 'some_url'  ) ).
    SELECT SINGLE * FROM zdemo_abap_carr WHERE carrid = 'XY' INTO @DATA(row4update).

    out->write( data =  row4update name = `row4update` ).
    out->write( |\n\n| ).

    "Assuming the carrier name is to be changed (that was previously created and retrieved
    "for demo purposes). The carrier name is provided via external input, represented by
    "the following data object assignment.
    DATA(input_carrname) = 'Air XY'.

    "Specifying a potentially dangerous dynamic SET clause by directly using external
    "input in the clause
    DATA(dyn_set_clause) = `CARRNAME = '` && input_carrname && `'`.

    UPDATE zdemo_abap_carr
      SET (dyn_set_clause)
      WHERE carrid = @row4update-carrid.

    SELECT SINGLE * FROM zdemo_abap_carr WHERE carrid = 'XY' INTO @row4update.
    out->write( data =  row4update name = `row4update` ).
    out->write( |\n\n| ).

    "Bad input, not using cl_abap_dyn_prg
    "In the example, the input is manipulated in a way that also changes
    "another field value.
    DATA(bad_input_carrname) = |XY Airways', URL = '#########|.
    dyn_set_clause = `CARRNAME = '` && bad_input_carrname && `'`.

    UPDATE zdemo_abap_carr
      SET (dyn_set_clause)
      WHERE carrid = @row4update-carrid.

    SELECT SINGLE * FROM zdemo_abap_carr WHERE carrid = 'XY' INTO @row4update.
    out->write( data =  row4update name = `row4update` ).
    out->write( |\n\n| ).

    "Bad input, using cl_abap_dyn_prg
    "Undoing the changes for the demo database table row
    MODIFY zdemo_abap_carr FROM @( VALUE #( carrid = 'XY' carrname = 'XY Airways' currcode = 'EUR' url = 'some_url' ) ).
    SELECT SINGLE * FROM zdemo_abap_carr WHERE carrid = 'XY' INTO @row4update.

    bad_input_carrname = |XY Airways', URL = '#########|.
    dyn_set_clause = `CARRNAME = ` && cl_abap_dyn_prg=>quote( bad_input_carrname ).

    TRY.
        UPDATE zdemo_abap_carr
          SET (dyn_set_clause)
          WHERE carrid = @row4update-carrid.
      CATCH cx_sy_open_sql_data_error INTO DATA(error_set).
        out->write( error_set->get_text( ) ).
    ENDTRY.

    out->write( |{ repeat( val = `*` occ = 70 ) }| ).

    "--------------------------------------------------------------------
    "---------------------------- Escaping ------------------------------
    "--------------------------------------------------------------------

    "In various contexts, a replacement of special characters may be important.
    "Such an escaping is applied on characters contained in a string according
    "to a set of rules.

    "The following example deals with Cross Site Scripting, e.g. manipulating
    "HTML pages and embedding scripts displayed in a browser. In ABAP, this
    "enters the picture, for example, when directly dealing with the Internet
    "Communication Framework.
    "The built-in function escape can be used to escape content in various contexts.
    "The cl_abap_dyn_prg class also offers methods to escape. However, the function
    "is recommended due to performance reasons.

    "Assuming building HTML code by using external input
    DATA your_name TYPE string.
    your_name = sy-uname.
    DATA(html) = `<p>Hello ` && your_name && `!</p>`.

    out->write( data =  html name = `html` ).
    out->write( |\n\n| ).

    "Embedding potentially malicious scripts into the code
    your_name = `<script>alert("Hmmm... potentially malicious code!");</script>`.
    html = `<p>Hello ` && your_name && `!</p>`.

    "Inserted this in an HTML and run in a browser, an alert will be displayed.
    out->write( data =  html name = `html` ).
    out->write( |\n\n| ).

    "Escaping may be done as follows
    "Check the various methods available for escaping with cl_abap_dyn_prg, as well as
    "the formats in the context of the escape function
    DATA(esc_js_cl) = `<p>Hello ` && cl_abap_dyn_prg=>escape_xss_javascript( html ) && `!</p>`.

    "Using the built-in function escape
    DATA(esc_js_fu) = `<p>Hello ` && escape( val = html format = cl_abap_format=>e_xss_js ) && `!</p>`.

    "Further character handling and escaping examples using the cl_abap_dyn_prg class
    DATA(quote) = |10 o'clock|.
    DATA(handle_quotes) = cl_abap_dyn_prg=>quote( quote ).
    DATA(backtick) = |The character ` is a backtick|.
    DATA(handle_backtick) = cl_abap_dyn_prg=>quote_str( backtick ).
    DATA(esc_quotes) = cl_abap_dyn_prg=>escape_quotes( quote ).
    DATA(esc_backticks) = cl_abap_dyn_prg=>escape_quotes_str( backtick ).
    "You may also do the escaping using string processing techniques, e.g.
    "using the replace function.
    DATA(esc_quotes_replace) = replace( val = quote sub = |'| with = |''| occ = 0 ).
    DATA(esc_backticks_replace) = replace( val = backtick sub = |`| with = |``| occ = 0 ).

    out->write( data =  esc_js_cl name = `esc_js_cl` ).
    out->write( data =  esc_js_fu name = `esc_js_fu` ).
    out->write( data =  handle_quotes name = `handle_quotes` ).
    out->write( data =  handle_backtick name = `handle_backtick` ).
    out->write( data =  esc_quotes name = `esc_quotes` ).
    out->write( data =  esc_backticks name = `esc_backticks` ).
    out->write( data =  esc_quotes_replace name = `esc_quotes_replace` ).
    out->write( data =  esc_backticks_replace name = `esc_backticks` ).

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

    out->write( data = tab_kind name = `tab_kind` ).
    out->write( |\n| ).
    out->write( data = tab_keys name = `tab_keys` ).
    out->write( |\n| ).
    out->write( data = tab_keys2 name = `tab_keys2` ).
    out->write( |\n| ).
    out->write( data = tab_comps name = `tab_comps` ).
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

    out->write( zcl_demo_abap_aux=>heading( `54a) RTTI: Getting Type Information at Runtime for Miscellaneous Types` ) ).

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

    out->write( zcl_demo_abap_aux=>heading( `54b) RTTI: Getting Type Information at Runtime for Miscellaneous Types` ) ).

    "Data objects to work with in the example
    DATA itab_refs TYPE TABLE OF REF TO data.
    DATA str_tab TYPE string_table.
    DATA dyn_dobj TYPE REF TO data.
    DATA dyn_obj TYPE REF TO object.
    DATA typdeobj TYPE REF TO cl_abap_typedescr.

    "Data objects of different kinds based on which type information shall be retrieved
    "Elementary type
    DATA elem_dobj TYPE c LENGTH 4 VALUE 'ABAP'.

    "Enumerated type
    TYPES: BEGIN OF ENUM enum_t,
             enum1,
             enum2,
             enum3,
           END OF ENUM enum_t.
    DATA(dobj_enum) = enum2.

    "demo_structured types
    DATA(demo_struct) = VALUE zdemo_abap_carr( carrid = 'XY' carrname = 'XY Airlines' ).
    "BDEF derived type (demo_structure)
    DATA demo_struct_rap TYPE STRUCTURE FOR CREATE zdemo_abap_rap_ro_m.

    "Internal table types
    "Standard table with standard table key
    DATA(string_table) = VALUE string_table( ( `AB` ) ( `AP` ) ).
    "Local demo_structured type as basis for a sorted internal table that
    "includes primary and secondary table key specifiactions (including
    "an alias name)
    TYPES: BEGIN OF struc_type,
             a TYPE c LENGTH 3,
             b TYPE i,
             c TYPE decfloat34,
           END OF struc_type.
    TYPES demo_tab_type TYPE SORTED TABLE OF struc_type
      WITH UNIQUE KEY a
      WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS b c .
    DATA(demo_sorted_tab) = VALUE demo_tab_type( ( a = 'aaa' ) ).

    "Reference variables
    "Data reference variable
    DATA(demo_dref) = NEW i( 123 ).
    "Object reference variable
    DATA(demo_oref) = NEW zcl_demo_abap_objects( ).
    "Interface reference variable
    DATA demo_iref TYPE REF TO zdemo_abap_objects_interface.
    demo_iref = CAST #( demo_oref ).

    "Adding the previous (data) objects to an internal table which is
    "looped over to retrieve type information for all
    itab_refs = VALUE #( ( REF #( elem_dobj ) ) "elementary type (1)
                         ( REF #( dobj_enum ) ) "enumerated type (2)
                         ( REF #( demo_struct ) ) "flat demo_structure (3)
                         ( REF #( demo_struct_rap ) ) "demo_structure typed with BDEF derived type (4)
                         ( REF #( string_table ) ) "internal table, elementary line type (5)
                         ( REF #( demo_sorted_tab ) ) "internal table, local line type (6)
                         ( REF #( demo_dref ) ) "data reference variable (7)
                         ( REF #( demo_oref ) ) "object reference variable (8)
                         ( REF #( demo_iref ) ) "interface reference variable (9)
                      ).


    LOOP AT itab_refs INTO DATA(type).
      DATA(tabix) = sy-tabix.
      TRY.
          "The reference returned points to an object from the class CL_ABAP_CLASSDESCR
          typdeobj = cl_abap_typedescr=>describe_by_object_ref( type->* ).
        CATCH cx_sy_dyn_call_illegal_type.
          "The reference returned points to an object from the class CL_ABAP_DATADESCR
          typdeobj = cl_abap_typedescr=>describe_by_data( type->* ).
      ENDTRY.

      "----------------- Exploring general type information -----------------
      "At this stage, with using the static methods above, you already get general type
      "information such as the type kind or the abosulte name. Check the type description
      "object in the debugger for more attributes.
      "When performing a down cast to more specific classes, you can access special
      "methods of the type object and get more detailed information.

      "Getting the type kind
      "For the constant values of type abap_typekind, see cl_abap_typedescr. For example, 'h'
      "stands for internal table.
      DATA(type_kind) = typdeobj->type_kind.
      INSERT |{ tabix } Type kind: { type_kind }| INTO TABLE str_tab.

      "Type category
      "For the constant values of type abap_typecategory, see cl_abap_typedescr.
      "C (class), E (elementary), I (interface), R (Reference), S (demo_structure), T (table)
      DATA(type_category) = typdeobj->kind.
      INSERT |{ tabix } Type category: { type_category }| INTO TABLE str_tab.

      "Absolute name (used later for dynamic (data) object creation)
      "Note: In ABAP for Cloud Development, absolute names having the pattern \TYPE=%_...
      "cannot be used to create (data) objects dynamically.
      DATA(absolutename) = typdeobj->absolute_name.
      INSERT |{ tabix } Absolute name: { absolutename }| INTO TABLE str_tab.

      "Relative name
      "Types that are implicitly defined (e.g. created using DATA) do not have a relative
      "type name. Explicitly defined types are, for example, standard ABAP types, Dictionary
      "types, classes and interfaces.
      DATA(relative_name) = typdeobj->get_relative_name( ).
      IF relative_name IS NOT INITIAL.
        INSERT |{ tabix } Relative name: { relative_name }| INTO TABLE str_tab.
      ENDIF.

      "Checking if it is a DDIC type
      DATA(is_ddic_type) = typdeobj->is_ddic_type( ).
      IF is_ddic_type IS NOT INITIAL.
        INSERT |{ tabix } Is DDIC type: "{ is_ddic_type }"| INTO TABLE str_tab.
      ENDIF.

      "----------------- Exploring more specific information by casting -----------------
      "For checking the type before performing the cast, you can use statements with
      "CASE TYPE OF and IS INSTANCE. The example demonstrates both options.

      CASE TYPE OF typdeobj.

        WHEN TYPE cl_abap_datadescr.
          INSERT |{ tabix } Is instance of cl_abap_datadescr| INTO TABLE str_tab.

          "-----------------------------------------------------------------------
          "----------------------- Elementary types ------------------------------
          "-----------------------------------------------------------------------
          IF typdeobj IS INSTANCE OF cl_abap_elemdescr.
            INSERT |{ tabix } Is instance of cl_abap_elemdescr| INTO TABLE str_tab.

            "Enumerated types
            IF typdeobj IS INSTANCE OF cl_abap_enumdescr.
              INSERT |{ tabix } Is instance of cl_abap_enumdescr| INTO TABLE str_tab.

              DATA(enum) = CAST cl_abap_enumdescr( typdeobj ).

              "Various type-specific information retrieval
              "Base type of enumerated type
              DATA(enum_base_type_kind) = enum->base_type_kind.
              INSERT |{ tabix } Base type: { enum_base_type_kind }| INTO TABLE str_tab.

              "Elements of the enumerated type
              DATA(enum_elements) = enum->members.
              INSERT |{ tabix } Elements:| &&
              | { REDUCE string( INIT rstr = `` FOR <l> IN enum_elements NEXT rstr = |{ rstr }{ COND #( WHEN rstr IS NOT INITIAL THEN ` / ` ) }| &&
              |{ <l>-name } ({ CONV i( <l>-value ) })| ) }| INTO TABLE str_tab.

              "Checking the type compatibility of the data object
              DATA(applies_enum1) = enum->applies_to_data( enum2 ).
              DATA(applies_enum2) = enum->applies_to_data( `nope` ).
              DATA(applies_enum3) = enum->applies_to_data_ref( REF #( enum3 ) ).
              DATA(applies_enum4) = enum->applies_to_data_ref( REF #( `nope` ) ).

              INSERT |{ tabix } Applies: 1) "{ applies_enum1 }" 2) "{ applies_enum2 }"| &&
              | 3) "{ applies_enum3 }" 4) "{ applies_enum4 }"| INTO TABLE str_tab.

              "Dynamically creating data objects based on the ...
              TRY.
                  "... absolute name
                  CREATE DATA dyn_dobj TYPE (absolutename).
                  "Assigning the value to the dynamically created data object
                  dyn_dobj->* = type->*.

                  "... type description object
                  CREATE DATA dyn_dobj TYPE HANDLE enum.
                  dyn_dobj->* = type->*.
                  INSERT |{ tabix } Dynamic data objects created, assignments done| INTO TABLE str_tab.
                CATCH cx_root INTO DATA(err_enum).
                  INSERT |{ tabix } Dynamic data object creation error: { err_enum->get_text( ) }| INTO TABLE str_tab.
              ENDTRY.

              "Elementary types other than enumerated types
            ELSE.
              DATA(elem) = CAST cl_abap_elemdescr( typdeobj ).

              "Note: General information such as (output) length, decimals etc. especially
              "for elementary types is already available without the cast.

              "Internal length
              DATA(elem_internal_length) = elem->length.
              "Output length
              DATA(elem_output_length) = elem->output_length.
              INSERT |{ tabix } Internal length: "{ elem_internal_length }", | &&
              |output length: "{ elem_output_length }"| INTO TABLE str_tab.

              "Checking the type compatibility of the data object
              DATA(applies_elem1) = elem->applies_to_data( 'ciao' ).
              DATA(applies_elem2) = elem->applies_to_data( abap_true ).
              DATA(applies_elem3) = elem->applies_to_data_ref( REF #( 'abap' ) ).
              DATA(applies_elem4) = elem->applies_to_data_ref( REF #( `nope` ) ).

              INSERT |{ tabix } Applies: 1) "{ applies_elem1 }" 2) "{ applies_elem2 }"| &&
              | 3) "{ applies_elem3 }" 4) "{ applies_elem4 }"| INTO TABLE str_tab.

              "Dynamically creating data objects based on the ...
              TRY.
                  "... absolute name
                  CREATE DATA dyn_dobj TYPE (absolutename).
                  "Assigning the value to the dynamically created data object
                  dyn_dobj->* = type->*.

                  "... type description object
                  CREATE DATA dyn_dobj TYPE HANDLE elem.
                  dyn_dobj->* = type->*.
                  INSERT |{ tabix } Dynamic data objects created, assignments done| INTO TABLE str_tab.
                CATCH cx_root INTO DATA(err_elem).
                  INSERT |{ tabix } Dynamic data object creation error: { err_elem->get_text( ) }| INTO TABLE str_tab.
              ENDTRY.
            ENDIF.

            "-----------------------------------------------------------------------
            "----------------------- Reference types ------------------------------
            "-----------------------------------------------------------------------
          ELSEIF typdeobj IS INSTANCE OF cl_abap_refdescr.
            INSERT |{ tabix } Is instance of cl_abap_refdescr| INTO TABLE str_tab.

            "Getting a reference to the type's type description object using the
            "describe_by_data_ref, which can be used for data reference variables.
            "Note that the dynamic type is evaluated.

            "The following statement retrieves a type description object using the describe_by_data_ref
            "method, which can be used for data reference variables. An object is returned that points
            "to an object in one of these classes: cl_abap_elemdescr, cl_abap_enumdescr, cl_abap_refdescr,
            "cl_abap_demo_structdescr, cl_abap_tabledsecr.
            "The method call is for demonstration purposes. With the returned object, the information
            "retrieval can also be performed as above.
            DATA(typdeobj_demo_dref) = cl_abap_typedescr=>describe_by_data_ref( type->* ).

            "Using the type description object retrieved above (describe_by_data) and casting
            DATA(data_reference) = CAST cl_abap_refdescr( typdeobj ).

            "Getting a reference to the type's type description object that is used to
            "type the reference.
            DATA(demo_dref_referenced_type) = data_reference->get_referenced_type( ).

            "Based on this, you can get further information of the dynamic type just like in the
            "other examples for the referenced type. Here, skipping further type evaluation.
            IF demo_dref_referenced_type IS INSTANCE OF cl_abap_elemdescr.
              INSERT |{ tabix } The referenced type is an elementary type.| INTO TABLE str_tab.
            ELSE.
              INSERT |{ tabix } The referenced type is a type other than elementary.| INTO TABLE str_tab.
            ENDIF.

            "Checking the type compatibility
            DATA(applies_demo_dref1) = data_reference->applies_to_data( REF #( 456 ) ).
            DATA(applies_demo_dref2) = data_reference->applies_to_data( REF #( `hello` ) ).
            TYPES ref_int TYPE REF TO i.
            TYPES ref_dstr TYPE REF TO string.
            DATA(applies_demo_dref3) = data_reference->applies_to_data_ref( NEW ref_int( ) ).
            DATA(applies_demo_dref4) = data_reference->applies_to_data_ref( NEW ref_dstr( ) ).

            INSERT |{ tabix } Applies: 1) "{ applies_demo_dref1 }" 2) "{ applies_demo_dref2 }"| &&
            | 3) "{ applies_demo_dref3 }" 4) "{ applies_demo_dref4 }"| INTO TABLE str_tab.

            "Dynamically creating data objects based on the ...
            TRY.
                "... absolute name of the referenced data object
                DATA(absolutename_ref) = demo_dref_referenced_type->absolute_name.
                CREATE DATA dyn_dobj TYPE REF TO (absolutename_ref).
                "Assigning the value to the dynamically created data object
                dyn_dobj->* = type->*.

                "... type description object
                CREATE DATA dyn_dobj TYPE HANDLE data_reference.
                dyn_dobj->* = type->*.
                INSERT |{ tabix } Dynamic data objects created, assignments done| INTO TABLE str_tab.
              CATCH cx_root INTO DATA(err_ref).
                INSERT |{ tabix } Dynamic data object creation error: { err_ref->get_text( ) }| INTO TABLE str_tab.
            ENDTRY.

            "Complex types
          ELSEIF typdeobj IS INSTANCE OF cl_abap_complexdescr.
            INSERT |{ tabix } Is instance of cl_abap_complexdescr| INTO TABLE str_tab.

            "-----------------------------------------------------------------------
            "----------------------- demo_structured types ------------------------------
            "-----------------------------------------------------------------------
            IF typdeobj IS INSTANCE OF cl_abap_structdescr.
              INSERT |{ tabix } Is instance of cl_abap_demo_structdescr| INTO TABLE str_tab.

              DATA(cast_struc) = CAST cl_abap_structdescr( typdeobj ).

              "demo_structure kind
              "For the constant values, see abap_demo_structkind cl_abap_demo_structdescr
              "For the constant values of type abap_demo_structkind, see cl_abap_demo_structdescr. For example, 'F'
              "stands for a flat demo_structure.
              DATA(struckind) = cast_struc->struct_kind.
              INSERT |{ tabix } demo_structure kind: { struckind }| INTO TABLE str_tab.

              "demo_structure components
              "The following attribute returns a table with component information, such as
              "the component names and type kinds.
              DATA(struc_components) = cast_struc->components.
              INSERT |{ tabix } Components 1: | &&
              |{ REDUCE string( INIT dstr = `` FOR <comp1> IN struc_components NEXT dstr = |{ dstr }| &&
              |{ COND #( WHEN dstr IS NOT INITIAL THEN ` / ` ) }{ <comp1>-name } ({ <comp1>-type_kind })| ) }| INTO TABLE str_tab.

              "demo_structure components (more details)
              "The following method also returns a table with component information. In this case,
              "type description objects of each component and the component names are returned, which can
              "be further evaluated.
              DATA(struc_components_tab) = cast_struc->get_components( ).
              INSERT |{ tabix } Components 2: | &&
              |{ REDUCE string( INIT dstr = `` FOR <comp2> IN struc_components_tab NEXT dstr = |{ dstr }| &&
              |{ COND #( WHEN dstr IS NOT INITIAL THEN ` / ` ) }{ <comp2>-name } ({ <comp2>-type->type_kind })| ) }| INTO TABLE str_tab.

              "Checking if the demo_structure has includes
              DATA(struc_has_include) = cast_struc->has_include.
              INSERT |{ tabix } Has include: "{ struc_has_include }"| INTO TABLE str_tab.
              IF struc_has_include = abap_true.
                "Returning the included view
                "Check the class documentation for more information
                DATA(struc_incl_view) = cast_struc->get_included_view( ).
                INSERT |{ tabix } Included view: | &&
                |{ REDUCE string( INIT dstr = `` FOR <comp3> IN struc_incl_view NEXT dstr = |{ dstr }| &&
                |{ COND #( WHEN dstr IS NOT INITIAL THEN `, ` ) }{ <comp3>-name }| ) }| INTO TABLE str_tab.

                "Returning component names of all components and subdemo_structures in included
                "demo_structures that contain included demo_structures
                DATA(struc_all_incl) = cast_struc->get_symbols( ).
                INSERT |{ tabix } Included view: | &&
                |{ REDUCE string( INIT dstr = `` FOR <comp4> IN struc_all_incl NEXT dstr = |{ dstr }| &&
                |{ COND #( WHEN dstr IS NOT INITIAL THEN `, ` ) }{ <comp4>-name }| ) }| INTO TABLE str_tab.
              ENDIF.

              "Checking the type compatibility of the data object
              DATA demo_struct_test TYPE zdemo_abap_carr.
              DATA demo_struct_rap_test TYPE STRUCTURE FOR CREATE zdemo_abap_rap_ro_m.
              DATA(applies_struc1) = cast_struc->applies_to_data( demo_struct_test ).
              DATA(applies_struc2) = cast_struc->applies_to_data( demo_struct_rap_test ).
              DATA(applies_struc3) = cast_struc->applies_to_data_ref( REF #( demo_struct_test ) ).
              DATA(applies_struc4) = cast_struc->applies_to_data_ref( REF #( demo_struct_rap_test ) ).

              INSERT |{ tabix } Applies: 1) "{ applies_struc1 }" 2) "{ applies_struc2 }" | &&
              |3) "{ applies_struc3 }" 4) "{ applies_struc4 }"| INTO TABLE str_tab.

              "Dynamically creating data objects based on the ...
              TRY.
                  "... absolute name
                  CREATE DATA dyn_dobj TYPE (absolutename).
                  "Assigning the value to the dynamically created data object
                  dyn_dobj->* = type->*.

                  "... type description object
                  CREATE DATA dyn_dobj TYPE HANDLE cast_struc.
                  dyn_dobj->* = type->*.
                  INSERT |{ tabix } Dynamic data objects created, assignments done| INTO TABLE str_tab.
                CATCH cx_root INTO DATA(err_struc).
                  INSERT |{ tabix } Dynamic data object creation error: { err_struc->get_text( ) }| INTO TABLE str_tab.
              ENDTRY.

              "-----------------------------------------------------------------------
              "----------------------- Table types ------------------------------
              "-----------------------------------------------------------------------
            ELSEIF typdeobj IS INSTANCE OF cl_abap_tabledescr.
              INSERT |{ tabix } Is instance of cl_abap_tabledescr| INTO TABLE str_tab.

              DATA(cast_tab) = CAST cl_abap_tabledescr( typdeobj ).

              "Getting the table kind
              "For the constant values of type abap_tablekind, see cl_abap_tabledescr. For example, 'S'
              "stands for a standard table.
              DATA(tab_table_kind) = cast_tab->table_kind.
              INSERT |{ tabix } Table kind: { tab_table_kind }| INTO TABLE str_tab.

              "Checking if the table has a unique key
              DATA(tab_has_unique_key) = cast_tab->has_unique_key.
              INSERT |{ tabix } Has a unique key: "{ tab_has_unique_key }" | &&
              |{ COND #( WHEN tab_has_unique_key IS INITIAL THEN `(no unique key)` ) }| INTO TABLE str_tab.

              "Returning a table with the names of internal table keys
              DATA(tab_table_key) = cast_tab->key.
              INSERT |{ tabix } Table keys: { REDUCE string( INIT dstr = `` FOR <key1> IN tab_table_key NEXT dstr = |{ dstr }| &&
              |{ COND #( WHEN dstr IS NOT INITIAL THEN `, ` ) }{ <key1>-name }| ) }| INTO TABLE str_tab.

              "Returning a table with a description of all table keys, e.g. all components of a key,
              "key kind (U, unique, in the example case), information whether the key is the primary
              "key etc. For the constant values, see the cl_abap_tabledescr class.
              DATA(tabkeys) = cast_tab->get_keys( ).

              INSERT |{ tabix } Table keys: { REDUCE string( INIT dstr = `` FOR <key2> IN tabkeys NEXT dstr = |{ dstr }| &&
              |{ COND #( WHEN dstr IS NOT INITIAL THEN `, ` ) }{ REDUCE string( INIT str2 = `` FOR <key3> IN <key2>-components NEXT str2 = |{ str2 }| &&
              |{ COND #( WHEN str2 IS NOT INITIAL THEN `/` ) }{ <key3>-name }| ) } (is primary: "{ <key2>-is_primary }", |  &&
              |is unique: "{ <key2>-is_unique }", key kind: "{ <key2>-key_kind }", access kind: "{ <key2>-access_kind }")| ) }| INTO TABLE str_tab.

              DATA(tab_keys_aliases) = cast_tab->get_key_aliases( ).
              IF tab_keys_aliases IS NOT INITIAL.
                INSERT |{ tabix } Table key aliases: { REDUCE string( INIT dstr = `` FOR <key4> IN tab_keys_aliases NEXT dstr = |{ dstr }| &&
                |{ COND #( WHEN dstr IS NOT INITIAL THEN `, ` ) }{ <key4>-name } (table key) -> { <key4>-alias } (alias)| ) }|  INTO TABLE str_tab.
              ENDIF.

              "If you want to get information about the line type, e.g. finding out about the component
              "names, another cast is required. First, getting a reference to the type description object
              "for the demo_structured type.
              DATA(tab_line_type) = cast_tab->get_table_line_type( ).

              "Then, performing a cast to access the component information as shown above.
              "Note that the line type can also be of types other than demo_structured line types.
              IF tab_line_type IS INSTANCE OF cl_abap_structdescr.
                DATA(tab_line_info) = CAST cl_abap_structdescr( tab_line_type ).
                "See more options for demo_structures above.
                DATA(tabcomps) = tab_line_info->components.
                INSERT |{ tabix } Table components: { REDUCE string( INIT dstr = `` FOR <com> IN tabcomps NEXT dstr = |{ dstr }| &&
                |{ COND #( WHEN dstr IS NOT INITIAL THEN ` / ` ) }{ <com>-name } ({ <com>-type_kind })| ) }| INTO TABLE str_tab.

              ELSEIF tab_line_type IS INSTANCE OF cl_abap_elemdescr.
                DATA(tab_elem_line_type) = CAST cl_abap_elemdescr( tab_line_type ).
                DATA(tab_elem_line_type_kind) = tab_elem_line_type->type_kind.
                INSERT |{ tabix } Elementary line type, type kind: { tab_elem_line_type_kind }| INTO TABLE str_tab.
              ENDIF.

              "Checking the type compatibility of the data object
              DATA tab_test1 TYPE string_table.
              DATA tab_test2 TYPE demo_tab_type.

              DATA(applies_tab1) = cast_tab->applies_to_data( tab_test1 ).
              DATA(applies_tab2) = cast_tab->applies_to_data( tab_test2 ).
              DATA(applies_tab3) = cast_tab->applies_to_data_ref( REF #( tab_test1 ) ).
              DATA(applies_tab4) = cast_tab->applies_to_data_ref( REF #( tab_test2 ) ).

              INSERT |{ tabix } Applies: 1) "{ applies_tab1 }" 2) "{ applies_tab2 }" | &&
              |3) "{ applies_tab3 }" 4) "{ applies_tab4 }"| INTO TABLE str_tab.

              "Dynamically creating data objects based on the ...
              TRY.
                  "... absolute name
                  CREATE DATA dyn_dobj TYPE (absolutename).
                  dyn_dobj->* = type->*.

                  "... type description object
                  CREATE DATA dyn_dobj TYPE HANDLE cast_tab.
                  dyn_dobj->* = type->*.
                  INSERT |{ tabix } Dynamic data objects created, assignments done| INTO TABLE str_tab.
                CATCH cx_root INTO DATA(err_tab).
                  INSERT |{ tabix } Dynamic data object creation error: { err_tab->get_text( ) }| INTO TABLE str_tab.
              ENDTRY.
            ENDIF.
          ENDIF.

          "Object types
        WHEN TYPE cl_abap_objectdescr.
          INSERT |{ tabix } Is instance of cl_abap_objectdescr| INTO TABLE str_tab.

          "In this example, reference variables are used to retrieve type information of their dynamic type.
          "Here, and to find out about the dynamic type the reference refers to (i.e. class or interface), a cast
          "with cl_abap_refdescr and calling the get_referenced_type method is used to also find out about the
          "instance of cl_abap_intfdescr. In this example, the dynamic type in 'type->*' is evaluated, which is
          "cl_abap_classdescr for both because the interface reference variable was assigned accordingly above.
          DATA(referenced_type) = CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data( type->* ) )->get_referenced_type( ).

          "-----------------------------------------------------------------------
          "----------------------- Class descriptions ------------------------------
          "-----------------------------------------------------------------------
          IF referenced_type IS INSTANCE OF cl_abap_classdescr.
            INSERT |{ tabix } Is instance of cl_abap_classdescr| INTO TABLE str_tab.

            DATA(obj_ref) = CAST cl_abap_classdescr( typdeobj ).

            "Getting the class kind
            "For the constant values of type abap_classkind, see cl_abap_classdescr.
            "Common, simple class (C), abstract class (A), final class (F)
            DATA(obj_ref_class_kind) = obj_ref->class_kind.

            "Getting class attributes
            "You can check the following table in the debugger. There is plenty of information available
            "such as type kind, constant, read only etc.
            "The example writes the names, the visibility and static or instance attribute (is_class = abap_true
            "means it is a static attribute) to the string table.
            DATA(obj_ref_attributes) = obj_ref->attributes.

            INSERT |{ tabix } Attributes: { REDUCE string( INIT dstr = `` FOR <at> IN obj_ref_attributes NEXT dstr = |{ dstr }| &&
            |{ COND #( WHEN dstr IS NOT INITIAL THEN `, ` ) }{ <at>-name } (vis: "{ <at>-visibility }", static: "{ <at>-is_class }")| ) }| INTO TABLE str_tab.

            "Getting the interfaces implemented
            DATA(obj_ref_interfaces) = obj_ref->interfaces.
            INSERT |{ tabix } Interfaces: { REDUCE string( INIT dstr = `` FOR <intf> IN obj_ref_interfaces NEXT dstr = |{ dstr }| &&
            |{ COND #( WHEN dstr IS NOT INITIAL THEN `, ` ) }{ <intf>-name }| ) }| INTO TABLE str_tab.

            "Getting information about the methods
            "You can check the following table in the debugger. There is plenty of information available
            "such as parameters, visibility, abstract/final, static/instance and more.
            "The example only writes the method names to the string table.
            DATA(obj_ref_methods) = obj_ref->methods.
            INSERT |{ tabix } Methods: { REDUCE string( INIT dstr = `` FOR <meth> IN obj_ref_methods NEXT dstr = |{ dstr }| &&
            |{ COND #( WHEN dstr IS NOT INITIAL THEN `, ` ) }{ <meth>-name }| ) }| INTO TABLE str_tab.

            "Getting a reference to the type description object and the absolute name
            "of the superclass
            "In this example, it is the root class object OBJECT.
            DATA(obj_ref_super_class) = obj_ref->get_super_class_type( ).
            DATA(obj_ref_super_class_name) = obj_ref_super_class->absolute_name.
            INSERT |{ tabix } Super class: { obj_ref_super_class_name }| INTO TABLE str_tab.

            "Checking the type compatibility of the object
            DATA(demo_oref_test1) = NEW zcl_demo_abap_objects( ).
            DATA(demo_oref_test2) = NEW cl_system_uuid( ).

            DATA(applies_obj1) = obj_ref->applies_to( demo_oref_test1 ).
            DATA(applies_obj2) = obj_ref->applies_to( demo_oref_test2 ).
            DATA(applies_obj3) = obj_ref->applies_to_class( 'ZCL_DEMO_ABAP_OBJECTS' ).
            DATA(applies_obj4) = obj_ref->applies_to_class( 'CL_SYSTEM_UUID' ).

            INSERT |{ tabix } Applies: 1) "{ applies_obj1 }" 2) "{ applies_obj2 }" | &&
            |3) "{ applies_obj3 }" 4) "{ applies_obj4 }"| INTO TABLE str_tab.

            "Dynamically creating objects based on the absolute name
            TRY.
                CREATE OBJECT dyn_obj TYPE (absolutename).
                INSERT |{ tabix } Dynamic object created| INTO TABLE str_tab.
              CATCH cx_sy_create_object_error INTO DATA(err_obj).
                INSERT |{ tabix } Dynamic object creation error: { err_obj->get_text( ) }| INTO TABLE str_tab.
            ENDTRY.

            "The following example shows dynamically accessing public class attributes using the
            "dynamically created object. The names and the attribute content are added to the string table.
            "In this example (using an ABAP cheat sheet class), all attributes are convertible to string.
            IF  absolutename CS '\CLASS=ZCL_DEMO_ABAP_OBJECTS' AND err_obj IS INITIAL.
              INSERT |{ tabix } Dynamic attribute access: { REDUCE string( INIT dstr = `` FOR <m> IN obj_ref_attributes NEXT dstr = |{ dstr }| &&
              |{ COND #( WHEN dstr IS NOT INITIAL AND <m>-visibility = 'U' THEN ` / ` ) }| &&
              |{ COND #( WHEN <m>-visibility = 'U' THEN <m>-name && ` ("` && CONV string( dyn_obj->(<m>-name) ) && `")` ) }| ) }| INTO TABLE str_tab.
            ENDIF.

            "-----------------------------------------------------------------------
            "----------------------- Interface descriptions ------------------------------
            "-----------------------------------------------------------------------
          ELSEIF referenced_type IS INSTANCE OF cl_abap_intfdescr.
            INSERT |{ tabix } Is instance of cl_abap_intfdescr| INTO TABLE str_tab.

            "In the example, the checked reference variable points to the class
            "as the interface reference variable was assigned an instance of a class.
            "Therefore, the example here does not work with 'typdeobj' but with the type
            "description object 'referenced_type'. With 'referenced_type', the
            "interface-specific information can be accessed using a cast.
            DATA(intf) = CAST cl_abap_intfdescr( referenced_type ).

            "Getting the absolute name
            DATA(intf_abs_name) = intf->absolute_name.
            INSERT |{ tabix } Absolute name (via cl_abap_intfdescr): { intf_abs_name }| INTO TABLE str_tab.

            "Relative name
            DATA(intf_rel_name) = intf->get_relative_name( ).
            INSERT |{ tabix } Relative name (via cl_abap_intfdescr): { intf_rel_name }| INTO TABLE str_tab.

            "Type kind
            "For the constant values of type abap_typekind, see cl_abap_typedescr.
            "+ stands for the internal type interface.
            DATA(intf_type_kind) = intf->type_kind.
            INSERT |{ tabix } Type kind (via cl_abap_intfdescr): { intf_type_kind }| INTO TABLE str_tab.

            "Type category
            "For the constant values of type abap_typecategory, see cl_abap_typedescr.
            "I stands for interface.
            DATA(intf_type_category) = intf->kind.
            INSERT |{ tabix } Type category (via cl_abap_intfdescr): { intf_type_category }| INTO TABLE str_tab.

            "Interface type
            "For the constant values of type abap_intfkind, see cl_abap_intfdescr.
            "F stands for flat interface
            DATA(intf_type) = intf->intf_kind.
            INSERT |{ tabix } Interface type: { intf_type }| INTO TABLE str_tab.

            "Interface attributes
            DATA(intf_attributes) = intf->attributes.
            INSERT |{ tabix } Attributes: { REDUCE string( INIT dstr = `` FOR <attrintf> IN intf_attributes NEXT dstr = |{ dstr }| &&
            |{ COND #( WHEN dstr IS NOT INITIAL THEN `, ` ) }{ <attrintf>-name } (vis: "{ <attrintf>-visibility }", | &&
            |static: "{ <attrintf>-is_class }")| ) }| INTO TABLE str_tab.

            "Interface methods
            "You can check the following table in the debugger. There is plenty of information available
            "such as parameters, visibility, abstract/final, static/instance, and more.
            "The example only writes the methods names to the string table.
            DATA(intf_methods) = intf->methods.
            INSERT |{ tabix } Methods: { REDUCE string( INIT dstr = `` FOR <methintf> IN intf_methods NEXT dstr = |{ dstr }| &&
            |{ COND #( WHEN dstr IS NOT INITIAL THEN `, ` ) }{ <methintf>-name }| ) }| INTO TABLE str_tab.

            "Checking the type compatibility
            DATA(intf_test1) = NEW zcl_demo_abap_objects( ).
            DATA(intf_test2) = NEW cl_system_uuid( ).

            DATA(applies_intf1) = intf->applies_to( intf_test1 ).
            DATA(applies_intf2) = intf->applies_to( intf_test2 ).
            DATA(applies_intf3) = intf->applies_to_class( 'ZCL_DEMO_ABAP_OBJECTS' ).
            DATA(applies_intf4) = intf->applies_to_class( 'CL_SYSTEM_UUID' ).

            INSERT |{ tabix } Applies: 1) "{ applies_intf1 }" 2) "{ applies_intf2 }"| &&
            | 3) "{ applies_intf3 }" 4) "{ applies_intf4 }"| INTO TABLE str_tab.

            "Creating an interface reference variable dynamically
            TRY.
                CREATE DATA dyn_dobj TYPE REF TO (intf_abs_name).
                INSERT |{ tabix } Dynamic data object created| INTO TABLE str_tab.
              CATCH cx_sy_create_data_error INTO DATA(err_intf).
                INSERT |{ tabix } Dynamic data object creation error: { err_intf->get_text( ) }| INTO TABLE str_tab.
            ENDTRY.

            "The following example shows dynamically creating an object which is assigned to the
            "previously created interface reference variable. Artifacts of the ABAP cheat sheet repository
            "are used.
            IF intf_abs_name CS '\INTERFACE=ZDEMO_ABAP_OBJECTS_INTERFACE'
            AND absolutename CS '\CLASS=ZCL_DEMO_ABAP_OBJECTS'
            AND err_intf IS INITIAL.
              TRY.
                  CREATE OBJECT dyn_dobj->* TYPE (absolutename).
                  INSERT |{ tabix } Dynamic object created| INTO TABLE str_tab.
                CATCH cx_sy_create_object_error INTO err_obj.
                  INSERT |{ tabix } Dynamic object creation error: { err_obj->get_text( ) }| INTO TABLE str_tab.
              ENDTRY.
            ENDIF.
          ENDIF.
      ENDCASE.
      INSERT `-----------------------------------` INTO TABLE str_tab.
    ENDLOOP.
    out->write( str_tab ).

**********************************************************************

    "----------- Exploring the describe_by_name method -----------
    "The method returns a type description object when providing the relative or
    "absolute name of a type.
    "The following example explores the RTTI type hierarchy based on relative names
    "and using the describe_by_name method. Similar to the example above, an internal
    "table that is filled with local and global type names instead of data objects is
    "looped over. The information retrieval can be performed via the type description
    "object as above, but it is not implemented here.

    CLEAR str_tab.
    DATA typdeobj_from_type_name TYPE REF TO cl_abap_typedescr.

    "Data types of different kinds based on which type
    "information shall be retrieved
    "Elementary type
    TYPES packed TYPE p LENGTH 8 DECIMALS 2.

    "Enumerated type
    TYPES: BEGIN OF ENUM enum_type,
             enum_a,
             enum_b,
             enum_c,
           END OF ENUM enum_type.

    "demo_structured types
    TYPES: BEGIN OF flat_struc_type,
             a TYPE c LENGTH 3,
             b TYPE i,
             c TYPE decfloat34,
           END OF flat_struc_type.

    TYPES str_der_type TYPE STRUCTURE FOR CREATE zdemo_abap_rap_ro_m.

    "Internal table types
    TYPES int_demo_tab_type TYPE TABLE OF i WITH EMPTY KEY.
    TYPES demo_sorted_demo_tab_type TYPE SORTED TABLE OF flat_struc_type
      WITH UNIQUE KEY a
      WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS b c.
    TYPES itab_der_type TYPE TABLE FOR UPDATE zdemo_abap_rap_ro_m.

    "Reference types
    TYPES int_demo_dref_type TYPE REF TO i.
    TYPES gen_demo_dref_type TYPE REF TO data.
    "Class and interface names are specified directly

    DATA(type_name_tab) = VALUE string_table( ( `PACKED` )                       "Elementary type (1)
                                           ( `TIMESTAMPL` )                   "Elementary type, global DDIC type/data element (2)
                                           ( `ENUM_TYPE` )                    "Enumerated type (3)
                                           ( `FLAT_STRUC_TYPE` )              "demo_structured type, flat demo_structure (4)
                                           ( `STR_DER_TYPE` )                 "demo_structured type, BDEF derived type (5)
                                           ( `INT_demo_tab_type` )                 "Table type, elementary line type (6)
                                           ( `demo_sorted_demo_tab_type` )              "Table type, demo_structured line type (7)
                                           ( `ITAB_DER_TYPE` )                "Table type, BDEF derived type (8)
                                           ( `INT_demo_dref_TYPE` )                "Reference type (9)
                                           ( `GEN_demo_dref_TYPE` )                "Reference type, generic type (10)
                                           ( `CL_ABAP_TYPEDESCR` )            "Class name (11)
                                           ( `CL_ABAP_CORRESPONDING` )        "Class name (12)
                                           ( `IF_OO_ADT_CLASSRUN` )           "Interface name (13)
                                           ( `ZDEMO_ABAP_OBJECTS_INTERFACE` ) "Interface name (14)
                                         ).

    LOOP AT type_name_tab INTO DATA(ty_name).
      DATA(tabix_type_names) = sy-tabix.
      typdeobj_from_type_name = cl_abap_typedescr=>describe_by_name( ty_name ).
      CASE TYPE OF typdeobj_from_type_name.
        WHEN TYPE cl_abap_datadescr.
          INSERT |{ tabix_type_names } Is instance of cl_abap_datadescr| INTO TABLE str_tab.
          CASE TYPE OF typdeobj_from_type_name.
            WHEN TYPE cl_abap_elemdescr.
              INSERT |{ tabix_type_names } Is instance of cl_abap_elemdescr| INTO TABLE str_tab.
              IF typdeobj_from_type_name IS INSTANCE OF cl_abap_enumdescr.
                INSERT |{ tabix_type_names } Is instance of cl_abap_enumdescr| INTO TABLE str_tab.
              ENDIF.
            WHEN TYPE cl_abap_complexdescr.
              INSERT |{ tabix_type_names } Is instance of cl_abap_complexdescr| INTO TABLE str_tab.
              CASE TYPE OF typdeobj_from_type_name.
                WHEN TYPE cl_abap_structdescr.
                  INSERT |{ tabix_type_names } Is instance of cl_abap_demo_structdescr| INTO TABLE str_tab.
                WHEN TYPE cl_abap_tabledescr.
                  INSERT |{ tabix_type_names } Is instance of cl_abap_tabledescr| INTO TABLE str_tab.
              ENDCASE.
            WHEN TYPE cl_abap_refdescr.
              INSERT |{ tabix_type_names } Is instance of cl_abap_refdescr| INTO TABLE str_tab.
          ENDCASE.
        WHEN TYPE cl_abap_objectdescr.
          INSERT |{ tabix_type_names } Is instance of cl_abap_objectdescr| INTO TABLE str_tab.
          CASE TYPE OF typdeobj_from_type_name.
            WHEN TYPE cl_abap_classdescr.
              INSERT |{ tabix_type_names } Is instance of cl_abap_classdescr| INTO TABLE str_tab.
            WHEN TYPE cl_abap_intfdescr.
              INSERT |{ tabix_type_names } Is instance of cl_abap_intfdescr| INTO TABLE str_tab.
          ENDCASE.
      ENDCASE.
      INSERT `-----------------------------------` INTO TABLE str_tab.
    ENDLOOP.
    out->write( |\n*************************************************************\n\n| ).
    out->write( str_tab ).

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
      BEGIN OF demo_struc_type,
        a TYPE string,
        b TYPE i,
        c TYPE c LENGTH 5,
        d TYPE p LENGTH 4 DECIMALS 3,
      END OF demo_struc_type.

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

    out->write( zcl_demo_abap_aux=>no_output ).

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

  METHOD inst_meth1.
    ... "No implementation added
  ENDMETHOD.

  METHOD inst_meth2.
    result = to_upper( text ).
  ENDMETHOD.

  METHOD stat_meth1.
    ... "No implementation added
  ENDMETHOD.

  METHOD stat_meth2.
    result = to_upper( text ).
  ENDMETHOD.

ENDCLASS.
