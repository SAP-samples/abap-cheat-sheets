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
* Some code sections are commented out. The syntax is only available in
* newer ABAP releases. Comment them in if you are running a newer
* ABAP release, for example, in the SAP BTP environment.
*
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

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_demo_abap_dynamic_prog IMPLEMENTATION.

  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_flight_tables=>fill_dbtabs( ).
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA(output) = NEW zcl_demo_abap_display( out ).

    output->display( `ABAP Cheat Sheet Example: Dynamic Programming` ).

**********************************************************************

    output->display( `Excursion: Field Symbols` ).
    output->display( `1) Declaring Field Symbols` ).

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

    output->display( `No output for this section. See the code.` ).

**********************************************************************

    output->next_section( `2) Assigning Data Objects to Field Symbols` ).

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

    "Field symbols with complete types
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

    "Inline declaration is possible, too. The type is derived automatically.
    ASSIGN num_a TO FIELD-SYMBOL(<fs_inl>).

    output->display( `No output for this section. See the code.` ).

**********************************************************************

    output->next_section( `3) Checking Field Symbol Assignment` ).

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
      output->display( `Field symbol <fs_i_b> is assigned.` ).
    ELSE.
      output->display( `Field symbol <fs_i_b is> not assigned.` ).
    ENDIF.

    IF <fs_str_b> IS ASSIGNED.
      output->display( `Field symbol <fs_str_b> is assigned.` ).
    ELSE.
      output->display( `Field symbol <fs_str_b> is not assigned.` ).
    ENDIF.

**********************************************************************

    output->next_section( `4) Unassigning Data Objects from Field Symbols` ).

    "If you use an unassigned field symbol, an exception is raised. Before
    "using it, you can check the assignment with the following logical
    "expression. The statement is true if the field symbol is assigned.
    "Using the statement UNASSIGN, you can explicitly remove the assignment
    "of the field symbol.

    DATA num_c TYPE i VALUE 123.

    FIELD-SYMBOLS: <fs_i_c> TYPE i.

    ASSIGN num_c TO <fs_i_c>.

    IF <fs_i_c> IS ASSIGNED.
      output->display( `1. Field symbol <fs_i_c> is assigned.` ).
    ELSE.
      output->display( `1. Field symbol <fs_i_c> is not assigned.` ).
    ENDIF.

    UNASSIGN <fs_i_c>.

    IF <fs_i_c> IS ASSIGNED.
      output->display( `2. Field symbol <fs_i_c> is assigned.` ).
    ELSE.
      output->display( `2. Field symbol <fs_i_c> is not assigned.` ).
    ENDIF.

**********************************************************************

    output->next_section( `5) Type Casting with Field Symbols` ).

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

    output->display( input = <fs_d2> name = `<fs_d2>` ).

    ASSIGN dobj_d_l10 TO <fs_d1> CASTING TYPE type_d_l9.

    output->display( input = <fs_d1> name = `<fs_d1>` ).

    "Casting to a generic type
    ASSIGN dobj_d_l10 TO <fs_d1> CASTING TYPE c.

    output->display( input = <fs_d1> name = `<fs_d1>` ).

    "Casting to a static field type
    ASSIGN dobj_d_l10 TO <fs_d1> CASTING LIKE dobj_d_l5.

    output->display( input = <fs_d1> name = `<fs_d1>` ).

    "Casting to a dynamic field type
    ASSIGN dobj_d_l10 TO <fs_d1> CASTING LIKE <fs_d1>.

    output->display( input = <fs_d1> name = `<fs_d1>` ).

    "Anticipating dynamic specification of data types
    "for the CASTING addition.
    "The type name is specified as a character-like data
    "object within parentheses.
    ASSIGN dobj_d_l10 TO <fs_d1> CASTING TYPE (type_name_d).

    output->display( input = <fs_d1> name = `<fs_d1>` ).

    "Anticipating RTTS
    "A type description object is created which can be
    "specified after the TYPE HANDLE additions.
    DATA(sometype) = CAST cl_abap_datadescr(
      cl_abap_typedescr=>describe_by_name( 'TYPE_D_L9' ) ).
    ASSIGN dobj_d_l10 TO <fs_d1> CASTING TYPE HANDLE sometype.

    output->display( input = <fs_d1> name = `<fs_d1>` ).

**********************************************************************

    output->next_section( `6) Addressing Field Symbols` ).

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

    output->display( input = <fs_i_e> name = `<fs_i_e>` ).
    output->display( input = num_e name = `num_e` ).

    "Use in expressions
    DATA(calc_e) = <fs_i_e> + 211.

    output->display( input = calc_e name = `calc_e` ).

    IF <fs_i_e> < 1000.
      output->display( `The value of <fs_i_e> is less than 1000` ).
    ELSE.
      output->display( `The value of <fs_i_e> is greater than 1000` ).
    ENDIF.

    "Structure
    output->display( input = <fs_struc_e> name = `<fs_struc_e>` ).

    DATA(comp_e1) = <fs_struc_e>-carrid.

    output->display( input = comp_e1 name = `comp_e1` ).

    <fs_struc_e>-url = 'www.lh.com'.

    output->display( input = <fs_struc_e>-url name = `<fs_struc_e>-url` ).

    "Internal table
    SELECT *
      FROM zdemo_abap_carr
      ORDER BY carrid
      INTO TABLE @<fs_tab_e>
      UP TO 3 ROWS.

    output->display( input = <fs_tab_e> name = `<fs_tab_e>` ).

    TRY.
        DATA(comp_e2) = <fs_tab_e>[ 2 ]-carrname.
        output->display( input = comp_e2 name = `comp_e2` ).
      CATCH cx_sy_itab_line_not_found INTO DATA(error_e).
    ENDTRY.

    SELECT *
      FROM zdemo_abap_carr
      ORDER BY carrid
      INTO TABLE @<fs_anytab_e>
      UP TO 3 ROWS.

    output->display( input = <fs_anytab_e> name = `<fs_anytab_e>` ).

**********************************************************************

    output->next_section( `7) Using Field Symbols when Processing ` &&
    `Internal Tables` ).

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

    output->display( input = tab_f1 name = `tab_f1` ).

    "The following example shows a field symbol declared inline.
    LOOP AT <fs_tab_f> ASSIGNING FIELD-SYMBOL(<fs_struc_f2>).
      <fs_struc_f2>-connid = '100'.
      <fs_struc_f2>-fldate = cl_abap_context_info=>get_system_date( ) + 1.
      <fs_struc_f2>-price = <fs_struc_f2>-price - 50.
      <fs_struc_f2>-currency = 'USD'.

      "Filling another itab
      <fs_anytab_f> = VALUE #( BASE <fs_anytab_f> ( <fs_struc_f2> ) ).
    ENDLOOP.

    output->display( input = <fs_tab_f> name = `<fs_tab_f>` ).
    output->display( input = <fs_anytab_f> name = `<fs_anytab_f>` ).

**********************************************************************

    output->next_section( `8) Field Symbols in the Context of Processing a Structure` ).

    "In this example, all components of a structure are processed using
    "field symbols and an ASSIGN COMPONENT ... OF STRUCTURE ... statement.
    "First, a field symbol is declared with a generic type. A structure is
    "filled with values from a demo table. The structure is assigned to the
    "field symbol. Using a DO loop, all components are processed. The
    "sy-index value represents the position of the component in the
    "structure. Once all components have been processed (i. e. if sy-subrc
    "does not return '0' for a sy-index value), the loop is exited. The output
    "shows all components and their values.

    FIELD-SYMBOLS: <comp> TYPE data.

    DATA: comp_tab TYPE string_table.

    SELECT SINGLE carrid, carrname, currcode, url
      FROM zdemo_abap_carr
      WHERE carrid = 'LH'
      INTO @DATA(struct).

    FIELD-SYMBOLS: <struct> TYPE data.

    ASSIGN struct TO <struct>.

    DO.
      "sy-index represents the position of a structure component
      "NOTE: The following statement is replaced by the newer syntax that is
      "commented out below. Therefore, it is recommended that you use this
      "syntax in newer ABAP releases.
      ASSIGN COMPONENT sy-index OF STRUCTURE <struct> TO <comp>.

      "ASSIGN <struct>-(sy-index) to <comp>.

      IF sy-subrc <> 0.
        "If all components are processed, the loop is exited.
        EXIT.
      ELSE.
        output->display( |sy-index: { sy-index }, component content:| ).
        output->display( <comp> ).
      ENDIF.

    ENDDO.

**********************************************************************

    output->next_section( `Data references` ).
    output->display( `9) Declaring Data References` ).

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

    output->display( `No output for this section. See the code.` ).

**********************************************************************

    output->next_section( `10) Creating Data References ` &&
    `to Existing Data Objects` ).

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

    output->display( `No output for this section. See the code.` ).

**********************************************************************

    output->next_section( `11) Dynamically Creating Data Objects at Runtime Using Static Type Definitions` ).

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
    "Using the NEW addition in the INTO clause, an anonymous data object with
    "suitable type can be created in place.
    SELECT *
      FROM zdemo_abap_carr
      INTO TABLE NEW @DATA(dref_c7)   "Internal table
      UP TO 3 ROWS.

    SELECT SINGLE *
      FROM zdemo_abap_carr
      WHERE carrid = 'LH'
      INTO NEW @DATA(dref_c8). "Structure

    output->display( input = dref_c6->* name = `dref_c6->*` ).
    output->display( input = dref_c7->* name = `dref_c7->*` ).
    output->display( input = dref_c8->* name = `dref_c8->*` ).

**********************************************************************

    output->next_section( `12) Data References and Assignments` ).

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

    output->display( input = ref_d2->* name = `ref_d2->*` ).
    output->display( input = ref_data_d1->* name = `ref_data_d1->*` ).
    output->display( input = ref_d5->* name = `ref_d5->*` ).

**********************************************************************

    output->next_section( `13) Addressing Data References ` ).

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

    output->display( input = ref_e1->* name = `ref_e1->*` ).

    "Content of referenced data object is changed
    ref_e1->* = 10.

    output->display( input = ref_e1->* name = `ref_e1->*` ).

    "Data reference used in a logical expression
    IF ref_e1->* > 5.
      output->display( `The value of ref_e1 is greater than 5.` ).
    ELSE.
      output->display( `The value of ref_e1 is lower than 5.` ).
    ENDIF.

    "Dereferenced generic type
    DATA(calc) = 1 + ref_data_e->*.

    output->display( input = calc name = `calc` ).

    "Complete structure
    DATA(struc) = ref_e2->*.

    output->display( input = ref_e2->* name = `ref_e2->*` ).

    "Individual structure component
    "When dereferencing a data reference variable that has a structured
    "data type, you can use the component selector -> to address individual components.
    DATA(carrid) = ref_e2->carrid.

    ref_e2->carrid = 'UA'.

    output->display( input = ref_e2->carrid name = `ref_e2->carrid` ).

    "The following syntax also works (dereferencing operator and the component selector).
    ref_e2->*-carrname = 'United Airlines'.

    output->display( input = ref_e2->*-carrname name = `ref_e2->*-carrname` ).

**********************************************************************

    output->next_section( `14) Checking if Data Reference ` &&
    `Can Be Dereferenced` ).

    "You can check if a data reference can be dereferenced by using
    "a logical expression with IS [NOT] BOUND.
    "The example shows both a data reference that is bound and not bound.

    DATA(ref_f1) = NEW string( `hello` ).
    DATA ref_f2 TYPE REF TO i.

    IF ref_f1 IS BOUND.
      output->display( `ref_f1 is bound.` ).
    ELSE.
      output->display( `ref_f1 is not bound.` ).
    ENDIF.

    IF ref_f2 IS BOUND.
      output->display( `ref_f2 is bound.` ).
    ELSE.
      output->display( `ref_f2 is not bound.` ).
    ENDIF.

**********************************************************************

    output->next_section( `15) Explicitly Removing a Reference` ).

    "Note that the garbage collector takes care of removing the references
    "automatically once the data is not used any more by a reference.

    DATA(ref_g1) = NEW string( `hello` ).

    IF ref_g1 IS INITIAL.
      output->display( `Before CLEAR: ref_g1 is initial.` ).
    ELSE.
      output->display( `Before CLEAR: ref_g1 is not intial.` ).
    ENDIF.

    IF ref_g1 IS BOUND.
      output->display( `Before CLEAR: ref_g1 is bound.` ).
    ELSE.
      output->display( `Before CLEAR: ref_g1 is not bound.` ).
    ENDIF.

    CLEAR ref_g1.

    IF ref_g1 IS INITIAL.
      output->display( `After CLEAR: ref_g1 is initial.` ).
    ELSE.
      output->display( `After CLEAR: ref_g1 is not initial.` ).
    ENDIF.

    IF ref_g1 IS BOUND.
      output->display( `After CLEAR: ref_g1 is bound.` ).
    ELSE.
      output->display( `After CLEAR: ref_g1 is not bound.` ).
    ENDIF.

**********************************************************************

    output->next_section( `16) Overwriting Data Reference Variables` ).

    "A data reference variable is overwritten if a new object is created
    "with a data reference variable already pointing to a data object

    DATA ref_h1 TYPE REF TO data.

    ref_h1 = NEW i( 111 ).

    output->display( input = ref_h1->* name = `ref_h1->*` ).

    ref_h1 = NEW string( `ref_h1 overwritten.` ).

    output->display( input = ref_h1->* name = `ref_h1->*` ).

**********************************************************************

    output->next_section( `17) Retaining Data References` ).

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

    output->display( input = itab_i name = `itab_i` ).
    output->display( input = `The derefenced value of the data reference - which ` &&
    `was changed in the course of the loop - in the second table ` &&
    `entry is ` && itab_i[ 2 ]->* && `.` ).

**********************************************************************

    output->next_section( `18) Processing Internal Tables Using ` &&
    `Data References ` ).

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

    output->display( input = flsch_tab name = `flsch_tab` ).

**********************************************************************

    output->next_section( `19) Data References as Part of ` &&
    `Structures and Internal Tables` ).

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

    output->display( input = struc_l ).

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

    output->display( input = itab_l name = `itab_l` ).

**********************************************************************

    output->next_section( `Dynamic ABAP Statements` ).
    output->display( `20) Dynamic Specifications in ASSIGN Statements (1) - Attributes of Classes/Interfaces` ).

    "The following examples demonstrate a selection of various dynamic specifications
    "that are possible with ASSIGN statements.

    "Dynamic specification of attributes of classes/interfaces that are assigned to a field symbol.

    DATA(dobj_name) = lcl_det_at_runtime=>get_dyn_dobj( ).

    ASSIGN lcl_det_at_runtime=>(dobj_name) TO FIELD-SYMBOL(<fs_m1>).

    output->display( |Data object name determined at runtime: { dobj_name } | ).
    output->display( |The content of the data object is: { <fs_m1> } | ).

    dobj_name = lcl_det_at_runtime=>get_dyn_dobj( ).

    "Completely dynamic assign
    ASSIGN ('lcl_det_at_runtime')=>(dobj_name) TO FIELD-SYMBOL(<fs_m2>).
    output->display( input = <fs_m2> name = `<fs_m2>` ).

    ASSIGN ('zdemo_abap_objects_interface')=>('const_intf') TO FIELD-SYMBOL(<fs_m3>).
    output->display( input = <fs_m3> name = `<fs_m3>` ).

    "Class/interface reference variables pointing to an object that contains attributes
    "and that are specified dynamically.
    DATA iref TYPE REF TO zdemo_abap_objects_interface.
    DATA(cl_ref) = NEW zcl_demo_abap_objects( ).
    iref = cl_ref.

    ASSIGN iref->('const_intf') TO FIELD-SYMBOL(<fs_m4>).

    output->display( input = <fs_m4> name = `<fs_m4>` ).

    ASSIGN cl_ref->('another_string') TO FIELD-SYMBOL(<fs_m5>).

    output->display( input = <fs_m5> name = `<fs_m5>` ).

**********************************************************************
    "Note: Comment in the following example in newer ABAP releases and in the SAP BTP environment.

*    output->next_section( `21) Dynamic Specifications in ASSIGN Statements (2) - Setting sy-subrc/ELSE UNASSIGN` ).
*
*    "In dynamic assignments, the statement ASSIGN sets the return code sy-subrc.
*    "If ELSE UNASSIGN is specified, no memory area is assigned to the field symbol. It has the state unassigned after the ASSIGN statement.
*
*    DATA(attr) = VALUE string_table( ( `another_string` ) ( `public_string` ) ( `this_will_fail` ) ).
*
*    LOOP AT attr INTO DATA(attribute).
*
*      ASSIGN cl_ref->(attribute) TO FIELD-SYMBOL(<attr>) ELSE UNASSIGN.
*      IF sy-subrc = 0.
*        output->display( |Successful assignment for attribute "{ attribute }". sy-subrc = { sy-subrc }. | ).
*        output->display( input = <attr> name = `<attr>` ).
*      ELSE.
*        output->display( |Assignment not successful for attribute "{ attribute }". sy-subrc = { sy-subrc }. | ).
*      ENDIF.
*
*      IF <attr> IS ASSIGNED.
*        output->display( `The field symbol is assigned.` ).
*        output->display( `--------------------` ).
*      ELSE.
*        output->display( `The field symbol is not assigned.` ).
*      ENDIF.
*
*    ENDLOOP.

**********************************************************************
    "Note: The following code contains syntax that is only available in
    "newer ABAP releases and in the SAP BTP environment. In these contexts,
    "you can comment in the code.

    output->next_section( `22) Dynamic Specifications in ASSIGN Statements (3) - Structure Components` ).

    "Dynamic specification of structure components that are assigned to a field symbol.

    SELECT SINGLE * FROM zdemo_abap_carr INTO @DATA(wa).
    "Reading into data reference variable
    SELECT SINGLE * FROM zdemo_abap_carr INTO NEW @DATA(ref_m).

    DATA(comp_name) = lcl_det_at_runtime=>get_dyn_field( ).

*    ASSIGN wa-(comp_name) TO FIELD-SYMBOL(<fs_m6>).
*
*    ASSIGN wa-('CARRNAME') TO FIELD-SYMBOL(<fs_m7>).
*
*    IF sy-subrc = 0.
*      DATA(subrc1) = sy-subrc.
*    ENDIF.
*
*    "No exception occurs in case of an unsuccessful assignment.
*    ASSIGN wa-('CRRNM') TO FIELD-SYMBOL(<fs_m8>).
*
*    IF sy-subrc <> 0.
*      DATA(subrc2) = sy-subrc.
*    ENDIF.
*
*    "Numeric expressions are possible. Its value is interpreted as the position
*    "of the component in the structure.
*    ASSIGN wa-(4) TO FIELD-SYMBOL(<fs_m9>).
*
*    "If the value is 0, the memory area of the entire structure is assigned to the field symbol.
*    ASSIGN wa-(0) TO FIELD-SYMBOL(<fs_m10>).

    "The above statements replace the following syntax
    ASSIGN COMPONENT 'CARRID' OF STRUCTURE wa TO FIELD-SYMBOL(<fs_m11>).

    ASSIGN COMPONENT 5 OF STRUCTURE wa TO FIELD-SYMBOL(<fs_m12>).

    "Dynamically specifying components of structures that are referenced by
    "a data reference variable

    ASSIGN ref_m->('CARRNAME') TO FIELD-SYMBOL(<fs_m13>).

*    output->display( input = <fs_m6> name = `<fs_m6>` ).
*    output->display( input = <fs_m7> name = `<fs_m7>` ).
*    output->display( input = subrc1 name = `subrc1` ).
*    output->display( input = subrc2 name = `subrc2` ).
*    output->display( input = <fs_m9> name = `<fs_m9>` ).
*    output->display( input = <fs_m10> name = `<fs_m10>` ).
    output->display( input = <fs_m11> name = `<fs_m11>` ).
    output->display( input = <fs_m12> name = `<fs_m12>` ).
    output->display( input = <fs_m13> name = `<fs_m13>` ).

**********************************************************************

    output->next_section( `23) Dynamic Specifications in ASSIGN Statements (4) - Type Casting` ).

    "As covered above, the CASTING addition of the ASSIGN statement
    "has dynamic syntax elements.
    DATA dobj_c_l5 TYPE c LENGTH 5 VALUE 'abcde'.
    TYPES dtype_c_l2 TYPE c LENGTH 2.
    FIELD-SYMBOLS <fs_dyn_as> TYPE data.

    "A text literal with the name of a type is specified within the parentheses.
    ASSIGN dobj_c_l5 TO <fs_dyn_as> CASTING TYPE ('DTYPE_C_L2').

    output->display( input = <fs_dyn_as> name = `<fs_dyn_as1>` ).

**********************************************************************

    output->next_section( `Dynamically Creating Data Objects at Runtime Using Dynamic Type Definitions` ).
    output->display( `24) Miscellaneous Data Objects (1)` ).

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
      output->display( |***** Loop iteration { sy-tabix }. Type: { refwa->* } *****| ).

      CASE refwa->*.
        WHEN `I`.
          CREATE DATA dataref TYPE (refwa->*).
          dataref->* = 123.

          output->display( input = dataref->* name = `dataref->*` ).

          CREATE DATA dataref TYPE TABLE OF (refwa->*).

          INSERT 1 INTO TABLE dataref->*.
          INSERT 2 INTO TABLE dataref->*.

          output->display( input = dataref->* name = `dataref->*` ).

          CREATE DATA dataref TYPE REF TO (refwa->*).
          dataref->* = REF i( 456 ).

          output->display( input = dataref->* name = `dataref->*` ).
        WHEN `STRING`.
          CREATE DATA dataref TYPE (refwa->*).
          dataref->* = `hello`.

          output->display( input = dataref->* name = `dataref->*` ).

          CREATE DATA dataref TYPE TABLE OF (refwa->*).
          INSERT `hello` INTO TABLE dataref->*.
          INSERT `abap` INTO TABLE dataref->*.

          output->display( input = dataref->* name = `dataref->*` ).

          CREATE DATA dataref TYPE REF TO (refwa->*).
          dataref->* = REF string( `hi` ).

          output->display( input = dataref->* name = `dataref->*` ).
        WHEN `ZDEMO_ABAP_CARR`.
          CREATE DATA dataref TYPE (refwa->*).
          SELECT SINGLE * FROM zdemo_abap_carr INTO @dataref->*.

          output->display( input = dataref->* name = `dataref->*` ).

          CREATE DATA dataref TYPE TABLE OF (refwa->*).
          SELECT * FROM zdemo_abap_carr INTO TABLE @dataref->* UP TO 3 ROWS.

          output->display( input = dataref->* name = `dataref->*` ).

          CREATE DATA dataref TYPE REF TO (refwa->*).
          SELECT SINGLE * FROM zdemo_abap_carr INTO NEW @dataref->*.

          output->display( input = dataref->* name = `dataref->*` ).
      ENDCASE.
    ENDLOOP.

**********************************************************************

    output->next_section( `25) Elementary Data Object (2)` ).

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
            output->display( `That didn't work.` ).
        ENDCASE.

        "Getting type information using RTTI
        DATA(descr_builtin_type) = CAST cl_abap_elemdescr(
          cl_abap_typedescr=>describe_by_data( ref_bt->* ) ).

        output->display( |Built-in type determined at runtime: { b_type-builtin_type } | ).
        output->display( `Created data object at runtime:` ).
        output->display( input = descr_builtin_type name = `descr_builtin_type` ).

      CATCH cx_root.
        output->display( `Something went wrong.` ).
    ENDTRY.

**********************************************************************

    output->next_section( `26) Structure (3)` ).

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

    output->display( |Structured data type/database table name determined at runtime: { type4struc } | ).
    output->display( input = ref_dynstruc->* name = `ref_dynstruc->*`  ).

**********************************************************************

    output->next_section( `27) Internal Table (4)` ).

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

    output->display( |Table/type name determined at runtime: { type_name } | ).
    output->display( |At most, { random_upto } lines should have been read from the database table.| ).
    output->display( input = ref_n->* name = `ref_n->*` ).

**********************************************************************

    output->next_section( `28) Excursion: Absolute Type Names for Dynamically Specifying Types` ).

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

    output->display( `No output for this section. See the code.` ).

**********************************************************************

    output->next_section( `Dynamically Specifying Components/Clauses in Statements for Processing Internal Tables with ...` ).
    output->display( `29) SORT` ).

    "A field is determined at runtime on whose basis a sorting is done on an
    "internal table.

    DATA(field_name) = lcl_det_at_runtime=>get_dyn_field( ).

    SELECT *
     FROM zdemo_abap_carr
     ORDER BY carrid
     INTO TABLE @DATA(carr_itab)
     UP TO 3 ROWS.

    SORT carr_itab BY (field_name).

    output->display(  |Field name determined at runtime | &&
    |by which the sorting was done: { field_name } | ).
    output->display( input = carr_itab name = `carr_itab` ).

**********************************************************************

    output->next_section( `30) READ TABLE` ).

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
        output->display( input = wa_comps-comp name = `wa_comps-comp` ).
        output->display( input = wa_comps-value name = `wa_comps-value` ).
        output->display( input = read_line name = `read_line` ).
        CLEAR read_line.
      ELSE.
        output->display( input = wa_comps-comp name = `wa_comps-comp` ).
        output->display( input = wa_comps-value name = `wa_comps-value` ).
        output->display( `Line not found.` ).
      ENDIF.

    ENDLOOP.

**********************************************************************

    output->next_section( `31) MODIFY` ).

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

    output->display( `Internal table content before modifications:` ).
    output->display( input = itab_mod_tab_dyn name = `itab_mod_tab_dyn` ).

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
          output->display( |Invalid WHERE condition "{ condition }".| ).
      ENDTRY.
    ENDLOOP.

    output->display( `Internal table content after modifications:` ).
    output->display( input = itab_mod_tab_dyn name = `itab_mod_tab_dyn` ).

**********************************************************************

    output->next_section( `32) DELETE` ).

    "Dynamic WHERE condition in DELETE statements

    DATA itab_del_tab_dyn TYPE TABLE OF i WITH EMPTY KEY
                   WITH NON-UNIQUE SORTED KEY skey COMPONENTS table_line.

    itab_del_tab_dyn = VALUE #( ( 100 )
                                ( 200 )
                                ( 300 )
                                ( 400 )
                                ( 500 )
                                ( 600 ) ).

    output->display( `Internal table content before modifications:` ).
    output->display( input = itab_del_tab_dyn name = `itab_del_tab_dyn` ).

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

          output->display( |Condition: { condition }| ).
          output->display( input = itab_del_tab_dyn name = `itab_del_tab_dyn` ).

        CATCH cx_sy_itab_dyn_loop.
          output->display( |Invalid WHERE condition "{ condition }".| ).
      ENDTRY.
    ENDDO.

**********************************************************************

    output->next_section( `33) LOOP` ).

    "Dynamic specification of the key in LOOP statements
    "In the example, the loop can be executed with the entries 'skey' and 'primary_key'.
    "This is not case sensitive. Any other entries produce a runtime error.

    DATA(keys) = VALUE string_table( ( `primary_key` ) ( `SKEY` ) ).

    DATA itab_loop TYPE TABLE OF i
              WITH NON-UNIQUE KEY primary_key COMPONENTS table_line
              WITH NON-UNIQUE SORTED KEY skey COMPONENTS table_line.

    itab_loop = VALUE #( ( 3 ) ( 2 ) ( 1 ) ).

    DATA itab_dyn_key LIKE itab_loop.

    LOOP AT keys INTO DATA(k).

      LOOP AT itab_loop INTO DATA(wa_lo) USING KEY (k).
        APPEND wa_lo TO itab_dyn_key.
      ENDLOOP.

      output->display( |Loop over internal table using key "{ k }".| ).
      output->display( input = itab_dyn_key name = `itab_dyn_key` ).
      CLEAR itab_dyn_key.

    ENDLOOP.

**********************************************************************

    output->next_section( `Dynamically Specifying Clauses in ABAP SQL SELECT Statements` ).
    output->display( `34) SELECT List` ).

    "In the example, the SELECT list that is used in a SELECT statement is
    "determined at runtime.

    DATA(select_list) = lcl_det_at_runtime=>get_dyn_select_list( ).

    DATA sel_table TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY.

    SELECT (select_list)
     FROM zdemo_abap_flsch
     ORDER BY carrid
     INTO CORRESPONDING FIELDS OF TABLE @sel_table
     UP TO 3 ROWS.

    output->display( |SELECT list determined at runtime: { select_list } | ).
    output->display( input = sel_table name = `sel_table` ).

**********************************************************************

    output->next_section( `35) FROM Clause` ).

    "In the example, the FROM clause that is used in a SELECT statement is
    "determined at runtime. Here, the number of entries of a database table
    "is counted.

    DATA(tab_name) = lcl_det_at_runtime=>get_dyn_table_name( ).

    SELECT COUNT(*)
     FROM (tab_name)
     INTO @DATA(count).

    output->display( |Table name determined at runtime: { tab_name } | ).
    output->display( |The table { tab_name } has { count } entries.| ).

**********************************************************************

    output->next_section( `36) WHERE Clause` ).

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

    output->display( |WHERE clause determined at runtime:| ).
    output->display( input =  where_clause name = `where_clause` ).
    output->display( input = where_tab name = `where_tab` ).

**********************************************************************

    output->next_section( `37) Excursion: Multiple Dynamically Specified ` &&
                          `Clauses in an ABAP SQL SELECT Statement` ).

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

      output->display( `Dynamically determined syntax elements:` ).
      output->display( input = dyn_syntax_elem name = `dyn_syntax_elem` ).

      SELECT (dyn_syntax_elem-select_list)
        FROM (dyn_syntax_elem-table)
        WHERE (dyn_syntax_elem-where_clause)
        ORDER BY (dyn_syntax_elem-order_by)
        INTO CORRESPONDING FIELDS OF TABLE @dyn_syntax_elem-target->*
        UP TO @dyn_syntax_elem-rows ROWS.

      output->display( input = dyn_syntax_elem-target->* name = `dyn_syntax_elem-target->*` ).

    ELSE.
      output->display( `There's an issue with syntax elements.` ).
    ENDIF.

**********************************************************************

    output->next_section( `38) Dynamic Invoke` ).

    "In the example, both class and method are determined at runtime for
    "the method call. The suitable parameter table is filled in the
    "method get_dyn_class_meth. The example is implemented in a way that
    "all the methods that are called store some text in a string. This
    "string is displayed to see the effect of the dynamic method call.

    lcl_det_at_runtime=>get_dyn_class_meth( IMPORTING cl = DATA(cl_name)
                                                      meth = DATA(meth_name)
                                                      ptab = DATA(p_tab) ).

    CALL METHOD (cl_name)=>(meth_name) PARAMETER-TABLE p_tab.

    output->display( |Class name determined at runtime: { cl_name } | ).
    output->display( |Method name determined at runtime: { meth_name } | ).

    output->display( `Result of method call (text stored in a variable):` ).
    output->display( input = lcl_det_at_runtime=>dyn_meth_call_result name = `lcl_det_at_runtime=>dyn_meth_call_result` ).

    "Further method calls
    "The class and method to be used is determined here by just providing
    "the character-like content (the name) via a data object in a predefined way.

    DATA method TYPE string VALUE `FILL_STRING`.

    "Note that the method has no parameters in this example.
    "Similar to above. The method stores some text in a string which is
    "displayed to see the effect of the dynamic method call.
    CALL METHOD lcl_det_at_runtime=>(method).

    output->display( input = lcl_det_at_runtime=>dyn_meth_call_result name = `lcl_det_at_runtime=>dyn_meth_call_result` ).

    DATA class TYPE string VALUE `LCL_DET_AT_RUNTIME`.

    CALL METHOD (class)=>fill_string.

    output->display( input = lcl_det_at_runtime=>dyn_meth_call_result name = `lcl_det_at_runtime=>dyn_meth_call_result` ).

**********************************************************************

    output->next_section( `39) RTTI: Getting Type Information at Runtime/Getting a Reference to a Type Description Object` ).

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
    output->display( input = type_descr_obj_elem_inl name = `type_descr_obj_elem_inl` ).

    "Various methods/attributes (note that they vary depending on the type) provide
    "you with detailed information.
    "The following examples show a selection.
    "Kind/Type kind/Output length
    DATA(kind_elem) = type_descr_obj_elem_inl->kind.
    DATA(type_kind_elem) = type_descr_obj_elem_inl->type_kind.
    DATA(output_length_elem) = type_descr_obj_elem_inl->output_length.

    output->display( input = kind_elem name = `kind_elem` ).
    output->display( input = type_kind_elem name = `type_kind_elem` ).
    output->display( input = output_length_elem name = `output_length_elem` ).

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

    output->display( input = type_descr_obj_elem_inl_2->kind name = `type_descr_obj_elem_inl_2->kind` ).
    output->display( input = type_descr_obj_elem_inl_2->type_kind name = `type_descr_obj_elem_inl_2->type_kind` ).

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

    output->display( input = struc_kind name = `struc_kind` ).
    output->display( input = comps_struc name = `comps_struc` ).
    output->display( input = comps_struc2 name = `comps_struc2` ).
    output->display( input = struct_kind name = `struct_kind` ).

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

    output->display( input = tab_kind name = `tab_kind` ).
    output->display( input = tab_keys name = `tab_keys` ).
    output->display( input = tab_keys2 name = `tab_keys2` ).
    output->display( input = tab_comps name = `tab_comps` ).

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

    output->display( input = ref_kind name = `ref_kind` ).
    output->display( input = ref_type name = `ref_type` ).
    output->display( input = ref_type_abs_name name = `ref_type_abs_name` ).
    output->display( input = ref_type_type_kind name = `ref_type_type_kind` ).

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

    output->next_section( `40) RTTI: Getting Type Information at Runtime for Miscellaneous Types` ).

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

    output->display( |Type name determined at runtime: { get_type }| ).

    DATA dref TYPE REF TO data.

    IF get_type <> `LCL_DET_AT_RUNTIME`
    AND get_type <> `IF_OO_ADT_CLASSRUN`.
      TRY.
          CREATE DATA dref TYPE (get_type).
        CATCH cx_sy_create_data_error.
          output->display( `Create data error!` ).
      ENDTRY.

      "Retrieving type information
      "When referring to a concrete data object name, you can use this method:
      DATA(some_type) = cl_abap_typedescr=>describe_by_data( dref->* ).

      "Elementary type
      IF some_type->kind = cl_abap_typedescr=>kind_elem.
        DATA(el) = CAST cl_abap_elemdescr( some_type ).
        output->display( input = el name = `el` ).
        "Various attributes and methods possible
        output->display( input = el->type_kind name = `el->type_kind` ).
        output->display( input = el->absolute_name name = `el->absolute_name` ).
        output->display( input = el->get_relative_name( ) name = `el->get_relative_name( )` ).

        "Structure
      ELSEIF some_type->kind = cl_abap_typedescr=>kind_struct.
        DATA(stru) = CAST cl_abap_structdescr( some_type ).
        output->display( input = stru->absolute_name name = `stru->absolute_name` ).
        output->display( input = stru->components name = `stru->components` ).
        output->display( input = stru->struct_kind name = `stru->struct_kind` ).
        output->display( input = stru->get_components( ) name = `stru->get_components( )` ).

        "Internal table
      ELSEIF some_type->kind =  cl_abap_typedescr=>kind_table.
        DATA(tab) = CAST cl_abap_tabledescr( some_type ).
        output->display( input = tab->absolute_name name = `tab->absolute_name` ).
        output->display( input = tab->table_kind name = `tab->table_kind` ).
        output->display( input = tab->get_keys( ) name = `tab->get_keys` ).
        output->display( input = tab->get_table_line_type( ) name = `tab->get_table_line_type( )` ).

        "Reference
      ELSEIF some_type->kind =  cl_abap_typedescr=>kind_ref.
        DATA(ref_descr) = CAST cl_abap_refdescr( some_type ).
        output->display( input = ref_descr->absolute_name name = `ref_descr->absolute_name` ).
        output->display( input = ref_descr->get_referenced_type( ) name = `ref_descr->get_referenced_type( )` ).
      ELSE.
        output->display( `Others ...` ).
      ENDIF.

    ELSE.

      "Retrieving type information
      "Here, using the type name and not a concrete data object as above.
      some_type = cl_abap_typedescr=>describe_by_name( get_type ).

      "Class
      IF some_type->kind =  cl_abap_typedescr=>kind_class.
        DATA(class_desc) = CAST cl_abap_classdescr( some_type ).
        output->display( input = class_desc->absolute_name name = `class_desc->absolute_name` ).
        output->display( input = class_desc->attributes name = `class_desc->attributes` ).
        output->display( input = class_desc->methods name = `class_desc->methods` ).

        "Creating an object based on a type determined at runtime
        DATA oref TYPE REF TO object.

        TRY.
            CREATE OBJECT oref TYPE (get_type).
            "Retrieving type information
            DATA(descr_ref) = cl_abap_typedescr=>describe_by_object_ref( oref ).
            output->display( input = descr_ref->absolute_name name = `descr_ref->absolute_name` ).
            output->display( input = descr_ref->kind name = `descr_ref->kind` ).
          CATCH cx_root.
            output->display( `Error` ).
        ENDTRY.

        "Interface
      ELSEIF some_type->kind =  cl_abap_typedescr=>kind_intf.
        DATA(if_descr) = CAST cl_abap_intfdescr( some_type ).
        output->display( input = if_descr->absolute_name name = `if_descr->absolute_name` ).
        output->display( input = if_descr->methods name = `class_desc->methods` ).
      ELSE.
        output->display( `Others ...` ).
      ENDIF.
    ENDIF.

**********************************************************************

    output->next_section( `41) RTTC: Dynamically Creating Data Types at Runtime` ).

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

    output->display( `No output for this section. See the code.` ).

**********************************************************************

    output->next_section( `42) Dynamically Creating Data Objects at Runtime Using Type Description Objects (1) - Miscellaneous` ).

    "Using the TYPE HANDLE addition to CREATE DATA statements, you can
    "dynamically create data objects at runtime based on type description objects.
    "The following example uses type description objects from the previous example.
    "For output purposes, the created data objects are assigned values.

    DATA dref_typ_obj TYPE REF TO data.

    "Elementary data object
    CREATE DATA dref_typ_obj TYPE HANDLE tdo_elem_i.
    dref_typ_obj->* = 5 + 4.

    output->display( input = dref_typ_obj->* name = `dref_typ_obj->*` ).

    "Structured data object
    CREATE DATA dref_typ_obj TYPE HANDLE tdo_struc.
    dref_typ_obj->('A') = `hello`.
    dref_typ_obj->('B') = 4 + 3.
    dref_typ_obj->('C') = 'abcde'.
    dref_typ_obj->('D') = '1.234'.

    output->display( input = dref_typ_obj->* name = `dref_typ_obj->*` ).

    "Internal table
    CREATE DATA dref_typ_obj TYPE HANDLE tdo_tab_2.
    SELECT * FROM zdemo_abap_flsch INTO TABLE @dref_typ_obj->* UP TO 3 ROWS.

    output->display( input = dref_typ_obj->* name = `dref_typ_obj->*` ).

    "Reference
    CREATE DATA dref_typ_obj TYPE HANDLE tdo_ref_3.
    dref_typ_obj->* = NEW t( '120000' ).

    output->display( input = dref_typ_obj->* name = `dref_typ_obj->*` ).

**********************************************************************

    output->next_section( `43) Dynamically Creating Data Objects at Runtime Using Type Description Objects (2) - Structure` ).

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

    output->display( input = dref_struc->* name = `dref_struc->*` ).

**********************************************************************

    output->next_section( `44) Dynamically Creating Data Objects at Runtime Using Type Description Objects (3) - Internal Table` ).

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

    output->display( |Type/Database table name determined at runtime: { table_name }| ).
    output->display( |Internal table entries (ordered by { dyn_order_by }):| ).
    output->display( input = ref_tab->* name = `ref_tab->*` ).

  ENDMETHOD.
ENDCLASS.