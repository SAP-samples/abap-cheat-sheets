***********************************************************************
*
*           ABAP cheat sheet: Dynamic programming
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntactical options and concepts related
*   to dynamic programming.
* - Topics covered: Field symbols and data references (both as supporting
*   elements for dynamic programming), dynamic ABAP syntax components,
*   runtime type services (RTTS), i. e. runtime type identification (RTTI)
*   and runtime type creation (RTTC)
* - To provide a "real" dynamic determination at runtime, the example
*   includes local classes in the CCIMP include (local types tab in ADT)
*   whose methods return character-like content to be used in the
*   ABAP statements. The content is predefined in these classes but
*   the content that is actually used in the end is random.
*
* ----------------------- GETTING STARTED -----------------------------
* - Open the class with the ABAP Development Tools (ADT).
* - Choose F9 to run the class.
* - Check the console output.
* - To understand the context and the ABAP syntax used, check the notes
*   included in the class as comments and refer to the ABAP Keyword
*   Documentation.
* - Due to the amount of output in the console, the examples include
*   numbers (e. g. 1) ..., 2) ..., 3) ...) for the individual example
*   sections. Plus, the variable name is displayed in most cases. Hence,
*   to easier and faster find the relevant output in the console, just
*   search in the console for the number/variable name (CTRL+F in the
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
"! <p class="shorttext synchronized">ABAP cheat sheet: Dynamic programming</p>
"! Example to demonstrate concepts related to dynamic programming.<br>Choose F9 in ADT to run the class.
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

    output->display( `ABAP Cheat Sheet: Dynamic Programming` ).

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
    "Here, a chained statement using a colon.
    FIELD-SYMBOLS: <fs_i>        TYPE i,
                   <fs_flsch>    TYPE zdemo_abap_flsch,
                   <fs_tab_type> TYPE LINE OF tab_type,
                   <fs_like>     LIKE str.

    "Generic types
    "There are plenty of options for generic ABAP types. Check the
    "keyword docu.
    "The most prominent is 'data' that stands for any data type (the
    "older generic type 'any' has the same effect).
    FIELD-SYMBOLS <fs_cseq>      TYPE csequence.
    FIELD-SYMBOLS <fs_data>      TYPE data.
    FIELD-SYMBOLS <fs_any>       TYPE any.
    FIELD-SYMBOLS <fs_any_table> TYPE ANY TABLE.

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

    "Inline declaration is possible, too. The type is automatically derived.
    ASSIGN num_a TO FIELD-SYMBOL(<fs_inl>).

    "Assigning structure components to field symbols
    "Component position
    ASSIGN COMPONENT 2 OF STRUCTURE struc_a TO <fs_data_a>.
    "Component name
    ASSIGN COMPONENT 'CONNID' OF STRUCTURE struc_a TO <fs_data_a>.

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

    "Use the CASTING addition for matching types of data object and field symbol
    "when assigning memory areas. You can cast either implicitly or explicitly
    "by specifying the concrete type.
    "In the example, a data type and object are created. The data object is
    "assigned to a field symbol. In case of implicit casting, the field symbol
    "is typed with the created data type, so only CASTING is needed. In case
    "of explicit casting, the field symbol is typed with a generic type.
    "Here, just having CASTING would not be sufficient.
    "As a result, the type that only accepts 3 characters is respected.

    TYPES c_len_3 TYPE c LENGTH 3.

    DATA(chars) = 'abcdefg'.

    FIELD-SYMBOLS <fs_d1> TYPE c_len_3.

    "Implicit casting
    ASSIGN chars TO <fs_d1> CASTING.

    FIELD-SYMBOLS <fs_d2> TYPE data.

    "Explicit casting
    ASSIGN chars TO <fs_d2> CASTING TYPE c_len_3.

    output->display( input = <fs_d1> name = `<fs_d1>` ).
    output->display( input = <fs_d2> name = `<fs_d2>` ).

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

    output->display( input = tab_f1 ).

    "Regarding the field symbol, the data type is derived automatically.
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
      ASSIGN COMPONENT sy-index OF STRUCTURE <struct> TO <comp>.

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
    DATA(ref_b3) = REF string( `hallo` ).

    output->display( `No output for this section. See the code.` ).

**********************************************************************

    output->next_section( `11) Creating New Data Objects at Runtime` ).

    "You create a so-called anonymous data object at runtime by putting the
    "reference into the variable and providing the desired type. Use the
    "instance operator NEW. The older syntax CREATE DATA has the same effect
    "as using the newer instance operator.
    "The example includes various anonymous data objects. Note: To output
    "the content of the data reference variables, they must be dereferenced
    "first. The details are shown further down.

    "Declaring data reference variables
    DATA ref_c1     TYPE REF TO i. "Complete type
    DATA ref_data_c TYPE REF TO data. "Generic type

    "Creating anonymous data objects
    "Using the '#' sign and the explicit type: see REF #( ) above.
    ref_c1     = NEW #( ).
    ref_data_c = NEW string( ).

    "For directly assigning values, insert the values within the parentheses.
    ref_c1 = NEW #( 123 ).

    "Using inline declarations to omit a prior declaration of a variable.
    DATA(ref_c2) = NEW i( 456 ).

    "Internal table type
    TYPES i_table TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    "Filling internal table
    DATA(ref_c3) = NEW i_table( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).

    "Older syntax
    DATA ref_c4 TYPE REF TO string.
    DATA ref_c5 TYPE REF TO data.

    CREATE DATA ref_c4.
    CREATE DATA ref_c5 TYPE p LENGTH 6 DECIMALS 2.
    CREATE DATA ref_c5 LIKE ref_c4.

    output->display( input = ref_c1->* name = `ref_c1->*` ).
    output->display( input = ref_c2->* name = `ref_c2->*` ).
    output->display( input = ref_c3->* name = `ref_c3->*` ).

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

    "This syntax also works but it's less comfortable.
    ref_e2->*-carrname = 'United Airlines'.

    output->display( input = ref_e2->*-carrname name = `ref_e2->*-carrname` ).

**********************************************************************

    output->next_section( `14) Checking if Data Reference ` &&
    `Can Be Dereferenced` ).

    "You can check if a data reference can be dereferenced by using
    "a logical expression with IS [NOT] BOUND.
    "The example shows both a data reference that is bound and not bound.

    DATA(ref_f1) = NEW string( `hallo` ).
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

    DATA(ref_g1) = NEW string( `hallo` ).

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

    output->next_section( `17) Retaining Data References`).

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

    output->next_section( `19) Data References as Part of  ` &&
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
    output->display( `20) Assignment of Dynamically ` &&
    `Determined Data Objects to Field Symbols` ).

    "A dynamically determined data object is assigned to a field symbol.
    "In this case, data objects that are declared in the public section of
    "the local demo class are assigned to a field symbol.

    DATA(dobj_name) = lcl_det_at_runtime=>get_dyn_dobj( ).

    ASSIGN lcl_det_at_runtime=>(dobj_name) TO FIELD-SYMBOL(<fs_m>).

    output->display( |Data object name determined at runtime: { dobj_name } | ).
    output->display( |The content of the data object is: { <fs_m> } | ).

**********************************************************************

    output->next_section( `21) Dynamically Specifying a Data Object` ).

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

    output->next_section( `Dynamically Specifying Clauses in ABAP SQL SELECT Statements` ).
    output->display( `22) SELECT List` ).

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

    output->next_section( `23) FROM Clause` ).

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

    output->next_section( `24) WHERE Clause` ).

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

    output->next_section( `25) Dynamic Invoke` ).

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

    "Note that method has no parameters in this example.
    "Similar to above. The method stores some text in a string which is
    "displayed to see the effect of the dynamic method call.
    CALL METHOD lcl_det_at_runtime=>(method).

    output->display( input = lcl_det_at_runtime=>dyn_meth_call_result name = `lcl_det_at_runtime=>dyn_meth_call_result` ).

    DATA class TYPE string VALUE `LCL_DET_AT_RUNTIME`.

    CALL METHOD (class)=>fill_string.

    output->display( input = lcl_det_at_runtime=>dyn_meth_call_result name = `lcl_det_at_runtime=>dyn_meth_call_result` ).

**********************************************************************

    output->next_section( `26) RTTI: Determining Data and Object Types at Runtime` ).

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

    DATA: dref TYPE REF TO data.

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

    output->next_section( `27) RTTC: Dynamically Creating Elementary Data Objects` ).

    "The example demonstrates RTTC as follows:
    "- The method call takes care of providing the name of a built-in data type and more
    "- Depending on the type an elementary data object is created
    "- For demonstration purposes, RTTI is used to check on the created data object by
    "  retrieving the type description.

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
        DATA(descr_builtin_type) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( ref_bt->* ) ).

        output->display( |Built-in type determined at runtime: { b_type-builtin_type } | ).
        output->display( `Created data object at runtime:` ).
        output->display( input = descr_builtin_type name = `descr_builtin_type` ).

      CATCH cx_root.
        output->display( `Something went wrong.` ).
    ENDTRY.

**********************************************************************

    output->next_section( `28) RTTC: Dynamically Creating Structured Data Object (1)` ).

    "The example demonstrates RTTC as follows:
    "- The method call takes care of providing the name of a database table name.
    "- A structured data object is created based on the dynamically determined type.
    "  It is used as target data object for a SELECT statement.
    "- A SELECT loop is used to sequentially process the read database table lines.
    "  Here, the processed line is just output.
    "- The integer for the UP TO clause is specified at runtime.

    "Retrieving table name
    DATA(type4struc) = lcl_det_at_runtime=>get_dyn_table_name( ).

    DATA ref_dynstruc TYPE REF TO data.

    "Creating structure data object
    CREATE DATA ref_dynstruc TYPE (type4struc).

    "specifying random number for up to clause
    DATA(random_upto) = cl_abap_random_int=>create(
         seed = cl_abap_random=>seed( ) min = 2
                                        max = 6 )->get_next( ).

    output->display( |Structured data type/database table name determined at runtime: { type4struc } | ).
    output->display( |At most, { random_upto } lines should have been read from the database table.| ).

    "SELECT loop
    SELECT *
         FROM (type4struc)
         INTO @ref_dynstruc->*
         UP TO @random_upto ROWS.

      output->display( input = ref_dynstruc->*  ).

    ENDSELECT.

**********************************************************************

    output->next_section( `29) RTTC: Dynamically Creating Structured Data Object (2)` ).

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

    output->next_section( `30) RTTC: Dynamically Creating Internal Table (1)` ).

    "The example demonstrates RTTC as follows:
    "- The method call takes care of providing the name of a database table name.
    "- An internal table is created based on the dynamically determined type.
    "  It is used as target data object for a SELECT statement.
    "- The SELECT statement includes the dynamic specification of the FROM clause.

    "Retrieving table name
    DATA(type_name) = lcl_det_at_runtime=>get_dyn_table_name( ).

    DATA ref_n TYPE REF TO data.

    "Creating internal table based on the type determined at runtime
    CREATE DATA ref_n TYPE TABLE OF (type_name).

    "Dynamic specification of FROM clause
    SELECT *
      FROM (type_name)
      INTO TABLE @ref_n->*
      UP TO 3 ROWS.

    output->display( |Table/type name determined at runtime: { type_name } | ).
    output->display( input = ref_n->* name = `ref_n->*` ).

**********************************************************************

    output->next_section( `31) RTTC: Dynamically Creating Internal Table (2)` ).

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

      "Just for fun. The SELECT statement further down includes a dynamic specification
      "of the ORDER BY clause :)
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