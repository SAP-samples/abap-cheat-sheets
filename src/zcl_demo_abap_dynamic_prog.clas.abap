***********************************************************************
*
*           ABAP cheat sheet: Dynamic programming
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntactical options and concepts related
*   to dynamic programming as outlined in the respective ABAP cheat sheet.
* - Topics covered: field symbols and data references (both as supporting
*   elements for dynamic programming), dynamic ABAP syntax components,
*   runtime type services (RTTS), i. e. runtime type identification (RTTI)
*   and runtime type creation (RTTC)
* - To provide a "real" dynamic determination at runtime, the example
*   includes local classes in the CCIMP include (local types tab in ADT)
*   whose methods return character-like content to be used in the tokens
*   of ABAP statements. The content is predefined in these classes but
*   the content that is actually used in the end is completely random.
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

protected section.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_DEMO_ABAP_DYNAMIC_PROG IMPLEMENTATION.


  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_flight_tables=>fill_dbtabs( ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DATA(output) = NEW zcl_demo_abap_display( out ).

    output->display( `Demo: Dynamic Programming` ).
    output->display( `Field Symbols` ).
    output->display( `1) Declaring Field Symbols` ).

    "Field symbols are declared with the FIELD-SYMBOLS statement which is
    "also possible as chained statement using a colon. You provide the name
    "of the field symbol between angle brackets. You can either type them
    "with a complete data type or with a generic type.
    "The example includes some demo data declarations and type definitions
    "to refer to. Various field symbols are declared with both complete and
    "generic data types. Note: There are plenty of options for generic ABAP
    "types. The most prominent is 'data' that stands for any data type (the
    "older generic type 'any' has the same effect).

    "Some data declarations and type definitions
    DATA: str TYPE string.

    TYPES: BEGIN OF struc, "Structured data type
             num1 TYPE i,
             num2 TYPE i,
           END OF struc,
           tab_type TYPE TABLE OF struc. "Internal table type

    "Complete types
    FIELD-SYMBOLS: <fs_i>        TYPE i,
                   <fs_flsch>    TYPE zdemo_abap_flsch,
                   <fs_tab_type> TYPE LINE OF tab_type,
                   <fs_like>     LIKE str.

    "Generic types
    FIELD-SYMBOLS <fs_cseq>      TYPE csequence.
    FIELD-SYMBOLS <fs_data>      TYPE data.
    FIELD-SYMBOLS <fs_any>       TYPE any.
    FIELD-SYMBOLS <fs_any_table> TYPE ANY TABLE.

    output->display( `No output for this section.` ).

    output->next_section( `2) Assigning Data Objects to Field Symbols` ).

    "When assigning data objects to field symbols with the ASSIGN statement,
    "field symbols receive all properties and values from the data objects.
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

    "Assigning structure components to field symbols
    ASSIGN COMPONENT 2 OF STRUCTURE struc_a TO <fs_data_a>.
    ASSIGN COMPONENT 'CONNID' OF STRUCTURE struc_a TO <fs_data_a>.

    output->display( `No output for this section.` ).

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

    output->next_section( `4) Unassigning Data Objects from Field Symbols` ).

    "You can explicitly remove the assignment of a field symbol. After this,
    "the field symbol does not point to any data object any more. Note that
    "a CLEAR statement only initializes the value.
    "The example includes the declaration of a data object and its
    "assignment to a field symbol. It is then unassigned using an UNASSIGN
    "statement. The effect is demonstrated using logical expressions with IS
    "ASSIGNED.

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

    output->next_section( `5) Type Casting with Field Symbols` ).

    "When assigning data objects to fields symbols, you should pay attention
    "to compatible types of data object and field symbol. There is also an
    "ABAP syntax with which you can carry out type casting for incompatible
    "types. You can cast either implicitly or explicitly by specifying the
    "concrete type.
    "In the example, a data type and object are created. The data object is
    "assigned to a field symbol. In case of implicit casting, the field symbol
    "is typed with the created data type, so only CASTING is needed. In case
    "of explicit casting, the field symbol is typed with a generic type.
    "Here, just having CASTING would not be sufficient.
    "As a result, the type that only accepts 3 characters is respected.

    TYPES c_len_3 TYPE c LENGTH 3.

    DATA(chars) = 'abcdefg'.

    FIELD-SYMBOLS <fs_d1> TYPE c_len_3.

    ASSIGN chars TO <fs_d1> CASTING. "Implicit casting

    FIELD-SYMBOLS <fs_d2> TYPE data.

    ASSIGN chars TO <fs_d2> CASTING TYPE c_len_3. "Explicit casting

    output->display( input = <fs_d1> name = `<fs_d1>` ).
    output->display( input = <fs_d2> name = `<fs_d2>` ).

    output->next_section( `6) Accessing Field Symbols` ).

    "When accessing field symbols, you just address the value of the
    "assigned data object. You can use the field symbols as any other data
    "object.
    "The example includes multiple data objects that are assigned to field
    "symbols. It is demonstrated that field symbols are accessed in various
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

    "Change values
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

    "Note: Declarations not possible.
    SELECT *
      FROM zdemo_abap_carr
      ORDER BY carrid
      INTO TABLE @<fs_anytab_e>
      UP TO 3 ROWS.

    output->display( input = <fs_anytab_e> name = `<fs_anytab_e>` ).

    output->next_section( `7) Using Field Symbols when Processing ` &&
    `Internal Tables` ).

    "Field symbols are often used when working with internal tables, for
    "example, in LOOP statements. In this context, field symbols are very
    "handy. You can avoid an actual copying of content to a work area during
    "the loop. In doing so, the loop is considerably faster especially when
    "dealing with large tables. You can assign the field symbol using the
    "ASSIGNING addition. Using ASSIGNING FIELD-SYMBOL(...), you can directly
    "declare and assign the field symbol in one go.
    "The example includes multiple loops. First, internal tables are
    "declared. One of them is filled. Then, field symbols are declared to
    "which data objects are assigned. In the first loop, a previously
    "declared field symbol is used as target area to hold the table line
    "that
    "is processed. In the course of the loop, some values are changed. The
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

      "Another itab is filled.
      <fs_tab_f> = VALUE #( BASE <fs_tab_f> ( <fs_struc_f> ) ).
    ENDLOOP.

    output->display( input = tab_f1 ).

    "Regrding the field symbol, the data type is derived automatically.
    LOOP AT <fs_tab_f> ASSIGNING FIELD-SYMBOL(<fs_struc_f2>).
      <fs_struc_f2>-connid = '100'.
      <fs_struc_f2>-fldate = cl_abap_context_info=>get_system_date( ) + 1.
      <fs_struc_f2>-price = <fs_struc_f2>-price - 50.
      <fs_struc_f2>-currency = 'USD'.

      "Another itab is filled.
      <fs_anytab_f> = VALUE #( BASE <fs_anytab_f> ( <fs_struc_f2> ) ).
    ENDLOOP.

    output->display( input = <fs_tab_f> name = `<fs_tab_f>` ).
    output->display( input = <fs_anytab_f> name = `<fs_anytab_f>` ).

    output->next_section( `8) Using Field Symbols to Process ` &&
    `All Components of a Structure` ).

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

    output->display( `No output for this section.` ).

    output->next_section( `10) Creating Data References ` &&
    `to Existing Data Objects` ).

    "Using the reference operator REF, you can get a data reference to an
    "existing data object. The older syntax GET REFERENCE has the same
    "effect as using the newer reference operator.
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

    "Older syntax: GET REFERENCE
    "Note: This example is only possible in unrestricted language scope.
    "If you are in an environment allowing unrestricted language scope,
    "you can comment the following statements in.
*    GET REFERENCE OF number_b INTO ref_b1.
*    GET REFERENCE OF number_b INTO DATA(ref_b4).
*    GET REFERENCE OF `abcdef` INTO DATA(ref_b5).

    output->display( `No output for this section.` ).

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

    output->next_section( `12) Copying Existing Data References` ).

    "You can copy a data reference into another one. Note that static types
    "of both data reference variables must be compatible and that only the
    "reference is copied and not the data object as such. That means that,
    "when copied, both data reference variables point to the same data
    "object.
    "Data references also support upcasts and downcasts in case a data
    "reference variable is of generic type in the assignment.
    "Upcast here: The target data reference variable is of generic type, the
    "source variable is of complete type. The assignment is done with the
    "assignment operator '='.
    "Downcast here: The target data reference variable is of complete type, the
    "source variable is of generic type. The assignment is done with casting
    "operators, either with the constructor operator CAST or the older '?='.

    "Declaring data reference variables
    DATA ref_d1 TYPE REF TO i.
    DATA ref_d2 TYPE REF TO i.

    ref_d1 = NEW #( 789 ).

    ref_d2 = ref_d1. "Copying data reference

    "Casting
    DATA(ref_d3) = NEW i( 321 ). "Complete type

    DATA ref_data_d1 TYPE REF TO data. "Generic type

    ref_data_d1 = ref_d3. "Upcast

    "Downcasts
    DATA ref_d5 TYPE REF TO i.

    DATA ref_data_d2 TYPE REF TO data. "Generic type

    ref_data_d2 = NEW i( 654 ).

    ref_d5 = CAST #( ref_data_d2 ).

    ref_d5 ?= ref_data_d2.

    output->display( input = ref_d2->* name = `ref_d2->*` ).
    output->display( input = ref_data_d1->* name = `ref_data_d1->*` ).
    output->display( input = ref_d5->* name = `ref_d5->*` ).

    output->next_section( `13) Accessing Data References ` ).

    "The content of data objects a data reference refers to can only be
    "accessed via dereferencing data reference variables using the
    "dereferencing operator '->*'.
    "Note: When dereferencing a data reference variable that has a
    "structured data type, you can use the component selector '->' to access
    "individual components.
    "The example includes multiple occasions in which data reference are
    "accessed: Changing the content of a referenced data object, the use in
    "logical expressions, structures, and internal tables.

    "Creating data reference variables and assigning values
    DATA(ref_e1) = NEW i( 1 ).
    DATA(ref_e2) = NEW zdemo_abap_carr( carrid   = 'LH'
                                        carrname = 'Lufthansa' ).

    "Generic type
    DATA ref_data_e TYPE REF TO data.

    "Copying reference
    ref_data_e = ref_e1.

    "Accessing
    "Variable receives the content.
    DATA(some_num) = ref_e1->*.

    output->display( input = ref_e1->* name = `ref_e1->*` ).

    "Content of referenced data object is changed.
    ref_e1->* = 10.

    output->display( input = ref_e1->* name = `ref_e1->*` ).

    "Data reference used in a logical expression.
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
    DATA(carrid) = ref_e2->carrid.

    ref_e2->carrid = 'UA'.

    output->display( input = ref_e2->carrid name = `ref_e2->carrid` ).

    "This syntax also works but it's less comfortable.
    ref_e2->*-carrname = 'United Airlines'.

    output->display( input = ref_e2->*-carrname name = `ref_e2->*-carrname` ).

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

    output->next_section( `15) Clearing Data References` ).

    "If you explicitly want to remove a reference from a data reference
    "variable, you can use a CLEAR statement. However, the garbage collector
    "takes over the reference removal automatically once the data is not
    "used any more by a reference.

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

    output->next_section( `16) Overwriting Data Reference Variables` ).

    "A data reference variable is overwritten when a new object is created
    "with a data reference variable already pointing to a data object.

    DATA ref_h1 TYPE REF TO data.

    ref_h1 = NEW i( 111 ).

    output->display( input = ref_h1->* name = `ref_h1->*` ).

    ref_h1 = NEW string( `ref_h1 overwritten.` ).

    output->display( input = ref_h1->* name = `ref_h1->*` ).

    output->next_section( `17) Keeping Data References`).

    "If your use case is to retain the data references and you want to
    "prevent that data references are overwritten when using the same
    "reference variable, you can put the reference variables in internal
    "tables.
    "The example demonstrates that three data references are created with
    "the same reference variable in the course of a DO loop. There, the data
    "reference is overwritten. However, due to saving the data reference
    "variable in an internal table, the content is preserved.

    DATA: ref_data_i TYPE REF TO data,
          itab_i     TYPE TABLE OF REF TO data,
          number_i   TYPE i VALUE 0.

    DO 3 TIMES.
      number_i += 1.  "Add up 1 to demonstrate a changed data object.
      "Create data reference and assign value
      "In the course of the loop, the variable gets overwritten.
      ref_data_i = NEW i( number_i ).
      "Adds the reference to internal table.
      itab_i     = VALUE #( BASE itab_i ( ref_data_i ) ).
    ENDDO.

    output->display( input = itab_i name = `itab_i` ).
    output->display( input = `The derefenced value of the data reference - which ` &&
    `was changed in the course of the loop - in the second table ` &&
    `entry is ` && itab_i[ 2 ]->* && `.` ).

    output->next_section( `18) Processing Internal Tables Using ` &&
    `Data References ` ).

    "Similar to using field symbols, you can avoid the copying of table rows
    "into a work area, for example, in a loop using data reference variables
    "and a REFERENCE INTO statement. In doing so, the processing of internal
    "tables is much faster than copying table lines to a work area.
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

    output->next_section( `Dynamic ABAP Syntax Components` ).
    output->display( `20) Assignment of Dynamically ` &&
    `Determined Data Objects to Field Symbols` ).

    "A dynamically determined data object is assigned to a field symbol.
    "In this case, data objects that are declared in the public section of
    "the local demo class are assigned to a field symbol.

    DATA(dobj_name) = lcl_det_at_runtime=>get_dyn_dobj( ).

    ASSIGN lcl_det_at_runtime=>(dobj_name) TO FIELD-SYMBOL(<fs_m>).

    output->display( |Data object name determined at runtime: { dobj_name } | ).
    output->display( |The content of the data object is: { <fs_m> } | ).

    output->next_section( `21) Dynamic Specification of Field Name` ).

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

    output->next_section( `22) Dynamic Specification of Data Type: ` &&
    `Creating Internal Table at Runtime and Filling It` ).

    "The example includes the creation of an internal table based on a data
    "type that is determined at runtime. Note that when using tokens, you
    "cannot use the NEW operator. The SELECT statement includes a token in
    "the FROM clause, too. In this example, the database table's name from
    "which to be read from is included in the dynamically determined type
    "name.

    DATA(type_name) = lcl_det_at_runtime=>get_dyn_table_name( ).

    DATA ref_n TYPE REF TO data.

    CREATE DATA ref_n TYPE TABLE OF (type_name).

    "Dynamic specification of a clause in an ABAP SQL statement
    SELECT *
      FROM (type_name)
      INTO TABLE @ref_n->*
      UP TO 3 ROWS.

    output->display( |Table/type name determined at runtime: { type_name } | ).
    output->display( input = ref_n->* name = `ref_n->*` ).

    output->next_section( `Dynamic Specification of Clauses ` &&
    `in SELECT Statements` ).
    output->display( `23) SELECT List` ).

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

    output->next_section( `24) FROM Clause` ).

    "In the example, the FROM clause that is used in a SELECT statement is
    "determined at runtime. Here, the number of entries of a database table
    "is counted.

    DATA(tab_name) = lcl_det_at_runtime=>get_dyn_table_name( ).

    SELECT COUNT(*)
     FROM (tab_name)
     INTO @DATA(count).

    output->display( |Table name determined at runtime: { tab_name } | ).
    output->display( |The table { tab_name } has { count } entries.| ).

    output->next_section( `25) WHERE Clause` ).

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

    output->next_section( `26) Dynamic Specification of Classes and Methods` ).

    "The example includes a dynamic method call. In this case, both class
    "and method are determined at runtime.
    "The implementation represents a rudimentary method test. The CALL
    "METHOD statement includes the addition PARAMETER-TABLE. Here, you
    "specify an internal table of type ABAP_PARMBIND_TAB for the parameter
    "transfer. The filling of the table must be done with care, i. e. it
    "must contain exactly one line for each non-optional formal parameter.
    "Furthermore, it can include a line for each optional formal parameter.
    "Actually, you would need to individually fill the table for the method
    "calls.
    "The implementation here only uses two demo classes and its methods from
    "within the program. To really test out your methods, you would need to
    "provide, for example, proper input values for importing parameters of
    "your methods. Here, the methods often have only a returning
    "parameter, so other kinds of parameters are of no relevance in the
    "example. The example is intended to give
    "you a rough idea about how to work with the syntax.
    "First, the class is determined at runtime. The example anticipates
    "RTTI. Using RTTI, the method names of the determined class are
    "retrieved. A random method is chosen. RTTI returns a table for the
    "individual method information. This table is looped across. In the
    "course of the loop, the parameter table is filled. The value field
    "expects a data reference. Using RTTI, the type is determined and based
    "on this type, a data reference is created to fill the value field.
    "After the dynamic method call, the parameters table is output as well
    "as the class and method names used.

    DATA(class) = lcl_det_at_runtime=>get_dyn_class( ).

    "Data declarations for parameter table
    DATA: parameter  TYPE abap_parmbind,
          parameters TYPE abap_parmbind_tab.

    "Getting methods using RTTI
    DATA(methods) = CAST cl_abap_objectdescr(
                      cl_abap_objectdescr=>describe_by_name( class )
                    )->methods.

    "Since the constructors are present in the result table, too,
    "they are deleted here from the table.
    "They cannot be called directly.
    DELETE methods WHERE name = `CLASS_CONSTRUCTOR`.
    DELETE methods WHERE name = `CONSTRUCTOR`.

    "Choosing a random method
    IF methods IS NOT INITIAL.
      DATA(random) = cl_abap_random_int=>create(
       seed = cl_abap_random=>seed( ) min = 1 max = lines( methods ) ).
      DATA(idx) = random->get_next( ).

      "Retrieving method name
      DATA(meth) = to_upper( methods[ idx ]-name ).

      "Looping across the parameters that are contained in a table
      "to get their parameters and fill the parameter table accordingly
      LOOP AT methods[ name = meth ]-parameters
      ASSIGNING FIELD-SYMBOL(<fs>).
        parameter-name = <fs>-name.

        "Note: If the method signature has an importing parameter,
        "it must be specified as exporting parameter here.
        "Same is true for the exporting parameter in the signature
        "that must be specified as importing parameter here.
        parameter-kind = SWITCH #( <fs>-parm_kind
               WHEN  cl_abap_objectdescr=>exporting
                  THEN cl_abap_objectdescr=>importing
               WHEN  cl_abap_objectdescr=>importing
                  THEN cl_abap_objectdescr=>exporting
               WHEN  cl_abap_objectdescr=>returning
                  THEN cl_abap_objectdescr=>returning
               WHEN  cl_abap_objectdescr=>changing
                  THEN cl_abap_objectdescr=>changing
               WHEN  cl_abap_objectdescr=>receiving
                  THEN cl_abap_objectdescr=>receiving ).

        "Getting the parameter types using RTTI
        DATA(param) = CAST cl_abap_objectdescr(
                        cl_abap_objectdescr=>describe_by_name( class )
                      )->get_method_parameter_type( EXPORTING
                             p_method_name = meth
                             p_parameter_name  = <fs>-name ).

        "To fill the value field in the parameter table,
        "a reference must be specified. Here, the type of the parameter
        "is only known at runtime. Hence, an anonymous data object is
        "created to be referred to.
        DATA t TYPE REF TO data.

        CREATE DATA t TYPE (param->absolute_name).

        "Getting the reference of dereferenced anonymous data object
        "into the value field.
        parameter-value = REF #( t->* ).

        "Inserting the line into the parameters table.
        INSERT parameter INTO TABLE parameters.
      ENDLOOP.

      TRY.
          IF parameters IS NOT INITIAL.
            "Dynamic method call using a CALL METHOD statement.
            CALL METHOD (class)=>(meth) PARAMETER-TABLE parameters.

            output->display( |Class determined at runtime: { class } | ).
            output->display( |Method determined at runtime: { meth } | ).
            output->display( |Parameters table:| ).
            output->display( input = parameters name = `parameters` )..
          ENDIF.
        CATCH cx_sy_dyn_call_error INTO DATA(error_params).
          output->display( input = error_params->get_longtext(  ) name = `error_params` ).
      ENDTRY.
    ENDIF.

    output->next_section( `27) RTTI: Get Type Descriptions` ).

    "In the example, RTTI is used to get type descriptions based on the name
    "of a random type. The CASE statement handles the following type kinds:
    "elementary, structure, internal table, reference, class, interface. In
    "case of an elementary type, structure an internal table, the type
    "description is output. In case of a reference type, the referenced type
    "is output. In case of a class and interface, their methods are output.

    "Getting random type
    DATA(get_type) = lcl_det_at_runtime=>get_random_type( ).

    output->display( |Type name determined at runtime: { get_type }| ).

    "Getting type description
    DATA(some_type) = cl_abap_typedescr=>describe_by_name( get_type ).

    "When referring to a concrete data object, you can use this method:
    "DATA(some_type) = cl_abap_typedescr=>describe_by_data( ... ).

    CASE some_type->kind.
        "Elementary type
      WHEN cl_abap_typedescr=>kind_elem.
        DATA(element_descr) = CAST cl_abap_elemdescr( some_type ).
        output->display( input = element_descr name = `element_descr` ).

        "Get components of structure
      WHEN cl_abap_typedescr=>kind_struct.
        DATA(struct_descr) = CAST cl_abap_structdescr( some_type ).
        output->display( input = struct_descr->components name = `struct_descr->components` ).

        "Get components of table
      WHEN cl_abap_typedescr=>kind_table.
        DATA(table_descr) = CAST cl_abap_tabledescr( some_type ).
        DATA(tab_struc) = CAST cl_abap_structdescr(
          table_descr->get_table_line_type( ) ).
        output->display( input = tab_struc->components name = `tab_struc->components` ).

        "Get referenced type
      WHEN cl_abap_typedescr=>kind_ref.
        DATA(ref_descr) = CAST cl_abap_refdescr( some_type ).
        output->display( input = ref_descr->get_referenced_type( ) name = `ref_descr->get_referenced_type( )` ).

        "Get class methods
      WHEN cl_abap_typedescr=>kind_class.
        DATA(class_desc) = CAST cl_abap_classdescr( some_type ).
        output->display( input = class_desc->methods name = `class_desc->methods` ).

        "Get interface methods
      WHEN cl_abap_typedescr=>kind_intf.
        DATA(if_descr) = CAST cl_abap_intfdescr( some_type ).
        output->display( input = if_descr->methods name = `class_desc->methods` ).

      WHEN OTHERS.
        output->display( `Others ...` ).
    ENDCASE.

    output->next_section( `28) RTTC: Creating Data Type and Data ` &&
    `Object Based on this Type at Runtime` ).

    "In the example an internal table type is created based on a DDIC type.
    "First, the line type is retrieved using RTTI. The primary table keys
    "the dynamically created table type receives is determined using an
    "internal table of type abap_keydescr_tab. Using the method
    "cl_abap_tabledescr=>create, the internal table type is created.
    "The parameters are filled
    "accordingly. You can make use of the constants provided in class
    "cl_abap_tabledescr. In this case, the table type is sorted and has
    "unique keys. The keys that are passed have been specified before. Then,
    "an internal table is created based on this dynamically created type. In
    "the CREATE DATA statement, this type is referred to using the addition
    "TYPE HANDLE. Finally, the table is filled and output.

    "Getting the line type of a DDIC table
    DATA(line_type) =  CAST cl_abap_structdescr(
             cl_abap_tabledescr=>describe_by_name( `ZDEMO_ABAP_CARR` ) ).

    "Defining primary table keys of internal table to be created
    DATA(key_tab) = VALUE abap_keydescr_tab( ( name = 'CARRID' )
                                             ( name = 'CARRNAME' ) ).
    "Creating an internal table type
    DATA(table_type_sorted) = cl_abap_tabledescr=>create(
        p_line_type  = line_type
        p_table_kind = cl_abap_tabledescr=>tablekind_sorted
        p_unique     = cl_abap_typedescr=>true
        p_key        = key_tab ).

    "Creating an internal table based on the created table type
    DATA ref_tab TYPE REF TO data.
    CREATE DATA ref_tab TYPE HANDLE table_type_sorted.

    "Filling an internal table
    SELECT *
      FROM zdemo_abap_carr
      ORDER BY carrid
      INTO TABLE @ref_tab->*
      UP TO 3 ROWS.

    output->display( |Primary table keys of the created table type:| ).
    output->display( input = table_type_sorted->get_keys( ) name = `table_type_sorted->get_keys( )` ).

    output->display( |Internal table entries:| ).
    output->display( input = ref_tab->* name = `ref_tab->*` ).

    output->next_section( `29) RTTC: Creating Table Type with ` &&
    `Custom Components at Runtime` ).

    "In the example, an internal table type is created using custom
    "components.
    "First, a component description table is filled as the basis of the line
    "type. Here, names and types are provided similar to the demo table
    "zdemo_abap_carr. The type is created using both elementary types and DDIC
    "data elements whose properties are retrieved using RTTI.
    "As a next step, the structure description is created using
    "cl_abap_structdescr=>create. Then, based on this structure description,
    "an internal table type is created as shown in the previous example. The
    "table type is used to create an internal table using the addition TYPE
    "HANDLE as part of a CREATE DATA statement. Finally, to have some
    "content in this table, a SELECT statement is used to retrieve data
    "from zdemo_abap_carr.

    "Creating custom components based on built-in types and data elements in the DDIC
    DATA(custom_comp) = VALUE cl_abap_structdescr=>component_table(
       ( name = 'CARRIER_ID'   type = cl_abap_elemdescr=>get_c( 3 ) )
       ( name = 'CARRIER_NAME' type = cl_abap_elemdescr=>get_c( 20 ) )
       ( name = 'CURRENCY'     type = cl_abap_elemdescr=>get_c( 5 ) )
       ( name = 'WEBSITE'      type = cl_abap_elemdescr=>get_c( 255 ) ) ).

    "Note: If you are in an environment allowing unrestricted language scope,
    "you can exchange the 'currency' and 'website' component table entries with the
    "following ones. It demonstrates the type property retrieval from DDIC types.

*    ( name = 'CURRENCY'     type = CAST #(
*          cl_abap_elemdescr=>describe_by_name( 'S_CURRCODE' ) ) )
*    ( name = 'WEBSITE'      type = CAST #(
*          cl_abap_datadescr=>describe_by_name( 'S_CARRURL' ) ) ) ).

    "Getting structure description
    DATA(custom_structure) = cl_abap_structdescr=>create( custom_comp ).

    "Creating an internal table type
    DATA(table_type_std) = cl_abap_tabledescr=>create(
       p_line_type  = CAST #( custom_structure )
       p_table_kind = cl_abap_tabledescr=>tablekind_sorted
       p_unique     = abap_true
       p_key = VALUE abap_keydescr_tab( ( name = 'CARRIER_ID' ) ) ).

    "Creating an internal table based on the created table type
    DATA ref_tab_2 TYPE REF TO data.

    CREATE DATA ref_tab_2 TYPE HANDLE table_type_std.

    "Filling internal table
    SELECT carrid   AS carrier_id,
           carrname AS carrier_name,
           currcode AS currency,
           url      AS website
      FROM zdemo_abap_carr
      ORDER BY carrier_id
      INTO CORRESPONDING FIELDS OF TABLE @ref_tab_2->*
      UP TO 3 ROWS.

    output->display( input = ref_tab_2->* name = `ref_tab_2->*` ).

  ENDMETHOD.
ENDCLASS.
