***********************************************************************
*
*           ABAP cheat sheet: Data Types and Data Objects
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate data types and data objects.
* - Note that in many cases there is no output displayed because the
*   focus is on syntax options and declarations. In the class, you can
*   set breakpoints and use the debugger to check out data objects.
*   You can also use the F2 information for the many types and data
*   objects. Simply select a type or object in the code and press F2
*   in ADT to check out the information.
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
"! <p class="shorttext synchronized">ABAP cheat sheet: Data Types and Data Objects</p>
"! Example to demonstrate data types and data objects in ABAP.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_dtype_dobj DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.
    CLASS-METHODS:
      class_constructor.

    TYPES t_pub_text_c30 TYPE c LENGTH 30.
    CONSTANTS: comma TYPE string VALUE `, `.
    CLASS-DATA: read_only_attribute TYPE string VALUE `Hallo` READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES t_prv_text_c30 TYPE c LENGTH 30.
    CLASS-DATA cl_text TYPE t_prv_text_c30.
    DATA text TYPE t_pub_text_c30 VALUE '!!!'.

    METHODS adapt_text RETURNING VALUE(str) TYPE string.

    METHODS addition_with_generic_num IMPORTING num1          TYPE numeric
                                                num2          TYPE numeric
                                      RETURNING VALUE(result) TYPE decfloat34.

    constants no_output type string value `No output for this section. Check out the types in the code e.g. using the F2 information.`.

**********************************************************************

    "Types and methods for demonstrating enumerated types and objects

    "The definition of an enumerated type in ABAP declares its enumerated constants (these are special enumerated objects).
    "a) In the case below, no explicit base type is specified. Then, the standard base type of the constants is i. The
    "   enumerated values are counted up starting with 0 (a -> 0, b -> 1 ...).

    TYPES: BEGIN OF ENUM t_enum,
             a,
             b,
             c,
             d,
           END OF ENUM t_enum.

    "b) For the following enumerated type, an explicit base type is specified and start values provided using the VALUE addition
    "   Note that one value must be initial.

    TYPES: basetype TYPE c LENGTH 2,
           BEGIN OF ENUM t_enum_base BASE TYPE basetype,
             "If VALUE is specified explicitly, VALUE IS INITIAL must be used exactly once.
             e VALUE IS INITIAL,
             f VALUE 'u',
             g VALUE 'v',
             h VALUE 'wx',
             i VALUE 'yz',
           END OF ENUM t_enum_base.

    "c) Optionally an enumerated structure can be declared in the context of the type declaration.
    "Use case: If you have more than one enumerated type within one context. In doing so, you declare a constant enumeration structure.
    "The components of the structure are the enumeration constants of the enumerated type.
    TYPES: BEGIN OF ENUM t_enum_struc STRUCTURE en_struc BASE TYPE basetype,
             j VALUE IS INITIAL,
             k VALUE 'hi',
             l VALUE 'ab',
             m VALUE 'ap',
           END OF ENUM t_enum_struc STRUCTURE en_struc.


    METHODS enum_meth_params IMPORTING char          TYPE t_enum
                             RETURNING VALUE(output) TYPE string.
    METHODS enum_processing RETURNING VALUE(output) TYPE string_table.
    METHODS rtti_enum RETURNING VALUE(output) TYPE string_table.

ENDCLASS.



CLASS zcl_demo_abap_dtype_dobj IMPLEMENTATION.


  METHOD adapt_text.
    DATA text TYPE t_pub_text_c30.

    text = cl_text && comma && sy-uname && me->text.

    str = text && | (Note: The value of me->text is "{ me->text }")|.
  ENDMETHOD.


  METHOD addition_with_generic_num.
    result = num1 + num2.
  ENDMETHOD.


  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.


  METHOD enum_meth_params.

    CASE char.
      WHEN a.
        output = a.
      WHEN b.
        output = b.
      WHEN OTHERS.
        output = `Either c or d: ` && char.
    ENDCASE.

  ENDMETHOD.


  METHOD enum_processing.

    "Read and write positions of enumerated objects
    "Enumerated objects can be used in all read positions in which the operand
    "type is their enumerated type.
    "Likewise, enumerated variables can only be used in write positions in which
    "the operand type is the enumerated type and only the associated enumerated
    "values can be written.
    "So, assignments are possible only from one enumerated type to the same (with one
    "exception -> assignment to character-like variables of the types c and string)
    DATA do_enum TYPE t_enum.
    do_enum = a.
    APPEND |do_enum: { do_enum }| TO output.

    DATA do_enum_2 LIKE do_enum.
    do_enum_2 = do_enum.
    APPEND |do_enum_2: { do_enum_2 }| TO output.

    "Assignment to character-like variables of the types c and string.
    "In this case, the target field is assigned the name of the enumerated constant or
    "the component of the enumerated structure under which the enumerated value of the
    "source field is defined in the enumerated type.
    DATA do_a_string TYPE string.
    do_a_string = do_enum.
    APPEND |do_a_string: { do_a_string }| TO output.

    "Or using the CONV operator as follows
    DATA(do_next_string) = CONV string( do_enum ).
    APPEND |do_next_string: { do_next_string }| TO output.

    "Enumerated constants are converted implicitly to the type string
    "before the concatenation in the string template.
    DATA(str_from_enum) = |{ a }{ b }{ c }{ d }|.
    APPEND |str_from_enum: { str_from_enum }| TO output.

    "Note that only the enumerated type itself is relevant. Usually, the content
    "of an enumerated object is not of interest.
    "The enumerated value in the base type can be accessed using the constructor
    "operators CONV and EXACT only. The base type is i in this case.
    DATA(conv_value) = CONV i( do_enum ).
    APPEND |conv_value: { conv_value }| TO output.

    "Converting the other way round.
    DATA(another_conv) = CONV t_enum( 3 ).
    APPEND |another_conv: { another_conv }| TO output.

    "If known statically, an attempt to assign a value other than a valid enumerated value
    "to an enumerated variable produces a syntax error.
    "If not known statically, an exception is raised.
    "The following produces a syntax error
    "do_enum = f.

    "The following example shows raising an exception.
    DATA dobj TYPE t_enum.

    TYPES t_int_tab TYPE TABLE OF i WITH EMPTY KEY.
    DATA(int_tab) = VALUE t_int_tab( ( 0 ) ( 1 ) ( 2 ) ( 3 ) ( 4 ) ).

    DATA str_tab TYPE TABLE OF string.
    LOOP AT int_tab INTO DATA(wa_en).
      TRY.
          dobj = CONV t_enum( wa_en ).
          APPEND dobj TO str_tab.
        CATCH cx_sy_conversion_no_enum_value INTO DATA(error_enum).
          APPEND error_enum->get_text( ) TO str_tab.
      ENDTRY.
    ENDLOOP.

    APPEND `------------- START: Output for str_tab -------------` TO output.
    APPEND LINES OF str_tab TO output.
    APPEND `^^^^^^^^^^^^^ END: Output for str_tab ^^^^^^^^^^^^^` TO output.

    "An enumerated variable can be set to the initial value of its base type
    "using CLEAR.
    CLEAR do_enum.
    APPEND |do_enum: { do_enum }| TO output.

    "Enumerated structures
    DATA do_enum_s TYPE t_enum_struc.

    "The enumerated structure en_struc was decalred in the public section.
    "Using the addition LIKE, a second structure is created referring to the enumerated structure.
    "Note that the second structure is not a constant structure.
    "The components of the constant structure contain the enumerated values of the enumerated type.
    "All the components of the variable structure declared by LIKE contain the initial values.
    DATA do_s LIKE en_struc.
    APPEND |do_s: { do_s-j } / { do_s-k } / { do_s-l } / { do_s-m }| TO output.

    DATA(do_en) = en_struc.
    APPEND |do_en: { do_en-j } / { do_en-k } / { do_en-l } / { do_en-m }| TO output.

    "Accessing structure components using the component selector
    DATA(do_en_k) = en_struc-k.
    APPEND |do_en_k: { do_en_k }| TO output.

    DATA(do_s_m) = do_s-m.
    APPEND |do_s_m: { do_s_m }| TO output.
    "Assigning enumerated constants to the variable structure
    do_s = en_struc.
    APPEND |do_s: { do_s-j } / { do_s-k } / { do_s-l } / { do_s-m }| TO output.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: Data Types and Data Objects\n\n| ).

**********************************************************************

    out->write( |Declaring data types\n\n| ).

    "The following examples deal with the declaration of data types.
    "They show how data types can be declared locally in an ABAP program.
    "Note:
    "- Data types can also be declared in the ABAP Dictionary (DDIC) or as a
    "  CDS entity, which is not covered in this demo example.
    "- In ADT and because of the many type declarations, you may want to press
    "  F2 on the types to get more information.
    "- The examples show a selection.
    "- Only non-generic types can be used.

    out->write( |1) Declaring data types based on elementary types\n\n| ).

    "See the ABAP Keyword Documentation for the value ranges that are
    "accepted by these types.

    "Data type declarations based on built-in ABAP types

    "Numeric types
    TYPES te_i TYPE i.
    TYPES te_int8 TYPE int8.
    TYPES te_decfl16 TYPE decfloat16.
    TYPES te_decfl34 TYPE decfloat34.
    TYPES te_f TYPE f.
    TYPES te_p_l4_d2 TYPE p LENGTH 4 DECIMALS 2.
    "Note: LENGTH/DECIMALS must be specified when using the types c, p, n, x
    "in ABAP Objects contexts as it is the case here in the example class.

    "Character-like types
    "To combine TYPES statements, you can use chained statements,
    "i.e. TYPES followed by a colon and then listing the type declarations separated
    "by a comma.
    TYPES: te_c5  TYPE c LENGTH 5,
           te_n4  TYPE n LENGTH 4,
           te_str TYPE string.

    "Byte-like types
    TYPES te_do_string TYPE x LENGTH 2.
    TYPES te_xstr TYPE xstring.

    "Types for date and time
    TYPES te_d TYPE d.
    TYPES te_t TYPE t.
    TYPES te_utc TYPE utclong.

    "You might also stumble on a length specification in parentheses following the
    "data type name. It is recommended that you use addition LENGTH instead of the
    "parentheses.
    TYPES te_cfour(4) TYPE c.

    "**** Data type declarations based on existing types or data objects ****

    "Type declaration based on an existing type visible at this location;
    "all properties of the specified data type are inherited.
    TYPES te_another_i TYPE te_i.

    "Anticipating the data object declaration needed to demonstrate the LIKE addition
    DATA do_num TYPE i.

    "LIKE addition:
    "Type declaration based on an existing data object visible at this location;
    "all properties of the type of the specified data object are inherited.
    TYPES te_from_int LIKE do_num.

    "**** Data type declarations based on globally available types or data objects ****

    "DDIC Types
    "Note that the built-in types b and s cannot be specified for type
    "declarations. However, the value range for these types can be obtained by
    "referencing the built-in DDIC types INT1 and INT2. These are data elements.
    "In ADT, you can check out the data elements by forward navigation (hold CTRL
    "and click on the type). You can also use F2 Information (click F2 when on
    "the type) to get information.
    TYPES te_int1 TYPE int1.
    TYPES te_int2 TYPE int2.

    "Referring to types in global classes
    "Also here, check out the forward navigation or F2 information for the types.
    "In the example, the type exists in a global interface.
    TYPES te_elem_from_itf TYPE zdemo_abap_get_data_itf=>occ_rate.

    "Referring to a data object that exists in a global interface
    TYPES te_dobj_from_itf LIKE zdemo_abap_objects_interface=>stat_str.

    "Referring to a data object that exists in the public visibility section of
    "a global class
    TYPES te_dobj_from_cl LIKE zcl_demo_abap_objects=>public_string.

    "Referring to a component of a DDIC table (also possible for views;
    "the components have elementary types)
    TYPES te_comp_ddic_tab TYPE zdemo_abap_carr-carrid.

    "Type pools (ABAP program, administrated by the ABAP Dictionary; may only be
    "created in standard ABAP; but is considered obsolete).
    "However, the following example is accessible in ABAP for Cloud Development.
    "The type pool contains the definitions of globally visible data types and
    "constants. Check it out using the forward navigation and the F2 information.
    TYPES te_tp TYPE abap_bool.
    TYPES te_const_in_tp LIKE abap_true.

    out->write( no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Declaring data types based on complex types` ) ).

    "Structure and internal table types as examples for complex types

    "Structure type, can contain any type
    TYPES: BEGIN OF ts_misc_comps,
             comp1 TYPE i,
             comp2 TYPE string,
             comp3 TYPE te_i,                                    "Existing type
             comp4 LIKE do_num,                                  "Referring to existing data object
             comp5 TYPE string_table,                            "Internal table type (available in DDIC)
             comp6 TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY, "Internal table type (based on database table)
             comp7 TYPE REF TO i,                                "Reference type
           END OF ts_misc_comps.

    "Internal table types
    "Note: The examples only use the implicit STANDARD for standard tables.
    "Internal table type declaration based on a local structure type
    TYPES tt_local_ts TYPE TABLE OF ts_misc_comps WITH EMPTY KEY.

    "Internal table type declaration based on an elementary data type
    TYPES tt_int TYPE TABLE OF i.

    "Referring to existing types and data objects

    "Anticipating the creation of structured data objects for the LIKE addition
    DATA struc_local_ts TYPE ts_misc_comps.

    "Structure type creation based on an existing structured data object
    TYPES ts_w_like LIKE struc_local_ts.

    "Anticipating the creation of an internal table for the LIKE addition
    DATA itab_local_ts TYPE TABLE OF ts_misc_comps WITH EMPTY KEY.

    "Internal table type declaration based on an existing internal table
    TYPES tt_w_like LIKE itab_local_ts.

    "Internal table type declaration based on the existing internal table type
    TYPES tt_another_type TYPE tt_w_like.

    "Structured types based on an internal table's line type
    TYPES ts_type_line TYPE LINE OF tt_w_like.
    TYPES ts_like_line LIKE LINE OF itab_local_ts.

    "Internal table typed with internal table as line type
    TYPES tt_like_table LIKE TABLE OF itab_local_ts.

    "Referring to global types

    "Structure type based on DDIC type
    "In this case, a database table is specified whose line type is used as data type
    "in this type declaration. You may also use a CDS view (or classic DDIC view in
    "standard ABAP) or a dedicated structured type defined in the DDIC.
    TYPES ts_ddic_tab TYPE zdemo_abap_carr.

    "Internal table type based on internal type that exists in a gloabl interface
    TYPES tt_tab_type_from_itf TYPE zdemo_abap_get_data_itf=>carr_tab.

    "Internal table types with an elementary line type based on globally available types
    "Elementary table type
    TYPES tt_strtab TYPE string_table.
    "Elementary line type; the type is available in a global interface
    TYPES tt_elem_type_from_itf TYPE TABLE OF zdemo_abap_get_data_itf=>occ_rate.

    out->write( no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Declaring reference types` ) ).

    "Declaring reference types with static types
    TYPES tr_i TYPE REF TO i.
    TYPES tr_str TYPE REF TO string.
    TYPES tr_ddic_tab TYPE REF TO zdemo_abap_carr.
    "Using the generic type data as static type
    TYPES tr_data TYPE REF TO data.

    "Referring to an existing reference type
    TYPES tr_ref_i TYPE tr_i.

    "Anticipating the creation of a data reference variable for showing
    "the LIKE addition
    DATA dref_i TYPE REF TO i.

    "Creating a reference type based on a data reference variable
    TYPES tr_like_ref_i LIKE dref_i.

    "Creating a data object for the LIKE REF TO addition
    DATA str TYPE string.

    "Creating a reference type whose static type is inherited from the data
    "type of the specified data object
    TYPES tr_like_ref2str LIKE REF TO str.

    "Reference table types
    TYPES tr_tab_ref_i TYPE TABLE OF REF TO i.
    DATA itab_str TYPE TABLE OF string.
    TYPES tr_like_table_ref LIKE TABLE OF ref TO itab_str.

    out->write( no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Declaring data objects` ) ).

    "The following examples deal with the declaration of data ojects.
    "They show how data objects can be declared locally in an ABAP program.
    "Note:
    "- With the exception of two additions (VALUE and READ-ONLY), the syntax
    "  follows the pattern as for TYPES statements.
    "- A data type defined using DATA, which is not derived from an existing type,
    "  is available only as a property of the
    "  declared data object and not on its own. This kind of data type is bound to
    "  its data object.
    "- The examples show the creation of named data objects. Anonymous data objects
    "  are covered below.
    "- In ADT and because of the many type declarations, you may want to press F2 on
    "  the types to get more information.
    "- The examples show a selection. For more information, check out the ABAP
    "  Keyword Documentation.

    out->write( |4) Declaring data objects based on elementary data types\n\n| ).

    "The elementary, built-in data types can be used as shown for data type
    " declarations. Chained statements are also possible with DATA.
    "Note that not all types as shown above are used here.
    DATA: do_i       TYPE i,
          do_c_l5    TYPE c LENGTH 5,
          do_p_l3_d2 TYPE p LENGTH 3 DECIMALS 2,
          do_decfl16 TYPE decfloat16,
          do_str     TYPE string,
          "Specifying the length in parantheses instead of using the
          "LENGTH addition is not recommended
          do_ctwo(2) TYPE c.

    "Referring to locally declared data types
    TYPES te_string TYPE string.
    DATA do_another_str TYPE te_string.

    "Referring to other data objects
    DATA do_like_dobj LIKE do_i.

    "If the length is not specified explicitly for the ABAP types c, n, p, and x,
    "the standard length is defined implicitly. Check the F2 information.
    DATA do_c_std TYPE c.
    DATA do_p_std TYPE p.

    "If neither TYPE nor LIKE is specified, a data object with the bound
    "data type 'c LENGTH 1' is created.
    DATA do_c.

    "VALUE addition
    "Start values can be set for the data objects when they are declared.
    "Without the addition VALUE, data objects are filled with their type-specific
    "initial values. The start value can either be specified as a literal or as a
    "predefined constant.
    "Note: The VALUE addition is not to be confused with the VALUE operator that
    "can be used to construct the content of complex data objects as shown below.
    DATA do_c_l2 TYPE c LENGTH 2 VALUE 'hi'.
    DATA do_i_val TYPE i VALUE 123.
    DATA do_like_val LIKE do_i VALUE 9.

    "Specifying a constant (data object that cannot be changed at runtime) after
    "the VALUE addition
    CONSTANTS con TYPE string VALUE `abcdef`.
    DATA do_val_con TYPE string VALUE con.

    "VALUE IS INITIAL addition: Explicitly specifying the type-specific initial value
    DATA do_i_init TYPE i VALUE IS INITIAL.
    DATA do_i_like_init LIKE do_i VALUE IS INITIAL.

    "Data objects can also be created in the declaration part of classes and
    "interfaces. There you can use the READ-ONLY addition for data object
    "declarations in the public visibility section. In doing so, an attribute
    "declared using CLASS-DATA or DATA can be read from outside of the class but
    "can only be changed using methods of the class or its subclasses.
    "The following attribute is taken from this executable example. It shows a
    "read access in a control structure. If you wanted to assign a new value to
    "the attribute outside of the class, a syntax error would be displayed.
    "Note that when you are in the class itself, there is no need to specify the
    "class name.
    "read_only_attribute = ... would be sufficient. And changing the value would
    "be possible within the class, too.
    "Declaration in the example:
    "CLASS-DATA: read_only_attribute TYPE string VALUE `Hallo` READ-ONLY.
    IF zcl_demo_abap_dtype_dobj=>read_only_attribute = `adapt read only attribute`.
      ...
      "Since we are here in the very class of this example attribute, a changing
      "of the value would be possible. And the class name can, in that case, be
      "ommitted.
      read_only_attribute = `changed`.
    ELSE.
      ...
    ENDIF.

    out->write( no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Declaring structures and internal tables as examples for complex types` ) ).

    "Note: See more details and examples in the ABAP Keyword Documentations and in the
    "respective ABAP cheat sheets.

    "Creating a structure with DATA and providing start values with the VALUE addition.
    "If not specified, then the components have their type-specific initial value.
    DATA: BEGIN OF a_structure,
            comp1 TYPE i VALUE 1,
            comp2 TYPE string VALUE `hi`,
            comp3 TYPE string,
          END OF a_structure.

    "Creating a structure based on a global type. In this case, it is a DDIC database
    "table whose line type is used. You can also use a CDS view or a dedicated structured type
    "from the DDIC, for example.
    DATA struc_ddic_tab TYPE zdemo_abap_carr.

    "Creating a structure as a constant. Providing values is mandatory.
    CONSTANTS: BEGIN OF con_struc,
                 comp1 TYPE i VALUE 1,
                 comp2 TYPE string VALUE `hallo`,
                 comp3 TYPE string VALUE `salut`,
               END OF con_struc.

    "Using the constant as start value for a structure declaration.
    DATA struc_w_val LIKE con_struc VALUE con_struc.

    "Declaring a structure and explicitly specifying the type-specific
    "initial values of the structure components as start values.
    DATA struc_init_val LIKE con_struc VALUE IS INITIAL.

    "Creating internal tables ...
    "Based on a globally available DDIC database table whose line type is used
    DATA itab_ddic_tab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
    "Based on an elementary type
    DATA itab_tab_i TYPE TABLE OF i.
    "The table type is declared in a global interface
    DATA itab_tab_type_from_itf TYPE zdemo_abap_get_data_itf=>carr_tab.
    "Based on globally available DDIC internal table type; explicitly specifying as initial
    DATA itab_ddic_tab_type TYPE string_table VALUE IS INITIAL.
    "Based on locally available structured data object
    DATA itab_like_struc LIKE TABLE OF struc_w_val WITH EMPTY KEY.
    "Based on locally available internal table
    DATA itab_like_another_itab LIKE itab_tab_i.

    "Creating an internal table type locally
    TYPES tt_ddic_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.
    "... and an internal table based on it.
    DATA itab_w_itab_type TYPE tt_ddic_tab.

    "Creating a structure based on the line of an internal table (type)
    DATA struc_from_itab_type TYPE LINE OF tt_ddic_tab.
    DATA struc_like_line LIKE LINE OF itab_ddic_tab.

    out->write( no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) Declaring data reference variables` ) ).

    "Declaring data reference variables types with static types
    DATA dref_int TYPE REF TO i.
    DATA dref_str TYPE REF TO string.
    DATA dref_ddic_tab TYPE REF TO zdemo_abap_carr.
    "Using the generic type data as static type
    DATA dref_8_dataa TYPE REF TO data.

    "Referring to an existing reference type
    TYPES tr_int TYPE REF TO i.
    DATA dref_tr_int TYPE tr_int.

    "Creating a data reference variable based on a data reference variable
    DATA dref_like LIKE dref_int.

    "Creating a data object for the LIKE REF TO addition
    DATA do_some_string TYPE string.

    "Reference type is created whose static type is inherited from the data type of
    "the specified data object
    DATA dref_like_ref_str LIKE REF TO do_some_string.

    "Reference tables
    DATA dref_tab_i TYPE TABLE OF REF TO i.
    DATA dref_tab_str LIKE TABLE OF REF TO do_some_string.

    out->write( no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) Assigning values to data objects` ) ).

    "An assignment passes the content of a source to a target data object.
    "Note:
    "- There are conversion rules when assigning a source to a target data object
    "  that have different types. See further down.
    "- In old ABAP code, you may see MOVE ... TO ... statements for value
    "  assignments. These statements are obsolete. This is not to be confused
    "  with MOVE-CORRESPONDING statements for complex types. They are not obsolete.

    "As mentioned, a start value can be directly assigned when declaring a data object.
    DATA some_int TYPE i VALUE 123.

    "Assignment using the assignement operator =
    "The source of the assigment on the right side (also known as general
    "expressions posisiton) can be specified with many things.

    "Single data object as source of the assignment
    "In the following cases, the literal and data object have the exact type
    "as the data object on the left side.

    some_int = 456.
    DATA num TYPE i.
    num = some_int.

    DATA str_a1 TYPE string VALUE `hallo`.
    DATA str_a2 TYPE string.
    str_a2 = str_a1.

    "Functional method as source of the assignment
    "In the following example, the method get_next of the class cl_abap_random_int
    "returns an integer. Check the F2 information for get_next (return value of type i).
    "A random integer that is in the specified value range is assigned to the data object
    "on the left side.
    num = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( )
      min  = 1
      max  = 10 )->get_next( ).

    "Built-in functions as source of the assignment
    "There are plenty of functions available.

    "Built-in numeric function
    "The following built-in function calculates 2 to the power of 4. The
    "result is assigned to the data object on the left side.
    num = ipow( base = 2 exp = 4 ).

    "Built-in string function
    "The following function transforms the specified data object to upper case letters.
    "The result is assigned to the data object on the left side.
    str_a1 = to_upper( str_a2 ).

    "Constructor expressions as source of the assignment
    "There are various options and expressions available (with many additions).
    "Check the ABAP Keyword Documentation and the cheat sheet. Here, taking the VALUE
    "operator as an example. This operator is very handy especially for complex types.

    "Creating a structure
    DATA some_struc TYPE zdemo_abap_carr.

    "Assignment using the VALUE operator
    "Note the # character that stands for the type. Here, the structure type can be
    "derived from the context. Hence, the explicit name can but need not be specified.
    some_struc = VALUE #( carrid = 'XY' carrname = 'XY Airways' ).

    "Creating an internal table and assigning values
    "Note that components that are not specified and assigned a value retain their
    "type-specific ininial value.
    DATA some_itab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
    some_itab = VALUE #( ( carrid = 'XY' carrname = 'XY Airways' )
                         ( carrid = 'ZZ' carrname = 'ZZ Airlines' ) ).

    "Table expressions as source of the assignment
    "A structure is assigned an internal table line
    some_struc = some_itab[ 2 ].

    "Calculation expressions as source of the assignment
    "Arithmetic expressions
    num = 1 + 2.
    "A calculation assignment as follows
    num += 1.
    "is the short form of
    num = num + 1.
    "Syntax options: +=, -=, *= and /=

    "String expressions as source of the assignment
    str_a2 = str_a1 && ` blabla`. "Strings are appended using the && operator
    str_a2 = |{ str_a1 } some more bla.|. "String templates
    "Note: Data objects are specified in curly brackets. The content is converted to type string.
    "      It must be convertible to type string.

    "An elementary data object is assigned a component of a specific table line using
    "a table expression. Note: In the following case, the types of source and target are not
    "the same (type c versus type string). As shown further down, such an assignment can lead
    "to unexpected or undesired results. Type-dependent conversions are made in accordance
    "with the conversion rules.
    str_a2 = some_itab[ 2 ]-carrname.

    out->write( no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Creating data objects by inline declaration` ) ).

    "The declaration operator DATA can be specified in any designated declaration position.
    "The data type of the variable is determined by the operand type. It must be possible
    "to derive this type completely statically.
    "Note:
    "- The FINAL declaration operator is available for creating
    "  immutable variables as shon below.
    "- As shown in the previous section, there are many options for what can be placed on
    "  the right side.

    "Data object declarations and assignements as shown above.
    DATA str_b1 TYPE string VALUE `abc`.

    DATA str_b2 TYPE string.
    str_b2 = `def`.

    "Using a declaration expression with the declaration operator DATA, a data object
    "can be declared inline.
    "The data type of the declared variable is determined by the right side.
    "In doing so, a data object is assigned a value in one go.
    "In the following case, it is a string literal with backquotes on the right side.
    "The data type is derived and, hence, the data object is of type string.
    DATA(str_b3) = `ghi`.

    "In the following case, it is a text field literal with quotes. Type c is derived.
    "The length is derived from the number of characters.
    DATA(c_l3) = 'jkl'. "type c length 3
    DATA(c_l4) = 'mnop'. "type c length 4
    "Note the type conversion implications when making an assignment with these two
    "data objects.
    c_l3 = c_l4. "c_l3: 'mno'

    "Structures and internal tables
    "In declaration expressions, structures and internal tables can be declared inline
    "and filled with, for example, the help of the VALUE operator.

    "Structured data type
    TYPES: BEGIN OF ts_struc,
             comp1 TYPE i,
             comp2 TYPE string,
           END OF ts_struc.

    "In the following examples, structures are created. The structured data type is
    "derived from the type specified before the parentheses.
    DATA(struc_b1) = VALUE ts_struc( comp1 = 1 comp2 = `A` ).
    "No components specified and values assigned means an initial structure.
    "This syntax is also possible for declaring data objects with elementary types
    "and explicitly specifiying initial values, but only for initial values.
    "See the CONV operator below.
    DATA(struc_b2) = VALUE ts_struc( ).
    DATA(elem_init) = VALUE i( ).
    "Note that components that are not specified and assigned a value remain initial.
    DATA(struc_b3) = VALUE zdemo_abap_carr( carrid = 'AB' carrname = 'AB Airlines' ).
    "An entire structure is assigned.
    DATA(struc_b4) = struc_b1.

    "Note: When the structure has already been declared, and you want to assign values,
    "you can use the VALUE operator followed by the # character instead of the explicit
    "type name. In that case, it is possible to derive the type from the context.
    struc_b4 = VALUE #( comp1 = 2 comp2 = `b` ).

    "Internal tables
    "The internal table type is specified before the parentheses after the VALUE operator.
    "The following example uses a table type that is globally available in the DDIC.
    DATA(itab_b1) = VALUE string_table( ( `a` )
                                        ( `b` )
                                        ( `c` ) ).

    "Using a local internal table type
    TYPES tt_b1 TYPE TABLE OF ts_struc WITH EMPTY KEY.
    DATA(itab_b2) = VALUE tt_b1( ( comp1 = 1 comp2 = `a` )
                                 ( comp1 = 2 comp2 = `b` )
                                 ( comp1 = 3 comp2 = `c` ) ).

    "In the context of other ABAP statements such as LOOP, READ TABLE or ABAP SQL
    "SELECT statements, inline declarations are useful for creating target variables with
    "appropriate data types in place. This includes data reference variables and field
    "symbols. Field symbols are not covered below.

    "A work area/structure to hold the current internal table line is created inline.
    LOOP AT itab_b2 INTO DATA(wa_b1).
      wa_b1-comp1 = 12345.
      ...
    ENDLOOP.

    "Using the REFERENCE addition, a data reference variable can be created inline.
    LOOP AT itab_b2 REFERENCE INTO DATA(wa_ref_b1).
      wa_ref_b1->comp1 = 67890.
      ...
    ENDLOOP.

    "A structure to hold the internal table line read is created inline.
    READ TABLE itab_b2 INTO DATA(wa_b2) INDEX 2.
    "Data reference variable
    READ TABLE itab_b2 REFERENCE INTO DATA(wa_ref_b2) INDEX 2.

    "ABAP SQL statements
    "A structure as target data object is created inline.
    SELECT SINGLE * FROM zdemo_abap_carr INTO @DATA(struc_b5).
    "NEW addition of the INTO clause creates a data reference variable
    SELECT SINGLE * FROM zdemo_abap_carr INTO NEW @DATA(struc_ref).

    "Internal table as target data object is created inline.
    SELECT * FROM zdemo_abap_carr INTO TABLE @DATA(itab_b3).
    "NEW addition
    SELECT * FROM zdemo_abap_carr INTO TABLE NEW @DATA(itab_ref).

    out->write( no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `9) Assigning references to data reference variables` ) ).

    "Note:
    "- As is true for other data object and types, there are special assignment rules
    "  for data reference variables. Check out the ABAP Keyword Documentation.
    "- An initial reference variable contains the null reference, which does not point
    "  to any objects. This means that it does not have a data type or a class as
    "  a dynamic type.

    "Declaring data reference variables with static types
    "At this stage, initial reference variables contain null references.
    DATA dref_1_i TYPE REF TO i.
    DATA dref_2_str TYPE REF TO string.
    "Generic type as static type
    DATA dref_3_data TYPE REF TO data.

    "References in data reference variables can point to existing data objects.
    "For assigning the reference, you can use the REF operator.
    "There is also an ABAP statement available doing the same: GET REFERENCE.
    "It should not be used anymore, especially in ABAP for Cloud development.

    "Creating data objects to refer to and providing a start value
    DATA do_number TYPE i VALUE 987.
    DATA do_string TYPE string VALUE `abc`.

    "After the assignment, the data reference variable points to the values.
    "The data type is derived (dynamic type).
    dref_1_i = REF #( do_number ). "Dynamic type is the same as the static type in this case
    dref_2_str = REF #( do_string ). "Dynamic type is the same as the static type in this case
    "Dynamic types of the followig examples are more specific than static type,
    "which is a generic type in this case.
    dref_3_data = REF #( do_number ).
    dref_3_data = REF #( do_string ).

    "Note: Table expressions can be also specified within the parentheses.

    "Inline declarations are also possible to create data reference variables
    "and assigning values in one go. Check the F2 information.
    DATA(dref_4_data) = dref_3_data.
    DATA(dref_5_str) = REF #( `hi` ).
    DATA(dref_6_i) = REF #( do_number ).

    "Assignments between two data reference variables mean that references are copied.
    "The concepts of upcast and downcast enter the picture here.
    "Two different assignment operators are used, as well as the casting operator CAST.

    "Upcast is possible for elementary data types
    "- The static type of the target variable is more general or identical to the static
    "  type of the source variable.
    "- Assignment operator used: =
    "- Note that the operators for downcasts can also be used explicitly here, but it is
    "  usually not needed.
    "- In this example, elementary data types are covered. An upcast works ...
    "  - if the data types have identical type properties (i.e. the built-in type match
    "    as well as length and decimal places).
    "  - the static type of the source variable is completely typed, and the static type
    "    of the target variable is generic.

    "The following upcasts work. Both point to data objects of type i or string.
    dref_1_i = dref_6_i.
    "The source on the right side is completely typed (type i),
    "the target on the left side is a generic type (type data).
    dref_3_data = dref_1_i.

    "Downcasts
    "- The static type of the target variable is more specific than the static type of the
    "  source variable.
    "- The assignability is not checked until runtime.
    "- Must always be performed explicitly using the casting operator ?= or the more
    "  modern casting operator CAST.

    "The following example would result in a syntax error due to type incompatibility.
    "dref_1_i = dref_3_data.

    "In the following example, the source has a generic static type (data). The target type
    "has a more specific type (type i).
    "To suppress the syntax error, the CAST operator is needed.
    "Note:
    "- The assignability is still not checked. This is done at runtime.
    "  In this example, it works since the dynamic type of the source is also of type i.
    "- An advantage of the CAST operator compared to ?= is that the operator enables downcasts
    "  in operand positions, which helps reduce helper variables.
    dref_1_i = CAST #( dref_3_data ).

    "If not caught, the following would result in a runtime error.
    "dref_3_data points to a data object of type i, the static type of dref_2_str is string.
    "So, the downcast does not work.
    TRY.
        dref_2_str = CAST #( dref_3_data ).
      CATCH cx_sy_move_cast_error INTO DATA(e).
        out->write( data = e->get_text( ) name = `e->get_text( )` ).
    ENDTRY.

    "Old syntax using the ?= operator
    dref_1_i ?= dref_3_data.

    "For upcasts, the operators can be used, too, but they are usually not necessary.
    "So, an assignment as follows is possible but not needed. Only using = is sufficient.
    dref_1_i = CAST #( dref_6_i ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) Creating anonymous data objects` ) ).

    "Anonymous data objects are a topic related to data reference variables.
    "These data objects are unnamed data objects.
    "Most of the data objects above are named data objects, i.e. they can be addressed
    "by a dedicated name.
    "Unnamed data objects are literals and anonymous data objects. Anonymous data objects
    "can be addressed using data reference variables.
    "Note: Unlike data objects created with the statement DATA, anonymous data objects
    "are created at runtime. Data objects declared using DATA are created when the
    "program is loaded.

    "Options to create anonymous data objects
    "- CREATE DATA statements
    "- Using the instance operator NEW
    "- Addition NEW of the INTO clause in ABAP SQL SELECT statements

    "CREATE DATA statements
    "Note that there are many additions available. The examples show a selection.

    "Creating an anonymous data object with an implicit type.
    "If neither of the additions TYPE or LIKE are specified, the data reference variable
    "must be completely typed.
    DATA dref_7_str TYPE REF TO string.
    CREATE DATA dref_7_str.

    "Note: If you want to assign a value to the data object, this can't be done directly.
    "The data reference variable must be dereferenced first using the dereferencing operator.
    dref_7_str->* = `hi`.

    "Creating an anonymous data object with an explicitly specified data type
    DATA dref_8_data TYPE REF TO data.
    CREATE DATA dref_8_data TYPE p LENGTH 8 DECIMALS 3.
    dref_8_data->* = 1 / 3.

    "Creating a named data object
    DATA it TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.

    "Creating an anomyous internal table.
    "Using the LIKE addition to refer to an existing internal table
    CREATE DATA dref_8_data LIKE it.

    "Using the anonymous data object as target in the INTO clause of a SELECT statement.
    "As above, note the dereferencing.
    SELECT *
      FROM zdemo_abap_carr
      INTO TABLE @dref_8_data->*.

    "Creating an anonymous hashed table by specifying the entire table type
    CREATE DATA dref_8_data TYPE HASHED TABLE OF zdemo_abap_carr WITH UNIQUE KEY carrid.

    "Using the anonymous data object as target in the INTO clause of a SELECT statement
    SELECT *
       FROM zdemo_abap_carr
       INTO TABLE @dref_8_data->*.

    "Creating an anonymous structure
    CREATE DATA dref_8_data TYPE zdemo_abap_fli.

    "Using the anonymous data object as target in the INTO clause of a SELECT statement
    SELECT SINGLE *
       FROM zdemo_abap_fli
       INTO @dref_8_data->*.

    "NEW operator
    "- Works like CREATE DATA dref TYPE type statements and can be used in general
    "  expression positions.
    "- Allows to assign values to the new anonymous data objects in parentheses

    "Creating data reference variables
    DATA: dref_9_data TYPE REF TO data,
          dref_10_i   TYPE REF TO i.

    "Assining a value to an anonymous data object of the type i
    dref_9_data = NEW i( 555 ).

    "The # character can be used instead of the complete type spefication
    "when the type can be derived from the context.
    dref_10_i = NEW #( 653 ).

    "Inline declarations are handy.
    "Creating a suitable anonymous data object in place. Here, the type must be specificed
    "explicitly.
    DATA(dref_11_i) = NEW i( 32 ).

    "Creating an anonymous structure
    DATA(dref_12_ddic_tab) = NEW zdemo_abap_carr( carrid = 'AB' carrname = 'AB Airlines'  ).

    "The # character can be omitted when they type can be derived.
    DATA dref_13_ddic_tab TYPE REF TO zdemo_abap_carr.
    dref_13_ddic_tab = NEW #( carrid = 'AB' carrname = 'AB Airlines'  ).

    "ABAP SQL SELECT statement
    "As shown above, using the NEW addition in the INTO clause, an anonymous data
    "object can be created in place.
    SELECT *
      FROM zdemo_abap_carr
      INTO TABLE NEW @DATA(dref_14_inline).

    out->write( no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Excursions: Elementary types and type conversions` ) ).

    out->write( |11) Implicit and explicit conversion\n\n| ).

    "Implicit conversions are performed in assignments using the assignment operator =
    "The content of a data object is converted according to the associated conversion rules.
    "The following examples demonstrate implicit type conversions and their consequences
    "that might be undesired.

    "Conversions with the types c and string
    DATA do_1_str TYPE string VALUE `abcedf`.
    DATA do_2_c3 TYPE c LENGTH 3.
    do_2_c3 = do_1_str.

    out->write( data = do_2_c3 name = `do_2_c3` ).
    out->write( |\n| ).

    "Conversions with the types i and decfloat34
    DATA do_4_i TYPE i.
    DATA do_5_dcfl34 TYPE decfloat34 VALUE '4.56'.
    do_4_i = do_5_dcfl34.

    out->write( data = do_4_i name = `do_4_i` ).
    out->write( |\n| ).

    "Conversions with the types i and string
    do_4_i = -5.
    do_1_str = do_4_i.

    out->write( data = do_1_str name = `do_1_str` ).
    out->write( |\n| ).

    "Explicit type conversions can be performed with the CONV operator
    "It converts the value specified within the parentheses to the data type specified
    "before the parentheses and and creates an appropriate result.
    "Note:
    "- CONV closes the gap where the value operator VALUE cannot be used to construct
    "  values for elementary data objects except for the initial value.
    "- The conversion is performed in accordance with the associated conversion rule.

    "Explicitly converting decfloat34 to i
    DATA do_6_dcfl34 TYPE decfloat34 VALUE '2.78'.
    DATA(do_7_i) = CONV i( do_6_dcfl34 ).

    out->write( data = do_7_i name = `do_7_i` ).
    out->write( |\n| ).

    "# character when the type can be derived
    DATA do_8_i TYPE i.
    do_8_i = CONV #( do_6_dcfl34 ).

    out->write( data = do_8_i name = `do_8_i` ).
    out->write( |\n| ).

    "The following two calculations yield different results
    do_8_i = sqrt( 5 ) + sqrt( 6 ).

    out->write( data = do_8_i name = `do_8_i` ).
    out->write( |\n| ).

    do_8_i = CONV i( sqrt( 5 ) ) + CONV i( sqrt( 6 ) ).

    out->write( data = do_8_i name = `do_8_i` ).

    "CONV operator for creating data objects inline with elementary data types
    "Assume, you want a data object typed with decfloat34.
    "The following example assigns a text field literal to the left side.
    DATA(do_9_c4) = '4.56'. "It is type c

    "Using the CONV operator and explicitly specifing the type, you can contruct a
    "data object with an appropriate elementary data type.
    DATA(do_10_dcfl34) = CONV decfloat34( '4.56' ).
    "It corresponds to the following
    DATA do_11_dcfl34 TYPE decfloat34 VALUE '4.56'.

    "Note that the EXACT operator is available for lossless assignments.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `12) Character strings and text field strings` ) ).

    "The following example shows the difference between text field strings
    "of type c and character strings of type string when it comes to trailing
    "blanks.

    DATA: text_space   TYPE c LENGTH 1 VALUE ' ',
          string_space TYPE string VALUE ` `,
          result3      TYPE string,
          result4      TYPE string.
    result3 = '-->' && text_space && '<--'.
    result4 = '-->' && string_space && '<--'.

    out->write( |{ result3 }\n{ result4 }| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `13) Floating point numbers` ) ).

    "The following example shows the difference between binary and decimal
    "floating point numbers.

    DATA: result1 TYPE f,
          result2 TYPE decfloat34.
    result1 = 815 / 1000.
    result2 = 815 / 1000.

    out->write( |Binary floating point: { result1 }\n| &&
    |Decimal floating point: { result2 }\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `14) Byte-like types` ) ).

    "The following example shows byte-like types x and xstring.

    "A byte string is filled with binary data created from a string
    "using the convert_out method of the cl_abap_codepage class.
    "The result is the UTF-8 representation of the string (type xstring)
    "which is displayed at first.
    "Using the create_in method, the data object of type xstring is converted
    "back to string.
    "Without knowing how it was created or what it is supposed to mean, you can
    "hardly do anything with a byte string.
    "The second part of the example deals with type x.
    "The binary representation of a new line feed (0A) is converted to string.
    "The result is compared to the actual means of previding a new line feed
    "(string template with \n and the older cl_abap_char_utilities=>newline).
    "The same is done using the binary representation of a blank (20).

    DATA(some_string) = `Hi there!`.

    DATA(xstr) =
      cl_abap_conv_codepage=>create_out(
      codepage = `UTF-8`
                 )->convert( source = some_string ).

    out->write( data = xstr name = `xstr` ).
    out->write( |\n| ).

    DATA(xstring2string) = cl_abap_conv_codepage=>create_in( codepage = `UTF-8`
                                                                        )->convert( source = xstr ).

    out->write( data = xstring2string name = `xstring2string` ).
    out->write( |\n| ).

    DATA line_feed_hex TYPE x LENGTH 1 VALUE '0A'.

    DATA(line_feed_str) = cl_abap_conv_codepage=>create_in( codepage = `UTF-8`
        )->convert( source = CONV xstring( line_feed_hex ) ).

    ASSERT line_feed_str = |\n|.
    ASSERT line_feed_str = cl_abap_char_utilities=>newline.

    out->write( `Y-->` && line_feed_str && `<--` ).
    out->write( |\n| ).
    out->write( `Y-->` && |\n| && `<--` ).
    out->write( |\n| ).

    DATA a_blank_x TYPE x LENGTH 1 VALUE '20'.

    DATA(blank) = cl_abap_conv_codepage=>create_in( codepage = `UTF-8`
        )->convert( source = CONV xstring( a_blank_x ) ).

    ASSERT blank =  ` `.

    out->write( `-->` && blank && `<--` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `15) Date and time` ) ).

    "In the example, a date field is assigned the current values
    "using the cl_abap_context_info class. A calculation follows. The date of next
    "day is calculated.
    "Note: The data types behave like numeric values in the context of calculations.
    "In assignments and in the output, they behave like character-like types.
    "The second example shows an undesired result for a conversion of type i to d.
    "The number is interpreted as number of days since 0001-01-01 of the Gregorian
    "calendar. In this case, the date would exceeed the maximum value '99991231'.
    "In such a case, the date field is assigned the invalid value '00000000'.

    DATA: today    TYPE d,
          tomorrow TYPE d.
    today = cl_abap_context_info=>get_system_date( ).
    tomorrow = today + 1.
    out->write( data = today name = `today` ).
    out->write( |\n| ).
    out->write( data = tomorrow name = `tomorrow` ).
    out->write( |\n| ).

    DATA date TYPE d.
    date = '20240101'.
    out->write( data = date name = `date` ).
    out->write( |\n| ).

    date = 20240101.
    out->write( data = date name = `date` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `16) Type conversion rules` ) ).

    "The purpose of this example is to emphasize the conversion rules
    "that should be noted when performing conversions. The example
    "uses only a selection of elementary data types.
    "The result of the conversion may be unexpected or surprising.
    "For all the details, check the ABAP Keyword Documentation.
    "In the following example, a structured type is created.
    "The components are as follows:
    "- A data object having a specific data type is to be converted.
    "- The conversion results are reflected in the other components.
    "- If there is a conversion error, the conv_err* components hold
    "  the error message.
    "An internal table is created based on this structured type.
    "Several data objects are inserted into the internal table, on the
    "basis of which a conversion is performed. The table is looped over.
    "Implicit conversions are performed using the assignment operator.

    "Some noteworthy type conversions:
    "Conversion of d to decfloat34 and i:
    "  If the source field contains a valid date in the format yyyymmdd,
    "  it is used to calculate the number of days since 01.01.0001, and
    "  this value is then converted to the internal representation of the
    "  corresponding integer. If the source field contains an invalid date,
    "  the target field is assigned the value 0.
    "Conversion of t to decfloat34 and i:
    "  If the source field contains only digits, the content is interpreted
    "  as a time specification in the format hhmmss from which the value
    "  hh*3600+mm*60+ss is calculated and then converted to the internal
    "  representation of the corresponding integer.
    "Conversion of type i to c:
    "  The value is passed right-aligned. If the target field is too short,
    "  it is truncated on the left and the carachter * is set in the first
    "  position.
    "Conversion of type i to t:
    "  The value of the integer is divided by the number of seconds in a day
    "  (86,400) and the integer remainder of the division is interpreted as
    "  the number of seconds since midnight. The resulting time is placed in
    "  the target field in the format hhmmss.
    "Conversion of type i to string:
    "  The character - is set in the last position for a negative value and
    "  a blank is set in the last position for a positive value.
    "Conversion of type decfloat34 to t:
    "  The content of the source field is first converted to data type i
    "  and then to type t.

    TYPES: BEGIN OF ts_conv_struc,
             to_be_converted  TYPE REF TO data,
             type             TYPE string,
             conv_c_len2      TYPE c LENGTH 2,
             conv_err_c_len2  TYPE string,
             conv_d           TYPE d,
             conv_err_d       TYPE string,
             conv_n_len3      TYPE n LENGTH 3,
             conv_err_n_len3  TYPE string,
             conv_t           TYPE t,
             conv_err_t       TYPE string,
             conv_decfl34     TYPE decfloat34,
             conv_err_decfl34 TYPE string,
             conv_i           TYPE i,
             conv_err_i       TYPE string,
             conv_str         TYPE string,
             conv_err_str     TYPE string,
           END OF ts_conv_struc.

    DATA tt_conv_tab TYPE TABLE OF ts_conv_struc WITH EMPTY KEY.
    DATA err TYPE REF TO cx_root.

    DATA d_to_conv TYPE d VALUE '20230101'.
    DATA t_to_conv TYPE t VALUE '120000'.
    DATA dec34_to_conv TYPE decfloat34 VALUE '36000.999'.

    "Filling internal table
    tt_conv_tab = VALUE #(
    "c
    ( to_be_converted = REF #( 'abc' )         type = `C LENGTH 3` )
    ( to_be_converted = REF #( '11111111' )    type = `C LENGTH 8` )
    "d
    ( to_be_converted = REF #( d_to_conv )     type = `D` )
    "t
    ( to_be_converted = REF #( t_to_conv )     type = `T` )
    "decfloat34
    ( to_be_converted = REF #( dec34_to_conv ) type = `DECFLOAT34` )
    "i
    ( to_be_converted = REF #( 12345678 )      type = `I` )
    ( to_be_converted = REF #( -321 )          type = `I` )
    "string
    ( to_be_converted = REF #( `hello abap` )  type = `STRING` )
    ( to_be_converted = REF #( `12345` )       type = `STRING` )
    ).

    LOOP AT tt_conv_tab REFERENCE INTO DATA(wa_ref_con).
      TRY.
          wa_ref_con->conv_c_len2 = wa_ref_con->to_be_converted->*.
          wa_ref_con->conv_err_c_len2 = `-`.
        CATCH cx_root INTO err.
          wa_ref_con->conv_err_c_len2 = err->get_text( ).
      ENDTRY.

      IF wa_ref_con->type = `T`.
        wa_ref_con->conv_err_d = `T to D not possible.`.
      ELSE.
        TRY.
            wa_ref_con->conv_d = wa_ref_con->to_be_converted->*.
            wa_ref_con->conv_err_d = `-`.
          CATCH cx_root INTO err.
            wa_ref_con->conv_err_d = err->get_text( ).
        ENDTRY.
      ENDIF.

      TRY.
          wa_ref_con->conv_n_len3 = wa_ref_con->to_be_converted->*.
          wa_ref_con->conv_err_n_len3 = `-`.
        CATCH cx_root INTO err.
          wa_ref_con->conv_err_n_len3 = err->get_text( ).
      ENDTRY.

      IF wa_ref_con->type = `D`.
        wa_ref_con->conv_err_t = `D to T not possible.`.
      ELSE.
        TRY.
            wa_ref_con->conv_t = wa_ref_con->to_be_converted->*.
            wa_ref_con->conv_err_t = `-`.
          CATCH cx_root INTO err.
            wa_ref_con->conv_err_t = err->get_text( ).
        ENDTRY.
      ENDIF.

      TRY.
          wa_ref_con->conv_decfl34 = wa_ref_con->to_be_converted->*.
          wa_ref_con->conv_err_decfl34 = `-`.
        CATCH cx_root INTO err.
          wa_ref_con->conv_err_decfl34 = err->get_text( ).
      ENDTRY.

      TRY.
          wa_ref_con->conv_i = wa_ref_con->to_be_converted->*.
          wa_ref_con->conv_err_i = `-`.
        CATCH cx_root INTO err.
          wa_ref_con->conv_err_i = err->get_text( ).
      ENDTRY.

      TRY.
          wa_ref_con->conv_str = wa_ref_con->to_be_converted->*.
          wa_ref_con->conv_err_str = `-`.
        CATCH cx_root INTO err.
          wa_ref_con->conv_err_str = err->get_text( ).
      ENDTRY.

    ENDLOOP.

    out->write( data = tt_conv_tab name = `tt_conv_tab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `17) Excursion: RTTI` ) ).

    "Using RTTI to check type compatibility
    "In the following example the applies_to_data method of the RTTI class
    "cl_abap_datadescr is used to check the compatibility of generically typed
    "data reference variables pointing to different data objects.
    "An assignment of ref1->* to ref3->* would raise an uncatchable exception.

    DATA num1 TYPE i.
    DATA num2 TYPE i.
    DATA itab_i TYPE TABLE OF i WITH EMPTY KEY.

    DATA ref1 TYPE REF TO data.
    DATA ref2 TYPE REF TO data.
    DATA ref3 TYPE REF TO data.

    ref1 = REF #( num1 ).
    ref2 = REF #( num2 ).
    ref3 = REF #( itab_i ).

    IF CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( ref1->* )
        )->applies_to_data( ref2->* ).
      ref1->* = ref2->*.
      out->write( `The types of ref1->* and ref2->* are compatible.` ).
    ELSE.
      out->write( `The types of ref1->* and ref2->* are not compatible.` ).
    ENDIF.

    out->write( |\n| ).

    IF CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( ref1->* )
        )->applies_to_data( ref3->* ).
      ref1->* = ref3->*.
      out->write( `The types of ref1->* and ref3->* are compatible.` ).
    ELSE.
      out->write( `The types of ref1->* and ref3->* are not compatible.` ).
    ENDIF.

    out->write( |\n| ).
    out->write( |\n| ).

    "Using RTTI to get type descriptions
    "In the following example, an internal table that has been filled in
    "a previous example is looped over. It contains references to various types
    "for whom specific type descriptions are retrieved.
    "You may want to set a break-point and check out the variety of information
    "contained in the the data reference variable 'rtti'.

    TYPES: BEGIN OF ts_rtti,
             absolute_name TYPE string,
             kind          TYPE string,
             type_kind     TYPE string,
           END OF ts_rtti.

    DATA rtti_tab TYPE TABLE OF ts_rtti WITH EMPTY KEY.

    LOOP AT tt_conv_tab REFERENCE INTO DATA(wa_ref).

      DATA(rtti) = CAST cl_abap_datadescr(
          cl_abap_typedescr=>describe_by_data( wa_ref->to_be_converted->* ) ).

      APPEND VALUE #( absolute_name = rtti->absolute_name
                      kind          = rtti->kind
                      type_kind     = rtti->type_kind ) TO rtti_tab.

    ENDLOOP.

    out->write( data = rtti_tab name = `rtti_tab` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `18) Constants and immutable variables` ) ).

    "As mentioned above, constants cannot be changed at runtime.
    CONSTANTS con_str TYPE string VALUE `hallo`.

    "Constants as start values for dobj declarations following value
    CONSTANTS con_underscores TYPE string VALUE `__________`.
    DATA str_w_con_as_start_value TYPE string VALUE con_underscores.

    "Immutable variables
    FINAL(do_final_inl) = 1.
    DATA(do_data_inl) = 1 + do_final_inl.
    "not possible
    "do_final_inl = 2.

    SELECT * FROM zdemo_abap_carr INTO TABLE @DATA(itab_data_inl).

    DATA itab_like_inline LIKE itab_data_inl.

    "Using an inline declaration as target of a LOOP statement
    "A value is assigned multiple times, but it cannot be changed in any other
    "write positions.
    LOOP AT itab_data_inl INTO FINAL(wa_final).

      "The following is not possible
      "wa_final-carrid = 'd'.
      "only read access
      APPEND wa_final TO itab_like_inline.
    ENDLOOP.

    "SELECT statement with a an immutable target table declared inline
    SELECT * FROM zdemo_abap_carr INTO TABLE @FINAL(itab_final_inl).

    out->write( no_output ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `19) Various ABAP glossary terms on data types and objects in a nutshell` ) ).

    "Standalone and bound data types
    "Standalone: Data type that is defined using the statement TYPES in an ABAP program, as
    "            a data type of the ABAP Dictionary or as an CDS entity.
    "Bound: Data type that only exists as a property of a data object.

    "Standalone data type
    TYPES te_a_c10 TYPE c LENGTH 10.

    "Bound data types
    DATA do_a_c20 TYPE c LENGTH 20.
    DATA do_b_like LIKE do_a_c20.
    TYPES te_b_like LIKE do_a_c20.

**********************************************************************

    "Complex and elementary data type/object
    "Elementary: Data type of fixed or variable length that is neither structured, nor a
    "            table type or a reference type.
    "Complex: Made up of other data types, for example structured data type/objects, a
    "         table type/internal tables

    "Elementary
    DATA do_c_i TYPE i.

    "Complex
    DATA: BEGIN OF struc_a,
            comp1 TYPE i,
            comp2 TYPE string,
            comp3 TYPE zdemo_abap_carr, "structure
            comp4 TYPE string_table, "internal table
            comp5 TYPE REF TO i, "reference type
          END OF struc_a.

**********************************************************************

    "Complete and generic data types
    "Complete: Non-generic data type
    "Generic:
    "- Data type that does not set all properties of a data object.
    "- Can only be used for the typing of formal parameters and field symbols.

    "Complete data type
    DATA do_d_i TYPE i.

    "Field symbols typed with generic data types
    "Note: A field symbol is a symbolic name for a data object to which actual
    "memory can be assigned at runtime. A field symbol can be used as a placeholder
    "for a data object at an operand position. For more information, see the ABAP
    "Cheat Sheet on dynamic programming. Field symbols are used in this example
    "to demonstrate generic types other than just data with which data reference
    "variables can be typed.
    "As the name implies, clike expects character-like data types. data can
    "expect any data type. This is shown in the example. Apart from the
    "character-like types, internal table types are also accepted.
    FIELD-SYMBOLS <fs_a> TYPE clike.
    FIELD-SYMBOLS <fs_b> TYPE data.

    "Data object declarations
    DATA do_e_c5 TYPE c LENGTH 5 VALUE 'abcde'.
    DATA do_f_str TYPE string VALUE `Hallo, how are you?`.
    DATA(itab_a) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).

    "Generic type clike
    "Field symbols with generic data types can be assigned appropriate values
    ASSIGN do_e_c5 TO <fs_a>.

    out->write( data = <fs_a> name = `<fs_a>` ).
    out->write( |\n| ).

    ASSIGN do_f_str TO <fs_a>.

    out->write( data = <fs_a> name = `<fs_a>` ).
    out->write( |\n| ).

    "Generic type data
    ASSIGN do_e_c5 TO <fs_b>.

    out->write( data = <fs_b> name = `<fs_b>` ).
    out->write( |\n| ).

    ASSIGN do_f_str TO <fs_b>.

    out->write( data = <fs_b> name = `<fs_b>` ).
    out->write( |\n| ).

    ASSIGN itab_a TO <fs_b>.

    out->write( data = <fs_b> name = `<fs_b>` ).
    out->write( |\n| ).

**********************************************************************

    "Variable and constant data objects
    "Variable: Named data object whose value can be changed during the runtime
    "          of an ABAP program.
    "Constant: Named data object whose value cannot be changed at runtime.

    "Variable
    DATA do_g_i TYPE i VALUE 123.
    do_g_i = 456.

    CONSTANTS con_a_i TYPE i VALUE 789.
    "An assignment as follows is not possible. The data object cannot be
    "modified.
    "con_a_i = 321.

**********************************************************************

    "Static and dynamic data objects
    "Static:
    "- Data object for which all attributes, including memory use, are specified
    "  statically by the data type.
    "- Apart from reference variables, all static data objects are flat.
    "Dynamic:
    "- Data object for which all properties apart from the memory consumption are
    "  statically determined by the data type.
    "- Dynamic data objects are strings and internal tables. They belong to the
    "  deep data objects. Structures that contain dynamic components are also
    "  dynamic data objects.

    "Static data object
    DATA do_h_c5 TYPE c LENGTH 3.
    "Dynamic data object
    DATA do_i_str TYPE string.

    "Assignments
    do_h_c5 = 'abc'.
    do_h_c5 = 'defghi'. "only 'def' assigned -> length and memory use do not change

    "Memory consumption changes for dynamic data objects
    do_i_str = `abc`.
    do_i_str = `d`.
    do_i_str = `efghijklmnopqrstuvwxyz`.

**********************************************************************

    "Static type and dynamic type
    "Both are data types of a reference variable (reference type) that determine
    "the objects a reference variable can point to.
    "Static type: For data reference variables, the static type is a data type that
    "             is always more general than or the same as the dynamic type.
    "Dynamic type: For a reference variable, the dynamic type is always more special
    "              than or equal to the static type.

    "Static type
    DATA dref_a_i TYPE REF TO i. "Static type is i
    DATA dref_b_data TYPE REF TO data. "Static type can also be generic

    "Creating data objects to refer to
    DATA do_j_i TYPE i VALUE 3.
    DATA do_k_str TYPE string VALUE `hallo`.

    "Dynamic types
    "Set a breakpoint here and check the Variables tab in ADT how the value and
    "the information changes.
    dref_a_i = REF #( do_j_i ). "Only type i possible; the dynamic type is the same

    "The dynamic type is more special than the static type (which is the generic
    "type data in this case)
    dref_b_data  = REF #( do_j_i ).
    dref_b_data  = REF #( do_k_str ).
    dref_b_data  = REF #( dref_a_i ).

**********************************************************************

    "Flat and deep data objects
    "Flat:
    "- Property of a data type, where the content of its data objects represents
    "  the actual work data.
    "- All elementary data types except string and xstring are flat
    "Deep:
    "- Dynamic data objects and reference variables are deep, and they contain
    "  references that refer to the actual content.
    "- The handling of references is implicit for dynamic data objects (strings
    "  and internal tables), and explicit for reference variables.
    "- Structures that do not contain any deep components are flat structures.
    "  Structures that contain at least one deep component are deep structures.

    "Flat data object
    DATA do_l_i TYPE i.

    "Flat structure
    DATA: BEGIN OF struc_b_flat,
            comp1 TYPE i,
            comp2 TYPE c LENGTH 3,
          END OF struc_b_flat.

    "Deep data object
    DATA do_m_str TYPE string.

    "Deep structure
    DATA: BEGIN OF struc_c_deep,
            comp1 TYPE i,
            comp2 TYPE c LENGTH 3,
            comp3 TYPE string,  "string as deep data object
            comp4 TYPE string_table, "internal table as deep data object
          END OF struc_c_deep.

**********************************************************************

    "Named and unnamed data object
    "Named: Data object that can be identified via a name.
    "Unnamed: Data object that cannot be addressed by a name. Unnamed data
    "         objects are literals and anonymous data objects.

    "Named data objects
    DATA do_n_i TYPE i.
    CONSTANTS con_b_str TYPE string VALUE `hi`.

    "Unnamed data objects
    "Literal that is output. It cannot be addressed via a dedicated name.
    out->write( `I'm a literal...` ).
    out->write( |\n| ).

    "Anonymous data object created using the NEW operator
    "Can be addressed using reference variables or field symbols.

    DATA(dref_c_str) = NEW string( `hi` ).

    out->write( data = dref_c_str->* name = `dref_c_str->*` ).
    out->write( |\n| ).

    "Anonymous data object created inline using the NEW addition to the INTO
    "clause of a SELECT statement
    SELECT *
      FROM zdemo_abap_carr
      INTO TABLE NEW @DATA(dref_d_tab)
      UP TO 3 ROWS.

    out->write( data = dref_d_tab->* name = `dref_d_tab->*` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `20) Generic ABAP types for formal parameters of methods` ) ).

    "Generic data types have already been covered above.
    "A generic data type is an incomplete type specification that covers multiple
    "complete type specifications.
    "This example demonstrates generic ABAP types in the context of formal parameters
    "of methods.
    "Unlike data objects, where the data type has a specific property and is always
    "completely known, formal parameters and field symbols that are generically typed
    "receive their complete data type only when an actual parameter is passed in a method
    "call, or, regarding field symbols, when a memory area is assigned.
    "In the following example, a method is called that has two importing parameters typed
    "with the generic type numeric. An internal table is filled with values on whose basis
    "the method is provided with actual parameters. Intentionally, various numeric values
    "are included as well as values that will not work.

    TYPES: BEGIN OF str_num,
             num1   TYPE REF TO data,
             num2   TYPE REF TO data,
             result TYPE decfloat34,
           END OF str_num.

    DATA tab_num TYPE TABLE OF str_num WITH EMPTY KEY.

    tab_num = VALUE #( ( num1 = NEW i( 1 )                  num2 = NEW i( 2 ) )
                       ( num1 = NEW decfloat34( '1.74' )    num2 = NEW decfloat34( '4.04' ) )
                       ( num1 = NEW i( 11 )                 num2 = NEW decfloat34( '10.621' ) )
                       ( num1 = NEW string( `Some string` ) num2 = NEW i( 2 ) ) ).

    LOOP AT tab_num REFERENCE INTO DATA(fp_ref).
      TRY.
          fp_ref->result = addition_with_generic_num( num1 = fp_ref->num1->* num2 = fp_ref->num2->* ).
        CATCH cx_sy_dyn_call_illegal_type INTO DATA(error).
      ENDTRY.
    ENDLOOP.

    out->write( data = tab_num name = `tab_num` ).
    out->write( |\n| ).
    out->write( data = error->get_text( ) name = `error->get_text( )` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `21) Built-in data objects` ) ).

    "This example demonstrates the availability of built-in data objects in ABAP.

    "System fields are filled by the ABAP runtime framework and can be used in an ABAP
    "program to query various things.
    "The following example covers the values provided with sy-index and sy-tabix.
    "Check both loops in the debugger and insert sy-index/sy-tabix in the
    "Variables tab to see the value change.

    DATA syidx LIKE TABLE OF sy-index.

    "In DO and WHILE loops, sy-index contains the number of previous loop passes,
    "including the current pass.
    DO 5 TIMES.
      APPEND sy-index TO syidx.
    ENDDO.

    out->write( data = syidx name = `syidx` ).
    out->write( |\n| ).

    DATA str_tab TYPE TABLE OF string.

    "In loops with LOOP AT, sy-tabix contains the line number in the table
    "index of an index table.
    LOOP AT syidx INTO DATA(wa_sy1).
      APPEND |Value of processed table line: { wa_sy1 } / Table index { sy-tabix }| TO str_tab.
    ENDLOOP.

    out->write( data = str_tab name = `str_tab` ).
    out->write( |\n| ).

    "sy-subrc contains a return code that is set by many ABAP statements.
    "In general, the value 0 means that the statement was executed without problems.

    READ TABLE syidx INDEX 6 INTO DATA(wa_sy2).

    IF sy-subrc = 0.
      out->write( |Yes, the table line was found. sy-subrc value that was returned is { sy-subrc }.| ).
    ELSE.
      out->write( |No, the table line was not found. sy-subrc value that was returned is { sy-subrc }.| ).
    ENDIF.

    out->write( |\n| ).
    out->write( |\n| ).

    "The program-global constant space has the data type c, length 1, and contains a blank character.
    "In the following example, the table lines are concatenated into a string, separated by a blank.

    DATA ctab TYPE TABLE OF c.

    ctab = VALUE #( ( 'a' ) ( 'b' ) ( 'c' ) ).

    DATA c_f TYPE c LENGTH 10.

    CONCATENATE LINES OF ctab INTO c_f SEPARATED BY space.

    out->write( data = c_f name = `c_f` ).
    out->write( |\n| ).

    "Self-reference me
    "Within the implementation of each instance method, an implicitly created local
    "reference variable called me is available at runtime, which points to the instance
    "in which the method is currently being executed.
    "me is handled like a local constant, which means that the value of me cannot be
    "changed in an instance method. The static type of me is the class in which the
    "instance method is implemented.
    "In the following example, an instance of this class is created. Then a method is
    "called, and a string is returned. The method implementation includes a data object
    "that is (intentionally) declared with the same name of the instance attribute
    "available in the private section of the class (text). Using the self-reference me
    "(me->text), you can explicitly refer to the instance attribute. Just 'text' means
    "referring to the data object locally declared in the method.

    DATA(oref) = NEW zcl_demo_abap_dtype_dobj( ).

    cl_text = 'Hallo'.

    DATA(res_str) = oref->adapt_text( ).

    out->write( data = res_str name = `res_str` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `22) Declaration context` ) ).

    "The purpose of this example is to emphasize the importance of where
    "data objects are decalred. The example deals with local declarations
    "and control structures. A data object of type i is declared within
    "the control structure. However, it is valid globally in this ABAP
    "program, i.e. in the whole method here.
    "The value of the data object is not set to 10 in each loop pass.
    "Furthermore, the data object can also be used in the second DO loop.

    DO 10 TIMES.
      DATA number TYPE i VALUE 10.
      number = number + 1.
    ENDDO.

    out->write( data = number name = `number` ).
    out->write( |\n| ).

    DO 10 TIMES.
      number = number + 1.
    ENDDO.

    out->write( data = number name = `number` ).
    out->write( |\n| ).

    "Comparing the behavior with a data object declared inline.
    "In each loop pass, the value object is set to 10. Therefore,
    "the value is 11 after exiting the DO loop.
    "The data object declared inline can also be used in the second DO loop.
    "The value of the data object is 21 when exiting the loop.
    "That means you should be careful when using variables declared inline
    "in control structures. It should only be used within the control structure
    "in which it is declared and not beyond.

    DO 10 TIMES.
      DATA(number_B) = 10.
      number_B = number_B + 1.
    ENDDO.

    out->write( data = number_b name = `number_b` ).
    out->write( |\n| ).

    DO 10 TIMES.
      number_b = number_b + 1.
    ENDDO.
    out->write( data = number_b name = `number_b` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `23) Enumerated Types and Objects` ) ).

    "Examples for enumerated types and objects are contained in
    "separate methods. Check the comments there.
    "Note that the output that is created in the methods is combined
    "in a string (table).

    "The enum_meth_params method demonstrates the use of enumerated objects
    "when comparing them with the respective enumeration constants to branch
    "to some functionality (CASE statement in the method implementation).
    "In typing the formal parameter this way, it is guaranteed that only
    "enumerated values of the enumerated type can be passed to the parameter.
    DATA enum_var1 TYPE t_enum VALUE a.
    DATA(output_for_enum_var1) = enum_meth_params( enum_var1 ).
    out->write( data = output_for_enum_var1 name = `output_for_enum_var1` ).
    out->write( |\n| ).

    DATA enum_var2 TYPE t_enum VALUE b.
    DATA(output_for_enum_var2) = enum_meth_params( enum_var2 ).
    out->write( data = output_for_enum_var2 name = `output_for_enum_var2` ).
    out->write( |\n| ).

    DATA enum_var3 TYPE t_enum VALUE d.
    DATA(output_for_enum_var3) = enum_meth_params( enum_var3 ).
    out->write( data = output_for_enum_var3 name = `output_for_enum_var3` ).
    out->write( |\n| ).

    "The enum_processing method demonstrates various ways of processing enumerated
    "objects.
    DATA(output_for_enum_processing) = enum_processing(  ).
    out->write( data = output_for_enum_processing name = `output_for_enum_processing` ).
    out->write( |\n| ).

    "The rtti_enum method demonstrates the RTTI class cl_abap_enumdescr.
    DATA(output_for_rtti_enum) = rtti_enum( ).
    out->write( data = output_for_rtti_enum name = `output_for_rtti_enum` ).

  ENDMETHOD.

  METHOD rtti_enum.

    DATA enum1 TYPE t_enum.
    enum1 = d.

    DATA enum2 TYPE t_enum_base.
    enum2 = f.

    "Return type information
    DATA(enum_descr) = CAST cl_abap_enumdescr(
          cl_abap_typedescr=>describe_by_data( enum1 ) ).

    APPEND `------ Properties for enum1 ------` TO output.

    APPEND ` kind: ` && enum_descr->kind TO output.
    APPEND ` type_kind: ` && enum_descr->type_kind TO output.
    APPEND ` base_type_kind: ` && enum_descr->base_type_kind TO output.

    DATA mem_string TYPE string.

    "For output purposes, the table content is put in a string.
    "Note the object component selector -> when reading into data reference variables
    "and accessing componts. You can also use the dereferencing operator followed by the
    "structure component selector ref->*-comp.
    LOOP AT enum_descr->members REFERENCE INTO DATA(ref_en1).
      mem_string = mem_string && ` / Name: ` && ref_en1->name && `; Value: ` && ref_en1->*-value.
    ENDLOOP.

    REPLACE FIRST OCCURRENCE OF PCRE `/\s` IN mem_string WITH ``.
    APPEND ` members:` && mem_string TO output.
    CLEAR mem_string.

    enum_descr = CAST cl_abap_enumdescr(
         cl_abap_typedescr=>describe_by_data( enum2 ) ).

    APPEND `------ Properties for enum2 ------` TO output.
    APPEND ` kind: ` && enum_descr->kind TO output.
    APPEND ` type_kind: ` && enum_descr->type_kind TO output.
    APPEND ` base_type_kind: ` && enum_descr->base_type_kind TO output.

    "For output purposes, the table content is put in a string.
    LOOP AT enum_descr->members REFERENCE INTO DATA(ref_en2).
      mem_string = mem_string && ` / Name: ` && ref_en2->name && `; Value: ` && ref_en2->value.
    ENDLOOP.

    REPLACE FIRST OCCURRENCE OF PCRE `/\s` IN mem_string WITH ``.
    APPEND ` members:` && mem_string TO output.
  ENDMETHOD.
ENDCLASS.
