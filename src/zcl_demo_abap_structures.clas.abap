***********************************************************************
*
*           ABAP cheat sheet: Working with structures
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntactical options for working with
*   structures as outlined in the respective ABAP cheat sheet.
* - Topics covered: creating structures and structured types, variants
*   of structures, accessing components of structures, filling structures,
*   clearing structures, structures in use in the context of tables
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
"! <p class="shorttext synchronized">ABAP cheat sheet: Structures</p>
"! Example to demonstrate working with structures.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_structures DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    CLASS-METHODS: class_constructor.

protected section.
  PRIVATE SECTION.
    "Creating structured data types
    TYPES: "Flat structure
      BEGIN OF gty_struc,
        num1  TYPE i,
        num2  TYPE i,
        char1 TYPE c LENGTH 10,
        char2 TYPE c LENGTH 5,
        pnum  TYPE p LENGTH 8 DECIMALS 2,
      END OF gty_struc,
      "Structures within deep structure
      BEGIN OF line1,
        col1 TYPE i,
        col2 TYPE i,
      END OF line1,
      BEGIN OF line2,
        col2 TYPE i,
        col3 TYPE i,
        col4 TYPE i,
      END OF line2.

    CLASS-DATA:
      "Flat structure
      gs_struc TYPE gty_struc,
      "Deep structure 1
      BEGIN OF gs_deep1,
        comp1 TYPE c LENGTH 1 VALUE 'W',
        BEGIN OF substruc,
          comp1 TYPE c LENGTH 1 VALUE 'X',
          BEGIN OF comp2,
            col1 TYPE c LENGTH 1 VALUE 'Y',
            col2 TYPE c LENGTH 1 VALUE 'Z',
          END OF comp2,
        END OF substruc,
        itab  TYPE TABLE OF line1 WITH EMPTY KEY,
      END OF gs_deep1,
      "Deep structure 2
      BEGIN OF gs_deep2,
        BEGIN OF substruc,
          comp1 TYPE string,
          comp2 TYPE string,
          comp3 TYPE string,
        END OF substruc,
        itab  TYPE TABLE OF line2 WITH EMPTY KEY,
        comp4 TYPE i,
      END OF gs_deep2,
      "Creating internal table for displaying purposes
      gt_tab TYPE STANDARD TABLE OF zdemo_abap_tab1
          WITH NON-UNIQUE KEY key_field.

    CLASS-METHODS:
      initialize_dbtabs,
      fill_deep_structures,
      select_from_dbtab.

ENDCLASS.



CLASS ZCL_DEMO_ABAP_STRUCTURES IMPLEMENTATION.


  METHOD class_constructor.
    initialize_dbtabs( ).
    fill_deep_structures( ).
    "Filling demo database tables.
    zcl_demo_abap_flight_tables=>fill_dbtabs( ).
  ENDMETHOD.


  METHOD fill_deep_structures.
    "Clearing all content of gs_deep2
    CLEAR gs_deep2.
    "Filling nested tables in deep structures
    gs_deep2-substruc = VALUE #( comp1 = `aaa`
                                 comp2 = `bbb`
                                 comp3 = `ccc`).

    gs_deep1-itab = VALUE #(
      ( col1 = 111 col2 = 222 )
      ( col1 = 333 col2 = 444
      ) ).

    gs_deep2-itab = VALUE #(
      ( col2 = 1 col3 = 2 col4 = 3 )
      ( col2 = 4 col3 = 5 col4 = 6 )
      ( col2 = 7 col3 = 8 col4 = 9 )
      ).

    "Filling individual component that is not shared by both structures
    gs_deep2-comp4 = 999.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DATA(output) = NEW zcl_demo_abap_display( out ).

    output->display( `Demo: Working with Structures` ).

    "The following declarations are just for demonstration purposes to
    "visualize how declarations of local structures and structured
    "types can look like.

    "Declaring structured type locally (flat structure)
    TYPES: BEGIN OF lty_struc,
             num1  TYPE i,
             num2  TYPE i,
             char1 TYPE c LENGTH 10,
             char2 TYPE c LENGTH 5,
             pnum  TYPE p LENGTH 8 DECIMALS 2,
           END OF lty_struc.

    "Alternatively, you could also use the syntax below. For better
    "readability, use a chained statement with the colon as above.
    TYPES BEGIN OF gs_struc_alt.
    TYPES num1    TYPE i.
    TYPES num2    TYPE i.
    TYPES char1   TYPE c LENGTH 10.
    TYPES char2   TYPE c LENGTH 5.
    TYPES pnum    TYPE p LENGTH 8 DECIMALS 2.
    TYPES END OF gs_struc_alt.

    "Creating local structures
    "a. Based on a local structured type.
    DATA ls_struc TYPE lty_struc.

    "b. Based on global types in the DDIC
    "Note: There might also be global structures or views
    "which are not covered here.
    DATA ls_glo_tab TYPE zdemo_abap_flsch. "database table

    "c. Directly declare data object plus defining the components
    DATA: BEGIN OF ls_direct_decl,
            num1  TYPE i,
            num2  TYPE i,
            char1 TYPE c LENGTH 10,
            char2 TYPE c LENGTH 5,
            pnum  TYPE p LENGTH 8 DECIMALS 2,
          END OF ls_direct_decl.

    "Alternatively, you could also use the syntax below. For better
    "readability, use a chained statement with the colon as above.
    DATA BEGIN OF ls_direct_decl_alt.
    DATA num1    TYPE i.
    DATA num2    TYPE i.
    DATA char1   TYPE c LENGTH 10.
    DATA char2   TYPE c LENGTH 5.
    DATA pnum    TYPE p LENGTH 8 DECIMALS 2.
    DATA END OF ls_direct_decl_alt.

    "d. Based on another local structured data object + internal table
    DATA ls_like_dobj LIKE ls_struc.
    DATA ls_like_line_of_itab LIKE LINE OF gt_tab.

    "e. Using inline declaration.
    DATA(struc_inl1) = ls_struc.
    "Declaring structure inline and filling with VALUE operator
    DATA(struc_inl2) = VALUE lty_struc( num1 = 1 num2 = 2 ).

    output->display( `Variants of structures` ).
    output->display( `1) Flat structure with default values` ).

    "Flat structures only contain elementary data types

    "Flat structure with default values
    DATA: BEGIN OF ls_flat,
            num1  TYPE i VALUE 1,
            num2  TYPE i VALUE 2,
            char1 TYPE c LENGTH 10 VALUE 'abcdefghij',
            char2 TYPE c LENGTH 5 VALUE 'klmno',
            pnum  TYPE p LENGTH 8 DECIMALS 2 VALUE '123.45',
          END OF ls_flat.

    output->display( input = ls_flat name = `ls_flat` ).

    output->next_section( `2) Nested structure` ).

    "Nested structures contain at least one structure as component

    "Nested structure with default values
    DATA: BEGIN OF ls_nested_address,
            BEGIN OF name,
              title      TYPE string VALUE `Mr.`,
              first_name TYPE string VALUE `Duncan`,
              surname    TYPE string VALUE `Pea`,
            END OF name,
            BEGIN OF street,
              name   TYPE string VALUE `Vegetable Lane`,
              number TYPE string VALUE `11`,
            END OF street,
            BEGIN OF city,
              zipcode TYPE string VALUE `349875`,
              name    TYPE string VALUE `Botanica`,
            END OF city,
          END OF ls_nested_address.

    output->display( input = ls_nested_address name = `ls_nested_address` ).

    output->next_section( `3) Deep structure with strings` ).

    "Deep structures contain at least one deep component, for
    "example, internal tables, strings.

    "Deep structure with strings and with default values.
    DATA: BEGIN OF ls_flat_address,
            name   TYPE string VALUE `Mr. Duncan Pea`,
            street TYPE string VALUE `Vegetable Lane 11`,
            city   TYPE string VALUE `349875 Botanica`,
          END OF ls_flat_address.

    output->display( input = ls_flat_address name = `ls_flat_address` ).

    output->next_section( `4) Deep structure with internal table as component` ).

    "Structured type for nested internal table
    TYPES: BEGIN OF lty_flights,
             connid    TYPE zdemo_abap_flsch-connid,
             countryfr TYPE zdemo_abap_flsch-countryfr,
             cityfrom  TYPE zdemo_abap_flsch-cityfrom,
             airpfrom  TYPE zdemo_abap_flsch-airpfrom,
             countryto TYPE zdemo_abap_flsch-countryto,
             cityto    TYPE zdemo_abap_flsch-cityto,
             airpto    TYPE zdemo_abap_flsch-airpto,
           END OF lty_flights.

    "Creating deep structure
    DATA: BEGIN OF ls_flights,
            carrier      TYPE zdemo_abap_flsch-carrid VALUE 'LH',
            carrier_name TYPE zdemo_abap_carr-carrname VALUE 'Lufthansa',
            lt_flights   TYPE TABLE OF lty_flights WITH EMPTY KEY,
          END OF ls_flights.

    "Filling nested internal table for the output
    SELECT *
      FROM zdemo_abap_flsch
      WHERE carrid = 'LH'
      INTO CORRESPONDING FIELDS OF TABLE @ls_flights-lt_flights
      UP TO 4 ROWS.

    output->display( input = ls_flights name = `ls_flights` ).

    output->next_section( `Accessing and filling structures` ).
    output->display( `5) Filling structure components` &&
       ` using the component selector` ).

    gs_struc-num1  = 1.
    gs_struc-num2  = 2.
    gs_struc-char1 = 'aaa'.
    gs_struc-char2 = 'bbb'.
    gs_struc-pnum  = '333.33'.

    output->display( input = gs_struc name = `gs_struc` ).

    output->next_section( `6) Filling structure components ` &&
    `using the VALUE operator` ).

    "Value assignments by addressing the structure components individually
    "can be very bulky. Hence, the use of the VALUE operator is
    "very handy for the value assignment, especially for filling structure
    "components at operand position. In below examples the # sign is used
    "before the parentheses which means that the type of the operand can be
    "implicitly derived.

    "Flat structure
    gs_struc = VALUE #( num1  = 3
                        num2  = 4
                        char1 = 'ccc'
                        char2 = 'ddd'
                        pnum  = '555.55' ).

    "Nested structure
    ls_nested_address = VALUE #(
      name   = VALUE #( title = `Mrs.`
                        first_name = `Jane`
                        surname = `Doe` )
      street = VALUE #( name  = `Main Street`
                        number = 1 )
      city   = VALUE #( zipcode = 12345
                        name = `London` ) ).

    "Deep structure
    ls_flights = VALUE #(
        carrier      = 'AA'
        carrier_name = 'American Airlines'
        lt_flights   = VALUE #( ( connid    = 17 countryfr = 'US'
                                  cityfrom  = 'New York'
                                  airpfrom  = 'JFK'
                                  countryto = 'US'
                                  cityto    = 'San Francisco'
                                  airpto    = 'SFO' )
                                ( connid    = 64
                                  countryfr = 'US'
                                  cityfrom  = 'San Francisco'
                                  airpfrom  = 'SFO'
                                  countryto = 'US'
                                  cityto    = 'New York'
                                  airpto    = 'JFK' ) ) ).

    output->display( input = gs_struc name = `gs_struc` ).
    output->display( input = ls_nested_address name = `ls_nested_address` ).
    output->display( input = ls_flights name = `ls_flights` ).

    output->next_section( `7) Creating and filling a new structure ` &&
    `using the VALUE operator` ).

    "In the example below in which a new structure is created by declaring
    "a variable inline the '#' sign cannot be used before the parentheses
    "because a type cannot be derived. Instead, the type must be
    "specified before the parentheses explicitly.

    DATA(ls_copy) = VALUE gty_struc( num1  = 5
                                     num2  = 6
                                     char1 = 'ggg'
                                     char2 = 'hhh'
                                     pnum  = '555.55' ).

    output->display( input = ls_copy name = `ls_copy` ).

    output->next_section( `8) Accessing individual components using the ` &&
    `component selector "-"` ).

    "Assigning value of individual component to a variable
    DATA(lv_copy) = gs_struc-num1.

    "Assigning a value to a component in a nested structure.
    ls_nested_address-name-first_name = 'Emma'.

    "Assigning a value to a component in a deep structure.
    "The table line is determined using a table expression.
    ls_flights-lt_flights[ 1 ]-cityto = 'San Fran'.

    output->display( input = lv_copy name = `lv_copy` ).
    output->display( input = ls_nested_address-name-first_name name = `ls_nested_address-name-first_name` ).
    output->display( input = ls_flights-lt_flights[ 1 ]-cityto name = `ls_flights-lt_flights[ 1 ]-cityto` ).

    output->next_section( `9) Excursion: Addressing components of a variable` &&
    ` referencing to a structure ` ).

    "Creating a data reference variable.
    DATA(ref) = NEW gty_struc( ).

    "Assigning a structure to the data reference
    ref->* = gs_struc.

    "Accessing a component using the object component selector
    DATA(ref_comp1) = ref->char1.

    "The following syntax is also possible but less comfortable.
    DATA(ref_comp2) = ref->*-char2.

    output->display( input = ref_comp1 name = `ref_comp1` ).
    output->display( input = ref_comp2 name = `ref_comp2` ).

    output->next_section( `10) Using structure components for ` &&
    `data type and data object declarations` ).

    TYPES: lty_1 TYPE gty_struc-num1,
           lty_2 LIKE gs_struc-num2.

    DATA: lv_num1 TYPE gty_struc-num1 VALUE 123,
          lv_num2 LIKE gs_struc-num2 VALUE 456.

    output->display( input = lv_num1 name = `lv_num1` ).
    output->display( input = lv_num2 name = `lv_num2` ).

    output->next_section( `11) Copying content of a structure to another ` &&
    `one that has the same type` ).

    "Note: In the case below, a MOVE-CORRESPONDING statement as shown
    "further down would have the same effect:
    "MOVE-CORRESPONDING gs_struc TO gs_struc_2.

    DATA gs_struc_2 TYPE gty_struc.

    gs_struc_2 = gs_struc.

    output->display( input = gs_struc_2 name = `gs_struc_2` ).

    output->next_section( `12) Copying content of a structure to another` &&
    ` one that has a different type using` &&
    ` MOVE-CORRESPONDING/CORRESPONDING operator` ).

    "Both statements with MOVE-CORRESPONDING and the CORRESPONDING
    "operator are used to assign identically named components of
    "structures to each other.
    "Note: For value assignments, generally bear in mind that there are
    "special conversion and comparison rules that apply to assignments.
    "The following examples focus on flat structures.

    "Creating flat structure with different type and assigning
    "default values.
    DATA: BEGIN OF gs_struc_diff,
            num1  TYPE i VALUE 111,
            num2  TYPE i VALUE 222,
            char1 TYPE c LENGTH 10 VALUE 'AAA',
            c1    TYPE c LENGTH 1 VALUE 'B',
          END OF gs_struc_diff.

    "Copying structure to have the same values for another syntax variant.
    DATA(gs_struc_diff2) = gs_struc_diff.
    DATA(gs_struc_diff3) = gs_struc_diff.
    DATA(gs_struc_diff4) = gs_struc_diff.
    DATA(gs_struc_diff5) = gs_struc_diff.

    output->display( `Original content of structures:` ).
    output->display( input = gs_struc name = `gs_struc` ).
    output->display( input = gs_struc_diff name = `gs_struc_diff` ).

    "Identically named components are moved...
    "... and the content in nonidentical components of the target
    "structure are kept.
    MOVE-CORRESPONDING gs_struc TO gs_struc_diff.

    "... and the content in nonidentical components in the target
    "structure are initialized.
    gs_struc_diff2   = CORRESPONDING #( gs_struc ).

    "... and the content in nonidentical components of the target
    "structure are kept. Same as MOVE-CORRESPONDING without additions.
    gs_struc_diff3 = CORRESPONDING #( BASE ( gs_struc_diff3 )
                                      gs_struc ).

    "MAPPING addition: Specifying components of a source structure that
    "are assigned to the components of a target structure in mapping
    "relationships. Note the conversion and assignement rules.
    gs_struc_diff4 = CORRESPONDING #( BASE ( gs_struc_diff4 )
                                      gs_struc MAPPING c1 = char2 ).

    "EXCEPT addition: Excluding components from the assignment.
    gs_struc_diff5 = CORRESPONDING #( BASE ( gs_struc_diff5 )
                                      gs_struc EXCEPT num2 ).

    output->display( `Results of statements:` ).
    output->display( input = gs_struc_diff name = `gs_struc_diff` ).
    output->display( input = gs_struc_diff2 name = `gs_struc_diff2` ).
    output->display( input = gs_struc_diff3 name = `gs_struc_diff3` ).
    output->display( input = gs_struc_diff4 name = `gs_struc_diff4` ).
    output->display( input = gs_struc_diff5 name = `gs_struc_diff5` ).

    output->next_section( `13) Excursion: Copying content of a deep ` &&
    `structure to another one by using various additions` ).
    output->display( 'Original content of deep structures:' ).

    "Note: The example purposely uses non-fitting components
    "to emphasize conversion and assignment rules.

    output->display( input = gs_deep1 name = `gs_deep1` ).
    output->display( input = gs_deep2 name = `gs_deep2` ).

    output->display( `13a) MOVE-CORRESPONDING without additions` ).

    "Notes on the result:
    "- Existing content of identically named components is replaced.
    "- Content in nonidentical components of the target structure is
    "  kept.
    "- Substructure substruc is converted to string. Note that the two
    "  components in component substruc-comp2 of gs_deep1 are drawn
    "  together when being converted to string.
    "- Content of gs_deep2-itab is replaced by table content of
    "  gs_deep1-itab. Value assignment, for example,
    "  for col2 in gs_deep2-itab: Despite the fact that there is no
    "  identically named component col1  in the target structure,
    "  values are assigned starting with the first column of the source
    "  structure.

    MOVE-CORRESPONDING gs_deep1 TO gs_deep2.

    output->display( input = gs_deep2 name = `gs_deep2` ).

    fill_deep_structures( ).

    output->next_section( `13b) MOVE-CORRESPONDING with the addition ` &&
    `EXPANDING NESTED TABLES` ).

    "Notes on the result:
    "- Existing content of identically named components is replaced.
    "- Content in nonidentical components of the target structure is
    "  kept.
    "- Substructure substruc: Same as above
    "- Content of gs_deep2-itab is replaced by table content of
    "  gs_deep1-itab. Due to the addition EXPANDING NESTED TABLES, the
    "  value assignment happens for identically named components. Hence,
    "  only col2 as the only shared and identically named component is
    "  filled.

    MOVE-CORRESPONDING gs_deep1 TO gs_deep2 EXPANDING NESTED TABLES.

    output->display( input = gs_deep2 name = `gs_deep2` ).

    fill_deep_structures( ).

    output->next_section( `13c) MOVE-CORRESPONDING with the addition` &&
    ` KEEPING TARGET LINES` ).

    "Notes on the result:
    "- Existing content of identically named components is replaced.
    "- Content in nonidentical components of the target structure is
    "  kept.
    "- Substructure substruc: Same as above
    "- Content of gs_deep2-itab is kept due to the addition KEEPING
    "  TARGET LINES and content of gs_deep1-itab is added. The value
    "  assignment concerning the added lines happens like the
    "  MOVE-CORRESPONDING statement without addition. That is, despite
    "  the fact that there is no identically named component col1 in
    "  the target structure, values are assigned starting with the
    "  first column of the source structure.

    MOVE-CORRESPONDING gs_deep1 TO gs_deep2 KEEPING TARGET LINES.

    output->display( input = gs_deep2 name = `gs_deep2` ).

    fill_deep_structures( ).

    output->next_section( `13d) MOVE-CORRESPONDING with the addition ` &&
    `EXPANDING NESTED TABLES KEEPING TARGET LINES` ).

    "Notes on the result:
    "- Existing content of identically named components is replaced.
    "- Content in nonidentical components of the target structure is
    "  kept.
    "- Substructure substruc: Same as above
    "- Content of gs_deep2-itab is kept due to the addition KEEPING
    "  TARGET LINES. Content of gs_deep1-itab is added. The value
    "  assignment concerning the added lines happens like the
    "  MOVE-CORRESPONDING statement with the addition EXPANDING NESTED
    "  TABLES. That is, the value assignment happens for identically
    "  named components. Hence, only col2 as the only shared and
    "  identically named component is filled.

    MOVE-CORRESPONDING gs_deep1 TO gs_deep2
      EXPANDING NESTED TABLES KEEPING TARGET LINES.

    output->display( input = gs_deep2 name = `gs_deep2` ).

    fill_deep_structures( ).

    output->next_section( `13e) CORRESPONDING operator without addition` ).

    "Notes on the result:
    "- Existing content of identically named components is replaced.
    "- Content in nonidentical components of the target structure is
    "  initialized.
    "- Substructure substruc: Same as above
    "- Content of gs_deep2-itab is replaced by table content of
    "  gs_deep1-itab. Note the value assignment, for example, for col2
    "  in gs_deep2-itab. Despite the fact that there is no identically
    "  named component comp1 in the target structure, values are
    "  assigned starting with the first column of the source structure.

    gs_deep2 = CORRESPONDING #( gs_deep1 ).

    output->display( input = gs_deep2 name = `gs_deep2` ).

    fill_deep_structures( ).

    output->next_section( `13f) CORRESPONDING operator with the addition` &&
    ` DEEP` ).

    "Notes on the result:
    "- Existing content of identically named components is replaced.
    "- Content in nonidentical components of the target structure is
    "  initialized.
    "- Substructure substruc: Same as above
    "- Content of gs_deep2-itab is replaced by table content of
    "  gs_deep1-itab. Due to the addition DEEP, the value assignment
    "  happens for identically named components in the nested table.
    "  Hence, only col2 as the only shared and identically named
    "  component is filled.

    gs_deep2 = CORRESPONDING #( DEEP gs_deep1 ).

    output->display( input = gs_deep2 name = `gs_deep2` ).

    fill_deep_structures( ).

    output->next_section( `13g) CORRESPONDING operator with the addition` &&
    ` BASE` ).

    "Notes on the result:
    "- Existing content of identically named components is replaced.
    "- Content in nonidentical components of the target structure is
    "  kept.
    "- Substructure substruc: Same as above
    "- Content of gs_deep2-itab is replaced by table content of
    "  gs_deep1-itab. The value assignment in the nested table happens
    "  like using the CORRESPONDING operator without addition. Note the
    "  value assignment, for example, for col2 in gs_deep2-itab.
    "  Despite the fact that there is no identically named component
    "  col1 in the target structure, values are assigned starting with
    "  the first column of the source structure.

    gs_deep2 = CORRESPONDING #( BASE ( gs_deep2 ) gs_deep1 ).

    output->display( input = gs_deep2 name = `gs_deep2` ).

    fill_deep_structures( ).

    output->next_section( `13h) CORRESPONDING operator with the additions ` &&
    `DEEP BASE` ).

    "Notes on the result:
    "- Existing content of identically named components is replaced.
    "- Content in nonidentical components of the target structure is
    "  kept.
    "- Substructure substruc: Same as above
    "- Content of gs_deep2-itab is replaced by table content of
    "  gs_deep1-itab. The value assignment in the nested table happens
    "  like using the CORRESPONDING operator with the addition DEEP.
    "  That is,  the value assignment happens for identically named
    "  components in the nested table. Hence, only col2 as the only
    "  shared and identically named component is filled.

    gs_deep2 = CORRESPONDING #( DEEP BASE ( gs_deep2 ) gs_deep1 ).

    output->display( input = gs_deep2 name = `gs_deep2` ).

    fill_deep_structures( ).

    output->next_section( `13i) CORRESPONDING operator with the additions ` &&
    `APPENDING` ).

    "Notes on the result:
    "- Existing content of identically named components is replaced.
    "- Content in nonidentical components of the target structure is
    "  kept.
    "- Substructure substruc: Same as above
    "- Content of gs_deep2-itab is kept and content of gs_deep1-itab is
    "  added. The value assignment concerning the added lines happens
    "  like using the CORRESPONDING operator without addition. Note the
    "  value assignment, for example, for col2 in gs_deep2- itab.
    "  Despite the fact that there is no identically named component
    "  col1 in the target structure, values are assigned starting with
    "  the first column of the source structure.

    gs_deep2 = CORRESPONDING #( APPENDING ( gs_deep2 ) gs_deep1 ).

    output->display( input = gs_deep2 name = `gs_deep2` ).

    fill_deep_structures( ).

    output->next_section( `13j) CORRESPONDING operator with the additions ` &&
    `DEEP APPENDING` ).

    "Notes on the result:
    "- Existing content of identically named components is replaced.
    "- Content in nonidentical components of the target structure is
    "  kept.
    "- Substructure substruc: Same as above
    "- Content of gs_deep2-itab is kept and content of gs_deep1-itab is
    "  added. The value assignment concerning the added lines happens
    "  like using the CORRESPONDING operator with the addition DEEP.
    "  That is, the value assignment happens for identically named
    "  components in the nested table. Hence, only col2 as the only
    "  shared and identically named component is filled.
    "- It has the same effect as using DEEP APPENDING BASE.

    gs_deep2 = CORRESPONDING #( DEEP APPENDING ( gs_deep2 )
                                gs_deep1 ).

    output->display( input = gs_deep2 name = `gs_deep2` ).

    output->next_section( `14) Clearing individual components of a ` &&
    `structure and the complete structure` ).

    "Clearing individual component
    CLEAR gs_struc-char1.

    output->display( input = gs_struc name = `gs_struc` ).

    "Clearing the whole structure
    CLEAR gs_struc.

    output->display( input = gs_struc name = `gs_struc` ).

    output->next_section( `Structures in use in the context of tables` ).
    output->display( `15) Reading a line from a database table into a ` &&
    `structure ...` ).
    output->display( `15a) ... that has a matching type` ).

    "The first entry that is found according to the WHERE condition is
    "returned. Instead of creating a structure having a matching type,
    "the structure can be declared inline.

    DATA ls_flsch1 TYPE zdemo_abap_flsch.

    SELECT SINGLE FROM zdemo_abap_flsch
      FIELDS *
      WHERE carrid = 'LH' AND connid = '0400'
      INTO @ls_flsch1.

    SELECT SINGLE FROM zdemo_abap_flsch
      FIELDS *
      WHERE carrid = 'LH' AND connid = '0400'
      INTO @DATA(ls_flsch2).

    output->display( input = ls_flsch1 name = `ls_flsch1` ).
    output->display( input = ls_flsch2 name = `ls_flsch2` ).

    output->next_section( `15b) ... that has a different type` ).

    "Creating structure having a different type.
    DATA: BEGIN OF ls_fli_diff,
            carrid    TYPE  zdemo_abap_flsch-carrid,
            connid    TYPE zdemo_abap_flsch-connid,
            countryfr TYPE zdemo_abap_flsch-countryfr,
            cityfrom  TYPE zdemo_abap_flsch-cityfrom,
            countryto TYPE zdemo_abap_flsch-countryto,
            cityto    TYPE zdemo_abap_flsch-cityto,
            fldate    TYPE zdemo_abap_fli-fldate,
          END OF ls_fli_diff.

    SELECT SINGLE FROM zdemo_abap_flsch
      FIELDS *
      WHERE carrid = 'JL' AND connid = '0408'
      INTO CORRESPONDING FIELDS OF @ls_fli_diff.

    output->display( input = ls_fli_diff name = `ls_fli_diff` ).

    output->next_section( `16) Reading a line from an internal table into a structure ...` ).
    output->display( `16a) ... using a SELECT statement` ).

    "Creating and filling an internal table to be read from
    DATA itab TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY.
    SELECT FROM zdemo_abap_flsch
      FIELDS *
      WHERE carrid = 'LH' ORDER BY PRIMARY KEY
      INTO TABLE @itab
      UP TO 4 ROWS.

    "Reading from an internal table
    SELECT SINGLE FROM @itab AS itab
      FIELDS *
      WHERE carrid = 'LH'
      INTO @DATA(ls_select_itab).

    output->display( input = ls_select_itab name = `ls_select_itab` ).

    output->next_section( `16b) ... using a READ TABLE statement` ).

    "The example shows the reading of one line into a work area, field
    "symbol and a data reference variable, all representing structured
    "data objects and declared inline below. Here, the reading of a
    "line is based on the line number by specifying INDEX.

    "Copying line into a work area
    READ TABLE itab INTO DATA(ls_read_table) INDEX 1.

    "Assignment to a field symbol
    READ TABLE itab ASSIGNING FIELD-SYMBOL(<fs1>) INDEX 2.

    "Reading into a data reference variable
    READ TABLE itab REFERENCE INTO DATA(dref) INDEX 3.

    output->display( input = ls_read_table name = `ls_read_table` ).
    output->display( input = <fs1> name = `<fs1>` ).
    output->display( input = dref->* name = `dref->*` ).

    output->next_section( `16c) ... using a table expression` ).
    "The line number, that is, the index, is specified in square
    "brackets.

    DATA(ls_table_exp) = itab[ 3 ].

    output->display( input = ls_table_exp name = `ls_table_exp` ).

    output->next_section( `17) Sequentially reading a line from ...` ).
    output->display( `17a) ... a database table into a structure` ).

    "In the given simple example, the line that is found and returned
    "in a structure, that is declared inline, is simply added to an
    "internal table.

    SELECT FROM zdemo_abap_flsch
      FIELDS *
      WHERE carrid = 'AZ'
      INTO @DATA(ls_sel_loop).
      IF sy-subrc = 0.
        APPEND  ls_sel_loop TO itab.
      ENDIF.
    ENDSELECT.

    output->display( input = itab name = `itab` ).

    output->next_section( `17b) ... an internal table into a structure` ).

    "The given example covers the reading of a line into a field symbol.
    "Within the loop, a modification is carried out on a component
    "of the structures.

    LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs_loop>) WHERE carrid <> 'LH'.
      <fs_loop>-carrid = 'XY'.
    ENDLOOP.

    output->display( input = itab name = `itab` ).

    output->next_section( `18) Inserting an individual row from a structure` &&
    ` into a database table` ).

    "The statements in the given example can be considered as
    "alternatives. The third statement demonstrates that the structure
    "might also be created and filled in place instead of inserting a
    "line from an existing structure.

    DATA ls_struc_db TYPE zdemo_abap_tab1.

    ls_struc_db = VALUE #( key_field = 1
                           char1     = 'aaa'
                           char2     = 'bbb'
                           num1      = 2
                           num2      = 3 ).

    INSERT INTO zdemo_abap_tab1 VALUES @ls_struc_db.

    "Structure filled anew with new primary key to
    "avoid duplicate key error.
    ls_struc_db = VALUE #( key_field = 2
                           char1     = 'ccc'
                           char2     = 'ddd'
                           num1      = 4
                           num2      = 5 ).

    INSERT zdemo_abap_tab1 FROM @ls_struc_db.

    INSERT zdemo_abap_tab1 FROM @( VALUE #( key_field = 3
                                            char1     = 'eee'
                                            char2     = 'fff'
                                            num1      = 6
                                            num2      = 7 ) ).

    select_from_dbtab( ).
    output->display( input = gt_tab name = `gt_tab` ).

    output->next_section( `19) Updating an individual row from a structure` &&
    ` in a database table` ).

    ls_struc_db = VALUE #( key_field = 2
                           char1     = 'GGG'
                           char2     = 'HHH'
                           num1      = 8
                           num2      = 9 ).

    UPDATE zdemo_abap_tab1 FROM @ls_struc_db.

    UPDATE zdemo_abap_tab1 FROM @( VALUE #( key_field = 3
                                            char1     = 'III'
                                            char2     = 'JJJ'
                                            num1      = 10
                                            num2      = 11 ) ).

    select_from_dbtab( ).
    output->display( input = gt_tab name = `gt_tab` ).

    output->next_section( `20) Excursion: Updating an individual row from a ` &&
    `structure in a database table without overwriting specific ` &&
    `components` ).

    "If you want to update a database table row from a structure by
    "specifying components to be changed without overwriting other
    "components, you might choose the following way. First, read the
    "intended line from the database table into a structure.
    "Then, use the VALUE operator with the addition BASE and specify
    "the components to be changed.

    SELECT SINGLE *
      FROM zdemo_abap_tab1
      WHERE key_field = 2
      INTO @DATA(wa).

    UPDATE zdemo_abap_tab1 FROM @( VALUE #( BASE wa char2 = '###' ) ).

    select_from_dbtab( ).
    output->display( input = gt_tab name = `gt_tab` ).

    output->next_section( `21) Updating or creating an individual` &&
    ` row in a database table from a structure using MODIFY` ).

    "You can update or create an individual row in a database table
    "from a structure using ABAP SQL statements with MODIFY. If a
    "line in the database table already exists having the same keys as
    "specified in the structure, the line gets updated. If a line does
    "not exist with the keys specified in the structure, a new line is
    "created in the database table. In the given example, the first
    "statement demonstrates a modification of an existing line in the
    "database table.The second and third statements create a new line
    "in the database table. The third statement demonstrates that the
    "structure might also be created and filled in place instead of
    "inserting a line based on an existing structure.

    ls_struc_db = VALUE #( key_field = 1
                           char1     = 'kkk'
                           char2     = 'lll'
                           num1      = 12
                           num2      = 13 ).

    MODIFY zdemo_abap_tab1 FROM @ls_struc_db.

    ls_struc_db = VALUE #( key_field = 4
                           char1     = 'mmm'
                           char2     = 'nnn'
                           num1      = 14
                           num2      = 15 ).

    MODIFY zdemo_abap_tab1 FROM @ls_struc_db.

    MODIFY zdemo_abap_tab1 FROM @( VALUE #( key_field = 5
                                            char1     = 'ooo'
                                            char2     = 'ppp'
                                            num1      = 16
                                            num2      = 17 ) ).

    select_from_dbtab( ).
    output->display( input = gt_tab name = `gt_tab` ).

    output->next_section( `22) Adding rows to and updating individual rows` &&
    ` in an internal table from a structure` ).

    "INSERT and MODIFY are ABAP statements in this context, not ABAP SQL
    "statements. Both INSERT and APPEND add one line (or more) to an
    "internal table. While APPEND adds at the bottom of the internal
    "table, INSERT can be used to add lines at a specific position in
    "tables. MODIFY changes the content of an internal table entry.

    ls_struc_db = VALUE #( key_field = 6
                           char1     = 'ZZZ'
                           char2     = 'YYY'
                           num1      = 18
                           num2      = 19 ).

    INSERT ls_struc_db INTO TABLE gt_tab.

    INSERT VALUE #( key_field = 7
                    char1     = 'XXX'
                    char2     = 'WWW'
                    num1      = 20
                    num2      = 21 ) INTO TABLE gt_tab.

    ls_struc_db = VALUE #( key_field = 8
                           char1     = 'VVV'
                           char2     = 'UUU'
                           num1      = 22
                           num2      = 23 ).

    APPEND ls_struc_db TO gt_tab.

    APPEND VALUE #( key_field = 9
                    char1     = 'TTT'
                    char2     = 'SSS'
                    num1      = 24
                    num2      = 25 ) TO gt_tab.

    ls_struc_db = VALUE #( key_field = 1
                           char1     = 'RRR'
                           char2     = 'QQQ'
                           num1      = 26
                           num2      = 27 ).

    MODIFY TABLE gt_tab FROM ls_struc_db.

    MODIFY TABLE gt_tab FROM VALUE #( key_field = 2
                                      char1     = 'PPP'
                                      char2     = 'OOO'
                                      num1      = 28
                                      num2      = 29 ).

    output->display( input = gt_tab name = `gt_tab` ).

    output->next_section( `23) Excursion: Including structures` ).

    "The example shows the inclusion of structured types and data
    "objects in another structure. First, three structured types as
    "well as a structured data object based on one of those types are
    "created. Then, the types and the structure are included in the
    "structured type address_type. With the optional addition AS and
    "the specification of a name, the included components can be
    "addressed by this common name as if the components are actually
    "components of a substructure. With the optional addition
    "RENAMING WITH SUFFIX and the specification of a name, the included
    "components get a suffix name to avoid naming conflicts with other
    "components.

    TYPES: BEGIN OF name_type,
             title   TYPE string,
             prename TYPE string,
             surname TYPE string,
           END OF name_type,
           BEGIN OF street_type,
             name   TYPE string,
             number TYPE string,
           END OF street_type,
           BEGIN OF city_type,
             zipcode TYPE string,
             name    TYPE string,
           END OF city_type.

    DATA: city_struc TYPE city_type.

    TYPES BEGIN OF address_type.
    INCLUDE TYPE name_type AS name.
    INCLUDE TYPE street_type AS street RENAMING WITH SUFFIX _street.
    INCLUDE STRUCTURE city_struc AS city RENAMING WITH SUFFIX _city.
    TYPES END OF address_type.

    DATA: name    TYPE name_type,
          address TYPE address_type.

    name-title = `Mr.`.
    name-prename = `Duncan`.
    name-surname = `Pea`.
    address-name = name.
    address-street-name = `Vegetable Lane`.
    address-street-number = `11`.
    address-zipcode_city = `349875`.
    address-name_city = `Botanica`.

    output->display( input = address name = `address` ).

  ENDMETHOD.


  METHOD initialize_dbtabs.
    DELETE FROM zdemo_abap_tab1.
  ENDMETHOD.


  METHOD select_from_dbtab.

    SELECT FROM zdemo_abap_tab1
      FIELDS *
      WHERE key_field <> 0
      ORDER BY key_field
      INTO TABLE @gt_tab.

  ENDMETHOD.
ENDCLASS.
