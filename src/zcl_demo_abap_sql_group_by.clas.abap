***********************************************************************
*
*       ABAP cheat sheet: ABAP SQL - Grouping Internal Tables
*
* -------------------------- PURPOSE ----------------------------------
* Example to demonstrate syntax options when grouping internal tables.
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
"! <p class="shorttext synchronized">ABAP cheat sheet: ABAP SQL - Grouping internal tables</p>
"! Example to demonstrate grouping internal tables.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_sql_group_by DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    CLASS-METHODS: class_constructor.

protected section.
  PRIVATE SECTION.
    CLASS-DATA:
      wa      TYPE zdemo_abap_flsch,
      member  TYPE zdemo_abap_flsch,
      members TYPE STANDARD TABLE OF zdemo_abap_flsch WITH EMPTY KEY.

ENDCLASS.



CLASS zcl_demo_abap_sql_group_by IMPLEMENTATION.


  METHOD class_constructor.
    "Fill demo database tables.
    zcl_demo_abap_flight_tables=>fill_dbtabs( ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DATA(output) = NEW zcl_demo_abap_display( out ).

    output->display( `ABAP Cheat Sheet Example: Grouping Internal Tables` ).

    SELECT *
           FROM zdemo_abap_flsch
           INTO TABLE @DATA(fli_tab).

    output->next_section( `1) Representative Binding` ).
    output->display( `1a) Grouping by one column` ).

    LOOP AT fli_tab INTO wa
                      GROUP BY wa-carrid.
      output->display( wa-carrid ).
    ENDLOOP.

    output->next_section( `1b) Members of one column groups` ).

    LOOP AT fli_tab INTO wa
                      GROUP BY wa-carrid.
      CLEAR members.
      LOOP AT GROUP wa INTO member.
        members = VALUE #( BASE members ( member ) ).
      ENDLOOP.

      output->display( members ).
    ENDLOOP.

    output->next_section( `1c) Grouping by two columns` ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom ).

     output->display( |{ wa-carrid } { wa-airpfrom }| ).
    ENDLOOP.

    output->next_section( `1d) Members of two column groups` ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom ).
      CLEAR members.
      LOOP AT GROUP wa INTO member.
        members = VALUE #( BASE members ( member ) ).
      ENDLOOP.

      output->display( members ).
    ENDLOOP.

    output->next_section( `2) Group Key Binding` ).
    output->display( `2a) Grouping by one column` ).

    LOOP AT fli_tab INTO wa
                      GROUP BY wa-carrid
                      INTO DATA(key).

      output->display( key ).
    ENDLOOP.

    output->next_section( `2b) Members of one column groups` ).

    LOOP AT fli_tab INTO wa
                      GROUP BY wa-carrid
                      INTO key.
      CLEAR members.
      LOOP AT GROUP key INTO member.
        members = VALUE #( BASE members ( member ) ).
      ENDLOOP.

      output->display( members ).
    ENDLOOP.

    output->next_section( `2c) Grouping by two columns` ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom )
                      INTO DATA(keys).

      output->display( keys ).
    ENDLOOP.

    output->next_section( `2d) Members of two column groups` ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom )
                    INTO keys.
      CLEAR members.
      LOOP AT GROUP keys INTO member.
        members = VALUE #( BASE members ( member ) ).
      ENDLOOP.

      output->display( members ).
    ENDLOOP.

    output->next_section( `2e) Two column groups without members` ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom
                                 index = GROUP INDEX size = GROUP SIZE )
                      WITHOUT MEMBERS
                      INTO DATA(keysplus).

      output->display( keysplus ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
