"! <p class="shorttext"><strong>Grouping internal tables</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates syntax options when grouping internal tables.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <p>Find the following information in the ABAP Doc comment of class {@link zcl_demo_abap_aux}:</p>
"! <ul><li>How to get started with the example class</li>
"! <li>Structuring of (most of) the example classes</li>
"! <li>Disclaimer</li></ul>
CLASS zcl_demo_abap_sql_group_by DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    CLASS-METHODS: class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      wa      TYPE zdemo_abap_flsch,
      member  TYPE zdemo_abap_flsch,
      members TYPE STANDARD TABLE OF zdemo_abap_flsch WITH EMPTY KEY.

ENDCLASS.



CLASS zcl_demo_abap_sql_group_by IMPLEMENTATION.

  METHOD class_constructor.
    "Fill demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_aux=>set_example_divider(
     out  = out
     text = `ABAP Cheat Sheet Example: Grouping Internal Tables`
    ).

    SELECT *
           FROM zdemo_abap_flsch
           INTO TABLE @DATA(fli_tab).

    zcl_demo_abap_aux=>set_example_divider(
     out  = out
     text = `1a) Representative Binding: Grouping by one column`
    ).

    LOOP AT fli_tab INTO wa
                      GROUP BY wa-carrid.
      out->write( wa-carrid ).
    ENDLOOP.

    zcl_demo_abap_aux=>set_example_divider(
     out  = out
     text = `1b) Representative Binding: Members of one column groups`
    ).

    LOOP AT fli_tab INTO wa
                      GROUP BY wa-carrid.
      CLEAR members.
      LOOP AT GROUP wa INTO member.
        members = VALUE #( BASE members ( member ) ).
      ENDLOOP.

      out->write( members ).
      out->write( |\n| ).
    ENDLOOP.

    zcl_demo_abap_aux=>set_example_divider(
     out  = out
     text = `1c) Representative Binding: Grouping by two columns`
     ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom ).

      out->write( |{ wa-carrid } { wa-airpfrom }| ).
      out->write( |\n| ).
    ENDLOOP.

    zcl_demo_abap_aux=>set_example_divider(
     out  = out
     text = `1d) Representative Binding: Members of two column groups`
    ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom ).
      CLEAR members.
      LOOP AT GROUP wa INTO member.
        members = VALUE #( BASE members ( member ) ).
      ENDLOOP.

      out->write( members ).
      out->write( |\n| ).
    ENDLOOP.

    zcl_demo_abap_aux=>set_example_divider(
     out  = out
     text = `2a) Group Key Binding: Grouping by one column`
     ).

    LOOP AT fli_tab INTO wa
                      GROUP BY wa-carrid
                      INTO DATA(key).

      out->write( key ).
      out->write( |\n| ).
    ENDLOOP.

    zcl_demo_abap_aux=>set_example_divider(
      out  = out
      text = `2b) Group Key Binding: Members of one column groups`
    ).

    LOOP AT fli_tab INTO wa
                      GROUP BY wa-carrid
                      INTO key.
      CLEAR members.
      LOOP AT GROUP key INTO member.
        members = VALUE #( BASE members ( member ) ).
      ENDLOOP.

      out->write( members ).
      out->write( |\n| ).
    ENDLOOP.

    zcl_demo_abap_aux=>set_example_divider(
     out  = out
     text = `2c) Group Key Binding: Grouping by two columns`
   ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom )
                      INTO DATA(keys).

      out->write( keys ).
      out->write( |\n| ).
    ENDLOOP.

    zcl_demo_abap_aux=>set_example_divider(
         out  = out
         text = `2d) Group Key Binding: Members of two column groups`
       ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom )
                    INTO keys.
      CLEAR members.
      LOOP AT GROUP keys INTO member.
        members = VALUE #( BASE members ( member ) ).
      ENDLOOP.

      out->write( members ).
      out->write( |\n| ).
    ENDLOOP.

    zcl_demo_abap_aux=>set_example_divider(
         out  = out
         text = `2e) Group Key Binding: Two column groups without members`
       ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom
                                 index = GROUP INDEX size = GROUP SIZE )
                      WITHOUT MEMBERS
                      INTO DATA(keysplus).

      out->write( keysplus ).
      out->write( |\n| ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
