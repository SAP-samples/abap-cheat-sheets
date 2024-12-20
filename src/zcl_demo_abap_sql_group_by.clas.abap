"! <p class="shorttext"><strong>Grouping internal tables</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates syntax options when grouping internal tables.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <p>Find information on <strong>getting started with the example class</strong> and the
"! <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
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



CLASS ZCL_DEMO_ABAP_SQL_GROUP_BY IMPLEMENTATION.


  METHOD class_constructor.
    "Fill demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: Grouping Internal Tables\n\n| ).

    SELECT *
           FROM zdemo_abap_flsch
           INTO TABLE @DATA(fli_tab).

    out->write( |1) Representative Binding\n| ).
    out->write( |1a) Grouping by one column\n| ).

    LOOP AT fli_tab INTO wa
                      GROUP BY wa-carrid.
      out->write( wa-carrid ).
    ENDLOOP.

    out->write( zcl_demo_abap_aux=>heading( `1b) Members of one column groups` ) ).

    LOOP AT fli_tab INTO wa
                      GROUP BY wa-carrid.
      CLEAR members.
      LOOP AT GROUP wa INTO member.
        members = VALUE #( BASE members ( member ) ).
      ENDLOOP.

      out->write( members ).
      out->write( |\n| ).
    ENDLOOP.

    out->write( zcl_demo_abap_aux=>heading( `1c) Grouping by two columns` ) ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom ).

     out->write( |{ wa-carrid } { wa-airpfrom }| ).
     out->write( |\n| ).
    ENDLOOP.

    out->write( zcl_demo_abap_aux=>heading( `1d) Members of two column groups` ) ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom ).
      CLEAR members.
      LOOP AT GROUP wa INTO member.
        members = VALUE #( BASE members ( member ) ).
      ENDLOOP.

      out->write( members ).
      out->write( |\n| ).
    ENDLOOP.

    out->write( zcl_demo_abap_aux=>heading( `2) Group Key Binding` ) ).
    out->write( |2a) Grouping by one column\n| ).

    LOOP AT fli_tab INTO wa
                      GROUP BY wa-carrid
                      INTO DATA(key).

      out->write( key ).
      out->write( |\n| ).
    ENDLOOP.

    out->write( zcl_demo_abap_aux=>heading( `2b) Members of one column groups` ) ).

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

    out->write( zcl_demo_abap_aux=>heading( `2c) Grouping by two columns` ) ).

    LOOP AT fli_tab INTO wa
                      GROUP BY ( key1 = wa-carrid key2 = wa-airpfrom )
                      INTO DATA(keys).

      out->write( keys ).
      out->write( |\n| ).
    ENDLOOP.

    out->write( zcl_demo_abap_aux=>heading( `2d) Members of two column groups` ) ).

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

    out->write( zcl_demo_abap_aux=>heading( `2e) Two column groups without members` ) ).

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
