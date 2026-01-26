"! <p class="shorttext"><strong>Data Access Object (DAO)</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates a Data Access Object (DAO).<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_dao DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS prep_dbtab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA example_number TYPE i.
ENDCLASS.


CLASS zcl_demo_abap_oodp_dao IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Data Access Object (DAO)` ).

    "Preparing the demo database table
    prep_dbtab( ).

    "Getting DAO
    DATA(dao) = lcl_dao=>get_dao( ).

*&---------------------------------------------------------------------*
*& Counting entries
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Counting entries| ).

    DATA(count) = dao->count_entries( ).

    out->write( |Number of database table entries: { count }| ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Getting entries
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Getting entries| ).

    "Getting all entries
    DATA(entries) = dao->get_all_entries( ).
    out->write( `All database table entries:` ).
    out->write( entries ).
    out->write( |\n| ).

    "Getting single entries
    "Non-existent entry
    DATA(entry) = dao->get_entry( 'ZZ' ).
    IF entry IS INITIAL.
      out->write( `The returned data object is initial.` ).
    ELSE.
      out->write( entry ).
    ENDIF.
    out->write( |\n| ).

    "Using the does_entry_exist method to check the existence
    DATA(exists) = dao->does_entry_exist( 'ZZ' ).
    out->write( |The entry with ID "ZZ" { COND #( WHEN exists = abap_true THEN `exists` ELSE `does not exist` ) }.| ).
    out->write( |\n| ).
    out->write( |\n| ).

    "Existent single entries
    entry = dao->get_entry( 'AB' ).
    out->write( entry ).
    out->write( |\n| ).

    entry = dao->get_entry( 'EF' ).
    out->write( entry ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Creating entries
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Creating entries| ).

    "Creating single entries
    DATA(carr_data) = VALUE lif_dao=>ts_carr( carrid = 'MN' carrname = 'MN Regional'  currcode = 'CHF' url = 'url_for_MN' ).

    DATA(done) = dao->create_entry( carr_data ).

    out->write( |Insert operation { COND #( WHEN done = abap_true THEN `done` ELSE `not done` ) }.| ).
    out->write( |\n| ).

    exists = dao->does_entry_exist( 'MN' ).
    out->write( |The entry with ID "MN" { COND #( WHEN exists = abap_true THEN `exists` ELSE `does not exist` ) }.| ).
    out->write( |\n| ).

    "Creating single entry with existing ID
    done = dao->create_entry( carr_data ).

    out->write( |Insert operation { COND #( WHEN done = abap_true THEN `done` ELSE `not done` ) }.| ).
    out->write( |\n| ).

    exists = dao->does_entry_exist( 'MN' ).
    out->write( |The entry with ID "MN" { COND #( WHEN exists = abap_true THEN `exists` ELSE `does not exist` ) }.| ).
    out->write( |\n| ).

    "Creating multiple entries
    DATA(carr_tab) = VALUE lif_dao=>tt_carr( ( carrid = 'OP' carrname = 'OP Cargo' currcode = 'CNY' url = 'url_for_OP' )
                                             ( carrid = 'QR' carrname = 'QR International' currcode = 'INR' url = 'url_for_QR' ) ).

    done = dao->create_entries( carr_tab ).

    out->write( |Insert operation { COND #( WHEN done = abap_true THEN `done` ELSE `not or partly not done` ) }.| ).
    out->write( |\n| ).

    "Example input with one already existing entry
    carr_tab = VALUE lif_dao=>tt_carr( ( carrid = 'OP' carrname = 'OP Cargo' currcode = 'CNY' url = 'url_for_OP' )
                                       ( carrid = 'ST' carrname = 'ST Air Service' currcode = 'BRL' url = 'url_for_ST' ) ).

    done = dao->create_entries( carr_tab ).

    out->write( |Insert operation { COND #( WHEN done = abap_true THEN `done` ELSE `not or partly not done` ) }.| ).
    out->write( |\n| ).

    "Getting all database table entries
    entries = dao->get_all_entries( ).
    out->write( `All database table entries:` ).
    out->write( entries ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Upserting entries
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Upserting entries| ).

    out->write( `--- Operation with ID "MN" ---` ).
    out->write( |\n| ).

    exists = dao->does_entry_exist( 'MN' ).
    out->write( |The entry with ID "MN" { COND #( WHEN exists = abap_true THEN `exists` ELSE `does not exist` ) }.| ).
    out->write( |\n| ).
    out->write( |\n| ).
    IF exists = abap_true.
      entry = dao->get_entry( 'MN' ).
      out->write( entry ).
      out->write( |\n| ).
    ENDIF.

    carr_data = VALUE lif_dao=>ts_carr( carrid = 'MN' carrname = 'Air MN' currcode = 'AED' url = 'url_for_MN' ).

    DATA(operation) = dao->upsert_entry( carr_data ).
    out->write( |Entry was { COND #( WHEN operation = lif_dao=>update THEN `updated` ELSE `inserted` ) }.| ).
    out->write( |\n| ).
    out->write( |\n| ).

    entry = dao->get_entry( 'MN' ).
    out->write( entry ).
    out->write( |\n| ).

    out->write( `--- Operation with ID "YZ" ---` ).
    out->write( |\n| ).

    exists = dao->does_entry_exist( 'YZ' ).
    out->write( |The entry with ID "YZ" { COND #( WHEN exists = abap_true THEN `exists` ELSE `does not exist` ) }.| ).
    out->write( |\n| ).
    IF exists = abap_true.
      entry = dao->get_entry( 'YZ' ).
      out->write( entry ).
      out->write( |\n| ).
    ENDIF.

    carr_data = VALUE lif_dao=>ts_carr( carrid = 'YZ' carrname = 'YZ Air Lines' currcode = 'MXN' url = 'url_for_YZ' ).

    operation = dao->upsert_entry( carr_data ).
    out->write( |Entry was { COND #( WHEN operation = lif_dao=>update THEN `updated` ELSE `inserted` ) }.| ).
    out->write( |\n| ).
    out->write( |\n| ).

    entry = dao->get_entry( 'YZ' ).
    out->write( entry ).

    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Deleting entries
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Deleting entries| ).

    "Non-existent entry
    done = dao->delete_entry( 'XY' ).

    out->write( |Delete operation { COND #( WHEN done = abap_true THEN `done` ELSE `not done` ) } for ID "XY".| ).
    out->write( |\n| ).

    done = dao->delete_entry( 'YZ' ).

    out->write( |Delete operation { COND #( WHEN done = abap_true THEN `done` ELSE `not done` ) } for ID "YZ".| ).
    out->write( |\n| ).

    done = dao->delete_entry( 'AB' ).

    out->write( |Delete operation { COND #( WHEN done = abap_true THEN `done` ELSE `not done` ) } for ID "AB".| ).
    out->write( |\n| ).

    "Getting count
    count = dao->count_entries( ).

    out->write( |Number of database table entries: { count }| ).
    out->write( |\n| ).

    "Getting all entries
    entries = dao->get_all_entries( ).
    out->write( `All database table entries:` ).
    out->write( entries ).
  ENDMETHOD.

  METHOD prep_dbtab.
    DELETE FROM zdemoabapcarrier.

    INSERT zdemoabapcarrier FROM TABLE @( VALUE #(
      ( carrid = 'AB' carrname = 'AB Airways'     currcode = 'USD' url = 'url_for_AB' )
      ( carrid = 'CD' carrname = 'CD Air'         currcode = 'EUR' url = 'url_for_CD' )
      ( carrid = 'EF' carrname = 'EF Aviation'    currcode = 'GBP' url = 'url_for_EF' )
      ( carrid = 'GH' carrname = 'GH Airlines'    currcode = 'JPY' url = 'url_for_GH' )
      ( carrid = 'IJ' carrname = 'IJ Skyways'     currcode = 'AUD' url = 'url_for_IJ' )
      ( carrid = 'KL' carrname = 'KL Air Express' currcode = 'ZAR' url = 'url_for_KL' ) ) ).
  ENDMETHOD.
ENDCLASS.
