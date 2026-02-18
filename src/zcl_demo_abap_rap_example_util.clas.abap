"! <p class="shorttext"><strong>Utility class for ABAP cheat sheets examples</strong></p>
"!
"! <p>This class prepares the ABAP cheat sheet examples. The preparation steps include:</p>
"! <ul><li>Clearing database tables</li>
"! <li>Setting number ranges for demo RAP BOs that use late numbering and internal
"! early unmanaged numbering</li></ul>
"!
"! <p>Before checking out the demo RAP BOs with the preview SAP Fiori UI, make sure to run
"! this class by pressing F9 in ADT.</p>
CLASS zcl_demo_abap_rap_example_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS clear_tables IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    CLASS-METHODS setup_number_range IMPORTING out TYPE REF TO if_oo_adt_classrun_out
                                               nro TYPE cl_numberrange_intervals=>nr_object.
    CONSTANTS number_range_object99 TYPE cl_numberrange_intervals=>nr_object VALUE 'ZNR_DEMO99'.
    CONSTANTS number_range_object95 TYPE cl_numberrange_intervals=>nr_object VALUE 'ZNR_DEMO95'.
    CONSTANTS fromnr TYPE cl_numberrange_intervals=>nr_nriv_line-fromnumber VALUE '000000000000001'.
    CONSTANTS tonr TYPE cl_numberrange_intervals=>nr_nriv_line-tonumber VALUE '000009999999999'.
    CONSTANTS nrranr TYPE cl_numberrange_intervals=>nr_interval_int VALUE '01'.
    CONSTANTS nrrangenr TYPE cl_numberrange_intervals=>nr_interval_int VALUE '01'.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_rap_example_util IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    out->write( |-------------- Clearing database tables --------------| ).
    clear_tables( out ).
    out->write( |\n| ).
    out->write( |{ repeat( val = `*` occ = 100 ) }| ).
    out->write( |\n| ).

    out->write( |-------------- Setting up number ranges --------------| ).
    DO 2 TIMES.
      setup_number_range( out = out nro = SWITCH #( sy-index WHEN 1 THEN number_range_object99
                                                             WHEN 2 THEN number_range_object95 ) ).
      out->write( |\n| ).
      out->write( |{ repeat( val = `*` occ = 100 ) }| ).
      out->write( |\n| ).
    ENDDO.
  ENDMETHOD.

  METHOD clear_tables.
*&---------------------------------------------------------------------*
*& The purpose is to clear all involved demo database tables, for
*& example, if you want to start the examples anew.
*&---------------------------------------------------------------------*

    DELETE FROM zdemoabap99.
    DELETE FROM zdemoabap_d99.

    DELETE FROM zdemoabap98.
    DELETE FROM zdemoabap_d98.

    DELETE FROM zdemoabap97.

    DELETE FROM zdemoabap96.
    DELETE FROM zdemoabaplog96.

    DELETE FROM zrootentity95.
    DELETE FROM zchildentity95.

    out->write( |\n| ).
    out->write( `Database tables were cleared.` ).
  ENDMETHOD.

  METHOD setup_number_range.

*&---------------------------------------------------------------------*
*& The example implementation is relevant for those example BOs that
*& implement late numbering and unmanaged internal early numbering
*& (early_numbering_create method) and use the cl_numberrange_intervals
*& class for assigning key values (instead of the dummy implementation).
*&
*& The example implementation only uses a predefined interval. The
*& implementation initializes the number range interval if you run
*& the class the first time and if you want to start anew.
*& Note: Assume you have run the class once and created instances via
*& the preview UI, and, for example, a root entity instance with the
*& ID 1 exists, and you want to start anew and the update is
*& performed in this method without also clearing the database table.
*& A conflict will occur regarding the numbering (which starts at 1
*& again) and unique IDs in the database table.
*&---------------------------------------------------------------------*

    out->write( |\n| ).
    out->write( |--- Number range object { nro } ---| ).
    out->write( |\n| ).

    TRY.
        cl_numberrange_intervals=>read(
          EXPORTING
            object       = nro
          IMPORTING
            interval     = DATA(intervals) ).

        IF intervals IS INITIAL.
          out->write( `Interval table is initial.` ).
        ELSE.
          out->write( `Interval table is not initial.` ).
          out->write( intervals ).
          out->write( |\n| ).

          READ TABLE intervals ASSIGNING FIELD-SYMBOL(<intval>) WITH KEY nrrangenr = nrranr.
          IF sy-subrc = 0.
            out->write( |Number range number { nrranr } already maintained.| ).
          ELSE.
            out->write( |Number range number { nrranr } is not maintained.| ).
          ENDIF.
        ENDIF.
        out->write( |\n| ).

        "In case the level of the demo interval is not 0, setting it to 0, update and remove it.
        "Removing other intervals
        LOOP AT intervals INTO DATA(interval).
          IF interval-nrrangenr <> nrranr.
            "In case the level is not 0, setting it to 0, updating and removing it.
            IF interval-nrlevel <> 0.
              interval-nrlevel = 0.
              interval-procind = 'U'.
              cl_numberrange_intervals=>update(
                EXPORTING
                  interval  = VALUE #( ( interval ) )
                  object    = nro ).
            ENDIF.
            interval-procind = 'D'.
            cl_numberrange_intervals=>delete(
              EXPORTING
                interval  = VALUE #( ( interval ) )
                object    = nro ).
          ENDIF.
        ENDLOOP.

        "Resetting or creating the default demo interval
        IF <intval> IS ASSIGNED.
          "Update, i.e. reset if the numbering level is not 0
          IF <intval>-nrlevel <> 0.
            <intval>-fromnumber = fromnr.
            <intval>-tonumber = tonr.
            <intval>-nrlevel = 0.
            <intval>-procind = 'U'.

            cl_numberrange_intervals=>update(
              EXPORTING
                interval  = VALUE #( ( <intval> ) )
                object    = nro ).
          ENDIF.
        ELSE.

          "Creating interval
          cl_numberrange_intervals=>create(
             EXPORTING
               interval  = VALUE #( ( nrrangenr = nrranr
                                      fromnumber = fromnr
                                      tonumber = tonr
                                      procind    = 'I' ) )
               object    = nro
         IMPORTING error     = DATA(error)
                   error_inf = DATA(error_inf)
                   error_iv  = DATA(error_iv)
                   warning   = DATA(warning) ).

          IF error IS NOT INITIAL OR error_inf IS NOT INITIAL OR error_iv IS NOT INITIAL OR warning IS NOT INITIAL.
            out->write( `Errors/warnings:` ).
            out->write( data = error name = `error` ).
            out->write( data = error_inf name = `error_inf` ).
            out->write( data = error_iv name = `error_iv` ).
            out->write( data = warning name = `warning` ).
            out->write( |\n| ).
          ENDIF.
        ENDIF.

        COMMIT WORK.

        out->write( |\n| ).
        out->write( |Interval table { cond #( when <intval> is assigned then `reset` else `created` ) }:| ).
        cl_numberrange_intervals=>read(
          EXPORTING
            nr_range_nr1 = nrranr
            object       = nro
          IMPORTING
            interval     = intervals ).

        out->write( intervals ).
      CATCH cx_nr_object_not_found cx_nr_subobject cx_number_ranges INTO DATA(err).
        out->write( err->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
