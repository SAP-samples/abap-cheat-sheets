"! <p class="shorttext"><strong>AMDP</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates AMDP procedures and functions.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li>The example covers the basics of AMDP method declarations and implementations.</li>
"! <li>The goal is to provide a general overview of AMDP, so the SQLScript code used in these
"! methods is quite simple. AMDP is usually not necessary for such simple cases.</li>
"! <li>This example is mainly intended for ABAP Cloud, where only read-only operations are
"! possible. In classic ABAP, there are more syntax options available. Refer to the ABAP
"! Keyword Documentation for more details and examples.</li>
"! <li>The example includes a CDS table function.</li>
"! <li>Find the following information in the ABAP Doc comment of class {@link zcl_demo_abap_aux}:
"! <ul><li>How to get started with the example class</li>
"! <li>Structuring of (most of) the example classes</li>
"! <li>Disclaimer</li></ul></li></ul>
CLASS zcl_demo_abap_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      if_oo_adt_classrun, "Interface for displaying output
      if_amdp_marker_hdb. "This interface specification is mandatory for an AMDP class

    METHODS:
      m01_amdp_procedure  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m02_call_amdp_proc_sqlscript  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m03_table_func_amdp_meth  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string,
      m04_table_func_cds_table_func  IMPORTING out TYPE REF TO if_oo_adt_classrun_out text TYPE string.

    "Various internal table type specifications for the parameters of AMDP methods
    "Note: Only table and elementary data types are possible for the parameters.
    TYPES carr_tab TYPE STANDARD TABLE OF zdemo_abap_carr_ve WITH EMPTY KEY.

    TYPES fli_tab TYPE STANDARD TABLE OF zdemo_abap_fli_ve WITH EMPTY KEY.

    TYPES:
      "Structured data type as basis for the table type below
      BEGIN OF carr_fli_struc,
        carrname TYPE zdemo_abap_carr_ve-carrname,
        connid   TYPE zdemo_abap_flsch_ve-connid,
        cityfrom TYPE zdemo_abap_flsch_ve-cityfrom,
        cityto   TYPE zdemo_abap_flsch_ve-cityto,
      END OF carr_fli_struc,

      "Internal table type
      carr_fli_tab TYPE STANDARD TABLE OF carr_fli_struc WITH EMPTY KEY,

      "Structured data type as basis for the table type below
      BEGIN OF fli_struc,
        carrid   TYPE zdemo_abap_flsch_ve-carrid,
        connid   TYPE zdemo_abap_flsch_ve-connid,
        cityfrom TYPE zdemo_abap_flsch_ve-cityfrom,
        cityto   TYPE zdemo_abap_flsch_ve-cityto,
        fltime   TYPE zdemo_abap_flsch_ve-fltime,
      END OF fli_struc,

      "Internal table type
      flsch_tab TYPE STANDARD TABLE OF zdemo_abap_flsch_ve WITH EMPTY KEY.

    "Various instance method declarations
    "The selection for instance and static methods is irrelevant for the example.
    "It is just meant to visualize that AMDP methods can be declared as either of them.

    "AMDP procedure
    "It's a simple AMDP procedure having only an output parameter with tabular type.
    "Note the parameter declaration that includes the mandatory passing by value.
    "This is true for all of the AMDP method declarations.
    METHODS select_carriers
      AMDP OPTIONS READ-ONLY CDS SESSION CLIENT DEPENDENT
      EXPORTING VALUE(carr_tab) TYPE carr_tab.

    "AMDP procedure to call an AMDP table function
    "As can be seen in the implementation part, this example method calls the
    "AMDP table function get_carr_fli. AMDP table functions can only be called
    "by other AMDP methods.
    METHODS select_get_carr_fli
      AMDP OPTIONS READ-ONLY CDS SESSION CLIENT DEPENDENT
      IMPORTING VALUE(carrid)       TYPE zdemo_abap_fli_ve-carrid
      EXPORTING VALUE(carr_fli_tab) TYPE carr_fli_tab.

    "Various static method declarations

    "The purpose of the implementation of the static constructor in this example is to
    "fill a demo database table to have data to work with in the example.
    CLASS-METHODS class_constructor.

    "AMDP procedure
    "This method demonstrates the calling of an AMDP procedure from SQLScript.
    "In this example, the selection of data is 'delegated' to another AMDP method get_flights_amdp
    "in the same AMDP class. The method declaration includes the addition RAISING with an
    "exception class for AMDP-specific exceptions.
    CLASS-METHODS  get_flights
      AMDP OPTIONS READ-ONLY CDS SESSION CLIENT DEPENDENT
      IMPORTING VALUE(carrid)  TYPE zdemo_abap_fli_ve-carrid
      EXPORTING VALUE(fli_tab) TYPE fli_tab
      RAISING   cx_amdp_execution_error.

    "AMDP Table Function for CDS Table Function
    "Note that, in this case, a static method declaration is required along with the special
    "syntax FOR TABLE FUNCTION. Plus, there are no parameters specified and the declaration
    "is made in the PUBLIC visibility section.
    CLASS-METHODS flight_analysis FOR TABLE FUNCTION zdemo_abap_table_function.

  PROTECTED SECTION.
  PRIVATE SECTION.

    "AMDP procedure
    "This method demonstrates the calling of an AMDP procedure from SQLScript as mentioned above.
    CLASS-METHODS get_flights_amdp
      AMDP OPTIONS READ-ONLY CDS SESSION CLIENT DEPENDENT
      IMPORTING VALUE(carrid)  TYPE zdemo_abap_fli_ve-carrid
      EXPORTING VALUE(fli_tab) TYPE fli_tab
      RAISING   cx_amdp_execution_error.

    "AMDP table function
    "AMDP table functions can only be called by other AMDP methods. In this example,
    "the AMDP procedure select_get_carr_fli calls this AMDP table function.
    METHODS get_carr_fli
      AMDP OPTIONS READ-ONLY CDS SESSION CLIENT DEPENDENT
      IMPORTING VALUE(carrid)       TYPE zdemo_abap_flsch_ve-carrid
      RETURNING VALUE(carr_fli_tab) TYPE carr_fli_tab.

    CONSTANTS nl TYPE string VALUE cl_abap_char_utilities=>newline.
ENDCLASS.



CLASS zcl_demo_abap_amdp IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_aux=>set_example_divider(
         out  = out
         text = `ABAP Cheat Sheet Example: AMDP`
       ).

    "Dynamically calling methods of the class
    "The method names are retrieved using RTTI. For more information, refer to the
    "Dynamic Programming ABAP cheat sheet.
    "Only those methods should be called that follow the naming convention M + digit.
    DATA(methods) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->methods.
    SORT methods BY name ASCENDING.

    "To call a particular method only, you can comment in the WHERE clause and
    "adapt the literal appropriately.
    LOOP AT methods INTO DATA(meth_wa)
    "WHERE name CS 'M01'
    .
      TRY.
          "The find function result indicates that the method name begins (offset = 0) with M and a digit.
          IF find( val = meth_wa-name pcre = `^M\d` case = abap_false ) = 0.
            CALL METHOD (meth_wa-name) EXPORTING out = out text = CONV string( meth_wa-name ).
          ENDIF.
        CATCH cx_root INTO DATA(error).
          out->write( error->get_text( ) ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD m01_amdp_procedure.
    zcl_demo_abap_aux=>set_example_divider(
         out  = out
         text = |{ text }: AMDP Procedure|
       ).

    "Declaring an internal table to store the data that are
    "returned by the following method.
    "You could also choose to create the internal table inline
    "within the method call,
    "i. e. like ... IMPORTING carr_tab = DATA(tab) ).
    DATA amdp_proc_res TYPE zcl_demo_abap_amdp=>carr_tab.

    "Since the method is declared as an instance method, an instance
    "has to be created. Here, the instance constructor NEW is used
    "in a standalone method call that includes a chained method call.
    NEW zcl_demo_abap_amdp( )->select_carriers(
      IMPORTING carr_tab = amdp_proc_res ).

    out->write( data = amdp_proc_res name = `amdp_proc_res` ).
  ENDMETHOD.

  METHOD m02_call_amdp_proc_sqlscript.
    zcl_demo_abap_aux=>set_example_divider(
         out  = out
         text = |{ text }: Calling an AMDP Procedure from SQLScript|
       ).

    "As can be seen in the method implementation part, this AMDP procedure
    "includes an AMDP procedure call from SQLScript.
    "In this example, the AMDP procedure get_flights_amdp is called by
    "get_flights which is meant to select data from a CDS view entity.
    "The returned result is displayed.
    TRY.
        zcl_demo_abap_amdp=>get_flights( EXPORTING carrid = 'LH'
                                         IMPORTING fli_tab = DATA(call_amdp_res) ).
      CATCH cx_amdp_execution_error INTO DATA(error1).
        out->write( error1->get_text( ) ).
    ENDTRY.

    out->write( data = call_amdp_res name = `call_amdp_res` ).
  ENDMETHOD.

  METHOD m03_table_func_amdp_meth.
    zcl_demo_abap_aux=>set_example_divider(
         out  = out
         text = |{ text }: AMDP Table Function for AMDP Method|
       ).

    "The AMDP procedure select_get_carr_fli calls the AMDP table function
    "get_carr_fli in the implementation part. AMDP table functions can
    "only be called by other AMDP methods.
    TRY.
        NEW zcl_demo_abap_amdp( )->select_get_carr_fli(
          EXPORTING carrid = 'LH'
          IMPORTING carr_fli_tab = DATA(amdp_tab_func) ).
      CATCH cx_amdp_execution_error INTO DATA(error2).
        out->write( error2->get_text( ) ).
    ENDTRY.

    out->write( data = amdp_tab_func name = `amdp_tab_func` ).

    "Note: When commented in, the following code results in a runtime
    "error since you cannot call an AMDP function in ABAP directly.
*        NEW zcl_demo_abap_amdp( )->get_carr_fli(
*          EXPORTING carrid = 'LH' ).
  ENDMETHOD.

  METHOD m04_table_func_cds_table_func.
    zcl_demo_abap_aux=>set_example_divider(
         out  = out
         text = |{ text }: AMDP Table Function for CDS Table Function|
       ).

    "The example demonstrates that a CDS table function can be used as a
    "data source of ABAP SQL read statements.
    "You might want to navigate to the DDL source after FROM by holding
    "CTRL and clicking the DDL source name in ADT to see the details.
    "Or, just check out the F2 help.
    "In this example, the CDS table function is implemented in a way to
    "return accumulated data.
    "In the method implementation for flight_analysis, first two kinds of
    "data sets from two CDS view entities are gathered. These data sets are
    "joined using an inner join. There, some expressions are included
    "(strings are aggregated, average values are determined).

    SELECT * FROM zdemo_abap_table_function
      INTO TABLE @DATA(cds_tab_func).

    out->write( data = cds_tab_func name = `cds_tab_func` ).
  ENDMETHOD.

  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.


  METHOD flight_analysis
         BY DATABASE FUNCTION
         FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY
         USING zdemo_abap_flsch_ve
               zdemo_abap_carr_ve.
* Reading data from two CDS view entities
    itab_cities =
     select DISTINCT
            zdemo_abap_flsch_ve.mandt    as client,
            zdemo_abap_flsch_ve.carrid   as carrier_id,
            zdemo_abap_flsch_ve.airpfrom as airport_from,
            zdemo_abap_flsch_ve.airpto   as airport_to,
            zdemo_abap_flsch_ve.fltime   as flight_time,
            zdemo_abap_flsch_ve.distance as flight_distance,
            zdemo_abap_flsch_ve.distid   as unit
       from zdemo_abap_flsch_ve;

    itab_carrier_names =
     select distinct
            zdemo_abap_carr_ve.mandt    as client,
            zdemo_abap_carr_ve.carrid   as carrier_id,
            zdemo_abap_carr_ve.carrname as carrier_name
       from zdemo_abap_carr_ve;

* Returning joined data using an inner join
   return
     select fl.client, fl.carrier_id, ca.carrier_name,
* Departure and destination airports are concatenated; then all results are joined by string aggregation
        string_agg( concat(concat(fl.airport_from,' -> '),fl.airport_to), ', ' ORDER BY fl.airport_from) AS connections,
* Retrieving the average flight time of all flights by carrier
        AVG( fl.flight_time ) as avg_flight_time,
* Retrieving the average flight distance of all flights by carrier; miles are converted to kilometers
        avg( case 'MI'
             when fl.unit then fl.flight_distance * 1.609
             ELSE fl.flight_distance
             end ) as avg_distance
       from :itab_cities as fl
       inner join :itab_carrier_names as ca
       on ca.client = fl.client
       and ca.carrier_id = fl.carrier_id
       where fl.client = ca.client and fl.carrier_id = ca.carrier_id
       group by fl.client, ca.carrier_name, fl.carrier_id;
  endmethod.


  METHOD get_carr_fli
         BY DATABASE FUNCTION
         FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY
         USING zdemo_abap_carr_ve zdemo_abap_flsch_ve.
* AMDP table function to be called by other AMDP methods only.
* In the example, joined data from two CDS view entities are returned.
    RETURN
      select ca.carrname, fl.connid, fl.cityfrom, fl.cityto
        from zdemo_abap_carr_ve as ca
        inner join zdemo_abap_flsch_ve as fl
        on ca.carrid = fl.carrid
        where fl.carrid = :carrid
        ORDER BY ca.mandt, ca.carrname, fl.connid;
  endmethod.


  METHOD get_flights
         BY DATABASE PROCEDURE
         FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY
         USING zcl_demo_abap_amdp=>get_flights_amdp.
* Another AMDP procedure is called from SQLScript
    CALL "ZCL_DEMO_ABAP_AMDP=>GET_FLIGHTS_AMDP"(
      carrid => :carrid,
      fli_tab => :fli_tab );
  ENDMETHOD.


  METHOD get_flights_amdp
         BY DATABASE PROCEDURE
         FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY
         USING zdemo_abap_fli_ve.
* Simple data selection
    fli_tab = SELECT carrid, connid, fldate, price, currency, planetype,
                     seatsmax, seatsocc, paymentsum, seatsmax_b, seatsocc_b,
                     seatsmax_f, seatsocc_f
                FROM "ZDEMO_ABAP_FLI_VE"
                WHERE carrid = :carrid
                ORDER BY carrid;
  ENDMETHOD.

  METHOD select_carriers
         BY DATABASE PROCEDURE
         FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY
         USING zdemo_abap_carr_ve.
* Simple data selection
    carr_tab = SELECT carrid, carrname, currcode, url
                FROM "ZDEMO_ABAP_CARR_VE"
                ORDER BY carrid;
  ENDMETHOD.


  METHOD select_get_carr_fli
         BY DATABASE PROCEDURE
         FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY
         USING zcl_demo_abap_amdp=>get_carr_fli.
* AMDP procedure to call an AMDP table function as specified after USING
    carr_fli_tab = SELECT *
                     FROM "ZCL_DEMO_ABAP_AMDP=>GET_CARR_FLI"(
                        carrid => :carrid );
  ENDMETHOD.


ENDCLASS.
