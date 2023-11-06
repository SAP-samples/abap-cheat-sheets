***********************************************************************
*
*                     ABAP cheat sheet: CDS View Entities
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate CDS view entities. See the CDS view entities
*   that are used in the example for more details.
* - Topics covered: Operands, expressions, and built-in functions in the
*   element list of CDS view entities, input parameters, joins,
*   associations
* - Note: In ADT, check out the CDS view entities used in this example
*   by holding down CTRL and clicking on the CDS view entity. This will
*   take you to the artifact. There you can choose F8 to open the data
*   preview.

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
"! <p class="shorttext synchronized">ABAP cheat sheet: CDS view entities</p>
"! Example to demonstrate CDS view entities.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_cds_ve DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      if_oo_adt_classrun.

    CLASS-METHODS class_constructor.
protected section.
private section.
ENDCLASS.



CLASS ZCL_DEMO_ABAP_CDS_VE IMPLEMENTATION.


  METHOD class_constructor.
    "Filling demo database tables.
     zcl_demo_abap_aux=>fill_dbtabs( ).

    "Some more database table insertions for this particular example
    MODIFY zdemo_abap_carr FROM TABLE @( VALUE #(
        ( carrid = 'SQ'
          carrname = 'Singapore Airlines'
          currcode = 'SGD'
          url =  'http://www.singaporeair.com ' )
        ( carrid = 'QF'
          carrname = 'Qantas Airways'
          currcode = 'AUD'
          url =  'http://www.qantas.com.au' ) ) ).

    MODIFY zdemo_abap_flsch FROM TABLE @( VALUE #(
      ( carrid = 'UA'
        connid =  3517
        countryfr =  'DE'
        cityfrom =  'FRANKFURT'
        airpfrom = 'FRA'
        countryto = 'US'
        cityto = 'NEW YORK'
        airpto = 'JFK'
        fltime = 495
        deptime = '104000'
        arrtime = '125500'
        distance =  6162
        distid = 'KM'
        fltype = ''
        period = 0 ) ) ).

    MODIFY zdemo_abap_fli FROM TABLE @( VALUE #( ( carrid = 'UA' ) ) ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: CDS view entities\n\n| ).

    out->write( `1) Operands, expressions and built-in functions ` &&
    |in a CDS view entity\n\n|  ).

    "The following ABAP SQL SELECT statement uses a CDS view entity as
    "the data source. All data is retrieved. The sample CDS view entity
    "uses many operands, expressions and built-in functions to demonstrate
    "various syntax options. In addition, the view contains an input
    "parameter that must be provided with an actual parameter and
    "specified in the ABAP SQL SELECT statement.
    "In ADT, check out the CDS view entity by holding down CTRL and clicking
    "on the CDS view entity. This will take you to the artifact. There you
    "can choose F8 to open the data preview.

    SELECT *
      FROM zdemo_abap_cds_ve_sel( p_smax = 20 )
      ORDER BY CarrierId
      INTO TABLE @DATA(select_from_cds).

    out->write( data = select_from_cds name = `select_from_cds` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Aggregate Expressions` ) ).

    "The following ABAP SQL SELECT statement uses a CDS view entity as
    "the data source. All data is retrieved. The sample CDS view entity
    "uses aggregate expressions.

    SELECT *
      FROM zdemo_abap_cds_ve_agg_exp
      ORDER BY carrid
      INTO TABLE @DATA(agg_expr).

    out->write( data = agg_expr name = `agg_expr` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Joins` ) ).

    "The following ABAP SQL SELECT statement uses a CDS view entity as
    "the data source. All data is retrieved. The sample CDS view entity
    "contains multiple joins.
    "The CDS view entity is designed to contain different join variants
    "in one artifact. There, you can comment in/out code sections to
    "check out the individual join variants. Therefore, the result of
    "the following SELECT statement depends on which section you have
    "commented in in the CDS view entity.

    SELECT *
      FROM zdemo_abap_cds_ve_joins
      ORDER BY carrid
      INTO TABLE @DATA(cds_joins).

    out->write( data = cds_joins name = `cds_joins` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Excursion: ABAP SQL and joins` ) ).

    "The following ABAP SQL SELECT statements are intended to reproduce
    "the different joins that are performed by the CDS view entity.
    "Inner, left and right outer, and cross joins are covered. The data
    "sources for the SELECT statements are the database tables.
    "Note:
    "- The prefix ~ is used in the ABAP SQL statements instead of . in
    "  the CDS view entity.
    "- To demonstrate the handling of null values, some SELECT statements
    "  contain the coalesce function and CASE expressions similar to the
    "  CDS view entity.

    out->write( `---------- Inner join ----------` ).
    out->write( |\n| ).
    out->write( |\n| ).

    SELECT _carr~carrid,
           _carr~carrname,
           _flsch_in~cityfrom AS cityfr_in,
           _flsch_in~cityto AS cityto_in
      FROM zdemo_abap_carr AS _carr
      INNER JOIN zdemo_abap_flsch AS _flsch_in
      ON _carr~carrid = _flsch_in~carrid
      ORDER BY _carr~carrid
      INTO TABLE @DATA(sql_inner_join).

    out->write( data = sql_inner_join name = `sql_inner_join` ).

    out->write( |\n| ).
    out->write( `---------- Left outer join ----------` ).
    out->write( |\n| ).
    out->write( |\n| ).

    SELECT _carr~carrid,
           _carr~carrname,
           _flsch_lo~cityfrom AS cityfr_lo,
           coalesce( _flsch_lo~cityto, '???' ) AS cityto_lo
      FROM zdemo_abap_carr AS _carr
      LEFT OUTER JOIN zdemo_abap_flsch AS _flsch_lo
      ON _carr~carrid = _flsch_lo~carrid
      ORDER BY _carr~carrid
      INTO TABLE @DATA(sql_left_outer_join).

    out->write( data = sql_left_outer_join name = `sql_left_outer_join` ).

    out->write( |\n| ).
    out->write( `---------- Right outer join ----------` ).
    out->write( |\n| ).
    out->write( |\n| ).

    SELECT _carr~carrid,
           _carr~carrname,
           CASE WHEN _carr~url IS NOT NULL THEN _carr~url
                ELSE '!!!'
           END AS url_ro,
           _flsch_ro~cityfrom AS cityfr_ro,
           _flsch_ro~cityto AS cityto_ro
      FROM zdemo_abap_carr AS _carr
      RIGHT OUTER JOIN zdemo_abap_flsch AS _flsch_ro
      ON _carr~carrid = _flsch_ro~carrid
      ORDER BY _carr~carrid
      INTO TABLE @DATA(sql_right_outer_join).

    out->write( data = sql_right_outer_join name = `sql_right_outer_join` ).

    out->write( |\n| ).
    out->write( `---------- Cross join ----------` ).
    out->write( |\n| ).
    out->write( |\n| ).

    SELECT _carr~carrid,
           _carr~carrname,
           _flsch_cr~cityfrom AS cityfr_cr,
           _flsch_cr~cityto AS cityto_cr
      FROM zdemo_abap_carr AS _carr
      CROSS JOIN zdemo_abap_flsch AS _flsch_cr
      ORDER BY _carr~carrid
      INTO TABLE @DATA(sql_cross_join).

    out->write( data = sql_cross_join name = `sql_cross_join` ).
    out->write( |\n| ).

    "Just a check what join example is currently commented in
    IF cds_joins = sql_inner_join.

      out->write( `In the example CDS view entity, the inner join example is commented in.` ).

    ELSEIF cds_joins = sql_left_outer_join.

      out->write( `In the example CDS view entity, the left outer join example is commented in.` ).

    ELSEIF cds_joins = sql_right_outer_join.

      out->write( `In the example CDS view entity, the right outer join example is commented in.` ).

    ELSEIF cds_joins = sql_cross_join.

      out->write( `In the example CDS view entity, the cross join example is commented in.` ).

    ELSE.

      out->write( `In the example CDS view entity, there is some other code present.` ).

    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Associations` ) ).

    out->write( |5) Selecting data from a CDS view that contains associations\n\n|  ).

    "The following ABAP SQL SELECT statement uses a CDS view entity as
    "the data source. All data is retrieved. The sample CDS view entity
    "contains multiple associations.
    "Some fields of some associations are used in the element list of the
    "CDS view entity. This data is included in the result set. Some
    "associations are exposed but no fields of those associations are
    "included in the element list. Therefore, no join is instantiated on
    "the database and no data from these exposed assocations is included
    "in the result set.

    SELECT *
      FROM zdemo_abap_cds_ve_assoc
      ORDER BY carrier
      INTO TABLE @DATA(assoc).

    out->write( data = assoc name = `assoc` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `Using exposed associations in ABAP SQL statements: ...` ) ).

    "The following examples use path expressions to access the association
    "targets of exposed associations.

    out->write( |6) ... SELECT clause\n\n|  ).

    "The following ABAP SQL SELECT statement uses a CDS view entity as
    "the data source. The statement uses an exposed association.
    "The SELECT list contains fields from the exposed association. Only in
    "this case (when a consumer, such as an ABAP SQL statement, requests
    "data) is the join instantiated on the database.
    "Note:
    "- No attributes are specified for the path expression (attributes
    "  are covered in examples further down). In particular, a join type is
    "  not explicitly specified. In such a case, the join type depends on
    "  the place where the path expression is used. Since the path expression
    "  is used in the SELECT list of an ABAP SQL SELECT statement (where
    "  fields are specified), a LEFT OUTER JOIN is used by default.
    "- The coalesce function is included for a field to handle null values.

    SELECT carrier,
           \_carr3-carrname,
           coalesce( \_carr3-url, '###' ) AS cityto_lo
      FROM zdemo_abap_cds_ve_assoc
      ORDER BY carrier
      INTO TABLE @DATA(assoc_exp_select).

    out->write( data = assoc_exp_select name = `assoc_exp_select` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) ... FROM clause` ) ).

    "The following ABAP SQL SELECT statement uses a CDS view entity as
    "the data source. All data is retrieved.
    "In this case, an exposed association is used in the FROM clause.
    "Note:
    "- No join type is explicitly specified. As mentioned above, the
    "  position of the path expression is important. In this case (when
    "  used in the FROM clause), an INNER JOIN is used by default.
    "- You can open the data preview for the CDS view entity used and
    "  compare the result set with the output here. Due to the inner
    "  join, non-existent 'carrid' values in the association target are
    "  not contained in the result set.

    SELECT *
      FROM zdemo_abap_cds_ve_assoc\_carr3 AS _exp
      ORDER BY carrid
      INTO TABLE @DATA(assoc_exp_from).

    out->write( data = assoc_exp_from name = `assoc_exp_from` ).
    out->write( |\n| ).

    "The following ABAP SQL SELECT statement is intended to reproduce
    "the data retrieval as above.
    "The statement uses the same CDS view entity as data source that
    "is used by the CDS view entity above as data source.
    "An inner join is performed on a database table (the _carr3
    "association from above has this table defined as the association
    "target in the CDS view entity) since the inner join is used by
    "default above so as to reproduce the effect. The result set
    "should be the same as above.

    SELECT _carr~mandt,
           _carr~carrid,
           _carr~carrname,
           _carr~currcode,
           _carr~url
      FROM zdemo_abap_cds_ve_assoc_e AS _cds
      JOIN  zdemo_abap_carr AS _carr ON _cds~carrid = _carr~carrid
      ORDER BY _carr~carrid
      INTO TABLE @DATA(sql_repr).

    out->write( data = sql_repr name = `sql_repr` ).
    out->write( |\n| ).

    IF sql_repr = assoc_exp_from.
      out->write( `The result sets are the same.` ).
    ELSE.
      out->write( `The result sets are differrent.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) ... Specifying attributes` ) ).

    "The following ABAP SQL SELECT statement uses a CDS view entity as
    "the data source. The statement uses an exposed association.
    "The SELECT list contains a path expression that is specified with
    "an attribute.
    "Note:
    "- Cardinality, join types, and filter conditions can be used as
    "  attributes.
    "- In the example, only the cardinality is specified.
    "- The cardinality can be specified to prevent syntax warnings/errors
    "  in cases where the cardinality of the association does not match
    "  the way it is used in a path expression.
    "- The example does not explicitly specify a join type. As mentioned
    "  above, if not explicitly specified, the join type depends on where
    "  the path expression is used. Here, a column is specified in the
    "  SELECT list. This means that a LEFT OUTER JOIN is used by default.
    "- The result set should contain an entry for 'UA' having an initial
    "  value for 'fldate'.

    SELECT carrid,
           connid,
           cityfrom,
           cityto,
          "Without specifying the cardinality, the following warning
          "occurs: Using association "_FLI" can increase the cardinality
          "of the results set
          "\_fli-fldate AS flightdate

          \_fli[ (*) ]-fldate AS flightdate
          "The specification above corresponds to the following specification
          "that includes an explicit specification of LEFT OUTER
          "\_fli[ (*) LEFT OUTER ]-fldate AS flightdate
       FROM zdemo_abap_cds_ve_assoc_e
       ORDER BY carrid, connid, flightdate
       INTO TABLE @DATA(assoc_attr_card).

    out->write( data = assoc_attr_card name = `assoc_attr_card` ).
    out->write( |\n| ).

    "Specifying the join type explicitly
    "- INNER, LEFT/RIGHT OUTER are possible
    "- The join type can only be specified together with the cardinality.
    "- In the result set of the example, the 'UA' entry should not be
    "  contained.

    SELECT carrid,
           connid,
           cityfrom,
           cityto,
           \_fli[ (*) INNER ]-fldate AS flightdate
      FROM zdemo_abap_cds_ve_assoc_e
      ORDER BY carrid, connid, flightdate
      INTO TABLE @DATA(assoc_attr_joty).

    out->write( data = assoc_attr_joty name = `assoc_attr_joty` ).
    out->write( |\n| ).

    "Specifying conditions
    "- Filter conditions can be specified for the current association
    "- The addition WHERE is optional in cases where the filter condition
    "  is the only attribute specified in the square brackets.
    "- When the association is instantiated as a join, the filter condition
    "  is transformed into an extended condition for the join.
    "- In the example, a specific 'carrid' value is filtered for. LEFT OUTER
    "  is specified as join type explicitly. Not specifying the join type here
    "  has the same effect. The 'fldate' value is only retrieved for 'DL'
    "  entries. The other ones have initial values.

    SELECT carrid,
           connid,
           cityfrom,
           cityto,
           \_fli[ (*) LEFT OUTER WHERE carrid = 'DL' ]-fldate AS flightdate
           "The following has the same effect in this example
           "\_fli[ (*) WHERE carrid = 'DL' ]-fldate as flightdate
      FROM zdemo_abap_cds_ve_assoc_e
      ORDER BY carrid, connid, flightdate
      INTO TABLE @DATA(assoc_attr_where).

    out->write( data = assoc_attr_where name = `assoc_attr_where` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `9) ... WHERE clause` ) ).

    "The following ABAP SQL SELECT statement uses a CDS view entity as
    "the data source. The statement uses an exposed association.
    "The SELECT list and the WHERE clause contain a path expression.

    SELECT carrid,
           connid,
           countryfr,
           countryto,
           \_carr_exp-carrname
      FROM zdemo_abap_cds_ve_assoc_e
      WHERE \_carr_exp-carrid LIKE 'A_'
      ORDER BY carrid, connid
      INTO TABLE @DATA(assoc_exp_where).

    out->write( data = assoc_exp_where name = `assoc_exp_where` ).

  ENDMETHOD.
ENDCLASS.
