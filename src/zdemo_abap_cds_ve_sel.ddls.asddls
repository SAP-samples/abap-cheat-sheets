// ABAP CDS cheat sheet example:
// Operands and expressions in CDS view entities
//
//////////////////////////------ NOTES ------//////////////////////////////////
// - CDS view entity selects from a demo database table
// - Demonstrates various syntax options regarding operands and expressions 
// - As a prerequisite, run the class zcl_demo_abap_cds_ve to populate the
//   database tables of the example. Otherwise, no data is displayed.
//
//////////////////////------ DATA PREVIEW ------///////////////////////////////
// - Choose F8 in ADT to open the data preview and check out the data displayed
// - The example includes parameters. Therefore, you are prompted to insert a
//   value. In this example, the parameter (maximum seats in a plane) is used for
//   the WHERE clause (the lower the number entered, the more entries in the result
//   set).
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////------ Annotations ------///////////////////////////////
// Annotations add metadata to a CDS object that expands the syntax options of SQL.
// There's a predefined set of SAP annotations. Their specification is optional.

// Example for a view entity annotation (only possible in CDS view entities)
// The following annotation defines implicit access control when ABAP SQL is used
// to access the CDS view entity. Here, it is determined that no access control
// is required.
@AccessControl.authorizationCheck: #NOT_REQUIRED

// Example for an entity annotation (annotations that can be used in all CDS entities)
// The following annotation provides a translatable short text of the CDS entity.
@EndUserText.label: 'CDS view entity: Operands/Expressions'
define view entity zdemo_abap_cds_ve_sel
  with parameters
    p_smax : abap.int4 //Input parameter typed with an elementary data type;
                       //can also be a DDIC data element

  as select from zdemo_abap_fli //Selection from a demo database table;
                                //an alias name can be also specified ... as _fli, for example

  // The following element list demonstrates possible elements, operands and expressions.
  // Note: Many of the operands and expressions demonstrated below can occur in multiple positions.
  //       Refer to the ABAP Keyword Documentation for the details.
{
      // -------- Specifying fields of the data source --------
      //- Multiple fields of the data source from which to be selected are specified.
      //- Field names can be prefixed with the name of the data source (or, if specified, with the alias name).
      //- Alias names can be specified for the elements. Note: In case of joins (selection from multiple sources)
      //  all elements must be prefixed with the name of the data source. See the view entity demonstrating joins.
      //- KEY defines the current element as a key element; must be placed at the beginning; in CDS entities the key
      //  elements are mainly used to document the semantics of the data model (Note: They do not define unique lines
      //  in its result with regard to the key.)

  key carrid                as CarrierId, //Alias name specified
  key zdemo_abap_fli.connid as Connid, //Element prefixed with data source; alias name specified
  key fldate,
      price,
      currency,
      paymentsum,
      planetype,
      seatsmax,
      seatsocc,

      // -------- Literals --------
      // Typed literals (cover most built-in types of the ABAP Dictionary)
      // As is true for all of the following elements, an alias name must be specified.
      abap.int4'12345'    as int4, //Typed numeric literal
      abap.char'hallo'    as c5, //Typed character literal
      abap.dats'20240101' as date_lit, //Date

      // Note: In case of a currency or quantity field specified with a typed literal, a reference
      // to a currency key or unit is mandatory, which requires an annotation. The currency example
      // here uses the following annotation. The element 'currency' from above is referenced.
      @Semantics.amount.currencyCode: 'currency'
      abap.curr'12.34' as curr,
      abap.dec'0.9'    as discnt,
      abap.unit'KM'    as kilometers,

      // Untyped literals
      'Minutes' as fltime_ut, //Untyped character literal
      1         as num_lit, //Untyped numeric literal

      // -------- Parameters --------
      // Parameters can be specified in an operand position.
      // The name of the parameter must be prefixed by $parameters.
      $parameters.p_smax as param,

      // -------- Session variables --------
      // Among other sesion variables, there are built-in session variables available.
      $session.user            as usr,
      $session.client          as clnt,
      $session.system_language as langu,
      $session.system_date     as sys_date,
      $session.user_timezone   as usr_time,
      $session.user_date       as usr_date,

      // -------- Expressions  --------
      //Note: Aggregate and path expressions are covered in a separate CDS view entity.

      // -------- Cast expressions  --------
      // Convert the value of operands to a specified type after as
      // The examples use built-in data types only.
      // Note: Regarding which conversion combinations of types are possible, see the ABAP Keyword Documentation.
      // There are special conversion rules for every combination.
      cast( price as abap.dec(15,2) ) as cast_curr2dec,
      
      // The following example uses the prefix $projection. which defines reuse expressions.
      // That is, you can refer to an element defined previously in the element list of the same CDS view entity.
      // Note that this is only possible in dedicated positions. One of them is a cast expression.
      cast( $projection.date_lit as abap.char(8) ) as cast_dats2c,

      // -------- Arithmetic expressions  --------
      seatsocc_b + seatsocc_f as occ_seats_classes,
      seatsmax - seatsocc     as free_seats,
      2 * 2                   as mult,
      9 / 3                   as div,
      //Arithmetic expressions using cast and reuse expressions
      cast( paymentsum as abap.dec(17,2) ) * abap.dec'0.75' as discount_1,
      $projection.cast_curr2dec * $projection.discnt        as discount_2,
      cast( seatsocc / seatsmax * 100 as abap.dec(10, 2) )  as occupancy_rate,

      // -------- Case expressions  --------
      // Simple case distinction for comparing values of operands with other operands.
      // The first operand specified after THEN for which the comparison is true is
      // returned as a result. No match: Result is determined by the ELSE branch.

      case currency
        when 'EUR' then 'X'
        when 'USD' then 'Y'
        else 'Z'
      end as case1,

      // You can use the ELSE NULL addition that returns the null value.
      // Note: If ELSE is not specified, the null value is returned as a result.
      case $projection.case1
        when 'X' then 'A'
        else null
      end as case2,

      // Complex case distinction (searched case) for evaluating conditions
      case 
        when seatsmax <= 150 then 'small'
        when seatsmax > 150 and seatsmax < 300 then 'middle'
        when seatsmax >= 300 then 'large'
        else '?'
      end as case3,

      // -------- Excursion: Logical expressions --------
      // The following nonsense example using a CASE expression just visualizes the rich variety of options.

      //Comparison operators
      // Boolean operators AND, OR, NOT as well as parenthesized expressions are possible.
      case 
        when seatsmax = 385 and not ( seatsocc > 380 and seatsocc <> 379 or seatsocc <= 120 or paymentsum >= 200000 ) then 'A'
      //Interval comparisons
        when  seatsmax between 250 and 350 and seatsocc not between 1 and 100 then 'B'
      //Pattern comparisons ('%' -> wildcard character, represents any character string, '_' -> stands for any character)
        when carrid like '_L' then 'C'
      //Checking for null and initial value
        when currency is not null  or carrid is not initial then 'D'
        else '?'
      end as case4,

      // -------- Built-in functions --------
      // SQL functions (only a selection is covered here)

      // Numeric functions
      // The example uses typed and untyped literals only as arguments.
      abs( abap.int4'-1' )               as nf_abs,
      ceil( abap.decfloat34'3.333' )     as nf_ceil,
      floor( abap.decfloat34'3.333' )    as nf_floor,
      div( 25, 5 )                       as nf_div,
      mod( 11, 3 )                       as nf_mod,
      division( 1, 3, 2 )                as nf_division,
      round( abap.decfloat34'1.337', 2 ) as nf_round,

      // String functions
      concat(planetype, '-#')            as sf_concat,
      concat_with_space(carrid, '#', 1)  as sf_conc_ws, //3rd argument: number of spaces
      instr(currency, 'U')               as sf_instr, //Position of the first occurrence of the string from the substring in the argument (case-sensitive)
      left(currency, 2)                  as sf_left, //String of length n starting from the left of an expression
      length(planetype)                  as sf_len, //Number of characters in an argument ignoring trailing blanks
      lower(carrid)                      as sf_lower, //String with a length of an argument in which all uppercase letters are transformed to lowercase letters
      lpad(carrid, 5, '#')               as sf_lpad, //String of a length with the right-aligned content of an argument without trailing blanks and in which leading blanks produced by the expanded string are replaced by the characters from an argument (respecting all blanks)
      ltrim(planetype, 'A')              as sf_ltrim, //String with the content of an argument in which all trailing blanks and leading characters are removed that match the specified character. 
      replace(currency, 'U', '#')        as sf_repl, //String in which all instances of the second argument are replaced by the content from the third argument
      replace_regexpr(PCRE => '\\d',      //More optional parameters are possible; the example replaces all digits
                      VALUE => planetype,
                      WITH => '#',
                      RESULT_LENGTH => 10) as sf_repl_regex,
      right(currency, 2)                   as sf_right, //String of length n starting from the right of an expression
      rpad(carrid, 5, '#')                 as sf_rpad, //See lpad; here, right-algined content
      rtrim(planetype, '0')                as sf_rtrim, //See ltrim; here, from the right
      substring(planetype, 4, 3)           as sf_sub, //Returns a substring; second argument: position from where to start; third argument: length of the extracted substring
      upper( 'abap' )                      as sf_upper, //Transforms to upper case

      // Coalesce function
      // Checks whether the argument contains a null value. If it contains it, it returns the value of the second argument
      // Otherwise, it returns the value of the first argument.
      // This example has no null values in carrid, therefore the carrid value is output. See the example view about joins.
      coalesce(carrid, 'N') as coalesced,

      // Special functions
      // Type conversion functions
      fltp_to_dec( abap.fltp'12.34' as abap.dec(10,1) ) as fltp2dec,
    
      // Unit conversion
      // In the following example, the number that is input as parameter is used as the value for a distance in miles. It is converted to kilometers.
      @Semantics.quantity.unitOfMeasure: 'kilometers'
      unit_conversion( quantity => $parameters.p_smax,
                       source_unit => abap.unit'MI',
                       target_unit => $projection.kilometers ) as converted_value,

      // Date/Time functions
      // The function in the example calculates the days between to dates. The actual parameters must have the built-in data type DATS.
      dats_days_between(fldate,$projection.date_lit) as days_bw1,

      // The following example is similar to the example above. Here, the function expects the type DATN.
      // It also shows the use of another function (dats_to_datn) that converts the types.
      // Note: Only literals can be passed to the final two parameters on_error, on_initial.
      datn_days_between(dats_to_datn(fldate,'INITIAL','INITIAL') , dats_to_datn($session.user_date,'INITIAL','INITIAL')) as days_bw2,

      // The following function adds days to a date. Here, the date a week from today is calculated.
      dats_add_days($session.user_date,7,'INITIAL') as in1week,

      // Time stamp functions
      // Getting the current time stamp
      utcl_current() as ts,
      // Adding seconds to a time stamp
      utcl_add_seconds($projection.ts,60) as in1minute
}
// -------- Clauses for the SELECT statement --------
// SELECT statements of a CDS view entitiy can be specified with optional clauses
// Among them, there are WHERE (to restrict the rows of the result set), GROUP BY (grouping the result set(
// HAVING (further restriction after the GROUP BY clause) clauses and other set operators like EXCEPT, INTERSECT and UNION.
// This example uses a simple WHERE clause. It uses a condition that includes the input parameter to restrict the result set.
where seatsmax > $parameters.p_smax
