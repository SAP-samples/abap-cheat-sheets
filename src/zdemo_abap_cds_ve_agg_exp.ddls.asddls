// ABAP CDS cheat sheet example:
// Aggregate expressions in the element list of CDS view entities
//
//////////////////////////------ NOTES ------//////////////////////////////////
// - CDS view entity selects from a demo database table
// - Demonstrates various aggregate expressions in the element list
// - As a prerequisite, run the class zcl_demo_abap_cds_ve to populate the
//   database tables of the example. Otherwise, no data is displayed.
//
//////////////////////------ DATA PREVIEW ------///////////////////////////////
// - Choose F8 in ADT to open the data preview and check out the data displayed
// - For comparing and checking the output, you can also open the data preview
//   for the database table. In ADT, press and hold CTRL and click the database
//   table name. In the opened table artifact, choose F8 to open the data preview.
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS view entity: Aggregate expressions'
define view entity zdemo_abap_cds_ve_agg_exp
  as select from zdemo_abap_fli

{

      // The element list intentionally includes few fields only to focus on the effect of aggregate expressions.
  key carrid,
      currency,

      // -------- Aggregate expressions --------
      // - Aggregate expressions can be used as elements of an element list. Other positions are possible.
      // - An alias name must be specified.
      // - A GROUP BY clause is required. It must list all non-aggregated fields from the element list.
      // - Additions: If ALL is used, all rows in the result set are respected. This is the default setting.
      //              If DISTINCT is used, only distinct values of an argument are respected.
      // - Note: There may or may not be spaces between the parentheses following avg, min, etc., and the 
      //   content specified within.

      // AVG (Returns the average value of an argument)
      avg( seatsocc as abap.dec(15,2))                            as avg_seats_occ,
      avg( cast(paymentsum as abap.dec(15, 2)) as abap.dec(15,2)) as avg_paysum,

      // SUM (Returns the sum of an argument)
      // Since a currency field is used in the example, an annotatin is required.
      @Semantics.amount.currencyCode: 'currency'
      sum(paymentsum)                                             as total_paysum,

      // MIN (Returns the least value of an argument)
      min( seatsocc )                                             as min_occ_seats,

      // MAX (Returns the greatest value of an argument)
      max( seatsocc )                                             as max_occ_seats,
      max( all seatsocc )                                         as max_occ_seats_all, //Same result as above, ALL is optional

      // COUNT (Returns counted lines)
      count(*)                                                    as cnt, // * means that all lines are respected
      count(distinct planetype)                                   as cnt_planetype //DISTINCT means that the number of dinstinct values if an argument is counted;
                                                                                   //e.g. if 3 is returned, it means there are 3 different plane types among the result set

}
//GROUP BY clause that lists all non-aggregated fields from the element list
group by
  carrid,
  currency
