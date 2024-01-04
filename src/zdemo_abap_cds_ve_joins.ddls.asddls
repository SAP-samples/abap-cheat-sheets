// ABAP CDS cheat sheet example: Joins
//
//////////////////////////------ NOTES ------//////////////////////////////////
// - CDS view entity selects from a demo database table
// - Demonstrates various joins
// - As a prerequisite, run the class zcl_demo_abap_cds_ve to populate the
//   database tables of the example. Otherwise, no data is displayed.
//   It is particularly needed in this case because it contains entries to
//   visualize the effect of outer joins.
// - HOW TO:
//   - To reduce the number of separate artifacts, this example CDS view entity
//     contains the code for multiple joins, which is commented out.
//   - To test out various joins, you can comment out and comment in the
//     respective code sections (select the lines and choose CTRL + 7).
//   - The example for inner joins is commented in by default. The relevant
//     code sections are marked with "COMMENT IN/OUT ... START" and
//     "COMMENT IN/OUT ... END" in both SELECT and element list
//     (inner joins -> 1a / 1b). To test the left outer joins, for example,
//     comment out the respective sections of the inner join and comment in
//     the sections for left outer joins (2a, 2b), and so on.
//   - Once done, activate the view and choose F8 to open the data preview.
//     You must activate the view and choose F8 again for every change here
//     because the alias names are different.
//
//////////////////////------ DATA PREVIEW ------///////////////////////////////
// - Choose F8 in ADT to open the data preview and check out the data displayed
// - Note the hints above regarding commenting in/out of code.
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS view entity: Joins'
define view entity zdemo_abap_cds_ve_joins
  as select from zdemo_abap_carr  as _carr

  // Notes on joins:
  // - Joins combine two data sources into one result set, consisting of columns of both data sources
  // - Rows of the result set are determined by the join type and by join conditions between columns of the data sources
  // - Joins can be nested
  // - Inner or outer join must contain a join condition after ON
  // - Each element of the SELECT list must have the name of its data source as prefix
  // - When joins are used, a WHERE condition affects the result set
  // - The cardinality can be specified for left outer joins. For more details, see the ABAP Keyword Documentation.

  /////////////////// ----- Example for INNER JOIN ----- /////////////////////////////////
  // Notes:
  // - Joins all entries of the data sources whose fields match the ON condition
  // - In the table zdemo_abap_carr, there are entries in carrid that are not present in zdemo_abap_flsch.
  //   The same is true the other way round. The result only contains those entries that exist in both data sources
  //   based on the ON condition.

  //// ----> COMMENT IN/OUT 1a START <-----
    inner join   zdemo_abap_flsch as _flsch_in on _carr.carrid = _flsch_in.carrid
  //// ----> COMMENT IN/OUT 1a END <-----

  //Alternative syntax: Specifying 'inner' is optional
  //     join zdemo_abap_flsch
  //       as _flsch_in on _carr.carrid = _flsch_in.carrid

  /////////////////// ----- Example for LEFT OUTER JOIN ----- /////////////////////////////////
  // Notes:
  // - The join selects all entries on the left side. Entries that match the ON condition have the same content as in the inner join.
  //   In entries that do not match the ON condition, the elements on the right side have the null value.
  // - To demonstrate the effect, the table zdemo_abap_carr contains entries for carrid that are not present in zdemo_abap_flsch.
  // - Example in the element list:
  //   - The coalesce function can be used to prevent null values in the result set.
  //   - In the example, the function is used for an element to prevent null values.

  //// ----> COMMENT IN/OUT 2A START <-----
  //    left outer join zdemo_abap_flsch as _flsch_lo on _carr.carrid = _flsch_lo.carrid
  //// ----> COMMENT IN/OUT 2A END <-----

  /////////////////// ----- Example for RIGHT OUTER JOIN ----- /////////////////////////////////
  // Notes:
  // - The join selects all entries on the right side. Entries that match the ON condition have the same content as in the inner join.
  //   In entries that do not match the ON condition, the elements on the left side have the null value.
  // - To demonstrate the effect, the table zdemo_abap_carr contains entries for carrid that are not present in zdemo_abap_flsch.
  // - Example in the element list:
  //   - Instead of the coalesce function, you can also use a CASE expression using a logical expression with IS [NOT] NULL
  //     to prevent null values in the result set.
  //   - In the example, a CASE expression is used for an element to prevent null values.

  //// ----> COMMENT IN/OUT 3A START <-----
  //    right outer join zdemo_abap_flsch as _flsch_ro on _carr.carrid = _flsch_ro.carrid
  //// ----> COMMENT IN/OUT 3A END <-----

  /////////////////// ----- Example for CROSS JOIN ----- /////////////////////////////////
  // Notes:
  // - All entries on the left side are combined with all entries on the right side.
  // - It is not possible to specify an ON condition.
  //- The number of lines of the result is the number of left lines multiplied by the number of lines of the right table.

  //// ----> COMMENT IN/OUT 4A START <-----
  //    cross join zdemo_abap_flsch as _flsch_cr
  //// ----> COMMENT IN/OUT 4A END <-----

{
  key _carr.carrid,
      _carr.carrname,

      /////////////////// ----- Example for INNER JOIN ----- /////////////////////////////////

      // ----> COMMENT IN/OUT 1b START <-----
      _flsch_in.cityfrom as cityfr_in,
      _flsch_in.cityto   as cityto_in
      // ----> COMMENT IN/OUT 1b END <-----

      /////////////////// ----- Example for LEFT OUTER JOIN ----- /////////////////////////////////

      //// ----> COMMENT IN/OUT 2b START <-----
      //      _flsch_lo.cityfrom                as cityfr_lo,
      //      coalesce(_flsch_lo.cityto, '???') as cityto_lo
      //// ----> COMMENT IN/OUT 2b END <-----

      /////////////////// ----- Example for RIGHT OUTER JOIN ----- /////////////////////////////////

      //// ----> COMMENT IN/OUT 3b START <-----
      //      case when _carr.url is not null then _carr.url
      //           else '!!!'
      //      end                               as url_ro,
      //      _flsch_ro.cityfrom                as cityfr_ro,
      //      _flsch_ro.cityto                  as cityto_ro
      //// ----> COMMENT IN/OUT 3b END <-----

      /////////////////// ----- Example for CROSS JOIN ----- /////////////////////////////////

      //// ----> COMMENT IN/OUT 4b START <-----
      //      _flsch_cr.cityfrom                as cityfr_cr,
      //      _flsch_cr.cityto                  as cityto_cr
      //// ----> COMMENT IN/OUT 4b END <-----

}
