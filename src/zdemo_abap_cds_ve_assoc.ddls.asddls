// ABAP CDS cheat sheet example: Associations
//
//////////////////////////------ NOTES ------//////////////////////////////////
// - CDS view entity that selects from a demo CDS view entity and demonstrates
//   associations. A selection of use cases of associations is covered. For
//   more information and examples, see the ABAP Keyword Documentation.
// - As a prerequisite, run the class zcl_demo_abap_cds_ve to populate the
//   database tables of the example. Otherwise, no data is displayed.
// - For further notes on associations, see the commented out information further
//   down.
// - To see how the joins are realized on the database, you can right-click anywhere
//   in the source code and choose 'Show SQL Create Statement'. The example
//   includes associations where only left outer joins are performed.  
//
//////////////////////------ DATA PREVIEW ------///////////////////////////////
// - Choose F8 in ADT to open the data preview and check out the data displayed
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS view entity: Associations 1'
define view entity zdemo_abap_cds_ve_assoc
  // In this example, the data source, from which data is read, is another CDS view entity.
  // This source CDS view entity selects all fields from the demo database table zdemo_abap_flsch.
  // It also contains exposed associations that are used here.
  as select from zdemo_abap_cds_ve_assoc_e as _flsch

  // 1) One use case of assocations, which is not covered here, can be the use in the FROM clause.
  //    - That is, you can use an association as data source by specifying a path expression.
  //    - For example, the CDS view entity zdemo_abap_cds_ve_assoc_e exposes an association
  //      _carr_exp. Therefore, a FROM clause as follows is possible:
  // ... as select from zdemo_abap_cds_ve_assoc_e._carr_exp as _flsch ...

  // 2) Adding fields of an association target to the element list (of the same CDS view entity)
  //    - In this case, since data is requested, a join is carried out. 
  //    - The data preview (choose F8) shows the values for the added fields.
  //    - In this example, the association is used, but not exposed. Therefore, it cannot be used
  //      in path expressions in other CDS entities as shown in 3).  
  association [1..1] to zdemo_abap_carr as _carr1 on _flsch.Carrid = _carr1.carrid
  association [1..1] to zdemo_abap_carr as _carr2 on _flsch.Carrid = _carr2.carrid
  association [1..*] to zdemo_abap_fli  as _fli on _flsch.Carrid = _fli.carrid

  // 3) Exposing associations
  //    - In this example, no fields of the association target are added to the element list. 
  //      But the assocation is exposed in the element list. Therefore, it can be used
  //      in path expressions in other CDS entities.  
  //    - In this case, since data is not requested, a join is not carried out. It is up to 
  //      to the consumer (e.g. an ABAP SQL statement or other CDS view entities) to request
  //      fields (one field, multiple fields, all fields ...). Only then, a join is performed. 
  //    - The data preview (choose F8) does not show any data of the association target.
  //    - Compare the ON condition to the ones above. For demonstration purposes, the carrid
  //      field is specified with an alias name in the element list. To refer to this name, 
  //      the prefix $projection. is required. As above, you can also use the original name. 
  association [1..1] to zdemo_abap_carr as _carr3 on $projection.carrier = _carr3.carrid

{
      // Including fields from the data source
      // The prefix _flsch. is actually not necessary here in the example. It's intentionally included to emphasize the data source.
  key _flsch.Carrid                                      as carrier,
  key _flsch.Connid                                      as connection_id,
      _flsch.Cityfrom,
      _flsch.Cityto,

      // Regarding 2) Adding fields of an association target to the element list
      // - Two fields are added in the element list.       
      // - You can make a right-click and choose 'Show SQL Create Statement' to check how the join is performed.
      // - Because a left outer join is performed, the coalesce function is included for a field. 
      //   If there are null values, the specified character literal is inserted.
      // - Only these two fields are respected from the association target.  
      _carr1.carrname                                    as carrier_name,
      coalesce(_carr1.url, 'NULL')                       as carr_url,

      // Regarding 3) Exposing associations
      // - The association is exposed in the element list (but no fields of the association target).
      // - As demonstrated in the executable class of this cheat sheet, an ABAP SQL statement
      //   can request data of the association target. 
      // - The data preview does not include any fields from the association target. Likewise,
      //   the 'SQL Create Statement' does not show any join with this assocation. 
      _carr3,
      
      // 4) Using associations that are exposed in external views
      //    - The CDS view entity that is used as data source in the SELECT list exposes
      //      associations. 
      //    - Such an association is used in a path expression here to include a field from the 
      //      association target in the element list. In this case, this CDS view entity is
      //      a consumer of the association. A join is performed, as can be seen in the
      //      'SQL Create Statement' view. 
      _flsch._carr_exp.currcode                         as curr_exposed,
      

      // 5) Attributes of path expression  
      //    - Attributes are specified in angle brackets after each CDS association to define
      //      further properties. These can be the cardinality, join type that is implemented when used,
      //      and filter conditions
      
      // Speciying filter conditions and the join type
      // - Only if a join type is specified explicitly, the addition WHERE must be specified
      // - The example specifies the join type. Therefore, WHERE is required. Here in the example, 
      //   the specification can also be the one commented out below because it implicitly has
      //   the same effect. Here, a left outer join is performed by default. 
      // - The operand specified on the left side of the condition must be a field of the association target.
      _carr2[left outer where $projection.carrier = 'LH'].url  as fcond_url,
      //_carr2[$projection.carrier = 'LH'].url as fcond_url
      
      // Specifying the cardinality of the current CDS association
      // - The values 1: (current CDS association is declared as unique) and *: (non-unique) are possible.      
      // - You can use 1: to prevent a syntax warning in case of a path specified with filter conditions, for example.
      // - In the example, an association is specified with to many (there can be multiple flights for a flight route).
      //   Without the cardinality specification 1:, a warning would be displayed ("The association _fli can modify 
      //   the cardinality of the results set"). 
      // - Similar to the example above, the first line commented out below is also possible in this example.
      //   The second line commented out results in a syntax warning because it does not specify the 
      //   cardinality 1:.   
      
      _fli[1:$projection.connection_id = connid].fldate as fcond_fldate
      //_fli[1:left outer where $projection.connection_id = connid].fldate as fcond_fldate
      //_fli[$projection.connection_id = connid].fldate as fcond_fldate
}

////////////////////////// --------- Notes on associations --------- /////////////////////////////////////////
// - They offer an advanced modelling capability for CDS data models. They define relationships between
//   CDS entities (association source and target). On the database, associations are internally transformed into joins.
// - Compared to regular joins, associations differ in the following respects:
//    - Joins are only performed on demand, i.e. when data is requested from the assocation by a consumer, e.g.
//      using another CDS view entity or an ABAP SQL statement. In the regular joins (e.g. inner, outer joins), the
//      join is always performed. When a CDS association is instantiated as join on the database, the association
//      source represents the left side and the association target represents the right side of the join.
//    - In the SELECT list, specifying the data source as a prefix is mandatory for all fields of the association.
//      For joins, it is mandatory only for for non-unique names.
//    - Unlike regular joins, associations can be reused in different positions and basically replace very complex
//      join expressions.
// - Use of associations:
//    - Including fields from the association target in the current view
//    - Exposing associations so that they can be used in other CDS entities or in ABAP SQL
//    - Associations can be used in path expressions (a sequence of associations) at different operand positions.
// - Associations can be specified with additional semantic information, such as cardinality.
//   - Specifying the (optional) cardinality is a means of documenting the semantics of the data model.
//   - Cardinality is specified in square brackets [ ]. Minimum and maximum can be specified, for example
//     one to one [1..1], one to many [1..*]. The default is [0..1]. Specifying the minimum value is optional (the
//     default value is 0), i.e. [0..1] is [1], [0..*] is [*]. The minimum cannot be *, the maximum not 0.
//   - Maximum values greater than 1 can lead to syntax errors or warnings. Generally, a non-matching cardinality
//     usually produces a warning.
//   - If the cardinality is not specified, it is to one by default [x..1].
// - Compositions and to-parent associations are special kinds of CDS associations. See the ABAP Keyword Documentation. 
