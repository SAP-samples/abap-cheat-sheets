// ABAP CDS cheat sheet example: Associations
//
//////////////////////////------ NOTES ------//////////////////////////////////
// - CDS view entity that selects from a demo database table.
// - The purpose of this CDS view entity is to demonstrate associations. The view
//   is used as a data source in another CDS view entity.
// - As a prerequisite, run the class zcl_demo_abap_cds_ve to populate the
//   database tables of the example. Otherwise, no data is displayed.
//
//////////////////////------ DATA PREVIEW ------///////////////////////////////
// - Choose F8 in ADT to open the data preview and check out the data displayed
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS view entity: Associations'
define view entity zdemo_abap_cds_ve_assoc_e
  as select from zdemo_abap_flsch as _flsch
  association [1..1] to zdemo_abap_carr as _carr_exp on _flsch.carrid = _carr_exp.carrid
  association [1..*] to zdemo_abap_fli  as _fli on _flsch.carrid = _fli.carrid and _flsch.connid = _fli.connid

{
  key _flsch.carrid    as Carrid,
  key _flsch.connid    as Connid,
      _flsch.countryfr as Countryfr,
      _flsch.cityfrom  as Cityfrom,
      _flsch.airpfrom  as Airpfrom,
      _flsch.countryto as Countryto,
      _flsch.cityto    as Cityto,
      _flsch.airpto    as Airpto,
      _flsch.fltime    as Fltime,
      _flsch.deptime   as Deptime,
      _flsch.arrtime   as Arrtime,
      _flsch.distance  as Distance,
      _flsch.distid    as Distid,
      _flsch.fltype    as Fltype,
      _flsch.period    as Period,
      _carr_exp,
      _fli
}
