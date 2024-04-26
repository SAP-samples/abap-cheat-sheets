@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZDEMO_ABAP_FLSCH_VE
  as select from zdemo_abap_flsch
{
  key carrid,
  key connid,
      countryfr,
      cityfrom,
      airpfrom,
      countryto,
      cityto,
      airpto,
      fltime,
      deptime,
      arrtime,
      distance,
      distid,
      fltype,
      period
}
