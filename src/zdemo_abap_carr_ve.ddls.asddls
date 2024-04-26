@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZDEMO_ABAP_CARR_VE
  as select from zdemo_abap_carr
{
  key carrid,
      carrname,
      currcode,
      url
}
