@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZDEMO_ABAP_RAP_RO_M 
as select from zdemo_abap_rapt1 
composition [0..*] of ZDEMO_ABAP_RAP_CH_M as _child
{
  key key_field,
  field1,
  field2,
  field3,
  field4,
  _child
}
