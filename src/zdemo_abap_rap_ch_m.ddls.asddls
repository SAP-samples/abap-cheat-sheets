@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZDEMO_ABAP_RAP_CH_M 
as select from zdemo_abap_rapt2 
association to parent ZDEMO_ABAP_RAP_RO_M 
  as _parent on $projection.key_field = _parent.key_field
{
  _parent,
  key key_field,
  key key_ch,
  field_ch1,
  field_ch2
}
