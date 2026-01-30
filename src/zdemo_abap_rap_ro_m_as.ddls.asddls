@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZDEMO_ABAP_RAP_RO_M_AS
  as select from zdemo_abap_tabca
{
  key id,
      num1,
      arithm_op,
      num2,
      calc_result,
      @Semantics.systemDateTime.createdAt: true       
      crea_date_time,      
      @Semantics.systemDateTime.lastChangedAt: true  
      lchg_date_time
}
