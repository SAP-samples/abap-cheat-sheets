@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText.label: 'Demo RAP BO'
define root view entity ZC_DEMO_ABAP97
  provider contract transactional_query
  as projection on ZR_DEMO_ABAP97
{
   key ID,
  Number1,
  Number2,
  Operator,
  CalcResult 
}


