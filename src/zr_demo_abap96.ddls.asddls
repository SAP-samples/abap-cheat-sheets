@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Demo RAP BO'
define root view entity ZR_DEMO_ABAP96
  as select from zdemoabap96 as demo_abap
{
  key uuid as UUID,
  text1 as Text1,
  text2 as Text2,
  num1 as Num1,
  num2 as Num2,
  country as Country
}



