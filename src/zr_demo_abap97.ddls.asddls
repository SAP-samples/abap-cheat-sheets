@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Demo RAP BO'
define root view entity ZR_DEMO_ABAP97
  as select from zdemoabap97 as demo_abap
{
   key id as ID,
  number1 as Number1,
  number2 as Number2,
  operator as Operator,
  calcresult as CalcResult 
}


