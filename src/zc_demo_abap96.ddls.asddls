@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText.label: 'Demo RAP BO'
define root view entity ZC_DEMO_ABAP96
  provider contract transactional_query
  as projection on ZR_DEMO_ABAP96
  association to
    ZR_DEMO_ABAP96 as toBase
      on
        toBase.UUID = $projection.UUID  
{
  key UUID,
      Text1,
      Text2,
      Num1,
      Num2,
      Country,
      toBase
}



