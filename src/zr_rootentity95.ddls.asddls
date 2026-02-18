@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText.label: 'Demo RAP BO'
define root view entity ZR_ROOTENTITY95
  as select from zrootentity95 as RootEntity
  composition [1..*] of ZR_CHILDENTITY95 as _ChildEntity
{
  key id as ID,
  root_text as RootText,
  text1 as Text1,
  text2 as Text2,
  num1 as Num1,
  num2 as Num2,
  _ChildEntity
}




