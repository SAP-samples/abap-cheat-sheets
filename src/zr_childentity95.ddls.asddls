@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Demo RAP BO'
define view entity ZR_CHILDENTITY95
  as select from zchildentity95 as ChildEntity
  association to parent ZR_ROOTENTITY95 as _RootEntity on $projection.ParentID = _RootEntity.ID
{
  key uuid as UUID,
  parent_id as ParentID,
  child_text as ChildText,
  text as Text,
  num as Num,
  _RootEntity
}




