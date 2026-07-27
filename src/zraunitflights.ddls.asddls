@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZRAUNITFLIGHTS
  as select from ztaunitflights
{
  key carrid    as Carrid,
  key connid    as Connid,
  key fldate    as Fldate,
      planetype as Planetype,
      seatsmax  as Seatsmax,
      seatsocc  as Seatsocc
}
