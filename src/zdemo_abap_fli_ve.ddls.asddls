@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZDEMO_ABAP_FLI_VE
  as select from zdemo_abap_fli
{
  key carrid,
  key connid,
  key fldate,
      price,
      currency,
      planetype,
      seatsmax,
      seatsocc,
      paymentsum,
      seatsmax_b,
      seatsocc_b,
      seatsmax_f,
      seatsocc_f
}
