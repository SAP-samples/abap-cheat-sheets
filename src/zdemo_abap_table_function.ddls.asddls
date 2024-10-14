@AccessControl.authorizationCheck: #NOT_REQUIRED
@ClientHandling.type: #CLIENT_DEPENDENT
@ClientHandling.algorithm: #SESSION_VARIABLE 
define table function ZDEMO_ABAP_TABLE_FUNCTION
  returns
  {
    client          : abap.clnt;
    carrier_id      : abap.char(3);
    carrier_name    : abap.char(20);
    connections     : abap.string;
    avg_flight_time : abap.decfloat34;
    avg_distance    : abap.decfloat34;
  }
  implemented by method
    zcl_demo_abap_amdp=>flight_analysis;
