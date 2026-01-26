"! <p class="shorttext">Auxiliary class for ABAP cheat sheet example classes</p>
"!
"! <p>The class supports the ABAP cheat examples by providing reusable functionality.</p>
"!
"! <h2>Disclaimer</h2>
"! <p>The code presented in this class is only meant for supporting the ABAP
"! cheat sheets. It is not intended for direct use in a
"! production system environment. The code examples in the ABAP cheat
"! sheets are primarily intended to provide a better explanation and
"! visualization of the syntax and semantics of ABAP statements and not to
"! solve concrete programming tasks. For production application programs,
"! a dedicated solution should therefore always be worked out for each
"! individual case. There is no guarantee for either the correctness or
"! the completeness of the code. In addition, there is no legal
"! responsibility or liability for possible errors or their consequences
"! which occur through the use of the example code.</p>
CLASS zcl_demo_abap_oodp_aux DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS: clear_dbtabs,
      fill_dbtabs,
      set_example_divider IMPORTING out  TYPE REF TO if_oo_adt_classrun_out
                                    text TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_oodp_aux IMPLEMENTATION.
  METHOD clear_dbtabs.
    DELETE FROM zdemoabapcarrier.
    DELETE FROM zdemoabapflights.
  ENDMETHOD.


  METHOD fill_dbtabs.

    "Clearing db tables before filling
    clear_dbtabs( ).

    "Filling db table
    MODIFY zdemoabapcarrier FROM TABLE @( VALUE #(
        ( carrid = 'AA'
          carrname = 'American Airlines'
          currcode = 'USD'
          url =  'http://www.aa.com' )
        ( carrid = 'LH'
          carrname = 'Lufthansa'
          currcode = 'EUR'
          url =  'http://www.lufthansa.com' )
        ( carrid = 'JL'
          carrname = 'Japan Airlines'
          currcode = 'JPY'
          url =  'http://www.jal.co.jp' )
        ( carrid = 'DL'
          carrname = 'Delta Airlines'
          currcode = 'USD'
          url =  'http://www.delta-air.com' )
        ( carrid = 'AZ'
          carrname = 'ITA Airways'
          currcode = 'EUR'
          url =  'http://www.ita-airways.com' ) ) ).

    "Filling db table
    MODIFY zdemoabapflights FROM TABLE @( VALUE #(
        ( carrid = 'AA'
          connid =  0017
          fldate =  '20230923'
          price  =    '464.35'
          currency = 'USD'
          planetype  =    '747-400'
          seatsmax =   385
          seatsocc =   369
          paymentsum =   '191993.87'
          seatsmax_b = 31
          seatsocc_b = 31
          seatsmax_f =  21
          seatsocc_f = 19 )
        ( carrid = 'AA'
          connid =  0017
          fldate =  '20230929'
          price  =    '464.35'
          currency = 'USD'
          planetype  =    '747-400'
          seatsmax =   385
          seatsocc =   372
          paymentsum =   '193537.52'
          seatsmax_b = 31
          seatsocc_b = 30
          seatsmax_f =  21
          seatsocc_f = 20 )
        ( carrid = 'AA'
          connid =  0017
          fldate =  '20231111'
          price  =    '464.35'
          currency = 'USD'
          planetype  =    '747-400'
          seatsmax =   385
          seatsocc =   374
          paymentsum =   '193651.77'
          seatsmax_b = 31
          seatsocc_b = 29
          seatsmax_f =  21
          seatsocc_f = 21 )
        ( carrid = 'AA'
          connid =  0064
          fldate =  '20220131'
          price  =    '464.35'
          currency = 'USD'
          planetype  =    'A340-600'
          seatsmax =   330
          seatsocc =   313
          paymentsum =   '168469.88'
          seatsmax_b = 30
          seatsocc_b = 30
          seatsmax_f =  20
          seatsocc_f = 19 )
        ( carrid = 'AA'
          connid =  0064
          fldate =  '20220215'
          price  =    '464.35'
          currency = 'USD'
          planetype  =    'A340-600'
          seatsmax =   330
          seatsocc =   157
          paymentsum =   '84846.15'
          seatsmax_b = 30
          seatsocc_b = 15
          seatsmax_f =  20
          seatsocc_f = 10 )
        ( carrid = 'AZ'
          connid =  0555
          fldate =  '20230721'
          price  =    '226.41'
          currency = 'EUR'
          planetype  =    'A319-100'
          seatsmax =   120
          seatsocc =   114
          paymentsum =   '26519.75'
          seatsmax_b = 8
          seatsocc_b = 8
          seatsmax_f =  8
          seatsocc_f = 8 )
        ( carrid = 'AZ'
          connid =  0555
          fldate =  '20230728'
          price  =    '226.41'
          currency = 'EUR'
          planetype  =    'A319-100'
          seatsmax =   120
          seatsocc =   115
          paymentsum =   '16695.50'
          seatsmax_b = 8
          seatsocc_b = 8
          seatsmax_f =  8
          seatsocc_f = 8 )
        ( carrid = 'AZ'
          connid =  0788
          fldate =  '20230922'
          price  =    '1071.41'
          currency = 'EUR'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   456
          paymentsum =   '548722.20'
          seatsmax_b = 30
          seatsocc_b = 30
          seatsmax_f =  20
          seatsocc_f = 20 )
        ( carrid = 'AZ'
          connid =  0788
          fldate =  '20230722'
          price  =    '1071.41'
          currency = 'EUR'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   455
          paymentsum =   '544674.30'
          seatsmax_b = 30
          seatsocc_b = 28
          seatsmax_f =  20
          seatsocc_f = 20 )
        ( carrid = 'AZ'
          connid =  0789
          fldate =  '20231025'
          price  =    '1071.41'
          currency = 'EUR'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   455
          paymentsum =   '545704.30'
          seatsmax_b = 30
          seatsocc_b = 30
          seatsmax_f =  20
          seatsocc_f = 19 )
        ( carrid = 'AZ'
          connid =  0789
          fldate =  '20230221'
          price  =    '1071.41'
          currency = 'EUR'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   459
          paymentsum =   '549226.90'
          seatsmax_b = 30
          seatsocc_b = 30
          seatsmax_f =  20
          seatsocc_f = 20 )
        ( carrid = 'AZ'
          connid =  0790
          fldate =  '20231228'
          price  =    '1055.41'
          currency = 'EUR'
          planetype  =    '747-400'
          seatsmax =   385
          seatsocc =   370
          paymentsum =   '462373.86'
          seatsmax_b = 31
          seatsocc_b = 30
          seatsmax_f =  21
          seatsocc_f = 21 )
        ( carrid = 'AZ'
          connid =  0790
          fldate =  '20231201'
          price  =    '1055.41'
          currency = 'EUR'
          planetype  =    '747-400'
          seatsmax =   385
          seatsocc =   367
          paymentsum =   '463661.64'
          seatsmax_b = 31
          seatsocc_b = 31
          seatsmax_f =  21
          seatsocc_f = 21 )
        ( carrid = 'DL'
          connid =  0106
          fldate =  '20230209'
          price  =    '652.42'
          currency = 'USD'
          planetype  =    'A340-600'
          seatsmax =   330
          seatsocc =   178
          paymentsum =   '136750.33'
          seatsmax_b = 30
          seatsocc_b = 17
          seatsmax_f =  20
          seatsocc_f = 10 )
        ( carrid = 'DL'
          connid =  0106
          fldate =  '20240102'
          price  =    '652.42'
          currency = 'USD'
          planetype  =    'A340-600'
          seatsmax =   330
          seatsocc =   16
          paymentsum =   '12892.33'
          seatsmax_b = 30
          seatsocc_b = 2
          seatsmax_f =  20
          seatsocc_f = 10 )
        ( carrid = 'DL'
          connid =  1699
          fldate =  '20230921'
          price  =    '464.35'
          currency = 'USD'
          planetype  =    '767-200'
          seatsmax =   260
          seatsocc =   250
          paymentsum =   '126636.91'
          seatsmax_b = 21
          seatsocc_b = 20
          seatsmax_f =  11
          seatsocc_f = 11 )
        ( carrid = 'DL'
          connid =  1699
          fldate =  '20230511'
          price  =    '464.35'
          currency = 'USD'
          planetype  =    '767-200'
          seatsmax =   260
          seatsocc =   251
          paymentsum =   '126493.06'
          seatsmax_b = 21
          seatsocc_b = 20
          seatsmax_f =  11
          seatsocc_f = 11 )
        ( carrid = 'DL'
          connid =  1984
          fldate =  '20230719'
          price  =    '464.35'
          currency = 'USD'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   460
          paymentsum =   '225427.35'
          seatsmax_b = 30
          seatsocc_b = 29
          seatsmax_f =  20
          seatsocc_f = 19 )
        ( carrid = 'DL'
          connid =  1984
          fldate =  '20230213'
          price  =    '464.35'
          currency = 'USD'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   458
          paymentsum =   '225088.83'
          seatsmax_b = 30
          seatsocc_b = 30
          seatsmax_f =  20
          seatsocc_f = 19 )
        ( carrid = 'JL'
          connid =  0407
          fldate =  '20231128'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   458
          paymentsum =   '563231.65'
          seatsmax_b = 30
          seatsocc_b = 27
          seatsmax_f =  20
          seatsocc_f = 20 )
        ( carrid = 'JL'
          connid =  0407
          fldate =  '20231019'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   452
          paymentsum =   '553552.12'
          seatsmax_b = 30
          seatsocc_b = 28
          seatsmax_f =  20
          seatsocc_f = 19 )
        ( carrid = 'JL'
          connid =  0408
          fldate =  '20231128'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    '747-400'
          seatsmax =   385
          seatsocc =   365
          paymentsum =   '470129.20'
          seatsmax_b = 31
          seatsocc_b = 28
          seatsmax_f =  21
          seatsocc_f = 20 )
        ( carrid = 'JL'
          connid =  0408
          fldate =  '20230123'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    '747-400'
          seatsmax =   385
          seatsocc =   372
          paymentsum =   '487715.90'
          seatsmax_b = 31
          seatsocc_b = 31
          seatsmax_f =  21
          seatsocc_f = 20 )
        ( carrid = 'LH'
          connid =  0400
          fldate =  '20230628'
          price  =    '1184.54'
          currency = 'EUR'
          planetype  =    'A340-600'
          seatsmax =   330
          seatsocc =   319
          paymentsum =   '270822.24'
          seatsmax_b = 30
          seatsocc_b = 30
          seatsmax_f =  20
          seatsocc_f = 20 )
        ( carrid = 'LH'
          connid =  0400
          fldate =  '20230323'
          price  =    '1184.54'
          currency = 'EUR'
          planetype  =    'A340-600'
          seatsmax =   330
          seatsocc =   312
          paymentsum =   '262597.14'
          seatsmax_b = 30
          seatsocc_b = 28
          seatsmax_f =  20
          seatsocc_f = 19 )
        ( carrid = 'LH'
          connid =  0401
          fldate =  '20231128'
          price  =    '669.20'
          currency = 'EUR'
          planetype  =    '767-200'
          seatsmax =   260
          seatsocc =   246
          paymentsum =   '195417.72'
          seatsmax_b = 21
          seatsocc_b = 19
          seatsmax_f =  11
          seatsocc_f = 10 )
        ( carrid = 'LH'
          connid =  0401
          fldate =  '20231229'
          price  =    '669.20'
          currency = 'EUR'
          planetype  =    '767-200'
          seatsmax =   260
          seatsocc =   252
          paymentsum =   '199300.50'
          seatsmax_b = 21
          seatsocc_b = 19
          seatsmax_f =  11
          seatsocc_f = 11 )
        ( carrid = 'LH'
          connid =  0402
          fldate =  '20230617'
          price  =    '669.20'
          currency = 'EUR'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   461
          paymentsum =   '353526.12'
          seatsmax_b = 30
          seatsocc_b = 29
          seatsmax_f =  20
          seatsocc_f = 18 )
        ( carrid = 'LH'
          connid =  0402
          fldate =  '20230313'
          price  =    '669.20'
          currency = 'EUR'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   450
          paymentsum =   '349223.76'
          seatsmax_b = 30
          seatsocc_b = 29
          seatsmax_f =  20
          seatsocc_f = 19 )
        ( carrid = 'LH'
          connid =  2402
          fldate =  '20231028'
          price  =    '245.20'
          currency = 'EUR'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   451
          paymentsum =   '127197.62'
          seatsmax_b = 30
          seatsocc_b = 29
          seatsmax_f =  20
          seatsocc_f = 19 )
        ( carrid = 'LH'
          connid =  2402
          fldate =  '20231223'
          price  =    '245.20'
          currency = 'EUR'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   458
          paymentsum =   '18944.86'
          seatsmax_b = 30
          seatsocc_b = 30
          seatsmax_f =  20
          seatsocc_f = 20 ) ) ).
  ENDMETHOD.

  METHOD set_example_divider.
    out->write( |\n| ).
    out->write( |*&{ repeat( val = `-` occ = strlen( text ) + 2 ) }*| ).
    out->write( |*& { text }| ).
    out->write( |*&{ repeat( val = `-` occ = strlen( text ) + 2 ) }*| ).
    out->write( |\n| ).
    out->write( |\n| ).
  ENDMETHOD.
ENDCLASS.
