"! <p class="shorttext">ABAP cheat sheet example class: ABAP cheat sheet auxiliary class</p>
"!
"! <p>The class supports the ABAP cheat examples by clearing and populating demo database tables that are used there.
"! The demo database tables contain airline and flight information.</p>
"!
"! <h2>Getting started with ABAP cheat sheet example classes</h2>
"! <ol><li>Open the class using ABAP development tools for Eclipse (ADT).</li>
"! <li>Choose F9 to run the class.</li>
"! <li>Check the console output.</li>
"! <li>To understand the context and ABAP syntax used, refer to the notes in the
"! class comments or the relevant topic in the ABAP Keyword Documentation.</li>
"! <li>The console output contains numbered sections (e.g., 1) ... , 2) ..., 3) ...)
"! for each example. Most of the time, the variable name is diplayed. To quickly find
"! the output, use CTRL+F to search for the specific number or variable name in the
"! console. Alternatively, debug the class using the debugger to check the data
"! objects' content and values.</li></ol>
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
CLASS zcl_demo_abap_aux DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS: clear_dbtabs,
      fill_dbtabs,
      heading IMPORTING text          TYPE string
              RETURNING VALUE(output) TYPE string.
    CONSTANTS no_output TYPE string VALUE `No output for this section. You can set breakpoints and check the content of data objects (if available) in the debugger.`.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_aux IMPLEMENTATION.

  METHOD clear_dbtabs.
    DELETE FROM zdemo_abap_flsch.
    DELETE FROM zdemo_abap_carr.
    DELETE FROM zdemo_abap_fli.
  ENDMETHOD.


  METHOD fill_dbtabs.

    "Clearing db tables before filling
    clear_dbtabs( ).

    "Filling db table
    MODIFY zdemo_abap_flsch FROM TABLE @( VALUE #(
      ( carrid = 'AA'
        connid =  0017
        countryfr =  'US'
        cityfrom =  'NEW YORK'
        airpfrom = 'JFK'
        countryto = 'US'
        cityto = 'SAN FRANCISCO'
        airpto = 'SFO'
        fltime = 361
        deptime = '110000'
        arrtime = '140100'
        distance =  2572
        distid = 'MI'
        fltype = ''
        period = 0 )
      ( carrid = 'AA'
        connid =  0064
        countryfr =  'US'
        cityfrom =  'SAN FRANCISCO'
        airpfrom = 'SFO'
        countryto = 'US'
        cityto = 'NEW YORK'
        airpto = 'JFK'
        fltime = 321
        deptime = '090000'
        arrtime = '172100'
        distance =  2572
        distid = 'MI'
        fltype = ''
        period = 0 )
      ( carrid = 'AZ'
        connid =  0555
        countryfr =  'IT'
        cityfrom =  'ROME'
        airpfrom = 'FCO'
        countryto = 'DE'
        cityto = 'FRANKFURT'
        airpto = 'FRA'
        fltime = 125
        deptime = '190000'
        arrtime = '210500'
        distance =  845
        distid = 'MI'
        fltype = ''
        period = 0 )
      ( carrid = 'AZ'
        connid =  0788
        countryfr =  'IT'
        cityfrom =  'ROME'
        airpfrom = 'FCO'
        countryto = 'JP'
        cityto = 'TOKYO'
        airpto = 'TYO'
        fltime = 775
        deptime = '120000'
        arrtime = '085500'
        distance =  6130
        distid = 'MI'
        fltype = ''
        period = 1 )
      ( carrid = 'AZ'
        connid =  0789
        countryfr =  'JP'
        cityfrom =  'TOKYO'
        airpfrom = 'TYO'
        countryto = 'IT'
        cityto = 'ROME'
        airpto = 'FCO'
        fltime = 940
        deptime = '114500'
        arrtime = '192500'
        distance =  6130
        distid = 'MI'
        fltype = ''
        period = 0 )
      ( carrid = 'AZ'
        connid =  0790
        countryfr =  'IT'
        cityfrom =  'ROME'
        airpfrom = 'FCO'
        countryto = 'JP'
        cityto = 'OSAKA'
        airpto = 'KIX'
        fltime = 815
        deptime = '103500'
        arrtime = '081000'
        distance =  6030
        distid = 'MI'
        fltype = 'X'
        period = 1 )
      ( carrid = 'DL'
        connid =  0106
        countryfr =  'US'
        cityfrom =  'NEW YORK'
        airpfrom = 'JFK'
        countryto = 'DE'
        cityto = 'FRANKFURT'
        airpto = 'FRA'
        fltime = 475
        deptime = '193500'
        arrtime = '093000'
        distance =  3851
        distid = 'MI'
        fltype = ''
        period = 1 )
      ( carrid = 'DL'
        connid =  1699
        countryfr =  'US'
        cityfrom =  'NEW YORK'
        airpfrom = 'JFK'
        countryto = 'US'
        cityto = 'SAN FRANCISCO'
        airpto = 'SFO'
        fltime = 382
        deptime = '171500'
        arrtime = '203700'
        distance =  2572
        distid = 'MI'
        fltype = ''
        period = 0 )
      ( carrid = 'DL'
        connid =  1984
        countryfr =  'US'
        cityfrom =  'SAN FRANCISCO'
        airpfrom = 'SFO'
        countryto = 'US'
        cityto = 'NEW YORK'
        airpto = 'JFK'
        fltime = 325
        deptime = '100000'
        arrtime = '182500'
        distance =  2572
        distid = 'MI'
        fltype = ''
        period = 0 )
      ( carrid = 'JL'
        connid =  0407
        countryfr =  'JP'
        cityfrom =  'TOKYO'
        airpfrom = 'NRT'
        countryto = 'DE'
        cityto = 'FRANKFURT'
        airpto = 'FRA'
        fltime = 725
        deptime = '133000'
        arrtime = '173500'
        distance =  9100
        distid = 'KM'
        fltype = ''
        period = 0 )
      ( carrid = 'JL'
        connid =  0408
        countryfr =  'DE'
        cityfrom =  'FRANKFURT'
        airpfrom = 'FRA'
        countryto = 'JP'
        cityto = 'TOKYO'
        airpto = 'NRT'
        fltime = 675
        deptime = '202500'
        arrtime = '154000'
        distance =  9100
        distid = 'KM'
        fltype = 'X'
        period = 1 )
      ( carrid = 'LH'
        connid =  0400
        countryfr =  'DE'
        cityfrom =  'FRANKFURT'
        airpfrom = 'FRA'
        countryto = 'US'
        cityto = 'NEW YORK'
        airpto = 'JFK'
        fltime = 444
        deptime = '101000'
        arrtime = '113400'
        distance =  6162
        distid = 'KM'
        fltype = ''
        period = 0 )
      ( carrid = 'LH'
        connid =  0401
        countryfr =  'US'
        cityfrom =  'NEW YORK'
        airpfrom = 'JFK'
        countryto = 'DE'
        cityto = 'FRANKFURT'
        airpto = 'FRA'
        fltime = 435
        deptime = '183000'
        arrtime = '074500'
        distance =  6162
        distid = 'KM'
        fltype = ''
        period = 1 )
      ( carrid = 'LH'
        connid =  0402
        countryfr =  'DE'
        cityfrom =  'FRANKFURT'
        airpfrom = 'FRA'
        countryto = 'US'
        cityto = 'NEW YORK'
        airpto = 'JFK'
        fltime = 455
        deptime = '133000'
        arrtime = '150500'
        distance =  6162
        distid = 'KM'
        fltype = 'X'
        period = 0 )
      ( carrid = 'LH'
        connid =  2402
        countryfr =  'DE'
        cityfrom =  'FRANKFURT'
        airpfrom = 'FRA'
        countryto = 'DE'
        cityto = 'BERLIN'
        airpto = 'SXF'
        fltime = 65
        deptime = '103000'
        arrtime = '113500'
        distance =  555
        distid = 'KM'
        fltype = ''
        period = 0 ) ) ).

    "Filling db table
    MODIFY zdemo_abap_carr FROM TABLE @( VALUE #(
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
    MODIFY zdemo_abap_fli FROM TABLE @( VALUE #(
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
  METHOD heading.
    output = |\n_________________________________________________________________________________\n\n{ text }\n\n|.
  ENDMETHOD.

ENDCLASS.

