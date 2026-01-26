"! <p class="shorttext"><strong>Template Method</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the tempmate Method design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_template_m DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_oodp_template_m IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Template Method` ).

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `1) Parsing XML using iXML` ).

    DATA(demo_xml) =
      `<?xml version="1.0"?>` &&
      `<node attr_a="123">` &&
      ` <subnode1>` &&
      `  <hallo>hi</hallo>` &&
      ` </subnode1>` &&
      ` <subnode2>` &&
      `  <letter>a</letter>` &&
      `  <date format="mm-dd-yyyy">01-01-2025</date>` &&
      ` </subnode2>` &&
      ` <subnode3>`  &&
      `  <text attr_b="1" attr_c="a">abc</text>` &&
      `  <text attr_b="2" attr_c="b">def</text>` &&
      `  <text attr_b="3" attr_c="c">ghi</text>` &&
      `  <text attr_b="4" attr_c="d">jkl</text>` &&
      ` </subnode3>` &&
      `</node>`.

    DATA(oref_ixml) = NEW lcl_xml_parser_ixml( ).
    oref_ixml->template_method( data = demo_xml out = out ).

    DATA(result_ixml) = oref_ixml->result.

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `2) Parsing XML using sXML` ).

    DATA(oref_sxml) = NEW lcl_xml_parser_sxml( ).
    oref_sxml->template_method( data = demo_xml out = out ).

    DATA(result_sxml) = oref_sxml->result.

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `3) Parsing JSON using sXML` ).

    DATA(demo_json) =
       `[` &&
       `    {` &&
       `        "carrier_id": "LH",` &&
       `        "connection_id": "400",` &&
       `        "city_from": "Frankfurt",` &&
       `        "city_to": "Berlin"` &&
       `    },` &&
       `    {` &&
       `        "carrier_id": "DL",` &&
       `        "connection_id": "1984",` &&
       `        "city_from": "San Francisco",` &&
       `        "city_to": "New York"` &&
       `    },` &&
       `    {` &&
       `        "carrier_id": "AZ",` &&
       `        "connection_id": "790",` &&
       `        "city_from": "Rome",` &&
       `        "city_to": "Osaka"` &&
       `    }` &&
       `]`.

    DATA(oref_json_sxml) = NEW lcl_json_parser_sxml( ).
    oref_json_sxml->template_method( data = demo_json out = out ).

    DATA(result_json_sxml) = oref_json_sxml->result.

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `4) Parsing JSON using /ui2/cl_json` ).

    DATA(complex_json) =
     `[` &&
    `{` &&
    `  "orderId": "9876",` &&
    `  "customer": {` &&
    `    "name": "John Doe",` &&
    `    "email": "john.doe@example.com",` &&
    `    "phone": "+49-01234-56789",` &&
    `    "address": {` &&
    `      "street": "Some Street 1",` &&
    `      "city": "Walldorf",` &&
    `      "zipcode": "12345",` &&
    `      "state": "BW",` &&
    `      "country": "Germany"` &&
    `    }` &&
    `  },` &&
    `  "items": [` &&
    `    {` &&
    `      "productId": "123",` &&
    `      "name": "Laptop",` &&
    `      "quantity": 1,` &&
    `      "price": 1200.00,` &&
    `      "attributes": {` &&
    `        "color": "Black",` &&
    `        "warranty": "2 years"` &&
    `      }` &&
    `    },` &&
    `    {` &&
    `      "productId": "654",` &&
    `      "name": "Keyboard",` &&
    `      "quantity": 2,` &&
    `      "price": 45.50,` &&
    `      "attributes": {` &&
    `        "color": "Grey",` &&
    `        "warranty": "1 year"` &&
    `      }` &&
    `    }` &&
    `  ],` &&
    `  "payment": {` &&
    `    "method": "Credit card",` &&
    `    "transactionId": "t1234567890",` &&
    `    "amount": 1291.00,` &&
    `    "currency": "EUR"` &&
    `  },` &&
    `  "orderDate": "2025-15-01T15:36:10Z",` &&
    `  "status": "In progress"` &&
    `}` &&
       `]`.

    DATA(oref_json_ui2_cl_json) = NEW lcl_json_parser_ui2cljson( ).
    oref_json_ui2_cl_json->template_method( data = complex_json out = out ).

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

    "More demo json
    LOOP AT VALUE string_table(
    ( `[` &&
    `{` &&
    `    "name": "Jane Doe",` &&
    `    "age": 25,` &&
    `    "height": null,` &&
    `    "isStudent": true,` &&
    `    "courses": ["Mathematics", "Computer Science", "Art History"]` &&
    `}` &&
    `]` )

    ( `[` &&
    `    "lion",` &&
    `    "elephant",` &&
    `    "bear",` &&
    `    "penguin"` &&
    `]` )

    ( `{` &&
    `    "personalInfo": {` &&
    `        "basic": {` &&
    `            "firstName": "Jane",` &&
    `            "lastName": "Doe",` &&
    `            "age": 24,` &&
    `            "gender": "Female",` &&
    `            "contactDetails": {` &&
    `                "emails": [` &&
    `                    "jane.doe@example.com",` &&
    `                    "janedoe@example.com"` &&
    `                ],` &&
    `                "phone": [` &&
    `                    {` &&
    `                        "type": "mobile",` &&
    `                        "phoneNumber": "0123456789"` &&
    `                    },` &&
    `                    {` &&
    `                        "type": "home",` &&
    `                        "phoneNumber": "9876543210"` &&
    `                    }` &&
    `                ]` &&
    `            }` &&
    `        },` &&
    `        "address": {` &&
    `            "currentAddress": {` &&
    `                "street": "Some Street 1",` &&
    `                "city": "Walldorf",` &&
    `                "zipCode": "69190",` &&
    `                "state": "AA",` &&
    `                "country": "Germany"` &&
    `            },` &&
    `            "previousAddresses": [` &&
    `                {` &&
    `                    "street": "Some Street 2",` &&
    `                    "city": "Somewhere",` &&
    `                    "zipCode": "12345",` &&
    `                    "state": "BB",` &&
    `                    "country": "UK"` &&
    `                },` &&
    `                {` &&
    `                    "street": "Some Street 3",` &&
    `                    "city": "Another Place",` &&
    `                    "zipCode": "98765",` &&
    `                    "state": "CC",` &&
    `                    "country": "USA"` &&
    `                }` &&
    `            ]` &&
    `        },` &&
    `        "career": [` &&
    `            {` &&
    `                "company": "Some company",` &&
    `                "role": "Software Engineer",` &&
    `                "timespan": "2018-2025",` &&
    `                "jobInformation": {` &&
    `                    "tasks": [` &&
    `                        "Doing something",` &&
    `                        "Doing another thing",` &&
    `                        "Doing this"` &&
    `                    ],` &&
    `                    "projects": [` &&
    `                        {` &&
    `                            "name": "Project A",` &&
    `                            "description": "Development of A"` &&
    `                        },` &&
    `                        {` &&
    `                            "name": "Project B",` &&
    `                            "description": "Development of B"` &&
    `                        }` &&
    `                    ]` &&
    `                }` &&
    `            },` &&
    `            {` &&
    `                "company": "Another company",` &&
    `                "role": "Developer",` &&
    `                "timespan": "2016-2018",` &&
    `                "jobInformation": {` &&
    `                    "tasks": [` &&
    `                        "Doing abc",` &&
    `                        "Doing def",` &&
    `                        "Doing ghi"` &&
    `                    ],` &&
    `                    "projects": [` &&
    `                        {` &&
    `                            "name": "Project C",` &&
    `                            "description": "Development of C"` &&
    `                        }` &&
    `                    ]` &&
    `                }` &&
    `            }` &&
    `        ],` &&
    `        "education": [` &&
    `            {` &&
    `                "institution": "Some University",` &&
    `                "degree": "Degree A",` &&
    `                "field": "Computer Science",` &&
    `                "attendanceTime": "2012-2016",` &&
    `                "achievements": [` &&
    `                    "Achievement A",` &&
    `                    "Achievement B"` &&
    `                ]` &&
    `            },` &&
    `            {` &&
    `                "institution": "Some School",` &&
    `                "degree": "Diploma B",` &&
    `                "attendanceTime": "2008-2012"` &&
    `            }` &&
    `        ]` &&
    `    }` &&
    `}` ) ) INTO DATA(json).

      DATA(oref_json_ui2_cl_json_tests) = NEW lcl_json_parser_ui2cljson( ).
      oref_json_ui2_cl_json_tests->template_method( data = json out = out ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
