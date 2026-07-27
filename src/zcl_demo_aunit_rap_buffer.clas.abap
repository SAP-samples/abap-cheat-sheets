"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class demonstrates Unit tests for RAP BO interaction logic using the RAP transaction buffer test double framework.
"! It shows testing ABAP EML read behavior using a transaction buffer test double framework without productive data.
CLASS zcl_demo_aunit_rap_buffer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    TYPES flights TYPE TABLE FOR READ RESULT zraunitflights.
    TYPES keys TYPE TABLE FOR READ IMPORT zraunitflights.
    TYPES failed_resp TYPE RESPONSE FOR FAILED zraunitflights.

    "! <p class="shorttext synchronized" lang="en">Adapts flight Planetype based on Seatsmax values</p>
    "!
    "! @parameter failed_resp | <p class="shorttext synchronized" lang="en">Response for any failures during read operations</p>
    "! @parameter flights     | <p class="shorttext synchronized" lang="en">Table of flights retrieved from the entity</p>
    "! @parameter keys        | <p class="shorttext synchronized" lang="en">Keys to identify the flights to read</p>
    METHODS adapt_planetype IMPORTING VALUE(keys) TYPE keys
                            EXPORTING flights     TYPE flights
                                      failed_resp TYPE failed_resp.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_rap_buffer IMPLEMENTATION.
  METHOD adapt_planetype.
    READ ENTITY zraunitflights
      ALL FIELDS WITH keys
      RESULT flights
      FAILED failed_resp.

    LOOP AT flights REFERENCE INTO DATA(flight).
      flight->Planetype = COND #( WHEN flight->Seatsmax < 100 THEN 'A'
                                  WHEN flight->Seatsmax BETWEEN 100 AND 200 THEN 'B'
                                  ELSE 'C' ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
