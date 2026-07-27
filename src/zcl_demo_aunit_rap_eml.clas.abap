"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class demonstrates ABAP Unit tests with ABAP EML requests. It shows mocking of ABAP EML requests for both read and modify
"! operations. The class provides wrapper methods for demonstration purposes.
CLASS zcl_demo_aunit_rap_eml DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    TYPES flights TYPE TABLE FOR READ RESULT zraunitflights.
    TYPES create_tab TYPE TABLE FOR CREATE zraunitflights.
    TYPES failed_resp TYPE RESPONSE FOR FAILED zraunitflights.
    TYPES mapped_resp TYPE RESPONSE FOR MAPPED zraunitflights.
    TYPES read_import_tab TYPE TABLE FOR READ IMPORT zraunitflights.
    TYPES read_result_tab TYPE TABLE FOR READ RESULT zraunitflights.

    "! Modifies entity data based on input keys with mapped and failed responses
    "!
    "! @parameter failed_resp | <p class="shorttext synchronized" lang="en">Response for failures</p>
    "! @parameter keys        | <p class="shorttext synchronized" lang="en">Keys for the entity to modify</p>
    "! @parameter mapped_resp | <p class="shorttext synchronized" lang="en">Mapped response</p>
    METHODS demo_eml_modify IMPORTING VALUE(keys) TYPE create_tab
                            EXPORTING mapped_resp TYPE mapped_resp
                                      failed_resp TYPE failed_resp.

    "! Reads entity data using specified keys and returns the result
    "!
    "! @parameter failed_resp | <p class="shorttext synchronized" lang="en">Response for failures</p>
    "! @parameter keys        | <p class="shorttext synchronized" lang="en">Keys identifying the entity to read</p>
    "! @parameter read_result | <p class="shorttext synchronized" lang="en">Results of the read operation</p>
    METHODS demo_eml_read IMPORTING VALUE(keys) TYPE read_import_tab
                          EXPORTING read_result TYPE read_result_tab
                                    failed_resp TYPE failed_resp.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_rap_eml IMPLEMENTATION.
  METHOD demo_eml_modify.
    MODIFY ENTITY zraunitflights
     CREATE FIELDS ( carrid connid fldate seatsmax seatsocc )
     WITH keys
     MAPPED mapped_resp
     FAILED failed_resp.
  ENDMETHOD.

  METHOD demo_eml_read.
    READ ENTITY zraunitflights
     ALL FIELDS WITH keys
     RESULT read_result
     FAILED failed_resp.
  ENDMETHOD.
ENDCLASS.
