"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class is used for testing authorization checks within an ABAP Unit testing context. It defines an enumerated type for
"! different activities and executes an AUTHORITY-CHECK against a demo authorization object. The test class handles various authorization
"! scenarios and verifies expected outcomes based on user authorizations.
CLASS zcl_demo_aunit_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM enum_field_value,
             create,
             change,
             display,
             delete,
           END OF ENUM enum_field_value.

    "! <p class="shorttext synchronized" lang="en">Checks user authorization for the specified field value</p>
    "!
    "! @parameter field_value   | <p class="shorttext synchronized" lang="en">Activity enum value to check authorization for</p>
    "! @parameter is_authorized | <p class="shorttext synchronized" lang="en">Boolean indicating if the authorization check was successful</p>
    METHODS call_authority_check IMPORTING field_value          TYPE enum_field_value
                                 RETURNING VALUE(is_authorized) TYPE abap_boolean.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_auth IMPLEMENTATION.
  METHOD call_authority_check.
    DATA(val) = SWITCH #( field_value WHEN create THEN '01'
                                      WHEN change THEN '02'
                                      WHEN display THEN '03'
                                      WHEN delete THEN '06' ).

    AUTHORITY-CHECK OBJECT 'ZAUTH_OB'
      ID 'ACTVT' FIELD val.

    IF sy-subrc = 0.
      is_authorized = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
