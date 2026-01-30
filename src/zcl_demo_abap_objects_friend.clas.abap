"! <p class="shorttext"><strong>Class supporting the ABAP object orientation example</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class supports the ABAP object orientation example.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <p>Find the <strong>disclaimer</strong> in the ABAP Doc comment of class
"! {@link zcl_demo_abap_aux}.</p>
 CLASS zcl_demo_abap_objects_friend DEFINITION PUBLIC FINAL CREATE PUBLIC.

   PUBLIC SECTION.
     CLASS-METHODS get_strings RETURNING VALUE(res_string) TYPE string_table.

protected section.
private section.
ENDCLASS.



CLASS ZCL_DEMO_ABAP_OBJECTS_FRIEND IMPLEMENTATION.


  METHOD get_strings.
    "Getting the strings and adding them to the string table.
    "Accessing an attribute in a public visibility section.
    APPEND zcl_demo_abap_objects=>public_string TO res_string.

    "Accessing an attribute in a protected visibility section.
    APPEND zcl_demo_abap_objects=>protected_string TO res_string.

    "Accessing an attribute in a private visibility section.
    APPEND zcl_demo_abap_objects=>private_string TO res_string.
  ENDMETHOD.
ENDCLASS.
