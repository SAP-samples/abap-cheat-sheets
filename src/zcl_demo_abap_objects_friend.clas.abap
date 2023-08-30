***********************************************************************
*
*               Class supporting an ABAP cheat sheet example
*
* ----------------------------- NOTE -----------------------------------
* The code presented in this class is intended only to support the ABAP
* cheat sheets. It is not intended for direct use in a production system
* environment. The code examples in the ABAP cheat sheets are primarily
* intended to provide a better explanation and visualization of the
* syntax and semantics of ABAP statements, not to solve concrete
* programming tasks. For production application programs, you should
* always work out your own solution for each individual case. There is
* no guarantee for the correctness or completeness of the code.
* Furthermore, there is no legal responsibility or liability for any
* errors or their consequences that may occur when using the the example
* code.
*
***********************************************************************
 "! <p class="shorttext synchronized">Class supporting ABAP cheat sheet example</p>
 "! The class supports the ABAP cheat sheet example about object orientation.
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
