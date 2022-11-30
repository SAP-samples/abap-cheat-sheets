***********************************************************************
*
*               Class for ABAP cheat sheet example
*
* ----------------------------- NOTE -----------------------------------
* The code presented in this class is only meant for supporting the ABAP
* cheat sheets in this repository. It is not intended for direct use in a
* production system environment. The code examples in the ABAP cheat
* sheets are primarily intended to provide a better explanation and
* visualization of the syntax and semantics of ABAP statements and not to
* solve concrete programming tasks. For production application programs,
* a dedicated solution should therefore always be worked out for each
* individual case. There is no guarantee for either the correctness or
* the completeness of the code. In addition, there is no legal
* responsibility or liability for possible errors or their consequences
* which occur through the use of the example code.
*
***********************************************************************
 "! <p class="shorttext synchronized">Class for ABAP cheat sheet example</p>
 "! The class supports the ABAP cheat sheet on object orientation and demonstrates the concept of friendship.
 CLASS zcl_demo_abap_objects_friend DEFINITION PUBLIC FINAL CREATE PUBLIC.

   PUBLIC SECTION.
     CLASS-METHODS get_strings RETURNING VALUE(res_string) TYPE string_table.

   PROTECTED SECTION.
   PRIVATE SECTION.
 ENDCLASS.



 CLASS zcl_demo_abap_objects_friend IMPLEMENTATION.


   METHOD get_strings.
     "Getting the strings and put them in the string table
     APPEND zcl_demo_abap_objects=>public_string TO res_string.
     APPEND zcl_demo_abap_objects=>protected_string TO res_string.
     APPEND zcl_demo_abap_objects=>private_string TO res_string.
   ENDMETHOD.
 ENDCLASS.
