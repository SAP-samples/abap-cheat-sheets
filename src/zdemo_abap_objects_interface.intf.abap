***********************************************************************
* ---------------------------- PURPOSE --------------------------------
* Interface to support the ABAP cheat sheet ABAP object orientation.
*
* ----------------------------- NOTE ----------------------------------
* The code presented in this class is only meant for supporting the ABAP
* cheat sheets. It is not intended for direct use in a
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
"! <p class="shorttext synchronized">Interface for ABAP cheat sheet example</p>
"! The interface supporta the ABAP cheat sheet on object orientation and demonstrates the use of interfaces.
INTERFACE zdemo_abap_objects_interface
  PUBLIC.

  METHODS: double IMPORTING i_op            TYPE i
                  RETURNING VALUE(r_double) TYPE i,

           triple DEFAULT IGNORE IMPORTING i_op TYPE i
                  RETURNING VALUE(r_triple)     TYPE i .

  DATA in_str TYPE string.

  CLASS-METHODS: halve IMPORTING i_op           TYPE i
                       RETURNING VALUE(r_halve) TYPE i.

  CLASS-DATA: stat_str TYPE string.

  CONSTANTS const_intf TYPE i VALUE 987.

ENDINTERFACE.
