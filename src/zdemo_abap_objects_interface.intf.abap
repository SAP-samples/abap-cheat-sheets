"! <p class="shorttext"><strong>Interface supporting the ABAP object orientation example class</strong>
"! <br/>ABAP cheat sheet example interface</p>
"!
"! <h2>Note</h2>
"! <p>Find the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
INTERFACE zdemo_abap_objects_interface
  PUBLIC.

  METHODS: double IMPORTING i_op            TYPE i
                  RETURNING VALUE(r_double) TYPE i,

    triple DEFAULT IGNORE IMPORTING i_op            TYPE i
                          RETURNING VALUE(r_triple) TYPE i .

  DATA in_str TYPE string.

  CLASS-METHODS: halve IMPORTING i_op           TYPE i
                       RETURNING VALUE(r_halve) TYPE i.

  CLASS-DATA: stat_str TYPE string.

  CONSTANTS const_intf TYPE i VALUE 987.

  TYPES c3 TYPE c LENGTH 3.
  DATA add_result TYPE i.
  CLASS-DATA: subtr_result TYPE i.
  METHODS addition IMPORTING num1 TYPE i
                             num2 TYPE i.
  CLASS-METHODS subtraction IMPORTING num1 TYPE i
                                      num2 TYPE i.

  METHODS meth_ignore DEFAULT IGNORE RETURNING VALUE(int) TYPE i.
  METHODS meth_fail DEFAULT FAIL RETURNING VALUE(int) TYPE i.

ENDINTERFACE.
