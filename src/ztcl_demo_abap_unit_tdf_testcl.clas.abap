"! <p class="shorttext"><strong>Test Class Supporting an ABAP Unit Test Example</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class {@link zcl_demo_abap_unit_tdf} demonstrates managing dependencies (dependent-on-components, DOC)
"! with ABAP Unit and explores the creation of test doubles using ABAP frameworks.<br/>
"! This global class is empty, but the the <em>Test Classes</em> tab includes a test class. Using the syntax
"! <em>&quot;! &#64;testing ...</em>, a test relation between this test class and another global class
"! ({@link zcl_demo_abap_unit_tdf}) is defined. Therefore, when you run unit tests of class {@link zcl_demo_abap_unit_tdf},
"! the tests contained in the class {@link ztcl_demo_abap_unit_tdf_testcl} are executed.</p>
"!
"! <h2>Information</h2>
"! <p>Find information on getting started with the example class and the disclaimer in
"! the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
CLASS ztcl_demo_abap_unit_tdf_testcl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ztcl_demo_abap_unit_tdf_testcl IMPLEMENTATION.

ENDCLASS.
