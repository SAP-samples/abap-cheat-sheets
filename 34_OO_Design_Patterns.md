<a name="top"></a>

# ABAP Examples Using Object-Oriented Design Patterns

- [ABAP Examples Using Object-Oriented Design Patterns](#abap-examples-using-object-oriented-design-patterns)
  - [Example Setup and Running Example Classes](#example-setup-and-running-example-classes)
  - [ABAP Unit: Backdoor Injection](#abap-unit-backdoor-injection)
  - [ABAP Unit: Constructor Injection](#abap-unit-constructor-injection)
  - [ABAP Unit: Parameter Injection](#abap-unit-parameter-injection)
  - [ABAP Unit: Setter Injection](#abap-unit-setter-injection)
  - [ABAP Unit: Test Double Injection Using Inheritance and Method Redefinition](#abap-unit-test-double-injection-using-inheritance-and-method-redefinition)
  - [Abstract Factory](#abstract-factory)
  - [Adapter](#adapter)
  - [Builder](#builder)
  - [Chain of Responsibility](#chain-of-responsibility)
  - [Command](#command)
  - [Decorator](#decorator)
  - [Facade](#facade)
  - [Factory Method](#factory-method)
  - [Fluent Interface](#fluent-interface)
  - [Flyweight](#flyweight)
  - [Memento](#memento)
  - [Multiton](#multiton)
  - [Observer](#observer)
  - [Prototype](#prototype)
  - [Proxy](#proxy)
  - [Singleton](#singleton)
  - [State](#state)
  - [Strategy](#strategy)
  - [Template Method](#template-method)
  - [Visitor](#visitor)


This ABAP cheat sheet includes a selection of design patterns you may have come across in object-oriented programming. It serves as an excursion. Unlike other ABAP cheat sheets, it does not cover ABAP-specific topics as such. Instead, it includes ABAP code explorations and experiments related to various design patterns.

Design patterns address common software design challenges, aiming to improve modularity, scalability, reusability, and more.
The examples largely draw inspiration from design patterns established by the _Gang of Four_ (GoF), applicable across different object-oriented programming languages. Apart from selected, renowned GoF patterns, the sheet also includes a random selection of other techniques.

> [!IMPORTANT]
> - The focus of the examples in the ABAP cheat sheet is on basic conceptual considerations regarding the design patterns, and experimenting with ABAP syntax and concepts (such as interfaces, abstract classes, encapsulation, and more). The examples aim to illustrate basic design pattern ideas using simplified ABAP demo classes. 
> - The examples are meant for [exploration, experimentation, and demonstration](./README.md#%EF%B8%8F-disclaimer), using non-semantic and non-real-world contexts to reduce complexity and give a rough idea. Due to their experimental nature, these examples do not represent best practices or model approaches, as various approaches and class setup strategies may apply. It is up to you to evaluate whether the patterns are suitable and beneficial for your setup. Always create your own solutions.
> - To further explore the patterns, many publications, articles, and community resources offer in-depth insights into their origins, purposes, real-world examples, similarities, differences, combinations of patterns, pros, cons, and more.


## Example Setup and Running Example Classes

Most examples are set up for easy exploration using simple, (mostly) self-contained ABAP classes, i.e. there is only one class pool including local classes/interfaces instead of multiple global classes/interfaces:
- Global class (_Global Class_ tab in ADT):
  - Includes the `if_oo_adt_classrun` interface to run the class with F9 in ADT.
  - Serves as the client that makes use of the local classes to demonstrate design patterns. Largely, the declarations and implementations in the CCIMP include are relevant for the conceptual considerations.
- CCIMP include (_Local Types_ tab in ADT):
  - Contains various local classes/interfaces to demonstrate design patterns, allowing quick copying and pasting without creating multiple global classes.

In the following sections, click the expandable sections for further descriptions and example code. To try the examples out, create a demo class named `zcl_demo_abap` and paste the code into it (*Global Class* and *Local Types* tabs in ADT). After activation, choose *F9* in ADT to execute the class. The examples are set up to display output in the console.

> [!NOTE]
> - Some examples use artifacts such as database tables from the ABAP cheat sheet GitHub repository.
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)


<p align="right"><a href="#top">⬆️ back to top</a></p>




## ABAP Unit: Backdoor Injection

- The backdoor injection technique can be applied in the context of ABAP Unit.
- As described [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/04a2d0fc9cd940db8aedf3fa29e5f07e.html?locale=en-US), multiple techniques exist for injecting test doubles into a class under test to ensure their usage during test execution. Also find more information in the ABAP Unit Tests cheat sheet.
- A sample setup to inject test doubles, as illustrated in the example, may look as follows:
  - A dependent-on component (DOC) is identified (for example, reading data from a database table).
  - Ideally, an interface to the DOC is available (here, a local interface is used). A data provider class implements this interface for the data provisioning (here, a local class is used).
  - The class under test has been prepared. This example uses:
    - A private instance attribute declared with a reference to the interface.
    - The method to be tested calls a method via this interface reference variable for the data retrieval.
    - The class's instance contructor assigns the interface reference variable by creating an object of the data provider class, which implements the interface.
  - The test class may be implemented as follows:
    - Since private attributes are not accessible in local test classes, the local test class is declared as a local friend of the global class.
    - In the test method, an object of the class under test and a test double are created. The test double is declared with a reference to the interface (here, a local class is used that creates a test double). The private attribute is then assigned the test double, realizing the backdoor injection.
    - The effect is as follows: When the method to be tested is called "regularly" (for example, when executing the class using _F9_, as in the example case), the object refers to the "regular" data provider (in this example, it is of type `lcl_data_provider`), which means it uses production data. However, when the unit test is executed (for example, by choosing _Ctrl/Cmd + Shift + F10_), the object refers to the test double (in this case, of type `ltd_test_data`), which is the local test double that gets injected.



<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

Unlike other examples, this example class has a different setup: 
- It focuses on ABAP Unit tests. You can indeed execute the simple example class by choosing _F9_. To run the unit test, use _Ctrl/Cmd + Shift + F10_.
- The example class includes code from the global class and the CCIMP include, as well as the CCDEF (_Class-relevant Local Types_ tab in ADT) and CCAU (_Test Classes_ tab in ADT) includes.
- The example uses a demo database table (and a class to populate it) from the ABAP Cheat Sheet GitHub repository.

<br>

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: carr_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY,
           occ_rate TYPE p LENGTH 5 DECIMALS 2.

    CLASS-METHODS class_constructor.

    METHODS: constructor,
      "Method to demonstrate test double injection using back door injection
      get_occ_rate IMPORTING carrier_id            TYPE zdemo_abap_fli-carrid
                   RETURNING VALUE(occupancy_rate) TYPE occ_rate.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA data_provider TYPE REF TO lif_get_data.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Filling an internal table with carrier IDs used to calculate
    "the occupancy rate.
    DATA(carrier_tab) = VALUE carr_tab( ( carrid = 'LH' )
                                        ( carrid = 'AA' )
                                        ( carrid = 'DL' ) ).

    LOOP AT carrier_tab INTO DATA(carr).
      DATA(oc_rate) = get_occ_rate( carr-carrid ).

      out->write( |The occupancy rate for airline { carr-carrid } is { oc_rate }%.|  ).
      out->write( |\n| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.

  METHOD get_occ_rate.

    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.

    "When the method to be tested is called "regularly" (for example, when executing the class using F9),
    "the object refers to the "regular" data provider (type ref to lcl_data_provider), which means it uses
    "production data. However, when the unit test is executed (for example, by choosing Ctrl/Cmd + Shift + F10),
    "the object refers to the test double (type ref to ltd_test_data), which is the local test double that gets
    "injected.
    DATA(flight_data) = data_provider->select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data INTO DATA(wa).
      total_seatsmax = total_seatsmax + wa-seatsmax.
      total_seatsocc = total_seatsocc + wa-seatsocc.
    ENDLOOP.

    occupancy_rate = total_seatsocc / total_seatsmax * 100.
  ENDMETHOD.

  METHOD constructor.
    data_provider = NEW lcl_data_provider( ).
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCDEF include (Class-relevant Local Types tab in ADT)

 </td>

 <td> 

``` abap
INTERFACE lif_get_data.
  METHODS:
    select_flight_data IMPORTING carrier            TYPE zdemo_abap_fli-carrid
                       RETURNING VALUE(flight_data) TYPE zcl_demo_abap=>carr_tab.
ENDINTERFACE.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
CLASS lcl_data_provider DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_get_data.
ENDCLASS.

CLASS lcl_data_provider IMPLEMENTATION.
  METHOD lif_get_data~select_flight_data.
    SELECT seatsmax, seatsocc
      FROM zdemo_abap_fli
      WHERE carrid = @carrier
      INTO CORRESPONDING FIELDS OF TABLE @flight_data.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCAU include (Test Classes tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Local test double class
*&---------------------------------------------------------------------*

"The example uses manually created test doubles. You may also want to
"check out creating test doubles using ABAP frameworks. Find more
"information in the ABAP Unit Tests cheat sheet.

CLASS ltd_test_data DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_get_data PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd_test_data IMPLEMENTATION.
  METHOD lif_get_data~select_flight_data.

    flight_data = SWITCH #( carrier
      WHEN 'IJ' THEN VALUE #( ( carrid = carrier  seatsmax = 300 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 300 ) )
      WHEN 'KL' THEN VALUE #( ( carrid = carrier  seatsmax = 350 seatsocc = 300 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 250 )
                              ( carrid = carrier  seatsmax = 300 seatsocc = 250 ) ) ).

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Test class demonstrating backdoor injection
*&---------------------------------------------------------------------*

"Note that the example uses a local interface.

CLASS ltc_test DEFINITION
  FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_get_occ_rate FOR TESTING.
ENDCLASS.

"In this example, a test class is created in the test include. Since private
"attributes are not accessible in local test classes, the local test class
"is declared as a local friend of the global class.
CLASS zcl_demo_abap DEFINITION LOCAL FRIENDS ltc_test.

CLASS ltc_test IMPLEMENTATION.
  METHOD test_get_occ_rate.
    DATA(ref_cut) = NEW zcl_demo_abap( ).
    DATA ref_data_prov TYPE REF TO lif_get_data.
    ref_data_prov = NEW ltd_test_data( ).
    ref_cut->data_provider = ref_data_prov.

    DATA(act_occ_rate) = ref_cut->get_occ_rate( 'IJ' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '70.00'
        msg = |The expected occupancy rate for carrier 'IJ' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).

    act_occ_rate = ref_cut->get_occ_rate( 'KL' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '80.00'
        msg = |The expected occupancy rate for carrier 'KL' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## ABAP Unit: Constructor Injection

- The constructor injection technique can be applied in the context of ABAP Unit.
- As described [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/04a2d0fc9cd940db8aedf3fa29e5f07e.html?locale=en-US), multiple techniques exist for injecting test doubles into a class under test to ensure their usage during test execution. Also find more information in the ABAP Unit Tests cheat sheet.
- Here, the test double is passed as a parameter to the instance constructor of the class under test.
- A sample setup to inject test doubles, as illustrated in the example, may look as follows:
  - A dependent-on component (DOC) is identified (for example, reading data from a database table).
  - Ideally, an interface to the DOC is available. A data provider class implements this interface for the data provisioning.
  - The class under test has been prepared. This example uses:
    - A private instance attribute declared with a reference to the interface.
    - The method to be tested calls a method via this interface reference variable for the data retrieval.
    - The class's instance contructor is declared with an optional parameter referencing the interface. When an object of the class is created, the interface reference variable is assigned appropriately, i.e. using the "regular" data provider or the test double.
  - The test class may be implemented as follows:
    - Since private attributes are not accessible in local test classes, the local test class is declared as a local friend of the global class.
    - In the test method, an object of the class under test and a test double are created. The test double is declared with a reference to the interface (here, a local class is used that creates a test double). By creating an object of the class under test, the test double is passed as parameter of the instance constructor. 
    - The effect is as follows: When the method to be tested is called "regularly" (for example, when executing the class using _F9_, as in the example case), the object refers to the "regular" data provider (in this example, it is of type `lcl_data_provider`), which means it uses production data (an object is created in the instance constrctor implementation accordingly). However, when the unit test is executed (for example, by choosing _Ctrl/Cmd + Shift + F10_), the object refers to the test double (in this case, of type `ltd_test_data`), which is the local test double that gets injected via the constructor. In the instance constructor implementation, the instance attribute is assigned the reference to the test double.


<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

Unlike other examples, this example class has a different setup: 
- It focuses on ABAP Unit tests. You can indeed execute the simple example class by choosing _F9_. To run the unit test, use _Ctrl/Cmd + Shift + F10_.
- The example class includes code from the global class, the CCIMP and CCAU (_Test Classes_ tab in ADT) includes. 
- The example uses a demo database table (and a class to populate it) and a global interface from the ABAP cheat sheet GitHub repository.

<br>

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: carr_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY,
           occ_rate TYPE p LENGTH 5 DECIMALS 2.

    CLASS-METHODS class_constructor.
    METHODS: constructor IMPORTING iref_data_prov TYPE REF TO zdemo_abap_get_data_itf OPTIONAL,
      get_occ_rate IMPORTING carrier_id            TYPE zdemo_abap_fli-carrid
                   RETURNING VALUE(occupancy_rate) TYPE occ_rate.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA data_provider TYPE REF TO zdemo_abap_get_data_itf.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Filling an internal table with carrier IDs used to calculate
    "the occupancy rate.
    DATA(carrier_tab) = VALUE carr_tab( ( carrid = 'LH' )
                                        ( carrid = 'AA' )
                                        ( carrid = 'DL' ) ).

    LOOP AT carrier_tab INTO DATA(carr).
      DATA(oc_rate) = get_occ_rate( carr-carrid ).

      out->write( |The occupancy rate for airline { carr-carrid } is { oc_rate }%.|  ).
      out->write( |\n| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.

  METHOD get_occ_rate.
    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.

    DATA(flight_data) = data_provider->select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data INTO DATA(wa).
      total_seatsmax = total_seatsmax + wa-seatsmax.
      total_seatsocc = total_seatsocc + wa-seatsocc.
    ENDLOOP.

    occupancy_rate = total_seatsocc / total_seatsmax * 100.
  ENDMETHOD.

  METHOD constructor.
    IF iref_data_prov IS BOUND.
      data_provider = iref_data_prov.
    ELSE.
      data_provider = NEW lcl_data_provider( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>


<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
CLASS lcl_data_provider DEFINITION.
  PUBLIC SECTION.
    INTERFACES zdemo_abap_get_data_itf.
ENDCLASS.

CLASS lcl_data_provider IMPLEMENTATION.
  METHOD zdemo_abap_get_data_itf~select_flight_data.
    SELECT seatsmax, seatsocc
      FROM zdemo_abap_fli
      WHERE carrid = @carrier
      INTO CORRESPONDING FIELDS OF TABLE @flight_data.
  ENDMETHOD.

  METHOD zdemo_abap_get_data_itf~say_hello.
   "The implementation is not relevant for the example.
   "See the executable example of the ABAP Unit Tests cheat sheet.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCAU include (Test Classes tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Local test double class
*&---------------------------------------------------------------------*

"The example uses manually created test doubles. You may also want to
"check out creating test doubles using ABAP frameworks. Find more
"information in the ABAP Unit Tests cheat sheet.

CLASS ltd_test_data DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zdemo_abap_get_data_itf PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd_test_data IMPLEMENTATION.
  METHOD zdemo_abap_get_data_itf~select_flight_data.

    flight_data = SWITCH #( carrier
      WHEN 'IJ' THEN VALUE #( ( carrid = carrier  seatsmax = 300 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 300 ) )
      WHEN 'KL' THEN VALUE #( ( carrid = carrier  seatsmax = 350 seatsocc = 300 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 250 )
                              ( carrid = carrier  seatsmax = 300 seatsocc = 250 ) ) ).

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Test class demonstrating constructor injection
*&---------------------------------------------------------------------*

"Note that the example uses a local interface.

CLASS ltc_test DEFINITION
  FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_get_occ_rate FOR TESTING.
ENDCLASS.

"In this example, a test class is created in the test include. Since private
"attributes are not accessible in local test classes, the local test class
"is declared as a local friend of the global class.
CLASS zcl_demo_abap DEFINITION LOCAL FRIENDS ltc_test.

CLASS ltc_test IMPLEMENTATION.
  METHOD test_get_occ_rate.
    DATA ref_data_prov TYPE REF TO zdemo_abap_get_data_itf.

    "Creating an instance of the local test double
    ref_data_prov = NEW ltd_test_data( ).

    "Instance is provided for the constructor injection
    DATA(ref_cut) = NEW zcl_demo_abap( ref_data_prov ).

    DATA(act_occ_rate) = ref_cut->get_occ_rate( carrier_id = 'IJ' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '70.00'
        msg = |The expected occupancy rate for carrier 'IJ' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).

    act_occ_rate = ref_cut->get_occ_rate( carrier_id = 'KL' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '80.00'
        msg = |The expected occupancy rate for carrier 'KL' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## ABAP Unit: Parameter Injection

- The parameter injection technique can be applied in the context of ABAP Unit.
- As described [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/04a2d0fc9cd940db8aedf3fa29e5f07e.html?locale=en-US), multiple techniques exist for injecting test doubles into a class under test to ensure their usage during test execution. Also find more information in the ABAP Unit Tests cheat sheet.
- Here, the test double is passed as a parameter to the tested method (i.e. an optional importing parameter) in the class under test.
- A sample setup to inject test doubles, as illustrated in the example, may look as follows:
  - A dependent-on component (DOC) is identified (for example, reading data from a database table).
  - Ideally, an interface to the DOC is available (here, a local interface is used). A data provider class implements this interface for the data provisioning (here, a local class is used).
  - The class under test has been prepared. The method to be tested:        
    - Calls a method via an interface reference variable for the data retrieval.
    - Is defined with an optional parameter. It is typed with reference to the interface. 
    - Includes a check whether the optional parameter is bound.
    - Note: To use the local demo interface in the signature, the method is defined in the private visibility section.
  - The test class may be implemented as follows:
    - Since private attributes are not accessible in local test classes, the local test class is declared as a local friend of the global class.
    - In the test method, an object of the class under test and a test double are created. The test double is declared with a reference to the interface (here, a local class is used that creates a test double). 
    - The effect is as follows: When the method to be tested is called "regularly" (for example, when executing the class using _F9_, as in the example case) without passing the optional parameter, an object of the "regular" data provider (in this example, it is of type `lcl_data_provider`) is created following the evaluation of the `IF` statement, which means it uses production data. However, when the unit test is executed (for example, by choosing _Ctrl/Cmd + Shift + F10_) and the optional parameter is supplied, the parameter is bound and, thus, uses the test double (in this case, of type `ltd_test_data`), which gets injected via the parameter.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

Unlike other examples, this example class has a different setup: 
- It focuses on ABAP Unit tests. You can indeed execute the simple example class by choosing _F9_. To run the unit test, use _Ctrl/Cmd + Shift + F10_.
- The example class includes code from the global class and the CCIMP include, as well as the CCDEF (_Class-relevant Local Types_ tab in ADT) and CCAU (_Test Classes_ tab in ADT) includes.
- The example uses a demo database table (and a class to populate it) from the ABAP Cheat Sheet GitHub repository.

<br>

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: carr_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY,
           occ_rate TYPE p LENGTH 5 DECIMALS 2.

    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    "Method to demonstrate test double injection using parameter injection
    "The example uses a private method to refer to a local interface.
    METHODS  get_occ_rate IMPORTING carrier_id            TYPE zdemo_abap_fli-carrid
                                    data_prov             TYPE REF TO lif_get_data OPTIONAL
                          RETURNING VALUE(occupancy_rate) TYPE occ_rate.

    " DATA data_provider TYPE REF TO lif_get_data.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Filling an internal table with carrier IDs used to calculate
    "the occupancy rate.
    DATA(carrier_tab) = VALUE carr_tab( ( carrid = 'LH' )
                                        ( carrid = 'AA' )
                                        ( carrid = 'DL' ) ).

    LOOP AT carrier_tab INTO DATA(carr).
      DATA(oc_rate) = get_occ_rate( carr-carrid ).

      out->write( |The occupancy rate for airline { carr-carrid } is { oc_rate }%.|  ).
      out->write( |\n| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.

  METHOD get_occ_rate.

    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.
    DATA data_provider TYPE REF TO lif_get_data.

    "The method has an optional importing parameter. When the unit test is executed,
    "the parameter is bound. An object of the test double class is passed in that case.
    "Otherwise, when the class is executed using F9, an object of the actual data provider
    "is created.
    IF data_prov IS BOUND.
      data_provider = data_prov.
    ELSE.
      data_provider = NEW lcl_data_provider( ).
    ENDIF.

    DATA(flight_data) = data_provider->select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data INTO DATA(wa).
      total_seatsmax = total_seatsmax + wa-seatsmax.
      total_seatsocc = total_seatsocc + wa-seatsocc.
    ENDLOOP.

    occupancy_rate = total_seatsocc / total_seatsmax * 100.
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCDEF include (Class-relevant Local Types tab in ADT)

 </td>

 <td> 

``` abap
INTERFACE lif_get_data.
  METHODS:
    select_flight_data IMPORTING carrier            TYPE zdemo_abap_fli-carrid
                       RETURNING VALUE(flight_data) TYPE zcl_demo_abap=>carr_tab.
ENDINTERFACE.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
CLASS lcl_data_provider DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_get_data.
ENDCLASS.

CLASS lcl_data_provider IMPLEMENTATION.
  METHOD lif_get_data~select_flight_data.
    SELECT seatsmax, seatsocc
      FROM zdemo_abap_fli
      WHERE carrid = @carrier
      INTO CORRESPONDING FIELDS OF TABLE @flight_data.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCAU include (Test Classes tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Local test double class
*&---------------------------------------------------------------------*

"The example uses manually created test doubles. You may also want to
"check out creating test doubles using ABAP frameworks. Find more
"information in the ABAP Unit Tests cheat sheet.

CLASS ltd_test_data DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_get_data PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd_test_data IMPLEMENTATION.
  METHOD lif_get_data~select_flight_data.

    flight_data = SWITCH #( carrier
      WHEN 'IJ' THEN VALUE #( ( carrid = carrier  seatsmax = 300 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 300 ) )
      WHEN 'KL' THEN VALUE #( ( carrid = carrier  seatsmax = 350 seatsocc = 300 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 250 )
                              ( carrid = carrier  seatsmax = 300 seatsocc = 250 ) ) ).

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Test class demonstrating parameter injection
*&---------------------------------------------------------------------*

"Note that the example uses a local interface.

CLASS ltc_test DEFINITION
  FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_get_occ_rate FOR TESTING.
ENDCLASS.

"In this example, a test class is created in the test include. Since private
"attributes are not accessible in local test classes, the local test class
"is declared as a local friend of the global class.
CLASS zcl_demo_abap DEFINITION LOCAL FRIENDS ltc_test.

CLASS ltc_test IMPLEMENTATION.
  METHOD test_get_occ_rate.
    DATA(ref_cut) = NEW zcl_demo_abap( ).
    DATA ref_data_prov TYPE REF TO lif_get_data.
    ref_data_prov = NEW ltd_test_data( ).

    DATA(act_occ_rate) = ref_cut->get_occ_rate( carrier_id = 'IJ'
                                                data_prov  = ref_data_prov ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '70.00'
        msg = |The expected occupancy rate for carrier 'IJ' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).

    act_occ_rate = ref_cut->get_occ_rate( carrier_id = 'KL'
                                          data_prov  = ref_data_prov ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '80.00'
        msg = |The expected occupancy rate for carrier 'KL' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>    

<p align="right"><a href="#top">⬆️ back to top</a></p>


## ABAP Unit: Setter Injection 

- The setter injection technique can be applied in the context of ABAP Unit.
- As described [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/04a2d0fc9cd940db8aedf3fa29e5f07e.html?locale=en-US), multiple techniques exist for injecting test doubles into a class under test to ensure their usage during test execution. Also find more information in the ABAP Unit Tests cheat sheet.
- Here, the test double is passed as a parameter to a setter method.
- A sample setup to inject test doubles, as illustrated in the example, may look as follows:
  - A dependent-on component (DOC) is identified (for example, reading data from a database table).
  - Ideally, an interface to the DOC is available (here, a local interface is used). A data provider class implements this interface for the data provisioning (here, a local class is used).
  - The class under test has been prepared. This example uses:
    - A private instance attribute declared with a reference to the interface.
    - The method to be tested calls a method via this interface reference variable for the data retrieval.
    - The class's instance contructor assigns the interface reference variable by creating an object of the data provider class, which implements the interface.
    - A setter method is declared which expects a reference to the interface to be passed. To use the local demo interface in the signature, the setter method is defined in the private visibility section.
  - The test class may be implemented as follows:
    - Since private attributes are not accessible in local test classes, the local test class is declared as a local friend of the global class.
    - In the test method, an object of the class under test and a test double are created. The test double is declared with a reference to the interface (here, a local class is used that creates a test double). The setter method is called, passing the test double. Following the setter method implementation, the interface reference variable is assigned the test double.
    - The effect is as follows: When the method to be tested is called "regularly" (for example, when executing the class using _F9_, as in the example case), the object refers to the "regular" data provider (in this example, it is of type `lcl_data_provider`), which means it uses production data. However, when the unit test is executed (for example, by choosing _Ctrl/Cmd + Shift + F10_), the object refers to the test double (in this case, of type `ltd_test_data`), which is the local test double that gets injected via the setter method.

<br>

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

Unlike other examples, this example class has a different setup: 
- It focuses on ABAP Unit tests. You can indeed execute the simple example class by choosing _F9_. To run the unit test, use _Ctrl/Cmd + Shift + F10_.
- The example class includes code from the global class and the CCIMP include, as well as the CCDEF (_Class-relevant Local Types_ tab in ADT) and CCAU (_Test Classes_ tab in ADT) includes.
- The example uses a demo database table (and a class to populate it) from the ABAP Cheat Sheet GitHub repository.

<br>

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: carr_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY,
           occ_rate TYPE p LENGTH 5 DECIMALS 2.

    CLASS-METHODS class_constructor.
    METHODS: constructor,
            get_occ_rate IMPORTING carrier_id            TYPE zdemo_abap_fli-carrid
                         RETURNING VALUE(occupancy_rate) TYPE occ_rate.

  PROTECTED SECTION.
  PRIVATE SECTION.
    "Method for setter injection
    METHODS setter_meth IMPORTING data_prov TYPE REF TO lif_get_data.

    DATA data_provider TYPE REF TO lif_get_data.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Filling an internal table with carrier IDs used to calculate
    "the occupancy rate.
    DATA(carrier_tab) = VALUE carr_tab( ( carrid = 'LH' )
                                        ( carrid = 'AA' )
                                        ( carrid = 'DL' ) ).

    LOOP AT carrier_tab INTO DATA(carr).
      DATA(oc_rate) = get_occ_rate( carr-carrid ).

      out->write( |The occupancy rate for airline { carr-carrid } is { oc_rate }%.|  ).
      out->write( |\n| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.

  METHOD get_occ_rate.
    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.

    DATA(flight_data) = data_provider->select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data INTO DATA(wa).
      total_seatsmax = total_seatsmax + wa-seatsmax.
      total_seatsocc = total_seatsocc + wa-seatsocc.
    ENDLOOP.

    occupancy_rate = total_seatsocc / total_seatsmax * 100.
  ENDMETHOD.

  METHOD setter_meth.
    "When the unit test is executed, an object of the test double class is passed as
    "parameter. Then, the object used here refers the local test double.
    data_provider = data_prov.
  ENDMETHOD.

  METHOD constructor.
    data_provider = NEW lcl_data_provider( ).
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCDEF include (Class-relevant Local Types tab in ADT)

 </td>

 <td> 

``` abap
INTERFACE lif_get_data.
  METHODS:
    select_flight_data IMPORTING carrier            TYPE zdemo_abap_fli-carrid
                       RETURNING VALUE(flight_data) TYPE zcl_demo_abap=>carr_tab.
ENDINTERFACE.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
CLASS lcl_data_provider DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_get_data.
ENDCLASS.

CLASS lcl_data_provider IMPLEMENTATION.
  METHOD lif_get_data~select_flight_data.
    SELECT seatsmax, seatsocc
      FROM zdemo_abap_fli
      WHERE carrid = @carrier
      INTO CORRESPONDING FIELDS OF TABLE @flight_data.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCAU include (Test Classes tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Local test double class
*&---------------------------------------------------------------------*

"The example uses manually created test doubles. You may also want to
"check out creating test doubles using ABAP frameworks. Find more
"information in the ABAP Unit Tests cheat sheet.

CLASS ltd_test_data DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_get_data PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd_test_data IMPLEMENTATION.
  METHOD lif_get_data~select_flight_data.

    flight_data = SWITCH #( carrier
      WHEN 'IJ' THEN VALUE #( ( carrid = carrier  seatsmax = 300 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 300 ) )
      WHEN 'KL' THEN VALUE #( ( carrid = carrier  seatsmax = 350 seatsocc = 300 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 250 )
                              ( carrid = carrier  seatsmax = 300 seatsocc = 250 ) ) ).

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Test class demonstrating setter injection
*&---------------------------------------------------------------------*

"Note that the example uses a local interface.

CLASS ltc_test DEFINITION
  FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_get_occ_rate FOR TESTING.
ENDCLASS.

"In this example, a test class is created in the test include. Since private
"attributes are not accessible in local test classes, the local test class
"is declared as a local friend of the global class.
CLASS zcl_demo_abap DEFINITION LOCAL FRIENDS ltc_test.

CLASS ltc_test IMPLEMENTATION.
  METHOD test_get_occ_rate.
    DATA(ref_cut) = NEW zcl_demo_abap( ).
    DATA ref_data_prov TYPE REF TO lif_get_data.
    ref_data_prov = NEW ltd_test_data( ).
    "Passing the test double as a parameter of a setter method
    ref_cut->setter_meth( ref_data_prov ).

    DATA(act_occ_rate) = ref_cut->get_occ_rate( carrier_id = 'IJ' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '70.00'
        msg = |The expected occupancy rate for carrier 'IJ' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).

    act_occ_rate = ref_cut->get_occ_rate( carrier_id = 'KL' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '80.00'
        msg = |The expected occupancy rate for carrier 'KL' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>   

<p align="right"><a href="#top">⬆️ back to top</a></p>


## ABAP Unit: Test Double Injection Using Inheritance and Method Redefinition

- This ABAP Unit test double injection technique makes use of inheritance and redefining methods.
- Here, a local test double class is created by redefining a method of the class under test.
- A sample setup to inject test doubles, as illustrated in the example, may look as follows:
  - A dependent-on component (DOC) is identified (for example, reading data from a database table). Unlike the other ABAP Unit examples, there are no local or global interfaces involved.
  - The global class that includes a method to be tested is not declared as a final class, allowing for the creation of subclasses. For simplicity, this global class implements a method specifically for the test. You can imagine that a data retrieval method is contained in another global class, which the class under test uses.
  - The test class may be implemented as follows:
    - A test double class is created that inherits from the global class and redefines the method identified as the DOC, enabling the provisioning of test data for the test classes.
    - The test method does not instantiate the class under test. Instead, it creates an object of the test double class. The method to be tested is then called through the test double's reference variable.
    - The effect is as follows: When the method to be tested is called "regularly" (for example, by executing the class using _F9_), it runs as usual with production data. However, when the unit test is executed (for example, by choosing _Ctrl/Cmd + Shift + F10_), the redefined method in the test class is called, injecting and using the test double.

<br>

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

Unlike other examples, this example class has a different setup: 
- It focuses on ABAP Unit tests. You can indeed execute the simple example class by choosing _F9_. To run the unit test, use _Ctrl/Cmd + Shift + F10_.
- The example class includes code from the global class and the CCAU include (_Test Classes_ tab in ADT).
- The example uses a demo database table (and a class to populate it) from the ABAP Cheat Sheet GitHub repository.



<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: carr_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY,
           occ_rate TYPE p LENGTH 5 DECIMALS 2.

    CLASS-METHODS class_constructor.
    METHODS get_occ_rate IMPORTING carrier_id            TYPE zdemo_abap_fli-carrid
                         RETURNING VALUE(occupancy_rate) TYPE occ_rate.

  PROTECTED SECTION.
    METHODS select_flight_data IMPORTING carrier            TYPE zdemo_abap_fli-carrid
                               RETURNING VALUE(flight_data) TYPE carr_tab.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Filling an internal table with carrier IDs used to calculate
    "the occupancy rate.
    DATA(carrier_tab) = VALUE carr_tab( ( carrid = 'LH' )
                                        ( carrid = 'AA' )
                                        ( carrid = 'DL' ) ).

    LOOP AT carrier_tab INTO DATA(carr).
      DATA(oc_rate) = get_occ_rate( carr-carrid ).

      out->write( |The occupancy rate for airline { carr-carrid } is { oc_rate }%.|  ).
      out->write( |\n| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD class_constructor.
    "Filling demo database tables.
    zcl_demo_abap_aux=>fill_dbtabs( ).
  ENDMETHOD.

  METHOD get_occ_rate.
    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.

    "During the unit test, the redefined method in the test class is called.
    DATA(flight_data) = select_flight_data( carrier = carrier_id ).

    LOOP AT flight_data INTO DATA(wa).
      total_seatsmax = total_seatsmax + wa-seatsmax.
      total_seatsocc = total_seatsocc + wa-seatsocc.
    ENDLOOP.

    occupancy_rate = total_seatsocc / total_seatsmax * 100.
  ENDMETHOD.

  METHOD select_flight_data.
    SELECT seatsmax, seatsocc
      FROM zdemo_abap_fli
      WHERE carrid = @carrier
      INTO CORRESPONDING FIELDS OF TABLE @flight_data.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>



<tr>
<td> 

CCAU include (Test Classes tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Local test double class
*&---------------------------------------------------------------------*

"The example uses manually created test doubles. You may also want to
"check out creating test doubles using ABAP frameworks. Find more
"information in the ABAP Unit Tests cheat sheet.

CLASS ltd_test_data DEFINITION FOR TESTING
INHERITING FROM zcl_demo_abap.
  PROTECTED SECTION.
    METHODS select_flight_data REDEFINITION.
ENDCLASS.

CLASS ltd_test_data IMPLEMENTATION.
  METHOD select_flight_data.

    flight_data = SWITCH #( carrier
      WHEN 'IJ' THEN VALUE #( ( carrid = carrier  seatsmax = 300 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 200 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 300 ) )
      WHEN 'KL' THEN VALUE #( ( carrid = carrier  seatsmax = 350 seatsocc = 300 )
                              ( carrid = carrier  seatsmax = 350 seatsocc = 250 )
                              ( carrid = carrier  seatsmax = 300 seatsocc = 250 ) ) ).

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Test class demonstrating test double injection using inheritance
*& and method redefinition
*&---------------------------------------------------------------------*

CLASS ltc_test DEFINITION
  FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_get_occ_rate FOR TESTING.
ENDCLASS.

CLASS ltc_test IMPLEMENTATION.
  METHOD test_get_occ_rate.
    "Creating an object of the test double class that inherits
    "from the class under test
    DATA(ref_cut) = NEW ltd_test_data(  ).

    DATA(act_occ_rate) = ref_cut->get_occ_rate( 'IJ' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '70.00'
        msg = |The expected occupancy rate for carrier 'IJ' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).

    act_occ_rate = ref_cut->get_occ_rate( 'KL' ).

    cl_abap_unit_assert=>assert_equals(
        act = act_occ_rate
        exp = '80.00'
        msg = |The expected occupancy rate for carrier 'KL' is wrong.|
        quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Abstract Factory

- The pattern may be used when you need to create a set of related objects (a family of "products") that work together or are processed together for specific purposes. 
- Such families of related objects may be required in different variants. However, the created objects must be compatible with each variant. 
- Here are some examples to illustrate this:
  - Consider a car manufacturer. Multiple components are needed to assemble a car, such as the chassis, engine, and equipment (certainly, there are more components). These components form a family of related objects to a create car. All cars from the manufacturer follow the same setup: each requires a chassis, engine, and equipment. However, the manufacturer offers various car models like convertibles, sedans, SUVs, and pickup trucks. When producing these different variants, specific components may vary. The assembly must only use compatible objects. For example, when producing a convertible, a chassis with a sedan roof should not be used.
  - Consider a restaurant offers various three-course menus (this is the example used by the demo classes). Menu items include starters, main dishes, and desserts, forming a family of related objects. Different variants exist to create specific menus. For example, the restaurant offers a vegan menu. When creating the vegan menu, it should be ensured the menu does not include a beef steak as a main dish or dairy products in desserts.
- In terms of code, you may need a setup to create related objects in an organized and consistent way. The abstract factory design pattern enables this by using a high level of abstraction in your class setup, i.e. it enables the creation of related objects that belong to the same family through abstractions like abstract classes, without bothering about their specific implementations and allowing for the creation of objects with appropriate types determined at runtime. A factory, such as an abstract factory class, sets up the object creation process by specifying methods that provide objects. Concrete factories then inherit from the abstract factory class and implement the methods to create specific kinds of objects. More abstraction is involved, as outlined in the example description below.
- Some of the benefits of the pattern include: centralizing multiple object creations in one location, ensuring consistency and compatibility, simplifying object creation for users by hiding complexity, adding or modifying different variants without affecting existing code, providing flexibility and adaptability (however, adding new products might be cumbersome as it requires changes in many parts of the code).
- The abstract factory pattern differs from the factory method design pattern in several ways. Both facilitate object creation through abstraction. The factory method uses a single interface as an abstraction layer and single factory methods, while the abstract factory pattern uses a higher degree of abstraction and multiple factories. The factory method pattern primarily creates single objects, whereas the abstract factory pattern involves creating multiple related objects.


<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example Notes**

The example uses the following class setup in the CCIMP include (Local Types tab in ADT) to illustrate the abstract factory pattern: 
 
- Setting up abstract classes for products to establish a template for concrete products
  - Abstract classes for the products (abstract products)
    - In this example, the `starters`, `main_dishes`, and `desserts` classes serve as abstract classes for products.
    - These classes provide a common interface for concrete products, ensuring all variants are created consistently.
  - Concrete classes for the products (concrete products)
    - In the example, concrete classes for the products, inheriting from the `starters`, `main_dishes`, and `desserts` abstract classes, are named using the format `starters_*`, `main_dishes_*`, and `desserts_*`.
    - For each variant, such as the seafood menu, specific concrete products are provided. Here, the method implementations of the concrete classes involve adding various dishes to a string table, representing available menu options for customers. Numbers in parentheses (1-3) denote the menu course. Generally, the implementations must ensure that the products fit their specific variant.

- Setting up an abstract factory and concrete factories
  - Abstract factory class
    - The abstract factory, represented by the `menu_factory` class, defines a common interface for product families to create related objects.
    - It includes factory methods to provide these related objects.
    - The returned related objects are typed with references to the abstract products (`starters`, `main_dishes`, and `desserts`).
  - Concrete factory classes
    - The variants enter the picture with concrete factories. Here, the concrete factories are represented by the `italian_menu_creator`, `seafood_menu_creator`, and `vegan_menu_creator` classes.
    - Each variant has a concrete factory, and these factories inherit from the abstract factory class `menu_factory`.
    - The concrete factories include factory methods to create and return concrete products at runtime. Note that the returning parameter is typed with reference to the abstract product. 
    - This implementation ensures compatibility, meaning the seafood factory should only create and provide objects for starters, main dishes, and desserts related to seafood.

- Setting up a client
  - The `menu_provider` class represents the client. 
  - The client uses the abstract factories for object creation, allowing it to work with any variant without dealing with concrete factories. 
  - In this example, the implementation returns the entire menu (`create_menu` method) based on the selected menu variant. The "selection" of the menu variant is realized by the instance constructor of the class. The desired menu variant is passed to the instance constructor as an object reference, typed with reference to the abstract factory.
  - At runtime, the abstract factories connect with concrete factories so that the `create_menu` method implementation can work with them. Regardless of which variant is chosen (i.e., which object reference is passed), the client can handle it, use compatible objects, and return the desired menu.
  - The example includes the helper class `customer_order`, which has a factory method to create the appropriate object reference required by the client, based on the desired menu.

Global class:
- The global class implements the `if_oo_adt_classrun` interface and calls methods from local classes.
- It serves as a vehicle for demonstrating the design pattern. The declarations and implementations in the `CCIMP` include are relevant for the conceptual considerations.
- Using the `menu_provider` client class and specifying the variant using the `customer_order` class, a menu is created. During the method calls, all related objects (products) are created, and string tables are filled with various dishes to illustrate the pattern. As a result, a single string table is returned, containing meals for all three courses per variant offered by the restaurant. The example includes method calls for all three example variants and outputs the resulting string tables to the ADT console.
 
<br>

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
  DATA(italian_menu) = NEW menu_provider( customer_order=>place_order( menu_factory=>italian ) )->create_menu(  ).
  out->write( data = italian_menu name = `italian_menu` ).
  out->write( |\n| ).

  DATA(seafood_menu) = NEW menu_provider( customer_order=>place_order( menu_factory=>seafood ) )->create_menu(  ).
  out->write( data = seafood_menu name = `seafood_menu` ).
  out->write( |\n| ).

  DATA(vegan_menu) = NEW menu_provider( customer_order=>place_order( menu_factory=>vegan ) )->create_menu(  ).
  out->write( data = vegan_menu name = `vegan_menu` ).
  out->write( |\n| ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Abstract products
*&---------------------------------------------------------------------*

CLASS starters DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS get_starters ABSTRACT RETURNING VALUE(starters) TYPE string_table.
ENDCLASS.

CLASS main_dishes DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS get_main_dishes ABSTRACT RETURNING VALUE(main_dishes) TYPE string_table.
ENDCLASS.

CLASS desserts DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS get_desserts ABSTRACT RETURNING VALUE(desserts) TYPE string_table.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete products
*&---------------------------------------------------------------------*

CLASS starters_italian DEFINITION INHERITING FROM starters.
  PUBLIC SECTION.
    METHODS get_starters REDEFINITION.
ENDCLASS.

CLASS starters_italian IMPLEMENTATION.
  METHOD get_starters.
    starters = VALUE #( ( `Bruschetta (1)` ) ( `Caprese salad (1)` ) ( `Antipasto platter (1)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS starters_vegan DEFINITION INHERITING FROM starters.
  PUBLIC SECTION.
    METHODS get_starters REDEFINITION.
ENDCLASS.

CLASS starters_vegan IMPLEMENTATION.
  METHOD get_starters.
    starters = VALUE #( ( `Stuffed mushrooms (1)` ) ( `Zucchini fritters (1)` ) ( `Tomato soup (1)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS starters_seafood DEFINITION INHERITING FROM starters.
  PUBLIC SECTION.
    METHODS get_starters REDEFINITION.
ENDCLASS.

CLASS starters_seafood IMPLEMENTATION.
  METHOD get_starters.
    starters = VALUE #( ( `Shrimp cocktail (1)` ) ( `Crab cakes (1)` ) ( `Calamari (1)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS main_dishes_italian DEFINITION INHERITING FROM main_dishes.
  PUBLIC SECTION.
    METHODS get_main_dishes REDEFINITION.
ENDCLASS.

CLASS main_dishes_italian IMPLEMENTATION.
  METHOD get_main_dishes.
    main_dishes = VALUE #( ( `Spaghetti Carbonara (2)` ) ( `Lasagna alla Bolognese (2)` ) ( `Saltimbocca alla Romana (2)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS main_dishes_vegan DEFINITION INHERITING FROM main_dishes.
  PUBLIC SECTION.
    METHODS get_main_dishes REDEFINITION.
ENDCLASS.

CLASS main_dishes_vegan IMPLEMENTATION.
  METHOD get_main_dishes.
    main_dishes = VALUE #( ( `Chickpea curry (2)` ) ( `Cauliflower steak (2)` ) ( `Vegan burger (2)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS main_dishes_seafood DEFINITION INHERITING FROM main_dishes.
  PUBLIC SECTION.
    METHODS get_main_dishes REDEFINITION.
ENDCLASS.

CLASS main_dishes_seafood IMPLEMENTATION.
  METHOD get_main_dishes.
    main_dishes = VALUE #( ( `Baked salmon (2)` ) ( `Grilled lobster (2)` ) ( `Fish and chips (2)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS desserts_italian DEFINITION INHERITING FROM desserts.
  PUBLIC SECTION.
    METHODS get_desserts REDEFINITION.
ENDCLASS.

CLASS desserts_italian IMPLEMENTATION.
  METHOD get_desserts.
    desserts = VALUE #( ( `Tiramisu (3)` ) ( `Panna cotta (3)` ) ( `Tartufo (3)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS desserts_vegan DEFINITION INHERITING FROM desserts.
  PUBLIC SECTION.
    METHODS get_desserts REDEFINITION.
ENDCLASS.

CLASS desserts_vegan IMPLEMENTATION.
  METHOD get_desserts.
    desserts = VALUE #( ( `Fruit sorbet (3)` ) ( `Almond milk vanilla pudding (3)` ) ( `Apple crumble (3)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS desserts_seafood DEFINITION INHERITING FROM desserts.
  PUBLIC SECTION.
    METHODS get_desserts REDEFINITION.
ENDCLASS.

CLASS desserts_seafood IMPLEMENTATION.
  METHOD get_desserts.
    desserts = VALUE #( ( `Lemon sorbet (3)` ) ( `Cheesecake (3)` ) ( `Chocolate mousse (3)` ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Abstract factory
*&---------------------------------------------------------------------*

CLASS menu_factory DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM menu_variant,
             italian,
             seafood,
             vegan,
           END OF ENUM menu_variant.

    METHODS: create_starters ABSTRACT RETURNING VALUE(starters_ref) TYPE REF TO starters,
      create_main_dishes ABSTRACT RETURNING VALUE(main_dishes_ref) TYPE REF TO main_dishes,
      create_desserts ABSTRACT RETURNING VALUE(desserts_ref) TYPE REF TO desserts.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete factories
*&---------------------------------------------------------------------*

CLASS italian_menu_creator DEFINITION INHERITING FROM menu_factory.
  PUBLIC SECTION.
    METHODS: create_starters REDEFINITION,
      create_main_dishes REDEFINITION,
      create_desserts REDEFINITION.

ENDCLASS.

CLASS italian_menu_creator IMPLEMENTATION.

  METHOD create_starters.
    starters_ref = NEW starters_italian( ).
  ENDMETHOD.

  METHOD create_main_dishes.
    main_dishes_ref = NEW main_dishes_italian( ).
  ENDMETHOD.

  METHOD create_desserts.
    desserts_ref = NEW desserts_italian( ).
  ENDMETHOD.
ENDCLASS.

CLASS seafood_menu_creator DEFINITION INHERITING FROM menu_factory.
  PUBLIC SECTION.
    METHODS: create_starters REDEFINITION,
      create_main_dishes REDEFINITION,
      create_desserts REDEFINITION.

ENDCLASS.

CLASS seafood_menu_creator IMPLEMENTATION.

  METHOD create_starters.
    starters_ref = NEW starters_seafood( ).
  ENDMETHOD.

  METHOD create_main_dishes.
    main_dishes_ref = NEW main_dishes_seafood( ).
  ENDMETHOD.

  METHOD create_desserts.
    desserts_ref = NEW desserts_seafood( ).
  ENDMETHOD.

ENDCLASS.

CLASS vegan_menu_creator DEFINITION INHERITING FROM menu_factory.
  PUBLIC SECTION.
    METHODS: create_starters REDEFINITION,
      create_main_dishes REDEFINITION,
      create_desserts REDEFINITION.

ENDCLASS.

CLASS vegan_menu_creator IMPLEMENTATION.

  METHOD create_starters.
    starters_ref = NEW starters_vegan( ).
  ENDMETHOD.

  METHOD create_main_dishes.
    main_dishes_ref = NEW main_dishes_vegan( ).
  ENDMETHOD.

  METHOD create_desserts.
    desserts_ref = NEW desserts_vegan( ).
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Client
*&---------------------------------------------------------------------*

CLASS menu_provider DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING factory TYPE REF TO menu_factory,
             create_menu RETURNING VALUE(menu) TYPE string_table.
  PRIVATE SECTION.
    DATA factory TYPE REF TO menu_factory.
    DATA starters_factory TYPE REF TO starters.
    DATA main_dishes_factory TYPE REF TO main_dishes.
    DATA desserts_factory TYPE REF TO desserts.
ENDCLASS.

CLASS menu_provider IMPLEMENTATION.
  METHOD create_menu.
    "The more detailed out code lines are to emphasize that this class deals
    "with references to abstract types. Appending the lines may also
    "be achieved with fewer lines of code, as commented out below.
    starters_factory = factory->create_starters( ).
    main_dishes_factory = factory->create_main_dishes( ).
    desserts_factory = factory->create_desserts( ).

    DATA(starters_for_menu) = starters_factory->get_starters( ).
    DATA(main_dishes_for_menu) = main_dishes_factory->get_main_dishes( ).
    DATA(desserts_for_menu) = desserts_factory->get_desserts( ).

    APPEND LINES OF starters_for_menu TO menu.
    APPEND LINES OF main_dishes_for_menu TO menu.
    APPEND LINES OF desserts_for_menu TO menu.

    "APPEND LINES OF factory->create_starters( )->get_starters( ) TO menu.
    "APPEND LINES OF factory->create_main_dishes( )->get_main_dishes( ) TO menu.
    "APPEND LINES OF factory->create_desserts( )->get_desserts( ) TO menu.
  ENDMETHOD.

  METHOD constructor.
    me->factory = factory.
  ENDMETHOD.
ENDCLASS.

"Helper class
CLASS customer_order DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS place_order IMPORTING menu_variant   TYPE menu_factory=>menu_variant
                              RETURNING VALUE(factory) TYPE REF TO menu_factory.
ENDCLASS.

CLASS customer_order IMPLEMENTATION.
  METHOD place_order.
    CASE menu_variant.
      WHEN menu_factory=>italian.
        factory = NEW italian_menu_creator( ).
      WHEN menu_factory=>seafood.
        factory = NEW seafood_menu_creator( ).
      WHEN menu_factory=>vegan.
        factory = NEW vegan_menu_creator( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Adapter

- Used when APIs are incompatible, and you want to integrate those APIs into an existing one to consolidate all functionality.
- This can be achieved by using an adapter class to convert and transform functionality from a non-compatible API into the existing API.

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- This example demonstrates the adapter design pattern with the following declarations and implementations: 
  - Global class:  
    - Implements `if_oo_adt_classrun` and calls methods from local classes.  
    - Acts as a consumer of APIs (local classes) defined in the CCIMP include.  
  - CCIMP include (Local Types tab in ADT):  
    - The example is similar to the factory method example.
    - Local interface `lif_hello`: Defines an enumeration type and a method that returns a string. 
    - Local classes `lcl_**` implement `lif_hello`.
    - Local class `lcl_hello_factory`: Contains the factory method `create_hello`. The class specifies `CREATE PRIVATE` to prevent external instantiation. The `create_hello` returns a reference of type `REF TO lif_hello`.
    - Assuming functionality is extended. An existing API is integrated and reused.
    - This is represented by the `lcl_de_xstring` class. It implements a different interface. The method returns a data object of type `xstring`, which is not compatible with the `say_hello` method of the `lif_hello` interface that returns a `string`.
    - An adapter class is introduced that implements the `lif_hello` interface and handles conversion (in the example case, `xstring` is transformed to `string`) to allow the use of the non-compatible API. 
    - Users can the call methods of the adapter class.
- The class execution includes the following:
  - Multiple objects are created using the factory method with different input parameters.
  - Based on the parameter, a specific object is created. The `say_hello` method returns a string as implemented in the resepctive class.
  - When `lif_hello=>de` is supplied, an object is created using the adapter class.


<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Saying hello in English
    DATA(oref_en) = lcl_hello_factory=>create_hello( lif_hello=>en ).
    DATA(hello_en) = oref_en->say_hello( ).
    out->write( hello_en ).

    "Saying hello in French
    DATA(oref_fr) = lcl_hello_factory=>create_hello( lif_hello=>fr ).
    DATA(hello_fr) = oref_fr->say_hello( ).
    out->write( hello_fr ).

    "Saying hello in Italian
    DATA(oref_it) = lcl_hello_factory=>create_hello( lif_hello=>it ).
    DATA(hello_it) = oref_it->say_hello( ).
    out->write( hello_it ).

    "Saying hello in Spanish
    DATA(oref_es) = lcl_hello_factory=>create_hello( lif_hello=>es ).
    DATA(hello_es) = oref_es->say_hello( ).
    out->write( hello_es ).

    "Saying hello in German
    "See the local class implementation. This method call demonstrates the adapter since
    "the required data is originally available in a non-conform way ('Hallo' is
    "available as xstring, coming from a different API that does not implement the same
    "interface as the other classes). The adapter class (called when creating the instance
    "in the factory method) integrates the non-conform API and transforms the content.
    DATA(oref_de) = lcl_hello_factory=>create_hello( lif_hello=>de ).
    DATA(hello_de) = oref_de->say_hello( ).
    out->write( hello_de ).

    "Default hello
    DATA(oref_default) = lcl_hello_factory=>create_hello( lif_hello=>init ).
    DATA(hello_default) = oref_default->say_hello( ).
    out->write( hello_default ).

  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
INTERFACE lif_hello.
  TYPES: basetype TYPE i,
         BEGIN OF ENUM enum_langu BASE TYPE basetype,
           init VALUE IS INITIAL,
           en   VALUE 1,
           fr   VALUE 2,
           it   VALUE 3,
           es   VALUE 4,
           de   VALUE 5,
         END OF ENUM enum_langu.

  METHODS say_hello RETURNING VALUE(hi) TYPE string.
ENDINTERFACE.

CLASS lcl_en DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_hello.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_en IMPLEMENTATION.
  METHOD lif_hello~say_hello.
    hi = `Hi`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_fr DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_hello.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_fr IMPLEMENTATION.
  METHOD lif_hello~say_hello.
    hi = `Salut`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_it DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_hello.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_it IMPLEMENTATION.
  METHOD lif_hello~say_hello.
    hi = `Ciao`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_es DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_hello.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_es IMPLEMENTATION.
  METHOD lif_hello~say_hello.
    hi = `Hola`.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Class that does not implement the lif_hello interface
"The assumption is that functionality of the API is reused and integrated
"into the exsisting API. The non-compatible type is converted using an
"adapter class.

INTERFACE lif_hello_as_xstring.
  METHODS xstring_hello RETURNING VALUE(hi) TYPE xstring.
ENDINTERFACE.

CLASS lcl_de_xstring DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_hello_as_xstring.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_de_xstring IMPLEMENTATION.
  METHOD lif_hello_as_xstring~xstring_hello.
    hi = CONV xstring( `48616C6C6F` ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Adapter class

CLASS lcl_de_adapter DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_hello.
ENDCLASS.

CLASS lcl_de_adapter IMPLEMENTATION.
  METHOD lif_hello~say_hello.
    DATA(oref) = NEW lcl_de_xstring( ).
    DATA(hello_as_xstring) = oref->lif_hello_as_xstring~xstring_hello( ).
    hi = cl_abap_conv_codepage=>create_in( )->convert( hello_as_xstring ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Class containing a factory method

CLASS lcl_hello_factory DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS create_hello IMPORTING language     TYPE lif_hello=>enum_langu
                               RETURNING VALUE(hello) TYPE REF TO lif_hello.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_hello_factory IMPLEMENTATION.
  METHOD create_hello.
    hello = SWITCH #( language
                      WHEN lif_hello=>en THEN NEW lcl_en( )
                      WHEN lif_hello=>fr THEN NEW lcl_fr( )
                      WHEN lif_hello=>it THEN NEW lcl_it( )
                      WHEN lif_hello=>es THEN NEW lcl_es( )
                      "Calling the method in the adapter class
                      WHEN lif_hello=>de THEN NEW lcl_de_adapter( )
                      "E.g. raising an exception or returning a default object
                      ELSE NEW lcl_en( ) ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Builder

- The idea is to simplify the creation of complex objects, which may involve multiple steps, by hiding this complexity from users.
- Depending on requirements or user input, the class setup should allow for the creation of different objects that share similar or the same creation steps and attributes.
- The builder design pattern may be useful here. Builder classes consolidate the necessary steps to create objects and return specific ones, allowing users to get the objects without taking care of the exact process.
- By using the builder pattern, you can flexibly create different objects that follow the same or similar building processes. Builder classes also centralize code changes, and you can add more builder classes for additional use cases or objects.
- The example tries to demonstrate the builder design pattern with the following class setup: 
  - An abstract builder class defines the steps for object creation and includes methods and attributes shared by concrete builder classes responsible for creating specific objects. The example does not use an interface because the abstract class includes non-abstract methods performing general tasks not requiring object-specific implementations.
  - Multiple concrete builder classes inherit from the abstract superclass. They (can) follow the same object creation flow when redefining the superclass methods, but the implementation may vary among builder classes to serve different purposes for specific objects.
  - Another class orchestrates object creation. This class contains factory methods and acts as an object provider for external users (the global class in the example), simplifying object creation and hiding complexity. Users call a factory method, providing certain input parameters. Based on these values, specific objects are created by dedicated concrete builder classes. The orchestration class ensures methods are called in the correct sequence and returns the final created object.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>


**Example notes:**

- Purpose: The example represents an automatic internal table creator and table content creator using random values.
- Global class:
  - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
  - Acts as a consumer of APIs, i.e. local classes defined in the CCIMP include.
- CCIMP include (Local Types tab in ADT):
  - `lcl_itab_builder`:
    - An abstract builder class defining instance attributes and abstract methods. Concreate builder classes inherit from this superclass.
    - It is not an interface because it includes attributes and non-abstract methods for general tasks used by subclasses. These methods are in the public visibility section because the global example class also demonstrates their functionality.
  - The common building steps (i.e. the abstract methods) are:
    - `build_type_info`: Checks if the provided type is available.
    - `build_components`: Retrieves component names and their types (e.g., `string`, `i`, `decfloat34`).
    - `build_table_keys`: Builds or verifies a list of primary table keys used for internal table creation, depending on whether a custom key list is supplied or default keys are used. The latter retrieves key components specified centrally in the DDIC, such as when DDIC database tables are used as type names.
    - `build_table_type`: Builds a type description object based on the line type, table kind, and keys.
    - `build_data_object`: Creates the internal table. For flexibility, it is created as an anonymous data object (`TYPE REF TO data`) using a dynamic `CREATE DATA` statement and the created type description object.
    - `build_random_data`: Adds table entries to the created internal table, using nonsense random content for components with elementary types. Methods for retrieving random values are implemented in the abstract superclass.
  - `lcl_builder_*` classes:
    - Concrete builder classes, such as `lcl_builder_std_itab_w_prkey`, that inherit from the abstract superclass.
    - These classes implement the abstract methods of the superclass, representing the step-by-step building process to create specific objects.
    - The example includes concrete builder classes that create standard tables with non-unique primary keys and empty keys, as well as sorted and hashed tables.
  - `lcl_itab_data_provider`: A concrete builder class inheriting from the abstract builder class, used to populate existing internal tables with demo data. An internal table is passed as an actual parameter in a method described next.
  - `lcl_itab_provider`:
    - Orchestrates the step-by-step object building process and provides the objects to users to hide complexity and guide creation.
    - Called by users, which is the global class in the example.
    - Contains two factory methods:
      - `create_itab`: Used for internal table creation based on input parameters. It expects a line type name: globally available line types, such as DDIC database table or CDS entity names, or globally available elementary types for simple internal tables. Optionally, you can specify key component names for the target table; if component names not in the structure are used, they are ignored. If not specified, default key components are used to create tables with primary keys. Optionally, you can specify the number of table entries to create. The method returns an object with the table available via an instance attribute.
      - `populate_itab`: Supplies existing local internal tables with nonsense random demo data. The method returns an object with the populated table available via an instance attribute.
- Note:
  - This example is for experimentation and exploration. It does not claim to cover all aspects of the design pattern or internal table creation/population.
  - Excluded aspects include: Limited exception handling (a failing `ASSERT` statement is available in some cases), secondary table key handling, proper handling of key uniqueness, fixed or enumerated values, deep or nested components (such as references, nested structures, and tables), uppercase letters for character-like types, unnecessary population of the client field, returning the internal table in an output parameter (the table is available in an instance attribute of type `REF to data`).
  - The example uses repository objects from the ABAP cheat sheet repository. If you have not imported it, you can replace the `Z*` artifacts with others for exploration.

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
"! ABAP example demonstrating the builder design pattern
"! See the disclaimer in the ABAP cheat sheet repository's readme file.
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS get_table_info IMPORTING itab TYPE ANY TABLE RETURNING VALUE(info) TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Creating a string table that includes type names
    "The entries include database table, CDS entity names of the ABAP cheat sheet repository.
    "Plus, elementary types (built-in ABAP types) and a non-existent type are included.
    DATA(type_names) = VALUE string_table(
      ( `ZDEMO_ABAP_CARR` )
      ( `ZDEMO_ABAP_FLI` )
      ( `ZDEMO_ABAP_CARR_VE` )
      ( `ZDEMO_ABAP_ABSTRACT_ENT` )
      "Elementary types
      ( `STRING` )
      ( `UTCLONG` )
      "Non-existent type
      ( `TYPE_THAT_DOES_NOT_EXIST` ) ).

    out->write( `1) Creating standard tables with non-unique primary table keys` ).
    out->write( |\n| ).

    "Specifying no key components, using default ones
    LOOP AT type_names INTO DATA(name).
      DATA(oref) = lcl_itab_provider=>create_itab(
                     type_name         = name
                     table_kind        = lcl_itab_provider=>standard_w_nonunique_pr_key
                     table_entry_count = 10 ).

      out->write( |------------- Table with line type { oref->type_name } -------------| ).
      out->write( |\n| ).

      IF oref->type_exists = abap_true.
        "The example is designed to retrieve Table type informationrmation so as to display
        "information such type and table keys of the internal table.
        DATA(info) = get_table_info( oref->itab->* ).
        out->write( `Table type information:` ).
        out->write( info ).
        out->write( |\n| ).
        out->write( oref->itab->*  ).
        out->write( |\n| ).
      ELSE.
        out->write( `Type does not exist. Internal table not created.` ).
        out->write( |\n| ).
      ENDIF.
    ENDLOOP.

    out->write( `------------- Key components explicitly specified -------------` ).
    out->write( |\n| ).

    "Key components explicitly specified (including a non-existent component name that is ignored)
    DATA(oref_key_specified) = lcl_itab_provider=>create_itab(
                   type_name         = `ZDEMO_ABAP_CARR`
                   table_kind        = lcl_itab_provider=>standard_w_nonunique_pr_key
                   key_components    = VALUE #( ( `CARRID` ) ( `CARRNAME` ) ( `FALSE_COMPONENT` ) )
                   table_entry_count = 3 ).

    out->write( `------------- Table with line type ZDEMO_ABAP_CARR -------------` ).
    IF oref_key_specified->type_exists = abap_true.
      info = get_table_info( oref_key_specified->itab->* ).
      out->write( `Table type information:` ).
      out->write( info ).
      out->write( |\n| ).
      out->write( oref_key_specified->itab->*  ).
      out->write( |\n| ).
    ELSE.
      out->write( `Type does not exist. Internal table not created.` ).
      out->write( |\n| ).
    ENDIF.

    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `2) Creating standard tables with empty key` ).
    out->write( |\n| ).

    type_names = VALUE string_table(
       ( `ZDEMO_ABAP_FLSCH` )
       ( `D` ) ).

    LOOP AT type_names INTO name.
      DATA(oref_empty_key) = lcl_itab_provider=>create_itab(
                     type_name         = name
                     table_kind        = lcl_itab_provider=>standard_w_empty_key
                     "Specified key components are ignored in the example
                     "key_components    = VALUE #( ( `CARRID` ) ( `CARRNAME` ) )
                     table_entry_count = 3 ).

      out->write( |------------- Table with line type { oref_empty_key->type_name } -------------| ).
      out->write( |\n| ).

      IF oref_empty_key->type_exists = abap_true.
        info = get_table_info( oref_empty_key->itab->* ).
        out->write( `Table type information:` ).
        out->write( info ).
        out->write( |\n| ).
        out->write( oref_empty_key->itab->*  ).
        out->write( |\n| ).
      ELSE.
        out->write( `Type does not exist. Internal table not created.` ).
        out->write( |\n| ).
      ENDIF.
    ENDLOOP.

    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `3) Creating sorted tables with unique primary keys` ).
    out->write( |\n| ).

    type_names = VALUE string_table(
       ( `ZDEMO_ABAP_FLI` )
       ( `ZDEMO_ABAP_TAB1` )
       ( `T` ) ).

    LOOP AT type_names INTO name.
      DATA(oref_sorted) = lcl_itab_provider=>create_itab(
                     type_name         = name
                     table_kind        = lcl_itab_provider=>sorted_w_unique_pr_key
                     table_entry_count = 3 ).

      out->write( |------------- Table with line type { oref_sorted->type_name } -------------| ).
      out->write( |\n| ).

      IF oref_sorted->type_exists = abap_true.
        info = get_table_info( oref_sorted->itab->* ).
        out->write( `Table type information:` ).
        out->write( info ).
        out->write( |\n| ).
        out->write( oref_sorted->itab->*  ).
        out->write( |\n| ).
      ELSE.
        out->write( `Type does not exist. Internal table not created.` ).
        out->write( |\n| ).
      ENDIF.
    ENDLOOP.

    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `4) Creating hashed tables with unique primary keys` ).
    out->write( |\n| ).

    type_names = VALUE string_table(
       ( `ZDEMO_ABAP_CARR` )
       ( `ZDEMO_ABAP_TAB2` )
       ( `I` ) ).

    LOOP AT type_names INTO name.
      DATA(oref_hashed) = lcl_itab_provider=>create_itab(
                     type_name         = name
                     table_kind        = lcl_itab_provider=>hashed_w_unique_pr_key
                     table_entry_count = 3 ).

      out->write( |------------- Table with line type { oref_hashed->type_name } -------------| ).
      out->write( |\n| ).

      IF oref_hashed->type_exists = abap_true.
        info = get_table_info( oref_hashed->itab->* ).
        out->write( `Table type information:` ).
        out->write( info ).
        out->write( |\n| ).
        out->write( oref_hashed->itab->*  ).
        out->write( |\n| ).
      ELSE.
        out->write( `Type does not exist. Internal table not created.` ).
        out->write( |\n| ).
      ENDIF.
    ENDLOOP.

    out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `5) Populating internal tables with random data` ).
    out->write( |\n| ).

    "Demo internal tables to be filled
    DATA it_std1 TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
    DATA it_std2 TYPE string_table.
    DATA it_std3 TYPE TABLE OF i.

    TYPES: BEGIN OF various_types,
             c1        TYPE c LENGTH 1,
             c5        TYPE c LENGTH 5,
             c10       TYPE c LENGTH 10,
             str       TYPE string,
             int       TYPE i,
             f         TYPE f,
             dec16     TYPE decfloat16,
             dec34     TYPE decfloat34,
             i8        TYPE int8,
             n5        TYPE n LENGTH 5,
             time      TYPE t,
             date      TYPE d,
             timestamp TYPE utclong,
             x1        TYPE x LENGTH 1,
             x5        TYPE x LENGTH 5,
             xstr      TYPE xstring,
             pl2d1     TYPE p LENGTH 2 DECIMALS 1,
             pl3d2     TYPE p LENGTH 3 DECIMALS 2,
             pl4d3     TYPE p LENGTH 4 DECIMALS 3,
             pl5d4     TYPE p LENGTH 5 DECIMALS 4,
             pl6d5     TYPE p LENGTH 6 DECIMALS 5,
             pl7d6     TYPE p LENGTH 7 DECIMALS 6,
             pl8d7     TYPE p LENGTH 8 DECIMALS 7,
             pl9d8     TYPE p LENGTH 9 DECIMALS 8,
             pl10d9    TYPE p LENGTH 10 DECIMALS 9,
             pl11d10   TYPE p LENGTH 11 DECIMALS 10,
             pl12d11   TYPE p LENGTH 12 DECIMALS 11,
             pl13d12   TYPE p LENGTH 13 DECIMALS 12,
             pl14d13   TYPE p LENGTH 14 DECIMALS 13,
             pl15d14   TYPE p LENGTH 15 DECIMALS 14,
           END OF various_types.

    DATA it_std4 TYPE TABLE OF various_types WITH EMPTY KEY.

    "Sorted tables
    DATA it_sorted1 TYPE SORTED TABLE OF zdemo_abap_flsch WITH NON-UNIQUE KEY primary_key COMPONENTS carrid connid cityfrom.
    DATA it_sorted2 TYPE SORTED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid connid.
    DATA it_sorted3 TYPE SORTED TABLE OF utclong WITH NON-UNIQUE KEY table_line.

    "Hashed tables
    DATA it_hashed1 TYPE HASHED TABLE OF zdemo_abap_flsch WITH UNIQUE KEY primary_key COMPONENTS carrid connid.
    TYPES n5 TYPE n LENGTH 5.
    DATA it_hashed2 TYPE HASHED TABLE OF n5 WITH UNIQUE KEY primary_key COMPONENTS table_line.

    TYPES table_refs TYPE TABLE OF REF TO data WITH EMPTY KEY.
    DATA(itab_refs) = VALUE table_refs(
      ( REF #( it_std1 ) )
      ( REF #( it_std2 ) )
      ( REF #( it_std3 ) )
      ( REF #( it_std4 ) )
      ( REF #( it_sorted1 ) )
      ( REF #( it_sorted2 ) )
      ( REF #( it_sorted3 ) )
      ( REF #( it_hashed1 ) )
      ( REF #( it_hashed2 ) ) ).

    LOOP AT itab_refs INTO DATA(ref).
      DATA(tabix) = sy-tabix.
      DATA(oref_populate_itab) = lcl_itab_provider=>populate_itab(
                                   itab              = ref->*
                                   table_entry_count = 3
                                 ).

      out->write( |------------- Internal table { tabix } -------------| ).
      out->write( |\n| ).

      info = get_table_info( oref_populate_itab->itab->* ).
      out->write( `Table type information:` ).
      out->write( info ).

      out->write( |\n| ).
      out->write( oref_populate_itab->itab->*  ).
      out->write( |\n| ).
    ENDLOOP.

        out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `6) Other line types` ).
    out->write( |\n| ).

    "Other line types not supported in the example
*DATA(oref_not_working) = lcl_itab_provider=>populate_itab(
*                             itab              = itab_refs
*                             table_entry_count = 3  ).

    "Deep/nested line types are not supported
    "These components remain initial.
    TYPES: BEGIN OF deep_type,
             flag   TYPE abap_boolean,
             c5     TYPE c LENGTH 5,
             strtab TYPE string_table,
             struc  TYPE zdemo_abap_carr,
           END OF deep_type.

    DATA deep_itab TYPE TABLE OF deep_type WITH EMPTY KEY.

    DATA(oref_deep) = lcl_itab_provider=>populate_itab(
                                 itab              = deep_itab
                                 table_entry_count = 3  ).

    out->write( oref_deep->itab->*  ).
    out->write( |\n| ).
        out->write( repeat( val = `*` occ = 100 ) ).
    out->write( |\n| ).

**********************************************************************

    out->write( `7) Exploring the random value creation methods` ).
    out->write( |\n| ).

    out->write( `---------------- Getting random strings ----------------` ).
    DO 5 TIMES.
      DATA(str) = lcl_itab_builder=>get_random_string( sy-index ).
      out->write( str ).
    ENDDO.

    out->write( `---------------- Getting random number sequences ----------------` ).
    DO 5 TIMES.
      DATA(number_set) = lcl_itab_builder=>get_random_number_sequence( sy-index ).
      out->write( number_set ).
    ENDDO.

    out->write( `---------------- Getting random packed numbers ----------------` ).
    DO 15 TIMES.
      DATA(random_p) = lcl_itab_builder=>get_random_p(
                         length = 8
                         decimals  = sy-index - 1 ).
      out->write( random_p->* ).
    ENDDO.

    out->write( `---------------- Getting random dates ----------------` ).
    DO 5 TIMES.
      DATA(random_d) = lcl_itab_builder=>get_random_d( ).
      out->write( random_d ).
    ENDDO.

    out->write( `---------------- Getting random times ----------------` ).
    DO 5 TIMES.
      DATA(random_t) = lcl_itab_builder=>get_random_t( ).
      out->write( random_t ).
    ENDDO.

    out->write( `---------------- Getting random UTC timestamps ----------------` ).
    DO 5 TIMES.
      DATA(random_utc) = lcl_itab_builder=>get_random_utclong( ).
      out->write( random_utc ).
    ENDDO.

    out->write( `---------------- Getting random data objects of type decfloat16 ----------------` ).
    DO 5 TIMES.
      DATA(random_dec16) = lcl_itab_builder=>get_random_dec16( ).
      out->write( random_dec16 ).
    ENDDO.

    out->write( `---------------- Getting random data objects of type decfloat34 ----------------` ).
    DO 5 TIMES.
      DATA(random_dec34) = lcl_itab_builder=>get_random_dec34( ).
      out->write( random_dec34 ).
    ENDDO.

    out->write( `---------------- Getting random data objects of type f ----------------` ).
    DO 5 TIMES.
      DATA(random_f) = lcl_itab_builder=>get_random_f( ).
      out->write( random_f ).
    ENDDO.

    out->write( `---------------- Getting random data objects of type x ----------------` ).

    DATA(xstr_1) = lcl_itab_builder=>get_random_x( 1 ).
    DATA x1 TYPE x LENGTH 1.
    x1 = xstr_1.
    out->write( x1 ).

    DATA(xstr_2) = lcl_itab_builder=>get_random_x( 2 ).
    DATA x2 TYPE x LENGTH 2.
    x2 = xstr_2.
    out->write( x2 ).

    DATA(xstr_8) = lcl_itab_builder=>get_random_x( 8 ).
    DATA x8 TYPE x LENGTH 8.
    x8 = xstr_8.
    out->write( x8 ).

    out->write( `---------------- Getting random data objects of type xstring ----------------` ).

    DATA(xstr_a) = lcl_itab_builder=>get_random_xstring( ).
    out->write( xstr_a ).
    DATA(xstr_b) = lcl_itab_builder=>get_random_xstring( ).
    out->write( xstr_b ).
    DATA(xstr_c) = lcl_itab_builder=>get_random_xstring( ).
    out->write( xstr_c ).

  ENDMETHOD.

  METHOD get_table_info.
    DATA(tab_type_info) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab ) ).

    "Getting the table kind
    "For the constant values of type abap_tablekind, see cl_abap_tabledescr. For example, 'S'
    "stands for a standard table.
    DATA(tab_table_kind) = tab_type_info->table_kind.
    INSERT |Table kind: { tab_table_kind }| INTO TABLE info.

    "Checking if the table has a unique key
    DATA(tab_has_unique_key) = tab_type_info->has_unique_key.
    INSERT |Has a unique key: "{ tab_has_unique_key }" | &&
    |{ COND #( WHEN tab_has_unique_key IS INITIAL THEN `(no unique key)` ) }| INTO TABLE info.

    "Returning a table with a description of all table keys, e.g. all components of a key,
    "key kind (U, unique, in the example case), information whether the key is the primary
    "key etc. For the constant values, see the cl_abap_tabledescr class.
    DATA(tab_keys) = tab_type_info->get_keys( ).

    INSERT |Table keys: { REDUCE string( INIT str = `` FOR <key2> IN tab_keys NEXT str = |{ str }| &&
    |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ REDUCE string( INIT str2 = `` FOR <key3> IN <key2>-components NEXT str2 = |{ str2 }| &&
    |{ COND #( WHEN str2 IS NOT INITIAL THEN `/` ) }{ <key3>-name }| ) } (is primary: "{ <key2>-is_primary }", |  &&
    |is unique: "{ <key2>-is_unique }", key kind: "{ <key2>-key_kind }", access kind: "{ <key2>-access_kind }")| ) }| INTO TABLE info.
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
"Class for the builder
"An abstract class is used as there are more general tasks performed by the subclasses.
"These tasks are implemented in non-abstract methods in the class.
CLASS lcl_itab_builder DEFINITION ABSTRACT CREATE PUBLIC.
  PUBLIC SECTION.
    DATA type_name TYPE string.
    DATA primary_key_components TYPE string_table.
    DATA itab TYPE REF TO data.
    DATA table_entries_to_create TYPE i.
    DATA type_exists TYPE abap_boolean.
    DATA components TYPE cl_abap_structdescr=>component_table.
    DATA is_elementary_line_type TYPE abap_boolean.
    DATA table_type_descr_obj TYPE REF TO cl_abap_tabledescr.
    DATA line_type TYPE REF TO cl_abap_datadescr.

    "----------------------- Abstract methods -----------------------
    METHODS build_type_info ABSTRACT RETURNING VALUE(type_exists) TYPE abap_boolean.
    METHODS build_components ABSTRACT.
    METHODS build_table_keys ABSTRACT.
    METHODS build_table_type ABSTRACT.
    METHODS build_data_object ABSTRACT.
    METHODS build_random_data ABSTRACT.

    "----------------------- Constants used in non-abstract methods -----------------------
    CONSTANTS character_set TYPE string VALUE `abcdefghijklmnopqrstuvwxyz0123456789`.
    CONSTANTS number_set TYPE string VALUE `0123456789`.
    CONSTANTS max_length TYPE i VALUE 10.
    CONSTANTS start_date TYPE d VALUE '20250101'.
    CONSTANTS start_time TYPE t VALUE '000000'.
    CONSTANTS max_table_entry_count TYPE i VALUE 50.
    CONSTANTS min_int_value TYPE i VALUE 1.
    CONSTANTS max_int_value TYPE i VALUE 100.
    CONSTANTS min_int8_value TYPE int8 VALUE 1.
    CONSTANTS max_int8_value TYPE int8 VALUE 100.
    CONSTANTS min_p_value TYPE p VALUE 0.
    CONSTANTS max_p_value TYPE p VALUE 9.

    "----------------------- Non-abstract methods used by subclasses -----------------------
    "They are intentionally included in the public visibility section for the demo in the
    "global class.
    METHODS check_type.
    METHODS handle_keys.
    METHODS handle_components.

    METHODS add_table_entries
      IMPORTING
        VALUE(table_entry_count) TYPE i.

    CLASS-METHODS get_random_string
      IMPORTING
        length           TYPE i OPTIONAL
        randomize_length TYPE abap_boolean DEFAULT abap_true
          PREFERRED PARAMETER length
      RETURNING
        VALUE(str)       TYPE string.

    CLASS-METHODS get_random_number_sequence
      IMPORTING
        length           TYPE i OPTIONAL
        randomize_length TYPE abap_boolean DEFAULT abap_true
          PREFERRED PARAMETER length
      RETURNING
        VALUE(numbers)   TYPE string.

    CLASS-METHODS get_random_i
      IMPORTING min_value     TYPE i DEFAULT min_int_value
                max_value     TYPE i DEFAULT max_int_value
      RETURNING VALUE(number) TYPE i.

    CLASS-METHODS get_random_int8
      IMPORTING min_value     TYPE int8 DEFAULT min_int8_value
                max_value     TYPE int8 DEFAULT max_int8_value
      RETURNING VALUE(number) TYPE i.

    CLASS-METHODS get_random_p
      IMPORTING length               TYPE i
                decimals             TYPE i
                min_value            TYPE p DEFAULT min_p_value
                max_value            TYPE p DEFAULT max_p_value
      RETURNING VALUE(packed_number) TYPE REF TO data.

    CLASS-METHODS get_random_n
      IMPORTING length          TYPE i
      RETURNING VALUE(random_n) TYPE string.

    CLASS-METHODS get_random_d
      RETURNING VALUE(random_d) TYPE d.

    CLASS-METHODS get_random_t
      RETURNING VALUE(random_t) TYPE t.

    CLASS-METHODS get_random_utclong
      RETURNING VALUE(random_utc) TYPE utclong.

    CLASS-METHODS get_random_dec16
      RETURNING VALUE(random_dec16) TYPE decfloat16.

    CLASS-METHODS get_random_dec34
      RETURNING VALUE(random_dec34) TYPE decfloat34.

    CLASS-METHODS get_random_f
      RETURNING VALUE(random_f) TYPE f.

    CLASS-METHODS get_random_x
      IMPORTING length          TYPE i
      RETURNING VALUE(random_x) TYPE xstring.

    CLASS-METHODS get_random_xstring
      RETURNING VALUE(random_xstring) TYPE xstring.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_itab_builder IMPLEMENTATION.

  METHOD get_random_string.
    IF length IS NOT SUPPLIED.
      DATA(len) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 1
                                              max  = max_length )->get_next( ).
    ELSE.
      IF length NOT BETWEEN 1 AND max_length.
        len = max_length.
      ELSE.
        len = length.
      ENDIF.

      IF randomize_length = abap_true.
        len = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                          min  = 1
                                          max  = len )->get_next( ).
      ENDIF.
    ENDIF.

    DO len TIMES.
      DATA(num) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 0
                                              max  = strlen( character_set ) - 1 )->get_next( ).
      str &&= character_set+num(1).
    ENDDO.
  ENDMETHOD.

  METHOD get_random_number_sequence.
    IF length IS NOT SUPPLIED.
      DATA(len) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 1
                                              max  = max_length )->get_next( ).
    ELSE.
      IF length NOT BETWEEN 1 AND max_length.
        len = max_length.
      ELSE.
        len = length.
      ENDIF.
      IF randomize_length = abap_true.
        len = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                          min  = 1
                                          max  = len )->get_next( ).
      ENDIF.
    ENDIF.

    DO len TIMES.
      DATA(num) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 0
                                              max  = strlen( number_set ) - 1 )->get_next( ).
      numbers &&= number_set+num(1).
    ENDDO.
  ENDMETHOD.

  METHOD get_random_i.
    RETURN cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                       min  = min_value
                                       max  = max_value )->get_next( ).
  ENDMETHOD.

  METHOD get_random_int8.
    RETURN cl_abap_random_int8=>create( seed = cl_abap_random=>seed( )
                                        min  = min_value
                                        max  = max_value )->get_next( ).
  ENDMETHOD.

  METHOD get_random_p.
    IF length NOT BETWEEN 1 AND 16.
      RETURN.
    ENDIF.

    IF decimals NOT BETWEEN 0 AND 14.
      RETURN.
    ENDIF.

    TRY.
        CASE decimals.
          WHEN 0.
            DATA(a) = cl_abap_random_packed=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed=>p31_0( min_value ) max = CONV cl_abap_random_packed=>p31_0( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE a.
            packed_number->* = a.
          WHEN 1.
             DATA(b) = cl_abap_random_packed_dec1=>create( seed = cl_abap_random=>seed( )
                min = COND #( WHEN length = 1 THEN CONV cl_abap_random_packed_dec1=>p31_1( '0.1' ) ELSE CONV cl_abap_random_packed_dec1=>p31_1( min_value ) )
                max = COND #( WHEN length = 1 THEN CONV cl_abap_random_packed_dec1=>p31_1( '0.9' ) ELSE CONV cl_abap_random_packed_dec1=>p31_1( max_value ) )
              )->get_next( ).
            CREATE DATA packed_number LIKE b.
            packed_number->* = b.
          WHEN 2.
            DATA(c) = cl_abap_random_packed_dec2=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec2=>p31_2( min_value ) max = CONV cl_abap_random_packed_dec2=>p31_2( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE c.
            packed_number->* = c.
          WHEN 3.
            DATA(d) = cl_abap_random_packed_dec3=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec3=>p31_3( min_value ) max = CONV cl_abap_random_packed_dec3=>p31_3( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE d.
            packed_number->* = d.
          WHEN 4.
            DATA(e) = cl_abap_random_packed_dec4=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec4=>p31_4( min_value ) max = CONV cl_abap_random_packed_dec4=>p31_4( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE e.
            packed_number->* = e.
          WHEN 5.
            DATA(f) = cl_abap_random_packed_dec5=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec5=>p31_5( min_value ) max = CONV cl_abap_random_packed_dec5=>p31_5( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE f.
            packed_number->* = f.
          WHEN 6.
            DATA(g) = cl_abap_random_packed_dec6=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec6=>p31_6( min_value ) max = CONV cl_abap_random_packed_dec6=>p31_6( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE g.
            packed_number->* = g.
          WHEN 7.
            DATA(h) = cl_abap_random_packed_dec7=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec7=>p31_7( min_value ) max = CONV cl_abap_random_packed_dec7=>p31_7( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE h.
            packed_number->* = h.
          WHEN 8.
            DATA(i) = cl_abap_random_packed_dec8=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec8=>p31_8( min_value ) max = CONV cl_abap_random_packed_dec8=>p31_8( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE i.
            packed_number->* = i.
          WHEN 9.
            DATA(j) = cl_abap_random_packed_dec9=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec9=>p31_9( min_value ) max = CONV cl_abap_random_packed_dec9=>p31_9( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE j.
            packed_number->* = j.
          WHEN 10.
            DATA(k) = cl_abap_random_packed_dec10=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec10=>p31_10( min_value ) max = CONV cl_abap_random_packed_dec10=>p31_10( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE k.
            packed_number->* = k.
          WHEN 11.
            DATA(l) = cl_abap_random_packed_dec11=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec11=>p31_11( min_value ) max = CONV cl_abap_random_packed_dec11=>p31_11( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE l.
            packed_number->* = l.
          WHEN 12.
            DATA(m) = cl_abap_random_packed_dec12=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec12=>p31_12( min_value ) max = CONV cl_abap_random_packed_dec12=>p31_12( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE m.
            packed_number->* = m.
          WHEN 13.
            DATA(n) = cl_abap_random_packed_dec13=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec13=>p31_13( min_value ) max = CONV cl_abap_random_packed_dec13=>p31_13( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE n.
            packed_number->* = n.
          WHEN 14.
            DATA(o) = cl_abap_random_packed_dec14=>create( seed = cl_abap_random=>seed( ) min = CONV cl_abap_random_packed_dec14=>p31_14( min_value ) max = CONV cl_abap_random_packed_dec14=>p31_14( max_value ) )->get_next( ).
            CREATE DATA packed_number LIKE o.
            packed_number->* = o.
        ENDCASE.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD get_random_n.
    IF length > max_length OR length < 1.
      DATA(len) = max_length.
    ELSE.
      len = length.
    ENDIF.
    random_n = get_random_number_sequence( len ).
  ENDMETHOD.

  METHOD get_random_d.
    DATA(int) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                            min  = -100
                                            max  = 100 )->get_next( ).
    random_d =  start_date + int.
  ENDMETHOD.

  METHOD get_random_t.
    DATA(int) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                            min  = -36000
                                            max  = 36000 )->get_next( ).
    random_t = start_time + int.
  ENDMETHOD.

  METHOD get_random_utclong.
    DATA(int) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                            min  = -100
                                            max  = 100 )->get_next( ).

    random_utc = utclong_add( val = utclong_current( )
                              days = int
                              hours = int
                              minutes = int
                              seconds = int ).
  ENDMETHOD.

  METHOD get_random_dec16.
    random_dec16 = cl_abap_random_decfloat16=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
  ENDMETHOD.

  METHOD get_random_dec34.
    random_dec34 = cl_abap_random_decfloat34=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
  ENDMETHOD.

  METHOD get_random_f.
    random_f = cl_abap_random_float=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
  ENDMETHOD.

  METHOD get_random_x.
    DATA(random_string) = get_random_string( length ).
    random_x = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( random_string ).
  ENDMETHOD.

  METHOD get_random_xstring.
    DATA(random_string) = get_random_string( ).
    random_xstring = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( random_string ).
  ENDMETHOD.

  METHOD add_table_entries.

    IF table_entry_count < 0.
      table_entry_count = 1.
    ENDIF.

    IF table_entry_count > max_table_entry_count.
      table_entry_count = max_table_entry_count.
    ENDIF.

    DO table_entry_count TIMES.
      INSERT INITIAL LINE INTO TABLE itab->* ASSIGNING FIELD-SYMBOL(<line>).

      LOOP AT components ASSIGNING FIELD-SYMBOL(<comp>).
        DATA(tabix) = sy-tabix.

        IF <comp>-type IS INSTANCE OF cl_abap_elemdescr.
          DATA(tdo_elem) = CAST cl_abap_elemdescr( <comp>-type ).
          DATA(type_kind) = tdo_elem->type_kind.
          DATA(output_length) = tdo_elem->output_length.
          DATA(length) = tdo_elem->length.
          DATA(decimals) = tdo_elem->decimals.

          CASE type_kind.
            WHEN cl_abap_typedescr=>typekind_char.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_string( output_length ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_string( output_length ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_string.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_string( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_string( ).
              ENDIF.
            WHEN  cl_abap_typedescr=>typekind_hex.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_x( output_length ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_x( output_length ).
              ENDIF.
            WHEN  cl_abap_typedescr=>typekind_xstring.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_xstring( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_xstring( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_num.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_n( output_length ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_n( output_length ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_time.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_t( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_t( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_date.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_d( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_d( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_utclong.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_utclong( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_utclong( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_int
            OR cl_abap_typedescr=>typekind_int1 OR
            cl_abap_typedescr=>typekind_int2.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_i(  ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_i(  ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_int8.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_int8( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_int8( ).
              ENDIF.
            WHEN  cl_abap_typedescr=>typekind_packed.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_p(
                                  length    = length
                                  decimals  = decimals
                                )->*.
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_p(
                                     length    = length
                                     decimals  = decimals
                                   )->*.
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_decfloat16.
              IF is_elementary_line_type = abap_true.
                <line> =  lcl_itab_builder=>get_random_dec16( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_dec16( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_decfloat34.
              IF is_elementary_line_type = abap_true.
                <line> = lcl_itab_builder=>get_random_dec34( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_dec34( ).
              ENDIF.
            WHEN cl_abap_typedescr=>typekind_float.
              IF is_elementary_line_type = abap_true.
                <line> =  lcl_itab_builder=>get_random_f( ).
              ELSE.
                <line>-(tabix) = lcl_itab_builder=>get_random_f( ).
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDLOOP.
    ENDDO.
  ENDMETHOD.

  METHOD check_type.
    cl_abap_typedescr=>describe_by_name( EXPORTING p_name = type_name
                                         RECEIVING p_descr_ref = DATA(tdo)
                                         EXCEPTIONS type_not_found = 4 ).

    IF sy-subrc <> 0.
      type_exists = abap_false.
      RETURN.
    ELSE.
      type_exists = abap_true.
      line_type = CAST #( tdo ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_components.
    CASE TYPE OF line_type.
      WHEN TYPE cl_abap_structdescr.
        components = CAST cl_abap_structdescr( line_type )->get_components( ).
      WHEN TYPE cl_abap_elemdescr.
        is_elementary_line_type = abap_true.
        "Build the components table manually with TABLE_LINE to enable a loop..
        DATA(tdo_elem) = CAST cl_abap_elemdescr( line_type ).
        components = VALUE #( ( name = `TABLE_LINE` type = tdo_elem ) ).
      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_keys.
    "Checking if the provided key names are components of the structured type
    "In case of elementary line types, TABLE_LINE is used.
    IF primary_key_components IS NOT INITIAL.
      IF is_elementary_line_type = abap_true.
        CLEAR primary_key_components.
        APPEND `TABLE_LINE` TO primary_key_components.
      ELSE.
        DATA(comp_names) = CAST cl_abap_structdescr( line_type )->components.
        LOOP AT primary_key_components INTO DATA(prkey).
          IF NOT line_exists( components[ name = prkey ] ).
            DELETE primary_key_components WHERE table_line = prkey.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    "If no key names are provided, and the type refers to a global structured type such as
    "DDIC database tables or CDS entites, the following implementation collects those components
    "that are specified as keys for the repository object.
    IF primary_key_components IS INITIAL AND is_elementary_line_type = abap_false.
      "Checking whether the type is a global type, available as repository object.
      DATA(filter) = xco_cp_abap_repository=>object_name->get_filter( xco_cp_abap_sql=>constraint->equal( type_name ) ).
      DATA(repo_objects) = xco_cp_abap_repository=>objects->where( VALUE #( ( filter ) ) )->in( xco_cp_abap=>repository )->get( ).

      LOOP AT repo_objects INTO DATA(obj).
        DATA(val) = obj->type->value.
        IF val = `DDLS`.
          EXIT.
        ELSEIF val = `TABL`.
          EXIT.
        ENDIF.
      ENDLOOP.

      CASE val.
        WHEN `TABL`.
          "Retrieving the key component names of DDIC database tables
          primary_key_components = xco_cp_abap_repository=>object->tabl->database_table->for( CONV #( type_name ) )->fields->key->get_names( ).
          "Assuming the first key field in the list is the client field, removing this field from the key components.
          DELETE primary_key_components INDEX 1.
        WHEN `DDLS`.
          "Retrieving the key component names of CDS entities
          DATA(ddls_key_spec) = xco_cp_abap_repository=>object->ddls->for( CONV #( type_name ) )->entity( )->fields->all->get_names( ).
          LOOP AT ddls_key_spec INTO DATA(k).
            DATA(is_key) = xco_cp_abap_repository=>object->ddls->for( CONV #( type_name ) )->view_entity( )->field( k )->content( )->get_key_indicator( ).
            IF is_key IS NOT INITIAL.
              APPEND k TO primary_key_components.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDIF.

    "If the primary key table is still empty, e.g. when referring to an CDS abstract entity, the following implementation
    "adds the first component as key component.
    IF primary_key_components IS INITIAL AND is_elementary_line_type = abap_false.
      comp_names = CAST cl_abap_structdescr( line_type )->components.
      IF lines( comp_names ) > 0.
        APPEND comp_names[ 1 ]-name TO primary_key_components.
      ENDIF.
    ENDIF.

    "If an elementary line type is used, and the primary key table is still empty, adding TABLE_LINE.
    IF primary_key_components IS INITIAL AND is_elementary_line_type = abap_true.
      APPEND `TABLE_LINE` TO primary_key_components.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Concrete example classes inheriting from the abstract class
"The example concrete builder classes construct internal table of different
"kinds. Among them, standard tables with non-unique primary table key,
"standard tables with empty key, sorted and hashed tables.
"Additionally, a concrete builder class creates random data for an internal
"table that is supplied when calling the object creation method.

CLASS lcl_builder_std_itab_w_prkey DEFINITION INHERITING FROM lcl_itab_builder CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: build_type_info REDEFINITION,
      build_table_type REDEFINITION,
      build_data_object REDEFINITION,
      build_random_data REDEFINITION,
      build_table_keys REDEFINITION,
      build_components REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_builder_std_itab_w_prkey IMPLEMENTATION.

  METHOD build_type_info.
    check_type( ).
  ENDMETHOD.

  METHOD build_components.
    CHECK type_exists = abap_true.
    handle_components( ).
  ENDMETHOD.

  METHOD build_table_keys.
    CHECK type_exists = abap_true.
    handle_keys( ).
  ENDMETHOD.

  METHOD build_table_type.
    CHECK type_exists = abap_true.

    table_type_descr_obj = cl_abap_tabledescr=>get(
          p_line_type  = line_type
          p_table_kind = cl_abap_tabledescr=>tablekind_std
          p_key        = VALUE #( FOR wa IN primary_key_components ( name = wa ) )
          p_unique     = cl_abap_typedescr=>false ).
  ENDMETHOD.

  METHOD build_data_object.
    CHECK type_exists = abap_true.
    CREATE DATA itab TYPE HANDLE table_type_descr_obj.
    "Note: As the keys are non-unique, no temporary table is created as in other concrete
    "builder classes.
  ENDMETHOD.

  METHOD build_random_data.
    CHECK type_exists = abap_true.
    add_table_entries( table_entries_to_create ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_builder_std_itab_empty_key DEFINITION INHERITING FROM lcl_itab_builder CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: build_type_info REDEFINITION,
      build_table_keys REDEFINITION,
      build_table_type REDEFINITION,
      build_data_object REDEFINITION,
      build_random_data REDEFINITION,
      build_components REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_builder_std_itab_empty_key IMPLEMENTATION.
  METHOD build_type_info.
    check_type( ).
  ENDMETHOD.

  METHOD build_components.
    CHECK type_exists = abap_true.
    handle_components( ).
  ENDMETHOD.

  METHOD build_table_keys.
    CHECK type_exists = abap_true.
    CLEAR primary_key_components.
  ENDMETHOD.

  METHOD build_table_type.
    CHECK type_exists = abap_true.

    table_type_descr_obj = cl_abap_tabledescr=>get(
           p_line_type  = line_type
           p_table_kind = cl_abap_tabledescr=>tablekind_std
           p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).
  ENDMETHOD.

  METHOD build_data_object.
    CHECK type_exists = abap_true.
    CREATE DATA itab TYPE HANDLE table_type_descr_obj.
  ENDMETHOD.

  METHOD build_random_data.
    CHECK type_exists = abap_true.
    add_table_entries( table_entries_to_create ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_builder_sorted_itab DEFINITION INHERITING FROM lcl_itab_builder CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: build_type_info REDEFINITION,
      build_table_keys REDEFINITION,
      build_table_type REDEFINITION,
      build_data_object REDEFINITION,
      build_random_data REDEFINITION,
      build_components REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    "Attributes for tables that stores content and the table type
    DATA temporary_itab TYPE REF TO data.
    DATA temporary_table_type TYPE REF TO cl_abap_tabledescr.
ENDCLASS.

CLASS lcl_builder_sorted_itab IMPLEMENTATION.
  METHOD build_type_info.
    check_type( ).
  ENDMETHOD.

  METHOD build_components.
    CHECK type_exists = abap_true.
    handle_components( ).
  ENDMETHOD.

  METHOD build_table_keys.
    CHECK type_exists = abap_true.
    handle_keys( ).
  ENDMETHOD.

  METHOD build_table_type.
    CHECK type_exists = abap_true.

    table_type_descr_obj = cl_abap_tabledescr=>get(
           p_line_type  = line_type
           p_table_kind = cl_abap_tabledescr=>tablekind_sorted
           p_key        = VALUE #( FOR wa IN primary_key_components ( name = wa ) )
           p_unique     = cl_abap_typedescr=>true ).

    "In the previous statement, the type description object for the created table type
    "is assigned. The following assignment is performed for a temporary table.
    "This is done to not interfere with unique primary table keys and non-modifiable components
    "when adding table entries (because the creation procedure shared by all objects is performed
    "using the 'itab' attribute. Therefore, a standard table with empty key is created using the
    "same line type. The table entries are added to this table. In a later step, the table entries
    "are copied back to the original internal table.
    temporary_table_type = cl_abap_tabledescr=>get(
            p_line_type  = line_type
            p_table_kind = cl_abap_tabledescr=>tablekind_std
            p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).

  ENDMETHOD.

  METHOD build_data_object.
    CHECK type_exists = abap_true.

    "Creating two internal tables
    "See the comment in the build_table_type method.
    CREATE DATA itab TYPE HANDLE temporary_table_type.
    CREATE DATA temporary_itab TYPE HANDLE table_type_descr_obj.
  ENDMETHOD.

  METHOD build_random_data.
    CHECK type_exists = abap_true.
    add_table_entries( table_entries_to_create ).

    "See the comment in the build_table_type method.
    LOOP AT itab->* ASSIGNING FIELD-SYMBOL(<a>).
      INSERT <a> INTO TABLE temporary_itab->*.
    ENDLOOP.

    CLEAR itab.
    CREATE DATA itab LIKE temporary_itab->*.

    LOOP AT temporary_itab->* ASSIGNING FIELD-SYMBOL(<b>).
      INSERT <b> INTO TABLE itab->*.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_builder_hashed_itab DEFINITION INHERITING FROM lcl_itab_builder CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: build_type_info REDEFINITION,
      build_table_keys REDEFINITION,
      build_table_type REDEFINITION,
      build_data_object REDEFINITION,
      build_random_data REDEFINITION,
      build_components REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    DATA temporary_itab TYPE REF TO data.
    DATA temporary_table_type TYPE REF TO cl_abap_tabledescr.
ENDCLASS.

CLASS lcl_builder_hashed_itab IMPLEMENTATION.

  METHOD build_type_info.
    check_type( ).
  ENDMETHOD.

  METHOD build_components.
    CHECK type_exists = abap_true.
    handle_components( ).
  ENDMETHOD.

  METHOD build_table_keys.
    CHECK type_exists = abap_true.
    handle_keys( ).
  ENDMETHOD.

  METHOD build_table_type.
    CHECK type_exists = abap_true.

    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    table_type_descr_obj = cl_abap_tabledescr=>get(
           p_line_type  = line_type
           p_table_kind = cl_abap_tabledescr=>tablekind_hashed
           p_key        = VALUE #( FOR wa IN primary_key_components ( name = wa ) )
           p_unique     = cl_abap_typedescr=>true ).

    temporary_table_type = cl_abap_tabledescr=>get(
            p_line_type  = line_type
            p_table_kind = cl_abap_tabledescr=>tablekind_std
            p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).
  ENDMETHOD.

  METHOD build_data_object.
    CHECK type_exists = abap_true.

    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    CREATE DATA itab TYPE HANDLE temporary_table_type.
    CREATE DATA temporary_itab TYPE HANDLE table_type_descr_obj.
  ENDMETHOD.

  METHOD build_random_data.
    CHECK type_exists = abap_true.
    add_table_entries( table_entries_to_create ).

    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    LOOP AT itab->* ASSIGNING FIELD-SYMBOL(<a>).
      INSERT <a> INTO TABLE temporary_itab->*.
    ENDLOOP.

    CLEAR itab.
    CREATE DATA itab LIKE temporary_itab->*.

    LOOP AT temporary_itab->* ASSIGNING FIELD-SYMBOL(<b>).
      INSERT <b> INTO TABLE itab->*.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_itab_data_provider DEFINITION INHERITING FROM lcl_itab_builder CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: build_type_info REDEFINITION,
      build_table_keys REDEFINITION,
      build_table_type REDEFINITION,
      build_data_object REDEFINITION,
      build_random_data REDEFINITION,
      build_components REDEFINITION.

    METHODS constructor IMPORTING itab TYPE ANY TABLE.
  PROTECTED SECTION.
  PRIVATE SECTION.
    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    DATA temporary_itab TYPE REF TO data.
    DATA temporary_table_type TYPE REF TO cl_abap_tabledescr.
ENDCLASS.

CLASS lcl_itab_data_provider IMPLEMENTATION.

  METHOD build_type_info.
    line_type = table_type_descr_obj->get_table_line_type( ).
  ENDMETHOD.

  METHOD build_components.
    handle_components( ).
  ENDMETHOD.

  METHOD build_table_keys.
    "The table type including the key components is provided by the table supplied.
    CLEAR primary_key_components.
  ENDMETHOD.

  METHOD build_table_type.
    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    temporary_table_type = cl_abap_tabledescr=>get(
            p_line_type  = line_type
            p_table_kind = cl_abap_tabledescr=>tablekind_std
            p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).
  ENDMETHOD.

  METHOD build_data_object.
    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    CREATE DATA itab TYPE HANDLE temporary_table_type.
    CREATE DATA temporary_itab TYPE HANDLE table_type_descr_obj.
  ENDMETHOD.

  METHOD build_random_data.
    add_table_entries( table_entries_to_create ).

    "See the comment in the build_table_type method of class lcl_builder_sorted_itab.
    LOOP AT itab->* ASSIGNING FIELD-SYMBOL(<a>).
      INSERT <a> INTO TABLE temporary_itab->*.
    ENDLOOP.

    CLEAR itab.
    CREATE DATA itab LIKE temporary_itab->*.

    LOOP AT temporary_itab->* ASSIGNING FIELD-SYMBOL(<b>).
      INSERT <b> INTO TABLE itab->*.
    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    table_type_descr_obj = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab ) ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Class that includes factory methods that provide objects of the
"concrete builder classes
"The class is used by the consumers such as the global class in this
"example. It is responsible for a correct order of method calls so that
"an appropriate and correct object is returned.

CLASS lcl_itab_provider DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM table_kind,
             standard_w_nonunique_pr_key,
             sorted_w_unique_pr_key,
             hashed_w_unique_pr_key,
             standard_w_empty_key,
           END OF ENUM table_kind.

    CLASS-METHODS create_itab IMPORTING VALUE(type_name)  TYPE string
                                        table_kind        TYPE lcl_itab_provider=>table_kind DEFAULT standard_w_empty_key
                                        key_components    TYPE string_table OPTIONAL
                                        table_entry_count TYPE i DEFAULT 3
                              RETURNING VALUE(build)      TYPE REF TO lcl_itab_builder.

    CLASS-METHODS populate_itab IMPORTING itab              TYPE ANY TABLE
                                          table_entry_count TYPE i DEFAULT 3
                                RETURNING VALUE(build)      TYPE REF TO lcl_itab_builder.
ENDCLASS.

CLASS lcl_itab_provider IMPLEMENTATION.
  METHOD create_itab.
    type_name = to_upper( condense( val = type_name to = `` ) ).

    CASE table_kind.
      WHEN standard_w_nonunique_pr_key.
        build = NEW lcl_builder_std_itab_w_prkey( ).
      WHEN standard_w_empty_key.
        build = NEW lcl_builder_std_itab_empty_key( ).
      WHEN sorted_w_unique_pr_key.
        build = NEW lcl_builder_sorted_itab( ).
      WHEN hashed_w_unique_pr_key.
        build = NEW lcl_builder_hashed_itab( ).
      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

    build->type_name = type_name.
    build->table_entries_to_create = table_entry_count.
    build->primary_key_components = key_components.
    build->build_type_info( ).
    build->build_components( ).
    build->build_table_keys( ).
    build->build_table_type( ).
    build->build_data_object( ).
    build->build_random_data( ).
  ENDMETHOD.

  METHOD populate_itab.
    build = NEW lcl_itab_data_provider( itab ).
    build->table_entries_to_create = table_entry_count.
    build->build_type_info( ).
    build->build_components( ).
    build->build_table_type( ).
    build->build_data_object( ).
    build->build_random_data( ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Chain of Responsibility

- The idea behind the chain of responsibility design pattern is organizing handlers in a sequence (a chain), allowing for flexible request processing, potentially without knowing which handler will handle the request at compile time.
- Requests move through this chain of handlers until one is found that can process it.
- A potential setup, illustrated in the following example, may look like this:
  - An interface defines methods for processing requests.
  - An abstract handler class that implements the interface, includes default behavior, and maintains a reference to the next handler in the chain.
  - Concrete handler classes that inherit from the abstract handler, deciding whether to process the request or pass it to the next handler.
  - A client that establishes the chain of responsibility and sends requests.
- This pattern may be useful for creating flexible and maintainable setups, especially when processing various types of requests differently, managing multiple handlers without knowing in advance which one will actually handle the requests, following indeed a specific processing sequence, enabling easy extension by adding new concrete handlers, and more.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- The example demonstrates the chain of responsibility design pattern through the following declarations and implementations. Note that the example is simplified, and various class setup strategies may apply.
- As example context, it uses the retrieval of type information (RTTI) based on type description objects. For more details on RTTI and type description objects, refer to the Dynamic Programming cheat sheet. Different handler classes handle specific type description objects, including handlers for elementary, structured, and table types. Classes that can handle other types are not implemented. They should be handled by a default handler class.
- Global class:
  - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
  - Acts as the client to demonstrate the design pattern. The declarations and implementations in the *CCIMP include* are relevant for conceptual considerations.
- CCIMP include (Local Types tab in ADT):
  - Interface `lif_chain_of_resp`:
    - Defines the shared interface for concrete handler classes in the chain of responsibility.
    - Includes the `process` method for processing type description objects and returning type information (mutliple pieces of information are stored in a string table for display purposes), and the `set_handler` method for assigning the next handler in the chain.
  - Class `lcl_base_handler` 
    - Represents an abstract handler class that provides default implementations for concrete handler classes inheriting from it.
    - Since it is defined as an abstract class, it cannot be instantiated directly. Only its subclasses can be instantiated.
    - The class maintains a reference to the next handler in the protected `iref_handler` attribute.
    - By implementing the `lif_chain_of_resp` interface, it provides the methods `lif_chain_of_resp~set_handler` for setting the next handler and `lif_chain_of_resp~process` for processing requests.
  - `lcl_concrete_handler*` classes
    - Represents concrete handler classes that inherit from the `lcl_base_handler` class.
    - These classes provide specific implementations for handling different types of type description objects.
    - The example includes handler classes for processing elementary, structured, and table types. Other types are not addressed in detail.
    - If a class cannot handle the request (because the type description object has another type), it calls the next handler in the chain. For types not covered by the example, the default handler class `lcl_concrete_handler4` can manage requests and provide general information (no casting is implemented to obtain more specific type information as in other concrete handler classes).
- The purpose of the example is to illustrate the following:
  - The client, represented by the implementations in the global class, determines the chain of responsibility by setting the concrete sequence of handler calls and starting requests.
  - These requests (represented by passing a type description object) are processed by handler class objects.
  - Each handler in the chain attempts to process the request. If a handler can handle the respective type description, it processes it. Otherwise, it delegates the request to the next handler in the chain.

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS process_chain IMPORTING type_description_object TYPE REF TO cl_abap_typedescr
                                    example                 TYPE string
                                    out                     TYPE REF TO if_oo_adt_classrun_out.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------*
*& Example 1: Elementary type (local type)
*&---------------------------------------------------------------------*

    TYPES packed TYPE p LENGTH 16 DECIMALS 14.
    DATA(type_descr_obj) = cl_abap_typedescr=>describe_by_name( 'PACKED' ).

    process_chain(
      type_description_object = type_descr_obj
      example = `1) Elementary type (local type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Example 2: Elementary type (global DDIC type)
*&---------------------------------------------------------------------*

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'LAND1' ).

    process_chain(
      type_description_object = type_descr_obj
      example = `2) Elementary type (global DDIC type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Example 3: Structured type (local type)
*&---------------------------------------------------------------------*

    TYPES: BEGIN OF demo_struc,
             comp1 TYPE c LENGTH 3,
             comp2 TYPE i,
             comp3 TYPE string,
             comp4 TYPE n LENGTH 5,
           END OF demo_struc.

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'DEMO_STRUC' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `3) Structured type (local type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Example 4: Structured type (global type)
*&---------------------------------------------------------------------*

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'I_TIMEZONE' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `4) Structured type (global type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Example 5: Table type (local type)
*&---------------------------------------------------------------------*

    TYPES tab_type TYPE HASHED TABLE OF demo_struc
      WITH UNIQUE KEY comp1 comp2
      WITH NON-UNIQUE SORTED KEY sk COMPONENTS comp4.

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'TAB_TYPE' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `5) Table type (local type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Example 6: Table type (global type)
*&---------------------------------------------------------------------*

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'STRING_TABLE' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `6) Table type (global type)`
      out                     = out
    ).

*&---------------------------------------------------------------------*
*& Examples for types that are not covered by concrete handler classes
*&---------------------------------------------------------------------*

    "Enumerated type
    TYPES: BEGIN OF ENUM t_enum,
             a,
             b,
             c,
             d,
           END OF ENUM t_enum.

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'T_ENUM' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `7) Enumerated type`
      out                     = out
    ).

    "Reference type
    TYPES ref TYPE REF TO string.

    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'REF' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `8) Reference type`
      out                     = out
    ).

    "Class
    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'CL_SYSTEM_UUID' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `9) Class`
      out                     = out
    ).

    "Interface
    type_descr_obj = cl_abap_typedescr=>describe_by_name( 'IF_OO_ADT_CLASSRUN' ).

    process_chain(
      type_description_object = type_descr_obj
      example                 = `10) Interface`
      out                     = out
    ).
  ENDMETHOD.

  METHOD process_chain.
    DATA: handler1 TYPE REF TO lif_chain_of_resp,
          handler2 TYPE REF TO lif_chain_of_resp,
          handler3 TYPE REF TO lif_chain_of_resp,
          handler4 TYPE REF TO lif_chain_of_resp,
          info_tab TYPE string_table.

    handler1 = NEW lcl_concrete_handler1( ).
    handler2 = NEW lcl_concrete_handler2( ).
    handler3 = NEW lcl_concrete_handler3( ).
    handler4 = NEW lcl_concrete_handler4( ).

    handler1->set_handler( handler2 ).
    handler2->set_handler( handler3 ).
    handler3->set_handler( handler4 ).

    DATA(info) = handler1->process( type_description_object ).

    IF info IS INITIAL.
      APPEND |-------------------- { example } --------------------| TO info.
    ELSE.
      INSERT LINES OF VALUE string_table( ( |-------------------- { example } --------------------| ) ( ) ) INTO info INDEX 1.
    ENDIF.

    out->write( info ).
    out->write( |\n{ repeat( val = `*` occ = 100 ) }\n\n| ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Interface
*&---------------------------------------------------------------------*

INTERFACE lif_chain_of_resp.
  METHODS: process IMPORTING type_descr  TYPE REF TO cl_abap_typedescr
                   RETURNING VALUE(info) TYPE string_table,
    set_handler IMPORTING iref TYPE REF TO lif_chain_of_resp.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Abstract handler class
*&---------------------------------------------------------------------*

CLASS lcl_base_handler DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_chain_of_resp.
  PROTECTED SECTION.
    DATA iref_handler TYPE REF TO lif_chain_of_resp.
ENDCLASS.

CLASS lcl_base_handler IMPLEMENTATION.
  METHOD lif_chain_of_resp~set_handler.
    me->iref_handler = iref.
  ENDMETHOD.

  METHOD lif_chain_of_resp~process.
    IF iref_handler IS INITIAL.
      APPEND `Cannot handle request` TO info.
    ELSE.
      info = iref_handler->process( type_descr ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete handler classes
*&---------------------------------------------------------------------*

CLASS lcl_concrete_handler1 DEFINITION INHERITING FROM lcl_base_handler.
  PUBLIC SECTION.
    METHODS lif_chain_of_resp~process REDEFINITION.
ENDCLASS.

CLASS lcl_concrete_handler1 IMPLEMENTATION.
  METHOD lif_chain_of_resp~process.
    DATA(cl_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->get_relative_name( ).

    IF type_descr IS INSTANCE OF cl_abap_elemdescr
    AND type_descr IS NOT INSTANCE OF cl_abap_enumdescr.
      APPEND |Class { cl_name } handled request.| TO info.
      APPEND `--- Type information ---` TO info.

      DATA(tdo_elem) = CAST cl_abap_elemdescr( type_descr ).
      DATA(type_category_elem) = tdo_elem->kind.
      DATA(type_kind_elem) = tdo_elem->type_kind.
      DATA(decimals_elem) = tdo_elem->decimals.
      DATA(output_length_elem) = tdo_elem->output_length.
      DATA(absolute_name_elem) = tdo_elem->absolute_name.
      DATA(relative_name_elem) = tdo_elem->get_relative_name( ).
      DATA(is_ddic_type_elem) = tdo_elem->is_ddic_type( ).

      APPEND |Type category: { type_category_elem }| TO info.
      APPEND |Type kind: { type_kind_elem }| TO info.
      APPEND |Decimals: { decimals_elem }| TO info.
      APPEND |Output length: { output_length_elem }| TO info.
      APPEND |Absolute name: { absolute_name_elem }| TO info.
      APPEND |Relative name: { relative_name_elem }| TO info.
      APPEND |Is DDIC type: { COND #( WHEN is_ddic_type_elem = abap_true THEN abap_true ELSE `-` ) }| TO info.
    ELSE.
      info = super->lif_chain_of_resp~process( type_descr ).
      IF info IS INITIAL.
        APPEND |Class { cl_name } could not handle request.| TO info.
      ELSE.
        INSERT |Class { cl_name } could not handle request.| INTO info INDEX 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_concrete_handler2 DEFINITION INHERITING FROM lcl_base_handler.
  PUBLIC SECTION.
    METHODS lif_chain_of_resp~process REDEFINITION.
ENDCLASS.

CLASS lcl_concrete_handler2 IMPLEMENTATION.
  METHOD lif_chain_of_resp~process.
    DATA(cl_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->get_relative_name( ).

    IF type_descr IS INSTANCE OF cl_abap_structdescr.
      APPEND |Class { cl_name } handled request.| TO info.
      APPEND `--- Type information ---` TO info.

      DATA(tdo_struc) = CAST cl_abap_structdescr( type_descr ).
      DATA(type_category_struc) = tdo_struc->kind.
      DATA(type_kind_struc) = tdo_struc->type_kind.
      DATA(absolute_name_struc) = tdo_struc->absolute_name.
      DATA(relative_name_struc) = tdo_struc->get_relative_name( ).
      DATA(is_ddic_type_struc) = tdo_struc->is_ddic_type( ).
      DATA(comps_struc) = tdo_struc->components.

      APPEND |Type category: { type_category_struc }| TO info.
      APPEND |Type kind: { type_kind_struc }| TO info.
      APPEND |Absolute name: { absolute_name_struc }| TO info.
      APPEND |Relative name: { relative_name_struc }| TO info.
      APPEND |Is DDIC type: { COND #( WHEN is_ddic_type_struc = abap_true THEN abap_true ELSE `-` ) }| TO info.
      APPEND |Components: { REDUCE string( INIT s = VALUE #( ) FOR wa IN comps_struc NEXT s &&= |{ COND #( WHEN s IS NOT INITIAL THEN ` ` ) }{ wa-name }| ) }| TO info.
    ELSE.
      info = super->lif_chain_of_resp~process( type_descr ).
      IF info IS INITIAL.
        APPEND |Class { cl_name } could not handle request.| TO info.
      ELSE.
        INSERT |Class { cl_name } could not handle request.| INTO info INDEX 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_concrete_handler3 DEFINITION INHERITING FROM lcl_base_handler.
  PUBLIC SECTION.
    METHODS lif_chain_of_resp~process REDEFINITION.
ENDCLASS.

CLASS lcl_concrete_handler3 IMPLEMENTATION.
  METHOD lif_chain_of_resp~process.
    DATA(cl_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->get_relative_name( ).

    IF type_descr IS INSTANCE OF cl_abap_tabledescr.
      APPEND |Class { cl_name } handled request.| TO info.
      APPEND `--- Type information ---` TO info.

      DATA(tdo_tab) = CAST cl_abap_tabledescr( type_descr ).
      DATA(type_category_tab) = tdo_tab->kind.
      DATA(type_kind_tab) = tdo_tab->type_kind.
      DATA(absolute_name_tab) = tdo_tab->absolute_name.
      DATA(relative_name_tab) = tdo_tab->get_relative_name( ).
      DATA(table_kind_itab) = tdo_tab->table_kind.
      DATA(table_has_unique_key_itab) = tdo_tab->has_unique_key.
      DATA(table_keys_itab) = tdo_tab->get_keys( ).
      DATA(table_is_ddic_typw) = tdo_tab->is_ddic_type( ).

      APPEND |Type category: { type_category_tab }| TO info.
      APPEND |Type kind: { type_kind_tab }| TO info.
      APPEND |Absolute name: { absolute_name_tab }| TO info.
      APPEND |Relative name: { relative_name_tab }| TO info.
      APPEND |Table kind: { table_kind_itab }| TO info.
      APPEND |Does table have a unique key: { COND #( WHEN table_has_unique_key_itab = abap_true THEN abap_true ELSE `-` ) }| TO info.
      APPEND |Table keys: { REDUCE string( INIT str = `` FOR <key2> IN table_keys_itab NEXT str = |{ str }| &&
             |{ COND #( WHEN str IS NOT INITIAL THEN `, ` ) }{ REDUCE string( INIT str2 = `` FOR <key3> IN <key2>-components NEXT str2 = |{ str2 }| &&
             |{ COND #( WHEN str2 IS NOT INITIAL THEN `/` ) }{ <key3>-name }| ) } (is primary: "{ <key2>-is_primary }", |  &&
             |is unique: "{ <key2>-is_unique }", key kind: "{ <key2>-key_kind }", access kind: "{ <key2>-access_kind }")| ) }| TO info.
      APPEND |Is DDIC type: { COND #( WHEN table_is_ddic_typw = abap_true THEN abap_true ELSE `-` ) }| TO info.
      TRY.
          DATA(table_component_info_itab) = CAST cl_abap_structdescr( tdo_tab->get_table_line_type( ) )->components.
          APPEND |Table components: { REDUCE string( INIT s = VALUE #( ) FOR w IN table_component_info_itab NEXT s &&= |{ COND #( WHEN s IS NOT INITIAL THEN ` ` ) }{ w-name }| ) }| TO info.
        CATCH cx_sy_move_cast_error.
          APPEND |Table components: The table has not a structured line type.| TO info.
      ENDTRY.
    ELSE.
      info = super->lif_chain_of_resp~process( type_descr ).
      IF info IS INITIAL.
        APPEND |Class { cl_name } could not handle request.| TO info.
      ELSE.
        INSERT |Class { cl_name } could not handle request.| INTO info INDEX 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_concrete_handler4 DEFINITION INHERITING FROM lcl_base_handler.
  PUBLIC SECTION.
    METHODS: lif_chain_of_resp~process REDEFINITION.
ENDCLASS.

CLASS lcl_concrete_handler4 IMPLEMENTATION.
  METHOD lif_chain_of_resp~process.
    DATA(cl_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->get_relative_name( ).
    APPEND |Class { cl_name } handled request.| TO info.
    APPEND `--- Type information ---` TO info.

    IF type_descr IS INITIAL.
      APPEND `Cannot extract type information` TO info.
    ELSE.
      DATA(type_category) = type_descr->kind.
      DATA(type_kind) = type_descr->type_kind.
      DATA(absolute_name) = type_descr->absolute_name.
      DATA(relative_name) = type_descr->get_relative_name( ).

      APPEND |Type category: { type_category }| TO info.
      APPEND |Type kind: { type_kind }| TO info.
      APPEND |Absolute name: { absolute_name }| TO info.
      APPEND |Relative name: { relative_name }| TO info.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Command

- The idea behind the command design pattern is to wrap operations to be executed and the details required for the operations inside individual objects.  
- By placing all details in these command objects, you can, for example, store these objects, pass them as arguments of method calls, execute them in a specific order, or delay their execution to a later time.  
- Typically, objects invoking commands (invokers) are separated from the objects that receive the commands and finally execute the operations (receivers). Basically, invokers can execute any command without knowing the execution details defined by the receivers.
- The pattern may be useful for creating a flexible setup that can be easily extended by adding new commands, without requiring a modification of existing classes. By storing commands, the setup can also support undo and redo functionalities.
- A potential setup, illustrated in the following example, may look like this:
  - Command interface (or an abstract class) 
    - Defines a method for executing commands.
    - The method can be defined without any input parameters to have a generic approach for various kinds of operations. The actual data to work with can be assigned to instance attributes of command objects.
  - Concrete command classes 
    - These classes implement the command interface.
    - They execute the command by calling appropriate methods implemented by receiver objects, i.e. the actual operation is performed by the receivers.
  - Receiver class 
    - This class contains the implementation for executing the operations defined by the commands. 
    - Methods in the receiver class are called by the concrete command classes.
  - Invoker class
    - Responsible for triggering command execution.
    - Invokers are unaware of the implementation details.
  - Client 
    - Creates command objects and connects them with suitable receiver objects.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- The example demonstrates the command design pattern at a high level using the following context:
  - Assume there is a simplified image editor app.
  - The app may offer various functionalities, such as resizing and rotating images.
  - In the app, a user edits an image by triggering resizing and rotation - each operation represents an individual command object - by providing specific values, such as width and height to resize the image.
  - Additionally, the app supports undo and redo functionality.
- The code demonstrates the design pattern with the following declarations and implementations:  
  - CCIMP Include (_Local Types_ Tab in ADT):
    - Command interface `lif_command`
      - Defines a method for executing commands (`execute`) and a method for reverting executed commands (`undo`).
    - Receiver class `lcl_receiver`
      - Executes commands by implementing specific methods. In the image editor example, it includes methods like `resize` and `rotate` to edit images and change object states based on commands.
      - The instance constructor initializes the image's values, which are provided during object creation for simplicity.
      - The `get_current_state` method is implemented for logging and display purposes.
    - Concrete command classes `lcl_cmd_resize` and `lcl_cmd_rotate`
      - Implement the command interface.
      - Objects of these classes represent specific commands.
      - Each object of the classes holds a reference to the receiver (stored in the `rec` attribute), passed via the constructor. Details of the command are stored in instance attributes (`wd`, `ht`, `ang`), which the operation uses for execution.
      - The `execute` method calls appropriate methods through the receiver objects.
      - The `undo` method reverts changes by storing previous values in instance attributes during the `execute` method.
    - Invoker class `lcl_invoker`
      - Executes commands through the `exec_cmd` method.
      - In this example, the command object is passed as an argument to the instance constructor.
      - It includes internal tables that hold references for undo and redo operations (`undo_tab` and `redo_tab`). When executing a command via `exec_cmd`, a reference is added to `undo_tab`, and `redo_tab` is cleared. The `redo` method executes the last command from `redo_tab` and adds it to `undo_tab`.      
  - Global class
    - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
    - Acts as the client.
    - Creates objects of the receiver, invoker, and commands.
    - Command objects are created with references to the receiver and the necessary execution details.


<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-DATA log TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------*
*& Creating receiver, invoker and command objects and triggering
*& the execution of commands
*&---------------------------------------------------------------------*

    "Creating an object of the receiver class representing a
    "simplified image editor app
    "Here, start values for the image are supplied, which are
    "reflected in the current state that is retrieved.
    DATA(receiver) = NEW lcl_receiver( width = 800 height = 600 ).

    "For logging and display purposes, the current state of the
    "image is retrieved. The current state is retrieved and output
    "after each operation in the example.
    DATA(state) = receiver->get_current_state( ).
    out->write( state ).

    "Creating an object of the invoker class
    DATA(invoker) = NEW lcl_invoker( ).

    "Creating concrete commands by linking them to the receiver
    "In the example, a reference of the receiver class is passed as
    "argument of the instance constructor.

    "Resizing operation (1)
    "Here and in all other concrete command creations, values are
    "passed representing new values that should be applied for the
    "operation.

    DATA(cmd1) = NEW lcl_cmd_resize( receiver = receiver width = 1024 height = 768 ).

    "Passing the command object to the invoker that takes care of
    "executing the command. However, the invoker does not know about
    "the concrete details of the operation.
    invoker->exec_cmd( cmd1 ).

    state = receiver->get_current_state( ).
    out->write( state ).

    "Resizing operation (2)
    DATA(cmd2) = NEW lcl_cmd_resize( receiver = receiver width = 1920 height = 1080 ).
    invoker->exec_cmd( cmd2 ).

    state = receiver->get_current_state( ).
    out->write( state ).

    "Rotating operation (1)
    DATA(cmd3) = NEW lcl_cmd_rotate( receiver = receiver angle = 20 ).
    invoker->exec_cmd( cmd3 ).

    state = receiver->get_current_state( ).
    out->write( state ).

    "Rotating operation (2)
    DATA(cmd4) = NEW lcl_cmd_rotate( receiver = receiver angle = 45 ).
    invoker->exec_cmd( cmd4 ).

    state = receiver->get_current_state( ).
    out->write( state ).

    "Resizing operation (3)
    DATA(cmd5) = NEW lcl_cmd_resize( receiver = receiver width = 1280  height = 720  ).
    invoker->exec_cmd( cmd5 ).

    state = receiver->get_current_state( ).
    out->write( state ).

    "Rotating operation (3)
    DATA(cmd6) = NEW lcl_cmd_rotate( receiver = receiver angle = 180 ).
    invoker->exec_cmd( cmd6 ).

    state = receiver->get_current_state( ).
    out->write( state ).

*&---------------------------------------------------------------------*
*& Undo operations
*&---------------------------------------------------------------------*

    out->write( |\n{ repeat( val = `*` occ = 70 ) }\n| ).
    out->write( `********* Undo operations *********` ).

    DO 3 TIMES.
      invoker->undo( ).

      state = receiver->get_current_state( ).
      out->write( state ).
    ENDDO.

*&---------------------------------------------------------------------*
*& Redo operations
*&---------------------------------------------------------------------*

    out->write( |\n{ repeat( val = `*` occ = 70 ) }\n| ).
    out->write( `********* Redo operations *********` ).

    DO 5 TIMES.
      invoker->redo( ).

      state = receiver->get_current_state( ).
      out->write( state ).
    ENDDO.

*&---------------------------------------------------------------------*
*& Log
*&---------------------------------------------------------------------*

    out->write( |\n{ repeat( val = `*` occ = 70 ) }\n| ).
    out->write( `********* Log *********` ).
    out->write( log ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Command interface
*&---------------------------------------------------------------------*

INTERFACE lif_command.
  METHODS: execute,
    undo.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Receiver class
*&---------------------------------------------------------------------*

CLASS lcl_receiver DEFINITION.
  PUBLIC SECTION.
    DATA: wd  TYPE i,
          ht  TYPE i,
          ang TYPE i.

    METHODS:
      constructor IMPORTING width TYPE i height TYPE i,
      resize IMPORTING width TYPE i height TYPE i,
      rotate IMPORTING angle TYPE i,
      get_current_state RETURNING VALUE(state) TYPE string.
ENDCLASS.

CLASS lcl_receiver IMPLEMENTATION.
  METHOD constructor.
    wd  = width.
    ht = height.
    ang  = 0.
  ENDMETHOD.

  METHOD resize.
    wd  = width.
    ht = height.

    APPEND |New image size: { wd }x{ ht }| TO zcl_demo_abap=>log.
  ENDMETHOD.

  METHOD rotate.
    ang = ( ang + angle ) MOD 360.

    APPEND |Image rotated to { ang } (angle used: { angle })| TO zcl_demo_abap=>log.
  ENDMETHOD.

  METHOD get_current_state.
    state = |Current image state: { wd }x{ ht }, angle { ang } |.
    APPEND state TO zcl_demo_abap=>log.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete command classes
*&---------------------------------------------------------------------*

CLASS lcl_cmd_resize DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS: constructor IMPORTING receiver TYPE REF TO object
                                   width    TYPE i
                                   height   TYPE i.
  PRIVATE SECTION.
    DATA: rec             TYPE REF TO object,
          wd              TYPE i,
          ht              TYPE i,
          previous_width  TYPE i,
          previous_height TYPE i.
ENDCLASS.

CLASS lcl_cmd_resize IMPLEMENTATION.
  METHOD constructor.
    rec = receiver.
    wd  = width.
    ht = height.
  ENDMETHOD.

  METHOD lif_command~execute.
    IF rec IS BOUND AND rec IS INSTANCE OF lcl_receiver.
      DATA(img) = CAST lcl_receiver( rec ).
      previous_width  = img->wd.
      previous_height = img->ht.
      img->resize( width = wd height = ht ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_command~undo.
    IF rec IS BOUND AND rec IS INSTANCE OF lcl_receiver.
      DATA(img) = CAST lcl_receiver( rec ).
      DATA(current_wd) = img->wd.
      DATA(current_ht) = img->ht.
      img->resize( width = previous_width height = previous_height ).
      APPEND |Resizing undone: { previous_width }x{ previous_height } (previous value: { current_wd }x{ current_ht })| TO zcl_demo_abap=>log.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_cmd_rotate DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS: constructor IMPORTING receiver TYPE REF TO object
                                   angle    TYPE i.
  PRIVATE SECTION.
    DATA: rec            TYPE REF TO object,
          ang            TYPE i,
          previous_angle TYPE i.
ENDCLASS.

CLASS lcl_cmd_rotate IMPLEMENTATION.
  METHOD constructor.
    rec = receiver.
    ang = angle.
  ENDMETHOD.

  METHOD lif_command~execute.
    IF rec IS BOUND AND rec IS INSTANCE OF lcl_receiver.
      DATA(img) = CAST lcl_receiver( rec ).
      previous_angle = img->ang.
      img->rotate( angle = ang ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_command~undo.
    IF rec IS BOUND AND rec IS INSTANCE OF lcl_receiver.
      DATA(img) = CAST lcl_receiver( rec ).
      DATA(current_value) = img->ang.
      img->ang = previous_angle.
      APPEND |Rotation undone: { previous_angle } (previous value: { current_value })| TO zcl_demo_abap=>log.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Invoker classes
*&---------------------------------------------------------------------*

CLASS lcl_invoker DEFINITION.
  PUBLIC SECTION.
    METHODS:
      exec_cmd IMPORTING cmd TYPE REF TO lif_command,
      undo,
      redo.
  PRIVATE SECTION.
    DATA: undo_tab TYPE TABLE OF REF TO lif_command WITH EMPTY KEY,
          redo_tab TYPE TABLE OF REF TO lif_command WITH EMPTY KEY.
ENDCLASS.

CLASS lcl_invoker IMPLEMENTATION.
  METHOD exec_cmd.
    APPEND `---` TO zcl_demo_abap=>log.
    IF cmd IS BOUND.
      cmd->execute( ).
      APPEND cmd TO undo_tab.
      CLEAR redo_tab.
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD undo.
    APPEND `---` TO zcl_demo_abap=>log.
    APPEND `Undo operation triggered` TO zcl_demo_abap=>log.
    DATA(tabix) = lines( undo_tab ).
    READ TABLE undo_tab INTO DATA(cmd) INDEX tabix.
    IF sy-subrc = 0.
      cmd->undo( ).
      DELETE undo_tab INDEX tabix.
      APPEND cmd TO redo_tab.
    ELSE.
      APPEND `Undo operation not possible` TO zcl_demo_abap=>log.
    ENDIF.
  ENDMETHOD.

  METHOD redo.
    APPEND `---` TO zcl_demo_abap=>log.
    APPEND `Redo operation triggered` TO zcl_demo_abap=>log.
    DATA(tabix) = lines( redo_tab ).
    READ TABLE redo_tab INTO DATA(cmd) INDEX tabix.
    IF sy-subrc = 0.
      cmd->execute( ).
      DELETE redo_tab INDEX tabix.
      APPEND cmd TO undo_tab.
    ELSE.
      APPEND `Redo operation not possible` TO zcl_demo_abap=>log.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Decorator


- Enhances and modifies existing objects by wrapping them in other objects (objects of decorator classes) without modifying the original object setup and class code.
- Useful when flexibility is needed to handle various combinations and functionalities of objects.
- An exemplary class setup may include:
  - An interface that defines common functions
  - A base class implementing the interface; objects of this class are meant to be enhanced (decorated)
  - An abstract decorator class that delegates functionality enhancement
  - Concrete decorator classes that enhance existing objects
    - Objects of the base class can be wrapped in concrete decorator objects, enhancing functionality since all objects share features by implementing the interface
- Advantages:
  - Users can decorate objects with any decorator since all implement the interface.
  - All classes implement the interface, allowing all objects to be accessed through the same interface reference variable. 
  - Users can work with any base class object, be it a *direct* object of the base class or a decorated object.
  - The pattern offers flexibility as you can dynamically enhance objects using any decorators to achieve any desired object.
  - Maintenance can be simplified because each decorator class encapsulates specific functionality, and you can easily extend functionality by adding more decorator classes.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>


The following example demonstrates the decorator design pattern in a pizza creation context. Starting with a basic pizza (object), you can create various pizzas (decorate the pizza object) by adding ingredients, which also changes the pizza's cost:
 
- Global class:
  - Implements the `if_oo_adt_classrun` interface and calls methods from local classes in the CCIMP include.  
  - Demonstrates the decorator design pattern by creating an `lcl_basic_pizza` object. This object is enhanced (decorated) by wrapping it in objects of concrete decorator classes `lcl_decorator_*`.
  - You can run the class by choosing F9 in ADT. The example displays output in the ADT console.
- CCIMP include (*Local Types* tab in ADT):
  - `lif_pizza`
    - An interface defining a common setup for all decorators and decorated objects.
    - Implemented by all other local classes in the example, including the concrete base class and decorator classes.
    - Defines the methods `create_pizza` for pizza creation and `get_costs` for cost calculation.
  - `lcl_basic_pizza` 
    - Objects of the class represent basic objects that can be decorated.
    - Implements the `lif_pizza` interface.
    - Defines the instance `constructor` that specifies an integer as importing parameter to set the base cost.
  - `lcl_pizza_decorator`
    - Represents an abstract decorator class.
    - Implements the `lif_pizza` interface.
    - As an abstract class, object creation is restricted to its subclasses (the concrete decorator classes).
    - The instance constructor receives the decorated pizza object, linking concrete decorators to the object being decorated. This reference is assigned to an interface reference variable in the protected section, allowing subclass access (though not relevant for this example). Subclasses call the constructor.
    - The implemented interface methods delegate calls to the wrapped/decorated object using the interface reference variable.
  - `lcl_decorator_*` classes
    - Represent concrete decorator classes.
    - Inherit from the abstract decorator class `lcl_pizza_decorator`.
    - Their objects can wrap and enhance existing objects.
    - Since `lcl_pizza_decorator` implements `lif_pizza`, the `lcl_decorator_*` classes can also redefine the interface methods.
    - The superclass `lcl_pizza_decorator` defines an instance constructor expecting a reference (`REF TO lif_pizza`) as an importing parameter. It is mandatory that the subclasses call the superclass's constructor, yet it is not mandatory for the constructor to be explicitly defined and implemented. The subclasses in the example explicitly define and implement the instance constructor. You can, for example, check out the `lcl_decorator_salami` class, and remove the definition `METHODS constructor IMPORTING pizza TYPE REF TO lif_pizza.` and its implementation. There will not be syntax errors when creating objects in the global class using `lcl_decorator_salami`. Due to inheritance, a reference is still required to be passed for the importing parameter although not explicitly specified in the local subclass.
    - Implementations of interface methods in these classes make simple modifications, like adding ingredients and adjusting total costs. Each concrete decorator class introduces its own functionality, allowing dynamic stacking of decorations. The `super->...` calls trigger the calling of superclass implementations. Since the link to the decorated object is estrablished, ingredients are added to the existing ingredients, and costs are added to the existing costs.

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA(divider) = |{ repeat( val = `_` occ = 75 ) }\n|.

    "---------------- Basic pizza ----------------
    DATA(basic_pizza) = NEW lcl_basic_pizza( 10 ).
    DATA(basic_pizza_created) = basic_pizza->lif_pizza~create_pizza( ).
    DATA(basic_pizza_costs) = basic_pizza->lif_pizza~get_costs( ).

    out->write( `Basic pizza` ).
    out->write( |\nIngredients:| ).
    out->write( basic_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( basic_pizza_costs ).
    out->write( divider ).

    "---------------- Salami pizza ----------------
    DATA(salami_pizza) = NEW lcl_decorator_salami( pizza = basic_pizza ).
    DATA(salami_pizza_created) = salami_pizza->lif_pizza~create_pizza( ).
    DATA(salami_pizza_costs) = salami_pizza->lif_pizza~get_costs( ).

    out->write( `Salami pizza` ).
    out->write( |\nIngredients:| ).
    out->write( salami_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( salami_pizza_costs ).
    out->write( divider ).

    "---------------- Vegetable pizza ----------------
    DATA(vegetarian_pizza) = NEW lcl_decorator_vegetables( pizza = basic_pizza ).
    DATA(vegetarian_pizza_created) = vegetarian_pizza->lif_pizza~create_pizza( ).
    DATA(vegetarian_pizza_costs) = vegetarian_pizza->lif_pizza~get_costs( ).

    out->write( `Vegetable pizza` ).
    out->write( |\nIngredients:| ).
    out->write( vegetarian_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( vegetarian_pizza_costs ).
    out->write( divider ).

    "---------------- Vegetable/salami pizza ----------------
    DATA(vegetable_salami_pizza) = NEW lcl_decorator_vegetables(
      pizza = NEW lcl_decorator_salami(
      pizza = NEW lcl_basic_pizza( 10 ) ) ).
    DATA(vegetable_salami_pizza_created) = vegetable_salami_pizza->lif_pizza~create_pizza( ).
    DATA(vegetable_salami_pizza_costs) = vegetable_salami_pizza->lif_pizza~get_costs( ).

    out->write( `Vegetable/salami pizza` ).
    out->write( |\nIngredients:| ).
    out->write( vegetable_salami_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( vegetable_salami_pizza_costs ).
    out->write( divider ).

    "---------------- Mushroom/salami pizza ----------------
    DATA(mushrooms_salami_pizza) = NEW lcl_decorator_mushrooms(
      pizza = NEW lcl_decorator_salami(
      pizza = NEW lcl_basic_pizza( 10 ) ) ).
    DATA(mushrooms_salami_pizza_created) = mushrooms_salami_pizza->lif_pizza~create_pizza( ).
    DATA(mushrooms_salami_pizza_costs) = mushrooms_salami_pizza->lif_pizza~get_costs( ).

    out->write( `Mushrooms/salami pizza` ).
    out->write( |\nIngredients:| ).
    out->write( mushrooms_salami_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( mushrooms_salami_pizza_costs ).
    out->write( divider ).

    "--- Various pizzas (created using the same interface reference variable) ---
    DATA some_pizza TYPE REF TO lif_pizza.
    some_pizza = NEW lcl_basic_pizza( 10 ).
    DATA(some_pizza_created_a) = some_pizza->create_pizza( ).
    DATA(some_pizza_costs_a) = some_pizza->get_costs( ).

    some_pizza = NEW lcl_decorator_vegetables( pizza = some_pizza ).
    DATA(some_pizza_created_b) = some_pizza->create_pizza( ).
    DATA(some_pizza_costs_b) = some_pizza->get_costs( ).

    some_pizza = NEW lcl_decorator_salami( pizza = some_pizza ).
    DATA(some_pizza_created_c) = some_pizza->create_pizza( ).
    DATA(some_pizza_costs_c) = some_pizza->get_costs( ).

    "---------------- Allergy-friendly vegetable pizza ----------------
    DATA(allergy_friendly_vegetbl_pizza) = NEW lcl_decorator_allergy_friendly(
        pizza = NEW lcl_decorator_vegetables(
        pizza = NEW lcl_basic_pizza( 10 ) ) ).
    DATA(all_friendly_veg_pizza_created) = allergy_friendly_vegetbl_pizza->lif_pizza~create_pizza( ).
    DATA(all_friendly_veg_pizza_costs) = allergy_friendly_vegetbl_pizza->lif_pizza~get_costs( ).

    out->write( `Allergy-friendly vegetable pizza` ).
    out->write( |\nIngredients:| ).
    out->write( all_friendly_veg_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( all_friendly_veg_pizza_costs ).
    out->write( divider ).

    "---------------- Pizza with all ingredients ----------------
    "Using all concrete decorator classes of the example
    DATA(misc_pizza) = NEW lcl_decorator_allergy_friendly(
       pizza = NEW lcl_decorator_mushrooms(
       pizza = NEW lcl_decorator_salami(
       pizza = NEW lcl_decorator_vegetables(
       pizza = NEW lcl_basic_pizza( 10 ) ) ) ) ).
    DATA(misc_pizza_created) = misc_pizza->lif_pizza~create_pizza( ).
    DATA(misc_pizza_costs) = misc_pizza->lif_pizza~get_costs( ).

    out->write( `Pizza with all ingredients` ).
    out->write( |\nIngredients:| ).
    out->write( misc_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( misc_pizza_costs ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
**********************************************************************
"Interface

INTERFACE lif_pizza.
  METHODS create_pizza RETURNING VALUE(ingredients) TYPE string_table.
  METHODS get_costs RETURNING VALUE(costs) TYPE i.
ENDINTERFACE.

**********************************************************************
"Basic pizza to be decorated

CLASS lcl_basic_pizza DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_pizza.
    METHODS constructor IMPORTING basic_pizza_costs TYPE i.
  PRIVATE SECTION.
    DATA pizza_costs TYPE i.
ENDCLASS.

CLASS lcl_basic_pizza IMPLEMENTATION.
  METHOD constructor.
    pizza_costs = basic_pizza_costs.
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = VALUE #( ( `dough` ) ( `tomato sauce` ) ( `cheese` ) ).
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = pizza_costs.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Abstract decorator class

CLASS lcl_pizza_decorator DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_pizza.
    METHODS constructor IMPORTING pizza TYPE REF TO lif_pizza.
  PROTECTED SECTION.
    DATA decorated_pizza TYPE REF TO lif_pizza.
ENDCLASS.

CLASS lcl_pizza_decorator IMPLEMENTATION.
  METHOD constructor.
    decorated_pizza = pizza.
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = decorated_pizza->create_pizza( ).
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = decorated_pizza->get_costs( ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Concrete decorator classes

CLASS lcl_decorator_salami DEFINITION INHERITING FROM lcl_pizza_decorator.
  PUBLIC SECTION.
    METHODS constructor IMPORTING pizza TYPE REF TO lif_pizza.
    METHODS lif_pizza~create_pizza REDEFINITION.
    METHODS lif_pizza~get_costs REDEFINITION.
ENDCLASS.

CLASS lcl_decorator_salami IMPLEMENTATION.
  METHOD constructor.
    super->constructor( pizza ).
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = super->lif_pizza~create_pizza( ).
    APPEND `salami` TO ingredients.
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = super->lif_pizza~get_costs( ) + 1.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_decorator_mushrooms DEFINITION INHERITING FROM lcl_pizza_decorator.
  PUBLIC SECTION.
    METHODS constructor IMPORTING pizza TYPE REF TO lif_pizza.
    METHODS lif_pizza~create_pizza REDEFINITION.
    METHODS lif_pizza~get_costs REDEFINITION.
ENDCLASS.

CLASS lcl_decorator_mushrooms IMPLEMENTATION.
  METHOD constructor.
    super->constructor( pizza ).
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = super->lif_pizza~create_pizza( ).
    APPEND `mushrooms` TO ingredients.
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = super->lif_pizza~get_costs( ) + 1.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_decorator_vegetables DEFINITION INHERITING FROM lcl_pizza_decorator.
  PUBLIC SECTION.
    METHODS constructor IMPORTING pizza TYPE REF TO lif_pizza.
    METHODS lif_pizza~create_pizza REDEFINITION.
    METHODS lif_pizza~get_costs REDEFINITION.
ENDCLASS.

CLASS lcl_decorator_vegetables IMPLEMENTATION.
  METHOD constructor.
    super->constructor( pizza ).
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = super->lif_pizza~create_pizza( ).
    APPEND LINES OF VALUE string_table( ( `red pepper` ) ( `zucchini` ) ( `broccoli` ) ) TO ingredients.
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = super->lif_pizza~get_costs( ) + 2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_decorator_allergy_friendly DEFINITION INHERITING FROM lcl_pizza_decorator.
  PUBLIC SECTION.
    METHODS constructor IMPORTING pizza TYPE REF TO lif_pizza.
    METHODS lif_pizza~create_pizza REDEFINITION.
    METHODS lif_pizza~get_costs REDEFINITION.
ENDCLASS.

CLASS lcl_decorator_allergy_friendly IMPLEMENTATION.
  METHOD constructor.
    super->constructor( pizza ).
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = super->lif_pizza~create_pizza( ).
    REPLACE ALL OCCURRENCES OF `dough` IN TABLE ingredients WITH `gluten-free dough`.
    REPLACE ALL OCCURRENCES OF `cheese` IN TABLE ingredients WITH `lactose-free cheese`.
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = super->lif_pizza~get_costs( ) + 1.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Facade 

- Assume you must handle complex functionality that may involve creating objects from multiple classes and calling methods, maybe even in a specific sequence and involving certain dependencies. 
- The facade design pattern's purpose is to provide a simplified API, hiding the complexity from users and allowing them to achieve desired results without dealing with and knowing about underlying complexities. 
- Users can interact with this straightforward API instead of handling intricate steps and details.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- The simplified example uses a travel search context. Users receive information about available flights, hotels, and rental cars. This information is retrieved by creating instances of multiple classes and various method calls. A facade class provides a simplified API that manages complex underlying functionality. Users interact only with the facade class, eliminating the need to handle individual class objects directly.
- The example demonstrates the facade design pattern with the following declarations and implementations:
  - Global class:
    - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
    - Serves as a vehicle for demonstrating the design pattern. The declarations and implementations in the `CCIMP` are relevant for the for conceptual considerations.
    - The global class represents the user of the simplified API the facade class offers. 
  - CCIMP include (Local Types tab in ADT):    
    - Contains multiple local classes (`lcl_*`) that provide information about available flights, hotels and rental cars. 
    - The simple classes include a method that calls a method in another class. Information is retrieved based on origin, destination, and travel time span.   
    - The `lcl_travel_facade` class serves as the facade class. The `plan_travel` method includes object creations and method calls. For simplicity, the method returns information about available flights, hotels, and rental cars in a string table.
- The class execution involves the following:
  - The global class contains several method calls to the facade class. 
  - Based on the parameters passed, travel options are evaluated. As a result, a string table is output showing travel search information.

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    DATA(travel_object) = NEW lcl_travel_facade( ).
    DATA(travel_options_1) = travel_object->plan_travel(
                from      = 'Frankfurt'
                to        = 'Shanghai'
                arrival   = '20250511'
                departure = '20250521' ).

    out->write( travel_options_1 ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

    DATA(travel_options_2) = travel_object->plan_travel(
                from      = 'Frankfurt'
                to        = 'Shanghai'
                arrival   = '20250605'
                departure = '20250617' ).

    out->write( travel_options_2 ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

    DATA(travel_options_3) = travel_object->plan_travel(
                  from      = 'Frankfurt'
                  to        = 'Shanghai'
                  arrival   = '20250403'
                  departure = '20250410' ).

    out->write( travel_options_3 ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
"Class that includes a method to get flights from a data source
"In this simple, self-contained example, the data source is
"simulated by an internal table that includes demo data.
CLASS lcl_flight_retrieval DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF flight_struc,
             from        TYPE c LENGTH 20,
             to          TYPE c LENGTH 20,
             flight_date TYPE d,
             flight_time TYPE t,
           END OF flight_struc,
           flight_tab_type TYPE TABLE OF flight_struc WITH EMPTY KEY.

    METHODS get_flights
      IMPORTING
        from           TYPE string
        to             TYPE string
        flight_date    TYPE d
      RETURNING
        VALUE(flights) TYPE flight_tab_type.
    CLASS-METHODS class_constructor.
  PRIVATE SECTION.
    CLASS-DATA flight_tab TYPE flight_tab_type.
ENDCLASS.

CLASS lcl_flight_retrieval IMPLEMENTATION.
  METHOD get_flights.
    SELECT * FROM @flight_tab AS tab
      WHERE from = @from AND to = @to AND flight_date = @flight_date
      INTO TABLE @flights.
  ENDMETHOD.
  METHOD class_constructor.
    flight_tab = VALUE #(
     ( from = 'Frankfurt' to = 'Shanghai' flight_date = '20250511' flight_time = '050000' )
     ( from = 'Frankfurt' to = 'Shanghai' flight_date = '20250511' flight_time = '200000' )
     ( from = 'Frankfurt' to = 'Shanghai' flight_date = '20250605' flight_time = '151500' )
     ( from = 'Frankfurt' to = 'Shanghai' flight_date = '20250725' flight_time = '070000' )
     ( from = 'Shanghai' to = 'Frankfurt' flight_date = '20250410' flight_time = '194500' )
     ( from = 'Shanghai' to = 'Frankfurt' flight_date = '20250521' flight_time = '123000' )
     ( from = 'Shanghai' to = 'Frankfurt' flight_date = '20250805' flight_time = '184500' ) ).
  ENDMETHOD.
ENDCLASS.

"An object of the lcl_flight_search class is required for searching flights. This object is
"created by the facade class. The lcl_flight_search class implementation includes the calling
"of a method of the lcl_flight_retrieval class to retrieve a list of available flights.
CLASS lcl_flight_search DEFINITION.
  PUBLIC SECTION.
    METHODS search_flights
      IMPORTING
        from               TYPE string
        to                 TYPE string
        arrival            TYPE d
        departure          TYPE d
      RETURNING
        VALUE(flight_list) TYPE string_table.
ENDCLASS.

CLASS lcl_flight_search IMPLEMENTATION.
  METHOD search_flights.
    DATA(flight_obj) = NEW lcl_flight_retrieval( ).
    DATA(result) = flight_obj->get_flights(
                    from        = from
                    to          = to
                    flight_date = arrival ).

    IF result IS INITIAL.
      APPEND |X  \| There's no flight from { from } to { to } available on { arrival }.| TO flight_list.
    ELSE.
      LOOP AT result INTO DATA(flight_wa).
        APPEND |OK \| Flight from { flight_wa-from } to { flight_wa-to } available on { flight_wa-flight_date } at { flight_wa-flight_time }.| TO flight_list.
      ENDLOOP.
    ENDIF.

    result = flight_obj->get_flights(
                  from        = to
                  to          = from
                  flight_date = departure ).

    IF result IS INITIAL.
      APPEND |X  \| There's no flight from { to } to { from } available on { departure }.| TO flight_list.
    ELSE.
      LOOP AT result INTO flight_wa.
        APPEND |OK \| Flight from { flight_wa-from } to { flight_wa-to } available on { flight_wa-flight_date } at { flight_wa-flight_time }.| TO flight_list.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

"Class to get hotels from a data source
"In this simple, self-contained example, the data source is
"simulated by an internal table that includes demo data with
"hotel details such as unavailable dates.
CLASS lcl_hotel_retrieval DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF hotel_struc,
             city         TYPE c LENGTH 20,
             hotel_name   TYPE c LENGTH 20,
             is_available TYPE abap_boolean,
           END OF hotel_struc,
           hotel_tab_type TYPE TABLE OF hotel_struc WITH EMPTY KEY.

    METHODS get_hotels
      IMPORTING
        city          TYPE string
        arrival       TYPE d
        departure     TYPE  d
      RETURNING
        VALUE(hotels) TYPE hotel_tab_type.
    CLASS-METHODS class_constructor.
  PRIVATE SECTION.
    CLASS-DATA hotel_tab TYPE hotel_tab_type.
    TYPES: BEGIN OF hotel_struc_availability,
             city             TYPE c LENGTH 20,
             hotel_name       TYPE c LENGTH 20,
             unavailable_date TYPE d,
           END OF hotel_struc_availability,
           hotel_tab_availability_type TYPE TABLE OF hotel_struc_availability WITH EMPTY KEY.
    CLASS-DATA hotel_tab_availability TYPE hotel_tab_availability_type.
    DATA date_tab TYPE TABLE OF d WITH EMPTY KEY.
    DATA flag TYPE abap_boolean.
ENDCLASS.

CLASS lcl_hotel_retrieval IMPLEMENTATION.
  METHOD get_hotels.
    SELECT * FROM @hotel_tab_availability AS tab
      WHERE city = @city
      INTO TABLE @DATA(hotel_list).

    LOOP AT hotel_list INTO DATA(waf) GROUP BY ( key = waf-hotel_name ) ASCENDING INTO DATA(keyf).
      LOOP AT GROUP keyf INTO DATA(memberf).
        APPEND memberf-unavailable_date TO date_tab.
      ENDLOOP.

      "Checking whether hotels are available during the travel time span
      LOOP AT date_tab INTO DATA(date).
        IF date >= arrival AND date <= departure.
          flag = abap_false.
          EXIT.
        ELSE.
          flag = abap_true.
        ENDIF.
      ENDLOOP.

      IF flag = abap_true.
        APPEND VALUE #( city = memberf-city hotel_name = memberf-hotel_name is_available = abap_true ) TO hotels.
      ENDIF.

      CLEAR flag.
      CLEAR date_tab.
    ENDLOOP.

  ENDMETHOD.
  METHOD class_constructor.
    hotel_tab_availability = VALUE #(
        ( city = 'Frankfurt' hotel_name = 'ABC' unavailable_date = '20250512' )
        ( city = 'Frankfurt' hotel_name = 'ABC' unavailable_date = '20250612' )
        ( city = 'Frankfurt' hotel_name = 'DEF' unavailable_date = '20250512' )
        ( city = 'Frankfurt' hotel_name = 'DEF' unavailable_date = '20250712' )
        ( city = 'Frankfurt' hotel_name = 'GHI' unavailable_date = '20250512' )
        ( city = 'Frankfurt' hotel_name = 'GHI' unavailable_date = '20250812' )
        ( city = 'Shanghai' hotel_name = 'JKL' unavailable_date = '20250512' )
        ( city = 'Shanghai' hotel_name = 'JKL' unavailable_date = '20250610' )
        ( city = 'Shanghai' hotel_name = 'MNO' unavailable_date = '20250712' )
        ( city = 'Shanghai' hotel_name = 'MNO' unavailable_date = '20250611' )
        ( city = 'Shanghai' hotel_name = 'PQR' unavailable_date = '20250408' )
        ( city = 'Shanghai' hotel_name = 'PQR' unavailable_date = '20250912' )
        ( city = 'Shanghai' hotel_name = 'PQR' unavailable_date = '20250612' ) ).
  ENDMETHOD.
ENDCLASS.

"An object of the lcl_hotel_search class is required for searching hotels. This object is
"created by the facade class. The lcl_hotel_search class implementation includes the calling
"of a method of the lcl_hotel_retrieval class to retrieve a list of available hotels.
CLASS lcl_hotel_search DEFINITION.
  PUBLIC SECTION.
    METHODS search_hotel
      IMPORTING
        destination       TYPE string
        arrival           TYPE d
        departure         TYPE d
      RETURNING
        VALUE(hotel_list) TYPE string_table.
ENDCLASS.

CLASS lcl_hotel_search IMPLEMENTATION.
  METHOD search_hotel.

    DATA(hotel_obj) = NEW lcl_hotel_retrieval( ).
    DATA(result) = hotel_obj->get_hotels(
                    city      = destination
                    arrival   = arrival
                    departure = departure ).

    IF result IS INITIAL.
      APPEND |X  \| There's no hotel available in { destination } during your trip from { arrival } to { departure }.| TO hotel_list.
    ELSE.
      LOOP AT result INTO DATA(hotel_wa).
        APPEND |OK \| Hotel "{ hotel_wa-hotel_name }" in { destination } is availble during your trip from { arrival } to { departure }.| TO hotel_list.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

"Class to get rental cars from a data source
"In this simple, self-contained example, the data source is
"simulated by an internal table that includes demo data with
"rental car details such as unavailable dates.
CLASS lcl_rental_car_retrieval DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF rental_car_struc,
             city         TYPE c LENGTH 20,
             car          TYPE c LENGTH 20,
             is_available TYPE abap_boolean,
           END OF rental_car_struc,
           rental_car_tab_type TYPE TABLE OF rental_car_struc WITH EMPTY KEY.

    METHODS get_rental_cars
      IMPORTING
        city               TYPE string
        arrival            TYPE d
        departure          TYPE  d
      RETURNING
        VALUE(rental_cars) TYPE rental_car_tab_type.
    CLASS-METHODS class_constructor.
  PRIVATE SECTION.
    CLASS-DATA rental_car_tab TYPE rental_car_tab_type.
    TYPES: BEGIN OF rental_car_struc_availability,
             city             TYPE c LENGTH 20,
             car              TYPE c LENGTH 20,
             unavailable_date TYPE d,
           END OF rental_car_struc_availability,
           rent_car_tab_availability_type TYPE TABLE OF rental_car_struc_availability WITH EMPTY KEY.
    CLASS-DATA rental_car_availability_tab TYPE rent_car_tab_availability_type.
    DATA date_tab TYPE TABLE OF d WITH EMPTY KEY.
    DATA flag TYPE abap_boolean.
ENDCLASS.

CLASS lcl_rental_car_retrieval IMPLEMENTATION.
  METHOD get_rental_cars.
    SELECT * FROM @rental_car_availability_tab AS tab
      WHERE city = @city
      INTO TABLE @DATA(rental_car_list).

    LOOP AT rental_car_list INTO DATA(waf) GROUP BY ( key = waf-car ) ASCENDING INTO DATA(keyf).
      LOOP AT GROUP keyf INTO DATA(memberf).
        APPEND memberf-unavailable_date TO date_tab.
      ENDLOOP.

      LOOP AT date_tab INTO DATA(date).
        IF date >= arrival AND date <= departure.
          flag = abap_false.
          EXIT.
        ELSE.
          flag = abap_true.
        ENDIF.
      ENDLOOP.

      IF flag = abap_true.
        APPEND VALUE #( city = memberf-city car = memberf-car is_available = abap_true ) TO rental_cars.
      ENDIF.

      CLEAR flag.
      CLEAR date_tab.
    ENDLOOP.
  ENDMETHOD.
  METHOD class_constructor.
    rental_car_availability_tab = VALUE #(
      ( city = 'Frankfurt' car = 'Car 1' unavailable_date = '20250512' )
      ( city = 'Frankfurt' car = 'Car 1' unavailable_date = '20250612' )
      ( city = 'Frankfurt' car = 'Car 2' unavailable_date = '20250512' )
      ( city = 'Frankfurt' car = 'Car 2' unavailable_date = '20250712' )
      ( city = 'Frankfurt' car = 'Car 3' unavailable_date = '20250512' )
      ( city = 'Frankfurt' car = 'Car 3' unavailable_date = '20250812' )
      ( city = 'Shanghai' car = 'Car 4' unavailable_date = '20250512' )
      ( city = 'Shanghai' car = 'Car 4' unavailable_date = '20250612' )
      ( city = 'Shanghai' car = 'Car 5' unavailable_date = '20250607' )
      ( city = 'Shanghai' car = 'Car 5' unavailable_date = '20250712' )
      ( city = 'Shanghai' car = 'Car 5' unavailable_date = '20250812' )
      ( city = 'Shanghai' car = 'Car 6' unavailable_date = '20250912' )
      ( city = 'Shanghai' car = 'Car 6' unavailable_date = '20251012' ) ).
  ENDMETHOD.
ENDCLASS.

"An object of the lcl_rental_car_search class is required for searching rental cars. This object is
"created by the facade class. The lcl_rental_car_search class implementation includes the calling
"of a method of the lcl_rental_car_retrieval class to retrieve a list of available rental cars.
CLASS lcl_rental_car_search DEFINITION.
  PUBLIC SECTION.
    METHODS search_rental_car
      IMPORTING
        destination            TYPE string
        arrival                TYPE d
        departure              TYPE d
      RETURNING
        VALUE(rental_car_list) TYPE string_table.
ENDCLASS.

CLASS lcl_rental_car_search IMPLEMENTATION.
  METHOD search_rental_car.
    DATA(rental_car_obj) = NEW lcl_rental_car_retrieval( ).
    DATA(res) = rental_car_obj->get_rental_cars(
                  city      = destination
                  arrival   = arrival
                  departure = departure ).

    IF res IS INITIAL.
      APPEND |X  \| There's no rental car available in { destination } during your trip from { arrival } to { departure }.| TO rental_car_list.
    ELSE.
      LOOP AT res INTO DATA(rental_car_wa).
        APPEND |OK \| "{ rental_car_wa-car }" is available as rental car in { destination } during your trip from { arrival } to { departure }.| TO rental_car_list.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

"Facade class that represents a simplified API, hiding the complexity from users and allowing
"them to achieve desired results without dealing with and knowing about underlying complexities.
"It includes various object creations and method calls.
CLASS lcl_travel_facade DEFINITION.
  PUBLIC SECTION.
    METHODS plan_travel
      IMPORTING
        from                       TYPE string
        to                         TYPE string
        arrival                    TYPE d
        departure                  TYPE d
      RETURNING
        VALUE(reservation_options) TYPE string_table.
ENDCLASS.

CLASS lcl_travel_facade IMPLEMENTATION.
  METHOD plan_travel.
    "Abort proceeding with the example when the departure is before the arrival.
    IF departure <= arrival.
      ASSERT 1 = 0.
    ENDIF.

    "Creating multipled objects
    DATA(flight_search) = NEW lcl_flight_search( ).
    DATA(hotel_search) = NEW lcl_hotel_search( ).
    DATA(rental_car_search) = NEW lcl_rental_car_search( ).

    "Adding travel search information to a string table for demonstration and output purposes
    APPEND |Travel options for: { from } - { to }, { arrival } - { departure }| TO reservation_options.
    DATA(flights) = flight_search->search_flights( from = from to = to arrival = arrival departure = departure ).
    APPEND `---------- FLIGHTS -----------------------------------------------------------------------------` TO reservation_options.
    APPEND LINES OF flights TO reservation_options.
    DATA(hotels) = hotel_search->search_hotel( destination = to arrival = arrival departure = departure  ).
    APPEND `---------- HOTELS ------------------------------------------------------------------------------` TO reservation_options.
    APPEND LINES OF hotels TO reservation_options.
    DATA(rental_cars) = rental_car_search->search_rental_car( destination = to arrival = arrival departure = departure ).
    APPEND `---------- RENTAL CARS -------------------------------------------------------------------------` TO reservation_options.
    APPEND LINES OF rental_cars TO reservation_options.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Factory Method

- Used, for example, to:  
  - Provide users with an object of a class instead of them creating the objects themselves.  
  - Control and simplify class instantiation for external users.  
  - Offer a stable API for class users, so they only need to call one stable method. This way, the code may be modified or extended, and the changes to the class do not affect users.  
- Typically, using the `CREATE PRIVATE` addition in a class definition prevents object creation outside the class. A factory method, usually a static method, then supplies users with class objects. You can also include input parameters in the factory method to control instantiation.  
- Example of a predefined ABAP class with factory methods: `CL_ABAP_REGEX`.  


<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- This example demonstrates the factory design pattern with the following declarations and implementations:
  - Global class:
    - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
    - Serves as a vehicle for demonstrating the design pattern. The declarations and implementations in the `CCIMP` are relevant for the for conceptual considerations.
  - CCIMP include (Local Types tab in ADT):
    - Defines the `lif_factory` interface, specifying a method that is implemented in all classes that implement the interface.
    - Contains multiple local classes (`lcl_**`) implementing `lif_factory`. Each class returns a string in its method.
    - Class `lcl_factory_cl` containing a factory method:
      - Defined as `CREATE PRIVATE` to prevent object creation outside the class.
      - Offers the `create_hello` factory method returning an interface reference. This method uses an input value to create the appropriate object, which the reference points to. Here, the input is an enumerated type that is defined in the interface.
- The class execution includes the following:
  - Multiple objects are created using the factory method with different input parameters.
  - Based on the parameter, a specific object is created. The `say_hello` method will return a string as implemented in the resepctive class.


<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Saying hello in English
    DATA(oref_en) = lcl_factory_cl=>create_hello( lif_factory=>en ).
    DATA(hello_en) = oref_en->say_hello( ).
    out->write( hello_en ).

    "Saying hello in French
    DATA(oref_fr) = lcl_factory_cl=>create_hello( lif_factory=>fr ).
    DATA(hello_fr) = oref_fr->say_hello( ).
    out->write( hello_fr ).

    "Saying hello in Italian
    DATA(oref_it) = lcl_factory_cl=>create_hello( lif_factory=>it ).
    DATA(hello_it) = oref_it->say_hello( ).
    out->write( hello_it ).

    "Saying hello in Spanish
    DATA(oref_es) = lcl_factory_cl=>create_hello( lif_factory=>es ).
    DATA(hello_es) = oref_es->say_hello( ).
    out->write( hello_es ).

    "Saying hello in German
    DATA(oref_de) = lcl_factory_cl=>create_hello( lif_factory=>de ).
    DATA(hello_de) = oref_de->say_hello( ).
    out->write( hello_de ).

    "Default hello
    DATA(oref_default) = lcl_factory_cl=>create_hello( lif_factory=>init ).
    DATA(hello_default) = oref_default->say_hello( ).
    out->write( hello_default ).

  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
INTERFACE lif_factory.
  TYPES: basetype TYPE i,
         BEGIN OF ENUM enum_langu BASE TYPE basetype,
           init VALUE IS INITIAL,
           en   VALUE 1,
           fr   VALUE 2,
           it   VALUE 3,
           es   VALUE 4,
           de   VALUE 5,
         END OF ENUM enum_langu.

  METHODS say_hello RETURNING VALUE(hi) TYPE string.
ENDINTERFACE.

CLASS lcl_en DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_en IMPLEMENTATION.
  METHOD lif_factory~say_hello.
    hi = `Hi`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_fr DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_fr IMPLEMENTATION.
  METHOD lif_factory~say_hello.
    hi = `Salut`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_it DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_it IMPLEMENTATION.
  METHOD lif_factory~say_hello.
    hi = `Ciao`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_es DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_es IMPLEMENTATION.
  METHOD lif_factory~say_hello.
    hi = `Hola`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_de DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_de IMPLEMENTATION.
  METHOD lif_factory~say_hello.
    hi = `Hallo`.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_factory_cl DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS create_hello IMPORTING language     TYPE lif_factory=>enum_langu
                               RETURNING VALUE(hello) TYPE REF TO lif_factory.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_factory_cl IMPLEMENTATION.
  METHOD create_hello.
    hello = SWITCH #( language
                      WHEN lif_factory=>en THEN NEW lcl_en( )
                      WHEN lif_factory=>fr THEN NEW lcl_fr( )
                      WHEN lif_factory=>it THEN NEW lcl_it( )
                      WHEN lif_factory=>es THEN NEW lcl_es( )
                      WHEN lif_factory=>de THEN NEW lcl_de( )
                      "E.g. raising an exception or returning a default object
                      ELSE NEW lcl_en( ) ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Fluent Interface

- Enables method chaining  
- Achieves method chaining by returning a reference to the current object. There may be variations in the implementation, for example, the returned object may be a modified copy or the modified original.  
- Such a design may consolidate method calls for a simpler, more readable code flow, instead of having individual method calls.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- This example demonstrates the fluent interface design pattern with the following declarations and implementations: 
  - Global class:  
    - Implements `if_oo_adt_classrun` and calls methods from local classes.  
    - Acts as a consumer of APIs (local classes) defined in the CCIMP include.  
  - CCIMP include (Local Types tab in ADT):  
    - Example 1 (String building)  
      - Local interface `lif_string_processing`:  
        - Defines multiple methods for string modification  
        - Most of the methods specify a reference to the interface as returning parameter
      - Local class `lcl_string_processing`:  
        - Specifies the `CREATE PRIVATE` addition to prevent instantiation from outside the class
        - However, as `lcl_string` is declared as friend, `lcl_string` can instantiate the class. 
        - Implements the interface
        - The method implementations return a modified copy of the original object 
      - Local class `lcl_string`:  
        - Contains a static factory method returning an instance of `lcl_string_processing` (the returning parameter is typed with `TYPE REF TO lif_string_processing`)
        - The factory method requires a string to be supplied, which represents the base string that can be modified using the methods that `lif_string_processing` offers     
    - Example 2 (Simple calculations)  
      - Local class `lcl_calc`:  
        - Represents a simpler example of the fluent interface pattern  
        - Instantiable class  
        - Methods of the class return an object reference of the class 
- The class execution includes the following by demonstrating chained method calls:
  - Example 1: Multiple instances are created showing the variety of methods the interface offers for modifying a string (adding strings to strings, precedings strings with strings, splitting strings into a string table, performing replacements, transforming to lowercase and uppercase, reversing strings, inserting strings, removing spaces, retrieving the modified string)
  - Example 2: Multiple instances are created performing consecutive calculations; note that there is no proper exception handling in the simple example (e.g. a zero division is just ignored)


<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

    "Adding strings
    "Retrieving the resulting string using the attribute 'str'
    DATA(str1) = lcl_string=>string( `Lorem` )->add( ` ` )->add( `ipsum` )->str.

    "Instead of extra method calls using the reference variable
    DATA(str1b_ref) = lcl_string=>string( `Lorem` ).
    str1b_ref->add( ` ` ).
    str1b_ref->add( `ipsum` ).
    DATA(str1b) = str1b_ref->str.

    "Retrieving the resulting string using the method 'get_string'
    DATA(str2) = lcl_string=>string( `Lorem` )->add( ` ` )->add( `ipsum` )->add( ` ` )->add( `dolor` )->add( ` ` )->add( `sit` )->add( ` ` )->add( `amet` )->get_string( ).

    "Preceding strings
    DATA(str3) = lcl_string=>string( `world` )->precede( ` ` )->precede( `Hello` )->str.
    DATA(str4) = lcl_string=>string( `B` )->add( `A` )->precede( `A` )->add( `P` )->str.

    "Splitting into string table
    DATA(tab1) = lcl_string=>string( `Lorem` )->add( `#` )->add( `ipsum` )->add( `#` )->add( `dolor` )->add( `#` )->add( `sit` )->add( `#` )->add( `amet` )->split_into_table( `#` ).
    DATA(tab2) = lcl_string=>string( `Lorem` )->add( ` ` )->add( `ipsum` )->split_into_table( ` ` ).

    "Replacements
    DATA(str5) = lcl_string=>string( `Lorem#ipsum#dolor#sit#amet` )->replace_all( sub = `#` with = ` ` )->str.
    DATA(str6) = lcl_string=>string( `Lorem#ipsum#dolor#sit#amet` )->replace_occ( sub = `#` with = ` ` occ = 1 )->str.
    DATA(str7) = lcl_string=>string( `Lorem#ipsum#dolor#sit#amet` )->replace_occ( sub = `#` with = ` ` occ = 2 )->str.
    DATA(str8) = lcl_string=>string( `Lorem#ipsum#dolor#sit#amet` )->replace_occ( sub = `#` with = ` ` occ = -2 )->str.
    DATA(tab3) = lcl_string=>string( `hello` )->add( `#` )->add( `world` )->replace_all( sub = `#` with = `,` )->split_into_table( `,` ).

    "Transforming to lowercase and uppercase
    DATA(str9) = lcl_string=>string( `ab` )->add( `ap` )->uppercase( )->str.
    DATA(str10) = lcl_string=>string( `AP` )->precede( `AB` )->lowercase( )->str.
    DATA(str11) = lcl_string=>string( `AB` )->lowercase( )->add( `ap` )->uppercase( )->str. "First lowercasing overridden

    "Reversing string
    DATA(str12) = lcl_string=>string( `OLL` )->add( `AH` )->lowercase( )->reverse_string( )->str.

    "Inserting string
    DATA(str13) = lcl_string=>string( `abcghi` )->insert_string( string = `def` off = 3 )->str.
    DATA(str14) = lcl_string=>string( `vwxyz` )->insert_string( string = `stu` off = 0 )->str.

    "Removing spaces
    "All spaces
    DATA(str15) = lcl_string=>string( ` a b  c` )->add( ` d  e   f     gh i   ` )->remove_all_spaces( )->str.
    "Leading and trailing spaces
    DATA(str16) = lcl_string=>string( `      ab c d   e f     g   h i     ` )->remove_leading_trailing_spaces( )->str.
    DATA(str17) = lcl_string=>string( `abc     ` )->remove_leading_trailing_spaces( )->add( `def` )->str.

    "Displaying results in the console
    out->write( data = str1 name = `str1` ).
    out->write( |\n| ).
    out->write( data = str1b name = `str1b` ).
    out->write( |\n| ).
    out->write( data = str2 name = `str2` ).
    out->write( |\n| ).
    out->write( data = str3 name = `str3` ).
    out->write( |\n| ).
    out->write( data = str4 name = `str4` ).
    out->write( |\n| ).
    out->write( data = tab1 name = `tab1` ).
    out->write( |\n| ).
    out->write( data = tab2 name = `tab2` ).
    out->write( |\n| ).
    out->write( data = str5 name = `str5` ).
    out->write( |\n| ).
    out->write( data = str6 name = `str6` ).
    out->write( |\n| ).
    out->write( data = str7 name = `str7` ).
    out->write( |\n| ).
    out->write( data = str8 name = `str8` ).
    out->write( |\n| ).
    out->write( data = tab3 name = `tab3` ).
    out->write( |\n| ).
    out->write( data = str9 name = `str9` ).
    out->write( |\n| ).
    out->write( data = str10 name = `str10` ).
    out->write( |\n| ).
    out->write( data = str11 name = `str11` ).
    out->write( |\n| ).
    out->write( data = str12 name = `str12` ).
    out->write( |\n| ).
    out->write( data = str13 name = `str13` ).
    out->write( |\n| ).
    out->write( data = str14 name = `str14` ).
    out->write( |\n| ).
    out->write( data = str15 name = `str15` ).
    out->write( |\n| ).
    out->write( data = str16 name = `str16` ).
    out->write( |\n| ).
    out->write( data = str17 name = `str17` ).
    out->write( |\n| ).

**********************************************************************

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

    DATA(calc1) = NEW lcl_calc( 1 )->plus( 2 )->get_result( ).
    DATA(calc2) = NEW lcl_calc( 1 )->minus( 2 )->get_result( ).
    DATA(calc3) = NEW lcl_calc( 5 )->plus( 2 )->minus( 1 )->multiply( 3 )->get_result( ).
    DATA(calc4) = NEW lcl_calc( 10 )->multiply( 10 )->divide( 2 )->get_result( ).
    DATA(calc5) = NEW lcl_calc( 0 )->plus( 1 )->divide( 5 )->get_result( ).
    DATA(calc6) = NEW lcl_calc( '1.2' )->plus( '1.4' )->minus( '0.1' )->multiply( '2.5' )->divide( 2 )->get_result( ).

    "Arithmetic errors are just ignored in the example
    DATA(calc7) = NEW lcl_calc( 1 )->divide( 0 )->plus( 1 )->get_result( ).

    "Method chaining with a standalone statements
    NEW lcl_calc( 1 )->plus( 2 )->multiply( 5 )->minus( 5 )->divide( 2 )->get_result( RECEIVING result = DATA(calc8) ).

    IF NEW lcl_calc( 1 )->plus( 2 )->minus( 3 )->plus( 4 )->minus( 5 )->get_result( ) <= 0.
      DATA(if_statement) = `The result is equal to or lower than 0`.
    ELSE.
      if_statement = `The result is greater than 0`.
    ENDIF.

    out->write( data = calc1 name = `calc1` ).
    out->write( |\n| ).
    out->write( data = calc2 name = `calc2` ).
    out->write( |\n| ).
    out->write( data = calc3 name = `calc3` ).
    out->write( |\n| ).
    out->write( data = calc3 name = `calc3` ).
    out->write( |\n| ).
    out->write( data = calc4 name = `calc4` ).
    out->write( |\n| ).
    out->write( data = calc5 name = `calc5` ).
    out->write( |\n| ).
    out->write( data = calc6 name = `calc6` ).
    out->write( |\n| ).
    out->write( data = calc7 name = `calc7` ).
    out->write( |\n| ).
    out->write( data = calc8 name = `calc8` ).
    out->write( |\n| ).
    out->write( data = if_statement name = `if_statement` ).

  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

INTERFACE lif_string_processing.
  DATA str TYPE string READ-ONLY.
  METHODS add IMPORTING string     TYPE clike
              RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS precede IMPORTING string     TYPE clike
                  RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS replace_all IMPORTING sub        TYPE clike
                                with       TYPE clike
                      RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS replace_occ IMPORTING sub        TYPE clike
                                with       TYPE clike
                                occ        TYPE i DEFAULT 1
                      RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS lowercase RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS uppercase RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS remove_leading_trailing_spaces RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS remove_all_spaces RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS reverse_string RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS insert_string IMPORTING string     TYPE clike
                                  off        TYPE i
                        RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS get_string RETURNING VALUE(str) TYPE string.
  METHODS split_into_table IMPORTING split_at   TYPE clike
                           RETURNING VALUE(tab) TYPE string_table.
ENDINTERFACE.

CLASS lcl_string DEFINITION DEFERRED.
CLASS lcl_string_processing DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_string.
  PUBLIC SECTION.
    INTERFACES lif_string_processing.
    ALIASES: add FOR lif_string_processing~add,
             get_string FOR lif_string_processing~get_string,
             insert_string FOR lif_string_processing~insert_string,
             precede FOR lif_string_processing~precede,
             remove_all_spaces FOR lif_string_processing~remove_all_spaces,
             remove_leading_trailing_spaces FOR lif_string_processing~remove_leading_trailing_spaces,
             replace_all FOR lif_string_processing~replace_all,
             replace_occ FOR lif_string_processing~replace_occ,
             reverse_string FOR lif_string_processing~reverse_string,
             split_into_table FOR lif_string_processing~split_into_table,
             lowercase FOR lif_string_processing~lowercase,
             uppercase FOR lif_string_processing~uppercase.
  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES string_content FOR lif_string_processing~str.
    METHODS constructor IMPORTING content TYPE string.
    DATA oref TYPE REF TO lcl_string_processing.
ENDCLASS.

CLASS lcl_string_processing IMPLEMENTATION.
  METHOD add.
    oref->string_content &&= string.
    ref = oref.
  ENDMETHOD.

  METHOD get_string.
    str = oref->string_content.
  ENDMETHOD.

  METHOD insert_string.
    TRY.
        oref->string_content = insert( val = oref->string_content sub = string off = off ).
      CATCH cx_sy_range_out_of_bounds.
    ENDTRY.
    ref = oref.
  ENDMETHOD.

  METHOD precede.
    oref->string_content = string && oref->string_content.
    ref = oref.
  ENDMETHOD.

  METHOD remove_all_spaces.
    oref->string_content = condense( val = oref->string_content to = `` ).
    ref = oref.
  ENDMETHOD.

  METHOD remove_leading_trailing_spaces.
    oref->string_content = condense( val = oref->string_content from = `` ).
    ref = oref.
  ENDMETHOD.

  METHOD replace_all.
    oref->string_content = replace( val = oref->string_content sub = sub with = with  occ = 0 ).
    ref = oref.
  ENDMETHOD.

  METHOD replace_occ.
    oref->string_content = replace( val = oref->string_content sub = sub with = with occ = occ ).
    ref = oref.
  ENDMETHOD.

  METHOD reverse_string.
    oref->string_content = reverse( oref->string_content ).
    ref = oref.
  ENDMETHOD.

  METHOD split_into_table.
    SPLIT oref->string_content AT split_at INTO TABLE tab.
  ENDMETHOD.

  METHOD lowercase.
    oref->string_content = to_lower( oref->string_content ).
    ref = oref.
  ENDMETHOD.

  METHOD uppercase.
    oref->string_content = to_upper( oref->string_content ).
    ref = oref.
  ENDMETHOD.

  METHOD constructor.
    string_content = content.
    oref = me.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_string DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_string_processing.
  PUBLIC SECTION.
    CLASS-METHODS string IMPORTING string     TYPE clike
                         RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_string IMPLEMENTATION.
  METHOD string.
    ref = NEW lcl_string_processing( string ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

CLASS lcl_calc DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING num TYPE decfloat34.
    METHODS plus IMPORTING num        TYPE decfloat34
                 RETURNING VALUE(ref) TYPE REF TO lcl_calc.
    METHODS minus IMPORTING num        TYPE decfloat34
                  RETURNING VALUE(ref) TYPE REF TO lcl_calc.
    METHODS multiply IMPORTING num        TYPE decfloat34
                     RETURNING VALUE(ref) TYPE REF TO lcl_calc.
    METHODS divide IMPORTING num        TYPE decfloat34
                   RETURNING VALUE(ref) TYPE REF TO lcl_calc.
    METHODS get_result RETURNING VALUE(result) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA number TYPE decfloat34.
ENDCLASS.

CLASS lcl_calc IMPLEMENTATION.
  METHOD constructor.
    number = num.
  ENDMETHOD.

  METHOD divide.
    TRY.
        number /= num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD minus.
    TRY.
        number -= num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD multiply.
    TRY.
        number *= num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD plus.
    TRY.
        number += num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD get_result.
    result = number.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Flyweight 


- The flyweight design pattern may be useful when you have a large number of objects that share similar data. For example, these objects might have both unique and shared components. The idea behind the pattern is to avoid duplicating the shared components in each object by separating them for reuse. Such an approach can reduce the number of objects needed and reduce memory consumption.  
- A potential setup, illustrated in the following example, may look like this:  
  - A flyweight interface that defines methods for processing shared parts.  
  - A concrete flyweight class that implements the interface to handle shared parts. Objects of this class represent shareable objects and contain the shared data.  
  - A flyweight factory class that manages existing flyweight objects with the shared data in an internal table. If a flyweight object is requested by the client, the table is searched, and if found, the object is returned. If a flyweight object does not yet exist, a new object is created, added to the internal table, and returned.  
  - The client uses the flyweight objects through the flyweight factory and passes unique states.
 

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- The example demonstrates the flyweight design pattern through the following declarations and implementations. Note that the example is simplified, and various class setup strategies may apply.
- The context of this hypothetical example is as follows:
  - There are many customers, each of whom can have an object created for them. Depending on events - such as subscriptions, orders, New Year, ect. - each customer should receive a specific message. Hypothetically, another event object is created for this purpose and for each customer, but the event objects (the messages) are identical across all customers and stay consistent. Customer object and such an event/message object go together so that specific messages can be sent for specific customers. The trivial example uses simple strings, but it is assumed that the objects to be created involve large data sets and consume a considerable amount of memory.
  - In this context, the message represents the shared data (as it can be reused for all customers), while customer-specific information (such as ID, name, and email) constitutes the unique data. In this example, the notification is represented by filling a string table. 
  - Instead of creating separate objects for each customer and event, reusable data like the notification message for different events is stored in flyweight objects.
  - When a client wants to trigger a notification for a specific customer regarding an event, it is first checked whether a flyweight object containing the shared data already exists. If it does, the existing object is returned and used instead of creating a duplicate. Otherwise, a new flyweight object is created, added to a reference storage (an internal table), and returned.
- Global class (_Global Class_ tab in ADT):
  - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
  - Acts as the client to demonstrate the design pattern. The declarations and implementations in the *CCIMP include* are relevant for conceptual considerations.
  - The client requests flyweight objects via the factory class, and provides unique states to the method that handles the unique and shared data.
- CCIMP include (_Local Types_ tab in ADT):
  - Interface `lif_flyweight`:
    - Defines the common interface for concrete flyweight classes.
    - Includes the `notify` method for flyweight classes to implement, which accepts a reference to a customer object and returns text in a string table.
  - Class `lcl_customer`:
    - Holds the unique state of each customer.
    - The `get_customer_info` method retrieves unique customer information that is to be connected with the shared data, with the `customer_info` attribute storing detailed customer information.
  - Class `lcl_flyweight`:
    - Represents concrete flyweight objects.
    - Implements the `lif_flyweight` interface.
    - Holds shared data via the `text` attribute and processes notifications through the `notify` method.
  - Class `lcl_flyweight_factory`:
    - Provides access to flyweight objects stored in an internal table (`flyweight_tab`).
    - Checks for existing flyweight objects. If one exists, it returns that object to ensure reuse. Otherwise, it creates a new object, adds it to the internal table, and returns it.
  - The example includes the filling of a log table, indicating whether a flyweight object is created or reused.

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: BEGIN OF ts_customer_info,
             customer_id TYPE i,
             name        TYPE c LENGTH 30,
             email       TYPE c LENGTH 30,
             birthday    TYPE d,
             address     TYPE c LENGTH 50,
           END OF ts_customer_info.

    CLASS-METHODS class_constructor.

    CLASS-DATA: customer_data TYPE TABLE OF ts_customer_info WITH EMPTY KEY,
                log           TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA(customer_a) = NEW lcl_customer( 1 ).

    DATA(fw_a) = lcl_flyweight_factory=>get_flyweight_object( `SIGNUP` ).
    DATA(msg_a) = fw_a->notify( customer_a ).
    out->write( data = msg_a name = `msg_a` ).
    out->write( |\n| ).

    DATA(customer_b) = NEW lcl_customer( 2 ).
    DATA(fw_b) = lcl_flyweight_factory=>get_flyweight_object( `SIGNUP` ).
    DATA(msg_b) = fw_b->notify( customer_b ).
    out->write( data = msg_b name = `msg_b` ).
    out->write( |\n| ).

    DATA(fw_c) = lcl_flyweight_factory=>get_flyweight_object( `SIGNUP` ).
    DATA(msg_c) = fw_c->notify( NEW lcl_customer( 3 ) ).
    out->write( data = msg_c name = `msg_c` ).
    out->write( |\n| ).

    DATA(fw_d) = lcl_flyweight_factory=>get_flyweight_object( `ORDER` ).
    DATA(msg_d) = fw_d->notify( customer_a ).
    out->write( data = msg_d name = `msg_d` ).
    out->write( |\n| ).

    DATA(fw_e) = lcl_flyweight_factory=>get_flyweight_object( `ORDER` ).
    DATA(msg_e) = fw_e->notify( customer_b ).
    out->write( data = msg_e name = `msg_e` ).
    out->write( |\n| ).

    "Displaying log table
    out->write( log ).
    out->write( |\n| ).

    CLEAR log.

    "Checking out all events in the example
    DATA(event_tab) = VALUE string_table( ( `SIGNUP` )
                                          ( `ORDER` )
                                          ( `SHIPPING` )
                                          ( `NEW_YEAR` ) ).

    DATA msg_tab TYPE string_table.

    LOOP AT event_tab INTO DATA(evt).
      DO lines( customer_data ) TIMES.
        DATA(fw) = lcl_flyweight_factory=>get_flyweight_object( evt ).
        DATA(msg) = fw->notify( NEW lcl_customer( sy-index ) ).

        APPEND |-------------------- Example for event "{ evt }", customer ID "{ sy-index }" --------------------| TO msg_tab.
        APPEND LINES OF msg TO msg_tab.
        APPEND INITIAL LINE TO msg_tab.
      ENDDO.
    ENDLOOP.

    out->write( data = msg_tab name = `msg_tab` ).

    "Displaying log table
    out->write( log ).
  ENDMETHOD.

  METHOD class_constructor.
    customer_data = VALUE #(
      ( customer_id = 1 name = 'Jon Doe'       email = 'jon_doe@example.com'      birthday = '19801101' address = 'Some Street 1, London' )
      ( customer_id = 2 name = 'Jane Smith'    email = 'jane_smith@example.com'   birthday = '19900515' address = 'Another Ave 2, New York' )
      ( customer_id = 3 name = 'Alice Brown'   email = 'alice_brown@example.com'  birthday = '19870322' address = 'Red Road 3, Sydney' )
      ( customer_id = 4 name = 'Bob White'     email = 'bob_white@example.com'    birthday = '19751230' address = 'Blue Blvd 4, Paris' )
      ( customer_id = 5 name = 'Charlie Black' email = 'charlie_black@example.com' birthday = '19991111' address = 'Green Street 5, Berlin' )
      ( customer_id = 6 name = 'Diana Green'   email = 'diana_green@example.com'  birthday = '19860418' address = 'Yellow Lane 6, Tokyo' )
      ( customer_id = 7 name = 'Edward King'   email = 'edward_king@example.com'  birthday = '19930708' address = 'High Street 7, Toronto' )
      ( customer_id = 8 name = 'Fiona Blue'    email = 'fiona_blue@example.com'   birthday = '19820114' address = 'Blue Water 8, Auckland' )
      ( customer_id = 9 name = 'George Gray'   email = 'george_gray@example.com'  birthday = '19790925' address = 'Gray Path 9, Mumbai' )
      ( customer_id = 10 name = 'Hannah Gold'  email = 'hannah_gold@example.com'  birthday = '19881208' address = 'Golden Street 10, Cairo' ) ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
CLASS lcl_customer DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*& Flyweight interface
*&---------------------------------------------------------------------*

INTERFACE lif_flyweight.
  METHODS notify IMPORTING customer_ref TYPE REF TO lcl_customer
                 RETURNING VALUE(msg)   TYPE string_table.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Class for holding a unique state
*&---------------------------------------------------------------------*

CLASS lcl_customer DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING id TYPE i,
      get_customer_info RETURNING VALUE(customer_info) TYPE zcl_demo_abap=>ts_customer_info.
  PRIVATE SECTION.
    DATA: customer_info TYPE zcl_demo_abap=>ts_customer_info,
          msg           TYPE string_table.
ENDCLASS.

CLASS lcl_customer IMPLEMENTATION.
  METHOD constructor.
    "For the self-contained example, the customer data, representing
    "the unique data, is designed as content of an internal table.
    "Based on an ID, the customer data is retrieved.
    SELECT SINGLE *
      FROM @zcl_demo_abap=>customer_data AS tab
      WHERE customer_id = @id
      INTO @customer_info.

    "For simplicity, this example only covers the best case
    "scenario where actual data is avaialble.
    IF customer_info IS INITIAL.
      ASSERT 1 = 0.
    ENDIF.
  ENDMETHOD.

  METHOD get_customer_info.
    customer_info = me->customer_info.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete flyweight class
*&---------------------------------------------------------------------*

CLASS lcl_flyweight DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_flyweight.
    METHODS constructor IMPORTING text TYPE string.
  PRIVATE SECTION.
    DATA text TYPE string.
ENDCLASS.

CLASS lcl_flyweight IMPLEMENTATION.
  METHOD constructor.
    me->text = text.
  ENDMETHOD.

  METHOD lif_flyweight~notify.
    DATA(customer_info) = customer_ref->get_customer_info( ).

    msg = VALUE #( ( |Hallo, { customer_info-name }. { me->text }| )
                   ( |Customer info: ID: { customer_info-customer_id }, Name: { customer_info-name }, Birthday: { customer_info-customer_id }, | &&
                     |Address: { customer_info-address }, Email: { customer_info-email }| ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Flyweight factory class
*&---------------------------------------------------------------------*

CLASS lcl_flyweight_factory DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS get_flyweight_object IMPORTING event      TYPE string
                                       RETURNING VALUE(obj) TYPE REF TO lif_flyweight.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_flyweight,
             evt    TYPE string,
             fw_obj TYPE REF TO lif_flyweight,
           END OF ts_flyweight,
           tt_flyweight TYPE HASHED TABLE OF ts_flyweight WITH UNIQUE KEY evt.
    CLASS-DATA flyweight_tab TYPE tt_flyweight.
ENDCLASS.

CLASS lcl_flyweight_factory IMPLEMENTATION.
  METHOD get_flyweight_object.
    READ TABLE flyweight_tab WITH TABLE KEY evt = event INTO DATA(wa).
    IF sy-subrc = 0.
      obj = wa-fw_obj.
      "Adding information to a log table for display purposes.
      APPEND |Entry exists for key "{ event }". Object reused.| TO zcl_demo_abap=>log.
    ELSE.
      CASE event.
        WHEN `SIGNUP`.
          obj = NEW lcl_flyweight( text = `Thanks for signing up.` ).
        WHEN `ORDER`.
          obj = NEW lcl_flyweight( text = `Your order has been confirmed.` ).
        WHEN `SHIPPING`.
          obj = NEW lcl_flyweight( text = `Your order has been shipped.` ).
        WHEN `NEW_YEAR`.
          obj = NEW lcl_flyweight( text = `Happy New Year to you!` ).
        WHEN OTHERS.
          "For simplicity, this example only covers the best case
          "scenario where only supported events are processed.
          "Here might go exception raising for unsupported events.
          ASSERT 1 = 0.
      ENDCASE.

      INSERT VALUE #( evt = event fw_obj = obj ) INTO TABLE flyweight_tab.

      "Adding information to a log table for display purposes.
      APPEND |Entry does not exist for key "{ event }". New object created and added to the internal table.| TO zcl_demo_abap=>log.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Memento

- The idea of the memento design pattern is to allow the storing and restoring of an object's state.
- The state is stored in a separate object, the memento, which captures the object's state.
- A potential class setup for this pattern may include a class for which mementos are created, a memento class, and a class to manage the mementos.
- Use cases for this pattern include saving object states and providing undo (and possibly redo) functionality to revert changes.

<br>

 <details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- The example demonstrates the memento design pattern with the following declarations and implementations:
  - Global class:
    - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
    - Serves as a vehicle for demonstrating the design pattern. The declarations and implementations in the `CCIMP` are relevant for the for conceptual considerations.
  - CCIMP include (Local Types tab in ADT):
    - Several class setup and design strategies may apply. This simplified example includes three classes: 
      - `lcl`: Represents the class for which mementos are created.
      - `lcl_memento`: Captures the state of an `lcl` object.
      - `lcl_memento_controller`: Manages mementos.
    - More details:
      - `lcl` class
        - Represents the class for which mementos are created.
        - It is defined as `FINAL` and `CREATE PRIVATE`, which prevents inheritance and direct instantiation by other classes. This setup implements the singleton design pattern, allowing instantiation only through the `get_obj` method.
        - Attributes:
          - `state`: Represents the state of an `lcl` object. In this simplified example, the state is a string that can change.
          - `oref`: A static attribute holding a reference to the single `lcl` object.
        - Methods:
          - `get_obj`: Static method that returns a singleton instance of the `lcl` class.
          - `get_state`: Returns the current state of the object.
          - `change_state`: Updates the current state by appending text, with an optional leading space.
          - `save`: Creates a memento of the current state with a specified tag.
          - `restore`: Sets the current state to the state stored in the provided memento.

      - `lcl_memento` class
        - Captures the state of an `lcl` object. 
        - Marked as `FINAL` and `CREATE PRIVATE`, which prevents inheritance and direct instantiation by other classes, except for its friend (using the `FRIENDS` addition) class, `lcl`.
        - Attributes:
          - `state`: Represents the state of an `lcl` object. In this simplified example, it is a string that can change. 
          - `tag`: A string that labels each memento for identification.
          - `time_stamp`: Records the UTC timestamp when the memento is created. In this example, it is included for logging and display purposes.
        - Methods:
          - `constructor`: Initializes the `state` and `tag` attributes and captures the current timestamp.
          - `get_state`: Returns the memento's stored state.
          - `get_tag`: Returns the tag associated with the memento.
          - `get_timestamp`: Returns the creation timestamp of the memento.

      - `lcl_memento_controller` class
        - Manages mementos but does not modify their states.
        - Attributes:
          - `mementos_tab`: Internal table storing all created mementos.
          - `redo_tab`: Internal table storing mementos for potential redo operations.
          - `lcl_object`: Holds a reference to the `lcl` object for which it manages mementos.
        - Methods:
          - `constructor`: Initializes the reference variable for the `lcl_object`.
          - `save_memento`: Adds a new memento to the `mementos_tab`.
          - `undo`: Restores the `lcl` object to the previous state and adds the latest memento to the `redo_tab`.
          - `redo`: Restores the `lcl` object to the next state in the `redo_tab`, if available.
          - `restore_by_tag`: Searches for a memento by its tag in the `mementos_tab` and restores the `lcl` object to that state, also returning information on the success of the restoration.
          - `get_memento_log`: Returns logs of all mementos in the `mementos_tab` and `redo_tab`.


<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

*&-------------------------------------------------------------------------------------*
*& 1) Creating objects and mementos, changing and getting states
*&-------------------------------------------------------------------------------------*

    out->write( |1) Creating objects and mementos, changing and getting states\n| ).

    DATA(oref_lcl) = lcl=>get_obj( ).
    DATA(oref_memento_ctrl)  = NEW lcl_memento_controller( oref_lcl ).

    oref_lcl->change_state( `Lorem` ).
    oref_memento_ctrl->save_memento( `TAG_1` ).

    DATA(state_a) = oref_lcl->get_state( ).
    out->write( state_a ).

    oref_lcl->change_state( `ipsum` ).
    oref_memento_ctrl->save_memento( `TAG_2` ).

    DATA(state_b) = oref_lcl->get_state( ).
    out->write( state_b ).

    oref_lcl->change_state( `dolor` ).
    oref_memento_ctrl->save_memento( `TAG_3` ).

    DATA(state_c) = oref_lcl->get_state( ).
    out->write( state_c ).

    oref_lcl->change_state( `sit` ).
    oref_memento_ctrl->save_memento( `TAG_4` ).

    DATA(state_d) = oref_lcl->get_state(  ).
    out->write( state_d ).

    oref_lcl->change_state( `amet` ).
    oref_memento_ctrl->save_memento( `TAG_5` ).

    DATA(state_e) = oref_lcl->get_state( ).
    out->write( state_e ).

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

*&-------------------------------------------------------------------------------------*
*& 2) Memento log
*&-------------------------------------------------------------------------------------*

    out->write( |2) Memento log\n\n| ).


    DATA(memento_log) = oref_memento_ctrl->get_memento_log( ).
    out->write( data = memento_log name = `memento_log` ).

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

*&-------------------------------------------------------------------------------------*
*& 3) Undo operations
*&-------------------------------------------------------------------------------------*

    out->write( |3) Undo operations\n| ).

    DATA(state_f) = oref_lcl->get_state( ).
    out->write( `Current state:` ).
    out->write( state_f ).

    out->write( |\n| ).
    out->write( `State after first undo operation:` ).
    oref_memento_ctrl->undo( CHANGING lcl_obj = oref_lcl ).

    DATA(state_g) = oref_lcl->get_state( ).
    out->write( state_g ).

    oref_memento_ctrl->undo( CHANGING lcl_obj = oref_lcl ).

    DATA(state_h) = oref_lcl->get_state( ).

    out->write( |\n| ).
    out->write( `State after second undo operation:` ).
    out->write( state_h ).

    oref_memento_ctrl->undo( CHANGING lcl_obj = oref_lcl ).

    DATA(state_i) = oref_lcl->get_state( ).

    out->write( |\n| ).
    out->write( `State after third undo operation:` ).
    out->write( state_i && |\n\n| ).

    memento_log = oref_memento_ctrl->get_memento_log( ).
    out->write( data = memento_log name = `memento_log` ).

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

*&-------------------------------------------------------------------------------------*
*& 4) Redo operations
*&-------------------------------------------------------------------------------------*

    out->write( |4) Redo operations\n| ).

    DATA(state_j) = oref_lcl->get_state( ).
    out->write( `Current state:` ).
    out->write( state_j ).

    oref_memento_ctrl->redo( CHANGING lcl_obj = oref_lcl ).

    DATA(state_k) = oref_lcl->get_state( ).

    out->write( |\n| ).
    out->write( `State after first redo operation:` ).
    out->write( state_k && |\n\n| ).

    memento_log = oref_memento_ctrl->get_memento_log( ).
    out->write( data = memento_log name = `memento_log` ).

    oref_memento_ctrl->redo( CHANGING lcl_obj = oref_lcl ).

    DATA(state_l) = oref_lcl->get_state( ).

    out->write( |\n| ).
    out->write( `State after second redo operation:` ).
    out->write( state_l && |\n\n| ).

    memento_log = oref_memento_ctrl->get_memento_log( ).
    out->write( data = memento_log name = `memento_log` ).

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

*&-------------------------------------------------------------------------------------*
*& 5) Restoring by tag
*&-------------------------------------------------------------------------------------*

    out->write( |5) Restoring by tag\n| ).

    DATA(state_m) = oref_lcl->get_state( ).
    out->write( `Current state:` ).
    out->write( state_m ).
    out->write( |\n| ).

    DATA flag TYPE abap_boolean.

    "At this stage, the memento with the tag TAG_5 is not stored in the memento table but
    "in the redo table. Therefore, the current state remains unchanged because the restoration
    "only considers mementos available in the memento table in the example.
    out->write( `Tag used: TAG_5` ).
    oref_memento_ctrl->restore_by_tag( EXPORTING tag = `TAG_5`
                                       CHANGING  lcl_obj = oref_lcl
                                       RECEIVING restoration_ok = flag ).

    DATA(state_n) = oref_lcl->get_state( ).
    out->write( |Restoration operation successful? { COND #( WHEN flag = abap_true THEN `YES` ELSE `NO` ) }| ).
    out->write( state_n ).
    out->write( |\n| ).

    out->write( `Tag used: TAG_2` ).
    oref_memento_ctrl->restore_by_tag( EXPORTING tag = `TAG_2`
                                       CHANGING  lcl_obj = oref_lcl
                                       RECEIVING restoration_ok = flag ).

    DATA(state_o) = oref_lcl->get_state( ).
    out->write( |Restoration operation successful? { COND #( WHEN flag = abap_true THEN `YES` ELSE `NO` ) }| ).
    out->write( state_o ).
    out->write( |\n| ).

    "Checking on multiple tags in a loop
    DO 7 TIMES.
      DATA(tag) = |TAG_{ sy-index }|.
      out->write( |Tag used: { tag }| ).

      oref_memento_ctrl->restore_by_tag( EXPORTING tag = tag
                                         CHANGING  lcl_obj = oref_lcl
                                         RECEIVING restoration_ok = flag ).

      DATA(state_p) = oref_lcl->get_state( ).
      out->write( |Restoration operation successful? { COND #( WHEN flag = abap_true THEN `YES` ELSE `NO` ) }| ).
      out->write( state_p ).
      out->write( |\n| ).
    ENDDO.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
CLASS lcl DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*& Memento class
*&---------------------------------------------------------------------*

CLASS lcl_memento DEFINITION FINAL CREATE PRIVATE FRIENDS lcl.
  PUBLIC SECTION.
    METHODS: get_tag RETURNING VALUE(ret_tag) TYPE string,
             get_timestamp RETURNING VALUE(ret_ts) TYPE string.
  PRIVATE SECTION.
    METHODS: constructor IMPORTING state TYPE string
                                   tag   TYPE string,
             get_state RETURNING VALUE(ret_state) TYPE string.
    DATA: state     TYPE string,
          tag       TYPE string,
          timestamp TYPE utclong.
ENDCLASS.

CLASS lcl_memento IMPLEMENTATION.
  METHOD constructor.
    me->state = state.
    me->tag = tag.
    me->timestamp = utclong_current( ).
  ENDMETHOD.

  METHOD get_state.
    ret_state = me->state.
  ENDMETHOD.

  METHOD get_tag.
    ret_tag = me->tag.
  ENDMETHOD.

  METHOD get_timestamp.
    ret_ts = me->timestamp.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class for which mementos should be created
*&---------------------------------------------------------------------*

CLASS lcl DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS get_obj RETURNING VALUE(obj) TYPE REF TO lcl.
    METHODS: get_state RETURNING VALUE(ret_state) TYPE string,
             change_state IMPORTING text      TYPE string
                                    add_space TYPE abap_boolean DEFAULT abap_true,
             save IMPORTING tag                 TYPE string
                  RETURNING VALUE(oref_memento) TYPE REF TO lcl_memento,
             restore IMPORTING memento TYPE REF TO lcl_memento.
  PRIVATE SECTION.
    CLASS-DATA oref TYPE REF TO lcl.
    DATA state TYPE string.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD get_state.
    ret_state = state.
  ENDMETHOD.

  METHOD save.
    oref_memento = NEW #( state = state
                          tag   = tag ).
  ENDMETHOD.

  METHOD restore.
    state = memento->get_state( ).
  ENDMETHOD.

  METHOD change_state.
    state = |{ state }{ COND #( WHEN state IS NOT INITIAL AND add_space = abap_true THEN ` ` ) }{ text }|.
  ENDMETHOD.

  METHOD get_obj.
    IF oref IS NOT BOUND.
      oref = NEW lcl( ).
    ENDIF.
    obj = oref.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class that manages mementos
*&---------------------------------------------------------------------*

CLASS lcl_memento_controller DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: save_memento IMPORTING tag TYPE string,
             undo CHANGING lcl_obj TYPE REF TO lcl,
             redo CHANGING lcl_obj TYPE REF TO lcl,
             restore_by_tag IMPORTING tag                   TYPE string
                            CHANGING  lcl_obj               TYPE REF TO lcl
                            RETURNING VALUE(restoration_ok) TYPE abap_boolean,
             get_memento_log RETURNING VALUE(states) TYPE string_table,
             constructor IMPORTING oref TYPE REF TO lcl.
  PRIVATE SECTION.
    DATA mementos_tab TYPE TABLE OF REF TO lcl_memento WITH EMPTY KEY.
    DATA redo_tab     TYPE TABLE OF REF TO lcl_memento WITH EMPTY KEY.
    DATA lcl_object TYPE REF TO lcl.
ENDCLASS.

CLASS lcl_memento_controller IMPLEMENTATION.
  METHOD save_memento.
    "Clearing the redo table when a new state is added
    CLEAR redo_tab.
    "Adding the memento to the table holding all stored mementos
    APPEND lcl_object->save( tag ) TO mementos_tab.
  ENDMETHOD.

  METHOD undo.
    DATA(count) = lines( mementos_tab ).

    IF count <= 1.
      RETURN.
    ENDIF.

    "Adding the last memento from the memento table to the redo table
    READ TABLE mementos_tab INTO DATA(memento_oref) INDEX count.
    IF sy-subrc = 0.
      APPEND memento_oref TO redo_tab.
      DELETE mementos_tab INDEX count.
    ENDIF.

    "Restoring the previous state
    READ TABLE mementos_tab INTO memento_oref INDEX lines( mementos_tab ).
    IF sy-subrc = 0.
      lcl_object->restore( memento_oref ).
      lcl_obj = lcl_object.
    ENDIF.
  ENDMETHOD.

  METHOD redo.
    DATA(count) = lines( redo_tab ).

    IF count = 0.
      RETURN.
    ENDIF.

    "Restoring the state from the redo table
    READ TABLE redo_tab INTO DATA(memento_oref) INDEX count.
    IF sy-subrc = 0.
      DELETE redo_tab INDEX count.
      "Adding the memento to the memento table
      APPEND memento_oref TO mementos_tab.
      lcl_object->restore( memento_oref ).
      lcl_obj = lcl_object.
    ENDIF.
  ENDMETHOD.

  METHOD restore_by_tag.
    LOOP AT mementos_tab INTO DATA(memento_oref).
      IF memento_oref->get_tag( ) = tag.
        lcl_object->restore( memento_oref ).
        lcl_obj = lcl_object.
        restoration_ok = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
    restoration_ok = abap_false.
  ENDMETHOD.

  METHOD get_memento_log.
    DATA idx TYPE i.
    LOOP AT mementos_tab INTO DATA(memento_wa).
      idx = sy-tabix.
      APPEND |{ idx }) Memento created with tag "{ memento_wa->get_tag( ) }" at { memento_wa->get_timestamp( ) }| TO states.
    ENDLOOP.

    IF lines( redo_tab ) > 0.
      APPEND INITIAL LINE TO states.
      APPEND `*********************** Redo table entries ************************` TO states.
      LOOP AT redo_tab INTO memento_wa.
        idx = sy-tabix.
        APPEND |{ idx }) Memento with tag "{ memento_wa->get_tag( ) }", created at { memento_wa->get_timestamp( ) }| TO states.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    IF lcl_object IS NOT BOUND.
      lcl_object = oref.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Multiton

- The multiton design pattern is a variant of the singleton pattern designed to manage a fixed set of distinct instances, rather than just one.
- While the singleton pattern ensures a class has only one instance across the entire application, returned through a factory method, the multiton pattern controls instantiation by managing multiple unique instances. These instances are differentiated by unique keys, allowing for controlled, instance-specific access.
- The design pattern suggests that a key is mapped to a specific instance in an internal table. A factory method then returns that instance when the corresponding key is provided. If an instance does not exist for the provided key, the factory method can create a new instance, and add it to the internal table.
- This approach is beneficial when you need to control the instantiation process without restricting it to just a single instance. It offers greater flexibility by allowing multiple controlled instances, each suited to handle different needs or contexts. Furthermore, it allows for instance reuse, which can be especially useful in data-intensive or complex scenarios.


<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:** 

- The following example illustrates the multiton design pattern with the following declarations and implementations:
  - Global class:
    - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
    - Serves as a vehicle for demonstrating the design pattern. The declarations and implementations in the CCIMP include are relevant for the conceptual considerations.
    - Represents the user who requires instances to work with.
  - CCIMP include (Local Types tab in ADT):    
    - Class `lcl_multiton` 
        - Specifies the `CREATE PRIVATE` addition to prevent instance creation outside the class.  
        - Maintains one instance per unique key. The key values in this example are determined by ABAP enumerated types, which serve as identifiers for unique instances. The example assumes that users require specific configuration settings among a set of multiple settings, with each setting corresponding to a distinct class instance.  
        - Instance maintenance occurs in the `instance_table` internal table within the private visibility section. This table contains entries for created class instances, mapped to specific keys (the ABAP enumerated types).
        - The `get_instance` method is a public, static factory method that returns an instance of `lcl_multiton` for a given key. It first checks if an entry in the `instance_table` table already exists. If not, it creates a new instance and adds it to the table. Finally, it returns the instance corresponding to the provided key, ensuring that users receive the specific instance they need.
        - The instance `constructor` is declared in the private visibility section to prevent direct instantiation. The assumption is that instantiation may involve complex or data-intensive tasks, which should be avoided if users frequently require instances. The pattern promotes reuse and prevents duplicate instantiations. The simplified demo implementation only adds entries to an internal table for display purposes.
        - Other implementations in the demo class, like the `do_something` dummy method, also serve display purposes. A string table collects information, including the current timestamp, to demonstrate multiple different entries added during class instantiation and method calls. The `log` table tracks the number of instance retrieval requests and the timestamps of the first instantiation per key within the internal session.

      
<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    out->write( |1) First call with key { lcl_multiton=>config_a } \n\n| ).

    DATA(oref_config_a_1) = lcl_multiton=>get_instance( key = lcl_multiton=>config_a ).
    oref_config_a_1->do_something( ).
    DATA(tab) = oref_config_a_1->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).
    out->write( |\n\n| ).

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

**********************************************************************

    out->write( |2) Second call with key { lcl_multiton=>config_a } \n\n| ).

    DATA(oref_config_a_2) = lcl_multiton=>get_instance( key = lcl_multiton=>config_a ).
    oref_config_a_2->do_something( ).
    tab = oref_config_a_2->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).
    out->write( |\n\n| ).

**********************************************************************

    out->write( |3) Third call with key { lcl_multiton=>config_a } \n\n| ).

    DATA(oref_config_a_3) = lcl_multiton=>get_instance( key = lcl_multiton=>config_a ).
    oref_config_a_3->do_something( ).
    tab = oref_config_a_3->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).
    out->write( |\n\n| ).

**********************************************************************

    out->write( |4) First call with key { lcl_multiton=>config_b } \n\n| ).

    DATA(oref_config_b_1) = lcl_multiton=>get_instance( key = lcl_multiton=>config_b ).
    oref_config_b_1->do_something( ).
    tab = oref_config_b_1->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).
    out->write( |\n\n| ).

**********************************************************************

    out->write( |5) Second call with key { lcl_multiton=>config_b } \n\n| ).

    DATA(oref_config_b_2) = lcl_multiton=>get_instance( key = lcl_multiton=>config_b ).
    oref_config_b_2->do_something( ).
    tab = oref_config_b_2->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).
    out->write( |\n\n| ).

**********************************************************************

    out->write( |6) Call with key { lcl_multiton=>config_c } \n\n| ).

    DATA(oref_config_c) = lcl_multiton=>get_instance( key = lcl_multiton=>config_c ).
    oref_config_c->do_something( ).
    tab = oref_config_c->get_config_change_tab( ).

    out->write( data = tab name = `tab` ).
    out->write( |\n\n| ).

**********************************************************************

    "Using random keys
    "The base type of the enumerated type is i, and the example type has 12 components.
    "I.e. config_a corresponds to the value 0, ..., config_m corresponds to 11.
    "Here, the cl_abap_random_int class is used to create random integer values within
    "the range of 0 - 12. This random value is then passed when calling the get_instance
    "method. The output will show which enumerated type component is used.
    DO 5 TIMES.
      DATA(random_number) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                        min  = 0
                                                        max  = 11 )->get_next( ).

      DATA(enum) = CONV lcl_multiton=>t_enum( random_number ).

      out->write( |{ 6 + sy-index }) Call with key { enum } \n\n| ).
      DATA(oref_config_random) = lcl_multiton=>get_instance( key = enum ).
      oref_config_random->do_something( ).
      tab = oref_config_random->get_config_change_tab( ).

      out->write( data = tab name = `tab` ).
      out->write( |\n\n| ).
    ENDDO.

**********************************************************************

    "Log table that tracks the timestamp of the first instantiation per
    "key and the number of calls.
    DATA(log) = lcl_multiton=>log.
    out->write( data = log name = `log` ).

  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
CLASS lcl_multiton DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "ABAP enumerated type serving as identifiers for unique
    "instances
    TYPES: BEGIN OF ENUM t_enum,
             config_a,
             config_b,
             config_c,
             config_e,
             config_f,
             config_g,
             config_h,
             config_i,
             config_j,
             config_k,
             config_l,
             config_m,
           END OF ENUM t_enum.

    "Factory method that returns distinct instances based on given key
    CLASS-METHODS get_instance
      IMPORTING key             TYPE t_enum
      RETURNING VALUE(instance) TYPE REF TO lcl_multiton.

    "Dummy methods
    METHODS do_something.
    METHODS get_config_change_tab RETURNING VALUE(tab) TYPE string_table.

    "Table type and internal table used to store values for display purposes
    TYPES: BEGIN OF ty_log_struc,
             ikey      TYPE t_enum,
             first_instantiation TYPE utclong,
             instance_request_count type i,
           END OF ty_log_struc.

    CLASS-DATA log TYPE SORTED TABLE OF ty_log_struc WITH UNIQUE KEY ikey.

  PRIVATE SECTION.

    "Internal table containing the unique instances that are mapped to keys
    TYPES: BEGIN OF ty_instance_struc,
             ikey     TYPE t_enum,
             instance TYPE REF TO lcl_multiton,
           END OF ty_instance_struc,
           ty_instance_tab TYPE HASHED TABLE OF ty_instance_struc WITH UNIQUE KEY ikey.

    CLASS-DATA instance_table TYPE ty_instance_tab.

    METHODS constructor IMPORTING key TYPE t_enum.

    DATA config_change TYPE string_table.
ENDCLASS.

CLASS lcl_multiton IMPLEMENTATION.

  METHOD get_instance.
    "Factory method the returns an distinct instance out of a fixed set of
    "instance based on a give key. The implementation returns that instance
    "when the corresponding key is provided. If it is not yet stored in the
    "internal table, a new instance is created, added to the table, and returned.

    READ TABLE instance_table INTO DATA(ref) WITH KEY ikey = key.
    IF ref-instance IS NOT BOUND.
      ref-instance = NEW #( key ).
      INSERT VALUE #( ikey = key instance = ref-instance ) INTO TABLE instance_table.
    ENDIF.
    instance = ref-instance.

    "Modifying an entry in an internal table to log the number of instance retrieval requests
    "for display purposes.
    log[ ikey = key ]-instance_request_count += 1.
  ENDMETHOD.

  METHOD constructor.
    DATA(timestamp) = utclong_current( ).

    "Adding entries to an internal table for display purposes.
    "The example simply adds text to a string table, including timestamp values,
    "to illustrate different values when calling the method.
    APPEND key TO config_change.
    APPEND |constructor called at { timestamp }| TO config_change.

    IF NOT line_exists( log[ ikey = key ] ).
      INSERT VALUE #( ikey = key
                      first_instantiation = timestamp ) INTO TABLE log.

      APPEND |Instance for key { key } first created at { timestamp }| TO config_change.
    ENDIF.

    "Assumption: Complex or data-intensive tasks are executed to create an initial state
    "of instances.
    CASE key.
      WHEN config_a.
        ...
      WHEN config_b.
        ...
      WHEN config_c.
        ...

        ...
    ENDCASE.

  ENDMETHOD.

  METHOD do_something.
    APPEND |do_something called at { utclong_current( ) }| TO config_change.
  ENDMETHOD.

  METHOD get_config_change_tab.
    APPEND |get_config_change_tab called at { utclong_current( ) }| TO config_change.

    tab = config_change.
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Observer

- The observer design pattern allows an object to notify other objects - the *observers* - whenever its state changes.
- A class setup may include the following (as implemented in the first code example):
  - Observers can register or unregister to receive notifications (or not receive them anymore) about state changes of an observed object. The notification of observers may include parameters to be passed. The observed class may contain an internal table to store references to observers, along with methods for adding and removing those references. When a change occurs, a notification method can inform the observers by iterating through the internal table and calling a method that all observers implement through a common interface. This enables the observed object to communicate with all registered observers.
  - All registered observers can react to the information received.
  - Users of the setup are responsible not only for the object creation but also for the flexible registration and unregistration of observers.
- Such a setup can be useful when you want to avoid tightly coupling different objects while still providing a mechanism to transfer changes from a specific object to a random number of other objects.


<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- The example code includes two examples demonstrating the observer design pattern with the following declarations and implementations:
  - Global class:
    - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
    - Serves as a vehicle for demonstrating the design pattern. The declarations and implementations in the `CCIMP` are relevant for the for conceptual considerations.
  - CCIMP include (Local Types tab in ADT):
    - *Example 1*  
      - Defines the `lif_observer` interface that enables the observed object to communicate with observers via the `notify` method, which expects two numbers as importing parameters, among others. The `calculate` method is common to all observers, performing a simple calculation based on the two integer values provided.  
      - `lcl` represents the class for observed objects and offers several methods:  
        - `set_numbers`: Changes the state of the observed object by setting two integer values. The implementation includes calling the `notify_observers` method.
        - `register` and `unregister` are used to register and unregister observers. For that purpose, the internal table `observer_tab` is used to manage reference variables. When registering, references to observers are added. When unregistering, references are removed from the table.  
        - `notify_observers`: The method is responsible for notifying observers. The notification is performed by looping across the reference table and calling the common notification method `notify` defined in the interface.  
      - A string table is included for demonstration and display purposes, being populated throughout method calls to visualize the method call flow.  
      - Multiple local classes act as observers, performing simple calculations.  
      - When the observed object calls the `notify` method, it informs the observer about the state change, passes values, and triggers a calculation by invoking the `calculate` method.  
      - The global class represents the user, creating objects and registering and unregistering observers. The string table is output to visualize the method call flow.  
    - *Example 2*  
      - Provides a simpler example and setup, where the class being observed defines events.  
      - An interface implemented by all observers defines an event handler.  
      - Observers are notified through a `RAISE EVENT` statement, provided that event handlers are registered with `SET HANDLER` statements, which triggers the calling of event handlers. When they are not registered, the event handlers are not called.       
      - Like in example 1, a string table logs the method call flow, which is output.  
      - The user, represented by the global class implementation, manages object creation, and registration and unregistration using `SET HANDLER` statements. When the observed object's `set_text` method is called, the event is raised and handled accordingly by event handlers in the registered observers.

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    out->write( |Example 1\n\n| ).

*&-------------------------------------------------------------------------------------*
*& 1) Creating objects, observers not registered
*&-------------------------------------------------------------------------------------*

    out->write( |1) Creating objects, observers not registered\n\n| ).

    DATA(oref_observed) = NEW lcl( ).
    "Observers
    DATA(observer_add) = NEW lcl_addition( ).
    DATA(observer_subtract) = NEW lcl_subtraction( ).
    DATA(observer_multiply) = NEW lcl_multiplication( ).
    DATA(observer_divide) = NEW lcl_division( ).

    "Setting numbers representing a change of the object state
    "The method implementation includes a call to the notification method.
    oref_observed->set_numbers(
      value1 = 1
      value2 = 2 ).

    "At this stage, there are no observers registered.
    DATA(log) = lcl=>tab4display.

    out->write( data = log name = `log` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).
    CLEAR lcl=>tab4display.

*&-------------------------------------------------------------------------------------*
*& 2) Registering observers
*&-------------------------------------------------------------------------------------*

    out->write( |2) Registering observers\n\n| ).

    oref_observed->register( observer_add ).
    oref_observed->register( observer_subtract ).
    oref_observed->register( observer_multiply ).
    oref_observed->register( observer_divide ).

    oref_observed->set_numbers(
      value1 = 7
      value2 = 5 ).

    log = lcl=>tab4display.

    out->write( data = log name = `log` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

    CLEAR lcl=>tab4display.

*&-------------------------------------------------------------------------------------*
*& 3) Unregistering observers
*&-------------------------------------------------------------------------------------*

    out->write( |3) Unregistering observers\n\n| ).

    oref_observed->unregister( observer_multiply ).
    oref_observed->unregister( observer_divide ).

    oref_observed->set_numbers(
      value1 = 15
      value2 = 3 ).

    log = lcl=>tab4display.

    out->write( data = log name = `log` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

    CLEAR lcl=>tab4display.

*&-------------------------------------------------------------------------------------*
*& 4) More example values
*&-------------------------------------------------------------------------------------*

    out->write( |4) More example values\n\n| ).

    "Registering the unregistered observers again
    oref_observed->register( observer_multiply ).
    oref_observed->register( observer_divide ).

    "Creating an internal table holding two integer values per line
    TYPES: BEGIN OF struc_int,
             number1 TYPE i,
             number2 TYPE i,
           END OF struc_int.

    DATA numbers_tab TYPE TABLE OF struc_int WITH EMPTY KEY.

    numbers_tab = VALUE #( ( number1 = 1 number2 = 8 )
                           ( number1 = 50 number2 = 25 )
                           ( number1 = 100 number2 = 4 )
                           ( number1 = 24 number2 = 6 )
                           ( number1 = 35 number2 = 7 )
                           ( number1 = 2 number2 = 0 ) ).

    LOOP AT numbers_tab INTO DATA(wa).
      oref_observed->set_numbers(
        value1 = wa-number1
        value2 = wa-number2 ).

      APPEND INITIAL LINE TO lcl=>tab4display.
    ENDLOOP.

    log = lcl=>tab4display.

    out->write( data = log name = `log` ).
    out->write( |{ repeat( val = `*` occ = 75 ) }| ).
    out->write( |{ repeat( val = `*` occ = 75 ) }| ).
    out->write( |{ repeat( val = `*` occ = 75 ) }\n| ).

**********************************************************************
**********************************************************************
**********************************************************************

*&-------------------------------------------------------------------------------------*
*& Example 2
*&-------------------------------------------------------------------------------------*

    out->write( |Example 2\n\n| ).

    out->write( |6) Registering event handlers\n\n| ).

    DATA(o1) = NEW lcl_evt( ).
    DATA iref1 TYPE REF TO lif_obs.
    iref1 = NEW lcl_obs_1( ).
    DATA iref2 TYPE REF TO lif_obs.
    iref2 = NEW lcl_obs_2( ).

    "Registering event handlers
    SET HANDLER iref1->handle_event FOR o1.
    SET HANDLER iref2->handle_event FOR o1.

    o1->set_text( `AB` ).

    log = lcl_evt=>tab4display.
    out->write( data = log name = `log` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

    CLEAR lcl_evt=>tab4display.

**********************************************************************

    out->write( |7) Unregistering an event handler\n\n| ).

    SET HANDLER iref2->handle_event FOR o1 ACTIVATION ' '.

    o1->set_text( `Hello AB` ).

    log = lcl_evt=>tab4display.
    out->write( data = log name = `log` ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Interface for observers
*&---------------------------------------------------------------------*

INTERFACE lif_observer.
  METHODS: notify IMPORTING num1 TYPE i
                            num2 TYPE i
                            ts   TYPE utclong,
   calculate IMPORTING num1 TYPE i
                       num2 TYPE i
             RETURNING VALUE(result) TYPE string.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Class for observed object
*&---------------------------------------------------------------------*

CLASS lcl DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      set_numbers IMPORTING value1 TYPE i value2 TYPE i,
      register IMPORTING oref_observer TYPE REF TO lif_observer,
      unregister IMPORTING oref_observer TYPE REF TO lif_observer,
      notify_observers.

    "String table for display purposes
    CLASS-DATA tab4display TYPE string_table.
  PROTECTED SECTION.
    DATA: observer_tab TYPE TABLE OF REF TO lif_observer,
          num1         TYPE i,
          num2         TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD register.
    IF NOT line_exists( observer_tab[ table_line = oref_observer ] ).
      INSERT oref_observer INTO TABLE observer_tab.

      "Populating a string table for display purposes.
      APPEND |Object reference for { cl_abap_typedescr=>describe_by_object_ref( oref_observer )->get_relative_name( ) } inserted into observer table.| TO lcl=>tab4display.
    ENDIF.
  ENDMETHOD.
  METHOD unregister.
    DELETE TABLE observer_tab FROM oref_observer.

    "Populating a string table for display purposes.
    APPEND |Object reference for { cl_abap_typedescr=>describe_by_object_ref( oref_observer )->get_relative_name( ) } removed from observer table.| TO lcl=>tab4display.
  ENDMETHOD.

  METHOD notify_observers.
    LOOP AT observer_tab ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>->notify( num1 = me->num1 num2 = me->num2 ts = utclong_current( ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_numbers.
    num1 = value1.
    num2 = value2.
    
    "Populating a string table for display purposes.
    APPEND |The number values { num1 } and { num2 } were set in the observed object. Observers are about to be notified.| TO lcl=>tab4display.
    APPEND INITIAL LINE TO lcl=>tab4display.
    notify_observers( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Classes for observers
*&---------------------------------------------------------------------*

CLASS lcl_addition DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_observer.
ENDCLASS.

CLASS lcl_addition IMPLEMENTATION.
  METHOD lif_observer~notify.
    "Populating a string table for display purposes.
    APPEND |lcl_addition: Observer notified at { ts }.| TO lcl=>tab4display.

    lif_observer~calculate( num1 = num1 num2 = num2 ).
  ENDMETHOD.

  METHOD lif_observer~calculate.
    TRY.
        result = |{ num1 + num2 STYLE = SIMPLE }|.
      CATCH cx_sy_arithmetic_error INTO DATA(error).
        result = error->get_text( ).
    ENDTRY.

    "Populating a string table for display purposes.
    APPEND `lcl_addition: lif_observer~calculate called.` TO lcl=>tab4display.
    APPEND |{ num1 } + { num2 } = { result }| TO lcl=>tab4display.
    APPEND INITIAL LINE TO lcl=>tab4display.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_subtraction DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_observer.
ENDCLASS.

CLASS lcl_subtraction IMPLEMENTATION.
  METHOD lif_observer~notify.
    "Populating a string table for display purposes.
    APPEND |lcl_subtraction: Observer notified at { ts }.| TO lcl=>tab4display.

    lif_observer~calculate( num1 = num1 num2 = num2 ).
  ENDMETHOD.

  METHOD lif_observer~calculate.
    TRY.
        result = |{ num1 - num2 STYLE = SIMPLE }|.
      CATCH cx_sy_arithmetic_error INTO DATA(error).
        result = error->get_text( ).
    ENDTRY.

    "Populating a string table for display purposes.
    APPEND `lcl_subtraction: lif_observer~calculate called.` TO lcl=>tab4display.
    APPEND |{ num1 } - { num2 } = { result }| TO lcl=>tab4display.
    APPEND INITIAL LINE TO lcl=>tab4display.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_multiplication DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_observer.
ENDCLASS.

CLASS lcl_multiplication IMPLEMENTATION.
  METHOD lif_observer~notify.
    "Populating a string table for display purposes.
    APPEND |lcl_multiplication: Observer notified at { ts }.| TO lcl=>tab4display.

    lif_observer~calculate( num1 = num1 num2 = num2 ).
  ENDMETHOD.

  METHOD lif_observer~calculate.
    TRY.
        result = |{ num1 * num2 STYLE = SIMPLE }|.
      CATCH cx_sy_arithmetic_error INTO DATA(error).
        result = error->get_text( ).
    ENDTRY.

    "Populating a string table for display purposes.
    APPEND `lcl_multiplication: lif_observer~calculate called.` TO lcl=>tab4display.
    APPEND |{ num1 } * { num2 } = { result }| TO lcl=>tab4display.
    APPEND INITIAL LINE TO lcl=>tab4display.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_division DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_observer.
ENDCLASS.

CLASS lcl_division IMPLEMENTATION.
  METHOD lif_observer~notify.
    "Populating a string table for display purposes.
    APPEND |lcl_division: Observer notified at { ts }.| TO lcl=>tab4display.

    lif_observer~calculate( num1 = num1 num2 = num2 ).
  ENDMETHOD.

  METHOD lif_observer~calculate.
    TRY.
        result = |{ CONV decfloat34( num1 / num2 ) STYLE = SIMPLE }|.
      CATCH cx_sy_arithmetic_error INTO DATA(error).
        result = error->get_text( ).
    ENDTRY.

    "Populating a string table for display purposes.
    APPEND `lcl_division: lif_observer~calculate called.` TO lcl=>tab4display.
    APPEND |{ num1 } / { num2 } = { result }| TO lcl=>tab4display.
    APPEND INITIAL LINE TO lcl=>tab4display.
  ENDMETHOD.

ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

CLASS lcl_evt DEFINITION.
  PUBLIC SECTION.
    METHODS set_text IMPORTING txt TYPE string.
    EVENTS evt EXPORTING VALUE(info) TYPE string.

    CLASS-DATA tab4display TYPE string_table.
  PROTECTED SECTION.
    DATA text TYPE string.
ENDCLASS.

CLASS lcl_evt IMPLEMENTATION.
  METHOD set_text.
    text = txt.

    "Populating a string table for display purposes.
    APPEND |Object changed. "text" was assigned the value "{ text }". Event is about to be raised.| TO lcl_evt=>tab4display.
    APPEND INITIAL LINE TO lcl_evt=>tab4display.

    RAISE EVENT evt EXPORTING info = txt.
  ENDMETHOD.
ENDCLASS.

INTERFACE lif_obs.
  METHODS:
    add_text IMPORTING txt TYPE string,
    handle_event FOR EVENT evt OF lcl_evt IMPORTING info.
  DATA some_text TYPE string.
ENDINTERFACE.

CLASS lcl_obs_1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_obs.
ENDCLASS.

CLASS lcl_obs_1 IMPLEMENTATION.

  METHOD lif_obs~handle_event.
    "Populating a string table for display purposes.
    APPEND `lcl_obs_1: Event handled in lif_obs~handle_event.` TO lcl_evt=>tab4display.
    APPEND |Text "{ info }" is about to be processed.| TO lcl_evt=>tab4display.

    lif_obs~add_text( info ).
  ENDMETHOD.

  METHOD lif_obs~add_text.
    lif_obs~some_text = |{ txt }AP|.

    "Populating a string table for display purposes.
    APPEND |Text was processed in lif_obs~add_text. Result: "{ lif_obs~some_text }"| TO lcl_evt=>tab4display.
    APPEND INITIAL LINE TO lcl_evt=>tab4display.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_obs_2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_obs.
ENDCLASS.

CLASS lcl_obs_2 IMPLEMENTATION.

  METHOD lif_obs~handle_event.
    "Populating a string table for display purposes.
    APPEND `lcl_obs_2: Event handled in lif_obs~handle_event.` TO lcl_evt=>tab4display.
    APPEND |Text "{ info }" is about to be processed.| TO lcl_evt=>tab4display.

    lif_obs~add_text( info ).
  ENDMETHOD.

  METHOD lif_obs~add_text.
    lif_obs~some_text = |{ txt }CDEFGHIJKLMNOPQRSTUVWXYZ|.

    "Populating a string table for display purposes.
    APPEND |Text was processed in lif_obs~add_text. Result: "{ lif_obs~some_text }"| TO lcl_evt=>tab4display.
    APPEND INITIAL LINE TO lcl_evt=>tab4display.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Prototype

- The prototype design pattern suggests that you can clone objects to create copies of existing objects of classes.
- Cloning objects lets you preserve, reuse, and enhance an object's current state without altering the original prototype.
- This is especially useful when you want to delegate the task of object creation, particulary when the creation involves complex steps, instead of doing it manually. In cases where you need similar or the same objects, the prototype pattern simplifies and automates object creation, ensuring a consistent approach with the prototype providing a predefined state.
- Reusing and enhancing prototypes allows you to create variants of original objects. This pattern offers an alternative to complex inheritance trees, with object variants created through subclasses.
- In the pattern, prototype objects handle their own cloning. This can be done by providing a shared interface for all clonable objects. The interface may include a clone method. This clone method can be implemented to copy the entire state of the current object, including private attributes, or exclude attributes that should not be cloned. Instead of manually cloning objects to achieve a specific initial state, the prototype takes over the cloning, allowing users to easily retrieve the objects.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- The simplified example illustrates the prototype design pattern with the following declarations and implementations:
  - Global class:
    - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
    - Serves as a vehicle for demonstrating the design pattern. The declarations and implementations in the `CCIMP` are relevant for the for conceptual considerations.
    - The global class represents the user who creates objects of prototype classes and requires clones of these classes. 
  - CCIMP include (Local Types tab in ADT):    
    - Interface `lif_prototype` 
		- Serves as the common interface for all prototype classes, enforcing the implementation of the `clone` method.
		- This method allows the creation of an clone of an object of a class that implements this interface. 
		- It returns a new reference, representing the object clone.
	- Class `lcl_prototype_1`
	  - This demo class implements the `lif_prototype` interface that allows users to retrieve a clone of an object from the class.
	  - Contains various class attributes and methods. Methods such as `do_something` modify the value of class attributes.
	  - The constructor (or any other part of the class that modifies the object's state) assumes there may be complex operations involved. However, this simplified example only assigns values to class attributes to demonstrate object cloning and enhancement.
	  - The clone method `lif_prototype~clone` creates a clone of the current object by instantiating a new class instance with the `NEW` operator and copying the class attribute values. This example selectively clones certain attributes, showcasing a predefined state for the cloned object.  
	- Class `lcl_prototype_2`
	  - This class is structured similarly to `lcl_prototype_1`, but it features different attributes and behaviors.		
- The class execution includes the following:
  - The global class contains several method calls and class attribute accesses, demonstrating the prototype pattern. Both local classes, `lcl_prototype_1` and `lcl_prototype_2`, are included.
  - Steps 1 and 4: Create objects of the prototype classes. Methods are called and class attributes are accessed to show changes in attribute values, resulting in specific object states.
  - Steps 2 and 5: Clone objects using the `clone` method, which creates copies of existing objects. The assumption is that the user needs a copy of the object to reuse a specific state instead of creating a new object from scratch.
  - Steps 3 and 6: Enhance the cloned objects. This involves casting to the prototype class to access its attributes and methods. The assumption is to create a variant of the object by modifying its attributes while using the original state as a foundation. Working with the variant involves a separate object, distinct from the original. A demo attribute shows that the clone method can be implemented to allow selective cloning, excluding certain attribute values.
  - The example outputs various class attributes to illustrate both the original object states and the enhanced clone object states.
      
<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    out->write( |1) Processing original object (prototype 1) \n\n| ).

    "Notes:
    "- Creating objects of the prototype classes.
    "- Methods are called and class attributes are accessed to show changes in attribute values,
    "  resulting in specific object states.

    DATA(oref_1) = NEW lcl_prototype_1( str = `ABC` ).
    oref_1->do_something( `DEF` ).
    DATA(content) = oref_1->return_data( ).
    DATA(num) = oref_1->number.
    DATA(txt) = oref_1->txt.

    "Displaying content
    out->write( data = content name = `content` ).
    out->write( |\n| ).
    out->write( data = num name = `num` ).
    out->write( |\n| ).
    out->write( data = txt name = `txt` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

**********************************************************************

    out->write( |2) Cloning prototype (1) \n\n| ).

    "Notes:
    "- Cloning objects using the clone method, which creates copies of existing
    "  objects.
    "- The assumption is that the user needs a copy of the object to reuse a
    "  specific state instead of creating a new object from scratch.
    "- At this stage, the cloned object has the same state as the original object.

    DATA(oref_2) = oref_1->lif_prototype~clone( ).

    "Displaying the public class atrributes
    out->write( data = oref_2 name = `oref_2` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

**********************************************************************

    out->write( |3) Enhancing cloned prototype (1) \n\n| ).

    "Notes:
    "- Enhancing the cloned objects.
    "- This involves casting to the prototype class to access its attributes
    "  and methods.
    "- The assumption is to create a variant of the object by modifying its
    "  attributes while using the original state as a foundation.
    "- Working with the variant involves a separate object, distinct from the original.
    "  A demo attribute shows that the clone method can be implemented to allow selective
    "  cloning, excluding certain attribute values.

    DATA(oref_3) = CAST lcl_prototype_1( oref_2 ).
    content = oref_3->return_data( ).
    num = oref_3->number.
    txt = oref_3->txt.

    "Displaying content (original state)
    out->write( data = content name = `content` ).
    out->write( |\n| ).
    out->write( data = num name = `num` ).
    out->write( |\n| ).
    out->write( data = txt name = `txt` ).
    out->write( |\n\n| ).

    "Enhancing the cloned object
    oref_3->do_something( `GHI` ).
    content = oref_3->return_data( ).
    num = oref_3->number.
    txt = oref_3->txt.

    "Displaying content (enhanced state)
    out->write( data = content name = `content` ).
    out->write( |\n| ).
    out->write( data = num name = `num` ).
    out->write( |\n| ).
    out->write( data = txt name = `txt` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

**********************************************************************

    out->write( |4) Processing original object (prototype 2) \n\n| ).

    "Notes:
    "- Creating objects of the prototype classes.
    "- Methods are called and class attributes are accessed to show changes in attribute values,
    "  resulting in specific object states.

    DATA(oref_4) = NEW lcl_prototype_2( `JKL` ).
    oref_4->do_another_thing( `MNO` ).
    DATA(tab) = oref_4->tab.
    DATA(string) = oref_4->string.
    DATA(state) = oref_4->get_state( ).

    "Displaying content
    out->write( data = tab name = `tab` ).
    out->write( |\n| ).
    out->write( data = string name = `string` ).
    out->write( |\n| ).
    out->write( data = state name = `state` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

**********************************************************************

    out->write( |5) Cloning prototype (2) \n\n| ).

    "Notes:
    "- Cloning objects using the clone method, which creates copies of existing
    "  objects.
    "- The assumption is that the user needs a copy of the object to reuse a
    "  specific state instead of creating a new object from scratch.
    "- At this stage, the cloned object has the same state as the original object.

    DATA(oref_5) = oref_4->lif_prototype~clone( ).

    "Displaying the public class atrributes
    out->write( data = oref_5 name = `oref_5` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

**********************************************************************

    out->write( |6) Enhancing cloned prototype (2) \n\n| ).

    "Notes:
    "- Enhancing the cloned objects.
    "- This involves casting to the prototype class to access its attributes
    "  and methods.
    "- The assumption is to create a variant of the object by modifying its
    "  attributes while using the original state as a foundation.
    "- Working with the variant involves a separate object, distinct from the original.
    "  A demo attribute shows that the clone method can be implemented to allow selective
    "  cloning, excluding certain attribute values.

    DATA(oref_6) = CAST lcl_prototype_2( oref_5 ).
    tab = oref_6->tab.
    string = oref_6->string.
    state = oref_6->get_state( ).

    "Displaying content (original state)
    out->write( data = tab name = `tab` ).
    out->write( |\n| ).
    out->write( data = string name = `string` ).
    out->write( |\n| ).
    out->write( data = state name = `state` ).
    out->write( |\n\n| ).

    "Enhancing the cloned object
    oref_6->do_another_thing( `PQR` ).
    tab = oref_6->tab.
    string = oref_6->string.
    state = oref_6->get_state( ).

    "Displaying content (enhanced state)
    out->write( data = tab name = `tab` ).
    out->write( |\n| ).
    out->write( data = string name = `string` ).
    out->write( |\n| ).
    out->write( data = state name = `state` ).
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Interface serving as the common interface for prototypes
*&---------------------------------------------------------------------*
"- The interface serves as the common interface for all prototype classes,
"  enforcing the implementation of the clone method.
"- This method allows the creation of an clone of an object of a class that
"  implements this interface.
"- It returns a new reference, representing the object clone.

INTERFACE lif_prototype.
  METHODS clone RETURNING VALUE(ref) TYPE REF TO lif_prototype.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Prototype class 1
*&---------------------------------------------------------------------*
"- This demo class implements the lif_prototype interface that allows users
"  to retrieve a clone of an object from the class.
"- Contains various class attributes and methods. Methods such as 'do_something'
"  modify the value of class attributes.

CLASS lcl_prototype_1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_prototype.
    METHODS constructor IMPORTING str TYPE string.
    METHODS do_something IMPORTING text TYPE string.
    METHODS return_data RETURNING VALUE(str) TYPE string.

    DATA number TYPE i.
    DATA txt TYPE string.
    DATA itab TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA private_text TYPE string.
ENDCLASS.

CLASS lcl_prototype_1 IMPLEMENTATION.

  METHOD constructor.
    "The constructor (or any other part of the class that modifies the object's state) assumes
    "there may be complex operations involved. However, this simplified example only assigns
    "values to class attributes to demonstrate object cloning and enhancement.

    txt = str.
    private_text = `#`.
    itab = VALUE #( ( |{ txt } (added at { utclong_current( ) })| ) ).
  ENDMETHOD.

  METHOD lif_prototype~clone.
    "Creating a clone of the current object by instantiating a new class instance with the NEW
    "operator and copying the class attribute values. This example selectively clones certain
    "attributes, showcasing a predefined state for the cloned object.

    DATA(obj_ref) = NEW lcl_prototype_1( txt ).
    obj_ref->itab = me->itab.
    obj_ref->txt = me->txt.
    obj_ref->private_text = me->private_text.
    ref = obj_ref.
  ENDMETHOD.

  METHOD do_something.
    "The demo implementation includes some simple class attribute modifications.

    APPEND |{ text } (added at { utclong_current( ) })| TO itab.
    number += 1.
    txt &&= `!`.
    private_text &&= `#`.
  ENDMETHOD.

  METHOD return_data.
    "The demo implementation returns the content of the string table as string.
    "Additionally and for display purposes, the content of a private attribute
    "is added to the result string.

    str = concat_lines_of( table = itab sep = |\n| ).
    str &&= |\nprivate_text = "{ private_text }"|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Prototype class 2
*&---------------------------------------------------------------------*
"- This demo class implements the lif_prototype interface that allows users
"  to retrieve a clone of an object from the class.
"- It is structured similarly to lcl_prototype_1, but it features different
"  attributes and behaviors.

CLASS lcl_prototype_2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_prototype.
    METHODS constructor IMPORTING t TYPE string.
    METHODS do_another_thing IMPORTING text TYPE string.
    METHODS get_state RETURNING VALUE(state) TYPE string.

    DATA string TYPE string.
    DATA tab TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA private_integer TYPE i.
ENDCLASS.

CLASS lcl_prototype_2 IMPLEMENTATION.

  METHOD constructor.
    string = t.
    tab = VALUE #( ( |{ string } (added at { utclong_current( ) }| ) ).
  ENDMETHOD.

  METHOD lif_prototype~clone.
    DATA(obj_ref) = NEW lcl_prototype_2( string ).
    obj_ref->tab = me->tab.
    obj_ref->string = me->string.
    ref = obj_ref.
  ENDMETHOD.

  METHOD do_another_thing.
    APPEND |{ text } (added at { utclong_current( ) })| TO tab.
    private_integer += 1.
    string &&= `#`.
  ENDMETHOD.

  METHOD get_state.
    "This demo implementation is intended to visualize enhancement of cloned objects.
    "In this case, the value of the private attribute is evaluated.
    IF private_integer > 0.
      state = |State changed. Value of "private_integer": { private_integer }|.
    ELSE.
      state = |State not changed. Value of "private_integer": { private_integer }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details> 


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Proxy

- The proxy design pattern proposes that an object of a class serves as a surrogate for another object.
- This means users interact with objects of a proxy class instead of interacting directly with an object of the original class.
- In this way, the proxy class can delegate and forward access to the original class as if users were working directly with it.
- Among the use cases for this design pattern is access control. A proxy class manages and restricts access to objects of another class, particularly for sensitive operations that only authorized users should perform. This setup prevents unauthorized access to the original class, ensuring that its methods cannot be called through the proxy. Additionally, you can enhance the proxy class with features like a logging mechanism that the original class does not provide, without modifying the original class itself. Other potential use cases include proxy classes that serve as surrogates for objects located in remote destinations, allowing interaction with remote objects through these proxies.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- The simplified example illustrates an access control context where only authorized users can retrieve data via a proxy class.  
- This example demonstrates the proxy design pattern with the following declarations and implementations:  
  - Global class:  
    - Implements the `if_oo_adt_classrun` interface, so the class can be run using F9 in ADT.
    - Serves as a vehicle for showcasing the design pattern. The declarations and implementations in the CCIMP include are relevant for conceptual considerations.  
    - The implementation includes multiple access attempts, i.e. method calls, through the proxy class. If authorized, the calls are delegated to the original class. It includes both successful and discarded calls, and finally, it retrieves and displays the content of a log table.
  - CCIMP include (Local Types tab in ADT):  
	- Interface `lif_user_info`: Defines the common interface for both the original and proxy classes.  
	- Class `lcl_user_info`: Represents the original class that implements the interface and contains the logic for retrieving user information.
	- Class `lcl_proxy`: 
		- Represents the proxy class. 
		- Implements the interface to serve as a surrogate of the original class. So, users interact with this class instead of the the original class. 
		- The class maintains a reference to the original class and delegates requests to it. The logic can only be executed, and data can only be retrieved if authentication validation succeeds. The example uses hardcoded demo values for usernames and passwords. If validation fails, an object of the original class cannot be created, an exception is raised (local exception class `lcx_error`). 
		- The original class, `lcl_user_info`, is declared with `CREATE PRIVATE` to prevent external object creation. Only the class itself and its friends can instantiate objects, which is why the proxy class is declared as a friend of the class.
		- The proxy class includes additional functionality by implementing a simple logging mechanism. In this example, logging is achieved through entries in an internal table, which is a static component to track all access attempts from the internal session. Logging to a database table for permanent storage may be an option.


<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    out->write( `User that is allowed to get user info` ).
    TRY.
        DATA(oref_power_user) = NEW lcl_proxy( username = `power_user` password = `abc123` ).
        DATA(info_a) = oref_power_user->lif_user_info~get_user_info( user_id = `A0001` ).
        DATA(info_b) = oref_power_user->lif_user_info~get_user_info( user_id = `A0002` ).

        out->write( info_a ).
        out->write( info_b ).
      CATCH lcx_error INTO DATA(error).
        out->write( error->get_text( ) ).
    ENDTRY.

    out->write( |\n| ).
    out->write( `User that is not allowed to get user info` ).
    TRY.
        DATA(oref_other_user) = NEW lcl_proxy( username = `other_user` password = `some_pw` ).
        DATA(info_c) = oref_other_user->lif_user_info~get_user_info( user_id = `A0003` ).
        DATA(info_d) = oref_other_user->lif_user_info~get_user_info( user_id = `A0004` ).

        out->write( info_c ).
        out->write( info_d ).
      CATCH lcx_error INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

    out->write( |\n| ).
    out->write( `Wrong credentials` ).
    TRY.
        DATA(oref_typo) = NEW lcl_proxy( username = `power_user` password = `abc1234` ).
        DATA(info_e) = oref_typo->lif_user_info~get_user_info( user_id = `A0005` ).
        DATA(info_f) = oref_typo->lif_user_info~get_user_info( user_id = `A0006` ).

        out->write( info_e ).
        out->write( info_f ).
      CATCH lcx_error INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

    out->write( |\n| ).
    out->write( `Log` ).
    DATA(log) = lcl_proxy=>get_log( ).
    out->write( log ).

  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Interface serving as the common interface for both the original
*& and proxy classes
*&---------------------------------------------------------------------*

INTERFACE lif_user_info.
  METHODS get_user_info IMPORTING user_id     TYPE string
                        RETURNING VALUE(info) TYPE string.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Local exception class for errors due to failed authorization
*& validations
*&---------------------------------------------------------------------*

CLASS lcx_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.

    DATA text TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        text     TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcx_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    me->text = text.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Original class
*&---------------------------------------------------------------------*

"- Contains the business logic for retrieving user information
"- Declared with CREATE PRIVATE to prevent external object creation. Only the
"  class itself and its friends can instantiate objects, which is why the proxy
"  class is declared as a friend of the class. Since the proxy class is not
"  known yet in the CCIMP include, the class declaration is deferred.

CLASS lcl_proxy DEFINITION DEFERRED.

CLASS lcl_user_info DEFINITION CREATE PRIVATE FRIENDS lcl_proxy.
  PUBLIC SECTION.
    INTERFACES lif_user_info.
  PRIVATE SECTION.
    "For demo purposes, adding hardcoded values for a user authorized
    "to retrieve the data.
    CONSTANTS: user_name TYPE string VALUE `power_user`,
               pw        TYPE string VALUE `abc123`.
ENDCLASS.

CLASS lcl_user_info IMPLEMENTATION.
  METHOD lif_user_info~get_user_info.

    "Here goes an operation to get user information.
*    SELECT user_id, info_a, info_b
*     FROM some_data_source
*     WHERE user_id = @user_id
*     INTO ...

    info = |Information for user { user_id }: Some information ...|.

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Proxy class
*&---------------------------------------------------------------------*

"- Implements the interface to serve as a surrogate of the original class.
"  So, users interact with this class instead of the the original class.
"- Maintains a reference to the original class and delegates requests to it.
"  The business logic can only be executed, and data can only be retrieved
"  if authentication validation succeeds. If validation fails, the an object
"  of the original class cannot be created, and exception is raised.
"- Extra functionality is added to the proxy class by implementing a
"  simplified logging mechanism, realized by an internal table in the example.
CLASS lcl_proxy DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_user_info.
    METHODS constructor IMPORTING username TYPE string
                                  password TYPE string
                        RAISING   lcx_error.

    TYPES: BEGIN OF ty_log,
             user           TYPE string,
             time_stamp     TYPE utclong,
             access_granted TYPE abap_boolean,
           END OF ty_log,
           tt_log TYPE TABLE OF ty_log WITH EMPTY KEY.

    CLASS-METHODS: add_log_entry IMPORTING user_name      TYPE string
                                           time_stamp     TYPE utclong
                                           access_granted TYPE abap_boolean,
                   get_log RETURNING VALUE(log) TYPE tt_log.

  PRIVATE SECTION.

    DATA oref TYPE REF TO lcl_user_info..
    CLASS-DATA log_tab TYPE tt_log.
ENDCLASS.

CLASS lcl_proxy IMPLEMENTATION.
  METHOD constructor.

    "Here goes a proper authorization check ...
    "The simplified example includes a dummy authorization check.
    IF username = lcl_user_info=>user_name AND password = lcl_user_info=>pw.
      "Adding a log entry
      add_log_entry( user_name = username time_stamp = utclong_current( ) access_granted = abap_true ).

      "Instantiating the original class
      oref = NEW #( ).
    ELSE.
      "Add log entry
      add_log_entry( user_name = username time_stamp = utclong_current( ) access_granted = abap_false ).

      "Dummy authentication failed
      RAISE EXCEPTION TYPE lcx_error.
    ENDIF.

  ENDMETHOD.

  METHOD lif_user_info~get_user_info.
    "Calling a method of the original class via the proxy class
    "The calling is only possible if the authorization validation was successful
    "and an instance was created.
    TRY.
        info = oref->lif_user_info~get_user_info( user_id ).
      CATCH cx_sy_ref_is_initial.
        info = `No authorization`.
    ENDTRY.
  ENDMETHOD.

  METHOD add_log_entry.
    APPEND VALUE #( user = user_name time_stamp = time_stamp access_granted = access_granted ) TO log_tab.
  ENDMETHOD.

  METHOD get_log.
    log = log_tab.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Singleton

- Used to restrict external users from instantiating a class.  
- This is typically achieved by using the `CREATE PRIVATE` addition in a class definition, preventing object creation outside the class. A factory method, often a static method, then provides users with an instance of the class. The singleton pattern ensures only one instance per class within an internal session.  
- Example of a predefined ABAP class: `CL_IXML_CORE`.  

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

 - This example demonstrates the singleton design pattern with the following declarations and implementations:
  - Global class:
    - Implements the `if_oo_adt_classrun` interface and calls methods from a local class.
    - Acts as a consumer of an API (a local class) defined in the CCIMP include.
  - CCIMP include (Local Types tab in ADT):
    - Contains the local class `lcl_singleton`, which provides the static factory method `get_obj` that supplies consumers with an instance (a single instance in this case) of the `lcl_singleton` class.
    - Specifies the `CREATE PRIVATE` addition to prevent instance creation outside the class.
    - The `get_obj` method checks for an existing instance of the `lcl_singleton` class. If it exists, it returns that instance; otherwise, it creates and returns a new instance. A private static attribute stores the reference to this instance.
    - To demonstrate the singleton pattern, an internal table, which represents a log table, is filled when calling the `add_log` method and providing some text. The content of the table is returned when the consumer (the example global class) calls the `get_log` method. The `get_obj` is called several times, i.e. multiple object reference variables were assigned the reference to the single instance of the class. Yet, the table content returned at the end of the example shows the same content for all method calls via different object reference variables. It shows consistent data across object reference variables (i.e. the table has not been filled anew etc.). This way, a consistent logging is ensured within an internal session, throughout class execution.
    - Furthermore, the class implements both static and instance constructors. These constructors log time stamps and set static and instance attributes. The table that stores object reference variable names and time stamp values (created in the global class) and that is output to the console shows that the time stamps have not changed throughout the class execution, especially the time stamp set when calling the instance constructor, which is indeed called only once. 

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Internal table to store and display object reference variable names and
    "time stamp values
    TYPES: BEGIN OF s_ts,
             name               TYPE string,
             timestamp_static   TYPE utclong,
             timestamp_instance TYPE utclong,
           END OF s_ts.
    DATA ts_tab TYPE TABLE OF s_ts WITH EMPTY KEY.

    "Object creation as follows is not possible
    "DATA(oref) = NEW lcl_singleton( ).

    "Creating object
    DATA(oref1) = lcl_singleton=>get_obj( ).

    "Retrieving time stamps, and adding the values to the internal table created above
    "for display purposes
    oref1->get_timestamps( IMPORTING ts_static   = DATA(ts_static)
                                     ts_instance = DATA(ts_instance) ).
    APPEND VALUE #( name = `oref1` timestamp_static = ts_static timestamp_instance = ts_instance ) TO ts_tab.

    "Adding entries to a log table (represented by a private static attribute in the local class)
    oref1->add_log( |Text 1 added at { utclong_current( ) } (using oref1)| ).
    oref1->add_log( |Text 2 added at { utclong_current( ) } (using oref1)| ).

    "Creating more objects (however, the one created previously is returned) and adding entries
    "to the log table
    "Time stamp values are also added the to the internal table for display purposes
    DATA(oref2) = lcl_singleton=>get_obj( ).

    oref2->get_timestamps( IMPORTING ts_static   = ts_static
                                     ts_instance = ts_instance ).
    APPEND VALUE #( name = `oref2` timestamp_static = ts_static timestamp_instance = ts_instance ) TO ts_tab.

    oref2->add_log( |Text 3 added at { utclong_current( ) } (using oref2)| ).
    oref2->add_log( |Text 4 added at { utclong_current( ) } (using oref2)| ).

    oref1->add_log( |Text 5 added at { utclong_current( ) } (using oref1)| ).

    DATA(oref3) = lcl_singleton=>get_obj( ).

    oref3->get_timestamps( IMPORTING ts_static   = ts_static
                                     ts_instance = ts_instance ).
    APPEND VALUE #( name = `oref3` timestamp_static = ts_static timestamp_instance = ts_instance ) TO ts_tab.

    oref3->add_log( |Text 6 added at { utclong_current( ) } (using oref3)| ).
    oref3->add_log( |Text 7 added at { utclong_current( ) } (using oref3)| ).

    oref1->add_log( |Text 8 added at { utclong_current( ) } (using oref1)| ).
    oref2->add_log( |Text 9 added at { utclong_current( ) } (using oref2)| ).
    oref3->add_log( |Text 10 added at { utclong_current( ) } (using oref3)| ).

    DATA(oref4) = lcl_singleton=>get_obj( ).

    oref4->get_timestamps( IMPORTING ts_static   = ts_static
                                     ts_instance = ts_instance ).
    APPEND VALUE #( name = `oref4` timestamp_static = ts_static timestamp_instance = ts_instance ) TO ts_tab.

    oref4->add_log( |Text 11 added at { utclong_current( ) } (using oref4)| ).
    oref4->add_log( |Text 12 added at { utclong_current( ) } (using oref4)| ).

    "Retrieving the content of the log table per object
    "However, as it is one and the same object that is dealt with, the content is the same.
    DATA(log1) = oref1->get_log( ).
    DATA(log2) = oref2->get_log( ).
    DATA(log3) = oref3->get_log( ).

    out->write( log1 ).
    out->write( |\n| ).

    ASSERT log1 = log2.
    ASSERT log1 = log3.

    "Displaying the time stamps visualizing the singleton pattern
    SORT ts_tab BY name ASCENDING.
    out->write( ts_tab ).

  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
CLASS lcl_singleton DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_obj RETURNING VALUE(obj) TYPE REF TO lcl_singleton.

    METHODS add_log IMPORTING text TYPE string.
    METHODS get_log RETURNING VALUE(log) TYPE string_table.
    METHODS get_timestamps EXPORTING ts_static   TYPE utclong
                                     ts_instance TYPE utclong.

    CLASS-METHODS class_constructor.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA oref TYPE REF TO lcl_singleton.
    CLASS-DATA log_table TYPE string_table.
    CLASS-DATA timestamp_static TYPE utclong.
    DATA timestamp_instance TYPE utclong.
ENDCLASS.

CLASS lcl_singleton IMPLEMENTATION.
  METHOD get_obj.
    IF oref IS NOT BOUND.
      oref = NEW lcl_singleton( ).
    ENDIF.

    obj = oref.
  ENDMETHOD.

  METHOD add_log.
    INSERT text INTO TABLE log_table.
  ENDMETHOD.

  METHOD get_log.
    log = log_table.
  ENDMETHOD.

  METHOD get_timestamps.
    ts_static = timestamp_static.
    ts_instance = timestamp_instance.
  ENDMETHOD.

  METHOD class_constructor.
    timestamp_static = utclong_current( ).
  ENDMETHOD.

  METHOD constructor.
    timestamp_instance = utclong_current( ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## State

- The state of an object can be considered a specific situation that affects its behavior. So, an object's behavior depends on its current state.
- The state design pattern allows an object of a class to have different states, enabling it to change its behavior dynamically during runtime based on its current state.
- A potential setup, illustrated in the following example, may look like this:
  - A class is defined whose objects' behavior changes. This class may manage the current state by storing a reference to the current state object as an attribute. Based on the current state, it forwards state-specific behavior calls to this object. 
  - An interface is defined that declares methods whose concrete implementations represent behaviors related to different states.
  - Concrete state classes implement the interface, including code for the distinct behaviors for each state in the interface methods.
  - The actual state transitions, which enable the execution of different functionalities, may be implemented in various ways. The following example includes two approaches:
    - Delegating the task to the client, which is the global class in the example
    - Implementing state transitions within the class whose objects' behavior changes based on external input
- The design pattern can be useful when you want to organize state-specific functionality in dedicated classes, enhancing maintenance and extensibility while reducing class complexity, aimed at not overcrowding classes with extensive code. It can also help avoid complex and repeated conditional logic.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- The example demonstrates the state design pattern through the following declarations and implementations. Note that this example is oversimplified, and multiple class setup strategies may apply. It uses a mathematical context to allow objects of the `lcl_math` and `lcl_math_cond` classes to perform mathematical operations. The object's behavior changes dynamically based on its current state or a condition.
- Global class:
  - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
  - Serves as the client for demonstrating the design pattern. The declarations and implementations in the *CCIMP include* are relevant for conceptual considerations.  
- CCIMP include (Local Types tab in ADT):  
  - Interface `lif_calc`: Defines the shared interface for concrete state classes, including the `calculate` method for performing arithmetic operations. Each state class implements this method differently. 
  - Concrete state classes `lcl_state_addition`, `lcl_state_subtraction`, and `lcl_state_multiplication`: These classes implement the `lif_calc` interface and perform specific and simple calculations.
  - `lcl_math`:
    - Represents the class whose objects' behavior changes.
    - Declares private attributes, including the `state_ref` reference variable that holds the current object state, referencing an object that implements `lif_calc`.
    - The `set_state` method updates the current state in `state_ref`. To work with the latest calculation result, the resulting values are assigned accordingly.
    - The `execute` method calls the `calculate` method of the current state (if the reference variable is bound) and returns the result.
  - The `lcl_math_cond` class is similar to `lcl_math`, but it handles state transitions through conditional logic, and includes the object creation in the class itself.

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

    "In Example 1, the client transitions the states of the objects by
    "passing instances of the state classes to the set_state method.
    "The example begins with the creation of an object of the lcl_math
    "class. Its constructor expects an integer value, which serves as the
    "first operand for the calculation. The example method calls illustrate
    "several state transitions. When executing the execute method, the
    "second operand is provided as an argument, and the calculation is
    "performed based on the current state.

    DATA(oref) = NEW lcl_math( 15 ).

    "Performing additions by transitioning the state
    DATA(oref_add) = NEW lcl_state_addition( ).

    oref->set_state( oref_add ).
    DATA(result) = oref->execute( 5 ).
    out->write( result ).

    result = oref->execute( 10 ).
    out->write( result ).

    result = oref->execute( 40 ).
    out->write( result ).

    "Performing subtractions by transitioning the state
    DATA(oref_sub) = NEW lcl_state_subtraction( ).

    oref->set_state( oref_sub ).
    result = oref->execute( 10 ).
    out->write( result ).

    result = oref->execute( 80 ).
    out->write( result ).

    "Transitioning the state back to perform an addition
    oref->set_state( oref_add ).

    result = oref->execute( 25 ).
    out->write( result ).

    "Performing multiplications by transitioning the state
    DATA(oref_mul) = NEW lcl_state_multiplication( ).

    oref->set_state( oref_mul ).
    result = oref->execute( 5 ).
    out->write( result ).

    result = oref->execute( 4 ).
    out->write( result ).

    "Transitioning the state back to perform a subtraction
    oref->set_state( oref_sub ).
    result = oref->execute( 99 ).
    out->write( result ).

    out->write( |\n{ repeat( val = `*` occ = 70 ) }\n\n| ).

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

    "Example 2 is similar to Example 1, and yields the same results. Here,
    "the state transitions are performed by the class whose objects' behavior
    "changes, based on external input. In this case, the input is realized by
    "passing an enumeration type, which is evaluated in the class.

    DATA(oref_cond) = NEW lcl_math_cond( 15 ).

    oref_cond->set_state( lcl_math_cond=>plus ).

    result = oref_cond->execute( 5 ).
    out->write( result ).

    result = oref_cond->execute( 10 ).
    out->write( result ).

    result = oref_cond->execute( 40 ).
    out->write( result ).

    oref_cond->set_state( lcl_math_cond=>minus ).

    result = oref_cond->execute( 10 ).
    out->write( result ).

    result = oref_cond->execute( 80 ).
    out->write( result ).

    oref_cond->set_state( lcl_math_cond=>plus ).

    result = oref_cond->execute( 25 ).
    out->write( result ).

    oref_cond->set_state( lcl_math_cond=>multiply ).

    result = oref_cond->execute( 5 ).
    out->write( result ).

    result = oref_cond->execute( 4 ).
    out->write( result ).

    oref_cond->set_state( lcl_math_cond=>minus ).

    result = oref_cond->execute( 99 ).
    out->write( result ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& State interface
*&---------------------------------------------------------------------*

INTERFACE lif_calc.
  DATA number TYPE i.
  METHODS calculate IMPORTING value         TYPE i
                    RETURNING VALUE(result) TYPE i.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Concrete state classes
*&---------------------------------------------------------------------*

CLASS lcl_state_addition DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calc.
ENDCLASS.

CLASS lcl_state_addition IMPLEMENTATION.
  METHOD lif_calc~calculate.
    result = lif_calc~number + value.
    lif_calc~number = result.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_state_subtraction DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calc.
ENDCLASS.

CLASS lcl_state_subtraction IMPLEMENTATION.
  METHOD lif_calc~calculate.
    result = lif_calc~number - value.
    lif_calc~number = result.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_state_multiplication DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calc.
ENDCLASS.

CLASS lcl_state_multiplication IMPLEMENTATION.
  METHOD lif_calc~calculate.
    result = lif_calc~number * value.
    lif_calc~number = result.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class whose objects' behavior changes (Example 1)
*&---------------------------------------------------------------------*

CLASS lcl_math DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING start_num TYPE i,
      set_state IMPORTING state TYPE REF TO lif_calc,
      execute IMPORTING value TYPE i RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    DATA: num       TYPE i,
          state_ref TYPE REF TO lif_calc.
ENDCLASS.

CLASS lcl_math IMPLEMENTATION.
  METHOD constructor.
    num = start_num.
  ENDMETHOD.

  METHOD set_state.
    num = COND #( WHEN state_ref IS BOUND THEN state_ref->number ELSE num ).
    state_ref = state.
    state_ref->number = num.
  ENDMETHOD.

  METHOD execute.
    IF state_ref IS BOUND.
      result = state_ref->calculate( value ).
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class whose objects' behavior changes (Example 2)
*&---------------------------------------------------------------------*

CLASS lcl_math_cond DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM calc_operation,
             plus,
             minus,
             multiply,
           END OF ENUM calc_operation.
    METHODS:
      constructor IMPORTING start_num TYPE i,
      set_state IMPORTING calc_operation TYPE lcl_math_cond=>calc_operation,
      execute IMPORTING value TYPE i RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    DATA num TYPE i.
    DATA state_ref TYPE REF TO lif_calc.
ENDCLASS.

CLASS lcl_math_cond IMPLEMENTATION.
  METHOD constructor.
    num = start_num.
  ENDMETHOD.

  METHOD set_state.
    num = COND #( WHEN state_ref IS BOUND THEN state_ref->number ELSE num ).

    state_ref = COND #( WHEN calc_operation = plus AND state_ref IS NOT INSTANCE OF lcl_state_addition THEN NEW lcl_state_addition( )
                        WHEN calc_operation = minus AND state_ref IS NOT INSTANCE OF lcl_state_subtraction THEN NEW lcl_state_subtraction( )
                        WHEN calc_operation = multiply AND state_ref IS NOT INSTANCE OF lcl_state_multiplication THEN NEW lcl_state_multiplication( ) ).

    state_ref->number = num.
  ENDMETHOD.

  METHOD execute.
    IF state_ref IS BOUND.
      result = state_ref->calculate( value ).
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Strategy

- Assume you have a class and want to incorporate a set of related functionalities. Instead of overcrowding this class with multiple methods, the strategy design pattern allows you to put these functionalities in separate classes. Users can flexibly select and execute the functionality at runtime without modifying the base class code.
- To implement this, a strategy design pattern setup might look like this:  
  - A strategy interface defines a method that all concrete strategy classes must implement.  
  - Concrete strategy classes implement the strategy interface and provide specific functionalities.  
  - A base class maintains a reference to the strategy. Based on the strategy chosen at runtime, it delegates the execution of functionality accordingly, allowing users to interchange strategies seamlessly.
- This pattern enables flexibility in extending and maintaining functionality without requiring code modifications in the base class. It allows for the addition of more concrete strategy classes to enhance capabilities easily.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- The example code includes two examples demonstrating the observer design pattern with the following declarations and implementations:
  - Global class:
    - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
    - Serves as a vehicle for demonstrating the design pattern. The declarations and implementations in the `CCIMP` are relevant for the for conceptual considerations.
  - CCIMP include (Local Types tab in ADT):
    - The example uses a simplified calculator context (simplified also in the sense, for example, that no proper error handling is implemented). The calculator performs various arithmetic operations, representing familiar functionalities. These operations are delegated to specific strategy classes.
    - Two example setups are demonstrated: using an interface and an abstract class.
    - Example setup with an interface:
      - Interface `lif_calculation`: Defines the shared interface for concrete strategy classes, including the `calculate` method.
      - Concrete strategy classes (`lcl_addition`, `lcl_subtraction`, `lcl_multiplication`, `lcl_division`) implement the `lif_calculation` interface and provide specific calculation strategies.
      - The base class `lcl_calculator` has two methods:
        - `set_strategy_ref`: Sets a reference to a strategy. It expects an object that implements the `lif_calculation` interface.
        - `calculate`: Delegates execution to the concrete strategy class. This setup allows for flexible delegation at runtime to any concrete strategy class that implements the shared interface.
    - Example setup with an abstract class:
      - Abstract class `lcl_strategy_abstract`: Similar to the `lif_calculation` interface, it defines the shared interface that concrete strategy classes use.
      - Concrete strategy classes inherit from the abstract superclass and implement the abstract method.
    - The user, represented by the global class implementation, manages object creation, sets the strategy by calling the `set_strategy_ref` method, and passes appropriate objects. When the `calculate` method is called, the corresponding functionality is executed based on the previously passed object.

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------*
*& 1) Example setup with an interface
*&---------------------------------------------------------------------*

    out->write( |1) Example setup with an interface\n| ).

    DATA(calc) = NEW lcl_calculator( ).

    calc->set_strategy_ref( NEW lcl_addition( ) ).
    DATA(result) = calc->calculate( num1 = 7 num2 = 4 ).
    out->write( |7 + 4 = { result }| ).

    calc->set_strategy_ref( NEW lcl_subtraction( ) ).
    result = calc->calculate( num1 = 25 num2 = 15 ).
    out->write( |25 - 15 = { result }| ).

    calc->set_strategy_ref( NEW lcl_multiplication( ) ).
    result = calc->calculate( num1 = 8 num2 = 4 ).
    out->write( |8 * 4 = { result }| ).

    calc->set_strategy_ref( NEW lcl_division( ) ).
    result = calc->calculate( num1 = 36 num2 = 6 ).
    out->write( |36 / 6 = { result }| ).

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

*&---------------------------------------------------------------------*
*& 2) Example setup with an abstract class
*&---------------------------------------------------------------------*

    out->write( |2) Example setup with an abstract class\n| ).

    DATA(calc_abs) = NEW lcl_calc( ).

    calc_abs->set_strategy_ref( NEW lcl_add( ) ).
    result = calc_abs->calculate( num1 = 12 num2 = 8 ).
    out->write( |12 + 8 = { result }| ).

    calc_abs->set_strategy_ref( NEW lcl_sub( ) ).
    result = calc_abs->calculate( num1 = 5 num2 = 10 ).
    out->write( |5 - 10 = { result }| ).

    calc_abs->set_strategy_ref( NEW lcl_mult( ) ).
    result = calc_abs->calculate( num1 = 10 num2 = 10 ).
    out->write( |10 * 10 = { result }| ).

    calc_abs->set_strategy_ref( NEW lcl_div( ) ).
    result = calc_abs->calculate( num1 = 81 num2 = 9 ).
    out->write( |81 / 9 = { result }| ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& 1) Example setup with an interface
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Interface
*&---------------------------------------------------------------------*

INTERFACE lif_calculation.
  METHODS calculate
    IMPORTING num1          TYPE i
              num2          TYPE i
    RETURNING VALUE(result) TYPE i.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Concrete strategy classes
*&---------------------------------------------------------------------*

CLASS lcl_addition DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calculation.
ENDCLASS.

CLASS lcl_addition IMPLEMENTATION.
  METHOD lif_calculation~calculate.
    result = num1 + num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_subtraction DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calculation.
ENDCLASS.

CLASS lcl_subtraction IMPLEMENTATION.
  METHOD lif_calculation~calculate.
    result = num1 - num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_multiplication DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calculation.
ENDCLASS.

CLASS lcl_multiplication IMPLEMENTATION.
  METHOD lif_calculation~calculate.
    result = num1 * num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_division DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calculation.
ENDCLASS.

CLASS lcl_division IMPLEMENTATION.
  METHOD lif_calculation~calculate.
    IF num1 <> 0 AND num2 = 0.
      result = 0.
    ELSE.
      result = num1 / num2.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Base class that delegates the execution of functionality
*&---------------------------------------------------------------------*

CLASS lcl_calculator DEFINITION.
  PUBLIC SECTION.
    METHODS:
      set_strategy_ref IMPORTING strat_ref TYPE REF TO lif_calculation,
      calculate IMPORTING num1          TYPE i
                          num2          TYPE i
                RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    DATA strategy TYPE REF TO lif_calculation.
ENDCLASS.

CLASS lcl_calculator IMPLEMENTATION.
  METHOD set_strategy_ref.
    me->strategy = strat_ref.
  ENDMETHOD.

  METHOD calculate.
    IF strategy IS BOUND.
      result = strategy->calculate( num1 = num1 num2 = num2 ).
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************

*&---------------------------------------------------------------------*
*& 2) Example setup with an abstract class
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Abstract class
*&---------------------------------------------------------------------*

CLASS lcl_strategy_abstract DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS calculate ABSTRACT
      IMPORTING
        num1          TYPE i
        num2          TYPE i
      RETURNING
        VALUE(result) TYPE i.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete strategy classes
*&---------------------------------------------------------------------*

CLASS lcl_add DEFINITION INHERITING FROM lcl_strategy_abstract FINAL.
  PUBLIC SECTION.
    METHODS calculate REDEFINITION.
ENDCLASS.

CLASS lcl_add IMPLEMENTATION.
  METHOD calculate.
    result = num1 + num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sub DEFINITION INHERITING FROM lcl_strategy_abstract FINAL.
  PUBLIC SECTION.
    METHODS calculate REDEFINITION.
ENDCLASS.

CLASS lcl_sub IMPLEMENTATION.
  METHOD calculate.
    result = num1 - num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_mult DEFINITION INHERITING FROM lcl_strategy_abstract FINAL.
  PUBLIC SECTION.
    METHODS calculate REDEFINITION.
ENDCLASS.

CLASS lcl_mult IMPLEMENTATION.
  METHOD calculate.
    result = num1 * num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_div DEFINITION INHERITING FROM lcl_strategy_abstract FINAL.
  PUBLIC SECTION.
    METHODS calculate REDEFINITION.
ENDCLASS.

CLASS lcl_div IMPLEMENTATION.
  METHOD calculate.
    IF num1 <> 0 AND num2 = 0.
      result = 0.
    ELSE.
      result = num1 / num2.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Base class that delegates the execution of functionality
*&---------------------------------------------------------------------*

CLASS lcl_calc DEFINITION.
  PUBLIC SECTION.
    METHODS:
      set_strategy_ref IMPORTING strat_ref TYPE REF TO lcl_strategy_abstract,
      calculate IMPORTING num1          TYPE i
                          num2          TYPE i
                RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    DATA strategy TYPE REF TO lcl_strategy_abstract.
ENDCLASS.

CLASS lcl_calc IMPLEMENTATION.
  METHOD set_strategy_ref.
    strategy = strat_ref.
  ENDMETHOD.

  METHOD calculate.
    IF strategy IS BOUND.
      result = strategy->calculate( num1 = num1 num2 = num2 ).
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Template Method

- The template method design pattern suggests that a template method, typically included in an abstract superclass, defines and outlines the sequence of steps, i.e. method calls, needed to perform certain tasks.  
- Abstract methods can be defined in the abstract superclass, and calls to these methods can be included in the template method. Subclasses that inherit from the abstract superclass provide the concrete implementations of these abstract methods.
- The template method implementation may also include calls to non-abstract methods from the abstract superclass, which are implemented in the superclass. This ensures that all users share the same functionality.
- Thus, the template method pattern allows to organize method calls into a common structure. It enables the standard functionality provided by the superclass while allowing subclasses to flexibly modify specific methods for custom implementations. This can be helpful for use cases that involve similar tasks and require the same sequence of method calls, while still allowing for customized functionality.
- This approach can simplify code maintenance and promote encapsulation. For example, if you need to modify the standard functionality, you only have to adapt the code in the superclass. Similarly, if a specific subclass requires code changes, you can update the abstract method implementations without altering the underlying base structure.
- A potential, high-level class design, which is also used in the example, might look as follows:
	- An abstract class defines a common interface for its subclasses.
	- The abstract class may include:
		- Abstract methods for subclasses to implement.
		- Non-abstract methods, providing standard functionality for all users in the template method. This approach avoids code duplication.
		- A template method to orchestrate a specific sequence of method calls. These method calls may include both abstract methods (implemented in subclasses) and non-abstract methods (implemented in the abstract superclass).
	- Subclasses, representing concrete classes, inherit from the abstract superclass and must implement the abstract methods.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:** 

- The following example demonstrates a consistent flow of steps defined in a template method that is common to variants. It uses an XML and JSON parsing context. The steps in the template method are as follows:
	- The XML and JSON data is provided as a string and converted to an xstring.
	- The data is then parsed.
	- The parsed data is output.
- The example illustrates the template method design pattern with the following declarations and implementations:
  - Global class:
    - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
    - Serves as a vehicle for demonstrating the design pattern. The declarations and implementations in the CCIMP include are relevant for the conceptual considerations.
    - Represents the user who expects XML and JSON data to be parsed.
  - CCIMP include (Local Types tab in ADT):    
    - Abstract class `lcl_data_parser` 
		- Serves as the common interface for all subclasses that inherit from this class.
		- Includes protected non-abstract methods (`convert_to_xstring`, `output_data`) that are implemented in the superclass.
		- Contains the abstract method `parse`, requiring subclasses to provide their own implementation.
		- Includes the public, final template method `template_method`, which specifies the sequence of method calls mentioned earlier.
	- Multiple concrete subclasses inherit from the abstract superclass, each providing custom functionality in the `parse` method:
		- `lcl_xml_parser_ixml`: Reads XML data using the iXML library.
		- `lcl_xml_parser_sxml`: Reads XML data using the sXML library.
		- `lcl_json_parser_sxml`: Reads JSON data using the sXML library. This implementation is similar to `lcl_xml_parser_sxml` and is only included to add more concrete classes.
		- `lcl_json_parser_ui2cljson`: Reads JSON data using the `/ui2/cl_json` class and the `generate` method, which creates an ABAP data object from JSON. It is also only included to add more demo concrete classes. Instead of the typical usage of the class to include JSON data in known (in this case, unknown) data objects, the implementation here explores the created data and dynamic programming techniques. It is meant for exploration purposes and neither represents best practices nor claims to cover all JSON variants (beyond the demo JSON data covered in the global class). 
	- The simplified parsing returns values in a string table for display.
	- Note that the example does not include proper error handling.

      
<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    out->write( |1) Parsing XML using iXML \n\n| ).

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

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

**********************************************************************

    out->write( |2) Parsing XML using sXML \n\n| ).

    DATA(oref_sxml) = NEW lcl_xml_parser_sxml( ).
    oref_sxml->template_method( data = demo_xml out = out ).

    DATA(result_sxml) = oref_sxml->result.

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

**********************************************************************

    out->write( |3) Parsing JSON using sXML \n\n| ).

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

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).

**********************************************************************

    out->write( |4) Parsing JSON using /ui2/cl_json \n\n| ).

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

      out->write( |\n{ repeat( val = `*` occ = 75 ) }\n\n| ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
*&---------------------------------------------------------------------*
*& Abstract class
*&---------------------------------------------------------------------*
"- Serves as the common interface for all subclasses that inherit from this class.
"- Includes protected non-abstract methods ('convert_to_xstring', 'output_data')
"  that are implemented in the superclass.
"- Contains the abstract method 'parse', requiring subclasses to provide their own
"  implementation.
"- Includes the public, final template method 'template_method', which specifies
"  the sequence of method calls.

CLASS lcl_data_parser DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS template_method FINAL IMPORTING data TYPE string
                                            out  TYPE REF TO if_oo_adt_classrun_out OPTIONAL.
    DATA result TYPE string_table.
  PROTECTED SECTION.
    METHODS: parse ABSTRACT IMPORTING data          TYPE xstring
                            RETURNING VALUE(result) TYPE string_table,
      convert_to_xstring IMPORTING data        TYPE string
                         RETURNING VALUE(xstr) TYPE xstring,
      output_data IMPORTING data TYPE string_table
                            out  TYPE REF TO if_oo_adt_classrun_out OPTIONAL.
ENDCLASS.

CLASS lcl_data_parser IMPLEMENTATION.

  METHOD convert_to_xstring.
    TRY.
        xstr = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( data ).
      CATCH cx_sy_conversion_codepage.
        "If the conversion does not work, the program flow is interrupted.
        "The example does not include a proper error handling and raise a runtime error in case of a failure at this stage.
        "The example is designed for the demo input data.
        ASSERT 1 = 2.
    ENDTRY.
  ENDMETHOD.

  METHOD output_data.
    IF out IS SUPPLIED AND out IS BOUND.
      out->write( data ).
    ENDIF.
    "As an alternative to the output, the result is assigned
    "to a public instance attribute, for example, if the reference
    "variable cannot be supplied and so as to still be able to check
    "out the parsing result.
    result = data.
  ENDMETHOD.

  METHOD template_method.
    "Predefined steps in the template method:
    "- The XML and JSON data is provided as a string and converted to an xstring.
    "- The data is then parsed.
    "- The parsed data is output.

    DATA(xstr) = convert_to_xstring( data ).

    DATA(output) = parse( xstr ).

    IF out IS SUPPLIED AND out IS BOUND.
      output_data( data = output
                   out  = out ).
    ELSE.
      output_data( data = output ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete class 1 (XML parsing using iXML)
*&---------------------------------------------------------------------*

CLASS lcl_xml_parser_ixml DEFINITION INHERITING FROM lcl_data_parser.
  PROTECTED SECTION.
    METHODS parse REDEFINITION.
ENDCLASS.

CLASS lcl_xml_parser_ixml IMPLEMENTATION.
  METHOD parse.
    "For notes on the code example, see the XML and JSON cheat sheet.

    DATA(ixml_pa) = cl_ixml_core=>create( ).
    DATA(stream_factory_pa) = ixml_pa->create_stream_factory( ).
    DATA(document_pa) = ixml_pa->create_document( ).
    DATA(parser_pa) = ixml_pa->create_parser(
                        istream = stream_factory_pa->create_istream_xstring( string = data )
                        document = document_pa
                        stream_factory = stream_factory_pa ).

    DATA(parsing_check) = parser_pa->parse( ).
    IF parsing_check = 0.
      DATA(iterator_pa) = document_pa->create_iterator( ).
      DATA(node_pa) = iterator_pa->get_next( ).
      WHILE NOT node_pa IS INITIAL.
        DATA(node_type) = node_pa->get_type( ).
        CASE node_type.
          WHEN if_ixml_node=>co_node_element.
            APPEND |Element: "{ node_pa->get_name( ) }"| TO result.
            DATA(attributes_pa) = node_pa->get_attributes( ).
            IF NOT attributes_pa IS INITIAL.
              DO attributes_pa->get_length( ) TIMES.
                DATA(attr) = attributes_pa->get_item( sy-index - 1 ).
                APPEND |Attribute: "{ attr->get_name( ) } = { attr->get_value( ) }"| TO result.
              ENDDO.
            ENDIF.
          WHEN if_ixml_node=>co_node_text OR if_ixml_node=>co_node_cdata_section.
            APPEND |Text: "{ node_pa->get_value( ) }"| TO result.
            DATA(val) = to_upper( node_pa->get_value( ) ).
            node_pa->set_value( val ).
        ENDCASE.
        node_pa = iterator_pa->get_next( ).
      ENDWHILE.
    ELSE.
      "Parsing was not successful.
      "The example does not include a proper error handling.
      APPEND `--- Error ---` TO result.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete class 2 (XML parsing using sXML)
*&---------------------------------------------------------------------*

CLASS lcl_xml_parser_sxml DEFINITION INHERITING FROM lcl_data_parser.
  PROTECTED SECTION.
    METHODS parse REDEFINITION.
ENDCLASS.

CLASS lcl_xml_parser_sxml IMPLEMENTATION.
  METHOD parse.
    "For notes on the code, see the XML and JSON cheat sheet

    DATA(reader) = cl_sxml_string_reader=>create( data ).

    TRY.
        DO.
          reader->next_node( ).

          IF reader->node_type = if_sxml_node=>co_nt_final.
            EXIT.
          ENDIF.

          DATA(node_type) = SWITCH #( reader->node_type WHEN if_sxml_node=>co_nt_initial THEN `CO_NT_INITIAL`
                                                        WHEN if_sxml_node=>co_nt_element_open THEN `CO_NT_ELEMENT_OPEN`
                                                        WHEN if_sxml_node=>co_nt_element_close THEN `CO_NT_ELEMENT_CLOSE`
                                                        WHEN if_sxml_node=>co_nt_value THEN `CO_NT_VALUE`
                                                        WHEN if_sxml_node=>co_nt_attribute THEN `CO_NT_ATTRIBUTE`
                                                        ELSE `Error` ).

          DATA(name) = reader->name.
          DATA(value) = COND #( WHEN reader->node_type = if_sxml_node=>co_nt_value THEN reader->value ).
          APPEND |{ node_type }, { name }{ COND #( WHEN value IS NOT INITIAL THEN |, { value }| ) }| TO result.

          IF reader->node_type = if_sxml_node=>co_nt_element_open.
            DO.
              reader->next_attribute( ).
              IF reader->node_type <> if_sxml_node=>co_nt_attribute.
                EXIT.
              ENDIF.
              APPEND |CO_NT_ATTRIBUTE, { reader->name }{ COND #( WHEN reader->value IS NOT INITIAL THEN |, { reader->value }| ) }| TO result.
            ENDDO.
          ENDIF.
        ENDDO.
      CATCH cx_sxml_state_error INTO DATA(error).
        "The example does not include a proper error handling.
        APPEND |--- Error: { error->get_text( ) } ---| TO result.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete class 3 (JSON parsing using sXML)
*&---------------------------------------------------------------------*

"This class demonstrates the same code as lcl_xml_parser_sxml. It is
"included to add more example classes.

CLASS lcl_json_parser_sxml DEFINITION INHERITING FROM lcl_data_parser.
  PROTECTED SECTION.
    METHODS parse REDEFINITION.
ENDCLASS.

CLASS lcl_json_parser_sxml IMPLEMENTATION.
  METHOD parse.
    DATA(reader) = cl_sxml_string_reader=>create( data ).

    TRY.
        DO.
          reader->next_node( ).

          IF reader->node_type = if_sxml_node=>co_nt_final.
            EXIT.
          ENDIF.

          DATA(node_type) = SWITCH #( reader->node_type WHEN if_sxml_node=>co_nt_initial THEN `CO_NT_INITIAL`
                                                        WHEN if_sxml_node=>co_nt_element_open THEN `CO_NT_ELEMENT_OPEN`
                                                        WHEN if_sxml_node=>co_nt_element_close THEN `CO_NT_ELEMENT_CLOSE`
                                                        WHEN if_sxml_node=>co_nt_value THEN `CO_NT_VALUE`
                                                        WHEN if_sxml_node=>co_nt_attribute THEN `CO_NT_ATTRIBUTE`
                                                        ELSE `Error` ).

          DATA(name) = reader->name.
          DATA(value) = COND #( WHEN reader->node_type = if_sxml_node=>co_nt_value THEN reader->value ).
          APPEND |{ node_type }, { name }{ COND #( WHEN value IS NOT INITIAL THEN |, { value }| ) }| TO result.

          IF reader->node_type = if_sxml_node=>co_nt_element_open.
            DO.
              reader->next_attribute( ).
              IF reader->node_type <> if_sxml_node=>co_nt_attribute.
                EXIT.
              ENDIF.
              APPEND |CO_NT_ATTRIBUTE, { reader->name }{ COND #( WHEN reader->value IS NOT INITIAL THEN |, { reader->value }| ) }| TO result.
            ENDDO.
          ENDIF.
        ENDDO.
      CATCH cx_sxml_state_error INTO DATA(error).
        "The example does not include a proper error handling.
        APPEND |--- Error: { error->get_text( ) } ---| TO result.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete class 4 (JSON parsing using /ui2/cl_json)
*&---------------------------------------------------------------------*

"This class reads JSON data using the '/ui2/cl_json' class and the 'generate'
"method, which creates an ABAP data object from JSON. It is only included to
"add more demo concrete classes.
"Instead of the typical usage of the class to include JSON data in known (in
"this case, unknown) data objects, the implementation here explores the created
"data and experiments with dynamic programming techniques. It is a nonsensical
"example that just explores and experiments. It neither represents best practices
"nor claims to cover all JSON variants (beyond the demo JSON data covered in the
"global class). Uncaught exceptions may occur. It is particulary not meant to
"reinvent the wheel, especially with the availability of the sXML library for the
"purpose. Like the other classes, the example just adds items to a string table.

CLASS lcl_json_parser_ui2cljson DEFINITION INHERITING FROM lcl_data_parser.
  PROTECTED SECTION.
    METHODS parse REDEFINITION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_node,
             node_name     TYPE string,
             ref           TYPE REF TO data,
             is_nested     TYPE abap_boolean,
             nesting_level TYPE i,
             value         TYPE string,
             done          TYPE abap_boolean,
             kind          TYPE REF TO cl_abap_typedescr,
           END OF ty_node,
           ty_nodes TYPE TABLE OF ty_node WITH EMPTY KEY.

    METHODS:
      process_elementary IMPORTING data TYPE REF TO data
                         CHANGING  node TYPE ty_node,
      process_structure IMPORTING data          TYPE REF TO data
                                  is_nested     TYPE abap_boolean DEFAULT abap_false
                                  nesting_level TYPE i DEFAULT 0,
      process_table IMPORTING data          TYPE REF TO data
                              name          TYPE string OPTIONAL
                              is_nested     TYPE abap_boolean DEFAULT abap_false
                              nesting_level TYPE i DEFAULT 0.

    DATA: json_data  TYPE TABLE OF REF TO data,
          node_table TYPE ty_nodes,
          error      TYPE REF TO cx_root,
          tabix      LIKE sy-tabix.
ENDCLASS.

CLASS lcl_json_parser_ui2cljson IMPLEMENTATION.
  METHOD parse.
    DATA(abap_obj_from_json) = /ui2/cl_json=>generate( jsonx = data ).

    "Checking JSON data and adding the data to an internal table for further processing
    IF ( abap_obj_from_json IS INITIAL OR abap_obj_from_json IS NOT BOUND )
    OR ( abap_obj_from_json->* IS INITIAL ).
      APPEND `--- JSON parsing not possible ---` TO result.
      RETURN.
    ENDIF.

    TRY.
        DATA(tdo_json) = cl_abap_typedescr=>describe_by_data_ref( abap_obj_from_json ).

        CASE TYPE OF tdo_json.
          WHEN TYPE cl_abap_tabledescr.
            LOOP AT abap_obj_from_json->* ASSIGNING FIELD-SYMBOL(<json>).
              IF <json> IS BOUND.
                IF <json>->* IS INITIAL.
                  APPEND `--- INITIAL ---` TO result.
                ELSE.
                  APPEND <json> TO json_data.
                ENDIF.
              ELSE.
                APPEND `--- JSON parsing not possible ---` TO result.
              ENDIF.
            ENDLOOP.
          WHEN TYPE cl_abap_structdescr.
            LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( abap_obj_from_json ) )->components INTO DATA(comp_json).
              ASSIGN abap_obj_from_json->(comp_json-name)->* TO FIELD-SYMBOL(<co>).
              APPEND REF #( <co> ) TO json_data.
            ENDLOOP.
          WHEN OTHERS.
            APPEND |--- Not supported by the example implementation ---| TO result.
        ENDCASE.
      CATCH cx_root INTO error .
        APPEND |--- Error: { error->get_text( ) } ---| TO result.
    ENDTRY.

    "Processing JSON data in the node table
    LOOP AT json_data ASSIGNING FIELD-SYMBOL(<data>).
      DATA(tdo) = cl_abap_typedescr=>describe_by_data_ref( <data> ).
      CASE TYPE OF tdo.
        WHEN TYPE cl_abap_structdescr.
          process_structure( <data> ).
        WHEN TYPE  cl_abap_elemdescr.
          APPEND VALUE #( ref = <data> ) TO node_table REFERENCE INTO DATA(elem_ref).

          process_elementary( EXPORTING data = <data>
                              CHANGING  node = elem_ref->* ).
        WHEN OTHERS.
          APPEND |--- Not supported by the example implementation ---| TO result.
          RETURN.
      ENDCASE.
    ENDLOOP.

    "Processing nested nodes
    WHILE line_exists( node_table[ done = abap_false ] ).
      LOOP AT node_table ASSIGNING FIELD-SYMBOL(<node>) WHERE done = abap_false.
        tabix = sy-tabix.
        <node>-done = abap_true.

        IF <node>-kind IS BOUND.
          CASE TYPE OF <node>-kind.
            WHEN TYPE cl_abap_elemdescr.
              process_elementary( EXPORTING data = <node>-ref CHANGING node = <node> ).
            WHEN TYPE cl_abap_structdescr.
              process_structure( data = <node>-ref is_nested = <node>-is_nested nesting_level = <node>-nesting_level ).
            WHEN TYPE cl_abap_tabledescr.
              process_table( data = <node>-ref is_nested = <node>-is_nested nesting_level = <node>-nesting_level name = <node>-node_name ).
          ENDCASE.
        ELSE.
          IF <node>-ref IS INITIAL.
            <node>-value = `%%%NULL%%%`.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

    "Putting the collected values into a string table
    IF node_table IS NOT INITIAL.
      result = VALUE #( BASE result FOR wa IN node_table
        ( |{ repeat( val = ` ` occ = wa-nesting_level ) }{ COND #( WHEN wa-node_name IS NOT INITIAL THEN |{ wa-node_name } | ) }{ wa-value }| ) ).
    ENDIF.
  ENDMETHOD.

  METHOD process_elementary.
    node-value = |{ data->* }|.
  ENDMETHOD.

  METHOD process_structure.
    LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( data ) )->components INTO DATA(comp).
      ASSIGN data->(comp-name) TO FIELD-SYMBOL(<comp>).

      IF <comp> IS NOT INITIAL.
        DATA(refdescr) = cl_abap_refdescr=>describe_by_data( <comp>->* ).
      ELSE.
        CLEAR refdescr.
      ENDIF.

      IF is_nested = abap_false.
        APPEND VALUE #( node_name = comp-name
                        ref = <comp>
                        is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                        nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                        kind = refdescr
                       ) TO node_table.
      ELSE.
        tabix += 1.

        INSERT VALUE #( node_name = comp-name
                       ref           = <comp>
                       is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                       nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                       kind = refdescr
                      ) INTO node_table INDEX tabix.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD process_table.
    LOOP AT data->* ASSIGNING FIELD-SYMBOL(<t>).
      DATA(outer_index) = sy-tabix.

      TRY.
          LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( <t> ) )->components INTO DATA(comp).
            DATA(idx) = sy-tabix.

            ASSIGN <t>->(comp-name) TO FIELD-SYMBOL(<comp>).
            DATA(refdescr) = cl_abap_refdescr=>describe_by_data( <comp>->* ).

            IF is_nested = abap_false.
              IF idx = 1.
                "Inserting a divider for table nodes
                APPEND VALUE #( node_name = |--- { name }-{ outer_index } ---|
                                nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                                done          = abap_true
                              ) TO node_table.
              ENDIF.

              APPEND VALUE #( node_name = comp-name
                              ref           = <comp>
                              is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                                nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                                kind = refdescr
                             ) TO node_table.
            ELSE.
              tabix += 1.

              IF idx = 1.
                "Inserting a divider for table nodes
                INSERT VALUE #( node_name = |--- { name }-{ outer_index } ---|
                                nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                                done          = abap_true
                              ) INTO node_table INDEX tabix.

                tabix += 1.
              ENDIF.

              INSERT VALUE #( node_name = comp-name
                             ref         = <comp>
                             is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                             nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                             kind = refdescr
                            ) INTO node_table INDEX tabix.
            ENDIF.
          ENDLOOP.
        CATCH cx_sy_move_cast_error.
          IF is_nested = abap_false.
            APPEND VALUE #( ref           = <t>
                            is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                            nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                            kind = cl_abap_typedescr=>describe_by_data( <t>->* )
                           ) TO node_table.
          ELSE.
            tabix += 1.

            INSERT VALUE #( ref           = <t>
                           is_nested = COND #( WHEN refdescr IS INSTANCE OF cl_abap_structdescr OR refdescr IS INSTANCE OF cl_abap_tabledescr THEN abap_true )
                           nesting_level = COND #( WHEN nesting_level IS SUPPLIED THEN nesting_level + 1 )
                           kind = cl_abap_typedescr=>describe_by_data( <t>->* )
                          ) INTO node_table INDEX tabix.
          ENDIF.

      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Visitor

- The visitor design pattern allows you to add functionality to objects flexibly without altering the underlying class setup. This pattern enables you to perform various operations on the same object structure without bloating the class definitions.
- The demo example below illustrates this pattern: an object holds data (in this case, database table entries are retrieved and stored in an internal table), while different visitors can process that data through various operations, such as XML or JSON conversion. The class setup eliminates the need to add methods for each new functionality in the underlying class. In the future, if more functionalities need to be added, this approach prevents class bloating that would occur from continually adding methods. The visitor pattern offers the flexibility to extend functionality as needed, allowing users to integrate new capabilities into the object without overcrowding the original class.

<br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

**Example notes:**

- The example code includes two examples demonstrating the observer design pattern with the following declarations and implementations:
  - Global class:
    - Implements the `if_oo_adt_classrun` interface and calls methods from local classes.
    - Serves as a vehicle for demonstrating the design pattern. The declarations and implementations in the `CCIMP` are relevant for the for conceptual considerations.
  - CCIMP include (Local Types tab in ADT):
    - `lif_visitor`: 
      - Defines the shared interface for visitor classes. 
      - A method and attributes are defined that provide a way for different visitor implementations to interact with instances of the `lcl_visited` class.
      - The `visit` method expects an instance of the the `lcl_visited` class and can be processed further by the visitor class for whatever purpose.
      - The instance attributes `result_xstring` and `result_string` are included to store results of the processing in the `visit` method.
    - `lcl_visited`
      - Represents the class to be visited
      - It encapsulates the logic for loading and storing entries of a data source, and and allows visitor classes to operate on it. 
      - Methods:
        - `set_itab_content_ref`: Takes a the name of a data source with an optional number of entries to perform a `SELECT` statement, storing results in the data reference variable `table_data_ref`, a private attribute.
        - `get_itab_content_ref`: Returns a reference to the internal table data.
        - `receive_visitor`: Accepts a visitor object and calls its `visit` method, passing an object of the class itself (`me`) as an argument.
    - Concrete visitor classes 
      - Each visitor class implements the `lif_visitor` interface to process the table data of `lcl_visited` differently. In doing so, the setup allows for extending the capabilities of `lcl_visited` without changing the underlying code.
      - `lcl_visitor_xml`: Implements the conversion of table data into XML format using a `CALL TRANSFORMATION` statement.
      - `lcl_visitor_json`: Implements the serialization of table data into JSON format using `/ui2/cl_json`.
      - `lcl_visitor_count`: Determines the number of lines in the table.
      - `lcl_visitor_cont_str`: Converts the table content into string format.
  - The user, represented by the global class implementation, manages object creation, calls the `receive_visitor` method and passes appropriate objects. Accessing the `lif_visitor~result_xstring` and `lif_visitor~result_string` illustrates the results of the differently implemented operations. 

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class

 </td>

 <td> 

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

*&-------------------------------------------------------------------------------------*
*& 1) Creating an object of the class to be visited, getting data from a data source
*&-------------------------------------------------------------------------------------*

    DATA(oref_lcl) = NEW lcl_visited( ).

    "Getting data from a data source
    "The example uses the name of a view that contains timezone data.
    "Based on the name of the data source, a dynamic SELECT statement is used to
    "retrieve data and store it in an internal table.
    oref_lcl->set_itab_content_ref( data_source       = `I_TIMEZONE`
                                    number_of_entries = 10 ).

*&-------------------------------------------------------------------------------------*
*& 2) Visitor 1 (XML transformation)
*&-------------------------------------------------------------------------------------*

    out->write( |Visitor 1: XML transformation of internal table content\n\n| ).

    "This visitor example includes the creation of an interface reference variable.
    "The other examples use an object reference variable with the visitor class
    "as static type directly. In doing so, the cast can be avoided to access the
    "attribute.
    DATA iref_visitor_xml TYPE REF TO lif_visitor.

    iref_visitor_xml = NEW lcl_visitor_xml( ).

    oref_lcl->receive_visitor( iref_visitor_xml ).

    "Transforming the internal table data to XML
    DATA(result_xml_transformation) = CAST lcl_visitor_xml( iref_visitor_xml )->lif_visitor~result_xstring.

    "Transformation XML -> ABAP for display purposes
    DATA tab_from_xml TYPE TABLE OF i_timezone WITH EMPTY KEY.
    CALL TRANSFORMATION id SOURCE XML result_xml_transformation
                           RESULT itab = tab_from_xml.

    out->write( data = tab_from_xml name = `tab_from_xml` ).

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

*&-------------------------------------------------------------------------------------*
*& 3) Visitor 2 (JSON serialization)
*&-------------------------------------------------------------------------------------*

    out->write( |Visitor 2: JSON serialization\n\n| ).

    DATA(oref_visitor_json) = NEW lcl_visitor_json( ).

    oref_lcl->receive_visitor( oref_visitor_json ).

    "Serializing the internal table data
    DATA(result_json_serialization) = oref_visitor_json->lif_visitor~result_xstring.

    "Deserializing for display purposes
    DATA tab_from_json LIKE tab_from_xml.
    /ui2/cl_json=>deserialize( EXPORTING jsonx = result_json_serialization
                               CHANGING data  = tab_from_json ).

    out->write( data = tab_from_json name = `tab_from_json` ).
    out->write( |\n\n| ).

    "The example implementation also includes the assignment of the
    "interface attribute of type string.
    DATA(result_json_serialization_str) = oref_visitor_json->lif_visitor~result_string.

    out->write( result_json_serialization_str ).

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

*&-------------------------------------------------------------------------------------*
*& 4) Visitor 3 (Counting table entries)
*&-------------------------------------------------------------------------------------*

    out->write( |Visitor 3: Counting the internal table entries\n| ).

    DATA(oref_visitor_count) = NEW lcl_visitor_count( ).

    oref_lcl->receive_visitor( oref_visitor_count ).

    "Getting the table entry count
    DATA(result_count) = oref_visitor_count->lif_visitor~result_string.

    out->write( result_count ).

    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

*&-------------------------------------------------------------------------------------*
*& 5) Visitor 4 (Converting table content into string)
*&-------------------------------------------------------------------------------------*

    out->write( |Visitor 4: Converting the internal table content into a string\n| ).

    DATA(oref_visitor_cont_str) = NEW lcl_visitor_cont_str( ).

    oref_lcl->receive_visitor( oref_visitor_cont_str ).

    "Getting the string holding the internal table content
    DATA(result_cont_str) = oref_visitor_cont_str->lif_visitor~result_string.

    out->write( result_cont_str ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

``` abap
CLASS lcl_visited DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*& Interface
*&---------------------------------------------------------------------*

INTERFACE lif_visitor.
  DATA result_xstring TYPE xstring.
  DATA result_string TYPE string.
  METHODS visit IMPORTING oref TYPE REF TO lcl_visited.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Class to be visited
*&---------------------------------------------------------------------*

CLASS lcl_visited DEFINITION.
  PUBLIC SECTION.
    METHODS: set_itab_content_ref IMPORTING VALUE(data_source) TYPE string
                                            number_of_entries  TYPE i OPTIONAL,
      get_itab_content_ref RETURNING VALUE(content) TYPE REF TO data,
      receive_visitor IMPORTING obj TYPE REF TO lif_visitor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA table_data_ref TYPE REF TO data.
ENDCLASS.

CLASS lcl_visited IMPLEMENTATION.

  METHOD set_itab_content_ref.
    data_source = to_upper( condense( val = data_source to = `` ) ).

    "The example implementation includes a dynamic SELECT statement.
    "The example goes without appropriate checks regarding dynamically
    "inserting content from outside. Refer to the Dynamic Programming
    "ABAP cheat sheet.
    IF number_of_entries IS SUPPLIED.
      SELECT * FROM (data_source) INTO TABLE NEW @table_data_ref UP TO @number_of_entries ROWS.
    ELSE.
      SELECT * FROM (data_source) INTO TABLE NEW @table_data_ref.
    ENDIF.
  ENDMETHOD.

  METHOD receive_visitor.
    "Passing an object of this class to the concrete visitor.
    obj->visit( me ).
  ENDMETHOD.

  METHOD get_itab_content_ref.
    IF table_data_ref IS BOUND.
      TRY.
          "Is a table
          DATA(tdo) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data_ref( table_data_ref ) ).
        CATCH cx_root.
          "Is not a table
          RETURN.
      ENDTRY.
      content = table_data_ref.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete visitor 1: XML transformation
*&---------------------------------------------------------------------*

CLASS lcl_visitor_xml DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_visitor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_visitor_xml IMPLEMENTATION.
  METHOD lif_visitor~visit.


    DATA(tab_content) = oref->get_itab_content_ref( ).
    IF tab_content IS BOUND.
      CALL TRANSFORMATION id SOURCE itab = tab_content->*
                             RESULT XML lif_visitor~result_xstring.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete visitor 2: JSON serialization
*&---------------------------------------------------------------------*

CLASS lcl_visitor_json DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_visitor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_visitor_json IMPLEMENTATION.
  METHOD lif_visitor~visit.
    DATA(tab_content) = oref->get_itab_content_ref( ).
    IF tab_content IS BOUND.
      lif_visitor~result_string = /ui2/cl_json=>serialize( data = tab_content->* format_output = abap_true ).
      lif_visitor~result_xstring = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( lif_visitor~result_string ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete visitor 3: Counting table entries
*&---------------------------------------------------------------------*

CLASS lcl_visitor_count DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_visitor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_visitor_count IMPLEMENTATION.
  METHOD lif_visitor~visit.
    DATA(tab_content) = oref->get_itab_content_ref( ).
    IF tab_content IS BOUND.
      lif_visitor~result_string = lines( tab_content->* ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete visitor 4: Converting table content into string
*&---------------------------------------------------------------------*

CLASS lcl_visitor_cont_str DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_visitor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_visitor_cont_str IMPLEMENTATION.
  METHOD lif_visitor~visit.
    DATA(tab_content) = oref->get_itab_content_ref( ).
    IF tab_content IS BOUND.
      LOOP AT tab_content->* ASSIGNING FIELD-SYMBOL(<fs>).
        DO.
          ASSIGN <fs>-(sy-index) TO FIELD-SYMBOL(<comp>).
          IF sy-subrc = 0.
            lif_visitor~result_string &&= <comp> && `, ` .
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
        "Replacing the final comma in the string and adding a line break
        lif_visitor~result_string = replace( val = lif_visitor~result_string pcre = `,([^,]*)$` with = `` ).
        lif_visitor~result_string &&= |\n|.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>