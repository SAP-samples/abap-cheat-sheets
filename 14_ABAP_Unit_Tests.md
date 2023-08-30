<a name="top"></a>

# ABAP Unit Tests

- [ABAP Unit Tests](#abap-unit-tests)
  - [Unit Tests in ABAP](#unit-tests-in-abap)
  - [High-Level Steps for ABAP Unit Tests](#high-level-steps-for-abap-unit-tests)
  - [Creating Test Classes](#creating-test-classes)
  - [Creating and Implementing Test Methods](#creating-and-implementing-test-methods)
    - [Special Methods for Implementing the Test Fixture](#special-methods-for-implementing-the-test-fixture)
  - [Handling Dependencies](#handling-dependencies)
    - [Test Seams](#test-seams)
  - [Running and Evaluating ABAP Unit Tests](#running-and-evaluating-abap-unit-tests)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)
 

This cheat sheet contains basic information about [unit testing](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenunit_test_glosry.htm) in ABAP.

> **💡 Note**<br>
> - This cheat sheet focuses on testing methods. 
> - See the [More Information](#more-information) section for links to more in-depth information.
> - The executable examples are **not** suitable role models for ABAP unit tests. They are intended to give you a rough idea. You should always work out your own solution for each individual case.

## Unit Tests in ABAP 
- Unit tests  ...
  - ensure the functional correctness of individual software units (i.e. a unit of code whose execution has a verifiable effect). 
  - are designed to test that the individual components of a larger software unit work correctly during the development and quality assurance phases. Typically, such individual software units are methods (the focus of this cheat sheet).
  - must be created and run by developers.
- In ABAP, developers have [ABAP Unit](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_unit_glosry.htm) - a test tool integrated into the ABAP runtime framework - at their disposal. It can be used to run individual or mass tests, and to evaluate test results. Note that comprehensive test runs can be performed using the [ABAP Test Cockpit](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_test_cockpit_glosry.htm)
- In ABAP programs, individual unit tests are implemented as [test methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentest_method_glosry.htm) of local [test classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentest_class_glosry.htm). 

<p align="right"><a href="#top">⬆️ back to top</a></p>  

## High-Level Steps for ABAP Unit Tests

- Identify dependend-on components (DOC) in your production code (i.e. your class/method) that need to be tested, and prepare the code for testing.
  - Examples of DOCs: In your production code, a method is called that is outside of your code. Or, for example, whenever there is an interaction with the database. 
  - This means that there are dependencies that need to be taken into account for a unit test. These dependencies should be isolated and replaced by a test double.
- Create/Implement test doubles
  - You can create test doubles manually. For example, you hardcode the test data to be used when the test is executed. Note: Automatic creation is not covered here.
  - Ideally, the testability of your code has been prepared by providing interfaces to the DOC. Interfaces facilitate the testability because you can simply implement the interface methods.
- Inject the test doubles to ensure that the test data is used during the test run.
- Create test classes and methods
- Run unit tests

> **💡 Note**<br>
> Of course, if there are no dependend-on components in your production code, you can skip the considerations of dependency isolation, test doubles and their injection into the production code.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Creating Test Classes

Before we look at test doubles and injections, we will look at the creation of the test classes and methods to get an idea of the code skeletons (in which test doubles and injections can be implemented). 

Test classes ...
- are special [local](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlocal_class_glosry.htm) or [global classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_class_glosry.htm) in which tests for ABAP Unit are implemented in the form of test methods. Note: In this cheat sheet, the focus is on local test classes only.
- are created in [class pools](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_pool_glosry.htm) in special [test includes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentest_include_glosry.htm). See the *Test Classes* tab in the ADT. 
- can only be used as part of test runs.
- are not generated in production systems, i.e. the source code of a test class is not part of the production code of its program.
- can contain test methods, the special methods for the [fixture](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfixture_glosry.htm), and other components.
  - It is recommended that all components required for ABAP unit tests are defined in test classes only (so that they cannot be generated in production systems and cannot be addressed by production code). The components also include test doubles and other helper classes that do not contain test methods.


The skeleton of a test class might look like this:
``` abap
"Test class in the test include
CLASS ltc_test_class DEFINITION 
  FOR TESTING                     "Defines a class to be used in ABAP Unit
  RISK LEVEL HARMLESS             "Defines risk level, options: HARMLESS/CRITICAL/DANGEROUS
  DURATION SHORT.                 "Expected test execution time, options: SHORT/MEDIUM/LONG

  ...

ENDCLASS.  

CLASS ltc_test_class IMPLEMENTATION.
  ...
ENDCLASS.  
```

> **💡 Note**<br>
> - `FOR TESTING` can be used for multiple purposes:
>   - Creating a test class containing test methods
>   - Creating a test double
>   - Creating helper methods to support ABAP unit tests
>   - Note the possible [syntax options](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_options.htm) before `FOR TESTING` 
> - Optional addition `RISK LEVEL ...`: 
>   - `CRITICAL`: test changes system settings or customizing data (default)
>   - `DANGEROUS`: test changes persistent data
>   - `HARMLESS`: test does not change system settings or persistent data  
> - Optional addition `DURATION ...`:
>   - `SHORT`: execution time of only a few seconds is expected
>   - `MEDIUM`: execution time of about one minute is expected
>   - `LONG`: execution time of more than one minute is expected  
> - To create a class in ADT, type "test" in the "Test Classes" tab and choose `CTRL` and `SPACE` to display the templates suggestions. You can then choose "testClass – Test class (ABAP Unit)". The skeleton of a test class is automatically generated. 

To test protected or private methods, you must declare [friendship](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfriend_glosry.htm) with the class to be tested (class under test).
Example:

``` abap
"The code in this snippet refers to the test class in the test include.
"Test class, declaration part
CLASS ltc_test_class DEFINITION 
  FOR TESTING                     
  RISK LEVEL HARMLESS             
  DURATION SHORT.                 

  ...

ENDCLASS.  

"Declaring friendship
CLASS cl_class_under_test DEFINITION LOCAL FRIENDS ltc_test_class.

"Test class, implementation part
CLASS ltc_test_class IMPLEMENTATION.
  ...
ENDCLASS.  
```

If you have multiple test classes in the test include, you can place the friendship declaration for all classes at the top, for example, as follows. Note the [`DEFERRED`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_deferred.htm) addition.

``` abap
"Test include

CLASS ltc_test_class_1 DEFINITION DEFERRED.
CLASS ltc_test_class_2 DEFINITION DEFERRED.
CLASS cl_class_under_test DEFINITION LOCAL FRIENDS ltc_test_class_1
                                                   ltc_test_class_2.

CLASS ltc_test_class_1 DEFINITION 
  FOR TESTING 
  RISK LEVEL HARMLESS             
  DURATION SHORT.                 
  ...
ENDCLASS.  

CLASS ltc_test_class_1 IMPLEMENTATION.
  ...
ENDCLASS.  

CLASS ltc_test_class_2 DEFINITION 
  FOR TESTING                     
  RISK LEVEL HARMLESS             
  DURATION SHORT.                 
     ...
ENDCLASS.  

CLASS ltc_test_class_2 IMPLEMENTATION.
  ...
ENDCLASS.  
```


**Including Interfaces**
- Usually, all non-optional interface methods must be implemented. 
- When you use the `PARTIALLY IMPLEMENTED`  addition in test classes, you are not forced to implement all of the methods. 
- It is particularly useful for interfaces to implement test doubles, and not all methods are necessary.
 
Example of creating a test double:

``` abap
"Test double class in a test include
CLASS ltd_test_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES some_intf PARTIALLY IMPLEMENTED.    
ENDCLASS.

CLASS ltd_test_double IMPLEMENTATION.
  METHOD some_intf~some_meth.
    ...   
  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Creating and Implementing Test Methods

Test methods ...
- are special [instance methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_method_glosry.htm) of a test class in which a test is implemented. 
  - As with test classes, the `FOR TESTING` addition also applies to the test method declaration.
  - Note that there are other syntax options, such as [`ABSTRACT` and others](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_testing.htm).
- are called by the ABAP Unit framework during a test run.
- are used to call units of production code and to check the result.
  - The results are checked using methods of the class `CL_ABAP_UNIT_ASSERT`.
- should be private or protected if the methods are inherited. 
- are called in an undefined order.
- have no parameters.

**Creating Test Methods**

Example:
``` abap
"Test class in a test include
"Test class declaration part
CLASS ltc_test_class DEFINITION 
  FOR TESTING                     
  RISK LEVEL HARMLESS             
  DURATION SHORT.                 

    PRIVATE SECTION.
      "Note: As a further component in test classes, usually, a reference variable
      "is created for an instance of the class under test.
      DATA ref_cut TYPE REF TO cl_class_under_test.  

      "Test method declaration
      METHODS some_test_method FOR TESTING.

ENDCLASS.  

"Test class implementation part
CLASS ltc_test_class IMPLEMENTATION.

  METHOD some_test_method.
     ... "Here goes the implementation of the test.     
  ENDMETHOD.

ENDCLASS.  
```

**Implementing Test Methods**
- The implementation ideally follows the *given-when-then* pattern.
  - *given*: Preparing the test, e.g. creating an instance of the class under test and a local test double (to inject the test double into the class under test)
  - *when*: Calling the procedure to be tested
  - *then*: Checking and evaluating the test result using the static methods of the `CL_ABAP_UNIT_ASSERT` class
- A selection of static methods of the class `CL_ABAP_UNIT_ASSERT` that can be used for the checks:
  - `ASSERT_EQUALS`: Checks whether two data objects are the same. 
  - `ASSERT_BOUND`: Checks whether a reference variable is bound. 
  - `ASSERT_NOT_BOUND`: Negation of the one above.
  - `ASSERT_INITIAL`: Checks whether a data object has its initial value.
  - `ASSERT_NOT_INITIAL`: Negation of the one above.
  - `ASSERT_SUBRC`: Checks the value of `sy-subrc`. 
  - `FAIL`: Triggers an error. 
- For the class and methods, as well as the paramters, check the F2 information in the ADT. Depending on the method used, parameters can (or must) be specified. To name a few: 
  - `ACT`: of type `ANY` (in most cases), non-optional importing parameter of the methods, specifies a data object that is to be verified
  - `EXP`: of type `ANY` (in most cases), a data object holding the expected value (non-optional for the `ASSERT_EQUALS` method)
  - `MSG`: of type `CSEQUENCE`, error description
  - `QUIT`: of type `INT1`, the specification affects the unit test flow control in case of an error, e.g. the constant `if_abap_unit_constant=>quit-no` can be used to determine that the unit test should not be terminated in case of an error

The following code snippet shows a class declaration and implementation part in the production code. 
A test method is to be created for the method. 

``` abap
"The code in this snippet refers to the production code in the global class.
"It shows a method declaration for a simple calculation in the class under test. 

"Class declaration part
CLASS cl_class_under_test DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PRIVATE SECTION.

    METHODS:
      multiply_by_two IMPORTING num        TYPE i                        
                      RETURNING VALUE(result) TYPE i.

...
"Class implementation part
CLASS cl_class_under_test IMPLEMENTATION.

METHOD multiply_by_two.
  result = num * 2.
ENDMETHOD.
...

```

Example: Test method for the example method *multiply_by_two* in the production code

As mentioned above, in simple cases, there may not be any dependend-on component in (a method of) the production code. Therefore, the functionality of the method in the production code is tested by simply comparing the value of the actual value (the value that is returned by the method, for example) and the expected value.

``` abap
"The code in this snippet refers to the test class in the test include.
...

"Test class, implementation part
CLASS ltc_test_class IMPLEMENTATION.

  "Test method implementation
  METHOD some_test_method.
    "given
    "Creating an object of the class under test
    "Assumption: A reference variable has been declared (DATA ref_cut TYPE REF TO cl_class_under_test.)   
    "A variable declared inline may not be the best choice if there is more than one 
    "test method in the class that also needs an object of the class under test.
    "See also the setup method further down in this context.
    ref_cut = NEW #( ).
    "DATA(ref_cut) = NEW cl_class_under_test( ).
    
    "when
    "Calling method that is to be tested    
    "As an example, 5 is inserted. The result should be 10, which is then checked.    
    DATA(result) = ref_cut->multiply_by_two( 5 ).

    "then
    "Assertion
    cl_abap_unit_assert=>assert_equals(
          act = result
          exp = 10 ).

"Further optional parameters specified
*      cl_abap_unit_assert=>assert_equals(
*            act = result
*            exp = 10
*            msg = |The result of 5 multiplied by 2 is wrong. It is { result }.|  "Error message
*            quit = if_abap_unit_constant=>quit-no ).                             "No test termination 

  ENDMETHOD.

ENDCLASS.  
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Special Methods for Implementing the Test Fixture
- Special private methods for implementing the test [fixture](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfixture_glosry.htm), which may include test data and test objects among others, can be included in the local test class.
- They are not test methods and the `FOR TESTING` addition cannot be used.
- They have no parameters.
- Instance methods: 
  - `setup`: Executed before each execution of a test method of a test class
  - `teardown`: Executed after each execution of a test method of a test class
- Static methods
  - `class_setup`: Executed once before all tests of the class
  - `class_teardown`: Executed once after all tests of the class

Example: 

``` abap
"Test class, declaration part
CLASS ltc_test_class DEFINITION 
  FOR TESTING                     
  RISK LEVEL HARMLESS             
  DURATION SHORT.                 

    PRIVATE SECTION.
    DATA: ref_cut TYPE REF TO cl_class_under_test. 

    METHODS: 
             "special methods
             setup,    
             teardown, 
             
             "test methods 
             some_test_method FOR TESTING,
             another_test_method FOR TESTING.

ENDCLASS.  

"Test class, implementation part
CLASS ltc_test_class IMPLEMENTATION.

  METHOD setup.      
      "Creating an object of the class under test
      ref_cut = NEW #( ).

       "Here goes, for example, code for preparing the test data, 
       "e.g. filling a database table used for test methods.
       ... 
  ENDMETHOD.


  METHOD some_test_method.
     
     "Method call 
     DATA(result) = ref_cut->multiply_by_two( 5 ).
        
     "Assertion
     cl_abap_unit_assert=>...

  ENDMETHOD.

  METHOD another_test_method.
    "Another method to be tested, it can also use the centrally created instance.

     "Calling method
     DATA(result) = ref_cut->multiply_by_three( 6 ).
    
    "Assertion
     cl_abap_unit_assert=>...

  ENDMETHOD.

  METHOD teardown.
       "Here goes, for example, code to undo the test data preparation 
       "during the setup method (e.g. deleting the inserted database table entries).
       ... 
  ENDMETHOD.

ENDCLASS.  
```

> **💡 Note**<br>
> You can also specify helper methods, for example, for recurring tasks such as the assertions.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Handling Dependencies 

The code snippets above covered test classes and methods in simple contexts without dependend-on components (DOC) in the production code. In more complex cases with dependencies in the production code, there are ways to deal with them. You can create test doubles and inject them into the production code during the test run. 

It is assumed that you have identified the dependend-on components (DOC) in a method in your production code. They were isolated (which may have involved some major rebuilding if the code is not created from scratch and the requirements for unit testing were taken into account, e.g. by creating an interface), and you want to replace them with a test double (by injection) so that your code can be  properly tested without dependencies.


**Creating/Implementing test doubles**

As recommended, you should ideally have an interface to the DOC. 

There are multiple ways to implement test doubles manually: 
- Interface is available for DOC 
  - You simply create a local test double by implementing interface methods to create test data. See the code snippet in the *Including Interfaces* section.
  - As mentioned above, the `PARTIALLY IMPLEMENTED` addition is useful (and only possible) for interfaces in test classes.
- No interface is available for the DOC
  - You can create your own local interface, adapt your production code accordingly, and implement interface methods.  
- If the DOC is a method in a class that allows inheritance (i.e. it is not defined as `FINAL`), you can inherit from the class and redefine methods for which you need a test double. 

> **💡 Note**<br>
> Instead of manually creating test doubles, the [ABAP OO Test Double Framework](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/804c251e9c19426cadd1395978d3f17b.html?locale=en-US) helps you create test doubles automatically.

**Injecting the test doubles**

As described [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/04a2d0fc9cd940db8aedf3fa29e5f07e.html?locale=en-US), there are multiple techniques for injecting test doubles to ensure that the test doubles are used during the test run. 

Among them, there are the following. They are demonstrated in the executable example. Check the code and comments in the [global class](./src/zcl_demo_abap_unit_test.clas.abap) and [test include](./src/zcl_demo_abap_unit_test.clas.testclasses.abap) of the example.
- Constructor injection: The test double is passed as a parameter to the instance constructor `constructor` of the class under test.
- Setter injection: The test double is passed as a parameter to a setter method.
- Parameter injection: The test double is passed as a parameter to the tested method (i.e. an optional importing parameter) in the class under test. 
- Back door injection: A *back door* is created to inject a test double into the class under test. This *back door* is implemented by granting [friendship](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfriend_glosry.htm) to the test class. This makes internal attributes of the class under test accessible from the test class.
 
<p align="right"><a href="#top">⬆️ back to top</a></p>

### Test Seams
- Seams are sections of the production source code that can be dynamically included or replaced. 
- In ABAP, [test seams](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentest_seam_glosry.htm) can be used to replace source code in the production code by an injection when running unit tests. For more informarion, see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptest-seam.htm).
- This is particularly useful in situations where tests cannot be executed properly or are even prevented from doing so. For example: 
  - Authorization checks
  - Reading or modifying persistent data from the database
  - Creating test doubles
- If the code is not executed in the context of a unit test, there is no injection. The original code (i.e. the code in the block `TEST-SEAM ... END-TEST-SEAM`) is executed.
- A test seam can also be empty, and then some code can be injected where the test seam is declared.


Test seams can be implemented using the following syntax:

``` abap
"The code in this snippet refers to the production code.

...

DATA some_table TYPE TABLE OF dbtab WITH EMPTY KEY.

"TEST-SEAM + name of the test seam ... END-TEST-SEAM define a 
"code block that is to be replaced when running unit tests.

TEST-SEAM select_from_db.
  SELECT * FROM dbtab 
    INTO TABLE @some_table.
END-TEST-SEAM.

...
```

``` abap
"The code in this snippet refers to the code in the test class.

...

"TEST-INJECTION + name of the test seam ... END-TEST-INJECTION define a code 
"block to replace the code block in the production code when running unit tests.
"In the example below, the DOC (a database access) is solved by providing local 
"test data.

TEST-INJECTION select_from_db.
  some_table = VALUE #(
      ( ... )
      ( ... ) ).
END-TEST-INJECTION.

...
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Running and Evaluating ABAP Unit Tests
There are many ways to run ABAP unit tests as described [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/4ec4c6c66e391014adc9fffe4e204223.html?locale=en-US).

The focus of this cheat sheet is on running individual tests in a class that can be run directly in ADT. In your class in ADT (for example, in the class of the demonstration example), choose `Ctrl + Shift + F10` to run all tests in a class. You can also right-click anywhere in the code of the class and choose *Run as → ABAP Unit Test*. To run individual test classes or methods, place the cursor on the class/method name and run the unit test.

The results of a test run are displayed and can be evaluated in the *ABAP Unit* tab in ADT. The *Failure Trace* section provides information about any errors found.

If you are interested in the test coverage, you can choose `Ctrl + Shift + F11`, or make a right-click, choose *Run as → ABAP Unit Test With...*, select the *Coverage* checkbox and choose *Execute*. You can then check the results in the *ABAP Coverage* tab in ADT and see what code was tested and what was not. 

For more information about evaluating ABAP unit test results, see [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/4ec49c5b6e391014adc9fffe4e204223.html?locale=en-US).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information

- [ABAP Unit in the ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_unit.htm)
- [openSAP course: Writing Testable Code for ABAP](https://open.sap.com/courses/wtc1.OpenSAP+WTC1_W1U5+Writing+Testable+Code+for+ABAPComent)
- Information on the SAP Help Portal: 
  - [Unit Testing with ABAP Unit](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/08c60b52cb85444ea3069779274b43db.html?locale=en-US)
  - [ABAP Unit](https://help.sap.com/docs/ABAP_PLATFORM_NEW/ba879a6e2ea04d9bb94c7ccd7cdac446/491cfd8926bc14cde10000000a42189b.html?locale=en-US)
- The [ABAP OO Test Double Framework](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/804c251e9c19426cadd1395978d3f17b.html?locale=en-US) based on class `CL_ABAP_TESTDOUBLE` simplifies and standardizes the creation and configuration of test doubles.
- [Testing repository objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentest_relations.htm) (ABAP Keyword Documentation)
- [Development Guide for the ABAP RESTful Application Programming Model: Testing different artifacts of RAP business objects and related OData services](https://help.sap.com/docs/btp/sap-abap-restful-application-programming-model/test?locale=en-US)

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

[zcl_demo_abap_unit_test](./src/zcl_demo_abap_unit_test.clas.abap)

Note ...
- the steps outlined [here](README.md#-getting-started-with-the-examples) about how to import and run the code.
- the comments in the executable example.