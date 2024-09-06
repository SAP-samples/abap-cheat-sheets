<a name="top"></a>

# ABAP Unit Tests

- [ABAP Unit Tests](#abap-unit-tests)
  - [Unit Tests in ABAP](#unit-tests-in-abap)
  - [High-Level Steps for ABAP Unit Tests](#high-level-steps-for-abap-unit-tests)
  - [Creating Test Classes](#creating-test-classes)
  - [Creating Test Methods](#creating-test-methods)
  - [Implementing Test Methods](#implementing-test-methods)
    - [Evaluating the Test Result with Methods of the CL\_ABAP\_UNIT\_ASSERT Class](#evaluating-the-test-result-with-methods-of-the-cl_abap_unit_assert-class)
    - [Special Methods for Implementing the Test Fixture](#special-methods-for-implementing-the-test-fixture)
  - [Handling Dependencies](#handling-dependencies)
    - [Creating/Implementing Test Doubles](#creatingimplementing-test-doubles)
    - [Injecting Test Doubles](#injecting-test-doubles)
    - [Test Seams](#test-seams)
  - [Creating Test Doubles Using ABAP Frameworks](#creating-test-doubles-using-abap-frameworks)
    - [Example: Creating Test Doubles Using ABAP Frameworks](#example-creating-test-doubles-using-abap-frameworks)
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

- Identify dependent-on components (DOC) in your production code (i.e. your class/method) that need to be tested, and prepare the code for testing.
  - Examples of DOCs: In your production code, a method is called that is outside of your code. Or, for example, whenever there is an interaction with the database. 
  - This means that there are dependencies that need to be taken into account for a unit test. These dependencies should be isolated and replaced by a test double.
- Create/Implement test doubles
  - You can create test doubles manually. For example, you hardcode the test data to be used when the test is executed. You can also use ABAP frameworks that provide a standardized approach to creating test doubles. 
  - Ideally, the testability of your code has been prepared by providing interfaces to the DOC. Interfaces facilitate the testability because you can simply implement the interface methods.
- Inject the test doubles to ensure that the test data is used during the test run.
- Create test classes and methods
- Run unit tests

> **💡 Note**<br>
> In some examples, the code does not have any dependent-on components. Therefore, the considerations about dependency isolation, test doubles, and their injection into the code are skipped.

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
> - To create a class in ADT, type "test" in the "Test Classes" tab and choose `CTRL + SPACE` to display the template suggestions. You can then choose "testClass – Test class (ABAP Unit)". The skeleton of a test class is automatically generated. 

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

## Creating Test Methods

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

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Implementing Test Methods

- The implementation ideally follows the *given-when-then* pattern.
  - *given*: Preparing the test, e.g. creating an instance of the class under test and a local test double (to inject the test double into the class under test)
  - *when*: Calling the procedure to be tested
  - *then*: Checking and evaluating the test result using the static methods of the `CL_ABAP_UNIT_ASSERT` class (see next section)

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

As mentioned above, in simple cases, there may not be any dependent-on component in (a method of) the production code. Therefore, the functionality of the method in the production code is tested by simply comparing the value of the actual value (the value that is returned by the method, for example) and the expected value.

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


### Evaluating the Test Result with Methods of the CL_ABAP_UNIT_ASSERT Class

The following overview covers a selection of static methods of the `CL_ABAP_UNIT_ASSERT`  class that can be used for the checks.

| Method | Details |
|---|---|
| `ASSERT_EQUALS`  | Checks whether two data objects are the same  |
| `ASSERT_BOUND`  |  Checks whether a reference variable is bound  |
| `ASSERT_NOT_BOUND`  | Negation of the one above  |
| `ASSERT_INITIAL`  | Checks whether a data object has its initial value  |
| `ASSERT_NOT_INITIAL`  | Negation of the one above  |
| `ASSERT_SUBRC`  | Checks the value of `sy-subrc`   |
| `ASSERT_CHAR_CP`  | Character sequence matching a pattern   |
| `ASSERT_CHAR_NP`  | Negation of the one above  |
| `ASSERT_DIFFERS`  | Checks whether two elementary data objects are different   |
| `ASSERT_TRUE`  |  Checks whether boolean is true  |
| `ASSERT_FALSE`  |  Checks whether boolean is false  |
| `ASSERT_NUMBER_BETWEEN`  | Checks whether a number is in a given range  |
| `ASSERT_RETURN_CODE`  | Checks whether the return code has a specific value  |
| `ASSERT_TABLE_CONTAINS`  |  Checks whether data is contained as line in an internal table  |
| `ASSERT_TABLE_NOT_CONTAINS`  |  Negation of the one above  |
| `ASSERT_TEXT_NOT_MATCHES`  | Checks whether text does not contain/contains text matching a regular expression   |
| `ASSERT_THAT`  | Checks whether a constraint is met by a data object   |
| `FAIL`  | Triggers an error   |
| `SKIP`  | Skips a test because of missing prerequisites   |

For the class and methods, as well as the paramters, check the F2 information in the ADT. Depending on the method used, parameters can (or must) be specified. To name a few: 

| Parameters | Details |
|---|---|
| `ACT`  | It is of type `ANY` (in most cases), non-optional importing parameter of the methods, specifies a data object that is to be verified  |
| `EXP`  | It is of type `ANY` (in most cases), a data object holding the expected value (non-optional for the `ASSERT_EQUALS` method)  |
| `MSG`  | It is of type `CSEQUENCE`, error description  |
| `QUIT`  | It is of type `INT1`, the specification affects the unit test flow control in case of an error, e.g. the constant `if_abap_unit_constant=>quit-no` can be used to determine that the unit test should not be terminated in case of an error  |


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
    DATA ref_cut TYPE REF TO cl_class_under_test. 

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

The code snippets above covered test classes and methods in simple contexts without dependent-on components (DOC) in the production code. In more complex cases with dependencies in the production code, there are ways to deal with them. You can create test doubles and inject them into the production code during the test run. 

It is assumed that you have identified the dependent-on components (DOC) in a method in your production code. They were isolated (which may have involved some major rebuilding if the code is not created from scratch and the requirements for unit testing were taken into account, e.g. by creating an interface), and you want to replace them with a test double (by injection) so that your code can be  properly tested without dependencies.


### Creating/Implementing Test Doubles

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

### Injecting Test Doubles

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

## Creating Test Doubles Using ABAP Frameworks

The previous sections covered self-made test doubles. You can use ABAP frameworks to create and inject test doubles, providing a standardized method for replacing dependent-on components (DOC) during unit tests. These DOCs can include classes, database tables/CDS views, RAP business objects, and more.

Several frameworks are available and demonstrated in the example classes below. Note that there are more frameworks available that are not further covered here, e.g. for managing dependencies on function modules. For more details, refer to the [documentation](https://help.sap.com/docs/abap-cloud/abap-development-tools-user-guide/managing-dependencies-with-abap-unit).

<table>
<tr>
<td> DOC </td> <td> Details </td>
</tr>
<tr>
<td> Classes and interfaces </td>
<td>

- You can use the ABAP OO Test Double Framework.
- As a prerequisite, you ...
  - have an ABAP class ready to replace the DOC. Instead of using a test double with an interface, the example below involves the class under test directly using the DOC, which is a non-final ABAP class. 
  - apply an injection mechanism to replace the original class with the test double class. The following example illustrates constructor and parameter injection, but you can find more injection mechanisms in the documentation and the executable example in the ABAP cheat sheets repository.
- Using the framework, you have the ability to configure values for return, export, and change parameters, as well as handle exceptions and events with each method call. The `CL_ABAP_TESTDOUBLE` class supports you with  the test double configuration, offering a range of functionalities, including verifying interactions on the test double. 
- However, be aware that `CL_ABAP_TESTDOUBLE` does not support certain use cases, including local classes/interfaces and classes with declarations such as `FINAL`, `FOR TESTING`, `CREATE PRIVATE`, or constructors with mandatory parameters.


</td>
</tr>
<tr>
<td> Database (e.g. database tables or CDS view entities) </td>
<td>

- You can use these framworks: 
  - CDS Test Double Framework: For testing logic implemented in CDS entities; makes use of the `CL_CDS_TEST_ENVIRONMENT` class
  - ABAP SQL Test Double Framework: For testing ABAP SQL statements that depend on data sources such as database tables or CDS view entities; makes use the `CL_OSQL_TEST_ENVIRONMENT` class

</td>
</tr>

<tr>
<td> RAP business object </td>
<td>

- Dependencies on RAP business objects can be handled by ...
  - creating transactional buffer test doubles (`CL_BOTD_TXBUFDBL_BO_TEST_ENV` class)
  - mocking ABAP EML APIs (`CL_BOTD_MOCKEMLAPI_BO_TEST_ENV` class)

</td>
</tr>

</table>

### Example: Creating Test Doubles Using ABAP Frameworks

The example classes below illustrate how to create and inject test doubles using the mentioned ABAP frameworks.

To run the example, follow these steps:
- First, import the ABAP cheat sheet repository. The example relies on some of its repository objects.
- Next, create two classes: `ZCL_DEMO_ABAP_UNIT_TDF` (the class under test) and `ZCL_DEMO_ABAP_UNIT_DATAPROV` (the DOC containing methods called by the first class). Copy and paste the provided code into these classes. Note that `ZCL_DEMO_ABAP_UNIT_TDF` includes a test class in the test include (*Test Classes* tab in ADT).
- The example is set up to display content in the ADT console when you run the class with *F9*. This is solely to demonstrate the effects of statements and method calls. The main focus is on test methods that explore test double creation using the frameworks. To launch the tests in the class, use *Ctrl/Cmd + Shift + F10*.

> **💡 Note**<br>
> - The example classes are intentionally simplified and nonsemantic, designed to highlight basic unit tests and explore framework classes and methods. 
> - They are not meant to serve as a model for proper unit test design. Always devise your own solutions for each unique case. 
> - Refer to the code comments, SAP Help Portal and class documentation for additional information.


Expand the following collapsible section to view the code of an example.

<details>
  <summary>Expand to view the example code</summary>
  <!-- -->
<br>

**Class 1 `ZCL_DEMO_ABAP_UNIT_TDF` (the class under test)**

1a) Insert the following code in the `Global Class` tab


```abap
CLASS zcl_demo_abap_unit_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

* -------------------------- NOTE ----------------------------------
* - This is an example class that demonstrates managing dependencies
*   (dependent-on-components, DOC) with ABAP Unit.
* - Topics covered:
*   - ABAP OO Test Double Framework (test doubles injected using
*     constructor and parameter injection)
*   - ABAP SQL Test Double Framework
*   - ABAP CDS Test Double Framework
*   - Managing dependencies on RAP business objects (mocking ABAP EML
*     APIs, transactional buffer test doubles)
*
* ----------------------- RUN ABAP UNIT TEST---------------------------
* - Open the class with the ABAP development tools for Eclipse (ADT).
* - Choose Ctrl/Cmd + Shift + F10 to launch all tests in the class.
*   You can also right-click in the class and choose Run as -> ABAP Unit
*   Test.
* - The results of a test run are displayed in the ABAP Unit tab in ADT
*   and can be evaluated.
*
* ----------------------- RUN CLASS -----------------------------
* - Open the class with the ABAP development tools for Eclipse (ADT).
* - Choose F9 to run the class and check the console output.

    INTERFACES if_oo_adt_classrun.

    "--------------- 1) ABAP OO Test Double Framework ---------------
    "The examples in the test include use the cl_abap_testdouble class.

    "----------- 1a) Demonstrating constructor injection -----------

    "Specifying the instance constructor with an optional parameter
    "for the purpose of constructor injection as injection mechanism.
    "In this example, methods are called from another, non-final class.
    "They represent DOCs. Test doubles are injected when running unit
    "tests. The parameter expects an instance of this class.
    METHODS constructor
      IMPORTING oref_constr_inj TYPE REF TO zcl_demo_abap_unit_dataprov OPTIONAL.

    "Declaring an object reference variable that will be used to call
    "methods of an external class (the DOC)
    DATA oref_data_provider TYPE REF TO zcl_demo_abap_unit_dataprov.

    "Method that is tested and contains a DOC. In this case, it is an
    "external method that is called in the implementation part. The DOC
    "is replaced by a test double created using cl_abap_testdouble.
    METHODS td_constr_inj_calc_discount
      IMPORTING
        value         TYPE numeric
      RETURNING
        VALUE(result) TYPE decfloat34.

    "----------- 1b) Demonstrating parameter injection -----------

    "Method that is tested and contains a DOC. In this case, it is an
    "external method that is called in the implementation part. The DOC
    "is replaced by a test double created using cl_abap_testdouble.
    "The example demonstrates parameter injection. Therefore, an
    "optional importing parameter is included. When running the unit
    "test, 'data_prov' is bound, and the test double is injected.
    METHODS td_param_inj_calc_discount
      IMPORTING value         TYPE numeric
                date          TYPE d
                time          TYPE t
                data_prov     TYPE REF TO zcl_demo_abap_unit_dataprov OPTIONAL
      EXPORTING message       TYPE string
                weekday       TYPE string
      RETURNING VALUE(result) TYPE decfloat34.

    "----------- 2) ABAP SQL Test Double Framework -----------
    "The examples in the test include use the cl_osql_test_environment
    "class. Here, a database table represents the DOC.
    "The method includes a SELECT statement.

    METHODS sql_get_shortest_flight_time IMPORTING carrier                TYPE zdemo_abap_flsch-carrid
                                         RETURNING VALUE(shortest_flight) TYPE i.

    "----------- 3) ABAP CDS Test Double Framework -----------
    "The examples in the test include use the cl_cds_get_data_set_environment class.
    "Here, a CDS view entity represents the DOC. The method includes a SELECT
    "statement. Another test method is implemented in the test inlcude that
    "demonstrates the testing of a CDS view entity (without testing a method in the
    "class under test).
    METHODS cds_get_data_set IMPORTING carrier         TYPE zdemo_abap_cds_ve_agg_exp-carrid
                             RETURNING VALUE(agg_line) TYPE zdemo_abap_cds_ve_agg_exp.

    "----------- 4) Managing dependencies on RAP business objects -----------
    "----------- 4a) Demonstrating mocking ABAP EML APIs --------------------
    "----------- ABAP EML read operations -----------------------------------
    "The examples in the test include use the cl_botd_mockemlapi_bo_test_env class.

    "Populating database tables for ABAP EML read requests
    METHODS prep_dbtab_for_eml IMPORTING read_op TYPE abap_boolean DEFAULT abap_false.
    TYPES read_tab_ro TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m.
    TYPES read_tab_ch TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m\_child.

    "Method that includes an ABAP EML read request on the root entity
    METHODS eml_read_root IMPORTING key           TYPE zdemo_abap_rap_ro_m-key_field
                          RETURNING VALUE(tab_ro) TYPE read_tab_ro.

    "Method that includes an ABAP EML read-by-associaton request
    METHODS eml_rba IMPORTING key           TYPE zdemo_abap_rap_ro_m-key_field
                    RETURNING VALUE(tab_ch) TYPE read_tab_ch.

    "----------- ABAP EML modify operation -----------
    TYPES modify_tab_ro TYPE TABLE FOR CREATE zdemo_abap_rap_ro_m.
    TYPES ty_mapped TYPE RESPONSE FOR MAPPED EARLY zdemo_abap_rap_ro_m.
    TYPES ty_failed TYPE RESPONSE FOR FAILED EARLY zdemo_abap_rap_ro_m.
    TYPES ty_reported TYPE RESPONSE FOR REPORTED EARLY zdemo_abap_rap_ro_m.

    "Method that includes an ABAP EML modify request on the root entity
    METHODS eml_modify_root IMPORTING VALUE(instances) TYPE modify_tab_ro
                            EXPORTING mapped           TYPE ty_mapped
                                      failed           TYPE ty_failed
                                      reported         TYPE ty_reported.

    "----------- 4b) Demonstrating transactional buffer test doubles ---------
    "The examples in the test include use the cl_botd_txbufdbl_bo_test_env class.
    TYPES read_tab_ro_u TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_u.

    "Method that includes an ABAP EML read request on the root entity
    METHODS eml_read_root_buffer_td IMPORTING key             TYPE zdemo_abap_rap_ro_u-key_field
                                    RETURNING VALUE(tab_ro_u) TYPE read_tab_ro_u.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_demo_abap_unit_tdf IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    "This implementation includes method calls of those methods that are unit tested
    "in the test include.
    "The implementation is just for demonstration purposes to explore the effect of
    "the methods.

    "Populating demo database tables (only required for running the example class
    "with F9 and exploring the effect of various method calls)
    zcl_demo_abap_aux=>fill_dbtabs( ).

    "----- 1) Methods demonstrating the ABAP OO Test Double Framework in the test include -----
    "----- 1a) Using constructor injection -----
    DATA(td_constr_inj_calc_discount) = td_constr_inj_calc_discount( 100 ).
    out->write( data = td_constr_inj_calc_discount name = `td_constr_inj_calc_discount` ).
    out->write( |\n| ).

    td_constr_inj_calc_discount = td_constr_inj_calc_discount( 500 ).
    out->write( data = td_constr_inj_calc_discount name = `td_constr_inj_calc_discount` ).
    out->write( |\n| ).

    td_constr_inj_calc_discount = td_constr_inj_calc_discount( CONV decfloat34( '3.4' ) ).
    out->write( data = td_constr_inj_calc_discount name = `td_constr_inj_calc_discount` ).
    out->write( |\n| ).

    "----- 1a) Using parameter injection -----
    "Example with Sunday
    td_param_inj_calc_discount(
      EXPORTING
        value     = 100
        date     = '20241201'
        time     = '100000'
      IMPORTING
       weekday   = DATA(weekday)
       message   = DATA(message)
      RECEIVING
        result    = DATA(td_param_inj_calc_discount)
    ).

    out->write( data = td_param_inj_calc_discount name = `td_param_inj_calc_discount` ).
    out->write( data = weekday name = `weekday` ).
    out->write( data = message name = `message` ).
    out->write( |\n| ).

    "Example with a weekday, morning
    td_param_inj_calc_discount(
     EXPORTING
       value     = 100
       date     = '20241202'
       time     = '100000'
     IMPORTING
      weekday   = weekday
      message   = message
     RECEIVING
       result    = td_param_inj_calc_discount
   ).

    out->write( data = td_param_inj_calc_discount name = `td_param_inj_calc_discount` ).
    out->write( data = weekday name = `weekday` ).
    out->write( data = message name = `message` ).
    out->write( |\n| ).

    "Example with a weekday, afternoon
    td_param_inj_calc_discount(
     EXPORTING
       value    = 100
       date     = '20241203'
       time     = '150000'
     IMPORTING
      weekday   = weekday
      message   = message
     RECEIVING
       result    = td_param_inj_calc_discount
   ).

    out->write( data = td_param_inj_calc_discount name = `td_param_inj_calc_discount` ).
    out->write( data = weekday name = `weekday` ).
    out->write( data = message name = `message` ).
    out->write( |\n| ).

    "Example with a weekday, night
    td_param_inj_calc_discount(
     EXPORTING
       value    = 100
       date     = '20241204'
       time     = '230000'
     IMPORTING
      weekday   = weekday
      message   = message
     RECEIVING
       result    = td_param_inj_calc_discount
   ).

    out->write( data = td_param_inj_calc_discount name = `td_param_inj_calc_discount` ).
    out->write( data = weekday name = `weekday` ).
    out->write( data = message name = `message` ).
    out->write( |\n| ).

    "----- 2) Methods demonstrating the ABAP SQL Test Double Framework in the test include -----
    DATA(sql_shortest_flight_time) = sql_get_shortest_flight_time( 'LH' ).
    out->write( data = sql_shortest_flight_time name = `sql_shortest_flight_time` ).
    out->write( |\n| ).

    sql_shortest_flight_time = sql_get_shortest_flight_time( 'AA' ).
    out->write( data = sql_shortest_flight_time name = `sql_shortest_flight_time` ).
    out->write( |\n| ).

    "----- 3) Methods demonstrating the ABAP CDS Test Double Framework in the test include -----
    DATA(data_set_cds) = cds_get_data_set( 'LH' ).
    out->write( data = data_set_cds name = `data_set_cds` ).
    out->write( |\n\n| ).

    data_set_cds = cds_get_data_set( 'AA' ).
    out->write( data = data_set_cds name = `data_set_cds` ).
    out->write( |\n\n| ).

    "----- Methods demonstrating mocking ABAP EML APIs in the test include -----
    "Populating demo database tables
    prep_dbtab_for_eml( read_op = abap_true ).

    DATA(tab_ro) = eml_read_root( key = 1 ).
    out->write( tab_ro ).
    out->write( |\n\n| ).

    DATA(tab_ch) = eml_rba( key = 1 ).
    out->write( tab_ch ).
    out->write( |\n\n| ).

    prep_dbtab_for_eml( ).

    eml_modify_root(
      EXPORTING
        instances =  VALUE #( ( %cid = `cid1` key_field = 3 field1 = 'aaa' field2 = 'bbb' field3 = 30 field4 = 300  )
                              ( %cid = `cid2` key_field = 4 field1 = 'ccc' field2 = 'ddd' field3 = 40 field4 = 400  ) )
      IMPORTING
        mapped    = DATA(m)
        failed    = DATA(f)
        reported  = DATA(r)
    ).

    ASSERT f IS INITIAL.
    ASSERT r IS INITIAL.
    out->write( data = m-root name = `m-root` ).
    out->write( |\n\n| ).

    SELECT * FROM zdemo_abap_rapt1 INTO TABLE @DATA(itab).
    out->write( data = itab name = `itab` ).
  ENDMETHOD.

  METHOD constructor.
    "--------------------- 1a) ---------------------
    "Demonstrating the constructor injection
    "The parameter is only bound when you run the unit test.
    "In that case, the test double is injected, and method calls
    "in the implementations use data from the test double.
    "Otherwise, a new instance is created that is used to call
    "methods (a test double is not injected).
    IF oref_constr_inj IS BOUND.
      oref_data_provider = oref_constr_inj.
    ELSE.
      oref_data_provider = NEW #( ).
    ENDIF.
  ENDMETHOD.

  METHOD td_constr_inj_calc_discount.
    "--------------------- 1a) ---------------------
    "Method that demonstrates the ABAP OO Test Double Framework and
    "constructor injection in ABAP Unit.
    "When running the unit test, 'oref_data_provider' includes the test
    "double. The method expects a numeric value. 'get_discount' returns
    "another numeric value on whose basis a discount calculation is
    "performed. The value returned by the 'get_discount' method depends
    "on the current weekday and the UTC time.
    result = ( value * oref_data_provider->get_discount( ) ) / 100.
    result = value - result.
  ENDMETHOD.

  METHOD td_param_inj_calc_discount.
    "--------------------- 1b) ---------------------
    "Method that demonstrates the ABAP OO Test Double Framework and
    "parameter injection in ABAP Unit.
    "When running the unit test, the optional parameter 'data_prov' is
    "assigned, and therefore bound here. 'oref_data_provider' then includes
    "the test double.
    "The purpose of this method is similar to 'td_constr_inj_calc_discount'.
    "Here, the method expects a date and time besides a numeric value.
    IF data_prov IS BOUND.
      oref_data_provider = data_prov.
    ENDIF.

    "Getting the weekday
    DATA(day_value) = ( 5 + date MOD 7 ) MOD 7 + 1.
    weekday = SWITCH #( day_value
                        WHEN 1 THEN `Monday`
                        WHEN 2 THEN `Tuesday`
                        WHEN 3 THEN `Wednesday`
                        WHEN 4 THEN `Thursday`
                        WHEN 5 THEN `Friday`
                        WHEN 6 THEN `Saturday`
                        WHEN 7 THEN `Sunday` ).

    DATA(time_value) = COND #( WHEN time BETWEEN '000000' AND '045959' THEN 1 "Night discount
                               WHEN time BETWEEN '220000' AND '235959' THEN 1 "Night discount
                               WHEN time BETWEEN '050000' AND '115959' THEN 2 "Morning discount
                               WHEN time BETWEEN '180000' AND '215959' THEN 3 "Evening discount
                               ELSE 0                                         "No discount
                       ).

    weekday = |{ weekday } ({ SWITCH #( time_value WHEN 1 THEN `night` WHEN 2 THEN `morning` WHEN 3 THEN `evening` ELSE `afternoon` ) })|.

    DATA(disc) = oref_data_provider->get_discount_value( day_value = day_value time_value = time_value ).
    result = ( value * disc ) / 100.
    result = value - result.
    message = |Original value: { value }; discount: { disc }; value with discount: { result }; | &&
    |when: { weekday }, { date DATE = ISO }, { time TIME = ISO }|.
  ENDMETHOD.

  METHOD sql_get_shortest_flight_time.
    "--------------------- 2) ---------------------
    "When running the unit test, the DOC (database table) is replaced with a test double.

    "Getting the shortest flight time among a given carrier
    SELECT MIN( fltime ) AS fltime FROM zdemo_abap_flsch WHERE carrid = @carrier INTO @shortest_flight.
  ENDMETHOD.

  METHOD cds_get_data_set.
    "--------------------- 3) ---------------------
    "When running the unit test, the DOC (CDS view entity) is replaced with a test double.

    "Getting a line filtered by the carrier
    SELECT SINGLE * FROM zdemo_abap_cds_ve_agg_exp WHERE carrid = @carrier INTO @agg_line.
  ENDMETHOD.

  METHOD prep_dbtab_for_eml.
    "Preparing database tables for ABAP EML statements
    DELETE FROM zdemo_abap_rapt1.

    IF read_op = abap_true.
      DELETE FROM zdemo_abap_rapt2.
      INSERT zdemo_abap_rapt1 FROM TABLE @( VALUE #( ( key_field = 1 field1 = 'aaa' field2 = 'bbb' field3 = 10 field4 = 100  ) ) ).
      INSERT zdemo_abap_rapt2 FROM TABLE @( VALUE #( ( key_field = 1 key_ch = 11 field_ch1 = 'ccc' field_ch2 = 111  )
                                                     ( key_field = 1 key_ch = 12 field_ch1 = 'ddd' field_ch2 = 112  ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD eml_read_root.
    "--------------------- 4a) ---------------------
    "Method that includes an ABAP EML read request on the root entity
    "When running the unit test, the ABAP EML API is mocked.

    READ ENTITIES OF zdemo_abap_rap_ro_m
      ENTITY root
      ALL FIELDS WITH VALUE #( ( key_field = key ) )
      RESULT tab_ro.
  ENDMETHOD.

  METHOD eml_rba.
    "--------------------- 4a) ---------------------
    "Method that includes an ABAP EML read-by-assocation request
    "When running the unit test, the ABAP EML API is mocked.

    READ ENTITIES OF zdemo_abap_rap_ro_m
       ENTITY root
       BY \_child
       ALL FIELDS WITH VALUE #( ( key_field = key ) )
       RESULT tab_ch.
  ENDMETHOD.

  METHOD eml_modify_root.
    "--------------------- 4a) ---------------------
    "Method that includes an ABAP EML create request
    "When running the unit test, the ABAP EML API is mocked.

    MODIFY ENTITIES OF zdemo_abap_rap_ro_m
      ENTITY root
      CREATE
      FIELDS ( key_field field1 field2 field3 field4 )
      WITH instances
      MAPPED mapped
      FAILED failed
      REPORTED reported.

    COMMIT ENTITIES.
  ENDMETHOD.

  METHOD eml_read_root_buffer_td.
    "--------------------- 4b) ---------------------
    "Method that includes an ABAP EML read request
    "When running the unit test, a transactional buffer test double is
    "included.

    READ ENTITIES OF zdemo_abap_rap_ro_u
      ENTITY root
      ALL FIELDS WITH VALUE #( ( key_field = key ) )
      RESULT tab_ro_u.
  ENDMETHOD.

ENDCLASS.
```

1b) Insert the following code in the `Test Classes` tab (it is a local test class of `ZCL_DEMO_ABAP_UNIT_TDF`)

```abap
CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    "------------------- Local test class -------------------

    "Object reference variable for class under test
    CLASS-DATA cut TYPE REF TO zcl_demo_abap_unit_tdf.

    "----- 1) ABAP OO Test Double Framework  ------
    "Test method that demonstrates constructor injection
    METHODS test_tdf_constr_inj_get_discnt FOR TESTING.

    "Test method that demonstrates parameter injection
    METHODS test_tdf_param_inj_get_discnt FOR TESTING.

    "----------- 2) ABAP SQL Test Double Framework -----------
    METHODS test_sql_get_shortest_flight FOR TESTING RAISING cx_static_check.
    CLASS-DATA sql_env TYPE REF TO if_osql_test_environment.

    "----------- 3) ABAP CDS Test Double Framework-----------
    METHODS test_select_cds FOR TESTING RAISING cx_static_check.
    METHODS test_cds_standalone FOR TESTING RAISING cx_static_check.
    METHODS prepare_testdata_set_cds.
    CLASS-DATA cds_env TYPE REF TO if_cds_test_environment.
    DATA zdemo_abap_fli_tab TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

    "----------- 4) Managing dependencies on RAP business objects -----------
    CONSTANTS entity TYPE abp_entity_name VALUE 'ZDEMO_ABAP_RAP_RO_M'.
    CLASS-DATA eml_env TYPE REF TO if_botd_mockemlapi_bo_test_env.
    "--- 4a) Test methods that demonstrate mocking EML APIs ---
    METHODS test_eml_read_root FOR TESTING.
    METHODS test_eml_rba FOR TESTING.
    METHODS test_eml_modify_root FOR TESTING.
    DATA td_eml TYPE REF TO if_botd_mockemlapi_test_double.

    "--- 4b) Test method that demonstrates transactional buffer test doubles ---
    METHODS test_eml_read_root_buffer_td FOR TESTING.

    CLASS-DATA buffer_env TYPE REF TO if_botd_txbufdbl_bo_test_env.

    "----------- Test fixture -----------
    "Creating a common start state for each test method, clearing doubles
    METHODS setup RAISING cx_static_check.
    "Includes the creation of test environments
    CLASS-METHODS class_setup RAISING cx_static_check.
    "Clearing generated test doubles
    CLASS-METHODS class_teardown.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD class_setup.
    "Creating an instance for the class under test
    cut = NEW zcl_demo_abap_unit_tdf( ).

    "Creating instances of test environments for the test execution
    "------ 2) SQL ------
    sql_env = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #( ( 'zdemo_abap_flsch' ) ) ).

    "------ 3) CDS ------
    cds_env = cl_cds_test_environment=>create( i_for_entity = 'zdemo_abap_cds_ve_agg_exp'
                                               test_associations = 'X'  ).

    "------ 4) ABAP EML ------
    "4a) Mocking ABAP EML APIs
    eml_env = cl_botd_mockemlapi_bo_test_env=>create(
      environment_config = cl_botd_mockemlapi_bo_test_env=>prepare_environment_config(
      )->set_bdef_dependencies( bdef_dependencies = VALUE #( ( 'ZDEMO_ABAP_RAP_RO_M' ) ) ) ).

    "4b) Transactional buffer test doubles
    buffer_env = cl_botd_txbufdbl_bo_test_env=>create(
      environment_config = cl_botd_txbufdbl_bo_test_env=>prepare_environment_config(
      )->set_bdef_dependencies( bdef_dependencies = VALUE #( ( 'ZDEMO_ABAP_RAP_RO_U' ) ) ) ).

  ENDMETHOD.

  METHOD class_teardown.
    "Clearing generated test double at the end of the test execution
    sql_env->destroy( ).
    cds_env->destroy( ).
    eml_env->destroy( ).
    buffer_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    sql_env->clear_doubles( ).
    cds_env->clear_doubles( ).
    eml_env->clear_doubles( ).
    buffer_env->clear_doubles( ).
  ENDMETHOD.

  METHOD test_sql_get_shortest_flight.
    "--- 2) ---
    "Preparing and inserting test data
    DATA test_data TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY.
    test_data = VALUE #(
     ( carrid = 'LH'
       connid =  0401
       fltime = 435 )
     ( carrid = 'LH'
       connid =  0402
       fltime = 455 )
     ( carrid = 'LH'
       connid =  2402
       fltime = 65 ) ).

    sql_env->insert_test_data( test_data ).

    "Calling method of the class under test
    DATA carrier TYPE zdemo_abap_flsch-carrid VALUE 'LH'.
    DATA(result) = cut->sql_get_shortest_flight_time( carrier ).

    "Verifying result
    cl_abap_unit_assert=>assert_equals(
          act = result
          exp = 65
          msg = `Not the shortest flight`
          quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

  METHOD prepare_testdata_set_cds.
    "Preparing and inserting test data
    zdemo_abap_fli_tab = VALUE #(
        ( carrid = 'XX'
          connid =  0407
          fldate =  '20231128'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   458
          paymentsum =   '563231.65'
          seatsmax_b = 30
          seatsocc_b = 27
          seatsmax_f =  20
          seatsocc_f = 20 )
        ( carrid = 'XX'
          connid =  0407
          fldate =  '20231019'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    'A380-800'
          seatsmax =   475
          seatsocc =   452
          paymentsum =   '553552.12'
          seatsmax_b = 30
          seatsocc_b = 28
          seatsmax_f =  20
          seatsocc_f = 19 )
        ( carrid = 'XX'
          connid =  0408
          fldate =  '20231128'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    '747-400'
          seatsmax =   385
          seatsocc =   365
          paymentsum =   '470129.20'
          seatsmax_b = 31
          seatsocc_b = 28
          seatsmax_f =  21
          seatsocc_f = 20 )
        ( carrid = 'XX'
          connid =  0408
          fldate =  '20230123'
          price  =    '1102.77'
          currency = 'JPY'
          planetype  =    '747-400'
          seatsmax =   385
          seatsocc =   372
          paymentsum =   '487715.90'
          seatsmax_b = 31
          seatsocc_b = 31
          seatsmax_f =  21
          seatsocc_f = 20 ) ).

    cds_env->insert_test_data( i_data = zdemo_abap_fli_tab ).
  ENDMETHOD.

  METHOD test_select_cds.
    "--- 3) ---
    "Preparing and inserting test data
    prepare_testdata_set_cds( ).

    "Calling method of the class under test
    DATA carrier TYPE zdemo_abap_fli-carrid VALUE 'XX'.
    DATA(act_result) = cut->cds_get_data_set( carrier ).

    "Verifying result
    cl_abap_unit_assert=>assert_equals(
          act = act_result
          exp = VALUE zdemo_abap_cds_ve_agg_exp( carrid = 'XX' currency = 'JPY' avg_seats_occ = '411.75'
                                                 avg_paysum = '518657.22' total_paysum = '2074628.87'
                                                 min_occ_seats = 365 max_occ_seats = 458
                                                 max_occ_seats_all = 458 cnt = 4 cnt_planetype = 2 )
          msg = `The values do not match the expected result`
          quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

  METHOD test_eml_read_root.
    "--- 4a) ---
    "Mocking ABAP EML API (read request)

    "Preparing test data (RAP BO instances)
    DATA read_tab_ro_import TYPE TABLE FOR READ IMPORT zdemo_abap_rap_ro_m.
    DATA read_tab_ro_result TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m.
    read_tab_ro_import = VALUE #( ( key_field = 2  ) ).
    read_tab_ro_result = VALUE #( ( key_field = 2 field1 = 'uuu' field2 = 'vvv' field3 = 20 field4 = 200  ) ).

    "Configuring input and output for the ABAP EML read request
    "Creating input/output configuration builders
    DATA(input_config_read) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
    DATA(output_config_read) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    "Creating input for the entities (here, only one entity is included)
    "Instead of the entity name, an alias name is also accepted.
    DATA(eml_read_input) = input_config_read->build_entity_part( entity
          )->set_instances_for_read( read_tab_ro_import ).

    "Input configuration
    DATA(input) = input_config_read->build_input_for_eml(  )->add_entity_part( eml_read_input ).

    "Output configuration
    DATA(output) = output_config_read->build_output_for_eml( )->set_result_for_read( read_tab_ro_result ).

    "Configuring the RAP BO test double
    td_eml = eml_env->get_test_double( entity ).
    td_eml->configure_call(  )->for_read(  )->when_input( input )->then_set_output( output ).

    "Calling method (containing the ABAP EML read request) of the class under test
    DATA(t_read_res_ro) = cut->eml_read_root( key = 2 ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
        act = lines( t_read_res_ro )
        exp = 1
        msg = `The number of lines does not match`
        quit = if_abap_unit_constant=>quit-no ).

    TYPES struc_read_ro TYPE STRUCTURE FOR READ RESULT zdemo_abap_rap_ro_m.
    cl_abap_unit_assert=>assert_equals(
              act = t_read_res_ro[ 1 ]
              exp = VALUE struc_read_ro( key_field = 2 field1 = 'uuu' field2 = 'vvv' field3 = 20 field4 = 200  )
              msg = `The values do not match the expected result`
              quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

  METHOD test_tdf_constr_inj_get_discnt.
    "--- 1a) ---
    "Method that demonstrates the ABAP OO Test Double Framework
    "Injection mechanism: Constructor injection

    "Creating a test double
    DATA(test_double) = CAST zcl_demo_abap_unit_dataprov(
      cl_abap_testdouble=>create( 'zcl_demo_abap_unit_dataprov' ) ).

    "Configuring a method call
    "It is used for the next method call (the actual method calling)
    "In this example, the method only has a returning parameter. Check
    "the class documentation for more static methods of the cl_abap_testdouble
    "class, e.g. for configuring other parameters.
    cl_abap_testdouble=>configure_call( test_double
      )->returning( 5 ).

    "Calling method of the external class
    test_double->get_discount( ).

    "Injecting the test double
    "Here, the instance constructor is provided with the test double
    "instance
    cut = NEW #( test_double ).

    "Calling method of the class under test
    DATA(result) = cut->td_constr_inj_calc_discount( 500 ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = 475
      msg = `The values do not match the expected result`
      quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

  METHOD test_tdf_param_inj_get_discnt.
    "--- 1b) ---
    "Method that demonstrates the ABAP OO Test Double Framework
    "Injection mechanism: Parameter injection

    "Creating a test double
    DATA(test_double_param_inj) = CAST zcl_demo_abap_unit_dataprov(
      cl_abap_testdouble=>create( 'zcl_demo_abap_unit_dataprov' ) ).

    "Configuring a method call, ignoring importing parameters
    cl_abap_testdouble=>configure_call( test_double_param_inj
      )->ignore_parameter( 'DAY_VALUE'
      )->ignore_parameter( 'TIME_VALUE'
      )->returning( 40 ).

    "Calling method of the external class
    "Dummy values are provided for the non-optional importing
    "parameters (the returning value is determined above)
    test_double_param_inj->get_discount_value(
      EXPORTING
        day_value      = 99
        time_value     = 99
    ).

    "Calling method of the class under test
    "In this case, the optional parameter is assigned the
    "test double replacing the DOC
    cut->td_param_inj_calc_discount(
      EXPORTING
        value     = 100
        date     = '20241201' "Sunday
        time     = '100000'
        data_prov = test_double_param_inj
      IMPORTING
        message   = DATA(msg)
        weekday   = DATA(weekday)
      RECEIVING
        result    = DATA(result)
    ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = 60
      msg = `The values do not match the expected result`
      quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

  METHOD test_eml_rba.
    "--- 4a) ---
    "Method that demonstrates mocking an ABAP EML API (read-by-association request)

    "Preparing test data (RAP BO instances)
    DATA read_tab_ch_import TYPE TABLE FOR READ IMPORT zdemo_abap_rap_ro_m\_child.
    DATA read_tab_ch_result TYPE TABLE FOR READ RESULT zdemo_abap_rap_ro_m\_child.

    read_tab_ch_import = VALUE #( ( key_field = 2 ) ).

    read_tab_ch_result = VALUE #( ( key_field = 2 key_ch = 22 field_ch1 = 'www' field_ch2 = 222  )
                                  ( key_field = 2 key_ch = 23 field_ch1 = 'xxx' field_ch2 = 223  ) ).

    "Configuring input and output for the ABAP EML read-by-association request
    DATA(input_config_rba) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
    DATA(output_config_rba) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    "Creating input for the entities (here, only one entity is included)
    DATA(eml_rba_input) = input_config_rba->build_entity_part( entity
         )->set_instances_for_read_ba( read_tab_ch_import ).

    "Input configuration
    DATA(input_rba) = input_config_rba->build_input_for_eml( )->add_entity_part( eml_rba_input ).

    "Output configuration
    DATA(output_rba) = output_config_rba->build_output_for_eml(
       )->set_result_for_read_ba(
         source_entity_name = entity
         assoc_name         = '_CHILD'
         result             = read_tab_ch_result ).

    "Configuring the RAP BO test double
    td_eml = eml_env->get_test_double( entity ).
    td_eml->configure_call( )->for_read( )->when_input( input_rba )->then_set_output( output_rba ).

    "Calling method (containing the ABAP EML read-by-association request)
    "of the class under test
    DATA(t_read_res_ch) = cut->eml_rba( key = 2 ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
          act = lines( t_read_res_ch )
          exp = 2
          msg = `The number of lines does not match`
          quit = if_abap_unit_constant=>quit-no ).

    TYPES struc_read_ch TYPE STRUCTURE FOR READ RESULT zdemo_abap_rap_ro_m\_child.
    cl_abap_unit_assert=>assert_equals(
              act = t_read_res_ch[ 2 ]
              exp = VALUE struc_read_ch( key_field = 2 key_ch = 23 field_ch1 = 'xxx' field_ch2 = 223  )
              msg = `The values do not match the expected result`
              quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.


  METHOD test_eml_modify_root.
    "--- 4a) ---
    "Method that demonstrates mocking an ABAP EML API (create request)

    "Data object delcarations used in the test method
    DATA create_tab_ro TYPE TABLE FOR CREATE zdemo_abap_rap_ro_m.
    DATA mapped_resp TYPE RESPONSE FOR MAPPED EARLY zdemo_abap_rap_ro_m.
    DATA failed_resp TYPE RESPONSE FOR FAILED EARLY zdemo_abap_rap_ro_m.
    DATA reported_resp TYPE RESPONSE FOR REPORTED EARLY zdemo_abap_rap_ro_m.

    "Preparing test data (RAP BO instances, response parameters)
    create_tab_ro = VALUE #( ( %cid = `cid1` key_field = 3 field1 = 'aaa' field2 = 'bbb' field3 = 30 field4 = 300  )
                             ( %cid = `cid2` key_field = 4 field1 = 'ccc' field2 = 'ddd' field3 = 40 field4 = 400  ) ).

    mapped_resp-root = VALUE #( ( %cid = 'cid1' key_field = 3  ) ( %cid = 'cid2' key_field = 4  ) ).
    failed_resp-root = VALUE #( ).
    reported_resp-root = VALUE #( ).

    "Configuring input and output for the ABAP EML create request
    DATA(input_config_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    DATA(output_config_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Creating input for the entities (here, only one entity is included)
    DATA(eml_modify_input) = input_config_modify->build_entity_part( entity
      )->set_instances_for_create( create_tab_ro ).

    "Input configuration
    DATA(input) = input_config_modify->build_input_for_eml( )->add_entity_part( eml_modify_input ).

    "Output configuration
    DATA(output) = output_config_modify->build_output_for_eml( )->set_mapped( mapped_resp
      )->set_failed( failed_resp )->set_reported( reported_resp ).

    "Configuring the RAP BO test double
    td_eml = eml_env->get_test_double( entity ).
    td_eml->configure_call( )->for_modify(  )->when_input( input )->then_set_output( output ).

    "Calling method (containing the ABAP EML read request) of the class under test
    cut->eml_modify_root(
      EXPORTING
        instances =  VALUE #( ( %cid = `cid1` key_field = 3 field1 = 'aaa' field2 = 'bbb' field3 = 30 field4 = 300  )
                              ( %cid = `cid2` key_field = 4 field1 = 'ccc' field2 = 'ddd' field3 = 40 field4 = 400  ) )
      IMPORTING
        mapped    = DATA(m)
        failed    = DATA(f)
        reported  = DATA(r)
    ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
          act = lines( m-root )
          exp = 2
          msg = `The number of lines does not match`
          quit = if_abap_unit_constant=>quit-no ).

    cl_abap_unit_assert=>assert_equals(
              act = lines( f-root )
              exp = 0
              msg = `The number of lines does not match`
              quit = if_abap_unit_constant=>quit-no ).

    cl_abap_unit_assert=>assert_equals(
              act = lines( r-root )
              exp = 0
              msg = `The number of lines does not match`
              quit = if_abap_unit_constant=>quit-no ).

  ENDMETHOD.

  METHOD test_eml_read_root_buffer_td.
    "--- 4b) ---
    "Method that demonstrates transactional buffer test doubles

    "Creating test double
    "Note: You have more configuration options. Check, for example, the
    "methods configure_additional_behavior, set_fields_handler methods, etc.
    "as described in the documentation.
    DATA(double) =  buffer_env->get_test_double( 'zdemo_abap_rap_ro_u' ).

    "Preparing test data
    "Populating the transactional buffer with an ABAP EML create request
    MODIFY ENTITIES OF zdemo_abap_rap_ro_u
      ENTITY root
       CREATE FIELDS ( key_field field1 field2 field3 field4 )
       WITH VALUE #( ( %cid = `cid5` key_field = 5 field1 = 'eee' )
                     ( %cid = `cid6` key_field = 6 field1 = 'fff' ) )
     REPORTED DATA(reported)
     FAILED DATA(failed)
     MAPPED DATA(mapped).

    "Calling method (containing an ABAP EML read request) of the class under test
    cut->eml_read_root_buffer_td(
      EXPORTING
        key    = 5
      RECEIVING
        tab_ro_u = DATA(read_result) ).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
              act = read_result[ 1 ]-key_field
              exp = 5
              msg = `The value does not match the expected result`
              quit = if_abap_unit_constant=>quit-no ).

    cl_abap_unit_assert=>assert_equals(
                  act = read_result[ 1 ]-field1
                  exp = 'eee'
                  msg = `The value does not match the expected result`
                  quit = if_abap_unit_constant=>quit-no ).

    "Another calling of the method of the class under test (non-existent instance
    "in the transactional buffer test double)
    cut->eml_read_root_buffer_td(
      EXPORTING
        key    = 1
      RECEIVING
        tab_ro_u = DATA(read_result_f) ).

    cl_abap_unit_assert=>assert_initial(
      EXPORTING
        act  = read_result_f
        msg = `An instance was found`
        quit = if_abap_unit_constant=>quit-no ).

  ENDMETHOD.

  METHOD test_cds_standalone.
    "--- 3) ---
    "Method that tests the implementation logic of a CDS view entity
    "Unlike the 'test_select_cds' method, this method does not call a method
    "in the class under test.

    "Preparing test data
    prepare_testdata_set_cds( ).

    "Retrieving data from the CDS view entity (test data is used here)
    SELECT * FROM zdemo_abap_cds_ve_agg_exp INTO TABLE @DATA(result).

    "Verifying the result
    cl_abap_unit_assert=>assert_equals(
               act = result[ 1 ]-carrid
               exp = 'XX'
               msg = `The value does not match the expected result`
               quit = if_abap_unit_constant=>quit-no ).

    cl_abap_unit_assert=>assert_equals(
                      act = result[ 1 ]-avg_seats_occ
                      exp = '411.75'
                      msg = `The value does not match the expected result`
                      quit = if_abap_unit_constant=>quit-no ).

    cl_abap_unit_assert=>assert_equals(
                      act = result[ 1 ]-max_occ_seats
                      exp = 458
                      msg = `The value does not match the expected result`
                      quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

ENDCLASS.
```

**Class 2 `ZCL_DEMO_ABAP_UNIT_DATAPROV` (class representing a DOC)**

Insert the following code in the `Global Class` tab

```abap
CLASS zcl_demo_abap_unit_dataprov DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

* -------------------------- NOTE ----------------------------------
* This is an example class that represents a dependent-on-component
* (DOC). Methods of this class are called in another class. There,
* the DOCs are replaced by test doubles when running ABAP Unit tests.

    METHODS get_discount RETURNING VALUE(discount) TYPE decfloat34.
    METHODS get_discount_value IMPORTING day_value             TYPE i
                                         time_value            TYPE i
                               RETURNING VALUE(discount_value) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_unit_dataprov IMPLEMENTATION.
  METHOD get_discount.
    "Getting the weekday
    "1) Monday, 2) Tuesday, 3) Wednesday, 4) Thursday, 5) Friday, 6) Saturday, 7) Sunday
    DATA(weekday) = ( 5 + CONV d( xco_cp=>sy->date( xco_cp_time=>time_zone->utc
      )->as( xco_cp_time=>format->iso_8601_basic )->value ) MOD 7 ) MOD 7 + 1.

    "- Standard discount is granted at the weekend (Saturday, Sunday)
    "- On other weekdays, discount is granted depending on the daytime
    IF weekday = 6 OR weekday = 7.
      discount = '20'.
    ELSE.
      "Retrieving the current time in UTC
      DATA(utc_time) = CONV t( xco_cp=>sy->time( xco_cp_time=>time_zone->utc
         )->as( xco_cp_time=>format->iso_8601_basic )->value ).

      discount = COND #(  WHEN utc_time BETWEEN '000000' AND '045959' THEN '15' "Night discount
                          WHEN utc_time BETWEEN '220000' AND '235959' THEN '15' "Night discount
                          WHEN utc_time BETWEEN '050000' AND '115959' THEN '10' "Morning discount
                          WHEN utc_time BETWEEN '180000' AND '215959' THEN '5'  "Evening discount
                          ELSE 0                                                "No discount
                       ).
    ENDIF.
  ENDMETHOD.

  METHOD get_discount_value.
    CASE day_value.
        "Standard discount is granted at the weekend (Saturday, Sunday)
      WHEN 6 OR 7.
        discount_value = '20'.
        "On other weekdays, discount is granted depending on the daytime
      WHEN OTHERS.
        discount_value = SWITCH #( time_value
                                   WHEN 1 THEN '15' "Night discount
                                   WHEN 2 THEN '10' "Morning discount
                                   WHEN 3 THEN '5'  "Evening discount
                                   ELSE '0'         "No discount
         ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
```


</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Running and Evaluating ABAP Unit Tests
There are many ways to run ABAP unit tests as described [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/4ec4c6c66e391014adc9fffe4e204223.html?locale=en-US).

The focus of this cheat sheet is on running individual tests in a class that can be run directly in ADT. In your class in ADT (for example, in the class of the demonstration example), choose `Ctrl + Shift + F10` to run all tests in a class. You can also right-click anywhere in the code of the class and choose *Run as → ABAP Unit Test*. To run individual test classes or methods, place the cursor on the class/method name and run the unit test.

The results of a test run are displayed and can be evaluated in the *ABAP Unit* tab in ADT. The *Failure Trace* section provides information about any errors found.

If you are interested in the test coverage, you can choose `Ctrl + Shift + F11`, or make a right-click, choose *Run as → ABAP Unit Test With...*, select the *Coverage* checkbox and choose *Execute*. You can then check the results in the *ABAP Coverage* tab in ADT and see what code was tested and what was not. 

For more information about evaluating ABAP unit test results, see [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/4ec49c5b6e391014adc9fffe4e204223.html?locale=en-US).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information

- openSAP course: [Writing Testable Code for ABAP](https://open.sap.com/courses/wtc1.OpenSAP+WTC1_W1U5+Writing+Testable+Code+for+ABAPComent)
- ABAP Keyword Documentation
  - [ABAP Unit](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_unit.htm)
  - [Testing repository objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentest_relations.htm)
- SAP Help Portal: 
  - [Unit Testing with ABAP Unit](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/08c60b52cb85444ea3069779274b43db.html?locale=en-US)
  - [ABAP Unit](https://help.sap.com/docs/ABAP_PLATFORM_NEW/ba879a6e2ea04d9bb94c7ccd7cdac446/491cfd8926bc14cde10000000a42189b.html?locale=en-US)


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

[zcl_demo_abap_unit_test](./src/zcl_demo_abap_unit_test.clas.abap)

> **💡 Note**<br>
> - The executable example ...
>   - covers the following topics:
>     - Test classes and test/special methods
>     - Implementing and injecting test doubles (constructor injection, back door injection, test seams)
>   - contains comments in the code for more information.
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)