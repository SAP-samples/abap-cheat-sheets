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
  - [Using ABAP Frameworks](#using-abap-frameworks)
  - [Running and Evaluating ABAP Unit Tests](#running-and-evaluating-abap-unit-tests)
  - [More Information](#more-information)
  - [Executable Examples](#executable-examples)
    - [unit\_tests Branch](#unit_tests-branch)
    - [main Branch](#main-branch)
 

This cheat sheet contains basic information about [unit testing](https://help.sap.com/docs/abap-cloud/abap-keyword/unit-test) in ABAP.

> [!NOTE]
> - This cheat sheet focuses on testing methods. 
> - See the [More Information](#more-information) section for links to more in-depth information.
> - The executable examples are **not** suitable role models for ABAP unit tests. They are intended to give you a rough idea. You should always work out your own solution for each individual case.

## Unit Tests in ABAP 
- Unit tests  ...
  - ensure the functional correctness of individual software units (i.e. a unit of code whose execution has a verifiable effect). 
  - are designed to test that the individual components of a larger software unit work correctly during the development and quality assurance phases. Typically, such individual software units are methods (the focus of this cheat sheet).
  - must be created and run by developers.
- In ABAP, developers have [ABAP Unit](https://help.sap.com/docs/abap-cloud/abap-keyword/abap-unit-abenabap_unit_glosry) - a test tool integrated into the ABAP runtime framework - at their disposal. It can be used to run individual or mass tests, and to evaluate test results. Note that comprehensive test runs can be performed using the [ABAP Test Cockpit](https://help.sap.com/docs/abap-cloud/abap-keyword/abap-test-cockpit)
- In ABAP programs, individual unit tests are implemented as [test methods](https://help.sap.com/docs/abap-cloud/abap-keyword/test-method) of local [test classes](https://help.sap.com/docs/abap-cloud/abap-keyword/test-class). 

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

> [!NOTE]
> In some examples, the code does not have any dependent-on components. Therefore, the considerations about dependency isolation, test doubles, and their injection into the code are skipped.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Creating Test Classes

Before we look at test doubles and injections, we will look at the creation of the test classes and methods to get an idea of the code skeletons (in which test doubles and injections can be implemented). 

Test classes ...
- are special [local](https://help.sap.com/docs/abap-cloud/abap-keyword/local-class) or [global classes](https://help.sap.com/docs/abap-cloud/abap-keyword/global-class) in which tests for ABAP Unit are implemented in the form of test methods. 
  - You can define a test relation between a test class or a test method and another repository object using the ABAP Doc comment `"! @testing ...`. This is demonstrated by an [executable example](#executable-examples) class.
- are created in [class pools](https://help.sap.com/docs/abap-cloud/abap-keyword/class-pool-abenclass_pool_glosry) in special [test includes](https://help.sap.com/docs/abap-cloud/abap-keyword/test-include). See the *Test Classes* tab in the ADT. 
- can only be used as part of test runs.
- are not generated in production systems, i.e. the source code of a test class is not part of the production code of its program.
- can contain test methods, the special methods for the [fixture](https://help.sap.com/docs/abap-cloud/abap-keyword/fixture), and other components.
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

> [!NOTE]
> - `FOR TESTING` can be used for multiple purposes:
>   - Creating a test class containing test methods
>   - Creating a test double
>   - Creating helper methods to support ABAP unit tests
>   - Note the possible [syntax options](https://help.sap.com/docs/abap-cloud/abap-keyword/class-class-options) before `FOR TESTING` 
> - Optional addition `RISK LEVEL ...`: 
>   - `CRITICAL`: test changes system settings or customizing data (default)
>   - `DANGEROUS`: test changes persistent data
>   - `HARMLESS`: test does not change system settings or persistent data  
> - Optional addition `DURATION ...`:
>   - `SHORT`: execution time of only a few seconds is expected
>   - `MEDIUM`: execution time of about one minute is expected
>   - `LONG`: execution time of more than one minute is expected  
> - To create a class in ADT, type "test" in the "Test Classes" tab and choose `CTRL + SPACE` to display the template suggestions. You can then choose "testClass – Test class (ABAP Unit)". The skeleton of a test class is automatically generated. 

To test protected or private methods, you must declare [friendship](https://help.sap.com/docs/abap-cloud/abap-keyword/friend) with the class to be tested (class under test).
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

If you have multiple test classes in the test include, you can place the friendship declaration for all classes at the top, for example, as follows. Note the [`DEFERRED`](https://help.sap.com/docs/abap-cloud/abap-keyword/class-deferred) addition.

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
- are special [instance methods](https://help.sap.com/docs/abap-cloud/abap-keyword/instance-method) of a test class in which a test is implemented. 
  - As with test classes, the `FOR TESTING` addition also applies to the test method declaration.
  - Note that there are other syntax options, such as [`ABSTRACT` and others](https://help.sap.com/docs/abap-cloud/abap-keyword/methods-for-testing).
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
- Special private methods for implementing the test [fixture](https://help.sap.com/docs/abap-cloud/abap-keyword/fixture), which may include test data and test objects among others, can be included in the local test class.
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

> [!NOTE]
> You can also specify helper methods, for example, for recurring tasks such as the assertions.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Handling Dependencies 

The code snippets above covered test classes and methods in simple contexts without dependent-on components (DOC) in the production code. In more complex cases with dependencies in the production code, there are ways to deal with them. You can create test doubles and inject them into the production code during the test run. 

It is assumed that you have identified the dependent-on components (DOC) in a method in your production code. They were isolated (which may have involved some major rebuilding if the code is not created from scratch and the requirements for unit testing were taken into account, e.g. by creating an interface), and you want to replace them with a test double (by injection) so that your code can be  properly tested without dependencies.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating/Implementing Test Doubles

As recommended, you should ideally have an interface to the DOC. 

There are multiple ways to implement test doubles manually: 
- Interface is available for DOC 
  - You simply create a local test double by implementing interface methods to create test data. See the code snippet in the *Including Interfaces* section.
  - As mentioned above, the `PARTIALLY IMPLEMENTED` addition is useful (and only possible) for interfaces in test classes.
- No interface is available for the DOC
  - You can create your own local interface, adapt your production code accordingly, and implement interface methods.  
- If the DOC is a method in a class that allows inheritance (i.e. it is not defined as `FINAL`), you can inherit from the class and redefine methods for which you need a test double. 

> [!NOTE]
> See information and an example about frameworks such as the [ABAP OO Test Double Framework](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/804c251e9c19426cadd1395978d3f17b.html?locale=en-US) below that support you with creating the test doubles.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Injecting Test Doubles

As described [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/04a2d0fc9cd940db8aedf3fa29e5f07e.html?locale=en-US), there are multiple techniques for injecting test doubles to ensure that the test doubles are used during the test run. 

Among them, there are the following. They are demonstrated in the executable example. Check the code and comments in the [global class](./src/zcl_demo_abap_unit_test.clas.abap) and [test include](./src/zcl_demo_abap_unit_test.clas.testclasses.abap) of the example.
- Constructor injection: The test double is passed as a parameter to the instance constructor `constructor` of the class under test.
- Setter injection: The test double is passed as a parameter to a setter method.
- Parameter injection: The test double is passed as a parameter to the tested method (i.e. an optional importing parameter) in the class under test. 
- Back door injection: A *back door* is created to inject a test double into the class under test. This *back door* is implemented by granting [friendship](https://help.sap.com/docs/abap-cloud/abap-keyword/friend) to the test class. This makes internal attributes of the class under test accessible from the test class.
 
<p align="right"><a href="#top">⬆️ back to top</a></p>

### Test Seams
- Seams are sections of the production source code that can be dynamically included or replaced. 
- In ABAP, [test seams](https://help.sap.com/docs/abap-cloud/abap-keyword/test-seam-abentest_seam_glosry) can be used to replace source code in the production code by an injection when running unit tests. For more informarion, see [here](https://help.sap.com/docs/abap-cloud/abap-keyword/test-seam).
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

## Using ABAP Frameworks

The previous sections covered self-made test doubles. You can use ABAP frameworks, among others, to create and inject test doubles, providing a standardized method for replacing dependent-on components (DOC) during unit tests. These DOCs can include classes, database tables/CDS views, RAP business objects, and more.

Several frameworks are available and demonstrated in the ABAP cheat sheet example classes. For more details, refer to the [documentation](https://help.sap.com/docs/abap-cloud/abap-development-tools-user-guide/managing-dependencies-with-abap-unit). 
Note that the frameworks covered below and in the examples represent a selection rather than a comprehensive list of available frameworks. The examples in the repository's *main* branch and the examples in section [ABAP Unit Demo Examples](#abap-unit-demo-examples) cover a selection, showcasing exemplary uses of these frameworks. 


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

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Running and Evaluating ABAP Unit Tests

There are many ways to run ABAP unit tests as described [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/4ec4c6c66e391014adc9fffe4e204223.html?locale=en-US).

The focus of this cheat sheet is on running individual tests in a class that can be run directly in ADT. In your class in ADT (for example, in the class of the demonstration example), choose `Ctrl + Shift + F10` to run all tests in a class. You can also right-click anywhere in the code of the class and choose *Run as → ABAP Unit Test*. To run individual test classes or methods, place the cursor on the class/method name and run the unit test.

The results of a test run are displayed and can be evaluated in the *ABAP Unit* tab in ADT. The *Failure Trace* section provides information about any errors found.

If you are interested in the test coverage, you can choose `Ctrl + Shift + F11`, or make a right-click, choose *Run as → ABAP Unit Test With...*, select the *Coverage* checkbox and choose *Execute*. You can then check the results in the *ABAP Coverage* tab in ADT and see what code was tested and what was not. 

For more information about evaluating ABAP unit test results, see [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/4ec49c5b6e391014adc9fffe4e204223.html?locale=en-US).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information

- [Writing Testable Code for ABAP](https://learning.sap.com/courses/writing-testable-code-for-abap) course
- ABAP Keyword Documentation
  - [ABAP Unit](https://help.sap.com/docs/abap-cloud/abap-keyword/abap-unit)
  - [Testing repository objects](https://help.sap.com/docs/abap-cloud/abap-keyword/test-relations)
- SAP Help Portal: 
  - [Unit Testing with ABAP Unit](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/08c60b52cb85444ea3069779274b43db.html?locale=en-US)
  - [ABAP Unit](https://help.sap.com/docs/ABAP_PLATFORM_NEW/ba879a6e2ea04d9bb94c7ccd7cdac446/491cfd8926bc14cde10000000a42189b.html?locale=en-US)

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Examples

### unit_tests Branch

The [unit_tests](https://github.com/SAP-samples/abap-cheat-sheets/tree/unit_tests) branch of the ABAP cheat sheet GitHub repository features a selection of simplified ABAP Unit test scenarios across various contexts. 

Unlike the examples included in the `main` branch - they combine various scenarios and the use of ABAP frameworks to reduce the number of artifacts - the examples in the `unit_tests` branch are set up to be explored in individual classes. The code examples in this branch are designed to function independently from those in the `main` branch. Therefore, you can clone this branch without also cloning the main branch.

The following example contexts are covered:

- Testing methods without dependent-on components (DOCs) and without ABAP frameworks
- Testing methods with DOCs and without ABAP frameworks
- Using ABAP frameworks to manage these DOCs (for example, by creating and injecting test doubles):
    - Classes (ABAP OO Test Double Framework)
    - Database (ABAP SQL Test Double Framework)
    - CDS view entity (CDS Test Double Framework)
    - RAP business object (creating transactional buffer test doubles and mocking ABAP EML APIs)
    - Authority check dependencies
    - Function module (Function Module Test Double Framework)
    - Inspecting background processing using bgPF
- Using test seams
- Test classes located in an external class rather than the class being tested, demonstrating the use of the `"!@testing ...` syntax

If you prefer not to clone the branch and instead want to manually implement the examples, refer to the information in the collapsible section below.

<details>
  <summary>🟢 Click to expand for information and example code</summary>

<br>

> [!NOTE]  
> - Several contexts are covered in the ABAP cheat sheet's executable examples, combining various scenarios and the use of ABAP frameworks to reduce the number of artifacts. The ABAP Unit examples here focus on the various contexts in individual classes, independent of artifacts from the ABAP cheat sheet repository.
> - The examples do not claim to represent best practices or model approaches and setups. They serve only to illustrate ABAP Unit aspects and functionality, most of them making use of the available frameworks. Make sure that you create your own solutions.
> - For more information on the frameworks, refer to the ABAP Doc comments in the classes.
> - For simplicity, many example methods used for unit tests are similar or identical across the example classes.


**Prerequisistes**

To explore all/most of the ABAP Unit examples, the following repository objects must be created as a prerequisite. For example, the demo using the ABAP SQL Test Double Framework requires a database table. You can create a local package, such as `ZABAP_DEMO_AUNIT`, and add the repository objects as well as the example classes further down. 


<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

<table>

<tr>
<td> Repository Object </td> <td> Code/Details </td>
</tr>

<tr>
<td> 

Database table `ztaunitflights`

 </td>

 <td> 

``` abap
@EndUserText.label : 'ABAP Unit Demo'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztaunitflights {

  key client : abap.clnt not null;
  key carrid : abap.char(3) not null;
  key connid : abap.numc(4) not null;
  key fldate : abap.datn not null;
  planetype  : abap.char(10);
  seatsmax   : abap.int4;
  seatsocc   : abap.int4;

}
``` 

 </td>
</tr>

<tr>
<td> 

Root view entity `ZRAUNITFLIGHTS`

 </td>

 <td> 

``` abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZRAUNITFLIGHTS
  as select from ztaunitflights
{
  key carrid    as Carrid,
  key connid    as Connid,
  key fldate    as Fldate,
      planetype as Planetype,
      seatsmax  as Seatsmax,
      seatsocc  as Seatsocc
}
``` 

 </td>
</tr>

<tr>
<td> 

BDEF `ZRAUNITFLIGHTS`

 </td>

 <td> 

**Note**: The code for the ABAP behavior pool is available further down.

``` abap
managed implementation in class zbp_ZRAUNITFLIGHTS unique;
strict ( 2 );

define behavior for ZRAUNITFLIGHTS
persistent table ztaunitflights
lock master
authorization master ( none )
{
  create;
  update;
  delete;
  field ( readonly : update ) Carrid, Connid, Fldate;
  action calc_occ_rate result [1] d34n;
  validation val on save { field Seatsmax, Seatsocc; }
}
``` 

 </td>
</tr>

<tr>
<td> 

Interface `zif_demo_aunit_flights`

 </td>

 <td> 

``` abap
INTERFACE zif_demo_aunit_flights
  PUBLIC .

  TYPES t_flight_data TYPE TABLE OF ztaunitflights WITH EMPTY KEY.

  METHODS get_flight_data IMPORTING carrier_id         TYPE ztaunitflights-carrid
                          RETURNING VALUE(flight_data) TYPE t_flight_data.

ENDINTERFACE.
``` 

 </td>
</tr>

<tr>
<td> 

Interface `zif_demo_aunit_price`

 </td>

 <td> 

``` abap
INTERFACE zif_demo_aunit_price
  PUBLIC .

  METHODS get_discount RETURNING VALUE(discount_percentage) TYPE i.

ENDINTERFACE.
``` 

 </td>
</tr>

<tr>
<td> 

Class `zcl_demo_aunit_provider_flight`

 </td>

 <td> 

``` abap
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class represents a data provider class for an ABAP Unit demo. It implements the interface
"! {@link zif_demo_aunit_flights}, which defines a method for retrieving flight data.
CLASS zcl_demo_aunit_provider_flight DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES zif_demo_aunit_flights.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_provider_flight IMPLEMENTATION.

  METHOD zif_demo_aunit_flights~get_flight_data.
    SELECT seatsmax, seatsocc
       FROM ztaunitflights
       WHERE carrid = @carrier_id
       INTO CORRESPONDING FIELDS OF TABLE @flight_data.
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

Class `zcl_demo_aunit_provider_price`

 </td>

 <td> 

``` abap
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class represents a data provider class for an ABAP Unit demo. It implements the
"! interface {@link zif_demo_aunit_price} for providing discount information related to
"! price calculations. It contains methods for retrieving the discount percentage.
CLASS zcl_demo_aunit_provider_price DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES zif_demo_aunit_price.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS discount TYPE i VALUE 10.
ENDCLASS.



CLASS zcl_demo_aunit_provider_price IMPLEMENTATION.
  METHOD zif_demo_aunit_price~get_discount.
    discount_percentage = discount.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

Function module `zfunc_demo_aunit`

 </td>

 <td> 

First, create a function group, for example, `ZFUNC_DEMO_AUNIT_GR`. Then create the function module.
Note that the signature uses an enum type defined in the demo class `zcl_demo_aunit_func_tdf`.

``` abap
FUNCTION zfunc_demo_aunit
  IMPORTING
    num1 TYPE i
    operator TYPE zcl_demo_aunit_func_tdf=>operator
    num2 TYPE i
  EXPORTING
    result TYPE string
  RAISING
    cx_sy_arithmetic_error.





  "Raising zero division exception if both operands are 0.
  IF num1 = 0 AND num2 = 0.
    RAISE EXCEPTION TYPE cx_sy_zerodivide.
  ENDIF.

  result = SWITCH #( operator
                     WHEN zcl_demo_aunit_func_tdf=>add THEN |{ num1 } + { num2 } = { num1 + num2 STYLE = SIMPLE }|
                     WHEN zcl_demo_aunit_func_tdf=>subtract THEN |{ num1 } - { num2 } = { num1 - num2 STYLE = SIMPLE }|
                     WHEN zcl_demo_aunit_func_tdf=>multiply THEN |{ num1 } * { num2 } = { num1 * num2 STYLE = SIMPLE }|
                     WHEN zcl_demo_aunit_func_tdf=>divide THEN |{ num1 } / { num2 } = { CONV decfloat34( num1 / num2 ) STYLE = SIMPLE }| ).
ENDFUNCTION.
``` 

 </td>
</tr>



<tr>
<td> 

Authorization object `ZAUTH_OB`

 </td>

 <td> 

- **Note**: The examples are designed for the _SAP Business AI Platform, ABAP environment_. If you are using an on-premise environment and want to skip creating the demo authorization object and the steps to add a role to your user, you can replace the literal with the demo authorization object in the example class code with `S_DEVELOP` and omit the following (_SAP Business AI Platform, ABAP environment_-related) steps.
- Details regarding the demo authorization object and steps:
  - Object class: CPAE
  - Authorization field ACTVT should be available.
  - Permitted activities: 01 (create or generate), 02 (change), 03 (display), 06 (delete).
  - Note that the examples are designed for the _SAP Business AI Platform, ABAP environment_. If you want to test the examples with authority checks, follow the additional steps. Refer to the implementation details in the [Authorization Checks](25_Authorization_Checks.md) cheat sheet, section [Executable Example (_SAP Business AI Platform, ABAP environment_)](25_Authorization_Checks.md#executable-example-sap-btp-abap-environment). 
  - High-level steps: 
    - Create an IAM app, for example, `ZDEMO_AUTH_IAM`. Use External app as the application type. In the Authorization tab, add the demo object and select ACTVT. After adding it, select all field values for ACTVT, such as create, change, etc. Publish it locally.
    - Create a business catalog, for example, `ZDEMO_BUSINESS_CATALOG`. In the Apps tab, add `ZDEMO_AUTH_IAM_EXT`. Publish it locally.
    - Log in to the system and access the SAP Fiori Launchpad as an administrator. Open the Maintain Business Roles app. Create a business role, e.g., `ZBRAUTHDEMO`, add the created business catalog, and assign it to your user.


 </td>
</tr>

</table>

</details>  

<br>

**Examples**

Expand the following collapsible sections for more information and example code.


<details>
  <summary>🟢 Testing methods without dependent-on components (DOC) and without ABAP frameworks</summary>
  <!-- -->

<br>

- **Class**: `zcl_demo_aunit_no_tdf`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for a method that does not involve any dependent-on component (DOC) and does not use an ABAP framework.
    - The test values are hard-coded.
- **Global class**: 
    - Contains a private calculation method.
    - Takes two integer values and an enumeration type (indicating the operator) to calculate the result.
- **Test class**: 
    - Defines the local test class `ltc_calculate`.
    - Since the method being tested is private, the global class and the local test class are befriended using the `LOCAL FRIENDS` addition.
    - Several test methods assess various operations, including addition, subtraction, multiplication, and division. Edge cases, such as division by zero and arithmetic overflow, are also tested using hard-coded values.


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
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"! A demo method is included for ABAP Unit tests. It does not involve any dependent-on component (DOC).
"! The test class does not use ABAP frameworks for any test doubles. Values, on which the tests are
"! based, are hard-coded.
CLASS zcl_demo_aunit_no_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ENUM arithmetic_operation,
             addition,
             subtraction,
             multiplication,
             division,
           END OF ENUM arithmetic_operation.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate IMPORTING num1          TYPE i
                                num2          TYPE i
                                operation     TYPE arithmetic_operation
                      RETURNING VALUE(result) TYPE string
                      RAISING   cx_sy_arithmetic_error.
ENDCLASS.



CLASS zcl_demo_aunit_no_tdf IMPLEMENTATION.

  METHOD calculate.
    result = SWITCH #( operation WHEN addition THEN |{ num1 + num2 STYLE = SIMPLE }|
                                 WHEN subtraction THEN |{ num1 - num2 STYLE = SIMPLE }|
                                 WHEN multiplication THEN |{ num1 * num2 STYLE = SIMPLE }|
                                 WHEN division THEN |{ num1 / num2 STYLE = SIMPLE }| ).
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
CLASS ltc_calculate DEFINITION DEFERRED.
CLASS zcl_demo_aunit_no_tdf DEFINITION LOCAL FRIENDS ltc_calculate.

CLASS ltc_calculate DEFINITION FINAL FOR TESTING
	DURATION SHORT
	RISK LEVEL HARMLESS.
	PRIVATE SECTION.
		DATA cut TYPE REF TO zcl_demo_aunit_no_tdf.

		METHODS setup.
		METHODS test_addition FOR TESTING RAISING cx_static_check.
		METHODS test_subtraction FOR TESTING RAISING cx_static_check.
		METHODS test_multiplication FOR TESTING RAISING cx_static_check.
		METHODS test_division FOR TESTING RAISING cx_static_check.
		METHODS test_division_by_zero FOR TESTING RAISING cx_static_check.
		METHODS test_overflow FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_calculate IMPLEMENTATION.
	METHOD setup.
		cut = NEW zcl_demo_aunit_no_tdf( ).
	ENDMETHOD.

	METHOD test_addition.
		DATA(result) = cut->calculate(
			num1      = 10
			num2      = 15
			operation = zcl_demo_aunit_no_tdf=>addition ).

		cl_abap_unit_assert=>assert_equals(
			act = result
			exp = `25` ).
	ENDMETHOD.

	METHOD test_subtraction.
		DATA(result) = cut->calculate(
			num1      = 20
			num2      = 7
			operation = zcl_demo_aunit_no_tdf=>subtraction ).

		cl_abap_unit_assert=>assert_equals(
			act = result
			exp = `13` ).
	ENDMETHOD.

	METHOD test_multiplication.
		DATA(result) = cut->calculate(
			num1      = 6
			num2      = 8
			operation = zcl_demo_aunit_no_tdf=>multiplication ).

		cl_abap_unit_assert=>assert_equals(
			act = result
			exp = `48` ).
	ENDMETHOD.

	METHOD test_division.
		DATA(result) = cut->calculate(
			num1      = 42
			num2      = 6
			operation = zcl_demo_aunit_no_tdf=>division ).

		cl_abap_unit_assert=>assert_equals(
			act = result
			exp = `7` ).
	ENDMETHOD.

	METHOD test_division_by_zero.
		TRY.
				cut->calculate(
					num1      = 1
					num2      = 0
					operation = zcl_demo_aunit_no_tdf=>division ).
				cl_abap_unit_assert=>fail( msg = `Expected arithmetic error for division by zero.` ).
			CATCH cx_sy_arithmetic_error.
		ENDTRY.
	ENDMETHOD.

	METHOD test_overflow.
		TRY.
				cut->calculate(
					num1      = 2147483647
					num2      = 1
					operation = zcl_demo_aunit_no_tdf=>addition ).
				cl_abap_unit_assert=>fail( msg = `Expected arithmetic overflow.` ).
			CATCH cx_sy_arithmetic_error.
		ENDTRY.
	ENDMETHOD.
ENDCLASS.

``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 Testing methods with DOC and without ABAP frameworks</summary>
  <!-- -->

<br>

- **Class**: `zcl_demo_aunit_no_tdf_doc`
- **Purpose**:
    - Demonstrates ABAP Unit tests for methods that involve dependent-on components (DOCs) without using an ABAP test double framework.
    - Uses self-created local test doubles and hard-coded test data.
- **Global class**:
    - Implements constructor-based dependency injection for two DOC interfaces (price provider and flights provider), with default productive providers as fallback options.
    - Provides two methods:
        - Calculating price from the current price and discount (retrieves discount information from the DOC).
        - Computing occupancy rate from flight seat data (retrieves flight data from the DOC).
- **Test class**:
    - Defines local test doubles (`ltd_test_double_discount`, `ltd_test_double_occupancy`) and local test classes (`ltc_calculate_price`, `ltc_occupancy_rate`).
    - Injects the local test doubles into the constructor.
    - Includes multiple hard-coded test scenarios for normal, boundary, invalid, rounding, and no data cases for both price and occupancy calculations.

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
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class is designed to showcase ABAP Unit tests using self-created test doubles for price and flight data providers. It
"! offers methods for calculating price based on discounts and occupancy rates from flight data. The class supports
"! constructor-based dependency injection to handle dependencies.
CLASS zcl_demo_aunit_no_tdf_doc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    "! Initializes the class with optional flight and price providers or defaults to the productive ones
    "!
    "! @parameter flights | <p class="shorttext synchronized" lang="en">Reference to the flights provider interface</p>
    "! @parameter price   | <p class="shorttext synchronized" lang="en">Reference to the price provider interface</p>
    METHODS constructor
      IMPORTING
        flights TYPE REF TO zif_demo_aunit_flights OPTIONAL
        price   TYPE REF TO zif_demo_aunit_price OPTIONAL.

    "! Calculates the final price based on current price and applicable discount
    "!
    "! @parameter current_price | <p class="shorttext synchronized" lang="en">The original price before discount</p>
    "! @parameter final_price   | <p class="shorttext synchronized" lang="en">The calculated final price after discount</p>
    METHODS calculate_price IMPORTING current_price      TYPE decfloat34
                            RETURNING VALUE(final_price) TYPE decfloat34.

    "! Calculates the occupancy rate based on flight data for a specific carrier
    "!
    "! @parameter carrier_id     | <p class="shorttext synchronized" lang="en">Identifier for the specific airline carrier</p>
    "! @parameter occupancy_rate | <p class="shorttext synchronized" lang="en">The calculated occupancy rate as a percentage</p>
    METHODS calculate_occupancy_rate IMPORTING carrier_id            TYPE ztaunitflights-carrid
                                     RETURNING VALUE(occupancy_rate) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA data_prov_flights TYPE REF TO zif_demo_aunit_flights.
    DATA data_prov_price TYPE REF TO zif_demo_aunit_price.
ENDCLASS.



CLASS zcl_demo_aunit_no_tdf_doc IMPLEMENTATION.
  METHOD constructor.
    data_prov_flights = COND #( WHEN flights IS BOUND THEN flights
                                ELSE NEW zcl_demo_aunit_provider_flight( ) ).
    data_prov_price = COND #( WHEN price IS BOUND THEN price
                              ELSE NEW zcl_demo_aunit_provider_price( ) ).
  ENDMETHOD.

  METHOD calculate_price.
    DATA(discount_percentage) = data_prov_price->get_discount( ).
    DATA(discount_factor) = CONV decfloat34( 1 - ( discount_percentage / 100 ) ).
    final_price = COND #( WHEN discount_factor < 0 OR discount_factor > 1
                          THEN round( val = current_price dec = 2 )
                          ELSE round( val = current_price * discount_factor dec = 2 ) ).
  ENDMETHOD.

  METHOD calculate_occupancy_rate.
    DATA(flight_data) = data_prov_flights->get_flight_data( carrier_id ).

    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.

    LOOP AT flight_data ASSIGNING FIELD-SYMBOL(<flight>).
      total_seatsmax += <flight>-seatsmax.
      total_seatsocc += <flight>-seatsocc.
    ENDLOOP.

    IF total_seatsmax <> 0.
      occupancy_rate = round( val = total_seatsocc / total_seatsmax * 100 dec = 2 ).
    ENDIF.
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
"! <p class="shorttext synchronized" lang="en">Local test double for discount retrieval</p>
"! This local test double provides the discount values needed for price calculation.
"! It partially implements the provider interface and uses a constructor importing parameter
"! to set the discount value. In this simplified setup, the value passed during object creation
"! is stored in a private attribute, and the get_discount implementation returns it.
"! This allows each test to create an object with a specific discount and inject it into the
"! code under test, eliminating external dependencies.
CLASS ltd_test_double_discount DEFINITION FOR TESTING
DURATION SHORT RISK LEVEL HARMLESS.
  	PUBLIC SECTION.
    		INTERFACES zif_demo_aunit_price.
    		METHODS constructor IMPORTING discount_value TYPE i.
  	PRIVATE SECTION.
    		DATA discount TYPE i.
ENDCLASS.

CLASS ltd_test_double_discount IMPLEMENTATION.
  METHOD constructor.
    discount = discount_value.
  ENDMETHOD.

  METHOD zif_demo_aunit_price~get_discount.
    discount_percentage = discount.
  ENDMETHOD.
ENDCLASS.

"! <p class="shorttext synchronized" lang="en">Local test class for the calculate_price method</p>
"! This test class contains scenarios that verify calculate_price for normal,
"! boundary, and invalid discount values.
CLASS ltc_calculate_price DEFINITION FINAL FOR TESTING
	DURATION SHORT
	RISK LEVEL HARMLESS.
  	PRIVATE SECTION.
      METHODS calculate_price_with_discount
        IMPORTING
          discount_value   TYPE i
          current_price_in TYPE decfloat34
        RETURNING
          VALUE(final_price) TYPE decfloat34.
      METHODS assert_price
        IMPORTING
          act_price TYPE decfloat34
          exp_price TYPE decfloat34
          msg       TYPE string.
    		METHODS test_discount_15 FOR TESTING.
    		METHODS test_discount_0 FOR TESTING.
    		METHODS test_discount_100 FOR TESTING.
    		METHODS test_discount_negative FOR TESTING.
    		METHODS test_discount_over_100 FOR TESTING.
    		METHODS test_rounding_2_dec_a FOR TESTING.
    		METHODS test_rounding_2_dec_b FOR TESTING.
ENDCLASS.

CLASS ltc_calculate_price IMPLEMENTATION.
  METHOD calculate_price_with_discount.
    DATA(cut) = NEW zcl_demo_aunit_no_tdf_doc(
      price = NEW ltd_test_double_discount( discount_value = discount_value ) ).

    final_price = cut->calculate_price( current_price = current_price_in ).
  ENDMETHOD.

  METHOD assert_price.
    cl_abap_unit_assert=>assert_equals(
      act = act_price
      exp = exp_price
      msg = msg ).
  ENDMETHOD.

  	METHOD test_discount_15.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 15
          current_price_in = CONV decfloat34( '200.40' ) )
        exp_price = CONV decfloat34( '170.34' )
        msg       = '15 percent discount should be applied.' ).
  	ENDMETHOD.

  	METHOD test_discount_0.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 0
          current_price_in = CONV decfloat34( '123.45' ) )
        exp_price = CONV decfloat34( '123.45' )
        msg       = '0 percent discount should keep current price unchanged.' ).
  	ENDMETHOD.

  	METHOD test_discount_100.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 100
          current_price_in = CONV decfloat34( '89.99' ) )
        exp_price = CONV decfloat34( '0.00' )
        msg       = '100 percent discount should result in zero.' ).
  	ENDMETHOD.

  	METHOD test_discount_negative.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = -10
          current_price_in = CONV decfloat34( '50.50' ) )
        exp_price = CONV decfloat34( '50.50' )
        msg       = 'Negative discount should be treated as invalid.' ).
  	ENDMETHOD.

  	METHOD test_discount_over_100.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 150
          current_price_in = CONV decfloat34( '75.25' ) )
        exp_price = CONV decfloat34( '75.25' )
        msg       = 'Discount above 100 should be treated as invalid.' ).
  	ENDMETHOD.

  	METHOD test_rounding_2_dec_a.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 10
          current_price_in = CONV decfloat34( '99.995' ) )
        exp_price = CONV decfloat34( '90.00' )
        msg       = 'Result should be rounded to 2 decimals.' ).
  	ENDMETHOD.
  	
  	METHOD test_rounding_2_dec_b.
      assert_price(
        act_price = calculate_price_with_discount(
          discount_value   = 0
          current_price_in = CONV decfloat34( '99.958' ) )
        exp_price = CONV decfloat34( '99.96' )
        msg       = 'Rounding should also work with zero discount.' ).
  ENDMETHOD.	
ENDCLASS.

**********************************************************************

"! <p class="shorttext synchronized" lang="en">Local test double for flight data</p>
"! This local test double provides a manually created dataset for the code under test.
"! It populates a private internal table in the constructor and filters it by carrier
"! in the get_flight_data method. This way, tests can run with stable data and no
"! external dependencies.
CLASS ltd_test_double_occupancy DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    INTERFACES zif_demo_aunit_flights PARTIALLY IMPLEMENTED.
    METHODS constructor.
  PRIVATE SECTION.
    DATA flights TYPE zif_demo_aunit_flights=>t_flight_data.
ENDCLASS.

CLASS ltd_test_double_occupancy IMPLEMENTATION.
  METHOD constructor.

    flights = VALUE #( ( carrid = 'AA' seatsmax = 180 seatsocc = 135 )
                       ( carrid = 'AA' seatsmax = 220 seatsocc = 198 )
                       ( carrid = 'AA' seatsmax = 300 seatsocc = 280 )
                       ( carrid = 'BB' seatsmax = 150 seatsocc = 150 )
                       ( carrid = 'BB' seatsmax = 120 seatsocc = 120 )
                       ( carrid = 'CC' seatsmax = 100 seatsocc = 0 )
                       ( carrid = 'CC' seatsmax = 50 seatsocc = 0 )
                       ( carrid = 'DD' seatsmax = 3 seatsocc = 1 ) ).

  ENDMETHOD.

  METHOD zif_demo_aunit_flights~get_flight_data.
    LOOP AT flights ASSIGNING FIELD-SYMBOL(<flight>) WHERE carrid = carrier_id.
      APPEND <flight> TO flight_data.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

"! <p class="shorttext synchronized" lang="en">Local test class for the calculate_occupancy_rate method</p>
"! This test class contains scenarios that verify calculate_occupancy_rate for
"! full occupancy, empty occupancy, mixed loads, rounding behavior, and missing data,
"! using local test data.
"! The code-under-test reference is created once in setup with the occupancy test double.
CLASS ltc_occupancy_rate DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_demo_aunit_no_tdf_doc.

    METHODS setup.
    METHODS test_rate FOR TESTING.
    METHODS test_rate_full FOR TESTING.
    METHODS test_rate_zero FOR TESTING.
    METHODS test_rate_round FOR TESTING.
    METHODS test_rate_no_data FOR TESTING.
ENDCLASS.

CLASS ltc_occupancy_rate IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_demo_aunit_no_tdf_doc( flights = NEW ltd_test_double_occupancy( ) ).
  ENDMETHOD.

  METHOD test_rate.
    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'AA' )
      exp = CONV decfloat34( '87.57' )
      msg = 'AA occupancy rate should be 87.57.' ).
  ENDMETHOD.

  METHOD test_rate_full.
    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'BB' )
      exp = CONV decfloat34( '100' )
      msg = 'BB occupancy rate should be 100.' ).
  ENDMETHOD.

  METHOD test_rate_zero.
    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'CC' )
      exp = CONV decfloat34( '0' )
      msg = 'CC occupancy rate should be 0.' ).
  ENDMETHOD.

  METHOD test_rate_round.
    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'DD' )
      exp = CONV decfloat34( '33.33' )
      msg = 'DD occupancy rate should be rounded to 33.33.' ).
  ENDMETHOD.

  METHOD test_rate_no_data.
    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'XX' )
      exp = CONV decfloat34( '0' )
      msg = 'Missing carrier should return 0 occupancy rate.' ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 Classes (ABAP OO Test Double Framework)</summary>
  <!-- -->

<br>

- **Class**: `zcl_demo_aunit_abap_oo_tdf`
- **Purpose**:
    - Demonstrates ABAP Unit tests with a DOC using the ABAP OO Test Double Framework.
    - Illustrates constructor injection to replace a dependency during testing.
- **Global class**:
    - Contains constructor-based injection of a discount provider interface (`zif_demo_aunit_price`), with a default fallback for the productive provider.
    - Implements price calculation from the current price and discount.
- **Test class**:
    - Defines the local test class that creates and configures an ABAP OO framework test double for the discount provider interface.
    - Injects the configured test double into the class under test and verifies outcomes.
    - Asserts normal, boundary, invalid, and rounding scenarios.

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
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class is used to demonstrate ABAP Unit tests using the ABAP Object-Oriented Test Double Framework. It provides a constructor
"! for dependency injection and a method to calculate prices based on current prices and discounts, utilizing a discount provider
"! interface.
CLASS zcl_demo_aunit_abap_oo_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    "! Initializes the data provider with a fallback to a default provider
    "!
    "! @parameter iref_data_provider | <p class="shorttext synchronized" lang="en">Reference to the data provider interface for discounts</p>
    METHODS constructor
      IMPORTING
        iref_data_provider TYPE REF TO zif_demo_aunit_price.

    "! <p class="shorttext synchronized" lang="en">Calculates the final price after applying a discount</p>
    "!
    "! @parameter current_price | <p class="shorttext synchronized" lang="en">Current price before discount</p>
    "! @parameter final_price   | <p class="shorttext synchronized" lang="en">Final price after discount calculations</p>
    METHODS calculate_price IMPORTING current_price      TYPE decfloat34
                            RETURNING VALUE(final_price) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA data_prov TYPE REF TO zif_demo_aunit_price.
ENDCLASS.



CLASS zcl_demo_aunit_abap_oo_tdf IMPLEMENTATION.
  METHOD constructor.
    data_prov = COND #( WHEN iref_data_provider IS BOUND THEN iref_data_provider
                        ELSE NEW zcl_demo_aunit_provider_price( ) ).
  ENDMETHOD.

  METHOD calculate_price.
    DATA(discount_percentage) = data_prov->get_discount( ).
    DATA(discount_factor) = CONV decfloat34( 1 - ( discount_percentage / 100 ) ).
    final_price = COND #( WHEN discount_factor < 0 OR discount_factor > 1
                          THEN round( val = current_price dec = 2 )
                          ELSE round( val = current_price * discount_factor dec = 2 ) ).
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
"! <p class="shorttext synchronized" lang="en">Local test class for the calculate_price method</p>
"! This test class contains scenarios that verify calculate_price for normal,
"! boundary, and invalid discount values.
"! The test method implementations demonstrate the ABAP OO Test Double Framework.
"! The injection mechanism used in the example is constructor injection.
CLASS ltc_calculate_price DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS setup.
    METHODS calculate_with_injected_double
      IMPORTING
        discount_percentage TYPE i
        current_price       TYPE decfloat34
      RETURNING
        VALUE(result)       TYPE decfloat34.

    METHODS assert_price
      IMPORTING
        act_price TYPE decfloat34
        exp_price TYPE decfloat34.

    METHODS test_discount_15 FOR TESTING.
    METHODS test_discount_0 FOR TESTING.
    METHODS test_discount_100 FOR TESTING.
    METHODS test_discount_negative FOR TESTING.
    METHODS test_discount_over_100 FOR TESTING.
    METHODS test_rounding_2_dec_a FOR TESTING.
    METHODS test_rounding_2_dec_b FOR TESTING.

    DATA test_double TYPE REF TO zif_demo_aunit_price.
ENDCLASS.


CLASS ltc_calculate_price IMPLEMENTATION.
  METHOD setup.
    "Creating a test double
    test_double = CAST zif_demo_aunit_price( cl_abap_testdouble=>create( 'zif_demo_aunit_price' ) ).
  ENDMETHOD.

  METHOD calculate_with_injected_double.
    cl_abap_testdouble=>configure_call( test_double )->returning( discount_percentage ).

    test_double->get_discount( ).

    DATA(cut) = NEW zcl_demo_aunit_abap_oo_tdf( test_double ).

    result = cut->calculate_price( current_price = current_price ).
  ENDMETHOD.

  METHOD assert_price.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = act_price
        exp = exp_price ).
  ENDMETHOD.

  METHOD test_discount_15.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 15
      current_price       = CONV decfloat34( '200.40' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '170.34' ) ).
  ENDMETHOD.

  METHOD test_discount_0.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 0
      current_price       = CONV decfloat34( '123.45' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '123.45' ) ).
  ENDMETHOD.

  METHOD test_discount_100.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 100
      current_price       = CONV decfloat34( '89.99' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '0.00' ) ).
  ENDMETHOD.

  METHOD test_discount_negative.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = -10
      current_price       = CONV decfloat34( '50.50' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '50.50' ) ).
  ENDMETHOD.

  METHOD test_discount_over_100.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 150
      current_price       = CONV decfloat34( '75.25' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '75.25' ) ).
  ENDMETHOD.

  METHOD test_rounding_2_dec_a.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 10
      current_price       = CONV decfloat34( '99.995' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '90.00' ) ).
  ENDMETHOD.

  METHOD test_rounding_2_dec_b.
    DATA(result) = calculate_with_injected_double(
      discount_percentage = 0
      current_price       = CONV decfloat34( '99.958' ) ).

    assert_price(
      act_price = result
      exp_price = CONV decfloat34( '99.96' ) ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 Database (ABAP SQL Test Double Framework)</summary>
  <!-- -->

<br>

- **Class**: `zcl_demo_aunit_sql_tdf`
- **Purpose**:
    - Demonstrates ABAP Unit tests for database-dependent logic using the ABAP SQL Test Double Framework.
    - Shows testing of calculations without relying on productive database table data.
- **Global class**:
    - Contains a method that reads seat data from a database table for a carrier, aggregates totals, and calculates the rounded occupancy rate.    
- **Test class**:
    - Defines a local test class that creates an SQL test environment for the database table `ztaunitflights`.
    - Clears doubles per test, injects predefined table data, and verifies expected occupancy outcomes.
    - Asserts normal, full, zero, rounding, non-existing carrier, and no data scenarios.

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
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class demonstrates ABAP Unit tests for database-dependent logic without relying on productive database table data. It
"! contains a method that reads seat data from a database table for a specified carrier, aggregates seat totals, and calculates the
"! rounded occupancy rate.
CLASS zcl_demo_aunit_sql_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Calculates the occupancy rate for a given carrier</p>
    "!
    "! @parameter carrier_id     | <p class="shorttext synchronized" lang="en">ID of the carrier for which to calculate occupancy</p>
    "! @parameter occupancy_rate | <p class="shorttext synchronized" lang="en">Calculated occupancy rate as a decimal value</p>
    METHODS calculate_occupancy_rate IMPORTING carrier_id            TYPE ztaunitflights-carrid
                                     RETURNING VALUE(occupancy_rate) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_sql_tdf IMPLEMENTATION.
  METHOD calculate_occupancy_rate.
    SELECT seatsmax, seatsocc
     FROM ztaunitflights
     WHERE carrid = @carrier_id
     INTO TABLE @DATA(flight_data).

    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.

    LOOP AT flight_data ASSIGNING FIELD-SYMBOL(<flight>).
      total_seatsmax += <flight>-seatsmax.
      total_seatsocc += <flight>-seatsocc.
    ENDLOOP.

    IF total_seatsmax <> 0.
      occupancy_rate = round( val = total_seatsocc / total_seatsmax * 100 dec = 2 ).
    ENDIF.
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
CLASS ltc_occupancy_rate DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA cut TYPE REF TO zcl_demo_aunit_sql_tdf.
    CLASS-DATA sql_env TYPE REF TO if_osql_test_environment.
    CLASS-DATA flights_tab TYPE zif_demo_aunit_flights=>t_flight_data.

    CLASS-METHODS class_setup.
    METHODS setup.
    CLASS-METHODS class_teardown.
    METHODS test_rate FOR TESTING.
    METHODS test_rate_full FOR TESTING.
    METHODS test_rate_zero FOR TESTING.
    METHODS test_rate_round FOR TESTING.
    METHODS test_rate_non_existing_carrier FOR TESTING.
    METHODS test_rate_no_data FOR TESTING.
ENDCLASS.

CLASS ltc_occupancy_rate IMPLEMENTATION.

  METHOD class_setup.
    cut = NEW zcl_demo_aunit_sql_tdf( ).

    sql_env = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'ZTAUNITFLIGHTS' ) ) ).

    "Most example test methods use the same dataset on which the tests are based. Therefore, populating
    "the table once on test execution.
    flights_tab = VALUE #(  ( carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
     ( carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 )
     ( carrid = 'AA' connid = '1003' fldate = '20260803' seatsmax = 300 seatsocc = 280 )
     ( carrid = 'BB' connid = '2001' fldate = '20260801' seatsmax = 150 seatsocc = 150 )
     ( carrid = 'BB' connid = '2002' fldate = '20260802' seatsmax = 120 seatsocc = 120 )
     ( carrid = 'CC' connid = '3001' fldate = '20260801' seatsmax = 100 seatsocc = 0 )
     ( carrid = 'CC' connid = '3002' fldate = '20260802' seatsmax = 50 seatsocc = 0 )
     ( carrid = 'DD' connid = '4001' fldate = '20260801' seatsmax = 3 seatsocc = 1 ) ).

  ENDMETHOD.

  METHOD setup.
    sql_env->clear_doubles( ).
  ENDMETHOD.

  METHOD class_teardown.
    sql_env->destroy( ).
  ENDMETHOD.

  METHOD test_rate.
    sql_env->insert_test_data( flights_tab ).

    DATA(occupancy_rate) = cut->calculate_occupancy_rate( carrier_id = 'AA' ).

    cl_abap_unit_assert=>assert_equals(
     act = occupancy_rate
      exp = CONV decfloat34( '87.57' )
      msg = 'AA occupancy rate should be 87.57.' ).
  ENDMETHOD.

  METHOD test_rate_full.
    sql_env->insert_test_data( flights_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'BB' )
      exp = CONV decfloat34( '100' )
      msg = 'BB occupancy rate should be 100.' ).
  ENDMETHOD.

  METHOD test_rate_zero.
    sql_env->insert_test_data( flights_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'CC' )
      exp = CONV decfloat34( '0' )
      msg = 'CC occupancy rate should be 0.' ).
  ENDMETHOD.

  METHOD test_rate_round.
    sql_env->insert_test_data( flights_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->calculate_occupancy_rate( carrier_id = 'DD' )
      exp = CONV decfloat34( '33.33' )
      msg = 'DD occupancy rate should be rounded to 33.33.' ).
  ENDMETHOD.

  METHOD test_rate_non_existing_carrier.
    sql_env->insert_test_data( flights_tab ).

    cl_abap_unit_assert=>assert_equals(
       act = cut->calculate_occupancy_rate( carrier_id = 'XX' )
       exp = CONV decfloat34( '0' )
       msg = 'Unknown carrier should return 0 occupancy rate.' ).
  ENDMETHOD.

  METHOD test_rate_no_data.
    "No data inserted using the insert_test_data method
    cl_abap_unit_assert=>assert_initial(
      act = cut->calculate_occupancy_rate( carrier_id = 'AA' )
      msg = 'No data should return initial occupancy rate.' ).
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 CDS view entity (CDS Test Double Framework)</summary>
  <!-- -->

<br>

- **Class**: `zcl_demo_aunit_cds_tdf`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for CDS-dependent logic using the ABAP CDS Test Double Framework.
    - Shows testing of calculations without relying on productive data available via a CDS entity.    
- **Global class**: 
    - Contains a method that retrieves flight seat data from CDS entity `zraunitflights`, aggregates totals, and calculates the rounded occupancy rate.    
- **Test class**: 
    - Defines a local test class that creates a CDS test environment for `zraunitflights`.
    - Clears doubles per test, injects test datasets, and asserts expected calculation results.
    - Asserts normal, full, zero, rounding, non-existing carrier, and no data scenarios.


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
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class is used to demonstrate ABAP Unit tests for CDS-dependent logic without relying on productive data. It includes a
"! method for reading flight seat data from the CDS entity zraunitflights and calculating the occupancy rate.
CLASS zcl_demo_aunit_cds_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    "! Calculates the occupancy rate based on total seats and occupied seats
    "!
    "! @parameter carrier_id     | <p class="shorttext synchronized" lang="en">ID of the airline carrier</p>
    "! @parameter occupancy_rate | <p class="shorttext synchronized" lang="en">Calculated occupancy rate as a decimal value</p>
    METHODS calculate_occupancy_rate IMPORTING carrier_id            TYPE zraunitflights-carrid
                                     RETURNING VALUE(occupancy_rate) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_cds_tdf IMPLEMENTATION.
  METHOD calculate_occupancy_rate.
    SELECT seatsmax, seatsocc
     FROM zraunitflights
     WHERE carrid = @carrier_id
     INTO TABLE @DATA(flight_data).

    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.

    LOOP AT flight_data ASSIGNING FIELD-SYMBOL(<flight>).
      total_seatsmax += <flight>-seatsmax.
      total_seatsocc += <flight>-seatsocc.
    ENDLOOP.

    IF total_seatsmax <> 0.
      occupancy_rate = round( val = total_seatsocc / total_seatsmax * 100 dec = 2 ).
    ENDIF.
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
CLASS ltc_occupancy_rate DEFINITION FINAL FOR TESTING
	DURATION SHORT
	RISK LEVEL HARMLESS.
	PRIVATE SECTION.
		CLASS-DATA cds_env TYPE REF TO if_cds_test_environment.
		CLASS-DATA flights_tab TYPE zif_demo_aunit_flights=>t_flight_data.
		DATA cut TYPE REF TO zcl_demo_aunit_cds_tdf.

		CLASS-METHODS class_setup.
		METHODS setup.
		CLASS-METHODS class_teardown.
		METHODS test_rate FOR TESTING.
		METHODS test_rate_full FOR TESTING.
		METHODS test_rate_zero FOR TESTING.
		METHODS test_rate_round FOR TESTING.
		METHODS test_rate_non_existing_carrier FOR TESTING.
		METHODS test_rate_no_data FOR TESTING.
ENDCLASS.

CLASS ltc_occupancy_rate IMPLEMENTATION.

	METHOD class_setup.
		cds_env = cl_cds_test_environment=>create( i_for_entity = 'ZRAUNITFLIGHTS' ).

		"Most example test methods use the same dataset on which the tests are based. Therefore, populating
		"the table once on test execution.
		flights_tab = VALUE #(  ( carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
		 ( carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 )
		 ( carrid = 'AA' connid = '1003' fldate = '20260803' seatsmax = 300 seatsocc = 280 )
		 ( carrid = 'BB' connid = '2001' fldate = '20260801' seatsmax = 150 seatsocc = 150 )
		 ( carrid = 'BB' connid = '2002' fldate = '20260802' seatsmax = 120 seatsocc = 120 )
		 ( carrid = 'CC' connid = '3001' fldate = '20260801' seatsmax = 100 seatsocc = 0 )
		 ( carrid = 'CC' connid = '3002' fldate = '20260802' seatsmax = 50 seatsocc = 0 )
		 ( carrid = 'DD' connid = '4001' fldate = '20260801' seatsmax = 3 seatsocc = 1 ) ).

	ENDMETHOD.

	METHOD setup.
		cut = NEW zcl_demo_aunit_cds_tdf( ).
		cds_env->clear_doubles( ).
	ENDMETHOD.

	METHOD class_teardown.
		cds_env->destroy( ).
	ENDMETHOD.

	METHOD test_rate.
		cds_env->insert_test_data( i_data = flights_tab ).

		DATA(occupancy_rate) = cut->calculate_occupancy_rate( carrier_id = 'AA' ).

		cl_abap_unit_assert=>assert_equals(
		 act = occupancy_rate
		 exp = CONV decfloat34( '87.57' )
		 msg = 'AA occupancy rate should be rounded to 87.57.' ).
	ENDMETHOD.

	METHOD test_rate_full.
		cds_env->insert_test_data( i_data = flights_tab ).

		cl_abap_unit_assert=>assert_equals(
			act = cut->calculate_occupancy_rate( carrier_id = 'BB' )
			exp = CONV decfloat34( '100' )
			msg = 'BB occupancy rate should be 100.' ).
	ENDMETHOD.

	METHOD test_rate_zero.
		cds_env->insert_test_data( i_data = flights_tab ).

		cl_abap_unit_assert=>assert_equals(
			act = cut->calculate_occupancy_rate( carrier_id = 'CC' )
			exp = CONV decfloat34( '0' )
			msg = 'CC occupancy rate should be 0.' ).
	ENDMETHOD.

	METHOD test_rate_round.
		cds_env->insert_test_data( i_data = flights_tab ).

		cl_abap_unit_assert=>assert_equals(
			act = cut->calculate_occupancy_rate( carrier_id = 'DD' )
			exp = CONV decfloat34( '33.33' )
			msg = 'DD occupancy rate should be rounded to 33.33.' ).
	ENDMETHOD.

	METHOD test_rate_non_existing_carrier.
		cds_env->insert_test_data( i_data = flights_tab ).

    cl_abap_unit_assert=>assert_equals(
       act = cut->calculate_occupancy_rate( carrier_id = 'XX' )
       exp = CONV decfloat34( '0' ) ).
	ENDMETHOD.

	METHOD test_rate_no_data.
		"No data inserted using the insert_test_data method
	 cl_abap_unit_assert=>assert_initial( cut->calculate_occupancy_rate( carrier_id = 'AA' ) ).
	ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 RAP business object: Creating transactional buffer test doubles</summary>
  <!-- -->

<br>

- **Class**: `zcl_demo_aunit_rap_buffer`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for RAP BO interaction logic using the RAP transaction buffer test double framework.
    - Shows testing of EML read behavior without productive RAP BO data.
- **Global class**: 
    - Contains a method that reads RAP BO instances via `READ ENTITY` and returns the results along with any failed responses.
    - Adapts each returned flight by deriving `Planetype` from `Seatsmax` using explicit threshold rules (`A`, `B`, `C`).
- **Test class**: 
    - Defines a local test class that creates a RAP transaction buffer BO test environment and retrieves the RAP BO test double.
    - Clears doubles per test, inserts test instances, executes the method under test, and validates the results and failed response behavior.
    - Asserts scenarios with existing keys, mixed existing and non-existing keys, and situations with no data.

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
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class demonstrates Unit tests for RAP BO interaction logic using the RAP transaction buffer test double framework.
"! It shows testing ABAP EML read behavior using a transaction buffer test double framework without productive data.
CLASS zcl_demo_aunit_rap_buffer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    TYPES flights TYPE TABLE FOR READ RESULT zraunitflights.
    TYPES keys TYPE TABLE FOR READ IMPORT zraunitflights.
    TYPES failed_resp TYPE RESPONSE FOR FAILED zraunitflights.

    "! <p class="shorttext synchronized" lang="en">Adapts flight Planetype based on Seatsmax values</p>
    "!
    "! @parameter failed_resp | <p class="shorttext synchronized" lang="en">Response for any failures during read operations</p>
    "! @parameter flights     | <p class="shorttext synchronized" lang="en">Table of flights retrieved from the entity</p>
    "! @parameter keys        | <p class="shorttext synchronized" lang="en">Keys to identify the flights to read</p>
    METHODS adapt_planetype IMPORTING VALUE(keys) TYPE keys
                            EXPORTING flights     TYPE flights
                                      failed_resp TYPE failed_resp.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_rap_buffer IMPLEMENTATION.
  METHOD adapt_planetype.
    READ ENTITY zraunitflights
      ALL FIELDS WITH keys
      RESULT flights
      FAILED failed_resp.

    LOOP AT flights REFERENCE INTO DATA(flight).
      flight->Planetype = COND #( WHEN flight->Seatsmax < 100 THEN 'A'
                                  WHEN flight->Seatsmax BETWEEN 100 AND 200 THEN 'B'
                                  ELSE 'C' ).
    ENDLOOP.

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
CLASS ltc_rap_buffer DEFINITION FINAL FOR TESTING
	DURATION SHORT
	RISK LEVEL HARMLESS.
  	PRIVATE SECTION.
    		CLASS-DATA cut TYPE REF TO zcl_demo_aunit_rap_buffer.
    		CLASS-DATA txbuf_env TYPE REF TO if_botd_txbufdbl_bo_test_env.
    		CLASS-DATA test_double TYPE REF TO if_botd_txbufdbl_test_double.
    		
    		CLASS-METHODS class_setup.
    		METHODS setup.
    		CLASS-METHODS class_teardown.

    		METHODS test_read_existing_keys FOR TESTING.
    		METHODS test_read_non_existing_carrier FOR TESTING.
    		METHODS test_read_no_data FOR TESTING.
ENDCLASS.

CLASS ltc_rap_buffer IMPLEMENTATION.

  	METHOD class_setup.
    		cut = NEW zcl_demo_aunit_rap_buffer( ).
    				
    		txbuf_env = cl_botd_txbufdbl_bo_test_env=>create(
          environment_config = cl_botd_txbufdbl_bo_test_env=>prepare_environment_config(
          )->set_bdef_dependencies( bdef_dependencies = VALUE #( ( 'ZRAUNITFLIGHTS' ) ) ) ).		
  	ENDMETHOD.

  	METHOD setup.
    		 txbuf_env->clear_doubles( ).

    test_double =  txbuf_env->get_test_double( 'ZRAUNITFLIGHTS' ).
  	ENDMETHOD.

  	METHOD class_teardown.
    		txbuf_env->destroy( ).
  	ENDMETHOD.

  	METHOD test_read_existing_keys.
  		DATA keys4create TYPE TABLE FOR CREATE zraunitflights.
  		keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                             ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                     ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 )
                     ( %cid = `cid3` carrid = 'AA' connid = '1003' fldate = '20260803' seatsmax = 95 seatsocc = 50 ) ).
    	
    	test_double->insert_test_data( instances = keys4create ).
    	
    cut->adapt_planetype(
        EXPORTING keys = CORRESPONDING #( keys4create )
        IMPORTING flights = DATA(flights)
                  failed_resp = DATA(failed_resp) ).
    	    	
    	 cl_abap_unit_assert=>assert_initial( act = failed_resp ).
    	 cl_abap_unit_assert=>assert_equals( act = lines( flights ) exp = 3 ).
    	 cl_abap_unit_assert=>assert_equals( act = flights[ KEY id carrid = 'AA' connid = '1001' fldate = '20260801' ]-Planetype exp = 'B' ).
    	 cl_abap_unit_assert=>assert_equals( act = flights[ KEY id carrid = 'AA' connid = '1002' fldate = '20260802' ]-Planetype exp = 'C' ).
    	 cl_abap_unit_assert=>assert_equals( act = flights[ KEY id carrid = 'AA' connid = '1003' fldate = '20260803' ]-Planetype exp = 'A' ).
  	ENDMETHOD.

  	METHOD test_read_non_existing_carrier.

    DATA keys4create TYPE TABLE FOR CREATE zraunitflights.

    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                             ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                     ( %cid = `cid2` carrid = 'BB' connid = '2001' fldate = '20260802' seatsmax = 220 seatsocc = 198 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->adapt_planetype(
        EXPORTING keys = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' )
                                  ( carrid = 'BB' connid = '2001' fldate = '20260802' )
                                  ( carrid = 'CC' connid = '3001' fldate = '20260803' )
                                  ( carrid = 'DD' connid = '4001' fldate = '20260804' ) )
        IMPORTING flights = DATA(flights)
                  failed_resp = DATA(failed_resp) ).

    cl_abap_unit_assert=>assert_equals( act = lines( failed_resp-zraunitflights ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( flights ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = flights[ KEY id carrid = 'AA' connid = '1001' fldate = '20260801' ]-Planetype exp = 'B' ).
    cl_abap_unit_assert=>assert_equals( act = flights[ KEY id carrid = 'BB' connid = '2001' fldate = '20260802' ]-Planetype exp = 'C' ).
  	ENDMETHOD.

  METHOD test_read_no_data.
    cut->adapt_planetype(
        EXPORTING keys = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' )
                                  ( carrid = 'BB' connid = '2001' fldate = '20260802' )
                                  ( carrid = 'CC' connid = '3001' fldate = '20260803' ) )
        IMPORTING flights = DATA(flights)
                  failed_resp = DATA(failed_resp) ).

    cl_abap_unit_assert=>assert_initial( act = flights ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_resp-zraunitflights ) exp = 3 ).
  ENDMETHOD.

ENDCLASS.

``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 RAP business object: Mocking ABAP EML APIs</summary>
  <!-- -->

<br>

- **Class**: `zcl_demo_aunit_rap_eml`
- **Purpose**: 
    - Demonstrates ABAP Unit tests with ABAP EML requests as DOCs
    - Shows mocking of ABAP EML requests for both both read and modify requests.
- **Global class**: 
    - Provides wrapper methods around RAP EML for demonstration purposes:
      - `demo_eml_modify` for `MODIFY ENTITY ... CREATE` with mapped/ and failed responses.
      - `demo_eml_read` for `READ ENTITY` with the result and failed responses.    
- **Test class**: 
    - Defines a local test class that creates the ABAP EML mock environment and clears doubles per test.
    - Configures input/output expectations on the ABAP EML test double for read and modify operations, executes the code under test, and verifies responses.
    - Asserts successful, failed, and unconfigured test double scenarios for both read and create requests.


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
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class demonstrates ABAP Unit tests with ABAP EML requests. It shows mocking of ABAP EML requests for both read and modify
"! operations. The class provides wrapper methods for demonstration purposes.
CLASS zcl_demo_aunit_rap_eml DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    TYPES flights TYPE TABLE FOR READ RESULT zraunitflights.
    TYPES create_tab TYPE TABLE FOR CREATE zraunitflights.
    TYPES failed_resp TYPE RESPONSE FOR FAILED zraunitflights.
    TYPES mapped_resp TYPE RESPONSE FOR MAPPED zraunitflights.
    TYPES read_import_tab TYPE TABLE FOR READ IMPORT zraunitflights.
    TYPES read_result_tab TYPE TABLE FOR READ RESULT zraunitflights.

    "! Modifies entity data based on input keys with mapped and failed responses
    "!
    "! @parameter failed_resp | <p class="shorttext synchronized" lang="en">Response for failures</p>
    "! @parameter keys        | <p class="shorttext synchronized" lang="en">Keys for the entity to modify</p>
    "! @parameter mapped_resp | <p class="shorttext synchronized" lang="en">Mapped response</p>
    METHODS demo_eml_modify IMPORTING VALUE(keys) TYPE create_tab
                            EXPORTING mapped_resp TYPE mapped_resp
                                      failed_resp TYPE failed_resp.

    "! Reads entity data using specified keys and returns the result
    "!
    "! @parameter failed_resp | <p class="shorttext synchronized" lang="en">Response for failures</p>
    "! @parameter keys        | <p class="shorttext synchronized" lang="en">Keys identifying the entity to read</p>
    "! @parameter read_result | <p class="shorttext synchronized" lang="en">Results of the read operation</p>
    METHODS demo_eml_read IMPORTING VALUE(keys) TYPE read_import_tab
                          EXPORTING read_result TYPE read_result_tab
                                    failed_resp TYPE failed_resp.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_rap_eml IMPLEMENTATION.
  METHOD demo_eml_modify.
    MODIFY ENTITY zraunitflights
     CREATE FIELDS ( carrid connid fldate seatsmax seatsocc )
     WITH keys
     MAPPED mapped_resp
     FAILED failed_resp.
  ENDMETHOD.

  METHOD demo_eml_read.
    READ ENTITY zraunitflights
     ALL FIELDS WITH keys
     RESULT read_result
     FAILED failed_resp.
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
CLASS ltc_eml DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA cut TYPE REF TO zcl_demo_aunit_rap_eml.
    CLASS-DATA eml_env TYPE REF TO if_botd_mockemlapi_bo_test_env.

    CLASS-METHODS class_setup.
    METHODS setup.
    CLASS-METHODS class_teardown.

    METHODS test_create FOR TESTING.
    METHODS test_create_failed FOR TESTING.
    METHODS test_create_no_test_double FOR TESTING.

    METHODS test_read FOR TESTING.
    METHODS test_read_failed FOR TESTING.
    METHODS test_read_no_test_double FOR TESTING.

ENDCLASS.

CLASS ltc_eml IMPLEMENTATION.

  METHOD class_setup.
    cut = NEW zcl_demo_aunit_rap_eml( ).

    eml_env = cl_botd_mockemlapi_bo_test_env=>create(
  environment_config = cl_botd_mockemlapi_bo_test_env=>prepare_environment_config(
  )->set_bdef_dependencies( bdef_dependencies = VALUE #( ( 'ZRAUNITFLIGHTS' ) ) ) ).
  ENDMETHOD.

  METHOD setup.
    eml_env->clear_doubles( ).
  ENDMETHOD.

  METHOD class_teardown.
    eml_env->destroy( ).
  ENDMETHOD.

  METHOD test_read.
    DATA read_import_tab TYPE TABLE FOR READ IMPORT zraunitflights.
    DATA read_result_tab TYPE TABLE FOR READ RESULT zraunitflights.

    read_import_tab = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' )
                               ( carrid = 'AA' connid = '1002' fldate = '20260802' )
                               ( carrid = 'AA' connid = '1003' fldate = '20260803' ) ).

    read_result_tab = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                     ( carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 )
                     ( carrid = 'AA' connid = '1003' fldate = '20260803' seatsmax = 95 seatsocc = 50 ) ).

    DATA(eml_read_entpart_config_bldr) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
    DATA(eml_read_output_config_bldr) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    DATA(eml_read_entpart) = eml_read_entpart_config_bldr->build_entity_part( 'ZRAUNITFLIGHTS' )->set_instances_for_read( read_import_tab ).

    "Input configuration
    DATA(read_input) = eml_read_entpart_config_bldr->build_input_for_eml(  )->add_entity_part( eml_read_entpart ).

    "Output configuration
    DATA(read_output) = eml_read_output_config_bldr->build_output_for_eml( )->set_result_for_read( read_result_tab ).

    "Configuring the RAP BO test double
    DATA(test_double) = eml_env->get_test_double( 'ZRAUNITFLIGHTS' ).
    test_double->configure_call(  )->for_read( )->when_input( read_input )->then_set_output( read_output ).

    cut->demo_eml_read(
      EXPORTING
        keys        = read_import_tab
      IMPORTING
        read_result = DATA(cut_read_result)
        failed_resp = DATA(cut_failed_resp)
    ).

    cl_abap_unit_assert=>assert_initial( cut_failed_resp ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut_read_result ) exp = 3 ).
    ASSIGN cut_read_result[ 1 ] TO FIELD-SYMBOL(<fs>).
    cl_abap_unit_assert=>assert_equals( act = <fs>-carrid exp = 'AA' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-connid exp = '1001' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-fldate exp = '20260801' ).
    test_double->verify( )->read( read_input )->is_called_times( times = 1 ).
  ENDMETHOD.

  METHOD test_read_failed.
    DATA read_import_tab TYPE TABLE FOR READ IMPORT zraunitflights.
    DATA read_result_tab TYPE TABLE FOR READ RESULT zraunitflights.
    DATA failed_resp TYPE RESPONSE FOR FAILED EARLY zraunitflights.

    read_import_tab = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' ) ).
    read_result_tab = VALUE #( ).
    failed_resp-zraunitflights = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' ) ).

    DATA(eml_read_entpart_conf_bl) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
    DATA(eml_read_output_config_bldr) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    DATA(eml_read_entpart) = eml_read_entpart_conf_bl->build_entity_part( 'ZRAUNITFLIGHTS' )->set_instances_for_read( read_import_tab ).

    "Input configuration
    DATA(read_input) = eml_read_entpart_conf_bl->build_input_for_eml(  )->add_entity_part( eml_read_entpart ).

    "Output configuration
    DATA(read_output) = eml_read_output_config_bldr->build_output_for_eml( )->set_result_for_read( read_result_tab )->set_failed( failed_resp ).

    "Configuring the RAP BO test double
    DATA(test_double) = eml_env->get_test_double( 'ZRAUNITFLIGHTS' ).
    test_double->configure_call(  )->for_read( )->when_input( read_input )->then_set_output( read_output ).

    cut->demo_eml_read(
      EXPORTING
        keys        = read_import_tab
      IMPORTING
        read_result = DATA(cut_read_result)
        failed_resp = DATA(cut_failed_resp)
    ).

    cl_abap_unit_assert=>assert_initial( cut_read_result ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut_failed_resp-zraunitflights ) exp = 1 ).
    ASSIGN cut_failed_resp-zraunitflights[ 1 ] TO FIELD-SYMBOL(<fs>).
    cl_abap_unit_assert=>assert_equals( act = <fs>-carrid exp = 'AA' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-connid exp = '1001' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-fldate exp = '20260801' ).
    test_double->verify( )->read( read_input )->is_called_times( times = 1 ).
  ENDMETHOD.

  METHOD test_read_no_test_double.
    DATA read_import_tab TYPE TABLE FOR READ IMPORT zraunitflights.

    read_import_tab = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' ) ).

    cut->demo_eml_read(
         EXPORTING
           keys        = read_import_tab
         IMPORTING
           read_result = DATA(cut_read_result)
           failed_resp = DATA(cut_failed_resp)
       ).

    cl_abap_unit_assert=>assert_initial( cut_read_result ).
    cl_abap_unit_assert=>assert_initial( cut_failed_resp ).
  ENDMETHOD.

  METHOD test_create.

    "Preparing test data (RAP BO instances, response parameters)
    DATA create_tab_ro TYPE TABLE FOR CREATE zraunitflights.
    DATA mapped_resp TYPE RESPONSE FOR MAPPED EARLY zraunitflights.

    create_tab_ro = VALUE #( ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                             ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 ) ).

    mapped_resp-zraunitflights = VALUE #( ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801'  )
                                          ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260802'  ) ).

    DATA(eml_create_entpart_config_bldr) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    DATA(eml_output_config_builder) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    DATA(eml_create_entpart) = eml_create_entpart_config_bldr->build_entity_part( 'ZRAUNITFLIGHTS'
                                                         )->set_instances_for_create( create_tab_ro ).

    DATA(input) = eml_create_entpart_config_bldr->build_input_for_eml(  )->add_entity_part( eml_create_entpart ).

    DATA(output) = eml_output_config_builder->build_output_for_eml( )->set_mapped( mapped_resp ).

    DATA(test_double) = eml_env->get_test_double( 'ZRAUNITFLIGHTS' ).
    test_double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).

    cut->demo_eml_modify(
      EXPORTING
        keys             = create_tab_ro
      IMPORTING
        mapped_resp      = DATA(cut_mapped_response)
        failed_resp     = DATA(cut_failed_response)
    ).

    cl_abap_unit_assert=>assert_initial(  cut_failed_response ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut_mapped_response-zraunitflights ) exp = 2 ).
    ASSIGN cut_mapped_response-zraunitflights[ 1 ] TO FIELD-SYMBOL(<fs>).
    cl_abap_unit_assert=>assert_equals( act = <fs>-%cid exp = `cid1` ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-carrid exp = 'AA' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-connid exp = '1001' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-fldate exp = '20260801' ).
    test_double->verify( )->modify( input )->is_called_times( times = 1 ).

  ENDMETHOD.

  METHOD test_create_failed.

    "Preparing test data (RAP BO instances, response parameters)
    DATA create_tab_ro TYPE TABLE FOR CREATE zraunitflights.
    DATA failed_resp TYPE RESPONSE FOR FAILED EARLY zraunitflights.

    create_tab_ro = VALUE #( ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 ) ).

    failed_resp-zraunitflights = VALUE #( ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801'  ) ).

    DATA(eml_create_entpart_config_bldr) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    DATA(eml_output_config_builder) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    DATA(eml_create_entpart) = eml_create_entpart_config_bldr->build_entity_part( 'ZRAUNITFLIGHTS' )->set_instances_for_create( create_tab_ro ).

    DATA(input) = eml_create_entpart_config_bldr->build_input_for_eml(  )->add_entity_part( eml_create_entpart ).

    DATA(output) = eml_output_config_builder->build_output_for_eml( )->set_failed( failed_resp ).

    DATA(test_double) = eml_env->get_test_double( 'ZRAUNITFLIGHTS' ).
    test_double->configure_call( )->for_modify( )->when_input( input )->then_set_output( output ).

    cut->demo_eml_modify(
      EXPORTING
        keys             = create_tab_ro
      IMPORTING
        mapped_resp      = DATA(cut_mapped_response)
        failed_resp     = DATA(cut_failed_response)
    ).

    cl_abap_unit_assert=>assert_initial( cut_mapped_response ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut_failed_response-zraunitflights ) exp = 1 ).
    ASSIGN cut_failed_response-zraunitflights[ 1 ] TO FIELD-SYMBOL(<fs>).
    cl_abap_unit_assert=>assert_equals( act = <fs>-%cid exp = `cid1` ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-carrid exp = 'AA' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-connid exp = '1001' ).
    cl_abap_unit_assert=>assert_equals( act = <fs>-fldate exp = '20260801' ).
    test_double->verify( )->modify( input )->is_called_times( times = 1 ).

  ENDMETHOD.

  METHOD test_create_no_test_double.

    cut->demo_eml_modify(
          EXPORTING
            keys             = VALUE #( ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                                        ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 ) )
          IMPORTING
            mapped_resp      = DATA(cut_mapped_response)
            failed_resp     = DATA(cut_failed_response)
        ).

    cl_abap_unit_assert=>assert_initial( cut_mapped_response ).
    cl_abap_unit_assert=>assert_initial( cut_failed_response ).

  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 RAP business object: Testing ABAP behavior pool</summary>
  <!-- -->

<br>

- **Class**: `zbp_zraunitflights`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for an ABAP behavior pool
    - Shows testing an action and a validation
- **Global class**: 
    - Contains only the class definition skeleton    
- **Local types**:     
    - Includes the behavior implementation.
    - Defines friendship between local and test class to allow the test class to access private methods.
    - Action `calc_occ_rate`: Retrieves flight seat information based on instance keys and calculates the occupancy rate using this data.
    - Validation `val`: Triggered when saving seat-related fields. It retrieves flight seat information based on instance keys and fails if there are invalid entries (if seatsmax or seatsocc are below 0, or if seatsocc exceeds seatsmax).    
- **Test class**: 
    - Defines a local test class that creates transactional buffer test doubles. 
    - Uses the statement `CREATE OBJECT ... FOR TESTING` to instantiate the class under test.
    - Test methods address different aspects, including valid and invalid cases.


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
"! <p class="shorttext synchronized" lang="en">Behavior Implementation for ZRAUNITFLIGHTS</p>
"!
"! The test class demonstrates ABAP Unit tests for an ABAP behavior pool. It showcases testing
"! an action and a validation.
CLASS zbp_zraunitflights DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zraunitflights.
ENDCLASS.

CLASS zbp_zraunitflights IMPLEMENTATION.
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
CLASS ltc_zraunitflights DEFINITION DEFERRED FOR TESTING.
CLASS lhc_zraunitflights DEFINITION INHERITING FROM cl_abap_behavior_handler FRIENDS ltc_zraunitflights.

  PRIVATE SECTION.
    METHODS calc_occ_rate FOR MODIFY
       keys FOR ACTION zraunitflights~calc_occ_rate RESULT result.
    METHODS val FOR VALIDATE ON SAVE
       keys FOR zraunitflights~val.

ENDCLASS.

CLASS lhc_zraunitflights IMPLEMENTATION.

  METHOD calc_occ_rate.

    READ ENTITY IN LOCAL MODE zraunitflights
      FIELDS ( Seatsmax Seatsocc ) WITH CORRESPONDING #( keys )
      RESULT DATA(read_result)
      FAILED failed.

    CHECK read_result IS NOT INITIAL.

    LOOP AT read_result INTO DATA(wa).
      APPEND VALUE #( %tky = wa-%tky
                      %param = round( val = wa-Seatsocc / wa-Seatsmax * 100 dec = 2 ) ) TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD val.

    READ ENTITY IN LOCAL MODE zraunitflights
       FIELDS ( Seatsmax Seatsocc ) WITH CORRESPONDING #( keys )
       RESULT DATA(read_result).

    CHECK read_result IS NOT INITIAL.

    LOOP AT read_result INTO DATA(wa).
      IF wa-Seatsmax < 0
      OR wa-Seatsocc > wa-Seatsmax
      OR wa-Seatsocc < 0.
        APPEND VALUE #( %tky = wa-%tky
                        %fail-cause = if_abap_behv=>cause-unspecific )
                     TO failed-zraunitflights.

        APPEND VALUE #( %tky = wa-%tky
                        %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Validation failed' )
                      ) TO reported-zraunitflights.
      ENDIF.
    ENDLOOP.

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
CLASS ltc_zraunitflights DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA cut TYPE REF TO lhc_zraunitflights.
    CLASS-DATA txbuf_env TYPE REF TO if_botd_txbufdbl_bo_test_env.
    CLASS-DATA test_double TYPE REF TO if_botd_txbufdbl_test_double.

    DATA keys4create TYPE TABLE FOR CREATE zraunitflights.
    DATA result TYPE TABLE FOR ACTION RESULT zraunitflights~calc_occ_rate.
    DATA mapped TYPE RESPONSE FOR MAPPED EARLY zraunitflights.
    DATA failed TYPE RESPONSE FOR FAILED EARLY zraunitflights.
    DATA reported TYPE RESPONSE FOR REPORTED EARLY zraunitflights.
    DATA failed_late TYPE RESPONSE FOR FAILED LATE zraunitflights.
    DATA reported_late TYPE RESPONSE FOR REPORTED LATE zraunitflights.

    CLASS-METHODS class_setup.
    METHODS setup.
    CLASS-METHODS class_teardown.

    METHODS test_calc_occ_rate_action FOR TESTING.
    METHODS test_calc_occ_rate_act_no_in FOR TESTING.

    METHODS test_val_invalid_smaxLT0 FOR TESTING.
    METHODS test_val_invalid_soccGTsmax FOR TESTING.
    METHODS test_val_invalid_soccLT0 FOR TESTING.
    METHODS test_val_accepts_valid FOR TESTING.

ENDCLASS.

CLASS ltc_zraunitflights IMPLEMENTATION.
  METHOD class_setup.
    CREATE OBJECT cut FOR TESTING.

    txbuf_env = cl_botd_txbufdbl_bo_test_env=>create(
      environment_config = cl_botd_txbufdbl_bo_test_env=>prepare_environment_config(
      )->set_bdef_dependencies( bdef_dependencies = VALUE #( ( 'ZRAUNITFLIGHTS' ) ) ) ).
  ENDMETHOD.

  METHOD setup.
    txbuf_env->clear_doubles( ).

    test_double =  txbuf_env->get_test_double( 'ZRAUNITFLIGHTS' ).
  ENDMETHOD.

  METHOD class_teardown.
    txbuf_env->destroy( ).
  ENDMETHOD.

  METHOD test_calc_occ_rate_action.

    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                            ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260701' seatsmax = 100 seatsocc = 80 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->calc_occ_rate(
      EXPORTING
        keys     =  CORRESPONDING #( keys4create )
      CHANGING
        result   = result
        mapped   = mapped
        failed   = failed
        reported = reported
    ).

    cl_abap_unit_assert=>assert_equals(
      act = result[ 1 ]-%param
      exp = CONV decfloat34( '80' ) ).

    cl_abap_unit_assert=>assert_initial( act = failed ).

  ENDMETHOD.

  METHOD test_calc_occ_rate_act_no_in.
    cut->calc_occ_rate(
         EXPORTING
           keys     = VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260701' ) )
         CHANGING
           result   = result
           mapped   = mapped
           failed   = failed
           reported = reported
       ).

    cl_abap_unit_assert=>assert_initial( act = result ).
    cl_abap_unit_assert=>assert_not_initial( act = failed ).
  ENDMETHOD.


  METHOD test_val_invalid_soccGTsmax.

    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                           ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260701' seatsmax = 100 seatsocc = 120 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->val(
      EXPORTING
        keys     = CORRESPONDING #( keys4create )
      CHANGING
        failed   = failed_late
        reported = reported_late
    ).

    cl_abap_unit_assert=>assert_not_initial( act = failed_late ).
    cl_abap_unit_assert=>assert_not_initial( act = reported_late ).

    cl_abap_unit_assert=>assert_equals(
      act = failed_late-zraunitflights[ 1 ]-%key
      exp = keys4create[ 1 ]-%key ).

    cl_abap_unit_assert=>assert_equals(
          act = reported_late-zraunitflights[ 1 ]-%msg->if_t100_dyn_msg~msgv1
          exp = 'Validation failed' ).
  ENDMETHOD.

  METHOD test_val_accepts_valid.
    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                           ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260701' seatsmax = 120 seatsocc = 120 )
                           ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260702' seatsmax = 150 seatsocc = 120 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->val(
      EXPORTING
        keys     = CORRESPONDING #( keys4create )
      CHANGING
        failed   = failed_late
        reported = reported_late
    ).

    cl_abap_unit_assert=>assert_initial( act = failed_late ).
    cl_abap_unit_assert=>assert_initial( act = reported_late ).
  ENDMETHOD.

  METHOD test_val_invalid_smaxLT0.
    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                           ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260701' seatsmax = -10 seatsocc = 120 )
                           ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260702' seatsmax = -1 seatsocc = 120 )
                           ( %cid = `cid3` carrid = 'AA' connid = '1003' fldate = '20260703' seatsmax = 0 seatsocc = 0 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->val(
      EXPORTING
        keys     = CORRESPONDING #( keys4create )
      CHANGING
        failed   = failed_late
        reported = reported_late
    ).

    cl_abap_unit_assert=>assert_not_initial( act = failed_late ).
    cl_abap_unit_assert=>assert_not_initial( act = reported_late ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( failed_late-zraunitflights )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( reported_late-zraunitflights )
      exp = 2 ).
  ENDMETHOD.

  METHOD test_val_invalid_soccLT0.
    keys4create = VALUE #( %control = VALUE #( carrid = if_abap_behv=>mk-on connid = if_abap_behv=>mk-on
                                               fldate = if_abap_behv=>mk-on seatsmax = if_abap_behv=>mk-on
                                               seatsocc = if_abap_behv=>mk-on )
                           ( %cid = `cid1` carrid = 'AA' connid = '1001' fldate = '20260701' seatsmax = 100 seatsocc = -1 )
                           ( %cid = `cid2` carrid = 'AA' connid = '1002' fldate = '20260702' seatsmax = 150 seatsocc = -120 )
                           ( %cid = `cid3` carrid = 'AA' connid = '1003' fldate = '20260703' seatsmax = 180 seatsocc = 0 ) ).

    test_double->insert_test_data( instances = keys4create ).

    cut->val(
      EXPORTING
        keys     = CORRESPONDING #( keys4create )
      CHANGING
        failed   = failed_late
        reported = reported_late
    ).

    cl_abap_unit_assert=>assert_not_initial( act = failed_late ).
    cl_abap_unit_assert=>assert_not_initial( act = reported_late ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( failed_late-zraunitflights )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( reported_late-zraunitflights )
      exp = 2 ).
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

</table>

</details> 

<br>

<details>
  <summary>🟢 Authority check dependencies</summary>
  <!-- -->

<br>

- Note the prerequisites before exploring the demo.
- **Class**: `zcl_demo_aunit_auth`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for authorization-dependent logic as a DOC scenario.
    - Shows testing of `AUTHORITY-CHECK` behavior by controlling the authorization context.    
- **Global class**: 
    - Defines an enumerated type representing activities (`create`, `change`, `display`, `delete`) and maps each value to its corresponding `ACTVT` code.
    - A method executes `AUTHORITY-CHECK` for the demo object `ZAUTH_OB` and returns `abap_true` when `sy-subrc = 0`.
- **Test class**: 
    - Defines a local test class that uses an API for authorization checks.
    - Note that the API only restricts the authorizations of the user running the test and does not grant additional authorizations. To follow the example fully, complete the prerequisite steps, create the demo authorization object, and assign a business role to your user (along with any necessary steps in the _SAP Business AI Platform, ABAP environment_). In an on-premise environment, you could replace the demo authorization object in the code with `S_DEVELOP`, for example.
    - Configures different authorization sets, executes `call_authority_check`, and verifies expected outcomes for each action.
    - The demo authorization object assumes your user has the `create`, `change`, `display`, and `delete` authorizations. Therefore, the unrestricted test methods should return true for the authorization check. Some test methods are designed to restrict certain authorizations (only `change` and `display`). For example, even though deletion is actually authorized, the restriction setting using the API will indicate that deletion is not permitted.
    - Asserts both positive and negative authorization cases and covers execution log behavior.



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
``` 

 </td>
</tr>

<tr>
<td> 

CCAU include (Test Classes tab in ADT)

 </td>

 <td> 

``` abap
CLASS ltc_auth DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA ctrl_auth TYPE REF TO if_aunit_auth_check_controller.
    DATA cut TYPE REF TO zcl_demo_aunit_auth.
    CLASS-METHODS class_setup.
    METHODS setup.
    METHODS teardown.

    METHODS allow_all_authorizations.
    METHODS restrict_to_chg_disp_auth RAISING cx_static_check.
    METHODS assert_auth
      IMPORTING
        field_value        TYPE zcl_demo_aunit_auth=>enum_field_value
        exp_is_authorized  TYPE abap_boolean
        msg                TYPE string OPTIONAL.
    METHODS assert_execution_log_counts
      IMPORTING
        exp_passed TYPE i
        exp_failed TYPE i.

    METHODS test_create_full_auth FOR TESTING.
    METHODS test_change_full_auth FOR TESTING.
    METHODS test_display_full_auth FOR TESTING.
    METHODS test_delete_full_auth FOR TESTING.

    METHODS test_create_chg_disp_restr FOR TESTING RAISING cx_static_check.
    METHODS test_change_chg_disp_restr FOR TESTING RAISING cx_static_check.
    METHODS test_display_chg_disp_restr FOR TESTING RAISING cx_static_check.
    METHODS test_delete_chg_disp_restr FOR TESTING RAISING cx_static_check.
    METHODS test_exec_log_chg_disp_restr FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_auth IMPLEMENTATION.
  METHOD class_setup.
    ctrl_auth = cl_aunit_authority_check=>get_controller( ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
    allow_all_authorizations( ).
  ENDMETHOD.

  METHOD teardown.
    ctrl_auth->reset( ).
  ENDMETHOD.

  METHOD allow_all_authorizations.
    ctrl_auth->reset( ).
  ENDMETHOD.

  METHOD restrict_to_chg_disp_auth.
    DATA(auth_change_display) = VALUE cl_aunit_auth_check_types_def=>role_auth_objects(
        ( object         = 'ZAUTH_OB'
          authorizations = VALUE #( ( VALUE #( ( fieldname   = 'ACTVT'
                                                 fieldvalues = VALUE #( ( lower_value = '02' )
                                                                        ( lower_value = '03' ) ) ) ) ) ) ) ).

    DATA(auth_obj_set) = cl_aunit_authority_check=>create_auth_object_set(
      VALUE cl_aunit_auth_check_types_def=>user_role_authorizations( ( role_authorizations = auth_change_display ) ) ).

    ctrl_auth->restrict_authorizations_to( auth_obj_set ).
  ENDMETHOD.

  METHOD assert_auth.
    cl_abap_unit_assert=>assert_equals(
      act = cut->call_authority_check( field_value )
      exp = exp_is_authorized
      msg = msg ).
  ENDMETHOD.

  METHOD assert_execution_log_counts.
    ctrl_auth->get_auth_check_execution_log( )->get_execution_status(
      IMPORTING
        passed_execution = DATA(passed)
        failed_execution = DATA(failed) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( passed )
      exp = exp_passed
      msg = 'Unexpected number of passed AUTHORITY-CHECK executions.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( failed )
      exp = exp_failed
      msg = 'Unexpected number of failed AUTHORITY-CHECK executions.' ).
  ENDMETHOD.

  METHOD test_create_full_auth.
    allow_all_authorizations( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>create
      exp_is_authorized = abap_true
      msg               = 'Create should be authorized in unrestricted mode.' ).
  ENDMETHOD.

  METHOD test_change_full_auth.
    allow_all_authorizations( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>change
      exp_is_authorized = abap_true
      msg               = 'Change should be authorized in unrestricted mode.' ).
  ENDMETHOD.

  METHOD test_display_full_auth.
    allow_all_authorizations( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>display
      exp_is_authorized = abap_true
      msg               = 'Display should be authorized in unrestricted mode.' ).
  ENDMETHOD.

  METHOD test_delete_full_auth.
    allow_all_authorizations( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>delete
      exp_is_authorized = abap_true
      msg               = 'Delete should be authorized in unrestricted mode.' ).
  ENDMETHOD.

  METHOD test_create_chg_disp_restr.
    restrict_to_chg_disp_auth( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>create
      exp_is_authorized = abap_false
      msg               = 'Create should be blocked when only change/display are allowed.' ).
  ENDMETHOD.

  METHOD test_change_chg_disp_restr.
    restrict_to_chg_disp_auth( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>change
      exp_is_authorized = abap_true
      msg               = 'Change should be allowed when only change/display are allowed.' ).
  ENDMETHOD.

  METHOD test_display_chg_disp_restr.
    restrict_to_chg_disp_auth( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>display
      exp_is_authorized = abap_true
      msg               = 'Display should be allowed when only change/display are allowed.' ).
  ENDMETHOD.

  METHOD test_delete_chg_disp_restr.
    restrict_to_chg_disp_auth( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>delete
      exp_is_authorized = abap_false
      msg               = 'Delete should be blocked when only change/display are allowed.' ).
  ENDMETHOD.

  METHOD test_exec_log_chg_disp_restr.
    restrict_to_chg_disp_auth( ).

    assert_auth( field_value = zcl_demo_aunit_auth=>create exp_is_authorized = abap_false ).
    assert_auth( field_value = zcl_demo_aunit_auth=>change exp_is_authorized = abap_true ).
    assert_auth( field_value = zcl_demo_aunit_auth=>display exp_is_authorized = abap_true ).
    assert_auth( field_value = zcl_demo_aunit_auth=>delete exp_is_authorized = abap_false ).

    assert_execution_log_counts(
      exp_passed = 2
      exp_failed = 2 ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 Function module (Function Module Test Double Framework)</summary>
  <!-- -->

<br>

- **Class**: `zcl_demo_aunit_func_tdf`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for logic that depends on a function module.
- **Global class**: 
    - Defines an enumeration type for arithmetic operators and a `calculate` method that accepts two integers and an operator.
    - Delegates calculations to the `ZFUNC_DEMO_AUNIT` function module and returns the its result.
- **Test class**: 
    - Creates a local test class that sets up a test environment for `ZFUNC_DEMO_AUNIT`.
    - Configures test double behavior for specific input combinations (returned values and raised exceptions), executes the code under test, and asserts outcomes.
    - Asserts normal arithmetic operations and error scenarios, including division by zero and arithmetic overflow.

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
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class demonstrates ABAP Unit tests for logic that depends on a function module. It provides a method to
"! perform arithmetic calculations based on an operator. It defines an enumeration for arithmetic operators and
"! leverages the function module ZFUNC_DEMO_AUNIT to execute calculations.
CLASS zcl_demo_aunit_func_tdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM operator,
             add,
             subtract,
             multiply,
             divide,
           END OF ENUM operator.

    "! Performs arithmetic operations based on the given operator and returns the result
    "!
    "! @parameter num1                 | <p class="shorttext synchronized" lang="en">First integer input for calculation</p>
    "! @parameter num2                 | <p class="shorttext synchronized" lang="en">Second integer input for calculation</p>
    "! @parameter operator             | <p class="shorttext synchronized" lang="en">Arithmetic operator (add, subtract, multiply, divide)</p>
    "! @parameter result               | <p class="shorttext synchronized" lang="en">Result of the arithmetic operation as a string</p>
    "! @raising cx_sy_arithmetic_error | <p class="shorttext synchronized" lang="en">Exception raised for arithmetic errors</p>
    METHODS calculate IMPORTING num1          TYPE i
                                operator      TYPE operator
                                num2          TYPE i
                      RETURNING VALUE(result) TYPE string
                      RAISING   cx_sy_arithmetic_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_func_tdf IMPLEMENTATION.
  METHOD calculate.
    CALL FUNCTION 'ZFUNC_DEMO_AUNIT'
      EXPORTING
        num1     = num1
        operator = operator
        num2     = num2
      IMPORTING
        result   = result.
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
CLASS ltc_calculate_func_td DEFINITION FINAL FOR TESTING
	DURATION SHORT
	RISK LEVEL HARMLESS.
  	PRIVATE SECTION.
    		DATA cut TYPE REF TO zcl_demo_aunit_func_tdf.
    		CLASS-DATA function_env TYPE REF TO if_function_test_environment.

    		CLASS-METHODS class_setup.
    		CLASS-METHODS class_teardown.
    		METHODS setup.

    		METHODS configure_result
    			IMPORTING first_number     TYPE i
    								op TYPE zcl_demo_aunit_func_tdf=>operator
    								second_number     TYPE i
    								calc_result   TYPE string.

    		METHODS configure_exception
    			IMPORTING first_number     TYPE i
    								op TYPE zcl_demo_aunit_func_tdf=>operator
    								second_number     TYPE i
    								exception TYPE REF TO cx_sy_arithmetic_error.

    		METHODS test_addition FOR TESTING.
    		METHODS test_subtraction FOR TESTING.
    		METHODS test_multiplication FOR TESTING.
    		METHODS test_division FOR TESTING.
    		METHODS test_exc_0_div_0 FOR TESTING.
    		METHODS test_exc_1_div_0 FOR TESTING.
    		METHODS test_exc_mult_overflow FOR TESTING.
    		METHODS test_without_helper_class FOR TESTING.
ENDCLASS.

CLASS ltc_calculate_func_td IMPLEMENTATION.

  	METHOD class_setup.
    		function_env = cl_function_test_environment=>create( VALUE #( ( 'ZFUNC_DEMO_AUNIT' ) ) ).			
  	ENDMETHOD.

  	METHOD class_teardown.
    		function_env->clear_doubles( ).
    		CLEAR function_env.
  	ENDMETHOD.

  	METHOD setup.
    		cut = NEW zcl_demo_aunit_func_tdf( ).
    		function_env->clear_doubles( ).
  	ENDMETHOD.


  METHOD test_without_helper_class.

    DATA(test_double) = function_env->get_double( 'ZFUNC_DEMO_AUNIT' ).

    DATA(input_conf) = test_double->create_input_configuration(
                                         )->set_importing_parameter( name = 'NUM1' value = 1
    )->set_importing_parameter( name = 'OPERATOR' value = zcl_demo_aunit_func_tdf=>add
    )->set_importing_parameter( name = 'NUM2' value = 1 ).

    DATA(output_conf) = test_double->create_output_configuration(
                                         )->set_exporting_parameter( name = 'RESULT' value = 2 ).

    test_double->configure_call(
     )->when( input_configuration = input_conf
     )->then_set_output( output_configuration = output_conf ).

    DATA(calc_result) = cut->calculate(
                      num1     = 1
                      operator = zcl_demo_aunit_func_tdf=>add
                      num2     = 1
                    ).

    cl_abap_unit_assert=>assert_equals( act = calc_result
                                        exp = 2 ).
  ENDMETHOD.


  	METHOD configure_result.
    		DATA(function_double) = function_env->get_double( function_name = 'ZFUNC_DEMO_AUNIT' ).

    		DATA(input_cfg) = function_double->create_input_configuration( ).
    		input_cfg->set_importing_parameter( name = 'NUM1' value = first_number ).
    		input_cfg->set_importing_parameter( name = 'OPERATOR' value = op ).
    		input_cfg->set_importing_parameter( name = 'NUM2' value = second_number ).

    		DATA(output_cfg) = function_double->create_output_configuration( ).
    		output_cfg->set_exporting_parameter( name = 'RESULT' value = calc_result ).

    		DATA(call_cfg) = function_double->configure_call( ).
    		DATA(then_cfg) = call_cfg->when( input_configuration = input_cfg ).
    		then_cfg->then_set_output( output_configuration = output_cfg ).
  	ENDMETHOD.

  	METHOD configure_exception.
    		DATA(function_double) = function_env->get_double( function_name = 'ZFUNC_DEMO_AUNIT' ).

    		DATA(input_cfg) = function_double->create_input_configuration( ).
    		input_cfg->set_importing_parameter( name = 'NUM1' value = first_number ).
    		input_cfg->set_importing_parameter( name = 'OPERATOR' value = op ).
    		input_cfg->set_importing_parameter( name = 'NUM2' value = second_number ).

    		DATA(call_cfg) = function_double->configure_call( ).
    		DATA(then_cfg) = call_cfg->when( input_configuration = input_cfg ).
    		then_cfg->then_raise_exception( exception = exception ).
  	ENDMETHOD.

  	METHOD test_addition.
    		me->configure_result(
    			first_number = 7
    			op = zcl_demo_aunit_func_tdf=>add
    			second_number = 5
    			calc_result = '12' ).

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate(
    				num1 = 7
    				operator = zcl_demo_aunit_func_tdf=>add
    				num2 = 5 )
    			exp = '12' ).
  	ENDMETHOD.

  	METHOD test_subtraction.
    		me->configure_result(
    			first_number = 10
    			op = zcl_demo_aunit_func_tdf=>subtract
    			second_number = 3
    			calc_result = '7' ).

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate(
    				num1 = 10
    				operator = zcl_demo_aunit_func_tdf=>subtract
    				num2 = 3 )
    			exp = '7' ).
  	ENDMETHOD.

  	METHOD test_multiplication.
    		me->configure_result(
    			first_number = 6
    			op = zcl_demo_aunit_func_tdf=>multiply
    			second_number = 4
    			calc_result = '24' ).

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate(
    				num1 = 6
    				operator = zcl_demo_aunit_func_tdf=>multiply
    				num2 = 4 )
    			exp = '24' ).
  	ENDMETHOD.

  	METHOD test_division.
    		me->configure_result(
    			first_number = 20
    			op = zcl_demo_aunit_func_tdf=>divide
    			second_number = 5
    			calc_result = '4' ).

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate(
    				num1 = 20
    				operator = zcl_demo_aunit_func_tdf=>divide
    				num2 = 5 )
    			exp = '4' ).
  	ENDMETHOD.

  	METHOD test_exc_0_div_0.
    		me->configure_exception(
    			first_number = 0
    			op = zcl_demo_aunit_func_tdf=>divide
    			second_number = 0
    			exception = NEW cx_sy_zerodivide( ) ).

    		TRY.
        			cut->calculate(
        				num1 = 0
        				operator = zcl_demo_aunit_func_tdf=>divide
        				num2 = 0 ).
        			cl_abap_unit_assert=>fail( msg = 'Expected cx_sy_arithmetic_error for 0 / 0' ).
      		CATCH cx_sy_arithmetic_error.
    		ENDTRY.
  	ENDMETHOD.

  	METHOD test_exc_1_div_0.
    		me->configure_exception(
    			first_number = 1
    			op = zcl_demo_aunit_func_tdf=>divide
    			second_number = 0
    			exception = NEW cx_sy_zerodivide( ) ).

    		TRY.
        			cut->calculate(
        				num1 = 1
        				operator = zcl_demo_aunit_func_tdf=>divide
        				num2 = 0 ).
        			cl_abap_unit_assert=>fail( msg = 'Expected cx_sy_arithmetic_error for 1 / 0' ).
      		CATCH cx_sy_arithmetic_error.
    		ENDTRY.
  	ENDMETHOD.

  	METHOD test_exc_mult_overflow.
    		me->configure_exception(
    			first_number = 2147483647
    			op = zcl_demo_aunit_func_tdf=>multiply
    			second_number = 2147483647
    			exception = NEW cx_sy_arithmetic_overflow( ) ).

    		TRY.
        			cut->calculate(
        				num1 = 2147483647
        				operator = zcl_demo_aunit_func_tdf=>multiply
        				num2 = 2147483647 ).
        			cl_abap_unit_assert=>fail( msg = 'Expected cx_sy_arithmetic_error for overflow multiplication' ).
      		CATCH cx_sy_arithmetic_error.
    		ENDTRY.
  	ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 Inspecting background processing using bgPF</summary>
  <!-- -->

<br>

- **Class**: `zcl_demo_aunit_bgpf`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for background processing logic using the ABAP Background Processing Framework (bgPF).
    - Shows how background processing can be inspected with framework test spies instead of real asynchronous execution.
- **Global class**: 
    - Implements bgPF operation and scheduling behavior (`if_bgmc_op_single~execute`, `execute`, `execute_2`).
    - The example includes a transactionally controlled scenario (see [Controlled SAP LUW](https://help.sap.com/docs/abap-cloud/abap-concepts/controlled-sap-luw)). For that purpose, the `if_bgmc_op_single~execute` implementation includes a `cl_abap_tx=>save( ).` call, followed by a method call that modifies a database table. The previous method call, `set_attribute( ).`, is meant to transform a string that was passed via instance constructor to upper case and assign the value to an instance attribute (which can be retrieved by a `get_input` method call).
    - The methods `execute` and `execute_2` are tested. The include the (double) triggering of background processes.
    - The class can also be run using F9 as it implements the `if_oo_adt_classrun~main` method, illustrating the effect of the background processing.    
- **Test class**: 
    - Defines a local test class that creates a bgPF spy.
    - The test methods show various method calls the API offers, among them assertions regarding the number of background processes - which are not actually triggered.
- Find more information on bgPF and a similar example [here](https://help.sap.com/docs/abap-cloud/abap-concepts/background-processing-framework).

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
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class is demonstrates ABAP Unit tests for background processing logic using the ABAP Background
"! Processing Framework (bgPF). It shows how background processing can be inspected with framework test
"! spies instead of real asynchronous execution. The class can also be run using F9 as it implements the
"! if_oo_adt_classrun~main method, illustrating the effect of the background processing.
CLASS zcl_demo_aunit_bgpf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_bgmc_op_single.

    "! <p class="shorttext synchronized" lang="en">Executes the main background process and handles exceptions</p>
    "!
    "! @raising cx_bgmc | <p class="shorttext synchronized" lang="en">Exception class for background processing errors</p>
    METHODS execute RAISING cx_bgmc.

    "! Executes the main background process two times in a loop, handling exceptions
    "!
    "! @raising cx_bgmc | <p class="shorttext synchronized" lang="en">Exception class for background processing errors</p>
    METHODS execute_2 RAISING cx_bgmc.

**********************************************************************

    "! Deletes all entries from ztaunitflights when the class is instantiated
    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized" lang="en">Initializes the instance attribute with the provided string</p>
    "!
    "! @parameter str | <p class="shorttext synchronized" lang="en">The string to be set as the instance attribute</p>
    METHODS constructor
      IMPORTING
        str TYPE string OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Returns the input string that was set in the constructor</p>
    "!
    "! @parameter input | <p class="shorttext synchronized" lang="en">The input string initialized in the constructor</p>
    METHODS get_input
      RETURNING
        VALUE(input) TYPE string.
  PRIVATE SECTION.
    DATA string TYPE string.

    "! Transforms the instance attribute to upper case
    METHODS set_attribute.

    "! Modifies database table ztaunitflights with predefined flight records
    METHODS modify_dbtab.
ENDCLASS.

CLASS zcl_demo_aunit_bgpf IMPLEMENTATION.

  METHOD constructor.
    string = str.
  ENDMETHOD.

  METHOD if_bgmc_op_single~execute.
    set_attribute( ).

    cl_abap_tx=>save( ).

    modify_dbtab( ).
  ENDMETHOD.

  METHOD get_input.
    RETURN string.
  ENDMETHOD.

  METHOD set_attribute.
    IF string IS INITIAL.
      ASSERT 1 = 0.
    ENDIF.

    string = to_upper( string ).
  ENDMETHOD.

  METHOD modify_dbtab.
    MODIFY ztaunitflights FROM TABLE @( VALUE #( ( carrid = 'AA' connid = '1001' fldate = '20260801' seatsmax = 180 seatsocc = 135 )
                          ( carrid = 'AA' connid = '1002' fldate = '20260802' seatsmax = 220 seatsocc = 198 )
                     ( carrid = 'AA' connid = '1003' fldate = '20260803' seatsmax = 95 seatsocc = 50 ) ) ).
  ENDMETHOD.

  METHOD class_constructor.
    DELETE FROM ztaunitflights.
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    TRY.
        execute( ).
      CATCH cx_bgmc INTO DATA(error).
        out->write( error->get_text( ) ).
        RETURN.
    ENDTRY.

    WAIT UP TO 1 SECONDS.
    SELECT * FROM ztaunitflights INTO TABLE @DATA(flights).
    out->write( flights ).
  ENDMETHOD.

  METHOD execute.
    TRY.
        cl_bgmc_process_factory=>get_default(
                    )->create(
                    )->set_name( 'Test process'
                    )->set_operation( NEW zcl_demo_aunit_bgpf( 'abc' )
                    )->save_for_execution( ).

        COMMIT WORK.

      CATCH cx_bgmc INTO DATA(error).
        ROLLBACK WORK.

        RAISE EXCEPTION error.
    ENDTRY.
  ENDMETHOD.

  METHOD execute_2.
    DO 2 TIMES.
      TRY.
          cl_bgmc_process_factory=>get_default(
                      )->create(
                      )->set_name( |Test process { sy-index }|
                      )->set_operation( NEW zcl_demo_aunit_bgpf( |Number { sy-index }| )
                      )->save_for_execution( ).

          COMMIT WORK.

        CATCH cx_bgmc INTO DATA(error).
          ROLLBACK WORK.

          RAISE EXCEPTION error.
      ENDTRY.
    ENDDO.
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
CLASS ltc_bgpf DEFINITION FINAL FOR TESTING
                  DURATION SHORT
                  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut           TYPE REF TO zcl_demo_aunit_bgpf.
    CLASS-DATA bgpf_env TYPE REF TO if_bgmc_test_envir_spy.

    DATA process   TYPE REF TO if_bgmc_process_spy.
    DATA operation TYPE REF TO zcl_demo_aunit_bgpf.

    METHODS test_execute FOR TESTING RAISING cx_static_check.
    METHODS test_execute_2 FOR TESTING RAISING cx_static_check.

    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    METHODS teardown.
    METHODS setup.

ENDCLASS.

CLASS ltc_bgpf IMPLEMENTATION.

  METHOD class_setup.
    bgpf_env = cl_bgmc_test_environment=>create_for_spying( ).

    "Clearing the database table that is filled in the cut
    "Purpose: Illustrating with an ABAP SQL SELECT on the database
    "in the test method that the test method execution does not
    "modify it.
    DELETE FROM ztaunitflights.
  ENDMETHOD.

  METHOD class_teardown.
    bgpf_env->destroy( ).
  ENDMETHOD.

  METHOD teardown.
    bgpf_env->clear( ).
  ENDMETHOD.

  METHOD test_execute.

    cut->execute( ).

    bgpf_env->assert_number_of_processes( 1 ).
    process = bgpf_env->get_process( 1 ).
    process->assert_number_of_operations( 1 ).
    process->assert_is_saved_for_processing( ).
    operation = CAST #( process->get_operation( 1 ) ).

    "Illustrating that the string was not transformed to upper case
    cl_abap_unit_assert=>assert_equals(
      act = operation->get_input( )
      exp = 'abc'
      msg = 'Single scheduled operation should carry input abc.' ).

    SELECT * FROM ztaunitflights
      INTO TABLE @DATA(itab).

    cl_abap_unit_assert=>assert_initial( itab ).

  ENDMETHOD.

  METHOD test_execute_2.
    DATA test_inputs TYPE string_table.

    cut->execute_2( ).
    bgpf_env->assert_number_of_processes( 2 ).

    DO 2 TIMES.
      process = bgpf_env->get_process( sy-index ).
      process->assert_number_of_operations( 1 ).
      process->assert_is_saved_for_processing( ).
      operation = CAST #( process->get_operation( 1 ) ).
      APPEND operation->get_input( ) TO test_inputs.
    ENDDO.

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( line_exists( test_inputs[ table_line = `Number 1` ] ) )
      msg = 'Batch scheduling should include Number 1 input.' ).

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( line_exists( test_inputs[ table_line = `Number 2` ] ) )
      msg = 'Batch scheduling should include Number 2 input.' ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 Using test seams</summary>
  <!-- -->

<br>

- **Class**: `zcl_demo_aunit_test_seams`
- **Purpose**: 
    - Demonstrates ABAP Unit tests using ABAP test seams.
    - Shows how `TEST-SEAM` and `TEST-INJECTION` statements can isolate database access. The examples do not use ABAP frameworks.
- **Global class**: 
    - Includes a method for calculating occupancy rates with a seam (`select_from_db`) that wraps around the database selection logic.
    - Contains a demo method with seams (`ts1`, `ts2`) to illustrate how injected test code functions.
- **Test class**: 
    - Defines local test classes that inject code to control input data and behavior.
    - Verifies occupancy rate calculation scenarios (normal, full, zero, rounding, no data) via injected datasets.
    - Verifies behavior with and without injections (`ts1`, `ts2`).


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
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class demonstrates ABAP Unit tests using ABAP test seams. It shows how TEST-SEAM and TEST-INJECTION statements can be
"! used for code isolation. The class includes a method for calculating occupancy rates with a seam around database selection logic.
"! Additionally, it features a demo method that illustrates how injected test code can function.
CLASS zcl_demo_aunit_test_seams DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    "! Calculates the occupancy rate based on the carrier's flight data
    "!
    "! @parameter carrier_id     | <p class="shorttext synchronized" lang="en">ID of the carrier to calculate occupancy for</p>
    "! @parameter occupancy_rate | <p class="shorttext synchronized" lang="en">Computed occupancy rate of the carrier flights</p>
    METHODS calculate_occupancy_rate IMPORTING carrier_id            TYPE ztaunitflights-carrid
                                     RETURNING VALUE(occupancy_rate) TYPE decfloat34.

    "! <p class="shorttext synchronized" lang="en">Tests behavior using injected test code</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en">Stores a demo string</p>
    METHODS test_seams_demo RETURNING VALUE(result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_aunit_test_seams IMPLEMENTATION.
  METHOD calculate_occupancy_rate.
    TEST-SEAM select_from_db.
      SELECT seatsmax, seatsocc
        FROM ztaunitflights
        WHERE carrid = @carrier_id
        INTO TABLE @DATA(flight_data).
    END-TEST-SEAM.

    DATA total_seatsmax TYPE i.
    DATA total_seatsocc TYPE i.

    LOOP AT flight_data ASSIGNING FIELD-SYMBOL(<flight>).
      total_seatsmax += <flight>-seatsmax.
      total_seatsocc += <flight>-seatsocc.
    ENDLOOP.

    IF total_seatsmax <> 0.
      occupancy_rate = round( val = total_seatsocc / total_seatsmax * 100 dec = 2 ).
    ENDIF.
  ENDMETHOD.

  METHOD test_seams_demo.

    DATA(num) = 0.

    "Empty test seam; code is injected during unit test
    "Check the output when running the class using F9 and
    "the test results when running the unit test.
    TEST-SEAM ts1.
    END-TEST-SEAM.

    IF num = 0.
      result &&= `A`.
    ELSE.
      result &&= `B`.
    ENDIF.

    DATA str TYPE string.
    str = `C`.

    "Empty injection
    "See the test class: The code that is included in the test
    "seam should be excluded from the test. Therefore, the
    "test injection block in the test class is empty.
    TEST-SEAM ts2.
      str = `D`.
    END-TEST-SEAM.

    result &&= str.

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
CLASS ltc_occupancy_rate DEFINITION FINAL FOR TESTING
	DURATION SHORT
	RISK LEVEL HARMLESS.
  	PRIVATE SECTION.
			DATA cut TYPE REF TO zcl_demo_aunit_test_seams.

			METHODS setup.
    		METHODS test_rate FOR TESTING.
    		METHODS test_rate_full FOR TESTING.
    		METHODS test_rate_zero FOR TESTING.
    		METHODS test_rate_round FOR TESTING.
    		METHODS test_rate_no_data FOR TESTING.
ENDCLASS.

CLASS ltc_occupancy_rate IMPLEMENTATION.
	METHOD setup.
		cut = NEW zcl_demo_aunit_test_seams( ).
  	ENDMETHOD.

  	METHOD test_rate.
    		TEST-INJECTION select_from_db.
      			flight_data = VALUE #(
      				( seatsmax = 180 seatsocc = 135 )
      				( seatsmax = 220 seatsocc = 198 )
      				( seatsmax = 300 seatsocc = 280 ) ).
    		END-TEST-INJECTION.

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate_occupancy_rate( carrier_id = 'AA' )
				exp = CONV decfloat34( '87.57' )
				msg = 'AA occupancy rate should be 87.57.' ).
  	ENDMETHOD.

  	METHOD test_rate_full.
    		TEST-INJECTION select_from_db.
      			flight_data = VALUE #(
      				( seatsmax = 150 seatsocc = 150 )
      				( seatsmax = 120 seatsocc = 120 ) ).
    		END-TEST-INJECTION.

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate_occupancy_rate( carrier_id = 'BB' )
				exp = CONV decfloat34( '100' )
				msg = 'BB occupancy rate should be 100.' ).
  	ENDMETHOD.

  	METHOD test_rate_zero.
    		TEST-INJECTION select_from_db.
      			flight_data = VALUE #(
      				( seatsmax = 100 seatsocc = 0 )
      				( seatsmax = 50 seatsocc = 0 ) ).
    		END-TEST-INJECTION.

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate_occupancy_rate( carrier_id = 'CC' )
				exp = CONV decfloat34( '0' )
				msg = 'CC occupancy rate should be 0.' ).
  	ENDMETHOD.

  	METHOD test_rate_round.
    		TEST-INJECTION select_from_db.
      			flight_data = VALUE #(
      				( seatsmax = 300 seatsocc = 100 ) ).
    		END-TEST-INJECTION.

    		cl_abap_unit_assert=>assert_equals(
    			act = cut->calculate_occupancy_rate( carrier_id = 'DD' )
				exp = CONV decfloat34( '33.33' )
				msg = 'DD occupancy rate should be rounded to 33.33.' ).
  	ENDMETHOD.

  	METHOD test_rate_no_data.
    		TEST-INJECTION select_from_db.
      			flight_data = VALUE #( ).
    		END-TEST-INJECTION.

    		cl_abap_unit_assert=>assert_equals(
          act = cut->calculate_occupancy_rate( carrier_id = 'CC' )
	  exp = CONV decfloat34( '0' )
	  msg = 'No seam data should return initial occupancy rate.' ).
  	ENDMETHOD.
ENDCLASS.


**********************************************************************

CLASS ltc_test_seams DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
		DATA cut TYPE REF TO zcl_demo_aunit_test_seams.

		METHODS setup.
    METHODS test_test_seams1 FOR TESTING.
    METHODS test_test_seams2 FOR TESTING.
    METHODS no_injection FOR TESTING.
ENDCLASS.

CLASS ltc_test_seams IMPLEMENTATION.
	METHOD setup.
    cut = NEW zcl_demo_aunit_test_seams( ).
  ENDMETHOD.

  METHOD test_test_seams1.

    TEST-INJECTION ts1.
      num = 1.
    END-TEST-INJECTION.

    TEST-INJECTION ts2.
    END-TEST-INJECTION.

    cl_abap_unit_assert=>assert_equals(
        act = cut->test_seams_demo( )
		exp = `BC`
		msg = 'Injected ts1 and empty ts2 should return BC.' ).

  ENDMETHOD.

  METHOD test_test_seams2.

    TEST-INJECTION ts2.
      str = `E`.
    END-TEST-INJECTION.

    cl_abap_unit_assert=>assert_equals(
        act = cut->test_seams_demo( )
		exp = `AE`
		msg = 'Injected ts2 should override D and return AE.' ).
  ENDMETHOD.

  METHOD no_injection.
    cl_abap_unit_assert=>assert_equals(
            act = cut->test_seams_demo( )
		    exp = `AD`
		    msg = 'Without injections, default seam code should return AD.' ).
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 Test classes located in an external class rather than the class being tested</summary>
  <!-- -->

<br>


- **Purpose**: 
    - Demonstrates ABAP Unit testing with an external test class for scenarios where tests are outside the production class, showing the use of the `"! @testing ...` syntax.            
- **Involved classes**: 
    - `zcl_demo_aunit_external_cl`
        - Represents the class to be tested. 
        - Does not include DOCs.
        - Includes identical calculation methods in both public and private visibility section.
        - To enable the external test, class `zcl_demo_aunit_external_cl` befriends `ztcl_demo_aunit_external_cl`.
    - `ztcl_demo_aunit_external_cl`
        - Represents the class that includes the tests for class `zcl_demo_aunit_external_cl`
        - **Global class**: 
            - The example setup contains a private static method `call_private_calculate` that represents a bridge method to enable testing of the private method, which is accessible here in the global class due to the friendship.
        - **Test class**: 
            - Defines two local test classes for both the public and private method.
            - Test methods cover standard arithmetic operations and edge cases (division by zero, overflow) with explicit assertions.        
            - The test classes use the specification `"!@testing zcl_demo_aunit_external_cl`.
            - The test class testing the private method via the bridge is befriended with the global class to enable instantiation of the global class. The instance is passed in the test, along with demo values.

<table>

<tr>
<td> Class include </td> <td> Code </td>
</tr>

<tr>
<td> 

Global class `zcl_demo_aunit_external_cl`

 </td>

 <td> 

``` abap
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class provides a methods for performing arithmetic operations and demonstrates ABAP Unit
"! testing with an external test class ({@link ztcl_demo_aunit_external_cl}). It includes both public and
"! private calculation methods.
CLASS zcl_demo_aunit_external_cl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS ztcl_demo_aunit_external_cl.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM arithmetic_operation,
             addition,
             subtraction,
             multiplication,
             division,
           END OF ENUM arithmetic_operation.

    "! Performs arithmetic operations based on the specified operation type
    "!
    "! @parameter num1                 | <p class="shorttext synchronized" lang="en">First operand for the arithmetic operation</p>
    "! @parameter num2                 | <p class="shorttext synchronized" lang="en">Second operand for the arithmetic operation</p>
    "! @parameter operation            | <p class="shorttext synchronized" lang="en">Type of arithmetic operation to perform</p>
    "! @parameter result               | <p class="shorttext synchronized" lang="en">The result of the arithmetic operation</p>
    "! @raising cx_sy_arithmetic_error | <p class="shorttext synchronized" lang="en">Exception raised for errors during arithmetic operations</p>
    METHODS calculate IMPORTING num1          TYPE i
                                num2          TYPE i
                                operation     TYPE arithmetic_operation
                      RETURNING VALUE(result) TYPE string
                      RAISING   cx_sy_arithmetic_error.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Executes private arithmetic calculations based on the specified operation type
    "!
    "! @parameter num1                 | <p class="shorttext synchronized" lang="en">First operand for the arithmetic operation</p>
    "! @parameter num2                 | <p class="shorttext synchronized" lang="en">Second operand for the arithmetic operation</p>
    "! @parameter operation            | <p class="shorttext synchronized" lang="en">Type of arithmetic operation to perform</p>
    "! @parameter result               | <p class="shorttext synchronized" lang="en">The result of the arithmetic operation</p>
    "! @raising cx_sy_arithmetic_error | <p class="shorttext synchronized" lang="en">Exception raised for errors during arithmetic operations</p>
    METHODS calculate_private IMPORTING num1          TYPE i
                                        num2          TYPE i
                                        operation     TYPE arithmetic_operation
                              RETURNING VALUE(result) TYPE string
                              RAISING   cx_sy_arithmetic_error.
ENDCLASS.



CLASS zcl_demo_aunit_external_cl IMPLEMENTATION.

  METHOD calculate.
    result = SWITCH #( operation WHEN addition THEN |{ num1 + num2 STYLE = SIMPLE }|
                                 WHEN subtraction THEN |{ num1 - num2 STYLE = SIMPLE }|
                                 WHEN multiplication THEN |{ num1 * num2 STYLE = SIMPLE }|
                                 WHEN division THEN |{ num1 / num2 STYLE = SIMPLE }| ).
  ENDMETHOD.

  METHOD calculate_private.
    result = SWITCH #( operation WHEN addition THEN |{ num1 + num2 STYLE = SIMPLE }|
                                 WHEN subtraction THEN |{ num1 - num2 STYLE = SIMPLE }|
                                 WHEN multiplication THEN |{ num1 * num2 STYLE = SIMPLE }|
                                 WHEN division THEN |{ num1 / num2 STYLE = SIMPLE }| ).
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

Global class `ztcl_demo_aunit_external_cl`

 </td>

 <td> 

``` abap
"! <p class="shorttext synchronized" lang="en">ABAP Unit Demo</p>
"!
"! This class represents the class that includes the tests for class {@link zcl_demo_aunit_external_cl}.
"! {@link zcl_demo_aunit_external_cl} defines both a public and private method that should be tested. The
"! test class tests the private method via a bridge. To do so, this class is befriended with {@link zcl_demo_aunit_external_cl}
"! and its local class to enable instantiation of the global class in the local test class. In the test, the instance is passed,
"! along with demo values.
CLASS ztcl_demo_aunit_external_cl DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">Invokes a private calculation method for unit tests</p>
    "!
    "! @parameter cut                  | <p class="shorttext synchronized" lang="en">Instance reference of the class to test</p>
    "! @parameter num1                 | <p class="shorttext synchronized" lang="en">First operand for the arithmetic operation</p>
    "! @parameter num2                 | <p class="shorttext synchronized" lang="en">Second operand for the arithmetic operation</p>
    "! @parameter operation            | <p class="shorttext synchronized" lang="en">Type of arithmetic operation to perform</p>
    "! @parameter result               | <p class="shorttext synchronized" lang="en">String result of the arithmetic operation</p>
    "! @raising cx_sy_arithmetic_error | <p class="shorttext synchronized" lang="en">Exception raised for arithmetic errors</p>
    CLASS-METHODS call_private_calculate
      IMPORTING cut           TYPE REF TO zcl_demo_aunit_external_cl
                num1          TYPE i
                num2          TYPE i
                operation     TYPE zcl_demo_aunit_external_cl=>arithmetic_operation
      RETURNING VALUE(result) TYPE string
      RAISING   cx_sy_arithmetic_error.
ENDCLASS.



CLASS ztcl_demo_aunit_external_cl IMPLEMENTATION.
  METHOD call_private_calculate.
    result = cut->calculate_private(
      num1      = num1
      num2      = num2
      operation = operation ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>


<tr>
<td> 

CCAU include of `ztcl_demo_aunit_external_cl` (Test Classes tab in ADT)

 </td>

 <td> 

``` abap
"!@testing zcl_demo_aunit_external_cl
CLASS ltc_calculate DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_demo_aunit_external_cl.

    METHODS setup.
    METHODS test_addition FOR TESTING RAISING cx_static_check.
    METHODS test_subtraction FOR TESTING RAISING cx_static_check.
    METHODS test_multiplication FOR TESTING RAISING cx_static_check.
    METHODS test_division FOR TESTING RAISING cx_static_check.
    METHODS test_division_by_zero FOR TESTING RAISING cx_static_check.
    METHODS test_overflow FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_calculate IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_demo_aunit_external_cl( ).
  ENDMETHOD.

  METHOD test_addition.
    DATA(result) = cut->calculate(
      num1      = 10
      num2      = 15
      operation = zcl_demo_aunit_external_cl=>addition ).

    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = `25` ).
  ENDMETHOD.

  METHOD test_subtraction.
    DATA(result) = cut->calculate(
      num1      = 20
      num2      = 7
      operation = zcl_demo_aunit_external_cl=>subtraction ).

    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = `13` ).
  ENDMETHOD.

  METHOD test_multiplication.
    DATA(result) = cut->calculate(
      num1      = 6
      num2      = 8
      operation = zcl_demo_aunit_external_cl=>multiplication ).

    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = `48` ).
  ENDMETHOD.

  METHOD test_division.
    DATA(result) = cut->calculate(
      num1      = 42
      num2      = 6
      operation = zcl_demo_aunit_external_cl=>division ).

    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = `7` ).
  ENDMETHOD.

  METHOD test_division_by_zero.
    TRY.
        cut->calculate(
          num1      = 1
          num2      = 0
          operation = zcl_demo_aunit_external_cl=>division ).
        cl_abap_unit_assert=>fail( msg = `Expected arithmetic error for division by zero.` ).
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_overflow.
    TRY.
        cut->calculate(
          num1      = 2147483647
          num2      = 1
          operation = zcl_demo_aunit_external_cl=>addition ).
        cl_abap_unit_assert=>fail( msg = `Expected arithmetic overflow.` ).
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS ltc_calculate_private_bridge DEFINITION DEFERRED.
CLASS ztcl_demo_aunit_external_cl DEFINITION LOCAL FRIENDS ltc_calculate_private_bridge.

"!@testing zcl_demo_aunit_external_cl
CLASS ltc_calculate_private_bridge DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_demo_aunit_external_cl.

    METHODS setup.
    METHODS test_private_addition FOR TESTING RAISING cx_static_check.
    METHODS test_private_subtraction FOR TESTING RAISING cx_static_check.
    METHODS test_private_multiplication FOR TESTING RAISING cx_static_check.
    METHODS test_private_division FOR TESTING RAISING cx_static_check.
    METHODS test_private_division_by_zero FOR TESTING RAISING cx_static_check.
    METHODS test_private_overflow FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_calculate_private_bridge IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_demo_aunit_external_cl( ).
  ENDMETHOD.

  METHOD test_private_addition.
    DATA(result) = ztcl_demo_aunit_external_cl=>call_private_calculate(
      cut    = cut
      num1      = 9
      num2      = 4
      operation = zcl_demo_aunit_external_cl=>addition ).

    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = `13` ).
  ENDMETHOD.

  METHOD test_private_subtraction.
    DATA(result) = ztcl_demo_aunit_external_cl=>call_private_calculate(
      cut    = cut
      num1      = 20
      num2      = 7
      operation = zcl_demo_aunit_external_cl=>subtraction ).

    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = `13` ).
  ENDMETHOD.

  METHOD test_private_multiplication.
    DATA(result) = ztcl_demo_aunit_external_cl=>call_private_calculate(
      cut    = cut
      num1      = 6
      num2      = 8
      operation = zcl_demo_aunit_external_cl=>multiplication ).

    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = `48` ).
  ENDMETHOD.

  METHOD test_private_division.
    DATA(result) = ztcl_demo_aunit_external_cl=>call_private_calculate(
      cut    = cut
      num1      = 42
      num2      = 6
      operation = zcl_demo_aunit_external_cl=>division ).

    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = `7` ).
  ENDMETHOD.

  METHOD test_private_division_by_zero.
    TRY.
        ztcl_demo_aunit_external_cl=>call_private_calculate(
          cut    = cut
          num1      = 1
          num2      = 0
          operation = zcl_demo_aunit_external_cl=>division ).
        cl_abap_unit_assert=>fail( msg = `Expected arithmetic error for private division by zero.` ).
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_private_overflow.
    TRY.
        ztcl_demo_aunit_external_cl=>call_private_calculate(
          cut    = cut
          num1      = 2147483647
          num2      = 1
          operation = zcl_demo_aunit_external_cl=>addition ).
        cl_abap_unit_assert=>fail( msg = `Expected arithmetic overflow.` ).
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>   

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>  

### main Branch

- [zcl_demo_abap_unit_test](./src/zcl_demo_abap_unit_test.clas.abap): 
  - Explores test classes and test/special methods, implementing and injecting test doubles (constructor injection, back door injection, test seams)
  - The example class is supported by other repository objects such as the interface [zdemo_abap_get_data_itf](./src/zdemo_abap_get_data_itf.intf.abap).
- [zcl_demo_abap_unit_tdf](./src/zcl_demo_abap_unit_tdf.clas.abap): 
  - Explores creating test doubles using ABAP frameworks
  - It also explores test classes that are contained in an external class rather than the class being tested (`"! @testing ...` syntax).
  - The example class is supported by two additional classes: [zcl_demo_abap_unit_dataprov](./src/zcl_demo_abap_unit_dataprov.clas.abap) (includes dependent-on-components that are replaced by test doubles when running ABAP Unit tests) and [ztcl_demo_abap_unit_tdf_testcl](./src/ztcl_demo_abap_unit_tdf_testcl.clas.abap) (includes the test class for testing class `zcl_demo_abap_unit_tdf`; the local test class includes the `"! @testing ...` syntax).


> [!NOTE]
> - The executable examples contain comments in the code for more information.
> - The examples are designed to display output in the console when they are run using `F9`. However, the focus is on ABAP Unit tests. Choose `Ctrl/Cmd + Shift + F10` to run the unit tests.
> - The example classes are intentionally simplified and nonsemantic, designed to highlight basic unit tests and explore framework classes and methods. 
> - They are not meant to serve as best practices and a model for proper unit test design. The primary focus is on syntax, additions, and their basic functionality. Always devise your own solutions for each unique case. 
> - Refer to the code comments, SAP Help Portal and class documentation for additional information.
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)