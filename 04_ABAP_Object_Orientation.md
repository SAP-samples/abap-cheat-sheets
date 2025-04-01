<a name="top"></a>

# ABAP Object Orientation

- [ABAP Object Orientation](#abap-object-orientation)
  - [Classes and Objects](#classes-and-objects)
    - [Creating Classes](#creating-classes)
      - [Creating a Global Class](#creating-a-global-class)
      - [Creating a Local Class](#creating-a-local-class)
      - [Additions in the Class Declaration Part](#additions-in-the-class-declaration-part)
      - [Excursion: Class Pool and Include Programs](#excursion-class-pool-and-include-programs)
    - [Visibility of Components](#visibility-of-components)
      - [Creating the Visibility Sections](#creating-the-visibility-sections)
    - [Defining Components](#defining-components)
      - [Class Attributes](#class-attributes)
      - [Methods](#methods)
      - [Parameter Interface](#parameter-interface)
      - [Formal and Actual Parameters](#formal-and-actual-parameters)
      - [Complete Typing of Formal Parameters](#complete-typing-of-formal-parameters)
      - [Generic Typing of Formal Parameters](#generic-typing-of-formal-parameters)
      - [Defining Parameters as Optional](#defining-parameters-as-optional)
      - [Defining Input Parameters as Preferred](#defining-input-parameters-as-preferred)
      - [Constructors](#constructors)
  - [Working with Objects and Components](#working-with-objects-and-components)
    - [Declaring Object Reference Variables](#declaring-object-reference-variables)
    - [Creating Objects](#creating-objects)
    - [Working with Reference Variables](#working-with-reference-variables)
    - [Accessing Attributes](#accessing-attributes)
    - [Calling Methods](#calling-methods)
      - [Excursion: Inline Declarations, Returning Parameters](#excursion-inline-declarations-returning-parameters)
    - [Self-Reference me](#self-reference-me)
    - [Method Chaining and Chained Attribute Access](#method-chaining-and-chained-attribute-access)
    - [Excursion: Example Class](#excursion-example-class)
  - [Inheritance](#inheritance)
    - [Additions Related to Inheritance and Instantiation](#additions-related-to-inheritance-and-instantiation)
    - [Excursion: Inheritance Example](#excursion-inheritance-example)
  - [Polymorphism and Casting (Upcast/Downcast)](#polymorphism-and-casting-upcastdowncast)
    - [Example with Local Classes Demonstrating Upcasts and Downcasts](#example-with-local-classes-demonstrating-upcasts-and-downcasts)
    - [Upcasts and Downcasts Using the RTTS Inheritance Tree](#upcasts-and-downcasts-using-the-rtts-inheritance-tree)
    - [Checking the Dynamic Type of Object Reference Variables](#checking-the-dynamic-type-of-object-reference-variables)
  - [Interfaces](#interfaces)
    - [Defining Interfaces](#defining-interfaces)
    - [Implementing Interfaces](#implementing-interfaces)
    - [Additions Related to Interface Implementations](#additions-related-to-interface-implementations)
    - [Interface Reference Variables, Accessing Objects and Components](#interface-reference-variables-accessing-objects-and-components)
    - [Excursion: Example Interface](#excursion-example-interface)
  - [Friendship](#friendship)
    - [Friendship between Global and Local Classes](#friendship-between-global-and-local-classes)
  - [Excursions](#excursions)
    - [Events](#events)
    - [ABAP Examples of Design Patterns in Object-Oriented Programming](#abap-examples-of-design-patterns-in-object-oriented-programming)
    - [Class-Based and Classic Exceptions](#class-based-and-classic-exceptions)
    - [ABAP Unit Tests](#abap-unit-tests)
    - [ABAP Doc Comments](#abap-doc-comments)
    - [Escape Character](#escape-character)
  - [More Information](#more-information)
  - [Executable Examples](#executable-examples)


This ABAP cheat sheet provides an overview on selected syntax options and concepts related to ABAP object orientation.

> **💡 Note**<br>
> - The cheat sheet is supported by code snippets and an executable example. They are **not** suitable as role models for object-oriented design. Their primary focus is on the syntax and functionality. 
> - For more details, refer to the respective topics in the ABAP Keyword Documentation. Find an overview in the topic [ABAP Objects - Overview](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_objects_oview.htm).
> - The [executable examples](#executable-examples) reflect several points and code snippets covered in the cheat sheet.


## Classes and Objects

Object-oriented programming in ABAP means dealing with
[classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_glosry.htm "Glossary Entry")
and
[objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_glosry.htm "Glossary Entry").

Objects ...

-   are
    [instances](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_glosry.htm "Glossary Entry")
    of a
    [type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_glosry.htm "Glossary Entry").
    In this context, they are instances of a
    [class](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_glosry.htm "Glossary Entry").
    The terms *object* and *instance* are used synonymously.
-   exist in the [internal
    session](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninternal_session_glosry.htm "Glossary Entry")
    of an [ABAP
    program](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm "Glossary Entry").


Classes ...
-   are templates for objects, i. e. they determine how
    all instances of a class are set up. All instances are created (i.e they are [instantiated](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstantiation_glosry.htm "Glossary Entry")) based on this template and, thus, have the same setup.
    -   To give an example: If, for example, a vehicle represents a class, then the
        instances of the class `vehicle` have the same setup.
        That means they all share the same kind of components like a brand, model and color or the same functionality like the acceleration or braking distance.
        However, the values of these components are different from instance to instance. For example, one
        instance is a red sedan of brand A having a certain
        acceleration; another instance is a black SUV of brand B and so on. You can create an object (or instance respectively) that stands
        for an actual vehicle which you can work with. You might create any number of objects that are based on such a class - if instantiation is allowed.
-   contain
    [components](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomponent_glosry.htm "Glossary Entry"):
    -   [Attributes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenattribute_glosry.htm "Glossary Entry")
        of the objects (the data object declarations)
    -   [Methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmethod_glosry.htm "Glossary Entry")
        that determine the behavior of an object
    -   [Events](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenevent_glosry.htm "Glossary Entry") to trigger the processing of ABAP code


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Classes

You can either create local or global classes:

<table>
<tr>
    <td><a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlocal_class_glosry.htm">Local classes</a></td>
    <td><ul><li>can be defined within an <a href="[https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlocal_class_glosry.htm](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm)">ABAP program</a> such as in the <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenccimp_glosry.htm">CCIMP include</a> of global classes (Local Types tab in ADT) or in executable programs ("reports"; in <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm">Standard ABAP</a> only)</li><li>can only be used in the ABAP program in which the class is defined</li></ul></td>
</tr>
<tr>
    <td><a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_class_glosry.htm">Global
classes</a></td>
    <td><ul><li>are defined as
    <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_type_glosry.htm">global types</a>, i. e. they are visible as a repository object - in contrast to local classes. As a global type, they can be used - as the name implies - globally in other ABAP programs or global classes</li><li>are declared in <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_pool_glosry.htm">class pools</a> that contain a <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenccimp_glosry.htm">CCIMP include</a> and other <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninclude_program_glosry.htm">include programs</a></li></ul> </td>
</tr>
</table>

> **💡 Note**<br>
> - If a class is only used in one ABAP program, creating a local class is enough. However, if you choose to create a global class, you must bear in mind that such a class can be used everywhere. Consider the impact on the users of the global class when you change, for example, the visibility section of a component or you delete it.
> - Apart from ADT, global classes can also be created in the ABAP Workbench (`SE80`) or with transaction `SE24` in [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm).

Basic structure of classes:
- [Declaration part](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeclaration_part_glosry.htm "Glossary Entry") that includes declarations of the class components.
- [Implementation part](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenimplementation_part_glosry.htm "Glossary Entry") that includes method implementations.
- Both are introduced by `CLASS` and ended by `ENDCLASS`.

#### Creating a Global Class
The code snippet shows a basic skeleton of a global class. There are [further
additions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_options.htm)
possible for the declaration part.

``` abap
"Declaration part
CLASS global_class DEFINITION
  PUBLIC                       "Makes the class a global class in the class library.
  FINAL                        "Means that no subclasses can be derived from this class.
  CREATE PUBLIC.               "This class can be instantiated anywhere it is visible.

    ... "Here go the declarations for all components and visibility sections.

ENDCLASS.

"Implementation part
CLASS global_class IMPLEMENTATION.

    ... "Here go the method implementations.
        "Only required if you declare methods in the DEFINITION part.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Creating a Local Class

- You can create local classes, for example, in the [CCIMP include](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenccimp_glosry.htm) (*Local Types* tab in ADT) of a [class pool](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_pool_glosry.htm). 
- Local classes are used in their own ABAP program. While dynamic access beyond program boundaries is possible, it is not recommended.
- In the cheat sheet, local classes are used in several sections, particularly for self-contained examples that require multiple classes.

The following snippet shows the skeleton of local class declaration. 
- The `PUBLIC` addition makes a class a global class in the class library. This is not possible for local classes.
- It does not specify `CREATE ...`. Note that `CREATE PUBLIC` is the default, which means that not specifying any `CREATE ...` addition makes the class implicitly specified with `CREATE PUBLIC`.
- More additions are possible. They are covered in the following sections.

``` abap
"Declaration part
CLASS local_class DEFINITION.

    ... "Here go the declarations for all components and visibility sections.
        "You should place the declarations at the beginning of the program.

ENDCLASS.

"Implementation part
CLASS local_class IMPLEMENTATION.

    ...  "Here go the method implementations.
         "Only required if you declare methods in the declaration part.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


#### Additions in the Class Declaration Part

This section covers a selection of additions to declare classes. They are also covered in other sections below, e.g. [Additions Related to Inheritance and Instantiation](#additions-related-to-inheritance-and-instantiation). Find more information on the additions in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_options.htm). The additions assume dealing with public classes, however, many of the additions are also possible for local classes. 

<table>

<tr>
<td> Addition </td> <td> Notes </td>
</tr>

<tr>
<td> 

`PUBLIC`

 </td>

 <td> 

Creates a global class

 </td>
</tr>

<tr>
<td> 

`FINAL`

 </td>

 <td> 

The class does not allow inheritance.

 </td>
</tr>

<tr>
<td> 

`CREATE PUBLIC`

 </td>

 <td> 

The class is instantiable anywhere. Note that not specifying a `CREATE ...` addition means the class specifies  `CREATE PUBLIC` by default.

 </td>
</tr>

<tr>
<td> 

`CREATE PROTECTED`

 </td>

 <td> 

The class can only be instantiated in methods of its [subclasses](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubclass_glosry.htm "Glossary Entry"), of the class itself, and of its [friends](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfriend_glosry.htm "Glossary Entry").

 </td>
</tr>

<tr>
<td> 

`CREATE PRIVATE`

 </td>

 <td> 

The class can only be instantiated in methods of the class itself or of its friends. Hence, it cannot be instantiated as an inherited component of subclasses.

 </td>
</tr>

<tr>
<td> 

`INHERITING FROM superclass`

 </td>

 <td> 

As the name implies, it is used to inherit from a visible [superclass](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensuperclass_glosry.htm). If the addition is not specified, the created class implicitly inherits from the predefined empty, abstract class `object` (the root object).

 </td>
</tr>

<tr>
<td> 

`ABSTRACT`

 </td>

 <td> 

To define [abstract](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabstract_glosry.htm) classes. These classes cannot be instantiated. These classes can contain both abstract methods and non-abstract methods. Abstract methods can only be implemented in [subclasses](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubclass_glosry.htm) by redefinition. See a simple implementation example [here](#excursion-example-interface).

 </td>
</tr>

<tr>
<td> 

`[GLOBAL|LOCAL] FRIENDS class`

 </td>

 <td> 

- Used to define [friendships](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfriend_glosry.htm) (also possible for interfaces). Friends of a class have unrestricted access to all components of that class. 
- `GLOBAL FRIENDS`: Used in global classes (together with the `PUBLIC` addition) to grant friendship to other global classes and interfaces
- `FRIENDS`: For local classes, e.g. local classes granting friendship to other local classes or the global class of the class pool
- `LOCAL FRIENDS`: Used for global classes to grant friendship to local classes and interfaces in its own class pool. However, it is a dedicated statement, as shown in the [Friendship](#friendship) section. 

 </td>
</tr>

<tr>
<td> 

`FOR TESTING`

 </td>

 <td> 

For [ABAP Unit](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_unit_glosry.htm) tests. Find more information in the [ABAP Unit Tests](14_ABAP_Unit_Tests.md) cheat sheet.

 </td>
</tr>

<tr>
<td> 

`FOR BEHAVIOR OF`

 </td>

 <td> 

To define [ABAP behavior pools](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbehavior_pool_glosry.htm). Find more information in the [ABAP for RAP: Entity Manipulation Language (ABAP EML)](08_EML_ABAP_for_RAP.md) cheat sheet.

 </td>
</tr>

<tr>
<td> 

`DEFINITION DEFERRED`

 </td>

 <td> 

Makes a local class known in a program before the actual class definition. It is typically used in test classes of ABAP Unit. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_deferred.htm).

 </td>
</tr>


</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>


#### Excursion: Class Pool and Include Programs

- A class pool is an ABAP program containing the definition of one global class (*Global Class* tab in ADT)
- Global classes are marked as such with the `PUBLIC` addition to the `CLASS` statement: `CLASS zcl_demo_abap DEFINITION PUBLIC ...` 
- Additionally, a class pool can also contain local classes that are defined in dedicated include programs (CCDEF and the other include names are internal names the include programs end with):
  - CCDEF include (*Class-relevant Local Types* tab in ADT): Is included in front of the declaration part of the global class
  - CCIMP include (*Local Types* tab in ADT): Is included behind the declaration part and in front of the implementation part of the global class
  - CCAU include (*Test classes* tab in ADT): Test include; contains ABAP Unit test classes 

The following simplified example demonstrates include programs.

<details>
   <summary>🟢 Click to expand for more details</summary>
  <!-- -->

  Global class:
- Create a new global class (the example uses the name `zcl_demo_abap`) and copy and paste the following code in the *Global Class* tab in ADT.
- The method in the private section makes use of local types that are defined in the CCDEF include. In the example the method is deliberately included in the private visibility section. Having it like this in the public section is not possible due to the use of local types.
- The `if_oo_adt_classrun~main` implementation contains method calls to this method. It also contains method calls to a method implemented in a local class in the CCIMP include.
- As a prerequisite to activate the class, you also need to copy and paste the code snippets further down for the CCDEF and CCIMP include. Once you have pasted the code, the syntax errors in the global class will disappear.
- You can run the class using F9. Some output is displayed.
- When you have copied and pasted the CCAU code snippet for the simple ABAP Unit test, and activated the class, you can choose *CTRL+Shift+F10* in ADT to run the ABAP Unit test. Alternatively, you can make a right click in the class code, choose *Run As* and *4 ABAP Unit Test*.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
    "This method uses types (data type c1 and the local exception class)
    "defined in the CCDEF include
    METHODS calculate IMPORTING num1          TYPE i
                                operator      TYPE c1
                                num2          TYPE i
                      RETURNING VALUE(result) TYPE i
                      RAISING   lcx_wrong_operator.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "---- The method called has formal parameters using type declarations from the CCDEF include ----
    TRY.
        DATA(result1) = calculate( num1 = 10 operator = '+' num2 = 4 ).
        out->write( data = result1 name = `result1` ).
      CATCH lcx_wrong_operator.
        out->write( `Operator not allowed` ).
    ENDTRY.

    TRY.
        DATA(result2) = calculate( num1 = 10 operator = '-' num2 = 4 ).
        out->write( data = result2 name = `result2` ).
      CATCH lcx_wrong_operator.
        out->write( `Operator not allowed` ).
    ENDTRY.

    TRY.
        DATA(result3) = calculate( num1 = 10 operator = '*' num2 = 4 ).
        out->write( data = result3 name = `result3` ).
      CATCH lcx_wrong_operator.
        out->write( `Operator not allowed` ).
    ENDTRY.

    "---- Using local class implemented in the CCIMP include ----
    DATA(hi1) = lcl_demo=>say_hello( ).
    out->write( data = hi1 name = `hi1` ).
    DATA(hi2) = lcl_demo=>say_hello( xco_cp=>sy->user( )->name ).
    out->write( data = hi2 name = `hi2` ).

    "--------------- Test include (CCAU) ---------------
    "For running the ABAP Unit test, choose CTRL+Shift+F10 in ADT.
    "Or you can make a right click in the class code, choose
    "'Run As' and '4 ABAP Unit Test'.
  ENDMETHOD.

  METHOD calculate.
    result = SWITCH #( operator
                       WHEN '+' THEN num1 + num2
                       WHEN '-' THEN num1 - num2
                       ELSE THROW lcx_wrong_operator( ) ).
  ENDMETHOD.

ENDCLASS.
```
Code snippet for the CCDEF include:
```abap
CLASS lcx_wrong_operator DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

TYPES c1 TYPE c LENGTH 1.
```

Code snippet for the CCIMP include:
```abap
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: say_hello IMPORTING name TYPE string optional
                             RETURNING VALUE(hi) type string.

ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.

  METHOD say_hello.
    hi = |Hallo{ COND #( WHEN name IS SUPPLIED THEN ` ` && name ) }!|.
  ENDMETHOD.

ENDCLASS.
```

Code snippet for the test include (CCAU):
```abap
CLASS ltc_test DEFINITION DEFERRED.
CLASS zcl_demo_abap DEFINITION LOCAL FRIENDS ltc_test.

CLASS ltc_test DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_calculate FOR TESTING.

ENDCLASS.

CLASS ltc_test IMPLEMENTATION.

  METHOD test_calculate.
    "Creating an object of the class under test
    DATA(ref_cut) = NEW zcl_demo_abap( ).

    "Calling method that is to be tested
    TRY.
        DATA(result1) = ref_cut->calculate( num1 = 10 operator = '+' num2 = 4 ).
      CATCH lcx_wrong_operator.
    ENDTRY.

    TRY.
        DATA(result2) = ref_cut->calculate( num1 = 10 operator = '-' num2 = 4 ).
      CATCH lcx_wrong_operator.
    ENDTRY.

    "Assertions
    cl_abap_unit_assert=>assert_equals(
          act = result1
          exp = 14
          quit = if_abap_unit_constant=>quit-no ).

    cl_abap_unit_assert=>assert_equals(
          act = result2
          exp = 6
          quit = if_abap_unit_constant=>quit-no ).
  ENDMETHOD.

ENDCLASS.
```
</details>



<p align="right"><a href="#top">⬆️ back to top</a></p>

### Visibility of Components

In the class declaration part, you specify three [visibility sections](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenvisibility_section_glosry.htm "Glossary Entry") and include class components to define their visibility. These visibility sections serve the purpose of encapsulation in ABAP Objects. For example, you do not want to make certain components publicly available for all users. The visibility sections are as follows:

<table>
<tr>
  <td><pre>PUBLIC SECTION.</pre></td>
    <td>Components declared in this section can be accessed from within the class and from all users of the class.</td>
</tr>
<tr>
 <td><pre>PROTECTED SECTION.</pre></td>
    <td>Components declared in this section can be
    accessed from within the class and <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubclass_glosry.htm">subclasses</a> as well as <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfriend_glosry.htm">friends</a>.</td>
</tr>
  <tr>
 <td><pre>PRIVATE SECTION.</pre></td>
    <td>Components declared in this section can only be accessed from within the class in which they are declared and its friends.</td>
</tr>
</table>

Summary:

| Visible for  | PUBLIC SECTION  |  PROTECTED SECTION |  PRIVATE SECTION |
|---|---|---|---|
|  Same class and its friends |  X | X  |  X |
| Any subclasses	  |  X | X  | -  |
|  Any repository objects | X  |  - | -  |


#### Creating the Visibility Sections
At least one section must be specified.
``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC                       
  FINAL                        
  CREATE PUBLIC.  

    PUBLIC SECTION.
      "Here go the components.
    PROTECTED SECTION.
      "Here go the components.
    PRIVATE SECTION.
      "Here go the components.
ENDCLASS.

...
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Defining Components

All components, i. e.
- attributes (using `TYPES`, `DATA`, `CLASS-DATA`, and `CONSTANTS` for data types and data
objects),
- methods (using `METHODS` and `CLASS-METHODS`),
- events (using `EVENTS` and `CLASS-EVENTS`) as well as
- interfaces (only in the public visibility section),

are declared in the declaration part of the class. There, they must be
assigned to a visibility section.

Two
kinds of components are to be distinguished when, for example, looking at declarations using `DATA` and `CLASS-DATA` having a preceding `CLASS-`:

-   [Instance components](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_component_glosry.htm "Glossary Entry"):
    Components that exist separately for each instance and can only be accessed in instances of a class.
-   [Static components](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_component_glosry.htm "Glossary Entry") (the declarations with `CLASS-`):
    Components that exist only once per class. They do no not exclusively exist for specific instances. They can be addressed using the name of the class.

#### Class Attributes

-   The attributes of a class (or interface) mean the data objects declared within a
    class (or interface).
-   [Instance
    attributes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_attribute_glosry.htm "Glossary Entry")
    (`DATA`): Determine the state of a objects of a class. The data
    is only valid in the context of an instance. As shown further down,
    instance attributes can only be accessed via an [object reference
    variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm "Glossary Entry").
-   [Static
    attributes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_attribute_glosry.htm "Glossary Entry")
    (`CLASS-DATA`): Their content is independent of instances of
    a class and, thus, valid for all instances.  That means that if you change such a static
    attribute, the change is visible in all instances. As shown further down,
    static attributes can be accessed by both using an object reference variable and using the class name without a prior creation of an instance.


> **💡 Note**<br>
> - You can declare constant data objects that should not be
changed using
[`CONSTANTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconstants.htm)
statements. You specify the values for the constants (which are also static attributes) when you declare
them in the declaration part of a class.
> - The addition
[`READ-ONLY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_options.htm#!ABAP_ADDITION_2@2@)
can be used in the public visibility section. Effect:
>   - Can be read from outside of the class
>   - Cannot be changed from outside
>   - Can only be changed using methods of the class or its subclasses
> - Note that when creating attributes in the public visibility section, they are globally visible and can therefore be globally used. Note the consequences on the users when changing attributes in the public visibility section (e.g. making an attribute read-only at a later point in time when, for example, other classes use the attribute).

Declaring attributes in visibility sections. In the code snippet below, all attributes are declared in the public section.
``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC                       
  FINAL                        
  CREATE PUBLIC.  

    PUBLIC SECTION.
      TYPES some_type TYPE c LENGTH 3.               "Type declaration

      DATA: inst_number TYPE i,                      "Instance attributes
            inst_string TYPE string,
            dobj_r_only TYPE c LENGTH 5 READ-ONLY.   "Read-only attribute

      CLASS-DATA: stat_number TYPE i,                "Static attributes
                  stat_char   TYPE c LENGTH 3.

      CONSTANTS const_num TYPE i VALUE 123.          "Non-changeable constant

    PROTECTED SECTION.
      "Here go more attributes if needed.

    PRIVATE SECTION.
      "Here go more attributes if needed.

ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.

      ... "Here go all method implementations.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Methods

-   Are internal
    [procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm "Glossary Entry")
    determining the behavior of the class.
-   Can access all of the attributes of a class and, if not defined
    otherwise, change their content.
-   Have a [parameter
    interface](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenparameter_interface_glosry.htm "Glossary Entry")
    (also known as
    [signature](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensignature_glosry.htm "Glossary Entry"))
    with which methods can get values to work with when being called and pass values
    back to the caller (see the notes on formal and actual parameters below).
-   [Static
    methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_method_glosry.htm "Glossary Entry")
    can only access static attributes of a class and trigger static
    events. You declare them using `CLASS-METHODS` statements in
    a visibility section.
-   [Instance
    methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_method_glosry.htm "Glossary Entry")
    can access all of the attributes of a class and trigger all events.
    You declare them using `METHODS` statements in a visibility
    section. Note that you must create an instance of a class first before using instance methods.
- `CLASS-METHODS` and `METHODS` can be followed by a colon to list one or more methods, separated by commas, or without a colon to declare a single method.


The following code snippet (which anticipates aspects described in the following sections, such as specifying the method signature, constructors etc.) shows multiple method definitions in the public section of a global class. Most of the formal
parameters of the demo methods below are defined by just using the
parameter name. This means passing by reference (returning parameters
require to be passed by value).
``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC                       
  FINAL                        
  CREATE PUBLIC. 

    PUBLIC SECTION.
      METHODS: inst_meth1,                                      "instance methods

               inst_meth2 IMPORTING a TYPE string,

               inst_meth3 IMPORTING b TYPE i
                          EXPORTING c TYPE i,

               inst_meth4 IMPORTING d TYPE string
                          RETURNING VALUE(e) TYPE string,

               "Note that method declarations should be done with care. An example
               "as follows may not be advisable, e.g. specifying multiple output
               "parameters (both exporting and returning parameters for a method).
               "As is valid for all examples in the cheat sheet, the focus is on
               "syntax options.
               inst_meth5 IMPORTING f TYPE i
                          EXPORTING g TYPE i
                          CHANGING  h TYPE string
                          RETURNING VALUE(i) TYPE i
                          RAISING   cx_sy_zerodivide,

              constructor IMPORTING j TYPE i.                   "instance constructor with importing parameter

      CLASS-METHODS: stat_meth1,                                "static methods  

                     stat_meth2 IMPORTING k TYPE i              
                                EXPORTING l TYPE i,

                     class_constructor,                         "static constructor

                     "Options of formal parameter definitions
                     stat_meth3 IMPORTING VALUE(m) TYPE i,       "pass by value
                     stat_meth4 IMPORTING REFERENCE(n) TYPE i,   "pass by reference
                     stat_meth5 IMPORTING o TYPE i,              "same as n; the specification of REFERENCE(...) is optional
                     stat_meth6 RETURNING VALUE(p) TYPE i,       "pass by value once more (note: it's the only option for returning parameters)

                     "OPTIONAL/DEFAULT additions
                     stat_meth7 IMPORTING q TYPE i DEFAULT 123
                                          r TYPE i OPTIONAL,

                     "The examples above use a complete type for 
                     "the parameter specification. Generic types
                     "are possible.
                     stat_meth8 IMPORTING s TYPE any           "Any data type
                                          t TYPE any table     "Any internal table type                 
                                          u TYPE clike.        "Character-like types (c, n, string, d, t and character-like flat structures)

ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
   METHOD inst_meth1.
      ...
   ENDMETHOD.

  ... "Further method implementations. Note that all declared methods must go here.
ENDCLASS.
```



<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Parameter Interface

In the simplest form, methods can have no parameter at all. Apart from that, methods can be defined with the following parameters:

| Addition  | Details  |
|---|---|
|`IMPORTING`|Defines one or more input parameters to be imported by the method.  |
|`EXPORTING`|Defines one or more output parameters to be exported by the method.  |
|`CHANGING`|Defines one or more input or output parameters, i. e. that can be both imported and exported.  |
|`RETURNING`|For [functional methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfunctional_method_glosry.htm "Glossary Entry"), i. e. such methods have only one `RETURNING` parameter that can be defined. As an output parameter like the `EXPORTING` parameter, `RETURNING` parameters pass back values (note that the formal parameters of returning parameters must be passed by value as covered below; the parameter must be [completely typed](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplete_typing_glosry.htm)). In contrast to `EXPORTING` for which multiple parameters can be specified, only one `RETURNING` parameter can be specified in a method. If you only need one output parameter, you can benefit from using a `RETURNING` parameter by shortening the method call and enabling method chaining. Another big plus is that such functional methods can, for example, be used in expressions. In case of standalone method calls, the returned value can be accessed using the addition `RECEIVING`.  |
|`RAISING` | Used to declare the [class-based exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_based_exception_glosry.htm "Glossary Entry") that can be propagated from the method to the caller. It can also be specified with the addition `RESUMABLE` for [resumable exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenresumable_exception_glosry.htm). Find more information in the [Exceptions and Runtime Errors](27_Exceptions.md) cheat sheet. |


> **💡 Note**<br>
> - It is advisable to avoid specifying multiple different output parameters (exporting, returning, changing) in a signature to reduce complexity. 
> - Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_general.htm).
> - You may find the addition `EXCEPTIONS` especially in definitions of older classes. They are for non-class-based exceptions. This addition should not be used in ABAP for Cloud Development. See the section [Class-Based Exceptions](#class-based-exceptions), and the section [Classic Exceptions](27_Exceptions.md#classic-exceptions) in the [Exceptions and Runtime Errors](27_Exceptions.md) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Formal and Actual Parameters

- [Formal parameters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenformal_parameter_glosry.htm "Glossary Entry"): You define method parameters by specifying a name with a type which can be a [generic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengeneric_data_type_glosry.htm "Glossary Entry") or  [complete](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplete_data_type_glosry.htm "Glossary Entry")
    type. 
- Examples:
  - `fp` is the formal parameter that has a complete type: `... meth IMPORTING fp TYPE string ...`
  - `gen` is the formal parameter that has a generic type: `... meth IMPORTING gen TYPE any ...`
  - Find more information about generic types also in the [Data Types and Data Objects](16_Data_Types_and_Objects.md#generic-types) cheat sheet.
- This formal parameter includes the specification of how the value passing should happen. Parameters can be passed by ...
  - [reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpass_by_reference_glosry.htm "Glossary Entry"): `... REFERENCE(param) ...`; note that just specifying the parameter name `... param ...` - as a shorter syntax - means passing by reference by default) 
  - [value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpass_by_value_glosry.htm "Glossary Entry"): `... VALUE(param) ...`
- An [Actual parameter](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenactual_parameter_glosry.htm "Glossary Entry") represents the data object whose content is passed to or copied from a formal parameter as an argument when a procedure is called. 
- If passing by reference is used, a local data object is not created for the actual parameter. Instead, the procedure is given a [reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_glosry.htm "Glossary Entry") to the actual parameter during the call and works with the actual parameter itself. 
- Note that parameters that are input and passed by reference cannot be modified in the procedure. However, the use of a reference is beneficial regarding the performance compared to creating a local data object.

The following example (which anticipates aspects described in the following sections, such as calling methods) shows a class with a simple method demonstrating the syntax for formal parameter specifications. Complete types are used. 

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    "Passing by reference and value
    METHODS: meth IMPORTING i_a            TYPE i
                            REFERENCE(i_b) TYPE string
                            VALUE(i_c)     TYPE i
                  RETURNING VALUE(r_a)     TYPE string.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "The values 1, hello, and 2 are actual parameters supplied when 
    "the method is called.
    DATA(result) = meth( i_a = 1
                         i_b = `hello`
                         i_c = 2 ).

  ENDMETHOD.

  METHOD meth.
    ... "Method implementation

    "Input parameters passed by reference cannot be changed in the method implementation.
    "i_a += 1.
    "i_b &&= ` world`.

    "Input parameters passed by value can be changed in the method implementation.
    i_c += 1.

    r_a = `ABAP`.

  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Complete Typing of Formal Parameters

Syntax for [completely](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplete_data_type_glosry.htm "Glossary Entry") typing a formal parameter: 
- `TYPE complete_type`
- `TYPE LINE OF complete_type`
- `TYPE REF TO type`
- `LIKE dobj`
- `LIKE LINE OF dobj`
- `LIKE REF TO dobj`

> **💡 Note**<br>
> - `complete_type`: Stands for a non-generic built-in ABAP, ABAP DDIC, ABAP CDS, a public data type from a global class or interface, or a local type declared with `TYPES`
> - `REF TO` types as a reference variable. A generic type cannot be specified after `REF TO`. A typing with `TYPE REF TO data` and `TYPE REF TO object` is considered as completely typing a formal parameter.
> - Enumerated types can also be used to type the formal parameter.
> - The considerations also apply to the typing of field symbols.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    "Local types and data objects used in the example
    TYPES c3 TYPE c LENGTH 3.
    TYPES der_type TYPE TABLE FOR CREATE zdemo_abap_rap_ro_m.
    DATA int TYPE i.
    DATA itab TYPE TABLE OF zdemo_abap_fli_ve WITH EMPTY KEY.

    "Various syntax options for completely typing formal parameters
    "Note: The example parameters are all specified for passing
    "actual parameters by reference.
    METHODS: meth IMPORTING
                    "---- Non-generic built-in ABAP types ----
                    i_a TYPE i
                    i_b TYPE string
                    "---- ABAP DDIC types ----
                    i_c TYPE land1           "elementary type
                    i_d TYPE timestampl      "elementary type
                    i_e TYPE zdemo_abap_fli "structured type based on DDIC database table
                    i_f TYPE string_hashed_table "table type
                    "---- ABAP CDS types (all of the examples are structured types) ----
                    i_g TYPE zdemo_abap_fli_ve "CDS view entity
                    i_h TYPE zdemo_abap_abstract_ent "CDS abstract entity
                    i_i TYPE zdemo_abap_table_function "CDS table function
                    "---- Data types declared in public section of a class ----
                    i_j TYPE zcl_demo_abap_dtype_dobj=>t_pub_text_c30 "elementary type
                    i_k TYPE zcl_demo_abap_amdp=>carr_fli_struc "structured type
                    i_l TYPE zcl_demo_abap_amdp=>carr_fli_tab "table type
                    "---- Data types declared in an interface ----
                    i_m TYPE zdemo_abap_get_data_itf=>occ_rate "elementary type
                    i_n TYPE zdemo_abap_get_data_itf=>carr_tab "table type
                    "---- Local types ----
                    i_o TYPE c3 "elementary type
                    i_p TYPE der_type "table type (BDEF derived type)
                    "---- Note: Examples such as the following are not allowed type specifications of formal parameters. ----
                    "---- In the following cases, extra (local) type declarations with TYPES are required before the --------
                    "---- method declaration to type the formal parameters. -------------------------------------------------
                    "i_no1 TYPE c LENGTH 3
                    "i_no2 TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY
                    "---- Reference types ----
                    i_q TYPE REF TO i "Data reference
                    i_r TYPE REF TO zdemo_abap_carr "Data reference
                    i_s TYPE REF TO zcl_demo_abap_unit_test "Object reference
                    i_t TYPE REF TO data "Data reference (considered as complete typing, too)
                    i_u TYPE REF TO object "Object reference (considered as complete typing, too)
                    "---- TYPE LINE OF addition (structured type based on a table type) ----
                    i_v TYPE LINE OF zcl_demo_abap_amdp=>carr_fli_tab
                    i_w TYPE LINE OF der_type
                    "---- LIKE addition (types based on existing data objects) ----
                    i_x LIKE int "Local data object
                    i_y LIKE zcl_demo_abap_dtype_dobj=>comma "Constant specified in a class
                    i_z LIKE zdemo_abap_objects_interface=>stat_str "Data object specified in an interface
                    "---- LIKE LINE OF addition (types based on existing internal tables) ----
                    i_1 LIKE LINE OF itab "Local internal table
                    "---- LIKE REF TO addition (reference types based on existing data object) ----
                    i_2 LIKE REF TO int "Local elementary data object
                    i_3 LIKE REF TO itab "Local internal table
                  .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "Calling methods and providing actual parameters
*    meth(
*      i_a = 1
*      i_b = `hello`
*      ... ).

  ENDMETHOD.

  METHOD meth.
    ... "Method implementation
  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Generic Typing of Formal Parameters

Find more information on generic types in the [Data Types and Data Objects](16_Data_Types_and_Objects.md#generic-types) cheat sheet. The following code snippet anticipates aspects described in the following sections, such as calling methods.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    "Example method demonstrating the generic typing of formal parameters
    METHODS: meth IMPORTING
                    "---- Any data type ----
                    i_data           TYPE data
                    i_any            TYPE any

                    "---- Character-like types ----
                    i_c              TYPE c         "Text field with a generic length
                    i_clike          TYPE clike     "Character-like (c, n, string, d, t, and character-like flat structures)
                    i_csequence      TYPE csequence "Text-like (c, string)
                    i_n              TYPE n         "Numeric text with generic length
                    i_x              TYPE x         "Byte field with generic length
                    i_xsequence      TYPE xsequence "Byte-like (x, xstring)

                    "---- Numeric types ----
                    i_decfloat       TYPE decfloat "decfloat16 decfloat34
                    i_numeric        TYPE numeric  "Numeric (i, int8, p, decfloat16, decfloat34, f, (b, s))
                    i_p              TYPE p        "Packed number (generic length and number of decimal places)

                    "---- Internal table types ----
                    i_any_table      TYPE ANY TABLE      "Internal table with any table type
                    i_hashed_table   TYPE HASHED TABLE
                    i_index_table    TYPE INDEX TABLE
                    i_sorted_table   TYPE SORTED TABLE
                    i_standard_table TYPE STANDARD TABLE
                    i_table          TYPE table          "Standard table

                    "---- Other types ----
                    i_simple         TYPE simple "Elementary data type including enumerated types and
                    "structured types with exclusively character-like flat components
                  .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "Structure including various components of specific types
    "They represent actual parameters in the method call below
    DATA: BEGIN OF s,
            c3        TYPE c LENGTH 3,
            c10       TYPE c LENGTH 10,
            n4        TYPE n LENGTH 4,
            str       TYPE string,
            time      TYPE t,
            date      TYPE d,
            dec16     TYPE decfloat16,
            dec34     TYPE decfloat34,
            int       TYPE i,
            pl4d2     TYPE p LENGTH 4 DECIMALS 2,
            tab_std   TYPE STANDARD TABLE OF string WITH EMPTY KEY,
            tab_so    TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line,
            tab_ha    TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line,
            xl1       TYPE x LENGTH 1,
            xstr      TYPE xstring,
            structure TYPE zdemo_abap_carr, "character-like flat structure
          END OF s.

    "The following method call specifies various actual parameters for the
    "generic formal parameters.
    "Note the comments for allowed and not allowed example assignments of
    "actual parameters.
    meth(
      "------------- Any data type -------------
      "--- data/any: Allowed (examples) ---
      i_data = s-c3
      "i_data = s-time
      "i_data = s-tab_std
      "i_data = s-xstr

      i_any = s-c3
      "i_any = s-time
      "i_any = s-tab_std
      "i_any = s-xstr

      "------------- Character-like types -------------
      "--- c: Allowed (examples) ---
      i_c = s-c3
      "i_c = s-c10
      "--- c: Not allowed (examples) ---
      "i_c = s-str
      "i_c = s-n4

      "--- clike: Allowed (examples) ---
      i_clike = s-c3
      "i_clike = s-c10
      "i_clike = s-str
      "i_clike = s-structure
      "i_clike = s-time
      "i_clike = s-date
      "i_clike = s-n4
      "--- clike: Not allowed (examples) ---
      "i_clike = s-xstr
      "i_clike = s-xl1
      "i_clike = s-pl4d2

      "--- csequence: Allowed (examples) ---
      i_csequence  = s-c3
      "i_csequence  = s-c10
      "i_csequence  = s-str
      "--- csequence: Not allowed (examples) ---
      "i_csequence  = s-time
      "i_csequence  = s-date
      "i_csequence  = s-structure

      "--- n: Allowed ---
      i_n = s-n4
      "--- n: Not allowed (examples) ---
      "i_n = s-c3
      "i_n = s-int

      "--- x: Allowed ---
      i_x = s-xl1
      "--- x: Not allowed (examples) ---
      "i_x = s-xstr
      "i_x = s-c3

      "--- xsequence: Allowed ---
      i_xsequence = s-xstr
      "i_xsequence = s-xl1
      "--- xsequence: Not allowed (examples) ---
      "i_xsequence = s-c3
      "i_xsequence = s-str

      "--- decfloat: Allowed ---
      i_decfloat = s-dec16
      "i_decfloat = s-dec34
      "--- decfloat: Not allowed (examples) ---
      "i_decfloat = s-int
      "i_decfloat = s-pl4d2

      "--- numeric: Allowed (examples) ---
      i_numeric  = s-int
      "i_numeric = s-dec16
      "i_numeric = s-dec34
      "i_numeric = s-pl4d2
      "--- numeric: Not allowed (examples) ---
      "i_numeric = s-n4
      "i_numeric = s-date

      "--- p: Allowed ---
      i_p = s-pl4d2
      "--- p: Not allowed (examples) ---
      "i_p = s-dec16
      "i_p = s-dec34

      "--- any table: Allowed ---
      i_any_table = s-tab_std
      "i_any_table = s-tab_ha
      "i_any_table = s-tab_so
      "--- any table: Not allowed (examples) ---
      "i_any_table = s-structure
      "i_any_table = s-c3

      "--- hashed table: Allowed ---
      i_hashed_table = s-tab_ha
      "--- hashed table: Not allowed ---
      "i_hashed_table = s-tab_std
      "i_hashed_table = s-tab_so

      "--- index table: Allowed ---
      i_index_table = s-tab_std
      "i_index_table = s-tab_so
      "--- index table: Not allowed ---
      "i_index_table = s-tab_ha

      "--- sorted table: Allowed ---
      i_sorted_table = s-tab_so
      "--- sorted table: Not allowed ---
      "i_sorted_table = s-tab_std
      "i_sorted_table = s-tab_ha

      "--- standard table/table: Allowed ---
      i_standard_table = s-tab_std
      i_table = s-tab_std
      "--- standard table/table: Not allowed ---
      "i_standard_table = s-tab_so
      "i_standard_table = s-tab_ha
      "i_table = s-tab_so
      "i_table = s-tab_ha

     "--- simple: Allowed (examples) ---
      i_simple = s-structure
      "i_simple = s-c3
      "i_simple = s-n4
      "i_simple = s-int
      "i_simple = s-pl4d2
      "i_simple = s-xstr
      "i_simple = s-str
      "--- simple: Not allowed (examples) ---
      "i_simple = s-tab_ha
      "i_simple = s-tab_so

       ).

  ENDMETHOD.

  METHOD meth.
    ... "Method implementation
  ENDMETHOD.

ENDCLASS.
```


<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Defining Parameters as Optional

- Parameters specified after `IMPORTING` and `CHANGING` can be defined as optional using the `OPTIONAL` and `DEFAULT` additions: 
  - [`OPTIONAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_parameters.htm#!ABAP_ONE_ADD@1@): It is then not mandatory to pass an actual
    parameter. 
  - [`DEFAULT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_parameters.htm#!ABAP_ONE_ADD@1@): Also makes the passing of an actual parameter optional. However, when using this addition, as the name implies, a default value is set.
  - In the method implementations you may want to check whether an actual parameter was passed. You can use predicate expressions using `IS SUPPLIED`. See the example further down.


The following example (which anticipates aspects described in the following sections, such as calling methods) includes three methods that specify optional parameters. The methods are called with and without providing actual parameters. The method implementations include the use of the predicate expression `IS SUPPLIED` with `IF` statements and the `COND` operator. The console output of the example, run with F9, is as follows:

```
meth1_result_a                                    
The parameter is not supplied. Initial value: "0".
meth1_result_b                        
The parameter is supplied. Value: "2".
meth2_result_a                                    
The parameter is not supplied. Default value: "1".
meth2_result_b                        
The parameter is supplied. Value: "3".
meth3_result_b                                                                                      
num1: "4" / num2 (is not supplied; initial value): "0" / num3 (is not supplied; default value): "1" 
meth3_result_c                                                                   
num1: "5" / num2 (is supplied): "6" / num3 (is not supplied; default value): "1" 
meth3_result_d                                                                   
num1: "7" / num2 (is not supplied; initial value): "0" / num3 (is supplied): "8" 
```

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS meth1 IMPORTING num        TYPE i OPTIONAL
                  RETURNING VALUE(str) TYPE string.
    METHODS meth2 IMPORTING num        TYPE i DEFAULT 1
                  RETURNING VALUE(str) TYPE string.
    METHODS meth3 IMPORTING num1       TYPE i
                            num2       TYPE i OPTIONAL
                            num3       TYPE i DEFAULT 1
                  RETURNING VALUE(str) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA(meth1_result_a) = meth1( ).
    DATA(meth1_result_b) = meth1( 2 ).

    DATA(meth2_result_a) = meth2( ).
    DATA(meth2_result_b) = meth2( 3 ).

    "The commented out statement is not possible as there is one
    "non-optional parameter.
    "DATA(meth3_result_a) = meth3( ).
    DATA(meth3_result_b) = meth3( 4 ).
    DATA(meth3_result_c) = meth3( num1 = 5 num2 = 6 ).
    DATA(meth3_result_d) = meth3( num1 = 7 num3 = 8 ).

    out->write( data = meth1_result_a name = `meth1_result_a` ).
    out->write( data = meth1_result_b name = `meth1_result_b` ).
    out->write( data = meth2_result_a name = `meth2_result_a` ).
    out->write( data = meth2_result_b name = `meth2_result_b` ).
    out->write( data = meth3_result_b name = `meth3_result_b` ).
    out->write( data = meth3_result_c name = `meth3_result_c` ).
    out->write( data = meth3_result_d name = `meth3_result_d` ).

  ENDMETHOD.

  METHOD meth1.
    IF num IS SUPPLIED.
      str = |The parameter is supplied. Value: "{ num }".|.
    ELSE.
      str = |The parameter is not supplied. Initial value: "{ num }".|.
    ENDIF.
  ENDMETHOD.

  METHOD meth2.
    str = COND #( WHEN num IS SUPPLIED THEN |The parameter is supplied. Value: "{ num }".|
                  ELSE |The parameter is not supplied. Default value: "{ num }".|  ).
  ENDMETHOD.

  METHOD meth3.
    str = |num1: "{ num1 }" / |.

    str &&= |{ COND #( WHEN num2 IS SUPPLIED THEN |num2 (is supplied): "{ num2 }"|
                       ELSE |num2 (is not supplied; initial value): "{ num2 }"| ) } / |.

    str &&= |{ COND #( WHEN num3 IS SUPPLIED THEN |num3 (is supplied): "{ num3 }"|
                       ELSE |num3 (is not supplied; default value): "{ num3 }"| ) } |.
  ENDMETHOD.

ENDCLASS.
```


<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Defining Input Parameters as Preferred

- Using the `PREFERRED PARAMETER` addition, you can flag an input parameter from the list as preferred.
- All parameters must be optional (i.e. specified with `OPTIONAL` or `DEFAULT`).
- The preferred parameter is implicitly optional, but you should explicitly specify it as `OPTIONAL` or `DEFAULT`, or a warning will be displayed.
- When you call a method and specify a single actual parameter without specifying the name of the formal parameter in an assignment, the actual parameter is automatically assigned to the preferred parameter.

The following example (which anticipates aspects described in the following sections, such as calling methods) shows a simple method with input parameters of type `i` (an addition is performed using the actual parameter values), where one parameter is preferred. The `IS SUPPLIED` addition in `COND` statements checks whether parameters are supplied. The final output shows the preferred parameter assigned automatically when the formal parameter is not specified explicitly.
To try the example out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console. The example should display the following:

```
IS SUPPLIED: num1 "X", num2 "X", num3 "X" / Addition result "9"
IS SUPPLIED: num1 "X", num2 "X", num3 "" / Addition result "7"
IS SUPPLIED: num1 "", num2 "X", num3 "X" / Addition result "6"
IS SUPPLIED: num1 "X", num2 "", num3 "X" / Addition result "6"
IS SUPPLIED: num1 "", num2 "X", num3 "" / Addition result "4"
IS SUPPLIED: num1 "", num2 "", num3 "X" / Addition result "3"
IS SUPPLIED: num1 "", num2 "", num3 "" / Addition result "1"
IS SUPPLIED: num1 "X", num2 "", num3 "" / Addition result "4"
```

Example code:

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    CLASS-METHODS meth
      IMPORTING num1        TYPE i OPTIONAL
                num2        TYPE i OPTIONAL
                num3        TYPE i DEFAULT 1
                PREFERRED PARAMETER num1
      RETURNING VALUE(text) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA(text1) = meth( num1 = 3 num2 = 3 num3 = 3 ).
    out->write( text1 ).

    DATA(text2) = meth( num1 = 3 num2 = 3 ).
    out->write( text2 ).

    DATA(text3) = meth( num2 = 3 num3 = 3 ).
    out->write( text3 ).

    DATA(text4) = meth( num1 = 3 num3 = 3 ).
    out->write( text4 ).

    DATA(text5) = meth( num2 = 3 ).
    out->write( text5 ).

    DATA(text6) = meth( num3 = 3 ).
    out->write( text6 ).

    DATA(text7) = meth( ).
    out->write( text7 ).

    "Not specifying the name of the formal parameter. The
    "actual parameter is assigned to the preferred input
    "parameter.
    DATA(text8) = meth( 3 ).
    out->write( text8 ).
  ENDMETHOD.

  METHOD meth.
    DATA(addition) = num1 + num2 + num3.
    text = |IS SUPPLIED: num1 "{ COND #( WHEN num1 IS SUPPLIED THEN 'X' ELSE '' ) }", | &&
           |num2 "{ COND #( WHEN num2 IS SUPPLIED THEN 'X' ELSE '' ) }", | &&
           |num3 "{ COND #( WHEN num3 IS SUPPLIED THEN 'X' ELSE '' ) }" / | &&
           |Addition result "{ addition }"|.
  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Constructors

-   [Constructors](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_glosry.htm "Glossary Entry")
    are special methods that are usually used for setting a defined
    initial value for attributes of the class or its objects.
-   A class has exactly one [instance
    constructor](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_constructor_glosry.htm "Glossary Entry")
    and one [static
    constructor](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_constructor_glosry.htm "Glossary Entry").
-   The declaration and use of constructors is optional.
-   Static constructor:
    - Declared using the predefined name `class_constructor` as part of a
    `CLASS-METHODS` statement in the public visibility section.
    - Has no parameters.
    - Automatically and immediately called once for each class when calling a class for
    the first time in an internal session, i. e. when, for example, an instance of a class is created or a component is used. Note: If it is not explicitly declared and implemented, it is merely an empty method.
-   Instance constructor:
    - Declared using the predefined name `constructor` as part of a
    `METHODS` statement. In case of global classes, it can only be declared in the public visibility section.
    - Automatically called when a class is
    instantiated and an instance is created.
    - Can have `IMPORTING` parameters and raise exceptions.

The following example (which anticipates aspects described in the following sections, such as creating instances of classes) demonstrates static and instance constructors. To try the example out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.

Notes:  
- The example class defines the instance and static constructors.  
- The instance constructor includes an optional importing parameter, which static constructors do not allow. If the parameter were not optional, the class would not run with F9, as it could not pass an actual parameter. In the example, the actual parameter indicates the name of the instance created for output purposes.
- The example class creates multiple instances.
- Both instance and static attributes of each instance are accessed and added to an internal table, which is then output.
- The constructor implementations include:  
  - Instance constructor:  
    - Stores the current timestamp in an instance attribute.  
    - Maintains a call counter for the number of times the instance constructor is called, stored in a static attribute to reflect the count per internal session.  
    - Stores the manually provided instance name in an instance attribute.  
  - Static constructor:  
    - Stores the current timestamp in a static attribute.  
    - Maintains a call counter for static constructor calls, stored in a static attribute.  
- The constructors demonstrate:  
  - The static constructor is called only once, even when multiple class instances are created, leading to a constant `static_timestamp` value and `stat_constr_call_count` value, which remains 1.  
  - The `instance_timestamp` attribute shows different timestamps for each created instance.  
  - The static attribute `instance_constr_call_count` increases with each instance. Note that running the class with F9 in ADT also calls the instance and static constructors. Thus, the final `instance_constr_call_count` totals the number of `DO` loop passes plus 1, starting with 2 for `inst1` instead of 1.


```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS constructor IMPORTING text TYPE string OPTIONAL.
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA instance_timestamp TYPE utclong.
    CLASS-DATA static_timestamp TYPE utclong.
    DATA instance_name TYPE string.
    CLASS-DATA stat_constr_call_count TYPE i.
    CLASS-DATA instance_constr_call_count TYPE i.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA itab TYPE string_table.

    DO 5 TIMES.
      DATA(inst) = NEW zcl_demo_abap( |inst{ sy-index }| ).
      APPEND |-------------- Instance "{ inst->instance_name }" --------------| TO itab.
      APPEND |instance_timestamp: { inst->instance_timestamp }| TO itab.
      APPEND |static_timestamp: { inst->static_timestamp }| TO itab.
      APPEND |instance_constr_call_count: { inst->instance_constr_call_count }| TO itab.
      APPEND |stat_constr_call_count: { inst->stat_constr_call_count }| TO itab.
      APPEND INITIAL LINE TO itab.
    ENDDO.

    out->write( itab ).
  ENDMETHOD.

  METHOD class_constructor.
    static_timestamp = utclong_current( ).
    stat_constr_call_count += 1.
  ENDMETHOD.

  METHOD constructor.
    instance_timestamp = utclong_current( ).
    instance_constr_call_count += 1.

    IF text IS SUPPLIED AND text IS NOT INITIAL.
      instance_name = text.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
```


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Working with Objects and Components

### Declaring Object Reference Variables
- To create an object, an [object reference variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm "Glossary Entry") must be declared. 
- It is also necessary for accessing objects and their components. That means objects are not directly accessed but only via references that point to
those objects. 
- This object reference variable contains the reference to the object - after assigning the reference to the object (see further down).

``` abap
"Declaring object reference variables
DATA: ref1 TYPE REF TO local_class,
      ref2 TYPE REF TO global_class,
      ref3 LIKE ref1.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Creating Objects

-   Using the instance operator
    [`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm),
    you can create objects of a class (and [anonymous data
    objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenanonymous_data_object_glosry.htm "Glossary Entry"), too, that are not dealt with here). As a result,
    you get a [reference variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_variable_glosry.htm "Glossary Entry")
    that points to the created object.
-   Regarding the type specifications before and parameters within the
    parentheses:
    - Right before the first parenthesis after `NEW`, the type, i. e. the class, must be specified. The `#` character - instead of the class name -
means that the type (`TYPE REF TO ...`) can be derived from the context (in this case from the type of the reference variable). You can
also omit the explicit declaration of a reference variable by declaring a new reference variable
[inline](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_inline.htm),
for example, using `DATA`. In this case, the name of the class must be placed after `NEW` and before the first parenthesis.
    -   No parameter specified within the parentheses: No values are
        passed to the instance constructor of an object. However, non-optional input parameters of the
        instance constructor of the instantiated class must be filled.
        No parameters are passed for a class without an explicitly declared
        instance constructor. See more information:
        [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennew_constructor_params_class.htm).
- The operator
    basically replaces the syntax [`CREATE OBJECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_object.htm) you might stumble on. However, `CREATE OBJECT` statements are still required (i.e. they are the only option, `NEW` is not possible for them) for creating objects dynamically. For more information, see the [Dynamic Programming](06_Dynamic_Programming.md) cheat sheet.

``` abap
"Declaring object reference variable
DATA: ref1 TYPE REF TO some_class.

"Creating objects
ref1 = NEW #( ).                     "Type derived from already declared ref1

DATA(ref2) = NEW some_class( ).      "Reference variable declared inline, explicit type
                                     "(class) specification
"The assumption is that this class has no mandatory importing parameters for the 
"instance constructor. If a class has, actual parameters must be provided.                                      
DATA(ref_mand_param) = NEW another_class( ip1 = ... ip2 = ... ).

"Older syntax, replaced by NEW operator 
"However, CREATE OBJECT is required in dynamic object creation.
CREATE OBJECT ref3.                  "Type derived from already declared ref3
CREATE OBJECT ref4 TYPE some_class.  "Corresponds to the result of the expression above
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Working with Reference Variables

This section covers some aspects of working with reference variables.

**Assigning Reference Variables**

To assign or copy reference variables, use the [assignment operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm "Glossary Entry") `=`. In the example below, both object reference variables have the same type. Note the concepts of polymorphism, upcasts and downcasts when assigning reference variables covered further down. 

``` abap
DATA: ref1 TYPE REF TO some_class,
      ref2 TYPE REF TO some_class.

ref1 = NEW #( ).

"Assigning existing reference
ref2 = ref1.
```

**Overwriting reference variables**: An [object
reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_reference_glosry.htm "Glossary Entry")
is overwritten when a new object is created with a reference variable
already pointing to an instance.
``` abap
ref1 = NEW #( ).

"Existing reference is overwritten
ref1 = NEW #( ).
```

**Retaining object references**:
- If your use case is to retain the object references, for example, if you create multiple objects using the same object reference variable, you can put the reference variables in internal tables that are declared using `... TYPE TABLE OF REF TO ...`.
- The following code snippet just visualizes that the object references are not overwritten. Three objects are created with the same reference variable. The internal table includes all object references and, thus, their values are retained.
``` abap
DATA: ref TYPE REF TO some_class,
      itab TYPE TABLE OF REF TO some_class.

DO 3 TIMES.
  ref = NEW #( ).
  itab = VALUE #( BASE itab ( ref ) ).   "Adding the reference to itab
ENDDO.
```

**Clearing object references**: You can use
[`CLEAR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclear.htm)
statements to explicitly clear a reference variable.
```abap
CLEAR ref.
```

> **💡 Note**<br>
> Objects use up space in the memory and should therefore be
cleared if they are no longer needed. However, the [garbage collector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengarbage_collector_glosry.htm "Glossary Entry") is called periodically and automatically by the [ABAP runtime framework](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_runtime_frmwk_glosry.htm "Glossary Entry") and clears all objects without any reference.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Accessing Attributes
- Instance attributes: Accessed using
the [object component selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_component_select_glosry.htm "Glossary Entry")
`->` via a reference variable.
- Static attributes: Accessed (if the attributes are visible) using the [class component
selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_component_select_glosry.htm "Glossary Entry")
`=>` via the class name. You can also declare data objects and
types by referring to static attributes.
``` abap
"Accessing instance attribute via an object reference variable

... ref->some_attribute ...

"Accessing static attributes via the class name

... some_class=>static_attribute ...

"Without the class name only within the class itself
... static_attribute ...

"Type and data object declarations

TYPES some_type LIKE some_class=>some_static_attribute.
DATA dobj1      TYPE some_class=>some_type.
DATA dobj2      LIKE some_class=>some_static_attribute.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Calling Methods
- Similar to accessing attributes, instance
methods are called using `->` via a reference variable.
- Static
methods are called using `=>` via the class name. When used
within the class in which it is declared, the static method can also be
called without `class_name=>...`.
- Static methods can but should not be called via reference variable (<code>oref->some_static_method( ).</code>).
- When methods are called, the (non-optional) parameters must be specified within parentheses.
- You might also stumble on method calls with the older [`CALL METHOD`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_method_static.htm)
statements. It is recommended to use the new syntax in new developments. Note that `CALL METHOD` statements are still required in the context of [dynamic programming](06_Dynamic_Programming.md). Therefore, `CALL METHOD` statements should be reserved for dynamic method calls.
- Find an example class demonstrating various method calls in section [Excursion: Example Class](#excursion-example-class).


Examples for instance method calls and static method calls:
``` abap
"Calling instance methods via reference variable;
"within the parentheses, the parameters must be specified and assigned - if required

ref->inst_meth( ... ).

"Calling static methods via/without the class name

class_name=>stat_meth( ... ).

"Only within the program in which it is declared.
stat_meth( ... ).

"Calling (static) method having no parameter

class_name=>stat_meth( ).

"Calling (static) methods having a single importing parameter:

"Note that in the method call, the caller exports values to the
"method having importing parameters defined; hence, the addition
"EXPORTING is relevant for the caller. The following three method calls are the same

"Explicit use of EXPORTING.
class_name=>meth( EXPORTING a = b ).

"Only importing parameters in the method signature: explicit EXPORTING not needed

class_name=>meth( a = b ).

"If only a single value must be passed:
"the formal parameter name (a) and EXPORTING not needed

stat_meth( b ).

"Calling (static) methods having importing/exporting parameters
"Parameters must be specified if they are not marked as optional

class_name=>meth( EXPORTING a = b c = d     "a/c: importing parameters in the method signature
                  IMPORTING e = f ).        "e: exporting parameter in the method signature

"To store the value of the parameter, you may also declare it inline.

class_name=>meth( EXPORTING a = b c = d
                  IMPORTING e = DATA(z) ).  

"Calling (static) methods having a changing parameter;
"should be reserved for changing an existing local variable and value

DATA h TYPE i VALUE 123.
class_name=>meth( CHANGING g = h ).

"Calling (static) methods having a returning parameter.
"Basically, they do the same as methods with exporting parameters
"but they are way more versatile, and you can save lines of code.

"They do not need temporary variables.
"In the example, the return value is stored in a variable declared inline.

"i and k are importing parameters
DATA(result) = class_name=>meth( i = j k = l ).

"They can be used with other statements, e. g. logical expressions.
"In the example below, the assumption is that the returning parameter is of type i.
IF class_name=>meth( i = j k = l ) > 100.
  ...
ENDIF.

"They enable method chaining.
"The example shows a method to create random integer values.
"The methods have a returning parameter.
DATA(random_no) = cl_abap_random_int=>create( )->get_next( ).

"RECEIVING parameter: Available in methods defined with a returning parameter;
"used in standalone method calls only.
"In the snippet, m is the returning parameter; n stores the result.
class_name=>meth( EXPORTING i = j k = l RECEIVING m = DATA(n) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Excursion: Inline Declarations, Returning Parameters

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>


- The example code below highlights the convenience of inline declarations when defining target data objects for output parameters. 
- This approach allows you to create data objects on the spot, eliminating the need for additional helper variables. 
- It also mitigates the risk of type mismatches. 
- However, when declaring a method with both exporting and returning parameters, it is not possible to specify target data objects for both inline at the same time in case of functional method calls. 
- Additional code snippets illustrate various ways to use methods declared with returning parameters in expression positions.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS meth1 IMPORTING i_str        TYPE string
                                  i_tab        TYPE string_table OPTIONAL
                        EXPORTING e_dec        TYPE decfloat34
                                  e_tab        TYPE string_table
                        RETURNING VALUE(r_int) TYPE i.
    CLASS-METHODS meth2 RETURNING VALUE(r_tab) TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.    
    "Note: Calling the method in the same class means specifying 'zcl_demo_abap=>' is optional here.

    "Standalone method call
    "Specifying target data objects for all output parameters
    "Inline declarations are handy because you can create an
    "appropriately typed data object in place. No need to
    "create an extra variable, check on the type etc.
    zcl_demo_abap=>meth1(
      EXPORTING
        i_str = `ABAP`
      IMPORTING
        e_dec = DATA(a)
        e_tab = DATA(b)
      RECEIVING
        r_int = DATA(c)
    ).

    "Functional method call
    "The target data object of the returning parameter is specified on the left side of an assignment.
    "Note: In this case, you cannot specify inline declarations for the exporting parameters.
    DATA e TYPE decfloat34.
    DATA f TYPE string_table.
    DATA(g) = zcl_demo_abap=>meth1(
      EXPORTING
        i_str = `ABAP`
      IMPORTING
        "e_dec = DATA(h)
        "e_tab = DATA(i)
        e_dec = e
        e_tab = f
    ).

    "Benefits of returning parameters: They can, for example, be used in expressions
    "The following snippets show a selection (and ignore the available exporting
    "parameters).

    CASE zcl_demo_abap=>meth1( i_str = `ABAP` ).
      WHEN 0. ...
      WHEN 1. ...
      WHEN OTHERS. ...
    ENDCASE.

    IF zcl_demo_abap=>meth1( i_str = `ABAP` ) > 5.
      ...
    ELSE.
      ...
    ENDIF.

    "IF used with a predicative method call
    "The result of the relational expression is true if the result of the functional
    "method call is not initial and false if it is initial. The data type of the result
    "of the functional meth1od call, i. e. the return value of the called functional method,
    "is arbitrary. A check is made for the type-dependent initial value.
    IF zcl_demo_abap=>meth1( i_str = `ABAP` ).
      ...
    ELSE.
      ...
    ENDIF.

    DO zcl_demo_abap=>meth1( i_str = `ABAP` ) TIMES.
      ...
    ENDDO.

    "Method call result as actual parameter
    DATA(j) = zcl_demo_abap=>meth1( i_str = `ABAP` i_tab = zcl_demo_abap=>meth2( ) ).

    "Examples of returning parameters typed with a table type
    LOOP AT zcl_demo_abap=>meth2( ) INTO DATA(wa1).
      ...
    ENDLOOP.

    READ TABLE zcl_demo_abap=>meth2( ) INTO DATA(wa2) INDEX 1.

  ENDMETHOD.
  
  METHOD meth1.
    ... "Method implementation
  ENDMETHOD.

  METHOD meth2.
    ... "Method implementation
  ENDMETHOD.

ENDCLASS.
```

</details>  


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Self-Reference me

When implementing instance methods, you can optionally make use of the implicitly available object reference variable [`me`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenme.htm) which is always available at runtime and points to the respective object itself. You can use it, for example, to refer to components of the instance of a particular class:
``` abap
... some_method( ... ) ...

... me->some_method( ... ) ...
```

You can also address the entire object, which is illustrated in the example of section [Method Chaining and Chained Attribute Access](#method-chaining-and-chained-attribute-access).
The following code snippet declares a local data object in a method. It has the same name as a data object declared in the private visibility section. shows a method implementation. `me` is used to access the non-local data object.

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA str TYPE string.
    METHODS meth RETURNING VALUE(text) TYPE string.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    str = `AP`.
    DATA(text) = meth( ).
    ASSERT text = `ABAP`.

  ENDMETHOD.

  METHOD meth.

    "Declaring a local data object having the same
    "name as a data object declared in a visibility section
    DATA str TYPE string VALUE `AB`.

    "Addressing locally declared data object
    DATA(local_string) = str.

    "Addressing data object declared in private visibility section
    DATA(other_string) = me->str.

    text = local_string && other_string.

  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Method Chaining and Chained Attribute Access

- As shown in a previous example, method chaining is possible for functional method calls (i.e. methods that have exactly one return value declared with `RETURNING`) at appropriate read positions. 
- In this method design, the return values are reference variables that point to objects with the next method in question. The methods in the example class below assign the objects to the returning parameter using the self-reference `me`. 
- The method's return value is then used as an ABAP operand.
- A chained method call can consist of multiple functional methods that are linked using component selectors `->`. 
- Class attributes can also be added to the chain.
- In the context of method chaining, constructor expressions (e.g. with `NEW`) come in handy. The example class below shows functional method calls (i.e. the return value of the method is used as an ABAP operand), and a standalone statement.

The following example illustrates method chaining. Find a self-contained example class further down.

```abap
"The following class creates random integers. Find more information in the
"class documentation.
"Both methods have returning parameters specified.
DATA(some_int1) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 1
                                              max  = 10 )->get_next( ).

"Getting to the result as above - not using method chaining and inline declarations.
DATA some_int2 TYPE i.
DATA dref TYPE REF TO cl_abap_random_int.

dref = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                   min  = 1
                                   max  = 10 ).

some_int2 = dref->get_next( ).

"Using the RECEIVING parameter in a standalone method call
DATA some_int3 TYPE i.
dref->get_next( RECEIVING value = some_int3 ).

"IF statement that uses the return value in a read position
IF cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                               min  = 1
                               max  = 10 )->get_next( ) < 5.
  ... "The random number is lower than 5.
ELSE.
  ... "The random number is greater than 5.
ENDIF.

"Examples using classes of the XCO library (see more information in the
"ABAP for Cloud Development and Released ABAP Classes cheat sheets), in which
"multiple chained method calls can be specified. Each of the methods
"has a returning parameter specified.

"In the following example, 1 hour is added to the current time.
DATA(add1hour) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->add( iv_hour = 1 )->as( xco_cp_time=>format->iso_8601_extended )->value.

"In the following example, a string is converted to xstring using a codepage
DATA(xstr) = xco_cp=>string( `Some string` )->as_xstring( xco_cp_character=>code_page->utf_8 )->value.

"In the following example, JSON data is created. First, a JSON data builder
"is created. Then, using different methods, JSON data is added. Finally,
"the JSON data is turned to a string.
DATA(json) = xco_cp_json=>data->builder( )->begin_object(
  )->add_member( 'CarrierId' )->add_string( 'DL'
  )->add_member( 'ConnectionId' )->add_string( '1984'
  )->add_member( 'CityFrom' )->add_string( 'San Francisco'
  )->add_member( 'CityTo' )->add_string( 'New York'
  )->end_object( )->get_data( )->to_string( ).
```

Self-contained example class demonstrating chained method calls with a functional method call and a standalone statement:

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS add_text IMPORTING str        TYPE string
                     RETURNING VALUE(ref) TYPE REF TO zcl_demo_abap.
    METHODS add_space RETURNING VALUE(ref) TYPE REF TO zcl_demo_abap.
    METHODS add_period RETURNING VALUE(ref) TYPE REF TO zcl_demo_abap.
    METHODS return_text RETURNING VALUE(str) TYPE string.
    METHODS display_text IMPORTING cl_run_ref TYPE REF TO if_oo_adt_classrun_out.
    DATA text TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "----------------------------------------------------------------
    "-------- Method chaining with a functional method call ---------
    "----------------------------------------------------------------

    "Hallo NAME. This is an example of method chaining.
    DATA(text1) = NEW zcl_demo_abap(
                      )->add_text( `Hallo`
                      )->add_space(
                      )->add_text( xco_cp=>sy->user( )->name
                      )->add_period(
                      )->add_space(
                      )->add_text( `This`
                      )->add_space(
                      )->add_text( `is`
                      )->add_space(
                      )->add_text( `an`
                      )->add_space(
                      )->add_text( `example`
                      )->add_space(
                      )->add_text( `of`
                      )->add_space(
                      )->add_text( `method`
                      )->add_space(
                      )->add_text( `chaining`
                      )->add_period(
                      )->return_text( ).

    out->write( text1 ).

    "The following example chained method call includes a chained attribute
    "access at the end so that the target variable contains the content of
    "the attribute.

    "Example result: Today is 2025-03-05. It's 14:30:38. Have a nice day.
    DATA(text2) = NEW zcl_demo_abap(
                      )->add_text( `Today`
                      )->add_space(
                      )->add_text( `is`
                      )->add_space(
                      )->add_text( xco_cp=>sy->date( )->as( xco_cp_time=>format->iso_8601_extended )->value
                      )->add_period(
                      )->add_space(
                      )->add_text( `It's`
                      )->add_space(
                      )->add_text( xco_cp=>sy->time( xco_cp_time=>time_zone->user
                                    )->as( xco_cp_time=>format->iso_8601_extended
                                    )->value
                      )->add_period(
                      )->add_space(
                      )->add_text( `Have`
                      )->add_space(
                      )->add_text( `a`
                      )->add_space(
                      )->add_text( `nice`
                      )->add_space(
                      )->add_text( `day`
                      )->add_period(
                      )->text.

    out->write( text2 ).

    "----------------------------------------------------------------
    "-------- Method chaining with a standalone statement -----------
    "----------------------------------------------------------------

    "In the example, the final method call in the chain receives
    "the classrun instance available in the implementation of the
    "if_oo_adt_classrun~main method. The method implementation
    "includes the writing to the console.

    "Console output: Lorem ipsum dolor sit amet
    NEW zcl_demo_abap( )->add_text( `Lorem`
                        )->add_space(
                        )->add_text( `ipsum`
                        )->add_space(
                        )->add_text( `dolor`
                        )->add_space(
                        )->add_text( `sit`
                        )->add_space(
                        )->add_text( `amet`
                        )->display_text( out ).

  ENDMETHOD.

  METHOD add_text.
    text &&= str.
    ref = me.
  ENDMETHOD.

  METHOD add_space.
    text &&= ` `.
    ref = me.
  ENDMETHOD.

  METHOD display_text.
    cl_run_ref->write( text ).
  ENDMETHOD.

  METHOD add_period.
    text &&= `.`.
    ref = me.
  ENDMETHOD.

  METHOD return_text.
    str = me->text.
  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Excursion: Example Class

Expand the following collapsible section for an example class. The commented example class explores various aspects covered in the previous sections. You can create a demo class called `zcl_demo_abap` and copy and paste the following code. Once activated, you can choose *F9* in ADT to run the class. The example is designed to display output in the console that shows the result of calling different methods.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    "------------------------ Attributes ------------------------
    "Instance attributes
    DATA: inst_attr   TYPE utclong,
          inst_string TYPE string.

    "Static attribute
    CLASS-DATA stat_attr TYPE utclong.

    "Attribute with the READ-ONLY addition. It is also possible for instance
    "attributes. Such an attribute can only be read from outside the class,
    "not changed. Inside the class, it can be changed. This also applies to
    "subclasses (note that the example class does not allow inheritance).
    CLASS-DATA read_only_attr TYPE string VALUE `read only` READ-ONLY.

    "------------------------ Methods ------------------------
    "The parameter interfaces (signatures) of the methods are intended to
    "demonstrate various syntax options described in the cheat sheet.

    "Instance methods
    METHODS:
      "No parameter specified
      inst_meth1,

      "Single importing parameter
      inst_meth2 IMPORTING ip TYPE string,

      "Importing and exporting parameters
      inst_meth3 IMPORTING ip1 TYPE i
                           ip2 TYPE i
                 EXPORTING ep  TYPE i,

      "Returning parameters
      inst_meth4 RETURNING VALUE(ret) TYPE string,

      inst_meth5 IMPORTING ip1        TYPE string
                           ip2        TYPE string
                 EXPORTING ep         TYPE string
                 RETURNING VALUE(ret) TYPE string,

      "Changing parameter
      inst_meth6 CHANGING chg TYPE string,

      "Raising exceptions
      inst_meth7 IMPORTING ip1        TYPE i
                           ip2        TYPE i
                 RETURNING VALUE(ret) TYPE i
                 RAISING   cx_sy_arithmetic_overflow,

      inst_meth8 IMPORTING ip1        TYPE i
                 RETURNING VALUE(ret) TYPE string
                 RAISING   cx_uuid_error,

      "Instance constructor
      "Instance constructors can optionally have importing parameters
      "and raise exceptions.
      constructor.

    "Static methods
    CLASS-METHODS:
      "Options of formal parameter definitions
      stat_meth1 IMPORTING REFERENCE(ip1) TYPE string  "pass by reference (specifying REFERENCE(...) is optional)
                           ip2            TYPE string  "pass by reference
                           VALUE(ip3)     TYPE string  "pass by value
                 RETURNING VALUE(ret)     TYPE string, "pass by value (mandatory for returning parameters)

      "OPTIONAL/DEFAULT additions
      "Both additions denote that passing values is optional. DEFAULT: If not supplied,
      "the default value specified is used.
      stat_meth2 IMPORTING ip1        TYPE string DEFAULT `ABAP`
                           ip2        TYPE string OPTIONAL
                 RETURNING VALUE(ret) TYPE string_table,

      "Generic types are possible for field symbols and method parameters
      "All of the previous formal parameters are typed with complete types (e.g. type i).
      "The following method includes several formal parameters typed with generic
      "types. Find more information in the 'Data Types and Objects' cheat sheet.
      "Note: Returning parameters must be completely typed.
      stat_meth3 IMPORTING ip_data      TYPE data      "Any data type
                           ip_clike     TYPE clike     "Character-like data type (such as c, n, string, etc.)
                           ip_numeric   TYPE numeric   "Numeric type (such as i, p, decfloat34 etc.)
                           ip_any_table TYPE ANY TABLE "Any table type (standard, sorted, hashed)
                 RETURNING VALUE(ret)   TYPE string,   "No generic type possible for returning parameters

      "Static constructor
      "No parameters possible
      class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Note:
    "This is a self-contained example. Usually, you must create an instance
    "of a class first before using instance methods - when calling methods
    "from outside, e.g. from another class. Within the class itself, you can
    "indeed call the instance methods without a prior instance creation.
    "To demonstrate 'calls from external', an instance of the class is
    "nevertheless created in the example.

    "--------------- Constructors ---------------

    "Instance constructor: Automatically called when a class is instantiated
    "and an instance is created.
    "Creating an instance of a class and accessing an instance attribute
    "using the object component selector ->.
    DATA(oref) = NEW zcl_demo_abap( ).
    out->write( data = oref->inst_attr name = `oref->inst_attr` ).

    "Creating more instances
    "The example is implemented in a way that an instance attribute
    "is assigned a time stamp when the instance constructor is called.
    "As instance constructors are called once for each instance, the
    "inst_attr values differ.
    DATA(oref2) = NEW zcl_demo_abap( ).
    out->write( data = oref2->inst_attr name = `oref2->inst_attr` ).

    DATA(oref3) = NEW zcl_demo_abap( ).
    out->write( data = oref3->inst_attr name = `oref3->inst_attr` ).

    "Static constructor: Automatically and immediately called once for each class
    "when calling a class for the first time in an internal session (e.g. an instance
    "is created or a component is used).
    "The example is implemented in a way that a static attribute is assigned a time
    "stamp when the static constructor is called. The class was called with creating
    "an instance above, and so the static constructor was called once. There is no new
    "time stamp assigment, no new calls of the static constructor in this internal session.
    "Therefore, the value of stat_attr is the same.
    "The access is done using the class name, the class component selector =>, and
    "the attribute name.
    out->write( data = zcl_demo_abap=>stat_attr name = `zcl_demo_abap=>stat_attr` ).

    "Static attributes are also accessible via the object reference variable
    out->write( data = oref->stat_attr name = `oref->stat_attr` ).

    "Checking that the values of the instance attribute that is modified when calling
    "the instance constructor differ from instance to instance.
    IF ( oref3->inst_attr > oref2->inst_attr )
    AND ( oref2->inst_attr > oref->inst_attr ).
      out->write( `Instance attribute check: All values differ.` ).
    ELSE.
      out->write( `Instance attribute check: At least one check is not successful.` ).
    ENDIF.

    "Checking that the value of the static attribute that is modified when calling
    "the static constructor is identical from instance to instance (it is only changed
    "when the class is called for the first time).
    IF ( oref3->stat_attr = oref2->stat_attr )
    AND ( oref2->stat_attr = oref->stat_attr )
    AND ( oref->stat_attr = zcl_demo_abap=>stat_attr ).
      out->write( `Static attribute check: All values are identical.` ).
    ELSE.
      out->write( `Static attribute check: At least one check is not successful.` ).
    ENDIF.

    "Excursion: See the note above. In the self-contained example, in this class itself, the
    "components can be called without reference variable or providing the class name. The
    "assumption in the following snippets of the example is that the class is called
    "from outside and instances are created outside of the class, so the variable/class
    "name are provided.
    "The following access is possible in the same class (not outside of this class):
    DATA(a) = inst_attr.
    DATA(b) = stat_attr.

    "The following is also possible inside the class (it is the only option outside of
    "this class):
    DATA(c) = oref->inst_attr.
    DATA(d) = zcl_demo_abap=>stat_attr.
    DATA(e) = oref->stat_attr.

    "--------------- Method calls ---------------

    "Calling instance methods
    "Instance methods are called using the object component selector ->
    "via reference variables.

    "--- Method without parameters ---
    oref->inst_meth1( ).

    "To show an effect of the method call in the example, the method implementation
    "simply includes the change of an instance attribute value.
    out->write( data = oref->inst_string name = `oref->inst_string` ).

    "Notes:
    "- See the method implementation of inst_meth1. It shows that
    "  instance methods can access both static and instance attributes.
    "- As mentioned above regarding the attributes, in the same class and
    "  in this example, you can call the methods directly (without using
    "  a reference variable).
    inst_meth1( ).

    "--- Methods that specify importing parameters ---
    "Similar to the inst_meth1 method, the implementation includes the
    "change of an instance attribute.
    "Notes:
    "- In the method call, the caller exports values to the
    "  method having importing parameters defined. Therefore, the addition
    "  EXPORTING is relevant for the caller.
    "- If a method only specifies importing parameters, specifying EXPORTING
    "  is optional.
    "- If a method only specifies a single importing parameter, specifying
    "  EXPORTING and the formal parameter name is optional.

    "Method with a single importing parameter (the following three method
    "calls are basically the same)
    "Method call that specifies EXPORTING and the formal parameter name
    oref->inst_meth2( EXPORTING ip = `hello` ).

    out->write( data = oref->inst_string name = `oref->inst_string` ).

    "Method call that specifies the formal parameter, without EXPORTING
    oref->inst_meth2( ip = `world` ).

    out->write( data = oref->inst_string name = `oref->inst_string` ).

    "Method call that only includes the actual parameter
    oref->inst_meth2( `ABAP` ).

    out->write( data = oref->inst_string name = `oref->inst_string` ).

    "--- Methods that specify exporting parameters ---
    "For the method call, specify EXPORTING for importing parameters,
    "and IMPORTING for exporting parameters.
    "The method implementation performs a calculation. The calculation
    "result is assigned to the exporting parameter. You can assign the
    "value to a suitable data object.
    DATA calc_result1 TYPE i.
    oref->inst_meth3( EXPORTING ip1 = 5
                                ip2 = 3
                      IMPORTING ep = calc_result1 ).

    out->write( data = calc_result1 name = `calc_result1` ).

    "Inline declaration is also possible.
    oref->inst_meth3( EXPORTING ip1 = 2
                                ip2 = 4
                      IMPORTING ep = DATA(calc_result2) ).

    out->write( data = calc_result2 name = `calc_result2` ).

    "--- Methods that specify returning parameters ---
    DATA(result1) = oref->inst_meth4( ).
    out->write( data = result1 name = `result1` ).

    "The RECEIVING addition is available in standalone method
    "calls only if methods are defined with a returning parameter.
    "Inline declaration is also possible here.
    oref->inst_meth4( RECEIVING ret = DATA(result2) ).
    out->write( data = result2 name = `result2` ).

    "The following example method specifies multiple parameters,
    "among them 2 output parameters (exporting and reporting).

    "Functional method call
    "In a functional method call, inline declaration is not possible
    "for 'ep'.
    DATA str1 TYPE string.
    DATA(str2) = oref->inst_meth5( EXPORTING ip1 = `AB`
                                             ip2 = `AP`
                                   IMPORTING ep = str1 ).

    out->write( data = str1 name = `str1` ).
    out->write( data = str2 name = `str2` ).

    "Standalone method call (including inline declarations)
    oref->inst_meth5( EXPORTING ip1 = `AB`
                                ip2 = `AP`
                      IMPORTING ep = DATA(str3)
                      RECEIVING ret = DATA(str4) ).

    out->write( data = str3 name = `str3` ).
    out->write( data = str4 name = `str4` ).

    "Returning parameters enable ...
    "... the use of method calls in other statements, for example,
    "in logical expressions, instead of storing the method call
    "result in an extra variable.
    IF oref->inst_meth4( ) IS INITIAL.
      out->write( `Initial` ).
    ELSE.
      out->write( `Not initial` ).
    ENDIF.

    "... method chaining to write more concise code and avoid
    "declaring helper variables.
    "The following example uses a class that creates random integers.
    "The 'create' method has a returning parameter. It returns an
    "instance of the class (an object reference) based on which
    "more methods can be called. Using method chaining, you can
    "do the following in one go.
    DATA(inst) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                             min  = 1
                                             max  = 10 ).
    DATA(random_integer1) = inst->get_next( ).

    DATA(random_integer2) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                        min  = 1
                                                        max  = 10 )->get_next( ).

    out->write( data = random_integer1 name = `random_integer1` ).
    out->write( data = random_integer2 name = `random_integer2` ).

    "--- Method that specifies a changing parameter ---
    "Changing parameters should be reserved for changing an existing
    "local variable and value.
    "The method implementation includes the demonstration of the
    "self-reference 'me'. For this purpose, a data object is created
    "in the method implementation that has the same name as a data
    "object declared in the class's declaration part.

    "Changing the value of the instance attribute with the same name
    "as the local data object in the method implementation.
    oref->inst_string = `TEST`.

    DATA local_var TYPE string VALUE `hello`.
    oref->inst_meth6( CHANGING chg = local_var ).

    out->write( data = local_var name = `local_var` ).

    "--- Methods that specify RAISING ---
    "The following example method declares an exception of the category
    "CX_DYNAMIC_CHECK. That is, the check is not performed until runtime.
    "There is no syntax warning shown at compile time.
    "The ipow function is included in the method implementation. The
    "second example raises the exception.

    "No TRY control structure, no syntax warning at compile time. However,
    "the calculation works.
    DATA(power_res1) = oref->inst_meth7( ip1 = 5 ip2 = 2 ).
    out->write( data = power_res1 name = `power_res1` ).

    "The example raises the exception because the resulting number is too high.
    "Not catching the exception results in a runtime error.
    TRY.
        DATA(power_res2) = oref->inst_meth7( ip1 = 10 ip2 = 10 ).
        out->write( data = power_res2 name = `power_res2` ).
      CATCH cx_sy_arithmetic_overflow.
        out->write( `cx_sy_arithmetic_overflow was raised` ).
    ENDTRY.

    "The following example method declares an exception of the category
    "CX_STATIC_CHECK. That is, it is statically checked. Without the
    "TRY control structure and catching the exception, a syntax warning
    "is displayed. And in the case of the example implementation,
    "the exception is indeed raised. Not catching the exception results
    "in a runtime error.
    TRY.
        DATA(test) = oref->inst_meth8( ip1 = 1 ).
        out->write( data = test name = `test` ).
      CATCH cx_uuid_error.
        out->write( `cx_uuid_error was raised` ).
    ENDTRY.

    "A syntax warning is displayed for the following method call.
    "DATA(test2) = oref->inst_meth8( ip1 = 1 ).

    "Calling static methods
    "The method definitions/implementations cover further aspects.

    "--- Pass by reference/value ---
    "The following example method emphasizes pass by reference and
    "by value.
    "Notes:
    "- Returning parameters can only be declared with VALUE(...)
    "- When passing by reference, the content of the data objects
    "  cannot be changed in the method implementation.
    "- The method implementation includes that ...
    "  - attributes declared with READ-ONLY can be changed within
    "    the class they are declared.
    "  - static methods can only access static attributes.
    DATA(res) = zcl_demo_abap=>stat_meth1( ip1 = `a` "pass by reference (declaration with REFERENCE(...))
                                           ip2 = `b` "pass by reference (without REFERENCE(...))
                                           ip3 = `c` "pass by value (declaration with VALUE(...))
                                         ).
    out->write( data = res name = `res` ).

    "--- OPTIONAL/DEFAULT additions ---
    "The method implementation includes the predicate expression IS SUPPLIED
    "that checks whether a formal parameter is populated.

    DATA(res_tab1) = zcl_demo_abap=>stat_meth2( ip1 = `aaa` ip2 = `bbb` ).
    out->write( data = res_tab1 name = `res_tab1` ).

    DATA(res_tab2) = zcl_demo_abap=>stat_meth2( ip1 = `ccc` ).
    out->write( data = res_tab2 name = `res_tab2` ).

    DATA(res_tab3) = zcl_demo_abap=>stat_meth2( ip2 = `ddd` ).
    out->write( data = res_tab3 name = `res_tab3` ).

    DATA(res_tab4) = zcl_demo_abap=>stat_meth2( ).
    out->write( data = res_tab4 name = `res_tab4` ).

    "--- Generically typed formal parameters ---
    "In the following method calls, various data objects are
    "assigned to formal parameters. Note that returning parameters
    "must be completely typed.
    "In the example, the value passed for parameter ip_clike (which is
    "convertible to type string) is returned.
    DATA(res_gen1) = zcl_demo_abap=>stat_meth3( ip_data = VALUE string_table( ( `hi` ) ) "any data type
                                                ip_clike = abap_true "Character-like data type (such as c, n, string, etc.)
                                                ip_numeric = 1 "Numeric type (such as i, p, decfloat34 etc.)
                                                ip_any_table = VALUE string_table( ( `ABAP` ) ) "Any table type (standard, sorted, hashed)
                                              ).

    out->write( data = res_gen1 name = `res_gen1` ).

    DATA(res_gen2) = zcl_demo_abap=>stat_meth3( ip_data = 123
                                                ip_clike = 'ABAP'
                                                ip_numeric = CONV decfloat34( '1.23' )
                                                ip_any_table = VALUE string_hashed_table( ( `hello` ) ( `world` ) )
                                              ).

    out->write( data = res_gen2 name = `res_gen2` ).

  ENDMETHOD.

  METHOD class_constructor.
    "Assigning the current UTC timestamp to a static attribute
    stat_attr = utclong_current( ).
  ENDMETHOD.

  METHOD constructor.
    "Assigning the current UTC timestamp to an instance attribute
    inst_attr = utclong_current( ).
  ENDMETHOD.

  METHOD inst_meth1.
    inst_string = `Changed in inst_meth1`.

    "Instance methods can access both static and instance attributes.
    DATA(a) = inst_attr.
    DATA(b) = stat_attr.
  ENDMETHOD.

  METHOD inst_meth2.
    inst_string = |Changed in inst_meth2. Content of passed string: { ip }|.
  ENDMETHOD.

  METHOD inst_meth3.
    ep = ip1 + ip2.
  ENDMETHOD.

  METHOD inst_meth4.
    ret = |This string was assigned to the returning parameter at { utclong_current( ) }|.
  ENDMETHOD.

  METHOD inst_meth5.
    ep = |Strings '{ ip1 }' and '{ ip2 }' were assigned to the exporting parameter at { utclong_current( ) }|.
    ret = |Strings '{ ip1 }' and '{ ip2 }' were assigned to the returning parameter at { utclong_current( ) }|.
  ENDMETHOD.

  METHOD inst_meth6.
    "Demonstrating the self-reference 'me'
    "Its use is optional. The following data object intentionally
    "has the same name as an instance attribute specified in the
    "class's declaration part.
    DATA inst_string TYPE string.
    inst_string = `Local string`.

    chg = |The local data object was changed: '{ to_upper( chg ) }'\nChecking the self-reference me:\ninst_string: '{ inst_string }'\nme->inst_string: '{ me->inst_string }'|.
  ENDMETHOD.

  METHOD inst_meth7.
    ret = ipow( base = ip1 exp = ip2 ).
  ENDMETHOD.

  METHOD inst_meth8.
    IF ip1 = 1.
      RAISE EXCEPTION TYPE cx_uuid_error.
    ELSE.
      ret = `No exception was raised.`.
    ENDIF.
  ENDMETHOD.

  METHOD stat_meth1.
    "Static methods can only access static attributes.
    "DATA(test1) = inst_attr.
    DATA(test2) = stat_attr.

    "Only read access possible when parameters are passed by value
    DATA(a) = ip1.
    DATA(b) = ip2.

    "Write access is not possible (however, if you need to work with them
    "and change the content, you can create copies as above)
    "ip1 = `y`.
    "ip2 = `z`.

    "Pass by value, modification is possible
    ip3 = to_upper( ip3 ) && `##`.

    "Modifying a read only attribute is possible in the class
    "in which it is declared.
    read_only_attr = `Read-only attribute was modified`.

    ret = |ip1 = '{ ip1 }', ip2 = '{ ip2 }', ip3 (modified) = '{ ip3 }' / read_only_attr = '{ read_only_attr }'|.
  ENDMETHOD.

  METHOD stat_meth2.
    "Using IS SUPPLIED in an IF control structure
    IF ip1 IS SUPPLIED.
      APPEND |ip1 is supplied, value: '{ ip1 }'| TO ret.
    ELSE.
      APPEND |ip1 is not supplied, value: '{ ip1 }'| TO ret.
    ENDIF.

    "Using IS SUPPLIED with the COND operator
    APPEND COND #( WHEN ip2 IS SUPPLIED
                   THEN |ip2 is supplied, value: '{ ip2 }'|
                   ELSE |ip2 is not supplied, value: '{ ip2 }'| ) TO ret.
  ENDMETHOD.

  METHOD stat_meth3.
    "You may check the content in the debugger.
    DATA(a) = REF #( ip_data ).
    DATA(b) = REF #( ip_clike ).
    DATA(c) = REF #( ip_numeric ).
    DATA(d) = REF #( ip_any_table ).

    ret = ip_clike.
  ENDMETHOD.

ENDCLASS.
```

</details> 




<p align="right"><a href="#top">⬆️ back to top</a></p>


## Inheritance

The following table covers a selection of aspects about inheritance. The code snippets demonstrate local classes.

<table>

<tr>
<td> Aspect </td> <td> Notes/Code examples </td>
</tr>

<tr>
<td> 

Superclasses and subclasses

 </td>

 <td> 

- Inheritance means deriving any number of new classes ([subclasses](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubclass_glosry.htm)) from an existing class ([superclass](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensuperclass_glosry.htm)).  
- This derivation establishes a hierarchical relationship between superclasses and subclasses, forming an inheritance tree. A class can have multiple subclasses but only one direct superclass.  
- A subclass can also be a superclass to multiple direct subclasses but still has only one direct superclass.  

<br>

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

The inheritance tree of the example classes is as follows: `LCL1` is a superclass with direct subclasses `LCL2` and `LCL3`. `LCL3` is also a superclass from which more subclasses are derived. Similarly, `LCL5` is a superclass with `LCL6` as a direct subclass.


```
LCL1
  |
  |--LCL2
  |
  |--LCL3
  |   |
  |   |--LCL4
  |   |
  |   |--LCL5
  |   |   |
  |   |   |--LCL6
```

The classes are without any component specifications for demonstration purposes.


``` abap
CLASS lcl1 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
ENDCLASS.

CLASS lcl3 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
ENDCLASS.

CLASS lcl4 DEFINITION INHERITING FROM lcl3.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl4 IMPLEMENTATION.
ENDCLASS.

CLASS lcl5 DEFINITION INHERITING FROM lcl3.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl5 IMPLEMENTATION.
ENDCLASS.

CLASS lcl6 DEFINITION INHERITING FROM lcl5.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl6 IMPLEMENTATION.
ENDCLASS.
``` 

</details>  
<br>

 </td>
</tr>

<tr>
<td> 

Components specified in superclasses

 </td>

 <td> 

- Subclasses inherit and therefore adopt all components, such as attributes or methods, from superclasses.
- Subclasses can use these components and add new ones.
- Subclasses know about superclass components, but superclasses do not know about subclass components, unless a friendship relationgship is defined (see further down). Generally, superclasses are unaware of their subclasses.
- By adding new components - and redefining methods, as covered below - subclasses become more specific, while superclasses remain more generic. This distinction is important for polymorphism and casting as outlined below.
- If a subclass has no additional components, it contains only the components of the superclass, except for those in the private visibility section (unless friendship is granted).
- Changes or additions to attributes in subclasses are not visible to superclasses, making these changes relevant only to the subclass and its subclasses.

<br>

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

The inheritance tree of the following example classes is as follows:

```
LCL1
  |
  |--LCL2
  |   |
  |   |--LCL3
```

The code snippet includes various data object declarations and value assignments by exploring:

- Subclasses adopt all components from the public and protected sections. The example does not cover redefining instance methods.
- Adding new components.
- Superclasses are unaware of their subclasses and their components.
- Instance attributes are not accessible in static methods.


``` abap
CLASS lcl1 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    DATA pub_inst_str_lcl1 TYPE string.
    CLASS-DATA pub_stat_str_lcl1 TYPE string.
    METHODS inst_meth_lcl1.
    CLASS-METHODS stat_meth_lcl1.
  PROTECTED SECTION.
    DATA prot_inst_str_lcl1 TYPE string.
    CLASS-DATA prot_stat_str_lcl1 TYPE string.
  PRIVATE SECTION.
    DATA priv_inst_str_lcl1 TYPE string.
    CLASS-DATA priv_stat_str_lcl1 TYPE string.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD inst_meth_lcl1.
    "Accessing attributes of the class itself
    pub_inst_str_lcl1 = `a`.
    pub_stat_str_lcl1 = `b`.

    prot_inst_str_lcl1 = `c`.
    prot_stat_str_lcl1 = `d`.

    priv_inst_str_lcl1 = `e`.
    priv_stat_str_lcl1 = `f`.

    "Superclasses do not know of the components of their subclasses.
    "Visible static, public components can be accessed using the class name
    "followed by => and the component name. In case of this particular
    "local class example, the definition part of lcl2 must be specified before
    "the implementation part of lcl1 for the lcl2=>pub_stat_str_lcl2 ...
    "statement to work.
    "pub_inst_str_lcl2 = `g`.
    "pub_stat_str_lcl2 = `h`.
    "prot_inst_str_lcl2 = `i`.
    "prot_stat_str_lcl2 = `j`.
    "priv_inst_str_lcl2 = `k`.
    "priv_stat_str_lcl2 = `l`.
    "lcl2=>pub_stat_str_lcl2 = `m`.
  ENDMETHOD.

  METHOD stat_meth_lcl1.
    "Instance attributes are not accessible in static methods.

    "pub_inst_str_lcl1 = `g`.
    pub_stat_str_lcl1 = `h`.

    "prot_inst_str_lcl1 = `i`.
    prot_stat_str_lcl1 = `j`.

    "priv_inst_str_lcl1 = `k`.
    priv_stat_str_lcl1 = `l`.
  ENDMETHOD.

ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    DATA pub_inst_str_lcl2 TYPE string.
    CLASS-DATA pub_stat_str_lcl2 TYPE string.
    DATA added_pub_inst_str_lcl2 TYPE string.
    CLASS-DATA added_pub_stat_str_lcl2 TYPE string.
    METHODS inst_meth_lcl2.
    CLASS-METHODS stat_meth_lcl2.
  PROTECTED SECTION.
    DATA prot_inst_str_lcl2 TYPE string.
    CLASS-DATA prot_stat_str_lcl2 TYPE string.
    DATA added_prot_inst_str_lcl2 TYPE string.
    CLASS-DATA added_prot_stat_str_lcl2 TYPE string.
  PRIVATE SECTION.
    DATA priv_inst_str_lcl2 TYPE string.
    CLASS-DATA priv_stat_str_lcl2 TYPE string.
    DATA added_priv_inst_str_lcl2 TYPE string.
    CLASS-DATA added_priv_stat_str_lcl2 TYPE string.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD inst_meth_lcl2.
    "Accessing attributes of the class itself
    pub_inst_str_lcl2 = `a`.
    pub_stat_str_lcl2 = `b`.
    added_pub_inst_str_lcl2 = `c`.
    added_pub_stat_str_lcl2 = `d`.

    prot_inst_str_lcl2 = `e`.
    prot_stat_str_lcl2 = `f`.
    added_prot_inst_str_lcl2 = `g`.
    added_prot_stat_str_lcl2 = `h`.

    priv_inst_str_lcl2 = `i`.
    priv_stat_str_lcl2 = `j`.
    added_priv_inst_str_lcl2 = `k`.
    added_priv_stat_str_lcl2 = `l`.

    "Accessing attributes of the superclass
    pub_inst_str_lcl1 = `m`.
    pub_stat_str_lcl1 = `n`.

    prot_inst_str_lcl1 = `o`.
    prot_stat_str_lcl1 = `p`.

    "Private components of the superclass cannot be accessed
    "priv_inst_str_lcl1 = `q`.
    "priv_stat_str_lcl1 = `r`.

    "The following statement shows a syntax warning as an identically named
    "attribute as in the superclass is created.
    "DATA pub_inst_str_lcl1 TYPE string.
    "However, the following declaration using an identical name as one of the
    "superclass attributes is possible because the private component of the
    "superclass cannot be accessed.
    DATA priv_inst_str_lcl1 TYPE string.
  ENDMETHOD.

  METHOD stat_meth_lcl2.
    "Instance attributes are not accessible in static methods.

    "Accessing attributes of the class itself
    "pub_inst_str_lcl2 = `a`.
    pub_stat_str_lcl2 = `b`.
    "added_pub_inst_str_lcl2 = `c`.
    added_pub_stat_str_lcl2 = `d`.

    "prot_inst_str_lcl2 = `e`.
    prot_stat_str_lcl2 = `f`.
    "added_prot_inst_str_lcl2 = `g`.
    added_prot_stat_str_lcl2 = `h`.

    "priv_inst_str_lcl2 = `i`.
    priv_stat_str_lcl2 = `j`.
    "added_priv_inst_str_lcl2 = `k`.
    added_priv_stat_str_lcl2 = `l`.

    "Accessing attributes of the superclass
    "pub_inst_str_lcl1 = `m`.
    pub_stat_str_lcl1 = `n`.

    "prot_inst_str_lcl1 = `o`.
    prot_stat_str_lcl1 = `p`.

    "Private components of the superclass cannot be accessed
    "priv_inst_str_lcl1 = `q`.
    "priv_stat_str_lcl1 = `r`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl3 DEFINITION INHERITING FROM lcl2.
  PUBLIC SECTION.
    DATA pub_inst_str_lcl3 TYPE string.
    CLASS-DATA pub_stat_str_lcl3 TYPE string.
    METHODS inst_meth_lcl3.
    CLASS-METHODS stat_meth_lcl3.
  PROTECTED SECTION.
    DATA prot_inst_str_lcl3 TYPE string.
    CLASS-DATA prot_stat_str_lcl3 TYPE string.
  PRIVATE SECTION.
    DATA priv_inst_str_lcl3 TYPE string.
    CLASS-DATA priv_stat_str_lcl3 TYPE string.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD inst_meth_lcl3.
    "Accessing attributes of the class itself
    pub_inst_str_lcl3 = `a`.
    pub_stat_str_lcl3 = `b`.

    prot_inst_str_lcl3 = `c`.
    prot_stat_str_lcl3 = `d`.

    priv_inst_str_lcl3 = `e`.
    priv_stat_str_lcl3 = `f`.

    "Accessing attributes of the superclasses
    pub_inst_str_lcl1 = `g`.
    pub_stat_str_lcl1 = `h`.

    prot_inst_str_lcl1 = `i`.
    prot_stat_str_lcl1 = `j`.

    pub_inst_str_lcl2 = `k`.
    pub_stat_str_lcl2 = `l`.
    added_pub_inst_str_lcl2 = `m`.
    added_pub_stat_str_lcl2 = `n`.

    prot_inst_str_lcl2 = `o`.
    prot_stat_str_lcl2 = `p`.
    added_prot_inst_str_lcl2 = `q`.
    added_prot_stat_str_lcl2 = `r`.

    "Private components of the superclasses cannot be accessed
    "priv_inst_str_lcl1 = `s`.
    "priv_stat_str_lcl1 = `t`.

    "priv_inst_str_lcl2 = `u`.
    "priv_stat_str_lcl2 = `v`.
    "added_priv_inst_str_lcl2 = `w`.
    "added_priv_stat_str_lcl2 = `x`.
  ENDMETHOD.

  METHOD stat_meth_lcl3.
    "Instance attributes are not accessible in static methods.

    "Accessing attributes of the class itself
    "pub_inst_str_lcl3 = `a`.
    pub_stat_str_lcl3 = `b`.

    "prot_inst_str_lcl3 = `c`.
    prot_stat_str_lcl3 = `d`.

    "priv_inst_str_lcl3 = `e`.
    priv_stat_str_lcl3 = `f`.

    "Accessing attributes of the superclasses
    "pub_inst_str_lcl1 = `g`.
    pub_stat_str_lcl1 = `h`.

    "prot_inst_str_lcl1 = `i`.
    prot_stat_str_lcl1 = `j`.

    "pub_inst_str_lcl2 = `k`.
    pub_stat_str_lcl2 = `l`.
    "added_pub_inst_str_lcl2 = `m`.
    added_pub_stat_str_lcl2 = `n`.

    "prot_inst_str_lcl2 = `o`.
    prot_stat_str_lcl2 = `p`.
    "added_prot_inst_str_lcl2 = `q`.
    added_prot_stat_str_lcl2 = `r`.

    "Private components of the superclasses cannot be accessed
    "priv_inst_str_lcl1 = `s`.
    "priv_stat_str_lcl1 = `t`.

    "priv_inst_str_lcl2 = `u`.
    "priv_stat_str_lcl2 = `v`.
    "added_priv_inst_str_lcl2 = `w`.
    "added_priv_stat_str_lcl2 = `x`.
  ENDMETHOD.
ENDCLASS.
``` 

</details>
<br>

 </td>
</tr>

<tr>
<td> 

Redefinition of superclass methods

 </td>

 <td> 

- You can (but need not) reimplement public and protected instance methods of all preceding superclasses in subclasses through redefinition (`REDEFINITION` addition), making subclasses more specific.
- Reimplementing methods allows you to reuse the parameter interface without changes.
- You can use the pseudo-reference `super->...` to call direct superclass methods while implementing redefined instance methods, which is useful for taking over, overriding, or extending implementations.
- Static methods can be accessed but not redefined.
- Changes or additions of methods in subclasses are not visible to superclasses, affecting only the class itself and its subclasses.

<br>

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

The inheritance tree of the following example classes looks as follows:

```
LCL1
  |
  |--LCL2
  |   |
  |   |--LCL3
```

The example explores the redefinition of methods. 

Global class to run the example and visualize the implementations of the local classes in the CCIMP include.
The output should be as follows:

```
A
A1
B
A12
B3
C
```
<br>

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

    DATA(str1) = NEW lcl1( )->meth_lcl1( ).
    out->write( str1 ).
    DATA(str2) = NEW lcl2( )->meth_lcl1( ).
    out->write( str2 ).
    DATA(str3) = NEW lcl2( )->meth_lcl2( ).
    out->write( str3 ).
    DATA(str4) = NEW lcl3( )->meth_lcl1( ).
    out->write( str4 ).
    DATA(str5) = NEW lcl3( )->meth_lcl2( ).
    out->write( str5 ).
    DATA(str6) = NEW lcl3( )->meth_lcl3( ).
    out->write( str6 ).

  ENDMETHOD.
ENDCLASS.
``` 

Local classes in the CCIMP include:

```abap
CLASS lcl1 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS meth_lcl1 RETURNING VALUE(str1) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD meth_lcl1.
    str1 = `A`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS meth_lcl2 RETURNING VALUE(str2) TYPE string.
    METHODS meth_lcl1 REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD meth_lcl2.
    str2 = `B`.
  ENDMETHOD.

  METHOD meth_lcl1.
    DATA(super_str_lcl1) = super->meth_lcl1( ).
    str1 = super_str_lcl1 && `1`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl3 DEFINITION INHERITING FROM lcl2.
  PUBLIC SECTION.
    METHODS meth_lcl3 RETURNING VALUE(str3) TYPE string.
    METHODS meth_lcl2 REDEFINITION.
    METHODS meth_lcl1 REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD meth_lcl3.
    str3 = `C`.
  ENDMETHOD.
  METHOD meth_lcl1.
    DATA(super_str_lcl1) = super->meth_lcl1( ).
    str1 = super_str_lcl1 && `2`.
  ENDMETHOD.

  METHOD meth_lcl2.
    DATA(super_str_lcl2) = super->meth_lcl2( ).
    str2 = super_str_lcl2 && `3`.
  ENDMETHOD.
ENDCLASS.
```

</details>  
<br>
 </td>
</tr>

<tr>
<td> 

Visibility of components

 </td>

 <td> 

- Components in the public visibility section are accessible from anywhere (where visible).
- Protected components are accessible by subclasses and friends.
- Private components are not accessible by subclasses unless friendship is granted.
- See *Components specified in superclasses* for an example.

 </td>
</tr>

<tr>
<td> 

Additions impacting inheritance

 </td>

 <td> 

- You can use the `FINAL` addition to prevent classes from being inherited. 
- You can also apply the `FINAL` addition to methods to prevent them from being redefined in subclasses.
- Refer to the next section for notes on these topics and other inheritance and instantiation features.

 </td>
</tr>

<tr>
<td> 

Constructors 

 </td>

 <td> 


- Instance constructors (`constructor`):
  - Subclasses cannot redefine the instance constructors of superclasses.
  - These constructors are called automatically when creating an object; they cannot be called explicitly.
  - In inheritance, a subclass's constructor must call all its superclasses' constructors. When you implement the instance constructor in subclasses, you must use a `super->constructor( )` call to call the direct superclass's constructor, even if the superclass does not explicitly declare it. Similarly, even if a subclass does not define and implement its constructor, the superclass's constructor will be called.
  - It is required to fill any non-optional importing parameters.
  - Class instantiation is controlled by specific additions (`CREATE PUBLIC/PROTECTED/PRIVATE`), affecting the ability to call constructors. The constructor's visibility cannot be more specific than the instance creator's visibility. For example, if a class is declared using `CREATE PUBLIC`, the constructor must be in the public section. If a subclass implicitly specifies `CREATE PUBLIC`, its constructor must also be public. If it specifies `CREATE PROTECTED`, the constructor can be in the public or protected section, but not private. Subclasses can specify their instantiability independently from the superclass. In classes declared using `CREATE PRIVATE`, the constructor is only visible within the class itself, unless friendship is granted. Consider making such classes final to prevent derivation.
- Static constructors (`class_constructor`):
  - These constructors are implicitly available in all classes, whether declared or not.
  - They are called when creating a class instance for the first time in an ABAP program or when accessing a static component, except for types and constants.
  - In inheritance, static constructors in the entire inheritance tree are called first.
  - A static constructor is called only once during program runtime.
  - A static constructor is always public.

<br>

<details>
  <summary>🟢 Example 1</summary>
  <!-- -->

<br>

The inheritance tree of the following example classes is as follows: 

```
LCL1
  |
  |--LCL2
  |
  |--LCL3
  |   |
  |   |--LCL4
```

This example explores how instance and static constructors can be specified. It shows that subclasses can determine their own instantiation independently of the superclass. The visibility of the constructor cannot be more specific than that of the `CREATE ...` specification.

``` abap
CLASS lcl1 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD class_constructor.
  ENDMETHOD.

  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

"Implicit CREATE PUBLIC
CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS constructor.
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD class_constructor.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.
ENDCLASS.

"CREATE PROTECTED
CLASS lcl3 DEFINITION INHERITING FROM lcl1 CREATE PROTECTED.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    "METHODS constructor.
  PROTECTED SECTION.
    METHODS constructor.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD class_constructor.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.
ENDCLASS.

"CREATE PRIVATE
CLASS lcl4 DEFINITION INHERITING FROM lcl3 CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS constructor.
  PROTECTED SECTION.
    "METHODS constructor.
  PRIVATE SECTION.
    "METHODS constructor.
ENDCLASS.

CLASS lcl4 IMPLEMENTATION.
  METHOD class_constructor.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.
ENDCLASS.

"No instances of the subclass can be created. unless friendship is created
*CLASS lcl5 DEFINITION INHERITING FROM lcl4.
*...
``` 

</details>  

<br>

<details>
  <summary>🟢 Example 2</summary>
  <!-- -->

<br>

The inheritance tree of the following example classes is as follows: 

```
LCL1
  |
  |--LCL2
  |   |
  |   |--LCL3
  |   |   |
  |   |   |--LCL4
```

This example explores the order of instance and static constructor calls during object creation. The constructors add strings to a string table. To visualize the output, run the global class, which includes object creations. Refer to the notes on the example output below.

Global class:

```abap
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
    DATA(oref1) = NEW lcl1( ).
    out->write( lcl1=>tab ).
    out->write( repeat( val = `_` occ = 30 ) && |\n\n| ).

    CLEAR lcl1=>tab.

    DATA(oref2) = NEW lcl2( ).
    out->write( lcl1=>tab ).
    out->write( repeat( val = `_` occ = 30 ) && |\n\n| ).

    CLEAR lcl1=>tab.

    DATA(oref3) = NEW lcl3( ).
    out->write( lcl1=>tab ).
    out->write( repeat( val = `_` occ = 30 ) && |\n\n| ).

    CLEAR lcl1=>tab.

    DATA(oref4) = NEW lcl4( ).
    out->write( lcl1=>tab ).
  ENDMETHOD.
ENDCLASS.
```

Local classes in the CCIMP include:

```abap
CLASS lcl1 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.
    CLASS-METHODS class_constructor.
    CLASS-DATA tab TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD class_constructor.
    APPEND `lcl1 class_constructor` TO tab.
  ENDMETHOD.

  METHOD constructor.
    APPEND `lcl1 constructor` TO tab.
  ENDMETHOD.
ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS constructor.
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD class_constructor.
    APPEND `lcl2 class_constructor` TO tab.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    APPEND `lcl2 constructor` TO tab.
  ENDMETHOD.

ENDCLASS.

CLASS lcl3 DEFINITION INHERITING FROM lcl2.
  PUBLIC SECTION.
    METHODS constructor.
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD class_constructor.
    APPEND `lcl3 class_constructor` TO tab.
  ENDMETHOD.

  METHOD constructor.

    super->constructor( ).
    APPEND `lcl3 constructor` TO tab.
  ENDMETHOD.

ENDCLASS.

CLASS lcl4 DEFINITION INHERITING FROM lcl3.
  PUBLIC SECTION.
    METHODS constructor.
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl4 IMPLEMENTATION.
  METHOD class_constructor.
    APPEND `lcl4 class_constructor` TO tab.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    APPEND `lcl4 constructor` TO tab.
  ENDMETHOD.

ENDCLASS.
```

Notes on the output: 

- When an object is created, static constructors are called first, following the inheritance tree top down. Then, instance constructors are called, also top down.
- Note that when the class runs, the example is executed in a single internal session. The static constructor of a class is called only once. Therefore, you will not see `lcl1 class_constructor` in the output when creating an object of `lcl2`, and so on.

    ```
    lcl1 class_constructor
    lcl1 constructor      
    ______________________________

    lcl2 class_constructor
    lcl1 constructor      
    lcl2 constructor     
    ...
    ```
    If you comment out `DATA(oref1) = NEW lcl1( ).` and `out->write( lcl1=>tab ).`, which are the statements accessing `lcl1` for the first time, and then run the class, you will see in the output (or while debugging) that the static constructor is indeed called. The same applies to the static constructor of `lcl2`, which is not called when creating an object for `lcl3`, and so on.
    
    ```
    lcl1 class_constructor
    lcl2 class_constructor
    lcl1 constructor      
    lcl2 constructor      
    ______________________________

    ...
    ```


</details>  

<br>

 </td>
</tr>

<tr>
<td> 

`object` as root node of inheritance trees

 </td>

 <td> 

- In inheritance trees, the root node is the predefined `object` class, representing an empty, abstract class.
- If a class does not use the `INHERITING FROM` addition, it is implicitly a subclass of `object`.
- The following statement uses a released ABAP class and RTTI (see the [Dynamic Programming]() cheat sheet) to determine that the root class of a class that does not explicitly inherit from another class is `object`.

    ``` abap
    DATA(superclass) = cast cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( 'CL_SYSTEM_UUID' ) )->get_super_class_type( )->get_relative_name( ).
    ASSERT superclass = `OBJECT`.
    ``` 
 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Additions Related to Inheritance and Instantiation

The table below includes selected syntax related to inheritance in class and method declarations. It also includes additions related to instantiation.  

> **💡 Note**<br>
> - Some of the syntax options have already been mentioned previously. This is to summarize. 
> - The code examples show local classes and interfaces declared, for example, in the [CCIMP include](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenccimp_glosry.htm) of a class pool. 
> - The snippets provided do not represent all possible syntax combinations. For the complete picture, refer to the ABAP Keyword Documentation. Additional syntax options are available in the context of friendship (`GLOBAL FRIENDS/FRIENDS`), testing (`FOR TESTING`), [RAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenarap_glosry.htm) (`FOR BEHAVIOR OF`; to declare [ABAP behavior pools](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbehavior_pool_glosry.htm)), and more.
> - The order of the additions can vary.


**Declarations in Classes**

<table>

<tr>
<td> Syntax Example </td> <td> Details </td>
</tr>

<tr>
<td> 

``` abap
CLASS zcl_demo DEFINITION     
  PUBLIC
  FINAL
  CREATE PUBLIC .
``` 

 </td>
<td>

<br>

- `... PUBLIC ...`: Specifies that the class is a global class, available globally within the class library. Most of the subsequent snippets use the `PUBLIC` addition as the focus is on global classes.
- `... FINAL ...`: Specifies that the class cannot have any subclasses, effectively prohibiting inheritance. This addition seals off a branch of the inheritance tree. In final classes, all methods are automatically final.
- `... CREATE PUBLIC ...`: Specifies that the class can be instantiated wherever it is visible. Not specifying the addition `CREATE ...` means `CREATE PUBLIC` by default.

<br>

The following code example demonstrates local classes (no `... PUBLIC ...` addition is used): 

```abap
CLASS lcl1 DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS meth1.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD meth1.
   "Creating an instance of lcl1 within the class itself.
   DATA(oref1) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl2 DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS meth2.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD meth2.
   "Creating an instance of lcl1 wherever visible.
   DATA(oref2) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.

"Inheriting from the class is not possible
*CLASS lcl3 DEFINITION INHERITING FROM lcl1.
*  PUBLIC SECTION.
*ENDCLASS.
*
*CLASS lcl3 IMPLEMENTATION.
*ENDCLASS.
```

</td>
</tr>

<tr>
<td> 

``` abap
CLASS zcl_demo DEFINITION   
  PUBLIC
  CREATE PUBLIC .
``` 

 </td>
<td>

<br>

This class permits inheritance because it does not include the `FINAL` addition. Subclasses can be derived from this superclass.

<br>

The following code example demonstrates local classes: 

```abap
CLASS lcl1 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS meth1.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD meth1.
    "Creating an instance of lcl1 within the class itself.
    DATA(oref1) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl2 DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS meth2.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD meth2.
    "Creating an instance of lcl1 wherever visible.
    DATA(oref2) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.

"Inheriting from the class
CLASS lcl3 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS meth1 REDEFINITION.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD meth1.
    super->meth1( ).
    "Example instantiations (all classes allow instantation)
    "Note: lcl3 does not specify CREATE PUBLIC explicitly. It is
    "specified implicitly.
    DATA(oref3) = NEW lcl1( ).
    DATA(oref4) = NEW lcl2( ).
    DATA(oref5) = NEW lcl3( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl4 DEFINITION.
  PUBLIC SECTION.
    METHODS meth3.
ENDCLASS.

CLASS lcl4 IMPLEMENTATION.
  METHOD meth3.
    DATA(oref6) = NEW lcl3( ).
    DATA(oref7) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.
```

</td>
</tr>

<tr>
<td> 

``` abap
CLASS zcl_demo DEFINITION    
  PUBLIC
  CREATE PROTECTED .
``` 

 </td>
<td>

<br>

- This class permits inheritance because it does not include the `FINAL` addition. 
- `... CREATE PROTECTED ...`: Specifies that the class can only be instantiated within its own methods, its subclasses' methods, or those of its friends.

<br>

The following code example demonstrates local classes: 

```abap
CLASS lcl1 DEFINITION CREATE PROTECTED.
  PUBLIC SECTION.
    METHODS meth1.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD meth1.
    "Creating an instance of lcl1 within the class itself.
    DATA(oref1) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl2 DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS meth2.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD meth2.
    "Creating an instance of lcl1 is not possible as this class is not
    "a subclass or friend
    "DATA(oref2) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.

"Inheriting from the class
CLASS lcl3 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS meth1 REDEFINITION.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD meth1.
    super->meth1( ).

    "Creating an instance is possible as lcl3 is a subclass of lcl1
    DATA(oref3) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************

"Creating instances is allowed for friends
"Note the DEFINITION DEFERRED additions for lcl5. It is used to make the class known in the program
"before its actual definition. Such statements are particularly necessary in local classes, and if a
"reference to a local class is made before it is defined.
"A method of lcl5 includes the creation of an instance of lcl4, demonstrating that friends can
"indeed create the instances. You can comment out FRIENDS lcl5 in the class declaration part of
"lcl4. Consequently, a syntax error is displayed in lcl5 for the instance creation.
CLASS lcl5 DEFINITION DEFERRED.
CLASS lcl4 DEFINITION CREATE PROTECTED FRIENDS lcl5.
  PUBLIC SECTION.
    METHODS meth3.
ENDCLASS.

CLASS lcl4 IMPLEMENTATION.
  METHOD meth3.
  ENDMETHOD.
ENDCLASS.

CLASS lcl5 DEFINITION CREATE PROTECTED.
  PUBLIC SECTION.
    METHODS meth4.
ENDCLASS.

CLASS lcl5 IMPLEMENTATION.
  METHOD meth4.
    DATA(oref_friend) = NEW lcl4( ).
  ENDMETHOD.
ENDCLASS.
```


</td>
</tr>

<tr>
<td> 

``` abap
CLASS zcl_demo DEFINITION    
  FINAL
  CREATE PRIVATE .
``` 

 </td>
<td>

<br>

- This class does not permit inheritance because it includes the `FINAL` addition.
- `... CREATE PRIVATE ...`: Specifies that the class can only be instantiated within its own methods or those of its friends. It cannot be instantiated as a component of (not befriended) subclasses.
- Consider the implications of defining a superclass in this manner:
  - External users cannot instantiate a subclass.
  - If inheritance is allowed, subclasses cannot instantiate themselves though. This is because they cannot access the superclass's instance constructor, preventing the creation of subclass instances. However, this can be altered if the subclass is a friend of the superclass.
  - Similarly, subclass objects cannot be created within their superclass if declared using `CREATE PROTECTED` or `CREATE PRIVATE`. This is only possible if the superclasses are friends with their subclasses.
- The `FINAL` addition can be beneficial with `CREATE PRIVATE` to prevent the derivation of subclasses.
- Note: In each class, the `CREATE PUBLIC`, `CREATE PROTECTED`, and `CREATE PRIVATE` additions of the `CLASS` statement control who can create an instance of the class and who can call its instance constructor.

<br>

The following code example demonstrates local classes: 

```abap
CLASS lcl1 DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    METHODS meth1.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD meth1.
    "Creating an instance of lcl1 within the class itself.
    DATA(oref1) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.

"Creating instances is not possible outside the class
CLASS lcl2 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS meth2.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD meth2.
    "Creating an instance is not possible
    "DATA(oref2) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.

"Note: lcl1 is not defined as FINAL, so derivation is not explicitly
"disallowed. The following class definition shows a syntax warning as
"instantiation is not allowed.
*CLASS lcl3 DEFINITION INHERITING FROM lcl1.
*  PUBLIC SECTION.
*ENDCLASS.
*
*CLASS lcl3 IMPLEMENTATION.
*ENDCLASS.

**********************************************************************

"Instantiation is only possible for befriended classes
"Defining another class with CREATE PRIVATE, FINAL also specified
"See the notes on DEFINITION DEFERRED above.
CLASS lcl5 DEFINITION DEFERRED.
CLASS lcl4 DEFINITION FINAL CREATE PRIVATE FRIENDS lcl5.
  PUBLIC SECTION.
    METHODS meth3.
ENDCLASS.

CLASS lcl4 IMPLEMENTATION.
  METHOD meth3.
    "Creating an instance of lcl4 in the class itself.
    DATA(oref3) = NEW lcl4( ).

    "Creating an instance of lcl1 not possible outside of the class.
    DATA(oref4) = NEW lcl2( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl5 DEFINITION.
  PUBLIC SECTION.
    METHODS meth4.
ENDCLASS.

CLASS lcl5 IMPLEMENTATION.
  METHOD meth4.
    "Creating instances of lcl4 is allowed as lcl5 is befriended.
    DATA(oref_friend) = NEW lcl4( ).
  ENDMETHOD.
ENDCLASS.

"Another class with CREATE PRIVATE, FINAL not specified
CLASS lcl7 DEFINITION DEFERRED.
CLASS lcl6 DEFINITION CREATE PRIVATE FRIENDS lcl7.
  PUBLIC SECTION.
    METHODS meth5.
ENDCLASS.

CLASS lcl6 IMPLEMENTATION.
  METHOD meth5.
    "Creating an instance of lcl6 in the class itself.
    DATA(oref5) = NEW lcl6( ).
  ENDMETHOD.
ENDCLASS.

"Creating a subclass of lcl6, which is befriended
CLASS lcl7 DEFINITION INHERITING FROM lcl6.
  PUBLIC SECTION.
    METHODS meth5 REDEFINITION.
ENDCLASS.

CLASS lcl7 IMPLEMENTATION.
  METHOD meth5.
    "Creating instances of lcl6 is allowed as lcl7 is befriended.
    DATA(oref_friend) = NEW lcl6( ).
  ENDMETHOD.
ENDCLASS.
```

</td>
</tr>

<tr>
<td> 

``` abap
CLASS zcl_demo DEFINITION     
  PUBLIC
  ABSTRACT
  CREATE ...
``` 

 </td>
<td>

- `... ABSTRACT ...`:
  - Defines abstract classes
  - Abstract classes cannot be instantiated.
  - To use the instance components of an abstract class, you can instantiate a subclass of that class.
  - Abstract classes may contain both abstract and concrete instance methods. However, abstract methods are not implemented within abstract classes.
  - By adding the `FINAL` addition, abstract classes can be made final. In these cases, only static components are usable. While instance components may be declared, they are not usable.

<br>

The following code example demonstrates local classes: 

```abap
CLASS lcl1 DEFINITION ABSTRACT CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS meth1.
    DATA inst_attr TYPE i.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD meth1.
    "Creating an instance of lcl1 within the class itself
    "is not possible. Generally, abstract classes cannot be
    "instantiated.
    "DATA(oref1) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.

"Creating instances is not possible outside the class
CLASS lcl2 DEFINITION CREATE PUBLIC.
PUBLIC SECTION.
  METHODS meth2.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD meth2.
    "Creating an instance is not possible
    "DATA(oref2) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.

"Using instance components of abstract classes only via subclasses
CLASS lcl3 DEFINITION INHERITING FROM lcl1.
PUBLIC SECTION.
  METHODS meth1 REDEFINITION.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD meth1.
    ...
    DATA(oref3) = NEW lcl3( ).
    oref3->inst_attr = 1.

    "Creating instances of abstract classes not possible
    "DATA(oref4) = NEW lcl1( ).
  ENDMETHOD.
ENDCLASS.
```

</td>
</tr>

<tr>
<td> 

``` abap
CLASS zcl_sub DEFINITION       
  INHERITING FROM zcl_demo       
  ...
``` 

 </td>
<td>

- `... INHERITING FROM ...`: 
  - Can be specified in subclasses inheriting from visible superclasses  
  - If not specified, the class implicitly inherits from the predefined, empty, abstract class `OBJECT` (the root object).
  - A subclass inherits all components of the superclasses. 
    - For example, if an internal table `itab` is declared as a static component in superclass `zcl_super`, subclasses can refer to `itab` directly, not necessarily by specifying the class name as with `zcl_super=>itab` (which is also possible). 
    - If the superclass defines a type, subclasses cannot define a type with the same name.
  - The visibility of the components remains unchanged. 
  - Only the public and protected components of the superclass are visible in the subclass.
  - Private components of superclasses are inaccessible in subclasses.
  - The properties of inherited components are immutable. However, subclasses can declare additional components (with unique names) and redefine inherited methods without altering the interface.
  - Upon instantiation of a subclass, all superclasses are also instantiated, ensuring the initialization of superclass attributes through calling superclass constructors.

<br>

The following code example demonstrates local classes: 

```abap
CLASS lcl1 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS meth1.
    DATA num1 TYPE i.
    CLASS-DATA str TYPE string.
  PROTECTED SECTION.
    DATA num2 TYPE i.
  PRIVATE SECTION.
    DATA num3 TYPE i.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD meth1.
    ...
  ENDMETHOD.
ENDCLASS.

"Subclasses inheriting from lcl1
CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS meth1 REDEFINITION.
    "Data object with the same name as specified in the superclass
    "is not possible in subclasses
    "DATA num1 TYPE i.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD meth1.
    ...
    "Only public and protected components can be accessed in subclasses.
    DATA(oref) = NEW lcl2( ).
    oref->num1 = 1.
    oref->num2 = 2.
    "Private components are not accessible
    "oref->num3 = 3.

    "Static attribute accessible with the name directly without class=> (which is also possible)
    str = `hello`.
    lcl1=>str = `hi`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl3 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS meth1 REDEFINITION.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD meth1.
    ...
  ENDMETHOD.
ENDCLASS.
```

</td>
</tr>


</table>


**Declarations in Instance Methods**

<table>

<tr>
<td> Syntax Example </td> <td> Details </td>
</tr>

<tr>
<td> 

``` abap
METHODS some_meth FINAL ...    
``` 

 </td>
<td>

<br>

- Declares final methods
- These methods cannot be overridden in subclasses.
- Note: In final classes, all methods are inherently final. Therefore, the `FINAL` addition cannot be specified. Instance constructors are always final, but the use of the `FINAL` addition is optional.

<br>

The following code example demonstrates local classes: 

```abap
CLASS lcl1 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS meth1.
    METHODS meth2 FINAL.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD meth1.
    ...
  ENDMETHOD.
  
  METHOD meth2.
    ...
  ENDMETHOD.  
ENDCLASS.

"Subclass inheriting from lcl1
CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS meth1 REDEFINITION.
    "meth2 is a final method and cannot be redefined
    "METHODS meth2 REDEFINITION.    
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD meth1.
    ...
  ENDMETHOD.
ENDCLASS.
```

</td>
</tr>

<tr>
<td> 

``` abap
METHODS some_meth ABSTRACT ...    
``` 

 </td>
<td>

<br>

- Declares abstract methods
- Can only be used in abstract classes
- You can implement these methods in a subclass (by redefining using the `REDEFINITION`addition), not in the abstract class itself. When declared, there is no implementation part in the abstract class.  
- All instance methods can be declared as abstract, except for instance constructors.
- Private methods cannot be redefined and can therefore not be declared as abstract.
- If abstract methods are declared in classes that are both abstract and final, they cannot be implemented. Therefore, the methods are not usable.
- In interfaces, methods are implicitly abstract as interfaces do not contain method implementations.

<br>

The following code example demonstrates local classes: 

```abap
CLASS lcl1 DEFINITION ABSTRACT CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS meth1.
    METHODS meth2 ABSTRACT.
    DATA num TYPE i.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  "meth1 is a nonabstract meth
  METHOD meth1.
    ...
    num = 1.
  ENDMETHOD.

  "meth2 is defined as abstract and cannot be implemented
  "in the abstract class
*  METHOD meth2.
*    ...
*  ENDMETHOD.
ENDCLASS.

"Subclass inheriting from lcl1
CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS meth1 REDEFINITION.
    "The abstract method meth2 must be implemented in subclasses.
    METHODS meth2 REDEFINITION.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD meth1.
    ...
    super->meth1( ).
    DATA(result) = num + 1.
  ENDMETHOD.

  METHOD meth2.
    ...
  ENDMETHOD.
ENDCLASS.
```

</td>
</tr>


<tr>
<td> 

``` abap
"Instance constructor
METHODS constructor. 
"Static constructor
CLASS-METHODS class_constructor.
``` 

 </td>
<td>

<br>

Find information on constructors in the previous section.

<br>

The following code example demonstrates constructors. When the constructors are called, a string is added to an internal table (a static attribute). You can try out the example and explore the calling of constructors as follows:

1. Create a global class that implements the class run, and add the following codes. Local classes, implemented in the CCIMP include, are called. The example is designed to write the content of the internal table to the console. 

```abap
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
    DATA(oref1) = NEW lcl2( ).
    DATA(tab) = lcl1=>tab.
    out->write( tab ).
    out->write( |\n| ).
    DATA(oref2) = NEW lcl3( ).
    tab = lcl1=>tab.
    out->write( tab ).
  ENDMETHOD.

ENDCLASS.
```

2. Add the following code to the CCIMP include (*Local Types* tab in ADT).


```abap
CLASS lcl1 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.
    CLASS-METHODS class_constructor.
    CLASS-DATA tab TYPE string_table.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.

  METHOD class_constructor.
    APPEND `lcl1 class_constructor` TO tab.
  ENDMETHOD.

  METHOD constructor.
    APPEND `lcl1 constructor` TO tab.
  ENDMETHOD.

ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS constructor.
    CLASS-METHODS class_constructor.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD class_constructor.
    APPEND `lcl2 class_constructor` TO tab.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    APPEND `lcl2 constructor` TO tab.
  ENDMETHOD.
ENDCLASS.

CLASS lcl3 DEFINITION INHERITING FROM lcl2.
  PUBLIC SECTION.
    METHODS constructor.
    CLASS-METHODS class_constructor.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD class_constructor.
    APPEND `lcl3 class_constructor` TO tab.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    APPEND `lcl3 constructor` TO tab.
  ENDMETHOD.
ENDCLASS.
```

3. Activate and go back to the global class. Choose F9 to run the example. The internal table content that is output demonstrates the sequence of constructor calls of the simple example.

</td>
</tr>


<tr>
<td> 

``` abap
METHODS some_meth REDEFINITION.           
METHODS another_meth FINAL REDEFINITION.      
``` 

 </td>
<td>

<br>

- Specified in subclasses to redefine inherited methods from superclasses.
- The method's implementation is expected to reimplement the inherited method. However, the subclass's new implementation conceals the superclass's implementation.
- The redefined method accesses the private components of its class, not any similarly named private components in the superclass.
- The superclass's implementation can be called in the redefined method using the [pseudo reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpseudo_reference_glosry.htm "Glossary Entry") `super->meth( ).`. Note that non-optional importing parameters must be filled.
- The redefinition is valid for subclasses until the method is redefined again.
- The `FINAL` addition can be specified, preventing further redefinition of the method in other subclasses.

<br>

The following code example demonstrates local classes: 

```abap
CLASS lcl1 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS meth1.
    METHODS meth2.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD meth1.
    ...
  ENDMETHOD.

  METHOD meth2.
    ...
  ENDMETHOD.
ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS meth1 REDEFINITION.
    METHODS meth2 FINAL REDEFINITION.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD meth1.
    ...
  ENDMETHOD.

  METHOD meth2.
    ...
  ENDMETHOD.
ENDCLASS.

CLASS lcl3 DEFINITION INHERITING FROM lcl2.
  PUBLIC SECTION.
    METHODS meth1 REDEFINITION.
    "meth2 cannot be further redefined as it is defined as FINAL
    "in the class's superclass
    "METHODS meth2 REDEFINITION.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD meth1.
    ...
  ENDMETHOD.
ENDCLASS.
```

</td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


### Excursion: Inheritance Example

> **💡 Note**<br>
> This example is also included in the ABAP cheat sheet repository: [zcl_demo_abap_oo_inheritance_1](./src/zcl_demo_abap_oo_inheritance_1.clas.abap)

Expand the following collapsible section for example classes. The example classes explore inheritance and demonstrate a selection of the inheritance-related syntax described above. The inheritance tree consists of four example classes. The base class includes the implementation of the classrun interface. The example is designed to output information to the console. So, you can execute this class using F9 in ADT.
The purpose of the example and information output is to visualize and explore concepts and syntax related to inheritance, checking out when and how methods are called, redefining methods, abstract and final classes and methods.


<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

The inheritance tree of the example classes is as follows: 

```
ZCL_DEMO_ABAP_OO_INHERITANCE_1
  |
  |--ZCL_DEMO_ABAP_OO_INHERITANCE_2
  |   |
  |   |--ZCL_DEMO_ABAP_OO_INHERITANCE_3
  |   |   |
  |   |   |--ZCL_DEMO_ABAP_OO_INHERITANCE_4
```

Notes on the example: 
- All classes include instance and static constructor declarations.
- Many instance methods are declared in all classes to demonstrate inheritance. However, there is no meaningful implementation in these methods in all classes. All instance methods include the same code. 
  - The purpose of the code in the method implementations is to add a line to a log table (which is output to the console) with various pieces of information: 
      - Name of the method that is called 
      - In which class the method is implemented when it is called
      - From which class the method is called
      - Whether the method is inherited, redefined, final, or a static method
      - Visibility of the method
      - Time stamp of when the method is called 
  - The information retrieval is implemented in a static method in the `ZCL_DEMO_ABAP_OO_INHERITANCE_1` class by getting callstack information to determine which method in which class was called by whom. Based on the retrieved class and method names, RTTI is used to get detailed information about the methods.
- `ZCL_DEMO_ABAP_OO_INHERITANCE_1`
  - Allows inheritance and represents the root class of the inheritance hierarchy
  - Declares several instance methods in each visibility section
    - One of them is declared with `FINAL`, so no redefinition is possible in subclasses.
  - Includes the implementation of the classrun interface meaning this class is executable using F9 in ADT.
    - The class includes an internal table that represents a log table and that is output to the console as described above.    
- `ZCL_DEMO_ABAP_OO_INHERITANCE_2`
  - Inherits from `ZCL_DEMO_ABAP_OO_INHERITANCE_1` and allows inheritance itself 
  - Specifies `CREATE PROTECTED`, so the class can only be instantiated in methods of its subclasses, of the class itself, and of its friends   
    - You may want to try to create an instance of the class in `ZCL_DEMO_ABAP_OO_INHERITANCE_1` like this `DATA(oref) = NEW zcl_demo_abap_oo_inheritance_2( ).` It is not possible. In `ZCL_DEMO_ABAP_OO_INHERITANCE_3` and `ZCL_DEMO_ABAP_OO_INHERITANCE_4`, for example, it is possible.
  - Declares several instance methods
    - One of them is declared with `FINAL`, so no redefinition is possible in subclasses
    - Instance methods of the direct superclass are redefined
      - Note: Private methods of superclasses cannot be redefined. You cannot specify abstract methods, which is only possible in abstract classes. Abstract methods are generally not possible in the private visibility section since they cannot be redefined.
   - Declares a static method to delegate method calls of this class
     - The implementation includes the creation of an instance of the class and instance method calls (including redefined methods).
     - It is called in the classrun implementation in `ZCL_DEMO_ABAP_OO_INHERITANCE_1`.
- `ZCL_DEMO_ABAP_OO_INHERITANCE_3`
  - Inherits from `ZCL_DEMO_ABAP_OO_INHERITANCE_2` and thus from `ZCL_DEMO_ABAP_OO_INHERITANCE_1`     
  - Declared as abstract class using the `ABSTRACT` addition, so no instances can be created from the class
  - Declares several instance methods
    - Two abstract methods are included using the `ABSTRACT` addition, so they can only be implemented in subclasses (there is no implementation of these methods in the class) 
    - Instance methods of the direct superclass are redefined as well as methods from two levels up the inheritance hierarchy
    - One redefined method specifies `FINAL REDEFINITION`, so a further redefinition in subclasses is not possible.
- `ZCL_DEMO_ABAP_OO_INHERITANCE_4`
  - Inherits from `ZCL_DEMO_ABAP_OO_INHERITANCE_3` and thus from `ZCL_DEMO_ABAP_OO_INHERITANCE_2` and `ZCL_DEMO_ABAP_OO_INHERITANCE_1`
  - Specifies `FINAL` and so does not allow inheritance     
  - Declares several instance methods
    - Instance methods of the direct superclass, which is an abstract class, are redefined. It is mandatory to redefine the abstract methods. 
    - Other instance methods from further levels up the inheritance hierarchy are redefined (except one method that is declared with `FINAL REDEFINITION` in `ZCL_DEMO_ABAP_OO_INHERITANCE_3`)
    - For demonstration purposes, instance methods implemented in the abstract direct superclass (instances of abstract classes cannot be created) are called in the respective redefined methods by referring to the direct superclass using the syntax `super->...`.
  - Declares a static method to delegate method calls of this class


To try the example classes out, create four demo classes named 

- `ZCL_DEMO_ABAP_OO_INHERITANCE_1` 
- `ZCL_DEMO_ABAP_OO_INHERITANCE_2`
- `ZCL_DEMO_ABAP_OO_INHERITANCE_3`
- `ZCL_DEMO_ABAP_OO_INHERITANCE_4`

and paste the code into it. After activation, choose *F9* in ADT to execute the class `ZCL_DEMO_ABAP_OO_INHERITANCE_1`. The example is set up to display output in the console.


<br>

Class `ZCL_DEMO_ABAP_OO_INHERITANCE_1` 

```abap
CLASS zcl_demo_abap_oo_inheritance_1 DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Classrun interface
    INTERFACES if_oo_adt_classrun.

    "Instance/static constructor declarations
    METHODS constructor.
    CLASS-METHODS class_constructor.

    "Instance method declarations
    METHODS meth_public_1.
    "Final method
    METHODS meth_public_1_final FINAL.

    "Components used for logging information about method calls
    TYPES: BEGIN OF s_log,
             method            TYPE string,
             implemented_where TYPE string,
             called_from       TYPE syrepid,
             is_inherited      TYPE abap_boolean,
             is_redefined      TYPE abap_boolean,
             is_final          TYPE abap_boolean,
             visibility        TYPE abap_visibility,
             is_static_method  TYPE abap_boolean,
             called_at         TYPE utclong,
           END OF s_log,
           t_log TYPE TABLE OF s_log WITH EMPTY KEY.

    CLASS-DATA log_tab TYPE t_log.
    CLASS-METHODS get_method_info RETURNING VALUE(info) TYPE s_log.

  PROTECTED SECTION.
    METHODS meth_protected_1.

  PRIVATE SECTION.
    METHODS meth_private_1.
ENDCLASS.



CLASS zcl_demo_abap_oo_inheritance_1 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "----- First level in the inheritance hierarchy ----
    "Creating an instance of the class
    DATA(oref_super) = NEW zcl_demo_abap_oo_inheritance_1( ).

    "Calling methods of the class
    oref_super->meth_public_1( ).
    oref_super->meth_public_1_final( ).
    oref_super->meth_protected_1( ).
    oref_super->meth_private_1( ).

    "----- Second level in the inheritance hierarchy ----
    "The instance creation and method calling is delegated to
    "a static method in the class
    zcl_demo_abap_oo_inheritance_2=>perform_meth_calls_2( ).

    "----- Third level in the inheritance hierarchy ----
    "Note: The class zcl_demo_abap_oo_inheritance_3 is abstract and contains
    "both non-abstract and abstract instance methods. Instances of abstract
    "classes cannot be created. So, the following statement is not possible.
    "DATA(oref_3) = NEW zcl_demo_abap_oo_inheritance_3( ).

    "Instance components of an abstract class can be accessed via its subclasses.
    "zcl_demo_abap_oo_inheritance_4 inherits from zcl_demo_abap_oo_inheritance_3 and
    "redefines methods of zcl_demo_abap_oo_inheritance_3. Both abstract methods (which
    "are mandatory to implement) and non-abstract methods are redefined. To also access
    "the method implementations of the non-abstract instance methods of
    "zcl_demo_abap_oo_inheritance_3, the respective implementations of the redefined
    "methods in zcl_demo_abap_oo_inheritance_4 include method calls to the direct
    "superclass using the syntax super->meth( ).. The instance methods of
    "zcl_demo_abap_oo_inheritance_3 are called in the context of the static method call
    "via zcl_demo_abap_oo_inheritance_4 below.

    "----- Fourth level in the inheritance hierarchy ----
    "As above, the instance creation and method calling is delegated to
    "a static method in the class. This method call includes method calls to
    "non-abstract instance methods implemented in zcl_demo_abap_oo_inheritance_3.
    zcl_demo_abap_oo_inheritance_4=>perform_meth_calls_4( ).

    "Writing the log table to the console
    out->write( data = log_tab name = `log_tab` ).

    "Excursion: Using RTTI to retrieve the name of the superclass
    "As this class starts an inheritance hierarchy, the superclass of this class
    "is the root class OBJECT.
    DATA(tdo_cl) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( 'ZCL_DEMO_ABAP_OO_INHERITANCE_1' ) ).
    DATA(superclass) = tdo_cl->get_super_class_type( )->get_relative_name( ).
    out->write( |\n\n| ).
    out->write( data = superclass name = `superclass` ).

  ENDMETHOD.

  METHOD class_constructor.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD constructor.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_private_1.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_1.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_1.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_1_final.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD get_method_info.
    "This method retrieves callstack information to determine which method in which
    "class was called by whom.
    "Based on the retrieved class and method names, RTTI is used to get detailed
    "information about methods (such as the visibility or whether the method is
    "inherited, redefined, final, and a static method).

    "Getting callstack information
    DATA(call_stack_tab) = xco_cp=>current->call_stack->full( )->from->position( 2
                            )->to->position( 2 )->as_text( xco_cp_call_stack=>format->adt( )
                            )->get_lines( )->value.

    IF lines( call_stack_tab ) < 2.
      RETURN.
    ENDIF.

    LOOP AT call_stack_tab INTO DATA(wa) TO 2.
      DATA(tabix) = sy-tabix.
      SPLIT wa AT ` ` INTO TABLE DATA(entry).
      DELETE entry WHERE table_line IS INITIAL.

      DATA(class_name) = condense( val = entry[ 1 ] to = `` ).

      IF tabix = 1.
        info-implemented_where = class_name.

        DATA(meth_name) = condense( val = to_upper( entry[ 2 ] ) to = `` ).
        info-method = meth_name.

        IF class_name IS NOT INITIAL AND meth_name IS NOT INITIAL.
          DATA(tdo_cl) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( class_name ) ).
          DATA(methods_cl) = tdo_cl->methods.
          DATA(meth_info) = VALUE #( methods_cl[ name = meth_name ] OPTIONAL ).
          IF meth_info IS NOT INITIAL.
            info-is_inherited = meth_info-is_inherited.
            info-is_redefined = meth_info-is_redefined.
            info-is_final = meth_info-is_final.
            info-visibility = meth_info-visibility.
            info-is_static_method = meth_info-is_class.
          ENDIF.
        ENDIF.

      ELSE.
        info-called_from = class_name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
```

Class `ZCL_DEMO_ABAP_OO_INHERITANCE_2` 

```abap
CLASS zcl_demo_abap_oo_inheritance_2 DEFINITION
  INHERITING FROM zcl_demo_abap_oo_inheritance_1
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.
    "Instance/static constructor declarations
    METHODS constructor.
    CLASS-METHODS class_constructor.

    "Instance method declarations
    METHODS meth_public_2.
    METHODS meth_public_2_final FINAL.

    "Static method declaration for display purposes
    CLASS-METHODS perform_meth_calls_2.

    "Redefining method from the class one level up in the inheritance hierarchy
    "(i.e. the direct superclass)
    METHODS meth_public_1 REDEFINITION.

    "Excursions:
    "- Redefining the final public method of the superclass is not possible.
    "- The same applies to constructors.
    "- The following type has the same name as a type in the superclass. Since components
    "  are inherited, the following declaration is not possible.
    "TYPES t_log TYPE string_table.
    "- Similary, the log table, which is a static component in the superclass, can be
    "  referenced in the method implementations using 'log_table'. Using the class name
    "  and => is also possible, but not required.
    "INSERT VALUE #( ) INTO TABLE zcl_demo_abap_oo_inheritance_1=>log_tab.

  PROTECTED SECTION.
    "Instance method declaration
    METHODS meth_protected_2.

    "Redefining method from the class one level up in the inheritance hierarchy
    "(i.e. the direct superclass)
    METHODS meth_protected_1 REDEFINITION.

  PRIVATE SECTION.
    "Ecursion:
    "- The following declarations are not possible.
    "- Private methods cannot be redefined.
    "METHODS meth_private_1 REDEFINITION.
    "- Abstract methods can only be declared in abstract classes. And, since
    "  private methods cannot be redefined, abstract private methods are not
    "  possible.
    "METHODS meth_private_2_abstract ABSTRACT.
ENDCLASS.



CLASS zcl_demo_abap_oo_inheritance_2 IMPLEMENTATION.
  METHOD class_constructor.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_2.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_2_final.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_2.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_1.
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_1.
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD perform_meth_calls_2.
    "Method of this class
    "Creating an instance of the class
    DATA(oref_2) = NEW zcl_demo_abap_oo_inheritance_2( ).

    "Calling methods of this class
    oref_2->meth_public_2( ).
    oref_2->meth_public_2_final( ).
    oref_2->meth_protected_2( ).
    "Calling redefined methods from the class one level up in the inheritance hierarchy (direct superclass)
    oref_2->meth_protected_1( ).
    oref_2->meth_public_1( ).

  ENDMETHOD.

ENDCLASS.
```

Class `ZCL_DEMO_ABAP_OO_INHERITANCE_3` 

```abap
CLASS zcl_demo_abap_oo_inheritance_3 DEFINITION
  INHERITING FROM zcl_demo_abap_oo_inheritance_2
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Instance/static constructor declarations
    METHODS constructor.
    CLASS-METHODS class_constructor.

    "Instance method declarations
    METHODS meth_public_3.
    "Abstract method
    METHODS meth_public_3_abstract ABSTRACT.

    "Redefining methods from the class ...
    "... one level up in the inheritance hierarchy (i.e. the direct superclass)
    METHODS meth_public_2  REDEFINITION.
    "... two levels up in the inheritance hierarchy
    METHODS meth_public_1 REDEFINITION.

  PROTECTED SECTION.
    "Instance method declarations
    METHODS meth_protected_3.
    "Abstract method
    METHODS meth_protected_3_abstract ABSTRACT.

    "Redefining methods from the class ...
    "... one level up in the inheritance hierarchy (i.e. the direct superclass)
    METHODS meth_protected_2 REDEFINITION.
    "... two levels up in the inheritance hierarchy
    "Specifying the FINAL addition
    METHODS meth_protected_1 FINAL REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_oo_inheritance_3 IMPLEMENTATION.

  METHOD class_constructor.
     INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_3.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_3.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_2.
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_2.
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_1.
    "Reimplementing a method from the class two levels up in the inheritance hierarchy
    "Note that the method is specified with FINAL REDEFINITION. So, a further redefinition in subclasses is not possible.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_1.
    "Reimplementing a method from the class two levels up in the inheritance hierarchy
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.
ENDCLASS.
```

Class `ZCL_DEMO_ABAP_OO_INHERITANCE_4` 

```abap
CLASS zcl_demo_abap_oo_inheritance_4 DEFINITION
  INHERITING FROM zcl_demo_abap_oo_inheritance_3
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Instance/static constructor declarations
    METHODS constructor.
    CLASS-METHODS class_constructor.

    "Instance method declaration
    METHODS meth_public_4.
    "Static method declaration for display purposes
    CLASS-METHODS perform_meth_calls_4.

    "Redefining methods from the class ...
    "... one level up in the inheritance hierarchy (i.e. the direct superclass)
    METHODS meth_public_3 REDEFINITION.
    "Note: Redefining the abstract method here is mandatory.
    METHODS meth_public_3_abstract REDEFINITION.
    "... two levels up in the inheritance hierarchy
    METHODS meth_public_2 REDEFINITION.
    "... three levels up in the inheritance hierarchy
    METHODS meth_public_1 REDEFINITION.

  PROTECTED SECTION.

    "Instance method declaration
    METHODS meth_protected_4.

    "Redefining methods from the class ...
    "... one level up in the inheritance hierarchy (i.e. the direct superclass)
    METHODS meth_protected_3 REDEFINITION.
    "Note: Redefining the abstract method here is mandatory.
    METHODS meth_protected_3_abstract REDEFINITION.
    "... two levels up in the inheritance hierarchy
    METHODS meth_protected_2 REDEFINITION.
    "... three levels up in the inheritance hierarchy
    "The meth_protected_1 method is specified with FINAL REDEFINITION in the
    "direct superclass. Therefore, a further redefinition is not possible.
    "The following statement is not possible.
    "METHODS meth_protected_1 REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_oo_inheritance_4 IMPLEMENTATION.

  METHOD class_constructor.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_4.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_4.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_3.
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    "Calling this instance method that is redefined in the abstract direct superclass (instances of abstract classes cannot be created)
    "by referring to the direct superclass using the syntax super->...
    super->meth_public_3( ).
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_3.
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    "This method is a non-abstract instance method of the abstract direct superclass. Instances of abstract classes
    "cannot be created. The syntax super->meth is used to also call instance methods of the abstract direct superclass
    "for output purposes.
    super->meth_protected_3( ).
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_3_abstract.
    "Implementating abstract methods are only possible in subclasses of abstract classes
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_3_abstract.
    "Implementating abstract methods are only possible in subclasses of abstract classes
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_2.
    "Reimplementing a method from the class two levels up in the inheritance hierarchy
    "Calling this instance method that is redefined in the abstract direct superclass (instances of abstract
    "classes cannot be created) by referring to the direct superclass using the syntax super->....
    super->meth_public_2( ).
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_2.
    "Reimplementing a method from the class two levels up in the inheritance hierarchy
    "Calling this instance method that is redefined in the abstract direct superclass (instances of abstract
    "classes cannot be created) by referring to the direct superclass using the syntax super->....
    super->meth_protected_2( ).
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_1.
    "Reimplementing a method from the class three levels up in the inheritance hierarchy
    "Calling this instance method that is redefined in the abstract direct superclass (instances of abstract
    "classes cannot be created) by referring to the direct superclass using the syntax super->....
    super->meth_public_1( ).
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD perform_meth_calls_4.
    "Method of this class
    "Creating an instance of the class
    DATA(oref_4) = NEW zcl_demo_abap_oo_inheritance_4( ).

    "Calling methods of this class
    oref_4->meth_public_4( ).
    oref_4->meth_protected_4( ).
    "Calling redefined methods from the class ...
    "... one level up in the inheritance hierarchy (direct superclass)
    "Among them are abstract methods that can only be implemented in subclasses.
    "Note that the implementations of the non-abstract, redefined methods in this
    "class includes method calls of the abstract direct superclass so that
    "also these implementations are called for output purposes.
    oref_4->meth_public_3( ).
    oref_4->meth_public_3_abstract( ).
    oref_4->meth_protected_3( ).
    oref_4->meth_protected_3_abstract( ).
    "... two levels up in the inheritance hierarchy
    oref_4->meth_public_2( ).
    oref_4->meth_protected_2( ).
    "... three levels up in the inheritance hierarchy
    oref_4->meth_public_1( ).
    "Note: The following method call calls the method implementation in
    "class zcl_demo_abap_oo_inheritance_3. The method is specified with FINAL
    "REDEFINITION. So, it cannot be redefined in this class inheriting from
    "zcl_demo_abap_oo_inheritance_3.
    oref_4->meth_protected_1( ).
  ENDMETHOD.

ENDCLASS.
```

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Polymorphism and Casting (Upcast/Downcast)

The object orientation concept
[polymorphism](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpolymorphism_glosry.htm "Glossary Entry")
means you can address differently implemented methods belonging to different objects of different classes using one and the
same reference variable, for example,
object reference variables pointing to a superclass can point to objects of a subclass.

Note the concept of static and dynamic type in this context:

-   Object reference variables (and also interface reference variables) have both a
    [static](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_type_glosry.htm "Glossary Entry")
    and a [dynamic
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendynamic_type_glosry.htm "Glossary Entry").
-   When declaring an object reference variable, e. g. `DATA oref TYPE REF TO cl`, you determine the static type, i. e.
    `cl` - a class - is used to declare the reference variable that is statically defined in the code. This is the class of an object to which the reference variable points to.
- Similarly, the dynamic type also defines the class of an object which the reference variable points to. However, the dynamic type is determined at runtime, i. e. the class of an object which the reference variable points to can change.
- Relevant for? This differentiation enters the picture in polymorphism when a reference variable typed with reference to a subclass can always be assigned to reference variables typed with reference to one of its superclasses or their interfaces. That's what is called [upcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenup_cast_glosry.htm "Glossary Entry") (or widening cast). Or the assignment is done the other way round. That's what is called [downcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendown_cast_glosry.htm "Glossary Entry") (or narrowing cast).

> **✔️ Hints**<br>
> - The following basic rule applies: The static type is always more general than or the same as the dynamic type. The other way round: The dynamic type is always more special than or equal to the static type.
>- That means:
>   - If the static type is a class, the dynamic type must be the same class or one of its subclasses.
>   - If the static type is an interface, the dynamic type must implement the interface.

-   Regarding assignments: If it can be statically checked that an assignment is possible
    although the types are different, the assignment is done using the
    [assignment
    operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm "Glossary Entry")
    `=` that triggers an upcast automatically.
-   Otherwise, it is a
    [downcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendown_cast_glosry.htm "Glossary Entry").
    Here, the assignability is not checked until runtime. The downcast - in contrast to upcasts -
    must be triggered explicitly using the [casting
    operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencasting_operator_glosry.htm "Glossary Entry")
    [`CAST`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_cast.htm). You might see code using the older
    operator [`?=`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove_cast.htm).
-   See more information in the topic [Assignment Rules for Reference
    Variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_references.htm).

As an example, assume there is an inheritance tree with `lcl_super` as the superclass and `lcl_sub` as a direct subclass. `lcl_sub2` is a direct subclass of `lcl_sub`.

In the following code snippet, the rule is met since the superclass is either the same as or more generic than the subclass (the subclass has, for example, redefined methods and is, thus, more specific). Hence, the assignment of an object reference variable pointing to the subclass to a variable pointing to a superclass works. An upcast is triggered. After this casting, the type of `oref_super` has changed and the methods of `lcl_sub` can be accessed via `oref_super`.

``` abap
"Creating object references
DATA(oref_super) = NEW lcl_super( ).

DATA(oref_sub) = NEW lcl_sub( ).

"Upcast
oref_super = oref_sub.

"The casting might be done when creating the object.
DATA super_ref TYPE REF TO lcl_super.

super_ref = NEW lcl_sub( ).
```

- As mentioned above, a downcast must be triggered
manually. Just an assignment like `oref_sub = oref_super.`
does not work. A syntax error occurs saying the right-hand variable's type cannot be converted to the left-hand variable's type.
- If you indeed want to carry out this casting, you must use
`CAST` (or you might see code using the older operator `?=`) to overcome this syntax error (but just the syntax error!). Note: You might also use these casting operators for the upcasts. That means `oref_super = oref_sub.` has the same effect as `oref_super = CAST #( oref_sub ).`. Using the casting operator for upcasts is usually not necessary.
- At runtime, the assignment is checked and if the conversion does not work, you face a (catchable) exception. Even more so, the assignment `oref_sub = CAST #( oref_super ).` does not throw a syntax error but it does not work in this example either because it violates the rule mentioned above (`oref_sub` is more specific than `oref_super`).
- To check whether such an assignment is possible
on specific classes, you can use the predicate expression [`IS INSTANCE OF`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_instance_of.htm)
or the case distinction [`CASE TYPE OF`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcase_type.htm). Carrying out an upcast before the downcast ensures that the left-hand variable's type is compatible to the right-hand variable's type.

``` abap
DATA(oref_super) = NEW lcl_super( ).
DATA(oref_sub) = NEW lcl_sub( ).
DATA(oref_sub2) = NEW lcl_sub2( ).

"Downcast impossible (oref_sub is more specific than oref_super);
"the exception is caught here

TRY.
  oref_sub = CAST #( oref_super ).
  CATCH cx_sy_move_cast_error INTO DATA(e).
    ...
ENDTRY.

"Working downcast with a prior upcast

oref_super = oref_sub2.

"Due to the prior upcast, the following check is actually not necessary.

IF oref_super IS INSTANCE OF lcl_sub.
  oref_sub = CAST #( oref_super ).
  ...
ENDIF.

"Excursion RTTI: Downcasts, CAST and method chaining
"Downcasts particularly play, for example, a role in the context of
"retrieving type information using RTTI. Method chaining is handy
"because it reduces the lines of code in this case.
"The example below shows the retrieval of type information
"regarding the components of a structure. 
"Due to the method chaining in the second example, the three
"statements in the first example are reduced to one statement.

DATA struct4cast TYPE zdemo_abap_carr.

DATA(rtti_a) = cl_abap_typedescr=>describe_by_data( struct4cast ).
DATA(rtti_b) = CAST cl_abap_structdescr( rtti_a ).
DATA(rtti_c) = rtti_b->components.

DATA(rtti_d) = CAST cl_abap_structdescr(
  cl_abap_typedescr=>describe_by_data( struct4cast )
      )->components.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example with Local Classes Demonstrating Upcasts and Downcasts

The following example class demonstrates upcasts and downcasts using multiple local classes to avoid the extra creation of multiple global classes and to have a self-contained example. To try the example out, create a demo class named `zcl_demo_abap` and paste the code into it (*Global Class* and *Local Types* tabs in ADT). After activation, choose *F9* in ADT to execute the class. The example is not set up to display output in the console, but it contains comments. You may want to set break points and walk through the demo assignments.

The inheritance tree of the local example classes is as follows: 

```
LCL1
  |
  |--LCL2
  |
  |--LCL3
  |   |
  |   |--LCL4
```


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

    "Object reference variables
    "Create object references
    DATA: oref1 TYPE REF TO lcl1,
          oref2 TYPE REF TO lcl2,
          oref3 TYPE REF TO lcl3,
          oref4 TYPE REF TO lcl4.

    "Creating objects
    oref1 = NEW #( ).
    oref2 = NEW #( ).
    oref3 = NEW #( ).
    oref4 = NEW #( ).

    "Calling redefined methods
    DATA text TYPE string.

    "A
    text = oref1->meth( ).
    "B
    text = oref2->meth( ).
    "C
    text = oref3->meth( ).
    "D
    text = oref4->meth( ).

    "Polymorphism is demonstrated as follows: A reference variable typed with
    "reference to a subclass can always be assigned to reference variables typed
    "with reference to one of its superclasses.
    "The following statements show upcasts using the assignment operator =.
    "The upcast works because the basic rule is met that the static types of the
    "targets are more general or identical to the static type of the sources.
    "Example:
    "- oref1 is statically typed with the type ref to lcl1.
    "- oref2 is statically typed with the type ref to lcl2.
    "- In the assignment 'oref1 = oref2.', the rule is met as the target variable's
    "  static type of oref1 is more general (higher up in the inheritance tree).
    "- For the assignment to finally work, the dynamic type (which is the actual
    "  object the object reference points to at runtime) must be the same class or one
    "  of its subclasses.

    "lcl1 is the superclass of the other local classes
    oref1 = oref2.
    "B
    text = oref1->meth( ).

    oref1 = oref3.
    "C
    text = oref1->meth( ).

    "lcl4 is a subclass of lcl3 (and thus also from lcl1)
    oref1 = oref4.
    "D
    text = oref1->meth( ).

    oref3 = oref4.
    "D
    text = oref3->meth( ).

    "Upcasts also work if the static types are identical
    DATA(oref1b) = NEW lcl1( ).
    oref1 = oref1b.
    text = oref1->meth( ).

    "Re-creating objects
    oref1 = NEW #( ).
    oref2 = NEW #( ).
    oref3 = NEW #( ).
    oref4 = NEW #( ).

    "The following statement (basically moving down the inheritance tree) cannot
    "be specified using the assignment operator as there is a type conflict.
    "The right-hand variable's type cannot be converted to the left-hand variable's type.
    "If you indeed want to cast, you must perform a downcast.
    "oref2 = oref1.

    "Downcasts
    "Here, the static type of the target variables are more specific than the static types
    "of the source variables. The downcast must be triggered explicitly, e.g. by the CAST or
    "the older ?= operator.
    "However, note that the assignability is not checked until runtime. Only the syntax error
    "as in the previous statement is not displayed anymore.

    "The following example triggers a downcast explicitly. So, the syntax error from above
    "is not displayed. However, that does not mean that the assignment actually works.
    "For the downcast to work, the dynamic type must be the same class or one of its subclasses.
    TRY.
        oref2 = CAST #( oref1 ).
      CATCH cx_sy_move_cast_error INTO DATA(error).
        text = error->get_text( ).
    ENDTRY.

    "The following example performs an upcast so that the dynamic type of oref1 refers to
    "one of the subclasses. However, the example downcast does not work either. lcl3 (static type
    "of oref3) is not a subclass of lcl2 oref2 refers to.
    oref1 = oref3.

    TRY.
        oref2 = CAST #( oref1 ).
      CATCH cx_sy_move_cast_error INTO error.
        text = error->get_text( ).
    ENDTRY.

    "The following downcasts work because the rule is met that the dynamic type must be the same
    "class or one of its subclasses.
    "Upcast before the downcast
    oref1 = oref2.
    "Downcast (dynamic type is the same)
    oref2 = CAST #( oref1 ).
    "B
    text = oref2->meth( ).

    "Downcast (dynamic type is one of the subclasses)
    "Re-creating objects
    oref1 = NEW #( ).
    oref4 = NEW #( ).

    "lcl4 (oref4 points to) is a subclass of lcl3 (oref3 points to), and thus of lcl1
    "Upcast before the downcast
    oref1 = oref4.
    "Downcast (dynamic type is one of the subclasses)
    oref3 = CAST #( oref1 ).
    "D
    text = oref3->meth( ).

    "You can check whether downcasts are possible with IF and CASE statements
    "Re-creating objects
    oref1 = NEW #( ).
    oref3 = NEW #( ).
    oref4 = NEW #( ).

    IF oref1 IS INSTANCE OF lcl3.
      oref3 = CAST #( oref1 ).
      text = oref3->meth( ).
    ELSE.
      "This section is executed in the example.
      text = `...`.
    ENDIF.

    "Upcast
    oref1 = oref4.

    CASE TYPE OF oref1.
      WHEN TYPE lcl3.
        "This section is executed in the example.
        oref3 = CAST #( oref1 ).
        "D
        text = oref3->meth( ).
      WHEN OTHERS.
        ...
    ENDCASE.

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
CLASS lcl1 DEFINITION.
  PUBLIC SECTION.
    METHODS meth RETURNING VALUE(text) TYPE string.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD meth.
    text = `A`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS meth REDEFINITION.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD meth.
    text = `B`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl3 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS meth REDEFINITION.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD meth.
    text = `C`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl4 DEFINITION INHERITING FROM lcl3.
  PUBLIC SECTION.
    METHODS meth REDEFINITION.
ENDCLASS.

CLASS lcl4 IMPLEMENTATION.
  METHOD meth.
    text = `D`.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

<details>
  <summary>🟢 Example with local classes implementing a local interface</summary>
  <!-- -->

<br>

- This example, which anticipates interfaces, is similar to the previous one. Here, local classes implement a local interface. 
- The example emphasizes that interface reference variables can point to objects of classes that implement the same interface.

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

    "Object reference variables
    "Creating object references
    DATA:
      "lcl1 implements the interface lif
      "Up to oref4, the classes are in an inheritance relationship
      oref1 TYPE REF TO lcl1,
      oref2 TYPE REF TO lcl2,
      oref3 TYPE REF TO lcl3,
      oref4 TYPE REF TO lcl4,
      "Implements the interface lif but is not in the inheritance relationship above
      oref5 TYPE REF TO lcl5,
      "Interface reference variable
      iref  TYPE REF TO lif.

    "Creating objects
    oref1 = NEW #( ).
    oref2 = NEW #( ).
    oref3 = NEW #( ).
    oref4 = NEW #( ).
    oref5 = NEW #( ).

    "Calling redefined methods
    DATA text TYPE string.

    "A
    text = oref1->lif~meth( ).
    "B
    text = oref2->lif~meth( ).
    "C
    text = oref3->lif~meth( ).
    "D
    text = oref4->lif~meth( ).
    "E
    text = oref5->lif~meth( ).

    "The static type can also refer to an interface.
    "Assignments

    "The static types of the example reference variables refer to classes
    "that implement the interface and are in an inheritance relationship.
    iref = oref1.
    "A
    text = iref->meth( ).

    iref = oref2.
    "B
    text = iref->meth( ).

    iref = oref3.
    "C
    text = iref->meth( ).

    iref = oref4.
    "D
    text = iref->meth( ).

    "Example class that implements the interface
    iref = oref5.
    "E
    text = iref->meth( ).

    "Both lcl1 (oref1) and lcl5 (oref5) implement the interface lif,
    "so the assignment works as the static type of the interface reference
    "variables is the same.
    DATA irefb TYPE REF TO lif.
    iref = oref1.
    irefb = oref5.
    iref = irefb.

    "Downcasts
    "Upcast before the downcast
    iref = oref4.
    "The following statement triggers a sytax error
    "oref3 = iref.
    oref3 = CAST #( iref ).
    "D
    text = oref3->lif~meth( ).

    "Downcast not possible because the dynamic type is not the same or a subclass
    TRY.
        iref = oref2.
        oref3 = CAST #( iref ).
      CATCH cx_sy_move_cast_error INTO DATA(error).
        text = error->get_text( ).
    ENDTRY.
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
INTERFACE lif.
  METHODS meth RETURNING VALUE(text) TYPE string.
ENDINTERFACE.

CLASS lcl1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.
  METHOD lif~meth.
    text = `A`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS lif~meth REDEFINITION.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD lif~meth.
    text = `B`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl3 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS lif~meth REDEFINITION.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.
  METHOD lif~meth.
    text = `C`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl4 DEFINITION INHERITING FROM lcl3.
  PUBLIC SECTION.
    METHODS lif~meth REDEFINITION.
ENDCLASS.

CLASS lcl4 IMPLEMENTATION.
  METHOD lif~meth.
    text = `D`.
  ENDMETHOD.
ENDCLASS.

CLASS lcl5 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl5 IMPLEMENTATION.
  METHOD lif~meth.
    text = `E`.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Upcasts and Downcasts Using the RTTS Inheritance Tree

The examples in the following code snippet use object reference variables to illustrate the class hierarchy of the [Runtime Type Services (RTTS)](#runtime-type-services-rtts), which is covered in more detail in the [Dynamic Programming](06_Dynamic_Programming.md) cheat sheet. 

Hierarchy tree of the classes:

```
CL_ABAP_TYPEDESCR
  |
  |--CL_ABAP_DATADESCR
  |   |
  |   |--CL_ABAP_ELEMDESCR
  |   |   |
  |   |   |--CL_ABAP_ENUMDESCR
  |   |
  |   |--CL_ABAP_REFDESCR
  |   |--CL_ABAP_COMPLEXDESCR
  |       |
  |       |--CL_ABAP_STRUCTDESCR
  |       |--CL_ABAP_TABLEDESCR
  |
  |--CL_ABAP_OBJECTDESCR
     |
     |--CL_ABAP_CLASSDESCR
     |--CL_ABAP_INTFDESCR
``` 

Examples:

```abap
"------------ Object reference variables ------------

"Static and dynamic types
"Defining an object reference variable with a static type
DATA tdo TYPE REF TO cl_abap_typedescr.

"Retrieving type information
"The reference the reference variable points to is either cl_abap_elemdescr,
"cl_abap_enumdescr, cl_abap_refdescr, cl_abap_structdescr, or cl_abap_tabledescr.
"So, it points to one of the subclasses. The static type of tdo refers to
"cl_abap_typedescr, however, the dynamic type is one of the subclasses mentioned.
"in the case of the example, it is cl_abap_elemdescr. Check in the debugger.
DATA some_string TYPE string.
tdo = cl_abap_typedescr=>describe_by_data( some_string ).

"Some more object reference variables
DATA tdo_super TYPE REF TO cl_abap_typedescr.
DATA tdo_elem TYPE REF TO cl_abap_elemdescr.
DATA tdo_data TYPE REF TO cl_abap_datadescr.
DATA tdo_gen_obj TYPE REF TO object.

"------------ Upcasts ------------

"Moving up the inheritance tree
"Assignments:
"- If the static type of target variable is less specific or the same, an assignment works.
"- The target variable inherits the dynamic type of the source variable.

"Static type of target variable is the same
tdo_super = tdo.

"Examples for static types of target variables that are less specific
"Target variable has the generic type object
tdo_gen_obj = tdo.

"Target variable is less specific because the direct superclass of cl_abap_elemdescr
"is cl_abap_datadescr
"Note: In the following three assignments, the target variable remains initial 
"since the source variables do not (yet) point to any object.
tdo_data = tdo_elem.

"Target variable is less specific because the direct superclass of cl_abap_datadescr
"is cl_abap_typedescr
tdo_super = tdo_data.

"Target variable is less specific because the class cl_abap_typedescr is higher up in
"the inheritance tree than cl_abap_elemdescr
tdo_super = tdo_elem.

"The casting happens implicitly. You can also excplicitly cast and use
"casting operators, but it is usually not required.
tdo_super = CAST #( tdo ).
tdo_super ?= tdo.

"In combination with inline declarations, the CAST operator can be used to provide a
"reference variable with a more general type.
DATA(tdo_inl_cast) = CAST cl_abap_typedescr( tdo_elem ).

CLEAR: tdo_super, tdo_elem, tdo_data, tdo_gen_obj.

"------------ Downcasts ------------

"Moving down the inheritance tree
"Assignments:
"- If the static type of the target variable is more specific than the static type
"  of the source variable, performing a check whether it is less specific or the same
"  as the dynamic type of the source variable is required at runtime before the assignment
"- The target variable inherits the dynamic type of the source variable, however, the target
"  variable can accept fewer dynamic types than the source variable
"- Downcasts are always performed explicitly using casting operators

"Static type of the target is more specific
"object -> cl_abap_typedescr
tdo_super = CAST #( tdo_gen_obj ).
"cl_abap_typedescr -> cl_abap_datadescr
"Note: Here, the dynamic type of the source variable is cl_abap_elemdescr.
tdo_data = CAST #( tdo ).
"cl_abap_datadescr -> cl_abap_elemdescr
tdo_elem = CAST #( tdo_data ).
"cl_abap_typedescr -> cl_abap_elemdescr
tdo_elem = CAST #( tdo_super ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Checking the Dynamic Type of Object Reference Variables

- Special control structures using `IF ... IS INSTANCE OF ...` and `CASE TYPE OF` check the dynamic type of non-initial object reference variables and the static type of initial object reference variables.
- The branch's first statement block is executed when a specified class or interface is more general than or equal to the given type.
- In doing so, you can especially check if assignments between object reference variable work, and you can prevent errors in downcasts.


```abap
DATA type_descr_obj TYPE REF TO cl_abap_typedescr.
DATA type_descr_obj_gen TYPE REF TO object.
DATA str TYPE string.

"In the examples above, the assignments work. The following code snippets
"deal with examples in which a downcast is not possible. An exception is
"raised.
DATA str_table TYPE string_table.
DATA type_descr_obj_table TYPE REF TO cl_abap_tabledescr.

"With the following method call, type_descr_obj points to an object with
"reference to cl_abap_tabledescr.
type_descr_obj = cl_abap_typedescr=>describe_by_data( str_table ).

"Therefore, the following downcast works.
type_descr_obj_table = CAST #( type_descr_obj ).

"You could also achieve the same in one statement and with inline
"declaration.
DATA(type_descr_obj_table_2) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( str_table ) ).

"Example for an impossible downcast
"The generic object reference variable points to cl_abap_elemdescr after the following
"assignment.
type_descr_obj_gen = cl_abap_typedescr=>describe_by_data( str ).

"Without catching the exception, the runtime error MOVE_CAST_ERROR
"occurs. There is no syntax error at compile time. The static type of
"type_descr_obj_gen is more generic than the static type of the target variable.
"The error occurs when trying to downcast, and the dynamic type is used.
TRY.
    type_descr_obj_table = CAST #( type_descr_obj_gen ).
  CATCH cx_sy_move_cast_error.
ENDTRY.
"Note: type_descr_obj_table sill points to the reference as assigned above after trying
"to downcast in the TRY control structure.

"Using CASE TYPE OF and IS INSTANCE OF statements, you can check if downcasts
"are possible.
"Note: In case of ...
"- non-initial object reference variables, the dynamic type is checked.
"- initial object reference variables, the static type is checked.

"------------ IS INSTANCE OF ------------
DATA some_type_descr_obj TYPE REF TO cl_abap_typedescr.
some_type_descr_obj = cl_abap_typedescr=>describe_by_data( str_table ).

IF some_type_descr_obj IS INSTANCE OF cl_abap_elemdescr.
  DATA(type_descr_obj_a) = CAST cl_abap_elemdescr( some_type_descr_obj ).
ELSE.
  "This branch is executed. The downcast is not possible.
  ...
ENDIF.

IF some_type_descr_obj IS INSTANCE OF cl_abap_elemdescr.
  DATA(type_descr_obj_b) = CAST cl_abap_elemdescr( some_type_descr_obj ).
ELSEIF some_type_descr_obj IS INSTANCE OF cl_abap_refdescr.
  DATA(type_descr_obj_c) = CAST cl_abap_refdescr( some_type_descr_obj ).
ELSEIF some_type_descr_obj IS INSTANCE OF cl_abap_structdescr.
  DATA(type_descr_obj_d) = CAST cl_abap_structdescr( some_type_descr_obj ).
ELSEIF some_type_descr_obj IS INSTANCE OF cl_abap_tabledescr.
  "In this example, this branch is executed. With the check,
  "you can make sure that the downcast is indeed possible.
  DATA(type_descr_obj_e) = CAST cl_abap_tabledescr( some_type_descr_obj ).
ELSE.
  ...
ENDIF.

DATA initial_type_descr_obj TYPE REF TO cl_abap_typedescr.

IF initial_type_descr_obj IS INSTANCE OF cl_abap_elemdescr.
  DATA(type_descr_obj_f) = CAST cl_abap_elemdescr( some_type_descr_obj ).
ELSEIF initial_type_descr_obj IS INSTANCE OF cl_abap_refdescr.
  DATA(type_descr_obj_g) = CAST cl_abap_refdescr( some_type_descr_obj ).
ELSEIF initial_type_descr_obj IS INSTANCE OF cl_abap_structdescr.
  DATA(type_descr_obj_h) = CAST cl_abap_structdescr( some_type_descr_obj ).
ELSEIF initial_type_descr_obj IS INSTANCE OF cl_abap_tabledescr.
  DATA(type_descr_obj_i) = CAST cl_abap_tabledescr( some_type_descr_obj ).
ELSE.
  "In this example, this branch is executed. The static
  "type of the initial object reference variable is used,
  "which is cl_abap_typedescr here.
  ...
ENDIF.

"------------ CASE TYPE OF ------------
"The examples are desinged similarly to the IS INSTANCE OF examples.

DATA(dref) = REF #( str_table ).
some_type_descr_obj = cl_abap_typedescr=>describe_by_data( dref ).

CASE TYPE OF some_type_descr_obj.
  WHEN TYPE cl_abap_elemdescr.
    DATA(type_descr_obj_j) = CAST cl_abap_elemdescr( some_type_descr_obj ).
  WHEN TYPE cl_abap_refdescr.
    "In this example, this branch is executed. With the check,
    "you can make sure that the downcast is indeed possible.
    DATA(type_descr_obj_k) = CAST cl_abap_refdescr( some_type_descr_obj ).
  WHEN TYPE cl_abap_structdescr.
    DATA(type_descr_obj_l) = CAST cl_abap_structdescr( some_type_descr_obj ).
  WHEN TYPE cl_abap_tabledescr.
    DATA(type_descr_obj_m) = CAST cl_abap_tabledescr( some_type_descr_obj ).
  WHEN OTHERS.
    ...
ENDCASE.

"Example with initial object reference variable
CASE TYPE OF initial_type_descr_obj.
  WHEN TYPE cl_abap_elemdescr.
    DATA(type_descr_obj_n) = CAST cl_abap_elemdescr( some_type_descr_obj ).
  WHEN TYPE cl_abap_refdescr.
    DATA(type_descr_obj_o) = CAST cl_abap_refdescr( some_type_descr_obj ).
  WHEN TYPE cl_abap_structdescr.
    DATA(type_descr_obj_p) = CAST cl_abap_structdescr( some_type_descr_obj ).
  WHEN TYPE cl_abap_tabledescr.
    DATA(type_descr_obj_q) = CAST cl_abap_tabledescr( some_type_descr_obj ).
  WHEN OTHERS.
    "In this example, this branch is executed. The static
    "type of the initial object reference variable is used,
    "which is cl_abap_typedescr here.
    ...
ENDCASE.
```


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Interfaces

Interfaces ...

-   represent a template for the components in the public visibility
    section of classes.
- enhance classes by adding interface components.
-   are possible as both
    [local](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlocal_interface_glosry.htm "Glossary Entry")
    and [global
    interfaces](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_interface_glosry.htm "Glossary Entry").
-   support
    [polymorphism](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpolymorphism_glosry.htm "Glossary Entry") in classes. Each class that implements an interface can implement its methods differently. [Interface reference variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninterface_ref_variable_glosry.htm "Glossary Entry") can point to objects of all classes that implement the associated interface.
- can be implemented by classes of an inheritance tree. It can be any number of interfaces. However, each interface can be implemented only once in an inheritance tree.
-   are different from classes in the following ways:
    -   They only consist of a part declaring the components without an
        implementation part. The implementation is done in classes that use the interface.
    -   There are no visibility sections. All components of an interface are public.
    -   No instances can be created from interfaces.
    -   Declarations as mentioned for classes, e. g. `DATA`,
        `CLASS-DATA`, `METHODS`,
        `CLASS-METHODS`, are possible. Constructors are not
        possible.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Defining Interfaces

- You can define global and local interfaces. 
- Like global classes, global interfaces are defined using the `PUBLIC` addition. 


``` abap
"The addition PUBLIC is for global interfaces
INTERFACE intf_global PUBLIC.
"Local interface
"INTERFACE intf_local.
        
    DATA ...
    CLASS-DATA ...
    METHODS ...
    CLASS-METHODS ...
    CONSTANTS ...
    TYPES ...
    

ENDINTERFACE.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Implementing Interfaces 

-   A class can implement multiple interfaces.
-   Interfaces must be specified in the declaration part of a class using the statement
    [`INTERFACES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinterfaces.htm).
-   Since all interface components are public, you must include this statement and the interfaces in the public visibility section of a class. When an interface is implemented in a class, all interface components are added to the other components of the class in the public visibility section.
- Classes must implement the methods of all implemented interfaces in them unless ...
  - methods are flagged as abstract or final (see next section). 
  - methods mark their implementation as optional using the additions [`DEFAULT IGNORE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_default.htm) or [`DEFAULT FAIL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_default.htm) (see next section).
- Interface components can be addressed using the [interface component selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninterface_comp_selector_glosry.htm "Glossary Entry"): `... intf~comp ...`.
- You can also include other interfaces in interfaces.

```abap
"Local interface in a CCIMP include
INTERFACE lif.

  DATA str TYPE string.
  CLASS-DATA number TYPE i.
  CONSTANTS const_number TYPE i VALUE 1.
  "All of the following methods must be implemented in classes
  "that implement the interface.
  METHODS inst_meth IMPORTING num TYPE i.
  CLASS-METHODS stat_meth RETURNING VALUE(result) TYPE string.

ENDINTERFACE.

"Local class in a CCIMP include implementing the interface
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    "Multiple interface implementations are possible
    INTERFACES lif.
ENDCLASS.

CLASS lcl IMPLEMENTATION.

  METHOD lif~inst_meth.
    ...
    "Assuming lif~number has been assigned a value somewhere.
    DATA(addition) = num + lif~number.
    ...
  ENDMETHOD.

  METHOD lif~stat_meth.
    ...
  ENDMETHOD.

ENDCLASS.

**********************************************************************
"Including interfaces in other interfaces

INTERFACE lif2.
  METHODS meth_of_lif2.
  INTERFACES lif.
ENDINTERFACE.

CLASS lcl2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif2.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD lif2~meth_of_lif2.
    ...
  ENDMETHOD.

  METHOD lif~inst_meth.
    ...
  ENDMETHOD.

  METHOD lif~stat_meth.
    ...
  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Additions Related to Interface Implementations

> **💡 Note**<br>
> The code examples show local classes and interfaces declared, for example, in the [CCIMP include](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenccimp_glosry.htm) of a class pool. 

<table>

<tr>
<td> Addition </td> <td> Notes </td>
</tr>

<tr>
<td> 

[`ALIASES ... FOR ...`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapaliases.htm)

 </td>

 <td> 

Specifies alias names for the interface components. The components can then be addressed using the alias name.

<br>

```abap
"Local interface in a CCIMP include
INTERFACE lif.
  METHODS some_method.
  DATA some_string type string.
ENDINTERFACE.

"Local class in a CCIMP include implementing the interface
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
    ALIASES meth FOR lif~some_method.
    ALIASES str FOR lif~some_string.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD meth.
    "The following syntax is also possible: METHOD lif~some_method.
    ...
    DATA(string1) = str.
    "The following sytanx is also possible. However, when you have already addressed the component
    "with the alias as in the assignment above, the following statement shows a syntax warning.
    "DATA(string2) = lif~some_string.
  ENDMETHOD.
ENDCLASS.
```

 </td>
</tr>

<tr>
<td> 

[`ABSTRACT METHODS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinterfaces_class.htm)

 </td>

 <td> 

- Specifies instance methods as abstract.
- Multiple methods can be specified. 
- The addition can only be used when the whole class is defined as abstract.

<br>

``` abap
INTERFACE lif.
  METHODS meth1.
  METHODS meth2.
ENDINTERFACE.

"Local abstract class 
"meth1 is specified as abstract method, meth2 is not.
"Therefore, only meth2 must be implemented. meth1 must
"be implemented by the subclasses.
CLASS lcl1 DEFINITION ABSTRACT.
  PUBLIC SECTION.    
    INTERFACES lif ABSTRACT METHODS meth1.    
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.  
  METHOD lif~meth2.
    ...
  ENDMETHOD.
ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS lif~meth1 REDEFINITION. 
    METHODS lif~meth2 REDEFINITION.       
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD lif~meth1.
    ...
  ENDMETHOD.
  
  METHOD lif~meth2.
    ...
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

[`ALL METHODS ABSTRACT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinterfaces_class.htm)

 </td>

 <td> 

See above. With this addition, all methods are specified as abstract.

<br>

``` abap
INTERFACE lif.
  METHODS meth1.
  METHODS meth2.
ENDINTERFACE.

"Local abstract class
"All methods are specified as abstract methods.
"Therefore, all methods of the interface must be implemented
"by the subclasses.
CLASS lcl1 DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif ALL METHODS ABSTRACT.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.

ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    METHODS lif~meth1 REDEFINITION.
    METHODS lif~meth2 REDEFINITION.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD lif~meth1.
    ...
  ENDMETHOD.

  METHOD lif~meth2.
    ...
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

[`FINAL METHODS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinterfaces_class.htm)

 </td>

 <td> 

Specifies methods as final so they cannot be further redefined. Multiple methods can be specified.

<br>

```abap
INTERFACE lif.
  METHODS meth1.
  METHODS meth2.
ENDINTERFACE.

"meth1 is specified as final method, meth2 is not.
"Therefore, only meth2 can be further redefined in subclasses.
CLASS lcl1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif FINAL METHODS meth1.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.

  METHOD lif~meth1.
    ...
  ENDMETHOD.

  METHOD lif~meth2.
    ...
  ENDMETHOD.

ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    "meth1 cannot be redefined as it is declared as final in the superclass.
    "METHODS lif~meth1 REDEFINITION.
    METHODS lif~meth2 REDEFINITION.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.

  METHOD lif~meth2.
    ...
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

[`ALL METHODS FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinterfaces_class.htm)

 </td>

 <td> 

See above. With this addition, all methods are specified as final.

<br>

``` abap
INTERFACE lif.
  METHODS meth1.
  METHODS meth2.
ENDINTERFACE.

"All methods are specified as final. Therefore, they
"cannot be further redefined in subclasses.
CLASS lcl1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif ALL METHODS FINAL.
    METHODS meth3.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.

  METHOD lif~meth1.
    ...
  ENDMETHOD.

  METHOD lif~meth2.
    ...
  ENDMETHOD.

  METHOD meth3.
    ...
  ENDMETHOD.

ENDCLASS.

CLASS lcl2 DEFINITION INHERITING FROM lcl1.
  PUBLIC SECTION.
    "meth1 and meth2 cannot be redefined.
    "METHODS lif~meth1 REDEFINITION.
    "METHODS lif~meth2 REDEFINITION.
    METHODS meth3 REDEFINITION.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.

  METHOD meth3.
    ...
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

[`DATA VALUES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPINTERFACES_CLASS.html)

 </td>

 <td> 

- Used to assign start values for attributes
- Works in the style of `DATA ... VALUE ...` statements, e.g. `DATA number TYPE i VALUE 123`.

<br>

``` abap
INTERFACE lif.
  METHODS meth1 RETURNING VALUE(result) TYPE i.
  DATA num1 TYPE i.
  DATA num2 TYPE i.
  CLASS-DATA num3 TYPE i.
ENDINTERFACE.

CLASS lcl1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif DATA VALUES num1 = 1 num2 = 3 num3 = 6.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.

  METHOD lif~meth1.
    result = lif~num1 + lif~num2 + lif~num3.
  ENDMETHOD.

ENDCLASS.

CLASS lcl3 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS meth.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.

  METHOD meth.
    "result: 10
    DATA(result) = NEW lcl1( )->lif~meth1( ).    
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

[`PARTIALLY IMPLEMENTED`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPINTERFACES_PARTIALLY.html)

 </td>

 <td> 

- Relevant only for test classes.
- When you use the `PARTIALLY IMPLEMENTED` addition in test classes, you are not forced to implement all of the concrete non-optional methods. 
- It is particularly useful for interfaces to implement test doubles, and not all methods are necessary.
- See the [ABAP Unit Tests](14_ABAP_Unit_Tests.md) cheat sheet.

<br>

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

 </td>
</tr>



<tr>
<td> 

[`DEFAULT IGNORE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_default.htm) 

<br> (addition used in the interface definition)

 </td>

 <td> 

- Marks the implementation of methods as optional
- Defines a default behavior when non-implemented methods are called. When a method with such a declaration is called without an implementation, it behaves as though no implementation exists.
- Can only be used in interfaces for instance and static methods (except constructors and test methods)

<br>

``` abap
INTERFACE lif.
  METHODS meth1 DEFAULT IGNORE.
  METHODS meth2.
ENDINTERFACE.

CLASS lcl1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

"The class implementation does not include the optional
"implementation of lif~meth1.
CLASS lcl1 IMPLEMENTATION.

  METHOD lif~meth2.
    ...
  ENDMETHOD.

ENDCLASS.

CLASS lcl2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

"The class implementation includes the optional
"implementation of lif~meth1.
CLASS lcl2 IMPLEMENTATION.

  METHOD lif~meth1.
    ...
  ENDMETHOD.

  METHOD lif~meth2.
    ...
  ENDMETHOD.

ENDCLASS.

CLASS lcl3 DEFINITION.
  PUBLIC SECTION.
    class-methods meth3.
ENDCLASS.

"The class implementation includes the optional
"implementation of lif~meth1.
CLASS lcl3 IMPLEMENTATION.

  METHOD meth3.
    DATA(oref1) = NEW lcl1( ).
    DATA(oref2) = NEW lcl2( ).
    
    "Although not implemented, meth1 can be specified to be called. 
    "In this case, it is just like calling a method with empty implementation.
    oref1->lif~meth1( ).
    oref1->lif~meth2( ).
    
    "In this class, both methods are implemented.
    oref2->lif~meth1( ).
    oref2->lif~meth2( ).
        
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

[`DEFAULT FAIL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_default.htm)

<br> (addition used in the interface definition)

 </td>

 <td> 

See above. The behavior with this addition is that when an unimplemented method is called, the `CX_SY_DYN_CALL_ILLEGAL_METHOD` exception is raised.

<br>

``` abap
INTERFACE lif.
  METHODS meth1 DEFAULT FAIL.
  METHODS meth2.
ENDINTERFACE.

CLASS lcl1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

"The class implementation does not include the optional
"implementation of lif~meth1.
CLASS lcl1 IMPLEMENTATION.

  METHOD lif~meth2.
    ...
  ENDMETHOD.

ENDCLASS.

CLASS lcl2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

"The class implementation includes the optional
"implementation of lif~meth1.
CLASS lcl2 IMPLEMENTATION.

  METHOD lif~meth1.
    ...
  ENDMETHOD.

  METHOD lif~meth2.
    ...
  ENDMETHOD.

ENDCLASS.

CLASS lcl3 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS meth3.
ENDCLASS.

CLASS lcl3 IMPLEMENTATION.

  METHOD meth3.
    DATA(oref1) = NEW lcl1( ).
    DATA(oref2) = NEW lcl2( ).

    "Although not implemented, meth1 can be specified to be called.
    "However, with the DEFAULT FAIL addition, an exception is
    "raised.
    TRY.
        oref1->lif~meth1( ).
      CATCH cx_sy_dyn_call_illegal_method INTO DATA(error).
        DATA(error_text) = error->get_text( ).
    ENDTRY.
    oref1->lif~meth2( ).

    "In this class, both methods are implemented.
    oref2->lif~meth1( ).
    oref2->lif~meth2( ).

  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>


</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Interface Reference Variables, Accessing Objects and Components

- Addressing an object happens via an object reference variable with reference to a class.
- An interface variable can contain references to objects of classes that implement the corresponding interface.
- You create an interface reference variable like this: `DATA i_ref TYPE REF TO intf.`

Addressing interface components:
- Addressing instance components using interface reference variable
    - attribute: `i_ref->attr`
    - instance method: `i_ref->meth( )`
- Addressing instance components using an object reference variable (Note: The type is a class that implements the interface) is also possible but it's not the recommended way:
    - attribute: `cl_ref->intf~attr`
    - instance method: `cl_ref->intf~meth`
- Addressing static components:
    - static attribute: `class=>intf~attr`,
    - static method: `class=>intf~meth( )`
    - constant: `intf=>const`


``` abap
"----------------------- Syntax patterns -----------------------
"Addressing instance interface components using interface reference variable
DATA i_ref TYPE REF TO intf.

DATA cl_ref TYPE REF TO class.

"Creating an instance of a class that implements the interface intf
cl_ref = NEW #( ).

"If the class class implements an interface intf,
"the class reference variable cl_ref can be assigned
"to the interface reference variable i_ref.
"The reference in i_ref then points to the same object
"as the reference in cl_ref.
i_ref = cl_ref.

"Can also be done directly, i. e. directly creating an object to which the interface reference variable points
i_ref = NEW class( ).

"Instance interface method via interface reference variable
... i_ref->inst_method( ... ) ...

"Instance interface attribute via interface reference variable
... i_ref->inst_attr ...

"Addressing instance components using the class reference variable
"is also possible but it's not the recommended way.
... cl_ref->intf~inst_method( ... ) ...
... cl_ref->intf~inst_attr ...

"Addressing static interface components
"class=> can be dropped if the method is called in the same class that implements the interface
... class=>intf~stat_method( ... ) ...
... class=>intf~stat_attr ...

"Just for the record: Static interface components can be called via reference variables, too.
... i_ref->stat_method( ... ) ...
... i_ref->stat_attr ...
... cl_ref->intf~stat_method( ... ) ...

"Constants
"A constant can be addressed using the options mentioned above.
"Plus, it can be addressed using the following pattern
... intf=>const ...
```

Example using local interfaces and classes

```abap
INTERFACE lif.
  METHODS inst_meth.
  CLASS-METHODS stat_meth.
  TYPES c1 TYPE c LENGTH 1.
  DATA inst_num TYPE i.
  CLASS-DATA stat_str TYPE string.
  CONSTANTS const TYPE string VALUE `ABAP`.
ENDINTERFACE.

CLASS lcl1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl1 IMPLEMENTATION.

  METHOD lif~inst_meth.
    ...
  ENDMETHOD.

  METHOD lif~stat_meth.
    ...
  ENDMETHOD.

ENDCLASS.

"This class demonstrates addressing interface components
CLASS lcl2 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS meth.
    DATA flag TYPE lif=>c1.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.

  METHOD meth.

    "Addressing instance interface components using interface reference variable
    "Interface reference variable
    DATA i_ref TYPE REF TO lif.
    "Object reference variable
    DATA cl_ref TYPE REF TO lcl1.

    "Creating an instance of a class that implements the interface lif
    cl_ref = NEW #( ).

    "If the class lcl1 implements the interface lif,
    "the class reference variable cl_ref can be assigned
    "to the interface reference variable i_ref.
    "The reference in i_ref then points to the same object
    "as the reference in cl_ref.
    i_ref = cl_ref.

    "This can also be done directly, i. e. directly creating an object to
    "which the interface reference variable points
    DATA i_ref2 TYPE REF TO lif.
    i_ref2 = NEW lcl1( ).

    "Instance interface method via interface reference variable
    i_ref->inst_meth( ).

    "Instance interface attribute via interface reference variable
    DATA(a) = i_ref->inst_num.

    "Addressing instance components using the class reference variable
    "is also possible but it is not the recommended way.
    cl_ref->lif~inst_meth( ).
    DATA(b) = cl_ref->lif~inst_num.

    "Addressing static interface components
    "The class name and => can be dropped if the method is called in the
    "same class that implements the interface.
    lcl1=>lif~stat_meth( ).
    DATA(c) = lcl1=>lif~stat_str.

    "Note: Static interface components can be called via reference variables, too.
    i_ref->stat_meth( ).
    DATA(d) = i_ref->stat_str.
    cl_ref->lif~stat_meth( ).

    "Constants
    "A constant can be addressed using the options mentioned above.
    "Plus, it can be addressed using the pattern intf=>...
    DATA(e) = lif=>const.
    DATA(f) = i_ref->const.
    DATA(g) = cl_ref->lif~const.

    "Types
    DATA h TYPE lif=>c1.
    "Referring to attributes in the interface
    DATA i LIKE lif=>const.
  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Excursion: Example Interface

Expand the following collapsible section for example code. To try it out, create a demo interface named `zif_some_interface`, a class named `zcl_demo_abap`, and paste the code into the artifacts. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.

<details>
   <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->
<br>

Example interface:
```abap
INTERFACE zif_some_interface
  PUBLIC .

  TYPES c3 type c length 3.
  DATA add_result TYPE i.
  CLASS-DATA: subtr_result TYPE i.
  METHODS addition IMPORTING num1          TYPE i
                             num2          TYPE i.
  CLASS-METHODS subtraction IMPORTING num1          TYPE i
                                      num2          TYPE i.

  METHODS meth_ignore DEFAULT IGNORE returning value(int) type i.
  METHODS meth_fail DEFAULT FAIL returning value(int) type i.

ENDINTERFACE.
```

When you have activated the interface, create the class. The following steps comment on various things.

Example class implementations:

1) 
- When you create the class, you can add `INTERFACES zif_some_interface.` to the public visibility section.
- Using the quick fix in ADT, you can automatically add the method implementation skeletons of the interface methods.
- Note that the interface methods declared with `DEFAULT IGNORE` and `DEFAULT FAIL` are not automatically added. If implementations are desired, you can manually add the implementations for these methods. See the following step.
- The example class also implements the interface `if_oo_adt_classrun`.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES zif_some_interface.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
  ENDMETHOD.

  METHOD zif_some_interface~addition.
  ENDMETHOD.

  METHOD zif_some_interface~subtraction.
  ENDMETHOD.

ENDCLASS.
```

2) 
- Adding the implementations for the interface methods declared with `DEFAULT IGNORE` and `DEFAULT FAIL`.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES zif_some_interface.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
  ENDMETHOD.

  METHOD zif_some_interface~addition.
  ENDMETHOD.

  METHOD zif_some_interface~subtraction.
  ENDMETHOD.

  METHOD zif_some_interface~meth_fail.
  ENDMETHOD.

  METHOD zif_some_interface~meth_ignore.
  ENDMETHOD.

ENDCLASS.
``` 

3) 
- The following example class represents an executable example that displays output in the ADT console. 
- Apart from simple demo implementations, it includes alias names specified for interface components.
- The interface methods declared with `DEFAULT IGNORE` and `DEFAULT FAIL` are intentionally not implemented, but the methods are nevertheless called. 
   - Interface method declared with `DEFAULT IGNORE`: No implementation available, and no value is assigned for the returning parameter. Therefore, the value is initial. 
   - Interface method declared with `DEFAULT FAIL`: If the exception is not caught, a runtime error occurs.
- The example demonstrates method calls using object and interface reference variables.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES zif_some_interface.
    ALIASES res FOR zif_some_interface~add_result.
    ALIASES add FOR zif_some_interface~addition.
    ALIASES subtr FOR zif_some_interface~subtraction.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "Examples using an object reference variable
    DATA(oref) = NEW zcl_demo_abap( ).

    oref->add( num1 = 1 num2 = 2 ).
    DATA(res1) = oref->res.
    out->write( res1 ).

    oref->subtr( num1 = 1 num2 = 2 ).
    DATA(res2) = oref->zif_some_interface~subtr_result.
    out->write( res2 ).

    "Referring to a type declared in the interface
    DATA char_a TYPE zif_some_interface~c3.
    DATA char_b TYPE zif_some_interface=>c3.

    "Calling non-implemented methods
    DATA(int_ig_a) = oref->zif_some_interface~meth_ignore( ).
    ASSERT int_ig_a = 0.

    TRY.
        DATA(int_fl_a) = oref->zif_some_interface~meth_fail( ).
      CATCH cx_sy_dyn_call_illegal_method INTO DATA(error).
        out->write( error->get_text( ) ).
    ENDTRY.

    "Similar examples using an interface reference variable
    DATA iref TYPE REF TO zif_some_interface.
    iref = NEW zcl_demo_abap( ).

    iref->addition( num1 = 3 num2 = 5 ).
    DATA(res3) = iref->add_result.
    out->write( res3 ).

    iref->subtraction( num1 = 3 num2 = 5 ).
    DATA(res4) = iref->subtr_result.
    out->write( res4 ).

    "Referring to a type declared in the interface
    DATA char_c TYPE iref->c3.

    "Calling non-implemented methods
    DATA(int_ig_b) = iref->meth_ignore( ).
    ASSERT int_ig_b = 0.

    TRY.
        DATA(int_fl_b) = iref->meth_fail( ).
      CATCH cx_sy_dyn_call_illegal_method INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD add.
    res = num1 + num2.
  ENDMETHOD.

  METHOD subtr.
    zif_some_interface~subtr_result = num1 - num2.
  ENDMETHOD.
ENDCLASS.
```


</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Friendship

- The concept of friendship enters the picture if your use case for your classes is to work together very closely. This is true, for example, for [unit
tests](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenunit_test_glosry.htm "Glossary Entry") if you want to test private methods.
- Classes can grant access to invisible components for their [friends](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfriend_glosry.htm "Glossary Entry").
- The friends can be other classes and interfaces. In case of interfaces, friendship is granted to all classes that implement the interface.
- Impact of friendship:
    - Access is granted to all components, regardless of the visibility section or the addition `READ-ONLY`.
    - Friends of a class can create instances of the class without restrictions.
    - Friendship is a one-way street, i. e. a class granting friendship to another class is not granted friendship the other way round. If class `a` grants friendship to class `b`, class `b` must also explicitly grant friendship to class `a` so that `a` can access the invisible components of class `b`.
    - Friendship and inheritance: Heirs of friends and interfaces that contain a friend as a component interface also become friends. However, granting friendship is not inherited, i. e. a friend of a superclass is not automatically a friend of its subclasses.
- Additions in the context of granting friendship:
  - `FRIENDS`: For local classes, e.g. local classes granting friendship to other local classes or the global class of the class pool
  - `GLOBAL FRIENDS`: Used in global classes to grant friendship to other global classes and interfaces
  - `LOCAL FRIENDS`: Used for global classes to grant friendship to local classes and interfaces in its own class pool; however, it is a dedicated statement, as shown in the example below (the declaration `CLASS zcl_demo_abap DEFINITION LOCAL FRIENDS local_class.` in the CCDEF include) 

You specify the befriended class in the definition part using a `FRIENDS` addition:
``` abap
"For local classes. Friendship can be granted to all classes/interfaces
"of the same program and the class library.
"Multiple classes can be specified as friends.
CLASS lo_class DEFINITION FRIENDS other_class ... .
...

CLASS lo_class DEFINITION CREATE PRIVATE FRIENDS other_class ... .

"Addition GLOBAL only allowed for global classes, i. e. if the addition PUBLIC is also used
"Other global classes and interfaces from the class library can be specified after GLOBAL FRIENDS.
CLASS global_class DEFINITION CREATE PUBLIC FRIENDS other_global_class ... .
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Friendship between Global and Local Classes

Expand the following collapsible section for an example class. It demonstrates granting friendship between a global class and a local class (in the CCIMP include, *Local Types* tab in ADT). In the example, friendship is granted in both ways so that the global class can access private components of the local class, and the local class can access private components of the global class.
For more information, see the following topics: 
- [`LOCAL FRIENDS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_local_friends.htm) 
- [`DEFERRED`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_deferred.htm)

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

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

- Create a new global class (the example uses the name `zcl_demo_abap`) and copy and paste the following code in the *Global Class* tab in ADT.
- The class has a type and method declaration in the private section. They are used in the local class.
- Once activated (and the code of the other includes has been inserted), you can choose *F9* in ADT to run the class.
- When running the class, a method of the local class that is declared in the private section there is called. As a result of this method call, a string is assigned to an attribute that is also declared in the private section of the local class. This attribute is accessed by the global class, and finally displayed in the ADT console.

<br>

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES str TYPE string.
    CLASS-METHODS get_hello RETURNING VALUE(hello) TYPE str.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    local_class=>say_hello( ).
    DATA(hello) = local_class=>hello.
    out->write( hello ).
  ENDMETHOD.
  METHOD get_hello.
    hello = `Hello`.
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

- Regarding the includes, see the information in section [Excursion: Class Pool and Include Programs](#excursion-class-pool-and-include-programs)
- The `LOCAL FRIENDS` addition makes the local class a friend of the global class. The private components of the global class can then be accessed by the local class.

<br>

```abap
CLASS local_class DEFINITION DEFERRED.
CLASS zcl_demo_abap DEFINITION LOCAL FRIENDS local_class.
```


 </td>
</tr>

<tr>
<td> 

CCIMP include (Local Types tab in ADT)

 </td>

 <td> 

- The `FRIENDS` addition makes the global class a friend of the local class. The private components of the local class can then be accessed by the global class.
- A type declared in the private section of the global class is used to type an attribute.
- The method, which is also declared in the private section, includes a method call in the implementation. It is a method declared in the private section of the global class.

<br>

```abap
CLASS local_class DEFINITION FRIENDS zcl_demo_abap.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA hello TYPE zcl_demo_abap=>str.
    CLASS-METHODS say_hello.
    
ENDCLASS.

CLASS local_class IMPLEMENTATION.
  METHOD say_hello.
    hello = |{ zcl_demo_abap=>get_hello( ) } { sy-uname }.|.
  ENDMETHOD.
ENDCLASS.
```

 </td>
</tr>

</table>

</details>  


<p align="right"><a href="#top">⬆️ back to top</a></p>


## Excursions

### Events

- [Events](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenevent_glosry.htm "Glossary Entry")
can trigger the processing of [processing blocks](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocessing_block_glosry.htm "Glossary Entry").
- Declaring events: Can be declared in a visibility section of the declaration part of a class or in an interface, e. g. as
  - instance event using an [`EVENTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapevents.htm) statement. Note that they can only be raised in instance methods of the same class.
  - static event using [`CLASS-EVENTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass-events.htm). They can be raised in all methods of the same class or of a class that implements the interface. Static event handlers can be called by the event independently of an instance of the class.

``` abap
"Declaration part of a class/interface
"Instance events
EVENTS: i_evt1,

"Events can only have output parameters that are passed by value
        i_evt2 EXPORTING VALUE(num) TYPE i ...
...
"Static events
CLASS-EVENTS: st_evt1,
              st_evt2 EXPORTING VALUE(num) TYPE i ...

```

- Event handlers:
  - An event is raised by a [`RAISE EVENT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapraise_event.htm) statement in another method or in the same method.
  - Raising an event means that [event handlers](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenevent_handler_glosry.htm "Glossary Entry") are called.
  - This event handler must be declared with the following syntax (see more information and more additions [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_event_handler.htm)):

``` abap
"Event handlers for instance events
METHODS: handler_meth1 FOR EVENT i_evt1 OF some_class,

         "Parameter names must be the same as declared;
         "no further additions possible for the parameter (e.g. TYPE);
         "the predefined, implicit parameter sender as another formal parameter is possible with instance events,
         "it is typed as a reference variable, which itself has the class/interface as a static type,
         "If the event handler is called by an instance event, it is passed a reference to the raising object in sender.
         handler_meth2 FOR EVENT i_evt2 OF some_class IMPORTING num sender,
...
```

- To make sure that an event handler handles a raised event, it must be registered with the statement [`SET HANDLER`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapset_handler.htm). See more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapset_handler.htm) (for example, events can also be deregistered).

```abap
"Registering event for a specific instance
SET HANDLER handler1 FOR ref.

"Registering event for all instances
SET HANDLER handler2 FOR ALL INSTANCES.

"Registering static event for the whole class/interface
SET HANDLER handler3.
"Note that multiple handler methods can be specified.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### ABAP Examples of Design Patterns in Object-Oriented Programming

- This section explores design patterns you may encounter in object-oriented programming, using ABAP classes.
- In object-oriented programming, numerous design patterns enhance modularity, scalability, reusability, and more. 
- Here, a selection of design patterns is covered, using simplified, non-semantic examples to reduce complexity and give a rough idea.

> **💡 Note**<br>
> - The examples neither represent best practices nor role models. They only aim to experiment with the patterns in simplified contexts and convey the basic concepts. 
> - More design patterns exist beyond those covered here, and different implementations or class setup strategies may apply. 
> - Most examples are structured for easy exploration using simple, self-contained ABAP classes (i.e. only 1 class pool including local classes instead of multiple global classes) as follows:
>    - Global class:
>      - Includes the `if_oo_adt_classrun` interface to run the class with F9 in ADT.
>      - Serves as a *vehicle* for demonstrating the design pattern. Only the declarations and implementations in the CCIMP include are relevant for the conceptual considerations.
>    - CCIMP include (Local Types tab in ADT):
>      - Contains various local classes (some also include interfaces) to demonstrate design patterns, allowing quick copying and pasting without creating multiple global classes. 

Expand the following sections for further descriptions and example code. To try the examples, create a demo class named `zcl_demo_abap` and paste the code into it (*Global Class* and *Local Types* tabs in ADT). After activation, choose *F9* in ADT to execute the class. The examples are set up to display output in the console.

<details>
  <summary>🟢 Factory method</summary>
  <!-- -->

<br>

- Used, for example, to:  
  - Provide users with an object of a class instead of them creating the objects themselves.  
  - Control and simplify class instantiation for external users.  
  - Offer a stable API for class users, so they only need to call one stable method. This way, the code may be modified or extended, and the changes to the class do not affect users.  
- Typically, using the `CREATE PRIVATE` addition in a class definition prevents object creation outside the class. A factory method, usually a static method, then supplies users with class objects. You can also include input parameters in the factory method to control instantiation.  
- Example of a predefined ABAP class with factory methods: `CL_ABAP_REGEX`.  


Example notes:

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

<br>

<details>
  <summary>🟢 Singleton</summary>
  <!-- -->

<br>

- Used to restrict external users from instantiating a class.  
- As above, this is typically achieved by using the `CREATE PRIVATE` addition in a class definition, preventing object creation outside the class. A factory method, often a static method, then provides users with an instance of the class. The singleton pattern ensures only one instance per class within an internal session.  
- Example of a predefined ABAP class: `CL_IXML_CORE`.  



Example notes:

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


<br>

<details>
  <summary>🟢 Abstract factory</summary>
  <!-- -->

<br>

- The pattern may be used when you need to create a set of related objects (a family of "products") that work together or are processed together for specific purposes. 
- Such families of related objects may be required in different variants. However, the created objects must be compatible with each variant. 
- Here are some examples to illustrate this:
  - Consider a car manufacturer. Multiple components are needed to assemble a car, such as the chassis, engine, and equipment (certainly, there are more components). These components form a family of related objects to a create car. All cars from the manufacturer follow the same setup: each requires a chassis, engine, and equipment. However, the manufacturer offers various car models like convertibles, sedans, SUVs, and pickup trucks. When producing these different variants, specific components may vary. The assembly must only use compatible objects. For example, when producing a convertible, a chassis with a sedan roof should not be used.
  - Consider a restaurant offers various three-course menus (this is the example used by the demo classes). Menu items include starters, main dishes, and desserts, forming a family of related objects. Different variants exist to create specific menus. For example, the restaurant offers a vegan menu. When creating the vegan menu, it should be ensured the menu does not include a beef steak as a main dish or dairy products in desserts.
- In terms of code, you may need a setup to create related objects in an organized and consistent way. The abstract factory design pattern enables this by using a high level of abstraction in your class setup, i.e. it enables the creation of related objects that belong to the same family through abstractions like abstract classes, without bothering about their specific implementations and allowing for the creation of objects with appropriate types determined at runtime. A factory, such as an abstract factory class, sets up the object creation process by specifying methods that provide objects. Concrete factories then inherit from the abstract factory class and implement the methods to create specific kinds of objects. More abstraction is involved, as outlined in the example description below.
- Some of the benefits of the pattern include: centralizing multiple object creations in one location, ensuring consistency and compatibility, simplifying object creation for users by hiding complexity, adding or modifying different variants without affecting existing code, providing flexibility and adaptability (however, adding new products might be cumbersome as it requires changes in many parts of the code).
- The abstract factory pattern differs from the factory method design pattern in several ways. Both facilitate object creation through abstraction. The factory method uses a single interface as an abstraction layer and single factory methods, while the abstract factory pattern uses a higher degree of abstraction and multiple factories. The factory method pattern primarily creates single objects, whereas the abstract factory pattern involves creating multiple related objects.

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

<br>

<details>
  <summary>🟢 Fluent interface</summary>
  <!-- -->

<br>

- Enables method chaining  
- Achieves method chaining by returning a reference to the current object. There may be variations in the implementation, for example, the returned object may be a modified copy or the modified original.  
- Such a design may consolidate method calls for a simpler, more readable code flow, instead of having individual method calls.

Example notes:

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

<br>

<details>
  <summary>🟢 Adapter</summary>
  <!-- -->

<br>

- Used when APIs are incompatible, and you want to integrate those APIs into an existing one to consolidate all functionality.
- This can be achieved by using an adapter class to convert and transform functionality from a non-compatible API into the existing API.

Example notes:

- This example demonstrates the adapter design pattern with the following declarations and implementations: 
  - Global class:  
    - Implements `if_oo_adt_classrun` and calls methods from local classes.  
    - Acts as a consumer of APIs (local classes) defined in the CCIMP include.  
  - CCIMP include (Local Types tab in ADT):  
    - The example is similar to the factory method example above.
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

<br>

<details>
  <summary>🟢 Builder</summary>
  <!-- -->

<br>

- The idea is to simplify the creation of complex objects, which may involve multiple steps, by hiding this complexity from users.
- Depending on requirements or user input, the class setup should allow for the creation of different objects that share similar or the same creation steps and attributes.
- The builder design pattern may be useful here. Builder classes consolidate the necessary steps to create objects and return specific ones, allowing users to get the objects without taking care of the exact process.
- By using the builder pattern, you can flexibly create different objects that follow the same or similar building processes. Builder classes also centralize code changes, and you can add more builder classes for additional use cases or objects.
- The example tries to demonstrate the builder design pattern with the following class setup: 
  - An abstract builder class defines the steps for object creation and includes methods and attributes shared by concrete builder classes responsible for creating specific objects. The example does not use an interface because the abstract class includes non-abstract methods performing general tasks not requiring object-specific implementations.
  - Multiple concrete builder classes inherit from the abstract superclass. They (can) follow the same object creation flow when redefining the superclass methods, but the implementation may vary among builder classes to serve different purposes for specific objects.
  - Another class orchestrates object creation. This class contains factory methods and acts as an object provider for external users (the global class in the example), simplifying object creation and hiding complexity. Users call a factory method, providing certain input parameters. Based on these values, specific objects are created by dedicated concrete builder classes. The orchestration class ensures methods are called in the correct sequence and returns the final created object.

Example notes:

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

<br>

<details>
  <summary>🟢 Decorator</summary>
  <!-- -->

<br>

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

### Class-Based and Classic Exceptions

- [Catchable exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencatchable_exception_glosry.htm) are represented by exception objects of exception classes.
- Predefined global exception classes exist, custom exception classes can be created. You can also create local exception classes.
- Find an overview in the [Exceptions and Runtime Errors](27_Exceptions.md) cheat sheet.
- See the following code snippet for an ABAP method that specifies the `EXCEPTIONS` addition, which should not be specified anymore for new developments, raising classic, non-class based exceptions. 
  ```abap
  "ABAP class (creates UUIDs) raising a class-based exception
  TRY.
      DATA(uuid) = cl_system_uuid=>create_uuid_x16_static( ).
    CATCH cx_uuid_error.
  ENDTRY.
  
  "ABAP class (provides type information, see the Dynamic Programming cheat sheet)  specified with the EXCEPTIONS addition raising a non-class based exception 
  cl_abap_typedescr=>describe_by_name( EXPORTING p_name = `TYPE_THAT_DOES_NOT_EXIST`
                                       RECEIVING p_descr_ref = DATA(tdo1)
                                       EXCEPTIONS type_not_found = 4 ).

  IF sy-subrc <> 0.
  "Type not found
  ...
  ENDIF.

  cl_abap_typedescr=>describe_by_name( EXPORTING p_name = `ABAP_BOOLEAN`
                                       RECEIVING p_descr_ref = DATA(tdo2)
                                       EXCEPTIONS type_not_found = 4 ).

  ASSERT sy-subrc = 0.
  ```


<p align="right"><a href="#top">⬆️ back to top</a></p>

### ABAP Unit Tests

- [ABAP Unit](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_unit_glosry.htm) is a test tool integrated into the ABAP runtime framework. 
- It can be used to run individual or mass tests, and to evaluate test results. 
- In ABAP programs, individual unit tests are implemented as [test methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentest_method_glosry.htm) of local [test classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentest_class_glosry.htm). 
- Find information on ABAP Unit tests in the [ABAP Unit Tests](14_ABAP_Unit_Tests.md) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>


### ABAP Doc Comments

- The ABAP Doc documentation tool allows you to add special ABAP Doc comments to ABAP source code for documentation.  
- ABAP Doc comments consist of one or more lines starting with `"!`.  
- You can place these comments before declarations (e.g., in classes and methods) to document functionality, add notes, and so on.  
- In ADT, click, for example, on class names or methods to display ABAP Doc comments (if available) in the *ABAP Element Info* tab, or choose F2 to display the information.
- Find more information on ABAP Doc [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENDOCCOMMENT.html).
- The following example demonstrates ABAP Doc comments, showing various commenting options, including:  
  - Using HTML tags for formatting. Only a selected set of HTML tags is supported. You can choose *CTRL + Space* in the ABAP Doc comment for input help.
  - Special tagging and linking options. Refer to the [documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENDOCCOMMENT.html) for more details.
  - Special method signature specifications like `@parameter | ...` for parameters and `@raising | ...` for declared exceptions.


```abap
"! <p class="shorttext">Demo ABAP Doc comments</p>
"!
"! <p>This class serves as an example to illustrate comments for ABAP Doc.
"! The comments begin with the string <strong>"!</strong>, a special form of regular comments introduced by <strong>"</strong>. <br/><br/>
"! The {@link zcl_demo_abap.METH:calculate} method of the example class performs a calculation. </p>
"! <h2>Notes</h2>
"! <ul>
"! <li>ABAP Doc documents declarations in ABAP programs.</li>
"! <li>The ABAP development tools for Eclipse (ADT) support ABAP Doc.</li>
"! <li>The content of ABAP Doc comments is converted to HTML and displayed appropriately.</li>
"! <li>Check out the supported HTML tags by choosing <em>CTRL + Space</em> after <em>"!</em>. h1 - h3 tags can also be used.</li>
"! <li>Escaping special characters:  <strong>&quot;, &apos;, &lt;, &gt;, &#64;, &#123;, &#124;, and &#125;</strong></li>
"! </ul>
"! <h2>Steps</h2>
"! <ol>
"! <li>Create a demo class named <em>zcl_demo_abap</em></li>
"! <li>Copy and paste the code of this example and activate.</li>
"! <li>Click the class name to display the comments in the <em>ABAP Element Info</em> ADT tab.</li>
"! <li>You can also choose F2 for the class name to display the information.</li>
"! </ol>
"! <h3>More examples</h3>
"! Find more code examples in ABAP cheat sheet demo classes, such as {@link zcl_demo_abap_objects} and others. <br/>
"! <h3>Link examples</h3>
"! <ul>
"! <li>Repository objects such as the following, and more:
"! <ul><li>Classes, e.g. {@link zcl_demo_abap}</li>
"! <li>Interfaces, e.g. {@link if_oo_adt_classrun}</li>
"! <li>CDS artifacts, e.g. {@link i_apisforclouddevelopment}</li>
"! <li>DDIC database tables, e.g. {@link zdemo_abap_carr}</li>
"! <li>DDIC data elements, e.g. {@link land1}</li></ul>
"! <li>Method: {@link zcl_demo_abap.METH:calculate}</li>
"! <li>Constant: {@link zcl_demo_abap.DATA:const}</li>
"! <li>Data object: {@link zcl_demo_abap.DATA:dobj}</li>
"! <li>Method parameter: {@link zcl_demo_abap.METH:calculate.DATA:operator}</li>
"! <li>Interface implemented in a class: {@link zcl_demo_abap.INTF:if_oo_adt_classrun}</li>
"! <li>Interface method implemented in a class: {@link zcl_demo_abap.INTF:if_oo_adt_classrun.METH:main}</li>
"! <li>DDIC domain: {@link DOMA:land1}</li>
"! <li>XSLT: {@link XSLT:id}</li>
"! </ul>
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: basetype TYPE c LENGTH 1,
           BEGIN OF ENUM t_enum_calc STRUCTURE en_calc BASE TYPE basetype,
             init     VALUE IS INITIAL,
             plus     VALUE '+',
             minus    VALUE '-',
             multiply VALUE '*',
             divide   VALUE '/',
           END OF ENUM t_enum_calc STRUCTURE en_calc.

    "! <p>This method performs a calculation.</p>
    "! @parameter num1 | First number for the calcation
    "! @parameter num2 | Second number for the calcation
    "! @parameter operator | Operator. Only one of the four operators +, -, *, / is allowed.
    "! The value is represented by an enumerated type. Note that there is no handling if
    "! the assigned value is initial.
    "! @parameter result | Result of the calculation
    "! @raising cx_sy_zerodivide | Zero division
    "! @raising cx_sy_arithmetic_overflow | Arithmetic overflow
    CLASS-METHODS calculate IMPORTING num1          TYPE i
                                      num2          TYPE i
                                      operator      TYPE t_enum_calc
                            RETURNING VALUE(result) TYPE decfloat34
                            RAISING   cx_sy_zerodivide cx_sy_arithmetic_overflow.

    "! This is an ABAP Doc comment for a constant
    CONSTANTS const TYPE i VALUE 123.

    "! This is an ABAP Doc comment for a static attribute of a class
    CLASS-DATA dobj TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA(result_plus) = calculate( num1 = 1 num2 = 10 operator = en_calc-plus ).
    DATA(result_minus) = calculate( num1 = 1 num2 = 10 operator = en_calc-minus ).
    DATA(result_multiply) = calculate( num1 = 2 num2 = 5 operator = en_calc-multiply ).
    DATA(result_divide) = calculate( num1 = 10 num2 = 5 operator = en_calc-divide ).

    out->write( data = result_plus name = `result_plus` ).
    out->write( data = result_minus name = `result_minus` ).
    out->write( data = result_multiply name = `result_multiply` ).
    out->write( data = result_divide name = `result_divide` ).

  ENDMETHOD.

  METHOD calculate.

    result = SWITCH #( operator
                       WHEN en_calc-plus THEN num1 + num2
                       WHEN en_calc-minus THEN num1 - num2
                       WHEN en_calc-multiply THEN num1 * num2
                       WHEN en_calc-divide THEN num1 / num2
                       ELSE '0' ).

  ENDMETHOD.
ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Escape Character

- You may encounter [`!` characters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENNAMES_ESCAPING.html) specified before operands, particularly in signatures of [procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm).
- They are used to distinguish the operand's name from ABAP words. 
- When compiling ABAP programs, the specifications with the escape character are not considered as ABAP words. 
- When executing the programs, the escape characters are ignored.

The following nonsensical example shows various specifications with the escape character that emphasize in the program that the operands are not to be confused with ABAP words. These specifications are not mandatory in the example. The example only addresses escape characters you may encounter in (older) ABAP code.

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    DATA num TYPE i.
    CLASS-DATA default TYPE i VALUE 1.
    METHODS meth1 IMPORTING !num TYPE i.
    METHODS !methods IMPORTING !raising   TYPE i OPTIONAL
                               !optional  TYPE i
                               !exporting TYPE i
                     EXPORTING !importing TYPE i
                               !changing  TYPE i
                     CHANGING  !default   TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    meth1( 1 ).

    DATA !exporting TYPE i.
    DATA !changing TYPE i.
    methods(
      EXPORTING
        raising   = 1
        optional  = 5
        exporting = 10
      IMPORTING
        importing = !exporting
        changing  = !changing
     CHANGING
        default   = !default
    ).

  ENDMETHOD.
  METHOD meth1.
    DATA(a) = num.
    DATA(b) = me->num.
    DATA(c) = !num.

    me->num = num.
    me->num = !num.
  ENDMETHOD.

  METHOD methods.

    !importing = !raising + !optional.
    !changing = !exporting.
    !default += 1.

    importing = raising + optional.
    changing = exporting.
    default += 1.

  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


## More Information
You can check the subtopics of

- [ABAP Objects - Overview](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_objects_oview.htm)
- [Programming Guidlines - Object-Oriented Programming (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenobj_oriented_gdl.htm)
  
in the ABAP Keyword Documentation.

## Executable Examples

- [zcl_demo_abap_objects](./src/zcl_demo_abap_objects.clas.abap) 
- [zcl_demo_abap_objects_misc](./src/zcl_demo_abap_objects_misc.clas.abap): Additional syntax examples  
- [zcl_demo_abap_oo_inheritance_1](./src/zcl_demo_abap_oo_inheritance_1.clas.abap): Focuses on inheritance; the inheritance tree consists of 4 example classes 

> **💡 Note**<br>
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)
