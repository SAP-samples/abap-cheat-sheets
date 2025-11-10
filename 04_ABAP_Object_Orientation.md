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
  - [Events](#events)
  - [Excursions](#excursions)
    - [ABAP Examples of Design Patterns in Object-Oriented Programming](#abap-examples-of-design-patterns-in-object-oriented-programming)
    - [Class-Based and Classic Exceptions](#class-based-and-classic-exceptions)
    - [ABAP Unit Tests](#abap-unit-tests)
    - [ABAP Doc Comments](#abap-doc-comments)
    - [Escape Character](#escape-character)
  - [More Information](#more-information)
  - [Executable Examples](#executable-examples)


This ABAP cheat sheet provides an overview on selected syntax options and concepts related to ABAP object orientation.

> [!NOTE] 
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
    <td><ul><li>can be defined within an <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm">ABAP program</a> such as in include programs of global classes (e.g. the <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenccimp_glosry.htm">CCIMP include</a>, <i>Local Types</i> table in ADT) or in executable programs ("reports"; in <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm">Standard ABAP</a> only)</li><li>can only be used in the ABAP program in which the class is defined</li></ul></td>
</tr>
<tr>
    <td><a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_class_glosry.htm">Global
classes</a></td>
    <td><ul><li>are defined as
    <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_type_glosry.htm">global types</a>, i. e. they are visible as a repository object - in contrast to local classes. As a global type, they can be used - as the name implies - globally in other ABAP programs or global classes</li><li>are declared in <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_pool_glosry.htm">class pools</a> that contain a <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenccimp_glosry.htm">CCIMP include</a> and other <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninclude_program_glosry.htm">include programs</a></li></ul> </td>
</tr>
</table>

> [!NOTE] 
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

This section covers a selection of additions to declare classes. They are also covered in other sections below, e.g. [Additions Related to Inheritance and Instantiation](#additions-related-to-inheritance-and-instantiation). Find more information on the additions in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_options.htm). The additions assume dealing with global classes, however, many of the additions are also possible for local classes. 

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

To define [abstract](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabstract_glosry.htm) classes. These classes cannot be instantiated. They can contain both abstract methods and non-abstract methods. Abstract methods can only be implemented in [subclasses](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubclass_glosry.htm) by redefinition. See a simple implementation example [here](#excursion-example-interface).

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

    "The method called has formal parameters using type declarations from the CCDEF include
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

    "Using local class implemented in the CCIMP include
    DATA(hi1) = lcl_demo=>say_hello( ).
    out->write( data = hi1 name = `hi1` ).
    DATA(hi2) = lcl_demo=>say_hello( xco_cp=>sy->user( )->name ).
    out->write( data = hi2 name = `hi2` ).

    "Test include (CCAU)
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
    (`DATA`): Determine the state of objects of a class. The data
    is only valid in the context of an instance. As shown further down,
    instance attributes can only be accessed via an [object reference
    variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm "Glossary Entry") (or [interface reference variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninterface_ref_variable_glosry.htm)).
-   [Static
    attributes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_attribute_glosry.htm "Glossary Entry")
    (`CLASS-DATA`): Their content is independent of instances of
    a class and, thus, valid for all instances.  That means that if you change such a static
    attribute, the change is visible in all instances. As shown further down,
    static attributes can be accessed by both using an object reference variable (however, this access is not recommended) and using the class name without a prior creation of an instance.


> [!NOTE] 
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
    section. Note that you must create an instance of a class first before using instance methods. Called within the same class, an instance is not required.
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


> [!NOTE] 
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
- An [actual parameter](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenactual_parameter_glosry.htm "Glossary Entry") represents the data object whose content is passed to or copied from a formal parameter as an argument when a procedure is called. 
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

> [!NOTE] 
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
                    "Non-generic built-in ABAP types
                    i_a TYPE i
                    i_b TYPE string
                    "ABAP DDIC types
                    i_c TYPE land1           "elementary type
                    i_d TYPE timestampl      "elementary type
                    i_e TYPE zdemo_abap_fli "structured type based on DDIC database table
                    i_f TYPE string_hashed_table "table type
                    "ABAP CDS types (all of the examples are structured types)
                    i_g TYPE zdemo_abap_fli_ve "CDS view entity
                    i_h TYPE zdemo_abap_abstract_ent "CDS abstract entity
                    i_i TYPE zdemo_abap_table_function "CDS table function
                    "Data types declared in public section of a class
                    i_j TYPE zcl_demo_abap_dtype_dobj=>t_pub_text_c30 "elementary type
                    i_k TYPE zcl_demo_abap_amdp=>carr_fli_struc "structured type
                    i_l TYPE zcl_demo_abap_amdp=>carr_fli_tab "table type
                    "Data types declared in an interface
                    i_m TYPE zdemo_abap_get_data_itf=>occ_rate "elementary type
                    i_n TYPE zdemo_abap_get_data_itf=>carr_tab "table type
                    "Local types
                    i_o TYPE c3 "elementary type
                    i_p TYPE der_type "table type (BDEF derived type)
                    "Note: Examples such as the following are not allowed type specifications of formal parameters.
                    "In the following cases, extra (local) type declarations with TYPES are required before the
                    "method declaration to type the formal parameters.
                    "i_no1 TYPE c LENGTH 3
                    "i_no2 TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY
                    "Reference types
                    i_q TYPE REF TO i "Data reference
                    i_r TYPE REF TO zdemo_abap_carr "Data reference
                    i_s TYPE REF TO zcl_demo_abap_unit_test "Object reference
                    i_t TYPE REF TO data "Data reference (considered as complete typing, too)
                    i_u TYPE REF TO object "Object reference (considered as complete typing, too)
                    "TYPE LINE OF addition (structured type based on a table type)
                    i_v TYPE LINE OF zcl_demo_abap_amdp=>carr_fli_tab
                    i_w TYPE LINE OF der_type
                    "LIKE addition (types based on existing data objects)
                    i_x LIKE int "Local data object
                    i_y LIKE zcl_demo_abap_dtype_dobj=>comma "Constant specified in a class
                    i_z LIKE zdemo_abap_objects_interface=>stat_str "Data object specified in an interface
                    "LIKE LINE OF addition (types based on existing internal tables)
                    i_1 LIKE LINE OF itab "Local internal table
                    "LIKE REF TO addition (reference types based on existing data object)
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
                    "Any data type
                    i_data           TYPE data
                    i_any            TYPE any

                    "Character-like types
                    i_c              TYPE c         "Text field with a generic length
                    i_clike          TYPE clike     "Character-like (c, n, string, d, t, and character-like flat structures)
                    i_csequence      TYPE csequence "Text-like (c, string)
                    i_n              TYPE n         "Numeric text with generic length
                    i_x              TYPE x         "Byte field with generic length
                    i_xsequence      TYPE xsequence "Byte-like (x, xstring)

                    "Numeric types
                    i_decfloat       TYPE decfloat "decfloat16 decfloat34
                    i_numeric        TYPE numeric  "Numeric (i, int8, p, decfloat16, decfloat34, f, (b, s))
                    i_p              TYPE p        "Packed number (generic length and number of decimal places)

                    "Internal table types
                    i_any_table      TYPE ANY TABLE      "Internal table with any table type
                    i_hashed_table   TYPE HASHED TABLE
                    i_index_table    TYPE INDEX TABLE
                    i_sorted_table   TYPE SORTED TABLE
                    i_standard_table TYPE STANDARD TABLE
                    i_table          TYPE table          "Standard table

                    "Other types
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

This section covers some aspects of working with reference variables. Find a copyable example demonstrating the aspects in the next section.

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
DATA ref TYPE REF TO some_class.

"Creating a new object
"The example assumes there is a mandatory importing parameter for the 
"instance constructor.
ref = NEW #( 1 ).

"Creating another object using the same object reference variable
"The existing reference is overwritten.
ref = NEW #( 1 ).
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

> [!NOTE] 
> Objects use up space in the memory and should therefore be
cleared if they are no longer needed. However, the [garbage collector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengarbage_collector_glosry.htm "Glossary Entry") is called periodically and automatically by the [ABAP runtime framework](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_runtime_frmwk_glosry.htm "Glossary Entry") and clears all objects without any reference.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Accessing Attributes
- Instance attributes: Accessed using
the [object component selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_component_select_glosry.htm "Glossary Entry")
`->` via a reference variable.
- Static attributes: Accessed (if the attributes are visible) using the [class component
selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_component_select_glosry.htm "Glossary Entry")
`=>` via the class name. Static attributes can but should not be addressed via a reference variable.

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

The following executable example addresses aspects of this section and the previous one:

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS constructor IMPORTING ts TYPE utclong OPTIONAL.
    DATA timestamp TYPE utclong.
    DATA instance_number TYPE i.
    CLASS-DATA static_number TYPE i.
    TYPES chars TYPE c LENGTH 4.
    CONSTANTS text TYPE chars VALUE `ABAP`.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS test_static_method.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA oref TYPE REF TO zcl_demo_abap.
    DATA oref2 TYPE REF TO zcl_demo_abap.

*&---------------------------------------------------------------------*
*& Assigning object reference variables
*&---------------------------------------------------------------------*

    oref = NEW #( utclong_current( ) ).

    "Accessing instance attribute
    DATA(ts1) = oref->timestamp.
    out->write( data = ts1 name = `ts1` ).

    "Assigning reference to existing reference variable
    oref2 = oref.
    DATA(ts2) = oref2->timestamp.
    out->write( data = ts2 name = `ts2` ).

*&---------------------------------------------------------------------*
*& Overwriting a reference variable
*&---------------------------------------------------------------------*

    oref = NEW #( utclong_current( ) ).
    DATA(ts3) = oref->timestamp.
    out->write( data = ts3 name = `ts3` ).

*&---------------------------------------------------------------------*
*& Retaining object references
*&---------------------------------------------------------------------*

    DATA oref_tab TYPE TABLE OF REF TO zcl_demo_abap.

    DO 3 TIMES.
      oref = NEW #( utclong_current( ) ).
      oref->instance_number = sy-index.
      oref->static_number = sy-index.
      oref_tab = VALUE #( BASE oref_tab ( oref ) ).
    ENDDO.

    out->write( data = oref_tab name = `oref_tab` ).

*&---------------------------------------------------------------------*
*& Clearing object references
*&---------------------------------------------------------------------*

    DATA oref_generic TYPE REF TO object.
    oref_generic = NEW zcl_demo_abap( utclong_current( ) ).

    IF oref_generic IS INITIAL.
      out->write( `oref_generic is initial` ).
    ELSE.
      out->write( `oref_generic is not initial` ).
    ENDIF.

    IF oref_generic IS INSTANCE OF zcl_demo_abap.
      out->write( `oref_generic is an instance of zcl_demo_abap` ).
    ELSE.
      out->write( `oref_generic is not an instance of zcl_demo_abap` ).
    ENDIF.

    CLEAR oref_generic.

    IF oref_generic IS INITIAL.
      out->write( `oref_generic is initial` ).
    ELSE.
      out->write( `oref_generic is not initial` ).
    ENDIF.

    IF oref_generic IS INSTANCE OF zcl_demo_abap.
      out->write( `oref_generic is an instance of zcl_demo_abap` ).
    ELSE.
      out->write( `oref_generic is not an instance of zcl_demo_abap` ).
    ENDIF.

*&---------------------------------------------------------------------*
*& Accessing class attributes
*&---------------------------------------------------------------------*

    "Accessing instance attribute via an object reference variable
    oref = NEW #( utclong_current( ) ).
    "Read access
    DATA(ts4) = oref->timestamp.
    "Write access
    oref->instance_number = 123.

    "Accessing static attributes via the class name
    "Write access
    zcl_demo_abap=>static_number = 456.
    "Read access
    DATA(static_attr_access) = zcl_demo_abap=>static_number.
    "Read access (constant)
    DATA(const_value) = zcl_demo_abap=>text.

    "Accessing static attributes within the class without specifying the class name
    "See the test_static_method method for restrictions regarding instance attributes.
    "Write access
    static_number = 789.
    "Read access
    static_attr_access = static_number.
    "Read access (constant)
    const_value = text.

    "Type and data object declarations based on class attributes
    TYPES type1 TYPE zcl_demo_abap=>chars.
    TYPES type2 LIKE zcl_demo_abap=>static_number.
    TYPES type3 LIKE zcl_demo_abap=>text.
    DATA dobj1 TYPE zcl_demo_abap=>chars.
    DATA dobj2 LIKE zcl_demo_abap=>static_number.
    DATA dobj3 LIKE zcl_demo_abap=>text.

    "Without class name specification within the class itself
    TYPES type4 TYPE chars.
    TYPES type5 LIKE static_number.
    TYPES type6 LIKE text.
    DATA dobj4 TYPE chars.
    DATA dobj5 LIKE static_number.
    DATA dobj6 LIKE text.

    "Using instance attributes
    "Note: You can access static attributes via object reference variables.
    "However, it is advisable to access static attributes as shown above.
    DATA(oref3) = NEW zcl_demo_abap( utclong_current( ) ).
    TYPES type7 TYPE oref3->chars.
    TYPES type8 LIKE oref3->instance_number.

    DATA dobj7 TYPE oref3->chars.
    DATA dobj8 LIKE oref3->timestamp.
  ENDMETHOD.

  METHOD constructor.
    timestamp = ts.
  ENDMETHOD.

  METHOD test_static_method.
    "Accessing static attributes
    zcl_demo_abap=>static_number = 987.
    "Read access
    DATA(static_attr_access) = zcl_demo_abap=>static_number.
    "Read access (constant)
    DATA(const_value) = zcl_demo_abap=>text.

    "Accessing attributes within the class without specifying the class name
    "Write access
    static_number = 654.
    "Read access
    static_attr_access = static_number.
    "Read access (constant)
    const_value = text.

    "Access to instance attributes in static methods like above is not possible
    DATA inst_attr_access TYPE utclong.
    "inst_attr_access = timestamp.
    "Instance attribute access via object reference variable
    DATA(oref) = NEW zcl_demo_abap( utclong_current( ) ).
    inst_attr_access = oref->timestamp.
  ENDMETHOD.
ENDCLASS.
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
- When calling methods that declare importing and/or exporting parameters, keep the following in mind: For methods with importing parameters, you can or must (if other parameters are available and specified in the method call) precede the parameters and their assignments with `EXPORTING`. For methods with exporting parameters, you can or must use `IMPORTING` before the parameters and their assignments.


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
  ... "The random number is equal or greater than 5.
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

*&---------------------------------------------------------------------*
*& Method chaining with a functional method call
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& Method chaining with a standalone statement
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& Attributes
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& Methods
*&---------------------------------------------------------------------*
    
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

*&---------------------------------------------------------------------*
*& Constructors
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& Method calls
*&---------------------------------------------------------------------*

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

> [!NOTE] 
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

- `... PUBLIC ...`: Specifies that the class is a global class, available globally within the class library. The cheat sheet also includes example classes that do not specify  `PUBLIC`, declaring these classes as local classes.
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

> [!NOTE] 
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

> [!TIP]
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
*&---------------------------------------------------------------------*
*& Object reference variables
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& Upcasts
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& Downcasts
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& IS INSTANCE OF
*&---------------------------------------------------------------------*

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

*&---------------------------------------------------------------------*
*& CASE TYPE OF
*&---------------------------------------------------------------------*

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

> [!NOTE]  
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

## Events

High-level steps involved:
- Declaring an event
- Declaring an event handler
- Registering the event handler so that it can handle the event
- Raising an event

**Events**
- [Events](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenevent_glosry.htm "Glossary Entry") can trigger the processing of [processing blocks](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocessing_block_glosry.htm "Glossary Entry"), meaning they can initiate the execution of event handlers (that is, other methods).
- You can declare events in the visibility section of a class or interface declaration, for example:
  - as an instance event using an [`EVENTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapevents.htm) statement.
  - as a static event using [`CLASS-EVENTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass-events.htm).
- The key difference is that instance events are bound to specific instances of classes and can only be raised by instance methods within the same class. In contrast, static events are independent of class instances and can be raised by any method in the class.
- Parameter interface:
  - In the declaration, you can only specify output parameters (using `EXPORTING` and formal parameters specifying passing by value) for an event. When the event is raised, actual parameters are transferred to event handlers.
  - For instance events, the parameter interface includes an implicit output parameter called `sender`, which is a reference variable that automatically assigns the raising object.
- Example declarations:
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


**Event handlers** 
- An event is raised by a [`RAISE EVENT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapraise_event.htm) statement in either another method or the same method.
- Raising an event triggers the associated [event handlers](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenevent_handler_glosry.htm "Glossary Entry"), which are special methods that can handle the event.
- Typically, event handlers are called indirectly using `RAISE EVENT` rather than through direct method calls.
- To execute an event handler when an event is raised, you must register it with a `SET HANDLER` statement.
- Both static and instance methods can function as event handlers. The main difference is that static event handlers can be called independently of class instances.
- Use the `FOR EVENT some_event OF cl|intf` syntax for event handler declarations.
- Pay attention to the visibility section of an event handler; it cannot be more general than the event's visibility section.
- Case: Instance method as event handler:
   - If `some_event` is an instance event, the event handler can manage events for all objects of class `cl`, its subclasses, or objects implementing the interface `intf`.
   - If `some_event` is a static event, the event handler can manage events for class `cl`, its subclasses, or any classes implementing the interface `intf`.
- You can also specify additions like `ABSTRACT` and `FINAL` for instance methods, and `DEFAULT IGNORE` or `DEFAULT FAIL` for both instance and static methods.
- Parameter interface:
   - You can use `IMPORTING` in the declaration to define input parameters.
   - You may only specify those formal parameters defined as output parameters with `EXPORTING` in the declared event.
   - Specify only the names of the formal parameters without modifiers like `TYPE`, `LIKE`, `OPTIONAL`, or `DEFAULT`. The properties will be inherited from the specified event parameters.
   - You do not need to specify all output parameters.
   - If `some_event` is an instance event, you can specify the `sender` formal parameter, which is an implicit output parameter for each instance event, providing a reference to the object that raised the event.
- Example declarations:
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

**Registering and deregistering event handlers** 

- Two syntax forms are available for (de)registering:
  - For instance event handlers: `SET HANDLER handler1 handler2 ... FOR oref|ALL INSTANCES [ACTIVATION act].`
    - `handler1`, etc. can be specified as follows: `meth` (methods from the same or other classes defined as instance event handlers; event handlers for static events cannot be specified), `oref->meth`, `class=>meth`
    - `FOR oref`: (De)registers event handlers for a single object.
    - `FOR ALL INSTANCES`: (De)registers the event handlers for all instances.
    - `ACTIVATION`: Used to (de)register event handlers; expects a single-character text field. The default value is `X` (indicating registration; so, the addition is optional for registration). A blank value means it is deregistered.
  - For static event handlers: `SET HANDLER handler1 handler2 ... [ACTIVATION act].`
    - The statement (de)registers static event handlers (not instance events).
    - Note that the (de)registration is independent of class instances and applies globally to the current [internal session](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninternal_session_glosry.htm).
- `SET HANDLER` statements set the system field `sy-subrc`. See the example.
  
**Raising events**

- Events are raised using [`RAISE EVENT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapraise_event.htm) statements, which can only occur within methods.
- After raising the event, all registered event handlers are executed, and the program flow continues after the statement.
- You can use the `EXPORTING` addition (`RAISE EVENT some_event EXPORTING a = b c = d ...`) to pass actual parameters to event handlers.
- If the formal parameter `sender` is declared for an event handler, it automatically receives a reference to the raising object when instance events are triggered. However, `sender` cannot be explicitly specified or assigned here.
  
> [!NOTE]  
> In RAP, special [RAP business events](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_entity_event_glosry.htm) are available. They can be raised in [ABAP behavior pools](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbehavior_pool_glosry.htm) with [RAISE ENTITY EVENT](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapraise_entity_event.htm) statements. Find more information in the [ABAP for RAP: Entity Manipulation Language (ABAP EML)](08_EML_ABAP_for_RAP.md) cheat sheet.


Expand the following collapsible section for example code. To try it out, create a demo class named `zcl_demo_abap` and paste the code into it. Note that the example includes code in the global class and the CCIMP include (Local Types tab in ADT). After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console. For more information on the example, see the inline comments.


<details>
  <summary>🟢 Click to expand for example code</summary>
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

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-DATA event_log TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS display_log_table IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

*&-------------------------------------------------------------------------------------*
*& 1) Basic events
*&-------------------------------------------------------------------------------------*

    "- The class lcl_greetings in the CCIMP include declares four instance events without parameters.
    "- The same class also defines four event handler methods.
    "- Additionally, a separate method implements the raising of the four events.
    "- The event handlers are registered here using a SET HANDLER statement.
    "- Based on the current time, a string (an instance attribute in the class) is populated with a
    "  greeting appropriate for the time of day.

    out->write( |1) Basic events\n\n| ).

    DATA(ref_events) = NEW lcl_greetings( ).

    "Registering event handler methods
    SET HANDLER: ref_events->morning_greets
                 ref_events->afternoon_greets
                 ref_events->evening_greets
                 ref_events->night_greets
                 FOR ref_events.

    "Calling method that raises an event
    ref_events->greetings( ).

    out->write( data = ref_events->greets name = `ref_events->greets` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

*&-------------------------------------------------------------------------------------*
*& 2) Various instance and static events
*&-------------------------------------------------------------------------------------*

    "- The following code snippets explore various instance and static events.
    "- The class lcl_1 declares instance and static events, some with and some without
    "  exporting parameters.
    "- The class also declares event handlers. The evt_handler_c method intentionally omits the num2
    "  formal parameter, demonstrating that it is optional to specify all parameters. Additionally,
    "  explicitly specifying the sender parameter is also optional.
    "- The example implementations populate a string table for visualization and display purposes.

    out->write( |2) Various instance and static events\n\n| ).

    DATA(oref_evt_1) = NEW lcl_1( ).
    oref_evt_1->some_text = `ABAP`.

    "Registering event handler methods
    SET HANDLER: oref_evt_1->evt_handler_a
                 oref_evt_1->evt_handler_b
                 oref_evt_1->evt_handler_c
                 FOR oref_evt_1.

    SET HANDLER: lcl_1=>evt_handler_d lcl_1=>evt_handler_e.

    "Calling method that raises an event
    DO 5 TIMES.
      oref_evt_1->raise_event( int = sy-index
                               txt = CONV #( sy-index ) ).
    ENDDO.

    out->write( data = lcl_1=>event_log_lcl_1 name = `lcl_1=>event_log_lcl_1` ).
    out->write( |{ repeat( val = `*` occ = 75 ) }\n| ).

*&-------------------------------------------------------------------------------------*
*& SET HANDLER syntax options
*& 3a) Registering events
*&-------------------------------------------------------------------------------------*

    "- The following code snippets demonstrate syntax options of the SET HANDLER statement.
    "- The demo class declares two instance events and a static event. The class also
    "  includes the event handlers. The raise_event method raises a specific event based
    "  on the value of the importing parameter. Note that for simplicity of the example,
    "  the instance method raise_event also raises the static event.

    out->write( |3) SET HANDLER syntax options\n| ).
    out->write( |3a) Registering events\n\n| ).

    DATA(oref_evt_2a) = NEW lcl_2( ).

    "Registering instance events
    SET HANDLER oref_evt_2a->evt_handler_f FOR oref_evt_2a.
    "Optional specification of the ACTIVATION addition. The default value is X.
    SET HANDLER oref_evt_2a->evt_handler_g FOR oref_evt_2a ACTIVATION 'X'.
    "Registering static event
    SET HANDLER lcl_2=>evt_handler_h ACTIVATION 'X'.

    DO 3 TIMES.
      oref_evt_2a->raise_event( num = sy-index ).
    ENDDO.

    out->write( data = lcl_2=>event_log_lcl_2 name = `lcl_2=>event_log_lcl_2` ).
    out->write( |{ repeat( val = `*` occ = 75 ) }\n| ).
    CLEAR lcl_2=>event_log_lcl_2.

*&-------------------------------------------------------------------------------------*
*& 3b) Deregistering events
*&-------------------------------------------------------------------------------------*

    out->write( |3b) Deregistering events\n\n| ).

    DATA(oref_evt_2b) = NEW lcl_2( ).

    "Registering and deregistering an instance event handler.
    "Note that the handler for the static event is still registered from above, therefore it
    "is called. In the loop it is explicitly deregistered. So the result will not show two
    "entries for evt_handler_h.
    SET HANDLER oref_evt_2b->evt_handler_f FOR oref_evt_2b ACTIVATION 'X'.
    SET HANDLER oref_evt_2b->evt_handler_g FOR oref_evt_2b ACTIVATION ' '.

    DO 4 TIMES.
      IF sy-index < 4.
        oref_evt_2b->raise_event( num = sy-index ).
      ELSE.
        SET HANDLER lcl_2=>evt_handler_h ACTIVATION ' '.
        oref_evt_2b->raise_event( num = 3 ).
      ENDIF.
    ENDDO.

    out->write( data = lcl_2=>event_log_lcl_2 name = `lcl_2=>event_log_lcl_2` ).
    out->write( |{ repeat( val = `*` occ = 75 ) }\n| ).

    CLEAR lcl_2=>event_log_lcl_2.

*&-------------------------------------------------------------------------------------*
*& 3c) sy-subrc value setting of SET HANDLER statements
*&-------------------------------------------------------------------------------------*

    "sy-subrc values set by SET HANDLER statements
    "0: All specified handlers were successfully (de)registered.
    "4: At least one specified handler was not registered because it was already registered for the same event.
    "8: At least one specified handler was not deregistered because it was not registered for the current event.

    out->write( |3c) sy-subrc value setting of SET HANDLER statements\n\n| ).

    DATA(oref_evt_2c) = NEW lcl_2( ).

    SET HANDLER oref_evt_2c->evt_handler_f
                oref_evt_2c->evt_handler_g
                FOR oref_evt_2c ACTIVATION 'X'.

    DATA(subrc) = sy-subrc.

    out->write( data = subrc name = `sy-subrc value (1)` ).
    out->write( |\n| ).

    SET HANDLER oref_evt_2c->evt_handler_f
                oref_evt_2c->evt_handler_g
                FOR oref_evt_2c ACTIVATION 'X'.

    subrc = sy-subrc.

    out->write( data = subrc name = `sy-subrc value (2)` ).
    out->write( |\n| ).

    SET HANDLER lcl_2=>evt_handler_h ACTIVATION ' '.
    subrc = sy-subrc.

    out->write( data = subrc name = `sy-subrc value (3)` ).
    out->write( |\n| ).
    out->write( |{ repeat( val = `*` occ = 75 ) }\n| ).
    CLEAR lcl_2=>event_log_lcl_2.

*&-------------------------------------------------------------------------------------*
*& 3d) FOR ALL INSTANCES addition
*&-------------------------------------------------------------------------------------*

    out->write( |3d) FOR ALL INSTANCES addition\n| ).

    DATA(oref_evt_2d) = NEW lcl_2( ).

    out->write( |Example 1\n\n| ).

    "- evt_handler_f is registered for all instances
    "- evt_handler_g and evt_handler_h are not regitered
    SET HANDLER oref_evt_2d->evt_handler_f FOR ALL INSTANCES.

    "Creating some more instances
    DATA(oref_evt_2e) = NEW lcl_2( ).
    DATA(oref_evt_2f) = NEW lcl_2( ).

    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          APPEND `-------------- oref_evt_2d --------------` TO lcl_2=>event_log_lcl_2.
          DO 3 TIMES.
            oref_evt_2d->raise_event( num = sy-index ).
          ENDDO.
        WHEN 2.
          APPEND `-------------- oref_evt_2e --------------` TO lcl_2=>event_log_lcl_2.
          DO 3 TIMES.
            oref_evt_2e->raise_event( num = sy-index ).
          ENDDO.
        WHEN 3.
          APPEND `-------------- oref_evt_2f --------------` TO lcl_2=>event_log_lcl_2.
          DO 3 TIMES.
            oref_evt_2f->raise_event( num = sy-index ).
          ENDDO.
      ENDCASE.
    ENDDO.

    out->write( data = lcl_2=>event_log_lcl_2 name = `lcl_2=>event_log_lcl_2` ).
    out->write( |{ repeat( val = `*` occ = 75 ) }\n| ).
    CLEAR lcl_2=>event_log_lcl_2.

    out->write( |Example 2\n\n| ).

    "- evt_handler_g is registered for all instances
    "- evt_handler_f is still registered for all instances
    "- evt_handler_h is not regitered
    SET HANDLER oref_evt_2d->evt_handler_g FOR ALL INSTANCES.

    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          APPEND `-------------- oref_evt_2d --------------` TO lcl_2=>event_log_lcl_2.
          DO 3 TIMES.
            oref_evt_2d->raise_event( num = sy-index ).
          ENDDO.
        WHEN 2.
          APPEND `-------------- oref_evt_2e --------------` TO lcl_2=>event_log_lcl_2.
          DO 3 TIMES.
            oref_evt_2e->raise_event( num = sy-index ).
          ENDDO.
        WHEN 3.
          APPEND `-------------- oref_evt_2f --------------` TO lcl_2=>event_log_lcl_2.
          DO 3 TIMES.
            oref_evt_2f->raise_event( num = sy-index ).
          ENDDO.
      ENDCASE.
    ENDDO.

    out->write( data = lcl_2=>event_log_lcl_2 name = `lcl_2=>event_log_lcl_2` ).
    out->write( |{ repeat( val = `*` occ = 75 ) }\n| ).
    CLEAR lcl_2=>event_log_lcl_2.

    out->write( |Example 3\n\n| ).

    "- Deregistering evt_handler_g and evt_handler_h for all instances
    "- evt_handler_h is not regitered
    SET HANDLER oref_evt_2d->evt_handler_f
                oref_evt_2d->evt_handler_g
                FOR ALL INSTANCES ACTIVATION ' '.

    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          APPEND `-------------- oref_evt_2d --------------` TO lcl_2=>event_log_lcl_2.
          DO 3 TIMES.
            oref_evt_2d->raise_event( num = sy-index ).
          ENDDO.
        WHEN 2.
          APPEND `-------------- oref_evt_2e --------------` TO lcl_2=>event_log_lcl_2.
          DO 3 TIMES.
            oref_evt_2e->raise_event( num = sy-index ).
          ENDDO.
        WHEN 3.
          APPEND `-------------- oref_evt_2f --------------` TO lcl_2=>event_log_lcl_2.
          DO 3 TIMES.
            oref_evt_2f->raise_event( num = sy-index ).
          ENDDO.
      ENDCASE.
    ENDDO.

    out->write( data = lcl_2=>event_log_lcl_2 name = `lcl_2=>event_log_lcl_2` ).
    out->write( |\n{ repeat( val = `*` occ = 75 ) }\n| ).

*&-------------------------------------------------------------------------------------*
*& 4) Excursion: Events in inheritance
*&-------------------------------------------------------------------------------------*

    out->write( |4) Excursion: Events in inheritance\n| ).
    out->write( |4a) Static events\n| ).

    "Inheritance tree of the example
    "LCL_A
    "  |
    "  |--LCL_B
    "  |   |
    "  |   |--LCL_C
    "
    "LCL_D: Class that implements event handlers

    "- Static and instance events are declared in the superclass lcl_a.
    "- The superclass and its subclasses contain a static and an instance method that raise the events.
    "- The class lcl_d declares event handlers for the events in lcl_b, which are inherited from the superclass lcl_a.
    "- Different options are used to access methods. A log table visualizes the method call flow.
    "- The event handler stat_evt_handler in class lcl_b can only handle events raised within this class or its subclasses.
    "- Events raised by the static method stat_meth_1 in lcl_a are not handled, regardless of the class name used for the call.
    "- The event handler inst_evt_handler in class lcl_b can also only handle events raised within this class or its subclasses.
    "- Unlike calling static methods, events raised in the inherited instance method inst_meth_1 of the classes lcl_b and lcl_c
    "  are handled. The object's class is always addressed when addressig an object using an object reference.

    SET HANDLER lcl_d=>stat_evt_handler.

    lcl_a=>stat_meth_1( ).

    out->write( |lcl_a=>stat_meth_1( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).

    lcl_b=>stat_meth_1( ).

    out->write( |lcl_b=>stat_meth_1( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).

    lcl_c=>stat_meth_1( ).

    out->write( |lcl_c=>stat_meth_1( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).

    lcl_b=>stat_meth_2( ).

    out->write( |lcl_b=>stat_meth_2( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).

    lcl_c=>stat_meth_2( ).

    out->write( |lcl_c=>stat_meth_2( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).

    lcl_c=>stat_meth_3( ).

    out->write( |lcl_c=>stat_meth_3( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).
    out->write( |{ repeat( val = `*` occ = 75 ) }\n| ).

    out->write( |4b) Instance events\n| ).

    DATA(oref1) = NEW lcl_a( ).
    DATA(oref2) = NEW lcl_b( ).
    DATA(oref3) = NEW lcl_c( ).
    DATA(oref4) = NEW lcl_d( ).

    SET HANDLER oref4->inst_evt_handler FOR ALL INSTANCES.

    oref1->inst_meth_1( ).

    out->write( |oref1->inst_meth_1( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).

    oref2->inst_meth_1( ).

    out->write( |oref2->inst_meth_1( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).

    oref3->inst_meth_1( ).

    out->write( |oref3->inst_meth_1( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).

    oref2->inst_meth_2( ).

    out->write( |oref2->inst_meth_2( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).

    oref3->inst_meth_2( ).

    out->write( |oref3->inst_meth_2( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).

    oref3->inst_meth_3( ).

    out->write( |oref3->inst_meth_3( ): "{ lcl_a=>is_handled }"| ).
    display_log_table( out ).
  ENDMETHOD.

  METHOD display_log_table.
    out->write( data = lcl_a=>log_tab name = `lcl_a=>log_tab` ).
    out->write( |\n\n| ).
    CLEAR lcl_a=>log_tab.
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
*& Example class 1
*&---------------------------------------------------------------------*

"- The class declares four instance events without parameters.
"- The same class also defines four event handler methods.
"- Additionally, a separate method implements the raising of the four events.
"- Based on the current time, a string (an instance attribute in the class) is
"  populated with a greeting appropriate for the time of day.

CLASS lcl_greetings DEFINITION.
  PUBLIC SECTION.
    "Attributes for display purposes
    DATA: greets TYPE string,
          time   TYPE t.

    "Event declarations
    EVENTS: morning,
      afternoon,
      evening,
      night.

    "Event handler methods
    METHODS: morning_greets FOR EVENT morning OF lcl_greetings,
      afternoon_greets FOR EVENT afternoon OF lcl_greetings,
      evening_greets FOR EVENT evening OF lcl_greetings,
      night_greets FOR EVENT night OF lcl_greetings.

    "Method that includes event raising in the implementation
    METHODS: greetings.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_greetings IMPLEMENTATION.
  METHOD greetings.
    time = cl_abap_context_info=>get_system_time( ).

    IF time BETWEEN '050001' AND '120000'.
      RAISE EVENT morning.
    ELSEIF time BETWEEN '120001' AND '170000'.
      RAISE EVENT afternoon.
    ELSEIF time BETWEEN '170001' AND '210000'.
      RAISE EVENT evening.
    ELSEIF time BETWEEN '210001' AND '050000'.
      RAISE EVENT night.
    ENDIF.
  ENDMETHOD.

  METHOD morning_greets.
    greets = |Good morning, { sy-uname }. It's { time TIME = ENVIRONMENT }.|.
  ENDMETHOD.

  METHOD afternoon_greets.
    greets = |Good afternoon, { sy-uname }. It's { time TIME = ENVIRONMENT }.|.
  ENDMETHOD.

  METHOD evening_greets.
    greets = |Good evening, { sy-uname }. It's { time TIME = ENVIRONMENT }.|.
  ENDMETHOD.

  METHOD night_greets.
    greets = |Good night, { sy-uname }. It's { time TIME = ENVIRONMENT }.|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Example class 2
*&---------------------------------------------------------------------*

"- The class lcl_1 declares instance and static events, some with and some without
"  exporting parameters.
"- The class also declares event handlers. The evt_handler_c method intentionally omits the num2
"  formal parameter, demonstrating that it is optional to specify all parameters. Additionally,
"  explicitly specifying the sender parameter is also optional.
"- The example implementations populate a string table for visualization and display purposes.
"- Note that for simplicity of the example, the instance method raise_event not only raises instance
"  events but also static events.
"- Based on the value of the importing parameter of the raise_event method, a specific event
"  is raised.

CLASS lcl_1 DEFINITION.
  PUBLIC SECTION.
    "Attributes for display purposes
    CLASS-DATA event_log_lcl_1 TYPE string_table.
    DATA some_text TYPE string.

    "Event declarations
    "Instance events
    EVENTS: inst_event_a,
      inst_event_b EXPORTING VALUE(num) TYPE i,
      inst_event_c EXPORTING VALUE(num1) TYPE i VALUE(num2) TYPE i VALUE(text) TYPE string.
    "Static events
    CLASS-EVENTS: stat_event_d,
      stat_event_e EXPORTING VALUE(num) TYPE i.

    "Event handler methods
    "Instance methods
    METHODS: evt_handler_a FOR EVENT inst_event_a OF lcl_1,
      evt_handler_b FOR EVENT inst_event_b OF lcl_1 IMPORTING num,
      evt_handler_c FOR EVENT inst_event_c OF lcl_1 IMPORTING num1 text sender.
    "Static events
    CLASS-METHODS: evt_handler_d FOR EVENT stat_event_d OF lcl_1,
      evt_handler_e FOR EVENT stat_event_e OF lcl_1 IMPORTING num.

    "Method that includes event raising in the implementation
    METHODS raise_event IMPORTING int TYPE i
                                  txt TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_1 IMPLEMENTATION.
  METHOD evt_handler_a.
    APPEND `evt_handler_a: Event handled` TO event_log_lcl_1.
    APPEND INITIAL LINE TO event_log_lcl_1.
  ENDMETHOD.

  METHOD evt_handler_b.
    APPEND `evt_handler_b: Event handled` TO event_log_lcl_1.
    APPEND |Value of num passed: { num }| TO event_log_lcl_1.
    APPEND INITIAL LINE TO event_log_lcl_1.
  ENDMETHOD.

  METHOD evt_handler_c.
    APPEND `evt_handler_c: Event handled` TO event_log_lcl_1.
    APPEND |Value of num1 passed: { num1 }| TO event_log_lcl_1.
    APPEND |Value of text passed: { text }| TO event_log_lcl_1.
    DATA(sender_cl_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( sender ) )->get_relative_name( ).
    APPEND |Accessing sender reference variable; class name: { sender_cl_name }| TO event_log_lcl_1.
    APPEND |Value of instance attribute, accessed via sender: { sender->some_text }| TO event_log_lcl_1.
    APPEND INITIAL LINE TO event_log_lcl_1.
  ENDMETHOD.

  METHOD evt_handler_d.
    APPEND `evt_handler_d: Event handled` TO event_log_lcl_1.
    APPEND INITIAL LINE TO event_log_lcl_1.
  ENDMETHOD.

  METHOD evt_handler_e.
    APPEND `evt_handler_e: Event handled` TO event_log_lcl_1.
    APPEND |Value of num passed: { num }| TO event_log_lcl_1.
    APPEND INITIAL LINE TO event_log_lcl_1.
  ENDMETHOD.

  METHOD raise_event.
    CASE int.
      WHEN 1.
        RAISE EVENT inst_event_a.
      WHEN 2.
        RAISE EVENT inst_event_b EXPORTING num = int.
      WHEN 3.
        RAISE EVENT inst_event_c EXPORTING num1 = int num2 = int text = txt.
      WHEN 4.
        RAISE EVENT stat_event_d.
      WHEN 5.
        RAISE EVENT stat_event_e EXPORTING num = int.
      WHEN OTHERS.
        RAISE EVENT stat_event_d.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Example class 3
*&---------------------------------------------------------------------*

"This example class is setup similarly to lcl_1.

CLASS lcl_2 DEFINITION.
  PUBLIC SECTION.
    "Attribute for display purposes
    CLASS-DATA event_log_lcl_2 TYPE string_table.

    "Event declarations
    EVENTS: inst_event_f,
            inst_event_g.
    CLASS-EVENTS stat_event_h.

    "Event handler methods
    METHODS: evt_handler_f FOR EVENT inst_event_f OF lcl_2,
           evt_handler_g FOR EVENT inst_event_g OF lcl_2.
    CLASS-METHODS: evt_handler_h FOR EVENT stat_event_h OF lcl_2.

    "Method that includes event raising in the implementation
    METHODS raise_event IMPORTING num TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_2 IMPLEMENTATION.
  METHOD evt_handler_f.
    APPEND `evt_handler_f: Event handled` TO event_log_lcl_2.
    APPEND INITIAL LINE TO event_log_lcl_2.
  ENDMETHOD.

  METHOD evt_handler_g.
    APPEND `evt_handler_g: Event handled` TO event_log_lcl_2.
    APPEND INITIAL LINE TO event_log_lcl_2.
  ENDMETHOD.

  METHOD evt_handler_h.
    APPEND `evt_handler_h: Event handled` TO event_log_lcl_2.
    APPEND INITIAL LINE TO event_log_lcl_2.
  ENDMETHOD.

  METHOD raise_event.
    CASE num.
      WHEN 1.
        RAISE EVENT inst_event_f.
      WHEN 2.
        RAISE EVENT inst_event_g.
      WHEN OTHERS.
        RAISE EVENT stat_event_h.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Example classes 4 - 7
*&---------------------------------------------------------------------*

    "The example classes 4 - 6 represent an inheritance tree:
    "LCL_A
    "  |
    "  |--LCL_B
    "  |   |
    "  |   |--LCL_C
    "
    "Example class 7, LCL_D, is a class that implements event handlers.

    "Example notes:
    "- Static and instance events are declared in the superclass lcl_a.
    "- The superclass and its subclasses contain a static and an instance
    "  method that raise the events.
    "- The class lcl_d declares event handlers for the events in lcl_b, which
    "  are inherited from the superclass lcl_a.
    "- In the global class, different options are used to access methods.
    "- A log table visualizes the method call flow. Additionally, a flag is
    "  populated illustrating whether the event is handled.

CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: log_tab TYPE string_table,
                is_handled TYPE abap_boolean.
    CLASS-EVENTS stat_event.
    CLASS-METHODS stat_meth_1.
    EVENTS  inst_event.
    METHODS inst_meth_1.
ENDCLASS.

CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    CLASS-METHODS stat_meth_2.
    METHODS inst_meth_2.
ENDCLASS.

CLASS lcl_c DEFINITION INHERITING FROM lcl_b.
  PUBLIC SECTION.
    CLASS-METHODS stat_meth_3.
    METHODS inst_meth_3.
ENDCLASS.

CLASS lcl_d DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS stat_evt_handler FOR EVENT stat_event OF lcl_b.
    METHODS inst_evt_handler FOR EVENT inst_event OF lcl_b.
ENDCLASS.

CLASS lcl_a IMPLEMENTATION.
  METHOD stat_meth_1.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_a/stat_meth_1 called` TO lcl_a=>log_tab.
    RAISE EVENT stat_event.
  ENDMETHOD.
  METHOD inst_meth_1.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_a/inst_meth_1 called` TO lcl_a=>log_tab.
    RAISE EVENT inst_event.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_b IMPLEMENTATION.
  METHOD stat_meth_2.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_b/stat_meth_2 called` TO lcl_a=>log_tab.
    RAISE EVENT stat_event.
  ENDMETHOD.
  METHOD inst_meth_2.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_b/inst_event called` TO lcl_a=>log_tab.
    RAISE EVENT inst_event.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_c IMPLEMENTATION.
  METHOD stat_meth_3.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_c/stat_meth_3 called` TO lcl_a=>log_tab.
    RAISE EVENT stat_event.
  ENDMETHOD.
  METHOD inst_meth_3.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_c/inst_meth_3 called` TO lcl_a=>log_tab.
    RAISE EVENT inst_event.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_d IMPLEMENTATION.
  METHOD stat_evt_handler.
    lcl_a=>is_handled = abap_true.
    APPEND `lcl_d/stat_evt_handler called` TO lcl_a=>log_tab.
    APPEND `--- Static event handled ---` TO lcl_a=>log_tab.
  ENDMETHOD.
  METHOD inst_evt_handler.
    lcl_a=>is_handled = abap_true.
    APPEND `lcl_d/inst_evt_handler called` TO lcl_a=>log_tab.
    APPEND `--- Instance event handled ---` TO lcl_a=>log_tab.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  




<p align="right"><a href="#top">⬆️ back to top</a></p>



## Excursions

### ABAP Examples of Design Patterns in Object-Oriented Programming

Find ABAP examples of design patterns in object-oriented programming in [this ABAP cheat sheet](34_OO_Design_Patterns.md).

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

> [!NOTE]  
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)


