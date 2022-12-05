<a name="top"></a>

# ABAP Object Orientation

> **💡 Note**<br>
> This cheat sheet provides an overview on selected syntax options and concepts related to ABAP object orientation. It is supported by code snippets and an executable example. They are **not** suitable as role models for object-oriented design. Their primary focus is on the syntax and functionality. For more details, refer to the respective topics in the ABAP Keyword Documentation. Find an overview in the topic [ABAP Objects - Overview](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_objects_oview.htm).

- [ABAP Object Orientation](#abap-object-orientation)
  - [Classes and Objects](#classes-and-objects)
    - [Creating Classes](#creating-classes)
      - [Creating a Local Class](#creating-a-local-class)
      - [Creating a Global Class](#creating-a-global-class)
    - [Visibility of Components](#visibility-of-components)
      - [Creating the Visibility Sections](#creating-the-visibility-sections)
    - [Defining Components](#defining-components)
  - [Working with Objects and Components](#working-with-objects-and-components)
  - [Notes on Inheritance](#notes-on-inheritance)
  - [Notes on Polymorphism and Casting](#notes-on-polymorphism-and-casting)
  - [Working with Interfaces](#working-with-interfaces)
  - [Further Concepts](#further-concepts)
    - [Factory Methods](#factory-methods)
    - [Friendship](#friendship)
    - [Events](#events)
  - [Executable Example](#executable-example)

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
    The terms object and instance are used synonymously.
-   exist in the [internal
    session](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninternal_session_glosry.htm "Glossary Entry")
    of an [ABAP
    program](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_program_glosry.htm "Glossary Entry")


Classes ...
-   are templates for objects, i. e. they determine the appearance of
    all instances of a class. All instances are created based on this
    template (this is what is called
    [instantiation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstantiation_glosry.htm "Glossary Entry")).
    -   If, for example, a vehicle represents a class, then the
        instances of the class `vehicle` have the same setup.
        That means they all share the same kind of data like the brand,
        model and color or the same functionality like the acceleration.
        However, the values are different from instance to instance.
        Hence, an instance has a unique identity. For example, one
        instance is a red sedan of brand A having a certain
        acceleration; another instance is a black SUV of brand B and so
        on. You create an object (or instance respectively) that stands
        for an actual vehicle to work with in your ABAP program.
    -   Basically, you might create any number of objects that are based
        on a class.
-   contain
    [components](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomponent_glosry.htm "Glossary Entry"):
    -   [Attributes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenattribute_glosry.htm "Glossary Entry")
        of the objects (the data declarations)
    -   [Methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmethod_glosry.htm "Glossary Entry")
        that typically operate on this data
    -   [Events](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenevent_glosry.htm "Glossary Entry")

<p align="right">(<a href="#top">back to top</a>)</p>

### Creating Classes

You can either create local or global classes:

<table>
<tr>
    <td><a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlocal_class_glosry.htm">Local classes</a></td>
    <td><ul><li>can be defined within an ABAP program (or a <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenccimp_glosry.htm">CCIMP include</a> respectively)</li><li>can only be used in the program (or global class/CCIMP include respectively) in which the class is defined</li></ul></td>
</tr>
<tr>
    <td><a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_class_glosry.htm">Global
classes</a></td>
    <td><ul><li>are defined as global type; hence, they can be used by all ABAP programs or global classes respectively since they are globally visible</li><li>are declared in <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_pool_glosry.htm">class pools</a></li></ul> </td>
</tr>
</table>

> **💡 Note**<br>
> - Regarding the names of global and local classes and usage of classes in ABAP programs when, for example, calling methods, the system searches for a local class with the specific name at first. Then, if a local class with that name is not found, the systems searches for a global class.
> - The class design must be done with care. If a class is only used in one program (or class), choosing a local class is enough. However, global
classes must be prepared to be used anywhere. A later change of that class, especially regarding the visibility of components (see
further down) or the data types of attributes that are used in other programs might cause problems.
> - Apart from ADT, global classes can also be created in the ABAP Workbench (`SE80`) or with transaction `SE24` in on-premise systems.

The basic structure of classes consists of a declaration and an implementation part that are both introduced by `CLASS` and ended by `ENDCLASS`.

#### Creating a Local Class

``` abap
CLASS local_class DEFINITION.
    
    "Here go the declaration for all components and visibility sections.
    "You should place the declaration at the beginning of the program.

ENDCLASS.

CLASS local_class IMPLEMENTATION.

    "Here go the method implementations.
    "Only required if you declare methods in the DEFINITION part.

ENDCLASS.
```

#### Creating a Global Class
The code snippet shows a basic skeleton of a global class. There are [further
additions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_options.htm)
possible.

``` abap
CLASS global_class DEFINITION
  PUBLIC           "Makes the class a global class in the class library.
  FINAL            "Means that no subclasses can be derived from this class.
  CREATE PUBLIC.   "This class can be instantiated anywhere it is visible.

    ...

    "Here go the declaration for all components and visibility sections.

ENDCLASS.

CLASS global_class IMPLEMENTATION.

    "Here go the method implementations.
    "Only required if you declare methods in the DEFINITION part.

ENDCLASS.
```
> **💡 Note**<br>
> The addition `... CREATE PROTECTED.` of the class declaration part means that the class can only be instantiated in methods of its
[subclasses](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubclass_glosry.htm "Glossary Entry"),
of the class itself, and of its
[friends](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfriend_glosry.htm "Glossary Entry").
The addition `... CREATE PRIVATE` means that the class can only
be instantiated in methods of the class itself or of its friends. Hence,
it cannot be instantiated as an inherited component of subclasses.

<p align="right">(<a href="#top">back to top</a>)</p>

### Visibility of Components

In the class declaration part, you must specify (at least one of the)
[visibility
sections](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenvisibility_section_glosry.htm "Glossary Entry")
to determine how to interact with the class. For example, you want to
hide and thus disallow the usage of certain data. The visibility
sections are as follows:

<table>
<tr>
  <td><pre>PUBLIC.</pre></td>
    <td>Components declared in this section can be accessed from within the class and from outside including subclasses.</td>
</tr>
<tr>
 <td><pre>PROTECTED.</pre></td>
    <td>Components declared in this section can be
    accessed from within the class and from <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubclass_glosry.htm">subclasses</a> and <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfriend_glosry.htm">friends</a>
     - topics related to <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninheritance_glosry.htm">inheritance</a>.</td>
</tr>
  <tr>
 <td><pre>PRIVATE.</pre></td>
    <td>Components declared in this section can only be accessed from within the class in which they are declared and its friends.</td>
</tr>
</table>

#### Creating the Visibility Sections
At least one section must be specified.
``` abap
CLASS local_class DEFINITION.
    PUBLIC.
      "Here go the components.
    PROTECTED.
      "Here go the components.
    PRIVATE.
      "Here go the components.
ENDCLASS.
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Defining Components

All components - attributes (using `TYPES`, `DATA`,
`CLASS-DATA`, and `CONSTANTS` for data types and data
objects), methods (using `METHODS` and `CLASS-METHODS`),
events `EVENTS` and `CLASS-EVENTS` as well as interfaces - are declared in the declaration part of the class. There, they must be
assigned to a visibility section.

Regarding, for example, `DATA` and `CLASS-DATA`, two
kind of components are to be distinguished:

-   [Instance
    components](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_component_glosry.htm "Glossary Entry"):
    Components that exist separately for each instance and can only be
    accessed in instances of a class.
-   [Static
    components](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_component_glosry.htm "Glossary Entry"):
    Components that are not specific for instances. They exist only once
    per class and can be accessed using the name of the class.

**Attributes**

-   The attributes of a class mean the data objects declared within a
    class (or interface).
-   [Static
    attributes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_attribute_glosry.htm "Glossary Entry")
    (`CLASS-DATA`): Their content is independent of instances of
    a class and, thus, valid for all instances. As shown further down,
    static attributes can be accessed by using the class name without a
    prior creation of an instance. Note that changing an instance
    attribute means the change is visible in all instances.
-   [Instance
    attributes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_attribute_glosry.htm "Glossary Entry")
    (`DATA`): Determine the instance-dependent state. The data
    is only valid in the context of an instance. As shown further down,
    instance attributes can only be accessed via an [object reference
    variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm "Glossary Entry").

> **💡 Note**<br>
> You can declare static attributes that should not be
changed using
[`CONSTANTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapconstants.htm)
statements. You specify the values for the constants when you declare
them. Furthermore, the addition
[`READ-ONLY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapdata_options&sap-language=EN&sap-client=000&version=X&anchor=!ABAP_ADDITION_2@2@&tree=X)
can be used in the public visibility section as a means of securing data
objects against undesired changes from outside. A change is only
possible via the methods of the class or it subclasses.

Declaring attributes in visibility sections. In the snippet below, all attributes are declared in the `PUBLIC.` section of a local class.
``` abap
CLASS local_class DEFINITION.

    PUBLIC.
      TYPES: some_type TYPE c LENGTH 3.              "Type declaration

      DATA: inst_number TYPE i,                      "Instance attributes
            inst_string TYPE string,
            dobj_r_only TYPE c LENGTH 5 READ-ONLY.   "Read-only attribute

      CLASS-DATA: stat_number TYPE i,                "Static attributes
                  stat_char   TYPE c LENGTH 3.

      CONSTANTS: const_num TYPE i VALUE 123.         "Non-changeable constant

    PROTECTED.
      "Here go more attributes.

    PRIVATE.
      "Here go more attributes.

ENDCLASS.

CLASS local_class IMPLEMENTATION.

      "Here go all method implementations.

ENDCLASS.
```

<p align="right">(<a href="#top">back to top</a>)</p>

**Methods**

-   Are internal
    [procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm "Glossary Entry")
    determining the behavior of the class.
-   Can access all of the attributes of a class and, if not defined
    otherwise, change their content.
-   Have a [parameter
    interface](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenparameter_interface_glosry.htm "Glossary Entry")
    (also known as
    [signature](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensignature_glosry.htm "Glossary Entry"))
    with which methods can get values when being called and pass values
    back to the caller.
-   [Static
    methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_method_glosry.htm "Glossary Entry")
    can only access static attributes of a class and trigger static
    events. You declare them using `CLASS-METHODS` statements in
    a visibility section.
-   [Instance
    methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_method_glosry.htm "Glossary Entry")
    can access all of the attributes of a class and trigger all events.
    You declare them using `CLASS` statements in a visibility
    section. Since instance methods are bound to instances, an instance
    of the class must first be created before using them.

**Parameter Interface**

In the simplest form, methods can have no parameter at all. Apart from that, methods can be defined with the following parameters:

| Addition  | Details  |
|---|---|
|`IMPORTING`|Defines one or multiple parameters that are imported (or input) by the method (e. g. from another object or an ABAP program) with which the method can work.  |
|`EXPORTING`|Defines one or multiple parameters that are exported (or output) by the method (e. g. to another object or an ABAP program).  |
|`CHANGING`|Defines one or multiple parameters that can be both imported and exported.  |
|`RETURNING`|Only one `RETURNING` parameter can be defined for methods. These methods are called functional methods. Like `EXPORTING` parameters, `RETURNING` parameters pass back values (note that the formal parameters of returning parameters must be passed by value), i. e. they are output parameters. The difference is that there can be multiple `EXPORTING` parameters in a method. However, `RETURNING` parameters simplify the syntax if there is only one value to be passed back. It shortens the method call and enables method chaining.  Furthermore, functional methods can, for example, be used in arithmetic or logical expressions. In case of standalone method calls, the returned value can be accessed using the addition `RECEIVING`.  |
|`RAISING` | Used to declare the class-based exceptions to handle errors.  |


> **💡 Note**<br>
> - You may find the addition `EXCEPTIONS` especially in definitions of older classes. They are for non-class-based exceptions. The exceptions are based on the system field `sy-subrc` and raised according to setting this field. You can then check the value of `sy-subrc` and act accordingly. This addition should not be used in ABAP for Cloud Development.
> - [Formal
    parameter](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenformal_parameter_glosry.htm "Glossary Entry")
    versus [actual
    parameter](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenactual_parameter_glosry.htm "Glossary Entry"):
    You define method parameters by specifying a name with a type which
    can be a
    [generic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengeneric_data_type_glosry.htm "Glossary Entry")
    or
    [complete](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomplete_data_type_glosry.htm "Glossary Entry")
    type. This formal parameter includes the specification of how the
    value passing should happen. Parameters can be [passed by
    reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpass_by_reference_glosry.htm "Glossary Entry")
    (`... REFERENCE(param) ...`; note that just specifying the
    parameter name `... param ...` - as a shorter syntax -
    means passing by reference by default) or [by
    value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpass_by_value_glosry.htm "Glossary Entry")
    (`... VALUE(param) ...`). The actual parameter represents
    the data object whose content is passed to or copied from a formal
    parameter as an argument when a procedure is called. If
    pass-by-reference is used, a local data object is not created for
    the actual parameter. Instead, the procedure is given a
    [reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_glosry.htm "Glossary Entry")
    to the actual parameter during the call and works with the actual
    parameter itself. Note that parameters that are input and passed by
    reference cannot be modified in the procedure. However, the use of a
    reference is beneficial regarding the performance compared to
    creating a local data object.
>-   Parameters can be defined as optional using the
    [`OPTIONAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_parameters&sap-language=EN&sap-client=000&version=X&anchor=!ABAP_ONE_ADD@1@&tree=X)
    addition. In doing so, it is not mandatory to pass an actual
    parameter. The
    [`DEFAULT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_parameters&sap-language=EN&sap-client=000&version=X&anchor=!ABAP_ONE_ADD@1@&tree=X)
    addition also makes the passing of an actual parameter optional.
    However, when using this addition, as the name implies, a default
    value is set.

<p align="right">(<a href="#top">back to top</a>)</p>

**Constructors**

-   [Constructors](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_glosry.htm "Glossary Entry")
    are special methods that are usually used for setting a defined
    initial state of objects, e. g. for setting a particular starting
    value for attributes in an object.
-   A class can only have one [instance
    constructor](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninstance_constructor_glosry.htm "Glossary Entry")
    and one [static
    constructor](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_constructor_glosry.htm "Glossary Entry").
-   Constructors always exist implicitly in classes. However, their
    declaration and use is optional. If they are declared explicitly,
    they must consequently be implemented. Note that they are always
    called automatically even if not declared and implemented.
-   Static constructor: Automatically called when calling a class for
    the first time in an internal session. This constructor is declared
    using the predefined name `class_constructor` as part of a
    `CLASS-METHODS` statement in the public visibility section.
    Static constructors cannot have any parameters.
-   Instance constructor: Automatically called when a class is
    instantiated and an object is created. The constructor is declared
    using the predefined name `constructor` as part of a
    `METHODS` statement. In contrast to local classes, instance
    constructors must be declared in the public visibility section of
    global classes. They can only have `IMPORTING` parameters
    and exceptions. In case of exceptions, make sure that you use a
    [`TRY ... ENDTRY.`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptry.htm)
    block in the implementation since otherwise the instance is not
    created if uncaught errors occur.

Example for method definitions: The following snippet shows
multiple method definitions in the public section. Most of the formal
parameters of the demo methods below are defined by just using the
parameter name. This means passing by reference (returning parameters
require to be passed by value).
``` abap
CLASS local_class DEFINITION.
    PUBLIC.
      METHODS: inst_meth1 IMPORTING a TYPE i                    "instance methods
                          EXPORTING b TYPE i,

               inst_meth2 IMPORTING c TYPE string

               inst_meth2 IMPORTING d TYPE string
                          RETURNING VALUE(e) TYPE string,

               inst_meth3 IMPORTING f TYPE i
                          EXPORTING g TYPE i
                          CHANGING  h TYPE string_table
                          RETURNING VALUE(i) TYPE i
                          RAISING   cx_sy_zerodivide,

              constructor IMPORTING j TYPE i.                   "instance constructor with importing parameter

      CLASS-METHODS: stat_meth1 IMPORTING k TYPE i              "static methods
                                EXPORTING l TYPE i,
                     stat_meth2,                                "no formal parameters
                     class_constructor,                         "static constructor

                     "Formal parameter definitions
                     stat_meth3 IMPORTING VALUE(m) TYPE i       "pass by value
                                          REFERENCE(n) TYPE i   "pass by reference
                                          o TYPE i,             "same as n

                     "OPTIONAL/DEFAULT additions
                     stat_meth4 IMPORTING p TYPE i DEFAULT 123
                                          q TYPE i OPTIONAL.

ENDCLASS.

CLASS local_class IMPLEMENTATION.
   METHOD inst_meth1.
      ...                         "Here goes the method implementation.
   ENDMETHOD.

  ...                             "Note that all declared methods must be implemented.
ENDCLASS.
```

<p align="right">(<a href="#top">back to top</a>)</p>

## Working with Objects and Components

**Declaring reference variables**: To create an object, a
[reference
variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreference_variable_glosry.htm "Glossary Entry")
must be declared. Such an [object reference
variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_refer_variable_glosry.htm "Glossary Entry")
is also necessary for accessing objects and their components, i. e.
objects are not directly accessed but only via references that point to
those objects. This reference is stored in the reference variables.
``` abap
DATA: ref1 TYPE REF TO local_class,
      ref2 TYPE REF TO global_class,
      ref3 LIKE ref1.
```

**Creating objects**: You create an object by using the instance
operator
[`NEW`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_new.htm).
In doing so, a new instance of a class is created and the reference to
it is assigned to an object reference variable. The `#` sign
means that the type (`TYPE REF TO ...`) can be derived from the
context (in this case from the type of the reference variable). You can
also omit the explicit declaration of a reference variable by declaring
a new reference variable
[inline](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_inline.htm),
for example, using `DATA`. In this case, the name of the class
must be placed after `NEW` and before the first parenthesis. The `NEW` operator replaces the older
[`CREATE OBJECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcreate_object.htm)
statements.
``` abap
ref1 = NEW #( ).                   "Type derived from already declared ref1

DATA(ref2) = NEW local_class( ).   "Reference variable declared inline, explicit type
                                   "(class) specification

"Old syntax. Do not use.
"CREATE OBJECT ref3.               "Type derived from already declared ref3
```

**Assigning or copying reference variables**: To assign or copy
reference variables, use the [assignment
operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm "Glossary Entry")
`=`. In the example below, both reference variables have the same
type.

``` abap
DATA: ref1 TYPE REF TO local_class,
      ref2 TYPE REF TO local_class.

ref1 = NEW #( ).
ref2 = ref1.
```

**Overwriting reference variables**: An [object
reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_reference_glosry.htm "Glossary Entry")
is overwritten when a new object is created with a reference variable
already pointing to an instance.
``` abap
ref1 = NEW #( ).
ref1 = NEW #( ).
```

**Keeping object references in internal tables**: If your use case is to retain the object references, for example, if you create a series of objects and you want to prevent object references to be overwritten when using the same reference variable, you can put the reference variables in internal tables. The following code shows that three objects are created with the same reference variable. The internal table includes all object references and, thus, their values are retained.
``` abap
DATA: ref TYPE REF TO local_class,
      itab TYPE TABLE OF REF TO local_class.

DO 3 TIMES.
  ref = NEW #( ).
  itab = VALUE #( BASE itab ( ref ) ).   "Adding the reference to itab
ENDDO.
```
**Clearing object references**: Use
[`CLEAR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclear.htm)
statements to explicitly clear a reference variable.
```
CLEAR ref.
```
> **💡 Note**<br>
> Since objects use up space in the memory, they should be
cleared if they are no longer needed. The [garbage
collector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abengarbage_collector_glosry.htm "Glossary Entry")
takes over this task automatically, i. e. all objects without any
reference are cleared and the memory space is released.

**Accessing attributes**: Instance attributes are accessed using
the [object component
selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_component_select_glosry.htm "Glossary Entry")
`->` via a reference variable. Visible static attributes are
accessed using the [class component
selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_component_select_glosry.htm "Glossary Entry")
`=>` via the class name. You can also declare data objects and
types by referring to the static attributes.
``` abap
"Accessing instance attribute via a reference variable

... ref->some_attribute ...

"Accessing static attributes via the class name

... local_class=>static_attribute ...

"Without the class name only within the class
... static_attribute ...

"Type and data object declarations

TYPES some_type LIKE local_class=>some_attribute.
DATA dobj1      TYPE local_class=>some_type.
DATA dobj2      LIKE local_class=>some_attribute.
```

**Calling methods**: Similar to accessing attributes, instance
methods are called using `->` via a reference variable. Static
methods are called using `=>` via the class name. When used
within the class in which it is declared, the static method can also be
called without `class_name=>...`. You might also see method
calls with [`CALL METHOD`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcall_method_static.htm)
statements which are not used here (however, these statements are the
only option in the context of dynamic programming.
When methods are called, the parameters must be specified within the
parentheses.


Examples for instance method calls and static method calls:
``` abap
"Calling instance methods via reference variable

ref->inst_meth( ... ).

"Calling static methods via/without the class name

class_name=>stat_meth( ... ).

"Only within the program in which it is declared.
stat_meth( ... ).  

"Calling (static) methdod having no parameter

class_name=>stat_meth( ).

"Calling (static) methods having a single importing parameter:

"Note that in the method call, the caller exports values to the
"method having importing parameters defined; hence, the ABAP word
"EXPORTING is relevant. The three following method calls are the same

"Explicit use of EXPORTING.
class_name=>meth( EXPORTING a = b ).

"Only importing parameters in the signature: explicit EXPORTING not needed

class_name=>meth( a = b ).

"Only a single value must be passed: parameter name (a) and EXPORTING not needed

stat_meth( b ).

"Calling (static) methods having importing/exporting parameters
"Parameters must be specified if they are not marked as optional

class_name=>meth( EXPORTING a = b c = d     "a/c: importing parameters
                  IMPORTING e = f ).        "e: exporting parameter

"If f is not yet available, you could also declare it inline.

class_name=>meth( EXPORTING a = b c = d
                  IMPORTING e = DATA(f) ).  "f receives type of e

"Calling (static) methods having a changing parameter;
"should be reserved for changing an existing local variable and value

DATA h TYPE i VALUE 123.
class_name=>meth( CHANGING g = h ).

"Calling (static) methods having a returning parameter.
"Basically, they do the same as methods with exporting parameters
"but they are way more versatile and you save lines of code.

"They do not need temporary variables.
"In the example, the return value is stored in a variable declared inline.

"i and k are importing parameters
DATA(result) = class_name=>meth( i = j k = l )

"They can be used with other statements, e. g. logical expressions.
IF class_name=>meth( i = j k = l ) > 100.
  ...
ENDIF.

"They enable method chaining.
"The example shows a method to create random integer values.
"The methods have a returning parameter.

DATA(random_no) = cl_abap_random_int=>create( )->get_next( ).

"Receiving parameter: Available in methods defined with a returning parameter;
"used in standalone method calls only.
"In the snippet, m is the returning parameter; n stores the result.

class_name=>meth( EXPORTING i = j k = l RECEIVING m = DATA(n) ).
```

**Self-Reference me**

When implementing instance methods, you can make use of the implicitly available object reference variable `me` which is always available and points to the respective object itself. You can use it to refer to components of the instance of a particular class but it is not needed:
``` abap
... some_method( ... ) ...

... me->some_method( ... ) ...
```

However, if you want to access attributes of the particular class and these attributes have identical names as local attributes within the method, you can make use of `me` to access the attributes that are outside of the method and within the class. The following example demonstrates the use of `me` in a method implementation.

``` abap
METHOD me_ref.

  DATA str TYPE string VALUE `Local string`.

  DATA(local_string) = str.

  "Assuming there is a variable str declared in the class declaration part.
  DATA(other_string) = me->str.

ENDMETHOD.
```

<p align="right">(<a href="#top">back to top</a>)</p>

## Notes on Inheritance

-   Concept: Deriving a new class (i. e. a
    [subclass](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensubclass_glosry.htm "Glossary Entry"))
    from an existing one
    ([superclass](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensuperclass_glosry.htm "Glossary Entry"))
    to share common components between classes. In doing so, you create
    hierarchies of classes (an [inheritance
    tree](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninheritance_tree_glosry.htm "Glossary Entry"))
    while a superclass includes components that are shared by all
    subclasses to provide a better structure for your code.
-   Subclasses ...
    -   inherit all components from superclasses
    -   can access instance components from superclasses
    -   can be made more specific by declaring new components and
        redefining instance methods (i. e. replacing the implementations
        of inherited methods).
    -   can access static components but not redefine them.
    -   can only handle components in the `PROTECTED` and
        `PUBLIC` section of superclasses.
    -   know their direct superclass but they do not know which classes
        inherit from them. [Note:] Handle component definitions
        and implementations in the superclass with great care since a
        change might have undesired consequences for the subclasses.
-   Components ...
    -   that are changed and added to subclasses are not visible to
        superclasses, hence, these changes are only relevant for this
        class itself and its subclasses.
    -   that are added should have a different name than those of the
        superclass.
-   A class ...
    -   can only inherit from one superclass, i. e. a subclass can only
        have one superclass, however, a superclass can have any number
        of subclasses.
    -   must enable derivation, i. e. classes cannot inherit from
        classes that are specified with the addition
        [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_options&sap-language=EN&sap-client=000&version=X&anchor=!ABAP_ADDITION_4@4@&tree=X)
        (e. g. `CLASS global_class DEFINITION PUBLIC FINAL CREATE PUBLIC. ...`).

<p align="right">(<a href="#top">back to top</a>)</p>

**Excursion: `ABSTRACT` and `FINAL`**
-   A global class declared with the addition `FINAL` rules out
    inheritance. The addition `FINAL` is also available for
    [method
    declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_abstract_final&sap-language=EN&sap-client=000&version=X&anchor=!ABAP_ADDITION_2@2@&tree=X)
    in classes which rules out that such a method is redefined in
    subclasses. In classes that are declared with `FINAL`, all
    methods are implicitly final. Instance constructors are always final
    by default.
-   The addition
    [`ABSTRACT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_abstract_final.htm)
    is meant for abstract classes.
-   The addition `ABSTRACT` enters the picture in the context of
    abstract classes. These classes are used if you want to have a
    template for subclasses and you do not need instances of such
    classes. Instances are only possible for their subclasses. Instance
    components of an abstract class can then be accessed via an
    instantiated subclass. `ABSTRACT` is available for [class
    declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_options&sap-language=EN&sap-client=000&version=X&anchor=!ABAP_ADDITION_3@3@&tree=X)
    (e. g. `CLASS cl DEFINITION ABSTRACT. ...`) and [method
    declarations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_abstract_final&sap-language=EN&sap-client=000&version=X&anchor=!ABAP_ADDITION_1@1@&tree=X)
    (e. g. `METHODS meth ABSTRACT. ...`). Abstract methods can
    only be declared within abstract classes. They are not implemented
    in the implementation part of the abstract class. Instead, they must
    be redefined in subclasses. Note that in abstract classes,
    non-abstract methods can also be declared and that private methods
    cannot be redefined, i. e. methods in this section cannot be
    declared as abstract. See a high-level comparison of abstract
    classes and interfaces further down.

<p align="right">(<a href="#top">back to top</a>)</p>

**Redefining Methods**

-   The non-final method from the superclass that is redefined must be
    specified in the declaration part of the subclass as follows:
    `METHODS meth REDEFINITION.`

    It must be specified with the same method name and in the same
    visibility section as in the superclass. Specifying or changing the
    signature is not possible.

-   If you want to access the original method in the superclass within
    the method implementation of the subclass, use the [pseudo
    reference](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpseudo_reference_glosry.htm "Glossary Entry")
    `super->...`.

> **💡 Note**<br>
> If the instance constructor is implemented in a subclass, the instance constructor of the superclass must be called explicitly using `super->constructor`, even if the latter is not explicitly declared. An exception to this: Direct subclasses of the root node object.

<p align="right">(<a href="#top">back to top</a>)</p>

## Notes on Polymorphism and Casting

The object orientation concept
[polymorphism](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpolymorphism_glosry.htm "Glossary Entry")
means accessing different methods in different objects and with
different behavior via the same interface, i. e. you can use one and the
same reference variable to access various objects, for example,
references to a superclass can point to objects of a subclass.

Note the concept of static and dynamic type in this context:

-   Object reference variables (and also interface reference variables
    as outlined further down) have both a
    [static](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstatic_type_glosry.htm "Glossary Entry")
    and a [dynamic
    type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendynamic_type_glosry.htm "Glossary Entry").
-   When declaring an object reference variable, e. g. `DATA oref TYPE
    REF TO cl`, you determine the static type, i. e.
    `cl` is used to declare the reference variable that is
    statically defined in your program. The dynamic type is determined
    at runtime of the program and is the class of an object. Especially
    in the context of assigning object or interface references (and also
    [data
    references](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_reference_glosry.htm "Glossary Entry")),
    this differentiation enters the picture.
-   The following basic rule applies: The assignment of an object or
    interface reference variable to another one is possible if the
    static type of the target reference variable is more general than or
    the same as the dynamic type of the source reference variable.
-   If it can be statically checked that an assignment is possible
    although the types are different, the assignment is done using the
    [assignment
    operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenassignment_operator_glosry.htm "Glossary Entry")
    `=` that triggers an
    [upcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenup_cast_glosry.htm "Glossary Entry")
    automatically.
-   Otherwise, it is a
    [downcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendown_cast_glosry.htm "Glossary Entry").
    Here, the assignability is not checked until runtime. The downcast
    must be triggered explicitly using [casting
    operators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencasting_operator_glosry.htm "Glossary Entry"),
    either with the [constructor
    operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_operator_glosry.htm "Glossary Entry")
    [`CAST`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_cast.htm)
    or the older
    [`?=`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove_cast.htm),
    for the assignment of object or interface reference variables.
-   See more information in the topic [Assignment Rules for Reference
    Variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_references.htm).

> **✔️ Hints**<br>
> - If the static type is a class, the dynamic type must be the same class or one of its subclasses.
> - If the static type is an interface, the dynamic type must implement the interface.

As an example, assume there is an inheritance tree with `lcl_super` as the superclass and `lcl_sub` as a direct subclass. `lcl_sub2` is a direct subclass of `lcl_sub`.

In the following code snippet, the rule is met since the superclass is either the same as or more generic than the subclass (the subclass has, for example, redefined methods and is, thus, more specific). Hence, the assignment of an object reference variable pointing to the subclass to a variable pointing to a superclass works. An upcast is triggered. After this casting, the type of `oref_super` has changed and the methods of `lcl_sub` can be accessed via `oref_super`.

``` abap
oref_super = NEW lcl_super( ).

oref_sub = NEW lcl_sub( ).

"Upcast
oref_super = oref_sub.

"The casting might be done when creating the object.
DATA super_ref TYPE REF TO lcl_super.

super_ref = NEW lcl_sub( ).
```

Such upcasts frequently occur if you want to access objects via an
object reference variable pointing to the superclass. However, there
might also be situations when you want to do the assignment the other
way round, i. e. going from specific to more generic. In this case, a
more generic reference variable is assigned to a specific variable which
can be depicted as moving downwards in the inheritance tree concerning
the assignment. As mentioned above, a downcast must be triggered
manually. Just an assignment like `oref_sub = oref_super.`
does not work. A syntax error occurs saying the right-hand variable's
type cannot be converted to the left-hand variable's type.

If you indeed want to carry out this casting, you must use
`CAST` or `?=` to overcome this syntax error (but just
the syntax error!). Note: You might also use these two
operators for the upcasts. That means, `oref_super =
oref_sub.` has the same effect as `oref_super = CAST #(
oref_sub ).`. This syntax is usually not necessary.

At runtime, the assignment is checked and if the conversion does not
work, you face a (catchable) runtime error. Even more so, the assignment
`oref_sub ?= oref_super.` does not throw a syntax error but it
does not work in this example either because it violates the rule
mentioned above (`oref_sub` is more specific than
`oref_super`). To check whether such an assignment is possible
on specific classes, you can use the predicate expression [`IS INSTANCE
OF`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_instance_of.htm)
or the case distinction [`CASE TYPE
OF`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcase_type.htm).
Carrying out an upcast before the downcast ensures that the left-hand
variable's type is compatible to the right-hand variable's type.

``` abap
oref_super = NEW lcl_super( ).
oref_sub = NEW lcl_sub( ).
oref_sub2 = NEW lcl_sub2( ).

"Downcast resulting in an error; error is caught

TRY.
  oref_sub = CAST #( oref_super ).
  CATCH CX_SY_MOVE_CAST_ERROR INTO DATA(e).
    ...
ENDTRY.

"Working downcast with a prior upcast

oref_super = oref_sub2.

"Due to the prior upcast, the following check is actually not necessary.

IF oref_super IS INSTANCE OF lcl_sub.
  oref_sub = CAST #( oref_super ).
  ...
ENDIF.
```

<p align="right">(<a href="#top">back to top</a>)</p>

## Working with Interfaces

Interfaces ...

-   represent a template for the components in the public visibility
    section of classes and thus enhance the components of classes by
    adding components of interfaces.
-   represent a means to deal with multiple inheritance in ABAP Object
    Orientation.
-   serve the concept of
    [polymorphism](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpolymorphism_glosry.htm "Glossary Entry").
    Any number of classes can implement the same interface.
-   are beneficial if you want to share and reuse common components
    across classes especially if those classes are not in an inheritance
    relationship.
-   are possible as both
    [local](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlocal_interface_glosry.htm "Glossary Entry")
    and [global
    interfaces](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_interface_glosry.htm "Glossary Entry").
-   are different from classes in the following ways:
    -   They only consist of a part declaring the components without an
        implementation part. The classes using the interfaces are
        responsible for the implementation.
    -   They do not include visibility sections. All interface
        components are public.
    -   No instances can be created from interfaces.
    -   Declarations as mentioned for classes, e. g. `DATA`,
        `CLASS-DATA`, `METHODS`,
        `CLASS-METHODS`, are possible. Constructors are not
        possible.

<p align="right">(<a href="#top">back to top</a>)</p>

**Defining Interfaces**

> **💡 Note**<br>
> The addition `DEFINITION` is not relevant here since there is no implementation part.

``` abap
INTERFACE intf.
"The addition PUBLIC is for global interfaces:
"INTERFACE intf_g PUBLIC.

    DATA ...
    CLASS-DATA ...
    METHODS ...
    CLASS-METHODS ...

ENDINTERFACE.
```

<p align="right">(<a href="#top">back to top</a>)</p>

**Using Interfaces in Classes**

-   A class can implement multiple interfaces.
-   As a prerequisite, the interfaces must be specified in the
    declaration part of a class using the statement
    [`INTERFACES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinterfaces.htm).
-   Since all interface components are public, you must include this
    statement and the interfaces in the public section of a class.
-   In doing so, the interface components become part of the class
    itself. Methods that are specified in interfaces must be implemented
    in the class unless the methods are marked as optional in the
    interface using the additions [`DEFAULT
    IGNORE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_default.htm)
    or [`DEFAULT FAIL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_default.htm).
    Furthermore, you can specify the addition [`ABSTRACT
    METHODS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinterfaces_class.htm)
    for the `INTERFACES` statement in the declaration part of
    classes followed by method names. In this case, the class need not
    implement the methods of the interface. The implementation is then
    relevant for a subclass inheriting from a superclass that includes
    such an interface declaration. Find more information
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinterfaces_class.htm).
-   You can specify alias names for the interface components using the
    statement [`ALIASES ... FOR
    ...`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapaliases.htm).
    The components can then be addressed using the alias name everywhere
    (since the alias is public).

Syntax for using interfaces in classes:
``` abap
CLASS class DEFINITION.
  PUBLIC SECTION.
    INTERFACES intf1.
    INTERFACES intf2 ABSTRACT METHODS meth1.   "No implementation required for meth1
    INTERFACES intf3 ALL METHODS ABSTRACT.     "All methods abstract

    ALIASES meth_alias FOR intf1~some_method.
ENDCLASS.

CLASS class IMPLEMENTATION.
  METHOD intf1~some_meth.           "Method implementation using the original name
   ...
  ENDMETHOD.

  "Just for demo purposes: Method implementation using the alias name  
  "METHOD meth_alias.  
  " ...
  "ENDMETHOD.

  ...
ENDCLASS.
```

<p align="right">(<a href="#top">back to top</a>)</p>

**Accessing Interface Components**

-   Interface components can be addressed using the [interface component
    selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninterface_comp_selector_glosry.htm "Glossary Entry"):
    `... intf~comp ...`.
    -   Note: Due to the unique interface name and the use of
        the name as prefix (e. g. `intf~`) that is followed by
        a component name, there is no problem regarding the component
        naming within a class, i. e. the class can have components with
        the same name, and other interfaces can have components with the
        same name, too.
-   You can then access them either using class references or interface
    references:
    -   Class references: `... class_ref->intf~comp ...`
    -   Interface references: `... i_ref->comp ...`. In this
        case, the [object component
        selector](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_component_select_glosry.htm "Glossary Entry")
        is used to access the components without the interface name.

Before making use of interface references, an [interface reference
variable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninterface_ref_variable_glosry.htm "Glossary Entry")
must be created: `DATA i_ref TYPE REF TO intf.` Interfaces
cannot be instantiated, i. e. an object cannot be created, however,
interface references can point to the objects of any class that includes
the interface so that interface components (and only them) can be
accessed via the variable. Accessing an object via an interface
reference variable is basically the same as accessing a subclass object
via a superclass reference variable.

<p align="right">(<a href="#top">back to top</a>)</p>

**Assigning Interface Reference Variables**

As mentioned above, interface references can point to the objects of any
class that includes the interface. This is true when object references
are assigned to interface references. Here, as touched on before, the
concept of
[upcasting](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenup_cast_glosry.htm "Glossary Entry")
enters the picture.
``` abap
DATA i_ref TYPE REF TO intf.

DATA cl_ref TYPE REF TO class.

cl_ref = NEW #( ).

"Upcast
i_ref = cl_ref.

"Method call using the interface reference variable
i_ref->some_method( ... ).
```

The other way round, i. e. the assignment of an interface reference to
an object reference, is also possible
([downcast](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendown_cast_glosry.htm "Glossary Entry")
or narrowing cast). However, as mentioned before, this assignment can be
problematic since a successful assignment is dependent on whether the
object the interface reference points to is actually an object of the
implementing class. If this is not the case, a runtime error occurs. You
can carry out a downcast using the casting operator
[`CAST`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_cast.htm)
or the operator
[`?=`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmove_cast.htm).
The example shows the use of an [`IS INSTANCE
OF`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlogexp_instance_of.htm)
expression to prevent a runtime error as also shown before.

``` abap
DATA i_ref TYPE REF TO intf.

DATA cl_ref TYPE REF TO class.

cl_ref = NEW #( ).

IF i_ref IS INSTANCE OF class.
  cl_ref = CAST #( i_ref ).
...
ENDIF.
```

> **✔️ Hints**<br>
> Interfaces versus abstract classes<br>
> Coming back to
[abstract](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabstract_glosry.htm "Glossary Entry")
classes in the context of interfaces. Like interfaces, abstract classes
cannot be instantiated - only their subclasses. The following list
includes differences between abstract classes and interfaces that should
be considered when creating them:
>-   Abstract classes can have components (including abstract and
    non-abstract methods) in other visibility sections than only public.
>-   Multiple inheritance is impossible with abstract classes since there
    can be only one abstract class as superclass.
>-   Non-abstract methods in abstract classes can be implemented whereas
    interfaces do not allow any method implementations.

<p align="right">(<a href="#top">back to top</a>)</p>

## Further Concepts

### Factory Methods

A factory method is relevant if you want to restrict and control the
instantiation of a class by external users of this class. Still, the
users should be able to work with objects of the class. This is true for
cases when, for example, there must be only a single object of a class
(a singleton) or certain checks must be carried out before a class can
be instantiated so as to guarantee a consistent creation of all objects.

A (static) factory method implemented in such a class does the trick: It
creates an object of the class and returns a reference to the object.

Example for a class and factory method:
``` abap
"Addition CREATE PRIVATE
CLASS class DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
  CLASS-METHODS factory_method
         IMPORTING ...
         RETRUNING VALUE(obj) TYPE REF TO class. "Returns an object

ENDCLASS.
...

"Calling a factory method.
DATA obj_factory TYPE REF TO class.

obj_factory = class=>factory_method( ... ).
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Friendship

Classes can grant friendship to other classes and interfaces to enable
the access to protected and private components. However, the friendship
is not reciprocal. If class `a` grants friendship to class `b`, class `b` must
also explicitly grant friendship to class `a` if the component should be
made accessible also the other way round.

Friends of a class can create instances of the class without any
restrictions. They are not automatically made friends of the subclasses
of the class.

Friendship prevents that the components are made available to all users.
A typical use case for friendship between classes is [unit
tests](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenunit_test_glosry.htm "Glossary Entry"),
i. e. friendship is granted to test classes so that they can access and
test private components, too.

You specify the befriended class in the definition part:
``` abap
CLASS class DEFINITION FRIENDS other_class.
...
```

<p align="right">(<a href="#top">back to top</a>)</p>

### Events

[Events](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenevent_glosry.htm "Glossary Entry")
are components of classes that can be triggered by methods. If an event
is raised (by a [`RAISE EVENT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapraise_event.htm)
statement), specific [event handler
methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenevent_handler_glosry.htm "Glossary Entry")
are called to react on the event. The following points are relevant for
raising events:

-   Defining events
    -   Events must be defined in a visibility section of the
        declaration part of a class or in an interface, e. g. as
        instance (using an
        [`EVENTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapevents.htm)
        statement) or static events
        ([`CLASS-EVENTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass-events.htm)).
    -   Similar to methods, static events can be triggered by instance
        and static methods, instance events can only be triggered by
        instance methods.
    -   Events allow exporting parameters to be defined. They must be
        passed by value. Each instance event also includes the implicit
        output parameter `sender` representing an object
        reference variable for the instance for which the event is
        defined.
-   Defining and implementing [event handler
    methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenevent_handler_glosry.htm "Glossary Entry").
    These methods are defined with a special syntax:

``` abap
CLASS-METHODS handler_meth FOR EVENT evt OF class
...
```

-   Registering event handler methods

    -   Event handler methods must be registered using a [`SET
        HANDLER`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapset_handler.htm)
        statement at runtime so that they can handle a raised event at
        all and react accordingly.
    -   You can register instance events for a specific instance or for
        all instances of a class. Static events are registered to the
        whole class without any addition to the `SET HANDLER`
        statement.

```abap
SET HANDLER handler_meth FOR ref.              "Specific instance

SET HANDLER handler_meth FOR ALL INSTANCES.    "All instances

SET HANDLER handler_meth.                      "For static events
```

<p align="right">(<a href="#top">back to top</a>)</p>

## Executable Example
[zcl_demo_abap_objects](./src/zcl_demo_abap_objects.clas.abap)

Note the steps outlined [here](README.md#🎬-getting-started-with-the-examples) about how to import and run the code.
