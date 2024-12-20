<a name="top"></a>

# Exceptions and Runtime Errors

- [Exceptions and Runtime Errors](#exceptions-and-runtime-errors)
  - [Exceptions](#exceptions)
  - [Exception Classes](#exception-classes)
    - [Exception Categories](#exception-categories)
    - [Components of Exception Classes](#components-of-exception-classes)
    - [Raising Class-Based Exceptions](#raising-class-based-exceptions)
    - [Handling Exceptions Using TRY Control Structures](#handling-exceptions-using-try-control-structures)
      - [CATCH ... INTO ...: Storing Exception Objects and Evaluation](#catch--into--storing-exception-objects-and-evaluation)
      - [CLEANUP Statements](#cleanup-statements)
      - [RETRY Statements](#retry-statements)
      - [RESUME Statements and RESUMABLE Additions](#resume-statements-and-resumable-additions)
  - [Using Messages as Exception Texts](#using-messages-as-exception-texts)
    - [Excursion: MESSAGE Statements](#excursion-message-statements)
  - [Syntax Variants of RAISE EXCEPTION/THROW](#syntax-variants-of-raise-exceptionthrow)
  - [Runtime Errors](#runtime-errors)
    - [Programmatically Raising Runtime Erros](#programmatically-raising-runtime-erros)
    - [Assertions](#assertions)
  - [Excursions](#excursions)
    - [Local Exception Classes](#local-exception-classes)
    - [Messages in RAP](#messages-in-rap)
    - [Violations of ABAP Contract Checks Causing Runtime Errors](#violations-of-abap-contract-checks-causing-runtime-errors)
    - [Classic Exceptions](#classic-exceptions)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)


This cheat sheet includes an overview about syntax in the context of exceptions and runtime errors in ABAP.

> **💡 Note**<br>
> Several code snippets in the cheat sheet use artifacts from the [executable example](#executable-example).

## Exceptions

[Exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenexception_glosry.htm) ...
- are error situations that occur during the execution of an ABAP program and interrupt the program flow. You can implement exception handling to appropriately react to these situations. Consider, for example, the implementation of a simple calculation. If there is a division by zero, the program will be terminated with a [runtime error](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenruntime_error_glosry.htm) unless you handle the exception appropriately.

  ```abap
  "The following statement raises an exception because of zero division. 
  "Since the exception is not handled, a runtime error occurs.
  DATA(div_result) = 1 / 0.
  ```

- can be raised programmatically or by the [ABAP runtime framework](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_runtime_frmwk_glosry.htm), typically from errors not detected by static program checks. The division by zero is such an example.
- should, in modern ABAP, only be designed as [class-based exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_based_exception_glosry.htm). Exceptions are represented by [objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_glosry.htm) of classes, i.e. instances of exception classes. Global and local exception classes are possible, with global classes usually using the naming convention `[...]CX_...`.
- are either [catchable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencatchable_exception_glosry.htm) (they are based on predefined or self-defined exception classes) or [uncatchable](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenuncatchable_exception_glosry.htm) (they directly produce runtime errors, i. e. error situations cannot be handled appropriately).


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Exception Classes

Exception classes ...
- are special classes that form the basis of catchable exceptions. 
  - When an exception is raised, an object of an exception class is created, making exceptions instances of these classes.
  - Using components of raised exception classes, you can retrieve and evaluate information on the exception.
- are available as predefined and globally available exception classes for exceptions of the ABAP runtime framework, typically following the the naming convention `CX_...` instead of `CL_...` to distinguish them from *regular* classes (e.g. `CX_SY_ZERODIVIDE` for zero division). 
- can be self-defined as global or local exception classes (typically following the naming conventions `ZCX_...`/`YCX_...` or `LCX_...` ) to react on issues that are specific to your ABAP program.
- are direct or indirect subclasses of the [abstract](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabstract_glosry.htm) [superclasses](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensuperclass_glosry.htm):
  - `CX_STATIC_CHECK`
  - `CX_DYNAMIC_CHECK`
  - `CX_NO_CHECK`
  - They represent different exception categories and are themselves subclasses of the abstract superclass `CX_ROOT`.
- are typically specified in signatures of [procedures](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprocedure_glosry.htm) such as methods of classes using the `RAISING` addition. 
  ```abap
  METHODS divide
    IMPORTING num1              TYPE i
              num2              TYPE i
    RETURNING VALUE(div_result) TYPE decfloat34
    RAISING   cx_sy_zerodivide.
  ```

> **💡 Note**<br>
> - Non-class-based exceptions are obsolete and should no longer be used in new developments. See the [guidelines (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclass_exception_guidl.htm).
> -  Unhandled exceptions raised by the ABAP runtime environment trigger a corresponding runtime error. For example, the exception class `CX_SY_ZERODIVIDE` causes the runtime error `COMPUTE_INT_ZERODIVIDE`. For self-defined exception classes, unhandled exceptions generally trigger the runtime error `UNCAUGHT_EXCEPTION`.


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Exception Categories

Exception classes are subclasses of three abstract classes, which in turn are subclasses of the root class `CX_ROOT`:

```
CX_ROOT
  |
  |--CX_STATIC_CHECK
  |
  |--CX_DYNAMIC_CHECK
  |
  |--CX_NO_CHECK
```

Notes on the superclasses of exception classes:

<table>

<tr>
<td> Superclass of Exception Classes </td> <td> Notes </td>
</tr>

<tr>
<td> 

`CX_STATIC_CHECK`

 </td>

 <td> 

- Users must handle exceptions.
- Exceptions that may occur in procedures should be handled locally within the implementation or explicitly declared in the [procedure interface](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenparameter_interface_glosry.htm) so callers know what errors to expect.
- Exception classes of type `CX_STATIC_CHECK` enforce this by performing a static check at compile time. Procedure users must handle the exception locally in a `TRY` control structure or declare it in their procedure interface to propagate the exception. If not, a warning is produced.

Example:

- A method signature might look like this. The `RAISING` addition in method signatures declares one or more class-based exceptions that can be propagated to the caller.
- When users implement the method, they are aware that an error might occur, and a specific exception can be raised.
- `CX_UUID_ERROR` is a predefined exception class derived from `CX_STATIC_CHECK`. Method users should prepare their code accordingly.
  ```abap
  "Method definition using the RAISING parameter
   CLASS-METHODS get_uuid
      RETURNING VALUE(uuid) TYPE sysuuid_x16
      RAISING   cx_uuid_error.

  ...
 
  "Method implementation
  METHOD get_uuid.
    uuid = cl_system_uuid=>create_uuid_x16_static( ) .
  ENDMETHOD.

  ...
  
  "Method call: Somewhere in the code of a user that calls the method
  "Exception handled locally in a TRY control structure
   TRY.
      DATA(uuid) = get_uuid( ).

      CATCH cx_uuid_error.
        ...
    ENDTRY.

  "If the statement is specified without the TRY control structure, 
  "a warning is shown.
  DATA(uuid2) = get_uuid( ).
  ```

 </td>
</tr>

<tr>
<td> 

`CX_DYNAMIC_CHECK`

 </td>

 <td> 


- For exceptions that can be checked and avoided by preconditions:
- Unlike exception classes derived from `CX_STATIC_CHECK`, exception classes derived from `CX_DYNAMIC_CHECK` do not enforce local handling or declaration in procedure interfaces.
- However, proper exception handling is necessary when you cannot prevent the exceptions from being raised in your program logic.
- Runtime checks verify whether local handling or explicit declaration in procedure interfaces are available only if the exception is raised.
- If, at runtime, such an exception is neither locally handled nor properly declared in an interface and the exception is raised, a new exception of type `CX_SY_NO_HANDLER` is raised, with the `PREVIOUS` attribute referencing the original exception.

Example:
- The predefined class `CX_SY_ZERODIVIDE` is derived from `CX_SY_ARITHMETIC_ERROR`, which is derived from `CX_DYNAMIC_CHECK`.
- Operands in a calculation should be checked (e.g., ensuring the second operand is not 0 in a division) before performing the arithmetic operation to avoid exceptions.

<br>


  ```abap
  "Method definition using the RAISING parameter
  CLASS-METHODS divide
    IMPORTING num1              TYPE i
              num2              TYPE i
    RETURNING VALUE(div_result) TYPE decfloat34
    RAISING   cx_sy_zerodivide.

  ...
 
  "Method implementation
  METHOD divide.
    div_result = num1 / num2.
  ENDMETHOD.
 
  ...
  
  "Method call: Somewhere in the code of a user that calls the method
  "Unlike procedures specifying an exception class derived from CX_STATIC_CHECK,
  "procedures specifying an exception class derived from CX_DYNAMIC_CHECK do not
  "enforce the local handling of exceptions. So, the following statement does
  "not show a warning.
  DATA(div_result1) = divide( num1 = 5 num2 = 2 ).

  "Exception handled locally in a TRY control structure
  TRY.
      DATA(div_result2) = divide( num1 = 5 num2 = 0 ).
    CATCH cx_sy_zerodivide.
  ENDTRY.
  ```

 </td>
</tr>

<tr>
<td> 

`CX_NO_CHECK`

 </td>

 <td> 

- For errors that may occur anytime, cannot be handled locally in a meaningful way, or cannot be avoided even after a check.
- An example of such an error is a lack of memory. If such exceptions were checked statically or dynamically, it would require specification in each procedure interface, which is not ideal for clear program structure.
- Note that exceptions derived from `CX_NO_CHECK` are always implicitly declared in all procedure interfaces.

 </td>
</tr>

</table>

> **💡 Note**<br>
> - Basic rule: [Use a suitable exception category (F1 docu for standard ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexception_category_guidl.htm).
> - Directly deriving from `CX_ROOT` is not possible.
> - As covered in the following sections, exception classes have specific components so that exceptions can be evaluated. 


<p align="right"><a href="#top">⬆️ back to top</a></p>


### Components of Exception Classes

- The following list covers a selection of exception class components relevant, for example, to evaluate exceptions raised using exception objects.
- Exception classes include instance methods because of inheriting from the root class `CX_ROOT`:
  - `get_text`: Returns the exception text 
  - `get_source_position`: Returns the program name, the name of a possible [include program](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeninclude_program_glosry.htm), and the line number of the statement that raised the exception.
- Additionally, instance attributes are available:
  - `textid`: Key for the database table `T100` used for exception texts (retrievable using `get_text`); usually set by the constructor 
  - `previous`: Reference to a previous exception; the type is a reference to `CX_ROOT`; also usually set by the constructor
  - `is_resumable`: Flag for [resumable exceptions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenresumable_exception_glosry.htm); indicates whether the exception can be resumed and leave a `CATCH BEFORE UNWIND` block
  
> **💡 Note**<br>
> - Usually, instances of exception classes are created when exceptions are raised. However, instances can also be created programmatically, e.g., with the `NEW` operator (if the classes are not abstract).
> - It is possible to define additional methods and attributes in exception classes, for example, for passing more information about error situations to handlers. Custom attributes should be defined as `READ-ONLY`.
> - Find examples in the section below, covering `CATCH ... INTO ...`.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Raising Class-Based Exceptions

- Class-based exceptions are raised in two ways:
  - The ABAP runtime framework raises predefined exceptions. 
    - Taking zero division as an example, not handling the class-based exception `CX_SY_ZERODIVIDE` causes the ABAP runtime framework to raise an exception and cause a runtime error. 
      ```abap      
      DATA(div_result) = 1 / 0.
      ```
  - You raise exceptions programmatically using dedicated statements. Both predefined and self-defined exceptions can be raised programmatically.
    - [`RAISE EXCEPTION`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapraise_exception_class.htm) statements raise class-based exceptions and thus interrupt the execution of the current statement block.
    - The `COND` and `SWITCH` operators include the optional addition `THROW` to raise class-based exceptions.
    - Same as above, if these exceptions raised are not handled, a runtime error occurs.

Syntax examples for raising exceptions programmatically:

> **💡 Note**<br>
> More variants of the statements shown are possible. They are covered in a [separate section below](#syntax-variants-of-raise-exceptionthrow) because they relate to topics covered in the following sections.

```abap
"-------------------------------------------------------------------
"----------------- RAISE EXCEPTION statements ----------------------
"-------------------------------------------------------------------

"RAISE EXCEPTION statement with the TYPE addition, specifying
"the name of a visible exception class; an exception
"object is created (if necessary, see the ABAP Keyword Documentation
"for more details)
RAISE EXCEPTION TYPE cx_sy_zerodivide.

"RAISE EXCEPTION statement specifying an exception object (an object
"reference variable pointing to an exception class)
DATA(exc) = NEW cx_sy_zerodivide( ).
RAISE EXCEPTION exc.

"Creating an exception object inline using the NEW operator
RAISE EXCEPTION NEW cx_sy_zerodivide( ).

"Note: Instances of abstract classes cannot be created. So, the
"following statements are not possible.
"RAISE EXCEPTION NEW cx_sy_arithmetic_error( ).
"RAISE EXCEPTION NEW cx_static_check( ).
"RAISE EXCEPTION NEW cx_dynamic_check( ).
"RAISE EXCEPTION NEW cx_no_check( ).
"RAISE EXCEPTION NEW cx_root( ).

"Dynamic creation of an exception object with CREATE OBJECT
DATA dyn_exc TYPE REF TO cx_root.
CREATE OBJECT dyn_exc TYPE ('CX_SY_ZERODIVIDE').
RAISE EXCEPTION dyn_exc.

"-------------------------------------------------------------------
"----------- COND/SWITCH operators with THROW addition -------------
"-------------------------------------------------------------------

"THROW addition in conditional expressions with the COND and SWITCH operators
"enabling raising class-based exceptions in operand positions
"The addition works like RAISE EXCEPTION TYPE statements.

"COND operator
DATA(int1) = 1.
DATA(int2) = 0.
"The statement considers ABAP "allowing" zero division when both operands are 0.
DATA(res1) = COND decfloat34( WHEN ( int1 <> 0 AND int2 <> 0 ) OR ( int1 = 0 AND int2 <> 0 ) THEN int1 / int2
                              ELSE THROW cx_sy_zerodivide( ) ).

"SWITCH operator
"The following example shows SWITCH with the THROW addition
"and uses cx_sy_zerodivide for demo purposes.
DO 5 TIMES.
  DATA(num) = SWITCH #( sy-index
                        WHEN 1 THEN `one`
                        WHEN 2 THEN `two`
                        WHEN 3 THEN `three`
                        WHEN 4 THEN `four`
                        ELSE THROW cx_sy_zerodivide( ) ).
ENDDO.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Handling Exceptions Using TRY Control Structures 

- Exceptions can be handled locally using [`TRY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptry.htm) control structures
- To be prepared for potential exceptions that are raised when executing statements, statements can be included and executed within such a `TRY` control structure representing a *protected area*.
- In doing so, it is possible for the ABAP runtime framework to catch exceptions, and you can react on error situations.
- A `TRY` control structure is initiated with `TRY` and ended with `ENDTRY`. The statements expect a [`CATCH`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcatch_try.htm) block, otherwise a syntax warning occurs.
  ```abap
  TRY.
      ...
      "Statement block
    CATCH ...
      ... "Statements  
  ENDTRY.
  ```

- One or more class-based exceptions can be handled in one or more subsequent `CATCH` blocks. 
- The `CATCH` statement should include a class-based exception suitable for the error handling. 

  ```abap
  TRY.    
    DATA(div1) = 1 / 0.

    "Predefined exception class cx_sy_zerodivide as suitable exception class to be used here.
    "If the exception is not handled, the program is terminated and the runtime error
    "COMPUTE_INT_ZERODIVIDE occurs.
    CATCH cx_sy_zerodivide.
      ... "CATCH block
  ENDTRY.

  "Example for catching an exception in the context of a table expression
  TRY.
    "Copying a line of an internal table
    DATA(line) = some_itab[ 12345 ].

    "Predefined exception class cx_sy_itab_line_not_found as suitable exception class to be used here.
    "If the exception is not handled, the program is terminated and the runtime error
    "ITAB_LINE_NOT_FOUND occurs.
    CATCH cx_sy_itab_line_not_found.
      ... "CATCH block
  ENDTRY.

  "Multiple classes in a list and CATCH blocks can be specified
  "Note: If there are multiple CATCH blocks for exceptions that are in an inheritance
  "relationship, you must pay attention that the more special exceptions are specified
  "before the more general ones.
  TRY.
      ... "TRY block
    CATCH cx_abc cx_bla cx_blabla.
      ... "CATCH block
    CATCH cx_la cx_lala.
      ... "CATCH block
    CATCH cx_lalala.
      ... "CATCH block
  ENDTRY.
  ```

- Note the inheritance relationships in exception classes. For example, open the exception class `CX_SY_ZERODIVIDE` to check that it inherits from `CX_SY_ARITHMETIC_ERROR`, which is derived from `CX_DYNAMIC_CHECK`.
- Specifying the exception root class `CX_ROOT` after `CATCH` is possible to catch all exceptions. However, it is advisable to choose specific exception classes to clarify which exceptions to expect.

  ```abap
    TRY.
      "TRY block
      DATA(div2) = 1 / 0.

      "A CATCH block is in this example not only valid for cx_sy_zerodivide as specified above
      "but also for all derived exceptions classes.
      "In the following CATCH block, the predefined exception class cx_sy_arithmetic_error
      "is specified. cx_sy_zerodivide is derived from cx_sy_arithmetic_error.
      "Hence, cx_sy_arithmetic_error can be specified and handle the exception, too.
      CATCH cx_sy_arithmetic_error.
        ... "CATCH block
    ENDTRY.

    "Example demonstrating the exception root class cx_root specifying after CATCH to 
    "catch all catchable exception.
     DO 3 TIMES.
      TRY.
          CASE sy-index.
            WHEN 1.
              RAISE EXCEPTION TYPE cx_sy_zerodivide.
            WHEN 2.
              RAISE EXCEPTION TYPE cx_uuid_error.
            WHEN 3.
              RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
          ENDCASE.
        CATCH cx_root.
        "Instead of explicit specification of potential exception classes involved
        "CATCH cx_sy_zerodivide cx_uuid_error cx_sy_itab_line_not_found.
      ENDTRY.
    ENDDO.
  ```

- Regarding the program flow:
  - The statement block following `TRY` is always executed. If an exception is raised within this `TRY` block, the system searches for a `CATCH` block that can handle the exception.
  - If there is no `CATCH` block that can handle the exception, or if the code is not within a `TRY` structure, the exception is propagated to the caller.
    - Exceptions can be handled locally using a `TRY` structure or be propagated to the caller, making the caller responsible for handling the error. This approach allows for better code structure by managing errors centrally rather than checking each procedure call individually.   
  - If the exception cannot be caught and handled, the program terminates with a runtime error.
  - If the exception is handled or no exception is raised, processing continues after `ENDTRY`.
  - Options for how to continue after an exception is raised and caught may include ignoring the error, correcting and retrying, evaluating the error situation (see notes on the `INTO` addition to the `CATCH` statement), showing an error message to the user, logging the error, etc.

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### CATCH ... INTO ...: Storing Exception Objects and Evaluation

- If you specify `INTO` in the `CATCH` statement, it stores a reference to the exception object.
- This helps to determine and evaluate the specific exception.
- You can specify ...
  - an existing object reference variable with a suitable static type. You can use a reference to the root class `CX_ROOT` to cover all possible exception objects.
  - a data object created inline using `DATA` or `FINAL`, which automatically receives a suitable type.


Example:
- In the code snippet above, the exception class `CX_SY_ZERODIVIDE` is used. Consider a calculator. It should not only handle errors like zero division, but also arithmetic overflows. The predefined exception class `CX_SY_ARITHMETIC_OVERFLOW` is available. It is derived from `CX_SY_ARITHMETIC_ERROR`. If you specify the exception class `CX_SY_ARITHMETIC_ERROR`, which is higher in the inheritance hierarchy and can handle both error situations (`CX_SY_ARITHMETIC_OVERFLOW` and `CX_SY_ZERODIVIDE`), the specific exception raised becomes unclear.
- Using the `INTO` clause and the stored exception object, you can perform tasks like retrieving and displaying the exception text.
- Many code snippets in the cheat sheet use [Runtime Type Identification (RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm). Find more information in the [Dynamic Programming cheat sheet](06_Dynamic_Programming.md).


```abap
"Note the specification of the root class cx_root
DATA exception TYPE REF TO cx_root. 

TRY.
    ... "TRY block

  "Storing a reference to the exception object.
  "Note: The type is cx_root since attributes and methods of the root class that are defined there can be accessed.
  CATCH INTO exception.
    ... "CATCH block
ENDTRY.

"Inline creation of exception object reference and getting exception texts
TRY.
    ... "TRY block

  "The object reference variable can be created inline, for example, using DATA(...).
  CATCH cx_sy_arithmetic_error INTO DATA(error_oref).
    ... "catch block

ENDTRY.

*******************************************************************************************************************
"Copyable example
DO 2 TIMES.
  TRY.
      IF sy-index = 1.
        DATA(div) = CONV decfloat34( 1 / 0 ).
      ELSE.
        DATA(powers) = ipow( base = 10 exp = 100 ).
      ENDIF.
    CATCH cx_sy_arithmetic_error INTO DATA(error_arithm).
      DATA(text) = error_arithm->get_text( ).
      "Using RTTI to retrieve the relative name of the class
      DATA(cl_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( 
            error_arithm ) )->get_relative_name( ).
      
      "Results:
      "---- sy-index = 1 ----
      "text: "Division by zero"
      "cl_name = CX_SY_ZERODIVIDE
      "---- sy-index = 2 ----
      "text: Overflow in the operation IPOW
      "cl_name = CX_SY_ARITHMETIC_OVERFLOW
  ENDTRY.
ENDDO.
```

Examples for evaluating exception information accessible via components of the exception object specified after `INTO`. See [Components of Exception Classes](#components-of-exception-classes).

```abap
TRY.
    DATA(div_result) = CONV decfloat34( 1 / 0 ).
  CATCH cx_sy_zerodivide INTO DATA(error).
    "Getting short text
    DATA(text) = error->get_text( ).

    "Getting source code position of where the error occured
    error->get_source_position(
      IMPORTING
        program_name = DATA(program_name)
        include_name = DATA(include_name)
        source_line  = DATA(source_line)
    ).
ENDTRY.
```

Demonstrating the instance attribute `previous`:

```abap
"Demonstrating the instance attribute 'previous' with
"nested TRY control structures.
"Consider the following scenario: A method is called and
"an exception is raised. Once evaluated, another exception
"is raised, and the previous exception (i.e. a reference to
"the previous exception object) is passed on to the caller.
"The caller can then evaluate the exceptions raised.
"The following example includes nested TRY control structures.
"In CATCH blocks, further exceptions are raised. There, the
"exception object is passed. In a WHILE loop, several pieces
"of information are retrieved, including the use of RTTI. 
"Once the information has been retrieved, the previous attribute
"is used to assign the previous exception object. Note the cast.

DATA info TYPE string_table.

TRY.
    TRY.
        TRY.
            RAISE EXCEPTION NEW cx_sy_zerodivide( ).
          CATCH cx_sy_zerodivide INTO DATA(err1).
            RAISE EXCEPTION NEW cx_sy_arithmetic_overflow( previous = err1 ).
        ENDTRY.
      CATCH cx_sy_arithmetic_overflow INTO DATA(err2).
        RAISE EXCEPTION NEW cx_sy_itab_line_not_found( previous = err2 ).
    ENDTRY.
  CATCH cx_sy_itab_line_not_found INTO DATA(err3).
ENDTRY.

DATA(acc_err) = CAST cx_root( err3 ).
WHILE acc_err IS BOUND.
  DATA(txt) = acc_err->get_text( ).

  acc_err->get_source_position(
    IMPORTING
      program_name = DATA(prog_name)
      include_name = DATA(incl_name)
      source_line  = DATA(src_line)
  ).

  "Using RTTI to gather more information on the exception
  "See the Dynamic Programming cheat sheet about RTTI.
  DATA(tdo) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( acc_err ) ).
  DATA(cl_relative_name) = tdo->get_relative_name( ).
  DATA(attributes) = tdo->attributes.
  DATA(superclass) = tdo->get_super_class_type( )->get_relative_name( ).

  "Populating info table
  APPEND repeat( val = `-` occ = 110 ) TO info.
  APPEND |Text: { txt }| TO info.
  APPEND |Position of raised exception: { prog_name } / { incl_name } / { src_line }| TO info.
  APPEND |Exception class name: { cl_relative_name }| TO info.
  APPEND |Superclass of exception class: { superclass }| TO info.

  "Using the previous attribute to assign the previous exception
  "object
  acc_err = acc_err->previous.
  "If there was no cast to cx_root, a cast would be required here,
  "for example, as follows.
  "acc_err = CAST #( acc_err->previous ).
ENDWHILE.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### CLEANUP Statements

- `CLEANUP` statements introduce statement blocks within a `TRY` control structure.
- They include implementations to create consistent states before further processing.
- The blocks' implementations are executed when an exception is raised, but the exception is handled externally, not within the same `TRY` structure.
  - See notes in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcleanup.htm) for details on executing the `CLEANUP` blocks.
- `CLEANUP` blocks must be executed completely. Statements leaving the block (e.g., using `RETURN`) are not allowed, and any raised exceptions must be handled within the block.
- The `INTO` addition can optionally store a reference to the exception object.

Examples:
```abap
"The following example shows nested TRY control structures.
"The inner TRY control structure includes a CLEANUP block.
"The example demonstrates that an exception raised cannot
"be handled by the TRY control structure where it is raised.
"No meaningful 'cleanup' action is performed in the example's
"CLEANUP block. For demonstration purposes, an info table is 
"populated demonstrating the execution of the block.

DATA info_tab TYPE string_table.
DATA cleanup TYPE REF TO cx_root.
DO 2 TIMES.
  APPEND |---- Loop pass { sy-index } ----| TO info_tab.
  TRY.
      TRY.

          IF sy-index = 1.
            DATA(calc) = CONV decfloat34( 1 / 0 ).
          ELSE.
            DATA(strtab) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).
            DATA(line_4) = strtab[ 4 ].
          ENDIF.

        CATCH cx_sy_zerodivide INTO cleanup.
          APPEND `---- Catching cx_sy_zerodivide ----` TO info_tab.
          APPEND cleanup->get_text( ) TO info_tab.

        CLEANUP.
          APPEND `#### Executing CLEANUP block ####` TO info_tab.
          APPEND `d` TO strtab.

          "CLEANUP block must be executed completely
          "Leaving the block prematurely causes a runtime error. If it is statically known
          "that it is not possible to return to the block, a syntax error is shown.
          "So, the following statement is not allowed in the CLEANUP block.
          "RETURN.
      ENDTRY.
    CATCH cx_sy_itab_line_not_found INTO cleanup.
      APPEND `---- Catching cx_sy_itab_line_not_found ----` TO info_tab.
      APPEND cleanup->get_text( ) TO info_tab.
      TRY.
          line_4 = strtab[ 4 ].
          APPEND `Since the row was added in the CLEANUP block, it is now in the table.` TO info_tab.
        CATCH cx_sy_itab_line_not_found INTO DATA(line_not_found).
          APPEND `The row is not in the table.` TO info_tab.
      ENDTRY.
  ENDTRY.
ENDDO.

"----------------------------------------------------------------------------
"----------------------- INTO addition --------------------------------------
"----------------------------------------------------------------------------
"The example is the same as above. Here, the CLEANUP statement is specified
"with the addition INTO. If required, you can evaluate the exception information
"as shown above.

DATA info_tab_b TYPE string_table.
DATA cleanup_b TYPE REF TO cx_root.
DO 2 TIMES.
  APPEND |---- Loop pass { sy-index } ----| TO info_tab_b.
  TRY.
      TRY.

          IF sy-index = 1.
            DATA(calc_b) = CONV decfloat34( 1 / 0 ).
          ELSE.
            DATA(strtab_b) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).
            DATA(line_4_b) = strtab_b[ 4 ].
          ENDIF.

        CATCH cx_sy_zerodivide INTO cleanup_b.
          APPEND `---- Catching cx_sy_zerodivide ----` TO info_tab_b.
          APPEND cleanup_b->get_text( ) TO info_tab_b.

        CLEANUP INTO cleanup_b.
          APPEND `#### Executing CLEANUP block ####` TO info_tab_b.
          "USing RTTI to find out the absolute name of the class of the raised execption
          DATA(cl_name) = cl_abap_classdescr=>get_class_name( p_object = cleanup_b ).
          APPEND cl_name TO info_tab_b.
          APPEND `d` TO strtab_b.

          "CLEANUP block must be executed completely
          "Leaving the block prematurely causes a runtime error. If it is statically known
          "that it is not possible to return to the block, a syntax error is shown.
          "So, the following statement is not allowed in the CLEANUP block.
          "RETURN.
      ENDTRY.
    CATCH cx_sy_itab_line_not_found INTO cleanup_b.
      APPEND `---- Catching cx_sy_itab_line_not_found ----` TO info_tab_b.
      APPEND cleanup_b->get_text( ) TO info_tab_b.
  ENDTRY.
ENDDO.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### RETRY Statements

`RETRY` statements ...
- exit the exception handling in the `CATCH` block and continue with the `TRY` block of the current control structure.
- cause the entire `TRY` control structure to process again. Remove the exception cause before or after `RETRY` in the `TRY` block executed again.
- can only be specified within `CATCH` blocks of `TRY` control structures.

Example:
```abap
"The following example includes a division of 1 by another number.
"In a DO loop 1 is divided by all numbers from 10 to -10.
"When the zero division exception is caught, the exception cause is
"removed by changing the first operand's value to 0, too (Note: ABAP
"'allows' zero division) before the RETRY statement. The RETRY statement
"triggers the execution of the TRY control structure again, now
"resulting in 0 as division result. After that, and to have a self-contained
"example, the operand value is changed back to 1.
DATA division_result_tab TYPE TABLE OF decfloat34 WITH EMPTY KEY.
DATA(number1) = 1.
DATA(number2) = 11.
DATA division_result TYPE decfloat34.
DO.
  number2 -= 1.
  TRY.
      division_result = number1 / number2.
      APPEND division_result TO division_result_tab.
    CATCH cx_sy_zerodivide.
      "Removing the exception cause by setting a value that
      "does not raise the zero division exception
      number1 = 0.
      DATA(retry_flag) = abap_true.
      "Processing the TRY control structure again
      RETRY.
  ENDTRY.

  "Resetting of number1 value to 1
  IF retry_flag = abap_true.
    number1 = 1.
    retry_flag = abap_false.
  ENDIF.

  IF sy-index = 21.
    EXIT.
  ENDIF.
ENDDO.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### RESUME Statements and RESUMABLE Additions


- `RESUME` statements ...
  - exit the exception handling in the `CATCH` block of a [resumable exception](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenresumable_exception_glosry.htm) and continue processing from where the exception was raised.
    - The exception context is preserved, and `CLEANUP` blocks are not executed.
  - Can only be specified within `CATCH BEFORE UNWIND` blocks of `TRY` control structures.
- As a prerequisite, exceptions ...
  - must be raised using `RAISE EXCEPTION` statements or `THROW` additions of the `COND` and `SWITCH` operators with the `RESUMABLE` addition.
  - must be declared with the `RESUMABLE` addition in the `RAISING` clause of procedure signatures.
- You can use the `is_resumable` attribute of exception objects to check if resuming is possible.

Example:
```abap
"----------------------------------------------------------------------------
"------------ RESUMABLE addition in method declarations and  ----------------
"------------ RESUMABLE addition with RAISE EXCEPTION/THROW -----------------
"----------------------------------------------------------------------------
"The following demo method declaration shows one prerequisite of resumable
"exceptions. The RAISING clause specifies a demo exception class with the 
"RESUMABLE addition.

CLASS-METHODS meth_resumable
  IMPORTING num1              TYPE i
            num2              TYPE i
  RETURNING VALUE(div_result) TYPE string
  RAISING   RESUMABLE(zcx_demo_abap_error_b).

...

"Demo method implementation demonstrating a RAISE EXCEPTION statement
"with the RESUMABLE addition (and an example for statement using the COND 
"operator commented out)
METHOD meth_resumable.

  IF num2 = 0 AND num1 <> 0.
    RAISE RESUMABLE EXCEPTION TYPE zcx_demo_abap_error_b.
    div_result = `No result. Resumed!`.
  ELSE.
    div_result = num1 / num2.
  ENDIF.

  "div_result = COND #( WHEN num2 = 0 AND num1 <> 0 THEN num1 / num2
  "                     ELSE THROW RESUMABLE zcx_demo_abap_error_b( ) ).

ENDMETHOD.

"----------------------------------------------------------------------------
"----------------------- CATCH BEFORE UNWIND  -------------------------------
"----------------------------------------------------------------------------
"Example using the method
"The following example creates a table containing integers from -5 to 5. This
"table is looped across to have different values for divisions. Here, this
"value represents the second operand. The first operand is the sy-tabix 
"value. The result is added to an internal table. When the second operand's
"value is 0, the exception is raised. In the CATCH BEFORE UNWIND block, the
"is_resumable attribute is evaluated. An info message is added to the internal
"table, and the processing continues after executing the RESUME statement.

DATA restab TYPE string_table.
TYPES ty_inttab TYPE TABLE OF i WITH EMPTY KEY.
DATA(inttab) = REDUCE ty_inttab( INIT tab = VALUE ty_inttab( )
                                 FOR  i = -5 UNTIL i > 5
                                 NEXT tab = VALUE #( BASE tab ( i ) ) ).

LOOP AT inttab INTO DATA(wa).
  TRY.
      DATA(divres) = meth_resumable(
        num1 = sy-tabix
        num2 = wa
      ).
      APPEND |{ sy-tabix } / { wa } = { divres }| TO restab.
    CATCH BEFORE UNWIND zcx_demo_abap_error_b INTO DATA(error_resume).
      DATA(is_resumable) = error_resume->is_resumable.
      IF is_resumable IS NOT INITIAL.
        APPEND |Exception raised. Is resumable? -> "{ is_resumable }"| TO restab.
        RESUME.
      ENDIF.
  ENDTRY.
ENDLOOP.


*Content of the result table:
*1 / -5 = 0.2-                         
*2 / -4 = 0.5-                         
*3 / -3 = 1-                           
*4 / -2 = 2-                           
*5 / -1 = 5-                           
*Exception raised. Is resumable? -> "X"
*6 / 0 = No result. Resumed!           
*7 / 1 = 7                             
*8 / 2 = 4                             
*9 / 3 = 3                             
*10 / 4 = 2.5                          
*11 / 5 = 2.2
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Using Messages as Exception Texts

- Each exception has an [exception text](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexception_text_glosry.htm) that describes the error and can be retrieved as outlined above. This helps you analyze the error. Imagine using exceptions in user interfaces; if a user encounters an error, the exception texts may be displayed on the UI.
- Typically, messages are texts organized in message classes and accessed using the `MESSAGE` statement. In [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm), these statements are relevant for classic UIs, which are not supported in [ABAP Cloud](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_cloud_glosry.htm). However, messages can also be used as exception texts for exception classes.
- In ABAP for Cloud Development, you can define exception texts using message classes to describe raised exceptions.
- Message classes group messages by an identifier.
- Messages are identified by a key consisting of the message class name and a 3-digit message number.
- If an exception is raised and ...
  - not handled, the exception text is displayed in the [short dump](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenshort_dump_glosry.htm) of the runtime error.
  - handled, the text can be retrieved using the `get_text` method as shown above.
- Exception classes must implement one of the system interfaces for messages to use exception texts:
  - `IF_T100_MESSAGE`:
    - Contains the `T100KEY` structured attribute (type `SCX_T100KEY`), specifying a message in the `T100` database table. Components are: `msgid` for the message class, `msgno` for the message number, and `attr1`/`attr2`/`attr3`/`attr4` for potential placeholders in message texts.
    - Used for statically assigning exception texts to messages of a message class. When you raise an exception programmatically, you pass an actual parameter to the optional `textid` parameter of the instance constructor, determining the exception text. If not passed, a predefined exception text can be used. The instance constructor must be implemented accordingly.
    - Exception texts are typically assigned to messages using constant structures defining the message attributes (message id, number, and attributes for placeholders) in the public visibility section of the exception class.
    - You can add attributes for placeholders as optional importing parameters to the instance constructor.
    - To create the constant structures for each exception text, you can use the `textIdExceptionClass` code template in ADT. Write `text` in the public visibility section of the class, choose *CTRL + Space*, and select the template.
    - Use the constants defined in the exception class for the message when referring to specific texts and passing using `textid`, rather than creating the message properties manually (e.g., using `VALUE scx_t100key( ... )`) in statements raising exceptions programmatically.
  - `IF_T100_DYN_MSG`:
    - Includes `IF_T100_MESSAGE` and is a more modern, recommended interface for handling messages, particularly designed for class-based exceptions.
    - The interface includes attributes to handle message-related attributes automatically for more comfortable message handling.
    - The interface can associate any messages with exception classes.
    - When exception classes implement the interface, more additions to ABAP statements are possible than with `IF_T100_MESSAGE`, such as `MESSAGE` and `WITH`. For example, the `MESSAGE` addition to `RAISE EXCEPTION` statements and `THROW` automatically assigns the correct values to the required exception class attributes, mapping the correct values to the `t100key` structure. `msgid` is assigned the message class, `msgno` is assigned the message number, and `attr1`/`attr2`/`attr3`/`attr4` are assigned the values of `if_t100_dyn_msg~msgv1` to `if_t100_dyn_msg~msgv4` (when including the `WITH` addition).
- Optionally, up to four placeholders (`&1`, `&2`, `&3`, `&4`) can be specified for messages. At runtime, placeholders are replaced by values specified with the attributes.


> **💡 Note**<br>
> When you create global exception classes in ADT, and you specify one of the exception superclasses to inherit from in the creation wizard, the resulting class code contains required implementations such as the instance constructor.


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Excursion: MESSAGE Statements

- Apart from using messages in exceptions and accessing them via `get_text`, you can also access them with `MESSAGE` statements.
- The `MESSAGE` addition in statements populates the attributes of the `IF_T100_DYN_MSG` interface with values (see the `WITH` addition as well).
- When using `MESSAGE`, do not pass a value to the `textid` input parameter. This parameter is only for predefined exception texts.
- In [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm), `MESSAGE` statements are suitable for classic UIs (e.g., error dialogs), which ABAP Cloud does not support. Therefore, several additions are unavailable in ABAP Cloud.
- In ABAP for Cloud Development, the variant `MESSAGE ... INTO ...` is possible. It adds short message texts to a field and populates `sy` components (`sy-msgid`, `sy-msgno`, `sy-msgty`, `sy-msgv1` to `sy-msgv4`).

Example:
```abap
"The following examples demonstrate several MESSAGE statements. 
"The message class from the executable demo example is used. So, 
"the content of msg shows the specified example text, and includes
"the specified parameters.

"After MESSAGE, a message type is specified (which is not of relevance in ABAP for Cloud
"Development). These types can be the following: A, E, I, S, W, or X. See the ABAP Keyword
"Documentation for information. For example, E representing an error message.
"The character is followed by the message number. The message class name is directly
"specified within a pair of parentheses.
MESSAGE e001(zdemo_abap_messages) INTO DATA(msg).
"msg: An error occured (Nr. 001)

DATA(msgid) = sy-msgid. "ZDEMO_ABAP_MESSAGES
DATA(msgty) = sy-msgty. "E

MESSAGE e004(zdemo_abap_messages)
        WITH 'A' 'B' 'C' 'D'
        INTO msg.
"msg: Message (Nr. 004) p1: "A" p2: "B" p3: "C" p4: "D"

msgid = sy-msgid. "ZDEMO_ABAP_MESSAGES
msgty = sy-msgty. "E
DATA(msgv1) = sy-msgv1. "A
DATA(msgv2) = sy-msgv2. "B
DATA(msgv3) = sy-msgv3. "C
DATA(msgv4) = sy-msgv4. "D

MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
    TYPE 'E'
    NUMBER '005'
    WITH 'E' 'F' 'G' 'H'
    INTO msg.
"msg: E F G H

msgid = sy-msgid. "ZDEMO_ABAP_MESSAGES
msgty = sy-msgty. "E
msgv1 = sy-msgv1. "E
msgv2 = sy-msgv2. "F
msgv3 = sy-msgv3. "G
msgv4 = sy-msgv4. "H
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Syntax Variants of RAISE EXCEPTION/THROW

- This section provides examples of `RAISE EXCEPTION` statements and the `THROW` addition to the `COND` and `SWITCH` operators.
- Note that the `RESUMABLE` addition is not covered. See more details above. 
- The variants of statements covered in the example include the use of these additions. Check the ABAP Keyword Documentation for details and all possible syntax options.
  - `RAISE EXCEPTION TYPE ...`
  - `RAISE EXCEPTION TYPE ... EXPORTING ...`
  - `RAISE EXCEPTION object_reference_variable.`
  - `RAISE EXCEPTION TYPE ... MESSAGE ...`
  - `RAISE EXCEPTION TYPE ... MESSAGE ... WITH ...`
  - `RAISE EXCEPTION TYPE ... MESSAGE ID ... TYPE ... NUMBER ...`
  - `RAISE EXCEPTION TYPE ... MESSAGE ID ... TYPE ... NUMBER ... WITH ...`
  - `RAISE EXCEPTION TYPE ... USING MESSAGE.`
  - `... THROW exc( ) ...`
  - `... THROW exc( ... ) ...`
  - `... THROW exc( MESSAGE ... ) ...`
  - `... THROW exc( MESSAGE ... WITH ... ) ...`
  - `... THROW exc( MESSAGE ID ... TYPE ... NUMBER ... ) ...`
  - `... THROW exc( MESSAGE ID ... TYPE ... NUMBER ... WITH ... ) ...`
  - `... THROW exc( USING MESSAGE ) ...`

> **💡 Note**<br>
> - The code snippets below use exception classes, a message class and messages from the executable demo example.
> - The snippets include additions that are only available when exception classes implement the `IF_T100_DYN_MSG` interface. 
> - In the executable example, `zcx_demo_abap_error_a` implements the `IF_T100_MESSAGE` interface and  `zcx_demo_abap_error_b` implements `IF_T100_DYN_MSG`.

```abap
"----------------------------------------------------------------------------
"------------------- RAISE EXCEPTION statements  ----------------------------
"----------------------------------------------------------------------------

"Multiple additions and variants are possible. The following examples
"demonstrate a selection.
"Note that the examples include additions that are only available when
"exception classes implement the if_t100_dyn_msg interface.

"----- TYPE addition -----
"Raises an exception of a specified exception class
RAISE EXCEPTION TYPE zcx_demo_abap_error_b.

"----- TYPE addition including EXPORTING -----
"Assigns actual parameters to input parameters of the instance constructor
"particularly for classes implementing if_t100_message. The example uses
"a constant structure having the same name as the exception class.
RAISE EXCEPTION TYPE zcx_demo_abap_error_a EXPORTING textid = zcx_demo_abap_error_a=>zcx_demo_abap_error_a.

"Assigning values to replace placeholders of a message (see the executable example)
"In this case, replacing the placeholders &1 and &2 in a message.
RAISE EXCEPTION TYPE zcx_demo_abap_error_a
  EXPORTING
    textid  = zcx_demo_abap_error_a=>error_003
    p_003_a = `a`
    p_003_b = `b`.

"----- Specifying an exception object -----
DATA(cx_oref) = NEW zcx_demo_abap_error_a( textid = zcx_demo_abap_error_a=>error_005
                                            p_005_a = `Some`
                                            p_005_b = `error` ).

RAISE EXCEPTION cx_oref.

"Object reference created inline
RAISE EXCEPTION NEW zcx_demo_abap_error_a( textid  = zcx_demo_abap_error_a=>error_004
                                            p_004_a = `f`
                                            p_004_b = `g` ).


"----- MESSAGE addition -----
"For passing message specification to an exception object
"Note that EXPORTING can also be specified
"After MESSAGE, a message type is specified (which is not of relevance in ABAP for Cloud
"Development). These types can be the following: A, E, I, S, W, or X. See the ABAP Keyword
"Documentation for information. For example, E representing an error message.
"The character is followed by the message number. The message class name is directly
"specified within a pair of parentheses.
RAISE EXCEPTION TYPE zcx_demo_abap_error_a MESSAGE e002(zdemo_abap_messages).

"----- MESSAGE, ID, TYPE, NUMBER additions -----
"Specifying the message class, the message type, and the message number following
"dedicated additions
"The example is an alternative to the one above.
RAISE EXCEPTION TYPE zcx_demo_abap_error_a MESSAGE ID 'ZDEMO_ABAP_MESSAGES' TYPE 'E' NUMBER '002'.

"Specifying the values using data objects, not as literals as above.
"Note that uppercase letters are expected.
DATA(msgid) = 'ZDEMO_ABAP_MESSAGES'.
DATA(msgtype) = 'E'.
DATA(msgnum) = '002'.

RAISE EXCEPTION TYPE zcx_demo_abap_error_a MESSAGE ID msgid TYPE msgtype NUMBER msgnum.

"----- WITH addition -----
"Placeholders are replaced by the values specified after WITH
"Note that the positions of the operands determine which placeholders are replaced.
"The first specified operand replaces &1 and so on.

RAISE EXCEPTION TYPE zcx_demo_abap_error_b MESSAGE e005(zdemo_abap_messages) WITH 'Hello' 'world'.

RAISE EXCEPTION TYPE zcx_demo_abap_error_b MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
                                                   TYPE 'E'
                                                   NUMBER '004'
                                                   WITH 'abc' 'def' 'ghi'.

"----- USING MESSAGE addition -----
"Implicitly passing message properties
"The examples show MESSAGE statement before RAISE EXCEPTION
"statements.
"In ABAP for Cloud Development, the message must be stored in
"a data object using the INTO addition.

MESSAGE ID 'ZDEMO_ABAP_MESSAGES' TYPE 'E' NUMBER '004'
        WITH 'abc' 'def' 'ghi' 'jkl'
        INTO DATA(msg1).

RAISE EXCEPTION TYPE zcx_demo_abap_error_b USING MESSAGE.

DATA: mid     TYPE sy-msgid VALUE 'ZDEMO_ABAP_MESSAGES',
      mtype   TYPE sy-msgty VALUE 'E',
      msg_num TYPE sy-msgno VALUE '002'.

MESSAGE ID mid TYPE mtype NUMBER msg_num INTO DATA(msg2).

RAISE EXCEPTION TYPE zcx_demo_abap_error_b USING MESSAGE.

"Alternative MESSAGE statement
MESSAGE e002(zdemo_abap_messages) INTO DATA(msg3).

RAISE EXCEPTION TYPE zcx_demo_abap_error_b USING MESSAGE.

"Alternative MESSAGE statement
"Here, specifying the WITH addition.
MESSAGE e005(zdemo_abap_messages) WITH 'a' 'b' 'c' 'd' INTO DATA(msg4).

RAISE EXCEPTION TYPE zcx_demo_abap_error_b USING MESSAGE.

"The RAISE EXCEPTION statement with USING MESSAGE can be considered
"as a short form of the following statement. Note that with the
"MESSAGE statement the system fields are populated.
MESSAGE e005(zdemo_abap_messages) WITH 'a' 'b' 'c' 'd' INTO DATA(msg5).

RAISE EXCEPTION TYPE zcx_demo_abap_error_b MESSAGE ID sy-msgid
                                                   TYPE sy-msgty
                                                   NUMBER sy-msgno
                                                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

"----------------------------------------------------------------------------
"------------------- COND operator, THROW addition  -------------------------
"----------------------------------------------------------------------------
DATA(flag) = 'X'.

"Specifying the exception class after THROW including a mandatory pair of parentheses
"even if no messages or actual parameters are passed. EXPORTING cannot be specified.
DATA(cond_w_throw_1) = COND #( WHEN flag IS INITIAL THEN `works`
                               ELSE THROW zcx_demo_abap_error_a( ) ).

"Passing actual parameters
DATA(cond_w_throw_2) = COND #( WHEN flag IS INITIAL THEN `works`
                               ELSE THROW zcx_demo_abap_error_a( textid  = zcx_demo_abap_error_a=>error_004
                                                                 p_004_a = `cond_a`
                                                                 p_004_b = `cond_b` ) ).

"MESSAGE addition
DATA(cond_w_throw_3) = COND #( WHEN flag IS INITIAL THEN `works`
                               ELSE THROW zcx_demo_abap_error_a( MESSAGE e005(zdemo_abap_messages)
                                                                 p_005_a = `An exception raised with COND` )  ).

DATA(cond_w_throw_4) = COND #( WHEN flag IS INITIAL THEN `works`
                               ELSE THROW zcx_demo_abap_error_b( MESSAGE e005(zdemo_abap_messages)
                                                                 WITH 'Lorem' 'ipsum' )  ).

"MESSAGE addition including ID, TYPE, NUMBER
DATA(cond_w_throw_5) = COND #( WHEN flag IS INITIAL THEN `works`
                               ELSE THROW zcx_demo_abap_error_a( MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
                                                                 TYPE 'E'
                                                                 NUMBER '002' ) ).

DATA(cond_w_throw_6) =  COND #( WHEN flag IS INITIAL THEN `works`
                                ELSE THROW zcx_demo_abap_error_a( MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
                                                                          TYPE 'E'
                                                                          NUMBER '005'
                                                                          p_005_a = `blabla` ) ).

"MESSAGE addition including ID, TYPE, NUMBER, WITH
DATA(cond_w_throw_7) = COND #( WHEN flag IS INITIAL THEN `works`
                               ELSE THROW zcx_demo_abap_error_b( MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
                                                                         TYPE 'E'
                                                                         NUMBER '005'
                                                                         WITH `lorem ipsum` ) ).

"USING MESSAGE addition
MESSAGE e005(zdemo_abap_messages) WITH 'Some message' INTO DATA(msg6).

DATA(cond_w_throw_8) = COND #( WHEN flag IS INITIAL THEN `works`
                               ELSE THROW zcx_demo_abap_error_b( USING MESSAGE ) ).

"----------------------------------------------------------------------------
"------------------- SWITCH operator, THROW addition  ----------------------
"----------------------------------------------------------------------------
DATA(switch_w_throw_1) = SWITCH #( flag WHEN '' THEN `works`
                                   ELSE THROW zcx_demo_abap_error_a( ) ).

DATA(switch_w_throw_2) = SWITCH #( flag WHEN '' THEN `works`
                                   ELSE THROW zcx_demo_abap_error_a( textid  = zcx_demo_abap_error_a=>error_004
                                                                     p_004_a = `switch_a`
                                                                     p_004_b = `switch_b` ) ).

DATA(switch_w_throw_3) = SWITCH #( flag WHEN '' THEN `works`
                                   ELSE THROW zcx_demo_abap_error_a( MESSAGE e005(zdemo_abap_messages)
                                                                     p_005_a = `An exception raised with SWITCH` ) ).

DATA(switch_w_throw_4) = SWITCH #( flag WHEN '' THEN `works`
                                   ELSE THROW zcx_demo_abap_error_b( MESSAGE e005(zdemo_abap_messages)
                                                                     WITH 'Test' 'message' ) ).

DATA(switch_w_throw_5) = SWITCH #( flag WHEN '' THEN `works`
                                   ELSE THROW zcx_demo_abap_error_a( MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
                                                                             TYPE 'E'
                                                                             NUMBER '005'
                                                                             p_005_a = `lorem ipsum` ) ).

DATA(switch_w_throw_6) = SWITCH #( flag WHEN '' THEN `works`
                                   ELSE THROW zcx_demo_abap_error_b( MESSAGE ID 'ZDEMO_ABAP_MESSAGES'
                                                                                 TYPE 'E'
                                                                                 NUMBER '005'
                                                                                 WITH `lorem ipsum` ) ).

MESSAGE e005(zdemo_abap_messages) WITH 'Some message' INTO DATA(msg7).

DATA(switch_w_throw_7) = SWITCH #( flag WHEN '' THEN `works`
                                   ELSE THROW zcx_demo_abap_error_b( USING MESSAGE ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Runtime Errors

Runtime errors ...
- occur when the execution of an ABAP program cannot continue and is terminated. 
- are caused in the following situations:
   - Unhandled catchable exceptions
   - Uncatchable exceptions
   - Programmatically raised by statements
   - Failed assertions
- are identified by a name.
- lead to a [database rollback](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendatabase_rollback_glosry.htm). 
- trigger the generation of a short dump, an error log that is displayed and stored. 
  - When you execute a program, and a runtime error occurs, a popup appears in ADT informing you of the error.
  - You can check the details by choosing the "Show" button in the popup.
  - You can also directly go to the ABAP Debugger.
  - You can also check the content of the *Feed Reader* tab in ADT. There, expand your project and find the runtime errors caused by you.

Example of a catchable exception not handled: 
- In ADT, run a class (e.g. `zcl_some_class`) that, for example, contains the statement `DATA(res) = 1 / 0.`, without a `TRY` control structure to handle the exception.
- When running the class, a popup is displayed.
- Clicking the *Show* button opens the short dump, providing information such as the following: 
  ```
  Short Text  Division by 0 (type I or INT8)  
  Runtime Error  COMPUTE_INT_ZERODIVIDE  
  Exception  CX_SY_ZERODIVIDE  
  Program  ZCL_SOME_CLASS================CP
  ...
  ```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Programmatically Raising Runtime Erros 

- Runtime errors can be raised using ...
  - [`RAISE SHORTDUMP`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapraise_shortdump.htm) statments or 
  - the `THROW SHORTDUMP` addition in the context of `COND` and `SWITCH` operators
- Note that the statements have similar variants (using messages, specifying actual parameters for importing parameters, etc.) than those of `RAISE EXCEPTION` statements. They are not outlined here. For more information, see the sections above and the ABAP Keyword Documentation.

```abap
RAISE SHORTDUMP TYPE cx_sy_zerodivide.

DATA(flag) = 'X'.
DATA(cond_w_throw_shortdump) = COND #( WHEN flag IS INITIAL THEN `works`
                                       ELSE THROW SHORTDUMP zcx_demo_abap_error_b( ) ).

DATA(switch_w_throw_shortdump) = SWITCH #( flag WHEN '' THEN `works`
                                           ELSE THROW SHORTDUMP zcx_demo_abap_error_b( ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Assertions

- Assertions are defined as conditional checkpoints using `ASSERT` statements. 
- `ASSERT` is followed by a logical expression.
- If the expression is false, the program is terminated and an uncatchable exception is raised, resulting in the runtime error `ASSERTION_FAILED`. 

```abap
"The ASSERT keyword is followed by a logical expression.
"If the expression is false, the program is terminated and an uncatchable exception is raised
"resulting in the runtime error ASSERTION_FAILED.

DATA(number) = 0.
ASSERT number IS INITIAL.
ASSERT number > -1.
number = 1.
ASSERT number = 1.

DATA(flag) = abap_false.
"Raises a runtime error
ASSERT flag = abap_true.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions 

### Local Exception Classes

The following simplified example demonstrates a local exception class: 
- In a class, e.g. `zcl_some_class`, go to the [CCDEF include](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenccdef_glosry.htm), i.e. the *Class-relevant local types* tab in ADT.
- Add the following local class declaration
  ```abap
  CLASS lcx_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
      INTERFACES if_t100_dyn_msg.
  ENDCLASS.
  ```
- In the global class, add the following code. 
  - A method declaration includes the local execption class following the `RAISING` addition.  
  - In a simple way, the example explores the raising of the exception.
  - A message class of the executable example is used. Without the `MESSAGE` addition after `THROW`, a default message would be used. 
  - You can run the class using *F9*.

  ```abap
  CLASS zcl_some_class DEFINITION
    PUBLIC
    FINAL
    CREATE PUBLIC .

    PUBLIC SECTION.
      INTERFACES if_oo_adt_classrun.
    PROTECTED SECTION.
    PRIVATE SECTION.
      METHODS test_meth IMPORTING flag         TYPE abap_boolean
                        RETURNING VALUE(hello) TYPE string
                        RAISING   lcx_error.
  ENDCLASS.



  CLASS zcl_some_class IMPLEMENTATION.

    METHOD if_oo_adt_classrun~main.

      TRY.
          DATA(hi1) = test_meth( abap_true ).
          out->write( hi1 ).
        CATCH lcx_error INTO DATA(error1).
          out->write( error1->get_text( ) ).
      ENDTRY.

      TRY.
          DATA(hi2) = test_meth( abap_false ).
          out->write( hi2 ).
        CATCH lcx_error INTO DATA(error2).
          out->write( error2->get_text( ) ).
      ENDTRY.

    ENDMETHOD.

    METHOD test_meth.
      hello = COND #( WHEN flag = abap_true THEN |Hello, { xco_cp=>sy->user( )->name }.|
                      ELSE THROW lcx_error( MESSAGE e005(zdemo_abap_messages) WITH |See you, { xco_cp=>sy->user( )->name }.| ) ).
    ENDMETHOD.

  ENDCLASS.
  ```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Messages in RAP


- From an ABAP EML perspective, the relevant [BDEF derived type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_derived_type_glosry.htm) is `TYPE ... REPORTED`, available in the context of [RAP responses](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_response_glosry.htm).
- The [RAP response parameter](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_response_param_glosry.htm) `reported` includes the `%msg` component of BDEF derived types.
- `%msg` provides an instance of the message interface `IF_ABAP_BEHV_MESSAGE`.
- If you need a custom implementation for your messages, you can implement the interface. Find an example in the [Development Guide for the ABAP RESTful Application Programming Model](https://help.sap.com/docs/ABAP_Cloud/f055b8bf582d4f34b91da667bc1fcce6/d0ba40477fba4ad5a373670c99d2956c.html). Otherwise, you can use the inherited methods `new_message` or `new_message_with_text` for a standard implementation.

The following example is from an ABAP EML cheat sheet example. Here, the standard implementation is used.

```abap
...
APPEND VALUE #( %tky = <fs_res>-%tky
                %msg = new_message_with_text(
                  severity = if_abap_behv_message=>severity-error
                  text = 'Validation failed!' )
              ) TO reported-root.
...              
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Violations of ABAP Contract Checks Causing Runtime Errors

ABAP contract checks include ...

- RAP BO contract checks 
  - They define rules for the [RAP BO provider](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_provider_glosry.htm) and [RAP BO consumer](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_consumer_glosry.htm) implementation to ensure consistency and reliability.
  - They include transactional contract checks, too.
  - More information:     
    - [Development guide for the ABAP RESTful Application Programming Model, section RAP Business Object Contract (SAP Help Portal)](https://help.sap.com/docs/ABAP_Cloud/f055b8bf582d4f34b91da667bc1fcce6/3a402c5cf6a74bc1a1de080b2a7c6978.html)
    - [RAP Implementation Rules (ABAP Keyword Documentation)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaprap_impl_rules.htm)
    - [Restrictions in RAP Handler and Saver Methods (ABAP Keyword Documentation)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapinvalid_stmts_in_rap_methods.htm)

    Example: 
    - Violation: Missing `%cid`
    - `%cid` should always be filled in RAP creation requests
    - You can try out the following:
      - You are in your SAP BTP ABAP environment.
      - Provided that you have imported the ABAP cheat sheet repository, you can add the following ABAP EML create request in a class that implements the classrun (`if_oo_adt_classrun`).
      - The `%cid` specification in the code snippet is intentionally commented out, so `%cid` is not specified.
      - Run the class. 
      - The `BEHAVIOR_CONTRACT_VIOLATION` runtime error will be raised.
      - To avoid the runtime error, specify `%cid` explicitly or use the [`AUTO FILL CID`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_fields.htm) addition to create `%cid` automatically.

    ```abap
    DELETE from zdemo_abap_rapt1.

    MODIFY ENTITIES OF zdemo_abap_rap_ro_m
        ENTITY root
        CREATE FIELDS ( key_field field1 field2 field3 field4 )
        WITH VALUE #( ( "%cid = 'cid1'
                        key_field = 1
                        field1    = 'aaa'
                        field2    = 'bbb'
                        field3    = 10
                        field4    = 11 ) )
        MAPPED FINAL(mapped)
        FAILED FINAL(failed)
        REPORTED FINAL(reported).

    COMMIT ENTITIES.

    SELECT SINGLE * from zdemo_abap_rapt1 where key_field = 1 into @FINAL(entry).
    ```


- Transactional contract checks 
  - Implemented in the [controlled SAP LUW](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencontrolled_sap_luw_glosry.htm) concept. 
  - Check for transactional consistency violations in a transactional phase.
  - Transactional contracts specify which ABAP statements and operations are allowed and which are not allowed in a transactional phase.
  - Such a transactional phase is either the *modify* or the *save* transactional phase.
  - The phases are set either implicitly (e.g. in RAP handler and saver methods), or explicitly using the static methods of the `CL_ABAP_TX` class.
  - In RAP, the *modify* transactional phase includes the [RAP interaction phase](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_int_phase_glosry.htm) and the [RAP early save phase](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenearly_rap_save_phase_glosry.htm) of the [RAP save sequence](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_save_seq_glosry.htm). The *save* transactional phase includes the [RAP late save phase](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlate_rap_save_phase_glosry.htm).
  - Transactional contracts can be set explicitly by API classifications (starting with `IF_ABAP_TX...`, for example, `IF_ABAP_TX_SAVE`). 
  - More information: 
    - [Controlled SAP LUW (SAP LUW cheat sheet)](17_SAP_LUW.md#controlled-sap-luw)
    - [Ensuring Data Consistency in a RAP Transaction (ABAP EML cheat sheet)](08_EML_ABAP_for_RAP.md#ensuring-data-consistency-in-a-rap-transaction)
    - [Controlled SAP LUW (SAP Help Portal)](https://help.sap.com/docs/ABAP_Cloud/f2961be2bd3d403585563277e65d108f/80fe04141e30456c80cc90c5cc838e94.html)
    - [API Classifications (ABAP Keyword Documentation)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapapi_classification.htm)
   

    Example: 
    ```abap
    "Activating the modify transactional phase
    cl_abap_tx=>modify( ).

    "The following database modification statement is not allowed in the
    "modify transactional phase. In certain contexts, e.g. in ABAP Cloud,
    "the runtime error BEHAVIOR_ILLEGAL_STMT_IN_CALL occurs.
    MODIFY zdemo_abap_carr FROM TABLE @( VALUE #(
        ( carrid = 'XY'
          carrname = 'XY Airlines'
          currcode = 'EUR'
          url =  'some_url' ) ) ).

    ...

    "Activating the save transactional phase
    cl_abap_tx=>save( ).

    "In this phase, database modifications are allowed.
    MODIFY zdemo_abap_carr FROM TABLE @( VALUE #(
        ( carrid = 'XY'
          carrname = 'XY Airlines'
          currcode = 'EUR'
          url =  'some_url' ) ) ).
    ...
    ```  

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Classic Exceptions

- In older ABAP code, you may encounter non-class-based exceptions, the predecessors of class-based exceptions.
- They should no longer be used.
- They are specified in method signatures with the `EXCEPTIONS` addition.
- Find more details in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenexceptions_non_class.htm).

Example: 
- The following example shows the longer available `describe_by_name` method available in the [Runtime Type Identification (RTTI)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrun_time_type_identific_glosry.htm) class `cl_abap_typedescr` (find more information in the Dynamic Programming cheat sheet).
- This method specifies `EXCEPTIONS` in the method signature.
- The example intentionally uses a class name that is (most probably) not available in the system.
- The exception is raised, and the `sy-subrc` value is evaluated. 
- The example is implemented in a way that a more modern class-based exception is raised (using a demo exception and message class from the executable example) following the raising of the non-class-based exception.

```abap
TRY.
    cl_abap_typedescr=>describe_by_name( EXPORTING p_name = 'CL_THAT_DOES_NOT_EXIST'
                                         RECEIVING p_descr_ref = DATA(tdo_type)
                                         EXCEPTIONS type_not_found = 4 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_demo_abap_error_b MESSAGE e005(zdemo_abap_messages) WITH 'Type not found'.
    ELSE.
      DATA(tdo_cl) = CAST cl_abap_classdescr( tdo_type ).

      "Getting more type information; find more information in the Dynamic Programming cheat sheet
      ...
    ENDIF.
  CATCH zcx_demo_abap_error_b INTO DATA(err).
    DATA(text) = err->get_text( ).
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information 
[ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_exceptions.htm)

## Executable Example

[zcl_demo_abap_error_handling](./src/zcl_demo_abap_error_handling.clas.abap)

> **💡 Note**<br>
> - The example involves the following artifacts: 
>   - One executable class `zcl_demo_abap_error_handling`
>   - Two self-defined example exception classes `zcx_demo_abap_error_a` and `zcx_demo_abap_error_b`  (one implements the `IF_T100_DYN_MSG`, the other implements `IF_T100_MESSAGE`)
>   - One message class `zdemo_abap_messages` with 5 message texts that are used as exception texts
> - The example explores syntax described and uses code snippets in the cheat sheet. It does not represent best practice implementations, and only focuses on demonstrating ABAP syntax.
> - The executable example of the [Program Flow Logic](13_Program_Flow_Logic.md) cheat sheet also includes examples with error handling.
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)