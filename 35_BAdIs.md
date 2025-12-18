<a name="top"></a>

# Enhancements Using BAdIs

- [Enhancements Using BAdIs](#enhancements-using-badis)
  - [About BAdIs](#about-badis)
  - [Excursion: Demo BAdIs](#excursion-demo-badis)
    - [Demo BAdI 1](#demo-badi-1)
    - [Demo BAdI 2](#demo-badi-2)
  - [BAdI-Related ABAP Statements](#badi-related-abap-statements)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)


This ABAP cheat sheet provides a high-level overview of enhancements using BAdIs (Business Add-Ins), focusing on the relevant ABAP syntax.
The cheat sheet aims to illustrate the BAdI-related ABAP syntax with simple, self-created demo BAdIs, whose creation is outlined in tutorial-style descriptions. The descriptions are intended to get an idea about the involved repository objects. The repository objects serve as the basis for the ABAP example code snippets in the [executable example](#executable-example).


## About BAdIs

- A BAdI (Business Add-In) is an object-oriented option for enhancing the functionality of standard SAP or custom objects without modifying the underlying source code.
- These enhancement options are realized through specific positions (enhancement spots) that allow custom logic to be plugged in and executed at runtime.
- A BAdI mainly consists of a BAdI interface, which describes a template for BAdI objects.
- The BAdI interface includes the specification of the predefined tag interface `IF_BADI_INTERFACE` and the declaration of BAdI methods. Note that the specification of variable attributes is not allowed.
- BAdIs offer various properties to be set, including:
  - Filter conditions
    - You can specify one or more filters by providing filter names and associating them with basic data types (for example, integer or string).
    - This allows you to select different BAdI implementations. Callers can dynamically control which BAdI implementations are used by specifying filter values.
  - Defining a BAdI for single or multiple use
    - Single use: Only one BAdI implementation can be used in an internal session, and one BAdI implementation must be available for each use. In single-use cases, it is advisable to specify and implement an optional fallback class for scenarios where no (default) BAdI implementation is found or when the caller-provided filter values do not match existing filters.
    - Multiple use: Multiple BAdI implementations (or none) can exist in parallel, enabling the execution of multiple functionalities. Note the restrictions with this setting: The parameter interface of a BAdI method can only contain importing and changing parameters. With multiple functionalities being executed, there cannot be a single output parameter. Changing parameters incorporate input and output parameter capabilities, allowing an existing data object to be altered sequentially. 
  - Instantiation-related properties such as new instance creation, instance reuse, or context-dependent instantiation
    - With the first two properties set, BAdIs are considered context-free. 
    - Context-dependent instantiation means the context controls instantiation, allowing only one instance for each context and implementing class. The prerequisite is the implementation of the tag interface `IF_BADI_CONTEXT`. The `GET BADI` statement includes the `CONTEXT` addition relevant here, which is not covered in this cheat sheet. Refer to the documentation for more details.
- BAdI methods are implemented in BAdI implementations.
  - A BAdI implementation consists of a BAdI implementation class whose instances enhance the functionality at runtime in the form of an [object plug-in](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenobject_plugin_glosry.htm).
  - Multiple BAdI implementations can be assigned to a single BAdI.
  - BAdI methods can be called in ABAP programs exclusively via the associated BAdI object using `CALL BADI`.

> [!NOTE]
> The steps and syntax outlined in this document focus on the kernel-based BAdI concept, which integrates BAdIs in ABAP for use with related ABAP syntax. Legacy classic BAdIs found in [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm) are not covered here. In classic ABAP, the BAdI builder, an ABAP Workbench tool, allows you to create and maintain BAdIs. The relevant transaction codes are `SE18` for defining and `SE19` for implementing BAdIs.

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Excursion: Demo BAdIs

This section provides high-level walkthrough examples to illustrate simplified BAdIs, which serve as examples for demonstrating the ABAP syntax below. The step-by-step procedure includes the creation of the following elements:

- **BAdI enhancement spot**
  - Serves as a container for the BAdI.
- **BAdI** (**BAdI definition**)
  - For a BAdI, you create a BAdI definition and provide a dedicated BAdI name.
  - This BAdI must be assigned to the BAdI enhancement spot.
  - Various properties can be set, such as filter conditions, single/multiple use, and more. Optionally, you can specify and implement a fallback class.
- **BAdI interface**
  - For the BAdI, you create a BAdI interface to define the BAdI methods that implementation classes will implement.
  - The BAdI Interface must include the interface `IF_BADI_INTERFACE`.
- **BAdI enhancement implementation**
  - Since the BAdI only defines an interface, you need a class that implements the interface to create a usable BAdI object.
  - The BAdI enhancement implementation serves as a container for **BAdI implementations**.
  - You can include multiple BAdI implementations, all assigned to a specific BAdI enhancement spot.
  - A **BAdI implementation** specifies properties such as the **BAdI implementation class**, which contains the concrete code enhancement, and filter combinations (that is, when a specific filter is selected, BAdI methods in that specific BAdI implementation class should be called).

> [!NOTE]
> - The demo BAdIs outlined here are simplified and non-semantic, intended solely for [exploration, experimentation, and demonstration](./README.md#%EF%B8%8F-disclaimer). They represent self-contained examples on which the BAdI-related ABAP statements are illustrated in the cheat sheet.  
> - As a prerequisite for walking through the creation steps, you have: 
>   - Opened ADT.
>   - Logged in to your SAP BTP ABAP Environment.
>   - Created a package for demo content.
> - The steps create demo repository objects step by step. You can also create objects separately and add them to the appropriate fields on the maintenance screens in ADT.
> - When enhancing existing functionality, you need to know about the existing BAdI and proceed with creating custom implementations.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Demo BAdI 1

> [!NOTE]
> - Context of the simplified demo: Calculations 
> - The example includes subtraction and division (and addition provided through a fallback class). The idea is that the functionality can be enhanced by, for example, introducing additional calculation options, such as multiplication.
> - The BAdI is defined for single use, including filter conditions and a fallback class.

1. **BAdI enhancement spot**
   - In your ADT package, create a new object by right-clicking the package, selecting _New_, then _Other ABAP Repository Object_, and filtering for _BAdI Enhancement Spot_.
   - Name the enhancement spot _ZES_DEMO_ABAP_CALCULATOR_ and provide a description.
   - Follow the wizard prompts. Once created, the _BAdI Enhancement Spot_ screen will display.

2. **BAdI / BAdI definition**
   - With the BAdI enhancement spot repository object open from the previous step, you are on the _BAdI Enhancement Spot_ screen.
   - Choose the _Add BAdI_ button.
   - In the _New BAdI Definition_ pop-up, enter the name _ZBADI_DEMO_ABAP_CALCULATOR_ and confirm.
   - The new BAdI will appear in the _BAdI Definitions_ list on the left. A message will prompt you to add an interface to the BAdI.
   - Set the following options:
     - Deselect the _Multiple Use_ checkbox, as the BAdI is intended for single use.
     - Select the _Use Fallback_ checkbox.
     - Leave other settings unchanged (the _Interface_ and _Fallback Class_ fields will be covered later).
   - Choose the _Add Filter_ button.
     - In the _New Filter_ pop-up, enter the name _OPERATOR_ and choose _Add_.
     - The filter will appear below the _ZBADI_DEMO_ABAP_CALCULATOR_ node on the left.
     - In the _Filter Details_ on the right, select _Character_ as the _Type_. Additional settings, like value checks, are not included in this example.
     - Return to the detail view for _ZBADI_DEMO_ABAP_CALCULATOR_ by selecting the _ZBADI_DEMO_ABAP_CALCULATOR_ node on the left.

3. **BAdI interface**
   - The BAdI enhancement spot repository object is opened.
   - Choose the _Interface_ link in the _Configuration_ section.
   - Follow the _New ABAP Interface_ wizard:
     - Enter the name _ZIF_DEMO_ABAP_CALCULATOR_ and provide a description.
     - Ensure that the interface _IF_BADI_INTERFACE_ is included in the _Interfaces_ list.
   - Add the following code and activate the interface. Note that the returning parameter in this simplified example is intentionally typed as `string`, and an appropriate error handling is not covered.

      ```abap
      INTERFACE zif_demo_abap_calculator
        PUBLIC .

        INTERFACES if_badi_interface .

        METHODS calculate IMPORTING num1          TYPE i
                                    num2          TYPE i
                          RETURNING VALUE(result) TYPE string.
      ENDINTERFACE.
      ```

4. **Fallback class**
     - The BAdI enhancement spot repository object is opened.
     - Choose the _Fallback Class_ link in the _Configuration_ section.
     - Walk through the _New ABAP Class_ wizard:
       - Enter the name _ZCL_DEMO_ABAP_CALCULATOR_FB_ and add a description.
       - Ensure the interface _ZIF_DEMO_ABAP_CALCULATOR_ is included in the _Interfaces_ list.
     - Add the following code and activate the class. 

       ```abap
       CLASS zcl_demo_abap_calculator_fb DEFINITION
         PUBLIC
         FINAL
         CREATE PUBLIC .

         PUBLIC SECTION.

           INTERFACES if_badi_interface .
           INTERFACES zif_demo_abap_calculator .
         PROTECTED SECTION.
         PRIVATE SECTION.
       ENDCLASS.



       CLASS zcl_demo_abap_calculator_fb IMPLEMENTATION.
         METHOD zif_demo_abap_calculator~calculate.
           result = |{ num1 + num2 }|.
         ENDMETHOD.
       ENDCLASS.
       ```

5. **BAdI implementation (1)**
   - Ensure you activate the BAdI if you have not done already.
   - In your package in ADT, find the enhancement spot _ZES_DEMO_ABAP_CALCULATOR_. Right-click it and select _New BAdI Enhancement Implementation_.
   - Follow the _New BAdI Enhancement Implementation_ wizard:
     - Add a suitable target package.
     - Enter the name _ZEI_DEMO_ABAP_CALCULATOR_ and a description.
     - Ensure the enhancement spot _ZES_DEMO_ABAP_CALCULATOR_ is included in the _Enhancement Spot_ input field.
   - After creation, the _BAdI Enhancement Implementation_ screen will appear.
   - Click the _Add BAdI Implementation_ button.
   - In the _Add BAdI Implementation_ pop-up, enter the following:
     - _BAdI Definition_: _ZBADI_DEMO_ABAP_CALCULATOR_
     - _BAdI Implementation Name_: _ZBADI_IMPL_DEMO_ABAP_CALC_SUB_
   - In the _BAdI Implementation Details_ section on the right, choose the _Implementing Class_ link.
   - Follow the _New ABAP Class_ wizard:
     - Enter the name _ZCL_DEMO_ABAP_CALCULATOR_SUBTR_ and a description.
     - Ensure the interface _ZIF_DEMO_ABAP_CALCULATOR_ is added to the Interfaces list.
   - Add the following code and activate the class:

      ```abap
      CLASS zcl_demo_abap_calculator_subtr DEFINITION
        PUBLIC
        FINAL
        CREATE PUBLIC .

        PUBLIC SECTION.

          INTERFACES if_badi_interface .
          INTERFACES zif_demo_abap_calculator .
        PROTECTED SECTION.
        PRIVATE SECTION.
      ENDCLASS.



      CLASS zcl_demo_abap_calculator_subtr IMPLEMENTATION.
        METHOD zif_demo_abap_calculator~calculate.
          result = |{ num1 - num2 }|.
        ENDMETHOD.
      ENDCLASS.
      ```

   - Set filter conditions
     - Back on the _BAdI Enhancement Implementation_ screen, choose the _Add Filter Combination_ button.
     - A _Filter Values_ node should be added below the _ZBADI_IMPL_DEMO_ABAP_CALC_SUB_ node.
     - In the _Filter Details_ section on the right, insert _-_ (minus character) in the _Value_ field.

6. **BAdI implementation (2)**
- Repeat the previous steps and create another BAdI implementation to reflect division:
  - _BAdI Definition_: _ZBADI_DEMO_ABAP_CALCULATOR_
  - _BAdI Implementation Name_: _ZBADI_IMPL_DEMO_ABAP_CALC_DIV_
  - Implementing class: _ZCL_DEMO_ABAP_CALCULATOR_DIV_

    ```abap
    CLASS zcl_demo_abap_calculator_div DEFINITION
      PUBLIC
      FINAL
      CREATE PUBLIC .

      PUBLIC SECTION.

        INTERFACES if_badi_interface .
        INTERFACES zif_demo_abap_calculator .
      PROTECTED SECTION.
      PRIVATE SECTION.
    ENDCLASS.



    CLASS zcl_demo_abap_calculator_div IMPLEMENTATION.
      METHOD zif_demo_abap_calculator~calculate.
        TRY.
            IF num1 = 0 AND num2 = 0.
              RAISE EXCEPTION TYPE cx_sy_zerodivide.
            ENDIF.

            result = |{ CONV decfloat34( num1 / num2 ) STYLE = SIMPLE }|.
          CATCH cx_sy_zerodivide.
            result = |Dividing { num1 } by { num2 } is not possible.|.
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.
    ```
  - _Filter Combination_/_Value_: _/_


### Demo BAdI 2

> [!NOTE]
> - Context of the simplified demo: Data object content conversion 
> - The example includes XML and JSON conversion. The idea is that the functionality can be enhanced by, for example, introducing additional conversion options.
> - The BAdI is defined for multiple use, without filter, and without fallback class.
> - The descriptions here are reduced. The creation follows the same steps as above (except for not specifying filter conditions and a fallback class). 

1. **BAdI enhancement spot** 
     - Create an enhancement spot with the name _ZES_DEMO_ABAP_CONVERTER_. 

2. **BAdI**
     - Create a BAdI definition with the name _ZBADI_DEMO_ABAP_CONVERTER_. 
     - Set the following options: 
       - Select the _Multiple use_ checkbox since the BAdI is intended for multiple use.    
       - Leave the other settings unchanged (only the _Interface_ field is relevant).  
        
3. **BAdI interface**
     - Create a BAdI interface with the name _ZIF_DEMO_ABAP_CONVERTER_.
     - Note that only importing and changing parameters are possible here. For display purposes, the method definition includes a referene to the classrun output. In the executable example class, the calling class implements the classrun and supplies a bound interface reference variable when calling the BAdI method.
      
        ```abap
        INTERFACE zif_demo_abap_converter
          PUBLIC .

          INTERFACES if_badi_interface .
          METHODS convert IMPORTING content TYPE any
                                    out     TYPE REF TO if_oo_adt_classrun_out.
        ENDINTERFACE.
        ```

4. **BAdI enhancement implementation**
     - Name: _ZEI_DEMO_ABAP_CONVERTER_
     - BAdI implementations (using the _ZBADI_DEMO_ABAP_CONVERTER_ BAdI definition)
       - _ZBADI_IMPL_DEMO_ABAP_CONV_XML_ 
         - Implementing class: _ZCL_DEMO_ABAP_CONVERTER_XML_  
       - _ZBADI_IMPL_DEMO_ABAP_CONV_JSON_ 
         - Implementing class: _ZCL_DEMO_ABAP_CONVERTER_JSON_  
     - Add the following code to the classes activate them: 
       - _ZCL_DEMO_ABAP_CONVERTER_XML_

          ```abap
          CLASS zcl_demo_abap_converter_xml DEFINITION
            PUBLIC
            FINAL
            CREATE PUBLIC .

            PUBLIC SECTION.

              INTERFACES if_badi_interface .
              INTERFACES zif_demo_abap_converter .
            PROTECTED SECTION.
            PRIVATE SECTION.
          ENDCLASS.



          CLASS zcl_demo_abap_converter_xml IMPLEMENTATION.
            METHOD zif_demo_abap_converter~convert.
              IF out IS NOT BOUND.
                RETURN.
              ENDIF.

              DATA xml TYPE xstring.

              DATA(tdo) = cl_abap_typedescr=>describe_by_data( content ).
              CASE TYPE OF tdo.
                WHEN TYPE cl_abap_elemdescr.
                  CALL TRANSFORMATION id
                    SOURCE var = content
                    RESULT XML xml.
                WHEN TYPE cl_abap_structdescr.
                  CALL TRANSFORMATION id
                    SOURCE structure = content
                    RESULT XML xml.
                WHEN TYPE cl_abap_tabledescr.
                  CALL TRANSFORMATION id
                    SOURCE table = content
                    RESULT XML xml.
                WHEN OTHERS.
                  RETURN.
              ENDCASE.

              DATA(xml_str) = cl_abap_conv_codepage=>create_in( )->convert( xml ).

              out->write( xml_str ).
              out->write( |\n| ).
            ENDMETHOD.
          ENDCLASS.
          ```

       - _ZCL_DEMO_ABAP_CONVERTER_JSON_ 

          ```abap
          CLASS zcl_demo_abap_converter_json DEFINITION
            PUBLIC
            FINAL
            CREATE PUBLIC .

            PUBLIC SECTION.

              INTERFACES if_badi_interface .
              INTERFACES zif_demo_abap_converter .
            PROTECTED SECTION.
            PRIVATE SECTION.
          ENDCLASS.



          CLASS zcl_demo_abap_converter_json IMPLEMENTATION.
            METHOD zif_demo_abap_converter~convert.
              IF out IS NOT BOUND.
                RETURN.
              ENDIF.

              DATA(json) = /ui2/cl_json=>serialize( data = content
                                                    format_output = abap_true ).

              out->write( json ).
              out->write( |\n| ).    
            ENDMETHOD.
          ENDCLASS.
          ```

## BAdI-Related ABAP Statements

<table>

<tr>
<th> Statement </th> <th> Notes </th>
</tr>

<tr>
<td> 

`GET BADI`

 </td>

 <td> 

- Creates a new BAdI object and assigns the BAdI reference to a BAdI reference variable.
- If the BAdI includes filters, the addition `FILTERS` must be specified, followed by assignments. The arguments for the filters must by type-compliant. Note that filters can also be defined to use only constants. In this case, only literals and constants can be specified. If a BAdI does not define filters, the `FILTERS` addition cannot be specified.
- After instantiation, the system searches for BAdI implementation classes. Only active classes that match the filter criteria are included in the search. If no matching implementations are found, the system will look for standard implementations. If none are found, it will use the fallback class, if available. For more information, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPGET_BADI.html).
- In addition to a static variant, a dynamic variant is also available:
  - The dynamic variant uses the addition `TYPE` and requires the name of a BAdI as a character-like data object in parentheses.
  - The static type of the reference variable must be `CL_BADI_BASE`, the superclass of all BAdI classes.
  - The `FILTERS` addition can also be applied in the dynamic variant. Alternatively, the `FILTER-TABLE` addition can be used, requiring a table of type `badi_filter_bindings`. Its components are `name` (type c, length 30; the filter name should be in uppercase) and `value` (type `REF TO data`; it expects a pointer to a matching data object).
- For context-dependent BAdIs, the `CONTEXT` addition is available. Find more details in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABAPGET_BADI.html).

 </td>
</tr>

<tr>
<td> 

`CALL BADI`

 </td>

 <td> 

- Calls a BAdI method using the BAdI reference variable.
- Expects a list of parameters to assign actual parameters to the method's formal parameters.
- In addition to a static variant, a dynamic variant is available:
  - The method name comes after the BAdI reference variable, which has the static type of `CL_BADI_BASE`, and is specified as a character-like data object within parentheses.
  - You can either specify the parameter list as in static statements or use a parameter table with `PARAMETER-TABLE`, which requires a table of type `abap_parmbind_tab` (refer to dynamic `CALL METHOD` statements in the _Dynamic Programming_ cheat sheet).

 </td>
</tr>

</table>

> [!NOTE]
> Note that various catchable exceptions can be raised when using `GET BADI` and `CALL BADI`.


The following snippet uses the demo BAdIs outlined earlier. For more example snippets, see the [executable example](#executable-example).

```abap
*&---------------------------------------------------------------------*
*& Static variant of the GET BADI and CALL BADI statements
*&---------------------------------------------------------------------*

"Declaring a BAdI reference variable
"The variable references the multiple-use demo BAdI definition.
DATA badi_a TYPE REF TO zbadi_demo_abap_converter.

"Creating a BAdI object
"The demo BAdI does not specify filters.
GET BADI badi_a.

"Calling a BAdI method through the BAdI reference variable
"and providing actual parameters to formal parameters.
CALL BADI badi_a->convert
  EXPORTING
    content = `Hello world`
    out     = out.

"Declaring another BAdI reference variable
"The variable references the single-use demo BAdI.
DATA badi_b TYPE REF TO zbadi_demo_abap_calculator.

"The demo BAdI specifies a filter condition. Therefore, the GET BADI
"statement requires to specify the FILTERS addition.
GET BADI badi_b FILTERS operator = '-'.

"Not that unlike methods of multiple-use BAdIs, methods of single-use
"BAdIs can include output parameters in the signature.
CALL BADI badi_b->calculate
  EXPORTING
    num1   = 10
    num2   = 8
  RECEIVING
    result = DATA(res).

*&---------------------------------------------------------------------*
*& Dynamic variants of the GET BADI and CALL BADI statements
*&---------------------------------------------------------------------*

"Character-like data objects to be used in the dynamic statements
DATA(badi_name) = 'ZBADI_DEMO_ABAP_CALCULATOR'.
DATA(method_name) = 'CALCULATE'.
DATA dyn_result TYPE string.

"Declaring a BAdI reference variable
"The static type of the reference variable must reference CL_BADI_BASE,
"the superclass of all BAdI classes.
DATA badi_dyn TYPE REF TO cl_badi_base.

"Dynamic GET BADI statement specifying the TYPE addition and
"the name of a BAdI (a data object containing it) in parentheses.
GET BADI badi_dyn TYPE (badi_name) FILTERS operator = '-'.

"Dynamic CALL BADI statement specifying the method name
"dyanmically (i.e. a data object containing the name) in
"parentheses following object component selector ->.
CALL BADI badi_dyn->(method_name)
  EXPORTING
    num1   = 10
    num2   = 8
  RECEIVING
    result = dyn_result.

"Using the FILTER-TABLE addition in dynamic GET BADI statements
"After the addition, a filter table of type badi_filter_bindings is
"expected.
DATA(op) = '/'.
DATA(filter_name) = CONV badi_filter_name( 'OPERATOR' ).

DATA(filter_tab) = VALUE badi_filter_bindings( ( name  = filter_name
                                                 value = REF #( op ) ) ).

GET BADI badi_dyn TYPE (badi_name) FILTER-TABLE filter_tab.

CALL BADI badi_dyn->(method_name)
  EXPORTING
    num1   = 10
    num2   = 5
  RECEIVING
    result = dyn_result.

"Using the PARAMETER-TABLE addition in dynamic CALL BADI statements
"Similar to dynamic CALL METHOD statements, dynamic CALL BADI statements
"can specify a parameter table of type abap_parmbind_tab.
DATA(ptab) = VALUE abap_parmbind_tab( ( name  = 'NUM1'
                                        kind  = cl_abap_objectdescr=>exporting
                                        value = NEW i( 10 ) )
                                      ( name  = 'NUM2'
                                        kind  = cl_abap_objectdescr=>exporting
                                        value = NEW i( 5 ) )
                                      ( name  = 'RESULT'
                                        kind  = cl_abap_objectdescr=>returning
                                        value = REF #( dyn_result ) ) ).

CALL BADI badi_dyn->(method_name) PARAMETER-TABLE ptab.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information

- [Enhancements Using BAdIs (ABAP Keyword Documentation)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbadi_enhancement.htm)
- [Business Add-Ins (BAdIs) on SAP Help Portal](https://help.sap.com/docs/ABAP_PLATFORM_NEW/46a2cfc13d25463b8b9a3d2a3c3ba0d9/8ff2e540f8648431e10000000a1550b0.html?version=LATEST&locale=en-US) (Note that the context of the documentation is classic ABAP)
- [Extend SAP S/4HANA in the cloud and on premise with ABAP based extensions](https://www.sap.com/documents/2022/10/52e0cd9b-497e-0010-bca6-c68f7e60039b.html)  
- Exploring usable standard BAdIs:
  - You can go to [https://api.sap.com/](https://api.sap.com/).
  - Then, you can, for example, choose _Explore_ from the menu -> _Categories -> Business Add-Ins (BAdIs)_. Apply appropriate filtering if required.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

> [!NOTE] 
> - The commented example illustrates various `GET BADI` and `CALL BADI` statements. 
> - As a prerequisite, you have walked through the [previous steps](#excursion-demo-badis) and created the demo BAdIs.
> - To try the example out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES c1 TYPE c LENGTH 1.
    DATA: operand1       TYPE i,
          operand2       TYPE i,
          op             TYPE c1,
          example_number TYPE i.
    METHODS set_example_divider IMPORTING out  TYPE REF TO if_oo_adt_classrun_out
                                          text TYPE string.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------*
*& Multiple-use demo BAdI
*&---------------------------------------------------------------------*

    "The multiple-use demo BAdI does not specify filters. Therefore, the syntax
    "does not allow the use of the FILTERS addition. Two demo BAdI implementations
    "are available in the demo, converting data objects to XML and JSON. When
    "calling the BAdI method, it executes both implementations. Consequently,
    "the output includes data objects converted to both XML and JSON.

    example_number += 1.
    set_example_divider( out  = out
                         text = |{ example_number }) Multiple-use BAdI| ).

    "Creating demo data objects for the example to work with
    DATA str TYPE string VALUE `Hello world`.

    DATA: BEGIN OF struct,
            comp1 TYPE string VALUE `ABAP`,
            comp2 TYPE i VALUE 987,
            comp3 TYPE n LENGTH 5 VALUE '12345',
            comp4 TYPE abap_boolean VALUE abap_true,
          END OF struct.

    DATA itab LIKE TABLE OF struct WITH EMPTY KEY.
    itab = VALUE #( ( struct )
                    ( comp1 = `BAdI` comp2 = 321 comp3 = '67890' comp4 = abap_false ) ).

    "Declaring a BAdI reference variable
    "The variable references the multiple-use demo BAdI definition.
    DATA badi_m TYPE REF TO zbadi_demo_abap_converter.

    "Creating a BAdI object
    GET BADI badi_m.

    out->write( |---------- Converting elementary data objects ----------\n| ).

    CALL BADI badi_m->convert
      EXPORTING
        content = str
        out     = out.

    out->write( |---------- Converting a structure ----------\n| ).

    CALL BADI badi_m->convert
      EXPORTING
        content = struct
        out     = out.

    out->write( |---------- Converting an internal table ----------\n| ).

    CALL BADI badi_m->convert
      EXPORTING
        content = itab
        out     = out.

*&---------------------------------------------------------------------*
*& Single-use demo BAdI
*&---------------------------------------------------------------------*

    "The single-use demo BAdI specifies filters. Therefore, the GET BADI
    "statement requires to specify the FILTERS addition.
    "Additionally, a fallback class is created for the demo BAdI. It
    "means that if an implementation class is not found that matches
    "the filter condition, the BAdI method implemented in the fallback
    "class is called.

    example_number += 1.
    set_example_divider( out  = out
                         text = |{ example_number }) Single-use demo BAdI| ).

    "Declaring a BAdI reference variable
    "The variable references the single-use demo BAdI definition.
    DATA badi_s TYPE REF TO zbadi_demo_abap_calculator.

    "Assigning demo values to data objects used for the BAdI
    "method calls
    operand1 = 10.
    operand2 = 8.
    op = '-'.

    "Creating a BAdI object
    "Note that the demo BAdI requires filters to be specified.
    GET BADI badi_s FILTERS operator = op.

    CALL BADI badi_s->calculate
      EXPORTING
        num1   = operand1
        num2   = operand2
      RECEIVING
        result = DATA(result).

    out->write( |{ operand1 } { op } { operand2 } = { result }| ).
    out->write( |\n| ).

    op = '/'.

    GET BADI badi_s FILTERS operator = op.

    CALL BADI badi_s->calculate
      EXPORTING
        num1   = operand1
        num2   = operand2
      RECEIVING
        result = result.

    out->write( |{ operand1 } { op } { operand2 } = { result }| ).
    out->write( |\n| ).

    operand1 = 5.
    operand2 = 0.

    CALL BADI badi_s->calculate
      EXPORTING
        num1   = operand1
        num2   = operand2
      RECEIVING
        result = result.

    out->write( |{ operand1 } { op } { operand2 } = { result }| ).
    out->write( |\n| ).

    "Using a non-specified filter
    "In this case, the BAdI method in the fallback class is called,
    "reflecting addition.
    op = '?'.

    GET BADI badi_s FILTERS operator = op.

    operand1 = 3.
    operand2 = 2.

    CALL BADI badi_s->calculate
      EXPORTING
        num1   = operand1
        num2   = operand2
      RECEIVING
        result = result.

    out->write( |{ operand1 } { op } { operand2 } = { result }| ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Dynamic specifications
*&---------------------------------------------------------------------*

    example_number += 1.
    set_example_divider( out  = out
                         text = |{ example_number }) Dynamic specifications| ).

    "Dynamic variant of the GET BADI statement
    "Creating character-like data objects holding the names of a BAdI
    "and BAdI method
    DATA(badi_name) = 'ZBADI_DEMO_ABAP_CALCULATOR'.
    DATA(method_name) = 'CALCULATE'.

    operand1 = 15.
    operand2 = 9.
    op = '-'.

    "The dynamic variant of the GET BADI statement specifies the TYPE addition,
    "followed by the BAdI name (contained in a character-like data object) in
    "parentheses. The BAdI reference variable must have the static type CL_BADI_BASE,
    "the superclass of all BAdI classes.
    DATA badi_dyn TYPE REF TO cl_badi_base.

    GET BADI badi_dyn TYPE (badi_name) FILTERS operator = op.

    "The dynamic variant of the CALL BADI statement specifies the object component
    "selector -> followed by the BAdI method name (contained in a character-like
    "data object) in parentheses. The following statement explicitly specifies the
    "parameters, similar to dynamic CALL METHOD statements.
    CALL BADI badi_dyn->(method_name)
      EXPORTING
        num1   = operand1
        num2   = operand2
      RECEIVING
        result = result.

    out->write( |{ operand1 } { op } { operand2 } = { result }| ).
    out->write( |\n| ).

    "Using the FILTER-TABLE addition in dynamic GET BADI statements
    "After the addition, a filter table of type badi_filter_bindings is
    "expected.
    DATA filter_tab TYPE badi_filter_bindings.
    DATA badi_filter_name TYPE badi_filter_name.

    badi_filter_name = 'OPERATOR'.
    op = '/'.
    operand1 = 30.
    operand2 = 6.

    filter_tab = VALUE #( ( name  = badi_filter_name
                            value = NEW c1( op ) ) ).

    GET BADI badi_dyn TYPE (badi_name) FILTER-TABLE filter_tab.

    CALL BADI badi_dyn->(method_name)
      EXPORTING
        num1   = operand1
        num2   = operand2
      RECEIVING
        result = result.

    out->write( |{ operand1 } { op } { operand2 } = { result }| ).
    out->write( |\n| ).

    "Using the PARAMETER-TABLE addition in dynamic CALL BADI statements
    "Similar to dynamic CALL METHOD statements, dynamic CALL BADI statements
    "can specify a parameter table of type abap_parmbind_tab.
    badi_filter_name = 'OPERATOR'.
    op = '/'.
    operand1 = 1.
    operand2 = 8.

    filter_tab = VALUE #( ( name  = badi_filter_name
                            value = NEW c1( op ) ) ).

    GET BADI badi_dyn TYPE (badi_name) FILTER-TABLE filter_tab.

    DATA(ptab) = VALUE abap_parmbind_tab( ( name  = 'NUM1'
                                            kind  = cl_abap_objectdescr=>exporting
                                            value = REF #( operand1 ) )
                                          ( name  = 'NUM2'
                                            kind  = cl_abap_objectdescr=>exporting
                                            value = REF #( operand2 ) )
                                          ( name  = 'RESULT'
                                            kind  = cl_abap_objectdescr=>returning
                                            value = REF #( result ) ) ).

    CALL BADI badi_dyn->(method_name) PARAMETER-TABLE ptab.

    out->write( |{ operand1 } { op } { operand2 } = { result }| ).
    out->write( |\n| ).

    "Note:
    "The following statement is not possible as the BAdI reference variable
    "is typed with type ref to zbadi_demo_abap_calculator. A reference to
    "the superclass CL_BADI_BASE is expected.
    "GET BADI badi_s TYPE (name) FILTERS operator = '-'.

*&---------------------------------------------------------------------*
*& Exceptions
*&---------------------------------------------------------------------*

    example_number += 1.
    set_example_divider( out  = out
                         text = |{ example_number }) Demonstrating selected exceptions| ).

    DATA error TYPE REF TO cx_root.

    "1) Filter not bound
    CLEAR filter_tab.
    TRY.
        GET BADI badi_dyn TYPE (badi_name) FILTER-TABLE filter_tab.
      CATCH cx_badi_filter_error INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

    out->write( |\n| ).

    "2) Filter not bound (not specifying filters at all)
    TRY.
        GET BADI badi_dyn TYPE (badi_name).
      CATCH cx_badi_filter_error INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

    out->write( |\n| ).

    "3) Non-existant BAdI name
    DATA badi_dyn_2 TYPE REF TO cl_badi_base.
    badi_name = 'NOPE'.

    TRY.
        GET BADI badi_dyn_2 TYPE (badi_name).
      CATCH cx_badi_filter_error INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

    out->write( |\n| ).

    "4) Wrong parameters (you might know the exception class
    "   from regular dynamic method calls)
    badi_name = 'ZBADI_DEMO_ABAP_CONVERTER'.
    method_name = 'CONVERT'.

    GET BADI badi_dyn_2 TYPE (badi_name).

    TRY.
        CALL BADI badi_dyn_2->(method_name)
          EXPORTING
            nope1 = str
            nope2 = out.
      CATCH cx_sy_dyn_call_param_missing INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

    out->write( |\n| ).

    "5) Wrong method name (you might know the exception class
    "   from regular dynamic method calls)
    TRY.
        CALL BADI badi_dyn_2->('NOPE')
          EXPORTING
            content = str
            out     = out.
      CATCH cx_sy_dyn_call_illegal_method INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

    out->write( |\n| ).

    "6) Initial BAdI reference variable
    CLEAR badi_dyn_2.

    TRY.
        CALL BADI badi_dyn_2->('CONVERT')
          EXPORTING
            content = str
            out     = out.
      CATCH cx_badi_initial_reference INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

    out->write( |\n| ).

    "7) No exception raised here, but demonstrating the following:
    "   In case of an initial BAdI reference variable whose static type
    "   refers to a multiple-use BAdI, there is no effect of the statement
    "   and no exception raised. In the console, there should not be any
    "   output displayed between the two dividers.
    GET BADI badi_m.

    CLEAR badi_m.

    out->write( `----------` ).

    CALL BADI badi_m->convert
      EXPORTING
        content = 123
        out     = out.

    out->write( `----------` ).
    out->write( |\n| ).

    "8) BAdI reference variable whose static type refers to a single-use
    "   BAdI: Exception is raised in case of an initial BAdI reference variable.
    op = '-'.
    operand1 = 3.
    operand2 = 2.

    GET BADI badi_s FILTERS operator = op.

    CLEAR badi_s.

    TRY.
        CALL BADI badi_s->calculate
          EXPORTING
            num1   = operand1
            num2   = operand2
          RECEIVING
            result = result.

        out->write( |{ operand1 } { op } { operand2 } = { result }| ).
      CATCH cx_badi_initial_reference INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD set_example_divider.
    out->write( |*&{ repeat( val = `-` occ = 70 ) }*| ).
    out->write( |*& { text }| ).
    out->write( |*&{ repeat( val = `-` occ = 70 ) }*| ).
    out->write( |\n| ).
  ENDMETHOD.

ENDCLASS.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>