<a name="top"></a>

# Misc ABAP Classes

- [Misc ABAP Classes](#misc-abap-classes)
  - [Excursion: Available Classes in ABAP for Cloud Development](#excursion-available-classes-in-abap-for-cloud-development)
  - [Creating and Transforming UUIDs](#creating-and-transforming-uuids)
  - [Displaying Output in the ADT Console](#displaying-output-in-the-adt-console)
  - [RAP](#rap)
  - [Transactional Consistency](#transactional-consistency)
  - [Numbers and Calculations](#numbers-and-calculations)
  - [String Processing](#string-processing)
  - [Time and Date](#time-and-date)
  - [Runtime Type Services (RTTS)](#runtime-type-services-rtts)
  - [Assignments](#assignments)
  - [Information about Non-Initial Structure Components](#information-about-non-initial-structure-components)
  - [Comparing Content of Compatible Internal Tables](#comparing-content-of-compatible-internal-tables)
  - [Dynamic Programming](#dynamic-programming)
  - [Context Information](#context-information)
  - [XML/JSON](#xmljson)
  - [ABAP Repository Object Information](#abap-repository-object-information)
  - [Generating ABAP Repository Objects](#generating-abap-repository-objects)
  - [Call Stack](#call-stack)
  - [Sending Emails](#sending-emails)
  - [Tenant Information](#tenant-information)
  - [Exception Classes](#exception-classes)
  - [Parallel Processing](#parallel-processing)
  - [Application Log](#application-log)
  - [Running Code in the Background](#running-code-in-the-background)
  - [Locking](#locking)
  - [Calling Services](#calling-services)
  - [Reading and Writing XLSX Content](#reading-and-writing-xlsx-content)


This ABAP cheat sheet contains a selection of available ABAP classes, serving as a quick introduction, along with code snippets to explore the functionality in action.

> **💡 Note**<br>
> - The cheat sheet is not a comprehensive overview, and the code snippets do not claim to be comprehensive as far as options, methods, or parameters are concerned. It is intended to give you a rough overview, for you to get an idea. It is an invitation to a more in-depth exploration.
> - For more information and where available, refer to the class documentation (for example, choose F2 when the cursor is on the class name in ADT), the ABAP Keyword Documentation, and the SAP Help Portal documentation.
> - In the cheat sheet, the focus is on a selected set of classes that are available in [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)

## Excursion: Available Classes in ABAP for Cloud Development

If available to you, you have accessed an [SAP BTP ABAP Environment](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensap_btp_abap_env_glosry.htm) using the [ABAP development tools for Eclipse (ADT)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenadt_glosry.htm).
Access to SAP-provided repository objects is restricted to objects that have been released for [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm) ([released APIs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreleased_api_glosry.htm)). You can find the released repository objects in the *Project Explorer* view in ADT under *Released Objects*. The classes are located in the *Source Code Library* folder:

![Released APIs](./files/released_APIs.png)

You can also programmatically get the released obects. You can use specific XCO classes or a CDS view, as shown in the example code snippet below.

```abap
SELECT ReleasedObjectType, ReleasedObjectName, ReleaseState
  FROM i_apisforclouddevelopment
  WHERE releasestate = 'RELEASED'
  AND ReleasedObjectType = 'CLAS'
  ORDER BY ReleasedObjectName
  INTO TABLE @DATA(released_classes).
```

## Creating and Transforming UUIDs

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_SYSTEM_UUID</code> </td>
<td>
Creating and and converting system UUIDs with various algorithms 
<br><br>

``` abap
"Creating UUIDs in binary format (16 bytes)
TRY.
    DATA(uuid) = cl_system_uuid=>create_uuid_x16_static( ) .
  CATCH cx_uuid_error.
ENDTRY.

"e.g. B2B012691AC31EDEADA0A495A7130961
``` 

</td>
</tr>
<tr>
<td> <code>XCO_CP_UUID</code> </td>
<td>
Transforming between different UUID formats
<br><br>

``` abap
DATA(uuid_c36) = xco_cp_uuid=>format->c36->to_uuid( '7cd44fff-036a-4155-b0d2-f5a4dfbcee92' ).

"7CD44FFF036A4155B0D2F5A4DFBCEE92
DATA(uuid_c36_to_c32) =  CONV sysuuid_c32( xco_cp_uuid=>format->c32->from_uuid( uuid_c36 ) ).

"7cd44fff-036a-4155-b0d2-f5a4dfbcee92
DATA(uuid_c32_to_c36) = to_lower( CONV sysuuid_c36( xco_cp_uuid=>format->c36->from_uuid( uuid_c36 ) ) ).
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Displaying Output in the ADT Console

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_DEMO_CLASSRUN</code> </td>
<td>
As an alternative to using the <code>IF_OO_ADT_CLASSRUN</code> interface for displaying output in the console, you can also use the <code>CL_DEMO_CLASSRUN</code> class, which offers more methods.
For more information, refer to <a href="https://blogs.sap.com/2023/10/24/abap-console-reloaded/">this blog</a>.
The following example makes use of the <code>CL_DEMO_CLASSRUN</code>. A structure and an internal table are displayed in the console. A structure component is a reference variable, which is automatically dereferenced. Plus, the <code>write_xml</code> method is shown, which displays XML data.
<br><br>

``` abap
CLASS zcl_some_class DEFINITION
  INHERITING FROM cl_demo_classrun
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS main REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_some_class IMPLEMENTATION.
  METHOD main.
    TYPES: BEGIN OF s,
             comp1 TYPE string,
             comp2 TYPE i,
             comp3 TYPE string_table,
             comp4 TYPE REF TO string,
           END OF s,
           it_type TYPE TABLE OF s WITH EMPTY KEY.

    DATA(struct) = VALUE s( comp1 = `Hello`
                            comp2 = 1
                            comp3 = VALUE #( ( `a` ) ( `b` ) ( `a` ) ( `p` ) )
                            comp4 = NEW #( `world` ) ).

    DATA(itab) = VALUE it_type( ( struct )
                                ( comp1 = `Hi`
                                  comp2 = 2
                                  comp3 = VALUE #( ( `x` ) ( `y` ) ( `z` ) )
                                  comp4 = NEW #( `ABAP` ) ) ).

    out->write( struct ).
    out->write( itab ).

    DATA(some_xml) = cl_abap_conv_codepage=>create_out( )->convert(
    `<hi><word1>hallo</word1><word2>how</word2><word3>are</word3><word4>you</word4></hi>` ).

    out->write( some_xml ).
    out->write_xml( some_xml ).
  ENDMETHOD.
ENDCLASS.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## RAP

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_BEHV_AUX</code> </td>
<td>
A utility class for retrieving information about RAP handler implementations, such as the current context of RAP handler/saver methods, the handler kind, and the current <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentransactional_phase_glosry.htm">RAP transactional phase</a> (e.g., <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_int_phase_glosry.htm">RAP interaction phase</a>.
<br><br>

``` abap
"Getting the current context of RAP handler implementations; 
"storing the information (true/false) in a variable declared inline
cl_abap_behv_aux=>get_current_context( IMPORTING from_projection = FINAL(fr_proj) 
                                                 from_interface  = FINAL(fr_int) 
                                                 in_local_mode   = FINAL(loc_mode) 
                                                 draft_activate  = FINAL(dr_act) 
                                                 for_permissions = FINAL(for_perm) 
                                                 privileged      = FINAL(priv) ).


"Getting the root entity name and handler kind
"For the latter, you can check the fixed values of abp_behv_kind (e.g., 'R' stands for read)  
DATA ent TYPE abp_root_entity_name.
FINAL(handler_kind) = cl_abap_behv_aux=>get_current_handler_kind( IMPORTING root_entity = ent ).

"Getting information about the current transactional phase
FINAL(phase) = cl_abap_behv_aux=>get_current_phase( ). 
"e.g. INTERACTION or EARLY_SAVE
``` 

</td>
</tr>
<tr>
<td> <code>CL_ABAP_BEHAVIOR_HANDLER</code> </td>
<td>
Used for <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_handler_class_glosry.htm">RAP handler classes</a> that inherit from class <code>CL_ABAP_BEHAVIOR_HANDLER</code>. 
<br><br>

``` abap
"Skeleton of a RAP handler class declaration part in a CCIMP include
CLASS lcl_handler DEFINITION INHERITING FROM cl_abap_behavior_handler. 
  PRIVATE SECTION. 
  ... "Here go RAP handler method declarations
ENDCLASS. 

...
``` 

</td>
</tr>

<tr>
<td> <code>CL_ABAP_BEHAVIOR_SAVER</code> </td>
<td>
Used as base class from which a <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_saver_class_glosry.htm">RAP saver class</a> in an <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbehavior_pool_glosry.htm">ABAP behavior pool (ABP)</a> inherits. The RAP saver class must be defined in the <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenccimp_glosry.htm">CCIMP include</a> of an ABP. 
<br><br>

``` abap
"Skeleton of a RAP saver class declaration part in a CCIMP include
CLASS lsc_bdef DEFINITION INHERITING FROM cl_abap_behavior_saver. 
  PROTECTED SECTION. 
     METHODS finalize REDEFINITION. 
     METHODS check_before_save REDEFINITION. 
     METHODS save REDEFINITION.      
     METHODS cleanup REDEFINITION. 
     METHODS cleanup_finalize REDEFINITION. 
ENDCLASS. 

...
``` 

</td>
</tr>
<tr>
<td> <code>CL_ABAP_BEHAVIOR_SAVER_FAILED</code> </td>
<td>
<ul>
<li>Same as <code>CL_ABAP_BEHAVIOR_SAVER</code>. It is used as base class from which a RAP saver class in an ABAP behavior pool (ABP) inherits. </li>
<li>Normally, the basic rule is that failures must not occur in the RAP late save phase, but must be detected in the <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenearly_rap_save_phase_glosry.htm">RAP early save phase</a> in order for the save to be successful. The base class <code>CL_ABAP_BEHAVIOR_SAVER_FAILED</code> can be used in exceptional cases where the basic rule cannot be met.</li>
<li>For more information, see the <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_cl_abap_beh_saver_failed.htm">ABAP Keyword Documentation</a>.</li>
</ul>
<br>

``` abap
"Skeleton of a RAP saver class declaration part in a CCIMP include
CLASS lsc_bdef DEFINITION INHERITING FROM cl_abap_behavior_saver_failed. 
  PROTECTED SECTION.    
     ...
     METHODS save REDEFINITION.
     "Unlike in CL_ABAP_BEHAVIOR_SAVER, the RAP response parameters failed and reported are included.
     ...
ENDCLASS. 

...
``` 

</td>
</tr>
<tr>
<td> <code>CL_ABAP_BEHAVIOR_EVENT_HANDLER</code> </td>
<td>
It is used as base class from which a RAP event handler class in its CCIMP include inherits. Its purpose is to locally consume RAP business events.
More information: <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_events.htm">ABAP for RAP Business Events</a>.
<br><br>

``` abap
"Skeleton of a RAP event handler class declaration part in a CCIMP include
"The assumption is that there are the events 'created', 'updated', and 
"'deleted' specified in the BDEF.
CLASS lhe_event DEFINITION INHERITING FROM cl_abap_behavior_event_handler.

  PRIVATE SECTION.

    METHODS on_updated FOR ENTITY EVENT
       updated FOR some_bdef~updated.

    METHODS on_deleted FOR ENTITY EVENT
       deleted FOR some_bdef~deleted.

    METHODS on_created FOR ENTITY EVENT
       created FOR some_bdef~created.

ENDCLASS. 

...
```

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Transactional Consistency

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_TX</code> </td>
<td>
<ul>
<li>Explicitly setting <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentr_phase_glosry.htm">transactional phases</a> (the modify and save transactional phase) to enable transactional consistency checks with the <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencontrolled_sap_luw_glosry.htm">controlled SAP LUW</a> (which is an extension of the SAP LUW concept)</li>
<li>The controlled SAP LUW is automatically and implicitly supported by newer ABAP concepts such as RAP, i.e. the transactional phases are implicitly active when RAP handler methods are called.</li>
<li>Operations that are not allowed in a transactional phase are detected, resulting in a runtime error in certain contexts (or the violations are logged). 
For example, database modifications are only allowed in the save transactional phase because the data being processed in the modify phase may be inconsistent. Therefore, a database modification in the modify phase can disrupt the SAP LUW.</li>
<li>For more information, refer to the chapter <a href="https://help.sap.com/docs/abap-cloud/abap-concepts/controlled-sap-luw">Controlled SAP LUW</a>.</li>
<li>See the restrictions in the ABAP Keyword Documentation: <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapinvalid_stmts_in_tx.htm">ABAP for Cloud Development</a> / <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapinvalid_stmts_in_tx.htm">Standard ABAP</a>.</li>
</ul>
<br>

``` abap
...

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

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Numbers and Calculations

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_MATH</code> </td>
<td>
For operations with (decimal) floating point numbers and for providing constants for minimum and maximum values.
<br><br>

``` abap
"Constants for the minimum and maximum values of built-in numeric types
"Example: Type i
DATA(min_int4) = cl_abap_math=>min_int4. "-2147483648  
DATA(max_int4) = cl_abap_math=>max_int4. "2147483647 

"Rounding binary floating point number to 15 places using commercial rounding
DATA flpnum TYPE f VALUE '1.005'.
DATA(rd) = cl_abap_math=>round_f_to_15_decs( flpnum ). "1.005000000000001E0
DATA(str2dcm) = |{ flpnum DECIMALS = 2 }|. "1.00
DATA(str2dcm_rd) = |{ cl_abap_math=>round_f_to_15_decs( flpnum ) DECIMALS = 2 }|. "1.01

"Properties of decimal floating point numbers
"Example: 98.765; an integer with fixed precision, i.e. a given length (the length of 98765),
"which is scaled by dividing through a power of 10 (10 powered by n, representing
"the number of decimal places; in the example, the scaling is the negative exponent)
DATA(decf) = CONV decfloat34( '98.765' ).
DATA(scale) = cl_abap_math=>get_scale( decf ). "3
DATA(precision) = cl_abap_math=>get_number_of_digits( decf ). "5
``` 

</td>
</tr>
<tr>
<td> <code>CL_ABAP_DECFLOAT</code> </td>
<td>
For handling decimal floating point numbers.
<br><br>

``` abap
"Converting currency amounts to decimal floating point numbers using a
"a currency key. The reverse is also possible.
DATA(dec) = CONV decfloat34( '12340'
                             "'123.456'
                            ).
DATA pdec TYPE p LENGTH 9 DECIMALS 2.
cl_abap_decfloat=>convert_decfloat_to_curr( EXPORTING amount_decfloat = dec
                                                      cuky            = 'EUR'
                                            IMPORTING amount_curr = pdec ) .
"12340.0
"123.46

"Converting strings to decimal floating point numbers
"The character string is converted to a value of type decfloat34,
"which is passed back in VALUE.
DATA dcfl34 TYPE decfloat34.
DATA return_code TYPE i.
DATA str TYPE string VALUE `1234.8652`.
TRY.
    cl_abap_decfloat=>read_decfloat34( EXPORTING string = str
                                        IMPORTING value  = dcfl34
                                                  rc     = return_code ).
    CATCH cx_sy_conversion_overflow cx_abap_decfloat_invalid_char cx_abap_decfloat_parse_err.
ENDTRY.
``` 

</td>
</tr>
<tr>
<td> <code>CL_ABAP_BIGINT</code> </td>
<td>
For calculations with integers of any size (e.g. to avoid the risk of an arithmetic overflow). Find more information in
<a href="https://blogs.sap.com/2023/08/09/new-classes-for-arbitrary-precision-arithmetic-in-abap/">this blog</a>, and check out the different methods available.
<br><br>

``` abap
"Creating an arbitrary precision integer from a variable of type i
"and calculating the power; converting the result to string
DATA(pow1) = cl_abap_bigint=>factory_from_int4( 10 )->pow( 10 )->to_string( ).
"Comparison with integer value (the maximum value of type i; see the class above)
DATA(compare_i) = cl_abap_bigint=>factory_from_int4( 10 )->pow( 10 )->compare_int4( cl_abap_math=>max_int4 ). "LARGER

"10000000000 (result)
"2147483647  (maximum value for type i)

"Comparison with the ipow function (here, an exception is raised)
TRY.
    DATA(pow2) = ipow( base = 10 exp = 10 ).
  CATCH cx_sy_arithmetic_overflow.
ENDTRY.
``` 

</td>
</tr>
<tr>
<td> <code>CL_ABAP_RATIONAL</code> </td>
<td>
For calculations with rational numbers without precision loss and rounding errors.
<br><br>

``` abap
"Creating a rational number from a string
DATA(rat_num) = cl_abap_rational=>factory_from_string(
  EXPORTING iv_value = `-1/3` ).

"Performing an addition and converting the result to string
DATA(addition_res) = rat_num->add( cl_abap_rational=>factory_from_string(
  EXPORTING iv_value = `3/2` ) )->to_string( ). "7/6
``` 

</td>
</tr>
<td> <code>CL_ABAP_RANDOM*</code> </td>
<td>
For generating arbitrary numbers for different numeric types: 
<code>CL_ABAP_RANDOM_INT</code> (type <code>i</code>), 
<code>CL_ABAP_RANDOM_INT8</code> (<code>int8</code>), 
<code>CL_ABAP_RANDOM_FLOAT</code> (<code>f</code>),
<code>CL_ABAP_RANDOM_PACKED</code> (<code>p</code>),
<code>CL_ABAP_RANDOM_PACKED_DEC1</code> - <code>CL_ABAP_RANDOM_PACKED_DEC14</code> (<code>p</code> with 1 to 14 decimal places),
<code>CL_ABAP_RANDOM_DECFLOAT16</code> (<code>decfloat16</code>),
<code>CL_ABAP_RANDOM_DECFLOAT34</code> (<code>decfloat34</code>)
<br><br>

``` abap
"Getting multiple random integers that are to be stored in an 
"internal table of type i
TYPES int_tab_type TYPE TABLE OF i WITH EMPTY KEY.
DATA int_tab TYPE int_tab_type.

"The optional parameters are explicitly specified in the example;
"'seed' represents the initial starting number, you can use 
"'cl_abap_random=>seed( )' to specify an arbitrary start value
DATA(random_num1) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                min  = 1
                                                max  = 100 ).
DO 3 TIMES.
  APPEND random_num1->get_next( ) TO int_tab.
ENDDO.

"Getting a random integer in one go using method chaining
DATA(random_num2) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                min  = 100
                                                max  = 1000 )->get_next( ).
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## String Processing 

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_GZIP*</code> </td>
<td>
For (de)compressing character strings and byte strings using GZIP: 
<code>CL_ABAP_GZIP</code>, 
<code>CL_ABAP_GZIP_BINARY_STREAM</code>,
<code>CL_ABAP_GZIP_TEXT_STREAM</code>,
<code>CL_ABAP_UNGZIP_BINARY_STREAM</code>,
<code>CL_ABAP_UNGZIP_TEXT_STREAM</code>
<br><br>

``` abap
"------- (De)compressing binary data -------
DATA(str) = `This is a data object of type string. It should be converted to xstring, compressed and decompressed.`.
DATA(xstr) = cl_abap_conv_codepage=>create_out( )->convert( str ).
DATA xstr_comp TYPE xstring.

"Compressing binary data
TRY.
    cl_abap_gzip=>compress_binary( EXPORTING raw_in   = xstr
                                   IMPORTING gzip_out = xstr_comp ).
  CATCH cx_parameter_invalid_range cx_sy_buffer_overflow cx_sy_compression_error.
ENDTRY.

"Comparing the length of the data objects
DATA(len_xstr) = xstrlen( xstr ). "101
DATA(len_xstr_comp) = xstrlen( xstr_comp ). "81

"Decompressing binary data
DATA xstr_decomp TYPE xstring.
TRY.
    cl_abap_gzip=>decompress_binary( EXPORTING gzip_in = xstr_comp
                                     IMPORTING raw_out = xstr_decomp ).
  CATCH cx_parameter_invalid_range cx_sy_buffer_overflow cx_sy_compression_error.
ENDTRY.

DATA(len_xstr_decomp) = xstrlen( xstr_decomp ). "101
DATA(conv_str) = cl_abap_conv_codepage=>create_in( )->convert( xstr_decomp ).

"abap_true
DATA(is_equal) = xsdbool( len_xstr = len_xstr_decomp AND str = conv_str ). 

"------- (De)compressing character strings -------
DATA zipped TYPE xstring.
TRY.
    cl_abap_gzip=>compress_text( EXPORTING text_in  = `Hello world`
                                 IMPORTING gzip_out = zipped ).
  CATCH cx_parameter_invalid_range cx_sy_buffer_overflow cx_sy_conversion_codepage cx_sy_compression_error.
ENDTRY.

DATA txt TYPE string.
TRY.
    cl_abap_gzip=>decompress_text( EXPORTING gzip_in  = zipped
                                   IMPORTING text_out = txt ).
  CATCH cx_parameter_invalid_range cx_sy_buffer_overflow cx_sy_conversion_codepage cx_sy_compression_error.
ENDTRY.

ASSERT txt = `Hello world`.
``` 

</td>
</tr>
<tr>
<td> <code>CL_ABAP_CHAR_UTILITIES</code> </td>
<td> 
Provides utilities for string processing, such as attributes that represent new lines and horizontal tabs.
<br><br>

``` abap
DATA(tabbed) = `#` && cl_abap_char_utilities=>horizontal_tab && `#`.

"The following attributes can be replaced by a representation of
"the control characters in a string template.
ASSERT cl_abap_char_utilities=>newline        = |\n|.
ASSERT cl_abap_char_utilities=>horizontal_tab = |\t|.
ASSERT cl_abap_char_utilities=>cr_lf          = |\r\n|.
``` 

</td>
</tr>
<tr>
<td> <code>CL_ABAP_STRING_UTILITIES</code> </td>
<td> 
For processing text strings, such as handling trailing blanks in character strings (i.e. data objects of type <code>string</code>).
<br><br>

``` abap
DATA(string) = `ABAP   `.
"Removing trailing blanks
cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = string ).
"`ABAP`

"Preserving trailing blanks when assigning text fields to data objects of
"type string
DATA(chars) = 'ABAP   '.
cl_abap_string_utilities=>c2str_preserving_blanks( EXPORTING source = chars 
                                                   IMPORTING dest   = DATA(str_w_blanks) ).
"`ABAP   `

DATA(str_no_blanks) = CONV string( chars ).
"`ABAP`
``` 

</td>
</tr>
<tr>
<td> <code>CL_ABAP_CONV_CODEPAGE</code> </td>
<td> 
For handling code pages, converting strings to the binary representation of different code pages and vice versa. 
<br><br>

``` abap
DATA(hi) = `Hello world`.

"string -> xstring
"Note: UTF-8 is used by default. Here, it is specified explicitly.
TRY.
    DATA(conv_xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( hi ).
  CATCH cx_sy_conversion_codepage.
ENDTRY.
"48656C6C6F20776F726C64

"xstring -> string
DATA(conv_string) = cl_abap_conv_codepage=>create_in( )->convert( conv_xstring ).
"Hello world
``` 

</td>
</tr>
<tr>
<td> <code>CL_ABAP_REGEX</code> <br> <code>CL_ABAP_MATCHER</code> </td>
<td> 
<ul>
<li>For an object-oriented representation of regular expressions.</li>
<li>For example, the <code>CREATE_PCRE</code> method creates instances of regular expressions with PCRE syntax.</li>
<li>The instances can be used, for example, with the <code>CL_ABAP_MATCHER</code> class, which applies the regular expressions.</li>
<li>A variety of methods and parameters can be specified to accomplish various things and to further specify the handling of the regular expression.</li>
</ul>
<br>

``` abap
DATA(str) = `a1 # B2 ? cd . E3`.

"Creating a regex instance for PCRE regular expressions
"In the example, regex_inst has the type ref to cl_abap_regex. 
DATA(regex_inst) = cl_abap_regex=>create_pcre( pattern     = `\D\d`           "Any-non digit followed by a digit
                                               ignore_case = abap_true ).

"Creating an instance of CL_ABAP_MATCHER using the method CREATE_MATCHER of the class CL_ABAP_REGEX
"You can also specify internal tables with the 'table' parameter and more.
DATA(matcher) = regex_inst->create_matcher( text = str ).

"Finding all results using the 'find_all' method
"In the example, result has the type match_result_tab containing the findings.
DATA(result) = matcher->find_all( ).

"Using method chaining
DATA(res) = cl_abap_regex=>create_pcre( pattern     = `\s\w`        "Any blank followed by any word character
                                        ignore_case = abap_true )->create_matcher( text = str )->find_all( ).
``` 

</td>
</tr>
<tr>
<td> <code>XCO_CP</code> </td>
<td> 
Offers various options to process strings using the XCO Library; see a selection in the code snippet
<br><br>

``` abap
"--------- Extracting a substring from a string ---------
DATA(some_string) = `abcdefghijklmnopqrstuvwxyz`.

"Creating an encapsulation of a string using XCO
DATA(str) = xco_cp=>string( some_string ).

"Using the FROM and TO methods, you can determine
"the character position. Note that the value includes the
"character at the position specified.
"The character index pattern for the example string above
"is (the string has 26 characters in total):
"a = 1, b = 2, c = 3 ... z = 26
"a = -26, b = -25, c = -24 ... z = -1
"Providing a value that is out of bounds means that
"the first (or the last) character of the string is used
"by default.
"Note: When combining FROM and TO, e.g. with method
"chaining ...->from( ...)->to( ... ), note that another
"instance is created with the first 'from', and another
"character index pattern is created based on the new
"and adjusted string value.

"bcdefghijklmnopqrstuvwxyz
DATA(sub1) = str->from( 2 )->value.

"defghijklmnopqrstuvwxyz
DATA(sub2) = str->from( -23 )->value.

"vwxyz
DATA(sub3) = str->from( -5 )->value.

"abcde
DATA(sub4) = str->to( 5 )->value.

"ab
DATA(sub5) = str->to( -25 )->value.

"Result of 1st 'from' method call: bcdefghijklmnopqrstuvwxyz
"Based on this result, the 'to' method call is
"applied.
"bcdefg
DATA(sub6) = str->from( 2 )->to( 6 )->value.

"Result of 1st 'to' method call: abcdefghijklmnopq
"Based on this result, the 'from' method call is
"applied.
"defghijklmnopq
DATA(sub7) = str->to( -10 )->from( 4 )->value.

"Values that are out of bounds.
"In the example, the first and last character of the
"string are used.
"abcdefghijklmnopqrstuvwxyz
DATA(sub8) = str->from( 0 )->to( 100 )->value.

"--------- Splitting and joining ---------

"Splitting a string into a string table
DATA(str_table) = xco_cp=>string( `Hello.World.ABAP` )->split( `.` )->value.
"Hello
"World
"ABAP

"Concatenating a string table into a string; specifying a delimiter
str_table = VALUE #( ( `a` ) ( `b` ) ( `c` ) ).
"a, b, c
DATA(conc_str1) = xco_cp=>strings( str_table )->join( `, ` )->value.

"Concatenating a string table into a string; specifying a delimiter and
"reversing the table order
"c / b / a
DATA(conc_str2) = xco_cp=>strings( str_table )->reverse( )->join( ` / ` )->value.

"--------- Prepending and appending strings ---------
DATA(name) = xco_cp=>string( `Max Mustermann` ).

"Max Mustermann, Some Street 1, 12345 Someplace
DATA(address) = name->append( `, Some Street 1, 12345 Someplace` )->value.

"Mr. Max Mustermann
DATA(title) = name->prepend( `Mr. ` )->value.

"--------- Transforming to lowercase and uppercase ---------
"ABAP
DATA(to_upper) = xco_cp=>string( `abap` )->to_upper_case( )->value.

"hallo world
DATA(to_lower) = xco_cp=>string( `HALLO WORLD` )->to_lower_case( )->value.

"--------- Checking if a string starts/ends with a specific string ---------
DATA check TYPE string.
DATA(str_check) = xco_cp=>string( `Max Mustermann` ).

"yes
IF str_check->ends_with( `mann` ).
  check = `yes`.
ELSE.
  check = `no`.
ENDIF.

"no
IF str_check->starts_with( `John` ).
  check = `yes`.
ELSE.
  check = `no`.
ENDIF.

"--------- Converting strings to xstrings using a codepage ---------
"536F6D6520737472696E67
DATA(xstr) = xco_cp=>string( `Some string` )->as_xstring( xco_cp_character=>code_page->utf_8 )->value.

"--------- Camel case compositions and decompositions with split and join operations ---------
"Pascal case is also possible
"someValue
DATA(comp) = xco_cp=>string( `some_value` )->split( `_` )->compose( xco_cp_string=>composition->camel_case )->value.

"Camel case decomposition
"some_value
DATA(decomp) = xco_cp=>string( `someValue` )->decompose( xco_cp_string=>decomposition->camel_case )->join( `_` )->value.

"------ Processing Base64 representations of raw binary data ------
DATA(a_string) = `Hello world`.
"string -> xstring
"Result: 48656C6C6F20776F726C64
DATA(conv_xstring) = xco_cp=>string( a_string
  )->as_xstring( xco_cp_character=>code_page->utf_8
  )->value.
"Encoding of raw binary data into its Base64 representation
"Result: SGVsbG8gd29ybGQ=
DATA(raw2base64) = xco_cp=>xstring( conv_xstring
  )->as_string( xco_cp_binary=>text_encoding->base64
  )->value.
"Decoding of a Base64 representation into raw binary data
"Result: 48656C6C6F20776F726C64
DATA(base642raw) = xco_cp=>string( raw2base64
  )->as_xstring( xco_cp_binary=>text_encoding->base64
  )->value.
"xstring -> string
"Result: Hello world
DATA(conv_string_xco) = xco_cp=>xstring( base642raw
  )->as_string( xco_cp_character=>code_page->utf_8
  )->value.

"--------- Matching string against regular expression ---------
DATA match TYPE string.

"yes
IF xco_cp=>string( ` 1` )->matches( `\s\d` ).
  match = 'yes'.
ELSE.
  match = 'no'.
ENDIF.

"no
IF xco_cp=>string( ` X` )->matches( `\s\d` ).
  match = 'yes'.
ELSE.
  match = 'no'.
ENDIF.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Time and Date

> **💡 Note**<br>
> In [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm), do not use the date and time-related system fields such as `sy-datum` and `sy-uzeit`, and others. User-related time and date values can be retrieved using the XCO library. For code snippets, see the [Date, Time, and Time Stamp](23_Date_and_Time.md) cheat sheet.

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_UTCLONG</code> </td>
<td>
For handling time stamps in time stamp fields (data objects with type <code>utclong</code>).
<br><br>

``` abap
"utclong_current: Using the built-in function to create a UTC time stamp
DATA(low_timestamp)  = utclong_current( ).
"utclong_add: Using the built-in function to create a UTC time stamp and
"adding time values
DATA(high_timestamp) = utclong_add( val     = low_timestamp
                                     days    = 1
                                     hours   = 2
                                     minutes = 3
                                     seconds = 4 ).

"diff: Calculating time differences
"In the example, the returned values correspond to the ones added above.
cl_abap_utclong=>diff( EXPORTING high     = high_timestamp
                                 low      = low_timestamp
                        IMPORTING days    = DATA(days)
                                  hours   = DATA(hours)
                                  minutes = DATA(minutes)
                                  seconds = DATA(seconds) ).

"read: Reading a time stamp from a string
DATA(ts) = |{ utclong_current( ) TIMESTAMP = ENVIRONMENT TIMEZONE = 'UTC' }|.

TRY.

    cl_abap_utclong=>read( EXPORTING string   = ts
                                     timezone = 'UTC'
                           IMPORTING value    = DATA(utc_ts) ).
    CATCH cx_abap_utclong_invalid.
ENDTRY.
"e.g. 2024-01-01T13:01:54.546134Z
``` 

</td>
</tr>

<tr>
<td> <code>CL_ABAP_TSTMP</code> </td>
<td>
For calculating and converting time stamps in packed numbers (types <code>timestamp</code> and <code>timestampl</code>)
<br><br>

``` abap
"Creating a time stamp of type timestamp (the inline creation creates 
"a data object with that type by default)
GET TIME STAMP FIELD DATA(ts1). 
"e.g. 20240101131220

"Calculations for time stamps in packed numbers
"Adding 1 hour
DATA(ts2) = cl_abap_tstmp=>add( tstmp = ts1
                                secs  = 3600 ).
"e.g. 20240101141220.0000000

"Subtracting 2 hours
DATA(ts3) = cl_abap_tstmp=>subtractsecs( tstmp = ts1
                                            secs  = 7200 ).
"e.g. 20240101111220.0000000

"Type timestampl
DATA tsl1 TYPE timestampl.
GET TIME STAMP FIELD tsl1.
"e.g. 20240101131701.3309040

"Converting type timestampl to timestamp
DATA(ts4) = cl_abap_tstmp=>move_to_short( tsl1 ).
"e.g. 20240101131701

"Converting types timestamp/timestampl to UTCLONG
DATA(ts2utcl) = cl_abap_tstmp=>tstmp2utclong( tsl1 ).
"e.g. 2024-01-01 13:19:23.8622560

"Converting type utclong to timestamp
DATA(utcl2ts) = cl_abap_tstmp=>utclong2tstmp_short( ts2utcl ).
"e.g. 20240101132231

"Converting type utclong to timestampl
DATA(utcl2tsl) = cl_abap_tstmp=>utclong2tstmp( ts2utcl ).
"e.g. 20240101132231.0667200
``` 

</td>
</tr>


<tr>
<td> <code>XCO_CP_TIME</code> </td>
<td>
Class of the XCO time library that provides abstractions for getting and working with date and time information. Find more details <a href="https://help.sap.com/docs/btp/sap-business-technology-platform/time-library">here</a>. 
<br><br>

``` abap
"Creating a time stamp
"As a result (which is also true for other results below),
"you get a handler with which you can get further information
"(check the options following '->').
DATA(m_moment) = xco_cp_time=>moment(
  iv_year   = '2024'
  iv_month  = '01'
  iv_day    = '01'
  iv_hour   = '12'
  iv_minute = '34'
  iv_second = '55' ).

"Getting the created time stamp as a string
"20240101123455
DATA(m2_moment_string) = m_moment->as( xco_cp_time=>format->abap )->value.
"... and with other formats applied.
"20240101T123455
DATA(m3_moment_format_a) = m_moment->as( xco_cp_time=>format->iso_8601_basic )->value.
"2024-01-01T12:34:55
DATA(m4_moment_format_b) = m_moment->as( xco_cp_time=>format->iso_8601_extended )->value.

"Getting user time zone
"e.g. UTC
DATA(m1_user_time_zone) = xco_cp_time=>time_zone->user->value.

"Getting the current moment in the time zone of the current user
"e.g. 2024-01-01T08:54:39
DATA(m5_cur_moment4user) = xco_cp=>sy->moment( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->iso_8601_extended )->value.
"Current moment in UTC
"e.g. 2024-01-01T08:54:39 (result is the same as above in the case of the cheat sheet example)
DATA(m6_cur_moment_utc) = xco_cp=>sy->moment( xco_cp_time=>time_zone->utc )->as( xco_cp_time=>format->iso_8601_extended )->value.
"Current UNIX timestamp
"e.g. 1703863291
DATA(m7_unix_tstmp) = xco_cp=>sy->unix_timestamp( )->value.

"For the time stamp, you can also use the TIME method
"e.g. 10:27:59
DATA(m8_time) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->iso_8601_extended )->value.
"Getting second, minute, hour information
"e.g. 59
DATA(m9_seconds) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->second.
"e.g. 27
DATA(m10_minutes) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->minute.
"e.g. 10
DATA(m11_hours) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->hour.
"Calculations with time
"Adding
"e.g. 11:29:00
DATA(m12_add_time) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->add( iv_hour = 1 iv_minute = 1 iv_second = 1 )->as( xco_cp_time=>format->iso_8601_extended )->value.
"Subtracting
"e.g. 09:26:58
DATA(m13_subtract_time) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->subtract( iv_hour = 1 iv_minute = 1 iv_second = 1 )->as( xco_cp_time=>format->iso_8601_extended )->value.

"Getting date information
"e.g. 2024-01-01
DATA(m14_date) = xco_cp=>sy->date( )->as( xco_cp_time=>format->iso_8601_extended )->value.
"e.g. 01
DATA(m15_day) = xco_cp=>sy->date( )->day.
"e.g. 01
DATA(m16_month) = xco_cp=>sy->date( )->month.
"e.g. 2024
DATA(m17_year) = xco_cp=>sy->date( )->year.
"Calculations with dates
"Adding
"e.g. 2025-02-02
DATA(m18_add_date) = xco_cp=>sy->date( )->add( iv_day = 1 iv_month = 1 iv_year = 1 )->as( xco_cp_time=>format->iso_8601_extended )->value.
"Subtracting
"e.g. 2022-12-31
DATA(m19_subtract_date) = xco_cp=>sy->date( )->subtract( iv_day = 1 iv_month = 1 iv_year = 1 )->as( xco_cp_time=>format->iso_8601_extended )->value.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Runtime Type Services (RTTS)

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_TYPEDESCR</code> </td>
<td>
RTTS represent a hierarchy of type description classes containing methods for:
<ul>
<li>getting type information on data objects, data types or instances at runtime (Runtime Type Identification (RTTI)).</li>
<li>defining and creating new data types as type description objects at runtime (Runtime Type Creation (RTTC)).</li>
</ul>

Apart from <code>CL_ABAP_TYPEDESCR</code>, there are the following classes available. The list shows the hierarchy of type description classes. The example shows a small selection. See also the dynamic programming cheat sheet.

<pre>
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
</pre>


``` abap
TYPES elem_type TYPE c LENGTH 5.
DATA(tdo_elem) = CAST cl_abap_elemdescr(
    cl_abap_typedescr=>describe_by_name( 'ELEM_TYPE' ) ).
DATA(rel_name) = tdo_elem->get_relative_name( ).

TYPES reftype TYPE REF TO string.
DATA(dref) = NEW i( 123 ).
DATA(type_descr1) = cl_abap_typedescr=>describe_by_data_ref( dref ).
DATA(tdo_ref) = CAST cl_abap_refdescr(
    cl_abap_typedescr=>describe_by_name( 'REFTYPE' ) ).
DATA(type_descr2) = tdo_ref->get_referenced_type( ).

DATA structure TYPE zdemo_abap_carr.
DATA(tdo_struc) = CAST cl_abap_structdescr(
    cl_abap_typedescr=>describe_by_data( structure ) ).
DATA(abs_name) = tdo_struc->absolute_name.
DATA(struc_components) = tdo_struc->get_components( ).

DATA itab TYPE SORTED TABLE OF zdemo_abap_carr WITH UNIQUE KEY carrid.
DATA(tdo_itab) = CAST cl_abap_tabledescr(
    cl_abap_typedescr=>describe_by_data( itab ) ).
DATA(keys) = tdo_itab->get_keys( ).
DATA(tab_components) = CAST cl_abap_structdescr( tdo_itab->get_table_line_type( ) )->components.

DATA(tdo_oref) = CAST cl_abap_classdescr(
    cl_abap_typedescr=>describe_by_name( 'CL_ABAP_CLASSDESCR' ) ).
DATA(cl_methods) = tdo_oref->methods.
DATA(cl_attr) = tdo_oref->attributes.

DATA(tdo_iref) = CAST cl_abap_intfdescr(
    cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_OBJECTS_INTERFACE' ) ).
DATA(intf_methods) = tdo_iref->methods.
DATA(intf_attr) = tdo_iref->attributes.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Assignments

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_CORRESPONDING</code> </td>
<td>

For assignments of components between structures or between internal tables with dynamically specified mapping rules. For more information, you can refer to the <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencl_abap_corresponding.htm">ABAP Keyword Documentation</a>.
The example shows simple assignments with structures.
<br><br>

``` abap
DATA: BEGIN OF s1,
        a TYPE i,
        b TYPE c LENGTH 3,
        c TYPE c LENGTH 5,
        END OF s1.

DATA: BEGIN OF s2,
        a TYPE i,
        b TYPE c LENGTH 3,
        d TYPE string,
        END OF s2.

"Populating structures
s1 = VALUE #( a = 1 b = 'aaa' c = 'bbbbb' ).
s2 = VALUE #( a = 2 b = 'ccc' d = `dddd` ).
DATA(s3) = s1.
DATA(s4) = s2.

"Creating a mapping object
"An initial mapping table means that only identically named components are assigned.
"Other components retain their original values, i.e. the assignment for structures
"works like MOVE-CORRESPONDING or the CORRESPONDING operator with the BASE addition
DATA(map_obj) = cl_abap_corresponding=>create( source  = s1
                                               destination = s2
                                               mapping  = VALUE #( ) ).

"Performing the assignment
map_obj->execute( EXPORTING source      = s1
                  CHANGING  destination = s2 ).

*s2
*A    B      D
*1    aaa    dddd

"Performing the assignment without extra variable; specifying the mapping table:
"level: 0 means the top level
"kind: 1 means that components specified in this line are mapped to each other;
"      see the specification options in cl_abap_corresponding=>mapping_...
"srcname/dstname: source component/target component

cl_abap_corresponding=>create(
    source  = s3
    destination = s4
    mapping  = VALUE #( ( level = 0 kind = 1 srcname = 'c' dstname = 'd' ) )
  )->execute( EXPORTING source      = s3
              CHANGING  destination = s4 ).

*s4
*A    B      D
*1    aaa    bbbbb
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Information about Non-Initial Structure Components

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_STRUCT_UTILITIES</code> </td>
<td>
Provides methods to get information about filled components in structures allowing an efficient processing of non-initial components of a structure.
<br><br>

``` abap
CLASS zcl_some_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_some_class IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    out->write( `---------- filled_components method ----------` ).
    "It returns an internal table containing the names of the non-initial
    "components of the structure and their index.
    DATA: BEGIN OF struct,
            a TYPE c LENGTH 1,
            b TYPE i,
            c TYPE zdemo_abap_carr,
            d TYPE REF TO string,
          END OF struct.

    struct = VALUE #( a = 'X'
                      b = 123
                      c = VALUE #( carrid = 'LH' )
                      d = NEW #( `ABAP` ) ).

    "Getting component information using RTTI
    DATA(all_comps) = CAST cl_abap_structdescr(  cl_abap_typedescr=>describe_by_data( struct ) )->components.
    out->write( all_comps ).

    "Getting an internal table containing the names of non-initial components
    "and their index
    DATA(filled_comps) = cl_abap_struct_utilities=>filled_components( struct ).
    out->write( filled_comps ).

    "In a loop, structure components are cleared. The filled_components method
    "is called in each loop pass, visualizing the filled components in the
    "structure.
    DO lines( all_comps ) TIMES.
      CLEAR struct-(sy-index).
      filled_comps = cl_abap_struct_utilities=>filled_components( struct ).
      out->write( filled_comps ).
    ENDDO.

    out->write( `---------- filled_components_c method ----------` ).
    "Same as above. All structure components must be typed with c LENGTH 1.
    DATA: BEGIN OF struct_c1,
            a TYPE c LENGTH 1 VALUE 'X',
            b TYPE c LENGTH 1,
            c TYPE c LENGTH 1 VALUE 'X',
          END OF struct_c1.

    filled_comps = cl_abap_struct_utilities=>filled_components_c( struct_c1 ).
    out->write( filled_comps ).

    out->write( `---------- filled_components_x method ----------` ).
    "Same as above. All structure components must be typed with x LENGTH 1.
    "It is especially useful for checking filled components of BDEF derived types,
    "for example, %control.
    DATA struc_der_type TYPE STRUCTURE FOR READ IMPORT zdemo_abap_rap_ro_m.
    struc_der_type = VALUE #( %control = VALUE #( key_field = if_abap_behv=>mk-on
                                                  field1    = if_abap_behv=>mk-on
                                                  field2    = if_abap_behv=>mk-off
                                                  field3    = if_abap_behv=>mk-off
                                                  field4    = if_abap_behv=>mk-on ) ).

    filled_comps = cl_abap_struct_utilities=>filled_components_x( struc_der_type-%control ).
    out->write( filled_comps ).
  ENDMETHOD.
ENDCLASS.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Comparing Content of Compatible Internal Tables

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_DIFF</code> </td>
<td>
Using the methods <code>diff</code> and <code>diff_with_line_ref</code> of the <code>CL_ABAP_DIFF</code> class, you can compare the content of two compatible index tables. The returning parameter is an internal table showing how the content of one internal table can be modified to match another one. <code>diff_with_line_ref</code> also returns a reference to the original table lines. Various importing parameters are available to adjust the comparison. Find more information in the class documentation and in the <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencl_abap_diff.htm">ABAP Keyword Documentation</a>.

<br>

```abap
CLASS zcl_demo_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_test IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    TYPES: BEGIN OF s,
             a TYPE i,
             b TYPE string,
             c TYPE c LENGTH 3,
           END OF s.

    DATA it1 TYPE TABLE OF s WITH EMPTY KEY.
    DATA it2 TYPE TABLE OF s WITH EMPTY KEY.

    it1 = VALUE #(
    ( a = 1 b = `aaa` c = 'zzz' )
    ( a = 2 b = `bbb` c = 'yyy' )
    ( a = 3 b = `ccc` c = 'xxx' )
    ( a = 4 b = `ddd` c = 'www' )
    ).

    it2 = VALUE #(
    ( a = 1 b = `aaa` c = 'zzz' )
    ( a = 2 b = `#bb` c = 'yy#' )
    ( a = 3 b = `cc` c = 'x' )
    ( a = 4 b = `ddd` c = 'www' )
    ( a = 5 b = `eee` c = 'vvv' )
    ).

    DATA(it3) = it1.
    DATA is_identical TYPE abap_bool.

    DATA(comparison) = cl_abap_diff=>create(  ).
    TRY.
        DATA(comp_res1) = comparison->diff( EXPORTING target = it2
                                                      source = it1
                                            IMPORTING flag_identical = is_identical ).
        IF is_identical = abap_true.
          out->write( `The two internal tables have identical content.` ).          
        ELSE.
          out->write( comp_res1 ).
        ENDIF.
      CATCH cx_abap_diff INTO DATA(error1).
        out->write( error1->get_text( ) ).
    ENDTRY.

    TRY.
        DATA(comp_res2) = comparison->diff_with_line_ref( EXPORTING target = it2
                                                                    source = it1
                                                          IMPORTING flag_identical = is_identical ).
        IF is_identical = abap_true.
          out->write( `The two internal tables have identical content.` ).          
        ELSE.
          out->write( comp_res2 ).
        ENDIF.
      CATCH cx_abap_diff INTO DATA(error2).
        out->write( error2->get_text( ) ).
    ENDTRY.

    TRY.
        DATA(comp_res3) = comparison->diff_with_line_ref( EXPORTING target = it3
                                                                    source = it1
                                                          IMPORTING flag_identical = is_identical ).
        IF is_identical = abap_true.
          out->write( `The two internal tables have identical content.` ).          
        ELSE.
          out->write( comp_res3 ).
        ENDIF.
      CATCH cx_abap_diff INTO DATA(error3).
        out->write( error3->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Dynamic Programming

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_DYN_PRG</code> </td>
<td>
For validating input for dynamic specifications. 
<br><br>

``` abap
"The following method checks database table names. The name is provided
"with the val parameter. The packages formal parameter expects a table
"containing the names of packages in which the specified table should be
"included. Assuming you provide incorrect input for the table name, or
"the table is not contained in the specified packages, you can expect an
"exception to be raised.
TRY.
    DATA(dbtab) = cl_abap_dyn_prg=>check_table_name_tab(
      val      = `ZDEMO_ABAP_FLI`
      packages = VALUE #( ( `TEST_ABAP_CHEAT_SHEETS` )
                          ( `TEST_SOME_PACK` ) ) ).

    SELECT SINGLE * FROM (dbtab) INTO NEW @DATA(ref_wa).
  CATCH cx_abap_not_a_table cx_abap_not_in_package.
    ...
ENDTRY.

"In the following examples, a method is used to check whether
"the input is allowed or not. For this, you specify an allowlist.
"Here, the relvant parameter expects a comma-separated list of
"allowed values.
TRY.
    DATA(value1) = cl_abap_dyn_prg=>check_allowlist(
        val           = `A`
        allowlist_str = `A,B,C,D` ).

    ... "Here might go an ABAP SQL statement with a dynamic specification.
  CATCH cx_abap_not_in_allowlist.
    ...
ENDTRY.

"Another parameter of the method expects an internal table that
"contains the allowed values.
TRY.
    DATA(value2) = cl_abap_dyn_prg=>check_allowlist(
        val            = `XYZ`
        allowlist_htab = VALUE #( ( `A` )
                                  ( `B` )
                                  ( `C` )
                                  ( `D` ) ) ).

    ... "Here might go an ABAP SQL statement with a dynamic specification.
  CATCH cx_abap_not_in_allowlist.
    ...
ENDTRY.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Context Information

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_CONTEXT_INFO</code> </td>
<td>
Provides context information relevant to the current ABAP session.
<br><br>

``` abap
"Getting current date in UTC (not the system or user time), e.g. 20240101
DATA(sys_date) = cl_abap_context_info=>get_system_date( ).

"Getting current time in UTC, e.g. 152450
DATA(sys_time) = cl_abap_context_info=>get_system_time( ).

"User alias, e.g. XY0000001234
DATA(alias) = cl_abap_context_info=>get_user_alias( ).
"You can also get user information using XCO classes
DATA(user_w_xco) = xco_cp=>sy->user( )->name.

"Formatted name, e.g. John Doe
TRY.
    DATA(formatted_name) = cl_abap_context_info=>get_user_formatted_name( ).
  CATCH cx_abap_context_info_error.
ENDTRY.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## XML/JSON

<table>
<tr>
<td> Class </td> <td> Details </td>
</tr>
<tr>
<td> <code>CL_IXML_*</code> <br> <code>CL_SXML_*</code> </td>
<td>
<ul>
<li>The processing of XML can be done using class libraries such as the Integrated XML Library (iXML) and the Serial XML Library (sXML).</li>
<li>In iXML, you need input and output streams that are created using iXML methods to access XML data.</li>
<li>sXML provides XML readers and writers for different sources and targets to process XML data sequentially. JSON can also be handled.</li>
<li>For more information and code snippets, you can refer to the ABAP Keyword Documentation and the XML/JSON cheat sheet.</li>
</ul>
<br>

```abap
"As mentioned, refer to the ABAP Keyword Documentation and the 
"XML/JSON cheat sheet (and executable example) for examples

"Creating an XML writer using sXML
DATA(writer) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type     = if_sxml=>co_xt_xml10
                                                                   encoding = 'UTF-8' ) ).

"The above writer uses a cast to if_sxml_writer. In doing so,
"more special methods can be accessed. You can check it by adding
"the object component selector (->) behind the closing parenthesis.
"Plus: The example above specifies the default parameters
"explicitly. Here, they are omitted. You can also create a JSON
"writer.
DATA(writer2) = cl_sxml_string_writer=>create(  ).

... "Creating some XML data

"Creating an XML reader using sXML
"Note: Similar to the writer, the interface IF_SXML_READER exists
"for readers. 
DATA(reader) = cl_sxml_string_reader=>create( some_xml ).

... "Reading XML data sequentially
```

</td>
</tr>
<tr>
<td> <code>XCO_CP_JSON</code> </td>
<td>
Handling JSON data using the XCO library
<br><br>

``` abap
"Creating and populating a demo structure and internal table
DATA: BEGIN OF carrier_struc,
        carrier_id    TYPE c length 3,
        connection_id TYPE n length 4,
        city_from TYPE c length 20,
        city_to TYPE c length 20,
      END OF carrier_struc.

DATA carriers_tab LIKE TABLE OF carrier_struc WITH EMPTY KEY.

carrier_struc = VALUE #( carrier_id = 'AA' connection_id = '17' city_from = 'New York' city_to = 'San Francisco' ).
carriers_tab = VALUE #( ( carrier_id = 'AZ' connection_id = '788' city_from = 'Rome' city_to = 'Tokyo' )
                        ( carrier_id = 'JL' connection_id = '408' city_from = 'Frankfurt' city_to = 'Tokyo' ) ).

"ABAP (structure) -> JSON using XCO
DATA(struc2json_xco) = xco_cp_json=>data->from_abap( carrier_struc )->to_string( ).
"Result: {"CARRIER_ID":"AA","CONNECTION_ID":"0017","CITY_FROM":"New York","CITY_TO":"San Francisco"}

"ABAP (internal table) -> JSON using XCO
DATA(itab2json_xco) = xco_cp_json=>data->from_abap( carriers_tab )->to_string( ).
"Result: [{"CARRIER_ID":"AZ","CONNECTION_ID":"0788","CITY_FROM":"Rome","CITY_TO":"Tokyo"},
"         {"CARRIER_ID":"JL","CONNECTION_ID":"0408","CITY_FROM":"Frankfurt","CITY_TO":"Tokyo"}]

"JSON -> ABAP (structure) using XCO
DATA json2struc_xco LIKE carrier_struc.
xco_cp_json=>data->from_string( struc2json_xco )->write_to( REF #( json2struc_xco ) ).
"Result:
"CARRIER_ID    CONNECTION_ID    CITY_FROM    CITY_TO
"AA            0017             New York     San Francisco

"JSON -> ABAP (internal table) using XCO
DATA json2itab_xco LIKE carriers_tab.
xco_cp_json=>data->from_string( itab2json_xco )->write_to( REF #( json2itab_xco ) ).
"Result:
"CARRIER_ID    CONNECTION_ID    CITY_FROM    CITY_TO
"AZ            0788             Rome         Tokyo
"JL            0408             Frankfurt    Tokyo

"Creating JSON using XCO
"You can check out more methods that offer various options to build
"the JSON by clicking CTRL + Space after '->' in ADT.
"In the following example, JSON data similar to above is created.
"First, a JSON data builder is created. Then, using different methods,
"JSON data is created.
DATA(json_builder_xco) = xco_cp_json=>data->builder( ).
json_builder_xco->begin_object(
  )->add_member( 'CarrierId' )->add_string( 'DL'
  )->add_member( 'ConnectionId' )->add_string( '1984'
  )->add_member( 'CityFrom' )->add_string( 'San Francisco'
  )->add_member( 'CityTo' )->add_string( 'New York'
  )->end_object( ).

"Getting JSON data
DATA(json_created_xco) = json_builder_xco->get_data( )->to_string( ).
"Result: {"CarrierId":"DL","ConnectionId":"1984","CityFrom":"San Francisco","CityTo":"New York"}

"Transforming the created JSON to ABAP (structure)
"Note: The JSON was intentionally created without the underscores in the
"name to demonstrate the 'apply' method. The following example demonstrates
"a transformation of camel case and underscore notation. As above, check out
"more options by clicking CTRL + Space after '...transformation->'.
CLEAR json2struc_xco.
xco_cp_json=>data->from_string( json_created_xco )->apply( VALUE #(
  ( xco_cp_json=>transformation->pascal_case_to_underscore ) ) )->write_to( REF #( json2struc_xco ) ).
"Result
"CARRIER_ID    CONNECTION_ID    CITY_FROM        CITY_TO
"DL            1984             San Francisco    New York
``` 

</td>
</tr>
<tr>
<td> <code>/UI2/CL_JSON</code> </td>
<td>

- Handling JSON data using the <code>/UI2/CL_JSON</code> class
- Note that there are many additional and optional parameters available, some of which are explored in examples in the [Working with XML and JSON in ABAP](21_XML_JSON.md) cheat sheet.


<br>

```abap
DATA(some_table) = VALUE string_table( ( `aaa` ) ( `bbb` ) ( `ddd` ) ).

"--------- ABAP -> JSON ---------
DATA(abap_to_json) = /ui2/cl_json=>serialize( data = some_table ).

"--------- JSON -> ABAP ---------
DATA table_json_to_abap TYPE string_table.
/ui2/cl_json=>deserialize( EXPORTING json = abap_to_json
                           CHANGING data  = table_json_to_abap ).
```

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## ABAP Repository Object Information 

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>XCO_CP_ABAP</code><br><code>XCO_CP_ABAP_REPOSITORY</code> </td>
<td>

<ul>
<li>Provides access to abstractions for ABAP Dictionary objects such as database tables, data elements, types and more.</li>
<li>For more detailed examples, refer to the <a href="https://help.sap.com/docs/btp/sap-business-technology-platform/overview-of-xco-modules">SAP Help Portal</a>. See also the executable example of the ABAP for Cloud Development cheat sheet.</li>
<li>The code snippet contains a small selection and only hints at the many ways to retrieve different pieces of information that the classes provide.</li>
<li>The snippet does not only include the <code>XCO_CP_ABAP_REPOSITORY</code> class but also others such as <code>XCO_CP_ABAP</code>, <code>XCO_CP_ABAP_SQL</code>, and <code>XCO_CP_CDS</code>. More XCO classes are available in that context.</li>
</ul>
<br>

``` abap
"Getting all accessible repository objects in the system
"To further process the returned values, you can loop over them.
DATA(all_obj) = xco_cp_abap_repository=>objects->all->in( xco_cp_abap=>repository )->get( ).

"Gettig all database tables
DATA(all_tables) = xco_cp_abap_repository=>objects->tabl->all->in( xco_cp_abap=>repository )->get( ).

"Refining the search by applying a filter
"Creating a filter and adding a search pattern
  DATA(filter1) = xco_cp_abap_repository=>object_name->get_filter(
    xco_cp_abap_sql=>constraint->contains_pattern( 'ZDEMO_ABAP_RAP_R%' ) ).

"Getting all BDEFs in the system having a specific pattern
DATA(bdefs_in_package) = xco_cp_abap_repository=>objects->bdef->where( VALUE #( ( filter1 )
    ) )->in( xco_cp_abap=>repository )->get( ).

"Getting all accessible interfaces with a particular name pattern in the entire system
DATA(filter2) = xco_cp_abap_repository=>object_name->get_filter(
  xco_cp_abap_sql=>constraint->contains_pattern( 'IF_ABAP_BEHV%' ) ).
DATA(all_intfs) = xco_cp_abap_repository=>objects->intf->where( VALUE #( ( filter2 )
  ) )->in( xco_cp_abap=>repository )->get( ).

"Getting all classes in the system that correspond to a specific name pattern and
"that are contained in a specific software component
"Creating filters
DATA(filter3) = xco_cp_system=>software_component->get_filter( xco_cp_abap_sql=>constraint->equal( 'SAP_BASIS' ) ).
DATA(filter4) = xco_cp_abap_repository=>object_name->get_filter( xco_cp_abap_sql=>constraint->contains_pattern( '%CL_ABAP_RAND%' ) ).
DATA(filtered_classes) = xco_cp_abap_repository=>objects->clas->where( VALUE #( ( filter3 ) ( filter4 )
  ) )->in( xco_cp_abap=>repository )->get( ).

"Getting information about some of the technical properties of different
"repository objects. The examples show the creation of handlers. You can
"explore more options, e.g., by adding the object component selector (->)
"to the final parenthesis and checking the suggestions by ADT.
"Database table
DATA(handler_tabl) = xco_cp_abap_dictionary=>database_table( 'ZDEMO_ABAP_CARR' ).
"Data element
DATA(handler_dtel) = xco_cp_abap_dictionary=>data_element( 'MANDT' ).
"Table type
DATA(handler_table_type) = xco_cp_abap_dictionary=>table_type( 'STRING_TABLE' ).
"CDS view entity
DATA(handler_cds_ve) = xco_cp_cds=>view_entity( 'ZDEMO_ABAP_RAP_RO_M' ).
"Interface
DATA(handler_intf) = xco_cp_abap=>interface( 'ZDEMO_ABAP_OBJECTS_INTERFACE' ).
"Getting information about where the interface is implemented
DATA(where_is_intf_impl) = handler_intf->implementations->all->get_names( ).
"Class
DATA(handler_cl) = xco_cp_abap=>class( 'ZCL_DEMO_ABAP_UNIT_TEST' ).
"Getting subclasses
DATA(subcl) = xco_cp_abap=>class( 'CL_ABAP_TYPEDESCR' )->subclasses->all->get( ).
"Getting the names of the subclasses
DATA(subcl_names) = xco_cp_abap=>class( 'CL_ABAP_TYPEDESCR' )->subclasses->all->get_names( ).
"Getting the direct superclass
DATA(direct_super_class) = xco_cp_abap=>class( 'CL_ABAP_DATADESCR' )->definition->content( 
  )->get_superclass( )->name.

"Taking an XCO handler for a database table as an example, see some of the
"details you can retrieve. The method names should be self-explanatory.
DATA(dbtab_name) = handler_tabl->name.
DATA(dbtab_descr) = handler_tabl->content( )->get_short_description( ).
DATA(dbtab_del_cl) = handler_tabl->content( )->get_delivery_class( )->value.
DATA(dbtab_field_names) = handler_tabl->fields->all->get_names( ).
DATA(dbtab_keys) = handler_tabl->fields->key->get_names( ).
DATA(dbtab_vis) = handler_tabl->get_api_state( )->get_visibilities( ). "Initial for the cheat sheet table
DATA(dbtab_rel_state) = handler_tabl->get_api_state( )->get_release_state( )->value.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Generating ABAP Repository Objects

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>XCO_CP_GENERATION</code> </td>
<td>
For creating, updating and deleting ABAP repository objects. More information: <a href="https://help.sap.com/docs/btp/sap-business-technology-platform/generation-apis">Generation APIs</a><br>The rudimentary snippet is taken from the executable example of the ABAP for Cloud Development cheat sheet.

<br><br>

``` abap
...
DATA(n10_handler) = xco_cp_generation=>environment->dev_system( ... ).
DATA(n11_put) = n10_handler->create_put_operation( ).

"Creating a domain
DATA(n12_doma_spec) = n11_put->for-doma->add_object( ... "e.g. 'ZDEMO_ABAP_STATUS'
  )->set_package( ...
  )->create_form_specification( ).
n12_doma_spec->set_short_description( 'Demo domain' ).
n12_doma_spec->set_format( xco_cp_abap_dictionary=>built_in_type->char( 10 ) ).
n12_doma_spec->fixed_values->add_fixed_value( 'BOOKED'
  )->set_description( 'Booked' ).
n12_doma_spec->fixed_values->add_fixed_value( 'CANCELED'
  )->set_description( 'Canceled' ).

...

"Executing the generation
TRY.
    n11_put->execute( ).      
  CATCH cx_xco_gen_put_exception.
ENDTRY.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Call Stack

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>XCO_CP</code><br><code>XCO_CP_CALL_STACK</code> </td>
<td>
Getting the current ABAP call stack programmatically. See more information <a href="https://help.sap.com/docs/btp/sap-business-technology-platform/call-stack">here</a>.
<br><br>


``` abap
"Getting the full current call stack
DATA(call_stack) = xco_cp=>current->call_stack->full( ).

"Creating a format for the call stack
"In the example, an ADT debugger-like style is used
DATA(format) = xco_cp_call_stack=>format->adt(
  )->with_line_number_flavor( xco_cp_call_stack=>line_number_flavor->source ).

"Retrieving the full call stack as text, e.g. so that it can be output
"or written somewhere
DATA(call_stack_as_text) = call_stack->as_text( format ).

"Extracting the call stack based on specifications
"You can specify the extractions using from/to and
"further detailing out the kinds of extractions
"such as the position or the first/last occurrence
"of a specific line pattern.
"In the example, a line pattern is created (method that
"starts with a specific pattern). The extracting should
"go up to the last occurrence of this pattern. It is
"started at position 1.
DATA(line_pattern) = xco_cp_call_stack=>line_pattern->method(
  )->where_class_name_starts_with( 'CL_REST' ).
DATA(extracted_call_stack_as_text) = call_stack->from->position( 1
  )->to->last_occurrence_of( line_pattern )->as_text( format ).
``` 

</td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Sending Emails

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_BCS_MAIL_MESSAGE</code> </td>
<td>

<ul>
<li>For sending emails, a configuration is required. Find more information <a href="https://help.sap.com/docs/btp/sap-business-technology-platform/emailing">here</a>.</li>
<li>Note that you can also send emails asynchronously using the method <code>send_async</code>.</li>
</ul>
<br>

``` abap
TRY.
    DATA(mail) = cl_bcs_mail_message=>create_instance( ).
    mail->set_sender( '...@...' ).
    mail->add_recipient( '...@...' ).
    mail->set_subject( 'Test Mail' ).
    mail->set_main( cl_bcs_mail_textpart=>create_instance(
      iv_content      = '<h1>Hello</h1><p>This is a test mail.</p>'
      iv_content_type = 'text/html' ) ).
    mail->send( IMPORTING et_status = DATA(status_table) ).
    "You can check the status of the email sending in the returned table. 
    "'status' components: E (error), S (sent), W (waiting)
  CATCH cx_bcs_mail INTO DATA(error_mail).
    ...
ENDTRY.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Tenant Information

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>XCO_CP</code><br><code>XCO_CP_TENANT</code> </td>
<td>
For obtaining various information about the currently active tenant.
<br><br>

``` abap
"Getting a handler for the currently active tenant
DATA(ten) = xco_cp=>current->tenant( ).

"Getting the UI URL of the currently active tenant
DATA(ui_url) = ten->get_url( xco_cp_tenant=>url_type->ui ).

"Protocol
"e.g. https
DATA(prot) = ui_url->get_protocol( ).
"Host and domain
"e.g. abcde-...-com
DATA(host) = ui_url->get_host( ).
"Port
"e.g. 443
DATA(port) = ui_url->get_port( ).

"Global account ID
DATA(global_acc_id) = ten->get_global_account_id( )->as_string( ).
"Guid
DATA(guid) = ten->get_guid( )->value.
"Id
DATA(id) = ten->get_id( ).
"Subaccount ID
DATA(sub_acc_id) = ten->get_subaccount_id( )->as_string( ).
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Exception Classes

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CX_*</code> </td>
<td>
Exception classes are special classes, usually starting with the name <code>CX_*</code>, that serve as the basis for <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencatchable_exception_glosry.htm">catchable exceptions</a>. When an exception is raised, an object of such an exception class is created. There are several predefined exception classes. Find more information in the cheat sheet about program flow logic. 
<br><br>

``` abap
TRY.
    DATA(res) = 1 / 0.
  CATCH cx_sy_zerodivide.
ENDTRY.

DATA(str_table) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).
TRY.
    DATA(table_entry) = str_table[ 4 ].
  CATCH cx_sy_itab_line_not_found.
ENDTRY.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Parallel Processing

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_PARALLEL</code> </td>
<td>
For performing the parallel processing for instances of ABAP Objects. The following example class is intended to be a self-contained example that tries to visualize the functionality of the <code>CL_ABAP_PARALLEL</code> class. For more information, refer to the class documentation. 
<br><br>

``` abap
CLASS zcl_some_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_abap_parallel.
    METHODS: constructor.
  PRIVATE SECTION.
    "This table holds entries that are added when
    "calling the 'do' method.
    DATA info_parallel_proc TYPE string_table.

    "Data types and objects for the data cluster
    "that contains information to be output
    CLASS-DATA buffer TYPE xstring.
    DATA obj TYPE string.
    DATA parallel_log TYPE string_table.
    DATA inst_name TYPE string.
    "Structure to hold information for the output
    DATA: BEGIN OF info,
            name   TYPE string,
            tmstmp TYPE utclong,
          END OF info.
    "Preparing the data cluster
    METHODS handle_cluster IMPORTING name TYPE string.
ENDCLASS.

CLASS zcl_some_class IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    "This is a self-contained example class trying to visualize the
    "functionality of the cl_abap_parallel class.
    "As a prerequisite, you have a class that implements the interface
    "IF_ABAP_PARALLEL. In the self-contained example, it is this class
    "itself that implements it.
    "When running the class with F9, the following code is executed.

    "Creating an instance of class cl_abap_parallel
    DATA(parallel) = NEW cl_abap_parallel( ).

    "Creating instances of this example class for demonstration
    "purposes. The handle_cluster method is used to manually
    "provide the name of the object reference variable for
    "output purposes.
    handle_cluster( `inst1` ).
    DATA(inst1) = NEW zcl_some_class( ).

    handle_cluster( `inst2` ).
    DATA(inst2) = NEW zcl_some_class( ).

    handle_cluster( `inst3` ).
    DATA(inst3) = NEW zcl_some_class( ).

    handle_cluster( `inst4` ).
    DATA(inst4) = NEW zcl_some_class( ).

    WAIT UP TO 1 SECONDS.

    "Starting the parallel processing for objects with the 'run_inst' method.
    "As input parameters, instances of the classes to be processed are expected.
    "The importing parameter is an internal table. It contains result information
    "for the tasks that were processed in parallel.
    parallel->run_inst( EXPORTING p_in_tab  = VALUE #( ( inst1 )
                                                       ( inst2 )
                                                       ( inst3 )
                                                       ( inst4 ) )
                        IMPORTING p_out_tab = DATA(result_info) ).

    WAIT UP TO 1 SECONDS.

    LOOP AT result_info INTO DATA(wa).
      "For accessing the details, a cast to the respective class is required (in this
      "case, it is this very class).
      DATA(res) = CAST zcl_some_class( wa-inst ).

      "Adding information to a string table that is output
      APPEND |**** Instance "{ res->info-name }" created at { res->info-tmstmp } ****| TO parallel_log.
      LOOP AT res->info_parallel_proc INTO DATA(wap).
        APPEND |{ wap }| TO parallel_log.
      ENDLOOP.
      APPEND `-----------------------------------------------------------------` TO parallel_log.
    ENDLOOP.

    out->write( parallel_log ).
  ENDMETHOD.

  METHOD if_abap_parallel~do.
    "The following code is executed in parallel.
    "In the simplified example, some text is added to a string table that
    "is output later on. It includes a time stamp to compare the different
    "outputs.
    DO 3 TIMES.
      APPEND |This text was added at { utclong_current( ) }| TO info_parallel_proc.
    ENDDO.
  ENDMETHOD.

  METHOD constructor.
    "The purpose of the implementation in the instance constructor
    "is to pass the manually added name of the object reference
    "variable for output purposes.
    "A data cluster is used to have a self-contained example class
    "that can be run with F9.
    TRY.
        IMPORT str = inst_name FROM DATA BUFFER buffer.
      CATCH cx_sy_import_format_error INTO DATA(error).
    ENDTRY.

    IF error IS INITIAL.
      "The information provided here is used for output purposes.
      "The name of the object reference variable is
      "expected to be the only input in the table, as well as
      "a time stamp to compare with other time stamps that are
      "output.
      info = VALUE #( name   = inst_name
                      tmstmp = utclong_current( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_cluster.
    "This method is used to prepare the data cluster and
    "pass the name of the object reference variable that
    "was added manually via the input parameter 'name'.
    CLEAR buffer.
    obj = name.
    EXPORT str = obj TO DATA BUFFER buffer.
  ENDMETHOD.
ENDCLASS.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Application Log

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_BALI_LOG</code> </td>
<td>
For creating and reading application logs. Refer to <a href="https://help.sap.com/docs/btp/sap-business-technology-platform/application-logs">this documentation</a> for more information and code snippets. Note that there is also an XCO module available dealing with business application log (<code>XCO_CP_BAL</code>; see <a href="https://help.sap.com/docs/btp/sap-business-technology-platform/business-application-log">this documentation</a>).
<br><br>

``` abap
"Demo application job (sub)objects and external ID
"Note:
"- Refer to the documentation about how to create an application log object in ADT,
"  and for more options the classes offer in the context of application logs.
"- Required authorization for authorization object: S_APPL_LOG.
CONSTANTS: obj         TYPE cl_bali_header_setter=>ty_object VALUE 'DEMO_LOG',
           subobj      TYPE cl_bali_header_setter=>ty_subobject  VALUE 'DEMO_SUB',
           external_id TYPE cl_bali_header_setter=>ty_external_id VALUE 'DEMO_EXT_ID'.

"---- Creating (and deleting) application log entries ----
TRY.
    "Creating an instance of the application Log class
    DATA(appl_log) = cl_bali_log=>create( ).

    "Setting the header log, using application log object descriptions
    appl_log->set_header( header = cl_bali_header_setter=>create( object      = obj
                                                                  subobject   = subobj
                                                                  external_id = external_id ) ).

    "Before creating new application log entries, deleting existing ones based on the application
    "log object information and the user
    DATA(filter4del) = cl_bali_log_filter=>create( ).
    filter4del->set_create_info( user = xco_cp=>sy->user( )->name
                                )->set_descriptor( object      = obj
                                                   subobject   = subobj
                                                   external_id = external_id ).

    "Reading the logs from the database by applying the filter
    DATA(del_appl_log) = cl_bali_log_db=>get_instance( )->load_logs_w_items_via_filter( filter = filter4del ).
    "Deleting existing application log entries that are retrieved based on the applied filter
    LOOP AT del_appl_log INTO DATA(log_del).
      cl_bali_log_db=>get_instance( )->delete_log( log = log_del ).
    ENDLOOP.

  CATCH cx_bali_runtime.
ENDTRY.

TRY.
    "Creating new application log entries
    "Creating a free text and adding it to the application log
    DATA(free_txt) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_category_free_text
                                                        text = |This text is added to the application log at { utclong_current( ) }| ).
    appl_log->add_item( item = free_txt ).

    "Creating an exception and adding it to the application log
    DATA(strtab) = VALUE string_table( ( `a` ) ( `b` ) ( `c` ) ).
    TRY.
        DATA(line) = strtab[ 4 ].
      CATCH cx_sy_itab_line_not_found INTO DATA(error).
    ENDTRY.
    DATA(exc) = cl_bali_exception_setter=>create( severity  = if_bali_constants=>c_category_exception
                                                  exception = error ).
    appl_log->add_item( item = exc ).

    "Saving the application log to the database
    cl_bali_log_db=>get_instance( )->save_log( log = appl_log ).
  CATCH cx_bali_runtime.
ENDTRY.

WAIT UP TO 1 SECONDS.

"---- Reading the application log ----
TRY.
    "Creating a filter
    "Only the messages from the current user based on the application log
    "object information and in the time frame now - 1 hour are retrieved.
    DATA(filter) = cl_bali_log_filter=>create( ).
    DATA(now) = utclong_current( ).
    DATA(anhourearlier) =  utclong_add( val = now
                                        hours = '1-' ).
    filter->set_create_info( user = xco_cp=>sy->user( )->name
                            )->set_descriptor( object      = obj
                                                subobject   = subobj
                                                external_id = external_id
                            )->set_time_interval( start_time = anhourearlier
                                                  end_time   = now ).

    "Reading the logs from the database by applying the filter
    DATA(log_table) = cl_bali_log_db=>get_instance( )->load_logs_w_items_via_filter( filter = filter ).

    "Processing the read result
    LOOP AT log_table INTO DATA(log).
      "Retrieving all items
      DATA(items) = log->get_all_items( ).
      "Retrieving the application log messages (and displaying)
      LOOP AT items INTO DATA(item).
        DATA(log_item_number) = item-log_item_number.
        DATA(msg) = item-item->get_message_text( ).
        "Displaying the entries when running a class
        "that implements the if_oo_adt_classrun interface
        "out->write( |{ log_item_number } { msg }| ).
      ENDLOOP.
    ENDLOOP.
  CATCH cx_bali_runtime.
ENDTRY.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Running Code in the Background

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_BGMC_PROCESS_FACTORY</code> </td>
<td>
<ul>
<li>Used in the context of the Background Processing Framework (bgPF) to run code asynchronously in the background.</li>
<li>Different flavors are available:</li> 
<ul>
<li>Using bgPF without transactional control, for example, if you do not work with a RAP application or transactional control is not relevant in an ABAP program. In this case, you can implement the <code>IF_BGMC_OP_SINGLE_TX_UNCONTR</code> interface.</li> 
<li>Using bgPF with transactional control, for example, if you work with a RAP application. In that case, you can implement the <code>IF_BGMC_OP_SINGLE</code> interface. Note: If you are in a RAP context, you do not need to implement <code>COMMIT/ROLLBACK WORK</code> because the RAP framework takes care of it.</li>
</ul>
<li>More information:</li> 
<ul>
<li><a href="https://help.sap.com/docs/abap-cloud/abap-concepts/background-processing-framework ">Background Processing Framework</a></li>
<li>Transactional control with the <a href="https://help.sap.com/docs/abap-cloud/abap-concepts/controlled-sap-luw">controlled SAP LUW</a></li>
</ul>
<li>The following, self-contained, and oversimplified example is intended to give a rough idea about the functionality. It does not include transactional control. The example class can be run using F9 in ADT. It does the following: A demo database table of the cheat sheet repository is filled synchronously and asynchronously (using bgPF) with entries, just to show an effect and get an idea. Two entries are created in the background. <code>WAIT</code> statements are included to have a self-contained example, and that all created database entries can be shown in the output. In the example, the background processing may be visualized, for example, by the <code>MODIFY</code> statement that is followed by a <code>WAIT</code> statement in the loop. The output can show that the entry for the first asynchronously created entry was added before a synchronously created entry. For more visualization options regarding the execution in the background, you can, for example, check the ABAP Cross Trace. For more information, refer to the documentation.</li>
</ul>
<br>

``` abap
CLASS zcl_some_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_bgmc_op_single_tx_uncontr.
    CLASS-METHODS get_uuid RETURNING VALUE(uuid) TYPE sysuuid_x16.
  PRIVATE SECTION.
    CLASS-DATA number TYPE i.
ENDCLASS.

CLASS zcl_some_class IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    "Deleting a demo database table
    DELETE FROM zdemo_abap_tabca.

    number += 1.
    MODIFY zdemo_abap_tabca FROM @( VALUE #(
      id = get_uuid( )
      calc_result = |Synchronous entry creation in the MAIN method { number }|
      crea_date_time = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ) ) ).

    "Processing code in the background
    DO 2 TIMES.
      "Creating an instance of the example class (that implements the bgPF-relevant
      "interface if_bgmc_op_single_tx_uncontr)
      DATA(inst) = NEW zcl_some_class(  ).

      TRY.
          "Getting the default factory for transactional background processes and
          "creating a process for a single operation
          DATA(backgr) = cl_bgmc_process_factory=>get_default( )->create( ).
          "Setting a name of the process
          backgr->set_name( `bgPF Test` ).
          "Setting the transactionally uncontrolled operation of the process
          backgr->set_operation_tx_uncontrolled( inst ).
          "Saving the background process for the asynchronous execution
          backgr->save_for_execution(  ).
          "An explicit COMMIT WORK is required to start the background process.
          "This explicit call is not needed in the context of RAP because the RAP
          "framework will take care of the commit call.
          COMMIT WORK.
        CATCH cx_bgmc INTO DATA(error).
          out->write( error->get_text( ) ).
          ROLLBACK WORK.
      ENDTRY.

      number += 1.
      MODIFY zdemo_abap_tabca FROM @( VALUE #(
        id = get_uuid( )
        calc_result = |Synchronous entry creation in the MAIN method { number }|
        crea_date_time = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ) ) ).
      WAIT UP TO 1 SECONDS.
    ENDDO.

    WAIT UP TO 2 SECONDS.

    "Displaying the content of a demo database table that was filled
    "in the course of the class execution
    SELECT id, calc_result, crea_date_time
      FROM zdemo_abap_tabca
      ORDER BY crea_date_time
      INTO TABLE @DATA(itab).

    out->write( itab ).
  ENDMETHOD.

  METHOD if_bgmc_op_single_tx_uncontr~execute.
    MODIFY zdemo_abap_tabca FROM @( VALUE #(
      id = get_uuid( )
      calc_result = `Asynchronous entry creation in background in the EXECUTE method`
      crea_date_time = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ) ) ).
  ENDMETHOD.

  METHOD get_uuid.
    TRY.
        uuid = cl_system_uuid=>create_uuid_x16_static( ) .
      CATCH cx_uuid_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Locking

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_LOCK_OBJECT_FACTORY</code> </td>
<td>
For activating lock objects. Note that you can also use the <code>DEQUEUE_ALL</code> method to remove all locks in the current SAP LUW.
The following example code snippet uses artifacts from the ABAP cheat sheet repository.
<br><br>

``` abap
"Deleting demo database table
DELETE FROM zdemo_abap_rapt1.
"Creating a database table entry and reading it into a data reference variable
MODIFY zdemo_abap_rapt1 FROM @( VALUE #( key_field = 1 field1 = 'aaa' field2 = 'bbb' field3 = 2 field4 = 3 ) ).
SELECT SINGLE * FROM zdemo_abap_rapt1 WHERE key_field = 1 INTO NEW @DATA(key).

TRY.
    "Instantiating a lock object    
    DATA(lock) = cl_abap_lock_object_factory=>get_instance( iv_name = 'EZDEMO_ABAP_LOCK' ).

    "Enqueuing a lock object
    "Note that there are various parameters. The parameter used in the example contains
    "a list of lock fields including a reference to the parameter value.
    lock->enqueue( it_parameter = VALUE #( ( name = 'KEY_FIELD' value = REF #( key->key_field ) ) ) ).
  CATCH cx_abap_lock_failure cx_abap_foreign_lock INTO DATA(enq_error).
ENDTRY.

TRY.
    "Dequeuing a lock object
    lock->dequeue( it_parameter = VALUE #( ( name = 'KEY_FIELD' value = REF #( key->key_field ) ) ) ).
  CATCH cx_abap_lock_failure INTO DATA(deq_error).  
ENDTRY.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Calling Services

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_WEB_HTTP_CLIENT_MANAGER</code><br><code>CL_HTTP_DESTINATION_PROVIDER</code> </td>
<td>
<ul>
<li>For creating a client object using an HTTP destination. The HTTP destination is provided based on an HTTP destination object.
The latter can be created, among others, based on a communication arrangement or a plain URL. </li>
<li>For more information, refer to the class documentation and the topic <a href="https://help.sap.com/docs/btp/sap-business-technology-platform/integration-and-connectivity">Integration and Connectivity</a>.</li>
<li>To check out the classes in an example, expand the collapsible section below.</li>
</ul>

<details>
  <summary>Expand to view an example</summary>
  <!-- -->

<ul>
<li>⚠️ Notes on the example</li>
<ul>
<li>The following self-contained and oversimplified example is not a representative best practice example, nor does it cover a meaningful use case. 
It only explores method calls and is intended to give a rough idea of the functionality.</li>
<li>The example uses the <code>create_by_url</code> method, which is only suitable for public services or testing purposes. No authentication is required for the APIs used.</li>
<li>Note the <a href="README.md#%EF%B8%8F-disclaimer">Disclaimer</a>.</li>
<li>For more information, more meaningful examples, and tutorials that deal with the classes and methods, see the following links:</li>
<ul>
<li><a href="https://developers.sap.com/tutorials/abap-environment-external-api.html">Call an External API and Parse the Response in SAP BTP ABAP Environment</a></li>
<li><a href="https://community.sap.com/t5/technology-blogs-by-sap/how-to-call-a-remote-odata-service-from-the-trial-version-of-sap-cloud/ba-p/13411535">How to call a remote OData service from the trial version of SAP Cloud Platform ABAP environment</a></li>
</ul>
<li>The example is generally about calling external APIs and parsing the HTTP responses. It retrieves most of the code snippets contained in the ABAP cheat sheet markdown files of the ABAP cheat sheet GitHub repository. However, the focus is on the service calls, not on the actual output. More details:</li>
<ul>
<li>At various points in the code, the methods of the classes are used to create client objects. In all cases, an HTTP destination is created using a plain URL.</li>
<li>In all cases, public GitHub URLs/APIs are used that do not require an authentication (note the GitHub documentation links further down).</li>
<li>First, after creating a client object using a destination object that is based on a URL, an HTTP GET request is sent. The response is a string containing all file names that are contained in the ABAP cheat sheet GitHub repository. The file names are needed to construct URLs so as to retrieve the markdown content of the files. The file names are extracted and stored in an internal table.</li>
<li>Second, the internal table containing the file names is looped over. A URL is constructed for each markdown file. New client objects are created using destination objects that are based on the constructed URLs. Markdown content is retrieved by sending an HTTP GET request. To better process the retrieved content, it is stored in a string table. In the loop, all content from the markdown that is not part of a code snippet (indicated by the triple backticks) is deleted.</li>
<li>Third, a public API provided by GitHub is used to render markdown text to HTML. This is done by creating another client object. HTTP POST requests are sent, and the responses are retrieved. The responses should contain the code snippets converted to HTML in a string. The code snippets are added to HTML expandable sections.</li>
<li>Finally, the expandable sections containing the code snippets per cheat sheet are added to a simple HTML page. The code of the assembled HTML page is displayed in the ADT console.</li>
<li>For example and for demonstration purposes, if the HTML code is displayed in the ADT console, you can create a file named <em>ABAP_cheat_sheet_code.html</em> on your local machine. Open the file in an editor, copy and paste the entire ADT console content (it is recommended that you clear the ADT console before running the class to avoid copying and pasting unwanted output), and save the local file. Open the saved file in a web browser. You will now have several code snippets from the cheat sheets available offline. 
In fact, the output (plain html with a lot of code) of this example may not be a meaningful reference artifact. 
Nevertheless, the example may give you an idea of how to use the ABAP classes, GET and POST requests, and so on (and you may also be interested in the various options for string processing as used in the example and described in the respective cheat sheet). Follow the links for more information. </li>
<li>Before using the GitHub APIs, make sure that you have consulted the following documentation: <a href="https://docs.github.com/en">GitHub Docs</a>, <a href="https://docs.github.com/en/enterprise-cloud@latest/rest/markdown/markdown?apiVersion=2022-11-28#render-a-markdown-document">Render a Markdown document</a>, <a href="https://docs.github.com/en/rest/using-the-rest-api/rate-limits-for-the-rest-api?apiVersion=2022-11-28">Rate limits for the REST API</a> </li>
</ul>
</ul>
</ul>

<br>

``` abap
CLASS zcl_some_class DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PRIVATE SECTION.
    CONSTANTS url_cs TYPE string VALUE `https://api.github.com/repos/SAP-samples/abap-cheat-sheets/git/trees/main`.
    CONSTANTS url_gh TYPE string VALUE `https://raw.githubusercontent.com/SAP-samples/abap-cheat-sheets/main/`.
    CONSTANTS url_api TYPE string VALUE `https://api.github.com/markdown`.
    DATA url TYPE string.
    TYPES: BEGIN OF s,
             file_name     TYPE string,
             title         TYPE string,
             code_snippets TYPE string_table,
             error         TYPE abap_boolean,
           END OF s.
    DATA tab TYPE TABLE OF s WITH EMPTY KEY.
    DATA snippets TYPE string_table.
    DATA html TYPE string.
ENDCLASS.
CLASS zcl_some_class IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    TRY.
        "Creating a client object using a destination
        "In the example, the HTTP destination is created using a plain URL.
        "Here, a GitHub API is used to retrieve file names of the ABAP cheat sheet repository.
        DATA(http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = cl_http_destination_provider=>create_by_url( i_url = url_cs ) ).
        "Sending an HTTP GET request and returning the response
        "In the example, the HTTP body is retrieved as string data.
        DATA(response) = http_client->execute( if_web_http_client=>get )->get_text(  ).
      CATCH cx_root INTO DATA(err).
        out->write( err->get_text( ) ).
    ENDTRY.
    IF err IS INITIAL.
      "Markdown file names are contained in the returned string in a specific
      "pattern. In the following code, the markdown file names are extracted
      "using a regular expression (pattern: "path":"04_ABAP_Object_Orientation.md")
      "After '"path":"' (not including this part, indivated by \K), two
      "digits must follow. Then, the further file name is captured with a
      "non-greedy capturing up to '.md'.
      FIND ALL OCCURRENCES OF PCRE `("path":")\K\d\d.*?\.md` IN response
        RESULTS DATA(results)
        IGNORING CASE.

      "The 'results' internal table contains all findings and includes their
      "offset and length information.
      "Using a loop, the actual file names are extracted from the 'response'
      "string and added to an internal table that is to receive more information
      "in the code below.
      LOOP AT results REFERENCE INTO DATA(md).
        tab = VALUE #( BASE tab ( file_name = substring( val = response off = md->offset len = md->length ) ) ).
      ENDLOOP.
      SORT tab BY file_name ASCENDING.

      "In the following loop, the raw markdown content is retrieved using an HTTP GET request, also
      "by creating a client object and using a destination (another plain URL). The URL is constructed
      "using the constant value plus the markdown file that was retrieved before.
      LOOP AT tab REFERENCE INTO DATA(cs).
        url = url_gh && cs->file_name.
        TRY.
            http_client = cl_web_http_client_manager=>create_by_http_destination( i_destination = cl_http_destination_provider=>create_by_url( i_url = url ) ).
            DATA(raw_md) = http_client->execute( if_web_http_client=>get )->get_text(  ).
            "Putting the long string that was retrieved in an internal table of type string
            "for further processing (extracting the code snippets).
            SPLIT raw_md AT |\n| INTO TABLE snippets.
            DATA(flag) = ''.
            "In the loop, all content from the markdown that is not part of a code
            "snippet (indicated by the triple ```) is deleted.
            "The replacements with dummy content in the loop are only done so that
            "the POST request further down can work with the provided content
            "(i.e. avoiding issues with characters such as "; they are inserted later again).
            LOOP AT snippets REFERENCE INTO DATA(line).
              DATA(tabix) = sy-tabix.
              FIND PCRE '^\s*```' IN line->*.
              IF sy-subrc = 0 AND flag = ''.
                line->* = `%%%--START--%%%%`.
                flag = 'X'.
              ELSEIF sy-subrc = 0 AND flag = 'X'.
                line->* = `%%%--END--%%%%`.
                flag = ''.
              ELSEIF flag <> 'X'.
                DELETE snippets INDEX tabix.
              ELSE.
                FIND PCRE `^\s*"` IN line->*.
                IF sy-subrc = 0.
                  DATA(comment1) = 'X'.
                ENDIF.
                FIND PCRE `^\*` IN line->*.
                IF sy-subrc = 0.
                  DATA(comment2) = 'X'.
                ENDIF.
                FIND `***********************************************************************`
                IN line->*.
                IF sy-subrc = 0.
                  DATA(divider) = 'X'.
                ENDIF.
                IF comment1 = 'X' OR comment2 = 'X' OR divider = 'X'.
                  DELETE snippets INDEX tabix.
                  CLEAR: comment1, comment2, divider.
                ELSE.
                  REPLACE ALL OCCURRENCES OF `"` IN line->* WITH `§§§§§`.
                  REPLACE ALL OCCURRENCES OF `\` IN line->* WITH `%%%%%`.
                ENDIF.
              ENDIF.
            ENDLOOP.
            "Adding the code snippets to the information table
            cs->code_snippets = snippets.
            CLEAR snippets.
          CATCH cx_root INTO err.
            cs->error = abap_true.
        ENDTRY.
        DELETE ADJACENT DUPLICATES FROM cs->code_snippets COMPARING table_line.
      ENDLOOP.
      "Creating the final html to be displayed
      LOOP AT tab REFERENCE INTO cs WHERE code_snippets IS NOT INITIAL AND error = abap_false.
        LOOP AT cs->code_snippets REFERENCE INTO DATA(code).
          tabix = sy-tabix.
          IF code->* = `%%%--START--%%%%`.
            code->* = |```|.
          ENDIF.
          IF code->* = `%%%--END--%%%%`.
            code->* = |```|.
            INSERT `*****************` && |\\n|
            INTO cs->code_snippets INDEX tabix + 1.
          ENDIF.
          code->* = code->* && |\\n|.
        ENDLOOP.
        "For the POST request, concatenating the string table to a single string.
        DATA(code_string) = concat_lines_of( table = cs->code_snippets ).
        TRY.
            "Another creation of a client object using a destination
            "This example deals with a POST request.
            http_client = cl_web_http_client_manager=>create_by_http_destination( i_destination =  cl_http_destination_provider=>create_by_url( i_url = url_api ) ).
            DATA(request) = http_client->get_http_request( ).
            request->set_text( `{"text":"` && code_string && `"}` ).
            request->set_header_fields( VALUE #( ( name = 'Accept' value = 'application/vnd.github+json' ) ) ).
            DATA(post) = http_client->execute( if_web_http_client=>post ).
            DATA(status) = post->get_status( ).
            IF status-code <> 200.
              cs->error = abap_true.
              DATA(status_error) = |Post request error: { status-code } / { status-reason }|.
            ELSE.
              "Retrieving the created html code
              DATA(html_code) = post->get_text( ).
              REPLACE ALL OCCURRENCES OF `§§§§§` IN html_code WITH `"`.
              REPLACE ALL OCCURRENCES OF `%%%%%` IN html_code WITH `\`.
              REPLACE ALL OCCURRENCES OF PCRE `(<code>)(\w.*)` IN html_code WITH `$1  $2`.
            ENDIF.
          CATCH cx_root INTO DATA(error).
            cs->error = abap_true.
        ENDTRY.
        "Preparing the title for expandable sections
        DATA(title) = cs->file_name.
        REPLACE ALL OCCURRENCES OF `_` IN title WITH ` `.
        REPLACE PCRE `^..` IN title WITH ``.
        REPLACE `.md` IN title WITH ``.
        "Assembling expandable sections
        html = html &&
          `<br><details>` &&
          ` <summary>` && title && `</summary>` &&
          COND #( WHEN cs->error = abap_false THEN html_code ELSE COND #( WHEN error IS INITIAL THEN status_error ELSE error->get_text( ) ) ) &&
          `</details>`.
      ENDLOOP.
      "Providing the html skeleton and inserting the assembled expandable sections from above
      DATA(final_html) =
      `<!DOCTYPE html>` &&
      `<html>` &&
      `<head>` &&
      `<title>ABAP Cheat Sheet Code Snippets</title>` &&
      `<style>` &&
      `  body {background-color: #F8F8F8;}` &&
      `  h1 {color: blue; font-family: verdana;}` &&
      `  pre {background: #f4f4f4;border: 1px solid #ddd;border-left: 3px solid #0070f2;color: #36454F;` &&
      `       page-break-inside: avoid;font-size: 14px;line-height: 1.3;max-width: 100%;overflow: auto;padding: 1em 1.5em;` &&
      `       display: block;word-wrap: break-word;} ` &&
      `</style>` &&
      `</head>` &&
      `<body>` &&
      `<h1>ABAP Cheat Sheet Code Snippets</h1>` &&
      `<a href="https://github.com/SAP-samples/abap-cheat-sheets">https://github.com/SAP-samples/abap-cheat-sheets</a><br><br>` &&
      html &&
      `<script>` &&
      `  const snippets = document.querySelectorAll("code");` &&
      `  snippets.forEach(elem => {` &&
      `    var abap = elem.innerHTML;` &&
      `    abap = abap.replace(/(\b[A-Z]{2,}\b)/g, "<strong>$1</strong>");` &&
      `    elem.innerHTML = abap;` &&
      `  });` &&
      `</script>` &&
      `</body>` &&
      `</html>`.
      "Displaying the html result in the ADT console
      "Note: Before running the class, clear the ADT console.
      "When the html code is displayed in the ADT console, you can, for example,
      "create a file named ABAP_cheat_sheet_code.html on your local machine.
      "Open the file in an editor, copy & paste the entire ADT console content and
      "save the local file. In doing so, you have various code snippets at your
      "disposal offline.
      out->write( final_html ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 
</details>  



</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Reading and Writing XLSX Content

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>XCO_CP_XLSX</code> </td>
<td>

The XCO library offers classes (`XCO_CP_XLSX` and others) and methods for reading and writing XLSX content. You can find more information [here](https://help.sap.com/docs/btp/sap-business-technology-platform/xlsx). The following example demonstrates a selection of  methods and includes the following steps:

- Importing existing XLSX content into your SAP BTP ABAP Environment system (not related to the XCO library; just to have content to work with in the self-contained example below)
  - This is a simplified, nonsemantic, and explorative RAP example (not delving into RAP as such; just using various ABAP repository objects related to RAP) solely for importing XLSX content to work with in the example. 
  - ⚠️ Note the repository's readme file for disclaimer and the [documentation](https://help.sap.com/docs/btp/sap-business-technology-platform/xlsx-read-access) for security considerations when importing and processing external content.
  - The import is done using an automatically created SAP Fiori Elements app preview, which provides a simple UI for uploading local XLSX content.
  - The repository objects are automatically created in ADT when walking through a wizard. Refer to the prerequisite steps for details.
- Reading XLSX content into an internal table using XCO
- Writing XLSX content using XCO based on internal table content
  - A demo class explores a selection of XCO classes and methods. You can find the code in the expandable section.
- Exporting the adapted XLSX content (not related to the XCO library; just to visualize newly created XLSX content using XCO)

Expand the following collapsible section to view the steps and code of the example. 

<details>
  <summary>Expand to view the details</summary>
  <!-- -->
<br>

**Prerequisite Steps for the XLSX Content Import into the SAP BTP ABAP Environment**

The XLSX XCO module works with XLSX content in the form of an xstring. The following example assumes that you have XLSX content available as xstring. To try out the example, you can proceed as follows: 


- In your ABAP Cloud project in ADT, create a database table in the desired target package. This example uses the name `ztdemo_abap_xlsx`. 
- Give it a description, such as *Demo table*, and assign it to a transport. 
- Set up the example database table as follows:

  ```abap
  @EndUserText.label : 'Demo table'
  @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
  @AbapCatalog.tableCategory : #TRANSPARENT
  @AbapCatalog.deliveryClass : #A
  @AbapCatalog.dataMaintenance : #RESTRICTED
  define table ztdemo_abap_xlsx {

    key client            : abap.clnt not null;
    key id                : sysuuid_x16 not null;
    file_content          : abap.rawstring(0);
    file_name             : abap.char(128);
    file_mimetype         : abap.char(128);
    @AbapCatalog.anonymizedWhenDelivered : true
    local_created_by      : abp_creation_user;
    local_created_at      : abp_creation_tstmpl;
    @AbapCatalog.anonymizedWhenDelivered : true
    local_last_changed_by : abp_locinst_lastchange_user;
    local_last_changed_at : abp_locinst_lastchange_tstmpl;
    last_changed_at       : abp_lastchange_tstmpl;

  }
  ```

- Activate the database table.
- In the *Project Explorer* view on the left, locate your created database table and right-click on it. Choose *Generate ABAP Repository Objects...* from the top section of the context menu.
- This opens the *Generate ABAP Repository Objects* dialog box. 
- Select *OData UI Service*. Find information on the right about the RAP-specific repository objects to be created. These objects include CDS entities, BDEFs, ABPs, service definition and binding, among others.
- Choose *Next*.
- Select the package and choose *Next*.
- The next screen in the dialog box lets you specify additional details for RAP repository objects, such as alias names. However, you can also leave it as is and choose *Next*. 
- The following screen displays a list of artifacts set to be created. Choose *Next*. 
- Then, select the transport request and choose *Finish*. 
- The creation of the repository objects may take some time. Once completed, the service binding will appear. Keep it open, but refrain from making any adjustments at this point. 
- Proceed to access the generated CDS entity. Note that two CDS entities are created. Access the one **without** the *with projection on* specification (the example entity name is  `ZR_TDEMO_ABAP_XLSX`). The specification appears as follows.
  ```abap
  ...
  define root view entity ZR_TDEMO_ABAP_XLSX
    as select from ZTDEMO_ABAP_XLSX
  ...  
  ```
- Adapt the CDS entity as follows. It is essential that the `@Semantics` annotations are specified and the provided names match. 
  ```abap
  @AccessControl.authorizationCheck: #NOT_REQUIRED
  @Metadata.allowExtensions: true
  @EndUserText.label: 'Demo CDS Entity'
  define root view entity ZR_TDEMO_ABAP_XLSX
    as select from ztdemo_abap_xlsx
  {
    key id                as Id,
    @Semantics.largeObject: { mimeType: 'FileMimetype',
                              fileName: 'FileName',
                              contentDispositionPreference: #ATTACHMENT }
    file_content          as FileContent,
    file_name             as FileName,
    @Semantics.mimeType: true
    file_mimetype         as FileMimetype,
    @Semantics.user.createdBy: true
    @UI.hidden
    local_created_by      as LocalCreatedBy,
    @Semantics.systemDateTime.createdAt: true
    @UI.hidden
    local_created_at      as LocalCreatedAt,
    @Semantics.user.localInstanceLastChangedBy: true
    @UI.hidden
    local_last_changed_by as LocalLastChangedBy,
    @Semantics.systemDateTime.localInstanceLastChangedAt: true
    @UI.hidden
    local_last_changed_at as LocalLastChangedAt,
    @Semantics.systemDateTime.lastChangedAt: true
    @UI.hidden
    last_changed_at       as LastChangedAt

  }
  ```
- Activate the CDS entity.
- Return to the service binding located in your package under *Business Services -> Service Bindings* (e.g.  `ZUI_TDEMO_ABAP_XLSX_O4`).
- Choose the *Publish* button for the *Local Service Endpoint*. The publishing may take some time.
- After publishing, locate the *Service Version Details* section on the right. Under *Entity Set and Association*, right-click the newly created entity set and select *Open Fiori Elements App Preview*.
- This action opens the SAP Fiori Elements app in a new browser window.
- Within the app, choose the *Create* button.
- You should see a *Browse* button in the *FileContent* field.
- Choose this button and select a local XLSX file. Note: You can use the example XLSX content provided below. Only with this content does the example class work properly. For your own explorations, adapt the class code accordingly.
- After uploading the file, choose *Create* at the bottom to save the entry.
- Refer to the comments in the example class below. You can either exit the app now, leaving one entry in the database with the XLSX content, or use the UUID value of the created entry in the `WHERE` clause of the `SELECT` statement at the beginning of the `main` method implementation below.


> **💡 Note**<br>
> - If there are issues with the UI (e.g. you cannot upload), try out UI V2.
> - Create a new service definition, e.g. right-click the *Service Definition* folder and choose *New Service Definition*. 
> - Provide a name (e.g. `ZUI_TDEMO_ABAP_XLSX_O2`) and description. Choose *Next*.
> - The code should look as follows:
>   ```abap
>   @EndUserText.label: 'Service Definition'
>   define service ZUI_TDEMO_ABAP_XLSX_O2
>    provider contracts odata_v2_ui {
>     expose ZC_TDEMO_ABAP_XLSX;
>   }
>   ```
> - Activate it and create a new service binding, e.g. by right-clicking the service definition and choosing *New Service Binding*.
> - Create the binding (e.g. with the same name as the service definition `ZUI_TDEMO_ABAP_XLSX_O2`), and select *OData V2 - UI* as *Binding Type*.
> - Activate and publish the local service endpoint.
> - Access the preview app as described above.

> **📝 Example XLSX content**<br>
>- Copy and paste the following data into your demo XLSX file. The example only uses simple character-like values. Note the information in the documentation about value transformation when processing the XLSX content.
>- Your XLSX creation program may have a feature, such as *text to column*, that can help you divide the text into separate columns. In this case, the first four columns are filled.  
>- The delimiter is a comma, indicating that there are four columns to be filled with the demo data. 
>- Name the first worksheet (where you insert the demo data) `Sheet1`.
> ``` 
>AA,American Airlines,USD,http://www.aa.com
>AC,Air Canada,CAD,http://www.aircanada.ca
>AF,Air France,EUR,http://www.airfrance.fr
>BA,British Airways,GBP,http://www.british-airways.com
>CO,Continental Airlines,USD,http://www.continental.com
>DL,Delta Airlines,USD,http://www.delta-air.com
>LH,Lufthansa,EUR,http://www.lufthansa.com
>JL,Japan Airlines,JPY,http://www.jal.co.jp
>NW,Northwest Airlines,USD,http://www.nwa.com
>QF,Qantas Airways,AUD,http://www.qantas.com.au
>SA,South African Air.,ZAR,http://www.saa.co.za
>SQ,Singapore Airlines,SGD,http://www.singaporeair.com
>SR,Swiss,CHF,http://www.swiss.com
>UA,United Airlines,USD,http://www.ual.com
>```

**Exploring the XCO XLSX Module**

Assuming you have the XLSX content created and uploaded above on your system, you can explore the following example using the XCO classes/methods. Set up a demo class called `zcl_some_class` and use the code provided below. After activating it, choose *F9* in ADT to run the class. The example is designed to show output in the console. 

> **💡 Note**<br>
> - Refer to the comments in the code for information.
> - If you have used different names than those in this example, make sure to replace those names in the code.
> - If your artifacts have a different setup, names, or XLSX content, the example class will not function properly. You willl need to modify the class code to match your specific requirements.

```abap
CLASS zcl_some_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_some_class IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    "As a prerequisite, you have walked through the steps mentioned. RAP repository objects
    "were created, adapted as mentioned, and activated. You have at least one XLSX file imported
    "using the preview app. XLSX content is available in the database table as xstring.
    "--------------------------------------- NOTE ----------------------------------------
    "------ This is only intended for demonstration purposes!! Do note the security ------
    "------ aspects etc. when importing external content. The focus of this example ------
    "------ is to explore the XCO XLSX  module. ------------------------------------------

    "The SELECT statement assumes there is one entry available. Otherwise, adapt the
    "statement accordingly, e.g. by adding a WHERE clause and providing the UUID you can
    "copy for a created entry in the Fiori Elements preview app. Note the XCO class with
    "which to transform the UUID. Replace the dummy UUID.
    SELECT SINGLE id, file_content FROM ztdemo_abap_xlsx
    "WHERE id = @( CONV sysuuid_c32(
    " xco_cp_uuid=>format->c32->from_uuid( xco_cp_uuid=>format->c36->to_uuid( 'a1a2a3a4-b1b2-c1c2-d3d4-e5e6e7e8e9e0' ) ) ) )
    INTO @DATA(xlsx).

    IF sy-subrc <> 0 OR xlsx-file_content IS INITIAL.
      out->write( `No XLSX content available to work with` ).
      RETURN.
    ENDIF.

    out->write( `Exploring reading and writing XLSX content with the XCO library` ).
    out->write( |\n\n| ).
    out->write( `-------------------- Read access to XLSX content --------------------` ).
    out->write( |\n\n| ).

    "Note: Using method chaining, the following method calls can also be done in a more concise way.

    "As a first step (for reading and writing XLSX content), getting a handle to process
    "XLSX content, which is available as xstring.
    DATA(xlsx_doc) = xco_cp_xlsx=>document->for_file_content( xlsx-file_content ).

    "------------------------ Read access to XLSX content ------------------------

    "Getting read access
    DATA(read_xlsx) = xlsx_doc->read_access( ).

    "Reading the first worksheet of the XLSX file
    DATA(worksheet1) = read_xlsx->get_workbook( )->worksheet->at_position( 1 ).
    "You can also specify the name of the worksheet
    "DATA(worksheet1) = read_xlsx->get_workbook( )->worksheet->for_name( 'Sheet1' ).

    "Using selection patterns
    "The XCO XLSX module works with selection patterns that define how the content
    "in a worksheet is selected (i.e. whether everything or a restricted set is selected).
    "Check the documentation for further information.
    "The following selection pattern respects all values.
    DATA(pattern_all) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to( )->get_pattern( ).

    "The following selection pattern restricts the content used. Here, column A is left
    "out (starting from column B), and the first two rows are skipped (starting from row
    "number 3).
    DATA(pattern_restrict) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to(
      )->from_column( xco_cp_xlsx=>coordinate->for_alphabetic_value( 'B' )
      )->from_row( xco_cp_xlsx=>coordinate->for_numeric_value( 3 )
      )->get_pattern( ).

    "--- Reading using row streams ---
    "The examples reads into an internal table of a known structure.
    "Declaring an internal table based on a local structure (representing
    "the 'known structure')
    TYPES: BEGIN OF carr_line_type,
             carrid   TYPE c LENGTH 3,
             carrname TYPE c LENGTH 20,
             currcode TYPE string,
             url      TYPE c LENGTH 255,
           END OF carr_line_type,
           carr_tab_type TYPE TABLE OF carr_line_type WITH EMPTY KEY.
    DATA carr_tab TYPE carr_tab_type.

    "Note that the content is written to a reference to an internal table
    "with the write_to method.
    worksheet1->select( pattern_all
      )->row_stream(
      )->operation->write_to( REF #( carr_tab )
      )->execute( ).

    "Retrieving the table lines to compare the values with the next
    "example that uses a different selection pattern
    DATA(lines_tab1) = lines( carr_tab ).
    "Displaying the read result in the console
    out->write( |Lines in carr_tab: { lines_tab1 }| ).
    out->write( data = carr_tab name = `carr_tab` ).
    out->write( |\n\n| ).
    CLEAR carr_tab.

    "Compare the content/output of the previous internal table and
    "the following one. The CARRID field is not filled because of starting
    "the selection in the second column. The first two entries of the previous
    "internal table are not available in the other because of starting in
    "the third row.
    worksheet1->select( pattern_restrict
      )->row_stream(
      )->operation->write_to( REF #( carr_tab )
      )->execute( ).

    DATA(lines_tab2) = lines( carr_tab ).
    out->write( |Lines in carr_tab: { lines_tab2 }| ).
    out->write( data = carr_tab name = `carr_tab` ).
    out->write( |\n\n| ).

    "--- Reading via a cursor ---
    "I.e. you specify the cursor position which represents the coordinate
    "values of column and row. Once specified, you can access the content based
    "on the cursor position in various directions using the MOVE* methods.

    "The following internal table is created for output purposes and includes
    "various pieces of cursor position information.
    TYPES: BEGIN OF cursor_info,
             string_value                  TYPE string,
             cursor_position_column_alphab TYPE string,
             cursor_position_column_num    TYPE i,
             cursor_position_row_alphab    TYPE string,
             cursor_position_row_num       TYPE i,
           END OF cursor_info.
    DATA itab_cursor_info_move_down TYPE TABLE OF cursor_info WITH EMPTY KEY.
    DATA itab_cursor_info_move_right TYPE TABLE OF cursor_info WITH EMPTY KEY.
    DATA itab_cursor_info_move_left TYPE TABLE OF cursor_info WITH EMPTY KEY.
    DATA itab_cursor_info_move_up TYPE TABLE OF cursor_info WITH EMPTY KEY.
    DATA string_value TYPE string.

    "Positioning the cursor (starting in column B, third row)
    DATA(cursor) = worksheet1->cursor(
      io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'B' )
      io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( 3 )
       ).

    "Retrieving column and row information and storing it in a structure for
    "output purposes (to show where the cursor is positioned as specifed above)
    DATA(cursor_set) = VALUE cursor_info(
        cursor_position_column_alphab = cursor->position->column->get_alphabetic_value( )
        cursor_position_column_num = cursor->position->column->get_numeric_value( )
        cursor_position_row_alphab = cursor->position->row->get_alphabetic_value( )
        cursor_position_row_num = cursor->position->row->get_numeric_value( )
     ).

    "Getting the value
    "Note: String value transformation is done in the example. Note the information
    "in the documentation.
    cursor->get_cell( )->get_value(
        )->set_transformation( xco_cp_xlsx_read_access=>value_transformation->string_value
        )->write_to( REF #( string_value ) ).
    cursor_set-string_value = string_value.

    out->write( |Cursor information| ).
    out->write( |\n\n| ).
    out->write( data = cursor_set name = `cursor_set` ).
    out->write( |\n\n| ).

    "Looping across the cells and retrieving individual cursor position information and
    "the string values
    "--- Moving down ---
    WHILE cursor->has_cell( ) = abap_true AND cursor->get_cell( )->has_value( ) = abap_true.
      DATA(cell_cnt) = cursor->get_cell( ).
      "Storing column and row information in data objects
      DATA(cursor_position_column_alphab) = cursor->position->column->get_alphabetic_value( ).
      DATA(cursor_position_column_num) = cursor->position->column->get_numeric_value( ).
      DATA(cursor_position_row_alphab) = cursor->position->row->get_alphabetic_value( ).
      DATA(cursor_position_row_num) = cursor->position->row->get_numeric_value( ).

      "Getting the content of the cell by applying a value transformation to the
      "content
      cell_cnt->get_value(
        )->set_transformation( xco_cp_xlsx_read_access=>value_transformation->string_value
        )->write_to( REF #( string_value ) ).

      "Adding the column and row information and the string value to an internal table
      "for output purposes
      APPEND VALUE #(
        string_value = string_value cursor_position_column_alphab = cursor_position_column_alphab
        cursor_position_column_num = cursor_position_column_num
        cursor_position_row_alphab = cursor_position_row_alphab
        cursor_position_row_num = cursor_position_row_num ) TO itab_cursor_info_move_down.

      "Moving the cursor down
      TRY.
          cursor->move_down( ).
        CATCH cx_xco_runtime_exception.
          EXIT.
      ENDTRY.
    ENDWHILE.

    out->write( data = itab_cursor_info_move_down name = `itab_cursor_info_move_down` ).
    out->write( |\n\n| ).

    "--- Moving right ---
    "Repositioning the cursor
    cursor = worksheet1->cursor(
     io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'B' )
     io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( 3 ) ).

    WHILE cursor->has_cell( ) = abap_true AND cursor->get_cell( )->has_value( ) = abap_true.
      cell_cnt = cursor->get_cell( ).
      cursor_position_column_alphab = cursor->position->column->get_alphabetic_value( ).
      cursor_position_column_num = cursor->position->column->get_numeric_value( ).
      cursor_position_row_alphab = cursor->position->row->get_alphabetic_value( ).
      cursor_position_row_num = cursor->position->row->get_numeric_value( ).
      cell_cnt->get_value(
        )->set_transformation( xco_cp_xlsx_read_access=>value_transformation->string_value
        )->write_to( REF #( string_value ) ).
      APPEND VALUE #( string_value = string_value cursor_position_column_alphab = cursor_position_column_alphab
      cursor_position_column_num = cursor_position_column_num
      cursor_position_row_alphab = cursor_position_row_alphab
      cursor_position_row_num = cursor_position_row_num ) TO itab_cursor_info_move_right.
      "Moving the cursor right
      TRY.
          cursor->move_right( ).
        CATCH cx_xco_runtime_exception.
          EXIT.
      ENDTRY.
    ENDWHILE.

    out->write( data = itab_cursor_info_move_right name = `itab_cursor_info_move_right` ).
    out->write( |\n\n| ).

    "--- Moving left ---
    "Repositioning the cursor
    cursor = worksheet1->cursor(
     io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'B' )
     io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( 3 ) ).

    WHILE cursor->has_cell( ) = abap_true AND cursor->get_cell( )->has_value( ) = abap_true.
      cell_cnt = cursor->get_cell( ).
      cursor_position_column_alphab = cursor->position->column->get_alphabetic_value( ).
      cursor_position_column_num = cursor->position->column->get_numeric_value( ).
      cursor_position_row_alphab = cursor->position->row->get_alphabetic_value( ).
      cursor_position_row_num = cursor->position->row->get_numeric_value( ).
      cell_cnt->get_value(
        )->set_transformation( xco_cp_xlsx_read_access=>value_transformation->string_value
        )->write_to( REF #( string_value ) ).
      APPEND VALUE #( string_value = string_value cursor_position_column_alphab = cursor_position_column_alphab
      cursor_position_column_num = cursor_position_column_num
      cursor_position_row_alphab = cursor_position_row_alphab
      cursor_position_row_num = cursor_position_row_num ) TO itab_cursor_info_move_left.
      "Moving the cursor left
      TRY.
          cursor->move_left( ).
        CATCH cx_xco_runtime_exception.
          EXIT.
      ENDTRY.
    ENDWHILE.

    out->write( data = itab_cursor_info_move_left name = `itab_cursor_info_move_left` ).
    out->write( |\n\n| ).

    "--- Moving up ---
    "Repositioning the cursor
    cursor = worksheet1->cursor(
     io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'B' )
     io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( 3 ) ).

    WHILE cursor->has_cell( ) = abap_true AND cursor->get_cell( )->has_value( ) = abap_true.
      cell_cnt = cursor->get_cell( ).
      cursor_position_column_alphab = cursor->position->column->get_alphabetic_value( ).
      cursor_position_column_num = cursor->position->column->get_numeric_value( ).
      cursor_position_row_alphab = cursor->position->row->get_alphabetic_value( ).
      cursor_position_row_num = cursor->position->row->get_numeric_value( ).
      cell_cnt->get_value(
        )->set_transformation( xco_cp_xlsx_read_access=>value_transformation->string_value
        )->write_to( REF #( string_value ) ).
      APPEND VALUE #( string_value = string_value cursor_position_column_alphab = cursor_position_column_alphab
      cursor_position_column_num = cursor_position_column_num
      cursor_position_row_alphab = cursor_position_row_alphab
      cursor_position_row_num = cursor_position_row_num ) TO itab_cursor_info_move_up.
      "Moving the cursor up
      TRY.
          cursor->move_up( ).
        CATCH cx_xco_runtime_exception.
          EXIT.
      ENDTRY.
    ENDWHILE.

    out->write( data = itab_cursor_info_move_up name = `itab_cursor_info_move_up` ).
    out->write( |\n\n| ).

**********************************************************************

    "------------------------ Write access to XLSX content ------------------------
    "- The following example creates a new XLSX document.
    "- Two worksheets are created.
    "- New content is added (based on demo internal tables).
    "- The existing XLSX content is replaced by the new content. An ABAP SQL UPDATE
    "  statement is included to update the file content. If you have implemented
    "  the SAP Fiori Elements and access the entry, you can download the new
    "  XLSX file to your local machine.
    "---------------------------------- NOTE ----------------------------------
    "---- Do note that this is just for demonstration purposes to quickly  ----
    "---- explore the new XLSX content (without considering RAP-related    ----
    "---  things and other database table fields such as the time stamps). ----

    out->write( `-------------------- Write XLSX content --------------------` ).
    out->write( |\n\n| ).

    "Creating a new XLSX document
    DATA(write_xlsx) = xco_cp_xlsx=>document->empty( )->write_access( ).
    "Note that the name of the created worksheet is Sheet1
    "Accessing the first worksheet via the position
    DATA(ws1) = write_xlsx->get_workbook( )->worksheet->at_position( 1 ).

    "Poviding XLSX content
    "--- Writing data via a row stream ---
    "In this case, the structure is statically known. The content of an internal table
    "is used.
    DATA(itab) = VALUE carr_tab_type(
      ( carrid = 'WX' carrname = 'Air WZ' currcode = 'GBP' url = 'some_url_wx' )
      ( carrid = 'XY' carrname = 'XY Airlines' currcode = 'EUR' url = 'some_url_xy' )
      ( carrid = 'YZ' carrname = 'YZ Airways' currcode = 'USD' url = 'some_url_yz' )
    ).

    "As is the case with the read access, a pattern is used.
    "The following pattern uses the entire content.
    DATA(pattern_all4write) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to( )->get_pattern( ).

    "Writing the internal table lines to the worksheet using the write_from method
    ws1->select( pattern_all4write
      )->row_stream(
      )->operation->write_from( REF #( itab )
      )->execute( ).

    "--- Writing data via a cursor ---
    "To demonstrate this writing method, a new worksheet is created using the
    "add_new_sheet method. A name is provided, too.
    DATA(create_new_worksheet) = write_xlsx->get_workbook( )->add_new_sheet( `Sheet2` ).

    "Accessing the second worksheet via the position
    DATA(ws2) = write_xlsx->get_workbook( )->worksheet->at_position( 2 ).

    "Checking if the new worksheet exists at position 2 using the exists method
    DATA(ws2_pos_exists) = write_xlsx->get_workbook( )->worksheet->at_position( 2 )->exists( ).
    IF ws2_pos_exists = abap_true.
      out->write( `The newly created worksheet exists at position 2` ).
    ELSE.
      out->write( `The newly created worksheet does not exist at position 2` ).
    ENDIF.
    out->write( |\n\n| ).

    "Another internal table to have different demo data in the two worksheets
    DATA(itab2) = VALUE carr_tab_type(
      ( carrid = 'ZA' carrname = 'Air ZA' currcode = 'CAD' url = 'some_url_za' )
      ( carrid = 'YB' carrname = 'YB Airlines' currcode = 'JPY' url = 'some_url_yb' )
      ( carrid = 'XC' carrname = 'XC Airways' currcode = 'ZAR' url = 'some_url_xc' )
    ).

    "Example implementation: Looping across the internal table to put individual
    "component values in the worksheet
    LOOP AT itab2 INTO DATA(wa).
      DATA(tabix) = sy-tabix.
      "Positioning the cursor
      "The idea is to write all the content starting in the top left
      "corner of the worksheet. You can imagine other positions when
      "specifying actual parameters for io_column and io_row.
      "More methods are available. See the documentation.
      "Note: The cursor is repositioned with every loop pass here starting
      "with the first column. The row value is represented by the table
      "index (sy-tabix value), i.e. the second data set of the internal table
      "available in the work area in the second loop pass table goes in the
      "second row, and so on.
      DATA(cursor4write) = ws2->cursor(
        io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( 'A' )
        io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( tabix ) ).

      cursor4write->get_cell( )->value->write_from( wa-carrid ).
      "Moving right to populate other columns
      cursor4write->move_right( )->get_cell( )->value->write_from( wa-carrname ).
      cursor4write->move_right( )->get_cell( )->value->write_from( wa-url ).
      "Note: The example skips one component.
    ENDLOOP.

    "Once content has been provided, getting the file content (both worksheets of
    "the example)
    DATA(file_content) = write_xlsx->get_file_content( ).

    "In this example, the uploaded XLSX content is replaced by the newly created
    "XLSX content by just updating the file_content field. It is just for demonstration
    "purposes as commented above. 
    "If you have created the SAP Fiori Elements preview app, you can download the XLSX file.
    "To do so, access the app via the service binding as described. Find the entry with the
    "provided ID (it is output in the console), and choose it. In the detail view of the app,
    "you can choose the button next to the file content. It should download an XLSX file,
    "now having the created content.
    UPDATE ztdemo_abap_xlsx SET file_content = @file_content WHERE id = @xlsx-id.

    IF sy-subrc = 0.
      out->write( |The database table was updated. If implemented, check the file with id { xlsx-id }| ).
    ELSE.
      out->write( `The database table was not updated.` ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

</details>  

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>