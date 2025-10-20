<a name="top"></a>

# Released ABAP Classes

- [Released ABAP Classes](#released-abap-classes)
  - [Excursions](#excursions)
    - [Available Classes in ABAP for Cloud Development](#available-classes-in-abap-for-cloud-development)
    - [Cloud Development Successors](#cloud-development-successors)
  - [Running a Class and Displaying Output in the ADT Console](#running-a-class-and-displaying-output-in-the-adt-console)
  - [Creating and Transforming UUIDs](#creating-and-transforming-uuids)
  - [XCO Representations of SY Components](#xco-representations-of-sy-components)
  - [RAP](#rap)
  - [Transactional Consistency](#transactional-consistency)
  - [Numbers and Calculations](#numbers-and-calculations)
  - [String Processing](#string-processing)
  - [Handling Codepages and Binary Data](#handling-codepages-and-binary-data)
  - [Regular Expressions](#regular-expressions)
  - [Time and Date](#time-and-date)
  - [Calendar-Related Information](#calendar-related-information)
  - [Runtime Type Services (RTTS)](#runtime-type-services-rtts)
  - [Assignments](#assignments)
  - [Information about Non-Initial Structure Components](#information-about-non-initial-structure-components)
  - [Comparing Content of Compatible Internal Tables](#comparing-content-of-compatible-internal-tables)
  - [Dynamic Programming](#dynamic-programming)
  - [Getting the Current User Name](#getting-the-current-user-name)
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
  - [Zip Files](#zip-files)
  - [ABAP Unit](#abap-unit)
  - [Units of Measurement](#units-of-measurement)
  - [Programmatic ABAP Test Cockpit (ATC) Check](#programmatic-abap-test-cockpit-atc-check)
  - [Handling Number Ranges](#handling-number-ranges)
  - [Releasing APIs](#releasing-apis)
  - [Application Jobs](#application-jobs)
  - [Generative AI](#generative-ai)
  - [Programmatically Creating and Releasing Transport Requests](#programmatically-creating-and-releasing-transport-requests)
  - [Repairing and Cleaning up HTML and XML Documents](#repairing-and-cleaning-up-html-and-xml-documents)
  - [Creating and Using IDE Actions](#creating-and-using-ide-actions)


This ABAP cheat sheet contains a selection of [released](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreleased_api_glosry.htm) ABAP classes that are available in [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm). It serves as a quick introduction, along with code snippets to explore the functionality in action.

> [!NOTE]  
> - The cheat sheet is not a comprehensive overview, and the code snippets do not claim to be comprehensive as far as options, methods, or parameters are concerned. It is intended to give you a rough overview, for you to get an idea. It is an invitation to a more in-depth exploration.
> - For more information and where available, refer to the class documentation (for example, choose F2 when the cursor is on the class name in ADT), the ABAP Keyword Documentation, and the SAP Help Portal documentation.
> - You might find that different classes can achieve similar or the same results, especially with the Extension Components Library (XCO), a general-purpose development library designed specifically for ABAP for Cloud Development. Choose the classes that best meet your needs.
> - For XCO classes, the cheat sheet covers released XCO APIs. In [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm), XCO APIs are also available that do not have `CP` in the class name. These classes are not covered here. 
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)

## Excursions

### Available Classes in ABAP for Cloud Development

If available to you, you have accessed an [SAP BTP ABAP Environment](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensap_btp_abap_env_glosry.htm) using the [ABAP development tools for Eclipse (ADT)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenadt_glosry.htm).
Access to SAP-provided repository objects is restricted to objects that have been released for [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm) ([released APIs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreleased_api_glosry.htm)). You can find the released repository objects in the *Project Explorer* view in ADT under *Released Objects*. The classes are located in the *Source Code Library* folder:

![Released APIs](./files/released_APIs.png)

You can also programmatically get the released objects. You can use specific XCO classes or a CDS view, as shown in the example code snippet below.

```abap
SELECT ReleasedObjectType, ReleasedObjectName, ReleaseState
  FROM i_apisforclouddevelopment
  WHERE releasestate = 'RELEASED'
  AND ReleasedObjectType = 'CLAS'
  ORDER BY ReleasedObjectName
  INTO TABLE @DATA(released_classes).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Cloud Development Successors

The `I_APIsWithCloudDevSuccessor` view provides information on objects that cannot or should no longer be used in ABAP for Cloud Development along with their successors that can be used.

```abap
SELECT * FROM I_APIsWithCloudDevSuccessor
 INTO TABLE @DATA(successors_all).

SELECT * FROM I_APIsWithCloudDevSuccessor
 WHERE PredecessorObjectType = 'TABL'
 INTO TABLE @DATA(successors_tables).

SELECT SINGLE * FROM I_APIsWithCloudDevSuccessor
 WHERE PredecessorObjectName = 'TADIR'
 INTO @DATA(successor_tadir).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Running a Class and Displaying Output in the ADT Console

The table includes the <code>IF_OO_ADT_CLASSRUN</code> interface.

<table>
<tr>
<td> Interface/Class </td> <td> Details/Code Snippet </td>
</tr>

<tr>
<td> <code>IF_OO_ADT_CLASSRUN</code> </td>
<td>

- By implementing the <code>IF_OO_ADT_CLASSRUN</code> interface in a global class, you can make the class executable. 
- In ADT, you can execute the class using F9. 
- The statements that are processed when executing the class can be included in the implementation of the `if_oo_adt_classrun~main` method.
- Using `out->write( ... ).` statements, you can output the content of data objects to the ADT console. The `name` parameter can be used to precede the data object content with a string.

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
    out->write( `Hello world` ).

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
    out->write( data = itab name = `itab` ).
  ENDMETHOD.
ENDCLASS.
``` 

</td>
</tr>


<tr>
<td> <code>CL_DEMO_CLASSRUN</code> </td>
<td>
As an alternative to using the <code>IF_OO_ADT_CLASSRUN</code> interface for displaying output in the console, you can also use the <code>CL_DEMO_CLASSRUN</code> class, which offers more methods.
For more information, refer to <a href="https://blogs.sap.com/2023/10/24/abap-console-reloaded/">this blog</a>.
The following example makes use of the <code>CL_DEMO_CLASSRUN</code> class. A structure and an internal table are displayed in the console. A structure component is a reference variable, which is automatically dereferenced. Plus, the <code>write_xml</code> method is shown, which displays XML data.
<br>

``` abap
CLASS zcl_demo_abap DEFINITION
  INHERITING FROM cl_demo_classrun
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS main REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
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

<tr>
<td> <code>CL_XCO_CP_ADT_SIMPLE_CLASSRUN</code> </td>
<td>

XCO alternative for output in the ADT console

<br>

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  INHERITING FROM cl_xco_cp_adt_simple_classrun
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: main REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD main.
    out->write( `Hello world` ).
  ENDMETHOD.
ENDCLASS.
``` 

</td>
</tr>

</table>

> [!TIP]
> - The `CL_DEMO_OUTPUT_CLOUD` class is available for demo purposes only. It wraps the output functionalities of the `CL_DEMO_OUTPUT` class that is available in Standard ABAP and used for demo display purposes, especially useful for displaying internal table content.
>- As an excursion, find examples using the class in [Creating and Using IDE Actions](#creating-and-using-ide-actions).
> - Find more information in this [blog](https://community.sap.com/t5/technology-blog-posts-by-sap/cl-demo-output-goes-abap-cloud/ba-p/13782903).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Creating and Transforming UUIDs

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_SYSTEM_UUID</code> </td>
<td>
Creating and converting system UUIDs with various algorithms 
<br><br>

``` abap
TRY.
    "----------- Creating UUIDs -----------

    "16 Byte System UUID in Binary Format
    "Example: 429A229A88021EDFB7E2E25DF99A8E73
    DATA(uuid_x16) = cl_system_uuid=>create_uuid_x16_static( ).

    "16 Byte System UUID in Base64
    "Example: GfeYceW27j{tuk9T{PgkSm
    DATA(uuid_c22) = cl_system_uuid=>create_uuid_c22_static( ).

    "16 Byte System UUID in Base32
    "Example: IKNCFGUIAIPN7N7C4JO7TGWOOM
    DATA(uuid_c26) = cl_system_uuid=>create_uuid_c26_static( ).

    "16 Byte System UUID in Hex Format
    "Example: 429A229A88021EDFB7E2E25DF99AEE73
    DATA(uuid_c32) = cl_system_uuid=>create_uuid_c32_static( ).

    "16 Byte System UUID in RFC4122 Format
    "Example: 429A229A-8802-1EDF-B7E2-E25DF99B0E73
    DATA(uuid_c36) = cl_system_uuid=>create_uuid_c36_static( ).

    "----------- Converting UUIDs -----------

    cl_system_uuid=>convert_uuid_x16_static(
      EXPORTING
        uuid     = uuid_x16
      IMPORTING
        uuid_c22 = DATA(x16_to_c22)
        uuid_c32 = DATA(x16_to_c32)
        uuid_c26 = DATA(x16_to_c26)
        uuid_c36 = DATA(x16_to_c36) ).

    cl_system_uuid=>convert_uuid_c22_static(
      EXPORTING
        uuid     = uuid_c22
      IMPORTING
        uuid_x16 = DATA(c22_to_x16)
        uuid_c32 = DATA(c22_to_c32)
        uuid_c26 = DATA(c22_to_c26)
        uuid_c36 = DATA(c22_to_c36) ).

    cl_system_uuid=>convert_uuid_c26_static(
      EXPORTING
        uuid     = uuid_c26
      IMPORTING
        uuid_x16 = DATA(c26_to_x16)
        uuid_c22 = DATA(c26_to_c22)
        uuid_c32 = DATA(c26_to_c32)
        uuid_c36 = DATA(c26_to_c36) ).

    cl_system_uuid=>convert_uuid_c32_static(
      EXPORTING
        uuid     = uuid_c32
      IMPORTING
        uuid_x16 = DATA(c32_to_x16)
        uuid_c22 = DATA(c32_to_c22)
        uuid_c26 = DATA(c32_to_c26)
        uuid_c36 = DATA(c32_to_c36) ).

    cl_system_uuid=>convert_uuid_c36_static(
      EXPORTING
        uuid     = uuid_c36
      IMPORTING
        uuid_x16 = DATA(c36_to_x16)
        uuid_c22 = DATA(c36_to_c22)
        uuid_c26 = DATA(c36_to_c26)
        uuid_c32 = DATA(c36_to_c32) ).

  CATCH cx_uuid_error INTO DATA(error).
    DATA(error_text) = error->get_text( ).
ENDTRY.
``` 

</td>
</tr>
<tr>
<td> <code>XCO_CP</code> <br> <code>XCO_CP_UUID</code> </td>
<td>
Creating UUIDs and transforming between different UUID formats using XCO
<br><br>

``` abap
"Creating UUIDs
"Type sysuuid_x16
DATA(x16_uuid) = xco_cp=>uuid( )->value.
"Other types using the 'as' method and providing a format
"Note that a data object of type string is returned when using the 'value' attribute.
DATA(c22_string) = xco_cp=>uuid( )->as( xco_cp_uuid=>format->c22 )->value.
DATA(c32_string) = xco_cp=>uuid( )->as( xco_cp_uuid=>format->c32 )->value.
DATA(c36_string) = xco_cp=>uuid( )->as( xco_cp_uuid=>format->c36 )->value.

"Tranforming between different formats
DATA(uuid_string_c36) = `429A229A-8802-1EDF-B7E2-E25DF99B0E73`.
DATA(uuid_string_c32) = `429A229A88021EDFB7E2E25DF99B0E73`.

DATA(uuid_x16) = xco_cp_uuid=>format->c36->to_uuid( uuid_string_c36 )->value.
ASSERT uuid_x16 = uuid_string_c32.

DATA(uuid_c32) = CONV sysuuid_c32( xco_cp_uuid=>format->c32->from_uuid( xco_cp_uuid=>format->c36->to_uuid( uuid_string_c36 ) ) ).
ASSERT uuid_c32 = uuid_string_c32.

uuid_x16 = xco_cp_uuid=>format->c32->to_uuid( uuid_string_c32 )->value.
ASSERT uuid_x16 = uuid_string_c32.

DATA(uuid_c36) = CONV sysuuid_c36( xco_cp_uuid=>format->c36->from_uuid( xco_cp_uuid=>format->c32->to_uuid( uuid_string_c32 ) ) ).
ASSERT uuid_c36 = uuid_string_c36.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## XCO Representations of SY Components

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>XCO_CP</code> </td>
<td>

- Several `sy` components have XCO representations. 
- Find general information on XCO and more code snippets on the [SAP Help Portal](https://help.sap.com/docs/btp/sap-business-technology-platform/standard-library&version=Cloud&locale=en-US). 
- The following code snippet shows a selection of `sy` methods of the `XCO_CP` class. Some of the examples are also covered in other section of the cheat sheet.


```abap
"Currently, the following sy components are usable in ABAP for Cloud Development.
DATA(syindex) = sy-index.
DATA(sytabix) = sy-tabix.
DATA(sydbcnt) = sy-dbcnt.
DATA(syfdpos) = sy-fdpos.
DATA(sysubrc) = sy-subrc.
DATA(sylangu) = sy-langu.
DATA(sybatch) = sy-batch.
DATA(symandt) = sy-mandt.
DATA(sysysid) = sy-sysid.
DATA(syuname) = sy-uname.
DATA(symsgid) = sy-msgid.
DATA(symsgty) = sy-msgty.
DATA(symsgno) = sy-msgno.
DATA(symsgv1) = sy-msgv1.
DATA(symsgv2) = sy-msgv2.
DATA(symsgv3) = sy-msgv3.
DATA(symsgv4) = sy-msgv4.

"Others should not be used. For example, sy-datum.
"Some sy components have an XCO representation.
"The following example statements experiment with the API. To check out
"more methods and attributes, position the cursor after '->' or a closing
"parenthesis, and choose CTRL + Space.
"Note:
"- In many cases, importing parameters can be specified, e.g. to apply a
"  a specific format, etc..
"- Note the type when retrieving the value with the 'value' attribute.
"- Some excursions are included (i.e. extra functionality the API offers, such as
"  formatting options and other methods of xco_cp=>sy... that offer functionality
"  beyond sy components).
"- Some of the examples are also covered in other sections. More options are available
"  to handle the values. For example, you can perform date and time calculations with
"  the API.
"- Some of the examples use artifacts from the ABAP cheat sheet repository.

"Current user name
DATA(user) = xco_cp=>sy->user( )->name.

"Current date
"User time zone
"The example statements show different formats that can be applied. They can also be applied
"to other statements below, but they are not all covered.
"e.g. 2025-01-25
DATA(date_user_iso) = xco_cp=>sy->date( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->iso_8601_extended )->value.
"e.g. 20250125
DATA(date_user_iso_basic) = xco_cp=>sy->date( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->iso_8601_basic )->value.
DATA(date_user_abap) = xco_cp=>sy->date( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->abap )->value.
"UTC
DATA(date_utc_abap) = xco_cp=>sy->date( xco_cp_time=>time_zone->utc )->as( xco_cp_time=>format->abap )->value.

"Current time
"e.g. 09:23:40
DATA(date_time_iso) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->iso_8601_extended )->value.
DATA(date_time_abap) = xco_cp=>sy->time( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->abap )->value.

"Excursion: Time stamp
"e.g. 2025-01-25T09:27:26
DATA(moment_user_iso) = xco_cp=>sy->moment( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->iso_8601_extended )->value.
"e.g. 20250125092726
DATA(moment_user_abap) = xco_cp=>sy->moment( xco_cp_time=>time_zone->user )->as( xco_cp_time=>format->abap )->value.
DATA(moment_utc_iso) = xco_cp=>sy->moment( xco_cp_time=>time_zone->utc )->as( xco_cp_time=>format->iso_8601_extended )->value.
DATA(moment_utc_abap) = xco_cp=>sy->moment( xco_cp_time=>time_zone->utc )->as( xco_cp_time=>format->abap )->value.
"Excursion: Unix time stamp
DATA(unix_time_stamp_abap) = xco_cp=>sy->unix_timestamp( )->value.

"Current language
"e.g. E
DATA(lang_value) = xco_cp=>sy->language( )->value.
"e.g. English
DATA(lang_name) = xco_cp=>sy->language( )->get_name( ).
"e.g. EN
DATA(lang_iso_639) = xco_cp=>sy->language( )->as( xco_cp_language=>format->iso_639 ).

"Handling messages
"Apart from retrieving msgid, msgty, msgv1, msgv2, msgv3, and msgv4 values of message
"objects, the examples experiment with other methods available.

"Creating a message object
DATA(msg) = xco_cp=>sy->message( ).

"Using the 'string' and 'as_message' methods to create message object
"based on a random string, and using a default type, id, etc.
"In the example, the created message object is assigned to the previous one
"work with in the example.
msg = xco_cp=>string( `Test message` )->as_message( ).

"The following data object is of type symsg.
DATA(msg_value) = msg->value.

"Using a message class from the ABAP cheat sheet repository
msg = msg->overwrite( iv_msgty = 'E'
                      iv_msgid = 'ZDEMO_ABAP_MESSAGES'
                      iv_msgno = '5'
                      iv_msgv1 = 'Some'
                      iv_msgv2 = 'error'
                      iv_msgv3 = 'text'
                      iv_msgv4 = '' ).

DATA(msg_val) = msg->value.
DATA(msgid) = msg_val-msgid.
DATA(msgty) = msg_val-msgty.
DATA(msgv1) = msg_val-msgv1.
DATA(msgv2) = msg_val-msgv2.
DATA(msgv3) = msg_val-msgv3.
DATA(msgv4) = msg_val-msgv4.
"Some error text
DATA(msg_text_a) = msg->get_text( ).
"E
DATA(msg_type) = msg->get_type( )->value.

msg = msg->place_string( iv_string = `example`
                         iv_msgv1  = abap_false
                         iv_msgv2  = abap_false
                         iv_msgv3  = abap_false
                         iv_msgv4  = abap_true ).

"Some error text example
DATA(msg_text_b) = msg->get_text( ).
"example
DATA(msgv4_b) = msg->value-msgv4.

DATA(err_obj) = NEW zcx_demo_abap_error_b( ).
msg->write_to_t100_dyn_msg( err_obj ).

TRY.
    RAISE EXCEPTION err_obj.
  CATCH zcx_demo_abap_error_b INTO DATA(error).
    "Some error text example
    DATA(error_text_a) = error->get_text( ).
ENDTRY.

"Converting a random string into a structured message
"using the 'string' method
xco_cp=>string( `Hello world` )->as_message( )->write_to_t100_dyn_msg( err_obj ).

TRY.
    RAISE EXCEPTION err_obj.
  CATCH zcx_demo_abap_error_b INTO error.
    "Hello world
    DATA(error_text_b) = error->get_text( ).
ENDTRY.

msg->overwrite( iv_msgty = 'E'
                iv_msgid = 'ZDEMO_ABAP_MESSAGES'
                iv_msgno = '5'
                iv_msgv1 = 'Another'
                iv_msgv2 = 'example'
                iv_msgv3 = 'error'
                iv_msgv4 = 'text' )->write_to_t100_dyn_msg( err_obj ).

TRY.
    RAISE EXCEPTION err_obj.
  CATCH zcx_demo_abap_error_b INTO error.
    "Another example error text
    DATA(error_text_c) = error->get_text( ).
ENDTRY.
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
"Factories
DATA(bigint_int4) = cl_abap_bigint=>factory_from_int4( 10 ).
DATA(bigint_int4c) = cl_abap_bigint=>factory_from_string( `283469208407283452340` ).
DATA(bigint_int4d) = cl_abap_bigint=>factory_from_int8( CONV int8( 1234567890123456 ) ).

DATA(a1) = cl_abap_bigint=>factory_from_int4( -10 )->to_external( ).
DATA(a2) = cl_abap_bigint=>factory_from_int4( -10 )->to_external( iv_flg_minus_in_front = abap_true ).
DATA(a3) = cl_abap_bigint=>factory_from_int4( 100 )->to_utf8( ).
DATA(a4) = cl_abap_bigint=>factory_from_string( `123` )->to_df34( ).
DATA(a5) = cl_abap_bigint=>factory_from_int4( -10 )->to_string( ).
DATA(a6) = cl_abap_bigint=>factory_from_int4( 4 )->add( bigint_int4 )->to_string( ).
DATA(a7) = cl_abap_bigint=>factory_from_int4( 7 )->add_int4( 2 )->to_string( ).
DATA(a8) = cl_abap_bigint=>factory_from_int4( -10 )->abs( )->to_string( ).
DATA(a9) = cl_abap_bigint=>factory_from_int4( 19 )->compare_int4( 20 ).
DATA(a10) = cl_abap_bigint=>factory_from_int4( 100 )->compare( bigint_int4 ).
DATA(a11) = cl_abap_bigint=>factory_from_int4( 20 )->div( bigint_int4 ).
DATA(a12) = a11-quotient->to_string( ).
DATA(a13) = a11-remainder->to_string( ).
DATA(a14) = cl_abap_bigint=>factory_from_int4( 10 )->div_int4( 3 ).
DATA(a15) = a14-quotient->to_string( ).
DATA(a16) = a14-remainder.
DATA(a17) = cl_abap_bigint=>factory_from_int4( 10 )->div_by_two_power( CONV int8( 2 ) )->to_string( ).
DATA(a18) = cl_abap_bigint=>factory_from_int4( 5 )->div_to_df34( bigint_int4 ).
DATA(a19) = cl_abap_bigint=>factory_from_int4( 50 )->gcd( bigint_int4 )->to_string( ).
DATA(a20) = cl_abap_bigint=>factory_from_int4( 1000 )->get_number_of_bits( ).
DATA(a21) = cl_abap_bigint=>factory_from_int4( 10 )->is_equal( bigint_int4 ).

cl_abap_bigint=>factory_from_string( `123` )->is_int4(
  IMPORTING
    ev_int4_value  = DATA(a22)
  RECEIVING
    rv_flg_is_int4 = DATA(a23)
).
DATA(a24) = cl_abap_bigint=>factory_from_int4( 11 )->is_larger( bigint_int4 ).
DATA(a25) = cl_abap_bigint=>factory_from_int4( 10 )->is_larger_or_equal( bigint_int4 ).
DATA(a26) = cl_abap_bigint=>factory_from_int4( -10 )->is_negative( ).
DATA(a27) = cl_abap_bigint=>factory_from_int4( 0 )->is_zero( ).
DATA(a28) = cl_abap_bigint=>factory_from_int4( 123 )->mod( bigint_int4 )->to_string( ).
DATA(a29) = cl_abap_bigint=>factory_from_int4( 10 )->mod_int4( 3 ).
DATA(a30) = cl_abap_bigint=>factory_from_int4( 10 )->mul( bigint_int4 )->to_string( ).
DATA(a31) = cl_abap_bigint=>factory_from_int4( 5 )->mul_by_two_power( 2 )->to_string( ).
DATA(a32) = cl_abap_bigint=>factory_from_int4( 2 )->mul_int4( 5 )->to_string( ).
DATA(a33) = cl_abap_bigint=>factory_from_int4( 8 )->pow( 2 )->to_string( ).
DATA(a34) = cl_abap_bigint=>factory_from_int4( 9 )->sqrt( )->to_string( ).
DATA(a35) = cl_abap_bigint=>factory_from_int4( 18 )->sub( bigint_int4 )->to_string( ).
DATA(a36) = cl_abap_bigint=>factory_from_int4( 15 )->sub_int4( 9 )->to_string( ).
"Cloning
DATA(a37) = cl_abap_bigint=>factory_from_int4( 15 ).
DATA(a38) = cl_abap_bigint=>factory_from_int4( 5 ).
"Adding a number to another number to not get a new instance but the original instance
DATA(a39) = a37->add( a38 ).
ASSERT a39 = a37.
DATA(a40) = a37->to_string( ).
DATA(a41) = a39->to_string( ).
DATA(a42) = cl_abap_bigint=>factory_from_int4( 15 ).
DATA(a43) = cl_abap_bigint=>factory_from_int4( 5 ).

DATA(a44) = a42->clone( )->add( a43 ).
ASSERT a44 <> a42.
DATA(a45) = a42->to_string( ).
DATA(a46) = a44->to_string( ).
DATA(a47) = cl_abap_bigint=>factory_from_int4( 15 )->sub_int4( 9 )->clone( )->to_string( ).
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
DATA(rat_num) = cl_abap_rational=>factory_from_string( `-1/3` ).

"Performing an addition and converting the result to string
"7/6
DATA(addition_res) = rat_num->add( cl_abap_rational=>factory_from_string( `3/2` ) )->to_string( ). 

"Factories
DATA(r1) = cl_abap_rational=>factory_from_string( `-1/3` ).
DATA(r2) = cl_abap_rational=>factory_from_bigint( cl_abap_bigint=>factory_from_int4( 11 ) ).

TYPES p_l16d5 TYPE p LENGTH 16 DECIMALS 5.
DATA(r3) = cl_abap_rational=>factory_from_dec( CONV p_l16d5( '123456.789' ) ).
DATA(r4) = cl_abap_rational=>factory_from_decimal_string( `1.234567890` ).
DATA(r5) = cl_abap_rational=>factory_from_df34( `1.4` ).
DATA(r6) = cl_abap_rational=>factory_from_int4( 100 ).
DATA(r7) = cl_abap_rational=>factory_from_int8( CONV int8( 123 ) ).
DATA(r8) = cl_abap_rational=>factory_from_string( `-2/3` )->add_int4( 1 )->to_df34( ).
DATA(r9) = cl_abap_rational=>factory_from_string( `-2/3` )->add_int4( 1 )->to_string( ).
DATA r10 TYPE p_l16d5.
cl_abap_rational=>factory_from_string( `-2/3` )->add_int4( 1 )->to_dec( IMPORTING ev_decimal = r10 ).
"Methods that include cl_abap_bigint instances
DATA(r11) = cl_abap_rational=>factory_from_string( `-2/3` )->add_bigint( cl_abap_bigint=>factory_from_int4( 1 ) )->to_string( ).
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

<br>

The following example explores the generation of arbitrary numeric values. 
- It uses dynamic programming techniques. Find more information in the [Dynamic Programming](06_Dynamic_Programming.md) cheat sheet.
- The class names are constructed dynamically. They all begin with `CL_ABAP_RANDOM_`.
- An object is created dynamically based on the constructed class name.
- This object is assigned the result of a dynamic method call. The error handling is included as the `min` and `max` parameters are not available for all methods.
- The `get_next` method returns an appropriately typed data object, e.g. in case of `CL_ABAP_RANDOM_DECFLOAT34`, a data object of type `decfloat34` is returned. As a generic returning parameter is not possible, the example uses a data object of type `string`. So, the value returned is converted to type `string`. Note that are special conversion rules (e.g. the minus character for negative values are added at the end by default).
- The resulting string values are added to an internal table for display purposes.
- The example also includes static method calls.


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

    TYPES: BEGIN OF random_values,
             class        TYPE string,
             random_value TYPE string,
           END OF random_values.
    DATA random_value_table TYPE TABLE OF random_values WITH EMPTY KEY.

    DATA(cl_name_parts) = VALUE string_table( ( `DECFLOAT16` )
                                              ( `DECFLOAT34` )
                                              ( `FLOAT` )
                                              ( `INT` )
                                              ( `INT8` )
                                              ( `PACKED` )
                                              ( `PACKED_DEC` ) ).

    LOOP AT cl_name_parts INTO DATA(wa).

      IF wa CS `PACKED_DEC`.
        FIND PCRE `\d` IN wa.
        IF sy-subrc <> 0.
          DELETE cl_name_parts INDEX sy-tabix.
          DO 14 TIMES.
            APPEND wa && sy-index TO cl_name_parts.
          ENDDO.
          CONTINUE.
        ENDIF.
      ENDIF.

      DATA(cl_name) = `CL_ABAP_RANDOM_` && wa.
      DATA oref TYPE REF TO object.

      TRY.
          CALL METHOD (cl_name)=>create
            EXPORTING
              seed = cl_abap_random=>seed( )
              min  = 1
              max  = 1000
            RECEIVING
              prng = oref.
        CATCH cx_sy_dyn_call_param_not_found.
          CALL METHOD (cl_name)=>create
            EXPORTING
              seed = cl_abap_random=>seed( )
            RECEIVING
              prng = oref.
      ENDTRY.

      DATA value_conv2string TYPE string.

      CALL METHOD oref->('GET_NEXT') RECEIVING value = value_conv2string.

      APPEND VALUE #( class = cl_name random_value = value_conv2string ) TO random_value_table.
    ENDLOOP.

    out->write( random_value_table ).

    DATA(a) = cl_abap_random_decfloat16=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(b) = cl_abap_random_decfloat34=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(c) = cl_abap_random_float=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(d) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(e) = cl_abap_random_int8=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(f) = cl_abap_random_packed=>create( seed = cl_abap_random=>seed( ) )->get_next( ).    
    DATA(g) = cl_abap_random_packed_dec1=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(h) = cl_abap_random_packed_dec2=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(i) = cl_abap_random_packed_dec3=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(j) = cl_abap_random_packed_dec4=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(k) = cl_abap_random_packed_dec5=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(l) = cl_abap_random_packed_dec6=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(m) = cl_abap_random_packed_dec7=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(n) = cl_abap_random_packed_dec8=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(o) = cl_abap_random_packed_dec9=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(p) = cl_abap_random_packed_dec10=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(q) = cl_abap_random_packed_dec11=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(r) = cl_abap_random_packed_dec12=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(s) = cl_abap_random_packed_dec13=>create( seed = cl_abap_random=>seed( ) )->get_next( ).
    DATA(t) = cl_abap_random_packed_dec14=>create( seed = cl_abap_random=>seed( ) )->get_next( ).

  ENDMETHOD.

ENDCLASS.
```

</td>
</tr>
<tr>
<td> <code>CL_ABAP_PROB_DISTRIBUTION</code><br><code>CL_ABAP_PROB_DISTRIBUTION_DF34</code> </td>
<td>

- For generating random numbers from various probability distributions and performing probability calculations. 
- `CL_ABAP_PROB_DISTRIBUTION` calculates with float probabilities, `CL_ABAP_PROB_DISTRIBUTION_DF34` with decfloat34 probabilities.
- For more information, refer to the class documentation and [this blog](https://community.sap.com/t5/technology-blog-posts-by-sap/random-numbers-and-probability-distributions-in-the-abap-environments-for/ba-p/14173266).

<br>

``` abap
"The following code snippet shows the generation of many random numbers in one go.
"20 random numbers in the value range 1 - 100 are created.
DATA(random) = cl_abap_random=>create( seed = cl_abap_random=>seed( ) ).
DATA ranges_tab TYPE if_abap_prob_types=>int_range.
ranges_tab = VALUE #( ( sign = 'I' option = 'BT' low = 1 high = 100 ) ).
DATA(distribution) = cl_abap_prob_distribution=>get_uniform_int_distribution( range = ranges_tab ).
DATA(int_tab) = distribution->next_random_numbers( count = 20 rng = random ).

"An initial ranges tables means that all numbers are respected.
distribution = cl_abap_prob_distribution=>get_uniform_int_distribution( range = VALUE #( ) ).
int_tab = distribution->next_random_numbers( count = 20 rng = random ).

"The following example gets random, distinct integer values from a specified range and stores
"them in an internal table (integer table of type i). The example selects 10 distinct random 
"numbers from the range of 1 to 100.
TYPES ty_range TYPE if_abap_prob_types=>int_range.
DATA(demo_range) = VALUE ty_range( ( sign = 'I' option = 'BT' low = 1 high = 100 ) ).

DATA(nums) = REDUCE if_abap_prob_distribution_int=>random_numbers(
  LET let_rng = cl_abap_random=>create( cl_abap_random=>seed( ) ) IN
  INIT numbers = VALUE if_abap_prob_distribution_int=>random_numbers( )
        rng = demo_range
        number = 0
        dist = cl_abap_prob_distribution=>get_uniform_int_distribution( range = demo_range )
  FOR i = 1  UNTIL i > 10
  NEXT number = dist->next_random_number( let_rng )
        numbers = VALUE #( BASE numbers ( number ) )
        rng = VALUE #( BASE rng ( sign = 'E' option = 'EQ' low = number ) )
        dist =  cl_abap_prob_distribution=>get_uniform_int_distribution( range = rng ) ).
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

"--------- Matching string against regular expression ---------
DATA match TYPE string.

"yes
IF xco_cp=>string( ` 1` )->matches( `\s\d` ).
  match = `yes`.
ELSE.
  match = `no`.
ENDIF.

"no
IF xco_cp=>string( ` X` )->matches( `\s\d` ).
  match = `yes`.
ELSE.
  match = `no`.
ENDIF.
``` 

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Handling Codepages and Binary Data 

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_GZIP*</code> </td>
<td>
(De)compressing character strings and byte strings using GZIP: 
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
<td> <code>XCO_CP</code> </td>
<td> 
Converting strings to xstrings using a codepage using the XCO Library
<br><br>

``` abap
"536F6D6520737472696E67
DATA(xstr) = xco_cp=>string( `Some string` )->as_xstring( xco_cp_character=>code_page->utf_8 )->value.

"Some string
DATA(str) = xco_cp=>xstring( xstr )->as_string( xco_cp_character=>code_page->utf_8 )->value.
``` 

</td>
</tr>

<tr>
<td> <code>XCO_CP</code> </td>
<td> 
Base64 decoding/encoding using XCO
<br><br>

``` abap
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
``` 

</td>
</tr>

<tr>
<td> <code>CL_WEB_HTTP_UTILITY</code> </td>
<td> 
Endcoding strings/xstrings in Base64 and decoding Base64-encoded strings/xstrings
<br><br>

``` abap
DATA(hi) = `Hello world`.

"Encoding a string in BASE64
"Result is of type string
"SGVsbG8gd29ybGQ=
DATA(encode_base64_str) = cl_web_http_utility=>encode_base64( unencoded = hi ).

"Decoding a base64-encoded String
"Result is of type string
"Hello world
DATA(decode_base64_str) = cl_web_http_utility=>decode_base64( encoded = encode_base64_str ).

**********************************************************************

"string -> xstring
"48656C6C6F20776F726C64
DATA(conv_xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( hi ).

"Encoding an xstring in BASE64
"Result is of type string
"SGVsbG8gd29ybGQ=
DATA(encode_base64_xstr) = cl_web_http_utility=>encode_x_base64( unencoded = conv_xstring ).

"Decoding a base64-encoded xstring
"48656C6C6F20776F726C64
DATA(decode_base64_xstr) = cl_web_http_utility=>decode_x_base64( encoded = encode_base64_xstr ).

"xstring -> string
"Hello world
DATA(conv_string) = cl_abap_conv_codepage=>create_in( )->convert( decode_base64_xstr ).
``` 

</td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>



## Regular Expressions 

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
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
"Example string
DATA(str) = `a1 # B2 ? cd . E3`.

"----------- Creating an instance of a regular expression -----------

"Creating an instance of a regular expression with PCRE syntax
"using cl_abap_regex

"Example pattern: Any-non digit followed by a digit
DATA(regex) = cl_abap_regex=>create_pcre( pattern = `\D\d`
                                          ignore_case = abap_true ).

"----------- Creating matchers -----------

"Two ways are possible (both ways return references of type
"ref to cl_abap_matcher):
"- create_matcher method of the cl_abap_regex class
"- create_pcre method of the cl_abap_matcher class
"Note that several importing parameters are available to enable
"further settings of the regular expression, e.g. ignoring the
"case, using the extended mode, etc. The example pass a string
"to the 'text' parameter. You can also specify internal tables
"with the 'table' parameter and more.

"Creating a matcher using the create_matcher method of the cl_abap_regex class
DATA(matcher_1) = regex->create_matcher( text = str ).
"Creating a matcher in one go using method chaining
DATA(matcher_2) = cl_abap_regex=>create_pcre( pattern = `\D\d`
                                              ignore_case = abap_true
                                            )->create_matcher( text = str ).

"Creating a matcher using the create_pcre method of the cl_abap_matcher class
DATA(matcher_3) = cl_abap_matcher=>create_pcre( pattern = `\D\d`
                                                text    = str
                                                ignore_case = abap_true ).

"----------- Exploring searching and replacing -----------

"--- Finding all occurrences using the find_all method ---
"In the example, result has the type match_result_tab containing the findings.
DATA(result_fa1) = matcher_1->find_all( ).

*LINE    OFFSET    LENGTH    SUBMATCHES
*0       0         2         OFFSET     LENGTH
*
*0       5         2         OFFSET     LENGTH
*
*0       15        2         OFFSET     LENGTH

DATA(result_fa2) = matcher_2->find_all( ).
ASSERT result_fa2 = result_fa1.

"Getting the result in one go using method chaining with cl_abap_matcher
DATA(result_fa3) = cl_abap_matcher=>create_pcre( pattern = `\D\d`
                                                text    = str
                                                ignore_case = abap_true
                                              )->find_all( ).
ASSERT result_fa3 = result_fa1.

"--- Example with submatches ---

str = `XabcdXXefgXXhXXijklmnXX`.

DATA(result_fa4) = cl_abap_matcher=>create_pcre( pattern = `X(.*?)X`
                                                 text    = str
                                                 ignore_case = abap_true
                                               )->find_all( ).

*LINE    OFFSET    LENGTH    SUBMATCHES
*0       0         6         OFFSET    LENGTH
*                            1         4
*0       6         5         OFFSET    LENGTH
*                            7         3
*0       11        3         OFFSET    LENGTH
*                            12        1
*0       14        8         OFFSET    LENGTH
*                            15        6

"--- Replacing all occurrences using the 'replace_all' method ---

DATA(matcher_repl_1) = cl_abap_regex=>create_pcre( pattern = `X(.*?)X`
                                                 )->create_matcher( text = str ).

"4
DATA(repl_count_1) = matcher_repl_1->replace_all( newtext = `#$1#` ).

"#abcd##efg##h##ijklmn#X
DATA(repl_result_1) = matcher_repl_1->text.

"Using cl_abap_matcher
DATA(matcher_repl_2) = cl_abap_matcher=>create_pcre( pattern = `X(.*?)X`
                                                     text    = str ).
DATA(repl_count_2) = matcher_repl_2->replace_all( newtext = `#$1#` ).
DATA(repl_result_2) = matcher_repl_2->text.

"---- Sequential processing of the regular expression ---
"---- using the find_next method ------------------------
"The example explores various other methods, and writes
"information to a string table.

str = `a1bc2def3ghij45klm67opqr8stuvwx90yz`.

DATA(matcher_fn) = cl_abap_matcher=>create_pcre( pattern = `\d(\D.)`
                                                 text    = str ).

DATA strtab TYPE string_table.
WHILE matcher_fn->find_next( ) = abap_true.
  APPEND |---- Finding { sy-index } -----| TO strtab.

  "Type match_result
  DATA(match_result) = matcher_fn->get_match( ).

  DATA(offset) = matcher_fn->get_offset( ).
  DATA(length) = matcher_fn->get_length( ).
  DATA(matched_content) = str+offset(length).

  APPEND |Match offset: { offset }| TO strtab.
  APPEND |Match length: { length }| TO strtab.
  APPEND |Match content: { matched_content }| TO strtab.

  "Type match_result
  DATA(subgroup) = matcher_fn->get_match( )-submatches.

  LOOP AT subgroup INTO DATA(wa).
    DATA(sub_tabix) = sy-tabix.
    DATA(submatch_line) = wa.
    DATA(submatch_offset) = wa-offset.
    DATA(submatch_length) = wa-length.
    DATA(submatch) = matcher_fn->get_submatch( sub_tabix ).
    APPEND |Submatch { sub_tabix } offset: { submatch_offset }| TO strtab.
    APPEND |Submatch { sub_tabix } length: { submatch_length }| TO strtab.
    APPEND |Submatch { sub_tabix } content: { submatch }| TO strtab.
  ENDLOOP.

ENDWHILE.

"---- Using an object of type cl_abap_regex in ABAP ---
"---- statements with the REGEX addition --------------

DATA(result_find_all_1) = cl_abap_matcher=>create_pcre( pattern = `\d(\D.)`
                                                        text = str
                                                      )->find_all( ).
DATA(result_find_all_2) = cl_abap_regex=>create_pcre( pattern = `\d(\D.)`
                                           )->create_matcher( text = str
                                           )->find_all( ).

DATA(reg_expr) = cl_abap_regex=>create_pcre( pattern = `\d(\D.)` ).

FIND ALL OCCURRENCES OF REGEX reg_expr IN str RESULTS DATA(result_find_all_3).

*LINE    OFFSET    LENGTH    SUBMATCHES
*0       1         3         OFFSET    LENGTH
*                            2         2
*0       4         3         OFFSET    LENGTH
*                            5         2
*0       8         3         OFFSET    LENGTH
*                            9         2
*0       14        3         OFFSET    LENGTH
*                            15        2
*0       19        3         OFFSET    LENGTH
*                            20        2
*0       24        3         OFFSET    LENGTH
*                            25        2
*0       32        3         OFFSET    LENGTH
*                            33        2

ASSERT result_find_all_3 = result_find_all_1.
ASSERT result_find_all_3 = result_find_all_2.

"Note that the REGEX addition is obsolete when using (POSIX) syntax patterns
"A syntax warning is displayed for the following example.
"FIND ALL OCCURRENCES OF REGEX `\d(\D.)` IN str RESULTS DATA(result_8).

"The syntax warning can be suppressed using a pragma
FIND ALL OCCURRENCES OF REGEX `\d(\D.)` IN str RESULTS DATA(result_find_all_4) ##REGEX_POSIX.

"Using PCRE instead
FIND ALL OCCURRENCES OF PCRE `\d(\D.)` IN str RESULTS DATA(result_find_all_5).
ASSERT result_find_all_5 = result_find_all_3.

"---------------- Exploring more parameters of the create_pcre method ----------------
"See the class documentation for more parameters and information.

"--- enable_multiline parameter ---

str = |abc\ndef\nghi\njkl|.

DATA(matcher_no_ml) = cl_abap_matcher=>create_pcre( pattern = `^`
                                                    text    = str ).
"1
DATA(repl_count_no_ml) = matcher_no_ml->replace_all( newtext = `#` ).
"|#abc\ndef\nghi\njkl|
DATA(repl_result_no_ml) = matcher_no_ml->text.

DATA(matcher_w_ml) = cl_abap_matcher=>create_pcre( pattern = `^`
                                                   text    = str
                                                   enable_multiline = abap_true ).
"4
DATA(repl_count_w_ml) = matcher_w_ml->replace_all( newtext = `#` ).
"|#abc\n#def\n#ghi\n#jkl|
DATA(repl_result_w_ml) = matcher_w_ml->text.

"--- table/ignore_case parameters ---

data(str_table) = VALUE string_table( ( `abZdez` ) ( `zZfghZ` ) ( `ijkZZz` ) ( `zzzzZ` ) ).

DATA(matcher_tab) = cl_abap_matcher=>create_pcre( pattern = `z+`
                                                  table   = str_table
                                                  ignore_case = abap_true ).
"6
DATA(repl_count_tab) = matcher_tab->replace_all( newtext = `#` ).
"ab#de# / #fgh# / ijk# / #
DATA(repl_result_tab) = matcher_tab->table.

"--- extended parameter ---

str = `abc def`.

DATA(matcher_w_extended) = cl_abap_matcher=>create_pcre( pattern = `abc def`
                                                         text    = str ).

"No replacement in the following example as the extended mode is
"enabled by default.
"0
DATA(repl_count_w_extended) = matcher_w_extended->replace_all( newtext = `#` ).
"abc def
DATA(repl_result_w_extended) = matcher_w_extended->text.

"Disabling the extended mode so that whitespaces are not ignored
DATA(matcher_not_extended) = cl_abap_matcher=>create_pcre( pattern = `abc def`
                                                           text    = str
                                                           extended = abap_false ).

"1
DATA(repl_count_not_extended) = matcher_not_extended->replace_all( newtext = `#` ).
"#
DATA(repl_result_not_extended) = matcher_not_extended->text.
```

</td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Time and Date

> [!NOTE] 
> In [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm), do not use the date and time-related system fields such as `sy-datum` and `sy-uzeit`, and others. User-related time and date values can be retrieved using the XCO library. For code snippets, see the [Date, Time, and Time Stamp](23_Date_and_Time.md) cheat sheet.

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
"Getting the current date in UTC (not the system or user time), e.g. 20240101
DATA(sys_date) = cl_abap_context_info=>get_system_date( ).

"Getting the current time in UTC, e.g. 152450
DATA(sys_time) = cl_abap_context_info=>get_system_time( ).
``` 

</td>
</tr>
<tr>
<td> <code>CL_ABAP_DATFM</code> </td>
<td>
For conversions between the external and the internal representation of a date specification

<br><br>

``` abap
DATA(date4conversion) = CONV d( '20240202' ).
DATA conv_date_str TYPE string.
DATA conv_date_d TYPE d.
DATA date_format TYPE cl_abap_datfm=>ty_datfm.

"Conversion of d (internal) to string (external time format)
TRY.
    cl_abap_datfm=>conv_date_int_to_ext(
      EXPORTING im_datint    = date4conversion
                im_datfmdes  = cl_abap_datfm=>get_datfm( )
      IMPORTING ex_datext    = conv_date_str
                ex_datfmused = date_format ).
  CATCH cx_abap_datfm_format_unknown.
ENDTRY.

"Conversion of string (external) to d (internal time format)
TRY.
    cl_abap_datfm=>conv_date_ext_to_int(
      EXPORTING im_datext    = conv_date_str
                im_datfmdes  = date_format
      IMPORTING ex_datint    = conv_date_d
                ex_datfmused = date_format ).
  CATCH cx_abap_datfm_no_date cx_abap_datfm_invalid_date
        cx_abap_datfm_format_unknown cx_abap_datfm_ambiguous.
ENDTRY.
``` 

</td>
</tr>

<tr>
<td> <code>CL_ABAP_TIMEFM</code> </td>
<td>
For conversions between the external and the internal representation of a time specification

<br><br>

``` abap
DATA(time4conversion) = CONV t( '123456' ).
DATA conv_time_str TYPE string.
DATA conv_time_t TYPE t.

"Conversion of t (internal) to string (external time format)
TRY.
    cl_abap_timefm=>conv_time_int_to_ext(
      EXPORTING time_int            = time4conversion
                without_seconds     = abap_false
                format_according_to = cl_abap_timefm=>iso
      IMPORTING time_ext            = conv_time_str ).
  CATCH cx_parameter_invalid_range.
ENDTRY.

"Conversion of string (external) to t (internal time format)
TRY.
    cl_abap_timefm=>conv_time_ext_to_int(
      EXPORTING time_ext            = conv_time_str
      IMPORTING time_int            = conv_time_t ).
  CATCH cx_abap_timefm_invalid.
ENDTRY.
``` 

</td>
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
<td> <code>XCO_CP_TIME</code><br><code>XCO_CP</code> </td>
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

## Calendar-Related Information

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_FHC_CALENDAR_RUNTIME</code> </td>
<td>

The following example explores accessing calendar-related information (factory and holiday calendars). Find more information in the [SAP Help Portal documentation](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/f7cbd3c336f84dc09c85639c55b4309f.html?version=Cloud). Note the [released CDS views](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/cc36b142349f40c499155b65e812c3ac.html?version=Cloud) in that context.


``` abap
SELECT FactoryCalendarID
  FROM I_FactoryCalendarBasic
  ORDER BY FactoryCalendarID
  INTO TABLE @DATA(factory_cal_ids).

SELECT PublicHolidayCalendarID
  FROM I_PublicHolidayCalendarBasic
  ORDER BY PublicHolidayCalendarID
  INTO TABLE @DATA(public_holiday_cal_ids).

DATA(example_cal_ids) = VALUE string_table( ( `SAP_US` ) ( `SAP_IN` ) ( `SAP_QA` )
                                            ( `SAP_SA` ) ( `SAP_DE_BW` ) ).

LOOP AT example_cal_ids INTO DATA(example_cal_id).

  IF line_exists( factory_cal_ids[ table_line = example_cal_id ] ).
    DATA(factory_calendar_id) = CONV cl_fhc_calendar_runtime=>ty_fcal_id( example_cal_id ).
  ELSE.
    CLEAR factory_calendar_id.
  ENDIF.

  IF line_exists( public_holiday_cal_ids[ table_line = example_cal_id ] ).
    DATA(holiday_calendar_id) = CONV cl_fhc_calendar_runtime=>ty_hcal_id( example_cal_id ).
  ELSE.
    CLEAR holiday_calendar_id.
  ENDIF.

  "---------------------- Factory calendar-related information ----------------------
  TRY.
      DATA(factory_cal) = cl_fhc_calendar_runtime=>create_factorycalendar_runtime( iv_factorycalendar_id = factory_calendar_id ).
      DATA(fc_date_conv) = factory_cal->convert_date_to_factorydate( CONV d( '20241115' ) ).
      DATA(fc_factory_date_conv) = factory_cal->convert_factorydate_to_date( 7219 ).
      DATA(fc_last_factory_date) = factory_cal->get_last_factorydate( ).
      DATA(fc_days_between) = factory_cal->calc_workingdays_between_dates( iv_start = '20241201'
                                                                            iv_end = '20250101'  ).
      DATA(fc_days_add) = factory_cal->add_workingdays_to_date( iv_start = '20241220'
                                                                iv_number_of_workingdays  = 5  ).
      DATA(fc_days_subtract) = factory_cal->subtract_workingdays_from_date( iv_start = '20250101'
                                                                            iv_number_of_workingdays  = 5  ).
      DATA(fc_is_working_date_1) = factory_cal->is_date_workingday( '20250101' ).
      DATA(fc_is_working_date_2) = factory_cal->is_date_workingday( '20241231' ).
      DATA(fc_description) = factory_cal->get_description( ).
      DATA(fc_id) = factory_cal->get_id( ).
    CATCH cx_fhc_runtime INTO DATA(error_factory_cal).
      DATA(error_msg_factory_cal) = error_factory_cal->get_text( ).
  ENDTRY.

  "---------------------- Holiday calendar-related information ----------------------
  TRY.
      DATA(holiday_cal) = cl_fhc_calendar_runtime=>create_holidaycalendar_runtime( iv_holidaycalendar_id = holiday_calendar_id ).
      DATA(hc_is_holiday) = holiday_cal->is_holiday( CONV d( '20250101' ) ).
      holiday_cal->calc_holidays_between_dates(
        EXPORTING
          iv_start = '20240101'
          iv_end = '20250101'
        IMPORTING
          et_holidays = DATA(hc_holidays_between)
      ).
      DATA(hc_val_start) = holiday_cal->get_validity_start( ).
      DATA(hc_val_end) = holiday_cal->get_validity_end( ).
      DATA(hc_description) = holiday_cal->get_description( ).
      DATA(hc_id) = holiday_cal->get_id( ).
      DATA(hc_holiday_assignm) = holiday_cal->get_holiday_assignments( ).

      DATA holidays TYPE string_table.
      LOOP AT hc_holiday_assignm INTO DATA(holiday_wa).
        APPEND |Holiday ID: "{ holiday_wa->get_holiday_id( ) }", "{ holiday_wa->get_text( )-description }"| TO holidays.
      ENDLOOP.

      DATA holidays_in_time_frame TYPE string_table.
      LOOP AT hc_holidays_between INTO DATA(holiday_info_wa).
        DATA(hc_get_holiday) = holiday_cal->get_holiday( holiday_info_wa-date ).
        DATA(hc_holiday_text) = hc_get_holiday->get_text( ).
        DATA(hc_holiday_class) = hc_get_holiday->get_class( ).
        DATA(hc_holiday_conf) = hc_get_holiday->get_confession( ).
        DATA(hc_holiday_type) = hc_get_holiday->get_type( ).
        DATA(hc_holiday_id) = hc_get_holiday->get_holiday_id( ).
        APPEND |Date "{ holiday_info_wa-date }", Title "{ hc_holiday_text-description }"| TO holidays_in_time_frame.
      ENDLOOP.
    CATCH cx_fhc_runtime INTO DATA(error_holiday_cal).
      DATA(error_msg_holidays_cal) = error_holiday_cal->get_text( ).
  ENDTRY.
  CLEAR: holidays, holidays_in_time_frame.
ENDLOOP.
``` 

</td>
</tr>
<tr>
<td> <code>CL_SCAL_UTILS</code> </td>
<td>
Calendar utilities for getting month names, year and week of a date, first day of a week, name and number of the weekday for a specified date


``` abap
TRY.
  "Getting month names
  cl_scal_utils=>month_names_get(
    EXPORTING
      iv_language    = sy-langu
    IMPORTING
      et_month_names = DATA(months)
      ev_returncode  = DATA(return_code)
  ).

  DATA(month_names) = VALUE string_table( FOR wa IN months ( CONV string( wa-ltx ) ) ).

  "Getting year and week of a date
  cl_scal_utils=>date_get_week(
    EXPORTING
      iv_date = '20251201'
    IMPORTING
      ev_year = DATA(year)
      ev_week = DATA(week) ).

  "Getting the first day of a week
  "Note the class documentation
  cl_scal_utils=>week_get_first_day(
    EXPORTING
      iv_year_week = 0
      iv_year      = 2025
      iv_week      = 48
    IMPORTING
      ev_date      = DATA(date)
  ).

  "Getting the name and number of the weekday for a specified date
  cl_scal_utils=>date_compute_day(
    EXPORTING
      iv_date           = '20251201'
    IMPORTING
      ev_weekday_number = DATA(day_number)
      ev_weekday_name   = DATA(day_name)
  ).

CATCH cx_scal INTO DATA(error_scal).
  DATA(error_msg_scal) = error_scal->get_text( ).
ENDTRY.
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
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
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

## Getting the Current User Name

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
"User alias, e.g. XY0000001234
DATA(alias) = cl_abap_context_info=>get_user_alias( ).

"Formatted name, e.g. John Doe
TRY.
    DATA(formatted_name) = cl_abap_context_info=>get_user_formatted_name( ).
  CATCH cx_abap_context_info_error.
ENDTRY.

"The class also provides the option to retrieve the current date and time 
"in UTC.
"Getting the current date in UTC (not the system or user time), e.g. 20240101
DATA(sys_date) = cl_abap_context_info=>get_system_date( ).

"Getting the current time in UTC, e.g. 152450
DATA(sys_time) = cl_abap_context_info=>get_system_time( ).
``` 

</td>
</tr>

<tr>
<td> <code>XCO_CP</code> </td>
<td>

``` abap
DATA(user_w_xco) = xco_cp=>sy->user( )->name.
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
<td> <code>XCO_CP_ABAP</code><br><code>XCO_CP_ABAP_REPOSITORY</code><br><code>XCO_CP_ABAP_DICTIONARY</code> </td>
<td>

<ul>
<li>Provides access to abstractions for ABAP Dictionary objects such as database tables, data elements, types and more.</li>
<li>For more detailed examples, refer to the <a href="https://help.sap.com/docs/btp/sap-business-technology-platform/overview-of-xco-modules">SAP Help Portal</a>. See also the executable example of the <a href="19_ABAP_for_Cloud_Development.md">ABAP for Cloud Development</a> cheat sheet.</li>
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

"Checking if a repository object with a specific name exists in the system
DATA(type_names) = VALUE string_table( ( `ZDEMO_ABAP_FLI` ) ( `ZDEMO_ABAP_FLSCH_VE` ) ).
LOOP AT type_names INTO DATA(type_name).
  DATA(filter5) = xco_cp_abap_repository=>object_name->get_filter( xco_cp_abap_sql=>constraint->equal( type_name ) ).
  "A table is returned containing found repository objects of the specified name
  DATA(repo_objects) = xco_cp_abap_repository=>objects->where( VALUE #( ( filter5 ) ) )->in( xco_cp_abap=>repository )->get( ).

  "Some examples for further processing the returned objects
  "You can explore more options, e.g., by adding the object component selector (->)
  "to the final parenthesis and checking the suggestions by ADT.
  LOOP AT repo_objects INTO DATA(obj).
    "Retrieving the four-character value of the object type
    DATA(val) = obj->type->value.
    "Retrieving the package name of the repository object
    DATA(package) = obj->get_package( )->name.

    "The following example implementation retrieves key component.
    CASE val.
      WHEN `TABL`.
        "Retrieving the key component names of DDIC database tables
        DATA(table_keys) = xco_cp_abap_repository=>object->tabl->database_table->for( CONV #( obj->name->value ) )->fields->key->get_names( ).
      WHEN `DDLS`.
        "Retrieving the key component names of CDS entities
        DATA(ddls_key_spec) = xco_cp_abap_repository=>object->ddls->for( CONV #( obj->name->value ) )->entity( )->fields->all->get_names( ).
        LOOP AT ddls_key_spec INTO DATA(field).
          DATA(is_key) = xco_cp_abap_repository=>object->ddls->for( CONV #( obj->name->value ) )->view_entity( )->field( field )->content( )->get_key_indicator( ).
          DATA ddls_keys TYPE string_table.
          IF is_key IS NOT INITIAL.
            APPEND field TO ddls_keys.
          ENDIF.
        ENDLOOP.
        CLEAR ddls_keys.
    ENDCASE.
  ENDLOOP.
ENDLOOP.

"The following code snippets demonstrate getting information about some
"of the technical properties of different ABAP repository objects.
"Using the handlers, you can explore more options, e.g., by adding the object
"component selector (->) to the final parenthesis and checking the suggestions
"by ADT. The options shown are not comprehensive. They show a selection.
"The same information may be accessed in different ways via the various
"methods available.

*&---------------------------------------------------------------------*
*& Database table
*&---------------------------------------------------------------------*

DATA(handler_tabl) = xco_cp_abap_dictionary=>database_table( 'ZDEMO_ABAP_CARR' ).
DATA(dbtab_exists) = handler_tabl->exists( ).
DATA(dbtab_name) = handler_tabl->name.
DATA(dbtab_syntax_check_msg) = handler_tabl->check_syntax( )->messages.
DATA(dbtab_content) = handler_tabl->content( ).
"Technical information
DATA(dbtab_short_description) = dbtab_content->get_short_description( ).
DATA(dbtab_delivery_class) = dbtab_content->get_delivery_class( )->value.
DATA(dbtab_technical_settings) = dbtab_content->get_technical_settings( ).
DATA(dbtab_data_maintenance) = dbtab_content->get_data_maintenance( )->value.
DATA(dbtab_enhancement_category) = dbtab_content->get_enhancement_category( )->value.
DATA(dbtab_includes) = dbtab_content->get_includes( ).
"Same information accessed in other ways
DATA(dbtab_get_content) = dbtab_content->get( ).
DATA(dbtab_data_maint_alt) = dbtab_get_content-data_maintenance->value.
DATA(dbtab_delivery_cl_alt) = dbtab_get_content-delivery_class->value.
DATA(dbtab_enh_cat_alt) = dbtab_get_content-enhancement_category->value.
DATA(dbtab_includes_alt) = dbtab_get_content-includes.
"Table fields and keys
DATA(dbtab_field_names) = handler_tabl->fields->all->get_names( ).
DATA(dbtab_fields_built_in_types) = handler_tabl->fields->all->content( )->get_underlying_built_in_types( ).
DATA(dbtab_keys) = handler_tabl->fields->key->get_names( ).
DATA(dbtab_included_fields) = handler_tabl->fields->included->get_names( ).
"Release state and visibilities
DATA(dbtab_visibilities) = handler_tabl->get_api_state( )->get_visibilities( ).
DATA(dbtab_release_state) = handler_tabl->get_api_state( )->get_release_state( )->value.


*&---------------------------------------------------------------------*
*& Data element
*&---------------------------------------------------------------------*

DATA(handler_dtel) = xco_cp_abap_dictionary=>data_element( 'ABAP_BOOLEAN' ).
DATA(dtel_exists) = handler_dtel->exists( ).
DATA(dtel_name) = handler_dtel->name.
DATA(dtel_syntax_check_msg) = handler_dtel->check_syntax( )->messages.
DATA(dtel_content) = handler_dtel->content( ).
"Labels
DATA(dtel_heading_label) = dtel_content->get_heading_field_label( ).
DATA(dtel_short_label) = dtel_content->get_short_field_label( ).
DATA(dtel_long_label) = dtel_content->get_long_field_label( ).
DATA(dtel_medium_label) = dtel_content->get_medium_field_label( ).
DATA(dtel_description) = dtel_content->get_short_description( ).
"Type information
DATA(dtel_has_built_in_type) = dtel_content->has_underlying_built_in_type( ).
IF dtel_has_built_in_type = abap_true.
  DATA(dtel_built_in_type) = dtel_content->get_underlying_built_in_type( ).
  DATA(dtel_built_in_type_length) = dtel_built_in_type->length.
  DATA(dtel_buift_in_type_type) = dtel_built_in_type->type.
  DATA(dtel_type_descriptor) = dtel_built_in_type->abap_type->get_type_descriptor( ).
  DATA(dtel_built_in_type_abs_name) = dtel_built_in_type->abap_type->get_type_descriptor( )->absolute_name.
  DATA(dtel_built_in_type_kind) = dtel_built_in_type->abap_type->get_type_descriptor( )->kind.
ENDIF.
"Domain information
DATA(dtel_domain) = dtel_content->get_data_type( )->get_domain( ).
IF dtel_domain IS NOT INITIAL.
  DATA(dtel_domain_name) = dtel_domain->name.
  DATA(dtel_domain_content) = dtel_domain->content( ).
  DATA(dtel_domain_built_in_type) = dtel_domain_content->get_format( )->get_built_in_type( ).
  DATA(dtel_domain_output_ch) = dtel_domain_content->get_output_characteristics( ).
  DATA(dtel_domain_description) = dtel_domain_content->get_short_description( ).
  DATA(dtel_domain_fixed_values) = dtel_domain->fixed_values->all->get( ).
ENDIF.
"Release state and visibilities
DATA(dtel_release_state) = handler_dtel->get_api_state( )->get_release_state( )->value.
DATA(dtel_visibilities) = handler_dtel->get_api_state( )->get_visibilities( ).

*&---------------------------------------------------------------------*
*& Table type
*&---------------------------------------------------------------------*

DATA(handler_table_type) = xco_cp_abap_dictionary=>table_type( 'ABP_BEHV_RESPONSE_TAB' ).
DATA(table_type_exists) = handler_table_type->exists( ).
DATA(table_type_name) = handler_table_type->name.
DATA(table_type_syntax_check_msg) = handler_table_type->check_syntax( )->messages.
DATA(table_type_content) = handler_table_type->content( ).
DATA(table_type_access) = table_type_content->get_access( )->value.
"Key information
DATA(table_type_primary_key) = table_type_content->get_primary_key( ).
DATA(table_type_key_category) = table_type_primary_key->key_category->value.
DATA(table_type_key_components) = table_type_primary_key->key_components.
DATA(table_type_key_definition) = table_type_primary_key->key_definition->value.
DATA(table_type_secondary_keys) = table_type_content->get_secondary_keys( ).
DATA(table_type_description) = table_type_content->get_short_description( ).
"Row type information
DATA(table_type_row_type) = table_type_content->get_row_type( ).
DATA(table_type_structure) = table_type_row_type->get_structure( ).
DATA(table_type_struct_name) = table_type_structure->name.
DATA(table_type_struct_comp_names) = table_type_structure->components->all->get_names( ).
DATA(table_type_struct_comps) = table_type_structure->components->all->get( ).
DATA(table_type_content_get_alt) = table_type_content->get( ).
DATA(table_type_pr_key_alt) = table_type_content_get_alt-primary_key->key_components.
DATA(table_type_sec_key_alt) = table_type_content_get_alt-secondary_keys.
"Release state and visibilities
DATA(table_type_release_state) = handler_table_type->get_api_state( )->get_release_state( )->value.
DATA(table_type_visibilities) = handler_table_type->get_api_state( )->get_visibilities( ).

*&---------------------------------------------------------------------*
*& CDS view entity
*&---------------------------------------------------------------------*

DATA(handler_cds_ve) = xco_cp_cds=>view_entity( 'ZDEMO_ABAP_RAP_RO_M' ).
DATA(cds_ve_exists) = handler_cds_ve->exists( ).
DATA(cds_ve_name) = handler_cds_ve->name.
DATA(cds_ve_associations) = handler_cds_ve->associations->all->get( ).
DATA(cds_ve_compositions) = handler_cds_ve->compositions->all->get( ).
DATA(cds_ve_field_names) = handler_cds_ve->fields->all->get_names( ).
"Field information
DATA(cds_ve_fields) = handler_cds_ve->fields->all->get( ).
LOOP AT cds_ve_fields INTO DATA(ve).
  DATA(cds_ve_f_content) = ve->content( ).
  DATA(cds_ve_f_alias) = cds_ve_f_content->get_alias( ).
  DATA(cds_ve_f_assoc) = cds_ve_f_content->get_association( ).
  DATA(cds_ve_f_comp) = cds_ve_f_content->get_composition( ).
  DATA(cds_ve_f_key_indicator) = cds_ve_f_content->get_key_indicator( ).
ENDLOOP.

*&---------------------------------------------------------------------*
*& Class
*&---------------------------------------------------------------------*

DATA(handler_cl) = xco_cp_abap=>class( 'CL_ABAP_TYPEDESCR' ).
DATA(cl_exists) = handler_cl->exists( ).
DATA(cl_name) = handler_cl->name.
DATA(cl_syntax_check_msg) = handler_cl->check_syntax( )->messages.
"Creation and change information
DATA(cl_created_by) = handler_cl->get_created_by( ).
DATA(cl_created_on) = handler_cl->get_created_on( ).
DATA(cl_last_changed_by) = handler_cl->get_last_changed_by( ).
DATA(cl_last_changed_on) = handler_cl->get_last_changed_on( ).
"Subclasses
DATA(cl_subclasses) = handler_cl->subclasses->all->get_names( ).
"Information on the definition part
DATA(cl_definition) = handler_cl->definition->content( ).
DATA(cl_definition_get) = cl_definition->get( ).
DATA(cl_final) = cl_definition->get_final_indicator( ).
DATA(cl_abstract) = cl_definition->get_abstract_indicator( ).
DATA(cl_friends) = cl_definition->get_global_friends( ).
DATA(cl_interfaces) = cl_definition->get_interfaces( ).
DATA(cl_definition_visibility) = cl_definition->get_visibility( )->value.
"Superclass
DATA(cl_superclass) = cl_definition->get_superclass( ).
IF cl_superclass IS NOT INITIAL.
  DATA(cl_superclass_name) = cl_superclass->name.
  DATA(cl_superclass_content) = cl_superclass->content( ).
ENDIF.
"Associated behavior definition
DATA(cl_for_behv) = cl_definition->get_for_behavior_of( ).
IF cl_for_behv IS NOT INITIAL.
  DATA(cl_for_behv_name) = cl_for_behv->name.
  DATA(cl_for_behv_content) = cl_for_behv->content( ).
ENDIF.
"Information on components in the visibility sections
DATA(cl_private_components) = handler_cl->definition->section-private->components.
DATA(cl_protected_components) = handler_cl->definition->section-protected->components.
DATA(cl_public_components) = handler_cl->definition->section-public->components.
"Component information
DATA(cl_all_public_inst_methods) = cl_public_components->method->all->get( ).
DATA(cl_all_public_stat_methods) = cl_public_components->class_method->all->get( ).
DATA(cl_all_public_inst_attr) = cl_public_components->data->all->get( ).
DATA(cl_all_public_static_attr) = cl_public_components->class_data->all->get( ).
DATA(cl_all_public_constants) = cl_public_components->constant->all->get( ).
DATA(cl_all_public_alias_comps) = cl_public_components->alias->all->get( ).
"More detailed information on components (the examples use static methods of the class)
LOOP AT cl_all_public_stat_methods INTO DATA(stat_meth).
  DATA(meth_name) = stat_meth->name.
  "Parameter information
  DATA(meth_importing_params) = stat_meth->importing_parameters->all->get( ).
  LOOP AT meth_importing_params INTO DATA(import).
    DATA(import_param_name) = import->name.
    DATA(import_param_content) = import->content( )->get( ).
  ENDLOOP.

  DATA(meth_exporting_params) = stat_meth->exporting_parameters->all->get( ).
  DATA(meth_changing_params) = stat_meth->changing_parameters->all->get( ).
  DATA(meth_returning_params) = stat_meth->returning_parameters->all->get( ).
  DATA(meth_exceptions) = stat_meth->exceptions->all->get( ).
  DATA(meth_all_param_names) = stat_meth->parameters->all->get_names( ).

  DATA(meth_get) = stat_meth->content( )->get( ).
  DATA(meth_abstract) = stat_meth->content( )->get_abstract_indicator( ).
  DATA(meth_amdp) = stat_meth->content( )->get_amdp( ).
  DATA(meth_final) = stat_meth->content( )->get_final_indicator( ).
  DATA(meth_redefinition) = stat_meth->content( )->get_redefinition_indicator( ).
  DATA(meth_description) = stat_meth->content( )->get_short_description( ).

  "Source code of the method implementation
  DATA(meth_implementation) = handler_cl->implementation->method( meth_name )->content( )->get( )-source.
ENDLOOP.

*&---------------------------------------------------------------------*
*& Interface
*&---------------------------------------------------------------------*

DATA(handler_intf) = xco_cp_abap=>interface( 'ZDEMO_ABAP_OBJECTS_INTERFACE' ).
DATA(intf_exists) = handler_intf->exists( ).
DATA(intf_name) = handler_intf->name.
DATA(intf_syntax_check_msg) = handler_intf->check_syntax( )->messages.
"Creation and change information
DATA(intf_created_by) = handler_intf->get_created_by( ).
DATA(intf_created_on) = handler_intf->get_created_on( ).
DATA(intf_last_changed_by) = handler_intf->get_last_changed_by( ).
DATA(intf_last_changed_on) = handler_intf->get_last_changed_on( ).
"Interface content information
DATA(intf_get) = handler_intf->content( )->get( ).
DATA(intf_description) = handler_intf->content( )->get_short_description( ).
"Component information
DATA(intf_components) = handler_intf->components.
"Getting information about where the interface is implemented
DATA(intf_implementations_get) = handler_intf->implementations->all->get( ).
DATA(intf_implementations_names) = handler_intf->implementations->all->get_names( ).
"Note that methods are available as shown for classes
DATA(intf_static_meths) = handler_intf->components->class_method->all->get( ).
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

The following snippet includes some file attachments:

```abap
TRY.
    "Creating a new mail instance
    DATA(mail) = cl_bcs_mail_message=>create_instance( ).
    "Settings
    mail->set_sender( '...@...' ).
    mail->add_recipient( '...@...' ).
    mail->set_subject( 'Test Mail' ).    
    mail->set_main( cl_bcs_mail_textpart=>create_instance(
      iv_content      = `<h3>Test Mail</h3><p>Please find some files attached.<br>Cheers</p>`
      iv_content_type = `text/html` ) ).

    "Adding attachments
    "Adding a text file
    mail->add_attachment( cl_bcs_mail_textpart=>create_text_plain(
                            iv_content  = `This is some sample text.`
                            iv_filename = `txt_file.txt` ) ).

    "Adding an XML file
    mail->add_attachment( cl_bcs_mail_textpart=>create_instance(
                            iv_content = `<?xml version="1.0"?>` &&
                                         `<node attr_a="123">` &&
                                         ` <subnode1>` &&
                                         ` <status>A</status>` &&
                                         ` <date format="mm-dd-yyyy">01-01-2024</date>` &&
                                         ` </subnode1>` &&
                                         ` <subnode2>`  &&
                                         ` <text attr_b="1" attr_c="a">abc</text>` &&
                                         ` <text attr_b="2" attr_c="b">def</text>` &&
                                         ` <text attr_b="3" attr_c="c">ghi</text>` &&
                                         ` </subnode2>` &&
                                         `</node>`
                            iv_content_type = `text/xml`
                            iv_filename     = `xml_file.xml` ) ).

    "Adding a zip file
    DATA(zip) = NEW cl_abap_zip( ).
    DATA(txt_content) = `This is some sample text for a file that is zipped.`.
    TRY.
        DATA(conv_xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( txt_content ).
      CATCH cx_sy_conversion_codepage.
    ENDTRY.
    zip->add( EXPORTING name = |test_txt_file.txt|
                        content = conv_xstring ).
    DATA(zipped_file) = zip->save( ).

    mail->add_attachment( cl_bcs_mail_binarypart=>create_instance(
          iv_content      =  zipped_file
          iv_content_type = 'application/x-zip-compressed'
          iv_filename     = 'zip_file.zip' ) ).

    mail->send( ).
  CATCH cx_bcs_mail INTO DATA(error_mail).
    DATA(error_mail_msg) = error_mail->get_text( ).
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

Exception classes are special classes, usually starting with the name <code>CX_*</code>, that serve as the basis for <a href="https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencatchable_exception_glosry.htm">catchable exceptions</a>. When an exception is raised, an object of such an exception class is created. There are several predefined exception classes. Find more information in the [Exceptions and Runtime Errors](27_Exceptions.md) cheat sheet. 

<br>

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
For performing the parallel processing for instances of ABAP Objects. For more information, refer to the class documentation. 
<br><br>

The following example class demonstrates parallel processing using the `CL_ABAP_PARALLEL` class.

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

Notes on the example: 
- As a prerequisite, ensure you have a class implementing the `IF_ABAP_PARALLEL` interface. In this self-contained example, the executable class itself implements it. 
- Before running the class with F9 in ADT, set a breakpoint for the `ASSERT` statement in the implementation of the `if_oo_adt_classrun~main` method. 
- The example visualizes parallel processing by adding timestamps to an internal table. When program execution stops, you can check the table contents.
- First, instances of the example class are created for the parallel processing of instances.
- Next, an instance of the `CL_ABAP_PARALLEL` class is created. The example omits optional parameters; refer to the class documentation for details.
- Parallel processing begins with the `run_inst` method, adding the instances to the input parameter.
- A table of result information for tasks is returned and stored in a data object.
- After starting the parallel processing, the `if_abap_parallel~do` method is called for each instance. This method includes a `DO` loop that populates the `info` table with timestamps.
- The example includes a `WAIT` statement to pause program execution to ensure that all the parallel processing runs have completed.
- The result information table is then looped over, and details are accessed by casting to the respective class (in this case, the example class).
- The `info` table is sorted by timestamps. In the debugger, you can explore the functionality of the parallel processing in the following ways:
  - The `instance` component may show a random instance, like `inst3`, processed first.
  - Due to many loop passes, other instances might start in parallel before the current instance finishes. The `instance` component might show, for example, that `inst3` was processed first while `inst1` began. Some `time_stamp` values may even show identical values.

<br>

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_abap_parallel.
    METHODS constructor IMPORTING text TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA instance_name TYPE string.
    DATA time_stamp TYPE utclong.

    TYPES: BEGIN OF struct,
             time_stamp   TYPE utclong,
             instance TYPE string,
             comment  TYPE string,
           END OF struct.
    DATA info TYPE TABLE OF struct WITH EMPTY KEY.
    DATA parallel_proc LIKE info.

ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    APPEND VALUE #( time_stamp = time_stamp instance = `----` comment = `Time stamp stored when first running/calling the class` ) TO info.

    DATA(inst1) = NEW zcl_demo_abap( `inst1` ).
    DATA(inst2) = NEW zcl_demo_abap( `inst2` ).
    DATA(inst3) = NEW zcl_demo_abap( `inst3` ).
    DATA(inst4) = NEW zcl_demo_abap( `inst4` ).
    DATA(inst5) = NEW zcl_demo_abap( `inst5` ).

    APPEND VALUE #( time_stamp = utclong_current( ) instance = `----` comment = `Time stamp stored before starting parallel processing` ) TO info.

    DATA(parallel) = NEW cl_abap_parallel( ).

    parallel->run_inst( EXPORTING p_in_tab  = VALUE #( ( inst1 )
                                                       ( inst2 )
                                                       ( inst3 )
                                                       ( inst4 )
                                                       ( inst5 ) )
                        IMPORTING p_out_tab = DATA(result_info) ).

    APPEND VALUE #( time_stamp = utclong_current( ) instance = `----` comment = `Time stamp stored after starting parallel processing` ) TO info.

    WAIT UP TO 1 SECONDS.

    APPEND VALUE #( time_stamp = utclong_current( ) instance = `----` comment = `Time stamp stored after the WAIT statement` ) TO info.

    LOOP AT result_info INTO DATA(wa).
      DATA(res) = CAST zcl_demo_abap( wa-inst ).
      APPEND LINES OF res->parallel_proc TO info.
      APPEND VALUE #( time_stamp = res->time_stamp instance = res->instance_name comment = `Time stamp stored in constructor implementation when instantiating class` ) TO info.
    ENDLOOP.

    SORT info BY time_stamp ASCENDING.

    ASSERT 1 = 1.
  ENDMETHOD.

  METHOD if_abap_parallel~do.
    DO 1000 TIMES.
      APPEND VALUE #( time_stamp = utclong_current( ) instance = instance_name comment = |Entry { sy-index } added within "do" method| ) TO parallel_proc.
    ENDDO.
  ENDMETHOD.

  METHOD constructor.
    IF text IS SUPPLIED AND text IS NOT INITIAL.
      instance_name =  text.
    ENDIF.
    time_stamp = utclong_current( ).
  ENDMETHOD.

ENDCLASS.
```

</details> 

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


- Used in the context of the Background Processing Framework (bgPF) to run code asynchronously in the background.
- Different flavors are available:
  - Using bgPF without transactional control, for example, if you do not work with a RAP application or transactional control is not relevant in an ABAP program. In this case, you can implement the <code>IF_BGMC_OP_SINGLE_TX_UNCONTR</code> interface. 
  - Using bgPF with transactional control, for example, if you work with a RAP application. In that case, you can implement the <code>IF_BGMC_OP_SINGLE</code> interface. Note: If you are in a RAP context, you do not need to implement <code>COMMIT/ROLLBACK WORK</code> because the RAP framework takes care of it.
- More information: 
  - <a href="https://help.sap.com/docs/abap-cloud/abap-concepts/background-processing-framework ">Background Processing Framework</a>
  - Transactional control with the <a href="https://help.sap.com/docs/abap-cloud/abap-concepts/controlled-sap-luw">controlled SAP LUW</a>


**Example 1: Using bgPF without transactional control**

<details>
  <summary>🟢 Click to expand for example code (<i>Example 1</i>) </summary>
  <!-- -->

<br>
The following, self-contained, and oversimplified example is intended to give a rough idea about the functionality. It does not include transactional control. The example class can be run using F9 in ADT. It does the following: A demo database table of the cheat sheet repository is filled synchronously and asynchronously (using bgPF) with entries, just to show an effect and get an idea. Two entries are created in the background. <code>WAIT</code> statements are included to have a self-contained example, and that all created database entries can be shown in the output. In the example, the background processing may be visualized, for example, by the <code>MODIFY</code> statement that is followed by a <code>WAIT</code> statement in the loop. The output can show that the entry for the first asynchronously created entry was added before a synchronously created entry. For more visualization options regarding the execution in the background, you can, for example, check the ABAP Cross Trace. For more information, refer to the documentation.

<br>

``` abap
CLASS zcl_demo_abap DEFINITION
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

CLASS zcl_demo_abap IMPLEMENTATION.
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
      DATA(inst) = NEW zcl_demo_abap(  ).

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

</details>  

<br>

**Example 2: Using bgPF with transactional control**

<details>
  <summary>🟢 Click to expand for example code (<i>Example 2</i>)</summary>
  <!-- -->

<br>

This example is similar to example 1. Unlike example 1, example 2 executes operations under transactional control. The transactional phase is explicitly switched using the `cl_abap_tx` class.

<br>

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_bgmc_op_single.
    METHODS constructor IMPORTING num TYPE i OPTIONAL.

  PRIVATE SECTION.
    DATA num TYPE i.
    DATA uuid TYPE sysuuid_x16.
    DATA timestamp TYPE utclong.
    METHODS modify.
    METHODS save.
    METHODS get_uuid RETURNING VALUE(uuid) TYPE sysuuid_x16.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    "Deleting a demo database table
    DELETE FROM zdemo_abap_tabca.

    "Synchronous entry creation
    MODIFY zdemo_abap_tabca FROM @( VALUE #(
      id = get_uuid( )
      calc_result = |Synchronous entry creation in the MAIN method. "num" value: { num }|
      crea_date_time = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ) ) ).

    "Processing code in the background
    DO 2 TIMES.
      "Creating an instance of the example class (that implements the bgPF-relevant
      "interface if_bgmc_op_single)
      DATA(inst) = NEW zcl_demo_abap( num = sy-index ).

      TRY.
          "Getting the default factory for transactional background processes and
          "creating a process for a single operation
          DATA(backgr) = cl_bgmc_process_factory=>get_default( )->create( ).
          "Setting a name of the process
          backgr->set_name( `bgPF Test` ).
          "Setting the transactionally controlled operation of the process
          backgr->set_operation( inst ).
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

      "Another synchronous entry creation
      MODIFY zdemo_abap_tabca FROM @( VALUE #(
        id = get_uuid( )
        calc_result = |Synchronous entry creation in the MAIN method. "num" value: { num }|
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

  METHOD if_bgmc_op_single~execute.
  "Executing the operation under transactional control
  "Note:
  "- The method execution is started in the MODIFY transactional phase
  "- This means that only those operations are allowed that do not violate
  "  transactional contracts to guarantee transactional consistency.
  "- As an example, database modifications performed when the MODIFY transactional
  "  phase is active are not allowed.
  "- When the transactional phase is switched from MODIFY to SAVE, such database
  "  modifications are allowed.
  "- This may happen explicitly using the save method of the cl_abap_tx class,
  "  as is the case in the example.
  "- You can try out the following: Comment out the cl_abap_tx=>save( ). statement,
  "  and run the example again. It results in a runtime error.

    "Includes the modification of class attributes
    modify( ).

    "Explicitly switches from the MODIFY to the SAVE transactional phase
    cl_abap_tx=>save( ).

    "Includes a database modification
    save( ).
  ENDMETHOD.

  METHOD get_uuid.
    TRY.
        uuid = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
    ENDTRY.
  ENDMETHOD.
  METHOD constructor.
    IF num IS SUPPLIED.
      me->num = num.
    ENDIF.
  ENDMETHOD.

  METHOD modify.
    me->uuid = get_uuid( ).
    me->timestamp = utclong_current( ).
  ENDMETHOD.

  METHOD save.
    MODIFY zdemo_abap_tabca FROM @( VALUE #(
      id = uuid
      calc_result = |Asynchronous entry creation in background in the EXECUTE method. "num" value: { num }|
      crea_date_time = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ) ) ).
  ENDMETHOD.

ENDCLASS.
```

</details>  



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
</ul>

To check out examples in demo classes, expand the collapsible sections below.


<details>
  <summary>🟢 1. Read example: Retrieving ABAP cheat sheet markdown content using a GitHub API and sending a ZIP file with the content via email</summary>
  <!-- -->



> [!WARNING] 
> - The following self-contained and oversimplified example is not a representative best practice example, nor does it cover a meaningful use case. It only explores method calls and is intended to give a rough idea of the functionality.</li>
> - The example uses the <code>create_by_url</code> method, which is only suitable for public services or testing purposes. No authentication is required for the APIs used.
> - Note the <a href="README.md#%EF%B8%8F-disclaimer">Disclaimer</a>.</li>
> - For more information, more meaningful examples, and tutorials that deal with the classes and methods, see the following links:
>   - <a href="https://developers.sap.com/tutorials/abap-environment-external-api.html">Call an External API and Parse the Response in SAP BTP ABAP Environment</a>
>   - <a href="https://community.sap.com/t5/technology-blogs-by-sap/how-to-call-a-remote-odata-service-from-the-trial-version-of-sap-cloud/ba-p/13411535">How to call a remote OData service from the trial version of SAP Cloud Platform ABAP environment</a>
> - The example is generally about calling external APIs and parsing the HTTP responses. It retrieves the Markdown files of the ABAP cheat sheet documents contained in the ABAP cheat sheet GitHub repository.  
> - Before using the GitHub APIs, make sure that you have consulted the following documentation: <a href="https://docs.github.com/en">GitHub Docs</a>, <a href="https://docs.github.com/en/enterprise-cloud@latest/rest/markdown/markdown?apiVersion=2022-11-28#render-a-markdown-document">Render a Markdown document</a>, <a href="https://docs.github.com/en/rest/using-the-rest-api/rate-limits-for-the-rest-api?apiVersion=2022-11-28">Rate limits for the REST API</a>
> - For the example to work and send emails, make sure that the configurations from [here](https://help.sap.com/docs/btp/sap-business-technology-platform/emailing) have been performed.
> - To run the example class, copy and paste the code into a class named `zcl_demo_abap`. Run the class using F9. The email sending status will be displayed, and you can expect an email to be sent.   




``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    "Markdown URLs
    CONSTANTS url_cs TYPE string VALUE `https://api.github.com/repos/SAP-samples/abap-cheat-sheets/git/trees/main`.
    CONSTANTS url_gh TYPE string VALUE `https://raw.githubusercontent.com/SAP-samples/abap-cheat-sheets/main/`.
    "Here go email addresses
    CONSTANTS sender_addr TYPE cl_bcs_mail_message=>ty_address VALUE '...@...'.
    CONSTANTS recipient_addr TYPE cl_bcs_mail_message=>ty_address VALUE '...@...'.

    TYPES: BEGIN OF s,
             file_name TYPE string,
             markdown  TYPE string,
             error     TYPE abap_boolean,
           END OF s.
    DATA tab TYPE TABLE OF s WITH EMPTY KEY.
    DATA url TYPE string.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    "----------- Retrieving all Markdown file names in the ABAP cheat sheet GitHub repository -----------

    TRY.
        "Creating a client object using a destination
        "In the example, the HTTP destination is created using a plain URL.
        "Here, a GitHub API is used to retrieve file names of the ABAP cheat sheet repository.
        DATA(http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = cl_http_destination_provider=>create_by_url( i_url = url_cs ) ).
        "Sending an HTTP GET request and returning the response
        "In the example, the HTTP body is retrieved as string data.
        DATA(response) = http_client->execute( if_web_http_client=>get )->get_text(  ).
        IF response CS `API rate limit exceeded`.
          out->write( `API rate limit exceeded` ).
          RETURN.
        ENDIF.

      CATCH cx_root INTO DATA(err).
        out->write( err->get_text( ) ).
        RETURN.
    ENDTRY.


    "----------- Retrieving Markdown content from ABAP cheat sheet GitHub repository -----------

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

      "In the following loop, the Markdown content is retrieved using an HTTP GET request, also
      "by creating a client object and using a destination (another plain URL). The URL is constructed
      "using the constant value plus the markdown file that was retrieved before.
      LOOP AT tab REFERENCE INTO DATA(cs).
        url = url_gh && cs->file_name.
        TRY.
            http_client = cl_web_http_client_manager=>create_by_http_destination( i_destination = cl_http_destination_provider=>create_by_url( i_url = url ) ).
            DATA(raw_md) = http_client->execute( if_web_http_client=>get )->get_text(  ).
            cs->markdown = raw_md.
          CATCH cx_root.
            cs->error = abap_true.
        ENDTRY.
      ENDLOOP.

      "----------- Creating a zip file containing all ABAP cheat sheet Markdown documents -----------

      DATA(zip) = NEW cl_abap_zip( ).

      "Iteratively adding the ABAP cheat sheet Markdown documents to the zip file
      LOOP AT tab REFERENCE INTO cs WHERE error = abap_false.        
        TRY.
            DATA(conv_xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( cs->markdown ).
          CATCH cx_sy_conversion_codepage.
        ENDTRY.

        "Adding the xstring content as file content to zip
        zip->add( name = cs->file_name
                  content = conv_xstring ).

      ENDLOOP.

      "Saving the zip file
      DATA(zipped_file) = zip->save( ).

      "----------- Creating a ZIP file containing all ABAP cheat sheet Markdown documents -----------

      "Sending email
      TRY.
          "Creating a new mail instance
          DATA(mail) = cl_bcs_mail_message=>create_instance( ).
          "Settings
          mail->set_sender( sender_addr ).
          mail->add_recipient( recipient_addr ).
          mail->set_subject( 'Test Mail' ).
          "Main document
          mail->set_main( cl_bcs_mail_textpart=>create_instance(
            iv_content      = '<h3>Test Mail</h3><p>Please find ABAP cheat sheet markdown files attached.<br>Cheers</p>'
            iv_content_type = 'text/html' ) ).
          "Adding an attachment
          mail->add_attachment( cl_bcs_mail_binarypart=>create_instance(
                iv_content      =  zipped_file
                iv_content_type = 'application/x-zip-compressed'
                iv_filename     = 'abap_cheat_sheets.zip' ) ).

          "Sending mail synchronously, displaying the status for each recipient
          mail->send( IMPORTING et_status = DATA(status_table) ).
          out->write( status_table ).
        CATCH cx_bcs_mail INTO DATA(error_mail).
          out->write( |Mail sending error: { error_mail->get_text( ) }| ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 
</details>  

<br>

<details>
  <summary>🟢 2. Post example: Demonstrating a post request by converting Markdown to HTML using the GitHub API</summary>
  <!-- -->



> [!WARNING] 
> - As stated for the previous example, also note for this example: Before using the GitHub APIs, make sure that you have consulted the following documentation: <a href="https://docs.github.com/en">GitHub Docs</a>, <a href="https://docs.github.com/en/enterprise-cloud@latest/rest/markdown/markdown?apiVersion=2022-11-28#render-a-markdown-document">Render a Markdown document</a>, <a href="https://docs.github.com/en/rest/using-the-rest-api/rate-limits-for-the-rest-api?apiVersion=2022-11-28">Rate limits for the REST API</a>
> - To run the example class, copy and paste the code into a class named `zcl_demo_abap`. Run the class using F9. It is set up to display HTML content in the console. Using the GitHub API, sample Markdown content is sent and converted to HTML.



``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS url_api TYPE string VALUE `https://api.github.com/markdown`.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA(markdown_content) =
    `# Lorem ipsum dolor sit amet \n`    &&
    `Consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. \n` &&
    `Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. \n` &&
    `- Duis aute irure \n` &&
    `- Dolor in reprehenderit in voluptate \n` &&
    `1. Velit esse cillum dolore eu fugiat nulla pariatur \n` &&
    `2. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. \n` &&
    `3. [ABAP cheat sheets](https://github.com/SAP-samples/abap-cheat-sheets)`.

    TRY.
        "Creation of a client object using a destination
        "This example deals with a POST request.
        DATA(http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = cl_http_destination_provider=>create_by_url( i_url = url_api ) ).
        DATA(request) = http_client->get_http_request( ).
        request->set_text( `{"text":"` && markdown_content && `"}` ).
        request->set_header_fields( VALUE #( ( name = 'Accept' value = 'application/vnd.github+json' ) ) ).
        DATA(post) = http_client->execute( if_web_http_client=>post ).
        DATA(status) = post->get_status( ).
        IF status-code <> 200.
          out->write( |Post request error: { status-code } / { status-reason }| ).
        ELSE.
          DATA(html) = post->get_text( ).
          out->write( html ).
        ENDIF.
      CATCH cx_root INTO DATA(error).
        out->write( error->get_text( ) ).
    ENDTRY.
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

The XCO library offers classes such as `XCO_CP_XLSX` and methods for reading and writing XLSX content. You can find more information [here](https://help.sap.com/docs/btp/sap-business-technology-platform/xlsx). The following example demonstrates a selection of methods.

Expand the following collapsible sections for further information and example code.

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->
<br>

The following example demonstrates a selection of methods and includes the following steps:

- Importing existing XLSX content into your SAP BTP ABAP Environment system (not related to the XCO library; just to have content to work with in the self-contained example below)
  - This is a simplified, nonsemantic, and explorative RAP example (not delving into RAP as such; just using various ABAP repository objects related to RAP) solely for importing XLSX content to work with in the example.   
  - The import is done using an automatically created SAP Fiori Elements app preview, which provides a simple UI for uploading local XLSX content.
  - The repository objects are automatically created in ADT when walking through a wizard. Refer to the prerequisite steps for details.
- Reading XLSX content into an internal table using XCO
- Writing XLSX content using XCO based on internal table content
  - A demo class explores a selection of XCO classes and methods. 
- Exporting the adapted XLSX content (not related to the XCO library; just to visualize newly created XLSX content using XCO)

> [!NOTE]  
> - Note [this repository's disclaimer](./README.md#%EF%B8%8F-disclaimer) and [the disclaimer in the documentation about XCO](https://help.sap.com/docs/btp/sap-business-technology-platform/xlsx-read-access) for security considerations when importing and processing external content.
> - IDE actions represent a simple way of file import. Find more information in section [Excursion: Exploring Demo Display Class Using IDE Actions](#excursion-exploring-demo-display-class-using-ide-actions).


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


> [!NOTE]  
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

Assuming you have the XLSX content created and uploaded above on your system, you can explore the following example using the XCO classes/methods. Set up a demo class called `zcl_demo_abap` and use the code provided below. After activating it, choose *F9* in ADT to run the class. The example is designed to show output in the console. 

> [!NOTE]  
> - Refer to the comments in the code for information.
> - If you have used different names than those in this example, make sure to replace those names in the code.
> - If your artifacts have a different setup, names, or XLSX content, the example class will not function properly. You willl need to modify the class code to match your specific requirements.

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

## Zip Files

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_ZIP</code> </td>
<td>

The following example creates a zip file and adds three txt files with sample content. Note that the example snippet for `CL_WEB_HTTP_CLIENT_MANAGER` and `CL_HTTP_DESTINATION_PROVIDER` (calling services) also includes the use of `CL_ABAP_ZIP`.

```abap
"Creating a zip file
DATA(zip) = NEW cl_abap_zip( ).

"Adding 3 files to the zip file
DO 3 TIMES.
  DATA(some_content) = |Some text content for file number { sy-index }.|.

  TRY.
      DATA(conv_xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( some_content ).
    CATCH cx_sy_conversion_codepage.
  ENDTRY.

  "Adding xstring as file content to zip
  zip->add( EXPORTING name = |file{ sy-index }.txt|
                      content = conv_xstring ).

ENDDO.

"Saving the zip file
DATA(zipped_file) = zip->save( ).

"Unzipping a zip file
DATA unzipped_content_tab TYPE string_table.

DATA(unzip) = NEW cl_abap_zip( ).
unzip->load( zipped_file ).

LOOP AT unzip->files INTO DATA(file).
  unzip->get( EXPORTING name    = file-name
              IMPORTING content = DATA(unzipped_content)
  ).
  "Converting xstring to string
  TRY.
     DATA(conv_string) = cl_abap_conv_codepage=>create_in( )->convert( unzipped_content ).
    CATCH cx_sy_conversion_codepage.
  ENDTRY.
  APPEND conv_string TO unzipped_content_tab.
ENDLOOP.

*Content of unzipped_content_tab:
*Some text content for file number 1.
*Some text content for file number 2.
*Some text content for file number 3.
```

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## ABAP Unit

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_UNIT_ASSERT</code> </td>
<td>

Provides methods to verify test expectations in ABAP Unit tests. For more information, see the [ABAP Unit Tests](14_ABAP_Unit_Tests.md) cheat sheet.


```abap
"Code in a test class

...

DATA(result) = 100.

...

cl_abap_unit_assert=>assert_equals(
  act = result
  exp = 100
  msg = `The value does not match the expected result`
  quit = if_abap_unit_constant=>quit-no ).
```

</td>
</tr>
<tr>
<td> <code>CL_ABAP_TESTDOUBLE</code><br><code>CL_OSQL_TEST_ENVIRONMENT</code><br><code>CL_CDS_TEST_ENVIRONMENT</code><br><code>CL_BOTD_TXBUFDBL_BO_TEST_ENV</code><br><code>CL_BOTD_MOCKEMLAPI_BO_TEST_ENV</code> </td>
<td>


- The classes can be used in the context of ABAP Unit to create test doubles in a standardized way. 
- The test doubles replace dependent-on components (DOC) during unit tests.
- DOCs:
  - Classes and interfaces: 
    - `CL_ABAP_TESTDOUBLE`: ABAP OO Test Double Framework
  - Database (e.g. database tables or CDS view entities) 
    - `CL_OSQL_TEST_ENVIRONMENT`: ABAP SQL Test Double Framework; to test ABAP SQL statements that depend on data sources such as database tables or CDS view entities
    - `CL_CDS_TEST_ENVIRONMENT`: ABAP CDS Test Double Framework; to test logic implemented in CDS entities
  - RAP business objects
    - `CL_BOTD_TXBUFDBL_BO_TEST_ENV`: Creating transactional buffer test doubles
    - `CL_BOTD_MOCKEMLAPI_BO_TEST_ENV`: Mocking ABAP EML APIs

- Note that more classes are available for other use cases. 
- For more information, see the [ABAP Unit Tests](14_ABAP_Unit_Tests.md) cheat sheet and the [documentation](https://help.sap.com/docs/abap-cloud/abap-development-tools-user-guide/managing-dependencies-with-abap-unit).

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Units of Measurement

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_UOM_DIM_MAINTENANCE</code> </td>
<td>

- Handling dimensions
- Find more information [here](https://help.sap.com/docs/ABAP_ENVIRONMENT/250515df61b74848810389e964f8c367/8961c2c4cebf457f95fb080a736babdc.html?locale=en-US) and in the class documentation.
- The example code snippet includes reading dimensions. See the link above for the methods to create, change and delete dimensions.
 
<br><br>

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

    DATA(dimension_inst) = cl_uom_dim_maintenance=>get_instance( ).

    TRY.
        dimension_inst->read( EXPORTING  dimid  = 'TEMP'
                              IMPORTING  dim_st = DATA(dimension_details) ).

        DATA(id) = dimension_details-dimid.
        DATA(text) = dimension_details-txdim.

        out->write( data = id name = `id` ).
        out->write( data = text name = `text` ).
      CATCH cx_uom_error INTO DATA(read_error).
        out->write( read_error->get_text( ) ).
    ENDTRY.

    out->write( repeat( val = `-` occ = 50 ) ).

**********************************************************************

    "Reading dimensions based on entries in the released view I_UnitOfMeasureDimension
    "Various components and values from the returned structure are output for demonstration purposes.

    SELECT UnitOfMeasureDimension FROM I_UnitOfMeasureDimension INTO TABLE @DATA(dimensions) UP TO 10 ROWS.

    LOOP AT dimensions INTO DATA(wa).

      DATA(dim_inst) = cl_uom_dim_maintenance=>get_instance( ).
      TYPES c6 TYPE c LENGTH 6.
      TRY.
          dim_inst->read( EXPORTING  dimid  = CONV c6( wa )
                          IMPORTING  dim_st = DATA(dim_details) ).

          LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( dim_details ) )->components INTO DATA(co).
            "Applying string conversion to name and values for output purposes
            "Note that many of the components have numeric types.
            DATA(comp) = CONV string( co-name ).
            DATA(value) = CONV string( dim_details-(comp) ).

            "Outputting only those entries that do not have an initial value
            IF value IS NOT INITIAL AND value NP `0*`.
              out->write( data = value name = comp ).
            ENDIF.
          ENDLOOP.
        CATCH cx_uom_error INTO DATA(read_err).
          out->write( read_err->get_text( ) ).
      ENDTRY.
      out->write( repeat( val = `-` occ = 20 ) ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
``` 

</td>
</tr>
<tr>
<td> <code>CL_UOM_MAINTENANCE</code> </td>
<td>

- Handling units of measurement
- Find more information [here](https://help.sap.com/docs/ABAP_ENVIRONMENT/250515df61b74848810389e964f8c367/238be94930874ed9ba3a3dc6469e99b3.html?locale=en-US) and in the class documentation.
- The example code snippet includes reading units of measurement. See the link above for the methods to create, change and delete dimensions.

<br><br>

```abap
DATA(unit_mea_inst) = cl_uom_maintenance=>get_instance( ).
TRY.
 unit_mea_inst->read( EXPORTING unit = 'S'
                      IMPORTING unit_st = DATA(unit_mea) ).

 DATA(unit) = unit_mea-unit.
 DATA(comm) = unit_mea-commercial.
 DATA(tech) = unit_mea-technical.
 DATA(iso) = unit_mea-isocode.
 DATA(id) = unit_mea-dimid.
 DATA(text) = unit_mea-long_text.

 CATCH cx_uom_error INTO DATA(unit_read_error).
  DATA(error_msg) = unit_read_error->get_text( ).
ENDTRY.

"Note: Released view for units of measurement: I_UNITOFMEASURE.
``` 

</td>
</tr>

<tr>
<td> <code>CL_UOM_CONVERSION</code> </td>
<td>

- Converting units of measurement
- Find more information [here](https://help.sap.com/docs/ABAP_ENVIRONMENT/250515df61b74848810389e964f8c367/73109c66f397494abfa2bf3608740c12.html?locale=en-US) and in the class documentation.
- The example code snippet explores unit of measurement conversion. See the link above for more methods available.

<br><br>

```abap
DATA output TYPE decfloat34.

DATA(conv_unit_inst) = cl_uom_conversion=>create( ).

conv_unit_inst->unit_conversion_simple( EXPORTING  input                = CONV decfloat34( '1' )
                                                   round_sign           = 'X'
                                                   unit_in              = 'KG'
                                                   unit_out             = 'G'
                                        IMPORTING  output               = output
                                        EXCEPTIONS conversion_not_found = 01
                                                   division_by_zero     = 02
                                                   input_invalid        = 03
                                                   output_invalid       = 04
                                                   overflow             = 05
                                                   units_missing        = 06
                                                   unit_in_not_found    = 07
                                                   unit_out_not_found   = 08 ).

IF sy-subrc = 0.
 "1000.000
 DATA(outp) = output.
ELSE.
 DATA(subrc) = sy-subrc.
ENDIF.

TYPES: BEGIN OF s,
         input      TYPE decfloat34,
         round_sign TYPE c LENGTH 1,
         unit_in    TYPE cl_uom_conversion=>ty_msehi,
         unit_out   TYPE cl_uom_conversion=>ty_msehi,        
       END OF s,
       tab_type TYPE TABLE OF s WITH EMPTY KEY.

DATA(tab4conv) = VALUE tab_type( ( input = '1.9876543210'
                                   round_sign = '+'  "rounding up
                                   unit_in = 'KG'
                                   unit_out = 'G' )
                                 ( input = '1.9876543210'
                                   round_sign = '-' "rounding down
                                   unit_in = 'KG'
                                   unit_out = 'G' )
                                 ( input = '1987.6543210'
                                   round_sign = ' ' "no rounding
                                   unit_in = 'G'
                                   unit_out = 'KG' )
                                 ( input = '60'
                                   round_sign = 'X' "commercial
                                   unit_in = 'MIN'
                                   unit_out = 'H' )
                                 ( input = '1'
                                   round_sign = 'X'
                                   unit_in = 'TAG'
                                   unit_out = 'H' )
                                 ( input = '1'
                                   round_sign = 'X'
                                   unit_in = 'JHR'
                                   unit_out = 'TAG' )
                                 ( input = '123456'
                                   round_sign = 'X'
                                   unit_in = 'ABC'
                                   unit_out = 'H' ) ).

LOOP AT tab4conv INTO DATA(conversion).
    DATA(conv_inst) = cl_uom_conversion=>create( ).

    conv_inst->unit_conversion_simple( EXPORTING input                  = conversion-input
                                                 round_sign             = conversion-round_sign
                                                 unit_in                = conversion-unit_in
                                                 unit_out               = conversion-unit_out
                                        IMPORTING  output               = output
                                        EXCEPTIONS conversion_not_found = 01
                                                   division_by_zero     = 02
                                                   input_invalid        = 03
                                                   output_invalid       = 04
                                                   overflow             = 05
                                                   units_missing        = 06
                                                   unit_in_not_found    = 07
                                                   unit_out_not_found   = 08 ).

    IF sy-subrc = 0.
     outp = output.
    ELSE.
     subrc = sy-subrc.
    ENDIF.
ENDLOOP.

*Results:
*1987.655
*1987.654
*1.98765
*1.0
*24.0
*365.0
*7 (error, sy-subrc value)

"Retrieving mass- and time-related units of measurement using
"a released API
SELECT *
 FROM i_unitofmeasure
 WHERE unitofmeasuredimension = `TIME` OR unitofmeasuredimension = `MASS`
 INTO TABLE @DATA(umea).
``` 

</td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Programmatic ABAP Test Cockpit (ATC) Check

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_SATC_API</code> </td>
<td>

- The class provides access to the ABAP Test Cockpit (ATC) API.
- Find more information about checking the quality of ABAP Code with ATC [here](https://help.sap.com/docs/ABAP_Cloud/bbcee501b99848bdadecd4e290db3ae4/4ec5711c6e391014adc9fffe4e204223.html?locale=en-US).
- The example code snippet ...
  - explores the API by creating a factory object, creating and starting an ATC run, and checking the information returned. 
  - includes some statements that are found by ATC runs such as a deprecated statement, and a literal in the code. 
  - includes check variants. You may want to comment out the one, and comment in the other. For example, find check variants as follows: Right-click in the code -> Run as -> 3 ABAP Text Cockpit With... -> Choose Browse in the pop-up -> Insert `*` for *Select your check variant* and find available check variants in the system. You can also create your own ATC check variant, for example, by right-clicking your package -> New -> Other ABAP Repository Object -> Filter for *ATC Check Variant*, select it and proceed with the wizard. 
 
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
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA num TYPE i VALUE 1.
    GET REFERENCE OF num INTO DATA(ref).
    DATA(current_time) = sy-uzeit.
    out->write( `Some text` ).

**********************************************************************

    TRY.
        "Creating a factory object
        DATA(atc) = cl_satc_api=>create_api_factory( ).
        "Creating an ATC run and starting it
        "The ATC run result is stored in a variable.
        DATA(atc_result) = atc->create_run(
        atc->create_run_configuration( atc->create_object_set_for_list( VALUE #( ( obj_type = 'CLAS' obj_name = 'ZCL_DEMO_ABAP' ) ) )
        )->set_check_variant( atc->get_check_variant_by_name(
        'ABAP_CLOUD_READINESS'
        "'ABAP_CLOUD_DEVELOPMENT_DEFAULT'
        ) )
        )->run( ).
      CATCH cx_satc_api INTO DATA(exc).
        out->write( |Error: { exc->get_text( ) }| ).
        RETURN.
    ENDTRY.
    "Returning the result ID
    DATA(result_id) = atc_result->get_result_id( ).
    out->write( |Result ID: { result_id }| ).

    "Returning all information of the findings reported during the run
    DATA(findings) = atc_result->get_findings_with_text( ).
    IF findings IS INITIAL.
      out->write( `No findings` ).
    ELSE.
      out->write( findings ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
``` 

</td>
</tr>


</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Handling Number Ranges

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_NUMBERRANGE_OBJECTS</code><br><code>CL_NUMBERRANGE_INTERVALS</code><br><code>CL_NUMBERRANGE_RUNTIME</code> </td>
<td>

- Find more information in the SAP Help Portal, section [Number Range Solution](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/0335201e35bb433eab298bf8f389ea11.html), and in the class documentation.
- The code snippets rudimentarily show methods of the classes to retrieve information. You can find more snippets in the documentation linked above and check the various parameters.
 
<br>

```abap
"Use a valid number range object
DATA(numberrange_object) = 'Z.........'.

"Retrieving attributes of number range objects
TRY.
    cl_numberrange_objects=>read(
      EXPORTING
        object          = numberrange_object
      IMPORTING
        attributes      = DATA(attr)
        interval_exists = DATA(intv_exists)
        obj_text        = DATA(obj_text)
    ).
  CATCH cx_nr_object_not_found cx_number_ranges.
ENDTRY.

"Retrieving intervals of number range objects
TRY.
    cl_numberrange_intervals=>read(
      EXPORTING
        object       = numberrange_object
      IMPORTING
        interval     = DATA(intv)
    ).
  CATCH cx_nr_object_not_found cx_nr_subobject cx_number_ranges.
ENDTRY.

"Retrieving numbers from number range object intervals
TRY.
    cl_numberrange_runtime=>number_get(
      EXPORTING
        nr_range_nr       = '01'
        object            = numberrange_object
      IMPORTING
        number            = DATA(number)
        returncode        = DATA(rc)
        returned_quantity = DATA(quan)
    ).
  CATCH cx_nr_object_not_found cx_number_ranges.
ENDTRY.
``` 

</td>
</tr>


</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Releasing APIs

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_ABAP_API_STATE</code> </td>
<td>

- You can use the class to release APIs programmatically. Note that you can also achieve this using ADT tools. 
- Find more information in the class documentation.
- The code snippets uses various methods offered by the class and illustrates the following aspects: creating an instance of the API state handler for a specified API (a demo class is inserted), releasing the API for ABAP for Cloud Development (by also specifying a transport request), retrieving release information, and deleting the release state for the specified API again. 
 
<br>

```abap
TRY.
    DATA(api_state) = cl_abap_api_state=>create_instance( api_key = VALUE #( object_type = 'CLAS' object_name = 'ZCL_DEMO_TEST' ) ).

    api_state->release( use_in_cloud_development = abap_true
                        use_in_key_user_apps     = abap_false
                        request                  = 'SOME_TR_REQ' ).

    DATA(rel_info) = api_state->get_release_info( ).

    DATA(is_released) = api_state->is_released( use_in_cloud_development = abap_true
                                                use_in_key_user_apps     = abap_false ).

    IF is_released = abap_true.
      api_state->delete_release_state( request = 'SOME_TR_REQ' ).

      rel_info = api_state->get_release_info( ).

      is_released = api_state->is_released( use_in_cloud_development = abap_true
                                            use_in_key_user_apps     = abap_false ).

    ENDIF.

  CATCH cx_abap_api_state INTO DATA(error).
    DATA(error_text) = error->get_text( ).
ENDTRY.
``` 

</td>
</tr>


</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Application Jobs

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_APJ_DT_CREATE_CONTENT</code><br><code>CL_APJ_RT_API</code> </td>
<td>

- `cl_apj_dt_create_content`: Used to create and delete class-based application job catalog entries and templates programmatically.
- `cl_apj_rt_api`: Provides methods to schedule and manage application jobs programmatically.
- A prerequisite for application jobs is a class implementing the `if_apj_rt_run` interface, with its `execute` method run by the background job.
- Find more information [here](https://help.sap.com/docs/btp/sap-business-technology-platform/application-jobs).
- The example, which you can copy into a demo class (e.g. `zcl_demo_abap`) and run using F9 in ADT, covers methods of both classes and explores:
  - Creating an application job catalog entry
  - Creating an application job template
  - Scheduling an application job
  - Getting the application job status
  - Retrieving and canceling application jobs
  - Deleting an application job template
  - Deleting an application job catalog entry

 <br>

<details>
  <summary>🟢 Click to expand for more information and example code</summary>
  <!-- -->

<br>

Notes on the example:

- It rudimentarily explores class methods and is not intended as a best practice implementation. Check the class documentation and the [SAP Help Portal](https://help.sap.com/docs/btp/sap-business-technology-platform/application-jobs) for detailed information.
- It uses a demo database table from the ABAP cheat sheet repository.
- The `execute` method, run by the background job, creates several random entries in a database table to demonstrate the effect of application job runs.
- The example class is self-contained, including the classrun interface so it can be executed, and the implementation of the `if_apj_rt_run` interface. After scheduling the application job, a `WAIT` statement interrupts the program flow. The application job is scheduled to start immediately and run every minute, potentially starting during the wait. The `SELECT` statement following the `WAIT` statement retrieves data from the demo database table into an internal table, possibly including demo data inserted during the job run. If there are no entries, try running the class again and/or change the value of `wait_seconds` constant.
- Due to program interruption, the class run takes time to complete and display output in the console.

The example does not cover several aspects (check the documentation for more information), including:  
- Error handling in the `execute` method  
- Implementing other available interfaces, like `if_apj_dt_defaults`  
- Setting up authorization  
- Using ADT to create job catalog entries and templates

Before running the example class, check the values of the constants in the public section of the class. In particular, ensure you have valid values for these constants:
- `class_name`: Provide the class name where you have copied the code (if it is not `zcl_demo_abap`).
- `transport_request`: Enter a valid transport request.
- `package`: Enter a valid package name.
- Check the *constants related to the example class execution*. Changes to these values affect the program flow.

<br>

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_apj_rt_run.
    "Public attribute supplied with a value in the job run
    DATA text TYPE c LENGTH 10.

    "---------------------------------------------------------------------
    "Constants related to the catalog
    CONSTANTS catalog_name      TYPE cl_apj_dt_create_content=>ty_catalog_name  VALUE 'ZDEMO_ABAP_JCAT'.
    CONSTANTS catalog_text      TYPE cl_apj_dt_create_content=>ty_text          VALUE 'Demo application job catalog'.
    CONSTANTS class_name        TYPE cl_apj_dt_create_content=>ty_class_name    VALUE 'ZCL_DEMO_ABAP'.
    "Constants related to the template
    CONSTANTS template_name     TYPE cl_apj_dt_create_content=>ty_template_name VALUE 'ZDEMO_ABAP_JTEMPLATE'.
    CONSTANTS template_text     TYPE cl_apj_dt_create_content=>ty_text          VALUE 'Demo application job template'.
    "Constants related to the transport request and package
    CONSTANTS transport_request TYPE cl_apj_dt_create_content=>ty_transport_request VALUE '...'.
    CONSTANTS package           TYPE cl_apj_dt_create_content=>ty_package           VALUE '...'.
    "Constant related to the application job
    CONSTANTS appl_job_text TYPE cl_apj_rt_api=>ty_job_text VALUE 'DEMO_APPL_JOB'.
    "Constants related to the example class execution
    CONSTANTS delete_entries TYPE abap_boolean VALUE abap_false.
    CONSTANTS demo_attribute_value LIKE text VALUE 'TEST'.
    CONSTANTS delete_demo_dbtab_entries TYPE abap_boolean VALUE abap_true.
    "The following constant specifies the duration in seconds the executing class should wait before
    "continuing the processing. Depending on the value, you can expect none or multiple entries in
    "an internal table that is displayed after running the class.
    CONSTANTS wait_seconds TYPE i VALUE
    40
    "80
    "120
    .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA(timestamp) = utclong_current( ).

    IF delete_demo_dbtab_entries = abap_true.
      DELETE FROM zdemo_abap_tabca.
    ENDIF.

*&---------------------------------------------------------------------*
*& Creating an application job catalog entry
*&---------------------------------------------------------------------*

    "Creating an instance to create an application job catalog entry and template
    DATA(jcat) = cl_apj_dt_create_content=>get_instance( ).

    "Checking whether the application job catalog exists
    DATA(jcat_exists) = jcat->exists_job_cat_entry( catalog_name ).

    IF jcat_exists <> 'Y'.
      TRY.
          DATA(jcat_flag) = jcat->create_job_cat_entry(
              iv_catalog_name       = catalog_name
              iv_class_name         = class_name
              iv_text               = catalog_text
              iv_catalog_entry_type = cl_apj_dt_create_content=>class_based
              iv_transport_request  = transport_request
              iv_package            = package ).

          IF jcat_flag = abap_true.
            out->write( |Job catalog entry { catalog_name } creation: Ok!| ).
          ENDIF.
        CATCH cx_apj_dt_content INTO DATA(error_jcat).
          DATA(jcat_error_msg) = error_jcat->get_longtext( ).
          out->write( jcat_error_msg ).
      ENDTRY.
    ELSE.
      out->write( |Job catalog entry { catalog_name } already available.| ).
    ENDIF.

*&---------------------------------------------------------------------*
*& Creating an application job template
*&---------------------------------------------------------------------*

    "Checking whether the application job template exists
    DATA(jtempl_exists) = jcat->exists_job_template_entry( template_name ).

    IF jtempl_exists <> 'Y'.
      TRY.
          DATA(jtempl_flag) =  jcat->create_job_template_entry(
              iv_template_name     = template_name
              iv_catalog_name      = catalog_name
              iv_text              = template_text
              iv_transport_request = transport_request
              iv_package           = package ).

          IF jtempl_flag = abap_true.
            out->write( |Job template { template_name } creation: Ok!| ).
          ENDIF.

        CATCH cx_apj_dt_content INTO DATA(error_jtempl).
          DATA(jtempl_error_msg) = error_jtempl->get_longtext( ).
          out->write( jcat_error_msg ).
      ENDTRY.
    ELSE.
      out->write( |Job template { template_name } already available.| ).
    ENDIF.

*&---------------------------------------------------------------------*
*& Scheduling an application job
*&---------------------------------------------------------------------*

    "Note:
    "- Using the generate_jobkey method, you can create the job key (i.e.
    "  job name and count). The values can be passed to the optional parameters
    "  iv_jobname and iv_jobcount of the schedule_job method. This example does
    "  not use the method. The job key is created when calling the schedule_job
    "  method.
    "- The schedule_job method has numerous mandatory and optional parameters.
    "  Make sure that you check the class documentation for more information.

    GET TIME STAMP FIELD DATA(ts).

    TRY.
        cl_apj_rt_api=>schedule_job(
          EXPORTING
            iv_job_template_name   = template_name
            iv_job_text            = appl_job_text
            "The example specifies the start condition as starting immediately.
            "You can also provide a time stamp.
            is_start_info          = VALUE #( start_immediately = abap_true )

            "Providing information about periodicity
            "The example assigns values to several components of the structure,
            "including the granularity, which is assigned 'MI' (minutes).
            is_scheduling_info     = VALUE #( test_mode = abap_false
                                              periodic_granularity = 'MI'
                                              periodic_value = '1'
                                              timezone = 'UTC' )

            "Providing information about when to end the application job execution
            "The example specifies some components of the structure. Among them is
            "a time stamp that specifies to stop the execution in half an hour.
            is_end_info            = VALUE #( type = 'DATE' "End by
                                              timestamp = cl_abap_tstmp=>move_to_short(
                                                cl_abap_tstmp=>add( tstmp = ts
                                                                    secs  = 1800 ) ) "Half an hour in seconds
                                            )

            "Providing the public attribute of the class with a concrete value
            it_job_parameter_value = VALUE #( ( name = 'TEXT'
                                                t_value = VALUE #( ( sign = 'I' option = 'EQ' low = demo_attribute_value ) ) ) )
          IMPORTING
            ev_jobname             = DATA(jobname)
            ev_jobcount            = DATA(jobcount)
        ).

        out->write( |Job name: "{ jobname }"| ).
        out->write( |Job count: "{ jobcount }"| ).

      CATCH cx_apj_rt INTO DATA(error_jschedule).
        DATA(jschedule_error_msg) = error_jschedule->get_longtext( ).
        out->write( jschedule_error_msg ).
        out->write( `Program execution terminated` ).
        RETURN.
    ENDTRY.

*&---------------------------------------------------------------------*
*& Getting the application job status
*&---------------------------------------------------------------------*

    TRY.
        cl_apj_rt_api=>get_job_status(
          EXPORTING
            iv_jobname         = jobname
            iv_jobcount        = jobcount
          IMPORTING
            ev_job_status      = DATA(jobstatus)
            ev_job_status_text = DATA(jobtext)
        ).

        out->write( |Job status: "{ jobstatus }"| ).
        out->write( |Job text: "{ jobtext }"| ).

      CATCH cx_apj_rt INTO DATA(error_jstatus).
        DATA(jstatus_error_msg) = error_jstatus->get_longtext( ).
        out->write( jstatus_error_msg ).
    ENDTRY.

*&---------------------------------------------------------------------*
*& Evaluating the result of the job run
*&---------------------------------------------------------------------*

    "Note:
    "- The example job run includes the creation of database table entries.
    "- The WAIT statement is used to interrupt the program flow of the
    "  self-contained example class so that the application jobs can run
    "  in the meantime.
    "- Depending on the waiting time, you can expect entries in the internal
    "  table. Otherwise, you may increase the value for the seconds after WAIT
    "  UP TO as the example is designed to run the application job minute-wise.

    WAIT UP TO wait_seconds SECONDS.

    SELECT * FROM zdemo_abap_tabca INTO TABLE @DATA(applj_result).
    out->write( |\n| ).
    out->write( `Application job result` ).
    out->write( |Time stamp when the class was executed: "{ timestamp }"| ).
    out->write( |\n| ).
    IF applj_result IS INITIAL.
      out->write( `No table entries have been created yet. You may want to run the class again.` ).
    ELSE.
      out->write( applj_result ).
    ENDIF.

*&---------------------------------------------------------------------*
*& Retrieving and cancelling aplication jobs
*&---------------------------------------------------------------------*

    "Note:
    "- In the example, application jobs are first retrieved based on the
    "  the specified job catalog. An internal table is returned containing
    "  details of the application jobs, such as the job name and count.
    "- The intenral table is looped across. Using the cancel_job method,
    "  the application jobs are cancelled (if currently running) or deleted.

    TRY.
        DATA(jlist) = cl_apj_rt_api=>find_jobs_with_jce( catalog_name ).

        out->write( |\n| ).
        out->write( `Application job list:` ).
        out->write( jlist ).
      CATCH cx_apj_rt INTO DATA(error_jfind).
        DATA(jfind_error_msg) = error_jfind->get_longtext( ).
        out->write( jfind_error_msg ).
    ENDTRY.

    LOOP AT jlist INTO DATA(j).
      TRY.
          cl_apj_rt_api=>cancel_job(
            EXPORTING
              iv_jobname  = j-jobname
              iv_jobcount = j-jobcount
          ).
        CATCH cx_apj_rt INTO DATA(error_jcancel).
          DATA(jcancel_error_msg)  = error_jcancel->get_longtext( ).
          out->write( jcancel_error_msg ).
      ENDTRY.
    ENDLOOP.

    TRY.
        jlist = cl_apj_rt_api=>find_jobs_with_jce( catalog_name ).

        out->write( |\n| ).

        IF jlist IS INITIAL.
          out->write( `Application jobs for the demo job catalog cancelled.` ).
        ELSE.
          out->write( `Cancel the demo application jobs` ).
        ENDIF.
      CATCH cx_apj_rt INTO error_jfind.
        jfind_error_msg = error_jfind->get_longtext( ).
        out->write( jfind_error_msg ).
    ENDTRY.

*&---------------------------------------------------------------------*
*& Deleting an application job template
*&---------------------------------------------------------------------*

    IF delete_entries = abap_true.
      TRY.
          DATA(jtempl_del_flag) = jcat->delete_job_template_entry(
                                    iv_template_name     = template_name
                                    iv_transport_request = transport_request ).

          IF jtempl_del_flag = abap_true.
            out->write( |Job template { template_name } deletion: Ok!| ).
          ENDIF.
        CATCH cx_apj_dt_content INTO DATA(error_jtempl_del).
          DATA(jtempl_del_error_msg) = error_jtempl_del->get_longtext( ).
          out->write( jtempl_del_error_msg ).          
      ENDTRY.

*&---------------------------------------------------------------------*
*& Deleting an aplication job catalog entry
*&---------------------------------------------------------------------*

      TRY.
          DATA(jcat_del_flag) =  jcat->delete_job_cat_entry(
                                   iv_catalog_name      = catalog_name
                                   iv_transport_request = transport_request ).

          IF jcat_del_flag = abap_true.
            out->write( |Job catalog entry { catalog_name } deletion: Ok!| ).
          ENDIF.

        CATCH cx_apj_dt_content INTO DATA(error_jcat_del).
          DATA(jcat_del_error_msg) = error_jcat_del->get_text( ).
          out->write( jcat_del_error_msg ).          
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD if_apj_rt_run~execute.
    "Adding sample entries to a demo database table
    DATA itab TYPE TABLE OF zdemo_abap_tabca WITH EMPTY KEY.

    DATA(random_num) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                              min  = 1
                                              max  = 10000 )->get_next( ).

    DO 3 TIMES.
      APPEND INITIAL LINE TO itab ASSIGNING FIELD-SYMBOL(<line>).
      TRY.
          <line>-id = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.
      ENDTRY.
      <line>-num1 = random_num.
      <line>-num2 = random_num.
      <line>-calc_result = |Attribute value: "{ text }"; timestamp: "{ utclong_current( ) }"|.
    ENDDO.

    MODIFY zdemo_abap_tabca FROM TABLE @itab.
    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.
``` 

</details>  
</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Generative AI

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_AIC_ISLM_COMPL_API_FACTORY</code><br><code>CL_AIC_ISLM_PROMPT_TPL_FACTORY</code> </td>
<td>

- ABAP classes available in the *ABAP AI SDK powered by Intelligent Scenario Lifecycle Management* for interacting with large language models (LLMs) in custom implementations
- Find more information in the [documentation](https://help.sap.com/docs/abap-ai/generative-ai-in-abap-cloud/generative-ai-in-abap-cloud?locale=en-US) and the [Generative AI](30_Generative_AI.md) cheat sheet.
- The following method calls create an instance of the ISLM completion API, use a prompt as string, and retrieve the LLM answer.

 <br>

```abap
TRY.
    FINAL(ai_api) = cl_aic_islm_compl_api_factory=>get( )->create_instance( 'ZDEMO_ABAP_INT_SCEN' ).
    FINAL(result) = ai_api->execute_for_string( `Tell me a joke.` ).
    FINAL(completion) = result->get_completion( ).
  CATCH cx_aic_api_factory cx_aic_completion_api INTO FINAL(error).
    FINAL(error_text) = error->get_text( ).
ENDTRY.
```
 
</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Programmatically Creating and Releasing Transport Requests

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>XCO_CP_CTS</code> </td>
<td>

The following code snippet uses the `XCO_CP_CTS` class, among others, to demonstrate:

- Programmatically creating a transport request 
- Retrieving information about the transport request
- Excursions:
  - Programmatically creating a demo class (`ZCL_DEMO_ABAP_CALCULATE`) and assigning it to the new transport request. This simple, executable class inherits from `CL_XCO_CP_ADT_SIMPLE_CLASSRUN` and includes the `calculate` method.
  - Dynamically calling the `calculate` method to confirm the class creation. For more details, refer to the [Dynamic Programming](06_Dynamic_Programming.md) cheat sheet.
- Programmatically releasing a transport task and request

> [!NOTE]  
> - The example is simplified and non-semantic, exploring various functionalities offered by the XCO APIs. See the repository's [disclaimer](./README.md#%EF%B8%8F-disclaimer).
> - For more information and code snippets, refer to the [SAP Help documentation](https://help.sap.com/docs/btp/sap-business-technology-platform/correction-and-transport-system).
> - The example assumes you have a transportable package, represented by the `pkg_name` constant in the example.
> - To try the example out, create a demo class named `ZCL_DEMO_ABAP` and paste the code into it. Edit the code by providing the `pkg_name` constant with your package name. It is assumed that a demo class named `ZCL_DEMO_ABAP_CALCULATE` does not exist. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console. You may also want to open the created `ZCL_DEMO_ABAP_CALCULATE` class.

 <br>

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
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "Constant names used in the example
    "Name of a package that is assigned a demo class that is to be transported
    CONSTANTS pkg_name TYPE sxco_package VALUE 'Z_SOME_PACKAGE'.
    "Names of a class and a method to be created
    CONSTANTS cl_name TYPE sxco_ao_object_name VALUE 'ZCL_DEMO_ABAP_CALCULATE'.
    CONSTANTS meth_name TYPE if_xco_gen_clas_s_fo_d_section=>tv_method_name VALUE 'CALCULATE'.

*&---------------------------------------------------------------------*
*& Creating transport request
*&---------------------------------------------------------------------*

    "Getting transport- and package-related information
    DATA(pkg) = xco_cp_abap_repository=>package->for( pkg_name ).
    IF pkg->exists( ) = abap_false.
      out->write( |Package { pkg_name } does not exist.| ).
      RETURN.
    ENDIF.

    DATA(pkg_read) = pkg->read( ).

    "Transport layer
    DATA(tr_layer) = pkg_read-property-transport_layer->value.
    "Transportation target
    DATA(tr_target) = pkg_read-property-transport_layer->get_transport_target( )->value.
    "Package type
    DATA(pkg_type) = pkg_read-property-package_type->value.
    "Software component
    DATA(software_comp) = pkg_read-property-software_component->name.

    IF tr_layer IS NOT INITIAL.
      out->write( |Transport layer: { tr_layer }| ).
    ENDIF.
    IF tr_target IS NOT INITIAL.
      out->write( |Transport target: { tr_target }| ).
    ENDIF.
    IF pkg_type IS NOT INITIAL.
      out->write( |Package type: { pkg_type }| ).
    ENDIF.
    IF software_comp IS NOT INITIAL.
      out->write( |Software component: { software_comp }| ).
    ENDIF.

    "Creating transport request with an unclassified task based on the transport target
    DATA(tr_request) = xco_cp_cts=>transports->workbench( tr_target )->create_request( 'Some transport request' ).
    DATA(tr_req_id) = tr_request->value.

    out->write( |Transport request ID: { tr_req_id }| ).

*    "Attribute information about transport request
*    DATA tr_attribute_infos TYPE string_table.
*    DATA(tr_attributes) = tr_request->attributes->all->get( ).
*    LOOP AT tr_attributes INTO DATA(attr).
*      DATA(attr_name) = attr->get_attribute( )->name.
*      DATA(attr_value) = attr->get_value( ).
*      APPEND |Attribute "{ attr_name }", value "{ attr_value }"| TO tr_attribute_infos.
*    ENDLOOP.
*
*    IF tr_attribute_infos IS NOT INITIAL.
*      out->write( `Transport request attributes:` ).
*      out->write( tr_attribute_infos ).
*    ENDIF.

    "Transport request status
    DATA(tr_status) = tr_request->get_status( )->value.
    out->write( |Transport request status: { tr_status }| ).

    "Retrieving information about tasks
    DATA(tr_req_tasks) = tr_request->get_tasks( ).

    DATA tr_tasks_info TYPE string_table.
    LOOP AT tr_req_tasks INTO DATA(task).
      DATA(task_value) = task->value.
      DATA(request_of_task) = task->get_request( )->value.
      DATA(status_of_task) = task->get_status( )->value.
      DATA(task_last_changed) = task->properties( )->get_last_changed( )->as( xco_cp_time=>format->iso_8601_extended )->value.
      DATA(task_owner) = task->properties( )->get_owner( )->name.
      DATA(task_descr) = task->properties( )->get_short_description( ).
      APPEND |Task "{ task_value }", request "{ request_of_task }", status "{ status_of_task }", last changed at "{ task_last_changed }", | &&
      |owner "{ task_owner }", description "{ task_descr }"| TO tr_tasks_info.
    ENDLOOP.

    out->write( `Transport tasks:` ).
    out->write( tr_tasks_info ).

*&---------------------------------------------------------------------*
*& Excursion: Creating a demo class programmatically and assigning it
*&            to the transport request
*&---------------------------------------------------------------------*

    DATA(demo_cl) = xco_cp_abap=>class( cl_name ).

    "Checking if the class exists
    IF demo_cl->exists( ).
      out->write( |Class { cl_name } already exists.| ).
      RETURN.
    ENDIF.

    "Using the XCO generation API
    DATA(env) = xco_cp_generation=>environment->dev_system( tr_req_id ).
    DATA(put) = env->create_put_operation( ).
    DATA(cl_spec) = put->for-clas->add_object( cl_name )->set_package( pkg_name )->create_form_specification( ).

    "Setting up the class
    cl_spec->set_short_description( 'Demo class' ).
    cl_spec->definition->set_superclass( 'CL_XCO_CP_ADT_SIMPLE_CLASSRUN' ).
    cl_spec->definition->set_create_visibility( xco_cp_abap_objects=>visibility->public ).
    cl_spec->definition->set_final( ).

    "Method/type definitions in the public visibility section
    cl_spec->definition->section-public->add_method( 'constructor' ).
    cl_spec->definition->section-public->add_method( 'main' )->set_redefinition( ).
    cl_spec->definition->section-public->add_type( `op` )->for( xco_cp_abap=>type-source->for( 'c LENGTH 1' ) ).

    DATA(calc) = cl_spec->definition->section-public->add_method( meth_name ).
    calc->add_importing_parameter( 'num1' )->set_pass_by_reference( )->set_type( xco_cp_abap=>type-source->for( 'i' ) ).
    calc->add_importing_parameter( 'operator' )->set_pass_by_reference( )->set_type( xco_cp_abap=>type-source->for( 'op' ) ).
    calc->add_importing_parameter( 'num2' )->set_pass_by_reference( )->set_type( xco_cp_abap=>type-source->for( 'i' ) ).
    calc->add_returning_parameter( 'result' )->set_type( xco_cp_abap=>type-source->for( 'decfloat34' ) ).

    "Method implementations
    cl_spec->implementation->add_method( `constructor` )->set_source( VALUE #( ( `super->constructor( ).` ) ) ).

    cl_spec->implementation->add_method( 'main' )->set_source( VALUE #( ( |DATA(result) = { meth_name }( num1 = 1 operator = '+' num2 = 2 ).| )
                                                                        ( `out->write( result ).` ) ) ).

    cl_spec->implementation->add_method( 'calculate' )->set_source( VALUE #( ( `CASE operator.` )
                                                                             ( `WHEN '+'. result = num1 + num2.` )
                                                                             ( `WHEN '-'. result = num1 - num2.` )
                                                                             ( `WHEN '*'. result = num1 * num2.` )
                                                                             ( `WHEN '/'. result = num1 / num2.` )
                                                                             ( `WHEN OTHERS. result = 0.` )
                                                                             ( `ENDCASE.` ) ) ).

    TRY.
        DATA(creation_result) = put->execute( ).
        out->write( |Class { cl_name } generated.| ).
      CATCH cx_xco_gen_put_exception INTO DATA(err).
        out->write( err->get_text( ) ).
        RETURN.
    ENDTRY.

*&---------------------------------------------------------------------*
*& Excursion: Calling a method in the newly created class dynamically
*&---------------------------------------------------------------------*

    DATA oref TYPE REF TO object.
    CREATE OBJECT oref TYPE (cl_name).

    DATA dref TYPE REF TO data.
    DATA(type_name) = |{ cl_name }=>OP|.
    CREATE DATA dref TYPE (type_name).
    dref->* = '*'.

    DATA(ptab) = VALUE abap_parmbind_tab( ( name  = 'NUM1'
                                            kind  = cl_abap_objectdescr=>exporting
                                            value = NEW i( 5 ) )
                                          ( name  = 'OPERATOR'
                                            kind  = cl_abap_objectdescr=>exporting
                                            value = dref )
                                          ( name  = 'NUM2'
                                            kind  = cl_abap_objectdescr=>exporting
                                            value = NEW i( 5 ) )
                                          ( name  = 'RESULT'
                                            kind  = cl_abap_objectdescr=>returning
                                            value = NEW decfloat34( ) ) ).

    CALL METHOD oref->(meth_name) PARAMETER-TABLE ptab.

    DATA(result) = CONV decfloat34( ptab[ name = 'RESULT' ]-value->* ).

    out->write( |Calculation result when calling method { meth_name } of class { cl_name }: { result }| ).

*&---------------------------------------------------------------------*
*& Releasing transport tasks and request
*&---------------------------------------------------------------------*

    DATA(cl_handler) = xco_cp_abap_repository=>object->clas->for( cl_name ).
    "You might also use the handler demo_cl from above.

    "Checking if the class is locked in a transport request
    IF cl_handler->if_xco_cts_changeable~get_object( )->is_locked( ) = abap_true.
      DATA(tr_lock) = cl_handler->if_xco_cts_changeable~get_object(
        )->get_lock(
        )->get_transport( ).

      DATA(tr_req_for_cl) = xco_cp_cts=>transport->for( tr_lock )->get_request( )->value.
      out->write( |Class { cl_name } is currently locked in TR { tr_req_for_cl }| ).
    ENDIF.

    "Releasing the tasks
    DATA(tr_tasks) = tr_request->get_tasks( ).

    LOOP AT tr_tasks INTO DATA(tr_task).
      IF tr_task->get_status( ) = xco_cp_transport=>status->modifiable.
        tr_task->release( ).
      ENDIF.
    ENDLOOP.

    "Releasing the transport request
    TRY.
        tr_request->release( ).
        out->write( |Transport { tr_req_id } request released| ).
        tr_status = tr_request->get_status( )->value.
        out->write( |Transport request status: { tr_status }| ).
      CATCH cx_xco_runtime_exception INTO DATA(rel_error).
        out->write( rel_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
``` 

</details>  
</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Repairing and Cleaning up HTML and XML Documents

<table>
<tr>
<td> Class </td> <td> Details/Code Snippet </td>
</tr>
<tr>
<td> <code>CL_HTMLTIDY</code> </td>
<td>

- The `CL_HTMLTIDY` class repairs and cleans up HTML and XML documents.
- It fixes coding errors like missing or incorrect tags, converting documents into well-formatted versions.
- It performs tasks such as indentation adjustments and removes unnecessary markup and comments.
- The class allows customization of formatting options.
- The ABAP class uses HTML Tidy. Refer to the links available in the class's ABAP Doc comment.

<br>

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

The following example explores some of the capabilities of the `CL_HTMLTIDY` class:

- Repairing and cleaning up an HTML document
  - Creating an HTMLTidy instance
  - Resetting all option settings to default values
  - Getting default values of option settings
  - Setting options explicitly
  - Getting set options
  - Evaluating the cleanup operation result
- Repairing and cleaning up an XML document
  
To try the example out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.


```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS is_successful IMPORTING option  TYPE string
                                          success TYPE abap_boolean
                                          out     TYPE REF TO  if_oo_adt_classrun_out.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "Options
    "Refer to the links available in the ABAP Doc comment of the CL_HTMLTIDY class.
    DATA(options_tab) = VALUE string_table(
      "Document display options
      ( `gnu-emacs` )
      ( `markup` )
      ( `mute` )
      ( `mute-id` )
      ( `quiet` )
      ( `show-body-only` )
      ( `show-errors` )
      ( `show-info` )
      ( `show-warnings` )
      "Document in and out options
      ( `add-meta-charset` )
      ( `add-xml-decl` )
      ( `add-xml-space` )
      ( `doctype` )
      ( `input-xml` )
      ( `output-html` )
      ( `output-xhtml` )
      ( `output-xml` )
      "File input output options
      ( `error-file` )
      ( `keep-time` )
      ( `output-file` )
      ( `write-back` )
      "Diagnostics options
      ( `accessibility-check` )
      ( `force-output` )
      ( `show-meta-change` )
      ( `warn-proprietary-attributes` )
      "Encoding options
      ( `char-encoding` )
      ( `input-encoding` )
      ( `newline` )
      ( `output-bom` )
      ( `output-encoding` )
      "Cleanup options
      ( `bare` )
      ( `clean` )
      ( `drop-empty-elements` )
      ( `drop-empty-paras` )
      ( `drop-proprietary-attributes` )
      ( `gdoc` )
      ( `logical-emphasis` )
      ( `merge-divs` )
      ( `merge-spans` )
      ( `word-2000` )
      "Entities options
      ( `ascii-chars` )
      ( `ncr` )
      ( `numeric-entities` )
      ( `preserve-entities` )
      ( `quote-ampersand` )
      ( `quote-marks` )
      ( `quote-nbsp` )
      "Repair options
      ( `alt-text` )
      ( `anchor-as-name` )
      ( `assume-xml-procins` )
      ( `coerce-endtags` )
      ( `css-prefix` )
      ( `custom-tags` )
      ( `enclose-block-text` )
      ( `enclose-text` )
      ( `escape-scripts` )
      ( `fix-backslash` )
      ( `fix-bad-comments` )
      ( `fix-style-tags` )
      ( `fix-uri` )
      ( `literal-attributes` )
      ( `lower-literals` )
      ( `repeated-attributes` )
      ( `skip-nested` )
      ( `strict-tags-attributes` )
      ( `uppercase-attributes` )
      ( `uppercase-tags` )
      "Transformation options
      ( `decorate-inferred-ul` )
      ( `escape-cdata` )
      ( `hide-comments` )
      ( `join-classes` )
      ( `join-styles` )
      ( `merge-emphasis` )
      ( `replace-color` )
      "Teaching tidy options
      ( `new-blocklevel-tags` )
      ( `new-empty-tags` )
      ( `new-inline-tags` )
      ( `new-pre-tags` )
      "Pretty print options
      ( `break-before-br` )
      ( `indent` )
      ( `indent-attributes` )
      ( `indent-cdata` )
      ( `indent-spaces` )
      ( `indent-with-tabs` )
      ( `keep-tabs` )
      ( `omit-optional-tags` )
      ( `priority-attributes` )
      ( `punctuation-wrap` )
      ( `sort-attributes` )
      ( `tab-size` )
      ( `tidy-mark` )
      ( `vertical-space` )
      ( `wrap` )
      ( `wrap-asp` )
      ( `wrap-attributes` )
      ( `wrap-jste` )
      ( `wrap-php` )
      ( `wrap-script-literals` )
      ( `wrap-sections` ) ).

**********************************************************************

    out->write( |******************************** Repairing and cleaning up an HTML document ********************************\n| ).

    "Creating an HTMLTidy instance
    DATA(htmltidy) = cl_htmltidy=>create( ).

**********************************************************************

    "Resetting all option settings to default values
    DATA success TYPE c LENGTH 1.
    htmltidy->reset_options( IMPORTING success = success ).

    out->write( |***************** Resetting options *****************\n| ).
    IF success IS INITIAL.
      out->write( `Resetting all option settings to default values not successful.` ).
    ELSE.
      out->write( `Resetting all option settings to default values successful.` ).
    ENDIF.
    out->write( |\n\n\n| ).

**********************************************************************

    "Getting default values of option settings

    "Internal table to visualize options and their values
    TYPES: BEGIN OF options_struc,
             option  TYPE string,
             value   TYPE string,
             success TYPE c LENGTH 1,
           END OF options_struc,
           tt_options TYPE TABLE OF options_struc WITH EMPTY KEY.

    DATA default_values TYPE tt_options.

    LOOP AT options_tab REFERENCE INTO DATA(opt).
      htmltidy->get_default(
        EXPORTING
          option  = opt->*
        IMPORTING
          value   = DATA(opt_value)
          success = success ).

      APPEND VALUE #( option = opt->* value = opt_value success = success ) TO default_values.
    ENDLOOP.

    "You might set a break point and check out the default values (if any).

**********************************************************************

    "Setting options explicitly
    "The following code demonstrates exemplary option settings.

    "Adding a met tag including the charset attribute
    htmltidy->set_option( EXPORTING option  = 'add-meta-charset'
                                    value   = 'yes'
                          IMPORTING success = success ).

    is_successful( option = `add-meta-charset` out = out success = success ).

    "Omitting the DOCTYPE declaration
    htmltidy->set_option( EXPORTING option  = 'doctype'
                                    value   = 'omit'
                          IMPORTING success = success ).

    is_successful( option = `doctype` out = out success = success ).

    "Indenting tags
    htmltidy->set_option( EXPORTING option  = 'indent'
                                    value   = 'auto'
                          IMPORTING success = success ).

    is_successful( option = `indent` out = out success = success ).

    "Avoiding an extra meta tag indicating that the code was cleaned up
    htmltidy->set_option( EXPORTING option  = 'tidy-mark'
                                    value   = 'no'
                          IMPORTING success = success ).

    is_successful( option = `tidy-mark` out = out success = success ).

    "Specifying the right margin for line wrapping
    htmltidy->set_option( EXPORTING option  = 'wrap'
                                    value   = '80'
                          IMPORTING success = success ).

    is_successful( option = `wrap` out = out success = success ).

    "Removing comments
    htmltidy->set_option( EXPORTING option  = 'hide-comments'
                                    value   = 'yes'
                          IMPORTING success = success ).

    is_successful( option = `hide-comments` out = out success = success ).

**********************************************************************

    "Getting set options
    DATA option_values TYPE tt_options.

    LOOP AT options_tab REFERENCE INTO opt.
      htmltidy->get_option(
        EXPORTING
          option  = opt->*
        IMPORTING
          value   = opt_value
          success = success ).

      APPEND VALUE #( option = opt->*
                      value = COND #( WHEN success IS INITIAL THEN `no value set` ELSE opt_value )
                      success = success ) TO option_values.
    ENDLOOP.

    out->write( |***************** Set options *****************\n\n| ).
    out->write( option_values ).
    out->write( |\n\n\n| ).

**********************************************************************

    "Demo HTML code to be repaired and cleaned up
    DATA(html) =
    `<!DOCTYPE html> <html>` && "DOCTYPE used
    `    ` && "Missing <head> tag
    `<title>Sample HTML Page    <title>` && "Wrong closing tag, / missing
    `       </head>        <body>` &&
    `<h1>Heading 1     </h5>` && "White spaces, Wrong </h5> closing tag
    `<p>          This is a paragraph.       </p>` && "White spaces
    ` <!-- Some comment --> ` && "Comment that will be hidden
    `                    <h2>Heading 2</h2>` &&
    `<p>Some text</>` && "Wrong closing tag, </p> tag is added and </> is transformed to &lt;/&gt;
    "Long text that will be wrapped
    `<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore ` &&
    `et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut ` &&
    `aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum ` &&
    `dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui ` &&
    `officia deserunt mollit anim id est laborum.</p>` &&
    `<nope><p>Some bullet points:</p></nope>  ` && "Invalid tags
    `<ul>   <li>abc` && "Missing closing li tag
    `        <li>def     </li>` && "White spaces
    `<li>ghi   ` && "Missing closing li tag
    `</u>   ` && "Wrong closing ul tag
    `<style> p {color:green;}</style>` && "style tag not in the HTML document's head tag
    ` </body>  ` &&
    `           `. "Missing </html> tag

    "Converting string -> xstring for the importing parameter of the repair method
    TRY.
        DATA(html_as_xstring) = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( html ).
      CATCH cx_sy_conversion_codepage INTO DATA(conversion_error).
        out->write( conversion_error->get_text( ) ).
        RETURN.
    ENDTRY.

    "Repairing and cleaning up HTML code
    htmltidy->repair(
      EXPORTING
        input              = html_as_xstring
        diagnostics        = 'X'
      IMPORTING
        output             = DATA(output)
        retcode            = DATA(retcode)
        errors             = DATA(errors)
        tidy_status        = DATA(tidy_status)
        html_version       = DATA(html_version)
        num_error          = DATA(num_error)
        num_warning        = DATA(num_warning)
        num_access_warning = DATA(num_access_warning)
        num_config_error   = DATA(num_config_error) ).

    out->write( |***************** Results of repairing and cleaning up HTML document *****************\n| ).
    out->write( |retcode: { retcode }| ).
    out->write( |tidy_status: { tidy_status }| ).
    out->write( |html_version: { html_version }| ).
    out->write( |num_error: { num_error }| ).
    out->write( |num_warning: { num_warning }| ).
    out->write( |num_access_warning: { num_access_warning }| ).
    out->write( |num_config_error: { num_config_error }| ).
    out->write( |\n\n\n| ).

    htmltidy->errtab(
      EXPORTING
        msgstr = errors
      IMPORTING
        msgtab = DATA(errtab) ).

    IF errtab IS NOT INITIAL.
      out->write( `Error table:` ).
      out->write( errtab ).
      out->write( |\n\n| ).
    ENDIF.

    "Converting the result (xstring -> string)
    DATA(html_output) = cl_abap_conv_codepage=>create_in( )->convert( output ).
    out->write( |output:\n| ).
    out->write( html_output ).

**********************************************************************

   out->write( |******************************** Repairing and cleaning up an XML document ********************************\n| ).

    "Demo XML
    DATA(xml) = cl_abap_conv_codepage=>create_out( )->convert(
          `    <node attr_a="123">    <subnode1> <hallo>hi              </hallo>` &&
          `</subnode1> <subnode2> <letter>a</letter> <date format="mm-dd-yyyy">01-01-2025</date>` &&
          `</subnode2>` &&
          `      <subnode3>`  &&
          `        <text             attr_b="1"               attr_c="a">abc                </text>` &&
          `    <text     attr_b="2" attr_c="b">def</text>` &&
          `<text attr_b="3"           attr_c="c">   ghi  </text>` &&
          `   <text attr_b="4" attr_c="d">jkl   </text>` &&
          `   </subnode3>` &&
          `</node>                ` ).

    DATA(format_xml) = cl_htmltidy=>create( ).
    "Resetting all option settings to default values
    format_xml->reset_options( IMPORTING success = success ).

    IF success IS INITIAL.
      out->write( `Resetting all option settings to default values not successful.` ).
    ENDIF.

    "Setting options explicitly
    "The following code demonstrates exemplary option settings, including settings
    "particularly for XML documents.

    "Using XML parser
    format_xml->set_option( EXPORTING option  = 'input-xml'
                                      value   = 'yes'
                            IMPORTING success = success ).

    is_successful( option = `input-xml` out = out success = success ).

    "Writing well-formed XML
    format_xml->set_option( EXPORTING option  = 'output-xml'
                                      value   = 'yes'
                            IMPORTING success = success ).

    is_successful( option = `output-xml` out = out success = success ).

    "Adding XML declaration
    format_xml->set_option( EXPORTING option  = 'add-xml-decl'
                                      value   = 'yes'
                            IMPORTING success = success ).

    is_successful( option = `add-xml-decl` out = out success = success ).

    format_xml->set_option( EXPORTING option  = 'indent'
                                      value   = 'auto'
                            IMPORTING success = success ).

    is_successful( option = `indent` out = out success = success ).

    format_xml->repair(
       EXPORTING
         input              = xml
         diagnostics        = 'X'
       IMPORTING
         output             = DATA(xml_output)
         retcode            = DATA(xml_retcode)
         errors             = DATA(xml_errors)
         tidy_status        = DATA(xml_tidy_status)
         num_error          = DATA(xml_num_error)
         num_warning        = DATA(xml_num_warning)
         num_access_warning = DATA(xml_num_access_warning)
         num_config_error   = DATA(xml_num_config_error) ).

    out->write( |xml_retcode: { xml_retcode }| ).
    out->write( |xml_tidy_status: { xml_tidy_status }| ).
    out->write( |xml_num_error: { xml_num_error }| ).
    out->write( |xml_num_warning: { xml_num_warning }| ).
    out->write( |xml_num_access_warning: { xml_num_access_warning }| ).
    out->write( |xml_num_config_error: { xml_num_config_error }| ).
    out->write( |\n\n| ).

    format_xml->errtab(
      EXPORTING
        msgstr = xml_errors
      IMPORTING
        msgtab = errtab ).

    IF errtab IS NOT INITIAL.
      out->write( `Error table:` ).
      out->write( errtab ).
      out->write( |\n\n| ).
    ENDIF.

    out->write( |output:\n| ).

    DATA(formatted_xml_output) = cl_abap_conv_codepage=>create_in( )->convert( xml_output ).
    out->write( formatted_xml_output ).

  ENDMETHOD.

  METHOD is_successful.
    IF success IS INITIAL.
      out->write( |Option setting for "{ option }" not successful.| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 

</details>  

</td>
</tr>
</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Creating and Using IDE Actions


- Miscellaneous interfaces such as `IF_AIA_ACTION` and classes (which are not detailed out here, and some of which are used by the demo examples) are usable in the context of IDE actions.
- IDE actions let you create simple features to extend ABAP development tools for Eclipse (ADT).
- With IDE actions, you can build custom functionalities using ABAP, especially for AI-related use cases.

> [!NOTE]  
> - Find more information on IDE actions and examples: 
>   - [in the SAP documentation](https://help.sap.com/docs/abap-cloud/abap-development-tools-user-guide/working-with-ide-actions). 
>   - [in this Devtoberfest session](https://youtu.be/YOIsyR5C2Mk?t=2304).
> - Your user must be assigned to the `SAP_A4C_BC_DEV_AIA_PC` business catalog. 

> [!IMPORTANT]   
> - The examples are simplified and only intended for [exploration, experimentation, and demonstration](./README.md#%EF%B8%8F-disclaimer), to get a high-level idea. They do not represent best practices or role model approaches for IDE action implementation.
> - The examples use `CL_DEMO_OUTPUT_CLOUD` to retrieve HTML output of data objects. Note that this class is intended for demo purposes only. The examples use the class's HTML capabilities for display. When you execute the example IDE actions, they present data objects in HTML within the IDE action result dialog. You may recognize this HTML presentation from the `CL_DEMO_OUTPUT` class that is available in Standard ABAP and used for demo display purposes, especially useful for displaying internal table content.
> - The use of IDE actions is your responsibility. Development and use are up to you. Refer to [this repository's disclaimer](./README.md#%EF%B8%8F-disclaimer) and the [disclaimer in the IDE action documentation](https://help.sap.com/docs/abap-cloud/abap-development-tools-user-guide/working-with-ide-actions). 
> - Be particularly mindful of the potential security risks when importing external content, which the simplified example code below does not address. The example code also uses dynamic programming techniques. They can pose a security risk. Refer to the [Dynamic Programming cheat sheet](06_Dynamic_Programming.md) and the ABAP Keyword Documentation.


The (non-AI-related) experiments include these IDE actions for demo purposes: 

<table>

<tr>
<th> IDE Action </th> <th> Notes </th>
</tr>

<tr>
<td> 

**File uploader and downloader** 

 </td>

 <td> 

- The demo IDE action allows for uploading and downloading files.
- The example includes the upload and processing of JSON and XLSX file content, as well as options to download and display XML, JSON, TXT, and HTML content.
- Find more notes in the code examples.

 </td>
</tr>

<tr>
<td> 

**Class runner and data object displayer** 

 </td>

 <td> 

- Similar to implementing the `IF_OO_ADT_CLASSRUN` interface (or its alternatives for demo display purposes), the demo IDE action lets you select and run a class that implements a specific demo interface.
- The IDE action result dialog uses HTML to display the content of data objects, like elementary data objects, structures, internal tables, and references, with the help of `CL_DEMO_OUTPUT_CLOUD`, instead of plain text in the ADT console.

 </td>
</tr>

<tr>
<td> 

**Method runner and parameter displayer**

 </td>

 <td> 

- The demo IDE action lets you select a usable and instantiable class. Once selected, you can choose a public instance or static method of that class to be called.
- After choosing a method, the IDE action dialog presents its input parameters (if any), allowing you to provide actual parameters for the formal parameters. Note that the example is simplified, only allowing character-like input. For example, a calculator method may require three inputs: two numbers and an operator. The example focuses on basic cases and does not perform input validation at runtime.
- When you run the IDE action, the method is executed. For instance methods, an instance of the class is created.
- As a result, the parameter table (refer to the Dynamic Programming cheat sheet) is displayed as HTML in the IDE action result dialog using `CL_DEMO_OUTPUT_CLOUD`, including any the input and output parameters and their values (if any).

 </td>
</tr>

<tr>
<td> 

**XLSX file importer and content displayer**

 </td>

 <td> 

- The demo IDE action lets you select an XLSX file from your local machine, retrieve its content, and display it in an internal table as HTML using `CL_DEMO_OUTPUT_CLOUD` in the IDE action result dialog.  
- In the selection IDE action dialog, you can specify which worksheet contains the content to retrieve. You can also choose where to start retrieving the content (e.g., from column A, row 1) and indicate if there is a header line. If a header exists, the content of that cells in that line are used as field names for the internal table.  
- When you run the IDE action, the XCO API is used to read the XLSX content. An internal table is created dynamically, and the worksheet content is added to it. Finally, this internal table is displayed as HTML in the IDE action result dialog.  

 </td>
</tr>

</table>


The class setup is similar across all examples:  
- Creation of repository objects for demo IDE actions.
- Each demo IDE action involves two global classes:
  - Implementing class
  - Input UI configuration class (some of which include implementation in the CCIMP include/Local types tab in ADT)
- A demo class to demonstrate the IDE action (except for the *XLSX file importer and content displayer* example; the examples use a demo class named `ZCL_DEMO_ABAP`)
- Some IDE action classes contain code within the CCIMP include.
- Some examples require the creation of a demo interface.

Expand the following collapsible sections for descriptions and example code:

<details>
  <summary>🟢 1) File uploader and downloader </summary>
  <!-- -->
<br>

**Demo Purpose** 

This demo IDE action lets you upload and process files, as well as download them. It showcases the upload and processing of JSON and XLSX files, along with download and display options for XML, JSON, TXT, and HTML content.

**Setup Steps**

1. Create IDE action
   - Access your demo package in ADT in your SAP BTP ABAP Environment.
   - Create a new repository object e.g. by right-clicking the package and choosing *New -> Other ABAP Repository Object -> Filter for IDE Action*.
   - Action creation: 
     - Name: *ZDEMO_ABAP_FILE_IMPORT_EXPORT*
     - Description: *File importer and exporter*
   - Make the following entries on the IDE action screen: 
     - Title: *Demo IDE Action: Import/Export*
     - Summary: *Action to upload and download files* 
     - Implementing Class: `ZCL_DEMO_ABAP_IDE_IMP_EXP_IMPL`
     - Input UI Configuration Class: `ZCL_DEMO_ABAP_IDE_IMP_EXP_UI`
     - Number of Focused Resources: *Any*
     - Filter Object Types: `CLAS` (choose *Add* and filter for `CLAS`)
2. Create the IDE action classes (implementing and input UI configuration class)
   - Choose the *Implementing Class* link. A pop-up is displayed to start the class creation. Provide a description and walk through the wizard. Once the class has been created, activate the class having no implementation at this stage. 
   - Go back and repeat the steps for the *Input UI Configuration Class*.
   - Once the classes have been created, activate the IDE action. The screen will show warnings regarding interfaces not implemented in the classes. 
   - For demo purposes, you can copy and paste the code from below. Note that the `ZCL_DEMO_ABAP_IDE_IMP_EXP_UI` class also requires code in the CCIMP include/Local Types tab in ADT.   
3. Create interface
     - Create an interface named `ZIF_DEMO_ABAP_IDE_IMP_EXP` and use the code below. Implementing `ZIF_DEMO_ABAP_IDE_IMP_EXP` is essential for checking out the IDE action. 
     - This interface defines the methods `process_import` and `export`, which include custom logic to process content for upload and download. For example, the example demonstrates uploading an XLSX file. The `process_import` method implementation in the `ZCL_DEMO_ABAP` class is specifically tailored for this example, allowing XLSX content to be uploaded and processed using the XLSX XCO API. The `export` method is designed to provide content for download based on the selected operation in the IDE action dialog. 
4. Create a class that implements the demo logic for uploading, downloading, and processing file content
    - Create a class named `ZCL_DEMO_ABAP` and use the code provided below. 
    - Note: The examples use a demo database table from the ABAP cheat sheet repository. If you have not imported the repository, create the `ZDEMO_ABAP_CARR` database table by reusing the code from the repository (see below). You can run this class using F9 in ADT to initialize the demo database table and add entries for testing. Refer to the comments in the class code, as you need to comment out a section of the code beforehand.
      ```abap
      @EndUserText.label : 'Demo table: Airline'
      @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
      @AbapCatalog.tableCategory : #TRANSPARENT
      @AbapCatalog.deliveryClass : #A
      @AbapCatalog.dataMaintenance : #RESTRICTED
      define table zdemo_abap_carr {

          key mandt  : abap.clnt not null;
          key carrid : abap.char(3) not null;
          carrname   : abap.char(20) not null;
          currcode   : abap.cuky not null;
          url        : abap.char(255) not null;

      }
      ```

5. Perform a mass activation to activate all artifacts.
6. The demo includes uploading JSON and XLSX files. Ensure you have the necessary files and content to execute the demo according to the implementations in the `zif_demo_abap_ide_imp_exp~process_import` method of the `ZCL_DEMO_ABAP` class:      
   - Create a JSON file on your local machine, e.g. `carriers.json`, and insert the following content: 
      ```json
      [
        {
          "MANDT": "000",
          "CARRID": "ZZ",
          "CARRNAME": "ZZ Airlines",
          "CURRCODE": "USD",
          "URL": "some_url_zz_airlines"
        },
        {
          "MANDT": "000",
          "CARRID": "YY",
          "CARRNAME": "YY Airways",
          "CURRCODE": "EUR",
          "URL": "some_url_yy_airways"
        },
        {
          "MANDT": "000",
          "CARRID": "XX",
          "CARRNAME": "Air XX",
          "CURRCODE": "JPY",
          "URL": "some_url_air_xx"
        }
      ]
      ```
   - Create an XLSX file on your local machine, e.g. `carriers.xlsx`, and insert the following content: 

      <table border="1">
        <tr>
          <td>100</td>
          <td>AB</td>
          <td>AB Airlines</td>
          <td>EUR</td>
          <td>some_url_ab_airlines</td>
        </tr>
        <tr>
          <td>100</td>
          <td>CD</td>
          <td>CD Airways</td>
          <td>USD</td>
          <td>some_url_cd_airways</td>
        </tr>
        <tr>
          <td>100</td>
          <td>EF</td>
          <td>Air EF</td>
          <td>JPY</td>
          <td>some_url_air_ef</td>
        </tr>
      </table>

<br>

**Demo Execution Steps**

- In ADT, you have opened any class.
- Choose *STR + ALT/CMD + R* to run an IDE action.
- Select the IDE action *Demo IDE Action: Import/Export*. You can filter for *Export*.
- Choose *Run* (or double-click the IDE action).
- An IDE action dialog is displayed prompting you to provide a class. Insert the demo class `ZCL_DEMO_ABAP` (or browse). The demo expects a class to be specified that implements the `ZIF_DEMO_ABAP_IDE_IMP_EXP` interface.
- Checking out the demo **upload** functionality: 
  - Select *Upload* from the *Operation* dropdown list.
  - In the *File Import from Path* input field, select a file from your local machine. You can browse for it.
  - **Example 1** (JSON content upload): 
    - It is assumed that you have created a JSON file (e.g., `carriers.json`) as described above. 
    - Ensure you comment in the code for processing the JSON content in the `zif_demo_abap_ide_imp_exp~process_import` method of the `ZCL_DEMO_ABAP` class. Also, comment out the code for XLSX content processing.
    - Choose *Run*. The demo implementation includes deserializing the imported JSON content. The processing is demonstrated by adding the contained datasets to the demo database table. An IDE action popup will display a message based on the `message` parameter.
  - **Example 2** (XLSX content upload): 
    - Similar to example 1, it is assumed that you have created an XLSX file (e.g., `carriers.xlsx`) as described above. 
    - Ensure you comment in the code for processing the XLSX content in the `zif_demo_abap_ide_imp_exp~process_import` method of the `ZCL_DEMO_ABAP` class. Also, comment out the code for JSON content processing.
    - Choose *Run*. The demo implementation processes the XLSX content using the XLSX XCO API. Similar to the JSON example, a demo database table is updated based on the XLSX file content, and an IDE action popup will display a message.
- Checking out the demo **display and download** functionality: 
  - The demo includes the display and download of XML, JSON, TXT and HTML content. The *Operation* dropdown list includes the respective selection options.
  - When you select a specific download option, the `zif_demo_abap_ide_imp_exp~export` method of the `ZCL_DEMO_ABAP` class is called. Depending on the selection, demo code is executed. This is realized by an enumerated type, defined in the interface. An importing parameter is typed with this enumerated type. Its value determines what code is executed, realized by a `CASE` statement.
    - **XML** (asXML): Database table entries are retrieved using a `SELECT` statement. Using a `CALL TRANSFORMATION` statement, the internal table content is transformed (to asXML) and returned.
    - **JSON**: Database table entries are retrieved using a `SELECT` statement. Using the `/UI2/CL_JSON` class, the internal table content is converted to JSON and returned.
    - **TXT**: A simple string, representing the content of a TXT file, is returned.
    - **HTML**: Database table entries are retrieved using a `SELECT` statement. Using the `CL_DEMO_OUTPUT_CLOUD` class, the internal table content is converted to HTML and returned.
  - You can use the *Export...* button to download the appropriate files. The file extension is predefined; for example, XML downloads use `*.xml`. 
    - The IDE action is designed so that the *Run* functionality cannot be selected for download; it is only available for checking the upload functionality.
    - The HTML content is directly displayed in the IDE action dialog. You can download the file by right-clicking the displayed HTML and selecting the option to save it to your local machine.

<table>

<tr>
<th> Class (include)/Interface </th> <th> Code </th>
</tr>

<tr>
<td> 

`ZCL_DEMO_ABAP_IDE_IMP_EXP_IMPL` (global class; no code in other includes)

 </td>

 <td> 


``` abap
CLASS zcl_demo_abap_ide_imp_exp_impl DEFINITION
 PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_aia_action.

    CLASS-METHODS check_provided_class
      IMPORTING cl_name TYPE string
      EXPORTING result  TYPE string
                is_ok   TYPE abap_boolean.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS create_popup
      IMPORTING input         TYPE string
      RETURNING VALUE(result) TYPE REF TO if_aia_action_result.
ENDCLASS.



CLASS zcl_demo_abap_ide_imp_exp_impl IMPLEMENTATION.
  METHOD if_aia_action~run.
    TRY.
        DATA ui_input TYPE zcl_demo_abap_ide_imp_exp_ui=>ty_input.
        context->get_input_config_content( )->get_as_structure( IMPORTING result = ui_input ).
      CATCH cx_sd_invalid_data INTO DATA(exception).
        CLEAR ui_input.
        result = create_popup( `Initial input data. Check the implementation.` ).
        RETURN.
    ENDTRY.

    "Note: The following code is for the import scenario only.

    "Checking the provided class
    DATA(cl) = to_upper( condense( val = ui_input-class_impl_intf to = `` ) ).

    zcl_demo_abap_ide_imp_exp_impl=>check_provided_class( EXPORTING cl_name = cl
                                                          IMPORTING result  = DATA(cl_res)
                                                                    is_ok   = DATA(ok) ).

    IF ok = abap_true.
      DATA xstr TYPE xstring.
      DATA str TYPE string.
      DATA msg TYPE string.

      TRY.
          DATA(decoded_content) = cl_web_http_utility=>decode_x_base64( ui_input-path_text_field ).

          CALL METHOD (cl)=>zif_demo_abap_ide_imp_exp~process_import
            EXPORTING imported_content = decoded_content
                      str              = ui_input-path_text_field
            IMPORTING content_xstring  = xstr
                      content_string   = str
                      message          = msg.

          "Here; only a message is output.
          result = create_popup( msg ).
        CATCH cx_root into data(err).
          result = create_popup( `ERROR: Method call error with zif_demo_abap_ide_imp_exp~process_import. ` && err->get_text( ) ).
      ENDTRY.
    ELSE.
      result = create_popup( `ERROR: The provided class is not suitable for the IDE Action demo.` ).
    ENDIF.
  ENDMETHOD.

  METHOD check_provided_class.

    cl_abap_typedescr=>describe_by_name( EXPORTING p_name = cl_name
                                         RECEIVING p_descr_ref = DATA(tdo_type)
                                         EXCEPTIONS type_not_found = 4 ).

    IF sy-subrc <> 0.
      result = |Class { cl_name } does not exist.|.
    ELSE.
      TRY.
          DATA(tdo_cl) = CAST cl_abap_classdescr( tdo_type ).
          DATA(interfaces_cl) = tdo_cl->interfaces.

          IF NOT line_exists( interfaces_cl[ name = 'ZIF_DEMO_ABAP_IDE_IMP_EXP' ] ).
            result = |Class { cl_name } does not implement interface ZIF_DEMO_ABAP_IDE_IMP_EXP.|.
          ELSE.
            is_ok = abap_true.
          ENDIF.
        CATCH cx_root INTO DATA(err).
          result = err->get_text( ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD create_popup.
    DATA(popup_result) = cl_aia_result_factory=>create_html_popup_result( ).
    popup_result->set_content( input ).
    result = popup_result.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

`ZCL_DEMO_ABAP_IDE_IMP_EXP_UI` (global class)

 </td>

 <td> 


``` abap
CLASS zcl_demo_abap_ide_imp_exp_ui DEFINITION
 PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_aia_sd_action_input.

    CONSTANTS:
      BEGIN OF co_combo_box_operation,
        "! <p class="shorttext">Upload</p>
        upload       TYPE string VALUE `UPLOAD`,
        "! <p class="shorttext">Download XML</p>
        downloadxml  TYPE string VALUE `DOWNLOADXML`,
        "! <p class="shorttext">Download JSON</p>
        downloadjson TYPE string VALUE `DOWNLOADJSON`,
        "! <p class="shorttext">Download TXT</p>
        downloadtxt  TYPE string VALUE `DOWNLOADTXT`,
        "! <p class="shorttext">Display HTML</p>
        displayhtml  TYPE string VALUE `DISPLAYHTML`,
      END OF co_combo_box_operation.

    TYPES:
      "! <p class="shorttext">Make entries for uploading or downloading content:</p>
      BEGIN OF ty_input,
        "! <p class="shorttext">Class Implementing the ZIF_DEMO_ABAP_IDE_IMP_EXP Interface</p>
        "! $required
        class_impl_intf        TYPE string,
        "! $values { @link zcl_demo_abap_ide_imp_exp_ui.data:co_combo_box_operation }
        "! $default { @link zcl_demo_abap_ide_imp_exp_ui.data:co_combo_box_operation.upload }
        "! <p class="shorttext">Operation</p>
        ty_combo_box_operation TYPE string,
        "! <p class="shorttext">Note</p>
        note                   TYPE string,
        "! <p class="shorttext">File Import from Path</p>
        "! $contentEncoding 'base64'
        "! $required
        path_text_field        TYPE string,
        "! <p class="shorttext">XML Download</p>
        "! $contentMediaType 'application/xml'
        "! $contentEncoding 'base64'
        xmldownload            TYPE xstring,
        "! <p class="shorttext">JSON Download</p>
        "! $contentMediaType 'application/json'
        "! $contentEncoding 'base64'
        jsondownload           TYPE xstring,
        "! <p class="shorttext">TXT Download</p>
        "! $contentMediaType 'text/plain'
        txtdownload            TYPE string,
        "! <p class="shorttext">HTML Display</p>
        "! $contentMediaType 'text/html'
        htmldisplay            TYPE string,
      END OF ty_input.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_ide_imp_exp_ui IMPLEMENTATION.
  METHOD if_aia_sd_action_input~create_input_config.
    TRY.
        DATA(resource) = CAST if_adt_context_src_based_obj( context->get_focused_resource( ) ).
        DATA(focused_resources) = context->get_focused_resources( ).
        LOOP AT focused_resources ASSIGNING FIELD-SYMBOL(<focused_resource>).
          IF <focused_resource>->get_kind( ) = if_adt_context_resource=>kind-source_based_dev_object.
            DATA(source_based_object) = CAST if_adt_context_src_based_obj( <focused_resource> ).
            DATA(cl) = source_based_object->get_object_info( )-display_name.
            DATA(type) = <focused_resource>->get_type( ).
            IF type = `CLAS/OC`.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
    ENDTRY.

    "Default values for input data
    DATA(input_data) = VALUE ty_input( ty_combo_box_operation = `upload`
                                       note = `Provide a class` ).

    DATA(configuration) = ui_information_factory->get_configuration_factory( )->create_for_data( input_data ).

    configuration->get_element( 'path_text_field' )->set_file_properties( kind          = if_sd_config_element=>file_properties_kind-path
                                                                          transfer_mode = VALUE #( import = abap_true ) ).

    configuration->get_element( 'xmldownload' )->set_file_properties( kind           = if_sd_config_element=>file_properties_kind-content
                                                                      transfer_mode  = VALUE #( export = abap_true )
                                              )->set_multiline( if_sd_config_element=>height-large
                                              )->set_read_only( abap_true ).

    configuration->get_element( 'jsondownload' )->set_file_properties( kind           = if_sd_config_element=>file_properties_kind-content
                                                                       transfer_mode  = VALUE #( export = abap_true )
                                               )->set_multiline( if_sd_config_element=>height-large
                                               )->set_read_only( abap_true ).

    configuration->get_element( 'txtdownload' )->set_file_properties( kind           = if_sd_config_element=>file_properties_kind-content
                                                                      transfer_mode  = VALUE #( export = abap_true )
                                              )->set_multiline( if_sd_config_element=>height-large
                                              )->set_read_only( abap_true ).

    configuration->get_element( 'htmldisplay' )->set_file_properties( kind           = if_sd_config_element=>file_properties_kind-content
                                                                      transfer_mode  = VALUE #( export = abap_true )
                                              )->set_multiline( if_sd_config_element=>height-large
                                              )->set_read_only( abap_true ).

    configuration->get_element( 'note' )->set_read_only( abap_true ).

    configuration->get_element( 'class_impl_intf' )->set_types( VALUE #( ( `CLAS/OC` ) ) ).

    "Setting side effects
    configuration->get_element( 'class_impl_intf' )->set_sideeffect( after_update    = abap_true
                                                                     feature_control = abap_true ).

    configuration->get_element( 'ty_combo_box_operation' )->set_sideeffect( after_update = abap_true  ).

    configuration->get_element( 'path_text_field' )->set_sideeffect( after_update    = abap_true
                                                                     feature_control = abap_true ).

    configuration->get_element( 'note' )->set_sideeffect( feature_control = abap_true ).

    result = ui_information_factory->for_abap_type( abap_type     = input_data
                                                    configuration = configuration ).

  ENDMETHOD.

  METHOD if_aia_sd_action_input~get_value_help_provider.
    "Getting a reference to the value help handler via the value help provider
    "The example uses a local class as value help provider. The method implementation
    "there is intentionally empty. The value help provider is required in the example
    "for the class selection popup to display.
    result = cl_sd_value_help_provider=>create( NEW lcl_demo_abap_action_impexp_vh( ) ).
  ENDMETHOD.

  METHOD if_aia_sd_action_input~get_side_effect_provider.
    "Getting a reference to the side effect handler via the side effect provider
    "The example uses a local class as side effect provider.
    result = cl_sd_sideeffect_provider=>create( feature_control = NEW lcl_demo_abap_action_impexp_se( )
                                                determination   = NEW lcl_demo_abap_action_impexp_se( ) ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

`ZCL_DEMO_ABAP_IDE_IMP_EXP_UI` (CCIMP include/Local Types tab in ADT)

 </td>

 <td> 


``` abap
*&---------------------------------------------------------------------*
*& Value help handler
*&---------------------------------------------------------------------*

CLASS lcl_demo_abap_action_impexp_vh DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_sd_value_help_dsni.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_demo_abap_action_impexp_vh IMPLEMENTATION.
  METHOD if_sd_value_help_dsni~get_value_help_items.
    "The method implementation is intentionally empty.
  ENDMETHOD.
ENDCLASS.


*&---------------------------------------------------------------------*
*& Side effect handler
*&---------------------------------------------------------------------*

CLASS lcl_demo_abap_action_impexp_se DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      if_sd_determination,
      if_sd_feature_control.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS call_export_method
      IMPORTING operation  TYPE string
                class_name TYPE string
      CHANGING  input      TYPE zcl_demo_abap_ide_imp_exp_ui=>ty_input.
ENDCLASS.

CLASS lcl_demo_abap_action_impexp_se IMPLEMENTATION.

  METHOD if_sd_determination~run.
    IF context IS INSTANCE OF cl_aia_sd_context.
      FINAL(action_context) = CAST cl_aia_sd_context( context )->get_ide_action_context( ).
    ENDIF.

    "Retrieving the model
    DATA(input_data) = VALUE zcl_demo_abap_ide_imp_exp_ui=>ty_input( ).
    model->get_as_structure( IMPORTING result = input_data ).

    IF ( id = 'CLASS_IMPL_INTF' AND determination_kind = if_sd_determination=>kind-after_update )
    OR ( id = 'TY_COMBO_BOX_OPERATION' AND determination_kind = if_sd_determination=>kind-after_update ).
      TRY.
          DATA(cl) = to_upper( condense( val = input_data-class_impl_intf to = `` ) ).
          zcl_demo_abap_ide_imp_exp_impl=>check_provided_class( EXPORTING cl_name = cl
                                                                IMPORTING result  = DATA(res)
                                                                          is_ok = DATA(ok) ).

          IF ok = abap_false.
            IF input_data-class_impl_intf IS INITIAL.
              input_data-note = |NOTE: Insert a class in the input field.|.
            ELSE.
              input_data-note = |NOTE: Class { input_data-class_impl_intf } does not implement interface ZIF_DEMO_ABAP_IDE_IMP_EXP.|.
            ENDIF.
            CLEAR input_data-txtdownload.
            CLEAR input_data-jsondownload.
            CLEAR input_data-xmldownload.
            CLEAR input_data-htmldisplay.
          ELSE.
            IF input_data-ty_combo_box_operation = `upload`.
              input_data-note = |Upload option selected. Provide the file path and choose Run.|.
            ELSE.
              call_export_method( EXPORTING operation  = input_data-ty_combo_box_operation
                                            class_name = cl
                                  CHANGING  input      = input_data ).
            ENDIF.
          ENDIF.
        CATCH cx_root INTO DATA(e).
          input_data-note = |NOTE: { e->get_text( ) }|.
          CLEAR input_data-txtdownload.
          CLEAR input_data-jsondownload.
          CLEAR input_data-xmldownload.
          CLEAR input_data-htmldisplay.
      ENDTRY.
    ENDIF.

    "Returning the changed model
    result = input_data.
  ENDMETHOD.

  METHOD if_sd_feature_control~run.
    "Retrieving the model
    DATA(input_data) = VALUE zcl_demo_abap_ide_imp_exp_ui=>ty_input( ).
    model->get_as_structure( IMPORTING result = input_data ).

    "Creating the initial feature control
    DATA(feature_control) = feature_control_factory->create_for_data( input_data ).

    IF feature_control_kind = if_sd_feature_control=>kind-on_input.
      feature_control->get_element( 'XMLDOWNLOAD' )->set_hidden( abap_true ).
      feature_control->get_element( 'JSONDOWNLOAD' )->set_hidden( abap_true ).
      feature_control->get_element( 'TXTDOWNLOAD' )->set_hidden( abap_true ).
      feature_control->get_element( 'HTMLDISPLAY' )->set_hidden( abap_true ).
    ENDIF.

    IF feature_control_kind = if_sd_feature_control=>kind-on_update.
      CASE input_data-ty_combo_box_operation.
        WHEN `upload`.
          IF input_data-note CS `NOTE:`.
            feature_control->get_element( 'PATH_TEXT_FIELD' )->set_hidden( abap_true ).
            feature_control->get_element( 'XMLDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'JSONDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'TXTDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'HTMLDISPLAY' )->set_hidden( abap_true ).
          ELSE.
            feature_control->get_element( 'PATH_TEXT_FIELD' )->set_hidden( abap_false  ).
            feature_control->get_element( 'XMLDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'JSONDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'TXTDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'HTMLDISPLAY' )->set_hidden( abap_true ).
          ENDIF.
        WHEN `downloadxml`.
          IF input_data-note CS `NOTE:`.
            feature_control->get_element( 'PATH_TEXT_FIELD' )->set_hidden( abap_true ).
            feature_control->get_element( 'XMLDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'JSONDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'TXTDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'HTMLDISPLAY' )->set_hidden( abap_true ).
          ELSE.
            feature_control->get_element( 'PATH_TEXT_FIELD' )->set_hidden( abap_true  ).
            feature_control->get_element( 'XMLDOWNLOAD' )->set_hidden( abap_false ).
            feature_control->get_element( 'JSONDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'TXTDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'HTMLDISPLAY' )->set_hidden( abap_true ).
          ENDIF.
        WHEN `downloadjson`.
          IF input_data-note CS `NOTE:`.
            feature_control->get_element( 'PATH_TEXT_FIELD' )->set_hidden( abap_true ).
            feature_control->get_element( 'XMLDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'JSONDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'TXTDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'HTMLDISPLAY' )->set_hidden( abap_true ).
          ELSE.
            feature_control->get_element( 'PATH_TEXT_FIELD' )->set_hidden( abap_true  ).
            feature_control->get_element( 'XMLDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'JSONDOWNLOAD' )->set_hidden( abap_false ).
            feature_control->get_element( 'TXTDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'HTMLDISPLAY' )->set_hidden( abap_true ).
          ENDIF.
        WHEN `downloadtxt`.
          IF input_data-note CS `NOTE:`.
            feature_control->get_element( 'PATH_TEXT_FIELD' )->set_hidden( abap_true ).
            feature_control->get_element( 'XMLDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'JSONDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'TXTDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'HTMLDISPLAY' )->set_hidden( abap_true ).
          ELSE.
            feature_control->get_element( 'PATH_TEXT_FIELD' )->set_hidden( abap_true ).
            feature_control->get_element( 'XMLDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'JSONDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'TXTDOWNLOAD' )->set_hidden( abap_false ).
            feature_control->get_element( 'HTMLDISPLAY' )->set_hidden( abap_true ).
          ENDIF.
        WHEN `displayhtml`.
          IF input_data-note CS `NOTE:`.
            feature_control->get_element( 'PATH_TEXT_FIELD' )->set_hidden( abap_true ).
            feature_control->get_element( 'XMLDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'JSONDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'TXTDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'HTMLDISPLAY' )->set_hidden( abap_true ).
          ELSE.
            feature_control->get_element( 'PATH_TEXT_FIELD' )->set_hidden( abap_true  ).
            feature_control->get_element( 'XMLDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'JSONDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'TXTDOWNLOAD' )->set_hidden( abap_true ).
            feature_control->get_element( 'HTMLDISPLAY' )->set_hidden( abap_false ).
          ENDIF.
      ENDCASE.
    ENDIF.

    "Returning the feature control
    result = feature_control.
  ENDMETHOD.

  METHOD call_export_method.

    DATA xstr TYPE xstring.
    DATA msg TYPE string.
    DATA str TYPE string.

    TRY.
        CALL METHOD (class_name)=>zif_demo_abap_ide_imp_exp~export
          EXPORTING
            proc_kind      = SWITCH zif_demo_abap_ide_imp_exp=>proc_kind( input-ty_combo_box_operation
                                                                          WHEN `downloadxml` THEN zif_demo_abap_ide_imp_exp=>xml
                                                                          WHEN `downloadjson` THEN zif_demo_abap_ide_imp_exp=>json
                                                                          WHEN `downloadtxt` THEN zif_demo_abap_ide_imp_exp=>txt
                                                                          WHEN `displayhtml` THEN zif_demo_abap_ide_imp_exp=>html )
          IMPORTING
            content        = xstr
            content_string = str
            message        = msg.
      CATCH cx_root INTO DATA(err).
        input-note = |ERROR: Method call error with zif_demo_abap_ide_imp_exp~export. { err->get_text( ) }|.
        RETURN.
    ENDTRY.

    IF xstr IS INITIAL AND str IS INITIAL.
      input-note = |Initial result when calling the EXPORT method.|.
      CLEAR input-txtdownload.
      CLEAR input-jsondownload.
      CLEAR input-xmldownload.
      CLEAR input-htmldisplay.
    ELSE.
      CASE input-ty_combo_box_operation.
        WHEN `downloadxml`.
          input-xmldownload = xstr.
        WHEN `downloadjson`.
          input-jsondownload = xstr.
        WHEN `downloadtxt`.
          input-txtdownload = str.
        WHEN `displayhtml`.
          input-htmldisplay = str.
      ENDCASE.
      input-note = |Download/Display option selected. No 'Run' functionality.|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

Interface to check out the IDE action<br><br>
`ZIF_DEMO_ABAP_IDE_IMP_EXP` 

 </td>

 <td> 


``` abap
INTERFACE zif_demo_abap_ide_imp_exp
  PUBLIC .

  TYPES: BEGIN OF ENUM proc_kind,
           xml,
           json,
           txt,
           html,
         END OF ENUM proc_kind.

  CLASS-METHODS: process_import IMPORTING imported_content TYPE xstring OPTIONAL
                                          str              TYPE string OPTIONAL
                                EXPORTING content_xstring  TYPE xstring
                                          content_string   TYPE string
                                          message          TYPE string,

    export IMPORTING proc_kind      TYPE zif_demo_abap_ide_imp_exp=>proc_kind
           EXPORTING content        TYPE xstring
                     content_string TYPE string
                     message        TYPE string.
ENDINTERFACE.
``` 

 </td>
</tr>

<tr>
<td> 

Class to check out the IDE action<br><br>
`ZCL_DEMO_ABAP` (global class) 

 </td>

 <td> 


``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_demo_abap_ide_imp_exp,
      if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.

  METHOD zif_demo_abap_ide_imp_exp~export.

*&---------------------------------------------------------------------*
*& NOTE
*&
*& Here goes logic to prepare file download / HTML content display.
*& The method includes an enumerated type as importing parameter, based
*& on which demo implementations are executed.
*&
*& The examples use a demo database table of the ABAP cheat sheet
*& repository. If you have not imported the repository, you can create
*& the zdemo_abap_carr database table by reusing the code from the
*& repository. You can then run this class using F9 in ADT to initialize
*& the demo database table, and add some entries to have data to work with.
*&
*& This demo example covers these use cases:
*& - XML download (asXML created by transforming internal table content to
*&   XML)
*& - JSON download (JSON created by converting internal table content to
*&   JSON
*& - TXT download (the content of the file is represented by a simple
*&   string)
*& - HTML display/file download (using the cl_demo_output_cloud class -
*&   which is for demo purposes only - internal table content is
*&   converted to HTML; check the notes below)
*&
*&---------------------------------------------------------------------*

    CASE proc_kind.

*&---------------------------------------------------------------------*
*& XML file download (asXML)
*&---------------------------------------------------------------------*

      WHEN zif_demo_abap_ide_imp_exp=>xml.

        SELECT * FROM zdemo_abap_carr INTO TABLE @DATA(itab).
        CALL TRANSFORMATION id SOURCE itab = itab
                               RESULT XML content.

*&---------------------------------------------------------------------*
*& JSON file download
*&---------------------------------------------------------------------*

      WHEN  zif_demo_abap_ide_imp_exp=>json.

        SELECT * FROM zdemo_abap_carr INTO TABLE @DATA(it).
        DATA(abap_to_json) = /ui2/cl_json=>serialize( data = it ).
        content = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( abap_to_json ).

*&---------------------------------------------------------------------*
*& TXT file download
*&---------------------------------------------------------------------*

      WHEN zif_demo_abap_ide_imp_exp=>txt.

        content_string = `This is some text for a txt file.`.

*&---------------------------------------------------------------------*
*& HTML display/file download
*&---------------------------------------------------------------------*

        "Note:
        "- See the notes in the implementation of if_oo_adt_classrun~main regarding
        "  commenting in and out of the following code snippet.
        "- The IDE action will display the HTML content in a window. To download the
        "  HTML file, you can make a right-click in this window and save it locally..

      WHEN zif_demo_abap_ide_imp_exp=>html.

        SELECT * FROM zdemo_abap_carr INTO TABLE @DATA(it4html).
        DATA(output_oref) = cl_demo_output_cloud=>new( cl_demo_output_cloud=>html ).
        content_string = output_oref->get( it4html ).

    ENDCASE.

  ENDMETHOD.

  METHOD zif_demo_abap_ide_imp_exp~process_import.

*&---------------------------------------------------------------------------------*
*& NOTE
*&
*& Here goes logic to process imported content, which can be content
*& of type string or xstring, depending on how the input fields are typed.
*& The example only uses xstring. The example method's exporting parameters
*& are not used except for the 'message' parameter that is used for IDE
*& action output.
*&
*& This demo example covers these use cases (see the notes in the cheat sheet
*& document of the ABAP cheat sheets GitHub repository):
*&
*& 1) JSON import
*&    The assumption is that you have created the demo json file on your local
*&    machine as described. This file is uploaded and processed further in this
*&    method by deserializing the JSON content. The JSON content represents
*&    datasets that are added to a demo database table.
*&
*& 2) XLSX import
*&    As above, it is assumed that you have created a demo XLSX file as described.
*&    Using the XLSX XCO API, the XLSX content is further processed. Similar to the
*&    JSON import example, the XLSX content represents datasets to be added to
*&    the database table.
*&
*& Comment in/out the code for checking out the demo. Ensure that only one example
*& is commented in, and you use the appropriate demo content to be uploaded.
*&
*&---------------------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& 1) JSON import
*&---------------------------------------------------------------------*

    TRY.
        DATA(file_content) = cl_abap_conv_codepage=>create_in( )->convert( imported_content ).

        DATA tab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
        /ui2/cl_json=>deserialize( EXPORTING json = file_content
                                   CHANGING data  = tab ).

        MODIFY zdemo_abap_carr FROM TABLE @tab.

        IF tab IS INITIAL.
          message = `Internal table is initial. No database table modification performed.`.
        ELSEIF sy-subrc = 0.
          message = `Database table modification successful.`.
        ELSE.
          message = `Issue with implemented database table modification.`.
        ENDIF.

      CATCH cx_root INTO DATA(err_json).
        message = err_json->get_text( ).
    ENDTRY.

*&---------------------------------------------------------------------*
*& 2) XLSX import
*&---------------------------------------------------------------------*

*    DATA itab TYPE TABLE OF zdemo_abap_carr WITH EMPTY KEY.
*
*    TRY.
*        DATA(xlsx_content) = cl_web_http_utility=>decode_x_base64( str ).
*        DATA(xlsx_doc) = xco_cp_xlsx=>document->for_file_content( xlsx_content ).
*                DATA(read_xlsx) = xlsx_doc->read_access( ).
*        DATA(worksheet) = read_xlsx->get_workbook( )->worksheet->at_position( 1 ).
*        DATA(pattern_all) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to( )->get_pattern( ).
*        worksheet->select( pattern_all
*                )->row_stream(
*                )->operation->write_to( REF #( itab )
*                )->execute( ).
*
*        MODIFY zdemo_abap_carr FROM TABLE @itab.
*
*        IF itab IS INITIAL.
*          message = `Internal table is initial. No database table modification performed.`.
*        ELSEIF sy-subrc = 0.
*          message = `Database table modification successful.`.
*        ELSE.
*          message = `Issue with implemented database table modification.`.
*        ENDIF.
*
*      CATCH cx_root INTO DATA(err_xlsx).
*        message = err_xlsx->get_text( ).
*    ENDTRY.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------------------*
*& NOTE
*&
*& - Run this class to initialize the demo database table for the example.
*& - For the classrun to execute, comment out the code available in the
*&   'WHEN zif_demo_abap_ide_imp_exp=>html' branch since it uses the
*&   CL_DEMO_OUTPUT_CLOUD class. If the code is still in, the message
*&   'Do not use CL_DEMO_OUTPUT_CLOUD in classes implementing IF_OO_ADT_CLASSRUN.'
*&   is displayed in the ADT console. After the successful execution, the
*&   database table should be initialized, and you can comment in the code
*&   again.
*&---------------------------------------------------------------------------------*

    DELETE FROM zdemo_abap_carr.

    MODIFY zdemo_abap_carr FROM TABLE @( VALUE #(
           ( carrid = 'AA'
             carrname = 'American Airlines'
             currcode = 'USD'
             url =  'http://www.aa.com' )
           ( carrid = 'LH'
             carrname = 'Lufthansa'
             currcode = 'EUR'
             url =  'http://www.lufthansa.com' )
           ( carrid = 'JL'
             carrname = 'Japan Airlines'
             currcode = 'JPY'
             url =  'http://www.jal.co.jp' )
           ( carrid = 'DL'
             carrname = 'Delta Airlines'
             currcode = 'USD'
             url =  'http://www.delta-air.com' )
           ( carrid = 'AZ'
             carrname = 'ITA Airways'
             currcode = 'EUR'
             url =  'http://www.ita-airways.com' ) ) ).

    SELECT * FROM zdemo_abap_carr INTO TABLE @DATA(it).
    out->write( it ).

  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 2) Class runner and data object displayer </summary>
  <!-- -->
<br>

**Demo Purpose** 

This demo IDE action runs classes and displays the content of data objects in an `IF_OO_ADT_CLASSRUN`-like style. Instead of plain text in the ADT console, it presents the results as HTML in an IDE action result dialog in ADT.

**Steps**

- Access your demo package in ADT in your SAP BTP ABAP Environment.
- Create a new repository object e.g. by right-clicking the package and choosing *New -> Other ABAP Repository Object -> Filter for IDE Action*.
- Action creation: 
  - Name: *ZDEMO_ABAP_IDE_ACTION_OUTPUT*
  - Description: *IDE Action for HTML Output*
- Make the following entries on the IDE action screen: 
  - Title: *Demo IDE Action: HTML Output*
  - Summary: *Action to display the content of data objects as HTML in an IDE action result dialog* 
  - Implementing Class: `ZCL_DEMO_ABAP_ACTION_OUTPUT`
  - Input UI Configuration Class: `ZCL_DEMO_ABAP_ACTION_OUTPUT_UI`
  - Number of Focused Resources: *Any*
  - Filter Object Types: `CLAS` (choose *Add* and filter for `CLAS`)
- Create the classes
  - Click the *Implementing Class* link. A pop-up is displayed to start the class creation. Provide a description and walk through the wizard. Once the class has been created, activate the class having no implementation at this stage. 
  - Go back and repeat the steps for the *Input UI Configuration Class*.
- Once the classes have been created, activate the IDE action. The screen will show warnings regarding interfaces not implemented in the classes. 
- For demo purposes, you can copy and paste the code from below. 
- Perform a mass activation to activate the classes because one class uses types specified in the other class. 
- To check out the action, proceed as follows: 
  - Create an interface named `ZIF_DEMO_ABAP_OUTPUT` and use the code from below. `ZIF_DEMO_ABAP_OUTPUT` is essential to be implemented for checking out the IDE action.     
  - Create a class named `ZCL_DEMO_ABAP` and use the code from below (contains a collection of various data objects that should be displayed). 
  - In ADT, you have opened the demo class `ZCL_DEMO_ABAP` (or any class implementing the interface).
  - Choose *STR + ALT/CMD + R* to run an IDE action.
  - Select the IDE action. You can filter for *HTML Output*.
  - Choose *Run*.
  - An IDE action dialog is displayed prompting you to provide a class. Insert the testing class `ZCL_DEMO_ABAP` (or browse, or you have run the action in a class filling the input field automatically).
  - Choose *Run*.
  - The IDE action result dialog is displayed showing the HTML output of the data objects that were written using `out->write( ... ).` and other methods that the `CL_DEMO_OUTPUT_CLOUD` class offers..
  - The example class is designed to demonstrate various data objects, ranging from elementary types to more complex types such as structures and internal tables, including nested structures, deep internal tables, and data references. 

<table>

<tr>
<th> Class (include)/Interface </th> <th> Code </th>
</tr>

<tr>
<td> 

`ZCL_DEMO_ABAP_ACTION_OUTPUT` (global class; no code in other includes)

 </td>

 <td> 


``` abap
CLASS zcl_demo_abap_action_output DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_aia_action.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_output IMPORTING VALUE(class)  TYPE string
                             RETURNING VALUE(result) TYPE string.
ENDCLASS.



CLASS zcl_demo_abap_action_output IMPLEMENTATION.
  METHOD get_output.
    class = condense( val = to_upper( class ) to = `` ).

    "Checking if input is a class
    cl_abap_classdescr=>describe_by_name(
      EXPORTING
        p_name         = class
      RECEIVING
        p_descr_ref    = DATA(tdo)
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).

    IF sy-subrc <> 0.
      result = |Class { class } not found|.
      RETURN.
    ENDIF.

    "Checking whether class is instantiatable
    IF tdo->is_instantiatable( ) = abap_false.
      result = |Class { class } cannot be instantiated|.
      RETURN.
    ENDIF.

    "Checking whether the class implements the ZIF_DEMO_ABAP_OUTPUT interface
    READ TABLE CAST cl_abap_classdescr( tdo )->interfaces WITH KEY name = 'ZIF_DEMO_ABAP_OUTPUT' TRANSPORTING NO FIELDS.

    IF sy-subrc <> 0.
      result = |Class { class } does not implement the ZIF_DEMO_ABAP_OUTPUT interface|.
      RETURN.
    ENDIF.

    DATA oref TYPE REF TO object.
    TRY.
        DATA(output_oref) = cl_demo_output_cloud=>new( cl_demo_output_cloud=>html ).
        CREATE OBJECT oref TYPE (class).
        CALL METHOD oref->('ZIF_DEMO_ABAP_OUTPUT~MAIN') EXPORTING out = output_oref.
        result = output_oref->get( ).
      CATCH cx_root INTO DATA(error).
        result = error->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD if_aia_action~run.
    DATA popup_result TYPE REF TO if_aia_popup_result.
    popup_result = cl_aia_result_factory=>create_html_popup_result( ).

    TRY.
        DATA ui_input TYPE zcl_demo_abap_action_output_ui=>ty_input.
        context->get_input_config_content( )->get_as_structure( IMPORTING result = ui_input ).
      CATCH cx_sd_invalid_data INTO DATA(exception).
        CLEAR ui_input.
        popup_result->set_content( exception->get_text( ) ).
        result = popup_result.
        RETURN.
    ENDTRY.

    popup_result->set_content( get_output( ui_input-class_name ) ).
    result = popup_result.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

`ZCL_DEMO_ABAP_ACTION_OUTPUT_UI` (global class)

 </td>

 <td> 


``` abap
CLASS zcl_demo_abap_action_output_ui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_aia_sd_action_input.

    TYPES:
      "! <p class="shorttext">Provide a class that implements the interface zif_demo_abap_output</p>
      BEGIN OF ty_input,
        "! <p class="shorttext">Class</p>
        "! $required
        class_name TYPE string,
      END OF ty_input.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_action_output_ui IMPLEMENTATION.


  METHOD if_aia_sd_action_input~create_input_config.

    TRY.
        DATA(resource) = CAST if_adt_context_src_based_obj( context->get_focused_resource( ) ).
        DATA(focused_resources) = context->get_focused_resources( ).
        LOOP AT focused_resources ASSIGNING FIELD-SYMBOL(<focused_resource>).
          IF <focused_resource>->get_kind( ) = if_adt_context_resource=>kind-source_based_dev_object.
            DATA(source_based_object) = CAST if_adt_context_src_based_obj( <focused_resource> ).
            DATA(cl) = source_based_object->get_object_info( )-display_name.
            DATA(type) = <focused_resource>->get_type( ).

            IF type = `CLAS/OC`.
              DATA(ok) = abap_true.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
    ENDTRY.

    "Default values for input data
    DATA(input_data) = VALUE ty_input( class_name = cl ).

    "Creating the configuration
    DATA(configuration) = ui_information_factory->get_configuration_factory(
                                               )->create_for_data( input_data ).
    "Setting the layout
    configuration->set_layout( if_sd_config_element=>layout-grid ).

    "Setting element configuration
    configuration->get_element( 'class_name' )->set_types( VALUE #( ( `CLAS/OC` ) ) ).

    "Returning the UI information
    result = ui_information_factory->for_abap_type( abap_type     = input_data
                                                    configuration = configuration ).
  ENDMETHOD.


  METHOD if_aia_sd_action_input~get_value_help_provider.
    "Getting a reference to the value help handler via the value help provider
    "The example uses a local class as value help provider. The method implementation
    "there is intentionally empty. The value help provider is required in the example
    "for the class selection popup to display.
    result = cl_sd_value_help_provider=>create( NEW lcl_demo_abap_action_output_vh( ) ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

`ZCL_DEMO_ABAP_ACTION_OUTPUT_UI` (CCIMP include/Local Types tab in ADT)

 </td>

 <td> 


``` abap
CLASS lcl_demo_abap_action_output_vh DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_sd_value_help_dsni.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_demo_abap_action_output_vh IMPLEMENTATION.
  METHOD if_sd_value_help_dsni~get_value_help_items.
   "The method implementation is intentionally empty.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

Interface to check out the IDE action<br><br>
`ZIF_DEMO_ABAP_OUTPUT` 

 </td>

 <td> 


``` abap
INTERFACE zif_demo_abap_output
  PUBLIC .
  METHODS main
    IMPORTING
      out TYPE REF TO if_demo_output_cloud.
ENDINTERFACE.
``` 

 </td>
</tr>

<tr>
<td> 

Class to check out the IDE action<br><br>
`ZCL_DEMO_ABAP` (global class) 

 </td>

 <td> 


``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_demo_abap_output.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD zif_demo_abap_output~main.

    out->write_html( `<h2 style="color: green;">Displaying simple string</h2>` ).

    out->write( `Hello, this is a demo example.` ).

    out->line( ).

**********************************************************************

    out->write_html( `<h2 style="color: green;">Displaying flat structure</h2>` ).

    TYPES: BEGIN OF struc_type,
             comp1 TYPE i,
             comp2 TYPE c LENGTH 5,
             comp3 TYPE string,
             comp4 TYPE n LENGTH 5,
           END OF struc_type.

    DATA(struct) = VALUE struc_type( comp1 = 123 comp2 = 'abcde' comp3 = `This is a string` comp4 = '12345' ).

    out->write( struct ).

    out->line( ).

**********************************************************************

    out->write_html( `<h2 style="color: green;">Displaying nested structure</h2>` ).

    DATA: BEGIN OF address,
            BEGIN OF name,
              title   TYPE string VALUE `Mr.`,
              prename TYPE string VALUE `Max`,
              surname TYPE string VALUE `Mustermann`,
            END OF name,
            BEGIN OF street,
              name TYPE string VALUE `Some Street`,
              num  TYPE i VALUE 111,
            END OF street,
            BEGIN OF city,
              zipcode TYPE n LENGTH 5 VALUE `12345`,
              name    TYPE string VALUE `Some Place`,
            END OF city,
          END OF address.

    out->write( address ).

    out->line( ).

**********************************************************************

    out->write_html( `<h2 style="color: green;">Displaying internal table</h2>` ).

    SELECT * FROM I_timezone INTO TABLE @DATA(timezones) UP TO 10 ROWS.

    out->write( timezones ).

    out->line( ).

**********************************************************************

    out->write_html( `<h2 style="color: green;">Displaying complex internal table</h2>` ).

    TYPES: BEGIN OF deep_struc_type,
             comp1 TYPE i,
             comp2 TYPE c LENGTH 5,
             comp3 TYPE struc_type,
             comp4 TYPE string_table,
             comp5 TYPE TABLE OF i_timezone WITH EMPTY KEY,
             comp6 TYPE REF TO string,
           END OF deep_struc_type,
           deep_tab_type TYPE TABLE OF deep_struc_type WITH EMPTY KEY.

    DATA(deep_itab) = VALUE deep_tab_type(
      ( comp1 = 1
        comp2 = 'abc'
        comp3 = VALUE struc_type( comp1 = 123 comp2 = 'abcde' comp3 = `This is a string` comp4 = '12345' )
        comp4 = VALUE #( ( `a` ) ( `b` ) ( `c` ) )
        comp5 = VALUE #( ( LINES OF timezones TO 2 ) )
        comp6 = NEW #( `Hello` ) )
      ( comp1 = 2
        comp2 = 'def'
        comp3 = VALUE struc_type( comp1 = 456 comp2 = 'fghij' comp3 = `This is another string` comp4 = '67890' )
        comp4 = VALUE #( ( `d` ) ( `e` ) ( `f` ) )
        comp5 = VALUE #( ( LINES OF timezones FROM 3 TO 4 ) )
        comp6 = NEW #( `World` ) ) ).

    out->write( deep_itab ).

    out->line( ).

**********************************************************************

    out->write_html( `<h2 style="color: green;">Displaying references</h2>` ).

    DATA(dref_int) = NEW i( 123 ).
    out->write( dref_int ).

    DATA(dref_str) = NEW string( `Hello world` ).
    out->write( dref_str ).

    DATA(current_date) = xco_cp=>sy->date( )->as( xco_cp_time=>format->iso_8601_extended ).
    out->write( current_date ).

    out->line( ).

**********************************************************************

    out->write_html( `<h2 style="color: green;">Exploring further methods</h2>` ).

    "write_html
    out->write_html( `<h3>Test Heading</h3><p>This is some sample <span style="color: blue;">text</span>.</p><ul><li>a</li><li>b</li><li>c</li></ul>` ).

    out->begin_code( `1` ).
    "This is some code block
    ASSERT 1 = 1.
    out->end_code( `1` ).

    out->begin_section( 'Heading level 1' ).
    out->write_text( 'Lorem ipsum dolor sit amet (write_text method)' ).
    out->write( 'Lorem ipsum dolor sit amet (write method; non proportional)' ).

    out->begin_section( 'Heading level 2' ).

    "write_data method
    out->write_data( value = -100 ).

    "Applying a custom name
    out->write_data( value = -200 name = 'Custom name' ).

    "Writes with named data objects
    DATA some_num TYPE i VALUE -300.
    out->write_data( some_num ).
    out->write( some_num ).

    "exclude/include parameters
    SELECT * FROM I_timezone INTO TABLE @DATA(tz) UP TO 5 ROWS.

    out->write( tz ).
    out->write( data = tz include = `TimeZoneID, TimeZoneRule` name = `include parameter` ).
    out->write( data = tz exclude = `TimeZoneIsActive` name = `exclude parameter` ).

    out->begin_section( 'Heading level 3' ).

    "write_json
    out->write_json( /ui2/cl_json=>serialize( tz ) ).

    CALL TRANSFORMATION id SOURCE itab = tz
                           RESULT XML DATA(xml_tab).

    "write_xml
    out->write_xml( xml_tab ).

    out->end_section( ).
    out->end_section( ).
    out->end_section( ).

  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 3) Method runner and parameter displayer </summary>
  <!-- -->
<br>
<br>

**Demo Purpose**

- This demo IDE action lets you select usable and instantiable classes, select one of their public instance or static methods, and provide the input parameters (if any) with character-like actual parameters. 
- For example, a calculator method may require three inputs: two numbers and an operator. The example focuses on basic cases and does not perform input validation at runtime.
- When you run the IDE action, the method is executed. 
- As a result, the parameter table (refer to the Dynamic Programming cheat sheet) is displayed as HTML in the IDE action result dialog using `CL_DEMO_OUTPUT_CLOUD`, including any of the input and output parameters and their values (if any).


**Steps**
 
- Access your demo package in ADT in your SAP BTP ABAP Environment.
- Create a new repository object by, e.g. right-clicking the package and choosing *New -> Other ABAP Repository Object -> Filter for IDE Action*.
- Action creation: 
  - Name: *ZDEMO_ABAP_IDE_ACTION_M_RUN*
  - Description: *IDE Action for Method Running and Parameter Displaying*
- Make the following entries on the IDE Action screen: 
  - Title: Demo IDE Action: Method Run
  - Summary: Action to run public instance and static class methods and display the content of parameters as HTML in an IDE action pop-up 
  - Implementing Class: `ZCL_DEMO_ABAP_ACTION_METHODRUN`
  - Input UI Configuration Class: `ZCL_DEMO_ABAP_ACTION_M_RUN_UI`
  - Number of Focused Resources: Any
  - Filter Object Types: `CLAS`
- Create the classes (see the description on the steps in the first expandable section).
- Create a demo class named `ZCL_DEMO_ABAP` to check out the IDE action.
- For the classes, you can use the code below.
- To check out the demo IDE action, proceed as follows:   
  - In ADT, you have opened the demo class `ZCL_DEMO_ABAP` (or any class implementing the interface).
  - Choose *STR + ALT/CMD + R* to run an IDE action.
  - Select the IDE action. You can filter for *Method Run*.
  - Choose *Run*.
  - An IDE action dialog is displayed prompting you to provide a class. 
  - Once you have provided the class name, you can choose *Browse* for *Public Method*. Select the method you want to call.
  - If the selected method has input parameters, they are displayed in the *Method Input Parameters* section.
  - To provide actual parameters for the formal parameters, select the line and choose *Edit*. You can then provide input for *Value*.
  - Choose *Run*.
  - The IDE action result dialog is displayed showing the HTML output of the parameter table (if any), i.e. also possible output parameters and their content.  

<table>

<tr>
<th> Class (include) </th> <th> Code </th>
</tr>

<tr>
<td> 

`ZCL_DEMO_ABAP_ACTION_METHODRUN` (global class; no code in other includes)

 </td>

 <td> 


``` abap
CLASS zcl_demo_abap_action_methodrun DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_aia_action.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_output IMPORTING VALUE(ui_input) TYPE zcl_demo_abap_action_m_run_ui=>ty_input
                             RETURNING VALUE(result)   TYPE string.

    CLASS-DATA ptab TYPE abap_parmbind_tab.
    CLASS-DATA dref TYPE REF TO data.
    TYPES: BEGIN OF p_struct,
             parameter TYPE string,
             dref      TYPE REF TO data,
           END OF p_struct.
    CLASS-DATA tab TYPE TABLE OF p_struct WITH EMPTY KEY.
    CLASS-DATA html TYPE string.
ENDCLASS.



CLASS zcl_demo_abap_action_methodrun IMPLEMENTATION.

  METHOD get_output.

    ui_input-class_name = condense( val = to_upper( ui_input-class_name ) to = `` ).

    "The example is set up to allow only your own Z classes
*    FIND PCRE `\AZ` IN ui_input-class_name.
*    IF sy-subrc <> 0.
*      result = |The example IDE action is set up to only process Z classes|.
*      RETURN.
*    ENDIF.
*
*    DATA(created_by) = xco_cp_abap=>class( CONV #( ui_input-class_name ) )->get_created_by( ).
*    IF created_by <> sy-uname.
*      result = |Class { ui_input-class_name } is not created by you. The example IDE action is set up to only process your own Z classes|.
*      RETURN.
*    ENDIF.

    cl_abap_classdescr=>describe_by_name(
      EXPORTING
        p_name         = ui_input-class_name
      RECEIVING
        p_descr_ref    = DATA(tdo)
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).

    IF sy-subrc <> 0.
      result = |Class { ui_input-class_name } not found|.
      RETURN.
    ENDIF.

    DATA(tdo_cl) = CAST cl_abap_classdescr( tdo ).

    "Checking whether the method is available
    DATA(methods) = tdo_cl->methods.
    READ TABLE methods ASSIGNING FIELD-SYMBOL(<meth>) WITH KEY name = ui_input-method_name.

    IF sy-subrc <> 0.
      result = |Method { ui_input-method_name } not available|.
      RETURN.
    ENDIF.

    "Checking that the method is not one of the constructurs
    IF <meth>-name = 'CLASS_CONSTRUCTOR' OR <meth>-name = 'CONSTRUCTOR'.
      result = |Constructors cannot be called|.
      RETURN.
    ENDIF.

    "Checking that the method is not the main method of the if_oo_adt_classrun interface
    IF <meth>-name = 'IF_OO_ADT_CLASSRUN~MAIN'.
      result = |The IF_OO_ADT_CLASSRUN~MAIN method cannot be called|.
      RETURN.
    ENDIF.

    "Checking that the method is not abstract
    IF <meth>-is_abstract = abap_true.
      result = |Method { ui_input-method_name } is abstract|.
      RETURN.
    ENDIF.

    "Checking that the method is public
    IF <meth>-visibility <> 'U'.
      result = |Method { ui_input-method_name } is not public|.
      RETURN.
    ENDIF.

    "Checking whether the class is instantiatable and instance methods can be called
    IF <meth>-is_class = abap_false AND tdo->is_instantiatable( ) = abap_false.
      result = |Instance method { ui_input-method_name } cannot be called because { ui_input-class_name } cannot be instantiated|.
      RETURN.
    ENDIF.

    "Processing method parameters
    TRY.
        LOOP AT <meth>-parameters ASSIGNING FIELD-SYMBOL(<m>).
          DATA(param_type) = tdo_cl->get_method_parameter_type( p_method_name    = ui_input-method_name
                                                                p_parameter_name = <m>-name  ).

          "Checking types and creating data objects dynamically
          CASE TYPE OF param_type.
            WHEN TYPE cl_abap_tabledescr.
              DATA(table_tdo) = CAST cl_abap_tabledescr( param_type ).
              CASE table_tdo->table_kind.
                WHEN cl_abap_tabledescr=>tablekind_any.
                  CREATE DATA dref TYPE string_table.
                WHEN cl_abap_tabledescr=>tablekind_index.
                  CREATE DATA dref TYPE string_table.
                WHEN cl_abap_tabledescr=>tablekind_std.
                  IF table_tdo->is_instantiatable( ) = abap_true AND table_tdo->get_relative_name( ) IS NOT INITIAL.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE string_table.
                  ENDIF.
                WHEN cl_abap_tabledescr=>tablekind_hashed.
                  IF table_tdo->is_instantiatable( ) = abap_true AND table_tdo->get_relative_name( ) IS NOT INITIAL.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE string_hashed_table.
                  ENDIF.
                WHEN cl_abap_tabledescr=>tablekind_sorted.
                  IF table_tdo->is_instantiatable( ) = abap_true AND table_tdo->get_relative_name( ) IS NOT INITIAL.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    TYPES sorted_tab TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.
                    CREATE DATA dref TYPE sorted_tab.
                  ENDIF.
              ENDCASE.
            WHEN TYPE cl_abap_elemdescr.
              DATA(elem_tdo) = CAST cl_abap_elemdescr( param_type ).
              CASE elem_tdo->type_kind.
                WHEN cl_abap_typedescr=>typekind_numeric.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE i.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_char.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE c LENGTH 10.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_clike.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE c LENGTH 10.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_csequence.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE c LENGTH 10.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_num.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE n LENGTH 10.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_hex.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE x LENGTH 10.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_xsequence.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE xstring.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_decfloat.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE decfloat34.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_packed.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE p LENGTH 16 DECIMALS 14.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_any.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE string.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_data.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE string.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_simple.
                  IF elem_tdo->is_instantiatable( ) = abap_true.
                    CREATE DATA dref TYPE HANDLE param_type.
                  ELSE.
                    CREATE DATA dref TYPE c LENGTH 10.
                  ENDIF.
              ENDCASE.
            WHEN OTHERS.
              CREATE DATA dref TYPE HANDLE param_type.
          ENDCASE.

          IF dref IS NOT BOUND.
            CREATE DATA dref TYPE HANDLE param_type.
          ENDIF.

          "Preparing parameter table
          APPEND VALUE #( parameter = <m>-name dref = dref ) TO tab ASSIGNING FIELD-SYMBOL(<val>).

          IF <m>-parm_kind = 'I' OR <m>-parm_kind = 'C'.
            DATA(type_descr) = cl_abap_typedescr=>describe_by_data( dref->* ).

            IF type_descr IS INSTANCE OF cl_abap_elemdescr.
              TRY.
                  <val>-dref->* = VALUE #( ui_input-parameters[ parameter = <m>-name ]-value OPTIONAL ).
                CATCH cx_root.
              ENDTRY.
            ELSE.
              "In case of non-elementary types, assigning initial value.
              TRY.
                  IF <m>-parm_kind = 'R'.
                    <val>-dref->* = dref.
                  ELSE.
                    <val>-dref->* = dref->*.
                  ENDIF.
                CATCH cx_root.
              ENDTRY.

            ENDIF.
          ENDIF.
          UNASSIGN <val>.

          INSERT VALUE #( name = <m>-name
                          kind = COND #( WHEN <m>-parm_kind = cl_abap_objectdescr=>importing THEN cl_abap_objectdescr=>exporting
                                         WHEN <m>-parm_kind = cl_abap_objectdescr=>exporting THEN cl_abap_objectdescr=>importing
                                         WHEN <m>-parm_kind = cl_abap_objectdescr=>changing THEN cl_abap_objectdescr=>changing
                                         WHEN <m>-parm_kind = cl_abap_objectdescr=>returning THEN cl_abap_objectdescr=>returning )
                          value = tab[ parameter = <m>-name ]-dref ) INTO TABLE ptab.
          CLEAR dref.
        ENDLOOP.

        "Calling methods and preparing output
        DATA(output_oref) = cl_demo_output_cloud=>new( cl_demo_output_cloud=>html ).
        IF <meth>-is_class = abap_true.
          CALL METHOD (ui_input-class_name)=>(ui_input-method_name) PARAMETER-TABLE ptab.
          IF ptab IS INITIAL.
            result = `**** Nothing to display ****`.
          ELSE.
            result = output_oref->get( ptab ).
          ENDIF.
        ELSE.
          DATA oref TYPE REF TO object.
          CREATE OBJECT oref TYPE (ui_input-class_name).
          CALL METHOD oref->(ui_input-method_name) PARAMETER-TABLE ptab.
          IF ptab IS INITIAL.
            result = `**** Nothing to display ****`.
          ELSE.
            result = output_oref->get( ptab ).
          ENDIF.
        ENDIF.
      CATCH cx_root INTO DATA(error).
        result = error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD if_aia_action~run.
    DATA(popup_result) = cl_aia_result_factory=>create_html_popup_result( ).

    TRY.
        DATA ui_input TYPE zcl_demo_abap_action_m_run_ui=>ty_input.
        context->get_input_config_content( )->get_as_structure( IMPORTING result = ui_input ).
      CATCH cx_sd_invalid_data INTO DATA(exception).
        CLEAR ui_input.
        popup_result->set_content( exception->get_text( ) ).
        result = popup_result.
        RETURN.
    ENDTRY.

    popup_result->set_content( get_output( ui_input = ui_input ) ).
    result = popup_result.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

`ZCL_DEMO_ABAP_ACTION_M_RUN_UI` (global class)

 </td>

 <td> 


``` abap
CLASS zcl_demo_abap_action_m_run_ui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_aia_sd_action_input.

    TYPES:
      "! <p class="shorttext">Input Parameters</p>
      BEGIN OF ty_table_row,
        "! <p class="shorttext">Parameter</p>
        parameter TYPE c LENGTH 30,
        "! <p class="shorttext">Value</p>
        value     TYPE string,
      END OF ty_table_row,

      ty_table TYPE STANDARD TABLE OF ty_table_row WITH DEFAULT KEY,

      "! <p class="shorttext">Provide class and method parameter values</p>
      BEGIN OF ty_input,
        "! <p class="shorttext">Class Name</p>
        class_name  TYPE string,
        "! <p class="shorttext">Public Method</p>
        method_name TYPE string,
        "! <p class="shorttext">Method Input Parameters</p>
        parameters  TYPE ty_table,
      END OF ty_input.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_action_m_run_ui IMPLEMENTATION.

  METHOD if_aia_sd_action_input~create_input_config.
    TRY.
        DATA(resource) = CAST if_adt_context_src_based_obj( context->get_focused_resource( ) ).
        DATA(focused_resources) = context->get_focused_resources( ).
        LOOP AT focused_resources ASSIGNING FIELD-SYMBOL(<focused_resource>).
          IF <focused_resource>->get_kind( ) = if_adt_context_resource=>kind-source_based_dev_object.
            DATA(source_based_object) = CAST if_adt_context_src_based_obj( <focused_resource> ).
            DATA(cl) = source_based_object->get_object_info( )-display_name.
            DATA(type) = <focused_resource>->get_type( ).
            IF type = `CLAS/OC`.
              DATA(ok) = abap_true.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
    ENDTRY.

    "Default values for input data
    DATA(input_data) = VALUE ty_input( class_name = cl ).

    "Creating the configuration
    DATA(configuration) = ui_information_factory->get_configuration_factory(
                                               )->create_for_data( input_data ).
    "Setting the layout
    configuration->set_layout( if_sd_config_element=>layout-grid ).

    "Setting element and table configuration
    configuration->get_element( 'class_name' )->set_types( VALUE #( ( `CLAS/OC` ) ) ).
    configuration->get_element( 'method_name' )->set_values( if_sd_config_element=>values_kind-domain_specific_named_items ).
    DATA(parameters) = configuration->get_structured_table( 'parameters' ).
    parameters->set_layout( if_sd_config_element=>layout-table ).
    parameters->if_sd_config_element~set_read_only( abap_true ).
    parameters->get_line_structure( )->if_sd_config_element~set_read_only( abap_false ).
    parameters->get_line_structure( )->get_element( 'parameter' )->set_read_only( abap_true ).
    configuration->get_element( 'class_name' )->set_sideeffect( after_update = abap_true ).
    configuration->get_element( 'method_name' )->set_sideeffect( after_update = abap_true ).

    "Setting side effect on the table
    parameters->get_line_structure( )->set_sideeffect( before_create   = abap_true
                                                       feature_control = abap_true ).

    "Returning the UI information
    result = ui_information_factory->for_abap_type( abap_type     = input_data
                                                    configuration = configuration ).
  ENDMETHOD.


  METHOD if_aia_sd_action_input~get_side_effect_provider.
    "Getting a reference to the side effect handler via the side effect provider
    "The example uses a local class as side effect provider.
    result = cl_sd_sideeffect_provider=>create( feature_control = NEW lcl_demo_abap_action_m_run_se( )
                                                determination   = NEW lcl_demo_abap_action_m_run_se( ) ).
  ENDMETHOD.


  METHOD if_aia_sd_action_input~get_value_help_provider.
    "Getting a reference to the value help handler via the value help provider
    "The example uses a local class as value help provider.
    result = cl_sd_value_help_provider=>create( NEW lcl_demo_abap_action_m_run_vh( ) ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

`ZCL_DEMO_ABAP_ACTION_M_RUN_UI` (CCIMP include/Local Types tab in ADT)

 </td>

 <td> 


``` abap
*&---------------------------------------------------------------------*
*& Value help handler
*&---------------------------------------------------------------------*

CLASS lcl_demo_abap_action_m_run_vh DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_sd_value_help_dsni.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_demo_abap_action_m_run_vh IMPLEMENTATION.

  METHOD if_sd_value_help_dsni~get_value_help_items.
    DATA items TYPE STANDARD TABLE OF if_sd_value_help_dsni=>ty_named_item.

    IF model->get_data_type_name( ) = 'ZCL_DEMO_ABAP_ACTION_M_RUN_UI=>TY_INPUT'.
      DATA creation_model TYPE zcl_demo_abap_action_m_run_ui=>ty_input.
      model->get_as_structure( IMPORTING result = creation_model ).
      DATA(tdo_cl) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( creation_model-class_name ) ).
      DATA(methods_cl) = tdo_cl->methods.

      LOOP AT methods_cl INTO DATA(wa) WHERE is_abstract = abap_false AND visibility = 'U'.
        APPEND VALUE #( name = wa-name ) TO items.
      ENDLOOP.

      DELETE items WHERE name = 'CLASS_CONSTRUCTOR'.
      DELETE items WHERE name = 'CONSTRUCTOR'.
      DELETE items WHERE name = 'IF_OO_ADT_CLASSRUN~MAIN'.
    ENDIF.

    DELETE items WHERE name NOT IN search_pattern_range.
    WHILE lines( items ) > max_item_count.
      DELETE items INDEX lines( items ).
    ENDWHILE.

    "Returning the value help items
    result = VALUE #( items            = items
                      total_item_count = lines( items ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Side effect handler
*&---------------------------------------------------------------------*

CLASS lcl_demo_abap_action_m_run_se DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      if_sd_determination,
      if_sd_feature_control.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_demo_abap_action_m_run_se IMPLEMENTATION.

  METHOD if_sd_determination~run.
    DATA(input_data) = VALUE zcl_demo_abap_action_m_run_ui=>ty_input( ).
    model->get_as_structure( IMPORTING result = input_data ).
    DATA(table_path) = path->get_as_table( ).

    IF id = 'CLASS_NAME' AND determination_kind = if_sd_determination=>kind-after_update.
      CLEAR input_data-method_name.
      CLEAR input_data-parameters.
    ENDIF.

    IF id = 'METHOD_NAME' AND determination_kind = if_sd_determination=>kind-after_update.
      CLEAR input_data-parameters.
      DATA creation_model TYPE zcl_demo_abap_action_m_run_ui=>ty_input.
      model->get_as_structure( IMPORTING result = creation_model ).
      DATA(tdo_cl) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( creation_model-class_name ) ).
      DATA(methods_cl) = tdo_cl->methods.
      TRY.
          DATA(meth) = methods_cl[ name = creation_model-method_name ].
          LOOP AT meth-parameters INTO DATA(param).
            IF param-parm_kind = cl_abap_objectdescr=>importing OR param-parm_kind = cl_abap_objectdescr=>changing.
              APPEND VALUE #( parameter = param-name ) TO input_data-parameters.
            ENDIF.
          ENDLOOP.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.

    result = input_data.
  ENDMETHOD.

  METHOD if_sd_feature_control~run.
    DATA(input_data) = VALUE zcl_demo_abap_action_m_run_ui=>ty_input( ).
    model->get_as_structure( IMPORTING result = input_data ).
    DATA(feature_control) = feature_control_factory->create_for_data( input_data ).

    IF feature_control_kind = if_sd_feature_control=>kind-on_create.
      feature_control->get_structured_table( 'parameters'
                    )->get_line_structure(
                    )->get_element( 'value'
                    )->set_disabled( ).
    ENDIF.

    result = feature_control.
  ENDMETHOD.

ENDCLASS.
``` 

 </td>
</tr>


<tr>
<td> 

Class to check out the IDE action<br><br>
`ZCL_DEMO_ABAP` (global class) 

 </td>

 <td> 

- As an example, select the `stat_meth_calc` method that represents a simple calculator method, expecting three input parameters to be provided with actual parameters.
- Note the simplified demo implementation. For example, the `stat_meth_all_params` method involves importing, exporting, changing and returning parameters. The changing parameter `number` expects a value of type `decfloat34`. If you provide a nonsensical character value through the IDE action, like *Test input* or others which cannot be converted, the method will not be able to process it. Or you provide a number that exceeds the maximum value for integers (parameters typed with `i`). In such situations,  initial or demo values are automatically used to ensure the method call works.


<br>

``` abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: BEGIN OF elem_struc,
             int     TYPE i,
             int8    TYPE int8,
             dec16   TYPE decfloat16,
             dec34   TYPE decfloat34,
             float   TYPE f,
             c1      TYPE c LENGTH 1,
             c20     TYPE c LENGTH 20,
             n5      TYPE n LENGTH 5,
             string  TYPE string,
             pl8d2   TYPE p LENGTH 8 DECIMALS 2,
             pl16d14 TYPE p  LENGTH 16 DECIMALS 14,
             x4      TYPE x LENGTH 4,
             xstring TYPE xstring,
           END OF elem_struc.

    CLASS-METHODS stat_meth_all_params
      IMPORTING text           TYPE string
      EXPORTING uppercase_text TYPE string
      CHANGING  number         TYPE decfloat34
      RETURNING VALUE(result)  TYPE string.

    CLASS-METHODS inst_meth_all_params
      IMPORTING text           TYPE string
      EXPORTING uppercase_text TYPE string
      CHANGING  number         TYPE decfloat34
      RETURNING VALUE(result)  TYPE string.

    CLASS-METHODS stat_meth_calc
      IMPORTING num1          TYPE i
                operator      TYPE elem_struc-c1
                num2          TYPE i
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS stat_meth_no_importing
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS stat_meth_no_params.

    CLASS-METHODS stat_meth_mult_elem_imp_params
      IMPORTING num_i              TYPE elem_struc-int
                num_int8           TYPE elem_struc-int8
                dec16              TYPE elem_struc-dec16
                dec34              TYPE elem_struc-dec34
                float              TYPE elem_struc-float
                pl8d2              TYPE elem_struc-pl8d2
                pl16d14            TYPE elem_struc-pl16d14
                str                TYPE elem_struc-string
                c1                 TYPE elem_struc-c1
                c20                TYPE elem_struc-c20
                n5                 TYPE elem_struc-n5
                x4                 TYPE elem_struc-x4
                xstr               TYPE elem_struc-xstring
      RETURNING VALUE(elem_values) TYPE elem_struc.

    CLASS-METHODS stat_meth_mult_params
      IMPORTING text          TYPE string
      EXPORTING upper_text    TYPE string
                lower_text    TYPE string
      CHANGING  str           TYPE string
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS stat_meth_generic_imp_par
      IMPORTING any           TYPE any
                data          TYPE data
                c             TYPE c
                clike         TYPE clike
                csequence     TYPE csequence
                n             TYPE n
                x             TYPE x
                xsequence     TYPE xsequence
                dec           TYPE decfloat
                numeric       TYPE numeric
                p             TYPE p
                simple        TYPE simple
      RETURNING VALUE(result) TYPE string_table.

    METHODS inst_meth_mult_params
      IMPORTING text          TYPE string
      EXPORTING upper_text    TYPE string
                lower_text    TYPE string
      CHANGING  str           TYPE string
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS inst_meth_mult_elem_imp_params
      IMPORTING num_i              TYPE elem_struc-int
                num_int8           TYPE elem_struc-int8
                dec16              TYPE elem_struc-dec16
                dec34              TYPE elem_struc-dec34
                float              TYPE elem_struc-float
                pl8d2              TYPE elem_struc-pl8d2
                pl16d14            TYPE elem_struc-pl16d14
                str                TYPE elem_struc-string
                c1                 TYPE elem_struc-c1
                c20                TYPE elem_struc-c20
                n5                 TYPE elem_struc-n5
                x4                 TYPE elem_struc-x4
                xstr               TYPE elem_struc-xstring
      RETURNING VALUE(elem_values) TYPE elem_struc.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    out->write( 'hello' ).
  ENDMETHOD.

  METHOD stat_meth_no_importing.
    result = `Hello world`.
  ENDMETHOD.

  METHOD stat_meth_no_params.
    ...
  ENDMETHOD.

  METHOD stat_meth_mult_elem_imp_params.
    elem_values = VALUE #( int  = num_i
                           int8   = num_int8
                           dec16 = dec16
                           dec34 = dec34
                           float = float
                           pl8d2 = pl8d2
                           pl16d14 = pl16d14
                           string = str
                           c1 = c1
                           c20 = c20
                           n5 = n5
                           x4 = x4
                           xstring = xstr ).
  ENDMETHOD.

  METHOD stat_meth_generic_imp_par.
    APPEND LINES OF VALUE string_table( ( |any: { any }| )
                                        ( |data: { data }| )
                                        ( |c: { c }| )
                                        ( |clike: { clike }| )
                                        ( |csequence: { csequence }| )
                                        ( |n: { n }| )
                                        ( |x: { x }| )
                                        ( |xsequence: { xsequence }| )
                                        ( |dec: { dec }| )
                                        ( |numeric: { numeric }| )
                                        ( |p: { p }| )
                                        ( |simple: { simple }| ) ) TO result.
  ENDMETHOD.

  METHOD stat_meth_mult_params.
    upper_text = to_upper( text ).
    lower_text = to_lower( text ).
    str = reverse( str ).
    result = |upper_text = "{ upper_text }"; lower_text = "{ lower_text }"|.
  ENDMETHOD.

  METHOD inst_meth_mult_params.
    upper_text = to_upper( text ).
    lower_text = to_lower( text ).
    str = reverse( str ).
    result = |upper_text = "{ upper_text }"; lower_text = "{ lower_text }"|.
  ENDMETHOD.

  METHOD inst_meth_mult_elem_imp_params.
    elem_values = VALUE #( int  = num_i
                           int8   = num_int8
                           dec16 = dec16
                           dec34 = dec34
                           float = float
                           pl8d2 = pl8d2
                           pl16d14 = pl16d14
                           string = str
                           c1 = c1
                           c20 = c20
                           n5 = n5
                           x4 = x4
                           xstring = xstr ).
  ENDMETHOD.

  METHOD stat_meth_calc.
    CASE operator.
      WHEN '+'.
        result = num1 + num2.
      WHEN '-'.
        result = num1 - num2.
      WHEN '/'.
        result = num1 / num2.
      WHEN '*'.
        result = num1 * num2.
      WHEN OTHERS.
        result = `Calculation not possible`.
    ENDCASE.

  ENDMETHOD.

  METHOD inst_meth_all_params.
    uppercase_text = to_upper( text ).
    DATA(number_copy) = number.
    number += 1.
    result = |IMPORTING text = "{ text }" / EXPORTING uppercase_text = "{ uppercase_text }" / CHANGING number = "{ number }" (Original value = "{ number_copy }")|.
  ENDMETHOD.

  METHOD stat_meth_all_params.
    uppercase_text = to_upper( text ).
    DATA(number_copy) = number.
    number += 1.
    result = |IMPORTING text = "{ text }" / EXPORTING uppercase_text = "{ uppercase_text }" / CHANGING number = "{ number }" (Original value = "{ number_copy }")|.
  ENDMETHOD.

ENDCLASS.

``` 

 </td>
</tr>

</table>

</details>  

<br>

<details>
  <summary>🟢 4) XLSX file importer and content displayer </summary>
  <!-- -->
<br>


**Demo Purpose**

- The demo IDE action lets you select an XLSX file from your local machine, retrieve its content, and display it in an internal table as HTML using `CL_DEMO_OUTPUT_CLOUD` in the IDE action result dialog.  
- In the selection IDE action dialog, you can specify which worksheet contains the content to retrieve. You can also choose where to start retrieving the content (e.g., from column A, row 1) and indicate if there is a header line. If a header exists, the content of that cells in that line are used as field names for the internal table.  
- When you run the IDE action, the XCO API is used to read the XLSX content. An internal table is created dynamically, and the worksheet content is added to it. Finally, this internal table is displayed as HTML in the IDE action result dialog.  


**Steps**

- Access your demo package in ADT in your SAP BTP ABAP Environment.
- Create a new repository object by, e.g. right-clicking the package and choosing *New -> Other ABAP Repository Object -> Filter for IDE Action*.
- Action creation: 
  - Name: *ZDEMO_ABAP_IDE_ACTION_IMPORT*
  - Description: *IDE Action for XLSX file import*
- Make the following entries on the IDE Action screen: 
  - Title: Demo IDE Action: File Importer
  - Summary: Action to import XLSX files and display the content in an internal table in an IDE action pop-up 
  - Implementing Class: `ZCL_DEMO_ABAP_ACTION_IMPORT`
  - Input UI Configuration Class: `ZCL_DEMO_ABAP_ACTION_IMPORT_UI`
  - Number of Focused Resources: Any
  - Filter Object Types: `CLAS` (choose *Add* and filter for `CLAS`)
- Create the classes (see the description on the steps in the first expandable section). 
- You do not need a demo class named `ZCL_DEMO_ABAP` to check out the IDE action for this example.
- For the classes, you can use the code below. The table below also contains demo content you can provide your demo worksheet with.
- To check out the demo IDE action, proceed as follows:   
  - In ADT, you have opened a class (no particular class).
  - Choose *STR + ALT/CMD + R* to run an IDE action.
  - Select the IDE action. You can filter for *File import*.
  - Choose *Run*.
  - An IDE action dialog is displayed prompting you to provide a path to an XLSX file on your local machine. 
  - The assumption is that a very simple worksheet is imported, for example, the content is available in the first worksheet and has a *header line*. The content starts at column A in row 1. If the content does not include a header line, you can select the checkbox. If the content starts elsewhere in the sheet (e.g. at column C, row 3), you can make the appropriate entry. 
    - The table below includes descriptions for demonstrations.
  - Choose *Run*.
  - If the XLSX content processing is successful, the IDE action result dialog will display the XLSX content in an internal table, presented as HTML table. 

> [!WARNING]  
> Since the example IDE action lets you import a local file, be mindful of the potential security risks when importing external content. Refer to [this repository's disclaimer](./README.md#%EF%B8%8F-disclaimer) and to [the disclaimer in the documentation about XCO](https://help.sap.com/docs/btp/sap-business-technology-platform/xlsx-read-access).


<table>

<tr>
<th> Subject/Class </th> <th> Notes/Code </th>
</tr>

<tr>
<td> 

`ZCL_DEMO_ABAP_ACTION_IMPORT` (global class; no code in other includes)

 </td>

 <td> 


``` abap
CLASS zcl_demo_abap_action_import DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_aia_action.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS process_file_content
      IMPORTING ui_input      TYPE zcl_demo_abap_action_import_ui=>ty_input
      RETURNING VALUE(result) TYPE REF TO data.
    METHODS get_input_as_html
      IMPORTING ui_input      TYPE zcl_demo_abap_action_import_ui=>ty_input
      RETURNING VALUE(result) TYPE string.
    CONSTANTS error TYPE string VALUE `Cannot process input (e.g. column/row values are invalid for the selected file)`.
ENDCLASS.



CLASS zcl_demo_abap_action_import IMPLEMENTATION.
  METHOD get_input_as_html.
    DATA(output_oref) = cl_demo_output_cloud=>new( cl_demo_output_cloud=>html ).
    DATA(ref) = process_file_content( ui_input ).
    IF ref IS BOUND.
      result = output_oref->get( ref->* ).
    ELSE.
      result = output_oref->get( error ).
    ENDIF.
  ENDMETHOD.

  METHOD if_aia_action~run.
    TRY.
        DATA ui_input TYPE zcl_demo_abap_action_import_ui=>ty_input.
        context->get_input_config_content( )->get_as_structure( IMPORTING result = ui_input ).
      CATCH cx_sd_invalid_data INTO DATA(exception).
        CLEAR ui_input.
    ENDTRY.

    "Creating HTML popup
    DATA(popup_result) = cl_aia_result_factory=>create_html_popup_result( ).
    popup_result->set_content( get_input_as_html( ui_input ) ).
    result = popup_result.
  ENDMETHOD.


  METHOD process_file_content.
    TRY.
        DATA(val) = ui_input-path_text_field.
        "Decoding XLSX file input
        DATA(xlsx_content) = cl_web_http_utility=>decode_x_base64( ui_input-path_text_field ).
        DATA(xlsx_doc) = xco_cp_xlsx=>document->for_file_content( xlsx_content ).

        "Getting read access to XLSX content
        DATA(read_xlsx) = xlsx_doc->read_access( ).
        "Worksheet selection
        DATA(worksheet) = read_xlsx->get_workbook( )->worksheet->at_position( COND #( WHEN ui_input-worksheet_number < 1 THEN 1 ELSE ui_input-worksheet_number ) ).
        TRY.
            DATA(cursor) = worksheet->cursor(
              io_column = xco_cp_xlsx=>coordinate->for_alphabetic_value( COND #( WHEN ui_input-start_column IS INITIAL THEN `A` ELSE to_upper( ui_input-start_column ) ) )
              io_row    = xco_cp_xlsx=>coordinate->for_numeric_value( COND #( WHEN ui_input-start_row < 1 THEN 1 ELSE ui_input-start_row ) ) ).
          CATCH cx_root.
            result = REF #( error ).
            RETURN.
        ENDTRY.

        "Preparing table column names
        DATA column_names TYPE string_table.
        WHILE cursor->has_cell( ) = abap_true AND cursor->get_cell( )->has_value( ) = abap_true.
          DATA(cell) = cursor->get_cell( ).
          DATA(string_value) = ``.

          IF ui_input-no_table_header = abap_true.
            string_value = cursor->position->column->get_alphabetic_value( ).
          ELSE.
            cell->get_value(
              )->set_transformation( xco_cp_xlsx_read_access=>value_transformation->string_value
              )->write_to( REF #( string_value ) ).
          ENDIF.

          APPEND string_value TO column_names.

          TRY.
              cursor->move_right( ).
            CATCH cx_xco_runtime_exception.
              EXIT.
          ENDTRY.
        ENDWHILE.

        IF column_names IS INITIAL.
          result = REF #( `Cannot process input (e.g. column/row values are invalid for the file)` ).
          RETURN.
        ENDIF.

        "Creating a table type dynamically
        "The loop implementation includes name preparations so that they can be used as component names.
        DATA components TYPE cl_abap_structdescr=>component_table.
        LOOP AT column_names REFERENCE INTO DATA(wa).
          wa->* = condense( val = wa->* to = `` ).
          wa->* = replace( val = wa->* pcre = `\W` with = `` occ = 0 ).
          wa->* = replace( val = wa->* pcre = `^(\d)` with = `_$1` ).
          APPEND VALUE #( name = wa->* type = cl_abap_elemdescr=>get_string( ) ) TO components.
        ENDLOOP.

        DATA(tdo_tab) = cl_abap_tabledescr=>get( p_line_type  = cl_abap_structdescr=>get( components )
                                                 p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                 p_key_kind   = cl_abap_tabledescr=>keydefkind_empty ).

        DATA itab TYPE REF TO data.
        CREATE DATA itab TYPE HANDLE tdo_tab.

        "Specifying the pattern for the worksheet selection
        DATA(pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to(
              )->from_column( xco_cp_xlsx=>coordinate->for_alphabetic_value( ui_input-start_column )
              )->from_row( xco_cp_xlsx=>coordinate->for_numeric_value( COND #( WHEN ui_input-no_table_header IS INITIAL THEN ui_input-start_row + 1 ELSE ui_input-start_row ) )
              )->get_pattern( ).

        worksheet->select( pattern
              )->row_stream(
              )->operation->write_to( REF #( itab->* )
              )->execute( ).

        result = itab.
      CATCH cx_root.
        result = REF #( error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

`ZCL_DEMO_ABAP_ACTION_IMPORT_UI` (global class; no code in other includes)

 </td>

 <td> 


``` abap
CLASS zcl_demo_abap_action_import_ui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_aia_sd_action_input.
    TYPES:
      "! <p class="shorttext">Make entries for XLSX content retrieval:</p>
      BEGIN OF ty_input,
        "! <p class="shorttext">File Import from Path</p>
        "! $contentMediaType 'application/xlsx'
        "! $contentEncoding 'base64'
        "! $required
        path_text_field  TYPE string,
        worksheet_number TYPE i,
        start_column     TYPE string,
        start_row        TYPE i,
        "! <p class="shorttext">XLSX Content without Table Header Row</p>
        no_table_header  TYPE abap_boolean,
      END OF ty_input.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_action_import_ui IMPLEMENTATION.
  METHOD if_aia_sd_action_input~create_input_config.
    "Default values for input data
    DATA(input_data) = VALUE ty_input( worksheet_number = 1
                                       start_column = `A`
                                       start_row = 1 ).

    "Creating the configuration
    DATA(configuration) = ui_information_factory->get_configuration_factory( )->create_for_data( input_data ).
    "Setting the layout
    configuration->set_layout( if_sd_config_element=>layout-grid ).
    configuration->get_element( 'path_text_field' )->set_file_properties( kind          = if_sd_config_element=>file_properties_kind-path
                                                                          transfer_mode = VALUE #( import = abap_true ) ).

    "Returning the UI information
    result = ui_information_factory->for_abap_type( abap_type     = input_data
                                                    configuration = configuration ).
  ENDMETHOD.
ENDCLASS.
``` 

 </td>
</tr>

<tr>
<td> 

Example XLSX content and demo execution steps

 </td>

 <td> 

**First Example**

- The following table shows content you can use for demonstration purposes.
- You can copy the content of the entire table to the clipboard, go to your worksheet program and paste the content in the first worksheet, first cell, first column. Depending on your program, you may require to pass the clipboard content into the worksheet without any formatting (or with a special pasting option) so that the content is properly inserted line by line.
- Save the XLSX file and proceed with the IDE action execution as described above. 
- On the selection IDE action dialog, just provide the path to the file and leave the other values unchanged. Run the IDE action.


| Carrier ID  | Carrier Name         | Currency | URL                            |
| ----------- | -------------------- | -------- | ------------------------------ |
| AA          | American Airlines    | USD      | http://www.aa.com              |
| AC          | Air Canada           | CAD      | http://www.aircanada.ca        |
| AF          | Air France           | EUR      | http://www.airfrance.fr        |
| BA          | British Airways      | GBP      | http://www.british-airways.com |
| CO          | Continental Airlines | USD      | http://www.continental.com     |
| DL          | Delta Airlines       | USD      | http://www.delta-air.com       |
| LH          | Lufthansa            | EUR      | http://www.lufthansa.com       |
| JL          | Japan Airlines       | JPY      | http://www.jal.co.jp           |
| NW          | Northwest Airlines   | USD      | http://www.nwa.com             |
| QF          | Qantas Airways       | AUD      | http://www.qantas.com.au       |
| SA          | South African Air.   | ZAR      | http://www.saa.co.za           |
| SQ          | Singapore Airlines   | SGD      | http://www.singaporeair.com    |
| SR          | Swiss                | CHF      | http://www.swiss.com           |
| UA          | United Airlines      | USD      | http://www.ual.com             |

**Second Example**

- In the XLSX file having the content from the first example, cut the entire content, and paste it to cell 3 in column 3.
- Save the file and run the IDE action. 
- On the selection IDE action dialog, provide the path to the file and specify these values and run the IDE action:
  - *Start Column*: C  
  - *Start Row*: 3 
  

**Third Example**

- In the XLSX file having the content from the second example, clear the content of the third row, which represents the header line.
- Save the file and run the IDE action. 
- On the selection IDE action dialog, provide the path to the file and specify these values and run the IDE action:
  - *Start Column*: C  
  - *Start Row*: 4
  - Select the *XLSX Content without Table Header Row* checkbox. This will create dummy names (the names of the worksheet columns) for the internal table that is created dynamically.


 </td>
</tr>

</table>

</details>  

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>