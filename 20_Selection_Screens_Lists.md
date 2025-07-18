<a name="top"></a>

# Selection Screens and Classic Lists

- [Selection Screens and Classic Lists](#selection-screens-and-classic-lists)
  - [Introduction](#introduction)
    - [Selection Screens](#selection-screens)
    - [Classic Lists](#classic-lists)
  - [ABAP Statements for Selection Screens](#abap-statements-for-selection-screens)
    - [PARAMETERS](#parameters)
    - [SELECT-OPTIONS](#select-options)
    - [SELECTION-SCREEN](#selection-screen)
      - [Creating Selection Screens](#creating-selection-screens)
      - [Adapting the Selection Screen Layout](#adapting-the-selection-screen-layout)
      - [Using Elements from Other Selection Screens](#using-elements-from-other-selection-screens)
    - [Calling Selection Screens](#calling-selection-screens)
  - [Excursion: SUBMIT Statements](#excursion-submit-statements)
  - [ABAP Statements for Classic Lists](#abap-statements-for-classic-lists)
    - [Creating Lists with WRITE Statements](#creating-lists-with-write-statements)
    - [Formatting Output Using FORMAT Statements](#formatting-output-using-format-statements)
    - [List Creation-Related ABAP Keywords and Additions](#list-creation-related-abap-keywords-and-additions)
    - [Processing Lists in List Buffers](#processing-lists-in-list-buffers)
  - [Event Blocks](#event-blocks)
  - [Excursion: SAP List Viewer (ALV)](#excursion-sap-list-viewer-alv)
    - [Getting Started](#getting-started)
    - [Optional Layout Settings and Functionality](#optional-layout-settings-and-functionality)
  - [More Information](#more-information)
  - [Executable Examples](#executable-examples)


## Introduction

> [!NOTE]
>  The content of this cheat sheet and the executable examples are only relevant to [classic ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclassic_abap_glosry.htm).

[Selection screens](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenselection_screen_glosry.htm) and [classic lists](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclassic_list_glosry.htm) are among the classic ABAP user interfaces. They are integrated into the ABAP language itself, which means that special ABAP statements are available to create and handle them.

This cheat sheet provides a high-level overview of selection screens and classic lists, focusing on a selection of related statements, supported by executable examples to explore the syntax in action. It includes an excursion into the SAP List Viewer (ALV).

For more detailed information and syntax options, see the topics [Selection Screens](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenselection_screen.htm) and [Classic Lists](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_dynpro_list.htm) in the ABAP Keyword Documentation.

> [!NOTE]
> - Although they are considered outdated for application programs, you will still find classic ABAP UIs frequently in classic ABAP.
> - Classic ABAP UIs cannot be created in [ABAP Cloud](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_cloud_glosry.htm).
> - This cheat sheet ...
>   - is not intended to encourage you to start creating classic ABAP UIs for programming new applications.  
>   - does not cover all facets, techniques, and syntax options (there's a rich variety of keywords and additions) in great detail. 
>   - is intended to cover a selection of related syntax that you may encounter in older ABAP code. If you need more information, always consult the ABAP Keyword Documentation.
> - Links to the ABAP Keyword Documentation in this cheat sheet refer to the documentation for [Standard ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_abap_glosry.htm) (latest version).

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Selection Screens

- Are special [dynpros](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynpro_glosry.htm) in [executable programs](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexecutable_program_glosry.htm) (*"reports"*; they're also possible in [function groups](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfunction_group_glosry.htm) and [module pools](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenmodul_pool_glosry.htm), but the focus here is on executable programs).
- Used for data entry in an executable program, i.e. they allow users to ...
  - enter parameters (for entering single values).
  - provide selection criteria (complex selection options for value lists and value ranges) that can be used to supply an executable program with values to work with.
- Can be created using special ABAP statements in the global declaration part of executable programs 
- Are processed by the ABAP runtime framework, which triggers [selection screen events](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenselection_screen_event_glosry.htm) ([see further down](#event-blocks)).
- When you create a selection screen and run an ABAP program, the dynpros are generated automatically. These generated dynpros cannot be edited directly.
- Further ways in which selections screens are different from *regular* dynpros:
  - Unlike dynpros, you can create selection screens without using the [screen painter](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenscreen_painter_glosry.htm) tool. The [ABAP Editor](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_editor_glosry.htm) is the only tool you use to define a selection screen (using ABAP statements).
  - They have a more limited set of features, for example, the layout of selection screens is more restricted. However, selection screens offer various types of input fields, which can also be expressed as checkboxes or radio buttons, and which can be grouped and labeled.
  - Modification options are limited. For example, you can only use some predefined function codes.
  - You cannot define [dialog modules](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendialog_module_glosry.htm) in the ABAP program.
- There are two types of selection screens: 
  - [Standalone selection screens](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstand-alone_sel_screen_glosry.htm)
    - Defined between the statements `SELECTION-SCREEN BEGIN OF SCREEN` and `SELECTION-SCREEN END OF SCREEN`
    - Can be defined in all programs that can contain dynpros
  - [Standard selection screens](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_selscreen_glosry.htm)    
    - Each executable program (and only there) contains a standard selection screen with the dynpro number 1000. Note: If you create a standalone selection screen, you cannot use dynpro number 1000.
    - The screen elements on the standard selection screen are defined by all `PARAMETERS`, `SELECT-OPTIONS`, and `SELECTION-SCREEN` statements that are defined outside of the statements mentioned above for creating standalone selection screens.
  
<p align="right"><a href="#top">⬆️ back to top</a></p>

### Classic Lists
- Used to output data in a structured and formatted way.
- This output can be ...
  - created in the so-called [list buffer](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlist_buffer_glosry.htm), a memory area for storing [screen lists](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenscreen_list_glosry.htm), i.e. a list that is displayed on a predefined list dynpro (list dynpro that is accessed implicitly when a program is executed; note: this dynpro is not part of the executed program). 
  - sent to the [SAP spool system](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensap_spool_system_glosry.htm) as a spool list (i.e. a list that is not stored as a screen list but is intended for printing or archiving; the focus of this cheat sheet is on screen lists).
- Typical flow of executable programs that include selection screens and content to be displayed on the list dynpro:
  - Programs are executed using the *Execute* button (F8).
  - Selection screens are displayed, users usually make entries for whatever purpose, which are further processed in the program.  
  - Output result is written to the list buffer and finally displayed (on a list dynpro; note: the list processing is automatically started when running executable programs). 
  - The list can be further implemented to respond to user interaction, such as clicking a line in the list.  
- More modern alternatives for classic lists are available, such as the classes of the SAP List Viewer (ALV), for example `CL_SALV_TABLE`.

> [!NOTE]
> The program is grouped into so-called [event blocks](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenevent_block_glosry.htm), which can contain implementations for various events (e.g. a [selection screen event](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenselection_screen_event_glosry.htm) when a user selects a radio button, or a [list event](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlist_event_glosry.htm) when a user double-clicks a line in the list). These event blocks are introduced by special ABAP statements. When a particular event is triggered, the corresponding event block is called and the appropriate code can be implemented there to react to the user action. See more information [here](#event-blocks).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## ABAP Statements for Selection Screens

Selection screens can be created by using special ABAP statements in the global declaration part of executable programs: 
- [`PARAMETERS`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapparameters.htm)
- [`SELECT-OPTIONS`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselect-options.htm)
- [`SELECTION-SCREEN`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselection-screen.htm)

### PARAMETERS

- Creates ...
  - a program-global variable (similar to `DATA` statements) 
  - an input field on the selection screen automatically, i.e. the content of the program-global variable can be changed via the input field.
- The parameter name can be up to eight characters long.
- Multiple additions are available to define the appearance (e.g. `AS CHECKBOX`) or influence the user interface (e.g. `OBLIGATORY`).

The following table includes code snippets with a selection of available syntax options:

> [!NOTE]
> - Various combinations of multiple additions are possible with `PARAMETERS` statement, and some are not. See the ABAP Keyword Documentation for details.
> - To try out the code examples, you can create a demo executable program, copy and paste the code snippets, and run the programs using F8. They are designed to output content using classic lists.
> - The code snippets anticipate topics outlined further down, for example, event blocks and `WRITE` statements.

<table>

<tr>
<th> Subject/Additions </th> <th> Notes </th> <th> Code Snippet </th>
</tr>

<tr>
<td> 

Type specification options<br><br>
Additions `TYPE` and `LIKE`

 </td>

 <td> 

- You can specify data types for parameters similar to `DATA` statements. 
- This includes the `TYPE` and `LIKE` additions. Types requiring a length specification use the `TYPE ... LENGTH ...` additions.
- Regarding the type `c`, there are syntax variants to specify the length and the type itself (such as leaving out the explicit type or length specification; length specification in parentheses). 
- Note that without the `LOWER CASE` addition, inserted character-like values are transformed to uppercase.
- For type `p`, you can specify the `DECIMALS` addition.

 </td>

<td> 

``` abap
PROGRAM.

"TYPE addition
"The example uses a built-in ABAP type
"Note the LOWER CASE addition further down.
PARAMETERS p_str TYPE string.

"LIKE addition
"Referring to an existing data object
DATA some_number TYPE i.
PARAMETERS p_num LIKE some_number.

"TYPE ... LENGTH for types that require a length specification
"such as c, n, p, and x
PARAMETERS p_clen10 TYPE c LENGTH 10.
PARAMETERS p_nlen5 TYPE n LENGTH 5.

"Length specification variants
"The length can be specified in parentheses, however, specifying LENGTH
"explicitly is recommended for better readability.
PARAMETERS p_lenpar(3) TYPE c.
"No length specified means LENGTH 1 by default
PARAMETERS p_wo_len TYPE c.
"No explicit type specification means TYPE c by default
PARAMETERS p_wo_c_a(20).
PARAMETERS p_wo_c_b LENGTH 20.
"No explicit type and length specification means TYPE c LENGTH 1 by default
PARAMETERS p_woclen.

START-OF-SELECTION.

  WRITE / `Inserted values (note that inserted characters are capitalized):`.
  SKIP.
  WRITE / |p_str: "{ p_str }"|.
  WRITE / |p_num: "{ p_num }"|.
  WRITE / |p_clen10: "{ p_clen10 }"|.
  WRITE / |p_nlen5: "{ p_nlen5 }"|.
  WRITE / |p_lenpar: "{ p_lenpar }"|.
  WRITE / |p_wo_len: "{ p_wo_len }"|.
  WRITE / |p_wo_c_a: "{ p_wo_c_a }"|.
  WRITE / |p_wo_c_b: "{ p_wo_c_b }"|.
  WRITE / |p_woclen: "{ p_woclen }"|.
``` 

 </td>
</tr>

<tr>
<td> 

Parameters using ABAP Dicitionary input helps

 </td>

 <td> 

- When referring to data types from the ABAP Dictionary, all screen-relvant properties of that type are adopted.
- If the referred type includes input help functionality, a callable field help is automatically created.
- The input helps inlcude search helps, check tables, fixed values and calendar/clock helps. Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_dynpros_value_help_auto.htm).
- Note that there is no automatic value check. For that, use the `VALUE CHECK` addition.
- The code snippet includes input helps from a DDIC structure of an ABAP Keyword Documentation example.

 </td>

<td> 

``` abap
PROGRAM.

"carrier1 is based on DDIC data element demof4de, which has the search help
"demo_f4_de assigned
PARAMETERS p_carr1 TYPE demof4help-carrier1.

"carrier2 is assigned to check table scarr, which is assigned to the
"search help h_scarr
PARAMETERS p_carr2 TYPE demof4help-carrier2.

"DDIC data element that has the search help s_carrier_id assigned
PARAMETERS p_carr3 TYPE s_carr_id.

"connid is assigned to the search help demo_f4_field
PARAMETERS p_connid TYPE demof4help-connid.

"Component of demo database table that specifies a foreign key
PARAMETERS p_plane TYPE sflight-planetype.

"Fixed values as input help
PARAMETERS p_num TYPE demo_numbers.

"ABAP Dictionary types such as DATS and TIMS have
"a predefined input help (calendar and clock).
PARAMETERS p_dats TYPE dats.
PARAMETERS p_tims TYPE tims.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |p_carr1: "{ p_carr1 }"|.
  WRITE / |p_carr2: "{ p_carr2 }"|.
  WRITE / |p_carr3: "{ p_carr3 }"|.
  WRITE / |p_connid: "{ p_connid }"|.
  WRITE / |p_plane: "{ p_plane }"|.
  WRITE / |p_num: "{ p_num }"|.
  WRITE / |p_dats: "{ p_dats }"|.
  WRITE / |p_tims: "{ p_tims }"|.
``` 

 </td>
</tr>


<tr>
<td> 

Dynamically specifying the type<br><br>
Addition `LIKE (...)`

 </td>

 <td> 

- The dynamic specification works with `LIKE` followed by a data object name in parentheses.
- As name, a character-like data object that contains the name of a structure component in the ABAP Dictionary in uppercase letters.
- The parameter is then of type `c` length 132, however, the properties such as length and input help are taken from the type specified in parentheses.

 </td>

<td> 

``` abap
PROGRAM.

"In the example, the currently loaded text pool does not contain a selection text 
"for the parameters. Here, the output field displays the field label from the
"ABAP Dictionary.
DATA some_dobj TYPE c LENGTH 30.
PARAMETERS p_dyn1 LIKE (some_dobj).

DATA comp TYPE c LENGTH 20.
PARAMETERS p_dyn2 LIKE (comp).

"Parameter on the screen with type c length 132
"Here, no selection text is created for the example, and no field
"label field can be referred to. The output field displays the text 
"'Dynamic Parameter'.
DATA another_dobj like some_dobj.
PARAMETERS p_dyn3 LIKE (another_dobj).

"Text field literal, however, its content is not evaluated.
"Also here, the parameter on the screen is of type
"c length 132.
PARAMETERS p_dyn4 LIKE ('SPFLI-CARRID').

INITIALIZATION.
  some_dobj = 'SPFLI-CARRID'.
  comp = 'SFLIGHT-PLANETYPE'.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |p_dyn1: "{ p_dyn1 }"|.
  WRITE / |p_dyn2: "{ p_dyn2 }"|.
  WRITE / |p_dyn3: "{ p_dyn3 }"|.
  WRITE / |p_dyn4: "{ p_dyn4 }"|.
``` 

 </td>
</tr>

<tr>
<td> 

Specifying value options<br><br>
Additions `DEFAULT`, `LOWER CASE`, `MATCHCODE OBJECT`, `MEMORY ID`, `VALUE CHECK`

 </td>

 <td> 

- `DEFAULT`: Defines a start value (can also be a data object instead of a literal)
- `LOWER CASE`: Prevents the effect of capitalizing the entry made when the content is transported to the data object
- `MATCHCODE OBJECT`: Links an input field with a DDIC search help
- `MEMORY ID`: Links an input field with an SPA/GPA parameter in the user memory, i.e. data objects in the user memory accessible by ABAP programs. When calling the selection screen, the input field receives the SPA/GPA parameter value if the `PARAMETERS`'s data object is initial after processing of the `AT SELECTION-SCREEN OUTPUT` event block. The SPA/GPA parameters are set using [`SET PARAMETER`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapset_parameter.htm) and read using [`GET PARAMETER`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapget_parameter.htm) statements. See the names of the parameters in the `TPARA` database table.
- `VALUE CHECK`: Checks input against fixed values defined for the domain of a data type. It can only be used for DDIC data types. Checks are also available for data types being components of foreign key tables. It is recommended to specify the `VALUE CHECK` addition with the `OBLIGATORY` addition as the check is also applied in case of empty input fields.

 </td>

<td> 

``` abap
PROGRAM.

*&---------------------------------------------------------------------*
*& DEFAULT addition
*&---------------------------------------------------------------------*

PARAMETERS p_deflti TYPE i DEFAULT 12345.
PARAMETERS p_deflts TYPE string DEFAULT `This is some demo text`.

*&---------------------------------------------------------------------*
*& LOWER CASE addition
*&---------------------------------------------------------------------*

PARAMETERS p_upper TYPE string DEFAULT `Hello World`.
PARAMETERS p_lower TYPE string DEFAULT `Hello World` LOWER CASE.

*&---------------------------------------------------------------------*
*& MATCHCODE OBJECT addition
*&---------------------------------------------------------------------*

"The following example uses a search help of an ABAP Keyword Documentation example.
PARAMETERS p_carr1 TYPE s_carr_id MATCHCODE OBJECT demo_f4_de.
"Another predelivered demo search help.
PARAMETERS p_carr2 TYPE c LENGTH 3 MATCHCODE OBJECT s_carrier_id.
"Non-existent search help (triggers a message in the status line)
PARAMETERS p_nope TYPE s_carr_id MATCHCODE OBJECT demo_f4_de###.

*&---------------------------------------------------------------------*
*& MEMORY ID addition
*&---------------------------------------------------------------------*

"The example use the memory id 'rid' that stores the name of the program
"processed last. The demo explicitly sets the value to a dummy program
"name in the AT SELECTION-SCREEN OUTPUT event block using a SET PARAMETER ID
"statement.
PARAMETERS p_prog TYPE sy-repid MEMORY ID rid.

*&---------------------------------------------------------------------*
*& VALUE CHECK addition
*&---------------------------------------------------------------------*

"The example selection parameters are specified with reference to a
"demo database table field for which the check table scarr is specified.
"So, you can only enter values that are also contained in scarr.
"When running the example program, you can ...
"- make entries that are not contained in the list and execute the program 
"  to demonstrate that the value check is applied.
"- leave the input for pvalchk1 empty to demonstrate that the value check is 
"  also applied on empty fields. Therefore, also specifying the OBLIGATORY 
"  addition is recommended.
PARAMETERS pvalchk1 TYPE spfli-carrid VALUE CHECK.
PARAMETERS pvalchk2 TYPE spfli-carrid VALUE CHECK OBLIGATORY.

AT SELECTION-SCREEN OUTPUT.
  SET PARAMETER ID 'RID' FIELD 'SOME_TEST_REPORT'.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |p_deflti: "{ p_deflti }"|.
  WRITE / |p_deflts: "{ p_deflts }"|.
  WRITE / |p_upper: "{ p_upper }"|.
  WRITE / |p_lower: "{ p_lower }"|.
  WRITE / |p_carr1: "{ p_carr1 }"|.
  WRITE / |p_nope: "{ p_nope }"|.
  WRITE / |p_prog: "{ p_prog }"|.
  WRITE / |pvalchk1: "{ pvalchk1 }"|.
  WRITE / |pvalchk2: "{ pvalchk2 }"|.
``` 

 </td>
</tr>

<tr>
<td> 

Specifying screen options<br><br>
Additions `OBLIGATORY`, `NO-DISPLAY`, `VISIBLE LENGTH`, `AS CHECKBOX`, `RADIOBUTTON GROUP`, `AS LISTBOX VISIBLE LENGTH`

 </td>

 <td> 

- `OBLIGATORY`: Declares the input field as a required field. If there is no entry, the program cannot proceed when choosing *Execute*. A message will be displayed.
- `NO-DISPLAY`: Hides the input field on the selection screen. A value can be supplied when calling the program with `SUBMIT` and the `WITH` addition. Note that with the `NO-DISPLAY` addition, the parameter can have any data types except for reference/enumerated types, unlike in the other additions which require flat types (except type `string`).
- `VISIBLE LENGTH`: Defines the visible length of the field. If you specify the parameter with type `c` and length 10, and you specify the visible length as 3, the input field size is reduced accordingly, but you can still insert 10 characters.
- `AS CHECKBOX`: Displays an input field as checkbox. Type `c` and length 1 is expected, but the explicit length specification is not allowed. The checkbox is selected if the value has the value `X` or `x`.
- `RADIOBUTTON GROUP`: Defines a radio button group for parameters. Note:
  - The group name can have a maximum of four characters.
  - Regarding the data type, the same applies as for `AS CHECKBOX`.
  - Only one parameter can be defined with the `DEFAULT` addition.
  - If `DEFAULT` is not specified, the first parameter of the group is set to the value `X`.
- `AS LISTBOX VISIBLE LENGTH`: Creates a dropdown list box. You can use the function module `VRM_SET_VALUES` by passing a suitable list at the 
`AT SELECTION-SCREEN OUTPUT` or `AT SELECTION-SCREEN ON VALUE-REQUEST` events. You may want to specify the `OBLIGATORY` addition here, too, as the check is also applied to empty fields.

> [!NOTE]
> The `USER-COMMAND` addition can be specified together with `AS CHECKBOX`, `RADIOBUTTON GROUP` and `AS LISTBOX VISIBLE LENGTH` to assign a function code to the selection parameter.



 </td>

<td> 

``` abap
PROGRAM.

*&---------------------------------------------------------------------*
*& OBLIGATORY addition
*&---------------------------------------------------------------------*

PARAMETERS p_oblgty TYPE c LENGTH 10 OBLIGATORY.

*&---------------------------------------------------------------------*
*& NO-DISPLAY addition
*&---------------------------------------------------------------------*

PARAMETERS p_hidden TYPE c LENGTH 10 NO-DISPLAY.
"With NO-DISPLAY, also complex types (except reference types) can be
"specified.
PARAMETERS p_tabtyp TYPE string_table NO-DISPLAY.

*&---------------------------------------------------------------------*
*& VISIBLE LENGTH addition
*&---------------------------------------------------------------------*

PARAMETERS p_vislen TYPE c LENGTH 10 VISIBLE LENGTH 3.

*&---------------------------------------------------------------------*
*& AS CHECKBOX addition
*&---------------------------------------------------------------------*

"Implicit type c
PARAMETERS p_chkbx1 AS CHECKBOX.
"Explicit type specification (but no explicit length)
PARAMETERS p_chkbx2 TYPE c AS CHECKBOX.

*&---------------------------------------------------------------------*
*& RADIOBUTTON GROUP addition
*&---------------------------------------------------------------------*

"The following example uses a chained statement.
PARAMETERS: p_rb1a RADIOBUTTON GROUP rbg1,
            p_rb1b TYPE c RADIOBUTTON GROUP rbg1,       "Explicit type specification
            p_rb1c RADIOBUTTON GROUP rbg1 DEFAULT 'X'.  "Select this radiobutton by default

"Separate specifications, no DEFAULT specification
"Here, the first radio button is selected by default.
PARAMETERS p_rb2a RADIOBUTTON GROUP rbg2.
PARAMETERS p_rb2b RADIOBUTTON GROUP rbg2.
PARAMETERS p_rb2c RADIOBUTTON GROUP rbg2.
PARAMETERS p_rb2d RADIOBUTTON GROUP rbg2.

*&---------------------------------------------------------------------*
*& AS LISTBOX VISIBLE LENGTH
*&---------------------------------------------------------------------*

"See the implementation in the AT SELECTION-SCREEN OUTPUT event block
PARAMETERS plistbox TYPE i AS LISTBOX VISIBLE LENGTH 10 OBLIGATORY.

AT SELECTION-SCREEN OUTPUT.

  "The following implementation is relevant for the AS LISTBOX VISIBLE LENGTH
  "addition. Note:
  "- The function module VRM_SET_VALUES is used.
  "- Column TEXT: What is displayed in the list box.
  "- Colum KEY: When a line is selected, the KEY value is added to the parameter
  "- In the example, the numbers 1 - 10 are added as values.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = CONV vrm_id( 'PLISTBOX' )
      values = VALUE vrm_values( FOR i = 1 UNTIL i > 10 ( key = i text = |Value { i }| ) ).

  "Filling the string table for the NO-DISPLAY example statement.
  p_tabtyp = VALUE #( ( `Some` ) ( `demo` ) ( `strings` ) ).

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |p_oblgty: "{ p_oblgty }"|.
  WRITE / |p_hidden: "{ p_hidden }"|.

  LOOP AT p_tabtyp into data(wa).
    WRITE / |p_tabtyp (line { sy-tabix }): "{ wa }"|.
  endloop.

  IF p_chkbx1 IS INITIAL.
    WRITE / `Checkbox p_chkbx1 was not selected.`.
  ELSE.
    WRITE / `Checkbox p_chkbx1 was selected.`.
  ENDIF.

  IF p_chkbx2 IS INITIAL.
    WRITE / `Checkbox p_chkbx2 was not selected.`.
  ELSE.
    WRITE / `Checkbox p_chkbx2 was selected.`.
  ENDIF.

  WRITE / |Radio button rbg1 selection: "{ COND #( WHEN p_rb1a IS NOT INITIAL THEN 'p_rb1a' WHEN p_rb1b IS NOT INITIAL THEN 'p_rb1b' ELSE 'p_rb1c' ) }"| .
  WRITE / |Radio button rbg2 selection: "{ COND #( WHEN p_rb2a IS NOT INITIAL THEN 'p_rb2a' WHEN p_rb2b IS NOT INITIAL THEN 'p_rb2b' WHEN p_rb2c IS NOT INITIAL THEN 'p_rb2c' ELSE 'p_rb2d' ) }"| .

  WRITE / |p_list: "{ plistbox }"|.
``` 

 </td>
</tr>

<tr>
<td> 

Assigning function codes to selection parameters<br><br>
Addition `USER-COMMAND`

 </td>

 <td> 

- You can assign function codes to selection parameters using the `USER-COMMAND` addition.
- The addition is supported by `PARAMETERS` statements specifying the `AS CHECKBOX`, `RADIOBUTTON GROUP` and `AS LISTBOX VISIBLE LENGTH` additions.
- As a prerequisite, you must specify a [`TABLES`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaptables.htm) statement to declare an [interface work area](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abeninterface_work_area_glosry.htm) of the `sscrfields` DDIC structure.
- On user selection (e.g. a checkbox or radio button is selected), the `AT SELECTION-SCREEN` event is raised, and the function code is passed to the `ucomm` component of `sscrfields`. For evaluation purposes, evaluate `sscrfields-ucomm` instead of `sy-ucomm` as it is not guaranteed that the latter is always passed the correct value during selection screen processing.

 </td>

<td> 

``` abap
PROGRAM.

"Declaring interface work area
TABLES sscrfields.

*&---------------------------------------------------------------------*
*& AS CHECKBOX USER-COMMAND
*&---------------------------------------------------------------------*

PARAMETERS p_chkbox AS CHECKBOX USER-COMMAND box_cmd.

*&---------------------------------------------------------------------*
*& RADIOBUTTON GROUP group USER-COMMAND
*&---------------------------------------------------------------------*

PARAMETERS: p_rb1 RADIOBUTTON GROUP rbg USER-COMMAND rb_cmd,
            p_rb2 RADIOBUTTON GROUP rbg,
            p_rb3 RADIOBUTTON GROUP rbg.

*&---------------------------------------------------------------------*
*& AS LISTBOX VISIBLE LENGTH ... USER-COMMAND
*&---------------------------------------------------------------------*

PARAMETERS plistbox TYPE i AS LISTBOX VISIBLE LENGTH 10 USER-COMMAND list_cmd.

AT SELECTION-SCREEN OUTPUT.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = CONV vrm_id( 'PLISTBOX' )
      values = VALUE vrm_values( FOR i = 1 UNTIL i > 10 ( key = i text = |Value { i }| ) ).

AT SELECTION-SCREEN.

  CASE sscrfields-ucomm.
    WHEN 'BOX_CMD'.
      MESSAGE |Hallo { sy-uname }. You selected a checkbox.| TYPE 'I'.
    WHEN 'RB_CMD'.
      MESSAGE |Hallo { sy-uname }. You selected a radio button.| TYPE 'I'.
    WHEN 'LIST_CMD'.
      MESSAGE |Hallo { sy-uname }. You selected an item from the list box.| TYPE 'I'.
  ENDCASE.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.

  IF p_chkbox IS INITIAL.
    WRITE / `Checkbox p_chkbox was not selected.`.
  ELSE.
    WRITE / `Checkbox p_chkbox was selected.`.
  ENDIF.

  WRITE / |Radio button rbg selection: "{ COND #( WHEN p_rb1 IS NOT INITIAL THEN 'p_rb1' WHEN p_rb2 IS NOT INITIAL THEN 'p_rb2' ELSE 'p_rb3' ) }"| .

  WRITE / |p_list: "{ plistbox }"|.
``` 

 </td>
</tr>

<tr>
<td> 

Assigning a screen element of a selection screen to a modification group<br><br>
Addition `MODIF ID`

 </td>

 <td> 

- The `PARAMETERS` statement can be specified with the `MODIF ID` addition to assign a screen element of a selection screen to a modification group, which represents a group of multiple screen elements of a dynpro having a three-character ID. This ID is used to modify the display properties of all those elements specifying the ID before they are actually displayed. For that purpose, you can use the `MODIFY SCREEN` statement. The ID is assigned to the `screen-group1` component that can be evaluated. 
- Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenmodification_group_glosry.htm).

 </td>

<td> 

``` abap
PROGRAM.

TABLES sscrfields.

PARAMETERS showhide AS CHECKBOX USER-COMMAND box_cmd1.

PARAMETERS p_input1 TYPE c LENGTH 20.
PARAMETERS p_input2 TYPE c LENGTH 20 MODIF ID id1.
PARAMETERS p_input3 TYPE c LENGTH 20.
PARAMETERS p_input4 TYPE c LENGTH 20 MODIF ID id1.

PARAMETERS colored AS CHECKBOX USER-COMMAND box_cmd2.

PARAMETERS p_input5 TYPE c LENGTH 20 DEFAULT 'abcdefg'.
PARAMETERS p_input6 TYPE c LENGTH 20 MODIF ID id2 DEFAULT 'hijklm'.
PARAMETERS p_input7 TYPE c LENGTH 20 MODIF ID id2 DEFAULT 'nopqrs'.
PARAMETERS p_input8 TYPE c LENGTH 20 DEFAULT 'tuvwxyz'.

AT SELECTION-SCREEN.

  CASE sscrfields-ucomm.
    WHEN 'BOX_CMD1'.
      MESSAGE |Hallo { sy-uname }. You selected the SHOWHIDE checkbox. Screen elements are about to be shown or hidden.| TYPE 'I'.
    WHEN 'BOX_CMD2'.
      MESSAGE |Hallo { sy-uname }. You selected the COLORED checkbox. The colors of output fields are about to be intensified or reverted.| TYPE 'I'.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN INTO DATA(screen_wa).
    IF showhide = 'X' AND screen_wa-group1 = 'ID1'.
      screen_wa-active = '0'.
    ENDIF.
    IF colored = 'X' AND screen_wa-group1 = 'ID2'.
      screen_wa-intensified = '1'.
    ENDIF.
    MODIFY SCREEN FROM screen_wa.
  ENDLOOP.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |p_input1: "{ p_input1 }"|.
  WRITE / |p_input2: "{ p_input2 }"|.
  WRITE / |p_input3: "{ p_input3 }"|.
  WRITE / |p_input4: "{ p_input4 }"|.
  WRITE / |p_input5: "{ p_input5 }"|.
  WRITE / |p_input6: "{ p_input6 }"|.
  WRITE / |p_input7: "{ p_input7 }"|.
  WRITE / |p_input8: "{ p_input8 }"|.
``` 

 </td>
</tr>

<tr>
<td> 

Replacing the technical name displayed on the screen 


 </td>

 <td> 

- The parameter name is a technical name. You probably do not want to display technical names on the user interface. You can define proper names ([text elements](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentext_element_glosry.htm)) in the ABAP Editor, e.g. for translation purposes.
- For the example code snippet, proceed as follows: 
  - In ADT, make a right-click inside your demo report you copied the code to. Choose *Open Others -> Text Elements*.
  - At the bottom of the opened window, select the *Selection Texts* tab.
  - Insert `PARAM=Enter some text` and activate. Leave the *number* parameter unchanged. 
  - You can also use `SELECTION-SCREEN` statements such as the `... COMMENT ...` statement, as outlined below, and assign a text value to the screen comment.
  - When you execute the program, you will see the technical names replaced by *Enter some text* and *Enter a number*.

 </td>

<td> 

``` abap
PROGRAM.

PARAMETERS param type string.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (33) txt.
  PARAMETERS number type i.
SELECTION-SCREEN END OF LINE.

INITIALIZATION.

  txt = 'Enter a number'.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |param: "{ param }"|.
  WRITE / |number: "{ number }"|.
``` 

 </td>
</tr>

</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

### SELECT-OPTIONS

- Declares selection criteria for a data object
- Unlike the `PARAMETERS` statement, which specifies a single value for a variable, the `SELECT-OPTIONS` statement allows you to specify complex criteria, such as an value range or a list of single values, to include or exclude values, that can be evaluated.
- The selection criteria are assigned to a [selection table](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenselection_table_glosry.htm):
  - Such a table contains four columns - `LOW`, `HIGH`, `OPTION`, `SIGN` - for determining range conditions. Each line of such a table represents a condition. 
  - Typically, the content of the selection table can be evaluated in `SELECT` statements using the `IN` operator in the `WHERE` clause.
  - If the selection table is empty, all lines are respected.
  - Selection tables have the same layout as [ranges table](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenranges_table_glosry.htm) that can be created using the syntax `TYPE RANGE OF`. 
  - For historical reasons, the selection table is a table with [header line](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenheader_line_glosry.htm). Therefore, if you want to address the table content (beyond the use in a `SELECT ... WHERE ... IN ...` statement), use the syntax `a[]`.
- Multiple additions are available, and combinations of them are possible.


The following table includes code snippets with a selection of available syntax options:

> [!NOTE]
> - Various combinations of multiple additions are possible with `SELECT-OPTIONS` statements.
> - To try out the code examples, you can create a demo executable program, copy and paste the code snippets, and run the programs using F8. They are designed to output content using classic lists.
> - The code snippets anticipate topics outlined further down, for example, event blocks and `WRITE` statements.

<table>

<tr>
<th> Subject/Addition </th> <th> Notes </th> <th> Code Snippet </th>
</tr>

<tr>
<td> 

Determining the `low` and `high` columns in the selection table<br><br>
Addition `FOR`

 </td>

 <td> 

- Determines the `low` and `high` columns in the selection table.
- Besides a statically defined data object from the program or public attributes of global classes, you can also dynamically specify a DDIC data type within parentheses.
- Note:
  - Without the addition `NO_DISPLAY`, the data types must be elementary and flat (except type `f` and enumerated types).
  - When you specify a reference to a DDIC type, its screen-related properties are used.
  - In case of the dynamic specification, the columns in the selection table are created using type `c` length 45. A constant or variable containing the name of component of a flat structure from the DDIC can be specified. Literals are not evaluated.

 </td>

<td> 

The code snippet, copyable to a demo program and executable using F8, implements the following: 
- A demo internal table is created. It is filled in the `INITIALIZATION` event block.
- Two `SELECT-OPTIONS` statements are included. The first references a statically data object after `FOR`, the second a dynamically specified DDIC type. The latter demonstrates that screen-relevant properties are inherited.
- When running the program, you can make entries. `SELECT` statements retrieve data from the internal table based on the specifications in the ranges table. To visualize the selection result, `WRITE` statements display table entries in a classic list.


``` abap
PROGRAM.

DATA: BEGIN OF demo_structure,
        carrid TYPE c LENGTH 3,
        num    TYPE i,
      END OF demo_structure.

DATA itab LIKE TABLE OF demo_structure WITH EMPTY KEY.

*&---------------------------------------------------------------------*
*& Referencing a statically defined data object
*&---------------------------------------------------------------------*

"In the example, a structure component is specified.
SELECT-OPTIONS sel1 FOR demo_structure-num.

*&---------------------------------------------------------------------*
*& Referencing a dynamically specified DDIC type
*&---------------------------------------------------------------------*

"Here, the name of a component of a flat structure from the DDIC
"is passed. The example type involves search help functionality.
"Screen-relevant properties are inherited.
DATA dyn_spec TYPE c LENGTH 12.
SELECT-OPTIONS sel2 FOR (dyn_spec).

INITIALIZATION.

  itab = VALUE #( ( carrid = 'AA' num = 1 )
                  ( carrid = 'AC' num = 2 )
                  ( carrid = 'AF' num = 3 )
                  ( carrid = 'AZ' num = 4 )
                  ( carrid = 'BA' num = 5 )
                  ( carrid = 'FJ' num = 6 )
                  ( carrid = 'JL' num = 7 )
                  ( carrid = 'LH' num = 8 )                  
                  ( carrid = 'NW' num = 9 )
                  ( carrid = 'QF' num = 10 )
                  ( carrid = 'SQ' num = 11 )
                  ( carrid = 'UA' num = 12 ) ).

  dyn_spec = 'SCARR-CARRID'.

START-OF-SELECTION.

  WRITE / `Selected values (sel1):`.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE num IN @sel1
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.

  SKIP.
  SKIP.
  WRITE / `Selected values (sel2):`.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE carrid IN @sel2
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.
``` 

 </td>
</tr>

<tr>
<td> 

Specifying screen options<br><br>
Additions `OBLIGATORY`, `NO-DISPLAY`, `VISIBLE LENGTH`, `NO-EXTENSION`, `NO INTERVALS`, `MODIF ID`

 </td>

 <td> 

- `OBLIGATORY`: Declares the first input field as a required field. If there is no entry, the program cannot proceed when choosing Execute. A message will be displayed.
- `NO-DISPLAY`: Hides screen elements. Values (any flat types) can be supplied using `SUBMIT` statements. Note that there is no restriction to 45 characters. 
- `VISIBLE LENGTH`: Defines the visible length of the input field.
- `NO-EXTENSION`: Cancels the option for multiple selection on the screen.
- `NO INTERVALS`: Removes the second input field on the screen.
- `MODIF ID`: See a description in the previous section about `PARAMETERS`.
- Note that combinations of the additions are possible.

 </td>

<td> 

The code snippet, copyable to a demo program and executable using F8, implements the following: 
- A demo internal table is created. It is filled in the `INITIALIZATION` event block.
- Multiple `SELECT-OPTIONS` statements are included that specify additions. 
- When running the program, you can make entries. `SELECT` statements retrieve data from the internal table based on the specifications in the ranges table. To visualize the selection result, `WRITE` statements display table entries in a classic list.


``` abap
PROGRAM.

DATA: BEGIN OF demo_structure,
        carrid TYPE c LENGTH 3,
        num    TYPE i,
      END OF demo_structure.

DATA itab LIKE TABLE OF demo_structure WITH EMPTY KEY.

*&---------------------------------------------------------------------*
*& OBLIGATORY
*&---------------------------------------------------------------------*

SELECT-OPTIONS sel_oblg FOR demo_structure-num OBLIGATORY.

*&---------------------------------------------------------------------*
*& NO-DISPLAY
*&---------------------------------------------------------------------*

SELECT-OPTIONS sel_nodi FOR demo_structure-num NO-DISPLAY.

*&---------------------------------------------------------------------*
*& VISIBLE LENGTH
*&---------------------------------------------------------------------*

SELECT-OPTIONS sel_visl FOR demo_structure-carrid VISIBLE LENGTH 2.

*&---------------------------------------------------------------------*
*& NO-EXTENSION
*&---------------------------------------------------------------------*

SELECT-OPTIONS sel_noet FOR demo_structure-num NO-EXTENSION.

*&---------------------------------------------------------------------*
*& NO INTERVALS
*&---------------------------------------------------------------------*

SELECT-OPTIONS sel_noin FOR demo_structure-num NO INTERVALS.

INITIALIZATION.

  itab = VALUE #( ( carrid = 'AA' num = 1 )
                  ( carrid = 'AC' num = 2 )
                  ( carrid = 'AF' num = 3 )
                  ( carrid = 'AZ' num = 4 )
                  ( carrid = 'BA' num = 5 )
                  ( carrid = 'FJ' num = 6 )
                  ( carrid = 'JL' num = 7 )
                  ( carrid = 'LH' num = 8 )                  
                  ( carrid = 'NW' num = 9 )
                  ( carrid = 'QF' num = 10 )
                  ( carrid = 'SQ' num = 11 )
                  ( carrid = 'UA' num = 12 ) ).

START-OF-SELECTION.

  WRITE / `Selected values (OBLIGATORY addition):`.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE num IN @sel_oblg
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.

  SKIP.
  SKIP.
  WRITE / `Selected values (NO-DISPLAY addition):`.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE num IN @sel_nodi
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.

  SKIP.
  SKIP.
  WRITE / `Selected values (VISIBLE LENGTH addition):`.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE carrid IN @sel_visl
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.

  SKIP.
  SKIP.
  WRITE / `Selected values (NO-EXTENSION addition):`.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE num IN @sel_noet
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.

  SKIP.
  SKIP.
  WRITE / `Selected values (NO INTERVALS addition):`.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE num IN @sel_noin
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.
``` 

 </td>
</tr>

<tr>
<td> 

Specifying value options<br><br>
Additions `DEFAULT a [TO b] [OPTION c] [SIGN d]`, `LOWER CASE`, `MATCHCODE OBJECT`, `MEMORY ID`

 </td>

 <td> 

- `DEFAULT a [TO b] [OPTION c] [SIGN d]`: Defines start values for the first line of the selection table. 
  - `a` specifies the `low` column 
  - `b` specifies the `high` column 
  - `c` specifies the `option` column (specify the comparison expressions `eq`, `ne`, `ge`, `gt`, `le`, `lt`, `cp`, `np`, `bt`, `nb` directly after `OPTION`; restrictions apply if you do not specify `TO`; not specifying `OPTION` means `eq` or `bt` by default; to use patterns with `cp` and `np`, specify wildcard characters `*` or `+`)
  - `d` specifies the `sign` column (specify either `i` or `e` to include or exclude; not specifying `SIGN` means `i` is used by default)
- `LOWER CASE`: Avoid autoamtic transformation to uppercase letters.
- `MATCHCODE OBJECT`: Links an input field with a DDIC search help
- `MEMORY ID`: See a description in the previous section about `PARAMETERS`.


 </td>

<td> 

The code snippet, copyable to a demo program and executable using F8, implements the following: 
- A demo internal table is created. It is filled in the `INITIALIZATION` event block.
- Multiple `SELECT-OPTIONS` statements are included that specify additions. 
- When running the program, you can make entries. `SELECT` statements retrieve data from the internal table based on the specifications in the ranges table. To visualize the selection result, `WRITE` statements display table entries in a classic list. Additionally, the content of the ranges tables is displayed. 


``` abap
PROGRAM.

DATA: BEGIN OF demo_structure,
        carrid TYPE c LENGTH 3,
        num    TYPE i,
      END OF demo_structure.

DATA itab LIKE TABLE OF demo_structure WITH EMPTY KEY.

*&---------------------------------------------------------------------*
*& DEFAULT a [TO b] [OPTION c] [SIGN d]
*&---------------------------------------------------------------------*

SELECT-OPTIONS sel_dfl1 FOR demo_structure-num DEFAULT 4.
SELECT-OPTIONS sel_dfl2 FOR demo_structure-num DEFAULT 2 TO 8.
SELECT-OPTIONS sel_dfl3 FOR demo_structure-num DEFAULT 5 TO 10 OPTION BT SIGN I.
SELECT-OPTIONS sel_dfl4 FOR demo_structure-num DEFAULT 5 TO 10 OPTION BT SIGN E.
SELECT-OPTIONS sel_dfl5 FOR demo_structure-carrid DEFAULT 'A*' OPTION CP SIGN I.

*&---------------------------------------------------------------------*
*& LOWER CASE
*&---------------------------------------------------------------------*

"The example is specified with DEFAULT ... TO ... and includes values
"with uppercase and lowercase. If you leave the demo values, all table
"entries are read (starting from AA which is the first) as there is no
"lh value. It should be specified as LH in uppercase.
SELECT-OPTIONS sel_lc FOR demo_structure-carrid DEFAULT 'AA' TO 'lh' LOWER CASE.

*&---------------------------------------------------------------------*
*& MATCHCODE OBJECT
*&---------------------------------------------------------------------*

"The specified type includes a search help
SELECT-OPTIONS sel_mtch FOR demo_structure-carrid MATCHCODE OBJECT s_carrier_id.

INITIALIZATION.

  itab = VALUE #( ( carrid = 'AA' num = 1 )
                  ( carrid = 'AC' num = 2 )
                  ( carrid = 'AF' num = 3 )
                  ( carrid = 'AZ' num = 4 )
                  ( carrid = 'BA' num = 5 )
                  ( carrid = 'FJ' num = 6 )
                  ( carrid = 'JL' num = 7 )
                  ( carrid = 'LH' num = 8 )
                  ( carrid = 'NW' num = 9 )
                  ( carrid = 'QF' num = 10 )
                  ( carrid = 'SQ' num = 11 )
                  ( carrid = 'UA' num = 12 ) ).

START-OF-SELECTION.

  WRITE / `Selected values (only DEFAULT):`.
  LOOP AT sel_dfl1 ASSIGNING FIELD-SYMBOL(<st>).
    WRITE / |low: { <st>-low }, high: { <st>-high }, option: { <st>-option }, sign: { <st>-sign }|.
  ENDLOOP.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE num IN @sel_dfl1
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.

  SKIP.
  SKIP.
  WRITE / `Selected values (DEFAULT, TO):`.
  LOOP AT sel_dfl2 ASSIGNING <st>.
    WRITE / |low: { <st>-low }, high: { <st>-high }, option: { <st>-option }, sign: { <st>-sign }|.
  ENDLOOP.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE num IN @sel_dfl2
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.

  SKIP.
  SKIP.
  WRITE / `Selected values (DEFAULT, TO, OPTION, SIGN include):`.
  LOOP AT sel_dfl3 ASSIGNING <st>.
    WRITE / |low: { <st>-low }, high: { <st>-high }, option: { <st>-option }, sign: { <st>-sign }|.
  ENDLOOP.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE num IN @sel_dfl3
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.

  SKIP.
  SKIP.
  WRITE / `Selected values (DEFAULT, TO, OPTION, SIGN exclude):`.
  LOOP AT sel_dfl4 ASSIGNING <st>.
    WRITE / |low: { <st>-low }, high: { <st>-high }, option: { <st>-option }, sign: { <st>-sign }|.
  ENDLOOP.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE num IN @sel_dfl4
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.

  SKIP.
  SKIP.
  WRITE / `Selected values (DEFAULT, OPTION using CP for patterns, SIGN include):`.
  LOOP AT sel_dfl5 ASSIGNING FIELD-SYMBOL(<fs>).
    WRITE / |low: { <fs>-low }, high: { <fs>-high }, option: { <fs>-option }, sign: { <fs>-sign }|.
  ENDLOOP.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE carrid IN @sel_dfl5
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.

  SKIP.
  SKIP.
  WRITE / `Selected values (LOWER CASE):`.
  LOOP AT sel_lc ASSIGNING <fs>.
    WRITE / |low: { <fs>-low }, high: { <fs>-high }, option: { <fs>-option }, sign: { <fs>-sign }|.
  ENDLOOP.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE carrid IN @sel_lc
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.

  SKIP.
  SKIP.
  WRITE / `Selected values (MATCHCODE OBJECT):`.
  LOOP AT sel_mtch ASSIGNING <fs>.
    WRITE / |low: { <fs>-low }, high: { <fs>-high }, option: { <fs>-option }, sign: { <fs>-sign }|.
  ENDLOOP.
  SKIP.

  SELECT *
         FROM @itab AS tab
         WHERE carrid IN @sel_mtch
         INTO @demo_structure.
    WRITE / |{ demo_structure-carrid } { demo_structure-num }|.
  ENDSELECT.
``` 

 </td>
</tr>

</table>


<p align="right"><a href="#top">⬆️ back to top</a></p>

### SELECTION-SCREEN

- `SELECTION-SCREEN` statements can be used to create and modify the layout of standalone selection screens.
- Note: The standard selection screen for executable programs is created automatically. That is, each executable program contains a standard selection screen with dynpro number 1000. The screen elements there are defined by all `PARAMETERS`, `SELECT-OPTIONS`, and `SELECTION-SCREEN` statements that are **not** specified within the `SELECTION-SCREEN BEGIN OF SCREEN ... SELECTION-SCREEN END OF SCREEN ...` block.
- Selection screens can be created as regular dynpros or as subscreen dynpros.
- Multiple syntax variants with `SELECTION-SCREEN` are available. Most of them do not create selection screens, but are used to change the layout of selection screens, create additional screen elements, and so on.
- Example: The `PARAMETERS` and `SELECT-OPTIONS` statements create input fields on an individual line. Using the variants of the `SELECTION-SCREEN` statement, you can arrange the layout differently.
- However, the selection screen cannot be generated if there are conflicts with existing screen elements.
- See the effect with the following snippet. Run a program with the code snippet and comment in the third line. The program terminates. The text to be created (in the same line as the first text) interferes with the first text. However, the fourth line would be possible because there is no overlap.
  ```abap
  PARAMETERS pa TYPE string.
  SELECTION-SCREEN COMMENT /5(10) t1.
  "SELECTION-SCREEN COMMENT 10(20) t2.
  "SELECTION-SCREEN COMMENT 20(20) t3.
  ```

#### Creating Selection Screens

<table>

<tr>
<th> Subject/Statement </th> <th> Notes </th> <th> Code Snippet </th>
</tr>

<tr>
<td> 

Creating a selection screen as regular dynpro<br><br>
`SELECTION-SCREEN BEGIN OF SCREEN ...`

 </td>

 <td> 

- Creates a selection screen as regular dynpro by specifying a dynpro number.
- The dynpro number must be unique in the program. Do not use 1000.
- Additions (which can be combined):    
  - `... TITLE ...`: Defines a title (e.g. a random name or the name of a text symbol) for the title bar 
  - `... AS WINDOW`: Displays the selection screen in a modal dialog box

 </td>

<td> 

``` abap
PROGRAM.

SELECTION-SCREEN BEGIN OF SCREEN 9000.
  PARAMETERS param TYPE string.
  PARAMETERS number TYPE i.
SELECTION-SCREEN END OF SCREEN 9000.

START-OF-SELECTION.

  CALL SELECTION-SCREEN 9000.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |param: "{ param }"|.
  WRITE / |number: "{ number }"|.
``` 

<br>

`TITLE` addition
For the example, proceed as follows:
- Copy and paste the code snippet to a demo executable program.
- Create a text symbol by making a right-click in the source code, and choose *Open Others -> Text Elements*.
- Select the *Text Symbols* tab.
- Add the following code and activate.
  ```abap
  @MaxLength:64
  001=Selection screen demo with the TITLE addition
  ```

```abap
PROGRAM.

SELECTION-SCREEN BEGIN OF SCREEN 9001 TITLE TEXT-001.
  PARAMETERS param TYPE string.
  PARAMETERS number TYPE i.
SELECTION-SCREEN END OF SCREEN 9001.

START-OF-SELECTION.

  CALL SELECTION-SCREEN 9001.

  WRITE / `Text of text symbol:`.
  WRITE / TEXT-001.
  SKIP.
  SKIP.
  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |param: "{ param }"|.
  WRITE / |number: "{ number }"|.
```

<br>

`AS WINDOW` addition
- The example code includes multiple `SELECTION-SCREEN` statements that specify the `AS WINDOW` addition. They also specify the `TITLE` addition. In these cases, a random name is used. The name is assigned a value in the `INITIALIZATION` event block.
- You can select a radio button. Depending on the selection a selection screen is called. 
- The example demonstrates selection screens displayed as modal dialog box. They are called with `CALL SELECTION-SCREEN ... STARTING AT ...` statements (except one example, `p_winno` selection, that does not open a modal dialog box). 

```abap
PROGRAM.

PARAMETERS: p_winat1 RADIOBUTTON GROUP rbg,
            p_winat2 RADIOBUTTON GROUP rbg,
            p_noat  RADIOBUTTON GROUP rbg.

SELECTION-SCREEN BEGIN OF SCREEN 9001 TITLE seltitl1 AS WINDOW.
  PARAMETERS param1 TYPE string.
SELECTION-SCREEN END OF SCREEN 9001.

SELECTION-SCREEN BEGIN OF SCREEN 9002 TITLE seltitl2 AS WINDOW.
  PARAMETERS param2 TYPE string.
SELECTION-SCREEN END OF SCREEN 9002.

INITIALIZATION.

  seltitl1 = 'Selection screen demo with the AS WINDOW addition (1)'.
  seltitl2 = 'Another selection screen with the AS WINDOW addition (2)'.

START-OF-SELECTION.

  CASE 'X'.
    WHEN p_winat1.
      CALL SELECTION-SCREEN 9001 STARTING AT 10 10.
      WRITE / `Inserted value:`.
      SKIP.
      WRITE / |param1: "{ param1 }"|.
    WHEN p_winat2.
      CALL SELECTION-SCREEN 9002 STARTING AT 1 5.
      WRITE / `Inserted value:`.
      SKIP.
      WRITE / |param2: "{ param2 }"|.
    WHEN p_noat.
      CALL SELECTION-SCREEN 9001.
      WRITE / `Inserted value:`.
      SKIP.
      WRITE / |param1: "{ param1 }"|.
  ENDCASE.
```

 </td>
</tr>

<tr>
<td> 

Creating a selection screen as subscreen dynpro<br><br>
`SELECTION-SCREEN BEGIN OF SCREEN ... AS SUBSCREEN`

 </td>

 <td> 

- Can be included in other dynpros or selection screens, or in subscreen areas or tab pages. 
- They cannot be called explicitly using `CALL SELECTION-SCREEN` statements.
- Additions: 
  - `NO INTERVALS`: `NO INTERVALS` is implicitly used in `SELECT-OPTIONS` statements
  - `NESTING LEVEL` Adjusts the subscreen width (a number between 0 and 4 is ecpected) when the subscreen is part of one or more frames in tabstrip controls

 </td>

<td> 

The code snippet, copyable to a demo program and executable using F8, implements the following: 
- Multiple selection screens are created as subscreen dynpros, each including a `PARAMETERS` statement.
- They are included in a tabstrip control on the standard selection screen of an executable program. The `TABBED BLOCK` syntax is covered further down. In the example, the first tabbed block specifies default screens. The second does not. There, the switch on tab click is realized by evaluating `sscrfields-ucomm`.

``` abap
PROGRAM.

TABLES sscrfields.

SELECTION-SCREEN BEGIN OF SCREEN 9001 AS SUBSCREEN.
  PARAMETERS param1 TYPE c LENGTH 20.
SELECTION-SCREEN END OF SCREEN 9001.

SELECTION-SCREEN BEGIN OF SCREEN 9002 AS SUBSCREEN.
  PARAMETERS param2 TYPE c LENGTH 20.
SELECTION-SCREEN END OF SCREEN 9002.

SELECTION-SCREEN BEGIN OF SCREEN 9003 AS SUBSCREEN.
  PARAMETERS param3 TYPE c LENGTH 20.
SELECTION-SCREEN END OF SCREEN 9003.

SELECTION-SCREEN BEGIN OF SCREEN 9004 AS SUBSCREEN.
  PARAMETERS param4 TYPE c LENGTH 20.
SELECTION-SCREEN END OF SCREEN 9004.

SELECTION-SCREEN: BEGIN OF TABBED BLOCK tabbl1 FOR 2 LINES,
TAB (20) tab1 USER-COMMAND push1 DEFAULT SCREEN 9001,
TAB (20) tab2 USER-COMMAND push2 DEFAULT SCREEN 9002,
END OF BLOCK tabbl1.

SELECTION-SCREEN: BEGIN OF TABBED BLOCK tabbl2 FOR 2 LINES,
TAB (20) tab3 USER-COMMAND push3,
TAB (20) tab4 USER-COMMAND push4,
END OF BLOCK tabbl2.

INITIALIZATION.

  tab1 = 'Subscreen 1'.
  tab2 = 'Subscreen 2'.
  tab3 = 'Subscreen 3'.
  tab4 = 'Subscreen 4'.

  tabbl2-prog = sy-repid.
  tabbl2-dynnr = 9003.
  tabbl2-activetab = 'TAB3'.

AT SELECTION-SCREEN.

  CASE sscrfields-ucomm.
    WHEN 'PUSH1'.
      tab1 = 'New subscreen title'.
    WHEN 'PUSH2'.
      tab2 = 'Some tab'.
    WHEN 'PUSH3'.
      tabbl2-dynnr = 9003.
    WHEN 'PUSH4'.
      tabbl2-dynnr = 9004.
  ENDCASE.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |param1: "{ param1 }"|.
  WRITE / |param2: "{ param2 }"|.
  WRITE / |param3: "{ param3 }"|.
  WRITE / |param4: "{ param4 }"|.
``` 

 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Adapting the Selection Screen Layout

<table>

<tr>
<th> Subject/Statement </th> <th> Notes </th> <th> Code Snippet </th>
</tr>

<tr>
<td> 

Adding blank lines<br><br>
`SELECTION-SCREEN SKIP`

 </td>

 <td> 

- Creates a specified number of lines
- If a number is not specified, one blank line is created by default.

 </td>

<td> 

``` abap
PARAMETERS param1 TYPE c LENGTH 20.
SELECTION-SCREEN SKIP.
PARAMETERS param2 TYPE c LENGTH 20.
SELECTION-SCREEN SKIP 3.
PARAMETERS param3 TYPE c LENGTH 20.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |param1: "{ param1 }"|.
  WRITE / |param2: "{ param2 }"|.
  WRITE / |param3: "{ param3 }"|.
``` 

 </td>
</tr>

<tr>
<td> 

Creating horizontal lines<br><br>
`SELECTION-SCREEN ULINE`

 </td>

 <td> 

- Formatting options are available:
  - Position from where to start the line and length specification 
  - Either the position (1-83) is specified directly as number or with the additions `POS_LOW` (position of first input field) and `POS_HIGH` (position of second inut field).
  - `/` creates the line a new line; cannot be specified if multiple elements are specified in a line
- If no formatting options are specified, a line is created that goes accross the whole screen below the lines that are already filled.
- The addition `MODIF ID` is supported. 
- Find more details [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselection-screen_uline.htm).
  

 </td>

<td> 

``` abap
PROGRAM.

PARAMETERS param1 TYPE c LENGTH 20.
SELECTION-SCREEN ULINE.
PARAMETERS param2 TYPE c LENGTH 20.
"/ means the line is created in a new line
"/ is followed by the position and the length in parentheses
SELECTION-SCREEN ULINE /5(10).
PARAMETERS param3 TYPE c LENGTH 20.
"SELECTION-SCREEN ULINE 5(10).
PARAMETERS param4 TYPE c LENGTH 20.
SELECTION-SCREEN ULINE /pos_low(15).
PARAMETERS param5 TYPE c LENGTH 20.
SELECTION-SCREEN ULINE /pos_high(15).
"/ can be ommitted if the elements are in the same line
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS param6 TYPE c LENGTH 1.
  SELECTION-SCREEN ULINE 4(5).
  PARAMETERS param7 TYPE c LENGTH 1.
  SELECTION-SCREEN ULINE pos_low(20).
  PARAMETERS param8 TYPE c LENGTH 1.
  SELECTION-SCREEN ULINE pos_high(40).
SELECTION-SCREEN END OF LINE.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |param1: "{ param1 }"|.
  WRITE / |param2: "{ param2 }"|.
  WRITE / |param3: "{ param3 }"|.
  WRITE / |param4: "{ param4 }"|.
  WRITE / |param5: "{ param5 }"|.
  WRITE / |param6: "{ param6 }"|.
  WRITE / |param7: "{ param7 }"|.
  WRITE / |param8: "{ param8 }"|.
``` 

 </td>
</tr>

<tr>
<td> 

Creating an output field/comment on the selection screen<br><br>
`SELECTION-SCREEN COMMENT`

 </td>

 <td> 

- Creates text content in a field
- The statement expects a random name (maximum 8 characters; a global variable with type `c` and length 83 is implicitly created) or a text symbol
- Additions
  - Position-related additions as outlined for `SELECTION-SCREEN ULINE`
  - `FOR FIELD`: Links output field to elements specified with `PARAMETERS` or `SELECT-OPTIONS`
  - `VISIBLE LENGTH`: Defines the visible length of the output field
  - `MODIF ID`: See above.



 </td>

<td> 

``` abap
PROGRAM.

*&---------------------------------------------------------------------*
*& Position-related additions
*&---------------------------------------------------------------------*

SELECTION-SCREEN COMMENT /1(20) txt1.
SELECTION-SCREEN ULINE /1(50).
PARAMETERS: rb1 RADIOBUTTON GROUP rbgr,
            rb2 RADIOBUTTON GROUP rbgr,
            rb3 RADIOBUTTON GROUP rbgr.
SELECTION-SCREEN ULINE /1(50).

SELECTION-SCREEN COMMENT /pos_high(40) txt2.

"Providing a text for a parameter
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (15) txt3.
  PARAMETERS param1 TYPE c LENGTH 10.
SELECTION-SCREEN END OF LINE.

"Using a text symbol
SELECTION-SCREEN COMMENT /1(20) TEXT-001.

*&---------------------------------------------------------------------*
*& VISIBLE LENGTH
*&---------------------------------------------------------------------*

SELECTION-SCREEN COMMENT /1(20) txt4 VISIBLE LENGTH 4.

*&---------------------------------------------------------------------*
*& MODIF ID
*&---------------------------------------------------------------------*

SELECTION-SCREEN COMMENT /1(50) txt5 MODIF ID mid.
SELECTION-SCREEN COMMENT /1(50) txt6.
SELECTION-SCREEN COMMENT /1(50) txt7 MODIF ID mid.

AT SELECTION-SCREEN OUTPUT.
  txt1 = 'Make a selection:'.
  txt2 = 'Text with pos_high'.
  txt3 = 'Make an entry'.
  txt4 = 'Some long text'.
  txt5 = 'First intensified text'.
  txt6 = 'Not intensified text'.
  txt7 = 'Second intensified tex'.
  LOOP AT SCREEN INTO DATA(screen_wa).
    IF screen_wa-group1 = 'MID'.
      screen_wa-intensified = '1'.
      MODIFY SCREEN FROM screen_wa.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |Radio button selection: "{ COND #( WHEN rb1 IS NOT INITIAL THEN 'rb1' WHEN rb2 IS NOT INITIAL THEN 'rb2' ELSE 'rb3' ) }"|.
  WRITE / |param1: "{ param1 }"|.

  IF TEXT-001 IS INITIAL.
    WRITE / |Text symbol TEXT-001 does not exist.|.
  ENDIF.
``` 

 </td>
</tr>

<tr>
<td> 

Specifying multiple elements in one line and their position in the line<br><br>
`SELECTION-SCREEN BEGIN OF LINE. ... SELECTION-SCREEN END OF LINE.` and `SELECTION-SCREEN POSITION`

 </td>

 <td> 

- Places all `PARAMETERS`, `SELECT-OPTIONS` and `SELECTION-SCREEN` statements in one line without spaces
- The statement `SELECTION-SCREEN POSITION` can only be specified in the statement block. The position can be specified as number (1-83) or using the additions `POS_LOW` and `POS_HIGH`. The statement relates to positioning the following statement.
- Note that if there are positioning conflicts, the selection screen cannot be created.
- In the statement block, certain restrictions apply. See [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselection-screen_line.htm).

 </td>

<td> 

``` abap
PROGRAM.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (10) txt1.
  SELECTION-SCREEN POSITION 15.
  PARAMETERS param1 TYPE c LENGTH 3.
  SELECTION-SCREEN COMMENT (10) txt2.
  SELECTION-SCREEN POSITION 45.
  PARAMETERS param2 TYPE c LENGTH 3.
  SELECTION-SCREEN COMMENT (10) txt3.
  SELECTION-SCREEN POSITION 70.
  SELECTION-SCREEN COMMENT (10) txt4.
SELECTION-SCREEN END OF LINE.

DATA dyn_spec TYPE c LENGTH 12.
"Chained statement
"The example emphasizes potential positioning
"conflicts that can prevent the selection screen
"from being created. For demo purposes, you can
"comment in param3 and run the program, resulting
"in a runtime error. Then you can comment in the
"NO-EXTENSION and NO INTERVALS additions to the
"SELECT-OPTIONS statement, and run the program
"again. Since the extra screen elements are not
"displayed, the selection screen can be created.
SELECTION-SCREEN: BEGIN OF LINE,
POSITION 5,
COMMENT (10) txt5.
SELECT-OPTIONS sel FOR (dyn_spec)
"NO-EXTENSION
"NO INTERVALS
.
"PARAMETERS param3 TYPE c LENGTH 3.
SELECTION-SCREEN: POSITION 70,
COMMENT (10) txt6,
END OF LINE.

INITIALIZATION.
  txt1 = 'Some text'.
  txt2 = 'Hello'.
  txt3 = sy-uname.
  txt4 = sy-datum.
  txt5 = 'Select'.
  txt6 = 'More text'.

  dyn_spec = 'SCARR-CARRID'.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |param1: "{ param1 }"|.
  WRITE / |param2: "{ param2 }"|.
  SKIP.
  WRITE / `Selection table content:`.
  LOOP AT sel ASSIGNING FIELD-SYMBOL(<st>).
    WRITE / |low: { <st>-low }, high: { <st>-high }, option: { <st>-option }, sign: { <st>-sign }|.
  ENDLOOP.
``` 

 </td>
</tr>


<tr>
<td> 

Creating pushbuttons<br><br>
`SELECTION-SCREEN PUSHBUTTON`

 </td>

 <td> 

- Typically, pushbuttons are used to modify the selection screen, not to control the program (e.g. to exit the program).
- Additions: 
  - Position-related additions as outlined above
  - `USER-COMMAND`: Used to assign a function code. The pushbutton is enabled by implementing a `TABLES sscrfields.` statement. In doing so, an interface work area is created. When a pushbutton is clicked, the `AT SELECTION-SCREEN` event is raised, and the function code can be evaluated using `sscrfields-ucomm` (do not use `sy-ucomm`).
  - `VISIBLE LENGTH`: Defines the visible length of the pushbutton
  - `MODIF ID`: See above
- You can use the function module `ICON_CREATE` to assign an icon, a tooltip, and a text to a pushbutton. 


 </td>

<td> 

``` abap
PROGRAM.

TABLES sscrfields.

SELECTION-SCREEN PUSHBUTTON /1(15) btn_a USER-COMMAND cmd1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON /1(15) btn_b USER-COMMAND cmd2.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON /1(30) btn_c USER-COMMAND cmd3 VISIBLE LENGTH 7.

INITIALIZATION.

  btn_a = 'Button A'.
  btn_c = 'Button C with long text'.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_information
      text   = 'Button B'
      info   = 'Some info'
    IMPORTING
      result = btn_b
    EXCEPTIONS
      OTHERS = 0.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'CMD1'.
      MESSAGE |Hello { sy-uname }. You clicked button A.| TYPE 'I'.
    WHEN 'CMD2'.
      MESSAGE |Hello { sy-uname }. You clicked button B.| TYPE 'I'.
    WHEN 'CMD3'.
      MESSAGE |Hello { sy-uname }. You clicked button C.| TYPE 'I'.
  ENDCASE.

START-OF-SELECTION.

  WRITE / `No content to display in this example.`.
``` 

 </td>
</tr>


<tr>
<td> 

Defining a block<br><br>
`SELECTION-SCREEN BEGIN OF BLOCK ... SELECTION-SCREEN END OF BLOCK ...`

 </td>

 <td> 

- You can also create blocks within blocks.
- The blocks raise the `AT SELECTION-SCREEN ON BLOCK`. There, the entries of a block can be processed together.
- Additions: 
  - `WITH FRAME [TITLE ...]`: Draws a frame round a block. You can optionally specify a title either by specifying a random name or a text symbol.
  - `NO INTERVALS`: When specified, `NO INTERVALS` is used implicitly for `SELECT-OPTIONS` (in all nested blocks).

 </td>

<td> 

The code snippet, copyable to a demo program and executable using F8, implements the following:
- Multiple blocks are created using `SELECTION-SCREEN ... BLOCK ...` statements, including different additions or none. One block contains nested blocks.
- The raising of `AT SELECTION-SCREEN ON BLOCK` events for the blocks defined is demonstrated using messages displayed when the program is run.

``` abap
PROGRAM.

*&---------------------------------------------------------------------*
*& No addition
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK block1.
  PARAMETERS param1 TYPE c LENGTH 10 DEFAULT 'abc' LOWER CASE.
  PARAMETERS param2 TYPE c LENGTH 10 DEFAULT 'def' LOWER CASE.
SELECTION-SCREEN END OF BLOCK block1.

*&---------------------------------------------------------------------*
*& WITH FRAME
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME.
  PARAMETERS param3 TYPE c LENGTH 10 DEFAULT 'hij' LOWER CASE.
  PARAMETERS param4 TYPE c LENGTH 10 DEFAULT 'klm' LOWER CASE.
SELECTION-SCREEN END OF BLOCK block2.

*&---------------------------------------------------------------------*
*& WITH FRAME WITH TITLE
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE titl.
  PARAMETERS: rb1 RADIOBUTTON GROUP gr,
              rb2 RADIOBUTTON GROUP gr,
              rb3 RADIOBUTTON GROUP gr.
SELECTION-SCREEN END OF BLOCK block3.

*&---------------------------------------------------------------------*
*& Nested blocks
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK block4a WITH FRAME TITLE titl4a.
  PARAMETERS param5 TYPE c LENGTH 10 DEFAULT 'nop' LOWER CASE.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF BLOCK block4b WITH FRAME TITLE titl4b.
    PARAMETERS param6 TYPE c LENGTH 10 DEFAULT 'qrs' LOWER CASE.
    SELECTION-SCREEN SKIP.
    SELECTION-SCREEN BEGIN OF BLOCK block4c WITH FRAME TITLE titl4c.
      PARAMETERS param7 TYPE c LENGTH 10 DEFAULT 'tuv' LOWER CASE.
      SELECTION-SCREEN SKIP.
    SELECTION-SCREEN END OF BLOCK block4c.
  SELECTION-SCREEN END OF BLOCK block4b.
SELECTION-SCREEN END OF BLOCK block4a.

"Demo flags for the AT SELECTION-SCREEN ON BLOCK implementations
DATA block1_flag TYPE abap_boolean.
DATA block2_flag TYPE abap_boolean.
DATA block3_flag TYPE abap_boolean.
DATA block4a_flag TYPE abap_boolean.
DATA block4b_flag TYPE abap_boolean.
DATA block4c_flag TYPE abap_boolean.

INITIALIZATION.
  titl = 'Radiobutton Selection'.
  titl4a = 'Title 1'.
  titl4b = 'Title 2'.
  titl4c = 'Title 3'.

AT SELECTION-SCREEN ON BLOCK block1.
  block1_flag = abap_true.
  MESSAGE |Event AT SELECTION-SCREEN ON BLOCK block1 raised. Others already raised? | &&
     |block2: "{ COND #( WHEN block2_flag = abap_true THEN abap_true ) }" | &&
     |block3: "{ COND #( WHEN block3_flag = abap_true THEN abap_true ) }" | &&
     |block4a: "{ COND #( WHEN block4a_flag = abap_true THEN abap_true ) }" | &&
     |block4b: "{ COND #( WHEN block4b_flag = abap_true THEN abap_true ) }" | &&
     |block4c: "{ COND #( WHEN block4c_flag = abap_true THEN abap_true ) }" | TYPE 'I'.

AT SELECTION-SCREEN ON BLOCK block2.
  block2_flag = abap_true.
  MESSAGE |Event AT SELECTION-SCREEN ON BLOCK block2 raised. Others already raised? | &&
     |block1: "{ COND #( WHEN block1_flag = abap_true THEN abap_true ) }" | &&
     |block3: "{ COND #( WHEN block3_flag = abap_true THEN abap_true ) }" | &&
     |block4a: "{ COND #( WHEN block4a_flag = abap_true THEN abap_true ) }" | &&
     |block4b: "{ COND #( WHEN block4b_flag = abap_true THEN abap_true ) }" | &&
     |block4c: "{ COND #( WHEN block4c_flag = abap_true THEN abap_true ) }" | TYPE 'I'.

AT SELECTION-SCREEN ON BLOCK block3.
  block3_flag = abap_true.
  MESSAGE |Event AT SELECTION-SCREEN ON BLOCK block3 raised. Others already raised? | &&
     |block1: "{ COND #( WHEN block1_flag = abap_true THEN abap_true ) }" | &&
     |block2: "{ COND #( WHEN block2_flag = abap_true THEN abap_true ) }" | &&
     |block4a: "{ COND #( WHEN block4a_flag = abap_true THEN abap_true ) }" | &&
     |block4b: "{ COND #( WHEN block4b_flag = abap_true THEN abap_true ) }" | &&
     |block4c: "{ COND #( WHEN block4c_flag = abap_true THEN abap_true ) }" | TYPE 'I'.

AT SELECTION-SCREEN ON BLOCK block4a.
  block4a_flag = abap_true.
  MESSAGE |Event AT SELECTION-SCREEN ON BLOCK block4a raised. Others already raised? | &&
     |block1: "{ COND #( WHEN block1_flag = abap_true THEN abap_true ) }" | &&
     |block2: "{ COND #( WHEN block2_flag = abap_true THEN abap_true ) }" | &&
     |block3: "{ COND #( WHEN block3_flag = abap_true THEN abap_true ) }" | &&
     |block4b: "{ COND #( WHEN block4b_flag = abap_true THEN abap_true ) }" | &&
     |block4c: "{ COND #( WHEN block4c_flag = abap_true THEN abap_true ) }" | TYPE 'I'.

AT SELECTION-SCREEN ON BLOCK block4b.
  block4b_flag = abap_true.

  MESSAGE |Event AT SELECTION-SCREEN ON BLOCK block4b raised. Others already raised? | &&
     |block1: "{ COND #( WHEN block1_flag = abap_true THEN abap_true ) }" | &&
     |block2: "{ COND #( WHEN block2_flag = abap_true THEN abap_true ) }" | &&
     |block3: "{ COND #( WHEN block3_flag = abap_true THEN abap_true ) }" | &&
     |block4a: "{ COND #( WHEN block4a_flag = abap_true THEN abap_true ) }" | &&
     |block4c: "{ COND #( WHEN block4c_flag = abap_true THEN abap_true ) }" | TYPE 'I'.

AT SELECTION-SCREEN ON BLOCK block4c.
  block4c_flag = abap_true.
  MESSAGE |Event AT SELECTION-SCREEN ON BLOCK block4c raised. Others already raised? | &&
   |block1: "{ COND #( WHEN block1_flag = abap_true THEN abap_true ) }" | &&
   |block2: "{ COND #( WHEN block2_flag = abap_true THEN abap_true ) }" | &&
   |block3: "{ COND #( WHEN block3_flag = abap_true THEN abap_true ) }" | &&
   |block4a: "{ COND #( WHEN block4a_flag = abap_true THEN abap_true ) }" | &&
   |block4b: "{ COND #( WHEN block4b_flag = abap_true THEN abap_true ) }" | TYPE 'I'.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |param1: "{ param1 }"|.
  WRITE / |param2: "{ param2 }"|.
  WRITE / |param3: "{ param3 }"|.
  WRITE / |param4: "{ param4 }"|.
  WRITE / |param5: "{ param5 }"|.
  WRITE / |param6: "{ param6 }"|.
  WRITE / |param7: "{ param7 }"|.
``` 

 </td>
</tr>

<tr>
<td> 

Defining tabstrips<br><br>
`SELECTION-SCREEN BEGIN OF TABBED BLOCK ... SELECTION-SCREEN END OF BLOCK ...` and `SELECTION-SCREEN TAB`

 </td>

 <td> 

- Within the statement block, you can only specify `SELECTION-SCREEN ... TAB ...` statements (they define tab titles).
- Additions to `... TABBED BLOCK ...` statements: 
  - `FOR ... LINES`: Defines the number of lines in the tabstrip area
  - `NO INTERVALS`: To narrow the size for subscreen dynpros. 
- Additions to `... TAB ...` statements: 
  - `(...)`: Length specification
  - `USER-COMMAND`: To assign a function code. See above.
  - `DEFAULT [PROGRAM ...] SCREEN`: 
    - The tab titles must be assigned a subscreen dynpro.
    - For the tabstrip areas, structures are implicitly created having the specified name. They have 3 components: `prog`, `dynnr` and `activetab`. Not specifying `DEFAULT ...` means the values must be passed before the selection screen is sent. Find [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselection-screen_tabbed.htm) more information.
  - `MODIF ID`: See above


 </td>

<td> 

See the example for `SELECTION-SCREEN BEGIN OF SCREEN ... AS SUBSCREEN` that uses `... TABBED BLOCK ...` and `... TAB ...` statements.

 </td>
</tr>

<tr>
<td> 

Adding pushbuttons in the application toolbar<br><br>
`SELECTION-SCREEN FUNCTION KEY`

 </td>

 <td> 

- The statement activates pushbuttons in the application toolbar for specific function codes (`FC0n`).
- There are five inactive pushbuttons to which the function codes `FC01`, `FC02` up to `FC05` are assigned. 
- To enable the pushbutton, include the `TABLES sscrfields.` statement in the code for the interface work area. 
- When the button is clicked, the `AT SELECTION-SCREEN` event is raised and the function code is passed to `sscrfields-ucomm`, which can be evaluated and reacted upon accordingly.

 </td>

<td> 

The code snippet, copyable to a demo program and executable using F8, implements the following:
- Five pushbuttons are added to the application toolbar.
- To provide text for the buttons, values are assigned to the `sscrfields-functxt_0n` component.
- In the `AT SELECTION-SCREEN` event block, `sscrfields-ucomm` is evaluated. Depending on the function code, parameter values are assigned or messages are raised for demonstration purposes.


``` abap
PROGRAM.

TABLES sscrfields.
PARAMETERS param1 TYPE c LENGTH 20.
PARAMETERS param2 TYPE c LENGTH 20.
PARAMETERS param3 TYPE c LENGTH 20.

SELECTION-SCREEN: FUNCTION KEY 1,  "Stands for FC01
                  FUNCTION KEY 2,  "FC02
                  FUNCTION KEY 3,  "FC03
                  FUNCTION KEY 4,  "FC04
                  FUNCTION KEY 5.  "FC05

INITIALIZATION.
  sscrfields-functxt_01 = 'Insert name'.
  sscrfields-functxt_02 = 'Insert time'.
  sscrfields-functxt_03 = 'Insert date'.
  sscrfields-functxt_04 = 'Say hi'.
  sscrfields-functxt_05 = 'Error message'.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      param1 = sy-uname.
    WHEN 'FC02'.
      param2 = sy-uzeit.
    WHEN 'FC03'.
      param3 = sy-datum.
    WHEN 'FC04'.
      MESSAGE |Hello { sy-uname }. You clicked the fourth button.| TYPE 'I'.
    WHEN 'FC05'.
      MESSAGE |Error message raised by clicking the fifth button.| TYPE 'E'.
  ENDCASE.

START-OF-SELECTION.

  WRITE / `Inserted values:`.
  SKIP.
  WRITE / |param1: "{ param1 }"|.
  WRITE / |param2: "{ param2 }"|.
  WRITE / |param3: "{ param3 }"|.
``` 

 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### Using Elements from Other Selection Screens

- You can include screen elements created in other selection screens.
- Statements include the `INCLUDE` addition. These are possible: 
  - `SELECTION-SCREEN INCLUDE PARAMETERS`
  - `SELECTION-SCREEN INCLUDE SELECT-OPTIONS`
  - `SELECTION-SCREEN INCLUDE COMMENT`
  - `SELECTION-SCREEN INCLUDE PUSHBUTTON`
  - `SELECTION-SCREEN INCLUDE BLOCKS` 
- Find more information in the subtopics [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselection-screen_include.htm).


The following code snippet, copyable to a demo program and executable using F8, implements the following:
- Various screen elements are created. Many of them - all possible syntax options are reflected - are reused and included in another selection screen (9000). 
- The selection screen with number 9000 is called if a checkbox is selected. This selection screen then opens in a modal dialog box.

```abap
PROGRAM.

TABLES sscrfields.

SELECTION-SCREEN COMMENT /1(30) txt1.
SELECTION-SCREEN SKIP.
PARAMETERS param1 TYPE c LENGTH 10.
SELECTION-SCREEN SKIP.
DATA num TYPE c LENGTH 10.
SELECT-OPTIONS sel FOR num.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(30) txt2.
SELECTION-SCREEN PUSHBUTTON /1(15) btn USER-COMMAND cmd.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(70) txt3.
PARAMETERS toincl AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN: BEGIN OF BLOCK blck,
                  COMMENT /1(30) txt4,
                  ULINE.
   PARAMETERS param2 TYPE c LENGTH 10.
SELECTION-SCREEN END OF BLOCK blck.

SELECTION-SCREEN: BEGIN OF SCREEN 9000 AS WINDOW,
                  INCLUDE PARAMETERS param1,
                  INCLUDE SELECT-OPTIONS sel,
                  INCLUDE COMMENT /1(10) txt2,
                  INCLUDE PUSHBUTTON /1(15) btn,
                  INCLUDE BLOCKS blck,
END OF SCREEN 9000.

INITIALIZATION.
  txt1 = 'Standard Selection Screen'.
  txt2 = 'Pushbutton:'.
  txt3 = 'Select the checkbox to demonstrate SELECTION-SCREEN ... INCLUDE'.
  txt4 = 'Some text'.
  btn = 'Button'.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'CMD'.
      MESSAGE |Hello { sy-uname }. You clicked the pushbutton| TYPE 'I'.
  ENDCASE.

START-OF-SELECTION.

  IF toincl = 'X'.
    CALL SELECTION-SCREEN 9000 STARTING AT 10 10.
    WRITE / `The checkbox was selected, and the modal dialog box opened.`.
    WRITE / `This modal dialog box contains multiple included elements.`.
  ELSE.
    WRITE / `The checkbox was not selected, so the modal dialog box was not opened.`.
    WRITE / `This modal dialog box contains multiple included elements.`.
  ENDIF.

  SKIP.
  WRITE / |param1: "{ param1 }"|.
  WRITE / |param2: "{ param2 }"|.
  WRITE / `Selection table content:`.
  LOOP AT sel ASSIGNING FIELD-SYMBOL(<st>).
    WRITE / |low: { <st>-low }, high: { <st>-high }, option: { <st>-option }, sign: { <st>-sign }|.
  ENDLOOP.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Calling Selection Screens

- There are several ways of how to call selection screens (see [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenselection_screen_call.htm)), among them [`CALL SELECTION-SCREEN`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcall_selection_screen.htm) statements. 
- These statements call selection screens by specifying the dynpro number. 
- Any selection screen of the main program (and only from there; including 1000) can be called.

```abap
CALL SELECTION-SCREEN 9876.

"STARTING AT/ENDING AT additions for creating a modal dialog box
"The numbers determine how it should be displayed. 
"STARTING AT: upper left corner (column and line); ENDING AT: bottom right 
"corner (if not specified, it is set automatically)
CALL SELECTION-SCREEN 9345 STARTING AT 10 10.
CALL SELECTION-SCREEN 9345 STARTING AT a b ENDING AT c d.

"You can also predefine selection criteria for a selection screen using 
"the addition USING SELECTION-SET.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursion: SUBMIT Statements

- [`SUBMIT`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsubmit.htm) statements call executable programs and, therefore, selection screens.
- In general, every executable program is started implicitly with `SUBMIT`.
- Selection screens can be considered a parameter interface if the program containing them is executed using `SUBMIT`.
- Note that when using `SUBMIT`, an authorization check for the [authorization group](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenauthorization_group_glosry.htm) is performed using the `S_PROGRAM` authorization object.

The following table includes a selection of additions and code snippets. Two programs use the demo names `zdemo_abap_report` for the calling program and `zdemo_abap_report_submit` for the called program.

<table>

<tr>
<th> Subject/Addition </th> <th> Notes </th> <th> Code Snippet </th>
</tr>

<tr>
<td> 

Calling a program, ending the current program

 </td>

 <td> 

- The program name is specified directly.
- If the program does not exist, a runtime error occurs.
- Without the `AND RETURN` addition, the current [SAP LUW](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensap_luw_glosry.htm) is terminated.


 </td>

<td> 

The following code snippets, copyable to demo programs and executable using F8 (execute program `zdemo_abap_report`), implements the following:
- Using radio buttons in the calling program, you can determine which `SUBMIT` statement to execute. 
- One statement specifies the `AND RETURN` addition, the others do not.
- Regarding the third radiobutton: 
  - The example is implemented to demonstrate the program interruption triggered by the `AND RETURN` addition. In the calling program, the SAP LUW key is retrieved using a system class before and after the `SUBMIT` statement. Both values are the same, demonstrating that the SAP LUW has been kept and is still the same. However, the called program has its own SAP LUW. The code of the called program also includes the retrieval of the SAP LUW key. This value differs from the SAP LUW key of the calling program, thus, showing that two different SAP LUWs have been started.
  - As there is no "returning parameter-like" functionality when returning to the calling program, the called program uses an `EXPORT ... TO MEMORY ID` statement to store the SAP LUW key value in the ABAP memory. For display and demonstration, the calling program retrieves this stored content with an `IMPORT ... TO MEMORY ID` statement. It then writes the content to a list using a `WRITE` statement. Find more information and criteria for the use [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendata_cluster.htm).


`zdemo_abap_report`
``` abap
PROGRAM.

PARAMETERS: noreturn RADIOBUTTON GROUP rbg,
            w_return RADIOBUTTON GROUP rbg,
            w_retkey RADIOBUTTON GROUP rbg.

START-OF-SELECTION.

  CASE 'X'.
    WHEN noreturn.

      SUBMIT zdemo_abap_report_submit.
      "Text will not be written to the list since the calling program is ended.
      WRITE / |This text was written to the list in program { sy-repid }.|.

    WHEN w_return.

      SUBMIT zdemo_abap_report_submit AND RETURN.
      "Text will be written to the list since the calling program is only interrupted.
      WRITE / |This text was written to the list in program { sy-repid }.|.

    WHEN w_retkey.

      "SAP LUW key information
      DATA(sap_luw_key_calling_prog1) = cl_system_transaction_state=>get_sap_luw_key( ).
      SUBMIT zdemo_abap_report_submit AND RETURN.
      DATA(sap_luw_key_calling_prog2) = cl_system_transaction_state=>get_sap_luw_key( ).

      DATA luw_key_called_prog TYPE sychar32.
      IMPORT key = luw_key_called_prog FROM MEMORY ID 'ID4LUWKEY'.

      WRITE / |This text was written to the list in program { sy-repid }.|.
      SKIP.
      ASSERT sap_luw_key_calling_prog1 <> luw_key_called_prog.
      WRITE: / |SAP LUW key in calling program (1): { sap_luw_key_calling_prog1 }|.
      WRITE: / |SAP LUW key in calling program (2): { sap_luw_key_calling_prog2 }|.
      WRITE: / |SAP LUW key in called program: { luw_key_called_prog }|.

  ENDCASE.
``` 

<br>

`zdemo_abap_report_submit`
``` abap
PROGRAM.

START-OF-SELECTION.

DATA(sap_luw_key_called_prog) = cl_system_transaction_state=>get_sap_luw_key( ).
EXPORT key = sap_luw_key_called_prog TO MEMORY ID 'ID4LUWKEY'.
MESSAGE |Hello from program { sy-repid }, { sy-uname }. The SAP LUW key value is "{ sap_luw_key_called_prog }".| &&
        | If you selected the first radiobutton (no "AND RETURN"), you must run the program again to check out more radiobuttons.| TYPE 'I'.
``` 


 </td>
</tr>

<tr>
<td> 

Calling a program, interrupting the current program and return<br><br>
Addition `AND RETURN`

 </td>

 <td> 

- The calling program is interrupted. 
- With the `AND RETURN` addition, the called program starts in a new [internal session](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abeninternal_session_glosry.htm) and has its own [SAP LUW](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensap_luw_glosry.htm). The current internal session and [SAP LUW](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensap_luw_glosry.htm) is kept. When the called program is ended, the execution of the calling program continues.
- Find [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsubmit.htm) more information regarding the impact of the involved SAP LUWs. 

 </td>

<td> 

See above.

 </td>
</tr>

<tr>
<td> 

Dynamically calling a program

 </td>

 <td> 

- A parenthesized flat character-like data object (literal, constannt or variable) is specified.
- The name must be specified in uppercase.
- Note the security risks and consequences a program submitted from outside can cause. You can use the class `CL_ABAP_DYN_PRG` to tackle security risks.
- As there is no "returning parameter-like" functionality when returning to the calling program, the called program uses an `EXPORT ... TO MEMORY ID` statement to store parameter values in a string table within the ABAP memory. For display and demonstration, the calling program retrieves this stored content with an `IMPORT ... TO MEMORY ID` statement. It then writes the content to a list using `WRITE` statements. Find more information and criteria for the use [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendata_cluster.htm).

 </td>

<td> 

The following code snippets, copyable to demo programs and executable using F8 (execute program `zdemo_abap_report`), implements the following:
- Using radio buttons in the calling program, you can determine which dynamic `SUBMIT` statement to execute. 
- Some statements specify the `AND RETURN` addition, some do not.
- Regarding the fourth radiobutton, see the description in the first example.
  

`zdemo_abap_report`
``` abap
PROGRAM.

PARAMETERS: noreturn RADIOBUTTON GROUP rbg,
            no_retdo RADIOBUTTON GROUP rbg,
            w_return RADIOBUTTON GROUP rbg,
            w_retkey RADIOBUTTON GROUP rbg.

START-OF-SELECTION.

  CASE 'X'.
    WHEN noreturn.

      "Program name specified as literal
      SUBMIT ('ZDEMO_ABAP_REPORT_SUBMIT').
      "Text will not be written to the list since the calling program is ended.
      WRITE / |This text was written to the list in program { sy-repid }.|.

    WHEN no_retdo.

      "Specifying the program with a variable
      DATA prog_name TYPE syrepid VALUE 'ZDEMO_ABAP_REPORT_SUBMIT'.
      SUBMIT (prog_name).
      "Text will not be written to the list since the calling program is ended.
      WRITE / |This text was written to the list in program { sy-repid }.|.

    WHEN w_return.

      SUBMIT ('ZDEMO_ABAP_REPORT_SUBMIT') AND RETURN.
      "Text will be written to the list since the calling program is only interrupted.
      WRITE / |This text was written to the list in program { sy-repid }.|.

    WHEN w_retkey.

      "SAP LUW key information
      DATA(sap_luw_key_calling_prog1) = cl_system_transaction_state=>get_sap_luw_key( ).
      SUBMIT ('ZDEMO_ABAP_REPORT_SUBMIT') AND RETURN.
      DATA(sap_luw_key_calling_prog2) = cl_system_transaction_state=>get_sap_luw_key( ).

      DATA luw_key_called_prog TYPE sychar32.
      IMPORT key = luw_key_called_prog FROM MEMORY ID 'ID4LUWKEY'.

      WRITE / |This text was written to the list in program { sy-repid }.|.
      SKIP.
      ASSERT sap_luw_key_calling_prog1 <> luw_key_called_prog.
      WRITE: / |SAP LUW key in calling program (1): { sap_luw_key_calling_prog1 }|.
      WRITE: / |SAP LUW key in calling program (2): { sap_luw_key_calling_prog2 }|.
      WRITE: / |SAP LUW key in called program: { luw_key_called_prog }|.

  ENDCASE.
``` 

<br>

`zdemo_abap_report_submit`
``` abap
PROGRAM.

START-OF-SELECTION.

DATA(sap_luw_key_called_prog) = cl_system_transaction_state=>get_sap_luw_key( ).
EXPORT key = sap_luw_key_called_prog TO MEMORY ID 'ID4LUWKEY'.
MESSAGE |Hello from program { sy-repid }, { sy-uname }. The SAP LUW key value is "{ sap_luw_key_called_prog }".| &&
        | If you selected one of the first two radiobuttons (no "AND RETURN"), you must run the program again to check out more radiobuttons.| TYPE 'I'.
``` 

 </td>
</tr>



<tr>
<td> 

Specifying a particular selection screen to be called and display it or not<br><br>
Additions `USING SELECTION-SCREEN` and `VIA SELECTION-SCREEN`

 </td>

 <td> 

- `USING SELECTION-SCREEN`: Specifies the selection screen. If the addition is not specified, the standard selection screen 1000 is called. The specified seclection screen must exist, otherwise a runtime error occurs.
- `VIA SELECTION-SCREEN`: Determines whether the selection is displayed or not.
- The following sections outline options to pass parameter and selection criteria values.

 </td>

<td> 

The following code snippets, copyable to demo programs and executable using F8 (execute program `zdemo_abap_report`), implements the following:
- Using radio buttons in the calling program, you can determine which `SUBMIT` statement to execute. 
- All statements use the `AND RETURN` addition.
- Some statements anticipate the `WITH` addition with which you can pass values for parameters and selection criteria.
- As there is no "returning parameter-like" functionality when returning to the calling program, the called program uses an `EXPORT ... TO MEMORY ID` statement to store parameter values in a string table within the ABAP memory. For display and demonstration, the calling program retrieves this stored content with an `IMPORT ... TO MEMORY ID` statement. It then writes the content to a list using `WRITE` statements. Find more information and criteria for the use [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendata_cluster.htm).


`zdemo_abap_report`
``` abap
PROGRAM.

PARAMETERS: use_sel  RADIOBUTTON GROUP rbg,
            use_selp RADIOBUTTON GROUP rbg,
            use_sels RADIOBUTTON GROUP rbg,
            sel_via  RADIOBUTTON GROUP rbg,
            onlyvia  RADIOBUTTON GROUP rbg.

DATA param1 TYPE c LENGTH 10.
DATA param2 TYPE c LENGTH 10.

START-OF-SELECTION.

  CASE 'X'.
    WHEN use_sel.

      "In this example, the called selection screen is not displayed. As no parameter values
      "are passed (see next example), none of the parameters in the called program are assigned
      "values.
      SUBMIT zdemo_abap_report_submit USING SELECTION-SCREEN 9000 AND RETURN.

    WHEN use_selp.

      "The following statement does not specify VIA SELECTION-SCREEN either, but anticipates
      "the WITH addition to pass parameter values.
      SUBMIT zdemo_abap_report_submit WITH param2 = 'hello'
                                      USING SELECTION-SCREEN 9000 AND RETURN.

    WHEN use_sels.

      "This example statement explicitly calls the standard selection screen.
      SUBMIT zdemo_abap_report_submit WITH param1 = 'hi'
                                      USING SELECTION-SCREEN 1000 AND RETURN.

    WHEN sel_via.

      "The specified selection screen is called.
      SUBMIT zdemo_abap_report_submit USING SELECTION-SCREEN 9000
                                      VIA SELECTION-SCREEN
                                      AND RETURN.

    WHEN onlyvia.

      "The standard selection screen 1000 is called.
      SUBMIT zdemo_abap_report_submit VIA SELECTION-SCREEN
                                      AND RETURN.


  ENDCASE.

  DATA input_values TYPE string_table.
  IMPORT input = input_values FROM MEMORY ID 'DEMO_ID'.

  WRITE / |This text was written to the list in program { sy-repid }.|.
  LOOP AT input_values INTO DATA(wa).
    WRITE / wa.
  ENDLOOP.
``` 

<br>

`zdemo_abap_report_submit`
``` abap
PROGRAM.

PARAMETERS param1 TYPE c LENGTH 10 LOWER CASE.

SELECTION-SCREEN BEGIN OF SCREEN 9000.
  PARAMETERS param2 TYPE c LENGTH 10 LOWER CASE.
SELECTION-SCREEN END OF SCREEN 9000.

START-OF-SELECTION.

  DATA input_values TYPE string_table.
  APPEND |param1: "{ param1 }"| TO input_values.
  APPEND |param2: "{ param2 }"| TO input_values.

  EXPORT input = input_values TO MEMORY ID 'DEMO_ID'.

  MESSAGE |Hello from program { sy-repid }, { sy-uname }. The parameter values are param1 = "{ param1 }" and param2 = "{ param2 }".| TYPE 'I'.
```  

 </td>
</tr>

<tr>
<td> 

Passing values to a called selection screen<br><br>
Additions `WITH SELECTION-TABLE`, `WITH ...` 

 </td>

 <td> 

- `WITH SELECTION-TABLE`: Passes values specified in an internal of a special line type (`RSPARAMS`, `RSPARAMSL_255`, without secondary table keys). The line type has specific component, among them: `selname` (parameter name in uppercase), `kind` (`P` for parameters, `S` for selection criteria), `sign`/`option`/`low`/`high` (ranges table components; note that for parameters, the value must be specified for `low`) 
- `WITH ...` : Supplying individual values
- More additions are available. Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsubmit_selscreen_parameters.htm):
  - `WITH FREE SELECTIONS`: For dynamic selections of selection screens of [logical databases](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlogical_data_base_glosry.htm)
  - `USING SELECTION-SET`: Passes values of a [selection screen variant](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenvariant_glosry.htm) 
  - `USING SELECTION-SETS OF PROGRAM`: Passes values of a selection screen variant of a particular program

 </td>

<td> 

The following code snippets, copyable to demo programs and executable using F8 (execute program `zdemo_abap_report`), implements the following:
- Using radio buttons in the calling program, you can determine which `SUBMIT` statement to execute. 
- The `SUBMIT` statements do not specify the `VIA SELECTION-SCREEN` addition on purpose. Parameter values and selection criteria are predefined in the calling program and then used by the `SUBMIT` statements.
- As there is no "returning parameter-like" functionality when returning to the calling program, the called program uses an `EXPORT ... TO MEMORY ID` statement to store parameter values in a string table within the ABAP memory. For display and demonstration, the calling program retrieves this stored content with an `IMPORT ... TO MEMORY ID` statement. It then writes the content to a list using `WRITE` statements. Find more information and criteria for the use [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendata_cluster.htm).


`zdemo_abap_report`
``` abap
PROGRAM.

PARAMETERS: wseltab1 RADIOBUTTON GROUP rbg,
            wseltab2 RADIOBUTTON GROUP rbg,
            wseltab3 RADIOBUTTON GROUP rbg,
            simple_w RADIOBUTTON GROUP rbg,
            with_sel RADIOBUTTON GROUP rbg,
            singlrt  RADIOBUTTON GROUP rbg,
            intv_rt  RADIOBUTTON GROUP rbg,
            rangest  RADIOBUTTON GROUP rbg.

START-OF-SELECTION.

  CASE 'X'.
    WHEN wseltab1.

      DATA seltab1 TYPE TABLE OF rsparams WITH EMPTY KEY.
      seltab1 = VALUE #( ( selname = 'PARAM2' kind = 'P' low = 'Test' ) ).
      SUBMIT zdemo_abap_report_submit USING SELECTION-SCREEN 9000 WITH SELECTION-TABLE seltab1 AND RETURN.

    WHEN wseltab2.

      DATA seltab2 TYPE TABLE OF rsparams WITH EMPTY KEY.
      seltab2 = VALUE #( ( selname = 'SEL1' kind = 'S' low = 'A' high = 'M' option = 'BT' sign = 'I' ) ).
      SUBMIT zdemo_abap_report_submit USING SELECTION-SCREEN 9001 WITH SELECTION-TABLE seltab2 AND RETURN.

    WHEN wseltab3.

      DATA seltab3 TYPE TABLE OF rsparams WITH EMPTY KEY.
      seltab3 = VALUE #( ( selname = 'PARAM3' kind = 'P' low = 'Hello' )
                         ( selname = 'SEL2' kind = 'S' low = 1 high = 10 option = 'BT' sign = 'I' ) ).
      SUBMIT zdemo_abap_report_submit USING SELECTION-SCREEN 9002 WITH SELECTION-TABLE seltab3 AND RETURN.

    WHEN simple_w.

      "Calls the standard selection screen
      SUBMIT zdemo_abap_report_submit WITH param1 = 'ABAP' AND RETURN.

    WHEN with_sel.

      SUBMIT zdemo_abap_report_submit USING SELECTION-SCREEN 9000 WITH param2 = 'lorem' AND RETURN.

    WHEN singlrt.

      "Single value passed for ranges table
      "Note:
      "- Specifying SIGN is optional. The default value for sign is 'I'.
      "- Specify the comparions operations EQ|NE|CP|NP|GT|GE|LT|LE and =|INCL instead of EQ
      SUBMIT zdemo_abap_report_submit USING SELECTION-SCREEN 9001 WITH sel1 = 'A' SIGN 'I' AND RETURN.

    WHEN intv_rt.

      "Passed interval for ranges table
      SUBMIT zdemo_abap_report_submit USING SELECTION-SCREEN 9001 WITH sel1 BETWEEN 'A' AND 'M' SIGN 'I' AND RETURN.

    WHEN rangest.

      "Passing a ranges table
      TYPES c10 TYPE c LENGTH 10.
      DATA rangetab TYPE RANGE OF c10.
      rangetab = VALUE #( sign   = 'I'
                          option = 'BT' ( low = 'A'  high = 'C' )
                                        ( low = 'F'  high = 'M' )
                                        ( low = 'S'  high = 'U' )
                          option = 'GE' ( low = 'X' ) ).

      SUBMIT zdemo_abap_report_submit USING SELECTION-SCREEN 9001 WITH sel1 IN rangetab AND RETURN.

  ENDCASE.

  DATA input_tab TYPE string_table.
  IMPORT values = input_tab FROM MEMORY ID 'ID4INPUT'.

  LOOP AT input_tab INTO DATA(wa).
    SET BLANK LINES ON.
    WRITE / wa.
  ENDLOOP.
``` 

<br>

`zdemo_abap_report_submit`
``` abap
PROGRAM.

PARAMETERS param1 TYPE c LENGTH 10 LOWER CASE.

SELECTION-SCREEN BEGIN OF SCREEN 9000.
  PARAMETERS param2 TYPE c LENGTH 10 LOWER CASE.
SELECTION-SCREEN END OF SCREEN 9000.

DATA txt TYPE c LENGTH 10.
SELECTION-SCREEN BEGIN OF SCREEN 9001.
  SELECT-OPTIONS: sel1 FOR txt.
SELECTION-SCREEN END OF SCREEN 9001.

DATA num TYPE i.
SELECTION-SCREEN BEGIN OF SCREEN 9002.
  PARAMETERS param3 TYPE c LENGTH 10 LOWER CASE.
  SELECT-OPTIONS: sel2 FOR num.
SELECTION-SCREEN END OF SCREEN 9002.

START-OF-SELECTION.

  DATA input_values TYPE string_table.

  APPEND |param1: "{ param1 }"| TO input_values.
  APPEND INITIAL LINE TO input_values.
  APPEND |param2: "{ param2 }"| TO input_values.
  APPEND INITIAL LINE TO input_values.
  APPEND |param3: "{ param3 }"| TO input_values.
  APPEND INITIAL LINE TO input_values.

  IF sel1 IS INITIAL.
    APPEND `No sel1 selection criteria.` TO input_values.
  ELSE.
    APPEND `sel1 selection criteria:` TO input_values.
    LOOP AT sel1 ASSIGNING FIELD-SYMBOL(<st1>).
      APPEND |low: { <st1>-low }, high: { <st1>-high }, option: { <st1>-option }, sign: { <st1>-sign }| TO input_values.
    ENDLOOP.
  ENDIF.
  APPEND INITIAL LINE TO input_values.
  IF sel2 IS INITIAL.
    APPEND `No sel2 selection criteria.` TO input_values.
  ELSE.
    APPEND `sel2 selection criteria:` TO input_values.
    LOOP AT sel2 ASSIGNING FIELD-SYMBOL(<st2>).
      APPEND |low: { <st2>-low }, high: { <st2>-high }, option: { <st2>-option }, sign: { <st2>-sign }| TO input_values.
    ENDLOOP.
  ENDIF.

  EXPORT values = input_values TO MEMORY ID 'ID4INPUT'.
``` 

 </td>
</tr>

<tr>
<td> 

Handling the list of the called program<br><br>
Addition `EXPORTING LIST TO MEMORY`

 </td>

 <td> 

- After a program has been called, the output statements of that program write to the [basic list](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenbasic_list_glosry.htm).
- If this addition is used, the output list is stored in the [ABAP memory](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_memory_glosry.htm) in an internal table of type `abaplist`. If it is not used, the list of the called program is displayed. That's why the examples above do not use `WRITE` statements in the called program.
- The addition must be used with `AND RETURN`.
- You can access the stored list in the calling program using these function modules: 
  - `LIST_FROM_MEMORY`: Retrieves the list from the ABAP memory in an internal table of type `abaplist`
  - `WRITE_LIST`: Writes the content of the table (of type `abaplist`) to the current list
  - `DISPLAY_LIST`: Displays the content the content of the table (of type `abaplist`) in a separate [list dynpro](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlist_dynpro_glosry.htm)
  - `LIST_TO_ASCI`: Converts the content the content of the table (of type `abaplist`) to ASCII representation
- Find more information in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsubmit_list_options.htm). More additions are available to handle lists:
  - `TO SAP-SPOOL`: Sends the output list to the SAP spool system as spool list
  - `LINE-SIZE`: Defines the line width of the basic list
  - `LINE-COUNT`: Defines the page length the basic list


 </td>

<td> 

The following code snippets, copyable to demo programs and executable using F8 (execute program `zdemo_abap_report`), implements the following:
- Using radio buttons in the calling program, you can determine which `SUBMIT` statement to execute and how the list is handled afterwards. 
- The examples cover the calling of the function modules mentioned above.

`zdemo_abap_report`

```abap
PROGRAM.

PARAMETERS: expwrite RADIOBUTTON GROUP rbg,
            expdispl RADIOBUTTON GROUP rbg,
            expascii RADIOBUTTON GROUP rbg.

DATA list_tab TYPE TABLE OF abaplist.

START-OF-SELECTION.

  CASE 'X'.
    WHEN expwrite.

      SUBMIT zdemo_abap_report_submit WITH param = 'WRITE_LIST'
                                      EXPORTING LIST TO MEMORY AND RETURN.

      CALL FUNCTION 'LIST_FROM_MEMORY'
        TABLES
          listobject = list_tab
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2.

      IF sy-subrc = 0.
        CALL FUNCTION 'WRITE_LIST'
          TABLES
            listobject = list_tab.
      ELSE.
        WRITE / `List not found.`.
      ENDIF.

      WRITE / |This text was written in the calling program { sy-repid }.|.

    WHEN expdispl.

      SUBMIT zdemo_abap_report_submit WITH param = 'DISPLAY_LIST'
                                      EXPORTING LIST TO MEMORY AND RETURN.

      CALL FUNCTION 'LIST_FROM_MEMORY'
        TABLES
          listobject = list_tab
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2.

      IF sy-subrc = 0.
        CALL FUNCTION 'DISPLAY_LIST'
          TABLES
            listobject = list_tab
          EXCEPTIONS
            empty_list = 1
            OTHERS     = 2.
        IF sy-subrc <> 0.
          WRITE / `Cannot display list.`.
        ENDIF.
      ELSE.
        WRITE / `List not found.`.
      ENDIF.

      WRITE / |This text was written in the calling program { sy-repid }.|.

    WHEN expascii.

      SUBMIT zdemo_abap_report_submit WITH param = 'LIST_TO_ASCI'
                                      EXPORTING LIST TO MEMORY AND RETURN.

      CALL FUNCTION 'LIST_FROM_MEMORY'
        TABLES
          listobject = list_tab
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2.

      TYPES c255 TYPE c LENGTH 255.
      DATA list_ascii TYPE TABLE OF c255 WITH EMPTY KEY.

      IF sy-subrc = 0.
        CALL FUNCTION 'LIST_TO_ASCI'
          TABLES
            listasci           = list_ascii
            listobject         = list_tab
          EXCEPTIONS
            empty_list         = 1
            list_index_invalid = 2
            OTHERS             = 3.
        IF sy-subrc <> 0.
          WRITE / `Cannot convert list.`.
        ELSE.
          WRITE / `List converted to ASCII representation:`.
          LOOP AT list_ascii INTO DATA(wa_ascii).
            WRITE / wa_ascii.
          ENDLOOP.
        ENDIF.
      ELSE.
        WRITE / `List not found.`.
      ENDIF.

      SKIP.
      WRITE / |This text was written in the calling program { sy-repid }.|.

  ENDCASE.
```


<br>

`zdemo_abap_report_submit`

```abap
PROGRAM.

PARAMETERS param TYPE c LENGTH 20.

START-OF-SELECTION.

  IF param IS INITIAL.
    WRITE / `No value passed for parameter "param".`.
  ELSE.
    WRITE / |param: "{ param }"|.
  ENDIF.
  SKIP.
  WRITE / |Some text written in the called program { sy-repid }|.
  DO 10 TIMES.
    WRITE / |Text no. { sy-index } added to list.|.
  ENDDO.
```


 </td>
</tr>



<tr>
<td> 

Running a program in the background<br><br>
Addition `VIA JOB ... NUMBER ...` 

 </td>

 <td> 

- Must be used with `AND RETURN`
- Background jobs can be created programmatically using function modules. The example uses some of them, e.g. for creating, completing, and analyzing background jobs.
- See the documentation for the [details](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsubmit_via_job.htm).

 </td>

<td> 

The following code snippets, copyable to demo programs and executable using F8 (execute program `zdemo_abap_report`), implements the following:
- A program is scheduled as background task, and then completed and released.
- The program execution is interrupted for one second. Then, function modules are used to analyze the background job.
- Using the `cl_demo_output` class for demo output purposes, text and the content of data objects are displayed.
- The transaction `SMX` is called to view the job overview. In the *Spool* column, you can click the icon to display the list output. On the next screen, the list of spool request is displayed. Choose the icon in the *Type* column. The `sy-batch` should be not initial denoting the run in the background.


`zdemo_abap_report`

```abap
PROGRAM.

PARAMETERS callbgr TYPE c AS CHECKBOX DEFAULT 'X'.

DATA: number           TYPE tbtcjob-jobcount,
      name             TYPE tbtcjob-jobname VALUE 'DEMO_JOB',
      print_parameters TYPE pri_params.

START-OF-SELECTION.

  IF callbgr IS INITIAL.

    MESSAGE `You chose not to run the called program in the background.` TYPE 'I'.
    SUBMIT zdemo_abap_report_submit WITH param = 'not in background' AND RETURN.

  ELSE.

    MESSAGE `You chose to run the called program in the background.` TYPE 'I'.

    "Creating background job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = name
      IMPORTING
        jobcount         = number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc = 0.
      "Scheduling a program as a background task using the number created previously
      "and the name specified
      SUBMIT zdemo_abap_report_submit WITH param = 'in background'
                                      TO SAP-SPOOL
                                      SPOOL PARAMETERS print_parameters
                                      WITHOUT SPOOL DYNPRO
                                      VIA JOB name NUMBER number
                                      AND RETURN.

      IF sy-subrc = 0.
        "Completing the background job
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = number
            jobname              = name
            strtimmed            = 'X'
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.
        IF sy-subrc = 0.
          cl_demo_output=>write( |The job { name } was submitted with number { number }| ).

          "Interrupting the program shortly
          WAIT UP TO 1 SECONDS.

          "Analyzing the background job
          DATA: BEGIN OF jobstate,
                  aborted     TYPE c LENGTH 1,
                  finished    TYPE c LENGTH 1,
                  preliminary TYPE c LENGTH 1,
                  ready       TYPE c LENGTH 1,
                  running     TYPE c LENGTH 1,
                  scheduled   TYPE c LENGTH 1,
                  suspended   TYPE c LENGTH 1,
                  other       TYPE c LENGTH 1,
                END OF jobstate.

          CALL FUNCTION 'SHOW_JOBSTATE'
            EXPORTING
              jobcount         = number
              jobname          = name
            IMPORTING
              aborted          = jobstate-aborted
              finished         = jobstate-finished
              preliminary      = jobstate-preliminary
              ready            = jobstate-ready
              running          = jobstate-running
              scheduled        = jobstate-scheduled
              suspended        = jobstate-suspended
              other            = jobstate-other
            EXCEPTIONS
              jobcount_missing = 1
              jobname_missing  = 2
              job_notex        = 3
              OTHERS           = 4.
          IF sy-subrc = 0.
            cl_demo_output=>write( jobstate ).
          ELSE.
            cl_demo_output=>write( `Exception in SHOW_JOBSTATE` ).
          ENDIF.

          DATA joblog TYPE TABLE OF tbtc5.
          CALL FUNCTION 'BP_JOBLOG_READ'
            EXPORTING
              jobcount              = number
              jobname               = name
            TABLES
              joblogtbl             = joblog
            EXCEPTIONS
              cant_read_joblog      = 1
              jobcount_missing      = 2
              joblog_does_not_exist = 3
              joblog_is_empty       = 4
              joblog_name_missing   = 5
              jobname_missing       = 6
              job_does_not_exist    = 7
              OTHERS                = 8.
          IF sy-subrc = 0.
            cl_demo_output=>write( joblog ).
          ELSE.
            cl_demo_output=>write( `Exception in BP_JOBLOG_READ` ).
          ENDIF.

        ELSE.
          cl_demo_output=>write( `Exception in JOB_CLOSE` ).
        ENDIF.
      ELSE.
        DATA(msg) = cl_abap_submit_handling=>get_error_message( ).
        cl_demo_output=>write( msg ).
      ENDIF.
    ENDIF.

    cl_demo_output=>display( ).
    MESSAGE `You are redirected to transaction SMX.` TYPE 'I'.
    CALL TRANSACTION 'SMX'.
  ENDIF.
```


<br>

`zdemo_abap_report_submit`

```abap
PROGRAM.

PARAMETERS param TYPE c LENGTH 20 LOWER CASE.

START-OF-SELECTION.

  IF param IS INITIAL.
    WRITE / `No value passed for parameter "param".`.
  ELSE.
    WRITE / |param: "{ param }"|.
  ENDIF.
  SKIP.
  WRITE / |Some text written in the called program { sy-repid }|.
  DO 10 TIMES.
    WRITE / |Text no. { sy-index } added to list.|.
  ENDDO.

  SKIP.
  WRITE / |Value of sy-batch: "{ sy-batch }"|.
```


 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## ABAP Statements for Classic Lists

### Creating Lists with WRITE Statements

- Lists consist of consecutive list lines that are filled one after the other using the statement [`WRITE`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapwrite-.htm).
- This way, you can output mostly flat data objects and strings/xstrings, types converted to a character-like value (i.e. no internal table content or structures directly).
- Each time a data object is output, an output length is defined, either implicitly or explicitly (see [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenwrite_output_length.htm)).
- The following snippets show a selection of additions to `WRITE` statements, covering positioning, creating special list elements, and formatting options.



<table>

<tr>
<th> Subject/Addition </th> <th> Notes </th> <th> Code Snippet </th>
</tr>

<tr>
<td> 

Writing content

 </td>

 <td> 

- You can write flat data objects (including flat structures with character-like components), data objects of type `string` and `xstring` and enumerated types.
- Also writable: result of function calls, string expressions and method calls
- Writing expressions in string templates: They are useful for writing embedded arithmetic or bit expressions as these cannot be specified directly. Note that control characters such as `\n` for new lines are ignored.
- Positioning options are avaialble - as outlined below - for the `WRITE` statement. Among them is `/` that places the output in the next line. Not specifying it means the output is written in the same line.

 </td>

<td> 


``` abap
"Writing in the same line
"The example uses a character literal, i.e. an unnamed data object.
"Note that there is a separating blank in the list output.
WRITE 'Hello'.
WRITE 'world'.

"Using named data objects and string expressions
DATA some_text TYPE string VALUE `Hi`.
WRITE some_text && `, ` && sy-uname && `.`.

"Writing to a new line with /
WRITE / 'New line'.

"More flat data objects
"Note the default positioning of the output regarding
"the numeric types.
WRITE / CONV decfloat34( '1.23456789' ).
TYPES n5 TYPE n LENGTH 5.
DATA numc5 TYPE n5 VALUE '12345'.
WRITE / numc5.
DATA number TYPE i VALUE 123.
WRITE / number.

"Flat structure with character-like components
"Note the conversion rules.
DATA: BEGIN OF flat_struc,
        comp1 TYPE c LENGTH 4 VALUE `ABAP`,
        comp2 TYPE n LENGTH 3 VALUE '987',
        comp3 TYPE abap_boolean VALUE 'X',
        comp4 TYPE d VALUE '20250101',
        comp5 TYPE t VALUE '123456',
      END OF flat_struc.

WRITE / flat_struc.

"String templates
WRITE / |Today's date is { sy-datum }.|.
"Formatting options with string templates
WRITE / |{ sy-uname WIDTH = 20 ALIGN = RIGHT CASE = LOWER }|.
"Arithmetic expressions cannot be specified directly. You may use
"the expressions in string templates.
"WRITE / 1 + 2.
WRITE / |{ 1 + 2 }|.
WRITE / |{ CONV decfloat34( 1 / 3 ) DECIMALS = 3 }|.

"Concatenating using &&
WRITE / `conc` && `atenated`.

"Function calls
"Note: You can use chained statements with a colon.
WRITE: / to_upper( 'abap' ),
       / repeat( val = `*` occ = 50 ),
       / replace( val = `He##o wor#d` sub = `#` with = `l` occ = 0 ).

"Result of method calls
"The example uses the retrieval of a random integer between 1 and 10
WRITE / cl_abap_random_int=>create(
              seed = cl_abap_random=>seed( )
              min  = 1
              max  = 10 )->get_next( ).

"Note the * specification. The returned value is of type i. Specifying *
"for the length means the length required to output the current value
"is used.
WRITE /(*) cl_abap_random_int=>create(
              seed = cl_abap_random=>seed( )
              min  = 1
              max  = 10 )->get_next( ).
``` 

 </td>
</tr>


<tr>
<td> 

Positioning options (1)<br><br>
Additions `/`, `AT ... position(length|*|**)`

 </td>

 <td> 

- `/`: Places the output in the next line. Not specifying it means the output is written in the same line.
- `AT`: 
  - Explicitly specifying the output position and length
  - `AT` can be omitted if the position and length are specified as numeric literals.
  - The position should not be greater than the list width; otherwise, there is not output. A value less than 1 is ignored.
  - The length value should also be between 1 and the list width.
  - The length specifications `*` and `**` have characteristics specific to data types that are output. They are particularly useful for ensuring that all characters are output (`*`: minimum output: `**`: maximum output). Find information in the [documentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenarithmetic_expression_glosry.htm) and [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenwrite_output_length.htm) regarding the output length. 
- `WRITE /.` works like `SKIP.`.

 </td>

<td> 


``` abap
*&---------------------------------------------------------------------*
*& WRITE with and without /
*&---------------------------------------------------------------------*

"Writing output with and without specifying /
WRITE 'Hello'.
WRITE 'world'.
WRITE / 'new line'.

*&---------------------------------------------------------------------*
*& Specifying the output position with(out) AT, position and length
*&---------------------------------------------------------------------*

WRITE / repeat( val = `*` occ = 80 ).
"WRITE statement working like SKIP.
WRITE /.

"Specifying the output position, a length and new line
"specification
WRITE 20 'test'.
"Note the overwriting in the following example.
WRITE / 'Lorem ipsum dolor sit amet'.
WRITE 10 '####'.

"Specifying the length, ommitting position specification
"This means that the output is written from the first column on.
WRITE /(10) 'Not displayed completely'.

"Specifying both length and position
WRITE /5(30) 'This text displayed completely starting from position 5.'.

"The AT addition can only be ommitted if position and length are specified
"as numeric literals as above.
WRITE AT /3(20) 'lorem ipsum'.
WRITE /3(20) 'dolor sit amet'.
"AT specification required
DATA position TYPE i VALUE 10.
DATA length TYPE i VALUE 50.
WRITE AT /position(length) |This text is starts from position { position }. Length specification { length }.|.

"Length specifications with * or **. In that case, the output length
"depends on the data type of the data object. There are special rules.
"See the ABAP Keyword Documentation for the details.

"Example with type c
DATA hi TYPE c LENGTH 10 VALUE 'Hello     '.

WRITE: / hi,
         sy-uname.
"Minimum output (trailing blanks are ignored)
WRITE: /(*) hi,
         sy-uname.
"Maximum output (doubled length of data object)
WRITE: /(**) hi,
         sy-uname.

"Example with type i
DATA some_number TYPE i VALUE 123.

WRITE: / some_number,
         '<- some_number'.
"Minimum length required to output content
WRITE: /(*) some_number,
            '<- some_number'.
"Maximum length to output content
WRITE: /(**) some_number,
             '<- some_number'.

"Example with type p
DATA some_packed_number TYPE p LENGTH 16 DECIMALS 14 VALUE '-123.4567890'.

WRITE: / some_packed_number,
         '<- some_packed_number'.
"Minimum length required to output content including separators
WRITE: /(*) some_packed_number,
            '<- some_packed_number'.
"Maximum length to output content including signs and separators
WRITE: /(**) some_packed_number,
             '<- some_packed_number'.
``` 

 </td>
</tr>

<tr>
<td> 

Positioning options (2)<br><br>
Additions `NO-GAP`, `UNDER`

 </td>

 <td> 

- `NO-GAP`: Positions directly after the output
- `UNDER`: Output in the position of previous output

 </td>

<td> 


``` abap
*&---------------------------------------------------------------------*
*& NO-GAP
*&---------------------------------------------------------------------*

WRITE / repeat( val = `*` occ = 80 ).

"Output: AB AP
WRITE / 'AB'.
WRITE 'AP'.
"Output: ABAP
WRITE: / 'AB' NO-GAP.
WRITE 'AP'.

*&---------------------------------------------------------------------*
*& UNDER
*&---------------------------------------------------------------------*

WRITE / repeat( val = `*` occ = 80 ).

"Note: If the output is written in the same line in which the previous output
"is displayed, this output is overwritten.
DATA abc TYPE c LENGTH 5 VALUE 'abcde'.
WRITE /5(5) abc.
WRITE / 'fghij' UNDER abc.

"Output in tabular form
DATA: carrid   TYPE scarr-carrid,
      carrname TYPE scarr-carrname,
      currcode TYPE scarr-currcode,
      url      TYPE scarr-url.

WRITE: /3 'Carrier', 12 'Name', 35 'Currency Code', 50 'URL'.
WRITE / repeat( val = `=` occ = 80 ).
SELECT carrid, carrname, currcode, url
       FROM scarr
       INTO @DATA(scarr_wa).
  WRITE: / scarr_wa-carrid UNDER 'Carrier',
           scarr_wa-carrname UNDER 'Name',
           scarr_wa-currcode UNDER 'Currency Code',
           scarr_wa-url UNDER 'URL'.
ENDSELECT.
WRITE / .

"Excursion: Writing an internal table in table-like fashion. As a prerequisite,
"all values are convertible to type string. The following example does not use
"UNDER, but calculates position and length values. It is an exploration and not 
"meant to present a best practice example, particularly because other options 
"such as ALV are available for that purpose. It may not be suitable for each 
"line and component type.

"Demo internal table
TYPES: BEGIN OF demo_struc,
         chars TYPE c LENGTH 5,
         num   TYPE i,
         dec   TYPE decfloat34,
         text  TYPE string,
         flag  TYPE abap_boolean,
         date  TYPE d,
       END OF demo_struc,
       ty_tab TYPE TABLE OF demo_struc WITH EMPTY KEY.

DATA(itab) = VALUE ty_tab( ( chars = 'a' num = 123 dec = CONV decfloat34( '-123.456' ) text = `Hello world` flag = abap_true date = CONV d( '20250101' ) )
                           ( chars = 'bcd' num = 4 dec = CONV decfloat34( '7.8945612' ) text = `Lorem ipsum` flag = abap_false date = CONV d( '20251227' ) )
                           ( chars = 'efghi' num = 987654 dec = CONV decfloat34( '1.01' ) text = `ABAP` flag = abap_true date = CONV d( '20250621' ) )
                           ( chars = 'jk' num = 321 dec = CONV decfloat34( '0.00000000000001' ) text = `test` flag = abap_true date = CONV d( '20251015' ) ) ).

TYPES: BEGIN OF comp_struc,
         name TYPE string,
         len  TYPE i,
         off  TYPE i,
         dref TYPE REF TO string,
       END OF comp_struc.
DATA it_comps TYPE TABLE OF comp_struc WITH EMPTY KEY.
DATA str TYPE string.

TRY.
    DATA(type_descr_obj_tab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab ) ).
    DATA(tab_comps) = CAST cl_abap_structdescr( type_descr_obj_tab->get_table_line_type( ) )->get_components( ).
    LOOP AT tab_comps ASSIGNING FIELD-SYMBOL(<comp>).
      APPEND VALUE #( name = <comp>-name len = strlen( <comp>-name ) ) TO it_comps.
    ENDLOOP.
  CATCH cx_sy_move_cast_error INTO DATA(error).
    WRITE  / error->get_text( ).
ENDTRY.

IF error IS INITIAL.
  "Implementation for properly aligning the content
  "The example is implemented to check the length of the column names as well as the
  "length of the values in the columns. It determines the length of the longest string
  "in each column. Depending on the length values, either the length of the column name
  "or the length of the longest string in a column is stored in an internal table that
  "contains information for calculating the offset.
  LOOP AT tab_comps ASSIGNING FIELD-SYMBOL(<len>).
    ASSIGN it_comps[ name = <len>-name ] TO FIELD-SYMBOL(<co>).
    DATA(max_content) = REDUCE i( INIT len = <co>-len
                                  FOR <line> IN itab
                                  NEXT len = COND #( LET lv = |{ <line>-(<co>-name) }| IN
                                                     WHEN strlen( lv ) > len
                                                     THEN strlen( lv )
                                                     ELSE len ) ).
    "Extend the length value to leave some more space
    IF max_content > <co>-len.
      <co>-len = max_content + 3.
    ELSE.
      <co>-len += 3.
    ENDIF.
  ENDLOOP.
  "Calculating offset values
  DATA max_str_len TYPE i.
  LOOP AT it_comps ASSIGNING FIELD-SYMBOL(<off>).
    DATA(tabix) = sy-tabix.
    READ TABLE it_comps INDEX tabix - 1 ASSIGNING FIELD-SYMBOL(<prev>).
    <off>-off = COND #( WHEN tabix = 1 THEN 0 ELSE <prev>-len + <prev>-off ).
    max_str_len += <off>-len.
  ENDLOOP.
  "Providing enough space so that table row content can be inserted based on
  "the offset specification
  SHIFT str BY max_str_len PLACES RIGHT.
  "Adding the column names first
  LOOP AT it_comps ASSIGNING FIELD-SYMBOL(<header>).
    str = insert( val = str sub = <header>-name off = <header>-off ).
  ENDLOOP.
  WRITE / str.
  "Processing all lines in the internal table containing the retrieved table rows
  LOOP AT itab ASSIGNING FIELD-SYMBOL(<wa>).
    CLEAR str.
    SHIFT str BY max_str_len PLACES RIGHT.
    DO.
      TRY.
          str = insert( val = str sub = <wa>-(sy-index) off = it_comps[ sy-index ]-off ).
        CATCH cx_sy_assign_illegal_component cx_sy_range_out_of_bounds cx_sy_itab_line_not_found.
          EXIT.
      ENDTRY.
    ENDDO.
    WRITE / str.
  ENDLOOP.
ENDIF.
``` 

 </td>
</tr>


<tr>
<td> 

Adding quickinfo<br><br>
Addition `QUICKINFO`

 </td>

 <td> 

Creates a tooltip for the output

 </td>

<td> 


``` abap
WRITE / repeat( val = `*` occ = 80 ).
WRITE / 'Place the cursor on the following output and check the tooltip:'.
WRITE: /(*) sy-uname QUICKINFO 'User name',
        '/',
        (*) sy-datum QUICKINFO 'Current date',
        '/',
        (*) sy-uzeit QUICKINFO 'Current time'.
``` 

 </td>
</tr>


<tr>
<td> 

Adding list elements such as checkboxes, icons, symbols, and frames<br><br>
Additions `AS CHECKBOX`, `AS ICON`, `AS SYMBOL`, `AS LINE`

 </td>

 <td> 

- `AS CHECKBOX`: The value (blank, or `X`) is stored in the list buffer and can be evaluated during a list event.
- `AS ICON`: Outputs icons. Check the type pool `ICON` for names of predefined icons.
- `AS SYMBOL`: Outputs symbols. Check the type pool `SYM` for names of predefined symbols. 
- `AS LINE`: Outputs corners, crosses, lines, and T sections. Check the type pool `LINE` for names of predefined options. 

 </td>

<td> 


``` abap
PROGRAM.

DATA: flag          TYPE c LENGTH 1,
      checkbox_name TYPE c LENGTH 10,
      wa            TYPE c LENGTH 20.

START-OF-SELECTION.

*&---------------------------------------------------------------------*
*& AS CHECKBOX
*&---------------------------------------------------------------------*

  DO 5 TIMES.
    checkbox_name = |Checkbox { sy-index }|.
    WRITE: / flag AS CHECKBOX, (10) checkbox_name.
  ENDDO.

*&---------------------------------------------------------------------*
*& AS ICON
*&---------------------------------------------------------------------*

  WRITE / icon_green_light AS ICON.
  WRITE / icon_red_light AS ICON.
  WRITE / icon_yellow_light AS ICON.
  WRITE / icon_activate AS ICON.

*&---------------------------------------------------------------------*
*& AS SYMBOL
*&---------------------------------------------------------------------*

  WRITE / sym_left_hand AS SYMBOL.
  WRITE / sym_caution AS SYMBOL.

*&---------------------------------------------------------------------*
*& AS LINE
*&---------------------------------------------------------------------*

  WRITE / line_horizontal_line AS LINE NO-GAP.
  WRITE  line_space AS LINE NO-GAP.
  WRITE  line_vertical_line AS LINE.

*&---------------------------------------------------------------------*
*& Evaluating the checkbox selections
*&---------------------------------------------------------------------*

AT LINE-SELECTION.

  WRITE / |You clicked line { sy-lilli }. |.
  WRITE / `Selected checkboxes:`.
    DO.
    READ LINE sy-index FIELD VALUE flag checkbox_name INTO wa.
    IF sy-subrc <> 0.
      EXIT.
    ELSEIF flag = 'X'.
      WRITE / wa.
    ENDIF.
  CLEAR wa.
  ENDDO.
``` 

 </td>
</tr>

<tr>
<td> 

Formatting options

 </td>

 <td> 

- Various additions are available to format output. 
- For example, they deal with aligning content, currency, unit, date and time-related formatting, among others. 
- Find more information in the ABAP Keyword Documentation [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapwrite_int_options.htm) and [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapwrite_ext_options.htm). See also the [`FORMAT`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapformat.htm) statement.

 </td>

<td> 

The example only shows a selection. Check the documentation for all additions.

``` abap
*&---------------------------------------------------------------------*
*& Additions for aligning content
*&---------------------------------------------------------------------*

WRITE /(10) 'X' RIGHT-JUSTIFIED. "'         X'
WRITE /(10) 'X' CENTERED.        "'    X     '
WRITE /(10) 'X' LEFT-JUSTIFIED.  "'X         '

*&---------------------------------------------------------------------*
*& NO-ZERO
*&---------------------------------------------------------------------*

DATA: numc10  TYPE n LENGTH 10 VALUE '123'.
WRITE /(10) numc10.
WRITE /(10) numc10 NO-ZERO.

*&---------------------------------------------------------------------*
*& DECIMALS
*&---------------------------------------------------------------------*

DATA: packed_num TYPE p LENGTH 8 DECIMALS 4 VALUE '9876.5432'.
WRITE /(*) packed_num.
WRITE /(*) packed_num DECIMALS 2.

*&---------------------------------------------------------------------*
*& COLOR
*&---------------------------------------------------------------------*

"The commented out value stands for the value that can also
"be directly specified in the syntax (except 0) or contained in
"a data object. There are several additions with many options.
WRITE / 'COL_BACKGROUND (0)' COLOR COL_BACKGROUND. "0 (GUI dependent)
WRITE / 'COL_HEADING (1)' COLOR COL_HEADING. "1 (gray-blue)
WRITE / 'COL_HEADING (1)' COLOR 1.
WRITE / 'COL_NORMAL (2)' COLOR COL_NORMAL. "2 (light gray)
WRITE / 'COL_TOTAL (3)' COLOR COL_TOTAL. "3 (yellow)
WRITE / 'COL_KEY (4)' COLOR COL_KEY. "4 (blue-green)
WRITE / 'COL_POSITIVE (5)' COLOR COL_POSITIVE. "5 (green)
WRITE / 'COL_NEGATIVE (6)' COLOR COL_NEGATIVE. "6 (red)
WRITE / 'COL_GROUP (7)' COLOR COL_GROUP. "7 (orange)
WRITE / 'COLOR 7 OFF' COLOR 7 OFF. "default color

"Setting the intensity of the background color.
"Note that many of the following additions can specify further
"additions such as ON and OFF. See the details in the documentation.
WRITE / 'COLOR 7 INTENSIFIED OFF' COLOR 7 INTENSIFIED OFF.
"INVERSE: When ON, the foreground, i.e. the output is displayed
"in the selected color
WRITE / 'COLOR 7 INVERSE ON' COLOR 7 INVERSE ON.
``` 

 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Formatting Output Using FORMAT Statements

- [`FORMAT`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapformat.htm) statements can be used to apply formattings to output statements.
- The formats are valid until reverted by a new `FORMAT` statement, using the additions `OFF` or `RESET`.
- Note that the additions for `FORMAT` are also available for `WRITE` statements and can be applied individually. See the ABAP Keyword Documentation for all details.

<table>

<tr>
<th> Subject/Addition </th> <th> Notes </th> <th> Code Snippet </th>
</tr>

<tr>
<td> 

Coloring output <br><br>
`COLOR` addition

 </td>

 <td> 

As shown above, multiple colors are available. Instead of `COL_*`, the numbers that represent the color specifications can also be specified.

 </td>

<td> 


``` abap
FORMAT COLOR COL_POSITIVE.
WRITE / 'ABC'.
WRITE / 'DEF'.
WRITE / 'GHI'.
FORMAT COLOR OFF.

WRITE / 'JKL'.
WRITE / 'MNO'.

FORMAT COLOR COL_KEY.
WRITE / 'PQR'.
WRITE / 'STU'.
FORMAT COLOR OFF.

WRITE / 'VWX'.
WRITE / 'YZ'.
``` 

 </td>
</tr>


<tr>
<td> 

Specifying a hotspot<br><br>
`HOTSPOT` addition

 </td>

 <td> 

- Displays the mouse pointer when hovering over an element. 
- Here, a single click has then the same effect as a double click (i.e. the selection of function key *F2*).

 </td>

<td> 


``` abap
PROGRAM.

START-OF-SELECTION.
  FORMAT HOTSPOT.
  DO 5 TIMES.
    WRITE / |{ sy-index } - some clickable colored text showing the mouse pointer.| COLOR 5.
  ENDDO.
  FORMAT HOTSPOT OFF.

  WRITE / `Some colored text that doesn't show the mouse pointer.` COLOR 6.

AT LINE-SELECTION.

  WRITE |You clicked line { sy-lilli }.|.
``` 

 </td>
</tr>

<tr>
<td> 

Specifying an input field<br><br>
`INPUT` addition

 </td>

 <td> 

Enables user input that can be evaluated during a list event.

 </td>

<td> 


``` abap
PROGRAM.

DATA: some_text   TYPE c LENGTH 100,
      line_number TYPE i.

START-OF-SELECTION.
  WRITE 'Make an entry:'.
  "Note: The input field is currently empty. Without the following statement,
  "the input field would not be displayed.
  SET BLANK LINES ON.
  FORMAT INPUT.
  WRITE / some_text.
  FORMAT INPUT OFF.

  WRITE / 'Submit' COLOR 5 HOTSPOT.

AT LINE-SELECTION.
  IF sy-lisel = 'Submit'.
    line_number = sy-lilli - 1.
    READ LINE line_number FIELD VALUE some_text.
    WRITE: 'Your entry:',
           / some_text.
  ENDIF.
``` 

 </td>
</tr>

<tr>
<td> 

Creating frames<br><br>
`FRAME` addition

 </td>

 <td> 

Defines whether the - and | characters are converted to line elements, producing continuous lines.

 </td>

<td> 


``` abap

FORMAT FRAMES ON.
WRITE: / '----',
       / '|  |',
       / '----'.
FORMAT FRAMES OFF.
``` 

 </td>
</tr>


<tr>
<td> 

Resetting formats<br><br>
`RESET` addition

 </td>

 <td> 

This addition sets all formatting settings for which the corresponding addition is not specified in the same `FORMAT` statement to the state `OFF` (exception: `FRAMES`).

 </td>

<td> 

In the following example, all settings are reset without specifying concrete additions (e.g. `INVERSE OFF`). Note the effects of such a statement (e.g. `INTENSIFIED`
is `ON` by default at program start. Now it is set to `OFF`. See the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapformat.htm) for more information. 


``` abap
WRITE / 'ABCD'.
FORMAT COLOR 7 INVERSE ON.
WRITE / 'DEFG'.
WRITE / 'HIJK'.
FORMAT RESET.
WRITE / 'LMNO'.
``` 

 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

### List Creation-Related ABAP Keywords and Additions

<table>

<tr>
<th> Subject/Keyword </th> <th> Notes </th> <th> Code Snippet </th>
</tr>

<tr>
<td> 

Creating a horizontal line<br><br>
`ULINE`

 </td>

 <td> 

Additions are available such as for position and length.

 </td>

<td> 


``` abap
ULINE.
ULINE AT 5(20).
``` 

 </td>
</tr>


<tr>
<td> 

Specifying if blank lines created using `WRITE` are displayed<br><br>
`SET BLANK LINES`

 </td>

 <td> 

- The statement must be specified with the additions `ON` and `OFF`.
- Note that blank lines are not displayed by default.

 </td>

<td> 


``` abap
DATA(itab) = VALUE string_table( ( `a` ) ( `b` ) ( `` ) ( `` )  ( `c` ) ( `` ) ( `d` ) ).

"Blank lines not output by default
LOOP AT itab INTO DATA(wa).
  WRITE / wa.
ENDLOOP.

WRITE / repeat( val = `*` occ = 80 ).

"Explicitly specifying that blank lines should not be output
SET BLANK LINES OFF.
LOOP AT itab INTO wa.
  WRITE / wa.
ENDLOOP.

WRITE / repeat( val = `*` occ = 80 ).

"Blank lines are output
SET BLANK LINES ON.
LOOP AT itab INTO wa.
  WRITE / wa.
ENDLOOP.
``` 

 </td>
</tr>

<tr>
<td> 

Setting the list cursor explicitly in another line<br><br>
`SKIP`, `SKIP TO LINE`

 </td>

 <td> 

You can create blank lines, but also set to the cursor to a specified line.

 </td>

<td> 


``` abap
*&---------------------------------------------------------------------*
*& Creating blank lines
*&---------------------------------------------------------------------*

WRITE / 'abc'.
"Skips 1 line
SKIP.
WRITE / 'def'.
"Specifying the number of lines to be skipped
SKIP 3.
WRITE / 'ghi'.
SKIP.

*&---------------------------------------------------------------------*
*& Positioning the list cursor in another line
*&---------------------------------------------------------------------*

"In the example, the list cursor is explicitly set to
"a particular number. Here, B and C are not output (i.e.
"overwritten with the output that follows).
WRITE / 'Text A'.
WRITE / 'Text B'.
"The line number of the previous WRITE is retrieved.
DATA(line) = sy-linno.
WRITE / 'Text C'.
SKIP TO LINE line.
WRITE / 'Text D'.
WRITE / 'Text E'.
WRITE / 'Text F'.
``` 

 </td>
</tr>


<tr>
<td> 

Setting the list cursor to the first position of the next line<br><br>
`NEW-LINE`

 </td>

 <td> 

Additions are available to affect the scrolling behavior

 </td>

<td> 


``` abap
WRITE / 'Next statement is NEW-LINE'.
NEW-LINE.
"The following output is in a new line.
WRITE 'WRITE statement without /'. 
``` 

 </td>
</tr>


<tr>
<td> 

Storing the content of a flat variable together with the current list line<br><br>
`HIDE`

 </td>

 <td> 

- You can read the stored content again during a list event.
- Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaphide.htm).

 </td>

<td> 


``` abap
PROGRAM.

TYPES: BEGIN OF s,
         num1 TYPE i,
         num2 TYPE i,
       END OF s,
       ty_int_tab TYPE TABLE OF s WITH EMPTY KEY.
DATA(itab) = VALUE ty_int_tab( ( num1 = 10 num2 = 5 )
                               ( num1 = 2346 num2 = 3475 )
                               ( num1 = 2 num2 = 4 )
                               ( num1 = 347 num2 = 823 )
                               ( num1 = 20 num2 = 54 ) ).

DATA: addition_result TYPE i.

START-OF-SELECTION.
  FORMAT HOTSPOT.
  LOOP AT itab INTO DATA(wa).
    addition_result = wa-num1 + wa-num2.
    WRITE / |{ wa-num1 } + { wa-num2 } = ...|.
    HIDE: addition_result.
  ENDLOOP.

AT LINE-SELECTION.
  WRITE (*) addition_result.
``` 

 </td>
</tr>

<tr>
<td> 

Creating a page break<br><br>
`RESERVE n LINES`

 </td>

 <td> 

- Relevant if there's not sufficient space on the list page between the last output and page end or footer.
- The page break creation raises the `END-OF-PAGE` event.
- Has an impact on the behavior of `BACK` statements as shown below.


 </td>

<td> 

- The following example creates multiple lines in a loop. 
- Additions to `REPORT` are used to cater for the example demonstration with a small page length: 
  - `NO STANDARD PAGE HEADING` suppresses the output of the standard page header in the basic list
  - `LINE-COUNT` determines the page length. The number of lines in parentheses stands for the number of lines in the page footer. The footer can be defined in the `END-OF-PAGE` block. Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapreport_list_options.htm).
- Due to the `RESERVE` statement, page breaks are created as there is not enough space left on the page. The loop writes 4 sample texts to the list and can be considered a line block. They should not be separated by page breaks.
- You can try and comment out the line with the `RESERVE` statement to check out the difference.

``` abap
REPORT NO STANDARD PAGE HEADING LINE-COUNT 7(2).

START-OF-SELECTION.
  DO 5 TIMES.
    RESERVE 4 LINES.
    WRITE: / 'text a', / 'text b', / 'text c', / 'text d'.
  ENDDO.

END-OF-PAGE.
  WRITE |--- { sy-pagno } ---|.
``` 

 </td>
</tr>


<tr>
<td> 

Positioning the list cursor in the first position of the first line of a logical unit<br><br>
`BACK`

 </td>

 <td> 

- Such a logical unit can be a line block determined using `RESERVE`
- Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapback.htm).

 </td>

<td> 

The following example demonstrates `BACK` with `RESERVE`.
Consider the `RESERVE 5 LINES` statements as defining a block of 5 lines. When using `BACK` after `RESERVE`, e.g. after a loop, you can put the suqsequent output to the first line of this block of lines.

``` abap
DATA int_for_back TYPE i.

WRITE / 'RESERVE without BACK:'.
int_for_back = sy-colno.
ULINE AT /(int_for_back).

RESERVE 5 LINES.

DO 5 TIMES.
  WRITE / sy-index.
ENDDO.

int_for_back = sy-colno.

WRITE AT int_for_back '   <- This is not number 1.'.
SKIP.

WRITE / 'RESERVE with BACK:'.
int_for_back = sy-colno.
ULINE AT /(int_for_back).

RESERVE 5 LINES.

DO 5 TIMES.
  WRITE / sy-index.
ENDDO.

int_for_back = sy-colno.

BACK.
WRITE AT int_for_back '   <- This should be number 1.'.
``` 

 </td>
</tr>


</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Processing Lists in List Buffers

- ABAP statements are available that allow lists to be processed that have already been saved as [screen lists](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenscreen_list_glosry.htm) in the [list buffer](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlist_buffer_glosry.htm).
- The following selection covers `READ [CURRENT] LINE` and `MODIFY [CURRENT] LINE`.


<table>

<tr>
<th> Subject/Keyword </th> <th> Notes </th> <th> Code Snippet </th>
</tr>

<tr>
<td> 

Reading content of lines<br><br>
`READ [CURRENT] LINE`

 </td>

 <td> 

- Assigns the content of a line stored in the list buffer to the system field `sy-lisel`.
- Allows other target fields to be specified.
- `sy-subrc` is set: 0 (line was read), other than 0 (line does not exist)
- Specifying the line with additions: 
  - `READ LINE` and a concrete line
  - `READ CURRENT LINE` to read the current line
- More additions are available regarding the list level and assigning the output of individual data objects to target fields. Find more information in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapread_line.htm).

 </td>

<td> 



``` abap
PROGRAM.

DATA: flag          TYPE c LENGTH 1,
      checkbox_name TYPE c LENGTH 10,
      flag_check    TYPE c LENGTH 1,
      target        TYPE c LENGTH 20.

START-OF-SELECTION.

  WRITE / 'Text A'.
  WRITE / 'Text B'.
  WRITE / 'Text C'.
  WRITE / 'Text D'.
  WRITE / 'Text E'.
  WRITE / 'Text F'.
  checkbox_name = 'Checkbox 1'.
  flag = 'X'.
  WRITE: / flag AS CHECKBOX, (10) checkbox_name.
  checkbox_name = 'Checkbox 2'.
  flag = 'X'.
  WRITE: / flag AS CHECKBOX, (10) checkbox_name.

AT LINE-SELECTION.

  WRITE / |You selected line { sy-lilli } (value of sy-lilli).|.
  "Accessing the clicked line using sy-lisel
  WRITE / |Value of sy-lisel: "{ sy-lisel }"|.

  SKIP.

*&---------------------------------------------------------------------*
*& READ LINE
*&---------------------------------------------------------------------*

  WRITE / `---------------- READ LINE ----------------`.

  "Reading the content of the clicked line using READ LINE
  READ LINE sy-lilli.
  WRITE / |Value of sy-lisel after "READ LINE sy-lisel.": "{ sy-lisel }"|.

  SKIP.

  "Reading the content of all lines
  "Note the output lines including the standard page header
  DO.
    READ LINE sy-index.
    IF sy-subrc = 0.
      WRITE / |Value of sy-lisel after READ LINE: "{ sy-lisel }"|.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  SKIP.

*&---------------------------------------------------------------------*
*& READ CURRENT LINE
*&---------------------------------------------------------------------*

  WRITE / `---------------- READ CURRENT LINE ----------------`.

  "sy-lisel contains the entire line content of the current line
  "Note: The line is read where the screen cursor was last positioned or
  "a line was last read.
  "As the previous example reads all lines, the current line is here the
  "last line available.
  READ CURRENT LINE.
  WRITE / |Value of sy-lisel after "READ CURRENT LINE.": "{ sy-lisel }"|.

  SKIP.

*&---------------------------------------------------------------------*
*& READ [CURRENT] LINE LINE VALUE
*&---------------------------------------------------------------------*

  "Assigning the output of individual data objects
  "Here, it is the entire line content.

  SKIP.
  WRITE / `---------------- READ ... LINE LINE VALUE ----------------`.

  "Specifying the current line
  "See the notes above regarding the current line.
  DATA wa TYPE c LENGTH 50.
  READ CURRENT LINE LINE VALUE INTO wa.
  WRITE / |Content of target field after READ CURRENT LINE using the LINE VALUE addition: "{ wa }"|.
  WRITE / |Value of sy-lisel after statement: "{ sy-lisel }"|.

  "Specifying a concrete line
  READ LINE 3 LINE VALUE INTO wa.
  WRITE / |Content of target field after READ LINE n using the LINE VALUE addition: "{ wa }"|.
  WRITE / |Value of sy-lisel after statement: "{ sy-lisel }"|.

  SKIP.

*&---------------------------------------------------------------------*
*& READ [CURRENT] LINE FIELD VALUE
*&---------------------------------------------------------------------*

  "Assigning the output of data objects to these data objects or other target
  "data objects.
  "In the example, the checkbox selections are evaluated. Dedicated target
  "objects are specified for both data objects using INTO.

  WRITE / `---------------- READ ... LINE FIELD VALUE ----------------`.

  DATA(no_selection) = 0.
  DO.
    READ LINE sy-index FIELD VALUE flag INTO flag_check checkbox_name INTO target.
    IF sy-subrc <> 0.
      EXIT.
    ELSEIF flag_check = 'X'.
      no_selection = 1.
      WRITE / |Value of  sy-lilli: "{ sy-lilli }"|.
      WRITE / |Value of target after READ LINE: "{ target }"|.
      WRITE / |Value of sy-lisel after READ LINE: "{ sy-lisel }"|.
      WRITE / `*****`.
    ENDIF.
    CLEAR: target, flag_check.
  ENDDO.

  IF no_selection = 0.
    WRITE / `No checkbox selected.`.
  ENDIF.
``` 

 </td>
</tr>


<tr>
<td> 

Overwriting line content and format<br><br>
`MODIFY [CURRENT] LINE`

 </td>

 <td> 

- Overwrites a line stored in the list buffer with the content of the `sy-lisel` system field
- Allows additional modifications such as changing the value and format.
- Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapmodify_line.htm).

 </td>

<td> 


``` abap
PROGRAM.

DATA text TYPE c LENGTH 4 VALUE 'Text'.
DATA letter TYPE c LENGTH 1.
DATA linno4mod_a TYPE i.
DATA linno4mod_b TYPE i.
DATA linno4mod_c TYPE i.
DATA linno4mod_d TYPE i.
DATA linno4mod_e TYPE i.
DATA linno4mod_f TYPE i.

START-OF-SELECTION.

  linno4mod_a = sy-linno.
  letter = 'A'.
  WRITE: / text, letter.
  letter = 'B'.
  WRITE: / text, letter.
  linno4mod_b = sy-linno.
  letter = 'C'.
  WRITE: / text, letter.
  letter = 'D'.
  WRITE: / text, letter.
  linno4mod_c = sy-linno.
  letter = 'E'.
  WRITE: / text, letter.
  letter = 'F'.
  WRITE: / text, letter.
  linno4mod_d = sy-linno.
  letter = 'G'.
  WRITE: / text, letter.
  letter = 'H'.
  WRITE: / text, letter.
  linno4mod_e = sy-linno.
  letter = 'I'.
  WRITE: / text, letter.
  letter = 'J'.
  WRITE: / text, letter.
  linno4mod_f = sy-linno.

AT LINE-SELECTION.

  IF sy-lilli BETWEEN linno4mod_a + 2 AND linno4mod_b.
    MODIFY CURRENT LINE LINE VALUE FROM 'Line overwritten'.
  ELSEIF sy-lilli BETWEEN linno4mod_b + 1 AND linno4mod_c.
    MODIFY CURRENT LINE FIELD VALUE text FROM 'ABAP'.
  ELSEIF sy-lilli BETWEEN linno4mod_c + 1 AND linno4mod_d.
    MODIFY CURRENT LINE FIELD FORMAT letter COLOR 3.
  ELSEIF sy-lilli BETWEEN linno4mod_d + 1 AND linno4mod_e.
    MODIFY CURRENT LINE LINE FORMAT COLOR 4.
  ELSEIF sy-lilli BETWEEN linno4mod_e + 1 AND linno4mod_f.
    MODIFY CURRENT LINE FIELD FORMAT text COLOR 5 letter COLOR 6 LINE FORMAT COLOR 7.
  ELSE.
    MESSAGE `That won't be modified.` TYPE 'I'.
  ENDIF.
``` 

 </td>
</tr>

</table>


> [!NOTE]
> - [Relevant sy components in the context of lists](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlist_systemfields.htm)
> - You can interact with a list by double-clicking a line (or pressing F2; the default `PICK` function code raises the `AT LINE-SELECTION` event). Other function codes usually trigger the `AT USER-COMMAND` event. See the event blocks below. In your ABAP program, you can react to the user action by implementing the individual event blocks. Regarding the function code, you can use the `sy-ucomm` system field for evaluation (unlike dynpros, there's no `OK_CODE` field to be filled).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Event Blocks

- Event blocks are introduced by an [event keyword](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenevent_keyword_glosry.htm). 
- Ended by the next processing block (there's no closing statement).
- Should not contain separate declarations, i.e. you should put all data object declarations in the global declaration part (except for `AT SELECTION-SCREEN ...`)
- It is recommended that you use ABAP Objects and methods consistently for data encapsulation
- Should not be specified more than once in a program
- Called depending on the user action and the program state (even if not explicitly specified)

> [!NOTE]
> The main purpose of the examples is to visualize the calling of the events. Internal tables are filled during the events to log the event name and the time stamp when it is called. The tables are then output.

<table>

<tr>
<th> Event block </th> <th> Notes </th> <th> Code Snippet </th>
</tr>

<tr>
<td> 

Program constructor  


 </td>

 <td> 

- `LOAD-OF-PROGRAM` 
  - Called when a program is loaded into the internal session
  - Can be used to initialize the global data of a program. However, for executable programs, it is recommended that you use the event block `INITIALIZATION` for this task since the start values for parameters and selection criteria are set after `LOAD-OF-PROGRAM`.   

 </td>

<td> 

See below

 </td>
</tr>


<tr>
<td> 

Reporting events related to executable programs in particular

 </td>

 <td> 

- `INITIALIZATION`
  - Used to initialize executable programs and provide start values (that cannot be statically expressed using `DEFAULT`)
  - Called directly after `LOAD-OF-PROGRAM` and before selection screen processing    
- `START-OF-SELECTION`
  - Called after selection screen processing (see the following event blocks)
  - If an executable program does not contain selection screens, this block is called automatically and implicitly.
  - All statements are implicitly assigned to this block unless they are explicitly assigned to another block.
  - Although it is optional, it is recommended that you specify the statement explicitly to improve readability.

 </td>

<td> 


``` abap
PROGRAM.

TYPES: BEGIN OF st,
         id          TYPE c LENGTH 3,
         event_block TYPE c LENGTH 50,
         time        TYPE utclong,
       END OF st.
DATA log TYPE TABLE OF st WITH EMPTY KEY.
DATA counter TYPE i.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (10) txt1.
  PARAMETERS text1 TYPE c LENGTH 10 LOWER CASE.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (10) txt2.
  PARAMETERS text2 TYPE c LENGTH 10 LOWER CASE.
SELECTION-SCREEN END OF LINE.

LOAD-OF-PROGRAM.
  counter += 1.
  APPEND VALUE #( id = counter event_block = `LOAD-OF-PROGRAM` time = utclong_current( ) ) TO log.

INITIALIZATION.
  counter += 1.
  APPEND VALUE #( id = counter event_block = `INITIALIZATION` time = utclong_current( ) ) TO log.
  text1 = 'Hello'.
  text2 = 'World'.
  txt1 = 'Text 1'.
  txt2 = 'Text 2'.

START-OF-SELECTION.
  counter += 1.
  APPEND VALUE #( id = counter event_block = `START-OF-SELECTION` time = utclong_current( ) ) TO log.

  LOOP AT log INTO DATA(wa).
    WRITE: /(*) wa-id, (20) wa-event_block, (40) wa-time.
  ENDLOOP.
``` 

 </td>
</tr>

<tr>
<td> 

Selection screen events 

 </td>

 <td> 

- Used to individually evaluate selection screen elements, e.g. a specific parameter.
- In many cases, the events can be used for input validation (this should not wait until `START-OF-SELECTION`). If your custom input validation fails on an input, you can raise a message (e.g. a message of type `E`: `MESSAGE 'Error' TYPE 'E'.`) and allow users to edit the input again.

Event blocks:  
- `AT SELECTION-SCREEN OUTPUT`: Called by the dynpro event PBO of a selection screen after `INITIALIZATION`; can be used for modifying screen elements (e.g. using `MODIFY SCREEN`)
- `AT SELECTION-SCREEN ON par`: Called when values for a parameter or selection criteria were passed to the program; note: the syntax is also available for selection criteria; an additional variant `... ON END OF ...` is available (this event block is called when the selection table is passed completely allowing to check the entire selection table content) 
- `AT SELECTION-SCREEN ON BLOCK bl`: Called when all input for a block is passed to the program
- `AT SELECTION-SCREEN ON RADIOBUTTON GROUP gr`: Called when the values for a radio button group were passed
- `AT SELECTION-SCREEN ON EXIT-COMMAND`: Called by *Back*, *Exit*, or *Cancel* (e.g. used for cleanup tasks)
- `AT SELECTION-SCREEN ON HELP-REQUEST | VALUE-REQUEST`: Relates to field (F1) and input (F4) help
- `AT SELECTION-SCREEN.`: Called as last event in the selection screen processing when all input values are passed to the program

 </td>

<td> 



``` abap
PROGRAM.

TYPES: BEGIN OF st,
         id          TYPE c LENGTH 3,
         event_block TYPE c LENGTH 50,
         time        TYPE utclong,
       END OF st.
DATA log TYPE TABLE OF st WITH EMPTY KEY.
DATA counter TYPE i.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (10) txt1.
  PARAMETERS text1 TYPE c LENGTH 10 LOWER CASE.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK blk.
  PARAMETERS text2 TYPE c LENGTH 10 LOWER CASE.
  PARAMETERS text3 TYPE c LENGTH 10 LOWER CASE.
SELECTION-SCREEN END OF BLOCK blk.

PARAMETERS: rb1 RADIOBUTTON GROUP grp DEFAULT 'X',
            rb2 RADIOBUTTON GROUP grp.

LOAD-OF-PROGRAM.
  counter += 1.
  APPEND VALUE #( id = counter event_block = `LOAD-OF-PROGRAM` time = utclong_current( ) ) TO log.

INITIALIZATION.
  counter += 1.
  APPEND VALUE #( id = counter event_block = `INITIALIZATION` time = utclong_current( ) ) TO log.
  text1 = 'Hello'.
  text2 = 'World'.
  text3 = 'ABAP'.
  txt1 = 'Text 1'.

AT SELECTION-SCREEN OUTPUT.
  counter += 1.
  APPEND VALUE #( id = counter event_block = `AT SELECTION-SCREEN OUTPUT` time = utclong_current( ) ) TO log.

  LOOP AT SCREEN INTO DATA(wa).
    IF wa-name = 'TEXT1'.
      wa-intensified = '1'.
      MODIFY SCREEN FROM wa.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON text1.
  counter += 1.
  APPEND VALUE #( id = counter event_block = `AT SELECTION-SCREEN ON` time = utclong_current( ) ) TO log.

AT SELECTION-SCREEN ON BLOCK blk.
  counter += 1.
  APPEND VALUE #( id = counter event_block = `AT SELECTION-SCREEN ON BLOCK` time = utclong_current( ) ) TO log.

AT SELECTION-SCREEN ON RADIOBUTTON GROUP grp.
  counter += 1.
  APPEND VALUE #( id = counter event_block = `AT SELECTION-SCREEN ON RADIOBUTTON GROUP` time = utclong_current( ) ) TO log.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  counter += 1.
  APPEND VALUE #( id = counter event_block = `AT SELECTION-SCREEN ON EXIT-COMMAND` time = utclong_current( ) ) TO log.

  DATA msg TYPE string VALUE `These events have already been raised:`.
  LOOP AT log INTO DATA(cmd).
    msg &&= | { cmd-event_block }|.
  ENDLOOP.
  MESSAGE msg TYPE 'I'.

AT SELECTION-SCREEN.
  APPEND VALUE #( id = counter event_block = `AT SELECTION-SCREEN` time = utclong_current( ) ) TO log.

START-OF-SELECTION.
  counter += 1.
  APPEND VALUE #( id = counter event_block = `START-OF-SELECTION` time = utclong_current( ) ) TO log.

  LOOP AT log INTO DATA(wa).
    WRITE: /(*) wa-id, (45) wa-event_block, (30) wa-time.
  ENDLOOP.
``` 

 </td>
</tr>

<tr>
<td> 

List events 

 </td>

 <td> 

- Are triggered during the list creation and after certain user actions on a displayed list.
- During list creation: 
  - `TOP-OF-PAGE` <br>For definining page headers; called when a [basic list](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenbasic_list_glosry.htm) is created and when a new page begins (`... DURING LINE-SELECTION` is called when [details lists](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendetails_list_glosry.htm) are created)
  - `END-OF-PAGE` <br>For definining page footers; called when the end of a page is reached, for example, when the specified number of lines in the `LINE-COUNT` addition (e.g. `PROGRAM LINE-COUNT 20(3).`) has been reached
- After user actions: 
  - `AT LINE-SELECTION` <br>Called when a screen list is displayed, the screen cursor is on a list line and a function is selected using the function code `PICK`; when specified, the function key F2 is enabled and, therefore, double click functionality; F2 and a double click are linked with the function code `PICK`.
  - [`AT USER-COMMAND`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapat_user-command.htm) <br>Called when a function with a user-defined function code is selected when a screen list is displayed; the function codes can be evaluated using `sy-ucomm`; you can set a list event programmatically using [`SET USER-COMMAND`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapset_user-command.htm).

 </td>

<td> 

The following example is an excerpt of the executable example contained in the repository. Find more information in the comments there. The example is just intended to explore and visualize various list events. 


``` abap
PROGRAM LINE-COUNT 20(3).

TYPES: BEGIN OF st,
         id          TYPE c LENGTH 3,
         event_block TYPE c LENGTH 50,
         time        TYPE utclong,
       END OF st.
DATA: counter_line             TYPE i,
      reporting_and_selscr_log TYPE TABLE OF st WITH EMPTY KEY,
      list_log                 TYPE TABLE OF st WITH EMPTY KEY,
      counter                  TYPE i,
      tstmp                    TYPE utclong.

SELECTION-SCREEN COMMENT /1(70) intro.
SELECTION-SCREEN SKIP.

PARAMETERS: text TYPE c length 10.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN COMMENT /1(70) note.

CLASS demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      sel_screen_evt IMPORTING evt TYPE string,
      write_line IMPORTING evt TYPE string
                           utc TYPE utclong,
      counter,
      list_evt IMPORTING evt TYPE string,
      write_rep_sel_log,
      write_list_log.
    CLASS-DATA: li_wa  LIKE LINE OF list_log,
                sel_wa LIKE LINE OF reporting_and_selscr_log.
ENDCLASS.

CLASS demo IMPLEMENTATION.
  METHOD counter.
    IF counter = 0.
      counter = 1.
    ELSE.
      counter += 1.
    ENDIF.
  ENDMETHOD.

  METHOD sel_screen_evt.
    counter( ).
    APPEND VALUE #( id = counter event_block  = evt time = utclong_current( ) ) TO reporting_and_selscr_log.
  ENDMETHOD.

  METHOD list_evt.
    counter( ).
    APPEND VALUE #( id = counter event_block  = evt time = utclong_current( ) ) TO list_log.
  ENDMETHOD.

  METHOD write_line.
    counter( ).
    WRITE: /3(*) counter,
            15  evt,
            60  utc.
  ENDMETHOD.

  METHOD write_rep_sel_log.
    SKIP.
    WRITE / '********** Log entries for reporting and selection screen events **********' COLOR 2.

    FORMAT COLOR 2.
    WRITE: /3 'ID', 15 'Event Block', 60 'Time' .
    FORMAT COLOR OFF.
    LOOP AT reporting_and_selscr_log INTO sel_wa.
      WRITE: / sel_wa-id UNDER 'ID',
              sel_wa-event_block UNDER 'Event Block',
              sel_wa-time UNDER 'Time'.
    ENDLOOP.
  ENDMETHOD.

  METHOD write_list_log.
    WRITE / '********** Log entries for list events **********' COLOR 2.

    FORMAT COLOR 2.
    WRITE: /3 'ID', 15 'Event Block', 60 'Time' .
    FORMAT COLOR OFF.
    LOOP AT list_log INTO li_wa.
      WRITE: / li_wa-id UNDER 'ID',
              li_wa-event_block UNDER 'Event Block',
              li_wa-time UNDER 'Time'.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

LOAD-OF-PROGRAM.

  demo=>sel_screen_evt( `LOAD-OF-PROGRAM` ).

**********************************************************************

INITIALIZATION.

  demo=>sel_screen_evt( `INITIALIZATION` ).
  intro = 'Exploring list events'.
  note = 'The Back/Exit/Cancel buttons raise the ON EXIT-COMMAND event.'.
  text = 'Hello'.

**********************************************************************

START-OF-SELECTION.

  SET USER-COMMAND 'START'.

  demo=>sel_screen_evt( `START-OF-SELECTION` ).

  WRITE / 'This is the basic list showing the content of a log table (mostly reporting and selection screen events). Double-click a line in this list to display more events in a details list.' COLOR 4.
  SKIP.
  WRITE / |Parameter value: "{ text }"| COLOR 2.


  IF reporting_and_selscr_log IS NOT INITIAL.
    demo=>write_rep_sel_log( ).
  ENDIF.

**********************************************************************

AT LINE-SELECTION.

  demo=>list_evt( evt = `AT LINE-SELECTION` ).

  SET USER-COMMAND 'LINE'.

  IF sy-lsind <= 1.
    WRITE / 'This is a details list. It shows the content of another log table (mostly list events). Clicking in this list does nothing meaningful.' COLOR 4.
    WRITE / 'You may want to go back to the basic list, double-click a line there to come back here, and see more log entries added for list events.' COLOR 4.
    WRITE / 'When the counter reaches 40, the program is restarted :)' COLOR 4.
    SKIP.
    WRITE / `************** Some sy Components **************` COLOR 2.
    WRITE / |Content of clicked line (sy-lisel): "{ sy-lisel }"|.
    WRITE / |Number of clicked line (sy-lilli): "{ sy-lilli }"|.
    WRITE / |List level of the current list (sy-lsind): "{ sy-lsind }"|.
    SKIP.
    IF list_log IS NOT INITIAL.
      demo=>write_list_log( ).
    ENDIF.
  ENDIF.

**********************************************************************

AT USER-COMMAND.

  CASE sy-ucomm.
    WHEN 'START'.
      demo=>list_evt( `AT USER-COMMAND (Function code: START)` ).
      MESSAGE  |AT USER-COMMAND event raised (Function code: START). Counter value: { counter }.| TYPE 'I'.
    WHEN 'LINE'.
      demo=>list_evt( `AT USER-COMMAND (Function code: LINE)` ).
      MESSAGE  |AT USER-COMMAND event raised (Function code: LINE). Counter value: { counter }.| TYPE 'I'.
      IF counter >= 40.
        MESSAGE  |The counter has reached the threshold. The program will be started again using a SUBMIT statement.| TYPE 'I'.
        SUBMIT (sy-repid) VIA SELECTION-SCREEN.
      ENDIF.
  ENDCASE.

**********************************************************************

TOP-OF-PAGE.

  tstmp = utclong_current( ).
  demo=>counter( ).
  APPEND VALUE #( id = counter event_block  = |TOP-OF-PAGE| time = tstmp ) TO list_log.
  WRITE: / |********************* Title inserted at TOP-OF-PAGE event ({ counter }) *********************|.

**********************************************************************

TOP-OF-PAGE DURING LINE-SELECTION.

  tstmp = utclong_current( ).
  demo=>counter( ).
  APPEND VALUE #( id = counter event_block  = |TOP-OF-PAGE DURING LINE-SELECTION| time = tstmp ) TO list_log.
  WRITE: / |********************* Title inserted at TOP-OF-PAGE DURING LINE-SELECTION event ({ counter }) *********************|.

**********************************************************************

END-OF-PAGE.

  tstmp = utclong_current( ).
  demo=>counter( ).
  APPEND VALUE #( id = counter event_block  = |END-OF-PAGE| time = tstmp ) TO list_log.
  WRITE: / |********************* Footer inserted at END-OF-PAGE event ({ counter }) *********************|.
``` 

 </td>
</tr>

</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursion: SAP List Viewer (ALV)

- Provides an object-oriented API for displaying and formatting lists 
- Allows you to specify layout settings and functionality
- Classes such as `CL_SALV_TABLE` encapsulate the use of the ALV grid control and simplify the integration into ABAP programs. Note: The older class `CL_GUI_ALV_GRID` should no longer be used directly for new developments.
- Other classes are available to handle hierarchical lists and tree structures. The focus of the code snippets is on simple, non-nested tables and the use of the `CL_SALV_TABLE` class.

### Getting Started
- To get started quickly, you only need to ...
  - instantiate an ALV table object using the `factory` method.
  - define the display type for the ALV output. By default, full-screen display is enabled. 
  - display the ALV output using the `display` method.

> [!NOTE]
> - When working with ALV, make sure that you implement appropriate error handling. 
> - The `factory` method also has optional exporting parameters. You can use the optional `list_display` parameter to specify whether you want to display the ALV output as classic list. It is set to false by default. There are also exporting parameters to display the ALV output in containers (e.g. see the dynpro cheat sheet example). The exporting parameters are not relevant in this example. Here, the ALV output is displayed on the entire screen (however, it is also possible to display the ALV output in a dialog box).

The following code snippet shows a simple example to get you started quickly with the necessary method calls. It includes a demo internal table with content.

```abap
TYPES: BEGIN OF demo_struct,
         col1 TYPE string,
         col2 TYPE i,
         col3 TYPE c LENGTH 1,
       END OF demo_struct.

DATA itab TYPE TABLE OF demo_struct WITH EMPTY KEY.
itab = VALUE #( ( col1 = `abc` col2 = 1 col3 = 'X' )
                ( col1 = `def` col2 = 2 col3 = 'Y' )
                ( col1 = `ghi` col2 = 3 col3 = 'Z' ) ).

TRY.
    cl_salv_table=>factory( IMPORTING r_salv_table = DATA(alv)
                            CHANGING  t_table      = itab ).

    alv->display( ).
  CATCH cx_salv_msg INTO DATA(err).
    MESSAGE err->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
ENDTRY.
```

### Optional Layout Settings and Functionality

- The following commented code snippet covers a selection of the many setting and functionality options.
- For simplicity of the snippet, the code only uses the root exception class. Make sure that you implement appropriate error handling and exception classes.  
- You can copy and paste the code into your own test program to explore the ALV output and the effect of the method calls. 

> [!NOTE]
> Check the comments for the custom functions. If there are errors in your test program, replace the relevant code section and enable the generic ALV functions.

```abap
TYPES: BEGIN OF demo_struct,
         col1 TYPE string,
         col2 TYPE i,
         col3 TYPE string,
         col4 TYPE icon_d,
         col5 TYPE string,
       END OF demo_struct.
DATA itab TYPE TABLE OF demo_struct WITH EMPTY KEY.
DATA alv TYPE REF TO cl_salv_table.
DATA cnt4evt TYPE i VALUE 6.

"Local class to handle events
CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS single_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.
    CLASS-METHODS double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
    CLASS-METHODS func_click
      FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.
ENDCLASS.
CLASS lcl_events IMPLEMENTATION.
  METHOD single_click.
    "Both single and double click events trigger messages.
    READ TABLE itab INDEX row REFERENCE INTO DATA(sc_ref).
    IF sy-subrc = 0.
      ASSIGN sc_ref->(column) TO FIELD-SYMBOL(<fs_sc>).
      MESSAGE `Single click event. ` &&
      |Row: { row } { COND #( WHEN column IS NOT INITIAL THEN `Column: ` && column ) } | &&
      |{ COND #( WHEN <fs_sc> IS ASSIGNED THEN `Value: ` && <fs_sc> ) }| TYPE 'I'.
    ELSE.
      MESSAGE `Single click event` TYPE 'I'.
    ENDIF.
  ENDMETHOD.
  METHOD double_click.
    READ TABLE itab INDEX row REFERENCE INTO DATA(dc_ref).
    IF sy-subrc = 0.
      ASSIGN dc_ref->(column) TO FIELD-SYMBOL(<fs_dc>).
      MESSAGE `Double click event. ` &&
      |Row: { row } { COND #( WHEN column IS NOT INITIAL THEN `Column: ` && column ) } | &&
      |{ COND #( WHEN <fs_dc> IS ASSIGNED THEN `Value: ` && <fs_dc> ) }| TYPE 'I'.
    ELSE.
      MESSAGE `Double click event` TYPE 'I'.
    ENDIF.
  ENDMETHOD.
  METHOD func_click.
    "Handling custom functions
    CASE e_salv_function.
      WHEN 'DATA'.
        MESSAGE `Custom function DATA. Do something ...` TYPE 'I'.
      WHEN 'TEST'.
        IF cnt4evt = 6.
          MESSAGE `A table row will be added, demonstrating the 'refresh' ` &&
          `method that rebuilds the output table.` TYPE 'I'.
        ENDIF.
        cnt4evt += 1.
        itab = VALUE #( BASE itab ( col1 = `fffff` col2 = cnt4evt
                                    col3 = `ttttt` col4 = icon_green_light ) ).
        "Rebuilding the output table
        alv->refresh( ).
      WHEN 'QUIT'.
        MESSAGE `Custom function QUIT. Do something ...` TYPE 'I'.
      WHEN OTHERS.
        MESSAGE `Some other function` TYPE 'I'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
"Populating internal table that is output
itab = VALUE #( ( col1 = `aaaaa` col2 = 1 col3 = `zzzzz` col4 = icon_green_light )
                ( col1 = `bbbbb` col2 = 2 col3 = 'yyyyy' col4 = icon_green_light )
                ( col1 = `ccccc` col2 = 3 col3 = 'xxxxx' col4 = icon_green_light ) ).
TRY.
    "----------- Instantiating an ALV table object -----------
    cl_salv_table=>factory( IMPORTING r_salv_table = alv
                            CHANGING  t_table      = itab ).
    "----------- Creating a new instance with new data -----------
    "Notes on the set_data method:
    "- All objects that were referred to previously are removed.
    "- Not to be used in event handlers.
    "In the example, new lines are added to the existing internal table.
    itab = VALUE #( BASE itab ( col1 = `ddddd` col2 = 4
                                col3 = `wwwww` col4 = icon_green_light )
                              ( col1 = `ddddd` col2 = 5
                                col3 = `vvvvv` col4 = icon_green_light )
                              ( col1 = `eeeee` col2 = 6
                                col3 = `uuuuu` col4 = icon_green_light ) ).
    alv->set_data( CHANGING t_table = itab ).
    "----------- Layout-related settings -----------
    "Changing the list header title
    CAST cl_salv_display_settings(
      alv->get_display_settings( ) )->set_list_header( 'Demo Title' ).
    "There are several methods to retrieve column-specific information.
    "The following examples show a selection. You can check the variable
    "content in the debugger.
    DATA(col1obj) = alv->get_columns( )->get_column( 'COL1' ).
    DATA(col1alignment) = col1obj->get_alignment( ).
    DATA(col1type) = col1obj->get_ddic_inttype( ).
    DATA(col1colname) = col1obj->get_columnname( ).
    DATA(is_col1_visible) = col1obj->is_visible( ).
    "Settings for the column header
    "This example uses the get method, which returns all column objects of
    "the output table. The table is processed in a loop.
    "Settings covered:
    "- Specifying the column header titles (long, medium, and short
    "  column header)
    "- Specifying a tooltip for the column header
    LOOP AT alv->get_columns( )->get( ) REFERENCE INTO DATA(colref).
      "Specifying the column header titles
      "The example sets the texts based on the columname value.
      "Note: The column width is optimized further down. You may want to
      "manually adjust the width in the ALV output to see the column header
      "name change.
      colref->r_column->set_long_text( |{ colref->columnname }| ).
      colref->r_column->set_medium_text( |{ colref->columnname+2(3) }| ).
      colref->r_column->set_short_text( |{ colref->columnname+3(3) }| ).
      "Specifying a tooltip for the column headers
      colref->r_column->set_tooltip(
        |Demo tooltip { colref->columnname+3(3) }| ).
    ENDLOOP.
    "Displaying/Hiding the column headers
    "In the example, the value is set to abap_true. Set it to abap_false
    "to hide the column header.
    "Note: In this and the following examples, the settings are
    "intentionally done using separate casts for each example. You can also
    "create object reference variables, such as for 'col1obj' to avoid having
    "to specify the casts explicitly each time.
    CAST cl_salv_columns_list(
      alv->get_columns( ) )->set_headers_visible( abap_true ).
    "Setting key columns
    "Note: The key columns have a default color setting. There, you can only
    "change the color in individual cells.
    CAST cl_salv_column_table(
        alv->get_columns( )->get_column( 'COL1' ) )->set_key( abap_true ).

    "Setting column color
    "Note: The executable example includes examples of coloring entire rows and
    "specific cells.
    CAST cl_salv_column_table( alv->get_columns( )->get_column( 'COL2' )
      )->set_color( VALUE lvc_s_colo( col = col_positive ) ).

    "Hiding columns
    CAST cl_salv_column_table( alv->get_columns( )->get_column( 'COL5' )
      )->set_visible( abap_false ).

    "Setting text alignment
    CAST cl_salv_column_table( alv->get_columns( )->get_column( 'COL2' )
      )->set_alignment( if_salv_c_alignment=>left ).

    "Optimizing column width, i.e. automatically adjusting the column width
    "to display values completely (can also be done for individual columns)
    alv->get_columns( )->set_optimize( abap_true ).

    "Setting output width explicitly (for an individual column)
    "Note: Just to demonstrate the method call. This setting has no effect in
    "the example due to the column width optimization above, and the column is
    "hidden anyway.
    CAST cl_salv_column_table( alv->get_columns( )->get_column( 'COL5' )
      )->set_output_length( '100' ).

    "Setting table rows to a striped pattern
    alv->get_display_settings( )->set_striped_pattern( abap_true ).

    "Displaying/hiding horizontal and vertical grid lines (here, they're hidden)
    alv->get_display_settings( )->set_horizontal_lines( abap_false ).
    alv->get_display_settings( )->set_vertical_lines( abap_false ).

    "With the following method call, the ALV output is displayed in a dialog box.
    "Demo coordinates are provided. You can comment it in to see the effect.
*      alv->set_screen_popup( start_column = 10
*                             end_column   = 100
*                             start_line   = 4
*                             end_line     = 15 ).

    "------ Adding functionality/user interaction options to the ALV output ------
    "Specifying the selection type
    "The following specification allows you to select multiple rows/columns (a column
    "is added on the left for selections). Check the other options in the
    "if_salv_c_selection_mode interface.
    alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    "Setting the sorting
    "In the example, a particular column is sorted in descending order.
    alv->get_sorts( )->add_sort( 'COL1' )->set_sequence( if_salv_c_sort=>sort_down ).

    "Applying a filter
    "The example is implemented to exclude one entry from the demo table.
    alv->get_filters( )->add_filter( 'COL2' )->add_selopt( sign   = 'E'
                                                            option = 'EQ'
                                                            low    = '6' ).

    "Making a calculation/aggregation
    "In this example, an aggregation is added for a specific column.
    "In this case, the average value is calculated. It is displayed in a row added
    "at the bottom of the table.
    alv->get_aggregations( )->add_aggregation(
      columnname  = 'COL2'
      aggregation = if_salv_c_aggregation=>average ).

    "Generic and custom ALV functions
    "Note:
    "- By default, ALV functions (such as sorting or filtering) are not available
    "  to users. You must explicitly enable them.
    "- Depending on whether the display is full-screen (as it is here) or in a
    "  container, restrictions apply. In the first case, you can use your own GUI status
    "  to integrate custom functions.

    "This (self-contained, ready-to-use) code snippet is intended so that you can simply
    "copy & paste the code into a test program. For this purpose, and to be able to
    "demonstrate custom functions in this full-screen ALV example using a GUI status, the
    "example uses a demo GUI status that is included in another sample program (in a
    "subpackage of SALV). For your own GUI status, you can check the SALV_TABLE_STANDARD
    "status contained in the SALV_METADATA_STATUS function group, which you can copy and
    "use as a template, for example.
    "*********************************** NOTE ***********************************
    "- The GUI status used here is just reused to have a copyable and self-contained example.
    "- The GUI status of the sample program (specified for the report parameter below)
    "  contains generic and additional functions. For the additional functions, a simple
    "  implementation is included in this snippet. The GUI status should include the 
    "  functions TEST, DATA, QUIT.
    "- The implementations in the event handler class here do not match the implementations
    "  there, the button texts do not make sense for the implementations here, and so on.
    "- So even though it (the button names, icons, implementations, etc.) does not make much
    "  sense for this snippet, you should get an idea about custom functions and be able to
    "  explore the event handling by clicking on the buttons.
    "- The TEST function demonstrates adding new rows to the table and the 'refresh' method.
    "- If this particular program, the GUI status and/or the custom functions are not available
    "  in your system, or if you insert the code into your own test program, run it, and
    "  encounter problems setting the status, remove the TRY control structure that contains
    "  the set_screen_status method call, and comment in the code below to use the standard
    "  ALV functions. In this case, the custom functions cannot be checked out with this
    "  snippet.
    "- Check out the executable example of the cheat sheet that includes a GUI status.
    TRY.
        alv->set_screen_status(
          pfstatus      = 'SALV_TABLE_STANDARD'
          report        = 'SALV_TEST_REFRESH'
          set_functions = alv->c_functions_all ).

        "In this GUI status, the custom functions TEST, DATA, and QUIT are specified. You can 
        "check the results of the following method calls in the debugger.
        DATA(getfunc) = alv->get_functions( ).
        "Checking the activation status
        DATA(is_enabled) = getfunc->is_enabled( 'TEST' ).
        "Checking the visibility
        DATA(is_visible) = getfunc->is_visible( 'TEST' ).

        "Registering an event handler for the custom functions
        "The added_function event is raised. In the implementation of the event handler method,
        "you can then implement your code based on the value of the e_salv_function parameter
        "(which contains the specified function name), for example, using a CASE statement.
        "The implementation in this code snippet is different from the original program.
        SET HANDLER lcl_events=>func_click FOR alv->get_event( ).

      CATCH cx_salv_object_not_found.
        MESSAGE `GUI status error. Instead, use the set_default method, for example.`
        TYPE 'I'.
    ENDTRY.

    "If you remove the TRY control structure above, you can comment in the following
    "code to use the generic ALV functions. Use the set_all method for all generic
    "functions. You can also enable generic ALV functions individually. Check the
    "set_* methods.
    "alv->get_functions( )->set_default( abap_true ).

    "Hotspot/single click functionality
    "You can define the content of a cell as a clickable area. You can do this by
    "specifying the cell type hotspot. When a user clicks the cell content,
    "the link_click event is raised.
    CAST cl_salv_column_table( alv->get_columns( )->get_column( 'COL1' )
      )->set_cell_type( if_salv_c_cell_type=>hotspot ).

    "You can set icon columns and make the icon cells clickable areas. To do this,
    "use the set_icon method. As above, the cell type of the column is set to hotspot.
    CAST cl_salv_column_table( alv->get_columns( )->get_column( 'COL4' )
      )->set_icon( if_salv_c_bool_sap=>true ).
    CAST cl_salv_column_table( alv->get_columns( )->get_column( 'COL4' )
      )->set_cell_type( if_salv_c_cell_type=>hotspot ).

    "An event handler is registered for the clickable areas (the link_click event
    "is raised). The get_event method returns the required event object. In the
    "example, a message is displayed.
    SET HANDLER lcl_events=>single_click FOR alv->get_event( ).

    "Double click functionality
    "Registering an event handler for the double click event. In the example, a message
    "is displayed. Double-click cells in columns for which the cell type is not set to hotspot.
    SET HANDLER lcl_events=>double_click FOR alv->get_event( ).

    "Adding tooltips to the icon column cells
    alv->get_functional_settings( )->get_tooltips( )->add_tooltip(
      type    = cl_salv_tooltip=>c_type_icon
      value   = CONV lvc_value( icon_green_light )
      tooltip = `This is a tooltip for an icon` ).

    "----------- Displaying the ALV output -----------
    alv->display( ).
  CATCH cx_root INTO DATA(error).
    "For simplicity, this example uses the root exception class.
    "Always make sure that you use appropriate exception classes. Check the F2
    "information for the methods in ADT.
    MESSAGE error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
ENDTRY.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information
- ABAP Keyword Documentation:
  - [Selection Screens](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenselection_screen.htm) 
  - [Classic Lists](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_dynpro_list.htm)
- ALV:
  - [SAP List Viewer (ALV)](https://help.sap.com/docs/SAP_NETWEAVER_731_BW_ABAP/b1c834a22d05483b8a75710743b5ff26/4ec38f8788d22b90e10000000a42189d.html?locale=en-US) on the SAP Help Portal
  - Find more demonstration examples in package `SALV` and its subpackages.

## Executable Examples

After the import of the repository, proceed as follows: 
- Find the program in ADT using the search by choosing `CTRL + SHIFT + A`.
- Enter `ZDEMO_ABAP_SELSCR_LISTS_INTRO` and open the program. This program serves as an entry point for all selection screen and list examples. Program names of the individual examples:
  - `ZDEMO_ABAP_SELSCR_PARAMETERS`: Demonstrates `PARAMETERS` statements
  - `ZDEMO_ABAP_SELSCR_SELECT_OPT`: Demonstrates `SELECT-OPTIONS` statements
  - `ZDEMO_ABAP_SELSCR_STANDALONE`: Demonstrates the creation (`SELECTION-SCREEN` statements) and calling of standalone selection screens
  - `ZDEMO_ABAP_SELSCR_STMTS_VAR`: Demonstrates variants of the `SELECTION-SCREEN` statements that do not create selection screens
  - `ZDEMO_ABAP_LISTS`: Demonstrates various ABAP statements to create and handle classic lists
  - `ZDEMO_ABAP_EVENT_BLOCKS`: Demonstrates event blocks
  - `ZDEMO_ABAP_ALV`: Demonstrates the SAP List Viewer (ALV)
- Run the program by choosing `F8`.

> [!NOTE]
> - The examples in the *main* branch of the ABAP cheat sheet repository are designed to be imported into the SAP BTP ABAP Environment. For Standard ABAP, you can find examples (such as `ZDEMO_ABAP_SELSCR_LISTS_INTRO`) in the other branches of the repository. 
> - The executable examples ...
>   - do not claim to include meaningful selection screens and lists.
>   - are not intended to be role models for proper selection screen and list design.   
>   - are not intended to solve concrete programming tasks. You should always work out your own solution for each individual case.
>   - are only intended to demonstrate a selection of keywords and visualize related syntax in action on a high level.
>   - include comments in the program code. 
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)