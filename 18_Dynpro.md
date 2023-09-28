<a name="top"></a>

# Dynpro

- [Dynpro](#dynpro)
  - [Introduction](#introduction)
  - [About Dynpros](#about-dynpros)
  - [Dynpro Flow Logic](#dynpro-flow-logic)
  - [Dialog Modules](#dialog-modules)
  - [Transporting Data between Dynpros and the ABAP Program](#transporting-data-between-dynpros-and-the-abap-program)
    - [Dynpro Fields](#dynpro-fields)
    - [OK Field and Function Codes](#ok-field-and-function-codes)
    - [Program-Controlled Data Transport](#program-controlled-data-transport)
    - [Calling Dialog Modules Conditionally](#calling-dialog-modules-conditionally)
  - [Input Checks](#input-checks)
  - [Field and Input Help](#field-and-input-help)
  - [Dnypro Sequence, Calling and Leaving Dynpros](#dnypro-sequence-calling-and-leaving-dynpros)
    - [Dynpro Sequence](#dynpro-sequence)
    - [ABAP Statements for Calling and Leaving Dynpros](#abap-statements-for-calling-and-leaving-dynpros)
  - [Modifying Static Attributes of Screen Elements](#modifying-static-attributes-of-screen-elements)
  - [Statements for the GUI Status and Title](#statements-for-the-gui-status-and-title)
  - [Controls](#controls)
    - [Table Controls](#table-controls)
    - [Tabstrips Controls](#tabstrips-controls)
    - [GUI Controls](#gui-controls)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)
 
## Introduction

⚠️ The content of this cheat sheet and the executable example are only relevant to [classic ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclassic_abap_glosry.htm).

[User interfaces (UI)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenuser_interface_glosry.htm) are not limited to displaying some information, they must also allow the user to interact with the program. 
In modern UI technologies, this can be achieved through [events](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenevent_glosry.htm), i.e. [user actions](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenuser_action_glosry.htm) on a UI trigger events, and UI methods register these events and react accordingly. In this way, users control the program flow through their actions.
In the early days of ABAP, classes, methods, and events did not exist. Program flow control had to be achieved in other ways. 
This is where [dynpros](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynpro_glosry.htm) (dynamic programs) representing a classic ABAP UI technology come into play.

This cheat sheet provides a high-level overview of classic dynpro topics with a focus on dynpro-related statements, supported by an executable example to check the syntax in action.

> **💡 Note**<br>
> - Classic dynpros are outdated for application programs. New developments should use web-based UIs, such as SAPUI5 or Web Dynpro. 
> - Dynpros cannot be created in ABAP Cloud.
> - This cheat sheet ...
>   - is not intended to encourage you to start creating classic dynpros for programming new applications.  
>   - does not cover all facets, techniques, and keywords in great detail. 
>   - is intended to touch on a selection of dynpro-related topics and syntax that you may encounter in older ABAP code. If you need more information, always consult the ABAP Keyword Documentation.
> - Some of the statements described here - the ones used in the dynpro flow logic - are programmed in a special programming language. Although it looks like ABAP, it is not ABAP.
> - Links to the ABAP Keyword Documentation in this cheat sheet refer to the documentation for [Standard ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_abap_glosry.htm) (latest version).

## About Dynpros

  - Stands for dynamic program, i.e. the program execution is dynmically controlled by user interactions
  - Can only be defined in [function groups](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfunction_group_glosry.htm), [module pools](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenmodul_pool_glosry.htm) (not [class pools](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclass_pool_glosry.htm)) and [executable program](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexecutable_program_glosry.htm) (*reports*; the focus in the cheat sheet is on the latter)
  - Can be identified by a unique, four-digit [dynpro number](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynpro_number_glosry.htm) in an [ABAP program](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_program_glosry.htm). Note that leading zeros need not be specified when calling the dynpro. Number 1000 is reserved, as are other dynpro number ranges (e.g. used by SAP). The current dynpro can be retrieved using `sy-dynnr`.
  - Is displayed in a window of [SAP GUI](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensap_gui_glosry.htm)  
  - Consists of the following main aspects: 
    - Specific characteristics when creating the dynpro. To name a few: 
      - Dynpro type: Defines whether the dynpro is displayed in the full GUI window (if *Normal* is selected), in a pop-up window (*Modal Dialog Box*), or as a subscreen in a specific area within another dynpro in the same ABAP program.
      - [Next dynpro](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennext_dynpro_glosry.htm): Statically specifies the next dynpro to be displayed in a [dynpro sequence](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynpro_sequence_glosry.htm). Setting the next dynpro to 0 or leaving the attribute blank will make the current dynpro the last dynpro in the sequence. If the next dynpro number is the same as the current dynpro, the dynpro continues to be called. The static next dynpro can be overwritten temporarily and dynamically in the ABAP program.
    - [Screen layout](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenscreen_glosry.htm): 
      - Has [screen elements](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenscreen_element_glosry.htm) and is visible to users     
      - Screen elements are, for example, checkboxes, radio buttons, custom controls, dropdown list boxes, pushbuttons, input/output fields, subscreens, table controls, tabstrip controls, text fields, and status icons.      
      - To add screen elements, use the [layout editor](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlayout_editor_glosry.htm) of the [screen painter](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenscreen_painter_glosry.htm) tool. It is available only in the [ABAP Workbench](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_workbench_glosry.htm).
      - For each screen element, you can define various static properties (attributes) that control its appearance. Double-clicking a screen element in the layout editor opens the attribute maintenance dialog box.
      - Various static attributes of the screen elements can be overwritten dynamically from within the ABAP program using special statements.
  - Has its own data objects, called [dynpro fields](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynpro_field_glosry.htm) (see more below)
  - Is called either by another dynpro (as the next dynpro), by a [transaction code](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentransaction_code_glosry.htm) ([dialog transaction](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendialog_transaction_glosry.htm)), or by ABAP statements (e.g. `CALL SCREEN`). Several dynpros in a single ABAP program can be called in sequence to form a dynpro sequence.

> **💡 Note**<br>
> There are special dynpros ([selection screens](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenselection_screen_glosry.htm), [classic lists](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclassic_list_glosry.htm)). They are created implicitly.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Dynpro Flow Logic

- Represents the procedural part of a dynpro
- Controls the dynpro processing, fills and processes the dynpro fields
- Is defined in the *Flow Logic* tab in the screen painter
- Has its own programming language, similar to ABAP, but runs in AS ABAP
- Contains [processing blocks](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenprocessing_block_glosry.htm) introduced by special keywords
- The processing blocks are executed in response to the [PAI](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpai_glosry.htm), [PBO](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpbo_glosry.htm), [POH](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpoh_glosry.htm), and [POV](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpov_glosry.htm) events of the corresponding ABAP program, and call [dialog modules](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendialog_module_glosry.htm):

  - `PROCESS BEFORE OUTPUT` (often also abbreviated as PBO): 
    - Triggered by the ABAP runtime framework and processed before the dynpro is displayed
    - Dialog modules can be called at the PBO event. They are mainly used to prepare the dynpro display, for example, by pre-populating input/output fields.    
    - When the dynpro is presented to the user, and the user has made entries and wants to leave the dynpro, the PAI event is triggered, for example, when a button is clicked.
    
  - `PROCESS AFTER INPUT` (PAI): 
    - Processed after a user action on the dynpro
    - The dialog modules called at PAI evaluate the user entries and process them. 
    - When the processing is complete, the processing of the current dynpro ends and the next dynpro is called.
    
      > **💡 Note**<br>
      > - The PAI processing of the current dynpro and the PBO processing of the next dynpro take place one after the other in the same [work process](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenwork_process_glosry.htm) on the application server and together form a dialog step.
      > - As soon as the screen is ready for input again, only the presentation server is active until the next user action. During this time, the ABAP program waiting for user input does not occupy a work process on the application server.

  - The other events are triggered when the user requests a field or input help for a field: 
    - `PROCESS ON HELP-REQUEST` (POH) 
    - `PROCESS ON VALUE-REQUEST` (POV) 

Example:

```abap
PROCESS BEFORE OUTPUT.
  ... "Here, for example, modules can be called.

PROCESS AFTER INPUT.
  ...

"When you implement field or value helps, use the following processing blocks.
PROCESS ON HELP-REQUEST. 
  ...

PROCESS ON VALUE-REQUEST.
  ...
```

The following statements are among the non-ABAP statements in the dynpro flow logic. They are covered briefly below.

-	`MODULE` for calling dialog modules
-	`FIELD` for controlling the data transport between dynpro fields and the ABAP program
-	`CHAIN` and `ENDCHAIN` for combining module calls
-	`LOOP` and `ENDLOOP` for processing lines of a table control (similar to the ABAP statement `LOOP`)
-	`CALL SUBSCREEN` for calling the flow logic of a subscreen

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Dialog Modules

- Are processing blocks in an ABAP program 
- Represent the procedural link between the dynpro and the ABAP program. 
- Are implemented between the statements [`MODULE`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapmodule.htm) and [`ENDMODULE`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapendmodule.htm). 
- In the [dynpro flow logic](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynpro_flow_logic_glosry.htm), they are called using the `MODULE some_module_9000.` statement, which calls an ABAP processing block called `some_module_9000`. 
- By calling dialog modules during the PBO, PAI, POH, and POV events of the dynpro, the dynpro controls the flow of the associated ABAP program.
- Do not have a parameter interface. The data transport between the dynpro fields and the ABAP program therefore takes place exclusively through the global variables of the ABAP program assigned to the dynpro fields (see below).
- Do not have a local data area. Data object declarations in dialog modules result in global variables of the program. If you really need local data declarations in this context, you can make local data declarations in a method in a local class of the program and call the method.
- Within the dynpro processing block(s) ...
  - `PROCESS BEFORE OUTPUT`, the dynpro statement `MODULE` can only be used to call dialog modules that were defined with the `OUTPUT` addition. 
  - `PROCESS AFTER INPUT`, `PROCESS ON HELP-REQUEST`, and `PROCESS ON VALUE-REQUEST`, only the dialog modules defined with the `INPUT` addition can be called.
- Are not associated with a specific dynpro and can therefore be called from different dynpros.
- You can choose random names for the dialog modules. The name chosen in the example indicates that they are modules for a dynpro with the number 9000.


Calling dialog modules in the dynpro flow logic:
```abap
"Flow Logic tab in the screen painter

PROCESS BEFORE OUTPUT.
  MODULE pbo_9000.

PROCESS AFTER INPUT.
  MODULE pai_9000.
```

Implementation of the dialog modules in the ABAP program

```abap
MODULE pbo_9000 OUTPUT.
  ...
ENDMODULE.

MODULE pai_9000 INPUT.
  ...
ENDMODULE.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Transporting Data between Dynpros and the ABAP Program

### Dynpro Fields

- The transport of data between dynpros and the ABAP program is performed using [dynpro fields](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynpro_field_glosry.htm).
- Dynpro fields are the [data objects](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendata_object_glosry.htm) of dynpros. They are data objects in the working memory of a dynpro.
- All dynpro fields, except the [*OK field*](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenok_field_glosry.htm), are linked to a screen element. See the *Element List* tab of dynpros.
- The data types of dynpro fields are determined either by reference to built-in [ABAP Dictionary (DDIC)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_dictionary_glosry.htm) data types (except `CLNT` and `FLTP`) or by reference to global data objects of the ABAP program. An advantage of DDIC types is that additional properties are available for display on the user interface, including description texts, field help, and input help.
- The actual transport of the data is done in the following way:
  - As a prerequisite, ...
    - you have created a screen element, such as an input field in a dynpro. 
    - You have assigned a name for this screen element - which is the name of the dynpro field. 
    - you have created a data object with the same name as the dynpro field in the ABAP program.
  - During PBO: You can assign a value to the data object, such as *demo text* for the input field, to prefill that input field. Or you can make no assignment to leave the input text blank.  
  - During PAI: After the PBO, the content of data objects are passed to dynpro fields of the same name.  
  - This is a two-way street: For example, when the user enters data in an input field, the data object in the ABAP program receives the value of the dynpro field.   
  - By default, all dynpro fields are transported directly to the ABAP program at the start of the PAI event (that is, at the start of a dialog step) and before the corresponding event block is processed. The reverse transport from the ABAP program to the dynpro takes place at the end of the dialog step, in the context of the PBO event.  
- About declaring data objects in the ABAP program in the global declaration part: 
  - You can use a global variable with `DATA` or a public static attribute of a local class with `CLASS-DATA`.
  - You can use a [`TABLES`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaptables.htm) statement:   
    - Declares a [table work area](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentable_work_area_glosry.htm), i.e. a structure whose data type is taken from the identically named structured data type from the ABAP Dictionary
    - Since it refers to a flat structure in the ABAP Dictionary, it is used to provide all the additional semantic information that is not available when you use `DATA` or `CLASS-DATA`.    
    - A [CDS entity](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencds_entity_glosry.htm) cannot be specified after `TABLES`.
- Note that you can control the data transport explicitly (see below).

Example:
```abap
PROGRAM some_program.

"Variable declarations
DATA some_dobj TYPE abap_bool.
TABLES some_struct.

CLASS local_class DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: another_dobj TYPE i.
...

ENDCLASS.

CLASS demo IMPLEMENTATION.
...
ENDCLASS.
```

### OK Field and Function Codes

- Each dynpro contains a twenty-character [*OK field*](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenok_field_glosry.htm), which is a dynpro field that is not associated with a screen element. 
- It is implicitly declared when a dynpro is created. It is the last entry in the *Element List* tab of the dynpro. Note that the *OK field* must be given a name, for example, `ok_code`.
- The *OK field* is relevant to [function codes](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfunction_code_glosry.htm): 
  - A function code (a sequence of up to 20 characters) can be assigned to specific control elements (for example, a pushbutton, a menu item, and so on) in SAP GUI.
  - When a user action is performed on a control, such as clicking a button, the PAI event is triggered. If the button is linked to a function code, the function code is placed in the *OK field* and passed to a data object of the same name. 
  - You can then evaluate the value in the ABAP program, for example, in a [`CASE`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcase.htm) control structure if you have multiple function codes for different control elements, and implement a response accordingly.
- Notes on working with the *OK field*:
  - As with the other dynpro fields, a data object must be created in the ABAP program.
  - The system field `sy-ucomm` automatically receives the value of the function code. However, it is recommended that you work with the *OK field* instead of `sy-ucomm`. You have full control over the fields you declare. Also, the value of an ABAP system field should not be changed.
  - It is recommended that you store the function code in an auxiliary variable and initialize the *OK field* field in an ABAP program. This ensures that the function code of a dynpro is not filled with an unwanted value in the PBO event (for example, the next PAI event can be triggered with an empty function code). You can then read the function code from the auxiliary variable (for example, using a `CASE` structure) and control the program flow from there.
  - The *OK field* field can have a different name on each dynpro. However, it is recommended that you use the same name for the field in each dynpro of an ABAP program. This way, you only need one field with the same name in the ABAP program, in which the function code is placed and from which you can read it.
  - In addition to screen elements, function codes can also be linked to various things in the dynpro, e.g. the definition of the menu bar takes place in the GUI status. They also trigger a PAI event.

Example:
```abap
PROGRAM zdemo_program.

"OK field declaration and auxiliary variable, assuming the OK field is called
"ok_code in the Element List tab of the dynpro.
DATA: ok_code LIKE sy-ucomm,
      save_ok LIKE ok_code.

...

"Dialog modules in the program

"PBO
MODULE pbo_9000 OUTPUT.
  ...
  "Prefill the value for a screen element, e.g. input field
  some_input_field = 'Hallo'.
  ...
ENDMODULE.

"PAI
MODULE pai_9000 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.    
    "For example, a button is linked with a function code named PUSH
    WHEN 'PUSH'.    
      ...
    WHEN 'ENTER'.
      ...    
    WHEN 'CANCEL'.
      ...  
    WHEN OTHERS.
      ...  
  ENDCASE.
ENDMODULE.
...
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Program-Controlled Data Transport

- In addition to the automatic data transport between the dynpro and the ABAP program, that is, between the dynpro fields and the global ABAP variables of the same name, you can also use [`FIELD`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynpro_field_abexa.htm) statements for program-controlled data transport in the *Flow Logic* tab. 
- These statements control the data transport from the dynpro to the ABAP program during the PAI event.
- Only those dynpro fields that are not specified after a `FIELD` statement are transported directly. 
- In doing so, you can specify the time of the transport from the dynpro to the ABAP program.
- One or more `FIELD` statements are possible, that is, the contents of dynpro fields specified after `FIELD` are transported to the global ABAP data object of the same name when the corresponding `FIELD` statement is executed.
- In the flow logic, the statements are implemented in the processing block `PROCESS AFTER INPUT`.

Example:
```abap
"Note: More syntax options are possible with the FIELD statement. 
"See some of them below and more details in the ABAP Keyword Documentation.

PROCESS AFTER INPUT.
  MODULE pai_9000.  "neither field_a nor field_b are available
  FIELD field_a.
  MODULE module_a.  "field_a is available, field_b is not
  FIELD field_b.
  MODULE module_b.  "both field_a and field_b are available
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Calling Dialog Modules Conditionally 

- By combining the `FIELD` statement with the `MODULE` statement, you can make the call of PAI modules dependent on conditions. 
- The following statement in the flow logic calls the dialog module `mod` only if the dynpro field `dynp_field` is not empty:

  ```abap
  FIELD dynp_field MODULE mod ON INPUT.
  ```

- To call a module `mod` only if the value of a dynpro field `dynp_field` has been changed (or just overwritten with the same value) by the user since the last PBO, use the following:

  ```abap
  FIELD dynp_field MODULE mod ON REQUEST.
  ```

- Conditional module calls can be combined into processing chains to make processing dependent on multiple dynpro fields. 
- A processing chain is defined using the dynpro statements [`CHAIN`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=dynpchain.htm) and [`ENDCHAIN`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=dynpendchain.htm):

  ```abap
  CHAIN.
    FIELD dynp_field1.
    FIELD dynp_field2.
    ...
    MODULE mod ON CHAIN-INPUT.  
    "Note: ON REQUEST is also possible
  ENDCHAIN.
  ```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Input Checks

- Automatic input checks 
  - Are performed in the PAI event before data is transported to the ABAP program and before dialog modules are called. 
  - If an error is detected, it is displayed in the status bar, and the affected input fields remain ready for input. 
  - Further PAI processing is not started until the user has successfully corrected the input.
  - For example, automatic input checks are performed for required fields: Input fields marked as `required` in the screen painter must be filled in by the user.
   
- Exit command
  - Since the automatic input checks are implicit, the user is always forced to make a valid input before the ABAP program can react. 
  - This is inconvenient in cases where users have changed their minds and only want to cancel processing. For this reason, there is a special function type exit command (`E`), which allows you to bypass the automatic input checks. 
  - You can choose this function type in the screen painter for screen elements with function codes, or in the menu painter for function codes. 
  - Such a function code bypasses the automatic input checks and leads directly to the call of a special dialog module using the following statement in the screen flow logic:

    ```abap
    MODULE mod AT EXIT-COMMAND.
    ```

  - Within the dialog module called in this way, you should end processing with an appropriate `LEAVE` statement. Otherwise, the normal PAI processing, which includes the automatic input checks, starts after the dialog module has been executed.

- Self-programmed input checks
  - For input checks that go beyond the automatic checks, you can program special dialog modules in which you can issue a warning (message of type `W`) or error message (`E`) using the ABAP statement [`MESSAGE`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapmessage.htm). 
  - These dialog modules are called in the flow logic using the `FIELD` and `CHAIN` statements.
  - To check a single field, use the `FIELD` statement as follows:

    ```abap
    FIELD dynp_field MODULE mod.
    ```

  - Within the dialog module `mod`, program your input validation and trigger a warning or error message if an error occurs. The field in question is the only field that is ready for input again, and  users can (in the case of a warning) or must (in the case of an error message) correct their input before the dynpro can be successfully exited.

  - To check several semantically related input fields, define a processing chain using the `CHAIN` statement:
    ```abap
    CHAIN.
      FIELD dynp_field1.
      FIELD dynp_field2.
      ...
      MODULE mod.
    ENDCHAIN.
    ```

  - If a warning or error message is triggered in the `mod` dialog module, all fields listed in the processing chain are ready for input again. In this way, users can correct an input that consists of several closely related individual fields, and where it is not clear from the start which of the individual fields users must change in order to create a valid input as a whole.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Field and Input Help

- Field help: 
  - Provides an explanation of the input field that appears when the user presses *F1* on a field on a screen layout.
  - When you create input fields with reference to DDIC types, you can benefit from data element documentation (which you can create yourself if it does not exist). This documentation is then automatically displayed when the user requests F1 help.
  - You can program field help yourself. You can call your own dialog modules for the POH event and program any help functions there:
    ```abap
    PROCESS ON HELP-REQUEST
      FIELD dynp_field MODULE mod.
    ```
  - At this point, the `FIELD` statement does not transport any data to the ABAP program, since field help is always independent of user input. In the `mod` dialog module, you must then display the appropriate help yourself. You can do this by calling the function module `HELP_OBJECT_SHOW`, for example, or by using GUI controls.
  
- Input help: 
  - List of values displayed when *F4* is chosen for a field on the screen layout
  - This help can either come from the ABAP Dictionary or be self-programmed.
  - There are several ways to create helps in the ABAP Dictionary. For more information, see the topic [Input Helps in the ABAP Dictionary](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_dynpros_value_help_auto.htm).
  - If the built-in input helps in the ABAP Dictionary and the search help exits are not sufficient for your purposes, you can program your own input helps. In this case, you can call your own dialog modules for the event POV and program input helps there.

    ```abap
    PROCESS ON VALUE-REQUEST
      FIELD dynp_field MODULE mod.
    ```

  - As with the POH event, the `FIELD` dynpro statement does not transport data between the dynpro field and the ABAP variable. You must therefore program the data transport yourself by calling the function modules `DYNP_VALUES_READ` and `DYNP_VALUES_UPDATE`. The function module `F4IF_INT_TABLE_VALUE_REQUEST`, which you can call in your dialog module, receives an internal table as a value list and transfers the dialog with the user and the data transport for you. 
  - [Dropdown list box](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendropdown_listbox_glosry.htm)
    - A special form that can be linked to an input field.    
    - When an input field is linked to a dropdown list box, the input value can only be selected from the list. Dropdown list boxes are therefore suitable for cases where the list of values is not too extensive and no other values than those in the list are allowed.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Dnypro Sequence, Calling and Leaving Dynpros

### Dynpro Sequence

- The [dynpro sequence](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendynpro_sequence_glosry.htm) is a sequence of different dynpros that are presented to the user one after the other. 
- The first dynpro in a dynpro sequence is the [initial dynpro](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abeninitial_dynpro_glosry.htm).
- The flow of the dynpros is determined by the next dynpro for each dynpro involved. 
- Each dynpro has a next dynpro. 
- The next dynpro is defined either statically (the number you enter in the dynpro properties) or in the ABAP program using statements which temporarily and dynamically overwrite the static definition of the next dynpro.
- This means that the next dynpro is automatically called when the current dynpro is exited, i.e. when the PAI processing is finished, the current dynpro is also finished and the next dynpro is called.
- A dynpro is normally exited when the end of PAI processing is reached. 
- If a dynpro is connected to a next dynpro with the dynpro number 0 (this dynpro does not exist), it is the last dynpro of the dynpro sequence.
- All dynpros in a dynpro sequence are displayed in the same GUI window. 
- Starting a dynpro sequence: When calling transaction codes (in a dialog transaction, the dynpro associated with the transaction code) or using a `CALL SCREEN` statement (the initial dynpro is the dynpro specified in this statement).
- Ending a dynpro sequence: As mentioned above, a dynpro sequence is terminated when the next dynpro with a dynpro number of 0 is called.
- The simplest dynpro sequence consists of a single dynpro with 0 as the next dynpro.
- When the current dynpro sequence is finished, the system returns to the previous dynpro sequence if the current dynpro sequence was nested.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### ABAP Statements for Calling and Leaving Dynpros

[`SET SCREEN dynnr.`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapset_screen.htm)
- Sets the next dynpro 
- Dynamically and temporarily overwrites the static definition and/or a previously set next dynpro
- Can also be 0
- The next dynpro is automatically called when the end of PAI processing for the current dynpro is reached. If the next screen number is 0, the current dynpro sequence is terminated.

```abap
"Assumption: The static next dynpro is 9100.
"With the statement, the next dynpro is dynamically set and 9100 overwritten.

SET SCREEN 9200.

***************************

"The current dynpro sequence is terminated.
SET SCREEN 0.

```

[`CALL SCREEN dynnr.`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcall_screen.htm)
- Starts a new dynpro sequence, which is embedded in the current dynpro sequence. 
- The dynpro that is specified is the initial dynpro of the dynpro sequence.
- This allows nested dynpro sequences to be created, i.e. if a dynpro sequence was already running at the time of the call, the newly started dynpro sequence is embedded in the already running one.
- By default, all the dynpros of the called dynpro sequence are displayed in the current GUI window. 

  ```abap
  "Starting a new dynpro sequence

  CALL SCREEN 9300.
  ```

- The `STARTING AT` and `ENDING AT` additions can be used to open a dynpro in a modal dialog box, i.e. a dynpro is displayed in a popup over the previous dynpro.
  
  ```abap
  "Opening a dynpro in a modal dialog box
  CALL SCREEN dynnr STARTING AT col_up_left line_up. 
  CALL SCREEN dynnr STARTING AT col_up_left line_up ENDING AT col_up_right line_low.
  ```

- The specifications (integer values) for `STARTING AT` and `ENDING AT` define the position of the dialog box with respect to the previous dynpro and its size:
  `col_up_left` and `line_up` stand for values for the column for the upper left corner and the upper line of the dialog box. The values refer to the GUI window with popup level 0 (maximum popup level is 9). The right column and lower line is set automatically or explicitly by specifying `col_up_right` and `line_low`. The values of `col_up_left` and `line_up` should be less than `col_up_right` and `line_low`, otherwise the behavior will be undefined. 

[`LEAVE SCREEN.`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapleave_screen.htm) and [`LEAVE TO SCREEN dynnr.`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapleave_screen.htm)
- As mentioned earlier, a dynpro is normally exited at the end of PAI processing.
- `LEAVE SCREEN.` exits the current dynpro and enters the next dynpro. This is either statically defined in the properties of the current dynpro or was previously set with the `SET SCREEN` statement.
- `LEAVE TO SCREEN` does the same, but first sets the next dynpro to the specified dynpro number. This statement is a short form of the statements `SET SCREEN dynnr. LEAVE SCREEN.`.

> **💡 Note**<br>
> - The statements do not exit the entire dynpro sequence and instead branch to another dynpro in the same sequence. Only if the number 0 is used to branch to the next dynpro does a `LEAVE` statement terminate the dynpro sequence.
> - A dialog transaction can be started from an ABAP program using the [`CALL TRANSACTION`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcall_transaction.htm) or [`LEAVE TO TRANSACTION`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapleave_to_transaction.htm) statements, or directly by the user by entering the transaction code in the input field of the [standard toolbar](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_toolbar_glosry.htm). When a dialog transaction is started, the associated ABAP program is loaded and the PBO processing of the initial dynpro is called.
> - [`LEAVE PROGRAM`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapleave_program.htm) statements terminate the program. 

  ```abap
  "Assumption: The next dynpro is statically defined as 9100.
  "The following statement ends the processing of the current dynpro. 
  "Dynpro 9100 is called.
  LEAVE SCREEN.

  ****************************************

  "The following statement ends the processing of the current dynpro. 
  "The statically defined dynpro 9100 is overwritten. Dynpro 9200 is called.
  SET SCREEN 9200.
  LEAVE SCREEN.

  ****************************************

  "The following statement is a shorter form of the one above.
  "It has the same effect.
  LEAVE TO SCREEN 9200.

  ****************************************

  "Ending the dynpro sequence
  SET SCREEN 0.
  LEAVE SCREEN.
  ```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Modifying Static Attributes of Screen Elements

- For each screen element, you can define various static attributes that control, for example, its appearance or status. 
- The static attributes of the screen elements can be overwritten from within the ABAP program during the PBO processing of the dynpro.
- To do this, use the ABAP statements [`LOOP AT SCREEN`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaploop_at_screen.htm) and [`MODIFY SCREEN`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapmodify_screen.htm). 
- `LOOP AT SCREEN`: 
  - Defines a loop over all screen elements of the current dynpro to which a dynpro field is assigned. 
   - A work area of type `SCREEN` should be declared. 
   - After the `LOOP AT SCREEN` statement, the work area contains the properties of the corresponding screen element.
- `MODIFY SCREEN`:   
  - Can only be used in the statement block after `LOOP AT SCREEN` and only makes sense during PBO processing.  
  - Expects a work area of type `SCREEN` after `FROM`. 
  - Modifies the properties of the current screen element with the values from the work area.
  - Note the use of the values 0 and 1 by the dynpro in contrast to Boolean values in ABAP.

```abap
MODULE pbo_9000 OUTPUT.
  "Assumption: An input field is not marked as required 
  "(i.e. the value of the component is 0).
  "The example makes the input field required.
  LOOP AT SCREEN INTO DATA(scr).
    IF scr-name = 'SOME_INPUT_FIELD'.
      scr-required = '1'.    
      MODIFY SCREEN FROM scr.
    ENDIF.
  ENDLOOP.  
ENDMODULE.
```
 
<p align="right"><a href="#top">⬆️ back to top</a></p> 


## Statements for the GUI Status and Title 

- [GUI status](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abengui_status_glosry.htm)
  - Groups the [menu bar](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenmenu_bar_glosry.htm), [standard toolbar](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_toolbar_glosry.htm), and [application toolbar](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenapplication_toolbar_glosry.htm) of a GUI window as well as the function key settings. 
  - A GUI status is set using the [`SET PF-STATUS`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapset_pf-status_dynpro.htm) statement and is created using the menu painter.

    ```abap
    "Setting the GUI status (in PBO)
    SET PF-STATUS status.

    ****************************************

    "Setting the GUI status and excluding function codes, i.e. the 
    "specified function codes are deactivated. fcode expects either 
    "a character-like data object or an internal table with a flat 
    "character-like line type.
    SET PF-STATUS status EXCLUDING fcode.
    ```
  - The name of the current GUI status can be obtained from the `sy-pfkey` system field and from the [`GET PF-STATUS`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapget_pf.htm) statement.
  
    ```abap
    "Getting the GUI status
    GET PF-STATUS status.
    st = sy-pf-key.

    ****************************************

    "The following statement inserts the function codes which are 
    "inactive in the current GUI status line by line into the 
    "internal table fcode
    GET PF-STATUS status EXCLUDING fcode.
    ```

- [GUI title](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abengui_title_glosry.htm)
  - Text that can be displayed in the title bar of a GUI window. 
  - A GUI title is set using the statement [`SET TITLEBAR`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapset_titlebar_dynpro.htm) and created using the menu painter.

    ```abap
    "Setting a title (in PBO)
    "Here, title is not the text content itself, but the name of a GUI title, 
    "a further UI component of the program.
    SET TITLEBAR title.

    "The name of the current GUI title is displayed in the system field sy-title.
    gui_title = sy-title.

    ****************************************

    "WITH addition
    "The placeholders of a GUI title can be replaced by the formatted 
    "content of data objects text1, and so on.
    SET TITLEBAR title WITH text1 ... text9.
    ```

> **💡 Note**<br>
>  By separating the GUI status and title from the dynpro itself, the screen layout can remain constant when switching dynpros, and only the title and available functions can be changed.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Controls

- Dynpro [controls](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencontrol_glosry.htm)
  - Complex screen elements with built-in functions that go beyond simple screen elements
  - Require additional variables and a [`CONTROLS`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcontrols.htm) statement in the ABAP program (note that wizards are available)
  - There are two types of dynpro controls:
    - [Table control](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentable_control_glosry.htm) for the input and output of tabular data
    - [Tabstrip control](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentabstrip_control_glosry.htm) for grouping screen elements on different tab pages
- [GUI controls](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abengui_control_glosry.htm)
  - External GUI controls that are included visually in dynpros

### Table Controls

- Used to display screen elements in tabular form 
- Allows users to display or edit not only individual values, but the contents of entire (internal) tables
- Declaring a table control in the ABAP program:
  ```abap
  CONTROLS contr TYPE TABLEVIEW USING SCREEN dynnr.
  ```
- Result: A structure with the name of the control is created in the ABAP program. The structure components contain the properties of the table control and allow you to process the control in the ABAP program, e.g. to change and read the properties of the corresponding table control.
- For the table controls, loops must be implemented in the dynpro flow logic using [`LOOP WITH CONTROL`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=dynploop.htm) statements that process table controls. 
- The loop sequentially processes the displayed lines of the table control by performing one loop pass for each table control line.
- The implementation of the loop must be done for each table control for both the PBO processing block and in the PAI processing block.
- In the loop, you can call dialog modules to process the relevant data objects in the ABAP program. For example, you can read data from an internal table at PBO and write it back to the table at PAI after processing it in the dynpro.


```abap
PROCESS BEFORE OUTPUT.
...
LOOP WITH CONTROL contr.
  MODULE mod_fill_table.
ENDLOOP.
...

PROCESS AFTER INPUT.
...
LOOP WITH CONTROL contr.
  MODULE mod_read_table.
ENDLOOP.
...
```

> **💡 Note**<br>
> - In a modern program, it is more comfortable to use an ALV Grid control.
> - More addiitions are available for the statement.

The ABAP statement [`REFRESH CONTROL`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaprefresh_control.htm) initializes the properties of a table control.

```abap
"Code in an ABAP Program
...
REFRESH CONTROL contr FROM SCREEN dynnr.
...
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Tabstrips Controls
- Allow tab pages to be displayed on dynpros
- Represent one of several available [subscreens](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensubscreen_glosry.htm) in dynpros. Users select the subscreen to be displayed using tab pages. 
- Declaring a tabstrip control in the ABAP program:
  ```abap
  CONTROLS tabstr TYPE TABSTRIP.
  ```
- Result: A structure with the name of the control is created in the ABAP program. From this structure, only the component `activetab` is required in the program.
- For the tabstrip controls, suitable subscreens must be called using [`CALL SUBSCREEN`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=dynpcall.htm) statements in the flow logic.
- In PBO processing, the active tab page is set by assigning the function code of a tab title to the `activetab` component. By default, the first tab page is active.
- For `CALL SUBSCREEN`, there is one variant for the PBO and one variant for the PAI event. 
  - PBO: `CALL SUBSCREEN sub_area INCLUDING prog dynnr.`
    - Includes the subscreen dynpro of the program and the dynpro number in a subscreen area of the current dynpro. It also processes its PBO flow logic at this point. After PBO processing of the subscreen dynpro, the flow logic of the current dynpro is continued after the `CALL SUBSCREEN` statement.
  - PAI: `CALL SUBSCREEN sub_area.`
    - Calls the PAI flow logic of the subscreen dynpro included in the subscreen area. 

Example: 
```abap
"Code in a PBO dialog module 
...
"Providing a dynpro number
dynnr = '9400'.
"Assign the activetab component
tabstr-activetab = 'SOME_TAB'.
"Providing teh prgram name
prog = sy-repid.
...

******************************************************

"Code in the flow logic

PROCESS BEFORE OUTPUT.
 ...
 CALL SUBSCREEN sub_area INCLUDING prog dynnr.
 ...

PROCESS AFTER INPUT.
  ...
  CALL SUBSCREEN sub_area.
  ...

```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### GUI Controls
- Are components of the presentation view of an AS ABAP. 
- They are not directly integrated into the ABAP language and are therefore not part of the classic ABAP interfaces. However, they are part of the SAP GUI and are designed to integrate seamlessly with classic dynpros.
- Are relatively modern UI elements that are used, for example, to display different types of data (images, HTML pages, hierarchical trees, and so on) in different ways or to enable user input.
- Are integrated into a dynpro interface using container controls that provide a screen area for the actual GUI control (e.g. by a custom container). 
- Examples of GUI controls
  - Toolbar control (`CL_GUI_TOOLBAR`): Implements an application toolbar that is independent of the GUI status. The input elements are created by method calls (for example, `ADD_BUTTON`). User actions are signaled by raising ABAP Objects events (for example, `FUNCTION_SELECTED`).
  - Picture control (`CL_GUI_PICTURE`): Allows to display images in BMP, JPG, or GIF format. The dynpro to be displayed is loaded using the `LOAD_PICTURE_FROM_URL` method. Events such as `PICTURE_DBLCLICK` signal user actions.
  - Browser control (`CL_GUI_HTML_VIEWER`): A browser for HTML pages or XML documents on the dynpro. Special links in the document trigger the `SAPEVENT` event when the user clicks, which can then be evaluated in the ABAP program. 
  - Edit control (`CL_GUI_TEXTEDIT`): A simple text editor with the basic functions such as select, find, and replace on the dynpro. These functions can also be called in a program-controlled way using methods such as `FIND_AND_REPLACE`.
  - Tree control: Are available in different versions (for example, `CL_GUI_SIMPLE_TREE`). They allow hierarchical relationships to be displayed in tree structures. 
  - ALV Grid control: Is the replacement for classic lists. It provides functions such as searching, sorting, and printing the content of the list. However, the associated class `CL_GUI_ALV_GRID` should no longer be used directly for new developments. Classes such as `CL_SALV_TABLE` encapsulate the use of the ALV Grid control and simplify the integration.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information

- [SAP GUI User Dialogs](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_screens.htm) in the ABAP Keyword Documentation as the entry topic for dynpro-related topics
- Documentation about the [screen painter in the Help Portal](https://help.sap.com/docs/ABAP_PLATFORM_NEW/bd833c8355f34e96a6e83096b38bf192/d1801b50454211d189710000e8322d00)
- Documentation about the [menu painter in the Help Portal](https://help.sap.com/docs/ABAP_PLATFORM_NEW/bd833c8355f34e96a6e83096b38bf192/d1801ce8454211d189710000e8322d00)
- Find more dynpro examples in the ABAP Keyword Documentation. In ADT, in your system, choose `CTRL + SHIFT + A` to open the search. Insert `demo_dynpro*` to get a list of dynpro examples.

## Executable Example

After the import of the repository, proceed as follows: 
- Find the program in ADT using the search by choosing `CTRL + SHIFT + A`.
- Enter `zdemo_abap_dynpro` and open the program. 
- Run the program by choosing `F8`.

> **💡 Note**<br>
> - The steps about how to import and run the code are outlined [here](README.md#-getting-started-with-the-examples). 
> - The executable example ...
>   - does not claim to include meaningful dynpros with meaningful dynpro sequences (branching to new dynpro sequences occur through using appropriate statements).
>   - is not intended to be a role model for proper dynpro design.   
>   - is not intended to solve concrete programming tasks. You should always work out your own solution for each individual case.
>   - is only intended to demonstrate a selection of keywords and visualize dynpro-related syntax in action on a high level. 
> - See notes on the executable example in the expandable section below. 



<br>
<details>
  <summary>Expand to see explanations of the executable example</summary>
  <!-- -->
  <br>
Th example demonstrates dynpro-related statements. In the dynpros, you can select various options for checking out the effect of the syntax. 
It covers the following aspects:

- Dynpro flow logic and related statements (`MODULE`, `FIELD`, `CHAIN`/`ENDCHAIN`, `LOOP`/`ENDLOOP`, `CALL SUBSCREEN`)
- ABAP statements for calling and leaving dynpros (`SET SCREEN`, `CALL SCREEN`, `LEAVE
SCREEN`)
- Modifying static attributes (`LOOP AT SCREEN`, `MODIFY SCREEN`),
- Statements related to the GUI status and title (`GET`/`SET PF-STATUS`, `SET TITLEBAR`)
- Controls (table and tabstrip controls)

**First dynpro** (the "home page"):
- Selection options for what can be explored in other dynpros
- Choose the *Go* button to switch to another dynpro

**Dynpro "Example of screen elements"**:
- Demonstrates several screen elements
- Pushbuttons, input field, boxes, checkboxes, radio buttons

**Dynpro "Statements I"**:
- Selection options for various dynpro-related ABAP statements
- When you choose a radio button and *Go*, a message is displayed providing some information. 

**Dynpro "Statements II"**:
- Covers statements in the flow logic
- The option `MODULE ... AT EXIT-COMMAND` opens another dynpro. It demonstrates the exit command. The input field is required to be filled. This denies the processing after choosing the *Close 1* and *Cancel* buttons. This is not true for the *Close 2* button.

**Dynpro "Controls"**:
- Shows several controls: Table and tabstrip controls as well as an ALV Grid control as an example for a GUI control
</details>