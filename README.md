[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-cheat-sheets)](https://api.reuse.software/info/github.com/SAP-samples/abap-cheat-sheets)

<a name="top"></a>

# ABAP Cheat Sheets: Demo RAP BOs

- [ABAP Cheat Sheets: Demo RAP BOs](#abap-cheat-sheets-demo-rap-bos)
  - [Getting Started](#getting-started)
  - [Example Notes](#example-notes)
  - [\*99: Managed / Draft-Enabled / Late Numbering / Validation / Determination / Side Effects](#99-managed--draft-enabled--late-numbering--validation--determination--side-effects)
    - [Example Context](#example-context)
    - [Involved Artifacts](#involved-artifacts)
    - [BDEF and Implementation Notes](#bdef-and-implementation-notes)
    - [Preparation Steps](#preparation-steps)
    - [Example Exploration Steps](#example-exploration-steps)
  - [\*98: Managed / Draft-Enabled / Internal Early Numbering / Actions / Feature Control](#98-managed--draft-enabled--internal-early-numbering--actions--feature-control)
    - [Example Context](#example-context-1)
    - [Involved Artifacts](#involved-artifacts-1)
    - [BDEF and Implementation Notes](#bdef-and-implementation-notes-1)
    - [Preparation Steps](#preparation-steps-1)
    - [Example Exploration Steps](#example-exploration-steps-1)
  - [\*97: Unmanaged / External Numbering / Side Effects](#97-unmanaged--external-numbering--side-effects)
    - [Example Context](#example-context-2)
    - [Involved Artifacts](#involved-artifacts-2)
    - [BDEF and Implementation Notes](#bdef-and-implementation-notes-2)
    - [Preparation Steps](#preparation-steps-2)
    - [Example Exploration Steps](#example-exploration-steps-2)
  - [\*96: Managed with Additional Save / Local RAP Business Events / Authorization / ABP Auxiliary Class](#96-managed-with-additional-save--local-rap-business-events--authorization--abp-auxiliary-class)
    - [Example Context](#example-context-3)
    - [Involved Artifacts](#involved-artifacts-3)
    - [BDEF and Implementation Notes](#bdef-and-implementation-notes-3)
    - [Preparation Steps](#preparation-steps-3)
    - [Example Exploration Steps](#example-exploration-steps-3)
  - [\*95: Managed RAP BO / Unmanaged Internal Early Numbering / Composition / Functions](#95-managed-rap-bo--unmanaged-internal-early-numbering--composition--functions)
    - [Example Context](#example-context-4)
    - [Involved Artifacts](#involved-artifacts-4)
    - [BDEF and Implementation Notes](#bdef-and-implementation-notes-4)
    - [Preparation Steps](#preparation-steps-4)
    - [Example Exploration Steps](#example-exploration-steps-4)
  - [⚡ Known Issues](#-known-issues)
  - [⚠️ Disclaimer](#️-disclaimer)
  - [📟 Support and Contribution](#-support-and-contribution)
  - [📜 License](#-license)


The `rap` branch of the [ABAP cheat sheet GitHub repository](https://github.com/SAP-samples/abap-cheat-sheets) features a selection of demo RAP BOs designed to illustrate various RAP-related features, syntax, and concepts. These examples are associated with the RAP BDL and ABAP EML cheat sheets.

> [!IMPORTANT]
> - The examples aim to illustrate basic RAP features, syntax, and concepts using simplified, non-semantic demo RAP BOs, intended to reduce complexity and provide a high-level overview. 
> - The examples serve the following purposes: 
>   - [Exploration, experimentation, and demonstration](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/README.md#%EF%B8%8F-disclaimer). 
>   - Highlighting syntax and technical aspects while considering the end-to-end aspect using predefined SAP Fiori UIs as RAP BO consumers.
>   - Offering insight into RAP BDL syntax and functionality.
>   - Providing a glimpse of the involved artifacts, potential RAP BO setups, and the interactions between RAP BO consumers and providers.
>   - Allowing exploration of RAP functionality and the effect of syntax options with preview SAP Fiori UIs.
> - The example RAP BOs are (randomly) set up to showcase specific features through smaller BOs, each focusing on selected RAP topics.
> - Given their experimental nature, these examples do not represent best practices for RAP BO setup and implementation. They do not claim to fully meet the [RAP BO contract](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/3a402c5cf6a74bc1a1de080b2a7c6978.html) requirements. Always create your own solutions.
> - For more detailed information and semantic RAP examples, refer to the [Development guide for the ABAP RESTful Application Programming Model](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/289477a81eec4d4e84c0302fb6835035.html) and the [ABAP Flight Reference Scenario for the ABAP RESTful Application Programming Model](https://github.com/SAP-samples/abap-platform-refscen-flight).


## Getting Started

> [!NOTE]
> - The code examples in this branch are designed to function independently from those in the `main` branch. Therefore, you can clone this branch without also cloning the `main` branch.
> - The following steps outline the import procedure in the SAP BTP ABAP environment.
> - If you have already imported the `main` branch of the ABAP cheat sheet repository, the repository is still linked in the *abapGit Repositories* view, and you want to have the artifacts of the `rap` branch in the same package, you can proceed with the steps in the note below. Otherwise, open the *abapGit Repositories* view in ADT, filter for the linked repository, right-click it, and choose _Unlink_.
> - If you face activation problems, leaving certain artifacts from the repository inactive once imported, refer to the [Known Issues](#-known-issues).

Use the [abapGit](https://github.com/abapGit/eclipse.abapgit.org) plug-in to install the examples by carrying out the following steps:

1. In your ABAP cloud project, create a package, for example, *ZABAP_CHEAT_SHEETS_RAP* as the target package. The package should be local.
2. Add the package to the *Favorite Packages* in the *Project Explorer* view in ADT.
3. To add the <em>abapGit Repositories</em> view to the <em>ABAP</em> perspective, choose *Window* → *Show View* → *Other...* from the menu bar and choose *abapGit Repositories*.
4. In the <em>abapGit Repositories</em> view, choose the `+` icon in the upper right corner of the ADT tab to link a new abapGit repository.
5. The *Link abapGit Repository* popup appears. Enter the following URL. Note that if you have already imported the `main` branch of the ABAP cheat sheet repository and linked it, unlink it first as described in the note above. 

    ```
    https://github.com/SAP-samples/abap-cheat-sheets.git
    ```

6. Choose *Next*. 
7. Provide your Git user and password/token.
8. Choose *Next*. A pop-up is displayed prompting you to choose to store or delete credentials in/from secure storage.
9.  On the *Branch and Package Selection* screen, select the `rap` branch and enter the name of the created package (for example, *ZABAP_CHEAT_SHEETS_RAP*) in the *Package* field. 
10. Choose *Next*.
11. On the *Select Transport Request* screen, choose *Finish* to link the Git repository to your ABAP cloud project. The package should be local.
12. In the *abapGit Repositories* view, filter for your package. The repository appears in the *abapGit Repositories* view with the status <em>Linked</em>.
13. Right-click on the new abapGit repository and choose *Pull...* to start the cloning of the repository contents. Note that you can also choose the pulling in a previous wizard step.
14. On the *Branch and Package Selection* screen, choose *Next*.
15. On the next screen, select the objects (select the package/top-most selection box to automatically select all artifacts) from the list and choose *Next*.
16. On the next screen - the package should be local - choose *Finish*. Same as above, if an *object already locked* message is displayed, choose *Finish* as well. The status in the *abapGit Repositories* view changes to <em>Pull running...</em>. Note that the pull run may take several minutes.
17. Once the cloning is complete, the status changes to *Pulled Successfully*. You may need to refresh the *abapGit Repositories* view to see the progress of the import. To do this, choose the  *Refresh* icon in the upper right corner of the view.
18. Refresh your project tree. For example, in ADT, right-click the package and choose *Refresh*. The package should contain all the artifacts from the GitHub repository.
19. Make sure that all artifacts are active. To activate all inactive development objects, choose the *Activate all inactive ABAP development objects* button from the menu (or choose *CTRL+Shift+F3*). If you encounter activation issues when trying to mass mass activate the artifacts, try to activate the artifacts bulk-wise and per artifact kind. Start with the database tables, then the CDS view entities, the BDEFs, classes and then the service-related artifacts. Once done, try to run mass activation again to check on still not activated artifacts. Activate not yet activated artifacts so that all artifacts of the repository can be used.


> [!NOTE]
> If you have already imported the `main` branch of the ABAP cheat sheet repository, and linking the repository anew is rejected, proceed as follows to include the artifacts in the package of the already imported repository: 
> - Open the *abapGit Repositories* view in ADT. 
> - Right-click the line with the above GitHub URL. Choose _Switch Branch_. Provide user credentials when prompted.
> - On the *Branch Selection* screen, select the `rap` branch. The package entry remains unchanged.
> - Choose *Next*.
> - In the *abapGit Repositories* view, the _Status_ should be _Linked_.
> - Right-click the line and choose _Pull_.
> - In the _Object Selection for Pull_ screen, you may choose _Next_ without selecting any artifacts for deletion.
> - Once having finished the wizard, the _Status_ should be _Pull starting_. You can choose the *Refresh* icon to check on the progress.
> - Make sure that all artifacts are active. To activate all inactive development objects, choose the *Activate all inactive ABAP development objects* button from the menu (or choose *CTRL+Shift+F3*).

> [!IMPORTANT]
> Before getting started with RAP BO exploration, make sure that you have walked through the preparation steps outlined for each example.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Example Notes

The example RAP BOs highlight syntax and technical aspects while considering the end-to-end process using predefined SAP Fiori UIs as RAP BO consumers. 
These examples are designed to showcase specific features through smaller BOs, each focusing on selected RAP topics. 
The following list provides an overview of the example RAP BOs and their focus topics. To help differentiate the artifacts per RAP BO, each RAP BO follows a naming convention that includes the suffixes 99 to 95.

Focus Topics

<table>

<tr>
<th> Example </th> <th> Focus Topics </th>
</tr>

<tr>
<td> 

*99

 </td>

 <td> 

<ul>
<li>Managed, draft-enabled RAP BO</li>
<li>Numbering concept: Late numbering</li>
<li>Validation</li>
<li>Determination</li>
<li>Side effects</li>
</ul>

 </td>
</tr>

<tr>
<td> 

*98

 </td>

 <td> 
  
<ul>
<li>Managed, draft-enabled RAP BO</li>
<li>Numbering concept: Managed internal early numbering</li>
<li>Actions</li>
<li>Global and instance feature control</li>
</ul>

 </td>
</tr>

<tr>
<td> 

*97

 </td>

 <td> 
  
<ul>
<li>Unmanaged RAP BO</li>
<li>Numbering concept: External numbering</li>
</ul>

 </td>
</tr>

<tr>
<td> 

*96

 </td>

 <td> 
  
<ul>
<li>Managed RAP BO with additional save</li>
<li>Numbering concept: Managed internal numbering</li>
<li>Raising local RAP business events</li>
<li>Global and instance authorization, access control</li>
<li>ABP auxiliary class</li>
</ul>

 </td>
</tr>

<tr>
<td> 

*95

 </td>

 <td> 

<ul>
<li>Managed RAP BO, composition relationship of a root entity and one child entity</li>
<li>Numbering concepts: Unmanaged internal early numbering (root entitiy) and managed internal early numbering (child entity)</li>
<li>Functions</li>
</ul>

 </td>
</tr>

</table>


> [!NOTE]  
> - To create a comprehensive RAP BO from scratch, you can use a [wizard](https://help.sap.com/docs/abap-cloud/abap-rap/odata-ui-service-from-scratch).
> - Projections are available for most of the demo examples to have a less minimalistic setup. However, they just reflect the base BO functionality here.
> - Some service bindings use OData V4, while others use OData V2. At the time of creating the examples, certain restrictions applied, leading to the choice of V2 to allow CUD operations for non-draft RAP BOs.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## *99: Managed / Draft-Enabled / Late Numbering / Validation / Determination / Side Effects

### Example Context

- This demo RAP BO represents a _"RAP calculator"_. A RAP BO instance represents, among others, the result of a simple calculation based on two operands with two integer values and an operator.
- The simplified RAP BO only includes the root entity.
- Primary focus topics of the example: 
  - Managed, draft-enabled RAP BO  
  - Numbering concept: Late numbering
  - Validation
  - Determination
  - Side effects

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Involved Artifacts

- All involved artifacts have the suffix 99
- Data modeling: 
  - Database table: `ZDEMOABAP99`
  - View entity: `ZR_DEMO_ABAP99`
  - CDS metadata extensions: `ZC_DEMO_ABAP99`  
- Behavior
  - BDEF: `ZR_ROOTENTITY99`
  - ABP: `zbp_r_rootentity99`
- Projection 
  - Projection view: `ZC_DEMO_ABAP99`
  - Projection BDEF: `ZC_DEMO_ABAP99`
- Service
  - Service definition: `ZUI_DEMO_ABAP_O499`
  - Service binding: `ZUI_DEMO_ABAP_O499`
- Further artifacts
  - Number range object: `ZNR_DEMO99`
  - Domain: `ZDEMO_ABAP_DO99`
  - CDS abstract entities: `ZDEMO_ABAP_ABSTRACT99`

<p align="right"><a href="#top">⬆️ back to top</a></p>

### BDEF and Implementation Notes

- Managed draft-enabled RAP BO
- In addition to the persistent table, a draft table specification is required.
- The authorization master is set to _none_ for demonstration purposes (every user can access and modify it).
- Numbering scenario: Late numbering. It requires implementing the `adjust_numbers` method in the RAP saver class to assign final numbers to key fields.
- Various field characteristics: Some fields are read-only, while others are mandatory.
- CUD operations are enabled.
- Mandatory draft actions are specified (no implementation needed).
- Validation on save: This validation checks the calculation result value. If errors occur, such as division by zero, the instance will not be saved.
- Determination on modify: Triggers (re)calculation of the result.
- An internal action is used for calculation purposes.
- Side effects are defined to (re)calculate the result based on new input.
- Mapping relationship

> [!NOTE]  
> See comments in the implementation class for more information on the demo implementations.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Preparation Steps

1. **Check Number Range Object**
  
     - The demo RAP BO uses late numbering as numbering concept. To support this, a demo number range object is included in the repository.
     - Open the `ZNR_DEMO99` number range object from _Number Range Management_.
     - Ensure the following settings are applied: 
       - Number Length Domain: `ZDEMO_ABAP_DO99`
       - Percent Warning: 10
       - Rolling: Selected
       - Buffering: Main Memory Buffering
       - Buffered Numbers: 1

2. **Run Utility Class**
  
     - Open the class `zcl_demo_abap_rap_example_util` and run it by right-clicking and selecting _Run as -> 2 ABAP Application (Console)_.
     - The class is designed to display output in the console. It clears demo database tables and sets number range objects for examples using late numbering and unmanaged internal early numbering. This example uses late numbering.

3. **Publish Service and Start Preview UI**
   
     - Open the service binding `ZUI_DEMO_ABAP_O499` from _Business Services -> Service Bindings_.
     - In the opened service binding, choose _Publish_ for the _Local Service Endpoint_. 
     - After the service is created, the _Local Service Endpoint_ will show _Published_, and the _Entity Set and Association_ section will display entries. Activate the service binding.
     - To launch the preview SAP Fiori UI in a browser, select the _demo_abap_ entity from the _Entity Set and Association_ section and choose _Preview_. 


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example Exploration Steps  

> [!IMPORTANT]  
> You have walked through all preparation steps and opened the SAP Fiori preview UI in a browser.

- CUD operations, determination and validation
  - CUD operations can be explored by selecting the _Create_, _Edit_ (on the instance detail screen), and _Delete_ buttons.
  - Choose the _Create_ button.
  - The new object screen is displayed.
  - The ID value is not filled since the example uses late numbering.
  - The field characteristics specified in the BDEF are visible, including the ID and result fields, which are read-only and cannot be user-edited. 
  - Input is required for three fields (two numbers and one operator). These fields are mandatory, and 0 is prefilled as the initial value for the integer fields.
  - The operator field (purposely) allows entry of any single character.
  - Create operation 1:
    - Enter valid values for the three fields.
    - Input two numbers and one of the allowed operators: +, -, /, *, or P.
    - Side effect: The result field displays the calculated value before creating the instance. Note: After making an adaptation, click elsewhere so the adapted field is no longer highlighted for the side effect to take effect.
    - Choose _Create_ at the bottom.
    - The instance should have been successfully created.
    - An ID should be assigned, and the result should be calculated.
  - Create operation 2:
    - Go back.
    - Choose _Create_.
    - Enter two numbers and a wrong operator, such as _#_.
    - Choose _Create_ at the bottom.
    - An error message should be displayed, and the operator field should be highlighted.
    - An ID has not yet been created, and the result shows _Wrong operator_.
    - Correct the operator value using a valid one and choose _Create_. The instance should be created, and the respective values for ID and result should be filled.
  - Update operations:
    - Open an instance from the list.
    - Choose the _Edit_ button.
    - Enter valid new values for the fields.
    - The side effect functionality (after leaving the input fields) should change the result value.
    - Choose _Save_ to save the instance.
    - Choose _Edit_ again to demonstrate another failure case.
    - Insert / as the operator and 0 for number2, then select save. The side effect functionality (after leaving the input fields) or choosing _Save_ should trigger an error message (the result value should be _Division by 0_), highlighting the operator and number2 fields. Adjust the values so the instance can be saved.
    - Choose _Edit_ again to demonstrate another failure case.
    - Insert a very high integer value into the number1 field. An error message indicates that the inserted number is out of range (the result value should be _Overflow error_). 
    - Enter a high value within the range, such as 1234567980, for number2 as well, using _P_ as the operator. Selecting Save raises an error (arithmetic overflow).
  - Delete operation:
    - Select an entry from the instance list.
    - The delete button should be enabled.
    - Select the button and confirm.
    - Delete operations can also be triggered on the object detail screen.
- Draft capabilities and late numbering:
  - Begin creating a new entry.
  - The ID field has not been assigned a value yet.
  - Insert the required entries, but do not select create (and do not discard the draft, as this would delete the draft instance that has not yet been persisted).
  - Go back.
  - The draft instance should be displayed in the list, with no ID value added yet (only prior to persisting).
  - Reopen the instance and choose _Create_ at the bottom to persist it. An ID value should now be assigned, reflecting late numbering.


<p align="right"><a href="#top">⬆️ back to top</a></p>

## *98: Managed / Draft-Enabled / Internal Early Numbering / Actions / Feature Control

### Example Context

- Non-semantic, simplified context; field values consist of demo integers and strings
- The simplified RAP BO only includes the root entity.
- Primary focus topics of the example: 
  - Managed, draft-enabled RAP BO
  - Numbering concept: Managed internal early numbering
  - Actions (the demo values of instances are changed through action implementation, solely for the purpose of illustrating various kinds of actions)
  - Global and instance feature control (some actions are specified with feature control)

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Involved Artifacts

- All involved artifacts have the suffix 98
- Data modeling: 
  - Database table: `ZDEMOABAP98`
  - View entity: `ZR_DEMO_ABAP98`
  - CDS metadata extensions: `ZC_DEMO_ABAP98`  
- Behavior
  - BDEF: `ZR_ROOTENTITY98`
  - ABP: `zbp_r_rootentity98`
- Projection 
  - Projection view: `ZC_DEMO_ABAP98`
  - Projection BDEF: `ZC_DEMO_ABAP98`
- Service
  - Service definition: `ZUI_DEMO_ABAP_O498`
  - Service binding: `ZUI_DEMO_ABAP_O498`
- Further artifact
  - CDS abstract entity: `ZDEMO_ABAP_ABSTRACT98`

<p align="right"><a href="#top">⬆️ back to top</a></p>

### BDEF and Implementation Notes

- Managed the draft-enabled RAP BO
- In addition to the persistent table, the draft table specification is necessary.
- The authorization master is none and is used here for demonstration purposes (every user can access and modify it).
- Numbering scenario: Managed internal early numbering is defined using the UUID field and the `(numbering: managed)` specification.
- Various field characteristics: Key and draft-related fields are read-only.
- CUD operations are enabled.
- Mandatory draft actions are specified (no implementation required).
- Multiple actions demonstrate various kinds of actions, including (non-)factory actions, internal actions, and action specifications with additional operations (such as prechecks and feature control).
- Mapping relationship

> [!NOTE]  
> See comments in the implementation class for more information on the demo implementations.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Preparation Steps

1. **Run Utility Class**
  
     - Open the class `zcl_demo_abap_rap_example_util` and run it by right-clicking and selecting _Run as -> 2 ABAP Application (Console)_.
     - The class is designed to display output in the console. It clears demo database tables and sets number range objects for examples using late numbering and unmanaged internal early numbering. The latter is not relevant for this example.

2. **Publish Service and Start Preview UI**
  
     - Open the service binding `ZUI_DEMO_ABAP_O498` from _Business Services -> Service Bindings_.
     - In the opened service binding, choose _Publish_ for the _Local Service Endpoint_. 
     - After the service is created, the _Local Service Endpoint_ will show _Published_, and the _Entity Set and Association_ section will display entries. Activate the service binding.
     - To launch the preview SAP Fiori UI in a browser, select the _demo_abap_ entity from the _Entity Set and Association_ section and click _Preview_.


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example Exploration Steps  

> [!IMPORTANT]  
> You have walked through all preparation steps and opened the SAP Fiori preview UI in a browser.

- CUD operations, managed internal early numbering
  - Explore CUD operations by selecting the _Create_, _Edit_ (on the instance detail screen), and _Delete_ buttons.
  - Choose _Create_.
  - The instance creation screen will display field-specific characteristics and illustrate managed internal numbering.
    - The UUID value is automatically filled with a unique identifier and is read-only.
    - Draft-related fields are also read-only.
    - No mandatory characteristics are specified for the two character-like and numeric fields, so they can be filled optionally.
    - To test the functionality implemented with actions, enter values in the fields.
    - Choose _Create_ at the bottom. The instance should be created.
    - As described in the previous example, you can explore update and delete operations and check draft capabilities (for example, by not saving the instance yet).
  
- Exploring actions (various buttons are available on the UI). Note that for most actions, you need to select an instance from the list first.
  - _Increment Number Fields_ (_increment_numbers_ action): 
    - Instance feature control is in place for this method. If the button is disabled, it means one or both values (num1 and num2) are greater than or equal to 100. If it is enabled, both values are below 100.
    - To check the functionality, create new instances or update existing ones with num1 or num2 values lower than or equal to 100.
    - If you select multiple instances, the button remains disabled if none meet the prerequisite (being lower than 100).
    - If at least one instance meets the prerequisite, the button is enabled, and only those instances will have their num1 and num2 values incremented by 1.
    
  - _To Upper_ (_uppercase_ action)
    - Select one or more instances from the list and choose the button. To visualize the functionality the text1 and/or text2 fields values should include lowercase letters.
    - The values of text1 and text2 will be transformed to uppercase, but this update will not appear on the UI immediately (as the action is not specified with an output parameter). Refresh the UI using the _Go_ button to display the changes.
  
  - _Random Numbers_ (_add_random_numbers_ action)
    - Select one or more instances and choose the button.
    - num1 and num2 will receive random integer values, and this change will reflect on the UI immediately, unlike the _uppercase_ action.

  - _Addition_ (_addition_ action)
    - Select one or more instances and choose the button.
    - Since the action has an input parameter, a popup will appear allowing you to enter an integer value. Input your value and proceed.
    - The values of num1 and num2 will be updated by adding your input to them.

  - _Count_ (_count_ action)
    - As this is a static action, it does not operate on selected instances. Therefore, the button is always enabled.
    - Choose the button to retrieve the number of currently available entries in the underlying database table.

  - _New Instance_ (_new_instance_ action)
    - This is also a static action that does not depend on selected instances. Therefore, the button is always enabled.
    - Clicking the button will create a new instance with random demo values for the text and numeric fields.
    - The method call includes calling the internal action _create_random_strings_.

  - _Copy Instance_ (_copy_instance_ action)
    - This is an instance-based factory action, so select one or more instances and choose the button. To keep the number of instances low, select just one instance.
    - As a result, the selected instances will be copied, each with a new UUID.

  - _To Lower_ (_lowercase_ action)  
    - Select one or more instances and choose the button.
    - A precheck is in place for this method.
    - To explore the functionality, prepare instances as follows: 
      - Example instance 1: Enter text in text1 and/or text2 fields that includes uppercase letters but no numbers.
      - Example instance 2: Enter text in text1 and/or text2 fields that includes at least one number and uppercase letters.
      - Select both instances and choose the button.
      - For the instance containing numbers, an error message will appear, and the uppercase letters will not be converted to lowercase.
      - The text values of the instance without numbers will be converted to lowercase.

  - _Reverse Text Fields_ (_reverse_text_ action)
    - A global feature control is in place for this method. If the button is disabled, the current time falls within a restricted time span for action execution. To explore the action, adjust the code as described in the _get_global_features_ method implementation of ABP `zbp_r_demo_abap98`, or vice versa, if enabled and you want to test disabling, adjust the time values accordingly. After the adjustment, refresh the UI.
      - If enabled, select one or more instances from the list and choose the button. 
      - The values in the text1 and text2 fields will be reversed.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## *97: Unmanaged / External Numbering / Side Effects

### Example Context

- This demo RAP BO is similar to the _*99_ RAP BO representing a RAP calculator. Unlike the _*99_ example, this is an unmanaged, non-draft enabled RAP BO and uses external numbering as numbering concept (that is, the RAP BO consumer provides the key values).
- Also here, an instance represents the result of a simple calculation based on two operands with two integer values and an operator.
- The simplified RAP BO only includes the root entity.
- Primary focus topics of the example: 
  - Unmanaged RAP BO
  - Numbering concept: External numbering

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Involved Artifacts

- All involved artifacts have the suffix 97
- Data modeling: 
  - Database table for the root: `zdemoabap97`
  - View entity for the table: `ZR_DEMO_ABAP97`
  - CDS metadata extension: `ZC_DEMO_ABAP97`
- Behavior
  - BDEF: `ZR_ROOTENTITY97`
  - ABP: `zbp_r_rootentity97`
- Projection 
  - Projection view: `ZC_DEMO_ABAP97`
  - Projection BDEF: `ZC_DEMO_ABAP97`
- Service
  - Service definition: `ZUI_DEMO_ABAP_O297`
  - Service binding: `ZUI_DEMO_ABAP_O297`
- Further artifacts
  - Lock object: `EZDEMOABAPL97`

<p align="right"><a href="#top">⬆️ back to top</a></p>

### BDEF and Implementation Notes

- Unmanaged, non-draft-enabled RAP BO  
- External numbering is used. There is no numbering-related syntax specification in the BDEF, so external numbering is the default. This is reflected in the field characteristics for the key field ID: `mandatory: create, readonly: update`.  
- Unlike the BDEF of the *99 RAP BO, the persistent table is not specified, and a draft table is unnecessary since it is not a draft-enabled BO.  
- To perform calculations, the field characteristics are specified accordingly. The two numbers and the operator are marked as mandatory, while the result is set as readonly to prevent editing by RAP BO consumers.  
- Side effects are defined to recalculate the result upon instance creation or update.  
- The authorization master is set to none for demonstration purposes, allowing every user to access and modify it.  
- CUD operations are enabled.  
- Mapping relationship  

> [!NOTE]  
> - See comments in the implementation class for more information on the demo implementations.
> - The demo RAP BO is designed as a self-contained, unmanaged RAP BO. For that purpose, the "existing" functionality is simulated using the local class `lcl_buffer`, which provides an internal table representing the transactional buffer. This table includes RAP BO instance data and flags indicating whether to create, update, or delete an instance. The flags are represented by an enumeration type. In the example implementations of the handler methods, a preparation method is called within the context of EML requests. Based on the content of the internal table - considering both the instance data and the flags - the underlying database table is modified.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Preparation Steps

1. **Run Utility Class**
   
     - Open the class `zcl_demo_abap_rap_example_util` and run it by right-clicking and selecting _Run as -> 2 ABAP Application (Console)_.
     - The class is designed to display output in the console. It clears demo database tables and sets number range objects for examples using late numbering and unmanaged internal early numbering. The latter is not relevant for this example.

2. **Publish Service and Start Preview UI**

     - Open the service binding `ZUI_DEMO_ABAP_O297` from _Business Services -> Service Bindings_.
     - In the opened service binding, choose _Publish_ for the _Local Service Endpoint_. 
     - After the service is created, the _Local Service Endpoint_ will show _Published_, and the _Entity Set and Association_ section will display entries. Activate the service binding.
     - To launch the preview SAP Fiori UI in a browser, select the _demo_abap_ entity from the _Entity Set and Association_ section and click _Preview_.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example Exploration Steps  

> [!IMPORTANT]  
> You have walked through all preparation steps and opened the SAP Fiori preview UI in a browser.

- CUD operations can be explored by choosing the _Create_, _Edit_ (on the instance detail screen) and _Delete_ buttons.
- The _*99_ example and this RAP BO are similar.
- Unlike the _*99_ example, you have to create the key field value. You can try and assign an ID value that already exists.

<p align="right"><a href="#top">⬆️ back to top</a></p>  

## *96: Managed with Additional Save / Local RAP Business Events / Authorization / ABP Auxiliary Class

### Example Context

- Non-semantic, simplified context; field values consist of demo integers and strings
- The simplified RAP BO only includes the root entity.
- Primary focus topics of the example: 
  - Managed RAP BO with additional save (requiring the implementation of the _save_modified_ method in the RAP saver class)
  - Raising local RAP business events
  - Global and instance authorization, access control (note the preparation steps)
  - ABP auxiliary class

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Involved Artifacts

- All involved artifacts have the suffix 96
- Data modeling: 
  - Database table for the root: `zdemoabap96`
  - View entity for the table: `ZR_DEMO_ABAP96`
  - CDS metadata extension: `ZC_DEMO_ABAP96`
- Behavior
  - BDEF: `zr_demo_abap96`
  - ABP: `zbp_r_demo_abap96`
- Projection 
  - Projection view: `ZC_DEMO_ABAP96`
  - Projection BDEF: `ZC_DEMO_ABAP96`
- Service
  - Service definition: `ZUI_DEMO_ABAP_O296`
  - Service binding: `ZUI_DEMO_ABAP_O296`
- Further artifacts
  - Authorization fields: `ZAUTHFLD96`, `ZAUTHFLB96`
  - Authorization object: `ZAUTHOBJ96`, `ZAUTHOBB96`
  - Event handler class: `zcl_demo_abap_evt_handler96`
  - ABP auxiliary class: `zcl_demo_abap_abp_aux_cl96`
  - DCL access controls: `ZDEMO_ABAP_DCL96`, `ZDEMO_ABAP_DCLPROJ96`
  - CDS abstract entity: `ZDEMO_ABAP_ABSTRACT96`

<p align="right"><a href="#top">⬆️ back to top</a></p>

### BDEF and Implementation Notes

- Managed, non-draft-enabled RAP BO that defines `with additional save`. Thus, the ABP has a local saver class. In this example, the implementation raises local RAP business events.
- Defines an ABP auxiliary class.
- As a managed RAP BO, the persistent table must be specified.
- Unlike the other RAP examples, this RAP BO uses both global and instance authorization, which is specified using the `authorization master` syntax. Note the prerequisite steps for the example.
- Numbering concept: Managed internal numbering (defined using the `numbering : managed` specification).
- CUD operations are enabled.
- Three RAP business events are specified, two of which use a parameter represented by a CDS abstract entity with two components (text and timestamp).
- Static factory action `check_log`: This action is solely for checking and visualizing the effect of event raising.
- Defines validation and determination.
- Mapping relationship.


> [!NOTE]  
> - See comments in the ABP for more information on the demo implementations.
> - If you choose not to include the setup and walk through preparation steps for authorization, the code offers simplified alternatives that allow you to check authorization-relevant effects without those steps.
> - Due to the additional save specification, the RAP BO implementation includes a local saver class that requires the implementation of the `save_modified` method. In this example, the method raises local RAP business events for demonstration purposes. Three event types are defined for create, update, and delete operations. An event is raised based on the input parameters of the `save_modified` method, determining the evaluation of the input BDEF derived type, and indicating affected instances. The create event does not include additional specifications, unlike the update and delete events. In those cases, a demo text and the current timestamp are assigned to `%param`. The associated event handler class is `zcl_demo_abap_evt_handler96`.
> - The event handler class `zcl_demo_abap_evt_handler96` includes a local class that inherits from `cl_abap_behavior_event_handler`. It implements three event handler methods for the defined events. In this simplified example, event raising is represented by inserting information into a database log table. For update and delete events, values from `%param` are used. For create, which has no specified parameters, the information is supplied within the method implementation. Note the required call to `cl_abap_tx=>save( ).` to switch the transactional phase for database modification statements (for more information, refer to [Controlled SAP LUW](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/17_SAP_LUW.md#controlled-sap-luw)). The result of the event raising is visualized through the static action `check_log`. Choosing the associated button (`Check Log Table`) in the UI displays the content of the log database table. The demo text added indicates which event was triggered, along with a timestamp indicating when the event was raised (or when the content was added to the log table in the case of the create event).
> - The ABP auxiliary class illustrates how functionality can be outsourced to an ABP auxiliary class. The main purpose of auxiliary classes is to provide reusable methods for an ABAP behavior pool. The demo implementation includes a method with `SELECT` statements (for data required in the context of the static action) and a method with an ABAP EML read request. The latter demonstrates that the `IN LOCAL MODE` can be used outside the original ABP. Furthermore, an enumeration type is defined, which is used in the determination implementation. Note that completely outsourcing handler method implementations to auxiliary classes is not recommended, as it incurs significant overhead, including additional BDEF derived type declarations for method parameters.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Preparation Steps

> [!NOTE]  
> - If you choose not to walk through the authorization preparation steps, the code includes dummy implementations in the `get_global_autorizations` and `get_instance_authorizations` method (constants indicating authorized or not authorized) that you can use and modify. You can comment these in while commenting out the other code (see the comments in the implementation) to explore global and instance authorization functionality.
> - The authorization setup in this example is intended to only meet the requirements of this simplified example to illustrate authorization-specific functionality. Note that the example is created in the SAP BTP ABAP Environment. Make sure to consult the [SAP Help documentation](https://help.sap.com/docs/sap-btp-abap-environment/abap-environment/apps-identity-and-access-management?locale=en-US) for further and the latest information.

1. **Preparation steps for authorization functionality**
  
     - Make sure that the authorization fields, authorization objects, and DCL access controls are activated.
     - **Create an IAM app**
       - In your target package, choose *New -> Other ABAP Repository Object*.
       - Filter for *IAM app* and choose *Next* and walk through the wizard. 
       - As the name, use *ZABAP_DEMO_IAM96*, and provide a description.
       - As the application type, use *External app*. Note that the artifact's name will then be *ZABAP_DEMO_IAM96_EXT*.
       - Proceed with the wizard and finalize the creation steps.
       - In the IAM app, open the *Authorizations* tab.
       - In the *Authorization Objects* section, add the authorization object *ZAUTHOBJ96* and choose *Ok*. 
       - Choose *ZAUTHOBJ96*, and in the *Instances of ...* section, click the expandable, and select *ACTVT*. Select the display checkbox in the *ACTVT* section on the right.
       - Choose *ZAUTHFLD96* and enter the value `AA` under *From* and `RZ` under *To* in the *ZAUTHFLD96* section on the right.    
       - In the *Authorization Objects* section, add the authorization object *ZAUTHOBB96* and choose *Ok*.  
       - In the *Instances of ...* section, click the expandable, and choose *ACTVT*. Select all checkboxes except display in the *ACTVT* section on the right.
       - Choose *ZAUTHFLB96* and enter the value `DA` under *From* and `IZ` under *To* in the *ZAUTHFLB96* section on the right.   
       - Save and activate.
       - Choose *Publish Locally* in the top right.
       - Choose the _Overview_ tab. The status should be _Published_.
      - **Create a business catalog**
        - In your target package, choose *New -> Other ABAP Repository Object*.
        - Filter for *Business catalog* and choose *Next* and walk through the wizard. 
        - As the name, use *ZDEMO_ABAP_BUS_CAT96*.  
        - In the *Apps* tab, choose *Add*. 
        - Select the IAM app *ZABAP_DEMO_IAM96_EXT*.
        - On the *Business Catalog App Assignment: ZAUTH_BUS_CAT_0001* screen, the *ZDEMO_ABAP_BUS_CAT96* business catalog ID and the *ZABAP_DEMO_IAM96_EXT* IAM app ID are displayed.
        - Go back to the business catalog, and choose *Publish Locally* in the top right corner. It may take a while to complete.
        - On the _Overview_ tab, the status should be _Published_.
      - **Create a business role, add the created business catalog and assign your user** (to get more details on the general implementation steps, see this [tutorial](https://developers.sap.com/tutorials/abap-environment-access-mgmt.html)):
         - Log on to your system and acccess the SAP Fiori Launchpad as administrator.
         - Access the *Maintain Business Roles* app.
         - Choose *New* and create a business role with the name *ZDEMO_ABAP_BR96*.
         - Go to the *Business Catalogs* tab.
         - Choose *Add*, and search for *ZDEMO_ABAP_BUS_CAT96*.
         - Select it and choose *Ok* to add it.
         - Go to the *Business Users* tab.
         - Choose *Add*. For this example, select your business user and choose *Ok*.
         - The access categories on the _General Role Details_ tab are maintained as *Unrestricted*.
         - Save it. To quickly check the read access aspect of the example implementation (without UI and RAP context), see a code snippet in the expandable section.

2. **Run utility class** 
 
     - Open the class `zcl_demo_abap_rap_example_util` and run it, e.g. by making a _right-click -> Run as -> 2 ABAP Application (Console)_.
     - The class is set up to show output in the console. The class implementation includes the clearing of demo database tables and seeting of number range objects for those examples using late numbering and unmanaged internal early numbering. The latter is not relevant for this example.

3. **Publish service and start preview UI**
  
     - Open the service binding `ZUI_DEMO_ABAP_O296` from _Business Services -> Service Bindings_.
     - In the opened service binding, choose _Publish_ for _Local Service Endpoint_. 
     - Once the service was created - the _Local Service Endpoint_ will show _Published_, and the _Entity Set and Association_ section will show entries - activate the service binding.
     - To start the preview UI in a browser, select the _demo_abap_ entity from the _Entity Set and Association_ section and choose _Preview_. 

<br>

<details>
  <summary>🟢 Click to expand for more details on authorization-related steps of the example</summary>
  <!-- -->

<br>



> Important 
> - The following steps outline the setup of the example artifacts. This setup is intended only for exploring authorization in the demo RAP BO context. Always develop your own solution.
> - The demo entity includes a country field, of type `land1`, used for authorization in the demo. The selection of countries and the validity of authorizations are completely random. 



1. [Included in the repository] Creation of authorization objects and fields

   - In addition to separate artifacts for access control in the next step, a distinct authorization object, `ZAUTHOBB96`, and an authorization field, `ZAUTHFLB96`, are provided to explore create, update, and delete operations.
   - Refer to the Authorizations tab in the IAM app. `ACTVT` is flagged for create or generate, change, and delete, while the authorization field specifies a value range from DA to IZ. This is applied in the RAP BO implementation for both global and instance authorization. For global authorization, the `ACTVT` definition is relevant in this example. Once a user is assigned the necessary authorization, the authorization check permits the relevant operations. In the case of instance authorization, the authorization field is part of the check. If a created instance has a country value outside the specified range, operations are denied.

2. [Included in the repository] Implicit authorization checks using CDS access control for read accesses 
   
     - Access control is created for the base entity. Since this example also includes a projection view, a separate access control for this view is established.
     - The example is set up as follows: For read access, a distinct authorization object, `ZAUTHOBJ96`, is employed with the assigned authorization field, `ZAUTHFLD96`. Refer to the Authorizations tab in the IAM app to see that `ACTVT` is flagged for display. The authorization field specifies a value range from AA to RZ, meaning instances can only be read if their country value falls within this range.
     - After completing the preparation steps and assigning your user the relevant business role, you can quickly test the read functionality (without UI or RAP context). Create a demo class, insert the following code, and run it using F9. The expected output is provided below. Since a value range for the country field is defined, the read operation from the CDS view entity and projection entity should return only those entries within the range. Entries outside this range will not be returned. After executing the ABAP SQL `INSERT` statement, use F8 for the CDS view entity `zr_demo_abap96` to open the data preview. Similar to the console output (with differing UUID values), not all demo entries are displayed. This contrasts with the database table preview of `zdemoabap96`, where all entries are shown.



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

    DELETE FROM zdemoabap96.

    INSERT zdemoabap96 FROM TABLE @( VALUE #( ( uuid = xco_cp=>uuid( )->value text1 = 'abc' text2 = 'def' num1 = 1 num2 = 2 country = 'AT' )
                                              ( uuid = xco_cp=>uuid( )->value text1 = 'ghi' text2 = 'jkl' num1 = 3 num2 = 4 country = 'CH' )
                                              ( uuid = xco_cp=>uuid( )->value text1 = 'mno' text2 = 'pqr' num1 = 5 num2 = 6 country = 'DE' )
                                              ( uuid = xco_cp=>uuid( )->value text1 = 'stu' text2 = 'vwx' num1 = 7 num2 = 8 country = 'PT' )
                                              ( uuid = xco_cp=>uuid( )->value text1 = 'yz1' text2 = '234' num1 = 9 num2 = 10 country = 'SE' )
                                              ( uuid = xco_cp=>uuid( )->value text1 = '567' text2 = '890' num1 = 11 num2 = 12 country = 'SK' ) ) ).

    "Retrieving data from the database table
    SELECT * FROM zdemoabap96 INTO TABLE @DATA(db).

    out->write( db ).
    out->write( |\n| ).

    "Retrieving data from the CDS view entity
    SELECT * FROM zr_demo_abap96 INTO TABLE @DATA(cds_base).

    out->write( cds_base ).
    out->write( |\n| ).

    "Retrieving data from the CDS projection view
    SELECT * FROM zc_demo_abap96 INTO TABLE @DATA(cds_proj).

    out->write( cds_proj ).

  ENDMETHOD.
ENDCLASS.
```

Expected output (the CLIENT and UUID values below are only demo values):

```
CLIENT  UUID                              TEXT1  TEXT2  NUM1  NUM2  COUNTRY  
100     5AFBDBC3C47A1FE1829BFAB253EE9CFD  abc    def    1     2     AT       
100     5AFBDBC3C47A1FE1829BFAB253EEBCFD  ghi    jkl    3     4     CH       
100     5AFBDBC3C47A1FE1829BFAB253EEDCFD  mno    pqr    5     6     DE       
100     5AFBDBC3C47A1FE1829BFAB253EEFCFD  stu    vwx    7     8     PT       
100     5AFBDBC3C47A1FE1829BFAB253EF1CFD  yz1    234    9     10    SE       
100     5AFBDBC3C47A1FE1829BFAB253EF3CFD  567    890    11    12    SK       

UUID                              TEXT1  TEXT2  NUM1  NUM2  COUNTRY  
5AFBDBC3C47A1FE1829BFAB253EE9CFD  abc    def    1     2     AT       
5AFBDBC3C47A1FE1829BFAB253EEBCFD  ghi    jkl    3     4     CH       
5AFBDBC3C47A1FE1829BFAB253EEDCFD  mno    pqr    5     6     DE       
5AFBDBC3C47A1FE1829BFAB253EEFCFD  stu    vwx    7     8     PT       

UUID                              TEXT1  TEXT2  NUM1  NUM2  COUNTRY  
5AFBDBC3C47A1FE1829BFAB253EE9CFD  abc    def    1     2     AT       
5AFBDBC3C47A1FE1829BFAB253EEBCFD  ghi    jkl    3     4     CH       
5AFBDBC3C47A1FE1829BFAB253EEDCFD  mno    pqr    5     6     DE       
5AFBDBC3C47A1FE1829BFAB253EEFCFD  stu    vwx    7     8     PT           
```



</details>  


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example Exploration Steps  


> [!IMPORTANT]  
> You have walked through all preparation steps - in particular, the steps regarding the authorization setup - and opened the SAP Fiori preview UI in a browser.

- CUD operations
  - You can explore CUD operations by selecting the _Create_, _Edit_ (on the instance detail screen), and _Delete_ buttons. Note that global and instance authorization affects the behavior. A dummy implementation is available if you choose not to follow the preparation steps for authorization.
  
- Global authorization
  - After your user is assigned the business role, you can perform CUD operations unless restricted by instance authorization for updating and deleting.
  - In the simplified, non-semantic example, the country field is automatically set when creating an instance. This value serves as the basis for instance authorization and determines read access in the example.

- Instance authorization
  - You can explore this through update and delete operations. Refer to the demo implementation in the `det_save` method. Note that, when the example was created, the choice of the country and its impact in the code is entirely random.
  - The example instance authorization allows only instances with country values within a specific range to be updated or deleted. Others cannot be modified or removed.
  - Create instances that trigger the assignment of specific country field values inside and outside this range. Create instances with these num1 values:
    - 5
    - 10
    - 35
    - 60
  - These instances should appear in the instance list.
  - Select one of the instances with num1 = 5 or 10. The _Delete_ button should remain deactivated, indicating that deletion for this instance is denied.
  - When you open these instances with values num1 = 5 or 10, neither the _Edit_ nor _Delete_ button should be available, indicating that updates and deletions are not possible for these instances.
  - For the other demo instances with num1 = 35 or 60, both update and delete operations should be enabled.

- Read access
  - DCL access control specifies that instances with country values outside a particular range will not be displayed.
  - Create an instance with a num1 field value of 99. In the non-semantic demo implementation, the country field is filled with a value outside the specified range.
  - Consequently, after creating the instance, it should not appear in the list because read access is denied.
  - To verify creation, open the database table `zdemoabap96` and view the preview with F8; the entry should be accessible.

- Raising local RAP business events
  - The managed RAP BO is defined with `with additional save` that requires implementing the `save_modified` method in the RAP saver class, where demo events are triggered.
  - Events are managed in the separate event handler class `zcl_demo_abap_evt_handler96`.
  - The local RAP business events are visualized in the demo as follows:
    - Each time an instance is created, updated, or deleted, an event is raised.
    - When events are raised, the example logs an entry in a database table.
    - To check the raised events directly via the UI, the demo RAP BO provides a static action reflected by the _Check Log Table_ button. Choosing this button retrieves entries from the log database table, displaying them in a popup with details about when they were raised, the corresponding UUID (partially displayed), and the event type (noted as _CREATED_, _UPDATED_, or _DELETED_).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## *95: Managed RAP BO / Unmanaged Internal Early Numbering / Composition / Functions

### Example Context
- Non-semantic, simplified context; field values consist of demo integers and strings 
- Primary focus topics of the example: 
  - Managed RAP BO
  - Composition relationship of a root entity and one child entity
  - Unmanaged internal early numbering for the root entitiy (requires the implementation of the `early_numbering_create` handler method); the child entity uses managed numbering
  - Functions  

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Involved Artifacts

- All involved artifacts have the suffix 95
- Data modeling: 
  - Two database tables for the root (`zrootentity95`) and child (`zchildentity95`) entity
  - View entities for the two tables (`ZR_ROOTENTITY95`, `ZR_CHILDENTITY95`)
  - CDS metadata extensions: `ZR_ROOTENTITY95`, `ZR_CHILDENTITY95`
  - Note that the example BO is set up without projection layer to keep the number of artifacts low.
- Behavior
  - BDEF: `ZR_ROOTENTITY95`
  - ABP: `zbp_r_rootentity95`
- Service
  - Service definition: `ZUI_DEMO_ABAP_O295`
  - Service binding: `ZUI_DEMO_ABAP_O295`
- Further artifact
  - Number range object: `ZNR_DEMO95`
  - CDS abstract entities: `ZDEMO_ABAP_ABSTRACT95`, `ZDEMO_ABAP_ABSTR95`

<p align="right"><a href="#top">⬆️ back to top</a></p>

### BDEF and Implementation Notes

- Managed, non-draft-enabled RAP BO.
- In addition to the behavior defined for the root entity, behavior is also defined for the child entity. The root entity definition enables create-by-association operations.
- Authorization master is set to none for demonstration purposes (every user can access and modify).
- Numbering scenarios: The root entity defines unmanaged internal early numbering (which requires implementing the `early_numbering_create` handler method), while the child entity defines managed internal early numbering using the `numbering : managed` field specification.
- CUD and create-by-association operations are enabled for the root entity; the child entity allows update and delete operations.
- A static factory action is designed to create random root and child instances. 
- An action is intended to execute functions. These functions perform demo calculations in the example.
- Mapping relationship


> [!NOTE]  
> See comments in the implementation class for more information on the demo implementations.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Preparation Steps

1. **Check number range object**
  
     - The demo RAP BO uses unmanaged internal early numbering as numbering concept. For that purpose, a demo number range object is included in the repository. 
     - Open the `ZNR_DEMO95` number range object from _Number Range Management_.
     - Make sure that the following settings are applied: 
       - Number Length Domain: `ZDEMO_ABAP_DO95`
       - Percent Warning: 10
       - Rolling: Is selected
       - Buffering: Main Memory Buffering
       - Buffered Numbers: 1

2. **Run utility class** 

     - Open the class `zcl_demo_abap_rap_example_util` and run it, e.g. by making a _right-click -> Run as -> 2 ABAP Application (Console)_.
     - The class is set up to show output in the console. The class implementation includes the clearing of demo database tables and seeting of number range objects for those examples using late numbering and unmanaged internal early numbering. The latter applies to this example RAP BO.

3. **Publish service and start preview UI**

     - Open the service binding `ZUI_DEMO_ABAP_O295` from _Business Services -> Service Bindings_.
     - In the opened service binding, choose _Publish_ for _Local Service Endpoint_. 
     - Once the service was created - the _Local Service Endpoint_ will show _Published_, and the _Entity Set and Association_ section will show entries - activate the service binding.
     - To start the preview UI in a browser, select the _RootEntity_ entity from the _Entity Set and Association_ section and choose _Preview_. Note that the other preview options are not relevant for the example.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example Exploration Steps  

> [!IMPORTANT]  
> You have walked through all preparation steps and opened the SAP Fiori preview UI in a browser.

- CUD operations / unmanaged internal early numbering
  - You can explore CUD operations by selecting the _Create_, _Edit_ (on the instance detail screen), and _Delete_ buttons.
  - To explore unmanaged internal early numbering, create an instance by choosing the _Create_ button.
  - On the _New Object_ screen, the ID field will not show an entry initially. You can enter values for text and numeric fields, particularly for the numeric fields Num1 and Num2.
  - Choose _Create_ at the bottom. The ID field will be assigned a value. If you start fresh, ID 1 will be created (ID is defined as type `numc` with a length of 15). Depending on your subsequent actions (for example, if you create or delete instances), new instances will automatically receive the next available number, ensuring unique keys.
  - After creating an instance for the root entity, you can create child entities by choosing the _Create_ button in the _ChildEntity Items_ section. The child entity, which is in a composition relationship, specifies internal managed numbering, allowing for automatic assignment of the UUID key field.

- Static factory action (_Create New Instances_ button)
  - A static factory action is implemented for convenience, allowing you to automatically create instances for both the root and child entities (using a deep create).
  - Create new instances by choosing the _Create New Instances_ button.
  - Since this action has an input parameter defined in the BDEF, a popup will appear. Enter a number from 1 - 10 to specify how many child instances to create.
  - In the demo implementation, the text and numeric field values will be automatically and randomly set.

- Functions (_Execute Functions_ button)
  - Select an instance from the list and choose the _Execute Functions_ button.
  - The action `execute_functions` is included in the demo to perform various functions. Choosing the button triggers the execution of these functions.
  - Two functions are included that perform demo calculations:
    - `calc_inst`: This function sums the numeric values of the num1 and num2 fields based on the requested keys and returns the result.
    - `calc_stat`: This static function retrieves all persisted instances from the underlying database table (for the root entity). It sums the values in the num1 and num2 fields for each instance and counts how many instances have a calculated value greater than 100, returning this information in the result.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## ⚡ Known Issues

If the mass activation fails after importing the repository, and certain artifacts are left inactive, try to mass activate again. Try to use a selective, step-by-step activation approach in the inactive objects popup with these artifacts in the following order:  
- Select the data modeling-related artifacts (database tables, CDS views) and activate them first.
- Then, classes, BDEFs, and further artifacts other than service definitions and bindings.  
- Service definitions and bindings  

<br>

## ⚠️ Disclaimer
The code examples presented in this repository are only syntax examples and are not intended for direct use in a production system environment. The code examples are primarily intended to provide a better explanation and visualization of the syntax and semantics of ABAP statements and not to solve concrete programming tasks. For production application programs, a dedicated solution should therefore always be worked out for each individual case.
There is no guarantee for either the correctness or the completeness of the code. In addition, there is no legal responsibility or liability for possible errors or their consequences, which occur through the use of the example code.

<br>

## 📟 Support and Contribution
This is not intended to be a contribution repository, so please do not create pull requests. If you like to address issues or suggestions, please create an issue. However, this project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

<br>

## 📜 License
Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.