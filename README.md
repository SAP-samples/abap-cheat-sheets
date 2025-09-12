<br />
<div align="center">
  <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap.htm">
    <img src="./files/ABAP_Keyword_Documentation.png" alt="ABAP Keyword Documentation" >
  </a>

  <h3 align="center" style="font-size: 40px; color: #FCB913;">ABAP Cheat Sheets</h3>

  <p align="center">
    Explore ABAP syntax in a nutshell & executable examples
    <br />
    <!--<a href=""><strong>Expore ABAP syntax in a nutshell & executable examples</strong></a>
    <br />-->
    <br />
    <a href="#%EF%B8%8F-how-to-use">How to Use</a>
    ·
    <a href="#-abap-cheat-sheets-overview">Cheat Sheets</a>
    ·
    <a href="#-getting-started-with-the-examples">Examples</a>
  </p>
</div>
<br>
<hr>
<br>

[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-cheat-sheets)](https://api.reuse.software/info/github.com/SAP-samples/abap-cheat-sheets)

ABAP cheat sheets[^1] ...
- provide a **collection of information on selected ABAP topics** in a nutshell for your reference.
- focus on **ABAP syntax** in the restricted ABAP language version [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm), particularly in the [SAP BTP ABAP Environment](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensap_btp_abap_env_glosry.htm).
- include **code snippets**.
- are supported by easy-to-consume **demonstration examples** that you can import into your system using [abapGit](https://abapgit.org/) to run and check out ABAP syntax in action in simple contexts:
  | Environment    | Branch | ABAP language version |
  | -------- | ------- | ------- |
  | [SAP BTP ABAP Environment](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensap_btp_abap_env_glosry.htm)  | *main* branch | [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm) |
  | System that supports [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm)  | The repository branches other than *main*. Check the available ABAP releases. |[Standard ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_abap_glosry.htm)  |

- are enriched by links to glossary entries and chapters of the **ABAP Keyword Documentation** (the *F1 help*) and more for you to deep dive into the respective ABAP topics and get more comprehensive information.

<br>

> [!IMPORTANT] 
> - Unless otherwise stated in the cheat sheets, the content of this repository is relevant for these ABAP language versions, with a focus on ABAP for Cloud Development, particularly in the SAP BTP ABAP Environment ⚠️:
>   - <b>[ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_for_sap_cloud_glosry.htm)</b> <br>Restricted ABAP language scope for [ABAP Cloud](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_cloud_glosry.htm) → [Online version of the ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm)
>   - <b>[Standard ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_abap_glosry.htm)</b> <br>Unrestricted ABAP language scope, for example, for [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm) → [Online version of the ABAP Keyword Documentation (latest version)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap.htm)
> - The ABAP cheat sheet documents and examples mainly highlight and explore ABAP syntax options. Most executable examples, code snippets, names of data objects, classes, methods, and interfaces are non-semantic. The code examples do not claim to illustrate best practices. They are simply meant to illustrate ABAP statements and additions to give an idea of their functionality.

<br>

> [!NOTE]  
> - Since the ABAP cheat sheets provide information in a nutshell, they are not fully comprehensive as far as the described syntax and concepts are concerned. If you need more details, you can always consult the ABAP Keyword Documentation, for example, by choosing *F1* on a keyword in your code, or by searching directly using the online or the system-internal version.
>- Some of the embedded code snippets in the cheat sheets only display high-level code patterns, while others are fully functional and can be directly copied into an ABAP test program for exploration.
>- Check the [Known Issues](#-known-issues) and [Disclaimer](#%EF%B8%8F-disclaimer).
>- The cheat sheets provide links to glossary entries and topics in the ABAP Keyword Documentation. Note that unlike the classic ABAP-only cheat sheets, in most cases these links refer to ABAP for Cloud Development.
>- [Here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrestricted_abap_elements.htm) is an overview of the different ABAP language elements in the different ABAP versions, i.e. what is allowed in ABAP for Cloud Development and what is not. See also the released APIs [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreleased_apis.htm).
>- In order to have all ABAP cheat sheet documents in one place, the *main* branch (for examples to be imported into the SAP BTP ABAP environment) also contains the ABAP cheat sheet documents that are only relevant for [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm). 
>- The example classes contained in the branches for classic ABAP mostly use syntax that is also available in ABAP for Cloud Development. Only the  `TEST_ABAP_CHEAT_SHEETS_CLASSIC` subpackage contains syntax relevant to Standard ABAP and that is not available in ABAP for Cloud Development, such as dynpro-related ABAP keywords.
>- The code snippets in the ABAP cheat sheet documents and the executable examples include many comments. While it is generally not recommended to overuse comments in your code, they are used here to explain and provide context directly with ABAP statements. In many cases, they illustrate the results of ABAP statements.
>- Many ABAP statements allow additions in various orders, and these orders are not always fixed.

</details>

<br>

## 🏗️ How to Use

1. **ABAP syntax info**: Get info in a nutshell on ABAP syntax and concepts related to various ABAP topics in the [ABAP cheat sheets](#-abap-cheat-sheets-overview).
2. **Demo examples**: Import the ABAP development objects of this repository (Note: *main* branch for the SAP BTP ABAP environment only) into your system using [abapGit](https://abapgit.org/) as described [here](#-getting-started-with-the-examples) and run the demo classes by choosing *F9* in the [ABAP development tools for Eclipse (ADT)](https://tools.eu1.hana.ondemand.com/) for checking out the ABAP syntax in action.

<br>

## 📝 ABAP Cheat Sheets Overview

| Cheat Sheet        | Topics Covered           |  Demo Example  |
| ------------- | ------------- | ----- |
|[ABAP for Cloud Development](19_ABAP_for_Cloud_Development.md)| Briefly outlines the terms ABAP Cloud and classic ABAP to set the context for ABAP for Cloud Development | [zcl_demo_abap_cloud_excursion](./src/zcl_demo_abap_cloud_excursion.clas.abap) (see the notes in the cheat sheet) |
|[Data Types and Data Objects](16_Data_Types_and_Objects.md)| Contains basic information about data types and data objects in ABAP  | [zcl_demo_abap_dtype_dobj](./src/zcl_demo_abap_dtype_dobj.clas.abap)   |
|[Internal Tables](01_Internal_Tables.md)| Creating, filling, reading from, sorting, modifying internal tables  | [zcl_demo_abap_internal_tables](./src/zcl_demo_abap_internal_tables.clas.abap)   |
|[Structures](02_Structures.md)| Some basics when working with structures  |  [zcl_demo_abap_structures](./src/zcl_demo_abap_structures.clas.abap)  |
|[ABAP SQL](03_ABAP_SQL.md)| Reading from database tables using `SELECT`, changing data in database tables using `INSERT`, `UPDATE`, `MODIFY` and `DELETE`    | [zcl_demo_abap_sql](./src/zcl_demo_abap_sql.clas.abap)   |
|[ABAP Object Orientation](04_ABAP_Object_Orientation.md)| Working with objects and components, concepts such as inheritance, interfaces, and more |<ul><li>[zcl_demo_abap_objects](./src/zcl_demo_abap_objects.clas.abap)</li><li>[zcl_demo_abap_objects_misc](./src/zcl_demo_abap_objects_misc.clas.abap)</li><li>[zcl_demo_abap_oo_inheritance_1](./src/zcl_demo_abap_oo_inheritance_1.clas.abap)</li></ul> |
|[Constructor Expressions](05_Constructor_Expressions.md)| Covers constructor expressions with operators such as `VALUE`, `CORRESPONDING`, `NEW`, `CONV`, `EXACT`, `REF`, `CAST`, `COND`, `SWITCH`, `FILTER`, `REDUCE`, iteration expressions with `FOR`, `LET` expressions  | [zcl_demo_abap_constructor_expr](./src/zcl_demo_abap_constructor_expr.clas.abap) |
|[Dynamic Programming](06_Dynamic_Programming.md)| Covers field symbols and data references as supporting elements for dynamic programming, dynamic ABAP syntax components, runtime type services (RTTS), i. e. runtime type identification (RTTI) and runtime type creation (RTTC) |  [zcl_demo_abap_dynamic_prog](./src/zcl_demo_abap_dynamic_prog.clas.abap)  |
|[String Processing](07_String_Processing.md)| Creating strings and assigning values, chaining strings, string templates, concatenating, splitting, modifying strings, searching and replacing |  [zcl_demo_abap_string_proc](./src/zcl_demo_abap_string_proc.clas.abap)  |
|[ABAP for RAP: Entity Manipulation Language (ABAP EML)](08_EML_ABAP_for_RAP.md)| Setting EML in the context of RAP, standard (create, read, update, delete) and non-standard operations (actions) | <ul><li>[Demo RAP scenario with a managed RAP BO, external numbering (zcl_demo_abap_rap_ext_num_m)](./src/zcl_demo_abap_rap_ext_num_m.clas.abap)</li><br><li>[Demo RAP scenario with an unmanaged RAP BO, external numbering (zcl_demo_abap_rap_ext_num_u)](./src/zcl_demo_abap_rap_ext_num_u.clas.abap)</li><br><li>[Demo RAP scenario ("RAP calculator") with a managed, draft-enabled RAP BO,  late numbering (zcl_demo_abap_rap_draft_ln_m)](./src/zcl_demo_abap_rap_draft_ln_m.clas.abap) <br>Note that this example can also be checked out using the preview version of an SAP Fiori UI. Check the comments in the class for the steps.</li><br><li>[Demonstrating the local consumption of RAP business events in the context of a RAP demo scenario, managed RAP BO with managed internal numbering and additional save (zcl_demo_abap_rap_m_as)](./src/zcl_demo_abap_rap_m_as.clas.abap)</li></ul>  |
|[Excursion Down to Bits and Bytes](09_Bits_and_Bytes.md)|Covers the technical background of data types and data objects|-|
|[ABAP SQL: Working with Hierarchies](10_ABAP_SQL_Hierarchies.md)|Summarizes the functions ABAP SQL offers together with ABAP CDS for working with hierarchical data that is stored in database tables|-|
|[Internal Tables: Grouping](11_Internal_Tables_Grouping.md)|Covers the `GROUP BY` clause in statements for internal tables.|[zcl_demo_abap_sql_group_by](./src/zcl_demo_abap_sql_group_by.clas.abap)|
|[ABAP Managed Database Procedures (AMDP)](12_AMDP.md)|Covers ABAP Managed Database Procedures (AMDP): AMDP Procedures and AMDP Functions (including CDS Table Functions)|[zcl_demo_abap_amdp](./src/zcl_demo_abap_amdp.clas.abap)|
|[Program Flow Logic](13_Program_Flow_Logic.md)|Deals with control structures (`IF`, `CASE`), loops (`DO`, `WHILE`)|[zcl_demo_abap_prog_flow_logic](./src/zcl_demo_abap_prog_flow_logic.clas.abap)|
|[ABAP Unit Tests](14_ABAP_Unit_Tests.md)|Contains basic information about unit testing in ABAP|[zcl_demo_abap_unit_test](./src/zcl_demo_abap_unit_test.clas.abap)|
|[CDS View Entities](15_CDS_View_Entities.md)|The cheat sheet provides references to information on ABAP CDS. Find a feature table for available language elements in ABAP CDS in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abencds_language_elements.html) with links to detailed topics. The focus here is on the example CDS artifacts and the [executable example class](./src/zcl_demo_abap_cds_ve.clas.abap), which include comments.|[zcl_demo_abap_cds_ve](./src/zcl_demo_abap_cds_ve.clas.abap)|
|[SAP LUW](17_SAP_LUW.md)|Provides a high-level overview of the [SAP LUW](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensap_luw_glosry.htm) concept that deals with data consistency with a focus on SAP LUW-related statements <br> 💡 Several statements covered in the cheat sheet and the executable example are only relevant to [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm).|Program `ZDEMO_ABAP_SAP_LUW`|
|[Dynpro](18_Dynpro.md)|Provides a high-level overview of dynpro topics with a focus on dynpro-related statements <br> 💡 The content of this cheat sheet and the executable example are only relevant to [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm).|Program `ZDEMO_ABAP_DYNPRO`|
|[Selection Screens and Classic Lists](20_Selection_Screens_Lists.md)|Provides a high-level overview of selection screens and classic lists with a focus on related statements. It includes an excursion into the SAP List Viewer (ALV). <br> 💡 The content of this cheat sheet and the executable examples are only relevant to [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm).|Program `ZDEMO_ABAP_SELSCR_LISTS_INTRO` (the "intro" program, from which the other related example programs can be started)|
|[Working with XML and JSON in ABAP](21_XML_JSON.md)|Covers processing XML using class libraries, XML transformations using XSLT and Simple Transformations (ST), serializations (ABAP to XML) and deserializations (XML to ABAP), dealing with JSON data|[zcl_demo_abap_xml_json](./src/zcl_demo_abap_xml_json.clas.abap)|
|[Released ABAP Classes](22_Released_ABAP_Classes.md)|Contains a selection of ABAP classes, serving as a quick introduction, along with code snippets to explore the functionality in action|- (The cheat sheet includes copy and paste code snippets and example classes)|
|[Date, Time, and Time Stamp](23_Date_and_Time.md)|Covers how to handle and process dates, times, and time stamps in ABAP|[zcl_demo_abap_date_time](./src/zcl_demo_abap_date_time.clas.abap)|
|[Built-In Functions](24_Builtin_Functions.md)|Covers a variety of built-in functions in ABAP|[zcl_demo_abap_builtin_func](./src/zcl_demo_abap_builtin_func.clas.abap)|
|[Authorization Checks](25_Authorization_Checks.md)|Provides a high-level overview of explicit and implicit authorization checks in ABAP|- (The cheat sheet includes a copy and paste example class)|
|[ABAP Dictionary](26_ABAP_Dictionary.md)|Covers a selection of repository objects in the ABAP Dictionary (DDIC) that represent global types|- (The cheat sheet includes a copy and paste example class)|
|[Exceptions and Runtime Errors](27_Exceptions.md)|Provides an overview on exceptions and runtime errors|[zcl_demo_abap_error_handling](./src/zcl_demo_abap_error_handling.clas.abap)|
|[Regular Expressions in ABAP](28_Regular_Expressions.md)|Includes an overview of common regular expressions and their use in ABAP through statements, built-in functions, and system classes|[zcl_demo_abap_regex](./src/zcl_demo_abap_regex.clas.abap)|
|[Numeric Operations in ABAP](29_Numeric_Operations.md)|Explores various aspects of numeric operations and calculations in ABAP|- (The cheat sheet includes a copy and paste example class)|
|[Generative AI](30_Generative_AI.md)|Provides references to detailed information on *Generative AI in ABAP Cloud* and explores released ABAP classes available in the *ABAP AI SDK powered by Intelligent Scenario Lifecycle Management*|- (The cheat sheet includes a copy and paste example class)|
|[WHERE Conditions](31_WHERE_Conditions.md)|Explores syntax options in ABAP statements that include `WHERE` for data filtering|- (The cheat sheet includes copy and paste example classes)|
|[Performance Notes](32_Performance_Notes.md)|Explores a selection of performance-related examples, aimed to illustrate potentially inefficient techniques and use of statements|- (The cheat sheet includes a copy and paste example class)|
|[ABAP Release News](33_ABAP_Release_News.md)|Summarizes the release news from the ABAP Keyword Documentation for both versions of the ABAP language, ABAP for Cloud Development and Standard ABAP |-|

<br>

## 🎬 Getting Started with the Examples

The main focus of the ABAP cheat sheets is ABAP for Cloud Development. The examples in the *main* branch of the repository are designed to be imported into the SAP BTP ABAP environment.
For Standard ABAP, you can find examples in the other branches of the repository (note that except for specific examples, the example code there uses syntax that is also availabe in ABAP for Cloud Development) that you can import into your sandbox SAP system. Just select the appropriate version (*v757* stands for ABAP version 7.57). Check the information in the following collapsible sections for your system environment and perform the required steps.

<details>
  <summary>1) General info</summary>
  <br>

- Some **DDIC artifacts**, such as database tables, are part of the repository. They are used by the examples to ensure self-contained examples. All artifacts must be imported for all examples to work.
- Most examples are designed to **display some output in the ADT console**. Once successfully imported, you can **run** the examples in ADT by choosing *F9* to display the output in the ADT console. The programs included in the branches for classic ABAP can be executed with *F8*.
- The examples **include descriptions and comments** in the code to provide explanations and set the context.
- Note that only one user on the system can import this repository because all object names must be globally unique. 
- Regarding the examples to be imported into a system supporting classic ABAP (where Standard ABAP is supported), note the following: In most cases, the cheat sheet documents and examples focus on ABAP for Cloud Development. Therefore, the lower the [ABAP release](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_release_glosry.htm) in your system, the fewer syntax options and examples are available. For example, the RAP examples need at least ABAP version 7.56. Or, the [`FINAL` declaration operator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm) is not available in ABAP version 7.56. The code examples in the classic ABAP branches do not necessarily reflect all (described) syntax variations and options that are available in classic ABAP and in the particular ABAP release. For more information on new ABAP features by release, see the ABAP Release News:
  - [ABAP Release News](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennews.htm) for ABAP for Cloud Development. Note the ABAP release cycles listed there.
  - For Standard ABAP, see the ABAP relese news [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews.htm).
</details>

<details>
  <summary>2a) SAP BTP ABAP environment</summary>
  <br>

**Prerequisites**
- [x] You have access to an SAP BTP ABAP Environment instance (see [here](https://blogs.sap.com/2018/09/04/sap-cloud-platform-abap-environment) for additional information).
- [x] You have downloaded and installed the ABAP development tools for Eclipse (ADT). Make sure that you are using the latest version, as indicated on the [installation page](https://tools.hana.ondemand.com/#abap).
- [x] Before importing the code, you have performed a system-wide search for classes named `ZCL_DEMO_ABAP*`, for example, so that you do not run into errors when you try to import the code. If someone has already imported the content into the system, you can simply check out that imported version and proceed to the step *3) Run the code*.
- [x] You have created an ABAP cloud project in ADT that allows you to access your SAP BTP ABAP Environment instance (see [here](https://help.sap.com/viewer/5371047f1273405bb46725a417f95433/Cloud/en-US/99cc54393e4c4e77a5b7f05567d4d14c.html) for more information). Your login language is English.
- [x] You have installed the [abapGit](https://github.com/abapGit/eclipse.abapgit.org) plug-in for ADT from the [update site](http://eclipse.abapgit.org/updatesite/).


**Import Code**

Use the abapGit plug-in to install the <em>ABAP cheat sheets</em> by carrying out the following steps:

1. In your ABAP cloud project, create a package, for example, *ZABAP_CHEAT_SHEETS* as the target package. It is recommended that you assign the package to a transport request that is suitable for demo content.
2. Add the package to the *Favorite Packages* in the *Project Explorer* view in ADT.
3. To add the <em>abapGit Repositories</em> view to the <em>ABAP</em> perspective, choose *Window* → *Show View* → *Other...* from the menu bar and choose *abapGit Repositories*.
4. In the <em>abapGit Repositories</em> view, click the `+` icon in the upper right corner of the ADT tab to link a new abapGit repository.
  <br>![ADT](./files/abapGit_Repositories.png)

5. The *Link abapGit Repository* popup appears. Enter the following URL:

```
https://github.com/SAP-samples/abap-cheat-sheets.git
```

6. Choose *Next*.

7. On the *Branch and Package Selection* screen, enter the name of the created package (for example, *ZABAP_CHEAT_SHEETS*) in the *Package* field.
8. Choose *Next*.
9.  On the *Select Transport Request* screen, select the created transport request that is suitable for the demo content and choose *Finish* to link the Git repository to your ABAP cloud project. If the created package is already assigned to a transport request for the demo content, and a message appears that an object is already locked in a transport request, choose *Finish*, too.
10. In the *abapGit Repositories* view, filter for your package. The repository appears in the *abapGit Repositories* view with the status <em>Linked</em>.
11. Right-click on the new abapGit repository and choose *Pull...* to start the cloning of the repository contents.
12. On the *Branch and Package Selection* screen, choose *Next*.
13. If the *Locally Modified Object* screen is displayed, select the objects (for example, the package to automatically select all artifacts) from the list and choose *Next*.
14. On the next screen, select a transport request and choose *Finish*. Same as above, if an *object already locked* message is displayed, choose *Finish* as well. The status in the *abapGit Repositories* view changes to <em>Pull running...</em>. Note that the pull run may take several minutes.
15. Once the cloning is complete, the status changes to *Pulled Successfully*. You may need to refresh the *abapGit Repositories* view to see the progress of the import. To do this, choose the  *Refresh* icon in the upper right corner of the view.
16. Refresh your project tree. For example, in ADT, right-click the package and choose *Refresh*. The package should contain all the artifacts from the GitHub repository.
17. Make sure that all artifacts are active. To activate all inactive development objects, choose the *Activate all inactive ABAP development objects* button from the menu (or choose *CTRL+Shift+F3*).
</details>

<details>
  <summary>2b) System supporting classic ABAP</summary>
<br>

**Prerequisites**
- [x] You are running an [ABAP release](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-75.htm) for which examples are available. See the different branches of the repository. For example, you can find out about your ABAP release by checking the value of `sy-saprl`:  
  ```abap
  DATA rel LIKE sy-saprl.
  rel = sy-saprl.
  BREAK-POINT.
  ```
- [x] Before importing the code, you have performed a system-wide search for classes named *ZCL_DEMO_ABAP**, for example, to avoid errors when you try to import the code. If someone has already imported the content into the system, you can simply check out that imported version and proceed to the step *3) Run the code*.
- [x] You have downloaded and installed the ABAP development tools for Eclipse (ADT). Make sure that you are using the latest version, as indicated on the [installation page](https://tools.hana.ondemand.com/#abap).
- [x] You have created an ABAP project in ADT that allows you to access your application server as mentioned above. Your login language is English.
- [x] You have downloaded and installed the standalone version of the abapGit report. Make sure you are using the latest version, as indicated on the [installation page](https://docs.abapgit.org/). You can create a report, for example, *zabapgit_standalone*, and copy and paste [this code](https://raw.githubusercontent.com/abapGit/build/main/zabapgit_standalone.prog.abap) into the program.

**Import Code**

Use the standalone version of the abapGit report to import the demo examples of the ABAP cheat sheets by performing the following steps:
1. In your ABAP project, create a package, such as *TEST_ABAP_CHEAT_SHEETS* as a target package suitable for demo content (for example, by using *LOCAL* as the software component).
2. Add the package to the *Favorite Packages* in the *Project Explorer* view in ADT.
3. Run the standalone version of the abapGit report.
4. Choose the *New Online* button. If the button is not available, for example, if another repository is already open, choose the *Repository List* button.
5. On the *New Online Repository* screen, make the following entries:
   - ***Git Repository URL***:

      ```
      https://github.com/SAP-samples/abap-cheat-sheets.git
      ```

   - ***Package***: Your demo package, for example, *TEST_ABAP_CHEAT_SHEETS*
   - ***Branch***: Choose the button with the 3 dots to the right of the input field. In the pop-up window, select the appropriate branch, e.g. *v757* if your ABAP release is 7.57, and choose the *Continue* (✔️) button. **Note**: The examples in the *main* branch are designed to be imported into the SAP BTP ABAP environment only. 
   - ***Folder Logic***: *Full*
6. Choose *Create Online Repo*.
7. The *Repository* screen displays the available ABAP artifacts to be imported into your ABAP system.
8. Choose the *Pull* button. The import of the artifacts is triggered. This may take a while.
9. If the *Inactive Objects* popup is displayed, select all artifacts and choose *Continue* (✔️).
10.	When the cloning is complete, refresh your project tree. For example, in ADT, right-click on the package and choose *Refresh*. The package should contain all artifacts from the GitHub repository.
11. Make sure that all artifacts are active. To activate all inactive development objects, choose the *Activate all inactive ABAP development objects* button from the menu (or choose *CTRL+Shift+F3*).

</details>

<details>
  <summary>3) Run the code</summary>
<br>

- Open the package you created containing the imported ABAP artifacts in the ABAP development tools for Eclipse (ADT).
- Classes: 
  - Open one of the ABAP cheat sheet example classes listed in the [ABAP Cheat Sheets Overview](#-abap-cheat-sheets-overview) section, for example, *zcl_demo_abap_string_proc*. The classes are located in the *Source Code Library* → *Classes* folder.
  - Choose *F9* to run the class. Alternatively, choose *Run* → *Run As* → *2 ABAP Application (Console)* from the menu. 
  - Check the console output.
    > [!NOTE] 
    >- Check the notes on the context and the ABAP syntax used that are included as comments in the class.
    >- Due to the amount of output in the console, the examples include numbers (e.g. 1) ..., 2) ..., 3) ...) that represent the headers of each example code section. Also, in most cases, the variable name is displayed in the console. Therefore, to find the relevant output in the console more easily and quickly, simply search the console for the number (e.g. search for *3)* for the particular output) or variable name (*CTRL+F* in the console), or use breakpoints in the code to check variables in the debugger.
    >- You may want to clear the console by right-clicking in the console and choosing *Clear* before running another demo class to avoid confusing the output of multiple classes.
- Programs:
  - The programs included in the repository can be executed with *F8* (or *Run* → *Run As* → *1 ABAP Application*). 


</details>

<br>

## ⚡ Known Issues
- Regarding possible code check warnings, e.g. for the many strings in the code, not using an `ORDER BY` clause, or messages regarding using `SELECT *`, the code deliberately avoids [pragmas](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpragma_glosry.htm) and [pseudo comments](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpseudo_comment_glosry.htm) in order to focus on the available ABAP syntax. See also the [Disclaimer](#%EF%B8%8F-disclaimer).
- Importing the `main` branch in a system other than the SAP BTP ABAP Environment may cause errors in various cases. ABAP keywords, additions, and syntax options may not yet be available in the ABAP release. You can check the ABAP Release News in the ABAP Keyword Documentation. Also note the *General Info* in the [Getting Started with the Examples](#-getting-started-with-the-examples) section.
- If you encounter import problems with the XSLT/ST objects, try to manually paste the code from the 3 `...source.xml` files ([zdemo_abap_st_carrhtml](./src/zdemo_abap_st_carrhtml.xslt.source.xml), [zdemo_abap_st_strhtml](./src/zdemo_abap_st_strhtml.xslt.source.xml), [zdemo_abap_xslt_fl](./src/zdemo_abap_xslt_fl.xslt.source.xml)) into the improperly imported objects and activate all inactive objects.

<br>

## ℹ️ More Information
- For the system-internal version of the ABAP Keyword Documentation in 
  - ... **ABAP Cloud**: In ADT, the documentation is in the *ABAP Language Help* view, where you can also search. If you choose `F1` on a keyword in your code, the documentation opens there.
  - ... **classic ABAP**: Access the documentation in the SAP GUI via the transactions `ABAPDOCU` (opens the documentation directly) and `ABAPHELP` (opens an input field with which you can search the documentation content, for example, you can search for a keyword such as `SELECT`). Or, of course, choose `F1` on a keyword in your code. If you are in the SAP GUI (e.g. in `SE80`), the system-internal version opens. If you are in ADT, the documentation opens in the *ABAP Language Help* view.
- Links to the online version of the ABAP Keyword Documentation for:
  - **Standard ABAP**: Unrestricted ABAP language scope for [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm):
    - [Latest version (i.e. version 7.58 currently)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap.htm)
    - [Version 7.58](https://help.sap.com/doc/abapdocu_758_index_htm/7.58/en-US/index.htm)
    - [Version 7.57](https://help.sap.com/doc/abapdocu_757_index_htm/7.57/en-US/index.htm)
    - [Version 7.56](https://help.sap.com/doc/abapdocu_756_index_htm/7.56/en-US/index.htm)
    - [Version 7.55](https://help.sap.com/doc/abapdocu_755_index_htm/7.55/en-US/index.htm)
    - [Version 7.54](https://help.sap.com/doc/abapdocu_754_index_htm/7.54/en-US/index.htm)    
    - [Version 7.53](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm)
    - [Version 7.52](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm)
    - [Version 7.51](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm)
    - [Version 7.50](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/index.htm)  
  - **ABAP for Cloud Development**: Restricted ABAP language scope for developments, for example, in the SAP BTP ABAP environment → [Online version of the documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm)
- For demonstration examples of the ABAP Keyword Documentation, see the `SABAPDEMOS` package. This package contains all the examples used in the ABAP Keyword Documentation. For the context, class names, etc., see the example page ([Cloud](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenabap_examples.html)/[Classic](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_examples.htm)), which is also available in the system-internal version as a node in the topic tree and which summarizes the executable examples. Of course, you can also find the example topics in the context of the individual topic of the ABAP keyword documentation. The example topics are marked with a ⚙️ icon:

  ![](./files/example_topics.png)

<br>

## ⚠️ Disclaimer
The code examples presented in this repository are only syntax examples and are not intended for direct use in a production system environment. The code examples are primarily intended to provide a better explanation and visualization of the syntax and semantics of ABAP statements and not to solve concrete programming tasks. For production application programs, a dedicated solution should therefore always be worked out for each individual case.
There is no guarantee for either the correctness or the completeness of the code. In addition, there is no legal responsibility or liability for possible errors or their consequences, which occur through the use of the example code.

<br>

## 📟 Support
This is not intended to be a contribution repository, so please do not create pull requests. If you like to address issues or suggestions regarding additional syntax to be covered, please create an issue. However, this project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

<br>

## 📜 License
Copyright (c) 2025 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.

[^1]: "A written [...] aid (such as a sheet of notes) that can be referred to for help in understanding or remembering something complex" (Definition for "cheat sheet" in Merriam-Webster Dictionary).
