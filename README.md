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
- focus on **ABAP syntax**.
- include **code snippets**.
- are supported by easy-to-consume **demonstration examples** that you can import into your [SAP BTP ABAP environment](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensap_btp_abap_env_glosry.htm) or on-premise ABAP system using [abapGit](https://abapgit.org/) to run and check out ABAP syntax in action in simple contexts.
- are enriched by links to glossary entries and chapters of the **ABAP Keyword Documentation** (the *F1 help*) for you to deep dive into the respective ABAP topics and get more comprehensive information.

<details>
<summary>💡 Note</summary>
<br>

- Since the ABAP cheat sheets provide information in a nutshell, they do not claim to be exhaustive as far as the described syntax and concepts are concerned. If you want to go more into the details, just consult the ABAP Keyword Documentation, for example, by choosing `F1` for a keyword in your code or directly researching via the online version or the system-internal version.
- If not stated otherwise in the cheat sheets and examples, the content of this repository is relevant for these ABAP language versions:
  - [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_for_sap_cloud_glosry.htm): Restricted ABAP language scope for developments in the [SAP BTP ABAP environment](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensap_btp_abap_env_glosry.htm) → [Online version of the documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm)
  - [Standard ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_abap_glosry.htm): Unrestricted ABAP language scope, for example, for developments in an on-premise ABAP system → [Online version of the documentation (latest version)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap.htm)
- Check the [Known Issues](#-known-issues) and [Disclaimer](#%EF%B8%8F-disclaimer).
- In the cheat sheets, links are available to glossary entries and topics of the ABAP Keyword Documentation. Note that these links refer to the ABAP for Cloud Development version in most cases.
</details>

<br>

## 🏗️ How to Use

1. **ABAP syntax info**: Get info in a nutshell on ABAP syntax and concepts related to various ABAP topics in the [ABAP cheat sheets](#-abap-cheat-sheets-overview).
2. **Demo examples**: Import the ABAP development objects of this repository into your system using [abapGit](https://abapgit.org/) as described [here](#-getting-started-with-the-examples) and run the demo classes by choosing `F9` in the [ABAP Development Tools (ADT)](https://tools.eu1.hana.ondemand.com/) for checking out the ABAP syntax in action.


## 📝 ABAP Cheat Sheets Overview

| Cheat Sheet        | Topics Covered           |  Demo Example  |
| ------------- | ------------- | ----- |
|[Working with Internal Tables](01_Internal_Tables.md)| Creating, filling, reading from, sorting, modifying internal tables  | [zcl_demo_abap_internal_tables](./src/zcl_demo_abap_internal_tables.clas.abap)   |
|[Working with Structures](02_Structures.md)| Creating structures and structured types, variants of structures, accessing components of structures, filling structures, clearing structures, structures in use in the context of tables   |  [zcl_demo_abap_structures](./src/zcl_demo_abap_structures.clas.abap)  |
|[ABAP SQL: Working with Persisted Data in Database Tables](03_ABAP_SQL.md)| Reading from database tables using `SELECT`, changing data in database tables using `INSERT`, `UPDATE`, `MODIFY` and `DELETE`    | [zcl_demo_abap_sql](./src/zcl_demo_abap_sql.clas.abap)   |
|[ABAP Object Orientation](04_ABAP_Object_Orientation.md)| Working with objects and components, concepts like inheritance, interfaces, and more | [zcl_demo_abap_objects](./src/zcl_demo_abap_objects.clas.abap) |
|[Working with Constructor Expressions](05_Constructor_Expressions.md)| Operators `VALUE`, `CORRESPONDING`, `NEW`, `CONV`, `EXACT`, `REF`, `CAST`, `COND`, `SWITCH`, `FILTER`, `REDUCE`, iteration expressions with `FOR`, `LET` expressions  | [zcl_demo_abap_constructor_expr](./src/zcl_demo_abap_constructor_expr.clas.abap) |
|[Dynamic Programming](06_Dynamic_Programming.md)| Touches on field symbols and data references as supporting elements for dynamic programming, dynamic ABAP syntax components, runtime type services (RTTS), i. e. runtime type identification (RTTI) and runtime type creation (RTTC) |  [zcl_demo_abap_dynamic_prog](./src/zcl_demo_abap_dynamic_prog.clas.abap)  |
|[String Processing](07_String_Processing.md)| Creating strings and assigning values, chaining strings, string templates, concatenating, splitting, modifying strings, searching and replacing, regular expressions  |  [zcl_demo_abap_string_proc](./src/zcl_demo_abap_string_proc.clas.abap)  |
|[ABAP for RAP: Entity Manipulation Language (ABAP EML)](08_EML_ABAP_for_RAP.md)| Setting EML in the context of RAP, standard (create, read, update, delete) and non-standard operations (actions) | <ul><li>[Demo RAP scenario with a managed RAP BO, external numbering (zcl_demo_abap_rap_ext_num_m)](./src/zcl_demo_abap_rap_ext_num_m.clas.abap)</li><br><li>[Demo RAP scenario with an unmanaged RAP BO, external numbering (zcl_demo_abap_rap_ext_num_u)](./src/zcl_demo_abap_rap_ext_num_u.clas.abap)</li><br><li>[Demo RAP scenario ("RAP calculator") with a managed, draft-enabled RAP BO,  late numbering (zcl_demo_abap_rap_draft_ln_m)](./src/zcl_demo_abap_rap_draft_ln_m.clas.abap) <br>Note that this example can also be checked out using the preview version of an SAP Fiori UI. Check the comments in the class for the steps.</li></ul>  |
|[Excursion Down to Bits and Bytes](09_Bits_and_Bytes.md)|Touches on the technical background of data types and data objects|-|
|[ABAP SQL: Working with Hierarchies](10_ABAP_SQL_Hierarchies.md)|Summarizes the functions ABAP SQL offers together with ABAP CDS for working with hierarchical data that is stored in database tables|-|
|[ABAP SQL: Grouping Internal Tables](11_ABAP_SQL_Grouping_Internal_Tables.md)|Touches on the `GROUP BY` clause in ABAP SQL|[zcl_demo_abap_sql_group_by](./src/zcl_demo_abap_sql_group_by.clas.abap)

## 🎬 Getting Started with the Examples

The executable examples are especially targeted at being imported into the SAP BTP ABAP environment, however, they are basically fit for both on-premise systems and the SAP BTP ABAP environment (that's why there are no ABAP programs included). Hence, check the info in the following collapsible sections for your system environment and carry out the prerequisite steps.

<details>
  <summary>1) General info</summary>
  <br>

- A few **DDIC artifacts**, for example, database tables, are part of the repository. They are used by the examples to ensure self-contained examples. For all examples to work, all artifacts must be imported.
- All examples are designed to **display some output in the ADT console**. Once successfully imported, you can **execute** the examples in ADT by choosing `F9` to display the output in the ADT console.
- The examples **include descriptions and comments** in the code for providing explanations and setting the context.
</details>

<details>
  <summary>2a) SAP BTP ABAP environment</summary>
  <br>

**Prerequisites**
- [x] Before importing the code, you have made a system-wide search for, for example, classes named `ZCL_DEMO_ABAP*` so as not to run into issues when you try to import the code. If someone has already imported the content in the system, you can simply check out that imported version and proceed with the step *3) Run the code*.
- [x] You have access to an SAP BTP ABAP Environment instance (see [here](https://blogs.sap.com/2018/09/04/sap-cloud-platform-abap-environment) for additional information).
- [x] You have downloaded and installed ABAP Development Tools (ADT). Make sure that you use the most recent version as indicated on the [installation page](https://tools.hana.ondemand.com/#abap).
- [x] You have created an ABAP cloud project in ADT that allows you to access your SAP BTP ABAP Environment instance (see [here](https://help.sap.com/viewer/5371047f1273405bb46725a417f95433/Cloud/en-US/99cc54393e4c4e77a5b7f05567d4d14c.html) for additional information). Your logon language is English.
- [x] You have installed the [abapGit](https://github.com/abapGit/eclipse.abapgit.org) plug-in for ADT from the [update site](http://eclipse.abapgit.org/updatesite/).

**Import Code**

Use the abapGit plug-in to install the <em>ABAP Cheat Sheets</em> by carrying out the following steps:

1. In your ABAP cloud project, create a package, for example, `ZABAP_CHEAT_SHEETS` as target package. It is recommended that you assign the package to a transport request suitable for demo content.
2. Add the package to the *Favorite Packages* in the *Project Explorer* view in ADT.
3. To add the <em>abapGit Repositories</em> view to the <em>ABAP</em> perspective, choose `Window` → `Show View` → `Other...` from the menu bar and choose `abapGit Repositories`.
4. On the <em>abapGit Repositories</em> view, click the `+` icon in the top right corner of the ADT tab to link a new abapGit repository.
  <br>![ADT](./files/abapGit_Repositories.png)

5. The *Link abapGit Repository* pop-up is displayed. Insert the following URL:

```
https://github.com/SAP-samples/abap-cheat-sheets.git
```

6. Choose `Next`.

7. On the *Branch and Package Selection* screen, insert the name of the created package (e. g. `ZABAP_CHEAT_SHEETS`) in the `Package` field.
8. Choose `Next`.
9.  On the *Select Transport Request* screen, select the created transport request suitable for demo content and choose `Finish` to link the Git repository to your ABAP cloud project. If the created package is already assigned to a transport request for the demo content and a message that an object is already locked in a transport request is displayed, choose `Finish`, too.
10. In the *abapGit Repositories* view, filter for your package. The repository appears in the *abapGit Repositories* view with status <em>Linked</em>.
11. Right-click the new abapGit repository and choose `Pull...` to start the cloning of the repository contents.
12. On the *Branch and Package Selection* screen, choose `Next`.
13. If the *Locally Modified Object* screen is displayed, select the objects (for example, the package to automatically select all artifacts) in the list and choose `Next`.
14. On the next screen, select a transport request and choose `Finish`. Same as above, if an *object already locked* message is displayed, choose `Finish`, too. The status in the *abapGit Repositories* view changes to <em>Pull running...</em>. Note that the pull run may take a few minutes.
15. Once the cloning has finished, the status is set to `Pulled Successfully`. You might need to refresh the `abapGit Repositories` view to see the progress of the import. To do so, choose the icon for refreshing (`Refresh`) in the top right corner of the view.
16. Refresh your project tree. E. g. in ADT, right-click the package and choose `Refresh` or `F5`. The package should contain all artifacts from the GitHub repository.
17. Make sure that all artifacts are active. To activate all inactive development objects, choose the `Activate all inactive ABAP development objects` button from the menu (or choose `CTRL+Shift+F3`).
</details>

<details>
  <summary>2b) Application Server ABAP On-Premise</summary>
<br>

**Prerequisites**
- [x] The assumption is that you are on the latest [ABAP release](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-75.htm). The content was also tested with release 7.56.
- [x] Before importing the code, you have made a system-wide search for, for example, classes named `ZCL_DEMO_ABAP*` so as not to run into issues when you try to import the code. If someone has already imported the content in the system, you can simply check out that imported version and proceed with the step *3) Run the code*.
- [x] You have downloaded and installed ABAP Development Tools (ADT). Make sure that you use the most recent version as indicated on the [installation page](https://tools.hana.ondemand.com/#abap).
- [x] You have created an ABAP Project in ADT that allows you to access your Application Server as mentioned above. Your logon language is English.
- [x] You have downloaded and installed the standalone version of the abapGit report. Make sure that you use the most recent version as indicated on the [installation page](https://docs.abapgit.org/). You can create a report, for example, `zabapgit_standalone` and copy and paste [this code](https://raw.githubusercontent.com/abapGit/build/main/zabapgit_standalone.prog.abap) into the program.
- [x] You have installed the certificate files for github.com, see [abapGit Documentation](https://docs.abapgit.org/guide-ssl-setup.html).

**Import Code**

Use the standalone version of the abapGit report to import the demo examples of the <em>ABAP Cheat Sheets</em> by carrying out the following steps:
1. In your ABAP project, create a package, for example, `TEST_ABAP_CHEAT_SHEETS` as target package suitable for demo content (e. g. by using `LOCAL` as software component).
2. Add the package to the *Favorite Packages* in the *Project Explorer* view in ADT.
3. Run the standalone version of the abapGit report.
4. Choose the `New Online` button. If the button is not available, for example, if another repository is already opened, choose the `Repository List` button.
5. On the *New Online Repository* screen, make the following entries:
   - `Git Repository URL`:

      ```
      https://github.com/SAP-samples/abap-cheat-sheets.git
      ```

   - `Package`: Your demo package, for example, `TEST_ABAP_CHEAT_SHEETS`
6. Leave the other fields unchanged and choose `Create Online Repo`.
7. On the *Repository* screen, you will see the available ABAP artifacts to be imported into your ABAP system.
8.  Choose the `Pull` button. The import of the artifacts is triggered. It might take some minutes.
9. If the `Inactive Objects` pop-up is displayed, select all artifacts and choose `Continue` (✔️).
10.	Once the cloning has finished, refresh your project tree. E. g. in ADT, right-click the package and choose `Refresh` or `F5`. The package should contain all artifacts from the GitHub repository.
11. Make sure that all artifacts are active. To activate all inactive development objects, choose the `Activate all inactive ABAP development objects` button from the (or choose `CTRL+Shift+F3`).

</details>

<details>
  <summary>3) Run the code</summary>
<br>

- Open your created package containing the imported ABAP artifacts in the ABAP Development Tools (ADT).
- Open an ABAP cheat sheet example class as listed in the [ABAP Cheat Sheets Overview](#-abap-cheat-sheets-overview) section, for example, `zcl_demo_abap_string_proc`. The classes are contained in folder `Source Code Library` → `Classes`.
- Choose `F9` to run the class. Alternatively, choose `Run` → `Run As` → `2 ABAP Application (Console)` from the menu.
- Check the console output.

> **💡 Note**<br>
>- Check notes on the context and the ABAP syntax used included in the class as comments.
>- Due to the amount of output in the console, the examples include numbers (e. g. 1) ..., 2) ..., 3) ...) representing the header of the individual example code sections. Plus, the variable name is displayed in the console in most cases. Hence, to easier and faster find the relevant output in the console, just search in the console for the number (e. g. search for `3)` for the particular output) or variable name (`STRG+F` in the console) or use break points in the code to check variables in the debugger.
>- You might want to clear the console by making a right-click within the console and choosing `Clear` before running another demo class so as not to confuse the output of multiple classes.
</details>

<br>

## ⚡ Known Issues
- Only one user on the system may import this repository as all object names must be globally unique. If you receive an error that the objects already exist when trying to import, search the system for classes named `ZCL_DEMO_ABAP*`. Someone has already imported the content in the system and you can simply check out that imported version.
- Since the repository contains self-contained examples, i. e. they work, for example, with demo database tables included in the repository (note that these tables are filled in the course of method executions), all demo artifacts must be imported so that all examples work.
- For the import into an on-premise system, note the following: The demos cover ABAP syntax irrespective of the ABAP release so as not to scatter information and to have the info in one go. Hence, there may be syntax that is not yet available with the ABAP version of your on-premise system. In that case, you might want to comment out affected code sections if an activation fails.
- Regarding potential code check warnings, for example, for the many strings in the code, not using an `ORDER BY` clause or messages regarding using `SELECT *`, the code purposely renounces pragmas and pseudo comments to keep the code fairly simple and to focus on the available ABAP syntax. See also the [Disclaimer](#%EF%B8%8F-disclaimer).

## 🛈 Further Information
- Regarding the system-internal version of the ABAP Keyword Documentation in your
  - ... **on-premise system**: Access the documentation in SAP GUI via the transactions `ABAPDOCU` (opens the documentation directly) and `ABAPHELP` (opens an input field with which you can search the documentation content, for example, you can search for a keyword like `SELECT`). Or, certainly, in your code, choose `F1` for a keyword. If you are in SAP GUI (e. g. in `SE80`), the system-internal version is opened. If you are in ADT, the documentation is opened in the *ABAP Language Help* view.
  - ... **SAP BTP ABAP environment**: In ADT, you find the documentation in the *ABAP Language Help* view where you can also search. When choosing `F1` for a keyword in your code, the documentation is opened there accordingly.
- Links to the online version of the ABAP Keyword Documentation for:
  - **Standard ABAP**: Unrestricted ABAP language scope, for example, for developments in an on-premise ABAP system → [Online version of the documentation (latest version)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap.htm)
  - **ABAP for Cloud Development**: Restricted ABAP language scope for developments in the SAP BTP ABAP environment → [Online version of the documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm)
- Regarding demonstration examples of the ABAP Keyword Documentation in your on-premise system: Have you ever checked out the package `SABAPDEMOS`? This package contains all the examples used in the ABAP Keyword Documentation. To get the context, program names etc., check out the [example page](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_examples.htm) (which is also available in the system-internal SAP GUI version as node in the topic tree) summarizing executable examples. Certainly, you also find the example topics in the context of the individual ABAP Keyword Documentation topic. The example topics have a ⚙️ sign:

  ![](./files/example_topics.png)

- See [this blog](https://blogs.sap.com/2021/04/28/video-tutorials-on-how-to-use-the-abap-keyword-documentation-abap-f1-help/) for videos about the ABAP Keyword Documentation on the [Help Portal](https://www.youtube.com/watch?v=a4ckF1XkfG8), in [SAP GUI](https://www.youtube.com/watch?v=fsX-085MlD8) and in [ADT](https://www.youtube.com/watch?v=hNGEYFpWwh0).

## ⚠️ Disclaimer
The code examples presented in this repository are only syntax examples and are not intended for direct use in a production system environment. The code examples are primarily intended to provide a better explanation and visualization of the syntax and semantics of ABAP statements and not to solve concrete programming tasks. For production application programs, a dedicated solution should therefore always be worked out for each individual case.
SAP does not guarantee either the correctness or the completeness of the code. In addition, SAP takes no legal responsibility or liability for possible errors or their consequences, which occur through the use of the example programs.

## 📟 How to Obtain Support
This project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

## 📜 License
Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.

[^1]: "A written [...] aid (such as a sheet of notes) that can be referred to for help in understanding or remembering something complex" (Definition for "cheat sheet" in Merriam-Webster Dictionary).
