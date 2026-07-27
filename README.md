[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-cheat-sheets)](https://api.reuse.software/info/github.com/SAP-samples/abap-cheat-sheets)

<a name="top"></a>

# ABAP Cheat Sheets: Demo ABAP Unit Tests

- [ABAP Cheat Sheets: Demo ABAP Unit Tests](#abap-cheat-sheets-demo-abap-unit-tests)
  - [Getting Started](#getting-started)
  - [Examples](#examples)
    - [Example Notes](#example-notes)
  - [⚠️ Disclaimer](#️-disclaimer)
  - [📟 Support and Contribution](#-support-and-contribution)
  - [📜 License](#-license)


The `unit_tests` branch of the [ABAP cheat sheet GitHub repository](https://github.com/SAP-samples/abap-cheat-sheets) features a selection of simplified ABAP Unit test scenarios across various contexts. These examples are associated with the *ABAP Unit Tests* cheat sheets.

> [!IMPORTANT]
> - The examples aim to illustrate basic ABAP unit tests with and without the use of ABAP frameworks, using simplified, non-semantic demo implementations, intended to reduce complexity and provide a high-level overview. 
> - Given their experimental nature, these examples do not claim to represent best practices for ABAP unit test setup and implementation. Always create your own solutions.


## Getting Started

> [!NOTE]
> - The code examples in this branch are designed to function independently from those in the `main` branch. Therefore, you can clone this branch without also cloning the `main` branch.
> - The following steps outline the import procedure in the SAP BTP ABAP environment.
> - If you have already imported the `main` branch of the ABAP cheat sheet repository, the repository is still linked in the *abapGit Repositories* view, and you want to have the artifacts of the `unit_tests` branch in the same package, you can proceed with the steps in the note below. Otherwise, open the *abapGit Repositories* view in ADT, filter for the linked repository, right-click it, and choose _Unlink_.

Use the [abapGit](https://github.com/abapGit/eclipse.abapgit.org) plug-in to install the examples by carrying out the following steps:

<details>
  <summary>🟢 Click for the installation steps</summary>
  <!-- -->

<br>


1. In your ABAP cloud project, create a package, for example, *ZABAP_DEMO_AUNIT* as the target package. The package should be local.
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
9.  On the *Branch and Package Selection* screen, select the `unit_tests` branch and enter the name of the created package (for example, *ZABAP_DEMO_AUNIT*) in the *Package* field. 
10. Choose *Next*.
11. On the *Select Transport Request* screen, choose *Finish* to link the Git repository to your ABAP cloud project. The package should be local.
12. In the *abapGit Repositories* view, filter for your package. The repository appears in the *abapGit Repositories* view with the status <em>Linked</em>.
13. Right-click on the new abapGit repository and choose *Pull...* to start the cloning of the repository contents. Note that you can also choose the pulling in a previous wizard step.
14. On the *Branch and Package Selection* screen, choose *Next*.
15. On the next screen, select the objects (select the package/top-most selection box to automatically select all artifacts) from the list and choose *Next*.
16. On the next screen - the package should be local - choose *Finish*. Same as above, if an *object already locked* message is displayed, choose *Finish* as well. The status in the *abapGit Repositories* view changes to <em>Pull running...</em>. Note that the pull run may take several minutes.
17. Once the cloning is complete, the status changes to *Pulled Successfully*. You may need to refresh the *abapGit Repositories* view to see the progress of the import. To do this, choose the  *Refresh* icon in the upper right corner of the view.
18. Refresh your project tree. For example, in ADT, right-click the package and choose *Refresh*. The package should contain all the artifacts from the GitHub repository.
19. Make sure that all artifacts are active. To activate all inactive development objects, choose the *Activate all inactive ABAP development objects* button from the menu (or choose *CTRL+Shift+F3*). 



</details>  

<br>

> [!NOTE]
> If you have already imported the `main` branch of the ABAP cheat sheet repository, and linking the repository anew is rejected, proceed as follows to include the artifacts in the package of the already imported repository: 
> - Open the *abapGit Repositories* view in ADT. 
> - Right-click the line with the above GitHub URL. Choose _Switch Branch_. Provide user credentials when prompted.
> - On the *Branch Selection* screen, select the `unit_tests` branch. The package entry remains unchanged.
> - Choose *Next*.
> - In the *abapGit Repositories* view, the _Status_ should be _Linked_.
> - Right-click the line and choose _Pull_.
> - In the _Object Selection for Pull_ screen, you may choose _Next_ without selecting any artifacts for deletion.
> - Once having finished the wizard, the _Status_ should be _Pull starting_. You can choose the *Refresh* icon to check on the progress.
> - Make sure that all artifacts are active. To activate all inactive development objects, choose the *Activate all inactive ABAP development objects* button from the menu (or choose *CTRL+Shift+F3*).


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Examples

The following examples illustrate simplified ABAP Unit test scenarios across various contexts, including:

- Testing methods without dependent-on components (DOCs) and without ABAP frameworks
- Testing methods with DOCs and without ABAP frameworks
- Using ABAP frameworks to manage these DOCs (for example, by creating and injecting test doubles):
    - Classes (ABAP OO Test Double Framework)
    - Database (ABAP SQL Test Double Framework)
    - CDS view entity (CDS Test Double Framework)
    - RAP business object (creating transactional buffer test doubles and mocking ABAP EML APIs)
    - Authority check dependencies
    - Function module (Function Module Test Double Framework)
    - Inspecting background processing using bgPF
- Using test seams
- Test classes located in an external class rather than the class being tested, demonstrating the use of the `"!@testing ...` syntax

> [!NOTE]  
> - Several contexts are covered in the ABAP cheat sheet's executable examples of the `main` branch, combining various scenarios and the use of ABAP frameworks to reduce the number of artifacts. The ABAP Unit examples here focus on the various contexts in individual classes, independent of artifacts from the ABAP cheat sheet repository.
> - The examples do not claim to represent best practices or model approaches and setups. They serve only to illustrate ABAP Unit aspects and functionality, most of them making use of the available frameworks. Make sure that you create your own solutions.
> - For more information on the frameworks, refer to the ABAP Doc comments in the classes.
> - For simplicity, many example methods used for unit tests are similar or identical across the example classes.


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example Notes

<table>

<tr>
<td> Context </td> <td> Notes </td>
</tr>

<tr>
<td> 
Testing methods without dependent-on components (DOC) and without ABAP frameworks
</td>
<td> 

- **Class**: `zcl_demo_aunit_no_tdf`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for a method that does not involve any dependent-on component (DOC) and does not use an ABAP framework.
    - The test values are hard-coded.
- **Global class**: 
    - Contains a private calculation method.
    - Takes two integer values and an enumeration type (indicating the operator) to calculate the result.
- **Test class**: 
    - Defines the local test class `ltc_calculate`.
    - Since the method being tested is private, the global class and the local test class are befriended using the `LOCAL FRIENDS` addition.
    - Several test methods assess various operations, including addition, subtraction, multiplication, and division. Edge cases, such as division by zero and arithmetic overflow, are also tested using hard-coded values.


</td>
</tr>


<tr>
<td> 
Testing methods with DOC and without ABAP frameworks
</td>
<td> 

- **Class**: `zcl_demo_aunit_no_tdf_doc`
- **Purpose**:
    - Demonstrates ABAP Unit tests for methods that involve dependent-on components (DOCs) without using an ABAP test double framework.
    - Uses self-created local test doubles and hard-coded test data.
- **Global class**:
    - Implements constructor-based dependency injection for two DOC interfaces (price provider and flights provider), with default productive providers as fallback options.
    - Provides two methods:
        - Calculating price from the current price and discount (retrieves discount information from the DOC).
        - Computing occupancy rate from flight seat data (retrieves flight data from the DOC).
- **Test class**:
    - Defines local test doubles (`ltd_test_double_discount`, `ltd_test_double_occupancy`) and local test classes (`ltc_calculate_price`, `ltc_occupancy_rate`).
    - Injects the local test doubles into the constructor.
    - Includes multiple hard-coded test scenarios for normal, boundary, invalid, rounding, and no data cases for both price and occupancy calculations.


</td>
</tr>

<tr>
<td> 
Classes (ABAP OO Test Double Framework)
</td>
<td> 

- **Class**: `zcl_demo_aunit_abap_oo_tdf`
- **Purpose**:
    - Demonstrates ABAP Unit tests with a DOC using the ABAP OO Test Double Framework.
    - Illustrates constructor injection to replace a dependency during testing.
- **Global class**:
    - Contains constructor-based injection of a discount provider interface (`zif_demo_aunit_price`), with a default fallback for the productive provider.
    - Implements price calculation from the current price and discount.
- **Test class**:
    - Defines the local test class that creates and configures an ABAP OO framework test double for the discount provider interface.
    - Injects the configured test double into the class under test and verifies outcomes.
    - Asserts normal, boundary, invalid, and rounding scenarios.

</td>
</tr>

<tr>
<td> 
Database (ABAP SQL Test Double Framework)
</td>
<td> 

- **Class**: `zcl_demo_aunit_sql_tdf`
- **Purpose**:
    - Demonstrates ABAP Unit tests for database-dependent logic using the ABAP SQL Test Double Framework.
    - Shows testing of calculations without relying on productive database table data.
- **Global class**:
    - Contains a method that reads seat data from a database table for a carrier, aggregates totals, and calculates the rounded occupancy rate.    
- **Test class**:
    - Defines a local test class that creates an SQL test environment for the database table `ztaunitflights`.
    - Clears doubles per test, injects predefined table data, and verifies expected occupancy outcomes.
    - Asserts normal, full, zero, rounding, non-existing carrier, and no data scenarios.


</td>
</tr>

<tr>
<td> 
CDS view entity (CDS Test Double Framework)
</td>
<td> 

- **Class**: `zcl_demo_aunit_cds_tdf`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for CDS-dependent logic using the ABAP CDS Test Double Framework.
    - Shows testing of calculations without relying on productive data available via a CDS entity.    
- **Global class**: 
    - Contains a method that retrieves flight seat data from CDS entity `zraunitflights`, aggregates totals, and calculates the rounded occupancy rate.    
- **Test class**: 
    - Defines a local test class that creates a CDS test environment for `zraunitflights`.
    - Clears doubles per test, injects test datasets, and asserts expected calculation results.
    - Asserts normal, full, zero, rounding, non-existing carrier, and no data scenarios.

</td>
</tr>

<tr>
<td> 
RAP business object: Creating transactional buffer test doubles
</td>
<td> 

- **Class**: `zcl_demo_aunit_rap_buffer`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for RAP BO interaction logic using the RAP transaction buffer test double framework.
    - Shows testing of EML read behavior without productive RAP BO data.
- **Global class**: 
    - Contains a method that reads RAP BO instances via `READ ENTITY` and returns the results along with any failed responses.
    - Adapts each returned flight by deriving `Planetype` from `Seatsmax` using explicit threshold rules (`A`, `B`, `C`).
- **Test class**: 
    - Defines a local test class that creates a RAP transaction buffer BO test environment and retrieves the RAP BO test double.
    - Clears doubles per test, inserts test instances, executes the method under test, and validates the results and failed response behavior.
    - Asserts scenarios with existing keys, mixed existing and non-existing keys, and situations with no data.

</td>
</tr>

<tr>
<td> 
RAP business object: Mocking ABAP EML APIs
</td>
<td> 

- **Class**: `zcl_demo_aunit_rap_eml`
- **Purpose**: 
    - Demonstrates ABAP Unit tests with ABAP EML requests as DOCs
    - Shows mocking of ABAP EML requests for both both read and modify requests.
- **Global class**: 
    - Provides wrapper methods around RAP EML for demonstration purposes:
      - `demo_eml_modify` for `MODIFY ENTITY ... CREATE` with mapped/ and failed responses.
      - `demo_eml_read` for `READ ENTITY` with the result and failed responses.    
- **Test class**: 
    - Defines a local test class that creates the ABAP EML mock environment and clears doubles per test.
    - Configures input/output expectations on the ABAP EML test double for read and modify operations, executes the code under test, and verifies responses.
    - Asserts successful, failed, and unconfigured test double scenarios for both read and create requests.



</td>
</tr>

<tr>
<td> 
RAP business object: Testing ABAP behavior pool
</td>
<td> 

- **Class**: `zbp_zraunitflights`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for an ABAP behavior pool
    - Shows testing an action and a validation
- **Global class**: 
    - Contains only the class definition skeleton    
- **Local types**:     
    - Includes the behavior implementation.
    - Defines friendship between local and test class to allow the test class to access private methods.
    - Action `calc_occ_rate`: Retrieves flight seat information based on instance keys and calculates the occupancy rate using this data.
    - Validation `val`: Triggered when saving seat-related fields. It retrieves flight seat information based on instance keys and fails if there are invalid entries (if seatsmax or seatsocc are below 0, or if seatsocc exceeds seatsmax).    
- **Test class**: 
    - Defines a local test class that creates transactional buffer test doubles. 
    - Uses the statement `CREATE OBJECT ... FOR TESTING` to instantiate the class under test.
    - Test methods address different aspects, including valid and invalid cases.

</td>
</tr>

<tr>
<td> 
Authority check dependencies
</td>
<td> 

Note the prerequisites before exploring the demo. For the details, expand the collapsible section below.

<details>
  <summary>🟢 Click for the prerequisites of the authority check dependencies example</summary>
  <!-- -->

<br>

- **Note**: 
  - The examples are designed for the SAP BTP ABAP Environment. 
  - The following steps are only relevant if you are using an SAP BTP ABAP Environment and you want to explore the example regarding the unit tests for the authority check dependencies using the demo authorization object `ZAUTH_OB`.
  - If you are using an on-premise environment and want to skip creating the demo authorization object `ZAUTH_OB` and the steps to add a role to your user, you can replace the literal with the demo authorization object in the example class code with `S_DEVELOP` and omit the following (SAP BTP ABAP Environment-related) steps.
- Details regarding the demo authorization object and steps:
  - Object class: CPAE
  - Authorization field ACTVT should be available.
  - Permitted activities: 01 (create or generate), 02 (change), 03 (display), 06 (delete).
- SAP BTP ABAP Environment-related steps:
  - Refer to the implementation details in the [Authorization Checks](25_Authorization_Checks.md) cheat sheet, section [Executable Example (SAP BTP ABAP Environment)](25_Authorization_Checks.md#executable-example-sap-btp-abap-environment). 
  - High-level steps: 
    - Create an IAM app, for example, `ZDEMO_AUTH_IAM`. Use External app as the application type. In the Authorization tab, add the demo object and select ACTVT. After adding it, select all field values for ACTVT, such as create, change, etc. Publish it locally.
    - Create a business catalog, for example, `ZDEMO_BUSINESS_CATALOG`. In the Apps tab, add `ZDEMO_AUTH_IAM_EXT`. Publish it locally.
    - Log in to the system and access the SAP Fiori Launchpad as an administrator. Open the Maintain Business Roles app. Create a business role, e.g., `ZBRAUTHDEMO`, add the created business catalog, and assign it to your user.



</details>  

<br>

- **Class**: `zcl_demo_aunit_auth`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for authorization-dependent logic as a DOC scenario.
    - Shows testing of `AUTHORITY-CHECK` behavior by controlling the authorization context.    
- **Global class**: 
    - Defines an enumerated type representing activities (`create`, `change`, `display`, `delete`) and maps each value to its corresponding `ACTVT` code.
    - A method executes `AUTHORITY-CHECK` for the demo object `ZAUTH_OB` and returns `abap_true` when `sy-subrc = 0`.
- **Test class**: 
    - Defines a local test class that uses an API for authorization checks.
    - Note that the API only restricts the authorizations of the user running the test and does not grant additional authorizations. To follow the example fully, complete the prerequisite steps, create the demo authorization object, and assign a business role to your user (along with any necessary steps in the SAP BTP ABAP Environment). In an on-premise environment, you could replace the demo authorization object in the code with `S_DEVELOP`, for example.
    - Configures different authorization sets, executes `call_authority_check`, and verifies expected outcomes for each action.
    - The demo authorization object assumes your user has the `create`, `change`, `display`, and `delete` authorizations. Therefore, the unrestricted test methods should return true for the authorization check. Some test methods are designed to restrict certain authorizations (only `change` and `display`). For example, even though deletion is actually authorized, the restriction setting using the API will indicate that deletion is not permitted.
    - Asserts both positive and negative authorization cases and covers execution log behavior.





</td>
</tr>

<tr>
<td> 
Function module (Function Module Test Double Framework)
</td>
<td> 


- **Class**: `zcl_demo_aunit_func_tdf`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for logic that depends on a function module.
- **Global class**: 
    - Defines an enumeration type for arithmetic operators and a `calculate` method that accepts two integers and an operator.
    - Delegates calculations to the `ZFUNC_DEMO_AUNIT` function module and returns the its result.
- **Test class**: 
    - Creates a local test class that sets up a test environment for `ZFUNC_DEMO_AUNIT`.
    - Configures test double behavior for specific input combinations (returned values and raised exceptions), executes the code under test, and asserts outcomes.
    - Asserts normal arithmetic operations and error scenarios, including division by zero and arithmetic overflow.

</td>
</tr>

<tr>
<td> 
Inspecting background processing using bgPF
</td>
<td> 

- **Class**: `zcl_demo_aunit_bgpf`
- **Purpose**: 
    - Demonstrates ABAP Unit tests for background processing logic using the ABAP Background Processing Framework (bgPF).
    - Shows how background processing can be inspected with framework test spies instead of real asynchronous execution.
- **Global class**: 
    - Implements bgPF operation and scheduling behavior (`if_bgmc_op_single~execute`, `execute`, `execute_2`).
    - The example includes a transactionally controlled scenario (see [Controlled SAP LUW](https://help.sap.com/docs/abap-cloud/abap-concepts/controlled-sap-luw)). For that purpose, the `if_bgmc_op_single~execute` implementation includes a `cl_abap_tx=>save( ).` call, followed by a method call that modifies a database table. The previous method call, `set_attribute( ).`, is meant to transform a string that was passed via instance constructor to upper case and assign the value to an instance attribute (which can be retrieved by a `get_input` method call).
    - The methods `execute` and `execute_2` are tested. The include the (double) triggering of background processes.
    - The class can also be run using F9 as it implements the `if_oo_adt_classrun~main` method, illustrating the effect of the background processing.    
- **Test class**: 
    - Defines a local test class that creates a bgPF spy.
    - The test methods show various method calls the API offers, among them assertions regarding the number of background processes - which are not actually triggered.
- Find more information on bgPF and a similar example [here](https://help.sap.com/docs/abap-cloud/abap-concepts/background-processing-framework).

</td>
</tr>

<tr>
<td> 
Using test seams
</td>
<td> 

- **Class**: `zcl_demo_aunit_test_seams`
- **Purpose**: 
    - Demonstrates ABAP Unit tests using ABAP test seams.
    - Shows how `TEST-SEAM` and `TEST-INJECTION` statements can isolate database access. The examples do not use ABAP frameworks.
- **Global class**: 
    - Includes a method for calculating occupancy rates with a seam (`select_from_db`) that wraps around the database selection logic.
    - Contains a demo method with seams (`ts1`, `ts2`) to illustrate how injected test code functions.
- **Test class**: 
    - Defines local test classes that inject code to control input data and behavior.
    - Verifies occupancy rate calculation scenarios (normal, full, zero, rounding, no data) via injected datasets.
    - Verifies behavior with and without injections (`ts1`, `ts2`).

</td>
</tr>

<tr>
<td> 
Test classes located in an external class rather than the class being tested
</td>
<td> 

- **Purpose**: 
    - Demonstrates ABAP Unit testing with an external test class for scenarios where tests are outside the production class, showing the use of the `"! @testing ...` syntax.            
- **Involved classes**: 
    - `zcl_demo_aunit_external_cl`
        - Represents the class to be tested. 
        - Does not include DOCs.
        - Includes identical calculation methods in both public and private visibility section.
        - To enable the external test, class `zcl_demo_aunit_external_cl` befriends `ztcl_demo_aunit_external_cl`.
    - `ztcl_demo_aunit_external_cl`
        - Represents the class that includes the tests for class `zcl_demo_aunit_external_cl`
        - **Global class**: 
            - The example setup contains a private static method `call_private_calculate` that represents a bridge method to enable testing of the private method, which is accessible here in the global class due to the friendship.
        - **Test class**: 
            - Defines two local test classes for both the public and private method.
            - Test methods cover standard arithmetic operations and edge cases (division by zero, overflow) with explicit assertions.        
            - The test classes use the specification `"!@testing zcl_demo_aunit_external_cl`.
            - The test class testing the private method via the bridge is befriended with the global class to enable instantiation of the global class. The instance is passed in the test, along with demo values.

</td>
</tr>


</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>

<br>

## ⚠️ Disclaimer
The code examples presented in this repository are only syntax examples and are not intended for direct use in a production system environment. The code examples are primarily intended to provide a better explanation and visualization of the syntax and semantics of ABAP statements and not to solve concrete programming tasks. For production application programs, a dedicated solution should therefore always be worked out for each individual case.
There is no guarantee for either the correctness or the completeness of the code. In addition, there is no legal responsibility or liability for possible errors or their consequences, which occur through the use of the example code.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## 📟 Support and Contribution
This is not intended to be a contribution repository, so please do not create pull requests. If you like to address issues or suggestions, please create an issue. However, this project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## 📜 License
Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.

<p align="right"><a href="#top">⬆️ back to top</a></p>