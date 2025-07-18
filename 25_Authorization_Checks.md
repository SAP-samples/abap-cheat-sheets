<a name="top"></a>

# Authorization Checks

- [Authorization Checks](#authorization-checks)
  - [Introduction](#introduction)
  - [Terms](#terms)
  - [Explicit Authorization Checks Using AUTHORITY-CHECK Statements](#explicit-authorization-checks-using-authority-check-statements)
  - [Implicit Authorization Checks Using CDS Access Control for Read Accesses](#implicit-authorization-checks-using-cds-access-control-for-read-accesses)
  - [Executable Example (SAP BTP ABAP Environment)](#executable-example-sap-btp-abap-environment)
    - [Implementation Steps](#implementation-steps)
    - [Example Class](#example-class)
  - [Excursion: Authorization Control in RAP](#excursion-authorization-control-in-rap)
  - [More Information](#more-information)


This cheat sheet ... 
- provides a high-level overview on authorization checks in ABAP, supported by an executable example to explore the authorization checks in action. 
- focuses on the SAP BTP ABAP Environment. Therefore, the procedure of assigning authorizations is different from, for example, [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm).
- is meant to give an idea about the topic. Make sure that you consult the documentation for more details and the complete picture.

## Introduction

- In an SAP system, you need to protect data from unauthorized access by making sure that only those authorized to access it can see and modify it.
- Authorization to access data can be set. Before a user can perform certain operations in your application, you need to implement authorization checks.
- This enters the picture, for example, in the context of ABAP SQL. Since ABAP SQL statements do not trigger any authorization checks in the database system, this is even more important. Database tables may be accessed without restriction using these statements. Conversely, not all users in a system are authorized to access all data available to ABAP SQL statements.
- Thus, it is up to the programmer to ensure that each user who can call the program is authorized to access the data it handles.
- Authorization checks can be performed ...
  - explicitly using `AUTHORITY-CHECK` statements.
  - implicitly using CDS access control for read accesses.

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Terms

The following topic covers authorization-related terms and provides you with the basics: [Authorization Basics](https://help.sap.com/docs/btp/sap-business-technology-platform/authorization-basics)

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Explicit Authorization Checks Using AUTHORITY-CHECK Statements


- `AUTHORITY-CHECK` statements check whether an authorization is available for the current user.
- This authorization is determined by the values set for the authorization fields within an authorization object.
- The combination of these two elements defines the activities users can perform and the data they can access.
- That is, users authenticate by logging into the system with their credentials. Once authenticated, they can access functions and data based on their authorizations.
- Syntax pattern of `AUTHORITY-CHECK` statements:

  ```abap
  AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ' 
      ID id1 FIELD val1
      ID id2 FIELD val2
      ID id3 DUMMY
      ... .
  ```

- Notes on the statement:
  - The authorization of the current user is checked. In [Standard ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_abap_glosry.htm), an addition is available with which you can specify other users.
  - `'ZAUTH_OBJ'` stands for the name of an authorization object in uppercase letters. In [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm), it must be a literal. It cannot be a (flat character-like) data object.
  - `ID ... FIELD ...`: Specifies at least one and a maximum of 10 different authorization fields for the specified authorization object. The name of the autorization field in uppercase letters comes after `ID`, the value after `FIELD`.
  - The `DUMMY` addition specifies that an authorization field is not checked.
  - The authorization check result is determined by the `sy-subrc` value. The value 0 means that the authorization check is successful or no check was performed. 4 means that the check is not successful. For information on other values, see the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapauthority-check.htm).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Implicit Authorization Checks Using CDS Access Control for Read Accesses


- Implicit authorization checks apply to several CDS entities, including [CDS view entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_v2_view_glosry.htm). 
- These checks come into play when you access the entities via ABAP SQL and have defined access conditions in a [CDS role](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_role_glosry.htm). 
- These roles, which are evaluated for each user, are defined by CDS artifacts using `DEFINE ROLE` statements in DCL (data control language) source code. 
- As a result, the returned data is limited to only what is permitted. 
- The `@AccessControl.authorizationCheck` CDS entity annotation influences access control with the following options:
  - `#NOT_REQUIRED`: No access control is needed, granting full access to all users.
  - `#CHECK`: A warning is issued if no access control object is present.
  - `#MANDATORY`: An access control object must be present.
  - `#NOT_ALLOWED`: An access control object must not be present. If one exists, it is disregarded. 
- The authorizations are based on authorization objects.
- Example CDS view entity with the `@AccessControl.authorizationCheck: #CHECK` annotation

  ```abap
  @AccessControl.authorizationCheck: #CHECK
  define view entity ZDEMO_ABAP_FLSCH_VE_AUTH
    as select from zdemo_abap_flsch
  {
    key carrid,
    key connid,
        countryfr,
        ...
  }
  ```

- Example CDS access control, which defines a CDS role and an [access rule](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_dcl_role_rules.htm).
  - Note that there are multiple access rules available to be specified. Refer to the documentation. 
  - In the example case, a conditional access rule is used with `grant select on ... where`.
  - `aspect pfcg_auth` is followed by a specification of an authorization object and authorization fields. The value for `ACTVT` is `03` (display authorization). See the example further down for the effect.

  ```abap
  @EndUserText.label: 'Test'
  @MappingRole: true
  define role ZCDS_ACC_CTRL {
    grant 
      select
        on
          ZDEMO_ABAP_FLSCH_VE_AUTH
            where
              (countryfr) = aspect pfcg_auth(zauth_obj, zauth_ctry, ACTVT = '03');
              
  }
  ```

- Find more details in the ABAP Keyword Documentation and the subtopics: [ABAP CDS - Access Control](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_access_control.htm)

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example (SAP BTP ABAP Environment)

> [!NOTE]
> - The example is intentionally simplified and nonsemantic, designed to explore basic authorization checks.
> - It is not meant to serve as a model for proper authorization check design. Always devise your own solutions for each unique case.
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)

### Implementation Steps

> [!NOTE]
> - As a prerequisite, you have imported the ABAP cheat sheet GitHub repository. The example relies on some of its repository objects.
> - To get more details on the general implementation steps, see this [tutorial](https://developers.sap.com/tutorials/abap-environment-authorization.html). Note that not all the steps there are covered and relevant for the executable example here. Plus, other artifact names are used.
> - The purpose of the example is that activities are only allowed when the *countryfr* value is *US*. This is checked by the authorization check examples in the class further down.


Expand the following collapsible section for the implementation steps required for the executable example.

<details>
  <summary>🟢 Click to expand for the implementation steps</summary>
  <!-- -->

<br>

- You have accessed your SAP BTP ABAP Environment in ADT.
- Create an authorization field
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *Authorization Field* and choose *Next* and walk through the wizard. 
  - As the name, use *ZAUTH_CTRY*.  
  - Specify the data element `LAND1`.
  - Save and activate.
- Create an authorization object
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *Authorization Object* and choose *Next* and walk through the wizard. 
  - As the name, use *ZAUTH_OBJ*.
  - In the *Authorizaton Fields* section, add the *ZAUTH_CTRY* field created above.
  - Select the *Activity Field* checkbox for the *ACTVT* field.
  - In the *Permitted Activities* section, the following activities should be available/added: 01 (create), 02 (change), 03 (display), 06 (delete)
    - Note: The *ACTVT* field is a standard field that determines what activities users are allowed to perform. There are more options available than those mentioned.
  - Save and activate.
- Create an IAM app
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *IAM app* and choose *Next* and walk through the wizard. 
  - As the name, use *ZIAM_AUTH*.   
  - As the application type, use *External app*. Note that the artifact's name will then be *ZIAM_AUTH_EXT*.
  - Open the *Authorizations* tab.
  - In the *Authorization Objects* section, add the authorization object *ZAUTH_OBJ* and choose *Ok*.  
  - In the *Instances of ...* section, choose *ACTVT* and select all checkboxes in the *ACTVT* section on the right.
  - Choose *ZAUTH_CTRY* and enter the value `US` under *From* in the *ZAUTH_CTRY* section on the right.    
  - Save and activate.
  - Choose *Publish Locally*
- Create a business catalog
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *Business catalog* and choose *Next* and walk through the wizard. 
  - As the name, use *ZAUTH_BUS_CAT*.  
  - In the *Apps* tab, choose *Add*. 
  - Select the IAM app *ZIAM_AUTH_EXT*.
  - On the *Business Catalog App Assignment: ZAUTH_BUS_CAT_0001* screen, the *ZAUTH_BUS_CAT* business catalog ID and the *ZIAM_AUTH_EXT* IAM app ID are displayed.
  - Go back to the business catalog, and choose *Publish Locally*. It may take a while to complete.
- Create a business role, add the created business catalog and assign your user
  - To get more details on the general implementation steps, see this [tutorial](https://developers.sap.com/tutorials/abap-environment-access-mgmt.html).
  - Log on to the system and acccess the SAP Fiori Launchpad as administrator.
  - Access the *Maintain Business Roles* app.
  - Choose *New* and create a business role with the name *ZBR_AUTH_TEST*.
  - Go to the *Business Catalogs* tab.
  - Choose *Add*, and search for *ZAUTH_BUS_CAT*.
  - Select it and choose *Ok* to add it.
  - Go to the *Business Users* tab.
  - Choose *Add*. For this example, select your business user and choose *Ok*.
  - The access categories are maintained as *Unrestricted*.
  - Save it.


For creating a CDS access control, proceed as follows: 
- First, create a CDS view entity. 
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *Data definition* and choose *Next* and walk through the wizard. 
  - As the name, use *ZDEMO_ABAP_FLSCH_VE_AUTH*. It is meant to be a duplicate of the *ZDEMO_ABAP_FLSCH_VE* CDS view entity from the repository. However, the new one should have the annotation `@AccessControl.authorizationCheck: #CHECK`.
  - Insert the following code, save and activate it.

    ```abap
    @AccessControl.authorizationCheck: #CHECK
    define view entity ZDEMO_ABAP_FLSCH_VE_AUTH
      as select from zdemo_abap_flsch
    {
      key carrid,
      key connid,
          countryfr,
          cityfrom,
          airpfrom,
          countryto,
          cityto,
          airpto,
          fltime,
          deptime,
          arrtime,
          distance,
          distid,
          fltype,
          period
    }
    ```
  - A syntax warning should be displayed that there is no access control available. 
- Create an access control
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *Data definition* and choose *Next*. 
  - As the name, use *ZCDS_ACC_CTRL*. 
  - In a step in the wizard, select *Define Role with PFCG Aspect*. 
  - As entity, use *ZDEMO_ABAP_FLSCH_VE_AUTH*.
  - Adapt the code according to the code below. The authorization object and fields from above are used. The specifications including *ACTVT = '03'* mean that entries can be read (displayed) if the *countryfr* is *US*.

    ```abap
    @EndUserText.label: 'Test'
    @MappingRole: true
    define role ZCDS_ACC_CTRL {
      grant 
        select
          on
            ZDEMO_ABAP_FLSCH_VE_AUTH
              where
                (countryfr) = aspect pfcg_auth(zauth_obj, zauth_ctry, ACTVT = '03');
                
    }
    ```
  - Save and activate it. After that, the syntax warning in the CDS view entity should be gone.
- Provided that you have walked through all steps, you can quickly test the effect of the authorization check:
  - Access the *ZDEMO_ABAP_FLSCH_VE_AUTH* view entity.
  - Choose *F8* to open the data preview.
  - Only entries with the value *US* in the *countryfr* column should be displayed.
    - Note: If you have not yet executed an example class from the ABAP cheat sheet repository that populates the underlying database table, there is no data to be displayed. In that case, implement and execute the following example class so that the database table is populated, and try again.

</details>  


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Example Class

Expand the following collapsible section for example code. Provided that the implementation steps have been walked through, the example explores explicit and implicit authorization checks. To try it out, create a demo class named `zcl_demo_abap_auth` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->

<br>

```abap
CLASS zcl_demo_abap_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_auth IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    "----------------- NOTE -----------------
    "The example explores explicit authorization checks with
    "AUTHORITY-CHECK statements and implicit authorization checks
    "when reading from CDS view entity for which an access control
    "has been specified.
    "The example assumes that you have walked through the
    "implementation steps in the Authorization Checks cheat sheet
    "document, and that you have used the names for artifacts
    "outline there.

    "Populating demo database tables
    zcl_demo_abap_aux=>fill_dbtabs( ).

    "SELECT statement retrieving data from a database table
    "without explicit authorization check
    "This is to emphasize that ABAP SQL statements do not trigger
    "any authorization checks in the database system. Therefore,
    "all data is read from the database table.
    SELECT * FROM zdemo_abap_flsch INTO TABLE @DATA(itab1).

    out->write( data = itab1 name = `itab1` ).
    out->write( |\n| ).

    "----------------- AUTHORITY-CHECK statements -----------------

    "The following examples explore AUTHORITY-CHECK statements by
    "checking out different values for the authorization fields.
    "In the example, CRUD operations are permitted in case of the
    "value 'US'.

    "Using field value 'US'
    "Expected result: Allowed
    AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ'
            ID 'ZAUTH_CTRY' FIELD 'US'
            ID 'ACTVT'      FIELD '03'.

    IF sy-subrc = 0.
      out->write( `US/03: Allowed` ).
    ELSE.
      out->write( `US/03: Not allowed` ).
    ENDIF.
    out->write( |\n| ).

    "Using a non-specified value for the authorization field
    "Expected result: Not allowed
    AUTHORITY-CHECK
        OBJECT 'ZAUTH_OBJ'
            ID 'ZAUTH_CTRY' FIELD 'DE'
            ID 'ACTVT'      FIELD '03'.

    IF sy-subrc = 0.
      out->write( `DE/03: Allowed` ).
    ELSE.
      out->write( `DE/03: Not allowed` ).
    ENDIF.
    out->write( |\n| ).

    "Using a non-specified value for the authorization field
    "Expected result: Not allowed
    AUTHORITY-CHECK
        OBJECT 'ZAUTH_OBJ'
            ID 'ZAUTH_CTRY' FIELD 'IT'
            ID 'ACTVT'     FIELD '03'.

    IF sy-subrc = 0.
      out->write( `IT/03: Allowed` ).
    ELSE.
      out->write( `IT/03: Not allowed` ).
    ENDIF.
    out->write( |\n| ).
    out->write( |\n| ).

    "The following example checks various values specified for
    "the authorization fields.
    "Expected result: Only the first 4 entries in the table should have "Allowed"
    "The value 10 is not specified for ACTVT in the example.
    TYPES c3 TYPE c LENGTH 3.
    TYPES c3tab TYPE TABLE OF c3 WITH EMPTY KEY.
    DATA auth_check TYPE string_table.
    LOOP AT VALUE c3tab( ( 'US' ) ( 'JP' ) ( 'IT' ) ( 'DE' ) ) INTO DATA(auth_field_z).
      LOOP AT VALUE c3tab( ( '01' ) ( '02' ) ( '03' ) ( '06' ) ( '10' ) ) INTO DATA(auth_field_atvt).
        AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ'
          ID 'ZAUTH_CTRY' FIELD auth_field_z
          ID 'ACTVT'      FIELD auth_field_atvt.

        IF sy-subrc = 0.
          APPEND |ZAUTH_CTRY '{ auth_field_z }', ACTVT '{ auth_field_atvt }', sy-subrc '{ sy-subrc }': Allowed| TO auth_check.
        ELSE.
          APPEND |ZAUTH_CTRY '{ auth_field_z }', ACTVT '{ auth_field_atvt }', sy-subrc '{ sy-subrc }': Not allowed| TO auth_check.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    out->write( data = auth_check name = `auth_check`  ).
    out->write( |\n|  ).

    "The following examples explore various specifications for the authorization field values
    "and the DUMMY addition.

    "Activity authorization field value not specified in the example
    "Expected result: sy-subrc = 4, authorization check not successful
    AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ'
      ID 'ACTVT' FIELD '10'.

    out->write( |1) ACTVT checked and value does not exist, sy-subrc '{ sy-subrc }'| ).

    "DUMMY addition
    "ZAUTH_CTRY not checked, ACTVT checked and value exists
    "Expected result: sy-subrc = 0, authorization check successful or no check was performed
    AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ'
      ID 'ZAUTH_CTRY' DUMMY
      ID 'ACTVT' FIELD '03'.

    out->write( |2) ZAUTH_CTRY not checked due to DUMMY addition, ACTVT checked and value exists, sy-subrc '{ sy-subrc }'| ).

    "ZAUTH_CTRY not checked, ACTVT checked and value does not exist
    "Expected result: sy-subrc = 4, authorization check not successful, value specified not found
    AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ'
      ID 'ZAUTH_CTRY' DUMMY
      ID 'ACTVT'      FIELD '10'.

    out->write( |3) ZAUTH_CTRY not checked due to DUMMY addition, ACTVT checked and value does not exist, sy-subrc '{ sy-subrc }'| ).

    "ZAUTH_CTRY checked and value does not exist, ACTVT not checked
    "Expected result: sy-subrc = 4, authorization check not successful, value specified not found
    AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ'
      ID 'ZAUTH_CTRY' FIELD 'XX'
      ID 'ACTVT'      DUMMY.

    out->write( |4) ZAUTH_CTRY checked and value does not exist, ACTVT not checked due to DUMMY addition, sy-subrc '{ sy-subrc }'| ).

    "The following example includes a non-existent authorization field and ACTVT. None is checked.
    "Expected result: sy-subrc = 0, in this case, no check was performed
    AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ'
      ID 'NON_EXISTENT' DUMMY
      ID 'ACTVT'        DUMMY.

    out->write( |5) DUMMY addition specified for all authorization fields, sy-subrc '{ sy-subrc }'| ).

    "Non-existent authorization object specified, authorization fields with DUMMY addition
    "Expected result: sy-subrc = 12, no authorization found
    AUTHORITY-CHECK OBJECT 'NON_EXISTENT'
      ID 'NON_EXISTENT' DUMMY
      ID 'ACTVT'        DUMMY.

    out->write( |6) Using a non-existent authorization object, DUMMY addition specified for all authorization fields, sy-subrc '{ sy-subrc }'| ).
    out->write( |\n| ).
    out->write( |\n| ).

    "----------------- Implict authorization checks using CDS access control -----------------
    "The following SELECT statement retrieves data using a CDS view entity as data source.
    "In the example, a CDS access control is included. A WHERE clause is not specified.
    SELECT * FROM zdemo_abap_flsch_ve_auth INTO TABLE @DATA(itab2).

    out->write( data = itab2 name = `itab2` ).
    out->write( |\n| ).

    DATA flag TYPE abap_boolean.
    LOOP AT itab2 INTO DATA(line) WHERE countryfr <> 'US'.
      flag = abap_true.
    ENDLOOP.

    IF flag = abap_true.
      out->write( `Something went wrong with the example procedure. There should only be entries with the condition countryfr = 'US'.` ).
    ELSE.
      out->write( `Expected result. There are only entries with the condition countryfr = 'US' in the internal table.` ).
    ENDIF.
    out->write( |\n| ).

    "Further SELECT statements that specify WHERE clauses
    SELECT * FROM zdemo_abap_flsch_ve_auth WHERE countryfr = 'US' INTO TABLE @DATA(itab3).
    SELECT * FROM zdemo_abap_flsch_ve_auth WHERE countryfr = 'DE' INTO TABLE @DATA(itab4).

    out->write( `SELECT statements that specify WHERE clauses:` ).
    IF itab3 IS NOT INITIAL AND itab4 IS INITIAL.
      out->write( `Expected result. Only entries with the condition countryfr = 'US' should be accessible.` ).
    ELSE.
      out->write( `Something went wrong with the example procedure. There should only be entries with the condition countryfr = 'US'.` ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
```

</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>


## Excursion: Authorization Control in RAP

- This section focuses on [authorization control](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_auth_control_glosry.htm) in the ABAP RESTful Application Programming Model (RAP). 
- The authorization control features safeguard your RAP business objects from unauthorized data access. 
- You can define authorization control in the [RAP behavior definition (BDEF)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_behavior_definition_glosry.htm) for each entity, which then needs to be implemented in the [RAP handler methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_handler_method_glosry.htm) of the [ABAP behavior pool (ABP)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbehavior_pool_glosry.htm). 
- In the BDEF, you can set authorization control for all [RAP BO operations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_operation_glosry.htm) of a specific entity or for particular RAP BO operations.
- For read operations on RAP business objects, you can utilize the CDS access control, which is automatically applied in managed scenarios. 
- For modify operations, dedicated authorization implementation options are available, such as the following: 
  - Global authorization restricts data access or the ability to perform certain operations for an entire RAP BO, regardless of individual instances. This can depend on user roles. For example, if a user is not allowed to delete following the authorization check, the method handling the delete operation in the ABAP behavior pool is not invoked. In doing so, it allows you to reject a request before it reaches any other method of the behavior handler classes.
  - Instance authorization, on the other hand, applies checks based on the state of an entity instance. Both global and instance authorizazion checks can be implemented simultaneously. The authorization checks can be implemented using authorization objects.
  - You can also implement authorization checks against incoming values using [RAP BO precheck](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_precheck_glosry.htm). This prevents unwanted values from reaching the transactional buffer. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbdl_precheck.htm).

The following code snippets provide a rudimentary code skeleton to get an idea about global and instance authorization implementations. For more detailed examples, refer to the links provided below.

Global authorization:

```abap
"------------ GLOBAL AUTHORIZATION ------------ 

"Example specification in the BDEF
...
authorization master ( global )
...

********************************************

"Example RAP handler method declaration in an ABAP 
"behavior pool to handle global authorization checks

...

METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR some_bdef RESULT result.

...

********************************************

"Example RAP handler method implementation 
"You can use the method to check the authorizations against 
"specified authorization objects, fields and activities.
"For example: create, value 01 / update and actions, value 02 /
"delete, value 06
"Note: A check against authorization fields and concrete values such as 
"ZAUTH_FIELD in the example is not relevant here. The authorization check
"is relevant for activities, independent of the state of the instances. 
"Therefore, the authorization field is specified with the DUMMY addition.

... 

METHOD get_global_authorizations.
    
    "Authorization check for create operations
    IF requested_authorizations-%create = if_abap_behv=>mk-on.

      AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ'
              ID 'ZAUTH_FIELD' DUMMY
              ID 'ACTVT'      FIELD '01'.

      result-%create = COND #( WHEN sy-subrc = 0 THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized ).
      "In case there is no authorization granted, you may want to add a message to the reported structure here.

    ENDIF.

    "Authorization check for update operations
    IF requested_authorizations-%update =  if_abap_behv=>mk-on.
      "Note: In draft scenarios, the Edit action is treated like an update operation.
      "So, you can add another condition such as ... OR requested_authorizations-%action-Edit =  if_abap_behv=>mk-on.

      AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ'
              ID 'ZAUTH_FIELD' DUMMY
              ID 'ACTVT'      FIELD '02'.

      result-%update  = COND #( WHEN sy-subrc = 0 THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized ).
      "result-%action-Edit    = COND #( WHEN sy-subrc = 0 THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized ).
      "In case there is no authorization granted, you may want to add a message to the reported structure here.
    ENDIF.

    "Authorization check for delete operations
    IF requested_authorizations-%delete = if_abap_behv=>mk-on.

      AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ'
              ID 'ZAUTH_FIELD' DUMMY
              ID 'ACTVT'      FIELD '06'.

      result-%delete = COND #( WHEN sy-subrc = 0 THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized ).
      "In case there is no authorization granted, you may want to add a message to the reported structure here.
    ENDIF.

  ENDMETHOD.
```

Instance authorization:

```abap
"------------ INSTANCE AUTHORIZATION ------------ 

"Example specification in the BDEF
"Both global and instance authorization can be specified.
...
authorization master ( global, instance )
...

********************************************

"Example RAP handler method declaration in an ABAP 
"behavior pool to handle instance authorization checks

...

METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
     IMPORTING keys REQUEST requested_authorizations FOR some_bdef RESULT result.

...

********************************************

"Example RAP handler method implementation 

... 

METHOD get_instance_authorizations.
    
   "Reading all the instances with the imported keys into an internal table
    READ ENTITIES OF some_bdef IN LOCAL MODE
      ENTITY some_entity
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(res)
      ...

    IF res IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT res INTO FINAL(r).
      
      ...

      "Covering only delete operations in the code snippet
      IF requested_authorizations-%delete = if_abap_behv=>mk-on.

        AUTHORITY-CHECK OBJECT 'ZAUTH_OBJ'
           ID 'ZAUTH_FIELD' FIELD r-some_field
           ID 'ACTVT'       FIELD '06'.

        FINAL(is_delete_allowed) = COND #( WHEN sy-subrc = 0 THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized ).
      ENDIF.

      ...

      APPEND VALUE #( ...
                      %delete = is_delete_allowed
                      ...
                    ) TO result.
    ENDLOOP.

ENDMETHOD.
```

More information and implementation examples: 
- For the complete picture, see the following topics in the Development guide for the ABAP RESTful Application Programming Model: 
  - [Authorization Control](https://help.sap.com/docs/abap-cloud/abap-rap/authorization-control?version=sap_btp)
  - [Developing Authorization](https://help.sap.com/docs/abap-cloud/abap-rap/developing-authorization?version=sap_btp)
- Find more information on the variety of syntax options in the BDEF in the ABAP Keyword Documentation [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbdl_authorization.htm)
- The BDEF syntax also allows to specify [authorization contexts](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbdl_authorization_context.htm).
- Using the [PRIVILEGED](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapeml_privileged.htm) mode, you can circumvent authorization checks performed by authorization objects that are called, for example, by global and instance authorization RAP handler methods.


<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information
- [Identity and Access Management (IAM)](https://help.sap.com/docs/btp/sap-business-technology-platform/identity-and-access-management-iam)
- [`AUTHORITY-CHECK OBJECT`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapauthority-check.htm)
- CDS Access Control
  - [SAP Help Portal](https://help.sap.com/docs/abap-cloud/abap-data-models/cds-authorization-concept)
  - [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_access_control.htm)
- [Tutorial: Create Authorization Model and App in SAP BTP, ABAP Environment](https://developers.sap.com/tutorials/abap-environment-authorization.html)
- [Authorization Control in RAP](https://help.sap.com/docs/ABAP_Cloud/f055b8bf582d4f34b91da667bc1fcce6/375a8124b22948688ac1c55297868d06.html?version=sap_btp)