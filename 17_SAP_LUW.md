<a name="top"></a>

# SAP LUW
- [SAP LUW](#sap-luw)
  - [Introduction](#introduction)
  - [Terms](#terms)
  - [SAP LUW Overview](#sap-luw-overview)
    - [Bundling Techniques](#bundling-techniques)
    - [Related ABAP Statements](#related-abap-statements)
  - [Concepts Related to the SAP LUW](#concepts-related-to-the-sap-luw)
  - [Notes on the SAP LUW in ABAP Cloud and RAP](#notes-on-the-sap-luw-in-abap-cloud-and-rap)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)

## Introduction

⚠️ The concept is relevant to both ABAP Cloud and classic ABAP, but some of the statements covered in the cheat sheet and the executable example are only relevant to [classic ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclassic_abap_glosry.htm).

This cheat sheet provides a high-level overview of the [SAP LUW](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensap_luw_glosry.htm) concept that deals with data consistency with a focus on SAP LUW-related statements, supported by an executable example to check the syntax in action.

When you run an application, you typically change data in a [transaction](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentransaction_glosry.htm), which may be temporarily stored in transactional buffers. 
This data may be temporarily inconsistent in the buffers, but it is important that the data be in a consistent state at the end of the transaction so that it can be saved to the database.

Consider the following example of transactional consistency: 
- A transaction consists of a money transfer from account A to account B, assuming the accounts are in the same bank and the data is stored in the same database.
- Such a transaction represents a logical unit. The transaction is successful when the money is debited from account A and credited to account B.
- This transaction may include other related tasks. Data may be loaded into a buffer, processed there, and become inconsistent during this time. It may also take a while for the whole process to be completed.
- However, at the end of the transaction, all data must be in a consistent state so that the database can be updated accordingly. Or, if errors occur during the transaction, it must be ensured that all changes can be reversed. It must not happen that money is credited to account B without also updating the totals of account A. In such a case, the previous consistent state must be restored.

> **💡 Note**<br>
> - This cheat sheet focuses on [classic ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenclassic_abap_glosry.htm). Hence, the links in this cheat sheet open topics in the ABAP Keyword Documentation for [Standard ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_abap_glosry.htm).
> - The SAP LUW concept is certainly relevant to ABAP Cloud, too. The [ABAP RESTful Application Programming Model (RAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenarap_glosry.htm) is the transactional programming model for [ABAP Cloud](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_cloud_glosry.htm). It comes with a well-defined transactional model and follows the rules of the SAP LUW. Find out more in this [blog](https://blogs.sap.com/2022/12/05/the-sap-luw-in-abap-cloud/).

## Terms

The following terms are related to the concept of the SAP LUW and try to give you some context about it:

- [Transaction](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentransaction_glosry.htm)
  - In a business context, a transaction describes a sequence of related and/or interdependent actions, such as retrieving or modifying data. 
  - The result of the transaction is a consistent state of data in the database.

- [Logical unit of work (LUW)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenluw_glosry.htm)
  - Describes the time interval at which one consistent state of the database is transitioned to another consistent state.
  - Follows an all-or-nothing approach: It ends either with a single and final commit, which saves the changed data in the database, or with a rollback, which undoes all changes and restores the consistent state before the changes (for example, in the case of an error during the LUW). Either all data changes are committed, or none at all.
  - For an [Application Server ABAP (AS ABAP)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenas_abap_glosry.htm), two types of LUWs come into play to achieve data consistency: Database LUW and SAP LUW (which is covered below).

- [Database LUW](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendatabase_luw_glosry.htm)
  - Also called database transaction.
  - Is an SAP-independent mechanism for transactional consistency in the database.
  - Describes an indivisible sequence of database operations concluded by a [database commit](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendatabase_commit_glosry.htm), that persists data to the database.
  - The [database system](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendatabase_system_glosry.htm) either executes the database LUW completely or not at all. If an error is detected within a database LUW, a [database rollback](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendatabase_rollback_glosry.htm) undoes all database changes made since the start of the database LUW.

- [Database commit](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendatabase_commit_glosry.htm)
  - Marks the end of a database LUW in which changed data records are written to the database. 
  - An important question for developers is how and when database commits and rollbacks are triggered (especially implicitly). 
  - In AS ABAP, database commits can be triggered implicitly as well as by means of explicit requests.
  - Find more information [here](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendb_commit.htm).

- Implicit database commits. Among others, implicit database commits are triggered by: 
  - Completing a [dialog step](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendialog_step_glosry.htm) in the context of dynpros
    - A dialog step describes the state of a [user session](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenuser_session_glosry.htm) between a [user action](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenuser_action_glosry.htm) on the user interface of a dynpro and the sending of a new [screen layout](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenscreen_glosry.htm), i.e. it covers logic implemented in the [PAI](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpai_glosry.htm) of the current dynpro and [PBO](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpbo_glosry.htm) of the following dynpro 
    - When the next screen is displayed, the program waits for a user action and does not occupy a [work process](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenwork_process_glosry.htm) during this time.
    - The next free work process is assigned to the program in the next dialog step.
    - A work process change requires and implicitly triggers a database commit.
  - Calling a [function module](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfunction_module_glosry.htm) in a [synchronous (sRFC)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensynchronous_rfc_glosry.htm) or [asynchronous remote function call (aRFC)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenasynchronous_rfc_glosry.htm)
    - This is when the current work process passes control to another work process or system. Exception: [Updates](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenupdate_glosry.htm). 
  - HTTP/HTTPS/SMTP communication executed using the [Internet Communication Framework (ICF)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenicf_glosry.htm)
  - [`WAIT`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapwait_up_to.htm) statements that interrupt the current work process 
  - Sending messages ([error messages](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenerror_message_glosry.htm), [information message](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abeninformation_message_glosry.htm), and [warning](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenwarning_glosry.htm))

- Explicit database commits. For example, database commits can be triggered explicitly in ABAP programs in the following ways:
  - Using the relevant database-specific [Native SQL](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennative_sql_glosry.htm) statement
  - Using the ABAP SQL statement [`COMMIT CONNECTION`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcommit_rollback_connection.htm)
  - Calling the function module `DB_COMMIT`, which encapsulates the corresponding Native SQL statement. 
  - Using the ABAP SQL statement [`COMMIT WORK`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcommit.htm). Note that the statement is particularly relevant to the SAP LUW as shown below. It also ends the SAP LUW. 
  
- [Database rollback](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendatabase_rollback_glosry.htm)
  - Like a database commit, ...
    - a database rollback marks the end of a database LUW. Here, all modifying database operations are undone until the beginning of the LUW. 
    - are triggered implicitly, as well as by explicit requests.
  - They are implicitly triggered, for example, by a [runtime error](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenruntime_error_glosry.htm) or a [termination message](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentermination_message_glosry.htm) (message of type `A`).
  - For example, explicit rollbacks are triggered by: 
    - Using the relevant database-specific Native SQL statement
    - Using the ABAP SQL statement [`ROLLBACK CONNECTION`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcommit_rollback_connection.htm) 
    - Calling the function module `DB_ROLLBACK`, which encapsulates the corresponding Native SQL statement. 
    - Using the ABAP SQL statement [`ROLLBACK WORK`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaprollback.htm). Note that the statement is particularly relevant to the SAP LUW as shown below. It also ends the SAP LUW. 

- [Work process](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenwork_process_glosry.htm)
  - As a component of an AS ABAP AS instance, work processes execute ABAP applications. Each ABAP program that is currently active requires a work process.
  - AS ABAP uses its own work processes to log on to the database system. Different types of work processes are available for applications, including dialog, enqueue, background, spool, and update work processes.
  - As mentioned earlier, in dialog processing, a work process is assigned to an ABAP program for the duration of a dialog step. An application program can be divided into several program sections and, in the case of dynpros, into several dialog steps that are processed sequentially by different work processes.
  - For example, in the context of a dynpro, when a dialog step that is waiting for user interaction completes, the work process is released (for another workload) and a new work process is assigned. The current workload is persisted and resources are released. This approach requires an (implicit) database commit that ends the database LUW. 
  - A work process can execute only a single database LUW. It cannot interfere with the database LUWs of other work processes.


<p align="right"><a href="#top">⬆️ back to top</a></p>


## SAP LUW Overview

For an [SAP LUW](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensap_luw_glosry.htm), the following aspects come into play: 

- Usually, an SAP LUW is started by opening a new [internal session](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abeninternal_session_glosry.htm). The execution of programming units can be distributed among several work processes.
- Database commits persist data in the database (especially implicit database commits when work processes are switched).
- The all-or-nothing rule applies: All database changes that occur during a transaction constitute a logical unit of work. They must be committed together, or rolled back together in the event of an error.
- This means that all database changes must be deferred and made in a final database commit (that is, in a single database LUW) at the end of a transaction - not in between.
- To ensure data consistency, all database changes are bundled, for example, in temporary tables or transactional buffers, and then, at the end of the transaction, all changes are written to the database together in a single work process, that is, in a single database LUW with a single database commit. This can be done directly using ABAP SQL statements at this stage, or using bundling techniques - special ABAP programming techniques.
  
Using the above bank transfer as an example: 
- At the end of the transaction, the new totals of both accounts are updated (money is debited from account A and credited to B).
- You cannot debit account A in one work process and then credit account B in a separate work process. When the work process changes, new totals would be available in one account, but not in the other. Consider the consequences if an error occurs and the new totals for account B cannot be updated, and so on. You would no longer be able to easily roll back the changes.
- Consider prematurely updating the database and notifying the users or processing the data while the logical unit has not been successfully completed.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Bundling Techniques
The following bundling techniques are available for classic ABAP. This means that programming units are registered in different work processes, but are executed by a single work process. All database changes are put into one database LUW, and all changes are committed in one final database commit.

**Using [update function modules](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenupdate_function_module_glosry.htm)**
- Are specially marked, i.e. the *update module* property is marked  
- Can be given specific attributes to determine the priority with which they are processed in the update work process
- Usually contain database modification operations/statements
- [`CALL FUNCTION ... IN UPDATE TASK`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcall_function_update.htm) statements are used to register the update function modules for later execution; the actual execution is triggered by a `COMMIT WORK` statement

- Example of a simple function module that has an importing parameter (a structure that is used to modify a database table). It simply shows a database modifying statement contained in a function module. The code alone does not distinguish it as an update function module. You can open the imported example function modules from the repository. They are marked as update function modules.
  ```abap
  FUNCTION zsome_update_fu_mod
    IMPORTING
      VALUE(values) TYPE some_dbtab.

    MODIFY some_dbtab FROM @values.

  ENDFUNCTION.
  ```

- Depending on your use case, you can run the update work process in several ways:
  - [Synchronous update](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensynchronous_update_glosry.htm): The calling program waits until the update work process has finished. The `COMMIT WORK` statement with the `AND WAIT` addition triggers a synchronous update in a separate update work process.
  - [Asynchronous update](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenasynchronous_update_glosry.htm): The calling program does not wait for the update work process to finish. The `COMMIT WORK` statement triggers an asynchronous update in a separate update work process.
  - [Local update](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlocal_update_glosry.htm): The update is performed immediately in the current work process in a separate internal session and not in a separate update work process, so that a synchronous update is always performed (regardless of whether `COMMIT WORK` is used with `AND WAIT` or not). By default, the local update is deactivated at the start of each SAP LUW. If required, you can activate local update for an SAP LUW using the [`SET UPDATE TASK LOCAL`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapset_update_task_local.htm) statement before registering the update function modules.


    ```abap
    "Synchronous update

    DATA(st_a) = VALUE some_type( ... ).

    ... 

    "Registering the update function module specified in uppercase letters
    CALL FUNCTION 'ZSOME_UPDATE_FU_MOD' IN UPDATE TASK 
        EXPORTING values = st_a.

    ...

    "Triggering the synchronous update
    COMMIT WORK AND WAIT.                              

    ***********************************************************************
    "Asynchronous update

    DATA(st_b) = VALUE some_type( ... ).

    ... 

    CALL FUNCTION 'ZSOME_UPDATE_FU_MOD' IN UPDATE TASK 
        EXPORTING values = st_b.

    ...

    "Triggering the asynchronous update
    COMMIT WORK.

    ***********************************************************************
    "Local update

    "Before update function modules are registered.
    SET UPDATE TASK LOCAL.

    DATA(st_c) = VALUE some_type( ... ).

    ... 

    CALL FUNCTION 'ZSOME_UPDATE_FU_MOD' IN UPDATE TASK 
        EXPORTING values = st_c.

    ...

    "The update will be synchronous no matter if you use the COMMIT WORK 
    "statement with or without the addition AND WAIT.
    COMMIT WORK.
    "COMMIT WORK AND WAIT.
    ```

> **💡 Note**<br>
> If a runtime error occurs during the update, the update work process executes a database rollback, and notifies the user whose program created the entries.

**Using [remote-enabled function modules](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenremote_enabled_fm_glosry.htm)**
- Also in this case, the bundling is done through function modules. 
- They are also specially marked as remote-enabled function modules.
- For example, you can use register them for later asynchronous execution in the background and through the [RFC interface](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenrfc_interface_glosry.htm) ([background Remote Function Call (bgRFC)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenbg_remote_function_glosry.htm)). With this technology, you can make calls in the same or different ABAP systems. 
- More information:
  - [SAP Help Portal documentation about RFC](https://help.sap.com/docs/ABAP_PLATFORM_NEW/753088fc00704d0a80e7fbd6803c8adb/4888068AD9134076E10000000A42189D)
  - [`CALL FUNCTION ... IN BACKGROUND UNIT`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcall_function_background_unit.htm)
  - Note the [background processing framework (bgPF)](https://help.sap.com/docs/abap-cloud/abap-concepts/background-processing-framework) as a successor technology 

**Using [subroutines](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensubroutine_glosry.htm)**
- Subroutines that are no longer recommended for use can be registered for later execution.

- They are registered with the [`PERFORM ... ON COMMIT`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapperform_on_commit.htm) statement. These subroutines are executed when a `COMMIT WORK` statement is called.
- An addition is available to control the order of execution.
- Similarly, a subroutine can be registered "on rollback" with [`PERFORM ... ON ROLLBACK`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapperform_on_commit.htm). These subroutines are executed when a `ROLLBACK WORK` statement is called.
- When executed: 
  - In the current work process, before update function modules.
  - When they are registered in an update function module with `ON COMMIT`, they are executed at the end of the update. This happens in the update work process for non-local updates, and in the current work process for local updates.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Related ABAP Statements

An SAP LUW is usually started by opening a new internal session. 
The statements to end an SAP LUW have already been mentioned above: [`COMMIT WORK [AND WAIT]`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcommit.htm) and [`ROLLBACK WORK`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaprollback.htm).

`COMMIT WORK [AND WAIT]`
- Closes the current SAP LUW and opens a new one.
- Commits all change requests in the current SAP LUW. 
- Among other things, this statement triggers ...
  - the processing of all registered update function modules. 
  - the update work process and the local updates in the current work process. 
  - a database commit for all currently open [database connections](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendatabase_connection_glosry.htm), which also terminates the current database LUW. 
- Note that `COMMIT WORK` triggers an asynchronous, `COMMIT WORK AND WAIT` a synchronous update.

`ROLLBACK WORK`
- Similar to `COMMIT WORK` statements, this statement closes the current SAP LUW and opens a new one.
- Among other things, this statement ...
  - causes all changes within a SAP LUW to be undone, that is, all previous registrations for the current SAP LUW are removed.
  - triggers a database rollback on all currently open database connections, which also terminates the current database LUW. 

> **💡 Note**<br>
> Notes on database connections:
> - The [database interface](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendatabase_interface_glosry.htm) uses the [standard connection](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_db_connection_glosry.htm) of the current work process to access the [standard database](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_db_glosry.htm) by default.
> - Optionally, database accesses can also be made by using [secondary connections](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensecondary_db_connection_glosry.htm) to [secondary databases](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensecondary_db_glosry.htm) or by using [service connections](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenservice_connection_glosry.htm) to the standard database. The secondary connections are usually used by technical components. For example, they are used for caches, traces, logs, and so on.
> - The implicit database rollback is performed on all database connections that are currently open.
> - Within the SAP LUW, database changes and commits are allowed on service connections or through secondary database connections.


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Concepts Related to the SAP LUW
The following concepts are related to the SAP LUW to ensure transactional consistency. They are not discussed in detail here. For more information, see the links.

**Authorization concept**
- In an SAP system, you need to protect data from unauthorized access by making sure that only those [authorized](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenauthorization_glosry.htm) to access it can see and modify it.
- Authorization to access data can be set. Before a user can perform certain operations in your application, you need to implement authorization checks.
- Since ABAP SQL statements do not trigger any [authorization checks](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenauthorization_check_glosry.htm) in the database system, this is even more important. Database tables may be accessed without restriction using these statements. Conversely, not all users in a system are authorized to access all data available to ABAP SQL statements.
- Thus, it is up to the programmer to ensure that each user who can call the program is authorized to access the data it handles.
- More information: 
  - [Authorizations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbc_authority_check.htm)
  - [`AUTHORITY-CHECK`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapauthority-check.htm)


**Lock concept**
- The database system automatically sets [database locks](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendatabase_lock_glosry.htm) when ABAP SQL statements are called to modify database table entries.
- These locks are implemented by automatically setting a lock flag, which can only be set for existing database table entries.
- After a database commit, these flags are removed.
- As a result, database locks are not available for more than one database LUW, which must be considered in the context of an SAP LUW, since multiple database LUWs may be involved. Therefore, the lock flags that are set in a transaction are not sufficient. For the duration of an entire SAP LUW, a lock on database entries must remain set.
- This is where the SAP lock concept comes into play, which is independent of the automatic database locks. It is based on [lock objects](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlock_object_glosry.htm).
- Lock objects ...
  - are [repository objects](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenrepository_object_glosry.htm) that are defined in the [ABAP Dictionary](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_dictionary_glosry.htm).
  - specify the database tables in which records are to be locked with a lock request. 
  - contain the key fields on which a lock is to be set. 
- When a lock object is created, two [lock function modules](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlock_function_module_glosry.htm) (`ENQUEUE_...` and `DEQUEUE_...`) are automatically generated. They are executed in a special enqueue work process. When a record is locked during a transaction (by the enqueue function module), a central lock table is filled with the table name and key field information. Unlike database locks, a locked entry in a lock object does not necessarily have to exist in a database table. Also, the locking must be done proactively, i. e. there is no automatic locking. You must make sure that the application implementation checks the lock entries. 
- At the end of an SAP LUW, all locks should be released, either automatically during the database update or explicitly when you call the corresponding dequeue function module.
- More information: 
  - [SAP Locks](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abensap_lock.htm)  
  - Note the information on the `CL_ABAP_LOCK_OBJECT_FACTORY` class that is related to this context [here](https://help.sap.com/docs/sap-btp-abap-environment/abap-environment/lock-objects). 

> **💡 Note**<br>
> RAP comes with its own implementation features to cover these concepts. See the topics [Authorization Control](https://help.sap.com/docs/SAP_S4HANA_CLOUD/e5522a8a7b174979913c99268bc03f1a/375a8124b22948688ac1c55297868d06.html) and [Concurrency Control](https://help.sap.com/docs/SAP_S4HANA_CLOUD/e5522a8a7b174979913c99268bc03f1a/d315c13677d94a6891beb3418e3e02ed.html) in the *Development guide for the ABAP RESTful Application Programming Model*.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Notes on the SAP LUW in ABAP Cloud and RAP

A limited set of ABAP language features is available in ABAP Cloud ([restricted ABAP language version](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenrestricted_version_glosry.htm)). 
The limitations include the fact that the above bundling techniques are not available. Note that the local update is enabled by default in ABAP Cloud. Find more information in this [blog](https://blogs.sap.com/2022/12/05/the-sap-luw-in-abap-cloud/).

In fact, RAP is the transactional programming model for ABAP Cloud.
And RAP comes with a well-defined transactional model and follows the rules of the SAP LUW. At the end of an SAP LUW in RAP, database modification operations should be performed in a final step in the [RAP late save phase](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlate_rap_save_phase_glosry.htm) by persisting the consistent data in the [RAP transactional buffer](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentransactional_buffer_glosry.htm) to the database.

There are RAP-specific [ABAP EML](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_eml_glosry.htm) statements for commit and rollback: 
- [`COMMIT ENTITIES`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcommit_entities.htm) implicitly triggers `COMMIT WORK`. Furthermore, `COMMIT ENTITIES` provides RAP-specific functionality with various additions. These EML statements implicitly enforce local updates with `COMMIT WORK`, or `COMMIT WORK AND WAIT` if the local update fails. Therefore, the update is either a local update or a synchronous update, but never an asynchronous update. 
- [`ROLLBACK ENTITIES`](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaprollback_entities.htm): Resets all changes of the current transaction and clears the transactional buffer. The statement triggers `ROLLBACK WORK`. 
- Find more information in the [ABAP cheat sheet about EML](08_EML_ABAP_for_RAP.md).

## More Information
- [The RAP Transactional Model and the SAP LUW](https://help.sap.com/docs/SAP_S4HANA_CLOUD/e5522a8a7b174979913c99268bc03f1a/ccda1094b0f845e28b88f9f50a68dfc4.html) (Development guide for the ABAP RESTful Application Programming Model)
- [The SAP LUW in ABAP Cloud](https://blogs.sap.com/2022/12/05/the-sap-luw-in-abap-cloud/) (blog)
- [SAP LUW in the ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensap_luw.htm)
- [Controlled SAP LUW](https://help.sap.com/docs/abap-cloud/abap-concepts/controlled-sap-luw) as an enhancement of the SAP LUW concept

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

After the import of the repository, proceed as follows: 
- Find the program in ADT using the search by choosing `CTRL + SHIFT + A`.
- Enter `zdemo_abap_sap_luw` and open the program. 
- Run the program by choosing `F8`.

> **💡 Note**<br>
> - The steps about how to import and run the code are outlined [here](README.md#-getting-started-with-the-examples). 
> - The SAP LUW is demonstrated using classic dynpros to provide a self-contained example (i.e., so as not to have more artifacts, service creation, etc.) that highlights the considerations regarding implicit database commits - without putting the spotlight on dynpros. Note that classic dynpros are outdated for application programs. New developments should use web-based UIs, such as SAPUI5 or Web Dynpro. 
> - Dynpros cannot be created in ABAP Cloud. As mentioned earlier, RAP is the transactional programming model for ABAP Cloud. It comes with a well-defined transactional model and follows the rules of the SAP LUW. See the links in the *More Information* section.
>  - The example ... 
>    - does not claim to include meaningful dynpros with meaningful dynpro sequences and is not intended to be a role model for proper dynpro design.   
>    - is not intended to solve concrete programming tasks. You should always work out your own solution for each individual case.
>    - is only intended to demonstrate a selection of keywords and visualize SAP LUW-related syntax in action on a high level. 
> - See notes on the executable example in the expandable section below. 

<br>
<details>
  <summary>Expand to see explanations of the executable example</summary>
  <!-- -->
  <br>
The example demonstrates the SAP LUW using dynpros and bundling techniques with update function modules and subroutines. In the dynpros, you can select various options that determine how the program runs. It covers the following aspects:

- Demonstrating synchronous update, asynchronous update, and local update triggered by `COMMIT WORK`, `COMMIT WORK AND WAIT`, and `SET UPDATE TASK LOCAL` using update function modules.
- Demonstrating the statements `PERFORM ... ON COMMIT` and `PERFORM ... ON ROLLBACK` using subroutines.



The selection options follow this pattern:

**First dynpro**:   
- The SAP LUW is started.
- The entries in a database table are displayed. There are four entries in total. 
- After you have selected an option to continue with either the update task, or the update task and perform a local update, or subroutines, an update function module or subroutine is registered to delete all entries from the database table.

**Second dynpro**: 
- You can create a new database table entry by making entries in input fields displayed on the dynpro. 
- When the program continues, it registers an update function module or subroutine that inserts the new entry into the database table.

**Third dynpro**: 
- The SAP LUW is ended. You have several options for ending the SAP LUW. 
- In addition to the `COMMIT WORK` and `COMMIT WORK AND WAIT` statements, you can use `ROLLBACK WORK` to roll back the changes. 
- Another option is to deliberately make the current SAP LUW fail. If a type A message is triggered, the SAP LUW is also terminated.

During program execution, logs are collected and eventually written to a database table (also using an update function module or subroutine). These logs document the progress of the transaction with various pieces of information. These include work process information, SAP LUW key retrieval, and transaction state retrieval (using methods of the `CL_SYSTEM_TRANSACTION_STATE` class).

If the program is not terminated immediately and the SAP LUW has ended, another program is called that displays a dynpro. 

**Fourth dynpro** (part of a new program that is started):
- The database table entries and logs are displayed. 
- If the transaction was successful, a single entry (the one created during the execution of the previous program) should be displayed for the modified database table, as well as the entries of the log table.
- Note that a helper class is available for this example. Methods perform various tasks, such as retrieving work process information.

**Notes on the various options for checking out the SAP LUW:**
  - **Asynchronous update** with `COMMIT WORK`: Immediately after the `COMMIT WORK` statement, a `SELECT` statement is executed, retrieving all the entries of the database table. In this case, the number of the retrieved entries should be the number of the original database table entries, i.e. 4 entries, and not 1, demonstrating the asynchronous update. The current number of records is displayed in a message. Result: The database table is deleted and a single new entry is added.
The log shows the value 1 for the transaction state after the update task is executed and the update function modules are called.
  - **Synchronous update** with `COMMIT WORK AND WAIT`: Immediately after the `COMMIT WORK AND WAIT` statement, a `SELECT` statement is executed to retrieve all the entries in the database table. In this case, there should be one entry instead of four to demonstrate the synchronous update. The current number of records is displayed in a message.
  - **Local update** using a `SET UPDATE TASK LOCAL` statement: Once the local update is enabled, it does not matter whether you choose `COMMIT WORK` or `COMMIT WORK AND WAIT` in the next step. It will be a synchronous update, so the number of current database table entries displayed in the message will be 1 in both cases. The log will show the value 1 for the transaction state after the `SET UPDATE TASK LOCAL` statement has been executed.
  - **Rolling back changes**: Although update function modules and subroutines are registered, none of them affect the database. All changes are rolled back. Result: The database table is not deleted, no new entry is created. The original content of the database table should be displayed.
  - **Causing a failure in the current SAP LUW**: An update function module intentionally includes a statement that causes a runtime error if not caught (zero division). All changes are rolled back implicitly. If the local update is active, you should be informed of the problem directly. The program is terminated. In this case, you can check the database table entries that remain unchanged. You can also use transaction ST22 to display the runtime error that occurred. In the case of a non-local update, you should receive a mail in the Business Workplace informing you of the problem in the SAP LUW. The original content of the database table should be displayed on the next screen. In this case, you can also check transaction ST22 for the runtime error.
  - **Terminating the program with an error message** of type A: This option only indicates that if such a message is generated, the program is terminated and all changes are implicitly rolled back. In this case, you may want to check the database table entries that remain unchanged.
- **Using subroutines**: 
  - Note that subroutines are considered obsolete and should no longer be used. This is to demonstrate the effect as a bundling technique in an SAP LUW. Selecting this option triggers the registration of subroutines for commit (to delete the database table entry, insert a newly created entry, insert entries in the log table) and rollback (this subroutine does nothing specific; it is just to demonstrate that the subroutine is called in the event of a rollback).
  - When you select the commit options, the subroutines registered with `ON COMMIT` are executed in the current work process.
  - Choosing `COMMIT WORK` or `COMMIT WORK AND WAIT` has the same effect: When these statements are called and a `SELECT` statement follows, the number of database table entries is 1 in both cases.
  - If the rollback option is selected, the subroutine registered with `ON ROLLBACK` is executed in the current work process.
  - The transaction state in the log is 1 for `ON COMMIT` or `ON ROLLBACK` when the corresponding subroutines are called.
  - Note that registered subroutines cannot have a parameter interface, so no parameters can be passed in this type of bundling. Therefore, data can only be passed through external interfaces, such as ABAP memory. In this example, the database table entry created is passed to and from ABAP memory using `EXPORT` and `IMPORT` statements. The subroutines themselves do not implement the writes themselves, but instead call methods of a class.
- The following aspects are valid for all selected options regarding the logs:
  - Before the commit is triggered (in the last PAI), the transaction state shows the value 0 for all retrieved transaction states.
  - The work process information may change due to the fact that database commits are triggered when completing a dialog step. So you might expect different numbers there, but not necessarily. The new free work process can also be the same as the one before it was freed. However, there will be no different work process information for the update. The numbers will be the same because the update is performed in a single work process.
  - Before calling the program that displays database entries and the log, the SAP LUW key is the same throughout the transaction. It does not change until a new SAP LUW is opened. See and compare the last entry for the SAP LUW key in the log that is retrieved for the program submitted.
</details>








