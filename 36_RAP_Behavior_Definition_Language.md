<a name="top"></a>

# RAP Behavior Definition Language

- [RAP Behavior Definition Language](#rap-behavior-definition-language)
  - [RAP-BO-Related Repository Objects in a Nutshell](#rap-bo-related-repository-objects-in-a-nutshell)
  - [RAP BDL Syntax](#rap-bdl-syntax)
    - [Behavior Definition Header](#behavior-definition-header)
    - [Entity Behavior Definitions](#entity-behavior-definitions)
      - [Entity Behavior Characteristics](#entity-behavior-characteristics)
      - [Entity Behavior Body](#entity-behavior-body)
  - [More Information](#more-information)


This ABAP cheat sheet highlights key features and syntax options of the [RAP Behavior Definition Language (BDL)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbdl_glosry.htm) for designing [RAP behavior definitions (BDEF)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_behavior_definition_glosry.htm).

> [!NOTE]  
> - A wide range of specification options is available. The ABAP cheat sheet illustrates a selection. It does not provide a comprehensive overview.
> - To get the complete picture, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL.html) and the [Development guide for the ABAP RESTful Application Programming Model](https://help.sap.com/docs/abap-cloud/abap-rap/abap-restful-application-programming-model).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## RAP-BO-Related Repository Objects in a Nutshell

This section highlights typical repository objects for a typical RAP BO setup, and how the BDEF fits in.

> [!NOTE]  
> What is a RAP BO?
> - A RAP BO represents a real-world business entity with specific data, characteristics, and functionality (its behavior).
> - Typically, a RAP BO involves:
>   - A standardized approach to modeling business data as CDS entities with relationships, forming a tree-like hierarchy of nodes. A RAP BO is based on a root entity (a special kind of CDS entity that serves as the top-level entity) and child entities usually linked in compositions, which are special kinds of associations. In simple cases, a RAP BO may involve just a root entity, while more complex scenarios include one or multiple child entities.
>   - Defining transactional behavior, which includes the functionality needed to work with the data. This can encompass standard operations like create, read, update, and delete (CRUD), as well as other nonstandard operations specific to the RAP BO.
>   - Implementing the transactional behavior.
>   - Exposing data and behavior as a service, allowing RAP BO consumers (such as UIs) to consume these services.


Typical RAP BO setups follow a layered approach, illustrated by the following table:

<table>

<tr>
<th> Layer </th> <th> Repository Objects </th> <th> Notes </th> 
</tr>

<tr>
<td rowspan="2"> 

Data modeling

 </td>

 <td> 

Database tables (or CDS table entities) 

 </td>

<td> 

Used for data persistence and as the basis of CDS entities.

 </td>
</tr>

<tr>
<td> 

Core Data Services (CDS)

 </td>

<td> 

- Core Data Services (CDS) are used to model RAP BO entities, with each RAP BO entity represented by its own CDS entity. 
- A tree-like hierarchy of entities is typically based on one CDS root entity, the top CDS entity which represents a CDS parent for further child entities. The CDS root entities are specified with a special syntax: [`define root view entity`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_define_root_view_v2.htm). 
- CDS entities are linked by defining relationships through associations or compositions. A composition relationship indicates a close dependency between entities, meaning a child entity relies on its parent entity. For example, using sales orders as example context. A sales order item (assumingly a child entity within a sales order BO) cannot exist without the corresponding sales order. If the sales order is deleted, the associated sales order item has to be deleted as well, along with any other related sales order items. This contrasts with association relationships, where parent and child entity data can exist independently.
- Find more information in the [ABAP Data Models guide](https://help.sap.com/docs/abap-cloud/abap-data-models/abap-data-models).

> [!NOTE]  
> - A related repository object is [CDS access control](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_access_control_glosry.htm). Here, you can define access rules for the involved CDS entities. In a RAP BO setup, CDS access control typically manages read access, while modification authorization (using authorization objects) is handled in behavior implementations.
> - [CDS annotations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_annotation_glosry.htm) represent metadata that can be added, for example, to control UI rendering and layout. You can add these annotations to the CDS entity's source code directly or outsource them to [CDS metadata extensions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_metadata_extension_glosry.htm), a further repository object. Find more information on CDS annotations [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENCDS_ANNOTATIONS.html).

 </td>
</tr>

<!-- -->

<tr>
<td rowspan="2"> 

RAP BO behavior

 </td>

 <td> 

RAP behavior definition (BDEF)

 </td>

<td> 

- Defines the behavior of a RAP BO and all its entities.  
- Behavior includes operations (such as create, update, delete, and others) and characteristics (which fields are read-only or mandatory, and so on).  
- Typically, the BDEF shares the same name as the root view entity to which it refers.  
- BDEFs use [RAP Behavior Definition Language (BDL)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbdl_glosry.htm) for the definitions.

 </td>
</tr>

<tr>
<td> 

ABAP behavior pool (ABP)

 </td>

<td> 

- Special [class pool](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_pool_glosry.htm), typically prefixed with `*BP`, that implements the behavior and contains the actual business logic.
- The actual implementation occurs in the [CCIMP include](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenccimp_glosry.htm) (_Local Types_ tab in ADT).

 </td>
</tr>

<!-- -->

<tr>
<td> 

Projections

 </td>

 <td> 

CDS projection view, 
RAP projection BDEF, 
ABAP behavior pool

 </td>

<td> 

- Projections introduce an optional layer (a [RAP projection business object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_projection_bo_glosry.htm)) that enables you to adapt the RAP BO's data model and functionality to tailor specific services for different [RAP BO consumers](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_consumer_glosry.htm), such as UIs or APIs. For example, target group A may access a UI with the full functionality, while target group B may use a UI with certain disabled functionalities, such as delete operations. 
- Projections involve creating artifacts like:
  - [CDS projection view](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_projection_view_glosry.htm): Represents a special CDS view entity that is based on another CDS view entity. CDS projection views adapt a CDS data model for service-specific use cases. For a projection BO, the root entity must be a CDS projection view. 
  - [RAP projection behavior definition](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_proj_bdef_glosry.htm): Created for the CDS projection view. Indicates that a base BDEF is projected. A projection BDEF builds on the base BDEF, reusing its operations and characteristics or limiting them to enable only a subset. Typical syntax elements are `use ...`, for example, `use create;`, `use update;`, `use action act;`, and so on.
  - A separate [ABAP behavior pool](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbehavior_pool_glosry.htm) for the RAP projection BDEF, if required.
- Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL_PROJECTION_BO.html).


 </td>
</tr>

<!-- -->

<tr>
<td rowspan="2"> 

Business service

 </td>

 <td> 

Service definition

 </td>

<td> 

Exposes RAP BO entities along with their data and behavior as a business service for consumers.

 </td>
</tr>

<tr>
<td> 

Service binding

 </td>

<td> 

Binds a service definition to a communication protocol and publishes it as a business service of [AS ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenas_abap_glosry.htm) (for example, a UI or [OData](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenodata_glosry.htm) serivce).

 </td>
</tr>

</table>

> [!TIP]  
> To create an OData UI service and all RAP service-related repository objects from scratch, you can use a wizard in ADT. Find more information [here](https://help.sap.com/docs/abap-cloud/abap-rap/odata-ui-service-from-scratch).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## RAP BDL Syntax  



The following sections illustrate the structure of BDEFs and syntax options.

Demo BDEF that highlights the structure:

```
// -------------- Behavior Definition Header --------------
managed implementation in class ZBP_R_DEMO_ABAP unique;
strict ( 2 );
with draft;

// -------------- Entity Behavior Definitions --------------
define behavior for ZR_DEMO_ABAP alias demo_abap
// -------------- Entity Behavior Characteristics --------------
persistent table zdemoabap
draft table zdemoabap_d
etag master LocalLastChangedAt
lock master 
total etag LastChangedAt
authorization master ( none )
late numbering
// -------------- Entity Behavior Body --------------
{
  field ( readonly ) ID, CalcResult, LocalCreatedBy, LocalCreatedAt, LocalLastChangedBy,
                     LocalLastChangedAt, LastChangedAt;

  field ( mandatory ) Number1, Number2, Operator;

  create;
  update;
  delete;

  validation validate on save { create; field Number1, Number2, Operator; }
  determination det_modify on modify { field Number1, Number2, Operator; }
  internal action calculation;

  draft action Resume;
  draft action Edit;
  draft action Activate optimized;
  draft action Discard;
  draft determine action Prepare
  {
    validation validate;
  }
  
  mapping for zdemoabap corresponding
    {
      ID                 = ID;
      Number1            = Number1;
      Number2            = Number2;
      Operator           = Operator;
      CalcResult         = calc_result;
      LocalCreatedBy     = LOCAL_CREATED_BY;
      LocalCreatedAt     = LOCAL_CREATED_AT;
      LocalLastChangedBy = LOCAL_LAST_CHANGED_BY;
      LocalLastChangedAt = LOCAL_LAST_CHANGED_AT;
      LastChangedAt      = LAST_CHANGED_AT;
    }
}
```

> [!NOTE] 
> - The syntax covered is not comprehensive and only provides a high-level overview. For example, it does not cover options related to interface, projection, abstract behavior definitions, or syntax related to BDEF extensions. 
> - For the complete picture, refer to the subtopics in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL.html).



### Behavior Definition Header 

<table>

<tr>
<th> Subject </th> <th> Code Snippet </th> <th> Notes </th> 
</tr>

<tr>
<td> 

Managed implementation type

 </td>

 <td> 

``` abap
managed implementation  
  in class zbp_demo_abap_rap_entity unique;
```

 </td>

<td> 

- _Managed_ and _unmanaged_ refer to two implementation types of RAP BOs. The key distinction lies in how the BO's transactional behavior and [transactional buffer](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentransactional_buffer_glosry.htm) are provisioned.
- The managed implementation is typically used for greenfield scenarios, where app development starts from scratch without any existing business logic.
- In a managed RAP BO, standard operations (CRUD) function out of the box, allowing applications to use them without custom development. This includes both the provisioning and handling of the transactional buffer, as well as save handling (except for the saving options for managed RAP BOs noted below).
- Essentially, you only need to specify `create`, `update`, and `delete` in the BDEF, and the managed RAP BO will automatically support transactional processing (syntax for enabling read operations is not available; read operations work out of the box as well). Additional business logic, such as authorization, actions, validations, and determinations, and more, can be implemented in the ABAP behavior pool.
- By using `implementation in class ... unique`, you can specify one or more implementation classes. Note that this specification is optional for managed RAP BOs since CRUD operations are implicitly supported. It is only necessary if your BO needs further transactional handling. The specification of `unique` is mandatory and indicates that each operation can be implemented only once.
- Find more information [here](https://help.sap.com/docs/abap-cloud/abap-rap/business-object-implementation-types).

 </td>
</tr>

<!-- -->

<tr>
<td> 

Unmanaged implementation type

 </td>

 <td> 

``` abap
unmanaged implementation 
  in class zbp_demo_abap_rap_entity_u unique;
```

 </td>

<td> 

- The unmanaged implementation type is typically used for brownfield scenarios where existing application logic needs to be embedded in the RAP world to leverage standardized RAP runtime orchestration.
- The unmanaged RAP BO provider must supply everything, meaning developers must provide the transactional buffer and implement all RAP BO operations.
- Find more information [here](https://help.sap.com/docs/abap-cloud/abap-rap/business-object-implementation-types).

 </td>
</tr>

<!-- -->

<tr>
<td> 

Saving options in managed RAP BOs

 </td>

 <td> 

``` abap
managed with additional save ...

managed with unmanaged save ...
```

 </td>

<td> 

- In managed RAP BOs, saving is automatically handled by default, eliminating the need for custom implementations. However, you can enhance or replace the default save sequence in managed RAP BOs. 
- Regardless of the approach, the save handling applies to the entire RAP BO. 
- The ABAP behavior pool requires the implementation of a local saver class, including the implementation of the saving mechanism in the `save_modified` method.
- You enhance the save sequence with `with additional save` for tasks like changing documents, raising business events, or logging applications.
- You replace the automatic saving process with your custom logic by using `with unmanaged save`. Note that you cannot specify the `persistent table` in this case.
- Additional syntax options (which can be combined) for both enhancements include:
  - `and cleanup ...`: This option requires the redefinition of the `cleanup` method in the saver class.
  - `with full data ...`: If specified, the full instance data is passed to the `save_modified` method in the saver class, which can help avoid extra read operations. By default, only the key values of RAP BO instances are passed.
- Note that you can specify `with additional save` and `with unmanaged save` not only in the BDEF header but also for each entity individually after `define behavior` (e.g., `define behavior for some_entity with unmanaged save`).
- Find more information [here](https://help.sap.com/docs/abap-cloud/abap-rap/save-options).

 </td>
</tr>

<!-- -->

<tr>
<td> 

Strict mode

 </td>

 <td> 

``` abap
strict(2);
```

 </td>

<td> 

- With strict mode enabled, additional syntax checks for RAP behavior definitions are activated.  
- Since the RAP framework provides the checks, no additional implementation is needed in the ABAP behavior pool.  
- It is recommended to use the most recent version, which is currently 2.  
- Find more information [here](https://help.sap.com/docs/abap-cloud/abap-rap/strict-mode).


 </td>
</tr>

<!-- -->

<tr>
<td> 

Draft handling

 </td>

 <td> 

``` abap
with draft; 
```

 </td>

<td> 

- Enables draft handling for the entire RAP BO, allowing users to save changes as drafts and resume editing later.
- Note that the transactional engine orchestrates draft handling, so developers do not need to manage how draft data is written to the draft database table.
- The `with collaborative draft;` notation also enables draft handling and allows multiple users to work on the same draft instance simultaneously.
- Find more information [here](https://help.sap.com/docs/abap-cloud/abap-rap/draft).

 </td>
</tr>

<!-- -->

<tr>
<td> 

ABP auxiliary classes

 </td>

 <td> 

``` abap
auxiliary class zcl_some_class;
```

 </td>

<td> 

- ABP auxiliary classes provide reusable methods for implementing behaviors, allowing you to outsource functionality and simplify concurrent work. They offer enhanced access rights to the referenced RAP BO, meaning you can use the `IN LOCAL MODE` addition with ABAP EML statements.
- You can specify one or more global ABP auxiliary classes. Multiple classes can be specified in a comma-separated list.  

 </td>
</tr>

</table>

> [!NOTE] 
> More syntax options are available in the BDEF header, including enabling BDEF extensions, privileged mode, defining authorization contexts, cross-BO transactional handling, and more. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL_BDEF_HEADER.html).


<p align="right"><a href="#top">⬆️ back to top</a></p>

### Entity Behavior Definitions

<table>

<tr>
<th> Subject </th> <th> Code Snippet </th> <th> Notes </th> 
</tr>

<tr>
<td> 

Entity behavior definition

 </td>

 <td> 

``` abap
define behavior for some_entity alias root
```

 </td>

<td> 

- A BDEF can include one or more entity behavior definitions that begin with `define behavior for`. Each definition corresponds to a different entity of the business object.
- You must specify an entity behavior definition for the root entity. Defining behaviors for child entities is optional.
- Syntax options: 
  - `alias some_name`: This optional alias allows for a clearer, more descriptive name. The alias is visible in the handler methods of the ABP and the [BDEF derived types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_derived_type_glosry.htm).
  - `external some_external_name`: This option specifies an alias for external use, which is exposed in the OData metadata.
  - `implementation in class ... unique`: Instead of specifying the ABAP behavior pool in the BDEF header, you can also specify it here for implementing the behavior of the referenced RAP BO entity. The previously mentioned additions for additional and unmanaged save are also applicable here.

 </td>
</tr>


</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


#### Entity Behavior Characteristics

Following the `define behavior for` specification, additional properties for a RAP BO entity can (or must) be specified.


<table>

<tr>
<th> Subject </th> <th> Code Snippet </th> <th> Notes </th> 
</tr>

<tr>
<td> 

Table specifications

 </td>

 <td> 

``` abap
persistent table some_table

draft table some_draft_table
```

 </td>

<td> 

- `persistent table`  
  - Specifies the [DDIC database table](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_db_table_glosry.htm) (or [CDS table entity](https://help.sap.com/docs/abap-cloud/abap-rap/using-table-entities-as-active-persistence)) for storing RAP BO data.  
  - Can only be specified for managed RAP BOs.  
  - Note that the transactional buffer serves as temporary storage for RAP BO instance data processed during a RAP transaction. For reads and modifications of persisted data, the data is loaded into the transactional buffer. At the end of a RAP transaction, changes in the buffer are either committed to the persistent table or rolled back.  
- `draft table`  
  - Specifies the database table for storing draft data in draft-enabled RAP BOs. Draft-enabled RAP BOs require two separate database tables: one for persistent data and another for draft instances.  
  - This table must reflect the fields from the underlying CDS entity. Alias field names must be used, if any. The types must be compatible, and a client field must be present. In cases of late numbering, an additional key field (`DRAFTUUID`, 16-character byte-like type) is required.  
  - Additionally, the draft table must include the draft admin entry `"%admin": include sych_bdl_draft_admin_inc;`. It represents technical information needed by the RAP transactional engine for draft handling.  
  - Note that the draft table should not be accessed directly using ABAP SQL for read or modify operations.

 </td>
</tr>

<tr>
<td> 

Concurrency control

 </td>

 <td> 

``` abap
//ETag for optimistic concurrency control
etag master some_etag_field
etag dependent by _Assoc
total etag some_total_etag_field

//Locking for pessimistic concurrency control
lock master
lock dependent by _Assoc
```

 </td>

<td> 

- Concurrency control manages concurrent access to the same data by different users. Two approaches to consider in RAP are:
  - Optimistic concurrency control: This approach allows multiple users to access data while avoiding inconsistencies caused by unintentional changes to already modified data. In RAP, the ETag field in the RAP behavior definition ensures this.
  - Pessimistic concurrency control: This approach involves [exclusive locking](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenexclusive_lock_glosry.htm) of data sets, preventing simultaneous modification by more than one user.
- Entity tag (ETag, `etag ...`) for optimistic concurrency control:
  - The ETag field specification is optional for each entity of the RAP BO. The `dependent` specification indicates that the ETag field of another entity is used for optimistic concurrency control.
  - This field logs modifications of an instance, using a timestamp or another unique identification value. It is particularly relevant when consuming RAP BOs via OData. For a modify operation to be accepted, the OData client must send an ETag value with each modify operation, which is compared to the stored ETag value to allow modification only when the values match.
  - Managed RAP BOs automatically manage optimistic concurrency control when the CDS annotation `@Semantics.systemDateTime.localInstanceLastChangedAt: true` is available. The ETag field's data type should be `utclong`, `timestamp`, or `timestampl`, and it is advisable to mark the field as read-only in the BDEF. In unmanaged RAP BOs, custom implementations for ETag field handling are required (for example, in the save sequence).
  - `total etag` is relevant and mandatory only for draft-enabled BOs. The value of the total ETag is particularly used for draft instances that are edited, and which have a corresponding active instance. When a draft instance is created, the active instance is locked exclusively and cannot be modified by another user. The exclusive lock remains for a (configurable) timespan, even if the ABAP session terminates. Once the lock expires, the optimistic lock phase begins. During this phase, users can resume the draft instance as long as the active instance remains unchanged. The total ETag value of the draft instance is compared to that of the active instance for resumability, allowing continuation when the values match.
  - It is recommended to use different fields for `total etag` and `etag master`. The specification must occur only after `lock master`. To support total ETag handling automatically in managed RAP BOs, the same type prerequisites apply as mentioned above. The required CDS annotation is `@Semantics.systemDateTime.lastChangedAt: true`.
- Locking for pessimistic concurrency control (`lock ...`):
  - The `lock master` specification must be applied to the root entity, and each child entity must be defined as a lock-dependent entity (`lock dependent by ...`).
  - In managed RAP BOs, the locking mechanism is automatically handled for update, delete, and create-by-association operations, as well as actions (with a `lock:none` specification available to prevent locking in that context). If you specify `lock master unmanaged`, the locking mechanism must be self-implemented in a dedicated RAP handler method.
  - Before changing data on the database, the corresponding data set receives a lock entry in the global lock table. When a request for a lock is made, this table is checked for an entry. If a lock already exists for the requested data set, the modify request is denied. Once the modification is committed (or rolled back, or the ABAP session is terminated), the lock entry is removed from the lock table.
- Find more information here:
  - [Optimistic Concurrency Control](https://help.sap.com/docs/abap-cloud/abap-rap/optimistic-concurrency-control)
  - [Pessimistic Concurrency Control](https://help.sap.com/docs/abap-cloud/abap-rap/pessimistic-concurrency-control-locking)


 </td>
</tr>

<tr>
<td> 

Numbering

 </td>

 <td> 

``` abap
early numbering

late numbering
```

 </td>

<td> 

- The concept of numbering in RAP is about assigning values to primary key fields.
- There are multiple options for handling the numbering of primary key fields, depending on when (early in the [RAP interaction phase](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_int_phase_glosry.htm) or late in the [RAP save sequence](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_save_seq_glosry.htm)) and by whom (RAP BO consumer, ABAP behavior pool, or framework).
- When: 
  - Early numbering: Final key values are assigned during a RAP create operation in the interaction phase.
  - Late numbering: Final key values are assigned during the RAP save sequence, specifically in the [RAP saver method](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_saver_method_glosry.htm) `adjust_numbers`.
- By whom:
  - External numbering: Key values come from the RAP BO consumer. For example, during a create operation, the RAP BO consumer specifies the key values just like other non-key field values.
  - Internal numbering: Key values are provided by the RAP BO provider. For example, in a create operation, the RAP BO consumer does not specify the key values in an EML create request. Instead, the RAP BO provider provides the keys.
- Variety of applicable numbering scenarios: 
  - External early numbering: 
    - Key values are provided by the RAP BO consumer, who must ensure that the keys are unique. Otherwise, the instance will be rejected during a save attempt.
    - Key fields should not be marked as read-only for create operations. It is advisable to define key fields with specific characteristics such as `... mandatory:create, readonly:update ...`.
  - Managed internal early numbering: 
    - The framework automatically creates the key without any custom development needed in the ABP. 
    - As a prerequisite, a [UUID](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenuuid_glosry.htm) scenario is in place, where the key is a 16-character byte-like type, which can hold UUID values. 
    - Typically, the key has the field-specific characteristics `readonly` in the BDEF, along with `numbering:managed`.
  - Unmanaged internal early numbering:
    - In unmanaged RAP BOs, key values are provided in create operation implementations (that is, the `create` method). Note that `early numbering` is only applicable for unmanaged, draft-enabled RAP BOs.
    - In managed RAP BOs and unmanaged, draft-enabled RAP BOs, unmanaged early numbering must be implemented in the ABP's handler method `FOR NUMBERING`.
    - The key typically has the field-specific characteristics `readonly` in the BDEF.
  - (Unmanaged internal) late numbering:
    - Late numbering is internal by default since no further interaction with the RAP BO consumer occurs in the late phase of the RAP save sequence.
    - It is available for both managed and unmanaged RAP BOs, but the numbering itself is unmanaged. It must be implemented in the `adjust_numbers` saver method.
- By default, when no specifications are provided, the numbering is external.
- `early numbering`:
  - Used for early unmanaged numbering in managed RAP BOs, and is also possible for unmanaged, draft-enabled RAP BOs. 
  - Typically, key fields are marked as read-only in the BDEF.
  - Requires implementation of key value assignment in the `FOR NUMBERING` handler method.  
- `late numbering`:
  - Used to enable late numbering for managed and unmanaged RAP BOs.
  - Requires implementation of key value assignment in the RAP saver method `adjust_numbers`.

> [!NOTE] 
> - Find more information on numbering [here](https://help.sap.com/docs/abap-cloud/abap-rap/numbering).
> - Find more information regarding draft-enabled RAP BOs and numbering [here](https://help.sap.com/docs/abap-cloud/abap-rap/unmanaged-internal-late-numbering).
> - Ensuring uniqueness for key values is essential. In scenarios where the framework does not handle uniqueness checks, developers must implement these checks explicitly. For example, such checks can be included in the precheck.

 </td>
</tr>

<tr>
<td> 

Authorization control

 </td>

 <td> 

``` abap
authorization master ( global ) 

authorization master ( instance ) 

authorization master ( global, instance ) 

authorization master ( none ) 

authorization dependent by _Assoc ...
```

 </td>

<td> 


- Authorization control in RAP restricts access to RAP BO data and/or protects it from unauthorized access.
- Various types of authorization control exist in RAP:
  - `global`: This refers to global authorization, which is independent of individual RAP BO instances. It checks user roles using authorization objects to determine if a user can create, update, delete, and so on.
  - `instance`: This refers to instance authorization, which depends on the state of a RAP BO instance. Both `global` and `instance` can be specified simultaneously.
  - `none`: This applies no authorization control.
- Authorization control is especially important for modification operations such as create (only for global authorization), create-by-association, update, delete, and certain action types. Note that read operation authorization is generally governed by CDS access control.
- At this stage, authorization control is declared using `authorization master`, defining the default for the entity. However, more specific options for authorization control can be set in the entity behavior body (for example, `authorization : update` or `authorization : none` for modification operations or actions). This allows for finer-grained authorization control. For example, while you might specify `authorization master ( global )` for the entity, you could also define an action in the entity behavior body with `authorization : none`, indicating that this action does not require an authorization check despite the global authorization default.
- The root entity must specify an `authorization master`. Other RAP BO entities can either be `authorization master` or, for authorization-dependent entities, `authorization dependent by`.
- Specifying global and/or instance authorization control with `authorization master` requires implementing dedicated RAP handler methods (`FOR GLOBAL AUTHORIZATION` and `FOR INSTANCE AUTHORIZATION`).

 </td>
</tr>

</table>

> [!NOTE]  
> Further syntax options are available such as for RAP change documents ([`changedocuments`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL_CHANGE_DOCUMENTS.html)). 

<p align="right"><a href="#top">⬆️ back to top</a></p>


#### Entity Behavior Body

The entity behavior body holds definitions of the transactional behavior of RAP BO entities. The definitions are enclosed in curly brackets `{ ... }`.


<table>

<tr>
<th> Subject </th> <th> Code Snippet </th> <th> Notes </th> 
</tr>

<tr>
<td> 

Field-specific characteristics

 </td>

 <td> 

``` abap
field ( readonly ) field_a, field_b, field_c; 
field ( readonly : create ) field_d;
field ( readonly : update ) field_e; 
field ( mandatory ) field_f; 
field ( mandatory : create ) field_g; 
field ( mandatory : update ) field_h;
field ( suppress ) field_i; 
field ( features : instance ) field_j; 
```



 </td>

<td> 

- You can provide RAP BO fields with certain characteristics regarding access and provision. 
- It is possible to combine certain characteristics (with restrictions identified by syntax checks) within parentheses (for example, `field ( mandatory : create, readonly : update) field_k;`).
- The field characteristics are static. Only the `features : instance` variant represents dynamic field characteristics.
- Typically, key fields are specified with `readonly` or `readonly : update`.
- Notes on the specifications:  
  - `readonly`: Values for the fields must not be created or updated by the RAP BO consumer (for example, key field values are provided solely by the RAP BO provider in internally managed numbering scenarios). By using `readonly : create` or `... update`, you explicitly specify a field as readonly during create or update operations.  
  - `mandatory`: Values for the fields must be provided before persisting the instance. By using `mandatory : create` or `... update`, you explicitly specify a field as mandatory for create or update operations. In external numbering scenarios, a typical specification combination for key fields is `mandatory : create` and `readonly : update`.  
  - `suppress`: Used to remove a field from BDEF derived types and all RAP APIs.  
  - `features : instance`: Defines access restrictions for fields based on the RAP BO instance state. When specified, the restrictions must be implemented in the `FOR INSTANCE FEATURES` handler method. For example, you might want to ensure that a field is mandatory or readonly when a RAP BO instance has a specific state (for example, when a field has a certain value).

 </td>
</tr>

<tr>
<td> 

Managed internal numbering

 </td>

 <td> 

``` abap
field ( numbering : managed ) key_field;
```

 </td>

<td> 

- Defines managed internal numbering for a RAP BO key field.  
- This specification applies only to managed RAP BOs.  
- When creating instances, the framework automatically assigns unique values to the specified fields. These fields must be the primary key in the underlying CDS data model and have a 16-character byte-like type to hold UUID values.  
- Typically, the key fields are specified as `readonly` to prevent RAP BO consumers from passing key values, enabling a full internal numbering scenario. However, there are cases where you may choose not to use the `readonly` characteristic, allowing RAP BO consumers to provide values for the key fields. This _optional external numbering_ use case means the framework supplies values only if the consumer provides initial values.  
- For custom numbering, the `early numbering` specification is required, which requires implementing the numbering in a RAP handler method.

 </td>
</tr>

<tr>
<td> 

Standard operations (CUD operations) 

 </td>

 <td> 

``` abap
create; 
update; 
delete; 

//Multiple additions are available (but the operation itself can 
//only be specified once). Combinations are possible. Examples:
internal create;
create ( features : global, precheck, authorization : none ); 
create { default function GetDefaultsForCreate; }                  
update ( features : instance ); 
delete ( authorization : update );
```

 </td>

<td> 

- Specifying CUD operations is optional. If you do not specify `update`, you cannot perform update operations for RAP BO instances.
- In a managed RAP BO, these operations (along with read operations) are automatically handled, requiring no custom implementation. This differs from unmanaged RAP BOs, where you must implement the entire logic in dedicated RAP handler methods.
- Various additions can be specified (unless stated otherwise, these additions apply to all CUD operations):
  - `internal`: Defines operations as internal.
  - `features : instance`: Enables dynamic feature control for update or delete (not create).
  - `features : global`: Enables global feature control.
  - `precheck`: Enables precheck.
  - `authorization : none`: Excludes operations from authorization checks.
  - `authorization : update`: Delegates authorization control to the update operation's authorization check implementation. This applies only to delete operations.
  - `{default function ...}`: Defines a [RAP default values function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_defaulting_glosry.htm) for the create operation only, which sets default field values on the user interface.

> [!NOTE] 
> - The read operation is always implicitly enabled for each entity listed in a BDEF and cannot be explicitly declared (there is no `read` syntax).
> - Delete operations on RAP BO instances of the parent entity in managed RAP BOs also delete associated child entity instances that are in a composition relationship.

 </td>
</tr>

<tr>
<td> 

Actions as non-standard operations 

 </td>

 <td> 


``` abap
action act1;
internal action act2;
static action act3;
repeatable action act4;

action ( features : global ) act5;
action ( features : instance ) act6;
action ( precheck ) act7;
action ( authorization : global ) act8;
action ( authorization : instance ) act9;
action ( authorization : update ) act10;
action ( authorization : none ) act11;
action ( lock : none ) act12;
action act13 parameter SOME_ENTITY;
action act14 parameter SOME_ENTITY result [1] $self;
action act15 deep table parameter SOME_ENTITY;
action act16 deep result selective [1] SOME_ENTITY;
action act17 external 'SomeName';
action act18 {default function SomeFuncName;}

factory action act19 [1];
default factory action act20 [1];
save(...) action act21;
```

> [!NOTE]  
> - Different kinds and flavors of actions can be specified, and numerous syntax additions are available. The code snippet shows a selection only.
> - Combinations are possible. Make sure that you get the complete picture in the ABAP Keyword Documentation as not all options are listed in the ABAP cheat sheet.

 </td>

<td> 

- Actions are self-implemented in dedicated handler methods and represent non-standard operations for modifying RAP BO instance data.
- Notes on syntax options: 
  - When neither the `factory` nor `save` specifications are used (as in `act1`), the action is categorized as non-factory, meaning its primary purpose is to modify rather than create instances.   
  - `static`: Defines a static action not bound to any RAP BO instance but related to the entire RAP BO entity.
  - `repeatable`: Defines a repeatable, non-factory action that can execute multiple times on the same RAP BO instance within one request.
  - `internal`: Restricts access to the action to within the business object implementation.
  - `external`: Allows specification of an alias name for external use.
  - `lock : none`: Prevents locking on the RAP BO instance. 
  - `features : instance`: Enables dynamic feature control for actions, allowing the action to be available only if certain preconditions regarding the instance state are met.
  - `features : global`: Enables global feature control for actions, allowing availability only if specific global, RAP BO-external preconditions are met.
  - `precheck`: Enables a precheck for actions to prevent unwanted changes from reaching the transactional buffer.
  - `authorization : none`: Excludes authorization checks.
  - `authorization : update`: Delegates authorization control to the check implemented for the update operation. 
  - `authorization : global`: Replaces the specified authorization control in the authorization  master entity with global authorization checks.
  - `authorization : instance`: Replaces the specified authorization control in the authorization  master entity with instance authorization checks.
  - `... parameter ...`: 
    - Defines an input parameter for actions. 
    - The input parameter can be flat or deep (using the specifications `deep` and `deep table`).
    - For flat parameters, `SOME_ENTITY` represents a CDS abstract entity or a DDIC type.
    - For deep parameters (structures or tables), `SOME_ENTITY` represents an [abstract BDEF](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_abstract_bdef_glosry.htm) defined with `with hierarchy`.
  - `... result ...`: 
    - Defines an output parameter for actions.
    - Used to store the result. When specified, the handler method in the ABP includes the `RESULT` addition in the signature.
    - There are several syntax options after `result`, including: 
      - `selective`: Returns only parts of the result, such as just the keys. When used, the corresponding handler method includes the input parameter `REQUEST ...` to mark the desired fields.
      - `[...]`: Defines the cardinality of the output parameter. Examples: `[0..1]`, `[1]`, `[0..*]`, `[1..*]`.
      - `$self`: Indicates that the result type matches the entity type.
      - `entity some_entity`: Specifies that the result type corresponds to another CDS view entity.
  - `default function`: Defines a [RAP default values function](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_defaulting_glosry.htm) for the action, which defaults input parameter values. As a prerequisite, the action must specify an input parameter.    
  - `factory`: Defines a factory action 
    - Used to create RAP BO instances (including child entities).
    - Can be either instance-bound (for copying specific field values from an instance) or static (for creating instances with predefined default values).
    - Output parameters are not permitted here.
    - Cardinality specifications: `[1]` (creates exactly one entity instance), `[0..1]` (creates none or exactly one entity instance), `[0..*]` (creates none or any number of instances), `[1..*]` (creates any number of instances, but at least one).
    - Besides `internal` and `static`, factory actions can be specified with `static default factory ...`. This addition is evaluated by consuming frameworks, such as OData, which use the default factory action as the standard action in certain scenarios.
  - `save( ... )`: 
    - Defines save actions callable only during specific RAP saver methods.
    - The parentheses can include `finalize` and/or `adjustnumbers`.

 </td>
</tr>

<tr>
<td> 

Functions as non-standard operations 

 </td>

 <td> 

``` abap
function func1 result [0..*] $self;

static function func2 result [1] SOME_ENTITY;

function func3 parameter SOME_ENTITY result [1] $self;
```

> [!NOTE]  
> - Different kinds and flavors of functions can be specified, and numerous syntax additions are available. The code snippet shows a selection only.
> - Combinations are possible. Make sure that you get the complete picture in the ABAP Keyword Documentation as not all options are listed.

 </td>

<td> 

- Functions are self-implemented in dedicated handler methods, designed for non-standard operations that return information, such as calculations or read operations, without locking or modifying data.
- You can also define static, internal, repeatable, and default functions, specify external names and input parameters.
- It is possible to define [RAP key functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_key_function_glosry.htm), which accept values of an alternative key as input and return matching entity instances. These functions are commonly used in [cross-BO association](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_cross_bo_assoc_glosry.htm) contexts. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL_KEYFUNCTION.html).
- Specifying an output parameter is mandatory. Refer to the actions section for details on specification options, including cardinality.

 </td>
</tr>

<tr>
<td> 

Operations for associations

 </td>

 <td> 

``` abap
association _Assoc;

association _Assoc { create; }

association _Assoc
  {
    create;
    link action some_link_action;
    unlink action some_unlink_action;
    inverse function some_inv_func;
  }
```

> [!NOTE]  
> Numerous syntax options are available in that context. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL_ASSOCIATION.html).

 </td>

<td> 


- The syntax allows operations along association paths and impacts association targets. You can enable both standard operations (such as create-by-association and read-by-association) and non-standard operations (such as link and unlink actions or inverse functions) for associations.
- As a prerequisite, you must define behavior for the association target using `define behavior ...`.
- Read-by-association operations are automatically enabled when you specify the association, for example, using `association _Assoc;`.
- The statement `association _Assoc { create; }` enables create-by-association as well as read-by-association operations. Note that `create` can be enhanced with additional specifications, such as precheck, feature control, authorization, and more. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL_ASSOC_STAND_OPS.html).
- The supported [non-standard operations for associations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL_ASSOC_NONSTAND_OPS.html) include:
  - Link action: Associates RAP BO instances with other instances using foreign key settings.
  - Unlink action: Disconnects two associated RAP BO instances by removing the value of the foreign key field.
  - Inverse Function: Receives the values of the foreign key fields from the association target instances as input and provides the corresponding association source instances as output. Explicit output parameters are optional.

 </td>
</tr>

<tr>
<td> 

Draft actions

 </td>

 <td> 

``` abap
draft action Activate optimized;
draft action Discard;
draft action Edit;
draft action Resume;

draft determine action Prepare;

draft determine action Prepare
  { determination ...;
    validation ...; }

```

 </td>

<td> 

- Draft actions, available only for draft-enabled RAP BOs, manage the state of draft and active instances during their lifecycle.
- The implementation is implicitly provided, requiring no additional implementation in the ABP. However, the draft actions `activate`, `discard`, `edit`, and `resume` support the optional addition `with additional implementation`. When specified, you can add a custom implementation for the respective `FOR MODIFY ... ACTION` method in the ABP.
- Notes on the draft actions:
  - `Edit`: Copies an active instance to the draft database table.
  - `Activate`: Transfers the content of the draft database table to the persistent database table and clears the draft table. This method call implicitly executes the draft determination action `Prepare`. For better performance, it is advisable to use the optional addition `optimized`, which reduces the number of determinations and validations executed.
  - `Discard`: Deletes draft instances from the draft database table.
  - `Resume`: Is automatically executed whenever a draft instance, whose exclusive lock has expired, is modified. It re-establishes the lock for the corresponding entity instance in the active database table.
  - `draft determine action Prepare`: Executes the determinations and validations (only those specified with `on save`) assigned to it in the behavior definition. `draft determine action Prepare;` denotes that it is possible not to define any determinations or validations.
- Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL_DRAFT_ACTION.html).


 </td>
</tr>

<tr>
<td> 

Validations

 </td>

 <td> 

``` abap
validation val1 on save { create; update; }
validation val2 on save { delete; }
validation val3 on save { field some_field; }
validation val4 on save { delete; field some_field; }
```

 </td>

<td> 


- Validations check the consistency of RAP BO instances based on trigger conditions. A called validation can reject inconsistent instance data from being saved and return messages (via failed and reported entries) to the RAP BO consumer.
- They are automatically called during the save sequence (indicated by the addition of `on save`) when trigger conditions are met.
- Trigger conditions may include modification operations (create, update - only in conjunction with create -, delete) or specific field modifications (that is, when particular field values change during create and update operations). If multiple trigger conditions are specified, the evaluation occurs if at least one condition is met.
- Validations are implemented in the `FOR VALIDATE` RAP handler method.
- When inconsistent RAP BO instance data is rejected, the entire transactional buffer is rejected, including instances without inconsistencies. This "all or nothing" approach ensures that a final commit to the database occurs only if all data is consistent. Therefore, inconsistencies must be resolved, or the transactional buffer must be rolled back.

 </td>
</tr>

<tr>
<td> 

Determinations

 </td>

 <td> 

``` abap
determination det1 on save { create; }
determination det2 on save { delete; }
determination det3 on save { field some_field; }

determination det4 on modify { create; }
determination det5 on modify { field some_field; }
```

 </td>

<td> 


- Determinations are used to modify RAP BO instances based on trigger conditions. A determination can be used for calculations and data modifications. 
- They are automatically triggered when conditions are met. 
- Trigger conditions include modify operations (create, update - only together with create in case of `on save` -, and delete) or specific field modifications (that is, when specific field values change during create and update operations). If multiple trigger conditions are specified, the determination is executed if at least one condition is satisfied.
- There are two types of determinations based on the program flow stage: 
  - `on modify`: Is executed immediately after data changes occur in the transactional buffer, making the result available during the transaction.
  - `on save`: Is executed at the end of a transaction during the save sequence when changes from the transactional buffer are persisted to the database.
- Determinations are implemented in `FOR DETERMINE` RAP handler methods.
- Note that the execution order of multiple specified determinations is arbitrary.

 </td>
</tr>

<tr>
<td> 

Determine actions

 </td>

 <td> 

``` abap
 determine action det_act
  {
    determination det1;
    determination ( always ) det2;
    validation val;
  }
```

 </td>

<td> 

- Determine actions execute assigned determinations and validations on request.  
- Only `on save` determinations and validations can be assigned. `on modify` determinations cannot be assigned.  
- By default, assigned determinations and validations are only executed if trigger conditions are met. You can use the `always` addition to ensure they are executed regardless of trigger conditions.  
- Execution order: First determinations, then validations. The order among determinations and validations is independent of the specification order.  
- After `determine action`, you can specify authorization-specific additions (such as `authorization : none`).  
- Note that the draft determine action `Prepare` corresponds to these determine actions.

 </td>
</tr>

<tr>
<td> 

RAP business events

 </td>

 <td> 

``` abap
event evt1;
event evt2 parameter SOME_ENTITY;
event evt3 deep parameter SOME_ENTITY;

managed event evt4 on evt1 parameter SOME_ENTITY;

event evt5 for side effects;
```

 </td>

<td> 


- A RAP business event typically represents information about the outcome of a CUD operation. 
- RAP business events enable asynchronous communication between an event provider and an event consumer. An event consumer that subscribes to events can receive and process them. The information passed, known as the event payload, always includes the instance key and, if necessary, additional details. 
- To pass information, you can define an optional input parameter `parameter ...`. `SOME_ENTITY` refers to a CDS abstract entity. The syntax `deep parameter ...` is also available, requiring `SOME_ENTITY` to be an abstract BDEF specified with `with hierarchy`.
- RAP business events are typically triggered in ABPs during the RAP late save, using the ABAP EML statement `RAISE ENTITY EVENT`. For managed RAP BOs, it is advisable to define the BO with `with additional save` and raise business events in the `save_modified` method.
- Special types of RAP business events: 
  - RAP derived events (`managed event`): These events are defined in relation to an existing RAP business event, allowing for a redefined payload. They are automatically raised when the referenced event is triggered.
  - RAP business events for side effects (`... for side effects`): These events are used solely for an [RAP event-driven side effect](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_event_sideef_glosry.htm), meaning a RAP side effect is initiated by this business event.
- Note that once a RAP business event is created and raised, you must define an event binding to map the event to an event type.

 </td>
</tr>

<tr>
<td> 

RAP side effects

 </td>

 <td> 

``` abap
side effects {

    // Side effect with single source property
    field a affects field b;

    $self affects field y;

    // Side effect with multiple source properties and trigger action
    determine action someDetermineAction
    // trigger field group
      executed on field c,
                  field d,
                  field _Assoc.e
      affects field f;

    // Side effect that triggers the reload of feature control
    field g affects permissions(field h);

    // Side effect on an action with operation control and messages
    action act1 affects field *,
                       permissions(action act2),
                       messages;

    // Side effect for refreshing data of an entity
    action act3 affects field i, entity _Assoc;

    event evt affects field j;

  }
```

 </td>

<td> 

- RAP side effects define interdependencies among RAP BO properties that trigger a reload of affected properties in the user interface.  
- No custom implementations are required.  
- Multiple side effects can be summarized within curly brackets, separated by semicolons (`;`).  
- Notes on selected trigger properties:  
  - `field ... affects`: When the defined field changes on the user interface, the side effect is triggered, and the specified targets are reloaded.  
  - `$self affects`: When the current RAP BO entity undergoes a CUD operation, the side effect is triggered, and the specified targets are reloaded. Note that the targets may include fields from associated entities but cannot include fields from the current entity.  
  - `action ... affects`: When the action is executed on the user interface, the side effect is triggered, and the specified targets are reloaded.  
  - `determine action ...`: Defines a side effect that triggers a determine action. When any defined source following `executed on` changes, the determine action is executed, and the specified targets are reloaded.  
  - `event`: Defines a business event for side effects. When the defined event occurs, a side effect is triggered, and the specified targets are reloaded.  
  - Regarding the targets (`affects ...`):  
    - `field`: The specified field is reloaded when a side effect is triggered. One or more fields can be indicated. `field *` means all fields of the same instance are reloaded. Fields from other entities can be accessed via the association path (`_Assoc.e`).  
    - `entity`: Triggers a determine action when anything in the specified entity changes.  
    - `permissions`: Reloads feature and authorization control. Syntax options include fields (`permissions(field a)`), actions (`permissions(action act)`), standard operations (`permissions(create)`), and all previously mentioned for associated entities (e.g., `permissions(_Assoc.e)` or `permissions(create _Assoc)`).  
    - `$self`: Reloads the own entity.  
    - `entity`: Reloads the specified entity. Grouping options are available, such as `... entity (_Assoc1, _Assoc2) ...` and `... entity _Assoc1.(_ItemAssoc1, _ItemAssoc2) ...`.  
    - `message`: Reloads all reported messages.

 </td>
</tr>

<tr>
<td> 

Type Mapping

 </td>

 <td> 

``` abap
mapping for demo_dbtab_root
{
  KeyFieldRoot = key_field;
  DataFieldRoot = data_field;
  CharFieldRoot = char_field;
  DecFieldRoot = dec_field;
}
```

 </td>

<td> 

- `mapping for` maps data types defined in CDS for a RAP business object to non-matching types. For example, it is used in behavior definitions to connect database field names to CDS element names.
- If the field names in the DDIC structure and the current data model are identical, no mapping is needed. Otherwise, it is mandatory and will trigger a syntax check warning. In particular, for a managed RAP BO, the field names in the RAP persistent table must align with the corresponding RAP BO entity names. If they do not match, a type mapping is necessary to avoid a syntax check warning.
- Syntax variants are available including `control` and `corresponding`. Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL_TYPE_MAPPING.html).

 </td>
</tr>


</table>

<p align="right"><a href="#top">⬆️ back to top</a></p>


## More Information

- [Development guide for the ABAP RESTful Application Programming Model](https://help.sap.com/docs/abap-cloud/abap-rap/abap-restful-application-programming-model)
- ABAP Keyword Documentation:
  - [RAP BDL](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENBDL.html)
  - [Feature Tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENRAP_FEATURE_TABLE.html)
  - [CDS annotations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENCDS_ANNOTATIONS.html)
- [ABAP Data Models guide](https://help.sap.com/docs/abap-cloud/abap-data-models/abap-data-models)

<p align="right"><a href="#top">⬆️ back to top</a></p>