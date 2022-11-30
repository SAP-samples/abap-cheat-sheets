<a name="top"></a>

# ABAP for RAP: Entity Manipulation Language (ABAP EML)

- [ABAP for RAP: Entity Manipulation Language (ABAP EML)](#abap-for-rap-entity-manipulation-language-abap-eml)
  - [Excursion: EML in the Context of RAP](#excursion-eml-in-the-context-of-rap)
  - [ABAP Behavior Pools (ABP)](#abap-behavior-pools-abp)
    - [RAP Handler Classes and Methods](#rap-handler-classes-and-methods)
    - [RAP Saver Class and Saver Methods](#rap-saver-class-and-saver-methods)
  - [BDEF Derived Types](#bdef-derived-types)
    - [Components of BDEF Derived Types](#components-of-bdef-derived-types)
  - [EML Syntax](#eml-syntax)
    - [EML Syntax for Modifying Operations](#eml-syntax-for-modifying-operations)
    - [EML Syntax for Reading Operations](#eml-syntax-for-reading-operations)
    - [Persisting to the Database](#persisting-to-the-database)
    - [EML Statements in ABAP Behavior Pools](#eml-statements-in-abap-behavior-pools)
  - [Excursion: Using Keys and Identifying RAP BO Instances in a Nutshell / RAP Concepts](#excursion-using-keys-and-identifying-rap-bo-instances-in-a-nutshell--rap-concepts)
  - [Further Information](#further-information)
  - [Executable Examples](#executable-examples)

## Excursion: EML in the Context of RAP

[ABAP Entity Manipulation Language](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenaeml_glosry.htm) (or EML for short) is a subset of ABAP that allows you to access the data of [RAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenarap_glosry.htm) business objects in an ABAP program.
The following points are meant to touch on RAP-related buzzwords like
*RAP business objects* and others for setting the context:

-   RAP business objects (RAP BO)
    -   A RAP BO is based on a special, tree-like hierarchical structure
        of [CDS
        entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_entity_glosry.htm "Glossary Entry")
        of a data model
    -   Such a structure of entities consists of [parent
        entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenparent_entity_glosry.htm "Glossary Entry")
        and [child
        entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenchild_entity_glosry.htm "Glossary Entry")
        that are themselves defined using [CDS
        compositions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_composition_glosry.htm "Glossary Entry")
        and [to-parent
        associations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abento_parent_association_glosry.htm "Glossary Entry").
    -   The top parent entity of a [CDS composition
        tree](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_composition_tree_glosry.htm "Glossary Entry")
        is the root entity that represents the business object. With a
        large composition tree, RAP BOs can be fairly complex. Or they
        can be very simple by just consisting of one root entity alone.
    -   Note: There is a special syntax for the CDS root view
        entity of a RAP BO: [`define root view
        entity`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_define_root_view_v2.htm)
-   [CDS behavior
    definition](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_behavior_definition_glosry.htm "Glossary Entry")
    (BDEF)
    -   RAP BOs are described by the definitions specified in a special
        [DDIC](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_glosry.htm "Glossary Entry")
        artifact: CDS behavior definition (BDEF)
    -   A BDEF defines the [RAP business object
        behavior](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_behavior_glosry.htm "Glossary Entry")
        (i. e. the transactional behavior of a RAP BO)
    -   Transactional behavior means a BDEF defines [behavior
        characteristics](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_entity_properties_glosry.htm "Glossary Entry")
        and [RAP BO
        operations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_operation_glosry.htm "Glossary Entry") i.
        e. [RAP BO standard
        operations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_standard_operation_glosry.htm "Glossary Entry")
        ([CRUD
        operations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencrud_glosry.htm "Glossary Entry")),
        [non-standard
        operations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_nstandard_operation_glosry.htm "Glossary Entry")
        like specific [RAP
        actions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_action_glosry.htm "Glossary Entry")
        and
        [functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_function_glosry.htm "Glossary Entry"),
        and more.
    -   There are many other things that can be included impacting the
        RAP BO behavior like [RAP feature
        control](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_feature_control_glosry.htm "Glossary Entry"),
        for example, defining which data is mandatory and which is
        read-only, or
        [determinations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_determination_glosry.htm "Glossary Entry")
        and
        [validations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_validation_glosry.htm "Glossary Entry").
    -   BDEFs use [Behavior Definition
        Language](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbdl_glosry.htm "Glossary Entry")
        (BDL) for the definitions. Find more information on the topic
        and various options to define the transactional behavior in
        section [BDL for Behavior
        Definitions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbdl.htm)
        in the ABAP Keyword Documentation.
-   Transactional buffer and implementation types
    -   A BDEF defines the behavior of a RAP BO and, thus, how to handle
        its data. This data is available in the [RAP transactional
        buffer](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentransactional_buffer_glosry.htm "Glossary Entry").
    -   It is a storage for a RAP BO's data that is used and worked on
        during a [RAP
        LUW](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_luw_glosry.htm "Glossary Entry").
    -   This data includes [RAP BO
        instances](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_instance_glosry.htm "Glossary Entry")
        (i. e. concrete data sets of an entity). This is where EML
        enters the picture: EML is used to access this data in the
        transactional buffer.
    -   Currently, there are two kinds of RAP BOs:
        [managed](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmanaged_rap_bo_glosry.htm "Glossary Entry")
        and [unmanaged RAP
        BOs](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenunmanaged_rap_bo_glosry.htm "Glossary Entry").
    -   Managed and unmanaged are implementation types that are also
        specified in the BDEF.
    -   The implementation type determines the [RAP BO
        provider](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_provider_glosry.htm "Glossary Entry"), i.
        e. how the transactional buffer is provided and how the behavior
        of a RAP BO is implemented.
-   Managed RAP BOs:
    -   The managed RAP BO provider fully or partly provides the
        transactional buffer and RAP BO behavior (for standard
        operations only). In this case, the developers need not cater
        for the transactional buffer and implement the standard
        operations. This implementation is mostly relevant for
        greenfield scenarios when starting from scratch.
    -   Example: Regarding CRUD operations in managed RAP BOs,
        developers need not cater for an implementation at all. The
        standard operations work out of the box. For example, in case of
        an update operation, RAP BO instance data that is to be updated
        is automatically read into the transactional buffer, and then
        updated accordingly there. Finally, when triggering the saving,
        the updated instance in the transactional buffer is
        automatically saved to the database without any custom
        development needed.
    -   The transactional buffer is provided, too. You do not need to
        create the buffer yourself.
    -   [Note:] Usually, the behavior of a RAP BO requires some
        additional implementations also in the context of managed RAP
        BOs. For example, non-standard operations or feature controls
        must be self-implemented in [ABAP behavior
        pools](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbehavior_pool_glosry.htm "Glossary Entry")
        (see the details further down).
-   Unmanaged RAP BOs:
    -   Everything must be provided by the [unmanaged RAP BO
        provider](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenunmanaged_rap_bo_prov_glosry.htm "Glossary Entry"), i.
        e. the transactional buffer and all RAP BO operations must be
        provided or self-implemented by developers in an ABAP behavior
        implementation
    -   Unmanaged RAP BOs are, for example, relevant for brownfield
        scenarios, i. e. in scenarios in which business logic is already
        available and should be embedded in the RAP world.
-   ABAP behavior implementation in an ABAP behavior pool (ABP)
    -   An [ABAP behavior
        pool](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbehavior_pool_glosry.htm "Glossary Entry")
        is a special [class
        pool](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclass_pool_glosry.htm "Glossary Entry")
        for an ABAP behavior implementation that implements the
        unmanaged RAP BO provider based on definitions in a BDEF. The
        class pool's name is specified in the BDEF.
    -   The [global
        class](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_class_glosry.htm "Glossary Entry")
        of a behavior pool does not implement the behavior itself. It is
        basically empty. The behavior implementation is coded in local
        [RAP handler
        classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_handler_class_glosry.htm "Glossary Entry")
        and a [RAP saver
        class](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_saver_class_glosry.htm "Glossary Entry")
        in the [CCIMP
        include](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenccimp_glosry.htm "Glossary Entry")
        of the behavior pool. These classes are called by the [RAP
        runtime
        engine](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_runtime_engine_glosry.htm "Glossary Entry")
        when the RAP BO is accessed. This is touched on in more detail
        further down.
    -   Usually, saver classes are not needed in managed RAP BOs (except
        for special variants of managed RAP BOs which are not touched on
        here). Local handler classes are, as mentioned above, usually
        needed in managed RAP BOs if implementations are required that
        go beyond standard operations.
    -   [Note:] In more complex scenarios, with RAP BOs that
        consist of many entities, you can define behavior pools for
        individual entities by adding the syntax to the [`define
        behavior
        for`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbdl_define_beh.htm)
        notation. There is not a saver class for each entity but only
        one saver class for the BO as a whole. Any number of behavior
        pools can be assigned to a BDEF allowing applications a
        structuring into multiple units.

There are more artifacts and concepts related to RAP that go way beyond
the scope of this cheat sheet. For example, a RAP BO can be exposed as a
[business
service](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbusiness_service_glosry.htm "Glossary Entry")
to be accessed from outside [AS
ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenas_abap_sys_environ_glosry.htm "Glossary Entry")
and consumed. A [RAP BO
consumer](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_consumer_glosry.htm "Glossary Entry")
is either the [RAP transactional
engine](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_transac_engine_glosry.htm "Glossary Entry")
that handles requests from outside the AS ABAP or, from inside AS ABAP,
an ABAP program using ABAP EML (which this cheat sheet and the examples
focus on).

<p align="right">(<a href="#top">back to top</a>)</p>

## ABAP Behavior Pools (ABP)

As mentioned above, you can access RAP BO data from inside AS ABAP using
EML. Among other things, EML allows you to read or modify RAP BOs by
accessing the RAP BO data (the RAP BO instances) in the transactional
buffer and trigger the persistent storage or reset changes. More
precisely, when EML statements are executed, the calling of [RAP handler
methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_handler_method_glosry.htm "Glossary Entry")
is triggered to access the transactional buffer of a RAP BO. As
mentioned, for unmanaged RAP BOs or unmanaged parts of managed RAP BOs,
the handler methods that are called are part of an ABAP behavior pool.

The global class of an ABP has the addition [`FOR BEHAVIOR OF
bdef`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapclass_for_behavior_of.htm)
to the definition while `bdef` stands for the name of the BDEF.
This class is usually empty.

The implementation is done in local classes in the CCIMP include. There,
two kinds of local classes are to be defined and implemented that are
related to the RAP BO's runtime: one or more [handler
classes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_handler_class_glosry.htm "Glossary Entry")
to implement the RAP BO behavior (in RAP handler methods) during the
[RAP interaction
phase](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_int_phase_glosry.htm "Glossary Entry")
(the data reading and modification phase) and a [saver
class](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_saver_class_glosry.htm "Glossary Entry")
to implement the [RAP save
sequence](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_save_seq_glosry.htm "Glossary Entry")
(in [saver
methods](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_saver_method_glosry.htm "Glossary Entry")
to save data from the transactional buffer to the database).

<p align="right">(<a href="#top">back to top</a>)</p>

### RAP Handler Classes and Methods

-   One or more handler classes implement the RAP interaction phase. For
    modularization purposes, one behavior pool can define multiple
    handler classes. For example, each entity can have its own handler
    class, or individual handler classes can be defined to distinguish
    between reading and changing RAP BO entities.
-   A handler class inherits from class
    `CL_ABAP_BEHAVIOR_HANDLER`.
-   These classes are implicitly `ABSTRACT` and `FINAL`
    since instantiating and calling only happens through the RAP runtime
    engine.
-   [ADT](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenadt_glosry.htm "Glossary Entry")
    helps you create the classes and methods (and basically the ABP as
    such) when creating the BDEF. A quick fix is available that creates
    the method definitions and a skeleton of the implementations
    automatically.

Example: Handler class definition
``` abap
CLASS lhc_root DEFINITION INHERITING FROM cl_abap_behavior_handler.
...
ENDCLASS.
```
-   Handler method definitions include the additions `... FOR ... FOR
    ...` followed by the kind of operations. There are various
    options depending on the RAP BO operation.
-   Depending on the definition in the BDEF, there might be more ABAP
    words with dedicated method parameters. For example, an action might
    be defined with a result parameter, hence, the method must be
    defined with the addition `RESULT` and a parameter.
-   The `FOR MODIFY` handler method can handle multiple entities
    and operations, i. e. not only create but also update or delete
    might be integrated in the method definition. However, it might be
    useful to split the handler method into separate methods for better
    readability.
-   See more details on the handler method definitions in the topic
    [`METHODS, FOR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmethods_for_rap_behv.htm).

Example: Handler method definitions
``` abap
"Create
METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE bdef.

"Read: Specifying a read result is mandatory.
METHODS read FOR READ
      IMPORTING keys FOR READ bdef RESULT result.

"Action: action name is preceded by the BDEF name and a tilde after FOR ACTION
METHODS some_action FOR MODIFY
      IMPORTING keys FOR ACTION bdef~some_action.
```

**Parameters of Handler Methods**

-   The handler method definition contains RAP-specific additions like
    `FOR MODIFY`, `FOR CREATE` or `FOR READ` as
    well as mandatory or optional additions like `RESULT` that
    are followed by parameters.
-   Nearly all parameters are typed with [BDEF derived
    types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_derived_type_glosry.htm "Glossary Entry")
    that have special RAP-related components as touched on further down.
-   The parameters' names can be chosen freely. This is also true for
    the method names except for some predefined names.
-   Each handler method must have at least one importing parameter. The
    addition `IMPORTING` is optional since it is used
    implicitly. In most cases, the whole instance or just the key values
    of an instance are imported.
-   All handler methods have changing parameters that are usually not
    explicitly specified in the definition but implicitly used. The
    addition `CHANGING` is not needed. In most cases, these are
    [RAP response
    parameters](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_response_param_glosry.htm "Glossary Entry").
    The following image shows the F2 information in ADT for the create
    handler method.
    ![RAP_handler_method_parameters](./files/rap_handler_method_parameters.png)
-   The response parameters `mapped`, `failed` and
    `reported` (the names are predefined) can be considered as
    containers for information - information a RAP BO consumer is
    provided with by a RAP BO provider, for example, an SAP Fiori app
    displays an error message if something went wrong. The availability
    of the parameters depends on the handler method used (e. g.
    `mapped` is only available for operations creating
    instances).
    -   `mapped`: Used to provide mapping information on RAP BO
        instances, for example, which key values were created for given
        content IDs (
        [`%cid`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_cid.htm)).
    -   `failed`: Information for identifying the data set for
        which an error occurred in a RAP operation
    -   `reported`: Used to exchange error messages for each
        entity defined in the BDEF and [not related to a specific
        entity](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_other.htm).
    -   Example: Technically, the `reported` parameter is a
        [deep
        structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendeep_structure_glosry.htm "Glossary Entry")
        containing, for example, the messages of the root entity and
        child entities. For example, if a create operation fails for a
        RAP BO instance of the root entity, a message, information about
        the instance key and other things can be included in this
        parameter which is passed to a RAP BO consumer. You could
        imagine that such an error message is displayed on an SAP Fiori
        UI if something goes wrong to inform the user.


<p align="right">(<a href="#top">back to top</a>)</p>

### RAP Saver Class and Saver Methods

-   A RAP saver class implements the [RAP save
    sequence](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_save_seq_glosry.htm "Glossary Entry").
    A saver class is usually only available in unmanaged RAP BOs (except
    for special variants of managed RAP BOs that are not outlined here).
-   The saver class is implicitly `ABSTRACT` and `FINAL`
    since the instantiating and calling only happens through the RAP
    runtime engine.
-   A saver class can be defined in the CCIMP include of an ABAP
    behavior pool. It includes the definitions and implementations of
    RAP saver methods.
-   The saver methods consist of a set of predefined methods having
    predefined names. Some of them are mandatory to implement, some are
    optional. The
    [`adjust_numbers`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensaver_adjust_numbers.htm)
    method is only available in [late
    numbering](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_late_numbering_glosry.htm "Glossary Entry")
    scenarios.
-   A saver class inherits from class
    `CL_ABAP_BEHAVIOR_SAVER`. The saver methods are
    declared by redefining predefined methods of the superclass. They
    implicitly have response parameters.
-   In contrast to RAP handler methods, saver methods do not have data
    of RAP BO instances as import parameter. Therefore, instance data
    must be handled via the transactional buffer when self-implementing
    the saver methods.
-   All saver methods are called after at least one successful
    modification of data in the current RAP LUW, for example, by
    triggering the save sequence using a [`COMMIT
    ENTITIES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcommit_entities.htm)
    statement.
-   Find more information on RAP saver methods
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabp_saver_class.htm).

Example: Definition of a RAP saver class and saver methods
``` abap
CLASS lsc_bdef DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    "For final calculations and data modifications involving all
    "BOs in the current RAP LUW
    METHODS finalize REDEFINITION.

    "Checks the consistency of the transactional buffer before
    "the save method saves data to the database
    METHODS check_before_save REDEFINITION.

    "Preliminary IDs are mapped to final keys. Only for late numbering.
    METHODS adjust_numbers REDEFINITION.

    "Saves the current state of the transactional buffer to the database
    METHODS save REDEFINITION.

    "Clear the transactional buffer
    METHODS cleanup REDEFINITION.
    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.
```

<p align="right">(<a href="#top">back to top</a>)</p>

## BDEF Derived Types

The operands of EML statements and parameters of handler and saver
methods are mainly special messenger tables for passing data and
receiving results or messages, i. e. the communication between a RAP BO
consumer and the RAP BO provider using EML consists (in most cases) of
exchanging data stored in internal tables that have special ABAP types -
[BDEF derived
types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_derived_type_glosry.htm "Glossary Entry").
These types are tailor-made for RAP purposes.

As the name implies, the types are derived by the [ABAP runtime
framework](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_runtime_frmwk_glosry.htm "Glossary Entry")
from CDS entities and their behavior definition in the BDEF. With these
special types, a type-safe access to RAP BOs is guaranteed.

You can create internal tables (using [`TYPE TABLE
FOR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptype_table_for.htm)),
structures (using [`TYPE STRUCTURE
FOR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptype_structure_for.htm))
and data types with BDEF derived types. For all operations and behavior
characteristics defined in the BDEF, types can be derived.

The syntax uses - similar to the method definitions mentioned before -
the addition `FOR` followed by the operation and the name of an
entity (and, if need be, the concrete name, e. g. in case of an action
defined in the BDEF).

Each BDEF derived type can be categorized as
[input](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_input_der_type_glosry.htm "Glossary Entry")
or [output derived
type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_output_der_type_glosry.htm "Glossary Entry")
according to its use as importing or exporting parameters in methods of
RAP BO providers. In most cases, structures of type `TYPE STRUCTURE
FOR` can be considered as serving as [work
area](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenwork_area_glosry.htm "Glossary Entry")
and line type of the internal tables. However, there are also structured
derived types that do serve as types for handler method parameters.

The response parameters `mapped`, `failed` and
`reported` have dedicated derived types: [`TYPE RESPONSE
FOR`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abaptype_response_for.htm).
They are deep structures containing the information for the individual
entities of the RAP BO. The components of these structures are internal
tables of appropriate types with `TYPE TABLE FOR`.

Examples for BDEF derived types:

``` abap
"Data objects with input derived types (entity = name of a root entity)

"For an EML create operation

DATA create_tab TYPE TABLE FOR CREATE entity.

"For an update operation

DATA update_tab TYPE TABLE FOR UPDATE entity.

"Type for create-by-association operations specifying the name of the entity
"and the association

DATA cba_tab TYPE TABLE FOR CREATE entity\_child.

"For an action execution; the name of the action is preceded by a tilde

DATA action_imp TYPE TABLE FOR ACTION IMPORT entity~action1.

"Data objects with output derived types

"For a read operation

DATA read_tab TYPE TABLE FOR READ RESULT entity.

"For an action defined with a result

DATA action_res TYPE TABLE FOR ACTION RESULT entity~action2.

"Examples for structures and types

DATA create_wa TYPE STRUCTURE FOR CREATE entity.

"For permission retrieval

DATA perm_req TYPE STRUCTURE FOR PERMISSIONS REQUEST entity.

"For retrieving global features

DATA feat_req TYPE STRUCTURE FOR GLOBAL FEATURES RESULT entity.

"Type declaration using a BDEF derived type

TYPES der_typ TYPE TABLE FOR DELETE entity.

"Response parameters

DATA map TYPE RESPONSE FOR MAPPED entity.
DATA fail TYPE RESPONSE FOR FAILED entity.
DATA rep TYPE RESPONSE FOR REPORTED entity.
```
> **💡 Note**<br>
> Some of the derived types can only be created and accessed in implementation classes.

<p align="right">(<a href="#top">back to top</a>)</p>

### Components of BDEF Derived Types

Many of the BDEF derived types contain components of CDS entities like
key and data fields that retain their original line type, for example, a
messenger table typed with `TYPE TABLE FOR CREATE`. Certainly,
if an instance is to be created, key and field values of a RAP BO
instance are of relevance.

Yet, all BDEF derived types contain special RAP components serving a
dedicated purpose. The names of these RAP components begin with
`%` to avoid naming conflicts with components of the CDS
entities. The following image shows the F2 information of a BDEF derived
type containing the `%` components and fields from the CDS
entity.

![BDEF_derived_types](./files/bdef_derived_types.png)

Some of the `%` components are [component
groups](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomponent_group_glosry.htm "Glossary Entry")
summarizing groups of table columns under a single name. In doing so,
they simplify the handling of derived types for developers. For example,
the component group
[`%data`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_data.htm)
contains all primary key and data fields of a RAP BO entity (actually,
by containing the keys, it also contains the component group
[`%key`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_key.htm)
in the case above). The F2 information in ADT helps you find out about
the available components in a variable. The image below shows the
details of `%data` when clicking the `derived type`
link in the first ADT F2 information screen.

![BDEF_derived_type_components](./files/bdef_derived_type_components.png)

The availability of `%` components depends on definitions in the
BDEF. Their availability also depends on more criteria, for example, the
scenario. For example, the component
[`%pid`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_pid.htm)
that represents a preliminary ID for a RAP BO instance is only available
in [late
numbering](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_late_numbering_glosry.htm "Glossary Entry")
scenarios. The draft indicator
[`%is_draft`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_is_draft.htm)
is only available in the context of
[draft](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbdl_with_draft.htm).

Find more details on the available components in section [Components of
BDEF Derived
Types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_comp.htm).

Bullet points on selected `%` components:

-   [`%cid`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_cid.htm)
    -   A string to define a content ID.
    -   Content IDs are used as a unique and preliminary identifier for
        RAP BO operations in which instances are created and especially
        in cases where the key values of RAP BO instances are not yet
        determined
    -   Assume that you create a RAP BO instance with an EML create
        request and the key value has not yet been determined. In the
        same request - a save has not yet been triggered - an update is
        requested for this RAP BO instance. Using the content ID, it is
        guaranteed that the update operation happens for the desired
        instance. For this purpose, derived types for operations like
        update or delete include the component
        [`%cid_ref`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_cid_ref.htm)
        to refer to the content ID `%cid` as the name implies.
    -   Note: You should always fill `%cid` even if not
        needed. The specified content ID is only valid within one ABAP
        EML request.
-   [`%key`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_key.htm)/[`%tky`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_tky.htm)
    -   Both are component groups summarizing all primary keys of a RAP
        BO instance.
    -   Where possible, it is recommended that you use `%tky`
        instead of `%key`. `%tky` includes
        `%key` and also the draft indicator
        `%is_draft`. When using `%tky` in non-draft
        scenarios, you are prepared for a later, potential switch to a
        draft scenario. In doing so, you can avoid lots of adaptations
        in your code by manually adding the indicator.
-   [`%control`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_control.htm)
    -   Component group that, for example, contains the names of all key
        and data fields of a RAP BO instance which indicate flags.
    -   Used to get information on which fields are provided or set a
        flag for which fields are requested by RAP BO providers or RAP
        BO consumers respectively during the current transaction.
    -   For this purpose, the value of each field in the
        `%control` structure is of type
        `ABP_BEHV_FLAG`. For the value setting,
        you can use the structured constant `mk` of interface
        `IF_ABAP_BEHV`. Note that the technical
        type is `x length 1`.
    -   Example: If you want to read data from a RAP BO instance and
        particular non-key fields in `%control` are set to
        `if_abap_behv=>mk-off`, the values of these fields
        are not returned in the result.

<p align="right">(<a href="#top">back to top</a>)</p>

## EML Syntax

The focus is here on selected EML statements. These statements can be
fairly long and various additions are possible. Find more information on
the EML statements
[here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeneml.htm).

### EML Syntax for Modifying Operations

The modifying operations covered include the standard operations (using
the additions
[`CREATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_op.htm),
[`CREATE
BY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_op.htm),
[`UPDATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_op.htm),
and
[`DELETE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_op.htm)
and non-standard operations (actions) using the addition
[`EXECUTE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_op.htm).
All EML statements for the mentioned operations begin with
[`MODIFY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities.htm).
The following commented code snippets demonstrate the
[short](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_short.htm)
and [long
form](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entities_long.htm)
of EML `MODIFY` statements.

Create operation for creating new instances of a RAP BO entity:

``` abap
"Declaration of data objects using BDEF derived types

DATA: cr_tab        TYPE TABLE FOR CREATE root_ent,    "input derived type
      mapped_resp   TYPE RESPONSE FOR MAPPED root_ent, "response parameters
      failed_resp   TYPE RESPONSE FOR FAILED root_ent,
      reported_resp TYPE RESPONSE FOR REPORTED root_ent.

"Input derived type for the EML statement is filled using the VALUE operator
"Assumption: key_field is the key field having type i,
"field1 and field2 are data fields with character-like data type.
"Specify %cid even if not used or of interest; it must be unique within a RAP LUW

cr_tab = VALUE #(
        ( %cid   = 'cid1' key_field = 1
          field1 = 'A'    field2    = 'B' )
        ( %cid = 'cid2'
          "Just to demo %data/%key. You can specify fields with or without
          "the derived type components
          %data = VALUE #( %key-key_field = 2
                          field1         = 'C'
                          field2         = 'D' ) ) ).

"EML statement, short form
"root_ent must be the full name of the root entity, it is basically the name of the BDEF

MODIFY ENTITY root_ent
  CREATE "determines the kind of operation
  FIELDS ( key_field field1 field2 ) WITH cr_tab   "Fields to be respected for the
                                                   "input derived type and the input
                                                   "derived type itself
  MAPPED mapped_resp          "mapping information
  FAILED failed_resp          "information on failures with instances
  REPORTED reported_resp.     "messages
```

> **💡 Note**<br>
> -   Addition [`FIELDS ( ... ) WITH`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_fields.htm):
    This field selection option specifies which fields are to be
    respected for the operation. The derived type, i. e. an internal
    table containing the concrete RAP BO instance values, follows
    `WITH`. If a field is specified in the field list within the
    pair of parentheses after `FIELDS`, the `%control`
    flag for this field is automatically set to
    `if_abap_behv=>mk-on`. Likewise, if a field is not
    contained in the list, the flag in `%control` is set to
    `if_abap_behv=>mk-off`. Assume `field2` is not
    specified in the list. The value for `field2` will not be
    respected (even if a value is specified in the internal table). The
    initial value will be used for the field.
>-   Retrieving the responses and specifying the parameters is optional.
    Assuming a data set with the value 2 for `key_field`
    already exists on the database for this BO, you should expect an
    entry for this particular instance in the `failed_resp`
    operand and potentially an error message in
    `reported_resp`, too. Nevertheless, especially in ABP
    implementations and depending on the context, you should implement
    and fill these parameters according to the [RAP BO
    contract](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_contract_glosry.htm "Glossary Entry")
    to meet the variety of implementation rules.
>-   `%cid` should be provided even if you are not interested in
    it and subsequent operations do not require the reference.

Long form of an EML `MODIFY` statement:
``` abap
MODIFY ENTITIES OF root_ent      "full name of root entity
  ENTITY root                    "root or child entity (alias name if available)
  CREATE FROM                    "FROM as further field selection variant
  VALUE #( ( %cid      = 'cid'   "Input derived type created inline
             key_field = 3
             field1    = 'E'
             field2    = 'F'
             %control = VALUE #(                "Must be filled when using FROM
               key_field = if_abap_behv=>mk-on
               field1    = if_abap_behv=>mk-on
               field2    = if_abap_behv=>mk-on ) ) )
  MAPPED DATA(m)       "Target variables declared inline
  FAILED DATA(f)
  REPORTED DATA(r).
```

> **💡 Note**<br>
>-   The entity specified after `ENTITY` can be either the root
    entity itself or a child entity. If an alias is defined, the alias
    should be used.
>-   The addition `FIELDS ( ... ) WITH` from the previous
    snippet is basically a shortcut for the addition
    [`FROM`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_fields.htm)
    that is used here. When using `FROM`, the values of the
    `%control` structure must be specified explicitly.
>-   The BDEF derived types can also be created
    [inline](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_inline.htm)
    as shown in the example using a [constructor
    expression](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconstructor_expression_glosry.htm "Glossary Entry")
    for the input derived type and with `DATA` or
    [`FINAL`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfinal_inline.htm)
    for the responses.
>-   The long form allows you to bundle several operations in one
    statement, either different operations on the same entity (for
    example, deleting some instances and updating some others) or
    operations on different entities of the same RAP BO (for example,
    creating a root entity instance and related instances of a child
    entity in one EML request). Long and short forms are also available
    for other EML statements.

The following EML statement combines multiple operations in one EML
request. It demonstrates the use of `%cid` and
`%cid_ref`. First, two instances are created by specifying
`%cid`. An update operation in the same request only specifies a
certain field within the parentheses of the `FIELDS ( ... )
WITH` addition which denotes that only this particular field
should be updated. The other field values remain unchanged. The
reference to the instance is made via `%cid_ref`. Consider an
EML request in which no instance to refer to using `%cid_ref`
exists, e. g. for an update operation. You can also make the reference
using the unique key. A delete operation is available in the same
request, too. `DELETE` can only be followed by the addition
`FROM`. In contrast to other derived types, the derived type
that is expected here (`TYPE TABLE FOR DELETE`) only has
`%cid_ref` and the key as components.
``` abap
MODIFY ENTITIES OF root_ent
  ENTITY root
  CREATE FIELDS ( key_field field1 field2 ) WITH
    VALUE #( ( %cid    = 'cid4' key_field = 4
                field1 = 'G'    field2    = 'H' )
              ( %cid   = 'cid5' key_field = 5
                field1 = 'I'    field2    = 'J' ) )

  UPDATE FIELDS ( field2 ) WITH
    VALUE #( ( %cid_ref = 'cid4' field2 = 'Z' ) )

  DELETE FROM
    VALUE #( ( %cid_ref  = 'cid5' ) "Instance referenced via %cid_ref
             ( key_field = 9 ) )    "Instance referenced via the key
...
```

EML statement including the execution of an action:
``` abap
MODIFY ENTITIES OF root_ent
  ENTITY root
  EXECUTE some_action
      FROM action_tab
    RESULT DATA(action_result) "Assumption: The action is defined with a
                               "result parameter.
    ...
```

The following code snippet shows a deep create. First, an instance is
created for the root entity. Then, in the same request, instances are
created for the child entity based on the root instance. In the example
below, the assumption is that a composition is specified in the root
view entity like `composition [1..*] of root_ent as _child` and `key_field` and
`key_field_child` are the keys of the child view entity. The
[`%target`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_target.htm)
component group enters the picture here which contains the target's
primary key and data fields.
``` abap
MODIFY ENTITIES OF root_ent
  ENTITY root_ent
  CREATE FIELDS ( key_field field1 field2 ) WITH
    VALUE #( ( %cid = 'cid6' key_field = 6
                field1 = 'I' field2 = 'J' ) )
  CREATE BY \_child
  FIELDS ( key_field_child field1_child field2_child  ) WITH
    VALUE #( ( %cid_ref = 'cid6'
                %target = VALUE #( ( %cid            = 'cid_child_1'
                                     key_field_child = 1
                                     field1_child    = 'aa'
                                     field2_child    = 'bb' )
                                   ( %cid            = 'cid_child_2'
                                     key_field_child = 2
                                     field1_child    = 'cc'
                                     field2_child    = 'dd' ) ) ) )
...
```

<p align="right">(<a href="#top">back to top</a>)</p>

### EML Syntax for Reading Operations

-   Read-only operations always return a result, i.e. the syntax of the
    EML statement requires the addition `RESULT` and an operand.
-   When RAP BO instances are read, the returned data include the
    current status of instances in the transactional buffer which
    includes unsaved modifications on instances. If an instance is not
    yet available in the transactional buffer, the currently persisted
    data set is automatically read into the transactional buffer.
-   Note that read operations are always implicitly enabled for each
    entity listed in a BDEF, i. e. there is no extra definition in the
    BDEF in contrast to, for example, create or update.

The following code snippet shows the long form of the EML
[`READ`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapread_entity_entities_op.htm)
statement for reading instances from the root entity. In `READ`
statements, the additions `FIELDS ( ... ) WITH` and
`FROM` can also be used to specify the fields that you intend to
read. Here, the addition [`ALL FIELDS
WITH`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapread_entity_entities_fields.htm)
is available for reading all field values.

``` abap
READ ENTITIES OF root_ent
  ENTITY root_ent
  ALL FIELDS WITH
  VALUE #( ( key_field = 1 )   "Derived type TYPE TABLE FOR READ IMPORT
                               "only includes the keys
           ( key_field = 2 ) )
  RESULT DATA(result)
  FAILED DATA(f)
  REPORTED DATA(r).
```

Read-by-association operations include the optional addition
[`LINK`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapread_entity_entities_op&sap-language=EN&sap-client=000&version=X&anchor=!ABAP_ONE_ADD@1@&tree=X)
with which you can retrieve the keys of the source and target (i. e. the
associated entity). The by-association operations work reciprocally, i.
e. you can, for example, read a child instance via the parent and a
parent instance via the child, too.

``` abap
"Read-by association operation: parent to child
READ ENTITIES OF root_ent
  ENTITY root_ent
  BY \_child
  ALL FIELDS WITH VALUE #( ( key_field = 1 ) )
  RESULT DATA(rba_res1)
  LINK DATA(links1).
  ...

"Read-by association operation: child to parent
READ ENTITIES OF root_ent
  ENTITY child_ent
  BY \_parent
  ALL FIELDS WITH VALUE #( ( key_field = 1 key_field_child = 1 ) )
  RESULT DATA(rba_res2)
  LINK DATA(links2).
  ...
```
<p align="right">(<a href="#top">back to top</a>)</p>

### Persisting to the Database

-   A [`COMMIT
    ENTITIES`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcommit_entities.htm)
    statement triggers the RAP save sequence. Without such a statement,
    the modified RAP BO instances that are available in the
    transactional buffer are not persisted to the database.
-   `COMMIT ENTITIES` implicitly includes [`COMMIT
    WORK`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcommit.htm).
-   Note: `COMMIT ENTITIES` statements cannot be used
    in behavior implementations. In case of a natively supported RAP
    scenario (for example, when using OData), the `COMMIT
    ENTITIES` request is executed automatically.
-   There a multiple variants available for the statement as described
    in the
    [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapcommit_entities.htm).
-   `COMMIT ENTITIES` statements set the system field
    `sy-subrc`. When using `COMMIT ENTITIES`, it is not
    guaranteed that `COMMIT WORK` is carried out successfully.
    Hence, you should include a check for `sy-subrc` after
    `COMMIT ENTITIES` so that you can react to failures
    accordingly.

The following snippet shows a create operation. This operation has only
an impact on the database with the `COMMIT ENTITIES` statement.
Triggering the save sequence means that the execution of the statement
triggers the calling of the saver methods available in the saver class
of a behavior implementation. In managed scenarios (except for some
special variants), the saving is done automatically without implementing
a dedicated saver method.
``` abap
MODIFY ENTITIES OF root_ent
  ENTITY root_ent
  CREATE FIELDS ( key_field field1 field2 ) WITH
  VALUE #( ( %cid = 'cid' key_field = 7
             field1 = 'K' field2 = 'L' ) ).

COMMIT ENTITIES.

IF sy-subrc <> 0.
  ...
ENDIF.
```

<p align="right">(<a href="#top">back to top</a>)</p>

### EML Statements in ABAP Behavior Pools

-   There are a [special additions when using EML in behavior
    pools](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abeneml_in_abp.htm).
    One of them is [`IN LOCAL MODE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapin_local_mode.htm).
-   This addition can be used to exclude feature controls and
    authorization checks.
-   Consider the following use case: There is a field to display the
    booking status of a trip on a UI. In the BDEF, this field is
    specified as read-only. Hence, it cannot be modified by a user on
    the UI. However, there is a button on the UI to book the trip. This
    button might trigger an action to book the trip so that the value of
    the field changes from open to booked. To enable this, the
    underlying handler method for the modify operation with the action
    to be executed has the addition `IN LOCAL MODE` that ignores
    the feature control.

Syntax:

``` abap
MODIFY ENTITIES OF root_ent IN LOCAL MODE
  ENTITY root
  EXECUTE book
  FROM action_tab
  ...
```

<p align="right">(<a href="#top">back to top</a>)</p>


## Excursion: Using Keys and Identifying RAP BO Instances in a Nutshell / RAP Concepts

<details>
  <summary>Using Keys and Identifying RAP BO Instances in a Nutshell</summary>

<br>

The following bullet points outline important aspects regarding
 keys and identifying [RAP BO
instances](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_instance_glosry.htm "Glossary Entry") in ABAP EML statements.

**Why is it important?**

-   The [primary
    key](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenprimary_key_glosry.htm "Glossary Entry")
    of a [RAP BO entity instance](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_bo_entity_inst_glosry.htm "Glossary Entry")
    is composed of one or more [key fields](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenkey_field_glosry.htm "Glossary Entry").
-   These key fields stand for the fields that are specified with
    `key` in the underlying [CDS view entity](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_v2_view_glosry.htm "Glossary Entry")
    of the RAP BO.
-   The primary key uniquely identifies each RAP BO entity instance.
-   After the creation of an instance and the primary key during a [RAP create operation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_create_operation_glosry.htm "Glossary Entry"),
    the primary key cannot be changed.
    -   Note the concept of [late numbering](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenlate_numbering_glosry.htm "Glossary Entry")
        where newly created entity instances are given their final key
        only shortly before saving in the database. Until then, the
        business logic uses a temporary key that has to be replaced.
-   If a data set with a particular primary key already exists in the
    persistent database table, the saving of a RAP BO instance with a
    duplicate primary key is rejected.

**How can a RAP BO instance be uniquely identified?**

-   It can be done by using a [RAP instance identifier](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_inst_identifier_glosry.htm "Glossary Entry")
    or [RAP content identifier](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_cont_identifier_glosry.htm "Glossary Entry")
    or both of them.
-   RAP instance identifier:
    -   It consists of the primary key fields and all relevant [BDEF derived type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_derived_type_glosry.htm "Glossary Entry")
        components.
    -   To ease the reference to all of these components, special
        [component groups](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencomponent_group_glosry.htm "Glossary Entry")
        are available to summarize the components and make them
        addressable via one single name.
    -   [`%key`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_key.htm):
        Contains the primary key fields of a RAP BO instance
    -   [`%tky`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_tky.htm):
        Specifies the transactional key. Comprises `%key` (and,
        thus, the primary key fields of a RAP BO instance) and more
        components that are relevant to uniquely identify a RAP BO
        instance. Among them,
        [`%pid`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_pid.htm)
        (relevant for late numbering scenarios) and the draft indicator
        [`%is_draft`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_is_draft.htm)
        (relevant for
        [draft](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbdl_with_draft.htm)
        scenarios). In non-late numbering or non-draft scenarios, these
        extra components are just blank. However, it is recommended that
        you use `%tky` in all scenarios since it simplifies a
        possible later switch, for example, to a draft scenario. In
        doing so, lots of adaptations to the code regarding the keys and
        the inclusion of `%is_draft` can be avoided.
-   RAP content identifier:
    -   Reflected in the component
        [`%cid`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_cid.htm)
        which is a string of type
        `ABP_BEHV_CID` to define a content ID.
    -   Used as a unique and preliminary identifier for RAP BO instances
        in RAP create operations, especially where no primary key exists
        for the particular instance.
    -   For newly created instances, the ID can then be used for
        performing further, referencing modifications on those instances
        using
        [`%cid_ref`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_cid_ref.htm)
        (which has the same value as `%cid` is then used, for
        example, in RAP operations using [`CREATE BY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_op.htm), [`UPDATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_op.htm)
        and
        [`DELETE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_op.htm),
        as well as
        [actions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbdl_action.htm)
        with
        [`EXECUTE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapmodify_entity_entities_op.htm)).
    -   In contrast to the primary key and the preliminary ID
        `%pid` for late numbering scenarios, `%cid` (and
        `%cid_ref`) are only available on a short-term basis
        for the current ABAP EML request within the [RAP interaction phase](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_int_phase_glosry.htm "Glossary Entry") in one [RAP LUW](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_luw_glosry.htm "Glossary Entry").
    -   **Note:** The specification of `%cid` should be done even if there are no further operations referring to it.
-   Special case: Late numbering
    -   As mentioned above, in late numbering scenarios newly created
        entity instances are given their final key only shortly before
        saving in the database, i. e. you deal with preliminary keys in
        the RAP interaction phase and the early phase of the [RAP save sequence](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_save_seq_glosry.htm "Glossary Entry").
    -   In this case, you can use `%key` to hold the preliminary
        keys or use a preliminary ID in the dedicated component
        [`%key`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_pid.htm)
        which is of type `ABP_BEHV_PID` and
        only available in late numbering scenarios.
    -   Similar to above, to uniquely identify RAP BO instances in late
        numbering scenarios, you can use either `%key` or
        `%pid` or both of them. In any case, the use of
        `%tky` is handy because it includes both components. You
        must ensure that `%tky` in total uniquely identifies the
        instances.
    -   **Note:** A further component group to refer to the keys is available: [`%pky`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?     file=abapderived_types_pky.htm). `%pky` contains `%pid` and `%key` in late numbering scenarios. In non-late numbering scenarios, it just contains `%key`. `%pky` itself is contained in `%tky`. There are contexts, for example, particular actions, where `%tky` is not available but `%pky` is. This way, there is still the option to summarize `%pid` and `%key` in one component group in the absence of `%tky`.

**General rule**: A RAP BO instance must always be uniquely
identifiable by its transactional key (`%tky`) for internal
processing during the RAP interaction phase. `%tky` always
contains all relevant components for the chosen scenario.

</details>


<details>
  <summary>RAP Concepts</summary>

<br>

**RAP numbering**

-   A concept that deals with setting values for primary key fields.
-   There are multiple options to handle the numbering for primary key
    fields depending on when (early in the RAP interaction phase or late
    in the RAP save sequence) and by whom (RAP BO consumer, behavior
    pool, or framework) the primary key values are set.
-   When:
    -   [Early numbering](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_early_numbering_glosry.htm "Glossary Entry"):
        The final key values are assigned during a RAP create operation
        in the interaction phase.
    -   [Late numbering](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_late_numbering_glosry.htm "Glossary Entry"):
        The final key values are assigned during the RAP save sequence
        (and here only in the RAP saver method
        [`adjust_numbers`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensaver_adjust_numbers.htm)).
-   By whom
    -   [External numbering](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_ext_numbering_glosry.htm "Glossary Entry"):
        Key values are provided by the RAP BO consumer. For example, in
        a create operation, the key values are specified by the RAP BO
        consumer like other non-key field values. Basically, this is the
        concept with which the snippets above are tailored.
    -   [Internal numbering](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_int_numbering_glosry.htm "Glossary Entry"):
        Key values are provided by the RAP BO provider. For example, in
        a create operation, the key values are not specified in an EML
        create request by the RAP BO consumer but rather by the RAP BO
        provider. In case of a managed RAP BO, the key is automatically
        created by the framework which only works if the key is of a
        certain type (16-character byte-like
        [UUID](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenuuid_glosry.htm "Glossary Entry")).
        In case of an unmanaged RAP BO, the key values are provided in a
        dedicated handler method which must be self-implemented. Note
        that late numbering is internal by default since no further RAP
        BO consumer interaction is possible in the late phase of the RAP
        save sequence.

**Draft**

-   The draft concept in RAP allows the content of the transactional
    buffer to be stored in intermediate storages ([draft tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendraft_table_glosry.htm "Glossary Entry"))
    in order to allow transactions to expand over different ABAP
    sessions.
-   Like the concepts mentioned above, a RAP BO can be draft-enabled in
    the BDEF. This concept enters the picture, for example, if you have
    an application allowing data modifications and the temporary storage
    of modifications but does not yet persist them to the database. You
    can continue modifying this data later and you might even use a
    different device from the one where you modified the data
    previously.
-   The draft indicator `%is_draft` enters the picture here for
    RAP BO instance identification. It is used to indicate if a RAP BO
    instance is a [draft instance](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_draft_instance_glosry.htm "Glossary Entry")
    or an [active instance](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_active_instance_glosry.htm "Glossary Entry").
    Conveniently, the component group `%tky` contains
    `%is_draft`. `%is_draft` can then be addressed via
    `%tky`.

> **💡 Note**<br>
> Late numbering and identification in the late phase of the RAP save sequence]
>-   Context: RAP saver method
>    [`adjust_numbers`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensaver_adjust_numbers.htm)
>    in which the final key values are assigned; the preliminary keys can
>    be included in `%key` or `%pid` or both of them.
>-   `%pid` and the preliminary key values in `%key` are
>    automatically assigned to the following component groups when
>    reaching the `adjust_numbers` method:
>    -   [`%tmp`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_tmp.htm):
>        A component group that is assigned the preliminary key values
>        contained in `%key`. In doing so, `%tmp` takes
>        over the role that `%key` has had in the RAP interaction
>        phase to hold the preliminary key values.
>    -   `%pid` remains as is. The component group
>        [`%pre`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapderived_types_pre.htm)
>        contains `%pid` and `%tmp` and, thus, all
>        preliminary identifiers.
>-   In the `adjust_numbers` method, the preliminary keys are
>    transformed into the final keys, i. e. the preliminary keys are
>    mapped to `%key` (which holds the final keys in this
>    context) in the `mapped` [response parameter](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrap_response_param_glosry.htm "Glossary Entry").
>-   Depending on your use case to use either `%pid` or (the
>    preliminary key values in) `%key` (which is `%tmp`
>    here in this method) during the interaction phase or both of them,
>    you must ensure that `%pre` in total (since it contains both
>    `%pid` and `%tmp`) is unique and mapped to the final
>    keys that are to be contained in `%key`.

</details>

<p align="right">(<a href="#top">back to top</a>)</p>

## Further Information

-   Section [ABAP for RAP Business
    Objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_rap_bos.htm)
    in the ABAP Keyword Documentation including EML
-   [Development guide for the ABAP RESTful Application Programming
    Model](https://help.sap.com/http.svc/ahp2/DRAFT/SAP_S4HANA_ON-PREMISE/2022.000/EN/28/9477a81eec4d4e84c0302fb6835035/frameset.htm)
-   [RAP
    Contract](https://help.sap.com/http.svc/ahp2/DRAFT/SAP_S4HANA_ON-PREMISE/2022.000/EN/3a/402c5cf6a74bc1a1de080b2a7c6978/frameset.htm):
    Rules for the RAP BO provider and consumer implementation to ensure
    consistency and reliability


<p align="right">(<a href="#top">back to top</a>)</p>

## Executable Examples
This cheat sheet is supported by different executable examples demonstrating various scenarios:
- Demo RAP scenario with a managed RAP BO, external numbering: [zcl_demo_abap_rap_ext_num_m](./src/zcl_demo_abap_rap_ext_num_m.clas.abap)
- Demo RAP scenario with an unmanaged RAP BO, external numbering: [zcl_demo_abap_rap_ext_num_u](./src/zcl_demo_abap_rap_ext_num_u.clas.abap)
- Demo RAP scenario ("RAP calculator") with a managed, draft-enabled RAP BO, late numbering [zcl_demo_abap_rap_draft_ln_m](./src/zcl_demo_abap_rap_draft_ln_m.clas.abap)


> **💡 Note**<br>
> - To reduce the complexity, the executable examples are purposely kept simple and only focus on the technical side. ABAP classes play the role of a RAP BO consumer here.
>- The examples do not represent real life scenarios and are not suitable as role models for proper RAP scenarios. They rather focus on the technical side by giving an idea how the communication and data exchange between a RAP BO consumer and RAP BO provider can work. Additionally, the examples show how the methods for non-standard RAP BO operations might be self-implemented in an ABAP behavior pool.
>- Due to the simplification, the examples might not fully meet the requirements of the [RAP BO contract](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenrap_bo_contract_glosry.htm) in many respects.
>- The "RAP calculator" example can be checked out using the preview version of an SAP Fiori Elements UI. See the comments in the class for more information.
