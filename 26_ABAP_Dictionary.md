<a name="top"></a>

# ABAP Dictionary (DDIC)

- [ABAP Dictionary (DDIC)](#abap-dictionary-ddic)
  - [Introduction](#introduction)
  - [Data Types in DDIC](#data-types-in-ddic)
    - [Built-in ABAP Dictionary Types](#built-in-abap-dictionary-types)
    - [DDIC Data Types](#ddic-data-types)
      - [DDIC Data Elements](#ddic-data-elements)
      - [DDIC Domains](#ddic-domains)
      - [DDIC Structures](#ddic-structures)
      - [DDIC Table Types](#ddic-table-types)
    - [Predefined Types in DDIC](#predefined-types-in-ddic)
  - [DDIC Database Tables](#ddic-database-tables)
  - [ABAP CDS Objects as Global Types](#abap-cds-objects-as-global-types)
    - [CDS Types Usable as Elementary Types](#cds-types-usable-as-elementary-types)
      - [CDS Built-In Types](#cds-built-in-types)
      - [CDS Simple Types](#cds-simple-types)
      - [CDS Enumerated Types](#cds-enumerated-types)
    - [CDS Entities Usable as Structured Types](#cds-entities-usable-as-structured-types)
    - [CRUD Operations Using CDS Artifacts](#crud-operations-using-cds-artifacts)
  - [Excursions](#excursions)
    - [Built-in Database Functions](#built-in-database-functions)
    - [Finding Released Repository Objects in the System](#finding-released-repository-objects-in-the-system)
    - [Retrieving Repository Objects Information and Creating Repository Object Programmatically with XCO](#retrieving-repository-objects-information-and-creating-repository-object-programmatically-with-xco)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)


This cheat sheet ...
- covers a selection of [repository objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenrepository_object_glosry.htm) in the [ABAP Dictionary (DDIC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_dictionary_glosry.htm) that represent [global types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_type_glosry.htm). Additionally, [CDS objects](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_object_glosry.htm) also representing global types are mentioned. 
- focuses on [ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_for_cloud_dev_glosry.htm), and therefore, does not cover certain DDIC topics and functionalities that are not relevant or supported in this context. 
- invites you to a more in-depth exploration. Make sure that you refer to the documentation for more details and the complete picture.

> [!NOTE]
> - While several DDIC objects are still supported in [ABAP Cloud](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_cloud_glosry.htm) (excluding, for example, classic DDIC views), it is recommended to use their ABAP CDS-based successors for new developments.

## Introduction

- The ABAP Dictionary (DDIC) serves as a persistent repository for type definitions represented by dictionary objects.
- These objects constitute global data types that are accessible by other repository objects.
- Many of these objects can also be accessed in CDS objects and ABAP itself.
- DDIC provides built-in and predefined data types, but also allows for the creation of custom data types.
- The data types that can be defined in the DDIC, similar to the ABAP language, include the following: 
  - Elementary types
  - Structured types
  - Table types
  - Reference types 
- They provide the same functionality as the local types that can be defined in ABAP programs with `TYPES` statements.  
- DDIC also enables the creation of additional objects, such as database tables, which are then created in the underlying database.
- Furthermore, the DDIC includes objects that support development in various contexts, such as lock objects related to the [SAP LUW concept](17_SAP_LUW.md) and more. They are not outlined here.
- DDIC offers many objects and functionalities particularly suited for classic ABAP technologies and user interfaces, such as dynpros. However, these are not relevant in ABAP Cloud and are not covered in this cheat sheet. For example, DDIC search helps (that are used to create value lists for input fields on dynpros) can be replaced by CDS-based search helps.
- The more modern concept, ABAP CDS, is integrated into the DDIC, allowing for the creation of dedicated CDS objects that can replace DDIC objects to be used in newer concepts like RAP.
- DDIC objects are transportable.
- In terms of repository objects in ABAP Cloud, you can use only those SAP-delivered repository objects that are released as APIs for the ABAP language version ABAP for Cloud Development, in addition to your custom repository objects. See a list of released APIs [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreleased_apis.htm). Also, ABAP development tools for Eclipse (ADT) are the only supported tools for creating repository objects.
- Many repository objects are created from source code using dedicated data definition languages, as opposed to the form-based creation you may be familiar from [classic ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclassic_abap_glosry.htm).


> [!NOTE]
> - DDIC provides a centralized location for defining and managing types that are frequently (re)used in DDIC, ABAP CDS, or ABAP programs. This means that a type change in DDIC will automatically update in all relevant places, potentially causing inconsistencies and making adjustments necessary. To find the users of a particular type in ADT, right-click the type and select *Get Where-Used List*.
> - Global access to types is only possible if the package concept does not specify otherwise. A package, which encapsulates repository objects into self-contained units, can be set to disallow external access. Find more information [here](https://help.sap.com/docs/abap-cloud/abap-development-tools-user-guide/editing-abap-packages?locale=en-US&version=sap_btp).


<p align="right"><a href="#top">⬆️ back to top</a></p>

## Data Types in DDIC

DDIC supports the following data types:
- Data Elements that describe elementary types and reference types
- Structured types
- Table types

### Built-in ABAP Dictionary Types

- Built-in ABAP dictionary types ...
  - serve as the foundation for elementary types in all repository objects, including structured and table types, as well as ABAP CDS.
  - allow for the creation of custom types.
    - In data definition languages, you can refer to these types using `abap.type`.
  - are not directly usable in ABAP, except for types specified in ABAP SQL. You can use them through DDIC or CDS objects that include the built-in dictionary types.
  - In ABAP programs, the built-in dictionary types are mapped to corresponding ABAP data types.
- For all available types and more details, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_builtin_types.htm). The following list shows a selection of available built-in dicitionary types: 

  | Built-in Dictionary Type  |  Details |
  |---|---|
  |  `int4`  | Represents 4-byte integers <br>Mapped to the ABAP type `i`  |
  | `char`   | Represents strings of fixed length <br>Mapped to the ABAP type `c` with a dedicated length <br>For example, in database tables and fields of CDS entities, the maximum length is 1333 characters. <br><br>The built-in dictionary type `clnt` is a special character-like type and has special semantics. It denotes the [client column](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclient_column_glosry.htm) in [client-dependent](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclient_dependent_glosry.htm) DDIC database tables. The type is mapped to the ABAP type `c`, length 3.   |
  | `string`  | Represents strings of variable length<br>In DDIC, it is handled like a character large object/CLOB. <br>A maximum length can be specified, but there is no upper limit. The type cannot be used to specify key fields in database tables. In the tables, a maximum length of at least 256 can be specfied to restrict the length of database table fields. <br>Mapped to the identically named ABAP type `string` <br><br>Another built-in dicitionary type mapped to the ABAP type `string` is `sstring`, which represents shorter text strings having a maximum length of 1333. It is handled like `char` and can be used as type for a database table key field (unlike `string`; note that trailing blanks are removed).   |
  |  `datn`/`timn` | Represent date and time formats <br>Mapped to `d`/`t` <br><br>`dats`/`tims` are older types. `datn`/`timn` are preferred because `dats`/`tims` require conversion to actual date and time types.   |
  |  `utclong` | Represents time stamps <br>Mapped to the identically named ABAP data type `utclong`   |
  |  `numc` | Represents numeric text <br>Mapped to `n` |
  |  `raw` | Represents byte strings <br>Mapped to `x`  |


> [!NOTE]
> - There are restrictions when using strings in ABAP CDS and ABAP SQL. For more information, see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_character_byte_types.htm).
> - Built-in dictionary types cannot be used directly in ABAP, e.g. for typing local data objects. However, the types can be used in ABAP SQL, and also ABAP CDS, in the context of [typed literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentyped_literal_glosry.htm). Find more information in the [ABAP SQL](03_ABAP_SQL.md#typed-literals) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### DDIC Data Types

They can be ... 
- defined in the DDIC and represent [global types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenglobal_type_glosry.htm).
- accessed by all repository objects (as long as the package concept allows it).
- referenced in ABAP programs, for example, using `TYPES` and `DATA` statements.
- used to type components in other repository objects.

#### DDIC Data Elements

- DDIC data elements ...
  - are used to create elementary and reference data types. 
  - specify the type of components in repository objects, such as database table fields, or in ABAP programs to create local data types and objects.
  - include both technical and semantic properties.
- Technical properties of ...
  - elementary types are based on a built-in dictionary type. Length, decimal places, and other attributes (if applicable) can be specified either directly in the data element or inherited from a domain.
  - reference types allow for the creation of data and object references. Data references can refer to existing DDIC types or built-in types, or be the generic type `data`. Object references can refer to classes and interfaces, or be the generic type `object`. Reference types cannot be used to type database table fields.
- The technical properties specified depend on the built-in type. For example, the output length includes the total number of characters. For the `dec` type used for packed numbers, a length of 5 with 2 decimals means the output length is 6, including a decimal point. If a sign is selected, the first character is reserved for the + or - sign, making the output length 7.
- Details on semantic properties for data elements can be found [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_data_elements_sema.htm). 
  - Some semantic properties apply only to classic ABAP, particularly classic UIs, and cannot be specified in ABAP for Cloud Development. 
  - Field labels, primarily for classic ABAP UIs, can be specified. The *Medium Text* field label is exposed to OData.
  - Domains provide additional semantic properties, such as a value range.

> [!NOTE]
> - CDS simple types represent successor artifacts of DDIC data elements. 
> - As is true for all the global types discussed, locally defined types hide identically named global types in ABAP programs.

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### DDIC Domains

- DDIC domains are standalone dictionary objects.
- They define both technical and semantic properties of elementary data types, which multiple data elements can reuse. 
- When a data element references a domain, it inherits the domain's properties.
- Although domains define type properties, they cannot be used to declare types and data objects with `TYPES` and `DATA` in ABAP programs. They cannot be reused in repository objects other than data elements.
- You can specify value ranges to set fixed values or intervals as a semantic property. These are typically used for input checks in classic ABAP UIs, which ABAP Cloud does not support. While these value ranges have no effect when used in ABAP statements, they can be used for value helps in the context of CDS views.

> [!NOTE]
> In modern ABAP, CDS enumerated types offer a similar functionality for fixed values.

**Example: DDIC Data Elements/Domains**

- You have accessed your SAP BTP ABAP Environment in ADT.
- Create a data element based on a built-in dicitionary type
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *data element* and choose *Next* and walk through the wizard. 
  - As the data element name, use *ZDEMO_ABAP_DTEL_PR*.
  - On the screen displayed, you have several specification options. Make the following entries:
    - Category: Predefined type
    - Data type: Choose Browse. From the list displayed, select *CHAR*.
    - Length: 3  
  - Save and activate.
- Create a domain
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *domain* and choose *Next* and walk through the wizard. 
  - As the domain name, use *ZDEMO_ABAP_DOMA*.
  - On the screen displayed, you have several specification options. Make the following entries:
    - Data type: Choose Browse. From the list displayed, select *CHAR*.
    - Length/Output Length: 1      
  - Save and activate.
- Create a data element based on a domain
  - Proceed as above and use the name *ZDEMO_ABAP_DTEL_DO*.
  - Category: Domain
  - Type name: *ZDEMO_ABAP_DOMA*
  - Save and activate.
- Create a data element based on a reference type
  - Proceed as above and use the name *ZDEMO_ABAP_DTEL_REF*.
  - Category: Reference to predefined type
  - Data type: *CHAR*
  - Length: 10
  - Save and activate. 


```abap
"Using data elements for local types and data objects

"Data element based on a built-in dicitionary type
TYPES ty_dtel1 TYPE zdemo_abap_dtel_pr.
DATA char3_dtel TYPE zdemo_abap_dtel_pr.

"Data element based on a domain
TYPES ty_dtel2 TYPE zdemo_abap_dtel_do.
DATA char1_dtel TYPE zdemo_abap_dtel_do.

"Data element based on a reference type
TYPES ty_dtel3 TYPE zdemo_abap_dtel_ref.
DATA char10_dtel_ref TYPE zdemo_abap_dtel_ref.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### DDIC Structures

- DDIC structures ...
  - define structured types.
  - are complex types, composed of other data types such as elementary, reference, structured, and table types.
  - can be flat, nested, or deep. Refer to the [Structures](02_Structures.md) cheat sheet for more details on these variants.
  - can be used in other DDIC objects like database tables or to define structured types and data objects in ABAP programs.
  - can be incorporated into other structures. A suffix can be assigned to the components of the included substructure.
- In ADT, structures are created using data definition language:
  - The primary syntax element to define a structure in DDIC is `DEFINE STRUCTURE`.
  - Before this syntax, annotations are used to define structure properties. These include a descriptive text and the enhancement category that determines specific classifications (e.g., `#NOT_EXTENSIBLE` indicates that the object cannot be enhanced). Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_structures_enh_cat.htm).
  - Structure components are listed and separated by semicolons; includes are possible.
    - Structure components can also be specified with annotations.
    - The following list contains a selection of possible types that can be defined for components:
      - Elementary types of components can be specified using data elements or built-in types using `abap.type` (technical properties such as length are specified in parentheses).
      - Reference types: Data elements having reference types and the syntax `REFERENCE TO someType`.
      - Structures (DDIC structures or database tables) as substructures.
      - An include structure can be specified using `INCLUDE`; optionally, a three-character suffix can be specified.
      - Table types
- For comprehensive details and more annotations, refer to the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddicddl_define_structure.htm).

> [!NOTE]
> DDIC database tables and several CDS entities also represent structured types that can be used as such in ABAP programs.

**Example: DDIC Structures**

The following example creates two DDIC structures exploring several options: 
- You have accessed your SAP BTP ABAP Environment in ADT.
- Create a flat DDIC structure 
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *structure* and choose *Next* and walk through the wizard. 
  - As the structure name, use *ZDEMO_ABAP_STRUC_FLAT*.
  - Insert the following code:
    ```abap
    @EndUserText.label : 'Demo flat DDIC structure'
    @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
    define structure zdemo_abap_struc_flat {

      chars : abap.char(3);
      num   : abap.int4;
      cuky  : abap.cuky;
      @Semantics.amount.currencyCode : 'zdemo_abap_struc_flat.cuky'
      curr  : abap.curr(8,2);
      id    : zdemo_abap_dtel_pr;
      flag  : zdemo_abap_dtel_do;

    }
    ```
  - Save and activate.
- Create a deep DDIC structure   
  - As the structure name, use *ZDEMO_ABAP_STRUC_DEEP*.
  - Insert the following code:
    ```abap
    @EndUserText.label : 'Demo deep DDIC structure'
    @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
    define structure zdemo_abap_struc_deep {

      bt_elem1   : abap.char(5);
      bt_elem2   : abap.int4;
      bt_elem3   : abap.dec(8,2);
      bt_elem4   : abap.cuky;
      @Semantics.amount.currencyCode : 'zdemo_abap_struc_deep.bt_elem4'
      bt_elem5   : abap.curr(5,2);
      dtel_elem1 : land1;
      dtel_elem2 : tzntstmpl;
      dref1      : reference to abap.char(3);
      dref2      : reference to abap_boolean;
      dref3      : reference to data;
      oref1      : reference to cl_abap_math;
      oref2      : reference to object;
      iref       : reference to if_oo_adt_classrun;
      struc1     : zdemo_abap_tab1;
      struc2     : include zdemo_abap_carr;
      struc3     : include zdemo_abap_fli with suffix _in;
      tab1       : string_table;
      tab2       : xstring_table;

    }
    ```
    - Notes on the example:
      - `bt_elem*` represents components with elementary types using built-in DDIC types
      - `bt_elem5` represents a type that requires additional specifications. The type alone is not possible since it is a currency field that must relate to a currency code field. The annotation can be added using an ADT quick fix.
      - `dtel_elem*` represents components with elementary types using data elements. The data elements used are all released for ABAP for Cloud Development.
      - `dref*` represents components with data references. The example covers references to built-in types, data elements and the generic type data. 
      - `oref*`/`iref` represents components with object/interface references. Random released classes/interfaces and the generic type `object` are used.
      - `struc*` represent components with structured types. The example does not cover DDIC structures, but DDIC database tables. Tables from the ABAP cheat sheet repository are used. Two structures represent include structures. One of them is specified with suffix. The following would not be possible in the example case since both tables contain some identically named components that would prevent an activation:
        ```abap
        struc2: include zdemo_abap_carr;
        struc3: include zdemo_abap_fli;
        ```
       - `tab*` represents components with table types. The example uses released table types with elementary line types.
  - Save and activate.
   
<p align="right"><a href="#top">⬆️ back to top</a></p>

#### DDIC Table Types

- DDIC table types ...
  - are complex types that describe internal tables in ABAP.
  - are defined by the following properties. For additional details, refer to the [Internal Tables](01_Internal_Tables.md) cheat sheet.
    - Line type (can be elementary, reference, or structured types; the latter may use DDIC database tables; note: CDS entities are not allowed)
    - Table category (options include standard, sorted, hashed, or generic category index for standard and sorted)
    - Table key (the primary table key can be empty, standard, or consist of specific components; secondary table keys and an alias name an be specified)
  - are not to be confused with DDIC database tables, which represent tables on the database.

**Example: DDIC Table Type**

The following example creates three DDIC table types exploring several options: 
- You have accessed your SAP BTP ABAP Environment in ADT.
- Create a table type with elementary line type, no explicit key specification
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *table type* and choose *Next* and walk through the wizard. 
  - As the table type name, use *ZDEMO_ABAP_TT_STR*.
  - On the screen displayed, you have several specification options. Make the following entries:
    - Category: Predefined type
    - Length: 0
    - Leave the other fields as they are. This type basically corresponds to the released table type `string_table`.
  - Save and activate.
- Create a table type with flat line type (based on a DDIC structure created above), with explicit key specifications
  - Proceed as described above, and use the name *ZDEMO_ABAP_TT_SO*.
  - Make the following entries:
    - Category: Dictionary type
    - Type name: *ZDEMO_ABAP_STRUC_FLAT*
    - Initialization and Access section:
      - Access: Sorted Table
    - Primary Key Details section: 
      - Key Definition: Key Components
      - Key Category: Unique
      - Alias: *ch*
    - Key components section:
      - Enter *CHARS* (i.e. the component *chars* of the demo structure)       
    - Key overview section:      
      - Right-click *<Secondary Keys>*, and choose *Create Secondary Key* 
      - Select *<initial>* 
    - Secondary Keys Details section:
      - Name: *sec_key* 
      - Key Definition: Key Components
      - Key Access: Unique Sorted
      - Key Components: *NUM* (i.e. the component *num* of the demo structure)  
    - Save and activate.
- Create a table type with reference type 
  - Proceed as described above, and use the name *ZDEMO_ABAP_TT_REF*.
  - Make the following entries:
    - Category: Reference to Dictionary Type
    - Data type: *data* (to create a table type with reference to the generic type data)
    - Save and activate.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Predefined Types in DDIC

- Multiple DDIC types are predefined and accessible in the system.
- In ABAP for Cloud Development, only released APIs can be used. See the section [Finding Released Repository Objects in the System](#finding-released-repository-objects-in-the-system) further down. 
- The list below showcases a selection of available predefined types:
  - A large number of data elements (and domains) is available. See, for example, the following data elements for built-in dictionary types. In these cases, the data elements have the same names as the built-in dictionary types.
 
    ```abap
    DATA a TYPE int1.
    DATA b TYPE int2.
    DATA c TYPE int4.
    DATA d TYPE d16n.
    DATA e TYPE d34n.
    DATA f TYPE datn.
    DATA g TYPE timn.
    DATA h TYPE utcl.
    
    "Respective built-in DDIC types cannot be specified directly 
    "in ABAP programs. So, the following data object declaration 
    "using a built-in DDIC type is not possible.
    "DATA i type rawstring.
    
    "Note: The following type definition emphasizes that local 
    "types hide global type names.
    TYPES rawstring TYPE xstring.
    DATA j TYPE rawstring.

    "Programmatically retrieving information about which released APIs are available 
    "The following statement retrieves released data elements and domains.
    SELECT ReleasedObjectType, ReleasedObjectName, ReleaseState
      FROM i_apisforclouddevelopment
      WHERE releasestate = 'RELEASED'
      AND ( ReleasedObjectType = 'DTEL' OR ReleasedObjectType = 'DOMA' )
      ORDER BY ReleasedObjectType, ReleasedObjectName
      INTO TABLE @DATA(released_dtel_doma).
    ``` 

  - Truth values: For the data type, it is recommended that you use the `abap_boolean` data element. The released `abap` type pool provides `abap_true` ('X') and `abap_false` ('') to be used as constant values.

    ```abap
    "Using the data element abap_boolean as type in the data object declaration.
    DATA is_true TYPE abap_boolean.

    DATA(str) = `Hello#ABAP##World#`.
    IF str CS `#`.
      "Using abap_true/abap_false from the released abap type pool as constant values
      is_true = abap_true.
    ELSE.
      is_true = abap_false.
    ENDIF.
    ```   

  - Many predefined data types are used in operand positions of ABAP statements. 

    ```abap
    DATA(some_string) = `Hello#ABAP##World#`.

    "The RESULTS addition to the FIND statement can expect 
    "the table type match_result_tab that contains search results. 
    DATA findings TYPE match_result_tab.
    FIND ALL OCCURRENCES OF `#` IN some_string RESULTS findings.
    ASSERT lines( findings ) = 4.

    "Dynamic method call and specifying a parameter table of 
    "type abap_parmbind_tab.
    DATA(oref) = NEW zcl_demo_abap_objects( ).
    "Creating parameter table
    DATA(ptab) = VALUE abap_parmbind_tab( ( name  = 'I_OP'
                                            kind  = cl_abap_objectdescr=>exporting
                                            value = NEW i( 3 ) )
                                          ( name  = 'R_TRIPLE'
                                            kind  = cl_abap_objectdescr=>returning
                                            value = NEW i( ) ) ).

    "The example method triplse the value specified for i_op.
    CALL METHOD oref->('TRIPLE') PARAMETER-TABLE ptab.
    ASSERT ptab[ name = 'R_TRIPLE' ]-value->* = 9.
    ```

<p align="right"><a href="#top">⬆️ back to top</a></p>

## DDIC Database Tables

- DDIC Database tables ...
  - are DDIC objects that describe physical database tables in the current standard database.
  - function as two-dimensional matrices with rows and columns.
  - include a table key, which is a field or combination of fields that uniquely identifies each row. Every database table must have a primary key.
    - Note the concept of foreign keys, where one or more columns in a database table can serve as primary keys in another table. See more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_database_tables_forkey.htm).  
  - have a non-nested structured type. 
  - include both technical and semantic properties. 
  - act as structured types and can be referenced as such in ABAP.
  - can be accessed using ABAP SQL, for example, in `SELECT` statements to retrieve database table entries into an internal table.
  - are created in ADT and ABAP Cloud using source code (dictionary definition language).
- The typical table category of a DDIC database table is a transparent table. When activated, a physical database table is created on the underlying database. This table has the same component names, but they may not be in the same order, allowing extra fields to be added to a DDIC database table without affecting the underlying database object.
- DDIC database table specifications include table fields, which represent the table columns:
  - These must have unique names.
  - Reference types, substructures, and table types are not allowed.
  - One or more key fields must be specified at the beginning to uniquely identify table entries.
    - Among others, key fields cannot be of the types `string` and `rawstring`.
  - The `NOT NULL` flag is always set for key fields, meaning they cannot have a [null value](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennull_value_glosry.htm).
  - Typically, the first column of DDIC database tables is a key field with the built-in type `clnt`, indicating the table is client-dependent. Tables in the delivery class A are generally client-dependent.
  - Some field types, such as `curr` (currency) or `quan` (quantity), require specific specifications and must have reference tables specified.
  - For table field typing, you can use built-in types and data elements.
  - You can specify non-nested structures to be included in tables to avoid redundant structure definitions.
- Specific technical and semantic properties for DDIC database tables can or must be specified. More information is available [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_database_tables_techspec.htm). Among the specifications are the following. They are added by default when creating a new DDIC database table. Certain specifications are added by default when creating a new DDIC database table. 

  | Specification  | Purpose  |
  |---|---|
  | `@EndUserText.label : 'Demo database table'` | Providing a short description  |
  | `@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE`  | Specifies whether the object can be enhanced <br>Find more options using *CTRL + Space* after the # character. In this example, the table cannot be enhanced. 
  | `@AbapCatalog.tableCategory : #TRANSPARENT`  | The default table category is transparent table <br>The defined DDIC database table has an identically named variant with the same columns in the underlying database.  |
  | `@AbapCatalog.deliveryClass : #A`  | The delivery class controls the transport of table data in installations, upgrades, and so on. <br>*A* stands for an application table for master and transaction data.  |
  | `@AbapCatalog.dataMaintenance : #RESTRICTED`  | Defines whether it is possible to display/maintain a database table or view its contents using tools. In the example, the table can be displayed using Data Preview in ADT. |

- Find more information about DDIC database tables in the [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddic_database_tables.htm). 

**Example: DDIC Database Tables**

The following example creates two DDIC database tables exploring several options: 
- You have accessed your SAP BTP ABAP Environment in ADT.
- Create DDIC database tables 
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *database table* and choose *Next* and walk through the wizard. 
  - As the database table name, use *ZDEMO_ABAP_TABL1*.
  - Insert the following code:
    ```abap
    @EndUserText.label : 'Demo DDIC database table'
    @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
    @AbapCatalog.tableCategory : #TRANSPARENT
    @AbapCatalog.deliveryClass : #A
    @AbapCatalog.dataMaintenance : #RESTRICTED
    define table zdemo_abap_tabl1 {

      key client : abap.clnt not null;
      key num    : abap.int4 not null;
      chars      : abap.char(5);
      id         : zdemo_abap_dtel_pr;
      flag       : zdemo_abap_dtel_do;
      str        : abap.string(0);
      cuky       : abap.cuky;
      @Semantics.amount.currencyCode : 'zdemo_abap_tabl1.cuky'
      curr       : abap.curr(8,2);

    }
    ```
    - Note: The numeric specification `abap.string(0);` for a component with type `string` (and also `rawstring`) means to specify the maximum length of the component. `0` in `abap.string(0);` means there is no restriction, `256` in `abap.string(256);` means restricting the length to 256. This restriction is checked in read and write operations.
  - Save and activate.
  - Proceed as above and create another DDIC database table using the name *ZDEMO_ABAP_TABL2*.
  - Insert the following code. The example includes a flat DDIC structure (created above) using the `include` specification. In doing so, you can avoid an extra declaration of table fields.
    ```abap
    @EndUserText.label : 'Demo DDIC database table'
    @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
    @AbapCatalog.tableCategory : #TRANSPARENT
    @AbapCatalog.deliveryClass : #A
    @AbapCatalog.dataMaintenance : #RESTRICTED
    define table zdemo_abap_tabl2 {

      key client    : abap.clnt not null;
      key key_field : abap.int4 not null;
      include zdemo_abap_struc_flat;

    }
    ```
  - Save and activate.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## ABAP CDS Objects as Global Types

- ABAP CDS objects are integrated into DDIC.
- Like DDIC objects, many ABAP CDS objects represent global types usable in ABAP. For example, CDS view entities can be used as data sources in ABAP SQL statements.
- Although DDIC objects can be used in CDS objects, the reverse is not true. For example, a CDS view entity cannot be used in a DDIC database table.
- While ABAP Cloud still supports several DDIC objects, some have ABAP CDS-based successors. These successors offer advanced functionalities and support modern concepts such as ABAP RAP, which relies on data models defined in ABAP CDS and RAP behavior definitions that determine the model behavior.

> [!NOTE]
> - This cheat sheet only emphasizes ABAP CDS objects as global types to be used in ABAP. It does not cover use cases, data modeling aspects, annotations, syntax, and more. Refer to the [documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds.htm) for the complete picture. The executable example of the [CDS View Entities](15_CDS_View_Entities.md) cheat sheet provides a demonstration of a selection of CDS-related syntax.
> - ABAP CDS objects are created in ADT from source code.

### CDS Types Usable as Elementary Types

#### CDS Built-In Types

CDS built-in types...
- Are elementary types derived from the built-in dictionary types.
- Can be used in various ABAP CDS contexts, such as elements in CDS entities (for example, elements in CDS abstract entities, typed literals in CDS view entities, etc.) or in CDS simple types, and for casting in ABAP CDS.
- Are denoted using `abap.type`, with optional or mandatory length and decimal specifications in parentheses. For example, `abap.char(len)` corresponds to the DDIC type `char` with a length specification.
- A comprehensive list can be found [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_overview_builtin_types.htm).

#### CDS Simple Types

- CDS simple types are user-defined types in ABAP CDS that define elementary data types.
- They represent successors to DDIC domains and data elements.
- The types available for use include:
  - Most built-in CDS types
  - DDIC data elements
  - Other simple types
- They can be used ...
  - in ABAP CDS for typing elements or parameters and for casting
  - in ABAP for defining types
- CDS annotations can enhance these types with metadata, which frameworks like RAP evaluate (for example, in the context of OData). When using a data element, DDIC properties are inherited. For more information, see [this topic](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_dtel.htm).
- CDS simple types support type stacking.
- Find more information [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_define_simple_type.htm).

**Example: CDS Simple Types**

The following example creates CDS simple types: 
- You have accessed your SAP BTP ABAP Environment in ADT.
- Create a CDS simple type based on a CDS built-in type
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *type* (under Core Data Services) and choose *Next* and walk through the wizard. 
  - As the type name, use *ZDEMO_ABAP_CDS_TYPE*.
  - Insert the following code:
    ```abap
    @EndUserText.label: 'Demo CDS simple type'
    define type ZDEMO_ABAP_CDS_TYPE : abap.char(5)
    ```
  - Save and activate.
- More syntax options: 
  - CDS simple type based on a DDIC data element
    ```abap
    @EndUserText.label: 'Demo CDS simple type'
    define type ZDEMO_ABAP_CDS_TYPE : land1
    ```
  - Another CDS simple type (assuming a type with that name exists)
    ```abap
    @EndUserText.label: 'Demo CDS simple type'
    define type ZDEMO_ABAP_CDS_TYPE : ZDEMO_ABAP_CDS_ST_BT
    ```

```abap
"Using CDS simple types in ABAP, for example, to declare local
"types and data objects
"Elementary types/data objects
TYPES ty_cds_simple TYPE zdemo_abap_cds_type.
DATA dobj_w_simple_type TYPE zdemo_abap_cds_type.
"Typing a structure component with a CDS simple type
DATA: BEGIN OF struc_w_st,
        comp1 type string,
        comp2 type i,
        comp3 type zdemo_abap_cds_type,
      END OF struc_w_st.
"Internal table with elementary line type represented by the
"CDS simple type
DATA itab_w_st TYPE TABLE OF zdemo_abap_cds_type WITH EMPTY KEY.

"Retrieving type information using RTTI
DATA(tdo_cds_simple_type) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( `ZDEMO_ABAP_CDS_TYPE` ) ).
DATA(type_category_cds_st) = tdo_cds_simple_type->kind.
DATA(type_kind_cds_st) = tdo_cds_simple_type->type_kind.
DATA(is_ddic_type_cds_st) = tdo_cds_simple_type->is_ddic_type( ).
"If you use abap.char(5) as type in the CDS simple type, the following data object contains X
TYPES c5 TYPE c LENGTH 5.
DATA(applies_to_data_cds_st) = tdo_cds_simple_type->applies_to_data( CONV c5( 'abcde' ) ).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

#### CDS Enumerated Types

- CDS enumerated types are user-defined types in ABAP CDS.
- Such an [enumerated type](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenum_type_glosry.htm) is a data type that specifies a value set in addition to the actual type properties.  
- The value set can be used to validate allowed values, similar to fixed values in DDIC domains.
- They can be used ...
  - in ABAP CDS to type elements or parameters, for casting, and in comparisons. More details and examples are available [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_use_enum_type.htm).
  - in ABAP for [enumerated variables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenumerated_variable_glosry.htm). An [enumerated structure](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenumerated_structure_glosry.htm) with the CDS enumerated type name is automatically declared, functioning like [ABAP enumerated types](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenenumerated_type_glosry.htm).
- To create the types, you ...
  - specify a list of enumerated constants that are assigned values as untyped literals. One component must be set as initial to establish the base type's initial value.
  - define a base type. Possible base types include `int1`, `int2`, `int4`, `char`, and `numc`, with the last two having a maximum length of 8.
  - include [annotations](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_enum_type_anno.htm) if needed.

> [!NOTE]
> - Enumerated types can also be specified in ABAP using the syntax `TYPES BEGIN OF ENUM`. See examples in the [Data Types and Data Objects](16_Data_Types_and_Objects.md) cheat sheet.
> - Find more information about RTTI, which is used in this and the previous examples, in the [Dynamic Programming](06_Dynamic_Programming.md) cheat sheet.

**Example: CDS Enumerated Types**

The following example creates a CDS enumerated type: 
- You have accessed your SAP BTP ABAP Environment in ADT.
- Create a CDS enumerated type
  - In your target package, choose *New -> Other ABAP Repository Object*.
  - Filter for *type* (under Core Data Services) and choose *Next* and walk through the wizard. 
  - As the type name, use *ZDEMO_ABAP_CDS_ENUM*.
  - Insert the following code: 
    ```abap
    @EndUserText.label: 'Demo CDS enumerated type'
    define type ZDEMO_ABAP_CDS_ENUM : abap.char(1) enum
    {
      enum_constant_1 = initial;
      enum_constant_2 = 'X';
      enum_constant_3 = 'Y';
      enum_constant_4 = 'Z';
    }
    ```
  - Save and activate.

```abap
"Using CDS enumerated types in ABAP, for example, to declare
"local types and data objects
TYPES ty_cds_enum TYPE zdemo_abap_cds_enum.
DATA dobj_w_enum_type TYPE zdemo_abap_cds_enum.

"The technical data type of an enumerated value is the base type
"of the enumerated type. You can use the base type of an enumerated
"type in special conversions using the constructor operator CONV.
DATA dobj_c1 TYPE c LENGTH 1 VALUE 'Y'.

DATA(conv_enum_1) = CONV zdemo_abap_cds_enum( dobj_c1 ).
"An enumerated structure with the CDS enumerated type name is
"automatically declared. Therefore, you can access the constants
"with the component selector preceded by the CDS enumerated type
"name.
ASSERT conv_enum_1 = zdemo_abap_cds_enum-enum_constant_3.

"Using a value that is not available in the CDS enumerated type
dobj_c1 = 'A'.
TRY.
    DATA(conv_enum_2) = CONV zdemo_abap_cds_enum( dobj_c1 ).
  CATCH cx_sy_conversion_no_enum_value.
    "This example raises an exception.
ENDTRY.

"Retrieving type information using RTTI
DATA(tdo_cds_enum) = CAST cl_abap_enumdescr( cl_abap_typedescr=>describe_by_name( `ZDEMO_ABAP_CDS_ENUM` ) ).
DATA(type_category_cds_enum) = tdo_cds_enum->kind.
DATA(type_kind_cds_enum) = tdo_cds_enum->type_kind.
DATA(is_ddic_type_cds_enum) = tdo_cds_enum->is_ddic_type( ).
DATA(base_kind_cds_enum) = tdo_cds_enum->base_type_kind.
DATA(members_cds_enum) = tdo_cds_enum->members.
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### CDS Entities Usable as Structured Types

- Several [CDS entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_entity_glosry.htm) represent structured types usable in ABAP, however, they are not usable in DDIC objects. 
- These include: 
  - [CDS table entities](https://help.sap.com/docs/ABAP_Cloud/aaae421481034feab3e71dd9e0f643bf/100ab51935544f18b4f4be9b4abb91e8.html)
  - [CDS view entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_v2_view_glosry.htm) for data modeling and selection
  - [CDS table functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_table_function_glosry.htm) for data selection with AMDP
  - [CDS abstract entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_abstract_entity_glosry.htm) for modeling structures, similar to DDIC structures but with advanced features for use in RAP
  - [CDS hierarchies](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_hierarchy_glosry.htm) for retrieving hierarchical data nodes
  - [CDS custom entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_custom_entity_glosry.htm) for implementing custom data retrieval with ABAP
- For an overview and additional details about CDS entities, see [here](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_view_entity.htm) and the [ABAP Data Models Guide](https://help.sap.com/docs/abap-cloud/abap-data-models/abap-data-models?locale=en-US?version=sap_btp).

```abap
"Creating structured types and data objects using ABAP cheat sheet
"repository objects
"CDS view entity
TYPES ty_struc_cds_ve TYPE zdemo_abap_fli_ve.
DATA dobj_struc_cds_ve TYPE zdemo_abap_fli_ve.
"CDS abstract entity
TYPES ty_struc_cds_abstr TYPE zdemo_abap_abstract_ent.
DATA dobj_struc_cds_abstr TYPE zdemo_abap_abstract_ent.
"CDS table function
TYPES ty_struc_cds_tabfunc TYPE zdemo_abap_table_function.
DATA dobj_struc_cds_tabfunc TYPE zdemo_abap_table_function.

"Using CDS entities as data souces in ABAP SQL statements
DATA itab_cds_ve TYPE TABLE OF zdemo_abap_fli_ve WITH EMPTY KEY.
DATA itab_cds_tabfunc TYPE TABLE OF zdemo_abap_table_function WITH EMPTY KEY.

SELECT * FROM zdemo_abap_fli_ve INTO TABLE @itab_cds_ve.
SELECT * FROM zdemo_abap_table_function INTO TABLE @itab_cds_tabfunc.
```

> [!NOTE]
> - Some of the CDS entities can also be used as data sources in ABAP SQL statements, some cannot (for example, CDS custom entities).
> - [DDIC-based views](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_v1_view_glosry.htm) are obsolete and should not be used anymore.


<p align="right"><a href="#top">⬆️ back to top</a></p>

### CRUD Operations Using CDS Artifacts

- There are CDS artifacts that allow not only reading but also creating, updating, and deleting: Table entities and writable CDS view entities.  
- Find example artifacts and an example class using various ABAP SQL statements in the [ABAP SQL](03_ABAP_SQL.md#crud-operations-using-cds-artifacts) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Excursions 

### Built-in Database Functions

- DDIC offers a variety of predefined functions for use in ABAP CDS and ABAP SQL. 
- These functions include numeric functions for computations, string functions for string manipulation, and table functions for handling internal tables, among others. 
- For additional information and code examples, refer to the [Built-In Functions](24_Builtin_Functions.md) cheat sheet.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Finding Released Repository Objects in the System

In an SAP BTP ABAP environment and using ADT, you can find released repository objects in the *Project Explorer* view under *Released Objects*:

![Released APIs](./files/released_APIs.png)

Programmatically retrieving information about which repository objects are released and available in the system:
    
```abap
SELECT ReleasedObjectType, ReleasedObjectName, ReleaseState
  FROM i_apisforclouddevelopment
  WHERE releasestate = 'RELEASED'
  ORDER BY ReleasedObjectType, ReleasedObjectName
  INTO TABLE @DATA(released_apis).
```

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Retrieving Repository Objects Information and Creating Repository Object Programmatically with XCO

Using the [XCO library](https://help.sap.com/docs/btp/sap-business-technology-platform/generation-apis), you can create retrieve ABAP repository objects information and create the objects programmatically. The executable example ([zcl_demo_abap_cloud_excursion](src/zcl_demo_abap_cloud_excursion.clas.abap)) of the [ABAP for Cloud Development](19_ABAP_for_Cloud_Development.md) cheat sheet includes demo code snippets. The [Released ABAP Classes](22_Released_ABAP_Classes.md) cheat sheet also includes a small selection of code snippets in that context. For more information, refer to the [documentation](https://help.sap.com/docs/btp/sap-business-technology-platform/generation-apis).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## More Information
- ABAP Keyword Documentation (ABAP for Cloud Development)
  - [Dictionary (DDIC)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_dictionary.htm)
  - [Core Data Services (ABAP CDS)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds.htm)
  - [ABAP CDS - Type Definitions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_tdl.htm)
  - [ABAP CDS - SAP Annotation Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_annotations_ktd_docu.htm)
- [Working with Classic Objects in ABAP Dictionary](https://help.sap.com/docs/abap-cloud/abap-development-tools-user-guide/working-with-classic-objects-in-abap-dictionary?locale=en-US&version=sap_btp)
- [ABAP Data Models Guide](https://help.sap.com/docs/abap-cloud/abap-data-models/abap-data-models?locale=en-US?version=sap_btp)

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Executable Example

> [!NOTE]
> - The executable example uses both the created repository objects as described in this cheat sheet and some repository objects of the ABAP cheat sheet repository. The example explores and uses code snippets of this cheat sheet, emphasizing the use of global types in ABAP programs.
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)

Expand the following collapsible section for example code. To try it out, create a demo class named `zcl_demo_abap` and paste the code into it. After activation, choose *F9* in ADT to execute the class. The example is set up to display output in the console.

<details>
  <summary>🟢 Click to expand for example code</summary>
  <!-- -->


```abap
CLASS zcl_demo_abap DEFINITION
      PUBLIC
      FINAL
      CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.

CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    "Populating demo database tables from the ABAP cheat sheet repository
    zcl_demo_abap_aux=>fill_dbtabs( ).

    "---------------------------------------------------
    "------------ Built-in dictionary types ------------
    "---------------------------------------------------

    "Built-in dictionary types are not directly usable in ABAP,
    "except for types specified in ABAP SQL.
    "The following example shows miscellaneous typed literals in
    "an ABAP SQL statement. Typed literals can be specified in read
    "positions where host variables are possible.
    DATA(tmstamp) = CONV timestamp( '20240808112517' ).
    DATA(str) = `Some string`.
    SELECT SINGLE
      FROM zdemo_abap_fli
      FIELDS
        carrid,
        @str AS host_var,
        char`X` AS flag,
        int8`32984723948723` AS int8,
        raw`11` AS raw,
        numc`1234` AS numc,
        utclong`2024-01-01T10:01:02,2` AS utc,
        tims`101507` AS tims,
        curr`173.95` AS curr,
        "Multiple cast expressions splitting a time stamp into date and time parts
        CAST( CAST( div( @tmstamp, 1000000 ) AS CHAR ) AS DATS ) AS date,
        CAST( substring( CAST( @tmstamp AS CHAR ), 9, 6 ) AS TIMS ) AS time,
        "Untyped literal
        'ABAP' AS txt
      WHERE fldate = datn`20240102`
      INTO @DATA(misc_typed_literals).

    out->write( data = misc_typed_literals name = `misc_typed_literals` ).

    "-----------------------------------------------------------
    "--------------- DDIC data elements/domains ----------------
    "-----------------------------------------------------------

    "Using DDIC data elements for local types and data objects

    "Data element based on a built-in dicitionary type
    TYPES ty_dtel1 TYPE zdemo_abap_dtel_pr.
    DATA char3_dtel TYPE zdemo_abap_dtel_pr.

    "Data element based on a domain
    TYPES ty_dtel2 TYPE zdemo_abap_dtel_do.
    DATA char1_dtel TYPE zdemo_abap_dtel_do.

    "Data element based on a reference type
    TYPES ty_dtel3 TYPE zdemo_abap_dtel_ref.
    DATA char10_dtel_ref TYPE zdemo_abap_dtel_do.

    "Using a data element in an explicit conversion
    DATA(char3_dtel_inl) = CONV zdemo_abap_dtel_pr( '1234567890' ).
    out->write( data = char3_dtel_inl name = `char3_dtel_inl` ).

    "Using RTTI to demonstrate the type mapping of built-in dicitionary
    "types (that serve as the types of the DDIC data elements) to ABAP types
    "The example data element ZDEMO_ABAP_DTEL_PR is typed with numc, length 5.
    DATA(tdo_a) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_DTEL_PR' ) ).
    DATA(type_kind_a) = tdo_a->type_kind.
    DATA(output_length_a) = tdo_a->output_length.
    TYPES n5 TYPE n LENGTH 5.
    DATA(applies_a) = tdo_a->applies_to_data( CONV n5( '12345' ) ).

    "The example data element ZDEMO_ABAP_DTEL_PR is based on a domain. The type used is char, length 1.
    DATA(tdo_b) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_DTEL_DO' ) ).
    DATA(type_kind_b) = tdo_b->type_kind.
    DATA(output_length_b) = tdo_b->output_length.
    TYPES c1 TYPE c LENGTH 1.
    DATA(applies_b) = tdo_b->applies_to_data( CONV c1( 'A' ) ).

    out->write( data = type_kind_a name = `type_kind_a` ).
    out->write( data = output_length_a name = `output_length_a` ).
    out->write( data = applies_a name = `applies_a` ).
    out->write( data = type_kind_b name = `type_kind_b` ).
    out->write( data = output_length_b name = `output_length_b` ).
    out->write( data = applies_b name = `applies_b` ).

    "-----------------------------------------------------------
    "-------------------- DDIC structures ----------------------
    "-----------------------------------------------------------

    "Using DDIC structures to declare local types and data objects
    TYPES ty_struc_flat TYPE zdemo_abap_struc_flat.
    TYPES ty_struc_deep TYPE zdemo_abap_struc_deep.
    DATA flat_struc1 TYPE ty_struc_flat.
    DATA flat_struc2 TYPE zdemo_abap_struc_flat.
    DATA deep_struc1 TYPE ty_struc_deep.
    DATA deep_struc2 TYPE zdemo_abap_struc_deep.

    "Populating the demo structures
    "Flat structure
    flat_struc1 = VALUE #( chars = 'abc'
                           num = 123
                           cuky = 'EUR'
                           curr = '123.45'
                           id  = '98765'
                           flag = 'X' ).

    "Deep structures including structures
    "Addressing the components of nested structures via the structure name
    "Note struc3: The include structure component in the DDIC structure is specified
    "with a suffix. When addressing the components via the structure name, the suffix
    "is not needed. Check the F2 information, which lists the individual components
    "with suffix.
    deep_struc2 = VALUE #( bt_elem1 = 'abcde'
                           bt_elem2 = 123
                           "...
                           dref1 = NEW #( 'abc' )
                           "...
                           struc2 = VALUE #( carrid = 'XY' carrname = 'XY Airways' )
                           struc3 = VALUE #( carrid = 'YZ' connid = '1234' )
                           "...
                          ).

    "Addressing the components of included structures individually
    DATA(deep_struc3) = VALUE zdemo_abap_struc_deep( bt_elem1 = 'abcde'
                                                     bt_elem2 = 123
                                                     "...
                                                     dref1 = NEW #( 'abc' )
                                                     "...
                                                     "Components referring to struc2
                                                     carrid = 'XY'
                                                     carrname = 'XY Airways'
                                                     "Components referring to struc3 (with suffixes)
                                                     carrid_in = 'YZ'
                                                     connid_in = '1234'
                                                     "...
                                                    ).

    DATA(comp_val1) = deep_struc2-carrid.
    DATA(comp_val2) = deep_struc2-struc2-carrid.
    DATA(comp_val3) = deep_struc2-struc3-carrid.
    ASSERT comp_val1 = comp_val2.
    ASSERT comp_val1 <> comp_val3.

    DATA(comp_val4) = deep_struc3-carrid.
    DATA(comp_val5) = deep_struc3-struc2-carrid.
    DATA(comp_val6) = deep_struc2-struc3-carrid.
    DATA(comp_val7) = deep_struc3-carrid_in.
    ASSERT comp_val4 = comp_val5.
    ASSERT comp_val6 = comp_val7.
    ASSERT comp_val4 <> comp_val6.

    "----------------------------------------------------
    "------------------ DDIC table types ----------------
    "----------------------------------------------------

    "Declaring internal tables and local table types based on
    "DDIC table types
    TYPES ty_tab_elem TYPE zdemo_abap_tt_str.
    TYPES ty_tab_struc TYPE zdemo_abap_tt_so.
    DATA tab_elem1 TYPE ty_tab_elem.
    DATA tab_elem2 TYPE zdemo_abap_tt_str.
    DATA tab_struct1 TYPE ty_tab_struc.
    DATA tab_struct2 TYPE zdemo_abap_tt_so.

    "Using RTTI to check whether the technical properties of the demo table
    "types are identical to those of the released table type string_table
    DATA(applies) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_TT_STR' ) )->applies_to_data( VALUE string_table( ) ).
    ASSERT applies = 'X'.

    "Using RTTI to retrieve table keys
    "The example table type zdemo_abap_tt_so is specified with a primary table with components
    "and secondary table key
    DATA(tdo_table_type) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_name( 'ZDEMO_ABAP_TT_SO' ) ).
    DATA(table_keys) = tdo_table_type->get_keys( ).
    out->write( data = table_keys name = `table_keys` ).

    "The example also specifies an alias name
    DATA(table_key_aliases) = tdo_table_type->get_key_aliases( ).
    out->write( data = table_key_aliases name = `table_key_aliases` ).

    "-------------------------------------------------------
    "-------------- Predefined types in DDIC ---------------
    "-------------------------------------------------------

    "Multiple predefined domains and data elements are available in the system
    "The following example demonstrates data elements for built-in dictionary types.
    "In these cases, the data elements have the same names as the built-in dictionary
    "types.
    DATA a TYPE int1.
    DATA b TYPE int2.
    DATA c TYPE int4.
    DATA d TYPE d16n.
    DATA e TYPE d34n.
    DATA f TYPE datn.
    DATA g TYPE timn.
    DATA h TYPE utcl.

    "Respective built-in DDIC types cannot be specified directly
    "in ABAP programs. So, the following data object declaration
    "using a built-in DDIC type is not possible.
    "DATA i type rawstring.

    "Note: The following type definition emphasizes that local
    "types hide global type names.
    TYPES rawstring TYPE xstring.
    DATA j TYPE rawstring.

    "Programmatically retrieving information about which released APIs are available
    "The following statement retrieves released data elements and domains.
    SELECT ReleasedObjectType, ReleasedObjectName, ReleaseState
      FROM i_apisforclouddevelopment
      WHERE releasestate = 'RELEASED'
      AND ( ReleasedObjectType = 'DTEL' OR ReleasedObjectType = 'DOMA' )
      ORDER BY ReleasedObjectType, ReleasedObjectName
      INTO TABLE @DATA(released_dtel_doma).

    "Truth values
    "Using the data element abap_boolean as type in the data object declaration.
    DATA is_true TYPE abap_boolean.

    DATA(some_string) = `Hello#ABAP##World#`.
    IF some_string CS `#`.
      "Using abap_true/abap_false from the released abap type pool as constant values
      is_true = abap_true.
    ELSE.
      is_true = abap_false.
    ENDIF.

    out->write( data = is_true name = `is_true` ).

    "Many predefined data types are used in operand positions of ABAP statements

    "The RESULTS addition to the FIND statement can expect
    "the table type match_result_tab that contains search results.
    DATA findings TYPE match_result_tab.
    FIND ALL OCCURRENCES OF `#` IN some_string RESULTS findings.
    ASSERT lines( findings ) = 4.
    out->write( data = findings name = `findings` ).

    "Dynamic method call and specifying a parameter table of
    "type abap_parmbind_tab.
    DATA(oref) = NEW zcl_demo_abap_objects( ).
    "Creating parameter table
    DATA(ptab) = VALUE abap_parmbind_tab( ( name  = 'I_OP'
                                            kind  = cl_abap_objectdescr=>exporting
                                            value = NEW i( 3 ) )
                                          ( name  = 'R_TRIPLE'
                                            kind  = cl_abap_objectdescr=>returning
                                            value = NEW i( ) ) ).

    "The example method triplse the value specified for i_op.
    CALL METHOD oref->('TRIPLE') PARAMETER-TABLE ptab.
    ASSERT ptab[ name = 'R_TRIPLE' ]-value->* = 9.
    out->write( data = ptab name = `ptab` ).

    "-------------------------------------------------------
    "------------------ DDIC database tables ---------------
    "-------------------------------------------------------

    "Clearing demo database tables
    DELETE FROM zdemo_abap_tabl1.
    DELETE FROM zdemo_abap_tabl2.

    "DDIC database tables represent structued types.
    DATA struc_from_dbtab TYPE zdemo_abap_tabl1.

    struc_from_dbtab = VALUE #( num  = 1
                                chars = 'abcde'
                                id     = '12345'
                                flag  = 'X'
                                str = `ABAP`
                                cuky = 'EUR'
                                curr = '1.2'  ).

    MODIFY zdemo_abap_tabl1 FROM @struc_from_dbtab.

    DATA itab_from_dbtab TYPE TABLE OF zdemo_abap_tabl1 WITH EMPTY KEY.
    SELECT * FROM zdemo_abap_tabl1 INTO TABLE @itab_from_dbtab.
    out->write( data = itab_from_dbtab name = `itab_from_dbtab` ).

    "The following example DDIC database table specifies an included
    "DDIC structure

    DATA itab_from_dbtab2 TYPE TABLE OF zdemo_abap_tabl2 WITH EMPTY KEY.
    itab_from_dbtab2 = VALUE #( ( key_field = 1
                                  chars = 'abcde'
                                  num  = 2
                                  cuky = 'EUR'
                                  curr = '1.2'
                                  id   = '12345'
                                  flag = 'X' )
                                ( key_field = 3
                                  chars = 'fghij'
                                  num  = 4
                                  cuky = 'USD'
                                  curr = '3.4'
                                  id   = '67890'
                                  flag = 'A' )
                                ( key_field = 5
                                  flag = 'Y' ) ).

    MODIFY zdemo_abap_tabl2 FROM TABLE @itab_from_dbtab2.

    CLEAR itab_from_dbtab2.
    SELECT * FROM zdemo_abap_tabl2 INTO TABLE @itab_from_dbtab2.
    out->write( data = itab_from_dbtab2 name = `itab_from_dbtab2` ).

    "----------------------------------------------------
    "------------------ CDS simple types ----------------
    "----------------------------------------------------

    "Using CDS simple types in ABAP, for example, to declarie local
    "types and data objects
    "Elementary types/data objects
    TYPES ty_cds_simple TYPE zdemo_abap_cds_type.
    DATA dobj_w_simple_type TYPE zdemo_abap_cds_type.
    "Typing a structure component with a CDS simple type
    DATA: BEGIN OF struc_w_st,
            comp1 TYPE string,
            comp2 TYPE i,
            comp3 TYPE zdemo_abap_cds_type,
          END OF struc_w_st.
    "Internal table with elementary line type represented by the
    "CDS simple type
    DATA itab_w_st TYPE TABLE OF zdemo_abap_cds_type WITH EMPTY KEY.

    "Retrieving type information using RTTI
    DATA(tdo_cds_simple_type) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( `ZDEMO_ABAP_CDS_TYPE` ) ).
    DATA(type_category_cds_st) = tdo_cds_simple_type->kind.
    DATA(type_kind_cds_st) = tdo_cds_simple_type->type_kind.
    DATA(is_ddic_type_cds_st) = tdo_cds_simple_type->is_ddic_type( ).
    "If you use abap.char(5) as type in the CDS simple type, the following data object contains X.
    TYPES c5 TYPE c LENGTH 5.
    DATA(applies_to_data_cds_st) = tdo_cds_simple_type->applies_to_data( CONV c5( 'abcde' ) ).

    out->write( data = type_category_cds_st name = `type_category_cds_st` ).
    out->write( data = type_kind_cds_st name = `type_kind_cds_st` ).
    out->write( data = is_ddic_type_cds_st name = `is_ddic_type_cds_st` ).
    out->write( data = applies_to_data_cds_st name = `applies_to_data_cds_st` ).

    "----------------------------------------------------
    "------------------ CDS enumerated types ------------
    "----------------------------------------------------

    "Using CDS enumerated types in ABAP, for example, to declare
    "local types and data objects
    "The example CDS enumerated type's base type is specified with abap.char(1).
    TYPES ty_cds_enum TYPE zdemo_abap_cds_enum.
    DATA dobj_w_enum_type TYPE zdemo_abap_cds_enum.

    "The technical data type of an enumerated value is the base type
    "of the enumerated type. You can use the base type of an enumerated
    "type in special conversions using the constructor operator CONV.
    DATA dobj_c1 TYPE c LENGTH 1 VALUE 'Y'.

    DATA(conv_enum_1) = CONV zdemo_abap_cds_enum( dobj_c1 ).
    ASSERT conv_enum_1 = zdemo_abap_cds_enum-enum_constant_3.
    out->write( data = conv_enum_1 name = `conv_enum_1` ).

    "Using a value that is not available in the CDS enumerated type
    dobj_c1 = 'A'.
    TRY.
        DATA(conv_enum_2) = CONV zdemo_abap_cds_enum( dobj_c1 ).
      CATCH cx_sy_conversion_no_enum_value INTO DATA(enum_error).
        out->write( data = enum_error->get_text( ) name = `enum_error->get_text( )` ).
    ENDTRY.

    "Retrieving type information using RTTI
    DATA(tdo_cds_enum) = CAST cl_abap_enumdescr( cl_abap_typedescr=>describe_by_name( `ZDEMO_ABAP_CDS_ENUM` ) ).
    DATA(type_category_cds_enum) = tdo_cds_enum->kind.
    DATA(type_kind_cds_enum) = tdo_cds_enum->type_kind.
    DATA(is_ddic_type_cds_enum) = tdo_cds_enum->is_ddic_type( ).
    DATA(base_kind_cds_enum) = tdo_cds_enum->base_type_kind.
    DATA(members_cds_enum) = tdo_cds_enum->members.

    "Although abap.char(1) is used as base type in the example CDS enumerated type,
    "the following data object is not type compliant.
    DATA(applies_to_data_cds_enum1) = tdo_cds_enum->applies_to_data( abap_true ).
    ASSERT applies_to_data_cds_enum1 IS INITIAL.
    DATA(applies_to_data_cds_enum2) = tdo_cds_enum->applies_to_data( CONV zdemo_abap_cds_enum( 'X' ) ).

    out->write( data = type_category_cds_enum name = `type_category_cds_enum` ).
    out->write( data = type_kind_cds_enum name = `type_kind_cds_enum` ).
    out->write( data = is_ddic_type_cds_enum name = `is_ddic_type_cds_enum` ).
    out->write( data = base_kind_cds_enum name = `base_kind_cds_enum` ).
    out->write( data = members_cds_enum name = `members_cds_enum` ).
    out->write( data = applies_to_data_cds_enum1 name = `applies_to_data_cds_enum1` ).
    out->write( data = applies_to_data_cds_enum2 name = `applies_to_data_cds_enum2` ).

    "Some demo assertions regarding the example CDS enumerated type, which is usable as
    "enumerated structure in ABAP.
    FIELD-SYMBOLS <fs_enum> TYPE data.
    LOOP AT CAST cl_abap_enumdescr( cl_abap_typedescr=>describe_by_name( `ZDEMO_ABAP_CDS_ENUM` ) )->members INTO DATA(enum_comps).
      "Dynamically assigning the enumerated constant to a field symbol
      ASSIGN zdemo_abap_cds_enum-(enum_comps-name) TO <fs_enum>.
      CASE enum_comps-name.
        WHEN 'ENUM_CONSTANT_1'.
          ASSERT <fs_enum> = zdemo_abap_cds_enum-enum_constant_1.
          ASSERT <fs_enum> = CONV zdemo_abap_cds_enum( '' ).
          ASSERT enum_comps-value = ''.
        WHEN 'ENUM_CONSTANT_2'.
          ASSERT <fs_enum> = zdemo_abap_cds_enum-enum_constant_2.
          ASSERT <fs_enum> = CONV zdemo_abap_cds_enum( 'X' ).
          ASSERT enum_comps-value = 'X'.
        WHEN 'ENUM_CONSTANT_3'.
          ASSERT <fs_enum> = zdemo_abap_cds_enum-enum_constant_3.
          ASSERT <fs_enum> = CONV zdemo_abap_cds_enum( 'Y' ).
          ASSERT enum_comps-value = 'Y'.
        WHEN 'ENUM_CONSTANT_4'.
          ASSERT <fs_enum> = zdemo_abap_cds_enum-enum_constant_4.
          ASSERT <fs_enum> = CONV zdemo_abap_cds_enum( 'Z' ).
          ASSERT enum_comps-value = 'Z'.
      ENDCASE.
    ENDLOOP.

    "----------------------------------------------------
    "----- CDS entities usable as structured types ------
    "----------------------------------------------------

    "Several CDS entities represent structured types that can be
    "used in ABAP. The example shows the creation of local data
    "types and objects based on CDS entities. Demo artifacts from
    "the ABAP cheat sheet repository are used.

    "CDS view entity
    TYPES ty_struc_cds_ve TYPE zdemo_abap_fli_ve.
    DATA dobj_struc_cds_ve TYPE zdemo_abap_fli_ve.
    "CDS abstract entity
    TYPES ty_struc_cds_abstr TYPE zdemo_abap_abstract_ent.
    DATA dobj_struc_cds_abstr TYPE zdemo_abap_abstract_ent.
    "CDS table function
    TYPES ty_struc_cds_tabfunc TYPE zdemo_abap_table_function.
    DATA dobj_struc_cds_tabfunc TYPE zdemo_abap_table_function.

    "Using CDS view entities and CDS table functions as data souces
    "in ABAP SQL statements
    DATA itab_cds_ve TYPE TABLE OF zdemo_abap_fli_ve WITH EMPTY KEY.
    DATA itab_cds_tabfunc TYPE TABLE OF zdemo_abap_table_function WITH EMPTY KEY.

    SELECT * FROM zdemo_abap_fli_ve INTO TABLE @itab_cds_ve.
    SELECT * FROM zdemo_abap_table_function INTO TABLE @itab_cds_tabfunc.

    out->write( data = itab_cds_ve name = `itab_cds_ve` ).
    out->write( data = itab_cds_tabfunc name = `itab_cds_tabfunc` ).

  ENDMETHOD.
ENDCLASS.
```

</details>  

